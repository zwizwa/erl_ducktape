-module(kodi).
-export([%% server
         cmd/2,
         activeplayers/1, playerid/1,
         %% single message
         http_jsonrpc/3,
         video_library_scan/1,
         video_library_clean/1]).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% Two mechanism are available:
%% - An Erlang server process
%% - A function, using curl for message delivery over http.
%%
%% The latter doesn't need OTP and can be run from an escript.
%%
%% A server is only necessary to handle notifications.  Currently I
%% have no use case for this, so the server is being moved elsewhere.



%% JSONRPC notes:

%% - id: absence of id means it is a notification: no reply is sent.
%%   otherwise, a reply always sent.  the exception seems to be
%%   badly structured mesages such as '{}'.



%% Operations like video_library_clean take a long time.
%% FIXME: is it ok to keep waiting?
-define(TIMEOUT,infinity).



rpc(Method, Params) -> #{
     <<"jsonrpc">> => <<"2.0">>,
     <<"id">>      => tools:format("~p",[self()]),
     <<"method">>  => Method,
     <<"params">>  => Params
    }.

%% Convert binary keys to atoms.
clean(Map) when is_map(Map) ->
    maps:from_list(
      maps:fold(
        fun(K,V,Stack) ->
                [{binary_to_atom(K,utf8),clean(V)}
                 |Stack]
        end, [], Map));
clean(List) when is_list(List) ->
    lists:map(fun clean/1, List);
clean(Other) ->
    Other.



unpack(Reply) ->
    clean(maps:get(<<"result">>, Reply, {error, Reply})).


%% For some actions we can go through stateless http.  In this case
%% all actions that otherwise depend on kodi_serv state will use only
%% their defaults.
call({http,Host}, Fun) ->     
    FakeState = #{},
    {_IgnoredState, EJson} = Fun(FakeState),
    unpack(http_jsonrpc(Host, EJson, ?TIMEOUT));

    
%% Full server interface.
%% Server will run: {NewState, Json} = Fun(State),
%% And will return an EJson reply.
call(K, Fun) ->
    K ! {command, self(), Fun},
    receive 
        {reply, Reply} -> unpack(Reply)
    after
        10000 -> exit(timeout)
    end.

cmd(K,playpause) -> call_rpc_p(K,<<"Player.PlayPause">>, #{});
cmd(K,{seek,V})  -> call_rpc_p(K,<<"Player.Seek">>, #{<<"value">> => V});
cmd(K,backward)  -> cmd(K,{seek,<<"smallbackward">>});
cmd(K,forward)   -> cmd(K,{seek,<<"smallforward">>});

cmd(K,{vol,V})   -> call_rpc(K,<<"Application.SetVolume">>, #{<<"volume">> => V});

cmd(K,volup)     -> cmd(K,{volrel, 5});
cmd(K,voldown)   -> cmd(K,{volrel,-5});

cmd(K,{url,Url}) ->
    tools:re_dispatch(
      Url, [{"https://www.youtube.com/watch\\?v=(.*)",
             fun([ID]) -> cmd(K,{youtube,ID}) end},
            {"",
             fun(_) -> ignore end}]);

cmd(K,{youtube,ID}) ->
    F = iolist_to_binary(
          [<<"plugin://plugin.video.youtube/?action=play_video&videoid=">>, ID]),
    call_rpc(K,<<"Player.Open">>, #{<<"item">> => #{<<"file">> => F}});
%%116> http_uri:parse("http://foo/bar/baz?a=1&b2").
%%{ok,{http,[],"foo",80,"/bar/baz","?a=1&b2"}}

cmd(K,activeplayers) -> call_rpc(K,<<"Player.GetActivePlayers">>, #{});

cmd(K,{volrel,Offset}) ->
    call(K,fun(State) ->
                   Vol = clip(maps:get(volume, State, 70) + Offset),
                   NextState = maps:put(volume, Vol, State),
                   {NextState, rpc(<<"Application.SetVolume">>, #{<<"volume">> => Vol})}
           end);

%% ["title", "album", "artist", "season", "episode", "duration", "showtitle", "tvshowid", "thumbnail", "file", "fanart", "streamdetails"]
cmd(K,current) ->
    call_rpc_p(
      K, <<"Player.GetItem">>, 
      #{<<"properties">> => 
            [<<"title">>, <<"album">>, <<"artist">>, 
             <<"file">>, <<"streamdetails">>]});

cmd(_,_) -> false.

call_rpc_p(K,Cmd,Args) ->
    P=playerid(K), %% Separate call. Keep outside of the fun!
    call(K, fun(S) -> {S, rpc(Cmd, maps:merge(#{<<"playerid">> => P}, Args))} end).

call_rpc(K,Cmd,Args) ->
    call(K, fun(S) -> {S, rpc(Cmd, Args)} end).

activeplayers(K) -> [maps:get(playerid,P) || P <- cmd(K,activeplayers)].
playerid(K) -> hd(activeplayers(K) ++ [-1]).  %% don't crash in case nothing is playing. give fake value instead.

%%17> kodi:cmd(activeplayers).
%%[#{<<"playerid">> => 1,<<"type">> => <<"video">>}]
%%[]
    

clip(X) -> max(0,min(100,X)).


     

%% =========================================================================


%% Interact through HTTP.  This uses curl with message passed as a
%% command line argument.  Passing it on stdin doesn't seem to work,
%% as we can't close a process' stdin to signal end of message when we
%% still need to read from its stdout.  http_client from inets needs
%% background processes while this should also work from escript.
http_post(URL, ContentType, Bin, TimeOut) ->
    Port =
        open_port(
          {spawn_executable, "/usr/bin/curl"},
          [stream, binary, use_stdio, exit_status,
           {args,
            ["--silent",
             "--data-binary", Bin,
             "--header", tools:format_binary("content-type: ~s;",[ContentType]),
             URL]}]),
    iolist_to_binary(
      fold:to_list(
        fold:from_port(Port, TimeOut))).

http_jsonrpc(Host, EJson, TimeOut) ->
    PW = pw("kodi"),
    URL = tools:format_binary("http://kodi:~s@~s:8080/jsonrpc",[PW,Host]),
    Bin = http_post(URL, "application/json", jiffy:encode(EJson), TimeOut),
    %% io:format("~p~n",[{URL,Bin}]),
    jiffy:decode(Bin,[return_maps]).

pw(File) ->
    {ok, Bin} = 
        file:read_file(
          lists:flatten(
            [os:getenv("HOME"),"/.pw/",File])),
    hd(re:split(Bin,"\n")).

video_library_scan(Host) ->    
    http_jsonrpc(Host, rpc(<<"VideoLibrary.Scan">>,[]), ?TIMEOUT).

video_library_clean(Host) ->    
    http_jsonrpc(Host, rpc(<<"VideoLibrary.Clean">>,[]), ?TIMEOUT).



-ifdef(TEST).
tok_test_() ->
    [?_assert(http_jsonrpc("lroom.zoo",[]) =:= asdf)].
-endif.
