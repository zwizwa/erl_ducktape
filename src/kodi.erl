-module(kodi).
-export([start_link/0, connected/2, cmd/2,
         activeplayers/1, playerid/1, connect/1, proxy/1, proxy_tcp/2]).

start_link() ->
    Pid = spawn_link(fun not_connected/0),
    register(kodi, Pid),
    {ok, Pid}.


connect_init(Host) ->
    connect_init(Host, fun() -> ignore end).
connect_init(Host, Init) ->
    TcpPort = 9090,
    case gen_tcp:connect(Host, TcpPort, [binary, {active, true}]) of
        {ok, Port} ->
            Init(),
            connected(Port, #{});
        Error ->
            tools:info("error: ~p~n", [Error]),
            not_connected()
    end.

not_connected() ->
    receive 
        {proxy, Node}   -> proxy_init(Node);
        {connect, Host} -> connect_init(Host);
        %% By default, start on localhost.
        Msg -> connect_init("ip6-localhost", fun() -> self() ! Msg end)
    end.


%% Also plug local port, e.g.:
%% ssh -L 8080:localhost:8080 pi2.vpn.k
%% Or start an erlang server process.
%%     spawn_link(fun() -> os:cmd(tools:format("ssh -L 8080:localhost:8080")) ...


%% Proxy TCP over erlang distro network.
proxy_tcp(Node,Port) ->
    serv:start(
      {handler,
       fun() ->
               serv_tcp:init(
                 [Port],
                 %% Connect init + handler
                 {handler,
                  fun(Sock,_) -> #{sock => Sock, node => Node} end,
                  fun proxy_tcp_handle/2},
                 %% Registry message handler
                 fun(_, _) -> void end, void)
       end}).

proxy_tcp_handle(Msg, State) ->                 
    tools:info("~p~n",[Msg]),
    State.



proxy_init(Node) when is_atom(Node) ->
    proxy_loop(rpc:call(Node,erlang,whereis,[kodi])).
proxy_loop(Proxy) when is_pid(Proxy) ->
    receive
        {proxy, Node}   -> proxy_init(Node);
        {connect, Host} -> connect_init(Host);
        Msg -> Proxy ! Msg, proxy_loop(Proxy)
    end.        

connected(Port, State) ->
    receive
        %% FIXME: there is no guarantee Bin contains a full message!
        {tcp, Port, Bin} ->
            NextTrailer = handle_bin(iolist_to_binary([maps:get(trailer, State, <<>>),Bin])),
            kodi:connected(Port, maps:put(trailer, NextTrailer, State));
        {command, Pid, Fun} ->
            {NewState, Json} = Fun(State),
            JsonId = jiffy:encode(maps:put(<<"id">>,pid_enc(Pid),Json)),
            gen_tcp:send(Port, JsonId),
            kodi:connected(Port, NewState);
        {connect, Host} ->
            gen_tcp:close(Port),
            connect_init(Host, fun() -> ignore end);
        Other ->
            io:format("unknown: ~p~n", [Other]),
            exit(Other)
            %%kodi:connected(Port, State)
    end.
handle_json(Json) ->
    case maps:get(<<"id">>, Json, notification) of
        notification ->
            %% io:format("notification:~n  ~p~n", [Json]),
            ignore;
        PidEnc ->
            %% io:format("reply:~n  ~p~n", [Json]),
            pid_dec(PidEnc) ! {reply, Json}
    end.
handle_bin(Bin) ->
    case jiffy:decode(Bin, [return_maps, return_trailer]) of
        {error, {_, truncated_json}} ->
            Bin;
        {has_trailer, Json, Rest} ->
            handle_json(Json),
            handle_bin(Rest);
        Json -> 
            handle_json(Json),
            <<>>
    end.


%% This is not proper.. but works.
pid_enc(X) -> list_to_binary(io_lib:format("~p",[X])).
pid_dec(X) -> list_to_pid(binary_to_list(X)).


rpc(Method, Params) ->
    #{
       <<"jsonrpc">> => <<"2.0">>,
       <<"method">>  => Method,
       <<"params">>  => Params
     }.
                            
call(K, Fun) ->
    K ! {command, self(), Fun},
    receive 
        {reply, Reply} -> maps:get(<<"result">>, Reply, {error, Reply})
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

cmd(_,_) -> false.

call_rpc_p(K,Cmd,Args) ->
    P=playerid(K), %% Separate call. Keep outside of the fun!
    call(K, fun(S) -> {S, rpc(Cmd, maps:merge(#{<<"playerid">> => P}, Args))} end).

call_rpc(K,Cmd,Args) ->
    call(K, fun(S) -> {S, rpc(Cmd, Args)} end).

activeplayers(K) -> [maps:get(<<"playerid">>,P) || P <- cmd(K,activeplayers)].
playerid(K) -> hd(activeplayers(K) ++ [-1]).  %% don't crash in case nothing is playing. give fake value instead.

%%17> kodi:cmd(activeplayers).
%%[#{<<"playerid">> => 1,<<"type">> => <<"video">>}]
%%[]
    

clip(X) -> max(0,min(100,X)).


%% Connect to kodi directly.
connect(Host)  -> kodi ! {connect, Host}.
%% Connect to different node.
proxy(Node) when is_atom(Node) -> kodi ! {proxy, Node};
proxy(Host) -> proxy(list_to_atom(tools:format("exo@~s.vpn.k",[Host]))).
     


