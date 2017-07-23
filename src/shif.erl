-module(shif).
-export([run/1,serve/2,accept/1,start_link/0,sys/2]).

%% SHIF = SHell InterFace.
%%
%% The protocol is such to make it easy to interface with shell scripts.
%% A message consists of lines of <variable>=<value> bindings sent to
%% a tcp socket, delimited by end of connection.
%%
%% The variable 'EXO_TYPE' is used to identify the command. 
%%
%% This allows the 'set' command to dump an entire shell environment
%% as a message, after stripping out the function definitions:
%%
%% set|grep -P '^\S+='|netcat -q0 localhost 12345


run(PortNo) ->
    {ok, LSock} = gen_tcp:listen(PortNo, [binary, {packet, line}, {active, false}, {reuseaddr, true}]),
    shif:accept(LSock).
accept(LSock) ->
    {ok, Socket} = gen_tcp:accept(LSock),
    spawn(fun() -> shif:serve(Socket, #{}) end),
    shif:accept(LSock).
serve(Socket,Map) ->
    case gen_tcp:recv(Socket,0) of
        {error,closed} ->
            handle(Map);
        {error,E} ->
            exit(E);
        {ok, Data} ->
            [Line,<<>>] = binary:split(Data,<<"\n">>),
            [Key,Value] = binary:split(Line,<<"=">>),
            shif:serve(Socket,maps:put(Key,Value,Map))
    end.

%% Dispatch on type    
handle(#{<<"EXO_TYPE">> := <<"udev">>} = Msg) ->
    udev(Msg);
handle(#{<<"EXO_TYPE">> := <<"kodi">>, <<"CMD">> := Cmd}) ->
    kodi:cmd(kodi,binary_to_existing_atom(Cmd,utf8));
handle(#{<<"EXO_TYPE">> := <<"prompt">>, <<"PWD">> := Pwd}) ->
    tools:info("prompt ~s~n", [Pwd]);
handle(Msg) ->
    tools:info("unknown: ~p~n", [Msg]).



%% Midi is now handled by studio/src/midi.erl
%% Path for usb: eg "1-1.2:1.0":
%% <bus>-<port>.<port>.<port>....:<configuration>:<interface>
%% udev(#{<<"ACTION">>    := <<"add">>,
%%        <<"DEVPATH">>   := Path,
%%        <<"DEVNAME">>   := <<"/dev/midi",N/binary>>=Devname} = _M) ->
%%     %% More info through DEVPATH (strip /sound/card1/midi1)
%%     [Product] = sys(Path,"/../../../../product"),
%%     io:format("udev add midi ~s ~p ~p~n",[N, Product, path(Path)]),
%%     %%io:format("~p~n",[_M]),
%%     midi:start(Devname, Product),
%%     ok;

udev(#{<<"ACTION">>    := <<"add">>,
       <<"DEVPATH">>   := Path,
       <<"DEVNAME">>   := <<"/dev/tty",TTY/binary>>} = _M) ->
    io:format("udev add tty ~s ~p~n",[TTY, path(Path)]),
    %%io:format("~p~n",[_M]),
    ok;

udev(#{<<"ACTION">>    := <<"add">>,
       <<"DEVPATH">>   := Path,
       <<"DEVNAME">>   := Devname} = _M) ->
    io:format("udev add ~s ~p~n",[Devname, path(Path)]),
    %%io:format("~p~n",[_M]),
    ok;

%% udev(#{<<"ACTION">>    := <<"add">>,
%%        <<"SUBSYSTEM">> := <<"sound">>,
%%        <<"ID_SERIAL">> := <<"BEHRINGER_BCR2000">>,
%%        <<"DEVNAME">>   := Devname}) ->
%%     io:format("udev add BCR2000 ~s~n",[Devname]);

%% udev(#{<<"ACTION">>    := <<"add">>,
%%        <<"SUBSYSTEM">> := <<"sound">>,
%%        <<"DEVNAME">>   := Devname}) ->
%%     io:format("udev add sound ~s~n",[Devname]);

udev(_) ->
    ignored.


%% Tools
path(P) ->
    binary:split(P,<<"/">>, [global,trim_all]).


sys(Path,Suffix) ->
    {ok,Bin} = file:read_file(lists:flatten(io_lib:format("/sys/~s~s",[Path,Suffix]))),
    binary:split(Bin,<<"\n">>,[global,trim_all]).

start_link() ->
    {ok, spawn_link(fun() -> run(12345) end)}.
