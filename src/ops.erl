%% System level tools, e.g. linux ssh access.
-module(ops).
-export([ssh/3, cmd/1,
         hp/1,hs/1,set_cookie/1,distribution/2]).

    

%% Probe first? rpc:call(Node,erlang,self,[],ProbeTimeout) of

ssh({pmap,Hosts}, Cmd, Timeout) ->
    tools:pmap(fun(Host) -> ssh({host,Host},Cmd, Timeout) end, Hosts);
ssh({host,Host}, Cmd, Timeout) ->
    SshCmd = io_lib:format("ssh -A ~s '~s'", [Host, Cmd]),
    tools:script_lines(SshCmd, Timeout).

cmd(Cmd) ->
    ssh({pmap,hosts()}, Cmd, 3000).


%% Test
%% hosts() -> [tp,pt,zoo,revo,pi,xm,zoe,soekris].
%% hosts() -> [zoo,zoe,tp,pt,soekris,zni,revo].
hosts() -> [zoo,zoe,tp,soekris].
hp(F) -> tools:pmap(F, hosts()).
hs(F) -> lists:map(F, hosts()).
    

%nmap_ping() ->
%    {ok, {El, _}} = tools:script_xml("nmap -oX - -sn 10.71.71.0/24", 20000),
%    lists:map(fun tools:xmlElement_attributes_proplist/1,
%              xmerl_xpath:string("//nmaprun/host/address",El)).


set_cookie({pw,Name}) ->
    set_cookie({file,"~/.pw/" ++ Name});
set_cookie({file,File}) ->
    {ok, Bin} = file:read_file(File),
    set_cookie(Bin);
set_cookie(Bin) when is_binary(Bin) ->
    erlang:set_cookie(node(), binary_to_atom(Bin, utf8)).

distribution(Name, Cookie) ->
    net_kernel:start([Name, longnames]),
    set_cookie(Cookie).
