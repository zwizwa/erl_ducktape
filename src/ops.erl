%% System level tools, e.g. linux ssh access.
-module(ops).
-export([ssh/3, cmd/1,
         hp/1,hs/1]).

    

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
