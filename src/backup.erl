-module(backup).
-export([snapshots/1,borg/1]).

paths2map(Paths) ->
    Map1 =
        lists:foldl(
          fun(Path, Map) ->
                  case re:split(Path, "_") of
                      [Subvol, _Date, _Time, _Host] ->
                          maps:update_with(
                            Subvol,
                            fun(List) -> [Path|List] end,
                            [Path],
                            Map);
                      Other ->
                          io:format("WARNING: ~p~n",[{other,Other}]),
                          Map
                  end
          end,
          #{},
          Paths),
    maps:map(
      fun(_,List) -> lists:reverse(lists:sort(List)) end,
      Map1).

snapshots(Dir) ->
    {ok, Paths} = file:list_dir(Dir),
    paths2map(Paths).
 
borg("systems"=Repo) -> borg_(Repo);
borg("hatd"=Repo)    -> borg_(Repo);
borg("data"=Repo)    -> borg_(Repo).

borg_(Repo) ->                    
    Out = os:cmd(tools:format("borg list --short /borg/~s 2>/dev/null", [Repo])),
    Paths = re:split(Out,"\n",[trim]),
    paths2map(Paths).
    
    
%% FIXME: tree diff
