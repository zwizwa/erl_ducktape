-module(backup).
-export([subvols/0,
         archives/1,
         test/1]).

paths2map(Paths) ->
    lists:foldl(
      fun(Path, Map) ->
              case re:split(Path, "_") of
                  [Subvol, _Date, _Time, _Host] ->
                      maps:update_with(
                        Subvol,
                        fun(M) -> maps:put(Path,true,M) end,
                        #{Path => true},
                        Map);
                  Other ->
                      io:format("WARNING: ~p~n",[{other,Other}]),
                      Map
              end
      end,
      #{},
      Paths).


prefix(Subvol) ->
    Subvol = filename:basename(Subvol), %% Assert
    tools:format("/subvol/~s",[Subvol]).

%% Diffable tree representing current snapshots.
subvols() ->
    {ok, Subvols} = file:list_dir("/subvol"),
    lists:foldl(
      fun(Subvol,SubvolMap) ->
              Snapshots =
                  filelib:wildcard(
                    tools:format("~s_*_*_*",[Subvol]), prefix(Subvol)),
              maps:put(
                Subvol,
                lists:foldl(
                  fun(Snapshot, SnapshotMap) ->
                          maps:put(Snapshot, true, SnapshotMap)
                  end, #{}, Snapshots),
                SubvolMap)
      end, #{}, Subvols).
      

borg_archives(Repo) ->
    Repo = filename:basename(Repo), %% Ensure this is a basename.
    Dir = tools:format("/borg/~s", [Repo]),
    true = filelib:is_dir(Dir),
    Out = os:cmd(tools:format("borg list --short ~s 2>/dev/null", [Dir])),
    re:split(Out,"\n",[trim]).


archives(Repo) ->
    paths2map(borg_archives(Repo)).

     


test(_) ->
    lists:filter(
      fun({insert,_,_}) -> true; (_) -> false end,
      diff:diff(archives("systems"),subvols())).
    
    
%% FIXME: tree diff: per borg repo, get a list of subvols that are
%% not yet added.

