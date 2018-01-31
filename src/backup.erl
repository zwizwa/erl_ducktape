-module(backup).
-export([subvols/0,
         archives/1,
         borg_archives/1,
         borg_diff/1,
         borg_todo/1,
         subvols_in_borg/1,
         subvol_snapshots/0]).

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
                      exit({paths2map,Paths,Other})
              end
      end,
      #{},
      Paths).


prefix(Subvol) ->
    Subvol = filename:basename(Subvol), %% Assert
    tools:format("/subvol/~s",[Subvol]).

subvol_snapshots() ->
    {ok, Subvols} = file:list_dir("/subvol"),
    lists:map(
      fun list_to_binary/1,
      lists:append(
        [filelib:wildcard(
           tools:format("~s_*_*_*",[Subvol]), prefix(Subvol))
         || Subvol <- Subvols])).
    
      

borg_archives(Repo) ->
    Repo = filename:basename(Repo), %% Ensure this is a basename.
    Dir = tools:format("/borg/~s", [Repo]),
    true = filelib:is_dir(Dir),
    case os:cmd(tools:format("borg list --short ~s 2>/dev/null", [Dir])) of
        "" -> [];  %% Why special case?  Note this is what happens when repo is locked.
        Out -> re:split(Out,"\n",[trim])
    end.


%% Diffable trees representing current snapshots and borg archives.
subvols()      -> paths2map(subvol_snapshots()).
archives(Repo) -> paths2map(borg_archives(Repo)).

%% FIXME: this is wrong.  Write it in terms of set difference.
     
%% List of snapshots not currently in borg repo.
borg_diff(Repo) ->
    BorgArchives = archives(Repo),
    BorgKeys = maps:keys(BorgArchives),
    Subvols = maps:filter(
                fun(K,_V) -> lists:member(K,BorgKeys) end,
                subvols()),
   diff:diff(BorgArchives,Subvols).


%% Compile diff into batch list.
borg_todo(Repo) ->
    lists:append(
      lists:map(
        fun({insert,[Subvol,Snapshot],true}) ->
                [{Subvol,Snapshot}];
           (_) ->
                []
        end,
        borg_diff(Repo))).

subvols_in_borg(Repos) ->
    InBorg = 
        lists:append(
          lists:map(
            fun borg_archives/1, 
            Repos)),
    maps:map(
      fun(_, Snapshots) ->
              maps:filter(
                fun(Snapshot, true) ->
                        lists:member(Snapshot, InBorg)
                end,
                Snapshots)
      end,
      subvols()).


