-module(backup).
-export([subvols/0,
         archives/1,
         borg_diff/1,
         borg_todo/1,
         script/1 %% output is shell script
]).

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
    Out = os:cmd(tools:format("borg list --short ~s 2>/dev/null", [Dir])),
    re:split(Out,"\n",[trim]).


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

%% Generate shell script.
%% # net-escript --apply1 backup script borg_todo systems
script(["borg_todo", Repo]) ->
    io:format("#!/bin/bash\n"),
    lists:foreach(
      fun({Subvol,Snapshot}) ->
              io:format(
                "borg.add.sh /borg/~s /subvol/~s/~s~n",
                [Repo,Subvol,Snapshot])
      end,
      borg_todo(Repo)).
