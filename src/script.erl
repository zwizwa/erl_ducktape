%% Script generators.  Admin code does not perform operations, but
%% generates scripts that can be executed.  This allows for easy
%% manual validation, and also allows concentrating the
%% string<->object conversions here.

%% E.g.
%% # net-escript --apply script borg_batch systems | bash

-module(script).
-export([borg_update/1, subvol_prune/1]).


%% Archive: snapshot -> borg
%% Implemented as queue add.
borg_update(Repo) ->
    io:format("#!/bin/bash\n"),
    lists:foreach(
      fun({Subvol,Snapshot}) ->
              io:format(
                "tsp /etc/net/bin/borg.add.sh /borg/~s /subvol/~s/~s~n",
                [Repo,Subvol,Snapshot])
      end,
      backup:borg_todo(Repo)).

%% Prune:
%% - if not last (needed for btrfs transfer)
%% - if present in borg

%% To implement, for all snapshots, remove the last one, then subtract
%% the list of all borg backups.

%% I wonder if it's easier to implement these as sqlite virtual tables..
subvol_prune(Repos) ->
    io:format("#!/bin/bash\nset -x\n"),
    lists:foreach(
      fun({Subvol,SnapshotMap}) ->
              Snapshots = maps:keys(SnapshotMap),
              case lists:sort(
                    fun(A,B)-> A>B end,
                     Snapshots) of
                  [Latest|Old] ->
                      io:format("# keep /subvol/~s/~s~n", [Subvol,Latest]),
                      lists:foreach(
                        fun(Snapshot) ->
                                io:format(
                                  "btrfs sub del /subvol/~s/~s~n",
                                  [Subvol,Snapshot])
                        end,
                        Old);
                  [] ->
                      io:format("# ign /subvol/~s~n", [Subvol])
              end
      end,
      maps:to_list(backup:subvols_in_borg(Repos))).
      
