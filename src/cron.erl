-module(cron).
-export([archive_email/0,
         email/1, parse/1,
         kodi_scan_video/0,
         kodi_scan_video/1]).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% Parse Cron email on stdin.
archive_email() ->
    email(
      fun(_Headers, BodyLines) ->
              Terms = [parse(L) || L <- BodyLines],
              io:format("~p~n",[Terms]),
              %% FIXME: do something with the content of the notification?
              kodi_scan_video()
      end).
    

%% KODI notifications.
kodi_scan_video() ->
    [kodi_scan_video(Host) || Host <- ["lroom", "broom"]].
kodi_scan_video(Host) ->
    Reply =
        os:cmd(
          lists:flatten(
            %% Use .zoo VPN
            ["/etc/net/bin/kodi_scan_video.sh ", Host, ".zoo"])),
    io:format("~s", [Reply]),
    Reply.


%% Use erlang parser.
parse(Str) ->
    {ok, Tokens, _} = erl_scan:string(Str),
    {ok, Term} = erl_parse:parse_term(Tokens),
    Term.


%% Read email from stdin and pass header and body list of lines to
%% function.
email(Fun) ->
    email_headers(Fun,[]).
line() ->
    io:get_line("").
email_headers(K, Stack) ->
    case line() of
        "\n" -> email_body(K, lists:reverse(Stack), []);
        Line -> email_headers(K, [Line|Stack])
    end.
email_body(K,H,Stack) ->            
    case line() of
        "\n" ->
            eof = line(),
            K(H,lists:reverse(Stack));
        Line ->
            email_body(K,H,[Line|Stack])
    end.


-ifdef(TEST).
tok_test_() ->
    [?_assert(parse("[copy,4,\"abc\",\"def\"].") =:= [copy,4,"abc","def"])].
-endif.
