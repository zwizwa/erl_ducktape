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
      fun(H,B) ->
              io:format("~p~n",[{H,B}]),
              kodi_scan_video()
      end).
    
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

-ifdef(TEST).

tok_test_() ->
    [?_assert(parse("[copy,4,\"abc\",\"def\"].") =:= [copy,4,"abc","def"])].
              
    

%% test() ->
%%     [?_assert(
%%         archive_parse("[copy,4,\"/ftp/downloads/Dark.Matter.S03E06.720p.HDTV.x264-AVS[rarbg]\",\"/ftp/tvshows/Dark Matter\"]\n")
%%         =:= ok)].
-endif.
