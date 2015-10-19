#!/usr/bin/env escript
main([In]) ->
    {ok, File} = file:open(In, [read]),
    case io:get_line(File, "") of
        eof ->
            file:close(File);
        Cap ->
            io:format("-module(cf_term).~n"),
            io:format("-export([has_color/1]).~n"),
            Terms = read_lines(File, {parse_name(Cap), ""}, []),
            Terms1 = [{T, has_color(Vs, Terms)} || {T, Vs} <- Terms],
            [io:format("has_color(~p) -> true;~n", [T]) || {T, true} <- Terms1],
            io:format("has_color(_) -> false.~n")
    end.

read_lines(File, {H, Acc}, FAcc) ->
    case io:get_line(File, "") of
        eof  ->
            file:close(File),
            lists:sort([{H, parse_caps(Acc)} | FAcc]);
        " " ++ Rest ->
            case Acc of
                "" ->
                    read_lines(File, {H, remove_newline(strip_ws(Rest))}, FAcc);
                _ ->
                    read_lines(File, {H, [Acc, " ", remove_newline(strip_ws(Rest))]}, FAcc)
            end;
        "\t" ++ Rest ->
            case Acc of
                "" ->
                    read_lines(File, {H, remove_newline(strip_ws(Rest))}, FAcc);
                _ ->
                    read_lines(File, {H, [Acc, " ", remove_newline(strip_ws(Rest))]}, FAcc)
            end;
        Cap ->
            read_lines(File, {parse_name(Cap), ""}, [{H, parse_caps(Acc)} | FAcc])
    end.

has_color([], _Ts) ->
    false;
has_color([{colors, _} | _], _Ts) ->
    true;
has_color([{use, T} | R], Ts) ->
    case orddict:find(T, Ts) of
        error ->
            has_color(R, Ts);
        {ok, V} ->
            has_color(V, Ts) orelse
                has_color(R, Ts)
    end.

remove_newline(S) ->
    remove_newline(S, []).

remove_newline("\n", Acc) ->
    lists:reverse(Acc);

remove_newline("", Acc) ->
    lists:reverse(Acc);
remove_newline([C | R], Acc) ->
    remove_newline(R, [C | Acc]).

strip_ws([$\s | R]) ->
    strip_ws(R);
strip_ws([$\t | R]) ->
    strip_ws(R);
strip_ws(R) ->
    R.

filter_caps([]) ->
    [];
filter_caps([{colors, _} = C | R]) ->
    [C | filter_caps(R)];
filter_caps([{use, _} = C | R]) ->
    [C | filter_caps(R)];
filter_caps([_ | R]) ->
    filter_caps(R).


remove_colon(S) ->
    re:replace(S, ",[\\s\\n\\t]*$", "", [{return,list}]).

parse_name(S) ->
    [Term | _Rest] = re:split(remove_colon(S), "\\|", [{return,list}]),
    Term.

parse_caps(S) ->
    Caps = re:split(remove_colon(S), ",[\\s\\t]+", [{return,list}]),
    filter_caps([parse_cap(C, []) || C <- Caps, C =/= ""]).

parse_cap([], Name) ->
    list_to_atom(lists:reverse(Name));
parse_cap([$= | V], Name) ->
    {list_to_atom(lists:reverse(Name)), V};
parse_cap([$# | V], Name) ->
    {list_to_atom(lists:reverse(Name)), list_to_integer(V)};
parse_cap([C | R], Name) ->
    parse_cap(R, [ C | Name]).

