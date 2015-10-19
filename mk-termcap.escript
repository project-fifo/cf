#!/usr/bin/env escript
main([In]) ->
    {ok, File} = file:open(In, [read]),
    case io:get_line(File, "") of
        eof ->
            file:close(File);
        Cap ->
            read_lines(File, {parse_name(Cap), ""})
    end.

read_lines(File, {H, Acc}) ->
    case io:get_line(File, "") of
        eof  ->
            file:close(File),
            print_term(H, parse_caps(Acc)),
            io:format("cap_(_) -> [].\n");
        " " ++ Rest ->
            case Acc of
                "" ->
                    read_lines(File, {H, remove_newline(strip_ws(Rest))});
                _ ->
                    read_lines(File, {H, [Acc, " ", remove_newline(strip_ws(Rest))]})
            end;
        "\t" ++ Rest ->
            case Acc of
                "" ->
                    read_lines(File, {H, remove_newline(strip_ws(Rest))});
                _ ->
                    read_lines(File, {H, [Acc, " ", remove_newline(strip_ws(Rest))]})
            end;
        Cap ->
            print_term(H, parse_caps(Acc)),
            read_lines(File, {parse_name(Cap), ""})
    end.

print_term({Term, _Desc, _Aliases}, Caps) ->
    io:format("cap_(~p) -> ~p;~n", [Term, Caps]).


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
remove_colon(S) ->
    remove_colon(S, []).

remove_colon(", \n", Acc) ->
    lists:reverse(Acc);
remove_colon(",\n", Acc) ->
    lists:reverse(Acc);
remove_colon("\n", Acc) ->
    lists:reverse(Acc);
remove_colon("", Acc) ->
    lists:reverse(Acc);
remove_colon([C | R], Acc) ->
    remove_colon(R, [C | Acc]).


parse_name(S) ->
    [Term | Rest] = re:split(remove_colon(S), "\\|", [{return,list}]),
    case lists:reverse(Rest) of
        [] ->
            {Term, [], []};
        [Title | Aliases] ->
            {Term, Title, Aliases}
    end.

parse_caps(S) ->
    Caps = re:split(S, ",[\\s\\t]*", [{return,list}]),
    [parse_cap(C, []) || C <- Caps, C =/= ""].

parse_cap([], Name) ->
    lists:reverse(Name);
parse_cap([$= | V], Name) ->
    {lists:reverse(Name), V};
parse_cap([$# | V], Name) ->
    {lists:reverse(Name), V};
parse_cap([C | R], Name) ->
    parse_cap(R, [ C | Name]).

