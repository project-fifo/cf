-module(termcap).

-export([cap/1]).

cap(Term) ->
    resolve(cap_(Term)).

resolve([]) ->
    [];
resolve([{"use", Cap1} | R]) ->
    cap(Cap1) ++ resolve(R);
resolve([C | R]) ->
    [C | resolve(R)].

