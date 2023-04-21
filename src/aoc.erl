-module(aoc).
-compile(export_all).

% day 1
elf(Line) ->
    Item = binary:split(Line, <<"\n">>, [trim, global]),
    lists:sum(lists:map(fun(C) -> binary_to_integer(C) end, Item)).

read(Filename) ->
    {ok, Bin} = file:read_file(Filename),
    binary:split(Bin, <<"\n\n">>, [trim, global]).

riddle1() ->
    Lines = read("input"),
    lists:max(lists:map(fun elf/1, Lines)).

riddle2() ->
    Lines = read("input"),
    Calories = lists:map(fun elf/1, Lines),
    [A,B,C|_] = lists:reverse(lists:sort(Calories)),
    A+B+C.

% day 2
translate("A") -> rock;
translate("B") -> paper;
translate("C") -> scissors;
translate("X") -> rock;
translate("Y") -> paper;
translate("Z") -> scissors.

shape(rock) -> 1;
shape(paper) -> 2;
shape(scissors) -> 3.

outcome(X, X) -> 3;
outcome(rock, scissors) -> 0;
outcome(rock, paper) -> 6;
outcome(scissors, paper) -> 0;
outcome(scissors, rock) -> 6;
outcome(paper, rock) -> 0;
outcome(paper, scissors) -> 6.

calc(X, Y) -> shape(Y) + outcome(X, Y).
round({X, Y}) -> calc(translate(X), translate(Y)).

read2(Filename) ->
    {ok, Bin} = file:read_file(Filename),
    L = binary:split(Bin, <<"\n">>, [trim, global]),
    L2 = lists:map(fun(B) -> string:tokens(binary_to_list(B), " ") end, L),
    [list_to_tuple(X) || X <- L2].

riddle3() ->
    L = read2("input2"),
    lists:sum(lists:map(fun round/1, L)).

translate(X, "Y") -> translate(X);
translate("A", "X") -> scissors;
translate("A", "Z") -> paper;
translate("B", "X") -> rock;
translate("B", "Z") -> scissors;
translate("C", "X") -> paper;
translate("C", "Z") -> rock.

strategy({X, Y}) -> calc(translate(X), translate(X, Y)).

riddle4() ->
    L = read2("input2"),
    lists:sum(lists:map(fun strategy/1, L)).

% day 3
str_to_list(Str) -> [[X] || X <- Str].

find_item(Str) ->
    L = str_to_list(Str),
    Len = trunc(length(L)/2),
    {A,B} = lists:split(Len, L),
    I = sets:intersection(sets:from_list(A), sets:from_list(B)),
    sets:to_list(I).

generate_lookup_table() ->
    L = lists:append(lists:seq(97, 122), lists:seq(65, 90)),
    T = [{C, I} || {C, I} <- lists:zip(L, lists:seq(1, 52))],
    maps:from_list(T).

read3(Filename) ->
    {ok, Bin} = file:read_file(Filename),
    L = binary:split(Bin, <<"\n">>, [trim, global]),
    lists:map(fun(S) -> binary_to_list(S) end, L).

riddle5() ->
    T = generate_lookup_table(),
    R = read3("input3"),
    Items = lists:map(fun find_item/1, R),
    lists:sum(lists:map(fun([X]) -> maps:get(hd(X), T) end, Items)).

partitionImpl(N, Acc, Rest) when length(Rest) >= N ->
    {H, R} = lists:split(N, Rest),
    AccNext = lists:append(Acc, [H]),
    partitionImpl(N, AccNext, R);
partitionImpl(_, Acc, _) -> Acc.

partition(N, List) -> partitionImpl(N, [], List).

to_set(Str) -> sets:from_list(str_to_list(Str)).

find_batch([A,B,C|[]]) ->
    As = to_set(A),
    Bs = to_set(B),
    Cs = to_set(C),
    AintersectB = sets:intersection(As, Bs),
    sets:to_list(sets:intersection(AintersectB, Cs)).

riddle6() ->
    T = generate_lookup_table(),
    R = read3("input3"),
    Groups = partition(3, R),
    Badges = lists:map(fun(X) -> find_batch(X) end, Groups),
    lists:sum(lists:map(fun([X]) -> maps:get(hd(X), T) end, Badges)).
