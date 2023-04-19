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
