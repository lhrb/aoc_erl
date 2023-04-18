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
outcome(scissors, rock) -> 0;
outcome(scissors, paper) -> 6;
outcome(paper, scissors) -> 0;
outcome(paper, rock) -> 6.
