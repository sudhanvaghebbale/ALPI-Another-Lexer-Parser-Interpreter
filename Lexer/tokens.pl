#!/usr/local/bin/swipl

:-initialization(main,program).


main :-
    open('tokens.txt', read, Str),
    read_file(Str,Lines),
    close(Str),
    write(Lines), nl,
    % insert the code here, lines is the list of tokens

    halt.

read_file(Stream,[]) :-
    at_end_of_stream(Stream).

read_file(Stream,[X|L]) :-
    \+ at_end_of_stream(Stream),
    read(Stream,X),
    read_file(Stream,L).
