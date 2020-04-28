#!/usr/local/bin/swipl

:- initialization(main,program).


main :-
    %Paste the path to the tokens.txt file here
    open('C:\\ASU\\SER503\\SER502-Spring2020-Team26\\Lexer\\tokens.txt', read, Str),
    read_file(Str,Lines),!,
    close(Str),
    list_butlast(Lines,Tokens),
    writeq(Tokens), nl,
    program(P,Tokens,[]),
    eval_program(P,[],_),
    halt.

%Removes end_of_file token
list_butlast([X|Xs], Ys) :-
   list_butlast_prev(Xs, Ys, X).
list_butlast_prev([], [], _).
list_butlast_prev([X1|Xs], [X0|Ys], X0) :-
   list_butlast_prev(Xs, Ys, X1).

%Reads tokens line by line from the file
read_file(Stream,[]) :-
    at_end_of_stream(Stream).
read_file(Stream,[X|L]) :-
    \+ at_end_of_stream(Stream),
    read(Stream,X),
    read_file(Stream,L).

% Tables are used to tackle left recursion
:- table expr/3, term/3, value/3.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%% Program Section %%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

program(t_program(K)) --> block(K).
eval_program(t_program(K), Env, FinalEnv) :- eval_block(K, Env, FinalEnv).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%% Block Section %%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

block(t_block(DL, CL)) --> [begin], declarationList(DL), commandList(CL), [end].

funcBlock(t_funcBlock(DL, CL)) --> declarationList(DL), commandList(CL).

eval_block(t_block(D, C), Env, NewEnv) :-
    eval_declarationList(D, Env, Env1), eval_commandList(C, Env1, NewEnv).

eval_funcBlock(t_funcBlock(D, C), Env, NewEnv) :-
    eval_declarationList(D, Env, Env1), eval_commandList(C, Env1, NewEnv).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% Declaration Grammar %%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

declarationList(t_declarationList(D, DL)) --> declaration(D), declarationList(DL).
declarationList(t_declarationList(D)) --> declaration(D).

declaration(t_declaration(I)) -->  dataType, varIdentifier(I).
declaration(t_funcDeclaration(FD)) --> functionDeclaration(FD).

declaration(t_init_expr(I, N)) --> [num], varIdentifier(I), [=], expression(N).
declaration(t_init_string(I, S)) --> [str], varIdentifier(I), [=], string(S).
declaration(t_init_bool(I, B)) --> [bool], varIdentifier(I), [=], boolean(B).
declaration(t_init_list(I, L)) --> [list], varIdentifier(I), [=], list(L).
declaration(t_init_dict(I, D)) --> [dict], varIdentifier(I), [=], dictionary(D).
dataType --> [num] ; [str] ; [bool] ; [list] ; [dict].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% String Grammar %%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

string(t_string(S)) --> ['"'], stringTerm(S), ['"'].
stringTerm(t_stringTerm(S)) --> [S], {atom(S)}.
stringTerm(t_stringTerm()) --> [].

stringOps(t_stringOps_concat(CST)) -->  concatString(CST).
stringOps(t_stringOps_rev(RST)) -->  revString(RST).
stringOps(t_stringOps_len(SLEN)) -->  stringLength(SLEN).

concatString(t_concatStr(S1, S2)) --> [concat], expression(S1) , expression(S2).
revString(t_revStr(S)) --> [rev], expression(S).
stringLength(t_strLen(S)) --> [len], expression(S).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% String Evaluation %%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

eval_string(t_string(S),Str):- eval_stringTerm(S,Str).
eval_stringTerm(t_stringTerm(S),S).

eval_stringOps(t_stringOps_concat(X),Env,Val) :- eval_concatString(X,Env,Val).
eval_stringOps(t_stringOps_rev(X),Env,Val) :- eval_revStr(X,Env,Val).
eval_stringOps(t_stringOps_len(X),Env,Val) :- eval_stringLength(X,Env,Val).

eval_revStr(t_revStr(S),Env,Ans) :- eval_expression(S,Env,_,String), atom(String), atom_string(String,Str), string_chars(Str, List), rev(List, TempAns, []), string_chars(Ans, TempAns).
eval_revStr(t_revStr(S),Env,Ans) :- eval_expression(S,Env,_,String), string_chars(String, List), rev(List, TempAns, []), string_chars(Ans, TempAns).
rev([], Z, Z).
rev([H|T],Z,Acc) :- rev(T,Z,[H|Acc]).

%Concat String
eval_concatString(t_concatStr(S1, S2),Env,Ans) :- eval_expression(S1,Env,_,String1), atom(String1), atom_string(String1,Str1), eval_expression(S2,Env,_,String2), atom(String2), atom_string(String2,Str2), string_concat(Str1, Str2, Ans).
eval_concatString(t_concatStr(S1, S2),Env,Ans) :- eval_expression(S1,Env,_,String1), eval_expression(S2,Env,_,String2), string_concat(String1, String2, Ans).



%String Length
eval_stringLength(t_strLen(S),Env,Ans) :- eval_expression(S,Env,_,String), atom(String), atom_string(String,Str), string_length(Str, Ans).
eval_stringLength(t_strLen(S),Env,Ans) :- eval_expression(S,Env,_,String), string_length(String, Ans).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% Declaration Evaluation %%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

eval_declarationList(t_declarationList(C, CL), Env, NewEnv) :-
    eval_declaration(C, Env, Env1), eval_declarationList(CL, Env1, NewEnv).

% declarationList(t_decList(D)) --> declaration(D).
eval_declarationList(t_declarationList(C), Env, NewEnv) :- eval_declaration(C, Env, NewEnv).

% declaration(t_declaration(I)) -->  dataType, identifier(I).
eval_declaration(t_declaration(Id), Env, NewEnv) :- update(Id, 'None', Env, NewEnv).

eval_declaration(t_funcDeclaration(FD), Env, NewEnv) :- eval_functionDeclaration(FD, Env, NewEnv).

% If value not in lookup table, put a garbage value.
/*eval_declaration(t_declaration(I), Env, NewEnv) :-
    eval_id(I, Id), not(lookup(Id, Env, _)), update(Id, _, Env, NewEnv).
*/
% declaration(t_init(I, _)) --> identifier(I), [=], expression.
eval_declaration(t_init_expr(Id,E), Env, NewEnv) :-
    eval_expression(E, Env, Env1, Val1), update(Id, Val1, Env1, NewEnv).

eval_declaration(t_init_string(Id, S), Env, NewEnv) :-
    eval_string(S,Val), update(Id, Val, Env, NewEnv).

% bool(t_bool(true)) --> [true].
eval_declaration(t_init_bool(Id, V), Env, NewEnv) :-
    eval_boolean(V, Env, Env, true), update(Id, true, Env, NewEnv).

% bool(t_bool(false)) --> [false].
eval_declaration(t_init_bool(Id, V), Env, NewEnv) :-
    eval_boolean(V, Env, Env, false), update(Id, false, Env, NewEnv).

% declaration(t_init_list(I,L)) --> identifier(I), [=], list(L)
eval_declaration(t_init_list(Id,L), Env, NewEnv) :-
    eval_list(L,Id,Env,NewEnv).
% declaration(t_init_list(I,D)) --> identifier(I), [=], dictionary(D)
eval_declaration(t_init_dict(Id,D), Env, NewEnv) :-
    eval_dictionary(D,Id,Env,NewEnv).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%% Command Grammar %%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

commandList(t_commandList(C, CL)) --> command(C), commandList(CL).
commandList(t_commandList(C))  --> command(C).
%commandList(t_commandList()) --> [].

command(t_command_assign_expr(I,E)) -->  identifier(I), [=], expression(E).
command(t_command_assign_id(ID1,ID2)) -->  identifier(ID1), [=], identifier(ID2).
%command(t_command_assign_string(I,S)) -->  identifier(I), [=], string(S).
command(t_command_assign_bool(I,B)) -->  identifier(I), [=], boolean(B).
command(t_command_if(B,CL1)) --> [if], ['('], boolean(B), [')'], ['{'],
    commandList(CL1), [ '}' ].
command(t_command_ifte(B,CL1,CL2)) --> [if], ['('], boolean(B), [')'], ['{'],
    commandList(CL1), [ '}' ], [else], ['{'], commandList(CL2), ['}'].
command(t_command_while(B,CL)) --> [while], ['('], boolean(B), [')'], ['{'],
    commandList(CL), ['}'].

command(t_command_print(P)) --> [print], printStatement(P).
command(t_command_print_string(P)) --> [print], ['"'], printStrings(P), ['"'].

command(t_command_func(FC)) --> funCall(FC).
command(t_command_funcReturn(I,FC)) --> identifier(I), [=], funCall(FC).

command(t_command_ternary(B,E1,E2)) -->
    boolean(B), [?],  expression(E1), [:], expression(E2).

command(t_command_for_inc(I,E,B,IN,CL)) --> [for], [ '(' ], value(I), [=],
    expression(E), [;], boolean(B), [;],
    increment(IN), [ ')' ], [ '{' ], commandList(CL), [ '}' ].
command(t_command_for_dec(I,E,B,DEC,CL)) --> [for], [ '(' ], value(I), [=],
    expression(E), [;], boolean(B), [ ; ], decrement(DEC), [ ')' ], [ '{' ],
    commandList(CL), [ '}' ].

command(t_command_for_range(I,D1,D2,CL)) --> [for], value(I), [in], [range],  ['('],
    value(D1),  [','], value(D2),  [')'], ['{'], commandList(CL), ['}'].

command(t_command_stringOps(I,OP)) --> identifier(I), [=], stringOps(OP).

command(t_command_increment(I)) --> increment(I).
command(t_command_increment(I)) --> decrement(I).
/*
command(t_command_assignlist(I,L)) --> identifier(I),  [=],  list(L).
command(t_command_assigndict(I,D)) --> identifier(I), [=], dictionary(D).
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%% Command Evaluation %%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

eval_commandList(t_commandList(C, CL), Env, NewEnv) :-
    eval_command(C, Env, Env1), eval_commandList(CL, Env1, NewEnv).

eval_commandList(t_commandList(C), Env, NewEnv) :- eval_command(C, Env, NewEnv).

% command(t_command_assign_expr(I,E)) --> identifier(I),[=],expression(E).
eval_command(t_command_assign_expr(I, E), Env, NewEnv) :-
    eval_expression(E, Env, Env1, Val), eval_identifier_LHS(I,Env1,NewEnv,Val).

% command(t_command_assign_string(I,S)) --> identifier(I),[=],string(S).
%eval_command(t_command_assign_string(I,S), Env, NewEnv) :-
    %eval_string(S,Val), eval_identifier_LHS(I,Env,NewEnv,Val).

% command(t_command_assign_bool(I,B)) --> identifier(I),[=],boolean(B).
eval_command(t_command_assign_bool(I,B), Env, NewEnv) :-
    eval_boolean(B, Env, Env1, Val), eval_identifier_LHS(I,Env1,NewEnv,Val).

% command(t_command_assign_id(I,I)) --> identifier(I),[=],identifier(I).
eval_command(t_command_assign_id(I1,I2), Env, NewEnv) :-
    eval_identifier_RHS(I2,Env,Val), eval_identifier_LHS(I1,Env,NewEnv,Val).

% Execute commandList if boolean is true
eval_command(t_command_if(B, C1), Env, NewEnv) :-
    eval_boolean(B, Env, Env, true), eval_commandList(C1, Env, NewEnv).

% Do nothing if boolean is false
eval_command(t_command_if(B, _C1), Env, Env) :- eval_boolean(B, Env, Env, false).

% Execute commandList if boolean is true
eval_command(t_command_ifte(B, C1, _C2), Env, NewEnv) :-
    eval_boolean(B, Env, Env, true), eval_commandList(C1, Env, NewEnv).

% Execute commandList if boolean is false
eval_command(t_command_ifte(B, _C1, C2), Env, NewEnv) :-
    eval_boolean(B, Env, Env, false), eval_commandList(C2, Env, NewEnv).

% Execute commandList until the boolean is true
eval_command(t_command_while(B, C), Env, NewEnv) :-
    eval_boolean(B, Env, Env, true), eval_commandList(C, Env, Env1),
    eval_command(t_command_while(B, C), Env1, NewEnv).

% Return new environment if the boolean is false
eval_command(t_command_while(B, _C), Env, Env) :- eval_boolean(B, Env, Env, false).

eval_command(t_command_print(P), Env, Env) :- eval_printStatement(P, Env, Env).

eval_command(t_command_print_string(P), Env, Env) :- eval_printString(P, Env, Env).

eval_command(t_command_func(FC), Env, NewEnv) :- eval_funCall(FC, Env, NewEnv).

eval_command(t_command_funcReturn(I, FC), Env, NewEnv) :- eval_id(I, Id),
    eval_funCall(FC, Env, Env1, Val), update(Id, Val, Env1, NewEnv).

eval_command(t_command_ternary(B, E1, _E2), Env, NewEnv) :-
    eval_boolean(B, Env, Env, true), eval_expression(E1, Env, NewEnv, _Val).

eval_command(t_command_ternary(B, _E1, E2), Env, NewEnv) :-
    eval_boolean(B, Env, Env, false), eval_expression(E2, Env, NewEnv, _Val).

eval_command(t_command_for_inc(I,E,B,IN,CL), Env, NewEnv) :-
    eval_command(t_command_assign_expr(I, E), Env, Env1),
    eval_forLoop_inc(t_for(B, IN, CL), Env1, NewEnv).

eval_command(t_command_for_dec(I,E,B,DE,CL), Env, NewEnv) :- eval_expression(E, Env, Env1, Val), 
    check_number(Val), eval_command(t_command_assign_expr(I, E), Env1, Env2),
    eval_forLoop_dec(t_for(B, DE, CL), Env2, NewEnv).

eval_command(t_command_for_range(I, D1, D2, CL), Env, NewEnv) :- 
    eval_command(t_command_assign_expr(I, D1), Env, Env1),
    eval_forLoop_range(t_for_range(I, D1, D2, CL), Env1, NewEnv).

eval_command(t_command_stringOps(I,OP),Env,NewEnv) :- 
    eval_stringOps(OP,Env, Val), eval_identifier_LHS(I,Env,NewEnv,Val).

eval_command(t_command_increment(I),Env,NewEnv) :- eval_increment(I,Env,NewEnv).

eval_command(t_command_decrement(I),Env,NewEnv) :- eval_decrement(I,Env,NewEnv).

check_number(Val) :- number(Val), !.
check_number(Val) :- not(number(Val)), 
    print_message(error, format('~a has to be an integer', [Val])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%% Boolean Grammar %%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

boolean(t_boolean(true)) --> [true].
boolean(t_boolean(false)) --> [false].
boolean(t_boolean_equal(E1, E2)) --> expression(E1), [==], expression(E2).
boolean(t_boolean_not(B)) --> [not], boolean(B).
boolean(t_boolean_and(E1, E2)) --> expression(E1), [and], expression(E2).
boolean(t_boolean_or(E1, E2)) --> expression(E1), [or], expression(E2).
boolean(t_boolean_lt(E1, E2)) --> expression(E1), [<], expression(E2).
boolean(t_boolean_gt(E1, E2)) --> expression(E1), [>], expression(E2).
boolean(t_boolean_lteq(E1, E2)) --> expression(E1), [<=], expression(E2).
boolean(t_boolean_gteq(E1, E2)) --> expression(E1), [>=], expression(E2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%% Boolean Evaluation %%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

eval_boolean(t_boolean(true), Env, Env, true).
eval_boolean(t_boolean(false), Env, Env, false).

eval_boolean(t_boolean_equal(E1, E2), Env, NewEnv, Val) :-
    eval_expression(E1, Env, Env1, Val1), eval_expression(E2, Env1, NewEnv, Val2),
    equal(Val1, Val2, Val).

eval_boolean(t_boolean_not(B), Env, NewEnv, Val) :-
    eval_boolean(B, Env, NewEnv, Value), not(Value, Val).

% And Evaluation
eval_boolean(t_boolean_and(E1, E2), Env, NewEnv, true) :-
    eval_boolean(E1, Env, NewEnv, true), eval_boolean(E2, Env, NewEnv, true).

eval_boolean(t_boolean_and(E1, E2), Env, NewEnv, false) :-
    eval_boolean(E1, Env, NewEnv, false); eval_boolean(E2, Env, NewEnv, false).

eval_boolean(t_boolean_and(E1, E2), Env, NewEnv, Val) :-
    eval_expression(E1, Env, NewEnv, Val1), eval_expression(E2, Env, NewEnv, Val2),
    and(Val1, Val2, Val).

% Or Evaluation
eval_boolean(t_boolean_and(E1, E2), Env, NewEnv, true) :-
    eval_boolean(E1, Env, NewEnv, true); eval_boolean(E2, Env, NewEnv, true).

eval_boolean(t_boolean_and(E1, E2), Env, NewEnv, false) :-
    eval_boolean(E1, Env, NewEnv, false), eval_boolean(E2, Env, NewEnv, false).

eval_boolean(t_boolean_or(E1, E2), Env, NewEnv, Val) :-
    eval_boolean(E1, Env, NewEnv, Val1), eval_boolean(E2, Env, NewEnv, Val2), or(Val1, Val2, Val).

% Relational Operators
eval_boolean(t_boolean_lt(E1, E2), Env, NewEnv, Val) :-
    eval_expression(E1, Env, NewEnv, Val1), eval_expression(E2, Env, NewEnv, Val2),
    less(Val1, Val2, Val).

eval_boolean(t_boolean_gt(E1, E2), Env, NewEnv, Val) :-
    eval_expression(E1, Env, NewEnv, Val1), eval_expression(E2, Env, NewEnv, Val2),
    greater(Val1, Val2, Val).

eval_boolean(t_boolean_lteq(E1, E2), Env, NewEnv, Val) :-
    eval_expression(E1, Env, NewEnv, Val1), eval_expression(E2, Env, NewEnv, Val2),
    less_eq(Val1, Val2, Val).

eval_boolean(t_boolean_gteq(E1, E2), Env, NewEnv, Val) :-
    eval_expression(E1, Env, NewEnv, Val1), eval_expression(E2, Env, NewEnv, Val2),
    greater_eq(Val1, Val2, Val).

% Not predicate to reverse the value of the expression
not(true, false).
not(false, true).

% To check if the two expressions are the same
equal(Val1, Val2, true) :- Val1 = Val2.
equal(Val1, Val2, false) :- Val1 \= Val2.

% And predicate
and(Val1, Val2, true) :- Val1 >= 0, Val2 >= 0.
and(Val1, Val2, false) :- Val1 < 0 ; Val2 < 0.

% Or predicate
or(Val1, Val2, true) :- Val1 >= 0 ; Val2 >= 0.
or(Val1, Val2, false) :- Val1 < 0 , Val2 < 0.

% Less than Predicate
less(Val1, Val2, true) :- Val1 < Val2.
less(Val1, Val2, false) :- Val1 >= Val2.

% Greater than Predicate
greater(Val1, Val2, true) :- Val1 > Val2.
greater(Val1, Val2, false) :- Val1 =< Val2.

% Less than or Equal to Predicate
less_eq(Val1, Val2, true) :- Val1 =< Val2.
less_eq(Val1, Val2, false) :- Val1 > Val2.

% Greater than or Equal to Predicate
greater_eq(Val1, Val2, true) :- Val1 >= Val2.
greater_eq(Val1, Val2, false) :- Val1 < Val2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% Expression Grammar %%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Assign has the least precedence
expression(t_expr_assign(I, E)) --> identifier(I), [=], expression(E).
expression(X) --> expr(X).

% Expression to add and subtract
expr(t_add(X,Y)) --> expr(X), [+], term(Y).
expr(t_subr(X,Y)) --> expr(X), [-], term(Y).
expr(X) --> term(X).

% Multiply and Divide. Precedence is given to this over addition and subtraction
term(t_mult(X,Y)) --> term(X), [*], value(Y).
term(t_div(X,Y)) --> term(X), [/], value(Y).
term(t_mod(X,Y)) --> term(X), ['%'], value(Y).
term(X) --> value(X).

% To tackle brackets -> Highest Priority
value(t_brackets(X)) --> ['('], expression(X), [')'].

% To set the identifier or number from the treenode
value(t_num(X)) --> number(X).
value(t_id(X)) --> identifier(X).
value(t_expr_string(X)) --> string(X).
number(X) --> [X], {number(X)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% Expression Evaluation %%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Update the new value into the lookup table
eval_expression(t_expr_assign(Id, E), Env, NewEnv, Val1) :-
    eval_expression(E, Env, Env1, Val1), eval_identifier_LHS(Id,Env1,NewEnv,Val1).

% E -> E + E
eval_expression(t_add(X,Y), Env, NewEnv, Val) :-
    eval_expression(X, Env, Env1, Val1), eval_expression(Y, Env1, NewEnv, Val2),
    check_numbers(Val1, Val2), Val is Val1 + Val2.

% E -> E - E
eval_expression(t_subr(X,Y), Env, NewEnv, Val) :-
    eval_expression(X, Env, Env1, Val1), eval_expression(Y, Env1, NewEnv, Val2), 
    check_numbers(Val1, Val2), Val is Val1 - Val2.

% E -> E * E
eval_expression(t_mult(X,Y), Env, NewEnv, Val) :-
    eval_expression(X, Env, Env1, Val1), eval_expression(Y, Env1, NewEnv, Val2),
    check_numbers(Val1, Val2), Val is Val1 * Val2.

% E -> E / E
eval_expression(t_div(X,Y), Env, NewEnv, Val) :-
    eval_expression(X, Env, Env1, Val1), eval_expression(Y, Env1, NewEnv, Val2),
    check_numbers(Val1, Val2), Val is Val1 / Val2.

eval_expression(t_mod(X,Y), Env, NewEnv, Val) :-
    eval_expression(X, Env, Env1, Val1), eval_expression(Y, Env1, NewEnv, Val2),
    check_numbers(Val1, Val2), Val is mod(Val1, Val2).

% E -> (E)
eval_expression(t_brackets(X), Env, NewEnv, Val) :- eval_expression(X, Env, NewEnv, Val).

% Return the value to the same environment
eval_expression(t_num(X), Env, Env, X).

eval_expression(t_id(X),Env,Env,Val):- eval_identifier_RHS(X,Env,Val).

eval_expression(t_expr_string(X),Env,Env,Val):- eval_string(X,Val).

check_numbers(Val1, Val2) :- number(Val1), number(Val2), !.
check_numbers(Val1, Val2) :- 
    print_message(error, format('Illegal operation between ~a and ~a.', [Val1, Val2])), fail.

% To return true or false based on the presence of the term. 
check_lookup(_, [], false).
check_lookup(Id, [(Id, _) | _], true).
check_lookup(Id, [_ | T], Val) :- lookup(Id, T, Val).

% To lookup the value of a variable in a particular environment.
lookup(Id, [(Id, Val) | _], Val).
lookup(Id, [_ | T], Val) :- lookup(Id, T, Val).

% To update values in the environment
update(Id, Val, [], [(Id, Val)]).
update(Id, Val, [(Id, _) | T], [(Id, Val) | T]).
update(Id, Val, [H | T], [H | R]) :-
    H \= (Id, _), update(Id, Val, T, R).

% To 
updateAppend(Id, Val, [], [(Id, Val)]).
updateAppend(Id, Val, [(Id, Prev) | T], [(Id, Result) | T]) :- append(Prev, [Val], Result).
updateAppend(Id, Val, [H | T], [H | R]) :-
    H \= (Id, _), updateAppend(Id, Val, T, R).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% Function Declaration %%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

functionDeclaration(t_funcDeclr(I, PL, DL, CL)) --> [func], identifier(I), ['('],
    parameterList(PL), [')'], ['{'], funcBlock(t_funcBlock(DL, CL)), ['}'].

functionDeclaration(t_funcDeclr(I, PL, DL, CL, E)) --> [func], identifier(I), ['('],
    parameterList(PL), [')'], ['{'], funcBlock(t_funcBlock(DL, CL)), return(E), ['}'].

parameterList(t_parList(P, PL)) --> parameter(P), [,], parameterList(PL).
parameterList(t_parList(P)) --> parameter(P).
parameterList(t_parList()) --> [].
parameter(I) --> identifier(I).

funCall(t_funCall(I, CPL)) --> value(I), ['('], callParameterList(CPL), [')'].
callParameterList(t_callParList(CP, CPS)) -->
    callParameter(CP), [,], callParameterList(CPS).
callParameterList(t_callParList(CP)) --> callParameter(CP).
callParameterList(t_callParList()) --> [].
callParameter(I) --> value(I).
return(t_return(E)) --> [return], expression(E).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%% Function Evaluation %%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Function Declaration for functions without return statement
eval_functionDeclaration(t_funcDeclr(I, PL, DL, CL), Env, NewEnv) :-
    eval_id(I, Id), update(Id, [PL, t_funcBlock(DL, CL)], Env, NewEnv).

% Function Declaration for functions with return statement
eval_functionDeclaration(t_funcDeclr(I, PL, DL, CL, E), Env, NewEnv) :-
    eval_id(I, Id), update(Id, [PL, t_funcBlock(DL, CL), E], Env, NewEnv).

% Function Call for functions without return
eval_funCall(t_funCall(I, CPL), Env, Env) :-
     eval_id(I, Id),  lookup(Id, Env, [P, K]),
    eval_parameters(I, CPL, P, Env, Env1), lookup(Id, Env1, [P, K | T]), eval_funcBlock(K, T, _Env2), !.

% Function Call for functions with return
eval_funCall(t_funCall(I, CPL), Env, Env, Val) :- eval_id(I, Id),  lookup(Id, Env, [P, K, R]),
    eval_parameters(I, CPL, P, Env, Env1), lookup(Id, Env1, [P, K, R | T]), eval_funcBlock(K, T, Env2),
    eval_return(R, Env2, _Env3, Val), !.

% To check if a particular function is declared or not. 
eval_funCall(t_funCall(I, _CPL), Env, Env, _Val) :- eval_id(I, Id), check_lookup(Id, Env, false),
    print_message(error, format('Function "~s" does not exist!', [Id])), fail.

% To evaluate the return statement
eval_return(t_return(E), Env, NewEnv, Val) :- eval_expression(E, Env, NewEnv, Val).

% To check the length of the parameters passed and merge them to a tuple for evaluation.
eval_parameters(I, CPL, P, Env, NewEnv) :-
    eval_id(I, Id), eval_callParList(CPL, Env, Env1, [], ComPar), eval_parList(P, Env1, Env2, [], Par),
    localScope(Id, ComPar, Par, Env2, NewEnv).

% Produce error if the parameters don't match
eval_parameters(I, CPL, P, Env, Env2) :-
    eval_id(I, Id), eval_callParList(CPL, Env, Env1, [], ComPar), eval_parList(P, Env1, Env2, [], Par),
    find_length(ComPar, Val1), find_length(Par, Val2), Val1 =\= Val2,
    print_message(error, format('Parameters passed to function "~s" do not match with its definition!', [Id])), 
    fail.

% To get all the function call parameters
eval_callParList(t_callParList(P, PS), Env, Env, Val, Res) :-
    eval_expression(P, Env, Env, Id), append(Val, Id, Result), eval_callParList(PS, Env, Env, [Result], Res).

eval_callParList(t_callParList(P), Env, Env, Val, Result) :-
    eval_expression(P, Env, Env, Id), append(Val, Id, Result).

% Empty parameter call list
eval_callParList(t_callParList(), Env, Env, Val, Val).

% To get all function parameters
eval_parList(t_parList(P, PS), Env, Env, Val, Res) :-
    eval_id(P, Id), append(Val, Id, Result), eval_parList(PS, Env, Env, [Result], Res).

eval_parList(t_parList(P), Env, Env, Val, Result) :-
    eval_id(P, Id), append(Val, Id, Result).

% Empty parameter list
eval_parList(t_parList(), Env, Env, Val, Val).

% To join all the parameters into a tuple
localScope(_, [], [], Env, Env).

localScope(Id, H, H1, Env, NewEnv) :- updateAppend(Id, (H1, H), Env, NewEnv).

localScope(Id, [H | T], [H1 | T1], Env, NewEnv) :- updateAppend(Id, (H1, H), Env, Env1),
    localScope(Id, T, T1, Env1, NewEnv).

% To get from the environment
eval_id(t_id(t_varID(X)), X).
eval_id(t_varID(X), X).
eval_id(t_listID(_,_), list_value).
eval_id(t_listID_identifier(_,_), list_value).
eval_id(t_dictID(_,_), dictionary_value).


% To find the length of parameter list.
find_length([], 0).
find_length([_H | T], Length) :- find_length(T, Val), Length is Val + 1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%% Print Grammar %%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

printStatement(t_print_var(I, PS)) --> identifier(I), [','], printStatement(PS).
printStatement(t_print_var(I)) --> identifier(I).

printStrings(t_print_string(I, PS)) --> printString(I), [','], printStrings(PS).
printStrings(t_print_string(I)) --> printString(I).
printString(S) --> [S], { atom(S) }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%% Print Evaluation %%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Print value of variables
eval_printStatement(t_print_var(P, PS), Env, Env) :- eval_id(P, Id), eval_identifier_RHS(P, Env, Val),
    print_message(debug, format('~w = ~w', [Id, Val])), eval_printStatement(PS, Env, Env).

eval_printStatement(t_print_var(P), Env, Env) :- eval_id(P, Id), eval_identifier_RHS(P, Env, Val),
    print_message(debug, format('~w = ~w', [Id, Val])).

% Print strings on the result screen.
eval_printString(t_print_string(P, PS), Env, Env) :-
    print_message(debug, format('~w', [P])), eval_printString(PS, Env, Env).

eval_printString(t_print_string(P), Env, Env) :-
    print_message(debug, format('~w', [P])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%% For Loop Grammar %%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

increment(t_increment(I)) --> identifier(I), [++].
decrement(t_decrement(I)) --> identifier(I), [--].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%% For Loop Evaluation %%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Incrementing for loop
eval_forLoop_inc(t_for(B, IN, CL), Env, NewEnv) :-
    eval_boolean(B, Env, Env1, true), eval_commandList(CL, Env1, Env2), 
    eval_increment(IN, Env2, Env3), eval_forLoop_inc(t_for(B, IN, CL), Env3, NewEnv).

% To return current environment when boolean fails.
eval_forLoop_inc(t_for(B, _IN, _C), Env, Env) :- eval_boolean(B, Env, Env, false).

% To increment the value of an identifier
eval_increment(t_increment(Id), Env, NewEnv) :- eval_identifier_RHS(Id,Env,Val),
    Val1 is Val + 1, eval_identifier_LHS(Id,Env,NewEnv,Val1).

% Incrementing for loop
eval_forLoop_dec(t_for(B, DE, CL), Env, NewEnv) :-
    eval_boolean(B, Env, Env1, true), eval_commandList(CL, Env1, Env2), 
    eval_decrement(DE, Env2, Env3), eval_forLoop_dec(t_for(B, DE, CL), Env3, NewEnv).

% To return current environment when boolean fails.
eval_forLoop_dec(t_for(B, _DE, _C), Env, Env) :- eval_boolean(B, Env, Env, false).

% To decrement the value of an identifier
eval_decrement(t_decrement(Id), Env, NewEnv) :- eval_identifier_RHS(Id,Env,Val),
    Val1 is Val - 1, eval_identifier_LHS(Id,Env,NewEnv,Val1).

% Range Loop
eval_forLoop_range(t_for_range(I, D1, D2, CL), Env, NewEnv) :-
    eval_boolean(t_boolean_lt(I, D2), Env, Env1, true),
    eval_commandList(CL, Env1, Env2), eval_expression(I, Env2, Env2, Val), Val1 is Val + 1,
    eval_identifier_LHS(I, Env2, Env3, Val1), eval_forLoop_range(t_for_range(I, D1, D2, CL), Env3, NewEnv).

% To return current environment when boolean fails.
eval_forLoop_range(t_for_range(I, _D1, D2, _CL), Env, NewEnv) :-
    eval_boolean(t_boolean_lt(I, D2), Env, Env, false), eval_identifier_LHS(I, Env, NewEnv, 0).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% Identifier Grammar %%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Different formats for identifiers: 
identifier(t_varID(I)) --> varIdentifier(I).

%List index is specified as a number.
identifier(t_listID(I, D)) --> varIdentifier(I), [ '[' ], number(D), [ ']' ].

%List index is specified as a variable.
identifier(t_listID_identifier(I, D)) --> varIdentifier(I), ['['], varIdentifier(D), [']'].

%Dictionary identifier with key string.
identifier(t_dictID(I,S)) --> varIdentifier(I), ['['], string(S), [']'].
varIdentifier(I) -->  [I], {atom(I)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% Identifier Evaluation %%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%When identifier appears on LHS, update its value.
eval_identifier_LHS(t_id(t_varID(Id)),Env,NewEnv,Val):- update(Id,Val,Env,NewEnv).
eval_identifier_LHS(t_varID(Id),Env,NewEnv,Val):- update(Id,Val,Env,NewEnv).

%Update the value at a specific list index.
eval_identifier_LHS(t_listID(Id,Pos),Env,NewEnv,Val):- updateList(Id,Pos,Val,Env,NewEnv).
eval_identifier_LHS(t_listID_identifier(Id1,Id2),Env,NewEnv,Val):- lookup(Id2,Env,Pos), updateList(Id1,Pos,Val,Env,NewEnv).

%Update the value for a specific dictionary key.
eval_identifier_LHS(t_dictID(Id,Key),Env,NewEnv,Val):- eval_string(Key,Key1), updateDict(Id,Key1,Val,Env,NewEnv).


%When identifier appears on RHS, lookup its value.
eval_identifier_RHS(t_varID(Id),Env,Val):- lookup(Id,Env,Val).

%Lookup the value at a specific list index.
eval_identifier_RHS(t_listID(Id,Pos),Env,Val):- lookupList(Id,Pos,Env,Val).
eval_identifier_RHS(t_listID_identifier(Id1,Id2),Env,Val):- lookup(Id2,Env,Pos), lookupList(Id1,Pos,Env,Val).

%Lookup the value for a specific dictionary key.
eval_identifier_RHS(t_dictID(Id,S),Env,Val):- eval_string(S,Key), lookupDict(Id,Key,Env,Val).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%% Lists Grammar %%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

list(t_list(IL)) --> [ '[' ], identifierList(IL), [ ']' ].
identifierList(t_idList(LV,ELE)) --> listValues(LV), element(ELE).
identifierList(t_idList()) --> [].
%Added comma between elements
listValues(t_listVal(ELE, LV)) --> element(ELE), [,], listValues(LV).
listValues(t_listVal()) --> [].
element(t_element_expr(E))  --> expression(E).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%% Lists Evaluation %%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Parses the items in the list one by one
eval_list(t_list(X),ID,Env,Env1):- eval_identifierList(X,ID,Env,Env1).
eval_identifierList(t_idList(LV,ELE),ID,Env,Env2):- eval_listValues(LV,ID,Env,Env1), eval_element(ELE,ID,Env1,Env2).
eval_identifierList(t_idList(),_,Env,Env).
eval_listValues(t_listVal(ELE,LV),ID,Env,Env2):- eval_element(ELE,ID,Env,Env1), eval_listValues(LV,ID,Env1,Env2).
eval_listValues(t_listVal(),_,Env,Env).
eval_element(t_element_expr(X),ID,Env,Env2):- eval_expression(X,Env,Env1,Val), initializeList(ID,Val,Env1,Env2).

%Initializes the list
initializeList(ID,X,[],[(ID,[X])]).
initializeList(ID,X,[(ID,T)|TL],[(ID,L)|TL]):- append(T,[X],L).
initializeList(ID,X,[H|T],[H|Env]):- H \= (ID,_), initializeList(ID,X,T,Env).

%Finds the list identifier in the environment
lookupList(ID,Pos,[(ID,T)|_],Val):- lookupList(T,Pos,Val).
lookupList(ID,Pos,[H|T],Val):- H \= (ID,_), lookupList(ID,Pos,T,Val).

%Finds the item within the list
lookupList([_|T],Pos,Val):- Pos \= 0, Pos1 is Pos - 1, lookupList(T,Pos1,Val).
lookupList([H|_],0,H).

%Finds the list identifier in the environment
updateList(ID,Pos,Val,[(ID,T)|TL],[(ID,L)|TL]):- updateList(T,Pos,Val,L).
updateList(ID,Pos,Val,[H|T],[H|L]):-  H \= (ID,_), updateList(ID,Pos,Val,T,L).

%Updates the element within the list
updateList([H|T],Pos,Val,[H|L]):- Pos \= 0, Pos1 is Pos - 1, updateList(T,Pos1,Val,L).
updateList([_|T],0,Val,[Val|T]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%% Dictionary Grammar %%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dictionary(t_dictionary(DI)) --> ['{'], dictionaryItems(DI), ['}'].

dictionaryItems(t_dictItems(DV, DE)) --> dictionaryValues(DV), dictionaryElement(DE).
dictionaryItems(t_dictItems()) --> [].

dictionaryValues(t_dictValues(DE, DV)) --> dictionaryElement(DE), [,], dictionaryValues(DV).
dictionaryValues(t_dictValues()) --> [].

dictionaryElement(t_dictElement(S, DV)) --> string(S), [:], dictionaryValue(DV).
dictionaryValue(t_dictVal_expr(E)) --> expression(E).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%% Dictionary Evaluation %%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%To parse the dictionary key, value pairs one by one
eval_dictionary(t_dictionary(DI),ID,Env,Env1):- eval_dictionaryItems(DI,ID,Env,Env1).
eval_dictionaryItems(t_dictItems(DV,DE),ID,Env,Env2):- eval_dictionaryValues(DV,ID,Env,Env1), eval_dictionaryElement(DE,ID,Env1,Env2).
eval_dictionaryItems(t_dictItems(),_,Env,Env).
eval_dictionaryValues(t_dictValues(DE,DV),ID,Env,Env2):- eval_dictionaryElement(DE,ID,Env,Env1), eval_dictionaryValues(DV,ID,Env1,Env2).
eval_dictionaryValues(t_dictValues(),_,Env,Env).
eval_dictionaryElement(t_dictElement(S,DV),ID,Env,Env1):- eval_string(S,Key), eval_dictionaryValue(DV,Value), initializeDict(ID,Key,Value,Env,Env1).
eval_dictionaryValue(t_dictVal_expr(E),Val):- eval_expression(E,_,_,Val).

%To initialize the dictionary identifier with the dictionary items 
initializeDict(ID,Key,Value,[],[(ID,[(Key,Value)])]).
initializeDict(ID,Key,Value,[(ID,T)|TL],[(ID,L)|TL]):- append(T,[(Key,Value)],L).
initializeDict(ID,Key,Value,[H|T],[H|Env]):- H \= (ID,_), initializeDict(ID,Key,Value,T,Env).

%Finds the dictionary identifier in the environment
lookupDict(ID,Key,[(ID,T)|_],Val):- lookupDict(T,Key,Val).
lookupDict(ID,Key,[H|T],Val):- H \= (ID,_), lookupDict(ID,Key,T,Val).

%Finds the value for a given key within the dictionary
lookupDict([(Key,Val)|_],Key,Val).
lookupDict([H|T],Key,Val):- H \= (Key,_), lookupDict(T,Key,Val).

%To update the dictionary
updateDict(ID,Key,Value,[(ID,T)|TL],[(ID,L)|TL]):- updateDict(T,Key,Value,L).
updateDict(ID,Key,Value,[H|T],[H|Env]):-  H \= (ID,_), updateDict(ID,Key,Value,T,Env).

%Updates a particular key within the dictionary
updateDict([(Key,_)|T],Key,Value,[(Key,Value)|T]).
updateDict([H|T],Key,Value,[H|L]):- H \= (Key,_), updateDict(T,Key,Value,L).
