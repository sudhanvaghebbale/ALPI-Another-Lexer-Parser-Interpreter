% Tables are used to tackle left recursion
:- table expr/3, term/3, value/3.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%% Program Section %%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

program(t_program(K)) --> [begin], beginningBlock(K), [end].
eval_program(t_program(K), Env, FinalEnv) :- eval_block(K, Env, FinalEnv).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%% Block Section %%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

beginningBlock(t_beginning_block(DL, CL)) --> declarationList(DL), [;],  commandList(CL), [;].
block(t_block(DL, CL)) --> ['{'], declarationList(DL), [;],  commandList(CL), [;], [ '}' ].

eval_block(t_beginning_block(D, C), Env, NewEnv) :- 
    eval_declarationList(D, Env, Env1), eval_commandList(C, Env1, NewEnv).

eval_block(t_block(D, C), Env, NewEnv) :- 
    eval_declarationList(D, Env, Env1), eval_commandList(C, Env1, NewEnv).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% Declaration Grammar %%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

declarationList(t_declarationList(D, DL)) --> declaration(D), [;], declarationList(DL).
declarationList(t_declarationList(D)) --> declaration(D).

declaration(t_declaration(I)) -->  dataType, value(I).
declaration(t_funcDeclaration(FD)) --> functionDeclaration(FD).

declaration(t_init(I, N)) --> [num], value(I), [=], value(N).
declaration(t_init_string(I, S)) --> [str], value(I), [=], string(S).
declaration(t_init_bool(I, B)) --> [bool], value(I), [=], boolean(B).

dataType --> [num] ; [str] ; [bool] ; [list] ; [dict].

string(t_string(S)) --> ['"'], stringTerm(S), ['"'].
string(t_string(S)) --> ['\''], stringTerm(S), ['\''].
stringTerm(t_stringTerm(S)) --> [S], {atom(S)}.
stringTerm(t_stringTerm()) --> [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% Declaration Evaluation %%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

eval_declarationList(t_declarationList(C, CL), Env, NewEnv) :- 
    eval_declaration(C, Env, Env1), eval_declarationList(CL, Env1, NewEnv).

% declarationList(t_decList(D)) --> declaration(D).
eval_declarationList(t_declarationList(C), Env, NewEnv) :- eval_declaration(C, Env, NewEnv).

% declaration(t_declaration(I)) -->  dataType, identifier(I).
eval_declaration(t_declaration(I), Env, NewEnv) :-
    eval_id(I, Id), lookup(Id, Env, Val), update(Id, Val, Env, NewEnv).

eval_declaration(t_funcDeclaration(FD), Env, NewEnv) :- eval_functionDeclaration(FD, Env, NewEnv).

% If value not in lookup table, put a garbage value.
eval_declaration(t_declaration(I), Env, NewEnv) :-
    eval_id(I, Id), not(lookup(Id, Env, _)), update(Id, _, Env, NewEnv).

% declaration(t_init(I, _)) --> identifier(I), [=], integer/float/String.
eval_declaration(t_init(I,D), Env, NewEnv) :-
    eval_id(I, Id), eval_expression(D, Env, Env1, Val1), update(Id, Val1, Env1, NewEnv).

% bool(t_bool(true)) --> [true].
eval_declaration(t_init_bool(I, V), Env, NewEnv) :-
	eval_id(I, Id), eval_boolean(V, Env, Env, true), update(Id, true, Env, NewEnv).

% bool(t_bool(false)) --> [false].
eval_declaration(t_init_bool(I, V), Env, NewEnv) :-
	eval_id(I, Id), eval_boolean(V, Env, Env, false), update(Id, false, Env, NewEnv).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%% Command Grammar %%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

commandList(t_commandList(C, CL)) --> command(C),  [ ; ],  commandList(CL).
commandList(t_commandList(C))  --> command(C).

command(t_command_assign(I,E)) -->  value(I), [=], expression(E).
command(t_command_ifte(B,CL1,CL2)) --> [if], ['('], boolean(B), [')'], ['{'], 
    commandList(CL1), [;], [ '}' ], [else], ['{'], commandList(CL2), ['}'].
command(t_command_while(B,CL)) --> [while], ['('], boolean(B), [')'], ['{'], 
    commandList(CL), ['}'].
command(t_command_block(K)) --> block(K).

command(t_command_func(FC)) --> funCall(FC).
command(t_command_funcReturn(I,FC)) --> value(I), [=], funCall(FC).

command(t_command_print(PS)) --> printStatement(PS).
command(t_command_ternary(B,E1,E2)) --> 
    boolean(B), [?],  expression(E1), [:], expression(E2).

command(t_command_for_inc(I,E,B,IN,CL)) --> [for], [ '(' ], value(I), [=], 
    expression(E), [ ; ], boolean(B), [ ; ], 
    increment(IN), [ ')' ], [ '{' ], commandList(CL), [ '}' ].
command(t_command_for_dec(I,E,B,DEC,CL)) --> [for], [ '(' ], value(I), [=], 
    expression(E), [ ; ], boolean(B), [ ; ], decrement(DEC), [ ')' ], [ '{' ], 
    commandList(CL), [ '}' ].

command(t_command_for_range(I,D1,D2,CL)) --> [for], value(I), [in], [range],  ['('], 
    value(D1),  [','], value(D2),  [')'], ['{'], commandList(CL), ['}'].

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

% command(t_command_assignexpr(I,E)) -->  identifier(I), [=], expression(E).
eval_command(t_command_assign(I, E), Env, NewEnv) :- 
    eval_id(I, Id), eval_expression(E, Env, Env1, Val), update(Id, Val, Env1, NewEnv).

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

eval_command(t_command_block(K), Env, NewEnv) :- eval_block(K, Env, NewEnv).

eval_command(t_command_func(FC), Env, NewEnv) :- eval_funCall(FC, Env, NewEnv). 

eval_command(t_command_ternary(B, E1, _E2), Env, NewEnv) :- 
    eval_boolean(B, Env, Env, true), eval_expression(E1, Env, NewEnv, _Val).

eval_command(t_command_ternary(B, _E1, E2), Env, NewEnv) :- 
    eval_boolean(B, Env, Env, false), eval_expression(E2, Env, NewEnv, _Val).

eval_command(t_command_for_inc(I,E,B,IN,CL), Env, NewEnv) :- 
    eval_command(t_command_assign(I, E), Env, Env1), 
    eval_forLoop_inc(t_for(B, IN, CL), Env1, NewEnv).

eval_command(t_command_for_dec(I,E,B,DE,CL), Env, NewEnv) :- 
    eval_command(t_command_assign(I, E), Env, Env1), 
    eval_forLoop_dec(t_for(B, DE, CL), Env1, NewEnv).

eval_command(t_command_for_range(I, D1, D2, CL), Env, NewEnv) :- 
    eval_command(t_command_assign(I, D1), Env, Env1), 
    eval_forLoop_range(t_for_range(I, D1, D2, CL), Env1, NewEnv).
    
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

eval_boolean(true, Env, Env, true).
eval_boolean(false, Env, Env, false).

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
expression(t_command_assign(I, E)) --> value(I), [=], expression(E).
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
value(t_num(X)) --> [X], {number(X)}.
value(t_id(I)) --> [I], {atom(I)}.
% value(t_listID(L)) --> listIdentifier(L).
% value(t_dictionaryID(D)) --> dictionaryIdentifier(D).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% Expression Evaluation %%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Update the new value into the lookup table
eval_expression(t_command_assign(I, E), Env, NewEnv, Val1) :- 
    eval_id(I, Id), eval_expression(E, Env, Env1, Val1), update(Id, Val1, Env1, NewEnv).

% E -> E + E
eval_expression(t_add(X,Y), Env, NewEnv, Val) :- 
    eval_expression(X, Env, Env1, Val1), eval_expression(Y, Env1, NewEnv, Val2), 
    Val is Val1 + Val2.

% E -> E - E 
eval_expression(t_subr(X,Y), Env, NewEnv, Val) :- 
    eval_expression(X, Env, Env1, Val1), eval_expression(Y, Env1, NewEnv, Val2), 
    Val is Val1 - Val2.
                
% E -> E * E
eval_expression(t_mult(X,Y), Env, NewEnv, Val) :- 
    eval_expression(X, Env, Env1, Val1), eval_expression(Y, Env1, NewEnv, Val2), 
    Val is Val1 * Val2.

% E -> E / E
eval_expression(t_div(X,Y), Env, NewEnv, Val) :- 
    eval_expression(X, Env, Env1, Val1), eval_expression(Y, Env1, NewEnv, Val2), 
    Val is Val1 / Val2.       

% E -> (E)
eval_expression(t_brackets(X), Env, NewEnv, Val) :- eval_expression(X, Env, NewEnv, Val).

% Get the value of identifier from the lookup table
eval_expression(t_id(I), Env,  Env, Val) :- lookup(I, Env, Val).

% Return the value to the same environment
eval_expression(t_num(X), Env, Env, X).

% To extract id from the tree node
eval_id(t_id(I), I).
    
% To find the value of the variable in the particular environment.
lookup(Id, [(Id, Val) | _], Val).
lookup(Id, [_ | T], Val) :- lookup(Id, T, Val).

% To update values in the environment
update(Id, Val, [], [(Id, Val)]).
update(Id, Val, [(Id, _) | T], [(Id, Val) | T]).
update(Id, Val, [H | T], [H | R]) :- 
    H \= (Id, _), update(Id, Val, T, R).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% Function Declaration %%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

functionDeclaration(t_funcDeclr(I, PL, CL, E)) --> [func], value(I), ['('], 
    parameterList(PL), [')'], ['{'], commandList(CL), [;], 
    [return], expression(E), [;], ['}'].
parameterList(t_parList(P, PL)) --> parameter(P), [','], parameterList(PL).
parameterList(t_parList(P)) --> parameter(P).
parameter(t_parameter(I)) --> value(I).

funCall(t_funCall(I, CPL)) --> value(I), ['('], callParameterList(CPL), [')'].
callParameterList(t_callParList(CP, CPS)) --> 
    callParameter(CP), [','], callParameterList(CPS).
callParameterList(t_callParList(CP)) --> callParameter(CP).
callParameter(t_callPar(I)) --> value(I).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%% Function Evaluation %%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

eval_functionDeclaration(t_funcDeclr(I, _PL, CL, _E), Env, NewEnv) :- 
    eval_id(I, Id), update(Id, CL, Env, NewEnv).

eval_funCall(t_funCall(I, _CPL), Env, NewEnv) :- eval_id(I, Id), lookup(Id, Env, Val),
    eval_commandList(Val, Env, NewEnv).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%% Print Statement %%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

printStatement(t_print(IT)) --> [print], ['('], value(IT), [')'].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%% For Loop Grammar %%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

increment(t_increment(IN)) --> value(IN), [++].
decrement(t_decrement(DE)) --> value(DE), [--].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%% For Loop Evaluation %%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

eval_forLoop_inc(t_for(B, IN, CL), Env, NewEnv) :- 
    eval_boolean(B, Env, Env1, true), eval_increment(IN, Env1, Env2), 
    eval_commandList(CL, Env2, Env3), eval_forLoop_inc(t_for(B, IN, CL), Env3, NewEnv).
    
eval_forLoop_inc(t_for(B, _IN, _C), Env, Env) :- eval_boolean(B, Env, Env, false). 

eval_increment(t_increment(IN), Env, NewEnv) :- eval_id(IN, Id), lookup(Id, Env, Val), 
    Val1 is Val + 1, update(Id, Val1, Env, NewEnv).

eval_forLoop_dec(t_for(B, DE, CL), Env, NewEnv) :- 
    eval_boolean(B, Env, Env1, true), eval_decrement(DE, Env1, Env2), 
    eval_commandList(CL, Env2, Env3), eval_forLoop_dec(t_for(B, DE, CL), Env3, NewEnv).
    
eval_forLoop_dec(t_for(B, _DE, _C), Env, Env) :- eval_boolean(B, Env, Env, false). 

eval_decrement(t_decrement(IN), Env, NewEnv) :- eval_id(IN, Id), lookup(Id, Env, Val), 
    Val1 is Val - 1, update(Id, Val1, Env, NewEnv).

eval_forLoop_range(t_for_range(I, D1, D2, CL), Env, NewEnv) :- 
    eval_boolean(t_boolean_lt(I, D2), Env, Env1, true),
    eval_commandList(CL, Env1, Env2), eval_id(I, Id), lookup(Id, Env2, Val), Val1 is Val + 1, 
    update(Id, Val1, Env2, Env3), eval_forLoop_range(t_for_range(I, D1, D2, CL), Env3, NewEnv).

eval_forLoop_range(t_for_range(I, _D1, D2, _CL), Env, Env) :- 
    eval_boolean(t_boolean_lt(I, D2), Env, Env, false).
