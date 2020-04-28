% Tables are used to tackle left recursion
:- table expr/3, term/3, value/3.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%% Program Section %%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

program(t_program(K)) --> block(K).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%% Block Section %%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

block(t_block(DL, CL)) --> [begin], declarationList(DL), commandList(CL), [end].

funcBlock(t_funcBlock(DL, CL)) --> declarationList(DL), commandList(CL).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% Identifier Grammar %%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

identifier(t_varID(I)) --> varIdentifier(I).
%identifier(t_listIdentifier(L)) --> listIdentifier(L).
%identifier(t_dictionaryIdentifier(D)) --> dictionaryIdentifier(D).
identifier(t_listID(I, D)) --> varIdentifier(I), [ '[' ], number(D), [ ']' ].
identifier(t_dictID(I,S)) --> varIdentifier(I), ['['], string(S), [']'].
varIdentifier(I) -->  [I], {atom(I)}.


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
%%%%%%%%%%%%%%%%%%%%%%%%%% Command Grammar %%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

commandList(t_commandList(C, CL)) --> command(C), commandList(CL).
commandList(t_commandList(C))  --> command(C).
%commandList(t_commandList()) --> [].

command(t_command_assign_expr(I,E)) -->  identifier(I), [=], expression(E).
command(t_command_assign_id(ID1,ID2)) -->  identifier(ID1), [=], identifier(ID2).
command(t_command_assign_string(I,S)) -->  identifier(I), [=], string(S).
command(t_command_assign_bool(I,B)) -->  identifier(I), [=], boolean(B).
command(t_command_ifte(B,CL1,CL2)) --> [if], ['('], boolean(B), [')'], ['{'],
    commandList(CL1), [ '}' ], [else], ['{'], commandList(CL2), ['}'].
command(t_command_while(B,CL)) --> [while], ['('], boolean(B), [')'], ['{'],
    commandList(CL), ['}'].

command(t_command_print(P)) --> [print], ['"'], printStatement(P), ['"'].

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

command(t_command_block(K)) --> block(K).

command(t_command_stringOps(I,OP)) --> identifier(I), [=], stringOps(OP).

/*
command(t_command_assignlist(I,L)) --> identifier(I),  [=],  list(L).
command(t_command_assigndict(I,D)) --> identifier(I), [=], dictionary(D).
*/


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
% value(t_id(I)) --> [I], {atom(I)}.
% value(t_listID(L)) --> listIdentifier(L).
% value(t_dictionaryID(D)) --> dictionaryIdentifier(D).
number(X) --> [X], {number(X)}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% Function Declaration %%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

functionDeclaration(t_funcDeclr(I, PL, DL, CL)) --> [func], identifier(I), ['('],
    parameterList(PL), [')'], ['{'], funcBlock(t_funcBlock(DL, CL)), ['}'].

functionDeclaration(t_funcDeclr(I, PL, DL, CL, E)) --> [func], identifier(I), ['('],
    parameterList(PL), [')'], ['{'], funcBlock(t_funcBlock(DL, CL)), return(E), ['}'].

parameterList(t_parList(P, PL)) --> parameter(P), [,], parameterList(PL).
parameterList(t_parList(P)) --> parameter(P).
parameter(I) --> identifier(I).

funCall(t_funCall(I, CPL)) --> value(I), ['('], callParameterList(CPL), [')'].
callParameterList(t_callParList(CP, CPS)) -->
    callParameter(CP), [,], callParameterList(CPS).
callParameterList(t_callParList(CP)) --> callParameter(CP).
callParameter(I) --> value(I).
return(t_return(E)) --> [return], expression(E).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%% Print Grammar %%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

printStatement(t_print_var(I, PS)) --> identifier(I), [','], printStatement(PS).
printStatement(t_print_var(I)) --> identifier(I).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%% For Loop Grammar %%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

increment(t_increment(I)) --> varIdentifier(I), [++].
decrement(t_decrement(I)) --> varIdentifier(I), [--].


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
element(t_element_string(S))  --> string(S).

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
dictionaryValue(t_dictVal_string(S)) --> string(S).
