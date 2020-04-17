:- table expression/3, term/3, value/3.

%----------------------------------- Main Section ----------------------------------------
program(t_program(K)) --> [begin], beginningBlock(K), [end].
beginningBlock(t_beginningBlock(DL, CL)) --> declarationList(DL), [ ; ],  commandList(CL).
block(t_block(DL, CL)) --> ['{'], declarationList(DL), [ ; ],  commandList(CL),  [ '}' ].

%---------------------------------- Declaration Section ---------------------------------------
declarationList(t_decList(D, DL)) --> declaration(D), [ ; ], declarationList(DL).
declarationList(t_decList(D)) --> declaration(D).
declaration(t_declaration(I)) -->  dataType, identifier(I).
declaration(t_declaration(FD)) --> functionDeclaration(FD).
dataType --> [int] ; [float] ; [str] ; [bool] ; [list] ; [dict].
% Created different types of identifiers. Because list and dictionary
% can appear on LHS as well.
identifier(t_varID(I)) --> varIdentifier(I).
identifier(t_listID(L)) --> listIdentifier(L).
identifier(t_dictionaryID(D)) --> dictionaryIdentifier(D).
varIdentifier(I) -->  [I], {atom(I)}.
integer(D) --> [D], {integer(D)}.
float(F) --> [F], {float(F)}.
bool --> [true] ; [false].

%------------------------------------------------ Strings -------------------------------------------
%Added inverted commas for the quotes
string(t_string(I,S)) --> ['"'], [I], string(S), ['"'], {atom(I)} ; [].

%--------------------------------------------- Functions --------------------------------------------
functionDeclaration(t_functionDecl(I, PL, CL, E)) --> [def],  returnType, identifier(I), [ '(' ], parameterList(PL), [ ')' ], [ '{' ], commandList(CL), [ ; ], [return], expression(E), [ ; ], [ '}' ].
returnType --> dataType ; [void].
parameterList(t_parList(PS, P)) --> parameters(PS), parameter(P) ; [ ].
parameters(t_parameters(P, PS)) --> parameter(P), parameters(PS) ; [ ].
parameter(t_parameter(I)) --> dataType, identifier(I).

%--------------------------------------------- Command Section  -------------------------------------
commandList(t_commandList(C, CL)) --> command(C),  [ ; ],  commandList(CL).
commandList(t_commandList(C))  --> command(C).
command(t_command_assignexpr(I,E)) -->  identifier(I), [=], expression(E).
command(t_command_assignlist(I,L)) --> identifier(I),  [=],  list(L).
command(t_command_assigndict(I,D)) --> identifier(I), [=], dictionary(D).
command(t_command_if(B,CL1, CL2)) --> [if], boolean(B),  [ '{' ], commandList(CL1), [ '}' ], [else], ['{'], commandList(CL2), [ '}' ].
command(t_command_boolean(B,CL)) --> [while], boolean(B), [ '{' ], commandList(CL), [ '}' ].
command(t_command_for1(I,E,B,IN,K)) --> [for], [ '(' ], identifier(I), [=], expression(E), [ ; ], boolean(B), [ ; ], increment(IN), [ ')' ], [ '{' ], block(K), [ '}' ].
command(t_command_for2(I,D1,D2,K)) --> [for], identifier(I), [in], [range],  [ '(' ], integer(D1),  [ ',' ], integer(D2),  [ ')' ], [ '{' ], block(K), [ '}' ].
command(t_command_for3(I,E,B,DEC,K)) --> [for], [ '(' ], identifier(I), [=], expression(E), [ ; ], boolean(B), [ ; ], decrement(DEC), [ ')' ], [ '{' ], block(K), [ '}' ].
command(t_command_block(K)) --> block(K).
command(t_command_fun(FC)) --> funCall(FC).
command(t_command_funreturn(I,FC)) --> identifier(I), [=], funCall(FC).
command(t_command_print(PS)) --> printStatement(PS).
command(t_command_ternary(B,E1,E2)) --> boolean(B), [ ?],  expression(E1), [ : ], expression(E2).
%--------------------------------------------- Expression Section  -------------------------------------
expression(t_expression_add(E, T)) --> expression(E), [+], term(T).
expression(t_expression_sub(E, T)) --> expression(E), [-], term(T).
expression(t_expression_term(T)) --> term(T).
term(t_term_mul(T,V)) -->  term(T), [*], value(V).
term(t_term_div(T,V)) --> term(T), [/], value(V).
term(t_term_mod(T,V)) --> term(T), ['%'], value(V).
term(t_term_value(V)) --> value(V).
value(t_value_expr(E)) -->  ['('], expression(E), [')'].
value(t_value_id(I)) --> identifier(I).
value(t_value_int(D)) --> integer(D).
value(t_value_float(F)) --> float(F).
value(t_value_numberOps(NOP)) --> numberOps(NOP).
value(t_value_stringOps(SOP)) --> stringOps(SOP).
value(t_value_list(LID)) --> listIdentifier(LID).
value(t_value_dict(DID)) --> dictionaryIdentifier(DID).
value(t_value_string(S)) --> string(S).

%--------------------------------------------- List Section  ---------------------------------------------------
list(t_list(IL)) --> [ '[' ], identifierList(IL), [ ']' ].
identifierList(t_idList(LV,ELE)) --> listValues(LV), element(ELE).
identifierList(t_idList()) --> [].
%Added comma between elements
listValues(t_listVal(ELE, LV)) --> element(ELE), [,], listValues(LV).
listValues(t_listVal()) --> [].
element(t_element(D))  --> integer(D).
element(t_element(F))  --> float(F).
element(t_element(S))  --> string(S).
listIdentifier(t_listID(I, D)) --> varIdentifier(I), [ '[' ], integer(D), [ ']' ].

%--------------------------------------------- Dictionary Section  --------------------------------------------
dictionary(t_dictionary(DI)) --> ['{'], dictionaryItems(DI), ['}'].
dictionaryItems(t_dictItems(DV, DE)) --> dictionaryValues(DV), dictionaryElement(DE) ; [ ].
dictionaryValues(t_dictValues(DE, DV)) --> dictionaryElement(DE), dictionaryValues(DV) ;[ ].
dictionaryElement(t_dictElement(S, DV)) --> string(S), [:], dictionaryValue(DV).
dictionaryValue(t_dictVal(I)) --> integer(I).
dictionaryValue(t_dictVal(F)) --> float(F).
dictionaryValue(t_dictVal(S)) --> string(S).
dictionaryIdentifier(t_dictID(I, S)) --> identifier(I), ['['], string(S), [']'].

%--------------------------------------------- Operations Section  -------------------------------------
numberOps(t_numberOps(TC)) --> typeCast(TC).
numberOps(t_numberOps(INC)) --> increment(INC).
numberOps(t_numberOps(DEC)) --> decrement(DEC).
increment(t_inc(I)) --> identifier(I), [+], [+].
decrement(t_dec(I)) --> identifier(I), [-], [-].
typeCast(t_typeCast(I)) --> ['('], castDataType, [')'], identifier(I).
castDataType --> [int] ; [float] ; [str].

stringOps(t_stringOps(CST)) -->  concatString(CST).
stringOps(t_stringOps(RST)) -->  revString(RST).
stringOps(t_stringOps(SST)) -->  splitString(SST).
stringOps(t_stringOps(SLEN)) -->  stringLength(SLEN).

concatString(t_concatStr(S1, S2)) --> [concat], ['('], string(S1) , string(S2), [')'].
concatString(t_concatStr(I1, I2)) --> [concat], ['('], identifier(I1), identifier(I2), [')'].
revString(t_revStr(S)) --> [rev], ['('], string(S), [')'].
splitString(t_splitStr(S, D)) --> [split], ['('], string(S), integer(D).
stringLength(t_strLen(S)) --> [len], ['('], string(S), [')'].

%-------------------------------------- Conditional and Loop Section  -------------------------------------
boolean(t_boolean()) --> bool.
boolean(t_boolean_and(E1, E2)) --> expression(E1), [and], expression(E2).
boolean(t_boolean_or(E1, E2)) --> expression(E1), [or], expression(E2).
boolean(t_boolean_eq(E1, E2)) --> expression(E1), [==], expression(E2).
boolean(t_boolean_lt(E1, E2)) --> expression(E1), [<], expression(E2).
boolean(t_boolean_gt(E1, E2)) --> expression(E1), [>], expression(E2).
boolean(t_boolean_lteq(E1, E2)) --> expression(E1), [<=], expression(E2).
boolean(t_boolean_gteq(E1, E2)) --> expression(E1), [>=], expression(E2).
boolean(t_boolean_neq(E1, E2)) --> expression(E1), ['!='], expression(E2).
boolean(t_boolean_not(B)) --> [not], boolean(B).

%--------------------------------------------- Function Call  --------------------------------------------
funCall(t_funCall(I, CPL)) --> identifier(I), ['('], callParameterList(CPL), [')'].
callParameterList(t_callParList(CPS, CP)) --> callParameters(CPS), callParameter(CP) ; [ ].
%Added comma between parameters.
callParameters(t_callPars(CP, CPS)) --> callParameter(CP), [,], callParameters(CPS) ; [ ].
callParameter(t_callPar(I)) --> identifier(I).

%--------------------------------------------- Print Statement  --------------------------------------------
printStatement(t_print(IT)) --> [print], ['('], item(IT), [')'].
item(t_item(E)) --> expression(E).
item(t_item(S)) --> string(S).


% ---------------------------------------------Semantics ----------------

%----------------------------------------------Lists Section------------
eval_list(t_list(X),ID,Env,Env1):- eval_identifierList(X,ID,Env,Env1).
eval_identifierList(t_idList(LV,ELE),ID,Env,Env2):- eval_listValues(LV,ID,Env,Env1), eval_element(ELE,ID,Env1,Env2).
eval_listValues(t_listVal(ELE,LV),ID,Env,Env2):- eval_element(ELE,ID,Env,Env1), eval_listValues(LV,ID,Env1,Env2).
eval_listValues(t_listVal(),_,Env,Env).
eval_element(t_element(X),ID,Env,Env1):- initializeList(ID,X,Env,Env1).

eval_listIdentifier_LHS(t_listID(ID,Pos),Val,Env,Env1):- updateList(ID,Pos,Val,Env,Env1).
eval_listIdentifier_RHS(t_listID(ID,Pos),Env,Val):- lookupList(ID,Pos,Env,Val).


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