%----------------------------------- Main Section ----------------------------------------
program --> [begin], block, [end].
block --> ['{'], declarationList, [ ; ],  commandList,  [ '}' ].

%---------------------------------- Declaration Section ---------------------------------------
declarationList --> declaration, [ ; ], declarationList.
declarationList --> declaration.
declaration -->  dataType, identifier.
declaration --> functionDeclaration.
dataType --> [int] ; [float] ; [str] ; [bool] ; [list] ; [dict].
identifier -->  [X], {atom(X)}.
integer --> [X], {integer(X)}.
float --> [X], {float(X)}.
bool --> [true] ; [false].

%------------------------------------------------ Strings -------------------------------------------
string--> [“], identifier, string, [“].
string--> [ ].

%--------------------------------------------- Functions --------------------------------------------
functionDeclaration --> [def],  returnType, identifier, [ '(' ], parameterList, [ ')' ], [ '{' ], commandList, [ ; ], [return], expression, [ ; ], [ '}' ].
returnType --> dataType ; [void].
parameterList --> parameters, parameter ; [ ].
parameters --> parameter, parameters ; [ ].
parameter --> dataType, identifier.

%--------------------------------------------- Command Section  -------------------------------------
commandList --> command,  [ ; ],  commandList.
commandList  --> command.
command -->  identifier, [=], expression.
command --> identifier,  [=],  list.
command --> identifier, [=], dictionary.
command --> [if], boolean,  [ '{' ], commandList, [ '}' ], [else], ['{'], commandList, [ '}' ].
command --> [while], boolean, [ '{' ], commandList, [ '}' ].
command --> [for], [ '(' ], identifier, [=], expression, [ ; ], boolean, [ ; ], increment, [ ')' ], [ '{' ], block, [ '}' ].
command --> [for], identifier, [in], [range],  [ '(' ], integer,  [ ',' ], integer,  [ ')' ], [ '{' ], block, [ '}' ].
command --> [for], [ '(' ], identifier, [=], expression, [ ; ], [boolean], [ ; ], decrement, [ ')' ], [ '{' ], block, [ '}' ].
command --> block.
command --> funCall.
command --> identifier, [=], funCall.
command --> printStatement.
command --> boolean, [ ?],  expression, [ : ], expression.

%--------------------------------------------- Expression Section  -------------------------------------
expression --> expression, [+], term.
expression --> expression, [-], term.
expression --> term.
term -->  term, [*], value.
term --> term, [/], value.
term --> term, ['%'], value.
term --> value.
value -->  ['('], expression, [')'].
value --> identifier.
value --> integer.
value --> float.
value --> numberOps.
value --> stringOps.
value --> listIdentifier.
value --> dictionaryIdentifier.
value --> string.

%--------------------------------------------- List Section  ---------------------------------------------------
list --> [ '[' ], identifierList, [ ']' ].
identifierList --> listValues, element ; [ ].
listValues --> element, listValues ; [ ].
element --> integer ; float ; string.
listIdentifier --> identifier, [ '[' ], integer, [ ']' ].

%--------------------------------------------- Dictionary Section  --------------------------------------------
dictionary --> ['{'], dictionaryItems, ['}'].
dictionaryItems --> dictionaryValues, dictionaryElement ; [ ].
dictionaryValues --> dictionaryElement, dictionaryValues ;[ ].
dictionaryElement --> string, [:], dictionaryValue.
dictionaryValue --> integer ; float ; string.
dictionaryIdentifier --> identifier, ['['], string, [']'].

%--------------------------------------------- Operations Section  -------------------------------------
numberOps --> typeCast ; increment ;decrement.
increment --> identifier, [+], [+].
decrement --> identifier, [-], [-].
typeCast --> ['('], castDataType, [')'], identifier.
castDataType --> [int] ; [float] ; [str].
stringOps -->  concatString ; revString ; splitString ; stringLength.
concatString --> [concat], ['('], string , string, [')'].
concatString --> [concat], ['('], identifier, identifier, [')'].
revString --> [rev], ['('], string, [')'].
splitString --> [split], ['('], string, integer.
stringLength --> [len], ['('], string, [')'].

%-------------------------------------- Conditional and Loop Section  -------------------------------------
boolean --> bool.
boolean --> expression, [and], expression.
boolean --> expression, [or], expression.
boolean --> expression, [==], expression.
boolean --> [not], boolean.
boolean --> expression, [<], expression.
boolean --> expression, [>], expression.
boolean --> expression, [<=], expression.
boolean --> expression, [>=], expression.
boolean --> expression, ['!='], expression.

%--------------------------------------------- Function Call  --------------------------------------------
funCall --> identifier, ['('], callParameterList, [')'].
callParameterList --> callParameters, callParameter ; [ ].
callParameters --> callParameter, callParameters ; [ ].
callParameter --> identifier.

%--------------------------------------------- Print Statement  --------------------------------------------
printStatement --> [print], ['('], item, [')'].
item --> expression; string.
