/*

*/

:- module(lexer, [
   string_to_tokens/2
]).

:- use_module(library(pcre)).
:- use_module(library(readutil)).

getTokens(Input, Tokens) :- extractTokens(Input, ExTokens),
							comment(ExTokens,'/*','*/',RFirst),
							comment(RFirst,'//','\n',Rest),
                            delete(Rest, [], Tokens) % delete empty tokens
.
% tokenizar y quitar espacion en blanco
extractTokens([], []) :- !.
extractTokens(Input, [Token | Tokens]) :-       skipWhiteSpace(Input, InputNWS),
                                                startOneToken(InputNWS, Token, Rest),
                                                extractTokens(Rest, Tokens)
												
.
% quitar espacios en blanco
skipWhiteSpace([C | Input], Output) :- isWhiteSpace(C), !, 
                                       skipWhiteSpace(Input, Output)
.
skipWhiteSpace(Input, Input).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%eliminar comentarios multi linea
comment([],_X,_Y,_L) :-!.
comment([X|R],X,Y,L) :- drop(R,X,Y,L),!.
comment([F|R],X,Y,[F|L]) :- ( F\=X ), comment(R,X,Y,L),!.

drop([],_X,_Y,[]) :- !.
drop([Y|R],X,Y,L) :- comment(R,X,Y,L).
drop([F|R],X,Y,L) :- ( F\=Y ), drop(R,X,Y,L),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

startOneToken(Input, Token, Rest) :- startOneToken(Input, [], Token, Rest).
startOneToken([], P, P, []).


startOneToken([C, A | Input], Partial, Token, Rest) :- doubleSpecial(C, A), !,
                                                    finishSpecial([A | Input], [ C | Partial], Token, Rest)
.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%parametros extra
%Signo, Decimal,iniciado

startOneToken([C | Input], Partial, Token, Rest) :- isCero(C), !,
                                                    finishNumber(Input,Partial, Token, Rest,'0','0','0')
.
startOneToken([C | Input], Partial, Token, Rest) :- isDigit(C), !,
                                                    finishNumber(Input, [ C | Partial], Token, Rest,'0','0','1')
.
startOneToken([C,F | Input], Partial, Token, Rest) :- isDot(C),isDigit(F), !,
                                                    finishNumber(Input, [ F ,C,'0'| Partial], Token, Rest,'0','1','1')
.

startOneToken([C | Input], Partial, Token, Rest) :- isString(C), !,
                                                    finishString(Input, [ C | Partial], Token, Rest)
.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

startOneToken([C | Input], Partial, Token, Rest) :- isLetter(C), !,
                                                    finishId(Input, [ C | Partial], Token, Rest)
.

startOneToken([C | Input], Partial, Token, Rest) :- isSpecial(C), !,
                                                    finishSpecial(Input, [ C | Partial], Token, Rest)
.


startOneToken([C | _] , _, _, _)                  :- report_invalid_symbol(C)
.

report_invalid_symbol(C) :-
    Msg='*** >>> "~s": invalid symbol found in input Stream ***',
    format(atom(A), Msg, C),
    throw(A)
. 

% finalizar lectura de un ID
finishId(Input, Partial, Token, Rest) :- finishToken(Input, isLetterOrDigit, Partial, Token, Rest)
.

% finalizar lectura de un caracte especial, para el caso de si es doble y simple
finishSpecial([C | Input], [PC | Partial], Token, Input) :- doubleSpecial(PC, C), !, 
                                                         convertToAtom([C, PC | Partial], Token) 
.

finishSpecial(Input, Input, Token, Input) :- isSpecial(Input), convertToAtom(Input, Token) .

finishSpecial(Input, Partial, Token, Input) :- convertToAtom(Partial, Token) 
.
% finalizar lectura de un token
finishToken([C | Input], Continue, Partial, Token, Rest) :- call(Continue, C), !, 
                                                            finishToken(Input, Continue, [ C | Partial], Token, Rest)
.

finishToken(Input, _, Partial, Token, Input) :- convertToAtom(Partial, Token).

finishString([C | Input], Partial, Token, Rest) :- 	isNotString(C),!,
													finishString(Input, [ C | Partial ], Token, Rest)
.
finishString([C | Input], Partial, Token, Input) :- 	isString(C),!,
													convertToAtom([C|Partial], Token)
.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% finalizar lectura de un numero
% parametros extra
%Signo Decimal iniciado

finishNumber([C|Input], Partial, Token, Rest,Sign,Dec,Strt) 	:- isCero(C),!,
															(	isStarter(Strt) ->	
																finishNumber(Input, [ C | Partial ], Token, Rest,Sign,Dec,'1');
																finishNumber(Input,Partial, Token, Rest,Sign,Dec,'0'))
.

finishNumber([C|Input], Partial, Token, Rest,Sign,Dec,_) :- isDigit(C),!,
												finishNumber(Input, [ C | Partial ], Token, Rest,Sign,Dec,'1')
.

finishNumber([C|Input], Partial, Token, Rest,Sign,Dec,Strt) :- isDot(C),!,
												isDecimal(Dec),!,
												(	isStarter(Strt) ->
													finishNumber(Input, [ C | Partial ], Token, Rest,Sign,'1','1');
													finishNumber(Input, [ C ,'0'| Partial ], Token, Rest,Sign,'1','1')	)
.
finishNumber([A,C|Input], Partial, Token, Rest,Sign,Dec,Strt) :- isE(A),!,
												isSign(C),!,
												finishNumber(Input, [ C ,A | Partial ], Token, Rest,Sign,Dec,Strt)
.

finishNumber(Input, Partial, Token, Input,_,_,_) :-	convertToAtom(Partial, Token)
.

finishNumber(Input, [], Token, Input,_,_,_) :- convertToAtom(['0'], Token).

finishNumber(Input, Partial, Token, Input,_,_,_) :-	convertToAtom(Partial, Token)
.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

isWhiteSpace(C) :- member(C, ['\r', '\t', ' ']).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
isVoid('').
isVoidList(['']).
isDot(D)	:- 	D='.'.
isE(D)		:- 	member(D,['e','E']).
isSign(D)	:- 	member(D,['+','-']).
isNSign(D)	:- 	D ='-'.
isCero(D)	:-	D='0'.
isDigit(D)  :- 	D @>= '0', D @=< '9'.

isDecimal(D)	:-	D='0'.
isNegative(D)	:-	D='1'.
isPositive(D)	:-	D='0'.
isStarter(D)	:-	D='1'.
isInStart(D)	:-	D='0'.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

isLetter('_') :- !. 
isLetter('$') :- !. 
isLetter(D)  :- D @>='a', D @=< 'z', !.  % a .. z
isLetter(D)  :- D @>= 'A', D @=< 'Z'.    % A .. Z

isLetterOrDigit(C) :- isLetter(C),!.
isLetterOrDigit(D) :- isDigit(D),!.
isString('"').
isNotString(D) :- D\='"'.


% special tokens
isSpecial(O)    :- member(O, ['\n','=', '<', '>', '*', '-', '+', '/', '\\', '.', '(', ')']), !.
isSpecial(O)    :- member(O, ['{', '}' ,'[', ']', '&', '|', '%', '!', '?', ';', ',']), !.
isSpecial(O)    :- member(O, ['@', ':']), !.

% double operators
doubleSpecial('!', '='). % !=
doubleSpecial('=', '='). % ==
doubleSpecial('<', '='). % <=
doubleSpecial('>', '='). % >=
doubleSpecial('&', '&'). % &&
doubleSpecial('|', '|'). % &&
doubleSpecial('-', '>'). % ->
doubleSpecial('+', '+'). % ++
doubleSpecial('-', '-'). % --

% comments tokens
doubleSpecial('/', '/'). % //
doubleSpecial('/', '*'). % open comments
doubleSpecial('*', '/'). % close comments

convertToAtom(Partial, Token) :- reverse(Partial, TokenCodes), 
                                 atom_codes(Token, TokenCodes)
.

codes_to_chars(Lcodes, Lchars):-
    atom_codes(Atom_from_codes, Lcodes), 
    atom_chars(Atom_from_codes, Lchars).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tokenize(File, Tokens) :- open(File, read, Stream),
                          read_stream_to_codes(Stream, Codes),
                          close(Stream),
                          codes_to_chars(Codes, Chars),
                          getTokens(Chars, Tokens)
.

string_to_tokens(String, Tokens) :- 	open_string(String, Stream),
								read_stream_to_codes(Stream, Codes),
								close(Stream),
								codes_to_chars(Codes, Chars),
								getTokens(Chars, Tokens)
.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

classify_token(Token, num(Token)) :- re_match("^\\d+(\\.\\d+)?$", Token),!.
classify_token(Token, id(Token))  :- re_match("^[a-zA-Z]+[\\w$]*$", Token),!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% esi
%string_to_tokens('',T).