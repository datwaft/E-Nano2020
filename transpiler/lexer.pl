/*

*/

:- module(lexer, [
   string_to_tokens/2
]).

:- use_module(library(pcre)).
:- use_module(library(readutil)).

getTokens(Input, Tokens) :- extractTokens(Input, ExTokens), 
                            delete(ExTokens, [], Tokens) % delete empty tokens
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

startOneToken(Input, Token, Rest) :- startOneToken(Input, [], Token, Rest).
startOneToken([], P, P, []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%parametros extra
%signo, decimal,iniciado

startOneToken([C | Input], Partial, Token, Rest) :- isCero(C), !,
                                                    finishNumber(Input,Partial, Token, Rest,'0','0','0')
.
startOneToken([C | Input], Partial, Token, Rest) :- isDigit(C), !,
                                                    finishNumber(Input, [ C | Partial], Token, Rest,'0','0','1')
.
startOneToken([C | Input], Partial, Token, Rest) :- isDot(C), !,
                                                    finishNumber(Input, [ C ,'0'| Partial], Token, Rest,'0','1','1')
.
startOneToken([C | Input], Partial, Token, Rest) :- isSign(C), !,
													(	isNSign(C) ->
														finishNumber(Input, Partial, Token, Rest,'1','0','0');
														finishNumber(Input, Partial, Token, Rest,'0','0','0'))
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
    Msg='*** >>> "~s": invalid symbol found in input stream ***',
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

finishSpecial(Input, Partial, Token, Input) :- convertToAtom(Partial, Token) 
.
% finalizar lectura de un token
finishToken([C | Input], Continue, Partial, Token, Rest) :- call(Continue, C), !, 
                                                            finishToken(Input, Continue, [ C | Partial], Token, Rest)
.

finishToken(Input, _, Partial, Token, Input) :- convertToAtom(Partial, Token).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% finalizar lectura de un numero
% parametros extra
% signo decimal iniciado
finishNumber([], [], Token, [],_,_,_) :- convertToAtom(['0'], Token).

finishNumber(Input, Partial, Token, Input,sign,_,_) :- (	isNegative(sign) ->
															convertToAtom(['-'|Partial], Token)	;
															convertToAtom(Partial, Token)	)
.

finishNumber([C|Input], Partial, Token, Rest,sign,dec,str) 	:- isCero(C),!,
															(	isStarter(str) ->	
																finishNumber(Input, [ C | Partial ], Token, Rest,sign,dec,'1');
																finishNumber(Input,Partial, Token, Rest,sign,dec,'0'))
.

finishNumber([C|Input], Partial, Token, Rest,sign,dec,str) :- isDigit(C),!,
												finishNumber(Input, [ C | Partial ], Token, Rest,sign,dec,'1')
.

finishNumber([C|Input], Partial, Token, Rest,sign,dec,str) :- isDot(C),!,
												isDecimal(dec),!,
												(	isStarter(str) ->
													finishNumber(Input, [ C | Partial ], Token, Rest,sign,'1','1');
													finishNumber(Input, [ C ,'0'| Partial ], Token, Rest,sign,'1','1')	)
.
finishNumber([C|Input], Partial, Token, Rest,sign,dec,str) :- isSign(C), !,
													(	isNSign(C) ->
														(	isNegative(sign) ->
															finishNumber(Input, Partial, Token, Rest,'0',dec,str);
															finishNumber(Input, Partial, Token, Rest,'1',dec,str)	);
														finishNumber(Input, Partial, Token, Rest,sign,dec,str))
.
finishNumber([A,C|Input], Partial, Token, Rest,sign,dec,str) :- isE(A),!,
												isSign(C),!,
												finishNumber(Input, [ C ,A | Partial ], Token, Rest,sign,dec,str)
.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

isWhiteSpace(C) :- member(C, ['\r', '\n', '\t', ' ']).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
isDot(D)	:- 	D='.'.
isE(D)		:- 	member(D,['e','E']).
isSign(D)	:- 	member(D,['+','-']).
isNSign(D)	:- 	D ='-'.
isCero(D)	:-	D='0'.
isDigit(D)  :- 	D @>= '1', D @=< '9'.

isDecimal(D)	:-	D='0'.
isNegative(D)	:-	D='1'.
isStarter(D)	:-	D='1'.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

isLetter('_') :- !. 
isLetter('$') :- !. 
isLetter(D)  :- D @>='a', D @=< 'z', !.  % a .. z
isLetter(D)  :- D @>= 'A', D @=< 'Z'.    % A .. Z

isLetterOrDigit(C) :- isLetter(C),!.
isLetterOrDigit(D) :- isDigit(D),!.

% special tokens
isSpecial(O)    :- member(O, ['=', '<', '>', '*', '-', '+', '/', '\\', '.', '(', ')']), !.
isSpecial(O)    :- member(O, ['{', '}', '"','[', ']', '&', '|', '%', '!', '?', ';', ',']), !.
isSpecial(O)    :- member(O, ['@', ':']), !.

% double operators
doubleSpecial('!', '='). % !=
doubleSpecial('=', '='). % ==
doubleSpecial('<', '='). % <=
doubleSpecial('>', '='). % >=
doubleSpecial('&', '&'). % &&
doubleSpecial('|', '|'). % &&
doubleSpecial('-', '>'). % ->

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