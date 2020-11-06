/*

*/

:- module(lexer, [
   tokenizer/2
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
startOneToken([C | Input], Partial, Token, Rest) :- isDigit(C), !,
                                                    finishNumber(Input, [ C | Partial], Token, Rest)
.

startOneToken([C | Input], Partial, Token, Rest) :- isLetter(C), !,
                                                    finishId(Input, [ C | Partial], Token, Rest)
.

startOneToken([C | Input], Partial, Token, Rest) :- isSpecial(C), !,
                                                    finishSpecial(Input, [ C | Partial], Token, Rest)
.
startOneToken([C | Input], Partial, Token, Rest) :- isQuote(C), !,
                                                    finishQuote(Input, [ C | Partial], Token, Rest)
.
startOneToken([C | _] , _, _, _)                  :- report_invalid_symbol(C)
.

report_invalid_symbol(C) :-
    Msg='*** >>> "~s": invalid symbol found in input stream ***',
    format(atom(A), Msg, C),
    throw(A)
. 

% finalizar lectura de un numero
finishNumber(Input, Partial, Token, Rest) :- finishToken(Input, isDigit, Partial, Token, Rest)
.
% finalizar lectura de un ID
finishId(Input, Partial, Token, Rest) :- finishToken(Input, isLetterOrDigit, Partial, Token, Rest)
.

finishQuote([C | Input], Partial, Token, Input) :- isQuote(C), !,
                                                   convertToAtom([C | Partial], Token) 
.

finishQuote([C | Input], Partial, Token, Rest) :- finishQuote(Input, [C |Partial], Token, Rest).
finishQuote([] , _Partial, _Token, _Input) :- throw('opened and not closed string').

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



isWhiteSpace(C) :- member(C, ['\r', '\n', '\t', ' ']).

isDigit(D)   :- D @>= '0', D @=< '9'.

isLetter('_') :- !. 
isLetter('$') :- !. 
isLetter(D)  :- D @>='a', D @=< 'z', !.  % a .. z
isLetter(D)  :- D @>= 'A', D @=< 'Z'.    % A .. Z

isLetterOrDigit(C) :- isLetter(C),!.
isLetterOrDigit(D) :- isDigit(D),!.

isQuote('"'). 
% special tokens
isSpecial(O)    :- member(O, ['=', '<', '>', '*', '-', '+', '/', '\\', '.', '(', ')']), !.
isSpecial(O)    :- member(O, ['{', '}', '[', ']', '&', '|', '%', '!', '?', ';', ',']), !.
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

tokenizer(String, Tokens) :- 	open_string(String, Stream),
								read_stream_to_codes(Stream, Codes),
								close(Stream),
								codes_to_chars(Codes, Chars),
								getTokens(Chars, Tokens)
.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

classify_token(Token, num(Token)) :- re_match("^\\d+(\\.\\d+)?$", Token),!.
classify_token(Token, id(Token))  :- re_match("^[a-zA-Z]+[\\w$]*$", Token),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% esi
