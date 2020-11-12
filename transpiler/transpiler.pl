:- module(transpiler, [

]).

:- set_prolog_flag(double_quotes, chars).

% Utils
% =====
  % Transpile
  transpile(Tree, String) :-
    phrase(term(Tree), Ls), atomic_list_concat(Ls, String).

% Assignment definition
% =====================
  assignment(assignment(Name, _)) --> variable_name(Name), " = ", "value".
% Term
% ====
  term(number(Term)) --> [Term].
  term(string(Term)) --> [Term].
  term(list(Term)) --> "List.of(", list_body(Term), ")".
  term(const(Term)) --> [Term].
  term(Term) --> variable_name(Term).
  % List body
  list_body([]) --> [].
  list_body([Term]) --> term(Term), {!}.
  list_body([Term | Rest]) --> term(Term), ", ", list_body(Rest).
  % TODO: change term(Term) for general_body(Term).
% Identifier definition
% =====================
  % Variable name
  variable_name(var(Name)) --> identifier(Name).
  % Identifier
  identifier(Name) --> [Name].
  
