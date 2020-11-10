:- module(transpiler, [

]).

:- set_prolog_flag(double_quotes, chars).

% Utils
% =====
  % Transpile
  transpile(Tree, String) :-
    phrase(assignment(Tree), Ls), atomic_list_concat(Ls, String).

% Assignment definition
% =====================
  assignment(assignment(Name, _)) --> variable_name(Name), " = ", "value", ";".
% Identifier definition
% =====================
  % Variable name
  variable_name(var(Name)) --> identifier(Name).
  % Identifier
  identifier(Name) --> [Name].
  
