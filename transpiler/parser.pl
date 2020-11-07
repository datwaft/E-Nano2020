:- module(parser, [
  
]).

:- use_module(library(pcre)).

% Test cases
% ==========
  test_case(1, ['val', '<', 'int', '>', 'x', '=', '666']).
  test_case(2, ['val', '<', 'int', '->', 'int', '>', 'x', '=', 'x', '->', 'x', '+', '2']).
  test_case(3, ['function', '(', 'function', '(', ')', '+', 'y', ')']).
% Assignment definition
% =====================
  assignment(assignment(Name, Value)) --> variable_name(Name), ['='], advanced_body(Value).
% Declaration definition
% ======================
  declaration(declaration(Type, Name, Value)) --> declaration(declaration(Type, Name)), ['='], advanced_body(Value).
  declaration(declaration(Type, Name)) --> ['val'], ['<'], type(Type), ['>'], variable_name(Name).
  declaration(declaration(Type, Name, Lambda)) --> ['val'], ['<'], method_type(Type), ['>'], variable_name(Name), ['='], lambda(Lambda).
% Method definition
% =================
  % TODO
% Lambda definition
% =================
  lambda(lambda(Variable, Body)) --> variable_name(Variable), ['->'], lambda_body(Body).
  % Body
  lambda_body(body(Body)) --> advanced_body(Body).
% Type definition
% ===============
  type(type(Type)) --> [Type], { identifier(Type) }.
  type(list_type(Type)) --> ['['], type(Type), [']'].
  % Method type definition
  method_type(type(From, To)) --> type(From), ['->'], type(To).
% Term definition
% ===============
  term(number(Term)) --> [Term], { number(Term) }.
  term(number(Number)) --> [Term], { atom(Term) }, { atom_number(Term, Number) }.
  term(string(Term)) --> [Term], { sub_atom(Term, 0, 1, _, '"') }, { sub_atom(Term, _, 1, 0, '"') }.
  term(list(Term)) --> ['['], list_body(Term), [']'].
  term(const(Term)) --> [Term], { member(Term, ['true', 'false', 'null']) }.
  term(Term) --> variable_name(Term).
  % List body
  list_body([]), [']'] --> [']'].
  list_body([Term | Rest]) --> general_body(Term), [','], list_body(Rest).
  list_body([Term]) --> general_body(Term).
% Function definition
% ===================
  % Funciton call definition
  function_call(function_call(Name, Arguments)) --> variable_name(Name), ['('], function_call_arguments(Arguments) , [')'].
  function_call_arguments([]), [')'] --> [')'].
  function_call_arguments([Arg | Rest]) --> advanced_body(Arg), [','], function_call_arguments(Rest).
  function_call_arguments([Arg]) --> advanced_body(Arg).
% Operation definition
% ====================
  % Operator definition
    unary_ls_operator(operator(Operator)) --> [Operator], { member(Operator, ['+', '-', '~', '++', '--']) }.
    unary_rs_operator(operator(Operator)) --> [Operator], { member(Operator, ['++', '--']) }.
    binary_operator(operator(Operator)) --> [Operator], { member(Operator, ['+', '-', '*', '/', '%', '==', '!=', '<=', '>=', '<', '>', '&&', '||', '^']) }.
  % Unary operation definiton
    operation(operation(Operator, Term)) --> unary_ls_operator(Operator), specific_body(Term).
    operation(operation(Term, Operator)) --> specific_body(Term), unary_rs_operator(Operator).
  % Binary operation definition
    operation(operation(First, Operator, Second)) --> specific_body(First), binary_operator(Operator), general_body(Second).
  % Ternary operation definition
    ternary_operation(operation(True, Condition, False)) --> general_body(True), ['if'], general_body(Condition), ['else'], advanced_body(False).
% Identifier definition
% =====================
  % Variable
  variable_name(var(Name)) --> [Name], { identifier(Name) }.
  % Identifier
  identifier(Name) :- atom(Name), re_match('^[a-zA-Z_][\\w]*$'/i, Name).
  % Specific body
  specific_body(Body) --> term(Body).
  specific_body(Body) --> function_call(Body).
  % General body
  general_body(Body) --> specific_body(Body).
  general_body(Body) --> operation(Body).
  % Advanced body
  advanced_body(Body) --> general_body(Body).
  advanced_body(Body) --> ternary_operation(Body).
