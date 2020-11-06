:- module(parser, [
  
]).

:- use_module(library(pcre)).

% Test cases
% ==========
  test_case(['val', '<', 'int', '>', 'x', '=', '666']).
% Assignment definition
% =====================
  assignment(assignment(Name, Value)) --> variable_name(Name), ['='], term(Value).
% Declaration definition
% ======================
  declaration(declaration(Type, Name, Value)) --> declaration(declaration(Type, Name)), ['='], term(Value).
  declaration(declaration(Type, Name)) --> ['val'], ['<'], type(Type), ['>'], variable_name(Name).
  declaration(declaration(Type, Name, Lambda)) --> ['val'], ['<'], method_type(Type), ['>'], variable_name(Name), ['='], lambda(Lambda).
% Type definition
% ===============
  type(type(Type)) --> [Type], { identifier(Type) }.
  type(list_type(Type)) --> ['['], type(Type), [']'].
  % Method type definition
  method_type(type(From, To)) --> type(From), ['->'], type(To).
% Term definition
% ===============
  term(number(Term)) --> [Term], { number(Term) }.
  term(number(Term)) --> [Term], { atom_number(Term, _) }.
  term(string(Term)) --> [Term], { sub_atom(Term, 0, 1, _, '"') }, { sub_atom(Term, _, 1, 0, '"') }.
  term(list(Term)) --> ['['], list_body(Term), [']'].
  term(const(Term)) --> [Term], { member(Term, ['true', 'false', 'null']) }.
  term(Term) --> variable_name(Term).
  % List body
  list_body([]), [']'] --> [']'].
  list_body([Term | Rest]) --> operation(Term), [','], list_body(Rest).
  list_body([Term | Rest]) --> term(Term), [','], list_body(Rest).
  list_body([Term]) --> operation(Term).
  list_body([Term]) --> term(Term).
% Function definition
% ===================
  % TODO
% Lambda definition
% =================
  lambda(lambda(Variable, Body)) --> variable_name(Variable), ['->'], lambda_body(Body).
  % Body
  lambda_body(body(Body)) --> general_body(Body).
% Method definition
% =================
  % TODO
% Operation definition
% ====================
  % Operator definition
    unary_operator(operator(Operator)) --> [Operator], { member(Operator, ['+', '-', '~']) }.
    binary_operator(operator(Operator)) --> [Operator], { member(Operator, ['+', '-', '*', '/', '%', '==', '!=', '<=', '>=', '<', '>', '&&', '||', '^']) }.
  % Unary operation definiton
    operation(operation(Operator, Term)) --> unary_operator(Operator), term(Term).
  % Binary operation definition
    operation(operation(First, Operator, Second)) --> term(First), binary_operator(Operator), term(Second).
  % Ternary operation definition
    ternary_operation(operation(True, Condition, False)) --> operation_statement(True), ['if'], operation_statement(Condition), ['else'], general_body(False).
  % Statement definition
    operation_statement(Statement) --> term(Statement).
    operation_statement(Statement) --> operation(Statement).
% Identifier definition
% =====================
  % Variable
  variable_name(var(Name)) --> [Name], { identifier(Name) }.
  % Identifier
  identifier(Name) :- re_match('^[a-zA-Z_][\\w]*$'/i, Name).
  % General body
  general_body(Body) --> term(Body).
  general_body(Body) --> operation(Body).
  general_body(Body) --> ternary_operation(Body).
