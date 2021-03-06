/*
 *  _____      _   _                  ____   ___ ____   ___
 * | ____|    | \ | | __ _ _ __   ___|___ \ / _ \___ \ / _ \
 * |  _| _____|  \| |/ _` | '_ \ / _ \ __) | | | |__) | | | |
 * | |__|_____| |\  | (_| | | | | (_) / __/| |_| / __/| |_| |
 * |_____|    |_| \_|\__,_|_| |_|\___/_____|\___/_____|\___/
 *
 * File: parser.pl
 * Description:
 *    Contiene las utilidades de prolog para parsear un archivo de expresión a partir de tokens.
 * Authors:
 * - David Alberto Guevara Sánchez
 *   402450355
 * - Joy Bonilla Fley
 *   402360421
 * - Jose Barrantes Araya
 *   207600954
 * - Natalia Solano Azofeifa
 *   117290958
 * - Luis David Villalobos Gonzalez
 *   117540697
 * Group: 03
 * Schedule: 10am
 * Date of modification: 2020-11-15
 */

:- module(parser, [
  program/3
]).

:- use_module(library(pcre)).

:- table lambda_type/3.
:- table operation/3.

% Program body definition
% =======================
  program(program(Global, Main)) --> global(Global), main(Main).
% Global scope definition
% =======================
  global(Assignments) --> declaration_list(Assignments).
  declaration_list([]) --> [].
  declaration_list([Declaration | Rest]) --> declaration(Declaration), declaration_list(Rest).
  declaration_list([Declaration | Rest]) --> method(Declaration), declaration_list(Rest).
% Main scope definition
% =====================
  main(Statements) --> ['main'], ['{'], statement_list(Statements), ['}'], {!}.
  main([]) --> [].
  statement_list([]) --> [].
  statement_list([Statement | Rest]) --> declaration(Statement), statement_list(Rest).
  statement_list([Statement | Rest]) --> assignment(Statement), statement_list(Rest).
  statement_list([Statement | Rest]) --> function_call(Statement), statement_list(Rest).
% Assignment definition
% =====================
  assignment(assignment(Name, Value)) --> variable_name(Name), ['='], advanced_body(Value).
% Declaration definition
% ======================
  declaration(declaration(Type, Name, Value)) --> declaration(declaration(Type, Name)), ['='], advanced_body(Value).
  declaration(declaration(Type, Name)) --> ['val'], ['<'], type(Type), ['>'], variable_name(Name).
  declaration(declaration(Type, Name, Lambda)) --> ['val'], ['<'], lambda_type(Type), ['>'], variable_name(Name), ['='], lambda(Lambda).
% Method definition
% =================
  method(method(Type, Function, Body)) --> ['method'], ['<'], lambda_type(Type), ['>'], function_declaration(Function), ['='], lambda_body(Body).
  method(method(Generics, Type, Function, Body)) --> ['method'], ['<'], open_parentheses, lambda_type_list(Generics), close_parentheses, open_parentheses, lambda_type(Type), close_parentheses, ['>'], function_declaration(Function), ['='], lambda_body(Body).
  open_parentheses --> ['('].
  close_parentheses --> [')'].
% Lambda definition
% =================
  lambda(lambda(Variable, Body)) --> variable_name(Variable), ['->'], lambda_body(Body).
  lambda(lambda(Variable, Body)) --> ['('], lambda_parameter_list(Variable), [')'], ['->'], lambda_body(Body).
  % Lambda parameter list
  lambda_parameter_list([]), [')'] --> [')'].
  lambda_parameter_list([Variable | Rest]) --> variable_name(Variable), [','], lambda_parameter_list(Rest).
  lambda_parameter_list([Variable]) --> variable_name(Variable).
  % Body
  lambda_body(body(Body)) --> advanced_body(Body).
% Type definition
% ===============
  type(type(Type)) --> [Type], { identifier(Type) }.
  type(list_type(Type)) --> ['['], type(Type), [']'].
  type(Type) --> lambda_type(Type).
  % Method type definition
  lambda_type(type([From], To)) --> type(From), ['->'], type(To).
  lambda_type(type(From, To)) --> ['('], lambda_type_list(From), [')'], ['->'], type(To).
  % Lambda type list
  lambda_type_list([]), [')'] --> [')'].
  lambda_type_list([Type | Rest]) --> type(Type), [','], lambda_type_list(Rest).
  lambda_type_list([Type]) --> type(Type).
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
  % Parentheses
  parentheses(parentheses(Expression)) --> ['('], advanced_body(Expression), [')'].
% Function definition
% ===================
  % Function declaration definition
  function_declaration(function_declaration(Name, Arguments)) --> variable_name(Name), ['('], function_declaration_arguments(Arguments) , [')'].
  function_declaration_arguments([]), [')'] --> [')'].
  function_declaration_arguments([Arg | Rest]) --> variable_name(Arg), [','], function_declaration_arguments(Rest).
  function_declaration_arguments([Arg]) --> variable_name(Arg).
  % Funciton call definition
  function_call(function_call(Name, Arguments)) --> function_call_namespaces(Name), ['('], function_call_arguments(Arguments), [')'].
  % Function call namespaces
  function_call_namespaces([Name | Rest]) --> variable_name(Name), ['.'], function_call_namespaces(Rest).
  function_call_namespaces([Name]) --> variable_name(Name).
  % Function call arguments
  function_call_arguments([]), [')'] --> [')'].
  function_call_arguments([Arg | Rest]) --> advanced_body(Arg), [','], function_call_arguments(Rest).
  function_call_arguments([Arg]) --> advanced_body(Arg).
% Operation definition
% ====================
  % Operator definition
    unary_ls_operator(lsoperator(Operator)) --> [Operator], { member(Operator, ['+', '-', '~', '++', '--']) }.
    unary_rs_operator(rsoperator(Operator)) --> [Operator], { member(Operator, ['++', '--']) }.
    binary_operator(bioperator(Operator)) --> [Operator], { member(Operator, ['+', '-', '*', '/', '%', '==', '!=', '<=', '>=', '<', '>', '&&', '||', '^']) }.
  % Unary operation definiton
    operation(operation(Operator, Term)) --> unary_ls_operator(Operator), specific_body(Term).
    operation(operation(Term, Operator)) --> specific_body(Term), unary_rs_operator(Operator).
  % Binary operation definition
    operation(operation(First, Operator, Second)) --> general_body(First), binary_operator(Operator), specific_body(Second).
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
  specific_body(Body) --> parentheses(Body).
  specific_body(Body) --> function_call(Body).
  % General body
  general_body(Body) --> specific_body(Body).
  general_body(Body) --> operation(Body).
  % Advanced body
  advanced_body(Body) --> general_body(Body).
  advanced_body(Body) --> ternary_operation(Body).
