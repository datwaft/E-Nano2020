:- module(transpiler, [
  transpile/
]).

:- set_prolog_flag(double_quotes, chars).

% Utils
% =====
  % Transpile
  transpile(Tree, ClassName, String) :-
    phrase(program(Tree, ClassName), Ls), atomic_list_concat(Ls, String).

% Program body definition
% =======================
  program(program(Global, Main), ClassName) -->
    "public class ", [ClassName], " {\n",
      global(Global),
      main(Main),
      "public static void main(String[] args) {\n",
        "var me = new ", [ClassName], "();\n",
        "me.main();\n",
      "}\n",
    "}\n".
% Global scope definition
% =======================
  global(Assignments) --> declaration_list(Assignments).
  declaration_list([]) --> [].
  declaration_list([Declaration | Rest]) -->
    declaration(Declaration), ";\n",
    declaration_list(Rest).
  declaration_list([Declaration | Rest]) -->
    method(Declaration), ";\n",
    declaration_list(Rest).
% Main scope definition
% =====================
  main(Statements) -->
    "public void main() {\n",
      statement_list(Statements), 
    "}\n".
  statement_list([]) --> [].
  statement_list([Statement | Rest]) -->
    declaration(Statement), ";\n",
    statement_list(Rest).
  statement_list([Statement | Rest]) -->
    method(Statement), ";\n",
    statement_list(Rest).
  statement_list([Statement | Rest]) -->
    assignment(Statement), ";\n",
    statement_list(Rest).
  statement_list([Statement | Rest]) -->
    function_call(Statement), ";\n",
    statement_list(Rest).
% Assignment definition
% =====================
  assignment(assignment(Name, Value)) --> 
    variable_name(Name), " = ", advanced_body(Value).
% Declaration definition
% ======================
  declaration(declaration(Type, Name, Value)) -->
    declaration(declaration(Type, Name)), " = ", advanced_body(Value).
  declaration(declaration(Type, Name)) -->
    type(Type), " ", variable_name(Name).
  declaration(declaration(Type, Name, Lambda)) -->
    lambda_type(Type), " ", variable_name(Name), " = ", lambda(Lambda).
% Method definition
% =================
  method(method(Type, Function, Body)) -->
    method_declaration(Type, Function), " {\n",
      "return ", lambda_body(Body), ";\n",
    "}".
  method(method(Generics, Type, Function, Body)) -->
    "<", lambda_type_list(Generics), "> ", method_declaration(Type, Function), " {\n",
      "return ", lambda_body(Body), ";\n",
    "}".
  % Method declaration
  method_declaration(type(From, To), function_declaration(Name, Arguments)) -->
    type(To), " ", variable_name(Name), "(", method_arguments(From, Arguments), ")".
  % Method arguments
  method_arguments([], []) --> [].
  method_arguments([Type], [Name]) --> 
    type(Type), " ", variable_name(Name).
  method_arguments([Type | TRest], [Name | NRest]) --> 
    { TRest \= [], NRest \= [] },
    type(Type), " ", variable_name(Name), ", ", method_arguments(TRest, NRest).
% Lambda definition
% =================
  lambda(lambda(Variable, Body)) -->
    variable_name(Variable), " -> ", lambda_body(Body).
  lambda(lambda(Variable, Body)) -->
    "(", lambda_parameter_list(Variable), ") -> ", lambda_body(Body).
  % Lambda parameter list
  lambda_parameter_list([]) --> [].
  lambda_parameter_list([Variable]) -->
    variable_name(Variable).
  lambda_parameter_list([Variable | Rest]) -->
    { Rest \= [] },
    variable_name(Variable), ", ", lambda_parameter_list(Rest).
  % Body
  lambda_body(body(Body)) --> advanced_body(Body).
% Type definition
% ===============
  type(type(Type)) --> identifier(Type).
  type(list_type(Type)) --> "List<", type(Type), ">".
  type(Type) --> lambda_type(Type).
  % Method type definition
  lambda_type(type([From], To)) --> {From = To}, "UnaryOperator<", type(From), ", ", type(To), ">", {!}.
  lambda_type(type([From], To)) --> "Function<", type(From), ", ", type(To), ">", {!}.
  lambda_type(type([From1, From2], To)) --> "BiFunction<", type(From1), ", ", type(From2), ", ", type(To), ">", {!}.
  lambda_type(type(From, To)) --> "Function<", lambda_type_list(From), ", ", type(To), ">".
  % Lambda type list
  lambda_type_list([]) --> [].
  lambda_type_list([Type]) --> type(Type).
  lambda_type_list([Type | Rest]) -->
    { Rest \= [] },
    type(Type), ", ", lambda_type_list(Rest).
% Term
% ====
  term(number(Term)) --> [Term].
  term(string(Term)) --> [Term].
  term(list(Term)) --> "List.of(", list_body(Term), ")".
  term(const(Term)) --> [Term].
  term(Term) --> variable_name(Term).
  % List body
  list_body([]) --> [].
  list_body([Term]) --> general_body(Term).
  list_body([Term | Rest]) --> { Rest \= [] }, general_body(Term), ", ", list_body(Rest).
  % Parentheses
  parentheses(parentheses(Expression)) --> "(", advanced_body(Expression), ")".
% Function definition
% ===================
  % Function declaration definition
  function_declaration(function_declaration(Name, Arguments)) -->
    variable_name(Name), "(", function_declaration_arguments(Arguments), ")".
  function_declaration_arguments([]) --> [].
  function_declaration_arguments([Arg]) --> variable_name(Arg).
  function_declaration_arguments([Arg | Rest]) -->
    { Rest \= [] },
    variable_name(Arg), ", ", function_declaration_arguments(Rest).
  % Funciton call definition
  function_call(function_call(Name, Arguments)) --> function_call_namespaces(Name), "(", function_call_arguments(Arguments), ")".
  % Function call namespaces
  function_call_namespaces([Name | Rest]) --> variable_name(Name), ".", function_call_namespaces(Rest).
  function_call_namespaces([Name]) --> variable_name(Name).
  % Function call arguments
  function_call_arguments([]) --> [].
  function_call_arguments([Arg]) --> advanced_body(Arg).
  function_call_arguments([Arg | Rest]) -->
    { Rest \= [] },
    advanced_body(Arg), ", ", function_call_arguments(Rest).
% Operation definition
% ====================
  % Operator definition
    unary_ls_operator(lsoperator(Operator)) --> [Operator].
    unary_rs_operator(rsoperator(Operator)) --> [Operator].
    binary_operator(bioperator(Operator)) --> [Operator].
  % Unary operation definiton
    operation(operation(Operator, Term)) --> unary_ls_operator(Operator), specific_body(Term).
    operation(operation(Term, Operator)) --> specific_body(Term), unary_rs_operator(Operator).
  % Binary operation definition
    operation(operation(First, Operator, Second)) --> general_body(First), " ", binary_operator(Operator), " ", specific_body(Second).
  % Ternary operation definition
    ternary_operation(operation(True, Condition, False)) --> general_body(Condition), " ? ", general_body(True), " : ", advanced_body(False).
% Identifier definition
% =====================
  % Variable name
  variable_name(var(Name)) --> identifier(Name).
  % Identifier
  identifier(Name) --> [Name].
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
