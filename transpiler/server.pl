:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_server)).
:- use_module(library(http/http_cors)).
:- use_module(library(http/json)).

:- use_module('lexer').
:- use_module('parser').
:- use_module('transpiler').

:- http_handler('/compile', compile_handler, []).

:- set_setting_default(http:cors, [*]).

:- initialization
  (current_prolog_flag(argv, [SPort | _]) -> true ; SPort='8077'),
  atom_number(SPort, Port),
  format('Starting server on port ~d...', [Port]),
  server(Port).

server(Port) :-
  http_server(http_dispatch, [port(Port)]).

compile_handler(Request) :-
  option(method(options), Request), !,
  cors_enable(Request, [methods([get, post, delete])]),
  format('~n').

compile_handler(Request) :-
  cors_enable,
  member(method(post), Request), !,
  http_read_json_dict(Request, Data),
  transpile(Data, Response),
  reply_json_dict(Response).

transpile(_{ source: Input }, Response) :-
  string_to_tokens(Input, Tokens),
  phrase(program(Tree), Tokens),
  transpile(Tree, 'TempClassName', Output),
  ( catch(solve(_{ source: Output}, Response), _, fail) ; solve(Response) ).

transpile(_, _{ messages: [ ['error', 'The code wasn\'t able to be transpiled properly.'] ] }) :- !.

solve(Input, Output) :-
  atom_json_dict(Source, Input, [as(atom)]),
  http_post(
    'http://localhost:8099/compile',
    atom('application/json', Source),
    Reply,
    [method(post)]
  ),
  atom_json_term(String, Reply, []),
  atom_json_dict(String, Output, []).

solve(_{ messages: [ ['error', 'Couldn\'t contact the compilation server.'] ] }).
