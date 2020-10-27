:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_server)).
:- use_module(library(http/http_cors)).
:- use_module(library(http/json)).

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
  solve(Data, Response),
  reply_json_dict(Response).

solve(Input, Output) :-
  atom_json_dict(Source, Input, [as(atom)]),
  http_post(
    'http://localhost:8099/compile',
    atom('application/json', Source),
    Reply,
    [method(post)]
  ),
  atom_json_term(String, Reply, [as(string)]),
  open_string(String, Stream),
  json_read_dict(Stream, Output).
