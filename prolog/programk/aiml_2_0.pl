
:- module(programk,[process_argv_aiml/0]).

:- ensure_loaded(library(logicmoo_common)).

:- ensure_loaded(logicmoo_module_aiml_toplevel).

:- ensure_loaded(logicmoo_module_aiml_testing).

process_argv_aiml:- ignore((current_prolog_flag(argv,Y),alicebot(Y))).

% :- initialization(process_argv_aiml).

