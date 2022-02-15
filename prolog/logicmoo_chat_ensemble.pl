
:- module(logicmoo_chat_ensemble,[]).
% use_module(library(logicmoo_chat_ensemble)).

:- multifile(user:network_service_info/3).
:- dynamic(user:network_service_info/3).

user:network_service_info(pyaiml,port,4081).
user:network_service_info(neox,port,4082).
user:network_service_info(factoids,port,4083).

:- reexport(library(chat_ensemble/bot_factoids)).
:- reexport(library(chat_ensemble/bot_neox)).
:- reexport(library(chat_ensemble/bot_pyaiml)).

:- install_converter(bot_pyaiml, pyaiml_parse(+text80, -text_response)).
:- install_converter(bot_neox, neox_completion(+text80, -text_completion)).
:- install_converter(bot_factoids, factoids_parse(+text80, -text_response)).
