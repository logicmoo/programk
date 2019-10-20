
:-ensure_loaded(('cyc_pl/cyc.pl')).

/*
:- if( predicate_property(cyc:debugFmt(_), defined)).
:- abolish(cyc:debugFmt/1).
:- endif.

:- if( predicate_property(cyc:debugFmt(_,_), defined)).
:- abolish(cyc:debugFmt/2).
:- else.
:- endif.
:- if(\+ predicate_property(nop(_),defined)).
:- endif.
:- if( \+ predicate_property(term_to_string(_,_),defined)).
:- endif.
%:- if(\+ predicate_property(alldiscontiguous(),defined)).
%:- endif.

*/
term_to_string(I,IS):- error_catch(string_to_atom(IS,I),_,fail),!.
term_to_string(I,IS):- term_to_atom(I,A),string_to_atom(IS,A),!.
alldiscontiguous:-!.
cyc:debugFmt(Stuff):-once(lmdebugFmt(Stuff)).
cyc:debugFmt(F,A):-once(lmdebugFmt(F,A)).

nop(_).

string_parse_structure_opts_547(Parser, _In, _M, Options,Options2):-
	sgml:set_parser_options(Parser, Options, Options1),
	Options2=Options1.

:- module_transparent(setup_call_cleanup/3).

setup_call_cleanup(X,Y,Z):- X,!,call_cleanup(Y,Z).
% atomic_list_concat_aiml(X,Y,Z):- atomic_list_concat_aiml(X,Y,Z).


