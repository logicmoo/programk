
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
%:- context_module(M),hidden_away:module(hidden_away),
%   module(M),hidden_away:use_module(library(debuggery/dmsg),[dmsg/1,dmsg/2]).

:- use_module(library(debuggery/ucatch),[catchv/3]).
:- use_module(library(programk/cyc_pl/cyc),[is_string/1,isConsole/0,atom_to_number/2,balanceBinding/2,writeFmtFlushed/2,writeFmtFlushed/3,toCycApiExpression/3]).

:- multifile(dumpst_hook:simple_rewrite/2).
:- dynamic(dumpst_hook:simple_rewrite/2).

dumpst_hook:simple_rewrite(I,O):- hide_complex_ctx(I,O).

