% ===================================================================
% File 'logicmoo_util_bugger.pl'
% Purpose: An Implementation in SWI-Prolog of certain debugging tools
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'logicmoo_util_bugger.pl' 1.0.0
% Revision:  $Revision: 1.1 $
% Revised At:   $Date: 2002/07/11 21:57:28 $
% ===================================================================
% ===================================================================
:-module(bugger,[

         programmer_error/1,
         global_pathname/2,
         printAll/2,

 
         os_to_prolog_filename/2,
         dumpList/1,

         nop/1,
         test_call/1,
         makeArgIndexes/1,
         printAll/1,
         dynamic_load_pl/1,

%%%         read_line_with_nl/3,
	 unnumbervars/2,

         flush_output_safe/1,

         logOnFailure/1,
	 
         debugFmt/1,
	 debugFmt/2,
	 debugFmtFast/1,

         loggerFmt/1,
         loggerFmt/2,
         loggerFmt/3,
         %%logLevel/2,
         setLogLevel/2,

         logOnFailureIgnore/1,
	 sendNote/1,
	 sendNote/4,
	 writeFailureLog/2,
	%% debugOnFailure/2,
	 debugOnFailure/1,
         debugOnFailureEach/1,
         prolog_must/1,


         writeFmtFlushed/1,
         writeFmtFlushed/2,
         writeFmtFlushed/3,

         unlistify/2,
         listify/2,

         canTrace/0, 
         ctrace/0,
         isConsole/0,
        
         must_assign/1,
         must_assign/2,
         hotrace/1,

         bugger/0


	 ]).

:-ensure_loaded('../logicmoo/logicmoo_util_library.pl').
:-use_module(library('logicmoo/logicmoo_util_library.pl')).
:-use_module(library('logicmoo/logicmoo_util_ctx_frame.pl')).

:- multi_transparent(current_directory_search/1).
:- multi_transparent(isDebugOption/1).
:- multi_transparent(formatter_hook/4).
:-module_transparent(hotrace/1).

atom_contains(F,C):- hotrace((atom(F),atom(C),sub_atom(F,_,_,_,C))).

:-guitracer.

dhideTrace(X):-'$hide'(X),!.

% ==========================================================
%  can/will Tracer.
% ==========================================================
 
:-dynamic(canTrace/0).
canTrace.

%isConsole :- telling(user).
isConsole :- current_output(X),!,stream_property(X,alias(user_output)).

willTrace:-not(isConsole),!,fail.
willTrace:-canTrace.

hideTrace:-
   hideTrace([hotrace/1], -all),
   %%hideTrace(computeInnerEach/4, -all),

   hideTrace(
     [maplist_safe/2, 
              maplist_safe/3], -all),


   hideTrace([hideTrace/0,
         canTrace/0,
         ctrace/0,         
         willTrace/0], -all),

   hideTrace([
         traceafter_call/1,

         notrace_call/1], -all),

   hideTrace(user:[
      call/1,
      call/2,
      apply/2,
      '$bags':findall/3,
      '$bags':findall/4,
      once/1,
      ','/2,
      catch/3,
      member/2], -all),

   hideTrace(user:setup_call_catcher_cleanup/4,-all),

   hideTrace(system:throw/1, +all),
   %%hideTrace(system:print_message/2, +all),
   hideTrace(user:message_hook/3 , +all),
   hideTrace(system:message_to_string/2, +all),
   !,hideRest,!.
   %%findall(File-F/A,(functor_source_file(M,P,F,A,File),M==user),List),sort(List,Sort),debugFmt(Sort),!.

hideRest:- fail, logicmoo_util_library:buggerDir(BuggerDir),
      functor_source_file(M,_P,F,A,File),atom_concat(BuggerDir,_,File),hideTraceMFA(M,F,A,-all),
      fail.
hideRest:- functor_source_file(system,_P,F,A,_File),hideTraceMFA(system,F,A,-all), fail.
hideRest.

:- meta_predicate(hideTrace(:,+)).

functor_source_file(M,P,F,A,File):-functor_source_file0(M,P,F,A,File). %% prolog_must(ground((M,F,A,File))),prolog_must(user:nonvar(P)).
functor_source_file0(M,P,F,A,File):-current_predicate(F/A),functor(P,F,A),source_file(P,File),predicate_module(P,M).

predicate_module(P,M):- predicate_property(P,imported_from(M)),!.
predicate_module(M:_,M):-!. %strip_module(P,M,_F),!.
predicate_module(_P,user):-!. %strip_module(P,M,_F),!.
%%predicate_module(P,M):- strip_module(P,M,_F),!.

hideTrace(_:A, _) :-
        var(A), !, trace, fail,
        throw(error(instantiation_error, _)).
hideTrace(_:[], _) :- !.
hideTrace(A:[B|D], C) :- !,
        hideTrace(A:B, C),
        hideTrace(A:D, C),!.

hideTrace(M:A,T):-!,hideTraceMP(M,A,T),!.
hideTrace(MA,T):-hideTraceMP(_,MA,T),!.

hideTraceMP(M,F/A,T):-!,hideTraceMFA(M,F,A,T),!.
hideTraceMP(M,P,T):-functor(P,F,0),trace,hideTraceMFA(M,F,_A,T),!.
hideTraceMP(M,P,T):-functor(P,F,A),hideTraceMFA(M,F,A,T),!.

tryCatchIgnore(MFA):- catch(MFA,_E,true). %%debugFmt(tryCatchIgnoreError(MFA:E))),!.
tryCatchIgnore(_MFA):- !. %%debugFmt(tryCatchIgnoreFailed(MFA)).

tryHide(MFA):- tryCatchIgnore('$hide'(MFA)).

hideTraceMFA(_,M:F,A,T):-!,hideTraceMFA(M,F,A,T),!. 
hideTraceMFA(M,F,A,T):-user:nonvar(A),functor(P,F,A),predicate_property(P,imported_from(IM)),IM \== M,!,nop(debugFmt(doHideTrace(IM,F,A,T))),hideTraceMFA(IM,F,A,T),!.
hideTraceMFA(M,F,A,T):-hideTraceMFAT(M,F,A,T),!.

hideTraceMFAT(M,F,A,T):-doHideTrace(M,F,A,T),!.

doHideTrace(_M,_F,_A,[]):-!.
doHideTrace(M,F,A,[hide|T]):- tryHide(M:F/A),!,doHideTrace(M,F,A,T),!.
doHideTrace(M,F,A,ATTRIB):- tryHide(M:F/A),!, 
   tryCatchIgnore(trace(M:F/A,ATTRIB)),!.


ctrace:-willTrace->trace;notrace.

bugger:-hideTrace,traceAll,guitracer,debug,list_undefined.

singletons(_).



dumpList(B):- currentContext(dumpList,Ctx),dumpList(Ctx,B).
dumpList(_,AB):-debugFmt(dumpList(AB)),!.

dumpList(_,[]):-!.
%dumpList(Ctx,[A|B]):-!,say(Ctx,A),dumpList(Ctx,B),!.
%dumpList(Ctx,B):-say(Ctx,dumpList(B)).


ifThen(When,Do):-When->Do;true.

%%:- current_predicate(F/N),trace(F/N, -all),fail.
/*
traceAll:- current_predicate(user:F/N),
   functor(P,F,N),
   local_predicate(P,F/N),
   trace(F/N, +fail),fail.
traceAll:- not((predicate_property(clearCateStack/1,_))),!.
traceAll:-findall(_,(member(F,[member/2,debugFmt/1,takeout/3,findall/3,clearCateStack/1]),trace(F, -all)),_).
*/
traceAll:-!.



%%% peekAttributes/2,pushAttributes/2,pushCateElement/2.

/*
neverUse:- meta_predicate_transparent
	maplist_safe(2,:),
	maplist_safe(3,:,:),
        asserta_new(2,:),
        writeqnl(2,:),
        prolog_must_tracing(1),
        prolog_must(1),
        beenCaught(1),
        debugOnFailureEach(1),
        prolog_must(1),ignore(1), %%withAttributes(3,:,:),call_cleanup(0,0),call_cleanup(0,?,0),
        !.
*/


%:-module()
%:-include('logicmoo_utils_header.pl'). %<?
%:- style_check(-singleton).
%%:- style_check(-discontiguous).
:- style_check(-atom).
:- style_check(-string).

:-op(1150,fx,meta_predicate_transparent).

must_assign(From=To):-must_assign(From,To).
must_assign(From,To):-To=From,!.
must_assign(From,To):-debugFmt(From),debugFmt(=),debugFmt(From),debugFmt(must_assign),!,trace,To=From.

:-dhideTrace(prolog_must/1).
:-dhideTrace(ctrace/0).
prolog_must(Call):-tracing,!,prolog_must_tracing(Call).
prolog_must(Call):-debugOnFailure(Call).


:-dhideTrace(debugOnFailure/1).
:-dhideTrace(debugOnFailure0/1).
debugOnFailure(X):-prolog_ecall(debugOnFailure0,X).
debugOnFailure0(X):-!,atLeastOne(X).
%%debugOnFailure0(X):-!,atLeastOne(debugOnError(X)).
%%debugOnFailure0(X):-catch(X,E,(writeFailureLog(E,X),throw(E))).
%%debugOnFailure0(X):-ctrace,X.

debugOnFailure1(arg_domains,CALL):-!,logOnFailure(CALL),!.
debugOnFailure1(Module,CALL):-trace,debugOnFailure(Module:CALL),!.


:-dhideTrace(debugOnError/1).
debugOnError(Call):- prolog_ecall(debugOnError0,Call).   
:-dhideTrace(debugOnError0/1).
debugOnError0(Call):- catch(Call,E,debugFmt(error(Call,E),trace,Call)).


:-'$hide'(prolog_may/1).
prolog_may(Call):-prolog_ecall(debugOnError,Call).


:-dhideTrace(prolog_must_tracing/1).
:-dhideTrace(prolog_must_tracing0/1).
prolog_must_tracing(Call):-!, Call.
prolog_must_tracing(Call):- notrace,prolog_ecall(prolog_must_tracing0,Call).   
prolog_must_tracing0(Call):- 
   notrace((trace(Call,Before),trace(Call,[-all,+fail,+exit]))), 
   atLeastOne(Call,(trace,Call)), 
   notrace(trace(Call,Before)).


traceCall(A):-trace(A,[-all,+fail]),A,!.


:-dhideTrace(debugOnFailureEach/1).
:-dhideTrace(debugOnFailureEach0/1).
debugOnFailureEach(Call):- prolog_ecall(debugOnFailure,fail,debugOnFailureEach0,Call).
debugOnFailureEach0(Call):- once(Call;(trace,Call)).


beenCaught(prolog_must(Call)):- !, beenCaught(Call).
beenCaught((A,B)):- !,beenCaught(A),beenCaught(B).
beenCaught(Call):- fail, predicate_property(Call,number_of_clauses(_Count)), clause(Call,(_A,_B)),!,clause(Call,Body),beenCaught(Body).
beenCaught(Call):- catch(once(Call),E,(debugFmt(caugth(Call,E)),beenCaught(Call))),!.
beenCaught(Call):- traceAll,debugFmt(tracing(Call)),debug,trace,Call.


local_predicate(_,_/0):-!,fail.
local_predicate(_,_/N):-N>7,!,fail.
local_predicate(P,_):-predicate_property(P,built_in),!,fail.
local_predicate(P,_):-predicate_property(P,imported_from(_)),!,fail.
local_predicate(P,_):-predicate_property(P,file(F)),!,atom_contains(F,'aiml_'),!.
local_predicate(P,F/N):-functor(P,F,N),!,fail.




:-dhideTrace(prolog_ecall/2).
:-dhideTrace(prolog_ecall/4).
prolog_ecall(Pred,Call):-prolog_ecall(call,fail,Pred,Call).
prolog_ecall(ClauseEach,Pred,Call):-prolog_ecall(ClauseEach,fail,Pred,Call).

prolog_ecall(_ClauseEach,_Tracing,_Pred,Call):-notrace,var(Call),!,trace,randomVars(Call).
prolog_ecall(_ClauseEach,_Tracing,_Pred,Call):-functor(Call,F,_),member(F,[assert,asserta,assertz]),!,catch(Call,_,true),!.
prolog_ecall(_ClauseEach,_Tracing,_Pred,Call):-functor(Call,F,A),member(F/A,[retract/_,retractall/_]),!,debugFmt(fakingCall(Call)),numbervars(Call,0,_),!.
prolog_ecall(ClauseEach,Tracing,Pred,(X->Y;Z)):-!,(hotrace(X) -> prolog_ecall(ClauseEach,Tracing,Pred,Y) ; prolog_ecall(ClauseEach,Tracing,Pred,Z)).
prolog_ecall(ClauseEach,Tracing,Pred,(X->Y)):-!,(hotrace(X)->prolog_ecall(ClauseEach,Tracing,Pred,Y)).
prolog_ecall(ClauseEach,Tracing,Pred,(X;Y)):-!,prolog_ecall(ClauseEach,Tracing,Pred,X);prolog_ecall(ClauseEach,Tracing,Pred,Y).
prolog_ecall(ClauseEach,Tracing,Pred,(X,Y)):-!,prolog_ecall(ClauseEach,Tracing,Pred,X),prolog_ecall(ClauseEach,Tracing,Pred,Y).
prolog_ecall(ClauseEach,Tracing,Pred,prolog_must(Call)):-!,prolog_ecall(ClauseEach,Tracing,Pred,Call).
prolog_ecall(ClauseEach,_Tracing,_Pred,Call):- fail, ignore((Call=atom(_),trace)), 
    predicate_property(Call,number_of_clauses(_Count)),
    clause(Call,NT),NT \== true,!,
    catch(clause(Call,Body),_,
      (trace,predicate_property(Call,number_of_clauses(_Count2)),
      clause(Call,Body))),
      call(ClauseEach,Body).

prolog_ecall(_ClauseEach,_Fail,Pred,Call):- call(Pred,Call).
%prolog_ecall(_ClauseEach,Fail,_Pred,Call):- Fail\=fail, call(Fail,Call).


programmer_error(E):-trace,  randomVars(E),debugFmt('~q~n',[error(E)]),trace,randomVars(E),!,throw(E).


:-dhideTrace(atLeastOne/1).
:-dhideTrace(atLeastOne/2).
:-dhideTrace(atLeastOne0/2).

atLeastOne(OneA):- atLeastOne(OneA,(trace,OneA)).
atLeastOne(OneA,Else):-atLeastOne0(OneA,Else).

atLeastOne0(OneA,_Else):-copy_term(OneA,One),findall(One,call(One),OneL),[_|_]=OneL,!,member(OneA,OneL).
atLeastOne0(OneA,Else):-debugFmt(failed(OneA)),!,Else,!,fail.


randomVars(Term):- random(R), StartR is round('*'(R,1000000)), !,
  %ignore(Start=0),
  ignore(Start=StartR),
  numbervars(Term, Start, _End, [attvar(skip),functor_name('$VAR')]).

prolog_must_not(Call):-Call,!,trace,!,programmer_error(prolog_must_not(Call)).
prolog_must_not(_Call):-!.

%:- meta_predicate dynamic_if_missing(:).
%:- meta_predicate meta_predicate_transparent(:).


dynamic_if_missing(F/A):-functor(X,F,A),predicate_property(X,_),!.
dynamic_if_missing(F/A):- 
  dynamic([F/A]).

meta_predicate_transparent(X):-strip_module(X,M,F),!, meta_predicate_transparent(M,F).
meta_predicate_transparent(M,(X,Y)):-!,meta_predicate_transparent(M,X),meta_predicate_transparent(M,Y),!.
meta_predicate_transparent(_M,X):-atom(X),!.
meta_predicate_transparent(_M,X):- 
   debugOnFailureEach((   
   arg(1,X,A),functor(X,F,_),
   FA=F/A,
   dynamic_if_missing(FA),
   %module_transparent(FA),
   %%meta_predicate(X),
   %trace(FA, -all),
   %%dhideTrace(FA),
   !)).


asserta_new(_Ctx,NEW):-ignore(retract(NEW)),asserta(NEW).
writeqnl(_Ctx,NEW):- format('~q.~n',[NEW]),!.


%%%retractall(E):- retractall(E),functor(E,File,A),dynamic(File/A),!.

pp_listing(_Pred):-!. %%functor(Pred,File,A),functor(FA,File,A),listing(File),nl,findall(NV,predicate_property(FA,NV),LIST),writeq(LIST),nl,!.

% =================================================================================
% Utils
% =================================================================================

printPredCount(Msg,Pred,N1):- compound(Pred), debugOnFailureEach((arg(_,Pred,NG))),user:nonvar(NG),!,
   findall(Pred,Pred,LEFTOVERS),length(LEFTOVERS,N1),debugFmt(num_clauses(Msg,Pred,N1)),!.

printPredCount(Msg,Pred,N1):-!,functor(Pred,File,A),functor(FA,File,A), predicate_property(FA,number_of_clauses(N1)),debugFmt(num_clauses(Msg,File/A,N1)),!.

% =================================================================================
% Loader Utils
% =================================================================================


dynamic_load_pl(PLNAME):-consult(PLNAME),!.

dynamic_load_pl(PLNAME):- %% unload_file(PLNAME),
     open(PLNAME, read, In, []),
     repeat,
      line_count(In,Lineno),
      %% double_quotes(_DQBool)
      Options = [variables(_Vars),variable_names(_VarNames),singletons(_Singletons),comment(_Comment)],
      catch((read_term(In,Term,[syntax_errors(error)|Options])),E,(debugFmt(E),fail)),      
      load_term(Term,[line_count(Lineno),file(PLNAME),stream(In)|Options]),
     Term==end_of_file,
     close(In).

load_term(end_of_file,_Options):-!.
load_term(Term,Options):-catch(load_term2(Term,Options),E,(debugFmt(error(load_term(Term,Options,E))),throw_safe(E))).

load_term2(':-'(Term),Options):-!,load_dirrective(Term,Options),!.
load_term2(:-(H,B),Options):-!,load_assert(H,B,Options).
load_term2(Fact,Options):-!,load_assert(Fact,true,Options).

load_assert(H,B,_Options):-assert((H:-B)),!.

load_dirrective(include(PLNAME),_Options):-  (atom_concat_safe(Key,'.pl',PLNAME);Key=PLNAME),!, dynamic_load_pl(PLNAME).
load_dirrective(module(M,Preds),_Options):-!,module(M),call(module(M,Preds)).
load_dirrective(Term,_Options):-!,Term.

showProfilerStatistics(FileMatch):-
   statistics(global,Mem), MU is (Mem / 1024 / 1024),
   printPredCount('showProfilerStatistics: '(MU),FileMatch,_N1).



% ===============================================================================================
% UTILS
% ===============================================================================================


unify_listing(FileMatch):-functor(FileMatch,F,A),unify_listing(FileMatch,F,A),!.
unify_listing_header(FileMatch):-functor(FileMatch,F,A),unify_listing_header(FileMatch,F,A),!.


unify_listing_header(FileMatch,F,A):- (format('~n/* Prediate:  ~q / ~q  ~n',[F,A,FileMatch])),fail.
unify_listing_header(FileMatch,_F,_A):- printAll(predicate_property(FileMatch,PP),PP),fail.
unify_listing_header(FileMatch,_F,_A):- (format('~n ~q. ~n */ ~n',[FileMatch])),fail.
unify_listing_header(FileMatch,F,A):-predicate_property(FileMatch,dynamic),(format(':-dynamic(~q).~n',[F/A])),fail.
unify_listing_header(FileMatch,F,A):-predicate_property(FileMatch,multifile),(format(':-multifile(~q).~n',[F/A])),fail.
unify_listing_header(_FileMatch,_F,_A).

unify_listing(FileMatch,F,A):- unify_listing_header(FileMatch,F,A), printAll(FileMatch).

printAll(FileMatch):-printAll(FileMatch,FileMatch).
printAll(Call,Print):- flag(printAll,_,0), forall((Call,flag(printAll,N,N+1)),(format('~q.~n',[Print]))),fail.
printAll(Call,Print):- flag(printAll,PA,0),(format('~n /* found ~q for ~q. ~n */ ~n',[PA,Print])).

contains_term(SearchThis,Find):-Find==SearchThis,!.
contains_term(SearchThis,Find):-compound(SearchThis),functor(SearchThis,Func,_),(Func==Find;arg(_,SearchThis,Arg),contains_term(Arg,Find)).

% =================================================================================
% Utils
% =================================================================================

global_pathname(B,A):-absolute_file_name(B,A),!.
global_pathname(B,A):-relative_pathname(B,A).

relative_pathname(Path,Relative):-absolute_file_name(Path,[relative_to('./')],Absolute),member(Rel,['./','../','../../']),absolute_file_name(Rel,Clip),
   canonical_pathname(Absolute,AbsoluteA),
   canonical_pathname(Clip,ClipA),
   atom_concat_safe(ClipA,RelativeA,AbsoluteA),!,atom_concat_safe(Rel,RelativeA,Relative),!.
relative_pathname(Path,Relative):-canonical_pathname(Path,Relative),!.

canonical_pathname(Absolute,AbsoluteB):-prolog_to_os_filename(AbsoluteA,Absolute),expand_file_name(AbsoluteA,[AbsoluteB]),!.

alldiscontiguous:-!.


% ===================================================================
% Lowlevel printng
% ===================================================================
writeFmtFlushed(X,Y,Z):-catch((format(X,Y,Z),flush_output_safe(X)),_,true).
writeFmtFlushed(X,Y):-catch((format(X,Y),flush_output),_,true).
writeFmtFlushed(X):- once((atom(X) -> catch((format(X,[]),flush_output),_,true) ; writeFmtFlushed('~q~n',[X]))).

% ===================================================================
% Lowlevel printng
% ===================================================================
unnumbervars(STUFF,UN):-sformat(S,'~W',[STUFF,[quoted(true),character_escapes(true),module(user),numbervars(true),portray(false),double_quotes(true)]]),string_to_atom(S,Atom),atom_to_term(Atom,UN,_).

open_list(V,V):-var(V).
open_list(A,B):-append(A,_,B).

unnumbervars_nil(X,Y):-!,unnumbervars(X,Y).

collect_temp_vars(VARS):-!,(setof(=(Name,Number),numbered_var(Name,Number),VARS);VARS=[]).

% ==========================================================
%  Sending Notes
% ==========================================================
 

logOnFailureIgnore(X):-ignore(logOnFailure(X)),!.

writeModePush(_Push):-!.
writeModePop(_Pop):-!.


if_prolog(swi,G):-call(G).  % Run B-Prolog Specifics
if_prolog(_,_):-!.  % Dont run SWI Specificd or others

%debugFmt([-1]).
%debugFmt([[-1]]).
%debugFmt(T):- isDebugOption(opt_debug=off),!.
debugFmt(StuffIn):-copy_term(StuffIn,Stuff), randomVars(Stuff),!,debugFmt('% ~q~n',[Stuff]).
%:- abolish(debugFmt/1).
%debugFmt(Stuff):- notrace((debugFmtS(Stuff))),!.

debugFmt(T):-!,
	((
	if_prolog(swi,
		(prolog_current_frame(Frame),
		prolog_frame_attribute(Frame,level,Depth),!,
		Depth2 = (Depth-25))),
	writeFmt(';;',[T]),!,
	indent_e(Depth2),!,
	writeFmt('~q\n',[T]))),!.

indent_e(X):- catch((X < 2),_,true),write(' '),!.
indent_e(X):-XX is X -1,!,write(' '), indent_e(XX).


:-dynamic(user:logLevel/2).
:-module_transparent(user:logLevel/2).
:-multifile(user:user:logLevel/2).

setLogLevel(M,L):-retractall(user:logLevel(M,_)),(user:nonvar(L)->asserta(user:logLevel(M,L));true).

user:logLevel(debug,user_error).
user:logLevel(error,user_error).
user:logLevel(private,none).
user:logLevel(S,Z):-current_stream(_X,write,Z),stream_property(Z,alias(S)).

loggerReFmt(L,LRR):-user:logLevel(L,LR),L \==LR,!,loggerReFmt(LR,LRR),!.
loggerReFmt(L,L).

loggerFmt(LF):-functor(LF,F,_),loggerReFmt(F,LR),
  LR==F->loggerFmtReal(F,LF,[]);loggerFmt(LR,LF,[]).

loggerFmt(L,F):-loggerReFmt(L,LR),loggerFmtReal(LR,F,[]).
loggerFmt(L,F,A):-loggerReFmt(L,LR),loggerFmtReal(LR,F,A).

loggerFmtReal(none,_F,_A):-!.
loggerFmtReal(S,F,A):-
   current_stream(_,write,S),
        writeFmtFlushed(S,F,A),
        flush_output_safe(S),!.


%debugFmt(C,T):- isDebugOption(opt_debug=off),!.
debugFmt(_,F):-F==[-1];F==[[-1]].

debugFmt(F,A):-
        nl(user_error),
        writeFmtFlushed(user_error,F,A),
        nl(user_error),
        flush_output_safe(user_error),!.

debugFmt(C,T):-!,
	((
	writeFmt('<font size=+1 color=~w>',[C]),
	debugFmt(T),
        writeFmt('</font>',[]))),!.

dumpstack_argument(_T):-isDebugOption(opt_debug=off),!.  
	
dumpstack_argument(Frame):-
	write(frame=Frame),write(' '),
	dumpstack_argument(1,Frame).

dumpstack_argument(1,Frame):-!,
	prolog_frame_attribute(Frame,goal,Goal),!,
	write(goal=Goal),write('\n').
	
dumpstack_argument(N,Frame):-
	prolog_frame_attribute(Frame,argument(N),O),!,
	write(N=O),write(' '),
	NN is N +1,
	dumpstack_argument(NN,Frame).
	
dumpstack_argument(_N,_Frame):-!,write('\n').
	
:-dynamic_transparent(seenNote/1).

sendNote(X):-var(X),!.
sendNote(X):-seenNote(X),!.
sendNote(X):-!,assert(seenNote(X)).
sendNote(_).			 

sendNote(To,From,Subj,Message):-sendNote(To,From,Subj,Message,_).

sendNote(To,From,Subj,Message,Vars):-
	not(not((safe_numbervars((To,From,Subj,Message,Vars)),
	%debugFmt(sendNote(To,From,Subj,Message,Vars)),
	catch(sendNote_1(To,From,Subj,Message,Vars),E,
	writeFmt('send note ~w ~w \n <HR>',[E,sendNote(To,From,Subj,Message,Vars)]))))).

withFormatter(Lang,From,Vars,SForm):-formatter_hook(Lang,From,Vars,SForm),!.
withFormatter(_Lang,From,_Vars,SForm):-sformat(SForm,'~w',[From]).


sendNote_1(To,From,Subj,surf,Vars):-singletons([To,Subj,From,Vars]),!.
sendNote_1(To,From,[],surf,Vars):-singletons([To,From,Vars]),!.
sendNote_1(To,From,[],end_of_file,Vars):-singletons([To,From,Vars]),!.
sendNote_1(doug,From,_,_,Vars):-singletons([From,Vars]),!.
sendNote_1(extreme_debug,_From,_,_,_Vars):-!.
sendNote_1(debug,'Belief',_,_,_Vars):-!.

%sendNote_1(canonicalizer,From,Subj,Message,Vars):-!.


sendNote_1(canonicalizer,From,Subj,Message,Vars):-
            withFormatter(cycl,From,Vars,SFrom),
            withFormatter(cycl,nv(Subj),Vars,SS),
            withFormatter(cycl,nv(Message),Vars,SA),
            writeFmt('<font color=red>canonicalizer</font>: ~w "~w" (from ~w). \n',[SA,SS,SFrom]),!.

/*

sendNote_1(debug,From,Subj,Message,Vars):- %isDebugOption(disp_notes_nonuser=on),!,
            withFormatter(cycl,From,Vars,SFrom),
            withFormatter(cycl,Subj,Vars,SS),
            withFormatter(cycl,Message,Vars,SA),
            writeFmt('% debug: ~w "~w" (from ~w). \n',[SA,SS,SFrom]).
sendNote_1(debug,From,Subj,Message,Vars):-!.
*/

            /*


sendNote_1(To,From,Subj,Message,Vars):- isDebugOption(client=consultation),  !, 
            withFormatter(cycl,To,Vars,STo),
            withFormatter(cycl,From,Vars,SFrom),
            withFormatter(cycl,nv(Subj),Vars,S),
            withFormatter(cycl,nv(Message),Vars,A),
            fmtString(Output,'~w (~w from ~w) ',[A,S,SFrom]),
	    sayn(Output),!.

sendNote_1(To,From,'Rejected',Message,Vars):- isDebugOption(client=automata),  !.

sendNote_1(To,From,Subj,Message,Vars):- isDebugOption(client=automata),  !, 
            withFormatter(cycl,To,Vars,STo),
            withFormatter(cycl,From,Vars,SFrom),
            withFormatter(cycl,nv(Subj),Vars,S),
            withFormatter(cycl,nv(Message),Vars,A),
            writeFmt(user_error,'% ~w (~w from ~w) ',[A,S,SFrom]).

sendNote_1(To,From,Subj,Message,Vars):- isDebugOption(client=html),  !, %  In Html
            withFormatter(cycl,To,Vars,STo),
            withFormatter(cycl,From,Vars,SFrom),
            withFormatter(cycl,nv(Subj),Vars,S),
            withFormatter(html,nv(Message),Vars,A),
            writeFmt('<hr><B>To=<font color=green>~w</font> From=<font color=green>~w</font> Subj=<font color=green>~w</font></B><BR>~w\n',[To,From,S,A]),!.

sendNote_1(To,From,Subj,Message,Vars):- isDebugOption(client=console),!, % In CYC
            withFormatter(cycl,To,Vars,STo),
            withFormatter(cycl,From,Vars,SFrom),
            withFormatter(cycl,nv(Subj),Vars,SS),
            withFormatter(cycl,nv(Message),Vars,SA),
            writeFmt(user_error,'; ~w: ~w "~w" (from ~w). \n',[STo,SA,SS,SFrom]),!.
  
sendNote_1(To,From,Subj,Message,Vars):-  % In CYC
            withFormatter(cycl,To,Vars,STo),
            withFormatter(cycl,From,Vars,SFrom),
            withFormatter(cycl,nv(Subj),Vars,SS),
            withFormatter(cycl,nv(Message),Vars,SA),
            writeFmt(user_error,'; ~w: ~w "~w" (from ~w). \n',[STo,SA,SS,SFrom]),!.

sendNote(To,From,Subj,Message,Vars):-!.
                                                                       */
debugFmtFast(X):-writeq(X),nl.

logOnFailure(assert(X,Y)):- catch(assert(X,Y),_,Y=0),!.
logOnFailure(assert(X)):- catch(assert(X),_,true),!.
logOnFailure(assert(X)):- catch(assert(X),_,true),!.
%logOnFailure(X):-catch(X,E,true),!.
logOnFailure(X):-catch(X,E,(writeFailureLog(E,X),!,catch((true,X),_,fail))),!.
logOnFailure(X):- writeFailureLog('Predicate Failed',X),!.


flush_output_safe(X):-catch(flush_output(X),_,true),!.
flush_output_safe(_).

writeFailureLog(E,X):-
		writeFmt(user_error,'\n% error:  ~q ~q\n',[E,X]),flush_output_safe(user_error),!,
		%,true.
		writeFmt('\n;; error:  ~q ~q\n',[E,X]),!,flush_output. %,writeFmtFlushed([E,X]).
		
noDebug(CALL):-CALL.
	


%unknown(Old, autoload).




% ========================================================================================
% Some prologs have a printf() type predicate.. so I made up fmtString/writeFmt in the Cyc code that calls the per-prolog mechaism
% in SWI it''s formzat/N and sformat/N
% ========================================================================================
:-dynamic_transparent(isConsoleOverwritten/0).

/*
defined above
writeFmtFlushed(X,Y,Z):-catch((format(X,Y,Z),flush_output_safe(X)),_,true).
writeFmtFlushed(X,Y):-catch((format(X,Y),flush_output),_,true).
writeFmtFlushed(X):- once((atom(X) -> catch((format(X,[]),flush_output),_,true) ; writeFmtFlushed('~q~n',[X]))).
*/

writeFmt(X,Y,Z):-catch(format(X,Y,Z),_,true).
writeFmt(X,Y):-format(X,Y).
writeFmt(X):-format(X,[]).

fmtString(X,Y,Z):-sformat(X,Y,Z).
fmtString(Y,Z):-sformat(Y,Z).

saveUserInput:-retractall(isConsoleOverwritten),flush_output.
writeSavedPrompt:-not(isConsoleOverwritten),!.
writeSavedPrompt:-flush_output.
writeOverwritten:-isConsoleOverwritten,!.
writeOverwritten:-assert(isConsoleOverwritten).

writeErrMsg(Out,E):- message_to_string(E,S),writeFmtFlushed(Out,'<cycml:error>~s</cycml:error>\n',[S]),!.
writeErrMsg(Out,E,Goal):- message_to_string(E,S),writeFmtFlushed(Out,'<cycml:error>goal "~q" ~s</cycml:error>\n',[Goal,S]),!.
writeFileToStream(Dest,Filename):-
        catch((
        open(Filename,'r',Input),
        repeat,
                get_code(Input,Char),
                put(Dest,Char),
        at_end_of_stream(Input),
        close(Input)),E,
        writeFmtFlushed('<cycml:error goal="~q">~w</cycml:error>\n',[writeFileToStream(Dest,Filename),E])).















join_path(CurrentDir,Filename,Name):-
         atom_ensure_endswtih(CurrentDir,'/',Out),atom_ensure_endswtih('./',Right,Filename),
         atom_concat(Out,Right,Name),!.

atom_ensure_endswtih(A,E,A):-atom(E),atom_concat(_Left,E,A),!.
atom_ensure_endswtih(A,E,O):-atom(A),atom(E),atom_concat(A,E,O),!.
atom_ensure_endswtih(A,E,O):-atom(A),atom(O),atom_concat(A,E,O),!.
atom_ensure_endswtih(A,O,O):-atom(A),atom(O),!.

os_to_prolog_filename(OS,_PL):-prolog_must(atom(OS)),fail.
os_to_prolog_filename(_OS,PL):-prolog_must(var(PL)),fail.
os_to_prolog_filename(OS,PL):-exists_file_safe(OS),!,PL=OS.
os_to_prolog_filename(OS,PL):-exists_directory_safe(OS),!,PL=OS.
os_to_prolog_filename(OS,PL):-current_directory_search(CurrentDir),join_path(CurrentDir,OS,PL),exists_file_safe(PL),!.
os_to_prolog_filename(OS,PL):-current_directory_search(CurrentDir),join_path(CurrentDir,OS,PL),exists_directory_safe(PL),!.

os_to_prolog_filename(OS,PL):-atom(OS),atomic_list_concat([X,Y|Z],'\\',OS),atomic_list_concat([X,Y|Z],'/',OPS),!,os_to_prolog_filename(OPS,PL).
os_to_prolog_filename(OS,PL):-atom_concat_safe(BeforeSlash,'/',OS),os_to_prolog_filename(BeforeSlash,PL).
os_to_prolog_filename(OS,PL):-absolute_file_name(OS,OSP),OS \= OSP,!,os_to_prolog_filename(OSP,PL).


% =================================================================================
% Utils
% =================================================================================


:-dynamic(argNumsTracked/3).
:-dynamic(argNFound/3).
:-index(argNFound(1,1,1)).

makeArgIndexes(CateSig):-functor(CateSig,F,_),makeArgIndexes(CateSig,F),!.
makeArgIndexes(CateSig,F):- argNumsTracked(F,Atom,Number),arg(Number,CateSig,Arg),user:nonvar(Arg),
         %%Number<10,user:nonvar(Arg),atom_number(Atom,Number),
         assert_if_new(argNFound(F,Atom,Arg)),fail.
makeArgIndexes(_NEW,_F).


assert_if_new(N):-N,!.
assert_if_new(N):-assert(N),!.


% =================================================================================
% Utils
% =================================================================================
test_call(G):-writeln(G),ignore(once(catch(G,E,writeln(E)))).

nop(_).

debugFmtList(ListI):-notrace((copy_term(ListI,List),debugFmtList0(List,List0),randomVars(List0),debugFmt(List0))),!.
debugFmtList0([],[]):-!.
debugFmtList0([A|ListA],[B|ListB]):-debugFmtList1(A,B),!,debugFmtList0(ListA,ListB),!.

debugFmtList1(Value,Value):-var(Value),!.
debugFmtList1(Name=Number,Name=Number):-number(Number).
debugFmtList1(Name=Value,Name=Value):-var(Value),!.
debugFmtList1(Name=Value,Name=(len:Len)):-copy_term(Value,ValueO),append(ValueO,[],ValueO),is_list(ValueO),length(ValueO,Len),!.
debugFmtList1(Name=Value,Name=(F:A)):-functor(Value,F,A).
debugFmtList1(Value,shown(Value)).

% ===============================================================================================
% unlistify / listify
% ===============================================================================================

unlistify([L],O):-user:nonvar(L),unlistify(L,O),!.
unlistify(L,L).

listify(OUT,OUT):-not(not(is_list(OUT))),!.
listify(OUT,[OUT]).





traceIf(_Call):-!.
traceIf(Call):-ignore((Call,trace)).


% When you trust the code enough you dont to debug it
%  but if that code does something wrong while your not debugging, you want to see the error
hotrace(X):- tracing -> notrace_call(X) ; call(X).

notrace_call(X):-notrace,catch(traceafter_call(X),E,(debugFmt(E-X),trace,throw(E))).
traceafter_call(X):-X,trace.
traceafter_call(_):-tracing,fail.
traceafter_call(_):-trace,fail.


debugFmtS([]):-!.
debugFmtS([A|L]):-!,debugFmt('% ~q~n',[[A|L]]).
debugFmtS(Comp):-ctxHideIfNeeded(_,Comp,Comp2),!,debugFmt('% ~q~n',[Comp2]).
debugFmtS(Stuff):-!,debugFmt('% ~q~n',[Stuff]).


%getWordTokens(WORDS,TOKENS):-concat_atom(TOKENS,' ',WORDS).
%is_string(S):-string(S).

%================================================================
% decends tree
%================================================================

map_tree_to_list(_,PATTERN,Output):- (var(PATTERN);number(PATTERN)),!,must_assign([PATTERN],Output).
map_tree_to_list(_,[],OUT):-!,must_assign([],OUT).
map_tree_to_list(Pred,IN,Output):- once(call(Pred,IN,MID)),prolog_must((MID=IN -> flatten([MID],OUT) ; map_tree_to_list(Pred,MID,OUT))),!,must_assign(OUT=Output).
map_tree_to_list(Pred,[I|IN],Output):-!,debugOnFailureEach((map_tree_to_list(Pred,I,O1),map_tree_to_list(Pred,IN,O2),!,append(O1,O2,OUT))),!,must_assign(OUT=Output).
map_tree_to_list(Pred,IN,Output):-atom(IN),prolog_must((atomSplit(IN,MID),!,map_tree_to_list(Pred,MID,OUT))),!,must_assign(OUT=Output).
map_tree_to_list(Pred,IN,Output):-
  prolog_must((compound(IN), IN=..INP, append(Left,[Last],INP), map_tree_to_list(Pred,Last,UT),!, 
   append(Left,[UT],OUTP),!, OUT =.. OUTP)),must_assign([OUT],Output).
map_tree_to_list(_,IN,IN):-trace,must_assign([IN],IN).

