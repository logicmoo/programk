% ===================================================================
% File 'logicmoo_module_aiml_loader.pl'
% Purpose: An Implementation in SWI-Prolog of AIML
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'logicmoo_module_aiml.pl' 1.0.0
% Revision:  $Revision: 1.7 $
% Revised At:   $Date: 2002/07/11 21:57:28 $
% ===================================================================
:-dynamic(lineInfoElement/4).


hideIfNeeded(I,I):- (var(I);atomic(I)),!.
hideIfNeeded([I|_],ctx):-nonvar(I),I=frame(_,_,_),!.
hideIfNeeded([I|N],[I0|N0]):-!,hideIfNeeded(I,I0),hideIfNeeded(N,N0),!.
hideIfNeeded(Comp,Comp2):-compound(Comp),Comp=..[L,I|ST],hideIfNeeded([I|ST],[OI|OIST]),Comp2=..[L,OI|OIST],!.
hideIfNeeded(I,I):-!.

exists_file_safe(File):-prolog_must(atomic(File)),exists_file(File).
exists_directory_safe(File):-prolog_must(atomic(File)),exists_directory(File).
%:-module()
%:-include('logicmoo_utils_header.pl'). %<?
%:- style_check(-singleton).
%%:- style_check(-discontiguous).
:- style_check(-atom).
:- style_check(-string).

throw_safe(Exc):-trace,throw(Exc).

:-op(1150,fx,meta_predicate_transparent).

must_assign(From,To):-To=From,!.
must_assign(From,To):-trace,To=From.

:-'$hide'(prolog_must/1).
prolog_must(Call):-tracing,!,Call. %%prolog_must_tracing(Call).
prolog_must(Call):-prolog_must_call(Call).


:-'$hide'(prolog_must_call/1).
prolog_must_call(Call):- prolog_ecall(prolog_must_call0,Call).   
prolog_must_call0(Call):- atLeastOne((Call),(trace,Call)).


:-'$hide'(prolog_must_tracing/1).
prolog_must_tracing(Call):- prolog_ecall(prolog_must_tracing0,Call).   
prolog_must_tracing0(Call):-trace(Call,[-all,+fail]), atLeastOne(Call,hotrace(aiml_error(Call))).


:-'$hide'(prolog_ecall/2).
prolog_ecall(_Pred,Call):-var(Call),!,trace,randomVars(Call).
prolog_ecall(Pred,(X->Y;Z)):-!,(call(X) -> prolog_ecall(Pred,Y) ; prolog_ecall(Pred,Z)).
prolog_ecall(Pred,(X->Y)):-!,(call(X)->prolog_ecall(Pred,Y)).
prolog_ecall(Pred,(X;Y)):-!,prolog_ecall(Pred,X);prolog_ecall(Pred,Y).
prolog_ecall(Pred,(X,Y)):-!,prolog_ecall(Pred,X),prolog_ecall(Pred,Y).
prolog_ecall(Pred,prolog_must(Call)):-!,prolog_ecall(Pred,Call).
prolog_ecall(Pred,Call):- fail, ignore((Call=atom(_),trace)), 
    predicate_property(Call,number_of_clauses(_Count)),
    clause(Call,(_A,_B)),!,catch(clause(Call,Body),_,
      (trace,predicate_property(Call,number_of_clauses(_Count2)),
      clause(Call,Body))),prolog_ecall(Pred,Body).

prolog_ecall(Pred,Call):- call(Pred,Call).


%%:-'$hide'(atLeastOne/1).
%%:-'$hide'(atLeastOne/2).
%%:-'$hide'(atLeastOne0/2).

atLeastOne(OneA):- atLeastOne(OneA,(trace,OneA)).
atLeastOne(OneA,Else):-atLeastOne0(OneA,Else).

atLeastOne0(OneA,_Else):-copy_term(OneA,One),findall(One,call(One),OneL),[_|_]=OneL,!,member(OneA,OneL).
atLeastOne0(OneA,Else):-debugFmt(failed(OneA)),!,Else,!,fail.


randomVars(Term):- random(R), Start is round('*'(R,1000000)), !,
  numbervars(Term, Start, _End, [attvar(skip),functor_name('$VAR')]).

prolog_must_not(Call):-Call,!,trace,!,aiml_error(prolog_must_not(Call)).
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
   debugOnFailureAimlEach((   
   arg(1,X,A),functor(X,F,_),
   FA=F/A,
   dynamic_if_missing(FA),
   %module_transparent(FA),
   %%meta_predicate(X),
   %trace(FA, -all),
   %%'$hide'(FA),
   !)).

   asserta_new(_Ctx,NEW):-ignore(retract(NEW)),asserta(NEW).
writeqnl(_Ctx,NEW):- format('~q.~n',[NEW]),!.


revappend([], Ys, Ys).
revappend([X|Xs], Ys, Zs) :- revappend(Xs, [X|Ys], Zs).

reverseA(Xs,Ys) :- revappend(Xs,[],Ys).

appendAttributes(_Ctx,L,R,AA):-hotrace((mergeAppend0(L,R,A),list_to_set_safe(A,AA))),!.
mergeAppend0(L,R,R):-var(L),!,var(R),!.
mergeAppend0(L,R,A):-var(R),append(L,R,A),!.
mergeAppend0(L,R,A):-var(L),append(L,R,A),!.
mergeAppend0(L,[R|RR],A):-eqmember(R,L),mergeAppend0(L,RR,A).
mergeAppend0([L|LL],R,A):-eqmember(L,R),mergeAppend0(LL,R,A).
mergeAppend0(L,R,A):-append(L,R,A).

eqmember(E,List):-copy_term_numvars(E:List,E0:List0),member(E0,List0).

list_to_set_safe(A,A):-(var(A);atomic(A)),!.
list_to_set_safe([A|AA],BB):- (not(not(lastMember(A,AA))) -> list_to_set_safe(AA,BB) ; (list_to_set_safe(AA,NB),BB=[A|NB])),!.


lastMember(_E,List):-var(List),!,fail.
lastMember(E,[H|List]):-lastMember(E,List);E=H.

lastMember(E,List,Rest):-lastMember(E,List),!,delete_safe(List,E,Rest),!.

delete_safe(List,_E,Rest):-var(List),!,Rest=List.
delete_safe(List,E,Rest):-is_list(List),!,delete(List,E,Rest).
delete_safe([H|List],E,Rest):- H==E,!,delete_safe(List,E,Rest).
delete_safe([H|List],E,[H|Rest]):-delete_safe(List,E,Rest).


getKeyValue(FullList,N=V):-lastMember(N=V,FullList),!.
%%addKeyValue(FullList,N=V):-nonvar(N),!,append(_Closed,[N=V|_],FullList),!.
addKeyValue(FullList,NV):- prolog_must((not(ground(FullList)),nonvar(NV))),append(_Closed,[NV|_],FullList),!.


lastMember2(E,List):-to_open_list(_,Closed,_Open,List),reverse(Closed,Rev),member(E,Rev).

%lastMember(End,List) :- append(_,[End|_],List).


to_open_list(FullList,Closed,Open,FullList) :- append(Closed,Open,FullList),var(Open),!.
to_open_list(Closed,Closed,Open,FullList) :- append(Closed,Open,FullList),!.




copy_term_numvars(OLD,NEW):-copy_term(OLD,NEW),numbervars(NEW,0,_).

%%%retractall(E):- retractall(E),functor(E,File,A),dynamic(File/A),!.

pp_listing(_Pred):-!. %%functor(Pred,File,A),functor(FA,File,A),listing(File),nl,findall(NV,predicate_property(FA,NV),LIST),writeq(LIST),nl,!.

% =================================================================================
% Utils
% =================================================================================

printPredCount(Msg,Pred,N1):- compound(Pred), debugOnFailureAimlEach((arg(_,Pred,NG))),nonvar(NG),!,
   findall(Pred,Pred,LEFTOVERS),length(LEFTOVERS,N1),debugFmt(num_clauses(Msg,Pred,N1)),!.

printPredCount(Msg,Pred,N1):-!,functor(Pred,File,A),functor(FA,File,A), predicate_property(FA,number_of_clauses(N1)),debugFmt(num_clauses(Msg,File/A,N1)),!.

% =================================================================================
% Loader Utils
% =================================================================================

dynamic_load(Key,PLNAME):- creating_aiml_file(Key,PLNAME),throw_safe(creating_aiml_file(Key,PLNAME)),assert(pending_aiml_file(Key,PLNAME)).
dynamic_load(Key,PLNAME):- not(ground(dynamic_load(Key,PLNAME))),throw_safe(not(ground(dynamic_load(Key,PLNAME)))).
dynamic_load(Key,PLNAME):- loaded_aiml_file(Key,PLNAME,Time),!,debugFmt(loaded_aiml_file(Key,PLNAME,Time)).
dynamic_load(Key,_PLNAME):- nonvar(Key), once(cateForFile(_,Key,Cate)),Cate,!.

dynamic_load(Key,PLNAME):- 
   global_pathname(PLNAME,PN),
   exists_file_safe(PLNAME),
   time_file_safe(PN,Time),
   assert(loaded_aiml_file(Key,PLNAME,Time)),
   dynamic_load2(Key,PLNAME).

dynamic_load2(_Key,PLNAME):-consult(PLNAME),!.

dynamic_load2(_Key,PLNAME):- %% unload_file(PLNAME),
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

load_dirrective(include(PLNAME),_Options):-  (atom_concat_safe(Key,'.pl',PLNAME);Key=PLNAME),!, dynamic_load(Key,PLNAME).
load_dirrective(module(M,Preds),_Options):-!,module(M),call(module(M,Preds)).
load_dirrective(Term,_Options):-!,Term.

showProfilerStatistics(FileMatch):-
   statistics(global,Mem), MU is (Mem / 1024 / 1024),
   printPredCount('showProfilerStatistics: '(MU),FileMatch,_N1).

aimlPredCount:-printAll(aimlPredCount(_,_,0)),printAll((aimlPredCount(Pred,File,Count),Count>0),aimlPredCount(Pred,File,Count)).
aimlPredCount(Pred,File,Count):-source_file(File),atom_contains(File,'aiml'),source_file(Pred,File),functor(Pred,F,A),current_predicate(F/A),
    predicate_property(Pred,dynamic),predicate_property(Pred,number_of_clauses(Count)).

% =================================================================================
% Utils
% =================================================================================

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
printAll(Call,Print):-forall(Call,(format('~q.~n',[Print]))).

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


% =================================================================================
% Utils
% =================================================================================

% ===============================================================================================
% UTILS
% ===============================================================================================

callInteractive(Term,Var):-catch(callInteractive0(Term,Var),E,aiml_error(E)),!.

%callInteractive0(Term,_):-atom(Term),!,Term,!,writeln(called(Term)),!.
callInteractive0(Term,Var):- call(Term),writeq(Term:Var),nl,fail.
callInteractive0(_,_):-!.

currentContext(Name,X):-makeAimlContext(Name,X),!.


%getWordTokens(WORDS,TOKENS):-concat_atom(TOKENS,' ',WORDS).
%is_string(S):-string(S).


sentenceEnder(Last):-member(Last,[?,('.'),('\n')]).
sentenceEnderOrPunct(Last):-member(Last,[?,('.'),(','),('\n')]).

removePMark(UCase,Atoms):-append(AtomsPre,[Last],UCase),sentenceEnderOrPunct(Last),!,removePMark(AtomsPre,Atoms).
removePMark(Atoms,Atoms).

leftTrim([B|Before],ToRemove,After):-call(ToRemove,B),!,leftTrim(Before,ToRemove,After).
leftTrim([B|Before],ToRemove,[B|After]):-leftTrim(Before,ToRemove,After).
leftTrim([],_ToRemove,[]):-!.


randomPick(List,Ele):-length(List,Len),Pick is random(Len),nth0(Pick,List,Ele),!.

%================================================================
% Atom / String functions
%================================================================
atomsSameCI(Name1,Name1):-!.
atomsSameCI(Name1,Name2):-atom(Name1),atom(Name2),downcase_atom(Name1,D1),downcase_atom(Name2,D2),!,D1=D2.

clean_codes(X,Y):-trim(X,Y),!.  % actually cyc:trim/2
clean_codes(X,X).

%clean_out_atom(X,Y):-atomSplit(X,C),delete(C,'',O),concat_atom_safe(C,' ',Y).
clean_out_atom(X,Y):-atom_codes(X,C),clean_codes(C,D),!,atom_codes(X,D),!,Y=X.

atomSplit(A,B):- notrace((cyc:atomSplit(A,BB),!,BB=B)).

%%atomSplit(A,B):-token_stream_of(A,AA),findall(B0,arg(1,AA,B),B).

all_upper_atom(X):-toUppercase(X,N),!,N=X.

atom_concat_safe(L,R,A):- ((atom(A),(atom(L);atom(R))) ; ((atom(L),atom(R)))), !, atom_concat(L,R,A),!.

concat_atom_safe(List,Sep,[Atom]):-atom(Atom),!,concat_atom(List,Sep,Atom),!.
concat_atom_safe(List,Sep,Atom):-atom(Atom),!,concat_atom(ListM,Sep,Atom),!,List = ListM.
concat_atom_safe(List,Sep,Atom):- concat_atom(List,Sep,Atom),!.

upcase_atom_safe(A,B):-atom(A),upcase_atom(A,B),!.

atom_contains(F,C):- hotrace((atom(F),atom(C),sub_atom(F,_,_,_,C))).

toCodes(B,A):-cyc:stringToCodelist(B,AO),(is_list(A) -> A=AO ; string_to_list(AO,A)),!.

% convert any term to 'atom' string
convert_to_string(I,ISO):-
                term_to_string(I,IS),!,
		string_to_list(IS,LIST),!,
		list_replace(LIST,92,[92,92],LISTM),
		list_replace(LISTM,34,[92,34],LISTO),!,
		string_to_atom_safe(ISO,LISTO),!.

string_to_atom_safe(ISO,LISTO):-LISTO==[],!,string_to_atom(ISO,'').
string_to_atom_safe(ISO,LISTO):-string_to_atom(ISO,LISTO).

list_replace(List,Char,Replace,NewList):-
	append(Left,[Char|Right],List),
	append(Left,Replace,NewLeft),
	list_replace(Right,Char,Replace,NewRight),
	append(NewLeft,NewRight,NewList),!.
list_replace(List,_Char,_Replace,List):-!.

term_to_string(I,IS):- catch(string_to_atom(IS,I),_,fail),!.
term_to_string(I,IS):- term_to_atom(I,A),string_to_atom(IS,A),!.

%================================================================
% maplist/[2,3]
% this must succeed  maplist_safe(=,[X,X,X],[1,2,3]).
% well if its not "maplist" what shall we call it?
%================================================================
% so far only the findall version works .. the other runs out of local stack!?

maplist_safe(_Pred,[]):-!.
maplist_safe(Pred,LIST):-findall(E,(member(E,LIST),debugOnFailureAiml(apply(Pred,[E]))),LISTO), debugOnFailureAiml(LIST=LISTO),!.
%% though this should been fine %%  maplist_safe(Pred,[A|B]):- copy_term(Pred+A, Pred0+A0), debugOnFailureAiml(once(call(Pred0,A0))),     maplist_safe(Pred,B),!.

maplist_safe(_Pred,[],[]):-!.
maplist_safe(Pred,LISTIN, LIST):-!, findall(EE, ((member(E,LISTIN),debugOnFailureAiml(apply(Pred,[E,EE])))), LISTO),  debugOnFailureAiml(LIST=LISTO),!.
%% though this should been fine %% maplist_safe(Pred,[A|B],OUT):- copy_term(Pred+A, Pred0+A0), debugOnFailureAiml(once(call(Pred0,A0,AA))),  maplist_safe(Pred,B,BB), !, OUT=[AA|BB].

%================================================================
% decends tree
%================================================================

map_tree_to_list(_,PATTERN,Output):- (var(PATTERN);number(PATTERN)),!,must_assign([PATTERN],Output).
map_tree_to_list(_,[],OUT):-!,must_assign([],OUT).
map_tree_to_list(Pred,IN,Output):- once(call(Pred,IN,MID)),debugOnFailureAiml((MID=IN -> flatten([MID],OUT) ; map_tree_to_list(Pred,MID,OUT))),!,must_assign(OUT,Output).
map_tree_to_list(Pred,[I|IN],Output):-!,debugOnFailureAiml((map_tree_to_list(Pred,I,O1),map_tree_to_list(Pred,IN,O2),!,append(O1,O2,OUT))),!,must_assign(OUT,Output).
map_tree_to_list(Pred,IN,Output):-atom(IN),debugOnFailureAiml((atomSplit(IN,MID),!,map_tree_to_list(Pred,MID,OUT))),!,must_assign(OUT,Output).
map_tree_to_list(Pred,IN,Output):-
  debugOnFailureAiml((compound(IN), IN=..INP, append(Left,[Last],INP), map_tree_to_list(Pred,Last,UT),!, 
   append(Left,[UT],OUTP),!, OUT =.. OUTP)),must_assign([OUT],Output).
map_tree_to_list(_,IN,IN):-trace,must_assign([IN],IN).


dumpList(B):-currentContext(dumpList,Ctx),dumpList(Ctx,B).
dumpList(_,AB):-debugFmt(dumpList(AB)),!.

dumpList(_,[]):-!.
%dumpList(Ctx,[A|B]):-!,say(Ctx,A),dumpList(Ctx,B),!.
%dumpList(Ctx,B):-say(Ctx,dumpList(B)).


ifThen(When,Do):-When->Do;true.

traceCall(A):-trace(A,[-all,+fail]),A,!.

debugOnFailureAimlEach((A,B)):- !,debugOnFailureAimlEach(A),debugOnFailureAimlEach(B).
debugOnFailureAimlEach(Call):- once(prolog_must(Call)).


debugOnFailureAiml(Call):-prolog_ecall(debugOnFailureAiml0,Call).

%%%%%%%%%%%%%5%%debugOnFailureAiml(Call):- clause(Call,(_A,_B)),!,clause(Call,Body),trace,debugOnFailureAiml(Body),!.
debugOnFailureAiml0(Call):-  Call,!.
%%%%%%%%%%%%%%debugOnFailureAiml(Call):- debugOnFailureAimlTrace(Call),!.
debugOnFailureAiml0(Call):- beenCaught(Call),!.

debugOnFailureAimlTrace(debugOnFailureAiml(Call)):-!,debugOnFailureAimlTrace(Call),!.
debugOnFailureAimlTrace((A,B)):- !,debugOnFailureAimlTrace(A),!,debugOnFailureAimlTrace(B),!.
debugOnFailureAimlTrace(Call):- debugOnFailureAimlTrace1(Call),debugFmt(success(Call)),!.
debugOnFailureAimlTrace(Call):- debugFmt(faild(Call)),!,trace,Call.


debugOnFailureAimlTrace1((A,B)):- !,debugOnFailureAimlTrace1(A),!,debugOnFailureAimlTrace1(B),!.
debugOnFailureAimlTrace1(Call):- functor(Call,F,A),member(F/A,[retract/_,retractall/_]),!,debugFmt(fakingCall(Call)),numbervars(Call,0,_),!.
debugOnFailureAimlTrace1(Call):- Call,!.

beenCaught(debugOnFailureAiml(Call)):- !, beenCaught(Call).
beenCaught((A,B)):- !,beenCaught(A),beenCaught(B).
beenCaught(Call):- fail, predicate_property(Call,number_of_clauses(_Count)), clause(Call,(_A,_B)),!,clause(Call,Body),beenCaught(Body).
beenCaught(Call):- catch(once(Call),E,(debugFmt(caugth(Call,E)),beenCaught(Call))),!.
beenCaught(Call):- traceAll,debugFmt(tracing(Call)),debug,trace,Call.


takeout(_,[],[]):-!.
takeout(X,[Y|R],RR):-not(not(X=Y)),!,takeout(X,R,RR),!.
takeout(X,[F|R],[F|S]) :- takeout(X,R,S),!.
takeout(_,X,X).

local_predicate(_,_/0):-!,fail.
local_predicate(_,_/N):-N>7,!,fail.
local_predicate(P,_):-predicate_property(P,built_in),!,fail.
local_predicate(P,_):-predicate_property(P,imported_from(_)),!,fail.
local_predicate(P,_):-predicate_property(P,file(F)),!,atom_contains(F,'aiml_'),!.
local_predicate(P,F/N):-functor(P,F,N),!,fail.


time_file_safe(F,INNER_XML):-exists_file_safe(F),time_file(F,INNER_XML).


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


lengthAtLeast(N,GE):-atom(N),atom_length(N,L),L>=GE.
/*
neverUse:- meta_predicate_transparent
	maplist_safe(2,:),
	maplist_safe(3,:,:),
        asserta_new(2,:),
        writeqnl(2,:),
        debugOnFailureAimlTrace(1),
        debugOnFailureAiml(1),
        beenCaught(1),
        debugOnFailureAimlEach(1),
        prolog_must(1),ignore(1), %%withAttributes(3,:,:),call_cleanup(0,0),call_cleanup(0,?,0),
        !.
*/



