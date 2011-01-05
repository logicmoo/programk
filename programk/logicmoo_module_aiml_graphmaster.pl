% ===================================================================
% File 'logicmoo_module_aiml_loader.pl'
% Purpose: An Implementation in SWI-Prolog of AIML
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'logicmoo_module_aiml_loader.pl' 1.0.0
% Revision:  $Revision: 1.7 $
% Revised At:   $Date: 2002/07/11 21:57:28 $
% ===================================================================


%:-module()
%:-include('logicmoo_utils_header.pl'). %<?
%:- style_check(-singleton).
%%:- style_check(-discontiguous).
:- style_check(-atom).
:- style_check(-string).

:-ensure_loaded(library('programk/logicmoo_module_aiml_graphmaster.pl')).
:-ensure_loaded(library('programk/logicmoo_module_aiml_convertor.pl')).
:-ensure_loaded(library('programk/logicmoo_module_aiml_xpath.pl')).
:-discontiguous(load_dict_structure/2).

innerTagPriority(graph,[topic,prepattern]).
innerTagPriority(precall,[that,prepattern]).
innerTagPriority(topic,[topic,prepattern]).
innerTagPriority(that,[that,prepattern]).
innerTagPriority(request,[that,prepattern]).
innerTagPriority(response,[that,prepattern]).
innerTagPriority(pattern,[pattern]).
innerTagPriority(flags,[pattern,prepattern]).
innerTagPriority(call,[that,postpattern]).
innerTagPriority(guard,[that,postpattern]).
innerTagPriority(userdict,[template,postpattern]).
innerTagPriority(template,[template,postpattern]).


% ===============================================================================================
% ===============================================================================================
%%:-abolish(dict/3).

:-retractall(dict(_,_,_)).

:-pp_listing(dict(_,_,_)).


useNewCateSigSearch:-false.
useIndexPatternsForCateSearch:-true.

% ===================================================================
%  aimlCate database decl
% ===================================================================

:-dynamic(aimlCateSigCached/1).
aimlCateSig(X):-aimlCateSigCached(X),!.
aimlCateSig(X):-aimlCateOrder(List),length(List,L),functor(Pred,aimlCate,L),asserta(aimlCateSigCached(Pred)),!,copy_term(Pred,X).

aimlCateOrder([graph,precall,topic,that,request,pattern,flags,call,guard,userdict,template,srcinfo,srcfile]).

% [graph,precall,topic,that,pattern,flags,call,guard,template,userdict]
cateMemberTags(Result):- aimlCateOrder(List), findall(E,(member(E0,List),once((E0=[E|_];E0=E))), Result).

makeAimlCateSig(Ctx,ListOfValues,Pred):-aimlCateSig(Pred),!,makeAimlCate(Ctx,ListOfValues,Pred),!.

:- aimlCateOrder(List),length(List,L),dynamic(aimlCate/L),multifile(aimlCate/L). 

replaceArgsVar(_Ctx,[],_CateSig):-!.
replaceArgsVar(Ctx,[E=Replacement|L],CateSig):-
    getCategoryArg1(Ctx,E,_Old,StarNumber,CateSig),
    nb_setarg(StarNumber,CateSig,Replacement),
    replaceArgsVar(Ctx,L,CateSig),!.

% ===============================================================================================
%  Indexing of Categories
% ===============================================================================================

:-dynamic(argNumsTracked/3).
:-dynamic(argNFound/4).
:-index(argNFound(1,1,1,1)).


%% aimlCateOrder([graph,precall,topic,that,request,pattern,flags,call,guard,userdict,template,srcinfo,srcfile]).

argTypeIndexable(textInput).
%%argTypeIndexable(name).

argNumsIndexedRepr(aimlCate,topic,3,textInput).
argNumsIndexedRepr(aimlCate,that,4,textInput).
argNumsIndexedRepr(aimlCate,pattern,6,textInput).

argNumsIndexedRepr(aimlCate,graph,1,name).
argNumsIndexedRepr(aimlCate,precall,2,callable).
argNumsIndexedRepr(aimlCate,request,5,flags).
argNumsIndexedRepr(aimlCate,flags,7,flags).
argNumsIndexedRepr(aimlCate,call,8,callable).
argNumsIndexedRepr(aimlCate,guard,9,callable).
argNumsIndexedRepr(aimlCate,userdict,9,name).
argNumsIndexedRepr(aimlCate,template,10,textOutput).
argNumsIndexedRepr(aimlCate,srcinfo,11,any).
argNumsIndexedRepr(aimlCate,srcfile,12,any).


%%graph,precall,topic,that,request,pattern,flags,call,guard,userdict,template,srcinfo,srcfile

argNumsTracked(Pred,ArgName,Position):-argNumsIndexedRepr(Pred,ArgName,Position,ArgType),argTypeIndexable(ArgType).

argNFound(F,A,'_','_'):-argNumsIndexedRepr(F,A,_,textInput).
argNFound(F,A,*,*):-argNumsIndexedRepr(F,A,_,textInput).


assert_cate_in_load(NEW) :- currentContext(assert_cate_in_load,Ctx),prolog_must(assert_cate_in_load(Ctx,NEW)),!.

assert_cate_in_load(Ctx,CateSig):-
    duplicate_term(CateSig,CateSigTest),
    load_category(Ctx,CateSigTest),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% load_category(Ctx,CateSig) 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

load_category(Ctx,CateSig):-
      isRetraction(Ctx,CateSig,RemovemeMask),!,
      withArgIndexing(RemovemeMask,dirtyArgIndex,Removeme),
      findall(Removeme,retract_cate_post_index(Removeme),_Retracted),!.

load_category(Ctx,CateSig):-
      withArgIndexing(CateSig,addArgIndex,Indexable),
      asserta(Indexable),!,
      traceIf(((not(not(Indexable==CateSig))),not(arg(6,CateSig,*)))),!,
      immediateCall(Ctx,assert_cate_post_index(Indexable)),!,
      confirm_args_indexed(Indexable).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% assert_cate_post_index(Indexable)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
assert_cate_post_index(Indexable):- asserta(Indexable),debugFmt(asserta(Indexable)),confirm_args_indexed(Indexable).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% retract_cate_post_index(Indexable)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
retract_cate_post_index(Removeme):-
   immediateCall(_Ctx,retract_cate_post_index(Removeme)),!,
   %%withArgIndexing(Retract,dirtyArgIndex,Removeme),
   %%debugFmt(retract_cate_post_index(Removeme)),!,
   prolog_must(retract(Removeme)),!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% confirm_args_indexed(Indexable)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
confirm_args_indexed(Indexable):-functor(Indexable,F,_),
      argNumsIndexedRepr(F,ArgName,N,ArgType),argTypeIndexable(ArgType),
      arg(N,Indexable,Value),
      not(argNFound(F,ArgName,_,Value)),
      debugFmt(not(argNFound(F,ArgName,_,Value))),fail.
confirm_args_indexed(_Indexable). %%confirmed

%%%
ffffffff.
noTrickyIndexing:-false.

toNonIndexable(FAKE,FAKE):-noTrickyIndexing,!.
toNonIndexable(OF,INDEXABLE):-OF=..[F|ARGS],functor(OF,F,A),toNonIndexable0(A,F,ARGS,NEWARGS),!,INDEXABLE=..[F|NEWARGS].
toNonIndexable0(0,_F,_,[]):-!.
toNonIndexable0(3,aimlCate,List,List):-!.
toNonIndexable0(N,F,[A|ARGS],[NEW|NEWARGS]):-N2 is N-1, toNonIndexableArg(A,NEW),toNonIndexable0(N2,F,ARGS,NEWARGS).

toNonIndexableArg(A,A):-var(A),!.

toNonIndexableArg(A,A):-member(A,['*','[]','_']),!.
toNonIndexableArg([A|H],[A|H]):-!.
toNonIndexableArg(A,A):-not(compound(A)),!.
toNonIndexableArg(A,[A]):-not(compound(A)),!.
toNonIndexableArg(A,[A0]):-A=..[A0/*,idx0*/].
toNonIndexableArg(A,[A0|AN]):-A=..[A0,idx|AN].
toNonIndexableArg(A,[AA|AL]):-A=..[A0,idxl,AN|AL],AA=..[A0|AN].
toNonIndexableArg(A,[A]).

%%toIndexable(FAKE,FAKE):-!.
toIndexable(OF,INDEXABLE):-OF=..[F|ARGS],functor(OF,F,A),toIndexable0(A,F,ARGS,NEWARGS),!,INDEXABLE=..[F|NEWARGS].
toIndexable0(0,_F,_,[]):-!.
toIndexable0(3,aimlCate,List,List):-!.
toIndexable0(N,F,[A|ARGS],[NEW|NEWARGS]):-N2 is N-1, makeIndexableArg(F,N,A,NEW),!,toIndexable0(N2,F,ARGS,NEWARGS).


makeIndexableArg(_,_,A,A):-noTrickyIndexing,!.  %%TODO: REMOVE THIS DISABLER
makeIndexableArg(F,ArgNumber,A,AHL):-argNumsIndexedRepr(F,_Pattern,ArgNumber,ArgType),makeIndexableArg(F,ArgType,A,AHL).
makeIndexableArg(F,ArgType,A,AHL):-argNumsIndexedRepr(F,Pattern,_,ArgType),makeIndexableArg(Pattern,ArgType,A,AHL).
makeIndexableArg(Pattern,ArgType,A,AH):-argNumsIndexedRepr(_F,Pattern,_ArgNumber,ArgType),argTypeIndexable(ArgType),toIndexableArg(Pattern,ArgType,A,AH).
makeIndexableArg(_,_,A,A).

toIndexableArg(_,_,B,AHL):-toIndexableArg(B,A),!,!,prolog_must(toLowercase(A,AHL)).

toIndexableArg(A,A):- noTrickyIndexing,!.  %%TODO: REMOVE THIS DISABLER
toIndexableArg(A,A):-var(A),!.
toIndexableArg(A,AH):-is_list(A),removeSkippables(A,AL),A\==AL,!,toIndexableArg(AL,AH).
toIndexableArg(A,A):-member(A,['*','[]','_']),!.
toIndexableArg([A],AA):-not(compound(A)),!,toIndexableArg(A,AA).
toIndexableArg(A,A):-not(compound(A)),!.
toIndexableArg([A],AH):- atom(A),!,AH=..[A/*,idx0*/],!.
toIndexableArg([A],N):-toIndexableArg(A,N).
toIndexableArg([A|H],AH):- atom(A),AH=..[A,idx|H],!.
toIndexableArg([A|H],AH):- A=..[A0|AN],predify(A,AH,A0,AN,H),!.
toIndexableArg(A,A).

predify(_A,AH,A0,AN,H):-predify(AH,A0,AN,H).
predify(A,[A|H],_A0,_AN,H).

predify(AH,A0,[],H):-AH=..[A0,idx|H].
predify(AH,A0,H,[]):-AH=..[A0,idx|H].
predify(AH,A0,AN,H):-AH=..[A0,idxl,AN|H].

%%%%%%%%%%%%%%%%%%%
%%withArgIndexing(+CateSig,+DoWhat,-Indexable):-!.
%%%%%%%%%%%%%%%%%%5
%%withArgIndexing(CateSig,DoWhat):-prolog_must(withArgIndexing(CateSig,DoWhat,_Indexable)).

withArgIndexing(CateSig,_DoWhat,Indexable):-not(useIndexPatternsForCateSearch),!,duplicate_term(CateSig,Indexable).
withArgIndexing(CateSig,DoWhat,Indexable):-
  functor(CateSig,F,A),  
  prolog_must(var(Indexable)),
  functor(Indexable,F,A),
  duplicate_term(CateSig,Indexable),
  prolog_must(withArgIndexing4(CateSig,F,DoWhat,Indexable)),!.

withArgIndexing4(CateSig,Functor,DoWhat,Indexable):- argNumsTracked(Functor,ArgName,ArgNumber), 
  argNumsIndexedRepr(Functor,ArgName,ArgNumber,ArgType),
  once((arg(ArgNumber,CateSig,Arg),
         once((call(DoWhat,CateSig,Indexable,Functor,ArgName,ArgNumber,Arg,IndexableArg,ArgType),
              nb_setarg(ArgNumber,Indexable,IndexableArg))))),fail.

withArgIndexing4(_CateSig,_F,_DoWhat,_Indexable).

staredArgIndex(_CateSig,_Indexable,_Functor,_ArgName,_ArgNumber,[IndexableArg],IndexableArg,ArgType):-argTypeIndexable(ArgType),isStar0(IndexableArg),!.
staredArgIndex(_CateSig,_Indexable,_Functor,_ArgName,_ArgNumber,IndexableArg,IndexableArg,ArgType):-argTypeIndexable(ArgType),isStar0(IndexableArg),!.

addArgIndex(CateSig,Indexable,Functor,ArgName,ArgNumber,Arg,IndexableArg,ArgType):-staredArgIndex(CateSig,Indexable,Functor,ArgName,ArgNumber,Arg,IndexableArg,ArgType),!.

addArgIndex(_CateSig,_Indexable,Functor,ArgName,ArgNumber,Arg,IndexableArg,ArgType):-argTypeIndexable(ArgType),
  makeIndexableArg(Functor,ArgNumber,Arg,IndexableArg),  
  asserta_if_new(argNFound(Functor,ArgName,Arg,IndexableArg)),!.
addArgIndex(CateSig,Indexable,Functor,ArgName,ArgNumber,Arg,IndexableArg,ArgType):- ctrace,
  debugFmt(addArgIndex(CateSig,Indexable,Functor,ArgName,ArgNumber,Arg,IndexableArg,ArgType)),
  prolog_must(Arg=IndexableArg),!.
  

dirtyArgIndex(CateSig,Indexable,Functor,ArgName,ArgNumber,Arg,IndexableArg,ArgType):-staredArgIndex(CateSig,Indexable,Functor,ArgName,ArgNumber,Arg,IndexableArg,ArgType),!.
dirtyArgIndex(CateSig,Indexable,Functor,ArgName,ArgNumber,Arg,IndexableArg,ArgType):- 
  debugFmt(dirtyArgIndex(CateSig,Indexable,Functor,ArgName,ArgNumber,Arg,IndexableArg,ArgType)),!.

%%%% todo maybe this.. once((retract(NEW),asserta(NEW)) ; (asserta(NEW),(debugFmt('~q.~n',[asserta(N)])))),!.
/*
asserta_if_new(NEW):-!,
  once(
   (retract(NEW),asserta(NEW)) ; 
   (asserta(NEW),debugFmt('~q.~n',[asserta(NEW)])) ),!.
*/
asserta_if_new(N):-catch(N,E,debugFmt(error_in(E,N))),!.
asserta_if_new(N):-asserta(N),debugFmt(asserta(N)),!.

% ===============================================================================================
%  Save Categories
% ===============================================================================================
assertCate(Ctx,Cate,DoWhat):-
      prolog_must(makeAimlCate(Ctx,Cate,Value)),!,
      prolog_must(ground(Value)),
      prolog_must(assertCate3(Ctx,Value,DoWhat)),!.

%% todo maybe this.. once((retract(NEW),asserta(NEW)) ; (asserta(NEW),(debugFmt('~q.~n',[NEW])))),!.
% assertCate3(Ctx,NEW,DoWhat):-NEW,!.
assertCate3(Ctx,NEW,DoWhat):-
  flag(cateSigCount,X,X+1), 
  forall(member(Pred,DoWhat),prolog_must(call(Pred,Ctx,NEW))).
% ===============================================================================================
%  Make AIML Categories
% ===============================================================================================
makeAimlCate(Ctx,Cate,Value):- 
 prolog_mustEach((
   convert_template(Ctx,Cate,Assert),
   aimlCateOrder(Order),
   makeAllParams(Ctx,Order,Assert,Result),
   arg2OfList(Result,LISTO), Value =.. [aimlCate|LISTO])).

arg2OfList(LIST,LISTO):-maplist_safe(arg2,LIST,LISTO),!.
arg2(_=Value,Value):-!.
arg2(Value,Value):-!,ctrace.


translate_cate(Ctx,CateSig):-replaceArgsVar(Ctx,[srcinfo=_],CateSig),assert_cate_in_load(Ctx,CateSig).

is_xml_missing(Var):-prolog_must(nonvar(Var)),!,member(Var,['[]','*','_']),!.

isRetraction(Ctx,CateSig,OF):-getCategoryArg1(Ctx,'template',NULL,_StarNumber,CateSig),!,is_xml_missing(NULL),
   duplicate_term(CateSig,OF),replaceArgsVar(Ctx,['template'=_,srcinfo=_,srcfile=_],OF),!.

% ===============================================================================================
%  Popping when Building categories
% ===============================================================================================

clearCateStack(_Ctx):- retractall(dict(category,_,_)).

peekCateElements(Ctx,Cate):- cateMemberTags(CATETAGS), peekAttributes(Ctx,CATETAGS,category,Cate),!.

popCateElements(Ctx,Cate):- cateMemberTags(CATETAGS), peekAttributes(Ctx,CATETAGS,category,Cate),!.
popCateElements(Ctx,CateO):- popCateElements1(Ctx,Cate1),popCateElements2(Ctx,Cate2),append(Cate1,Cate2,Cate),!,CateO=Cate.
popCateElements1(Ctx,CateO):- findall(Tag=DCG,cateNodes1(Ctx,category,Tag,DCG),Cate),!,CateO=Cate.
popCateElements2(Ctx,CateO):- findall(Tag=DCG,cateNodes2(Ctx,category,Tag,DCG),Cate),!,CateO=Cate.


cateNodes1(Ctx,Scope,Tag,DCGO):-member(Tag,[pattern,template]),once(cateNodes1a(Ctx,Scope,Tag,TEMPLATE)),once(convert_template(Ctx,TEMPLATE,DCG)),!,DCG=DCGO.

cateNodes1a(Ctx,Scope,Tag,DCGO):-peekNameValue(Ctx,Scope,Tag,DCG,'$failure'),popNameValue(Ctx,Scope,Tag,DCG),!,DCG=DCGO.
cateNodes1a(Ctx,Scope,Tag,DCGO):-listing(dict),aiml_error(peekNameValue(Ctx,Scope,Tag,DCG)),!,DCG=DCGO.
cateNodes1a(Ctx,Scope,Tag,DCGO):-peekNameValue(Ctx,Other,Tag,DCG,'$error'),Other\==Scope,!,DCG=DCGO.


cateNodes2(Scope,Tag,DCGO):-member(Tag,[that,guard,topic]),once(cateNodes2a(Scope,Tag,TEMPLATE)),once(convert_template(_Ctx,TEMPLATE,DCG)),!,DCG=DCGO.

cateNodes2a(Scope,Tag,DCGO):-peekNameValue(_Ctx,Other,Tag,DCG,'$failure'),Other\==Scope,!,DCG=DCGO.
cateNodes2a(Scope,Tag,DCGO):-aiml_error(peekNameValue(_Ctx,Scope,Tag,DCG)),!,DCG=DCGO.

defaultPredicates(N,V):-member(N,[username,botname]),V='*'.

%defaultPredicates(N,V):-member(N,[input,pattern]),V='*'.
defaultPredicates(N,V):-defaultPredicatesS(S),member(N=V,S).
defaultPredicatesS([
             graph='default',
             precall='true',
             topic='*',
             that='*',
             request='*',
             flags='*',
             pattern='*',
             call='true',
             % hide for testing 
             dictionary='default',
             userdict='user',
             substitutions='input',
             guard='*',
             template=['is ERROR IN CATE'],
             lang='bot',
             srcinfo=missinginfo,
             srcfile=missingfile,
             withCategory=[writeqnl,assert_cate_in_load]]).
 
cateMember(Tag):-cateMemberTags(List),member(Tag,List).

defaultCatePredicatesS(Defaults):-cateFallback(Defaults).

/*
And your chair is kept this time For some confidence you can ask them when the next trip to Value Villiage is.. 
You should be permited to keep your chair since you are willing to not leave the grounds except on nursing facility sanctioned trips.
I Think they have trips to the Dollar Tree and other places 
They also can give us special permission on Mondays and Thursdays 

The main thing is that you are willing to give them the peace of mind that they dont need to "watch you".

*/
cateFallback(N,V):-cateFallback(List),!,member(N=V,List).
cateFallback([
       graph = 'default',
       precall = 'true',
       topic = '*',
       that = '*',
       request = '*',
       pattern='*',
       flags= '*',
       call = 'true',
       guard = '*',
       userdict = 'user',
       template = '[]',
       srcinfo=missinginfo,
       srcfile=missingfile,
       withCategory=[writeqnl,assert_cate_in_load]]).
       %%|MORE]):-findall(N=V,defaultPredicates(N,V),MORE).

pathAttrib(S):-pathAttribS(SS),member(S,SS).
pathAttribS([uri,loc,filename,url,path,dir,file,pathname,src,location]).



% ===============================================================================================
% Callable input
% ===============================================================================================
callableInput(_Ctx,String,_Input,_Output):-traceIf(var(String)),fail.
callableInput(Ctx,[S|Tring],Input,Output):-joinAtoms([S|Tring],' ',String),!,callableInput0(Ctx,String,Input,Output).
callableInput(Ctx,String,Input,Output):-string(String),string_to_atom(String,Atom),!,callableInput0(Ctx,Atom,Input,Output).
callableInput(Ctx,Atom,Input,Output):-callableInput0(Ctx,Atom,Input,Output).

callableInput0(Ctx,[String],Input,Output):-!,callableInput(Ctx,String,Input,Output).
callableInput0(_Ctx,NonAtom,_Input,_Output):- not(atom(NonAtom)),!,fail.
callableInput0(_Ctx,Atom,_Input,result(Term,Vars)):-catch(atom_to_term(Atom,Term,Vars),_,fail),
  callable(Term),catch(callInteractive0(Term,Vars /*,Results */),_,fail).
callableInput0(Ctx,Atom,_Input,VotesO-Output):-atom_prefix(Atom,'@'),
  % re-direct to input
  withAttributes(Ctx,[],prolog_must(computeAnswer(Ctx,1,element(system,[],Atom),Output,VotesO))),!.


% ===============================================================================================
% Eval a SRAI
% ===============================================================================================
computeSRAIElement(Ctx,Votes,ATTRIBS,Input0,Output,VotesO):-
 withAttributes(Ctx,ATTRIBS, ((
   computeSRAIElement0(Ctx,Votes,ATTRIBS,Input0,OutputM,VotesOM),
   computeTemplateOutput(Ctx,VotesOM,OutputM,Output,VotesO)))),!.

computeSRAIElement0(Ctx,Votes,ATTRIBS,Input0,Output,VotesO):-
  prolog_must(ground(Input0)),!,
  flatten([Input0],Input),
  thread_local_flag(sraiDepth,SraiDepth,SraiDepth+1),
  computeSRAIElement1(Ctx,Votes,SraiDepth,ATTRIBS,Input,Output,VotesO),
  thread_local_flag(sraiDepth,_,SraiDepth),!.

computeSRAIElement1(Ctx,Votes,SraiDepth,ATTRIBS,Input,Output,VotesO):-SraiDepth>1,!,evalSRAI(Ctx,Votes,SraiDepth,ATTRIBS,Input,Output,VotesO),!.
computeSRAIElement1(Ctx,Votes,SraiDepth,ATTRIBS,Input,Output,VotesO):-catch(evalSRAI(Ctx,Votes,SraiDepth,ATTRIBS,Input,Output,VotesO),aiml_goto(Output,VotesO),thread_local_flag(sraiDepth,_,0)),!.


evalSRAI(Ctx,Votes,SraiDepth,ATTRIBS,_Input,_Unusued,_VotesO):- SraiDepth>80,
  getAliceMem(Ctx,bot,'infinite-loop-input',Output),!,VotesO is Votes * 0.8,
  throw_aiml_goto(element(srai,ATTRIBS,Output),VotesO).
  %%throw_aiml_goto(proof(element(template,ATTRIBS,[element(srai,ATTRIBS,Output)]),loop(sraiDepth,SraiDepth,80,ATTRIBS,Input)),VotesO).

/*
evalSRAI(Ctx,Votes,_SraiDepth,ATTRIBS,Input,_Unusued,_VotesO):-
 frame_depth(Depth),Depth>3000,getAliceMem(Ctx,bot,'infinite-loop-input',Output),!,VotesO is Votes * 0.8,
 throw_aiml_goto(proof(element(template,ATTRIBS,[element(srai,ATTRIBS,Output)]),loop(frameDepth,Depth,3000,ATTRIBS,Input)),VotesO).
*/


evalSRAI(Ctx,Votes,_SraiDepth,ATTRIBS,[I|Input0],Output,VotesO):-atom(I),atom_prefix(I,'@'),!,
  % re-direct to input
  withAttributes(Ctx,ATTRIBS,prolog_must(computeAnswer(Ctx,Votes,element(system,ATTRIBS,[I|Input0]),Output,VotesO))),!.

evalSRAI(Ctx,Votes,_SraiDepth,ATTRIBS,Input,Output,VotesO):-
 prolog_must(var(SYM)),
 prolog_must(peekNameValue(Ctx,ATTRIBS,['evalsrai','userdict','scope'],SYMPREV,'$value'(user))),
 ifThen(var(SYM),evalsrai(SYM)),
 var(Proof), 
   withAttributes(Ctx,['evalsrai'=SYM,proof=Proof],
  ((
    addInherit(SYM,SYMPREV),
    debugOnError(computeSRAI(Ctx,Votes,SYM,Input,MidIn,VotesM,Proof)),

    computeSRAIStars(Ctx,ATTRIBS,Input,MidIn,VotesM,SYM,Proof,Output,VotesO),
    remInherit(SYM,SYMPREV),
    ifThen(nonvar(SYM),retractallSrais(SYM))))).

    
computeSRAIStars(Ctx,ATTRIBS,Input,MidIn,VotesM,SYM,Proof,Output,VotesO):- fail,
    prolog_must((nonvar(MidIn),
                 nonvar(SYM),
                 singletons([Ctx,ATTRIBS]),
                 nonvar(Proof))),
      %% Proof = Output,       
      MidIn = Output, 
      VotesM = VotesO,
      nop(debugFmt(computeSRAIStars(SYM,Input,Output))),
      prolog_must((ground(Output),number(VotesO))),!.

computeSRAIStars(Ctx,ATTRIBS,Input,MidIn,VotesM,SYM,Proof,Output,VotesO):-
    prolog_must((nonvar(MidIn),
                 nonvar(SYM),
                 nonvar(Proof))),
      setCtxValue(Ctx,'evalsrai',SYM),
      %%MidProof = Proof, 
      computeElementMust(Ctx,VotesM,template,ATTRIBS,MidIn,MidIn9,VotesI9),
      prolog_must(answerOutput(MidIn9,Mid9)),
      debugFmt(evalSRAI(SYM,Input,MidIn,MidIn9,Mid9)),
      prolog_must(computeAnswer(Ctx,VotesI9,Mid9,Output,VotesO)),
      prolog_must((ground(Output),number(VotesO))),!.

 evalsrai(SYM):-gensym('evalsrai',SYM).

% ===============================================================================================
% Apply Input Match
% ===============================================================================================

computeSRAI(_Ctx,_Votes,_SYM,[],_,_,_Proof):- !,ctrace,fail.

computeSRAI(Ctx,Votes,SYM,Input,Result,VotesO,Proof):-
   getAliceMem(Ctx,'bot','me',Robot),
   getAliceMem(Ctx,'bot','you',User),
   ifThen(var(SYM),evalsrai(SYM)),
   getConversationThread(Ctx,User,Robot,ConvThread),
   prolog_must(computeSRAI0(Ctx,Votes,ConvThread,SYM,Input,Result,VotesO,Proof)).

getConversationThread(Ctx,User,Robot,ConvThread):-
   ConvThread = fromTo(User,Robot),
   setCtxValue(Ctx,'convthread',ConvThread),!.

computeSRAI0(Ctx,Votes,ConvThread,SYM,Input,Result,VotesO,Proof):-   
   computeInnerTemplate(Ctx,Votes,Input,NewIn,VotesM),NewIn \== Input,!,
   computeSRAI0(Ctx,VotesM,ConvThread,SYM,NewIn,Result,VotesO,Proof),!.

computeSRAI0(Ctx,Votes,ConvThread,SYM,Input,Result,VotesO,Proof):- not(is_list(Input)),compound(Input),
   answerOutput(Input,InputO),Input\==InputO,!,ctrace,
   computeSRAI0(Ctx,Votes,ConvThread,SYM,InputO,Result,VotesO,Proof).

computeSRAI0(Ctx,Votes,ConvThread,SYM,Input,Result,VotesO,Proof):- not(is_list(Input)),compound(Input),
   computeAnswer(Ctx,Votes,Input,InputO,VotesM),Input\==InputO,!,ctrace,
   computeSRAI0(Ctx,VotesM,ConvThread,SYM,InputO,Result,VotesO,Proof).

computeSRAI0(Ctx,Votes,ConvThread,SYM,Input,Result,VotesO,Proof):-
  Each = (OutputLevel - e(VotesM,Result,Proof)), %% VotesO make it sort/2-able
  Call = computeSRAI2(Ctx,Votes,ConvThread,SYM,Input,Result,VotesM,Proof,OutputLevel),
  copy_term(Each:Call,EachFound:CallFound),  
  findall(EachFound:CallFound, CallFound, FOUND),
  FOUND=[_|_],
  sort(FOUND,ORDER),!,
 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   member(Each:Call,ORDER),
   prolog_must(nonvar(Result)),
   debugFmt(computeSRAI(Input,SYM,Each)),
   VotesO is VotesM * 1.1.

computeSRAI0(_Ctx,Votes,ConvThread,SYM,Input,Result,VotesO,Proof):- !, VotesO is Votes * 0.7,
     Result = ['I',heard,you,think,SYM,and,'say:'|Input],
      Proof = result(Result,failed_computeSRAI2(Votes,Input,ConvThread)),
      debugFmt(Proof),!.

% now ctrace is ok

% this next line is what it does on fallback
computeSRAI0(Ctx,Votes,ConvThread,SYM,[B|Flat],[B|Result],VotesO,Proof):- fail,
   computeSRAI2(Ctx,Votes,ConvThread,SYM,Flat,Result,VotesO,Proof,_PostOutputLevel3),prolog_must(nonvar(Result)).

checkSym(_SYM).

subclassMakeUserDict(Ctx,UserDict,SYM):-debugFmt(subclassMakeUserDict(Ctx,UserDict,SYM)),addInherit(UserDict,SYM).

convThreadDict(_Ctx,ConvThreadHint,ConvThread):-answerOutput(ConvThreadHint,First),unlistify(First,ConvThread),!.

computeSRAI222(CtxIn,Votes,ConvThreadHint,SYM,Pattern,Compute,VotesO,ProofOut,OutputLevel):-    
 prolog_mustEach((
   %%convertToMatchable(Pattern,InputPattern),
   prolog_must(current_value(CtxIn,'evalsrai',SYM2)),
   ifThen(var(SYM),SYM=SYM2),
   ifThen(SYM\==SYM2,debugFmt(syms(SYM\==SYM2))),
      convThreadDict(Ctx,ConvThreadHint,ConvThread),
         getCategoryArg(Ctx,'template',Out, _Out_ ,CateSig),!,
         getAliceMemOrSetDefault(CtxIn,ConvThread,SYM,'topic',Topic,['Nothing']),
         getAliceMemOrSetDefault(CtxIn,ConvThread,SYM,'userdict',UserDict,'user'), 
         %%getAliceMemOrSetDefault(CtxIn,ConvThread,SYM,'convthread',ConvThread,SYM,ConvThreadHint), 
         subclassMakeUserDict(CtxIn,UserDict,SYM),
         getAliceMemOrSetDefault(CtxIn,ConvThread,SYM,'that',That,['Nothing']),
  
   PreTopic = ignore(CtxIn=Ctx),PreTopic,
   TTP = topicThatPattern(Topic,That,Pattern),
   debugFmt(TTP),!,
   traceIf(not(ground(TTP))),
   must_be_openCate(CateSig),!,
   /*
   prolog_must(topicThatPattern(Ctx,Topic,That,Pattern,PreTopic,Out,CateSig,OutputLevel,StarSets_All,ClauseNumber,CommitTemplate)),
   */
   
   UNIF= topicThatPattern(Ctx,Topic,That,Pattern,PreTopic,Out,CateSig,OutputLevel,StarSets_All,ClauseNumber,CommitTemplate),
   singletons([ClauseNumber]),
   findall(UNIF,UNIF,UNIFS),!,
   traceIf(UNIFS=[]),
   %%%%% iterate from here %%%%%
   member(UNIF,UNIFS), 
         once(prolog_mustEach((
            retractallSrais(SYM),
            prolog_must(CommitTemplate),
            prolog_must(nonvar(Out)),
            cateStrength(CateSig,Mult),
           %% not(contextUsedClaused(Ctx,CateSig,ClauseNumber)),
            VotesO is Votes * Mult,
            makeWithAttributes(StarSets_All,Out,Compute),       
            %%MoreProof = [cn(ClauseNumber),CateSig],
            MoreProof = [topicThatPatternOutput(Topic,That,Pattern,Out)],
            ProofOut=..[proof,Compute|MoreProof]))))).


clauseRef(_CateSig,0):-!.
clauseRef(CateSig,Pattern:Template):-arg(6,CateSig,Pattern),arg(11,CateSig,Template),!.
clauseRef(CateSig,ClauseNumber):-clause(CateSig,true,ClauseNumber).
clauseRef(_CateSig,-1):-!.

savedParts(Save,PreTopic,CommitTemplate,OutputLevel,StarSets_All,Out,ClauseNumber,CateSig):-
      Save = OutputLevel - StarSets_All - Out - ClauseNumber - CateSig - CommitTemplate - PreTopic.

starSetsAll(Ctx,Topic,That,Pattern,Save,PreTopic):-
   savedParts(Save,PreTopic,_CAfterPattern,OutputLevel,StarSets_All,Out,ClauseNumber,CateSig),
   getCategoryArg(Ctx,'template',Out, _Out_ ,CateSig),
   functor(CateSig,CateSigFunctor,_Args),
   OutputLevel = OutputLevel1 - OutputLevel2 - OutputLevel3,!,
   
   %%%%% Iterate here %%%%
   cate_match(Ctx,CateSigFunctor,'pattern',Pattern,CateSig,_MatchPattern,StarSets_Pattern,OutputLevel3),   
   call(CateSig),
   clauseRef(CateSig,ClauseNumber), %%%%%
   once(( cate_match(Ctx,CateSigFunctor,'topic',Topic,CateSig,_MatchTopic,StarSets_Topic,OutputLevel1),
   cate_match(Ctx,CateSigFunctor,'that',That,CateSig,_MatchThat,StarSets_That,OutputLevel2),
   combineStarSets(StarSets_Topic,StarSets_That,StarSets_Pattern,StarSets_All) )).

combineStarSets(StarSets_Topic,StarSets_That,StarSets_Pattern,StarSets_All):-
   append(StarSets_Topic,StarSets_That,StarSets_TopicThat),
   append(StarSets_Pattern,StarSets_TopicThat,StarSets_All),!.

cate_match(Ctx,CateSigFunctor,StarName,TextPattern,CateSig,MatchPattern,StarSets,OutputLevel):-
    getCategoryArg1(Ctx,StarName,MatchPattern,_StarNumber,CateSig),!,
    argNFound(CateSigFunctor,StarName,MatchPattern,_),
    make_star_binders(Ctx,StarName,1,TextPattern,MatchPattern,OutputLevelInv,StarSets),OutputLevel is 1/OutputLevelInv.

%% simpler but slower.. maybe comment (fail) this one out for the faster next one
%% DOES NOT USE INDEXES 
topicThatPattern(Ctx,Topic,That,Pattern,PreTopic,Out,CateSig,OutputLevel,StarSets_All,ClauseNumber,CommitTemplate):- useNewCateSigSearch,
  prolog_mustEach((
   debugFmt(debugWarn(useNewCateSigSearch)), 
   CommitTemplate = (nop(CateSig),prolog_must(PreTopic)),
   savedParts(Save,PreTopic,CommitTemplate,OutputLevel,StarSets_All,Out,ClauseNumber,CateSig),
   findall(Save,starSetsAll(Ctx,Topic,That,Pattern,Save,PreTopic),AllCateSig),
   prolog_must(AllCateSig=[_|_]),
   sort(AllCateSig,SetOfAllCateSig),!,
   %%%%% Iterate here %%%%
   member(Save,SetOfAllCateSig))).

%% WILL GET HERE ONLY IF NEW ROUTINES ARE NOT AS FAST (WHICH IS THE CASE)
topicThatPattern(Ctx,Topic,That,Pattern,PreTopic,Out,CateSig,OutputLevel,StarSets_All,ClauseNumber,CommitTemplate):-
 prolog_mustEach((
   traceIf(useNewCateSigSearch),
   OutputLevel = OutputLevel1 - OutputLevel2 - OutputLevel3,!,
   CPreTopic = true,
   make_preconds_for_match(Ctx,'topic',Topic,CateSig,PreTopic,AfterTopic,CPreTopic,CAfterTopic,Out,MinedCates,EachMatchSig_Topic,StarSets_Topic,OutputLevel1),
   make_preconds_for_match(Ctx,'that',That,CateSig,AfterTopic,AfterThat,CAfterTopic,CAfterThat,Out,MinedCates,EachMatchSig_That,StarSets_That,OutputLevel2),
   make_preconds_for_match(Ctx,'pattern',Pattern,CateSig,AfterThat,FindPatternGoal,CAfterThat,CommitTemplate,Out,MinedCates,EachMatchSig_Pattern,StarSets_Pattern,OutputLevel3),
   prolog_must((var(Out),var(OutputLevel1),var(OutputLevel2),var(OutputLevel3))),
   must_be_openCate(CateSig))),
  prolog_mustEach((
   %%%%% Iterate here %%%%   
   atLeastOne(call((FindPatternGoal,
       /*
       combineStarSets(StarSets_Topic,StarSets_That,StarSets_Pattern,StarSets_All),
       copy_term(CateSig,CateSig2),ignore(CateSig2),
       traceIf((That=['How',_|_],nop(CateSig=CateSig2))),
       */
       CateSig))),   
   clauseRef(CateSig,ClauseNumber),
   singletons([EachMatchSig_Topic,EachMatchSig_That,EachMatchSig_Pattern]),
   combineStarSets(StarSets_Topic,StarSets_That,StarSets_Pattern,StarSets_All))).

savedSetPatterns(LSP,OutputLevel,StarSets,MatchPattern):- LSP = lsp(OutputLevel,StarSets,MatchPattern).

make_preconds_for_match(Ctx,StarName,TextPattern,CateSig,PrecondsSearch,PostcondsSearch,PrecondsCommit,PostcondsCommit,Out,MinedCates,EachMatchSig,StarSets,
 OutputLevel):-   
   make_prepost_conds(Ctx,StarName,TextPattern,CateSig,FindPatternGoal,CommitTemplate,Out,MinedCates,EachMatchSig,StarSets,OutputLevel),
   combineConjCall(PrecondsSearch,FindPatternGoal,PostcondsSearch),
   combineConjCall(PrecondsCommit,CommitTemplate,PostcondsCommit).

%%TODO: MAKE THIS ONE WORK ! (CURRENTLY WORKS)
make_prepost_conds(Ctx,StarName,TextPattern,CateSig,FindPatternGoal,CommitTemplate,Out,MinedCates,EachMatchSig,StarSets,OutputLevel):-  
 prolog_mustEach((
  CommitTemplate = true,
  generateMatchPatterns(Ctx,StarName,Out,TextPattern,CateSig,MinedCates,EachMatchSig),
  savedSetPatterns(LSP,OutputLevel,StarSets,MatchPattern),
  getCategoryArg(Ctx,StarName,MatchPattern,Out,CateSig),
  FindPatternGoal = ( member(LSP,EachMatchSig)/*,CateSig */) )),!.

contextUsedClaused(Ctx,CateSig,ClauseNumber):- fail, contains_term(Ctx,CateSig)->not(contains_term(Ctx,ClauseNumber));not(contains_term(Ctx,ClauseNumber)).

makeWithAttributes([],Proof,Proof):-!.
makeWithAttributes(StarSets_All,Proof,withAttributes(StarSets_All,Proof)).

retractallSrais(SYM):-prolog_must(nonvar(SYM)),ifThen(nonvar(SYM),(retractall(dict(SYM,_,_)))),fail.
retractallSrais(_SYM):-!.

cateStrength(_CateSig,1.1):-!.

computeSRAI2(Ctx,Votes,ConvThread,_SYM1,Pattern,Out,VotesO,ProofOut,OutputLevel):- !, %% avoid next one    
    computeSRAI222(Ctx,Votes,ConvThread,_SYM2,Pattern,Out,VotesO,ProofOut,OutputLevel).

getCategoryArg(Ctx,StarName,MatchPattern,Out,CateSig):-
   prolog_must(getCategoryArg0(Ctx,StarName,MatchPattern,Out,CateSig)),!.

getCategoryArg0(Ctx,StarName,MatchPattern,_Out,CateSig):-atomic(StarName),!,
  getCategoryArg1(Ctx,StarName,MatchPattern,_StarNumber,CateSig),!.
  
getCategoryArg0(Ctx,FAB,OutAOutB,Out,CateSig):- FAB=..[F,A,B],
      getCategoryArg(Ctx,A,OutA,Out,CateSig),!,
      getCategoryArg(Ctx,B,OutB,Out,CateSig),!,
      OutAOutB=..[F,OutA,OutB].

getCategoryArg1(_Ctx,StarName,MatchPattern,StarNumber,CateSig):-
   prolog_must(aimlCateSig(CateSig)),
   aimlCateOrder(Order),
   nth1(StarNumber,Order,StarName),
   prolog_must(arg(StarNumber,CateSig,MatchPattern)),!.


meansNothing(Var,_Nothing):-var(Var),!,aiml_error(meansNothing(var(Var))),!.
meansNothing([Atomic],Nothing):-nonvar(Atomic),!,meansNothing(Atomic,Nothing),!.
meansNothing(N,['Nothing']):-member(N,[[],'Nothing']),!.
meansNothing(Atom,['Nothing']):-atom(Atom),!,fail.
meansNothing(InputNothing,InputPattern):-prolog_must((ground(InputNothing),var(InputPattern))),meansNothing0(InputNothing,InputPattern),!.

meansNothing0([Atom],Out):-!,meansNothing0(Atom,Out).
meansNothing0('_',['Nothing']).
meansNothing0('*',['Nothing']).

combineConjCall(A,B,C):-A==true,!,C=B.
combineConjCall(A,B,C):-B==true,!,C=A.
combineConjCall(A,B,C):- C = (A,B).

addToMinedCates(_MinedCates,_CateSig):-!.
addToMinedCates(MinedCates,CateSig):-prolog_must(ground(CateSig)),append(_,[CateSig|_],MinedCates),!.
addToMinedCates(MinedCates,CateSig):-ctrace,var(MinedCates),!,MinedCates=[CateSig|_].

notSingletons(_Singleton_List):-!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% generateMatchPatterns -  finds the candidate indexers for some textInput
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
generateMatchPatterns(_Ctx,_StarName,_Out,_InputNothing,_CateSig,_NC_MinedCates,EachMatchSig):-nonvar(EachMatchSig),trace,!. % already done

generateMatchPatterns(Ctx,StarName,Out,InputNothing,CateSig,NC_MinedCates,EachMatchSig):- fail,
  hotrace(meansNothing(InputNothing,InputPattern)),
  InputNothing\==InputPattern,!,
  generateMatchPatterns(Ctx,StarName,Out,InputPattern,CateSig,NC_MinedCates,EachMatchSig).

generateMatchPatterns(Ctx,StarName,Out,InputNothing,CateSig,_NC_MinedCates,EachMatchSig):- fail,
  hotrace(meansNothing(InputNothing,_InputPattern)),!,
  traceIf(InputNothing\==['Nothing']),
  must_be_openCate(CateSig),
  getCategoryArg(Ctx,StarName,'*',Out,CateSig),
   prolog_must(EachMatchSig=[_|_]),
  must_be_openCate(CateSig),!.

%% The NEWEST match patterns NOW using Indexing!
generateMatchPatterns(Ctx,StarName,Out,InputPattern,CateSigIn,MinedCates,SetOfEachMatchSig):-  useIndexPatternsForCateSearch,
 prolog_mustEach((
  copy_term(CateSigIn,CateSig),
  CateSigIn=CateSig,
 functor(CateSig,CateSigFunctor,_Args),
  must_be_openCate(CateSig),
  getCategoryArg(Ctx,StarName,IndexPattern,Out,CateSig),
  savedSetPatterns(LSP,OutputLevel,StarSets,IndexPattern),   
  findall(LSP,
             (argNFound(CateSigFunctor,StarName,MatchPattern,IndexPattern),
              canMatchAtAll_debug(Ctx,StarName,InputPattern,MatchPattern,OutputLevel,StarSets)),
      EachMatchSig),
  prolog_must(EachMatchSig=[_|_]),
  sort(EachMatchSig,SetOfEachMatchSig),  
  prolog_must(debugFmtList([
        starName = StarName,
        %%eachMatchSig(EachMatchSig),
        setOfEachMatchSig=SetOfEachMatchSig,
        eachMatchSig=EachMatchSig,
        matchPattern=MatchPattern,
        minedCates=(MinedCates),
        cateSig=CateSig
        ])))),!.


%% The OLD match patterns NOT using Indexing
generateMatchPatterns(Ctx,StarName,Out,InputPattern,CateSig,_MinedCates,EachMatchSig):- ifThen(useIndexPatternsForCateSearch,ctrace),
 %% convertToMatchable(TextPattern,InputPattern),
  must_be_openCate(CateSig),
  copy_term(CateSig,CateSigC),!,
  getCategoryArg(Ctx,StarName,MatchPattern,Out,CateSigC),
  findall(MatchPattern,CateSigC,AllMatchSig),!,sort(AllMatchSig,SetOfEachMatchSig),!,
  savedSetPatterns(LSP,OutputLevel,StarSets,MatchPattern),
  findall(LSP,
             (member(MatchPattern,SetOfEachMatchSig), 
              canMatchAtAll_debug(Ctx,StarName,InputPattern,MatchPattern,OutputLevel,StarSets)),
      EachMatchSig),
 %%traceIf((StarName==pattern,InputPattern=[_,_|_])),
   prolog_must(EachMatchSig=[_|_]),
  prolog_must(debugFmtList([
        starName = StarName,
        eachMatchSig(EachMatchSig),
        setOfEachMatchSig=SetOfEachMatchSig,
        eachMatchSig=EachMatchSig,
        matchPattern=MatchPattern,
        cateSig=CateSig
        ])).

% ========================================================================================
%  canMatchAtAll_debug(Ctx,StarName,InputPattern,MatchPattern,OutputLevel,StarSets)
% ========================================================================================

canMatchAtAll_debug(Ctx,StarName,InputPattern,MatchPattern,OutputLevel,StarSets):-
    make_star_binders(Ctx,StarName,1,InputPattern,MatchPattern,OutputLevelInv,StarSets),!,OutputLevel is 1/OutputLevelInv ,
    nop(debugFmt(pass_canMatchAtAll_debug(Ctx,StarName,InputPattern,MatchPattern,OutputLevel,StarSets))),!.

canMatchAtAll_debug(Ctx,StarName,InputPattern,MatchPattern,_OutputLevel,_StarSets):-
    nop(debugFmt(fail_canMatchAtAll_debug(Ctx,StarName,InputPattern,MatchPattern))),!,fail.

% skip over skipable words
consumeSkippables([],[]).
consumeSkippables([Skipable|B],BB):- isIgnoreableWord(Skipable),!,consumeSkippables(B,BB).
consumeSkippables(A,A).

removeSkippables(A,A):-atomic(A),!.
removeSkippables([Skipable|B],BB):- isIgnoreableWord(Skipable),!,removeSkippables(B,BB).
removeSkippables([Skipable|B],[Skipable|BB]):- removeSkippables(B,BB).
removeSkippables(A,A).

% ======================================================================================== 
% make_star_binders(Ctx, StarName, Text , Pattern, 1/OutputLevel, StarSetsNameValues).
%
% pattern_match(Text , Pattern)..  would be simply   make_star_binders(_Ctx, starName, Text , Pattern, _OutputLevel, _StarSetsNameValues).
% ========================================================================================
make_star_binders(_Ctx,StarName,_N,InputPattern,MatchPattern,OutputLevel,StarSets):- 
   prolog_must(var(StarSets)),prolog_must(var(OutputLevel)),prolog_must(ground(StarName:InputPattern:MatchPattern)),fail.  

:-setLogLevel(make_star_binders,none).

%end check
make_star_binders(_Ctx,_StarName,_N,L,R,1,[]):-R==[],!,consumeSkippables(L,LL),!,LL==[].
make_star_binders(_Ctx,_StarName,_N,L,R,1,[]):-L==[],!,consumeSkippables(R,RR),!,RR==[].

% left hand star/wild  (cannot really happen (i hope))
%make_star_binders(_Ctx,StarName,_N,Star,_Match,_OutputLevel,_StarSets):- fail, not([StarName]=Star),isStarOrWild(StarName,Star,_WildValue,_WMatch,_Pred),!,ctrace,fail. 


% simplify
make_star_binders(Ctx,StarName,N,[Word1|B],[Word2|BB],CountO,StarSets):-
     sameWords(Word1,Word2),!,make_star_binders(Ctx,StarName,N,B,BB,Count,StarSets),CountO is Count + 1.

% tail (all now in) star/wildcard
make_star_binders(_Ctx,StarName,N,InputText,WildCard,WildValue,[Pred]):-isStarOrWild(StarName,N,WildCard,WildValue,InputText,Pred),!.

% once in star.. walk past star
make_star_binders(Ctx,StarName,N,InputText,[WildCard,M0|More],ValueO,[Pred|StarSets]):-isStarOrWild(StarName,N,WildCard,WildValue,SkipedSTAR,Pred),
         append(SkipedSTAR,[M1|LeftMore],InputText),sameWords(M0,M1),N2 is N+1,
         make_star_binders(Ctx,StarName,N2,LeftMore,More,Value,StarSets),!,ValueO is WildValue + Value.

% is mid-right hand wildcard (this should be the last test)
make_star_binders(Ctx,StarName,N,[Match|B],[WildCard|BB],ValueO,[Pred|StarSets]):- isStarOrWild(StarName,N,WildCard,WildValue,Match, Pred),!,
     N2 is N+1,
     make_star_binders(Ctx,StarName,N2,B,BB,Value,StarSets),!,ValueO is WildValue + Value.

% tail is an atom (indexical unifier)
make_star_binders(Ctx,StarName,N,InputText,Indexical,WildValue,Pred):-
      atom(Indexical),!,
      make_star_binders(Ctx,StarName,N,InputText,[Indexical],WildValue,Pred).

% tail is a compound (indexical unifier)
make_star_binders(Ctx,StarName,N,InputText,Indexical,WildValue,Pred):-
      not(is_list(Indexical)),/*compound(Indexical),*/toNonIndexable(Indexical,[L|IST]),
      make_star_binders(Ctx,StarName,N,InputText,[L|IST],WildValue,Pred).



% skip over skippable words
make_star_binders(Ctx,StarName,N,Skipable,BB,CountO,StarSets):- 
  skipablePhrase(Skipable,B),!,make_star_binders(Ctx,StarName,N,B,BB,Count,StarSets),CountO is Count + 1.
  %%warnIf((isIgnoreableWord(Skipable),!,make_star_binders(Ctx,StarName,N,B,BB,Count,StarSets),CountO is Count + 1)),number(CountO).


skipablePhrase([Skipable|B],B):-isIgnoreableWord(Skipable),!.
skipablePhrase([Skip,'\b',Ble|B],[Skipable|B]):-joinAtoms([Skip,'\b',Ble],' ',Skipable),!.

isIgnoreableWord(Skipable):-member(Skipable,['-','(',')',',','?','.','','\'']).
isIgnoreableWord(Skipable):-isWhiteWord(Skipable).

isWhiteWord(Skipable):-member(Skipable,[' ','\b','\n','']).

%
% re-write section
%
%
/*
make_star_binders(Ctx,StarName,InputNothing,MatchPattern,OutputLevel,StarSets):- 
   hotrace((InputNothing \== '*',(InputPattern==StarName ; meansNothing(InputNothing,InputPattern)))),!, ctrace,
   make_star_binders(Ctx,StarName,['Nothing'],MatchPattern,OutputLevel,StarSets).


% must come before search failures
make_star_binders(Ctx,StarName,TextPattern,MatchPattern,OutputLevel,StarSets):- fail,
  hotrace(((convertToMatchable(TextPattern,InputPattern),TextPattern \== InputPattern))),!,
  make_star_binders(Ctx,StarName,InputPattern,MatchPattern,OutputLevel,StarSets),!,ctrace.

% fast veto
make_star_binders(_Ctx,StarName,[I0|Pattern],[Match|MPattern],_OutputLevel,_Commit):-
   member(M,[Match|MPattern]),requireableWord(StarName,M),not(member(M,[I0|Pattern])),!,fail.

% fast veto
make_star_binders(_Ctx,_StarName,[_],[_,_|_],_NoNum,_NoCommit):-!,fail.


make_star_binders(_Ctx,StarName,[E|More],Match,Value,[tryLater([E|More],Match)]):-compound(E),ctrace,isWildCard(StarName,E,Value),!.

% weird atom
%%make_star_binders(_Ctx,StarName,I,Atom,12,[Atom=I]):-atom(Atom),ctrace,!,loggerFmt(make_star_binders,canMatchAtAll_atom(StarName,I,Atom)),!.

*/
starNameTransform(Star,StarStar):-starName(Star,StarStar),!.
starNameTransform(StarName,StarName):-atom_concat(_,'star',StarName),!.
starNameTransform(StarName,StarNameStar):-atom_concat(StarName,'star',StarNameStar),!.

isStarOrWild(StarName,[StarNameText],WildValue,InputText,Pred):-nonvar(StarNameText),!,isStarOrWild(StarName,_,StarNameText,WildValue,InputText,Pred),!.

isStarOrWild(StarName,N,StarNameText,WildValue,InputText,StarNameStarN=InputText):-
   isStar(StarName,StarNameText,WildValue),!,starNameTransform(StarName,StarNameStar),atom_concat(StarNameStar,N,StarNameStarN),!,traceIf(isStarValue(InputText)).
isStarOrWild(StarName,_N,WildCardText,WildValue,InputText,Pred):- isWildCard(StarName,WildCardText,WildValue,InputText,Pred),!.

isWildCard(StarName,Wild,1,InputText,call(sameBinding(Wild,InputText))):- not(is_list(Wild)),compound(Wild),Wild=..LWild,not(not(member(StarName,LWild))),!.

requireableWord(StarName,M):-not(isOptionalOrStar(StarName,M)).

isOptionalOrStar(_StarName,M):-not(atom(M)),!,ctrace.
isOptionalOrStar(StarName,M):-isStar(StarName,M),!.

/*
isStar(StarName,'topic'):-!. %%,ctrace.
isStar(StarName,'that'):-!.
isStar(StarName,'input'):-!.
*/
isStar(StarName,StarNameText):-isStar(StarName,StarNameText,_Order),!.
isStar(StarName,StarNameText,WildValue):-not(ground(StarNameText)),ctrace,debugFmt(isStar(StarName,StarNameText,WildValue)),!,fail.
isStar(StarName,[StarNameText],WildValue):-isStar(StarName,StarNameText,WildValue),!.
isStar(_StarName,'*',0.3).
isStar(_StarName,'_',0.8).
%%WAS VERY BAD IDEA:  isStar(StarName,StarNameText,6):-atom(StarName),!,StarNameText==StarName,writeq(qqqq-qq),ctrace.


must_be_openCate(_CateSig):-!.
must_be_openCate(CateSig):- prolog_must(hotrace((((nonvar(CateSig),not(ground(CateSig)),must_be_openCate0(CateSig)))))),!.
must_be_openCate0(CateSig):- arg(_,CateSig,Arg),must_be_openCateArgs(Arg,CateSig),fail.
must_be_openCate0(_CateSig):-!.

must_be_openCateArgs(Arg,_CateSig):-var(Arg),!.
must_be_openCateArgs('*',_CateSig):-!.
must_be_openCateArgs(List,CateSig):-ctrace, throw(List:CateSig),!.

starSets(Ctx,List):-prolog_must((mapsome_openlist(starMust0,List),mapsome_openlist(starMust1(Ctx),List),mapsome_openlist(starMust2,List))),!.

star_flag(Flag,Out,In):- starNameTransform(Flag,StarFlag), flag(StarFlag,Out,In),!. %%,prolog_must(atom_concat(_,'star',Flag)),!.

endOfList(EndOfList):-(var(EndOfList);atomic(EndOfList)),!.

starMust0(StarName=_):-star_flag(StarName,_,1).
starMust1(Ctx,StarName=Value):-starSet(Ctx,StarName,Value).
starMust2(call(Call)):-!,prolog_must(Call).
starMust2(_Skip).

starSet(Ctx,StarNameI,Pattern):- 
   starName(StarNameI,StarName),
   ignore((var(N),star_flag(StarName,N,N))),
   traceIf(isStarValue(Pattern)),
   getDictFromAttributes(Ctx,'evalsrai',[],Dict),
   prolog_must(Dict\==user),
   atom_concat(StarName,N,StarNameN),
   prolog_must(not((getAliceMemComplete(Ctx,Dict,StarNameN,Old),debugFmt(getAliceMemComplete(Ctx,Dict,StarNameN,Old))))),
   setAliceMem(Ctx,Dict,StarNameN,Pattern),!,star_flag(StarName,NN,NN+1).

%%REAL-UNUSED  set_matchit1(StarName,Pattern,Matcher,OnBind):- length(Pattern,MaxLen0), MaxLen is MaxLen0 + 2,
%%REAL-UNUSED    set_matchit2(StarName,Pattern,Matcher,MaxLen,OnBind).

%%isStar0(Word1):- member(Word1,[*,'_']).
isStar0('*').
isStar0('_').

sameWords(Word1,Word2):-atom(Word1),atom(Word2),atoms_match0(Word1,Word2).
 atoms_match0(Word1,Word2):- (isStar0(Word1);isStar0(Word2)),!,fail.
 atoms_match0(Word1,Word1):-!.
 atoms_match0(Word1,Word2):-literal_atom(Word1,WordO),literal_atom(Word2,WordO),!.

