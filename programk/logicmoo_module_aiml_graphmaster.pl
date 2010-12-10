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

%:- cateFallback(ATTRIBS), pushAttributes(_Ctx,default,ATTRIBS).
%:- cateFallback(ATTRIBS), popAttributes(_Ctx,default,ATTRIBS).
%:- cateFallback(ATTRIBS), pushAttributes(_Ctx,default,ATTRIBS).

:-pp_listing(dict(_,_,_)).


% ===================================================================
%  aimlCate database decl
% ===================================================================

:-dynamic(aimlCateSigCached/1).
aimlCateSig(X):-aimlCateSigCached(X),!.
aimlCateSig(X):-aimlCateOrder(List),length(List,L),functor(Pred,aimlCate,L),asserta(aimlCateSigCached(Pred)),!,copy_term(Pred,X).

aimlCateOrder([graph,precall,topic,that,request,pattern,flags,call,guard,userdict,template,srcinfo,srcfile]).

% [graph,precall,topic,that,pattern,flags,call,guard,template,userdict]
cateMemberTags(Result):- aimlCateOrder(List), findall(E,(member(E0,List),once((E0=[E|_];E0=E))), Result).

makeAimlCateSig(Ctx,ListOfValues,Pred):-aimlCateSig(Pred),!,makeAimlCate(Ctx,ListOfValues,Pred,'$current_value'),!.

:- aimlCateOrder(List),length(List,L),dynamic(aimlCate/L),multifile(aimlCate/L). 

replaceArgsVar(_Ctx,[],_CateSig,_With):-!.
replaceArgsVar(Ctx,[E|L],CateSig,With):-copy_term(With,Replacement),
    getCategoryArg1(Ctx,E,_NULL,StarNumber,CateSig),nb_setarg(StarNumber,CateSig,Replacement),
    replaceArgsVar(Ctx,L,CateSig,With),!.

:-dynamic(argNumsTracked/3).
:-dynamic(argNFound/3).
:-index(argNFound(1,1,1)).


%% aimlCateOrder([graph,precall,topic,that,request,pattern,flags,call,guard,userdict,template,srcinfo,srcfile]).
argNumsTracked(aimlCate,topic,3).
argNumsTracked(aimlCate,that,4).
argNumsTracked(aimlCate,pattern,6).

argNFound(aimlCate,'13',_).
argNFound(aimlCate,'12',_).
argNFound(aimlCate,'11',_).
argNFound(aimlCate,'10',_).
argNFound(aimlCate,'9',_).

assert_cate_in_load(NEW):-isRetraction(_Ctx,NEW,OF),!,retractall(OF),!.
assert_cate_in_load(NEW):-asserta(NEW),makeArgIndexes(NEW),!.

makeArgIndexes(_CateSig):-!.
makeArgIndexes(CateSig):-functor(CateSig,F,_),makeArgIndexes(CateSig,F),!.
makeArgIndexes(CateSig,F):- argNumsTracked(F,Atom,Number),arg(Number,CateSig,Arg),nonvar(Arg),
         %%Number<10,nonvar(Arg),atom_number(Atom,Number),
         assert_if_new(argNFound(F,Atom,Arg)),fail.
makeArgIndexes(_NEW,_F).


assert_if_new(N):-N,!.
assert_if_new(N):-assert(N),!.

% ===============================================================================================
%  Save Categories
% ===============================================================================================
assertCate(Ctx,Cate,DoWhat):-
      makeAimlCate(Ctx,Cate,Value),!,
      assertCate3(Ctx,Value,DoWhat),!.

%% todo maybe this.. once((retract(NEW),asserta(NEW)) ; (asserta(NEW),(debugFmt('~q.~n',[NEW])))),!.
% assertCate3(Ctx,NEW,DoWhat):-NEW,!.
 assertCate3(Ctx,NEW,DoWhat):-
  flag(cateSigCount,X,X+1), forall(member(Pred,DoWhat),call(Pred,Ctx,NEW)).
% ===============================================================================================
%  Make AIML Categories
% ===============================================================================================
makeAimlCate(Ctx,Cate,Value):-makeAimlCate(Ctx,Cate,Value,'$first'(['$value'('*'),'$current_value'])),!.
makeAimlCate(Ctx,Cate,Value,UnboundDefault):- debugOnFailureAiml((convert_template(Ctx,Cate,Assert),!,makeAimlCate1(Ctx,Assert,Value,UnboundDefault))).

makeAimlCate1(Ctx,Assert,Value,UnboundDefault):-
   aimlCateOrder(Order),
   makeAllParams(Ctx,Order,Assert,UnboundDefault,Result),
   makeAimlCate2(Ctx,Result,UnboundDefault,Value),!.

arg2OfList(UnboundDefault,LIST,LISTO):-maplist_safe(arg2(UnboundDefault),LIST,LISTO),!.
arg2(_UnboundDefault,_=Value,Value):-!.
arg2(_UnboundDefault,Value,Value):-!,trace.

makeAimlCate2(_Ctx,LIST,UnboundDefault,Value):- arg2OfList(UnboundDefault,LIST,LISTO), Value =.. [aimlCate|LISTO],!.


translate_cate(Ctx,CateSig):-replaceArgsVar(Ctx,[srcinfo],CateSig,_),immediateCall(Ctx,assert_cate_in_load(CateSig)).
asserta_cate(Ctx,CateSig):-prolog_must(ground(CateSig)),assert_cate_in_load(CateSig),immediateCall(Ctx,assert_cate_in_load(CateSig)).

is_xml_missing(Var):-var(Var),!.
is_xml_missing([]).

isRetraction(Ctx,CateSig,OF):-getCategoryArg(Ctx,'template',NULL,_Out_,CateSig),is_xml_missing(NULL),!,
   replaceArgsVar(Ctx,['template',srcinfo,srcfile],CateSig,_),
   OF=CateSig.



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


evalSRAI(Ctx,Votes,SraiDepth,ATTRIBS,_Input,_Unusued,_VotesO):- SraiDepth>200,
  getAliceMem(Ctx,bot,'infinite-loop-input',Output),!,VotesO is Votes * 0.8,
  throw_aiml_goto(element(srai,ATTRIBS,Output),VotesO).
  %%throw_aiml_goto(proof(element(template,ATTRIBS,[element(srai,ATTRIBS,Output)]),loop(sraiDepth,SraiDepth,200,ATTRIBS,Input)),VotesO).

/*
evalSRAI(Ctx,Votes,_SraiDepth,ATTRIBS,Input,_Unusued,_VotesO):-
 frame_depth(Depth),Depth>3000,getAliceMem(Ctx,bot,'infinite-loop-input',Output),!,VotesO is Votes * 0.8,
 throw_aiml_goto(proof(element(template,ATTRIBS,[element(srai,ATTRIBS,Output)]),loop(frameDepth,Depth,3000,ATTRIBS,Input)),VotesO).
*/

evalSRAI(Ctx,Votes,_SraiDepth,ATTRIBS,[I|Input0],Output,VotesO):-atom(I),atom_prefix(I,'@'),!,
  % re-direct to input
  withAttributes(Ctx,ATTRIBS,prolog_must(computeAnswer(Ctx,Votes,element(system,ATTRIBS,[I|Input0]),Output,VotesO))),!.

evalSRAI(Ctx,Votes,_SraiDepth,ATTRIBS,Input,Output,VotesO):-
 ifThen(var(SYM),evalsrai(SYM)),
 var(Proof), 
   withAttributes(Ctx,['evalsrai'=SYM,proof=Proof],
  ((
    debugOnError(computeSRAI(Ctx,Votes,SYM,Input,MidIn,VotesM,Proof)),      
    computeSRAIStars(Ctx,ATTRIBS,Input,MidIn,VotesM,SYM,Proof,Output,VotesO),!, 
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
      setCtxValue('evalsrai',Ctx,SYM),
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

computeSRAI(_Ctx,_Votes,_SYM,[],_,_,_Proof):- !,trace,fail.

computeSRAI(Ctx,Votes,SYM,Input,Result,VotesO,Proof):-
   getAliceMem(Ctx,'bot','me',Robot),
   getAliceMem(Ctx,'bot','you',User),
   ifThen(var(SYM),evalsrai(SYM)),
   getConversationThread(Ctx,User,Robot,ConvThread),
   prolog_must(computeSRAI0(Ctx,Votes,ConvThread,SYM,Input,Result,VotesO,Proof)).

getConversationThread(Ctx,User,Robot,ConvThread):-
   ConvThread = fromTo(User,Robot),
   setCtxValue('convthread',Ctx,ConvThread),!.

computeSRAI0(Ctx,Votes,ConvThread,SYM,Input,Result,VotesO,Proof):-   
   computeInnerTemplate(Ctx,Votes,Input,NewIn,VotesM),NewIn \== Input,!,
   computeSRAI0(Ctx,VotesM,ConvThread,SYM,NewIn,Result,VotesO,Proof),!.

computeSRAI0(Ctx,Votes,ConvThread,SYM,Input,Result,VotesO,Proof):- not(is_list(Input)),compound(Input),
   answerOutput(Input,InputO),Input\==InputO,!,trace,
   computeSRAI0(Ctx,Votes,ConvThread,SYM,InputO,Result,VotesO,Proof).

computeSRAI0(Ctx,Votes,ConvThread,SYM,Input,Result,VotesO,Proof):- not(is_list(Input)),compound(Input),
   computeAnswer(Ctx,Votes,Input,InputO,VotesM),Input\==InputO,!,trace,
   computeSRAI0(Ctx,VotesM,ConvThread,SYM,InputO,Result,VotesO,Proof).

computeSRAI0(Ctx,Votes,ConvThread,SYM,Input,Result,VotesO,Proof):-
  Each = (MatchLevel - e(VotesM,Result,Proof)), %% VotesO make it sort/2-able
  Call = computeSRAI2(Ctx,Votes,ConvThread,SYM,Input,Result,VotesM,Proof,MatchLevel),
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

% now trace is ok

% this next line is what it does on fallback
computeSRAI0(Ctx,Votes,ConvThread,SYM,[B|Flat],[B|Result],VotesO,Proof):- fail,
   computeSRAI2(Ctx,Votes,ConvThread,SYM,Flat,Result,VotesO,Proof,_PostMatchLevel3),prolog_must(nonvar(Result)).

checkSym(_SYM).

subclassMakeUserDict(Ctx,UserDict,SYM):-debugFmt(subclassMakeUserDict(Ctx,UserDict,SYM)),!.

convThreadDict(_Ctx,ConvThreadHint,ConvThread):-answerOutput(ConvThreadHint,First),unlistify(First,ConvThread),!.

computeSRAI222(CtxIn,Votes,ConvThreadHint,SYM,Pattern,Compute,VotesO,ProofOut,OutputLevel):-    
   %%convertToMatchable(Pattern,InputPattern),
   prolog_must(getCtxValue('evalsrai',CtxIn,SYM2)),
   ifThen(var(SYM),SYM=SYM2),
   ifThen(SYM\==SYM2,debugFmt(syms(SYM\==SYM2))),
      convThreadDict(Ctx,ConvThreadHint,ConvThread),
         getCategoryArg(Ctx,'template',Out, _Out_ ,CateSig),!,
         getAliceMemOrSetDefault(CtxIn,ConvThread,SYM,'topic',Topic,['Nothing']),
         getAliceMemOrSetDefault(CtxIn,ConvThread,SYM,'userdict',UserDict,'user'), 
         %%getAliceMemOrSetDefault(CtxIn,ConvThread,SYM,'convthread',ConvThread,SYM,ConvThreadHint), 
         subclassMakeUserDict(CtxIn,UserDict,SYM),
         getAliceMemOrSetDefault(CtxIn,ConvThread,SYM,'that',That,['Nothing']),
  
   PreTopic = (CtxIn=Ctx),
   debugFmt(topicThatPattern(Topic,That,Pattern)),!,
   must_be_openCate(CateSig),
   prolog_must(topicThatPattern(Ctx,Topic,That,Pattern,PreTopic,Out,CateSig,OutputLevel,StarSets_All,ClauseNumber,CAfterPattern)),
         once((
            retractallSrais(SYM),
            prolog_must(CAfterPattern),
            prolog_must(nonvar(Out)),
            cateStrength(CateSig,Mult),
           %% not(contextUsedClaused(Ctx,CateSig,ClauseNumber)),
            VotesO is Votes * Mult,

            makeWithAttributes(StarSets_All,Out,Compute),       
            ProofOut=..[proof,Compute,cn(ClauseNumber),CateSig])).



savedParts(Save,PreTopic,CAfterPattern,OutputLevel,StarSets_All,Out,ClauseNumber,CateSig):-
      Save = OutputLevel - StarSets_All - Out - ClauseNumber - CateSig - CAfterPattern - PreTopic.

starSetsAll(Ctx,Topic,That,Pattern,Save,PreTopic):-
   savedParts(Save,PreTopic,_CAfterPattern,OutputLevel,StarSets_All,Out,ClauseNumber,CateSig),
   getCategoryArg(Ctx,'template',Out, _Out_ ,CateSig),
   OutputLevel = OutputLevel1 - OutputLevel2 - OutputLevel3,!,
   %%%%% Iterate here %%%%
   CateSig,
   clause(CateSig,true,ClauseNumber), %%%%%
   once(( cate_match(Ctx,'topic',Topic,CateSig,StarSets_Topic,OutputLevel1),
   cate_match(Ctx,'that',That,CateSig,StarSets_That,OutputLevel2),
   cate_match(Ctx,'pattern',Pattern,CateSig,StarSets_Pattern,OutputLevel3),
   combineStarSets(StarSets_Topic,StarSets_That,StarSets_Pattern,StarSets_All) )).
   

combineStarSets(StarSets_Topic,StarSets_That,StarSets_Pattern,StarSets_All):-
   append(StarSets_Topic,StarSets_That,StarSets_TopicThat),
   append(StarSets_Pattern,StarSets_TopicThat,StarSets_All),!.

cate_match(Ctx,StarName,TextPattern,CateSig,StarSets,MatchLevel):-
    getCategoryArg1(Ctx,StarName,MatchPattern,_StarNumber,CateSig),
    make_star_binders(Ctx,StarName,1,TextPattern,MatchPattern,MatchLevelInv,StarSets),MatchLevel is 1/MatchLevelInv,!.

%% simpler but slower.. maybe comment (fail) this one out for the faster next one
topicThatPattern(Ctx,Topic,That,Pattern,PreTopic,Out,CateSig,OutputLevel,StarSets_All,ClauseNumber,CAfterPattern):-
  %% not(member('STDCATCHALL',Pattern)),
   CAfterPattern = (CateSig,prolog_must(PreTopic)),
   savedParts(Save,PreTopic,CAfterPattern,OutputLevel,StarSets_All,Out,ClauseNumber,CateSig),
   findall(Save,starSetsAll(Ctx,Topic,That,Pattern,Save,PreTopic),AllCateSig),AllCateSig=[_|_],
   sort(AllCateSig,SetOfAllCateSig),!,
   %%%%% Iterate here %%%%
   member(Save,SetOfAllCateSig).
   
%% WILL GET HERE ONLY I NEW ROUTINES ARE BROKEN
topicThatPattern(Ctx,Topic,That,Pattern,PreTopic,Out,CateSig,OutputLevel,StarSets_All,ClauseNumber,CAfterPattern):- fail,
   debugFmt(debugWarnFallback(topicThatPattern2)), %% trace, 
   CPreTopic = true,
   make_preconds_for_match(Ctx,'topic',Topic,CateSig,PreTopic,AfterTopic, CPreTopic,CAfterTopic, Out,MinedCates,StarSets_Topic,OutputLevel1),
   make_preconds_for_match(Ctx,'that',That,CateSig,AfterTopic,AfterThat,CAfterTopic,CAfterThat,Out,MinedCates,StarSets_That,OutputLevel2),
   make_preconds_for_match(Ctx,'pattern',Pattern,CateSig,AfterThat,AfterPattern,CAfterThat,CAfterPattern,Out,MinedCates,StarSets_Pattern,OutputLevel3),!,
   prolog_must(var(Out)),
   must_be_openCate(CateSig),
   OutputLevel = OutputLevel1 - OutputLevel2 - OutputLevel3,!, 
   %%%%% Iterate here %%%%
   prolog_must(atLeastOne((AfterPattern,CateSig))),
   clause(CateSig,true,ClauseNumber),
   combineStarSets(StarSets_Topic,StarSets_That,StarSets_Pattern,StarSets_All).


contextUsedClaused(Ctx,CateSig,ClauseNumber):- fail, contains_term(Ctx,CateSig)->not(contains_term(Ctx,ClauseNumber));not(contains_term(Ctx,ClauseNumber)).

makeWithAttributes([],Proof,Proof):-!.
makeWithAttributes(StarSets_All,Proof,withAttributes(StarSets_All,Proof)).

retractallSrais(SYM):-prolog_must(nonvar(SYM)),ifThen(nonvar(SYM),(retractall(dict(SYM,_,_)))),fail.
retractallSrais(_SYM):-!.

cateStrength(_CateSig,1.1):-!.

computeSRAI2(Ctx,Votes,ConvThread,_SYM1,Pattern,Out,VotesO,ProofOut,MatchLevel):- !, %% avoid next one    
    computeSRAI222(Ctx,Votes,ConvThread,_SYM2,Pattern,Out,VotesO,ProofOut,MatchLevel).

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

savedSetPatterns(LSP,MatchLevel,StarSets,MatchPattern):- LSP = MatchLevel+StarSets+MatchPattern.

make_preconds_for_match(Ctx,StarName,InputNothing,CateSig,PrecondsSearch,PostcondsSearch,PrecondsCommit,PostcondsCommit,Out,MinedCates,ProofOut,
 OutputLevel):-
   make_prepost_conds(Ctx,StarName,InputNothing,CateSig,FindPattern,CommitResult,Out,MinedCates,ProofOut,OutputLevel),
   combineConjCall(PrecondsSearch,FindPattern,PostcondsSearch),
   combineConjCall(PrecondsCommit,CommitResult,PostcondsCommit).

combineConjCall(A,B,C):-A==true,!,C=B.
combineConjCall(A,B,C):-B==true,!,C=A.
combineConjCall(A,B,C):- C = (A,B).

make_prepost_conds(Ctx,StarName,TextPattern,CateSig,FindPattern,CommitPattern,Out,MinedCates,ProofOut,MatchLevel):- 
  hotrace(meansNothing(TextPattern,InputPattern)), 
   TextPattern \= InputPattern,!,
  make_prepost_conds(Ctx,StarName,InputPattern,CateSig,FindPattern,CommitPattern,Out,MinedCates,ProofOut,MatchLevel).

make_prepost_conds(Ctx,StarName,TextPattern,CateSig,FindPattern,CommitPattern,Out,MinedCates,ProofOut,MatchLevel):-
  generateMatchPatterns(Ctx,StarName,Out,TextPattern,CateSig,MinedCates,EachMatchSig),!,
  prolog_must(EachMatchSig=[_|_]),
  savedSetPatterns(LSP,MatchLevel,_StarSets,MatchPattern),
  getCategoryArg(Ctx,StarName,MatchPattern,Out,CateSig),
  FindPattern = 
      ((
        member(LSP,EachMatchSig),           
           prolog_must(make_star_binders(Ctx,StarName,1,TextPattern,MatchPattern,MatchLevel2,ProofOut)),
           (CommitPattern = ignore(MatchLevel2 = MatchLevel)))).         

notSingletons(_Singleton_List):-!.

generateMatchPatterns(Ctx,StarName,Out,InputNothing,CateSig,NC_MinedCates,EachMatchSig):-
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

generateMatchPatterns(Ctx,StarName,Out,InputPattern,CateSig,_MinedCates,EachMatchSig):-
 %% convertToMatchable(TextPattern,InputPattern),
  must_be_openCate(CateSig),
  copy_term(CateSig,CateSigC),!,
  getCategoryArg(Ctx,StarName,MatchPattern,Out,CateSigC),
  findall(MatchPattern,CateSigC,AllMatchSig),!,sort(AllMatchSig,SetOfEachMatchSig),!,
  savedSetPatterns(LSP,MatchLevel,StarSets,MatchPattern),
  findall(LSP,
             (member(MatchPattern,SetOfEachMatchSig), 
              canMatchAtAll_debug(Ctx,StarName,InputPattern,MatchPattern,MatchLevel,StarSets)),
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
%%make_star_binders(Ctx,_StarName,[_],[_,_|_]):-!,fail.
%%make_star_binders(Ctx,StarName,A,B):-make_star_binders(Ctx,StarName,A,B,_MatchLevel,_StarSets),!.    
% ========================================================================================

canMatchAtAll_debug(Ctx,StarName,InputPattern,MatchPattern,MatchLevel,StarSets):- 
    make_star_binders(Ctx,StarName,1,InputPattern,MatchPattern,MatchLevelInv,StarSets),!,MatchLevel is 1/MatchLevelInv ,
    nop(debugFmt(pass_canMatchAtAll_debug(Ctx,StarName,InputPattern,MatchPattern,MatchLevel,StarSets))),!.

canMatchAtAll_debug(Ctx,StarName,InputPattern,MatchPattern,_MatchLevel,_StarSets):-
    nop(debugFmt(fail_canMatchAtAll_debug(Ctx,StarName,InputPattern,MatchPattern))),!,fail.

% skip over skipable words
consumeSkippables([],[]).
consumeSkippables([Skipable|B],BB):- isIgnoreableWord(Skipable),!,consumeSkippables(B,BB).
consumeSkippables(A,A).


make_star_binders(_Ctx,StarName,_N,InputPattern,MatchPattern,MatchLevel,StarSets):- 
   prolog_must(var(StarSets)),prolog_must(var(MatchLevel)),prolog_must(ground(StarName:InputPattern:MatchPattern)),fail.  

:-setLogLevel(make_star_binders,none).

%end check
make_star_binders(_Ctx,_StarName,_N,L,R,1,[]):-R==[],!,consumeSkippables(L,LL),!,LL==[].
make_star_binders(_Ctx,_StarName,_N,L,R,1,[]):-L==[],!,consumeSkippables(R,RR),!,RR==[].

% left hand star/wild  (cannot really happen (i hope))
%make_star_binders(_Ctx,StarName,_N,Star,_Match,_MatchLevel,_StarSets):- fail, not([StarName]=Star),isStarOrWild(StarName,Star,_WildValue,_WMatch,_Pred),!,trace,fail. 


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

% skip over skippable words
make_star_binders(Ctx,StarName,N,[Skipable|B],BB,CountO,StarSets):- isIgnoreableWord(Skipable),!,make_star_binders(Ctx,StarName,N,B,BB,Count,StarSets),CountO is Count + 1.

isIgnoreableWord(Skipable):-member(Skipable,['-','\n','(',')',',','?','.']).
%
% re-write section
%
%
/*
make_star_binders(Ctx,StarName,InputNothing,MatchPattern,MatchLevel,StarSets):- 
   hotrace((InputNothing \== '*',(InputPattern==StarName ; meansNothing(InputNothing,InputPattern)))),!, trace,
   make_star_binders(Ctx,StarName,['Nothing'],MatchPattern,MatchLevel,StarSets).


% must come before search failures
make_star_binders(Ctx,StarName,TextPattern,MatchPattern,MatchLevel,StarSets):- fail,
  hotrace(((convertToMatchable(TextPattern,InputPattern),TextPattern \== InputPattern))),!,
  make_star_binders(Ctx,StarName,InputPattern,MatchPattern,MatchLevel,StarSets),!,trace.

% fast veto
make_star_binders(_Ctx,StarName,[I0|Pattern],[Match|MPattern],_MatchLevel,_Commit):-
   member(M,[Match|MPattern]),requireableWord(StarName,M),not(member(M,[I0|Pattern])),!,fail.

% fast veto
make_star_binders(_Ctx,_StarName,[_],[_,_|_],_NoNum,_NoCommit):-!,fail.


make_star_binders(_Ctx,StarName,[E|More],Match,Value,[tryLater([E|More],Match)]):-compound(E),trace,isWildCard(StarName,E,Value),!.

% weird atom
%%make_star_binders(_Ctx,StarName,I,Atom,12,[Atom=I]):-atom(Atom),trace,!,loggerFmt(make_star_binders,canMatchAtAll_atom(StarName,I,Atom)),!.

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

isOptionalOrStar(_StarName,M):-not(atom(M)),!,trace.
isOptionalOrStar(StarName,M):-isStar(StarName,M),!.

/*
isStar(StarName,'topic'):-!. %%,trace.
isStar(StarName,'that'):-!.
isStar(StarName,'input'):-!.
*/
isStar(StarName,StarNameText):-isStar(StarName,StarNameText,_Order),!.
isStar(StarName,StarNameText,WildValue):-not(ground(StarNameText)),trace,debugFmt(isStar(StarName,StarNameText,WildValue)),!,fail.
isStar(StarName,[StarNameText],WildValue):-isStar(StarName,StarNameText,WildValue),!.
isStar(_StarName,'*',0.3).
isStar(_StarName,'_',0.8).
%%WAS VERY BAD IDEA:  isStar(StarName,StarNameText,6):-atom(StarName),!,StarNameText==StarName,writeq(qqqq-qq),trace.


must_be_openCate(_CateSig):-!.
must_be_openCate(CateSig):- prolog_must(hotrace((((nonvar(CateSig),not(ground(CateSig)),must_be_openCate0(CateSig)))))),!.
must_be_openCate0(CateSig):- arg(_,CateSig,Arg),must_be_openCateArgs(Arg,CateSig),fail.
must_be_openCate0(_CateSig):-!.

must_be_openCateArgs(Arg,_CateSig):-var(Arg),!.
must_be_openCateArgs('*',_CateSig):-!.
must_be_openCateArgs(List,CateSig):-trace, throw(List:CateSig),!.

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
   (traceIf(isStarValue(Pattern))),
   getDictFromAttributes(Ctx,'evalsrai',[],Dict),
   prolog_must(Dict\==user),
   atom_concat(StarName,N,StarNameN),
   prolog_must(not((getAliceMemComplete(Ctx,Dict,StarNameN,Old),debugFmt(getAliceMemComplete(Ctx,Dict,StarNameN,Old))))),
   setAliceMem(Ctx,Dict,StarNameN,Pattern),!,star_flag(StarName,NN,NN+1).

%%REAL-UNUSED  set_matchit1(StarName,Pattern,Matcher,OnBind):- length(Pattern,MaxLen0), MaxLen is MaxLen0 + 2,
%%REAL-UNUSED    set_matchit2(StarName,Pattern,Matcher,MaxLen,OnBind).

isStar0(Word1):- member(Word1,[*,'_']).
sameWords(Word1,Word2):-atom(Word1),atom(Word2),atoms_match0(Word1,Word2).
 atoms_match0(Word1,Word2):- (isStar0(Word1);isStar0(Word2)),!,fail.
 atoms_match0(Word1,Word1):-!.
 atoms_match0(Word1,Word2):-litteral_atom(Word1,WordO),litteral_atom(Word2,WordO),!.

