% ===================================================================
% File 'logicmoo_module_aiml.pl'
% Purpose: An Implementation in SWI-Prolog of AIML
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'logicmoo_module_aiml.pl' 1.0.0
% Revision:  $Revision: 1.7 $
% Revised At:   $Date: 2002/07/11 21:57:28 $
% ===================================================================

%:-module()
%:-include('logicmoo_utils_header.pl'). %<?
%:- style_check(-singleton).
%%:- style_check(-discontiguous).
:- style_check(-atom).
:- style_check(-string).


:-multifile(what/3).
:-multifile(response/2).
:-dynamic(dict/3).
:-multifile(dict/3).

:-dynamic(lineInfoElement/4).

%% ======================================================
%% Add a library directory 2 levels above this file
%% ======================================================
asserta_if_new_hlper1(C):-catch(C,_,fail),!.
asserta_if_new_hlper1(C):-asserta(C),!.
:-source_location(File,_Line),file_directory_name(File, Directory),
   file_directory_name(Directory,ParentDir),
   writeq(logicmoo_module_aiml:asserta(library_directory(ParentDir))),
   asserta_if_new_hlper1(library_directory(ParentDir)).

:-ensure_loaded(library('programk/logicmoo_module_aiml_shared.pl')).
:-ensure_loaded(library('programk/logicmoo_module_aiml_xpath.pl')).
:-ensure_loaded(library('programk/logicmoo_module_aiml_loader.pl')).
:-ensure_loaded(library('programk/logicmoo_module_aiml_convertor.pl')).
:-ensure_loaded(library('programk/logicmoo_module_aiml_eval.pl')).
%%:-ensure_loaded(library('notaiml/tokenize.pl')).
:-dynamic(local_directory_search/1).
:-multifile(local_directory_search/1).
:-module_transparent(local_directory_search/1).

local_directory_search('cynd').
local_directory_search('cynd/programk').
local_directory_search('programk').
local_directory_search('programk/test_suite').
local_directory_search('../aiml').
local_directory_search('aiml').

local_directory_search_combined2(PL):-local_directory_search(A),local_directory_search(B),join_path(A,B,PL),exists_directory_safe(PL).

local_directory_search_combined(X):-local_directory_search(X).
local_directory_search_combined(X):-local_directory_search_combined2(X).
%% for now dont do the concat 3 version
%% local_directory_search_combined(PL):-local_directory_search_combined2(A),local_directory_search(B),join_path(A,B,PL),exists_directory_safe(PL).

run_chat_tests:-
   test_call(alicebot('Hi')),
   test_call(alicebot('What is your name')),
   test_call(alicebot('My name is Fred.')),
   test_call(alicebot('what is my name?')).

test_call(G):-writeln(G),ignore(once(catch(G,E,writeln(E)))).


main_loop1(Atom):- current_input(In),!,
            read_line_to_codes(In,Codes),!,
            atom_codes(Atom,Codes),!,
            alicebot(Atom),!.

main_loop:-repeat,main_loop1(_),fail.

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

:-dynamic(default_channel/1).
:-dynamic(default_user/1).

%default_channel( "#logicmoo").
%default_user(    "default_user").         

% say(Say):-writeq(Say),nl.

% ===============================================================================================
% ALICE IN PROLOG
% ===============================================================================================

say(X):-currentContext(say(X),Ctx),say(Ctx,X),!.
say(Ctx,X):- aiml_eval(Ctx,X,Y),!,answerOutput(Y,O),showTransition(O,Y,Trans),debugFmt(say(Trans)),!.

showTransition(O,Y,O):-O==Y,!.
showTransition(O,Y,O=Y).

alicebot:-repeat,
	read_line_with_nl(user,CodesIn,[]),        
        makeAimlContext(alicebot,Ctx),
        once((trim(CodesIn,Codes),atom_codes(Atom,Codes),alicebotCTX(Ctx,Atom))),fail.

alicebot(Input):- currentContext(alicebot(Input),Ctx),!,alicebotCTX(Ctx,Input).

% ===============================================================================================
% Main Alice 
% ===============================================================================================
alicebotCTX(_Ctx,Input):- atom(Input),catch(((atom_to_term(Input,Term,Vars),callInteractive0(Term,Vars))),_,fail),!.
alicebotCTX(Ctx,Input):- alicebotCTX(Ctx,Input,Resp),!,say(Ctx,Resp),!.
%%alicebotCTX(Ctx,_):- trace, say(Ctx,'-no response-').


alicebotCTX(_Ctx,[],_):-debugFmt('no input'),!,fail.
alicebotCTX(Ctx,Input,Resp):- atom(Input),!,
      atomSplit(Input,TokensO),!,Tokens=TokensO,
      alicebotCTX(Ctx,Tokens,Resp),!.
alicebotCTX(Ctx,[TOK|Tokens],Output):- atom(TOK),atom_concat_safe('@',_,TOK),!,systemCall(Ctx,'bot',[TOK|Tokens],Output),debugFmt(Output).
alicebotCTX(Ctx,Tokens,Resp):-
   %%convertToMatchable(Tokens,UCase),!,
   =(Tokens,UCase),!,
   removePMark(UCase,Atoms),!,
   alicebot2(Ctx,Atoms,Resp),!.
alicebotCTX(_Ctx,In,Res):- !,ignore(Res='-no response-'(In)).


% ===============================================================================================
% Main Alice 
% ===============================================================================================
%%alicebot2(Atoms,Resp):- currentContext(alicebot2(Atoms),Ctx),!,alicebot2(Ctx,Atoms,Resp).

alicebot2(_Ctx,[],[]):-!.
alicebot2(Ctx,[''|X],Resp):-!,alicebot2(Ctx,X,Resp).
alicebot2(Ctx,Atoms,Resp):-	
   retractall(posibleResponse(_,_)),
   flag(a_answers,_,0),!,
   prolog_must((
   getAliceMem(Ctx,'bot','you',User),
   getAliceMem(Ctx,'bot','me',_Robot),
   getAliceMem(Ctx,'bot',default('minanswers',1),MinAns),
   getAliceMem(Ctx,'bot',default('maxanswers',1),_MaxAns),
   %%setAliceMem(Ctx,User,'input',Atoms),
   pushInto1DAnd2DArray(Ctx,'request','input',10,Atoms,User),
   setAliceMem(Ctx,User,'rawinput',Atoms))),
   ((call_with_depth_limit_traceable(computeInputOutput(Ctx,1,Atoms,Output,N),8000,_DL),
	 ignore((nonvar(N),nonvar(Output),savePosibleResponse(N,Output))),flag(a_answers,X,X+1),
                X<MinAns)),!,
   findall(NR-OR,posibleResponse(NR,OR),L),!,
   (format('~n-> ~w~n',[L])),
   sort(L,S),
   dumpList(S),
   reverse(S,[Resp|_RR]),
   degrade(Resp),
   rememberSaidIt(Ctx,Resp),!.

% ===============================================================================================
% Call like a SRAI tag
% ===============================================================================================
computeInputOutput(Ctx,VoteIn,Input,Output,VotesOut):- prolog_must(computeAnswer(Ctx,VoteIn,element(srai,[],Input),Output,VotesOut)),!.

% ===============================================================================================
% Save Possible Responses (Degrade them as well)
% ===============================================================================================
:-dynamic(posibleResponse/2).

savePosibleResponse(_N,Output):-posibleResponse(_,Output),!.
savePosibleResponse(N,Output):-
   findall(1,degraded(Output),L),!,
   length(L,K),
   SN is N - (K * 0.6)  , !,
   asserta(posibleResponse(SN,Output)).


% ===============================================================================================
% Eval a SRAI
% ===============================================================================================
evalSRAI(Ctx,Votes,ATTRIBS,[I|Input0],Output,VotesO):-atom(I),atom_prefix(I,'@'),!,
  % re-direct to input
  withAttributes(Ctx,ATTRIBS,prolog_must(computeAnswer(Ctx,Votes,[I|Input0],Output,VotesO))),!.

evalSRAI(Ctx,Votes,ATTRIBS,Input0,Output,VotesO):-
  prolog_must(ground(Input0)),!,flatten([Input0],Input),  
  evalsrai(SYM),
  var(Proof),
 withAttributes(Ctx,ATTRIBS,
   withAttributes(Ctx,[evalsrai=SYM],
  ( 
    debugOnError(computeSRAI(Ctx,Votes,Input,MidIn,VotesM,Proof)),    
     withAttributes(Ctx,[evalsrai=SYM,proof=Proof],
      computeSRAIStars(Ctx,ATTRIBS,Input,MidIn,VotesM,SYM,Proof,Output,VotesO))))).

computeSRAIStars(Ctx,ATTRIBS,Input,MidIn,VotesM,SYM,Proof,Output,VotesO):-
    prolog_must((nonvar(MidIn),
                 nonvar(SYM),
                 singletons([Ctx,ATTRIBS]),
                 nonvar(Proof))),
      %% Proof = Output, 
      MidIn = Output, 
      VotesM = VotesO,
      debugFmt(computeSRAIStars(SYM,Input,Output)),
      %%%trace,
      prolog_must((ground(Output),number(VotesO))),!.

computeSRAIStars(Ctx,ATTRIBS,Input,MidIn,VotesM,SYM,Proof,Output,VotesO):-
    prolog_must((nonvar(MidIn),
                 nonvar(SYM),
                 nonvar(Proof))),
      debugFmt(computeSRAIStars(SYM,Input,MidIn)),
      computeElementMust(Ctx,VotesM,template,ATTRIBS,MidIn,MidIn9,VotesI9),
      prolog_must(answerOutput(MidIn9,Mid9)),
      debugFmt(evalSRAI(Input,MidIn,MidIn9,Mid9)),
      prolog_must(computeAnswer(Ctx,VotesI9,Mid9,Output,VotesO)),
      prolog_must((ground(Output),number(VotesO))),!.

 evalsrai(SYM):-gensym(evalsrai,SYM).
% ===============================================================================================
% Expand Answers
% ===============================================================================================
flatten_if_list(List,Flat):-is_list(List),!,flatten(List,Flat),!.

computeTemplate(Ctx,Votes,Input,Output,VotesO):-flatten_if_list(Input,Flat),!,computeTemplate0(Ctx,Votes,Flat,Output,VotesO).
computeTemplate(Ctx,Votes,Input,Output,VotesO):-computeTemplate0(Ctx,Votes,Input,Output,VotesO).

computeTemplate0(Ctx,Votes,Input,Output,VotesO):-computeTemplate1(Ctx,Votes,Input,Output,VotesO),!.
computeTemplate0(Ctx,Votes,Input,Output,VotesO):-trace,computeTemplate1(Ctx,Votes,Input,Output,VotesO),!.

computeTemplate1(_Ctx,Votes,In,Out,VotesO):-In==[],!,prolog_must((In=Out,Votes=VotesO)).
computeTemplate1(Ctx,Votes,IN,Out,VotesO):-IN=[I|N],!,computeTemplate11(Ctx,Votes,[I|N],Out,VotesO).
computeTemplate1(Ctx,VotesM,In,Out,VotesM):-traceAIML,expandVar(Ctx,In,Out),!.
computeTemplate1(_Ctx,Votes,In,Out,VotesO):-prolog_must((In=Out,Votes=VotesO)).

computeTemplate11(_Ctx,Votes,In,Out,VotesO):-In==[],!,prolog_must((In=Out,Votes=VotesO)).
computeTemplate11(Ctx,Votes,[<,BR,/,>|B],OO,VotesO):-atom(BR),!,
   computeTemplate11(Ctx,Votes,[element(BR,[],[])|B],OO,VotesO).
computeTemplate11(Ctx,Votes,[A|B],OO,VotesO):-expandVar(Ctx,A,AA),!,computeTemplate11(Ctx,Votes,B,BB,VotesO),once(flatten([AA,BB],OO)).
computeTemplate11(Ctx,Votes,[A|B],[A|BB],VotesO):-atomic(A),!,computeTemplate11(Ctx,Votes,B,BB,VotesO).


traceAIML:-!.

expandVar(_Ctx,Var,Var):-var(Var),!,traceAIML.
expandVar(_Ctx,[Var|_],Var):- !,trace,traceAIML.
expandVar(Ctx,In,Out):-atom(In),atom_concat('$',NameVar,In),!,expandVariable(Ctx,NameVar,Out),!.
expandVar(_Ctx,In,Out):-atomic(In),Out=In,!.
expandVar(Ctx,element(A,B,C),Out):-computeElementMust(Ctx,1,A,B,C,Out,_VotesO),!.
expandVar(Ctx,In,Out):-computeAnswerMaybe(Ctx,1,In,Out,_VotesO),!.
expandVar(_Ctx,In,Out):-trace,Out=In,!.

expandVariable(Ctx,In,Out):-atom(In),atom_concat('$',NameVar,In),!,expandVariable(Ctx,NameVar,Out),!.
expandVariable(Ctx,In,Out):-atom(In),atom_concat(NameVar,'$',In),!,expandVariable(Ctx,NameVar,Out),!.
expandVariable(Ctx,name=Name,Result):-!,expandVariable(Ctx,Name,Result),!.

expandVariable(_Ctx,nick,A):-!,default_user(B),!,from_atom_codes(A,B),!.
expandVariable(_Ctx,person,A):-!,default_user(B),!,from_atom_codes(A,B),!.
expandVariable(_Ctx,botnick,'jllykifsh'):-!.
expandVariable(_Ctx,mynick,'jllykifsh'):-!.
expandVariable(_Ctx,version,[push_nobrkspace,'1','.','0','.','1',pop_nobrkspace]):-!.
expandVariable(_Ctx,id,'$botnick$'):-!.
expandVariable(_Ctx,size,Size):-aimlCateSig(X),predicate_property(X,number_of_clauses(Size)),!.
%TODO extract the machine TimeZone
expandVariable(_Ctx,date,[Y,'-',M,'-',D]):-get_time(TimeStamp),stamp_date_time(TimeStamp,DateTime,'UTC'),date_time_value(date,DateTime,date(Y,M,D)),!.
expandVariable(_Ctx,mychan,A):-!,default_channel(B),!,from_atom_codes(A,B),!.
expandVariable(Ctx,NameVar,Result):-getAliceMem(Ctx,'bot',NameVar,Result),!.
expandVariable(Ctx,NameVar,Result):-getAliceMem(Ctx,'global',NameVar,Result),!.


globalAliceTagVar(BOT_ATOM):-member(BOT_ATOM,[version,id,favfood,date,size]).


from_atom_codes(Atom,Atom):-atom(Atom),!.
from_atom_codes(Atom,Codes):-convert_to_string(Codes,Atom),!.
from_atom_codes(Atom,Codes):-atom_codes(Atom,Codes).


:-dynamic(recursiveTag/1).


notRecursiveTag(system).
%%notRecursiveTag(template).
notRecursiveTag(condition).
notRecursiveTag(Loader):-loaderTag(Loader).
notRecursiveTag(li).

loaderTag(Loader):-member(Loader,[aiml,topic,category,learn,load]).

recursiveTag(random).
recursiveTag(srai).
recursiveTag(NoRec):-notRecursiveTag(NoRec),!,fail.

recursiveTag(_).
isAimlTag(result):-!,fail.
isAimlTag(proof):-!,fail.
isAimlTag(get).
isAimlTag(_).

prolog_mostly_ground(Out):-ground(Out),!.
prolog_mostly_ground(Out):-prolog_must(nonvar(Out)),!.

computeInner(_Ctx, _Votes, In, Out) :- atom(In),!,Out=In.
computeInner(Ctx,Votes, In, Out) :- not(Out=(_-_)),!,computeAnswer(Ctx,Votes, In, Out, _VoteMid),!.
computeInner(Ctx,Votes, In, VoteMid-Out) :-  computeAnswer(Ctx,Votes, In, Out, VoteMid),!,prolog_must(nonvar(Out)),prolog_must(nonvar(VoteMid)).

computeInnerEach(_Ctx, _Votes, In, Out) :- atom(In), !, Out=In , prolog_mostly_ground(Out).

computeInnerEach(Ctx, _Votes, element(eval,ATTRIBS,INNER_XML),Rendered):-!,
   withAttributes(Ctx,ATTRIBS,aiml_eval_each(Ctx,INNER_XML,Rendered)),!.

computeInnerEach(  Ctx, Votes, In, Out) :- debugOnFailureAiml(computeAnswer(Ctx,Votes, In, Out, _VoteMid)),!, prolog_mostly_ground(Out).
computeInnerEach(_Ctx, _Votes, In, Out) :- !, Out=In,  prolog_mostly_ground((Out)).


% ===============================================================================================
% Compute Answer Element Probilities
% ===============================================================================================
computeElementMust(Ctx,Votes,Tag,Attribs,InnerXml,Resp,VotesO):-computeElement(Ctx,Votes,Tag,Attribs,InnerXml,Resp,VotesO),!.
computeElementMust(Ctx,Votes,Tag,Attribs,InnerXml,Resp,VotesO):-trace,computeElement(Ctx,Votes,Tag,Attribs,InnerXml,Resp,VotesO),!.

computeAnswerMaybe(Ctx,Votes,element(Tag,Attribs,InnerXml),Output,VotesO):-!,computeElement(Ctx,Votes,Tag,Attribs,InnerXml,Output,VotesO),!.
computeAnswerMaybe(Ctx,Votes,InnerXml,Resp,VotesO):-computeAnswer(Ctx,Votes,InnerXml,Resp,VotesO),!.
computeAnswerMaybe(Ctx,Votes,InnerXml,Resp,VotesO):-debugFmt(computeAnswerMaybe(Ctx,Votes,InnerXml,Resp,VotesO)),fail.

unused_computeElement(Ctx,Votes, Tag, ATTRIBS, [DO|IT], OUT, VotesO) :- recursiveTag(Tag),!,
      withAttributes(_Ctx,ATTRIBS,
        ((findall(Out,((member(In,[DO|IT]),computeTemplate(Ctx,Votes, In, Out, _VoteMid))),INNERDONE),
         NOUT=..[Tag,ATTRIBS,INNERDONE],!,
         computeAnswer(Ctx,Votes,NOUT,OUT,VotesO)))).

% element inner reductions
still_computeElement(Ctx,Votes, Tag, ATTRIBS, [DO|IT], OUT, VotesO) :- recursiveTag(Tag),not(DO=(_-_)),!,
     appendAttributes(Ctx,ATTRIBS, [computeAnswer=[side_effects_allow=[transform],intag=Tag]], ATTRIBS_NEW),
     withAttributes(_Ctx,ATTRIBS_NEW,
       ((findall(OutVoteMid,((member(In,[DO|IT]),computeInner(Ctx,Votes, In, OutVoteMid))),INNERDONE),
        NOUT=..[Tag,ATTRIBS,INNERDONE]))),!,
         withAttributes(Ctx,ATTRIBS,computeInnerTemplate(Ctx,Votes,NOUT,OUT,VotesO)).

:-discontiguous(computeElement/7).

computeElement(_Ctx,Votes,Tag,ATTRIBS,InnerXml,Output,VotesO):- G=a(Votes,Tag,ATTRIBS,InnerXml),
   (prolog_must(ground(G)),not(var(Output);var(VotesO))),!,trace,throw(G).

% <html:br/>
computeElement(Ctx,Votes,Htmlbr,ATTRIBS,Input,Output,VotesO):- atom(Htmlbr),atom_concat_safe('html:',Br,Htmlbr),!,
   computeElementMust(Ctx,Votes,html:Br,ATTRIBS,Input,Output,VotesO).
computeElement(Ctx,Votes,html:Br,ATTRIBS,Input,Output,VotesO):- atom(Br),
   computeElementMust(Ctx,Votes,Br,ATTRIBS,Input,Output,VotesO).

% <br/>
computeElement(_Ctx,Votes,br,[],[],'\n',Votes):-!.
% <p/>
computeElement(_Ctx,Votes,p,[],[],'\r\n',Votes):-!.

% <sr/>
computeElement(Ctx,Votes,sr,ATTRIBS,Input,Output,VotesO):- !,
   computeElementMust(Ctx,Votes,srai,ATTRIBS,[element(star,ATTRIBS,Input)],Output,VotesO).

% <srai/>s   
computeElement(_Ctx,Votes,srai,ATTRIBS,[],result([],srai=ATTRIBS),VotesO):-trace,!,VotesO is Votes * 0.6.


% <srai>s   
computeElement(Ctx,Votes,srai,ATTRIBS,Input0,Output,VotesO):- % for evalSRAI
  prolog_must(ground(Input0)),!,flatten([Input0],Input), !,
  withAttributes(Ctx,ATTRIBS,
   prolog_must((computeInnerTemplate(Ctx,Votes,Input,Middle,VotesM),
    evalSRAI(Ctx,VotesM,ATTRIBS,Middle,OutputM,VotesOM),
    computeTemplateOutput(Ctx,VotesOM,OutputM,Output,VotesO) ))).

% <li...>
computeElement(Ctx,Votes,li,Preconds,InnerXml,OutProof,VotesO):- !, computeElement_li(Ctx,Votes,Preconds,InnerXml,OutProof,VotesO).

% <li> PASSED
computeElement_li(Ctx,Votes,Preconds,InnerXml,OutProof,VotesO):-
     precondsTrue(Ctx,Preconds),!,computeInnerTemplate(Ctx,Votes,InnerXml,Output,VotesM),VotesO is VotesM * 1.1,!,
     prolog_must(OutProof = proof(Output,Preconds)).

% <li> FAILED ==> []
computeElement_li(Ctx,Votes,Preconds,_InnerXml,OutProof,VotesO):-makeBlank(Ctx,Votes,failed(Preconds),OutProof,VotesO),!.

  precondsTrue(Ctx,PC):-lastMember(name=Name,PC,WO),lastMember(value=Value,WO,Rest),!,precondsTrue0(Ctx,[Name=Value|Rest]).
  precondsTrue(_Ctx,PC):-PC==[];var(PC),!.
  precondsTrue(Ctx,PC):-precondsTrue0(Ctx,PC).

  precondsTrue0(_Ctx,PC):-PC==[];var(PC),!.
  precondsTrue0(Ctx,[NV|MORE]):-!,precondsTrue0(Ctx,MORE),!,precondsTrue0(Ctx,NV).
  precondsTrue0(Ctx,N=V):- peekNameValue(Ctx,user,N,Value,'$value'([])),!,(valuesMatch(Ctx,Value,V)->debugFmt(valuesMatch(Value,V));debugFmt(valuesMatch(not,Value,V))),valuesMatch(Ctx,Value,V).
  precondsTrue0(_Ctx,_NV):-trace.

% <random...>
computeElement(Ctx,Votes,random,_Attribs,List,AA,VotesO):-!,randomPick(List,Pick),computeAnswer(Ctx,Votes,Pick,AA,VotesO).

% <condition...>
computeElement(Ctx,Votes,condition,CondAttribs,InnerXml,Result,VotesO):- 
     prolog_must(computeElement_condition(Ctx,Votes,CondAttribs,InnerXml,Result,VotesO)),!.

% <condition name="foo" value="bar"> (acts like a <li name="foo" value="bar">)
computeElement_condition(Ctx,Votes,CondAttribs,InnerXml,Result,VotesO):- 
   copy_term(CondAttribs,CondAttribsCopy),
     attributesContainOneOf0(CondAttribsCopy,[value=_]),attributesContainOneOf0(CondAttribsCopy,[var=_,name=_]),!,
      prolog_must(prolog_may(computeAnswerMaybe(Ctx,Votes,element(li,CondAttribs,InnerXml),Result,VotesO));makeBlank(Ctx,Votes,failed(CondAttribs),Result,VotesO)),!.

% <condition><li..>
computeElement_condition(Ctx,Votes,CondAttribs,InnerXml,Result,VotesO):-
  last(InnerXml,Last),
   withAttributes(Ctx, CondAttribs,
     once( 
      once((member(Pick,InnerXml),once((computeAnswerMaybe(Ctx,Votes,withAttributes(CondAttribs,Pick),Result,VotesO),isNonBlank(Result)))))
         ; (Result = Last, VotesO is Votes * 0.9))),!.

makeBlank(_Ctx,Votes,Message,result([],Message),VotesO):- VotesO is Votes * 0.3 . %%%failed(CondAttribs)

attributesContainOneOf(CondAttribs,AllOf):-copy_term(CondAttribs,CondAttribsCopy),attributesContainOneOf0(CondAttribsCopy,AllOf),!.
attributesContainOneOf0(CondAttribsCopy,AllOf):-member(Find,AllOf),member(Find,CondAttribsCopy),!.
isNonBlank(Result):-answerOutput(Result,Stuff),!,nonvar(Stuff),Stuff\==[].

% <input/response/that index="1"...>
computeElement(Ctx,Votes,InputResponse,Attribs,InnerXml,Resp,VotesO):-member(InputResponse,[input,response,that]),!,
  lastMemberOrDefault(index=Index,Attribs,AttribsNew,['1']),
  prolog_must(computeMetaStar(Ctx,Votes,InputResponse,Index,AttribsNew,InnerXml,Resp,VotesO)).

% <gossip...>
computeElement(Ctx,Votes,gossip,_Attribs,Input,Output,VotesO):-!,computeAnswer(Ctx,Votes,Input,Output,VotesO).

% <think...>
computeElement(Ctx,Votes,think,_Attribs,Input,proof([],think=Hidden),VotesO):-!,computeInnerTemplate(Ctx,Votes,Input,Hidden,VotesO).

% <formatter type="prologcall">
computeElement(Ctx,Votes,formatter,Attribs,Input,Result,VotesO):- 
      computeInnerTemplate(Ctx,Votes,Input,Mid,VotesO),
      lastMember(type=ProcI,Attribs,_NEW),listify(ProcI,[Proc|More]),atom(Proc),atomic_list_concat(['format_',Proc|More],Pred),
      functor(Callable,Pred,3),predicate_property(Callable,_),
      computeCall(Ctx,Pred,Mid,Result,'$error'),Result\=='$error'.

% <formatter type="sometag">
computeElement(Ctx,Votes,formatter,Attribs,Input,Result,VotesO):- !,
      computeInnerTemplate(Ctx,Votes,Input,Hidden,VotesO),
      lastMember(type=ProcI,Attribs,NEW),unlistify(ProcI,Proc),!,
      withAttributes(Ctx,NEW,computeElement(Ctx,Votes,Proc,NEW,Hidden,Result,VotesO)),!.


% <get,set,bot...>
computeElement(Ctx,Votes,GetSetBot,Attrib,InnerXml,Resp,VotesO):-member(GetSetBot,[get,set,bot]),!,computeGetSet(Ctx,Votes,GetSetBot,Attrib,InnerXml,Resp,VotesM),VotesO is VotesM * 1.1,!.

% for sure botvar-ish
% <version/id/date/size>
% HANDLE this in computeAnswer now convert_ele(Ctx,element(BOT_ATOM, ALIST, V),element(bot,[name=BOT_ATOM|ALIST],VV)):- globalAliceTagVar(BOT_ATOM),convert_template(Ctx,V,VV),!.
computeElement(Ctx,Votes,BOT_ATOM,[],[],proof(Resp,globalAliceTagVar(BOT_ATOM)),VotesO):- globalAliceTagVar(BOT_ATOM),!,expandVariable(Ctx,BOT_ATOM,Resp),VotesO is Votes  * 1.1.

% <topicstar,star,thatstar...>
computeElement(Ctx,Votes,StarTag,Attrib,InnerXml,Resp,VotesO):- hotrace(starType(StarTag,StarName)),!, %%trace,   
      computeStar(Ctx,Votes,StarName,Attrib,InnerXml,Resp,VotesM),VotesO is VotesM * 1.1,!.

% <cycrandom...>
computeElement(Ctx,Votes,cycrandom,_Attribs,RAND,Output,VotesO):-!, computeAnswer(Ctx,Votes,cyceval(RAND),RO,VotesO),randomPick(RO,Output).

% <system...>
computeElement(Ctx,Votes,Tag,Attribs,Input,result(RESULT,Tag=EVAL),VotesO):- 
   member(Tag,[system]),
   checkNameValue(Ctx,Attribs,[lang],Lang, 'bot'),
   computeInnerTemplate(Ctx,Votes,Input,EVAL,VotesO),
   systemCall(Ctx,Lang,EVAL,RESULT).

% <cyc..>
computeElement(Ctx,Votes,Tag,Attribs,Input,result(RESULT,Tag=EVAL),VotesO):- 
   member(Tag,[cycsystem,cyceval,cycquery]),
   checkNameValue(Ctx,Attribs,[lang],Lang, Tag),  
   computeInnerTemplate(Ctx,Votes,Input,EVAL,VotesO),
   systemCall(Ctx,Lang,EVAL,RESULT).

% <template, pre ...>
computeElement(Ctx,Votes,Tag,Attrib, DOIT, result(OUT,Tag=Attrib), VotesO) :- member(Tag,[template,pre]), !,
  computeTemplate(Ctx,Votes,DOIT,OUT,VotesO).

% <uppercase, lowercase ...>
computeElement(Ctx,Votes,Tag,Attrib, Input, Output, VotesO) :- formatterProc(Tag),
   formatterTypeMethod(Tag,Type,Method),!, 
   computeInnerTemplate(Ctx,Votes,Input,Mid,VotesO),
   computeCall(Ctx,Method,Mid,Output,prologCall(Method, result(Mid,element(Tag,[type=Type|Attrib])))),!.

% .... computeCall to formatter ....
computeCall(Ctx,Method,Mid,Output,ElseResult):-
   catch(prolog_must(call(Method,Ctx,Mid,Output)),
     E, (debugFmt(error(E,call(Method,Mid,Output))), trace, Output = ElseResult)).
computeCall(_Ctx,_Pred,_Mid,Result,ElseResult):-prolog_must(Result=ElseResult).

% .... formatters ....
format_formal(_Ctx,In,Out):-toPropercase(In,Out),!.
format_sentence(_Ctx,[In1|In],[Out1|Out]):-In=Out,toPropercase(In1,Out1),!.
format_sentence(_Ctx,In,Out):-In=Out.
format_think(_Ctx,_In,Out):-[]=Out.
format_gossip(_Ctx,In,Out):-In=Out.
format_uppercase(_Ctx,In,Out):-toUppercase(In,Out),!.
format_lowercase(_Ctx,In,Out):-toLowercase(In,Out),!.


% <gender..>
computeElement(Ctx,Votes,Gender,Attribs,Input,Result,VotesO):- substitutionDictsName(Gender,DictName),!,
   computeElement_subst(Ctx,Votes,Gender,DictName,Attribs,Input,Result,VotesO).

% <gender|person2/>
computeElement_subst(Ctx,Votes,_Gender,DictName,Attribs,[],Result,VotesO):-
    computeElementMust(Ctx,Votes,star,Attribs,[],Hidden,VotesO),    
    substituteFromDict(Ctx,DictName,Hidden,Result),!.

computeElement_subst(Ctx,Votes,_Gender,DictName,Attribs,Input,Result,VotesO):-
      withAttributes(Ctx,Attribs,((computeInnerTemplate(Ctx,Votes,Input,Hidden,VotesO),
      substituteFromDict(Ctx,DictName,Hidden,Result)))),!.

% <load...>
computeElement(Ctx,Votes,Tag,ATTRIBS,Input,result(RESULT,Tag=EVAL),VotesO):- 
   member(Tag,[load]),!,
   computeInnerTemplate(Ctx,Votes,Input,EVAL,VotesO),
   tag_eval(Ctx,element(Tag,ATTRIBS,EVAL),RESULT).

% <learn...>
computeElement(Ctx,Votes,Tag,ATTRIBS, NEWXML, result([learned,Diff,new,patterns]),Votes) :- 
  member(Tag,[learn]),!,
       NEW = element(aiml,ATTRIBS,NEWXML),  
       aimlCateSig(CateSig),
       predicate_property(CateSig,number_of_clauses(Before)),
        withAttributes(Ctx,ATTRIBS, load_aiml_structure(Ctx,NEW)),!,
           predicate_property(CateSig,number_of_clauses(After)),
           Diff is After - Before.

% <aiml/topic/category...>
computeElement(Ctx,Votes,Tag,ATTRIBS, NEWXML, result([learned,Tag,Diff,new,patterns]),Votes) :- member(Tag,[aiml,topic,category]),!,
       NEW = element(Tag,ATTRIBS,NEWXML),  
       aimlCateSig(CateSig),
       predicate_property(CateSig,number_of_clauses(Before)),
        withAttributes(Ctx,ATTRIBS, load_aiml_structure(Ctx,NEW)),!,
           predicate_property(CateSig,number_of_clauses(After)),
           Diff is After - Before.

% <eval...>
computeElement(Ctx,Votes,Tag,ATTRIBS, DOIT, RESULT, Votes) :- member(Tag,[eval]),!,
      withAttributes(Ctx,ATTRIBS,aiml_eval(Ctx,DOIT,RESULT)),!.

% other evals
computeElement(Ctx,Votes,Tag,ATTRIBS,Input,RESULT,VotesO):- evaluatorTag(Tag),
   computeInnerTemplate(Ctx,Votes,Input,EVAL,VotesO),!,
   tag_eval(Ctx,element(Tag,ATTRIBS,EVAL),RESULT),!.

% maybe not for sure botvar-ish 
% <favfood/master>
% HANDLE this in computeAnswer now convert_ele(Ctx,element(BOT_ATOM, ALIST, V),element(bot,[name=BOT_ATOM|ALIST],VV)):- globalAliceTagVar(BOT_ATOM),convert_template(Ctx,V,VV),!.
computeElement(Ctx,Votes,BOT_ATOM,[],[],proof(Resp,globalAliceTagVar(BOT_ATOM)),VotesO):- expandVariable(Ctx,BOT_ATOM,Resp),!, VotesO is Votes  * 1.1.

% rewrites
computeElement(_Ctx,Votes,Tag,[],[],result([reply,from,tag,Tag],element(Tag,[],[])),Votes):-!.
computeElement(_Ctx,Votes,Tag,Attribs,[],result([reply,from,Tag|Attribs],Tag,Attribs),Votes):-!,trace.
computeElement(Ctx,Votes,Tag,Attribs,InnerXml,Resp,VotesO):- 
  GETATTRIBS = element(Tag,Attribs,InnerXml), 
  convert_element(Ctx,GETATTRIBS,GETATTRIBS0), 
  GETATTRIBS \== GETATTRIBS0,!,
  %%trace,
  convert_element(Ctx,GETATTRIBS,_GETATTRIBS1), 
  computeAnswer(Ctx,Votes,GETATTRIBS0, Resp,VotesO).


% ===============================================================================================
% Compute Star
% ===============================================================================================

starName(StarStar,StarStar):- atom_concat(_,'star',StarStar),!.
starName(Star,StarStar):- atom_concat(Star,'star',StarStar),!.

computeStar(Ctx,Votes,Star,Attribs,InnerXml,Resp,VotesO):- 
   starName(Star,StarStar),Star \== StarStar,!,
   computeStar(Ctx,Votes,StarStar,Attribs,InnerXml,Resp,VotesO),!.

computeStar(Ctx,Votes,Star,Attribs,InnerXml,Resp,VotesO):- 
    lastMember(index=Index,Attribs,AttribsNew),!,
    computeStar1(Ctx,Votes,Star,Index,AttribsNew,InnerXml,Resp,VotesO),!.

computeStar(Ctx,Votes,Star,Attribs,InnerXml,Resp,VotesO):-
    computeStar1(Ctx,Votes,Star,[1],Attribs,InnerXml,Resp,VotesO),!.

computeStar1(Ctx,Votes,Star,Major,ATTRIBS,InnerXml,Proof,VotesO):-atomic(Major),!,
    computeStar1(Ctx,Votes,Star,[Major],ATTRIBS,InnerXml,Proof,VotesO),!.

computeStar1(Ctx,Votes,Star,Index,ATTRIBS,_InnerXml,proof(ValueO,StarVar=ValueI),VotesO):- is_list(Index),
      CALL=concat_atom([Star|Index],StarVar),
      prolog_must(catch(CALL,E,(debugFmt(CALL->E),fail))),      
      getDictFromAttributes(Ctx,evalsrai,ATTRIBS,Dict),
      getAliceMem(Ctx,Dict,StarVar,ValueI),!,
      computeTemplate(Ctx,Votes,element(template,ATTRIBS,ValueI),ValueO,VotesM),VotesO is VotesM * 1.1.

computeStar1(_Ctx,Votes,Star,Index,ATTRIBS,InnerXml,Resp,VotesO):- 
      traceIf(Resp = result(InnerXml,Star,Index,ATTRIBS)),!,VotesO is Votes * 1.1. 



computeMetaStar(Ctx,Votes,Star,Index,ATTRIBS,InnerXml,Resp,VotesO):-computeMetaStar0(Ctx,Votes,Star,Index,ATTRIBS,InnerXml,Resp,VotesO).

computeMetaStar0(Ctx,Votes,Star,MajorMinor,ATTRIBS,_InnerXml,proof(ValueO,Star=ValueI),VotesO):- 
      getDictFromAttributes(Ctx,evalsrai,ATTRIBS,Dict),
      getIndexedValue(Ctx,Dict,Star,MajorMinor,ValueI),!,
      computeInnerTemplate(Ctx,Votes,element(template,ATTRIBS,[ValueI]),ValueO,VotesM),VotesO is VotesM * 1.1.

computeMetaStar0(_Ctx,Votes,Star,Index,ATTRIBS,InnerXml,Resp,VotesO):- trace,
      traceIf(Resp = result(InnerXml,Star,Index,ATTRIBS)),!,VotesO is Votes * 0.9. 

getDictFromAttributes(Ctx,VarHolder,_ATTRIBS,SYM):-getCtxValue(VarHolder,Ctx,SYM).
getDictFromAttributes(_Ctx,_VarHolder,_ATTRIBS,'user').

% ===============================================================================================
% Compute Get/Set Probilities
% ===============================================================================================
/*

computeGetSet(Ctx,Votes,GetSetBot,ATTRIBS,[],Resp,VotesO):- !,computeGetSet(Ctx,Votes,GetSetBot,user,ATTRIBS,Resp,VotesO).
computeGetSet(Ctx,Votes,GetSetBot,name=NAME,MORE,Resp,VotesO):- !, computeGetSet(Ctx,Votes,GetSetBot,user,[name=NAME|MORE],Resp,VotesO).
computeGetSet(Ctx,Votes,GetSetBot,WHO,[X],Resp,VotesO):- !,computeGetSet(Ctx,Votes,GetSetBot,WHO,X,Resp,VotesO).
computeGetSet(Ctx,Votes,GetSetBot,ATTRIBS,Value,Resp,VotesO):- delete(ATTRIBS,type=bot,NEW),!,computeGetSet(Ctx,Votes,bot=GetSetBot,NEW,Value,Resp,VotesO).
computeGetSet(Ctx,Votes,GetSetBot,TYPE,[],Resp,VotesO):- !,computeGetSet(Ctx,Votes,GetSetBot,user,TYPE,Resp,VotesO).

*/

computeGetSet(Ctx,Votes,bot,ATTRIBS,InnerXml,Resp,VotesO):- !, computeGetSetVar(Ctx,Votes,bot,get,_VarName,ATTRIBS,InnerXml,Resp,VotesO),!.

computeGetSet(Ctx,Votes,GetSet,ATTRIBS,InnerXml,Resp,VotesO):- computeGetSetVar(Ctx,Votes,user,GetSet,_VarName,ATTRIBS,InnerXml,Resp,VotesO),!.

dictVarName(N):-member(N,[dictionary,dict,userdict,type,user,botname,username,you,me]).
dictFromAttribs(Ctx,ATTRIBS,Dict,NEW):-
      dictVarName(N),
      lastMember(N=Dict,ATTRIBS,NEW),getContextStoredValue(Ctx,Dict,_Name,Value),valuePresent(Value),!.

%%computeGetSetVar(Ctx,Votes,_Dict,bot,VarName,ATTRIBS,InnerXml,Resp,VotesO):- !,computeGetSetVar(Ctx,Votes,user,get,VarName,ATTRIBS,InnerXml,Resp,VotesO).
%% computeGetSetVar(Ctx,Votes,Dict,GetSetBot,VarName,ATTRIBS,InnerXml,Resp,VotesO).

computeGetSetVar(Ctx,Votes,Dict,GetSet,_OVarName,ATTRIBS,InnerXml,Resp,VotesO):- atom(ATTRIBS),ATTRIBS \= [],!, VarName = ATTRIBS,
     computeGetSetVar(Ctx,Votes,Dict,GetSet,VarName,[],InnerXml,Resp,VotesO).

computeGetSetVar(Ctx,Votes,Dict,GetSet,_OVarName,ATTRIBS,InnerXml,Resp,VotesO):-  
      member(N,[name,var]),
      lastMember(N=VarName,ATTRIBS,NEW),
      computeGetSetVar(Ctx,Votes,Dict,GetSet,VarName,NEW,InnerXml,Resp,VotesO).

computeGetSetVar(Ctx,Votes,_Dict,GetSet,VarName,ATTRIBS,InnerXml,Resp,VotesO):-
     dictFromAttribs(Ctx,ATTRIBS,Dict,NEW),
     %% MAYBE NEED THIS LATER ((member(EVarName,VarName),delete(ATTRIBS,EVarName,ATTRIBSOUT));ATTRIBSOUT=ATTRIBS),
      computeGetSetVar(Ctx,Votes,Dict,GetSet,VarName,NEW,InnerXml,Resp,VotesO).

computeGetSetVar(Ctx,Votes,Dict,get,VarName,ATTRIBS,_InnerXml,proof(ValueO,VarName=ValueI),VotesO):-!,
      getAliceMem(Ctx,Dict,VarName,ValueI),!,
      computeAnswer(Ctx,Votes,element(template,ATTRIBS,ValueI),ValueO,VotesM),VotesO is VotesM * 1.1.

computeGetSetVar(Ctx,Votes,Dict,set,VarName,ATTRIBS,InnerXml,proof(ReturnValue,VarName=InnerXml),VotesO):-!,
      computeAnswer(Ctx,Votes,element(template,ATTRIBS,InnerXml),ValueM,VotesM),
      computeInnerTemplate(Ctx,VotesM,ValueM,ValueO,VotesMO),
      setAliceMem(Ctx,Dict,VarName,ValueO),!,
      returnNameOrValue(Ctx,Dict,VarName,ValueO,ReturnValue),
      VotesO is VotesMO * 1.1.

%%computeGetSetVar(_Ctx,_Votes,_Get,_,_,_,_):-!,fail.

returnNameOrValue(Ctx,IDict,VarNameI,Value,ReturnValue):-dictNameDictNameC(Ctx,IDict,VarNameI,Scope,Name),!,returnNameOrValue(Ctx,Scope,Name,Value,ReturnValue).
returnNameOrValue(Ctx,_Dict,VarName,ValueO,ReturnValueO):-
      once(getAliceMem(Ctx,setReturn(_Default),VarName,NameOrValue);NameOrValue=value),
      returnNameOrValue0(NameOrValue,VarName,ValueO,ReturnValue),!,listify(ReturnValue,ReturnValueO).

returnNameOrValue0([NameOrValue],VarName,ValueO,ReturnValue):-!,returnNameOrValue0(NameOrValue,VarName,ValueO,ReturnValue),!.
returnNameOrValue0(name,VarName,_ValueO,VarName).
returnNameOrValue0(_Value,_VarName,ValueO,ValueO).


% ===============================================================================================
% Compute Answer Probilities
% ===============================================================================================
:-discontiguous(computeAnswer/5).
computeAnswer(Ctx,Votes,IN,Result,VotesOut):- not(tracing),computeAnswer0(Ctx,Votes,IN,Result,VotesOut),fail.

computeAnswer0(Ctx,Votes,IN,Result,VotesOut):- prolog_must((number(Votes),nonvar(IN),var(Result),var(VotesOut))),
      debugFmt(computeAnswer(Ctx,Votes,IN,Result,VotesOut)),fail.

computeAnswer0(Ctx,Votes,MidVote - In,Out,VotesO):- prolog_must(nonvar(MidVote)),
                           trace, !, computeAnswer(Ctx,Votes,In,Out,VotesA), VotesO is VotesA * MidVote.

computeAnswer0(_Ctx,Votes,_I,_,_):-(Votes>20;Votes<0.3),!,fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55
% elements
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55

% element inner reductions
computeAnswer_1_disabled(Ctx,Votes, element(Tag, ATTRIBS, [DO|IT]), OUT, VotesO) :- recursiveTag(Tag),not(DO=(_-_)),!,
     appendAttributes(Ctx,ATTRIBS, [computeAnswer=[side_effects_allow=[transform],intag=Tag]], ATTRIBS_NEW),
       withAttributes(_Ctx,ATTRIBS_NEW, findall(Each,((member(In,[DO|IT]),computeInner(Ctx,Votes, In, Each))),INNERDONE)),
       computeElementMust(Ctx,Votes,Tag, ATTRIBS, INNERDONE, OUT, VotesO).

computeAnswer(Ctx,Votes, element(Tag, ATTRIBS, [DO|IT]), OUT, VotesO) :- recursiveTag(Tag),not(DO=(_-_)),!,
     appendAttributes(Ctx,ATTRIBS, [computeAnswer=[side_effects_allow=[transform],intag=Tag]], ATTRIBS_NEW),
         withAttributes(Ctx,ATTRIBS_NEW, maplist_safe(computeInnerEach(Ctx, Votes),[DO|IT],INNERDONE)),
       prolog_mostly_ground((INNERDONE)),
       computeElementMust(Ctx,Votes,Tag, ATTRIBS, INNERDONE, OUT, VotesO).

computeAnswer(Ctx,Votes,element(Tag,Attribs,List),Out,VotesO):-!,computeElement(Ctx,Votes,Tag,Attribs,List,Out,VotesO),!.

% never gets here due to element/3 cutted above
computeAnswer(Ctx,Votes,element(Tag,Attribs,List),Output,VotesO):- !,computeElement(Ctx,Votes,Tag,Attribs,List,Output,VotesO),!.
computeAnswer(Ctx,Votes,element(Tag,Attribs,List),Out,VotesO):-trace,computeElement(Ctx,Votes,Tag,Attribs,List,Out,VotesO),!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55
% strings (must happen before list-check)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55
computeAnswer(Ctx,Votes,String,Out,VotesO):-string(String),string_to_atom(String,Atom),!,computeAnswer(Ctx,Votes,Atom,Out,VotesO).
computeAnswer(_Ctx,Votes,String,Atom,Votes):-is_string(String),toCodes(String,Codes),!,from_atom_codes(Atom,Codes),!.
computeAnswer(_Ctx,Votes,'$stringCodes'(List),AA,Votes):-!,from_atom_codes(AA,List),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55
% list-check
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55
% (should be no stars)
computeAnswer(_Ctx,_Votes,Pattern,_,_):- traceIf(isStarValue(Pattern)),fail.

computeAnswer(_Ctx,Votes,[],[],Votes):-!.
computeAnswer(Ctx,Votes,[A|B],OO,VotesO):- 
    atomic(A) -> 
      (!,computeTemplate(Ctx,Votes,B,BB,VotesO),OO=[A|BB]) ; 
      computeTemplate(Ctx,Votes,[A|B],OO,VotesO).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55
% atomic 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55
computeAnswer(Ctx,Votes,randomsentence,Output,VotesO):-!, choose_randomsentence(X),!,computeAnswer(Ctx,Votes,X,Output,VotesO).

computeAnswer(Ctx,Votes,In,Out,Votes):-atomic(In),expandVar(Ctx,In,Out).
computeAnswer(_Ctx,Votes,Resp,Resp,Votes):-atomic(Resp),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55
% prologCall
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55
computeAnswer(Ctx,Votes,prologCall(Method,Stuff),Resp,VotesO):- 
  computeAnswer(Ctx,Votes,Stuff,Mid,VotesO),
  call(Method,Ctx,Mid,Resp),!.

computeAnswer(_Ctx,Votes,prologCall(Method),Resp,VotesO):- trace,
   once(call(Method)->(Resp=pass(Method),VotesO=Votes);(Resp=failed(Method),VotesO is Votes*0.5)),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55
% Star Compounds
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55
computeAnswer(Ctx,Votes,star(Star,Attribs,InnerXml),Output,VotesO):- computeStar(Ctx,Votes,Star,Attribs,InnerXml,Output,VotesO),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55
% withAttributes Compounds
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55

computeAnswer(Ctx,Votes,Input,Output,VotesO):- functor(Input,withAttributes,_),
  Input = withAttributes(OuterAttribs,element(Tag,Attribs,InnerXml)),  
  append(Attribs,OuterAttribs,AllAttribs),
  withAttributes(Ctx,OuterAttribs,once(computeAnswer(Ctx,Votes,element(Tag,AllAttribs,InnerXml),Output,VotesO);Failed=failed)),!,Failed \== failed.

computeAnswer(Ctx,Votes,withAttributes(OuterAttribs,InnerXml),Output,VotesO):- 
  withAttributes(Ctx,OuterAttribs,once(computeAnswer(Ctx,Votes,InnerXml,Output,VotesO);Failed=failed)),!,Failed \== failed.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55
% Result or Proof already
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55

computeAnswer(_Ctx,Votes,Res, Res,Votes):-resultOrProof(Res,_Mid),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55
% Other Compounds
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55

computeAnswer(Ctx,Votes,GETATTRIBS, Resp,VotesO):- 
  convert_element(Ctx,GETATTRIBS,GETATTRIBS0), 
  GETATTRIBS \== GETATTRIBS0,!,
  trace,
  convert_element(Ctx,GETATTRIBS,_GETATTRIBS1),
  computeAnswer(Ctx,Votes,GETATTRIBS0, Resp,VotesO).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55
% errors
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55

computeAnswer(Ctx,Votes,GETATTRIBS, Resp,VotesO):- GETATTRIBS=..[GET], isAimlTag(GET), !, computeElementMust(Ctx,Votes,GET,[],[],Resp,VotesO).
computeAnswer(Ctx,Votes,GETATTRIBS, Resp,VotesO):- GETATTRIBS=..[GET,ATTRIBS], isAimlTag(GET), !, computeElementMust(Ctx,Votes,GET,ATTRIBS,[],Resp,VotesO).
computeAnswer(Ctx,Votes,GETATTRIBS, Resp,VotesO):- GETATTRIBS=..[GET,ATTRIBS,INNER], isAimlTag(GET), !, computeElementMust(Ctx,Votes,GET,ATTRIBS,INNER,Resp,VotesO).

computeAnswer(_Ctx,Votes,Resp,Resp,Votes):-trace,aiml_error(computeAnswer(Resp)).


% ===============================================================================================
% Apply Input Match
% ===============================================================================================

computeSRAI(_Ctx,_Votes,[],_,_,_Proof):- !,trace,fail.

computeSRAI(Ctx,Votes,Input,Result,VotesO,Proof):-
   getAliceMem(Ctx,'bot','me',Robot),
   getAliceMem(Ctx,'bot','you',User),
   getConversationThread(Ctx,User,Robot,ConvThread),
   prolog_must(computeSRAI0(Ctx,Votes,ConvThread,Input,Result,VotesO,Proof)).

getConversationThread(Ctx,User,Robot,ConvThread):-
   ConvThread = fromTo(User,Robot),
   setCtxValue('convthread',Ctx,ConvThread),!.

computeSRAI0(Ctx,Votes,ConvThread,Input,Result,VotesO,Proof):-   
   computeInnerTemplate(Ctx,Votes,Input,NewIn,VotesM),NewIn \== Input,!,
   computeSRAI0(Ctx,VotesM,ConvThread,NewIn,Result,VotesO,Proof),!.

computeSRAI0(Ctx,Votes,ConvThread,Input,Result,VotesO,Proof):- not(is_list(Input)),compound(Input),
   answerOutput(Input,InputO),Input\==InputO,!,trace,
   computeSRAI0(Ctx,Votes,ConvThread,InputO,Result,VotesO,Proof).

computeSRAI0(Ctx,Votes,ConvThread,Input,Result,VotesO,Proof):- not(is_list(Input)),compound(Input),
   computeAnswer(Ctx,Votes,Input,InputO,VotesM),Input\==InputO,!,trace,
   computeSRAI0(Ctx,VotesM,ConvThread,InputO,Result,VotesO,Proof).

computeSRAI0(Ctx,Votes,ConvThread,Input,Result,VotesO,Proof):-
  Each = (MatchLevel - e(VotesM,Result,Proof)), %% VotesO make it sort/2-able
  Call = computeSRAI2(Ctx,Votes,ConvThread,Input,Result,VotesM,Proof,MatchLevel),
  copy_term(Each:Call,EachFound:CallFound),
  findall(EachFound:CallFound, CallFound, FOUND),
  FOUND=[_|_],
  sort(FOUND,ORDER),!,
 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   member(Each:Call,ORDER),
   prolog_must(nonvar(Result)),
   debugFmt(computeSRAI(Input,Each)),
   VotesO is VotesM * 1.1.

computeSRAI0(_Ctx,Votes,ConvThread,Input,Result,VotesO,Proof):- !, VotesO is Votes * 0.7,
     Result = ['I',heard,you,'say:'|Input],
      Proof = result(Result,failed_computeSRAI2(Votes,Input,ConvThread)),
      debugFmt(Proof),!.

% now trace is ok

% this next line is what it does on fallback
computeSRAI0(Ctx,Votes,ConvThread,[B|Flat],[B|Result],VotesO,Proof):- fail,
   computeSRAI2(Ctx,Votes,ConvThread,Flat,Result,VotesO,Proof,_PostMatchLevel3),prolog_must(nonvar(Result)).

getAliceMemOrSetDefault(CtxIn,ConvThread,Name,Value,_OrDefault):-
   notrace(getCtxValue(ConvThread:Name,CtxIn,Value)),!.
getAliceMemOrSetDefault(CtxIn,ConvThread,Name,Value,_OrDefault):-
   notrace(getIndexedValue(CtxIn,ConvThread,Name,[],Value)),!.
getAliceMemOrSetDefault(CtxIn,ConvThread,Name,Value,OrDefault):-
   setAliceMem(CtxIn,ConvThread,Name,OrDefault),!,OrDefault=Value.


subclassMakeUserDict(Ctx,UserDict,SYM):-debugFmt(subclassMakeUserDict(Ctx,UserDict,SYM)),!.
convThreadDict(_Ctx,ConvThreadHint,ConvThread):-answerOutput(ConvThreadHint,First),unlistify(First,ConvThread),!.

computeSRAI222(CtxIn,Votes,ConvThreadHint,Pattern,Out,VotesO,ProofOut,OutputLevel):-    
   %%convertToMatchable(Pattern,InputPattern),
      convThreadDict(Ctx,ConvThreadHint,ConvThread),
         getCategoryArg(Ctx,'template',Out, _Out_ ,CateSig),!,
         getAliceMemOrSetDefault(CtxIn,ConvThread,'topic',Topic,['Nothing']),
         getAliceMemOrSetDefault(CtxIn,ConvThread,'userdict',UserDict,'user'), 
         %%getAliceMemOrSetDefault(CtxIn,ConvThread,'convthread',ConvThread,ConvThreadHint), 
         getCtxValue(evalsrai,CtxIn,SYM),         
         subclassMakeUserDict(CtxIn,UserDict,SYM),
         PreTopic = (CtxIn=Ctx),
         CPreTopic = true,
         make_preconds_for_match(Ctx,'topic',Topic,CateSig,PreTopic,AfterTopic, CPreTopic,CAfterTopic, Out,MinedCates,Proof,OutputLevel1),
         must_be_openCate(CateSig),         
         getAliceMemOrSetDefault(CtxIn,ConvThread,'that',That,['Nothing']),
         make_preconds_for_match(Ctx,'that',That,CateSig,AfterTopic,AfterThat,CAfterTopic,CAfterThat,Out,MinedCates,Proof,OutputLevel2),
         must_be_openCate(CateSig),
         make_preconds_for_match(Ctx,'pattern',Pattern,CateSig,AfterThat,AfterPattern,CAfterThat,CAfterPattern,Out,MinedCates,Proof,OutputLevel3),!,
         debugFmt(topicThatPattern(Topic,That,Pattern)),
         prolog_must(var(Out)),
         must_be_openCate(CateSig),!,
         prolog_must(atLeastOne((AfterPattern,CateSig))),  
         clause(CateSig,true,ClauseNumber),
         once((
            prolog_must(CAfterPattern),
            prolog_must(nonvar(Out)),
            OutputLevel = OutputLevel1 - OutputLevel2 - OutputLevel3,
            cateStrength(CateSig,Mult),
            (contains_term(Ctx,CateSig)->not(contains_term(Ctx,ClauseNumber));not(contains_term(Ctx,ClauseNumber))),
            VotesO is Votes * Mult,
           %% append([Out],Proof,FinalProof),
            FinalProof=Proof,
            append(FinalProof,[cn(ClauseNumber),CateSig],FinalProofClosed),
            %%debugFmt(result(out(Out),from(Pattern),FinalProofClosed)),
            ProofOut=..[proof,Out|FinalProofClosed])).


cateStrength(_CateSig,1.1):-!.

computeSRAI2(Ctx,Votes,ConvThread,Pattern,Out,VotesO,ProofOut,MatchLevel):- !, %% avoid next one
    computeSRAI222(Ctx,Votes,ConvThread,Pattern,Out,VotesO,ProofOut,MatchLevel).

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

make_preconds_for_match(Ctx,StarName,InputNothing,CateSig,PrecondsSearch,PostcondsSearch,PrecondsCommit,PostcondsCommit,Out,MinedCates,ProofOut,
 OutputLevel):-
   make_prepost_conds(Ctx,StarName,InputNothing,CateSig,FindPattern,CommitResult,Out,MinedCates,ProofOut,OutputLevel),
   PostcondsSearch = (PrecondsSearch,FindPattern),
   PostcondsCommit = (PrecondsCommit,CommitResult).

make_prepost_conds(Ctx,StarName,TextPattern,CateSig,FindPattern,CommitPattern,Out,MinedCates,ProofOut,MatchLevel):- 
  hotrace(meansNothing(TextPattern,InputPattern)), 
   TextPattern \= InputPattern,!,
  make_prepost_conds(Ctx,StarName,InputPattern,CateSig,FindPattern,CommitPattern,Out,MinedCates,ProofOut,MatchLevel).

make_prepost_conds(Ctx,StarName,TextPattern,CateSig,FindPattern,CommitPattern,Out,MinedCates,ProofOut,MatchLevel):-
  generateMatchPatterns(Ctx,StarName,Out,TextPattern,CateSig,MinedCates,EachMatchSig),!,
  prolog_must(EachMatchSig=[_|_]),
  FindPattern = 
      ((
        member(lsp(MatchLevel,_StarSets,MatchPattern),EachMatchSig),           
          getCategoryArg(Ctx,StarName,MatchPattern,Out,CateSig),
           prolog_must(make_star_binders(Ctx,StarName,TextPattern,MatchPattern,MatchLevel2,StarSets2)),
           %%prolog_must(StarSets=StarSets2),
           (CommitPattern = prolog_must(once((
              %%traceIf((StarName==pattern,TextPattern=[_,_|_])),
            once(
             (atLeastOne(starSets(Ctx,StarSets2)), 
              ignore(MatchLevel2 = MatchLevel),
              addKeyValue(ProofOut, StarName = (TextPattern:MatchPattern))
            ))))))
         )),!.
         

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
  LSP = lsp(MatchLevel,StarSets,MatchPattern),
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

callCatSig(Call,Arg,Length):-Call,argLength(Arg,Length).
argLength(Var,20):-var(Var),!.
argLength([X],N):-!,argLength(X,N),!.
argLength('*',10):-!.
argLength('_',8):-!.
argLength([L|IST],N):-length([L|IST],Len), N is 10 - Len,!.
argLength(_,40).


debugFmtList(ListI):-notrace((copy_term(ListI,List),debugFmtList0(List,List0),randomVars(List0),debugFmt(List0))),!.
debugFmtList0([],[]):-!.
debugFmtList0([A|ListA],[B|ListB]):-debugFmtList1(A,B),!,debugFmtList0(ListA,ListB),!.

debugFmtList1(Value,Value):-var(Value),!.
debugFmtList1(Name=Number,Name=Number):-number(Number).
debugFmtList1(Name=Value,Name=Value):-var(Value),!.
debugFmtList1(Name=Value,Name=(len:Len)):-copy_term(Value,ValueO),append(ValueO,[],ValueO),is_list(ValueO),length(ValueO,Len),!.
debugFmtList1(Name=Value,Name=(F:A)):-functor(Value,F,A).
debugFmtList1(Value,shown(Value)).


% ========================================================================================
%%make_star_binders(Ctx,_StarName,[_],[_,_|_]):-!,fail.
%%make_star_binders(Ctx,StarName,A,B):-make_star_binders(Ctx,StarName,A,B,_MatchLevel,_StarSets),!.    
% ========================================================================================

canMatchAtAll_debug(Ctx,StarName,InputPattern,MatchPattern,MatchLevel,StarSets):- 
    make_star_binders(Ctx,StarName,InputPattern,MatchPattern,MatchLevelInv,StarSets),!,MatchLevel is 1/MatchLevelInv ,
    nop(debugFmt(pass_canMatchAtAll_debug(Ctx,StarName,InputPattern,MatchPattern,MatchLevel,StarSets))),!.

canMatchAtAll_debug(Ctx,StarName,InputPattern,MatchPattern,_MatchLevel,_StarSets):-
    nop(debugFmt(fail_canMatchAtAll_debug(Ctx,StarName,InputPattern,MatchPattern))),!,fail.

make_star_binders(_Ctx,StarName,InputPattern,MatchPattern,MatchLevel,StarSets):- 
   prolog_must(var(StarSets)),prolog_must(var(MatchLevel)),prolog_must(ground(StarName:InputPattern:MatchPattern)),fail.  

:-setLogLevel(make_star_binders,none).

%end check
make_star_binders(_Ctx,_StarName,L,R,1,[]):-R==[],!,L==[].
make_star_binders(_Ctx,_StarName,L,R,1,[]):-L==[],!,R==[].


% left hand star/wild  (cannot really happen (i hope))
%make_star_binders(_Ctx,StarName,Star,_Match,_MatchLevel,_StarSets):- fail, not([StarName]=Star),isStarOrWild(StarName,Star,_WildValue,_WMatch,_Pred),!,trace,fail. 


% simplify
make_star_binders(Ctx,StarName,[Word1|B],[Word2|BB],CountO,StarSets):-
     sameWords(Word1,Word2),!,make_star_binders(Ctx,StarName,B,BB,Count,StarSets),CountO is Count + 1.

% tail (all now in) star/wildcard
make_star_binders(_Ctx,StarName,InputText,WildCard,WildValue,[Pred]):-isStarOrWild(StarName,WildCard,WildValue,InputText,Pred),!.

% once in star.. walk past star
make_star_binders(Ctx,StarName,InputText,[WildCard,M0|More],ValueO,[Pred|StarSets]):-isStarOrWild(StarName,WildCard,WildValue,SkipedSTAR,Pred),
         append(SkipedSTAR,[M1|LeftMore],InputText),sameWords(M0,M1),
         make_star_binders(Ctx,StarName,LeftMore,More,Value,StarSets),!,ValueO is WildValue + Value.

% is mid-right hand wildcard (this should be the last test)
make_star_binders(Ctx,StarName,[Match|B],[WildCard|BB],ValueO,[Pred|StarSets]):-!, isStarOrWild(StarName,WildCard,WildValue,Match, Pred),!,
     make_star_binders(Ctx,StarName,B,BB,Value,StarSets),!,ValueO is WildValue + Value.


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

isStarOrWild(StarName,[StarNameText],WildValue,InputText,Pred):-nonvar(StarNameText),!,isStarOrWild(StarName,StarNameText,WildValue,InputText,Pred),!.

isStarOrWild(StarName,StarNameText,WildValue,InputText,StarNameStar=InputText):-
   isStar(StarName,StarNameText,WildValue),!,starNameTransform(StarName,StarNameStar),!,traceIf(isStarValue(InputText)).
isStarOrWild(StarName,WildCardText,WildValue,InputText,Pred):- isWildCard(StarName,WildCardText,WildValue,InputText,Pred),!.

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
isStar(StarName,[StarNameText],WildValue):-trace,isStar(StarName,StarNameText,WildValue),!.
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


mapsome_openlist(_Pred,EndOfList):-endOfList(EndOfList),!.
mapsome_openlist(Pred,[Item|List]):-call(Pred,Item),!,mapsome_openlist(Pred,List).
mapsome_openlist(Pred,[_|List]):- mapsome_openlist(Pred,List).
mapsome_openlist(_Pred,_):-!.

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
   getDictFromAttributes(Ctx,evalsrai,[],Dict),
   prolog_must(Dict\==user),
   atom_concat(StarName,N,StarNameN),setAliceMem(Ctx,Dict,StarNameN,Pattern),!,star_flag(StarName,NN,NN+1).

%%REAL-UNUSED  set_matchit1(StarName,Pattern,Matcher,OnBind):- length(Pattern,MaxLen0), MaxLen is MaxLen0 + 2,
%%REAL-UNUSED    set_matchit2(StarName,Pattern,Matcher,MaxLen,OnBind).

isStar0(Word1):- member(Word1,[*,'_']).
sameWords(Word1,Word2):-atom(Word1),atom(Word2),atoms_match0(Word1,Word2).
 atoms_match0(Word1,Word2):- (isStar0(Word1);isStar0(Word2)),!,fail.
 atoms_match0(Word1,Word1):-!.
 atoms_match0(Word1,Word2):-litteral_atom(Word1,WordO),litteral_atom(Word2,WordO),!.


% ===============================================================================================
% Rate Match -  rateMatch(Markup,Original,InputBefore,InputAfter,Cost,Consumed)
% ===============================================================================================
rateMatch([],[],Out,Out,1,[]):-!.

rateMatch(Match,Match,Out,Out,1.3,[]):-!.

%%rateMatch(Match,More,Out,OOut,Vote2,Grabbed):-!,rateMatch(Match,More,Out,OOut,Vote2,Grabbed).

rateMatch([This|More],[This|More2],Out,OOut,Vote2,Grabbed):-
      rateMatch(More,More2,Out,OOut,Vote,Grabbed),
      Vote2 is Vote * (1.11).

rateMatch(['*'],More,Out,OOut,0.77,More):-!,subst(Out,'*',More,OOut),!.
rateMatch(['_'],More,Out,OOut,0.87,More):-!,subst(Out,'*',More,OOut),!.

rateMatch([*|Rest],More,Out,OOut,VoteO,Grabbed):-
      append(Grabbed,Rest,More),
      rateMatch(More,_More2,Out,OOut,Vote,_),
      subst(Out,'*',Grabbed,OOut),
      VoteO is Vote * (0.72).

rateMatch(['_'|Rest],More,Out,OOut,VoteO,Grabbed):-
      append(Grabbed,Rest,More),
      rateMatch(More,_More2,Out,OOut,Vote,_),
      subst(Out,'*',Grabbed,OOut),
      VoteO is Vote * (0.82).

rateMatch([This0|More],[This1|More2],Out,OOut,Vote2,Grabbed):- valuesMatch(_Ctx,This0,This1),!,
      rateMatch(More,More2,Out,OOut,Vote,Grabbed),
      Vote2 is Vote * (1.11).

rateMatch(More,More2,Out,Out2,1.0,Grabbed):-ignore(Out=Out2),ignore(Out=Grabbed),!,debugFmt(rateMatch(More,More2,Out,Out2,1.0,Grabbed)),trace,!.

% ===============================================================================================
% Run answer procs
% ===============================================================================================

choose_randomsentence(X):-
	repeat,
		retract(random_sent(Y)),
		assertz(random_sent(Y)),
		4 is random(10),!,
		Y=X.

% ===============================================================================================
% Get and rember Last Said
% ===============================================================================================
%% using dict now :-dynamic(getLastSaid/1). 
%%:-setAliceMem(_,_,that,(['where',am,'I'])).

rememberSaidIt(_Ctx,[]):-!.
rememberSaidIt(Ctx,_-R1):-!,rememberSaidIt(Ctx,R1).
rememberSaidIt(Ctx,R1):-append(New,'.',R1),!,rememberSaidIt(Ctx,New).
rememberSaidIt(Ctx,R1):-answerOutput(R1,SR1),R1\==SR1,!,rememberSaidIt(Ctx,SR1).
rememberSaidIt(Ctx,R1):- !,
   getAliceMem(Ctx,'bot','you',User),
   getAliceMem(Ctx,'bot','me',Robot),
   rememberSaidIt_SH(Ctx,R1,Robot,User).

rememberSaidIt_SH(_Ctx,[],_Robot,_User):-!.
rememberSaidIt_SH(Ctx,_-R1,Robot,User):-!,rememberSaidIt_SH(Ctx,R1,Robot,User).
rememberSaidIt_SH(Ctx,R1,Robot,User):-append(New,[Punct],R1),sentenceEnderOrPunct_NoQuestion(Punct),!,rememberSaidIt_SH(Ctx,New,Robot,User).
rememberSaidIt_SH(Ctx,R1,Robot,User):-answerOutput(R1,SR1),!,
   setAliceMem(Ctx,Robot,'lastSaid',SR1),
   pushInto1DAnd2DArray(Ctx,'response','that',10,SR1,User).


pushInto1DAnd2DArray(Ctx,Name,Name2,Ten,SR1,ConvThread):-
   splitSentences(SR1,SR2),
   addCtxValue(quiteMemOps,Ctx,true),
   previousVars(Name,PrevVars,Ten),
   maplist_safe(setEachSentenceThat(Ctx,ConvThread,PrevVars),SR2),!,

   previousVars(Name2,PrevVars2,Ten),
   setEachSentenceThat(Ctx,ConvThread,PrevVars2,SR2),
   setCtxValue(quiteMemOps,Ctx,false),
   !.


previousVars(That,[That],0):-!.
previousVars(That,[That],1):-!.
previousVars(That,[Item|Prevs],N):-indexOntoKey(That,N,Item), NN is N-1,previousVars(That,Prevs,NN).

indexOntoKey(That,N,That):-subscriptZeroOrOne(N),!.
indexOntoKey(That,N,Item):-atomic_list_concat([That,'(',N,')'],Item).

setEachSentenceThat(_Ctx,_User,_Vars,[]):-!.
setEachSentenceThat(Ctx,User,[Var],SR0):-
   cleanSentence(SR0,SR3),
   setAliceMem(Ctx,User,Var,SR3).

setEachSentenceThat(Ctx,User,[PrevVar,Var|MORE],SR0):-
   getAliceMem(Ctx,User,default(Var,'Nothing'),Prev),
   setAliceMem(Ctx,User,PrevVar,Prev),
   setEachSentenceThat(Ctx,User,[Var|MORE],SR0).

splitSentences([],[]):-!.   
splitSentences(SR1,[SR0|SRMORE]):-grabFirstSetence(SR1,SR0,LeftOver),splitSentences(LeftOver,SRMORE),!.
splitSentences(SR1,[SR1]):-!.

grabFirstSetence(SR1,SRS,LeftOver):-sentenceBreakChar(EOS),LeftSide=[_|_],append(LeftSide,[EOS|LeftOver],SR1),append(LeftSide,[EOS],SR0),cleanSentence(SR0,SRS),!.
cleanSentence(SR0,SRS):-prolog_must(leftTrim(SR0,sentenceEnderOrPunct,SRS)),!.

getRobot(Robot):-trace,getAliceMem(_Ctx,'bot','me',Robot),!.

getLastSaid(LastSaid):-getRobot(Robot),getAliceMem(_Ctx,Robot,'lastSaid',LastSaid).

getLastSaidAsInput(LastSaidMatchable):-getLastSaid(That),convertToMatchable(That,LastSaidMatchable),!.

% set some sane defaults to be overiden in config.xmls
:-setCurrentAliceMem('bot','me','bot').
:-setCurrentAliceMem('bot','you','user').
:-setCurrentAliceMem('bot','name',['The','Robot']).
:-setCurrentAliceMem('bot',version,'1.0.1').
:-setCurrentAliceMem('user','name',['Unknown','Partner']).
:-setCurrentAliceMem('user','me','user').
:-setCurrentAliceMem('user','is_type','agent').
:-setCurrentAliceMem('bot','is_type','agent').
:-setCurrentAliceMem('default','is_type','role').
%%:-setCurrentAliceMem(substitutions(_DictName),'is_type','substitutions').


% ===============================================================================================
% Template Output to text
% ===============================================================================================

%% <template> Used as a return result (non text maybe)(
computeTemplateOutput(Ctx,Votes,Input,Output,VotesO):-prolog_must(computeTemplate(Ctx,Votes,Input,Output,VotesO)),!.

%% like the above howeverr since its job is to be submitted to anohjter evaluator.. it needs to make "text" ussually
computeInnerTemplate(Ctx,Votes,Input,Output,VotesO):-
    computeTemplateOutput(Ctx,Votes,Input,Mid,VotesO),answerOutput(Mid,Output),!.

answerOutput(Output,NonVar):-nonvar(NonVar),answerOutput(Output,Var),!,valuesMatch(_Ctx,Var,NonVar).
answerOutput(Output,[Output]):-var(Output),!.
answerOutput([],Output):- !, Output=[].
%answerOutput(Output,Split):-atom(Output),atomSplit(Output,Split),Split==[Output],!.
%answerOutput(Output,Split):-atom(Output),trace,atomSplit(Output,Split),!.
answerOutput('<br/>',['\n']):-!.
answerOutput('<p/>',['\r\n']):-!.
answerOutput(element('br',[],[]),['\n']):-!.
answerOutput(Output,[Output]):-atomic(Output),!.
answerOutput([<,BR,/,>|B],OO):-atom(BR),!,
  answerOutput([element(BR,[],[])|B],OO),!.
answerOutput([A|AA],Output):-!,
   answerOutput(A,B),
   answerOutput(AA,BB),
   flatten([B,BB],Output).
answerOutput(element(Tag,Attribs,InnerXML),[element(Tag,Attribs,Output)]):- answerOutput(InnerXML,Output),!.
answerOutput(Term,Output):-resultOrProof(Term,Mid),!,answerOutput(Mid,Output).
answerOutput(Term,Output):-compound(Term),Term=..[_,Mid|_],debugFmt(answerOutput(Term->Mid)),!,answerOutput(Mid,Output).
answerOutput(Output,[Output]):-!.

lastMemberOrDefault(E,L,N,_D):-lastMember(E,L,N),!.
lastMemberOrDefault(_Named=E,L,N,D):-L=N,E=D,!.
lastMemberOrDefault(E,L,N,D):-L=N,E=D.
% ===============================================================================================
% Convert to Matchable
% ===============================================================================================

convertToMatchable(That,LastSaid):-
      answerOutput(That,AA),!,
      deleteAll(AA,['.','!','?','\'','!','','\n',',','\r\n','\n\n'],Words),
      toLowercase(Words,LastSaid),!.


matchable_litteral_safe(A,B):-atom(A),litteral_atom(A,B),!.
litteral_atom(A,B):-downcase_atom(A,B),!.
is_litteral(X):-atom(X),litteral_atom(X,N),!,N=X.

deleteAll(A,[],A):-!.
deleteAll(A,[L|List],AA):-delete(A,L,AAA),deleteAll(AAA,List,AA),!.
% ===============================================================================================
% Degrade Response
% ===============================================================================================

:-dynamic(degraded/1).

degrade(_-OR):-!,degrade(OR).
degrade(OR):-asserta(degraded(OR)).

% ===================================================================
% Substitution based on Pred like sameWordsDict(String,Pattern).
% ===================================================================

convert_substs(A,D):-simplify_atom0(A,M),A\==M,!,convert_substs(M,D).
convert_substs(A,D):-A=D.

simplify_atom0(A,A):-A==[],!.
simplify_atom0(A0,D):- is_list(A0),atomic_list_concat(A0,' ',A),!,simplify_atom0(A,D).
simplify_atom0(A,D):- atom(A),!,downcase_atom(A,B),atomic_list_concat(L0,'\\b',B),delete(L0,'',L),atomic_list_concat(L,' ',C),!,atomSplit(C,D),!.


sameWordsDict([String|A],[Pattern|B]):-!,sameWordsDict0(String,Pattern),!,sameWordsDict_l(A,B),!.
sameWordsDict(String,Pattern):-sameWordsDict0(String,Pattern),!.

sameWordsDict_l([String|A],[Pattern|B]):-sameWordsDict0(String,Pattern),sameWordsDict_l(A,B),!.
sameWordsDict_l([],[]):-!.

sameWordsDict0(verbatum(_String),_):-!,fail.
sameWordsDict0(_,verbatum(_Pattern)):-!,fail.
sameWordsDict0(String,Pattern):-compound(String), 
   debugOnFailure((answerOutput_atom(String,String1),debugFmt(interactStep(String,String1,Pattern)),String \== String1)),!,
   sameWordsDict0(String1,Pattern).

sameWordsDict0(String,Pattern):-compound(Pattern), 
   debugOnFailure((answerOutput_atom(Pattern,Pattern1),debugFmt(interactStep(Pattern,Pattern1,String)), Pattern \== Pattern1)),!,
   sameWordsDict0(String,Pattern1).

sameWordsDict0(String,Pattern):-String==Pattern,!,debugFmt(sameWordsDict(String,Pattern)).
%sameWordsDict0(String,Pattern):-debugFmt(sameWordsDict0(String,Pattern)),wildcard_match(Pattern,String),!.
%sameWordsDict0(String,Pattern):-dwim_match(Pattern,String),!.

answerOutput_atom(Pattern,Pattern1):-unresultify(Pattern,Pattern0),unlistify(Pattern0,Pattern1),!,prolog_must(atomic(Pattern1)).

dictReplace(DictName,Before,After):-dict(substitutions(DictName),Before,After).%%convert_substs(Before,B),convert_template(Ctx,After,A).

substituteFromDict(Ctx,DictName,Hidden,Output):-answerOutput(Hidden,Mid),Hidden\==Mid,!,substituteFromDict(Ctx,DictName,Mid,Output),!.

substituteFromDict(Ctx,DictName,Hidden,Output):- dictReplace(DictName,_,_),prolog_must(substituteFromDict_l(Ctx,DictName,Hidden,Output)),!.

substituteFromDict(_Ctx,DictName,Hidden,Output):- dictReplace(DictName,_,_),!,
      recorda(DictName,Hidden),
      forall(dictReplace(DictName,Before,After),
          (recorded(DictName,Start,Ref),
          erase(Ref),              
          pred_subst(sameWordsDict,Start,Before,verbatum(After),End),
          recorda(DictName,End))),
      recorded(DictName,Output,Ref),
      debugFmt(substituteFromDict(Hidden,Output)),
      erase(Ref),!.

substituteFromDict(_Ctx,DictName,Hidden,result([substs,DictName,on,Hidden],Result)):-Result=..[DictName,Hidden].

substituteFromDict_l(_Ctx,_DictName,Hidden,Output):-atomic(Hidden),!,Hidden=Output.
substituteFromDict_l(Ctx,DictName,[V|Hidden],[V|Output]):-verbatum(_)==V,!,substituteFromDict_l(Ctx,DictName,Hidden,Output).
substituteFromDict_l(Ctx,DictName,Hidden,[verbatum(After)|Output]):-dictReplace(DictName,Before,After),
   debugOnError((length(Before,Left),length(NewBefore,Left))),
   append(NewBefore,Rest,Hidden),sameBinding(NewBefore,Before),!,substituteFromDict_l(Ctx,DictName,Rest,Output).
substituteFromDict_l(Ctx,DictName,[V|Hidden],[V|Output]):-substituteFromDict_l(Ctx,DictName,Hidden,Output).

