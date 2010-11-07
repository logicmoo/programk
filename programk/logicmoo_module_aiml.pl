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

% When you trust the code enough you dont to debug it
%  but if that code does something wrong while your not debugging, you want to see the error
hotrace(X):-tracing,!, notrace(X).
hotrace(X):-call(X).

:-dynamic(lineInfoElement/4).

:-ensure_loaded(library('programk/logicmoo_module_aiml_shared.pl')).
:-ensure_loaded(library('programk/logicmoo_module_aiml_xpath.pl')).
:-ensure_loaded(library('programk/logicmoo_module_aiml_loader.pl')).
:-ensure_loaded(library('programk/logicmoo_module_aiml_convertor.pl')).
:-ensure_loaded(library('programk/logicmoo_module_aiml_eval.pl')).
%%:-ensure_loaded(library('notaiml/tokenize.pl')).
:-dynamic(aiml_directory_search/1).
:-multifile(aiml_directory_search/1).
:-module_transparent(aiml_directory_search/1).

aiml_directory_search('programk').
aiml_directory_search('programk/test_suite').



:- abolish(cyc:debugFmt/1).

cyc:debugFmt(Stuff):- notrace((debugFmtS(Stuff))),!.

debugFmtS([]):-!.
debugFmtS([A|L]):-!,debugFmt('% ~q~n',[[A|L]]).
debugFmtS(Comp):-hideIfNeeded(Comp,Comp2),!,debugFmt('% ~q~n',[Comp2]).
debugFmtS(Stuff):-!,debugFmt('% ~q~n',[Stuff]).

run_chat_tests:-
   test_call(alicebot('Hi')),
   test_call(alicebot('What is your name')),
   test_call(alicebot('My name is Fred.')),
   test_call(alicebot('what is my name?')).

test_call(G):-writeln(G),ignore(once(catch(G,E,writeln(E)))).

nop(_).

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
aimlCateSig(Pred):-aimlCateOrder(List),length(List,L),functor(Pred,aimlCate,L),asserta(aimlCateSigCached(Pred)),!.

aimlCateOrder([graph,precall,topic,that,request,pattern,flags,call,guard,userdict,template,srcinfo,srcfile]).

% [graph,precall,topic,that,pattern,flags,call,guard,template,userdict]
cateMemberTags(Result):- aimlCateOrder(List), findall(E,(member(E0,List),once((E0=[E|_];E0=E))), Result).

makeAimlCateSig(Ctx,ListOfValues,Pred):-aimlCateSig(Pred),!,makeAimlCate(Ctx,ListOfValues,Pred,current_value),!.

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
say(Ctx,X):- aiml_eval(Ctx,X,Y),!,answerOutput(Y,O),debugFmt(say(O,Y)),!.

alicebot:-repeat,
	read_line_with_nl(user,CodesIn,[]),        
        makeAimlContext(alicebot,Ctx),
        once((trim(CodesIn,Codes),atom_codes(Atom,Codes),alicebotCTX(Ctx,Atom))),fail.

alicebot(Input):- currentContext(alicebot(Input),Ctx),!,alicebotCTX(Ctx,Input).

alicebotCTX(_Ctx,Input):- atom(Input),catch(((atom_to_term(Input,Term,Vars),callInteractive0(Term,Vars))),_,fail),!.
alicebotCTX(Ctx,Input):- alicebotCTX(Ctx,Input,Resp),!,say(Ctx,Resp),!.
%%alicebotCTX(Ctx,_):- trace, say(Ctx,'-no response-').


alicebotCTX(_Ctx,[],_):-debugFmt('no input'),!,fail.
alicebotCTX(Ctx,Input,Resp):- atom(Input),!,
      atomSplit(Input,TokensO),!,Tokens=TokensO,
      alicebotCTX(Ctx,Tokens,Resp),!.
alicebotCTX(Ctx,[TOK|Tokens],Output):- atom(TOK),atom_concat('@',_,TOK),!,systemCall(Ctx,'bot',[TOK|Tokens],Output),debugFmt(Output).
alicebotCTX(Ctx,Tokens,Resp):-
   %%convertToMatchable(Tokens,UCase),!,
   =(Tokens,UCase),!,
   removePMark(UCase,Atoms),!,
   alicebot2(Ctx,Atoms,Resp),!.
alicebotCTX(_Ctx,In,Res):- !,ignore(Res='-no response-'(In)).




alicebot2(Atoms,Resp):- currentContext(alicebot2(Atoms),Ctx),!,alicebot2(Ctx,Atoms,Resp).

%% dont really call_with_depth_limit/3 as it spoils the debugger
call_with_depth_limit_traceable(G,Depth,Used):-tracing,!,G,ignore(Depth=1),ignore(Used=1).
call_with_depth_limit_traceable(G,Depth,Used):-call_with_depth_limit(G,Depth,Used).

alicebot2(Ctx,Atoms,Resp):-	
   retractall(posibleResponse(_,_)),
   flag(a_answers,_,0),!,
   ignore((
   getItemValue('you',Ctx,User),
   setAliceMem(Ctx,User,'input',Atoms),
   call_with_depth_limit_traceable(computeInputOutput(Ctx,1,Atoms,Output,N),8000,_DL),
	 prolog_must((nonvar(N),nonvar(Output),savePosibleResponse(N,Output))),flag(a_answers,X,X+1),X>3)),!,
   findall(NR-OR,posibleResponse(NR,OR),L),!,
   (format('~n-> ~w~n',[L])),
   keysort(L,S),
   dumpList(S),
   reverse(S,[Resp|_RR]),
   degrade(Resp),!,
   rememberSaidIt(Ctx,Resp),!.

computeInputOutput(Ctx,VoteIn,Input,Output,VotesOut):- computeAnswer(Ctx,VoteIn,element(srai,[],Input),Output,VotesOut),!.

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
evalSRAI(Ctx,Votes,ATTRIBS,Input0,Output,VotesO):-
  prolog_must(ground(Input0)),!,flatten([Input0],Input),  
  withAttributes(Ctx,[proof=Proof|ATTRIBS],
  (
      computeSRAI(Ctx,Votes,Input,MidIn,VotesM,Proof),      
      prolog_must(nonvar(MidIn)),
      debugFmt(sraiTRACE(Input,MidIn)),
      computeElementMust(Ctx,VotesM,template,ATTRIBS,MidIn,MidIn9,VotesI9),
      debugFmt(computeElementMust(Input,MidIn,MidIn9)),
      prolog_must(answerOutput(MidIn9,Mid9)),
      prolog_must(computeTemplate(Ctx,VotesI9,Mid9,Output,VotesO)))),!.

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
computeTemplate11(Ctx,Votes,[A|B],OO,Votes):-expandVar(Ctx,A,AA),!,computeTemplate11(Ctx,Votes,B,BB,Votes),once(flatten([AA,BB],OO)).
computeTemplate11(Ctx,Votes,[A|B],[A|BB],Votes):-atomic(A),!,computeTemplate11(Ctx,Votes,B,BB,Votes).


traceAIML:-!.

expandVar(_Ctx,Var,Var):-var(Var),!,traceAIML.
expandVar(_Ctx,[Var|_],Var):- !,traceAIML.
expandVar(_Ctx,nick,A):-!,default_user(B),!,from_atom_codes(A,B),!.
expandVar(_Ctx,person,A):-!,default_user(B),!,from_atom_codes(A,B),!.
expandVar(_Ctx,botnick,'jllykifsh'):-!.
expandVar(_Ctx,mynick,'jllykifsh'):-!.
expandVar(_Ctx,name=name,'jllykifsh'):-!.
expandVar(_Ctx,mychan,A):-!,default_channel(B),!,from_atom_codes(A,B),!.
expandVar(_Ctx,Resp,Resp):-atomic(Resp),!.
expandVar(Ctx,In,Out):-prolog_must(computeAnswer(Ctx,1,In,Out,_VotesO)).


from_atom_codes(Atom,Atom):-atom(Atom),!.
from_atom_codes(Atom,Codes):-convert_to_string(Codes,Atom),!.
from_atom_codes(Atom,Codes):-atom_codes(Atom,Codes).


:-dynamic(recursiveTag/1).


notRecursiveTag(system).
%%notRecursiveTag(template).
notRecursiveTag(condition).
notRecursiveTag(li).

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
computeInner(Ctx,Votes, In, VoteMid-Out) :-trace, computeAnswer(Ctx,Votes, In, Out, VoteMid),!,prolog_must(nonvar(Out)),prolog_must(nonvar(VoteMid)).

computeInnerEach(_Ctx, _Votes, In, Out) :- atom(In), !, Out=In , prolog_mostly_ground(Out).
computeInnerEach(  Ctx, Votes, In, Out) :- debugOnFailureAiml(computeAnswer(Ctx,Votes, In, Out, _VoteMid)),!, prolog_mostly_ground(Out).
computeInnerEach(_Ctx, _Votes, In, Out) :- !, Out=In,  prolog_mostly_ground((Out)).


% ===============================================================================================
% Compute Answer Element Probilities
% ===============================================================================================
computeElementMust(Ctx,Votes,Tag,Attribs,InnerXml,Resp,VotesO):-computeElement(Ctx,Votes,Tag,Attribs,InnerXml,Resp,VotesO),!.
computeElementMust(Ctx,Votes,Tag,Attribs,InnerXml,Resp,VotesO):-trace,computeElement(Ctx,Votes,Tag,Attribs,InnerXml,Resp,VotesO),!.

computeAnswerMaybe(Ctx,Votes,element(Tag,Attribs,InnerXml),Output,VotesO):-!,computeElement(Ctx,Votes,Tag,Attribs,InnerXml,Output,VotesO),!.
computeAnswerMaybe(Ctx,Votes,InnerXml,Resp,VotesO):-computeAnswer(Ctx,Votes,InnerXml,Resp,VotesO),!.

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
         withAttributes(Ctx,ATTRIBS,computeAnswer(Ctx,Votes,NOUT,OUT,VotesO)).

:-discontiguous(computeElement/7).

computeElement(_Ctx,Votes,Tag,ATTRIBS,InnerXml,Output,VotesO):- G=a(Votes,Tag,ATTRIBS,InnerXml),
   (prolog_must(ground(G)),not(var(Output);var(VotesO))),!,trace,throw(G).

% <html:br/>
computeElement(Ctx,Votes,Htmlbr,ATTRIBS,Input,Output,VotesO):- atom(Htmlbr),atom_concat_safe('html:',Br,Htmlbr),!,
   computeElementMust(Ctx,Votes,html:Br,ATTRIBS,Input,Output,VotesO).
computeElement(Ctx,Votes,html:Br,ATTRIBS,Input,Output,VotesO):- atom(Br),
   computeElementMust(Ctx,Votes,Br,ATTRIBS,Input,Output,VotesO).

% <br/>
computeElement(_Ctx,Votes,br,[],[],'<br/>',Votes):-!.
% <p/>
computeElement(_Ctx,Votes,p,[],[],'<p/>',Votes):-!.

% <sr/>
computeElement(Ctx,Votes,sr,ATTRIBS,Input,Output,VotesO):- !,
   computeElementMust(Ctx,Votes,srai,ATTRIBS,[element(star,ATTRIBS,Input)],Output,VotesO).

% <srai/>s   
computeElement(_Ctx,Votes,srai,ATTRIBS,[],result([],srai=ATTRIBS),VotesO):-trace,!,VotesO is Votes * 0.6.


% <srai>s   
computeElement(Ctx,Votes,srai,ATTRIBS,Input0,Output,VotesO):- 
  prolog_must(ground(Input0)),!,flatten([Input0],Input), 
   evalSRAI(Ctx,Votes,ATTRIBS,Input,Output,VotesO),!.


computeElement(Ctx,Votes,li,Preconds,InnerXml,proof(Output,Preconds),VotesO):-!,
     precondsTrue(Ctx,Preconds),!,computeTemplate(Ctx,Votes,InnerXml,Output,VotesM),VotesO is VotesM * 1.1,!.

  precondsTrue(Ctx,PC):-lastMember(name=Name,PC,WO),lastMember(value=Value,WO,Rest),!,precondsTrue0(Ctx,[Name=Value|Rest]).
  precondsTrue(_Ctx,PC):-PC==[];var(PC),!.
  precondsTrue(Ctx,PC):-trace,precondsTrue0(Ctx,PC).

  precondsTrue0(_Ctx,PC):-PC==[];var(PC),!.
  precondsTrue0(Ctx,[NV|MORE]):-!,precondsTrue0(Ctx,MORE),!,precondsTrue0(Ctx,NV).
  precondsTrue0(Ctx,N=V):- peekNameValue(Ctx,user,N,Value,[]),!,valuesMatch(Ctx,Value,V).
  precondsTrue0(_Ctx,_NV):-trace.


computeElement(Ctx,Votes,random,_Attribs,List,AA,VotesO):-!,randomPick(List,Pick),computeAnswer(Ctx,Votes,Pick,AA,VotesO).
computeElement(Ctx,Votes,condition,_Attribs,List,AA,VotesO):-!,debugOnFailureAiml(once(( member(Pick,List),computeAnswerMaybe(Ctx,Votes,Pick,AA,VotesO)))).
computeElement(Ctx,Votes,gossip,_Attribs,Input,Output,VotesO):-!,computeAnswer(Ctx,Votes,Input,Output,VotesO).
computeElement(Ctx,Votes,think,_Attribs,Input,proof([],Hidden),VotesO):-!,computeTemplate(Ctx,Votes,Input,Hidden,VotesO).
computeElement(Ctx,Votes,GetSetBot,Attrib,InnerXml,Resp,VotesO):-member(GetSetBot,[get,set,bot]),!,computeGetSet(Ctx,Votes,GetSetBot,Attrib,InnerXml,Resp,VotesM),VotesO is VotesM * 1.1,!.
computeElement(Ctx,Votes,StarTag,Attrib,InnerXml,Resp,VotesO):- starType(StarTag,StarName),!,computeStar(Ctx,Votes,StarName,Attrib,InnerXml,Resp,VotesM),VotesO is VotesM * 1.1,!.

computeElement(Ctx,Votes,cycrandom,_Attribs,RAND,Output,VotesO):-!, computeAnswer(Ctx,Votes,cyceval(RAND),RO,VotesO),randomPick(RO,Output).

computeElement(Ctx,Votes,Tag,Attribs,Input,result(RESULT,Tag=EVAL),VotesO):- 
   member(Tag,[system]),
   checkNameValue(Ctx,Attribs,[lang],Lang, 'bot'),
   computeTemplate(Ctx,Votes,Input,EVAL,VotesO),
   systemCall(Ctx,Lang,EVAL,RESULT).

computeElement(Ctx,Votes,Tag,Attribs,Input,result(RESULT,Tag=EVAL),VotesO):- 
   member(Tag,[cycsystem,cyceval,cycquery]),
   checkNameValue(Ctx,Attribs,[lang],Lang, Tag),  
   computeTemplate(Ctx,Votes,Input,EVAL,VotesO),
   systemCall(Ctx,Lang,EVAL,RESULT).

computeElement(Ctx,Votes,Tag,ATTRIBS,Input,result(RESULT,Tag=EVAL),VotesO):- 
   member(Tag,[load,learn]),
   computeTemplate(Ctx,Votes,Input,EVAL,VotesO),
   tag_eval(Ctx,element(Tag,ATTRIBS,EVAL),RESULT).


computeElement(Ctx,Votes,Tag,Attrib, DOIT, result(OUT,Tag=Attrib), VotesO) :- member(Tag,[template,pre]), !, computeTemplate(Ctx,Votes,DOIT,OUT,VotesO).
computeElement(Ctx,Votes,Tag,Attrib, DOIT, result(OUT,Tag=Attrib), VotesO) :- formatterProc(Tag), !, computeTemplate(Ctx,Votes,DOIT,OUT,VotesO).

computeElement(_Ctx,Votes,Tag,Attribs,[],result([],Tag,Attribs),Votes):-!,trace.
computeElement(Ctx,Votes,Tag,Attribs,InnerXml,result(Inner,Tag=Attribs),VotesO):-!,computeTemplate(Ctx,Votes,InnerXml,Inner,VotesO).

computeElement(Ctx,Votes,Tag,Attribs,InnerXml,Resp,VotesO):- 
  GETATTRIBS = element(Tag,Attribs,InnerXml), 
  convert_element(Ctx,GETATTRIBS,GETATTRIBS0), 
  GETATTRIBS \== GETATTRIBS0,!,
  trace,
  convert_element(Ctx,GETATTRIBS,_GETATTRIBS1), 
  computeAnswer(Ctx,Votes,GETATTRIBS0, Resp,VotesO).

% ===============================================================================================
% Compute Star
% ===============================================================================================

computeStar(Ctx,Votes,Star,Attribs,InnerXml,Resp,VotesO):- 
    lastMember(index=Index,Attribs,AttribsNew),!,
    computeStar0(Ctx,Votes,Star,Index,AttribsNew,InnerXml,Resp,VotesO),!.
computeStar(Ctx,Votes,Star,Attribs,InnerXml,Resp,VotesO):-
    computeStar0(Ctx,Votes,Star,1,Attribs,InnerXml,Resp,VotesO),!.

computeStar0(Ctx,Votes,Star,Index,ATTRIBS,_InnerXml,proof(ValueO,StarVar=ValueI),VotesO):- atom_concat(Star,Index,StarVar),
      getAliceMem(Ctx,_Dict,StarVar,ValueI),!,
      computeTemplate(Ctx,Votes,element(template,ATTRIBS,ValueI),ValueO,VotesM),VotesO is VotesM * 1.1.

computeStar0(_Ctx,Votes,Star,Index,ATTRIBS,InnerXml,Resp,VotesO):-
      Resp = result(InnerXml,Star,Index,ATTRIBS),!,VotesO is Votes * 1.1.


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
computeGetSet(Ctx,Votes,bot,ATTRIBS,InnerXml,Resp,VotesO):-!,computeGetSetVar(Ctx,Votes,bot,get,_VarName,ATTRIBS,InnerXml,Resp,VotesO).
computeGetSet(Ctx,Votes,GetSet,ATTRIBS,InnerXml,Resp,VotesO):-computeGetSetVar(Ctx,Votes,user,GetSet,_VarName,ATTRIBS,InnerXml,Resp,VotesO).


%%computeGetSetVar(Ctx,Votes,_Dict,bot,VarName,ATTRIBS,InnerXml,Resp,VotesO):- !,computeGetSetVar(Ctx,Votes,user,get,VarName,ATTRIBS,InnerXml,Resp,VotesO).
%% computeGetSetVar(Ctx,Votes,Dict,GetSetBot,VarName,ATTRIBS,InnerXml,Resp,VotesO).

computeGetSetVar(Ctx,Votes,Dict,GetSet,_OVarName,ATTRIBS,InnerXml,Resp,VotesO):- atom(ATTRIBS),ATTRIBS \= [],!, VarName = ATTRIBS,
     computeGetSetVar(Ctx,Votes,Dict,GetSet,VarName,[],InnerXml,Resp,VotesO).

computeGetSetVar(Ctx,Votes,Dict,GetSet,_OVarName,ATTRIBS,InnerXml,Resp,VotesO):-  
      member(N,[name,var]),
      lastMember(N=VarName,ATTRIBS,NEW),
      computeGetSetVar(Ctx,Votes,Dict,GetSet,NEW,VarName,InnerXml,Resp,VotesO).

computeGetSetVar(Ctx,Votes,_Dict,GetSet,VarName,ATTRIBS,InnerXml,Resp,VotesO):- 
      member(N,[type,dict,user,botname,username,you,me]),
      lastMember(N=Dict,ATTRIBS,NEW),!,
      computeGetSetVar(Ctx,Votes,Dict,GetSet,NEW,VarName,InnerXml,Resp,VotesO).

computeGetSetVar(Ctx,Votes,Dict,get,VarName,ATTRIBS,_InnerXml,proof(ValueO,VarName=ValueI),VotesO):-!,
      getAliceMem(Ctx,Dict,VarName,ValueI),!,
      computeTemplate(Ctx,Votes,element(template,ATTRIBS,ValueI),ValueO,VotesM),VotesO is VotesM * 1.1.

computeGetSetVar(Ctx,Votes,Dict,set,VarName,ATTRIBS,InnerXml,proof(ValueO,VarName=InnerXml),VotesO):-!,
      computeTemplate(Ctx,Votes,element(template,ATTRIBS,InnerXml),ValueO,VotesM),
      setAliceMem(Ctx,Dict,VarName,ValueO),!,VotesO is VotesM * 1.1.

computeGetSetVar(_Ctx,_Votes,_Get,_,_,_,_):-!,fail.

% ===============================================================================================
% Compute Answer Probilities
% ===============================================================================================
:-discontiguous(computeAnswer/5).

computeAnswer(Ctx,Votes,IN,Result,VotesOut):- prolog_must((number(Votes),nonvar(IN),var(Result),var(VotesOut))),
      debugFmt(computeAnswer(Ctx,Votes,IN,Result,VotesOut)),fail.

computeAnswer(Ctx,Votes,MidVote - In,Out,VotesO):- prolog_must(nonvar(MidVote)),
                           trace, !, computeAnswer(Ctx,Votes,In,Out,VotesA), VotesO is VotesA * MidVote.

computeAnswer(_Ctx,Votes,_I,_,_):-(Votes>20;Votes<0.3),!,fail.



computeAnswer(Ctx,Votes,Input,Output,Votes):- fail, Input=..[result,RESULT|REST],trace,Output=result(EVAL,RESULT-REST),
   aiml_eval(Ctx,RESULT,EVAL),!.


computeAnswer(_Ctx,Votes,Res, Res,Votes):-functor(Res,result,_),!.
computeAnswer(_Ctx,Votes,Res, Res,Votes):-functor(Res,proof,_),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55
% strings (must happen before list-check)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55
computeAnswer(Ctx,Votes,String,Out,VotesO):-string(String),string_to_atom(String,Atom),!,computeAnswer(Ctx,Votes,Atom,Out,VotesO).
computeAnswer(_Ctx,Votes,String,Atom,Votes):-is_string(String),toCodes(String,Codes),!,from_atom_codes(Atom,Codes),!.
computeAnswer(_Ctx,Votes,'$stringCodes'(List),AA,Votes):-!,from_atom_codes(AA,List),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55
% list-check
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55
computeAnswer(_Ctx,_Votes,['*'],_,_):- !,trace,fail.

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

computeAnswer(Ctx,Votes,element(Tag,Attribs,List),Out,VotesO):-computeElement(Ctx,Votes,Tag,Attribs,List,Out,VotesO),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55
% Other Compounds
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55
computeAnswer(Ctx,Votes,GETATTRIBS, Resp,VotesO):- 
  convert_element(Ctx,GETATTRIBS,GETATTRIBS0), 
  GETATTRIBS \== GETATTRIBS0,!,
  trace,
  convert_element(Ctx,GETATTRIBS,_GETATTRIBS1),
  computeAnswer(Ctx,Votes,GETATTRIBS0, Resp,VotesO).

computeAnswer(Ctx,Votes,GETATTRIBS, Resp,VotesO):- GETATTRIBS=..[GET], isAimlTag(GET), !, computeElementMust(Ctx,Votes,GET,[],[],Resp,VotesO).
computeAnswer(Ctx,Votes,GETATTRIBS, Resp,VotesO):- GETATTRIBS=..[GET,ATTRIBS], isAimlTag(GET), !, computeElementMust(Ctx,Votes,GET,ATTRIBS,[],Resp,VotesO).
computeAnswer(Ctx,Votes,GETATTRIBS, Resp,VotesO):- GETATTRIBS=..[GET,ATTRIBS,INNER], isAimlTag(GET), !, computeElementMust(Ctx,Votes,GET,ATTRIBS,INNER,Resp,VotesO).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55
% errors
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55
computeAnswer(Ctx,Votes,element(Tag,Attribs,List),Out,VotesO):-trace,computeElement(Ctx,Votes,Tag,Attribs,List,Out,VotesO),!.
computeAnswer(_Ctx,Votes,Resp,Resp,Votes):-trace,aiml_error(computeAnswer(Resp)).


% ===============================================================================================
% Apply Input Match
% ===============================================================================================

computeSRAI(_Ctx,_Votes,[],_,_,_Proof):- !,trace,fail.
computeSRAI(Ctx,Votes,Input,Result,VotesO,Proof):-
   getAliceMem(Ctx,'robot','I',Robot),
   getAliceMem(Ctx,'robot','you',User),
   prolog_must(computeSRAI0(Ctx,Votes,fromTo(User,Robot),Input,Result,VotesO,Proof)).

computeSRAI0(Ctx,Votes,ConvThread,Input,Result,VotesO,Proof):- not(is_list(Input)),
   compound(Input),answerOutput(Input,InputO),Input\==InputO,!,
   computeSRAI0(Ctx,Votes,ConvThread,InputO,Result,VotesO,Proof).

computeSRAI0(Ctx,Votes,ConvThread,Input,Result,VotesO,Proof):- not(is_list(Input)),
   compound(Input),computeAnswer(Ctx,Votes,Input,InputO,VotesM),Input\==InputO,!,
   computeSRAI0(Ctx,VotesM,ConvThread,InputO,Result,VotesO,Proof).

computeSRAI0(Ctx,Votes,ConvThread,Input,Result,VotesO,Proof):- %%VotesO is Votes * 1.1,
  Each = (VotesO - e(Result,Proof)), %% VotesO make it keysort/2-able
   findall(Each, computeSRAI2(Ctx,Votes,ConvThread,Input,Result,VotesO,Proof), FOUND),  
  FOUND=[_|_],keysort(FOUND,FOUNDORDER),!,findall(E,member(E,FOUNDORDER),EL),!,member(Each,EL),   
   prolog_must(nonvar(Result)),
   debugFmt(computeSRAI(Input,Each)).

computeSRAI0(_Ctx,Votes,ConvThread,Input,Result,VotesO,Proof):- !, VotesO is Votes * 0.7,
     Result = ['I',heard,you,'say:'|Input],
      Proof = result(Result,failed_computeSRAI2(Votes,Input,ConvThread)),
      debugFmt(Proof),!.

% now trace is ok

% this next line is what it does on fallback
computeSRAI0(Ctx,Votes,ConvThread,[B|Flat],[B|Result],VotesO,Proof):-
   computeSRAI2(Ctx,Votes,ConvThread,Flat,Result,VotesO,Proof),prolog_must(nonvar(Result)).

computeSRAI222(CtxIn,Votes,ConvThread,Pattern,Out,VotesO,ProofOut):-          
         getCategoryArg(Ctx,'template',Out, _Out_ ,CateSig),!,
         getAliceMem(CtxIn,ConvThread,default('topic',['Nothing']),Topic),
         PreTopic = (CtxIn=Ctx),
         make_preconds_for_match(Ctx,'topic',Topic,CateSig,PreTopic,AfterTopic,Out,MinedCates,Proof),
         must_be_openCate(CateSig),
         getAliceMem(CtxIn,ConvThread,default('that',['<ConvThread>']),That),
         make_preconds_for_match(Ctx,'that',That,CateSig,AfterTopic,AfterThat,Out,MinedCates,Proof),
         must_be_openCate(CateSig),
         make_preconds_for_match(Ctx,'pattern',Pattern,CateSig,AfterThat,AfterPattern,Out,MinedCates,Proof),!,
         prolog_must(var(Out)),
         must_be_openCate(CateSig),!,
         prolog_must(atLeastOne((AfterPattern,CateSig))),         
         once((
            prolog_must(nonvar(Out)),
            cateStrength(CateSig,Mult),
            VotesO is Votes * Mult,
            append([Out],Proof,FinalProof),
            append(FinalProof,[CateSig],FinalProofClosed),
            %%debugFmt(result(out(Out),from(Pattern),FinalProofClosed)),
            ProofOut=..[proof|FinalProofClosed])).


cateStrength(_CateSig,1.1):-!.

computeSRAI2(Ctx,Votes,ConvThread,Pattern,Out,VotesO,ProofOut):- !, %% avoid next one
    computeSRAI222(Ctx,Votes,ConvThread,Pattern,Out,VotesO,ProofOut).

/*

computeSRAI2(Ctx,Votes,ConvThread,Pattern,Out,VotesO,Proof):- fail,
	 getLastSaidAsInput(That),     
         % MinedCates = _,
	 prolog_must(make_star_binders(Ctx,'that',That,MatchThat,CommitThat)),
         prolog_must(make_star_binders(Ctx,'pattern',Pattern,MatchPattern,CommitPattern)), 
         prolog_must(get_aiml_that(Ctx,MatchThat,MatchPattern,Out,Proof)),
   trace,
   rateMatch(MatchThat,That,Out,_NewThat,ThatVote,_ThatStar), 
   rateMatch(MatchPattern,ConvThread,Out,_Next,Voted,_), 
         prolog_must((CommitThat,CommitPattern)),
       debugOnFailureAimlTrace((
	%% flatten([Next],NextO),
	%% subst(NextO,topicstar,ThatStar,ThatStarO),
	 VotesO is Votes * (Voted + ThatVote))).


*/

getCategoryArg(Ctx,StarName,MatchPattern,Out,CateSig):-
   prolog_must(getCategoryArg0(Ctx,StarName,MatchPattern,Out,CateSig)),!.

getCategoryArg0(_Ctx,StarName,MatchPattern,_Out,CateSig):-atomic(StarName),!,
   aimlCateOrder(Order),
   nth1(StarNumber,Order,StarName),
   aimlCateSig(CateSig),!,
   prolog_must(arg(StarNumber,CateSig,MatchPattern)),!.

getCategoryArg0(Ctx,FAB,OutAOutB,Out,CateSig):- FAB=..[F,A,B],
      getCategoryArg(Ctx,A,OutA,Out,CateSig),!,
      getCategoryArg(Ctx,B,OutB,Out,CateSig),!,
      OutAOutB=..[F,OutA,OutB].


meansNothing(Atom,[*]):-atom(Atom),!.
meansNothing(InputNothing,InputPattern):-prolog_must((ground(InputNothing),var(InputPattern))),meansNothing0(InputNothing,InputPattern),!.

meansNothing0([Atom],Out):-!,meansNothing0(Atom,Out).
meansNothing0('_',['*']).
meansNothing0('*',['*']).

make_preconds_for_match(Ctx,StarName,InputNothing,CateSig,Preconds,Postconds,Out,MinedCates,ProofOut):-
   make_prepost_conds(Ctx,StarName,InputNothing,CateSig,FindPattern,CommitResult,Out,MinedCates,ProofOut),
   Postconds = (FindPattern,Preconds,CommitResult).
  

make_prepost_conds(Ctx,StarName,InputNothing,CateSig,FindPattern,CommitPattern,Out,_MinedCates,ProofOut):- 
  notrace(meansNothing(InputNothing,InputPattern)),!,trace,
  FindPattern = ((
     getCategoryArg(Ctx,StarName,MatchPattern,Out,CateSig),
     prolog_must(make_star_binders(Ctx,StarName,InputPattern,MatchPattern,CommitPattern)),
     once(addKeyValue(ProofOut,( StarName=InputNothing:MatchPattern))))).

make_prepost_conds(Ctx,StarName,TextPattern,CateSig,FindPattern,CommitPattern,Out,MinedCates,ProofOut):-
  generateMatchPatterns(Ctx,StarName,Out,TextPattern,CateSig,MinedCates,EachMatchSig),!,
  %%prolog_must(EachMatchSig=[_|_]),
  FindPattern = 
         ((
           getCategoryArg(Ctx,StarName,MatchPattern,Out,CateSig),
           member(_Order-MatchPattern, EachMatchSig),           
           prolog_must(make_star_binders(Ctx,StarName,TextPattern,MatchPattern,CommitInput)),

           (CommitPattern = prolog_must(once((
              
              once((atLeastOne(CommitInput),               
              addKeyValue(ProofOut,StarName=TextPattern:MatchPattern)
            %%debugFmt(getCategoryArg(Ctx,StarName,MatchPattern,Out,CateSig))
            ))))))
         )),!.
         

notSingletons(_Singleton_List):-!.

generateMatchPatterns(Ctx,StarName,Out,InputNothing,CateSig,_NC_MinedCates,_NC_EachMatchSig):- 
  notrace(meansNothing(InputNothing,_InputPattern)),!,   trace,
  must_be_openCate(CateSig),
  getCategoryArg(Ctx,StarName,'*',Out,CateSig),
  must_be_openCate(CateSig),!.

generateMatchPatterns(Ctx,StarName,Out,TextPattern,CateSig,_MinedCates,EachMatchSig):-
  convertToMatchable(TextPattern,InputPattern),
  must_be_openCate(CateSig),
  copy_term(CateSig,CateSigC),
  getCategoryArg(Ctx,StarName,MatchPattern,Out,CateSigC),
  findall(MatchPattern,CateSigC,AllMatchSig),!,sort(AllMatchSig,SetOfEachMatchSig),!,
  findall(Order-MatchPattern,(member(MatchPattern,SetOfEachMatchSig),canMatchAtAll2(StarName,InputPattern,MatchPattern,Order,_Len)),EachMatchSigU),
  keysort(EachMatchSigU,EachMatchSig),
  prolog_must(debugFmtList([
        starName = StarName,
        eachMatchSig(EachMatchSig),
        setOfEachMatchSig=SetOfEachMatchSig,
        eachMatchSig=EachMatchSig,
        matchPattern=MatchPattern,
        eachMatchSigU=EachMatchSigU,
        cateSig=CateSig
        ])).

callCatSig(Call,Arg,Length):-Call,argLength(Arg,Length).
argLength(Var,20):-var(Var),!.
argLength([X],N):-!,argLength(X,N),!.
argLength('*',10):-!.
argLength('_',8):-!.
argLength([L|IST],N):-length([L|IST],Len), N is 10 - Len,!.
argLength(_,40).


debugFmtList(ListI):-copy_term(ListI,List),debugFmtList0(List,List0),randomVars(List0),debugFmt(List0),!.
debugFmtList0([],[]):-!.
debugFmtList0([A|ListA],[B|ListB]):-debugFmtList1(A,B),!,debugFmtList0(ListA,ListB),!.

debugFmtList1(Value,Value):-var(Value),!.
debugFmtList1(Name=Number,Name=Number):-number(Number).
debugFmtList1(Name=Value,Name=Value):-var(Value),!.
debugFmtList1(Name=Value,Name=(len:Len)):-copy_term(Value,ValueO),append(ValueO,[],ValueO),is_list(ValueO),length(ValueO,Len),!.
debugFmtList1(Name=Value,Name=(F:A)):-functor(Value,F,A).
debugFmtList1(Value,shown(Value)).

canMatchAtAll2(StarName,A,B,Num,Len):-canMatchAtAll2(StarName,A,B,Num),argLength(B,Len),!.

:-setLogLevel(canMatchAtAll2,none).

canMatchAtAll2(StarName,A,B,10):- (var(A);var(B)),!, loggerFmt(canMatchAtAll2,canMatchAtAll_vars(StarName,A,B)),!. % success
canMatchAtAll2(_StarName,[],O,0):-!,O==[].
canMatchAtAll2(_StarName,O,[],0):-!,O==[].
canMatchAtAll2(StarName,I,STAR,Value):-isStar(StarName,STAR,Value),!,loggerFmt(canMatchAtAll2, canMatchAtAll_star(StarName,I,STAR)),!. % success
canMatchAtAll2(StarName,I,Atom,12):-atom(Atom),!,loggerFmt(canMatchAtAll2,canMatchAtAll_atom(StarName,I,Atom)),!. % success
canMatchAtAll2(_StarName,['*'],_M,7):-!,trace.
canMatchAtAll2(StarName,[I0|Pattern],[Match|MPattern],_):-member(M,[Match|MPattern]),requireableWord(StarName,M),not(member(M,[I0|Pattern])),!,fail.
canMatchAtAll2(StarName,[A|B],[A|BB],Count):-!,canMatchAtAll2(StarName,B,BB,Count),!.
canMatchAtAll2(_StarName,[A],[A],2):-!.


requireableWord(StarName,M):-not(isOptionalOrStar(StarName,M)).

isOptionalOrStar(_StarName,M):-not(atom(M)),!.
isOptionalOrStar(StarName,M):-isStar(StarName,M),!.

/*
isStar(StarName,'topic'):-!. %%,trace.
isStar(StarName,'that'):-!.
isStar(StarName,'input'):-!.
*/
isStar(StarName,Text):-isStar(StarName,Text,_Order),!.
isStar(StarName,Var,Val):-not(ground(Var)),trace,debugFmt(isStar(StarName,Var,Val)),!,fail.
isStar(_StarName,'*',6).
isStar(_StarName,'_',2).
isStar(StarName,A,8):-atom(StarName),!,A==StarName.
isStar(StarName,[X],N):-isStar(StarName,X,N),!.



%%must_be_openCate(_CateSig):-!.
must_be_openCate(CateSig):- prolog_must(hotrace((((nonvar(CateSig),not(ground(CateSig)),must_be_openCate0(CateSig)))))),!.
must_be_openCate0(CateSig):- arg(_,CateSig,Arg),must_be_openCateArgs(Arg,CateSig),fail.
must_be_openCate0(_CateSig):-!.

must_be_openCateArgs(Arg,_CateSig):-var(Arg),!.
must_be_openCateArgs('*',_CateSig):-!.
must_be_openCateArgs(List,CateSig):-trace, throw(List:CateSig),!.


make_star_binders(Ctx,StarName,InputText,MatchPattern,CommitPattern):-
 flag(StarName,_,1),
 (sub_matchit(Ctx,StarName,InputText,MatchPattern,CommitPattern) 
  ->
  (  nop(debugFmt(passed_matchit(Ctx,StarName,InputText,MatchPattern,CommitPattern))),
     prolog_must(nonvar(CommitPattern)),!)
  ;
  (nop(debugFmt(failed_matchit(Ctx,StarName,InputText,MatchPattern,CommitPattern))),CommitPattern=fail,fail)).

sub_matchit(_Ctx,StarName,InputPattern,_MatchPattern,CommitPattern):- 
   prolog_must(var(CommitPattern)),prolog_must(ground(StarName:InputPattern)),fail.
sub_matchit(_Ctx,_StarName,*,[Match|Pattern],_CommitPattern):-var(Match),var(Pattern),!,fail.

sub_matchit(Ctx,StarName,InputNothing,MatchPattern,CommitPattern):- 
   notrace((InputNothing \== '*',(InputPattern==StarName ; meansNothing(InputNothing,InputPattern)))),!,   trace,
   make_star_binders(Ctx,StarName,'*',MatchPattern,CommitPattern).

sub_matchit(_Ctx,_StarName,'*','*', true):-trace,!,fail.

sub_matchit(_Ctx,StarName,InputPattern,OutPattern,setStar(StarName,StarNum,InputPattern)):-isStar(StarName,OutPattern),
      flag(StarName,StarNum,StarNum+1),!.

/*
sub_matchit(_Ctx,StarName,InputPattern,OutPattern,nop(debugFmt(equality(StarName,InputPattern,OutPattern)))):-aimlMatches(InputPattern,OutPattern),!.
aimlMatches(_,Star):-isStar(StarName,Star),!.
aimlMatches(Star,_):-isStar(StarName,Star),!.
*/

sub_matchit(Ctx,StarName,TextPattern,MatchPattern,CommitPattern):-
  notrace(((convertToMatchable(TextPattern,InputPattern),TextPattern \== InputPattern))),!,
  make_star_binders(Ctx,StarName,InputPattern,MatchPattern,CommitPattern),!.


sub_matchit(Ctx,StarName,[Word,W1|InputPattern],[Word,W2|OutPattern],Why):-
      atom(Word),!,sub_matchit(Ctx,StarName,[W1|InputPattern],[W2|OutPattern],Why).

%%REAL-UNUSED sub_matchit(_Ctx,StarName,Pattern,MatchPattern,CommitPattern):- set_matchit(StarName,Pattern,MatchPattern,CommitPattern).

sub_matchit(_Ctx,StarName,Pattern,MatchPattern,CommitPattern):-
   matchit_eliminate_exacts(StarName,Pattern,MatchPattern,CommitPattern).


%%sub_matchit(_Ctx,StarName,InputPattern,OutPattern,debugFmt(fakeEquality(StarName,InputPattern,OutPattern))):-!.



%%REAL-UNUSED set_matchit(_StarName,Pattern,Pattern,true).
/*

set_matchit([_,Pattern|_],[_,Pattern|_]).
set_matchit([Pattern|_],[Pattern|_]).
set_matchit([_,_,Pattern|_],[_,Pattern|_]).
set_matchit([_,Pattern|_],[_,_,Pattern|_]).
%%set_matchit([Pattern|_],[_,Pattern|_]).
*/
%%REAL-UNUSED set_matchit(StarName,Pattern,['*'],setStar(StarName,_,Pattern)).

%%REAL-UNUSED set_matchit0(StarName,[H|Pattern],[H|Matcher],OnBind):-set_matchit1(StarName,Pattern,Matcher,OnBind).
%%REAL-UNUSED set_matchit0(StarName,[H|Pattern],['*'|Matcher],(setStar(StarName,_,H),OnBind)):-set_matchit1(StarName,Pattern,Matcher,OnBind).


matchit_eliminate_exacts(_StarName,[],[],true):-!.
matchit_eliminate_exacts(StarName,Pattern,[Star],true):-isStar(StarName,Star),!,setStar(StarName,_,Pattern),!.
matchit_eliminate_exacts(StarName,[H|Pattern],[Star,Right|Matcher],(setStar(StarName,_,Left),OnBind)):-
      isStar(StarName,Star),append(Left,[Right|More],[H|Pattern]),!,
      matchit_eliminate_exacts(StarName,[Right|More],[Right|Matcher],OnBind).

matchit_eliminate_exacts(StarName,[Word,W1|InputPattern],[Word,W2|OutPattern],OnBind):- atom(Word),!,
      matchit_eliminate_exacts(StarName,[W1|InputPattern],[W2|OutPattern],OnBind).
matchit_eliminate_exacts(StarName,[H|Pattern],[H|Matcher],OnBind):-matchit_eliminate_exacts(StarName,Pattern,Matcher,OnBind).


setStar(StarName,N,Pattern):-ignore((var(N),flag(StarName,N,N))),
   atom_concat(StarName,N,StarNameN),setAliceMem(_Ctx,user,StarNameN,Pattern),!,flag(StarName,NN,NN+1).

%%REAL-UNUSED set_matchit1(StarName,Pattern,Matcher,OnBind):- length(Pattern,MaxLen0), MaxLen is MaxLen0 + 2,
%%REAL-UNUSED    set_matchit2(StarName,Pattern,Matcher,MaxLen,OnBind).

/*
set_matchit2(StarName,Pattern,Matcher,MaxLen,(setStar(StarName,_,Left),setStar(StarName,_,Right))):-member(Mid,[[_,_,_,_],[_,_,_],[_,_],[_]]),
   sublistspan(Left,Mid,Right,Pattern,MaxLen),
   set_matchit3(StarName,Pattern,Matcher,MaxLen,Mid).

set_matchit3(_StarName,_Pattern,Matcher,MaxLen,Mid):-sublistspan(_Left,Mid,_Right,Matcher,MaxLen).



%%sublistspan(Mid,Full,MaxLen):-sublistspan(_Left,Mid,_Right,Full,MaxLen).
sublistspan(Left,Mid,Right,Full,MaxLen):-append(Left,MidRight,Full),append(Mid,Right,MidRight),length(Full,FLen),FLen =< MaxLen.


%get_aiml_that(Ctx,SaidThat,Match,OOut):-get_aiml_cyc(SaidThat,Match,Out),(([srai(Out)] = OOut);OOut=Out).
get_aiml_that(_Ctx,SaidThat,Match,Out,what(SaidThat, Match,Out)):-what(SaidThat, Match,Out).
get_aiml_that(_Ctx,['*'],Match,Out,response(Match,Out)):-response(Match,Out).

get_aiml_that(_CTX,[T|HAT],MATCH,OUT,aimlCate(GRAPH,PRECALL,TOPIC,[T|HAT],INPUT,MATCH,FLAGS,CALL,GUARD,USERDICT,OUT,LINENO,SRCFILE)):-
    aimlCate(GRAPH,PRECALL,TOPIC,[T|HAT],INPUT,MATCH,FLAGS,CALL,GUARD,USERDICT,OUT,LINENO,SRCFILE).
get_aiml_that(_CTX,['*'],MATCH,OUT,aimlCate(GRAPH,PRECALL,TOPIC,(*),INPUT,MATCH,FLAGS,CALL,GUARD,USERDICT,OUT,LINENO,SRCFILE)):-
    aimlCate(GRAPH,PRECALL,TOPIC,(*),INPUT,MATCH,FLAGS,CALL,GUARD,USERDICT,OUT,LINENO,SRCFILE).

%get_aiml_cyc(['*'],[String|ListO],[Obj,*]):-poStr(Obj,[String|List]),append(List,['*'],ListO).
%get_aiml_cyc(['*'],[String,*],[Obj,*]):-poStr(Obj,String).

*/


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

getAliceMem(Ctx,Dict,Name,[Value]):-nonvar(Value),!,getAliceMem(Ctx,Dict,Name,Value),!.
getAliceMem(Ctx,Dict,Name,Value):-is_list(Dict),last(Dict,Last),!,getAliceMem(Ctx,Last,Name,Value),!.
getAliceMem(_Ctx,Dict,Name,Value):-dict(Dict,Name,Value),!.
getAliceMem(Ctx,Dict,Name,Value):-atom(Dict),downcase_atom(Dict,Down),Dict\=Down,!,getAliceMem(Ctx,Down,Name,Value),!.
getAliceMem(Ctx,Dict,Name,Value):-atom(Name),downcase_atom(Name,Down),Name\=Down,dict(Dict,Down,_SomeValue),!,getAliceMem(Ctx,Dict,Down,Value),!.
getAliceMem(Ctx,Dict,default(Name,Default),Value):-getAliceMem(Ctx,Dict,Name,'OM'),!,Default=Value.
getAliceMem(_Ctx,_Dict,_Name,OM):-'OM'==OM,!.
getAliceMem(_Ctx,_Dict,Name,[unknown,Name]):-!.

setAliceMem(Ctx,Dict,Name,[Value]):-nonvar(Value),!,setAliceMem(Ctx,Dict,Name,Value),!.
%setAliceMem(Ctx,Dict,Name,Value):-immediateCall(Ctx,setCurrentAliceMem(Dict,Name,Value)),fail.
%%setAliceMem(_Ctx,_Dict,_Name,'nick').
setAliceMem(Ctx,Dict,Name,Value):-atom(Dict),downcase_atom(Dict,Down),Dict\=Down,!,setAliceMem(Ctx,Down,Name,Value).
setAliceMem(Ctx,Dict,Name,Value):-atom(Name),downcase_atom(Name,Down),Name\=Down,!,setAliceMem(Ctx,Dict,Down,Value).
setAliceMem(Ctx,Dict,default(Name),DefaultValue):-getAliceMem(Ctx,Dict,Name,'OM')->setAliceMem(Ctx,Dict,Name,DefaultValue);true.
setAliceMem(_Ctx,Dict,Name,Value):- ignore(retract(dict(Dict,Name,B))),ignore(B='OM'),retractall(dict(Dict,Name,_)),
   asserta(dict(Dict,Name,Value)).%%,debugFmt('/* ~q. */~n',[dict(Dict,Name,B->Value)]),!.
:-dynamic(dict/3).

setCurrentAliceMem(Dict,X,E):-currentContext(setCurrentAliceMem(Dict,X,E),Ctx), setAliceMem(Ctx,Dict,X,E).


% ===============================================================================================
% Get and rember Last Said
% ===============================================================================================

%%:-dynamic(getLastSaid/1). 

%%:-setAliceMem(_,_,that,(['where',am,'I'])).

rememberSaidIt(_Ctx,[]):-!.
rememberSaidIt(Ctx,_-R1):-!,rememberSaidIt(Ctx,R1).
rememberSaidIt(Ctx,R1):-append(New,'.',R1),!,rememberSaidIt(Ctx,New).
rememberSaidIt(Ctx,R1):-answerOutput(R1,SR1),!,
   getItemValue('me',Ctx,Robot),
   setAliceMem(Ctx,Robot,'lastSaid',SR1).

getRobot(Robot):-trace,getAliceMem(_Ctx,'robot','me',Robot),!.

getLastSaid(LastSaid):-getRobot(Robot),getAliceMem(_Ctx,Robot,'lastSaid',LastSaid).

getLastSaidAsInput(LastSaidMatchable):-getLastSaid(That),convertToMatchable(That,LastSaidMatchable),!.

:-setCurrentAliceMem('robot','me','Robot').
:-setCurrentAliceMem('robot','you','user').

% ===============================================================================================
% Template Output to text
% ===============================================================================================

answerOutput(Output,NonVar):-nonvar(NonVar),answerOutput(Output,Var),!,valuesMatch(_Ctx,Var,NonVar).
answerOutput(Output,[Output]):-var(Output),!.
answerOutput([],Output):- !, Output=[].
answerOutput(Output,Split):-atom(Output),atomSplit(Output,Split),!.
answerOutput(Output,[Output]):-atomic(Output),!.
answerOutput([A|AA],Output):-!,
   answerOutput(A,B),
   answerOutput(AA,BB),
   flatten([B,BB],Output).
answerOutput(element(Tag,Attribs,InnerXML),[element(Tag,Attribs,Output)]):-trace,answerOutput(InnerXML,Output).
answerOutput(Term,Output):-compound(Term),arg(1,Term,Mid),!,answerOutput(Mid,Output).
answerOutput(Output,[Output]):-!.

% ===============================================================================================
% Convert to Matchable
% ===============================================================================================

convertToMatchable(That,LastSaid):-
      answerOutput(That,AA),!,
      deleteAll(AA,['.','!','?','\'','!','','\n'],Words),
      toUppercase(Words,LastSaid),!.

deleteAll(A,[],A):-!.
deleteAll(A,[L|List],AA):-delete(A,L,AAA),deleteAll(AAA,List,AA),!.
% ===============================================================================================
% Degrade Response
% ===============================================================================================

:-dynamic(degraded/1).

degrade(_-OR):-!,degrade(OR).
degrade(OR):-asserta(degraded(OR)).

aimlDebugFmt(X):-debugFmt(X),!.


