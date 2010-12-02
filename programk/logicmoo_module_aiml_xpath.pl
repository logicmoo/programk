% ===================================================================
% File 'logicmoo_module_aiml_xpath.pl'
% Purpose: An Implementation in SWI-Prolog of AIML
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'logicmoo_module_aiml_xpath.pl' 1.0.0
% Revision:  $Revision: 1.7 $
% Revised At:   $Date: 2002/07/11 21:57:28 $
% ===================================================================

%:-module()
%:-include('logicmoo_utils_header.pl'). %<?
%:- style_check(-singleton).
%%:- style_check(-discontiguous).
:- style_check(-atom).
:- style_check(-string).


% ===============================================================================================
% get / set  Global Variables
% ===============================================================================================
:-dynamic(dict/3).
:-multifile(dict/3).

getAliceMemElse(Ctx,Dict,Name,ValueO):-getAliceMemComplete(Ctx,Dict,Name,ValueO),!.
getAliceMemElse(_Ctx,Dict,Name,[Dict,(s),unknown,Name]).

getAliceMem(Ctx,Dict,DEFAULT,ValueOut):- compound(DEFAULT),DEFAULT=default(Name,Default),!, 
     (getAliceMemComplete(Ctx,Dict,Name,ValueO) -> xformOutput(ValueO, ValueOut)  ; xformOutput(Default, ValueOut)). 

getAliceMem(Ctx,IDict,NameI,ValueO):-
     dictNameDictNameC(Ctx,IDict,NameI,Dict,Name),!,
     getAliceMem(Ctx,Dict,Name,ValueO).
getAliceMem(Ctx,Dict,Name,ValueO):- var(ValueO), !, getAliceMemElse(Ctx,Dict,Name,ValueO),!.

getAliceMem(Ctx,Dict,Name,'OM'):- !,not((getAliceMemComplete(Ctx,Dict,Name,ValueO),ValueO\=='OM')).
%%getAliceMem(Ctx,Dict,Name,ValueI):- %%unresultifyC(ValueI,ValueM),!,getAliceMem(Ctx,Dict,Name,ValueO),!,sameBinding(ValueI,ValueO).%%prolog_must(nonvar(ValueI)),!.
getAliceMem(Ctx,Dict,Name,ValueI):- getAliceMemComplete(Ctx,Dict,Name,ValueO),!,sameBinding(ValueI,ValueO).

getAliceMemComplete(Ctx,Dict,Name,ValueO):-getInheritedStoredValue(Ctx,Dict,Name,ValueO),!.

dictNameKey(_Dict,Dict:Name,Key):-!,dictNameKey(Dict,Name,Key).
dictNameKey(Dict,Name,Dict:Name).
dictNameKey(_Dict,NameKey,NameKey):-!.

getStoredValue(_Ctx,_Dict,_Name,Value):-prolog_must(var(Value)),fail.

getStoredValue(Ctx,IDict,NameI,ValueO):-
     dictNameDictNameC(Ctx,IDict,NameI,Dict,Name),!,
     getStoredValue(Ctx,Dict,Name,ValueO).

getStoredValue(Ctx,Dict,Name,Value):-getContextStoredValue(Ctx,Dict,Name,Value),!.

% ===============================================================================================
% named context via inheritance
% ===============================================================================================

getInheritedStoredValue(Ctx,IScope,NameI,Value):-dictNameDictNameC(Ctx,IScope,NameI,Scope,Name),!,getInheritedStoredValue(Ctx,Scope,Name,Value).
getInheritedStoredValue(Ctx,Scope,DEFAULT,ValueOut):- compound(DEFAULT),DEFAULT=default(Name,Default),!, 
         (getInheritedStoredValue(Ctx,Scope,Name,ValueO) -> xformOutput(ValueO, ValueOut)  ; xformOutput(Default, ValueOut)). 

getInheritedStoredValue(Ctx,Scope,Name,Value):- getStoredValue(Ctx,Scope,Name,Value).
getInheritedStoredValue(Ctx,Scope,Name,Value):- atom(Scope),inheritedFrom(Scope,InHerit),getInheritedStoredValue(Ctx,InHerit,Name,Value).
 
inheritedFrom([Scope],To):-nonvar(Scope),!,inheritedFrom(Scope,To).
inheritedFrom(default,_):-!,fail.
inheritedFrom(defaultValue(_),_):-!,fail.
inheritedFrom(Scope,defaultValue(Scope)).
inheritedFrom(_Scope,default).
inheritedFrom(user,_):-!,fail.
inheritedFrom(Atom,user):-atom(Atom).


getIndexedValue(Ctx,IDict,Name,MajorMinor,Value):-unresultifyC(IDict,Dict),!,
    getIndexedValue(Ctx,Dict,Name,MajorMinor,Value),!.

getIndexedValue(Ctx,Dict,Name,[],Value):-!,
    getIndexedValue(Ctx,Dict,Name,[1],Value).

getIndexedValue(Ctx,Dict,Name,Major,Value):-atomic(Major),!,
    getIndexedValue(Ctx,Dict,Name,[Major],Value).

getIndexedValue(Ctx,Dict,Name,[Minor],Value):-atomic(Minor),!,
    getIndexedValue(Ctx,Dict,Name,[1,Minor],Value).

getIndexedValue(Ctx,Dict,Name,[Major,SEP|Minor],Value):- member(SEP,[',',':']),!,
    getIndexedValue(Ctx,Dict,Name,[Major|Minor],Value).

getIndexedValue(Ctx,Dict,Name,MajorMinor,ValueO):- numberFyList(MajorMinor,MajorMinorM),MajorMinor\==MajorMinorM,!,
   getIndexedValue(Ctx,Dict,Name,MajorMinorM,ValueO).

getIndexedValue(Ctx,Dict,DEFAULT,MajorMinor,ValueOut):- compound(DEFAULT),DEFAULT=default(Name,Default),!,
    (getIndexedValue(Ctx,Dict,Name,MajorMinor,ValueO)  -> xformOutput(ValueO, ValueOut)  ; xformOutput(Default, ValueOut)). 


getIndexedValue(Ctx,Dict,Name,MajorMinor,ValueO):-
    notrace(getIndexedValue0(Ctx,Dict,Name,MajorMinor,Value)),
    xformOutput(Value,ValueO).
   
getIndexedValue(Ctx,Dict,Name,MajorMinor,ValueO):-
    unify_listing(getContextStoredValue(Ctx,Dict,_N,_V)),
    %%unify_listing(getContextStoredValue(Ctx,_,Name,_)),
    getIndexedValue0(Ctx,Dict,Name,MajorMinor,Value),
    xformOutput(Value,ValueO).

getIndexedValue0(Ctx,Dict,Name,[Major|Minor],Value):-
   getMajorMinorIndexedValue(Ctx,Dict,Name,Major,Minor,Value),!.

getMajorMinorIndexedValue(Ctx,Dict,Name,Major,Minor,Value):-
   indexOntoKey(Name,Major,Item),
   getInheritedStoredValue(Ctx,Dict,Item,ValueS),!,
   getMajorMinorIndexedValue0(Ctx,Dict,Name,Major,Minor,ValueS,Value).

getMajorMinorIndexedValue0(_Ctx,_Dict,_Name,_Major,Minor,ValueS,Value):-
   getMinorSubscript(ValueS,Minor,Value),!.

getMajorMinorIndexedValue0(Ctx,Dict,Name,Major,[M|Minor],ValueS,Value):-
   prolog_must(is_list(ValueS)),
   length(ValueS,ValueSLen),
   MajorN is Major + 1,
   N is M - ValueSLen,
   trace,
   getMajorMinorIndexedValue(Ctx,Dict,Name,MajorN,[N|Minor],Value),!.


numberFyList([],[]).
numberFyList([A|MajorMinor],[B|MajorMinorM]):-
  atom(A),atom_to_number(A,B),
  numberFyList(MajorMinor,MajorMinorM),!.
numberFyList([A|MajorMinor],[A|MajorMinorM]):-numberFyList(MajorMinor,MajorMinorM).

isStarValue(Value):-ground(Value),not([_,_|_]=Value),member(Value,[[ValueM],ValueM]),!,member(ValueM,['*','_']),!.

xformOutput(Value,ValueO):-isStarValue(Value),!,trace,Value=ValueO.
xformOutput(Value,ValueO):-listify(Value,ValueL),Value\==ValueL,!,xformOutput(ValueL,ValueO).
xformOutput(Value,Value).

subscriptZeroOrOne(Major):-nonvar(Major),member(Major,[0,1,'0','1']).



%% getMinorSubscript(Items,Minor,Value).
getMinorSubscript(ItemsO,Index,Value):- not(is_list(ItemsO)),answerOutput(ItemsO,Items),prolog_must(is_list(Items)),getMinorSubscript(Items,Index,Value),!.
getMinorSubscript(Items,'*',Value):-!,prolog_must(flatten(Items,Value)),!.
getMinorSubscript(Items,',',Value):- throw_safe(getMinorSubscript(Items,',',Value)), !,prolog_must(=(Items,Value)),!.
getMinorSubscript(Items,[A|B],Value):-!,getMinorSubscript(Items,A,ValueS),!,getMinorSubscript(ValueS,B,Value),!.
getMinorSubscript(Items,[],Value):-!,xformOutput(Items,Value),!.
getMinorSubscript(Items,ANum,Value):-not(number(ANum)),!,prolog_must(atom_to_number(ANum,Num)),!,getMinorSubscript(Items,Num,Value).
%%%
getMinorSubscript(Items,Num,Value):- prolog_must(is_list(Items)),length(Items,Len),Index is Len-Num,nth0(Index,Items,Value),is_list(Value),!.
getMinorSubscript([],1,[]):-!.
getMinorSubscript(Items,1,Value):- trace,last(Items,Last), (is_list(Last)->Value=Last;Value=Items),!.
getMinorSubscript(Items,1,Value):- xformOutput(Items,Value),!,trace.
getMinorSubscript(Items,Num,Value):-debugFmt(getMinorSubscriptFailed(Items,Num,Value)),trace,fail.

getUserDicts(User,Name,Value):-isPersonaUser(User),isPersonaPred(Name),once(getInheritedStoredValue(_Ctx,User,Name,Value)).

isPersonaUser(User):-findall(User0,getContextStoredValue(_Ctx,User0,'is_type','agent'),Users),sort(Users,UsersS),!,member(User,UsersS).
isPersonaPred(Name):-findall(Pred,(getContextStoredValue(_Ctx,_Dict,Pred,_Value),atom(Pred)),Preds),sort(Preds,PredsS),!,member(Name,PredsS).



% ===============================================================================================
% substs dictionaries
% ===============================================================================================
addReplacement(Ctx,IDict,Find,Replace):-dictNameDictNameC(Ctx,IDict,before,Dict,before),!,addReplacement(Ctx,Dict,Find,Replace).
addReplacement(Ctx,SubstsNameI,Find,Replace):-
      convert_dictname(Ctx,SubstsNameI,SubstsName),SubstsNameI \== SubstsName,
      addReplacement(Ctx,SubstsName,Find,Replace).
addReplacement(Ctx,SubstsName,Find,Replace):-
      convert_substs(Find,FindM),
      convert_replacement(Ctx,Replace,ReplaceM),
      (Replace\==ReplaceM;Find\==FindM),!,
      addReplacement(Ctx,SubstsName,FindM,ReplaceM).
addReplacement(Ctx,Dict,Find,Replace):- immediateCall(Ctx,addReplacement(Dict,Find,Replace)),fail.
addReplacement(_Ctx,Dict,Find,Replace):- assertz(dict(substitutions(Dict),Find,Replace)),!.

addReplacement(Dict,Find,Replace):-currentContext(addReplacement(Dict,Find,Replace),Ctx), addReplacement(Ctx,Dict,Find,Replace).


% ===============================================================================================
% context/name cleanups
% ===============================================================================================
dictNameDictNameC(Ctx,IDict,NameI,Dict,Name):-dictNameDictName(Ctx,IDict,NameI,Dict,Name),!, IDict+NameI \==Dict+Name.

dictNameDictName(Ctx,IDict,NameI,Dict,Name):- hotrace(dictNameDictName0(Ctx,IDict,NameI,Dict,Name)).
dictNameDictName0(Ctx,_Dict,D:NameI,Dict,Name):- nonvar(D),!,dictNameDictName(Ctx,D,NameI,Dict,Name).
dictNameDictName0(Ctx,IDict,NameI,Dict,Name):- convert_dictname(Ctx,IDict,Dict),unresultifyL(Ctx,NameI,Name).

unresultifyL(Ctx,NameI,Name):-unresultifyLL(Ctx,NameI,NameU),toLowerIfAtom(NameU,Name),!.

unresultifyLL(Ctx,NameI,NameO):-unresultify(NameI,Name),NameI \== Name,!,unresultifyLL(Ctx,Name,NameO).
unresultifyLL(Ctx,NameI,NameO):-is_list(NameI),lastMember(Name,NameI),!,unresultifyLL(Ctx,Name,NameO).
unresultifyLL(_Ctx,Name,Name).
toLowerIfAtom(Dict,Down):-atom(Dict),downcase_atom(Dict,Down),!.
toLowerIfAtom(Dict,Dict).


% ===============================================================================================
% Setting globals
% ===============================================================================================
setAliceMem(Ctx,IDict,NameI,Value):-dictNameDictNameC(Ctx,IDict,NameI,Dict,Name),!,setAliceMem(Ctx,Dict,Name,Value),!.
setAliceMem(Ctx,Dict,Name,Var):-var(Var),!,setAliceMem(Ctx,Dict,Name,['$var'(Var)]).
setAliceMem(Ctx,Dict,Name,Atomic):-atomic(Atomic),Atomic\==[],!,setAliceMem(Ctx,Dict,Name,[Atomic]).
setAliceMem(Ctx,Dict,Name,NonList):-not(is_list(NonList)),trace,!,setAliceMem(Ctx,Dict,Name,[NonList]).
setAliceMem(Ctx,IDict,Name,Value):-is_list(IDict),!,foreach(member(Dict,IDict),setAliceMem(Ctx,Dict,Name,Value)),!.
setAliceMem(Ctx,Dict,Name,Value):-immediateCall(Ctx,setCurrentAliceMem(Dict,Name,Value)),fail.
setAliceMem(Ctx,Dict,Name,Value):-isStarValue(Value),debugFmt(setAliceMem(Ctx,Dict,Name,Value)),trace.
setAliceMem(Ctx,Dict,default(Name),DefaultValue):-getAliceMem(Ctx,Dict,Name,'OM')->setAliceMem(Ctx,Dict,Name,DefaultValue);true.
setAliceMem(Ctx,Dict,Name,Value):-resetAliceMem(Ctx,Dict,Name,Value),!.

% ===============================================================================================
%    AIML Runtime Database
% ===============================================================================================

quitely(Ctx):-getCtxValueElse(quiteMemOps,Ctx,True,false),!,True=true.

resetAliceMem(Ctx,IDict,NameI,Value):- dictNameDictName(Ctx,IDict,NameI,Dict,Name),
   % for printing
   %%%traceIf(Dict==filelevel),
   currentContextValue(Ctx,Dict,Name,B),   
   (atom_contains(Name,'(')->true;(not(quitely(Ctx))->true;debugFmt('/* ~q. */',[dict(Dict,Name,B->Value)]))),
   % for cleaning
   clearContextValues(Ctx,Dict,Name),
   % for setting
   addNewContextValue(Ctx,Dict,Name,Value),!.

setCurrentAliceMem(Dict,X,E):-currentContext(setCurrentAliceMem(Dict,X,E),Ctx), setAliceMem(Ctx,Dict,X,E).

%%getContextStoredValue(Ctx,Dict,Name,Value):-dictNameKey(Dict,Name,Key),debugOnError(getCtxValue(Key,Ctx,Value)),valuePresent(Value).
currentContextValue(Ctx,Dict,Name,Value):- getContextStoredValue(Ctx,Dict,Name,Value),!.
currentContextValue(_Ctx,_Dict,_Name,'OM'):-!.

getContextStoredValue(Ctx,IDict,NameI,Value):-dictNameDictNameC(Ctx,IDict,NameI,Dict,Name),!,getContextStoredValue(Ctx,Dict,Name,Value).
getContextStoredValue(_Ctx,Dict,Name,ValueO):- copy_term(ValueO,ValueI),dict(Dict,Name,ValueI),valuePresent(ValueI), ValueO=ValueI.

removeContextValue(Ctx,IDict,NameI,Value):-dictNameDictName(Ctx,IDict,NameI,Dict,Name),copy_term(Value,Kill),ignore(retract(dict(Dict,Name,Kill))).
clearContextValues(Ctx,IDict,NameI):-dictNameDictName(Ctx,IDict,NameI,Dict,Name),retractall(dict(Dict,Name,_Value)).

addNewContextValue(Ctx,IDict,NameI,Value):-dictNameDictNameC(Ctx,IDict,NameI,Dict,Name),!,addNewContextValue(Ctx,Dict,Name,Value).
addNewContextValue(Ctx,Dict,Name,OM):-OM=='OM',!,clearContextValues(Ctx,Dict,Name),!.
addNewContextValue(Ctx,Dict,Name,Value):- 
   dictNameKey(Dict,Name,Key),
   addCtxValue(Key,Ctx,Value),
   ifThen(nonvar(Value),asserta(dict(Dict,Name,Value))),
   ifThen(not(ground(Value)),debugFmt(addCtxValue(Key,Ctx,Value))).

% ===============================================================================================
%    Context values API
% ===============================================================================================

pushAttributes(Ctx,Scope,List):-pushCtxFrame(Scope,Ctx,List),pushAttributes0(Ctx,Scope,List),!.
pushAttributes0(Ctx,Scope,[N=V|L]):-pushNameValue(Ctx,Scope,N,V),pushAttributes0(Ctx,Scope,L).
pushAttributes0(_Ctx,_Scope,[]).

peekAttributes(Ctx,[Name|SList],Scope,[Name=Value|Results]):- peekNameValue(Ctx,Scope,Name,Value,'$error'),peekAttributes(Ctx,SList,Scope,Results),!.
peekAttributes(_Ctx,[],_Scope,[]):-!.

current_value(Ctx,arg(N),Value):-integer(N), N = -1,!,getItemValue(lastArg,Ctx,Value),!.
current_value(Ctx,Name,Value):-Name==lastArg,compound(Ctx),functor(Ctx,_F,A),arg(A,Ctx,Value),!.
current_value(Ctx,Name,Ctx):-Name==lastArg,!.
current_value(Ctx,arg(N),Value):-integer(N),prolog_must(compound(Ctx)),arg(N,Ctx,Value),!.
current_value(Ctx,Name,Value):-peekNameValue(Ctx,_,Name,Value,'$error').


%%peekNameValue(Ctx,Scope,Name,Value):-Failed='$error',peekNameValue(Ctx,Scope,Name,Value,Failed),ignore((Value==Failed,trace,Value = '*')),!.

peekNameValue(Ctx,Scope,Name,Value,Else):-nonvar(Value),!,checkNameValue(Ctx,Scope,Name,Value,Else).
peekNameValue(Ctx,_Scope,Name,Value,_ElseVar):-notrace(getCtxValue(Name,Ctx,Value)),!.
peekNameValue(Ctx,List,Name,Value,_ElseVar):- nonvar(List),not(atom(List)),attributeOrTagValue(Ctx,List,Name,Value,'$failure'),!.
peekNameValue(Ctx,Scope,Name,Value,_ElseVar):-getStoredValue(Ctx,Scope,Name,Value),checkAttribute(Scope,Name,Value),!.
%%%%%%peekNameValue(Ctx,Scope,Name,Value,_ElseVar):-nonvar(Scope),getStoredValue(Ctx,Scope2,Name,Value),Scope\=Scope2,checkAttribute(Scope2,Name,Value),!,checkValue(Value),!.
peekNameValue(Ctx,Scope,Name,Value,_ElseVar):-getInheritedStoredValue(Ctx,Scope,Name,Value),checkAttribute(Scope,Name,Value),!.
peekNameValue(Ctx,Scope,Name,Value,_ElseVar):-is_list(Name),member(N0,Name),peekNameValue(Ctx,Scope,N0,Value,'$failure'),!.
peekNameValue(Ctx,_Scope,Name,Value,ElseVar):-makeParamFallback(Ctx,Name,Value,ElseVar),!.

%%peekNameValue(_Ctx,_Scope,_Name,Value,ElseVar):-ignore(Value=ElseVar),!.

checkNameValue(Ctx,Scope,[Name],Value,Else):- nonvar(Name),!,checkNameValue(Ctx,Scope,Name,Value,Else).
checkNameValue(Ctx,Scope,Name,Value,Else):-notrace(( peekNameValue(Ctx,Scope,Name,ValueVar,Else),!,checkValue(ValueVar),valuesMatch(Ctx,ValueVar,Value))),!. %%,trace.

valuesMatch(_Ctx,V,A):-V=A,!.
valuesMatch(_Ctx,_V,A):-A=='*',!.
valuesMatch(_Ctx,V,_A):-V=='*',!.
valuesMatch(Ctx,V,A):-compound(V),convertToMatchable(V,VV),!,valuesMatch0(Ctx,VV,A).
valuesMatch(Ctx,V,A):-compound(A),convertToMatchable(A,AA),!,valuesMatch0(Ctx,V,AA).
valuesMatch(Ctx,V,A):-valuesMatch0(Ctx,V,A),!.

valuesMatch0(_Ctx,V,A):-V=A,!.
valuesMatch0(Ctx,[V|VV],[A|AA]):-valuesMatch(Ctx,V,A),!,valuesMatch(Ctx,VV,AA).
valuesMatch0(_Ctx,V,A):-sameBinding(V,A),!.
valuesMatch0(Ctx,[V],A):-!,valuesMatch(Ctx,V,A).
valuesMatch0(Ctx,V,[A]):-!,valuesMatch(Ctx,V,A).


valueMP(Var,M):- member(M, [var(Var), Var=missing, Var=[], Var=(*) ,  Var=('_') , (Var=(-(_))) ]),M,!.
valueMP(V,(V='ERROR')):-prolog_must(ground(V)),term_to_atom(V,A), concat_atom([_,_|_],'ERROR',A),!.


checkValue(Value):- valueMP(Value,M),throw_safe(M),!.
checkValue(_):-!.

valuePresent(Value):- var(Value),!,fail.
valuePresent(result(_)):- !.
valuePresent(Value):- valueMP(Value,_M),!,fail.
valuePresent(_):-!.

% ===============================================================================================
%    push/pop values API
% ===============================================================================================

popAttributes(Ctx,Scope,[N=V|L]):- !,checkAttribute(Scope,N,V),popNameValue(Ctx,Scope,N,V),!,popAttributes(Ctx,Scope,L),!.
popAttributes(_Ctx,_Scope,[]).

withAttributes(_Ctx,ATTRIBS,Call):-ATTRIBS==[],!,Call.
withAttributes(Ctx,ATTRIBS,Call):-
  hotrace((
   ensureScope(Ctx,ATTRIBS,Scope),
   checkAttributes(Scope,ATTRIBS))),
   call_cleanup((
    once(hotrace(pushAttributes(Ctx,Scope,ATTRIBS))),
      Call),
    once(hotrace(popAttributes(Ctx,Scope,ATTRIBS)))).

checkAttributes(Scope,ATTRIBS):-prolog_must(nonvar(ATTRIBS)),maplist(checkAttribute(Scope),ATTRIBS).
checkAttribute(Scope,N=V):-checkAttribute(Scope,N,V).
checkAttribute(Scope,N,_V):-N==proof,!,prolog_must(nonvar(Scope)).
checkAttribute(Scope,N,V):-prolog_must(nonvar(Scope)),prolog_must(nonvar(N)),!,prolog_must(nonvar(V)).

pushNameValue(Ctx,Scope,N,V):-
   checkAttribute(Scope,N,V),
   resetAliceMem(Ctx,Scope,N,V),!.
   %%asserta(dict(Scope,N,V)),!.

popNameValue(Ctx,Scope,N,V):-
   currentContextValue(Ctx,Scope,N,V),
   checkAttribute(Scope,N,V),
   removeContextValue(Ctx,Scope,N,V),
   checkAttribute(Scope,N,V),!.

%dyn_retract(dict(Scope,N,V)):-(retract(dict(Scope,N,V))),!.

ensureScope(_Ctx,_ATTRIBS,Scope):-nonvar(Scope),!.
ensureScope(_Ctx,_ATTRIBS,filelevel):-!.



replaceAttribute(Ctx,Before,After,ALIST,ATTRIBS):- replaceAttribute0(Ctx,Before,After,ALIST,AA),list_to_set_safe(AA,ATTRIBS),!.
% the endcase
replaceAttribute0(_Ctx,_Before,_After,[],[]):-!.
% only do the first found?
replaceAttribute0(_Ctx,Before,After,[Before=Value|ATTRIBS],[After=Value|ATTRIBS]):-prolog_must(ground(Before+After+Value+ATTRIBS)),!.
% comment out the line above to do all
replaceAttribute0(Ctx,Before,After,[Before=Value|ALIST],[After=Value|ATTRIBS]):-
   replaceAttribute0(Ctx,Before,After,ALIST,ATTRIBS),!.
% skip over BeforeValue
replaceAttribute0(Ctx,Before,After,[BeforeValue|ALIST],[BeforeValue|ATTRIBS]):-
   replaceAttribute0(Ctx,Before,After,ALIST,ATTRIBS),!.
% the last resort
replaceAttribute0(_Ctx,_Before,_After,B,B):-!.



makeAllParams(Ctx,[O|Order],Assert,UnboundDefault,[Tag=RR|Result]):-
   makeSingleTag(Ctx,O,Assert,UnboundDefault,Tag,RR),prolog_must(O\==RR),
   makeAllParams(Ctx,Order,Assert,UnboundDefault,Result),!.
makeAllParams(_Ctx,[],_,_,[]).


makeSingleTag(Ctx,Name,ATTRIBS,Default,Tag,Result):-atom(Name),!,makeSingleTag(Ctx,[Name],ATTRIBS,Default,Tag,Result),!.
makeSingleTag(Ctx,NameS,ATTRIBS,Default,Tag,ValueO):-makeAimlSingleParam0(Ctx,NameS,ATTRIBS,Default,Tag,ValueI),
      transformTagData(Ctx,Tag,Default,ValueI,ValueO),!.

makeAimlSingleParam0(_Ctx,[N|NameS],ATTRIBS,_D,N,Value):-member(O,[N|NameS]),lastMember(OI=Value,ATTRIBS),atomsSameCI(O,OI),!,prolog_must(N\==Value).
makeAimlSingleParam0(Ctx,[N|NameS],_,ElseVar,N,Value):- makeParamFallback(Ctx,[N|NameS],Value,ElseVar),!,prolog_must(N\==Value).


% ===============================================================================================
%  Fallback
% ===============================================================================================

makeParamFallback(Ctx,Name,Value,ElseVar):-var(ElseVar),!,throw_safe(makeParamFallback(Ctx,Name,Value,ElseVar)).
makeParamFallback(Ctx,Name,Value,ElseVar):-atom(Name),!,makeParamFallback(Ctx,[Name],Value,ElseVar).
makeParamFallback(_Ctx,_NameS,Value,ElseVar):-'var'(ElseVar),!,Value=ElseVar,!.
makeParamFallback(_Ctx,_NameS,_Value,'$failure'):-!,fail.
makeParamFallback(_Ctx,_NameS,_Value,'$call'(Prolog)):-!,call(Prolog).
makeParamFallback(Ctx,NameS,Value,'$error'):-aiml_error(makeParamFallback(Ctx,NameS,Value,'$error')),throw_safe(fallbackValue(Ctx,NameS,Value,'$error')),!.
makeParamFallback(Ctx,NameS,ValueO, '$current_value'):- member(Name,NameS),current_value(Ctx,Name,ValueO),valuePresent(ValueO),!.
makeParamFallback(_Ctx,_NameS,_Value,'$succeed'):-!.
makeParamFallback(_Ctx,_NameS,ValueO,'$value'(Else)):-!,ValueO=Else,!.
makeParamFallback(Ctx,NameS,ValueO,'$first'(List)):-!,member(E,List),makeParamFallback(Ctx,NameS,ValueO,E),!.
makeParamFallback(_Ctx,_NameS,ValueO,Else):-ValueO=Else,!.
makeParamFallback(_Ctx,_NameS,ValueO,Else):-trace,debugFmt(ignore(ValueO=Else)),!.

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
             %%topic='*',
             precall='true',
             call='true',
             flags='*',
             %that='*',
             % hide for testing 
             %dictionary='default',
             userdict='user',
             substitutions='input',
             graph='default',
             guard='*',
             %request='*',
             lang='bot']).
 
cateMember(Tag):-cateMemberTags(List),member(Tag,List).

defaultCatePredicatesS(Defaults):-cateFallback(Defaults).

cateFallback([
       srcinfo=missinginfo,
       srcfile=missingfile,
       withCategory=[writeqnl,asserta_new],
       pattern='ERROR PATTERN',
       template=[]|MORE]):-findall(N=V,defaultPredicates(N,V),MORE).

pathAttrib(S):-pathAttribS(SS),member(S,SS).
pathAttribS([uri,loc,filename,url,path,dir,file,pathname,src,location]).


% ===================================================================
%  AimlContexts
%   They hold name-values in
%     -- assoc/1 lists
%     -- open tailed lists
%     -- frame/1 contains one or more of the above

%% v/3s 
%  = v(Value,Setter,KeyDestructor)

%% frame/3s
%  = frame(Named,Destructor,Ctx)

/*

 well i played with a couple few differnt environment impls.. they have their pros cons.. one impl..
 that was unique is that an array of "binding pairs" live in an arraylist.. to be "in" an environment it meant that you held an "index" 
 into the arry list that as you went backwards you would find your bindings.. each symbol had a java int field "lastBindingIndex" .. 
 that was a "hint" to where you could fastforward the backwards search .. end named binding context also had a 
 "index" to when you leave a named block.. you could quickly reset the top of an index.

 */

% ===================================================================

withCurrentContext(Goal):-prolog_must(atom(Goal)),debugOnFailureAiml((currentContext(Goal,Ctx),call(Goal,Ctx))).

makeAimlContext(Name,Ctx):-makeContextBase(Name,Ctx),!,setCtxValue(ctx,Ctx,Name),!.

currentContext(Name,X):-makeAimlContext(Name,X),!.

makeContextBase(CtxNameKey, [frame(CtxNameKey,ndestruct,[assoc(AL)|_])|_]):- list_to_assoc([],AL).

makeContextBase__only_ForTesting(Gensym_Key, [frame(Gensym_Key,ndestruct,[assoc(AL)|_])|_]):-    
   list_to_assoc([
    a-v(error_in_assoc,set_assoc,ndestruct(a)),
    a-v(is_a2,set_assoc,ndestruct(a)),
    b-v(is_b,set_assoc,ndestruct(b))],AL).


% ===================================================================
% push/pop frames
% ===================================================================
pushCtxFrame(Name,Ctx,NewValues):-checkCtx(Ctx),get_ctx_holderFreeSpot(Ctx,Holder,GuestDest),!,Holder=frame(Name,GuestDest,NewValues).
popCtxFrame(Name,Ctx,PrevValues):-checkCtx(Ctx),get_ctx_frame_holder(Ctx,Name,Frame),Frame = frame(Name,Destructor,PrevValues),Destructor,!.
checkCtx(Ctx):-prolog_must(nonvar(Ctx)).
%%checkCtx(Ctx):-makeAimlContext(broken,Ctx),!.

:-dynamic(no_cyclic_terms).

no_cyclic_terms.


% ===================================================================
% value getter/setters
% ===================================================================
ndestruct:-trace.
ndestruct(Holder):-debugFmt(unImplemented(ndestruct(Holder))).
no_setter(Why,Name,Ctx,Value):-debugFmt(unImplemented2(no_setter(Why,Name,Ctx,Value))).


nb_setarg(N,Term,Name,OldCtx,Value):-var(Term),throw_safe(nb_setarg(N,Term,Name,OldCtx,Value)),!.
nb_setarg(N,NameT=Term,Name,_OldCtx,Value):-prolog_must(Name=NameT),!,nb_setarg(N,Term,Value).
nb_setarg(N,Term,_Name,_OldCtx,Value):-trace,nb_setarg(N,Term,Value).

% set_assoc as the "setter" means to use the term found in a assoc/1 .. change the calue and resave assoc/1 internal held term
set_assoc(ASSOC,Name,_Ctx,Value):- ASSOC = assoc(Assoc), 
      assoc_to_list(Assoc, List), !,
      append(List,[Name-Value],NewValues),!,
      list_to_assoc(NewValues,NewAssoc),nb_setarg(1,ASSOC,NewAssoc),!.

% set_v3 as the "setter" means to use the v3 data structure to set the value
set_assoc(_OrigName,Name,CtxIn,Value):- prolog_must(setCtxValue(Name,CtxIn,Value)),!.


unwrapValue(HValue,TValue):-TValue==deleted,!,not(unwrapValue1(HValue,_)),!.
unwrapValue(HValue,TValue):-unwrapValue1(HValue,Value),!,TValue=Value.

unwrapValue1(v(ValueHolder,_SetterFun,_KeyDestroyer),Value):-!,unwrapValue1(ValueHolder,Value).
unwrapValue1(deleted,_):-!,fail.
unwrapValue1(Value,Value):-!.

bestSetterFn(v(_,Setter,_),_OuterSetter,Setter):-!.
bestSetterFn(_Value,OuterSetter,OuterSetter).

getCtxValueElse(Name,Ctx,Value,_Else):-getCtxValue0(Name,Ctx,Value),!.
getCtxValueElse(_Name,_Ctx,Else,Else).

getCtxValue(Name,CtxI,Value):-getCtxValue0(Name,CtxI,Value).
getCtxValue(Name,CtxI,Value):-debugFmt(not(getCtxValue(Name,CtxI,Value))),fail.
getCtxValue0(Name,CtxIn,Value):-checkCtx(CtxIn), hotrace(( get_ctx_holder(CtxIn,Ctx),get_o_value(Name,Ctx,HValue,_Setter),!, unwrapValue(HValue,Value))),!.
getCtxValue0(Name,CtxI,Value):-checkCtx(CtxI),lastMember(Ctx,CtxI),hotrace(( get_ctx_holder(Ctx,CtxH),get_o_value(Name,CtxH,HValue,_Setter),!, unwrapValue(HValue,Value))),!.

setCtxValue(Name,CtxIn,Value):-checkCtx(CtxIn),get_ctx_holder(CtxIn,Ctx),get_o_value(Name,Ctx,HValue,Setter),unwrapValue(HValue,CurrentValue),!,(CurrentValue=Value;call(Setter,Name,CtxIn,Value)),!.
setCtxValue(Name,Ctx,Value):-checkCtx(Ctx),addCtxValue1(Name,Ctx,Value),!.

addCtxValue(Name,Ctx,Value):-checkCtx(Ctx),addCtxValue1(Name,Ctx,Value),!.
addCtxValue1(Name,Ctx,Value):-get_ctx_holderFreeSpot(Ctx,Name=v(Value,Setter,Destructor),Destructor),!,ignore(Setter=set_v3(Name)).

remCtxValue(Name,Ctx,_Value):-checkCtx(Ctx),setCtxValue(Name,Ctx,deleted),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% get the frame holder
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_ctx_frame_holder(Ctx,Name,R):-compound(Ctx),get_ctx_frame_holder1(Ctx,Name,R).
get_ctx_frame_holder1(v(_,_,_),_Name,_R):-!,fail.
get_ctx_frame_holder1(frame(Name,Dest,Ctx),Name,R):- R = frame(Name,Dest,Ctx),!.
get_ctx_frame_holder1([H|T],Name,R):- nonvar(H), !, ( get_ctx_frame_holder(T,Name,R);get_ctx_frame_holder1(H,Name,R)) .
%%get_ctx_frame_holder1(Ctx,Name,Ctx):-!,get_ctx_frame_holder1(Ctx,Name,R).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% get the holders areas last in first out %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% get_ctx_holder(+Ctx, -PlaceToSearch),

get_ctx_holder(Ctx,R):-compound(Ctx),get_ctx_holder1(Ctx,R).
get_ctx_holder1([H|T],R):- nonvar(H), !, ( get_ctx_holder(T,R);get_ctx_holder1(H,R)) .
get_ctx_holder1(v(_,_,_),_R):-!,fail.%% get_ctx_holder(Ctx,R).
get_ctx_holder1(frame(_N,_Dest,Ctx),R):-!,get_ctx_holder(Ctx,R).
%get_ctx_holder1(Ctx,R):- functor(Ctx,F,A),A<3,!,fail.
get_ctx_holder1(assoc(Ctx),assoc(Ctx)):-!.
get_ctx_holder1(Ctx,Ctx).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% find a free area to place a: vv(name,val) %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% get_ctx_holderFreeSpot(+Ctx, -Put_NV, -CallToRemoveNV)

get_ctx_holderFreeSpot(Ctx,NamedValue,ndestruct(holder)):-no_cyclic_terms,!,get_ctx_holderFreeSpot0(Ctx,NamedValue,_NO_Destruct),!.
get_ctx_holderFreeSpot(Ctx,NamedValue,Destruct):-get_ctx_holderFreeSpot0(Ctx,NamedValue,Destruct).

get_ctx_holderFreeSpot0(Ctx,NamedValue,Destruct):-compound(Ctx),get_ctx_holderFreeSpot1(Ctx,NamedValue,Destruct).

get_ctx_holderFreeSpot1(assoc(_Ctx),_,_):-!,fail.
get_ctx_holderFreeSpot1(frame(Key,_Inner_Dest,Ctx),NamedValue,Destruct):- nonvar(Key), !, get_ctx_holderFreeSpot1(Ctx,NamedValue,Destruct).
get_ctx_holderFreeSpot1(Ctx,NamedValue,Destruct):-functor(Ctx,F,A),!,get_ctx_holderFreeSpot1(Ctx,F,A,NamedValue,Destruct).

get_ctx_holderFreeSpot1(Ctx,'.',2,NamedValue,nb_setarg(2,NEXT)):-arg(2,Ctx,Try1), var(Try1),!, Try1 = [NamedValue|NEXT].
get_ctx_holderFreeSpot1(Ctx,'.',2,NamedValue,Destruct):-arg(2,Ctx,Try2),get_ctx_holderFreeSpot0(Try2,NamedValue,Destruct).

%%get_ctx_holderFreeSpot1(Ctx,_,_,NamedValue,_):-!,fail.
%%get_ctx_holderFreeSpot1(Ctx,_,_,NamedValue,nb_setarg(N,NEXT)):-arg(N,Ctx,Try3),var(Try3),!, Try3 = [NamedValue|NEXT].
%%get_ctx_holderFreeSpot1(Ctx,_,_,NamedValue,Destruct):-arg(N,Ctx,Try4),get_ctx_holderFreeSpot0(Try4,NamedValue,Destruct).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% find the value holder associated with a keyname
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_ctx_value(Name,Ctx,Value,Setter):-nonvar(Name),var(Value),get_o_value(Name,Ctx,Value,OuterSetter),bestSetterFn(Value,OuterSetter,Setter).

get_o_value00(Name,Ctx,Value,Setter):-get_o_value0(Name,Ctx,Value,HIDE_Setter),!,((no_cyclic_terms,cyclic_term(HIDE_Setter))-> Setter=no_setter(cyclicOn(Name)) ; Setter =HIDE_Setter).
get_o_value(Name,Ctx,Value,Setter):-hotrace(get_o_value00(Name,Ctx,Value,Setter)),!.

get_o_value0(Name,Ctx,Value,Setter):-compound(Ctx),get_o_value1(Name,Ctx,Value,Setter).
get_o_value1(Name,[H|T],Value,Setter):- !,(get_o_value0(Name,T,Value,Setter);get_o_value1(Name,H,Value,Setter)).
get_o_value1(Name,ASSOC,Value,set_assoc(ASSOC)):- ASSOC = assoc(Ctx),!, get_assoc(Name,Ctx,Value),!.
get_o_value1(Name,frame(Key,_Inner_Dest,Ctx),Value,Setter):- nonvar(Key), get_o_value0(Name,Ctx,Value,Setter),!.
get_o_value1(Name,Pred,Value,Setter):-functor(Pred,F,A),!,get_n_value(Name,Pred,F,A,Value,Setter).

get_n_value(Name,Name,_F,_A,_Value,_):-!,fail.
get_n_value(Name,Pred,Name,1,Value,nb_setarg(1,Pred)):-arg(1,Pred,Value).
get_n_value(Name,Pred,Name,_,Value,Setter):- arg(1,Pred,Value),!,arg(2,Pred,Setter). %% value can actually be 'Pred'
get_n_value(Name,Pred,Dash,2,Value,nb_setarg(2,Pred)):-arg(1,Pred,Name),member(Dash,[=,-,vv]),!, arg(2,Pred,Value).
%%get_n_value(Name,Pred,'.',2,Value,Setter):-arg(2,Pred,Try1), get_o_value0(Name,Try1,Value,Setter);(arg(1,Pred,Try2),get_o_value0(Name,Try2,Value,Setter)).
%%get_n_value(Name,Pred,_,_,Value,Setter):- !, arg(_,Pred,Try2),get_o_value0(Name,Try2,Value,Setter).

% ===================================================================
% attribute searching (Document contexts)
% ===================================================================

attributeOrTagValue(Ctx,ATTRIBS,NameS,ValueO,_Else):- hotrace((attributeValue(Ctx,ATTRIBS,NameS,ValueO,'$failure'))),!.
attributeOrTagValue(Ctx,XML,NameS,ValueO,_Else):- hotrace((findTagValue(Ctx,XML,NameS,ValueO,'$failure'))),!.
attributeOrTagValue(Ctx,ATTRIBS,NameS,ValueO,_Else):-compound(ATTRIBS),ATTRIBS=..[_|LIST],member(E,LIST),
   attributeOrTagValue(Ctx,E,NameS,ValueO,'$failure'),!.
attributeOrTagValue(Ctx,_,NameS,ValueO,ElseVar):-ElseVar\=='$failure',makeParamFallback(Ctx,NameS,ValueO,ElseVar),!.

attributeValue(Ctx,ATTRIBS,NameS,ValueO,Else):- hotrace((attributeValue0(Ctx,ATTRIBS,NameS,ValueI,Else), aiml_eval_to_unit(Ctx,ValueI,ValueO))),!.
attributeValue(Ctx,ATTRIBS,NameS,ValueO,Else):-   Else\=='$failure',debugOnFailure((attributeValue0(Ctx,ATTRIBS,NameS,ValueI,Else), aiml_eval_to_unit(Ctx,ValueI,ValueO))),!.

attributeValue0(_Ctx,ATTRIBS,NameS,ValueO,_Else):- member(Name,NameS), lastMember(NameE=ValueO,ATTRIBS), atomsSameCI(Name,NameE),!.
attributeValue0(Ctx,_ATTRIBS,NameS,Value,ElseVar):- makeParamFallback(Ctx,NameS,Value,ElseVar),!.
/*
attributeValue0(Ctx,_ATTRIBS,NameS,ValueO,'$current_value'):-member(Name,NameS), current_value(Ctx,Name,ValueO),valuePresent(ValueO),!.
attributeValue0(_Ctx,_ATTRIBS,_NameS,_Value,Failure):-'$failure'==Failure,!,fail.
attributeValue0(Ctx,ATTRIBS,NameS,Value,Error):- '$error'==Error,  
   aiml_error(attributeValue(Ctx,ATTRIBS,NameS,Value,'$error')).
attributeValue0(_Ctx,_ATTRIBS,_Name,ValueO,Else):-ValueO=Else,!.
*/

findTagValue(_Ctx,XML,_NameS,_ValueO,_Else):-var(XML),!,fail.

findTagValue(Ctx,XML,NameS,ValueO,Else):-
      member(Name,NameS),
      findTagValue(Ctx,XML,Name,ValueO,Else),!.

findTagValue(Ctx,XML,NameS,ValueO,Else):-
      member(element(NameE,ATTRIBS,ValueI),XML),
      findTagValue(Ctx,element(NameE,ATTRIBS,ValueI),NameS,ValueO,Else),!.

findTagValue(Ctx,XML,Name,ValueO,Else):-!,
      findTagValue0_of_xml_element(Ctx,XML,Name,ValueO,Else).

findTagValue(Ctx,XML,[NameE=_|_],ValueO,_Else):-
      member(element(NameA,ATTRIBS,ValueI),XML),member(_=NameI,ATTRIBS),
     atomsSameCI(NameA,NameE),atomsSameCI(NameA,NameI),
     aiml_select_unit(Ctx,NameA,ValueI,ValueO),!.


findTagValue0_of_xml_element(Ctx,element(NameE,ATTRIBS,ValueI),Name,ValueO,_Else):- 
      atomsSameCI(Name,NameE),!,
   aiml_select_unit(Ctx,NameE,element(NameE,ATTRIBS,ValueI),ValueO),!.

findTagValue0_of_xml_element(Ctx,element(NameE,ATTRIBS,ValueI),Name,ValueO,_Else):-
      lastMember(name=Name,ATTRIBS,Rest),atomsSameCI(Name,NameE),!,
   aiml_select_unit(Ctx,Name,element(NameE,Rest,ValueI),ValueO),!.


aiml_select_unit(Ctx,NameA,element(NameA,[],ValueI),ValueO):-aiml_eval_to_unit(Ctx,ValueI,ValueO),!.
aiml_select_unit(Ctx,_NameA,ValueI,ValueO):-aiml_eval_to_unit(Ctx,ValueI,ValueO),!.

%[''name'='SomeName','Description'='some descr','Input'='$error','ExpectedAnswer'='SomeAnswwer'']
/*
getAttributeOrTags(Ctx,[N=Default|More],ATTRIBS,INNERXML,Var):- var(Var),!,
  hotrace((getAttributeOrTags1(Ctx,[N=Default|More],ATTRIBS,INNERXML,[_=Var|_NormalProps]))),!.

getAttributeOrTags(Ctx,[N=Default|More],ATTRIBS,INNERXML,[N0=Var|NormalProps]):-
  hotrace((getAttributeOrTags1(Ctx,[N=Default|More],ATTRIBS,INNERXML,[N0=Var|NormalProps]))),!.

:-trace(getAttributeOrTags/5, -fail).

getAttributeOrTags1(_Ctx,[],_ATTRIBS,_INNERXML,[]):-!.

getAttributeOrTags1(Ctx,[N=_Default|More],ATTRIBS,INNERXML,[N0=ValueO|NormalProps]):- 
      member(element(NE,_Atribs,           Value),INNERXML), atomsSameCI(N,NE), aiml_eval_to_unit(Ctx,Value,ValueO),ignore(N=N0),
      getAttributeOrTags1(Ctx,More,ATTRIBS,INNERXML,NormalProps),!.

getAttributeOrTags1(Ctx,[N=_Default|More],ATTRIBS,INNERXML,[N0=ValueO|NormalProps]):- 
      member(element(_NE,[name=NE|_Atribs],Value),INNERXML), atomsSameCI(N,NE), aiml_eval_to_unit(Ctx,Value,ValueO),ignore(N=N0),
      getAttributeOrTags1(Ctx,More,ATTRIBS,INNERXML,NormalProps),!.

getAttributeOrTags1(Ctx,[N=Default|More],ATTRIBS,INNERXML,[N=Found|NormalProps]):- 
      attributeOrTagValue(Ctx,ATTRIBS:INNERXML,[N],Found,Default),
      getAttributeOrTags1(Ctx,More,ATTRIBS,INNERXML,NormalProps),!.

getAttributeOrTags1(Ctx,[N=Default|More],ATTRIBS,INNERXML,[N=Default|NormalProps]):- 
      getAttributeOrTags1(Ctx,More,ATTRIBS,INNERXML,NormalProps),!.
*/


