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
% get / set  Coversational Variables
% ===============================================================================================
getAliceMemOrSetDefault(CtxIn,ConvThread,SYM,Name,Value,_OrDefault):-checkSym(SYM),
   hotrace(getCtxValue(ConvThread:Name,CtxIn,Value)),!.
getAliceMemOrSetDefault(CtxIn,ConvThread,SYM,Name,Value,_OrDefault):-checkSym(SYM),
   hotrace(getIndexedValue(CtxIn,ConvThread,Name,[],Value)),!.
getAliceMemOrSetDefault(CtxIn,ConvThread,SYM,Name,Value,OrDefault):-checkSym(SYM),
   setAliceMem(CtxIn,ConvThread,Name,OrDefault),!,OrDefault=Value.

% ===============================================================================================
% get / set  Global Variables
% ===============================================================================================
:-dynamic(dict/3).
:-multifile(dict/3).

getAliceMemElse(Ctx,Dict,Name,ValueO):-getAliceMemComplete(Ctx,Dict,Name,ValueO),!.
getAliceMemElse(_Ctx,Dict,Name,[Dict,(s),unknown,Name]):-trace.

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

dictNameKey(_Dict,DictName,Key):- nonvar(DictName),DictName=Dict:Name,!,dictNameKey(Dict,Name,Key).
dictNameKey(Dict,Name,Dict:Name):-!.%%nonvar(Dict),!.
dictNameKey(_Dict,NameKey,NameKey).

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

% ===============================================================================================
% getIndexedValue
% ===============================================================================================

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
   
getIndexedValue(Ctx,Dict,Name,MajorMinor,ValueO):- fail,   
    unify_listing(getContextStoredValue(Ctx,Dict,_N,_V)),
    %%unify_listing(getContextStoredValue(Ctx,_,Name,_)),
    getIndexedValue0(Ctx,Dict,Name,MajorMinor,Value),
    xformOutput(Value,ValueO).

% ===============================================================================================
% getIndexedValue0
% ===============================================================================================

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
addNewContextValue(Ctx,Dict,Name,OM):- OM=='OM',!,clearContextValues(Ctx,Dict,Name),!.
addNewContextValue(Ctx,Dict,Name,Value):- 
   debugOnFailure((dictNameKey(Dict,Name,Key), addNewContextValue(Ctx,Dict,Key,Name,Value))),!.

addNewContextValue(Ctx,Dict,Key,Name,Value):- 
   ifThen(nonvar(Key),addCtxValue(Key,Ctx,Value)),   
   ifThen(nonvar(Dict),ifThen(nonvar(Value),asserta(dict(Dict,Name,Value)))),
   ifThen(not(ground(Value)),debugFmt(addCtxValue(Key,Ctx,Value))).


