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

:-ensure_loaded(library('programk/logicmoo_module_aiml_memory.pl')).

% ===============================================================================================
%    Context values API
% ===============================================================================================
pushAttributes(Ctx,Scope,List):-prolog_mustEach((prolog_mostly_ground(List),pushCtxFrame(Ctx,Scope,List),pushAttributes1(Ctx,Scope,List))),!.
pushAttributes1(Ctx,Scope,[N=V|L]):-pushNameValue(Ctx,Scope,N,V),pushAttributes1(Ctx,Scope,L).
pushAttributes1(_Ctx,_Scope,[]).
pushAttributes1(_Ctx,_Scope,_AnyPushed):-!.

peekAttributes(Ctx,SList,Scope,Results):-prolog_must(peekAttributes0(Ctx,SList,Scope,Results)).
peekAttributes0(Ctx,[Name|SList],Scope,[Name=Value|Results]):- peekNameValue(Ctx,Scope,Name,Value,'$error'),peekAttributes0(Ctx,SList,Scope,Results),!.
peekAttributes0(_Ctx,[],_Scope,[]):-!.

%%%%TODO: use?%%%%%%% current_value(Ctx,Scope:Name,Value):-!,attributeValue(Ctx,Scope,Name,Value,'$error').
%%%%TODO: use?%%%%%%% current_value(Ctx,Name,Value):-attributeValue(Ctx,_,Name,Value,'$error').
current_value(Ctx,Name,Value):-current_value(Ctx,Name,Value,'$global_value').
current_value(Ctx,Name,Value,Else):-locateNameValue(Ctx,_,Name,Value,'$first'(['$local_value','$global_value','$attribute_value',Else])).

checkNameValue(Pred,Ctx,Scope,[Name],Value,Else):- nonvar(Name),!,checkNameValue(Pred,Ctx,Scope,Name,Value,Else).
checkNameValue(Pred,Ctx,Scope,Name,Value,Else):-hotrace(( call(Pred,Ctx,Scope,Name,ValueVar,Else),!,checkValue(ValueVar),valuesMatch(Ctx,ValueVar,Value))),!. %%,ctrace.

peekNameValue(Ctx,Scope,Name,Value,Else):-nonvar(Value),!,checkNameValue(peekNameValue,Ctx,Scope,Name,Value,Else).
peekNameValue(Ctx,Scope,Name,Value,ElseVar):- locateNameValue(Ctx,Scope,Name,Value,'$first'(['$local_value','$global_value','$attribute_value',ElseVar])),!.

/*
peekNameValue0(Ctx,Scope,Name,Value):-contextScopeTerm(Ctx,Scope,Term),arg_value(Term,Name,Value),!.
%%%peekNameValue0(Ctx,List,Name,Value):- nonvar(List),not(atom(List)),attributeValue(Ctx,List,Name,Value,'$failure'),!.
%%%%peekNameValue0(CtxI,_Scope,Name,Value):-getCtxValueND(CtxI,Name,Value).
peekNameValue0(Ctx,Scope,Name,Value):-is_list(Name),member(N0,Name),peekNameValue(Ctx,Scope,N0,Value,'$failure'),!.
peekNameValue0(CtxI,Scope,Name,Value):-dictNameKey(Scope,Name,Key),getCtxValueND(CtxI,Key,Value).
peekNameValue0(Ctx,Scope,Name,Value):-peekGlobalMem(Ctx,Scope,Name,Value).
peekNameValue0(CtxI,Scope,Name,Value):-var(Scope),!,peekAnyNameValue(CtxI,Scope,Name,Value).
peekNameValue0(CtxI,Scope,Name,Value):-getCtxValueND(CtxI,Scope:Name,Value).
peekNameValue0(Ctx,ATTRIBS,NameS,ValueO):- hotrace((findAttributeValue(Ctx,ATTRIBS,NameS,ValueO,'$failure'))).
peekNameValue0(Ctx,XML,NameS,ValueO):- hotrace((findTagValue(Ctx,XML,NameS,ValueO,'$failure'))).
peekNameValue0(_Ctx,CtxI,Name,Value):- getCtxValueND(CtxI,Name,Value).
peekNameValue0(Ctx,Scope,Name,Value):-lotrace((getIndexedValue(Ctx,Scope,Name,[],Value),checkAttribute(Scope,Name,Value))).
peekNameValue0(Ctx,Scope,Name,Value):-getInheritedStoredValue(Ctx,Scope,Name,Value),checkAttribute(Scope,Name,Value),ctrace.
peekNameValue0(Ctx,ATTRIBS,NameS,ValueO):- compound(ATTRIBS),compound_or_list(ATTRIBS,LIST),member(E,LIST),prolog_must(nonvar(E)),attributeValue(Ctx,E,NameS,ValueO,'$failure').

%%%peekAnyNameValue(Ctx,_Scope,Name,Value):-arg_value(Ctx,Name,Value),!.
%%peekAnyNameValue(CtxI,Scope,Name,Value):-prolog_extra_checks, member(Name,[withCategory]),!,dictNameKey(Scope,Name,Key),trace,prolog_must(getCtxValueND(CtxI,Key,Value)),dictNameKey(Scope,Name,Key).
%%peekAnyNameValue(Ctx,Scope,Name,Value):-atom(Name),getCtxValueND(Ctx,Scope,Name,Value).
peekNameValue0(CtxI,Scope,Name,Value):-nop(debugFmt(not(peekNameValue0(CtxI,Scope,Name,Value)))),!,fail.
*/

local_value(Ctx,Scope,Name,Value):-contextScopeTerm(Ctx,Scope,Term),arg_value(Term,Name,Value).
local_value(CtxI,Scope,Name,Value):-dictNameKey(Scope,Name,Key),getCtxValueND(CtxI,Key,Value).
local_value(Ctx,Scope,Name,Value):-is_list(Name),!,member(N0,Name),local_value(Ctx,Scope,N0,Value).
local_value(Ctx,List,Name,Value):- nonvar(List),not(atom(List)),attributeValue(Ctx,List,Name,Value,'$failure').
%%local_value(Ctx,Scope,Name,Value):-attributeValue(Ctx,Scope,Name,ValueO,'$failure')



arg_value(Ctx,Name,Value):-atomic(Name),!,Name==lastArg,!,arg_value_lastArg(Ctx,Value).
arg_value(Ctx,arg(N),Value):-integer(N),arg_value_ArgN(Ctx,N,Value).
arg_value_lastArg(Ctx,Value):-compound(Ctx),functor(Ctx,_F,A),arg(A,Ctx,Value),!.
arg_value_ArgN(Ctx,-1,Value):- !,current_value(Ctx,lastArg,Value),!.
arg_value_ArgN(Ctx,N,Value):-prolog_must(compound(Ctx)),arg(N,Ctx,Value),!.

contextScopeTerm(Ctx,Scope,Term):- (Scope=Term;Ctx=Term),nonvar(Term).

peekGlobalMem(Ctx,[],Name,Value):- !,peekGlobalMem(Ctx,user,Name,Value).
peekGlobalMem(_Ctx,[_=_|_],_Name,_Value):-!,fail.
peekGlobalMem(Ctx,Scope,Name,Value):- getInheritedStoredValue(Ctx,Scope,Name,Value),checkAttribute(Scope,Name,Value).
peekGlobalMem(Ctx,Scope,Name,Value):-ignore(Scope=user),getIndexedValue(Ctx,Scope,Name,[],Value),checkAttribute(Scope,Name,Value),!.%%trace.

compound_or_list([ATT|RIBS],[ATT|RIBS]):-!.
compound_or_list(ATTRIBS,LIST):-ATTRIBS=..[_|LIST].


% ===============================================================================================
%    Value verification
% ===============================================================================================

illegalValue(ValueVar):- ValueVar=['YourBot'],!.

valuesMatch(_Ctx,V,V):-!.
valuesMatch(Ctx,V,A):-convertToMatchableCS(A,AA),convertToMatchableCS(V,VV),valuesMatch10(Ctx,VV,AA).


valuesMatch10(Ctx,V,A):-hotrace(valuesMatch11(Ctx,V,A)),!.
valuesMatch10(Ctx,V,A):-ignorecase_literal(A,AA),ignorecase_literal(V,VV),!,valuesMatch11(Ctx,VV,AA),!.

valuesMatch1(_Ctx,V,V).
valuesMatch1(_Ctx,V,A):- isStar0(V);isStar0(A).
valuesMatch1(Ctx,[V],A):-!,valuesMatch1(Ctx,V,A).
valuesMatch1(Ctx,V,[A]):-!,valuesMatch1(Ctx,V,A).
valuesMatch1(Ctx,V,A):-number(V),atom_number(VA,V),!,valuesMatch1(Ctx,A,VA).
valuesMatch1(_Ctx,V,A):-sameBinding(V,A).

valuesMatch11(_Ctx,A,A).
valuesMatch11(Ctx,[V|VV],[A|AA]):-valuesMatch1(Ctx,V,A),!,valuesMatch11(Ctx,VV,AA).


valueMP(Var,M):- member(M, [var(Var), Var=missing, Var=[], Var=(*) ,  Var=('_') , Var=('OM') , (Var=(-(_))) ]),M,!.
%valueMP(Var,'$deleted'):-functor(Var,·'$deleted',_),!.
valueMP(V,(V='ERROR')):-prolog_must(ground(V)),term_to_atom(V,A), concat_atom([_,_|_],'ERROR',A),trace,!.


checkValue(Value):- valueMP(Value,M),throw_safe(M),!.
checkValue(_):-!.

valuePresent(Value):- var(Value),!,fail.
valuePresent(result(_)):- !.
valuePresent('$first'(_)):- !,ctrace,fail.
valuePresent(Value):- valueMP(Value,_M),!,fail.
valuePresent(_):-!.

isValid(Value):- var(Value),!,fail.
isValid([X]):-!,isValid(X).
isValid(result(_)):- !.
isValid([]):-!.
isValid(Value):- valueMP(Value,_M),!,fail.
isValid(_):-!.

% ===============================================================================================
%    push/pop values API group operations
% ===============================================================================================
%%WIERD popAttributes(Ctx,Scope,OldVals):- popCtxFrame(Ctx,Scope,PrevValues),ignore(PrevValues=OldVals),!,ignore(popAttributes0(Ctx,Scope,OldVals)),!.
popAttributes(Ctx,Scope,OldVals):-prolog_must(atom(Scope)),ignore(popAttributes0(Ctx,Scope,OldVals)),popCtxFrame(Ctx,Scope,PrevValues),ignore(PrevValues=OldVals),!.
%%popAttributes(Ctx,Scope,List):- prolog_must(ground(Scope)),popAttributes0(Ctx,Scope,OldVals),!.


popNameValueOnce(Ctx,Scope,N,V):-nonvar(N),!,checkAttribute(Scope,N,V),popNameValue(Ctx,Scope,N,V),!.

popAttributes0(Ctx,Scope,[N=V|L]):- prolog_must(popNameValueOnce(Ctx,Scope,N,V)),popAttributes0(Ctx,Scope,L),!.
popAttributes0(_Ctx,_Scope,VA):-var(VA),!.
popAttributes0(_Ctx,_Scope,[]):-!.
popAttributes0(Ctx,Scope,What):-debugFmt(popAttributes0(Ctx,Scope,What)),unify_listing(retract(dict(Scope,_,_))),!. %%,ctrace.

withAttributes(_Ctx,ATTRIBS,Call):-ATTRIBS==[],!,Call.

withAttributes(CtxIn,ATTRIBS,Call):- fail,
 %%gensym(withAttribs,SYM),
  ensureScope(NewCtx,ATTRIBS,Scope),
  makeContextBase(Scope,NewCtx),
  subst(Call,CtxIn,Ctx,ReCall),!,
  Ctx = l2r(NewCtx,CtxIn),
  hotrace((
   ensureScope(NewCtx,ATTRIBS,Scope),
   checkAttributes(Scope,ATTRIBS))),
   call_cleanup((
    once(hotrace(pushAttributes(NewCtx,Scope,ATTRIBS))),
    prolog_must(ReCall)),
    once((hotrace(popAttributes(NewCtx,Scope,ATTRIBS))))).

withAttributes(CtxIn,ATTRIBS,Call):-
 duplicate_term(CtxIn,Ctx),!,
  hotrace((
   ensureScope(Ctx,ATTRIBS,Scope),
   checkAttributes(Scope,ATTRIBS),
   pushAttributes(Ctx,Scope,ATTRIBS))),!,
    subst(Call,CtxIn,Ctx,ReCall),!,
  call(ReCall).

withAttributes(Ctx,ATTRIBS,Call):-
  hotrace((
   ensureScope(Ctx,ATTRIBS,Scope),
   checkAttributes(Scope,ATTRIBS))),
   call_cleanup((
    once(hotrace(pushAttributes(Ctx,Scope,ATTRIBS))),
      Call),
    once(hotrace(popAttributes(Ctx,Scope,ATTRIBS)))).
    
addScopeParent(Child,Parent):-addInherit(Child,Parent).

checkAttributes(Scope,ATTRIBS):-prolog_must(nonvar(ATTRIBS)),maplist(checkAttribute(Scope),ATTRIBS).
checkAttribute(Scope,N=V):-checkAttribute(Scope,N,V).
checkAttribute(Scope,N,_V):-N==proof,!,prolog_must(nonvar(Scope)).
checkAttribute(Scope,N,V):-prolog_must(nonvar(Scope)),prolog_must(nonvar(N)),!,prolog_must(nonvar(V)).

pushNameValue(Ctx,Scope,N,V):-
   checkAttribute(Scope,N,V),
   insert1StValue(Ctx,Scope,N,V),!.
   %%asserta(dict(Scope,N,V)),!.

popNameValue(Ctx,Scope,N,Expect):-
   prolog_mustEach((
   currentContextValue(Ctx,Scope,N,V),
   ifThen(Expect\==V,debugFmt(popNameValue(Ctx,Scope,N,Expect,V))),
   checkAttribute(Scope,N,V),
   removeContextValue(Ctx,Scope,N,V),
   checkAttribute(Scope,N,V))),!.

%dyn_retract(dict(Scope,N,V)):-(retract(dict(Scope,N,V))),!.

ensureScope(Ctx,Attribs,ScopeName):-prolog_must(ensureScope0(Ctx,Attribs,ScopeName)),!.
ensureScope0(_Ctx,_ATTRIBS,Scope):-nonvar(Scope),!.
ensureScope0(_Ctx,_ATTRIBS,Scope):-gensym(scope,Scope),!.


% ===================================================================
%  Tagged/Attribute Contexts
% ===================================================================
replaceAttribute(Ctx,Before,After,ALIST,ATTRIBS):- replaceAttribute0(Ctx,Before,After,ALIST,AA),list_to_set_safe(AA,ATTRIBS),!.%%,traceIf((ALIST\==ATTRIBS,ctrace)).
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



makeAllParams(Ctx,[O|Order],Assert,[Tag=RR|Result]):-
   UnboundDefault = '$first'(['$attribute_value','$error'(makeAllParams(O))]),
   makeSingleTag(Ctx,O,Assert,UnboundDefault,Tag,RR),prolog_must(O\==RR),
   makeAllParams(Ctx,Order,Assert,Result),!.
makeAllParams(_Ctx,[],_,[]).


makeSingleTag(Ctx,Name,ATTRIBS,Default,Tag,Result):-atom(Name),!,makeSingleTag(Ctx,[Name],ATTRIBS,Default,Tag,Result),!.
makeSingleTag(Ctx,NameS,ATTRIBS,Default,Tag,ValueO):-makeAimlSingleParam0(Ctx,NameS,ATTRIBS,Default,Tag,ValueI),
      transformTagData(Ctx,Tag,Default,ValueI,ValueO),!.

makeAimlSingleParam0(_Ctx,[N|NameS],ATTRIBS,_D,N,Value):-member(O,[N|NameS]),lastMember(OI=Value,ATTRIBS),atomsSameCI(O,OI),!,prolog_must(N\==Value).
makeAimlSingleParam0(Ctx,[N|NameS],ATTRIBS,ElseVar,N,Value):- hotrace((locateNameValue(Ctx,ATTRIBS,[N|NameS],Value,
          '$first'(['$local_value','$call_name'(prolog_must(cateFallback(N,Value)),N),
                    '$global_value','$call_name'(prolog_must(defaultPredicates(N,Value)),N),
                       ElseVar,'$error'])))),!,
           prolog_must(N\==Value).


% ===============================================================================================
%  Fallback
% ===============================================================================================

valuePresentOrStar(Var):-var(Var),!,throw_safe(valuePresentOrStar(Var)).
valuePresentOrStar(*):-!.
valuePresentOrStar([]):-!.
valuePresentOrStar(Var):-valuePresent(Var),!.

%%% arity 5 version
locateNameValue(Ctx,Scope,NameS,ValueO,ElseVar):-makeParamFallback(Ctx,Scope,NameS,ValueO,ElseVar),prolog_must(valuePresentOrStar(ValueO)).

makeParamFallback(Ctx,Scope,NameS,Value,ElseVar):-var(ElseVar),!,throw_safe(makeParamFallback(Ctx,Scope,NameS,Value,ElseVar)).
makeParamFallback(_Ctx,_Scope,_NameS,_Value,'$aiml_error'(E)):-!,aiml_error(E),throw_safe(E).
makeParamFallback(_Ctx,_Scope,_NameS,_Value,'$error'(E)):-!,aiml_error(E),throw_safe(E).
makeParamFallback(Ctx,Scope,NameS,Value,    '$error'):-E =!,fallbackValue(Ctx,Scope,NameS,Value,'$error'),aiml_error(E),throw_safe(E).
makeParamFallback(_Ctx,_Scope,_NameS,_Value,'$failure'):-!,fail.
makeParamFallback(_Ctx,_Scope,_NameS,_Value,'$succeed'):-!.

makeParamFallback(Ctx,Scope,NameS,ValueO,   '$first'(List)):-!,anyOrEachOf(E,List),locateNameValue(Ctx,Scope,NameS,ValueO,E),!.
makeParamFallback(Ctx,Scope,NameS,ValueO,   '$current_value'):-!, locateNameValue(Ctx,Scope,NameS,ValueO,'$first'(['$local_value','$global_value','$attribute_value','$failure'])).

makeParamFallback(Ctx,Scope,NameS,ValueO,   '$local_value'):-!,local_value(Ctx,Scope,NameS,ValueO).
makeParamFallback(Ctx,Scope,NameS,ValueO,   '$global_value'):-!, peekGlobalMem(Ctx,Scope,NameS,ValueO),valuePresent(ValueO).
makeParamFallback(Ctx,Scope,NameS,ValueO,   '$attribute_value'):-!, attributeValue(Ctx,Scope,NameS,ValueO,'$failure').

makeParamFallback(_Ctx,_Scope,_NameS,_Value,'$call'(Prolog)):-!,call(Prolog).
makeParamFallback(_Ctx,_Scope,NameS,_Value, '$call_name'(Prolog,NameS)):-!,prolog_must(Prolog).
makeParamFallback(Ctx,Scope,NameS,ValueO,   '$call_value'(Pred)):-!, call(Pred,Ctx,Scope,NameS,ValueO,'$failure').

makeParamFallback(_Ctx,_Scope,_NameS,ValueO,'$value'(Else)):-!,ValueO=Else,!.

makeParamFallback(Ctx,Scope,NameS,Value,ElseVar):-not(atom(NameS)),!,anyOrEachOf(Name,NameS),makeParamFallback(Ctx,Scope,Name,Value,ElseVar).
makeParamFallback(_Ctx,_Scope,_NameS,ValueO,Else):-ValueO=Else,!.
makeParamFallback(_Ctx,_Scope,_NameS,ValueO,Else):-ctrace,debugFmt(ignore(ValueO=Else)),!.

anyOrEachOf(Name,NameL):-prolog_must(nonvar(NameL)),is_list(NameL),!,member(Name,NameL).
anyOrEachOf(Name,NameA):-atom(NameA),!,prolog_must(NameA=Name).
anyOrEachOf(Name,NameA):-ctrace,!,prolog_must(NameA=Name).

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

withCurrentContext(Goal):-prolog_must(atom(Goal)),prolog_must((currentContext(Goal,Ctx),call(Goal,Ctx))).

withNamedContext(CtxNameKey,NewCtx):-makeContextBase(CtxNameKey,NewCtx),!,setCtxValue(NewCtx,ctxname,CtxNameKey),!.

currentContext(CtxNameKey,CurrentCtx):-ifThen(var(CurrentCtx),withNamedContext(CtxNameKey,CurrentCtx)),!.

makeContextBase(CtxNameKey, [frame(CtxNameKey,ndestruct,[assoc(AL)|_])|_]):- list_to_assoc([],AL).
%% WAS makeContextBase(CtxNameKey, [frame(CtxNameKey,ndestruct,[assoc(AL)|_])|_]):- list_to_assoc([],AL).

makeContextBase__only_ForTesting(Gensym_Key, [frame(Gensym_Key,ndestruct,[assoc(AL)|_])|_]):-    
   list_to_assoc([
    a-v(error_in_assoc,set_assoc,ndestruct(a)),
    a-v(is_a2,set_assoc,ndestruct(a)),
    b-v(is_b,set_assoc,ndestruct(b))],AL).


% ===================================================================
% push/pop frames
% ===================================================================
pushCtxFrame(Ctx,Name,NewValues):-prolog_mustEach((checkCtx(pushCtxFrame,Ctx),get_ctx_holderFreeSpot(Ctx,Holder,GuestDest),!,Holder=frame(Name,GuestDest,NewValues))).

popCtxFrame(Ctx,Name,PrevValuesIn):- hotrace(prolog_mustEach(((
      checkCtx(popCtxFrame,Ctx),
      get_ctx_frame_holder(Ctx,Name,Frame,_Held),
      %%prolog_must(atom(Name)),prolog_must(compound(Frame)),
      Frame = frame(Name,Destructor,PrevValues),
      call(Destructor,Name,Ctx,Frame),!,
      mustMatch(PrevValues,PrevValuesIn))))).

%%checkCtx(Fallback,Ctx):-var(Ctx),!,Ctx=[broken(Fallback)|_]. 
checkCtx(Fallback,Ctx):-var(Ctx),!,brokenFallback(Fallback,Ctx).
checkCtx(_Fallback,_):-!.
checkCtx(_Fallback,Ctx):-prolog_must(nonvar(Ctx)).
checkCtx(Fallback,Ctx):-brokenFallback(Fallback,Ctx).

brokenFallback(Fallback,Ctx):-withNamedContext(broken(Fallback),Ctx),!.

mustMatch(PrevValues,PrevValuesIn):-ignore(PrevValues=PrevValuesIn).

:-dynamic(no_cyclic_terms).

no_cyclic_terms.

% ===================================================================
% value getter/setters
% ===================================================================
ndestruct:-ctrace.
ndestruct(Holder):-debugFmt(unImplemented(ndestruct(Holder))).

mdestruct(_Why,Name,_Ctx,Frame):-prolog_must(atom(Name)),prolog_must(compound(Frame)),Frame=frame(NameF,_,_),prolog_must(Name==NameF),
   Dest = '$deleted'(scope(Name)),prolog_must(atom(Name)),
   %%seeing is we can keep it arround setarg(1,Frame,Dest),
   setarg(2,Frame,Dest),setarg(3,Frame,Dest),!. %%,Frame=Frame.
mdestruct(Why,Name,Ctx,Value):-debugFmt(unImplemented11(mdestruct(Why,Name,Ctx,Value))).
no_setter(Why,Name,Ctx,Value):-debugFmt(unImplemented2(no_setter(Why,Name,Ctx,Value))).


nb_setarg(N,Term,Name,OldCtx,Value):-var(Term),throw_safe(nb_setarg(N,Term,Name,OldCtx,Value)),!.
nb_setarg(N,NameT=Term,Name,_OldCtx,Value):-prolog_must(Name=NameT),!,nb_setarg(N,Term,Value).
nb_setarg(N,Term,_Name,_OldCtx,Value):-ctrace,nb_setarg(N,Term,Value).

% set_assoc as the "setter" means to use the term found in a assoc/1 .. change the calue and resave assoc/1 internal held term
set_assoc(ASSOC,Name,_Ctx,Value):- ASSOC = assoc(Assoc), 
      assoc_to_list(Assoc, List), !,
      append(List,[Name-Value],NewValues),!,
      list_to_assoc(NewValues,NewAssoc),nb_setarg(1,ASSOC,NewAssoc),!.

% set_v3 as the "setter" means to use the v3 data structure to set the value
set_assoc(_OrigName,Name,CtxIn,Value):- prolog_must(setCtxValue(CtxIn,Name,Value)),!.


unwrapValue(HValue,TValue):-TValue=='$deleted',!,not(unwrapValue1(HValue,_)),!.
unwrapValue(HValue,TValue):-unwrapValue1(HValue,Value),!,TValue=Value.

unwrapValue1(Value,ValueOut):-var(Value),!,ctrace,throw_safe(unwrapValue1(Value,ValueOut)),Value=ValueOut.
unwrapValue1(v(ValueHolder,_SetterFun,_KeyDestroyer),Value):-!,unwrapValue1(ValueHolder,Value),!.
unwrapValue1('$deleted',_):-!,fail.
%unwrapValue1([Value],[ValueOut]):-!,unwrapValue1(Value,ValueOut),!.
%unwrapValue1([V|Value],[VO|ValueOut]):-!,unwrapValue1(V,VO),unwrapValue1(Value,ValueOut),!.
unwrapValue1(Value,Value):-not(compound(Value)),!.
unwrapValue1(Deleted,_):-functor(Deleted,'$deleted',_),!,fail.
%%unwrapValue1(Compound,ArgO):-compound(Compound),arg(_,Compound,Arg),unwrapValue1(Arg,ArgO),!.
unwrapValue1(Compound,Compound).


bestSetterFn(v(_,Setter,_),_OuterSetter,Setter):-!.
bestSetterFn(_Value,OuterSetter,OuterSetter).

getCtxValueND(CtxIn,Name,Value):-checkCtx(getCtxValueND,CtxIn), hotrace(( get_ctx_holder(CtxIn,Ctx),get_o_value(Name,Ctx,HValue,_Setter),unwrapValue(HValue,Value))).
getCtxValueND(CtxI,Name,Value):-checkCtx(getCtxValueND,CtxI),lastMember(Ctx,CtxI),hotrace(( get_ctx_holder(Ctx,CtxH),get_o_value(Name,CtxH,HValue,_Setter), unwrapValue(HValue,Value))),ctrace.

/*
getCtxValueND(CtxIn,Dict:Name,Value):-var(Dict),!,getCtxValueND(CtxIn,Name,Value).
getCtxValueND(CtxIn,Name,Value):-prolog_must(nonvar(Name)),getCtxValueND0(CtxIn,Name,Value).
getCtxValueND(CtxIn,Dict:Name,Value):-getNamedCtxValue(CtxIn,Dict,Name,Value).

getCtxValueND0(CtxIn,Name,Value):-checkCtx(getCtxValueND,CtxIn), hotrace(( get_ctx_holder(CtxIn,Ctx),get_o_value(Name,Ctx,HValue,_Setter),unwrapValue(HValue,Value))).
getCtxValueND0(CtxI,Name,Value):-checkCtx(getCtxValueND,CtxI),lastMember(Ctx,CtxI),hotrace(( get_ctx_holder(Ctx,CtxH),get_o_value(Name,CtxH,HValue,_Setter), unwrapValue(HValue,Value))),ctrace.
*/

getCtxValue_nd(Ctx,Key,Value):- getNamedCtxValue(Ctx,Dict,Name,Value),dictNameKey(Dict,Name,Key).

getNamedCtxValue(CtxIn,Dict,Name,Value):-get_ctx_frame_holder(CtxIn,Dict,_Frame,Held),ctrace,get_c_value_wrapped(Held,Name,Value).
getNamedCtxValue(CtxIn,Dict,Name,Value):-lastMember(Ctx,CtxIn),hotrace((get_ctx_holder(Ctx,CtxHolder),ctxDict(CtxHolder,Dict,Held),get_c_value_wrapped(Held,Name,Value) )).
getNamedCtxValue(CtxIn,Dict,Name,'$deleted'(CtxIn,Dict,Name)):-ignore(Dict=nodict),ignore(Name=noname).

ctxDict(Ctx,Dict,[]):-var(Ctx),!,Dict=noctx.
ctxDict(frame(Named,_,Held),Dict,Held):-!,Named=Dict.
ctxDict(Ctx,Dict,Ctx):-!,Dict=unnamedctx.

get_c_value_wrapped(Ctx,Name,Value):-get_o_value(Name,Ctx,Value,_Setter).%%,unwrapValue(HValue,Value).
get_c_value_wrapped(_Ctx,Name,'$deleted'(Name)):-ignore(Name=noname).

setCtxValue(Ctx,Name,Value):-(getCtxValueND(Ctx,Name,PrevValue),!, Value==PrevValue) -> true; addCtxValue1(Ctx,Name,Value).

addCtxValue(Ctx,Name,Value):-checkCtx(addCtxValue,Ctx),addCtxValue1(Ctx,Name,Value),!.
%%addCtxValue1(Ctx,Name,Value):-get_ctx_holderFreeSpot(Ctx,Name=v(Value,Setter,Destructor),Destructor),!,ignore(Setter=set_v3(Name)).
addCtxValue1(CtxIn,Name,Value):-get_ctx_frame_holder(CtxIn,_Dict,Ctx,_Held),get_ctx_holderFreeSpot(Ctx,NameValue,Destructor), NameValue = (Name=v(Value,set_v3(Name),Destructor)).
                                                                                             

%%remCtxValue(Ctx,Name,_Value):-checkCtx(remCtxValue,Ctx),setCtxValue(Ctx,Name,'$deleted'),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% get the frame holder
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_ctx_frame_holder(R,NameIn,Frame,Held):- prolog_must(get_ctx_frame_holder0(R,NameIn,Frame,Held)).

get_ctx_frame_holder0(v(_,_,_),_Name,_R,_Held):-!,fail.
get_ctx_frame_holder0(R,NameIn,Frame,Held):- R = frame(Name,_,Inner),!, Name\=destroyed(_),
                        (  get_ctx_frame_holder0(Inner,NameIn,Frame,Held); 
                           (Name=NameIn,prolog_mustEach((Held=Inner,R=Frame,Name=NameIn,!)))).
get_ctx_frame_holder0([H|T],Name,R,Held):- nonvar(H), !, ( get_ctx_frame_holder0(T,Name,R,Held);get_ctx_frame_holder0(H,Name,R,Held)) .
%%get_ctx_frame_holder(Ctx,Name,Ctx):-!,get_ctx_frame_holder(Ctx,Name,R).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% get the holders areas last in first out %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% get_ctx_holder(+Ctx, -PlaceToSearch),

get_ctx_holder(Ctx,R):-compound(Ctx),get_ctx_holder1(Ctx,R).
get_ctx_holder1([H|T],R):- nonvar(H), !, ( get_ctx_holder(T,R);get_ctx_holder1(H,R)) .
get_ctx_holder1(v(_,_,_),_R):-!,fail.%% get_ctx_holder(Ctx,R).
get_ctx_holder1(frame(_N,_Dest,Ctx),R):-!,get_ctx_holder(Ctx,R).
get_ctx_holder1(l2r(H,T),R):- !, ( get_ctx_holder1(H,R);get_ctx_holder(T,R)) .
get_ctx_holder1(assoc(Ctx),assoc(Ctx)):-!.
%get_ctx_holder1(Ctx,R):- functor(Ctx,F,A),A<3,!,fail.
get_ctx_holder1(Ctx,Ctx).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% find a free area to place a: vv(name,val) %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% get_ctx_holderFreeSpot(+Ctx, -Put_NV, -CallToRemoveNV)

get_ctx_holderFreeSpot(Ctx,NamedValue,mdestruct(no_cyclic_terms)):-no_cyclic_terms,!,get_ctx_holderFreeSpot0(Ctx,NamedValue,_NO_Destruct),!.
get_ctx_holderFreeSpot(Ctx,NamedValue,Destruct):-get_ctx_holderFreeSpot0(Ctx,NamedValue,Destruct).

get_ctx_holderFreeSpot0(Ctx,NamedValue,Destruct):-compound(Ctx),get_ctx_holderFreeSpot1(Ctx,NamedValue,Destruct).

get_ctx_holderFreeSpot1(assoc(_Ctx),_,_):-!,fail.
get_ctx_holderFreeSpot1(frame(Key,_Inner_Dest,Ctx),NamedValue,Destruct):- nonvar(Key), !, get_ctx_holderFreeSpot1(Ctx,NamedValue,Destruct).
get_ctx_holderFreeSpot1(Ctx,NamedValue,Destruct):-get_ctx_holderFreeSpot2(Ctx,Ctx,NamedValue,Destruct),!.

get_ctx_holderFreeSpot2(_,Try1,NamedValue,nb_setarg(1,Try1)):- var(Try1),!, Try1 = [NamedValue|_NEXT],ctrace.
get_ctx_holderFreeSpot2(_,[_|Try1],NamedValue,nb_setarg(1,Try1)):- var(Try1),!, Try1 = [NamedValue|_NEXT].
get_ctx_holderFreeSpot2(_,[_|Try2],NamedValue,Destruct):-get_ctx_holderFreeSpot0(Try2,NamedValue,Destruct).

%%get_ctx_holderFreeSpot1(Ctx,_,_,NamedValue,_):-!,fail.
%%get_ctx_holderFreeSpot1(Ctx,_,_,NamedValue,nb_setarg(N,NEXT)):-arg(N,Ctx,Try3),var(Try3),!, Try3 = [NamedValue|NEXT].
%%get_ctx_holderFreeSpot1(Ctx,_,_,NamedValue,Destruct):-arg(N,Ctx,Try4),get_ctx_holderFreeSpot0(Try4,NamedValue,Destruct).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% find the value holder associated with a keyname
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_ctx_value(Name,Ctx,Value,Setter):-nonvar(Name),var(Value),get_o_value(Name,Ctx,Value,OuterSetter),bestSetterFn(Value,OuterSetter,Setter).

get_o_value00(Name,Ctx,Value,Setter):-get_o_value0(Name,Ctx,Value,HIDE_Setter),((no_cyclic_terms,cyclic_term(HIDE_Setter))-> Setter=no_setter(cyclicOn(Name)) ; Setter =HIDE_Setter).
get_o_value(Name,Ctx,Value,Setter):-hotrace(get_o_value00(Name,Ctx,Value,Setter)).

get_o_value0(Name,Ctx,Value,Setter):-compound(Ctx),get_o_value1(Name,Ctx,Value,Setter).
get_o_value1(Name,[H|T],Value,Setter):- !,(get_o_value0(Name,T,Value,Setter);get_o_value1(Name,H,Value,Setter)).
get_o_value1(Name,ASSOC,Value,set_assoc(ASSOC)):- ASSOC = assoc(Ctx),!, get_assoc(Name,Ctx,Value).
get_o_value1(Name,frame(Key,_Inner_Dest,Ctx),Value,Setter):- nonvar(Key),!, get_o_value0(Name,Ctx,Value,Setter).
get_o_value1(Name,l2r(H,T),Value,Setter):- !,(get_o_value1(Name,H,Value,Setter);get_o_value0(Name,T,Value,Setter)).
get_o_value1(Name,Pred,Value,Setter):-functor(Pred,F,A),!,get_n_value(Name,Pred,F,A,Value,Setter),!.

get_n_value(Name,Name,_F,_A,_Value,_):-!,fail.
get_n_value(Name,Pred,Name,1,Value,nb_setarg(1,Pred)):-arg(1,Pred,Value).
get_n_value(Name,Pred,Name,_,Value,Setter):- arg(1,Pred,Value),!,arg(2,Pred,Setter). %% value can actually be 'Pred'
get_n_value(Name,Pred,Dash,2,Value,nb_setarg(2,Pred)):-arg(1,Pred,Name),member(Dash,[=,-,vv]),!, arg(2,Pred,Value).
%%get_n_value(Name,Pred,'.',2,Value,Setter):-arg(2,Pred,Try1), get_o_value0(Try1,Name,Value,Setter);(arg(1,Pred,Try2),get_o_value0(Try2,Name,Value,Setter)).
%%get_n_value(Name,Pred,_,_,Value,Setter):- !, arg(_,Pred,Try2),get_o_value0(Try2,Name,Value,Setter).

% ===================================================================
% attribute searching (Document contexts)
% ===================================================================
/*
attributeValue(Ctx,Scope,Name,Value,Else):-nonvar(Value),!,checkNameValue(attributeValue,Ctx,Scope,Name,Value,Else).
attributeValue(Ctx,Scope,Name,Value,_ElseVar):-peekNameValue0(Ctx,Scope,Name,Value),!.
attributeValue(Ctx,_Scope,Name,Value,ElseVar):-makeParamFallback(Ctx,Name,Value,ElseVar),!.
*/
attributeValue(Ctx,ATTRIBS,NameS,ValueO,_Else):- hotrace((findAttributeValue(Ctx,ATTRIBS,NameS,ValueO,'$failure'))),!.
attributeValue(Ctx,XML,NameS,ValueO,_Else):- hotrace((findTagValue(Ctx,XML,NameS,ValueO,'$failure'))),!.
attributeValue(Ctx,ATTRIBS,NameS,ValueO,_Else):-compound(ATTRIBS),ATTRIBS=..[_|LIST],member(E,LIST),
   attributeValue(Ctx,E,NameS,ValueO,'$failure'),!.
attributeValue(Ctx,Scope,NameS,ValueO,ElseVar):-ElseVar\=='$failure',makeParamFallback(Ctx,Scope,NameS,ValueO,ElseVar),!.

findAttributeValue(Ctx,ATTRIBS,NameS,ValueO,Else):- hotrace((findAttributeValue0(Ctx,ATTRIBS,NameS,ValueI,Else), aiml_eval_to_unit(Ctx,ValueI,ValueO))),!.
findAttributeValue(Ctx,ATTRIBS,NameS,ValueO,Else):-   Else\=='$failure',prolog_must((findAttributeValue0(Ctx,ATTRIBS,NameS,ValueI,Else), aiml_eval_to_unit(Ctx,ValueI,ValueO))),!.

findAttributeValue0(_Ctx,ATTRIBS,NameS,ValueO,_Else):- member(Name,NameS), lastMember(NameE=ValueO,ATTRIBS), atomsSameCI(Name,NameE),!.
findAttributeValue0(Ctx,ATTRIBS,NameS,Value,ElseVar):- makeParamFallback(Ctx,ATTRIBS,NameS,Value,ElseVar),!.

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

