% ===================================================================
% File 'logicmoo_module_aiml_loader.pl'
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


:-dynamic(dict/3).
:-multifile(dict/3).


% ===================================================================
% attribute searching
% ===================================================================

attributeOrTagValue(Ctx,ATTRIBS,NameS,ValueO,_Else):- noaimltrace((attributeValue(Ctx,ATTRIBS,NameS,ValueO,'$failure'))),!.
attributeOrTagValue(Ctx,XML,NameS,ValueO,_Else):- noaimltrace((findTagValue(Ctx,XML,NameS,ValueO,'$failure'))),!.
attributeOrTagValue(Ctx,ATTRIBS,NameS,ValueO,_Else):-compound(ATTRIBS),ATTRIBS=..[_|LIST],member(E,LIST),
   attributeOrTagValue(Ctx,E,NameS,ValueO,'$failure'),!.
attributeOrTagValue(Ctx,_,NameS,ValueO,ElseVar):-ElseVar\=='$failure',makeParamFallback(Ctx,NameS,ValueO,ElseVar),!.

attributeValue(Ctx,ATTRIBS,NameS,ValueO,Else):- noaimltrace((attributeValue0(Ctx,ATTRIBS,NameS,ValueI,Else), aiml_eval_to_unit(Ctx,ValueI,ValueO))),!.
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

%['name'='SomeName','Description'='some descr','Input'='$error','ExpectedAnswer'='SomeAnswwer']
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

set_current_value(Ctx,N,V):-pushNameValue(Ctx,user,N,V).

% ===============================================================================================
%    AIML Runtime Database
% ===============================================================================================
:-dynamic(saveDFAttribute/3).
                
saveFAttribute(Ctx,File,A):-saveDFAttribute(Ctx,File,A),!.
saveFAttribute(Ctx,File,A):-asserta(saveDFAttribute(Ctx,File,A)),dynamic(File/A).

pushAttributes(Ctx,Scope,List):-pushCtxFrame(Scope,Ctx,List),pushAttributes0(Ctx,Scope,List),!.
pushAttributes0(Ctx,Scope,[N=V|L]):-pushNameValue(Ctx,Scope,N,V),pushAttributes0(Ctx,Scope,L).
pushAttributes0(_Ctx,_Scope,[]).

peekAttributes(Ctx,[Name|SList],Scope,[Name=Value|Results]):- peekNameValue(Ctx,Scope,Name,Value),peekAttributes(Ctx,SList,Scope,Results),!.
peekAttributes(_Ctx,[],_Scope,[]):-!.

current_value(Ctx,Name,ValueI):-peekNameValue(Ctx,_,Name,ValueI).

peekNameValue(Ctx,Scope,Name,Value):-peekNameValue(Ctx,Scope,Name,Value,Failed),ignore((Value==Failed,trace,Value = '*')),!.

peekNameValue(Ctx,Scope,Name,Value,Else):-nonvar(Value),!,checkNameValue(Ctx,Scope,Name,Value,Else).
peekNameValue(Ctx,_Scope,Name,Value,_ElseVar):-getCtxValue(Name,Ctx,Value),!.
peekNameValue(Ctx,List,Name,Value,_ElseVar):- nonvar(List),not(atom(List)),attributeOrTagValue(Ctx,List,Name,Value,'$failure'),!.
peekNameValue(_Ctx,Scope,Name,Value,_ElseVar):-dict(Scope,Name,Value),checkAttribute(Scope,Name,Value),!.
peekNameValue(_Ctx,Scope,Name,Value,_ElseVar):-nonvar(Scope),dict(Scope2,Name,Value),Scope\=Scope2,checkAttribute(Scope2,Name,Value),!,checkValue(Value).
peekNameValue(Ctx,_Scope,Name,Value,ElseVar):-makeParamFallback(Ctx,Name,Value,ElseVar),!.
%%peekNameValue(_Ctx,_Scope,_Name,Value,ElseVar):-ignore(Value=ElseVar),!.

checkNameValue(Ctx,Scope,[Name],Value,Else):-!,peekNameValue(Ctx,Scope,Name,ValueVar,Else),!,checkValue(ValueVar),valuesMatch(Ctx,ValueVar,Value),!.
checkNameValue(Ctx,Scope,Name,Value,Else):-peekNameValue(Ctx,Scope,Name,ValueVar,Else),!,checkValue(ValueVar),valuesMatch(Ctx,ValueVar,Value),!.

valuesMatch(_Ctx,_V,A):-A=='*'.
valuesMatch(_Ctx,V,_A):-V=='*'.
valuesMatch(_Ctx,V,A):-V=A.
valuesMatch(Ctx,V,A):-compound(V),convertToMatchable(V,VV),!,valuesMatch0(Ctx,VV,A).
valuesMatch(Ctx,V,A):-compound(A),convertToMatchable(A,AA),!,valuesMatch0(Ctx,V,AA).

valuesMatch0(_Ctx,V,A):-V=A.
valuesMatch0(Ctx,[V|VV],[A|AA]):-valuesMatch0(Ctx,V,A),!,valuesMatch0(Ctx,VV,AA).
valuesMatch0(Ctx,[V],A):-!,valuesMatch0(Ctx,V,A).
valuesMatch0(Ctx,V,[A]):-!,valuesMatch0(Ctx,V,A).

valueMP(Var,M):- member(M, [var(Var), Var=missing, Var=[], Var=(*) , (Var=(-(_))) ]),M,!.
valueMP(V,(V='ERROR')):-prolog_must(ground(V)),term_to_atom(V,A), concat_atom_safe([_,_|_],'ERROR',A),!.


checkValue(Value):- valueMP(Value,M),throw_safe(M),!.
checkValue(_):-!.

valuePresent(Value):- valueMP(Value,_M),!,fail.
valuePresent(_):-!.


popAttributes(Ctx,Scope,[N=V|L]):- !,checkAttribute(Scope,N,V),popNameValue(Ctx,Scope,N,V),!,popAttributes(Ctx,Scope,L),!.
popAttributes(_Ctx,_Scope,[]).

withAttributes(_Ctx,ATTRIBS,Call):-ATTRIBS==[],!,Call.
withAttributes(Ctx,ATTRIBS,Call):- 
   ensureScope(Ctx,ATTRIBS,Scope),
   checkAttributes(Scope,ATTRIBS),
   call_cleanup((
    once(pushAttributes(Ctx,Scope,ATTRIBS)),
    Call),
    once(popAttributes(Ctx,Scope,ATTRIBS))).

checkAttributes(Scope,ATTRIBS):-prolog_must(nonvar(ATTRIBS)),maplist(checkAttribute(Scope),ATTRIBS).
checkAttribute(Scope,N=V):-checkAttribute(Scope,N,V).
checkAttribute(Scope,N,_V):-N==proof,!,prolog_must(nonvar(Scope)).
checkAttribute(Scope,N,V):-prolog_must(nonvar(Scope)),prolog_must(nonvar(N)),!,prolog_must(nonvar(V)).

pushNameValue(_Ctx,Scope,N,V):-
   checkAttribute(Scope,N,V),
   asserta(dict(Scope,N,V)),!.

popNameValue(_Ctx,Scope,N,V):-
   dict(Scope,N,V),checkAttribute(Scope,N,V),ignore(retract(dict(Scope,N,V))),!,
   checkAttribute(Scope,N,V),!.

dyn_retract(dict(Scope,N,V)):-(retract(dict(Scope,N,V))),!.

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
   makeSingleTag(Ctx,O,Assert,UnboundDefault,Tag,RR),
   makeAllParams(Ctx,Order,Assert,UnboundDefault,Result),!.
makeAllParams(_Ctx,[],_,_,[]).


makeSingleTag(Ctx,Name,ATTRIBS,Default,Tag,Result):-atom(Name),!,makeSingleTag(Ctx,[Name],ATTRIBS,Default,Tag,Result).
makeSingleTag(Ctx,NameS,ATTRIBS,Default,Tag,ValueO):-makeAimlSingleParam0(Ctx,NameS,ATTRIBS,Default,Tag,ValueI),
      transformTagData(Ctx,Tag,Default,ValueI,ValueO).

makeAimlSingleParam0(_Ctx,[N|NameS],ATTRIBS,_D,N,Value):-member(O,[N|NameS]),lastMember(OI=Value,ATTRIBS),atomsSameCI(O,OI),!.
makeAimlSingleParam0(Ctx,[N|NameS],_,ElseVar,N,Value):- makeParamFallback(Ctx,[N|NameS],Value,ElseVar),!.


% ===============================================================================================
%  Fallback
% ===============================================================================================

makeParamFallback(Ctx,Name,Value,ElseVar):-var(ElseVar),!,throw_safe(makeParamFallback(Ctx,Name,Value,ElseVar)).
makeParamFallback(Ctx,Name,Value,ElseVar):-atom(Name),!,makeParamFallback(Ctx,[Name],Value,ElseVar).
makeParamFallback(_Ctx,_NameS,Value,ElseVar):-'var'(ElseVar),!,Value=ElseVar,!.
makeParamFallback(_Ctx,_NameS,_Value,'$failure'):-!,fail.
makeParamFallback(_Ctx,_NameS,_Value,'$call'(Prolog)):-!,Prolog.
makeParamFallback(Ctx,NameS,Value,'$error'):-aiml_error(makeParamFallback(Ctx,NameS,Value,'$error')),throw_safe(fallbackValue(Ctx,NameS,Value,'$error')),!.
makeParamFallback(Ctx,NameS,ValueO, '$current_value'):- member(Name,NameS),current_value(Ctx,Name,ValueO),valuePresent(ValueO),!.
makeParamFallback(_Ctx,_NameS,_Value,'$succeed'):-!.
makeParamFallback(_Ctx,_NameS,ValueO,Else):-ValueO=Else,!.
makeParamFallback(_Ctx,_NameS,ValueO,Else):-trace,debugFmt(ignore(ValueO=Else)),!.

% ===============================================================================================
%  Popping when Building categories
% ===============================================================================================

clearCateStack(_Ctx):-retractall(dict(category,_,_)).

peekCateElements(Ctx,Cate):- cateMemberTags(CATETAGS), peekAttributes(Ctx,CATETAGS,category,Cate),!.

popCateElements(Ctx,Cate):- cateMemberTags(CATETAGS), peekAttributes(Ctx,CATETAGS,category,Cate),!.
popCateElements(Ctx,CateO):- popCateElements1(Ctx,Cate1),popCateElements2(Ctx,Cate2),append(Cate1,Cate2,Cate),!,CateO=Cate.
popCateElements1(Ctx,CateO):- findall(Tag=DCG,cateNodes1(Ctx,category,Tag,DCG),Cate),!,CateO=Cate.
popCateElements2(Ctx,CateO):- findall(Tag=DCG,cateNodes2(Ctx,category,Tag,DCG),Cate),!,CateO=Cate.


cateNodes1(Ctx,Scope,Tag,DCGO):-member(Tag,[pattern,template]),once(cateNodes1a(Ctx,Scope,Tag,TEMPLATE)),once(convert_template(Ctx,TEMPLATE,DCG)),!,DCG=DCGO.

cateNodes1a(Ctx,Scope,Tag,DCGO):-peekNameValue(Ctx,Scope,Tag,DCG),popNameValue(Ctx,Scope,Tag,DCG),!,DCG=DCGO.
cateNodes1a(Ctx,Scope,Tag,DCGO):-listing(dict),aiml_error(peekNameValue(Ctx,Scope,Tag,DCG)),!,DCG=DCGO.
cateNodes1a(Ctx,Scope,Tag,DCGO):-peekNameValue(Ctx,Other,Tag,DCG),Other\==Scope,!,DCG=DCGO.


cateNodes2(Scope,Tag,DCGO):-member(Tag,[that,guard,topic]),once(cateNodes2a(Scope,Tag,TEMPLATE)),once(convert_template(_Ctx,TEMPLATE,DCG)),!,DCG=DCGO.

cateNodes2a(Scope,Tag,DCGO):-peekNameValue(_Ctx,Other,Tag,DCG),Other\==Scope,!,DCG=DCGO.
cateNodes2a(Scope,Tag,DCGO):-aiml_error(peekNameValue(_Ctx,Scope,Tag,DCG)),!,DCG=DCGO.

defaultPredicates(N,V):-member(N,[username,botname]),V='*'.
defaultPredicates(N,V):-defaultPredicatesS(S),member(N=V,S).
defaultPredicatesS([topic='*',
             precall='true',
             call='true',
             flags='*',
             that='*',
             dictionary='userdict',
             userdict='user',
             substitutions='input',
             graph='default',
             guard='*',
             request='*',
             lang='bot']).
 
cateMember(Tag):-cateMemberTags(List),member(Tag,List).

cateFallback([
       srcinfo=missing,
       srcfile=missing,
       withCategory=[writeqnl,asserta_new],
       pattern='ERROR PATTERN',
       template='ERROR TEMPLATE'|MORE]):-findall(N=V,defaultPredicates(N,V),MORE).

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

%well i played with a couple few differnt environment impls.. they have their pros cons.. one impl.. that was unique is that an array of "binding pairs" live in an arraylist.. to be "in" an environment it meant that you held an "index" into the arry list that as you went backwards you'd find your bindings.. each symbol had a java int field "lastBindingIndex" .. that was a "hint" to where you could fastforward the backwards search .. end named binding context also had a "index" to when you leave a named block.. you could quickly reset the top of an index.

% ===================================================================
:-dynamic(no_cyclic_terms).

no_cyclic_terms.

makeAimlContext(Name,Ctx):-makeAimlContext1(Name,Ctx),!,setCtxValue(ctx,Ctx,Name),!.

makeAimlContext1(Gensym_Key, [frame(Gensym_Key,no_destructor,[assoc(AL)|_])|_]):-    
   list_to_assoc([
    a-v(is_a,set_assoc,no_destructor(a)),
    a-v(is_a2,set_assoc,no_destructor(a)),
    b-v(is_b,set_assoc,no_destructor(b))],AL).


unwrapValue(HValue,TValue):-TValue==deleted,!,not(unwrapValue1(HValue,_)),!.
unwrapValue(HValue,TValue):-unwrapValue1(HValue,Value),!,TValue=Value.

unwrapValue1(v(ValueHolder,_SetterFun,_KeyDestroyer),Value):-!,unwrapValue1(ValueHolder,Value).
unwrapValue1(deleted,_):-!,fail.
unwrapValue1(Value,Value):-!.

bestSetterFn(v(_,Setter,_),_OuterSetter,Setter):-!.
bestSetterFn(_Value,OuterSetter,OuterSetter).

getCtxValue(Name,Ctx,Value):-checkCtx(Ctx),hotrace(get_ctx_holder(Ctx,Holder)),get_o_value(Name,Holder,HValue,_Setter),!, unwrapValue(HValue,Value).

setCtxValue(Name,Ctx,Value):-checkCtx(Ctx),get_ctx_holder(Ctx,Holder),get_o_value(Name,Holder,HValue,Setter),unwrapValue(HValue,CurrentValue),!,(CurrentValue=Value;call(Setter,Value)),!.
setCtxValue(Name,Ctx,Value):-checkCtx(Ctx),addCtxValue1(Name,Ctx,Value),!.

addCtxValue(Name,Ctx,Value):-checkCtx(Ctx),addCtxValue1(Name,Ctx,Value),!.
addCtxValue1(Name,Ctx,Value):-get_ctx_holderFreeSpot(Ctx,Name=v(Value,Setter,Destructor),Destructor),!,ignore(Setter=no_setter(Name)).

remCtxValue(Name,Ctx,_Value):-checkCtx(Ctx),setCtxValue(Name,Ctx,deleted),!.


pushCtxFrame(Name,Ctx,NewValues):-checkCtx(Ctx),get_ctx_holderFreeSpot(Ctx,Holder,GuestDest),!,Holder=frame(Name,GuestDest,NewValues).

popCtxFrame(Name,Ctx,PrevValues):-checkCtx(Ctx),get_ctx_frame_holder(Ctx,Name,Frame),Frame = frame(Name,Destructor,PrevValues),Destructor,!.

checkCtx(Ctx):-nonvar(Ctx),!.
checkCtx(Ctx):-makeAimlContext(broken,Ctx),!.

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

get_ctx_holderFreeSpot(Ctx,NamedValue,no_destructor(holder)):-no_cyclic_terms,!,get_ctx_holderFreeSpot0(Ctx,NamedValue,_NO_Destruct),!.
get_ctx_holderFreeSpot(Ctx,NamedValue,Destruct):-noaimltrace(get_ctx_holderFreeSpot0(Ctx,NamedValue,Destruct)).

get_ctx_holderFreeSpot0(Ctx,NamedValue,Destruct):-compound(Ctx),get_ctx_holderFreeSpot1(Ctx,NamedValue,Destruct).

get_ctx_holderFreeSpot1(assoc(_Ctx),_,_):-!,fail.
get_ctx_holderFreeSpot1(frame(Key,_Inner_Dest,Ctx),NamedValue,Destruct):- nonvar(Key), !, get_ctx_holderFreeSpot1(Ctx,NamedValue,Destruct).
get_ctx_holderFreeSpot1(Ctx,NamedValue,Destruct):-functor(Ctx,F,A),!,get_ctx_holderFreeSpot1(Ctx,F,A,NamedValue,Destruct).

get_ctx_holderFreeSpot1(Ctx,'.',2,NamedValue,nb_setarg(Ctx,2,NEXT)):-arg(2,Ctx,Try1), var(Try1),!, Try1 = [NamedValue|NEXT].
get_ctx_holderFreeSpot1(Ctx,'.',2,NamedValue,Destruct):-arg(2,Ctx,Try2),get_ctx_holderFreeSpot0(Try2,NamedValue,Destruct).

%%get_ctx_holderFreeSpot1(Ctx,_,_,NamedValue,_):-!,fail.
%%get_ctx_holderFreeSpot1(Ctx,_,_,NamedValue,nb_setarg(Ctx,N,NEXT)):-arg(N,Ctx,Try3),var(Try3),!, Try3 = [NamedValue|NEXT].
%%get_ctx_holderFreeSpot1(Ctx,_,_,NamedValue,Destruct):-arg(N,Ctx,Try4),get_ctx_holderFreeSpot0(Try4,NamedValue,Destruct).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% find the value holder associated with a keyname
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_ctx_value(Name,Ctx,Value,Setter):-nonvar(Name),var(Value),get_o_value(Name,Ctx,Value,OuterSetter),bestSetterFn(Value,OuterSetter,Setter).

get_o_value(Name,Ctx,Value,no_setter(Name)):-no_cyclic_terms,!,get_o_value0(Name,Ctx,Value,_HIDE_Setter),!.
get_o_value(Name,Ctx,Value,Setter):-hotrace(get_o_value0(Name,Ctx,Value,Setter)),!.

get_o_value0(Name,Ctx,Value,Setter):-compound(Ctx),get_o_value1(Name,Ctx,Value,Setter).
get_o_value1(Name,assoc(Ctx),Value,set_assoc):- get_assoc(Name,Ctx,Value),!.
get_o_value1(Name,frame(Key,_Inner_Dest,Ctx),Value,Setter):- nonvar(Key), get_o_value0(Name,Ctx,Value,Setter),!.
get_o_value1(Name,[H|T],Value,Setter):- !,(get_o_value0(Name,T,Value,Setter);get_o_value1(Name,H,Value,Setter)).
get_o_value1(Name,Pred,Value,Setter):-functor(Pred,F,A),!,get_n_value(Name,Pred,F,A,Value,Setter).

get_n_value(Name,Name,_F,_A,_Value,_):-!,fail.
get_n_value(Name,Pred,Name,1,Value,nb_setarg(1,Pred)):-arg(1,Pred,Value).
get_n_value(Name,Pred,Name,_,Value,Setter):- arg(1,Pred,Value),!,arg(2,Pred,Setter). %% value can actually be 'Pred'
get_n_value(Name,Pred,Dash,2,Value,nb_setarg(2,Pred)):-arg(1,Pred,Name),member(Dash,[=,-,vv]),!, arg(2,Pred,Value).
%%get_n_value(Name,Pred,'.',2,Value,Setter):-arg(2,Pred,Try1), get_o_value0(Name,Try1,Value,Setter);(arg(1,Pred,Try2),get_o_value0(Name,Try2,Value,Setter)).
%%get_n_value(Name,Pred,_,_,Value,Setter):- !, arg(_,Pred,Try2),get_o_value0(Name,Try2,Value,Setter).


