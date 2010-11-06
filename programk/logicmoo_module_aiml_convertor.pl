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

:-discontiguous(convert_ele/3).


% ===================================================================
% ===================================================================

convert_text('',''):-!.
convert_text([],''):-!.
convert_text(C,D):-is_list(C),!,convert_text_list(C,D),!.
convert_text(A,O):-atom(A),!,convert_atom(A,O).
convert_text(A,''):-ignore_aiml(A),!.
convert_text(E,File):-aiml_error(convert_text(E,File)),!,E=File.


convert_text_list([],[]):-!.
convert_text_list([A],B):-!,convert_text(A,B).
convert_text_list(M,C):-delete(M,'',B), (M == B -> C=B ; convert_text_list(B,C)).
convert_text_list([A|AA],BBB):-convert_text(A,B),convert_text_list(AA,BB),!,flattem_append(B,BB,BBB0),!,BBB=BBB0.

convert_atom(A,Z):-convert_atom0(A,Y),!,Y=Z.
convert_atom(E,File):-aiml_error(convert_atom(E,File)),!,E=File.
%convert_atom(A,C):-atom_to_number(A,C),!.
convert_atom0(A,A):-concat_atom_safe([A],' ',A).
convert_atom0(A,C):-atomSplit(A,M),!,convert_text(M,C),!.
convert_atom0(A,A).

flattem_append(A,B,BBB):-flatten([A],AA),!,flatten([B],BB),!,append(AA,BB,BBB),!.



% ===============================================================================================
%  PATTERN/TEMPLATE normalization
% ===============================================================================================
convert_template(_Ctx,X,_Y):-var(X),throw_safe(var(X)).
convert_template(_Ctx,_X,Y):-nonvar(Y),throw_safe(nonvar(Y)).
convert_template(_Ctx,[],[]):-!.
convert_template(_Ctx,[ATOM],O):-atom(ATOM),!,atomSplit(ATOM,LIST),!,toAtomList(LIST,O),!.
convert_template(Ctx,[I|P],GOOD):- atom(I),atomSplit(I,LIST),!,toAtomList(LIST,O),!,convert_template(Ctx,P,L),!,flatten([O,L],GOOD),!.
convert_template(Ctx,[I|P],L):- ignore_aiml(I),!,convert_template(Ctx,P,L),!.
convert_template(Ctx,[I|P],GOOD):- convert_template(Ctx,I,O),!,convert_template(Ctx,P,L),!,flatten([O,L],GOOD),!.
convert_template(Ctx,element(TAG,ATTRIBS,P),element(TAG,ATTRIBS,PO)):-convert_template(Ctx,P,PO),!.
convert_template(Ctx,P,PO):-convert_element(Ctx,P,PO),!.


toAtomList(A,O):-delete(A,'',O),!.

convert_element(_Ctx,Input,Out):-atomic(Input),!,Out=Input.
convert_element(Ctx,Input,Out):-convert_ele(Ctx,Input,M),!,M=Out,!.
%%%,convert_ele(Ctx,M,OutO),!,OutO=Out.


      
nameOrValue(ALIST, _VALUE, NORV, 0):-lastMember(name=NORV,ALIST),!.
nameOrValue(ALIST, _VALUE, NORV, 0):-lastMember(var=NORV,ALIST),!.
nameOrValue(_XATS, VALUE, NORV, 1):- NORV = VALUE.

convert_ele(_Ctx,_X,Y):-nonvar(Y),throw_safe(nonvar(Y)).
convert_ele(_Ctx,In,_In):-not(ground(In)),aiml_error(not(ground(In))),!,fail.

convert_ele(Ctx,li(A),li(AA)):-convert_template(Ctx,A,AA).
convert_ele(_Ctx,element(NSLocal,_A,_B),_Out):- var(NSLocal),!,throw_safe(not(atom(NSLocal))),!.
convert_ele(Ctx,element(_NS:Local,A,B),Out):- !,convert_ele(Ctx,element(Local,A,B),Out),!.
convert_ele(_Ctx,element(NSLocal,_A,_B),_Out):-not(atom(NSLocal)),!,throw_safe(not(atom(NSLocal))),!.
convert_ele(Ctx,element(NSLocal,A,B),Out):- concat_atom_safe([_NS,Local],':',NSLocal),!,convert_ele(Ctx,element(Local,A,B),Out),!.
convert_ele(Ctx,element(html:TAG,A,B),Out):-!,convert_ele(Ctx,element(TAG,A,B),Out),!.
convert_ele(_Ctx,element(br,[],[]),'<br/>').
convert_ele(_Ctx,element(p,[],[]),'<p/>').
convert_ele(Ctx,element(pre,[],B),BB):-!,convert_template(Ctx,B,BB).

convert_ele(Ctx,element(catagory, A, B),Out):-convert_ele(Ctx,element(category, A, B),Out).
%%convert_ele(Ctx,element(Tag, A, B),BB):- member(Tag,[category,srai]), convert_template(Ctx,element(Tag, A, B),BB).


botGetSet(bot,bot,_NAME,_NUM).
botGetSet(get,user,_NAME,_NUM).
botGetSet(set,user,_NAME,0).

% bot/get/set
convert_ele(Ctx,element(TAG, ALIST, VALUE),element(TAG,NEWLIST,VALUEO)):-
            botGetSet(TAG,TYPE,NAME,NUM),not(member(var=_,ALIST)),         
            append(ALIST,[type=TYPE,var=NAME],NEWLIST),
            nameOrValue(ALIST,VALUE,NORV,NUM), 
            convert_template(Ctx,NORV,NAME), 
            convert_template(Ctx,VALUE,VALUEO).

% get_xxx/set_xxx
convert_ele(Ctx,element(VAR_ATOM, ALIST, V),element(get,[name=N|ALIST],VV)):-atom_concat_safe('get_',N,VAR_ATOM),convert_template(Ctx,V,VV).
convert_ele(Ctx,element(VAR_ATOM, ALIST, V),element(set,[name=N|ALIST],VV)):-atom_concat_safe('set_',N,VAR_ATOM),convert_template(Ctx,V,VV).

% bot_xxx/botxxx
convert_ele(Ctx,element(BOT_ATOM, ALIST, V),element(bot,[name=N|ALIST],VV)):-atom_concat_safe('bot_',N,BOT_ATOM),convert_template(Ctx,V,VV).
convert_ele(Ctx,element(BOT_ATOM, ALIST, V),element(bot,[name=N|ALIST],VV)):-atom_concat_safe('bot',N,BOT_ATOM),lengthAtLeast(N,2),convert_template(Ctx,V,VV),!.

% getXXX
convert_ele(Ctx,element(VAR_ATOM, ALIST, V),element(get,[name=N|ALIST],VV)):-atom_concat_safe('get',N,VAR_ATOM),lengthAtLeast(N,2),convert_template(Ctx,V,VV),!.

% version/name/favfood
convert_ele(Ctx,element(BOT_ATOM, ALIST, V),element(bot,[name=BOT_ATOM|ALIST],VV)):- member(BOT_ATOM,[version,id,favfood]),convert_template(Ctx,V,VV),!.

% ===================================================================
% ===================================================================

convert_ele(Ctx,element(random, [], B),random(BB)):-convert_template(Ctx,B,BB).
convert_ele(Ctx,element(li, [], B),li(BB)):-convert_template(Ctx,B,BB).
%DELAY convert_ele(Ctx,element(star, [], []),(*)).
convert_ele(_Ctx,element(a, [Target, Link], Name),A):-sformat(S,'<a ~q ~q>~w</a>',[Target, Link, Name]),string_to_atom(S,A).
convert_ele(_Ctx,element(a, [Link], Name),A):-sformat(S,'<a ~q>~w</a>',[Link, Name]),string_to_atom(S,A).

%DELAY convert_ele(Ctx,element(get, [name=Var], []),get(Var)):-!.
convert_ele(_Ctx,element(learn, [N=File]),load_any_file(File)):-pathAttrib(N),!.
convert_ele(_Ctx,element(sr,ALIST,MORE),element(srai,ALIST,[element(star,ALIST,MORE)])):-!.
convert_ele(_Ctx,element(star,ALIST,MORE),star(pattern,XLAT2,MORE2)):-!,starIndex(star,pattern,ALIST,MORE,XLAT2,MORE2).
  starIndex(_Tag,_Star,ALIST,MORE,XLAT2,MORE2):-convert_attributes(Ctx,ALIST,XLAT2),convert_template(Ctx,MORE,MORE2),!.

convert_ele(_Ctx,element(Tag,ALIST,MORE),star(Star,XLAT2,MORE2)):-starType(Tag,Star),!,starIndex(Tag,Star,ALIST,MORE,XLAT2,MORE2).
   starType(Tag,Star):-member(Tag=Star,[star=pattern,topicstar=topic,gruardstar=guard,inputstar=pattern,thatstar=that]),!.
   starType(Tag,Star):-atom_concat_safe(Star,'_star',Tag),!.
   starType(Tag,Star):-atom_concat_safe(Star,'star',Tag),!.

convert_ele(Ctx,element(Tag, ALIST , INNER_XML), RESULT):-
      transform_aiml_structure(Tag,NewTag,ALIST,NewProps,INNER_XML,NEWPATTERN),
      convert_ele(Ctx,element(NewTag, NewProps, NEWPATTERN),RESULT),!.

convert_ele(Ctx,L,LO):-is_list(L),flatten(L,M),!,
	    (L==M -> LO=M ; convert_template(Ctx,M,LO)).

%convert_ele(Ctx,A,B):-atom(A),atom_to_number(A,B).

convert_ele(_Ctx,A,W):-atom(A),atomSplit(A,B),!,convert_text(B,W),!.

convert_ele(Ctx,element(A, B, C),INNER_XML):-tagType(A, immediate),!,
      convert_name(A,AA),
      convert_attributes(Ctx,B,BB),
      convert_template(Ctx,C,CC),!,
   (element(A, B, C) == element(AA, BB, CC) ->  INNER_XML=element(AA, BB, CC); convert_element(Ctx,element(AA, BB, CC),INNER_XML)),!.

convert_ele(Ctx,element(A, B, C),INNER_XML):-
      convert_name(A,AA),
      convert_attributes(Ctx,B,BB),
      convert_template(Ctx,C,CC),!, 
   (element(A, B, C) == element(AA, BB, CC) ->  INNER_XML=element(AA, BB, CC); convert_element(Ctx,element(AA, BB, CC),INNER_XML)),!.

convert_ele(Ctx,element(Tag, A, B),element(Tag, A, BB)):- member(Tag,[category]), convert_template(Ctx,B,BB).

convert_ele(Ctx,element(Tag, A, B),element(Tag, A, BB)):- member(Tag,[srai]),trace,convert_template(Ctx,B,BB).

convert_ele(_Ctx,O,O).


convert_attributes(Ctx,A,AAA):- convert_attributes0(Ctx,A,AA),list_to_set_safe(AA,AAA).
convert_attributes0(Ctx,[B|A],[BB|AA]):-convert_attribute(B,BB),convert_attributes0(Ctx,A,AA).
convert_attributes0(_Ctx,[],[]).

convert_attribute(A=B,AA=BB):-convert_name(A,AA),convert_template(_Ctx,B,BB).

convert_name(A,AAA):-convert_name0(A,AA), (A==AA -> AAA=AA ; convert_name(AA,AAA)),!.

convert_name0(A,AA):-toLowercase(A,AA).
convert_name0(var,name).
convert_name0(Attrib,uri):-pathAttrib(Attrib),!.

% ===================================================================
% ===================================================================

% ===============================================================================================
%  refomat type transformations
% ===============================================================================================

isVerbatumTag(N):-memberchk(N,[call,precall,srcfile,srcdir,lineno,srcinfo]),!.
isVerbatumTag(N):-pathAttrib(N),!.


transformTagData(Ctx,[Name|S],Else,ValueI,ValueO):- member(N,[Name|S]),transformTagData0(Ctx,N,Else,ValueI,ValueO).
transformTagData(Ctx,[Name|S],Else,ValueI,ValueO):- member(N,[Name|S]),!,transformTagData1(Ctx,N,Else,ValueI,ValueO).
transformTagData(Ctx,Tag,Else,ValueI,ValueO):-transformTagData0(Ctx,Tag,Else,ValueI,ValueO).
transformTagData(Ctx,Tag,Else,ValueI,ValueO):-transformTagData1(Ctx,Tag,Else,ValueI,ValueO).

transformTagData0(_Ctx,TAG,_Default,[*],TAG).
transformTagData0(_Ctx,TAG,_Default,*,TAG).
transformTagData0(Ctx,Tag,_Else,ValueI,ValueO):- ValueI=='$current_value', current_value(Ctx,Tag,ValueO),!.
transformTagData0(_Ctx,N,Else,ValueO,ValueO):-isVerbatumTag(N),!, member(Else,['$current_value']),!.
transformTagData0(Ctx,TAG,_Default,PATTERN_IN,PATTERN_OUT):-isPatternTag(TAG),convert_pattern(Ctx,PATTERN_IN,PATTERN_OUT),!.
transformTagData0(Ctx,TAG,_Default,PATTERN_IN,PATTERN_OUT):-isOutputTag(TAG),convert_template_pred(Ctx,=,PATTERN_IN,PATTERN_OUT),!.

transformTagData1(_Ctx,TAG,_Default,PATTERN_IN,PATTERN_OUT):- member(TAG,[userdict,graph]),upcase_atom_safe(PATTERN_IN,PATTERN_OUT),!.
transformTagData1(_Ctx,TAG,_Default,PATTERN_IN,PATTERN_OUT):-infoTagLikeLineNumber(TAG),!,PATTERN_IN=PATTERN_OUT.

transformTagData1(Ctx,TAG,Default,PATTERN_IN,PATTERN_OUT):- debugFmt(transformTagData(TAG,Default,PATTERN_IN)), 
                 convert_template_pred(Ctx,upcase_atom_safe,PATTERN_IN,PATTERN_OUT),!.
transformTagData1(Ctx,_N,_Default,R,RR):-convert_template(Ctx,R,RR),!. 
transformTagData1(_Ctx,_TAG,_Default,PATTERN,PATTERN):-!.

% ===============================================================================================
% ===============================================================================================

convert_pattern(Ctx,PATTERN_IN,PATTERN_OUT):- convert_template_pred(Ctx,upcase_atom_safe_non_special,PATTERN_IN,PATTERN_OUT),!.

upcase_atom_safe_non_special(A,A):-not(atom(A)),!.
upcase_atom_safe_non_special(Atom,Atom):-atom_prefix(Atom,'#$'),!.
upcase_atom_safe_non_special(A,U):-upcase_atom_safe(A,U).

convert_template_pred(Ctx,Pred,PATTERN_IN,PATTERN_OUT):- convert_template(Ctx,PATTERN_IN,PATTERN_MID),!,
     debugOnFailureAiml(map_tree_to_list(Pred,PATTERN_MID,PATTERN_OUT)),!.

transform_aiml_structure(catagory,category,OldProps,OldProps,NEWPATTERN,NEWPATTERN).
transform_aiml_structure(alice,aiml,OldProps,OldProps,NEWPATTERN,NEWPATTERN).
transform_aiml_structure('name','bot',OldProps,[name=['name']|OldProps],NEWPATTERN,NEWPATTERN).
transform_aiml_structure(OldName,NewName,OldProps,NewProps,NEWPATTERN,NEWPATTERN):-
      specialIndex(OldName,NewName,AddProps),append(AddProps,OldProps,NewProps).

specialIndex(justbeforethat,that,[index=(2:1)]).
specialIndex(justthat ,input,[index=2]).
specialIndex(beforethat,input,[index=3]).

specialIndex(load,learn,[]).
specialIndex(set_female,set,[name=gender,value=female]).

specialIndex(getname,name,[name=[name]]).
specialIndex(gettopic,name,[name=[name]]).

specialIndex(personf,formatter,[type=url_encode]).
specialIndex(Name,formatter,[type=Method]):-formatterMethod(Name,Method).


formatterProc(Dict):-member(Dict,[formal,uppercase,lowercase,sentence,gossip,think,(format)]).
formatterMethod(NamedMethod,NamedMethod):-formatterProc(NamedMethod).


evaluatorsDicts(Dict):-member(Dict,[system,javascript,eval,
                                     cycquery,cycsystem,cycassert,
                                     fortunecookie,substitute,learn,aiml,genlMt,think,
                                     substitute,srai,testsuite,testcase,template]).


%substitutionDictsName(input,pattern).
substitutionDictsName(N,N):-substitutionDicts(N).

substitutionDicts(input).
substitutionDicts(output).
substitutionDicts(gender).
substitutionDicts(person).
substitutionDicts(person2).
substitutionDicts(person3).
%substitutionDicts(Dict):-evaluatorsDicts(Dict).


tagType(Tag,immediate):-evaluatorsDicts(Tag),!.
tagType(Tag,pushable):-cateFallback(LIST),member([Tag=_],LIST).
tagType(Tag,insideCate):-cateMember(Tag).

tagType(Tag,requiredCate):-member(Tag,[pattern,template]).
tagType(Tag,optionalCate):-cateMember(Tag),not(tagType(Tag,requiredCate)).



