% ===================================================================
% File 'logicmoo_module_aiml_eval.pl'
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

:-discontiguous(tag_eval/3).

% ===================================================================
%  Prolog-like call
% ===================================================================

aiml_call(Ctx,_ - Calls):- !,aiml_call(Ctx,Calls),!.

aiml_call(Ctx,[Atomic|Rest]):- atom(Atomic),!,trace, 
            aiml_eval(Ctx,[Atomic|Rest],Output),!,
            debugFmt(resultOf(aiml_call(Ctx,[Atomic|Rest]),Output)),!.

aiml_call(Ctx,[Atomic|Rest]):- !,trace, 
            aiml_eval(Ctx,[Atomic|Rest],Output),!,
            debugFmt(resultOf(aiml_call(Ctx,[Atomic|Rest]),Output)),!.

aiml_call(Ctx,element(A, B, C)):-tagType(A, immediate), prolog_must(nonvar(C)),
      convert_name(A,AA),
      convert_attributes(Ctx,B,BB),
      convert_template(Ctx,C,CC),
      (element(A, B, C) \== element(AA, BB, CC)),!,
      aiml_call(Ctx,element(AA, BB, C)),!.


aiml_call(Ctx,element(A, B, C)):- prolog_must(nonvar(C)),
      convert_name(A,AA),
      convert_attributes(Ctx,B,BB),
      convert_template(Ctx,C,CC),
      (element(A, B, C) \== element(AA, BB, CC)),!,trace,
      aiml_call(Ctx,element(AA, BB, C)),!.

aiml_call(Ctx,element(Learn, ATTRIBS, Value)):-  member(Learn,[load,learn]),!,
 debugOnFailureAiml((
     attributeValue(Ctx,ATTRIBS,[graph],Graph,'$current_value'),
     pathAttrib(PathAttrib),
     attributeValue(Ctx,ATTRIBS,PathAttrib,Filename,Value),
      withAttributes(Ctx,[srcfile=Filename,graph=Graph|ATTRIBS],
      load_aiml_files(Ctx,Filename)))).

aiml_call(Ctx,Call):- Call \= element(_,_,_), callEachElement(Ctx,Call),!.

aiml_call(Ctx,INNER_XML):-aiml_eval(Ctx,INNER_XML,Rendered),!, debugFmt(Rendered),!.

aiml_call(Ctx,element(genlmt,TOFROM,_)):-
 debugOnFailureAiml((
      attributeValue(Ctx,TOFROM,[to,name],TO,'$error'),
      attributeValue(Ctx,TOFROM,[graph,from],FROM,'$current_value'),
      immediateCall(Ctx,assertz(genlMtGraph(TO,FROM))))),!.

 aiml_call(Ctx,element(Learn, ATTRIBS, Value)):- aiml_error(aiml_call(Ctx,element(Learn, ATTRIBS, Value))),!.


% ===================================================================
%  Prolog-like call
% ===================================================================

callEachElement(Ctx,[C|Calls]):-!, callEachElement(Ctx,C),callEachElement(Ctx,Calls).
callEachElement(Ctx,element(A,B,C)):- convert_element(Ctx,element(A,B,C),ELE),callEachElement(Ctx,ELE),!.
callEachElement(_Ctx,C):-callInteractive(C,_).

% ===================================================================
%  render templates
% ===================================================================

aiml_eval_to_unit(Ctx,ValueI,ValueO):-is_list(ValueI),!,aiml_eval_each(Ctx,ValueI,ValueO),!.
aiml_eval_to_unit(Ctx,ValueI,ValueO):-aiml_eval0(Ctx,ValueI,ValueO),!.

render_value(template,ListOut,Render):-aiml_eval(_Ctx,ListOut,Render),!.

aiml_eval_each(Ctx,In,Out):-prolog_must((ground(In),var(Out))),aiml_eval_each_l(Ctx,In,Out).
aiml_eval_each_l(Ctx,[A|ATTRXML],Output):-aiml_eval0(Ctx,A,R),!,aiml_eval_each_l(Ctx,ATTRXML,RESULT),prolog_must(Output=[R|RESULT]).
aiml_eval_each_l(_Ctx,[],[]):-!.

aiml_eval(_Ctx,TAGATTRXML,RESULT):- TAGATTRXML == [],!,RESULT=TAGATTRXML.
aiml_eval(_Ctx,TAGATTRXML,_RESULT):- prolog_must(nonvar(TAGATTRXML)),fail.
aiml_eval(Ctx,TAGATTRXML,RESULT):- immediateCall(Ctx,aiml_eval_now(Ctx,TAGATTRXML)),aiml_eval0(Ctx,TAGATTRXML,RESULT),!.
aiml_eval_now(Ctx,TAGATTRXML):-aiml_eval(Ctx,TAGATTRXML,RESULT),!,debugFmt(aiml_eval_now(Ctx,TAGATTRXML,RESULT)).

immediateCall(Ctx,:-(Call)):-!,immediateCall0(Ctx,:-(Call)),!.
immediateCall(Ctx,Call):-immediateCall0(Ctx,:-(Call)),!.
immediateCall0(Ctx,C):-hideIfNeeded(C,Call),immediateCall1(Ctx,Call),!.
immediateCall1(_Ctx,C):- prolog_must(ground(C)),fail.
immediateCall1(_Ctx,Call):- (format('~q.~n',[Call])),debugFmt(Call),!.


%aiml_eval0(Ctx,[ValueI],ValueO):-atom(ValueI),!,aiml_eval(Ctx,ValueI,ValueO),!.
%aiml_eval0(Ctx,[Value|I],ValueO):-atom(Value),concat_atom([Value|I],' ',ValueI),!,aiml_eval(Ctx,ValueI,ValueO),!.
%aiml_eval0(Ctx,ValueI,ValueO):- !,ValueI=ValueO,!.

aiml_eval0(Ctx,I,R):- nonvar(R),throw_safe(var(R=aiml_eval0(Ctx,I,R))),!.
aiml_eval0(Ctx,_ - Calls,_):- var(Calls),throw_safe(var(Ctx=Calls)),!.

aiml_eval0(Ctx,_Num - Msg,Result):-is_list(Msg),!,aiml_eval_each(Ctx,Msg,Result),!.

aiml_eval0(Ctx,_Num - Msg,Result):-!,aiml_eval(Ctx,Msg,Result),!.

%aiml_evalL(_Ctx,[],[]):-!.
%aiml_evalL(Ctx,[Atomic|Rest],[Atomic|Output]):-atomic(Atomic),!,aiml_eval_each(Ctx,Rest,Output),!.

aiml_eval0(_Ctx,A,B):-atomic(A),!,B=A.

aiml_eval0(Ctx,element(Srai,ATTRIBS,DOIT),RETURN):- memberchk(Srai,[srai,template]),
      withAttributes(Ctx,ATTRIBS,
         (noaimltrace(aiml_eval_each(Ctx,DOIT,INNER)),
          computeAnswer(Ctx,1,element(Srai,ATTRIBS,INNER),RMID,_Votes))),!,
       RMID=RETURN.

aiml_eval0(Ctx,element(A, B, C), XML):-tagType(A, immediate),
      convert_name(A,AA),
      convert_attributes(Ctx,B,BB),
      aiml_eval_each(Ctx,C,CC),
      (element(A, B, C) \== element(AA, BB, CC)),!,
      aiml_eval(Ctx,element(AA, BB, CC),XML),!.


% NEXT aiml_evalL(Ctx,[A|AA], [B|BB]):- aiml_eval(Ctx,A,B),convert_template(Ctx,AA,BB),!.
%aiml_eval(Ctx,[A|AA], [B|BB]):- convert_element(Ctx,A,B),aiml_eval(Ctx,AA,BB),!.
%%aiml_eval(Ctx,[A|AA], [B|BB]):- convert_element(Ctx,A,B),convert_template(Ctx,AA,BB),!.



% ===================================================================
%  template tag impl
% ===================================================================


%aiml_eval(Ctx,INNER_XML,[debugFmt(Rendered)]):-aiml_eval(Ctx,INNER_XML,Rendered),!.


% ===================================================================
%  MISSING tag impl
% ===================================================================
%%aiml_eval(Ctx,AIML,[debugFmt(aiml_eval_missing(AIML))]):-!.


aiml_eval0(_Ctx,element(In, ATTRIBS, Value),element(In, ATTRIBS, Value)):- preserveTag(In,_Out),!.
aiml_eval0(Ctx,element(Learn, ATTRIBS, Value),RESULT):-tag_eval(Ctx,element(Learn, ATTRIBS, Value),RESULT),!.

aiml_eval0(Ctx,TAGATTRXML,RESULT):-TAGATTRXML=..[TAG,ATTR,[]],isAimlTag(TAG),!,tag_eval(Ctx,element(TAG,ATTR,[]),RESULT),!.
aiml_eval0(Ctx,TAGATTRXML,RESULT):-TAGATTRXML=..[TAG,ATTR,[X|ML]],isAimlTag(TAG),!,tag_eval(Ctx,element(TAG,ATTR,[X|ML]),RESULT),!.

aiml_eval0(Ctx,element(In, ATTRIBS, Value),Result):- convert_element(Ctx,element(In, ATTRIBS, Value),Result),!.

aiml_eval0(Ctx,element(Learn, ATTRIBS, Value),_):- aiml_error(aiml_eval(Ctx,element(Learn, ATTRIBS, Value))),!.

aiml_eval0(_Ctx,RESULT,RESULT):-!.


% ===================================================================
%  system tag impl
% ===================================================================

tag_eval(Ctx,I,R):- nonvar(R),throw_safe(var(R=tag_eval(Ctx,I,R))),!.
tag_eval(Ctx,_ - Calls,_):- var(Calls),throw_safe(var(tag_eval(Ctx=Calls))),!.

tag_eval(Ctx,element(system,ATTRIBS,INNER_XML),Output):-
         aiml_eval_each(Ctx,INNER_XML,Rendered),
         attributeValue(Ctx,ATTRIBS,[lang],Lang,['bot']),        
         systemCall(Ctx,Lang,Rendered,Output),!.


systemCall(Ctx,[Lang],Eval,Out):- nonvar(Lang),!, systemCall(Ctx,Lang,Eval,Out).

systemCall(_Ctx,_Lang,[],[]):-!.
systemCall(Ctx,Bot,[FIRST|REST],DONE):-atom_concat_safe('@',CMD,FIRST),!,systemCall(Ctx,Bot,[CMD|REST],DONE).
systemCall(Ctx,'bot',REST,OUT):-!,debugOnFailure(systemCall_Bot(Ctx,REST,OUT)),!.
systemCall(Ctx,Lang,[Eval],Out):-systemCall(Ctx,Lang,Eval,Out).
systemCall(Ctx,Lang,Eval,Out):-once((atom(Eval),atomSplit(Eval,Atoms))),Atoms=[_,_|_],!,trace,systemCall(Ctx,Lang,Atoms,Out).
systemCall(_Ctx,Lang,Eval,writeq(evaled(Lang,Eval))):- aiml_error(evaled(Lang,Eval)).


systemCall_Bot(_Ctx,['@'|DONE],template([did,DONE])):-!.
systemCall_Bot(Ctx,[FIRST|REST],DONE):-atom_concat_safe('@',CMD,FIRST),!,systemCall_Bot(Ctx,[CMD|REST],DONE).
systemCall_Bot(_Ctx,['eval'|DONE],template([evaled,DONE])):-!.
systemCall_Bot(Ctx,['set'],template([setted,Ctx])):-!,listing(dict).
%systemCall_Bot(Ctx,['ctx'],template([ctxed,Ctx])):-!,showCtx.
systemCall_Bot(Ctx,['load'|REST],OUT):-!,debugOnFailure(systemCall_Load(Ctx,REST,OUT)),!.
systemCall_Bot(Ctx,['chgraph',Graph],['chgraph',Graph]):-  set_current_value(Ctx,graph,Graph),!.


systemCall_Load(Ctx,[],template([loaded,Ctx])):-!.
systemCall_Load(Ctx,[File,Name|S],Output):-concat_atom_safe([File,Name|S],'',Filename),!,systemCall(Ctx,'bot',['load',Filename],Output).
systemCall_Load(Ctx,[Filename],template([loaded,Filename])):-
    current_value(Ctx,graph,GraphI), 
    (GraphI=='*'->Graph=default; Graph=GraphI),
    ATTRIBS=[srcfile=Filename,graph=Graph],
    gather_aiml_graph(Ctx,ATTRIBS,Graph,Filename,AIML),
    withAttributes(Ctx,ATTRIBS,load_aiml_structure(Ctx,AIML)),!.

% ===================================================================
%  learn tag impl
% ===================================================================

% 0.9 version
tag_eval(Ctx,element(Learn, ATTRIBS, EXTRA),done(new)/*NEW*/):- member(Learn,[load,learn]),
 debugOnFailureAiml((
     attributeValue(Ctx,ATTRIBS,[graph],Graph,'$current_value'),
     pathAttribS(PathAttribS),
     attributeValue(Ctx,ATTRIBS,PathAttribS,Filename,EXTRA),
      gather_aiml_graph(Ctx,ATTRIBS,Graph,Filename,MOREXML),
      append(EXTRA,MOREXML,NEWXML), 
      ATTRIBSNEW=[srcfile=Filename,graph=Graph|ATTRIBS],
       NEW = element(aiml,ATTRIBSNEW,NEWXML),  
        withAttributes(Ctx,ATTRIBSNEW,
            load_aiml_structure(Ctx,NEW)))),!.


gather_aiml_graph(Ctx,XML,Graph,Filename,AIML):-
 ATTRIBS=[srcfile=Filename,graph=Graph|XML],
 withAttributes(Ctx,ATTRIBS,graph_or_file(Ctx,ATTRIBS, Filename, AIML)),!.


graph_or_file(_Ctx,_ATTRIBS, [], []):-!.
graph_or_file(Ctx,ATTRIBS, [Filename], XML):-atomic(Filename),!,graph_or_file(Ctx,ATTRIBS, Filename, XML).

graph_or_file(Ctx,ATTRIBS,Filename,XML):-graph_or_file_or_dir(Ctx,ATTRIBS,Filename,XML).
graph_or_file(Ctx,ATTRIBS, Filename, XML):- 
     getCurrentFileDir(Ctx, ATTRIBS, CurrentDir),join_path(CurrentDir,Filename,Name),
     graph_or_file_or_dir(Ctx,[currentDir=CurrentDir|ATTRIBS],Name,XML),!.

graph_or_file(_Ctx,ATTRIBS, Filename, [nosuchfile(Filename,ATTRIBS)]).

join_path(CurrentDir,Filename,Name):-
         atom_ensure_endswtih(CurrentDir,'/',Out),atom_ensure_endswtih('./',Right,Filename),
         atom_concat(Out,Right,Name),!.

atom_ensure_endswtih(A,E,A):-atom(E),atom_concat(_Left,E,A),!.
atom_ensure_endswtih(A,E,O):-atom(A),atom(E),atom_concat(A,E,O),!.
atom_ensure_endswtih(A,E,O):-atom(A),atom(O),atom_concat(A,E,O),!.
atom_ensure_endswtih(A,O,O):-atom(A),atom(O),!.

graph_or_file_or_dir(_Ctx,_ATTRIBS, Filename, XML):-exists_file(Filename),!,load_structure(Filename,XML,[dialect(xml),space(remove)]),!.
graph_or_file_or_dir(Ctx,ATTRIBS, F, [element(aiml,DIRTRIBS,OUT)]):- DIRTRIBS = [srcdir=F|ATTRIBS],
      exists_directory(F), 
      aiml_files(F,Files), 
      findall(X, ((member(FF,Files), 
                   graph_or_file_or_dir(Ctx,[srcfile=FF|DIRTRIBS],FF,X))), OUT),!.


getCurrentFile(Ctx,_ATTRIBS,CurrentFile):-getItemValue(proof,Ctx,Proof),getItemValue(lastArg,Proof,CurrentFile).
getCurrentFileDir(Ctx,ATTRIBS,Dir):-getCurrentFile(Ctx, ATTRIBS, CurrentFile),file_directory_name(CurrentFile,Dir).

getItemValue(Name,Ctx,Value):-Name==lastArg,compound(Ctx),functor(Ctx,_F,A),arg(A,Ctx,Value),!.
getItemValue(Name,Ctx,Value):-getCtxValue(Name,Ctx,Value),!.
% ============================================
% Test Suite 
% ============================================
tag_eval(Ctx,element('testsuite',ATTRIBS,LIST),prologCall(maplist_safe(call,RESULT))):- 
   withAttributes(Ctx,ATTRIBS,aiml_eval_each(Ctx,LIST,RESULT)),!.
   
tag_eval(Ctx,Current,prologCall(TESTCALL)):- Current=element(TC,ATTRIBS,_LIST), member(TC,['testcase','TestCase']),     
 debugOnFailureAiml((
     attributeOrTagValue(Ctx,Current,['name'],Name,'SomeName'),
     attributeOrTagValue(Ctx,Current,['Input','Pattern'],Input,'ERROR Input'),
     attributeOrTagValue(Ctx,Current,['Description'],Description,'No Description'),
     attributeOrTagValue(Ctx,Current,['ExpectedAnswer'],ExpectedAnswer,'ERROR ExpectedAnswer'),
     attributeOrTagValue(Ctx,Current,['ExpectedKeywords'],ExpectedKeywords,'*'),
     (ExpectedKeywords=='*' -> Expected = ExpectedAnswer ;  Expected = ExpectedKeywords),     
     TESTCALL = testIt(ATTRIBS,Input=ExpectedAnswer,ExpectedKeywords=_Result,Name=Description,Ctx),
     debugFmt(testIt([Name,Description,Input,ExpectedAnswer,ExpectedKeywords,Expected])))),!.


prologCall(Call):-catch(Call,E,debugFmt(prologCall(Call,E))),!.

testIt(ATTRIBS,Input=ExpectedAnswer,ExpectedKeywords=Result,_Name=_Description,Ctx):-
   (ExpectedKeywords=='*' -> Expected = ExpectedAnswer ;  Expected = ExpectedKeywords),
    withAttributes(Ctx,ATTRIBS,(( runUnitTest(alicebot2(Ctx,Input,Resp),sameBinding(Resp,ExpectedAnswer),Result)))),!.


tag_eval(_Ctx,element(In, ATTRIBS, Value),element(In, ATTRIBS, Value)):- preserveTag(In,_Out),!.


preserveTag(In,Out):- member(Out,['input','description',expectedAnswer,'Name']),atomsSameCI(In,Out),!.


runUnitTest(Call,Req,Result):-runUnitTest1(Call,Result1),!,runUnitTest2(Req,Result2),!,Result=unit(Result1,Result2),debugFmt(Result),!.

runUnitTest1(Req,Result):-noaimltrace(catch((Req-> Result=passed(Req); Result=failed(Req)),E,Result=error(E,Req))).
runUnitTest2(Req,Result):-noaimltrace(catch((Req-> Result=passed(Req); Result=failed(Req)),E,Result=error(E,Req))).

sameBinding(X,Y):-noaimltrace((sameBinding1(X,X1),sameBinding1(Y,Y1),sameBinding1(X1,Y1))),!.

sameBinding1(X,X):-var(X),!.
sameBinding1(_-X,Y):-nonvar(X),!,sameBinding1(X,Y).
sameBinding1(X,X):-!.
sameBinding1(X,Y):- balanceBinding(X,Y),!.


