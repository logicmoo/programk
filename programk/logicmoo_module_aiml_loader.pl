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


:-ensure_loaded(library('programk/logicmoo_module_aiml_convertor.pl')).
:-ensure_loaded(library('programk/logicmoo_module_aiml_xpath.pl')).
:-discontiguous(load_dict_structure/2).


% =================================================================================
% AIML Loading
% =================================================================================
reloadAimlFiles:-withCurrentContext(reloadAimlFiles).
reloadAimlFiles(Ctx):-forall(retract(loaded_aiml_file(A,P,_)),assert(pending_aiml_file(A,P))),do_pending_loads(Ctx).

%%load_aiml_files:- aimlCateSig(CateSig),retractall(CateSig),fail.
%load_aiml_files:-once(load_aiml_files('programk/test_suite/*.aiml')),fail.
%%load_aiml_files:-once(load_aiml_files(Ctx,'*.aiml')),fail.
%load_aiml_files:-aimlCateSig(CateSig),pp_listing(CateSig).
load_aiml_files.

%%tell(f5),load_aiml_files('part5/*.aiml'),told.

load_aiml_files(Files):-currentContext(load_aiml_files,Ctx),load_aiml_files(Ctx,Files),!,do_pending_loads(Ctx).

load_aiml_files(Ctx,element(Tag,Attribs,ContentIn)):- !, debugOnFailureAiml((load_aiml_structure(Ctx,element(Tag,Attribs,ContentIn)),!,do_pending_loads(Ctx))).
load_aiml_files(Ctx,File):- withAttributes(Ctx,[withCategory=[writeqnl,asserta_new]],with_files(load_single_aiml_file(Ctx),File)),!,do_pending_loads(Ctx).


translate_aiml_files(Files):-currentContext(translate_aiml_files,Ctx),translate_aiml_files(Ctx,Files),!.

translate_aiml_files(Ctx,File):-not(is_list(File);atom(File)),translate_aiml_structure(Ctx,File),!.
translate_aiml_files(Ctx,File):-with_files(translate_single_aiml_file(Ctx),File),!.


with_files(_Verb,[]):-!.
with_files(Verb,[File|Rest]):-!,maplist_safe(Verb,[File|Rest]),!,do_pending_loads.
with_files(Verb,File):-compound(File),not(is_list(File)),global_pathname(File,FILES),not(File=FILES),!,with_files(Verb,FILES),!.
with_files(Verb,File):-exists_directory_safe(File),!,prolog_must(atomic(File)),aiml_files(File,Files),!,with_files(Verb,Files),!.
with_files(Verb,File):-exists_file_safe(File),!,with_files(Verb,[File]).
with_files(Verb,File):-file_name_extension(File,'aiml',Aiml), exists_file_safe(Aiml),!,with_files(Verb,[File]).
with_files(Verb,File):-expand_file_name(File,FILES),not([File]=FILES),!,with_files(Verb,FILES),!.
with_files(Verb,File):-debugOnFailureAiml(call(Verb,File)).
%%with_files(Verb,File):-throw_safe(error(existence_error(source_sink, File),functor(Verb,F,A),context(F/A, 'No such file or directory'))).

aiml_files(File,Files):-atom(File),sub_atom(File,_Before,_Len,_After,'*'),!,expand_file_name(File,Files),!.
aiml_files(File,Files):-atom_concat_safe(WithOutSlashes,'/',File),!,aiml_files(WithOutSlashes,Files).
aiml_files(File,Files):-exists_directory_safe(File), %absolute_file_name(File,_FileDir),
      atom_concat_safe(File,'/*.aiml',Mask),aiml_files(Mask,Files),!.


aimlOption(rebuild_Aiml_Files,false).

load_single_aiml_file(Ctx,F0):-global_pathname(F0,File),F0\==File,!,load_single_aiml_file(Ctx,File).

load_single_aiml_file(Ctx,F0):-
  debugOnFailureAimlEach((
   global_pathname(F0,File),
   cateForFile(Ctx,File,FileMatch),!,
   atom_concat_safe(File,'.pl',PLNAME),
   load_single_aiml_file(Ctx,File,PLNAME,FileMatch),!)).

%%load_single_aiml_file(_Ctx,File,PLNAME,_FileMatch):- loaded_aiml_file(File,PLNAME,_Time),!.
load_single_aiml_file(Ctx,File,PLNAME,FileMatch):-
   translate_single_aiml_file(Ctx,File,PLNAME,FileMatch),!,
   assertz(pending_aiml_file(File,PLNAME)),!.



translate_single_aiml_file(Ctx,F0):-
  debugOnFailureAimlEach((
   global_pathname(F0,File),!,
   cateForFile(Ctx,File,FileMatch),!,
   atom_concat_safe(File,'.pl',PLNAME),
   translate_single_aiml_file(Ctx,File,PLNAME,FileMatch))),!.

translate_aiml_structure(X,X).

cateForFile(_Ctx,SRCFILE,aimlCate(_GRAPH,_PRECALL,_TOPIC,_THAT,_INPUT,_PATTERN,_FLAGS,_CALL,_GUARD,_USERDICT,_TEMPLATE,_LINENO,SRCFILE:_-_)):-!.
cateForFile(Ctx,File,FileMatch):- trace,withNamedValue(Ctx,[anonvarsFroCate=true], makeAimlCate(Ctx,[srcfile=File:_-_],FileMatch)),!.

withNamedValue(Ctx,[N=V],Call):-withAttributes(Ctx,[N=V],Call),!.

% =================================================================================
% AIML -> Prolog pretransating
% =================================================================================

:-dynamic(creating_aiml_file/2).
:-dynamic(loaded_aiml_file/3).
:-dynamic(pending_aiml_file/2).

do_pending_loads:-withCurrentContext(do_pending_loads).
do_pending_loads(Ctx):-forall(retract(pending_aiml_file(File,PLNAME)),load_pending_aiml_file(Ctx,File,PLNAME)).

load_pending_aiml_file(Ctx,File,PLNAME):- debugFmt(load_pending_aiml_file(Ctx,File,PLNAME)),
  catch(debugOnFailureAiml(dynamic_load(File,PLNAME)),E,(debugFmt(E),assert(pending_aiml_file(File,PLNAME)))),!.

translate_single_aiml_file(_Ctx,File,PLNAME,FileMatch):- creating_aiml_file(File,PLNAME),!,
   throw_safe(already(creating_aiml_file(File,PLNAME),FileMatch)),!.

translate_single_aiml_file(_Ctx,File,PLNAME,FileMatch):- loaded_aiml_file(File,PLNAME,Time),!, throw_safe(already(loaded_aiml_file(File,PLNAME,Time),FileMatch)).

translate_single_aiml_file(_Ctx,File,PLNAME,_FileMatch):-  fail, %% fail if want to always remake file
   exists_file_safe(PLNAME),
   time_file_safe(PLNAME,PLTime), % fails on non-existent
   time_file_safe(File,FTime),
   %not(aimlOption(rebuild_Aiml_Files,true)),
   PLTime > FTime,!,
   debugFmt(up_to_date(create_aiml_file(File,PLNAME))),!,
   retractall(creating_aiml_file(File,PLNAME)).

translate_single_aiml_file(Ctx,File,PLNAME,FileMatch):-
  call_cleanup(
     translate_single_aiml_file0(Ctx,File,PLNAME,FileMatch),
     translate_single_aiml_file1(File,PLNAME,FileMatch)).

translate_single_aiml_file0(Ctx,File,PLNAME,FileMatch):-
 debugOnFailureAimlEach((
        asserta(creating_aiml_file(File,PLNAME)),
        debugFmt(doing(create_aiml_file(File,PLNAME))),
        aimlCateSig(CateSig),!,
   (format('%-----------------------------------------~n')),
        printPredCount('Cleaning',FileMatch,_CP),
   (format('%-----------------------------------------~n')),
        unify_listing(FileMatch),
        retractall(FileMatch),
   (format('%-----------------------------------------~n')),
 tell(PLNAME),
        flag(cateSigCount,PREV_cateSigCount,0),
   (format('%-----------------------------------------~n')),
      withAttributes(Ctx,[withCategory=[translate_cate]], %% asserta_cate = load it as well .. but interferes with timesrtamp
               ( fileToLineInfoElements(Ctx,File,AILSTRUCTURES),
                  load_aiml_structure(Ctx,AILSTRUCTURES))),

   (format('%-----------------------------------------~n')),
         unify_listing_header(FileMatch),
         %printAll(FileMatch),
   (format('%-----------------------------------------~n')),
        listing(xmlns),
   (format('%-----------------------------------------~n')),
        told,
        flag(cateSigCount,NEW_cateSigCount,PREV_cateSigCount),
        printPredCount('Errant Lines',lineInfoElement(File,_,_,_),_EL),
        printPredCount('Total Categories',CateSig,_TC),!,
        debugFmt('NEW_cateSigCount=~q~n',[NEW_cateSigCount]),!,
        statistics(global,Mem),MU is (Mem / 1024 / 1024),
        debugFmt(statistics(global,MU)),!,
        printPredCount('Loaded',FileMatch, _FM),
        retractall(creating_aiml_file(File,PLNAME)))),!.


translate_single_aiml_file1(File,PLNAME,FileMatch):-
    ignore((telling(PLNAME),told(PLNAME))),
    ignore((creating_aiml_file(File,PLNAME),delete_file(PLNAME))),
    retractall(lineInfoElement(File,_,_,_)),
    retractall(FileMatch),
    retractall(xmlns(_,_,_)),
    retractall(creating_aiml_file(File,PLNAME)),!,
    retractall(loaded_aiml_file(File,PLNAME,_Time)).


translate_cate(Ctx,CateSig):-replaceArgsVar(Ctx,[srcinfo],CateSig,_),immediateCall(Ctx,assert_cate_in_load(CateSig)).
asserta_cate(Ctx,CateSig):-prolog_must(ground(CateSig)),assert_cate_in_load(CateSig),immediateCall(Ctx,assert_cate_in_load(CateSig)).

is_xml_missing(Var):-var(Var),!.
is_xml_missing([]).

isRetraction(Ctx,CateSig,OF):-getCategoryArg(Ctx,'template',NULL,_Out_,CateSig),is_xml_missing(NULL),!,
   replaceArgsVar(Ctx,['template',srcinfo,srcfile],CateSig,_),
   OF=CateSig.

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

makeArgIndexes(CateSig):-functor(CateSig,F,_),makeArgIndexes(CateSig,F),!.
makeArgIndexes(CateSig,F):- argNumsTracked(F,Atom,Number),arg(Number,CateSig,Arg),nonvar(Arg),
         %%Number<10,nonvar(Arg),atom_number(Atom,Number),
         assert_if_new(argNFound(F,Atom,Arg)),fail.
makeArgIndexes(_NEW,_F).


assert_if_new(N):-N,!.
assert_if_new(N):-assert(N),!.

/*
translate_single_aiml_filexxx(Ctx,File,PLNAME):-
  debugOnFailureAiml((
     Dofile = true,
     aimlCateSig(CateSig),
   ifThen(Dofile,tell(PLNAME)),
   (format(user_error,'%~w~n',[File])),
   load_structure(File,X,[dialect(xml),space(remove)]),!,
   ATTRIBS = [srcfile=File],!,
   pushAttributes(Ctx,filelevel,ATTRIBS),
   load_aiml_structure_list(Ctx,X),!,
   popAttributes(Ctx,filelevel,ATTRIBS),!,
   ifThen(Dofile,((listing(CateSig),retractall(CateSig)))),
   ifThen(Dofile,(told /*,[PLNAME]*/ )))),!.
*/

%% sgml_parser_defs(PARSER_DEFAULTS,PARSER_CALLBACKS) /*shorttag(false),*/
sgml_parser_defs(
  [defaults(false), space(remove),number(integer), qualify_attributes(false),
         %call(decl, on_decl),
         %call(pi, on_pi),call(xmlns, on_xmlns),call(urlns, xmlns),call(error,xml_error),
         dialect(xml)
         ],
         [max_errors(0),call(begin, on_begin),call(end, on_end)]).



% gather line numbers
fileToLineInfoElements(Ctx,F0,XMLSTRUCTURES):-
   global_pathname(F0,File),
 debugOnFailureAiml((
       retractall(lineInfoElement(File,_,_,_)),
        open(File, read, In, [type(binary)]),
        new_sgml_parser(Parser, []),
        sgml_parser_defs(PARSER_DEFAULTS,PARSER_CALLBACKS),
        maplist_safe(set_sgml_parser(Parser),[file(File)|PARSER_DEFAULTS]),
        %% todo offset(Offset)
        sgml_parse(Parser,[source(In)|PARSER_CALLBACKS]),
        close(In),!,
        fileToLineInfoElements2(Ctx,File,XMLSTRUCTURES))).

% gather line contents
fileToLineInfoElements2(Ctx,File,XMLSTRUCTURES):-!,
  sgml_parser_defs(PARSER_DEFAULTS,_PARSER_CALLBACKS),
  load_structure(File,Whole, [file(File)|PARSER_DEFAULTS]),!,
   load_inner_aiml_w_lineno(File,[],[],[],Ctx,Whole,XMLSTRUCTURES),!.

load_inner_aiml_w_lineno(_SrcFile,_OuterTag,_Parent,_Attributes,_Ctx,Atom,Atom):-(atomic(Atom);var(Atom)),!.
load_inner_aiml_w_lineno(SrcFile,OuterTag,Parent,Attributes,Ctx,[H|T],LL):-!,
      maplist_safe(load_inner_aiml_w_lineno(SrcFile,OuterTag,Parent,Attributes,Ctx),[H|T],LL),!.

%% offset
load_inner_aiml_w_lineno(SrcFile,[OuterTag|PREV],Parent,Attributes,Ctx,element(Tag,Attribs,ContentIn),element(Tag,NewAttribs,ContentOut)):-
   Context=[Tag,OuterTag|_],
   MATCH = lineInfoElement(SrcFile,Line:Offset, Context, element(Tag, Attribs, no_content_yet)),
   MATCH,!,
   ignore(Line = nonfile),
   ignore(Offset = nonfile),
   appendAttributes(Ctx,Attributes,Attribs,RightAttribs),
   appendAttributes(Ctx,[srcfile=SrcFile:Line-Offset,srcinfo=element(Tag,Attribs,ContentIn)],RightAttribs,NewAttribs),
   ignore(retract(MATCH)),
   (member(Tag,[aiml,topic]) ->  NextAttribs = NewAttribs ; NextAttribs = []),
   maplist_safe(load_inner_aiml_w_lineno(SrcFile,[Tag,OuterTag|PREV],Parent,NextAttribs,Ctx),ContentIn,ContentOut),!.

load_inner_aiml_w_lineno(SrcFile,MORE,Parent,Attributes,Ctx,element(Tag,Attribs,ContentIn),element(Tag,RightAttribs,ContentOut)):-
   appendAttributes(Ctx,Attributes,Attribs,RightAttribs),
   load_inner_aiml_w_lineno(SrcFile,[Tag|MORE],Parent,[],Ctx,ContentIn,ContentOut),!.

load_inner_aiml_w_lineno(SrcFile,OuterTag,Parent,Attributes,_Ctx,L,L):-
   aiml_error(load_inner_aiml_w_lineno(SrcFile,OuterTag,Parent,Attributes,L)).


addAttribsToXML(Attribs,element(Tag,Pre,Content),element(Tag,Post,Content)):-appendAttributes(_Ctx,Pre,Attribs,Post),!.
addAttribsToXML(Attribs,[H|T],OUT):-maplist_safe(addAttribsToXML(Attribs),[H|T],OUT),!.
addAttribsToXML(Attribs,OUT,OUT):-!,debugFmt(addAttribsToXML(Attribs,OUT,OUT)),!.


:-dynamic(in_aiml_tag/1).
:-dynamic(inLineNum).

skipOver(_).

on_end('aiml', _) :- !,
        ignore(retract(in_aiml_tag(_))).

on_begin('aiml', Attribs, _) :- !,
        asserta(in_aiml_tag(Attribs)).


on_begin(Tag, Attr, Parser) :- skipOver(not(inLineNum)),
        get_sgml_parser(Parser,context(Context)), Context=[Tag,aiml|_],
        skipOver(debugFmt(on_begin(Tag, Attr, Context))),
        skipOver(retract(in_aiml_tag(AimlAttr))),
       % skipOver(sgml_parser_defs(PARSER_DEFAULTS, PARSER_CALLBACKS)),
        get_sgml_parser(Parser,line(Line)),
        get_sgml_parser(Parser,charpos(Offset)),
        get_sgml_parser(Parser,file(File)),
        global_pathname(File,Pathname),
      %  get_sgml_parser(Parser,source(Stream)),
        skipOver(asserta(inLineNum)),
%        load_structure(Stream,Content,[line(Line)|PARSER_DEFAULTS]),!,
 %      skipOver( sgml_parse(Parser,[ document(Content),parse(input)])),
        NEW = lineInfoElement(Pathname,Line:Offset, Context, element(Tag, Attr, no_content_yet)),
        %%debugFmt(NEW),
        skipOver(ignore(retract(inLineNum))),
        skipOver(asserta(in_aiml_tag(AimlAttr))),
        assertz(NEW),!.

on_begin(_Tag, _Attr, _Parser) :-!. %%get_sgml_parser(Parser,context(Context)),!. %%,debugFmt(on_begin_Context(Tag, Attr, Context)).

%%on_begin_ctx(TAG, URL, Parser, Context) :-!, debugFmt(on_begin_ctx(URL, TAG, Parser,Context)),!.
on_begin_ctx(_TAG, _URL, _Parser, _Context) :- !. %%, debugFmt(on_begin_ctx(URL, TAG, Parser,Context)),!.



:- dynamic
        xmlns/3.

on_xmlns(rdf, URL, _Parser) :- !,debugFmt(on_xmlns(URL, rdf)),asserta(xmlns(URL, rdf, _)).
on_xmlns(TAG, URL, _Parser) :- sub_atom(URL, _, _, _, 'rdf-syntax'), !,
        debugFmt('rdf-syntax'(URL, TAG)),
        immediateCall(_Ctx,asserta(xmlns(URL, rdf, _))).
on_xmlns(TAG, URL, _Parser) :- debugFmt(on_xmlns(URL, TAG)).

on_decl(URL, _Parser) :- debugFmt(on_decl(URL)).
on_pi(URL, _Parser) :- debugFmt(on_pi(URL)).


xml_error(TAG, URL, Parser) :- !, debugFmt(xml_error(URL, TAG, Parser)).
% ============================================
% Loading content
% ============================================

load_aiml_structure_lineno(Attributes,Ctx,L):-maplist_safe(load_inner_aiml_lineno(Attributes,Ctx),L),!.

%% offset
load_inner_aiml_lineno(Attributes,Ctx,element(Tag,Attribs,ContentIn)):-
   appendAttributes(Ctx,Attributes,Attribs,RightAttribs),
   debugOnFailureAiml(attributeValue(Ctx,RightAttribs,[srcfile,srcdir],File,'$error')),
   MATCH = lineInfoElement(File,Line:Offset, Context, element(Tag, Attribs, no_content_yet)),
   ignore(MATCH),
   Context=[_Tag0,aiml|_More],
   ignore(Line = nonfile),
   ignore(Offset = nonfile),
   NewAttribs  = [srcfile=File,lineno=Line:Offset|RightAttribs],
   ignore(retract(MATCH)),
   load_aiml_structure(Ctx,element(Tag,NewAttribs,ContentIn)),!.

   /*

   load_inner_aiml_lineno(Attributes,Ctx,element(Tag,Attribs,ContentIn)):-
   debugOnFailureAiml(current_value(Ctx,srcfile,File)),
   retract((lineInfoElement(File0,Line0:Offset0,graph, element(_Tag0, _Attr0, _Content0)))),
   debugOnFailureAiml(call(OLD)),

   MATCH = lineInfoElement(File,Line:Offset,Context, element(Tag, Attribs, _ContentIn)),!,
   debugOnFailureAiml((call(MATCH),!,not(not((Line:Offset)==(Line0:Offset0))),retract(OLD),
   load_aiml_structure(Ctx,element(Tag,[srcinfo=File0:Line0-Offset0|Attribs],ContentIn)),
        NEW = lineInfoElement(File,Line:Offset,Attributes, element(Tag, Attribs, ContentIn)),
        assertz(NEW))),!.

   */

%catagory
load_aiml_structure(Ctx,element(catagory,ALIST,LIST)):-load_aiml_structure(Ctx,element(category,ALIST,LIST)),!.


% aiml
load_aiml_structure(Ctx,element(aiml,ALIST,LIST)):-
    replaceAttribute(Ctx,name,graph,ALIST,ATTRIBS),!,
 defaultCatePredicatesS(Defaults),
  withAttributes(Ctx,Defaults,
        %withAttributes(Ctx,ATTRIBS,load_aiml_structure_lineno(ATTRIBS,Ctx,LIST)),!.
     withAttributes(Ctx,ATTRIBS,maplist_safe(load_aiml_structure(Ctx),LIST))),!.



% \n\n\n
load_aiml_structure(Ctx,O):-atomic(O),!,debugFmt(load_aiml_structure(Ctx,O)),!.


% topic/category/flags/that
load_aiml_structure(Ctx,element(Tag,ALIST,INNER_XML)):- member(Tag,[topic,category,flags,that]),!,
     replaceAttribute(Ctx,name,Tag,ALIST,ATTRIBS),
         withAttributes(Ctx,ATTRIBS, pushCateElement(Ctx,ATTRIBS,element(Tag,ALIST,INNER_XML))),!.

% substitute,learn,aiml,genlMt,srai,think,system,javascript,eval,template
load_aiml_structure(Ctx,element(A,B,C)):-
   convert_name(A,Tag),tagType(Tag,immediate),
   convert_attributes(Ctx,B,ALIST),
   convert_template(Ctx,C,LIST),
   replaceAttribute(Ctx,name,Tag,ALIST,ATTRIBS),
      withAttributes(Ctx,
        ATTRIBS,
          catch(aiml_call(Ctx,element(Tag,ALIST,LIST)),E,debugFmt(aiml_throw(element(Tag,ATTRIBS,LIST)=E)))),!.

/*

% error of pattern
load_aiml_structure(Ctx,element(Tag,ALIST,INNER_XML)):- cateMember(Tag), aiml_error(element(Tag,ALIST,INNER_XML)),
     replaceAttribute(Ctx,name,Tag,ALIST,ATTRIBS),
         withAttributes(Ctx,ATTRIBS, pushCateElement(Ctx,ATTRIBS,element(Tag,ALIST,INNER_XML))),!.

*/

load_aiml_structure(_Ctx,element(Tag,ALIST,LIST)):- member(Tag,[meta]),!,debugFmt(ignoring(element(Tag,ALIST,LIST))),!.

% special dictionaries
load_aiml_structure(Ctx,element(Tag,ALIST,LIST)):- %% member(Tag,[predicates,vars,properties,predicate,property,var,item]),
   notrace(load_dict_structure(Ctx,element(Tag,ALIST,LIST))),!.

/*
% ============================================
% Rewrite or Error loading
% ============================================

hide_load_aiml_structure(Ctx,element(Tag,ALIST,PATTERN)):-
     convert_element(Ctx,element(Tag,ALIST,PATTERN),NEW),
     load_aiml_structure_diff(Ctx,element(Tag,ALIST,PATTERN),NEW),!.


load_aiml_structure_diff(Ctx,BEFORE,AFTER):- BEFORE\==AFTER, load_aiml_structure(Ctx,AFTER),!.
%%load_aiml_structure_diff(Ctx,BEFORE,AFTER):- aiml_error(load_aiml_structure(Ctx,BEFORE)),!.

*/

% <aiml>
load_aiml_structure(Ctx,[A|B]):-!,debugOnFailureAiml(maplist_safe(load_aiml_structure(Ctx),[A|B])),!.

load_aiml_structure(Ctx,X):- aiml_error(missing_load_aiml_structure(Ctx,X)).


% ============================================
% special dictionaries
% ============================================
dictionaryTags(Tag):-dictionaryOuterTags(Tag);dictionaryItemTags(Tag);dictionaryTypeTags(Tag,_).

dictionaryOuterTags(Tag):- member(Tag,[predicates,vars,properties,bots,bot]).
dictionaryItemTags(Tag):- member(Tag,[predicate,property,var,item,set,entry]).

% Bot properties are predicates that cannot be changed during the runtime life of the bot,
% but which can be included in AIML patterns for matching.
dictionaryTypeTags(Tag,[bot,default]):-member(Tag,[properties]).
dictionaryTypeTags(Tag,[bot]):-member(Tag,[bots,bot,entry]).
% Default predicates can be thought of as your bot's "assumptions" about new users.
dictionaryTypeTags(Tag,default):-member(Tag,[predicate,predicates]).
dictionaryTypeTags(Tag,user):-member(Tag,[var,vars,set]).

obtainDictionaryName(Ctx,_Tag,ALIST,Dict):- dictVarName(N), peekNameValue(Ctx,ALIST,N,Dict,'$failure'),!.
obtainDictionaryName(_Ctx,Tag,_ALIST,Dict):- dictionaryTypeTags(Tag,Dict),!.
obtainDictionaryName(Ctx,_Tag,ALIST,Dict):- peekNameValue(Ctx,ALIST,[dictionary,name],Dict,'$error'),!.

% user/bot dictionaries (outers-only)
load_dict_structure(Ctx,element(Tag,ALIST,LIST)):-
   member(Tag,[predicates,vars,properties]),
   replaceAttribute(Ctx,name,dictionary,ALIST,ATTRIBS),
   obtainDictionaryName(Ctx,Tag,ATTRIBS,Dict),  
   withAttributes(Ctx,[dictionary=Dict|ATTRIBS],
    debugOnFailureAiml((
     current_value(Ctx,dictionary,_Dict),
      maplist_safe(load_dict_structure(Ctx),LIST)))).

% user/bot predicatates (inners-only)
load_dict_structure(Ctx,element(Tag,ALIST,LIST)):-member(Tag,[predicate]),
   current_value(Ctx,dictionary,Dict),
     attributeValue(Ctx,ALIST,[name,var],Name,'$error'),
     attributeValue(Ctx,ALIST,[default],Default,''),
     attributeValue(Ctx,ALIST,[value,default],Value,LIST),
     attributeValue(Ctx,ALIST,['set-return'],SetReturn,value),
  debugOnFailureAiml((
     load_dict_structure(Ctx,dict(Dict,Name,Value)),
     load_dict_structure(Ctx,dict(defaultValue(Dict),Name,Default)),
     load_dict_structure(Ctx,dict(setReturn(Dict),Name,SetReturn)))),!.

% user/bot dictionaries name/values
load_dict_structure(Ctx,element(Tag,ALIST,LIST)):-member(Tag,[property,var,item,set]),
   current_value(Ctx,dictionary,Dict),
   debugOnFailureAiml((
     attributeValue(Ctx,ALIST,[name,var],Name,'$error'),
     attributeValue(Ctx,ALIST,[value,default],Value,LIST),
     load_dict_structure(Ctx,dict(Dict,Name,Value)))),!.



% special substitution dictionaries
load_dict_structure(Ctx,element(substitutions,ALIST,LIST)):-
   debugOnFailureAiml((
      replaceAttribute(Ctx,name,graph,[dictionary=substitutions(input)|ALIST],ATTRIBS),
     withAttributes(Ctx,ATTRIBS,
     maplist_safe(load_substs(Ctx),LIST)))).


load_substs(Ctx,element(Tag,ALIST,LIST)):- substitutionDictsName(Tag,Dict),
   debugOnFailureAiml((
      replaceAttribute(Ctx,name,graph,[dictionary=substitutions(Dict)|ALIST],ATTRIBS),
     withAttributes(Ctx,ATTRIBS,
     maplist_safe(load_substs(Ctx),LIST)))).

load_substs(Ctx,element(Tag,ATTRIBS,LIST)):-member(Tag,[substitution,substitute]),!,
   debugOnFailureAiml((
      peekNameValue(Ctx,filelevel,dictionary,substitutions(Catalog),'$error'),
      attributeOrTagValue(Ctx,element(substitute,ATTRIBS,LIST),[old,find,name,before],Find,'$error'),
      attributeOrTagValue(Ctx,element(substitute,ATTRIBS,LIST),[new,replace,value,after],Replace,'$error'),
      debugOnFailureAiml(load_dict_structure(Ctx,dict(substitutions(Catalog),Find,Replace))))),!.

% substitutions
load_dict_structure(Ctx,element(substitute,ATTRIBS,LIST)):- load_substs(Ctx,element(substitute,ATTRIBS,LIST)),!.
load_dict_structure(Ctx,element(substitution,ATTRIBS,LIST)):- load_substs(Ctx,element(substitute,ATTRIBS,LIST)),!.


% detect substitutions
load_dict_structure(Ctx,dict(substitutions(Dict),Find,Replace)):-!,
   debugOnFailureAiml(load_dict_structure(Ctx,substitute(Dict,Find,Replace))),!.

load_dict_structure(Ctx,substitute(SubstsNameI,Find,Replace)):-!,
  debugOnFailureAiml((
      convert_dictname(Ctx,SubstsNameI,SubstsName),
      convert_substs(Find,FindM),
      %%%convert_text
      convert_replacement(Ctx,Replace,ReplaceM),
      addReplacement(Ctx,SubstsName,FindM,ReplaceM))),!.

% actual assertions
load_dict_structure(Ctx,dict(IDict,Name,Value)):-
     %%%debugFmt(dict(Dict,Name,Value)),
      convert_dictname(Ctx,IDict,Dict),
      setAliceMem(Ctx,Dict,Name,Value),!.

convert_dictname(Ctx,A,D):-!,unresultifyL(Ctx,A,D),!.
convert_dictname(Ctx,A,D):-convert_dictname0(Ctx,A,D),nop(traceIf((A\==D,A\==[D]))).

convert_dictname0(_Ctx,A,A):-var(A),!.
convert_dictname0(Ctx,A,D):-unresultifyL(Ctx,A,AD),A\==AD,!,convert_dictname0(Ctx,AD,D).
convert_dictname0(Ctx,[A],D):-nonvar(A),!,convert_dictname0(Ctx,A,D).
convert_dictname0(_Ctx,A,D):-atom(A),!,convert_name(A,D).
convert_dictname0(_Ctx,A,D):-compound(A),functor(A,F,1),A=..[F,AA],convert_name(AA,DD),D=..[F,DD],!.
convert_dictname0(_Ctx,A,A).


:-dynamic(replace_t/5).
:-dynamic(response_t/5).

convert_replacement(Ctx,Replace,ReplaceMM):-convert_template(Ctx,Replace,ReplaceM),listify(ReplaceM,ReplaceMM),!.

% ===============================================================================================
%  UTILS
% ===============================================================================================

ignore_aiml(VAR):-var(VAR),!,aiml_error(VAR).
ignore_aiml([]):-!.
ignore_aiml(''):-!.
ignore_aiml(A):-atom(A),!,atom_codes(A,C),!,clean_codes(C,D),!,D=[].
ignore_aiml([A|B]):-ignore_aiml(A),!,ignore_aiml(B),!.

/* commenting since proably not used

aiml_classify([],[]).
aiml_classify(Find,[atom]):-atomic(Find).
aiml_classify([H|INNER_XML],Out):-
      classifySingle(H,Class),
      aiml_classify(INNER_XML,More),
      sort([Class|More],OutM),!,
      classify2(OutM,Out).
aiml_classify(_T,[unk]).

classify2([in,out|Resp],[out|Resp]).
classify2(Out,Out).

classifySingle('_',var('_')).
classifySingle(*,var('*')).
classifySingle(Atom,in):-is_litteral(Atom).
classifySingle(Atom,out):-atom(Atom).
classifySingle(Atom,spec(File)):-compound(Atom),functor(Atom,File,_).
classifySingle(_Atom,unknown).

*/

varize(Find,Replace,FindO,ReplaceO):-
      subst((Find,Replace),'_','$VAR'(0),(FindM,ReplaceM)),
      subst((FindM,ReplaceM),'*','$VAR'(0),(FindO,ReplaceO)),!.


aiml_error(E):-trace,  randomVars(E),debugFmt('~q~n',[error(E)]),trace,randomVars(E),!,throw(E).


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

% ===============================================================================================
%  Load Categories
% ===============================================================================================

innerTagLikeThat(That):-hotrace(innerTagLike(That,prepattern)).

innerTagLike(That,Like):-hotrace((innerTagPriority(That,Atts),memberchk(Like,Atts))).

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


infoTagLikeLineNumber(X):-member(X,[lineno,srcdir,srcfile,srcinfo]).

isPatternTag(Tag):-member(Tag,[that,pattern,request,response,topic,flags,guard]).

isOutputTag(Tag):-member(Tag,[template,call]).
isOutputTag(Tag):-innerTagLike(Tag,postpattern).

each_category(_Ctx,_ATTRIBS,_TAGS,element(MUST_CAT,_ALIST,_NOCATEGORIES)):- not(MUST_CAT = category),throw_safe(each_category(MUST_CAT )).

% category tag contains pre-<pattern> which must be proccessed pre-template just like <that>
each_category(Ctx,ATTRIBS,TAGS,element(TAG,ALIST,NOCATEGORIES)):- innerTagLikeThat(That), member(element(That,WA,WP), NOCATEGORIES),!,
   takeout(element(That,WA,WP),NOCATEGORIES,NOPATTERNS),
   each_category(Ctx,ATTRIBS,[element(That,WA,WP)|TAGS],element(TAG,ALIST,NOPATTERNS)),!.

each_category(Ctx,ATTRIBS,NOPATTERNS,element(TAG,ALIST,PATTERN)):-
  debugOnFailureAiml((
   replaceAttribute(Ctx,name,TAG,ALIST,PATTRIBS),
   appendAttributes(Ctx,PATTRIBS,ATTRIBS,NEWATTRIBS),
   gatherEach(Ctx,[TAG=PATTERN|NEWATTRIBS],NOPATTERNS,Results),!,
   prolog_must(dumpListHere(Ctx,Results)))),!.



%catagory
pushCateElement(Ctx,ATTRIBS,element(catagory, A, B)):- !,pushCateElement(Ctx,ATTRIBS,element(category, A, B)),!.

% <topic> has non<category>s
pushCateElement(Ctx,INATTRIBS,element(Tag,ATTRIBS,INNER_XML)):- member(Tag,[topic,flag]),member(element(INNER,_,_),INNER_XML),INNER \= category,!,
 debugOnFailureAiml((
   unify_partition(element(category,_,_),INNER_XML,ALLCATEGORIES,NONCATE),
   %findall(element(category,ALIST,LIST),member(element(category,ALIST,LIST),INNER_XML),ALLCATEGORIES),
   %takeout(element(category,_,_),INNER_XML,NONCATE),
   appendAttributes(Ctx,ATTRIBS,INATTRIBS,OUTATTRIBS),
   maplist_safe(each_category(Ctx,OUTATTRIBS,NONCATE),ALLCATEGORIES))).

% flag/topic
pushCateElement(Ctx,INATTRIBS,element(Tag,ALIST,INNER_XML)):- member(Tag,[topic,flag]),!,
  debugOnFailureAiml((
  replaceAttribute(Ctx,name,Tag,ALIST,ATTRIBS),
  appendAttributes(Ctx,ATTRIBS,INATTRIBS,OUTATTRIBS),
  withAttributes(Ctx,OUTATTRIBS,
     maplist_safe(pushCateElement(Ctx,OUTATTRIBS),INNER_XML)))).

% remove <patterns>s from <category>s
pushCateElement(Ctx,INATTRIBS,element(Tag,ATTRIBS,INNER_XML)):- member(Tag,[outerctx,category]),!,
 debugOnFailureAiml((
   member(element(pattern,_,_),INNER_XML),
   unify_partition(element(pattern,_,_),INNER_XML,ALLPATTERNS,NOPATTERNS),
   %findall(element(pattern,ALIST,LIST),member(element(pattern,ALIST,LIST),INNER_XML),ALLPATTERNS),
   %takeout(element(pattern,_,_),INNER_XML,NOPATTERNS),
   appendAttributes(Ctx,ATTRIBS,INATTRIBS,OUTATTRIBS),
   maplist_safe(each_pattern(Ctx,OUTATTRIBS,NOPATTERNS),ALLPATTERNS))),!.

% error
pushCateElement(Ctx,ATTRIBS,M):-debugFmt('FAILURE'(pushCateElement(Ctx,ATTRIBS,M))),trace.

unify_partition(Mask, List, Included, Excluded):- partition(\=(Mask), List, Excluded , Included),!.
%%unify_partition(Mask, +List, ?Included, ?Excluded)

each_pattern(Ctx,ATTRIBS,TAGS,element(TAG,ALIST,PATTERN)):- innerTagLikeThat(That), member(element(That,WA,WP), PATTERN),!,
   debugOnFailureAiml((
   takeout(element(That,WA,WP),PATTERN,NOPATTERNS),
   each_pattern(Ctx,ATTRIBS,[element(That,WA,WP)|TAGS],element(TAG,ALIST,NOPATTERNS)))),!.

each_pattern(Ctx,ATTRIBS,NOPATTERNS,element(TAG,ALIST,PATTERNA)):-
  debugOnFailureAiml((
   convert_text(PATTERNA,PATTERN),
   replaceAttribute(Ctx,name,TAG,ALIST,PATTRIBS),
   appendAttributes(Ctx,PATTRIBS,ATTRIBS,NEWATTRIBS),
   gatherEach(Ctx,[TAG=PATTERN|NEWATTRIBS],NOPATTERNS,Results),
   prolog_must(dumpListHere(Ctx,Results)))),!.

dumpListHere(Ctx,DumpListHere):-
   debugOnFailureAiml((
    %%debugFmt(DumpListHere),
    current_value(Ctx,withCategory,Verbs),
    assertCate(Ctx,DumpListHere,Verbs))).

%%dumpListHere([]):-debugFmt(dumpListHere).
%%dumpListHere([R|Results]):-debugFmt(R),dumpListHere(Results),!.

gatherEach(Ctx,NEWATTRIBS,NOPATTERNS,RESULTS):-
   gatherEach0(Ctx,NEWATTRIBS,NOPATTERNS,RESULTS),
   debugFmt(gatherEach0(Ctx,NEWATTRIBS,NOPATTERNS,RESULTS)),!.


removeAlwaysFromTag(that,pattern).

gatherEach0(_Ctx,NEWATTRIBS,[],NEWATTRIBS):-!.

gatherEach0(Ctx,NEWATTRIBS,[element(TAG,ALIST,PATTERN)|NOPATTERNS],RESULTS):-  
   removeAlwaysFromTag(That,TAG),
      innerTagLikeThat(That), member(element(That,WA,WP), PATTERN),!,
      takeout(element(That,WA,WP),PATTERN,NOTHAT),!,
      prolog_must(removeAlwaysFromTag(TAG,That)),
      gatherEach0(Ctx,NEWATTRIBS,[element(That,WA,WP),element(TAG,ALIST,NOTHAT)|NOPATTERNS],RESULTS),!.

gatherEach0(Ctx,NEWATTRIBS,[element(TAG,ALIST,PATTERN_IN)|NOPATTERNS],[TAG=PATTERN_OUT|Result]):-
      transformTagData(Ctx,TAG,'$current_value',PATTERN_IN,PATTERN_OUT),!,
      gatherEach0(Ctx,NEWATTRIBS,NOPATTERNS,ResultM),!,
      appendAttributes(Ctx,ALIST,ResultM,Result),!.


each_template(Ctx,M):-debugFmt('FAILURE'(each_template(Ctx,M))),trace.
each_that(Ctx,M):-debugFmt('FAILURE'(each_that(Ctx,M))),trace.


% ===============================================================================================
% ===============================================================================================
%%:-abolish(dict/3).

:-retractall(dict(_,_,_)).

%:- cateFallback(ATTRIBS), pushAttributes(_Ctx,default,ATTRIBS).
%:- cateFallback(ATTRIBS), popAttributes(_Ctx,default,ATTRIBS).
%:- cateFallback(ATTRIBS), pushAttributes(_Ctx,default,ATTRIBS).

:-pp_listing(dict(_,_,_)).


save:-tell(aimlCate),
   aimlCateSig(CateSig),
   listing(CateSig),
   listing(dict),
   told,
   predicate_property(CateSig,number_of_clauses(N)),
   predicate_property(dict(_,_,_),number_of_clauses(ND)),
   debugFmt([aimlCate=N,dict=ND]),!.

dt:- withAttributes(Ctx,[graph='ChomskyAIML'],load_aiml_files(Ctx,'aiml/chomskyAIML/*.aiml')).

do:-load_aiml_files,alicebot.

