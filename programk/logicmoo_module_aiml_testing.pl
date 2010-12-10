% ===================================================================
% File 'logicmoo_module_aiml_main.pl'
% Purpose: To load and test the AIML interpretor (sanity checks)
% Maintainers: Douglas Miles/Annie Ogborn/Kino Coursey
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'logicmoo_module_aiml_main.pl' 1.0.0
% Revision:  $Revision: 1.7 $
% Revised At:   $Date: 2002/07/11 21:57:28 $
% ===================================================================

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


hasLibrarySupport :- absolute_file_name(library('programk/logicmoo_module_aiml.pl'),File),exists_file(File).
throwNoLib:-  absolute_file_name('.',Here),throw(error(existence_error(url, Here), context(_, status(404, Here)))).

% up one if we are in same directory
:-absolute_file_name('logicmoo_module_aiml.pl',File),(exists_file(File)->cd('../');true).

% if not has library suport, add this directory as a library directory
addSupportHere:-
  absolute_file_name('programk/logicmoo_module_aiml.pl',File),
  (exists_file(File)-> ((
  absolute_file_name('.',Here),
  asserta(library_directory(Here))));true).

:-not(hasLibrarySupport)->addSupportHere;true.

:-hasLibrarySupport->true;throwNoLib.

% goal is to remove this next line and have it work!
:-ensure_loaded(library('cyc_pl/cyc.pl')).
%%:-ensure_loaded(library('autolog/autolog.pl')).
:-ensure_loaded(library('programk/logicmoo_module_aiml.pl')).


%:-ensure_loaded('bootstrap.aiml.pl').

% :- tell(listing1),listing,told.

dtt:- time(dt),statistics,alicebot.

dttt:-time(consult(aimlCate_checkpoint)),alicebot.

:-catch(guitracer,_,true).
:-traceAll.
:-list_undefined.

:-debug.

%:-dttt.
%:-do.
%:-load_aiml_files.
%:-debug,run_chat_tests.
%:-main_loop.
:-'trace'(findall/3,[-all]).

:-assert_cate_in_load(aimlCate(*,*,*,*,*,*,*,*,*,*,[element(srai,[],['STDCATCHALL',star(pattern,[],[])])],element(category,[],[element(pattern,[],[*]),element(template,[],[element(srai,[],['STDCATCHALL',element(star,[],[])])])]),'c:/development/opensim4opencog/bin/cynd/programk/test_suite/customtagtest.aiml':737-20056)).
:-assert_cate_in_load(aimlCate(*,*,*,*,*,['STDCATCHALL',*],*,*,*,*,['ERROR',understanding,:,star(pattern,[],[])],element(category,[],[element(pattern,[],['STDCATCHALL *']),element(template,[],['ERROR understanding:',element(star,[],[])])]),'c:/development/opensim4opencog/bin/cynd/programk/test_suite/customtagtest.aiml':44-3205)).


chomskyAIML:-catch(consult(chomskyAIML),_,fail),!.
chomskyAIML:-once(load_aiml_files(library('programk/test_suite/chomskyAIML/*.aiml'))).

test_suite_files:-once(load_aiml_files(library('programk/test_suite/*.aiml'))).

run_chat_tests_here(Ctx):-     
   test_suite_files,
   test_call(alicebot(Ctx,'qt')),
   test_call(alicebot(Ctx,'qt1')),!.

run2(Ctx):-
   %%test_call(alicebot(Ctx,'Hi')),
   test_call(alicebot(Ctx,'What is your name')),
   test_call(alicebot(Ctx,'What is your thing')),
   test_call(alicebot(Ctx,'My name is Fred.')),
   test_call(alicebot(Ctx,'what is my name?')).


annie:-makeAimlContext(toplevel,Ctx),run_chat_tests_here(Ctx),alicebot(Ctx).

%:-test_suite_files.

:-time(annie).
%:-time(chomskyAIML).
:-alicebot.

%%:-time(load_aiml_files('programk/test_suite/special/*.aiml')).


