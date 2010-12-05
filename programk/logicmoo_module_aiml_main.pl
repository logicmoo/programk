:-'trace'(findall/3,[-all]).

% ===================================================================
% File 'logicmoo_module_aiml_main.pl'
% Purpose: To load and test the AIML interpretor (sanity checks)
% Maintainers: Douglas Miles/Annie Ogborn/Kino Coursey
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'logicmoo_module_aiml_main.pl' 1.0.0
% Revision:  $Revision: 1.7 $
% Revised At:   $Date: 2002/07/11 21:57:28 $
% ===================================================================

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


chomskyAIML:-once(load_aiml_files('programk/test_suite/chomskyAIML/*.aiml')).

test_suite_files:-once(load_aiml_files(library('programk/test_suite/*.aiml'))).

run_chat_tests_here:-     
   test_suite_files,
   test_call(alicebot('qt')),
   test_call(alicebot('qt1')),!.

run2:-
   %%test_call(alicebot('Hi')),
   test_call(alicebot('What is your name')),
   test_call(alicebot('What is your thing')),
   test_call(alicebot('My name is Fred.')),
   test_call(alicebot('what is my name?')).


annie:-run_chat_tests_here.

%:-test_suite_files.

:-time(annie).
%:-time(chomskyAIML).
:-alicebot.

%%:-time(load_aiml_files('programk/test_suite/special/*.aiml')).


