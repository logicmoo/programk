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
:-absolute_file_name('logicmoo_module_aiml.pl',File),exists_file(File),cd('../').

% if not has library suport, add this direcotry as a library directory
:-not(hasLibrarySupport),
  absolute_file_name('programk/logicmoo_module_aiml.pl',File),exists_file(File),
  absolute_file_name('.',Here),
  asserta(library_directory(Here)).

:-hasLibrarySupport->true;throwNoLib.

% goal is to remove this next line and have it work!
:-ensure_loaded(library('cyc_pl/cyc.pl')).

:-ensure_loaded(library('programk/logicmoo_module_aiml.pl')).


%:-ensure_loaded('bootstrap.aiml.pl').

% :- tell(listing1),listing,told.

dtt:- time(dt),statistics,alicebot.

dttt:-time(consult(aimlCate_checkpoint)),alicebot.

:-traceAll.
:-guitracer.
:-list_undefined.

%:-dttt.
%:-do.
%:-load_aiml_files.
%:-debug,run_chat_tests.
%:-main_loop.
:-once(load_aiml_files('programk/test_suite/*.aiml')).


run_chat_tests_here:-
   test_call(alicebot('Hi')),
   test_call(alicebot('What is your name')),
   test_call(alicebot('My name is Fred.')),
   test_call(alicebot('what is my name?')),
   test_call(alicebot('qt')),
   test_call(alicebot('qt1')),!.


annie:-run_chat_tests_here.

:-time(annie).

:-alicebot.

%%:-time(load_aiml_files('programk/test_suite/special/*.aiml')).
