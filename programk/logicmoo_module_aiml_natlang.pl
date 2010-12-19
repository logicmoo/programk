% ===================================================================
% File 'logicmoo_module_aiml_main.pl'
% Purpose: To load and test the AIML interpretor (sanity checks)
% Maintainers: Douglas Miles/Annie Ogborn/Kino Coursey
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'logicmoo_module_aiml_main.pl' 1.0.0
% Revision:  $Revision: 1.7 $
% Revised At:   $Date: 2002/07/11 21:57:28 $
% ===================================================================



% ===============================================================================================
% Split input into many words
% ===============================================================================================

tokenizeInput(String,Tokens):-notrace(tokenizeInput0(String,Tokens)).
tokenizeInput0(Input,Input):-var(Input),!.
tokenizeInput0([],[]):-!.
tokenizeInput0(Input,Tokens):- atom(Input),error_catch(((atom_to_term(Input,Tokens,Vars),ground(Vars))),_,fail),!.
tokenizeInput0(Input,Tokens):- atom(Input),load_structure_from_string(Input,Tokens0),(Tokens0\==[Input]->tokenizeInput0(Tokens0,Tokens);Tokens0=Tokens),!.
tokenizeInput0(Input,Tokens):- atom(Input),atomWSplit(Input,Tokens0), (Tokens0\==[Input]->tokenizeInput0(Tokens0,Tokens);Tokens0=Tokens),!.
tokenizeInput0([A|B],Out):- tokenizeInput0(A,AA),tokenizeInput0(B,BB),!,append(AA,BB,Out).
tokenizeInput0(A,A).


% ===============================================================================================
% Join input into many words
% ===============================================================================================

atomify(A,A):-atomic(A),!.
atomify([A],A):-atom(A),!.
atomify([A|List],Result):-joinAtoms([A|List],' ',Result).

joinAtoms([],_,'').
joinAtoms(List,Sep,Result):-atomify(Sep,SepA),Sep\==SepA,!,joinAtoms(List,SepA,Result).
joinAtoms(List,Sep,Result):-hotrace(joinAtoms0(List,Sep,Result)),!.

joinAtoms0([],_,'').
joinAtoms0([A],_,AA):-atomify(A,AA).
joinAtoms0([A|More],Sep,Result):-atomify(A,AA),A\==AA,!,joinAtoms0([AA|More],Sep,Result).
joinAtoms0([A|List],Sep,Result):-prolog_must(atomic(A)),joinAtoms1([A|List],Sep,Result).

joinAtoms1([A,'\b',B|List],Sep,Result):-atomify(B,BB),atom_concat(A,BB,C),!,joinAtoms0([C|List],Sep,Result).
joinAtoms1([A,B|List],'',Result):-atom_concat(A,B,C),!,joinAtoms1([C|List],'',Result).
joinAtoms1([A,B|List],Sep,Result):-atom_concat(A,Sep,C),!,joinAtoms0([C,B|List],Sep,Result).
joinAtoms1(List,Sep,Result):- debugOnError(atomic_list_concat(List,Sep,Result)),!.

% ===============================================================================================
% Split input into many sentences
% ===============================================================================================

splitSentences(In,Out):- notrace(splitSentences0(In,Out)). %%,flatten(Out,OutL),traceIf(member(xml,OutL)),!.
splitSentences0([],[]):-!.   
splitSentences0(SR1,[SR0|SRMORE]):-grabFirstSetence(SR1,SR0,LeftOver),!,splitSentences0(LeftOver,SRMORE),!.
splitSentences0(SR1,[SR1]):-!.

splitSentencesOn(Starters,Enders,In,Out):- hotrace(splitSentencesOn0(Starters,Enders,In,Out)).%%%,flatten(Out,OutL),traceIf(member(xml,OutL)),!.
splitSentencesOn0(_Starters,_Enders,[],[]):-!.   
splitSentencesOn0(Starters,Enders,SR1,[SR0|SRMORE]):-grabFirstSetenceOn(Starters,Enders,SR1,SR0,LeftOver),!,splitSentencesOn0(Starters,Enders,LeftOver,SRMORE),!.
splitSentencesOn0(_Starters,_Enders,SR1,[SR1]):-!.

grabFirstSetence(SR1,SRS,LeftOver):-LeftSide=[_|_],append(LeftSide,[EOS|LeftOver],SR1),sentenceBreakChar(EOS),validSentenceEnder([EOS|LeftOver]),append(LeftSide,[EOS],SR0),cleanSentence(SR0,SRS),!.
cleanSentence(SR0,SRSOutput):-prolog_must(leftTrim(SR0,sentenceEnderOrPunct,SRS)),!,rightTrim(SRS,sentenceEnderOrPunct_NoQuestion,SRSOut),trimWhitepaceOffEnds(SRSOut,SRSOutput).

trimWhitepaceOffEnds(SRSOut,SRSOutput):-leftTrim(SRSOut,isWhiteWord,SRS),rightTrim(SRS,isWhiteWord,SRSOutput).

validSentenceEnder(['.',xml|_]):-!,fail.
validSentenceEnder(['.','\b'|_]):-!,fail.
validSentenceEnder(_).

grabFirstSetenceOn(Starters,Enders,SR1,SRS,LeftOver):-LeftSide=[_|_],append(LeftSide,[EOS|LeftOver],SR1),
    ((member(EOS,Enders)->append(LeftSide,[EOS],SR0));
    (member(EOS,Starters)->append(LeftSide,[],SR0));
    fail),cleanSentence(SR0,SRS),!.

% ===============================================================================================
% Convert to Matchable
% ===============================================================================================

convertToMatchableCS(That,Words):-
      answerOutput(That,AA),!,
      deleteAll(AA,['.','!','?','\'','!','','\b',' ','\n',',','\r\n','\n\n'],Words),!.

convertToMatchable(That,LastSaid):-
      convertToMatchableCS(That,Words),!,
      ignorecase_literal(Words,LastSaid),!.



literal_atom_safe(A,B):-atom(A),literal_atom(A,B),!.
literal_atom(A,B):-downcase_atom(A,B),!.
is_literal(X):-atom(X),literal_atom(X,N),!,N=X.

ignorecase_literal(A,B):-literal_atom_safe(A,B),!.
ignorecase_literal(A,B):-toLowercase(A,B),!.

