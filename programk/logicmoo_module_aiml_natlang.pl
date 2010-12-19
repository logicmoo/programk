% ===================================================================
% File 'logicmoo_module_aiml_main.pl'
% Purpose: To load and test the AIML interpretor (sanity checks)
% Maintainers: Douglas Miles/Annie Ogborn/Kino Coursey
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'logicmoo_module_aiml_main.pl' 1.0.0
% Revision:  $Revision: 1.7 $
% Revised At:   $Date: 2002/07/11 21:57:28 $
% ===================================================================

splitSentences(In,Out):- notrace(splitSentences0(In,Out)). %%,flatten(Out,OutL),traceIf(member(xml,OutL)),!.
splitSentences0([],[]):-!.   
splitSentences0(SR1,[SR0|SRMORE]):-grabFirstSetence(SR1,SR0,LeftOver),!,splitSentences0(LeftOver,SRMORE),!.
splitSentences0(SR1,[SR1]):-!.

splitSentencesOn(Starters,Enders,In,Out):- hotrace(splitSentencesOn0(Starters,Enders,In,Out)).%%%,flatten(Out,OutL),traceIf(member(xml,OutL)),!.
splitSentencesOn0(_Starters,_Enders,[],[]):-!.   
splitSentencesOn0(Starters,Enders,SR1,[SR0|SRMORE]):-grabFirstSetenceOn(Starters,Enders,SR1,SR0,LeftOver),!,splitSentencesOn0(Starters,Enders,LeftOver,SRMORE),!.
splitSentencesOn0(_Starters,_Enders,SR1,[SR1]):-!.

grabFirstSetence(SR1,SRS,LeftOver):-LeftSide=[_|_],append(LeftSide,[EOS|LeftOver],SR1),sentenceBreakChar(EOS),validSentenceEnder([EOS|LeftOver]),append(LeftSide,[EOS],SR0),cleanSentence(SR0,SRS),!.
cleanSentence(SR0,SRS):-prolog_must(leftTrim(SR0,sentenceEnderOrPunct,SRS)),!.

validSentenceEnder(['.',xml|_]):-!,fail.
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
      deleteAll(AA,['.','!','?','\'','!','','\n',',','\r\n','\n\n'],Words),!.

convertToMatchable(That,LastSaid):-
      convertToMatchableCS(That,Words),!,
      ignorecase_literal(Words,LastSaid),!.



literal_atom_safe(A,B):-atom(A),literal_atom(A,B),!.
literal_atom(A,B):-downcase_atom(A,B),!.
is_literal(X):-atom(X),literal_atom(X,N),!,N=X.

ignorecase_literal(A,B):-literal_atom_safe(A,B),!.
ignorecase_literal(A,B):-toLowercase(A,B),!.

