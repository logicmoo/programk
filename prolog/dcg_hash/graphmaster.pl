% ===================================================================
% File 'graphmaster.pl'
% Purpose: An Implementation in SWI-Prolog of Graphmaster Index
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'graphmaster.pl' 1.0.0
% Revision: $Revision: 1.7 $
% Revised At: $Date: 2002/07/11 21:57:28 $
% ===================================================================


:- use_module(library(dictoo_lib)).
:- use_module(library(globals_api)).
%:- set_prolog_flag(generate_debug_info, false).
%:- cls.
% :- use_module(library(wam_cl/init)).

:- include(hashmap_oo).

% ===================================================================
% ===================================================================
track_now(Graph):- track_now(Graph, inst).
track_now(Graph, _Type):- hashtable_get(Graph, track_id, _), !.
track_now(Graph, Type):- gensym(Type, I), oo_set(Graph, track_id, I).

%%isStar0(Word1):- member(Word1, [*, '_']).
isStar0(X):-var(X), !, throw(isStar0(X)).
isStar0('*').
isStar0('_').

into_path(List, NList):- notrace((is_list(List), !, maplist(into_path, List, NList))), !.
into_path(List, NList):- atom(List), !, upcase_atom(List, NList).
into_path(List, NList):- compound(List), !, =(List, NList).
into_path(List, NList):- throw(into_path(List, NList)).

sameWords(Word1, Word2):-atom(Word1), atom(Word2), atoms_match0(Word1, Word2).
 atoms_match0(Word1, Word2):- (isStar0(Word1);isStar0(Word2)), !, fail.
 atoms_match0(Word1, Word1):-!.
 atoms_match0(Word1, Word2):-into_path(Word1, WordO), into_path(Word2, WordO), !.

into_name(Graph, Name):- atom(Graph), !, ignore((Graph=Name)).
into_name(Graph, Name):- is_hashtable(Graph), !, ignore((hashtable_get(Graph, name, Name))).

into_named_map(RB, Name, Graph, _ElseCall):- oo_get(RB, Name, Graph), !.
into_named_map(RB, Name, Graph, ElseCall):- hashtable_new(Graph), 
   call(ElseCall,Graph),oo_set(Graph, name, Name), track_now(Graph), oo_set(RB, Name, Graph).


:- nb_current('$graphs', _) -> true ; (hashtable_new( RB), nb_setval('$graphs', RB)).
into_graph(Name):- atom(Name), into_graph(Name, _O).
into_graph(Graph):- into_graph(_, Graph).
into_graph(Name, Graph):-  is_hashtable(Graph), !, ignore((hashtable_get(Graph, name, Name))).
into_graph(Name, Graph):- 
 ignore(Name=graphmaster), 
 into_name(Name, GName), 
 nb_getval('$graphs', RB), 
 into_named_map(RB, GName, Graph, make_graph).

make_graph(Graph):- hashtable_set(Graph,type,graph).

:- nb_current('$states', _) -> true ; (hashtable_new( RB), nb_setval('$states', RB)).
into_state(Name):- atom(Name), into_state(Name, _O).
into_state(State):- into_state(_, State).
into_state(Name, Graph):-  is_hashtable(Graph), !, ignore((hashtable_get(Graph, name, Name))).
into_state(Name, State):- 
 ignore(Name=statemaster),
 into_name(Name, GName), 
 nb_getval('$states', RB),
 into_named_map(RB, GName, State, make_state()).

make_state(State):- reset_state(State).
reset_state(State):- hashtable_set(State,star_name,star), hashtable_set(State,star_num,1).

into_props(NState,Props,NPropsO):-
 must(cate_states(NState,NCate)),
 must(into_pairs(Props,Pairs)),
 must(append(NCate,Pairs,NProps)),
 flatten(NProps, NPropsO).

cate_states(NState,NCate):-into_pairs(NState, Pairs),
   include(cate_state,Pairs,NCate).

cate_state(N=_):- cate_prop(N).
cate_prop(pattern).
cate_prop(template).



% ===================================================================
% ===================================================================
set_template(Path, Template, Graph):- into_state(State),set_pathprops( State, Path, template = (Template), Graph).

get_template(Path, Template, Graph):- into_state(State),get_pathprops( State, Path, template = (Template), Graph).

clear_graph(Graph):- notrace((into_graph(Graph, NGraph),hashtable_clear(NGraph))).

% ===================================================================
% ===================================================================
set_pathprops(Path, Props, Graph):- set_pathprops(_State, Path, Props, Graph).

set_pathprops(State, Path, Props, Graph):- 
 must(notrace((into_state(State, NState), 
          into_path(Path, NPath), 
          into_props([pattern=Path|NState],Props,NProps),
          into_graph(Graph, NGraph)))), 
 set_pathprop_now(NState, NPath, NProps, NGraph).
 
set_pathprop_now(_State, [], Props, Graph):- !, 
 must(compound(Props)), 
 hashtable_set_props(Graph, Props).


set_pathprop_now( State, [W1|More], Props, Graph):- 
 functor(W1,Index,_), !, 
 ( hashtable_get(Graph, Index, Next) 
   -> set_pathprop_now( State, More, Props, Next)
    ; (hashtable_new(NewNode),       
       set_pathprop_now( State, More, Props, NewNode),
       (Index==W1 -> NewNodeTerm = NewNode ; w(W1,NewNode) = NewNodeTerm ),
       hashtable_set(Graph, Index, NewNodeTerm))).


% ===================================================================
% ===================================================================
get_pathprops(Path, Props, Graph):- get_pathprops(_State, Path, Props, Graph),!.

get_pathprops(_State, Path, Props, Graph):- is_hashtable(Graph), Path==[], !,hashtable_get_props(Graph, Props).
get_pathprops( State, Path, Props, Graph):-
 term_variables(Props,PropsV),
 notrace((into_state(State, NState), 
          into_path(Path, NPath),
          into_props([pattern=Path|NState],Props,NProps),          
          into_graph(Graph, NGraph))), 
 get_pathprops_now(NState, NPath, NProps, NGraph),!,
 ignore((PropsV==[Props], flatten(NProps,Props))).

get_pathprops_now( State, [W1|More], Props, Graph):- !, 
 hashtable_get(Graph, W1, Next), 
 get_pathprops_now( State, More, Props, Next).
get_pathprops_now(_State, _, Props, Graph):-                       
 hashtable_get_props(Graph, Props).


% ===================================================================
% ===================================================================
path_match(Path, Result):- path_match(_State, Path, _Graph, Result).

path_match(State, Path, Graph, Result):-
 must(notrace((into_state(State, NState), 
          =(Path, NPath), 
          into_graph(Graph, NGraph), 
          copy_term(Result,Result0),
          reset_state(NState)))),
 path_match_now(NState, NPath, NGraph, Result0),
 notrace((duplicate_term(Result0,Result),
 set_result_vars(NState, Result))),!.


set_result_vars(S,X):- 
  ignore((
     compound(X),
     forall(arg(N,X,E),
           (compound(E),
            ((E=get(A),hashtable_get(S,A,V))
             -> nb_setarg(N,X,V)
             ; set_result_vars(S,E)))))).


call_with_filler(NewCall):- call(NewCall).

path_match_now(State, Path, Graph, Result):- 
  get_pathprops( State, Path, template = (Result), Graph).

% Call_Star match
path_match_now(State, InputList, Graph, Result):- 
 hashtable_get(Graph, 'call_star', Found),
 must(w(call_star(Star,Call),GraphMid)=Found),
 star_n(Star,N), subst(Call,Star,Left,NewCall),
 complex_match(State, N, InputList, Left, _Right, call_with_filler(NewCall), GraphMid, Result).

% Call then match
path_match_now(State, InputList, Graph, Result):- 
 hashtable_get(Graph, 'call', w(call(Call),GraphMid)),
 call_with_filler(Call),
 path_match_now(State, InputList, GraphMid, Result).

% phrase match
path_match_now(State, InputList, Graph, Result):- 
 hashtable_get(Graph, 'phrase', w(phrase(DCG),GraphMid)),
 phrase(DCG,InputList,Rest),
 path_match_now(State, Rest, GraphMid, Result).

% exact match
path_match_now(State, [Input|List], Graph, Result):- 
 into_path(Input,InputM),
 hashtable_get(Graph, InputM, GraphMid), 
 path_match_now(State, List, GraphMid, Result).
% Star match
path_match_now(State, InputList, Graph, Result):-
 star_n(Star,N),
 hashtable_get(Graph, Star, ComplexHT),   
 complex_match(State, N, InputList, _Left, _Right, true, ComplexHT, Result).

star_n('_',1).
star_n('^',0).
star_n('*',1).

complex_match(State, Min, InputList,Left,Right,NewCall,ComplexHT, Result):-
 member(NextWord, InputList), 
 into_path(NextWord,NextWordU),
 hashtable_get(ComplexHT, NextWordU, GraphNext), 
 append(Left, [NextWord|Right], InputList), 
 length(Left, LL), LL>=Min, 
 hashtable_get(State,star_num,StarNum),
 hashtable_get(State,star_name,StarName),
 %set_state(State, star, Left), 
   StarNum2 is StarNum + 1,
   hashtable_set(State,star_num,StarNum2),
   atom_concat(StarName,StarNum,StarVar),
   hashtable_set(State,StarVar,Left),
   NewCall,
 path_match_now(State, Right, GraphNext, Result).


  
 
%%REAL-UNUSED set_matchit1(StarName, Pattern, Matcher, OnBind):- length(Pattern, MaxLen0), MaxLen is MaxLen0 + 2, 
%%REAL-UNUSED set_matchit2(StarName, Pattern, Matcher, MaxLen, OnBind).


:- into_graph(_, _).

%:- rtrace(set_template([a, b1, c], template_a_b1_c, _)).
%:- set_template([a, b2, c], template_a_b2_c, _).
%:- set_pathprops([a, b, c2, d, e], pattern([a, b, c2, d, e]), _).
%:- set_pathprops([a, b, c2, d, e], [a=aaaa,b=bbbb], _).
:- set_template([a, b, c, d, e], abcde, _).
:- set_template([a, b, c2, d, e], abccde, _).
:- set_template([a, b, c2, d, e], abc2de, _).
:- set_template([a, b, *, e], ab_e(get(star1)), _).
:- set_template([a, b, '_'], ab(get(star1)), _).
:- set_template([a, b2, *, e], ab_e(get(star1)), _).
:- set_template([a, b2, '_', e], ab(get(star1)), _).
:- set_template([a, call_star(*,(trace,member(*,[['b3']]))),c,d,e], call_member(get(star1)), _).
:- set_template([a, call(X=1),b4,c,d,e], call_x(X), _).

:- path_match([a, b, c, d, e],R), dmsg(R), R = abcde.
:- path_match([a, b, c2, d, e],R),dmsg(R), R = abc2de.
:- path_match([a, b, c3, d, e],R),dmsg(R), R = ab_e([c3, d]).
:- path_match([a, b2, c4, d, e],R),dmsg(R), R = ab([c4, d]).
:- path_match([a, b3, c, d, e],R),dmsg(R).
:- path_match([a, b4, c, d, e],R),dmsg(R).

:- show_name_values.

% :- clear_graph(graphmaster).

