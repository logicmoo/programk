:-module(bot_factoids, 
 [
  test_factoids/0,
  test_factoids/1,
  test_factoids/2,
  test_factoids_parse1/0,
  test_factoids_parse2/0,  
  foc_factoids_stream/2,
  text_to_factoids_pos/2,
  text_to_factoids_sents/2,
  text_to_factoids_segs/2,
  factoids_parse/2]).

:- set_module(class(library)).
:- set_module(base(system)).
:- use_module(library(logicmoo_utils)).
:- use_module(library(logicmoo_nlu/bot_penn_trees)).
:- use_module(library(logicmoo_nlu/bot_tokenize)).

read_factoids_lines(In, Result):- factoids_to_w2(In, Result),!.

text_to_factoids_tree(Text,LExpr):-
  factoids_parse(Text, String),
  nop(dmsg(factoids_parse=String)),  
  factoids_to_w2(String,LExpr),
  nop(print_tree_nl(factoids=LExpr)).

%factoids_to_w2((Word,POS),[POS,Word]).
factoids_to_w2(Str,StrO):- string(Str),StrO=Str.
factoids_to_w2(In, Result):- is_stream(In),!,read_term(In,Term,[]),factoids_to_w2(Term, Result).
factoids_to_w2(List,ListO):- is_list(List),!,include(compound,List,ListO).
factoids_to_w2(factoids(_In,Text),Out):- !, factoids_to_w2(Text,ListO).
factoids_to_w2(Text,ListO):- \+ compound(Text), on_x_fail(atom_to_term(Text,Term,_)),!,factoids_to_w2(Term,ListO).
factoids_to_w2(Text,_ListO):- \+ compound(Text), nl,writeq(Text),nl,!,fail.

factoids_lexical_segs(I,O):-
  old_into_lexical_segs(I,M),!,
  factoids_parse_or_skip(I,S),!,
  merge_factoids(S,M,O),!.

%factoids_parse_or_skip(I,O):- catch(factoids_parse(I,O),_,fail),nonvar(O),!.
factoids_parse_or_skip(_,[]).

merge_factoids([],O,O):-!.
merge_factoids([H|T],I,O):- !, merge_factoids(H,I,M), merge_factoids(T,M,O).
merge_factoids(w(W,L),O,O):- member(w(W,OL),O), \+ member(factoids,OL),!,    
  ignore((member(spos(Pos),L),  downcase_atom(Pos,DPos), set_pos(2,DPos,OL))), 
  nb_set_add(OL,[factoids|L]), !.
merge_factoids(span(List),I,O):- member(span(_),List),!,
  merge_factoids(List,I,O),!.
merge_factoids(span(List),O,O):- 
  member(seg(S,E),List), member(span(Other),O), member(seg(S,E),Other),!,
  nb_set_add(Other,[factoids|List]).
merge_factoids(dep_tree(Type,R,Arg),O,O):- 
  member(w(_,Other),O),member(node(R),Other),
  nb_set_add(Other,dep_tree(Type,R,Arg)).
merge_factoids(_,I,I):-!.
merge_factoids(S,I,O):- append(I,[S],O).

factoids_stream_to_w2(In,_, Result):- peek_string(In,10,S),atom_contains(S,"factoids("),!,read_term(In,Term,[]),factoids_to_w2(Term, Result).
factoids_stream_to_w2(In,S, Result):- atom_contains(S,"factoids("),!,read_term_from_atom_rest(In,S,Term),factoids_to_w2(Term, Result).
factoids_stream_to_w2(In,S, Result):- at_end_of_stream(In),!,factoids_to_w2(S, Result).
factoids_stream_to_w2(In,_, Result):- repeat, read_pending_codes(In,Codes,[]),
 (Codes==[]->(sleep(0.1),fail);true),sformat(S,'~s',[Codes]),
 factoids_stream_to_w2(In,S, Result).


:- dynamic(tmp:existing_factoids_stream/4).
:- volatile(tmp:existing_factoids_stream/4).
foc_factoids_stream(Out,In):- thread_self(Self),tmp:existing_factoids_stream(Self,_,Out,In),!,clear_factoids_pending(In).
/*
foc_factoids_stream(Out,In):- tmp:existing_factoids_stream(OldThread,FFid,Out,In), \+ thread_property(OldThread,status(running)),!,
  retract(tmp:existing_factoids_stream(OldThread,FFid,Out,In)),
  thread_self(Self),
  assert(tmp:existing_factoids_stream(Self,FFid,Out,In)),!.
*/
foc_factoids_stream(Out,In):-
  thread_self(Self),
  tcp_socket(Socket),
  catch((tcp_connect(Socket, 'logicmoo.org':4096),
  tcp_open_socket(Socket, StreamPair)),_,fail),!,
  StreamPair = In, StreamPair = Out,
  set_stream(In,close_on_exec(false)),
  set_stream(In,close_on_abort(false)),
  set_stream(In,eof_action(eof_code)),
  assert(tmp:existing_factoids_stream(Self,_,Out,In)),!.

foc_factoids_stream(Out,In):- current_prolog_flag(python_local,true),
  lmconfig:space_py_dir(Dir),
  thread_self(Self),
  sformat(S,'python bot_factoids.py -nc -cmdloop ',[]),
  nop(writeln(S)),
    process_create(path(bash), ['-c', S], [ cwd(Dir),  stdin(pipe(Out)),stdout(pipe(In)), stderr(null), process(FFid)]),!,
  set_stream(In,close_on_exec(false)),
  set_stream(Out,close_on_exec(false)),
  set_stream(In,close_on_abort(false)),
  set_stream(Out,close_on_abort(false)),
  set_stream(In,eof_action(eof_code)),
  set_stream(Out,eof_action(eof_code)),
  sleep(1.0),
  read_until_factoids_notice(In,"cmdloop_Ready."),!,
  assert(tmp:existing_factoids_stream(Self,FFid,Out,In)).

read_until_factoids_notice(In,Txt):- repeat,read_line_to_string(In,Str),(Str==end_of_file;atom_contains(Str,Txt)),!.

clear_factoids_pending(In):- nop((read_pending_codes(In,Codes,[]),dmsg(clear_factoids_pending=Codes))).

:- prolog_load_context(directory,Dir), assert(lmconfig:space_py_dir(Dir)).

tokenize_factoids_string(Text,StrO):- any_to_string(Text,Str),  replace_in_string('\n',' ',Str,StrO).
/*
tokenize_factoids_string(Text,StrO):- any_to_string(Text,Str), replace_in_string(['\\'='\\\\','\''='\\\''],Str,StrM),
  atomics_to_string(["'",StrM,"'"],StrO).
*/


factoids_parse(Text, Lines) :- 
  tokenize_factoids_string(Text,String),
  factoids_parse2(String, Lines).

factoids_parse2(String, Lines) :- 
  once(factoids_parse3(String, Lines)
      ;factoids_parse4(String, Lines)).

try_factoids_stream(Out,Write):- once(catch((format(Out,'~w',[Write])),_,
  (retract(tmp:existing_factoids_stream(_,_,Out,_)),fail))).

% Clears if there is a dead one
factoids_parse3(_String, _Lines) :- fail,
  foc_factoids_stream(Out,_In),
  try_factoids_stream(Out,''),fail.
% Reuses or Creates
factoids_parse3(String, Lines) :-
  foc_factoids_stream(Out,In),
  try_factoids_stream(Out,String),
  try_factoids_stream(Out,'\n'),
  flush_output(Out),
  read_factoids_lines(In, Lines).

% Very slow version
factoids_parse4(String, Lines) :- current_prolog_flag(python_local,true),
  lmconfig:space_py_dir(Dir),
  sformat(S,'python bot_factoids.py -nc ~q ',[String]),
  nop(writeln(S)),
    process_create(path(bash), ['-c', S], [ cwd(Dir), stdout(pipe(In))]),!,
  read_until_factoids_notice(In,"cmdloop_Ready."),!,
  read_factoids_lines(In, Lines).

test_factoids_parse1 :-
  String = "Can the can do the Can Can?",
  factoids_parse3(String, Lines),
  pprint_ecp_cmt(yellow,test_factoids_parse1=Lines).

test_factoids_parse2 :-
  Text = "Can the can do the Can Can?",
  factoids_parse4(Text,Lines),
  pprint_ecp_cmt(yellow,test_factoids_parse2=Lines).

test_factoids_parse3 :-
  Text = "Can the can do the Can Can?",
  factoids_parse2(Text,Lines),
  pprint_ecp_cmt(yellow,test_factoids_parse3=Lines).

   
factoids_pos_info(Text,PosW2s,Info,LExpr):-
  text_to_factoids_sents(Text,LExpr),
  tree_to_lexical_segs(LExpr,SegsF),
  segs_retain_w2(SegsF,Info,PosW2s),!.

text_to_factoids_pos(Text,PosW2s):- factoids_parse(Text,PosW2s),!.
text_to_factoids_pos(Text,PosW2s):- factoids_pos_info(Text,PosW2s0,_Info,_LExpr),guess_pretty(PosW2s0),!,PosW2s=PosW2s0.
  
text_to_factoids_segs(Text,Segs):-
  text_to_factoids_tree(Text,LExpr),
  tree_to_lexical_segs(LExpr,Segs).

text_to_factoids_sents(Text,Sent):-
  text_to_factoids_segs(Text,Segs),!,
  factoids_segs_to_sentences(Segs,Sent),!.

factoids_segs_to_sentences(Segs,sentence(0,W2,Info)):-
  segs_retain_w2(Segs,Info,W2).


:- if( \+ getenv('keep_going','-k')).
:- use_module(library(editline)).
:- add_history((call(make),call(test_factoids1))).
:- endif.

baseKB:regression_test:- test_factoids(1,X),!,test_factoids(X).
baseKB:sanity_test:- make, forall(test_factoids(1,X),test_factoids(X)).
baseKB:feature_test:- test_factoids.

test_factoids0:- 
  Txt = "PERSON1 asks : Hey , what 's going on XVAR. < p >. PERSON2 said : Not a whole lot . . < p >. PERSON2 said : I 'm looking forward to the weekend , though . . < p >. PERSON1 asks : Do you have any big plans XVAR. < p >. PERSON2 said : Yes . . < p >. PERSON2 said : I 'm going to Wrigley Field on Saturday . . < p >. PERSON1 asks : Aren 't the Cubs out of town XVAR. < p >. PERSON2 said : Yes , but there 's a big concert at Wrigley this weekend . . < p >. PERSON1 said : Oh nice . . < p >. PERSON1 asks : Who 's playing XVAR. < p >. PERSON2 said : Pearl Jam is headlining the Saturday night show . . < p >. PERSON1 said : Wow , Pearl Jam . . < p >. PERSON1 said : I remeber when I got their first CD , Ten , at the record store at Harlem and Irving Plaza . . < p >. PERSON2 said : Oh right . . < p >. PERSON2 said : I remember that record store . . < p >. PERSON1 said : It was called Rolling Stone , and they went out of business many years ago . . < p >. PERSON2 said : Oh that 's too bad . . < p >. PERSON2 said : I really loved taking the bus to Harlem and Irving and visiting that store . . < p >. PERSON1 said : Me too . . < p >. PERSON1 said : We did n't have the internet back then and had to discover new music the hard way . . < p >. PERSON2 said : Haha yes . . < p >. PERSON2 said : I remember discovering ' ' Nirvana before they got famous . . < p >. PERSON1 said : Those were the good old days . . < p >. PERSON2 said : Yes they were . . < p >. PERSON2 said : I need to dig up my old Sony disc player and pop in an old CD . . < p >. PERSON1 asks : Where did the time go XVAR. < p >. PERSON1 said : Pearl Jam is 25 years old already . . < p >. PERSON2 said : It seems like only yesterday that the grunge music movement took over . . < p >. PERSON1 said : Right . . < p >. PERSON1 said : I bet everyone at the concert will be in their forty 's . . < p >. PERSON2 said : No doubt . . < p >. PERSON2 said : Well , I hope you have a great time at the concert . . < p > .",
  test_factoids(Txt),
  ttyflush,writeln(('\n test_factoids0.')),!.

test_factoids1:- 
  %Txt = "Rydell used his straw to stir the foam and ice remaining at the bottom of his tall plastic cup, as though he were hoping to find a secret prize.",
  Txt = "The Norwegian dude lives happily in the first house.",
  test_factoids(Txt),
  ttyflush,writeln(('\n test_factoids1.')),!.
test_factoids2:- 
  Txt = "Rydell used his straw to stir the foam and ice remaining at the bottom of his tall plastic cup, as though he were hoping to find a secret prize.",
  %Txt = "The Norwegian dude lives happily in the first house.",
  test_factoids(Txt),
  ttyflush,writeln(('\n test_factoids2.')),!.

test_factoids:- 
  Txt = "Rydell was a big quiet Tennessean with a sad shy grin, cheap sunglasses, and a walkie-talkie screwed permanently into one ear.",
  test_factoids(Txt),
  ttyflush,writeln(('\n test_factoids.')),
  fail.
test_factoids:- forall(test_factoids(X),test_factoids(X)).

test_1factoids(Text):- 
  format('~N?- ~p.~n',[test_factoids(Text)]),
  text_to_factoids_tree(Text,W),
  print_tree_nl(W),
  !.
test_1factoids(Text):- wdmsg(failed(test_1factoids(Text))).

test_factoids(N):- number(N),!, forall(test_factoids(N,X),test_1factoids(X)). 
test_factoids(X):- test_factoids(_,X),nop(lex_info(X)).

test_factoids(_,X):- nonvar(X), !, once(test_1factoids(X)).

test_factoids(1,".\nThe Norwegian lives in the first house.\n.").
test_factoids(1,"").
test_factoids(1,".").
test_factoids(1,"\n").

test_factoids(1,"Rydell used his straw to stir the foam and ice remaining at the bottom of his tall plastic cup, as though he were hoping to find a secret prize.").


test_factoids(2,Each):- test_factoids(3,Atom),atomic_list_concat(List,'\n',Atom), member(Each,List).

test_factoids(3,
'There are 5 houses with five different owners.
 These five owners drink a certain type of beverage, smoke a certain brand of cigar and keep a certain pet.
 No owners have the same pet, smoke the same brand of cigar or drink the same beverage.
 The man who smokes Blends has a neighbor who drinks water.
 A red cat fastly jumped onto the table which is in the kitchen of the house.
 After Slitscan, Laney heard about another job from Rydell, the night security man at the Chateau.
 Rydell was a big quiet Tennessean with a sad shy grin, cheap sunglasses, and a walkie-talkie screwed permanently into one ear.
 Concrete beams overhead had been hand-painted to vaguely resemble blond oak.
 The chairs, like the rest of the furniture in the Chateau\'s lobby, were oversized to the extent that whoever sat in them seemed built to a smaller scale.
 Rydell used his straw to stir the foam and ice remaining at the bottom of his tall plastic cup, as though he were hoping to find a secret prize.
 A book called, "A little tribute to Gibson".
 "You look like the cat that swallowed the canary, " he said, giving her a puzzled look.').


test_factoids(4,".
The Brit lives in the red house.
The Swede keeps dogs as pets.
The Dane drinks tea.
The green house is on the immediate left of the white house.
The green house's owner drinks coffee.
The owner who smokes Pall Mall rears birds.
The owner of the yellow house smokes Dunhill.
The owner living in the center house drinks milk.
The Norwegian lives in the first house.
The owner who smokes Blends lives next to the one who keeps cats.
The owner who keeps the horse lives next to the one who smokes Dunhills.
The owner who smokes Bluemasters drinks beer.
The German smokes Prince.
The Norwegian lives next to the blue house.
The owner who smokes Blends lives next to the one who drinks water.").

:- add_history(test_factoids).
:- fixup_exports.

