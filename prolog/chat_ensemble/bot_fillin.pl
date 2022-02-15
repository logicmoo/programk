:-module(bot_fillin, 
 [
  test_fillin/0,
  test_fillin/1,
  test_fillin/2,
  test_fillin_parse1/0,
  test_fillin_parse2/0,  
  foc_fillin_stream/2,
  text_to_fillin_pos/2,
  text_to_fillin_sents/2,
  text_to_fillin_segs/2,
  fillin_parse/2]).
% test_fillin('{"presence_penalty":1.4,"prompt":"How do you spell your name?"}',X).
% test_fillin('{"presence_penalty":2.0,"prompt":"A florida man was ", "max_tokens":750}',X),print(X).
:- set_module(class(library)).
:- set_module(base(system)).
:- use_module(library(logicmoo_utils)).
:- use_module(library(logicmoo_nlu/parser_penn_trees)).
:- use_module(library(logicmoo_nlu/parser_tokenize)).

:- dynamic(lmconfig:bot_py_dir/1).
:- ignore(( \+ lmconfig:bot_py_dir(Dir), prolog_load_context(directory,Dir), assert(lmconfig:bot_py_dir(Dir)))).

read_fillin_lines(In, Result):- fillin_to_dict(In, Result),!.

fillin_completion(Text,LExpr):-
  fillin_parse(Text, String),
  nop(dmsg(fillin_parse=String)),  
  fillin_to_txt(String,LExpr),
  nop(print_tree_nl(fillin=LExpr)),!.

fillin_to_w2(Text,Result):- fillin_to_dict(Text,M),dict_to_txt(M, Result).
%fillin_to_w2((Word,POS),[POS,Word]).
%fillin_to_w2(Text,Result):- fillin_to_dict(Text,Result),!.
%fillin_to_w2(Text,_ListO):- \+ compound(Text), nl,writeq(Text),nl,!,fail.

fillin_to_dict(Text,Result):- is_dict(Text),!,Result=Text.
fillin_to_dict(In, Result):- is_stream(In),!,fillin_stream_to_dict(In,_, Term),fillin_to_dict(Term, Result).
fillin_to_dict(fillin(_In,Text),Result):- !, fillin_to_dict(Text,Result).
fillin_to_dict(Compound,Result):- \+ atomic(Compound),!, any_to_json_dict(Compound,Result),!.
fillin_to_dict(Text,Result):- notrace(on_x_fail(atom_json_dict(Text,Term,[]))),!,fillin_to_dict(Term,Result).
fillin_to_dict(Text,Result):- notrace(on_x_fail(atom_json_term(Text,Term,[]))),!,fillin_to_dict(Term,Result).
fillin_to_dict(Text,Result):- notrace(on_x_fail(atom_to_term(Text,Term,_))),!,fillin_to_dict(Term,Result).

dict_to_txt(Dict,Result):- kv_name_value(Dict,choices,E),!,fillin_to_txt(E,Result).
dict_to_txt(Dict,Result):- kv_name_value(Dict,text,Result).

fillin_to_txt(In, Result):-  is_stream(In),!,fillin_stream_to_dict(In,_, Term),!,fillin_to_txt(Term, Result).
fillin_to_txt(E,V):- is_list(E),!,member(S,E),fillin_to_txt(S,V).
fillin_to_txt(Text,Result):- is_dict(Text),!,dict_to_txt(Text,Result).
fillin_to_txt(Text,Result):- compound(Text),!,kv_name_value(Text,text,Result).
fillin_to_txt(Text,Text):-!.

kv_name_value(E,_,_):- \+ compound(E),!,fail.
kv_name_value(E,K,V):- is_list(E),!,member(S,E),kv_name_value(S,K,V).
kv_name_value(E,K,V):- compound_name_arity(E,_,2),E=..[_,N,V],atomic(N),\+ number(N),name(K,N),!.
kv_name_value(E,K,V):- is_dict(E),get_dict(K,E,V),!.
kv_name_value(E,K,V):- arg(_,E,S),compound(S),kv_name_value(S,K,V).

fillin_lexical_segs(I,O):-
  old_into_lexical_segs(I,M),!,
  fillin_parse_or_skip(I,S),!,
  merge_fillin(S,M,O),!.

%fillin_parse_or_skip(I,O):- catch(fillin_parse(I,O),_,fail),nonvar(O),!.
fillin_parse_or_skip(_,[]).

merge_fillin([],O,O):-!.
merge_fillin([H|T],I,O):- !, merge_fillin(H,I,M), merge_fillin(T,M,O).
merge_fillin(w(W,L),O,O):- member(w(W,OL),O), \+ member(fillin,OL),!,    
  ignore((member(spos(Pos),L),  downcase_atom(Pos,DPos), set_pos(2,DPos,OL))), 
  nb_set_add(OL,[fillin|L]), !.
merge_fillin(span(List),I,O):- member(span(_),List),!,
  merge_fillin(List,I,O),!.
merge_fillin(span(List),O,O):- 
  member(seg(S,E),List), member(span(Other),O), member(seg(S,E),Other),!,
  nb_set_add(Other,[fillin|List]).
merge_fillin(dep_tree(Type,R,Arg),O,O):- 
  member(w(_,Other),O),member(node(R),Other),
  nb_set_add(Other,dep_tree(Type,R,Arg)).
merge_fillin(_,I,I):-!.
merge_fillin(S,I,O):- append(I,[S],O).

fillin_stream_to_dict(In,_, Result):- peek_string(In,10,S),atom_contains(S,"fillin("),!,read_term(In,Term,[]),fillin_to_dict(Term, Result).
fillin_stream_to_dict(In,S, Result):- atomic(S),atom_contains(S,"fillin("),!,read_term_from_atom_rest(In,S,Term),fillin_to_dict(Term, Result).
fillin_stream_to_dict(In,S, Result):- atomic(S),at_end_of_stream(In),!,fillin_to_dict(S, Result).
fillin_stream_to_dict(In,_, Result):- repeat, read_pending_codes(In,Codes,[]),
 (Codes==[]->(sleep(0.1),fail);true),sformat(S,'~s',[Codes]),
 fillin_stream_to_dict(In,S, Result).


:- dynamic(tmp:existing_fillin_stream/4).
:- volatile(tmp:existing_fillin_stream/4).
foc_fillin_stream(Out,In):- thread_self(Self),tmp:existing_fillin_stream(Self,_,Out,In),!,clear_fillin_pending(In).
/*
foc_fillin_stream(Out,In):- tmp:existing_fillin_stream(OldThread,FFid,Out,In), \+ thread_property(OldThread,status(running)),!,
  retract(tmp:existing_fillin_stream(OldThread,FFid,Out,In)),
  thread_self(Self),
  assert(tmp:existing_fillin_stream(Self,FFid,Out,In)),!.
*/
foc_fillin_stream(Out,In):- 
  user:network_service_info(fillin,port,P4083),
  thread_self(Self),
  tcp_socket(Socket),
  catch((tcp_connect(Socket, 'logicmoo.org':P4083),
  tcp_open_socket(Socket, StreamPair)),_,fail),!,
  StreamPair = In, StreamPair = Out,
  set_stream(In,close_on_exec(false)),
  set_stream(In,close_on_abort(false)),
  set_stream(In,eof_action(eof_code)),
  assert(tmp:existing_fillin_stream(Self,_,Out,In)),!.

foc_fillin_stream(Out,In):- current_prolog_flag(python_local,true),
  lmconfig:bot_py_dir(Dir),
  thread_self(Self),
  sformat(S,'python bot_fillin.py -nc -cmdloop ',[]),
  nop(writeln(S)),
    process_create(path(bash), ['-c', S], [ cwd(Dir),  stdin(pipe(Out)),stdout(pipe(In)), stderr(null), process(FFid)]),!,
  set_stream(In,close_on_exec(false)),
  set_stream(Out,close_on_exec(false)),
  set_stream(In,close_on_abort(false)),
  set_stream(Out,close_on_abort(false)),
  set_stream(In,eof_action(eof_code)),
  set_stream(Out,eof_action(eof_code)),
  sleep(1.0),
  read_until_fillin_notice(In,"cmdloop_Ready."),!,
  assert(tmp:existing_fillin_stream(Self,FFid,Out,In)).

read_until_fillin_notice(In,Txt):- repeat,read_line_to_string(In,Str),(Str==end_of_file;atom_contains(Str,Txt)),!.

current_fillin_stream(In):- thread_self(Self),tmp:existing_fillin_stream(Self,_FFid,_Out,In).

clear_fillin_pending:- current_fillin_stream(In), clear_fillin_pending0(In),!.
clear_fillin_pending(In):- nop(clear_fillin_pending0(In)).

clear_fillin_pending0(In):- at_end_of_stream(In),!,dmsg(clear_fillin_pending=at_end_of_stream).
clear_fillin_pending0(In):- read_pending_codes(In,Codes,[]),dmsg(clear_fillin_pending=Codes).

tokenize_fillin_string([Str|Text],AtomO):- atomic(Str), is_list(Text), !, text_to_fillin_string([Str|Text],AtomO).
tokenize_fillin_string(Atom,AtomO):- atom(Atom),!,Atom=AtomO.
tokenize_fillin_string(JSON,AtomO):- compound(JSON),!,any_to_json_dict(JSON,Dict),atom_json_dict(AtomO,Dict,[]).
tokenize_fillin_string(Text,AtomO):- tokenize_fillin_string(Text,AtomO).
text_to_fillin_string(Text,StrO):- any_to_string(Text,Str),  replace_in_string('\n',' ',Str,StrO).
/*
tokenize_fillin_string(Text,StrO):- any_to_string(Text,Str), replace_in_string(['\\'='\\\\','\''='\\\''],Str,StrM),
  atomics_to_string(["'",StrM,"'"],StrO).
*/

fillin_parse(Text, Lines) :- 
  tokenize_fillin_string(Text,Fillin),
  fillin_parse2(Fillin, Lines).

fillin_parse2(String, Lines) :- 
  once(fillin_parse3(String, Lines)
      ;fillin_parse4(String, Lines)).

try_fillin_stream(Out,Write):- once(catch((format(Out,'~w',[Write])),_,
  (retract(tmp:existing_fillin_stream(_,_,Out,_)),fail))).

% Clears if there is a dead one
fillin_parse3(_String, _Lines) :- fail,
  foc_fillin_stream(Out,_In),
  try_fillin_stream(Out,''),fail.
% Reuses or Creates
fillin_parse3(String, Lines) :-
  foc_fillin_stream(Out,In),
  try_fillin_stream(Out,String),
  try_fillin_stream(Out,'\n'),
  flush_output(Out),
  read_fillin_lines(In, Lines).

% Very slow version
fillin_parse4(String, Lines) :- current_prolog_flag(python_local,true),
  lmconfig:bot_py_dir(Dir),
  sformat(S,'python bot_fillin.py -nc ~q ',[String]),
  nop(writeln(S)),
    process_create(path(bash), ['-c', S], [ cwd(Dir), stdout(pipe(In))]),!,
  read_until_fillin_notice(In,"cmdloop_Ready."),!,
  read_fillin_lines(In, Lines).

test_fillin_parse1 :-
  String = "Can the can do the Can Can?",
  fillin_parse3(String, Lines),
  pprint_ecp_cmt(yellow,test_fillin_parse1=Lines).

test_fillin_parse2 :-
  Text = "Can the can do the Can Can?",
  fillin_parse4(Text,Lines),
  pprint_ecp_cmt(yellow,test_fillin_parse2=Lines).

test_fillin_parse3 :-
  Text = "Can the can do the Can Can?",
  fillin_parse2(Text,Lines),
  pprint_ecp_cmt(yellow,test_fillin_parse3=Lines).

   
fillin_pos_info(Text,PosW2s,Info,LExpr):-
  text_to_fillin_sents(Text,LExpr),
  tree_to_lexical_segs(LExpr,SegsF),
  segs_retain_w2(SegsF,Info,PosW2s),!.

text_to_fillin_pos(Text,PosW2s):- fillin_parse(Text,PosW2s),!.
text_to_fillin_pos(Text,PosW2s):- fillin_pos_info(Text,PosW2s0,_Info,_LExpr),guess_pretty(PosW2s0),!,PosW2s=PosW2s0.
  
text_to_fillin_segs(Text,Segs):-
  fillin_completion(Text,LExpr),
  tree_to_lexical_segs(LExpr,Segs).

text_to_fillin_sents(Text,Sent):-
  text_to_fillin_segs(Text,Segs),!,
  fillin_segs_to_sentences(Segs,Sent),!.

fillin_segs_to_sentences(Segs,sentence(0,W2,Info)):-
  segs_retain_w2(Segs,Info,W2).


:- if( \+ getenv('keep_going','-k')).
:- use_module(library(editline)).
:- add_history((call(make),call(test_fillin1))).
:- endif.

baseKB:regression_test:- test_fillin(1,X),!,test_fillin(X).
baseKB:sanity_test:- make, forall(test_fillin(1,X),test_fillin(X)).
baseKB:feature_test:- test_fillin.

test_fillin0:- 
  Txt = "PERSON1 asks : Hey , what 's going on XVAR. < p >. PERSON2 said : Not a whole lot . . < p >. PERSON2 said : I 'm looking forward to the weekend , though . . < p >. PERSON1 asks : Do you have any big plans XVAR. < p >. PERSON2 said : Yes . . < p >. PERSON2 said : I 'm going to Wrigley Field on Saturday . . < p >. PERSON1 asks : Aren 't the Cubs out of town XVAR. < p >. PERSON2 said : Yes , but there 's a big concert at Wrigley this weekend . . < p >. PERSON1 said : Oh nice . . < p >. PERSON1 asks : Who 's playing XVAR. < p >. PERSON2 said : Pearl Jam is headlining the Saturday night show . . < p >. PERSON1 said : Wow , Pearl Jam . . < p >. PERSON1 said : I remeber when I got their first CD , Ten , at the record store at Harlem and Irving Plaza . . < p >. PERSON2 said : Oh right . . < p >. PERSON2 said : I remember that record store . . < p >. PERSON1 said : It was called Rolling Stone , and they went out of business many years ago . . < p >. PERSON2 said : Oh that 's too bad . . < p >. PERSON2 said : I really loved taking the bus to Harlem and Irving and visiting that store . . < p >. PERSON1 said : Me too . . < p >. PERSON1 said : We did n't have the internet back then and had to discover new music the hard way . . < p >. PERSON2 said : Haha yes . . < p >. PERSON2 said : I remember discovering ' ' Nirvana before they got famous . . < p >. PERSON1 said : Those were the good old days . . < p >. PERSON2 said : Yes they were . . < p >. PERSON2 said : I need to dig up my old Sony disc player and pop in an old CD . . < p >. PERSON1 asks : Where did the time go XVAR. < p >. PERSON1 said : Pearl Jam is 25 years old already . . < p >. PERSON2 said : It seems like only yesterday that the grunge music movement took over . . < p >. PERSON1 said : Right . . < p >. PERSON1 said : I bet everyone at the concert will be in their forty 's . . < p >. PERSON2 said : No doubt . . < p >. PERSON2 said : Well , I hope you have a great time at the concert . . < p > .",
  test_fillin(Txt),
  ttyflush,writeln(('\n test_fillin0.')),!.

test_fillin1:- 
  %Txt = "Rydell used his straw to stir the foam and ice remaining at the bottom of his tall plastic cup, as though he were hoping to find a secret prize.",
  Txt = "The Norwegian dude lives happily in the first house.",
  test_fillin(Txt),
  ttyflush,writeln(('\n test_fillin1.')),!.
test_fillin2:- 
  Txt = "Rydell used his straw to stir the foam and ice remaining at the bottom of his tall plastic cup, as though he were hoping to find a secret prize.",
  %Txt = "The Norwegian dude lives happily in the first house.",
  test_fillin(Txt),
  ttyflush,writeln(('\n test_fillin2.')),!.

test_fillin:- 
  Txt = "Rydell was a big quiet Tennessean with a sad shy grin, cheap sunglasses, and a walkie-talkie screwed permanently into one ear.",
  test_fillin(Txt),
  ttyflush,writeln(('\n test_fillin.')),
  fail.
test_fillin:- forall(test_fillin(X),test_fillin(X)).

test_1fillin(Text):- 
  format('~N?- ~p.~n',[test_fillin(Text)]),
  fillin_completion(Text,W),
  print_tree_nl(W),
  !.
test_1fillin(Text):- wdmsg(failed(test_1fillin(Text))).

test_fillin(N):- number(N),!, forall(test_fillin(N,X),test_1fillin(X)). 
test_fillin(X):- test_fillin(_,X),nop(lex_info(X)).

test_fillin(In,Out):- nonvar(In),\+ number(In),var(Out),!,fillin_completion(In,Out).
test_fillin(_,X):- nonvar(X), !, once(test_1fillin(X)).

test_fillin(1,".\nThe Norwegian lives in the first house.\n.").
test_fillin(1,"").
test_fillin(1,".").
test_fillin(1,"\n").

test_fillin(1,"Rydell used his straw to stir the foam and ice remaining at the bottom of his tall plastic cup, as though he were hoping to find a secret prize.").

test_fillin(2,Each):- test_fillin(3,Atom),atomic_list_concat(List,'\n',Atom), member(Each,List).

test_fillin(3,
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


test_fillin(4,".
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

:- add_history(test_fillin).
:- fixup_exports.

