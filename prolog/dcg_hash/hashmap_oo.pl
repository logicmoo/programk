



portray_hashtable(T):- is_hashtable(T),!,hashtable_pairs(T,P),dict_create(Dict,'HT',P),writeq(Dict).
% portray_hashtable(T):- is_hashtable(T),!,hashtable_pairs(T,P),write('HT{'),writeq(P),write('}').


user:portray(T):- notrace(((portray_hashtable(T)))),!.

is_hashtable(UDT):- notrace(is_rbtree(UDT)).

hashtable_new(UDT):- notrace(rb_new(UDT)).

hashtable_lookup(Key, Val, UDT):- notrace(rb_lookup(Key, Val, UDT)).

hashtable_get(UDT, Key, Val):- notrace(rb_lookup(Key, Val, UDT)).

hashtable_insert(UDT,Key,Value,NewUDT):- notrace(rb_insert(UDT,Key,Value,NewUDT)).

nb_hashtable_insert(UDT,Key,Value):- notrace(nb_rb_insert(UDT,Key,Value)).

hashtable_remove(UDT,Key):- notrace((ignore((rb_delete(UDT,Key,NewUDT),nb_copy_rb(NewUDT,UDT))))).

hashtable_clear(UDT):- notrace((rb_empty(NewUDT),nb_copy_rb(NewUDT,UDT))).

hashtable_set(UDT,Key,Value):- notrace((
  (var(UDT)->hashtable_new(UDT);must(is_hashtable(UDT))),
 ((nb_hashtable_get_node(Key,UDT,Node) 
 -> nb_hashtable_set_node_value(Node, Value)
 ; (rb_insert(UDT,Key,Value,NewUDT),nb_copy_rb(NewUDT,UDT)))))).

nb_copy_rb(NewUDT,UDT):- 
 notrace((
  arg(1,NewUDT,Arg1),duplicate_term(Arg1,Arg1D),nb_setarg(1,UDT,Arg1D),
  arg(2,NewUDT,Arg2),duplicate_term(Arg2,Arg2D),nb_setarg(2,UDT,Arg2D))).
 

nb_hashtable_set_node_value(Node, Value):- notrace((nb_rb_set_node_value(Node, Value))).

nb_hashtable_get_node(Key, UDT, Node):- notrace((nb_rb_get_node(Key, UDT, Node))).



hashtable_set_props(Graph, Props):- is_list(Props), !, 
  maplist(hashtable_set_props(Graph), Props).
hashtable_set_props(Graph, HT):-
  is_hashtable(HT),hashtable_pairs(HT,Pairs),!,
  hashtable_set_props(Graph, Pairs).
hashtable_set_props(Graph, [P|Props]):- !, 
  hashtable_set_props(Graph, Props),
  hashtable_set_props(Graph, P).
hashtable_set_props(Graph, Props):-
  props_kv(Props,Key,Value), 
  hashtable_set(Graph, Key, Value).

hashtable_get_props(Graph, Props):- var(Props),!, hashtable_pairs(Graph,Props).
hashtable_get_props(Graph, Props):- is_list(Props), !, 
  maplist(hashtable_get_props(Graph), Props).
hashtable_get_props(Graph, Key=Value):- !,hashtable_get(Graph, Key, Value).
hashtable_get_props(Graph, Props):-
  props_kv(Props,Key,Value), 
  hashtable_get(Graph, Key, Value).


hashtable_pairs(A,B):-notrace(hashtable_pairs_now(A,B)).

hashtable_pairs_now(Var,VarO):- var(Var),!,Var=VarO.
hashtable_pairs_now(Atomic,Atom):- \+ compound(Atomic),!,Atom=Atomic.
hashtable_pairs_now(UDT,PairsO):- is_hashtable(UDT),!,rb_visit(UDT,Pairs),maplist(hashtable_pairs_now,Pairs,PairsO).
hashtable_pairs_now(Pairs,PairsO):- is_list(Pairs),!,maplist(hashtable_pairs_now,Pairs,PairsO).
hashtable_pairs_now(Props, Key=ValueO):- % compound(Props),
  props_kv(Props,Key,Value),
  hashtable_pairs_now(Value,ValueO),!.
hashtable_pairs_now(VV,VV).

props_kv(Props,Key,Value):- Props=..[_, Key, Value].


into_pairs(Graph, Props):- 
  notrace(\+ compound(Graph)->Props=Graph 
    ; (into_pairs_now(Graph, Pairs),flatten([Pairs],Props))).
into_pairs_now(Graph, Props):- is_list(Graph), !, 
  maplist(into_pairs_now,Graph,Props).
into_pairs_now(Graph, Props):- \+ compound(Graph),!,Props=Graph.
into_pairs_now(Graph, Props):- is_hashtable(Graph),!,
  hashtable_pairs(Graph,Props).
into_pairs_now(Props, Key=Value):- compound(Props),
  props_kv(Props,Key,Value).

