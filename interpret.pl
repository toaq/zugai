:- [parse].
:- [dict].

% mai4 sa poq4 sa poq4 da
% il(da,∃(p,∧(poq(p),∃(q,∧(poq(q),mai(p,q))))))

% scope:
% scope{ quant: [∃-q, ∃-p] % (in reverse)
%      , refs: refs{[poq]: q, [aq]: p, [ho]: q} }

free_var(W, Varname) :-
  atomic(W),
  sub_atom(W, 0, 1, _, Letter),
  (Sub = ""; between(1, inf, I), number_string(I, Sub)),
  atom_concat(Letter, Sub, Candidate),
  b_getval(scopes, Ss),
  \+ (member(S, Ss), member(_-Candidate, S.quant)),
  !, Varname = Candidate.

free_var(verb(W-_), Var) :- free_var(W, Var).
free_var(nonserial(name_verb(W-_),_,_), Var) :- free_var(W, Var).
free_var(nonserial(oiv(W-_),_,_), Var) :- free_var(W, Var).
free_var(nonserial(lu(_),_,_), Var) :- free_var(l, Var).
free_var(nonserial(mo(_),_,_), Var) :- free_var(m, Var).
free_var(nonserial(V), Var) :- free_var(V, Var).
free_var(serial(A,_), Var) :- free_var(A, Var).
free_var(vp(V), Var) :- free_var(V, Var).

% example: vp_words(vp(serial(nonserial(verb(de-4)), vp(nonserial(verb(poq-4))))), [de,poq]).
vp_words(vp(nonserial(verb(V-_))), [V]).
vp_words(vp(serial(nonserial(verb(H-_)), W)), [H|T]) :- vp_words(W, T).

push_scope :-
  b_getval(scopes, S),
  b_setval(scopes, [scope{quant:[], refs:refs{}} | S]).

push_quant(Q-V) :-
  b_getval(scopes, [Top|S]),
  b_setval(scopes, [Top.put(quant, [Q-V|Top.quant])|S]).

pop_scope(Out) :-
  b_getval(scopes, [Out|S]),
  b_setval(scopes, S).

interpret(discourse(D), Formulas) :-
  b_setval(scopes, []),
  maplist(interpret, D, Formulas).

interpret(sentence(sentence_connector(C),Statement,illocution(I)), Formula) :-
  interpret(Statement, F1),
  (I = [] -> F2=F1 ; F2=il(I,F1)),
  (C = [] -> F3=F2 ; F3=sc(C,F2)),
  Formula = F3.

interpret(statement(Prenex, Pred), Formula) :-
  push_scope,
  interpret(Prenex, _Topics),
  interpret(Pred, F),
  pop_scope(scope{quant: _Qs, refs: _V}),
  % todo: apply quants
  Formula = F.

interpret(prenex(Ts), Topics) :-
  % todo
  maplist(interpret_prenex_term, Ts, Results),
  findall(X, member(topic(X), Results), Topics).

interpret(predication(_P, _Ts), _Formula) :-
  % todo
  !.

interpret_prenex_term(_Term, _Result) :- !. %todo
