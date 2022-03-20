:- [parse].
:- [dict].

% logical terms:
% A B C...  % variables
% il(da,stmt)  % illocution
% sc(je,stmt)  % sentence connector
% pr(mai,[P,Q])  % predication, 1st term is a single toaq verb, rest are variables
% la([X,Y],pr(mai,[Y,X]))  % lambda
% qu(sa,P,stmt)  % quantifier
% qu(sa,P,pr(poq,P),stmt)  % restricted quantifier
% ev(E,stmt)  % e is an event of stmt being the case
% co(ru,stmt,stmt)  % connective of statements

% mai4 sa poq4 sa poq4 da
% il(da,∃(p,∧(poq(p),∃(q,∧(poq(q),mai(p,q))))))

% scope:
% scope{ event: E  % variable for this prop as an event
%      , quant: [qu(sa,Q,pr(poq,Q)), qu(sa,P,pr(poq,P))] % (in reverse)
%      , refs: refs{poq: Q, aq: P, ho: Q} }

% get all free vars in a term T  oops this is term_variables/2
%fvs(T, []) :- atomic(T), !.
%fvs(T, [T]) :- var(T), !.
%fvs(T, Vs) :- T =.. [_|Args], !, maplist(fvs, Args, Vss), append(Vss, Vs).

free_var(W, _Varname) :-
  atomic(W),
  !.
  % sub_atom(W, 0, 1, _, Letter),
  % (Sub = ""; between(1, inf, I), number_string(I, Sub)),
  % atom_concat(Letter, Sub, Candidate),
  % b_getval(scopes, Ss),
  % \+ (member(S, Ss), member(_-Candidate, S.quant)),
  % !, Varname = Candidate.

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

push_scope(Ev) :-
  b_getval(scopes, S),
  b_setval(scopes, [scope{event: Ev, quant:[], refs:refs{}} | S]).

push_quant(Q) :-
  b_getval(scopes, Sc),
  writeln(Sc),
  b_getval(scopes, [Top|S]),
  b_setval(scopes, [Top.put(quant, [Q|Top.quant])|S]).

set_ref(Words,V) :-
  b_getval(scopes, [Top|S]),
  b_setval(scopes, [Top.put(refs, Top.refs.put(Words,V))|S]).

pop_scope(Out) :-
  b_getval(scopes, [Out|S]),
  b_setval(scopes, S).

:- discontiguous interpret/2.
interpret(discourse(D), Formulas) :-
  b_setval(scopes, []),
  maplist(interpret, D, Formulas).

interpret(sentence(sentence_connector(C),Statement,illocution(I)), Formula) :-
  interpret(Statement, F1),
  (I = [] -> F2=F1 ; F2=il(I,F1)),
  (C = [] -> F3=F2 ; F3=sc(C,F2)),
  Formula = F3.

wrap_quant(qu(Q,V,R), F, qu(Q,V,R,F)).

interpret(statement(Prenex, Pred), Formula) :-
  push_scope(Ev),
  interpret(Prenex, _),
  interpret(Pred, F),
  pop_scope(scope{event: Ev, quant: Qs, refs: _Rs}),
  writeln(Qs),
  foldl(wrap_quant, Qs, F, Fq),

  % todo: apply quants
  Formula = qu(sa,Ev,ev(Ev,Fq)).

interpret(prenex(_Ts), _) :- !.

interpret(predication(Predicate, Terms), Formula) :-
  % todo
  interpret(Predicate, PP),
  PP = la(Args,Body),
  interpret(Terms, Positionals),
  % writeln(pp-PP),
  % writeln(po-Positionals),
  % writeln(b-Body),
  Args = Positionals, Body = Formula.

interpret(predicate(vp(nonserial(verb(W-4)))), Lambda) :-
  Lambda = la(Args,pr(W,Args)).

extract_positionals([], []).
extract_positionals([positional(P)|Xs], [P|Ys]) :- !, extract_positionals(Xs, Ys).
extract_positionals([_|Xs], Ys) :- !, extract_positionals(Xs, Ys).

interpret(terms(Ts), Positionals) :-
  maplist(interpret, Ts, Results),
  extract_positionals(Results, Positionals),
  % findall(P, member(positional(P), Results), Positionals),
  % writeln(pos-Positionals), %todo adverbials
  true.

interpret(term(np(T)), positional(Res)) :- interpret(np(T), Res).
interpret(term(ap(T)), adverbial(Res)) :- interpret(ap(T), Res).
interpret(term(pp(T)), adverbial(Res)) :- interpret(pp(T), Res).

interpret(np(dp(Det,Vp)), Result) :- interpret(dp(Det,Vp), Result).
interpret(dp(determiner(D-8),vp(nonserial(verb(W-4)))), Result) :-
  push_quant(qu(D,Result,pr(W,[Result]))),
  set_ref(W, Result).
