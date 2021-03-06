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
%      , refs: refs{poq: Q, aq: P, ho: Q}
%      , is_subject: no }  % are we parsing the subject (aq) rn?

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
  b_setval(scopes, [scope{event: Ev, quant:[], refs:refs{}, is_subject: no} | S]).

push_quant(Q) :-
  b_getval(scopes, [Top|S]),
  b_setval(scopes, [Top.put(quant, [Q|Top.quant])|S]).

set_ref(Words,V) :-
  b_getval(scopes, [Top|S]),
  b_setval(scopes, [Top.put(refs, Top.refs.put(Words,V))|S]).

set_is_subject(YesOrNo) :-
  b_getval(scopes, [Top|S]),
  b_setval(scopes, [Top.put(is_subject, YesOrNo)|S]).

is_subject :-
  b_getval(scopes, [Top|_]),
  Top.is_subject == yes.

resolve_ref(Words,V) :-
  b_getval(scopes, Scopes),
  member(S, Scopes),
  V = S.refs.get(Words), !.

pop_scope(Out) :-
  b_getval(scopes, [Out|S]),
  b_setval(scopes, S).

:- discontiguous interpret/2.
interpret(discourse(D), Formulas) :-
  b_setval(scopes, []),
  maplist(interpret, D, Formulas).

interpret(sentence(sentence_connector(C),Statement,illocution(I)), Formula) :-
  interpret(Statement, F1),
  (I = [] -> F2=il(da-8,F1) ; F2=il(I,F1)),
  (C = [] -> F3=F2 ; F3=sc(C,F2)),
  Formula = F3.

wrap_quant(qu(Q,V,R), F, qu(Q,V,R,F)).

interpret(statement(Prenex, Pred), Formula) :- interpret(statement(Prenex, Pred), Formula, []).
interpret(statement(Prenex, Pred), Formula, Hoas) :-
  push_scope(Ev),
  maplist(set_ref(hoa), Hoas),
  interpret(Prenex, _),
  interpret(Pred, F),
  pop_scope(Scope),
  Ev = Scope.event,
  Qs = Scope.quant,

  foldl(wrap_quant, Qs, F, Fq),
  term_variables(Fq, Occurs),
  ((member(V, Occurs), V == Ev) -> Formula = qu(sa,Ev,ev(Ev,Fq)) ; Formula = Fq).

interpret(prenex(_Ts), _) :- !.

interpret(predication(Predicate, Terms), Formula) :-
  % todo
  interpret(Predicate, PP),
  PP = la(Args,Body),
  set_is_subject(yes), % next positional term gets "aq"
  interpret(Terms, Positionals),
  % writeln(pp-PP),
  % writeln(po-Positionals),
  % writeln(b-Body),
  Args = Positionals, Body = Formula.

interpret(predicate(Vp), Lambda) :- interpret(Vp, Lambda).
interpret(vp(nonserial(verb(W-_))), Lambda) :- Lambda = la(Args,pr(W,Args)).
interpret(vp(nonserial(name_verb(W-_),N,_End)), Lambda) :-
    Lambda = la(Args,pr(name_verb(W,N),Args)).
interpret(vp(nonserial(lu(_Lu), Stmt, _End)), Lambda) :-
    interpret(Stmt, Body, [Hoa]),
    Lambda = la([Hoa],Body).


vp_word(vp(nonserial(verb(W-_))), W). % todo serials etc

extract_positionals([], []).
extract_positionals([positional(P)|Xs], [P|Ys]) :- !, extract_positionals(Xs, Ys).
extract_positionals([_|Xs], Ys) :- !, extract_positionals(Xs, Ys).

interpret(terms(Ts), Positionals) :-
  maplist(interpret, Ts, Results),
  extract_positionals(Results, Positionals),
  true.

interpret(term(np(T)), positional(Res)) :- interpret(np(T), Res), set_is_subject(no).
interpret(term(ap(T)), adverbial(Res)) :- interpret(ap(T), Res).
interpret(term(pp(T)), adverbial(Res)) :- interpret(pp(T), Res).

interpret(np(dp(Det,Vp)), Result) :- interpret(dp(Det,Vp), Result).
interpret(np(bound(Vp)), Result) :-
  ( vp_word(Vp, W), resolve_ref(W, Result), !
  ; interpret(dp(determiner(ke-8), Vp), Result)
  ).

interpret(dp(determiner(D-8),Vp), Result) :-
  interpret(Vp, Lam),
  Lam = la([Result], Body),
  push_quant(qu(D,Result, Body)),
  ( vp_word(Vp, W) ->
  ( set_ref(W, Result),
    ((is_subject -> A=aq ; anaphora(W, A)) -> set_ref(A, Result) ; true)
  ) ; true).
