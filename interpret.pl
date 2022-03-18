:- [parse].
:- [dict].

% mai4 sa poq4 sa poq4 da
% il(da,∃(p,∧(poq(p),∃(q,∧(poq(q),mai(p,q))))))

% scope:
% scope{ quant: [∃-p, ∃-q]
%      , vars: vars{poq: q, aq: p, ho: q} }
%

push_scope :-
  b_getval(scopes, S),
  b_setval(scopes, [scope{quant:[], vars:[]} | S]).

pop_scope(Out) :-
  b_getval(scopes, [Out|S]),
  b_setval(scopes, S).

interpret(discourse(D), Formulas) :-
  b_setval(scopes, []),
  maplist(interpret, D, Fss),
  append(Fss, Formulas).

interpret(sentence(sentence_connector(C),Statement,illocution(I)), Formulas) :-
  interpret(Statement, [F1]),
  (I = [] -> F2=F1 ; F2=il(I,F1)),
  (C = [] -> F3=F2 ; F3=sc(C,F2)),
  Formulas = [F3].

interpret(statement(Prenex, Pred), Formulas) :-
  push_scope,
  interpret(Prenex, _Topics),
  interpret(Pred, F),
  pop_scope(scope{quant: _Qs, vars: _V}),
  % todo: apply quants
  Formulas = [F].

interpret(prenex(_Ts), Topics) :-
  % todo
  Topics = [].

interpret(predication(_P, _Ts), _Formula) :-
  % todo
  !.
