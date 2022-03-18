:- [parse].
:- [dict].

% mai4 sa poq4 sa poq4 da
% il(da,∃(poq1,∧(poq(poq1),∃(poq2,∧(poq(poq2),mai(poq1,poq2))))))


interpret(discourse(D), Ls) :-
  maplist(interpret, D, Lss),
  append(Lss, Ls).

interpret(sentence(sentence_connector(C),Statement,illocution(I)), Ls) :-
  interpret(Statement, [L1]),
  (I = [] -> L2=L1 ; L2=il(I,L1)),
  (C = [] -> L3=L2 ; L3=sc(C,L2)),
  Ls = [L3].
