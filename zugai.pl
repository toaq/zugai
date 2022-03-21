:- [dict].
:- [lex].
:- [parse].
:- [interpret].
:- [pprint].

plug(T) :-
  term_variables(T, Vs),
  length(Vs, N),
  findall(X, (between(1, N, I), J is 64+I, atom_codes(X, [J])), Xs),
  maplist(=, Vs, Xs).

% lex, parse, pretty-print
lpp(Toaq) :- lex(Toaq, Tokens), phrase(discourse(D), Tokens), pprint(D).
lpi(Toaq) :- lex(Toaq, Tokens), phrase(discourse(D), Tokens), interpret(D, I), writeln(I), plug(I), lformat(I, S), writeln(S).
