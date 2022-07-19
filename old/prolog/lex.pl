:- use_module(library(unicode)).

odds(Xs, Odds) :-
  bagof(X, I^(nth0(I, Xs, X), I mod 2 > 0), Odds).

untone(Word, Untoned) :-
  re_replace("[\\d\u0300-\u030f]"/g, "", Word, S),
  atom_string(Untoned, S).

tone(Word, 2) :- re_match("[2\u0301]", Word), !.
tone(Word, 3) :- re_match("[3\u0308]", Word), !.
tone(Word, 4) :- re_match("[4\u0309]", Word), !.
tone(Word, 5) :- re_match("[5\u0302]", Word), !.
tone(Word, 6) :- re_match("[6\u0300]", Word), !.
tone(Word, 7) :- re_match("[7\u0303]", Word), !.
tone(_Word, 8) :- !.

decompose(Word, U-T) :- untone(Word, U), tone(Word, T).

lex(String, Words) :-
  unicode_nfd(String, S1),
  string_lower(S1, S2),
  re_replace("ı"/g, "i", S2, S3),
  re_split("[\\w\\d\u0300-\u030f]+", S3, S4),
  odds(S4, S5),
  maplist(decompose, S5, Words).
