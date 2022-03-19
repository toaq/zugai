:- [lex].
:- [parse].
:- [interpret].
:- [pprint].

% lex, parse, pretty-print
lpp(Toaq) :- lex(Toaq, Tokens), phrase(discourse(D), Tokens), pprint(D), !.
