indent(String, Indented) :- string_concat("  ", String, Indented).

ansi(2, "208").
ansi(3, "220").
ansi(4, "40").
ansi(5, "44").
ansi(6, "27").
ansi(7, "99").
ansi(8, "160").

layout(Term, [Line]) :- var(Term), !, Line = "?".
layout(Term, [Line]) :- atomic(Term), !, term_string(Term, Line).

layout(X-Y, [Line]) :-
  atomic(X), atomic(Y), !, ansi(Y, A),
  format(string(Line), '\x1b[38;5;~wm~w-~w\x1b[0m', [A,X,Y]).

layout(Term, Lines) :-
  is_list(Term), !,
  maplist(layout, Term, Layouts),
  append(Layouts, ArgLines),
  maplist(indent, ArgLines, Indented),
  append(["["|Indented], [], Lines).

layout(Term, Lines) :-
  Term =..[Name|Args], !,
  ( Args = [Inner], layout(Inner, [Single]), !,
    format(string(Wrapped), "~w: ~w", [Name, Single]),
    Lines = [Wrapped]

  ; maplist(layout, Args, Layouts), !,
    append(Layouts, ArgLines),
    maplist(indent, ArgLines, Indented),
    format(string(First), "~w:", [Name]),
    append([First|Indented], [], Lines)
  ).

pprint(Term) :- layout(Term, Lines), maplist(writeln, Lines).

tone_dia(2, "\u0301").
tone_dia(3, "\u0308").
tone_dia(4, "\u0309").
tone_dia(5, "\u0302").
tone_dia(6, "\u0300").
tone_dia(7, "\u0303").
tone_dia(8, "").

toned(V-T, S) :-
  tone_dia(T, Dia),
  string_concat("\\1", Dia, Dia1),
  re_replace('([aeiou])', Dia1, V, S0),
  unicode_nfc(S0, S).

terminals(V-T, S) :- toned(V-T, S).
terminals(Term, S) :-
  Term =.. [_|Args],
  findall(Y, (member(X, Args), terminals(X, Y), Y\=""), Ys),
  writeln(Ys),
  atomics_to_string(Ys, " ", S).

quantifier_symbol(sa, "∃").
quantifier_symbol(tu, "∀").
quantifier_symbol(tushi, "∀¹").
quantifier_symbol(tuq, "Λ").
quantifier_symbol(sia, "∄").
quantifier_symbol(ke, "ι").
quantifier_symbol(hoi, "hoi").
quantifier_symbol(baq, "baq").
quantifier_symbol(hi, "¿").
quantifier_symbol(ja, "λ").

% format logic terms
lformat(Term, String) :- atomic(Term), !, atom_string(Term, String).
lformat(Term, String) :- is_list(Term), !, maplist(lformat, Term, Strings), atomics_to_string(Strings, ". ", String).
lformat(qu(Q, V, T), S) :- !, quantifier_symbol(Q, Qs), lformat(T, St), format(string(S), "~w~w: ~w", [Qs,V,St]).
lformat(qu(Q, V, R, T), S) :- !, quantifier_symbol(Q, Qs), lformat(T, St), lformat(R, Sr), format(string(S), "[~w~w: ~w] ~w", [Qs,V,Sr,St]).
lformat(pr(P, Args), S) :-
  !, lformat(P, Sp),
  maplist(lformat, Args, Ss),
  atomics_to_string(Ss, ",", A),
  format(string(S), "~w(~w)", [Sp,A]).
lformat(ev(E, T), S) :- !, lformat(E, Se), lformat(T, St), format(string(S), "~w={~w}", [Se, St]).
lformat(il(da-8, T), S) :- !, lformat(T, St), format(string(S), "~w.", [St]).
lformat(il(I, T), S) :- !, toned(I, Si), lformat(T, St), format(string(S), "~w(~w)", [Si, St]).
lformat(name_verb(V, Q), S) :- !, lformat(V, Sv), terminals(Q, Sq), format(string(S), "~w\"~w\"", [Sv, Sq]).
lformat(X, S) :- !, format(string(S), "「~w」", [X]).
