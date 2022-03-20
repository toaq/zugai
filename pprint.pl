indent(String, Indented) :-
  format(string(Indented), "  ~w", [String]).

ansi(2, "208").
ansi(3, "220").
ansi(4, "40").
ansi(5, "44").
ansi(6, "27").
ansi(7, "99").
ansi(8, "160").

layout(Term, [Line]) :-
  var(Term), !,
  Line = "?".

layout(Term, [Line]) :-
  atomic(Term), !,
  format(string(Line), '~w', [Term]).

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

% format logic terms
lformat(Term, String) :- atomic(Term), !, atom_string(Term, String).
lformat(Term, String) :- is_list(Term), !, maplist(lformat, Term, Strings), atomics_to_string(Strings, ". ", String).
lformat(qu(Q, V, T), S) :- !, lformat(T, St), format(string(S), "~w ~w: ~w", [Q,V,St]).
lformat(qu(Q, V, R, T), S) :- !, lformat(T, St), lformat(R, Sr), format(string(S), "~w ~w [~w]: ~w", [Q,V,Sr,St]).
lformat(pr(P, Args), S) :-
  !, lformat(P, Sp),
  maplist(lformat, Args, Ss),
  atomics_to_string(Ss, ",", A),
  format(string(S), "~w(~w)", [Sp,A]).
lformat(ev(E, T), S) :- !, lformat(E, Se), lformat(T, St), format(string(S), "~w={~w}", [Se, St]).
lformat(_, "unk") :- !.
