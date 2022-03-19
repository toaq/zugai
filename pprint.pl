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
