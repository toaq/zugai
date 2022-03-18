indent(String, Indented) :-
  format(string(Indented), "  ~w", [String]).

layout(Term, [Line]) :-
  \+ compound(Term), !,
  format(string(Line), '~w', [Term]).

layout(X-Y, [Line]) :-
  \+ compound(X),
  \+ compound(Y), !,
  format(string(Line), '"~w-~w"', [X,Y]).

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
