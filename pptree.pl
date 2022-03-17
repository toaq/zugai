% https://gist.github.com/nicoabie/d23fa87eb16d02d9ee57b0b41fdc977f

pptree(Term) :-
  functor(Term, Name, Arity),
  write(Name),
  write('('),
  pptree_children(Term, 1, Arity, 1).

% prints atomic siblings without last
pptree_children(Term, ChildN, Total, Level) :-
  ChildN < Total,
  arg(ChildN, Term, Child),
  atomic(Child),
  !,
  write(Child),
  write(','),
  NextChildN is ChildN + 1,
  pptree_children(Term, NextChildN, Total, Level).

% prints last atomic sibling
pptree_children(Term, ChildN, Total, _Level) :-
  ChildN =:= Total,
  arg(ChildN, Term, Child),
  atomic(Child),
  !,
  write(Child),
  write(')').

% prints non atomic siblings without last
pptree_children(Term, ChildN, Total, Level) :-
  ChildN < Total,
  arg(ChildN, Term, Child),
  functor(Child, Name, Arity),
  nl,
  Spaces is Level * 2,
  NextLevel is Level + 1,
  tab(Spaces),
  write(Name),
  write('('),
  pptree_children(Child, 1, Arity, NextLevel),
  NextChildN is ChildN + 1,
  pptree_children(Term, NextChildN, Total, Level).


% prints last non atomic sibling
pptree_children(Term, ChildN, Total, Level) :-
  ChildN =:= Total,
  arg(ChildN, Term, Child),
  functor(Child, Name, Arity),
  nl,
  Spaces is Level * 2,
  NextLevel is Level + 1,
  tab(Spaces),
  write(Name),
  write('('),
  pptree_children(Child, 1, Arity, NextLevel),
  write(')').

