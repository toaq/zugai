:- use_module(library(http/json)).
:- use_module(library(unicode)).

dictionary(Dict) :-
  open("../dictionary/dictionary.json", read, Stream),
  json_read_dict(Stream, Dict),
  close(Stream), !.

atom_norm(Atom, Norm) :-
  atom_string(Atom, String),
  string_lower(String, Lower),
  re_replace("i"/g, "ı", Lower, Norm).

lookup_verb(VerbAtom, Entry) :-
  dictionary(Dict), !,
  atom_norm(VerbAtom, Norm),
  ( member(Entry, Dict),
    Entry.toaq = Norm, !
  ; % todo: this is stupid as hell
    re_replace("ı"/g, "i", Norm, Dotted),
    re_replace('([aeiou])', "\\1\u0301", Dotted, T2),
    unicode_nfc(T2, T2n),
    atom_string(T2n, Final),
    member(Entry, Dict),
    Entry.toaq = T,
    string(Final),
    T = Final,
    Entry.toaq = Final, !).

is_pronoun(VerbAtom) :-
  lookup_verb(VerbAtom, Entry),
  Entry.type == "pronoun".

anaphora(Verb, Pronoun) :-
  lookup_verb(Verb, Entry),
  Entry.pronominal_class = C,
  atom_string(Pronoun, C).
