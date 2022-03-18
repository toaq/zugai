:- use_module(library(http/json)).

dictionary(Dict) :-
  open("dictionary/dictionary.json", read, Stream),
  json_read_dict(Stream, Dict),
  close(Stream), !.

lookup_verb(VerbAtom, Entry) :-
  dictionary(Dict), !,
  atom_string(VerbAtom, String),
  string_lower(String, Lower),
  re_replace("i"/g, "ı", Lower, Dotless),
  member(Entry, Dict),
  Entry.toaq = Dotless, !.
