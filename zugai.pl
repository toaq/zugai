:- use_module(library(dcg/high_order)).
% ?- phrase(discourse(S), [gi-4, da-8]), !.
% S = dicourse([sentence(sentence_connector(), statement(prenex(), predication(predicate(vp(nonserial(verb(gi)))), terms([]))), illocution(da-8))]).

discourse(dicourse(Ds)) --> sequence(discourse_item, Ds).
discourse_item(D) --> sentence(D) | fragment(D).

sentence(sentence(C,S,I)) -->
    (sentence_connector(C) | { C=sentence_connector() }),
    statement(S),
    (illocution(I) | { I = illocution() }).

fragment(fragment(F)) --> prenex(F) | terms1(F).

statement(statement(P,Q)) -->
    (prenex(P) | { P=prenex() }),
    predication(Q).

% complementizers([la, ma, tio]).
% complementizer(Tone, complementizer(C)) --> [C-Tone], { complementizers(Cs), member(C, Cs) }.

prenex(prenex(Ts)) --> terms1(Ts), end_prenex.
end_prenex --> [bi-8].

predication(predication(P,Ts)) --> predicate(P), terms(Ts).
predicate(predicate(P)) --> vp(4, P).

terms(terms(Ts)) --> sequence(term, Ts).
term(term(T)) --> vp(2, T).
term(term(T)) --> dp(T).
% TODO: "arguments", adverbials, prepps, content clauses

terms1(T) --> terms(T), { T = terms([_|_]) }.

vp(Tone, vp(C)) --> serial(Tone, C).
vp(Tone, vp(C)) --> nonserial(Tone, C).

serial(Tone, serial(A,B)) --> nonserial(Tone, A), vp(4, B).
nonserial(Tone, nonserial(name_verb,V,N)) --> name_verb(Tone, V), !, (predicate(N) | term(N)), ([ga-8]|[]).
nonserial(Tone, nonserial(oiv,V,N)) --> oiv(Tone, V), !, term(N), ([ga-8]|[]).
nonserial(Tone, nonserial(moteo,D)) --> [mo-Tone], !, discourse(D), [teo-8].
nonserial(Tone, nonserial(lu,S)) --> [lu-Tone], statement(S), ([ky-8]|[]).
nonserial(Tone, nonserial(N)) --> verb(Tone, N).

name_verbs([mi, miru, shu]).
name_verb(Tone, V) --> [V-Tone], { name_verbs(Vs), member(V, Vs) }.

oivs([po, jei, mea]).
oiv(Tone, V) --> [V-Tone], { oivs(Vs), member(V, Vs) }.

verb(Tone, verb(W)) --> [W-Tone], { \+ function_word(W) }.

dp(dp(D,VP)) --> determiner(D), vp(4, VP).
determiner(determiner(D)) --> [D-8], { determiners(Ds), member(D, Ds) }.

sentence_connector(sentence_connector(C-8)) --> [C-8], { member(C, [je, keo, tiu, nhu]) }.
illocution(illocution(I-Tone)) --> [I-Tone], { illocutions(Is), member(I, Is) }.

sentence_connectors([je, keo, tiu, nhu]).
determiners([sa, tu, tuq, tushi, sia, ke, hoi, baq, hi, ja]).
illocutions([da, moq, nha, shou]).

function_word(bi).
function_word(ga).
function_word(ki).
function_word(kio).
function_word(ky).
function_word(teo).
function_word(P) :- sentence_connectors(Ps), member(P, Ps).
function_word(P) :- determiners(Ps), member(P, Ps).
function_word(P) :- illocutions(Ps), member(P, Ps).
