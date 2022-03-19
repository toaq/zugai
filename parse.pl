:- [pprint].
:- [lex].
:- use_module(library(dcg/high_order)).

% ?- phrase(discourse(S), [gi-4, da-8]), !.

% TODO: la4 as sentence start, more connectives, free modifiers
discourse(discourse(Ds)) --> sequence(discourse_item, Ds).
discourse_item(D) --> (sentence(D), !) | fragment(D).

sentence(sentence(C,S,I)) -->
    (sentence_connector(C) | { C=sentence_connector([]) }),
    statement(S),
    (illocution(I) | { I = illocution([]) }).

fragment(fragment(F)) --> prenex(F) | terms1(F).

statement(statement(P,Q)) --> (prenex(P) | { P=prenex([]) }), predication(4, Q).
statement1(statement(P,Q)) --> (prenex(P) | { P=prenex([]) }), predication1(4, Q).

prenex(prenex(Ts,end(Bi))) --> terms1(Ts), end_prenex(Bi).
end_prenex(bi-8) --> [bi-8].
to(to-8) --> [to-8].

predication(Tone, compp(C,S)) --> complementizer(Tone, C), !, statement1(S).
predication(Tone, P) --> predication1(Tone, P).
predication1(Tone, predication(P,Ts)) --> predicate(Tone, P), terms(Ts).
predicate(Tone, predicate(P)) --> vp(Tone, P).

terms1(T) --> terms(T), { T = terms([_|_]) }.
terms(terms(Ts)) --> sequence(term, Ts).
term(term(T)) --> argu(T).
term(term(T)) --> advp(T).
term(term(T)) --> prepp(T).

advp(advp(A)) --> vp(7, A).
prepp(prepp(P,C)) --> vp(6, P), argu(C).

argu(conn_argu(X,C,Y)) --> argu0(X), connective(C), !, argu(Y).
argu(conn_argu(to(T1),C,X,to(T2),Y)) --> to(T1), connective(C), argu(X), to(T2), argu(Y).
argu(T) --> argu0(T).
argu0(T) --> argu1(A), ((rel(P), { T=argrel(A,P) }) | {T=A}).
argu1(argu(T)) --> vp(2, T).
argu1(argu(T)) --> dp(T).
argu1(argu(T)) --> cc(T).

rel(rel(P,end(Cy))) --> predication(3, P), end(cy,Cy).
cc(cc(P,end(Cy))) --> predication(5, P), end(cy,Cy).

vp(Tone, conn_vp(C,X,Y)) --> vp0(Tone, X), connective(C), vp(4, Y).
vp(Tone, conn_vp(C,X,Y)) --> [to-8], connective(C), vp(Tone, X), [to-8], vp(4, Y).
vp(Tone, V) --> vp0(Tone, V).
vp0(Tone, vp(C)) --> serial(Tone, C).
vp0(Tone, vp(C)) --> nonserial(Tone, C).

% parse an optional terminator:
end(Particle, E) --> [Particle-8], {E=Particle-8}, ! | {E=[]}.

serial(Tone, serial(A,B)) --> nonserial(Tone, A), !, vp(4, B).
nonserial(Tone, nonserial(name_verb(V),N,end(Ga))) --> name_verb(Tone, V), !, (vp(4, N) | term(N)), end(ga,Ga).
nonserial(Tone, nonserial(oiv(V),N,end(Ga))) --> oiv(Tone, V), argu(N), end(ga,Ga).
nonserial(Tone, nonserial(mo(mo-Tone),D,end(teo-8))) --> [mo-Tone], !, discourse(D), [teo-8].
nonserial(Tone, nonserial(lu(lu-Tone),S,end(Ky))) --> [lu-Tone], statement(S), end(ky,Ky).
nonserial(Tone, nonserial(N)) --> verb(Tone, N).

name_verbs([mi, miru, shu]).
name_verb(Tone, V-Tone) --> [V-Tone], { name_verbs(Vs), member(V, Vs) }.

oivs([po, jei, mea]).
oiv(Tone, V-Tone) --> [V-Tone], { oivs(Vs), member(V, Vs) }.

verb(Tone, verb(W-Tone)) --> [W-Tone], { \+ function_word(W) }.

dp(dp(D,VP)) --> determiner(D), vp(4, VP).

determiners([sa, tu, tuq, tushi, sia, ke, hoi, baq, hi, ja]).
determiner(determiner(D-8)) --> [D-8], { determiners(Ds), member(D, Ds) }.

complementizers([la, ma, tio]).
complementizer(Tone, complementizer(C-Tone)) --> [C-Tone], { complementizers(Cs), member(C, Cs) }.

sentence_connectors([je, keo, tiu, nhu]).
sentence_connector(sentence_connector(C-8)) --> [C-8], { sentence_connectors(Cs), member(C, Cs) }.

interjections([ifu, aja, ahi, ume, ufu, a, ua, obe, upa, buzy, oai, ubai, eni, aiba, obe, e, nho, zi, jadi, kiji, jiki]).
interjection(interjection(I-Tone)) --> [I-Tone], { interjections(Is), member(I, Is) }.

illocutions([da, ka, moq, nha, ba, shou]).
illocution(illocution(I-Tone)) --> [I-Tone], { illocutions(Is), member(I, Is) }.

connectives([ra, ri, ru, ro, roi]).
connective(connective(C-Tone)) --> [C-Tone], { connectives(Cs), member(C, Cs) }.

particle(bi).
particle(cy).
particle(ga).
particle(ju).
particle(ki).
particle(kio).
particle(ky).
particle(teo).
particle(P) :- sentence_connectors(Ps), member(P, Ps).
particle(P) :- determiners(Ps), member(P, Ps).
particle(P) :- illocutions(Ps), member(P, Ps).

function_word(lu).
function_word(mo).
function_word(W) :- particle(W).
function_word(W) :- name_verbs(Ws), member(W, Ws).
function_word(W) :- oivs(Ws), member(W, Ws).
function_word(W) :- complementizers(Ws), member(W, Ws).
