:- [pprint].
:- [lex].
:- use_module(library(dcg/high_order)).

% ?- phrase(discourse(S), [gi-4, da-8]), !.

% TODO: "ru bi"
discourse(discourse(Ds)) --> sequence(discourse_item, Ds).
discourse_item(D) --> (sentence(D), !) | fragment(D) | (freemod(F), !, {D=free_item(F)}).

sentence(sentence(C,S,I)) -->
    (sentence_connector(C) | { C=sentence_connector([]) }),
    statement(S),
    (illocution(I) | { I = illocution([]) }).

fragment(fragment(F)) --> prenex(F) | terms1(F).

statement(statement(P,Q)) --> (prenex(P) | { P=prenex([]) }), predication(4, Q).
statement_nocomp(statement(P,Q)) --> (prenex(P) | { P=prenex([]) }), predication1(4, Q).

prenex(prenex(Ts,end(Bi))) --> terms1(Ts), t(bi,8,Bi).
to(T) --> t(to,8,T).

predication(Tone, conn_predication(X,na(Na),C,Y)) --> predicationC(Tone, X), t(na,8,Na), connective(C), predication(4, Y).
predication(Tone, conn_predication(to(T1),C,X,to(T2),Y)) --> to(T1), connective(C), predication(Tone, X), to(T2), predication(4, Y).
predication(Tone, P) --> predicationC(Tone, P).
predicationC(Tone, compp(C,S)) --> complementizer(Tone, C), !, statement_nocomp(S).
predicationC(Tone, P) --> predication1(Tone, P).
predication1(Tone, predication(P,Ts)) --> predicate(Tone, P), terms(Ts).
predicate(Tone, predicate(P)) --> vp(Tone, P).

terms1(T) --> terms(T), { T = terms([_|_]) }.
terms(terms(Ts)) --> sequence(term, Ts).
term(term(T)) --> np(T).
term(term(T)) --> ap(T).
term(term(T)) --> pp(T).

ap(conn_ap(X,C,Y)) --> apC(X), connective(C), ap(Y).
ap(conn_ap(to(T1),C,X,to(T2),Y)) --> to(T1), connective(C), ap(X), to(T2), ap(Y).
ap(A) --> apC(A).
apC(ap(A)) --> vp(7, A).

pp(conn_pp(X,C,Y)) --> ppC(X), connective(C), pp(Y).
pp(conn_pp(to(T1),C,X,to(T2),Y)) --> to(T1), connective(C), pp(X), to(T2), pp(Y).
pp(A) --> ppC(A).
ppC(pp(P,C)) --> prep(P), np(C).

prep(conn_prep(X,C,Y)) --> prepC(X), connective(C), prep(Y).
prep(conn_prep(to(T1),C,X,to(T2),Y)) --> to(T1), connective(C), prep(X), to(T2), prep(Y).
prep(P) --> prepC(P).
prepC(prep(P)) --> vp(6, P).

np(conn_np(X,C,Y)) --> npC(X), connective(C), np(Y).
np(conn_np(to(T1),C,X,to(T2),Y)) --> to(T1), connective(C), np(X), to(T2), np(Y).
np(T) --> npC(T).
npC(focused(F,T)) --> focus_prefix(F), npF(T).
npC(T) --> npF(T).
npF(T) --> npR(A), ((rel(P), { T=argrel(A,P) }) | {T=A}).
npR(np(bound(T))) --> vp(2, T).
npR(np(T)) --> dp(T).
npR(np(T)) --> cc(T).

rel(conn_rel(X,C,Y)) --> relC(X), connective(C), rel(Y).
rel(conn_rel(to(T1),C,X,to(T2),Y)) --> to(T1), connective(C), rel(X), to(T2), rel(Y).
rel(R) --> relC(R).
relC(rel(P,end(Cy))) --> predication(3, P), end(cy,Cy).
cc(cc(P,end(Cy))) --> predication(5, P), end(cy,Cy).

vp(Tone, conn_vp(X,C,Y)) --> vpC(Tone, X), connective(C), vp(4, Y).
vp(Tone, conn_vp(to(T1),C,X,to(T2),Y)) --> to(T1), connective(C), vp(Tone, X), to(T2), vp(4, Y).
vp(Tone, V) --> vpC(Tone, V).
vpC(Tone, vp(V)) --> serial(Tone, V).
vpC(Tone, vp(V)) --> nonserial(Tone, V).

% parse an optional terminator:
end(Particle, E) --> t(Particle,8,E), ! | {E=[]}.

serial(Tone, serial(A,B)) --> nonserial(Tone, A), !, vp(4, B).
nonserial(Tone, nonserial(name_verb(V),N,end(Ga))) --> name_verb(Tone, V), (vp(4, N) | term(N)), end(ga,Ga).
nonserial(Tone, nonserial(oiv(V),N,end(Ga))) --> oiv(Tone, V), np(N), end(ga,Ga).
nonserial(Tone, nonserial(mo(Mo),D,end(Teo))) --> t(mo,Tone,Mo), discourse(D), t(teo,8,Teo).
nonserial(Tone, nonserial(lu(Lu),S,end(Ky))) --> t(lu,Tone,Lu), statement(S), end(ky,Ky).
nonserial(Tone, nonserial(N)) --> verb(Tone, N).

name_verbs([mi, miru, shu]).
name_verb(Tone, T) --> t(V,Tone,T), { name_verbs(Vs), member(V, Vs) }.

oivs([po, jei, mea]).
oiv(Tone, T) --> t(V,Tone,T), { oivs(Vs), member(V, Vs) }.

verb(Tone, verb(T)) --> t(W,Tone,T), { \+ function_word(W) }.

dp(dp(D,VP)) --> determiner(D), vp(4, VP).

focus_prefixes([ku, bei, juaq, mao, tou]).
focus_prefix(focus_prefix(T)) --> t(D,8,T), { focus_prefixes(Ds), member(D, Ds) }.

determiners([sa, tu, tuq, tushi, sia, ke, hoi, baq, hi, ja]).
determiner(determiner(T)) --> t(D,8,T), { determiners(Ds), member(D, Ds) }.

complementizers([la, ma, tio]).
complementizer(Tone, complementizer(T)) --> t(C,Tone,T), { complementizers(Cs), member(C, Cs) }.

sentence_connectors([je, keo, tiu, nhu]).
sentence_connector(sentence_connector(T)) --> t(C,8,T), { sentence_connectors(Cs), member(C, Cs) }.

illocutions([da, ka, moq, ba, nha, shou, go]).
illocution(illocution(T)) --> t(I,_,T), { illocutions(Is), member(I, Is) }.

connectives([ra, ri, ru, ro, roi]).
connective(connective(T)) --> t(C,8,T), { connectives(Cs), member(C, Cs) }.

t(Word,Tone,free(Word-Tone,Fs)) --> [Word-Tone], sequence(freemod, Fs), { Fs = [_|_] }, !.
t(Word,Tone,Word-Tone) --> [Word-Tone], !.

freemod(F) --> interjection(F).
freemod(F) --> vocative(F).
freemod(F) --> incidental(F).
freemod(F) --> parenthetical(F).

interjections([ifu, aja, ahi, ume, ufu, a, ua, obe, upa, buzy, oai, ubai, eni, aiba, obe, e, nho, zi, jadi, kiji, jiki]).
interjection(interjection(I-Tone)) --> [I-Tone], { interjections(Is), member(I, Is) }.

vocative(vocative(Hu,Np)) --> t(hu,8,Hu), !, np(Np).
incidental(incidental(Ju,Se)) --> t(ju,8,Ju), !, sentence(Se).
parenthetical(parenthetical(Kio,D,Ki)) --> t(kio,8,Kio), !, discourse(D), t(ki,8,Ki).

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
