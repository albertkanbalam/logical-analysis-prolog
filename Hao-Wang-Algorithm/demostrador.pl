%-------------------------------------------------------------------
%
% Implementación del algoritmo Hao-Wang.
%
%-------------------------------------------------------------------

% Representación de las fórmulas:
% atom(X), and(P, Q), or(P, Q), imp(P,Q), syss(P, Q), neg(P)

demostrador(P, Q) :-
	interseccion(P, Q), !.
demostrador(P, Q) :-
	contieneNoAtom(P),
	ordena(P, [P1|Ps]),
	simplifica(P1, R),
	ejecutaP(R, Ps, Q).
demostrador(P, Q) :-
	soloAtoms(Q),
	interseccion(P, Q).
demostrador(P, Q) :-
	contieneNoAtom(Q),
	ordena(Q, [Q1|Qs]),
	simplifica(Q1, R),
	ejecutaQ(R, P, Qs).

% Verifica si una lista contiene solamente fórmulas atómicas.
soloAtoms([]).
soloAtoms([atom(_)|T]) :-
	soloAtoms(T).

ejecutaQ(P, or(S1, S2), Qs) :-
	demostrador(P, [S1|[S2|Qs]]), !.
ejecutaQ(P, and(S1, S2), Qs) :-
	demostrador(P, [S1|Qs]),
	demostrador(P, [S2|Qs]), !.
ejecutaQ(P, neg(S), Qs) :-
	demostrador([S|P], Qs), !.

ejecutaP(and(S1, S2), Ps, Q) :-
	demostrador([S1|[S2|Ps]], Q), !.
ejecutaP(or(S1, S2), Ps, Q) :-
	demostrador([S1|Ps], Q), 
	demostrador([S2|Ps], Q), !.
ejecutaP(neg(S), Ps, Q) :-
	demostrador(Ps, [S|Q]), !.


% Ordena/2 Coloca al principio las fórmulas atómicas
% y al final las NO atómicas.
ordena(L, R) :-
	separa(L, NoAt, At),
	concatena(NoAt, At, R).

% Predicados auxiliares para construir ordena/2.
concatena([], L, L).
concatena([X|L1], L2, [X|L3]) :-
	concatena(L1, L2, L3).

separa(L, NoAtomicas, Atomicas) :-
	noAtoms(L, [], NoAtomicas),
	atoms(L, [], Atomicas).


atoms([], L, L).
atoms([atom(P)|T], I, L) :-
	insertar(atom(P), I, R),
	atoms(T, R, L), !.
atoms([_|T], I, L) :-
	atoms(T, I, L).

noAtoms([], L, L).
noAtoms([H|T], I, L) :-
	esNOATOM(H),
	insertar(H, I, R),
	noAtoms(T, R, L), !.
noAtoms([_|T], I, L) :-
	noAtoms(T, I, L).

insertar(X, L1, L2) :-
        select(X, L2, L1).

select(X, [X|L], L).
select(X, [Y|L1], [Y|L2]) :-
    select(X, L1, L2).
    
%%%%%%%%%%%%%%%%%%%%% Fin auxiliares de ordena/2 %%%%%%%%%%%%%%%%%%%%

% Predicado auxiliar para verificar si una fórmula NO es atómica.
esNOATOM(imp(_, _)).
esNOATOM(sys(_, _)).
esNOATOM(and(_, _)).
esNOATOM(or(_, _)).
esNOATOM(neg(_)).

% Comprobar si una lista P contiene almenos
% una fórmula NO atómica.
contieneNoAtom(L) :-
	pertenece(P, L),
	esNOATOM(P).

% Intersección verifica si dos listas contienen algún elemento en común.
% NOTA: Anotar obervaciones al utilizar interseccion/2.
interseccion([X|_],L) :-
	pertenece(X, L), !.
interseccion([_|T], L) :-
	interseccion(T, L).

% Predicado auxiliar para verificar la pertenencia de un elemento.
pertenece(X, [X|_]).
pertenece(X, [_|T]) :- pertenece(X, T).

% Simplifica/2 transforma una fórmula F1 en una fórmula F2
% en la cual solo hay símbolos de negación, conjunción y disyunción.
simplifica(atom(P), atom(P)).
simplifica(neg(atom(P)), neg(atom(P))).
simplifica(neg(neg(atom(P))), atom(P)).
simplifica(and(F1, F1), R) :-
	simplifica(F1, R).
simplifica(or(F1, F1), R) :-
	simplifica(F1, R).
simplifica(and(F1, F2), and(R, S)) :-
	simplifica(F1, R),
	simplifica(F2, S).
simplifica(or(F1, F2), or(R, S)) :-
	simplifica(F1, R),
	simplifica(F2, S).
simplifica(neg(and(F1, F2)), or(R, S)) :-
	simplifica(neg(F1), R),
	simplifica(neg(F2), S).
simplifica(neg(or(F1, F2)), and(R, S)) :-
	simplifica(neg(F1), R),
	simplifica(neg(F2), S).
simplifica(neg(imp(F1, neg(F2))), and(R, S)) :-
	simplifica(F1, R),
	simplifica(F2, S).
simplifica(neg(imp(F1, F2)), and(R, S)) :-
	simplifica(F1, R),
	simplifica(neg(F2), S).
simplifica(neg(syss(F1, F2)), T) :-
	simplifica(syss(R, S), T),
	simplifica(neg(F1), R),
	simplifica(F2, S).
simplifica(imp(neg(F1), F2), or(R, S)) :-
	simplifica(F1, R),
	simplifica(F2, S).
simplifica(imp(F1, F2), or(R, S)) :-
	simplifica(neg(F1), R),
	simplifica(F2, S).
simplifica(syss(F1, F2), or(R, S)) :-
	simplifica(and(F1, F2), R),
	simplifica(and(neg(F1), neg(F2)), S).
