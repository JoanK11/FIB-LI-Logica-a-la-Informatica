main:- EstadoInicial = [[1, 2, 5, 8], [], 1], EstadoFinal = [[], [1, 2, 5, 8], 2],
    between(1, 1000, CosteMax), % Buscamos solución de coste 0; si no, de 1, etc.
    camino(CosteMax, EstadoInicial, EstadoFinal, [EstadoInicial], Camino),
    reverse(Camino, Camino1), write(Camino1), write(' con coste '), write(CosteMax), nl, halt.

camino(0, E, E, C, C). % Caso base: cuando el estado actual es el estado final.
camino(CosteMax, EstadoActual, EstadoFinal, CaminoHastaAhora, CaminoTotal):-
    CosteMax > 0,
    unPaso(CostePaso, EstadoActual, EstadoSiguiente), % En B.1 y B.2, CostePaso es 1.
    \+member(EstadoSiguiente, CaminoHastaAhora),
    CosteMax1 is CosteMax - CostePaso,
    camino(CosteMax1, EstadoSiguiente, EstadoFinal, [EstadoSiguiente|CaminoHastaAhora], CaminoTotal).

% Una persona pasa del primer extremo al segundo
unPaso(C, [L1A, L1B, 1], [L2A, L2B, 2]):- member(P, L1A),
    delete(L1A, P, L2A), C is P,
    append(L1B, [P], L2BAux), sort(L2BAux, L2B).

% Una persona pasa del segundo extremo al primero
unPaso(C, [L1A, L1B, 2], [L2A, L2B, 1]):- member(P, L1B),
    delete(L1B, P, L2B), C is P,
    append(L1A, [P], L2AAux), sort(L2AAux, L2A).

% Dos personas pasan del primer extremo al segundo
unPaso(C, [L1A, L1B, 1], [L2A, L2B, 2]):- member(P1, L1A), member(P2, L1A), P1 < P2,
    delete(L1A, P1, L2AAux), delete(L2AAux, P2, L2A), C is max(P1, P2),
    append(L1B, [P1], L2BAux1), append(L2BAux1, [P2], L2BAux2), sort(L2BAux2, L2B).

% Dos personas pasan del segundo extremo al primero
unPaso(C, [L1A, L1B, 2], [L2A, L2B, 1]):- member(P1, L1B), member(P2, L1B), P1 < P2,
    delete(L1B, P1, L2BAux), delete(L2BAux, P2, L2B), C is max(P1, P2),
    append(L1A, [P1], L2AAux1), append(L2AAux1, [P2], L2AAux2), sort(L2AAux2, L2A).
