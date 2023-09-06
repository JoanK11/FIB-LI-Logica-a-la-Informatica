main:- EstadoInicial = [[3, 3], [0, 0], 1], EstadoFinal = [[0, 0], [3, 3], 2],
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

% 1-2 misionero(s) pasan de la primera orilla a la segunda
unPaso(1, [[M1A, CA], [M1B, CB], 1], [[M2A, CA], [M2B, CB], 2]):- between(1, 2, X),
    M2A is M1A - X, M2A >= 0,
    M2B is M1B + X,
    (M2A >= CA ; M2A is 0),
    (M2B >= CB ; M2B is 0).

% 1-2 caníbal(es) pasan de la primera orilla a la segunda
unPaso(1, [[MA, C1A], [MB, C1B], 1], [[MA, C2A], [MB, C2B], 2]):- between(1, 2, X),
    C2A is C1A - X, C2A >= 0,
    C2B is C1B + X,
    (MA >= C2A ; MA is 0),
    (MB >= C2B ; MB is 0).

% 1 misionero y 1 caníbal pasan de la primera orilla a la segunda
unPaso(1, [[M1A, C1A], [M1B, C1B], 1], [[M2A, C2A], [M2B, C2B], 2]):-
    M2A is M1A - 1, M2A >= 0, M2B is M1B + 1,
    C2A is C1A - 1, C2A >= 0, C2B is C1B + 1,
    (M2A >= C2A ; M2A is 0),
    (M2B >= C2B ; M2B is 0).

% ----------------------------------------------------------------------------------

% 1-2 misionero(s) pasan de la segunda orilla a la primera
unPaso(1, [[M1A, CA], [M1B, CB], 2], [[M2A, CA], [M2B, CB], 1]):- between(1, 2, X),
    M2B is M1B - X, M2B >= 0,
    M2A is M1A + X,
    (M2A >= CA ; M2A is 0),
    (M2B >= CB ; M2B is 0).

% 1-2 caníbal(es) pasan de la segunda orilla a la primera
unPaso(1, [[MA, C1A], [MB, C1B], 2], [[MA, C2A], [MB, C2B], 1]):- between(1, 2, X),
    C2B is C1B - X, C2B >= 0,
    C2A is C1A + X,
    (MA >= C2A ; MA is 0),
    (MB >= C2B ; MB is 0).

% 1 misionero y 1 caníbal pasan de la segunda orilla a la primera
unPaso(1, [[M1A, C1A], [M1B, C1B], 2], [[M2A, C2A], [M2B, C2B], 1]):-
    M2B is M1B - 1, M2B >= 0, M2A is M1A + 1,
    C2B is C1B - 1, C2B >= 0, C2A is C1A + 1,
    (M2A >= C2A ; M2A is 0),
    (M2B >= C2B ; M2B is 0).
