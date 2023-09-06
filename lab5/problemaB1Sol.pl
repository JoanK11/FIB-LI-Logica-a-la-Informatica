main:- EstadoInicial = [0, 0], EstadoFinal = [0, 4],
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

unPaso(1, [X, Y], [0, Y]):- X \= 0.     % Vaciamos el cubo de 5 litros (si no está vacío)
unPaso(1, [X, Y], [X, 0]):- Y \= 0..    % Vaciamos el cubo de 8 litros (si no está vacío)
unPaso(1, [X, Y], [5, Y]):- X \= 5.     % Llenamos el cubo de 5 litros (si no está lleno)
unPaso(1, [X, Y], [X, 8]):- Y \= 8.     % Llenamos el cubo de 8 litros (si no está lleno)

unPaso(1, [X, Y], [0, Z]):- Z is X+Y, Z =< 8. % Vertemos el contenido de X a Y (todo el contenido de X cabe en Y).
unPaso(1, [X, Y], [Z, 8]):- Z is X+Y-8. % Vertemos el contenido de X a Y (todo el contenido de X no cabe en Y).

unPaso(1, [X, Y], [Z, 0]):- Z is X+Y, Z =< 5. % Vertemos el contenido de Y a X (todo el contenido de Y cabe en X).
unPaso(1, [X, Y], [5, Z]):- Z is X+Y-5. % Vertemos el contenido de Y a X (todo el contenido de Y no cabe en X).
