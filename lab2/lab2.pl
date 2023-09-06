% 1. (DONE) Escribe un predicado prod(L,P) que signifique: “P es el producto de los elementos de la lista de enteros dada L”. Debe poder generar la P y también comprobar una P dada.
prod([], 1).
prod([X|L1], P):- prod(L1, P1), P is P1*X.


% 2. (DONE) Escribe un predicado pescalar(L1,L2,P) que signifique: “P es el producto escalar de los vectores L1 y L2”, donde los vectores se representan como listas de enteros.
% El predicado debe fallar si los dos vectores tienen longitudes distintas.
pescalar([], [], 0).
pescalar([X|L1], [Y|L2], P):- pescalar(L1, L2, Q), P is Q + X*Y.


% 3. (DONE) Representando conjuntos con listas sin repeticiones, escribe predicados para las operaciones de intersección y unión de conjuntos dados.
inters([], _, []).
inters([X|L1], L2, [X|L3]):- member(X, L2), !, inters(L1, L2, L3).
inters([X|L1], L2, L3):- not(member(X, L2)), inters(L1, L2, L3).

union([], L, L).
union([X|L1], L2, L3):- member(X, L2), !, union(L1, L2, L3).
union([X|L1], L2, [X|L3]):- union(L1, L2, L3). % "not(member(X, L2))" equivalent a "\+ member(X, L2)"


% 4. (DONE) Usando append, escribe un predicado para calcular el último elemento de una lista dada, y otro para calcular la lista inversa de una lista dada.
last([X], X):- !.
last([_|L], Y):- last(L, Y).

inv([], []):- !.
inv([X|L1], L2):- inv(L1, L3), append(L3, [X], L2).


% 5. (DONE) Escribe un predicado fib(N,F) que signifique: “F es el N-ésimo número de Fibonacci para la N dada”. Estos números se definen así: fib(1) = 1, fib(2) = 1, y si N > 2 entonces fib(N) = fib(N − 1) + fib(N − 2).
fib(1, 1):- !.
fib(2, 1):- !.
fib(N, F):- N > 2, N1 is N-1, N2 is N-2, fib(N1, F1), fib(N2, F2), F is F1 + F2.


% 6. (DONE) Escribe un predicado dados(P,N,L) que signifique: “la lista L expresa una manera de sumar P puntos lanzando N dados”. Por ejemplo: si P es 5 y N es 2, una solución sería [1,4] (nótese que la longitud de L es N). Tanto P como N vienen instanciados. El predicado debe ser capaz de generar todas las soluciones posibles.
dados(P, N, L):- genera_daus(N, L), msort(L, L), sum_list(L, S), S is P, write(L), nl, fail.

genera_daus(0, []):- !.
genera_daus(N, [D|L1]):- between(1, 6, D), N1 is N-1, genera_daus(N1, L1).


% 7. (DONE) Escribe un predicado suma_demas(L) que, dada una lista de enteros L, se satisface si existe algún elemento en L que es igual a la suma de los demás elementos de L, y falla en caso contrario.
suma_demas(L):- permutation(L, [X|P]), sumL(P, X), !.

sumL([], 0):- !.
sumL([X|L], N):- sumL(L, N2), N is N2 + X.


% 8. (DONE) Escribe un predicado suma_ants(L) que, dada una lista de enteros L, se satisface si existe algún elemento en L que es igual a la suma de los elementos anteriores a él en L, y falla en caso contrario.
suma_ants(L):- inv(L, [X|P]), sumaR(P, X).

sumaR([], 0):- !.
sumaR([X|L], N):- sumL([X|L], N) ; sumaR(L, X).


% 9. (DONE) Escribe un predicado card(L) que, dada una lista de enteros L, escriba la lista que, para cada elemento de L, dice cuántas veces aparece este elemento en L.
card([]):- write("[]").
card([X|L]):- write('['), writeEl1(X, L), delete(X, L, L2), card2(L2) , write(']'), nl.

card2([]):- !.
card2([X|L]):- writeEl2(X, L), delete(X, L, L2), card2(L2).

writeEl1(X, L):- count(X, L, N), !, N2 is N+1, write([X, N2]).
writeEl2(X, L):- count(X, L, N), !, N2 is N+1, write(','), write([X, N2]).

% N son las veces que aparece el elemento X en la lista L
count(_, [], 0):- !.
count(X, [X|L], N):- count(X, L, N2), !, N is N2+1.
count(X, [_|L], N):- count(X, L, N).

% L2 es la lista L1 habiendo eliminado todos los elementos X.
delete(_, [], []):- !.
delete(X, [X|L1], L2):- delete(X, L1, L2), !.
delete(X, [Y|L1], [Y|L2]):- delete(X, L1, L2).


% 10. (DONE) Escribe un predicado esta_ordenada(L) que signifique: “la lista L de números enteros está ordenada de menor a mayor”.
esta_ordenada([]):- !.
esta_ordenada([_]):- !.
esta_ordenada([X,Y|L]):- X =< Y, esta_ordenada([Y|L]).


% 11. (DONE) Escribe un predicado ord(L1,L2) que signifique: “L2 es la lista de enteros L1 ordenada de menor a mayor”.
% Por ejemplo: si L1 es [4,5,3,3,2] entonces L2 será [2,3,3,4,5]. Hazlo en una lı́nea, usando sólo los predicados permutacion y esta_ordenada.
ord(L1, L2):- permutation(L1, L2), esta_ordenada(L2), !.


% 12. Escribe un predicado diccionario(A,N) que, dado un alfabeto A de sı́mbolos y un natural N, escriba todas las palabras de N sı́mbolos, por orden alfabético (el orden alfabético es según el alfabeto A dado).
palabra(_,0,''):- !.
palabra(L, N, X):- member(X1, L), N1 is N - 1, palabra(L, N1, X2), atom_concat(X1,X2,X).

diccionario(A,N):- palabra(A,N, X), write(X), write(' '), fail.


% 13. Escribe un predicado palindromos(L) que, dada una lista de letras L, escriba todas las permutaciones de sus elementos que sean palı́ndromos (capicúas). Por ejemplo, con la consulta palindromos([a,a,c,c]) se escribe [a,c,c,a] y [c,a,a,c].
/*
palindromos(L1):- permutation(L1, L2), esPalindromo(L2), write(L2), fail.

esPalindromo([]):- !.
esPalindromo([_]):- !.
esPalindromo([X|L]):- inv(L, [X|L2]), esPalindromo(L2).
*/

remove_letter(_,[], []).
remove_letter(X,[X|L], L):- !.
remove_letter(X,[Y|L], L1):- remove_letter(X, L, L2), append([Y], L2, L1).

remove(_,[], []):- !.
remove(X, [X|L], L1):- remove(X,L, L1), !.
remove(X,[Y|L], L1):- remove(X,L, L2), append([Y], L2, L1).

diff([],[]):- !.
diff(L, L1):- member(X, L),!, remove(X, L, L2), diff(L2, L3), append([X], L3, L1).

count13(_, [], 0).
count13(X,[X|L],P):- !, count13(X, L, P1), P is P1 + 1.
count13(X,[_|L], P):- count13(X, L, P).

make_palindromo([], []).
make_palindromo([X], [X]).
make_palindromo(L, L1):- diff(L, L6),
                        member(X, L6),
                        count13(X, L, C),
                        C >= 2,
                        remove_letter(X, L, L2),
                        remove_letter(X, L2, L3),
                        make_palindromo(L3, L4),
                        append([X], L4, L5),
                        append(L5, [X], L1).


% 14.
suma14([],[],[],C,C).
suma14([X1|L1],[X2|L2],[X3|L3],Cin,Cout) :-
	X3 is (X1 + X2 + Cin) mod 10,
	C  is (X1 + X2 + Cin) //  10,
	suma14(L1,L2,L3,C,Cout).

    send_more_money :-

        L = [S, E, N, D, M, O, R, Y, _, _],
        permutation(L, [0,1,2,3,4,5,6,7,8,9]),
        suma14([D, N, E, S], [E, R, O, M], [Y, E, N, O], 0, M),

        write('S = '), write(S), nl,
        write('E = '), write(E), nl,
        write('N = '), write(N), nl,
        write('D = '), write(D), nl,
        write('M = '), write(M), nl,
        write('O = '), write(O), nl,
        write('R = '), write(R), nl,
        write('Y = '), write(Y), nl,
        write('  '), write([S,E,N,D]), nl,
        write('  '), write([M,O,R,E]), nl,
        write('-------------------'), nl,
        write([M,O,N,E,Y]), nl.


% 15.
der(X, X, 1):-!.
der(C, _, 0) :- number(C).
der(A+B, X, A1+B1) :- der(A, X, A1), der(B, X, B1).
der(A-B, X, A1-B1) :- der(A, X, A1), der(B, X, B1).
der(A*B, X, A*B1+B*A1) :- der(A, X, A1), der(B, X, B1).
der(sin(A), X, cos(A)*B) :- der(A, X, B).
der(cos(A), X, -sin(A)*B) :- der(A, X, B).
der(e^A, X, B*e^A) :- der(A, X, B).
der(ln(A), X, B*1/A) :- der(A, X, B).


pasoSubExpression(A,B):-
	A=..[F|La],
	append(L1,[Ea|L2],La),
	unpaso(Ea,Eb),
	append(L1,[Eb|L2],Lb),
	B=..[F|Lb].

simplifica(A,B):- unpaso(A,C),!, simplifica(C,B).
	simplifica(A,A):-!.

	unpaso(A,B):- pasoSuperior(A,B).
	unpaso(A,B):- pasoSubExpression(A,B).



	pasoSuperior(A+0,A).
	pasoSuperior(0+B,B).
	pasoSuperior(A-0,A).
	pasoSuperior(0-B,-B).
	pasoSuperior(A-A,0).
	pasoSuperior(A*1,A).
	pasoSuperior(1*B,B).
	pasoSuperior(A/1,A).
	pasoSuperior(_*0,0).
	pasoSuperior(0*_,0).
	pasoSuperior(0/_,0).
	pasoSuperior(A+B,N ):- integer(A), integer(B), N is A+B.
	pasoSuperior(A*B,N ):- integer(A), integer(B), N is A*B.
	pasoSuperior(A-B,N ):- integer(A), integer(B), N is A-B.
	pasoSuperior(A//B,N):- integer(A), integer(B), B\=0, N is A//B.



% 16.
remove_f(_,[], []).
remove_f(X,[X|L], L):- !.
remove_f(X,[Y|L], L1):- remove_f(X, L, L2), append([Y], L2, L1).

path([],[],_):- !.
path(Dom, L, X):- member(f(X,Y), Dom), remove_f(f(X,Y),Dom, NDom), path(NDom, Rec, Y), !, append([f(X,Y)], Rec, L).
path(Dom, L, Y):- member(f(X,Y), Dom), remove_f(f(X,Y),Dom, NDom), path(NDom, Rec, X), !, append([fr(Y,X)], Rec, L).


path_ult(Dom):- findall(X, (member(f(X,Y), Dom); member(f(Y,X), Dom)), List), member(X, List), path(Dom,L,X), !, write(L).


% 17.
p:- readclauses(F), sat([],F).
p:- write('UNSAT'),nl.
sat(I,[]):- write('IT IS SATISFIABLE. Model: '), write(I),nl,!.
sat(I,F):-
decision_lit(F,Lit), % Select unit clause if any; otherwise, an arbitrary one.
simplif(Lit,F,F1), % Simplifies F. Warning: may fail and cause backtracking
sat( [Lit|I] , F1 ).
 %caso unit clause
decision_lit([X|_],Lit):- member(Lit, X), length(X, 1), !.
%caso donde es aleatorio
decision_lit([X|_],Lit):- member(Lit, X).


simplif(_, [], []):- !.
%si es miembro de una clausula
simplif(Lit,[X|F], F1):- member(Lit, X), simplif(Lit, F, F1), !.


%caso donde -Lit es miembro
simplif(Lit,[X|F], F1):-
    PositiveLit is 0 - Lit,
    member(PositiveLit, X),!,
    quitar_literal(PositiveLit, X, X1),
    simplif(Lit, F, F2),
    append([X1], F2, F1).

%caso donde el simbolo no es miembro
simplif(Lit,[X|F], F1):-
    simplif(Lit, F, F2),
    append([X], F2, F1).

quitar_literal(_, [], []):- !.
quitar_literal(Lit, [Lit|F], F1):- quitar_literal(Lit, F, F1),!.
quitar_literal(Lit, [X| F], F1):-quitar_literal(Lit, F, F2), append([X], F2, F1).


% 18.
num18(X):- between(1, 10, X).
smoker:-
    num18(NCNF1), num18(NCSF1), num18(SCNF1), num18(SCSF1),
    10 is NCNF1 + NCSF1 + SCNF1 + SCSF1,

    SCNF1 / (SCNF1 + NCNF1) < SCSF1 /(SCSF1 +NCSF1),

    num18(NCNF2), num18(NCSF2), num18(SCNF2), num18(SCSF2),
    10 is NCNF2 + NCSF2 + SCNF2 + SCSF2,

    SCNF2 / (SCNF2 + NCNF2) < SCSF2 /(SCSF2 +NCSF2),

    (SCNF1 + SCNF2) / (SCNF1 + SCNF2 + NCNF1+ NCNF2) > (SCSF1 + SCSF2) /(SCSF1 + SCSF2 + NCSF1 + NCSF2),

    write([NCNF1, NCSF1, SCNF1, SCSF1, ' ', NCNF2, NCSF2, SCNF2, SCSF1]), nl, fail.


% 19.
maq(L,C,M):- maqrecursivo(L,C,M, 1).

%caso en que se cumple
maqrecursivo(L,C,M, S):- length(L,N), randomPer(N,S, M), pescalar19(L,M, C), !.
%si no hay entonces pasa al siguiente nivel y sigue buscando.
maqrecursivo(L,C,M,S):- S1  is S + 1, maqrecursivo(L,C,M, S1).


randomPer(0,0,[]):- !.

randomPer(0,S,[S]):-!.
randomPer(N, S, L):- between(0,S, X), N1 is N - 1, S1 is S - X, randomPer(N1,S1, L2), append([X], L2, L).


pescalar19([],[], 0).
pescalar19([X|L1],[Y|L2], P):- pescalar19(L1,L2,P2), P is P2 + X*Y.


% 20. Write in Prolog a predicate flatten(L,F) that “flattens” (cast.: “aplana”) the list F as in the example:
flatten([],[]):- !.
flatten([X|L], F):- is_list(X), !,flatten(X, L1), flatten(L, L2), append(L1,L2, F).
flatten([X|L], F):- flatten(L, L2), append([X],L2, F).

% 21.
log(_, 1, 0) :- !.
log(B,N, L):-  between(0, N, L), L1 is L + 1 , N >= B**L, N < B**L1, !.


% 23.
%% Example:
numbers([2,5,7,-2,2,9,3,4,1]).
maxSum(6).

%% subsetWithRest(L, Subset, Rest) holds
%% if Subset is a subset of L and Rest is the rest of the elements.
subsetWithRest([], [], []).
subsetWithRest([X|L1], L2, [X|L3]):- subsetWithRest(L1, L2, L3).
subsetWithRest([X|L1], [X|L2], L3):- subsetWithRest(L1, L2, L3).

%% maxSubset(K, L, Sm) holds
%% if Sm is a subset of numbers of L such that
%% it sums at most K
%% and if we try to add any other element, the sum exceeds K.
maxSubset(K, L, Sm):-
    subsetWithRest(L, Sm, Rest),
    sum_list(Sm, X), K >= X,
    member(Y, Rest),
    append(Sm, Y, Sm2),
    sum_list(Sm2, X2), X2 > K.

main :-
    numbers(L), maxSum(K),
    maxSubset(K, L, Sm),
    write(Sm), nl, fail.
main:- halt.



% 25. Complete the following predicate in prolog.
nthRoot(N,K,R):- between(0, K, R), N1 is R**N, N2 is (R+1)**N, N1 =< K, N2 > K, !.

% 26. Complete the following predicate in prolog.
allSSSS(L):- subconjunto26(L, S),
            length(S, Size),
            Size >= 1,
            sum26(S, V),
            I is sqrt(V),
            I2 is float(floor(I)),
            I is I2,
            write(V-S),
            nl,
            fail.

sum26([], 0).
sum26([X|L], P):- sum26(L, X1), P is X+X1.

subconjunto26([],[]):-!.
subconjunto26([X|L],[X|S]):- subconjunto26(L,S).
subconjunto26([_|L],S):- subconjunto26(L,S).
