:- use_module(library(clpfd)).

%ejemplo(_, Big, [S1...SN]): how to fit all squares of sizes S1...SN in a square of size Big?
ejemplo(0,  3,[2,1,1,1,1,1]).
ejemplo(1,  4,[2,2,2,1,1,1,1]).
ejemplo(2,  5,[3,2,2,2,1,1,1,1]).
ejemplo(3, 19,[10,9,7,6,4,4,3,3,3,3,3,2,2,2,1,1,1,1,1,1]).
ejemplo(4, 40,[24,16,16,10,9,8,8,7,7,6,6,3,3,3,2,1,1]).   %<-- aquest ja costa bastant de resoldre...

%Aquests dos ultims son molt durs!!! Poden no sortir-te amb aquesta tècnica!!
ejemplo(5,112,[50,42,37,35,33,29,27,25,24,19,18,17,16,15,11,9,8,7,6,4,2]).
ejemplo(6,175,[81,64,56,55,51,43,39,38,35,33,31,30,29,20,18,16,14,9,8,5,4,3,2,1]).


%% Possible output solution for example 3:
%%  10 10 10 10 10 10 10 10 10 10  9  9  9  9  9  9  9  9  9
%%  10 10 10 10 10 10 10 10 10 10  9  9  9  9  9  9  9  9  9
%%  10 10 10 10 10 10 10 10 10 10  9  9  9  9  9  9  9  9  9
%%  10 10 10 10 10 10 10 10 10 10  9  9  9  9  9  9  9  9  9
%%  10 10 10 10 10 10 10 10 10 10  9  9  9  9  9  9  9  9  9
%%  10 10 10 10 10 10 10 10 10 10  9  9  9  9  9  9  9  9  9
%%  10 10 10 10 10 10 10 10 10 10  9  9  9  9  9  9  9  9  9
%%  10 10 10 10 10 10 10 10 10 10  9  9  9  9  9  9  9  9  9
%%  10 10 10 10 10 10 10 10 10 10  9  9  9  9  9  9  9  9  9
%%  10 10 10 10 10 10 10 10 10 10  7  7  7  7  7  7  7  2  2
%%   6  6  6  6  6  6  4  4  4  4  7  7  7  7  7  7  7  2  2
%%   6  6  6  6  6  6  4  4  4  4  7  7  7  7  7  7  7  2  2
%%   6  6  6  6  6  6  4  4  4  4  7  7  7  7  7  7  7  2  2
%%   6  6  6  6  6  6  4  4  4  4  7  7  7  7  7  7  7  2  2
%%   6  6  6  6  6  6  4  4  4  4  7  7  7  7  7  7  7  2  2
%%   6  6  6  6  6  6  4  4  4  4  7  7  7  7  7  7  7  1  1
%%   3  3  3  3  3  3  4  4  4  4  3  3  3  3  3  3  3  3  3
%%   3  3  3  3  3  3  4  4  4  4  3  3  3  3  3  3  3  3  3
%%   3  3  3  3  3  3  1  1  1  1  3  3  3  3  3  3  3  3  3


main:- 
    ejemplo(3, Big, Sides),
    nl, write('Fitting all squares of size '), write(Sides), write(' into big square of size '), write(Big), nl,nl,
    length(Sides, N),

    % 1a. Variables
    length(RowVars, N), % get list of N prolog vars: Row coordinates of each small square
    length(ColVars, N),
    append(RowVars, ColVars, Vars),

    % 1b. Domini
    Vars ins 1..Big,

    % 2. Constraints
    limitarDomini(Big, RowVars, Sides),
    limitarDomini(Big, ColVars, Sides),
    %insideBigSquare(N, Big, Sides, RowVars),
    %insideBigSquare(N, Big, Sides, ColVars),
    nooverlapping(N, Sides, RowVars, ColVars),

    % 3.Labeling
    labeling([ff], Vars),

    % 4. Write Solution
    displaySol(Big, Sides, RowVars, ColVars), halt.

limitarDomini(_, [], []).
limitarDomini(Big, [V|Vars], [S|Sides]):- V #=< Big-S+1, limitarDomini(Big, Vars, Sides).

insideBigSquare(_, _, [], []).
insideBigSquare(N, Big, [S|Sides], [V|Vars]):- V+S-1 #=< Big, insideBigSquare(N, Big, Sides, Vars).

nooverlapping(_, [], [], []).
nooverlapping(N, [S|Sides], [R|Rows], [C|Cols]):- generateConstraints(S, R, C, Sides, Rows, Cols), nooverlapping(N, Sides, Rows, Cols).

generateConstraints(_, _, _, [], [], []).
generateConstraints(S, R, C, [S2|Sides], [R2|Rows], [C2|Cols]):-
    SRfi #= R+S-1, SCfi #= C+S-1,
    S2Rfi #= R2+S2-1, S2Cfi #= C2+S2-1,
    (S2Rfi #< R #\/ R2 #> SRfi #\/ % La fila del segon/primer (acaba abans #\/ comença després) que la fila del primer/segon
    S2Cfi #< C #\/ C2 #> SCfi), % La columna del segon/primer (acaba abans #\/ comença després) que la columna del primer/segon
    generateConstraints(S, R, C, Sides, Rows, Cols).

displaySol(N, Sides, RowVars, ColVars):-
    between(1, N, Row), nl, between(1, N, Col),
    nth1(K, Sides, S),
    nth1(K, RowVars, RV),    RVS is RV+S-1,     between(RV, RVS, Row),
    nth1(K, ColVars, CV),    CVS is CV+S-1,     between(CV, CVS, Col),
    writeSide(S), fail.
displaySol(_,_,_,_):- nl,nl,!.

writeSide(S):- S<10, write('  '),write(S),!.
writeSide(S):-       write(' ' ),write(S),!.

