:- use_module(library(clpfd)).

ejemplo(0,   26, [1,2,5,10] ).  % Solution: [1,0,1,2]
ejemplo(1,  361, [1,2,5,13,17,35,157]).

main:- 
    ejemplo(0, Amount, Coins),
    nl, write('Paying amount '), write(Amount), write(' using the minimal number of coins of values '), write(Coins), nl,nl,
    length(Coins, N),

    % 1a. Variables
    length(Vars, N),

    % 1b. Domain
    Vars ins 0..Amount,

    % 2. Constraints
    expr(Vars, Coins, Sum),
    Sum #= Amount,
    exprSuma(Vars, ExprSuma),

    % 3. Labeling
    labeling([min(ExprSuma)], Vars),

    % 4. Write Solution
    nl, write(Vars), nl,nl, halt.

expr([], [], 0).
expr([V|Vars], [C|Coins], C*V+Sum):- expr(Vars, Coins, Sum).

exprSuma([], 0).
exprSuma([V|Vars], V+Sum):- exprSuma(Vars, Sum).
