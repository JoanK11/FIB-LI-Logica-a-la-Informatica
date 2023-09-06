:- use_module(library(clpfd)).

%% A (6-sided) "letter dice" has on each side a different letter.
%% Find four of them, with the 24 letters abcdefghijklmnoprstuvwxy such
%% that you can make all the following words: bake, onyx, echo, oval,
%% gird, smug, jump, torn, luck, viny, lush, wrap.

%Some helpful predicates:

word( [b,a,k,e] ).
word( [o,n,y,x] ).
word( [e,c,h,o] ).
word( [o,v,a,l] ).
word( [g,i,r,d] ).
word( [s,m,u,g] ).
word( [j,u,m,p] ).
word( [t,o,r,n] ).
word( [l,u,c,k] ).
word( [v,i,n,y] ).
word( [l,u,s,h] ).
word( [w,r,a,p] ).

num(X, N):- nth1( N, [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,r,s,t,u,v,w,x,y], X ).

main:-
    length(D1, 6),
    length(D2, 6),
    length(D3, 6),
    length(D4, 6),

    % 1a. Variables
    append([D1, D2, D3, D4], D),

    % 1b. Domain
    D ins 1..24,
    all_distinct(D),

    % 2. Constraints
    append(X1, R1, D1), length(X1, 1),
    all_pairs(D1, R1, PairsD1),
    append(X2, R2, D2), length(X2, 1),
    all_pairs(D2, R2, PairsD2),
    append(X3, R3, D3), length(X3, 1),
    all_pairs(D3, R3, PairsD3),
    append(X4, R4, D4), length(X4, 1),
    all_pairs(D4, R4, PairsD4),

    constraint(PairsD1),
    constraint(PairsD2),
    constraint(PairsD3),
    constraint(PairsD4),

    % 3. Labeling
    label(D),

    % 4. Write Solution
    writeN(D1), 
    writeN(D2), 
    writeN(D3), 
    writeN(D4), halt.

all_pairs([_], [], []):- !.
all_pairs([X|L], [Y|R], [P|Pairs]):- append([X], [Y], P), all_pairs([X|L], R, Pairs).
all_pairs([_,Y|L], [], Pairs):- all_pairs([Y|L], L, Pairs).
    
writeN(D):- findall(X,(member(N,D),num(X,N)),L), write(L), nl, !.
