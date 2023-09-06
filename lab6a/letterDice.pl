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

num(X,N):- nth1( N, [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,r,s,t,u,v,w,x,y], X ).

mes_gran_que([], _).
mes_gran_que([X|L], N):-
    X #> N,
    mes_gran_que(L, N).

main:-
    % 1a. Variables:
    length(D1,6),
    length(D2,6),
    length(D3,6),
    length(D4,6),

    % 1b. Domini:
    append([D1, D2, D3, D4], D),
    D ins 1..24,

    % 2. Constraints:
    all_distinct(D),
    all_restrictions(R),

    % 3. Labeling:
    label(D),

    % 4. Escriure sol:
    writeN(D1), 
    writeN(D2), 
    writeN(D3), 
    writeN(D4), halt.
    
writeN(D):- findall(X,(member(N,D),num(X,N)),L), write(L), nl, !.

all_restrictions(R):- word(),

D=[C1, C2,C3,C4,C5,C6] ---> a-c
   C1 #\ a #\/ C2 #\= c,
   C1 #\ a #\/ C3 #\= c,
   ...

% subsetOfSize(N, L, S). Creates all subsets S of L that have size of N
subsetOfSize(0, _, []):- !.
subsetOfSize
