%% Write a Prolog predicate eqSplit(L,S1,S2) that, given a list of
%% integers L, splits it into two disjoint subsets S1 and S2 such that
%% the sum of the numbers in S1 is equal to the sum of S2. It should
%% behave as follows:
%%
%% ?- eqSplit([1,5,2,3,4,7],S1,S2), write(S1), write('    '), write(S2), nl, fail.
%%
%% [1,5,2,3]    [4,7]
%% [1,3,7]    [5,2,4]
%% [5,2,4]    [1,3,7]
%% [4,7]    [1,5,2,3]


eqSplit(L, S1, S2):- subsetOf(L, S1), notHave(L, S1, S2), sumList(S1, N), sumList(S2, N).


sumList([], 0):- !.
sumList([X|L], N):- sumList(L, N2), N is N2 + X.

subsetOf([], []).
subsetOf([X|L], [X|R]):- subsetOf(L, R).
subsetOf([_|L], R):- subsetOf(L, R).

notHave([], _, []):- !.
notHave([X|Xs], Ys, Zs):- member(X, Ys), !, notHave(Xs, Ys, Zs).
notHave([X|Xs], Ys, [X|Zs]):- notHave(Xs, Ys, Zs).
