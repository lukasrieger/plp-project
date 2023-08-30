% :-  op(900, xfx, <---).
% :-  op(950, xfx, ::).



0.3 :: pandemic <--- [flu(X), cold].
0.6 :: epidemic <--- [flu(X)].

0.7 :: cold <--- [].

% 0.3 :: pandemic <--- [flu(X)].
% 0.6 :: epidemic <--- [flu(X)].

sortpred(flu).


flu(robert).
flu(david).



% [] <--- [flu(Robert)].
% [] <--- [flu(David)]. 