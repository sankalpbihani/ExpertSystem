:- op(900, xfx, ::).
:- op( 800, xfx, was). 
:- op( 870, fx, if). 
:- op( 880, xfx, then). 
:- op( 550, xfy, or). 
:- op( 540, xfy, and). 
:- op( 300, fx, 'derived by'). 
:- op( 600, xfx, from). 
:- op( 600, xfx, by). 
%:- op( 700, xfx, is).
%:- op( 900, fx, not). 

eval(Goal, [N :: (Goal is Answer) was 'found as fact'], Answer, N) :-
	fact(Goal is Answer).
	
eval(Goal, [N :: (Goal is Answer) was 'derived by' (if Cond then Goal is Answer) | T], Answer, N) :-
	rule(if Cond then Goal is Answer),
	N4 is N + 4,
	eval(Cond, T, true, N4).
	
%eval(Goal1 and Goal2)
	
	
:- assertz(fact(a is true)).
:- eval(a, [A :: B | _], _, 4), tab(A), write(B). 