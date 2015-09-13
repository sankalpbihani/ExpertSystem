:- op(900, xfx, ::).
:- op(800, xfx, was). 
:- op(870, fx, if). 
:- op(880, xfx, then). 
:- op(550, xfy, or). 
:- op(540, xfy, and). 
:- op(300, fx, 'derived by'). 
:- op(600, xfx, from). 
:- op(600, xfx, by). 
:- op(900, fx, not). 
%:- op(700, xfx, is).

:- op(500, fx, need).
:- op(450, xfx, ' to determine').
:- op(400, xfx, 'by rule').

:- op(100, xfx, has).

eval(Goal, [N :: (Goal is Answer) was 'found as fact'], Answer, N, Why) :-
	fact(Goal is Answer);
	(
		\+ fact(Goal is Answer),
		askable(Goal),
		ask_user(Goal, [N :: need Goal | Why]),
		fact(Goal is Answer)
	);
	(
		\+ fact(Goal is Answer),
		\+ askable(Goal),
		\+ rule(if _ then Goal is Answer),
		\+ Goal = _ and _,
		\+ Goal = _ or _,
		\+ Goal = (not _),
		write('Unable to determine truth value of statement '), write(Goal), write(' which is of non askable type '), nl,
		fail
	).
	
eval(Goal, [N :: (Goal is Answer) was 'derived by' (if Cond then Goal is Answer) | T], Answer, N, Why) :-
	rule(if Cond then Goal is Answer),
	N4 is N + 4,
	eval(Cond, T, true, N4, [N :: need Cond ' to determine' Goal 'by rule' (if Cond then Goal is Answer) | Why]).
	
eval(Goal1 and Goal2, Trace, Answer, N, Why) :- 
	eval(Goal1, Trace1, Answer1, N, Why),
	eval(Goal2, Trace2, Answer2, N, Why),
	logical_and(Answer1, Answer2, Answer),
	append(Trace1, Trace2, Trace).

eval(Goal1 or Goal2, Trace, Answer, N, Why) :- 
	eval(Goal1, Trace1, Answer1, N, Why),
	eval(Goal2, Trace2, Answer2, N, Why),
	logical_or(Answer1, Answer2, Answer),
	append(Trace1, Trace2, Trace).	
		
eval(not Goal1, Trace, Answer, N, Why) :- 
	eval(Goal1, Trace, Answer1, N, Why),
	logical_not(Answer1, Answer).

show_trace([]).
show_trace([N :: Line | Remaining]) :-
	tab(N), write(Line), nl, show_trace(Remaining).
	
ask_for_trace(Trace) :-
	write("Would you like to see how? (y/n) "),
	get_single_char(X), char_code(A, X), nl,
	ask_for_trace(Trace, A).
	
ask_for_trace(Trace, y) :-
	show_trace(Trace).
	
ask_user(Goal, Why) :-
	write(Goal), write(' (y/n/w) : '), get_single_char(X), char_code(A, X), nl, ask_user(Goal, Why, A).
	
ask_user(Goal, _, y) :-
	assertz(fact(Goal is true)).
	
ask_user(Goal, _, n) :-
	assertz(fact(Goal is false)).
	
ask_user(Goal, Why, w) :-
	process_why(Why),
	ask_user(Goal, Why).
	
ask_user(Goal, Trace, X) :-
	\+ X = y,
	\+ X = n,
	\+ X = w,
	write('Invalid input, please enter y=yes(true), n=no(false), w=why'), nl,
	ask_user(Goal, Trace).
	
get_max_len([], 0).
get_max_len([N1 :: _ | Why], N) :-
	get_max_len(Why, N2),
	N is max(N1, N2).

show_why([], _).
show_why([N1 :: Line| Remaining], N) :-
	N2 is N - N1,
	tab(N2), write(Line), nl, show_why(Remaining, N).
	
process_why(Why) :-
	get_max_len(Why, N),
	show_why(Why, N).
	
query(Goal) :-
	eval(Goal, Trace, Answer, 0, []),
	nl, write(Goal is Answer), nl,
	ask_for_trace(Trace);
	\+ eval(Goal, _, _, 0, []),
	nl, write('No path could be found which can determine truth value of '), write(Goal), nl.
	
logical_and(true, true, true) :- !.
logical_and(_, _, false).
logical_or(false, false, false) :- !.
logical_or(_, _, true).
logical_not(true, false).
logical_not(false, true).

askable(_ has _).
	
:- assertz(fact(a is true)).
:- assertz(rule(if a then b is true)).
:- assertz(rule(if (not (a or b)) then c is true)).
:- assertz(rule(if (a has b and b has a) then c is true)).
:- query(c). 