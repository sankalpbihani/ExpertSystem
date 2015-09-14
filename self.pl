:- op(900, xfx, ::).
:- op(800, xfx, was). 
:- op(870, fx, if). 
:- op(880, xfx, then). 
:- op(550, xfy, or). 
:- op(540, xfy, and). 
:- op(300, xfx, by). 
:- op(600, xfx, from). 
:- op(600, xfx, 'with certainty'). 
:- op(900, fx, not). 
%:- op(700, xfx, is).

:- op(500, fx, need).
:- op(450, xfx, ' to determine').
:- op(400, xfx, 'by rule').

:- op(100, xfx, [has, hass]).

% evaluate probability for fact
eval(Goal, [N :: Goal is true 'with certainty' P by fact], N, Why, P) :-
	fact(Goal, P);
	(
		\+ fact(Goal, _),
		askable(Goal),
		ask_user(Goal, [N :: need Goal | Why]),
		fact(Goal, P)
	);
	(
		\+ fact(Goal, _),
		\+ askable(Goal),
		\+ rule(if _ then Goal, P),
		\+ Goal = _ and _,
		\+ Goal = _ or _,
		\+ Goal = (not _),
		write('Unable to determine certainty of statement '), write(Goal), write(' which is of non askable type '), nl,
		fail
	).
	
% evaluate probability for rule
eval(Goal, [N :: (Goal is true 'with certainty' P) by (if Cond then Goal 'with certainty' P1) | T], N, Why, P) :-
	rule(if Cond then Goal, P1),
	N4 is N + 4,
	check_loop(Why, Goal),
	eval(Cond, T, N4, [N :: need Cond ' to determine' Goal 'by rule' (if Cond then Goal) | Why], P2),
	P is P1 * P2.
	
% evaluate probability for and operator
eval(Goal1 and Goal2, Trace, N, Why, P) :- 
	eval(Goal1, Trace1, N, Why, P1),
	eval(Goal2, Trace2, N, Why, P2),
	P is min(P1, P2),
	append(Trace1, Trace2, Trace).

% evaluate probability for or operator
eval(Goal1 or Goal2, Trace, N, Why, P) :- 
	eval(Goal1, Trace1, N, Why, P1),
	eval(Goal2, Trace2, N, Why, P2),
	P is max(P1, P2),
	append(Trace1, Trace2, Trace).	
		
% evaluate probability for not operator
eval(not Goal1, Trace, N, Why, P) :- 
	eval(Goal1, Trace, N, Why, P1),
	P is 1 - P1.

% show trace for how questions
show_trace([]).
show_trace([N :: Line | Remaining]) :-
	tab(N), write(Line), nl, show_trace(Remaining).
	
% ask whether trace is to be shown
ask_for_trace(Trace) :-
	write("Would you like to see how? (yes/no) "),
	read(X),
	ask_for_trace(Trace, X).
	
ask_for_trace(Trace, yes) :-
	nl, show_trace(Trace).
	
ask_for_trace(_, X) :-
	\+ X = yes.	
	
% check validity of entered probability
check_probability(P) :-
	(
		float(P);
		integer(P)
	),
	P =< 1,
	P >= 0.

% ask user for unknown askable facts
ask_user(Goal, Why) :-
	write(Goal), write(' is true with certainty : '), 
	read(A), ask_user(Goal, Why, A).
	
ask_user(Goal, Why, why) :-
	process_why(Why),
	ask_user(Goal, Why).
	
ask_user(Goal, _, P) :-
	check_probability(P),
	assertz(fact(Goal, P)).
	
ask_user(Goal, Trace, X) :-
	\+ X = why,
	\+ check_probability(X),
	write('Invalid input, please enter certainty, or ask why'), nl,
	ask_user(Goal, Trace).
	
% get length for formatting why questions
get_max_len([], 0).
get_max_len([N1 :: _ | Why], N) :-
	get_max_len(Why, N2),
	N is max(N1, N2).

% show result of why query
show_why([], _).
show_why([N1 :: Line| Remaining], N) :-
	N2 is N - N1,
	tab(N2), write(Line), nl, show_why(Remaining, N).
	
process_why(Why) :-
	get_max_len(Why, N),
	show_why(Why, N).
	
% ask for more possible solutions
ask_for_more(Rep) :-
	write("Find more solutions/paths? (yes/no) : "), 
	read(Rep), nl.
	
% check loop conditions
check_loop([], _).
check_loop([_ :: need _ ' to determine' Goal1 'by rule' (if Cond1 then Goal1) | Why], Goal) :-
	\+ Goal = Goal1,
	check_loop(Why, Goal);
	Goal = Goal1,
	write("Loop detected due to rule : "), write(if Cond1 then Goal1), nl, fail.
	
% run a query
query(Goal) :-
	eval(Goal, Trace, 0, [], P),
	write(Goal is true 'with certainty' P), nl,
	ask_for_trace(Trace),
	ask_for_more(Rep),
	(
		(
			Rep = yes,
			fail
		);
		(
			\+ Rep = yes,
			true
		)
	);
	\+ eval(Goal, _, 0, [], _),
	nl, write('No solutions can determine (un)certainty of '), write(Goal), nl.

% run query, check if no possible solution
ask_expert(Goal) :-
	\+ query(Goal),
	write('No more solutions to determine (un)certainty of '), write(Goal), nl;
	true.
	
% add fact
add_fact(Fact, P) :-
	check_probability(P),
	assertz(fact(Fact, P)).
	
% add rule
add_rule(if Condition then Consequence, P) :-
	check_probability(P),
	assertz(rule(if Condition then Consequence, P)).

% list all facts
list_facts :-
	listing(fact).

% list all rules
list_rules :-
	listing(rule).
	
% remove a fact
remove_fact(Fact) :-
	retractall(fact(Fact, _)).
	
% remove a rule
remove_rule(if Condition then Consequence) :-
	retractall(rule(if Condition then Consequence, _)).

% helper functions for shell
command('list facts') :-
	list_facts.
	
command('list rules') :-
	list_rules.
	
command('add fact') :-
	write("Input fact"), nl,
	read(F),
	write("Input Certainty of the entered fact"), nl,
	read(P),
	add_fact(F, P),
	write("Fact added successfully"), nl.	
	
command('add rule') :-
	write("Input rule"), nl,
	read(R),
	write("Input Certainty of the entered rule"), nl,
	read(P),
	add_rule(R, P),
	write("Rule added successfully"), nl.
	
command('remove fact') :-
	write("Input fact"), nl,
	read(F),
	remove_fact(F),
	write("Fact removed successfully"), nl.
	
command('remove rule') :-
	write("Input rule"), nl,
	read(R),
	remove_rule(R),
	write("Rule removed successfully"), nl.
	
command('ask expert') :-
	write("Input query"), nl,
	read(X),
	ask_expert(X).
	
command(exit) :-
	abort.
	
% shell for user
shell :-
	write("Wating for input"), nl, 
	read_string(user_input, ".", " \n", _, X),
	atom_string(A, X),
	command(A),
	shell;
	true,
	write("Invalid input"), nl,
	shell.

% askable questions/queries
askable(_ has _).
askable(a).
	
:- assertz(fact(dummy, 0)).
:- assertz(rule(if dummy1 then dummy2, 0)).
%:- assertz(fact(a, 1)).
:- assertz(rule(if a then b, 1)).
:- assertz(rule(if b then a, 1)).
:- assertz(rule(if (not (a or b)) then c, 1)).
:- assertz(rule(if (a has b and b has a) then c, 0.75)).
%:- ask_expert(c).
%:- shell.