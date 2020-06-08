:- begin_tests('lang_is_a').

:- use_module(library('gui_tracer')).
:- use_module(library('db/tripledb'), [ tripledb_load/2 ]).

:- use_module('is_a.pl').

:- tripledb_load('package://knowrob/owl/test/pancake.owl',[ graph(user),namespace(test_pancake,'http://knowrob.org/kb/pancake.owl#')]).

% :- guitracer.
% :- trace.

%%% is_a 
test("subclass_of") :-
	is_a(test_pancake:'BakingAPancake', dul:'Task').

test("instance_of") :-
	instance_of(test_pancake:'Baking_0', test_pancake:'BakingAPancake').

%test("tell_instance_of") :-
	%tell(instance_of(test_pancake:'Baking_0', dul:'Workflow')).

test("ask_instance_of") :-
	instance_of(test_pancake:'Baking_0', dul:'Workflow').

test("tell_list_instance_of") :-
	\+ instance_of(test_pancake:'Baking_0', [test_pancake:'BakingAPancake', dul:'Workflow']).

test("ask_list_instance_of") :-
	instance_of(test_pancake:'Baking_0', [test_pancake:'BakingAPancake', dul:'Workflow']).

test('subproperty') :-
	subproperty_of(knowrob:'actor', dul:'hasParticipant').

%% instantiation error
test('test_failing_case', ['fail']) :-
	is_a(A, dul:'Task').
	

end_tests('lang_is_a').

