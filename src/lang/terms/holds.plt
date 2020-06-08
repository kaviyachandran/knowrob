:- begin_tests('lang_holds').

:- use_module('../../../../rosprolog/src/rostest').

:- use_module(library('lang/query'),
    [ tell/1]).

:- use_module(library('gui_tracer')).
:- use_module(library('lang/terms/temporal')).
:- use_module(library('db/tripledb'), [ tripledb_load/2 ]).

:- use_module('holds.pl').

:- tripledb_load('package://knowrob/owl/test/events.owl',[ graph(user),namespace(test_events,'http://knowrob.org/kb/test_events.owl#')]).

:- tripledb_load('package://knowrob/owl/test/swrl.owl',[ graph(user),namespace(test_swrl,'http://knowrob.org/kb/swrl_test#')]).


% :- guitracer.
% :- trace.


%%% Testing triples

test('holds_triples') :-
    holds(test_events:'Event0',ease:'starts',test_events:'Event1').

test('holds_triples_hasTimeInterval') :-
    holds(test_events:'Short4',dul:'hasTimeInterval',test_events:'Time_Short4').

% %%% Testing Query holds/1, fails

test('holds_query') :-
    holds(test_swrl:'hasHeightInMeters'(test_swrl:'RectangleBig',13.5)).

%%% Testing operators "<, >, ="
test('holds_operator =') :-
    holds(test_swrl:'RectangleBig',test_swrl:'hasHeightInMeters', =(13.5)).

test('holds_operator <') :-
    holds(test_swrl:'RectangleBig',test_swrl:'hasHeightInMeters', <(15.0)).

test('holds_operator >') :-
    holds(test_swrl:'RectangleBig',test_swrl:'hasHeightInMeters', >(10.5)).

%%% Testing operators "<, >, = integer"
test('holds_operator = integer') :-
    holds(test_swrl:'RectangleSmall',test_swrl:'hasHeightInMeters', =(6)).

test('holds_operator < integer') :-
    holds(test_swrl:'RectangleSmall',test_swrl:'hasHeightInMeters', <(7)).

test('holds_operator > integer') :-
    holds(test_swrl:'RectangleSmall',test_swrl:'hasHeightInMeters', >(3)).

test('tell Lea hasNumber') :-
    assert_true(tell(holds(test_swrl:'Lea', test_swrl:'hasNumber', '+493564754647'))),
    assert_true(holds(test_swrl:'Lea', test_swrl:'hasNumber', '+493564754647')).

% test('holds tell') :-
%     tell(holds(test_swrl:'RectangleBig',test_swrl:'hasHeightInMeters', 15.2) during [25,45]).



% % fails
% test('holds_operator >') :-
%     holds(test_swrl:'Alex',test_swrl:'hasAge', >=(16)).

%%% Testing tell

% test('holds_tell') :-
%     tell(holds(test_swrl:'Alex',test_swrl:'hasAge', 25)).


%%% Testing ask

% test('holds_ask') :-
%     holds(test_events:'Time_Long',ease:'hasIntervalEnd', 1377777040).

% %%% Testing unit

% test('holds_unit') :-
%     holds(test_events:'Time_Long',ease:'hasIntervalEnd', s(1377777010)).

% %%% Testing tell unit

% test('holds_tell_unit') :-
%     \+ holds(test_events:'Time_Long',ease:'hasIntervalEnd', s(1377777010)).

%%% Testing unit conversion

test('holds_unit_conversion') :-
    assert_true(tell(holds(test_swrl:'RectangleSmall',test_swrl:'hasHeightInMeters', m(6)))),
    assert_true(holds(test_swrl:'RectangleSmall',test_swrl:'hasHeightInMeters', cm(X))),
    assert_equals(X, 600).


%%% Testing with a property other than object and data property
test('holds not object or data type property') :-
    assert_false(holds(test_events:'Short4', rdf:'type', dul:'Event')).

:- end_tests('lang_holds').

