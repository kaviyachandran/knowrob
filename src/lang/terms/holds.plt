:- begin_tests('lang_holds').

:- use_module(library('gui_tracer')).
:- use_module(library('db/tripledb'), [ tripledb_load/2 ]).

:- use_module('holds.pl', [holds/1, holds/3]).


:- tripledb_load('package://knowrob/owl/test/events.owl',[ graph(user),namespace(test_events,'http://knowrob.org/kb/test_events.owl#')]).

% :- guitracer.
% :- trace.


%%% Testing triples

test('holds_triples') :-
    holds(test_events:'Event0',ease:'starts',test_events:'Event1').

test('holds_triples_hasTimeInterval') :-
    holds(test_events:'Short4',dul:'hasTimeInterval',test_events:'Time_Short4').

%%% Testing Query holds/1, fails

test('holds_query') :-
    holds(ease:'starts'(test_events:'Event0',test_events:'Event1')).

%%% Testing the chain of properties.


%%% Testing operators "<, >, ="

test('holds_operator') :-
    holds(test_events:'Time_Long',ease:'hasIntervalBegin', <(1377667001)).

%%% Testing tell

test('holds_tell') :-
    \+ holds(test_events:'Time_Long',ease:'hasIntervalEnd', 1377777040).

%%% Testing ask

test('holds_ask') :-
    holds(test_events:'Time_Long',ease:'hasIntervalEnd', 1377777040).

%%% Testing unit

test('holds_unit') :-
    holds(test_events:'Time_Long',ease:'hasIntervalEnd', s(1377777010)).

%%% Testing tell unit

test('holds_tell_unit') :-
    \+ holds(test_events:'Time_Long',ease:'hasIntervalEnd', s(1377777010)).

%%% Testing unit conversion

test('holds_unit_conversion') :-
    holds(test_events:'Time_Long',ease:'hasIntervalEnd', (X, ms)).

%%% Testing with a property other than object and data property
test('holds_property', ['fail']) :-
    holds(test_events:'Short4', rdf:'type', dul:'Event').

:- end_tests('lang_holds').

