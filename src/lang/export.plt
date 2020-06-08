:- begin_tests('lang_export').

:- use_module(library('gui_tracer')).
:- use_module(library('db/tripledb'), [ tripledb_whipe/0 ]).

:- use_module('export.pl').
% :- guitracer.
% :- trace.


% :- tripledb_load('package://knowrob/owl/test/events.owl',[ graph(user),namespace(test_events,'http://knowrob.org/kb/test_events.owl#')]).

% clear_db(DB, Triples, Inferred):-
%     tripledb(DB, Triples, Inferred),
%     mng_drop(DB,Triples),
%     mng_drop(DB,Inferred).


test('memorize'):-
    memorize('testdb').
    %clear_db('testdb', 'tripledb', 'inferred').

%:- tripledb_whipe.

% test('remember'):-
%     remember('/home/kavya/ros_ws/src/knowrob/testdb').

:- end_tests('lang_export').
