:- module(lang_occurs,
    [ occurs(r) % ?Event
    ]).
/** <module> The occurs predicate.

@author Daniel Beßler
@license BSD
*/

:- op(1000, xf, occurs).

:- use_module(library('db/scope'),
    [ universal_scope/1
    ]).
:- use_module('../scopes/temporal.pl',
    [ time_scope/3,
      time_scope_data/2,
      time_subscope_of/2
    ]).

%% occurs(?Event) is nondet.
%
% True for all occurences (events).
%
% @param Event an event instance.
%
occurs(Evt) ?>
  has_interval_data(Evt,Since,Until),
  { ground([Since,Until]) },
  query_scope(QScope),
  { time_scope(=(Since),=(Until),OccursScope),
    subscope_of(OccursScope,QScope)
  }.

occurs(Evt) +>
  fact_scope(FScope),
  { get_dict(time,FScope,TimeScope),
    universal_scope(US)
  },
  call(
    [ is_event(Evt),
      occurs1(Evt,TimeScope)
    ],
    [scope(US)]
  ).

%%
occurs1(_,TimeScope)   +> { var(TimeScope),! }.
occurs1(Evt,TimeScope) +>
  { time_scope_data(TimeScope,[Since,Until]) },
  has_interval_data(Evt,Since,Until).


		 /*******************************
		 *	    UNIT TESTS	     		*
		 *******************************/

:- begin_tests(lang_occurs).

:- tripledb_load('package://knowrob/owl/test/events.owl',
      [ graph(user),
        namespace(test_events,'http://knowrob.org/kb/test_events.owl#')
      ]).

test('occurs_interval') :-
  assert_true(occurs(test_events:'Short4') during [1377777009, 1377777011]),
  assert_true(occurs(test_events:'Short1') during test_events:'Time_Short1').

% test('occurs_tell') :-
%   assert_true(tell(holds(test_swrl:'Fred', test_swrl:'hasAge', 34) until(2020))),
%   assert_true(tell(holds(test_swrl:'Fred', test_swrl:'hasAge', 34) since(2019))).

% test('occurs_until') :-
%   assert_true(occurs(holds(test_swrl:'Lea', test_swrl:'hasNumber', '+493455247') until(34))),
%   assert_true(occurs(holds(test_swrl:'Fred', test_swrl:'hasAge', 34) until(2020))).

% test('occurs_since') :-
%   assert_true(occurs(holds(test_swrl:'Lea', test_swrl:'hasNumber', '+493455247') since(10))),
%   assert_true(occurs(holds(test_swrl:'Fred', test_swrl:'hasAge', 34) since(2019))).

% test('occurs not') :-
%   occurs(holds(test_swrl:'Fred', test_swrl:'hasAge', 25)).

:-end_tests(lang_occurs).

  
