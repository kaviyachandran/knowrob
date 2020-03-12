/*
  Copyright (C) 2019 Daniel Beßler
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:
      * Redistributions of source code must retain the above copyright
        notice, this list of conditions and the following disclaimer.
      * Redistributions in binary form must reproduce the above copyright
        notice, this list of conditions and the following disclaimer in the
        documentation and/or other materials provided with the distribution.
      * Neither the name of the <organization> nor the
        names of its contributors may be used to endorse or promote products
        derived from this software without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
  DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
  DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

:- module(event_memory,
    [
      mem_episode_create/1,           % -EpisodeNode::iri
      mem_event/3,
      mem_event_create/3,             % +ParentNode::iri, -EventType::iri, -Node::iri
      mem_event_begin/1,              % +Node::iri
      mem_event_begin/2,              % +Node::iri, +BeginTime::number
      mem_event_end/1,                % +Node::iri
      mem_event_end/2,                % +Node::iri, +EndTime::number
      mem_event_interval/3,           % +Node::iri, +BeginTime::number, +EndTime::number
      mem_event_set_succeeded/1,
      mem_event_set_failed/1,
      mem_event_set_active/1,
      mem_event_set_paused/1,
      mem_event_set_pending/1,
      mem_event_set_cancelled/1,
      mem_event_role/3,
      mem_event_parameter/3,
      mem_event_constraint/2,
      mem_event_assignment/3,
      mem_event_classification/3,
      mem_event_add_role/2,
      mem_event_add_parameter/2,
      mem_event_add_constraint/5,
      mem_event_add_assignment/3,
      mem_event_add_classification/3,
      mem_event_add_diagnosis/2,
      mem_event_includes/2,           % +Node::iri, +EntityConcepts::list
      mem_event_includes/3,           % +Node::iri, +Entity::iri, +Concept::iri
      mem_event_satisfies/2,          % +Node::iri, +Description::iri
      mem_event_causes_transition/5   % +Node::iri, +Object::iri, +Quality::iri, +From::iri, +To::iri
    ]).
/** <module> RDF event graphs.

@author Daniel Beßler
@license BSD
*/

:- use_module(library('semweb/rdfs')).
:- use_module(library('semweb/rdf_db')).
:- use_module(library('semweb/owl')).

:- use_module(library('knowrob/lang/ask')).
:- use_module(library('knowrob/lang/tell')).

:- use_module(library('knowrob/model/OWL')).
:- use_module(library('knowrob/model/Event')).
:- use_module(library('knowrob/model/EventType')).
:- use_module(library('knowrob/model/Action')).
:- use_module(library('knowrob/model/Situation')).
:- use_module(library('knowrob/model/Constraint')).
:- use_module(library('knowrob/model/Role')).

:-  rdf_meta
    mem_episode_create(r),
    mem_episode_set_map(r,r),
    mem_event(r,r,r),
    mem_event_create(r,r,r),
    mem_event_interval(r,+,+),
    mem_event_begin(r,+),
    mem_event_end(r,+),
    mem_event_set_succeeded(r),
    mem_event_set_failed(r),
    mem_event_set_active(r),
    mem_event_set_paused(r),
    mem_event_set_pending(r),
    mem_event_set_cancelled(r),
    mem_event_role(r,r,r),
    mem_event_parameter(r,r,r),
    mem_event_constraint(r,r),
    mem_event_assignment(r,r,r),
    mem_event_classification(r,r,r),
    mem_event_add_role(r,r),
    mem_event_add_parameter(r,r),
    mem_event_add_constraint(r,r,r,r,+),
    mem_event_add_assignment(r,r,r),
    mem_event_add_classification(r,r,r),
    mem_event_add_diagnosis(r,r),
    mem_event_causes_transition(r,r,r,r,r),
    mem_event_includes(r,t),
    mem_event_includes(r,r,r),
    mem_event_satisfies(r,r),
    mem_new_individual(r,r),
    mem_assert(r,r,t),
    mem_update(r,r,t).

%% mem_episode_create(-Node0) is det.
%
% Creates a new epsiode symbol which is the root
% node in a task execution graph.
%
mem_episode_create(Episode) :-
  situation_create(ease:'Episode',Episode,belief_state),
  mem_new_individual(dul:'TimeInterval',I),
  mem_assert(Episode,dul:includesTime,I).

%% mem_event_create(+Parent0,+EventType,-Node0) is det.
%
% Creates an event node in a task execution graph.
% The event is classified by *EventType*, which is a *Concept*.
% *Parent0* is the parent node in the graph.
%
mem_event_create(Parent0,EventType,Node0) :-
  rdfs_individual_of(Parent0,dul:'Situation'),
  rdfs_subclass_of(EventType,dul:'Concept'),!,
  %% create an event that is classified by EventType
  event_type_create(EventType,EventType0,belief_state),
  mem_event_create_(EventType,Event),
  ( rdfs_individual_of(Event,dul:'Action')
  -> mem_assert(EventType0,dul:isExecutedIn,Event);
     mem_assert(EventType0,dul:classifies,Event)),
  %% create a situation that includes the event
  situation_create(dul:'PlanExecution',Node0,belief_state),
  situation_add(Parent0,Node0),
  situation_add(Node0,Event),
  %% initialize the time extend of the event+situation
  mem_new_individual(dul:'TimeInterval',I),
  mem_assert(Event,dul:hasTimeInterval,I),
  mem_assert(Node0,dul:includesTime,I),
  %%
  (  mem_event(Parent0,ParentEvent0,_) -> 
  (( rdfs_individual_of(ParentEvent0,dul:'Action'),
  \+ rdfs_individual_of(Event,dul:'Action') ) ->
     mem_assert(ParentEvent0,ease:hasPhase,Event) ;
     mem_assert(ParentEvent0,dul:hasConstituent,Event) ) ; true ).

%%
mem_event(Node,Event,EventType) :-
  rdf_has(Node,dul:includesEvent,Event),
  ( rdf_has(EventType,dul:classifies,Event) ;
    mem_event_classification(Node,Event,EventType)
  ),
  !.

		 /*******************************
		 *	interval		*
		 *******************************/

%% mem_event_begin(+Node0,+Begin) is det.
%
mem_event_begin(Node0,Begin) :-
  rdf_has(Node0,dul:includesTime,Interval), (
  ( kb_triple(Interval,ease:hasIntervalBegin,X), X=<Begin ) ;
  ( mem_update(Interval,ease:hasIntervalBegin,Begin),
    ( rdf_has(Parent,ease:includesSituation,Node0) ->
      mem_event_begin(Parent,Begin) ; true )
  )),!.

mem_event_begin(Node0) :-
  get_time(Begin),
  mem_event_begin(Node0, Begin).

%% mem_event_end(+Node0,+End) is det.
%
mem_event_end(Node0,End) :-
  rdf_has(Node0,dul:includesTime,Interval), (
  ( kb_triple(Interval,ease:hasIntervalEnd,X), X>=End ) ;
  ( mem_update(Interval,ease:hasIntervalEnd,End),
    ( rdf_has(Parent,ease:includesSituation,Node0) ->
      mem_event_end(Parent,End) ; true )
  )),!.

mem_event_end(Node0) :-
  get_time(End),
  mem_event_end(Node0, End).

%% mem_event_interval(+Node0,+Begin,+End) is det.
%
mem_event_interval(Node0,Begin,End) :-
  mem_event_begin(Node0,Begin),
  mem_event_end(Node0,End).

		 /*******************************
		 *	action status		*
		 *******************************/

%%
mem_event_set_succeeded(Node) :-
  rdf_has(Node, dul:includesEvent, Act),
  rdfs_individual_of(Act,dul:'Action'),
  action_set_succeeded(Act).
%%
mem_event_set_failed(Node) :-
  rdf_has(Node, dul:includesEvent, Act),
  rdfs_individual_of(Act,dul:'Action'),
  action_set_failed(Act).
%%
mem_event_set_active(Node) :-
  rdf_has(Node, dul:includesEvent, Act),
  rdfs_individual_of(Act,dul:'Action'),
  action_set_active(Act).
%%
mem_event_set_paused(Node) :-
  rdf_has(Node, dul:includesEvent, Act),
  rdfs_individual_of(Act,dul:'Action'),
  action_set_paused(Act).
%%
mem_event_set_pending(Node) :-
  rdf_has(Node, dul:includesEvent, Act),
  rdfs_individual_of(Act,dul:'Action'),
  action_set_pending(Act).
%%
mem_event_set_cancelled(Node) :-
  rdf_has(Node, dul:includesEvent, Act),
  rdfs_individual_of(Act,dul:'Action'),
  action_set_cancelled(Act).

%%
mem_event_concept(Node,Concept,Type) :-
  mem_event(Node, _, EvtType),
  kb_triple(EvtType, dul:isRelatedToConcept, Concept),
  once((
    Type == Concept ;
    kb_type_of(Concept,Type)
  )).

		 /*******************************
		 *	paramaters		*
		 *******************************/

%%
mem_event_parameter(Node,Param,Type) :-
  mem_event(Node, _, EvtType),
  kb_triple(EvtType, dul:hasParameter, Param),
  once((
    Type == Param ;
    kb_type_of(Param,Type)
  )).

%%
mem_event_add_parameter(Node,Param) :-
  mem_event_parameter(Node,Param,_),!.

mem_event_add_parameter(Node,Param) :-
  mem_event(Node, _, EvtType),
  kb_assert(EvtType, dul:hasParameter, Param).

		 /*******************************
		 *	roles			*
		 *******************************/

%%
mem_event_role(Node,Role,Type) :-
  mem_event(Node, _, EvtType),
  kb_triple(EvtType, dul:isTaskOf, Role),
  once((
    Type == Role ;
    kb_type_of(Role,Type)
  )).

%%
mem_event_add_role(Node,Role) :-
  kb_is_class(Role),!,
  once(rdf(Node,rdf:type,_,Graph)),
  once(( mem_event_role(Node,_,Role) ; (
    role_create(Role,Role0,Graph),
    mem_event_add_role(Node,Role0)
  ))).

mem_event_add_role(Node,Role) :-
  mem_event_role(Node,Role,_),!.

mem_event_add_role(Node,Role) :-
  mem_event(Node, _, EvtType),
  kb_assert(EvtType, dul:isTaskOf, Role).

		 /*******************************
		 *	includes		*
		 *******************************/

%% mem_event_includes(+Node0,+EntityClasses) is det.
%
mem_event_includes(_Node0,[]) :- !.

mem_event_includes(Node0,[[Entity,Concept]|Xs]) :-
  mem_event_includes(Node0,Entity,Concept),
  mem_event_includes(Node0,Xs).

%% mem_event_includes(+Node0,+Entity0,+Concept) is det.
%
mem_event_includes(Node0,Object0,Role) :-
  rdfs_individual_of(Object0,dul:'Object'),!,
  situation_add(Node0,Object0),
  mem_event(Node0,Event0,_),
  mem_assert(Event0,dul:hasParticipant,Object0),
  mem_event_add_classification(Node0,Object0,Role).

mem_event_includes(Node0,Region0,Parameter) :-
  rdfs_individual_of(Region0,dul:'Region'),!,
  %situation_add(Node0,Region0),
  mem_event(Node0,Event0,_),
  mem_assert(Event0,dul:hasRegion,Region0),
  mem_event_add_classification(Node0,Region0,Parameter).

		 /*******************************
		 *	satisfies		*
		 *******************************/

%% mem_event_satisfies(+Node0,+Description0) is det.
%
mem_event_satisfies(Node0,Description0) :-
  mem_assert(Node0,dul:satisfies,Description0),
  ( rdfs_individual_of(Description0,dul:'Plan') ->
    mem_assert(Node0,rdf:type,dul:'PlanExecution') ; true ),
  ( rdfs_individual_of(Description0,dul:'Workflow') ->
    mem_assert(Node0,rdf:type,dul:'WorkflowExecution') ; true ).

		 /*******************************
		 *	diagnosis		*
		 *******************************/

%%
mem_event_add_diagnosis(Node,DiagnosisType) :-
  rdfs_individual_of(DiagnosisType,owl:'Class'),!,
  mem_new_individual(DiagnosisType,Diagnosis),
  situation_add_satisfies(Node,Diagnosis).

mem_event_add_diagnosis(Node,Diagnosis) :-
  situation_add_satisfies(Node,Diagnosis).

		 /*******************************
		 *	constraints		*
		 *******************************/

%%
mem_event_constraint(Node,Constraint) :-
  mem_event_parameter(Node,Constraint,knowrob:'Constraint').

%%
mem_event_add_constraint(Node,C_type,
    ConstrainedRole,DependentRole,Constraint) :-
  once(( 
    mem_event_role(Node,ConstrainedRole0,ConstrainedRole),
    mem_event_role(Node,DependentRole0,DependentRole)
  )),
  %%
  mem_new_individual(C_type,Constraint),
  set_constrained_concept(Constraint,ConstrainedRole0),
  set_dependent_concept(Constraint,DependentRole0),
  mem_event_add_parameter(Node,Constraint).
  

		 /*******************************
		 *	classification		*
		 *******************************/

%%
mem_event_classification(Node,Entity,Concept) :-
  situation_includes_classification(Node,Entity,Concept).

%%
mem_event_add_classification(Node,Entity,Concept) :-
  mem_event_classification(Node,Entity,Concept),!.

mem_event_add_classification(Node,Entity,Concept) :-
  %%
  ( mem_event_concept(Node,Concept0,Concept) ;
    Concept0 = Concept
  ),!,
  %%
  situation_add_classification(Node,Entity,Concept0).

		 /*******************************
		 *	assignments		*
		 *******************************/

%%
mem_event_assignment(Node,Argument,Value) :-
  situation_includes_assignment(Node,Argument,Value).

%%
mem_event_add_assignment(Node,Argument,Value) :-
  mem_event_assignment(Node,Argument,Value),!.

mem_event_add_assignment(Node,Argument,Value) :-
  situation_add_assignment(Node,Argument,Value).

		 /*******************************
		 *	transition		*
		 *******************************/

%% mem_event_causes_transition(+Node0,+Object0,+Quality,+FromRegion0,+ToRegion0) is det.
%
mem_event_causes_transition(Node0,Object0,Quality,FromRegion0,ToRegion0) :-
  rdfs_individual_of(Node0,dul:'Situation'),
  rdfs_individual_of(Object0,dul:'Object'),!,
  %%
  rdf_has(Parent,ease:includesSituation,Node0),
  %%
  mem_individual_quality_(Object0,Quality,QualityO),
  mem_quality_state_(QualityO,FromRegion0,InitialState),
  mem_quality_state_(QualityO,ToRegion0,TerminalState),
  %%
  mem_new_individual(ease_obj:'QualityTransition',Transition),
  mem_assert(Transition,dul:includesObject,Object0),
  mem_assert(Transition,ease:hasInitialState,InitialState),
  mem_assert(Transition,ease:hasTerminalState,TerminalState),
  mem_assert(Parent,ease:includesSituation,Transition),
  mem_event(Node0,Event0,_),
  mem_assert(Transition,ease:isCausedByEvent,Event0).

		 /*******************************
		 *	helper		*
		 *******************************/

%%
mem_event_create_(Task,Action) :-
  rdfs_subclass_of(Task,dul:'Task'),!,
  mem_new_individual(dul:'Action',Action).

mem_event_create_(MotionType,Motion) :-
  rdfs_subclass_of(MotionType,ease_proc:'MotionType'),!,
  mem_new_individual(ease_proc:'Motion',Motion).

mem_event_create_(ForceEventType,ForceEvent) :-
  rdfs_subclass_of(ForceEventType,ease_proc:'ForceEventType'),!,
  mem_new_individual(ease_proc:'ForceEvent',ForceEvent).

mem_event_create_(ProcessType,Process) :-
  rdfs_subclass_of(ProcessType,ease_proc:'ProcessType'),!,
  mem_new_individual(dul:'Process',Process).

mem_event_create_(StateType,State) :-
  rdfs_subclass_of(StateType,ease:'StateType'),!,
  mem_new_individual(dul:'State',State).

mem_event_create_(_,Evt) :-
  mem_new_individual(dul:'Event',Evt).

%% get individual quality of an objecz
mem_individual_quality_(Object,Quality,IndividualQuality) :-
  rdf_has(Object,dul:hasQuality,IndividualQuality),
  ( Quality=IndividualQuality ;
    rdfs_individual_of(IndividualQuality,Quality) ),!.

mem_individual_quality_(Object,Quality,IndividualQuality) :-
  mem_new_individual(Quality,IndividualQuality),
  mem_assert(Object,dul:hasQuality,IndividualQuality).

%% create situation in which a quality has a value within a region
mem_quality_state_(Quality,Region,QualityState) :-
  rdfs_individual_of(Quality,dul:'Quality'),
  rdfs_individual_of(Region,dul:'Region'),
  %%
  mem_new_individual(ease_obj:'HasQualityRegion',QR),
  mem_assert(QR,dul:hasQuality,Quality),
  mem_assert(QR,dul:hasRegion,Region),
  %%
  mem_new_individual(dul:'Situation',QualityState),
  mem_assert(QualityState,dul:satisfies,QR).

%%
mem_new_individual(Type,Entity) :-
  kb_create(Type,Entity,_{graph:belief_state}).

%%
mem_assert(S,P,O) :-
  kb_assert(S,P,O,_{graph:belief_state}).

%%
mem_update(S,P,O) :-
  kb_retract(S,P,_),
  kb_assert(S,P,O).
