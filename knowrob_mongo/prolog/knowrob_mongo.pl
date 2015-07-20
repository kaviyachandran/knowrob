/** 

  Copyright (C) 2013 Moritz Tenorth, 2015 Daniel Beßler
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

@author Moritz Tenorth
@license BSD
*/

:- module(knowrob_mongo,
    [
      mng_db/1,
      mng_timestamp/2,
      
      mng_query/2,
      mng_query/3,
      mng_query_latest/4,
      mng_query_latest/5
    ]).

:- use_module(library('semweb/rdfs')).
:- use_module(library('owl_parser')).
:- use_module(library('owl')).
:- use_module(library('rdfs_computable')).
:- use_module(library('jpl')).
:- use_module(library('knowrob_objects')).
:- use_module(library('knowrob_perception')).
:- use_module(library('knowrob_coordinates')).
:- use_module(library('knowrob_mongo_interface')).

:- rdf_db:rdf_register_ns(rdf, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#', [keep(true)]).
:- rdf_db:rdf_register_ns(owl, 'http://www.w3.org/2002/07/owl#', [keep(true)]).
:- rdf_db:rdf_register_ns(knowrob, 'http://knowrob.org/kb/knowrob.owl#', [keep(true)]).
:- rdf_db:rdf_register_ns(xsd, 'http://www.w3.org/2001/XMLSchema#', [keep(true)]).

:-  rdf_meta
    mng_db(+),
    mng_timestamp(r, r),
    mng_query(+,-),
    mng_query(+,-,+),
    mng_query_latest(+,-,+,r),
    mng_query_latest(+,-,+,r,+).

%% mng_db(+DBName) is nondet.
%
% Change mongo database used for future queries
%
% @param DBName  The name of the db (e.g., 'roslog')
%
mng_db(DBName) :-
    mongo_interface(Mongo),
    jpl_call(Mongo, setDatabase, [DBName], _).

%% mng_timestamp(+Date, -Stamp) is nondet.
%
% Computes a timestamp that corresponds to the specified date.
% The specified date must be expressed in the format "yyyy-MM-dd'T'HH:mm:ss.SSS'Z'"
%
% @param Date String representation of a date
% @param Stamp Floating point timestamp that represents the date
%
mng_timestamp(Date, Stamp) :-
  mongo_interface(DB),
  jpl_call(DB, 'timestamp', [Date], Stamp).

%% mng_query(+Collection, -DBObj) is nondet.
%
% Query all records of a collection.
%
% @param Collection The collection name
% @param DBObj The query result (wrapped in access mode)
%
mng_query(Collection, DBObj) :-
  mng_jpl_query([Collection], DBObj).

%% mng_query(+Collection, -DBObj, +MongoPattern) is nondet.
%
% Query all records of a collection matching a pattern.
%
% @param Collection The collection name
% @param DBObj The query result (wrapped in access mode)
% @param MongoPattern The query pattern: A list of key-relation-value triples
%
mng_query(Collection, DBObj, MongoPattern) :-
writeln('mng_query0'),
  findall(Key, member([Key,_,_],MongoPattern), Keys),
  findall(Rel, member([_,Rel,_],MongoPattern), Relations),
  findall(Obj, (
      member([_,_,Val], MongoPattern),
      once(mng_value_object(Val, Obj))
  ), Values), !,
writeln('mng_query1'),
writeln(MongoPattern),
writeln(Keys),
writeln(Relations),
writeln(Values),
writeln('mng_query3'),
  jpl_list_to_array(Keys, KeysArray),
writeln('mng_query41'),
  jpl_list_to_array(Relations, RelationsArray),
writeln('mng_query42'),
  jpl_list_to_array(Values, ValuesArray),
writeln('mng_query43'),
  mng_jpl_query([Collection, KeysArray, RelationsArray, ValuesArray], DBObj).

%% mng_query_latest(+Collection, -DBObj, +TimeKey, +Time) is nondet.
%
% Query all records of a collection with earlier timestamp then specified.
%
% @param Collection The collection name
% @param DBObj The query result (wrapped in access mode)
% @param TimeKey The document key of time values
% @param Time The time value
%
mng_query_latest(Collection, DBObj, TimeKey, Time) :-
  mng_query_latest(Collection, DBObj, TimeKey, Time, []).

%% mng_query_latest(+Collection, -DBObj, +TimeKey, +Time, +MongoPattern) is nondet.
%
% Query all records of a collection matching a pattern
% and with earlier timestamp then specified.
%
% @param Collection The collection name
% @param DBObj The query result (wrapped in access mode)
% @param TimeKey The document key of time values
% @param Time The time value
% @param MongoPattern The query pattern: A list of key-relation-value triples
%
mng_query_latest(Collection, DBObj, TimeKey, Time, MongoPattern) :-
  mng_query(Collection, ascending(DBObj,TimeKey), [[TimeKey, '<', date(Time)]|MongoPattern]).

%% mng_jpl_query(+Args, -DBObj) is nondet.
%
% Query the mongo DB and access the results.
%
% @param Args Arguments for the query call
% @param DBObj The query result (wrapped in access mode)
%
mng_jpl_query(Args, DBObj) :-
  mongo_interface(DB),
  jpl_call(DB, 'query', Args, Cursor),
  not(Cursor = @(null)), !,
  mng_query_process(Cursor, DBObj), !,
  jpl_call(Cursor, 'close', [], _).

%% mng_jpl_query(+Args, -DBObj) is nondet.
%
% Access query results.
% DBObj must be wrapped in access mode.
% 'ascending' and 'descending' sorts the query results.
% 'all' reads all query results and
% 'next' reads only the next query result.
%
% @param Cursor The query result cursor
% @param DBObj The query result (wrapped in access mode)
%
mng_query_process(Cursor, ascending(DBObj,Key)) :-
  mongo_interface(DB),
  jpl_call(DB, 'ascending', [Cursor,Key], _),
  mng_query_process(Cursor, DBObj).

mng_query_process(Cursor, descending(DBObj,Key)) :-
  mongo_interface(DB),
  jpl_call(DB, 'descending', [Cursor,Key], _),
  mng_query_process(Cursor, DBObj).

mng_query_process(Cursor, all(DBObjs)) :-
  mongo_interface(DB),
  jpl_call(DB, 'all', [Cursor], DBObjsArr),
  not(DBObjsArr = @(null)),
  jpl_array_to_list(DBObjsArr, DBObjs).

mng_query_process(Cursor, one(DBObj)) :-
  mongo_interface(DB),
  jpl_call(DB, 'next', [Cursor], DBObj),
  not(DBObj = @(null)).

mng_query_process(_, n([],0)) :- true.
mng_query_process(Cursor, n(DBObjs,Count)) :-
  mongo_interface(DB),
  jpl_call(DB, 'next', [Cursor], DBObj),
  not(DBObj = @(null)),
  N is Count - 1,
  mng_query_process(Cursor, n(Tail,N)),
  DBObjs = [DBObj|Tail].

%% mng_value_object(+Value, -DBValue) is nondet.
%
% Converts Prolog value representation to type compatiple
% for DB queries.
%
% @param Value The Prolog value
% @param DBValue The query value
%
mng_value_object(date(Val), Date) :-
  number(Val),
  Miliseconds is Val * 1000.0,
  jpl_new('java.lang.Double', [Miliseconds], MilisecondsDouble), 
  jpl_call(MilisecondsDouble, 'longValue', [], MilisecondsLong),
  jpl_new('org.knowrob.interfaces.mongo.types.ISODate', [MilisecondsLong], ISODate),
  jpl_call(ISODate, 'getDate', [], Date).

mng_value_object(date(Timepoint), Date) :-
  atom(Timepoint),
  time_term(Timepoint, Time),
  mng_value_object(date(Time), Date).

mng_value_object(Val, ObjJava) :-
  integer(Val),
  jpl_new('java.lang.Long', [Val], ObjJava).

mng_value_object(Val, ObjJava) :-
  float(Val),
  jpl_new('java.lang.Double', [Val], ObjJava).

mng_value_object(Val, Val) :-
  atom(Val).
