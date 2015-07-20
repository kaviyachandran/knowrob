/*
 * Copyright (c) 2013 Moritz Tenorth, 2015 Daniel Beßler
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *     * Neither the name of the Technische Universiteit Eindhoven nor the
 *       names of its contributors may be used to endorse or promote products
 *       derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 *
 */

package org.knowrob.interfaces.mongo;

import java.sql.Timestamp;
import java.text.DecimalFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;
import java.util.TimeZone;
import java.util.regex.Pattern;

import javax.vecmath.Matrix4d;

import org.knowrob.interfaces.mongo.types.Designator;
import org.knowrob.interfaces.mongo.types.ISODate;
import org.knowrob.tfmemory.TFMemory;
import org.ros.message.Time;

import tfjava.Stamped;
import tfjava.StampedTransform;

import com.mongodb.BasicDBObject;
import com.mongodb.DB;
import com.mongodb.DBCollection;
import com.mongodb.DBCursor;
import com.mongodb.DBObject;
import com.mongodb.QueryBuilder;


public class MongoDBInterface {

	TFMemory mem;

	final SimpleDateFormat mongoDateFormat;
	
	/**
	 * Constructor
	 *
	 * Initialize DB client and connect to database.
	 *
	 */
	public MongoDBInterface() {
		// Format of dates as saved in mongo
		mongoDateFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'");
		mongoDateFormat.setTimeZone(TimeZone.getTimeZone("GMT"));
        
		mem = TFMemory.getInstance();
	}
	
	/**
	 * @return DB handle of currently active DB
	 */
	public DB getDatabase() {
		return mem.getDatabase();
	}
	
	/**
	 * Set the DB name that is used for mongo queries.
	 * @param name The DB name
	 * @return DB handle
	 */
	public DB setDatabase(String name) {
		mem.setDatabase(name);
		return getDatabase();
	}


	/**
	 * Wrapper around the lookupTransform method of the TFMemory class
	 *
	 * @param sourceFrameId ID of the source frame of the transformation
	 * @param targetFrameId ID of the target frame of the transformation
	 * @param posix_ts POSIX timestamp (seconds since 1.1.1970)
	 * @return
	 */
	public StampedTransform lookupTransform(String targetFrameId, String sourceFrameId, double posix_ts) {
		Time t = new Time();
		t.secs = (int)posix_ts;
		t.nsecs = (int) (1E9 * (posix_ts - ((int) posix_ts)));
		return(mem.lookupTransform(targetFrameId, sourceFrameId, t));
	}

	/**
	 * Wrapper around the transformPose method of the TFMemory class
	 *
	 * @param targetFrameID  ID of the target frame of the transformation
	 * @param stampedIn      Stamped<Matrix4d> with the pose in the original coordinates
	 * @param stampedOut     Stamped<Matrix4d> that will hold the resulting pose
	 * @return               true if transform succeeded
	 */
	public boolean transformPose(String targetFrameID, Stamped<Matrix4d> stampedIn, Stamped<Matrix4d> stampedOut) {
		return mem.transformPose(targetFrameID, stampedIn, stampedOut);
	}

	/**
	 * Read location of a designator.
	 * @param d The designator instance
	 * @return The pose transformation matrix
	 */
	public Matrix4d location(Designator d) {
		return location(d, "/map");
	}
	/**
	 * Read location of a designator.
	 * @param d The designator instance
	 * @param targetFrame The map frame
	 * @return The pose transformation matrix
	 */
	public Matrix4d location(Designator d, String targetFrame) {
		Stamped<Matrix4d> poseMatrix = null;
		
		try {
			Object mat = null;
			Designator loc = (Designator)d.get("AT");
			if(loc != null) {
				mat = loc.get("POSE");
			}
			if(mat == null) {
				mat = d.get("POSE");
			}
			
			if(mat!=null && mat instanceof Stamped<?>) {
				poseMatrix = (Stamped<Matrix4d>)mat;
				if(!poseMatrix.frameID.startsWith("/"))
					poseMatrix.frameID = "/"+poseMatrix.frameID;
				
				// Transform pose to target frame if required
				if(poseMatrix.frameID!=null && !targetFrame.equals(poseMatrix.frameID)) {
					final Stamped<Matrix4d> worldFrame = new Stamped<Matrix4d>();
					worldFrame.setData(new Matrix4d());
					worldFrame.getData().setIdentity();
					
					if(transformPose(targetFrame, poseMatrix, worldFrame))
						poseMatrix = worldFrame;
				}
			}
		}
		catch(Exception e){
			e.printStackTrace();
		}
		if(poseMatrix == null)
			return null;
		else
			return poseMatrix.getData();
	}

	/**
	 * Computes a timestamp that corresponds to the specified date.
	 * date format must be as follows: "yyyy-MM-dd'T'HH:mm:ss.SSS'Z'"
	 * @throws ParseException 
	 */
	public String timestamp(String date) throws ParseException {
		// Avoid scientific notation
		DecimalFormat df = new DecimalFormat("#");
		df.setMaximumFractionDigits(9);
		return df.format(mongoDateFormat.parse(date).getTime()/1000.0);
	}

	/**
	 * Read designator from query result.
	 * @param dbObj The result of a query
	 * @return The created designator instance
	 */
	public Designator designator(BasicDBObject dbObj) {
		try {
			return new Designator().readFromDBObject(dbObj.containsKey((Object)"designator") ?
					(BasicDBObject)dbObj.get("designator") : dbObj);
		}
		catch (Exception e) {
			// TODO: Proper logging
			System.err.println("designator failed: " + e.getMessage());
			e.printStackTrace();
			return null;
		}
	}

	/**
	 * Read next query result.
	 * @param cursor DB Cursor of matching records
	 * @return The next query result
	 */
	public BasicDBObject next(DBCursor cursor) {
		return cursor.hasNext() ? (BasicDBObject)cursor.next() : null;
	}

	/**
	 * Read all query results.
	 * @param cursor DB Cursor of matching records
	 * @return List of query results
	 */
	public BasicDBObject[] all(DBCursor cursor) {
		if(cursor.count()==0) return null;
		BasicDBObject[] objs = new BasicDBObject[cursor.count()];
		int index = 0;
		while(cursor.hasNext()) {
			objs[index++] = (BasicDBObject)cursor.next();
		}
		return objs;
	}

	/**
	 * Sorts query results in descending order.
	 * @param cursor DB Cursor of matching records
	 * @param sortKey The document key used for sorting
	 */
	public void descending(DBCursor cursor, String sortKey) {
		cursor.sort(new BasicDBObject(sortKey, -1));
	}

	/**
	 * Sorts query results in ascending order.
	 * @param cursor DB Cursor of matching records
	 * @param sortKey The document key used for sorting
	 */
	public void ascending(DBCursor cursor, String sortKey) {
		cursor.sort(new BasicDBObject(sortKey, 1));
	}

	/**
	 * Query all records of a collection with earlier time then specified and in descending order.
	 * @param collection The collection name
	 * @param timeKey The document key of time values
	 * @param posix_ts A time value
	 * @return DB Cursor of matching records
	 */
	public DBCursor latest(String collection, String timeKey, double posix_ts) {
		return latest(collection, null, null, null, timeKey, posix_ts);
	}
	
	/**
	 * Query all records of a collection with earlier time then specified and in descending order.
	 * @param collection The collection name
	 * @param keys List of pattern keys (e.g., '__recorded')
	 * @param relations List of pattern key-value relations (e.g., 'is', '>', '>=', ...)
	 * @param values List of pattern values (e.g., 0)
	 * @param timeKey The document key of time values
	 * @param posix_ts A time value
	 * @return DB Cursor of matching records
	 */
	public DBCursor latest(String collection, String[] keys, String[] relations, Object[] values, String timeKey, double posix_ts) {
		Date t = new ISODate((long) (1000.0 * posix_ts) ).getDate();
		QueryBuilder query = QueryBuilder.start(timeKey).lessThan( t );
		DBCursor cursor = query(collection, query, keys, relations, values);
		cursor.sort(new BasicDBObject(timeKey, -1));
		return cursor;
	}

	/**
	 * Query all records of a collection.
	 * @param collection The collection name
	 * @return DB Cursor of matching records
	 */
	public DBCursor query(String collection) {
		return query(collection, null, null, null);
	}
	
	/**
	 * Query for records that match given pattern.
	 * @param collection The collection name
	 * @param keys List of pattern keys (e.g., '__recorded')
	 * @param relations List of pattern key-value relations (e.g., 'is', '>', '>=', ...)
	 * @param values List of pattern values (e.g., 0)
	 * @return DB Cursor of matching records
	 */
	public DBCursor query(String collection, String[] keys, String[] relations, Object[] values) {
		return query(collection, QueryBuilder.start(), keys, relations, values);
	}
	
	private DBCursor query(String collection, QueryBuilder query, String[] keys, String[] relations, Object[] values) {
		System.err.println("MONGO query " + collection);
		try {
			DBCollection coll = getDatabase().getCollection(collection);
			
			if(relations!=null&&keys!=null&&values!=null) {
				for(int i=0; i<relations.length && i<keys.length && i<values.length; ++i) {
					String rel = relations[i];
					String key = keys[i];
					Object val = values[i];
					
					if("==".equals(rel) || "=".equals(rel) || "is".equals(rel))
						query = query.and(key).is(val);
					else if("!=".equals(rel))
						query = query.and(key).notEquals(val);
					else if("<".equals(rel))
						query = query.and(key).lessThan(val);
					else if("<=".equals(rel))
						query = query.and(key).lessThanEquals(val);
					else if(">".equals(rel))
						query = query.and(key).greaterThan(val);
					else if(">=".equals(rel))
						query = query.and(key).greaterThanEquals(val);
					else if("exist".equals(rel) || "exists".equals(rel))
						query = query.and(key).exists(val);
					else {
						System.err.println("Unknown mongo relation: " + rel);
					}
				}
			}
			
			DBObject queryInstance = query.get();
			System.err.println("   query: " + queryInstance.toString());
			DBObject cols  = new BasicDBObject();
			DBCursor cursor = coll.find(queryInstance, cols);
			if(!cursor.hasNext())  {
				return null; // No results
			}
			return cursor;
		}
		catch (Exception e) {
			// TODO: Proper logging
			System.err.println("query failed: " + e.getMessage());
			e.printStackTrace();
		}
		return null;
	}
	
	/**
	 * Read designators based on the given filter pattern. The strings in the
	 * keys and values lists are AND-joined to form query expressions for MongoDB.
	 * 
	 * K = ['designator.TYPE','designator.GOAL.TO','designator.GOAL.OBJ.TYPE']
	 * V = [NAVIGATION,SEE,PANCAKEMIX].
	 * 
	 * @param keys Strings describing fields in a document using the dot notation 
	 * @param values Strings of values that these fields need to have
	 * @return List of @Designator data structures that match the query expressions
	 */
	public Designator[] queryDesignatorsByPattern(String[] keys, String[] values) {
		
		DBCollection coll = getDatabase().getCollection("logged_designators");
		
		QueryBuilder qb = QueryBuilder.start("designator").exists("_id");
		for(int i=0; i<keys.length; i++) {
			qb = qb.and(keys[i]).is(Pattern.compile(values[i],Pattern.CASE_INSENSITIVE)); // pattern for case insensitive matching
		}
		
		DBObject query = qb.get();
		
		DBObject cols  = new BasicDBObject();
		cols.put("__recorded", 1 );		
		cols.put("designator", 1 );

		DBCursor cursor = coll.find(query, cols);

		Designator[] res = new Designator[cursor.size()];
		int r=0;
		
		while(cursor.hasNext()) {
			DBObject row = cursor.next();
			Designator desig = new Designator().readFromDBObject((BasicDBObject) row.get("designator"));
			res[r++]=desig;
		}
		cursor.close();
		
		return res;
	}
	
	/**
	 * Close data record cursor
	 * @param cursor The cursor object
	 */
	public void close(DBCursor cursor) {
		try {
			cursor.close();
		}
		catch (Exception e) {
			// TODO: Proper logging
			System.err.println("closing query failed: " + e.getMessage());
			e.printStackTrace();
		}
	}
	
	/**
	 * Query the set of distinct values for a record key in a collection.
	 * @param collection The collection name
	 * @param key The key of the record (e.g., 'designator.NAME')
	 * @return The set of distinct values
	 */
	public Object[] distinctValues(String collection, String key) {
		DBCollection coll = getDatabase().getCollection(collection);
		
		List<?> l = coll.distinct(key);
		Object[] out = new Object[l.size()];
		int index = 0;
		for(Object v : l) {
			out[index] = v;
			index += 1;
		}
		
		return out;
	}
	
	

	public static void main(String[] args) {

//		MongoDBInterface m = new MongoDBInterface();
//
//		Designator d = m.getDesignatorByID("designator_C4yixt3iPwKHCt");
//		
//		
//		ArrayList<String> k = new ArrayList<String>();
//		ArrayList<String> v = new ArrayList<String>();
//		
//		
//		k.add("designator.TYPE");
//		k.add("designator.GOAL.TO");
//		k.add("designator.GOAL.OBJ.TYPE");
//
//		v.add("NAVIGATION");
//		v.add("SEE");
//		v.add("PANCAKEMIX");
//		
//		Designator[] res = m.getDesignatorsByPattern(
//				new String[]{"designator.TYPE", "designator.GOAL.TO", "designator.GOAL.OBJ.TYPE"}, 
//				new String[]{"navigation", "see", "PANCAKEMIX"});
//		
//		System.out.println(res.length);

		// test transformation lookup based on DB information

//		Timestamp timestamp = Timestamp.valueOf("2013-07-26 14:27:22.0");
//		Time t = new Time(1396512420);
//		Time t = new Time(1396512422); // no
//		Time t = new Time(1396512424);  //1

		TFMemory tf = TFMemory.getInstance();

		try {
			SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS");
			Date date = sdf.parse("2014-04-30 13:31:51.224");
			Time t = new Time(date.getTime()/1000.0);
			System.out.println("UTC " + date + " -> " + date.getTime()/1000.0);
			sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
			date = sdf.parse("2014-04-30 13:31:51.224");
			System.out.println("GMT " + date + " -> " + date.getTime()/1000.0);
			
			t = new Time(date.getTime()/1000.0);
			
			System.out.println(tf.lookupTransform("/map", "/RightHand", t));
		}
		catch (ParseException e) {
			e.printStackTrace();
		}
		
		Timestamp timestamp = Timestamp.valueOf("2014-08-27 13:30:35.0");
		//Time t = new Time(timestamp.getTime());  //1
		System.out.println(timestamp.getTime());


		Time t_st  = new Time(1396512420);
		Time t_end = new Time(1396512422);

		long t0 = System.nanoTime();
		System.out.println(tf.lookupTransform("/base_link", "/l_gripper_palm_link", t_end));
		long t1 = System.nanoTime();
		System.out.println(tf.lookupTransform("/base_link", "/l_gripper_palm_link", t_end));
		long t2 = System.nanoTime();
		System.out.println(tf.lookupTransform("/base_link", "/l_gripper_palm_link", t_st));
		long t3 = System.nanoTime();

		double first  = (t1-t0)/ 1E6;
		double second = (t2-t1)/ 1E6;
		double third  = (t3-t2)/ 1E6;
		
		System.out.println("Time to look up first transform: " + first + "ms");
		System.out.println("Time to look up second transform: " + second + "ms");
		System.out.println("Time to look up second transform: " + third + "ms");

		// test lookupTransform wrapper
//		trans = m.lookupTransform("/map", "/head_mount_kinect_ir_link", 1377766521);
//		System.out.println(trans);

//		// test UIMA result interface
//		Designator d = m.latestUIMAPerceptionBefore(1377766521);
//		System.out.println(d);
//
//		// test designator reading
//		d = m.getDesignatorByID("designator_bunEaUUmPbuoLN");
//		System.out.println(d);
	}
}

