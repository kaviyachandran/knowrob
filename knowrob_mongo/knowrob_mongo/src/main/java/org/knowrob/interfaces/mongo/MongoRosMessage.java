/*
 * Copyright (c) 2015 Daniel Beßler
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

import java.io.IOException;
import java.lang.reflect.Method;
import java.nio.ByteOrder;
import java.util.GregorianCalendar;

import org.jboss.netty.buffer.ChannelBuffer;
import org.jboss.netty.buffer.ChannelBuffers;
import org.ros.message.Time;
import org.ros.node.ConnectedNode;
import org.ros.node.topic.Publisher;

import com.mongodb.BasicDBObject;

/**
 * Republishing of ROS messages logged with mongodb_log.
 * 
 * @param <RosType> The ROS message type
 */
public class MongoRosMessage<RosType> {
	Publisher<RosType> pub = null;
	// e.g. "std_msgs/String"
	final String typeName;
	final String topic;
	
	public MongoRosMessage(final String typeName, final String topic) {
		this.typeName = typeName;
		this.topic = topic;
	}

	public void connect(final ConnectedNode node) {
		this.pub = node.newPublisher(topic, typeName);
	}

	/**
	 * Publishes a ROS message instantiated by a mongo DB record.
	 * @param mngObj A DB object
	 * @return True if the message was published successfully
	 */
	public boolean publish(BasicDBObject mngObj) {
		if(pub==null) {
			// TODO proper logging
			System.err.println("Not connected.");
			return false;
		}
		try {
			pub.publish(createMessage(mngObj));
			return true;
		}
		catch (Exception e) {
			// TODO proper logging
			System.err.println("Failed to publish message: " + e.getMessage());
			e.printStackTrace();
			return false;
		}
	}
	
	/**
	 * Instantiates a ROS message by a mongo DB record.
	 * @param mngObj A DB object
	 * @return A ROS message
	 * @throws IOException
	 */
	public RosType createMessage(BasicDBObject mngObj) throws IOException {
		final RosType msg = pub.newMessage();
		createMessage(msg, mngObj);
		return msg;
	}

	protected void createMessage(Object msg, BasicDBObject mngObj) throws IOException {
		for(Method m : msg.getClass().getMethods()) {
			String name = m.getName();
			if(!name.startsWith("set")) continue;
			name = name.substring(3);
			
			String[] nameParts = name.split("(?=\\p{Upper})");
			StringBuilder fieldName_ = new StringBuilder();
			for(String x : nameParts) {
				if(fieldName_.length()>0) fieldName_.append('_');
				fieldName_.append(x.toLowerCase());
			}
			final String fieldName = fieldName_.toString();

			final Object value = mngObj.get(fieldName);
			if(value==null) {
				System.err.println("Mongo entry missing " + fieldName + " field.");
				continue;
			}
			
			final Class<?> paramType = m.getParameterTypes()[0];
			if(value instanceof BasicDBObject) {
				try {
					Method getter = msg.getClass().getMethod("get"+name);
					createMessage(getter.invoke(msg), (BasicDBObject)value);
				} 
				catch (Exception e) {
					// TODO proper logging
					System.err.println("Failed to get message field '" + fieldName + "'" +
							". Error: " + e.getMessage());
					e.printStackTrace();
				}
			}
			// Convert from Date to ROS Time
			else if(paramType == Time.class && value instanceof java.util.Date) {
				GregorianCalendar c = new GregorianCalendar();
				c.setTime((java.util.Date)value);
				long ms = c.getTimeInMillis();
				
				Time t  = new Time();
				t.secs  = (int) (ms / 1000);
				t.nsecs = (int) (ms % 1000) * 1000000;
				setMessageValue(m, fieldName, msg, t);
			}
			// Convert from number to byte
			else if(paramType == byte.class && value instanceof Number) {
				setMessageValue(m, fieldName, msg, ((Number)value).byteValue());
			}
			// Convert byte array to ChannelBuffer
			else if(paramType == ChannelBuffer.class) {
				setMessageValue(m, fieldName, msg, ChannelBuffers.copiedBuffer(ByteOrder.LITTLE_ENDIAN, (byte[])value));
			}
			// Use value as is
			else {
				setMessageValue(m, fieldName, msg, value);
			}
		}
	}

	private void setMessageValue(Method m, String fieldName, Object msg, Object value) {
		try {
			m.invoke(msg, value);
		}
		catch (Exception e) {
			// TODO proper logging
			System.err.println("Failed to set message field '" + fieldName + "'" +
					". Value type: " + value.getClass().getName() +
					". Method argument type: " + m.getParameterTypes()[0].getName() +
					". Error: " + e.getMessage());
			e.printStackTrace();
		}
	}
}
