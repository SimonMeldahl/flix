/*
 * Copyright 2017 Magnus Madsen
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package ca.uwaterloo.flix.runtime.interpreter

import ca.uwaterloo.flix.runtime.interpreter.{Channel => JavaChannel}
import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.FinalAst.{Expression, Root}
import ca.uwaterloo.flix.language.ast.FinalAst.Expression.KLabel
import ca.uwaterloo.flix.language.ast.{FinalAst, MonoType, SourceLocation, Symbol}
import ca.uwaterloo.flix.util.{InternalCompilerException, InternalRuntimeException}

import java.util.concurrent.locks.{Condition, Lock, ReentrantLock}
import scala.util.Random

sealed trait Value

object Value {

  object Unit extends Value

  object True extends Value

  object False extends Value

  case class Char(lit: scala.Char) extends Value

  case class Float32(lit: scala.Float) extends Value

  case class Float64(lit: scala.Double) extends Value

  case class Int8(lit: scala.Byte) extends Value

  case class Int16(lit: scala.Short) extends Value

  case class Int32(lit: scala.Int) extends Value

  case class Int64(lit: scala.Long) extends Value

  case class BigInt(lit: java.math.BigInteger) extends Value

  case class Str(lit: java.lang.String) extends Value

  case class Lambda(closure: AnyRef, fromLabel: KLabel, toLabel: KLabel) extends Value {
    def call(args: List[Expression], env0: Map[String, AnyRef], lenv0: Map[Symbol.LabelSym, Expression], root: Root)(implicit flix: Flix): AnyRef = {
      val kargs = args.map(e => Expression.K(e, fromLabel = toLabel, toLabel = fromLabel, e.tpe, e.loc))
      closure match {
        case c: Closure =>
          Interpreter.reduceK(fromLabel, toLabel, Interpreter.invokeClo(c, kargs, env0, lenv0, root, fromLabel))
        case l: Lambda => Interpreter.reduceK(fromLabel, toLabel, l.call(kargs, env0, lenv0, root))
      }
    }
  }

  class Box extends Value {
    /**
      * The internal value of the box.
      */
    private var value: AnyRef = _

    /**
      * Returns the value inside the box.
      */
    def getValue: AnyRef = value

    /**
      * Mutates the value inside the box.
      */
    def setValue(x: AnyRef): Unit = {
      value = x
    }

    final override def equals(obj: scala.Any): Boolean = throw InternalRuntimeException(s"Value.Box does not support `equals`.")

    final override def hashCode(): Int = throw InternalRuntimeException(s"Value.Box does not support `hashCode`.")

    final override def toString: String = throw InternalRuntimeException(s"Value.Box does not support `toString`.")
  }

  case class Closure(sym: Symbol.DefnSym, bindings: Array[AnyRef]) extends Value {
    final override def equals(obj: scala.Any): Boolean = throw InternalRuntimeException(s"Value.Closure does not support `equals`.")

    final override def hashCode(): Int = throw InternalRuntimeException(s"Value.Closure does not support `hashCode`.")

    final override def toString: String = throw InternalRuntimeException(s"Value.Closure does not support `toString`.")
  }

  case class Tag(enum: Symbol.EnumSym, tag: String, value: AnyRef) extends Value {
    def getTag: String = tag

    def getBoxedTagValue: AnyRef = value

    final override def equals(obj: scala.Any): Boolean = throw InternalRuntimeException(s"Value.Tag does not support `equals`.")

    final override def hashCode(): Int = throw InternalRuntimeException(s"Value.Tag does not support `hashCode`.")

    final override def toString: String = throw InternalRuntimeException(s"Value.Tag does not support `toString`.")
  }

  case class Tuple(elms: List[AnyRef]) extends Value {
    def getBoxedValue: Array[AnyRef] = elms.toArray

    final override def equals(obj: scala.Any): Boolean = throw InternalRuntimeException(s"Value.Tuple does not support `equals`.")

    final override def hashCode(): Int = throw InternalRuntimeException(s"Value.Tuple does not support `hashCode`.")

    final override def toString: String = throw InternalRuntimeException(s"Value.Tuple does not support `toString`.")
  }

  case object RecordEmpty extends Value

  case class RecordExtension(base: AnyRef, field: String, value: AnyRef) extends Value

  case class Arr(elms: Array[AnyRef], tpe: MonoType) {
    final override def equals(obj: scala.Any): Boolean = throw InternalRuntimeException(s"Value.Arr does not support `equals`.")

    final override def hashCode(): Int = throw InternalRuntimeException(s"Value.Arr does not support `hashCode`.")

    final override def toString: String = throw InternalRuntimeException(s"Value.Arr does not support `toString`.")
  }

  sealed trait Channel extends Value {
    def get()(implicit loc: SourceLocation): AnyRef
    def put(e: AnyRef)(implicit loc: SourceLocation): Channel
  }

  object Channel {
    def select(selectObjects: List[AnyRef], hasDefault: Boolean, loc: SourceLocation): SelectChoice = {
      // Create new Condition and channelLock the current thread
      val selectLock: Lock = new ReentrantLock()
      val condition: Condition = selectLock.newCondition()
      val channels: List[JavaChannel] = selectObjects.map{
        case Value.ChannelImpl(c, _) => c
        case g: Value.Guard => g.getChannel.c
        case ref => throw InternalRuntimeException(s"Unexpected non-channel or non-guard value: ${ref.getClass.getName}.")
      }
      // Sort channels to avoid deadlock when locking
      val sortedChannels: List[JavaChannel] = JavaChannel.sortChannels(channels.toArray).toList
      while (!Thread.interrupted()) {
        // Lock all channels in sorted order
        JavaChannel.lockAllChannels(sortedChannels.toArray)
        try {
          // Lock the select lock after the channels
          selectLock.lock()

          try {
            // Find channels with waiting elements in a random order to prevent backpressure.
            {
              // Build list mapping a channel to it's branchNumber (the index of the 'channels' array)
              var channelIndexPairs = selectObjects.zip(selectObjects.indices)

              // Randomize the order channels are looked at.
              // This prevents backpressure from building up on one channel.
              channelIndexPairs = Random.shuffle(channelIndexPairs)

              // Find channels with waiting elements
              for (channelIndexPair <- channelIndexPairs) {
                val element = channelIndexPair match {
                  case (c: ChannelImpl, _) =>
                    println(s"tryGet channel with no guard @$loc")
                    c.c.tryGet()
                  //TODO(LBS) check if we are allowed to try and get from the channel if there is a guard
                  case (g: Guard, _) =>
                    println(s"tryGet channel with guard ${g.toString} @$loc")
                    g.getChannel.c.tryGet()
                }

                if (element != null) {
                  // There is a waiting element in this channel.
                  // Return the element and the branchNumber of this channel
                  return new SelectChoice(channelIndexPair._2, element)
                }
              }
            }

            // No element was found.

            // If there is a default case, choose this
            if (hasDefault) {
              return SelectChoice.DEFAULT_CHOICE
            }

            // Add our condition to all channels to get notified when a new element is added
            for (channel <- selectObjects) {
              channel match {
                case (c: ChannelImpl, _) => c.c.addGetter(selectLock, condition)
                //TODO(LBS) check if we are allowed to add getter notification to the channel if there is a guard
                case (g: Guard, _) => g.getChannel.c.addGetter(selectLock, condition)
              }
            }
          } finally {
            // Unlock all channels in sorted order, so other threads may input elements
            JavaChannel.unlockAllChannels(sortedChannels.toArray)
          }

          // Wait for an element to be added to any of the channels
          condition.await()
        } catch {
          case _: InterruptedException => throw new RuntimeException("Thread interrupted");
        } finally {
          // Unlock the selectLock, which is relevant when a different thread wants to put
          // an element into a channel that was not selected from the select.
          // This other channel will then signal the condition from selectLock (in the put method),
          // so it needs the lock.
          selectLock.unlock()
        }
      }

      throw new RuntimeException("Thread interrupted");
    }
  }

  case class ChannelImpl(c: JavaChannel, pols: Option[List[String]]) extends Channel {
    def get()(implicit loc: SourceLocation): AnyRef = {
      println(s"get channel with no guard @$loc")
      c.get()
    }
    def put(e: AnyRef)(implicit loc: SourceLocation): Channel = {
      println(s"put channel with no guard @$loc")
      c.put(e)
      this
    }
  }

  case class Guard(lit: Channel, from: List[String], to: List[String]) extends Channel {
    def getChannel: ChannelImpl = lit match {
      case c: ChannelImpl => c
      case g: Guard => g.getChannel
    }

    def getAllLabels: List[(KLabel, KLabel)] = lit match {
      case _: ChannelImpl => List((to, from))
      case g: Guard => (to, from) :: g.getAllLabels
    }

    override def toString: String = {
      getAllLabels.map(l => s"${l._1} <- ${l._2}").mkString("[", ", ", "]")
    }

    override def get()(implicit loc: SourceLocation): AnyRef = {
      println(s"get channel with guard ${toString} @$loc")
      getChannel.c.get()
    }

    override def put(e: AnyRef)(implicit loc: SourceLocation): Channel = {
      println(s"put channel with guard ${toString} @$loc")
      getChannel.c.put(e)
      this
    }
  }
}