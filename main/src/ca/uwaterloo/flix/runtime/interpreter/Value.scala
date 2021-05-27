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

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.FinalAst.{Expression, Root}
import ca.uwaterloo.flix.language.ast.FinalAst.Expression.KLabel
import ca.uwaterloo.flix.language.ast.{FinalAst, MonoType, Symbol}
import ca.uwaterloo.flix.util.InternalRuntimeException

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

  // lit is a channel or Guard
  case class Guard(lit: AnyRef, from: List[String], to: List[String]) extends Value {
    def getChannel: Channel = lit match {
      case c: Channel => c
      case g: Guard => g.getChannel
    }

    def getAllLabels: List[(KLabel, KLabel)] = lit match {
      case _: Channel => List((to, from))
      case g: Guard => (to, from) :: g.getAllLabels
    }

    override def toString: String = {
      getAllLabels.map(l => s"${l._1} <- ${l._2}").mkString("[", ", ", "]")
    }
  }

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

}