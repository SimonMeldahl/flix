/*
 * Copyright 2020 Matthew Lutze
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


package ca.uwaterloo.flix.language.debug

import ca.uwaterloo.flix.language.ast.Kind.Bool
import ca.uwaterloo.flix.language.ast.{Kind, Contract, ContractConstructor}
import ca.uwaterloo.flix.util.InternalCompilerException
import ca.uwaterloo.flix.util.vt.{VirtualString, VirtualTerminal}

object FormatContract {

  def formatContract(tpe: Contract)(implicit audience: Audience): String = {

    val renameMap = alphaRenameVars(tpe)

    def formatWellFormedRecord(record: Contract): String = flattenRecord(record) match {
      case FlatNestable(fields, Contract.Cst(ContractConstructor.RecordEmpty, _)) =>
        fields.map { case (field, tpe) => formatRecordField(field, tpe) }.mkString("{ ", ", ", " }")
      case FlatNestable(fields, rest) =>
        val fieldString = fields.map { case (field, tpe) => formatRecordField(field, tpe) }.mkString(", ")
        s"{ $fieldString | ${visit(rest)} }"
    }

    def formatWellFormedSchema(schema: Contract): String = flattenSchema(schema) match {
      case FlatNestable(fields, Contract.Cst(ContractConstructor.SchemaEmpty, _)) =>
        fields.map { case (field, tpe) => formatSchemaField(field, tpe) }.mkString("#{ ", ", ", " }")
      case FlatNestable(fields, rest) =>
        val fieldString = fields.map { case (field, tpe) => formatSchemaField(field, tpe) }.mkString(", ")
        s"#{ $fieldString | ${visit(rest)} }"
    }

    def formatRecordField(field: String, tpe: Contract): String = {
      s"$field: ${visit(tpe)}"
    }

    def formatSchemaField(name: String, tpe: Contract): String = {
      val contractConstructor = tpe.contractConstructor
      val fullName = contractConstructor match {
        case Some(ContractConstructor.Relation) => name
        case Some(ContractConstructor.Lattice) => s"$name<>"
        case _ => s"$name?"
      }
      val arg = contractConstructor match {
        case Some(ContractConstructor.Relation) | Some(ContractConstructor.Lattice) =>
          tpe.contractArguments match {
            case Nil => "(???)"
            case arg :: tail => arg.contractConstructor match {
              case Some(ContractConstructor.Unit) => formatApply("()", tail)
              case Some(ContractConstructor.Tuple(_)) => formatApply(formatContract(arg), tail)
              case _ => formatApply(s"(${formatContract(arg)})", tail)
            }
          }
        case Some(ContractConstructor.Unit) => "()"
        case Some(ContractConstructor.Tuple(_)) => formatContract(tpe)
        case _ => s"(${formatContract(tpe)})"

      }
      s"$fullName$arg"
    }

    def formatApply(name: String, args: List[Contract]): String = {
      if (args.isEmpty)
        name
      else
        name + "[" + args.map(visit).mkString(", ") + "]"
    }


    def visit(tpe: Contract)(implicit audience: Audience): String = {
      val base = tpe.contractConstructor
      val args = tpe.contractArguments

      base match {
        case None => tpe match {
          case tvar@Contract.Var(id, kind, _, _) => audience match {
            case Audience.Internal => kind match {
              case Bool => s"''$id"
              case _ => s"'$id"
            }
            case Audience.External => tvar.text.getOrElse(renameMap(tvar.id))
          }
          case Contract.Lambda(tvar, tpe) => audience match {
            case Audience.Internal => s"${tvar.id.toString} => ${visit(tpe)}"
            case Audience.External => s"${tvar.text.getOrElse(renameMap(tvar.id))} => ${visit(tpe)}"
          }
          case Contract.Apply(tpe1, tpe2) => s"${visit(tpe1)}[${visit(tpe2)}]"
          case _ => throw InternalCompilerException(s"Unexpected contract: '${tpe.getClass}'.") // TODO: This can lead to infinite recursion.
        }

        case Some(tc) => tc match {
          case ContractConstructor.Unit => formatApply("Unit", args)

          case ContractConstructor.Null => formatApply("Null", args)

          case ContractConstructor.Bool => formatApply("Bool", args)

          case ContractConstructor.Char => formatApply("Char", args)

          case ContractConstructor.Float32 => formatApply("Float32", args)

          case ContractConstructor.Float64 => formatApply("Float64", args)

          case ContractConstructor.Int8 => formatApply("Int8", args)

          case ContractConstructor.Int16 => formatApply("Int16", args)

          case ContractConstructor.Int32 => formatApply("Int32", args)

          case ContractConstructor.Int64 => formatApply("Int64", args)

          case ContractConstructor.BigInt => formatApply("BigInt", args)

          case ContractConstructor.Str => formatApply("String", args)

          case ContractConstructor.RecordEmpty => formatApply("{ }", args)

          case ContractConstructor.SchemaEmpty => formatApply("#{ }", args)

          case ContractConstructor.True => formatApply("true", args)

          case ContractConstructor.False => formatApply("false", args)

          case ContractConstructor.Array => formatApply("Array", args)

          case ContractConstructor.Channel => formatApply("Channel", args)

          case ContractConstructor.Enum(sym, _) => formatApply(sym.toString, args)

          case ContractConstructor.Lattice => formatApply("Lattice", args)

          case ContractConstructor.Relation => formatApply("Relation", args)

          case ContractConstructor.Lazy => formatApply("Lazy", args)

          case ContractConstructor.Ref => formatApply("Ref", args)

          case ContractConstructor.RecordExtend(field) => args.length match {
            case 0 => s"{ $field: ??? }"
            case 1 => s"{ $field: ${visit(args.head)} | ??? }"
            case 2 => formatWellFormedRecord(tpe)
            case _ => formatApply(s"RecordExtend($field)", args)
          }

          case ContractConstructor.SchemaExtend(pred) => args.length match {
            case 0 => s"#{ ${pred.name}?(???) }"
            case 1 => s"#{ ${formatSchemaField(pred.name, args.head)} | ??? }"
            case 2 => formatWellFormedSchema(tpe)
            case _ => formatApply(s"SchemaExtend($pred)", args)
          }

          case ContractConstructor.Tuple(length) =>
            val elements = args.take(length).map(visit)
            val applyParams = args.drop(length) // excess elements
            val tuple = elements.padTo(length, "???").mkString("(", ", ", ")")
            formatApply(tuple, applyParams)

          case ContractConstructor.Tag(sym, tag) => // TODO better unhappy case handling
            if (args.lengthIs == 2)
              s"${tag.name}${args.head}"
            else
              formatApply(tag.name, args)

          case ContractConstructor.Not => args match {
            case (t1: Contract.Var) :: Nil => s"¬${visit(t1)}"
            case t1 :: Nil => s"¬(${visit(t1)})"
            case Nil => "¬???"
            case _ => formatApply("¬", args)
          }

          case ContractConstructor.And => args match {
            case (t1: Contract.Var) :: (t2: Contract.Var) :: Nil => s"${visit(t1)} ∧ ${visit(t2)}"
            case (t1: Contract.Var) :: t2 :: Nil => s"${visit(t1)} ∧ (${visit(t2)})"
            case t1 :: (t2: Contract.Var) :: Nil => s"(${visit(t1)}) ∧ ${visit(t2)}"
            case t1 :: t2 :: Nil => s"(${visit(t1)}) ∧ (${visit(t2)})"
            case (t1: Contract.Var) :: Nil => s"${visit(t1)} ∧ ???"
            case t1 :: Nil => s"(${visit(t1)}) ∧ ???"
            case Nil => s"??? ∧ ???"
            case _ => formatApply("∧", args)
          }

          case ContractConstructor.Or => args match {
            case (t1: Contract.Var) :: (t2: Contract.Var) :: Nil => s"${visit(t1)} ∨ ${visit(t2)}"
            case (t1: Contract.Var) :: t2 :: Nil => s"${visit(t1)} ∨ (${visit(t2)})"
            case t1 :: (t2: Contract.Var) :: Nil => s"(${visit(t1)}) ∨ ${visit(t2)}"
            case t1 :: t2 :: Nil => s"(${visit(t1)}) ∨ (${visit(t2)})"
            case (t1: Contract.Var) :: Nil => s"${visit(t1)} ∨ ???"
            case t1 :: Nil => s"(${visit(t1)}) ∨ ???"
            case Nil => s"??? ∨ ???"
            case _ => formatApply("∨", args)
          }

          case ContractConstructor.Arrow(arity) =>
            if (arity < 2) {
              formatApply(s"Arrow$arity", args)
            } else {

              // Retrieve and result contract.
              val eff = args.head
              val contractArgs = args.slice(1, arity + 1)
              val contracts = contractArgs.map(visit)
              val applyParams = contractArgs.drop(arity + 1) // excess args
              val contractStrings = contracts.padTo(arity, "???")

              // Format the arguments.
              val argPart = contractStrings.init.mkString(" -> ")
              // Format the arrow.
              val arrowPart = eff match {
                case Contract.Cst(ContractConstructor.False, _) => " ~> "
                case _ => " -> "
              }
              // Format the effect.
              val effPart = eff match {
                case Contract.Cst(ContractConstructor.True, _) => ""
                case Contract.Cst(ContractConstructor.False, _) => ""
                case _: Contract.Var => s" & ${visit(eff)}"
                case _ => " & (" + visit(eff) + ")"
              }
              // Format the result contract.
              val resultPart = contractStrings.last

              // Put everything together.
              val applyPart = argPart + arrowPart + resultPart + effPart
              if (applyParams.isEmpty) {
                applyPart
              } else {
                formatApply(s"($applyPart)", applyParams)
              }
            }

          case ContractConstructor.Native(clazz) => s"${clazz.getSimpleName}"
        }
      }
    }

    visit(tpe)
  }

  /**
    * Returns a human readable representation of the given contract difference.
    */
  def formatContractDiff(td: ContractDiff, color: String => VirtualString)(implicit audience: Audience): VirtualTerminal = {
    val vt = new VirtualTerminal()

    def visit(d: ContractDiff): Unit = {
      val base = d.contractConstructor
      val args = d.contractArguments

      base match {
        case ContractDiff.Arrow =>
          intercalate(args, visit, vt, before = "", separator = " -> ", after = "")
        case ContractDiff.Enum =>
          vt << "..."
          intercalate(args, visit, vt, before = "[", separator = ", ", after = "]")
        case ContractDiff.Tuple =>
          intercalate(args, visit, vt, before = "(", separator = ", ", after = ")")
        case ContractDiff.Other =>
          vt << "..."
          intercalate(args, visit, vt, before = "[", separator = ", ", after = "]")
        case ContractDiff.Mismatch(tpe1, _) => vt << color(formatContract(tpe1))
        case _ => throw InternalCompilerException(s"Unexpected base contract: '$base'.")
      }
    }

    visit(td)

    vt
  }

  /**
    * Helper function to generate text before, in the middle of, and after a list of items.
    */
  private def intercalate[A](xs: List[A], f: A => Unit, vt: VirtualTerminal, before: String, separator: String, after: String): Unit = {
    if (xs.isEmpty) return
    vt << before
    var first: Boolean = true
    for (x <- xs) {
      if (first) {
        f(x)
      } else {
        vt << separator
        f(x)
      }
      first = false
    }
    vt << after
  }


  /**
    * A flat representation of a schema or record.
    *
    * Contains the fields and their contracts as a list at the top level.
    * This better mirrors the structure of records and schemas as they are displayed (e.g. `{ x: Int8, y: Bool | r }`)
    * rather than their true underlying shape (e.g. `{ x: Int8 | { y: Bool | r } }`).
    */
  private case class FlatNestable(fields: List[(String, Contract)], rest: Contract) {
    def ::(head: (String, Contract)): FlatNestable = {
      copy(fields = head :: fields)
    }
  }

  /**
    * Convert a record to a [[FlatNestable]].
    */
  private def flattenRecord(record: Contract): FlatNestable = record match {
    case Contract.Apply(Contract.Apply(Contract.Cst(ContractConstructor.RecordExtend(field), _), tpe), rest) =>
      (field.name, tpe) :: flattenRecord(rest)
    case _ => FlatNestable(Nil, record)
  }

  /**
    * Convert a schema to a [[FlatNestable]].
    */
  private def flattenSchema(schema: Contract): FlatNestable = schema match {
    case Contract.Apply(Contract.Apply(Contract.Cst(ContractConstructor.SchemaExtend(pred), _), tpe), rest) =>
      (pred.name, tpe) :: flattenSchema(rest)
    case _ => FlatNestable(Nil, schema)
  }


  /**
    * Get the var name for the given index.
    * Maps `0-25` to `a-z`,
    * then `26-51` to `a1-z1`,
    * then `52-77` to `a2-z2`,
    * etc.
    */
  private def getVarName(index: Int): String = {
    if (index / 26 <= 0)
      "'" + (index + 'a').toChar.toString
    else
      "'" + (index + 'a').toChar.toString + (index / 26).toString
  }

  /**
    * Rename the variables in the given contract.
    */
  private def alphaRenameVars(tpe0: Contract): Map[Int, String] = {
    val tvars = contractVars(tpe0)
    val starContractVars = tvars.filter(_.kind == Kind.Star)
    val boolContractVars = tvars.filter(_.kind == Kind.Bool)
    val otherContractVars = tvars.filter(k => k.kind != Kind.Star && k.kind != Kind.Bool)
    val orderedContractVars = starContractVars ::: boolContractVars ::: otherContractVars

    orderedContractVars.zipWithIndex.map {
      case (tvar, index) => tvar.id -> getVarName(index)
    }.toMap
  }

  /**
    * Returns all contract variables in the contract in the order in which they appear.
    */
  private def contractVars(tpe0: Contract): List[Contract.Var] = {
    def visit(t: Contract): List[Contract.Var] = t match {
      case tvar: Contract.Var => tvar :: Nil
      case Contract.Cst(tc, loc) => Nil
      case Contract.Lambda(tvar, tpe) => tvar :: visit(tpe)
      case Contract.Apply(tpe1, tpe2) => visit(tpe1) ::: visit(tpe2)
    }

    visit(tpe0).distinct
  }

}
