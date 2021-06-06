/*
 * Copyright 2020 Matthew Lutze, Magnus Madsen
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

import ca.uwaterloo.flix.language.ast.{Contract, ContractConstructor}

sealed trait ContractDiff {

  /**
    * Returns the contract constructor of `this` contract.
    */
  def contractConstructor: ContractDiff = this match {
    case ContractDiff.Apply(t1, _) => t1.contractConstructor
    case _ => this
  }

  /**
    * Returns the contract parameters of `this` contract.
    */
  def contractArguments: List[ContractDiff] = this match {
    case ContractDiff.Apply(t1, t2) => t1.contractArguments ::: t2 :: Nil
    case _ => Nil
  }

}

object ContractDiff {

  /**
    * Represents a tuple.
    */
  case object Tuple extends ContractDiff

  /**
    * Represents an enum.
    */
  case object Enum extends ContractDiff

  /**
    * Represents an arrow contract.
    */
  case object Arrow extends ContractDiff

  /**
    * Represents a miscellaneous contract.
    */
  case object Other extends ContractDiff

  /**
    * Represents a contract application.
    */
  case class Apply(tpe1: ContractDiff, tpe2: ContractDiff) extends ContractDiff

  /**
    * Represents two mismatched contracts.
    */
  case class Mismatch(tpe1: Contract, tpe2: Contract) extends ContractDiff

  /**
    * Construct a ContractDiff from the given contracts.
    *
    * Recursively searches the contract structure, identifying mismatches between the two contracts. The resulting ContractDiff has
    * the same shape as the given contracts as far as they are identical, but replacing differences with a `Mismatch`.
    *
    * Does not handle schema or record contracts as their equality requires different logic.
    *
    * Examples:
    *   * `diff(Int32, Int32) => Other`
    *   * `diff((Int32, Int32), (Int32, Bool)) => Tuple[Other, Mismatch(Int32, Bool)]`
    *   * `diff(Int32 -> Int32, Int32 -> Bool) => Arrow[Other, Mismatch(Int32, Bool)]`
    *   * `diff(Option[Int32], Option[Bool]) => Enum[Mismatch(Int32, Bool)]`
    */
  def diff(tpe1: Contract, tpe2: Contract): ContractDiff = {
    val tyCon1 = tpe1.contractConstructor
    val tyCon2 = tpe2.contractConstructor

    (tyCon1, tyCon2) match {
      case (None, _) => ContractDiff.Other
      case (_, None) => ContractDiff.Other
      case (Some(tc1), Some(tc2)) => (tc1, tc2) match {
        case (ContractConstructor.Tuple(len1), ContractConstructor.Tuple(len2)) if (len1 == len2) =>
          val diffs = (tpe1.contractArguments zip tpe2.contractArguments).map { case (t1, t2) => diff(t1, t2) }
          mkApply(ContractDiff.Tuple, diffs)
        case (ContractConstructor.Enum(sym1, kind1), ContractConstructor.Enum(sym2, kind2)) if ((sym1 == sym2) && (kind1 == kind2)) =>
          val diffs = (tpe1.contractArguments zip tpe2.contractArguments).map { case (t1, t2) => diff(t1, t2) }
          mkApply(ContractDiff.Enum, diffs)
        case (ContractConstructor.Arrow(len1), ContractConstructor.Arrow(len2)) if (len1 == len2) =>
          val diffs = (tpe1.contractArguments.tail zip tpe2.contractArguments.tail).map { case (t1, t2) => diff(t1, t2) }
          mkApply(ContractDiff.Arrow, diffs)
        case _ => if (tc1 == tc2) ContractDiff.Other else ContractDiff.Mismatch(tpe1, tpe2)
      }
    }
  }

  private def mkApply(base: ContractDiff, params: List[ContractDiff]): ContractDiff = {
    params.foldLeft(base)((base0, param) => ContractDiff.Apply(base0, param))
  }
}

