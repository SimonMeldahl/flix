package ca.uwaterloo.flix.language.ast


import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.debug.{Audience, FormatContract}
import ca.uwaterloo.flix.util.InternalCompilerException

import scala.collection.immutable.SortedSet

/**
  * Representation of contracts.
  */
sealed trait Contract {

  /**
    * The kind of `this` contract.
    */
  def kind: Kind

  /**
    * Returns the contract variables in `this` contract.
    *
    * Returns a sorted set to ensure that the compiler is deterministic.
    */
  def contractVars: SortedSet[Contract.Var] = this match {
    case x: Contract.Var => SortedSet(x)
    case Contract.Cst(tc, _) => SortedSet.empty
    case Contract.Lambda(tvar, tpe) => tpe.contractVars - tvar
    case Contract.Apply(tpe1, tpe2) => tpe1.contractVars ++ tpe2.contractVars
  }

  /**
    * Optionally returns the contract constructor of `this` contract.
    *
    * Return `None` if the contract constructor is a variable.
    *
    * Otherwise returns `Some(tc)` where `tc` is the left-most contract constructor.
    *
    * For example,
    *
    * {{{
    * x                             =>      None
    * Celsius                       =>      Some(Celsius)
    * Option[Int]                   =>      Some(Option)
    * Arrow[Bool, Char]             =>      Some(Arrow)
    * Tuple[Bool, Int]              =>      Some(Tuple)
    * Result[Bool, Int]             =>      Some(Result)
    * Result[Bool][Int]             =>      Some(Result)
    * Option[Result[Bool, Int]]     =>      Some(Option)
    * }}}
    */
  def contractConstructor: Option[ContractConstructor] = this match {
    case Contract.Var(_, _, _, _) => None
    case Contract.Cst(tc, _) => Some(tc)
    case Contract.Apply(t1, _) => t1.contractConstructor
    case Contract.Lambda(_, _) => throw InternalCompilerException(s"Unexpected contract constructor: Lambda.")
  }

  /**
    * Returns a list of all contract constructors in `this` contract.
    */
  def contractConstructors: List[ContractConstructor] = this match {
    case Contract.Var(_, _, _, _) => Nil
    case Contract.Cst(tc, _) => tc :: Nil
    case Contract.Apply(t1, t2) => t1.contractConstructors ::: t2.contractConstructors
    case Contract.Lambda(_, _) => throw InternalCompilerException(s"Unexpected contract constructor: Lambda.")
  }

  /**
    * Returns the contract arguments of `this` contract.
    *
    * For example,
    *
    * {{{
    * Celsius                       =>      Nil
    * Option[Int]                   =>      Int :: Nil
    * Arrow[Bool, Char]             =>      Bool :: Char :: Nil
    * Tuple[Bool, Int]              =>      Bool :: Int :: Nil
    * Result[Bool, Int]             =>      Bool :: Int :: Nil
    * Result[Bool][Int]             =>      Bool :: Int :: Nil
    * Option[Result[Bool, Int]]     =>      Result[Bool, Int] :: Nil
    * }}}
    */
  def contractArguments: List[Contract] = this match {
    case Contract.Apply(tpe1, tpe2) => tpe1.contractArguments ::: tpe2 :: Nil
    case _ => Nil
  }

  /**
    * Applies `f` to every contract variable in `this` contract.
    */
  def map(f: Contract.Var => Contract): Contract = this match {
    case tvar: Contract.Var => f(tvar)
    case Contract.Cst(_, _) => this
    case Contract.Lambda(tvar, tpe) => Contract.Lambda(tvar, tpe.map(f))
    case Contract.Apply(tpe1, tpe2) => Contract.Apply(tpe1.map(f), tpe2.map(f))
  }

  /**
    * Returns the argument contracts of `this` arrow contract.
    *
    * NB: Assumes that `this` contract is an arrow.
    */
  def arrowArgContracts: List[Contract] = contractConstructor match {
    case Some(ContractConstructor.Arrow(n)) => contractArguments.drop(1).dropRight(1)
    case _ => throw InternalCompilerException(s"Unexpected non-arrow contract: '$this'.")
  }

  /**
    * Returns the result contract of `this` arrow contract.
    *
    * NB: Assumes that `this` contract is an arrow.
    */
  def arrowResultContract: Contract = contractConstructor match {
    case Some(ContractConstructor.Arrow(n)) => contractArguments.last
    case _ => throw InternalCompilerException(s"Unexpected non-arrow contract: '$this'.")
  }

  /**
    * Returns the effect contract of `this` arrow contract.
    *
    * NB: Assumes that `this` contract is an arrow.
    */
  def arrowEffectContract: Contract = contractConstructor match {
    case Some(ContractConstructor.Arrow(n)) => contractArguments.head
    case _ => throw InternalCompilerException(s"Unexpected non-arrow contract: '$this'.")
  }

  /**
    * Returns the size of `this` contract.
    */
  def size: Int = this match {
    case Contract.Var(_, _, _, _) => 1
    case Contract.Cst(tc, _) => 1
    case Contract.Lambda(_, tpe) => tpe.size + 1
    case Contract.Apply(tpe1, tpe2) => tpe1.size + tpe2.size + 1
  }

  /**
    * Returns a human readable string representation of `this` contract.
    */
  override def toString: String = FormatContract.formatContract(this)(Audience.Internal)

}

object Contract {

  /////////////////////////////////////////////////////////////////////////////
  // Constants                                                               //
  /////////////////////////////////////////////////////////////////////////////

  // TODO: Reduce usage of these in favor of the ones with source locations.

  val WildCard: Contract = Contract.Cst(ContractConstructor.WildCard, SourceLocation.Unknown)

  val WhiteList: Contract = Contract.Cst(ContractConstructor.WhiteList)

  /**
    * Represents the Unit contract.
    */
  val Unit: Contract = Contract.Cst(ContractConstructor.Unit, SourceLocation.Unknown)

  /**
    * Represents the Null contract.
    */
  val Null: Contract = Contract.Cst(ContractConstructor.Null, SourceLocation.Unknown)

  /**
    * Represents the Bool contract.
    */
  val Bool: Contract = Contract.Cst(ContractConstructor.Bool, SourceLocation.Unknown)

  /**
    * Represents the Char contract.
    */
  val Char: Contract = Contract.Cst(ContractConstructor.Char, SourceLocation.Unknown)

  /**
    * Represents the Float32 contract.
    */
  val Float32: Contract = Contract.Cst(ContractConstructor.Float32, SourceLocation.Unknown)

  /**
    * Represents the Float64 contract.
    */
  val Float64: Contract = Contract.Cst(ContractConstructor.Float64, SourceLocation.Unknown)

  /**
    * Represents the Int8 contract.
    */
  val Int8: Contract = Contract.Cst(ContractConstructor.Int8, SourceLocation.Unknown)

  /**
    * Represents the Int16 contract.
    */
  val Int16: Contract = Contract.Cst(ContractConstructor.Int16, SourceLocation.Unknown)

  /**
    * Represents the Int32 contract.
    */
  val Int32: Contract = Contract.Cst(ContractConstructor.Int32, SourceLocation.Unknown)

  /**
    * Represents the Int64 contract.
    */
  val Int64: Contract = Contract.Cst(ContractConstructor.Int64, SourceLocation.Unknown)

  /**
    * Represents the BigInt contract.
    */
  val BigInt: Contract = Contract.Cst(ContractConstructor.BigInt, SourceLocation.Unknown)

  /**
    * Represents the String contract.
    */
  val Str: Contract = Contract.Cst(ContractConstructor.Str, SourceLocation.Unknown)

  /**
    * Represents the Array contract constructor.
    *
    * NB: This contract has kind: * -> *.
    */
  val Array: Contract = Contract.Cst(ContractConstructor.Array, SourceLocation.Unknown)

  /**
    * Represents the Channel contract constructor.
    *
    * NB: This contract has kind: * -> *.
    */
  val Channel: Contract = Contract.Cst(ContractConstructor.Channel, SourceLocation.Unknown)

  /**
    * Represents the Lazy contract constructor.
    *
    * NB: This contract has kind: * -> *.
    */
  val Lazy: Contract = Contract.Cst(ContractConstructor.Lazy, SourceLocation.Unknown)

  /**
    * Represents the Reference contract constructor.
    *
    * NB: This contract has kind: * -> *.
    */
  val Ref: Contract = Contract.Cst(ContractConstructor.Ref, SourceLocation.Unknown)

  /**
    * Represents the Relation contract constructor.
    */
  val Relation: Contract = Contract.Cst(ContractConstructor.Relation, SourceLocation.Unknown)

  /**
    * Represents the Lattice contract constructor.
    */
  val Lattice: Contract = Contract.Cst(ContractConstructor.Lattice, SourceLocation.Unknown)

  /**
    * Represents the contract of an empty record.
    */
  val RecordEmpty: Contract = Contract.Cst(ContractConstructor.RecordEmpty, SourceLocation.Unknown)

  /**
    * Represents the contract of an empty schema.
    */
  val SchemaEmpty: Contract = Contract.Cst(ContractConstructor.SchemaEmpty, SourceLocation.Unknown)

  /**
    * Represents the Boolean True.
    */
  val True: Contract = Contract.Cst(ContractConstructor.True, SourceLocation.Unknown)

  /**
    * Represents the Boolean False.
    */
  val False: Contract = Contract.Cst(ContractConstructor.False, SourceLocation.Unknown)

  /**
    * Represents the Pure effect. (TRUE in the Boolean algebra.)
    */
  val Pure: Contract = True

  /**
    * Represents the Impure effect. (FALSE in the Boolean algebra.)
    */
  val Impure: Contract = False

  /**
    * Represents the Not contract constructor.
    *
    * NB: This contract has kind: * -> *.
    */
  val Not: Contract = Contract.Cst(ContractConstructor.Not, SourceLocation.Unknown)

  /**
    * Represents the And contract constructor.
    *
    * NB: This contract has kind: * -> (* -> *).
    */
  val And: Contract = Contract.Cst(ContractConstructor.And, SourceLocation.Unknown)

  /**
    * Represents the Or contract constructor.
    *
    * NB: This contract has kind: * -> (* -> *).
    */
  val Or: Contract = Contract.Cst(ContractConstructor.Or, SourceLocation.Unknown)

  /////////////////////////////////////////////////////////////////////////////
  // Constructors                                                            //
  /////////////////////////////////////////////////////////////////////////////

  /**
    * A contract variable expression.
    */
  case class Var(id: Int, kind: Kind, rigidity: Rigidity = Rigidity.Flexible, text: Option[String] = None) extends Contract with Ordered[Contract.Var] {
    /**
      * Returns `true` if `this` contract variable is equal to `o`.
      */
    override def equals(o: scala.Any): Boolean = o match {
      case that: Var => this.id == that.id
      case _ => false
    }

    /**
      * Returns the hash code of `this` contract variable.
      */
    override def hashCode(): Int = id

    /**
      * Compares `this` contract variable to `that` contract variable.
      */
    override def compare(that: Contract.Var): Int = this.id - that.id
  }

  /**
    * A contract represented by the contract constructor `tc`.
    */
  case class Cst(tc: ContractConstructor, loc: SourceLocation) extends Contract {
    def kind: Kind = tc.kind

    override def hashCode(): Int = tc.hashCode()

    override def equals(o: Any): Boolean = o match {
      case that: Cst => this.tc == that.tc
      case _ => false
    }
  }

  /**
    * A contract expression that represents a contract abstraction [x] => tpe.
    */
  case class Lambda(tvar: Contract.Var, tpe: Contract) extends Contract {
    def kind: Kind = Kind.Star ->: tpe.kind
  }

  /**
    * A contract expression that a represents a contract application tpe1[tpe2].
    */
  case class Apply(tpe1: Contract, tpe2: Contract) extends Contract {
    /**
      * Returns the kind of `this` contract.
      *
      * The kind of a contract application can unique be determined from the kind of the first contract argument `t1`.
      */
    val kind: Kind = {
      tpe1.kind match {
        case Kind.Arrow(k1, k2) =>
          // TODO: Kind check (but only for boolean formulas for now).
          //          if (k1 == Kind.Bool) {
          //            val k3 = tpe2.kind
          //            if (k3 != Kind.Bool && !k3.isInstanceOf[Kind.Var]) {
          //               throw InternalCompilerException(s"Unexpected non-bool kind: '$k3'.")
          //            }
          //          }
          k2
        case _ => throw InternalCompilerException(s"Illegal kind: '${tpe1.kind}' of contract '$tpe1'.")
      }
    }
  }

  /////////////////////////////////////////////////////////////////////////////
  // Utility Functions                                                       //
  /////////////////////////////////////////////////////////////////////////////

  /**
    * Returns a fresh contract variable of the given kind `k` and rigidity `r`.
    */
  def freshVar(k: Kind, r: Rigidity = Rigidity.Flexible, text: Option[String] = None)(implicit flix: Flix): Contract.Var = {
    val id = flix.genSym.freshId()
    Contract.Var(id, k, r, text)
  }

  /**
    * Returns the Unit contract with given source location `loc`.
    */
  def mkUnit(loc: SourceLocation): Contract = Contract.Cst(ContractConstructor.Unit, loc)

  /**
    * Returns the Null contract with the given source location `loc`.
    */
  def mkNull(loc: SourceLocation): Contract = Contract.Cst(ContractConstructor.Null, loc)

  /**
    * Returns the Bool contract with the given source location `loc`.
    */
  def mkBool(loc: SourceLocation): Contract = Contract.Cst(ContractConstructor.Bool, loc)

  /**
    * Returns the Char contract with the given source location `loc`.
    */
  def mkChar(loc: SourceLocation): Contract = Contract.Cst(ContractConstructor.Char, loc)

  /**
    * Returns the Float32 contract with the given source location `loc`.
    */
  def mkFloat32(loc: SourceLocation): Contract = Contract.Cst(ContractConstructor.Float32, loc)

  /**
    * Returns the Float64 contract with the given source location `loc`.
    */
  def mkFloat64(loc: SourceLocation): Contract = Contract.Cst(ContractConstructor.Float64, loc)

  /**
    * Returns the Int8 contract with the given source location `loc`.
    */
  def mkInt8(loc: SourceLocation): Contract = Contract.Cst(ContractConstructor.Int8, loc)

  /**
    * Returns the Int16 contract with the given source location `loc`.
    */
  def mkInt16(loc: SourceLocation): Contract = Contract.Cst(ContractConstructor.Int16, loc)

  /**
    * Returns the Int32 contract with the given source location `loc`.
    */
  def mkInt32(loc: SourceLocation): Contract = Contract.Cst(ContractConstructor.Int32, loc)

  /**
    * Returns the Int64 contract with the given source location `loc`.
    */
  def mkInt64(loc: SourceLocation): Contract = Contract.Cst(ContractConstructor.Int64, loc)

  /**
    * Returns the BigInt contract with the given source location `loc`.
    */
  def mkBigInt(loc: SourceLocation): Contract = Contract.Cst(ContractConstructor.BigInt, loc)

  /**
    * Returns the String contract with the given source location `loc`.
    */
  def mkString(loc: SourceLocation): Contract = Contract.Cst(ContractConstructor.Str, loc)

  /**
    * Returns the Array contract with the given source location `loc`.
    */
  def mkArray(loc: SourceLocation): Contract = Contract.Cst(ContractConstructor.Array, loc)

  /**
    * Returns the contract `Array[tpe]` with the given optional source location `loc`.
    */
  def mkArray(elmContract: Contract, loc: SourceLocation = SourceLocation.Unknown): Contract = Apply(Contract.Cst(ContractConstructor.Array, loc), elmContract)

  /**
    * Returns the Channel contract with the given source location `loc`.
    */
  def mkChannel(loc: SourceLocation): Contract = Contract.Cst(ContractConstructor.Channel, loc)

  /**
    * Returns the contract `Channel[tpe]` with the given optional source location `loc`.
    */
  def mkChannel(tpe: Contract, loc: SourceLocation = SourceLocation.Unknown): Contract = Contract.Apply(Contract.Cst(ContractConstructor.Channel, loc), tpe)

  /**
    * Returns the Lazy contract with the given source location `loc`.
    */
  def mkLazy(loc: SourceLocation): Contract = Contract.Cst(ContractConstructor.Lazy, loc)

  /**
    * Returns the contract `Lazy[tpe]` with the given optional source location `loc`.
    */
  def mkLazy(tpe: Contract, loc: SourceLocation = SourceLocation.Unknown): Contract = Contract.Apply(Contract.Cst(ContractConstructor.Lazy, loc), tpe)

  /**
    * Returns the Ref contract with the given source location `loc`.
    */
  def mkRef(loc: SourceLocation): Contract = Contract.Cst(ContractConstructor.Ref, loc)

  /**
    * Returns the contract `Ref[tpe]` with the given optional source location `loc`.
    */
  def mkRef(tpe: Contract, loc: SourceLocation = SourceLocation.Unknown): Contract = Contract.Apply(Contract.Cst(ContractConstructor.Ref, loc), tpe)

  /**
    * Constructs the pure arrow contract A -> B.
    */
  def mkPureArrow(a: Contract, b: Contract): Contract = mkArrowWithEffect(a, Pure, b)

  /**
    * Constructs the impure arrow contract A ~> B.
    */
  def mkImpureArrow(a: Contract, b: Contract): Contract = mkArrowWithEffect(a, Impure, b)

  /**
    * Constructs the arrow contract A -> B & e.
    */
  def mkArrowWithEffect(a: Contract, e: Contract, b: Contract): Contract = mkApply(Contract.Cst(ContractConstructor.Arrow(2), SourceLocation.Unknown), List(e, a, b))

  /**
    * Constructs the pure curried arrow contract A_1 -> (A_2  -> ... -> A_n) -> B.
    */
  def mkPureCurriedArrow(as: List[Contract], b: Contract): Contract = mkCurriedArrowWithEffect(as, Pure, b)

  /**
    * Constructs the impure curried arrow contract A_1 -> (A_2  -> ... -> A_n) ~> B.
    */
  def mkImpureCurriedArrow(as: List[Contract], b: Contract): Contract = mkCurriedArrowWithEffect(as, Impure, b)

  /**
    * Constructs the curried arrow contract A_1 -> (A_2  -> ... -> A_n) -> B & e.
    */
  def mkCurriedArrowWithEffect(as: List[Contract], e: Contract, b: Contract): Contract = {
    val a = as.last
    val base = mkArrowWithEffect(a, e, b)
    as.init.foldRight(base)(mkPureArrow)
  }

  /**
    * Constructs the pure uncurried arrow contract (A_1, ..., A_n) -> B.
    */
  def mkPureUncurriedArrow(as: List[Contract], b: Contract): Contract = mkUncurriedArrowWithEffect(as, Pure, b)

  /**
    * Constructs the impure uncurried arrow contract (A_1, ..., A_n) ~> B.
    */
  def mkImpureUncurriedArrow(as: List[Contract], b: Contract): Contract = mkUncurriedArrowWithEffect(as, Impure, b)

  /**
    * Constructs the uncurried arrow contract (A_1, ..., A_n) -> B & e.
    */
  def mkUncurriedArrowWithEffect(as: List[Contract], e: Contract, b: Contract): Contract = {
    val arrow = Contract.Apply(Contract.Cst(ContractConstructor.Arrow(as.length + 1), SourceLocation.Unknown), e)
    val inner = as.foldLeft(arrow: Contract) {
      case (acc, x) => Apply(acc, x)
    }
    Apply(inner, b)
  }

  /**
    * Constructs the apply contract base[t_1, ,..., t_n].
    */
  def mkApply(base: Contract, ts: List[Contract]): Contract = ts.foldLeft(base) {
    case (acc, t) => Apply(acc, t)
  }

  /**
    * Returns the contract `Choice[tpe, isAbsent, isPresent]`.
    */
  def mkChoice(tpe0: Contract, isAbsent: Contract, isPresent: Contract): Contract = {
    val sym = Symbol.mkEnumSym("Choice")
    val kind = Kind.Star ->: Kind.Bool ->: Kind.Bool ->: Kind.Star
    val tc = ContractConstructor.Enum(sym, kind)
    Apply(Apply(Apply(Cst(tc, SourceLocation.Unknown), tpe0), isAbsent), isPresent)
  }

  /**
    * Construct the enum contract constructor for the given symbol `sym` with the given kind `k`.
    */
  def mkEnum(sym: Symbol.EnumSym, k: Kind, loc: SourceLocation): Contract = Contract.Cst(ContractConstructor.Enum(sym, k), loc)

  /**
    * Construct the enum contract `Sym[ts]`.
    */
  def mkEnum(sym: Symbol.EnumSym, ts: List[Contract]): Contract = mkApply(Contract.Cst(ContractConstructor.Enum(sym, Kind.mkArrow(ts.length)), SourceLocation.Unknown), ts)

  /**
    * Constructs a tag contract for the given `sym`, `tag`, `caseContract` and `resultContract`.
    *
    * A tag contract can be understood as a "function contract" from the `caseContract` to the `resultContract`.
    *
    * For example, for:
    *
    * {{{
    * enum List[a] {
    *   case Nil,
    *   case Cons(a, List[a])
    * }
    *
    * We have:
    *
    *   Nil:  Unit -> List[a]           (caseContract = Unit, resultContract = List[a])
    *   Cons: (a, List[a]) -> List[a]   (caseContract = (a, List[a]), resultContract = List[a])
    * }}}
    */
  def mkTag(sym: Symbol.EnumSym, tag: Name.Tag, caseContract: Contract, resultContract: Contract): Contract = {
    Contract.Apply(Contract.Apply(Contract.Cst(ContractConstructor.Tag(sym, tag), SourceLocation.Unknown), caseContract), resultContract)
  }

  /**
    * Constructs the tuple contract (A, B, ...) where the contracts are drawn from the list `ts`.
    */
  def mkTuple(ts: List[Contract]): Contract = {
    val init = Contract.Cst(ContractConstructor.Tuple(ts.length), SourceLocation.Unknown)
    ts.foldLeft(init: Contract) {
      case (acc, x) => Apply(acc, x)
    }
  }

  /**
    * Constructs the a native contract.
    */
  def mkNative(clazz: Class[_]): Contract = Contract.Cst(ContractConstructor.Native(clazz), SourceLocation.Unknown)

  /**
    * Constructs a RecordExtend contract.
    */
  def mkRecordExtend(field: Name.Field, tpe: Contract, rest: Contract): Contract = {
    mkApply(Contract.Cst(ContractConstructor.RecordExtend(field), SourceLocation.Unknown), List(tpe, rest))
  }

  /**
    * Constructs a SchemaExtend contract.
    */
  def mkSchemaExtend(pred: Name.Pred, tpe: Contract, rest: Contract): Contract = {
    mkApply(Contract.Cst(ContractConstructor.SchemaExtend(pred), SourceLocation.Unknown), List(tpe, rest))
  }

  /**
    * Construct a relation contract with the given list of contract arguments `ts0`.
    */
  def mkRelation(ts0: List[Contract]): Contract = {
    val ts = ts0 match {
      case Nil => Contract.Unit
      case x :: Nil => x
      case xs => mkTuple(xs)
    }

    Apply(Relation, ts)
  }

  /**
    * Construct a lattice contract with the given list of contract arguments `ts0`.
    */
  def mkLattice(ts0: List[Contract]): Contract = {
    val ts = ts0 match {
      case Nil => Contract.Unit
      case x :: Nil => x
      case xs => mkTuple(xs)
    }

    Apply(Lattice, ts)
  }

  /**
    * Returns the contract `Not(tpe0)`.
    */
  def mkNot(tpe0: Contract): Contract = tpe0 match {
    case Contract.True => Contract.False
    case Contract.False => Contract.True
    case _ => Contract.Apply(Contract.Not, tpe0)
  }

  /**
    * Returns the contract `And(tpe1, tpe2)`.
    */
  def mkAnd(tpe1: Contract, tpe2: Contract): Contract = (tpe1, tpe2) match {
    case (Contract.Cst(ContractConstructor.True, _), _) => tpe2
    case (_, Contract.Cst(ContractConstructor.True, _)) => tpe1
    case (Contract.Cst(ContractConstructor.False, _), _) => Contract.False
    case (_, Contract.Cst(ContractConstructor.False, _)) => Contract.False
    case _ => Contract.Apply(Contract.Apply(Contract.And, tpe1), tpe2)
  }

  /**
    * Returns the contract `And(tpe1, And(tpe2, tpe3))`.
    */
  def mkAnd(tpe1: Contract, tpe2: Contract, tpe3: Contract): Contract = mkAnd(tpe1, mkAnd(tpe2, tpe3))

  /**
    * Returns the contract `And(tpe1, And(tpe2, ...))`.
    */
  def mkAnd(tpes: List[Contract]): Contract = tpes match {
    case Nil => Contract.True
    case x :: xs => mkAnd(x, mkAnd(xs))
  }

  /**
    * Returns the contract `Or(tpe1, tpe2)`.
    */
  def mkOr(tpe1: Contract, tpe2: Contract): Contract = (tpe1, tpe2) match {
    case (Contract.Cst(ContractConstructor.True, _), _) => Contract.True
    case (_, Contract.Cst(ContractConstructor.True, _)) => Contract.True
    case (Contract.Cst(ContractConstructor.False, _), _) => tpe2
    case (_, Contract.Cst(ContractConstructor.False, _)) => tpe1
    case _ => Contract.Apply(Contract.Apply(Contract.Or, tpe1), tpe2)
  }

  /**
    * Returns the contract `Or(tpe1, Or(tpe2, ...))`.
    */
  def mkOr(tpes: List[Contract]): Contract = tpes match {
    case Nil => Contract.False
    case x :: xs => mkOr(x, mkOr(xs))
  }

  /**
    * Returns the contract `tpe1 => tpe2`.
    */
  def mkImplies(tpe1: Contract, tpe2: Contract): Contract = Contract.mkOr(Contract.mkNot(tpe1), tpe2)

  /**
    * Returns a Boolean contract that represents the equivalence of `x` and `y`.
    *
    * That is, `x == y` iff `(x /\ y) \/ (not x /\ not y)`
    */
  def mkEquiv(x: Contract, y: Contract): Contract = Contract.mkOr(Contract.mkAnd(x, y), Contract.mkAnd(Contract.mkNot(x), Contract.mkNot(y)))

  /**
    * Returns a simplified (evaluated) form of the given contract `tpe0`.
    *
    * Performs beta-reduction of contract abstractions and applications.
    */
  def simplify(tpe0: Contract): Contract = {
    def eval(t: Contract, subst: Map[Contract.Var, Contract]): Contract = t match {
      case tvar: Contract.Var => subst.getOrElse(tvar, tvar)

      case Contract.Cst(_, _) => t

      case Contract.Apply(Contract.Apply(Contract.Cst(ContractConstructor.RecordExtend(field), _), tpe), rest) =>
        val t1 = eval(tpe, subst)
        val t2 = eval(rest, subst)
        Contract.mkRecordExtend(field, t1, t2)

      case Contract.Apply(Contract.Apply(Contract.Cst(ContractConstructor.SchemaExtend(pred), _), tpe), rest) =>
        val t1 = eval(tpe, subst)
        val t2 = eval(rest, subst)
        Contract.mkSchemaExtend(pred, t1, t2)

      case Contract.Lambda(tvar, tpe) => Contract.Lambda(tvar, eval(tpe, subst))

      // TODO: Does not take variable capture into account.
      case Contract.Apply(tpe1, tpe2) => (eval(tpe1, subst), eval(tpe2, subst)) match {
        case (Contract.Lambda(tvar, tpe3), t2) => eval(tpe3, subst + (tvar -> t2))
        case (t1, t2) => Contract.Apply(t1, t2)
      }
    }

    eval(tpe0, Map.empty)
  }

}
