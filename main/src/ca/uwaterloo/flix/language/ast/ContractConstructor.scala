package ca.uwaterloo.flix.language.ast

/**
  * Representation of contract constructors.
  */
sealed trait ContractConstructor {
  def kind: Kind
}

object ContractConstructor {

  /**
    * A contract constructor that represent the Unit contract.
    */
  case object Unit extends ContractConstructor {
    def kind: Kind = Kind.Star
  }

  /**
    * A contract constructor that represent the Null contract.
    */
  case object Null extends ContractConstructor {
    def kind: Kind = Kind.Star
  }

  /**
    * A contract constructor that represent the Bool contract.
    */
  case object Bool extends ContractConstructor {
    def kind: Kind = Kind.Star
  }

  /**
    * A contract constructor that represent the Char contract.
    */
  case object Char extends ContractConstructor {
    def kind: Kind = Kind.Star
  }

  /**
    * A contract constructor that represent the contract of 32-bit floating point numbers.
    */
  case object Float32 extends ContractConstructor {
    def kind: Kind = Kind.Star
  }

  /**
    * A contract constructor that represent the contract of 64-bit floating point numbers.
    */
  case object Float64 extends ContractConstructor {
    def kind: Kind = Kind.Star
  }

  /**
    * A contract constructor that represent the contract of 8-bit integers.
    */
  case object Int8 extends ContractConstructor {
    def kind: Kind = Kind.Star
  }

  /**
    * A contract constructor that represent the contract of 16-bit integers.
    */
  case object Int16 extends ContractConstructor {
    def kind: Kind = Kind.Star
  }

  /**
    * A contract constructor that represent the contract of 32-bit integers.
    */
  case object Int32 extends ContractConstructor {
    def kind: Kind = Kind.Star
  }

  /**
    * A contract constructor that represent the contract of 64-bit integers.
    */
  case object Int64 extends ContractConstructor {
    def kind: Kind = Kind.Star
  }

  /**
    * A contract constructor that represent the contract of arbitrary-precision integers.
    */
  case object BigInt extends ContractConstructor {
    def kind: Kind = Kind.Star
  }

  /**
    * A contract constructor that represent the contract of strings.
    */
  case object Str extends ContractConstructor {
    def kind: Kind = Kind.Star
  }

  /**
    * A contract constructor that represents the contract of functions.
    */
  case class Arrow(arity: Int) extends ContractConstructor {
    def kind: Kind = Kind.Bool ->: Kind.mkArrow(arity)
  }

  /**
    * A contract constructor that represents the contract of empty records.
    */
  case object RecordEmpty extends ContractConstructor {
    def kind: Kind = Kind.Record
  }

  /**
    * A contract constructor that represents the contract of extended records.
    */
  case class RecordExtend(field: Name.Field) extends ContractConstructor {
    /**
      * The shape of an extended record is { field: contract | rest }
      */
    def kind: Kind = Kind.Star ->: Kind.Record ->: Kind.Record
  }

  /**
    * A contract constructor that represents the contract of empty schemas.
    */
  case object SchemaEmpty extends ContractConstructor {
    def kind: Kind = Kind.Schema
  }

  /**
    * A contract constructor that represents the contract of extended schemas.
    */
  case class SchemaExtend(pred: Name.Pred) extends ContractConstructor {
    /**
      * The shape of an extended schema is { name: contract | rest }
      */
    def kind: Kind = Kind.Star ->: Kind.Schema ->: Kind.Schema
  }

  /**
    * A contract constructor that represent the contract of arrays.
    */
  case object Array extends ContractConstructor {
    /**
      * The shape of an array is Array[t].
      */
    def kind: Kind = Kind.Star ->: Kind.Star
  }

  /**
    * A contract constructor that represent the contract of channels.
    */
  case object Channel extends ContractConstructor {
    /**
      * The shape of a channel is Channel[t].
      */
    def kind: Kind = Kind.Star ->: Kind.Star
  }

  /**
    * A contract constructor that represent the contract of lazy expressions.
    */
  case object Lazy extends ContractConstructor {
    /**
      * The shape of lazy is Lazy[t].
      */
    def kind: Kind = Kind.Star ->: Kind.Star
  }

  /**
    * A contract constructor that represent the contract of tags.
    */
  case class Tag(sym: Symbol.EnumSym, tag: Name.Tag) extends ContractConstructor {
    /**
      * The shape of a tag is "like" a function `caseContract` -> (`resultContract`) -> *.
      */
    def kind: Kind = Kind.Star ->: Kind.Star ->: Kind.Star
  }

  /**
    * A contract constructor that represent the contract of enums.
    */
  case class Enum(sym: Symbol.EnumSym, kind: Kind) extends ContractConstructor

  /**
    * A contract constructor that represent the contract of JVM classes.
    */
  case class Native(clazz: Class[_]) extends ContractConstructor {
    def kind: Kind = Kind.Star
  }

  /**
    * A contract constructor that represent the contract of references.
    */
  case object Ref extends ContractConstructor {
    /**
      * The shape of a reference is Ref[t].
      */
    def kind: Kind = Kind.Star ->: Kind.Star
  }

  /**
    * A contract constructor that represent the contract of tuples.
    */
  case class Tuple(l: Int) extends ContractConstructor {
    /**
      * The shape of a tuple is (t1, ..., tn).
      */
    def kind: Kind = Kind.mkArrow(l)
  }

  /**
    * A contract constructor for relations.
    */
  case object Relation extends ContractConstructor {
    def kind: Kind = Kind.Star ->: Kind.Star
  }

  /**
    * A contract constructor for lattices.
    */
  case object Lattice extends ContractConstructor {
    def kind: Kind = Kind.Star ->: Kind.Star
  }

  /**
    * A contract constructor that represent the Boolean True.
    */
  case object True extends ContractConstructor {
    def kind: Kind = Kind.Bool
  }

  /**
    * A contract constructor that represents the Boolean False.
    */
  case object False extends ContractConstructor {
    def kind: Kind = Kind.Bool
  }

  /**
    * A contract constructor that represents the negation of an effect.
    */
  case object Not extends ContractConstructor {
    def kind: Kind = Kind.Bool ->: Kind.Bool
  }

  /**
    * A contract constructor that represents the conjunction of two effects.
    */
  case object And extends ContractConstructor {
    def kind: Kind = Kind.Bool ->: Kind.Bool ->: Kind.Bool
  }

  /**
    * A contract constructor that represents the disjunction of two effects.
    */
  case object Or extends ContractConstructor {
    def kind: Kind = Kind.Bool ->: Kind.Bool ->: Kind.Bool
  }

}
