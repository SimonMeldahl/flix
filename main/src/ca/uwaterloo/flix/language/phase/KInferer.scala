package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast.Ast.Polarity
import ca.uwaterloo.flix.language.ast.TypedAst._
import ca.uwaterloo.flix.language.ast.ops.TypedAstOps._
import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol, TypedAst}
import ca.uwaterloo.flix.language.errors.SafetyError
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation._

object KInferer extends Phase[Root, Root] {

  def run(root: Root)(implicit flix: Flix): Validation[Root, CompilationError] = flix.phase("KInferer") {
    val validation = Validation.traverse(root.defs) {
      case (sym, defn) =>
        visitDef(defn).map((sym, _))
    }
    validation.map { l =>
      Root(root.classes, root.instances, root.sigs, Map() ++ l, root.enums, root.reachable, root.sources, root.classEnv)
    }
  }

  def visitDef(def0: Def): Validation[Def, CompilationError] =
    visitExp(def0.impl.exp).map(e => Def(def0.sym, def0.spec, Impl(e, def0.impl.inferredScheme)))

  //TODO(LBS) impl
  def visitExp(exp: Expression): Validation[Expression, CompilationError] = exp match {
    case _ => exp.toSuccess
//    case Expression.Unit(loc) => exp.toSuccess
//    case Expression.Null(tpe, loc) => ???
//    case Expression.True(loc) => exp.toSuccess
//    case Expression.False(loc) => exp.toSuccess
//    case Expression.Char(lit, loc) => ???
//    case Expression.Float32(lit, loc) => ???
//    case Expression.Float64(lit, loc) => ???
//    case Expression.Int8(lit, loc) => ???
//    case Expression.Int16(lit, loc) => ???
//    case Expression.Int32(lit, loc) => exp.toSuccess
//    case Expression.Int64(lit, loc) => ???
//    case Expression.BigInt(lit, loc) => ???
//    case Expression.Str(lit, loc) => ???
//    case Expression.Default(tpe, loc) => ???
//    case Expression.Wild(tpe, loc) => ???
//    case Expression.Var(sym, tpe, loc) => exp.toSuccess
//    case Expression.Def(sym, tpe, loc) => exp.toSuccess
//    case Expression.Sig(sym, tpe, loc) => ???
//    case Expression.Hole(sym, tpe, eff, loc) => ???
//    case Expression.Lambda(fparam, exp, tpe, loc) =>
//    case Expression.Apply(exp, exps, tpe, eff, loc) =>
//    case Expression.Unary(sop, exp, tpe, eff, loc) =>
//    case Expression.Binary(sop, exp1, exp2, tpe, eff, loc) =>
//    case Expression.Let(sym, exp1, exp2, tpe, eff, loc) =>
//    case Expression.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
//    case Expression.Stm(exp1, exp2, tpe, eff, loc) =>
//    case Expression.Match(exp, rules, tpe, eff, loc) =>
//    case Expression.Choose(exps, rules, tpe, eff, loc) =>
//    case Expression.Tag(sym, tag, exp, tpe, eff, loc) =>
//    case Expression.Tuple(elms, tpe, eff, loc) =>
//    case Expression.RecordEmpty(tpe, loc) =>
//    case Expression.RecordSelect(exp, field, tpe, eff, loc) =>
//    case Expression.RecordExtend(field, value, rest, tpe, eff, loc) =>
//    case Expression.RecordRestrict(field, rest, tpe, eff, loc) =>
//    case Expression.ArrayLit(elms, tpe, eff, loc) =>
//    case Expression.ArrayNew(elm, len, tpe, eff, loc) =>
//    case Expression.ArrayLoad(base, index, tpe, eff, loc) =>
//    case Expression.ArrayLength(base, eff, loc) =>
//    case Expression.ArrayStore(base, index, elm, loc) =>
//    case Expression.ArraySlice(base, beginIndex, endIndex, tpe, loc) =>
//    case Expression.Ref(exp, tpe, eff, loc) =>
//    case Expression.Deref(exp, tpe, eff, loc) =>
//    case Expression.Assign(exp1, exp2, tpe, eff, loc) =>
//    case Expression.Existential(fparam, exp, loc) =>
//    case Expression.Universal(fparam, exp, loc) =>
//    case Expression.Ascribe(exp, tpe, eff, loc) =>
//    case Expression.Cast(exp, tpe, eff, loc) =>
//    case Expression.TryCatch(exp, rules, tpe, eff, loc) =>
//    case Expression.InvokeConstructor(constructor, args, tpe, eff, loc) =>
//    case Expression.InvokeMethod(method, exp, args, tpe, eff, loc) =>
//    case Expression.InvokeStaticMethod(method, args, tpe, eff, loc) =>
//    case Expression.GetField(field, exp, tpe, eff, loc) =>
//    case Expression.PutField(field, exp1, exp2, tpe, eff, loc) =>
//    case Expression.GetStaticField(field, tpe, eff, loc) =>
//    case Expression.PutStaticField(field, exp, tpe, eff, loc) =>
//    case Expression.NewChannel(exp, tpe, eff, loc) =>
//    case Expression.GetChannel(exp, tpe, eff, loc) =>
//    case Expression.PutChannel(exp1, exp2, tpe, eff, loc) =>
//    case Expression.SelectChannel(rules, default, tpe, eff, loc) =>
//    case Expression.Spawn(exp, tpe, eff, loc) =>
//    case Expression.Lazy(exp, tpe, loc) =>
//    case Expression.Force(exp, tpe, eff, loc) =>
//    case Expression.FixpointConstraintSet(cs, stf, tpe, loc) =>
//    case Expression.FixpointMerge(exp1, exp2, stf, tpe, eff, loc) =>
//    case Expression.FixpointSolve(exp, stf, tpe, eff, loc) =>
//    case Expression.FixpointFilter(pred, exp, tpe, eff, loc) =>
//    case Expression.FixpointProjectIn(exp, pred, tpe, eff, loc) =>
//    case Expression.FixpointProjectOut(pred, exp, tpe, eff, loc) =>
  }
}
