package com.melvic.lohika.formula

import cats.Endo
import com.melvic.lohika.expression.Expression.{Term, Var}
import com.melvic.lohika.formula.Formula.*
import com.melvic.lohika.formula.Formula.Quantified.BoundVars
import com.melvic.lohika.formula.conversions.Conversions
import com.melvic.lohika.parsers.Lexemes

import scala.annotation.targetName
import scala.util.chaining.*

type Formula = Compound | Not | Quantified | PredicateApp

object Formula extends FormulaGivens with Conversions with PrettyPrinting:
  type Compound = Imply | FList

  final case class Or(p: Formula, q: Formula, rs: List[Formula]) extends FList
  final case class And(p: Formula, q: Formula, rs: List[Formula]) extends FList
  final case class Imply(p: Formula, q: Formula)
  final case class Iff(p: Formula, q: Formula, rs: List[Formula]) extends FList
  final case class Not(p: Formula)
  final case class Forall(boundVars: BoundVars, matrix: Formula) extends Quantified
  final case class ThereExists(boundVars: BoundVars, matrix: Formula) extends Quantified
  final case class PredicateApp(name: String, args: List[Term])
  final case class FunctionApp(name: String, args: List[Term])

  type Property = Formula => Boolean
  val NullPred: PredicateApp.Nullary.type = PredicateApp.Nullary

  sealed trait FList:
    def p: Formula

    def q: Formula

    def rs: List[Formula]

    def components: List[Formula] = p :: q :: rs

  sealed trait Quantified:
    def boundVars: BoundVars

    def matrix: Formula

  def isInCnf: Property =
    case fm if isLiteral(fm) => true
    case and: And            => and.components.forall(and => isInCnf(and) && !isAnd(and))
    case or: Or              => or.components.forall(isLiteral)
    case _                   => false

  def isLiteral: Property =
    case _: PredicateApp        => true
    case Not(p) if isLiteral(p) => true
    case _                      => false

  def isAnd: Property =
    case _: And => true
    case _      => false

  def isOr: Property =
    case _: Or => true
    case _     => false

  def isClause: Property = fm => isOr(fm) || isLiteral(fm)

  // TODO: See if we can remove this and replace with the one from `Cnf`
  def flatten[A <: FList](make: FList.Make)(
      filter: PartialFunction[Formula, A]
  ): Endo[Formula] =
    def toList: Formula => List[Formula] =
      case fm if filter.isDefinedAt(fm) =>
        val chain = filter(fm)
        toList(chain.p) ++ toList(chain.q) ++ chain.rs.flatMap(toList)
      case fm => List(fm)

    {
      case fm if filter.isDefinedAt(fm) =>
        toList(fm).pipe:
          case h :: t :: rest => make(h, t, rest)
      case fm => fm
    }

  object Quantified:
    type BoundVars = (Var, List[Var])
    type Make[Q <: Quantified] = (BoundVars, Formula) => Q

    type Quantifier = Universal.type | Existential.type
    case object Universal
    case object Existential

    def unapply(quantified: Quantified): (Quantifier, BoundVars, Formula) =
      quantified match
        case Forall(boundVars, matrix)      => (Universal, boundVars, matrix)
        case ThereExists(boundVars, matrix) => (Existential, boundVars, matrix)

    def apply(quantifier: Quantifier, boundVars: BoundVars, matrix: Formula): Quantified =
      quantifier match
        case Universal   => Forall(boundVars, matrix)
        case Existential => ThereExists(boundVars, matrix)

    def quantified[Q <: Quantified](varName: String, rest: String*)(
        make: Make[Q]
    ): Formula => Q =
      make((Var(varName), rest.map(Var.apply).toList), _)

    def forall(varName: String, rest: String*): Formula => Forall =
      quantified(varName, rest*)(Forall.apply)

    def thereExists(varName: String, rest: String*): Formula => ThereExists =
      quantified(varName, rest*)(ThereExists.apply)

  object Or:
    def of(p: Formula, q: Formula, rs: Formula*): Or =
      Or(p, q, rs.toList)

    def fromList: List[Formula] => Formula =
      case Nil          => PredicateApp.False
      case p :: Nil     => p
      case p :: q :: rs => Or(p, q, rs)

    def flatten: Endo[Formula] =
      Formula.flatten(Or.apply) { case or: Or => or }

  object And:
    def of(p: Formula, q: Formula, rs: Formula*): And =
      And(p, q, rs.toList)

    def fromList: List[Formula] => Formula =
      case Nil          => PredicateApp.True
      case p :: Nil     => p
      case p :: q :: rs => And(p, q, rs)

    def flatten: Endo[Formula] =
      Formula.flatten(And.apply) { case and: And => and }

  object Iff:
    def of(p: Formula, q: Formula, rs: Formula*): Iff =
      Iff(p, q, rs.toList)

    def fromList: List[Formula] => Option[Formula] =
      case p :: Nil     => Some(p)
      case p :: q :: rs => Some(Iff(p, q, rs))
      case _            => None

    def flatten: Endo[Formula] =
      Formula.flatten(Iff.apply) { case iff: Iff => iff }

  object PredicateApp:
    def nullary(name: String): PredicateApp = PredicateApp(name, Nil)

    def unary(name: String, arg: Term): PredicateApp =
      PredicateApp(name, arg :: Nil)

    val False: PredicateApp = PredicateApp(Lexemes.False, Nil)
    val True: PredicateApp = PredicateApp(Lexemes.True, Nil)

    object Nullary:
      def unapply(predicate: PredicateApp): Option[String] =
        Option.when(predicate.args.isEmpty)(predicate.name)

  object FunctionApp:
    def unary(name: String, arg: Term): FunctionApp =
      FunctionApp(name, arg :: Nil)

  object Imply:
    def of(p: Formula, qs: Formula*): Imply =
      Imply.fromList(p :: qs.toList)

    def fromList(components: List[Formula]): Imply =
      (components.reduceRight(Imply.apply): @unchecked) match
        case imply: Imply => imply

  object FList:
    type Args = (Formula, Formula, List[Formula])
    type Make = Args => Formula

  extension (self: Formula)
    @targetName("and")
    def &(other: Formula): And = And.of(self, other)

    @targetName("or")
    def |(other: Formula): Or = Or.of(self, other)

    @targetName("implies")
    def ==>(other: Formula): Imply = Imply(self, other)

    @targetName("ifAndOnlyIf")
    def <==>(other: Formula): Iff = Iff.of(self, other)

    @targetName("not")
    def unary_! : Not = Not(self)

  extension (name: String)
    def of(args: String*): PredicateApp =
      of(args.map(Var.apply)*)

    @targetName("ofTerms")
    def of(term: Term*): PredicateApp =
      PredicateApp(name, term.toList)
