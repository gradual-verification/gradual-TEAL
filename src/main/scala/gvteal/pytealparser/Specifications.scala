package gvteal.pytealparser

import fastparse._
import Lexical.kw
import Ast._
import Expressions._

class Specifications{
  def specification[_: P]: P[Specification] =
    P(
      requiresSpecification |
      ensuresSpecification |
      loopInvariantSpecification |
      assertSpecification |
      foldSpecification |
      unfoldSpecification
    ).opaque("<specification>")
  
  def requiresSpecification[_: P]: P[Specification.RequiresSpecification] =
    P(kw("requires") ~/ test ~ ";").map({
      case (e) => Specification.RequiresSpecification(e)
    })

  def ensuresSpecification[_: P]: P[Specification.EnsuresSpecification] =
    P(kw("ensures") ~/ test ~ ";").map({
      case (e) => Specification.EnsuresSpecification(e)
    })
  
  def loopInvariantSpecification[_: P]: P[Specification.LoopInvariantSpecification] =
    P(kw("loop_invariant") ~/ test ~/ ";").map({
      case (e) => Specification.LoopInvariantSpecification(e)
    })
  
  def assertSpecification[_: P]: P[Specification.AssertSpecification] =
    P(kw("assert") ~/ test ~/ ";").map({
      case (e) => Specification.AssertSpecification(e)
    })

  def foldSpecification[_: P]: P[Specification.FoldSpecification] =
    P(kw("fold") ~/ Lexical.identifier ~ "(" ~ test.rep(sep = ",") ~ ")" ~ ";")
      .map { case ((ident, args)) => Specification.FoldSpecification(ident, args.toList) }
  
  def unfoldSpecification[_: P]: P[Specification.UnfoldSpecification] =
    P(kw("unfold") ~/ Lexical.identifier ~ "(" ~ test.rep(sep=",") ~ ")" ~ ";")
      .map { case ((ident, args)) => Specification.UnfoldSpecification(ident, args.toList) }
}