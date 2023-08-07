package gvteal.pytealparser

import fastparse._
import Lexical.kw

object Specifications{  
  implicit object whitespace extends fastparse.Whitespace {
    def apply(ctx: P[_]): P[Unit] = Lexical.wscomment(ctx)
  }

  def requiresSpecification[$: P]: P[Ast.stmt.Specification] =
    P(kw("requires") ~/ Expressions.test ~ ";").map {
      case (e) => Ast.stmt.Specification.RequiresSpecification(e)
    }

  def ensuresSpecification[$: P]: P[Ast.stmt.Specification] =
    P(kw("ensures") ~/ Expressions.test ~ ";").map {
      case (e) => Ast.stmt.Specification.EnsuresSpecification(e)
    }
  
  def assertSpecification[$: P]: P[Ast.stmt.Specification] =
    P(kw("assert") ~/ Expressions.test ~ ";").map {
      case (e) => Ast.stmt.Specification.AssertSpecification(e)
    }

  def loopInvariantSpecification[$: P]: P[Ast.stmt.Specification] =
    P(kw("loop_invariant") ~/ Expressions.test ~ ";").map {
      case (e) => Ast.stmt.Specification.LoopInvariantSpecification(e)
    }

  def foldSpecification[$: P]: P[Ast.stmt.Specification] =
    P(kw("fold") ~ ((Expressions.imprecisionExpression ~ kw("and")).? ~ Lexical.identifier ~ "(" ~ Expressions.test.rep(sep = ",") ~ ")").map {
      case (Some(imprecision), ident, args) => Ast.stmt.Specification.FoldSpecification(ident, imprecision +: args.toList)
      case (None, ident, args) => Ast.stmt.Specification.FoldSpecification(ident, args.toList)
    } ~ ";")

  def unfoldSpecification[$: P]: P[Ast.stmt.Specification] =
    P(kw("unfold") ~ ((Expressions.imprecisionExpression ~ kw("and")).? ~ Lexical.identifier ~ "(" ~ Expressions.test.rep(sep = ",") ~ ")").map {
      case (Some(imprecision), ident, args) => Ast.stmt.Specification.UnfoldSpecification(ident, imprecision +: args.toList)
      case (None, ident, args) => Ast.stmt.Specification.UnfoldSpecification(ident, args.toList)
    } ~ ";")
}