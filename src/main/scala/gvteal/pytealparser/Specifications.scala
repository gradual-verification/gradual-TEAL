package gvteal.pytealparser

import fastparse._
import Lexical.kw

object Specifications{  
  implicit object whitespace extends fastparse.Whitespace {
    def apply(ctx: P[_]): P[Unit] = Lexical.wscomment(ctx)
  }

  def requiresSpecification[$: P]: P[Ast.stmt.Specification] = 
    P(kw("requires") ~ ( Expressions.imprecisionAndTest | Expressions.imprecisionExpression | Expressions.test) ~ ";").map { e =>
      Ast.stmt.Specification.RequiresSpecification(e)
  }

  def ensuresSpecification[$: P]: P[Ast.stmt.Specification] = 
    P(kw("ensures") ~ Expressions.test ~ ";").map { e =>
      Ast.stmt.Specification.EnsuresSpecification(e)
  }
  
  def assertSpecification[$: P]: P[Ast.stmt.Specification] = 
    P(kw("assert") ~ Expressions.test ~ ";").map { e =>
      Ast.stmt.Specification.AssertSpecification(e)
  }

  def loopInvariantSpecification[$: P]: P[Ast.stmt.Specification] = 
    P(kw("loop_invariant") ~ Expressions.test ~ ";").map { e =>
      Ast.stmt.Specification.LoopInvariantSpecification(e)
  }

  def foldSpecification[$: P]: P[Ast.stmt.Specification] = 
    P(kw("fold") ~ Lexical.identifier ~ "(" ~ Expressions.test.rep(sep = ",") ~ ")" ~ ";").map { case (ident, args) =>
      Ast.stmt.Specification.FoldSpecification(ident, args.toList)
  }
  
  def unfoldSpecification[$: P]: P[Ast.stmt.Specification] =
    P(kw("unfold") ~ Lexical.identifier ~ "(" ~ Expressions.test.rep(sep=",") ~ ")" ~ ";").map { case (ident, args) =>
      Ast.stmt.Specification.UnfoldSpecification(ident, args.toList)
  }

  def globalDeclaration[$: P]: P[Ast.stmt.Specification] =
    P(kw("global") ~ " ".rep ~ Lexical.identifier.rep(sep=",") ~ ";").map { case (args) =>
      Ast.stmt.Specification.GlobalDeclaration(args.toList)
  }
}