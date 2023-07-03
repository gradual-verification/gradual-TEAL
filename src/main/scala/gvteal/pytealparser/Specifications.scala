package gvteal.pytealparser

import fastparse._
import Lexical.kw

object Specifications{  
  implicit object whitespace extends fastparse.Whitespace {
    def apply(ctx: P[_]): P[Unit] = Lexical.wscomment(ctx)
  }

  // def pyteal_specs[$: P]: P[Ast.specification] = 
  //       P( "#@ " ~ Specifications.requiresSpecification ~ ("\n" | End))

  // def pyteal_arithematic_op[$: P]: P[Ast.pytealop] = P (PyTealLt | PyTealGt | PyTealLe | PyTealGe | PyTealAdd
  //                                                 | PyTealMinus | PyTealMul | PyTealDiv | PyTealMod
  //                                                 | PyTealExp | PyTealEq | PyTealNeq | PyTealAnd | PyTealOr 
  //                                                 | PyTealBitwiseAnd | PyTealBitwiseOr | PyTealBitwiseXor
  //                                                 | PyTealNot | PyTealBitwiseNot)

  // def pyteal_expr[$: P]: P[Ast.expr] = P ((pyteal_arithematic_op | pyteal_byteslice_arithematic_op) ~ "(" ~ (Lexical.pytealInt | Lexical.pytealBytes | Lexical.identifier).rep(1, sep = ",") ~ ")").map { 
  //   case (op, values) => Ast.expr.PyTealBinOp(op, values)
  // }

  def requiresSpecification[$: P]: P[Ast.stmt.Specification] = 
    P(kw("requires") ~ Expressions.test ~ ";").map { e =>
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

  
  // def foldSpecification[$: P]: P[Ast.specification] = 
  //   P(kw("fold") ~ Lexical.identifier ~ "(" ~ Expressions.test.rep(sep = ",") ~ ")" ~ ";").map { (ident, args) =>
  //     Ast.specification.FoldSpecification(ident, args.toList)
  // }

  // def foldSpecification[_: P]: P[Specification.FoldSpecification] =
  //   P(kw("fold") ~/ Lexical.identifier ~ "(" ~ test.rep(sep = ",") ~ ")" ~ ";")
  //     .map { case ((ident, args)) => Specification.FoldSpecification(ident, args.toList) }
  
  // def unfoldSpecification[_: P]: P[Specification.UnfoldSpecification] =
  //   P(kw("unfold") ~/ Lexical.identifier ~ "(" ~ test.rep(sep=",") ~ ")" ~ ";")
  //     .map { case ((ident, args)) => Specification.UnfoldSpecification(ident, args.toList) }
}