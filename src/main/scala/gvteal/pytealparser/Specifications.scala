package gvteal.pytealparser

import fastparse._
import Lexical.kw
//import Lexical.span
import Ast._
import Expressions._

class Specifications{
//   def specification[_: P]: P[Specification] =
//     P(
//       requiresSpecification |
//       ensuresSpecification |
//       loopInvariantSpecification |
//       assertSpecification |
//       foldSpecification |
//       unfoldSpecification
//     ).opaque("<specification>")
  
//   def requiresSpecification[_: P]: P[Specification.RequiresSpecification] =
//     P(span(kw("requires") ~/ test ~ ";")).map({
//       case (e, span) => Specification.RequiresSpecification(e, span)
//     })

//   def ensuresSpecification[_: P]: P[Specification.EnsuresSpecification] =
//     P(span(kw("ensures") ~/ test ~ ";")).map({
//       case (e, span) => Specification.EnsuresSpecification(e, span)
//     })
  
//   def loopInvariantSpecification[_: P]: P[Specification.LoopInvariantSpecification] =
//     P(span(kw("loop_invariant") ~/ test ~/ ";")).map({
//       case (e, span) => Specification.LoopInvariantSpecification(e, span)
//     })
  
//   def assertSpecification[_: P]: P[Specification.AssertSpecification] =
//     P(span(kw("assert") ~/ test ~/ ";")).map({
//       case (e, span) => Specification.AssertSpecification(e, span)
//     })

//   def foldSpecification[_: P]: P[Specification.FoldSpecification] =
//     P(span(kw("fold") ~/ identifier ~ "(" ~ expr.rep(sep = ",") ~ ")" ~ ";"))
//       .map { case ((ident, args), span) => Specification.FoldSpecification(ident, args.toList, span) }
  
//   def unfoldSpecification[_: P]: P[Specification.UnfoldSpecification] =
//     P(span(kw("unfold") ~/ identifier ~ "(" ~ expr.rep(sep=",") ~ ")" ~ ";"))
//       .map { case ((ident, args), span) => Specification.UnfoldSpecification(ident, args.toList, span) }
  
//   /* ============ PyTEAL Extension ============ */

//   def annotations[_: P]: P[List[Specification]] =
//     P(annotation.rep).map(a => a.flatten.toList)

//   def annotation[_: P]: P[Seq[Specification]] =
//     P(singleLineAnnotation | multiLineAnnotation)
//   def singleLineAnnotation[_: P]: P[Seq[Specification]] =
//     P("#@"./.flatMapX(_ => new Parser(state.inSingleLineAnnotation()).specifications) ~~/ ("\n" | End))
//   def multiLineAnnotation[_: P]: P[Seq[Specification]] =
//     P("\"\"\"@"./.flatMapX(_ => new Parser(state.inAnnotation()).specifications) ~/ "@\"\"\"")
}