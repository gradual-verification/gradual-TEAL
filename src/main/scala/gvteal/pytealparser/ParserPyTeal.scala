package gvteal.pytealparser

object ParserPyTeal {
    //import fastparse.Parsed
    import fastparse._, NoWhitespace._

    // def specification_stmt[$: P]: P[Seq[Ast.stmt]] = P( singleLineAnnotation | multiLineAnnotation )
    // def specification_name[$: P]: P[Ast.stmt.Specification] = 
    //     P( "#@ " ~ (Specifications.requiresSpecification | Specifications.ensuresSpecification | Specifications.assertSpecification | Specifications.loopInvariantSpecification))
    // def singleLineAnnotation[$: P]: P[Seq[Ast.stmt.Specification]] = P( specification_name.rep(1, sep = "\n") ~ (End | Pass))
    // def multiLineAnnotation[$: P]: P[Seq[Ast.stmt.Specification]] = P( "\"\"\"@ " ~ specification_name.rep(1, sep = "\n") ~ "@\"\"\"" )

    // def parseProgram(src: String): Parsed[Seq[Ast.stmt]] = {
    //     fastparse.parse(src, specification_stmt(_))
    // }

    def specification_stmt[$: P]: P[Seq[Ast.stmt]] = P( singleLineAnnotation | multiLineAnnotation )
    def specification_name[$: P]: P[Ast.stmt.Specification] = 
        P( "#@ " ~ (Specifications.requiresSpecification | Specifications.ensuresSpecification | Specifications.assertSpecification | Specifications.loopInvariantSpecification))
    def singleLineAnnotation[$: P]: P[Seq[Ast.stmt.Specification]] = P( specification_name.rep(1, sep = "\n") ~ (End | Pass))
    def multiLineAnnotation[$: P]: P[Seq[Ast.stmt.Specification]] = P( "\"\"\"@ " ~ specification_name.rep(1, sep = "\n") ~ "@\"\"\"" )

    def parseProgram(src: String): Parsed[Seq[Ast.stmt]] = {
        fastparse.parse(src, specification_stmt(_))
    }

    // def pyteal_specs[$: P]: P[Seq[Ast.stmt.Specification]] =
    // P( (pyteal_requires | pyteal_ensures | pyteal_assert | pyteal_loop_invariant).rep ~ End )

    // def pyteal_requires[$: P]: P[Ast.stmt.Specification] =
    //     P( "#@ " ~ Specifications.requiresSpecification ~ ("\n") )

    // def pyteal_ensures[$: P]: P[Ast.stmt.Specification] =
    //     P( "#@ " ~ Specifications.ensuresSpecification ~ ("\n") )

    // def pyteal_assert[$: P]: P[Ast.stmt.Specification] =
    //     P( "#@ " ~ Specifications.assertSpecification ~ ("\n") )

    // def pyteal_loop_invariant[$: P]: P[Ast.stmt.Specification] =
    //     P( "#@ " ~ Specifications.loopInvariantSpecification ~ ("\n") )

    // def pyteal_specs[$:P] : P[Ast.stmt] = P ("\"\"\"@" ~ Expressions.pyteal_expr ~ "@\"\"\"").map {
    //     case (py_expr) => Ast.Specification.StringSpecification(py_expr)
    // } 

    // def parseProgram(src: String): Parsed[Seq[Ast.stmt]] = {
    //     fastparse.parse(src, Statements.file_input(_))
    // }
}