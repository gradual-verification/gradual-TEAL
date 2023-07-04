package gvteal.pytealparser

object ParserPyTeal {
    //import fastparse.Parsed
    import fastparse._, NoWhitespace._

//    def specification_stmt[$: P]: P[Seq[Ast.stmt]] = P( singleLineAnnotation | multiLineAnnotation ).map(specs => specs.map(Ast.stmt.Spec(_)))
//    def specification_name[$: P]: P[Ast.stmt.Specification] = 
//     P( "#@ " ~ (Specifications.requiresSpecification | Specifications.ensuresSpecification | Specifications.assertSpecification | Specifications.loopInvariantSpecification))
//    def singleLineAnnotation[$: P]: P[Seq[Ast.stmt.Specification]] = P( specification_name.rep(1, sep = "\n") ~ (End | Pass))
//    def multiLineAnnotation[$: P]: P[Seq[Ast.stmt.Specification]] = P( "\"\"\"@ " ~ specification_name.rep(1, sep = "\n") ~ "@\"\"\"" )

//     def parseProgram(src: String): Parsed[Seq[Ast.stmt]] = {
//         fastparse.parse(src, specification_stmt(_))
//     }

    def parseProgram(src: String): Parsed[Seq[Ast.stmt]] = {
        fastparse.parse(src, Statements.file_input(_))
    }
}