package gvteal.pytealparser

object ParserPyTeal {
    //import fastparse.Parsed
    import fastparse._, NoWhitespace._

    def parseProgram(src: String): Parsed[Seq[Ast.stmt]] = {
        fastparse.parse(src, Statements.file_input(_))
    }
}