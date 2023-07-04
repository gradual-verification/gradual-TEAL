package gvteal.pytealparser

object ParserPyTeal {
    import fastparse.Parsed
    
    def parseProgram(src: String): Parsed[Seq[Ast.stmt]] = {
        fastparse.parse(src, Statements.file_input(_))
    }
}