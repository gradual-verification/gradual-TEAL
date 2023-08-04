package gvteal.pytealparser

object ParserPyTeal {
    //import fastparse.Parsed
    import fastparse._, NoWhitespace._

    var state: Option[ParserState] = None

    def pos[_: P] = P(Index).map(state.get.position(_))
    def span[_: P, T](p: => P[T]): P[(T, Ast.Span.SourceSpan)] = P(pos ~~ p ~~ pos).map({
        case (start, value, end) => (value, Ast.Span.SourceSpan(start, end))
    })

    def parseProgram(src: String): Parsed[Seq[Ast.stmt]] = {
        state = Some(new ParserState(src))
        fastparse.parse(src, Statements.file_input(_))
    }
}