package gvteal.parser
import fastparse._

trait Lexer extends Whitespace {
  def ident[_: P] =
    P(CharIn("A-Za-z_") ~~ CharIn("A-Za-z0-9_").repX)

  def identifier[_: P] =
    P(span(ident.!)).map({
      case (id, span) => Identifier(id, span)
    })

  def decimalNumber[_: P] =
    P("0" | (CharIn("1-9") ~~ CharIn("0-9").repX))

  def hexNumber[_: P] =
    P("0" ~~ CharIn("xX") ~~/ CharIn("0-9a-fA-F").repX(1))

  def string[_: P] = P("\"" ~~/ stringChar.repX ~~ "\"")

  def character[_: P] = P("'" ~~/ charChar ~~ "'")

  def library[_: P] = P("<" ~~/ libraryChar.repX ~~ ">")

  def stringChar[_: P] = P(normalChar | escape)

  def charChar[_: P] = P(normalChar | escape | "\"" | "\\0")

  def normalChar[_: P] =
    P(CharPred(c => c != '"' && c != '\\' && !c.isControl))

  def libraryChar[_: P] =
    P(CharPred(c => c != '>' && !c.isControl))
  
  // <esc> ::= ::= \n | \t | \v | \b | \r | \f | \a | \\ | \' | \"
  // For some reason fastparse needs the \ escaped
  def escape[_: P] = P("\\" ~ CharIn("""ntvbrfa"'\\"""))

  // <unop> ::= ! | ~ | - | *
  def prefixOperator[_: P] = P(CharIn("!~\\-*"))

    // <postop> ::= -- | ++
  def postfixOperator[_: P] = P(StringIn("--", "++"))

  def binaryOperator[_: P] =
    P(StringIn("*", "/", "%", "+", "-", "<<", ">>",
               "<", "<=", ">=", ">", "==", "!=",
               "&", "^", "|", "&&", "||"));

  def pyNaryOperator[_: P] =
    P(StringIn("Or", "And", "Add", "Mul", "Concat"));

  def pyBinaryOperator[_: P] =
    P(StringIn("Div", "Mod", "Minus", "ShiftLeft", "ShiftRight",
               "Lt", "Le", "Ge", "Gt", "Eq", "Neq",
               "BitwiseAnd", "BitwiseXor", "BitwiseOr"));

  def assignmentOperator[_: P] =
    P(StringIn("=", "+=", "-=", "*=", "/=", "%=", "<<=", ">>=",
                "&=", "^=", "|="))

  // Helper for keywords
  def kw[_: P, T](p: => P[T]): P[T] = P(p ~~ !CharIn("A-Za-z0-9_"))

  // Helper for position
  def pos[_: P] = P(Index).map(state.position(_))

  def span[_: P, T](p: => P[T]): P[(T, SourceSpan)] = P(pos ~~ p ~~ pos).map({
    case (start, value, end) => (value, SourceSpan(start, end))
  })
}