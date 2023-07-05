package gvteal.typing
import gvteal.parser._

sealed trait Node{
  val span: SourceSpan
}