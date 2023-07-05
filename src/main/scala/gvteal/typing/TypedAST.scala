package gvteal.typing
import gvteal.pytealparser._

object TypedAST extends stmt {
  case class TypedPyTealFunctionDef(name: identifier, args: arguments, body: Seq[stmt], decorator_list: Seq[expr]) 
}