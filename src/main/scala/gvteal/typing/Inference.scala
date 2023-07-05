package gvteal.typing
import gvteal.pytealparser.Ast

/*
  1. Assign a type or type variable to the expression and each subexpression. For known expressions, like +, — and so on, use the types known for these expressions. Otherwise, use type variables — placeholders for not known types.
  
  2. Generate a set of constraints on types, using the parse tree of the expression. These constraints are something like this: if a function is applied to an integer, then the type of its argument is integer.
  
  3. Solve these constraints by unification — an algorithm for solving equations based on substitutions.
*/

enum Type:
  case Base(n: String) extends Type

  override def toString: String =
    import Type.*
    this match
      case Base(name) => name
  end toString

  def name: String =
    import Type.*
    this match
      case Base(name) => name
      case _ => throw IllegalStateException()
  end name
end Type
object TypePrimitives:
  val Int: Type = Type.Base("Int")
  val Byte: Type = Type.Base("Byte")
end TypePrimitives

// Get all the base type variables that are ever occurred in the given constraint set
def constraintVariables(constraintSet: Set[(Type, Type)]): Set[Type] =
  return constraintSet.flatMap {
    case (left, right) => typeVariables(left) | typeVariables(right)
  }
end constraintVariables

// Get all the base type variables in a type declaration
def typeVariables(decl: Type): Set[Type] =
  return decl match
    case base: Type.Base => Set(base)
end typeVariables

// Retrieves the free occurrences of variables in a term
def freeVariables(root: SyntaxNode): Set[String] =
  import SyntaxNode.*
  val symbols = mutable.Stack[String]()
  def helper(r: SyntaxNode): Set[String] =
    return r match
      case _ => Set.empty
  end helper
  return helper(root)
end freeVariables