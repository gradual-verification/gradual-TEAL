package gvteal.typing
import gvteal.pytealparser.Ast

/*
  1. Assign a type or type variable to the expression and each subexpression. For known expressions, like +, — and so on, use the types known for these expressions. Otherwise, use type variables — placeholders for not known types.
  
  2. Generate a set of constraints on types, using the parse tree of the expression. These constraints are something like this: if a function is applied to an integer, then the type of its argument is integer.
  
  3. Solve these constraints by unification — an algorithm for solving equations based on substitutions.
*/

object Unification {
  /**
  * Unify expressions if possible
  *
  * @param boundVariables cur bound variables
  * @param expressions expressions for unification
  * @return updated bound variables if possible
  */
  def apply(boundVariables: Map[Variable, Expression], expressions: Expression*): Try[Map[Variable, Expression]] = 
  expressions.toList match {
    case exp1 :: exp2 :: rest ->
  }

  /**
    * build DAG and update variables from leafs
    *
    * @param boundVariables cur bound variables
    * @return updated bound variables
    */
    private def buildAnswer(boundVariables: Map[Variable, Expression]): Map[Variable, Expression] = {
      var visitedVariables: Set[Variable] = Set.empty

      def updateVariable(boundVariables: Map[Variable, Expression],
      parentVariable: Variable,
      updatedVariable: Variable,
      parents: Set[Variable]): Map[Variable, Expression] = {

      }
    }
    
    def dfs(boundVariables: Map[Variable, Expression],
    variable: Variable,
    expression: Expression,
    parents: Set[Variable]): Map[Variable, Expression] = {

    }
}