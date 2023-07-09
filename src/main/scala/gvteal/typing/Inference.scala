package gvteal.typing

/*
  1. Assign a type or type variable to the expression and each subexpression. For known expressions, like +, — and so on, use the types known for these expressions. Otherwise, use type variables — placeholders for not known types.
  
  2. Generate a set of constraints on types, using the parse tree of the expression. These constraints are something like this: if a function is applied to an integer, then the type of its argument is integer.
  
  3. Solve these constraints by unification — an algorithm for solving equations based on substitutions.
*/

