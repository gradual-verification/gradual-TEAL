package gvteal.parser

sealed trait Node{
  val span: SourceSpan
}

case class SourcePosition(line: Int, column: Int, index: Int)
case class SourceSpan(start: SourcePosition, end: SourcePosition)

// Identifiers
case class Identifier(name: String, span: SourceSpan) extends Node {
  def ==(value: String): Boolean = {
    name == value
  }
}

// Types
sealed trait Type extends Node
case class NamedType(id: Identifier, span: SourceSpan) extends Type
case class NamedStructType(id: Identifier, span: SourceSpan) extends Type
case class PointerType(valueType: Type, span: SourceSpan) extends Type
case class ArrayType(valueType: Type, span: SourceSpan) extends Type

// Expressions
sealed trait Expression extends Node
case class VariableExpression(variable: Identifier, span: SourceSpan) extends Expression
case class IncrementExpression(value: Expression, operator: IncrementOperator, span: SourceSpan) extends Expression
case class BinaryExpression(left: Expression, operator: BinaryOperator.Value, right: Expression, span: SourceSpan) extends Expression
case class UnaryExpression(operand: Expression, operator: UnaryOperator.Value, span: SourceSpan) extends Expression
case class TernaryExpression(condition: Expression, ifTrue: Expression, ifFalse: Expression, span: SourceSpan) extends Expression
case class InvokeExpression(method: Identifier, arguments: List[Expression], span: SourceSpan) extends Expression
case class AllocExpression(valueType: Type, span: SourceSpan) extends Expression
case class AllocArrayExpression(valueType: Type, length: Expression, span: SourceSpan) extends Expression
case class IndexExpression(parent: Expression, index: Expression, span: SourceSpan) extends Expression
case class MemberExpression(parent: Expression, field: Identifier, isArrow: Boolean, span: SourceSpan) extends Expression
case class ResultExpression(span: SourceSpan) extends Expression
case class LengthExpression(value: Expression, span: SourceSpan) extends Expression
case class ImprecisionExpression(span: SourceSpan) extends Expression
case class AccessibilityExpression(field: Expression, span: SourceSpan) extends Expression

// TODO: (PyTEAL AST)[https://github.com/algorand/pyteal/blob/master/pyteal/ast]

/*
Transaction Types numerical value evaluation:
Unknown => 0
Payment => 1
KeyRegistration => 2
AssetConfig => 3
AssetTransfer => 4
AssetFreeze => 5
ApplicationCall => 6
*/

// Literal expressions
sealed trait LiteralExpression extends Expression {
  val raw: String
  val value: Any
}

case class StringExpression(raw: String, value: String, span: SourceSpan) extends LiteralExpression {
  def ==(other: String): Boolean = {
    value == other
  }
}

case class CharacterExpression(raw: String, value: Char, span: SourceSpan) extends LiteralExpression {
  def ==(other: Char): Boolean = {
    value == other
  }
}

case class IntegerExpression(raw: String, value: Int, span: SourceSpan) extends LiteralExpression {
  def ==(other: Int): Boolean = {
    value == other
  }
}

case class BooleanExpression(raw: String, value: Boolean, span: SourceSpan) extends LiteralExpression {
  def ==(other: Boolean): Boolean = {
    value == other
  }
}

case class NullExpression(raw: String = "NULL", value: Null, span: SourceSpan) extends LiteralExpression {
  def ==(other: Null): Boolean = true
}

// Specifications
sealed trait Specification extends Node
case class RequiresSpecification(value: Expression, span: SourceSpan) extends Specification
case class EnsuresSpecification(value: Expression, span: SourceSpan) extends Specification
case class LoopInvariantSpecification(value: Expression, span: SourceSpan) extends Specification
case class AssertSpecification(value: Expression, span: SourceSpan) extends Specification
case class FoldSpecification(predicate: Identifier, arguments: List[Expression], span: SourceSpan) extends Specification
case class UnfoldSpecification(predicate: Identifier, arguments: List[Expression], span: SourceSpan) extends Specification

// Statements
sealed trait Statement extends Node {
  val specifications: List[Specification]
  def withSpecifications(specs: List[Specification]): Statement
}

case class ExpressionStatement(
  expression: Expression,
  span: SourceSpan,
  specifications: List[Specification] = List.empty
) extends Statement {
  def withSpecifications(specs: List[Specification]): ExpressionStatement = copy(specifications = specs)
}
case class AssignmentStatement(
  left: Expression,
  operator: AssignOperator.Value,
  right: Expression,
  span: SourceSpan,
  specifications: List[Specification] = List.empty
) extends Statement {
  def withSpecifications(specs: List[Specification]): AssignmentStatement = copy(specifications = specs)
}
case class VariableStatement(
  valueType: Type,
  id: Identifier,
  initialValue: Option[Expression],
  span: SourceSpan,
  specifications: List[Specification] = List.empty
) extends Statement {
  def withSpecifications(specs: List[Specification]): VariableStatement = copy(specifications = specs)
}
case class IfStatement(
  condition: Expression,
  ifTrue: Statement,
  ifFalse: Option[Statement],
  span: SourceSpan,
  specifications: List[Specification] = List.empty
) extends Statement {
  def withSpecifications(specs: List[Specification]): IfStatement = copy(specifications = specs)
}
case class WhileStatement(
  condition: Expression,
  body: Statement,
  span: SourceSpan,
  specifications: List[Specification] = List.empty
) extends Statement {
  def withSpecifications(specs: List[Specification]): WhileStatement = copy(specifications = specs)
}
case class ForStatement(
  initializer: Statement,
  condition: Expression,
  incrementor: Statement,
  body: Statement,
  span: SourceSpan,
  specifications: List[Specification] = List.empty
) extends Statement {
  def withSpecifications(specs: List[Specification]): ForStatement = copy(specifications = specs)
}
case class ReturnStatement(
  value: Option[Expression],
  span: SourceSpan,
  specifications: List[Specification] = List.empty
) extends Statement {
  def withSpecifications(specs: List[Specification]): ReturnStatement = copy(specifications = specs)
}
case class AssertStatement(
  value: Expression,
  span: SourceSpan,
  specifications: List[Specification] = List.empty
) extends Statement {
  def withSpecifications(specs: List[Specification]): AssertStatement = copy(specifications = specs)
}
case class ErrorStatement(
  value: Expression,
  span: SourceSpan,
  specifications: List[Specification] = List.empty
) extends Statement {
  def withSpecifications(specs: List[Specification]): ErrorStatement = copy(specifications = specs)
}
case class BlockStatement(
  body: List[Statement],
  span: SourceSpan,
  specifications: List[Specification],
  trailingSpecifications: List[Specification]
) extends Statement {
  def withSpecifications(specs: List[Specification]): BlockStatement = copy(specifications = specs)
}

// Definitions
sealed trait Definition extends Node
case class MemberDefinition(id: Identifier, valueType: Type, span: SourceSpan) extends Node
case class TypeDefinition(id: Identifier, value: Type, span: SourceSpan) extends Definition
case class StructDefinition(id: Identifier, fields: Option[List[MemberDefinition]], span: SourceSpan) extends Definition
case class UseDeclaration(path: StringExpression, isLibrary: Boolean, span: SourceSpan) extends Definition
case class ImportSimple(path: StringExpression, span: SourceSpan) extends Definition
case class ImportFrom(name: StringExpression, functions: List[String], span: SourceSpan) extends Definition
case class ImportFromAll(name: StringExpression, span: SourceSpan) extends Definition
case class PredicateDefinition(
  id: Identifier,
  arguments: List[MemberDefinition],
  body: Option[Expression],
  span: SourceSpan
) extends Definition
case class MethodDefinition(
  id: Identifier,
  returnType: Type,
  arguments: List[MemberDefinition],
  body: Option[BlockStatement],
  specifications: List[Specification],
  span: SourceSpan
) extends Definition

object NaryOperator extends Enumeration {
  type NaryOperator = Value
  val LogicalOrNary = Value("Or")
  val LogicalAndNary = Value("And")
  val AddNary = Value("Add")
  val MulNary = Value("Mul")
  val ConcatNary = Value("Concat")
}

object BinaryOperator extends Enumeration {
  type BinaryOperator = Value
  
  //val LogicalOr = Value("||")
  //val LogicalAnd = Value("&&")
  val BitwiseOr = Value("BitwiseOr")
  val BitwiseXor = Value("BitwiseXor")
  val BitwiseAnd = Value("BitwiseAnd")
  val Equal = Value("Eq")
  val NotEqual = Value("Neq")
  val Less = Value("Lt")
  val LessEqual = Value("Le")
  val GreaterEqual = Value("Ge")
  val Greater = Value("Gt")
  val ShiftLeft = Value("ShiftLeft")
  val ShiftRight = Value("ShiftRight")
  //val Add = Value("+")
  val Subtract = Value("Minus")
  //val Multiply = Value("*")
  val Divide = Value("Div")
  val Modulus = Value("Mod")
  val GetBit = Value("GetBit")
  val GetByte = Value("GetByte")
  val BytesAdd = Value("BytesAdd")
  val BytesMinus = Value("BytesMinus")
  val BytesDiv = Value("BytesDiv")
  val BytesMul = Value("BytesMul")
  val BytesMod = Value("BytesMod")
  val BytesAnd = Value("BytesAnd")
  val BytesOr = Value("BytesOr")
  val BytesXor = Value("BytesXor")
  val BytesEq = Value("BytesEq")
  val BytesNeq = Value("BytesNeq")
  val BytesLt = Value("BytesLt")
  val BytesLe = Value("BytesLe")
  val BytesGt = Value("BytesGt")
  val BytesGe = Value("BytesGe")
  val ExtractUnit16 = Value("ExtractUnit16")
  val ExtractUnit32 = Value("ExtractUnit32")
  val ExtractUnit64 = Value("ExtractUnit64")
}

object UnaryOperator extends Enumeration {
  type UnaryOperator = Value
  val Not = Value("Not")
  val BitwiseNot = Value("BitwiseNot")
  // val Btoi = Value("Btoi")
  // val Itob = Value("Itob")
  // val Len = Value("Len")
  // val BitLen = Value("BitLen")
  // val Sha256 = Value("Sha256")
  // val Sqrt = Value("Sqrt")
  // val Pop = Value("Pop")
  // val Balance = Value("Balance")
  // val MinBalance = Value("MinBalance")
  // val BytesNot = Value("BytesNot")
  // val BytesSqrt = Value("BytesSqrt")
  // val BytesZero = Value("BytesZero")
  // val Log = Value("Log")
  // TODO: sha512_256, Sha3_256
  // val Negate = Value("-")
  // val Deref = Value("*")
}

sealed trait IncrementOperator
object IncrementOperator {
  case object Increment extends IncrementOperator
  case object Decrement extends IncrementOperator
}

object AssignOperator extends Enumeration {
  type AssignOperator = Value
  val Assign = Value("=")
  val Add = Value("+=")
  val Subtract = Value("-=")
  val Multiply = Value("*=")
  val Divide = Value("/=")
  val Modulus = Value("%=")
  val ShiftLeft = Value("<<=")
  val ShiftRight = Value(">>=")
  val BitwiseAnd = Value("&=")
  val BitwiseOr = Value("|=")
  val BitwiseXor = Value("^=")
}