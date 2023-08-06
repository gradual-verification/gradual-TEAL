package gvteal.pytealparser

/**
 * A python abstract syntax tree
 *
 * Basically transcribed from https://docs.python.org/2/library/ast.html
 */
object Ast{
  case class identifier(name: String)
  type bool = Boolean
  type int = Int
  type `object` = Double
  type string = String

  sealed trait span
  object Span{
    case class SourcePosition(line: Int, column: Int, index: Int) extends span
    case class SourceSpan(start: SourcePosition, end: SourcePosition) extends span
  }

  sealed trait mod
  object mod{
    case class Module(body: Seq[stmt]) extends mod
    case class Interactive(body: Seq[stmt]) extends mod
    case class Expression(body: Seq[stmt]) extends mod
  }

  sealed trait stmt
  object stmt{
    case class FunctionDef(name: identifier, args: arguments, body: Seq[stmt], decorator_list: Seq[expr]) extends stmt
    
    case class PyTealFunctionDef(name: identifier, args: Option[pytealarguments], body: Seq[stmt], decorator_list: Seq[expr]) extends stmt
    
    case class ClassDef(name: identifier, bases: Seq[expr], body: Seq[stmt], decorator_list: Seq[expr]) extends stmt
    case class Return(value: Option[expr]) extends stmt
    case class ScratchVar(name: identifier, teal_type: tealtype) extends stmt

    case class Delete(targets: Seq[expr]) extends stmt
    case class Assign(targets: Seq[expr], value: expr) extends stmt
    case class AugAssign(target: expr, op: operator, value: expr) extends stmt

    // not sure if bool allowed: is, can always use int
    case class Print(dest: Option[expr], values: Seq[expr], nl: bool) extends stmt

    // use 'orelse' because else is a keyword in target languages
    case class For(target: expr, iter: expr, body: Seq[stmt], orelse: Seq[stmt]) extends stmt
    case class While(test: expr, body: Seq[stmt], orelse: Seq[stmt]) extends stmt
    case class If(test: expr, body: Seq[stmt], orelse: Seq[stmt]) extends stmt
    case class With(context_expr: expr, optional_vars: Option[expr], body: Seq[stmt]) extends stmt

    // 'type' is a bad name
    case class Raise(`type`: Option[expr], inst: Option[expr], tback: Option[expr]) extends stmt
    case class TryExcept(body: Seq[stmt], handlers: Seq[excepthandler], orelse: Seq[stmt]) extends stmt
    case class TryFinally(body: Seq[stmt], finalbody: Seq[stmt]) extends stmt
    case class Assert(test: expr, msg: Option[expr]) extends stmt

    case class Import(names: Seq[alias]) extends stmt
    case class ImportFrom(module: Option[identifier], names: Seq[alias], level: Option[int]) extends stmt

    // Doesn't capture requirement that locals must be
    // defined if globals is
    // still supports use as a function!
    case class Exec(body: expr, globals: Option[expr], locals: Option[expr]) extends stmt

    case class Global(names: Seq[identifier]) extends stmt
    case class Expr(value: expr) extends stmt
    case object Pass extends stmt
    case object Break extends stmt
    case object Continue extends stmt

    // XXX Jython will be different
    // col_offset is the byte offset in the utf8 string the parser uses
    case class attributes(lineno: Int, col_offset: Int)
    
    // pyTEAL specifications
    sealed trait Specification extends stmt
    object Specification {
      case class RequiresSpecification(value: expr) extends Specification
      case class EnsuresSpecification(value: expr) extends Specification
      case class LoopInvariantSpecification(value: expr) extends Specification
      case class AssertSpecification(value: expr) extends Specification
      case class FoldSpecification(predicate: identifier, arguments: List[expr]) extends Specification
      case class UnfoldSpecification(predicate: identifier, arguments: List[expr]) extends Specification
      case class GlobalDeclaration(args: List[identifier]) extends Specification
    }

    case class Spec(specification: Specification) extends stmt
  }

  // BoolOp() can use left & right?
  sealed trait expr
  object expr{
    case class ImprecisionExpression(span: Span.SourceSpan) extends expr

    //case class PyTealInt(integer: Any) extends expr
    case class PyTealBytes(base: Any, value: Any) extends expr
    case class PyTealBytesStored(key: Any) extends expr

    //PyTeal operation

    case class PyTealBinOp(op: pytealop, values: Seq[Any]) extends expr

    case class PyTealConditionalExpr(expressions: Seq[Any]) extends expr

    //PyTeal If

    case class PyTealIf(expr: Any) extends expr
    case class PyTealThen(expr: Any) extends expr

    case class PyTealExpr(expr1: Any, op: pytealop, expr2: Any) extends expr

    //PyTeal Seq
    case class GlobalPut(key: Any, value: Any) extends expr
    case class GlobalGet(key: Any) extends expr

    case class Approve() extends expr

    case class PyTealSeq(values: Seq[Any]) extends expr

    case class ScratchLoad(name: identifier) extends expr
    case class ScratchStore(name: identifier, expr: Any) extends expr
    case class Get(name: identifier) extends expr
    case class SetValue(name: identifier, expr: Any) extends expr

    case class BoolOp(op: boolop, values: Seq[expr]) extends expr
    case class BinOp(left: expr, op: operator, right: expr) extends expr
    case class UnaryOp(op: unaryop, operand: expr) extends expr
    // case class NaryOp
    case class Lambda(args: arguments, body: expr) extends expr
    case class IfExp(test: expr, body: expr, orelse: expr) extends expr
    case class Dict(keys: Seq[expr], values: Seq[expr]) extends expr
    case class Set(elts: Seq[expr]) extends expr
    case class ListComp(elt: expr, generators: Seq[comprehension]) extends expr
    case class SetComp(elt: expr, generators: Seq[comprehension]) extends expr
    case class DictComp(key: expr, value: expr, generators: Seq[comprehension]) extends expr
    case class GeneratorExp(elt: expr, generators: Seq[comprehension]) extends expr
    // the grammar constrains where yield expressions can occur
    case class Yield(value: Option[expr]) extends expr
    // need sequences for compare to distinguish between
    // x < 4 < 3 and (x < 4) < 3
    case class Compare(left: expr, ops: Seq[cmpop], comparators: Seq[expr]) extends expr
    case class Call(func: expr, args: Seq[expr], keywords: Seq[keyword], starargs: Option[expr], kwargs: Option[expr]) extends expr
    case class Repr(value: expr) extends expr
    case class Num(n: Any) extends expr // a number as a PyObject.
    case class Str(s: string) extends expr // need to raw: specify, unicode, etc?
    // other bools: Option[literals]?

    // the following expression can appear in assignment context
    case class Attribute(value: expr, attr: identifier, ctx: expr_context) extends expr
    case class Subscript(value: expr, slice: slice, ctx: expr_context) extends expr
    case class Name(id: identifier, ctx: expr_context) extends expr
    case class List(elts: Seq[expr], ctx: expr_context) extends expr
    case class Tuple(elts: Seq[expr], ctx: expr_context) extends expr
  }
  // col_offset is the byte offset in the utf8 string the parser uses
  case class attributes(lineno: Int, col_offset: Int)

  sealed trait expr_context
  object expr_context{

    case object Load extends expr_context
    case object Store extends expr_context
    case object Del extends expr_context
    case object AugLoad extends expr_context
    case object AugStore extends expr_context
    case object Param extends expr_context
  }
  sealed trait slice
  object slice{

    case object Ellipsis extends slice
    case class Slice(lower: Option[expr], upper: Option[expr], step: Option[expr]) extends slice
    case class ExtSlice(dims: Seq[slice]) extends slice
    case class Index(value: expr) extends slice
  }

  sealed trait boolop
  object boolop{
    case object And extends boolop
    case object Or extends boolop
  }

  sealed trait abitype
  case object abitype {

    case object Uint8 extends abitype
    case object Uint16 extends abitype
    case object Uint32 extends abitype
    case object Uint64 extends abitype
    case object Bool extends abitype
    case object Byte extends abitype
    case object StaticArray extends abitype
    case object Address extends abitype
    case object StaticBytes extends abitype
    case object DynamicArray extends abitype
    case object DynamicBytes extends abitype
    case object String extends abitype
    case object Tuple extends abitype
    case object NamedTuple extends abitype

  }

  sealed trait tealtype
  case object tealtype {

    case object Uint64 extends tealtype
    case object Bytes extends tealtype

  }

  sealed trait pytealop
  case object pytealop {
    case object PyTealLt extends pytealop
    case object PyTealGt extends pytealop
    case object PyTealGt2 extends pytealop
    case object PyTealLe extends pytealop
    case object PyTealGe extends pytealop
    case object PyTealAdd extends pytealop
    case object PyTealMinus extends pytealop
    case object PyTealMinus2 extends pytealop
    case object PyTealMul extends pytealop
    case object PyTealDiv extends pytealop
    case object PyTealMod extends pytealop
    case object PyTealExp extends pytealop
    case object PyTealEq extends pytealop
    case object PyTealNeq extends pytealop
    case object PyTealAnd extends pytealop
    case object PyTealOr extends pytealop
    case object PyTealBitwiseAnd extends pytealop
    case object PyTealBitwiseOr extends pytealop
    case object PyTealBitwiseXor extends pytealop

    case object PyTealNot extends pytealop
    case object PyTealBitwiseNot extends pytealop

    case object PyTealBytesLt extends pytealop
    case object PyTealBytesGt extends pytealop
    case object PyTealBytesLe extends pytealop
    case object PyTealBytesGe extends pytealop
    case object PyTealBytesAdd extends pytealop
    case object PyTealBytesMinus extends pytealop
    case object PyTealBytesMul extends pytealop
    case object PyTealBytesDiv extends pytealop
    case object PyTealBytesMod extends pytealop
    case object PyTealBytesEq extends pytealop
    case object PyTealBytesNeq extends pytealop
    case object PyTealBytesAnd extends pytealop
    case object PyTealBytesOr extends pytealop
    case object PyTealBytesXor extends pytealop

    case object PyTealBytesNot extends pytealop
    case object PyTealBytesZero extends pytealop

  }

  sealed trait operator
  case object operator{
    case object Add extends operator
    case object Sub  extends operator
    case object Mult  extends operator
    case object Div  extends operator
    case object Mod  extends operator
    case object Pow  extends operator
    case object LShift  extends operator
    case object RShift  extends operator
    case object BitOr  extends operator
    case object BitXor  extends operator
    case object BitAnd  extends operator
    case object FloorDiv extends operator
  }

  sealed trait unaryop
  object unaryop{

    case object Invert extends unaryop
    case object Not extends unaryop
    case object UAdd extends unaryop
    case object USub extends unaryop
  }
  sealed trait cmpop
  object cmpop{

    case object Eq extends cmpop
    case object NotEq extends cmpop
    case object Lt extends cmpop
    case object LtE extends cmpop
    case object Gt extends cmpop
    case object GtE extends cmpop
    case object Is extends cmpop
    case object IsNot extends cmpop
    case object In extends cmpop
    case object NotIn extends cmpop
  }

  case class comprehension(target: expr, iter: expr, ifs: Seq[expr])

  // not sure what to call the first argument for raise and except
  sealed trait excepthandler
  object excepthandler{
    case class ExceptHandler(`type`: Option[expr], name: Option[expr], body: Seq[stmt]) extends excepthandler
  }

  case class arguments(args: Seq[expr], vararg: Option[identifier], kwarg: Option[identifier], defaults: Seq[expr])

  case class pytealarguments(args: Seq[(identifier, abitype)])

  // keyword arguments supplied to call
  case class keyword(arg: identifier, value: expr)

  // import name with optional 'as' alias.
  case class alias(name: identifier, asname: Option[identifier])

  /* ============ PyTEAL Extension ============ */
  case class SourcePosition(line: Int, column: Int, index: Int)
  case class SourceSpan(start: SourcePosition, end: SourcePosition)
}