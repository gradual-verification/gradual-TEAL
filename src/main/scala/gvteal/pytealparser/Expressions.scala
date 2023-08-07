package gvteal.pytealparser

import fastparse._

import Lexical.kw

/**
 * Python's expression grammar. This is stuff that can be used within a larger
 * expression. Everything here ignores whitespace and does not care about
 * indentation
 *
 * Manually transcribed from https://docs.python.org/2/reference/grammar.html
 */
object Expressions {
  implicit object whitespace extends fastparse.Whitespace {
    def apply(ctx: P[_]): P[Unit] = Lexical.wscomment(ctx)
  }
  def tuplize(xs: Seq[Ast.expr]) = xs match{
    case Seq(x) => x
    case xs => Ast.expr.Tuple(xs, Ast.expr_context.Load)
  }

  def NAME[$: P]: P[Ast.identifier] = Lexical.identifier
  def NUMBER[$: P]: P[Ast.expr.Num] = P( Lexical.floatnumber | Lexical.longinteger | Lexical.integer | Lexical.imagnumber ).map(Ast.expr.Num.apply)
  def STRING[$: P]: P[Ast.string] = Lexical.stringliteral

  def test[$: P]: P[Ast.expr] = {
    def ternary = P( or_test ~ (kw("if") ~ or_test ~ kw("else") ~ test).? ).map{
      case (x, None) => x
      case (x, Some((test, neg))) => Ast.expr.IfExp(test, x, neg)
    }
    P( ternary | lambdef )
  }
  def or_test[$: P] = P( and_test.rep(1, sep = kw("or")) ).map{
    case Seq(x) => x
    case xs => Ast.expr.BoolOp(Ast.boolop.Or, xs)
  }
  def and_test[$: P] = P( not_test.rep(1, sep = kw("and")) ).map{
    case Seq(x) => x
    case xs => Ast.expr.BoolOp(Ast.boolop.And, xs)
  }
  //def not_test[$: P]: P[Ast.expr] = P( (kw("not") ~ not_test).map(Ast.expr.UnaryOp(Ast.unaryop.Not, _)) | comparison )
  def not_test[$: P]: P[Ast.expr] = P(
    imprecisionExpression |
    (kw("not") ~ not_test).map(Ast.expr.UnaryOp(Ast.unaryop.Not, _)) | 
    comparison
  )
  def comparison[$: P]: P[Ast.expr] = P( expr ~ (comp_op ~ expr).rep ).map{
    case (lhs, Nil) => lhs
    case (lhs, chunks) =>
      val (ops, vals) = chunks.unzip
      Ast.expr.Compare(lhs, ops, vals)
  }

  // Common operators, mapped from their
  // strings to their type-safe representations
  def op[T, $: P](s: => P[Unit], rhs: T) = s.!.map(_ => rhs)
  def LShift[$: P] = op("<<", Ast.operator.LShift)
  def RShift[$: P] = op(">>", Ast.operator.RShift)
  def Lt[$: P] = op("<", Ast.cmpop.Lt)
  def Gt[$: P] = op(">", Ast.cmpop.Gt)
  def Eq[$: P] = op("==", Ast.cmpop.Eq)
  def GtE[$: P] = op(">=", Ast.cmpop.GtE)
  def LtE[$: P] = op("<=", Ast.cmpop.LtE)
  def NotEq[$: P] = op("<>" | "!=", Ast.cmpop.NotEq)
  def In[$: P] = op(kw("in"), Ast.cmpop.In)
  def NotIn[$: P] = op(kw("not") ~ kw("in"), Ast.cmpop.NotIn)
  def Is[$: P] = op(kw("is"), Ast.cmpop.Is)
  def IsNot[$: P] = op(kw("is") ~ kw("not"), Ast.cmpop.IsNot)
  def comp_op[$: P] = P( LtE|GtE|Eq|Gt|Lt|NotEq|In|NotIn|IsNot|Is )
  def Add[$: P] = op("+", Ast.operator.Add)
  def Sub[$: P] = op("-", Ast.operator.Sub)
  def Pow[$: P] = op("**", Ast.operator.Pow)
  def Mult[$: P] = op("*", Ast.operator.Mult)
  def Div[$: P] = op("/", Ast.operator.Div)
  def Mod[$: P] = op("%", Ast.operator.Mod)
  def FloorDiv[$: P] = op("//", Ast.operator.FloorDiv)
  def BitOr[$: P] = op("|", Ast.operator.BitOr)
  def BitAnd[$: P] = op("&", Ast.operator.BitAnd)
  def BitXor[$: P] = op("^", Ast.operator.BitXor)
  def UAdd[$: P] = op("+", Ast.unaryop.UAdd)
  def USub[$: P] = op("-", Ast.unaryop.USub)
  def Invert[$: P] = op("~", Ast.unaryop.Invert)
  def unary_op[$: P] = P ( UAdd | USub | Invert )


  //PyTeal abi Types

  def Uint8[$:P] = op("abi.Uint8", Ast.abitype.Uint8)
  def Uint16[$:P] = op("abi.Uint16", Ast.abitype.Uint16)
  def Uint32[$:P] = op("abi.Uint32", Ast.abitype.Uint32)
  def Uint64[$:P] = op("abi.Uint64", Ast.abitype.Uint64)
  def Bool[$:P] = op("abi.Bool", Ast.abitype.Bool)
  def Byte[$:P] = op("abi.Byte", Ast.abitype.Byte)
  def StaticArray[$:P] = op("abi.StaticArray", Ast.abitype.StaticArray)
  def Address[$:P] = op("abi.Address", Ast.abitype.Address)
  def StaticBytes[$:P] = op("abi.StaticBytes", Ast.abitype.StaticBytes)
  def DynamicArray[$:P] = op("abi.DynamicArray", Ast.abitype.DynamicArray)
  def DynamicBytes[$:P] = op("abi.DynamicBytes", Ast.abitype.DynamicBytes)
  def String[$:P] = op("abi.String", Ast.abitype.String)
  def Tuple[$:P] = op("abi.Tuple", Ast.abitype.Tuple)
  def NamedTuple[$:P] = op("abi.NamedTuple", Ast.abitype.NamedTuple)

  // PyTeal Operators

  // ---------- Binary Operators --------------

  def PyTealLt[$:P] = op("Lt", Ast.pytealop.PyTealLt)
  def PyTealGt[$:P] = op("Gt", Ast.pytealop.PyTealGt)
  def PyTealGt2[$:P] = op(">", Ast.pytealop.PyTealGt)
  def PyTealLe[$:P] = op("Le", Ast.pytealop.PyTealLe)
  def PyTealGe[$:P] = op("Le", Ast.pytealop.PyTealGe)
  def PyTealAdd[$:P] = op("Add", Ast.pytealop.PyTealAdd)
  def PyTealAdd2[$:P] = op("+", Ast.pytealop.PyTealAdd)
  def PyTealMinus[$:P] = op("Minus", Ast.pytealop.PyTealMinus)
  def PyTealMinus2[$:P] = op("-", Ast.pytealop.PyTealMinus)
  def PyTealMul[$:P] = op("Mul", Ast.pytealop.PyTealMul)
  def PyTealDiv[$:P] = op("Div", Ast.pytealop.PyTealDiv)
  def PyTealMod[$:P] = op("Mod", Ast.pytealop.PyTealMod)
  def PyTealExp[$:P] = op("Exp", Ast.pytealop.PyTealExp)
  def PyTealEq[$:P] = op("Eq", Ast.pytealop.PyTealEq)
  def PyTealNeq[$:P] = op("Neq", Ast.pytealop.PyTealNeq)
  def PyTealAnd[$:P] = op("And", Ast.pytealop.PyTealAnd)
  def PyTealOr[$:P] = op("Or", Ast.pytealop.PyTealOr)
  def PyTealBitwiseAnd[$:P] = op("BitwiseAnd", Ast.pytealop.PyTealBitwiseAnd)
  def PyTealBitwiseOr[$:P] = op("BitwiseOr", Ast.pytealop.PyTealBitwiseOr)
  def PyTealBitwiseXor[$:P] = op("BitwiseXor", Ast.pytealop.PyTealBitwiseXor)

  // ---------- Unary Operators --------------

  def PyTealNot[$:P] = op("Not", Ast.pytealop.PyTealNot)
  def PyTealBitwiseNot[$:P] = op("BitwiseNot", Ast.pytealop.PyTealBitwiseNot)

  // ---------- ByteSlice Operators --------------

  def PyTealBytesLt[$:P] = op("BytesLt", Ast.pytealop.PyTealBytesLt)
  def PyTealBytesGt[$:P] = op("BytesGt", Ast.pytealop.PyTealBytesGt)
  def PyTealBytesLe[$:P] = op("BytesLe", Ast.pytealop.PyTealBytesLe)
  def PyTealBytesGe[$:P] = op("BytesLe", Ast.pytealop.PyTealBytesGe)
  def PyTealBytesAdd[$:P] = op("BytesAdd", Ast.pytealop.PyTealBytesAdd)
  def PyTealBytesMinus[$:P] = op("BytesMinus", Ast.pytealop.PyTealBytesMinus)
  def PyTealBytesMul[$:P] = op("BytesMul", Ast.pytealop.PyTealBytesMul)
  def PyTealBytesDiv[$:P] = op("BytesDiv", Ast.pytealop.PyTealBytesDiv)
  def PyTealBytesMod[$:P] = op("BytesMod", Ast.pytealop.PyTealBytesMod)
  def PyTealBytesEq[$:P] = op("BytesEq", Ast.pytealop.PyTealBytesEq)
  def PyTealBytesNeq[$:P] = op("BytesNeq", Ast.pytealop.PyTealBytesNeq)
  def PyTealBytesAnd[$:P] = op("BytesAnd", Ast.pytealop.PyTealBytesAnd)
  def PyTealBytesOr[$:P] = op("BytesOr", Ast.pytealop.PyTealBytesOr)
  def PyTealBytesXor[$:P] = op("BytesXor", Ast.pytealop.PyTealBytesXor)

  // ---------- Unary Operators --------------

  def PyTealBytesNot[$:P] = op("BytesNot", Ast.pytealop.PyTealBytesNot)
  def PyTealBytesZero[$:P] = op("BytesZero", Ast.pytealop.PyTealBytesZero)

  def pytealInt[$: P]: P[Ast.expr] = P ( "Int" ~ "(" ~ Lexical.integer ~ ")").map { case (integer) => Ast.expr.Num(integer) }
  def pytealBytes[$: P]: P[Ast.expr] = P ( "Bytes" ~ "(" ~ (Lexical.stringliteral) ~ "," ~ " ".rep() ~ (Lexical.stringliteral) ~ ")").map { case (base, value) => Ast.expr.PyTealBytes(base, value) }
  def pytealBytesStored[$: P]: P[Ast.expr] = P ( "Bytes" ~ "(" ~ (Lexical.stringliteral) ~ ")").map { case (key) => Ast.expr.PyTealBytesStored(key) }


  def pyteal_arithematic_op[$: P]: P[Ast.pytealop] = P (PyTealLt | PyTealGt | PyTealLe | PyTealGe | PyTealAdd
                                                  | PyTealMinus | PyTealMul | PyTealDiv | PyTealMod
                                                  | PyTealExp | PyTealEq | PyTealNeq | PyTealAnd | PyTealOr 
                                                  | PyTealBitwiseAnd | PyTealBitwiseOr | PyTealBitwiseXor
                                                  | PyTealNot | PyTealBitwiseNot)
  
  def pyteal_byteslice_arithematic_op[$: P]: P[Ast.pytealop] = P (PyTealBytesLt | PyTealBytesGt | PyTealBytesLe | PyTealBytesGe 
                                                  | PyTealBytesAdd | PyTealBytesMinus | PyTealBytesMul | PyTealBytesDiv 
                                                  | PyTealBytesMod | PyTealBytesEq | PyTealBytesNeq | PyTealBytesAnd 
                                                  | PyTealBytesOr  | PyTealBytesXor | PyTealBytesNot | PyTealBytesZero)

  def pyteal_arithematic_op_symbols[$: P]: P[Ast.pytealop] = P (PyTealGt2 | PyTealMinus2 | PyTealAdd2)


  def pyteal_operators[$: P]: P[Ast.expr] = P ((pyteal_arithematic_op | pyteal_byteslice_arithematic_op) ~ "(" ~ (pytealInt | pytealBytes | Lexical.identifier).rep(1, sep = ",") ~ ")").map { 
    case (op, values) => Ast.expr.PyTealBinOp(op, values)
  }


  def pyteal_abi_types[$:P]: P[Ast.abitype] = P (Uint8 | Uint16 | Uint32 | Uint64 | Bool | Byte | StaticArray | Address
                                                  | StaticBytes | DynamicArray | DynamicBytes | String | Tuple | NamedTuple)


  def pyteal_args_list[$:P]: P[Ast.pytealarguments] = P ((Lexical.identifier ~ " ".rep() ~ ":" ~ " ".rep() ~ pyteal_abi_types).rep(1, sep = ",")).map {
    case (args) => Ast.pytealarguments(args)
  }

  def conditional_expr[$:P]: P[Ast.expr] = P ((if_expr | then_expr).rep(1, sep = ".")).map {
    case (expressions) => Ast.expr.PyTealConditionalExpr(expressions)
  }

  def scratch_load[$:P]: P[Ast.expr] = P (Lexical.identifier ~ "." ~ "load" ~ "()").map {
    case (identifier) => Ast.expr.ScratchLoad(identifier)
  }

  def scratch_store[$:P]: P[Ast.expr] = P (Lexical.identifier ~ "." ~ "store" ~ "(" ~ (global_get | pytealInt | pytealBytesStored) ~ ")").map {
    case (identifier, expr) => Ast.expr.ScratchStore(identifier, expr)
  }

  def get[$:P]: P[Ast.expr] = P (Lexical.identifier ~ "." ~ "get" ~ "()").map {
    case (identifier) => Ast.expr.Get(identifier)
  }

  def pyteal_expr[$:P]: P[Ast.expr] = P ((global_get | pytealInt | pytealBytesStored | scratch_load | get | scratch_store | Lexical.identifier) ~ " ".rep() ~ pyteal_arithematic_op_symbols ~ " ".rep() ~ (global_get | pytealInt | pytealBytesStored | scratch_load | get | scratch_store | Lexical.identifier)).map {
    case (expr1, op, expr2) => Ast.expr.PyTealExpr(expr1, op, expr2)
  }

  def global_put[$:P]: P[Ast.expr] = P ("App" ~ "." ~ "globalPut" ~ "(" ~ (pytealBytesStored) ~ "," ~ " ".rep() ~ (pytealInt | pyteal_expr | Lexical.identifier) ~ ")").map {
    case (key, value) => Ast.expr.GlobalPut(key, value)
  }

  def global_get[$:P]: P[Ast.expr] = P ("App" ~ "." ~ "globalGet" ~ "(" ~ (pytealInt | pytealBytesStored | Lexical.identifier) ~ ")").map {
    case (key) => Ast.expr.GlobalGet(key)
  }

  def if_expr[$:P]: P[Ast.expr] = P ("If" ~ "(" ~ pyteal_expr ~ ")").map {
    case (expr) => Ast.expr.PyTealIf(expr)
  }

  def then_expr[$:P]: P[Ast.expr] = P ("Then" ~ "(" ~ global_put ~ ")").map {
    case (expr) => Ast.expr.PyTealThen(expr)
  }

  def approve[$:P]: P[Ast.expr] = P("Approve" ~ "()").map {
    case (_) => Ast.expr.Approve()
  }

  def pyteal_seq[$:P]: P[Ast.expr] = P ("Seq" ~ "(" ~ (global_put | approve | conditional_expr | scratch_store).rep(1, sep = ",") ~ ")").map {
    case (values) => Ast.expr.PyTealSeq(values)
  }

  def Unary[$: P](p: => P[Ast.expr]) =
    (unary_op ~ p).map{ case (op, operand) => Ast.expr.UnaryOp(op, operand) }

  def Chain[$: P](p: => P[Ast.expr], op: => P[Ast.operator]) = P( p ~ (op ~ p).rep ).map{
    case (lhs, chunks) =>
      chunks.foldLeft(lhs){case (lhs, (op, rhs)) =>
        Ast.expr.BinOp(lhs, op, rhs)
      }
  }
  def expr[$: P]: P[Ast.expr] = P( Chain(xor_expr, BitOr) )
  def xor_expr[$: P]: P[Ast.expr] = P( Chain(and_expr, BitXor) )
  def and_expr[$: P]: P[Ast.expr] = P( Chain(shift_expr, BitAnd) )
  def shift_expr[$: P]: P[Ast.expr] = P( Chain(arith_expr, LShift | RShift) )

  def arith_expr[$: P]: P[Ast.expr] = P( Chain(term, Add | Sub) )
  def term[$: P]: P[Ast.expr] = P( Chain(factor, Mult | FloorDiv | Div | Mod ) )

  def factor[$: P]: P[Ast.expr] = P( power | Unary(factor) )
  def power[$: P]: P[Ast.expr] = P( atom ~ trailer.rep ~ (Pow ~ factor).? ).map{
    case (lhs, trailers, rhs) =>
      val left = trailers.foldLeft(lhs)((l, t) => t(l))
      rhs match{
        case None => left
        case Some((op, right)) => Ast.expr.BinOp(left, op, right)
      }
  }
  def atom[$: P]: P[Ast.expr] = {
    def empty_tuple = ("(" ~ ")").map(_ => Ast.expr.Tuple(Nil, Ast.expr_context.Load))
    def empty_list = ("[" ~ "]").map(_ => Ast.expr.List(Nil, Ast.expr_context.Load))
    def empty_dict = ("{" ~ "}").map(_ => Ast.expr.Dict(Nil, Nil))
    P(
      empty_tuple  |
      empty_list |
      empty_dict |
      "(" ~ (yield_expr | generator | tuple | test) ~ ")" |
      "[" ~ (list_comp | list) ~ "]" |
      "{" ~ dictorsetmaker ~ "}" |
      "`" ~ testlist1.map(x => Ast.expr.Repr(Ast.expr.Tuple(x, Ast.expr_context.Load))) ~ "`" |
      STRING.rep(1).map(_.mkString).map(Ast.expr.Str.apply) |
      pyteal_operators |
      global_put |
      pyteal_seq |
      pyteal_expr |
      resultExpression |
      NAME.map(Ast.expr.Name(_, Ast.expr_context.Load)) |
      NUMBER
    )
  }
  def list_contents[$: P] = P( test.rep(1, ",") ~ ",".? )
  def list[$: P] = P( list_contents ).map(Ast.expr.List(_, Ast.expr_context.Load))
  def tuple_contents[$: P] = P( test ~ "," ~ list_contents.?).map { case (head, rest)  => head +: rest.getOrElse(Seq.empty) }
  def tuple[$: P] = P( tuple_contents).map(Ast.expr.Tuple(_, Ast.expr_context.Load))
  def list_comp_contents[$: P] = P( test ~ comp_for.rep(1) )
  def list_comp[$: P] = P( list_comp_contents ).map((Ast.expr.ListComp.apply _).tupled)
  def generator[$: P] = P( list_comp_contents ).map((Ast.expr.GeneratorExp.apply _).tupled)

  def lambdef[$: P]: P[Ast.expr.Lambda] = P( kw("lambda") ~ varargslist ~ ":" ~ test ).map((Ast.expr.Lambda.apply _).tupled)
  def trailer[$: P]: P[Ast.expr => Ast.expr] = {
    def call = P("(" ~ arglist ~ ")").map{ case (args, (keywords, starargs, kwargs)) => (lhs: Ast.expr) => Ast.expr.Call(lhs, args, keywords, starargs, kwargs)}
    def slice = P("[" ~ subscriptlist ~ "]").map(args => (lhs: Ast.expr) => Ast.expr.Subscript(lhs, args, Ast.expr_context.Load))
    def attr = P("." ~ NAME).map(id => (lhs: Ast.expr) => Ast.expr.Attribute(lhs, id, Ast.expr_context.Load))
    P( call | slice | attr )
  }
  def subscriptlist[$: P] = P( subscript.rep(1, ",") ~ ",".? ).map{
    case Seq(x) => x
    case xs => Ast.slice.ExtSlice(xs)
  }
  def subscript[$: P]: P[Ast.slice] = {
    def ellipses = P( ("." ~ "." ~ ".").map(_ => Ast.slice.Ellipsis) )
    def single = P( test.map(Ast.slice.Index.apply) )
    def multi = P(test.? ~ ":" ~ test.? ~ sliceop.?).map { case (lower, upper, step) =>
      Ast.slice.Slice(
        lower,
        upper,
        step.map(_.getOrElse(Ast.expr.Name(Ast.identifier("None"), Ast.expr_context.Load)))
      )
    }
    P( ellipses | multi | single )
  }

  def sliceop[$: P] = P( ":" ~ test.? )
  def exprlist[$: P]: P[Seq[Ast.expr]] = P( expr.rep(1, sep = ",") ~ ",".? )
  def testlist[$: P]: P[Seq[Ast.expr]] = P( (test | pyteal_seq).rep(1, sep = ",") ~ ",".? )
  def dictorsetmaker[$: P]: P[Ast.expr] = {
    def dict_item = P( test ~ ":" ~ test )
    def dict: P[Ast.expr.Dict] = P(
      (dict_item.rep(1, ",") ~ ",".?).map{x =>
        val (keys, values) = x.unzip
        Ast.expr.Dict(keys, values)
      }
    )
    def dict_comp = P(
      (dict_item ~ comp_for.rep(1)).map((Ast.expr.DictComp.apply _).tupled)
    )
    def set: P[Ast.expr.Set] = P( test.rep(1, ",") ~ ",".? ).map(Ast.expr.Set.apply)
    def set_comp = P( test ~ comp_for.rep(1) ).map((Ast.expr.SetComp.apply _).tupled)
    P( dict_comp | dict | set_comp | set)
  }

  def arglist[$: P] = {
    def inits = P( (plain_argument ~ !"=").rep(0, ",") )
    def later = P( named_argument.rep(0, ",") ~ ",".? ~ ("*" ~ test).? ~ ",".? ~ ("**" ~ test).? ~ ",".? ~ named_argument.rep(0, ",")).map{
      case (named1, dot, star, named2) => (named1 ++ named2, dot, star )
    }
    P( inits ~ ",".? ~ later )
  }

  def plain_argument[$: P] = P( test ~ comp_for.rep ).map{
    case (x, Nil) => x
    case (x, gens) => Ast.expr.GeneratorExp(x, gens)
  }
  def named_argument[$: P] = P( NAME ~ "=" ~ test  ).map((Ast.keyword.apply _).tupled)

  def comp_for[$: P]: P[Ast.comprehension] = P( kw("for") ~ exprlist ~ kw("in") ~ or_test ~ comp_if.rep ).map{
    case (targets, test, ifs) => Ast.comprehension(tuplize(targets), test, ifs)
  }
  def comp_if[$: P]: P[Ast.expr] = P( kw("if") ~ test )

  def testlist1[$: P]: P[Seq[Ast.expr]] = P( test.rep(1, sep = ",") )

  // not used in grammar, but may appear in "node" passed from Parser to Compiler
  //  def encoding_decl[$: P]: P0 = P( NAME )

  def yield_expr[$: P]: P[Ast.expr.Yield] = P( kw("yield") ~ testlist.map(tuplize).? ).map(Ast.expr.Yield.apply)

  def varargslist[$: P]: P[Ast.arguments] = {
    def named_arg = P( fpdef ~ ("=" ~ test).? )
    def x = P( named_arg.rep(sep = ",") ~ ",".? ~ ("*" ~ NAME).? ~ ",".? ~ ("**" ~ NAME).? ).map{
      case (normal_args, starargs, kwargs) =>
        val (args, defaults) = normal_args.unzip
        Ast.arguments(args, starargs, kwargs, defaults.flatten)
    }
    P( x )
  }

  def fpdef[$: P]: P[Ast.expr] = P( NAME.map(Ast.expr.Name(_, Ast.expr_context.Param)) | "(" ~ fplist ~ ")" )
  def fplist[$: P]: P[Ast.expr] = P( fpdef.rep(sep = ",") ~ ",".? ).map(Ast.expr.Tuple(_, Ast.expr_context.Param))

  def imprecisionExpression[$: P]: P[Ast.expr.ImprecisionExpression] = P( ParserPyTeal.span(kw("?"))).map{
    case (_, s) => Ast.expr.ImprecisionExpression(s)
  }

  def resultExpression[$: P]: P[Ast.expr.ResultExpression] = P(ParserPyTeal.span(kw("\\result")))
    .map({ case (_, s) => Ast.expr.ResultExpression(s) })
}