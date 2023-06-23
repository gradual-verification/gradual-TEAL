package gvteal.transformer

object IRPrinter {
  private object Precedence {
    val Unary = 1
    val Multiply = 2
    val Add = 3
    val Inequality = 4
    val Equality = 5
    val And = 6
    val Or = 7
    val Conditional = 8
    val Top = 9
  }

  def print(program: IR.Program, includeSpecs: Boolean): String = {
    val p = new Printer()

    def printList[T](values: Seq[T])(action: T => Unit): Unit = {
      var first = true
      for (value <- values) {
        if (first) first = false
        else p.print(", ")
        action(value)
      }
    }

    // PyTEAL: Change for imports; hardcoded
    def printDependencySimple(dependency: IR.Dependency): Unit = {
      p.print("#import ")
      p.print(dependency.path)
    }
    def printDependencyCompound(dependency: IR.Dependency): Unit = {
      p.print("#from ")
      p.print(dependency.path)
      p.print(" import ")
      p.print(dependency.functions)
    }
    def printStructHeader(struct: IR.Struct): Unit = {
      p.print("struct ")
      p.print(struct.name)
    }

    def printStruct(struct: IR.Struct): Unit = {
      printStructHeader(struct)
      p.println()
      p.println("{")
      p.withIndent {
        for (field <- struct.fields) {
          printType(field.valueType)
          p.print(" ")
          p.print(field.name)
          p.println(";")
        }
      }
      p.println("};")
    }

    def printType(t: IR.Type): Unit = p.print(t.name)

    def printPredicateHeader(predicate: IR.Predicate): Unit = {
      p.print("//@predicate ")
      p.print(predicate.name)
      p.print("(")
      printList(predicate.parameters) { param =>
        printType(param.varType)
        p.print(" ")
        p.print(param.name)
      }
      p.print(")")
    }

    def printPredicate(predicate: IR.Predicate): Unit = {
      printPredicateHeader(predicate)
      p.print(" = ")
      printExpr(predicate.expression)
      p.println(";")
    }

    def printMethodHeader(method: IR.Method): Unit = {
      method.returnType match {
        case None      => p.print("void")
        case Some(ret) => printType(ret)
      }

      p.print(" ")
      p.print(method.name)
      p.print("(")

      var first = true
      printList(method.parameters) { param =>
        printType(param.varType)
        p.print(" ")
        p.print(param.name)
      }

      p.print(")")
    }

    def printMethod(method: IR.Method): Unit = {
      printMethodHeader(method)
      p.println()

      if (includeSpecs) {
        method.precondition.foreach { pre =>
          p.withIndent {
            p.print("//@requires ")
            printExpr(pre)
            p.println(";")
          }
        }

        method.postcondition.foreach { post =>
          p.withIndent {
            p.print("//@ensures ")
            printExpr(post)
            p.println(";")
          }
        }
      }
      p.println("{")
      p.withIndent {
        method.variables.foreach(printVar)
        method.body.foreach(printOp)
      }
      p.println("}")
    }

    def printVar(v: IR.Var): Unit = {
      p.print(v.varType.name)
      p.print(" ")
      p.print(v.name)
      v.varType match {
        case _: IR.ArrayType | _: IR.ReferenceArrayType => ()
        case varType => {
          p.print(" = ")
          printExpr(varType.default)
        }
      }
      p.println(";")
    }

    def printBlock(block: IR.Block): Unit = {
      p.println("{")
      p.withIndent(block.foreach(printOp))
      p.println("}")
    }

    def printOp(op: IR.Op): Unit = op match {
      case invoke: IR.Invoke => {
        invoke.target.foreach { target =>
          printExpr(target)
          p.print(" = ")
        }

        p.print(invoke.callee.name)
        p.print("(")

        printList(invoke.arguments) { arg =>
          printExpr(arg)
        }

        p.println(");")
      }
      case alloc: IR.AllocValue => {
        printExpr(alloc.target)
        p.print(" = alloc(")
        printType(alloc.valueType)
        p.println(");")
      }

      case alloc: IR.AllocArray => {
        printExpr(alloc.target)
        p.print(" = alloc_array(")
        printType(alloc.valueType)
        p.print(", ")
        printExpr(alloc.length)
        p.println(");")
      }

      case alloc: IR.AllocStruct => {
        printExpr(alloc.target)
        p.print(" = alloc(struct ")
        p.print(alloc.struct.name)
        p.println(");")
      }

      case assign: IR.Assign => {
        printExpr(assign.target)
        p.print(" = ")
        printExpr(assign.value)
        p.println(";")
      }

      case assign: IR.AssignMember => {
        assign.member match {
          case member: IR.FieldMember => {
            printExpr(member.root)
            p.print("->")
            p.print(member.field.name)
          }
          case member: IR.DereferenceMember => {
            p.print("*")
            printExpr(member.root, Precedence.Unary)
          }
          case member: IR.ArrayMember => {
            printExpr(member.root)
            p.print("[")
            printExpr(member.index)
            p.print("]")
          }
        }

        p.print(" = ")
        printExpr(assign.value)
        p.println(";")
      }

      case assert: IR.Assert =>
        assert.kind match {
          case IR.AssertKind.Specification =>
            if (includeSpecs) {
              p.print("//@assert ")
              printExpr(assert.value)
              p.println(";")
            }
          case IR.AssertKind.Imperative => {
            p.print("assert(")
            printExpr(assert.value)
            p.println(");")
          }
        }

      case fold: IR.Fold =>
        if (includeSpecs) {
          p.print("//@fold ")
          printExpr(fold.instance)
          p.println(";")
        }

      case unfold: IR.Unfold =>
        if (includeSpecs) {
          p.print("//@unfold ")
          printExpr(unfold.instance)
          p.println(";")
        }

      case error: IR.Error => {
        p.print("error(")
        printExpr(error.value)
        p.println(");")
      }

      case ret: IR.Return => {
        p.print("return")
        ret.value.foreach { value =>
          p.print(" ")
          printExpr(value)
        }
        p.println(";")
      }

      case iff: IR.If => {
        p.print("if (")
        printExpr(iff.condition)
        p.println(")")
        printBlock(iff.ifTrue)

        if (!iff.ifFalse.isEmpty) {
          p.println("else")
          printBlock(iff.ifFalse)
        }
      }

      case w: IR.While => {
        p.print("while (")
        printExpr(w.condition)
        p.println(")")
        if (includeSpecs) {
          p.withIndent {
            p.print("//@loop_invariant ")
            printExpr(w.invariant)
            p.println(";")
          }
        }
        printBlock(w.body)
      }
    }

    def wrapExpr(currentPrecedence: Int, exprPrecedence: Int)(
        action: => Unit
    ): Unit = {
      if (currentPrecedence < exprPrecedence) {
        p.print("(")
        action
        p.print(")")
      } else {
        action
      }
    }

    def printExpr(
        expr: IR.Expression,
        precedence: Int = Precedence.Top
    ): Unit = expr match {
      case v: IR.Var => p.print(v.name)
      case m: IR.FieldMember => {
        printExpr(m.root)
        p.print("->")
        p.print(m.field.name)
      }
      case deref: IR.DereferenceMember =>
        wrapExpr(precedence, Precedence.Unary) {
          p.print("*")
          printExpr(deref.root, Precedence.Unary)
        }
      case acc: IR.Accessibility => {
        p.print("acc(")
        printExpr(acc.member)
        p.print(")")
      }
      case pred: IR.PredicateInstance => {
        p.print(pred.predicate.name)
        p.print("(")
        printList(pred.arguments) { arg => printExpr(arg) }
        p.print(")")
      }
      case arr: IR.ArrayMember => {
        printExpr(arr.root)
        p.print("[")
        printExpr(arr.index)
        p.print("]")
      }
      case res: IR.Result => p.print("\\result")
      case imp: IR.Imprecise =>
        imp.precise match {
          case None => p.print("?")
          case Some(precise) =>
            wrapExpr(precedence, Precedence.And) {
              p.print("? && ")
              printExpr(precise, Precedence.And)
            }
        }
      case int: IR.IntLit => p.print(int.value.toString())
      case str: IR.StringLit =>
        p.print("\"")
        p.print(str.value)
        p.print("\"")
      case char: IR.CharLit => {
        p.print("'")
        p.print(char.value match {
          case '\\'  => "\\\\"
          case '\n'  => "\\n"
          case '\r'  => "\\r"
          case '\t'  => "\\t"
          case '\u0000' => "\\0"
          case other => other.toString()
        })
        p.print("'")
      }
      case bool: IR.BoolLit => p.print(if (bool.value) "true" else "false")
      case _: IR.NullLit    => p.print("NULL")

      case cond: IR.Conditional =>
        wrapExpr(precedence, Precedence.Conditional) {
          printExpr(cond.condition, Precedence.Conditional)
          p.print(" ? ")
          printExpr(cond.ifTrue, Precedence.Conditional)
          p.print(" : ")
          printExpr(cond.ifFalse, Precedence.Conditional)
        }

      case binary: IR.Binary => {
        val (sep, opPrecedence) = binary.operator match {
          case IR.BinaryOp.Add            => (" + ", Precedence.Add)
          case IR.BinaryOp.Subtract       => (" - ", Precedence.Add)
          case IR.BinaryOp.Divide         => (" / ", Precedence.Multiply)
          case IR.BinaryOp.Multiply       => (" * ", Precedence.Multiply)
          case IR.BinaryOp.And            => (" && ", Precedence.And)
          case IR.BinaryOp.Or             => (" || ", Precedence.Or)
          case IR.BinaryOp.Equal          => (" == ", Precedence.Equality)
          case IR.BinaryOp.NotEqual       => (" != ", Precedence.Equality)
          case IR.BinaryOp.Less           => (" < ", Precedence.Inequality)
          case IR.BinaryOp.LessOrEqual    => (" <= ", Precedence.Inequality)
          case IR.BinaryOp.Greater        => (" > ", Precedence.Inequality)
          case IR.BinaryOp.GreaterOrEqual => (" >= ", Precedence.Inequality)
        }

        wrapExpr(precedence, opPrecedence) {
          printExpr(binary.left, opPrecedence)
          p.print(sep)
          printExpr(binary.right, opPrecedence)
        }
      }

      case unary: IR.Unary =>
        wrapExpr(precedence, Precedence.Unary) {
          p.print(unary.operator match {
            case IR.UnaryOp.Not    => "!"
            case IR.UnaryOp.Negate => "-"
          })
          printExpr(unary.operand, Precedence.Unary)
        }
    }

    var empty = true
    def printSeparator() = {
      if (!empty) {
        empty = true
        p.println()
      }
    }

    for (dep <- program.dependencies) {
      if (dep.functions == "") {
        printDependencySimple(dep)
        p.println()
      }
      else {
        printDependencyCompound(dep)
        p.println()
      }
    }

    for (struct <- program.structs) {
      printStructHeader(struct)
      p.println(";")
      empty = false
    }

    printSeparator()

    for (struct <- program.structs) {
      printStruct(struct)
      p.println()
    }

    if (includeSpecs) {
      for (predicate <- program.predicates) {
        printPredicateHeader(predicate)
        p.println(";")
        empty = false
      }

      printSeparator()

      for (predicate <- program.predicates) {
        printPredicate(predicate)
        empty = false
      }
    }

    printSeparator()

    for (method <- program.methods) {
      printMethodHeader(method)
      p.println(";")
      empty = false
    }

    printSeparator()

    var first = true
    for (method <- program.methods) {
      if (first) first = false
      else p.println()
      printMethod(method)
    }

    p.toString()
  }

  private class Printer {
    var indentLevel = 0
    var startedLine = false
    val indentValue = "  "
    private val builder = new StringBuilder()

    private def startLine(): Unit = {
      if (!startedLine) {
        startedLine = true
        for (_ <- 0 until indentLevel) {
          builder ++= indentValue
        }
      }
    }

    def withIndent[T](action: => Unit): Unit = {
      indentLevel += 1
      action
      indentLevel -= 1
    }

    def print(value: String): Unit = {
      startLine()
      builder ++= value
    }

    def println(value: String): Unit = {
      startLine()
      builder ++= value
      builder += '\n'
      startedLine = false
    }

    def println(): Unit = {
      builder += '\n'
      startedLine = false
    }

    override def toString(): String = builder.toString()
  }
}
