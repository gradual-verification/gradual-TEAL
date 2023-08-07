package gvteal.analyzer
import fastparse.Parsed.{Failure, Success}
import gvteal.parser._
import gvteal.pytealparser._

import java.nio.file.{Files, Path, Paths}
import scala.collection.mutable.ListBuffer
import java.io.IOException
import scala.util.Random
import gvteal.pytealparser.Ast.pytealop.PyTealAdd
import gvteal.pytealparser.Ast.pytealop.PyTealMinus
import gvteal.pytealparser.Ast.pytealop.PyTealBytesMul
import gvteal.pytealparser.Ast.pytealop.PyTealBytesDiv

trait ResolvedNode {
  val parsed: Node
}

case class ResolvedProgram(
    methodDeclarations: List[ResolvedMethodDeclaration],
    // functionDefinitions: List[ResolvedFunctionDefinition],
    // functionDeclarations: List[ResolvedFunctionDeclaration],
    methodDefinitions: List[ResolvedMethodDefinition],
    predicateDeclarations: List[ResolvedPredicateDeclaration],
    predicateDefinitions: List[ResolvedPredicateDefinition],
    structDefinitions: List[ResolvedStructDefinition],
    types: List[ResolvedTypeDef],
    dependencies: List[ResolvedUseDeclaration],
    // simpleDependencies: List[ResolvedImportSimpleDeclaration]
    // compoundDependencies: List[ResolvedImportCompoundDeclaration]
)

case class Scope(
    variables: Map[String, ResolvedVariable],
    methodDeclarations: Map[String, ResolvedMethodDeclaration],
    // functionDefinitions: Map[String, ResolvedFunctionDefinition],
    // functionDeclarations: Map[String, ResolvedFunctionDeclaration],
    methodDefinitions: Map[String, ResolvedMethodDefinition],
    predicateDeclarations: Map[String, ResolvedPredicateDeclaration],
    predicateDefinitions: Map[String, ResolvedPredicateDefinition],
    structDefinitions: Map[String, ResolvedStructDefinition],
    typeDefs: Map[String, ResolvedTypeDef],
    libraries: Map[Path, ResolvedProgram],
    errors: ErrorSink
) {
  def defineStruct(struct: ResolvedStructDefinition): Scope = {
    if (structDefinitions.contains(struct.name)) {
      errors.error(
        struct.parsed,
        "'struct " + struct.name + "' is already defined"
      )
      this
    } else {
      copy(structDefinitions = structDefinitions + (struct.name -> struct))
    }
  }

  def defineType(typeDef: ResolvedTypeDef): Scope = {
    if (typeDefs.contains(typeDef.name)) {
      errors.error(typeDef.parsed, "'" + typeDef.name + "' is already defined")
      this
    } else {
      if (methodDeclarations.contains(typeDef.name)) {
        // Log error but add to scope
        errors.error(
          typeDef.parsed,
          "Type name '" + typeDef.name + "' already used as a method"
        )
      }

      copy(typeDefs = typeDefs + (typeDef.name -> typeDef))
    }
  }

  def declareMethod(method: ResolvedMethodDeclaration): Scope = {
    if (methodDeclarations.contains(method.name)) {
      this
    } else {
      copy(methodDeclarations = methodDeclarations + (method.name -> method))
    }
  }

  def defineMethod(method: ResolvedMethodDefinition): Scope = {
    if (methodDefinitions.contains(method.name)) {
      errors.error(method.parsed, "'" + method.name + "' is already defined")
      this
    } else {
      if (typeDefs.contains(method.name)) {
        // Log error but add to scope
        errors.error(
          method.parsed,
          "Method '" + method.name + "' already used as a type name"
        )
      }
      copy(methodDefinitions = methodDefinitions + (method.name -> method))
    }
  }

  // def defineFunction(function: ResolvedFunctionDefinition): Scope = {
  //   if (functionDefinitions.contains(function.name)) {
  //     errors.error(function.parsed, "'", + function.name + "' is already defined")
  //     this 
  //   } else {
  //     if(typeDefs.contains(function.name)) {
  //       errors.error(
  //         function.parsed,
  //         "Function '" + function.name + "' already uses a type name"
  //       )
  //     }
  //     copy(functionDefinitions = functionDefinitions + (function.name -> function))
  //   }
  // }

  // def declareFunction(function: ResolvedFunctionDeclaration): Scope = {
  //   if (functionDeclarations.contains(function.name)) {
  //     this
  //   } else {
  //     copy(functionDeclarations = functionDeclarations + (function.name -> function))
  //   }
  // }

  def declarePredicate(predicate: ResolvedPredicateDeclaration): Scope = {
    if (predicateDeclarations.contains(predicate.name)) this
    else
      copy(
        predicateDeclarations =
          predicateDeclarations + (predicate.name -> predicate))
  }

  def definePredicate(predicate: ResolvedPredicateDefinition): Scope = {
    if (predicateDefinitions.contains(predicate.name)) {
      errors.error(predicate, s"'${predicate.name}' is already defined")
      this
    } else {
      copy(
        predicateDefinitions =
          predicateDefinitions + (predicate.name -> predicate))
    }
  }

  def declareVariable(variable: ResolvedVariable): Scope = {
    if (variables.contains(variable.name)) {
      errors.error(
        variable.parsed,
        "'" + variable.name + "' is already declared"
      )
      this
    } else {
      // If it shadows a type name, log an error but add it to the scope to avoid
      // extra undeclared-variable errors
      if (typeDefs.contains(variable.name)) {
        errors.error(
          variable.parsed,
          "Type name '" + variable.name + "' used as a variable"
        )
      }

      copy(variables = variables + (variable.name -> variable))
    }
  }

  def declareVariables(variables: Seq[ResolvedVariable]) = {
    // Add one-by-one to check for already defined variables
    variables.foldLeft(this) { _.declareVariable(_) }
  }

  def declareLibrary(path: Path, library: ResolvedProgram): Scope =
    copy(libraries = libraries + (path -> library))
}

object Resolver {
  val reservedWords = Set(
    "int",
    "string",
    "char",
    "bool",
    "\result",
    "struct",
    "typedef"
  )

  sealed trait Context
  case object MethodContext extends Context
  case object SpecificationContext extends Context
  case class PostConditionContext(returnType: ResolvedType) extends Context

  def resolveType(input: Type, scope: Scope): ResolvedType = {
    input match {
      case ArrayType(valueType, _) =>
        ResolvedArray(resolveType(valueType, scope))
      case PointerType(valueType, _) =>
        ResolvedPointer(resolveType(valueType, scope))
      case NamedStructType(id, _) => resolveStruct(id, scope)
      case NamedType(id, _)       => resolveNamedType(id, scope)
    }
  }

  def resolveStruct(id: Identifier, scope: Scope): ResolvedType = {
    // Structs may be used before they are declared or defined
    ResolvedStructType(id.name)
  }

  def resolveNamedType(id: Identifier, scope: Scope): ResolvedType = {
    val name = id.name
    BuiltinType.lookup.get(name) match {
      case Some(value) => value
      case None =>
        scope.typeDefs.get(name) match {
          case Some(typeDef) => typeDef.actualType
          case None => {
            scope.errors.error(id, "Undefined type " + name)
            MissingNamedType(name)
          }
        }
    }
  }

  def resolveStructDefinition(
      input: StructDefinition,
      scope: Scope
  ): ResolvedStructDefinition = {
    val fields = ListBuffer[ResolvedStructField]()
    for (field <- input.fields.get) {
      if (fields.exists(_.name == field.id.name)) {
        scope.errors.error(
          field,
          "Field '" + field.id.name + "' is already defined"
        )
      }

      fields += ResolvedStructField(
        parsed = field,
        name = field.id.name,
        structName = input.id.name,
        valueType = resolveType(field.valueType, scope)
      )
    }

    ResolvedStructDefinition(input, input.id.name, fields.toList)
  }

  def resolveTypeDef(input: TypeDefinition, scope: Scope): ResolvedTypeDef = {
    ResolvedTypeDef(
      parsed = input,
      name = input.id.name,
      actualType = resolveType(input.value, scope)
    )
  }

  def resolveStatement(input: Statement, scope: Scope): ResolvedStatement = {
    input match {
      // Variable defs are processed in resolveBlock
      case variable: VariableStatement => ???

      case expr: ExpressionStatement =>
        expr.expression match {
          // Special-case increment inside an ExpressionStatement since that is the only
          // valid position to encounter an increment expression
          case increment: IncrementExpression => {
            ResolvedIncrement(
              parsed = expr,
              value = resolveExpression(increment.value, scope, MethodContext),
              operation = increment.operator match {
                case IncrementOperator.Increment => IncrementOperation.Increment
                case IncrementOperator.Decrement => IncrementOperation.Decrement
              }
            )
          }

          case _ =>
            ResolvedExpressionStatement(
              parsed = expr,
              value = resolveExpression(expr.expression, scope, MethodContext)
            )
        }

      case assign: AssignmentStatement =>
        ResolvedAssignment(
          parsed = assign,
          left = resolveExpression(assign.left, scope, MethodContext),
          value = resolveExpression(assign.right, scope, MethodContext),
          operation = convertAssignOp(assign, scope)
        )

      case iff: IfStatement =>
        ResolvedIf(
          parsed = iff,
          condition = resolveExpression(iff.condition, scope, MethodContext),
          ifTrue = resolveScopedStatement(iff.ifTrue, scope),
          ifFalse = iff.ifFalse.map(resolveScopedStatement(_, scope))
        )

      case w: WhileStatement => {
        val (invariant, body) = resolveLoopInvariants(w.body, scope)
        ResolvedWhile(
          parsed = w,
          condition = resolveExpression(w.condition, scope, MethodContext),
          body = resolveScopedStatement(body, scope),
          invariant = invariant
        )
      }

      case f: ForStatement => {
        // Parse does not allow blocks in the incrementor, but does allow variable declarations,
        // which are disallowed by the spec
        if (f.incrementor.isInstanceOf[VariableStatement]) {
          scope.errors.error(
            f.incrementor,
            "Invalid declaration in for loop step"
          )
        }

        // Rewrite for into a while loop
        // For loops introduce their own scope, so wrap it all in a block
        // The spans get a little jumbled; for example, the incrementor is outside the block span
        resolveBlock(
          parsed = f,
          scope = scope,
          body = List(
            f.initializer,
            WhileStatement(
              condition = f.condition,
              span = f.span,
              specifications = List.empty,
              body = BlockStatement(
                // Move body specs to the while body so that loop invariants
                // are in the right place
                body = List(
                  f.body.withSpecifications(List.empty),
                  f.incrementor
                ),
                span = f.body.span,
                specifications = f.body.specifications,
                trailingSpecifications = List.empty
              )
            )
          ),
          specifications = f.specifications
        )
      }

      case r: ReturnStatement =>
        ResolvedReturn(
          parsed = r,
          value = r.value.map(resolveExpression(_, scope, MethodContext))
        )

      case a: AssertStatement =>
        ResolvedAssert(a, resolveExpression(a.value, scope, MethodContext))

      case e: ErrorStatement =>
        ResolvedError(e, resolveExpression(e.value, scope, MethodContext))

      case b: BlockStatement => resolveBlock(b, scope, b.body)
    }
  }

  def convertAssignOp(
      stmt: AssignmentStatement,
      scope: Scope
  ): Option[ArithmeticOperation] =
    stmt.operator match {
      case AssignOperator.Assign   => None
      case AssignOperator.Add      => Some(ArithmeticOperation.Add)
      case AssignOperator.Divide   => Some(ArithmeticOperation.Divide)
      case AssignOperator.Multiply => Some(ArithmeticOperation.Multiply)
      case AssignOperator.Subtract => Some(ArithmeticOperation.Subtract)
      case _ => {
        scope.errors.error(stmt, "Unsupported assignment operator")
        None
      }
    }

  def resolveBlock(
      parsed: Statement,
      scope: Scope,
      body: List[Statement],
      specifications: List[Specification] = List.empty,
      trailingSpecifications: List[Specification] = List.empty
  ): ResolvedBlock = {
    var blockScope = scope
    var defs = ListBuffer[ResolvedVariable]()
    var resolved = ListBuffer[ResolvedStatement]()

    resolved ++= resolveSpecs(specifications, blockScope)

    for (stmt <- body) {
      resolved ++= resolveSpecs(stmt.specifications, blockScope)

      stmt match {
        case v: VariableStatement => {
          val varDef = ResolvedVariable(
            parsed = v,
            name = v.id.name,
            valueType = resolveType(v.valueType, scope)
          )

          defs += varDef

          v.initialValue match {
            case None => ()
            case Some(value) => {
              resolved += ResolvedAssignment(
                parsed = v,
                left = ResolvedVariableRef(v.id, Some(varDef)),
                value = resolveExpression(value, blockScope, MethodContext),
                operation = None
              )
            }
          }

          blockScope = blockScope.declareVariable(varDef)
        }

        case _ => {
          resolved += resolveStatement(stmt, blockScope)
        }
      }
    }

    resolved ++= resolveSpecs(trailingSpecifications, blockScope)

    ResolvedBlock(
      parsed = parsed,
      variableDefs = defs.toList,
      statements = resolved.toList
    )
  }

  def resolveScopedStatement(input: Statement, scope: Scope): ResolvedBlock = {
    // Statements such as if and while introduce a transient scope. A variable definition
    // is valid, but it does not exist beyond that single statement. Moreover, specifications
    // can be added to any statement, which would generate multiple resolved statements.
    // Because we are handling all variable definitions at the block level, just rewrite the
    // single statement to always be a block statement.
    val (body, trailing) = input match {
      case block: BlockStatement => (block.body, block.trailingSpecifications)
      case _                     => (List(input), List.empty)
    }

    resolveBlock(input, scope, body, input.specifications, trailing)
  }

  def resolveExpression(
      input: Expression,
      scope: Scope,
      context: Context
  ): ResolvedExpression = {
    input match {
      case variable: VariableExpression =>
        resolveVariable(variable.variable, scope)

      case increment: IncrementExpression => {
        scope.errors.error(
          increment,
          "Increment/decrement operators cannot be used as an expression"
        )
        return resolveExpression(increment.value, scope, context)
      }

      case binary: BinaryExpression => {
        val left = resolveExpression(binary.left, scope, context)
        val right = resolveExpression(binary.right, scope, context)

        binary.operator match {
          case BinaryOperator.Add =>
            ResolvedArithmetic(binary, left, right, ArithmeticOperation.Add)
          case BinaryOperator.Subtract =>
            ResolvedArithmetic(
              binary,
              left,
              right,
              ArithmeticOperation.Subtract
            )
          case BinaryOperator.Divide =>
            ResolvedArithmetic(binary, left, right, ArithmeticOperation.Divide)
          case BinaryOperator.Multiply =>
            ResolvedArithmetic(
              binary,
              left,
              right,
              ArithmeticOperation.Multiply
            )
          case BinaryOperator.Equal =>
            ResolvedComparison(binary, left, right, ComparisonOperation.EqualTo)
          case BinaryOperator.NotEqual =>
            ResolvedComparison(
              binary,
              left,
              right,
              ComparisonOperation.NotEqualTo
            )
          case BinaryOperator.Greater =>
            ResolvedComparison(
              binary,
              left,
              right,
              ComparisonOperation.GreaterThan
            )
          case BinaryOperator.GreaterEqual =>
            ResolvedComparison(
              binary,
              left,
              right,
              ComparisonOperation.GreaterThanOrEqualTo
            )
          case BinaryOperator.Less =>
            ResolvedComparison(
              binary,
              left,
              right,
              ComparisonOperation.LessThan
            )
          case BinaryOperator.LessEqual =>
            ResolvedComparison(
              binary,
              left,
              right,
              ComparisonOperation.LessThanOrEqualTo
            )
          case BinaryOperator.LogicalAnd =>
            ResolvedLogical(binary, left, right, LogicalOperation.And)
          case BinaryOperator.LogicalOr =>
            ResolvedLogical(binary, left, right, LogicalOperation.Or)
          case _ => {
            // Log the error and return a mock that assumes add
            scope.errors.error(
              binary,
              "Unsupported operator " + binary.operator.toString()
            )
            ResolvedArithmetic(binary, left, right, ArithmeticOperation.Add)
          }
        }
      }

      case unary: UnaryExpression =>
        unary.operator match {
          case UnaryOperator.Not =>
            ResolvedNot(unary, resolveExpression(unary.operand, scope, context))
          case op => {
            // Log the error and return the base expression
            scope.errors.error(unary, "Unsupported operator " + op.toString())
            resolveExpression(unary.operand, scope, context)
          }
        }

      case ternary: TernaryExpression =>
        ResolvedTernary(
          ternary,
          resolveExpression(ternary.condition, scope, context),
          resolveExpression(ternary.ifTrue, scope, context),
          resolveExpression(ternary.ifFalse, scope, context)
        )

      case invoke: InvokeExpression if context != MethodContext => {
        // Invokes in a specification must refer to a predicate
        resolvePredicate(
          invoke,
          invoke.method,
          invoke.arguments,
          scope,
          context
        )
      }

      case invoke: InvokeExpression => {
        val name = invoke.method.name

        val method =
          if (scope.variables.contains(name)) {
            scope.errors.error(invoke, s"'$name' is a variable, not a function")
            None
          } else if (scope.predicateDeclarations.contains(name)) {
            scope.errors.error(
              invoke,
              s"'$name' is a predicate, not a function"
            )
            None
          } else {
            val decl = scope.methodDeclarations.get(name)
            if (!decl.isDefined) {
              scope.errors.error(invoke, s"'$name' is not declared")
            }
            decl
          }

        ResolvedInvoke(
          parsed = invoke,
          method = method,
          methodName = name,
          arguments = invoke.arguments.map(resolveExpression(_, scope, context))
        )
      }

      case alloc: AllocExpression => {
        val valueType = resolveType(alloc.valueType, scope)
        verifyDefinedType(valueType, alloc, scope)
        ResolvedAlloc(alloc, valueType)
      }

      case alloc: AllocArrayExpression => {
        val valueType = resolveType(alloc.valueType, scope)
        verifyDefinedType(valueType, alloc, scope)
        ResolvedAllocArray(
          alloc,
          valueType,
          resolveExpression(alloc.length, scope, context)
        )
      }

      case index: IndexExpression =>
        ResolvedArrayIndex(
          index,
          resolveExpression(index.parent, scope, context),
          resolveExpression(index.index, scope, context)
        )

      case result: ResultExpression => {
        val retType = context match {
          case PostConditionContext(returnType) => returnType
          case _ => {
            scope.errors.error(
              result,
              "\\result expressions can only be used in 'ensures'"
            )
            UnknownType
          }
        }

        ResolvedResult(result, retType)
      }

      case length: LengthExpression => {
        if (context == MethodContext)
          scope.errors.error(
            length,
            "\\length expressions can only be used in specifications"
          )
        ResolvedLength(length, resolveExpression(length.value, scope, context))
      }

      case acc: AccessibilityExpression => {
        if (context == MethodContext) {
          scope.errors.error(
            acc,
            "acc() expressions can only be used in specifications"
          )
        }
        ResolvedAccessibility(acc, resolveExpression(acc.field, scope, context))
      }

      case imprecision: ImprecisionExpression => {
        if (context == MethodContext)
          scope.errors.error(
            imprecision,
            "? expressions can only be used in specifications"
          )
        ResolvedImprecision(imprecision)
      }

      case member: MemberExpression => {
        val parent = resolveExpression(member.parent, scope, context)
        val struct =
          if (member.isArrow) {
            parent.valueType match {
              case ResolvedPointer(ResolvedStructType(struct)) => Some(struct)
              case _ => {
                scope.errors.error(
                  member,
                  "Subject of '->' is not a pointer to a struct"
                )
                None
              }
            }
          } else {
            parent.valueType match {
              case ResolvedStructType(struct) => Some(struct)
              case _ => {
                scope.errors.error(member, "Subject of '.' is not a struct")
                None
              }
            }
          }

        val fieldName = member.field.name
        val field = struct match {
          case None => None

          case Some(struct) =>
            scope.structDefinitions.get(struct) match {
              case None => {
                scope.errors.error(
                  member,
                  "'struct " + struct + "' is not defined"
                )
                None
              }

              case Some(definition) =>
                definition.fields.find(_.name == fieldName) match {
                  case None => {
                    scope.errors.error(
                      member,
                      "'" + fieldName + "' is not defined in '" + parent.valueType.name
                    )
                    None
                  }

                  case Some(field) => Some(field)
                }
            }
        }

        ResolvedMember(
          parsed = member,
          parent = parent,
          field = field,
          fieldName = fieldName,
          isArrow = member.isArrow
        )
      }

      case string: StringExpression  => ResolvedString(string)
      case char: CharacterExpression => ResolvedChar(char)
      case int: IntegerExpression    => ResolvedInt(int)
      case bool: BooleanExpression   => ResolvedBool(bool)
      case n: NullExpression         => ResolvedNull(n)
    }
  }

  def resolveVariable(id: Identifier, scope: Scope): ResolvedExpression = {
    val variable = scope.variables.get(id.name)
    if (!variable.isDefined) {
      scope.errors.error(id, "'" + id.name + "' is not defined")
    }

    ResolvedVariableRef(id, variable)
  }

  def verifyDefinedType(t: ResolvedType, node: Node, scope: Scope): Unit = {
    t match {
      case ResolvedStructType(name) => {
        if (!scope.structDefinitions.contains(name)) {
          scope.errors.error(node, "'struct " + name + "' is not defined")
        }
      }

      case ResolvedPointer(valueType) =>
        verifyDefinedType(valueType, node, scope)
      case ResolvedArray(valueType) => verifyDefinedType(valueType, node, scope)
      case _                        => ()
    }
  }

  def resolveSpecs(
      specs: List[Specification],
      scope: Scope
  ): List[ResolvedStatement] = {
    specs.flatMap {
      case assert: AssertSpecification => {
        val value = resolveExpression(assert.value, scope, SpecificationContext)
        Some(ResolvedAssertSpecification(assert, value))
      }

      case unfold: UnfoldSpecification => {
        val predicate = resolvePredicate(
          unfold,
          unfold.predicate,
          unfold.arguments,
          scope,
          SpecificationContext
        )
        Some(ResolvedUnfoldPredicate(unfold, predicate))
      }

      case fold: FoldSpecification => {
        val predicate = resolvePredicate(
          fold,
          fold.predicate,
          fold.arguments,
          scope,
          SpecificationContext
        )
        Some(ResolvedFoldPredicate(fold, predicate))
      }
      case other => {
        scope.errors.error(other, "Invalid specification")
        None
      }
    }
  }

  def resolvePredicate(
      parsed: Node,
      id: Identifier,
      args: List[Expression],
      scope: Scope,
      context: Context
  ): ResolvedPredicate = {
    val name = id.name
    val predicate =
      if (scope.variables.contains(name)) {
        scope.errors.error(id, s"'$name' is a variable, not a predicate")
        None
      } else if (scope.methodDeclarations.contains(name)) {
        scope.errors.error(id, s"'$name' is a method, not a predicate")
        None
      } else {
        val decl = scope.predicateDeclarations.get(name)
        if (!decl.isDefined) {
          scope.errors.error(id, s"'$name' is not declared")
        }
        decl
      }

    val arguments = args.map(resolveExpression(_, scope, context))

    ResolvedPredicate(
      parsed = parsed,
      predicate = predicate,
      predicateName = name,
      arguments = arguments
    )
  }

  def combineBooleans(
      expressions: Seq[ResolvedExpression]
  ): Option[ResolvedExpression] = {
    expressions.foldLeft[Option[ResolvedExpression]](None)((current, expr) => {
      current match {
        case None => Some(expr)
        case Some(value) =>
          Some(
            ResolvedLogical(
              parsed = expr.parsed,
              left = value,
              right = expr,
              operation = LogicalOperation.And
            )
          )
      }
    })
  }

  def resolveLoopInvariants(
      stmt: Statement,
      scope: Scope
  ): (Option[ResolvedExpression], Statement) = {
    // Rewrite the loop body removing loop invariant specifications
    val loopInvariants = stmt.specifications.collect {
      case li: LoopInvariantSpecification => li
    }
    val invariant = combineBooleans(
      loopInvariants.map(spec =>
        resolveExpression(spec.value, scope, SpecificationContext))
    )
    val otherSpecs =
      stmt.specifications.filterNot(_.isInstanceOf[LoopInvariantSpecification])
    (invariant, stmt.withSpecifications(otherSpecs))
  }

  def resolveMethodArguments(args: List[MemberDefinition], scope: Scope) =
    args.map(arg =>
      ResolvedVariable(arg, arg.id.name, resolveType(arg.valueType, scope)))

  // TODO: Finish resolveFunctionDeclaration and resolveFunctionDefinition
  // def resolveFunctionDeclaration(
  //   input: FunctionDefinition, 
  //   scope: Scope
  // ): ResolvedFunctionDeclaration = {
  // //   val parameters = resolveFunctionAr
  // // }

  def resolveMethodDeclaration(
      input: MethodDefinition,
      scope: Scope
  ): ResolvedMethodDeclaration = {
    val retType = resolveType(input.returnType, scope)
    val parameters = resolveMethodArguments(input.arguments, scope)

    // Parameters may be referenced in method specifications
    val specScope = scope.declareVariables(parameters)

    val preconditions = ListBuffer[ResolvedExpression]()
    val postconditions = ListBuffer[ResolvedExpression]()
    for (spec <- input.specifications) {
      spec match {
        case requires: RequiresSpecification =>
          preconditions += resolveExpression(
            requires.value,
            specScope,
            SpecificationContext
          )
        case ensures: EnsuresSpecification =>
          postconditions += resolveExpression(
            ensures.value,
            specScope,
            PostConditionContext(retType)
          )

        // Continue checking values for resolving errors, even if invalid
        // TODO: Should invalid values also be type-checked?
        case invariant: LoopInvariantSpecification => {
          resolveExpression(invariant.value, specScope, SpecificationContext)
          scope.errors.error(invariant, "Invalid loop_invariant")
        }
        case assert: AssertSpecification => {
          resolveExpression(assert.value, specScope, SpecificationContext)
          scope.errors.error(assert, "Invalid assert")
        }
        case fold: FoldSpecification => {
          fold.arguments.foreach(
            resolveExpression(_, specScope, SpecificationContext)
          )
          scope.errors.error(fold, "Invalid fold")
        }
        case unfold: UnfoldSpecification => {
          unfold.arguments.foreach(
            resolveExpression(_, specScope, SpecificationContext)
          )
          scope.errors.error(unfold, "Invalid unfold")
        }
      }
    }
    ResolvedMethodDeclaration(
      parsed = input,
      name = input.id.name,
      returnType = retType,
      arguments = parameters,
      precondition = combineBooleans(preconditions.toSeq),
      postcondition = combineBooleans(postconditions.toSeq)
    )
  }

  def resolveMethodDefinition(
      input: MethodDefinition,
      localDecl: ResolvedMethodDeclaration,
      scope: Scope
  ): ResolvedMethodDefinition = {
    // Add method parameters to variable scope
    val methodScope = scope.declareVariables(localDecl.arguments)

    val block = input.body.get
    val resolvedBlock = resolveBlock(
      parsed = block,
      scope = methodScope,
      body = block.body,
      specifications = block.specifications,
      trailingSpecifications = block.trailingSpecifications
    )

    ResolvedMethodDefinition(input, localDecl, resolvedBlock)
  }

  def resolvePredicateDeclaration(
      input: PredicateDefinition,
      scope: Scope
  ): ResolvedPredicateDeclaration = {
    val parameters = resolveMethodArguments(input.arguments, scope)
    ResolvedPredicateDeclaration(
      parsed = input,
      name = input.id.name,
      arguments = parameters
    )
  }

  def resolvePredicateDefinition(
      input: PredicateDefinition,
      localDecl: ResolvedPredicateDeclaration,
      scope: Scope
  ): ResolvedPredicateDefinition = {
    val predicateScope = scope.declareVariables(localDecl.arguments)
    val body =
      resolveExpression(input.body.get, predicateScope, SpecificationContext)
    ResolvedPredicateDefinition(input, localDecl, body)
  }

  def resolveLibraryPath(
      lib: String,
      searchPaths: List[String]
  ): Option[Path] = {
    searchPaths.map(Paths.get(_).resolve(lib + ".h0"))
      .find(Files.exists(_))
  }

  def resolvePyTealProgram(
      seqs: Seq[Ast.stmt],
      errors: ErrorSink
  ): ResolvedProgram = {
    val scope = Scope(
      variables = Map.empty,
      methodDeclarations = Map.empty,
      methodDefinitions = Map.empty,
      // functionDefinitions = Map.empty,
      // functionDeclarations = Map.empty,
      predicateDeclarations = Map.empty,
      predicateDefinitions = Map.empty,
      structDefinitions = Map.empty,
      typeDefs = Map.empty,
      libraries = Map.empty,
      errors
    )

    val (_, program) = resolvePyTealProgram(seqs, scope)
    program
  }

  def convertSpan(span: Ast.Span.SourceSpan): SourceSpan = {
    val start = SourcePosition(
      line = span.start.line, 
      column = span.start.column, 
      index = span.start.index
    )

    val end = SourcePosition(
      line = span.end.line, 
      column = span.end.column, 
      index = span.end.index
    )

    SourceSpan(start, end)
  }

  val opBinMap: Map[String, BinaryOperator.BinaryOperator] = Map(
    "And"                 -> BinaryOperator.LogicalAnd,
    "Or"                  -> BinaryOperator.LogicalOr,
    "Eq"                  -> BinaryOperator.Equal,
    "NotEq"               -> BinaryOperator.NotEqual,
    "Lt"                  -> BinaryOperator.Less,
    "LtE"                 -> BinaryOperator.LessEqual,
    "Gt"                  -> BinaryOperator.Greater,
    "GtE"                 -> BinaryOperator.GreaterEqual,
    "Add"                 -> BinaryOperator.Add,
    "Sub"                 -> BinaryOperator.Subtract,
    "Mult"                -> BinaryOperator.Multiply,
    "Div"                 -> BinaryOperator.Divide,
    "Mod"                 -> BinaryOperator.Modulus,
    "BitOr"               -> BinaryOperator.BitwiseOr,
    "BitXor"              -> BinaryOperator.BitwiseXor,
    "BitAnd"              -> BinaryOperator.BitwiseAnd,
    "PyTealLt"            -> BinaryOperator.Less,
    "PyTealGt"            -> BinaryOperator.Greater,
    "PyTealGt2"           -> BinaryOperator.Greater,
    "PyTealLe"            -> BinaryOperator.LessEqual,
    "PyTealGe"            -> BinaryOperator.GreaterEqual,
    "PyTealAdd"           -> BinaryOperator.Add,
    "PyTealMinus"         -> BinaryOperator.Subtract,
    "PyTealMinus2"        -> BinaryOperator.Subtract,
    "PyTealMul"           -> BinaryOperator.Multiply,
    "PyTealDiv"           -> BinaryOperator.Divide,
    "PyTealMod"           -> BinaryOperator.Modulus,
    //"PyTealExp"           -> BinaryOperator.
    "PyTealEq"            -> BinaryOperator.Equal,
    "PyTealNeq"           -> BinaryOperator.NotEqual,
    "PyTealAnd"           -> BinaryOperator.LogicalAnd,
    "PyTealOr"            -> BinaryOperator.LogicalOr,
    "PyTealBitwiseAnd"    -> BinaryOperator.BitwiseAnd,
    "PyTealBitwiseOr"     -> BinaryOperator.BitwiseOr,
    "PyTealBitwiseXor"    -> BinaryOperator.BitwiseXor

    // "PyTealBytesLt"
    // "PyTealBytesGt"
    // "PyTealBytesLe"
    // "PyTealBytesGe"
    // "PyTealBytesAdd"
    // "PyTealBytesMinus"
    // "PyTealBytesMul"
    // "PyTealBytesDiv"
    // "PyTealBytesMod"
    // "PyTealBytesEq"
    // "PyTealBytesNeq"
    // "PyTealBytesAnd"
    // "PyTealBytesOr"
    // "PyTealBytesXor"

    // "PyTealBytesNot"
    // "PyTealBytesZero"
  )

  val opUnaryMap: Map[String, UnaryOperator.UnaryOperator] = Map(
    "Not"                -> UnaryOperator.Not,
    "PyTealNot"          -> UnaryOperator.Not,
    "PyTealBitwiseNot"   -> UnaryOperator.BitwiseNot
  )

  def stringToBoolean(s: String): Boolean = {
    s.toLowerCase match {
      case "true" => true
      case "false" => false
      case _ => throw new IllegalArgumentException(s"$s cannot be converted to Boolean.")
    }
  }

  def randomName: String = s"random_${Random.nextInt(10000)}"

  def combineExpressions(expressions: Seq[Expression], op: BinaryOperator.Value): Expression = expressions match {
    case Seq() => throw new IllegalArgumentException("No expressions to combine.")
    case Seq(head) => head
    case Seq(head, second, _*) => BinaryExpression(head, op, combineExpressions(expressions.tail, op), null)
  }

  def resolvePyTealProgram(
      seqs: Seq[Ast.stmt],
      initialScope: Scope
  ): (Scope, ResolvedProgram) = {
    val methodDeclarations = ListBuffer[ResolvedMethodDeclaration]()
    val methodDefinitions = ListBuffer[ResolvedMethodDefinition]()
    // val functionDefinitions = ListBuffer[ResolvedFunctionDefinition]()
    // val functionDeclarations = ListBuffer[ResolvedFunctionDeclaration]()
    val predicateDeclarations = ListBuffer[ResolvedPredicateDeclaration]()
    val predicateDefinitions = ListBuffer[ResolvedPredicateDefinition]()
    val structDefinitions = ListBuffer[ResolvedStructDefinition]()
    val types = ListBuffer[ResolvedTypeDef]()
    val dependencies = ListBuffer[ResolvedUseDeclaration]()
    // val dependencies = ListBuffer[ResolvedImportDeclaration]()
    // val simpleDependencies = ListBuffer[ResolvedImportSimpleDeclaration]()
    // val compoundDependencies = ListBuffer[ResolvedImportCompoundDeclaration]()
    var scope = initialScope

    for (seq <- seqs) {
      seq match {
        case i: Ast.stmt.ImportFrom => 

        case p: Ast.stmt.PyTealFunctionDef => {
          val name = p.name.name
          val args = p.args
          val body = p.body
          var blocks: Array[Statement] = Array()
          var argsList: List[(String, Type)] = Nil
          var specList: List[Specification] = Nil
          val arguments = args match {
            case Some(x) => x.args
            case None => Seq()
          }

          for (arg <- arguments) {
            argsList = argsList :+ (arg._1.name, if (arg._2 == "Uint64") namedType("int") else namedType("int"))
          }

          argsList = argsList :+ ("global", PointerType(NamedStructType(Identifier("Global", null), null), null))

          for (stmt <- body) {
            stmt match {
              case s: Ast.stmt.Spec => {
                s.specification match {
                  case requires: Ast.stmt.Specification.RequiresSpecification => {
                    val req = requires.value
                    req match {
                      case name: Ast.expr.Name =>
                          val raw = name.id.name
                          val value = stringToBoolean(raw)
                          val boolExpr = BooleanExpression(raw, value, null)
                          specList = specList :+ RequiresSpecification(boolExpr, null)
                      case id: Ast.identifier =>
                          val varExpr = VariableExpression(Identifier(id.name, null), null)
                          specList = specList :+ RequiresSpecification(varExpr, null)
                      case comp: Ast.expr.Compare =>
                          val compareExpr = resolveCompare(comp)
                          specList = specList :+ RequiresSpecification(compareExpr, null)
                      case boolOp: Ast.expr.BoolOp =>
                          val boolOpExpr = resolveBoolOp(boolOp)
                          specList = specList :+ RequiresSpecification(boolOpExpr, null)
                      case imprecise: Ast.expr.ImprecisionExpression =>
                          val imExpr = ImprecisionExpression(convertSpan(imprecise.span))
                          specList = specList :+ RequiresSpecification(imExpr, null)

                      // TODO: case ifExp: Ast.expr.IfExp =>
                      //   val ternaryExpr = resolveIfExp(ifExp)
                      //   specList = specList :+ RequiresSpecification(ternaryExpr, null)
                    }
                  }

                  case ensures: Ast.stmt.Specification.EnsuresSpecification => {
                    val en = ensures.value
                    en match {
                      case name: Ast.expr.Name =>
                          val raw = name.id.name
                          val value = stringToBoolean(raw)
                          val boolExpr = BooleanExpression(raw, value, null)
                          specList = specList :+ EnsuresSpecification(boolExpr, null)
                      case id: Ast.identifier =>
                          val varExpr = VariableExpression(Identifier(id.name, null), null)
                          specList = specList :+ EnsuresSpecification(varExpr, null)
                      case comp: Ast.expr.Compare =>
                          val compareExpr = resolveCompare(comp)
                          specList = specList :+ EnsuresSpecification(compareExpr, null)
                      case boolOp: Ast.expr.BoolOp =>
                          val boolOpExpr = resolveBoolOp(boolOp)
                          specList = specList :+ EnsuresSpecification(boolOpExpr, null)
                      case imprecise: Ast.expr.ImprecisionExpression =>
                          val imExpr = ImprecisionExpression(convertSpan(imprecise.span))
                          specList = specList :+ EnsuresSpecification(imExpr, null)

                      // case ifExp: Ast.expr.IfExp =>
                      //   val ternaryExpr = resolveIfExp(ifExp)
                      //   specList = specList :+ RequiresSpecification(ternaryExpr, null)
                    }
                  }

                  case asserts: Ast.stmt.Specification.AssertSpecification => {
                    val a = asserts.value
                    a match {
                      case name: Ast.expr.Name =>
                          val raw = name.id.name
                          val value = stringToBoolean(raw)
                          val boolExpr = BooleanExpression(raw, value, null)
                          specList = specList :+ AssertSpecification(boolExpr, null)
                      case id: Ast.identifier =>
                          val varExpr = VariableExpression(Identifier(id.name, null), null)
                          specList = specList :+ AssertSpecification(varExpr, null)
                      case comp: Ast.expr.Compare =>
                          val compareExpr = resolveCompare(comp)
                          specList = specList :+ AssertSpecification(compareExpr, null)
                      case boolOp: Ast.expr.BoolOp =>
                          val boolOpExpr = resolveBoolOp(boolOp)
                          specList = specList :+ AssertSpecification(boolOpExpr, null)
                      case imprecise: Ast.expr.ImprecisionExpression =>
                          val imExpr = ImprecisionExpression(convertSpan(imprecise.span))
                          specList = specList :+ AssertSpecification(imExpr, null)

                      // case ifExp: Ast.expr.IfExp =>
                      //   val ternaryExpr = resolveIfExp(ifExp)
                      //   specList = specList :+ RequiresSpecification(ternaryExpr, null)
                    }
                  }

                  case loop: Ast.stmt.Specification.LoopInvariantSpecification => {
                    val l = loop.value
                    l match {
                      case name: Ast.expr.Name =>
                          val raw = name.id.name
                          val value = stringToBoolean(raw)
                          val boolExpr = BooleanExpression(raw, value, null)
                          specList = specList :+ LoopInvariantSpecification(boolExpr, null)
                      case id: Ast.identifier =>
                          val varExpr = VariableExpression(Identifier(id.name, null), null)
                          specList = specList :+ LoopInvariantSpecification(varExpr, null)
                      case comp: Ast.expr.Compare =>
                          val compareExpr = resolveCompare(comp)
                          specList = specList :+ LoopInvariantSpecification(compareExpr, null)
                      case boolOp: Ast.expr.BoolOp =>
                          val boolOpExpr = resolveBoolOp(boolOp)
                          specList = specList :+ LoopInvariantSpecification(boolOpExpr, null)
                      case imprecise: Ast.expr.ImprecisionExpression =>
                          val imExpr = ImprecisionExpression(convertSpan(imprecise.span))
                          specList = specList :+ LoopInvariantSpecification(imExpr, null)

                      // case ifExp: Ast.expr.IfExp =>
                      //   val ternaryExpr = resolveIfExp(ifExp)
                      //   specList = specList :+ RequiresSpecification(ternaryExpr, null)
                    }
                  }

                  // TODO: fold, unfold
                }
              }
              case r: Ast.stmt.Return => {
                r.value.get match {
                  case name: Ast.expr.Name => 
                    for (valu <- r.value) {
                      var valuName = valu.asInstanceOf[Ast.expr.Name]
                      blocks = blocks :+ ReturnStatement(Some(varRef(valuName.id.name)), null)
                    }
                  
                  case seq: Ast.expr.PyTealSeq =>
                      for (expression <- seq.values.init) {
                        expression match {
                          case ss: Ast.expr.ScratchStore => {
                            ss.expr match {
                              case right: Ast.expr.GlobalGet => {
                                right.key match {
                                  case bytes : Ast.expr.PyTealBytesStored => {
                                    val rightexpr = MemberExpression(VariableExpression(Identifier("global", null), null), Identifier(bytes.key.toString, null), true, null)
                                    blocks = blocks :+ AssignmentStatement(VariableExpression(Identifier(ss.name.name, null), null), AssignOperator.Assign, rightexpr, null)
                                  }
                                }
                                
                              }
                            }
                          }
                        }
                      }
                      seq.values.last match {
                        case globalPut: Ast.expr.GlobalPut => {
                          var left:Expression = null
                          var right:Expression = null
                          var returnExpr:Expression = null
                          globalPut.key match {
                            case bytes: Ast.expr.PyTealBytesStored => {
                              left = MemberExpression(VariableExpression(Identifier("global", null), null), Identifier(bytes.key.toString, null), true, null)
                              returnExpr = MemberExpression(VariableExpression(Identifier("global", null), null), Identifier(bytes.key.toString, null), true, null)
                            }
                          }
                          globalPut.value match {
                            case pytealexpr: Ast.expr.PyTealExpr => {
                              var expr1:Expression = null
                              var expr2:Expression = null
                              pytealexpr.expr1 match {
                                case sl: Ast.expr.ScratchLoad => {
                                  expr1 = VariableExpression(Identifier(sl.name.name, null), null)
                                }
                              }
                              pytealexpr.expr2 match {
                                case g: Ast.expr.Get => {
                                  expr2 = VariableExpression(Identifier(g.name.name, null), null)
                                }
                              }
                              pytealexpr.op match {
                                case PyTealAdd => {
                                  right = BinaryExpression(expr1, BinaryOperator.Add, expr2, null)
                                }
                                case PyTealMinus => {
                                  right = BinaryExpression(expr1, BinaryOperator.Subtract, expr2, null)
                                }
                                case PyTealBytesMul => {
                                  right = BinaryExpression(expr1, BinaryOperator.Multiply, expr2, null)
                                }
                                case PyTealBytesDiv => {
                                  right = BinaryExpression(expr1, BinaryOperator.Divide, expr2, null)
                                }
                              }
                            }
                          }
                          blocks = blocks :+ AssignmentStatement(left, AssignOperator.Assign, right, null)
                          blocks = blocks :+ ReturnStatement(Some(returnExpr), null)
                        }
                      }
                
                  case setValue: Ast.expr.SetValue => {
                    var returnExpr:Expression = null 
                    setValue.expr match {
                      case right: Ast.expr.GlobalGet => {
                        right.key match {
                          case bytes : Ast.expr.PyTealBytesStored => {
                            val rightexpr = MemberExpression(VariableExpression(Identifier("global", null), null), Identifier(bytes.key.toString, null), true, null)
                            blocks = blocks :+ AssignmentStatement(VariableExpression(Identifier(setValue.name.name, null), null), AssignOperator.Assign, rightexpr, null)
                            returnExpr = VariableExpression(Identifier(setValue.name.name, null), null)
                          }
                        }
                      } 
                    }
                    blocks = blocks :+ ReturnStatement(Some(returnExpr), null)
                  }
                } 
              }
              case sv: Ast.stmt.ScratchVar => {
                if (sv.teal_type == Ast.tealtype.Uint64) {
                  blocks = blocks :+ varDef(sv.name.name, namedType("int"), Some(intVal(0)))
                }
              }
              case a: Ast.stmt.Assign => {
                var names: Array[String] = Array()
                var values: Array[Expression] = Array() 
                for (target <- a.targets) {
                  var assignName = target.asInstanceOf[Ast.expr.Name]
                  names = names :+ assignName.id.name
                }
                
                val value = a.value

                // int_15 = Int(15)
                // deal with Assign(List(Name(identifier(int_15), Load), 
                // Call(Name(Identifier(Int), Load), ArrayBuffer(Num(15)), ArrayBuffer(), None, None)))
                value match {
                  case call: Ast.expr.Call => {
                    for (valu <- call.args) {
                      val num = valu.asInstanceOf[Ast.expr.Num]
                      values = values :+ intVal(num.n.toString.toInt)
                    }
                    names.zipWithIndex.foreach{ case (name, index) => {
                        blocks = blocks :+ varDef(name, namedType("int"), Some(values(index)))
                      } 
                    }
                  } 
                  
                  case name: Ast.expr.Name => {
                    values = values :+ VariableExpression(Identifier(name.id.name, null), null)
                    names.zipWithIndex.foreach{ case (name, index) => {
                      blocks = blocks :+ varDef(name, namedType("int"), Some(values(index)))
                      }
                    }
                  }

                  // add_expr = Add(Int(1), Int(2), Int(356))
                  // deal with Assign(List(Name(identifier(add_expr),Load)),
                  // PyTealBinOp(PyTealAdd,ArrayBuffer(Num(1), Num(2), Num(356))))
                  case binOp: Ast.expr.PyTealBinOp => {
                    var argNames: Array[Expression] = Array()
                    val op = binOp.op.toString
                    for (valu <- binOp.values) {
                      valu match {
                        case num: Ast.expr.Num =>
                          val argName = randomName
                          blocks = blocks :+ varDef(argName, namedType("int"), Some(intVal(num.n.toString.toInt)))
                          argNames = argNames :+ VariableExpression(Identifier(argName, null), null)
                        case id: Ast.expr.Name =>
                          argNames = argNames :+ VariableExpression(Identifier(id.id.name, null), null)
                        case id: Ast.identifier =>
                          argNames = argNames :+ VariableExpression(Identifier(id.name, null), null)
                      }
                    }
                    val combinedExpr = combineExpressions(argNames, opBinMap.getOrElse(op, BinaryOperator.Add))
                    for (name <- names) {
                      blocks = blocks :+ varDef(name, namedType("int"), Some(combinedExpr))
                    }
                  }

                  // Assign(List(Name(identifier(int_0),Load)),Compare(Name(identifier(div_8),Load),ArrayBuffer(GtE),ArrayBuffer(Name(identifier(int_55),Load))))
                  case comp: Ast.expr.Compare => {
                    var compArgs: Array[Expression] = Array() // changed from val to var and rename comArgs to compArgs
                    for (compValu <- comp.comparators) {
                      compValu match {
                        case num: Ast.expr.Num =>
                          compArgs = compArgs :+ intVal(num.n.toString.toInt) 
                        case id: Ast.expr.Name =>
                          compArgs = compArgs :+ VariableExpression(Identifier(id.id.name, null), null) 
                        case id: Ast.identifier =>
                          compArgs = compArgs :+ VariableExpression(Identifier(id.name, null), null) 
                        case binOp: Ast.expr.PyTealBinOp => 
                          var binOpArgs: Array[Expression] = Array()
                          val op = binOp.op.toString
                          for (valu <- binOp.values) {
                              valu match {
                                  case num: Ast.expr.Num =>
                                      binOpArgs = binOpArgs :+ intVal(num.n.toString.toInt)
                                  case id: Ast.expr.Name =>
                                      binOpArgs = binOpArgs :+ VariableExpression(Identifier(id.id.name, null), null)
                                  case id: Ast.identifier =>
                                      binOpArgs = binOpArgs :+ VariableExpression(Identifier(id.name, null), null)
                              }
                          }
                          val combinedBinOpExpr = combineExpressions(binOpArgs, opBinMap.getOrElse(op, BinaryOperator.Add))
                          compArgs = compArgs :+ combinedBinOpExpr
                        case call: Ast.expr.Call =>
                          for (valu <- call.args) {
                            valu match {
                              case num: Ast.expr.Num =>
                                compArgs = compArgs :+ intVal(num.n.toString.toInt)
                              case id: Ast.expr.Name =>
                                compArgs = compArgs :+ VariableExpression(Identifier(id.id.name, null), null)
                              case id: Ast.identifier =>
                                compArgs = compArgs :+ VariableExpression(Identifier(id.name, null), null)
                            }
                          }
                      }
                    }
                    val compareOps = comp.ops.map(op => opBinMap.getOrElse(op.toString, BinaryOperator.Equal)) // changed BinaryOperator.And to BinaryOperator.Equal
                    val leftExpr = comp.left match {
                      case num: Ast.expr.Num => intVal(num.n.toString.toInt)
                      case id: Ast.expr.Name => VariableExpression(Identifier(id.id.name, null), null)
                      case id: Ast.identifier => VariableExpression(Identifier(id.name, null), null)
                    }
                    // Assuming only one comparator and one operator for simplicity
                    val compareExpr = BinaryExpression(leftExpr, compareOps(0), compArgs(0), null)
                    names.zipWithIndex.foreach{ case (name, index) => {
                      blocks = blocks :+ varDef(name, namedType("bool"), Some(compareExpr))
                    }}
                  }
                }
                
                // bytes_expr = BytesGt(Bytes("base16", "0xFF"), Bytes("base16", "0xFE"))
                // Assign(List(Name(identifier(bytes_expr),Load)),PyTealBinOp(PyTealBytesGt,ArrayBuffer(PyTealBytes(base16,0xFF), PyTealBytes(base16,0xFE)))), Return(Some(Name(identifier(add_expr),Load)))),ArrayBuffer())
              }
            }
          }

          // println(blocks.mkString(" "))

          val m = methodDef(
            name,
            namedType("int"),
            argsList,
            Some(
              block(blocks: _*)
            ),
            specList
          )
          // val  body = m.body
          // for (stmt <- body) {
          //   println(stmt)
          // }

          val decl = resolveMethodDeclaration(m, scope)
          //println("resolve method declaration = " + decl)
          methodDeclarations += decl
          scope = scope.declareMethod(decl)

          if (m.body.isDefined) {
            val definition = resolveMethodDefinition(m, decl, scope)
            methodDefinitions += definition
            scope = scope.defineMethod(definition)
          }
        }
        case s: Ast.stmt.Spec => {
            s.specification match {
              case global: Ast.stmt.Specification.GlobalDeclaration => {
                var memberDefinitions = List[MemberDefinition]()
                for (arg <- global.args) {
                  memberDefinitions =  MemberDefinition(Identifier(arg.name, null), NamedType(Identifier("int", null), null), null) :: memberDefinitions
                }
                val definition = resolveStructDefinition(StructDefinition(Identifier("Global", null), Some(memberDefinitions), null), scope)
                structDefinitions += definition
                scope = scope.defineStruct(definition)
              }
            }
        }
        case default => {

        }
      }
    }

    (scope,
     ResolvedProgram(
       methodDeclarations = methodDeclarations.toList,
       methodDefinitions = methodDefinitions.toList,
       predicateDeclarations = predicateDeclarations.toList,
       predicateDefinitions = predicateDefinitions.toList,
       structDefinitions = structDefinitions.toList,
       types = types.toList,
       dependencies = dependencies.toList
      //  simpleDependencies = simpleDependencies.toList
      //  compoundDependencies = compoundDependencies.toList
    ))
    

  }

   def methodDef(
      name: String,
      retType: Type,
      arguments: List[(String, Type)],
      body: Option[BlockStatement] = None,
      specifications: List[Specification]
  ) =
    MethodDefinition(
      id = Identifier(name, null),
      returnType = retType,
      arguments = arguments.map { case (name, typ) =>
        MemberDefinition(Identifier(name, null), typ, null)
      },
      body = body,
      specifications = specifications,
      span = null
    )

    def namedType(name: String): Type = NamedType(Identifier(name, null), null)
    def varDef(name: String, typ: Type, value: Option[Expression]) =
      VariableStatement(typ, Identifier(name, null), value, null)

    def varRef(name: String) = VariableExpression(Identifier(name, null), null)
    
    def block(body: Statement*) =
      BlockStatement(body.toList, null, List(), List())
    
    def intVal(value: Int) = IntegerExpression(value.toString(), value, null)

    def resolveBoolOp(boolOp: Ast.expr.BoolOp): Expression = {
      val op = boolOp.op.toString
      val expressions = boolOp.values.map {
        case imprecise: Ast.expr.ImprecisionExpression =>
          ImprecisionExpression(convertSpan(imprecise.span))
        case comp: Ast.expr.Compare =>
          resolveCompare(comp)
        case nestedBoolOp: Ast.expr.BoolOp =>
          resolveBoolOp(nestedBoolOp)
      }
      combineExpressions(expressions, opBinMap.getOrElse(op, BinaryOperator.Add))
    }

    def resolveCompare(comp: Ast.expr.Compare): Expression = {
      var compArgs: Array[Expression] = Array()
      for (compValu <- comp.comparators) {
        compValu match {
          case num: Ast.expr.Num =>
            compArgs = compArgs :+ intVal(num.n.toString.toInt) 
          case id: Ast.expr.Name =>
            compArgs = compArgs :+ VariableExpression(Identifier(id.id.name, null), null) 
          case id: Ast.identifier =>
            compArgs = compArgs :+ VariableExpression(Identifier(id.name, null), null) 
          case binOp: Ast.expr.PyTealBinOp => 
            var binOpArgs: Array[Expression] = Array()
            val op = binOp.op.toString
            for (valu <- binOp.values) {
                valu match {
                    case num: Ast.expr.Num =>
                        binOpArgs = binOpArgs :+ intVal(num.n.toString.toInt)
                    case id: Ast.expr.Name =>
                        binOpArgs = binOpArgs :+ VariableExpression(Identifier(id.id.name, null), null)
                    case id: Ast.identifier =>
                        binOpArgs = binOpArgs :+ VariableExpression(Identifier(id.name, null), null)
                }
            }
            val combinedBinOpExpr = combineExpressions(binOpArgs, opBinMap.getOrElse(op, BinaryOperator.Add))
            compArgs = compArgs :+ combinedBinOpExpr
          case call: Ast.expr.Call =>
            for (valu <- call.args) {
              valu match {
                case num: Ast.expr.Num =>
                  compArgs = compArgs :+ intVal(num.n.toString.toInt)
                case id: Ast.expr.Name =>
                  compArgs = compArgs :+ VariableExpression(Identifier(id.id.name, null), null)
                case id: Ast.identifier =>
                  compArgs = compArgs :+ VariableExpression(Identifier(id.name, null), null)
              }
            }
        }
      }
      
      val compareOps = comp.ops.map(op => opBinMap.getOrElse(op.toString, BinaryOperator.Equal))
      
      val leftExpr = comp.left match {
        case result: Ast.expr.ResultExpression => ResultExpression(convertSpan(result.span))
        case num: Ast.expr.Num => intVal(num.n.toString.toInt)
        case id: Ast.expr.Name => VariableExpression(Identifier(id.id.name, null), null)
        case id: Ast.identifier => VariableExpression(Identifier(id.name, null), null)
      }
      
      BinaryExpression(leftExpr, compareOps(0), compArgs(0), null)
    }

    // def resolveIfExp(ifExp: Ast.expr.IfExp): Node = {
    //   val conditionNode = resolveExpression(ifExp.test)
    //   val ifTrueNode = resolveExpression(ifExp.body)
    //   val ifFalseNode = resolveExpression(ifExp.orelse)

    //   (conditionNode, ifTrueNode, ifFalseNode) match {
    //     case (condition: Expression, ifTrue: Expression, ifFalse: Expression) =>
    //       TernaryExpression(condition, ifTrue, ifFalse, null)
    //     case _ => 
    //       throw new RuntimeException("Not all elements of IfExp could be resolved to Expressions")
    //   }
    // }

    // def resolveExpression(expr: Ast.expr): Node = expr match {
    //   case num: Ast.expr.Num => 
    //     intVal(num.n.toString.toInt) 
    //   case name: Ast.expr.Name => 
    //     VariableExpression(Identifier(name.id.name, null), null) 
    //   case id: Ast.identifier => 
    //     VariableExpression(Identifier(id.name, null), null) 
    //   case comp: Ast.expr.Compare => 
    //     resolveCompare(comp)
    //   case boolOp: Ast.expr.BoolOp => 
    //     resolveBoolOp(boolOp)
    //   case call: Ast.expr.Call => 
    //     resolveCall(call)
    // }

    // def resolveBoolOp(expr: Ast.expr): Node = {
      
    // }

  // def resolveProgram(
  //     defs: List[Definition],
  //     librarySearchPaths: List[String],
  //     errors: ErrorSink
  // ): ResolvedProgram = {
  //   val scope = Scope(
  //     variables = Map.empty,
  //     methodDeclarations = Map.empty,
  //     methodDefinitions = Map.empty,
  //     // functionDefinitions = Map.empty,
  //     // functionDeclarations = Map.empty,
  //     predicateDeclarations = Map.empty,
  //     predicateDefinitions = Map.empty,
  //     structDefinitions = Map.empty,
  //     typeDefs = Map.empty,
  //     libraries = Map.empty,
  //     errors
  //   )

  //   val (_, program) = resolveProgram(defs, librarySearchPaths, scope)
  //   program
  // }

  // def resolveProgram(
  //     program: List[Definition],
  //     librarySearchPaths: List[String],
  //     initialScope: Scope
  // ): (Scope, ResolvedProgram) = {
  //   val methodDeclarations = ListBuffer[ResolvedMethodDeclaration]()
  //   val methodDefinitions = ListBuffer[ResolvedMethodDefinition]()
  //   // val functionDefinitions = ListBuffer[ResolvedFunctionDefinition]()
  //   // val functionDeclarations = ListBuffer[ResolvedFunctionDeclaration]()
  //   val predicateDeclarations = ListBuffer[ResolvedPredicateDeclaration]()
  //   val predicateDefinitions = ListBuffer[ResolvedPredicateDefinition]()
  //   val structDefinitions = ListBuffer[ResolvedStructDefinition]()
  //   val types = ListBuffer[ResolvedTypeDef]()
  //   // val dependencies = ListBuffer[ResolvedUseDeclaration]()
  //   val dependencies = ListBuffer[ResolvedImportDeclaration]()
  //   var scope = initialScope

  //   for (definition <- program) {
  //     definition match {
  //       case u: SimpleImportDeclaration => {
  //         val library =
  //           // PyTEAL: hardcoded to true isLib check
  //           if (true) Some(u.path.value)
  //           else {
  //             scope.errors.error(u, "Local imports are not implemented")
  //             None
  //           }

  //         val path = library.flatMap(lib => {
  //           resolveLibraryPath(lib, librarySearchPaths)
  //           // .orElse({
  //             // scope.errors.error(u, s"Unable to find library ${u.path.raw}")
  //             // None
  //           // })
  //         })

  //         val resolved = path.flatMap(path =>
  //           scope.libraries.get(path).orElse {
  //             val source =
  //               try {
  //                 Some(Files.readString(path))
  //               } catch {
  //                 case e: IOException =>
  //                   scope.errors.error(u, s"Could not read file '$path'")
  //                   None
  //               }

  //             val parsed = source.flatMap(source => {
  //               Parser.parseProgram(source) match {
  //                 case Success(value, _) => Some(value)
  //                 case fail: Failure =>
  //                   val error = fail.trace().longAggregateMsg
  //                   scope.errors
  //                     .error(u, s"Parsing error while parsing '$path':\n$error")
  //                   None
  //               }
  //             })

  //             parsed.map(parsed => {
  //               val (s, p) = resolveProgram(parsed, librarySearchPaths, scope)
  //               scope = s.declareLibrary(path, p)
  //               p
  //             })
  //         })

  //         dependencies += ResolvedImportDeclaration(u,
  //                                                u.path.value,
  //                                                path,
  //                                                resolved)
  //       }

  //       case t: TypeDefinition => {
  //         val typeDef = resolveTypeDef(t, scope)
  //         types += typeDef
  //         scope = scope.defineType(typeDef)
  //       }

  //       case s: StructDefinition => {
  //         if (s.fields.isDefined) {
  //           val definition = resolveStructDefinition(s, scope)
  //           structDefinitions += definition
  //           scope = scope.defineStruct(definition)
  //         }
  //       }

  //       case m: MethodDefinition => {
  //         val decl = resolveMethodDeclaration(m, scope)
  //         methodDeclarations += decl
  //         scope = scope.declareMethod(decl)

  //         if (m.body.isDefined) {
  //           val definition = resolveMethodDefinition(m, decl, scope)
  //           methodDefinitions += definition
  //           scope = scope.defineMethod(definition)
  //         }
  //       }

  //       // case f: FunctionDefinition => {
  //       //   val decl = resolveFunctionDeclaration(f, scope)
  //       //   functionDeclarations += decl
  //       //   scope = scope.declareFunction(decl)

  //       //   if(f.body.isDefined) {
  //       //     val definition = resolveFunctionDefinition(f, decl, scope)
  //       //     functionDefinitions += definition
  //       //     scope = scope.defineFunction(definition)
  //       //   }
  //       // }

  //       case p: PredicateDefinition => {
  //         val decl = resolvePredicateDeclaration(p, scope)
  //         predicateDeclarations += decl
  //         scope = scope.declarePredicate(decl)

  //         if (p.body.isDefined) {
  //           val definition = resolvePredicateDefinition(p, decl, scope)
  //           predicateDefinitions += definition
  //           scope = scope.definePredicate(definition)
  //         }
  //       }
  //     }
  //   }

  //   (scope,
  //    ResolvedProgram(
  //      methodDeclarations = methodDeclarations.toList,
  //      methodDefinitions = methodDefinitions.toList,
  //      predicateDeclarations = predicateDeclarations.toList,
  //      predicateDefinitions = predicateDefinitions.toList,
  //      structDefinitions = structDefinitions.toList,
  //      types = types.toList,
  //      dependencies = dependencies.toList
  //    ))
  // }
}
