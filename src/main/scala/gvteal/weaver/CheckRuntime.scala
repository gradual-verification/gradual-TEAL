package gvteal.weaver

import scala.io.Source
import fastparse.Parsed.{Failure, Success}
import gvteal.parser.Parser
import gvteal.analyzer.{ErrorSink, ResolvedProgram, Resolver}
import gvteal.transformer.{DependencyTransformer, IR}
object CheckRuntime {
  val name = "runtime"
  private lazy val header: ResolvedProgram = {
    val runtimeSource = Source.fromResource(name + ".h0").mkString
    val parsed = Parser.parseProgram(runtimeSource) match {
      case _: Failure =>
        throw new WeaverException("Cannot parse runtime header")
      case Success(value, _) => value
    }

    val errors = new ErrorSink()
    val resolved = Resolver.resolveProgram(parsed, List(), errors)
    if (errors.errors.nonEmpty)
      throw new WeaverException("Cannot resolve runtime header")

    resolved
  }

  def addToIR(program: IR.Program): CheckRuntime = {
    // TODO: What should be the second parameter for functions in compound?
    val simpleDependency = program.addSimpleDependency(name, "")
    val compoundDependency = program.addCompoundDependency(name, "")
    DependencyTransformer.transform(program, simpleDependency, header)
    DependencyTransformer.transform(program, compoundDependency, header)
    new CheckRuntime(program)
  }

  object Names {
    val ownedFieldsStruct = "OwnedFields"
    val fieldArray = "FieldArray"
    val primaryOwnedFields = "_ownedFields"
    val temporaryOwnedFields = "_tempFields"
    val contextOwnedFields = "_contextFields"
    val initOwnedFields = "initOwnedFields"
    val addStructAcc = "addStructAcc"
    val addAcc = "addAcc"
    val loseAcc = "loseAcc"
    val join = "join"
    val assertAcc = "assertAcc"
    val addAccEnsureSeparate = "addAccEnsureSeparate"
    val find = "find"
    val instanceCounter = "_instanceCounter"
    val id = "_id"
    val removePrefix = "remove_"
    val addPrefix = "add_"
    val checkPrefix = "check_"
  }
}

class CheckRuntime private (program: IR.Program) {
  import CheckRuntime.Names
  val ownedFields: IR.StructDefinition =
    program.struct(Names.ownedFieldsStruct)
  val ownedFieldsRef = new IR.ReferenceType(ownedFields)
  val ownedFieldInstanceCounter: IR.StructField =
    ownedFields.fields.find(_.name == "instanceCounter").get
  val initOwnedFields: IR.MethodDefinition =
    program.method(Names.initOwnedFields)
  val addStructAcc: IR.MethodDefinition =
    program.method(Names.addStructAcc)
  val addAcc: IR.MethodDefinition = program.method(Names.addAcc)
  val addAccEnsureSeparate: IR.MethodDefinition =
    program.method(Names.addAccEnsureSeparate)
  val loseAcc: IR.MethodDefinition = program.method(Names.loseAcc)
  val join: IR.MethodDefinition = program.method(Names.join)
  val assertAcc: IR.MethodDefinition = program.method(Names.assertAcc)
  val find: IR.MethodDefinition = program.method(Names.find)
}
