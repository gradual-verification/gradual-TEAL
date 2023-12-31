package gvteal.parser
import fastparse._

trait Definitions extends Statements with Types {
  def definition[_: P]: P[Seq[Definition]] =
    P(
      structDefinition.map(Seq(_)) |
      typeDefinition.map(Seq(_)) |
      methodDefinition.map(Seq(_)) |
      predicateAnnotation |
      // PyTEAL extension
      // functionDefinition.map(Seq(_)) |
      simpleImportDeclaration.map(Seq(_)) |
      compoundImportDeclaration.map(Seq(_))
    )

  def structDefinition[_: P]: P[StructDefinition] =
    P(span(kw("struct") ~ identifier ~ structFields.? ~ ";")).map({
      case ((id, fields), span) => StructDefinition(id, fields, span)
    })
  def structFields[_: P]: P[List[MemberDefinition]] =
    P("{" ~ structField.rep ~ "}").map(f => f.toList)
  def structField[_: P]: P[MemberDefinition] =
    P(typeReference ~ identifier ~ ";" ~~ pos).map({
      case (typ, id, end) => MemberDefinition(id, typ, SourceSpan(typ.span.start, end))
    })
  
  def typeDefinition[_: P]: P[TypeDefinition] =
    P(span(kw("typedef") ~ typeReference ~ identifier ~ ";")).map({
      case ((defType, id), span) => TypeDefinition(id, defType, span)
    })

  def methodDefinition[_: P]: P[MethodDefinition] =
    P(
      typeReference ~ identifier ~ "(" ~ methodParameter.rep(0, ",") ~ ")" ~
      annotations ~
      (P(";").map(_ => None) | blockStatement.map(Some(_))) ~~
      pos
    ).map({
      case (ret, id, args, annot, body, end) =>
        MethodDefinition(id, ret, args.toList, body, annot, SourceSpan(ret.span.start, end))
    })

  def methodParameter[_: P]: P[MemberDefinition] =
    P(typeReference ~ identifier).map({
      case (paramType, id) => MemberDefinition(id, paramType, SourceSpan(paramType.span.start, id.span.end))
    })

  sealed trait UsePath {
    val path: StringExpression
  }
  case class LibraryPath(path: StringExpression) extends UsePath
  case class LocalPath(path: StringExpression) extends UsePath
  def usePath[_: P]: P[UsePath] =
    P(useLibraryPath | useLocalPath)
  def useLibraryPath[_: P]: P[LibraryPath] =
    P(span(library.!)).map({
      case (raw, span) => LibraryPath(StringExpression(raw, raw.substring(1, raw.length() - 1), span))
    })
  def useLocalPath[_: P]: P[LocalPath] =
    P(stringExpression).map(LocalPath(_))

  def predicateAnnotation[_: P]: P[Seq[PredicateDefinition]] =
    P(singleLinePredicateAnnotation | multiLinePredicateAnnotation)

  def singleLinePredicateAnnotation[_: P]: P[Seq[PredicateDefinition]] =
    P("#@"./.flatMapX(_ => new Parser(state.inSingleLineAnnotation()).predicateDefinitions) ~~/ ("\n" | End))
  def multiLinePredicateAnnotation[_: P]: P[Seq[PredicateDefinition]] =
    P("\"\"\"@"./.flatMapX(_ => new Parser(state.inAnnotation()).predicateDefinitions) ~/ "@\"\"\"")

  def predicateDefinitions[_: P]: P[Seq[PredicateDefinition]] =
    P(space ~~ predicateDefinition.rep ~~ space)

  def predicateDefinition[_: P]: P[PredicateDefinition] =
    P(span("predicate" ~ identifier ~ "(" ~ methodParameter.rep(sep = ",") ~ ")" ~/ (predicateBody | emptyPredicateBody)))
    .map { case ((ident, args, body), span) => PredicateDefinition(ident, args.toList, body, span) }
  
  def emptyPredicateBody[_: P]: P[Option[Expression]] = P(";").map(_ => None)

  def predicateBody[_: P]: P[Option[Expression]] =
    P("=" ~/ expression ~/ ";").map(Some(_))

  /* ============ PyTEAL Extension ============ */

  // Library imports [cuts](https://com-lihaoyi.github.io/fastparse/#Cuts)
  def simpleImportDeclaration[_: P]: P[SimpleImportDeclaration] = 
    P((pos ~~ kw("import") ~~ " " ~/ libraryPath))
    .map({
      // TODO (cleanup): This can probably be made nicer with an `p as mkString` variable carried
      case(start, p) => SimpleImportDeclaration(
        p.path, SourceSpan(start, p.path.span.end))
    })

  def compoundImportDeclaration[_: P]: P[CompoundImportDeclaration] =
    P((pos ~~ kw("from") ~~ " " ~~ libraryPath.rep(sep = ",") ~~ " " ~~ kw("import") ~~ " " ~~ libraryPath.rep(sep = ",")))
    .map({
      case(start, p, f) => 
        val pString = p.map(_.path.value).mkString(",")
        val fString = f.map(_.path.value).mkString(",") 
        val pSpan = if (p.nonEmpty) Some(SourceSpan(p.head.path.span.start, p.last.path.span.end)) else None
        val fSpan = if (f.nonEmpty) Some(SourceSpan(f.head.path.span.start, f.last.path.span.end)) else None
        CompoundImportDeclaration(
          pSpan.map(span => StringExpression(pString, pString, span)).getOrElse(StringExpression(pString, pString, SourceSpan(start, start))),
          fSpan.map(span => StringExpression(fString, fString, span)).getOrElse(StringExpression(fString, fString, SourceSpan(start, start))),
          SourceSpan(start, fSpan.get.end)
        )
    })

  sealed trait ImportPath {
    val path: StringExpression
  }
  case class PyLibraryPath(path: StringExpression) extends ImportPath
  case class PyLocalPath(path: StringExpression) extends ImportPath

  def libraryPath[_: P]: P[ImportPath] =
    P(importLibraryPath | importLocalPath)

  def importLibraryPath[_: P]: P[PyLibraryPath] = 
    P(span(library.!)).map({
      case (raw, span) => PyLibraryPath(StringExpression(raw, raw, span))
    })
  def importLocalPath[_: P]: P[PyLocalPath] = 
    P(stringExpression).map(PyLocalPath(_))

  // Function signatures
  // def functionDefinition[_: P]: P[FunctionDefinition] =
  //   P(span(kw("def") ~ identifier ~ "(" ~ functionParameter.rep(0, ",") ~ "):" ~ annotations ~ pyBlockStatement(Some(_))) ~~ pos)
  //   .map({
  //     case (id, args, annot, body, end) => 
  //       FunctionDefinition(id, args.toList, body, annot, SourceSpan(id.span.start, end))
  //   })
  
  // def functionParameter[_: P]: P[FunctionDefinition] = 
  //   P(identifier).map({
  //     case(id) => FunctionDefinition(id, SourceSpan(id.span.start, id.span.end))
  //   })
}