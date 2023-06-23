package gvteal.analyzer

// import gvteal.parser.UseDeclaration
import gvteal.parser.SimpleImportDeclaration
import gvteal.parser.CompoundImportDeclaration
import java.nio.file.Path

// case class ResolvedUseDeclaration(
//     parsed: UseDeclaration,
//     name: String,
//     isLibrary: Boolean,
//     path: Option[Path],
//     dependency: Option[ResolvedProgram]
// ) extends ResolvedNode

// TODO: Simple and Compound declaration difference 
case class ResolvedImportSimpleDeclaration(
        parsed: SimpleImportDeclaration,
        name: String,
        path: Option[Path],
        dependency: Option[ResolvedProgram]
) extends ResolvedNode

case class ResolvedImportCompoundDeclaration(
        parsed: CompoundImportDeclaration,
        name: String,
        functions: String,
        path: Option[Path],
        dependency: Option[ResolvedProgram]
) extends ResolvedNode