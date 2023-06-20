package gvteal.analyzer

// import gvteal.parser.UseDeclaration
import gvteal.parser.SimpleImportDeclaration

import java.nio.file.Path

// case class ResolvedUseDeclaration(
//     parsed: UseDeclaration,
//     name: String,
//     isLibrary: Boolean,
//     path: Option[Path],
//     dependency: Option[ResolvedProgram]
// ) extends ResolvedNode

// TODO: Simple and Compound declaration difference 
case class ResolvedImportDeclaration(
        parsed: SimpleImportDeclaration,
        name: String,
        path: Option[Path],
        dependency: Option[ResolvedProgram]
) extends ResolvedNode