package gvteal.pytealparser

import scala.collection.mutable.ArrayBuffer
import scala.collection.Searching._

class ParserState(val lines: Array[Int]) {
  def this(source: String) {
    this(ParserState.splitLines(source))
  }

  def position(index: Int): Ast.Span.SourcePosition = {
    val lineIndex = lines.search(index) match {
      case Found(i) => i
      case InsertionPoint(i) => i
    }

    val column = lineIndex match {
      case 0 => index + 1
      case _ => index - lines(lineIndex - 1)
    }

    Ast.Span.SourcePosition(lineIndex + 1, column, index)
  }
}

object ParserState {
  private def splitLines(source: String): Array[Int] = {
    var positions = new ArrayBuffer[Int]()
    var i = 0

    while(source.indexOf('\n', i) match {
      case -1 => {
        positions += source.length()
        false
      }
      case pos => {
        positions += pos
        i = pos + 1
        true
      }
    })()

    positions.toArray
  }
}