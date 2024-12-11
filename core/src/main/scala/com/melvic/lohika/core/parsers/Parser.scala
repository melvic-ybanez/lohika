package com.melvic.lohika.core.parsers

import fastparse.*

object Parser extends MetaParsing with ExprParsing with FormulaParsing:
  given whitespace: Whitespace = ScriptWhitespace.whitespace

  def alphabetic[$: P](filter: Char => Boolean): P[String] =
    CharPred(c => Character.isAlphabetic(c) && filter(c)).!

  def stmtDelimiter[$: P]: P[Unit] = P(Lexemes.StmtDelimiter)
