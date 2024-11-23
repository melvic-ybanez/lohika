package com.melvic.lohika.parsers

import fastparse.*
import fastparse.MultiLineWhitespace.*

object Parser extends MetaParsing with ExprParsing with FormulaParsing:
  def alphabetic[$: P](filter: Char => Boolean): P[String] =
    CharPred(c => Character.isAlphabetic(c) && filter(c)).!
    
  def stmtDelimiter[$: P]: P[Unit] = P(Lexemes.StmtDelimiter)
