package com.melvic.lohika.parsers

import fastparse.{parse as fastParse, *}

object Parser extends MetaParsing with ExprParsing with FormulaParsing:
  def alphabetic[$: P](filter: Char => Boolean): P[String] =
    (CharPred(c => Character.isAlphabetic(c) && filter(c))).!
