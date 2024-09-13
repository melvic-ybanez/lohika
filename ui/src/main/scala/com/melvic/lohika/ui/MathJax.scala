package com.melvic.lohika.ui

object MathJax:
  val And: String = "\\land"
  val Or: String = "\\lor"
  val Not: String = "\\neg "
  val Imply: String = "\\rightarrow"
  val Iff: String = "\\leftrightarrow"

  val TextToMathMap: List[(String, String)] = List(
    "&"   -> And,
    "|"   -> Or,
    "!"   -> Not,
    "=>"  -> Imply,
    "<=>" -> Iff
  )

  def applyToText(text: String): String =
    TextToMathMap.foldLeft(text):
      case (text, (symbol, code)) => text.replace(symbol, code)

  def removeFromText(text: String): String =
    TextToMathMap.foldLeft(text):
      case (text, (symbol, code)) => text.replace(code, symbol)
