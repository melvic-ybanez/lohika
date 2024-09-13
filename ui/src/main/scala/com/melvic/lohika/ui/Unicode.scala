package com.melvic.lohika.ui

object Unicode:
  val And: Char = '\u2227'
  val Or: Char = '\u2228'
  val Not: Char = '\u00AC'
  val Imply: Char = '\u2192'
  val Iff: Char = '\u2194'

  val TextToCodeMap: List[(String, Char)] = List(
    "&"   -> And,
    "|"   -> Or,
    "!"   -> Not,
    "=>"  -> Imply,
    "<=>" -> Iff
  )

  def applyToText(text: String): String =
    TextToCodeMap.foldLeft(text):
      case (text, (symbol, code)) => text.replace(symbol, code.toString)

  def removeFromText(text: String): String =
    TextToCodeMap.foldLeft(text):
      case (text, (symbol, code)) => text.replace(code.toString, symbol)
