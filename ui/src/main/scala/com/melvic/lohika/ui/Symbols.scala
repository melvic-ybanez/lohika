package com.melvic.lohika.ui

object Symbols:
  val And: Char = '\u2227'
  val Or: Char = '\u2228'
  val Not: Char = '\u00AC'
  val Imply: Char = '\u2192'
  val Iff: Char = '\u2194'

  def applyToText(text: String): String =
    val pairs = List(
      "&"   -> And,
      "|"   -> Or,
      "!"   -> Not,
      "=>"  -> Imply,
      "<=>" -> Iff
    )
    pairs.foldLeft(text):
      case (text, (symbol, code)) => text.replace(symbol, code.toString)
