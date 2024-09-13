package com.melvic.lohika.ui.symbols

trait Symbols:
  def And: String
  def Or: String
  def Not: String
  def Imply: String
  def Iff: String

  def TextToSymbolsMap: List[(String, String)] = List(
    "&"   -> And,
    "|"   -> Or,
    "!"   -> Not,
    "=>"  -> Imply,
    "<=>" -> Iff
  )

  def applyToText(text: String): String =
    TextToSymbolsMap.foldLeft(text):
      case (text, (symbol, code)) => text.replace(symbol, code)

  def removeFromText(text: String): String =
    TextToSymbolsMap.foldLeft(text):
      case (text, (symbol, code)) => text.replace(code, symbol)
