package com.melvic.lohika.ui.symbols

trait Symbols:
  def And: String
  def Or: String
  def Not: String
  def Imply: String
  def Iff: String
  def Entailment: String

  /**
   * Maps texts to symbols.
   *
   * Note: The ordering matters here, at least for some of them. In particular, `|=` should come
   * before `|` and `<=>` should come before `=>`. In other words, the longer symbols should be
   * prioritized to avoid rendering issues. If, for instance, `|` precedes `|=`, then `|=` will be
   * rendered as the symbol for `|` followed by `=`.
   */
  def TextToSymbolsMap: List[(String, String)] = List(
    "|="  -> Entailment,
    "&"   -> And,
    "|"   -> Or,
    "!"   -> Not,
    "<=>" -> Iff,
    "=>"  -> Imply
  )

  def applyToText(text: String): String =
    TextToSymbolsMap.foldLeft(text):
      case (text, (symbol, code)) => text.replace(symbol, code)

  def removeFromText(text: String): String =
    TextToSymbolsMap.foldLeft(text):
      case (text, (symbol, code)) => text.replace(code, symbol)
