package com.melvic.lohika.ui.symbols

import com.melvic.lohika.parsers.Lexemes

trait Symbols:
  def And: String
  def Or: String
  def Not: String
  def Imply: String
  def Iff: String
  def Entailment: String
  def Forall: String
  def ThereExists: String
  def DefinedAs: String

  /**
   * Maps texts to symbols.
   *
   * Note: The ordering matters here, at least for some of them. In particular, `|=` should come
   * before `|` and `<=>` should come before `=>`. In other words, the longer symbols should be
   * prioritized to avoid rendering issues. If, for instance, `|` precedes `|=`, then `|=` will be
   * rendered as the symbol for `|` followed by `=`.
   */
  def TextToSymbolsMap: List[(String, String)] = List(
    Lexemes.Entailment  -> Entailment,
    Lexemes.And         -> And,
    Lexemes.Or          -> Or,
    Lexemes.Not         -> Not,
    Lexemes.Iff         -> Iff,
    Lexemes.Imply       -> Imply,
    Lexemes.Forall      -> Forall,
    Lexemes.ThereExists -> ThereExists,
    Lexemes.DefinedAs   -> DefinedAs
  )

  def applyToText(text: String): String =
    TextToSymbolsMap.foldLeft(text):
      case (text, (symbol, code)) => text.replace(symbol, code)

  def removeFromText(text: String): String =
    TextToSymbolsMap.foldLeft(text):
      case (text, (symbol, code)) => text.replace(code, symbol)
