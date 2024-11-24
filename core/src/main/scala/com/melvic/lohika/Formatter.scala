package com.melvic.lohika

import com.melvic.lohika.Formatter.Format

trait Formatter {
  def emphasize: Format

  def strong: Format

  def link(target: String): Format

  def itemNumber: String

  def indent: String = " " * 4

  def formula: Format = emphasize
  
  def newline: String
}

object Formatter:
  type Format = String => String
  type FormatterContext = Formatter ?=> String

  extension (str: String)
    def emphasize: FormatterContext =
      summon[Formatter].emphasize(str)

    def strong: FormatterContext =
      summon[Formatter].strong(str)

    def link(target: String): FormatterContext =
      summon[Formatter].link(target)(str)

    def formula: FormatterContext =
      summon[Formatter].formula(str)

  def itemNumber: FormatterContext = summon[Formatter].itemNumber

  def indent: FormatterContext = summon[Formatter].indent
