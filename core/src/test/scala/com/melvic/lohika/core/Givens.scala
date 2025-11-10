package com.melvic.lohika.core

import com.melvic.lohika.core.Formatter.Format

trait Givens:
  given Formatter with
    override def emphasize: Format = identity

    override def strong: Format = identity

    override def link(target: String): Format = identity

    override def itemNumber: String = ""

    override def newline: String = "\n"

    override def sentence: Format = identity
