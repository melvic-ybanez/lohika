package com.melvic.lohika.tests

import com.melvic.lohika.Formatter
import com.melvic.lohika.Formatter.Format

trait Givens:
  given Formatter with
    override def emphasize: Format = identity

    override def strong: Format = identity

    override def link(target: String): Format = identity

    override def itemNumber: String = ""
