package com.melvic.lohika.core

trait FunctionUtils:
  def rewriteOrId[A, B >: A](pf: PartialFunction[A, B])(value: A): B =
    pf.applyOrElse(value, identity)
