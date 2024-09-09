package com.melvic.lohika

import cats.Show
import cats.implicits.*

object Givens:
  given csv[A: Show]: Show[List[A]] = Show.show: xs =>
    if xs.size > 1 then xs.init.map(_.show).mkString(", ") + xs.last.show
    else xs.head.show
