package com.melvic.lohika

import cats.Show
import cats.implicits.*

object Givens:
  given csv[A: Show]: Show[List[A]] = Show.show:
    case Nil      => ""
    case x :: Nil => x.show
    case xs       => xs.init.map(_.show).mkString(", ") + " and " + xs.last.show
