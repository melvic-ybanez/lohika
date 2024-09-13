package com.melvic.lohika

trait Emphasis {
  def weak(text: String): String
  
  def strong(text: String): String
}

object Emphasis:
  extension (str: String)
    def weak(using emphasis: Emphasis): String =
      emphasis.weak(str)

    def strong(using emphasis: Emphasis): String =
      emphasis.strong(str)
