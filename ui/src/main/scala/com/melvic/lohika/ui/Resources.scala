package com.melvic.lohika.ui

object Resources:
  def fromPath(path: String): String =
    getClass.getResource(path).toExternalForm

  def cssPath(path: String): String =
    fromPath(s"/css/$path")

  def iconPath(path: String): String =
    fromPath(s"/icons/$path")


