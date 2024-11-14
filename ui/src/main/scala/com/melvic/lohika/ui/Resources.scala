package com.melvic.lohika.ui

object Resources:
  def fromPath(path: String): String =
    getClass.getResource(path).toExternalForm

  def cssPath(path: String): String =
    val fullPath = if path.endsWith(".css") then path else path + ".css"
    fromPath(s"/css/$fullPath")

  def iconPath(path: String): String =
    fromPath(s"/icons/$path")
