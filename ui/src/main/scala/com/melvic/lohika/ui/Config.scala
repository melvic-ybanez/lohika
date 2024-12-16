package com.melvic.lohika.ui

trait Config:
  val appName: String

object Config:
  // TODO: We might want to fetch the values from a configuration file
  def live: Config = new Config:
    val appName: String = "Lohika"
