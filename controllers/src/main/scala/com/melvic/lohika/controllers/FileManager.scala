package com.melvic.lohika.controllers

import java.io.*
import scala.util.{Try, Using}

trait FileManager:
  def save(script: String, path: String): Try[Unit]

object FileManager:
  val FileExtension: String = ".lhk"

  def live: FileManager = (script, path) =>
    val pathWithExtension = if path.endsWith(FileExtension) then path else path + FileExtension
    Using(BufferedWriter(FileWriter(File(pathWithExtension), false)))(_.write(script))
