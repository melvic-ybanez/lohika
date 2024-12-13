package com.melvic.lohika.controllers

import java.io.*
import scala.util.{Try, Using}

trait FileManager:
  def save(script: String, path: String): Try[File]

object FileManager:
  val FileExtension: String = ".lhk"

  def live: FileManager = (script, path) =>
    val fileWithExtension = File(
      if path.endsWith(FileExtension) then path else path + FileExtension
    )
    Using(BufferedWriter(FileWriter(fileWithExtension, false))): writer =>
      writer.write(script)
      fileWithExtension
