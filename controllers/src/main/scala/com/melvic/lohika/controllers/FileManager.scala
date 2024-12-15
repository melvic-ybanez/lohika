package com.melvic.lohika.controllers

import java.io.*
import scala.io.Source
import scala.util.{Try, Using}

trait FileManager:
  def save(script: String, path: String): Try[File]

  def open(path: String): Try[String]

object FileManager:
  val FileExtension: String = ".lhk"

  def live: FileManager = LiveFileManager()

  def toExtendedFile(path: String): File =
    File(if path.endsWith(FileExtension) then path else path + FileExtension)

class LiveFileManager extends FileManager:
  def save(script: String, path: String): Try[File] =
    val extendedFile = FileManager.toExtendedFile(path)
    Using(BufferedWriter(FileWriter(extendedFile, false))): writer =>
      writer.write(script)
      extendedFile

  override def open(path: String): Try[String] =
    Using(Source.fromFile(path))(_.mkString)
