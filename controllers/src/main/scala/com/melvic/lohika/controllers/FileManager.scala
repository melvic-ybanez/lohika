package com.melvic.lohika.controllers

import java.io.*
import scala.io.Source
import scala.util.{Try, Using}

trait FileManager:
  val fileExtension: String

  def save(script: String, path: String): Try[File]

  def open(path: String): Try[String]

  def toExtendedFile(path: String): File =
    File(if path.endsWith(fileExtension) then path else path + fileExtension)

object FileManager:
  def live: FileManager = LiveFileManager()

class LiveFileManager extends FileManager:
  def save(script: String, path: String): Try[File] =
    val extendedFile = toExtendedFile(path)
    Using(BufferedWriter(FileWriter(extendedFile, false))): writer =>
      writer.write(script)
      extendedFile

  override def open(path: String): Try[String] =
    Using(Source.fromFile(path))(_.mkString)

  override val fileExtension: String = ".lhk"
