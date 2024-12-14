package com.melvic.lohika.ui.events

import com.melvic.lohika.controllers.FileManager
import com.melvic.lohika.ui.MainScene
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.control.{Alert, ButtonBar, ButtonType}

import java.io.File

class FileEventHandler(mainScene: MainScene):
  import mainScene.*

  def save(): Unit =
    if selectedPathProp.isEmpty.get() then saveAs()
    else
      val selectedPath = selectedPathProp.get()
      val selectedFileWithExtension = FileManager.toFileWithExtension(selectedPath)
      if selectedFileWithExtension.exists() then
        fileManager
          .save(rawContent, selectedFileWithExtension.getAbsolutePath)
          .foreach: _ =>
            selectedTitleProp.set(selectedFileWithExtension.getName)

  def saveAs(): Unit =
    Option(fileChooser.showSaveDialog(stage)).foreach: selectedFile =>
      saveSelected(selectedFile): fullPath =>
        fileManager
          .save(rawContent, fullPath)
          .foreach: fileWithExtension =>
            selectedTitleProp.set(fileWithExtension.getName)
            selectedPathProp.set(fullPath)

  private def saveSelected(file: File)(f: String => Unit): Unit =
    val extendedFile = FileManager.toFileWithExtension(file.getAbsolutePath)
    if extendedFile.exists then
      val alert = new Alert(AlertType.Confirmation):
        title = "Duplicate File"
        headerText = s"File ${extendedFile.getAbsolutePath} already exists."
        contentText = "Do you want to replace it?"

      val replaceButton = ButtonType("Replace", ButtonBar.ButtonData.OKDone)
      alert.buttonTypes = Seq(replaceButton, ButtonType.Cancel)

      alert.showAndWait() match
        case Some(`replaceButton`) => f(extendedFile.getAbsolutePath)
        case _                     => ()
    else f(extendedFile.getAbsolutePath)
