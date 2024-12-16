package com.melvic.lohika.ui.events

import com.melvic.lohika.controllers.FileManager
import com.melvic.lohika.ui.MainScene
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.control.{Alert, ButtonBar, ButtonType, TextInputDialog}

import java.io.File

class FileEventHandler(mainScene: MainScene):
  import mainScene.*

  def save(): Unit =
    if editorTabPane.selectedPathProp.isEmpty.get() then saveAs()
    else
      val selectedPath = editorTabPane.selectedPathProp.get()
      val selectedFileWithExtension = fileManager.toExtendedFile(selectedPath)
      if selectedFileWithExtension.exists() then
        fileManager
          .save(rawContent, selectedFileWithExtension.getAbsolutePath)
          .foreach: _ =>
            editorTabPane.selectedTitleProp.set(selectedFileWithExtension.getName)

  def saveAs(): Unit =
    Option(fileChooser.showSaveDialog(stage)).foreach: selectedFile =>
      saveSelected(selectedFile): fullPath =>
        fileManager
          .save(rawContent, fullPath)
          .fold(
            error =>
              new Alert(AlertType.Error) {
                title = "Error Saving File"
                headerText = s"Unable to save $fullPath."
                contentText = s"Message: ${error.getMessage}."
              }.showAndWait(),
            extendedFile =>
              editorTabPane.selectedTitleProp.set(extendedFile.getName)
              editorTabPane.selectedPathProp.set(fullPath)
          )

  def open(): Unit =
    Option(fileChooser.showOpenDialog(stage)).foreach: selectedFile =>
      val fullPath = selectedFile.getAbsolutePath
      fileManager
        .open(fullPath)
        .fold(
          error =>
            new Alert(AlertType.Error) {
              title = "Error Opening File"
              headerText = s"Unable to open $fullPath."
              contentText = s"Message: ${error.getMessage}"
            }.showAndWait(),
          script => editorTabPane.newTab(selectedFile.getName, script, fullPath)
        )

  private def saveSelected(file: File)(f: String => Unit): Unit =
    val extendedFile = fileManager.toExtendedFile(file.getAbsolutePath)
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
