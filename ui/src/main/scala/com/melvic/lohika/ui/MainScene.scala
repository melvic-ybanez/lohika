package com.melvic.lohika.ui

import com.melvic.lohika.controllers.symbols.Unicode
import com.melvic.lohika.controllers.{Eval, FileManager}
import com.melvic.lohika.core.meta.Entailment
import scalafx.beans.property.StringProperty
import scalafx.geometry.Orientation
import scalafx.scene.Scene
import scalafx.scene.control.*
import scalafx.scene.input.{KeyCode, KeyCodeCombination, KeyCombination}
import scalafx.scene.layout.BorderPane
import scalafx.stage.{FileChooser, Stage}

class MainScene(stage: Stage, eval: Eval, fileManager: FileManager) extends Scene:
  self =>
  val entailmentProp = new StringProperty("")
  val selectedTitle = new StringProperty("Untitled")
  val solutionsView = SolutionsView()
  lazy val fileChooser = FileChooser()

  solutionsView.init()
  stylesheets.add(Resources.cssPath("main.css"))

  root = new BorderPane:
    top = new MenuBar:
      val fileMenu: Menu = new Menu("File"):
        val saveMenuItem: MenuItem = new MenuItem("Save..."):
          onAction = _ => save()
          accelerator = KeyCodeCombination(KeyCode.S, KeyCombination.ShortcutDown)

        items = List(saveMenuItem)

      val runMenu: Menu = new Menu("Run"):
        val runMenuItem: MenuItem = new MenuItem("Run Logical Query"):
          onAction = _ => run()
          accelerator = KeyCombination.keyCombination("Ctrl+R")

        items = List(runMenuItem)

      menus = List(fileMenu, runMenu)

    center = new SplitPane:
      styleClass = Seq("editor-split")
      orientation = Orientation.Vertical

      items.addAll(EditorTabPane(self), solutionsView)

  def rawContent: String =
    Unicode.removeFromText(entailmentProp.value)

  def run(): Unit =
    solutionsView.setSolutionContent(eval.run(rawContent))

  def save(): Unit =
    Option(fileChooser.showSaveDialog(stage)).foreach: selectedFile =>
      fileManager.save(rawContent, selectedFile.getAbsolutePath).foreach: fileWithExtension =>
        selectedTitle.set(fileWithExtension.getName)
