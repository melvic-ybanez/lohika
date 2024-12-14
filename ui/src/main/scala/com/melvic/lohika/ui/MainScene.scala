package com.melvic.lohika.ui

import com.melvic.lohika.controllers.symbols.Unicode
import com.melvic.lohika.controllers.{Eval, FileManager}
import com.melvic.lohika.ui.events.FileEventHandler
import scalafx.beans.property.StringProperty
import scalafx.geometry.Orientation
import scalafx.scene.Scene
import scalafx.scene.control.*
import scalafx.scene.input.{KeyCode, KeyCodeCombination, KeyCombination}
import scalafx.scene.layout.BorderPane
import scalafx.stage.{FileChooser, Stage}

class MainScene(val stage: Stage, eval: Eval, val fileManager: FileManager) extends Scene:
  self =>
  lazy val solutionsView = SolutionsView()
  lazy val editorTabPane = EditorTabPane(self)
  lazy val fileEventHandler = FileEventHandler(self)

  val entailmentProp = new StringProperty("")
  val selectedTitleProp = new StringProperty("Untitled")
  val selectedPathProp = new StringProperty()

  lazy val fileChooser = FileChooser()

  solutionsView.init()
  stylesheets.add(Resources.cssPath("main.css"))

  root = new BorderPane:
    top = new MenuBar:
      val fileMenu: Menu = new Menu("File"):
        val saveMenuItem: MenuItem = new MenuItem("Save..."):
          onAction = _ => fileEventHandler.save()
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

      items.addAll(editorTabPane, solutionsView)

  def rawContent: String =
    Unicode.removeFromText(entailmentProp.value)

  def run(): Unit =
    solutionsView.setSolutionContent(eval.run(rawContent))
