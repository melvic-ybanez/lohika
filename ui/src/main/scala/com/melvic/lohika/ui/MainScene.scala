package com.melvic.lohika.ui

import cats.implicits.*
import com.melvic.lohika.core.meta.Entailment
import Entailment.{Derived, Direct}
import com.melvic.lohika.controllers.{Eval, FileManager}
import com.melvic.lohika.controllers.symbols.Unicode
import com.melvic.lohika.core.prover.interpreters.LiveProver.{Steps, given}
import com.melvic.lohika.core.prover.programs.ProverProgram
import scalafx.beans.property.StringProperty
import scalafx.geometry.Orientation
import scalafx.scene.Scene
import scalafx.scene.control.*
import scalafx.scene.input.{KeyCode, KeyCodeCombination, KeyCombination}
import scalafx.scene.layout.{AnchorPane, BorderPane}
import scalafx.stage.{FileChooser, Stage}

class MainScene(stage: Stage, eval: Eval, fileManager: FileManager) extends Scene:
  self =>
  val entailmentProp = new StringProperty("")
  val solutionsView = SolutionsView()
  lazy val fileChooser = FileChooser()

  solutionsView.init()
  stylesheets.add(Resources.cssPath("main.css"))

  root = new BorderPane:
    top = new MenuBar:
      val fileMenu = new Menu("File"):
        val saveMenuItem = new MenuItem("Save..."):
          onAction = _ => save()
          accelerator = KeyCodeCombination(KeyCode.S, KeyCombination.ShortcutDown)

        items = List(saveMenuItem)

      val runMenu = new Menu("Run"):
        val runMenuItem = new MenuItem("Run Logical Query"):
          onAction = _ => run()
          accelerator = KeyCombination.keyCombination("Ctrl+R")

        items = List(runMenuItem)

      menus = List(fileMenu, runMenu)

    center = new SplitPane:
      styleClass = Seq("editor-split")
      orientation = Orientation.Vertical

      items.addAll(EditorPane(self), solutionsView)

  def rawContent: String =
    Unicode.removeFromText(entailmentProp.value)

  def run(): Unit =
    solutionsView.setSolutionContent(eval.run(rawContent))

  def save(): Unit =
    Option(fileChooser.showSaveDialog(stage)).foreach: selectedFile =>
      fileManager.save(rawContent, selectedFile.getAbsolutePath)
