package com.melvic.lohika.ui

import cats.implicits.*
import com.melvic.lohika.core.meta.Entailment
import Entailment.{Derived, Direct}
import com.melvic.lohika.controllers.Eval
import com.melvic.lohika.controllers.symbols.Unicode
import com.melvic.lohika.core.prover.interpreters.LiveProver.{Steps, given}
import com.melvic.lohika.core.prover.programs.ProverProgram
import scalafx.beans.property.StringProperty
import scalafx.geometry.Orientation
import scalafx.scene.Scene
import scalafx.scene.control.*
import scalafx.scene.input.KeyCombination
import scalafx.scene.layout.{AnchorPane, BorderPane}

class MainScene(eval: Eval) extends Scene:
  self =>
  val entailmentProp = new StringProperty("")
  val solutionsView = SolutionsView()

  solutionsView.init()
  stylesheets.add(Resources.cssPath("main.css"))

  root = new BorderPane:
    top = new MenuBar:
      val runMenu = new Menu("Run"):
        val runMenuItem = new MenuItem("Run Logical Query"):
          onAction = _ => run()
          accelerator = KeyCombination.keyCombination("Ctrl+R")

        items = List(runMenuItem)

      menus = List(runMenu)

    center = new SplitPane:
      styleClass = Seq("editor-split")
      orientation = Orientation.Vertical

      items.addAll(EditorPane(self), solutionsView)

  def run(): Unit =
    val rawEntailment = Unicode.removeFromText(entailmentProp.value)
    solutionsView.setSolutionContent(eval.run(rawEntailment))
