package com.melvic.lohika.ui

import cats.implicits.*
import com.melvic.lohika.meta.Entailment
import com.melvic.lohika.meta.Entailment.{Derived, Direct}
import com.melvic.lohika.prover.interpreters.LiveProver.{Steps, given}
import com.melvic.lohika.prover.programs.ProverProgram
import com.melvic.lohika.ui.symbols.{MathJax, Unicode}
import scalafx.beans.property.StringProperty
import scalafx.geometry.Orientation
import scalafx.scene.Scene
import scalafx.scene.control.*
import scalafx.scene.input.KeyCombination
import scalafx.scene.layout.{AnchorPane, BorderPane}

class MainScene extends Scene:
  self =>
  val entailmentProp = new StringProperty("")
  val solutionsView = SolutionsView()

  solutionsView.init()
  stylesheets.add(Resources.cssPath("main.css"))

  root = new BorderPane:
    top = new MenuBar:
      val runMenu = new Menu("Run"):
        val runMenuItem = new MenuItem("Run Logical Query"):
          onAction = _ => handleInput()
          accelerator = KeyCombination.keyCombination("Ctrl+R")

        items = List(runMenuItem)

      menus = List(runMenu)

    center = new SplitPane:
      styleClass = Seq("editor-split")
      orientation = Orientation.Vertical

      items.addAll(EditorPane(self), solutionsView)

  def handleInput(): Unit =
    val rawEntailment = Unicode.removeFromText(entailmentProp.value)

    def handleDirect(entailment: Direct, steps: List[String]): Unit =
      val mdSteps = steps.map: step =>
        if step.endsWith(".") || step.endsWith(":") || step.trim.startsWith("*") then step
        else step + "."
      val entailmentElem = MathJax.applyToText(entailment.show)
      val solution = MathJax.applyToText(mdSteps.mkString("\n\n"))
      solutionsView.setSolutionContent(Right(entailmentElem, solution))

    ProverProgram.prove[Steps](rawEntailment).run match
      case Left(error)                            => solutionsView.setSolutionContent(Left(error))
      case Right(steps, (entailment: Direct, _))  => handleDirect(entailment, steps)
      case Right(steps, (entailment: Derived, _)) => handleDirect(Entailment.unfold(entailment), steps)
