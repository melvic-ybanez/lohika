package com.melvic.lohika.ui

import cats.implicits.*
import com.melvic.lohika.prover.interpreters.LiveProver.{Steps, given}
import com.melvic.lohika.prover.programs.ProverProgram
import com.melvic.lohika.ui.symbols.{MathJax, Unicode}
import org.fxmisc.richtext.CodeArea
import scalafx.Includes.*
import scalafx.beans.property.StringProperty
import scalafx.geometry.Orientation
import scalafx.scene.Scene
import scalafx.scene.control.SplitPane
import scalafx.scene.layout.BorderPane

class MainScene extends Scene:
  val entailmentProp = new StringProperty("")
  val solutionsView = SolutionsView()

  solutionsView.init()
  stylesheets.add(getClass.getResource("/css/main.css").toExternalForm)

  root = new SplitPane:
    styleClass = Seq("editor-split")
    orientation = Orientation.Vertical

    items.addAll(
      new BorderPane {
        center = new EditorView:
          entailmentProp <== textProperty()
      },
      solutionsView
    )

    def handleInput(): Unit =
      val rawEntailment = Unicode.removeFromText(entailmentProp.value)
      ProverProgram.prove[Steps](rawEntailment).run match
        case Left(error) => solutionsView.setSolutionContent(Left(error))
        case Right(steps, (entailment, _)) =>
          val mdSteps = steps.map: step =>
            if step.endsWith(".") || step.endsWith(":") || step.trim.startsWith("*") then step
            else step + "."
          val entailmentElem = MathJax.applyToText(entailment.show)
          val solution = MathJax.applyToText(mdSteps.mkString("\n\n"))
          solutionsView.setSolutionContent(Right(entailmentElem, solution))

class EditorView extends CodeArea:
  getStyleClass.add("editor")
