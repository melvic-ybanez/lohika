package com.melvic.lohika.ui

import com.melvic.lohika.prover.interpreters.LiveProver.{Steps, given}
import com.melvic.lohika.prover.programs.ProverProgram
import com.melvic.lohika.ui.symbols.{MathJax, Unicode}
import scalafx.beans.property.StringProperty
import scalafx.geometry.Insets
import scalafx.scene.Scene
import scalafx.scene.control.TextField
import scalafx.scene.input.ScrollEvent
import scalafx.scene.layout.{BorderPane, VBox}

class MainScene extends Scene:
  val entailmentProp = new StringProperty("")
  val solutionsView = SolutionsView()

  solutionsView.init()
  stylesheets.add(getClass.getResource("/css/main.css").toExternalForm)

  root = new BorderPane:
    center = new BorderPane:
      center = solutionsView

      padding = Insets(30)

    top = new VBox:
      children = Seq(
        new InputText:
          promptText =
            s"""Logical Entailment (e.g. "A => B, B => C |= A | !C" to mean "${Unicode.applyToText(
                "A => B, B => C |= A | !C"
              )}")"""
          text <==> entailmentProp

          onAction = event =>
            val rawEntailment = Unicode.removeFromText(entailmentProp.value)
            ProverProgram.prove[Steps](rawEntailment).run match
              case Left(error) => solutionsView.setSolutionContent(Left(error))
              case Right(steps, _) =>
                val mdSteps = steps.map: step =>
                  if step.endsWith(".") || step.endsWith(":") || step.trim.startsWith("*") then step
                  else step + "."
                val entailment = MathJax.applyToText(s"\\($rawEntailment\\)")
                val solution = MathJax.applyToText(mdSteps.mkString("\n\n"))
                solutionsView.setSolutionContent(Right(entailment, solution))
      )

  // We are disabling some scrolling functionalities (particularly mouse wheels, etc.)
  // for now because it causes issues with rendering when contents are long enough to
  // be scrollable
  solutionsView.addEventFilter(ScrollEvent.Scroll, _.consume())

class InputText extends TextField:
  styleClass += "main-io-text-field"
  text.onChange: (_, _, _) =>
    // unapply them first so we can re-apply with the correct priority (e.g. |= before |)
    val rawText = Unicode.removeFromText(text.value)

    text = Unicode.applyToText(rawText)
