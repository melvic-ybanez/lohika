package com.melvic.lohika.ui

import com.melvic.lohika.prover.interpreters.LiveProver.{Steps, given}
import com.melvic.lohika.prover.programs.ProverProgram
import scalafx.beans.property.StringProperty
import scalafx.geometry.Insets
import scalafx.scene.Scene
import scalafx.scene.control.TextField
import scalafx.scene.layout.{BorderPane, VBox}

class MainScene extends Scene:
  stylesheets.add(getClass.getResource("/css/styles.css").toExternalForm)

  val assumptionsProp = new StringProperty("")
  val propositionProp = new StringProperty("")
  val solutionsView = SolutionsView()

  root = new BorderPane:
    center = new BorderPane:
      center = solutionsView
      padding = Insets(30)
    top = new VBox:
      children = Seq(
        new InputText:
          promptText = s"""Optional Assumptions (e.g. "P & Q, A, B => C" to mean "${Unicode.applyToText(
              "P & Q, A, B => C"
            )}")"""
          text <==> assumptionsProp
        ,
        new InputText:
          promptText = s"""Proposition (e.g. "A | C" to mean "${Unicode.applyToText("A | C")}")"""
          text <==> propositionProp

          onAction = event =>
            val assumptions = Unicode.removeFromText(assumptionsProp.value)
            val proposition = Unicode.removeFromText(propositionProp.value)
            ProverProgram.prove[Steps](assumptions, proposition).run match
              case Left(error) => solutionsView.setSolutionContent(error)
              case Right(steps, _) =>
                val mdSteps = steps.map: step =>
                  if step.endsWith(".") || step.endsWith(":") || step.trim.startsWith("*") then step
                  else step + "."
                val content =s"### Solution:\n\n${mdSteps.mkString("\n\n")}"
                solutionsView.setSolutionContent(MathJax.applyToText(content))
      )

class InputText extends TextField:
  styleClass += "main-io-text-field"
  text.onChange: (_, _, newValue) =>
    text = Unicode.applyToText(text.value)
