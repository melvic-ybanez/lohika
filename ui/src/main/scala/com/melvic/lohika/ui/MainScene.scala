package com.melvic.lohika.ui

import com.melvic.lohika.prover.algebras.Prover
import com.melvic.lohika.prover.interpreters.LiveProver.{Steps, given}
import com.melvic.lohika.prover.programs.ProverProgram
import scalafx.beans.property.StringProperty
import scalafx.scene.Scene
import scalafx.scene.control.{TextArea, TextField}
import scalafx.scene.layout.{BorderPane, VBox}

class MainScene extends Scene:
  stylesheets.add(getClass.getResource("/css/styles.css").toExternalForm)

  val assumptionsProp = new StringProperty("")
  val propositionProp = new StringProperty("")
  val solutionsProp = new StringProperty("")

  root = new BorderPane:
    center = new TextArea:
      styleClass += "main-io-text-field"
      editable = false
      style = "-fx-background-color: white"
      minWidth = 600
      minHeight = 600
      text <== solutionsProp
    top = new VBox:
      children = Seq(
        new InputText:
          promptText = s"""Assumptions (e.g. "P & Q, A, B => C" to mean "${Symbols.applyToText(
              "P & Q, A, B => C"
            )}")"""
          text <==> assumptionsProp
        ,
        new InputText:
          promptText = s"""Proposition (e.g. "A | C" to mean "${Symbols.applyToText("A | C")}")"""
          text <==> propositionProp

          onAction = event =>
            val assumptions = Symbols.removeFromText(assumptionsProp.value)
            val proposition = Symbols.removeFromText(propositionProp.value)
            ProverProgram.prove[Steps](assumptions, proposition).run match
              case Left(error) => solutionsProp.value = error
              case Right(steps, _) =>
                solutionsProp.value = Symbols.applyToText(steps.mkString("\n"))
      )

class InputText extends TextField:
  styleClass ++= Seq("main-io-text-field", "main-io-input-text-field")
  text.onChange: (_, _, newValue) =>
    text = Symbols.applyToText(text.value)
