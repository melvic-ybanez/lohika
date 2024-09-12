package com.melvic.lohika.ui

import com.melvic.lohika.ui.Symbols.{And, Imply, Or}
import scalafx.scene.Scene
import scalafx.scene.control.{TextArea, TextField}
import scalafx.scene.layout.{BorderPane, VBox}

class MainScene extends Scene:
  stylesheets.add(getClass.getResource("/css/styles.css").toExternalForm)

  root = new BorderPane:
    center = new TextArea:
      styleClass += "main-io-text-field"
      editable = false
      style = "-fx-background-color: white"
      minWidth = 600
      minHeight = 600
    top = new VBox:
      children = Seq(
        new TextField:
          promptText = s"""Assumptions (e.g. "P & Q, A, B => C" to mean "${Symbols.applyToText(
              "P & Q, A, B => C"
            )}")"""
          styleClass ++= Seq("main-io-text-field", "main-io-input-text-field")
          text.onChange: (_, _, newValue) =>
            text = Symbols.applyToText(text.value)
        ,
        new TextField:
          promptText = s"""Proposition (e.g. "A | C" to mean "${Symbols.applyToText("A | C")}")"""
          styleClass ++= Seq("main-io-text-field", "main-io-input-text-field")
      )
