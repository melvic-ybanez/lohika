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
          promptText =
            s"""Assumptions (e.g. "P & Q, A, B -> C" to mean "P $And Q, A, B $Imply C")"""
          styleClass ++= Seq("main-io-text-field", "main-io-input-text-field")
        ,
        new TextField:
          promptText = s"""Proposition (e.g. "A | C" to mean "A $Or C")"""
          styleClass ++= Seq("main-io-text-field", "main-io-input-text-field")
      )
