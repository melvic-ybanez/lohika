package com.melvic.lohika.ui

import atlantafx.base.theme.{NordDark, PrimerDark, PrimerLight}
import javafx.application.Application
import scalafx.application.JFXApp3
import scalafx.geometry.Insets
import scalafx.scene.Scene
import scalafx.scene.control.{TextArea, TextField}
import scalafx.scene.effect.DropShadow
import scalafx.scene.layout.{Background, BackgroundFill, BorderPane, HBox, Priority, VBox}
import scalafx.scene.paint.*
import scalafx.scene.paint.Color.*
import scalafx.scene.text.{Font, Text}
import scalafx.stage.StageStyle

import scala.language.implicitConversions

object Main extends JFXApp3:

  override def start(): Unit =
    Application.setUserAgentStylesheet(new NordDark().getUserAgentStylesheet)

    stage = new JFXApp3.PrimaryStage:
      title = "Lohika"
      scene = new MainScene

    stage.maximized = true
