package com.melvic.lohika.ui

import atlantafx.base.theme.PrimerDark
import javafx.application.Application
import scalafx.application.JFXApp3

import scala.language.implicitConversions

object Main extends JFXApp3:

  override def start(): Unit =
    Application.setUserAgentStylesheet(PrimerDark().getUserAgentStylesheet)

    stage = new JFXApp3.PrimaryStage:
      title = "Lohika"
      scene = new MainScene

    stage.maximized = true
