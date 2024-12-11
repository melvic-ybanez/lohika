package com.melvic.lohika.ui

import atlantafx.base.theme.PrimerDark
import com.melvic.lohika.controllers.Eval
import javafx.application.Application
import scalafx.application.JFXApp3

import scala.language.implicitConversions

object Main extends JFXApp3:

  override def start(): Unit =
    Application.setUserAgentStylesheet(PrimerDark().getUserAgentStylesheet)

    stage = new JFXApp3.PrimaryStage:
      title = "Lohika"
      scene = new MainScene(Eval.live)

    stage.maximized = true
