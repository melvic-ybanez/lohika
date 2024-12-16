package com.melvic.lohika.ui

import atlantafx.base.theme.NordDark
import com.melvic.lohika.controllers.{Eval, FileManager}
import javafx.application.Application
import scalafx.application.JFXApp3

import scala.language.implicitConversions

object Main extends JFXApp3:

  override def start(): Unit =
    Application.setUserAgentStylesheet(NordDark().getUserAgentStylesheet)

    stage = new JFXApp3.PrimaryStage:
      title = Config.AppName
      scene = new MainScene(this, Eval.live, FileManager.live)

    stage.maximized = true
