package com.melvic.lohika.ui

import atlantafx.base.theme.NordDark
import com.melvic.lohika.controllers.{Eval, FileManager}
import javafx.application.Application
import scalafx.application.JFXApp3
import com.melvic.lohika.core.prover.Prover.given

import scala.language.implicitConversions

object Main extends JFXApp3:

  override def start(): Unit =
    Application.setUserAgentStylesheet(NordDark().getUserAgentStylesheet)
    val config = Config.live

    stage = new JFXApp3.PrimaryStage:
      title = config.appName
      scene = new MainScene(this, Eval.live, FileManager.live, config)

    stage.maximized = true
