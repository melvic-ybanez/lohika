package com.melvic.lohika.ui

import scalafx.Includes.*
import scalafx.scene.control.TabPane.TabClosingPolicy.Unavailable
import scalafx.scene.control.{Tab, TabPane}

class EditorTabPane(mainScene: MainScene) extends TabPane:
  tabClosingPolicy = Unavailable

  val tab: Tab = new Tab:
    content = EditorPane(mainScene)

  tab.textProperty() <== mainScene.selectedTitle

  tabs = List(tab)
