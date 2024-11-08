package com.melvic.lohika.ui

import org.fxmisc.richtext.CodeArea
import scalafx.scene.layout.AnchorPane
import scalafx.Includes.*

class EditorPane(mainScene: MainScene) extends AnchorPane:
  stylesheets += Resources.cssPath("editor")

  val runButton = new Icon("run.png"):
    onAction = _ => mainScene.handleInput()

  val editorArea = new EditorView:
    mainScene.entailmentProp <== textProperty()

  children.addAll(editorArea, runButton)

  AnchorPane.setTopAnchor(runButton, 10.0)
  AnchorPane.setRightAnchor(runButton, 20.0)
  AnchorPane.setTopAnchor(editorArea, 20.0)
  AnchorPane.setRightAnchor(editorArea, 0.0)
  AnchorPane.setLeftAnchor(editorArea, 0.0)
  AnchorPane.setBottomAnchor(editorArea, 0.0)

class EditorView extends CodeArea:
  getStyleClass.add("editor")
