package com.melvic.lohika.ui

import org.fxmisc.richtext.CodeArea
import org.fxmisc.richtext.model.{StyleSpans, StyleSpansBuilder}
import scalafx.scene.layout.AnchorPane
import scalafx.Includes.*

import java.util
import java.util.Collections
import scala.util.matching.Regex

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

  val operatorGroupName = "OPERATOR"

  val operatorPattern = raw"(<=>|=>|&|!|\||A:|E:)"
  val fullPattern: Regex = s"(?<$operatorGroupName>$operatorPattern)".r

  def syntaxHighlighting(text: String): StyleSpans[util.Collection[String]] =
    val spansBuilder = StyleSpansBuilder[util.Collection[String]]()
    val allMatches = fullPattern.findAllMatchIn(text)

    val lastEnd = allMatches.foldLeft(0): (lastEnd, rMatch) =>
      val styleClass =
        Option(rMatch.group(operatorGroupName)).map(_ => "operator")
      spansBuilder.add(Collections.emptyList(), rMatch.start - lastEnd)
      spansBuilder.add(Collections.singleton(styleClass.orNull), rMatch.end - rMatch.start)
      rMatch.end

    spansBuilder.add(Collections.emptyList(), text.length - lastEnd)
    spansBuilder.create()

  plainTextChanges().subscribe: change =>
    val highlighting = syntaxHighlighting(getText)
    setStyleSpans(0, highlighting)