package com.melvic.lohika.ui

import javafx.scene.input.KeyCode
import org.fxmisc.flowless.VirtualizedScrollPane
import org.fxmisc.richtext.CodeArea
import org.fxmisc.richtext.model.{StyleSpans, StyleSpansBuilder}
import scalafx.Includes.*
import scalafx.scene.control.Label
import scalafx.scene.input.KeyEvent
import scalafx.scene.layout.AnchorPane

import java.util
import java.util.Collections
import scala.util.matching.Regex

class EditorPane(mainScene: MainScene) extends AnchorPane:
  stylesheets += Resources.cssPath("editor")

  val runButton: Icon = new Icon("run.png"):
    onAction = _ => mainScene.handleInput()

  val editorArea: EditorView = new EditorView:
    mainScene.entailmentProp <== textProperty()

  val editorAreaPane = new VirtualizedScrollPane(editorArea)

  children.addAll(editorAreaPane, runButton)

  AnchorPane.setTopAnchor(runButton, 10.0)
  AnchorPane.setRightAnchor(runButton, 20.0)
  AnchorPane.setTopAnchor(editorAreaPane, 20.0)
  AnchorPane.setRightAnchor(editorAreaPane, 0.0)
  AnchorPane.setLeftAnchor(editorAreaPane, 0.0)
  AnchorPane.setBottomAnchor(editorAreaPane, 0.0)

class EditorView extends CodeArea with SyntaxHighlighting:
  init()

  def init(): Unit =
    getStyleClass.add("editor")

  plainTextChanges().subscribe: change =>
    val highlighting = syntaxHighlighting(getText)
    setStyleSpans(0, highlighting)
    setParagraphGraphicFactory: line =>
      val linesCount = getParagraphs.size()
      val lineText = (line + 1).toString
      val paddingLength = linesCount.toString.length - lineText.length
      new Label(" " * paddingLength + lineText):
        style = s"""
             |-fx-background-color: #333333;
             |-fx-text-fill: #88E7DC;
             |-fx-padding: 0 30px 0 0;
             |-fx-font-family: Monospaced;
             |""".stripMargin

  addEventHandler(
    KeyEvent.KeyPressed,
    event =>
      if event.getCode() == KeyCode.ENTER then
        val previousLine = getParagraph(getCurrentParagraph - 1).getText

        val prevIndent = previousLine.takeWhile(_.isWhitespace)
        val newIndent = if previousLine.trim.isEmpty then "" else " " * 4
        insertText(getCaretPosition, prevIndent + newIndent)
  )

private trait SyntaxHighlighting:
  object GroupNames:
    val Quantifiers = "QUANTIFIERS"
    val Operator = "OPERATOR"
    val Parens = "PARENS"

  object Patterns:
    val Quantifiers = raw"(A:|E:)"
    val Operator = raw"(<=>|=>|&|!|\|)"
    val Parens = raw"(\(|\)|\[|\])"

  val fullPattern: Regex =
    val rTable = List(
      GroupNames.Operator -> Patterns.Operator,
      GroupNames.Parens -> Patterns.Parens,
      GroupNames.Quantifiers -> Patterns.Quantifiers
    )
    rTable.map((op, pat) => s"(?<$op>$pat)").mkString("|").r

  def syntaxHighlighting(text: String): StyleSpans[util.Collection[String]] =
    val spansBuilder = StyleSpansBuilder[util.Collection[String]]()
    val allMatches = fullPattern.findAllMatchIn(text)

    val lastEnd = allMatches.foldLeft(0): (lastEnd, rMatch) =>
      val styleClass =
        Option(rMatch.group(GroupNames.Operator))
          .map(_ => "operator")
          .orElse(Option(rMatch.group(GroupNames.Parens)).map(_ => "parens"))
          .orElse(Option(rMatch.group(GroupNames.Quantifiers)).map(_ => "quantifiers"))
      spansBuilder.add(Collections.emptyList(), rMatch.start - lastEnd)
      spansBuilder.add(Collections.singleton(styleClass.orNull), rMatch.end - rMatch.start)
      rMatch.end

    spansBuilder.add(Collections.emptyList(), text.length - lastEnd)
    spansBuilder.create()