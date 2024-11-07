package com.melvic.lohika.ui

import cats.implicits.*
import com.melvic.lohika.prover.interpreters.LiveProver.{Steps, given}
import com.melvic.lohika.prover.programs.ProverProgram
import com.melvic.lohika.ui.symbols.{MathJax, Unicode}
import org.fxmisc.richtext.CodeArea
import scalafx.Includes.*
import scalafx.beans.property.StringProperty
import scalafx.geometry.Orientation
import scalafx.scene.{Cursor, Scene}
import scalafx.scene.control.{Button, SplitPane}
import scalafx.scene.effect.ColorInput
import scalafx.scene.image.{Image, ImageView}
import scalafx.scene.layout.{AnchorPane, BorderPane}

class MainScene extends Scene:
  val entailmentProp = new StringProperty("")
  val solutionsView = SolutionsView()

  solutionsView.init()
  stylesheets.add(Resources.cssPath("main.css"))

  root = new SplitPane:
    styleClass = Seq("editor-split")
    orientation = Orientation.Vertical

    items.addAll(
      new AnchorPane:
        val runButton = Icon("run.png")
        val editorArea = new EditorView:
          entailmentProp <== textProperty()

        children.addAll(editorArea, runButton)

        AnchorPane.setTopAnchor(runButton, 10.0)
        AnchorPane.setRightAnchor(runButton, 20.0)
        AnchorPane.setTopAnchor(editorArea, 20.0)
        AnchorPane.setRightAnchor(editorArea, 0.0)
        AnchorPane.setLeftAnchor(editorArea, 0.0)
        AnchorPane.setBottomAnchor(editorArea, 0.0)

        runButton.onAction = _ => handleInput()
      ,
      solutionsView
    )

    def handleInput(): Unit =
      val rawEntailment = Unicode.removeFromText(entailmentProp.value)
      ProverProgram.prove[Steps](rawEntailment).run match
        case Left(error) => solutionsView.setSolutionContent(Left(error))
        case Right(steps, (entailment, _)) =>
          val mdSteps = steps.map: step =>
            if step.endsWith(".") || step.endsWith(":") || step.trim.startsWith("*") then step
            else step + "."
          val entailmentElem = MathJax.applyToText(entailment.show)
          val solution = MathJax.applyToText(mdSteps.mkString("\n\n"))
          solutionsView.setSolutionContent(Right(entailmentElem, solution))

class EditorView extends CodeArea:
  getStyleClass.add("editor")

class Icon(imagePath: String) extends Button:
  val iconImageView = ImageView(Image(Resources.iconPath(imagePath)))
  val iconImageHoverView = ImageView(Image(Resources.iconPath(s"hovers/$imagePath")))

  graphic = iconImageView
  styleClass += "editor-icon"

  onMouseEntered = _ =>
    cursor = Cursor.Hand
    graphic = iconImageHoverView
  onMouseExited = _ =>
    cursor = Cursor.Default
    graphic = iconImageView
