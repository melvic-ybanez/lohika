package com.melvic.lohika.ui

import com.melvic.lohika.controllers.Eval
import com.melvic.lohika.core.Formatter
import com.melvic.lohika.core.Formatter.sentence
import org.commonmark.parser.Parser
import org.commonmark.renderer.html.HtmlRenderer
import scalafx.scene.web.WebView

class SolutionsView extends WebView:
  lazy val parser: Parser = Parser.builder().build()
  lazy val renderer: HtmlRenderer = HtmlRenderer.builder().build()

  minWidth = 600
  minHeight = 600

  def setSolutionContent(content: Eval.Result)(using formatter: Formatter): Unit =
    val htmlBody = content match
      case Left(errorMessage) => s"""<div class="centered error">$errorMessage<div>"""
      case Right(entailment, solution) =>
        val entailmentContent = renderer.render(
          parser.parse(
            s"<span class='solution-label'>${"Proposition".sentence}</span> ${entailment.sentence}"
          )
        )
        val proofContent = renderer.render(
          parser.parse(s"<span class='solution-label'>${"Proof".sentence}</span> $solution")
        )

        s"""
           |<div class="proof-content">
           |  $entailmentContent
           |</div>
           |
           |<div class="proof-content">
           |  $proofContent
           |</div>
           |""".stripMargin

    def sentenceAnimation(nth: Int): String =
      s"""
         |.sentence:nth-child($nth) {
         |  animation-delay: ${0.2 * nth}s;
         |}
         |""".stripMargin

    def allSentenceAnimationStyles: String =
      val sentenceCount = htmlBody.split("<span class='sentence'>").length - 1
      (1 to sentenceCount).map(sentenceAnimation).mkString("\n")

    engine.loadContent:
      s"""
         |<!DOCTYPE html>
         |<html>
         |<head>
         |  <meta charset="UTF-8">
         |  <meta name="viewport" content="width=device-width, initial-scale=1.0">
         |  <script async src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-svg.js"></script>
         |  <style>$allSentenceAnimationStyles</style>
         |</head>
         |<body>$htmlBody</body>
         |</html>
         |""".stripMargin

  def init(): Unit =
    engine.setUserStyleSheetLocation(Resources.cssPath("solutions_view.css"))
