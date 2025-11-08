package com.melvic.lohika.ui

import com.melvic.lohika.controllers.Eval
import org.commonmark.parser.Parser
import org.commonmark.renderer.html.HtmlRenderer
import scalafx.scene.web.WebView

class SolutionsView extends WebView:
  lazy val parser: Parser = Parser.builder().build()
  lazy val renderer: HtmlRenderer = HtmlRenderer.builder().build()

  minWidth = 600
  minHeight = 600

  def setSolutionContent(content: Eval.Result): Unit =
    val htmlBody = content match
      case Left(errorMessage) => s"""<div class="centered error">$errorMessage<div>"""
      case Right(entailment, solution) =>
        val entailmentContent = renderer.render(parser.parse(entailment))
        val proofContent = renderer.render(parser.parse(s"_Proof._ $solution"))

        s"""
           |<div class="problem-statement">
           |  <h3 class="problem-caption">Show that the following holds:</h3>
           |  <div class="entailment">$entailmentContent</div>
           |</div>
           |
           |<div class="proof-content">
           |  $proofContent
           |</div>
           |""".stripMargin

    def liAnimation(nth: Int): String =
      s"""
         |li:nth-child($nth) {
         |  animation-delay: ${0.2 * nth + 1.2}s;
         |}
         |""".stripMargin

    def allLiAnimationStyles: String =
      val liCount = htmlBody.split("</li>").length - 1
      (1 to liCount).map(liAnimation).mkString("\n")

    engine.loadContent:
      s"""
         |<!DOCTYPE html>
         |<html>
         |<head>
         |  <meta charset="UTF-8">
         |  <meta name="viewport" content="width=device-width, initial-scale=1.0">
         |  <script async src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-svg.js"></script>
         |  <style>$allLiAnimationStyles</style>
         |</head>
         |<body>$htmlBody</body>
         |</html>
         |""".stripMargin

  def init(): Unit =
    engine.setUserStyleSheetLocation(Resources.cssPath("solutions_view.css"))
