package com.melvic.lohika.ui

import org.commonmark.parser.Parser
import org.commonmark.renderer.html.HtmlRenderer
import scalafx.scene.web.WebView

class SolutionsView extends WebView:
  val parser: Parser = Parser.builder().build()
  val renderer: HtmlRenderer = HtmlRenderer.builder().build()

  minWidth = 600
  minHeight = 600

  def setSolutionContent(content: Either[String, (String, String)]): Unit =
    val htmlBody = content match
      case Left(errorMessage) => s"""<div class="centered error">$errorMessage<div>"""
      case Right(entailment, solution) =>
        val document = parser.parse(solution)
        val htmlContent = renderer.render(document)
        s"""
           |<div class="centered prove"><strong>Prove:</strong> <span class="entailment">$entailment</span></div>
           |
           |<h3 class="solution">Solution:</h3>
           |<div class="two-column">
           |  $htmlContent
           |</div>
           |""".stripMargin

    def liAnimation(nth: Int): String =
      s"""
         |li:nth-child($nth) {
         |  animation-delay: ${0.2 * nth + 0.4}s;
         |}
         |""".stripMargin

    def allLiAnimationStyles: String =
      val liCount = htmlBody.split("</li>").length - 1
      (1 to liCount).map(i => liAnimation(i)).mkString("\n")

    engine.loadContent:
      s"""
         |<!DOCTYPE html>
         |<html>
         |<head>
         |  <meta charset="UTF-8">
         |  <meta name="viewport" content="width=device-width, initial-scale=1.0">
         |  <script id="MathJax-script" async
         |      src="https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"></script>
         |  <style>$allLiAnimationStyles</style>
         |</head>
         |<body>$htmlBody</body>
         |</html>
         |""".stripMargin

  def init(): Unit =
    engine.setUserStyleSheetLocation(
      getClass.getResource("/css/webview.css").toExternalForm
    )