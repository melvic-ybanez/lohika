package com.melvic.lohika.ui

import org.commonmark.parser.Parser
import org.commonmark.renderer.html.HtmlRenderer
import scalafx.scene.web.WebView

class SolutionsView extends WebView:
  val parser: Parser = Parser.builder().build()
  val renderer: HtmlRenderer = HtmlRenderer.builder().build()

  minWidth = 600
  minHeight = 600

  def setSolutionContent(content: String): Unit =
    engine.loadContent:
      val document = parser.parse(content)
      val htmlContent = renderer.render(document)
      s"""
         |<!DOCTYPE html>
         |<html>
         |<head>
         |  <meta charset="UTF-8">
         |  <meta name="viewport" content="width=device-width, initial-scale=1.0">
         |  <script id="MathJax-script" async
         |      src="https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"></script>
         |</head>
         |<body>
         |  <h3>Solution:</h3>
         |  <div class="two-column">
         |    $htmlContent
         |  </div>
         |</body>
         |</html>
         |""".stripMargin

  def init(): Unit =
    engine.setUserStyleSheetLocation(
      getClass.getResource("/css/webview.css").toExternalForm
    )