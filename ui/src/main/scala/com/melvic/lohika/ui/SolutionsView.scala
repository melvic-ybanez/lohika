package com.melvic.lohika.ui

import org.commonmark.parser.Parser
import org.commonmark.renderer.html.HtmlRenderer
import scalafx.scene.web.WebView

class SolutionsView extends WebView:
  val parser: Parser = Parser.builder().build()
  val renderer: HtmlRenderer = HtmlRenderer.builder().build()

  prefWidth = 600
  prefHeight = 600

  def setSolutionContent(content: String): Unit =
    engine.loadContent:
      val document = parser.parse(content)
      val htmlContent = renderer.render(document)
      s"""
         |<html>
         |<head>
         |  <style>
         |    body {
         |      font-size: 1.2em;
         |    }
         |    a {
         |      text-decoration: none;
         |      font-style: italic;
         |      color: #007BFF;
         |    }
         |    a:hover {
         |      color: #0056b3;
         |    }
         |  </style>
         |</head>
         |<body>
         |  $htmlContent
         |</body>
         |</html>
         |""".stripMargin
