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
         |      font-family: "Open Sans";
         |      color: #4B2E2A;
         |    }
         |    h3 {
         |      color: #4B2E2A
         |    }
         |    a {
         |      font-style: italic;
         |      text-decoration: none;
         |      color: #008080;
         |    }
         |    a:hover {
         |      color: #CC5500;
         |    }
         |    .formula {
         |      color: #4A5D23
         |    }
         |  </style>
         |  <script src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"></script>
         |</head>
         |<body>
         |  $htmlContent
         |</body>
         |</html>
         |""".stripMargin
