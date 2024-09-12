package com.melvic.lohika.ui

import org.commonmark.parser.Parser
import org.commonmark.renderer.html.HtmlRenderer
import scalafx.scene.web.WebView

class SolutionsView extends WebView:
  val parser: Parser = Parser.builder().build()
  val renderer: HtmlRenderer = HtmlRenderer.builder().build()

  fontScale = 1.1

  def setSolutionContent(content: String): Unit =
    engine.loadContent:
      val document = parser.parse(content)
      renderer.render(document)
