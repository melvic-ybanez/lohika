package com.melvic.lohika.ui

import org.commonmark.parser.Parser
import org.commonmark.renderer.html.HtmlRenderer
import scalafx.Includes.when
import scalafx.collections.ObservableBuffer
import scalafx.geometry.Insets
import scalafx.scene.control.{ListCell, ListView}
import scalafx.scene.layout.{BorderPane, StackPane}
import scalafx.scene.web.WebView

class HistoryView(items: ObservableBuffer[String]) extends ListView(items):
  cellFactory = (_: ListView[String]) =>
    new ListCell[String]:
      styleClass += "dark"
      padding = Insets(0)

      val itemView = new WebView
      itemView.engine.setUserStyleSheetLocation(
        getClass.getResource("/css/history-view.css").toExternalForm
      )

      item.onChange: (_, _, newValue) =>
        if newValue != null then
          val content =
            HtmlRenderer.builder().build().render(Parser.builder().build().parse(newValue))
          itemView.engine.loadContent:
            s"""
             |<html>
             |<head><script async src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-svg.js"></script></head>
             |</html>
             |<body>
             |  $content
             |</body>
             |""".stripMargin
          graphic = itemView
        else graphic = null

        hover.onChange: (_, _, isHovered) =>
          val bgColor = if isHovered then "4D4D56" else "333333"
          itemView.engine.executeScript(s"document.body.style.backgroundColor = '#$bgColor';")
