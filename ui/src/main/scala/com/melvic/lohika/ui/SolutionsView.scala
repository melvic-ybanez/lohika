package com.melvic.lohika.ui

import com.melvic.lohika.controllers.Eval
import com.melvic.lohika.core.Formatter
import com.melvic.lohika.core.Formatter.sentence
import org.commonmark.parser.Parser
import org.commonmark.renderer.html.HtmlRenderer
import scalafx.scene.web.WebView

import scala.annotation.tailrec

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
            s"<span class='solution-label'>${"Entailment".sentence}</span> ${entailment.sentence}"
          )
        )
        val proofContent = renderer.render(
          parser.parse(s"<span class='solution-label'>${"Proof".sentence}</span> $solution")
        )

        s"""
           |<div id="entailment-pane" class="proof-paragraph">
           |  $entailmentContent
           |</div>
           |
           |<div id="proof-pane" class="proof-paragraph">
           |  $proofContent
           |</div>
           |""".stripMargin

    def sentenceAnimations: String =
      val sentenceCount = SolutionsView.countSentences(htmlBody)

      // suppose there are only 2 sentences in the entailment pane
      val entailmentSentenceCount = 2

      val delayPerSentence = 0.3

      val entailmentAnimations = (1 to entailmentSentenceCount).map: nth =>
        s"""
           |#entailment-pane .sentence:nth-child($nth) {
           |  animation-delay: ${delayPerSentence * nth}s;
           |}
           |""".stripMargin

      val proofAnimations = (1 to sentenceCount - entailmentSentenceCount).map: nth =>
        s"""
           |#proof-pane .sentence:nth-child($nth) {
           |  animation-delay: ${delayPerSentence * nth + delayPerSentence * entailmentSentenceCount}s;
           |}
           |""".stripMargin

      entailmentAnimations.mkString("\n") + "\n" + proofAnimations.mkString("\n")

    engine.loadContent:
      s"""
         |<!DOCTYPE html>
         |<html>
         |<head>
         |  <meta charset="UTF-8">
         |  <meta name="viewport" content="width=device-width, initial-scale=1.0">
         |  <script async src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-svg.js"></script>
         |  <style>$sentenceAnimations</style>
         |</head>
         |<body>$htmlBody</body>
         |</html>
         |""".stripMargin

  def init(): Unit =
    engine.setUserStyleSheetLocation(Resources.cssPath("solutions_view.css"))

object SolutionsView:
  private def countSentences(paragraph: String): Int =
    val keyword = "<span class='sentence'>"

    @tailrec
    def recurse(paragraph: String, count: Int): Int =
      if paragraph.isEmpty then count
      else if paragraph.startsWith(keyword) then recurse(paragraph.drop(keyword.length), count + 1)
      else recurse(paragraph.tail, count)

    recurse(paragraph, 0)
