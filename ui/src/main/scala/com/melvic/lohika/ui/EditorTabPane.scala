package com.melvic.lohika.ui

import javafx.scene.control.Tab
import scalafx.Includes.*
import scalafx.application.Platform
import scalafx.beans.property.{IntegerProperty, StringProperty}
import scalafx.scene.control.TabPane

class EditorTabPane(mainScene: MainScene) extends TabPane:
  val untitledCount: IntegerProperty = IntegerProperty(0)

  def newUntitled(): Unit =
    untitledCount.value += 1
    newTab(s"Untitled${untitledCount.get()}${mainScene.fileManager.fileExtension}", "", "")

  def newTab(title: String, script: String, path: String): Unit =
    val tab = EditorTab(mainScene)
    tab.setText(title)
    tab.editorPane.withContent(script)
    tab.pathProp.set(path)
    tabs.addOne(tab)
    selectionModel.value.select(tab)

  def withSelectedTitle(title: String): EditorTabPane =
    selectedEditorTab.text = title
    this

  def withSelectedContent(content: String): EditorTabPane =
    selectedEditorTab.editorPane.withContent(content)
    this

  def selectedEditorTab: EditorTab =
    selectionModel.value.getSelectedItem.asInstanceOf[EditorTab]

  def selectedPathProp: StringProperty =
    selectedEditorTab.pathProp

  def selectedTitleProp: StringProperty =
    selectedEditorTab.textProperty()

  def updateTabTitle(title: String): Unit =
    selectedTitleProp.set(title)
    mainScene.runnableFileNameProp.set(title)

class EditorTab(mainScene: MainScene) extends Tab:
  val editorPane = EditorPane(mainScene)
  val pathProp: StringProperty = StringProperty("")

  setContent(editorPane)
  setOnCloseRequest: _ =>
    if getTabPane.getTabs.size == 1 then Platform.exit()

  setOnSelectionChanged: _ =>
    mainScene.scriptProp <== editorPane.editorArea.textProperty()
