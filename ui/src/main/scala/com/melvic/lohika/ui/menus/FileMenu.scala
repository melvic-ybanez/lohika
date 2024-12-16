package com.melvic.lohika.ui.menus

import com.melvic.lohika.ui.{Config, MainScene}
import com.melvic.lohika.ui.events.FileEventHandler
import scalafx.scene.control.{Menu, MenuItem}
import scalafx.scene.input.{KeyCode, KeyCodeCombination, KeyCombination}

class FileMenu(mainScene: MainScene) extends Menu("File"):
  val fileEventHandler = FileEventHandler(mainScene)

  val newMenuItem: MenuItem = new MenuItem(s"New ${Config.AppName} Script"):
    onAction = _ => mainScene.editorTabPane.newUntitled()
    accelerator = KeyCodeCombination(KeyCode.N, KeyCombination.ShortcutDown)

  val openMenuItem: MenuItem = new MenuItem("Open..."):
    onAction = _ => fileEventHandler.open()
    accelerator = KeyCodeCombination(KeyCode.O, KeyCombination.ShortcutDown)

  val saveMenuItem: MenuItem = new MenuItem("Save..."):
    onAction = _ => fileEventHandler.save()
    accelerator = KeyCodeCombination(KeyCode.S, KeyCombination.ShortcutDown)

  val saveAsMenuItem: MenuItem = new MenuItem("Save As..."):
    onAction = _ => fileEventHandler.saveAs()
    accelerator =
      KeyCodeCombination(KeyCode.S, KeyCombination.ShortcutDown, KeyCombination.ShiftDown)

  items = List(newMenuItem, openMenuItem, saveMenuItem, saveAsMenuItem)
