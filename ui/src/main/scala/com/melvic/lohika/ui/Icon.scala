package com.melvic.lohika.ui

import scalafx.scene.Cursor
import scalafx.scene.control.Button
import scalafx.scene.image.{Image, ImageView}

class Icon(imagePath: String) extends Button:
  val iconImageView = ImageView(Image(Resources.iconPath(imagePath)))
  val iconImageHoverView = ImageView(Image(Resources.iconPath(s"hovers/$imagePath")))

  graphic = iconImageView
  styleClass += "editor-icon"

  onMouseEntered = _ =>
    cursor = Cursor.Hand
    graphic = iconImageHoverView
  onMouseExited = _ =>
    cursor = Cursor.Default
    graphic = iconImageView
