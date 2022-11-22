package org.example

import javapart.data.types.{UserFactory, UserInteger, UserType}
import javapart.gui.UI
import org.example.gui.ListActionListenerImpl
import org.example.structure.CircularListScala

object App {
  def main(args: Array[String]) {
    val listActionListener = new ListActionListenerImpl
    new UI(listActionListener)
  }
}
