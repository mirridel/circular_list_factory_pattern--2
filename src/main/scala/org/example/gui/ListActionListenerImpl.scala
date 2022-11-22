package org.example.gui

import org.example.structure.CircularListScala
import javapart.gui.AbstractListActionListener

import java.io._


class ListActionListenerImpl extends AbstractListActionListener {
  protected var items: CircularListScala[AnyRef] = new CircularListScala[AnyRef]

  override def onCreate(): Unit = {
    val data: AnyRef = builder.create
    items.add(data)
    listModel.addElement(data)
  }

  override def onAdd(text: String): Unit = {
    if (text.isEmpty) {
      return
    }
    val data: AnyRef = builder.parseValue(text)
    items.add(data)
    listModel.addElement(data)
  }

  override def onInsert(text: String, index: Int): Unit = {
    if (text.isEmpty) {
      return
    }
    val data: AnyRef = builder.parseValue(text)
    items.addAtPosition(data, index)
    listModel.add(index, data)
  }

  override def onRemove(index: Int): Unit = {
    items.removeAtPosition(index)
    listModel.remove(index)
  }

  override def onSort(): Unit = {
    items.sort(builder.getTypeComparator)
    listModel.clear()
    items.forEach(el => listModel.addElement(el))
  }

  override def onSave(): Unit = {
    try {
      val oos: ObjectOutputStream = new ObjectOutputStream(new FileOutputStream("save.dat"))
      try oos.writeObject(items)
      catch {
        case ex: Exception =>
          System.out.println(ex.getMessage)
      } finally {
        if (oos != null) oos.close()
      }
    }
  }

  override def onLoad(): Unit = {
    try {
      val ois: ObjectInputStream = new ObjectInputStream(new FileInputStream("save.dat"))
      try {
        items = ois.readObject.asInstanceOf[CircularListScala[AnyRef]]
        listModel.clear()
        items.forEach(el => listModel.addElement(el))
      } catch {
        case ex: Exception =>
          System.out.println(ex.getMessage)
      } finally {
        if (ois != null) ois.close()
      }
    }
  }
}
