package org.example.structure

class NodeScala[T] extends Serializable{
  protected var next: NodeScala[T] = null
  protected var previous: NodeScala[T] = null
  protected var data: T = null.asInstanceOf[T]

  def this(data: T) {
    this()
    this.data = data
    next = null
    previous = null
  }

  def getNext: NodeScala[T] = {
    return next
  }

  def setNext(next: NodeScala[T]): Unit = {
    this.next = next
  }

  def getData: T = {
    return data
  }

  def setData(data: T): Unit = {
    this.data = data
  }

  def getPrevious: NodeScala[T] = {
    return previous
  }

  def setPrevious(previous: NodeScala[T]): Unit = {
    this.previous = previous
  }
}
