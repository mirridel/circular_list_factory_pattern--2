package org.example.structure

import javapart.data.Action
import javapart.data.types.Comparator

class CircularListScala[T] extends Serializable{
  var head: NodeScala[T] = null
  var tail: NodeScala[T] = null
  var location: NodeScala[T] = null
  var numElements = 0
  var found = false

  def isEmpty: Boolean = {
    head == null
  }

  def size: Int = {
    numElements
  }

  protected def find(target: T): Unit = {
    location = head
    found = false
    if (!isEmpty) do if (location.getData == target) { // if they match
      found = true
      return
    }
    else location = location.getNext while ( {
      location ne tail.getNext
    })
  }

  def contains(element: T): Boolean = {
    find(element)
    found
  }

  protected def findPosition(position: Int): Unit = { //finds position in this list
    // Assumes Zero based indexing
    var start = 0
    location = head
    found = false
    if ((!isEmpty) && (position >= 0) && (position <= size)) {
      do { // move search to the next node
        location = location.getNext
        start += 1
      } while ( {
        (location ne head) && start < position
      })
      found = true
    }
  }

  def add(data: T): Unit = { // Adds element to this list.
    val newNode = new NodeScala[T](data) // Reference to the new node being added
    if (isEmpty) { // Adding into an empty list
      head = newNode
      tail = newNode
      head.setPrevious(tail)
      tail.setNext(head)
    }
    else { // Adding into a non-empty list
      tail.setNext(newNode)
      newNode.setPrevious(tail)
      tail = newNode
      tail.setNext(head)
    }
    numElements += 1
  }

  def addFront(data: T): Unit = { //adds new element to the front of the list
    val newNode = new NodeScala[T](data)
    if (isEmpty) {
      head = newNode
      tail = newNode
      head.setPrevious(tail)
      tail.setNext(head)
    }
    else {
      newNode.setNext(head)
      head.setPrevious(newNode)
      head = newNode
      head.setPrevious(tail)
      tail.setNext(head)
    }
    numElements += 1
  }

  def addBack(data: T): Unit = { //adds new element to the back of the list
    val newNode = new NodeScala[T](data)
    if (isEmpty) {
      head = newNode
      tail = newNode
      head.setPrevious(tail)
      tail.setNext(head)
    }
    else {
      tail.setNext(newNode)
      newNode.setPrevious(tail)
      tail = newNode
      tail.setNext(head)
    }
    numElements += 1
  }

  def addAtPosition(data: T, position: Int): Unit = { //adds new element to the specified position
    val newNode = new NodeScala[T](data)
    if (isEmpty) { // add element to an empty list
      head = newNode
      tail = newNode
      head.setPrevious(tail)
      tail.setNext(head)
    }
    else if (position <= 0) { // insert at front of the list
      newNode.setNext(head)
      head.setPrevious(newNode)
      head = newNode
      head.setPrevious(tail)
      tail.setNext(head)
    }
    else if (position >= size) { // if position does not exist, perform add at the most efficient
      // position for circular doubly linked list, the most efficient position is
      // at the end.
      tail.setNext(newNode)
      newNode.setPrevious(tail)
      tail = newNode
      tail.setNext(head)
    }
    else {
      /* General Case */
      // determine location where to perform insert
      findPosition(position)
      //inserts the elements to the specified position
      newNode.setPrevious(location.getPrevious)
      newNode.setNext(location)
      location.getPrevious.setNext(newNode)
      location.setPrevious(newNode)
    }
    numElements += 1
  }

  def remove(element: T): Boolean = { // Removes an element e from this list such that e.equals(element)
    // and returns true; if no such element exists, returns false.
    find(element)
    if (found) {
      if ((location eq head) && size == 1) { //removes the only existing element
        //empties the list
        head = null
        tail = null
      }
      else if (location eq head) { //removes first node
        head = head.getNext
        head.setPrevious(tail)
        tail.setNext(head)
      }
      else if (location eq tail) { //removes last node
        tail = tail.getPrevious
        tail.setNext(head)
        head.setPrevious(tail)
      }
      else { // removes node at location
        location.getPrevious.setNext(location.getNext)
        location.getNext.setPrevious(location.getPrevious)
      }
      numElements -= 1
    }
    found
  }

  def removeFront(): Unit = {
    if (!isEmpty) if (head.getNext eq head) {
      head = null
      tail = null
    }
    else {
      head = head.getNext
      head.setPrevious(tail)
      tail.setNext(head)
    }
    numElements -= 1
  }

  def removeBack(): Unit = {
    if (!isEmpty) if (head.getNext eq head) {
      head = null
      tail = null
    }
    else {
      tail = tail.getPrevious
      tail.setNext(head)
      head.setPrevious(tail)
    }
    numElements -= 1
  }

  def removeAtPosition(position: Int): Unit = {
    if (position <= 0) {
      head = head.getNext
      head.setPrevious(tail)
      tail.setNext(head)
    }
    else if (position >= size - 1) {
      tail = tail.getPrevious
      tail.setNext(head)
      head.setPrevious(tail)
    }
    else {
      findPosition(position)
      location.getPrevious.setNext(location.getNext)
      location.getNext.setPrevious(location.getPrevious)
    }
    numElements -= 1
  }

  def getHeadData: T = head.getData

  def getTailData: T = tail.getData

  def getDataAtPosition(position: Int): T = { //adds new element to the specified position
    if (position <= 0) getHeadData
    else if (position >= size) getTailData
    else {
      findPosition(position)
      location.getData
    }
  }

  override def toString: String = {
    var item = "List: [ "
    var current = head
    if (!isEmpty) do {
      item += current.getData + " "
      current = current.getNext
    } while ( {
      current ne head
    })
    item += "]"
    item
  }

  def printReverse: String = {
    var item = "List: [ "
    var current = tail
    if (!isEmpty) do {
      item += current.getData + " "
      current = current.getPrevious
    } while ( {
      current ne tail
    })
    item += "]"
    item
  }

  def forEach(a: Action[T]): Unit = {
    var tmp = head
    for (i <- 0 until this.size) {
      a.toDo(tmp.getData.asInstanceOf[T])
      tmp = tmp.getNext
    }
  }

  def sort(comparator: Comparator[T]): Unit = { //split the list
    tail.setNext(null)
    head = mergeSort(head, comparator)
    //close the list
    tail = head
    while ( {
      tail.getNext != null
    }) {
      tail = tail.getNext
    }
    tail.setNext(head)
    head.setPrevious(tail)
  }

  def mergeSort(h: NodeScala[T], comparator: Comparator[T]): NodeScala[T] = {
    if (h == null || h.getNext == null) {
      return h
    }
    val middle: NodeScala[T] = getMiddle(h)
    val middleNext: NodeScala[T] = middle.getNext
    middle.setNext(null)
    val left: NodeScala[T] = mergeSort(h, comparator)
    val right: NodeScala[T] = mergeSort(middleNext, comparator)
    return merge(left, right, comparator)
  }

  def merge(head11: NodeScala[T], head22: NodeScala[T], comparator: Comparator[T]): NodeScala[T] = {
    var left: NodeScala[T] = head11
    var right: NodeScala[T] = head22
    val merged: NodeScala[T] = new NodeScala[T](null.asInstanceOf[T])
    var temp: NodeScala[T] = merged
    while ( {
      left != null && right != null
    }) {
      if (comparator.compare(left.getData, right.getData) < 0) {
        temp.setNext(left)
        left.setPrevious(temp)
        left = left.getNext
      }
      else {
        temp.setNext(right)
        right.setPrevious(temp)
        right = right.getNext
      }
      temp = temp.getNext
    }
    while ( {
      left != null
    }) {
      temp.setNext(left)
      left.setPrevious(temp)
      left = left.getNext
      temp = temp.getNext
    }
    while ( {
      right != null
    }) {
      temp.setNext(right)
      right.setPrevious(temp)
      right = right.getNext
      temp = temp.getNext
      this.tail = temp
    }
    return merged.getNext
  }

  def getMiddle(h: NodeScala[T]): NodeScala[T] = {
    if (h == null) {
      return null
    }
    var fast: NodeScala[T] = h.getNext
    var slow: NodeScala[T] = h
    while ( {
      fast != null
    }) {
      fast = fast.getNext
      if (fast != null) {
        slow = slow.getNext
        fast = fast.getNext
      }
    }
    return slow
  }
}