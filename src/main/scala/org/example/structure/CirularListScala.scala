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

  def subList(min: Integer, max: Integer): CircularListScala[T] ={
    var lst = new CircularListScala[T];
    var n = max - min
    for (w <- 0 until n) {
      lst.addBack(this.getDataAtPosition(min+w))
    }
    return lst
  }

  def mergeSort(comparator: Comparator[T]): CircularListScala[T] = {

    var mid = size / 2
    if (mid == 0)
      return this
    merge(subList(0, mid).mergeSort(comparator), subList(mid, size).mergeSort(comparator), new CircularListScala[T], comparator)
  }

  def addAll(input: CircularListScala[T]): Unit = {
    for (w <- 0 until input.size) {
      this.addBack(input.getDataAtPosition(w))
    }
  }

  private def merge(first: CircularListScala[T], second: CircularListScala[T], accumulator: CircularListScala[T], comparator: Comparator[T]): CircularListScala[T] = {
    if (first.isEmpty) {
      accumulator.addAll(second)
    }
    else if (second.isEmpty) {
      accumulator.addAll(first)
    }
    else {
      if (comparator.compare(first.getDataAtPosition(0), second.getDataAtPosition(0)) < 0) {
        accumulator.addBack(first.getDataAtPosition(0))
        return merge(first.subList(1, first.size), second, accumulator, comparator)
      }
      else {
        accumulator.addBack(second.getDataAtPosition(0));
        return merge(first, second.subList(1, second.size), accumulator, comparator)
      }
    }
    return accumulator
  }
}