package org.example.structure

class MyCounter(var count: Long = 0) {
  def inc(): Unit = {
    count += 1
  }

  def inc(n: Long): Unit = {
    count += n
  }

  def get(): Long = {
    count
  }
}
