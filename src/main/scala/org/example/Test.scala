package org.example

import javapart.data.types.{Comparator, UserFactory, UserType}
import org.example.structure.CircularListScala
import org.example.structure.MyCounter

import scala.math.pow

object Test {
  def main(args: Array[String]) {

    val cl = new CircularListScala[Int]
    val p = UserFactory.getBuilderByName("Integer")

    for (i <- 20 until 0 by -1) {
      cl.add(i)
    }

    val myCounter = new MyCounter()

    //prints the original list
    System.out.println("\nOriginal list: ")
    System.out.println(cl.toString)

    //adds new item at the front of the list
    System.out.println("\naddFront(9)")
    cl.addFront(9)
    if (cl.getHeadData == 9) System.out.println("OK!")
    else System.out.println("FAIL!")

   //adds new item at the back of the list
   System.out.println("\naddBack(8)")
   cl.addBack(8)
   System.out.println(cl.toString)
   if (cl.getTailData == 8) System.out.println("OK!")
   else System.out.println("FAIL!")

   //inserts new item at the specified position
   System.out.println("\naddAtPosition(6,2)")
   cl.addAtPosition(6, 2)
   System.out.println(cl.toString)
   if (cl.getDataAtPosition(2) == 6) System.out.println("OK!")
   else System.out.println("FAIL!")

   //removes the specified item from the list
   System.out.println("\nremove(4)")
   cl.remove(4)
   System.out.println(cl.toString)
   if (cl.getDataAtPosition(4) != null) System.out.println("OK!")
   else System.out.println("FAIL!")

   //removes the front item from the list
   System.out.println("\nremoveFront()")
   cl.removeFront
   System.out.println(cl.toString)
   if (cl.getHeadData == 20) System.out.println("OK!")
   else System.out.println("FAIL!")

   //removes the last item  from the list
   System.out.println("\nremoveBack()")
   cl.removeBack
   System.out.println(cl.toString)
   if (cl.getTailData == 1) System.out.println("OK!")
   else System.out.println("FAIL!")

   //removes the item in the specified position
   System.out.println("\nremoveAtPosition(1)")
   cl.removeAtPosition(1)
   System.out.println(cl.toString)
   if (cl.getDataAtPosition(1) == 19) System.out.println("OK!")
   else System.out.println("FAIL!")


   System.out.println("\nsortList(comparator)")

   //sort list
   try {

     var clSorted = cl.mergeSort(p.getTypeComparator.asInstanceOf[Comparator[Int]], myCounter)
     System.out.println(clSorted.toString)
   }
   catch {
     case ignored: StackOverflowError =>
       System.err.println("Stack error")
   }

    test(p)
  }

  private def test(builder: UserType): Unit = {
    var i = 1
    while(i <= 512) {
      // System.out.println("i * i = " + (i*i))
      val n = i * 10000
      System.out.println("N = " + n)
      val list = new CircularListScala[AnyRef]
      for (j <- 0 until n.asInstanceOf[Int]) {
        list.add(builder.create)
      }
      val start = System.nanoTime()
      var myCounter = new MyCounter()
      try {
        var lst = list.mergeSort(builder.getTypeComparator, myCounter)
        System.out.println("Count: " + myCounter.get())
      }
      catch {
        case ignored: StackOverflowError =>
          System.err.println("Stack error")
          return
      }
      val end = System.nanoTime
      System.out.println("Seconds elapsed " + (end - start) * 0.000000001)
      i *= 2
    }
  }
}