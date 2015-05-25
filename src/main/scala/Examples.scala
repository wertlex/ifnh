package com.github.wertlex

import java.util.Date
import play.api.libs.iteratee._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object Examples {

  def code1(): Int = {
    val l = List(1, 234, 455, 987)

    var total = 0 // will contain the final total
    var it = l.iterator
    while( it.hasNext ) {
      total += it.next
    }

    total
  }

  def code2(): Int = {
    val l = List(1, 234, 456, 789)
    var total = 0
    l.foreach( item => total += item)
    total
  }

  def code3(): Int = {
    val l   = List(1, 234, 456, 789)
    val l2  = List(2, 3, 4, 5)
    var total = 0
    def step(item: Int) = total += item

    l foreach step
    total = 0
    l2 foreach step
    total
  }

  def code4(): Int = {
    def foreach(l: List[Int]): Int = {
      def step(l: List[Int], total: Int): Int = {
        l match {
          case List()       => total
          case List(elt)    => total + elt
          case head :: tail => step(tail, total + head)
        }
      }

      step(l, 0)
    }

    val l = List(123, 456, 789, 10)
    foreach(l)
  }

  def code5() = {
    val stringEnumerator:   Enumerator[String]  = Enumerator("alpha", "beta", "gamma")
    val integerEnumerator:  Enumerator[Int]     = Enumerator(1, 2, 3, 4)
    val doubleEnumerator:   Enumerator[Double]  = Enumerator(1.0, 2.0, 3.0, 4.0)

    /** Requires file */
//    val fileEnumerator:     Enumerator[Array[Byte]] = Enumerator.fromFile(new java.io.File("myfile.txt"))

    val dateGenerator: Enumerator[String] = Enumerator.generateM {
      Future{
        Thread.sleep(500)
        Some("current time %s".format(new Date()))
      }
    }

  }

  def code6() = {
    case class Pizza(name: String)
    val pizza = Pizza("napolitana")
    val enumerator: Enumerator[Pizza] = Enumerator.enumInput(Input.El(pizza))
    val emptyEnumerator: Enumerator[Pizza] = Enumerator.enumInput(Input.Empty)
  }


  def code7() = {
    val iterator: Iteratee[Int, Int] = Iteratee.fold(0){ (total, elt) => total + elt}
    val e1 = Enumerator(1, 2, 3, 4)
    val e2 = Enumerator(56, 7, 8, 9)

    val r1 = e1(iterator)
    val r2 = e2(iterator)

    val r3 = e1.run(iterator)
    val r4 = e2.run(iterator)
  }

  def code8(): Future[Int] = {
    val iterator: Iteratee[Int, Int] = Iteratee.fold(0){ (total, elt) => total + elt }
    val e: Enumerator[Int] = Enumerator(1, 2, 3, 4)
    e.run(iterator)
  }

  def code9() = {
    val e = Enumerator(1, 2, 3, 4)
    e(Iteratee.foreach( println _))
  }

  def code10(): Future[List[Int]] = {
    val e = Enumerator(1, 2, 3, 4)
    val list: Future[List[Int]] = e run Iteratee.getChunks[Int]
    list
  }

  def code11(): Future[Int] = {
    def total2Chunks: Iteratee[Int, Int] = {
      def step(idx: Int, total: Int)(i: Input[Int]): Iteratee[Int, Int] = i match {
        case Input.EOF | Input.Empty => Done(total, Input.EOF)
        case Input.El(e)  =>
          if(idx < 2) Cont[Int, Int](i => step(idx + 1, total + e)(i))
          else Done(total, Input.EOF)
      }
      Cont[Int, Int](i => step(0, 0)(i))
    }

    val e = Enumerator(1, 2, 3)
    e run total2Chunks
  }

  def code12() = {
    val e: Enumerator[Int]            = Enumerator(1, 2, 3, 4)
    val i: Iteratee[Int, Int]         = Iteratee.fold(0){ (total, elt) => total + elt}
    val p: Future[Iteratee[Int, Int]] = e.apply(i)

    val it = Iteratee.flatten(p)

    val p1: Future[Iteratee[Int, Int]] = Future(i)
    val p2: Future[Iteratee[Int, Int]] = i.unflatten.map(_.it)

  }

  def code13() = {
    val enumerator = Enumerator(1, 2, 3, 4)
    val iteratee: Iteratee[String, List[String]] = Iteratee.getChunks[String]
    val list: Future[List[String]] = enumerator through Enumeratee.map(_.toString) run iteratee
  }

  def code14() = {
    val enumerator =  Enumerator(1, 2, 3, 4)
    val stringEnumerator: Enumerator[String] = enumerator through Enumeratee.map(_.toString)
  }

  def code15() = {
    val stringIteratee: Iteratee[String, List[String]] = Iteratee.getChunks[String]

    val enumeratee: Enumeratee[Int, String] = Enumeratee.map[Int].apply[String](_.toString)
    val intIteratee = enumeratee.transform(stringIteratee)
  }

  def code16() = {
    case class Id(id: String)
    val enumeratee1: Enumeratee[Int, String]  = Enumeratee.map(_.toString)
    val enumeratee2: Enumeratee[String, Id]   = Enumeratee.map(s => Id(s))
    val enumeratee3: Enumeratee[Int, Id]      = enumeratee1 compose enumeratee2
  }
}