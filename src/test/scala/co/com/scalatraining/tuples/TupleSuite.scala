package co.com.scalatraining.tuples

import org.scalatest.FunSuite

class TupleSuite  extends FunSuite {

  test("Una tupla se debe poder crear"){
    val tupla = (1, 2,"3", List(1, 2, 3))
    assert(tupla._2 == 2)
    assert(tupla._4.tail.head == 2)
  }

  test("Una tupla 5 listas "){
    val tupla = (List(1, 2, 3),
                  List(11, 22, 33),
                  List(111, 222, 333),
                  List(1111, 2222, 3333),
                  List(11111, 22222, 33333))

    val lista = List(tupla._1.head,tupla._2.head,tupla._3.head,tupla._4.head,tupla._5.head)

    assert(lista == List(1,11,111,1111,11111))
  }

}



