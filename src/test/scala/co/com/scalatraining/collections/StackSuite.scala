package co.com.scalatraining.collections

import org.scalatest.FunSuite
import scala.collection.immutable.Stack

class StackSuite extends FunSuite{

  test("Construccion de un Stack"){
    val stk = Stack[Int](1,2,3,4)
    assert(Stack(1,2,3,4)==stk)
  }

  test("Construccion de un Stack vacio con tipo"){
    val stk = Stack[Int]()
    assert(Stack()==stk)
  }


  test("Que pasa si hacemos head a una Stack() vacia") {
    val stk = Stack[Int]()
    assertThrows[NoSuchElementException] {
      stk.head
    }
  }

  test("Push de un Stack"){
    val stk = Stack[Int](1,2,3,4)
    assertResult(Stack(3,2,1,2,3,4)) {
      stk.push(2,3)
    }
  }

  test("Pop de un Stack"){
    val stk = Stack[Int](1,2,3,4)
    assertResult(Stack(2,3,4)) {
      stk.pop
    }
  }

  test("map en un Stack") {
    val stk = Stack[String]("1", "2", "3")
    val stk2 = stk.map(dato => dato + "prueba")
    assert(stk != stk2)
  }

  test("Se debe poder obtener todos los elementos de una Stack sin el primer elemento") {
    val stk = Stack(1, 2, 3, 4)
    assertResult(Stack(2, 3, 4)) {
      stk.tail
    }
  }

  test("Una Stack se debe poder acumular") {
    val stk = Stack(1, 2, 3, 4, 5)
    assertResult(15) {
      stk.fold(0) { (acumulado, item) =>
        acumulado + item
      }
    }
  }

  

}
