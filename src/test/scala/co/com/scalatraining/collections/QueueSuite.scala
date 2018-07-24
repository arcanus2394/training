package co.com.scalatraining.collections

import org.scalatest.FunSuite
import scala.collection.immutable.Queue

class QueueSuite extends FunSuite {

  test("Construccion de un Queue"){
    val q = Queue[Int](1,2,3,4)
    assert(Queue(1,2,3,4)==q)
  }

  test("Construccion de un Queue Strings"){
    val q = Queue[String]("he","ro","Fe","4")
    assert(Queue("he","ro","Fe","4")==q)
  }

  test("Construccion de un Queue vacio con tipo"){
    val q = Queue[Int]()
    assert(Queue()==q)
  }

  test("Adicion de un elemento"){
    val q = Queue[String]()
    val q2 = q :+ "1"
    assert(Queue("1")==q2)
  }

  test("map en un set") {
    val q = Queue[String]("1", "2", "3")
    val q2 = q.map(dato => dato + "prueba")
    assert(q != q2)
  }

  test("Se debe poder consultar el primer elemento de una Queue de forma insegura") {
    val q = Queue(1, 2, 3, 4)
    assertResult(1) {
      q.head
    }
  }

  test("Que pasa si hacemos head a una Queue() vacia") {
    val q = Queue[Int]()
    assertThrows[NoSuchElementException] {
      q.head
    }
  }

  test("Usar enqueue para agregar un elemento a la queue"){
    val q = Queue[String]("1", "2", "3")
    val enqueue = q.enqueue("ultimo")
    assert(Queue("1","2","3","ultimo")==enqueue)
  }

  test("Usar enqueue para sacar tupla de primer elemento de la queue con la queue resultante"){
    val q = Queue[String]("1", "2", "3")
    val dequeue: (String, Queue[String]) = q.dequeue
    assert(("1",Queue("2","3"))==dequeue)
  }

  test("A una Queue se le debe poder eliminar elementos con drop") {
    val q = Queue(1, 2, 3, 4)
    val q2 =q.drop(2)

    assertResult(Queue(3, 4)) {
      q2
    }
  }

  test("Retornar primer elemento en la queue"){
    val q1 = Queue(1,2,3)
    assertResult(1) {
      q1.front
    }
  }

  test("Una Stack contiene o no un valor dado") {
    val q = Queue(1, 2, 3, 4, 5)
    assertResult(true) {
      q.contains(2)
    }
  }

  test("Se debe poder obtener todos los elementos de una queue sin el primer elemento") {
    val q = Queue(1, 2, 3, 4)
    assertResult(Queue(2, 3, 4)) {
      q.tail
    }
  }


  test("Una Queue se debe poder acumular") {
    val q = Queue(1, 2, 3, 4, 5)
    assertResult(15) {
      q.fold(0) { (acumulado, item) =>
        acumulado + item
      }
    }
  }

}
