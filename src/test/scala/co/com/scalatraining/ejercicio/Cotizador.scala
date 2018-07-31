package co.com.scalatraining.ejercicio

import org.scalatest.FunSuite

class Cotizador extends FunSuite{
  def calculoSueldo(c:Cotizacion):Cotizacion = {
    if (c.dias != "30") {
      val res = new Cotizacion(c.periodo,c.aportante,c.dias,(c.IBC.toInt * 30) / c.dias.toInt)
      res
    } else { val res= new Cotizacion(c.periodo,c.aportante,c.dias,c.IBC)
      res
    }
  }

  val file = List(Cotizacion("2018/07","S4N",10,1000000),
    Cotizacion("2018/07","S4N",20,1000000),
    Cotizacion("2018/08","S4N",30,2000000),
    Cotizacion("2018/08","S4N",30,0),
    Cotizacion("2018/08","S4N",0,1000000),
    Cotizacion("2018/08","S7N",10,1000000))

  /*Regla 1*/
  val res1 = file.filter(x=>x.IBC!=0&&x.dias!=0)
  println(res1)
  /*Regla 2*/
  val res2 = res1.map(x=>calculoSueldo(x))
  println(res2)
  /*Regla 3*/
  val res3 = res2.distinct
  /*Regla 4 seleccionando el mayor*/
  val res4 = res3.groupBy(x => (x.periodo,x.aportante)).map(x => x._1 -> x._2.foldLeft(0) { (acum, item) =>
    if (acum < item.IBC) {
      item.IBC
    } else {
      acum
    }
  })
  println(res4)

  test("Prueba regla 1"){
    val res1 = file.filter(x=>x.IBC!=0&&x.dias!=0)
    assert(res1==List(Cotizacion("2018/07","S4N",10,1000000),
      Cotizacion("2018/07","S4N",20,1000000),
      Cotizacion("2018/08","S4N",30,2000000),
      Cotizacion("2018/08","S7N",10,1000000)))
  }

}


