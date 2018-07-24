package co.com.scalatraining.ejercicio

object Cotizacion {
  def main(args: Array[String]): Unit = {
    val x = new servicio
  }
}

class datos {
  val file: List[List[String]] = List(List("2018/07","S4N","10","1000000"),
    List("2018/07","S4N","20","1000000"),
    List("2018/08","S4N","30","2000000"),
    List("2018/08","S4N","30","0"))
  def getfile():List[List[String]] ={
    file
  }
}

class servicio{
  val c = new datos
  val lista = c.getfile

  def calculoSueldo(s:String,dias:String):Int = {
    if (dias != "30") {
      (s.toInt * 30) / dias.toInt
    }else{s.toInt}
  }

  val sub = lista.filter(!_.isEmpty).map(x=>(x,calculoSueldo(x.reverse.head,"10")))
  println(sub)



}