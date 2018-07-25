package co.com.scalatraining.effects

import org.scalatest.FunSuite

import scala.collection.immutable.Seq

class OptionSuite extends FunSuite {

  test("Se debe poder crear un Option con valor"){
    val s = Option{
      1
    }
    assert(s == Some(1))
  }

  test("Se debe poder crear un Option Some con valor"){
    val s = Some{
      1
    }
    assert(s == Some(1))
  }

  test("Se debe poder crear un Option NULL con valor"){
    val s = Option{

    }
    assert(s == Some())
  }

  test("Se debe poder crear un Option para denotar que no hay valor"){
    val s = None
    assert(s == None)
  }

  test("Es inseguro acceder al valor de un Option con get"){
    val s = None
    assertThrows[NoSuchElementException]{
      val r = s.get
    }


  }

  test("Se debe poder hacer pattern match sobre un Option") {
    val lista: Seq[Option[String]] = List(Some("Andres"), None, Some("Luis"), Some("Pedro"))
    val nombre: Option[String] = lista(1)
    var res = ""
    res = nombre match {
      case Some(nom) => nom
      case None => "NONAME"
    }
    assert(res == "NONAME")
  }

  test("Fold en Option"){
    val o = Option(1)

    val res: Int = o.fold{
      10
    }{
      x => x + 20
    }

    assert(res == 21)
  }

  test("Fold en Option Null"){
    def foo(s:Int):Option[Int]=(
      if(s%2==0){
        Some(s)
      }else{None}
    )
    //val o:Int = Option(null)

    val res: Int = foo(2).fold{
      10
    }{
      x => x + 20
    }

    assert(res == 22)
  }

  test("Se debe poder saber si un Option tiene valor con isDefined") {
    val lista = List(Some("Andres"), None, Some("Luis"), Some("Pedro"))
    val nombre = lista(0)
    assert(nombre.isDefined)
  }

  test("Se debe poder acceder al valor de un Option de forma segura con getOrElse") {
    val lista = List(Some("Andres"), None, Some("Luis"), Some("Pedro"))
    val nombre = lista(1)
    val res = nombre.getOrElse("NONAME")
    assert(res == "NONAME")
  }

  test("getOrElse en terminos de Fold"){
    val lista = List(Some("Andres"), None, Some("Luis"), Some("Pedro"))
    val nombre = lista(1)

    val res: String = nombre.fold{
      "NONE"
    }{
      x => x
    }
    assert(res == "NONE")
  }

  test("Un Option se debe poder transformar con un map") {
    val lista = List(Some("Andres"), None, Some("Luis"), Some("Pedro"))
    val nombre = lista(0)
    val nombreCompleto: Option[String] = nombre.map(s => s + " Felipe")
    assert(nombreCompleto.getOrElse("NONAME") == "Andres Felipe")
  }

  test("Un Option se debe poder transformar con flatMap en otro Option") {
    val lista = List(Some("Andres"), None, Some("Luis"), Some("Pedro"))
    val nombre = lista(0)

    val resultado: Option[String] = nombre.flatMap(s => Option(s.toUpperCase))
    resultado.map( s => assert( s == "ANDRES"))
  }

  test("Un Option se debe poder filtrar con una hof con filter") {
    val lista = List(Some(5), None, Some(40), Some(20))
    val option0 = lista(0)
    val option1 = lista(1)
    val res0 = option0.filter(_>10)
    val res1 = option1.filter(_>10)

    assert(res0 == None)
    assert(res1 == None)
  }

  test("Un Option se debe poder filtrar con una hof con filter si cumple con la condicion") {
    val lista = List(Some(5), None, Some(40), Some(20))
    val option0 = lista(0)
    val res0: Option[Int] = option0.filter(_>4)
    assert(res0 != None)
    assert(res0 == Some(5))
  }

  test("for comprehensions en Option") {
    val lista = List(Some(5), None, Some(40), Some(20))
    val s1 = lista(0)
    val s2 = lista(2)

    val resultado = for {
      x <- s1
      y <- s2
    } yield x+y

    assert(resultado == Some(45))
  }

  test("for comprehensions en Option no es perfecto") {
    val lista = List(Some(5), None, Some(40), Some(20))
    val s1 = lista(0)
    val s2 = Some(3)
    val s3 = lista(1)

    val resultado = for {
      x <- s1
      z <- s3
    } yield x+z

    assert(resultado == None)
  }

  test("Test"){
    def foo(s:Int): Option[Int] ={
      println("ejecutando foo con")
      Some(s)
    }

    def bar(s:Int)={
      println(s"ejecutando con $s")
      None
    }

    val resultado = for {
      x <- foo(1)
      y <- bar(2) /*al momento de un for yield encontrar un None, todo el for yield es None*/
      z <- foo(1)
      x <- foo(1)
      y <- bar(2) /*al momento de un for yield encontrar un None, todo el for yield es None*/
      z <- foo(1)
      x <- foo(1)
      y <- bar(2) /*al momento de un for yield encontrar un None, todo el for yield es None*/
      z <- foo(1)
    } yield x+y+z

    assert(resultado==None)
  }

  test("prueba flatmap = a forcomp"){
    val o1 = Some(1)
    val o2 = Some(2)
    val o3 = Some(3)

    val res: Option[Int] = o1.flatMap(x=>
                            o2.flatMap(y=>
                              o3.flatMap(z=>
                                Option(x+y+z))))
  }


  test("for comprehesions None en Option") {
    val consultarNombre = Some("Andres")
    val consultarApellido = Some("Estrada")
    val consultarEdad = None
    val consultarSexo = Some("M")

    val resultado = for {
      nom <- consultarNombre
      ape <- consultarApellido
      eda <- consultarEdad
      sex <- consultarSexo
    //} yield (nom+","+ape+","+eda+","+sex)
    } yield (s"$nom $ape, $eda,$sex")

    assert(resultado == None)
  }

  test("for comprehesions None en Option 2") {

    def consultarNombre(dni:String): Option[String] = Some("Felix")
    def consultarApellido(dni:String): Option[String] = Some("Vergara")
    def consultarEdad(dni:String): Option[String] = None
    def consultarSexo(dni:String): Option[String] = Some("M")

    val dni = "8027133"
    val resultado = for {
      nom <- consultarNombre(dni)
      ape <- consultarApellido(dni)
      eda <- consultarEdad(dni)
      sex <- consultarSexo(dni)
    //} yield (nom+","+ape+","+eda+","+sex)
    } yield (s"$nom $ape, $eda,$sex")

    assert(resultado == None)
  }

  /*Ejercicio equivalentes de pattern match: Tony Morris*/

  test("flatmap"){

    /*option match {
        case None => None
        case Some(x) => foo(x)
      }*/

    def foo(s:Int): Option[Int] ={
      Some(s)
    }
    val o = Some(1)

    val res = o.flatMap(foo(_))
    assert(res==Some(1))

  }

  test("flatten"){

    /*option match {
        case None => None
        case Some(x) => x
      }*/

    val o = Some(Some(1))
    val res = o.flatten
    assert(res==Some(1))

  }

  test("map"){

    /*<option match {
        case None => None
        case Some(x) => Some(foo(x))
      }*/

    def foo(s:Int): Int ={
      val x= s+10
      x
    }

    val o = Some(1)

    val res: Option[Int] = o.map(foo(_))
    assert(res==Some(11))

  }


  test("foreach"){
    var temp=1
    /*option match {
        case None => {}
        case Some(x) => foo(x)
      }*/
    def foo(s:Int) ={
      temp = s+1
    }

    val o = Some(1)
    val res = o.foreach(foo(_))
    assert(temp==2)

  }

  test("isDefined"){

    /*option match {
        case None => false
        case Some(_) => true
      }*/

    val o = Some(1)
    val o2 = None
    assertResult(true){
      o.isDefined
    }
    assertResult(false){
      o2.isDefined
    }
  }

  test("isEmpty"){

    /*option match {
        case None => true
        case Some(_) => false
      }*/

    val o = Some(1)
    val o2 = None
    assertResult(false){
      o.isEmpty
    }
    assertResult(true){
      o2.isEmpty
    }
  }

  test("forall"){
    var temp=1
    /*option match {
        case None => true
        case Some(x) => foo(x)
      }*/
    def foo(s:Int) ={
      temp = s+1
    }

    val o = Some(5)
    val res = o.foreach(foo(_))
    assert(temp==6)

  }

  test("exist"){
    /*option match {
        case None => false
        case Some(x) => foo(x)
      }*/
    def foo(s:Int):Boolean ={
      if(s%2==0){true} else {
        false
      }
    }

    val o = Some(2)
    val res: Boolean = o.exists(foo(_))
    assert(res==true)
  }

  test("orelse"){
    /*option match {
        case None => foo
        case Some(x) => Some(x)
      }*/

    val o = Some(1)
    val res = o.orElse(Option(2))
    val no = None.orElse((Option(2)))
    assert(res==Option(1))
    assert(no==Option(2))
  }

  test("getOrElse"){
    /*option match {
        case None => foo
        case Some(x) => x
      }*/

    val o = Some(1)
    val res = o.getOrElse(Option(2))
    val no = None.getOrElse(Option(2))
    assert(res==1)
    assert(no==Some(2))
  }

  test("toList"){
    /*option match {
        case None => Nil
        case Some(x) => x :: Nil
      }*/

    val o = Some(1)
    val res = o.toList
    val no = None.toList
    assert(res==List(1))
    assert(no==Nil)
  }





}

