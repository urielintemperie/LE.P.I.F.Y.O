
case class Numero(var num : Int) extends Expresion

abstract class Expresion{
}

case class Suma(numero1 : Numero, numero2 : Numero) extends Expresion

case class Resta(numero1 : Numero, numero2 : Numero) extends Expresion

case class Division(numero1 : Numero, numero2 : Numero) extends Expresion

case class Multiplicacion(numero1 : Numero, numero2 : Numero) extends Expresion

case class Mayor(numero1 : Numero, numero2 : Numero) extends Expresion

case class Menor(numero1 : Numero, numero2 : Numero) extends Expresion

case class Igual(numero1 : Numero, numero2 : Numero) extends Expresion

case class Distinto(numero1 : Numero, numero2 : Numero) extends Expresion

case class MenorOIgual(numero1 : Numero, numero2 : Numero) extends Expresion

case class MayorOIgual(numero1 : Numero, numero2 : Numero) extends Expresion


case class Programa(var expresion : Expresion)

trait Gravedad{
}

case class Error() extends Gravedad

case class Advertencia() extends Gravedad

case class Problema(var descripcion : String, var gravedad : Gravedad, var expresion : Expresion)

abstract class Regla(){
  def evaluar(expresion : Expresion): Option[Problema]
}

class DivisionPor0 extends Regla{
  override def evaluar(expresion : Expresion): Option[Problema] = {
    expresion match {
      case Division(_, Numero(0)) => Some(Problema("no se puede dividir por cero", Error(), expresion))
      case _ => None
    }
  }
}

class OperacionRedundante extends Regla{
  override def evaluar(expresion : Expresion): Option[Problema] = {
    expresion match {
      case Resta(_, Numero(0)) | Suma(_, Numero(0)) | Multiplicacion(_, Numero(1)) | Division(_, Numero(1)) => Some(Problema("operacion redundante", Advertencia(), expresion))
      case _ => None
    }
  }
}

class ComparacionSinSentido extends Regla{
  override def evaluar(expresion : Expresion): Option[Problema] = {
    expresion match {
      case Igual(Numero(2), Numero(2)) | Menor(Numero(3), Numero(4)) | Mayor(Numero(3), Numero(4)) => Some(Problema("comparacion sin sentido", Advertencia(), expresion))
      case _ => None
    }
  }
}

class Chequeador(val prog : Programa, val _reglas : List[Regla]){
  var reglas : List[Regla] = _reglas

  def chequear(): List[Problema] = {
    var problemas = List[Problema]()
    _reglas.foreach(regla => regla.evaluar(prog.expresion).foreach(problema => problemas = problema :: problemas))
    return problemas
  }
}

trait Booleano extends Expresion{
}

case class True() extends Booleano

case class False() extends Booleano

class Refactorizador(){
  def refactorizar(prog : Programa): Programa = {
    prog.expresion match {
      case Suma(n, Numero(0)) => Programa(n)
      case Resta(n, Numero(0)) => Programa(n)
    }
  }
}

class Interprete(){

  def mayor(a : Int, b : Int): Booleano ={
    var rta : Booleano = False()
    if(a > b){
      rta = True()
    }
    return rta
  }

  def menor(a : Int, b : Int): Booleano = {
    var rta : Booleano = False()
    if(a < b){
      rta = True()
    }
    return rta
  }

  def igual(a : Int, b : Int): Booleano = {
    var rta : Booleano = False()
    if(a == b){
      rta = True()
    }
    return rta
  }

  def distinto(a : Int, b : Int): Booleano = {
    var rta : Booleano = False()
    if(a != b){
      rta = True()
    }
    return rta
  }

  def mayorOIgual(a: Int, b: Int): Booleano = {
    var rta : Booleano = False()
    if(a >= b){
      rta = True()
    }
    return rta
  }

  def menorOIgual(a: Int, b: Int): Booleano = {
    var rta : Booleano = False()
    if(a <= b){
      rta = True()
    }
    return rta
  }

  def ejecutar(prog : Programa): Expresion = {
    prog.expresion match {
      case Suma(Numero(a), Numero(b)) => Numero(a+b)
      case Resta(Numero(a), Numero(b)) => Numero(a-b)
      case Division(Numero(a), Numero(b)) => Numero(a / b)
      case Multiplicacion(Numero(a), Numero(b)) => Numero(a*b)
      case Mayor(Numero(a), Numero(b)) => mayor(a,b)
      case Menor(Numero(a), Numero(b)) => menor(a,b)
      case Igual(Numero(a), Numero(b)) => igual(a,b)
      case Distinto(Numero(a), Numero(b)) => distinto(a,b)
      case MayorOIgual(Numero(a), Numero(b)) => mayorOIgual(a,b)
      case MenorOIgual(Numero(a), Numero(b)) => menorOIgual(a,b)
    }
  }
}

