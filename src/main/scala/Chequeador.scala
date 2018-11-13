package Lenguaje

//import Lenguaje._

trait Gravedad{} //Podria pasar a ser un Enum?

case class Error() extends Gravedad

case class Advertencia() extends Gravedad

case class Problema(descripcion : String, gravedad : Gravedad, valorable: Valorable)

abstract class Regla() {
  def evaluar(valorable: Valorable): Option[Problema]
}

class DivisionPor0 extends Regla {
  override def evaluar(valorable: Valorable): Option[Problema] = {
    valorable match {
      case Division(_, Numero(0)) => Some(Problema("no se puede dividir por cero", Error(), valorable))
      case _ => None
    }
  }
}

class OperacionRedundante extends Regla {
  override def evaluar(valorable: Valorable): Option[Problema] = {
    valorable match {
      case Resta(_, Numero(0)) | Suma(_, Numero(0)) | Suma(Numero(0), _) | Multiplicacion(_, Numero(1)) | Multiplicacion(Numero(1), _) | Division(_, Numero(1)) => Some(Problema("operacion redundante", Advertencia(), valorable))
      case _ => None
    }
  }
}

class ComparacionSinSentido extends Regla {
  override def evaluar(valorable: Valorable): Option[Problema] = {
    valorable match {
      case Igual(Numero(_), Numero(_)) | Distinto(Numero(_), Numero(_)) | Menor(Numero(_), Numero(_)) | Mayor(Numero(_), Numero(_)) | MenorOIgual(Numero(_), Numero(_)) | MayorOIgual(Numero(_), Numero(_)) => Some(Problema("comparacion sin sentido", Advertencia(), valorable))
      case _ => None
    }
  }
}

class Chequeador() {

  def evaluacionReglas(valorable: Valorable, reglas: List[Regla]) : List[Problema] = reglas.foldLeft(List[Problema]()) { (acc, i) => acc ++ i.evaluar(valorable).toList }

  def evaluacion(valorable: Valorable, reglas: List[Regla]) : List[Problema] = {

    valorable match {
      case a: Absoluto => List[Problema]()

      case v@Suma(a: Valorable, b: Valorable) => evaluacionReglas(v, reglas) ++ evaluacion(a, reglas) ++ evaluacion(b, reglas) //TODO Son todos iguales ¿Se puede generalizar?

      case v@Resta(a: Valorable, b: Valorable) => evaluacionReglas(v, reglas) ++ evaluacion(a, reglas) ++ evaluacion(b, reglas)

      case v@Division(a: Valorable, b: Valorable) => evaluacionReglas(v, reglas) ++ evaluacion(a, reglas) ++ evaluacion(b, reglas)

      case v@Multiplicacion(a: Valorable, b: Valorable) => evaluacionReglas(v, reglas) ++ evaluacion(a, reglas) ++ evaluacion(b, reglas)

      case v@Mayor(a: Valorable, b: Valorable) => evaluacionReglas(v, reglas) ++ evaluacion(a, reglas) ++ evaluacion(b, reglas)

      case v@Menor(a: Valorable, b: Valorable) => evaluacionReglas(v, reglas) ++ evaluacion(a, reglas) ++ evaluacion(b, reglas)

      case v@Igual(a: Valorable, b: Valorable) => evaluacionReglas(v, reglas) ++ evaluacion(a, reglas) ++ evaluacion(b, reglas)

      case v@Distinto(a: Valorable, b: Valorable) => evaluacionReglas(v, reglas) ++ evaluacion(a, reglas) ++ evaluacion(b, reglas)

      case v@MenorOIgual(a: Valorable, b: Valorable) => evaluacionReglas(v, reglas) ++ evaluacion(a, reglas) ++ evaluacion(b, reglas)

      case v@MayorOIgual(a: Valorable, b: Valorable) => evaluacionReglas(v, reglas) ++ evaluacion(a, reglas) ++ evaluacion(b, reglas)
    }
  }
  
  def chequear(prog: Programa, reglas: List[Regla]): List[Problema] = evaluacion(prog.expresion, reglas)
}
