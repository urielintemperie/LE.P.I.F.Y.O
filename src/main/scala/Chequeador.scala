package Lenguaje

trait Gravedad{}

case class Error() extends Gravedad

case class Advertencia() extends Gravedad

case class Problema(descripcion : String, gravedad : Gravedad, valorable: Valorable)

abstract class Regla() {
  def evaluar(valorable: Valorable, scope: Map[String, Option[Valorable]]): Option[Problema]
}

class DivisionPor0 extends Regla {
  override def evaluar(valorable: Valorable, scope: Map[String, Option[Valorable]]): Option[Problema] = {
    valorable match {
      case Division(_, Numero(0)) => Some(Problema("no se puede dividir por cero", Error(), valorable))
      case _ => None
    }
  }
}

class OperacionRedundante extends Regla {
  override def evaluar(valorable: Valorable, scope: Map[String, Option[Valorable]]): Option[Problema] = {
    valorable match {
      case Resta(_, Numero(0)) | Suma(_, Numero(0)) | Suma(Numero(0), _) | Multiplicacion(_, Numero(1)) | Multiplicacion(Numero(1), _) | Division(_, Numero(1)) => Some(Problema("operacion redundante", Advertencia(), valorable))
      case _ => None
    }
  }
}

class ComparacionSinSentido extends Regla {
  override def evaluar(valorable: Valorable, scope: Map[String, Option[Valorable]]): Option[Problema] = {
    valorable match {
      case Igual(Numero(_), Numero(_)) | Distinto(Numero(_), Numero(_)) | Menor(Numero(_), Numero(_)) | Mayor(Numero(_), Numero(_)) | MenorOIgual(Numero(_), Numero(_)) | MayorOIgual(Numero(_), Numero(_)) => Some(Problema("comparacion sin sentido", Advertencia(), valorable))
      case _ => None
    }
  }
}

class VariableDuplicada extends Regla {
  override def evaluar(valorable: Valorable, scope: Map[String, Option[Valorable]]): Option[Problema] = {

    valorable match {
        case vari@Variable(k,v) => if (scope.contains(k)) Some(Problema("variable duplicada", Error(), vari)) else None
        case _ => None
    }

  }
}

class VariableNoDeclarada extends Regla {
  override def evaluar(valorable: Valorable, scope: Map[String, Option[Valorable]]): Option[Problema] = {

    valorable match {
      case vari@Referencia(k) => if (!scope.contains(k) || scope(k).isEmpty) Some(Problema("variable no declarada", Error(), vari)) else None
      case _ => None
    }

  }
}


class Chequeador() {

  def evaluacionReglas(valorable: Valorable, reglas: List[Regla], scope: Map[String, Option[Valorable]]) : List[Problema] = reglas.foldLeft(List[Problema]()) { (acc, i) => acc ++ i.evaluar(valorable, scope).toList }

  def evaluacion(valorable: Valorable, reglas: List[Regla], scope: Map[String, Option[Valorable]]) : List[Problema] = {

    valorable match {
      case a: Absoluto => List[Problema]()

      case v@Suma(a: Valorable, b: Valorable) => evaluacionReglas(v, reglas, scope) ++ evaluacion(a, reglas, scope) ++ evaluacion(b, reglas, scope) //TODO Son todos iguales ¿Se puede generalizar?

      case v@Resta(a: Valorable, b: Valorable) => evaluacionReglas(v, reglas, scope) ++ evaluacion(a, reglas, scope) ++ evaluacion(b, reglas, scope)

      case v@Division(a: Valorable, b: Valorable) => evaluacionReglas(v, reglas, scope) ++ evaluacion(a, reglas, scope) ++ evaluacion(b, reglas, scope)

      case v@Multiplicacion(a: Valorable, b: Valorable) => evaluacionReglas(v, reglas, scope) ++ evaluacion(a, reglas, scope) ++ evaluacion(b, reglas, scope)

      case v@Mayor(a: Valorable, b: Valorable) => evaluacionReglas(v, reglas, scope) ++ evaluacion(a, reglas, scope) ++ evaluacion(b, reglas, scope)

      case v@Menor(a: Valorable, b: Valorable) => evaluacionReglas(v, reglas, scope) ++ evaluacion(a, reglas, scope) ++ evaluacion(b, reglas, scope)

      case v@Igual(a: Valorable, b: Valorable) => evaluacionReglas(v, reglas, scope) ++ evaluacion(a, reglas, scope) ++ evaluacion(b, reglas, scope)

      case v@Distinto(a: Valorable, b: Valorable) => evaluacionReglas(v, reglas, scope) ++ evaluacion(a, reglas, scope) ++ evaluacion(b, reglas, scope)

      case v@MenorOIgual(a: Valorable, b: Valorable) => evaluacionReglas(v, reglas, scope) ++ evaluacion(a, reglas, scope) ++ evaluacion(b, reglas, scope)

      case v@MayorOIgual(a: Valorable, b: Valorable) => evaluacionReglas(v, reglas, scope) ++ evaluacion(a, reglas, scope) ++ evaluacion(b, reglas, scope)

      case v:Variable => evaluacionReglas(v, reglas, scope)

      case v:Asignar => evaluacionReglas(v, reglas, scope)

      case v:Referencia => evaluacionReglas(v, reglas, scope)
    }
  }

  def evaluacionSuperior(valorable: List[Valorable], reglas: List[Regla]) : List[Problema]= {

    var scope : Map[String, Option[Valorable]] = Map()
    var problemas : List[Problema] = List()

    valorable.foreach(i => {
      i match {
        case eval@Variable(k,v) => {
          problemas = problemas ++ evaluacion(eval, reglas, scope)
          scope += k -> v
        }
        case eval@Asignar(k, v) => {
          problemas = problemas ++ evaluacion(eval, reglas, scope)
          scope += k -> Some(v)
        }
        case f => problemas = problemas ++ evaluacion(f, reglas, scope)
      }
    })
    return problemas
  }

  def chequear(prog: Programa, reglas: List[Regla]): List[Problema] = evaluacionSuperior(prog.expresion, reglas)
}
