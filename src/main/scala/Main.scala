package Lenguaje

abstract class Valorable 

trait ValorableNumerico extends Valorable

trait ValorableBooleano extends Valorable


trait Absoluto extends Valorable with ValorableNumerico with ValorableBooleano



case class Numero(num : Int) extends ValorableNumerico with Absoluto

case class Booleano(boolean: Boolean) extends ValorableBooleano with Absoluto

case class Suma(valorA : ValorableNumerico, valorB : ValorableNumerico) extends ValorableNumerico 

case class Resta(valorA : ValorableNumerico, valorB : ValorableNumerico) extends ValorableNumerico 

case class Division(valorA : ValorableNumerico, valorB : ValorableNumerico) extends ValorableNumerico 

case class Multiplicacion(valorA : ValorableNumerico, valorB : ValorableNumerico) extends ValorableNumerico 

case class Mayor(valorA : ValorableNumerico, valorB : ValorableNumerico) extends ValorableBooleano 

case class Menor(valorA : ValorableNumerico, valorB : ValorableNumerico) extends ValorableBooleano 

case class Igual(valorA : ValorableNumerico, valorB : ValorableNumerico) extends ValorableBooleano 

case class Distinto(valorA : ValorableNumerico, valorB : ValorableNumerico) extends ValorableBooleano 

case class MenorOIgual(valorA : ValorableNumerico, valorB : ValorableNumerico) extends ValorableBooleano 

case class MayorOIgual(valorA : ValorableNumerico, valorB : ValorableNumerico) extends ValorableBooleano 


class Programa(var expresion : List[Valorable]) {

  override def equals(obj: scala.Any): Boolean = {
    obj match {
      case obj: Programa => obj.expresion == expresion
      case _ => false
    }
  }

}

object Programa {
  def apply(expresion: List[Valorable]): Programa = new Programa(expresion)
  def apply(expresion: Valorable): Programa = new Programa(List(expresion))

  def unapply(arg: Programa): Option[List[Valorable]] = Some(arg.expresion)
}


case class scopePrograma()

class Interprete() {

  def evaluar(valorable: Valorable): Absoluto = {
    valorable match {

      case a: Absoluto => a

      case Suma(Numero(a), Numero(b)) => Numero(a+b)
      case Suma(a: Valorable, b: Valorable) => evaluar (Suma (evaluar(a), evaluar(b)))

      case Resta(Numero(a), Numero(b)) => Numero(a-b)
      case Resta(a: Valorable, b: Valorable) => evaluar (Resta (evaluar(a), evaluar(b)))

      case Division(Numero(a), Numero(b)) => Numero(a/b)
      case Division(a: Valorable, b: Valorable) => evaluar (Division (evaluar(a), evaluar(b)))

      case Multiplicacion(Numero(a), Numero(b)) => Numero(a*b)
      case Multiplicacion(a: Valorable, b: Valorable) => evaluar (Multiplicacion (evaluar(a), evaluar(b)))

      case Mayor(Numero(a), Numero(b)) => Booleano(a>b)
      case Mayor(a: Valorable, b: Valorable) => evaluar (Mayor (evaluar(a), evaluar(b)))

      case Menor(Numero(a), Numero(b)) => Booleano(a<b)
      case Menor(a: Valorable, b: Valorable) => evaluar (Menor (evaluar(a), evaluar(b)))

      case Igual(Numero(a), Numero(b)) => Booleano(a==b)
      case Igual(a: Valorable, b: Valorable) => evaluar (Igual (evaluar(a), evaluar(b)))

      case Distinto(Numero(a), Numero(b)) => Booleano(a!=b)
      case Distinto(a: Valorable, b: Valorable) => evaluar (Distinto (evaluar(a), evaluar(b)))

      case MenorOIgual(Numero(a), Numero(b)) => Booleano(a<=b)
      case MenorOIgual(a: Valorable, b: Valorable) => evaluar (MenorOIgual (evaluar(a), evaluar(b)))

      case MayorOIgual(Numero(a), Numero(b)) => Booleano(a>=b)
      case MayorOIgual(a: Valorable, b: Valorable) => evaluar (MayorOIgual (evaluar(a), evaluar(b)))

    }
  }

  def solucionarReferenciasAux(valorable: Valorable, scope: Map[String, Option[Valorable]]) : Valorable = { //TODO hacer type dependant para sacar los asInstanceOf[]
    valorable match {
      case Referencia(k) => solucionarReferenciasAux(scope(k).get, scope) //El scope(k) retorna un Option[Valorable] es por eso que hago get

      case a: Absoluto => a
        
      case Suma(a: Valorable, b: Valorable) =>  Suma (solucionarReferenciasAux(a, scope).asInstanceOf[ValorableNumerico], solucionarReferenciasAux(b, scope).asInstanceOf[ValorableNumerico])
        
      case Resta(a: Valorable, b: Valorable) => Resta (solucionarReferenciasAux(a, scope).asInstanceOf[ValorableNumerico], solucionarReferenciasAux(b, scope).asInstanceOf[ValorableNumerico])
        
      case Division(a: Valorable, b: Valorable) => Division (solucionarReferenciasAux(a, scope).asInstanceOf[ValorableNumerico], solucionarReferenciasAux(b, scope).asInstanceOf[ValorableNumerico])

      case Multiplicacion(a: Valorable, b: Valorable) => Multiplicacion (solucionarReferenciasAux(a, scope).asInstanceOf[ValorableNumerico], solucionarReferenciasAux(b, scope).asInstanceOf[ValorableNumerico])

      case Mayor(a: Valorable, b: Valorable) => Mayor (solucionarReferenciasAux(a, scope).asInstanceOf[ValorableNumerico], solucionarReferenciasAux(b, scope).asInstanceOf[ValorableNumerico])

      case Menor(a: Valorable, b: Valorable) => Menor (solucionarReferenciasAux(a, scope).asInstanceOf[ValorableNumerico], solucionarReferenciasAux(b, scope).asInstanceOf[ValorableNumerico])

      case Igual(a: Valorable, b: Valorable) => Igual (solucionarReferenciasAux(a, scope).asInstanceOf[ValorableNumerico], solucionarReferenciasAux(b, scope).asInstanceOf[ValorableNumerico])

      case Distinto(a: Valorable, b: Valorable) => Distinto (solucionarReferenciasAux(a, scope).asInstanceOf[ValorableNumerico], solucionarReferenciasAux(b, scope).asInstanceOf[ValorableNumerico])

      case MenorOIgual(a: Valorable, b: Valorable) => MenorOIgual (solucionarReferenciasAux(a, scope).asInstanceOf[ValorableNumerico], solucionarReferenciasAux(b, scope).asInstanceOf[ValorableNumerico])

      case MayorOIgual(a: Valorable, b: Valorable) => MayorOIgual (solucionarReferenciasAux(a, scope).asInstanceOf[ValorableNumerico], solucionarReferenciasAux(b, scope).asInstanceOf[ValorableNumerico])
    }
  }
  
  def solucionarReferencias(instrucciones: List[Valorable]) : List[Valorable] = {
    var scope : Map[String, Option[Valorable]] = Map()
    var listaInstruccionesCargadas : List[Valorable]= List()

    instrucciones.foreach(i => {
      i match {
        case Variable(k,v) => scope += k -> v
        case Asignar(k, v) => scope += k -> Some(v)
        case f => listaInstruccionesCargadas = solucionarReferenciasAux(f, scope) :: listaInstruccionesCargadas
      }
    })
    
    return listaInstruccionesCargadas
  }
  
  def ejecutar(prog: Programa): Absoluto = this.evaluar(solucionarReferencias(prog.expresion).head)


}
