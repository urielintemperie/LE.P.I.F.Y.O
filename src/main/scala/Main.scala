package Lenguaje

abstract class Valorable //aka Expresion

trait ValorableNumerico extends Valorable

trait ValorableBooleano extends Valorable

//abstract class Funcion extends Valorable

trait Absoluto extends Valorable with ValorableNumerico with ValorableBooleano //Como modificar interprete.evaluar() para evitar esto?



case class Numero(num : Int) extends ValorableNumerico with Absoluto

case class Booleano(boolean: Boolean) extends ValorableBooleano with Absoluto

case class Suma(valorA : ValorableNumerico, valorB : ValorableNumerico) extends ValorableNumerico //Funcion

case class Resta(valorA : ValorableNumerico, valorB : ValorableNumerico) extends ValorableNumerico //Funcion

case class Division(valorA : ValorableNumerico, valorB : ValorableNumerico) extends ValorableNumerico //Funcion

case class Multiplicacion(valorA : ValorableNumerico, valorB : ValorableNumerico) extends ValorableNumerico //Funcion

case class Mayor(valorA : ValorableNumerico, valorB : ValorableNumerico) extends ValorableBooleano //Funcion

case class Menor(valorA : ValorableNumerico, valorB : ValorableNumerico) extends ValorableBooleano //Funcion

case class Igual(valorA : ValorableNumerico, valorB : ValorableNumerico) extends ValorableBooleano //Funcion

case class Distinto(valorA : ValorableNumerico, valorB : ValorableNumerico) extends ValorableBooleano //Funcion

case class MenorOIgual(valorA : ValorableNumerico, valorB : ValorableNumerico) extends ValorableBooleano //Funcion

case class MayorOIgual(valorA : ValorableNumerico, valorB : ValorableNumerico) extends ValorableBooleano //Funcion

case class Programa(expresion : Valorable)



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

  def ejecutar(prog: Programa): Absoluto = this.evaluar(prog.expresion)

}


/*
abstract class Expresion{
}

trait Evaluable extends Expresion

case class Numero(var num : Int) extends Expresion with Evaluable

trait Booleano extends Expresion with Evaluable  {
}

case class True() extends Booleano

case class False() extends Booleano


case class Suma(numero1 : Evaluable, numero2 : Evaluable) extends Expresion

case class Resta(numero1 : Evaluable, numero2 : Evaluable) extends Expresion

case class Division(numero1 : Evaluable, numero2 : Evaluable) extends Expresion

case class Multiplicacion(numero1 : Evaluable, numero2 : Evaluable) extends Expresion

case class Mayor(numero1 : Evaluable, numero2 : Evaluable) extends Expresion

case class Menor(numero1 : Evaluable, numero2 : Evaluable) extends Expresion

case class Igual(numero1 : Evaluable, numero2 : Evaluable) extends Expresion

case class Distinto(numero1 : Evaluable, numero2 : Evaluable) extends Expresion

case class MenorOIgual(numero1 : Evaluable, numero2 : Evaluable) extends Expresion

case class MayorOIgual(numero1 : Evaluable, numero2 : Evaluable) extends Expresion


case class Programa(var expresion : Expresion)

trait Gravedad{
}

case class Error() extends Gravedad

case class Advertencia() extends Gravedad

case class Problema(var descripcion : String, var gravedad : Gravedad, var expresion : Expresion)

abstract class Regla() {
  def evaluar(expresion: Expresion): Option[Problema]

  def listIfReferece(evaluable: Evaluable): List[Referencia] = {
    evaluable match {
      case r@Referencia(a) => List(r)
      case _ => List()
    }
  }

  def extraerReferencias(expresion: Expresion): List[Referencia] = {
    expresion match {
      case Suma(a, b) => listIfReferece(a) ++ listIfReferece(b) ++ List()

      case Resta(a, b) => listIfReferece(a) ++ listIfReferece(b) ++ List()

      case Division(a, b) => listIfReferece(a) ++ listIfReferece(b) ++ List()

      case Multiplicacion(a, b) => listIfReferece(a) ++ listIfReferece(b) ++ List()

      case Mayor(a, b) => listIfReferece(a) ++ listIfReferece(b) ++ List()

      case Menor(a, b) => listIfReferece(a) ++ listIfReferece(b) ++ List()

      case Igual(a, b) => listIfReferece(a) ++ listIfReferece(b) ++ List()

      case Distinto(a, b) => listIfReferece(a) ++ listIfReferece(b) ++ List()

      case MayorOIgual(a, b) => listIfReferece(a) ++ listIfReferece(b) ++ List()

      case MenorOIgual(a, b) => listIfReferece(a) ++ listIfReferece(b) ++ List()
    }
  }
}






class DivisionPor0 extends Regla {
  override def evaluar(expresion: Expresion): Option[Problema] = {
    expresion match {
      case Division(_, Numero(0)) => Some(Problema("no se puede dividir por cero", Error(), expresion))
      case _ => None
    }
  }
}

class OperacionRedundante extends Regla {
  override def evaluar(expresion: Expresion): Option[Problema] = {
    expresion match {
      case Resta(_, Numero(0)) | Suma(_, Numero(0)) | Multiplicacion(_, Numero(1)) | Division(_, Numero(1)) => Some(Problema("operacion redundante", Advertencia(), expresion))
      case _ => None
    }
  }
}

class ComparacionSinSentido extends Regla {
  override def evaluar(expresion: Expresion): Option[Problema] = {
    expresion match {
      case Igual(Numero(_), Numero(_)) | Menor(Numero(_), Numero(_)) | Mayor(Numero(_), Numero(_)) => Some(Problema("comparacion sin sentido", Advertencia(), expresion))
      case _ => None
    }
  }
}

class VariableDuplicada extends Regla {
  override def evaluar(expresion: Expresion): Option[Problema] = {
    if (Referencias.varDuplicated.nonEmpty) {
      Some(Problema("Variables Duplicadas", Error(), Referencias.varDuplicated.head))
    } else {
      None
    }
  }
}

class VariableNoDeclarada extends Regla{
  override def evaluar(expresion: Expresion): Option[Problema] = {
    //println(this.extraerReferencias(expresion))
    var referenciasNoDeclaradas = this.extraerReferencias(expresion).filter(r => Referencias.getVarOption(r.clave).isEmpty) //TODO revisar
    referenciasNoDeclaradas match {
      case r :: ls => Some(Problema("Variables no declarada", Error(), r))
      case Nil => None
    }
  }
}

class VariableDeclaradaNoUsada extends Regla{
  override def evaluar(expresion: Expresion): Option[Problema] = {
    var referencias = this.extraerReferencias(expresion)

    var variablesUsadas:List[Variable]= List()

    referencias.foreach(r => Referencias.getVarOption(r.clave).foreach(variable => variablesUsadas = variable :: variablesUsadas))

    //println(variablesUsadas.head.clave)
    var variablesNoUsadas = Referencias.referencias.values.toList.filter(v => !variablesUsadas.contains(v))

    //println(variablesNoUsadas.head.clave)

    variablesNoUsadas match {
      case r :: ls => Some(Problema("Variables declaradas no usadas", Advertencia(), r))
      case Nil => None
    }
  }
}

class VariableUsadaNoAsignada extends Regla{
  override def evaluar(expresion: Expresion): Option[Problema] = {
    var referencias = this.extraerReferencias(expresion)
    var variablesUsadas:List[Variable]= List()

    referencias.foreach(r => Referencias.getVarOption(r.clave).foreach(variable => variablesUsadas = variable :: variablesUsadas))
    variablesUsadas.filter(variable => variable.valor == None)

    variablesUsadas match {
      case r :: ls => Some(Problema("Variables usadas no asignadas", Error(), r))
      case Nil => None
    }
  }
}


class Chequeador(val prog: Programa, val _reglas: List[Regla]) {


  def chequear(): List[Problema] = {
    var problemas = List[Problema]()
    _reglas.foreach(regla => regla.evaluar(prog.expresion).foreach(problema => problemas = problema :: problemas))
    return problemas
  }
}

  class Refactorizador() {
    def boolToBooleano(boolean : Boolean): Booleano ={
      boolean match {
        case true => True()
        case false => False()
      }
    }

    def refactorizar(prog: Programa): Programa = {
      prog.expresion match {
        case Suma(n, Numero(0)) => Programa(n)
        case Resta(n, Numero(0)) => Programa(n)
        case Multiplicacion(n, Numero(1)) => Programa(n)
        case Division(n, Numero(1)) => Programa(n)
        case Mayor(Numero(n), Numero(m)) => Programa(boolToBooleano(n>m))
        case Menor(Numero(n), Numero(m)) => Programa(boolToBooleano(n<m))
        case Igual(Numero(n), Numero(m)) => Programa(boolToBooleano(n==m))
        case Distinto(Numero(n), Numero(m)) => Programa(boolToBooleano(n!=m))
        case MenorOIgual(Numero(n), Numero(m)) => Programa(boolToBooleano(n<=m))
        case MayorOIgual(Numero(n), Numero(m)) => Programa(boolToBooleano(n>=m))
        case _ => prog
      }
    }
  }

  class Interprete() {

    def mayor(a: Int, b: Int): Booleano = {
      var rta: Booleano = False()
      if (a > b) {
        rta = True()
      }
      return rta
    }

    def menor(a: Int, b: Int): Booleano = {
      var rta: Booleano = False()
      if (a < b) {
        rta = True()
      }
      return rta
    }

    def igual(a: Int, b: Int): Booleano = {
      var rta: Booleano = False()
      if (a == b) {
        rta = True()
      }
      return rta
    }

    def distinto(a: Int, b: Int): Booleano = {
      var rta: Booleano = False()
      if (a != b) {
        rta = True()
      }
      return rta
    }

    def mayorOIgual(a: Int, b: Int): Booleano = {
      var rta: Booleano = False()
      if (a >= b) {
        rta = True()
      }
      return rta
    }

    def menorOIgual(a: Int, b: Int): Booleano = {
      var rta: Booleano = False()
      if (a <= b) {
        rta = True()
      }
      return rta
    }


    def evaluar(exp: Evaluable): Evaluable = {
      exp match {
        case Numero(a) => Numero(a)
        case a: Booleano => a
        case r1@Referencia(a) => r1.getValue()
      }
    }

    def ejecutar(prog: Programa): Expresion = {
      prog.expresion match {
        case Suma(Numero(a), Numero(b)) => Numero(a + b)
        case Suma(a, b) => this.ejecutar(Programa(Suma(this.evaluar(a), this.evaluar(b))))

        case Resta(Numero(a), Numero(b)) => Numero(a - b)
        case Resta(a, b) => this.ejecutar(Programa(Resta(this.evaluar(a), this.evaluar(b))))

        case Division(Numero(a), Numero(b)) => Numero(a / b)
        case Division(a, b) => this.ejecutar(Programa(Division(this.evaluar(a), this.evaluar(b))))

        case Multiplicacion(Numero(a), Numero(b)) => Numero(a * b)
        case Multiplicacion(a, b) => this.ejecutar(Programa(Multiplicacion(this.evaluar(a), this.evaluar(b))))

        case Mayor(Numero(a), Numero(b)) => mayor(a, b)
        case Mayor(a, b) => this.ejecutar(Programa(Mayor(this.evaluar(a), this.evaluar(b))))

        case Menor(Numero(a), Numero(b)) => menor(a, b)
        case Menor(a, b) => this.ejecutar(Programa(Menor(this.evaluar(a), this.evaluar(b))))

        case Igual(Numero(a), Numero(b)) => igual(a, b)
        case Igual(a, b) => this.ejecutar(Programa(Igual(this.evaluar(a), this.evaluar(b))))

        case Distinto(Numero(a), Numero(b)) => distinto(a, b)
        case Distinto(a, b) => this.ejecutar(Programa(Distinto(this.evaluar(a), this.evaluar(b))))

        case MayorOIgual(Numero(a), Numero(b)) => mayorOIgual(a, b)
        case MayorOIgual(a, b) => this.ejecutar(Programa(MayorOIgual(this.evaluar(a), this.evaluar(b))))

        case MenorOIgual(Numero(a), Numero(b)) => menorOIgual(a, b)
        case MenorOIgual(a, b) => this.ejecutar(Programa(MenorOIgual(this.evaluar(a), this.evaluar(b))))

      }
    }
  }

*/