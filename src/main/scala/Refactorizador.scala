package Lenguaje

class Refactorizador {

  def refactorizarValorableEfectivo(valorable: Valorable): Valorable = { //TODO hacer type dependant para sacar los asInstanceOf[]
    valorable match {
      case Suma(n, Numero(0)) => refactorizarValorable(n)
      case Suma(Numero(0), n) => refactorizarValorable(n)
      case Resta(n, Numero(0)) => refactorizarValorable(n)
      case Multiplicacion(n, Numero(1)) => refactorizarValorable(n)
      case Multiplicacion(Numero(1), n) => refactorizarValorable(n)
      case Division(n, Numero(1)) => refactorizarValorable(n)
      case Mayor(Numero(n), Numero(m)) => Booleano(n > m)
      case Menor(Numero(n), Numero(m)) => Booleano(n < m)
      case Igual(Numero(n), Numero(m)) => Booleano(n == m)
      case Distinto(Numero(n), Numero(m)) => Booleano(n != m)
      case MenorOIgual(Numero(n), Numero(m)) => Booleano(n <= m)
      case MayorOIgual(Numero(n), Numero(m)) => Booleano(n >= m)
      case n => n
    }
  }

  def refactorizarValorable(valorable: Valorable) : Valorable = { //TODO hacer type dependant para sacar los asInstanceOf[]
    valorable match {
      case Suma(n, m) => refactorizarValorableEfectivo(Suma(refactorizarValorable(n).asInstanceOf[ValorableNumerico], refactorizarValorable(m).asInstanceOf[ValorableNumerico]))
      case Resta(n, m) => refactorizarValorableEfectivo(Resta(refactorizarValorable(n).asInstanceOf[ValorableNumerico], refactorizarValorable(m).asInstanceOf[ValorableNumerico]))
      case Multiplicacion(n, m) => refactorizarValorableEfectivo(Multiplicacion(refactorizarValorable(n).asInstanceOf[ValorableNumerico], refactorizarValorable(m).asInstanceOf[ValorableNumerico]))
      case Division(n, m) => refactorizarValorableEfectivo(Division(refactorizarValorable(n).asInstanceOf[ValorableNumerico], refactorizarValorable(m).asInstanceOf[ValorableNumerico]))
      case Mayor(n, m) => refactorizarValorableEfectivo(Mayor(refactorizarValorable(n).asInstanceOf[ValorableNumerico], refactorizarValorable(m).asInstanceOf[ValorableNumerico]))
      case Menor(n, m) => refactorizarValorableEfectivo(Menor(refactorizarValorable(n).asInstanceOf[ValorableNumerico], refactorizarValorable(m).asInstanceOf[ValorableNumerico]))
      case Igual(n, m) => refactorizarValorableEfectivo(Igual(refactorizarValorable(n).asInstanceOf[ValorableNumerico], refactorizarValorable(m).asInstanceOf[ValorableNumerico]))
      case Distinto(n, m) => refactorizarValorableEfectivo(Distinto(refactorizarValorable(n).asInstanceOf[ValorableNumerico], refactorizarValorable(m).asInstanceOf[ValorableNumerico]))
      case MenorOIgual(n, m) => refactorizarValorableEfectivo(MenorOIgual(refactorizarValorable(n).asInstanceOf[ValorableNumerico], refactorizarValorable(m).asInstanceOf[ValorableNumerico]))
      case MayorOIgual(n, m) => refactorizarValorableEfectivo(MayorOIgual(refactorizarValorable(n).asInstanceOf[ValorableNumerico], refactorizarValorable(m).asInstanceOf[ValorableNumerico]))
      case n => n
    }
  }

  def refactorizar(prog: Programa): Programa = Programa(prog.expresion.map(refactorizarValorable(_)))

}