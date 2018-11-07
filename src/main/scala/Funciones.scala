package Lenguaje

case class Parametro(var param: String) extends Expresion

trait Instruccion{

}

case class Retornar(var expresiones: Expresion) extends Expresion

case class Funcion(var nombre : String, var listaParametros : List[Parametro], var listaInstrucciones : List[Expresion]) extends Expresion


//-----------------------------Chequeos----------------------------------------

class DebeRetornarAlgo extends Regla{
  override def evaluar(expresion: Expresion): Option[Problema] = {
    expresion match {
      case Funcion(_,_,Nil) => Some(Problema("La funcion no tiene retorno", Error(), expresion))
      case Funcion(_,_,linst) => if (linst.exists(i => !i.isInstanceOf[Retornar])) Some(Problema("La funcion no tiene retorno", Error(), expresion)) else None
      case _ => None
    }
  }
}

class RetornoSiempreUltimo extends Regla{
  override def evaluar(expresion: Expresion): Option[Problema] = {
    expresion match {
      case Funcion(_, _, linst) => if (!linst.last.isInstanceOf[Retornar]) Some(Problema("La funcion posee instrucciones despues del retorno", Error(), expresion)) else None
      case _ => None
    }
  }
}

class NombreParametrosDuplicados extends Regla{

  def auxiliar(parametros: List[Parametro]): Option[Problema] = {
    parametros match {
      case x :: xs => if(xs.exists( _.param == x.param)) Some(Problema("nombres de parametros duplicados", Error(), x)) else auxiliar(xs)
      case _ => None
    }
  }

  override def evaluar(expresion: Expresion): Option[Problema] = {
    expresion match {
      case Funcion(_, lparam, _) => auxiliar(lparam)
      case _ => None
    }
  }
}