package Lenguaje

case class Variable(clave: String, valor: Option[Valorable] = None) extends Valorable

case class Asignar(clave: String, valor: Valorable) extends Valorable

case class Referencia(clave: String) extends ValorableNumerico with ValorableBooleano //Le pongo ambos traits para que las funciones no se quejen del tipo de parametro que esperan
