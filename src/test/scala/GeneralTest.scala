import org.scalatest.FlatSpec

class GeneralTest extends FlatSpec {
  //--------------------------Test parte 1--------------------------

  "Un programa" should "poder tener una expresion" in {
    val primerNumero = Numero(1)
    val segundoNumero = Numero(3)
    val operacionSuma = Suma(primerNumero, segundoNumero)
    val programa = Programa(operacionSuma)

    assert(programa.expresion == operacionSuma)
  }

  "Un chequeador" should "poder chequear una expresion y devolver posibles problemas" in {
    val primerNumero = Numero(4)
    val segundoNumero = Numero(0)
    val operacionDivision = Division(primerNumero, segundoNumero)
    val programa = Programa(operacionDivision)

    val problemaDivisionPor0 = Problema("no se puede dividir por cero", Error(), operacionDivision)
    val reglaDivisionPor0 = new DivisionPor0
    val chequeador = new Chequeador(programa, List(reglaDivisionPor0))

    assert(chequeador.chequear == List(problemaDivisionPor0))
  }

  "un refactoriador" should "recibir una operacion redundante y refactorizarla" in {
    val primerNumero = Numero(4)
    val segundoNumero = Numero(0)
    val operacionSuma = Suma(primerNumero, segundoNumero)
    val programa = Programa(operacionSuma)

    val refactorizador = new Refactorizador()

    assert(refactorizador.refactorizar(programa) == Programa(Numero(4)))

  }

  "un interprete" should "recibir un programa y devolver su valor" in {
    val primerNumero = Numero(4)
    val segundoNumero = Numero(1)
    val operacionSuma = Suma(primerNumero, segundoNumero)
    val programa = Programa(operacionSuma)

    val interprete = new Interprete()

    assert(interprete.ejecutar(programa) == Numero(5))
  }
}