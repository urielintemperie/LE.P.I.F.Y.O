package Lenguaje
import org.scalatest.FlatSpec

class GeneralTest extends FlatSpec {

  "Un programa de suma simple" should "retornar el resultado correcto" in {
    val numeroA = Numero(5)
    val numeroB = Numero(2)
    val operacionSuma = Suma(numeroA, numeroB)
    val programa = Programa(operacionSuma)
    val interprete = new Interprete()

    assert(interprete.ejecutar(programa) == Numero(7))
  }

  "Un programa de resta simple" should "retornar el resultado correcto" in {
    val numeroA = Numero(5)
    val numeroB = Numero(2)
    val operacionResta = Resta(numeroA, numeroB)
    val programa = Programa(operacionResta)
    val interprete = new Interprete()

    assert(interprete.ejecutar(programa) == Numero(3))
  }

  "Un programa de division simple" should "retornar el resultado correcto" in {
    val numeroA = Numero(10)
    val numeroB = Numero(2)
    val operacionDivision = Division(numeroA, numeroB)
    val programa = Programa(operacionDivision)
    val interprete = new Interprete()

    assert(interprete.ejecutar(programa) == Numero(5))
  }

  "Un programa de multiplicacion simple" should "retornar el resultado correcto" in {
    val numeroA = Numero(10)
    val numeroB = Numero(2)
    val operacionMultiplicacion = Multiplicacion(numeroA, numeroB)
    val programa = Programa(operacionMultiplicacion)
    val interprete = new Interprete()

    assert(interprete.ejecutar(programa) == Numero(20))
  }

  "Un programa de mayor simple" should "retornar el resultado correcto" in {
    val numeroA = Numero(10)
    val numeroB = Numero(2)
    val operacionMayor = Mayor(numeroA, numeroB)
    val programa = Programa(operacionMayor)
    val interprete = new Interprete()

    assert(interprete.ejecutar(programa) == Booleano(true))
  }

  "Un programa de menor simple" should "retornar el resultado correcto" in {
    val numeroA = Numero(10)
    val numeroB = Numero(2)
    val operacionMenor = Menor(numeroA, numeroB)
    val programa = Programa(operacionMenor)
    val interprete = new Interprete()

    assert(interprete.ejecutar(programa) == Booleano(false))
  }

  "Un programa de igual simple" should "retornar el resultado correcto" in {
    val numeroA = Numero(10)
    val numeroB = Numero(10)
    val operacionIgual = Igual(numeroA, numeroB)
    val programa = Programa(operacionIgual)
    val interprete = new Interprete()

    assert(interprete.ejecutar(programa) == Booleano(true))
  }

  "Un programa de disiinto simple" should "retornar el resultado correcto" in {
    val numeroA = Numero(10)
    val numeroB = Numero(0)
    val operacionDistinto = Distinto(numeroA, numeroB)
    val programa = Programa(operacionDistinto)
    val interprete = new Interprete()

    assert(interprete.ejecutar(programa) == Booleano(true))
  }

  "Un programa de menorOIgual simple" should "retornar el resultado correcto" in {
    val numeroA = Numero(0)
    val numeroB = Numero(10)
    val numeroC = Numero(10)
    val operacionMenorOIgualA = MenorOIgual(numeroA, numeroC)
    val operacionMenorOIgualB = MenorOIgual(numeroB, numeroC)
    val programaA = Programa(operacionMenorOIgualA)
    val programaB = Programa(operacionMenorOIgualB)
    val interprete = new Interprete()

    assert(interprete.ejecutar(programaA) == Booleano(true))
    assert(interprete.ejecutar(programaB) == Booleano(true))
  }

  "Un programa de mayorOIgual simple" should "retornar el resultado correcto" in {
    val numeroA = Numero(20)
    val numeroB = Numero(10)
    val numeroC = Numero(10)
    val operacionMayorOIgualA = MayorOIgual(numeroA, numeroC)
    val operacionMayorOIgualB = MayorOIgual(numeroB, numeroC)
    val programaA = Programa(operacionMayorOIgualA)
    val programaB = Programa(operacionMayorOIgualB)
    val interprete = new Interprete()

    assert(interprete.ejecutar(programaA) == Booleano(true))
    assert(interprete.ejecutar(programaB) == Booleano(true))
  }

  "Un programa de sumas anidadas" should "retornar el resultado correcto" in {
    val numeroA = Numero(5)
    val numeroB = Numero(2)
    val numeroC = Numero(3)
    val operacionSumaA = Suma(numeroA, numeroB)
    val operacionSumaB = Suma(numeroC, operacionSumaA)
    val programa = Programa(operacionSumaB)
    val interprete = new Interprete()

    assert(interprete.ejecutar(programa) == Numero(10))
  }


  "Un programa de resta anidadas" should "retornar el resultado correcto" in {
    val numeroA = Numero(5)
    val numeroB = Numero(2)
    val numeroC = Numero(3)
    val operacionRestaA = Resta(numeroA, numeroB)
    val operacionRestaB = Resta(numeroC, operacionRestaA)
    val programa = Programa(operacionRestaB)
    val interprete = new Interprete()

    assert(interprete.ejecutar(programa) == Numero(0))
  }

  "Un programa de division anidadas" should "retornar el resultado correcto" in {
    val numeroA = Numero(60)
    val numeroB = Numero(2)
    val numeroC = Numero(3)
    val operacionDivisionA = Division(numeroA, numeroB)
    val operacionDivisionB = Division(operacionDivisionA, numeroC)
    val programa = Programa(operacionDivisionB)
    val interprete = new Interprete()

    assert(interprete.ejecutar(programa) == Numero(10))
  }

  "Un programa de multiplicacion anidadas" should "retornar el resultado correcto" in {
    val numeroA = Numero(60)
    val numeroB = Numero(2)
    val numeroC = Numero(3)
    val operacionMultiplicacionA = Multiplicacion(numeroA, numeroB)
    val operacionMultiplicacionB = Multiplicacion(operacionMultiplicacionA, numeroC)
    val programa = Programa(operacionMultiplicacionB)
    val interprete = new Interprete()

    assert(interprete.ejecutar(programa) == Numero(360))
  }

  //No hay funciones de mayor | menor | igual | distinto | menorOIgual | mayorOIgual  anidadas dado que estas reciben dos Numeros pero retornan un Booleano
  //Ejemplo
  /*
  "Un programa de mayor anidadas" should "retornar el resultado correcto" in {
    val numeroA = Numero(60)
    val numeroB = Numero(2)
    val numeroC = Numero(3)
    val operacionMayorA = Mayor(numeroA, numeroB)
    val operacionMayorB = Mayor(operacionMayorA, numeroC) //<--- Error Type mismatch
    val programa = Programa(operacionMayorB)
    val interprete = new Interprete()
  }
  */

  "Un programa de multiples funciones anidadas" should "retornar el resultado correcto" in {
    val numeroA = Numero(60)
    val numeroB = Numero(2)
    val numeroC = Numero(210)
    val numeroD = Numero(3)
    val numeroE = Numero(50)
    val operacionMultiplicacion = Multiplicacion(numeroA, numeroB)
    val operacionDivision = Division(numeroC, numeroD)
    val operacionResta = Resta(operacionMultiplicacion, operacionDivision)
    val operacionIgual = Igual(operacionResta, Numero(50))
    val programa = Programa(operacionIgual)
    val interprete = new Interprete()

    assert(interprete.ejecutar(programa) == Booleano(true))
  }

  "Un chequeador" should "poder chequear una expresion y devolver posibles problemas" in {
    val primerNumero = Numero(4)
    val segundoNumero = Numero(0)
    val operacionDivision = Division(primerNumero, segundoNumero)
    val programa = Programa(operacionDivision)

    val problemaDivisionPor0 = Problema("no se puede dividir por cero", Error(), operacionDivision)
    val reglaDivisionPor0 = new DivisionPor0
    val chequeador = new Chequeador()

    assert(chequeador.chequear(programa, List(reglaDivisionPor0)) == List(problemaDivisionPor0))
  }

  "Un chequeador" should "poder chequear una expresion y devolver error por restar 0" in {
    val cuatro = Numero(4)
    val cero = Numero(0)
    val operacionResta = Resta(cuatro, cero)
    val programa = Programa(operacionResta)

    val problemaOperacionRedundante = Problema("operacion redundante", Advertencia(), operacionResta)
    val reglaOperacionRedundante = new OperacionRedundante
    val chequeador = new Chequeador()

    assert(chequeador.chequear(programa, List(reglaOperacionRedundante)) == List(problemaOperacionRedundante))
  }

  "Un chequeador" should "poder chequear una expresion anidada y devolver error por restar 0" in {
    val cuatro = Numero(4)
    val cero = Numero(0)
    val operacionResta = Resta(cuatro, cero)
    val operacionSuma = Suma(cuatro, operacionResta)
    val programa = Programa(operacionSuma)

    val problemaOperacionRedundante = Problema("operacion redundante", Advertencia(), operacionResta)
    val reglaOperacionRedundante = new OperacionRedundante
    val chequeador = new Chequeador()

    assert(chequeador.chequear(programa, List(reglaOperacionRedundante)) == List(problemaOperacionRedundante))
  }

  "Un chequeador" should "poder chequear una expresion anidada y devolver dos error por restar 0" in {
    val cuatro = Numero(4)
    val diez = Numero(10)
    val cero = Numero(0)
    val operacionRestaA = Resta(cuatro, cero)
    val operacionRestaB = Resta(diez, cero)
    val operacionSuma = Suma(cuatro, operacionRestaA)
    val operacionRestaC = Resta(operacionSuma, operacionRestaB)
    val programa = Programa(operacionRestaC)

    val problemaOperacionRedundanteA = Problema("operacion redundante", Advertencia(), operacionRestaA)
    val problemaOperacionRedundanteB = Problema("operacion redundante", Advertencia(), operacionRestaB)
    val problemasEsperados = List(problemaOperacionRedundanteB, problemaOperacionRedundanteA)
    val reglaOperacionRedundante = new OperacionRedundante
    val chequeador = new Chequeador()

    assert(problemasEsperados forall (chequeador.chequear(programa, List(reglaOperacionRedundante)).contains(_)))
  }


  "Un chequeador" should "poder chequear multiples funciones anidadas y devolver todos los problemas" in {
    val cuatro = Numero(4)
    val cero = Numero(0)
    val uno = Numero(1)
    val operacionDivision = Division(cuatro, cero)
    val operacionResta = Resta(operacionDivision, cero)
    val operacionSumar = Suma(cero, operacionResta)
    val operacionMultiplicar = Multiplicacion(operacionSumar, uno)
    val programa = Programa(operacionMultiplicar)

    val problemaDivisionPor0 = Problema("no se puede dividir por cero", Error(), operacionDivision)
    val problemaResta = Problema("operacion redundante", Advertencia(), operacionResta)
    val problemaSuma = Problema("operacion redundante", Advertencia(), operacionSumar)
    val problemaMultiplicar = Problema("operacion redundante", Advertencia(), operacionMultiplicar)
    val problemasEsperados = List(problemaDivisionPor0, problemaResta, problemaSuma, problemaMultiplicar)
    val reglaDivisionPor0 = new DivisionPor0
    val reglaOperacionRedundante = new OperacionRedundante
    val reglas = List(reglaOperacionRedundante, reglaDivisionPor0)
    val chequeador = new Chequeador()

    assert(problemasEsperados forall (chequeador.chequear(programa, reglas).contains(_)))
  }

  "Un chequeador" should "poder chequear una funcion por comparacionSinSentido y devolver los problemas" in {
    val cuatro = Numero(4)
    val cero = Numero(0)
    val uno = Numero(1)
    val operacionIgual = Igual(cuatro, cero)
    val programa = Programa(operacionIgual)

    val problemaComparacionSinSentido = Problema("comparacion sin sentido", Advertencia(), operacionIgual)
    val problemasEsperados = List(problemaComparacionSinSentido)
    val reglaComparacionSinSentido = new ComparacionSinSentido
    val reglas = List(reglaComparacionSinSentido)
    val chequeador = new Chequeador()

    assert(problemasEsperados forall (chequeador.chequear(programa, reglas).contains(_)))
  }

  "Un refactorizador" should "poder refactorizar una funcion simple" in {
    val cuatro = Numero(4)
    val cero = Numero(0)
    val operacionIgual = Igual(cuatro, cero)
    val programa = Programa(operacionIgual)

    val refactorizador = new Refactorizador()

    assert(refactorizador.refactorizar(programa) == Programa(Booleano(false)))
  }

  "Un refactorizador" should "poder refactorizar una funcion simple suma" in {
    val cuatro = Numero(4)
    val cero = Numero(0)
    val operacionSuma = Suma(cuatro, cero)
    val programa = Programa(operacionSuma)

    val refactorizador = new Refactorizador()

    assert(refactorizador.refactorizar(programa) == Programa(Numero(4)))
  }

  "Un refactorizador" should "poder refactorizar una funcion encadenada" in {
    val cuatro = Numero(4)
    val cero = Numero(0)
    val uno = Numero(1)
    val operacionSuma = Suma(cero, cuatro)
    val operacionResta = Resta(operacionSuma, cero)
    val operacionDivision = Division(operacionResta, uno)
    val operacionMultiplicacion = Multiplicacion(uno, operacionDivision)
    val operacionMenor = Menor(uno, operacionMultiplicacion)
    val programa = Programa(operacionMenor)

    val refactorizador = new Refactorizador()

    assert(refactorizador.refactorizar(programa) == Programa(Booleano(true)))
  }

  "Un refactorizador" should "devolver lo mismo si no hay nada para refactorizar" in {
    val cuatro = Numero(4)
    val siete = Numero(7)
    val cero = Numero(0)
    val uno = Numero(1)
    val operacionSuma = Suma(cuatro, cuatro)
    val operacionResta = Resta(operacionSuma, uno)
    val operacionDivision = Division(operacionResta, siete)
    val operacionMultiplicacion = Multiplicacion(siete, operacionDivision)
    val programa = Programa(operacionMultiplicacion)

    val refactorizador = new Refactorizador()

    assert(refactorizador.refactorizar(programa) == Programa(operacionMultiplicacion))
  }

  "Un refactorizador" should "funcionar pese a las referencias" in {
    val cuatro = Numero(4)
    val cero = Numero(0)

    val operacionResta = Resta(Referencia("numeroCuatro"), cero)

    val variableCuatro = Variable("numeroCuatro", Some(Numero(4)))

    val listaOperaciones = List(variableCuatro, operacionResta)
    val programa = Programa(listaOperaciones)

    val listaOperacionesEsperadas = List(variableCuatro, Referencia("numeroCuatro"))

    val refactorizador = new Refactorizador()

    assert(refactorizador.refactorizar(programa) == Programa(listaOperacionesEsperadas))
  }


  "Un refactorizador" should "funcionar pese a las referencias 2" in {
    val cuatro = Numero(4)
    val cero = Numero(0)

    val operacionResta = Resta(Referencia("numeroCuatro"), cero)
    val operacionSuma = Suma(operacionResta, cuatro)

    val variableCuatro = Variable("numeroCuatro", Some(Numero(4)))

    val listaOperaciones = List(variableCuatro, operacionSuma)
    val programa = Programa(listaOperaciones)

    val listaOperacionesEsperadas = List(variableCuatro, Suma(Referencia("numeroCuatro"),cuatro))
    val refactorizador = new Refactorizador()

    assert(refactorizador.refactorizar(programa) == Programa(listaOperacionesEsperadas))
  }


  "Un programa simple" should "poder ejecutarse teniendo refereencias" in {
    val cuatro = Numero(4)
    val operacionSuma = Suma(Referencia("numeroCuatro"), cuatro)
    val listaOperaciones = List(Variable("numeroCuatro", Some(Numero(4))), operacionSuma)
    val programa = Programa(listaOperaciones)

    val interprete = new Interprete()

    assert(interprete.ejecutar(programa) == Numero(8))
  }

  "Un programa simple" should "poder ejecutarse teniendo refereencias anidadas" in {
    val cuatro = Numero(4)
    val operacionResta = Resta(Numero(14), Referencia("numeroCuatro"))
    val operacionSuma = Suma(Referencia("resta"), cuatro)

    val variableResta = Variable("resta", Some(operacionResta))
    val variableCuatro = Variable("numeroCuatro", Some(Numero(4)))

    val listaOperaciones = List(variableCuatro, variableResta, operacionSuma)
    val programa = Programa(listaOperaciones)

    val interprete = new Interprete()

    assert(interprete.ejecutar(programa) == Numero(14))
  }

  "Un programa simple" should "poder ejecutarse teniendo refereencias no cargadas y luego asignandolas" in {
    val cuatro = Numero(4)
    val operacionResta = Resta(Numero(14), Referencia("numeroCuatro"))
    val operacionSuma = Suma(Referencia("resta"), cuatro)

    val variableCuatro = Variable("numeroCuatro", None)
    val variableResta = Variable("resta", Some(operacionResta))
    val asignacionCuatro = Asignar("numeroCuatro", Numero(4))

    val listaOperaciones = List(variableCuatro, variableResta, asignacionCuatro, operacionSuma)
    val programa = Programa(listaOperaciones)

    val interprete = new Interprete()

    assert(interprete.ejecutar(programa) == Numero(14))
  }

  "Un chequeador" should "poder chequear una funcion por variableDuplicada y devolver los problemas" in {
    val cuatro = Numero(4)
    val cero = Numero(0)
    val operacionIgual = Igual(cuatro, cero)
    val variableCuatro = Variable("numeroCuatro", None)
    val variableCuatroRepetida = Variable("numeroCuatro", Some(cuatro))
    val listaOperaciones = List(variableCuatro, variableCuatroRepetida, operacionIgual)
    val programa = Programa(listaOperaciones)

    val problemaVariableDuplicada = Problema("variable duplicada", Error(), variableCuatroRepetida)
    val problemasEsperados = List(problemaVariableDuplicada)
    val reglaVariableDuplicada = new VariableDuplicada
    val reglas = List(reglaVariableDuplicada)
    val chequeador = new Chequeador()

    assert(problemasEsperados forall (chequeador.chequear(programa, reglas).contains(_)))
  }

"Un chequeador" should "poder chequear un programa y devolver los problemas tanto normales como los variableDuplicada" in {
  val cuatro = Numero(4)
  val cero = Numero(0)
  val uno = Numero(1)
  val operacionDivision = Division(cuatro, cero)
  val operacionResta = Resta(operacionDivision, cero)
  val operacionSumar = Suma(cero, operacionResta)
  val operacionMultiplicar = Multiplicacion(operacionSumar, uno)
  val variableCuatro = Variable("numeroCuatro", None)
  val variableCuatroRepetida = Variable("numeroCuatro", Some(cuatro))
  val listaOperaciones = List(variableCuatro, variableCuatroRepetida, operacionMultiplicar)
  val programa = Programa(listaOperaciones)

  val problemaDivisionPor0 = Problema("no se puede dividir por cero", Error(), operacionDivision)
  val problemaResta = Problema("operacion redundante", Advertencia(), operacionResta)
  val problemaSuma = Problema("operacion redundante", Advertencia(), operacionSumar)
  val problemaMultiplicar = Problema("operacion redundante", Advertencia(), operacionMultiplicar)
  val problemaVariableDuplicada = Problema("variable duplicada", Error(), variableCuatroRepetida)
  val problemasEsperados = List(problemaDivisionPor0, problemaResta, problemaSuma, problemaMultiplicar, problemaVariableDuplicada)
  val reglaDivisionPor0 = new DivisionPor0
  val reglaOperacionRedundante = new OperacionRedundante
  val reglaVariableDuplicada = new VariableDuplicada
  val reglas = List(reglaOperacionRedundante, reglaDivisionPor0, reglaVariableDuplicada)
  val chequeador = new Chequeador()

  assert(problemasEsperados forall (chequeador.chequear(programa, reglas).contains(_)))
}

  "Un chequeador" should "poder chequear una funcion por variableNoDeclarada y devolver los problemas" in {
    val cero = Numero(0)
    val variableNoCargada = Variable("numeroCero", None)
    val operacionIgual = Igual(Referencia("numeroCuatro"), Referencia("numeroCero"))
    val listaOperaciones = List(variableNoCargada, operacionIgual)
    val programa = Programa(listaOperaciones)

    val problemaVariableNoDeclaradaA = Problema("variable no declarada", Error(), Referencia("numeroCero"))
    val problemaVariableNoDeclaradaB = Problema("variable no declarada", Error(), Referencia("numeroCuatro"))
    val problemasEsperados = List(problemaVariableNoDeclaradaA, problemaVariableNoDeclaradaB)
    val reglaVariableNoDeclarada = new VariableNoDeclarada
    val reglas = List(reglaVariableNoDeclarada)
    val chequeador = new Chequeador()

    assert(problemasEsperados forall (chequeador.chequear(programa, reglas).contains(_)))
  }

}