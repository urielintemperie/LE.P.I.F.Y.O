package Lenguaje
import org.scalatest.FlatSpec

class GeneralTest extends FlatSpec {
  //--------------------------Test parte 1--------------------------

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

    assert(problemasEsperados forall(chequeador.chequear(programa, List(reglaOperacionRedundante)).contains(_)))
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

    println(chequeador.chequear(programa, reglas))
    assert(problemasEsperados forall(chequeador.chequear(programa, reglas).contains(_)))
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

    println(chequeador.chequear(programa, reglas))
    assert(problemasEsperados forall(chequeador.chequear(programa, reglas).contains(_)))
  }

  "Un refactorizador" should "poder refactorizar una funcion simple" in {
    val cuatro = Numero(4)
    val cero = Numero(0)
    val operacionIgual = Igual(cuatro, cero)
    val programa = Programa(operacionIgual)

    val refactorizador = new Refactorizador()

    println(refactorizador.refactorizar(programa))
    assert(refactorizador.refactorizar(programa) == Programa(Booleano(false)))
  }

  "Un refactorizador" should "poder refactorizar una funcion simple suma" in {
    val cuatro = Numero(4)
    val cero = Numero(0)
    val operacionSuma = Suma(cuatro, cero)
    val programa = Programa(operacionSuma)

    val refactorizador = new Refactorizador()

    println(refactorizador.refactorizar(programa))
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

    println(refactorizador.refactorizar(programa))
    assert(refactorizador.refactorizar(programa) == Programa(Booleano(true)))
  }

/*
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

  "un refactoriador" should "recibir una operacion de Suma redundante y refactorizarla" in {
    val primerNumero = Numero(4)
    val segundoNumero = Numero(0)
    val operacionSuma = Suma(primerNumero, segundoNumero)
    val programa = Programa(operacionSuma)

    val refactorizador = new Refactorizador()

    assert(refactorizador.refactorizar(programa) == Programa(Numero(4)))

  }

  "un refactorizador" should "recibir una operacion de Multiplicacion redundante y refactorizarla" in {
    val primerNumero = Numero(3)
    val segundoNumero = Numero(1)
    val operacionMultiplicacion = Multiplicacion(primerNumero, segundoNumero)
    val programa = Programa(operacionMultiplicacion)

    val refactorizador = new Refactorizador()

    assert(refactorizador.refactorizar(programa) == Programa(Numero(3)))
  }

  "un refactorizador" should "recibir una comparacion Mayor sin sentido y refactorizarla" in {
    val primerNumero = Numero(3)
    val segundoNumero = Numero(2)
    val comparacionMayor = Mayor(primerNumero, segundoNumero)
    val programa = Programa(comparacionMayor)

    val refactorizador = new Refactorizador()

    assert(refactorizador.refactorizar(programa) == Programa(True()))
  }

  "un refactorizador" should "recibir una comparacion Menor sin sentido y refactorizarla" in {
    val primerNumero = Numero(4)
    val segundoNumero = Numero(3)
    val comparacionMenor = Menor(primerNumero, segundoNumero)
    val programa = Programa(comparacionMenor)

    val refactorizador = new Refactorizador()

    assert(refactorizador.refactorizar(programa) == Programa(False()))
  }

  "un interprete" should "recibir un programa y devolver su valor" in {
    val primerNumero = Numero(4)
    val segundoNumero = Numero(1)
    val operacionSuma = Suma(primerNumero, segundoNumero)
    val programa = Programa(operacionSuma)

    val interprete = new Interprete()

    assert(interprete.ejecutar(programa) == Numero(5))
  }


  "una variable" should "poder crearla y referenciarla" in {
    val variable1 = Variable("anioActual", Numero(2018))
    val variable2 = Variable("edad", Numero(25))
    //val programa = Programa(Resta(Referencia("anioActual"), Referencia("edad"))
    //val interprete = new Interprete()

    assert(Referencias.getVar("anioActual") == variable1)
    assert(Referencias.getVar("edad") != variable1)
    assert(Referencias.getVar("edad") == variable2)
  }

  /*
  "una variable" should "poder hacer una referecia con referencia" in {
    val variable1 = Variable("anioActual", Numero(2018))
    val variable2 = Variable("edad", Numero(25))
    val programa = Programa(Resta(Referencia("anioActual"), Referencia("edad"))
    val interprete = new Interprete()

    assert(Referencia("anioActual") == variable1)
  }
*/

  "un interprete" should "recibir un programa con referencias y devolver su valor" in {
    val primerNumero = Numero(4)
    val variable1 = Variable("segundoValor", Numero(1))
    val operacionSuma = Suma(primerNumero, Referencia("segundoValor"))
    val programa = Programa(operacionSuma)

    val interprete = new Interprete()

    assert(interprete.ejecutar(programa) == Numero(5))
  }


  "un variable" should "poderse instanciarse y luego inicializarse" in {
    val variable1 = Variable("altura") // sin inicializar
    assert(variable1.valor == None)

    Asignar(Referencia("altura"), Numero(172))
    assert(variable1.valor == Some(Numero(172)))
  }

  "el chequeador" should "detectar que hay variables duplicadas" in {
    val variable1 = Variable("altura", Numero(200))
    val variable2 = Variable("altura", Numero(400))
    val programa = Programa(Division(Referencia("altura"), Numero(2)))


    val reglaVariableDuplicada = new VariableDuplicada
    val chequeador = new Chequeador(programa, List(reglaVariableDuplicada))

    assert(chequeador.chequear == List(Problema("Variables Duplicadas", Error(), variable2)))
  }

  "el chequeador" should "detectar que hay variables no declaradas" in {
    Referencias.reiniciar()
    val referencia = Referencia("altura")
    val programa = Programa(Division(referencia, Numero(2)))


    val reglaVariableNoDeclarada = new VariableNoDeclarada
    val chequeador = new Chequeador(programa, List(reglaVariableNoDeclarada))

    assert(chequeador.chequear == List(Problema("Variables no declarada", Error(), referencia)))
  }

  "el chequeador" should "detectar que hay variables declaradas no usadas" in {
    Referencias.reiniciar()
    val referencia = Referencia("hola")
    val variable1 = Variable("hola", Numero(10))
    val variable2 = Variable("chau", Numero(5))
    val programa = Programa(Division(referencia, Numero(2)))


    val reglaVariableDeclaradaNoUsada = new VariableDeclaradaNoUsada
    val chequeador = new Chequeador(programa, List(reglaVariableDeclaradaNoUsada))

    assert(chequeador.chequear == List(Problema("Variables declaradas no usadas", Advertencia(), variable2)))
  }

  "el chequeador" should "detectar que hay variables usadas no asignadas" in {
    Referencias.reiniciar()
    val referencia = Referencia("hola")
    val variable1 = Variable("hola")
    val variable2 = Variable("chau", Numero(5))
    val programa = Programa(Division(referencia, Numero(2)))


    val reglaVariableUsadaNoAsignada = new VariableUsadaNoAsignada
    val chequeador = new Chequeador(programa, List(reglaVariableUsadaNoAsignada))

    assert(chequeador.chequear == List(Problema("Variables usadas no asignadas", Error(), variable1)))
  }


  "el chequeador" should "detectar que no hay instruccion Retornar en la funcion" in {
    val funcion = Funcion("suma", List(Parametro("a"), Parametro("b")), List())
    val reglaDebeRetornarAlgo = new DebeRetornarAlgo
    val programa = Programa(funcion)
    val chequeador = new Chequeador(programa, List(reglaDebeRetornarAlgo))

    //assert(!List().exists(p => p.isInstanceOf[Retornar]))
    assert(chequeador.chequear == List(Problema("La funcion no tiene retorno", Error(), funcion)))
  }

  "el chequeador" should "detectar que pese a haber instrucciones no Retorna nada" in {
    val funcion = Funcion("suma", List(Parametro("a"), Parametro("b")), List(Suma(Numero(1), Numero(2))))
    val reglaDebeRetornarAlgo = new DebeRetornarAlgo
    val programa = Programa(funcion)
    val chequeador = new Chequeador(programa, List(reglaDebeRetornarAlgo))

    //assert(!List().exists(p => p.isInstanceOf[Retornar]))
    assert(chequeador.chequear == List(Problema("La funcion no tiene retorno", Error(), funcion)))
  }

  "el chequeador" should "detectar que hay instrucciones despues del retorno" in {
    val funcion = Funcion("FuncionTest", List(Parametro("a"), Parametro("b")), List(Retornar(Suma(Numero(1), Numero(1))), Resta(Numero(4), Numero(1))))
    val reglaRetornoSiempreUltimo = new RetornoSiempreUltimo
    val programa = Programa(funcion)
    val chequeador = new Chequeador(programa, List(reglaRetornoSiempreUltimo))

    assert(chequeador.chequear == List(Problema("La funcion posee instrucciones despues del retorno", Error(), funcion)))
  }

  "el chequeador" should "detectar nombres de parametros duplicados" in {
    val funcion = Funcion("FuncionTest", List(Parametro("a"), Parametro("a")), List())
    val reglaNombreParametrosDuplicados = new NombreParametrosDuplicados
    val programa = Programa(funcion)
    val chequeador = new Chequeador(programa, List(reglaNombreParametrosDuplicados))

    assert(chequeador.chequear == List(Problema("nombres de parametros duplicados", Error(), Parametro("a"))))
  }
  */

}