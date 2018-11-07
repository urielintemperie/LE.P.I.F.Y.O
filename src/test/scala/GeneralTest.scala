package Lenguaje
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

}