import collection.mutable.Stack
import org.scalatest._
import codigocontrol.Verhoeff

class TestVerhoeff extends FlatSpec {

  "1. DigitoVerificador = ObtenerVerhoeff(12083)" should "Resultado: DigitoVerificador = 7" in {
    val verhoeff = (new Verhoeff).obtenerVerhoeff("12083")
    val stack = new Stack[Int]
    stack.push(verhoeff)
    assert(stack.pop() === 7)
  }

  "2. DigitoVerificador = ObtenerVerhoeff(0)" should "Resultado: DigitoVerificador = 1" in {
    val verhoeff = (new Verhoeff).obtenerVerhoeff("0")
    val stack = new Stack[Int]
    stack.push(verhoeff)
    assert(stack.pop() === 4)
  }

  "3. DigitoVerificador = ObtenerVerhoeff(1810)" should "Resultado: DigitoVerificador = 8" in {
    val verhoeff = (new Verhoeff).obtenerVerhoeff("1810")
    val stack = new Stack[Int]
    stack.push(verhoeff)
    assert(stack.pop() === 8)
  }

  "4. DigitoVerificador = ObtenerVerhoeff(04)" should "Resultado: DigitoVerificador = 7" in {
    val verhoeff = (new Verhoeff).obtenerVerhoeff("04")
    val stack = new Stack[Int]
    stack.push(verhoeff)
    assert(stack.pop() === 7)
  }

}