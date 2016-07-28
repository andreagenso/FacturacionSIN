import collection.mutable.Stack
import org.scalatest._
import codigocontrol.Base64

class TestBase64 extends FlatSpec {

  "1. Palabra = ObtenerBase64(934598591)" should "Resultado: Palabra = \"tjDU/\"" in {
    val base64 = (new Base64).obtenerBase64(934598591)
    val stack = new Stack[String]
    stack.push(base64)
    assert(stack.pop() === "tjDU/")
  }

  "2. Palabra = ObtenerBase64(434376710)" should "Resultado: Palabra = \"Pv106\"" in {
    val base64 = (new Base64).obtenerBase64(434376710)
    val stack = new Stack[String]
    stack.push(base64)
    assert(stack.pop() === "Pv106")
  }

  "3. Palabra = ObtenerBase64(204986118)" should "Resultado: Palabra = \"CDzS6\"" in {
    val base64 = (new Base64).obtenerBase64(204986118)
    val stack = new Stack[String]
    stack.push(base64)
    assert(stack.pop() === "CDzS6")
  }

}




