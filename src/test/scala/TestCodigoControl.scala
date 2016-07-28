import collection.mutable.Stack
import org.scalatest._
import codigocontrol.CodigoControl

class TestCodigoControl extends FlatSpec {
  "Ejemplo 1" should "Resultado: DF-CC-68-8D" in {
    val codigoControl = (new CodigoControl("79040011859","152","1026469026","20070728","135","A3Fs4s$)2cvD(eY667A5C4A2rsdf53kw9654E2B23s24df35F5")).generar()
    val stack = new Stack[String]
    stack.push(codigoControl)
    assert(stack.pop() === "DF-CC-68-8D")
  }
}