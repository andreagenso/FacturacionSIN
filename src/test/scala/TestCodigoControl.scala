import collection.mutable.Stack
import org.scalatest._
import codigocontrol.CodigoControl

class TestCodigoControl extends FlatSpec {
  "Ejemplo 1" should "Resultado: FB-A6-E4-78" in {
    val codigoControl = (new CodigoControl("79040011859","152","1026469026","20070728","135",
      "A3Fs4s$)2cvD(eY667A5C4A2rsdf53kw9654E2B23s24df35F5")).generar()
    val stack = new Stack[String]
    stack.push(codigoControl)
    assert(stack.pop() === "FB-A6-E4-78")
  }

  "Ejemplo 2" should "Resultado: 71-D5-61-C8" in {
    val codigoControl = (new CodigoControl("20040010113","665","1004141023","20070108","905.23",
      "442F3w5AggG7644D737asd4BH5677sasdL4%44643(3C3674F4")).generar()
    val stack = new Stack[String]
    stack.push(codigoControl)
    assert(stack.pop() === "71-D5-61-C8")
  }

  "Ejemplo 3" should "Resultado: 62-12-AF-1B" in {
    val codigoControl = (new CodigoControl("1904008691195","978256","0","20080201","26006",
      "pPgiFS%)v}@N4W3aQqqXCEHVS2[aDw_n%3)pFyU%bEB9)YXt%xNBub4@PZ4S9)ct")).generar()
    val stack = new Stack[String]
    stack.push(codigoControl)
    assert(stack.pop() === "62-12-AF-1B")
  }

  "Ejemplo 4" should "Resultado: 6A-50-31-01-32" in {
    val codigoControl = (new CodigoControl("10040010640","9901","1035012010","20070813","451,49",
      "DSrCB7Ssdfv4X29d)5k7N%3ab8p3S(asFG5YU8477SWW)FDAQA")).generar()
    val stack = new Stack[String]
    stack.push(codigoControl)
    assert(stack.pop() === "6A-50-31-01-32")
  }

  "Ejemplo 5" should "Resultado: A8-6B-FD-82-16" in {
    val codigoControl = (new CodigoControl("30040010595","10015","953387014","20070825","5725,90",
      "33E265B43C4435sdTuyBVssD355FC4A6F46sdQWasdA)d56666fDsmp9846636B3")).generar()
    val stack = new Stack[String]
    stack.push(codigoControl)
    assert(stack.pop() === "A8-6B-FD-82-16")
  }
}
