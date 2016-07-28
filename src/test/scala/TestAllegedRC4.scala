import collection.mutable.Stack
import org.scalatest._
import codigocontrol.AllegedRC4

class TestAllegedRC4 extends FlatSpec {

   "1. CadenaCifrada = CifrarMensajeRC4 (“d3Ir6”, “sesamo”)" should "Resultado: CadenaCifrada = EB-06-AE-F8-92" in {
    val allegedRC4 = (new AllegedRC4).CifrarMensajeRC4("d3Ir6", "sesamo")
    val stack = new Stack[String]
    stack.push(allegedRC4)
    assert(stack.pop() === "EB-06-AE-F8-92")
  }

  "2. CadenaCifrada = CifrarMensajeRC4 (“piWCp”, “Aa1-bb2-Cc3-Dd4”)" should "Resultado: CadenaCifrada = 37-71-2E-14-A0" in {
    val allegedRC4 = (new AllegedRC4).CifrarMensajeRC4("piWCp", "Aa1-bb2-Cc3-Dd4")
    val stack = new Stack[String]
    stack.push(allegedRC4)
    assert(stack.pop() === "37-71-2E-14-A0")
  }

  "3. CadenaCifrada = CifrarMensajeRC4 (“IUKYo”, “XBCPY-GKGX4-PGK44-8B632-X9P33”)" should "Resultado: CadenaCifrada = 83-62-FC-B0-F0" in {
    val allegedRC4 = (new AllegedRC4).CifrarMensajeRC4("IUKYo", "XBCPY-GKGX4-PGK44-8B632-X9P33")
    val stack = new Stack[String]
    stack.push(allegedRC4)
    assert(stack.pop() === "83-62-FC-B0-F0")
  }

}
