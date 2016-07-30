package sin

import java.io.{BufferedReader, FileReader}
import codigocontrol.CodigoControl

object FacturacionSIN extends App {

  val codigoControl: CodigoControl = null
  val file: String = "C:\\IdeaProjects\\FacturacionSIN\\doc\\5000CasosPruebaCCVer7.txt"
  var cont =0

  try{
    val br: BufferedReader = new BufferedReader(new FileReader(file))
    var linea: String = ""
    linea = br.readLine()

    while (cont < 5000) {
      cont += 1
      linea = br.readLine()
      //reemplaza "|" por "/-/" por no ser compatible con el metodo split
      val array = linea.replace("|", "/-/").split("/-/").filter(v => v != "/-/")

      val codigoControlGenerado = new CodigoControl(array(0), array(1), array(2), array(3).replace("/", ""),
        array(4), array(5)).generar()
      val codigoControlLeido = array(10)

      println(cont + " Codigo de control leido: " + codigoControlLeido +
        " Codigo de control generado " + codigoControlGenerado + " iguales -> " +
        (codigoControlGenerado == codigoControlLeido) )

    }
  } catch {
    case (e: java.io.IOException) =>
    println(e.getMessage());
  }


}