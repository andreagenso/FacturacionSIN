package codigocontrol

class Base64 {
  val diccionario: Array[String] = Array("0", "1", "2", "3", "4", "5", "6", "7", "8",
    "9", "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K",
    "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W",
    "X", "Y", "Z", "a", "b", "c", "d", "e", "f", "g", "h", "i",
    "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u",
    "v", "w", "x", "y", "z", "+", "/")

  def obtenerBase64(value: Long): String = {
    var varValue = value

    var cociente: Long = 1
    var resto: Int = 1
    var palabra: String = ""

    while (cociente > 0) {
      {
        cociente = varValue / 64
        resto = (varValue % 64).toInt
        palabra = diccionario(resto) + palabra
        varValue = cociente
      }
    }
    return palabra
  }
}
