package codigocontrol

class AllegedRC4 {

  def CifrarMensajeRC4(mensaje: String, key: String): String = {
    val state: Array[Int] = new Array[Int](256)
    var x: Int = 0
    var y: Int = 0
    var index1: Int = 0
    var index2: Int = 0
    var nMen: Int = 0
    var mensajeCifrado: String = ""

    for (i <- 0 to 255){
      state(i) = i
    }

    for (i <- 0 to 255){
      index2 = ( getAsccii(key(index1)) + state(i) + index2 ) % 256
      //IntercambiaValor( State[I], State[Index2] )
      val intercambiaVal = state(i)
      state(i) = state(index2)
      state(index2) = intercambiaVal

      index1 = (index1 + 1) % key.size
    }

    for (i <- 0 to mensaje.size -1){
      x = (x + 1) % 256
      y = (state(x) + y) % 256

      //IntercambiaValor( State[X] , State[Y] )
      val intercambiaVal = state(x)
      state(x) = state(y)
      state(y) = intercambiaVal

      // NMen = ObtieneASCII(Mensaje[I]) XOR State[(State[X] + State[Y]) MODULO 256]
      nMen = getAsccii(mensaje(i)) ^ state((state(x) + state(y)) % 256)
      //MensajeCifrado = MensajeCifrado + "-" + RellenaCero(ConvierteAHexadecimal(NMen))
      mensajeCifrado = mensajeCifrado + "-" + rellenarCero(convierteAHexadecimal(nMen))
    }

    mensajeCifrado.substring(1, mensajeCifrado.size)
  }

  private def getAsccii(char: Char): Int = char.toInt
  private def rellenarCero(msj: String): String = if (msj.size == 1) "0"+msj else msj
  private def convierteAHexadecimal(nMen: Int): String = Integer.toHexString(nMen).toUpperCase()
}
