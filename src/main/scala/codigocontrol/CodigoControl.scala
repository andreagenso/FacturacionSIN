package codigocontrol

class CodigoControl(var numeroAutorizacion: String, var numeroFactura: String, var nitCi: String,
                    var fechaTransacion: String, var montoTransaccion: String, var llaveDosificacion:String) {

  def generar(): String = {
    montoTransaccion = round(montoTransaccion)

    // Paso 1
    val sumatoria5Verhoeff = tratamientoVerhoeff()

    // Paso 2
    val _5DigitosVerhoeff = sumatoria5Verhoeff.substring(sumatoria5Verhoeff.size-5)
    val suma1Verhoeff = _5DigitosVerhoeff.split("").map(_.toInt+1)

    // Paso 3
    val cadenaConcatenada = tratamientoLlaveDosificacion(suma1Verhoeff)
    val llaveCifrado = llaveDosificacion + _5DigitosVerhoeff

    val claveAllegedRC4: String = (new AllegedRC4).CifrarMensajeRC4(cadenaConcatenada,llaveCifrado).filter(c => c != '-')

    // Paso 4 / 5
    val claveAllegerRC4Array: Array[Char] = claveAllegedRC4.split("").toArray.map(_.toCharArray).flatten

    val sumaProducto: Long = sumaProductoAscii(claveAllegerRC4Array, suma1Verhoeff)
    val base64 = (new Base64).obtenerBase64(sumaProducto)

    // Paso 6
    val allegedRC4 = (new AllegedRC4).CifrarMensajeRC4(base64,llaveCifrado)
    allegedRC4
  }

  private def tratamientoVerhoeff(): String = {
    // Paso 1 - obtener y concatenar consecutivamente 2 digitos verhoeff
    numeroFactura = agregarNVerhoeff(numeroFactura,2)
    nitCi = agregarNVerhoeff(nitCi,2)
    fechaTransacion = agregarNVerhoeff(fechaTransacion,2)
    montoTransaccion = agregarNVerhoeff(montoTransaccion,2)

    // sumar
    val sumatoria = numeroFactura.toLong +
      nitCi.toLong +
      fechaTransacion.toLong +
      montoTransaccion.toLong

    // agregar 5 digitos Verhoeff y retornar
    agregarNVerhoeff(sumatoria.toString,5)
  }

  /**
    * Paso 4 y 5
    * @param claveAllegerRC4Array
    * @param suma1Verhoeff
    * @return
    */
  private def sumaProductoAscii(claveAllegerRC4Array: Array[Char], suma1Verhoeff: Array[Int]): Long = {
    var sumatoriaTotal = 0
    var sumatoriaParcial1 = 0
    var sumatoriaParcial2 = 0
    var sumatoriaParcial3 = 0
    var sumatoriaParcial4 = 0
    var sumatoriaParcial5 = 0
    var cont = 1

    claveAllegerRC4Array.foreach(c => {
      sumatoriaTotal += c.toInt

      cont match {
        case 1 =>
          sumatoriaParcial1 += c.toInt
        case 2 =>
          sumatoriaParcial2 += c.toInt
        case 3 =>
          sumatoriaParcial3 += c.toInt
        case 4 =>
          sumatoriaParcial4 += c.toInt
        case _ =>
          sumatoriaParcial5 += c.toInt
      }

      cont = if(cont<5) cont+1 else 1
    })


    // Paso 5
    val stParcial1: Long = sumatoriaTotal * sumatoriaParcial1 / suma1Verhoeff(0)
    val stParcial2: Long = sumatoriaTotal * sumatoriaParcial2 / suma1Verhoeff(1)
    val stParcial3: Long = sumatoriaTotal * sumatoriaParcial3 / suma1Verhoeff(2)
    val stParcial4: Long = sumatoriaTotal * sumatoriaParcial4 / suma1Verhoeff(3)
    val stParcial5: Long = sumatoriaTotal * sumatoriaParcial5 / suma1Verhoeff(4)


    val sumaProducto: Long = (stParcial1 + stParcial2 + stParcial3
      + stParcial4 + stParcial5)

    sumaProducto
  }

  /**
    * @param suma1Verhoeff
    * realiza el tratamiento de llave de dosificacion correspondiente al paso 2
    */
  private def tratamientoLlaveDosificacion(suma1Verhoeff: Array[Int]): String = {

    val substDosificacion1 = llaveDosificacion.substring(0, suma1Verhoeff(0))
    val substDosificacion2 = llaveDosificacion.substring(suma1Verhoeff(0), suma1Verhoeff(0) + suma1Verhoeff(1))
    val substDosificacion3 = llaveDosificacion.substring(suma1Verhoeff(0) + suma1Verhoeff(1),
      suma1Verhoeff(0)+suma1Verhoeff(1) + suma1Verhoeff(2))
    val substDosificacion4 = llaveDosificacion.substring(suma1Verhoeff(0)+suma1Verhoeff(1) + suma1Verhoeff(2),
      suma1Verhoeff(0)+suma1Verhoeff(1) + suma1Verhoeff(2) + suma1Verhoeff(3))
    val substDosificacion5 = llaveDosificacion.substring(suma1Verhoeff(0)+suma1Verhoeff(1) + suma1Verhoeff(2) +
      suma1Verhoeff(3), suma1Verhoeff(0)+suma1Verhoeff(1) + suma1Verhoeff(2) + suma1Verhoeff(3) + suma1Verhoeff(4))

    numeroAutorizacion = numeroAutorizacion + substDosificacion1
    numeroFactura = numeroFactura + substDosificacion2
    nitCi = nitCi + substDosificacion3
    fechaTransacion = fechaTransacion + substDosificacion4
    montoTransaccion = montoTransaccion + substDosificacion5

    numeroAutorizacion + numeroFactura + nitCi + fechaTransacion + montoTransaccion
  }

  private def agregarNVerhoeff(value: String, n: Int): String = {
    var res = value
    for(i<- 0 to n-1){
      res += (new Verhoeff).obtenerVerhoeff(res)
    }
    return res
  }

  private def round(in: String): String = {
    var value = in
    value = value.replace(",", ".")
    var valueBD: java.math.BigDecimal = new java.math.BigDecimal(value.toDouble)
    valueBD = valueBD.setScale(0, java.math.BigDecimal.ROUND_HALF_UP)
    String.valueOf(valueBD)
  }

}
