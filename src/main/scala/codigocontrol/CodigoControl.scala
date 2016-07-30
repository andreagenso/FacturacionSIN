package codigocontrol


/**
  * V 7
  */
class CodigoControl(var numeroAutorizacion: String, var numeroFactura: String, var nitCi: String,
                    var fechaTransacion: String, var montoTransaccion: String, var llaveDosificacion:String) {

  def generar(): String = {
    montoTransaccion = round(montoTransaccion)

    println("PASO 1 ----------------------------------")
    println("Nro de autorizacion " + numeroAutorizacion)
    println("Nro Factura " + numeroFactura)
    println("NIT " + nitCi)
    println("Fecha de transaccion " + fechaTransacion)
    println("Monto de transaccion " + montoTransaccion)
    println("Llave de dosificacion " + llaveDosificacion)

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

    // agregar 5 digitos Verhoeff a la sumatoria
    val sumatoria5Verhoeff = agregarNVerhoeff(sumatoria.toString,5)

    println(" - Vernoheff -")
    println("Nro de autorizacion " + numeroAutorizacion)
    println("Nro Factura " + numeroFactura)
    println("NIT " + nitCi)
    println("Fecha de transaccion " + fechaTransacion)
    println("Monto de transaccion " + montoTransaccion)
    println("Suma  " + sumatoria5Verhoeff)

    // Paso 2
    println("PASO 2 ----------------------------------")
    val _5DigitosVerhoeff = sumatoria5Verhoeff.substring(sumatoria5Verhoeff.size-5)
    println(" _5DigitosVerhoeff " + _5DigitosVerhoeff)
    // largo de las cadenas, suma 1 a cada digito verhoeff
    val suma1Verhoeff = _5DigitosVerhoeff.split("").map(_.toInt+1)
    println("suma1Verhoeff " + suma1Verhoeff.foldLeft("")((a,b) => {a + "" + b }))

    val substDosificacion1 = llaveDosificacion.substring(0, suma1Verhoeff(0))
    val substDosificacion2 = llaveDosificacion.substring(suma1Verhoeff(0), suma1Verhoeff(0) + suma1Verhoeff(1))
    val substDosificacion3 = llaveDosificacion.substring(suma1Verhoeff(0) + suma1Verhoeff(1),
      suma1Verhoeff(0)+suma1Verhoeff(1) + suma1Verhoeff(2))
    val substDosificacion4 = llaveDosificacion.substring(suma1Verhoeff(0)+suma1Verhoeff(1) + suma1Verhoeff(2),
      suma1Verhoeff(0)+suma1Verhoeff(1) + suma1Verhoeff(2) + suma1Verhoeff(3))
    val substDosificacion5 = llaveDosificacion.substring(suma1Verhoeff(0)+suma1Verhoeff(1) + suma1Verhoeff(2) +
      suma1Verhoeff(3), suma1Verhoeff(0)+suma1Verhoeff(1) + suma1Verhoeff(2) + suma1Verhoeff(3) + suma1Verhoeff(4))

    println("substDosificacion 1 " + substDosificacion1)
    println("substDosificacion 2 " + substDosificacion2)
    println("substDosificacion 3 " + substDosificacion3)
    println("substDosificacion 4 " + substDosificacion4)
    println("substDosificacion 5 " + substDosificacion5)

    numeroAutorizacion = numeroAutorizacion + substDosificacion1
    numeroFactura = numeroFactura + substDosificacion2
    nitCi = nitCi + substDosificacion3
    fechaTransacion = fechaTransacion + substDosificacion4
    montoTransaccion = montoTransaccion + substDosificacion5

    println("numeroAutorizacion  " + numeroAutorizacion)
    println("numeroFactura " + numeroFactura)
    println("nitCi  " + nitCi)
    println("fechaTransacion " + fechaTransacion)
    println("montoTransaccion " + montoTransaccion)

    // Paso 3
    println("PASO 3 ----------------------------------")
    val cadenaConcatenada = numeroAutorizacion + numeroFactura + nitCi + fechaTransacion + montoTransaccion
    val llaveCifrado = llaveDosificacion + _5DigitosVerhoeff

    println(" cadena concatenada " + cadenaConcatenada)
    println(" llave de cifrado " +  llaveCifrado)

    val claveAllegedRC4: String = (new AllegedRC4).CifrarMensajeRC4(cadenaConcatenada,llaveCifrado).filter(c => c != '-')
    println(" claveAllegedRC4 " + claveAllegedRC4)

    // Paso 4
    val claveAllegerRC4Array: Array[Char] = claveAllegedRC4.split("").toArray.map(_.toCharArray).flatten

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

    println(" sumatoria total " + sumatoriaTotal)
    println(" sumatoria parcial 1 " + sumatoriaParcial1)
    println(" sumatoria parcial 2 " + sumatoriaParcial2)
    println(" sumatoria parcial 3 " + sumatoriaParcial3)
    println(" sumatoria parcial 4 " + sumatoriaParcial4)
    println(" sumatoria parcial 5 " + sumatoriaParcial5)

    // Paso 5
    println(" paso 5 --------------------- ")
    val stParcial1: Long = sumatoriaTotal * sumatoriaParcial1 / suma1Verhoeff(0)
    val stParcial2: Long = sumatoriaTotal * sumatoriaParcial2 / suma1Verhoeff(1)
    val stParcial3: Long = sumatoriaTotal * sumatoriaParcial3 / suma1Verhoeff(2)
    val stParcial4: Long = sumatoriaTotal * sumatoriaParcial4 / suma1Verhoeff(3)
    val stParcial5: Long = sumatoriaTotal * sumatoriaParcial5 / suma1Verhoeff(4)

    println(" stParcial1 " + stParcial1)
    println(" stParcial2 " + stParcial2)
    println(" stParcial3 " + stParcial3)
    println(" stParcial4 " + stParcial4)
    println(" stParcial5 " + stParcial5)

    val sumaProducto: Long = (stParcial1 + stParcial2 + stParcial3
      + stParcial4 + stParcial5)

    println(" sumaProducto  " + sumaProducto)

    val base64 = (new Base64).obtenerBase64(sumaProducto)

    println(" base 64 " + base64)

    // paso 6
    println(" paso 6 --------------------- ")
    val allegedRC4 = (new AllegedRC4).CifrarMensajeRC4(base64,llaveCifrado)
    println(" alleged " + allegedRC4)
    allegedRC4
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
