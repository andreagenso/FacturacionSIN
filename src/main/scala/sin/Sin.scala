package sin

import java.text.DecimalFormatSymbols

import codigocontrol.CodigoControl

import scala.util.control.Breaks

object InteraccionTeclado extends Enumeration {
  type InteraccionTeclado = Value
  val CONTINUAROPCION, CONTINUARFACTURA,SALIR = Value
}

object DatoFactura extends Enumeration {
  type DatoFactura = Value
  val NINGUNO, NUMEROAUTORIZACION, NUMEROFACTURA, NITCI, FECHATRANSACCION, MONTOTRANSACCION, LLAVEDOSIFICACION = Value
}

object FacturacionSIN extends App {

  var continuar = InteraccionTeclado.CONTINUAROPCION
  var datoFactura = DatoFactura.NINGUNO
  var inFactura:String = ""

  println(" ******* SISTEMA DE CRIPTOGRAFIA - IMPUESTOS NACIONALES - BOLIVIA ********")
  println("OPCIONES:")
  println("\t +Q -> Salir")
  println("\t +N -> Nuevos datos de Factura")
  println("\t +A -> Correr 5000 casos")
  println("\t +V -> Ver opciones")

  while(continuar.equals(InteraccionTeclado.CONTINUARFACTURA) ||  continuar.equals(InteraccionTeclado.CONTINUAROPCION)) {

    var in = ""

    if (continuar.equals(InteraccionTeclado.CONTINUAROPCION)){
      println("INGRESE OPCION (+N +Q +V +A):")
      in = readLine()
    }
    else {
      datoFactura = DatoFactura.NINGUNO
      in = "+N"
    }

    in match {
      case "+A" =>
        Casos.test5000Casos()
      case "+Q" =>
        continuar = InteraccionTeclado.SALIR
        println(" ****** --- ADIOS! --- ********* ")
      case "+N" =>
        val loop = new Breaks
        loop.breakable{
          println("NÚMERO DE AUTORIZACIÓN:")
          var numeroAutorizacion: String = ""
          datoFactura = DatoFactura.NUMEROAUTORIZACION
          validarLectura("NÚMERO DE AUTORIZACIÓN:", datoFactura) match {
            case (true, res) =>
              numeroAutorizacion = res
            case _ =>
              loop.break()
          }

          println("NÚMERO DE FACTURA:")
          var numeroFactura: String = ""
          datoFactura = DatoFactura.NUMEROFACTURA
          validarLectura("NÚMERO DE FACTURA:", datoFactura) match {
            case (true, res) =>
              numeroFactura = res
            case _ =>
              loop.break()
          }

          println("NIT/CI:")
          var nitCi: String = ""
          datoFactura = DatoFactura.NITCI
          validarLectura("NIT/CI:", datoFactura) match {
            case (true, res) =>
              nitCi = res
            case _ =>
              loop.break()
          }

          println("FECHA DE TRANSACCIÓN(YYYY/MM/DD):")
          var fechaTransacion: String = ""
          datoFactura = DatoFactura.FECHATRANSACCION
          validarLectura("FECHA DE TRANSACCIÓN(AAAAMMDD):", datoFactura) match {
            case (true, res) =>
              fechaTransacion = res
            case _ =>
              loop.break()
          }

          println("MONTO DE TRANSACCIÓN:")
          var montoTransaccion: String = ""
          datoFactura = DatoFactura.MONTOTRANSACCION
          validarLectura("MONTO DE TRANSACCIÓN:", datoFactura) match {
            case (true, res) =>
              montoTransaccion = res
            case _ =>
              loop.break()
          }

          println("LLAVE DE DOSIFICACIÓN:")
          var llaveDosificacion:String = ""
          datoFactura = DatoFactura.LLAVEDOSIFICACION
          validarLectura("LLAVE DE DOSIFICACIÓN:", datoFactura) match {
            case (true, res) =>
              llaveDosificacion = res
            case _ =>
              loop.break()
          }

          val codigosCifrados = new CodigoControl(numeroAutorizacion,numeroFactura,nitCi,
            fechaTransacion,montoTransaccion,llaveDosificacion).generar()

          println(" RESULTADOS: " +
            "\n\tVERHOEFF -> " + codigosCifrados._1 +
            "\n\tCADENA ->" + codigosCifrados._2 +
            "\n\tSUMATORIA PRODUCTO: " + codigosCifrados._3 +
            "\n\tBASE 64: " + codigosCifrados._4 +
            "\n\tCODIGO CONTROL: " + codigosCifrados._5
          )
          println(" ****** --- GRACIAS POR SU COMPRA --- ********* ")

        }
      case "+V" =>
        println("OPCIONES:")
        println("\t +Q -> Salir")
        println("\t +N -> Nuevos datos de Factura")
        println("\t +V -> Ver opciones")

      case _ =>
        println("OPCIÓN NO VÁLIDA (+Q - Salir /  +N - Nuevos datos de Factura /  +V - Ver opciones)")
    }
  }

  private def validarLectura(nombreParametro: String, datoFactura: DatoFactura.Value): (Boolean, String) = {
    val in = readLine()
    in match {
      case "+A" =>
        Casos.test5000Casos()
        (false, in)
      case "+Q" =>
        continuar = InteraccionTeclado.SALIR
        println(" ****** --- ADIOS! --- ********* ")
        (false, in)
      case "+N" =>
        continuar = InteraccionTeclado.CONTINUARFACTURA
        (false, in)
      case "+V" =>
        println("OPCIONES:")
        println("\t +Q -> Salir")
        println("\t +N -> Nuevos datos de Factura")
        println("\t +V -> Ver opciones")
        println("")
        println("!! INGRESE UNA CADENA VÁLIDA PARA: " + nombreParametro + "!!")
        validarLectura(nombreParametro, datoFactura)
      case "" =>
        println("!! INGRESE UNA CADENA VÁLIDA PARA: " + nombreParametro + "!!")
        validarLectura(nombreParametro, datoFactura)
      case _  =>
          datoFactura match {
            case DatoFactura.NUMEROAUTORIZACION =>
              try {
                val amount = in.toLong
                (true, in)
              } catch {
                case (e:Exception) =>
                  println("FORMATO NO VALIDO PARA NUMERO DE AUTORIZACIÓN, INGRESE UN NÚMERO")
                  validarLectura(nombreParametro, datoFactura)
              }
            case DatoFactura.NUMEROFACTURA =>
              try {
                val amount = in.toLong
                (true, in)
              } catch {
                case (e:Exception) =>
                  println("FORMATO NO VALIDO PARA NÚMERO DE FACTURA, INGRESE UN NÚMERO")
                  validarLectura(nombreParametro, datoFactura)
              }
            case DatoFactura.NITCI =>
              try {
                val amount = in.toLong
                (true, in)
              } catch {
                case (e:Exception) =>
                  println("FORMATO NO VALIDO PARA NIT/CI, INGRESE UN NÚMERO")
                  validarLectura(nombreParametro, datoFactura)
              }
            case DatoFactura.MONTOTRANSACCION =>
              val dfs: DecimalFormatSymbols = new DecimalFormatSymbols
              dfs.setDecimalSeparator(',')
              dfs.setGroupingSeparator('.')
              val format = new java.text.DecimalFormat("#,##0.00;(#,##0.00)")
              format.setDecimalFormatSymbols(dfs)

              try {
                val amount = format.parse(in)
                (true, amount.toString)
              } catch {
                case (e:Exception) =>
                  println("FORMATO NO VALIDO PARA MONTO DE TRANSACCIÓN, INGRESE EL FORMATO: ###.###,##")
                  validarLectura(nombreParametro, datoFactura)
              }
            case DatoFactura.FECHATRANSACCION =>
              val format = new java.text.SimpleDateFormat("yyyy/MM/dd")
              try {
                val date = format.parse(in)
                (true, in.filter(v => v != '/'))
              } catch {
                case (e: IllegalArgumentException) =>
                  println("FORMATO NO VALIDO PARA FECHA DE TRANSACCIÓN, INGRESE EL FORMATO: YYYY/MM/DD")
                  validarLectura(nombreParametro, datoFactura)
                case (e:Exception) =>
                  println("FORMATO NO VALIDO PARA FECHA DE TRANSACCIÓN, INGRESE EL FORMATO: YYYY/MM/DD")
                  validarLectura(nombreParametro, datoFactura)
              }
            case DatoFactura.LLAVEDOSIFICACION =>
              if (in.size < 64) {
                println("LLAVE DE DOSIFICACIÓN INCORRECTA, INGRESE UNA LLAVE VÁLIDA (64 CARACTERES)")
                validarLectura(nombreParametro, datoFactura)
              }
              else
                (true, in)
            case _ =>
              println("OPCION DESCONOCIDA, ESCRIBA +N")
              (false, in)
          }
    }
  }

}