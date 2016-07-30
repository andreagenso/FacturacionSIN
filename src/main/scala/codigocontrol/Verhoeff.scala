package codigocontrol

import scala.Array

class Verhoeff {
  // matriz de multiplicaciones
  val mul: Array[Array[Int]] = Array[Array[Int]] (Array(0, 1, 2, 3, 4, 5, 6, 7, 8, 9),
                                                  Array(1, 2, 3, 4, 0, 6, 7, 8, 9, 5),
                                                  Array(2, 3, 4, 0, 1, 7, 8, 9, 5, 6),
                                                  Array(3, 4, 0, 1, 2, 8, 9, 5, 6, 7),
                                                  Array(4, 0, 1, 2, 3, 9, 5, 6, 7, 8),
                                                  Array(5, 9, 8, 7, 6, 0, 4, 3, 2, 1),
                                                  Array(6, 5, 9, 8, 7, 1, 0, 4, 3, 2),
                                                  Array(7, 6, 5, 9, 8, 2, 1, 0, 4, 3),
                                                  Array(8, 7, 6, 5, 9, 3, 2, 1, 0, 4),
                                                  Array(9, 8, 7, 6, 5, 4, 3, 2, 1, 0))

  // tabla de permutacion
  val per: Array[Array[Int]] = Array[Array[Int]](Array(0, 1, 2, 3, 4, 5, 6, 7, 8, 9),
                                                 Array(1, 5, 7, 6, 2, 8, 3, 0, 9, 4),
                                                 Array(5, 8, 0, 3, 7, 9, 6, 1, 4, 2),
                                                 Array(8, 9, 1, 6, 0, 4, 3, 5, 2, 7),
                                                 Array(9, 4, 5, 3, 1, 2, 6, 8, 7, 0),
                                                 Array(4, 2, 8, 6, 5, 7, 3, 9, 0, 1),
                                                 Array(2, 7, 9, 3, 8, 0, 6, 4, 1, 5),
                                                 Array(7, 0, 4, 6, 9, 1, 3, 2, 5, 8))

  // array inversa
  val inv: Array[Int] = Array(0, 4, 3, 2, 1, 5, 6, 7, 8, 9)

  def obtenerVerhoeff(cifra: String): Int = {

    val numeroInvertido: Array[Int] = invierteNumero(cifra)

    var check = 0
    for (i <- 0 to largoNumero(numeroInvertido) - 1){
      check = mul(check)(per((i+1)%8)(numeroInvertido(i).toInt))
    }

    inv(check)
  }

  private def invierteNumero(cifra: String): Array[Int] = cifra.split("").toArray.map(_.toInt).reverse

  private def largoNumero(numeroInvertido: Array[Int]): Int = numeroInvertido.size

}
