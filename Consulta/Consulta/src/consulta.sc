object IntegracionSimpson {

  def integracion(f: Double => Double, a: Double, b: Double): Double = {
    val xMedia = (a + b) / 2.0
    val resultado = (b - a) * (f(a) + 4 * f(xMedia) + f(b)) / 6.0
    resultado
  }

  def calcularError(valorEsperado: Double, valorObtenido: Double): Double = {
    Math.abs(valorEsperado - valorObtenido)
  }

  def f1(x: Double): Double = -x*x + 8*x - 12

  def f2(x: Double): Double = x * x

  def f3(x: Double): Double = x*x*x + 2*x*x - x - 2

  def f4(x: Double): Double = 3*x*x + 2*x + 1

  def f5(x: Double): Double = 2*x + 1

  def f6(x: Double): Double = Math.sin(x)

  def f7(x: Double): Double = 1.0 / x

  def main(args: Array[String]): Unit = {
    println("=" * 70)
    println("INTEGRACIÓN NUMÉRICA - MÉTODO DE SIMPSON 1/3")
    println("=" * 70)

    println("\n1. ∫₃⁵ (-x² + 8x - 12)dx")
    val aprox1 = integracion(f1, 3, 5)
    val real1 = 22.0 / 3.0
    val error1 = calcularError(real1, aprox1)
    println(f"   Valor aproximado: $aprox1%.6f")
    println(f"   Valor real: $real1%.6f")
    println(f"   Error: $error1%.6f")

    println("\n2. ∫₀³ x²dx")
    val aprox2 = integracion(f2, 0, 3)
    val real2 = 9.0
    val error2 = calcularError(real2, aprox2)
    println(f"   Valor aproximado: $aprox2%.6f")
    println(f"   Valor real: $real2%.6f")
    println(f"   Error: $error2%.6f")

    println("\n3. ∫₋₁¹ (x³ + 2x² - x - 2)dx")
    val aprox3 = integracion(f3, -1, 1)
    val real3 = -4.0 / 3.0
    val error3 = calcularError(real3, aprox3)
    println(f"   Valor aproximado: $aprox3%.6f")
    println(f"   Valor real: $real3%.6f")
    println(f"   Error: $error3%.6f")

    println("\n4. ∫₀² (3x² + 2x + 1)dx")
    val aprox4 = integracion(f4, 0, 2)
    val real4 = 14.0
    val error4 = calcularError(real4, aprox4)
    println(f"   Valor aproximado: $aprox4%.6f")
    println(f"   Valor real: $real4%.6f")
    println(f"   Error: $error4%.6f")

    println("\n5. ∫₁⁴ (2x + 1)dx")
    val aprox5 = integracion(f5, 1, 4)
    val real5 = 18.0
    val error5 = calcularError(real5, aprox5)
    println(f"   Valor aproximado: $aprox5%.6f")
    println(f"   Valor real: $real5%.6f")
    println(f"   Error: $error5%.6f")

    println("\n6. ∫₀π sen(x)dx")
    val aprox6 = integracion(f6, 0, Math.PI)
    val real6 = 2.0
    val error6 = calcularError(real6, aprox6)
    println(f"   Valor aproximado: $aprox6%.6f")
    println(f"   Valor real: $real6%.6f")
    println(f"   Error: $error6%.6f")

    println("\n7. ∫₁³ (1/x)dx")
    val aprox7 = integracion(f7, 1, 3)
    val real7 = Math.log(3)
    val error7 = calcularError(real7, aprox7)
    println(f"   Valor aproximado: $aprox7%.6f")
    println(f"   Valor real: $real7%.6f")
    println(f"   Error: $error7%.6f")

    println("\n" + "=" * 70)
    println("RESUMEN DE ERRORES")
    println("=" * 70)
    val errores = List(error1, error2, error3, error4, error5, error6, error7)
    errores.zipWithIndex.foreach { case (error, idx) =>
      println(f"Integral ${idx + 1}: Error = $error%.6f")
    }

    val errorPromedio = errores.sum / errores.length
    println(f"\nError promedio: $errorPromedio%.6f")
    println("=" * 70)
  }
}
IntegracionSimpson.main(Array())