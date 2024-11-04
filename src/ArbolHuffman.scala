trait ArbolHuffman {
  def peso: Int = this match
    case RamaHuffman(nodoIzq, nodoDch) => nodoIzq.peso + nodoDch.peso
    case HojaHuffman(c, p) => p

  def caracteres: List[Char] =
    def caracteresAux(arbol2: ArbolHuffman, caract: String): String = arbol2 match
      case RamaHuffman(nodoIzq, nodoDcha) => caracteresAux(nodoIzq, caract) + caracteresAux(nodoDcha, caract)
      case HojaHuffman(caracter, peso) => caract + caracter
      case _ => throw new Error

    cadenaAListaChars(caracteresAux(this, ""))

  type Bit = 0 | 1

  def decodificar(bits: List[Bit]): String =
    def decodificarAux(arbolito: ArbolHuffman, cad: String, bitsaux: List[Bit]): String = arbolito match
      case RamaHuffman(nodoIzq, nodoDch) if bitsaux != Nil =>
        if bitsaux.head == 1 then decodificarAux(nodoDch, cad, bitsaux.tail)
        else decodificarAux(nodoIzq, cad, bitsaux.tail)

      case HojaHuffman(c, p) => decodificarAux(this, cad + c, bitsaux)
      case _ => cad

    decodificarAux(this, "", bits)

  def estaEnArbol(char: Char): Boolean = this match
    case RamaHuffman(nodoIzq, nodoDch) => caracteres.contains(char)
    case HojaHuffman(c, p) => c == char


  def codificar(cadena: String): List[Bit] =
    def auxCodificar(arbolito: ArbolHuffman, listChar: List[Char], res: List[Bit]): List[Bit] = arbolito match
      case RamaHuffman(nodoIzq, nodoDch) if listChar != Nil =>
        if nodoDch.estaEnArbol(listChar.head) then auxCodificar(nodoDch, listChar, res :+ 1)
        else if nodoIzq.estaEnArbol(listChar.head) then auxCodificar(nodoIzq, listChar, res :+ 0)
        else throw new Exception("No existe ese caracter en el arbol")

      case HojaHuffman(c, p) if listChar != Nil => auxCodificar(this, listChar.tail, res)

      case _ => res

    auxCodificar(this, cadena.toList, List())
}





case class RamaHuffman(nodoIzq: ArbolHuffman, nodoDch: ArbolHuffman) extends ArbolHuffman

case class HojaHuffman(c: Char, p: Int) extends ArbolHuffman

def crearArbolHuffman(cadena: String): ArbolHuffman =

  def cuantasVeces(listaChar: List[Char], char: Char): (Char, Int) =
    def aux(lista: List[Char], chart: Char, n: Int): Int = lista match
      case Nil => n
      case h :: t =>
        if h == chart then aux(lista.tail, chart, n + 1)
        else aux(lista.tail, chart, n)
  
    (char, aux(listaChar, char, 0))
  
  
  def ListaCharsADistFrec(listaChar: List[Char]): List[(Char, Int)] ={
    def aux(listChar: List[Char], lisTuplas: List[(Char, Int)]): List[(Char, Int)] = listChar match
      case Nil => lisTuplas
      case h :: t =>
        if lisTuplas.exists(_._1 == h) then aux(t, lisTuplas)
        else aux(t, lisTuplas :+ cuantasVeces(listChar, h))
  
    aux(listaChar, lisTuplas = Nil)}
  
  
  
  def DistribFrecAListaHojas(frec: List[(Char, Int)]): List[HojaHuffman] =
  
      def ordenar(hojita: HojaHuffman, list: List[HojaHuffman]): List[HojaHuffman] = list match
        case Nil => List(hojita)
        case head :: tail =>
          if hojita.peso <= head.peso then hojita :: list
          else head :: ordenar(hojita, tail)
  
  
      def aux(frecuencia: List[(Char, Int)], listHojas: List[HojaHuffman]): List[HojaHuffman] = frecuencia match
        case Nil => listHojas.sortBy(_.peso)
        case (c, f) :: tail =>
          aux(tail, ordenar(HojaHuffman(c, f), listHojas))
  
      aux(frec, Nil)
  
  
  def creaRamaHuff(izq: ArbolHuffman, dch: ArbolHuffman): RamaHuffman =
    RamaHuffman(izq, dch)
  
  def combinar(nodos: List[ArbolHuffman]): List[ArbolHuffman] =
    (creaRamaHuff(nodos.head,nodos.tail.head) :: nodos.tail.tail).sortBy(_.peso)
  
  def esListaSingleton(lista: List[ArbolHuffman]): Boolean = lista.length == 1
  
  
  def repetirHasta(operacion: List[ArbolHuffman]=> List[ArbolHuffman], criterio: List[ArbolHuffman]=> Boolean)(lista : List[ArbolHuffman]):ArbolHuffman=
    if criterio(lista) then lista.head
    else repetirHasta(operacion, criterio)(operacion(lista))

  val frec = ListaCharsADistFrec(cadena.toList)
  
  val ordenFrec = DistribFrecAListaHojas(frec)
  repetirHasta(combinar,esListaSingleton)(ordenFrec)
  




object miPrograma extends ArbolHuffman, App {


  val n1 = HojaHuffman(' ', 2)
  val n2 = HojaHuffman('e', 2)
  val r1 = RamaHuffman(n2, n1)
  val n3 = HojaHuffman('o', 3)
  val r2 = RamaHuffman(n3, r1)
  val n4 = HojaHuffman('s', 4)
  val r3 = RamaHuffman(n4, r2)
  val pes= r3.peso
  val cad = r3.caracteres
  println(pes)
  println(cad)


  val mensaje = r3.decodificar(List(0,1,0,0,1,1,1,1,1,0,0,1,1,0,1,1,1,1,0,0))
  println(mensaje)

  val mensajeCod = r3.codificar(mensaje)
  println(mensajeCod)

  val prueba = "hola bbe"
  val frec = ListaCharsADistFrec(prueba.toList)

  println(frec)
  val ordenFrec = DistribFrecAListaHojas(ListaCharsADistFrec(prueba.toList))
  println(ordenFrec)
  val combinado = combinar(ordenFrec)
  println(combinado)
}

