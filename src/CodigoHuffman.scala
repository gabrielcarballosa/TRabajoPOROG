
//*trait CodigoHuffman



def cadenaAListaChars(cadena: String): List[Char] =
  cadena.toList
def listaCharsACadena(listaCar: List[Char]): String =
  listaCar.toString()



def DistribFrecAListaHojas(frec: List[(Char,Int)]) : List[HojaHuffman] =
  def ordenar(hojita: HojaHuffman, list: List[HojaHuffman]): List[HojaHuffman]= list match
    case Nil => List(hojita)
    case head::tail =>
      if hojita.peso <= head.peso then hojita :: list
      else head:: ordenar(hojita,tail)


  def aux(frecuencia:List[(Char,Int)], listHojas: List[HojaHuffman]): List[HojaHuffman]= frecuencia match
    case Nil => listHojas.sortBy(_.peso)
    case (c,f) :: tail =>
      aux(tail,ordenar(HojaHuffman(c,f) ,listHojas))
  aux(frec,Nil)











