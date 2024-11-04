class CREao {
  def cuantasVeces(listaChar: List[Char], char: Char): (Char, Int) =
    def aux(lista: List[Char], chart: Char, n: Int): Int = lista match
      case Nil => n
      case h :: t =>
        if h == chart then aux(lista.tail, chart, n + 1)
        else aux(lista.tail, chart, n)

    (char, aux(listaChar, char, 0))


  def ListaCharsADistFrec(listaChar: List[Char]): List[(Char, Int)] =
    def aux(listChar: List[Char], lisTuplas: List[(Char, Int)]): List[(Char, Int)] = listChar match
      case Nil => lisTuplas
      case h :: t =>
        if lisTuplas.exists(_._1 == h) then aux(t, lisTuplas)
        else aux(t, lisTuplas :+ cuantasVeces(listChar, h))

    aux(listaChar, lisTuplas = Nil)



}

