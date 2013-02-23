package ayla.dataset

trait DsspProvider {
  def getDSSPLabels(numResidues: Int, id: Int): Array[Char]
}

class ServerDsspProvider(dsspArray: Array[Char]) extends DsspProvider {
  def getDSSPLabels(numResidues: Int, id: Int): Array[Char] = {
    val barLabels = Array.fill[Char](numResidues)('X')
    (0 until numResidues).foreach { i =>
      val ofs = id * numResidues + i
      barLabels(i) = dsspArray(ofs)
    }
    barLabels
  }
}