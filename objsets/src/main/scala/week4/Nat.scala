package week4

abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat = new Succ(this)
  def + (that: Nat): Nat = if (that.isZero) this else successor + that.predecessor
  def - (that: Nat): Nat = if (that.isZero) this else predecessor - that.predecessor
}

object Zero extends Nat {
  override def isZero: Boolean = true

  override def predecessor: Nat = throw new NoSuchElementException

}

class Succ(val pred: Nat) extends Nat {
  override def isZero: Boolean = false

  override def predecessor: Nat = pred
}
