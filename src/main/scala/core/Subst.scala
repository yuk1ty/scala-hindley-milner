package core

import core.Type.{Arrow, Tycon, Tyvar}

trait Subst extends ((Type) => Type) { self =>

  def lookup(x: Tyvar): Type

  override def apply(t: Type): Type = t match {
    case tv @ Tyvar(a) =>
      val u = lookup(tv)
      if (t == u) t else this.apply(u)
    case Arrow(t1, t2) => Arrow(this.apply(t1), this.apply(t2))
    case Tycon(k, ts)  => Tycon(k, ts map this.apply)
  }

  def extend(x: Tyvar, t: Type): Subst =
    (y: Tyvar) => if (x == y) t else self.lookup(y)
}

object Subst {
  def empty(): Subst = (x: Tyvar) => x
}
