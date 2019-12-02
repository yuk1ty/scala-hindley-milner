package core

sealed trait Type

object Type {

  case class Tyvar(a: String) extends Type {
    override def toString: String = a
  }

  case class Arrow(t1: Type, t2: Type) extends Type {
    override def toString: String = "(" + t1 + "->" + t2 + ")"
  }

  case class Tycon(k: String, ts: List[Type]) extends Type {
    override def toString: String =
      k + (if (ts.isEmpty) "" else ts.mkString("[", ",", "]"))
  }
}
