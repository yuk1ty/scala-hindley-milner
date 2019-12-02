package core

trait Term

object Term {

  case class Var(x: String) extends Term {
    override def toString: String = x
  }

  case class Lambda(x: String, e: Term) extends Term {
    override def toString: String = "(\\" + x + "." + e + ")"
  }

  case class Apply(f: Term, e: Term) extends Term {
    override def toString: String = "(" + f + " " + e + ")"
  }

  case class Let(x: String, e: Term, f: Term) extends Term {
    override def toString: String = "let " + x + " = " + e + " in " + f
  }
}
