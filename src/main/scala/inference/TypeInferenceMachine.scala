package inference

import core.Term.{Apply, Lambda, Let, Var}
import core.{Subst, Term, Type, TypeScheme}
import core.Type.{Arrow, Tycon, Tyvar}

object TypeInferenceMachine {

  type Env = Map[String, TypeScheme]

  private var n: Int = 0

  private var current: Option[Term] = None

  def newTyvar(): Type = {
    n += 1
    Tyvar("a" + n)
  }

  private def lookup(env: Env, x: String): Option[TypeScheme] = env.get(x)

  def gen(env: Env, t: Type): TypeScheme =
    TypeScheme(tyvars(t) diff tyvars(env), t)

  private def tyvars(t: Type): List[Tyvar] = t match {
    case tv @ Tyvar(a) => List(tv)
    case Arrow(t1, t2) => tyvars(t1) ++ tyvars(t2)
    case Tycon(k, ts) =>
      ts.foldLeft(List[Tyvar]())((tvs, t) => tvs ++ tyvars(t))
  }

  private def tyvars(ts: TypeScheme): List[Tyvar] = tyvars(ts.tpe) diff ts.tyvars

  private def tyvars(env: Env): List[Tyvar] =
    env.values.foldLeft(List[Tyvar]())((tvs, nt) => tvs ++ tyvars(nt))

  private def unify(t: Type, u: Type, s: Subst): Subst = (s(t), s(u)) match {
    case (Tyvar(a), Tyvar(b)) if (a == b)         => s
    case (Tyvar(a), _) if !(tyvars(u) contains a) => s.extend(Tyvar(a), u)
    case (_, Tyvar(a))                            => unify(u, t, s)
    case (Arrow(t1, t2), Arrow(u1, u2))           => unify(t1, u1, unify(t2, u2, s))
    case (Tycon(k1, ts), Tycon(k2, us)) if (k1 == k2) =>
      (ts zip us).foldLeft(s)((s, tu) => unify(tu._1, tu._2, s))
    case _ => throw new Exception(s"cannot unify ${s(t)} with ${s(u)}")
  }

  private def tp(env: Env, e: Term, t: Type, s: Subst): Subst = {
    current = Some(e)
    e match {
      case Var(x) =>
        val u = lookup(env, x)
        if (u.isEmpty) throw new Exception("undefined: " + x)
        else unify(u.get.newInstance(), t, s)
      case Lambda(x, e1) =>
        val a, b = newTyvar()
        val s1 = unify(t, Arrow(a, b), s)
        val env1 = env ++ Map((x, TypeScheme(List.empty, a)))
        tp(env1, e1, b, s1)
      case Apply(e1, e2) =>
        val a = newTyvar()
        val s1 = tp(env, e1, Arrow(a, t), s)
        tp(env, e2, a, s1)
      case Let(x, e1, e2) =>
        val a = newTyvar()
        val s1 = tp(env, e1, a, s)
        tp(Map((x, gen(env, s1(a)))) ++ env, e2, t, s1)
    }
  }

  def typeOf(env: Env, e: Term): Type = {
    val a = newTyvar()
    tp(env, e, a, Subst.empty())(a)
  }
}
