package core

import core.Type.Tyvar
import inference.TypeInferenceMachine

case class TypeScheme(tyvars: List[Tyvar], tpe: Type) {
  def newInstance(): Type =
    tyvars.foldLeft(Subst.empty())((s, tv) =>
      s.extend(tv, TypeInferenceMachine.newTyvar()))(tpe)
}
