package predef

import core.{Type, TypeScheme}
import core.Type.{Arrow, Tycon}
import inference.TypeInferenceMachine

object Predef {

  val booleanType: Type = Tycon("Boolean", List.empty)
  val intType: Type = Tycon("Int", List.empty)
  def listType(t: Type): Type = Tycon("List", List(t))

  private[this] def gen(t: Type): TypeScheme =
    TypeInferenceMachine.gen(Map.empty, t)

  private val a = TypeInferenceMachine.newTyvar()

  val env = Map(
    "true" -> gen(booleanType),
    "false" -> gen(booleanType),
    "if" -> gen(Arrow(booleanType, Arrow(a, Arrow(a, a)))),
    "zero" -> gen(intType),
    "succ" -> gen(Arrow(intType, intType)),
    "nil" -> gen(listType(a)),
    "cons" -> gen(Arrow(a, Arrow(listType(a), listType(a)))),
    "isEmpty" -> gen(Arrow(listType(a), listType(a))),
    "head" -> gen(Arrow(listType(a), a)),
    "tail" -> gen(Arrow(listType(a), listType(a))),
    "fix" -> gen(Arrow(Arrow(a, a), a))
  )
}
