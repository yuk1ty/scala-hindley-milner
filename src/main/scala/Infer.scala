import core.Term.{Apply, Lambda, Var}
import inference.TypeInferenceMachine

object Infer extends App {
  try {
    println(TypeInferenceMachine
      .typeOf(predef.Predef.env,
              Lambda("x", Apply(Apply(Var("cons"), Var("x")), Var("nil"))))
      .toString)
  } catch {
	  case err: Exception => err.printStackTrace()
  }
}
