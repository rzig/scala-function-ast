import scala.quoted.*

// If we put this as the parameter to functionAst, we get
// output of the "partial application" AST (the one in which
// no paramters have been applied). I can't figure out how
// to get around this for the time being, since the current
// way of grabbing AST is to get the AST of whatever is in
// the paretheses in call to functionAst.
inline def someOtherMethod(x: Int): Int = {
  val y = x + 1
  x + y
}

@main def hello(): Unit =
  val msg = functionAst(someOtherMethod)
  val msg2 = functionAst2({
    val y = 1 + 2
    y + 3
  })
  println(msg)
  println(msg2)

def msg = "I was compiled by Scala 3. :)"
