import scala.quoted.*

// Must use inline here otherwise weird reference things happen

inline def functionAst[T](inline f: T): String =
  ${ functionAstImpl('f) }

inline def functionAst2[T](inline f: T): String =
  ${ functionAstImpl2('f) }

def functionAstImpl[T: Type](f: Expr[T])(using Quotes): Expr[String] =
  import quotes.reflect.*

  def extractFunctionBody(term: Term): String = term match {
    // Handle references to defined functions
    case Inlined(_, _, body) => extractFunctionBody(body)
    case Ident(name) =>
      val symbol = term.symbol
      if symbol.isDefDef then
        // symbol.tree.show(using Printer.TreeStructure)
        // symbol.tree.show(using Printer.TreeStructure)
        // Could not get this to extract anything useful :(
        // Leaving commented code here in case it may be useful
        symbol.tree.asInstanceOf[DefDef].pos.sourceFile.toString()
        // symbol.tree.asInstanceOf[DefDef].rhs match {
        //     case Some(body) => body.show(using Printer.TreeCode)
        //     case _ => throw new RuntimeException(s"Could not extract body from function $name")
        // }
        // symbol.tree match
        //   case DefDef(_, _, _, Some(body)) => body.show(using Printer.TreeShortCode)
        //   case _ => throw new RuntimeException(s"Could not extract body from function $name")
      else
        throw new RuntimeException(s"$name is not a function definition")
    
    // Handle lambda expressions, this gets hit if inline is set
    case Lambda(params, body) =>
      // This will have type info
      body.show(using Printer.TreeStructure)

    case _ =>
      throw new RuntimeException(s"Could not extract function body from $term")
  }


  val tree: Term = f.asTerm
  Expr(extractFunctionBody(tree))

def functionAstImpl2[T: Type](f: Expr[T])(using Quotes): Expr[String] =
  import quotes.reflect.*
  val tree: Term = f.asTerm
  Expr(tree.show(using Printer.TreeStructure))
