import scala.io.Source
import scala.meta._
import scala.sys.process._
import scala.util.{Failure, Success, Using}

object Main extends App {

  def translateArguments(v: ujson.Value): List[List[Term.Param]] = v match {
    case o: ujson.Obj => o.obj("class") match {
      case ujson.Str("arguments") => {
        val args = o.obj("args").arr
        return List[List[Term.Param]](args.map(arg => translateArg(arg)).toList)
      }
      case _ => 
        println("translateArguments")
        ???
    }
    case _ => 
      println("translateArguments")
      ???
  }

  def translateArg(v: ujson.Value): Term.Param = v match {
    case o: ujson.Obj => o.obj("class") match {
      case ujson.Str("arg") => Term.Param(name= Term.Name(o.obj("arg").toString()),
                                          decltpe= Some(translateType(o.obj("annotation"))),
                                          mods = Nil,
                                          default=None,
                                          )
      case _ => 
        println("translateArg")
        ???
    }
    case _ => 
      println("translateArg")
      ???
  }

  def translateType(v: ujson.Value): Type = v match {
    case o: ujson.Obj => o.obj("class") match {
      case ujson.Str("Subscript") => Type.Apply(tpe= translateType(o.obj("value")),args= translateTypeArgs(o.obj("slice")))
      case ujson.Str("Name") => Type.Name(o.obj("id").toString())
      case _ => 
        println("translateType")
        ???
    }
    case _ => 
      println("translateType")
      ???
  } 
  
  def translateTypeArgs(v: ujson.Value): List[Type] = v match {
    case o: ujson.Obj => o.obj("value").obj("class") match {
      case ujson.Str("Name") => List[Type](translateType(o.obj("value")))
      case _ => o.obj("value").obj("elts").arr.map(i => translateType(i)).toList
    }
    case _ => 
      println("translateTypeArgs")
      ???
  }

  def translateOp(v: ujson.Value): Term.Name = v match {
    case o: ujson.Obj => o.obj("class") match {
      case ujson.Str("Add") => Term.Name("+")
      case ujson.Str("Sub") => Term.Name("-")
      case ujson.Str("Mult") => Term.Name("*")
      case ujson.Str("FloorDiv") => Term.Name("/")
      case ujson.Str("Mod") => Term.Name("%")
      case _ => 
        println("translateOp")
        ???
    }
    case _ => 
      println("translateOp")
      ???
  }

  def translateNum(v: ujson.Value): Term = v match {
    case ujson.Num(num) => Lit.Int(value = num.toInt)
    case _ => 
      println("translateNum")
      ???
  }

  def translateExpr(v: ujson.Value): Term = v match {
    case o: ujson.Obj => o.obj("class") match {
      case ujson.Str("BinOp") =>
        val left = translateExpr(o.obj("left"))
        val op = translateOp(o.obj("op"))
        val right = translateExpr(o.obj("right"))
        Term.ApplyInfix(lhs = left, op = op, targs = Nil, args = List(right))
      case ujson.Str("Num") =>
        translateNum(o.obj("n"))
      case _ => 
        println("translateExpr")
        ???
    }
    case _ => 
      println("translateExpr")
      ???
  }

  def translateTerm(v: ujson.Value): Term = v match {
    case a: ujson.Arr => translateTerm(a.arr.last) // incorrect
    case o: ujson.Obj => o.obj("class") match {
      case ujson.Str("Module") => translateTerm(o.obj("body"))
      case ujson.Str("Expr") => translateExpr(o.obj("value"))
      case _ => 
        println("translateTerm")
        ???
    }
    case _ => 
      println("translateTerm")
      ???
  }

  def translate(v: ujson.Value): Defn.Def = v match {
    case a: ujson.Arr => translate(a.arr.last)
    case o: ujson.Obj => o.obj("class") match {
      case ujson.Str("Module") => translate(o.obj("body"))
      case ujson.Str("FunctionDef") => Defn.Def(
        decltpe = Some(translateType(o.obj("returns"))),
        mods=Nil,
        tparams = Nil,
        name= Term.Name(o.obj("name").toString()),
        paramss= translateArguments(o.obj("args")),
        body= translateTerm(o.obj("body"))
      )
      case _ => 
        println("translate inner")
        println(o.obj("class"))
        ???
    }
    case _ => 
      println("translate outer")
      ???
  }

  val _ = Seq("python", "ast_to_json.py", "-f", "test/2.py", "test/2.json").!!
  val result = Using(Source.fromFile("test/2.json"))(_.mkString) match {
    case Success(jsonString) => translate(ujson.read(jsonString)).toString
    case Failure(_) => "failed to open json file"
  }
  println(result)
  //println("def binary_search(a: List[Int], low: Int, high: Int, value: Int): Int = {}".parse[Stat].get.structure)
  /*
  Defn.Def(Nil, Term.Name("binary_search"), Nil, List(List(Term.Param(Nil, Term.Name("a"), 
    Some(Type.Apply(Type.Name("List"), List(Type.Name("Int")))), None), 
      Term.Param(Nil, Term.Name("low"), Some(Type.Name("Int")), None), 
        Term.Param(Nil, Term.Name("high"), Some(Type.Name("Int")), None), 
          Term.Param(Nil, Term.Name("value"), Some(Type.Name("Int")), None))), 
            Some(Type.Name("Int")), Term.Block(Nil))
  */
}
