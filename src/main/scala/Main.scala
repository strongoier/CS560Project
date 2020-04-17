import scala.io.Source
import scala.meta._
import scala.sys.process._
import scala.util.{Failure, Success, Using}

object Main extends App {
  def translateOp(v: ujson.Value): Term.Name = v match {
    case o: ujson.Obj => o.obj("class") match {
      case ujson.Str("Add") => Term.Name("+")
      case ujson.Str("Sub") => Term.Name("-")
      case ujson.Str("Mult") => Term.Name("*")
      case ujson.Str("FloorDiv") => Term.Name("/")
      case ujson.Str("Mod") => Term.Name("%")
      case _ => ???
    }
    case _ => ???
  }

  def translateNum(v: ujson.Value): Term = v match {
    case ujson.Num(num) => Lit.Int(value = num.toInt)
    case _ => ???
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
      case _ => ???
    }
    case _ => ???
  }

  def translate(v: ujson.Value): Term = v match {
    case a: ujson.Arr => translate(a.arr.last) // incorrect
    case o: ujson.Obj => o.obj("class") match {
      case ujson.Str("Module") => translate(o.obj("body"))
      case ujson.Str("Expr") => translateExpr(o.obj("value"))
      case _ => ???
    }
    case _ => ???
  }

  val _ = Seq("python3", "ast_to_json.py", "-f", "test/1.py", "test/1.json").!!
  val result = Using(Source.fromFile("test/1.json"))(_.mkString) match {
    case Success(jsonString) => translate(ujson.read(jsonString)).toString
    case Failure(_) => "failed to open json file"
  }
  println(result)
}
