import scala.io.Source
import scala.meta._
import scala.sys.process._
import scala.util.{Failure, Success, Using}
import definitionTree.DefinitionTree

object Main extends App {
  def translateSource(v: ujson.Value): List[Stat] = v match{
    case a: ujson.Arr => translateSource(a.arr.last)
    case o: ujson.Obj => o.obj("class") match {
      case ujson.Str("Module") => o.obj("body").arr.filter((item: ujson.Value) => item.obj("class") == ujson.Str("FunctionDef")).map(bodyItem => new DefinitionTree(bodyItem).translateFunction).toList
      case _ => 
        println("Python script not supported")
        ???
    }
    case _ => 
      println("Python AST parse error")
      ???
  }

  val _ = Seq("python", "ast_to_json.py", "-f", "test/binary_search.py", "test/binary_search.json").!!
  val result = Using(Source.fromFile("test/binary_search.json"))(_.mkString) match {
    case Success(jsonString) => translateSource(ujson.read(jsonString)).toString
    case Failure(_) => "failed to open json file"
  }
  
  println(result)
}
