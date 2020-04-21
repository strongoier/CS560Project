import org.scalatest.FunSuite

import scala.collection.mutable
import scala.io.Source
import scala.sys.process._
import scala.util._
import scala.reflect.runtime.currentMirror
import scala.tools.reflect.ToolBox

class MainTest extends FunSuite {
  val toolbox = currentMirror.mkToolBox()

  def testCodeAndOutput(id: String, expectedCode: String, expectedOutput: Any) = {
    val _ = Seq("python", "ast_to_json.py", "-f", s"test/$id.py", s"test/$id.json").!!
    Using(Source.fromFile(s"test/$id.json"))(_.mkString) match {
      case Success(jsonString) =>
        val code = Main.translateSource(ujson.read(jsonString))(mutable.Set[String]()).toString
        assert(code === expectedCode)
        assert(toolbox.compile(toolbox.parse(code))() === expectedOutput)
      case Failure(_) => fail("failed to open json file")
    }
  }

  test("1") {
    testCodeAndOutput("1", "1 * (2 + 3) / 4 - 5 % 6", -4)
  }

  test("2") {
    testCodeAndOutput("2", "!(4 != 5) || 0 > -1 && 2 <= 2", true)
  }

  test("3") {
    testCodeAndOutput("3", """var a = List(1, 2, 3)
                             |a = List(4, 5, 6)
                             |var b = a.length
                             |for (c <- a) {
                             |  b = b + c
                             |  b = b - 1
                             |}
                             |b""".stripMargin, 15)
  }

  test("4") {
    testCodeAndOutput("4", """def test(a: Int, b: List[Int]): Int = {
                             |  if (b(0) < a) {
                             |    1
                             |  } else {
                             |    if (b(0) == a) {
                             |      2
                             |    } else {
                             |      3
                             |    }
                             |  }
                             |}
                             |test(0, List(1, 2)) * 100 + test(1, List(1, 2)) * 10 + test(2, List(1, 2))""".stripMargin, 321)
  }
}
