import org.scalatest.FunSuite

import scala.meta.Type
import scala.collection.mutable
import scala.io.Source
import scala.sys.process._
import scala.util._
import scala.reflect.runtime.currentMirror
import scala.tools.reflect.ToolBox

class MainTest extends FunSuite {
  val toolbox = currentMirror.mkToolBox()

  def testCodeAndOutput(id: String, expectedCode: String, expectedOutput: Any) = {
    val _ = Seq("python", "ast_to_json.py", "-f", s"test/$id.py").!!
    Using(Source.fromFile(s"test/$id.json"))(_.mkString) match {
      case Success(jsonString) =>
        val code = Main.translateSource(ujson.read(jsonString))(mutable.Map[String,Option[Type]]()).toString
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
  test("5"){
    testCodeAndOutput("5","""var a = List(10, 15)
                            |var c = List(20, -10)
                            |var b = List(0, 1)
                            |var tot = b.filter(_ > 0).map(a)
                            |var tot2 = b.filter(c(_) > 0).map(a)
                            |var tot3 = b.filter(_ > 0)
                            |var tot4 = b.map(a)
                            |var tot5 = b.filter(c(_) > 0).filter(_ > 0).map(a)
                            |(tot ++ tot2 ++ tot3 ++ tot4 ++ tot5).sum""".stripMargin,51)
  }
  test("6"){
    testCodeAndOutput("6","""def combine(a: List[Int], b: List[Int]): List[Int] = {
                            |  a ++ b
                            |}
                            |combine(List(1, 2, 3), List(4, 5, 6)) == List(1, 2, 3, 4, 5, 6)""".stripMargin,true)
  }
  test("binary_search"){
    testCodeAndOutput("binary_search","""def binary_search(a: List[Int], low: Int, high: Int, value: Int): Int = {
                                        |  if (low < high) {
                                        |    var mid = (low + high) / 2
                                        |    if (a(mid) < value) {
                                        |      binary_search(a, mid + 1, high, value)
                                        |    } else {
                                        |      if (value < a(mid)) {
                                        |        binary_search(a, low, mid, value)
                                        |      } else {
                                        |        mid
                                        |      }
                                        |    }
                                        |  } else {
                                        |    -1
                                        |  }
                                        |}
                                        |binary_search(List(5, 10, 20, 100, 200), 0, 4, 20)""".stripMargin
                                        ,2)
  }

  test("quicksort"){
    testCodeAndOutput("quicksort","""def quicksort(a: List[Int]): List[Int] = {
                                    |  if (a.length < 2) {
                                    |    a
                                    |  } else {
                                    |    var pivot = a(a.length / 2)
                                    |    quicksort(a.filter(pivot > (_))) ++ a.filter(pivot == (_)) ++ quicksort(a.filter(pivot < (_)))
                                    |  }
                                    |}
                                    |quicksort(List(10, 6, 8, 1, 0, 9)) == List(0, 1, 6, 8, 9, 10)""".stripMargin,true)
  }
}
