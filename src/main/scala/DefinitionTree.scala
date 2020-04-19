package definitionTree

import scala.meta._

class DefinitionTree(var jsonTree: ujson.Value){
    println(jsonTree)
    assert(jsonTree.obj("class") == ujson.Str("FunctionDef"),"Class DefinitionTree must be called on Python definition")
    var functionName: String = jsonTree.obj("name").str
    println(functionName)
    var variablesInScope: Set[Stat] = Set[Stat]()

    def translateFunction: Defn.Def = jsonTree match {
        case a: ujson.Arr => 
            this.jsonTree = a.arr.last 
            return this.translateFunction
        case o: ujson.Obj => o.obj("class") match {
        case ujson.Str("FunctionDef") => Defn.Def(
            decltpe = Some(translateType(o.obj("returns"))),
            mods=Nil,
            tparams = Nil,
            name= Term.Name(o.obj("name").str),
            paramss= translateArguments(o.obj("args")),
            body= translateBodyItem(o.obj("body"))
        )
        case _ => 
            println("The Python file must only contain functions")
            println(o.obj("class"))
            ???
        }
        case _ => 
        println("translateFunction outer")
        ???
    }

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
      case ujson.Str("arg") => Term.Param(name= Term.Name(o.obj("arg").str),
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
      case ujson.Str("Name") => Type.Name(o.obj("id").str)
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

//   def parseAssignLHS(targets: ujson.Value): Term = {}
//   def parseAssignRHS(value: ujson.Value): Term = {}

//   def translateAssign(v: ujson.Value): Term = v match {
//     case o: ujson.Obj => o.obj("class") match {
//       case ujson.Str("Assign") => {
//         val targets = o.obj("targets")
//         val value = o.obj("value")
//         if (targets.obj("elts").isNull){
//           // left-hand side has only 1 variable
//           if (value.obj("elts").isNull){
//             // right-hand side has only 1 variable -- normal assignment
//             val lhs = Pat.var()
//             return Defn.Var(mods = Nil,decltpe=None,)
//           }
//           else{
//             // right-hand side has multiple variables -- assigning a Tuple
//           }
//         }else{
//           // left-hand side has multiple varible
//           if (value.obj("elts").isNull){
//             // right-hand side has only 1 variable -- invalid syntax
//             throw new Exception("Invalid Python assignment -- cannot unpack non-iterable object")
//           }
//           else{
//             //must have same number of variable -- treat each as different assignment
//           }
//         }
//       }
//       case _ => ???
//     }
//     case _ => ???
//   }

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

  // def translateTerm(v: ujson.Value): Term = v match {
  //   case a: ujson.Arr => translateTerm(a.arr.last) // incorrect
  //   case o: ujson.Obj => o.obj("class") match {
  //     case ujson.Str("Module") => translateTerm(o.obj("body"))
  //     case ujson.Str("Expr") => translateExpr(o.obj("value"))
  //     case _ => 
  //       println("translateTerm")
  //       ???
  //   }
  //   case _ => 
  //     println("translateTerm")
  //     ???
  // }

//   def translateMethodCall(v: ujson.Value): Stat = v match {
//     case o: ujson.Obj => o.obj("class") match {
//       case ujson.Str("Call") => 
//     }
//   }

  def translateBodyItem(v: ujson.Value): Term = v match {
    case a: ujson.Arr => translateBodyItem(a.arr.last)
    case o: ujson.Obj => o.obj("class") match {
      case ujson.Str("FunctionDef") =>  throw new Exception("Nested function definitions are not allowed")
      case ujson.Str("Expr") => translateExpr(o.obj("value"))
      //case ujson.Str("Assign") => translateAssignment()
      //case ujson.Str("Call") => translateMethodCall(o)
      case _ => 
        println("translateBodyItemInner")
        println(o)
        ???
    }
    case _ => 
      println("translateBodyItemOuter")
      println(v)
      ???
  }

}