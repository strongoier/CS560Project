package definitionTree

import scala.meta._

class DefinitionTree(var jsonTree: ujson.Value){
    assert(jsonTree.obj("class") == ujson.Str("FunctionDef"),"Class DefinitionTree must be called on Python definition")
    var functionName: String = jsonTree.obj("name").str
    println(s"Function Name: $functionName")
    var variablesInScope: Set[Term.Name] = Set[Term.Name]()

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
            body= Term.Block(stats =o.obj("body").arr.map(item => translateExpr(item)).toList)
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
      case ujson.Str("LtE") => Term.Name("<=")
      case ujson.Str("Lt") => Term.Name("<")
      case ujson.Str("Eq") => Term.Name("==")
      case ujson.Str("NotEq") => Term.Name("!=")
      case ujson.Str("Gt") => Term.Name(">")
      case ujson.Str("GtE") => Term.Name(">=")
      case ujson.Str("And") => Term.Name("&&")
      case ujson.Str("Or") => Term.Name("||")
      case ujson.Str("Not") => Term.Name("!")
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
    case a: ujson.Arr => translateExpr(a.arr.last)
    case o: ujson.Obj => o.obj("class") match {
      case ujson.Str("FunctionDef") =>  throw new Exception("Nested function definitions are not allowed")
      case ujson.Str("Expr") => translateExpr(o.obj("value"))
      case ujson.Str("BinOp") =>
        val left = translateExpr(o.obj("left"))
        val op = translateOp(o.obj("op"))
        val right = translateExpr(o.obj("right"))
        Term.ApplyInfix(lhs = left, op = op, targs = Nil, args = List(right))
      case ujson.Str("BoolOp") =>
        val left = translateExpr(o.obj("values").arr(0))
        val op = translateOp(o.obj("op"))
        val right = translateExpr(o.obj("values").arr.last)
        Term.ApplyInfix(lhs = left, op = op, targs = Nil, args = List(right))
      case ujson.Str("UnaryOp") =>
        val op = translateOp(o.obj("op"))
        val operand = translateExpr(o.obj("operand"))
        Term.ApplyUnary(op=op,arg=operand)
      case ujson.Str("Num") =>
        translateNum(o.obj("n"))
      case ujson.Str("Compare") => 
        val left = translateExpr(o.obj("left"))
        val op = translateOp(o.obj("ops").arr.last)
        val right = (o.obj("comparators").arr.map(comp => translateExpr(comp))).toList 
        Term.ApplyInfix(lhs = left,op=op,targs=Nil,args=right)
      case ujson.Str("If") => 
        val cond = translateExpr(o.obj("test"))
        val thenp = Term.Block(stats = o.obj("body").arr.map(item => translateExpr(item)).toList)
        val orelse = if (!o.obj("orelse").arr.isEmpty) Term.Block(stats = o.obj("orelse").arr.map(item => translateExpr(item)).toList) else Lit.Unit()
        Term.If(cond=cond,thenp=thenp,elsep = orelse)
      case _ => 
        println("translateExpr")
        println(o)
        ???
    }
    case _ => 
      println("translateExpr")
      //println(o)
      ???
  }


//   def translateBodyItem(v: ujson.Value): Term = v match {
//     case a: ujson.Arr => translateBodyItem(a.arr.last)
//     case o: ujson.Obj => o.obj("class") match {
//       case ujson.Str("FunctionDef") =>  throw new Exception("Nested function definitions are not allowed")
//       case ujson.Str("Expr") => translateExpr(o.obj("value"))
//       case ujson.Str("Compare") => translateCompare(o)
//       //case ujson.Str("Assign") => translateAssignment()
//       //case ujson.Str("Call") => translateMethodCall(o)
//       case _ => 
//         println("translateBodyItemInner")
//         println(o)
//         ???
//     }
//     case _ => 
//       println("translateBodyItemOuter")
//       println(v)
//       ???
//   }

}