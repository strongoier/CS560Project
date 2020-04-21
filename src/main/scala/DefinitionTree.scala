package definitionTree

import scala.meta._

class DefinitionTree(var jsonTree: ujson.Value){
    assert(jsonTree.obj("class") == ujson.Str("FunctionDef"),"Class DefinitionTree must be called on Python definition")
    val functionName: String = jsonTree.obj("name").str
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
      case ujson.Str("USub") => Term.Name("-")
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
      case ujson.Str("Return") => 
        translateExpr(o.obj("value"))
      case ujson.Str("Name") => if (o.obj("id").str == "_") Term.Placeholder() else Term.Name(o.obj("id").str)
      case ujson.Str("Call") =>
        o.obj("func").obj("id") match {
            case ujson.Str("len") =>
                // method call
                val qual = translateExpr(o.obj("args").arr.last)
                Term.Select(qual = qual, name=Term.Name("length"))
            case _ => 
                //If not a covered scala method, will fall back on function call
                val args = o.obj("args").arr.map(arg => translateExpr(arg)).toList
                Term.Apply(fun = Term.Name(o.obj("func").obj("id").str),args = args)
        }
      case ujson.Str("List") => 
        Term.Apply(fun=Term.Name("List"),args = o.obj("elts").arr.map(item => translateExpr(item)).toList)
      case ujson.Str("For") =>
        val pat = Pat.Var(name=translateExpr(o.obj("target")).asInstanceOf[Term.Name])
        val enums = List(Enumerator.Generator(pat=pat,rhs=translateExpr(o.obj("iter"))))
        val body = Term.Block(stats=o.obj("body").arr.map(item => translateExpr(item)).toList)
        Term.For(enums=enums,body=body)
      case ujson.Str("Index") => translateExpr(o.obj("value"))
      case ujson.Str("Subscript") => 
        val args = List(translateExpr(o.obj("slice")))
        val fun = translateExpr(o.obj("value")).asInstanceOf[Term.Name]
        Term.Apply(fun=fun,args=args)
      case ujson.Str("ListComp") => o.obj("elt").obj("class") match {       
        case ujson.Str("Name") => 
            //Not map
            translateExpr(o.obj("generators").arr.last)
        case ujson.Str("Call") => 
            //Map method
            val map = Term.Select(qual=translateExpr(o.obj("generators").arr.last),name=Term.Name("map"))
            val args=List(Term.Name(o.obj("elt").obj("func").obj("id").str))
            Term.Apply(fun=map,args=args)
        case _ => throw new Exception("This list comprehension is not supported. Try using a for loop")
        }
      case ujson.Str("comprehension") => 
        if (o.obj("ifs").arr.isEmpty){
            // No filter
            translateExpr(o.obj("iter"))
        }else{
            //Filter
            val filter = Term.Select(qual=translateExpr(o.obj("iter")),name=Term.Name("filter"))
            val args = o.obj("ifs").arr.map(translateExpr).toList
            Term.Apply(fun=filter,args=List(andArguments(args)))
        }

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

  def andArguments(args: List[Term]): Term = {
      if (args.length == 1){
        args(0)
      }else{
        val lhs = args(0)
        Term.ApplyInfix(lhs=lhs,op=Term.Name("&&"),args=List(andArguments(args.drop(1))),targs=Nil)
      }
      
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