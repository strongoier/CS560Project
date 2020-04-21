import scala.meta._
import scala.collection.mutable

object Main extends App {

  def translateSource(v: ujson.Value)(implicit nameSet: mutable.Set[String]): Source = v match {
    case a: ujson.Arr => translateSource(a.arr.last)
    case o: ujson.Obj => o.obj("class") match {
      case ujson.Str("Module") => Source(stats = o.obj("body").arr.map(translateStat).toList)
      case _ => throw new Exception("Fail: Source")
    }
    case _ => throw new Exception("Fail: Source")
  }

  def translateStat(v: ujson.Value)(implicit nameSet: mutable.Set[String]): Stat = v match {
    case o: ujson.Obj => o.obj("class") match {
      case ujson.Str("Expr") => translateExpr(o.obj("value"))
      case ujson.Str("If") =>
        val cond = translateExpr(o.obj("test"))
        val thenp = Term.Block(stats = o.obj("body").arr.map(translateStat).toList)
        val elsep = if (o.obj("orelse").arr.nonEmpty) {
          Term.Block(stats = o.obj("orelse").arr.map(translateStat).toList)
        } else {
          Lit.Unit()
        }
        Term.If(cond = cond, thenp = thenp, elsep = elsep)
      case ujson.Str("Return") =>
        translateExpr(o.obj("value"))
      case ujson.Str("Assign") =>
        val name = translateExpr(o.obj("targets").arr.head).asInstanceOf[Term.Name]
        val value = translateExpr(o.obj("value"))
        if (nameSet.contains(name.toString)) {
          Term.Assign(lhs = name, rhs = value)
        } else {
          nameSet += name.toString
          Defn.Var(mods = Nil, pats = List(Pat.Var(name = name)), decltpe = None, rhs = Some(value))
        }
      case ujson.Str("For") =>
        val pat = Pat.Var(name = translateExpr(o.obj("target")).asInstanceOf[Term.Name])
        val enums = List(Enumerator.Generator(pat = pat, rhs = translateExpr(o.obj("iter"))))
        val body = Term.Block(stats = o.obj("body").arr.map(translateStat).toList)
        Term.For(enums = enums, body = body)
      case ujson.Str("FunctionDef") =>
        Defn.Def(decltpe = Some(translateType(o.obj("returns"))), mods = Nil, tparams = Nil,
          name = Term.Name(o.obj("name").str),
          paramss = translateArgs(o.obj("args")),
          body = Term.Block(stats = o.obj("body").arr.map(translateStat).toList)
        )
      case _ => throw new Exception("Fail: Stat")
    }
    case _ => throw new Exception("Fail: Stat")
  }

  def translateExpr(v: ujson.Value): Term = v match {
    case o: ujson.Obj => o.obj("class") match {
      case ujson.Str("BinOp") =>
        val left = translateExpr(o.obj("left"))
        val op = translateOp(o.obj("op"))
        val right = translateExpr(o.obj("right"))
        Term.ApplyInfix(lhs = left, op = op, targs = Nil, args = List(right))
      case ujson.Str("BoolOp") =>
        val left = translateExpr(o.obj("values").arr.head)
        val op = translateOp(o.obj("op"))
        val right = translateExpr(o.obj("values").arr.last)
        Term.ApplyInfix(lhs = left, op = op, targs = Nil, args = List(right))
      case ujson.Str("UnaryOp") =>
        val op = translateOp(o.obj("op"))
        val operand = translateExpr(o.obj("operand"))
        Term.ApplyUnary(op = op, arg = operand)
      case ujson.Str("Num") => o.obj("n") match {
        case ujson.Num(num) => Lit.Int(value = num.toInt)
        case _ => throw new Exception("Fail: num")
      }
      case ujson.Str("Compare") =>
        val left = translateExpr(o.obj("left"))
        val op = translateOp(o.obj("ops").arr.last)
        val right = translateExpr(o.obj("comparators").arr.last)
        Term.ApplyInfix(lhs = left, op = op, targs = Nil, args = List(right))        
      case ujson.Str("Name") => if (o.obj("id").str == "_") Term.Placeholder() else Term.Name(o.obj("id").str)
      case ujson.Str("Call") => o.obj("func").obj("id") match {
        case ujson.Str("len") =>
          // method call
          val qual = translateExpr(o.obj("args").arr.last)
          Term.Select(qual = qual, name = Term.Name("length"))
        case ujson.Str("sum") =>
          val qual = translateExpr(o.obj("args").arr.last)
          Term.Select(qual = qual, name = Term.Name("sum"))
        case _ =>
          // If not a covered scala method, will fall back on function call
          val args = o.obj("args").arr.map(translateExpr).toList
          Term.Apply(fun = Term.Name(o.obj("func").obj("id").str), args = args)
      }
      case ujson.Str("List") =>
        Term.Apply(fun = Term.Name("List"), args = o.obj("elts").arr.map(translateExpr).toList)
      case ujson.Str("Index") => translateExpr(o.obj("value"))
      case ujson.Str("Subscript") =>
        val args = List(translateExpr(o.obj("slice")))
        val fun = translateExpr(o.obj("value")).asInstanceOf[Term.Name]
        Term.Apply(fun = fun, args = args)
      case ujson.Str("ListComp") => o.obj("elt").obj("class") match {
        case ujson.Str("Call") =>
          //Map method
          val map = Term.Select(qual=translateExpr(o.obj("generators").arr.last),name=Term.Name("map"))
          val args=List(Term.Name(o.obj("elt").obj("func").obj("id").str))
          Term.Apply(fun=map,args=args)
        case ujson.Str("Subscript") =>
          //Map on index
          val map = Term.Select(qual=translateExpr(o.obj("generators").arr.last),name=Term.Name("map"))
          val args=List(translateExpr(o.obj("elt").obj("value")).asInstanceOf[Term.Name])
          Term.Apply(fun=map,args=args)
        case _ => 
          //Not map
          translateExpr(o.obj("generators").arr.last)
      }
      case ujson.Str("comprehension") =>
        if (o.obj("ifs").arr.isEmpty){
          // No filter
          translateExpr(o.obj("iter"))
        }else{
          //Filter
          val filter = Term.Select(qual=translateExpr(o.obj("iter")),name=Term.Name("filter"))
          val args = o.obj("ifs").arr.map(item => filterCompare(item.asInstanceOf[ujson.Obj])).toList
          concatFilterArguments(args,filter)
          //Term.Apply(fun=filter,args=List(andArguments(args)))
        }

      case _ => throw new Exception("Fail: Expr")
    }
    case _ => throw new Exception("Fail: Expr")
  }

  def filterCompare(o: ujson.Obj): Term = {
    val left = o.obj("left").obj("class") match {
      case ujson.Str("Subscript") => Term.Apply(fun = translateExpr(o.obj("left").obj("value")), args = List(Term.Placeholder()))
      case _ => Term.Placeholder()
    }
    val op = translateOp(o.obj("ops").arr.last)
    val right = translateExpr(o.obj("comparators").arr.last)
    Term.ApplyInfix(lhs = left, op = op, targs = Nil, args = List(right))
  }

  def concatFilterArguments(args: List[Term],filter: Term.Select): Term = {
      if (args.length == 1){
        Term.Apply(fun=filter,args=args)
      }else{
        Term.Apply(fun=Term.Select(qual=concatFilterArguments(args.dropRight(1),filter),name=Term.Name("filter")),args=List(args.last))
      }
      
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
      case _ => throw new Exception("Fail: Op")
    }
    case _ => throw new Exception("Fail: Op")
  }

  def translateArgs(v: ujson.Value): List[List[Term.Param]] = v match {
    case o: ujson.Obj => o.obj("class") match {
      case ujson.Str("arguments") =>
        val args = o.obj("args").arr
        List[List[Term.Param]](args.map(translateArg).toList)
      case _ => throw new Exception("Fail: Args")
    }
    case _ => throw new Exception("Fail: Args")
  }

  def translateArg(v: ujson.Value): Term.Param = v match {
    case o: ujson.Obj => o.obj("class") match {
      case ujson.Str("arg") => Term.Param(name = Term.Name(o.obj("arg").str),
        decltpe = Some(translateType(o.obj("annotation"))),
        mods = Nil, default = None)
      case _ => throw new Exception("Fail: Arg")
    }
    case _ => throw new Exception("Fail: Arg")
  }

  def translateType(v: ujson.Value): Type = v match {
    case o: ujson.Obj => o.obj("class") match {
      case ujson.Str("Subscript") =>
        Type.Apply(tpe = translateType(o.obj("value")), args = translateTypeArgs(o.obj("slice")))
      case ujson.Str("Name") => Type.Name(o.obj("id").str match {
        case "List" => "List"
        case "int" => "Int"
        case _ => throw new Exception("Fail: Type")
      })
      case _ => throw new Exception("Fail: Type")
    }
    case _ => throw new Exception("Fail: Type")
  }

  def translateTypeArgs(v: ujson.Value): List[Type] = v match {
    case o: ujson.Obj => o.obj("value").obj("class") match {
      case ujson.Str("Name") => List[Type](translateType(o.obj("value")))
      case ujson.Str("Tuple") => o.obj("value").obj("elts").arr.map(translateType).toList
      case _ => throw new Exception("Fail: TypeArgs")
    }
    case _ => throw new Exception("Fail: TypeArgs")
  }
}