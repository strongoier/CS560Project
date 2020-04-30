import scala.meta._
import scala.collection.mutable
import java.io.{File => ioFile} 
import java.io.PrintWriter
import scala.io.{Source => ioSource}
import scala.sys.process._
import scala.util._

object Main extends App {

  //https://stackoverflow.com/questions/2315912/best-way-to-parse-command-line-parameters
    val usage = """
    Usage: sbt "run [--input-file | -i]   inputFile  [--output-file | -o outputFile]"
    """
    val arglist = args.toList
    type OptionMap = Map[Symbol, Any]

    def nextOption(map : OptionMap, list: List[String]) : OptionMap = {
      def isSwitch(s : String) = (s(0) == '-')
      list match {
        case Nil => map
        case "--input-file" :: value :: tail =>
                               nextOption(map ++ Map('inputFile -> value), tail)
        case "-i" :: value :: tail =>
                               nextOption(map ++ Map('inputFile -> value), tail)
        case "--output-file" :: value :: tail =>
                               nextOption(map ++ Map('outputFile -> value), tail)
        case "-o" :: value :: tail =>
                               nextOption(map ++ Map('outputFile -> value), tail)
        case string :: Nil =>  nextOption(map ++ Map('inputFile -> string), list.tail)
        case option :: tail => println("Unknown option "+option) 
                               sys.exit(1) 
      }
    }
    val options = nextOption(Map(),arglist)
    if (options.contains('inputFile)){
      val inFile: String = options('inputFile).asInstanceOf[String]
      assert(inFile.endsWith(".py"), "Input file must be a Python file")
      val outFile: String = if (options.contains('outputFile)) options('outputFile).asInstanceOf[String] else inFile.replace(".py",".scala").asInstanceOf[String]
      val _ = Seq("python", "ast_to_json.py", "-f", inFile).!!
      Using(ioSource.fromFile(inFile.replace(".py",".json")))(_.mkString) match {
        case Success(jsonString) =>
          val code = Main.translateSource(ujson.read(jsonString))(mutable.Map[String,Option[Type]]()).toString
          new ioFile(inFile.replace(".py",".json")).delete()
          val file_Object = new ioFile(outFile)
          val print_Writer = new PrintWriter(file_Object)
          print_Writer.write(code) 
          print_Writer.close()
          
        case Failure(_) => throw new Exception("failed to open json file")
      }
    }else{
      println(usage)
      println("Must provide Python input file")
      sys.exit(1)
    }
    

  def translateSource(v: ujson.Value)(implicit nameSet: mutable.Map[String,Option[Type]]): Source = v match {
    case a: ujson.Arr => translateSource(a.arr.last)
    case o: ujson.Obj => o.obj("class") match {
      case ujson.Str("Module") => Source(stats = o.obj("body").arr.map(item => translateStat(item)(nameSet = nameSet)).toList)
      case _ => throw new Exception("Fail: Source")
    }
    case _ => throw new Exception("Fail: Source")
  }

  def translateStat(v: ujson.Value)(implicit nameSet: mutable.Map[String,Option[Type]]): Stat = v match {
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
          nameSet(name.toString) = getValueType(o.obj("value").obj)
          Term.Assign(lhs = name, rhs = value)
        } else {
          val decltpe = getValueType(o.obj("value").obj)
          nameSet(name.toString) = decltpe
          Defn.Var(mods = Nil, pats = List(Pat.Var(name = name)), decltpe = None, rhs = Some(value))
        }
      case ujson.Str("For") =>
        val pat = Pat.Var(name = translateExpr(o.obj("target")).asInstanceOf[Term.Name])
        val enums = List(Enumerator.Generator(pat = pat, rhs = translateExpr(o.obj("iter"))))
        val body = Term.Block(stats = o.obj("body").arr.map(translateStat).toList)
        Term.For(enums = enums, body = body)
      case ujson.Str("FunctionDef") =>
        val decltpe = Some(translateType(o.obj("returns")))
        val name = o.obj("name").str
        val freshNameSet = mutable.Map[String,Option[Type]]()
        freshNameSet(name) = decltpe
        Defn.Def(decltpe = decltpe, mods = Nil, tparams = Nil,
          name = Term.Name(name),
          paramss = translateArgs(o.obj("args"))(nameSet = freshNameSet),
          body = Term.Block(stats = o.obj("body").arr.map(item => translateStat(item)(nameSet=freshNameSet)).toList)
        )
      case _ => throw new Exception("Fail: Stat")
    }
    case _ => throw new Exception("Fail: Stat")
  }

  def translateExpr(v: ujson.Value)(implicit nameSet: mutable.Map[String,Option[Type]]): Term = v match {
    case o: ujson.Obj => o.obj("class") match {
      case ujson.Str("BinOp") => 
        val left = translateExpr(o.obj("left"))
        val right = translateExpr(o.obj("right"))
        val op = o.obj("op").obj("class") match{
          case ujson.Str("Add") =>
            val concatCases = List("List","ListComp")
            if (concatCases.contains(o.obj("left").obj("class").str) || concatCases.contains(o.obj("right").obj("class").str)){
              Term.Name("++")
            }else if (o.obj("left").obj("class").str == "Call"){
              val funcName = translateExpr(o.obj("left").obj("func")).asInstanceOf[Term.Name].value
              if (getType(nameSet.getOrElse(funcName,None)) == "List"){
                Term.Name("++")
              }else{
                translateOp(o.obj("op"))
              }
            }else if (o.obj("right").obj("class").str == "Call"){
              val funcName = translateExpr(o.obj("right").obj("func")).asInstanceOf[Term.Name].value
              if (getType(nameSet.getOrElse(funcName,None)) == "List"){
                Term.Name("++")
              }else{
                translateOp(o.obj("op"))
              }
            }else if (o.obj("left").obj("class").str == "Name"){
              val funcName = translateExpr(o.obj("left")).asInstanceOf[Term.Name].value
              if (getType(nameSet.getOrElse(funcName,None)) == "List"){
                Term.Name("++")
              }else{
                translateOp(o.obj("op"))
              }
            }else if (o.obj("right").obj("class").str == "Name"){
              val funcName = translateExpr(o.obj("right")).asInstanceOf[Term.Name].value
              if (getType(nameSet.getOrElse(funcName,None)) == "List"){
                Term.Name("++")
              }else{
                translateOp(o.obj("op"))
              }
            }
            else{
              translateOp(o.obj("op"))
            }
          case _ => 
            translateOp(o.obj("op"))
        }
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
        }

      case _ => throw new Exception("Fail: Expr")
    }
    case _ => throw new Exception("Fail: Expr")
  }

  def getValueType(o: ujson.Obj)(implicit nameSet: mutable.Map[String,Option[Type]]): Option[Type] = o.obj("class") match{
    case ujson.Str("List") => 
      Some(Type.Apply(tpe = Type.Name("List"), args = o.obj("elts").arr.map(item => getValueType(item.asInstanceOf[ujson.Obj]).getOrElse(Type.Name("Int"))).toList.distinct))
    case ujson.Str("Num") => Some(Type.Name("Int"))
    case ujson.Str("ListComp") => 
      Some(Type.Name("List"))
    case ujson.Str("Name") => 
      val name = translateExpr(o).asInstanceOf[Term.Name].value 
      if (nameSet.contains(name)) nameSet(name) else None
    case ujson.Str("Call") =>
      val name = translateExpr(o.obj("func")).asInstanceOf[Term.Name].value
      if (nameSet.contains(name)) nameSet(name) else None
    case ujson.Str("BinOp") =>
      getValueType(o.obj("left").obj)
    case ujson.Str("UnaryOp") => Some(Type.Name("Int"))
    case _ => None
  }

  def filterCompare(o: ujson.Obj)(implicit nameSet: mutable.Map[String,Option[Type]]): Term = {
    val left = o.obj("left").obj("class") match {
      case ujson.Str("Subscript") => Term.Apply(fun = translateExpr(o.obj("left").obj("value")), args = List(Term.Placeholder()))
      case _ => getVariableOrPlaceholder(o.obj("left"))(nameSet = nameSet)
    }
    val op = translateOp(o.obj("ops").arr.last)
    val comp = o.obj("comparators").arr.last
    val right = comp.obj("class") match {
        case ujson.Str("Subscript") => Term.Apply(fun = translateExpr(comp.obj("value")), args = List(Term.Placeholder()))
        case _ => getVariableOrPlaceholder(comp)(nameSet = nameSet)
      }
    Term.ApplyInfix(lhs = left, op = op, targs = Nil, args = List(right))
  }

  def getVariableOrPlaceholder(v: ujson.Value)(implicit nameSet: mutable.Map[String,Option[Type]]): Term = v match{
    case o: ujson.Obj => o.obj("class") match {
      case ujson.Str("Name") => 
        if (nameSet.contains(o.obj("id").str)) Term.Name(o.obj("id").str) else Term.Placeholder()
      case ujson.Str("Num") => translateExpr(o)
      case _ => Term.Placeholder()
    }
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

  def translateArgs(v: ujson.Value)(implicit nameSet: mutable.Map[String,Option[Type]]): List[List[Term.Param]] = v match {
    case o: ujson.Obj => o.obj("class") match {
      case ujson.Str("arguments") =>
        val args = o.obj("args").arr
        List[List[Term.Param]](args.map(item => translateArg(item)(nameSet=nameSet)).toList)
      case _ => throw new Exception("Fail: Args")
    }
    case _ => throw new Exception("Fail: Args")
  }

  def translateArg(v: ujson.Value)(implicit nameSet: mutable.Map[String,Option[Type]]): Term.Param = v match {
    case o: ujson.Obj => o.obj("class") match {
      case ujson.Str("arg") =>
        val name = o.obj("arg").str
        val decltpe = Some(translateType(o.obj("annotation")))
        nameSet(name) = decltpe
        Term.Param(name = Term.Name(name),
        decltpe = decltpe,
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

  def getType(v: Option[Type]): String = v match {
    case None => ""
    case Some(o) => o match {
      case n: Type.Name => n.value
      case a: Type.Apply => getType(Some(a.tpe))
      case _ => throw new Exception("Fail: Unsupported Type conversion")
    }
  }
}