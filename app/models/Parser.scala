package models

//import scala.util.{Try, Success, Failure}

object Parser {
  
  def parse(i: Int, cmd: String) = {
    println(" ORIGIN = "+cmd)
    
    i match {
      case 1 => parseCmd(prepareOrigin(cmd)) 
      case 2 => parseListCmd(prepareOrigin(cmd))
      case 3 => parseMathCmd(prepareOrigin(cmd))
      case _ => throw new IllegalStateException("Wrong command index") 
    }
  }
  
  def prepareOrigin(cmd: String) = {
    cmd.split(" ") map ( _.trim() )
  }
  
  def parseCmd(origin: Array[String]) = {
    val t = Transaction(Data.accs(0),0,Data.providers(0))
    // decompose parsing functions
    val t1 = parseAccount(t, origin(0))
    val t2 = parseValue(t1, origin(1))
    parseFields(t2, origin(2),origin.slice(3, origin.size))
  }
  
  def parseMathCmd(origin: Array[String]) = {
    val o = origin.slice(1,origin.size)
    origin(0) match {
      case "mean" => Presenter.meanOf(parseListCmd(origin.slice(1,origin.size)))
      case "min" => Presenter.minOf(parseListCmd(origin.slice(1,origin.size)))
      case "max" => Presenter.maxOf(parseListCmd(origin.slice(1,origin.size)))
      case "sum" => Presenter.sumOf(parseListCmd(origin.slice(1,origin.size)))
      case _ => 0
    }
  }
  
  def parseListCmd(origin: Array[String]) = {
    println(" CMD "+origin(0))
    val l: List[Qurriable] = Command.withName(origin(0)) match {
      case Command.ACC => Data.accs
      case Command.HIST => Data.history
      case Command.PRVD => Data.providers
      case _ => List()
    }
    val querrys = origin.slice(1,origin.size) map (parseQuerry(_))
    
    Presenter.querrysOr(l,querrys.toList)
  }
  
  def parseAccount(t: Transaction, str: String) = {
    Data.accs find(x => (x.id == str)) match {
      case Some(acc) => t.copy(accountSource = acc)
      case None => throw new IllegalStateException("Wrong account") 
    }  
  }
  
  def parseValue(t: Transaction, str: String) = {
    t.copy(valueSource = (str.toInt))
    /*
    Try(str.toInt).toOption match {
       case Some(v) => t.copy(valueSource = v)
       case None => throw new IllegalStateException("Wrong value")
    }
    */
  }
  
  def parseQuerry(str: String) = {
    parseQuerryType(str) match {
      case QuerryType.GretherThan => Querry(prepareQuerryString(str),QuerryType.GretherThan)
      case QuerryType.LessThan => Querry(prepareQuerryString(str),QuerryType.LessThan)
      case QuerryType.Equals => Querry(prepareQuerryString(str),QuerryType.Equals)
      case _ => Querry(prepareQuerryString(str),QuerryType.SubString)
    }
  }
  
  def parseQuerryType(str: String) = {
    str.take(1) match {
      case ">" => QuerryType.GretherThan
      case "<" => QuerryType.LessThan
      case "=" => QuerryType.Equals
      case _ => QuerryType.SubString
    }
  }
  
  def prepareQuerryString(str: String) = {
    str.replaceAll("<","").replaceAll(">","").replaceAll("=","")
  }
  
  def parseFields(t: Transaction, str: String, opt: Array[String]) = {
    Data.providers find(x => (x.id == str)) match {
      case Some(pr) => {
        //opt map ( x => Field(pr.options(0)._1, x) )
        val o = for ( i <- 0 to (opt.size-1) ) yield {
          Field(pr.options(i).name, opt(i))
        }
        t.copy(destination = pr.copy(options = o.toList ))
      }
      case None => throw new IllegalStateException("Wrong account") 
    }
  }
}