package models

import scala.compat._

object QuerryType extends Enumeration {
    type QuerryType = Value
    val SubString, LessThan, GretherThan, Equals = Value
}

object Command extends Enumeration {
   val ACC  = Value("acc")
   val HIST = Value("hist")
   val PRVD = Value("providers")
}


object CmdType extends Enumeration {
    type CmdType = Value
    val MathStatement, ListStatement, OptionStatement, AccountStatement, ProviderStatement, MoneyStatement = Value
}

import CmdType._
import QuerryType._

trait Qurriable extends Mathable{
  def applyQuerry(q: Querry): Boolean
  
  def toInt(s: String): Option[Int] = {
  try {
    Some(s.toInt)
  } catch {
    case e: Exception => None
  }
  }
}

trait Mathable {
  def mathValue: Int
}

case class Querry(q: String, t: QuerryType)


/**
 * money with id and type
 * */
case class Account(id: String,value: Int,accType: String) extends Qurriable with Mathable {
  def mathValue = value
  def applyQuerry(q: Querry): Boolean = {
    q.t match {
    case SubString => id.contains(q.q)
    case LessThan => toInt(q.q) match {
                      case Some(x) => (value < x)
                      case None => true
                     }
    case GretherThan => toInt(q.q) match {
                      case Some(x) => value > x
                      case _ => true
                     } 
    case Equals => toInt(q.q) match {
                      case Some(x) => value == x
                      case _ => true
                     }
    case _ => true
  } 
  }
}

object Account {
  val debitAccType: String = "DEBIT"
  val creditAccType: String = "CREDIT"
  val depositAccType: String = "DEPOSIT"
}

case class Field(name: String, value: String)
/**
 * destination with options parameters
 * */
case class Provider(id: String,options: List[Field]) extends Qurriable {
  def mathValue = 0
  def applyQuerry(q: Querry) = {
    q.t match {
    case SubString => id.contains(q.q)
    case _ => false
    }
  }
}

case class Transaction(accountSource: Account, valueSource: Int, destination: Provider)

/**
 * operation transaction or payment
 * */
case class HistoryEvent(id: String, paymentDate: Long, transaction: Transaction) extends Qurriable with Mathable{
  def mathValue = transaction.valueSource
  def applyQuerry(q: Querry) = {
    q.t match {
    case SubString => transaction.destination.id.contains(q.q) ||
                      transaction.accountSource.id.contains(q.q)
    case LessThan => toInt(q.q) match {
                      case Some(x) => (transaction.valueSource < x)
                      case None => true
                     }
    case GretherThan => toInt(q.q) match {
                      case Some(x) => transaction.valueSource > x
                      case _ => true
                     } 
    case Equals => toInt(q.q) match {
                      case Some(x) => transaction.valueSource == x
                      case _ => true
                     }
    case _ => true
  } 
  } 
}

object Transaction{
  
  def mkTransaction(accSrc: Account, valueSrc: Int, dst: Provider) = {
    Transaction(accSrc, valueSrc, dst)
  }
  
  def checkTransaction(transaction: Transaction) = {
    (transaction.accountSource.value < transaction.valueSource)
  }
  
  def startTransaction(transaction: Transaction) = {
    val id = scala.util.Random.nextString(5)
    HistoryEvent(id, Platform.currentTime, transaction)  
  }  
}

/**
 * 
 * */
object Presenter {
  
  def querry(list: List[Qurriable], q: Querry) = {
    list filter ((i: Qurriable) => i.applyQuerry(q))
  }
  
  def querrysOr(list: List[Qurriable], q: List[Querry]) = {
    list filter ((i: Qurriable) => q map (i.applyQuerry(_)) find (x => x) getOrElse(false))
  }
  
  def querrysAnd(list: List[Qurriable], q: List[Querry]) = {
    list filter ((i: Qurriable) => (q map (i.applyQuerry(_)) find (x => !x) getOrElse(true)))
  }
  
  def meanOf(list: List[Mathable]) = {
    if(list.size > 0) {
      sumOf(list: List[Mathable])/list.size  
    } else {
      0
    }
  }
  def maxOf(list: List[Mathable]) = {
    list.max(Ordering.by((_: Mathable).mathValue))
  }
  def minOf(list: List[Mathable]) = {
    list.min(Ordering.by((_: Mathable).mathValue))
  }
  def sumOf(list: List[Mathable]) = {
    list.foldLeft(0)(_ + _.mathValue)    
  }
}