package models

import scala.compat._

object Data {
  
  val defAcc = Account("salary1", 1000, Account.debitAccType);
  
  val emptyProvider = Provider("empty",List());
  
  val accs = List(defAcc,
      Account("salary2",2000,Account.debitAccType),
      Account("credit1",3000,Account.creditAccType),
      Account("credit2",1500,Account.creditAccType),
      Account("deposit",20000,Account.depositAccType))
      
  val providers = List(Provider("mts", List(Field("phone", ""))),
      Provider("internet", List(Field("number", ""))),
      Provider("house", List(Field("city", ""), Field("zip", ""), Field("street", ""))),
      Provider("toAccount", List(Field("accId", ""))),
      Provider("amazon", List(Field("productId", ""))),
      Provider("ebay", List(Field("productId", ""))))
  
  val history = List(HistoryEvent("1", Platform.currentTime, Transaction(getAccount("salary2"),100,getProvider("mts") copy(options = List( Field("number", "9059134799") )))), 
      HistoryEvent("2", Platform.currentTime, Transaction(getAccount("salary1"),200,getProvider("internet") copy(options = List( Field("number", "12345") )))), 
      HistoryEvent("3", Platform.currentTime, Transaction(getAccount("salary1"),300,getProvider("house") copy(options = List( Field("address", "street 1") )) )), 
      HistoryEvent("4", Platform.currentTime, Transaction(getAccount("salary2"),400,getProvider("amazon") copy(options = List( Field("prodictId", "12345") )))) )
  
  val cmd = List(("acc"), "hist", "providers")    
  def getAccount(id: String): Account = {
    accs find (_.id == id) match {
      case Some(x) => x
      case _ => defAcc
    }
  }
  
  def getProvider(id: String): Provider = {
    providers find (_.id == id) match {
      case Some(x) => x
      case _ => emptyProvider
    }
  }
  
}