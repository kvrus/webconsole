package models

import QuerryType._

object MyObject {
  def main(args: Array[String]) = {
    println("Hello TLang")
    val src = Account("my",1000,Account.debitAccType)
    val dst = Provider("mts",List(Field("phone","98785676y8"),Field("person","papa")))
    val transaction = Transaction.mkTransaction(src, 300, dst)
    println(Transaction.startTransaction(transaction))
    println(Data.accs)
    println(Data.providers)
    println(Data.history)
    println(Presenter.querry(Data.accs,Querry("1100",QuerryType.GretherThan)))
    println(Presenter.querrysOr(Data.accs,List(Querry("1100",QuerryType.GretherThan),
        Querry("3200",QuerryType.LessThan))))
    println(Presenter.querrysAnd(Data.accs,List(Querry("1100",QuerryType.GretherThan),
        Querry("3200",QuerryType.LessThan)))) 
    println(Presenter.sumOf(Data.accs))
    println(Presenter.minOf(Data.accs))
    println(Presenter.maxOf(Data.accs))
    println(Presenter.meanOf(Data.accs))
    
    println(Presenter.sumOf(Data.history))
    println(Presenter.minOf(Data.history))
    println(Presenter.maxOf(Data.history))
    println(Presenter.meanOf(Data.history))
    
    println(Parser.parse(1,"credit2 300 mts 97711111111"))
    println(Parser.parse(1,"credit2 300 house msk 143332 kirova"))
    println(Parser.parse(2,"acc credit2 credit"))
    println(Parser.parse(2,"hist >500"))
    println(Parser.parse(2,"acc =3000"))
    
    println(Parser.parse(3,"mean hist >100"))
    println(Parser.parse(3,"max hist <300"))
    println(Parser.parse(3,"min hist <300"))
    println(Parser.parse(3,"sum hist >100"))
  }
  
  
}