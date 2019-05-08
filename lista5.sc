//TOMASZ MOSUR 228068

//Zadanie 1
class Pair[A,B](var fst:A, var snd:B) {
  override def toString: String = "[ first: " + fst + ", second: " + snd + "]"
}

//Zadanie 2
//a
class BankAccount(initialBalance : Double) {
  private var balance = initialBalance
  def checkBalance = balance
  def deposit(amount : Double) = { balance += amount; balance}
  def withdraw(amount : Double) = { balance -= amount; balance}
}

class CheckingAccount(initialBalance:Double) extends BankAccount(initialBalance){
  override def deposit(amount: Double): Double = super.deposit(amount-1)

  override def withdraw(amount: Double): Double = super.withdraw(amount+1)
}
//Zadanie 2
//b
class SavingsAccount(initialBalance:Double) extends BankAccount(initialBalance) {
  private var remainingTransactions=3
  def earnMonthlyInterest(): Double = {
    remainingTransactions = 3
    super.deposit(checkBalance*0.0002)
  }

  def transactionLimitControl(): Unit =
  {
    if(remainingTransactions>0) {
      remainingTransactions-=1
    }else{
      super.withdraw(1)
    }
  }

  override def deposit(amount: Double): Double = {
    transactionLimitControl()
    super.deposit(amount)
  }

  override def withdraw(amount: Double): Double = {
    transactionLimitControl()
    super.withdraw(amount)
  }
}
//Zadanie 3
abstract class Zwierz(val imie: String="bez imienia"){
  def rodzaj()=this.getClass.getSimpleName
  def dajGlos():String

  override def toString: String =
    rodzaj() + " - " + imie + " daje glos: " + dajGlos()
}

class Pies(override val imie:String = "bez imienia") extends Zwierz(imie){
  override def dajGlos(): String = "Hau, Hau"
}
class Kot(override val imie:String = "bez imienia") extends Zwierz(imie){
  override def dajGlos(): String = "Miau, Miau"
}

def main(args: Array[String]): Unit = {
  println("Zadanie 1")
  val pair = new Pair(1,"asdf")
  println(pair.toString == "[ first: 1, second: asdf]")

  println("Zadanie 2")
  println("Zadanie a")
  val account = new CheckingAccount(1000)
  println(account.deposit(100) == 1099)
  println(account.withdraw(98) == 1000)
  println("Zadanie b")
  val savingsAccount = new SavingsAccount(1000)
  println(savingsAccount.earnMonthlyInterest()==1000.2)

  savingsAccount.deposit(1)
  savingsAccount.deposit(1)
  println(savingsAccount.deposit(3)==1005.2)
  println(savingsAccount.withdraw(4)==1000.2)

  println("Zadanie3")
  val pies = new Pies()
  println(pies.imie)
  val kot = new Kot("Mruczek")
  val vector = Vector(pies,kot)
  vector.foreach(u => println(u))
}

main(Array())