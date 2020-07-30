
class Rational(x: Int, y: Int) {
 
  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
  
  val gcdvalue = if(gcd(x, y) < 0) -gcd(x, y) else gcd(x, y)
  
  val numer = x / gcdvalue
  
  val denom = y / gcdvalue
  
  
  def neg = new Rational(-this.numer,this.denom)
   
  def +(r:Rational) = new Rational( this.numer * r.denom + r.numer * this.denom, denom * r.denom)
  
  def -(r:Rational) = this+r.neg
  
  override def toString = this.numer + "/" + this.denom
 
}

class Account(id:String,n: String, b: Double) {
  
  val nic:String=id
  
  val acnumber: String = n
  
  var balance: Double = b
  
  def withdraw(a:Double) =
    this.balance=this.balance-a

  def deposit(a:Double) =
    this.balance=this.balance+a
  
  def transfer(a:Account,b:Double)= {
    this.withdraw(b)
    a.deposit(b)
  }
  
  def +(a:Account) = new Account( "000000000","0000000000", this.balance+a.balance)
  
  override def toString = "["+nic+":"+acnumber +":"+ balance+"]"
  
}


object FunctionsAndData extends App{
  
  val x = new Rational(3,4)
  
  val y = new Rational(5,8)
  
  val z = new Rational(2,7)
  
  
  println(x.neg)  //Q1
  
  println(x-y-z)  //Q2
  
  val acc1 = new Account("980750736V", "101025454152", 53756.22)
  
  val acc2 = new Account("948552769V", "101038481275", 10233.78)
  
  val acc3 = new Account("984545455V", "101454524483", -756.22)
  
  val acc4 = new Account("946545445V", "101842412515", -1233.78)
  
  val acc5 = new Account("988528483V", "101346794465", 1756.22)
  
  
  acc1.transfer(acc2, 5000.00) //Q3
  
  println("Acc1 Bal: " + acc1.balance)
  
  println("Acc2 Bal: " + acc2.balance)
  
  //Q4 all
  var bank:List[Account]=List(acc1,acc2,acc3,acc4,acc5)
  
  val overdraft=(b:List[Account])=> b.filter(_.balance < 0)
  
  val balance=(b:List[Account])=> b.reduce((x,y) => x+y)
  
  val interest=(b:List[Account])=>b.map(x=> if(x.balance >=0) x.balance * 1.05 else x.balance * 1.1)

  val finalbalance=(b:List[Double]) => b.reduce((x,y) => x+y )
//If balance is positive deposit interest is .05
//If balance is negative overdraft interest is .1
  //val interest=(b:List[Account])=>b.map( )
  
  println(overdraft(bank))
  
  println("Sum of All Accounts Balances: "+ balance(bank))
  
  var newbank = interest(bank)
  
  println(finalbalance(newbank))
  
}