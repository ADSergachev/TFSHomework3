abstract class BankProducts(var balance: Int, val marketingName: String) {
  def putMoney(count: Int): Int = {
    balance = balance + count
    balance
  }

  def withdrawMoney(count: Int): Int
}

class CreditCard(var creditCardBalance: Int, marketingName: String, val limit: Int)
  extends BankProducts(creditCardBalance, marketingName) {
  require(creditCardBalance > -limit, "Баланс должен быть выше лимита. Пополните баланс.")

  /**
    * @inheritdoc
    */
  override def withdrawMoney(count: Int) = {
    require(balance - count > -limit, "Вы не можете снять деньги. Превышен лимит по кредитной карте. Лимит сосавляет: " + limit)

    creditCardBalance = creditCardBalance - count
    this.balance = creditCardBalance
    creditCardBalance
  }
}

class DebetCard(var debetCardBalance: Int, marketingName: String)
  extends BankProducts(debetCardBalance, marketingName) {


  override def withdrawMoney(count: Int) = {
    require(balance - count < 0, "Вы не можете снять деньги. По дебетовой карте баланс не может быть ниже нуля")

    debetCardBalance = debetCardBalance - count
    this.balance = debetCardBalance
    debetCardBalance
  }
}

class Debet(var debetBalance: Int, marketingName: String)
  extends BankProducts(debetBalance, marketingName) {
  override def withdrawMoney(count: Int) = {
    if (balance - count < 0)
      throw new IllegalStateException("Вы не можете снять деньги. Баланс вклада не может быть ниже нуля")

    debetBalance = debetBalance - count
    this.balance = debetBalance
    debetBalance
  }
}

val debet: Debet = new Debet(1200, "debet")

val creditCard: CreditCard = new CreditCard(30, "creditCard", 30)
creditCard.withdrawMoney(70)
val debetCard: DebetCard = new DebetCard(100, "debetCard")

val products: List[BankProducts] = List(debet, creditCard, debetCard)
val sumBalance = products.map(_.balance).sum