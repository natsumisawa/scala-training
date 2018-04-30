object Functor {

  // 愚直ver
  def num2Month(num: Int): String = num match {
    case 1 => "January"
    case 2 => "February"
    case 3 => "March"
    case 4 => "April"
    case 5 => "May"
    case 6 => "June"
    case 7 => "July"
    case 8 => "August"
    case 9 => "September"
    case 10 => "October"
    case 11 => "November"
    case 12 => "December"
    case _ => null
  }

  def month2Initial(num: Int): Command = {
    val month = num2Month(num)

    if (month != null) new MonthCommand(month.substring(0, 3))
    else new NullCommand
  }

  // nullObjectパターン
  trait Command {
    def execute(): Unit
  }

  class MonthCommand(month: String) extends Command {
    override def execute(): Unit = {
      println(month)
    }
  }

  class NullCommand extends Command {
    override def execute(): Unit = {
      // do nothing
    }
  }

  // Optionを使う
  def num2Month2(num: Int): Option[String] =
    Map(1 -> "January", 2 -> "February", 3 -> "March", 4 -> "April", 5 -> "May", 6 -> "June", 7 -> "July", 8 -> "August", 9 -> "September", 10 -> "October", 11 -> "November", 12 -> "December")
    .get(num)

  def month2Initial2(num: Int): Option[String] = {
    (num2Month2(num) match {
      case Some(month) => Some(month.substring(0, 3))
      case None => None
    }) match {
      case Some(month) => Some(if(month == "May") month else s"$month.")
      case None => None
    }
  }

  // Optionで高階関数を使う
  def month2Initial3(num: Int): String = {
    num2Month2(num)
      .map(_.substring(0, 3))
      .map(month => if (month == "May") month else s"$month.")
      .getOrElse(throw new RuntimeException("Illegal number"))
  }

  // Eitherを使う
  def num2Month3(num: Int): Either[Exception, String] =
    Map(1 -> "January", 2 -> "February", 3 -> "March", 4 -> "April", 5 -> "May", 6 -> "June", 7 -> "July", 8 -> "August", 9 -> "September", 10 -> "October", 11 -> "November", 12 -> "December")
      .get(num).toRight(new RuntimeException("Illegal number"))

  def month2Initial4(num: Int): String =
    num2Month3(num)
      .map(_.substring(0, 3))
      .map(month => if (month == "May") month else s"$month.")
      .getOrElse(throw new RuntimeException("Illegal number"))

  // Validateを使う
  import scalaz.Scalaz._
  import scalaz._

  // これは違う、アプリカティブなので今後
  //  def num2Month4(num: Int): ValidationNel[Exception, String] = {
  //    Map(1 -> "January", 2 -> "February", 3 -> "March", 4 -> "April", 5 -> "May", 6 -> "June", 7 -> "July", 8 -> "August", 9 -> "September", 10 -> "October", 11 -> "November", 12 -> "December")
  //      .get(num)
  //      .successNel[String]
  //    (new Exception("Illegal number")).failureNel[Exception]
  //  }

  //  def month2Initial5(num: Int): String =
  //    num2Month4(num)
  //      .map(_.substring(0, 3))
  //      .map(month => if (month == "May") month else s"$month.")
  //      .getOrElse(throw new RuntimeException)
}