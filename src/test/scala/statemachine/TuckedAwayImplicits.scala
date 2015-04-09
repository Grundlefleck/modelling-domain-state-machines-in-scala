package statemachine

object TuckedAwayImplicits {

  implicit val systemOut = System.out

  implicit def unitToGreeter(greeting: () => Unit) = new Greeter {
    override def greet() = greeting()
  }

}
