package statemachine

import java.io.PrintStream

import org.scalatest.exceptions.StackDepth
import statemachine.TuckedAwayImplicits.systemOut
import statemachine.TuckedAwayImplicits.unitToGreeter
import statemachine.v1.BankAccount_Nulls
import statemachine.v2.AccountState
import statemachine.v3.AwaitingApproval
import statemachine.v3.BankAccount_StateMachine
import statemachine.v3.Rejected
import statemachine.v4.BankAccount_PreventInvalidTransitions
import statemachine.v4.BankAccount_PreventInvalidTransitionsStartingPoint.Initial


class INeedAnImplicitParam {
  def sayHello(implicit out: PrintStream) = {
    out.println("Hello world, says the implicit parameter example!")
  }
}

class INeedAnImplicitConversion {
  def sayHello(greeter: Greeter) = {
    greeter.greet()
  }
}

trait Greeter {
  def greet(): Unit
}

object TamingImplicits {

  def main(args: Array[String]): Unit = {

    new INeedAnImplicitParam().sayHello

    new INeedAnImplicitConversion().sayHello(() => println("Hi there, says the implicit conversion example!"))
  }


  /**
   * Other stuff going on in this class.
   */
  def iDoOtherStuffThatRequiresLotsOfImports() = {
    classOf[StackDepth]
    classOf[BankAccount]
    classOf[AccountState]
    classOf[Initial]
    classOf[InvalidTransition]
    classOf[Rejected]
    classOf[AwaitingApproval]
    classOf[BankAccount_Nulls]
    classOf[BankAccount_PreventInvalidTransitions]
    classOf[BankAccount_StateMachine]
  }
}

