package statemachine

class InvalidTransition(event: AccountEvent, state: String)
  extends RuntimeException("Invalid transition: received %s in the %s state.".format(event, state))

object InvalidTransition {
  def handleHoweverYouLike(event: AccountEvent, state: String): BankAccount = {
    // Do whatever you like here, log, ignore, throw exception, etc.
    throw new IllegalArgumentException("Invalid transition: received %s in the %s state.".format(event, state))
  }
}
