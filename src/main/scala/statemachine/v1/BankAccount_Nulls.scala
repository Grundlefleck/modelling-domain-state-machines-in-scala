package statemachine.v1

import statemachine.AccountClosed
import statemachine.AccountEvent
import statemachine.AccountId
import statemachine.ApplicationApproved
import statemachine.ApplicationReceived
import statemachine.ApplicationRejected
import statemachine.Balance
import statemachine.BankAccount
import statemachine.CustomerInfo
import statemachine.InterestAccrued
import statemachine.InvalidTransition
import statemachine.Transfer


/*
 * Implemented in a horrible way:
 *  - fields can be null, and only become non-null once we have the data
 *  - state is a field that can be reassigned, with no restriction on which state we can transition to
 */
case class BankAccount_Nulls(
    customerInfo: CustomerInfo,
    accountId: AccountId,
    balance: Balance,
    state: AccountState)
  extends BankAccount {

  override def onEvent(event: AccountEvent) = (state, event) match {
    case (Initial, ApplicationReceived(newCustomerInfo)) => copy(state = AwaitingApproval, customerInfo = newCustomerInfo)
    case (AwaitingApproval, _: ApplicationRejected) => copy(state = Rejected)
    case (AwaitingApproval, _: ApplicationApproved) => copy(state = Open, balance = Balance(0))
    case (Open, _: AccountClosed) => copy(state = Closed)
    case (Open, Transfer(amount)) => copy(balance = balance.adjustBy(amount))
    case (Open, accrual: InterestAccrued) => copy(balance = balance.adjustBy(accrual.amount))
    case _ => InvalidTransition.handleHoweverYouLike(event, state.toString)
  }

  override def getBalance: Option[Balance] = Option(balance)
  override def isOpen: Boolean = state == Open
  override def isRejected: Boolean = state == Rejected
  override def isClosed: Boolean = state == Closed
}


sealed trait AccountState
case object Initial extends AccountState
case object AwaitingApproval extends AccountState
case object Open extends AccountState
case object Closed extends AccountState
case object Rejected extends AccountState

