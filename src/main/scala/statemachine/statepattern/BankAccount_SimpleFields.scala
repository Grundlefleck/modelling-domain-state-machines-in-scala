package statemachine.statepattern

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
import statemachine.Transfer

sealed trait AccountState
case object Initial extends AccountState
case object AwaitingApproval extends AccountState
case object Open extends AccountState
case object Closed extends AccountState
case object Rejected extends AccountState

case class BankAccount_SimpleFields(
    customerInfo: Option[CustomerInfo],
    accountId: Option[AccountId],
    getBalance: Option[Balance],
    state: AccountState = Initial)
  extends BankAccount {

  override def onEvent(event: AccountEvent) = (state, event) match {
    case (Initial, ApplicationReceived(newCustomerInfo)) => copy(state = AwaitingApproval, customerInfo = Some(newCustomerInfo))
    case (AwaitingApproval, _: ApplicationRejected) => copy(state = Rejected)
    case (AwaitingApproval, _: ApplicationApproved) => copy(state = Open, getBalance = Some(Balance(0)))
    case (Open, _: AccountClosed) => copy(state = Closed)
    case (Open, Transfer(amount)) =>
      copy(getBalance = getBalance.map(b => Balance(b.amount + amount)))
    case (Open, accrual: InterestAccrued) => copy(getBalance = getBalance.map(b => Balance(b.amount + accrual.amount)))
    case _ => invalidTransition(event)
  }

  private def invalidTransition(event: AccountEvent) = {
    // Do whatever you like here, log, ignore, throw exception, etc.
    throw new IllegalArgumentException("Invalid transition: received %s in the %s state.".format(event, state))
  }

  override def isOpen: Boolean = state == Open

  override def isRejected: Boolean = state == Rejected

  override def isClosed: Boolean = state == Closed
}
