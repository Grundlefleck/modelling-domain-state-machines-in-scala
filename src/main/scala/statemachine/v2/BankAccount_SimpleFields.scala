package statemachine.v2

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
 * Implemented in a slightly-less-horrible way:
 *  - fields are Options instead of nulls, still they only become Some once we have the data
 *  - state is a field that can be reassigned, with no restriction on which state we can transition to
 */
case class BankAccount_SimpleFields(
    customerInfo: Option[CustomerInfo],
    accountId: Option[AccountId],
    balance: Option[Balance],
    state: AccountState = Initial)
  extends BankAccount {

  override def onEvent(event: AccountEvent) = (state, event) match {
    case (Initial, ApplicationReceived(newCustomerInfo)) => copy(state = AwaitingApproval, customerInfo = Some(newCustomerInfo))
    case (AwaitingApproval, _: ApplicationRejected) => copy(state = Rejected)
    case (AwaitingApproval, _: ApplicationApproved) => copy(state = Open, balance = Some(Balance(0)))
    case (Open, _: AccountClosed) => copy(state = Closed)
    case (Open, Transfer(amount)) => copy(balance = balance.map(_.adjustBy(amount)))
    case (Open, InterestAccrued(amount)) => copy(balance = balance.map(_.adjustBy(amount)))
    case _ => InvalidTransition.handleHoweverYouLike(event, state.toString)
  }

  override def getBalance = balance
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
