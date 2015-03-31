package statemachine.v3

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
import statemachine.RejectionReason
import statemachine.Transfer

/*
 * Implemented in a better way:
 *  - data has been moved to be fields of each state
 *  - the fields are assigned to the state when we transition to it, no nulls or Nones to deal with
 *  - we can inspect the method signatures of the state implementations, to see all valid transitions
 *  - can still transition to any state we like, as long as we have the data to go to it
 */
case class BankAccount_StateMachine(state: AccountState) extends BankAccount {

  override def onEvent(event: AccountEvent): BankAccount = (state, event) match {
    case (Initial, ApplicationReceived(customerInfo)) => copy(Initial.receiveApplication(customerInfo))
    case (awaitingApproval: AwaitingApproval, ApplicationRejected(reason)) => copy(awaitingApproval.reject(reason))
    case (awaitingApproval: AwaitingApproval, ApplicationApproved(accountId)) => copy(awaitingApproval.approve(accountId))
    case (open: Open, AccountClosed()) => copy(open.close())
    case (open: Open, Transfer(amount)) => copy(open.transfer(amount))
    case (open: Open, InterestAccrued(amount)) => copy(open.accrueInterest(amount))
    case _ => InvalidTransition.handleHoweverYouLike(event, state.toString)
  }

  override def getBalance: Option[Balance] = state match {
    case Open(_, _, balance) => Some(balance)
    case _ => None
  }

  override def isOpen: Boolean = state.isInstanceOf[Open]
  override def isRejected: Boolean = state.isInstanceOf[Rejected]
  override def isClosed: Boolean = state.isInstanceOf[Closed]
}


sealed trait AccountState

case object Initial extends AccountState {
  def receiveApplication(customerInfo: CustomerInfo) = AwaitingApproval(customerInfo)
}
case class AwaitingApproval(customerInfo: CustomerInfo) extends AccountState {
  def reject(reason: RejectionReason) = Rejected(reason)
  def approve(accountId: AccountId) = Open(accountId, customerInfo, Balance(0))
}
case class Open(accountId: AccountId, customerInfo: CustomerInfo, balance: Balance) extends AccountState {
  def close() = Closed(accountId)
  def transfer(amount: Long): Open = copy(balance = balance.adjustBy(amount))
  def accrueInterest(amount: Long): Open = copy(balance = balance.adjustBy(amount))
}
case class Closed(accountId: AccountId) extends AccountState
case class Rejected(reason: RejectionReason) extends AccountState
