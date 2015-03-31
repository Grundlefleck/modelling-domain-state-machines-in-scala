package statemachine.domainstatemachine

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
case object Initial extends AccountState {
  def receiveApplication(customerInfo: CustomerInfo) = AwaitingApproval(customerInfo)
}
case class AwaitingApproval(customerInfo: CustomerInfo) extends AccountState {
  def reject() = Rejected
  def approve(accountId: AccountId) = Open(accountId, customerInfo, Balance(0))
}
case class Open(accountId: AccountId, customerInfo: CustomerInfo, balance: Balance) extends AccountState {
  def close() = Closed
  def transfer(amount: Long): Open = copy(balance = balance.adjustBy(amount))
  def accrueInterest(amount: Long): Open = copy(balance = balance.adjustBy(amount))
}
case object Closed extends AccountState
case object Rejected extends AccountState

case class BankAccount_StateMachine(state: AccountState) extends BankAccount {

  override def onEvent(event: AccountEvent): BankAccount = (state, event) match {
    case (Initial, ApplicationReceived(customerInfo)) => copy(Initial.receiveApplication(customerInfo))
    case (awaitingApproval: AwaitingApproval, ApplicationRejected(reason)) => copy(awaitingApproval.reject())
    case (awaitingApproval: AwaitingApproval, ApplicationApproved(accountId)) => copy(awaitingApproval.approve(accountId))
    case (open: Open, AccountClosed()) => copy(open.close())
    case (open: Open, Transfer(amount)) => copy(open.transfer(amount))
    case (open: Open, InterestAccrued(amount)) => copy(open.accrueInterest(amount))
    case _ => invalidTransition(event)
  }

  private def invalidTransition(event: AccountEvent) = {
    // Do whatever you like here, log, ignore, throw exception, etc.
    throw new IllegalArgumentException("Invalid transition: received %s in the %s state.".format(event, state))
  }

  override def getBalance: Option[Balance] = state match {
    case Open(_, _, balance) => Some(balance)
    case _ => None
  }

  override def isOpen: Boolean = state match {
    case _: Open => true
    case _ => false
  }

  override def isRejected: Boolean = state == Rejected

  override def isClosed: Boolean = state == Closed
}
