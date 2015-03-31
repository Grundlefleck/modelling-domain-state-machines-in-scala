package statemachine.v4

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
import statemachine.v4.BankAccount_PreventInvalidTransitionsStartingPoint.Initial
import statemachine.v4.BankAccount_PreventInvalidTransitionsStartingPoint.Initial.AwaitingApproval
import statemachine.v4.BankAccount_PreventInvalidTransitionsStartingPoint.Initial.AwaitingApproval.Open
import statemachine.v4.BankAccount_PreventInvalidTransitionsStartingPoint.Initial.AwaitingApproval.Open.Closed
import statemachine.v4.BankAccount_PreventInvalidTransitionsStartingPoint.Initial.AwaitingApproval.Rejected

/*
 * Okay, this might be a bit insane:
 *  - data is still represented as fields of each state
 *  - still no Nones or nulls
 *  - we can still inspect the method signatures of the state implementations, to see all valid transitions
 *  - within the onEvent method, we can no longer transition to any state we like, as we get a compile error
 *  - constructors of states have private visibility, and are only accessible to the state that is allowed to transition
 *  to it
 *  - if we could transition to a state from more than one other state, we could achieve the same semantics via
 *  traits with limited visibility, but it gets even more crazy
 */
case class BankAccount_PreventInvalidTransitions(state: AccountState) extends BankAccount {

  override def onEvent(event: AccountEvent): BankAccount = (state, event) match {
    case (initial: Initial, ApplicationReceived(customerInfo)) => copy(initial.receiveApplication(customerInfo))
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


object BankAccount_PreventInvalidTransitionsStartingPoint {

  case class Initial private[BankAccount_PreventInvalidTransitionsStartingPoint] () extends AccountState {
    def receiveApplication(customerInfo: CustomerInfo) = AwaitingApproval(customerInfo)
  }

  object Initial {
    case class AwaitingApproval private[Initial] (customerInfo: CustomerInfo) extends AccountState {
      def reject(reason: RejectionReason) = Rejected(reason)
      def approve(accountId: AccountId) = Open(accountId, customerInfo, Balance(0))
    }

    object AwaitingApproval {
      case class Rejected private[AwaitingApproval](reason: RejectionReason) extends AccountState

      case class Open private [AwaitingApproval](accountId: AccountId, customerInfo: CustomerInfo, balance: Balance) extends AccountState {
        def close() = Closed(accountId)
        def transfer(amount: Long): Open = copy(balance = balance.adjustBy(amount))
        def accrueInterest(amount: Long): Open = copy(balance = balance.adjustBy(amount))
      }

      object Open {
        case class Closed private[Open](accountId: AccountId) extends AccountState
      }
    }
  }

  def start() = BankAccount_PreventInvalidTransitions(Initial())
}

