package statemachine

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import statemachine.v1.BankAccount_Nulls
import statemachine.v3.BankAccount_StateMachine
import statemachine.v2.BankAccount_SimpleFields
import statemachine.v4.BankAccount_PreventInvalidTransitionsStartingPoint

class BankAccountTest extends FlatSpec with BankAccountBehaviours with Matchers {

  def bankAccountWithNulls = {
    val nullCustomerInfo: CustomerInfo = null.asInstanceOf[CustomerInfo]
    val nullAccountId: AccountId = null.asInstanceOf[AccountId]
    val nullBalance: Balance = null.asInstanceOf[Balance]
    BankAccount_Nulls(nullCustomerInfo, nullAccountId, nullBalance, v1.Initial)
  }
  def bankAccountWithSimpleFields = BankAccount_SimpleFields(None, None, None, v2.Initial)
  def bankAccountWithStateMachine = BankAccount_StateMachine(v3.Initial)
  def bankAccountPreventingInvalidTransitions = v4.BankAccount_PreventInvalidTransitionsStartingPoint.start()

  "A bank account implemented with nulls " should behave like bankAccount(bankAccountWithNulls)
  "A bank account implemented with simple field reassignment " should behave like bankAccount(bankAccountWithSimpleFields)
  "A bank account implemented as a state machine" should behave like bankAccount(bankAccountWithStateMachine)
  "A bank account implemented to prevent invalid transitions" should behave like bankAccount(bankAccountPreventingInvalidTransitions)
}

trait BankAccountBehaviours extends Matchers { this: FlatSpec =>

  def bankAccount(bankAccount: => BankAccount): Unit = {
    it should "be in Rejected state after application rejection" in {
      val afterApplication = bankAccount
        .onEvent(ApplicationReceived(CustomerInfo("Stanley Fink")))
        .onEvent(ApplicationRejected(BackgroundCheckFailed))

      afterApplication should be('rejected)
    }

    it should "be in Open state after an approved application" in {
      val afterApproval = bankAccount
        .onEvent(ApplicationReceived(CustomerInfo("Stanley Fink")))
        .onEvent(ApplicationApproved(AccountId("unique-account-number")))

      afterApproval should be('open)
    }

    it should "be in Closed state after an account closed event" in {
      val afterClosed = bankAccount
        .onEvent(ApplicationReceived(CustomerInfo("Stanley Fink")))
        .onEvent(ApplicationApproved(AccountId("unique-account-number")))
        .onEvent(AccountClosed())

      afterClosed should be('closed)
    }

    it should "have zero balance when just-opened account" in {
      val afterApproval = bankAccount
        .onEvent(ApplicationReceived(CustomerInfo("Stanley Fink")))
        .onEvent(ApplicationApproved(AccountId("unique-account-number")))

      afterApproval.getBalance should be(Some(Balance(0)))
    }

    it should "increase balance by specified amount on receiving a deposit transaction event" in {
      val open = bankAccount
        .onEvent(ApplicationReceived(CustomerInfo("Stanley Fink")))
        .onEvent(ApplicationApproved(AccountId("unique-account-number")))
        .onEvent(Transfer(14))
        .onEvent(Transfer(4))

      open.getBalance should be(Some(Balance(18)))
    }

    it should "decrease the balance by the specified amount on receiving a withdrawal transaction event" in {
      val account = bankAccount
        .onEvent(ApplicationReceived(CustomerInfo("Stanley Fink")))
        .onEvent(ApplicationApproved(AccountId("unique-account-number")))
        .onEvent(Transfer(14))
        .onEvent(Transfer(4))
        .onEvent(Transfer(-12))

      account.getBalance should be(Some(Balance(6)))
    }

    it should "increase the balance by the specified amount on receiving an interest accrued event" in {
      val account = bankAccount
        .onEvent(ApplicationReceived(CustomerInfo("Stanley Fink")))
        .onEvent(ApplicationApproved(AccountId("unique-account-number")))
        .onEvent(Transfer(14))
        .onEvent(InterestAccrued(1))

      account.getBalance should be(Some(Balance(15)))
    }
  }

}

