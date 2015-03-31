package statemachine

case class CustomerInfo(name: String) extends AnyVal
case class AccountId(accountNumber: String) extends AnyVal
case class Balance(amount: Long) extends AnyVal {
  def adjustBy(difference: Long) = copy(amount = amount + difference)
}


sealed trait AccountEvent
case class ApplicationReceived(customer: CustomerInfo) extends AccountEvent
case class ApplicationApproved(accountId: AccountId) extends AccountEvent
case class ApplicationRejected(reason: RejectionReason) extends AccountEvent
case class Transfer(amount: Long) extends AccountEvent
case class InterestAccrued(amount: Long) extends AccountEvent
case class AccountClosed() extends AccountEvent


sealed trait RejectionReason
case object BackgroundCheckFailed extends RejectionReason
case object OfferNoLongerAvailable extends RejectionReason
case object InsufficientInformationProvided extends RejectionReason

trait BankAccount {
  def onEvent(event: AccountEvent): BankAccount

  def getBalance: Option[Balance]
  def isOpen: Boolean
  def isRejected: Boolean
  def isClosed: Boolean
}




