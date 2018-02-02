REM +==================================================================+
REM |                Copyright (c) 2004, 2014 Oracle Corporation
REM |                   Redwood Shores, California, USA
REM |                        All rights reserved.
REM +==================================================================+
REM |  Name
REM |    apeventb.pls
REM |
REM |  Description - Package AP_ACCOUNTING_EVENTS_PKG
REM |  This package contains the logic for creating events from documents
REM |
REM |  History
REM |    Created By:  Kiran Joshi   (10-APR-2004)
REM |                 Neil Gavacs
REM |    Modified By: Shelley Feng
REM |                 Bug 5007425 - Modify ADJUSTED_TYPE CONSTANT for
REM |                               all Inoice entity event class
REM |                 Bug 4927664 - comment out all the code that set
REM |                               event to No Action
REM |                 Bug 7410001 - Payment Accounting Event is getting stampped
REM |                               for Tax Distributions for Payment Time AWT
REM |                               Accounting_event_id [Payment creation] on
REM |                               AWT distributions is stamped on
REM |                               REC_TAC/ NREC_TAX distributions.
REM +==================================================================+
REM dbdrv: sql ~PROD ~PATH ~FILE none none none package &phase=plb \
REM dbdrv: checkfile(120.54.12010000.71=120.123.12020000.4):~PROD:~PATH:~FILE

SET VERIFY OFF
WHENEVER SQLERROR EXIT FAILURE ROLLBACK
WHENEVER OSERROR EXIT FAILURE ROLLBACK

CREATE OR REPLACE PACKAGE BODY AP_ACCOUNTING_EVENTS_PKG AS
/* $Header: apeventb.pls 120.123.12020000.4 2014/04/30 19:57:43 vasvenka ship $ */

G_CURRENT_RUNTIME_LEVEL     NUMBER       := FND_LOG.G_CURRENT_RUNTIME_LEVEL;
G_LEVEL_UNEXPECTED CONSTANT NUMBER       := FND_LOG.LEVEL_UNEXPECTED;
G_LEVEL_ERROR      CONSTANT NUMBER       := FND_LOG.LEVEL_ERROR;
G_LEVEL_EXCEPTION  CONSTANT NUMBER       := FND_LOG.LEVEL_EXCEPTION;
G_LEVEL_EVENT      CONSTANT NUMBER       := FND_LOG.LEVEL_EVENT;
G_LEVEL_PROCEDURE  CONSTANT NUMBER       := FND_LOG.LEVEL_PROCEDURE;
G_LEVEL_STATEMENT  CONSTANT NUMBER       := FND_LOG.LEVEL_STATEMENT;
G_MODULE_NAME      CONSTANT VARCHAR2(50) :='AP.PLSQL.AP_ACCOUNTING_EVENT_PKG.';

---------------------------------------------------------------------
-- Definition of Accounting Event Entities, Classes and Types.
---------------------------------------------------------------------
---------------------------------------------------------------------
--INVOICES_ENTITY
---------------------------------------------------------------------
INVOICES_ENTITY CONSTANT VARCHAR2(30)            := 'AP_INVOICES';

INVOICES_CLASS CONSTANT VARCHAR2(30)               := 'INVOICES';
  INVOICE_VALIDATED_TYPE CONSTANT VARCHAR2(30)     := 'INVOICE VALIDATED';
  --INVOICE_ADJUSTED_TYPE CONSTANT VARCHAR2(30)      := 'INVOICE ADJUSTED';
  INVOICE_ADJUSTED_TYPE CONSTANT VARCHAR2(30)      := 'INVOICE VALIDATED';
  INVOICE_CANCELLED_TYPE CONSTANT VARCHAR2(30)     := 'INVOICE CANCELLED';

CREDIT_MEMOS_CLASS CONSTANT VARCHAR2(30)           := 'CREDIT MEMOS';
  CREDIT_MEMO_VALIDATED_TYPE CONSTANT VARCHAR2(30) := 'CREDIT MEMO VALIDATED';
  --CREDIT_MEMO_ADJUSTED_TYPE CONSTANT VARCHAR2(30)  := 'CREDIT MEMO ADJUSTED';
  CREDIT_MEMO_ADJUSTED_TYPE CONSTANT VARCHAR2(30)  := 'CREDIT MEMO VALIDATED';
  CREDIT_MEMO_CANCELLED_TYPE CONSTANT VARCHAR2(30) := 'CREDIT MEMO CANCELLED';

DEBIT_MEMOS_CLASS CONSTANT VARCHAR2(30)            := 'DEBIT MEMOS';
  DEBIT_MEMO_VALIDATED_TYPE CONSTANT VARCHAR2(30)  := 'DEBIT MEMO VALIDATED';
  --DEBIT_MEMO_ADJUSTED_TYPE CONSTANT VARCHAR2(30)   := 'DEBIT MEMO ADJUSTED';
  DEBIT_MEMO_ADJUSTED_TYPE CONSTANT VARCHAR2(30)   := 'DEBIT MEMO VALIDATED';
  DEBIT_MEMO_CANCELLED_TYPE CONSTANT VARCHAR2(30)  := 'DEBIT MEMO CANCELLED';

PREPAYMENTS_CLASS CONSTANT VARCHAR2(30)             := 'PREPAYMENTS';
  PREPAYMENT_VALIDATED_TYPE CONSTANT VARCHAR2(30)   := 'PREPAYMENT VALIDATED';
  --PREPAYMENT_ADJUSTED_TYPE CONSTANT VARCHAR2(30)    := 'PREPAYMENT ADJUSTED';
  PREPAYMENT_ADJUSTED_TYPE CONSTANT VARCHAR2(30)    := 'PREPAYMENT VALIDATED';
  PREPAYMENT_CANCELLED_TYPE CONSTANT VARCHAR2(30)   := 'PREPAYMENT CANCELLED';

PREPAYMENT_APPLICATIONS_CLASS CONSTANT VARCHAR2(30):=
                                             'PREPAYMENT APPLICATIONS';
  PREPAYMENT_APPLIED_TYPE CONSTANT VARCHAR2(30)     := 'PREPAYMENT APPLIED';
  PREPAYMENT_UNAPPLIED_TYPE CONSTANT VARCHAR2(30)   := 'PREPAYMENT UNAPPLIED';
  PREPAY_APP_ADJUSTED_TYPE CONSTANT VARCHAR2(30)    :=
-- bug fix 5656160
--                                             'PREPAY APPLICATION ADJUSTED';
                                             'PREPAYMENT APPLICATION ADJ';

--------------------------------------------------------------------
--PAYMENTS_ENTITY
---------------------------------------------------------------------

PAYMENTS_ENTITY CONSTANT VARCHAR2(30)              := 'AP_PAYMENTS';

PAYMENTS_CLASS CONSTANT VARCHAR2(30)                 := 'PAYMENTS';
  PAYMENT_CREATED_TYPE CONSTANT VARCHAR2(30)         := 'PAYMENT CREATED';
  PAYMENT_CANCELLED_TYPE CONSTANT VARCHAR2(30)       := 'PAYMENT CANCELLED';
  MANUAL_PAYMENT_ADJUSTED_TYPE CONSTANT VARCHAR2(30) :=
                                        'MANUAL PAYMENT ADJUSTED';
  PAYMENT_ADJUSTED_TYPE CONSTANT VARCHAR2(30)        := 'PAYMENT ADJUSTED';
  UPGRADED_MAN_PAY_REV_TYPE  CONSTANT VARCHAR2(30)   :=
                                           'UPGRADED MANUAL PMT REVERSED';
  UPGRADED_MAN_PAY_ADJ_TYPE  CONSTANT VARCHAR2(30)   :=
                                           'UPGRADED MANUAL PMT ADJUSTED';

REFUNDS_CLASS CONSTANT VARCHAR2(30)           := 'REFUNDS';
  REFUND_RECORDED_TYPE CONSTANT VARCHAR2(30)  := 'REFUND RECORDED';
  REFUND_CANCELLED_TYPE CONSTANT VARCHAR2(30) := 'REFUND CANCELLED';
  REFUND_ADJUSTED_TYPE CONSTANT VARCHAR2(30)  := 'REFUND ADJUSTED';
  MANUAL_REFUND_ADJUSTED_TYPE CONSTANT VARCHAR2(30) :=
                                       'MANUAL REFUND ADJUSTED'; --bug 10336668

FUTURE_DATED_PAYMENTS_CLASS CONSTANT VARCHAR2(30)   := 'FUTURE DATED PAYMENTS';
  PAYMENT_MATURED_TYPE CONSTANT VARCHAR2(30)          := 'PAYMENT MATURED';
  PAYMENT_MATURITY_REVERSED_TYPE CONSTANT VARCHAR2(30):=
                                        'PAYMENT MATURITY REVERSED';

  PAYMENT_MATURTY_RVRSL_TRX_TYPE CONSTANT VARCHAR2(30):=
                                        'PAYMENT MATURITY REVERSAL';
  PAYMENT_MATURTY_TRX_TYPE CONSTANT VARCHAR2(30):=
                                        'PAYMENT MATURITY';

  PAYMENT_MATURITY_ADJUSTED_TYPE CONSTANT VARCHAR2(30):=
                                        'PAYMENT MATURITY ADJUSTED';

RECONCILED_PAYMENTS_CLASS CONSTANT VARCHAR2(30) := 'RECONCILED PAYMENTS';
  PAYMENT_CLEARED_TYPE CONSTANT VARCHAR2(30)    := 'PAYMENT CLEARED';
  PAYMENT_UNCLEARED_TYPE CONSTANT VARCHAR2(30)  := 'PAYMENT UNCLEARED';
  PAYMENT_CLEARING_ADJUSTED_TYPE CONSTANT VARCHAR2(30) :=
                                        'PAYMENT CLEARING ADJUSTED';

  PAYMENT_CLEARED_TRX_TYPE CONSTANT VARCHAR2(30) := 'PAYMENT CLEARING';
  PAYMENT_UNCLEARED_TRX_TYPE CONSTANT VARCHAR2(30) := 'PAYMENT UNCLEARING';

---------------------------------------------------------------------
--Definition of derive_invoice_events procedure (private)
---------------------------------------------------------------------
PROCEDURE derive_invoice_events
( p_invoice_id IN NUMBER,
  p_calling_sequence IN VARCHAR2
);

---------------------------------------------------------------------
--Definition of create_invoice_event procedure (private)
---------------------------------------------------------------------
FUNCTION create_invoice_event
( p_event_type IN VARCHAR2,
  p_invoice_id IN NUMBER,
  p_event_date IN DATE,
  p_calling_sequence IN VARCHAR2
) RETURN NUMBER;

---------------------------------------------------------------------
--Definition of create_payment_event procedure (private)
---------------------------------------------------------------------
FUNCTION create_payment_event
( p_event_type IN VARCHAR2,
  p_check_id IN NUMBER,
  p_event_date IN DATE,
  p_calling_sequence IN VARCHAR2
) RETURN NUMBER;

---------------------------------------------------------------------
--Definition of is_event_complete function (private)
---------------------------------------------------------------------
FUNCTION is_event_complete
( p_doc_type IN VARCHAR2,
  p_source_id IN NUMBER,
  p_calling_sequence IN VARCHAR2
) RETURN VARCHAR2;

---------------------------------------------------------------------
--Definition of derive_payment_adj_event procedurs (private)
---------------------------------------------------------------------
PROCEDURE derive_payment_adj_event
( p_check_id IN NUMBER,
  p_accounting_date IN DATE,
  p_event_type      IN VARCHAR2,
  p_accounting_event_id OUT NOCOPY NUMBER,
  p_calling_sequence IN VARCHAR2
);

---------------------------------------------------------------------
--Definition of derive_cascade_events procedure (private)
---------------------------------------------------------------------
-- Bug 6996047. Added accounting date parameter
PROCEDURE derive_cascade_events
( p_invoice_id IN NUMBER,
  p_adj_accounting_event_id IN NUMBER,
  p_accounting_date IN DATE,
  p_calling_sequence IN VARCHAR2
);

---------------------------------------------------------------------
--Definition of derive_invoice_cancel_event procedure (private)
---------------------------------------------------------------------
PROCEDURE derive_invoice_cancel_events
( p_invoice_id IN NUMBER,
  p_calling_sequence IN VARCHAR2
);

---------------------------------------------------------------------
--Definition of no_action_pmt_event_update procedure (private)
---------------------------------------------------------------------
PROCEDURE no_action_pmt_event_update
( p_check_id IN NUMBER,
  p_event_type_code IN VARCHAR2,
  p_accounting_date IN DATE,
  p_accounting_event_id IN NUMBER,
  p_calling_sequence IN VARCHAR2
);

---------------------------------------------------------------------
--Definition of get_event_class function (private)
---------------------------------------------------------------------
FUNCTION get_event_class
( p_event_type IN VARCHAR2,
  p_calling_sequence IN VARCHAR2
) RETURN VARCHAR2;

---------------------------------------------------------------------
--Definition of event_security_context function (private)
---------------------------------------------------------------------
FUNCTION get_event_security_context
( p_org_id IN NUMBER,
  p_calling_sequence IN VARCHAR2
) RETURN XLA_EVENTS_PUB_PKG.T_SECURITY;

---------------------------------------------------------------------
--Definition of get_invoice_event_source_info function (private)
---------------------------------------------------------------------
FUNCTION get_invoice_event_source_info
( p_legal_entity_id IN NUMBER,
  p_ledger_id IN NUMBER,
  p_invoice_id IN NUMBER,
  p_calling_sequence IN VARCHAR2
) RETURN XLA_EVENTS_PUB_PKG.T_EVENT_SOURCE_INFO;

---------------------------------------------------------------------
--Definition of get_payment_event_source_info function (private)
---------------------------------------------------------------------
FUNCTION get_payment_event_source_info
( p_legal_entity_id IN NUMBER,
  p_ledger_id IN NUMBER,
  p_check_id IN NUMBER,
  p_calling_sequence IN VARCHAR2
) RETURN XLA_EVENTS_PUB_PKG.T_EVENT_SOURCE_INFO;

---------------------------------------------------------------------
--Definition of get_invoice_info procedure (private)
---------------------------------------------------------------------
PROCEDURE get_invoice_info
( p_invoice_id IN NUMBER,
  p_org_id OUT NOCOPY NUMBER,
  p_legal_entity_id OUT NOCOPY NUMBER,
  p_ledger_id OUT NOCOPY NUMBER,
  p_transaction_date OUT NOCOPY DATE,
  p_calling_sequence IN VARCHAR2
);

---------------------------------------------------------------------
--Definition of get_payment_info procedure (private)
---------------------------------------------------------------------
PROCEDURE get_payment_info
( p_check_id IN NUMBER,
  p_org_id OUT NOCOPY NUMBER,
  p_legal_entity_id OUT NOCOPY NUMBER,
  p_ledger_id OUT NOCOPY NUMBER,
  p_calling_sequence IN VARCHAR2
);

---------------------------------------------------------------------
--Definition of Insert_Prepayment_Header procedure (private)
---------------------------------------------------------------------
PROCEDURE Insert_Prepayment_Header
( p_invoice_id            IN NUMBER,
  p_invoice_line_number   IN NUMBER,
  p_accounting_event_id   IN NUMBER,
  p_accounting_date       IN DATE,
  p_invoice_adjustment_id IN NUMBER,
  p_calling_sequence      IN VARCHAR2
);

---------------------------------------------------------------------
--Definition of Update_Prepayment_Header procedure (private)
---------------------------------------------------------------------
-- Bug 4996808 Creating procedure to update the event_id on header
PROCEDURE Update_Prepayment_Header
( p_invoice_id            IN NUMBER,
  p_invoice_line_number   IN NUMBER,
  p_accounting_event_id   IN NUMBER,
  p_accounting_date       IN DATE,
  p_transaction_type      IN VARCHAR2,
  p_calling_sequence      IN VARCHAR2
);

--added procedure for bug 8527163
PROCEDURE delete_invoice_event
(
  p_accounting_event_id IN NUMBER,
  p_Invoice_Id IN NUMBER,
  p_calling_sequence IN VARCHAR2 DEFAULT NULL
)
IS

l_curr_calling_sequence VARCHAR2(2000);

   -- Logging:
  l_procedure_name CONSTANT VARCHAR2(30) := 'delete_invoice_event';
  l_log_msg        FND_LOG_MESSAGES.MESSAGE_TEXT%TYPE;

l_event_status VARCHAR2(1);
   l_legal_entity_id NUMBER(15);
   l_ledger_id NUMBER(15);
   l_org_id NUMBER(15);
   l_event_security_context XLA_EVENTS_PUB_PKG.T_SECURITY;
   l_event_source_info XLA_EVENTS_PUB_PKG.T_EVENT_SOURCE_INFO;
   l_transaction_date  AP_INVOICES_ALL.invoice_date%TYPE;

BEGIN

    l_curr_calling_sequence :=
    p_calling_sequence || ' -> AP_ACCOUNTING_EVENTS_PKG.CREATE_EVENTS';

  G_CURRENT_RUNTIME_LEVEL := FND_LOG.G_CURRENT_RUNTIME_LEVEL;

  l_log_msg := 'Begin of procedure '|| l_procedure_name;
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
    FND_LOG.STRING(G_LEVEL_PROCEDURE,
                   G_MODULE_NAME||l_procedure_name||'.begin',
                   l_log_msg);
  END IF;
   get_invoice_info
     ( p_invoice_id => p_Invoice_Id,
       p_org_id => l_org_id, -- OUT
       p_legal_entity_id => l_legal_entity_id, -- OUT
       p_ledger_id => l_ledger_id, -- OUT
       p_transaction_date => l_transaction_date, -- OUT
       p_calling_sequence => l_curr_calling_sequence
     );

    l_event_security_context :=
      get_event_security_context
      ( p_org_id => l_org_id,
        p_calling_sequence => l_curr_calling_sequence
      );


    l_event_source_info :=
      get_invoice_event_source_info
      ( p_legal_entity_id => l_legal_entity_id,
        p_ledger_id => l_ledger_id,
        p_invoice_id => p_Invoice_Id,
        p_calling_sequence => l_curr_calling_sequence
      );


      AP_XLA_EVENTS_PKG.delete_event
      ( p_event_source_info => l_event_source_info,
        p_event_id => p_accounting_event_id,
        p_valuation_method => NULL,
         p_security_context => l_event_security_context,
        p_calling_sequence => l_curr_calling_sequence
       );

  EXCEPTION
  WHEN OTHERS THEN
    IF (SQLCODE <> -20001) THEN
       FND_MESSAGE.SET_NAME('SQLAP','AP_DEBUG');
       FND_MESSAGE.SET_TOKEN('ERROR',SQLERRM);
       FND_MESSAGE.SET_TOKEN('CALLING_SEQUENCE',
                    l_curr_calling_sequence);
       FND_MESSAGE.SET_TOKEN('PARAMETERS',
              'p_accounting_event_id = '||p_accounting_event_id
          ||', p_Invoice_Id = '||p_Invoice_Id);
          FND_MESSAGE.SET_TOKEN('DEBUG_INFO',l_log_msg);
    END IF;
    APP_EXCEPTION.RAISE_EXCEPTION();
  END;


/*============================================================================
 |  PROCEDURE -  CREATE_EVENTS (PUBLIC)
 |
 |  DESCRIPTION
 |    This procedure is the single point of entry for the creation of all
 |    events resulting from user actions. Events not resulting from user
 |    actions (i.e. Cascade events and Commitment events which are created
 |    automatically as a result of another event being created) will be
 |    created from the procedure which is called by Create_Events.
 |
 |  PRAMETERS
 |          p_event_type:Possible values:
 |                   'INVOICES', 'INVOICE CANCELLATION', 'PAYMENT',
 |                   'PAYMENT ADJUSTMENT','UPGRADED MANUAL REVERSED PAYMENT',
 |                   'UPGRADED MANUAL ADJUSTED PAYMENT','PAYMENT CANCELLATION',
 |                   'PAYMENT CLEARING', 'PAYMENT MATURITY',
 |                   'PAYMENT MATURITY REVERSAL', 'PAYMENT BATCH'
 |          p_doc_type(IN):This parameter will be necessary to create events
 |                    under the classes Credit Memos, Debit Memos, Prpayments,
 |                    Refunds, Future Dated payments and Reconciled Payments.
 |                    Possible values:
 |                    p_event_type 'INVOICES' or 'INVOICE CANCELLATION':
 |                             'AWT',  'CREDIT,DEBIT',' EXPENSE REPORT',
 |                             'MIXED','INTEREST', 'PREPAYMENT','QUICKDEFAULT',
 |                             'QUICKMATCH', 'STANDARD'
 |                    p_event_type: 'PAYMENT' or 'PAYMENT CANCELLATION':
 |                             'A' - payment batches
 |                             'M' - manual payments
 |                             'Q' - quick payments
 |                             'R' - refunds
 |          P_doc_id: check_id for payment
 |                    invoice_id for invoice
 |          P_accounting_date:    Accounting Date
 |          p_accounting_event_id:Accounting event id generated in this
 |                                procedure
 |          p_checkrun_name:   Payment batch name
 |          p_calling_sequence:Debug information
 |
 |  KNOWN ISSUES:
 |
 |  NOTES:
 |
 |  MODIFICATION HISTORY
 |  Date         Author             Description of Change
 |
 *===========================================================================*/

PROCEDURE Create_Events(p_event_type           IN    VARCHAR2,
                        p_doc_type             IN    VARCHAR2,
                        p_doc_id               IN    NUMBER DEFAULT NULL,
                        p_accounting_date      IN    DATE,
                        p_accounting_event_id  OUT   NOCOPY NUMBER,
                        p_checkrun_name        IN    VARCHAR2,
                        p_calling_sequence     IN    VARCHAR2 DEFAULT NULL)
IS

  l_event_type VARCHAR2(30);
  l_curr_calling_sequence VARCHAR2(2000);

   -- Logging:
  l_procedure_name CONSTANT VARCHAR2(30) := 'Create_Events';
  l_log_msg        FND_LOG_MESSAGES.MESSAGE_TEXT%TYPE;

BEGIN

  l_curr_calling_sequence :=
    p_calling_sequence || ' -> AP_ACCOUNTING_EVENTS_PKG.CREATE_EVENTS';

  G_CURRENT_RUNTIME_LEVEL := FND_LOG.G_CURRENT_RUNTIME_LEVEL;

  l_log_msg := 'Begin of procedure '|| l_procedure_name;
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
    FND_LOG.STRING(G_LEVEL_PROCEDURE,
                   G_MODULE_NAME||l_procedure_name||'.begin',
                   l_log_msg);
  END IF;

  ----------------------------------------------------------------
  -- Calling private procedure to handle differenct events
  ----------------------------------------------------------------

  CASE (p_event_type)

    --------------------------------------------------------------
    -- CASE: 'INVOICES'
    --------------------------------------------------------------
    WHEN ('INVOICES') THEN

      l_log_msg := 'Before calling procedure Derive_invoice_events';
      IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
        FND_LOG.STRING(G_LEVEL_PROCEDURE,
                       G_MODULE_NAME || l_procedure_name,
                       l_log_msg);
      END IF;

      derive_invoice_events(p_invoice_id => p_doc_id,
                            p_calling_sequence => l_curr_calling_sequence);

      l_log_msg := 'After Calling Derive_invoice_events procedure';
      IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
        FND_LOG.STRING(G_LEVEL_PROCEDURE,
                        G_MODULE_NAME || l_procedure_name,
                        l_log_msg);
      END IF;

    -------------------------------------------------------------
    -- CASE: 'INVOICE CANCELLATION
    -------------------------------------------------------------
    WHEN ('INVOICE CANCELLATION') THEN

      l_log_msg := 'Before calling procedure Derive_invoice_cancel_events';
      IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
        FND_LOG.STRING(G_LEVEL_PROCEDURE,
                       G_MODULE_NAME || l_procedure_name,
                       l_log_msg);
      END IF;

      derive_invoice_cancel_events
                        (p_invoice_id => p_doc_id,
                         p_calling_sequence => l_curr_calling_sequence);

      l_log_msg := 'After calling procedure Derive_invoice_cancel_event';
      IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
        FND_LOG.STRING(G_LEVEL_PROCEDURE,
                       G_MODULE_NAME || l_procedure_name,
                       l_log_msg);
       END IF;
    ------------------------------------------------------------
    -- CASE: 'PAYMENT'
    ------------------------------------------------------------
    WHEN ('PAYMENT') THEN

      l_log_msg := 'Before calling procedure create_payment_event';
      IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
        FND_LOG.STRING(G_LEVEL_PROCEDURE,
                       G_MODULE_NAME || l_procedure_name,
                       l_log_msg);
      END IF;

      IF (p_doc_type = 'R') THEN
        l_event_type := REFUND_RECORDED_TYPE;
      ELSE
        l_event_type := PAYMENT_CREATED_TYPE;
      END IF;

      p_accounting_event_id :=
          create_payment_event(p_event_type => l_event_type,
                               p_check_id => p_doc_id,
                               p_event_date => p_accounting_date,
                               p_calling_sequence => l_curr_calling_sequence);

      l_log_msg := 'p_accounting_event_id = '|| p_accounting_event_id;
      IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL ) THEN
        FND_LOG.STRING(G_LEVEL_STATEMENT,
                       G_MODULE_NAME || l_procedure_name,
                       l_log_msg);
       END IF;

      l_log_msg := 'After calling procedure create_payment_event';
      IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
        FND_LOG.STRING(G_LEVEL_PROCEDURE,
                       G_MODULE_NAME || l_procedure_name,
                       l_log_msg);
       END IF;
    ------------------------------------------------------------
    -- CASE: 'REFUND ADJUSTMENT'
    ------------------------------------------------------------
    WHEN ('REFUND ADJUSTMENT') THEN  --bug 10336668


      l_log_msg := 'Before calling procedure derive_payment_adj_event';
      IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
        FND_LOG.STRING(G_LEVEL_PROCEDURE,
                       G_MODULE_NAME || l_procedure_name,
                       l_log_msg);
      END IF;

      derive_payment_adj_event
                       (p_check_id => p_doc_id,
                        p_accounting_date => p_accounting_date,
                        p_event_type => MANUAL_REFUND_ADJUSTED_TYPE,
                        p_accounting_event_id => p_accounting_event_id, --OUT
                        p_calling_sequence => l_curr_calling_sequence);

      l_log_msg := 'After calling procedure derive_payment_adj_event';
      IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
        FND_LOG.STRING(G_LEVEL_PROCEDURE,
                       G_MODULE_NAME || l_procedure_name,
                       l_log_msg);
      END IF;
    ------------------------------------------------------------
    -- CASE: 'PAYMENT ADJUSTMENT'
    ------------------------------------------------------------
    WHEN ('PAYMENT ADJUSTMENT') THEN

      l_log_msg := 'Before calling procedure derive_payment_adj_event';
      IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
        FND_LOG.STRING(G_LEVEL_PROCEDURE,
                       G_MODULE_NAME || l_procedure_name,
                       l_log_msg);
      END IF;

      derive_payment_adj_event
                       (p_check_id => p_doc_id,
                        p_accounting_date => p_accounting_date,
                        p_event_type => MANUAL_PAYMENT_ADJUSTED_TYPE,
                        p_accounting_event_id => p_accounting_event_id, --OUT
                        p_calling_sequence => l_curr_calling_sequence);

      l_log_msg := 'After calling procedure derive_payment_adj_event';
      IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
        FND_LOG.STRING(G_LEVEL_PROCEDURE,
                       G_MODULE_NAME || l_procedure_name,
                       l_log_msg);
      END IF;
    ------------------------------------------------------------
    -- CASE: 'UPGRADED MANUAL REVERSED PAYMENT'
    ------------------------------------------------------------
    WHEN ('UPGRADED MANUAL REVERSED PAYMENT') THEN

      l_log_msg := 'Before calling procedure derive_payment_adj_event';
      IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
        FND_LOG.STRING(G_LEVEL_PROCEDURE,
                       G_MODULE_NAME || l_procedure_name,
                       l_log_msg);
      END IF;

      derive_payment_adj_event
                       (p_check_id => p_doc_id,
                        p_accounting_date => p_accounting_date,
                        p_event_type => UPGRADED_MAN_PAY_REV_TYPE,
                        p_accounting_event_id => p_accounting_event_id, --OUT
                        p_calling_sequence => l_curr_calling_sequence);

      l_log_msg := 'After calling procedure derive_payment_adj_event';
      IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
        FND_LOG.STRING(G_LEVEL_PROCEDURE,
                       G_MODULE_NAME || l_procedure_name,
                       l_log_msg);
      END IF;

    ------------------------------------------------------------
    -- CASE: 'UPGRADED MANUAL ADJUSTED PAYMENT'
    ------------------------------------------------------------
    WHEN ('UPGRADED MANUAL ADJUSTED PAYMENT') THEN

      l_log_msg := 'Before calling procedure derive_payment_adj_event';
      IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
        FND_LOG.STRING(G_LEVEL_PROCEDURE,
                       G_MODULE_NAME || l_procedure_name,
                       l_log_msg);
      END IF;

      derive_payment_adj_event
                       (p_check_id => p_doc_id,
                        p_accounting_date => p_accounting_date,
                        p_event_type => UPGRADED_MAN_PAY_ADJ_TYPE,
                        p_accounting_event_id => p_accounting_event_id, --OUT
                        p_calling_sequence => l_curr_calling_sequence);

      l_log_msg := 'After calling procedure derive_payment_adj_event';
      IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
        FND_LOG.STRING(G_LEVEL_PROCEDURE,
                       G_MODULE_NAME || l_procedure_name,
                       l_log_msg);
      END IF;

    ------------------------------------------------------------
    -- CASE: 'PAYMENT CANCELLATION'
    ------------------------------------------------------------
    WHEN ('PAYMENT CANCELLATION') THEN

      IF (p_doc_type = 'R') THEN
        l_event_type := REFUND_CANCELLED_TYPE;
      ELSE
        l_event_type := PAYMENT_CANCELLED_TYPE;
      END IF;

      ----------------------------------------------------
      -- Step 1: Create payment event
      ----------------------------------------------------
      l_log_msg := 'Before calling procedure create_payment_event';
      IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
               FND_LOG.STRING(G_LEVEL_PROCEDURE,
                              G_MODULE_NAME || l_procedure_name,
                              l_log_msg);
      END IF;


      p_accounting_event_id :=
         create_payment_event(p_event_type => l_event_type,
                              p_check_id => p_doc_id,
                              p_event_date => p_accounting_date,
                              p_calling_sequence => l_curr_calling_sequence);

      l_log_msg := 'p_accounting_event_id = '|| p_accounting_event_id;
      IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL ) THEN
        FND_LOG.STRING(G_LEVEL_STATEMENT,
                       G_MODULE_NAME || l_procedure_name,
                       l_log_msg);
      END IF;

      l_log_msg := 'After calling Procedure create_payment_event';
      IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
        FND_LOG.STRING(G_LEVEL_PROCEDURE,
                       G_MODULE_NAME || l_procedure_name,
                       l_log_msg);
      END IF;



      -----------------------------------------------------
      -- Step 2: calling update payment event to 'No Action'
      -----------------------------------------------------
      l_log_msg := 'comment out procedure no_action_pmt_event_update';
      IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
        FND_LOG.STRING(G_LEVEL_PROCEDURE,
                       G_MODULE_NAME || l_procedure_name,
                       l_log_msg);
      END IF;

      -- Bug 4927664  comment out the NoAction update
      /* start to comment out
      no_action_pmt_event_update(p_check_id => p_doc_id,
                                p_event_type_code => l_event_type,
                                p_accounting_date => p_accounting_date,
                                p_accounting_event_id => p_accounting_event_id,
                                p_calling_sequence => l_curr_calling_sequence);

      l_log_msg := 'After calling procedure no_action_pmt_event_update';
      IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
        FND_LOG.STRING(G_LEVEL_PROCEDURE,
                       G_MODULE_NAME || l_procedure_name,
                       l_log_msg);
      END IF;    End of comment out */

    ------------------------------------------------------------
    -- CASE: 'PAYMENT CLEARING'
    ------------------------------------------------------------
    WHEN ('PAYMENT CLEARING') THEN


      l_log_msg := 'Before calling procedure create_payment_event';
      IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
        FND_LOG.STRING(G_LEVEL_PROCEDURE,
                       G_MODULE_NAME || l_procedure_name,
                       l_log_msg);
      END IF;

      p_accounting_event_id :=
          create_payment_event(p_event_type => PAYMENT_CLEARED_TYPE,
                               p_check_id => p_doc_id,
                               p_event_date => p_accounting_date,
                               p_calling_sequence => l_curr_calling_sequence);

      l_log_msg := 'p_accounting_event_id = '|| p_accounting_event_id;
      IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL ) THEN
        FND_LOG.STRING(G_LEVEL_STATEMENT,
                       G_MODULE_NAME || l_procedure_name,
                       l_log_msg);
      END IF;

      l_log_msg := 'After calling procedure create_payment_event';
      IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
        FND_LOG.STRING(G_LEVEL_PROCEDURE,
                       G_MODULE_NAME || l_procedure_name,
                       l_log_msg);
      END IF;

    ------------------------------------------------------------
    -- CASE: 'PAYMENT UNCLEARING'
    ------------------------------------------------------------
    WHEN ('PAYMENT UNCLEARING') THEN

      ----------------------------------------------------
      -- Step 1: Create payment event
      ----------------------------------------------------
      l_log_msg := 'Before calling procedure create_payment_event';
      IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
         FND_LOG.STRING(G_LEVEL_PROCEDURE,
                        G_MODULE_NAME || l_procedure_name,
                        l_log_msg);
      END IF;

      p_accounting_event_id :=
        create_payment_event
        ( p_event_type => PAYMENT_UNCLEARED_TYPE,
          p_check_id => p_doc_id,
          p_event_date => p_accounting_date,
          p_calling_sequence => l_curr_calling_sequence
        );

      l_log_msg := 'p_accounting_event_id = '|| p_accounting_event_id;
      IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL ) THEN
        FND_LOG.STRING(G_LEVEL_STATEMENT,
                       G_MODULE_NAME || l_procedure_name,
                       l_log_msg);
       END IF;

      l_log_msg := 'After calling procedure create_payment_event';
      IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
        FND_LOG.STRING(G_LEVEL_PROCEDURE,
                       G_MODULE_NAME || l_procedure_name,
                       l_log_msg);
      END IF;

      ----------------------------------------------------
      -- Step 2: Update payment event to 'No Action'
      ----------------------------------------------------
      l_log_msg := 'comment out calling procedure no_action_pmt_event_update';
      IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
        FND_LOG.STRING(G_LEVEL_PROCEDURE,
                       G_MODULE_NAME || l_procedure_name,
                       l_log_msg);
      END IF;

      -- Bug 4927664  comment out the NoAction update
      /* start to comment out
      no_action_pmt_event_update
      ( p_check_id => p_doc_id,
        p_event_type_code => PAYMENT_UNCLEARED_TYPE,
        p_accounting_date => p_accounting_date,
        p_accounting_event_id => p_accounting_event_id,
        p_calling_sequence => l_curr_calling_sequence
      );

      l_log_msg := 'After calling procedure no_action_pmt_event_update';
      IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
        FND_LOG.STRING(G_LEVEL_PROCEDURE,
                       G_MODULE_NAME || l_procedure_name,
                       l_log_msg);
      END IF;    End of comment out */

    ------------------------------------------------------------
    -- CASE: 'PAYMENT MATURITY'
    ------------------------------------------------------------
    WHEN ('PAYMENT MATURITY') THEN

      l_log_msg := 'Before calling procedure create_payment_event';
      IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
        FND_LOG.STRING(G_LEVEL_PROCEDURE,
                       G_MODULE_NAME || l_procedure_name,
                       l_log_msg);
      END IF;

      p_accounting_event_id :=
        create_payment_event
        ( p_event_type => PAYMENT_MATURED_TYPE,
          p_check_id => p_doc_id,
          p_event_date => p_accounting_date,
          p_calling_sequence => l_curr_calling_sequence
        );

      l_log_msg := 'p_accounting_event_id = '|| p_accounting_event_id;
      IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL ) THEN
        FND_LOG.STRING(G_LEVEL_STATEMENT,
                       G_MODULE_NAME || l_procedure_name,
                       l_log_msg);
      END IF;

      l_log_msg := 'After calling procedure create_payment_event';
      IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
        FND_LOG.STRING(G_LEVEL_PROCEDURE,
                       G_MODULE_NAME || l_procedure_name,
                       l_log_msg);
      END IF;

    ------------------------------------------------------------
    -- CASE: 'PAYMENT MATURITY REVERSAL'
    ------------------------------------------------------------
    WHEN ('PAYMENT MATURITY REVERSAL') THEN

       ----------------------------------------------------
      -- Step 1: Create payment event
      ----------------------------------------------------
      l_log_msg := 'Before calling procedure create_payment_event';
      IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
        FND_LOG.STRING(G_LEVEL_PROCEDURE,
                       G_MODULE_NAME || l_procedure_name,
                       l_log_msg);
      END IF;

      p_accounting_event_id :=
        create_payment_event
        ( p_event_type => PAYMENT_MATURITY_REVERSED_TYPE,
          p_check_id => p_doc_id,
          p_event_date => p_accounting_date,
          p_calling_sequence => l_curr_calling_sequence
        );

      l_log_msg := 'p_accounting_event_id = '|| p_accounting_event_id;
      IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL ) THEN
        FND_LOG.STRING(G_LEVEL_STATEMENT,
                       G_MODULE_NAME || l_procedure_name,
                       l_log_msg);
      END IF;

      l_log_msg := 'After calling procedure create_payment_event';
      IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
        FND_LOG.STRING(G_LEVEL_PROCEDURE,
                       G_MODULE_NAME || l_procedure_name,
                       l_log_msg);
      END IF;

      ----------------------------------------------------
      -- Step 2: Update payment event status to 'No Action'
      ----------------------------------------------------
      l_log_msg := 'comment out procedure no_action_pmt_event_update';
      IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
         FND_LOG.STRING(G_LEVEL_PROCEDURE,
                        G_MODULE_NAME || l_procedure_name,
                        l_log_msg);
      END IF;

      -- Bug 4927664  comment out the NoAction update
      /* start to comment out
      no_action_pmt_event_update
      ( p_check_id => p_doc_id,
        p_event_type_code => PAYMENT_MATURITY_REVERSED_TYPE,
        p_accounting_date => p_accounting_date,
        p_accounting_event_id => p_accounting_event_id,
        p_calling_sequence => l_curr_calling_sequence
      );

      l_log_msg := 'After Calling procedure no_pmt_event_update';
      IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
        FND_LOG.STRING(G_LEVEL_PROCEDURE,
                       G_MODULE_NAME || l_procedure_name,
                       l_log_msg);
      END IF;  End of comment out */

    ELSE --other p_event_type

      l_log_msg := 'Exception calling sequence '
                     ||l_curr_calling_sequence
                     ||' Error:Wrong p_event_type= '
                     ||p_event_type;
      IF (G_LEVEL_EXCEPTION >= G_CURRENT_RUNTIME_LEVEL ) THEN
        FND_LOG.STRING(G_LEVEL_EXCEPTION,
                       G_MODULE_NAME || l_procedure_name,
                       l_log_msg);
      END IF;

      APP_EXCEPTION.RAISE_EXCEPTION();

  END CASE; --p_event_type

  l_log_msg := 'End of procedure '|| l_procedure_name;
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
    FND_LOG.STRING(G_LEVEL_PROCEDURE,
                   G_MODULE_NAME||l_procedure_name||'.end',
                   l_log_msg);
  END IF;

EXCEPTION
  WHEN OTHERS THEN
    IF (SQLCODE <> -20001) THEN
       FND_MESSAGE.SET_NAME('SQLAP','AP_DEBUG');
       FND_MESSAGE.SET_TOKEN('ERROR',SQLERRM);
       FND_MESSAGE.SET_TOKEN('CALLING_SEQUENCE',
                    l_curr_calling_sequence);
       FND_MESSAGE.SET_TOKEN('PARAMETERS',
              'p_doc_type = '||p_doc_type
          ||', p_doc_id = '||p_doc_id
          ||', p_accounting_date = '||p_accounting_date
          ||', p_checkrun_name = '||p_checkrun_name);
          FND_MESSAGE.SET_TOKEN('DEBUG_INFO',l_log_msg);
    END IF;
    APP_EXCEPTION.RAISE_EXCEPTION();

END create_events;

/*============================================================================
 |  PROCEDURE -  DERIVE_INVOICE_EVENTS (PRIVATE)
 |
 |  DESCRIPTION
 |    This procedure is responsible for creating Invoice Validated, Invoice
 |    Adjusted (and corresponding events for Credit Memos, Debit Memos, and |    Prepayments), Prepayment Applied and Unapplied Events.
 |
 |  PRAMETERS
 |          p_invoice_id: Invoice ID
 |          p_calling_sequence: Debug info
 |
 |  KNOWN ISSUES:
 |
 |  NOTES:
 |
 |  MODIFICATION HISTORY
 |  Date         Author             Description of Change
 |
 *===========================================================================*/

PROCEDURE Derive_Invoice_Events (p_invoice_id       IN     NUMBER,
                                 p_calling_sequence IN     VARCHAR2)
IS

----------------------------------------------------------------------------
-- We will check if the invoice is approved and that an event does not
-- already exist for this distribution.  If encumbrances are being used, we
-- only want to select distributions with a match_status_flag of 'A',
-- otherwise we want to choose distributions with a status of 'A' or 'T'.
-----------------------------------------------------------------------------
  --bug 7421528  commented the previous select statement and added a new one.

CURSOR Inv_event_dists
IS
  SELECT accounting_date accounting_date,
         SUM(amount) dist_amount,
         DECODE (line_type_lookup_code, 'PREPAY','PREPAY','OTHER') dist_type,
        /*decode (line_type_lookup_code, 'PREPAY', invoice_distribution_id, -1) 
                                invoice_distribution_id,*/
        /* Bug 6931461: invoice_line_number invoice_line_number */
        DECODE(line_type_lookup_code, 'PREPAY', invoice_line_number, 1)
        invoice_line_number
  FROM
    (
      SELECT accounting_date ,
        accounting_event_id ,
        invoice_line_number,
        invoice_id ,
        amount ,
        line_type_lookup_code ,
        parent_reversal_id ,
        set_of_books_id ,
        org_id ,
        prepay_distribution_id ,
        charge_applicable_to_dist_id ,
        cancellation_flag ,
        match_status_flag ,
        awt_invoice_payment_id
      FROM ap_invoice_distributions_all    
    UNION    
    SELECT accounting_date ,
      accounting_event_id ,
      invoice_line_number,
      invoice_id ,
      amount ,
      line_type_lookup_code ,
      parent_reversal_id ,
      set_of_books_id ,
      org_id ,
      prepay_distribution_id ,
      charge_applicable_to_dist_id ,
      cancellation_flag ,
      match_status_flag ,
      awt_invoice_payment_id
    FROM ap_self_assessed_tax_dist_all ) AID,
    financials_system_parameters FSP
  WHERE AID.invoice_id = P_invoice_id
  AND AID.awt_invoice_payment_id IS NULL
  AND AID.org_id = FSP.org_id -- Bug 4516136
  AND AID.set_of_books_id = FSP.set_of_books_id 
  /*Bug 5608968 Avoid full index scan of fsp */
  AND (AID.prepay_distribution_id IS NULL -- prepay_tax_parent_id obsoleted
  OR AID.charge_Applicable_to_dist_id IS NULL) --Added for bug 4643339
  AND AID.accounting_event_id IS NULL
  AND NVL(AID.cancellation_flag, 'N') = 'N' -- replaced cancellation_date
  AND ( (NVL(FSP.purch_encumbrance_flag,'N') = 'N'
  AND match_Status_flag IN ('T','A') )
  OR ((NVL(FSP.purch_encumbrance_flag,'N') = 'Y'
  AND match_Status_flag = 'A')))
    /*since 'OTHER' comes before 'PREPAY' alphabetically, a prepayment
      event will not be created first */
  GROUP BY accounting_date,
    DECODE (line_type_lookup_code, 'PREPAY','PREPAY','OTHER'),
    /* Bug 6718967. Fix to create two events for prepayment applied
     and unapplied. */
    DECODE (line_type_lookup_code, 'PREPAY', DECODE(NVL(parent_reversal_id,-99)
    , -99, 1, 2), 3),
    /* decode (line_type_lookup_code, 'PREPAY',
                                       invoice_distribution_id, -1), */
    DECODE(line_type_lookup_code, 'PREPAY', invoice_line_number, 1) --Bug6931461
  ORDER BY dist_type,
    accounting_date;
  
  CURSOR prepay_adj_events(l_prepay_app_event_id NUMBER,
                          l_accounting_date DATE) is
  SELECT APPH.accounting_event_id
  FROM AP_PREPAY_HISTORY_ALL APPH, AP_INVOICE_DISTRIBUTIONS AID
  WHERE APPH.related_prepay_app_Event_id = l_prepay_app_event_id
  AND   APPH.invoice_adjustment_event_id = AID.accounting_event_id
  AND   nvl(APPH.posted_flag,'N') = 'N';
  --AND   AID.accounting_date = l_accounting_date; Commented for bug 10132577 

  l_event_num NUMBER;
  l_accounting_event_id NUMBER := NULL;

  l_event_type VARCHAR2(30);
  l_event_class VARCHAR2(30);
  l_same_gl_prepay_date DATE;
  l_accounting_date DATE;
  l_pay_accounting_event_id NUMBER;
  l_prepay_event_id NUMBER;
  l_event_status VARCHAR2(1);
  l_prepay_Adj_count NUMBER;

  l_legal_entity_id NUMBER(15);
  l_ledger_id NUMBER(15);
  l_org_id NUMBER(15);
  l_event_security_context XLA_EVENTS_PUB_PKG.T_SECURITY;
  l_event_source_info XLA_EVENTS_PUB_PKG.T_EVENT_SOURCE_INFO;
  l_prepay_app_event_id NUMBER;
  l_inv_adj_event_id    NUMBER;
  l_curr_calling_sequence VARCHAR2(2000);
  l_transaction_date  AP_INVOICES_ALL.invoice_date%TYPE;

  -- Logging:
  l_procedure_name CONSTANT VARCHAR2(30) := 'DERIVE_INVOICE_EVENTS';
  l_log_msg        FND_LOG_MESSAGES.MESSAGE_TEXT%TYPE;

  TYPE l_inv_dist_list IS TABLE OF ap_invoice_distributions.invoice_distribution_id%TYPE;
  l_inv_dist_tab  l_inv_dist_list;

BEGIN

  l_curr_calling_sequence :=
    p_calling_sequence || ' -> AP_ACCOUNTING_EVENTS_PKG.DERIVE_INVOICE_EVENTS';

  G_CURRENT_RUNTIME_LEVEL := FND_LOG.G_CURRENT_RUNTIME_LEVEL;

  l_log_msg := 'Begin of procedure '|| l_procedure_name;
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
      FND_LOG.STRING(G_LEVEL_PROCEDURE,
                     G_MODULE_NAME||l_procedure_name||'.begin',
                     l_log_msg);
  END IF;

  --------------------------------------------------------------------
  -- Figure out the existing adjustment event id.
  --------------------------------------------------------------------
  FOR event_dist_rec IN inv_event_dists LOOP

    l_accounting_event_id := NULL;
    l_pay_accounting_event_id := NULL;     --bug9973070
    l_prepay_event_id := NULL;             --bug9973070

    SELECT COUNT(distinct(accounting_event_id))
    INTO   l_event_num
    FROM   ap_invoice_distributions
    WHERE  invoice_id = p_invoice_id;



    -- Added for prepayment events
    IF (event_dist_rec.dist_type = 'PREPAY') THEN

      IF (event_dist_rec.dist_amount < 0) THEN
        l_event_type := PREPAYMENT_APPLIED_TYPE;
      ELSE
        l_event_type := PREPAYMENT_UNAPPLIED_TYPE;
      END IF;
      l_event_class := PREPAYMENT_APPLICATIONS_CLASS;

      -- if an accounting event already exists for the invoice line related
      -- to this distribution
      -- then stamp this distribution with that the ID of that
      BEGIN
--11659317
        SELECT distinct(AID.accounting_event_id)
        INTO   l_accounting_event_id
        FROM   ap_invoice_distributions AID,
               ap_invoice_lines AIL,
	       xla_events xe
        WHERE  AID.accounting_date = event_dist_rec.accounting_date
        AND    AIL.invoice_id = P_invoice_id
        AND    AIL.line_number = event_dist_rec.invoice_line_number
        AND    AIL.invoice_id = AID.invoice_id
        AND    AIL.line_number = AID.invoice_line_number
        AND    AID.accounting_date = AIL.accounting_date
        AND    AID.line_type_lookup_code = 'PREPAY'
        AND    nvl(posted_flag,'N') = 'N'
        AND    sign(AID.amount) = sign(event_dist_rec.dist_amount)
        AND    AID.accounting_event_id IS NOT NULL
	AND    xe.application_id =200
        AND    xe.event_id =AID.accounting_event_id
        AND     xe.process_status_code <> 'P';

      EXCEPTION
        WHEN NO_DATA_FOUND THEN
          l_log_msg := 'When no_data_found';
          IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
            FND_LOG.STRING(G_LEVEL_PROCEDURE,
                           G_MODULE_NAME || l_procedure_name,
                           l_log_msg);
          END IF;

          l_log_msg := 'Before calling procedure create_invoice_event';
          IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
            FND_LOG.STRING(G_LEVEL_PROCEDURE,
                           G_MODULE_NAME || l_procedure_name,
                           l_log_msg);
          END IF;

          l_accounting_event_id :=
          create_invoice_event (p_event_type => l_event_type,
                                p_invoice_id => p_invoice_id,
                                p_event_date => event_dist_rec.accounting_date,
                                p_calling_sequence => l_curr_calling_sequence);


         l_log_msg := 'After calling procedure create_invoice_event';
         IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
           FND_LOG.STRING(G_LEVEL_PROCEDURE,
                          G_MODULE_NAME || l_procedure_name,
                          l_log_msg);
         END IF;

         IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
           FND_LOG.STRING(G_LEVEL_PROCEDURE,
                          G_MODULE_NAME || l_procedure_name,
                          l_log_msg);
         END IF;

         -- Bug 4996808. Calling the update prepay header instead of the
         -- insert since the prepay header will be created during validation
         -- before the events are created.
         Update_Prepayment_Header
                 (p_invoice_id ,
                  p_invoice_line_number => event_dist_rec.invoice_line_number,
                  p_accounting_event_id => l_accounting_event_id,
                  p_accounting_date => event_dist_rec.accounting_date,
                  p_transaction_type => l_event_type,
                  p_calling_sequence => l_curr_calling_sequence);

         l_log_msg := 'After calling procedure update_prepayment_header';
         IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
           FND_LOG.STRING(G_LEVEL_PROCEDURE,
                          G_MODULE_NAME || l_procedure_name,
                          l_log_msg);
         END IF;
      END;

      IF (l_event_type = PREPAYMENT_UNAPPLIED_TYPE) THEN

        -- Update event Prepay Application Adjustment events related to this
        -- Prepayment Application to 'No Action'
        -- Bug 8547225, posted_flag should be 'N' for AID, not AID2

        BEGIN
          SELECT MAX(AID2.accounting_event_id)
          INTO   l_prepay_app_event_id
          FROM   AP_INVOICE_DISTRIBUTIONS AID, AP_INVOICE_DISTRIBUTIONS AID2
          WHERE  AID.invoice_id = p_invoice_id
          AND    AID.invoice_line_number = event_dist_rec.invoice_line_number
          AND    AID.parent_reversal_id = AID2.invoice_distribution_id
          --AND    AID.accounting_date = AID2.accounting_date Commented for bug 10132577 
          AND    nvl(AID.posted_flag,'N') = 'N';

        EXCEPTION
          WHEN NO_DATA_FOUND THEN
             l_prepay_app_event_id := -1;

        END;

        IF l_prepay_app_event_id <> -1 THEN

            l_log_msg := 'Before calling procedure get_invoice_info';
            IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
              FND_LOG.STRING(G_LEVEL_PROCEDURE,
                             G_MODULE_NAME || l_procedure_name,
                             l_log_msg);
            END IF;

            get_invoice_info(p_invoice_id => p_invoice_id,
                         p_org_id => l_org_id, -- OUT
                         p_legal_entity_id => l_legal_entity_id, -- OUT
                         p_ledger_id => l_ledger_id, -- OUT
                         p_transaction_date => l_transaction_date, -- OUT
                         p_calling_sequence => l_curr_calling_sequence);

            l_log_msg := 'After calling procedure get_insert_info executed';
            IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
               FND_LOG.STRING(G_LEVEL_PROCEDURE,
                              G_MODULE_NAME || l_procedure_name,
                              l_log_msg);
            END IF;

            l_log_msg :='Before calling proc get_event_security_context';
            IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
              FND_LOG.STRING(G_LEVEL_PROCEDURE,
                             G_MODULE_NAME || l_procedure_name,
                             l_log_msg);
            END IF;

            l_event_security_context :=
                   get_event_security_context(p_org_id => l_org_id,
                            p_calling_sequence => l_curr_calling_sequence);

            l_log_msg := 'After calling proc get_event_security_context';
            IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
              FND_LOG.STRING(G_LEVEL_PROCEDURE,
                             G_MODULE_NAME || l_procedure_name,
                             l_log_msg);
            END IF;

            IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
              l_log_msg := 'Before calling proc get_invoice_event_source_info';
              FND_LOG.STRING(G_LEVEL_PROCEDURE,
                             G_MODULE_NAME || l_procedure_name,
                             l_log_msg);
            END IF;

            l_event_source_info :=
               get_invoice_event_source_info
                   (p_legal_entity_id => l_legal_entity_id,
                    p_ledger_id => l_ledger_id,
                    p_invoice_id => p_invoice_id,
                    p_calling_sequence => l_curr_calling_sequence);

            l_log_msg := 'After calling proc get_invoice_envent_source_info';
            IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
              FND_LOG.STRING(G_LEVEL_PROCEDURE,
                             G_MODULE_NAME || l_procedure_name,
                             l_log_msg);
            END IF;

            l_log_msg := 'Before calling AP_XLA_EVENTS_PKG.UPDATE_EVENT';
            IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
              FND_LOG.STRING(G_LEVEL_PROCEDURE,
                             G_MODULE_NAME || l_procedure_name,
                             l_log_msg);
            END IF;

            -- Bug 4748373. Modified the event type to PREPAYMENT UNAPPLIED from
            -- PREPAYMENT APPLICATION UNAPPLIED
            --Update the Prepayment Unapplication event to No Action

            -- Bug 4927664. For now comment out the event NOACTION update
            /* Start comment out
             AP_XLA_EVENTS_PKG.UPDATE_EVENT
                (p_event_source_info => l_event_source_info,
                 p_event_id          => l_Accounting_Event_id,
                 p_event_type_code   => 'PREPAYMENT UNAPPLIED',
                 p_event_date        => NULL,
                 p_event_status_code => XLA_EVENTS_PUB_PKG.C_EVENT_NOACTION,
                 p_valuation_method  => NULL,
                 p_security_context  => l_event_security_context,
                 p_calling_sequence  => l_curr_calling_sequence);

            l_log_msg := 'After calling AP_XLA_EVENTS_PKG.UPDATE_EVENT';
            IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
              FND_LOG.STRING(G_LEVEL_PROCEDURE,
                             G_MODULE_NAME || l_procedure_name,
                             l_log_msg);
            END IF;    End comment out */

            l_log_msg := 'Before calling AP_XLA_EVENTS_PKG.UPDATE_EVENT';
            IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
              FND_LOG.STRING(G_LEVEL_PROCEDURE,
                             G_MODULE_NAME || l_procedure_name,
                             l_log_msg);
            END IF;

            -- Bug 4748373. Modified the event type to PREPAYMENT APPLIED from
            -- PREPAYMENT APPLICATION APPLIED
            --Update the Prepayment Application Event to No Action
            -- Bug 4927664. For now comment out the event NOACTION update
            /* Start comment out
            AP_XLA_EVENTS_PKG.UPDATE_EVENT
                (p_event_source_info => l_event_source_info,
                 p_event_id          => l_prepay_app_event_id,
                 p_event_type_code   => 'PREPAYMENT APPLIED',
                 p_event_date        => NULL,
                 p_event_status_code => XLA_EVENTS_PUB_PKG.C_EVENT_NOACTION,
                 p_valuation_method  => NULL,
                 p_security_context  => l_event_security_context,
                 p_calling_sequence  => l_curr_calling_sequence);

            l_log_msg := 'After calling AP_XLA_EVENTS_PKG.UPDATE_EVENT';
            IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
              FND_LOG.STRING(G_LEVEL_PROCEDURE,
                             G_MODULE_NAME || l_procedure_name,
                             l_log_msg);
            END IF;   End of comment out */


            l_Accounting_date:= event_dist_rec.accounting_date;

            l_log_msg := 'prepay_adj_event loop begin.';
            IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL ) THEN
              FND_LOG.STRING(G_LEVEL_STATEMENT,
                             G_MODULE_NAME || l_procedure_name,
                             l_log_msg);
            END IF;

            FOR l_prepay_adj_event_id IN
                     prepay_adj_events
                         (l_prepay_app_event_id,
                          l_Accounting_date) LOOP

              l_log_msg := 'Inside the prepay_adj_events cursor loop';
              IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
                FND_LOG.STRING(G_LEVEL_PROCEDURE,
                               G_MODULE_NAME || l_procedure_name,
                               l_log_msg);
              END IF;

              -- Bug 4748373. Modified the event type to PREPAYMENT APPLIED from
              -- PREPAYMENT APPLICATION UNAPPLIED
              --Update the Prepayment Application Adjusted Event to No Action
              -- Bug 4927664. For now comment out the event NOACTION update
              -- Bug 8547225 Uncomment the event NOACTION update for PREPAYMENT
              -- APPL ADJUSTMENT event type, and pass appropriate adj event_id

              AP_XLA_EVENTS_PKG.UPDATE_EVENT
                (p_event_source_info => l_event_source_info,
                 p_event_id          => l_prepay_adj_event_id.accounting_event_id,
                 p_event_type_code   => NULL,
                 p_event_date        => NULL,
                 p_event_status_code => XLA_EVENTS_PUB_PKG.C_EVENT_NOACTION,
                 p_valuation_method  => NULL,
                 p_security_context  => l_event_security_context,
                 p_calling_sequence  => l_curr_calling_sequence);

              l_log_msg := 'After calling AP_XLA_EVENTS_PKG.UPDATE_EVENT';
              IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
                FND_LOG.STRING(G_LEVEL_PROCEDURE,
                               G_MODULE_NAME || l_procedure_name,
                               l_log_msg);
              END IF;          --bug 8547225, uncommented the call to update_event

            END LOOP; --prepay_adj_events

            l_log_msg := 'prepay_adj_event loop end.';
            IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL ) THEN
              FND_LOG.STRING(G_LEVEL_STATEMENT,
                             G_MODULE_NAME || l_procedure_name,
                             l_log_msg);
            END IF;
          END IF;

        END IF;

    ELSIF NVL(l_event_num,0)= 0 THEN

      SELECT DECODE ( AI.invoice_type_lookup_code,
                      'CREDIT', CREDIT_MEMO_VALIDATED_TYPE,
                      'DEBIT', DEBIT_MEMO_VALIDATED_TYPE,
                      'PREPAYMENT', PREPAYMENT_VALIDATED_TYPE,
                       INVOICE_VALIDATED_TYPE) event_type,
             DECODE ( AI.invoice_type_lookup_code,
                      'CREDIT', CREDIT_MEMOS_CLASS,
                      'DEBIT', DEBIT_MEMOS_CLASS,
                      'PREPAYMENT', PREPAYMENTS_CLASS,
                       INVOICES_CLASS) event_class
      INTO l_event_type, l_event_class
      FROM ap_invoices_all AI
      WHERE AI.invoice_id = p_invoice_id;

      l_log_msg := 'Before calling procedure create_invoice_event';
      IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
        FND_LOG.STRING(G_LEVEL_PROCEDURE,
                       G_MODULE_NAME || l_procedure_name,
                       l_log_msg);
      END IF;

      l_accounting_event_id :=
        create_invoice_event
        ( p_event_type => l_event_type,
          p_invoice_id => p_invoice_id,
          p_event_date => event_dist_rec.accounting_date,
          p_calling_sequence => l_curr_calling_sequence
        );

      l_log_msg := 'After calling procedure create_invoice_event executed';
      IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
        FND_LOG.STRING(G_LEVEL_PROCEDURE,
                       G_MODULE_NAME || l_procedure_name,
                       l_log_msg);
      END IF;

    ELSE

      SELECT DECODE ( AI.invoice_type_lookup_code,
             'CREDIT', CREDIT_MEMO_ADJUSTED_TYPE,
             'DEBIT', DEBIT_MEMO_ADJUSTED_TYPE,
             'PREPAYMENT', PREPAYMENT_ADJUSTED_TYPE,
             INVOICE_ADJUSTED_TYPE) event_type,
        DECODE ( AI.invoice_type_lookup_code,
             'CREDIT', CREDIT_MEMOS_CLASS,
             'DEBIT', DEBIT_MEMOS_CLASS,
             'PREPAYMENT', PREPAYMENTS_CLASS,
             INVOICES_CLASS) event_class
      INTO l_event_type, l_event_class
      FROM ap_invoices_all AI
      WHERE AI.invoice_id = p_invoice_id;
   --11659317
      BEGIN
        SELECT MAX(accounting_event_id)
        INTO   l_accounting_event_id
        FROM   ap_invoice_distributions,
 	       xla_events xe,
	       xla_transaction_entities_upg xte --bug 13508211
        WHERE  invoice_id = p_invoice_id
        AND    NVL(posted_flag, 'N') <> 'Y'
        AND    line_type_lookup_code <> 'PREPAY'
        AND    prepay_distribution_id is NULL --for prepay tax
        AND    accounting_date = event_dist_rec.accounting_date
        AND    nvl(cancellation_flag, 'N') = 'N' /* Bug 11663644 */
        AND    awt_invoice_payment_id is null -- Bug 7410001
        AND    xe.application_id =200
        AND    xe.event_id = accounting_event_id
	--bug 13508211
	AND    xe.application_id = xte.application_id
	AND    xe.entity_id = xte.entity_id
	AND    xte.entity_code <> 'AP_PAYMENTS'
        AND    xe.process_status_code <> 'P';

      EXCEPTION
        WHEN NO_DATA_FOUND THEN
          l_accounting_event_id := NULL;
      END;

      BEGIN
        -- bug9973070, considering the Encumbered prepay distribution
        -- as well.
        --
        SELECT MAX(accounting_event_id)
        INTO   l_prepay_event_id
        FROM   ap_invoice_distributions AID
        WHERE  AID.invoice_id = p_invoice_id
        AND    AID.line_type_lookup_code = 'PREPAY'
        AND    AID.amount < 0
        AND    (AID.posted_flag = 'Y' OR
                AID.encumbered_flag = 'Y');

      EXCEPTION
        WHEN NO_DATA_FOUND THEN
          l_prepay_event_id := NULL;
      END;

     BEGIN
        SELECT MAX(accounting_event_id)
        INTO   l_pay_accounting_event_id
        FROM   ap_invoice_payments AIP
        WHERE  AIP.invoice_id = p_invoice_id
        AND    AIP.posted_flag = 'Y';
      EXCEPTION
        WHEN NO_DATA_FOUND THEN
          l_pay_accounting_event_id := NULL;
      END;

      -- If an unaccounted Invoice Validates or Invoice Adjustment
      -- event does not already exist for that date, we will create
      -- a new event.

      -- bug fix 5694577: fixed the following condition to raise invoice
      -- validate events properly.
      -- IF ( (l_accounting_event_id IS NULL)
      --      OR
      --      (l_pay_accounting_event_id IS NOT NULL)
      --      OR
      --      (l_prepay_event_id IS NOT NULL)
      --    ) THEN

      IF (l_accounting_event_id IS NULL) THEN

        IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
          l_log_msg := 'Before calling procedure create_invoice_event';
          FND_LOG.STRING(G_LEVEL_PROCEDURE,
                         G_MODULE_NAME || l_procedure_name,
                         l_log_msg);
        END IF;

        l_accounting_event_id :=
          create_invoice_event
          ( p_event_type => l_event_type,
            p_invoice_id => p_invoice_id,
            p_event_date => event_dist_rec.accounting_date,
            p_calling_sequence => l_curr_calling_sequence
          );

        l_log_msg := 'After calling procedure create_invoice_event executed';
        IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
          FND_LOG.STRING(G_LEVEL_PROCEDURE,
                         G_MODULE_NAME || l_procedure_name,
                         l_log_msg);
        END IF;

/*   start commented by abhsaxen for bug 9159069
 *   Moving this code because cascade event will work only
 *   if invoice_distribution is populated earlier
 *   This code is added below (after ap_invoice_distributions update statements).

        -- raise payment/prepayment cascade event only when there is payment
        -- or prepayment application existed for the invoice.

        IF ((l_pay_accounting_event_id IS NOT NULL)
           OR
           (l_prepay_event_id IS NOT NULL))
        THEN

          l_log_msg := 'Before calling procedure derive_cascade_events';
          IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
            FND_LOG.STRING(G_LEVEL_PROCEDURE,
                           G_MODULE_NAME || l_procedure_name,
                           l_log_msg);
          END IF;

          -- Bug 6996047. Passing accounting date so that the adjustment
          -- events are created in the same gl date as the invoice event.
          derive_cascade_events
          ( p_invoice_id => p_invoice_id,
            p_adj_accounting_event_id => l_accounting_event_id,
            p_accounting_date => event_dist_rec.accounting_date,
            p_calling_sequence => l_curr_calling_sequence
          );

          l_log_msg := 'After calling proc derive_cascade_events executed';
          IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
            FND_LOG.STRING(G_LEVEL_PROCEDURE,
                           G_MODULE_NAME || l_procedure_name,
                           l_log_msg);
          END IF;
        END IF;
 end commented by abhsaxen for bug 9159069*/
      ELSE

        -- For cases where we are not creating the event (since it already
        -- exists), we want to check for new status of those existing events.

        IF
        ( is_event_complete ( p_doc_type => INVOICES_ENTITY,
                              p_source_id => p_invoice_id,
                              p_calling_sequence => l_curr_calling_sequence
          ) = 'CREATED') THEN
          l_event_status := XLA_EVENTS_PUB_PKG.C_EVENT_UNPROCESSED;
        ELSE
          l_event_status := XLA_EVENTS_PUB_PKG.C_EVENT_INCOMPLETE;
        END IF;

        get_invoice_info
        ( p_invoice_id => p_invoice_id,
          p_org_id => l_org_id, -- OUT
          p_legal_entity_id => l_legal_entity_id, -- OUT
          p_ledger_id => l_ledger_id, -- OUT
          p_transaction_date => l_transaction_date, -- OUT
          p_calling_sequence => l_curr_calling_sequence
        );

        l_event_security_context :=
          get_event_security_context
          ( p_org_id => l_org_id,
            p_calling_sequence => l_curr_calling_sequence
          );

        l_event_source_info :=
          get_invoice_event_source_info
          ( p_legal_entity_id => l_legal_entity_id,
            p_ledger_id => l_ledger_id,
            p_invoice_id => p_invoice_id,
            p_calling_sequence => l_curr_calling_sequence
          );

        l_log_msg := 'Before calling AP_XLA_EVENTS_PKG.UPDATE_EVENT_STATUS';
        IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
          FND_LOG.STRING(G_LEVEL_PROCEDURE,
                         G_MODULE_NAME || l_procedure_name,
                         l_log_msg);
        END IF;

        AP_XLA_EVENTS_PKG.UPDATE_EVENT_STATUS
        ( p_event_source_info => l_event_source_info,
          p_event_class_code => l_event_class,
          p_event_type_code => l_event_type,
          p_event_date => event_dist_rec.accounting_date,
          p_event_status_code => l_event_status,
          p_valuation_method => NULL,
          p_security_context => l_event_security_context,
          p_calling_sequence => l_curr_calling_sequence
        );

       l_log_msg := 'After calling AP_XLA_EVENTS_PKG.UPDATE_EVENT_STATUS';
       IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
         FND_LOG.STRING(G_LEVEL_PROCEDURE,
                        G_MODULE_NAME || l_procedure_name,
                        l_log_msg);
        END IF;

      END IF;

    END IF;

    --Added IF to ensure that only non prepay lines are updated

    -- These updates are being done in the context of the loop as each
    -- cycle of the loop could be related to a different event
    IF ( l_event_type NOT IN ( PREPAYMENT_APPLIED_TYPE,
                               PREPAY_APP_ADJUSTED_TYPE,
                               PREPAYMENT_UNAPPLIED_TYPE)) THEN

      UPDATE ap_invoice_distributions
      SET    accounting_event_id = l_accounting_event_id
      WHERE  invoice_id = p_invoice_id
      AND    accounting_date = event_dist_rec.accounting_date
      AND    awt_invoice_payment_id IS NULL
      AND    line_type_lookup_code <> 'PREPAY'
      AND    prepay_distribution_id IS NULL --prepay_tax_parent_id obsoleted
      AND    accounting_event_id IS NULL
      AND    nvl(cancellation_flag, 'N') = 'N'; -- Bug 5455054: Added the cancellation_flag check.

      -------------------------------------------------
      -- bug 5525657
      -- We need to stamp the same accounting event id
      -- for self assessed tax distributions.
      -------------------------------------------------

      UPDATE ap_self_assessed_tax_dist_all
      SET    accounting_event_id = l_accounting_event_id
      WHERE  invoice_id = p_invoice_id
      AND    accounting_date = event_dist_rec.accounting_date
      AND    awt_invoice_payment_id IS NULL
      AND    line_type_lookup_code <> 'PREPAY'
      AND    prepay_distribution_id IS NULL
      AND    accounting_event_id IS NULL
      AND    nvl(cancellation_flag, 'N') = 'N';

    ELSE -- for prepay distribution lines

      -- Modified the sqls below for updating the Accounting
      -- Event_IDs on the Invoice distributions, to consider
      -- Prepayment Application and Unapplication distributions
      -- for amount 0
      --
      IF l_event_type = PREPAYMENT_APPLIED_TYPE THEN

        UPDATE ap_invoice_distributions
        SET    accounting_event_id =  l_accounting_event_id
        WHERE  accounting_event_id IS NULL
        AND    invoice_id = p_invoice_id
        AND    invoice_line_number = event_dist_rec.invoice_line_number
        AND    accounting_date = event_dist_rec.accounting_date
        AND    line_type_lookup_code = 'PREPAY'  -- Bug 6718967
        AND    (sign(amount) = sign(event_dist_rec.dist_amount) OR
                (parent_reversal_id IS NULL AND amount = 0))
        RETURNING invoice_distribution_id BULK COLLECT INTO l_inv_dist_tab;

      ELSE

        UPDATE ap_invoice_distributions
        SET    accounting_event_id =  l_accounting_event_id
        WHERE  accounting_event_id IS NULL
        AND    invoice_id = p_invoice_id
        AND    invoice_line_number = event_dist_rec.invoice_line_number
        AND    accounting_date = event_dist_rec.accounting_date
        AND    line_type_lookup_code = 'PREPAY'  -- Bug 6718967
        AND    (sign(amount) = sign(event_dist_rec.dist_amount) OR
                (parent_reversal_id IS NOT NULL AND amount = 0))
        RETURNING invoice_distribution_id BULK COLLECT INTO l_inv_dist_tab;

      END IF;


      FORALL i IN l_inv_dist_tab.FIRST..l_inv_dist_tab.LAST
      UPDATE ap_invoice_distributions_all
      SET    accounting_event_id =  l_accounting_event_id
      WHERE  line_type_lookup_code in ('REC_TAX','NONREC_TAX',
                                       'TRV','TIPV','TERV') --Bug5455985
      AND    accounting_event_id IS NULL
      AND    charge_applicable_to_dist_id = l_inv_dist_tab(i);

    END IF;  -- prepay distribution lines


    /*   start moved by abhsaxen for bug 9159069
     *   Moving this code because cascade event will work only
     *   if invoice_distribution is populated earlier
     */

    -- raise payment/prepayment cascade event only when there is payment
    -- or prepayment application existed for the invoice.

    IF ((l_pay_accounting_event_id IS NOT NULL)
       OR
       (l_prepay_event_id IS NOT NULL))
    THEN

      l_log_msg := 'Before calling procedure derive_cascade_events';
      IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
        FND_LOG.STRING(G_LEVEL_PROCEDURE,
                       G_MODULE_NAME || l_procedure_name,
                       l_log_msg);
      END IF;

      -- Bug 6996047. Passing accounting date so that the adjustment
      -- events are created in the same gl date as the invoice event.
      derive_cascade_events
      ( p_invoice_id => p_invoice_id,
        p_adj_accounting_event_id => l_accounting_event_id,
        p_accounting_date => event_dist_rec.accounting_date,
        p_calling_sequence => l_curr_calling_sequence
      );

      l_log_msg := 'After calling proc derive_cascade_events executed';
      IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
        FND_LOG.STRING(G_LEVEL_PROCEDURE,
                       G_MODULE_NAME || l_procedure_name,
                       l_log_msg);
      END IF;
    END IF;

   /* end moved by abhsaxen for bug 9159069*/

  END LOOP; /*inv_event_dists loop*/

  -- bug9441420
  l_log_msg := 'before calling the procedure Set_Prepay_Event_Noaction '||
               'for invoice_id :'||P_Invoice_ID;
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
    FND_LOG.STRING(G_LEVEL_PROCEDURE,
                   G_MODULE_NAME || l_procedure_name,
                   l_log_msg);
  END IF;

  AP_ACCOUNTING_EVENTS_PKG.Set_Prepay_Event_Noaction
     (p_invoice_id,
      l_curr_calling_sequence);

  l_log_msg := 'After calling the procedure Set_Prepay_Event_Noaction '||
               'for invoice_id :'||P_Invoice_ID;
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
    FND_LOG.STRING(G_LEVEL_PROCEDURE,
                   G_MODULE_NAME || l_procedure_name,
                   l_log_msg);
  END IF;

  l_log_msg := 'End of procedure '|| l_procedure_name;
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
      FND_LOG.STRING(G_LEVEL_PROCEDURE,
                     G_MODULE_NAME||l_procedure_name||'.begin',
                     l_log_msg);
  END IF;

EXCEPTION
  WHEN OTHERS THEN
    IF (inv_event_dists%ISOPEN) THEN
      CLOSE inv_event_dists;
    END IF;

    IF (SQLCODE <> -20001) THEN
       FND_MESSAGE.SET_NAME('SQLAP','AP_DEBUG');
       FND_MESSAGE.SET_TOKEN('ERROR',SQLERRM);
       FND_MESSAGE.SET_TOKEN('CALLING_SEQUENCE',
                    l_curr_calling_sequence);
       FND_MESSAGE.SET_TOKEN('PARAMETERS',
              'p_invoice_id = '||p_invoice_id);
       FND_MESSAGE.SET_TOKEN('DEBUG_INFO',l_log_msg);
     END IF;

    APP_EXCEPTION.RAISE_EXCEPTION();
END derive_invoice_events;


/*============================================================================
 |  PUNCTION -  CREATE_INVOICE_EVENT (PRIVATE)
 |
 |  DESCRIPTION
 |          Create accounting events for 'INVOICES' type
 |
 |  PRAMETERS
 |          p_event_type: Event type
 |          p_invoice_id: Invoice Unique Identifier
 |          P_event_date: Event date
 |          p_calling_sequence: Debug information
 |
 |  RETURN TYPE: NUMBER
 |
 |  KNOWN ISSUES:
 |
 |  NOTES:
 |
 |  MODIFICATION HISTORY
 |  Date         Author             Description of Change
 |
 *===========================================================================*/
FUNCTION create_invoice_event(p_event_type       IN    VARCHAR2,
                              p_invoice_id       IN    NUMBER,
                              p_event_date       IN    DATE,
                              p_calling_sequence IN    VARCHAR2)
RETURN NUMBER
IS

  l_event_status VARCHAR2(1);
  l_legal_entity_id NUMBER(15);
  l_ledger_id NUMBER(15);
  l_org_id NUMBER(15);
  l_event_security_context XLA_EVENTS_PUB_PKG.T_SECURITY;
  l_event_source_info XLA_EVENTS_PUB_PKG.T_EVENT_SOURCE_INFO;
  l_transaction_date  AP_INVOICES_ALL.invoice_date%TYPE;

  l_curr_calling_sequence VARCHAR2(2000);

  -- Logging:
  l_procedure_name CONSTANT VARCHAR2(30) := 'CREATE_INVOICE_EVENT';
  l_log_msg        FND_LOG_MESSAGES.MESSAGE_TEXT%TYPE;

BEGIN

  l_curr_calling_sequence :=
    p_calling_sequence || ' -> AP_ACCOUNTING_EVENTS_PKG.CREATE_INVOICE_EVENT';

  G_CURRENT_RUNTIME_LEVEL := FND_LOG.G_CURRENT_RUNTIME_LEVEL;

  l_log_msg := 'Begin of procedure '|| l_procedure_name;
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
     FND_LOG.STRING(G_LEVEL_PROCEDURE,
                    G_MODULE_NAME || l_procedure_name,
                    l_log_msg);
  END IF;

  IF
  ( is_event_complete
       ( p_doc_type => INVOICES_ENTITY,
         p_source_id => p_invoice_id,
         p_calling_sequence => l_curr_calling_sequence) = 'CREATED') THEN
    l_event_status := XLA_EVENTS_PUB_PKG.C_EVENT_UNPROCESSED;
  ELSE
    l_event_status := XLA_EVENTS_PUB_PKG.C_EVENT_INCOMPLETE;
  END IF;

  get_invoice_info
  ( p_invoice_id => p_invoice_id,
    p_org_id => l_org_id, -- OUT
    p_legal_entity_id => l_legal_entity_id, -- OUT
    p_ledger_id => l_ledger_id, -- OUT
    p_transaction_date => l_transaction_date, -- OUT
    p_calling_sequence => l_curr_calling_sequence
  );

  l_event_security_context :=
    get_event_security_context
    ( p_org_id => l_org_id,
      p_calling_sequence => l_curr_calling_sequence
    );

  l_event_source_info :=
    get_invoice_event_source_info
    ( p_legal_entity_id => l_legal_entity_id,
      p_ledger_id => l_ledger_id,
      p_invoice_id => p_invoice_id,
      p_calling_sequence => l_curr_calling_sequence
    );

  l_log_msg := 'End of procedure '|| l_procedure_name;
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
     FND_LOG.STRING(G_LEVEL_PROCEDURE,
                    G_MODULE_NAME || l_procedure_name,
                    l_log_msg);
  END IF;

  l_log_msg := 'Calling proc AP_XLA_EVENTS_PKG.CREATE_EVENT and return';
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
     FND_LOG.STRING(G_LEVEL_PROCEDURE,
                    G_MODULE_NAME || l_procedure_name,
                    l_log_msg);
  END IF;

  RETURN
    AP_XLA_EVENTS_PKG.CREATE_EVENT
    ( p_event_source_info => l_event_source_info,
      p_event_type_code => p_event_type,
      p_event_date => p_event_date,
      p_event_status_code => l_event_status,
      p_event_number => NULL,
      p_transaction_date => l_transaction_date,
      p_reference_info => NULL,
      p_valuation_method => NULL,
      p_security_context => l_event_security_context,
      p_calling_sequence => l_curr_calling_sequence
    );


EXCEPTION
  WHEN OTHERS THEN
    IF (SQLCODE <> -20001) THEN
       FND_MESSAGE.SET_NAME('SQLAP','AP_DEBUG');
       FND_MESSAGE.SET_TOKEN('ERROR',SQLERRM);
       FND_MESSAGE.SET_TOKEN('CALLING_SEQUENCE',
                    l_curr_calling_sequence);
       FND_MESSAGE.SET_TOKEN('PARAMETERS',
               'p_event_type = '||p_event_type
           ||', p_invoice_id = '||p_invoice_id
           ||', p_event_date = '||p_event_date);
       FND_MESSAGE.SET_TOKEN('DEBUG_INFO',l_log_msg);
    END IF;

    APP_EXCEPTION.RAISE_EXCEPTION();
END create_invoice_event;



/*============================================================================
 |  PUNCTION -  CREATE_PAYMENT_EVENT (PRIVATE)
 |
 |  DESCRIPTION
 |          Create accounting events for 'PAYMENT' type
 |
 |  PRAMETERS
 |          p_event_type: Event type
 |          p_check_id: Check ID
 |          p_event_date: Event date
 |          p_calling_sequence: Debug information
 |
 |  RETURN TYPE: NUMBER
 |
 |  KNOWN ISSUES:
 |
 |  NOTES:
 |
 |  MODIFICATION HISTORY
 |  Date         Author             Description of Change
 |
 *===========================================================================*/
FUNCTION create_payment_event( p_event_type       IN   VARCHAR2,
                               p_check_id         IN   NUMBER,
                               p_event_date       IN   DATE,
                               p_calling_sequence IN   VARCHAR2)
RETURN NUMBER
IS

  l_event_status VARCHAR2(1);
  l_legal_entity_id NUMBER(15);
  l_ledger_id NUMBER(15);
  l_org_id NUMBER(15);
  l_event_security_context XLA_EVENTS_PUB_PKG.T_SECURITY;
  l_event_source_info XLA_EVENTS_PUB_PKG.T_EVENT_SOURCE_INFO;
  l_matured_events_count NUMBER;
  l_unmatured_events_count NUMBER;
  l_amount  NUMBER;
  l_currency_code VARCHAR2(15);
  l_maturity_exchange_rate_type VARCHAR2(30);
  l_maturity_exchange_date DATE;
  l_maturity_exchange_rate NUMBER;
  l_ret NUMBER;
  l_curr_calling_sequence VARCHAR2(2000);

  -- Logging:
  l_procedure_name CONSTANT VARCHAR2(30) := 'CREATE_PAYMENT_EVENT';
  l_log_msg        FND_LOG_MESSAGES.MESSAGE_TEXT%TYPE;
  l_when_to_acct   ap_system_parameters.when_to_account_pmt%TYPE; --bug 8771563

  -- Bug 7374984.
  -- Cursor to get all the unaccounted maturity and clearing transactions
  -- Cursor to get all the unaccounted maturity and clearing transactions
  -- bug9051619, added NOT EXISTS clause
  CURSOR c_unacct_pmt_events IS
  SELECT accounting_date,
         accounting_event_id,
         transaction_type
  FROM   ap_payment_history aph
  WHERE  check_id = p_check_id
  AND    posted_flag <> 'Y'
  AND    transaction_type IN (PAYMENT_MATURTY_RVRSL_TRX_TYPE, PAYMENT_MATURTY_TRX_TYPE,
                              PAYMENT_MATURITY_ADJUSTED_TYPE, PAYMENT_CLEARED_TRX_TYPE,
                              PAYMENT_UNCLEARED_TRX_TYPE, PAYMENT_CLEARING_ADJUSTED_TYPE)
  AND    NOT EXISTS
         (SELECT 1
          FROM ap_payment_history_all aphp,
               xla_events xe,
               ap_system_parameters_all asp
          WHERE aph.check_id = aphp.check_id
          AND aphp.payment_history_id = aph.rev_pmt_hist_id
          AND aphp.transaction_type = PAYMENT_CLEARED_TRX_TYPE
          AND aphp.posted_flag = 'Y'
          AND aph.transaction_type = PAYMENT_UNCLEARED_TRX_TYPE
          AND aph.org_id = asp.org_id
          AND asp.when_to_account_pmt = 'CLEARING ONLY'
          AND aphp.accounting_event_id = xe.event_id
          AND xe.application_id = 200
          AND xe.event_status_code = 'P')
  AND    NVL(fnd_profile.value('FV_ENABLED'), 'N') <> 'Y' --bug11681786 start
  UNION		   
  SELECT accounting_date,
         accounting_event_id,
         transaction_type
  FROM   ap_payment_history aph
  WHERE  check_id = p_check_id
  AND    posted_flag <> 'Y'
  AND    transaction_type IN (PAYMENT_MATURTY_RVRSL_TRX_TYPE, PAYMENT_MATURTY_TRX_TYPE,
                              PAYMENT_CLEARED_TRX_TYPE, PAYMENT_UNCLEARED_TRX_TYPE)
  AND    NOT EXISTS
         (SELECT 1
          FROM ap_payment_history_all aphp,
               xla_events xe
          WHERE aph.check_id = aphp.check_id
          AND aphp.payment_history_id = aph.rev_pmt_hist_id
          AND aphp.posted_flag = 'Y'
          AND aphp.accounting_event_id = xe.event_id
          AND xe.application_id = 200
          AND xe.event_status_code = 'P')
  AND    NVL(fnd_profile.value('FV_ENABLED'), 'N') = 'Y'; --bug11681786 end

    --added cursor for bug 7594938, 10336668 added Manual Ref Adj
  CURSOR c_pmt_acct_clearing_only IS
  SELECT aph.accounting_date,
         aph.accounting_event_id,
         aph.transaction_type
  FROM   ap_payment_history aph, ap_system_parameters asp
  WHERE  aph.check_id = p_check_id
  AND    aph.org_id = asp.org_id
  AND    asp.when_to_account_pmt = 'CLEARING ONLY'
  AND    aph.posted_flag <> 'Y'
  AND    aph.transaction_type IN (PAYMENT_CREATED_TYPE, REFUND_RECORDED_TYPE,
                                  PAYMENT_MATURTY_TRX_TYPE, PAYMENT_MATURTY_RVRSL_TRX_TYPE,
                                  MANUAL_PAYMENT_ADJUSTED_TYPE, MANUAL_REFUND_ADJUSTED_TYPE);

  l_accounting_date       DATE;
  l_accounting_event_id   NUMBER;
  l_transaction_type      VARCHAR2(30);

  --bug 8358552 starts
  l_dummy       NUMBER;
  --bug 8358552 ends

BEGIN

  l_curr_calling_sequence :=
    p_calling_sequence || ' -> AP_ACCOUNTING_EVENTS_PKG.CREATE_PAYMENT_EVENT';

  G_CURRENT_RUNTIME_LEVEL := FND_LOG.G_CURRENT_RUNTIME_LEVEL;

  l_log_msg := 'Begin of procedure '|| l_procedure_name;
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
     FND_LOG.STRING(G_LEVEL_PROCEDURE,
                    G_MODULE_NAME || l_procedure_name,
                    l_log_msg);
  END IF;

  --bug 8771563
  SELECT asp.when_to_account_pmt
  INTO   l_when_to_acct
  FROM   ap_checks ac, ap_system_parameters asp
  WHERE  ac.check_id = p_check_id
  AND    ac.org_id   = asp.org_id;

  IF (p_event_type IN (PAYMENT_CANCELLED_TYPE, REFUND_CANCELLED_TYPE,PAYMENT_CREATED_TYPE, REFUND_RECORDED_TYPE,
                       PAYMENT_MATURED_TYPE, PAYMENT_MATURITY_REVERSED_TYPE,MANUAL_PAYMENT_ADJUSTED_TYPE,--14544093 
					   MANUAL_REFUND_ADJUSTED_TYPE)
       AND l_when_to_acct = 'CLEARING ONLY') THEN  --bug 8771563 and 12603516
        l_event_status := XLA_EVENTS_PUB_PKG.C_EVENT_NOACTION;
  ELSIF (is_event_complete
          (p_doc_type => PAYMENTS_ENTITY,
           p_source_id => p_check_id,
           p_calling_sequence => l_curr_calling_sequence) = 'CREATED') THEN
        l_event_status := XLA_EVENTS_PUB_PKG.C_EVENT_UNPROCESSED;
  ELSE
        l_event_status := XLA_EVENTS_PUB_PKG.C_EVENT_INCOMPLETE;
  END IF;

  get_payment_info
  ( p_check_id => p_check_id,
    p_org_id => l_org_id, -- OUT
    p_legal_entity_id => l_legal_entity_id, -- OUT
    p_ledger_id => l_ledger_id, -- OUT
    p_calling_sequence => l_curr_calling_sequence
  );

  l_event_security_context :=
    get_event_security_context
    ( p_org_id => l_org_id,
      p_calling_sequence => l_curr_calling_sequence
    );

  l_event_source_info :=
    get_payment_event_source_info
    ( p_legal_entity_id => l_legal_entity_id,
      p_ledger_id => l_ledger_id,
      p_check_id => p_check_id,
      p_calling_sequence => l_curr_calling_sequence
    );

  l_log_msg := 'Before calling procedure AP_XLA_EVENTS_PKG.CREATE_EVENT';
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
     FND_LOG.STRING(G_LEVEL_PROCEDURE,
                    G_MODULE_NAME || l_procedure_name,
                    l_log_msg);
  END IF;

    --bug 8358552 starts
    --bug 9069767 starts
    --bug 12555714 Starts : Added the if-else and have the l_event_status accordingly to I when NO_DATA_FOUND
    IF (p_event_type IN (PAYMENT_MATURTY_TRX_TYPE, PAYMENT_MATURTY_RVRSL_TRX_TYPE)) THEN
        BEGIN
                SELECT 1
                INTO l_dummy
                FROM ap_checks_all ac,
                        ap_system_parameters_all asp
                WHERE ac.check_id =l_event_source_info.source_id_int_1
                AND ac.org_id = asp.org_id
                AND exists (
                        SELECT 1
                        FROM ap_payment_history_all aph
                        WHERE ac.check_id = aph.check_id
                        AND aph.transaction_type = PAYMENT_MATURTY_TRX_TYPE
                )
                AND(
                        (
                                ac.currency_code <> asp.base_currency_code
                                AND ac.exchange_rate is not null
                                AND ac.exchange_rate_type is not null
                                AND ac.exchange_date is not null
                                AND ac.maturity_exchange_rate is not null
                                AND ac.maturity_exchange_date is not null
                                AND ac.maturity_exchange_rate_type is not null
                        )
                        OR (ac.currency_code = asp.base_currency_code));
        EXCEPTION
        WHEN NO_DATA_FOUND THEN
                l_event_status := XLA_EVENTS_PUB_PKG.C_EVENT_INCOMPLETE;
        END;
    ELSE
        BEGIN
                SELECT 1
                INTO l_dummy
                FROM ap_checks_all ac,
                        ap_system_parameters_all asp
                WHERE ac.check_id =l_event_source_info.source_id_int_1
                AND ac.org_id = asp.org_id
                /*AND not exists (
                        SELECT 1
                        FROM ap_payment_history_all aph
                        WHERE ac.check_id = aph.check_id
                        AND aph.transaction_type = PAYMENT_MATURTY_TRX_TYPE
                )*/ /* bug 13616704 */
                AND(
                        (
                                ac.currency_code <> asp.base_currency_code
                                AND ac.exchange_rate is not null
                                AND ac.exchange_rate_type is not null
                                AND ac.exchange_date is not null
                        )
                        OR (ac.currency_code = asp.base_currency_code));

        EXCEPTION
        WHEN NO_DATA_FOUND THEN
                l_event_status := XLA_EVENTS_PUB_PKG.C_EVENT_INCOMPLETE;
        END;
    END IF;
/*
    BEGIN
        SELECT 1
        INTO l_dummy
        FROM ap_checks_all ac,
                ap_system_parameters_all asp
        WHERE ac.check_id =l_event_source_info.source_id_int_1
        AND ac.org_id = asp.org_id
        AND exists (
                SELECT 1
                FROM ap_payment_history_all aph
                WHERE ac.check_id = aph.check_id
                AND aph.transaction_type = PAYMENT_MATURTY_TRX_TYPE
        )
        AND(
                (
                        ac.currency_code <> asp.base_currency_code
                        AND ac.exchange_rate is not null
                        AND ac.exchange_rate_type is not null
                        AND ac.exchange_date is not null
                        AND ac.maturity_exchange_rate is not null
                        AND ac.maturity_exchange_date is not null
                        AND ac.maturity_exchange_rate_type is not null
                )
                OR (ac.currency_code = asp.base_currency_code));
    EXCEPTION
        WHEN NO_DATA_FOUND THEN
                BEGIN
                        SELECT 1
                        INTO l_dummy
                        FROM ap_checks_all ac,
                                ap_system_parameters_all asp
                        WHERE ac.check_id =l_event_source_info.source_id_int_1
                        AND ac.org_id = asp.org_id
                        AND not exists (
                                SELECT 1
                                FROM ap_payment_history_all aph
                                WHERE ac.check_id = aph.check_id
                                AND aph.transaction_type = PAYMENT_MATURTY_TRX_TYPE
                        )
                        AND(
                                (
                                        ac.currency_code <> asp.base_currency_code
                                        AND ac.exchange_rate is not null
                                        AND ac.exchange_rate_type is not null
                                        AND ac.exchange_date is not null
                                )
                                OR (ac.currency_code = asp.base_currency_code));

                EXCEPTION
                        WHEN NO_DATA_FOUND THEN
                                l_event_status := XLA_EVENTS_PUB_PKG.C_EVENT_INCOMPLETE;
                END;
    END;
    --bug 9069767 starts
    --bug 8358552 ends
*/
    --bug 12555714 Ends

  l_ret:=
    AP_XLA_EVENTS_PKG.CREATE_EVENT
    ( p_event_source_info => l_event_source_info,
      p_event_type_code => p_event_type,
      p_event_date => p_event_date,
      p_event_status_code => l_event_status,
      p_event_number => NULL,
      p_transaction_date => NULL,
      p_reference_info => NULL,
      p_valuation_method => NULL,
      p_security_context => l_event_security_context,
      p_calling_sequence => l_curr_calling_sequence
    );

  l_log_msg := 'After calling AP_XLA_EVENTS_PKG.CREATE_EVENT';
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
     FND_LOG.STRING(G_LEVEL_PROCEDURE,
                    G_MODULE_NAME || l_procedure_name,
                    l_log_msg);
  END IF;

  -- bug fix 5659451
  -- change hard coded constant to global constant variables
  --bug fix 9136971
  -- added condition for Refund Cancellation event
  IF (p_event_type IN (PAYMENT_CANCELLED_TYPE, REFUND_CANCELLED_TYPE)) THEN

    select count(*)
    into l_matured_events_count
    from   AP_PAYMENT_HISTORY APH
    where  check_id = P_check_id
    and    transaction_type = PAYMENT_MATURTY_TRX_TYPE;


    l_log_msg := 'l_matured_events_count = '
                 ||to_char(l_matured_events_count);
    IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL ) THEN
       FND_LOG.STRING(G_LEVEL_STATEMENT,
                      G_MODULE_NAME || l_procedure_name,
                      l_log_msg);
    END IF;

    select  count(*)
    into l_unmatured_events_count
    from ap_payment_history APH
    where check_id = p_check_id
    and transaction_type = PAYMENT_MATURTY_RVRSL_TRX_TYPE;

    l_log_msg := 'l_unmatured_events_count = '
                 ||to_char(l_unmatured_events_count);
    IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL ) THEN
      FND_LOG.STRING(G_LEVEL_STATEMENT,
                     G_MODULE_NAME || l_procedure_name,
                     l_log_msg);
    END IF;

    IF (l_matured_events_count > 0 and
        l_matured_events_count > l_unmatured_events_count) THEN


       select amount,
              currency_code,
              maturity_exchange_rate_type,
              maturity_exchange_date,
              maturity_exchange_Rate
       into   l_amount,
              l_currency_code,
              l_maturity_exchange_rate_type,
              l_maturity_exchange_date,
              l_maturity_exchange_Rate
       from   ap_Checks
       where  check_id = P_check_id;

       l_log_msg := 'Before ap_reconciliation_pkg.recon_payment_history';
       IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
         FND_LOG.STRING(G_LEVEL_PROCEDURE,
                        G_MODULE_NAME || l_procedure_name,
                        l_log_msg);
       END IF;

       ap_reconciliation_pkg.recon_payment_history
               (X_CHECKRUN_ID            => NULL,
                X_CHECK_ID                => P_check_id,
                X_TRANSACTION_TYPE        => PAYMENT_MATURTY_RVRSL_TRX_TYPE,
                X_ACCOUNTING_DATE         => p_event_date,
                X_CLEARED_DATE            => NULL,
                X_TRANSACTION_AMOUNT      => l_amount,
                X_ERROR_AMOUNT            => NULL,
                X_CHARGE_AMOUNT           => NULL,
                X_CURRENCY_CODE           => l_currency_code,
                X_EXCHANGE_RATE_TYPE      => l_maturity_exchange_rate_type,
                X_EXCHANGE_RATE_DATE      => l_maturity_exchange_date,
                X_EXCHANGE_RATE           => l_maturity_exchange_rate,
                X_MATCHED_FLAG            => NULL,
                X_ACTUAL_VALUE_DATE       => NULL,
                X_CREATION_DATE           => sysdate,
                X_CREATED_BY              => FND_GLOBAL.user_id,
                X_LAST_UPDATE_DATE        => sysdate,
                X_LAST_UPDATED_BY         => FND_GLOBAL.user_id,
                X_LAST_UPDATE_LOGIN       => FND_GLOBAL.login_id,
                X_PROGRAM_UPDATE_DATE     => NULL,
                X_PROGRAM_APPLICATION_ID  => NULL,
                X_PROGRAM_ID              => NULL,
                X_REQUEST_ID              => NULL,
                X_CALLING_SEQUENCE        => l_curr_calling_sequence);

        l_log_msg := 'After ap_reconciliation_pkg.recon_payment_history';
        IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
          FND_LOG.STRING(G_LEVEL_PROCEDURE,
                         G_MODULE_NAME || l_procedure_name,
                         l_log_msg);
        END IF;
    END IF;

    l_log_msg := 'Open Cursor c_unacct_pmt_events';
    IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
       FND_LOG.STRING(G_LEVEL_PROCEDURE,
                      G_MODULE_NAME || l_procedure_name,
                      l_log_msg);
    END IF;

    -- 7374984
    -- Setting the event status to no action for all the unacctd
    -- events of this voided payment.
    OPEN c_unacct_pmt_events;
    LOOP
      FETCH c_unacct_pmt_events INTO
            l_accounting_date,
            l_accounting_event_id,
            l_transaction_type;
      EXIT  WHEN c_unacct_pmt_events%NOTFOUND;

      l_log_msg := 'Event id that needs to be set to no action ' ||
                     l_accounting_event_id;
      IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
          FND_LOG.STRING(G_LEVEL_PROCEDURE,
                         G_MODULE_NAME || l_procedure_name,
                         l_log_msg);
      END IF;

      AP_XLA_EVENTS_PKG.UPDATE_EVENT
      ( p_event_source_info => l_event_source_info,
        p_event_id => l_accounting_event_id,
        p_event_type_code => NULL,
        p_event_date => NULL,
        p_event_status_code => XLA_EVENTS_PUB_PKG.C_EVENT_NOACTION,
        p_valuation_method => NULL,
        p_security_context => l_event_security_context,
        p_calling_sequence => l_curr_calling_sequence
      );

      l_log_msg := 'End of call to AP_XLA_EVENTS_PKG.UPDATE_EVENT';
      IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
          FND_LOG.STRING(G_LEVEL_PROCEDURE,
                         G_MODULE_NAME || l_procedure_name,
                         l_log_msg);
      END IF;

    END LOOP;
    CLOSE c_unacct_pmt_events;
    -- End of bug 7374984

  END IF;


  --added for bug 7594938
  --bug 8771563, if only creation and cancellation events exist then for "clearing only" accounting
  --event_status_code for creation event should be marked as 'N' during payment cancellation
   IF p_event_type IN (PAYMENT_CLEARED_TYPE, PAYMENT_CANCELLED_TYPE,
                       REFUND_CANCELLED_TYPE) THEN
     OPEN c_pmt_acct_clearing_only;
     LOOP

     FETCH c_pmt_acct_clearing_only
      INTO l_accounting_date,
            l_accounting_event_id,
            l_transaction_type;

     EXIT WHEN c_pmt_acct_clearing_only%NOTFOUND;

     l_log_msg := 'Event id that needs to be set to no action ' ||
                     l_accounting_event_id;
      IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
          FND_LOG.STRING(G_LEVEL_PROCEDURE,
                         G_MODULE_NAME || l_procedure_name,
                         l_log_msg);
      END IF;

      AP_XLA_EVENTS_PKG.UPDATE_EVENT
      ( p_event_source_info => l_event_source_info,
        p_event_id => l_accounting_event_id,
        p_event_type_code => NULL,
        p_event_date => NULL,
        p_event_status_code => XLA_EVENTS_PUB_PKG.C_EVENT_NOACTION,
        p_valuation_method => NULL,
        p_security_context => l_event_security_context,
        p_calling_sequence => l_curr_calling_sequence
      );

      l_log_msg := 'End of call to AP_XLA_EVENTS_PKG.UPDATE_EVENT';
      IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
          FND_LOG.STRING(G_LEVEL_PROCEDURE,
                         G_MODULE_NAME || l_procedure_name,
                         l_log_msg);
      END IF;

    END LOOP;

    CLOSE c_pmt_acct_clearing_only;

    END IF;
   --changes for bug 7594938 ends

  l_log_msg := 'End of procedure'||l_procedure_name;
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
      FND_LOG.STRING(G_LEVEL_PROCEDURE,
                     G_MODULE_NAME || l_procedure_name,
                     l_log_msg);
  END IF;

  return l_ret;

EXCEPTION
  WHEN OTHERS THEN
    IF (SQLCODE <> -20001) THEN
       FND_MESSAGE.SET_NAME('SQLAP','AP_DEBUG');
       FND_MESSAGE.SET_TOKEN('ERROR',SQLERRM);
       FND_MESSAGE.SET_TOKEN('CALLING_SEQUENCE',
                    l_curr_calling_sequence);
       FND_MESSAGE.SET_TOKEN('PARAMETERS',
               'p_event_type = '||p_event_type
           ||', p_check_id = '||p_check_id
           ||', p_event_date = '||p_event_date);
       FND_MESSAGE.SET_TOKEN('DEBUG_INFO',l_log_msg);
    END IF;
    APP_EXCEPTION.RAISE_EXCEPTION();
END create_payment_event;

/*============================================================================
 |  PROCEDURE -  UPDATE_INVOICE_EVENT_STATUS (PUBLIC)
 |
 |  DESCRIPTION
 |          Update invoice event's status
 |
 |  PRAMETERS
 |          p_invoice_id: Invoice ID
 |          p_calling_sequence: Debug information
 |
 |  KNOWN ISSUES:
 |
 |  NOTES:
 |
 |  MODIFICATION HISTORY
 |  Date         Author             Description of Change
 |
 *===========================================================================*/
PROCEDURE update_invoice_events_status(
                        p_invoice_id        IN   NUMBER,
                        p_calling_sequence  IN   VARCHAR2)
IS

  TYPE t_check_ids IS TABLE OF NUMBER(15) INDEX BY PLS_INTEGER;
  TYPE t_accounting_event_ids IS TABLE OF NUMBER INDEX BY PLS_INTEGER;

  l_legal_entity_id NUMBER(15);
  l_ledger_id NUMBER(15);
  l_org_id NUMBER(15);
  l_Event_count   NUMBER;
  l_event_security_context XLA_EVENTS_PUB_PKG.T_SECURITY;
  l_event_source_info XLA_EVENTS_PUB_PKG.T_EVENT_SOURCE_INFO;
  l_event_info XLA_EVENTS_PUB_PKG.T_EVENT_INFO;
  l_transaction_date AP_INVOICES_ALL.invoice_date%TYPE;

  l_curr_calling_sequence VARCHAR2(2000);

  -- Logging:
  l_procedure_name CONSTANT VARCHAR2(30) := 'update_invoice_events_status';
  l_log_msg        FND_LOG_MESSAGES.MESSAGE_TEXT%TYPE;
  --7011943
    l_count NUMBER;
    l_check_id NUMBER;
    l_pmt_awt_event_source_info XLA_EVENTS_PUB_PKG.T_EVENT_SOURCE_INFO;
    l_event_source_info_temp XLA_EVENTS_PUB_PKG.T_EVENT_SOURCE_INFO;

    --bug 8358552 starts
    l_dummy number;
    --bug 8358552 ends

BEGIN

  l_curr_calling_sequence := p_calling_sequence ||
              ' -> AP_ACCOUNTING_EVENTS_PKG.UPDATE_INVOICE_EVENTS_STATUS';

  G_CURRENT_RUNTIME_LEVEL := FND_LOG.G_CURRENT_RUNTIME_LEVEL;

  l_log_msg := 'Begin of procedure '||l_procedure_name;
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
        FND_LOG.STRING(G_LEVEL_PROCEDURE,
                       G_MODULE_NAME || l_procedure_name,
                       l_log_msg);
  END IF;

  SELECT count(accounting_Event_id)
  INTO   l_event_count
  FROM   AP_INVOICE_DISTRIBUTIONS AID
  WHERE  AID.accounting_Event_id is not null
  AND    AID.invoice_id = P_Invoice_id;

  IF l_Event_count <> 0 THEN

  get_invoice_info
  ( p_invoice_id => p_invoice_id,
    p_org_id => l_org_id, -- OUT
    p_legal_entity_id => l_legal_entity_id, -- OUT
    p_ledger_id => l_ledger_id, -- OUT
    p_transaction_date => l_transaction_date, -- OUT
    p_calling_sequence => l_curr_calling_sequence
  );

  l_event_security_context :=
    get_event_security_context
    ( p_org_id => l_org_id,
      p_calling_sequence => l_curr_calling_sequence
    );

  DECLARE

    CURSOR l_invoice_distributions_cur IS
      SELECT  distinct(AID.accounting_event_id)
      FROM    ap_invoice_distributions AID
      WHERE   AID.invoice_id = p_invoice_id
      AND     AID.accounting_event_id IS NOT NULL;

    l_invoice_event_status VARCHAR2(1);
    l_do_updates_flag BOOLEAN;
    l_accounting_event_ids t_accounting_event_ids;

  BEGIN

    l_event_source_info :=
      get_invoice_event_source_info
      ( p_legal_entity_id => l_legal_entity_id,
        p_ledger_id => l_ledger_id,
        p_invoice_id => p_invoice_id,
        p_calling_sequence => l_curr_calling_sequence
      );
    l_event_source_info_temp := l_event_source_info ; --7011943
    IF
    ( is_event_complete
      ( p_doc_type => INVOICES_ENTITY,
        p_source_id => p_invoice_id,
        p_calling_sequence => l_curr_calling_sequence
      ) = 'CREATED') THEN

      l_invoice_event_status := XLA_EVENTS_PUB_PKG.C_EVENT_UNPROCESSED;

      l_log_msg := 'Before calling AP_XLA_EVENTS_PKG.EVENT_EXISTS';
      IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
          FND_LOG.STRING(G_LEVEL_PROCEDURE,
                         G_MODULE_NAME || l_procedure_name,
                         l_log_msg);
      END IF;

      l_do_updates_flag :=
        AP_XLA_EVENTS_PKG.EVENT_EXISTS
        ( p_event_source_info => l_event_source_info,
          p_event_class_code => NULL,
          p_event_type_code => NULL,
          p_event_date => NULL,
          p_event_status_code => XLA_EVENTS_PUB_PKG.C_EVENT_INCOMPLETE,
          p_event_number => NULL,
          p_valuation_method => NULL,
          p_security_context => l_event_security_context,
          p_calling_sequence => l_curr_calling_sequence
        );

       l_log_msg := 'After calling AP_XLA_EVENTS_PKG.EVENT_EXISTS';
       IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
          FND_LOG.STRING(G_LEVEL_PROCEDURE,
                         G_MODULE_NAME || l_procedure_name,
                         l_log_msg);
       END IF;

    ELSE

      l_invoice_event_status := XLA_EVENTS_PUB_PKG.C_EVENT_INCOMPLETE;

      l_log_msg := 'Before calling AP_XLA_EVENTS_PKG.EVENT_EXISTS';

      IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
          FND_LOG.STRING(G_LEVEL_PROCEDURE,
                         G_MODULE_NAME || l_procedure_name,
                         l_log_msg);
      END IF;

      l_do_updates_flag :=
        AP_XLA_EVENTS_PKG.EVENT_EXISTS
        ( p_event_source_info => l_event_source_info,
          p_event_class_code => NULL,
          p_event_type_code => NULL,
          p_event_date => NULL,
          p_event_status_code => XLA_EVENTS_PUB_PKG.C_EVENT_UNPROCESSED,
          p_event_number => NULL,
          p_valuation_method => NULL,
          p_security_context => l_event_security_context,
          p_calling_sequence => l_curr_calling_sequence
        );

       l_log_msg := 'After calling AP_XLA_EVENTS_PKG.EVENT_EXISTS';
       IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
          FND_LOG.STRING(G_LEVEL_PROCEDURE,
                         G_MODULE_NAME || l_procedure_name,
                         l_log_msg);
       END IF;

    END IF;

    IF (l_do_updates_flag) THEN

      OPEN l_invoice_distributions_cur;
      LOOP

        l_log_msg := 'Open cursor l_invoice_distributions_cur';
        IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL ) THEN
           FND_LOG.STRING(G_LEVEL_STATEMENT,
                          G_MODULE_NAME || l_procedure_name,
                          l_log_msg);
        END IF;

        FETCH l_invoice_distributions_cur
        BULK COLLECT INTO
          l_accounting_event_ids
        LIMIT 1000;

        FOR i IN 1 ..l_accounting_event_ids.count LOOP

          l_log_msg := 'Before calling AP_XLA_EVENTS_PKG.GET_EVENT_INFO';
          IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
             FND_LOG.STRING(G_LEVEL_PROCEDURE,
                            G_MODULE_NAME || l_procedure_name,
                            l_log_msg);
          END IF;

          --bug 7011943 There are payment time AWT dists.
          select count(*)
          into l_count
          from ap_invoice_distributions_all
          where accounting_event_id = l_accounting_event_ids(i)
          and invoice_id = l_event_source_info.source_id_int_1
          and awt_invoice_payment_id is not null;

          if (l_count > 0) then
            BEGIN
                select  DISTINCT ac.check_id        --bug10024752
                into l_check_id
                from ap_invoice_payments_all aip,
                     ap_checks_all ac
                where aip.check_id=ac.check_id
                  and   aip.accounting_event_id = l_accounting_event_ids(i)
                  and   aip.invoice_id=l_event_source_info.source_id_int_1;


             l_pmt_awt_event_source_info:=get_payment_event_source_info(p_legal_entity_id => l_legal_entity_id,
                              p_ledger_id => l_ledger_id,
                              p_check_id => l_check_id,
                              p_calling_sequence => l_curr_calling_sequence);
             l_event_source_info := l_pmt_awt_event_source_info;

             --bug 8358552 starts
             BEGIN
                     SELECT 1
                     INTO l_dummy
                     FROM ap_checks_all ac,
                        ap_system_parameters_all asp
                     WHERE ac.check_id =l_event_source_info.source_id_int_1
                     AND ac.org_id = asp.org_id
                     AND( (ac.currency_code <> asp.base_currency_code
                     AND ac.exchange_rate is not null
                     AND ac.exchange_rate_type is not null
                     AND ac.exchange_date is not null)
                     OR (ac.currency_code = asp.base_currency_code));
             EXCEPTION
                WHEN NO_DATA_FOUND THEN
                        l_invoice_event_status := XLA_EVENTS_PUB_PKG.C_EVENT_INCOMPLETE;
             END;
             --bug 8358552 ends

            EXCEPTION
             WHEN OTHERS THEN
               NULL;
            END;
          ELSE
            l_event_source_info := l_event_source_info_temp;
          end if;
          --bug 7011943 ends
          l_event_info :=
            AP_XLA_EVENTS_PKG.GET_EVENT_INFO
            ( p_event_source_info => l_event_source_info,
              p_event_id => l_accounting_event_ids(i),
              p_valuation_method => NULL,
              p_security_context => l_event_security_context,
              p_calling_sequence => l_curr_calling_sequence
            );

          --bug 7011943
          l_event_source_info := l_event_source_info_temp;
          l_log_msg := 'After calling AP_XLA_EVENTS_PKG.GET_EVENT_INFO';
          IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
             FND_LOG.STRING(G_LEVEL_PROCEDURE,
                            G_MODULE_NAME || l_procedure_name,
                            l_log_msg);
          END IF;

          IF ( l_event_info.event_status_code IN
               ( XLA_EVENTS_PUB_PKG.C_EVENT_UNPROCESSED,
                 XLA_EVENTS_PUB_PKG.C_EVENT_INCOMPLETE
               ) AND
               l_event_info.event_status_code <> l_invoice_event_status) THEN

            l_log_msg := 'Before AP_XLA_EVENTS_PKG.UPDATE_EVENT_STATUS';
            IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
               FND_LOG.STRING(G_LEVEL_PROCEDURE,
                              G_MODULE_NAME || l_procedure_name,
                              l_log_msg);
            END IF;

            AP_XLA_EVENTS_PKG.UPDATE_EVENT_STATUS
            ( p_event_source_info => l_event_source_info,
              p_event_class_code => null,
              p_event_type_code =>  null,
              p_event_date =>       null,
              p_event_status_code => l_invoice_event_status,
              p_valuation_method => NULL,
              p_security_context => l_event_security_context,
              p_calling_sequence => l_curr_calling_sequence
            );

            l_log_msg := 'After AP_XLA_EVENTS_PKG.GET_EVENT_INFO';
            IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
               FND_LOG.STRING(G_LEVEL_PROCEDURE,
                              G_MODULE_NAME || l_procedure_name,
                              l_log_msg);
            END IF;
          END IF;

        END LOOP;

        EXIT WHEN l_invoice_distributions_cur%NOTFOUND;

      END LOOP;
      CLOSE l_invoice_distributions_cur;

    END IF; -- l_do_updates_flag

  EXCEPTION
    WHEN OTHERS THEN
      IF (l_invoice_distributions_cur%ISOPEN) THEN
        CLOSE l_invoice_distributions_cur;
      END IF;

     l_log_msg := 'Exception calling sequence '
                      ||l_curr_calling_sequence
                      ||' Error: '
                      ||SQLERRM;
     IF (G_LEVEL_EXCEPTION >= G_CURRENT_RUNTIME_LEVEL ) THEN

         FND_LOG.STRING(G_LEVEL_EXCEPTION,
                        G_MODULE_NAME || l_procedure_name,
                        l_log_msg);
      END IF;

      RAISE;
  END;

  DECLARE

 -- Bug 7042296 Modified the cursor to pick the
 -- Payment Adjustment Events.
    CURSOR l_invoice_payments_cur IS  --bug 7042296
      SELECT distinct(AIP.check_id),
             APH.accounting_event_id
      FROM  ap_invoice_payments AIP, ap_payment_history_all APH
      WHERE AIP.invoice_id = p_invoice_id
      AND   AIP.accounting_event_id IS NOT NULL
      and   AIP.check_id = APH.check_id
      and   APH.accounting_event_id is not null
      ORDER BY AIP.check_id;


    l_check_ids t_check_ids;
    l_accounting_event_ids t_accounting_event_ids;
    l_last_check_id NUMBER(15) := NULL;
    l_curr_check_id NUMBER(15);
    l_payment_event_status VARCHAR2(1);
    l_do_updates_flag BOOLEAN;

  BEGIN

    OPEN l_invoice_payments_cur;
    LOOP

      l_log_msg := 'Cursor l_invoice_payment_cur ';
      IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL ) THEN
         FND_LOG.STRING(G_LEVEL_STATEMENT,
                        G_MODULE_NAME || l_procedure_name,
                        l_log_msg);
      END IF;

      FETCH l_invoice_payments_cur
      BULK COLLECT INTO
        l_check_ids,
        l_accounting_event_ids
      LIMIT 1000;

      FOR i IN 1 .. l_accounting_event_ids.count LOOP

        l_curr_check_id := l_check_ids(i);

        IF ( l_last_check_id IS NULL OR
             l_curr_check_id <> l_last_check_id) THEN

          l_last_check_id := l_curr_check_id;

          l_log_msg := 'Before calling proc get_payment_event_source_info';
          IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
            FND_LOG.STRING(G_LEVEL_PROCEDURE,
                           G_MODULE_NAME || l_procedure_name,
                           l_log_msg);
          END IF;

          l_event_source_info :=
            get_payment_event_source_info
            ( p_legal_entity_id => l_legal_entity_id,
              p_ledger_id => l_ledger_id,
              p_check_id => l_curr_check_id,
              p_calling_sequence => l_curr_calling_sequence
            );

          l_log_msg := 'After calling get_payment_event_source_info';
          IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
            FND_LOG.STRING(G_LEVEL_PROCEDURE,
                           G_MODULE_NAME || l_procedure_name,
                           l_log_msg);
          END IF;

          IF ( is_event_complete
               ( p_doc_type => PAYMENTS_ENTITY,
                 p_source_id => l_curr_check_id,
                 p_calling_sequence => l_curr_calling_sequence) = 'CREATED')
          THEN

            l_payment_event_status := XLA_EVENTS_PUB_PKG.C_EVENT_UNPROCESSED;

            l_log_msg := 'Before calling AP_XLA_EVENTS_PKG.EVENT_EIXSTS';
            IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
              FND_LOG.STRING(G_LEVEL_PROCEDURE,
                             G_MODULE_NAME || l_procedure_name,
                             l_log_msg);
            END IF;

            l_do_updates_flag :=
              AP_XLA_EVENTS_PKG.EVENT_EXISTS
              ( p_event_source_info => l_event_source_info,
                p_event_class_code => NULL,
                p_event_type_code => NULL,
                p_event_date => NULL,
                p_event_status_code => XLA_EVENTS_PUB_PKG.C_EVENT_INCOMPLETE,
                p_event_number => NULL,
                p_valuation_method => NULL,
                p_security_context => l_event_security_context,
                p_calling_sequence => l_curr_calling_sequence
              );

             l_log_msg := 'After AP_ACCOUNTING_EVENT_PKG.'
                            ||'get_payment_event_source_info';
             IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
               FND_LOG.STRING(G_LEVEL_PROCEDURE,
                              G_MODULE_NAME || l_procedure_name,
                              l_log_msg);
             END IF;

          ELSE

            l_payment_event_status := XLA_EVENTS_PUB_PKG.C_EVENT_INCOMPLETE;

            l_log_msg := 'Before calling AP_XLA_EVENTS_PKG.EVENT_EXISTS';
            IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
               FND_LOG.STRING(G_LEVEL_PROCEDURE,
                              G_MODULE_NAME || l_procedure_name,
                              l_log_msg);
            END IF;

            l_do_updates_flag :=
              AP_XLA_EVENTS_PKG.EVENT_EXISTS
              ( p_event_source_info => l_event_source_info,
                p_event_class_code => NULL,
                p_event_type_code => NULL,
                p_event_date => NULL,
                p_event_status_code => XLA_EVENTS_PUB_PKG.C_EVENT_UNPROCESSED,
                p_event_number => NULL,
                p_valuation_method => NULL,
                p_security_context => l_event_security_context,
                p_calling_sequence => l_curr_calling_sequence
              );

             l_log_msg := 'After calling AP_XLA_EVENTS_PKG.EVENT_EXISTS';
             IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
               FND_LOG.STRING(G_LEVEL_PROCEDURE,
                              G_MODULE_NAME || l_procedure_name,
                              l_log_msg);
             END IF;
          END IF;

        END IF;

        IF (l_do_updates_flag) THEN

          l_log_msg := 'Before calling AP_XLA_EVENTS_PKG.GET_EVENT_INFO';
          IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
            FND_LOG.STRING(G_LEVEL_PROCEDURE,
                           G_MODULE_NAME || l_procedure_name,
                           l_log_msg);
          END IF;

          l_event_info :=
            AP_XLA_EVENTS_PKG.GET_EVENT_INFO
            ( p_event_source_info => l_event_source_info,
              p_event_id => l_accounting_event_ids(i),
              p_valuation_method => NULL,
              p_security_context => l_event_security_context,
              p_calling_sequence => l_curr_calling_sequence
            );

          l_log_msg := 'After calling AP_XLA_EVENTS_PKG.GET_EVENT_INFO';
          IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
            FND_LOG.STRING(G_LEVEL_PROCEDURE,
                           G_MODULE_NAME || l_procedure_name,
                           l_log_msg);
          END IF;

          IF (l_event_info.event_status_code IN
                ( XLA_EVENTS_PUB_PKG.C_EVENT_UNPROCESSED,
                  XLA_EVENTS_PUB_PKG.C_EVENT_INCOMPLETE) AND
              l_event_info.event_status_code <> l_payment_event_status) THEN

            l_log_msg := 'Before AP_XLA_EVENTS_PKG.UPDATE_EVENT_INFO';
            IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
              FND_LOG.STRING(G_LEVEL_PROCEDURE,
                             G_MODULE_NAME || l_procedure_name,
                             l_log_msg);
            END IF;

            AP_XLA_EVENTS_PKG.UPDATE_EVENT_STATUS
            ( p_event_source_info => l_event_source_info,
              p_event_class_code =>  null,
              p_event_type_code =>   null,
              p_event_date =>        null,
              p_event_status_code => l_payment_event_status,
              p_valuation_method => NULL,
              p_security_context => l_event_security_context,
              p_calling_sequence => l_curr_calling_sequence
            );

            l_log_msg := 'After AP_XLA_EVENTS_PKG.UPDATE_EVENT_INFO';
            IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
              FND_LOG.STRING(G_LEVEL_PROCEDURE,
                             G_MODULE_NAME || l_procedure_name,
                             l_log_msg);
            END IF;
          END IF;
        END IF; -- l_do_updates_flag
      END LOOP;

      EXIT WHEN l_invoice_payments_cur%NOTFOUND;
    END LOOP;
    CLOSE l_invoice_payments_cur;

  EXCEPTION
    WHEN OTHERS THEN
      IF (l_invoice_payments_cur%ISOPEN) THEN
        CLOSE l_invoice_payments_cur;
      END IF;

      l_log_msg := 'Exception calling sequence '
                      ||l_curr_calling_sequence
                      ||' Error: '
                      ||SQLERRM;
      IF (G_LEVEL_EXCEPTION >= G_CURRENT_RUNTIME_LEVEL ) THEN
         FND_LOG.STRING(G_LEVEL_EXCEPTION,
                        G_MODULE_NAME || l_procedure_name,
                        l_log_msg);
      END IF;
      RAISE;
  END;

  l_log_msg := 'End of procedure '||l_procedure_name;
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
    FND_LOG.STRING(G_LEVEL_PROCEDURE,
                   G_MODULE_NAME || l_procedure_name,
                   l_log_msg);
  END IF;

  END IF; --l_event_count<>0

EXCEPTION
  WHEN OTHERS THEN
    IF (SQLCODE <> -20001) THEN
       FND_MESSAGE.SET_NAME('SQLAP','AP_DEBUG');
       FND_MESSAGE.SET_TOKEN('ERROR',SQLERRM);
       FND_MESSAGE.SET_TOKEN('CALLING_SEQUENCE',
                    l_curr_calling_sequence);
       FND_MESSAGE.SET_TOKEN('PARAMETERS',
              'p_invoice_id = '||p_invoice_id);
    END IF;
    APP_EXCEPTION.RAISE_EXCEPTION();
END update_invoice_events_status;


/*============================================================================
 |  PROCEDURE -  UPDATE_PAYMENT_EVENTS_STATUS (PUBLIC)
 |
 |  DESCRIPTION
 |          Update payment event's status
 |
 |  PRAMETERS
 |          p_check_id: Check ID
 |          p_calling_sequence: Debug information
 |
 |  KNOWN ISSUES:
 |
 |  NOTES:
 |
 |  MODIFICATION HISTORY
 |  Date         Author             Description of Change
 |
 *===========================================================================*/
PROCEDURE update_payment_events_status (
                       p_check_id         IN NUMBER,
                       p_calling_sequence IN VARCHAR2)
IS

  TYPE t_invoice_ids IS TABLE OF NUMBER(15) INDEX BY PLS_INTEGER;
  TYPE t_accounting_event_ids IS TABLE OF NUMBER INDEX BY PLS_INTEGER;

  l_legal_entity_id NUMBER(15);
  l_ledger_id NUMBER(15);
  l_org_id NUMBER(15);
  l_event_security_context XLA_EVENTS_PUB_PKG.T_SECURITY;
  l_event_source_info XLA_EVENTS_PUB_PKG.T_EVENT_SOURCE_INFO;
  l_event_info XLA_EVENTS_PUB_PKG.T_EVENT_INFO;
  l_transaction_date  AP_INVOICES_ALL.invoice_date%TYPE;

  l_curr_calling_sequence VARCHAR2(2000);

  -- Logging:
  l_procedure_name CONSTANT VARCHAR2(30) := 'UPDATE_PAYMENT_EVENTS_STATUS';
  l_log_msg        FND_LOG_MESSAGES.MESSAGE_TEXT%TYPE;
  --7011943
    l_count NUMBER;
    l_check_id NUMBER;
    l_pmt_awt_event_source_info XLA_EVENTS_PUB_PKG.T_EVENT_SOURCE_INFO;
    l_event_source_info_temp XLA_EVENTS_PUB_PKG.T_EVENT_SOURCE_INFO;


BEGIN

  l_curr_calling_sequence := p_calling_sequence ||
            ' -> AP_ACCOUNTING_EVENTS_PKG.UPDATE_PAYMENT_EVENTS_STATUS';

  G_CURRENT_RUNTIME_LEVEL := FND_LOG.G_CURRENT_RUNTIME_LEVEL;

  l_log_msg := 'Begin of procedure '|| l_procedure_name;
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
     FND_LOG.STRING(G_LEVEL_PROCEDURE,
                    G_MODULE_NAME || l_procedure_name,
                    l_log_msg);
  END IF;

  DECLARE

    CURSOR l_invoice_distributions_cur IS
      SELECT distinct(AID.invoice_id),
             AID.accounting_event_id
      FROM   ap_invoice_payments AIP,
             ap_invoice_distributions AID
      WHERE AIP.invoice_id = AID.invoice_id
      AND   AIP.check_id = p_check_id
      AND   AID.accounting_event_id IS NOT NULL
      ORDER BY AID.invoice_id;

    l_invoice_ids t_invoice_ids;
    l_accounting_event_ids t_accounting_event_ids;
    l_last_invoice_id NUMBER(15) := NULL;
    l_curr_invoice_id NUMBER(15);
    l_invoice_event_status VARCHAR2(1);
    l_do_updates_flag BOOLEAN;

  BEGIN

    l_log_msg := 'Open cursor l_invoice_distributions_cur';
    IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL ) THEN
      FND_LOG.STRING(G_LEVEL_STATEMENT,
                     G_MODULE_NAME || l_procedure_name,
                     l_log_msg);
    END IF;

    OPEN l_invoice_distributions_cur;
    LOOP

      FETCH l_invoice_distributions_cur
      BULK COLLECT INTO
        l_invoice_ids,
        l_accounting_event_ids
      LIMIT 1000;


      FOR i IN 1 .. l_accounting_event_ids.count LOOP

        l_curr_invoice_id := l_invoice_ids(i);

        IF ( l_last_invoice_id IS NULL OR
             l_curr_invoice_id <> l_last_invoice_id) THEN

          l_last_invoice_id := l_curr_invoice_id;

          l_log_msg := 'Before calling procedure get_invoice_info';
          IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
            FND_LOG.STRING(G_LEVEL_PROCEDURE,
                           G_MODULE_NAME || l_procedure_name,
                           l_log_msg);
          END IF;

          get_invoice_info
          ( p_invoice_id => l_curr_invoice_id,
            p_org_id => l_org_id, -- OUT
            p_legal_entity_id => l_legal_entity_id, -- OUT
            p_ledger_id => l_ledger_id, -- OUT
            p_transaction_date => l_transaction_date, -- OUT
            p_calling_sequence => l_curr_calling_sequence
          );

          l_log_msg := 'After calling procedure get_invoice_info';
          IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
            FND_LOG.STRING(G_LEVEL_PROCEDURE,
                           G_MODULE_NAME || l_procedure_name,
                           l_log_msg);
          END IF;

          l_log_msg := 'Before calling procedure get_event_security_context';
          IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
            FND_LOG.STRING(G_LEVEL_PROCEDURE,
                           G_MODULE_NAME || l_procedure_name,
                           l_log_msg);
          END IF;

          l_event_security_context :=
            get_event_security_context
            ( p_org_id => l_org_id,
              p_calling_sequence => l_curr_calling_sequence
            );

          l_log_msg := 'After calling procedure get_event_security_context';
          IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
            FND_LOG.STRING(G_LEVEL_PROCEDURE,
                           G_MODULE_NAME || l_procedure_name,
                           l_log_msg);
          END IF;

          l_log_msg := 'Before calling get_invoice_event_source_info';
          IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
            FND_LOG.STRING(G_LEVEL_PROCEDURE,
                           G_MODULE_NAME || l_procedure_name,
                           l_log_msg);
          END IF;

          l_event_source_info :=
            get_invoice_event_source_info
            ( p_legal_entity_id => l_legal_entity_id,
              p_ledger_id => l_ledger_id,
              p_invoice_id => l_curr_invoice_id,
              p_calling_sequence => l_curr_calling_sequence
            );
          l_event_source_info_temp := l_event_source_info; --7011943
          l_log_msg := 'After calling procedure get_invoice_info';
          IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
            FND_LOG.STRING(G_LEVEL_PROCEDURE,
                           G_MODULE_NAME || l_procedure_name,
                           l_log_msg);
          END IF;

          IF (is_event_complete
             (p_doc_type => INVOICES_ENTITY,
              p_source_id => l_curr_invoice_id,
              p_calling_sequence => l_curr_calling_sequence) = 'CREATED') THEN

            l_invoice_event_status := XLA_EVENTS_PUB_PKG.C_EVENT_UNPROCESSED;

            l_log_msg := 'Before calling AP_XLA_EVENTS_PKG.EVENT_EXISTS';
            IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
              FND_LOG.STRING(G_LEVEL_PROCEDURE,
                             G_MODULE_NAME || l_procedure_name,
                             l_log_msg);
            END IF;

            l_do_updates_flag :=
              AP_XLA_EVENTS_PKG.EVENT_EXISTS
              ( p_event_source_info => l_event_source_info,
                p_event_class_code => NULL,
                p_event_type_code => NULL,
                p_event_date => NULL,
                p_event_status_code => XLA_EVENTS_PUB_PKG.C_EVENT_INCOMPLETE,
                p_event_number => NULL,
                p_valuation_method => NULL,
                p_security_context => l_event_security_context,
                p_calling_sequence => l_curr_calling_sequence
              );

              l_log_msg := 'After calling AP_XLA_EVENTS_PKG.EVENT_EXISTS';
              IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
                  FND_LOG.STRING(G_LEVEL_PROCEDURE,
                                 G_MODULE_NAME || l_procedure_name,
                                 l_log_msg);
              END IF;
          ELSE

            l_invoice_event_status := XLA_EVENTS_PUB_PKG.C_EVENT_INCOMPLETE;

            l_log_msg := 'Before calling AP_XLA_EVENTS_PKG.EVENT_EXISTS';
            IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
              FND_LOG.STRING(G_LEVEL_PROCEDURE,
                             G_MODULE_NAME || l_procedure_name,
                             l_log_msg);
            END IF;

            l_do_updates_flag :=
              AP_XLA_EVENTS_PKG.EVENT_EXISTS
              ( p_event_source_info => l_event_source_info,
                p_event_class_code => NULL,
                p_event_type_code => NULL,
                p_event_date => NULL,
                p_event_status_code => XLA_EVENTS_PUB_PKG.C_EVENT_UNPROCESSED,
                p_event_number => NULL,
                p_valuation_method => NULL,
                p_security_context => l_event_security_context,
                p_calling_sequence => l_curr_calling_sequence);

            l_log_msg := 'After calling AP_XLA_EVENTS_PKG.EVENT_EXISTS';
            IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
              FND_LOG.STRING(G_LEVEL_PROCEDURE,
                             G_MODULE_NAME || l_procedure_name,
                             l_log_msg);
            END IF;

          END IF;

        END IF;

        IF (l_do_updates_flag) THEN

          l_log_msg := 'Before calling AP_XLA_EVENTS_PKG.GET_EVENT_INFO';
          IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
              FND_LOG.STRING(G_LEVEL_PROCEDURE,
                             G_MODULE_NAME || l_procedure_name,
                             l_log_msg);
          END IF;
          --bug 7011943
          select count(*)
          into l_count
          from ap_invoice_distributions_all
          where accounting_event_id = l_accounting_event_ids(i)
          and invoice_id = l_event_source_info.source_id_int_1
          and awt_invoice_payment_id is not null;

          if (l_count > 0) then
            BEGIN
                select ac.check_id
                into l_check_id
                from ap_invoice_payments_all aip,
                     ap_checks_all ac
                where aip.check_id=ac.check_id
                  and   aip.accounting_event_id = l_accounting_event_ids(i)
                  and   aip.invoice_id=l_event_source_info.source_id_int_1;


             l_pmt_awt_event_source_info:=get_payment_event_source_info(p_legal_entity_id => l_legal_entity_id,
                              p_ledger_id => l_ledger_id,
                              p_check_id => l_check_id,
                              p_calling_sequence => l_curr_calling_sequence);
             l_event_source_info := l_pmt_awt_event_source_info;

            EXCEPTION
             WHEN OTHERS THEN
               NULL;
            END;
          ELSE
            l_event_source_info := l_event_source_info_temp;
          end if;
          --bug 7011943 ends

          l_event_info :=
            AP_XLA_EVENTS_PKG.GET_EVENT_INFO
            ( p_event_source_info => l_event_source_info,
              p_event_id => l_accounting_event_ids(i),
              p_valuation_method => NULL,
              p_security_context => l_event_security_context,
              p_calling_sequence => l_curr_calling_sequence);
          --bug 7011943
          l_event_source_info := l_event_source_info_temp;

          l_log_msg := 'After calling AP_XLA_EVENTS_PKG.GET_EVENT_INFO';
          IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
              FND_LOG.STRING(G_LEVEL_PROCEDURE,
                             G_MODULE_NAME || l_procedure_name,
                             l_log_msg);
          END IF;

          IF ( l_event_info.event_status_code IN
               ( XLA_EVENTS_PUB_PKG.C_EVENT_UNPROCESSED,
                 XLA_EVENTS_PUB_PKG.C_EVENT_INCOMPLETE) AND
               l_event_info.event_status_code <> l_invoice_event_status) THEN

            l_log_msg := 'Before AP_XLA_EVENTS_PKG.UPDATE_EVENT_STATUS';
            IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
              FND_LOG.STRING(G_LEVEL_PROCEDURE,
                             G_MODULE_NAME || l_procedure_name,
                             l_log_msg);
            END IF;

            AP_XLA_EVENTS_PKG.UPDATE_EVENT_STATUS
            ( p_event_source_info => l_event_source_info,
              p_event_class_code =>  null,
              p_event_type_code =>   null,
              p_event_date =>        null,
              p_event_status_code => l_invoice_event_status,
              p_valuation_method =>  NULL,
              p_security_context =>  l_event_security_context,
              p_calling_sequence =>  l_curr_calling_sequence);

            l_log_msg := 'After AP_XLA_EVENTS_PKG.UPDATE_EVENT_STATUS';
            IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
              FND_LOG.STRING(G_LEVEL_PROCEDURE,
                             G_MODULE_NAME || l_procedure_name,
                             l_log_msg);
            END IF;
          END IF;


        END IF; -- l_do_updates_flag

      END LOOP;

      EXIT WHEN l_invoice_distributions_cur%NOTFOUND;
    END LOOP;
    CLOSE l_invoice_distributions_cur;

  EXCEPTION
    WHEN OTHERS THEN
      IF (l_invoice_distributions_cur%ISOPEN) THEN
        CLOSE l_invoice_distributions_cur;
      END IF;

      l_log_msg := 'Exception calling sequence '
                       ||l_curr_calling_sequence
                       ||' Error: '
                       ||SQLERRM;
      IF (G_LEVEL_EXCEPTION >= G_CURRENT_RUNTIME_LEVEL ) THEN
         FND_LOG.STRING(G_LEVEL_EXCEPTION,
                        G_MODULE_NAME || l_procedure_name,
                        l_log_msg);
      END IF;
      RAISE;
  END;

  DECLARE

    CURSOR l_invoice_payments_cur IS
      SELECT distinct(AIP.accounting_event_id)
      FROM   ap_invoice_payments AIP
      WHERE AIP.check_id = p_check_id
      AND   AIP.accounting_event_id IS NOT NULL;

    l_payment_event_status VARCHAR2(1);
    l_do_updates_flag BOOLEAN;
    l_accounting_event_ids t_accounting_event_ids;

  BEGIN

    l_log_msg := 'Before calling procedure get_payment_info';
    IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
       FND_LOG.STRING(G_LEVEL_PROCEDURE,
                      G_MODULE_NAME || l_procedure_name,
                      l_log_msg);
    END IF;

    get_payment_info
    ( p_check_id => p_check_id,
      p_org_id => l_org_id, -- OUT
      p_legal_entity_id => l_legal_entity_id, -- OUT
      p_ledger_id => l_ledger_id, -- OUT
      p_calling_sequence => l_curr_calling_sequence
    );

    l_log_msg := 'After calling procedure get_payment_info';
    IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
       FND_LOG.STRING(G_LEVEL_PROCEDURE,
                      G_MODULE_NAME || l_procedure_name,
                      l_log_msg);
    END IF;

    l_log_msg := 'Before calling procedure get_event_security_context';
    IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
       FND_LOG.STRING(G_LEVEL_PROCEDURE,
                      G_MODULE_NAME || l_procedure_name,
                      l_log_msg);
    END IF;

    l_event_security_context :=
      get_event_security_context
      ( p_org_id => l_org_id,
        p_calling_sequence => l_curr_calling_sequence
      );

    l_log_msg := 'After calling procedure get_event_security_context';
    IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
       FND_LOG.STRING(G_LEVEL_PROCEDURE,
                      G_MODULE_NAME || l_procedure_name,
                      l_log_msg);
    END IF;

    l_log_msg := 'Before calling procedure get_payment_event_source_info';
    IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
       FND_LOG.STRING(G_LEVEL_PROCEDURE,
                      G_MODULE_NAME || l_procedure_name,
                      l_log_msg);
    END IF;

    l_event_source_info :=
      get_payment_event_source_info
      ( p_legal_entity_id => l_legal_entity_id,
        p_ledger_id => l_ledger_id,
        p_check_id => p_check_id,
        p_calling_sequence => l_curr_calling_sequence
      );

    l_log_msg := 'After calling procedure get_payment_event_source_info';
    IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
       FND_LOG.STRING(G_LEVEL_PROCEDURE,
                      G_MODULE_NAME || l_procedure_name,
                      l_log_msg);
    END IF;

    IF (is_event_complete
         (p_doc_type => PAYMENTS_ENTITY,
          p_source_id => p_check_id,
          p_calling_sequence => l_curr_calling_sequence) = 'CREATED') THEN

      l_payment_event_status := XLA_EVENTS_PUB_PKG.C_EVENT_UNPROCESSED;

      l_log_msg := 'Before calling AP_XLA_EVENTS_PKG.EVENT_EXISTS';
      IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
        FND_LOG.STRING(G_LEVEL_PROCEDURE,
                       G_MODULE_NAME || l_procedure_name,
                       l_log_msg);
      END IF;

      l_do_updates_flag :=
        AP_XLA_EVENTS_PKG.EVENT_EXISTS
        ( p_event_source_info => l_event_source_info,
          p_event_class_code => NULL,
          p_event_type_code => NULL,
          p_event_date => NULL,
          p_event_status_code => XLA_EVENTS_PUB_PKG.C_EVENT_INCOMPLETE,
          p_event_number => NULL,
          p_valuation_method => NULL,
          p_security_context => l_event_security_context,
          p_calling_sequence => l_curr_calling_sequence
        );

      l_log_msg := 'After calling procedure AP_XLA_EVENTS_PKG.EVENT_EXISTS';
      IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
        FND_LOG.STRING(G_LEVEL_PROCEDURE,
                       G_MODULE_NAME || l_procedure_name,
                       l_log_msg);
      END IF;

    ELSE

      l_payment_event_status := XLA_EVENTS_PUB_PKG.C_EVENT_INCOMPLETE;

      l_log_msg := 'Before calling procedure AP_XLA_EVENTS_PKG.EVENT_EXISTS';
      IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
        FND_LOG.STRING(G_LEVEL_PROCEDURE,
                       G_MODULE_NAME || l_procedure_name,
                       l_log_msg);
      END IF;

      l_do_updates_flag :=
        AP_XLA_EVENTS_PKG.EVENT_EXISTS
        ( p_event_source_info => l_event_source_info,
          p_event_class_code => NULL,
          p_event_type_code => NULL,
          p_event_date => NULL,
          p_event_status_code => XLA_EVENTS_PUB_PKG.C_EVENT_UNPROCESSED,
          p_event_number => NULL,
          p_valuation_method => NULL,
          p_security_context => l_event_security_context,
          p_calling_sequence => l_curr_calling_sequence
        );

       l_log_msg := 'After calling procedure AP_XLA_EVENTS_PKG.EVENT_EXISTS';
       IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
         FND_LOG.STRING(G_LEVEL_PROCEDURE,
                        G_MODULE_NAME || l_procedure_name,
                        l_log_msg);
       END IF;

    END IF;

    IF (l_do_updates_flag) THEN

      OPEN l_invoice_payments_cur;
      LOOP

        l_log_msg := 'Fetch data from cursor l_invoice_payments_cur';
        IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL ) THEN
          FND_LOG.STRING(G_LEVEL_STATEMENT,
                         G_MODULE_NAME || l_procedure_name,
                         l_log_msg);
        END IF;

        FETCH l_invoice_payments_cur
        BULK COLLECT INTO
          l_accounting_event_ids
        LIMIT 1000;

        l_log_msg := 'Begin loop for l_accounting_event_ids';
        IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL ) THEN
           FND_LOG.STRING(G_LEVEL_STATEMENT,
                          G_MODULE_NAME || l_procedure_name,
                          l_log_msg);
        END IF;

        FOR i IN 1 ..l_accounting_event_ids.count LOOP

          l_log_msg := 'Before calling AP_XLA_EVENTS_PKG.GET_EVENT_INFO';
          IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
             FND_LOG.STRING(G_LEVEL_PROCEDURE,
                            G_MODULE_NAME || l_procedure_name,
                            l_log_msg);
          END IF;

          l_event_info :=
            AP_XLA_EVENTS_PKG.GET_EVENT_INFO
            ( p_event_source_info => l_event_source_info,
              p_event_id => l_accounting_event_ids(i),
              p_valuation_method => NULL,
              p_security_context => l_event_security_context,
              p_calling_sequence => l_curr_calling_sequence
            );

          l_log_msg := 'After calling  AP_XLA_EVENTS_PKG.GET_EVENT_INFO';
          IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
             FND_LOG.STRING(G_LEVEL_PROCEDURE,
                            G_MODULE_NAME || l_procedure_name,
                            l_log_msg);
          END IF;

          IF ( l_event_info.event_status_code IN
                ( XLA_EVENTS_PUB_PKG.C_EVENT_UNPROCESSED,
                  XLA_EVENTS_PUB_PKG.C_EVENT_INCOMPLETE) AND
            l_event_info.event_status_code <> l_payment_event_status) THEN

            l_log_msg := 'Before AP_XLA_EVENTS_PKG.UPDATE_EVENT_STATUS';
            IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
              FND_LOG.STRING(G_LEVEL_PROCEDURE,
                             G_MODULE_NAME || l_procedure_name,
                             l_log_msg);
            END IF;

            AP_XLA_EVENTS_PKG.UPDATE_EVENT_STATUS
            ( p_event_source_info => l_event_source_info,
              p_event_class_code =>
                get_event_class
                (
                  p_event_type => l_event_info.event_type_code,
                  p_calling_sequence => l_curr_calling_sequence
                ),
              p_event_type_code => l_event_info.event_type_code,
              p_event_date => l_event_info.event_date,
              p_event_status_code => l_payment_event_status,
              p_valuation_method => NULL,
              p_security_context => l_event_security_context,
              p_calling_sequence => l_curr_calling_sequence
            );

            l_log_msg := 'After AP_XLA_EVENTS_PKG.UPDATE_EVENT_STATUS';
            IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
              FND_LOG.STRING(G_LEVEL_PROCEDURE,
                             G_MODULE_NAME || l_procedure_name,
                             l_log_msg);
            END IF;

          END IF;

        END LOOP;
        EXIT WHEN l_invoice_payments_cur%NOTFOUND;
      END LOOP;

      CLOSE l_invoice_payments_cur;

      l_log_msg := 'End loop for l_accounting_event_ids';
      IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL ) THEN
        FND_LOG.STRING(G_LEVEL_STATEMENT,
                       G_MODULE_NAME || l_procedure_name,
                       l_log_msg);
      END IF;
    END IF; -- l_do_updates_flag

  EXCEPTION
    WHEN OTHERS THEN
      IF (l_invoice_payments_cur%ISOPEN) THEN
        CLOSE l_invoice_payments_cur;
      END IF;
      l_log_msg := 'Exception calling sequence '
                      ||l_curr_calling_sequence
                      ||' Error: '
                      ||SQLERRM;
      IF (G_LEVEL_EXCEPTION >= G_CURRENT_RUNTIME_LEVEL ) THEN
         FND_LOG.STRING(G_LEVEL_EXCEPTION,
                        G_MODULE_NAME || l_procedure_name,
                        l_log_msg);
      END IF;
      RAISE;
  END;

  l_log_msg := 'End of procedure '||l_procedure_name;
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
     FND_LOG.STRING(G_LEVEL_PROCEDURE,
                    G_MODULE_NAME || l_procedure_name,
                    l_log_msg);
  END IF;

EXCEPTION
  WHEN OTHERS THEN
    IF (SQLCODE <> -20001) THEN
       FND_MESSAGE.SET_NAME('SQLAP','AP_DEBUG');
       FND_MESSAGE.SET_TOKEN('ERROR',SQLERRM);
       FND_MESSAGE.SET_TOKEN('CALLING_SEQUENCE',
                    l_curr_calling_sequence);
       FND_MESSAGE.SET_TOKEN('PARAMETERS',
               'p_check_id = '||p_check_id);
       FND_MESSAGE.SET_TOKEN('DEBUG_INFO',l_log_msg);
    END IF;
    APP_EXCEPTION.RAISE_EXCEPTION();
END update_payment_events_status;

/*============================================================================
 |  PROCEDURE -  update_pmt_batch_event_status
 |
 |  DESCRIPTION
 |          Update accounting events for 'PAYMENT BATCH' type
 |
 |  PRAMETERS
 |          p_completed_pmts_group_id: payment request name
 |          p_accounting_date:Event Date
 |          p_org_id :  org id for each small batch inside the payment request
 |          p_set_of_books_id:  the ledger id for this ou
 |          p_calling_sequence: Debug information
 |
 |  KNOWN ISSUES:
 |
 |  NOTES:
 |
 |  MODIFICATION HISTORY
 |  Date         Author             Description of Change
 |
 *===========================================================================*/
PROCEDURE update_pmt_batch_event_status(
              p_checkrun_name              IN    VARCHAR2,
              p_completed_pmts_group_id    IN    NUMBER,
              p_org_id                     IN    NUMBER,
              p_calling_sequence           IN    VARCHAR2)
IS

  l_record_count          NUMBER;
  l_curr_calling_sequence VARCHAR2(2000);

  -- Logging:
  --4700081
  l_procedure_name CONSTANT VARCHAR2(40) := 'update_payment_batch_event_status';
  l_log_msg        FND_LOG_MESSAGES.MESSAGE_TEXT%TYPE;

BEGIN

  l_curr_calling_sequence := p_calling_sequence ||
          ' -> AP_ACCOUNTING_EVENTS_PKG.update_payment_batch_event_status';

  G_CURRENT_RUNTIME_LEVEL := FND_LOG.G_CURRENT_RUNTIME_LEVEL;

  l_log_msg := 'Begin of procedure'||l_procedure_name;
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
      FND_LOG.STRING(G_LEVEL_PROCEDURE,
                     G_MODULE_NAME || l_procedure_name,
                     l_log_msg);
  END IF;

  -- Bug 5512197. Changed the reference of XL_EVENTS_GT to XLA_EVENTS_INT_GT
  SELECT COUNT(*)
    INTO l_record_count
    FROM XLA_EVENTS_INT_GT XEG
   WHERE XEG.application_id = 200
     AND XEG.entity_code = PAYMENTS_ENTITY
     AND XEG.event_type_code = PAYMENT_CREATED_TYPE;

  IF ( l_record_count <> 0 ) THEN

    l_log_msg := 'update the gt table set event status = no action';
    IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL ) THEN
      FND_LOG.STRING(G_LEVEL_STATEMENT,
                     G_MODULE_NAME || l_procedure_name,
                     l_log_msg);
    END IF;

    UPDATE  XLA_EVENTS_INT_GT XEG
           SET  event_status_code = XLA_EVENTS_PUB_PKG.C_EVENT_NOACTION
        WHERE   XEG.application_id = 200
          AND   XEG.source_id_int_1  IN
                ( SELECT AC.check_id
                    FROM AP_CHECKS_ALL AC
                   WHERE AC.checkrun_name = p_checkrun_name
                     and AC.completed_pmts_group_id = p_completed_pmts_group_id
                     and AC.org_id = p_org_id );

        l_log_msg := 'update the gt table in success and call the api';
    IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL ) THEN
      FND_LOG.STRING(G_LEVEL_STATEMENT,
                     G_MODULE_NAME || l_procedure_name,
                     l_log_msg);
    END IF;

    XLA_EVENTS_PUB_PKG.update_bulk_event_statuses(p_application_id => '200');

    l_log_msg := 'after calling xla update status api';
        IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL ) THEN
      FND_LOG.STRING(G_LEVEL_STATEMENT,
                     G_MODULE_NAME || l_procedure_name,
                     l_log_msg);
    END IF;

  ELSE

    l_log_msg := 'XLA_EVENTS_INT_GT TABLE Has no records';
    IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL ) THEN
       FND_LOG.STRING(G_LEVEL_STATEMENT,
                      G_MODULE_NAME || l_procedure_name,
                      l_log_msg);
    END IF;
    APP_EXCEPTION.RAISE_EXCEPTION();
  END IF;

  l_log_msg := 'End of the procedure';
  IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL ) THEN
    FND_LOG.STRING(G_LEVEL_STATEMENT,
                   G_MODULE_NAME || l_procedure_name,
                   l_log_msg);
  END IF;

EXCEPTION
  WHEN OTHERS THEN

    IF (SQLCODE <> -20001) THEN
       FND_MESSAGE.SET_NAME('SQLAP','AP_DEBUG');
       FND_MESSAGE.SET_TOKEN('ERROR',SQLERRM);
       FND_MESSAGE.SET_TOKEN('CALLING_SEQUENCE',
                    l_curr_calling_sequence);
       FND_MESSAGE.SET_TOKEN('PARAMETERS',
               'p_checkrun_name = '||p_checkrun_name
           ||', p_completed_pmts_group_id = '||p_completed_pmts_group_id
           ||', p_org_id = '||p_org_id
           ||', p_checkrun_name = '||p_checkrun_name);
       FND_MESSAGE.SET_TOKEN('DEBUG_INFO',l_log_msg);
    END IF;
    fnd_file.put_line(FND_FILE.LOG,l_log_msg||'|'||SQLERRM||' - '||systimestamp); --Bug 14279929
    APP_EXCEPTION.RAISE_EXCEPTION();
END update_pmt_batch_event_status;


/*============================================================================
 |  PROCEDURE -  CREATE_PAYMENT_BATCH_EVENTS (PRIVATE)
 |
 |  DESCRIPTION
 |          Create accounting events for 'PAYMENT BATCH' type
 |
 |  PRAMETERS
 |          p_checkrun_name: payment request name
 |          p_accounting_date:Event Date
 |          p_org_id :  org id for each small batch inside the payment request
 |          p_set_of_books_id:  the ledger id for this ou
 |          p_calling_sequence: Debug information
 |
 |  KNOWN ISSUES:
 |
 |  NOTES:
 |
 |  MODIFICATION HISTORY
 |  Date         Author             Description of Change
 |
 *===========================================================================*/
PROCEDURE create_payment_batch_events(
              p_checkrun_name              IN    VARCHAR2,
              p_completed_pmts_group_id    IN    NUMBER,
              p_accounting_date            IN    DATE,
              p_org_id                     IN    NUMBER,
              p_set_of_books_id            IN    NUMBER,
              p_calling_sequence           IN    VARCHAR2)
IS

  l_event_type VARCHAR2(30) := PAYMENT_CREATED_TYPE;
  l_event_status VARCHAR2(1) := XLA_EVENTS_PUB_PKG.C_EVENT_UNPROCESSED;
  l_curr_calling_sequence VARCHAR2(2000);
  l_count NUMBER  := 0;

  -- Logging:
  l_procedure_name CONSTANT VARCHAR2(30) := 'CREATE_PAYMENT_BATCH_EVENTS';
  l_log_msg        FND_LOG_MESSAGES.MESSAGE_TEXT%TYPE;

  --bug 8358552 starts
  l_dummy number;
  --bug 8358552 ends

BEGIN

  l_curr_calling_sequence := p_calling_sequence ||
          ' -> AP_ACCOUNTING_EVENTS_PKG.CREATE_PAYMENT_BATCH_EVENTS';

  G_CURRENT_RUNTIME_LEVEL := FND_LOG.G_CURRENT_RUNTIME_LEVEL;

  l_log_msg := 'Begin of procedure'||l_procedure_name;
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
      FND_LOG.STRING(G_LEVEL_PROCEDURE,
                     G_MODULE_NAME || l_procedure_name,
                     l_log_msg);

  END IF;

  l_log_msg := 'p_set_of_books_id = '|| p_set_of_books_id ||
               'p_org_id = '|| p_org_id ||
               'p_completed_pmts_group_id = '|| p_completed_pmts_group_id ||
               'p_checkrun_name = '|| p_checkrun_name;
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
      FND_LOG.STRING(G_LEVEL_PROCEDURE,
                     G_MODULE_NAME || l_procedure_name,
                     l_log_msg);

  END IF;

  BEGIN

    l_log_msg := 'Begin to clean up the xla_event_gt_table';
    IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
      FND_LOG.STRING(G_LEVEL_PROCEDURE,
                     G_MODULE_NAME || l_procedure_name,
                     l_log_msg);
    END IF;

    DELETE XLA_EVENTS_INT_GT XEG
     WHERE application_id = 200;

    l_log_msg := 'After clean up the xla_event_gt_table';
    IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
      FND_LOG.STRING(G_LEVEL_PROCEDURE,
                     G_MODULE_NAME || l_procedure_name,
                     l_log_msg);
    END IF;

  END;

  BEGIN

        l_log_msg := 'create event for group_id = ' || p_completed_pmts_group_id ;
    IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
      FND_LOG.STRING(G_LEVEL_PROCEDURE,
                     G_MODULE_NAME || l_procedure_name,
                     l_log_msg);
    END IF;

    INSERT INTO  XLA_EVENTS_INT_GT (
        application_id,
        ledger_id,
        legal_entity_id,
        entity_code,
        transaction_number,
        source_id_int_1,
        transaction_date,
        security_id_int_1,
        event_type_code,
        event_date,
        event_status_code )
    (SELECT
           '200',
           p_set_of_books_id,
           ac.legal_entity_id,
           PAYMENTS_ENTITY,
           ac.check_number,
           ac.check_id,
           ac.check_date,
           ac.org_id,
           l_event_type,
           p_accounting_date,
           --bug 8358552 starts
           --l_event_status
           --Bug 9359625 Only check for null xrate fields on foreign curr.
           decode(ac.currency_code, asp.base_currency_code, XLA_EVENTS_PUB_PKG.C_EVENT_UNPROCESSED,
           decode(ac.exchange_date, NULL, XLA_EVENTS_PUB_PKG.C_EVENT_INCOMPLETE,
                decode(ac.exchange_rate_type, NULL, XLA_EVENTS_PUB_PKG.C_EVENT_INCOMPLETE,
                decode(ac.exchange_rate, NULL, XLA_EVENTS_PUB_PKG.C_EVENT_INCOMPLETE,
                XLA_EVENTS_PUB_PKG.C_EVENT_UNPROCESSED))))
           --bug 8358552 ends
      FROM ap_checks_all ac
      , ap_system_parameters_all asp
     WHERE ac.checkrun_name = p_checkrun_name
           AND ac.completed_pmts_group_id = p_completed_pmts_group_id
           AND ac.org_id = p_org_id
           AND ac.org_id = asp.org_id);


    IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL ) THEN

       SELECT COUNT(*)
       INTO l_count
       FROM XLA_EVENTS_INT_GT;

       l_log_msg := 'event gt table has record number = '|| l_count;
       FND_LOG.STRING(G_LEVEL_STATEMENT,
                     G_MODULE_NAME || l_procedure_name,
                     l_log_msg);
    END IF;

  EXCEPTION
  WHEN OTHERS THEN
       l_log_msg := 'unknown exception '||SQLERRM||' when try to insert into xla gt table';
       IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL ) THEN
       FND_LOG.STRING(G_LEVEL_STATEMENT,
                       G_MODULE_NAME || l_procedure_name,
                       l_log_msg);
       END IF;
     fnd_file.put_line(FND_FILE.LOG,l_log_msg||'|'||SQLERRM||' - '||systimestamp); --Bug 14279929
     APP_EXCEPTION.RAISE_EXCEPTION(); --Bug 14279929
  END;

  l_log_msg := 'after insert into xla_event_gt table';
  IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL ) THEN
    FND_LOG.STRING(G_LEVEL_STATEMENT,
                     G_MODULE_NAME || l_procedure_name,
                     l_log_msg);
  END IF;

  BEGIN
    XLA_EVENTS_PUB_PKG.create_bulk_events
       (p_source_application_id => NULL
       ,p_application_id        => '200'
       ,p_legal_entity_id       => NULL
       ,p_ledger_id             => p_set_of_books_id
       ,p_entity_type_code      => PAYMENTS_ENTITY );
  END;

  l_log_msg := 'After calling the sla create_bulk_events';
  IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL ) THEN
    FND_LOG.STRING(G_LEVEL_STATEMENT,
                     G_MODULE_NAME || l_procedure_name,
                     l_log_msg);
  END IF;

  l_log_msg := 'Right before insert into ap_payment_history table';
  IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL ) THEN
    FND_LOG.STRING(G_LEVEL_STATEMENT,
                     G_MODULE_NAME || l_procedure_name,
                     l_log_msg);
  END IF;

  BEGIN

    INSERT INTO ap_payment_history_all
    ( payment_history_id,
      check_id,
      accounting_date,
      transaction_type,
      posted_flag,
      trx_bank_amount,
      errors_bank_amount,
      charges_bank_amount,
      bank_currency_code,
      bank_to_base_xrate_type,
      bank_to_base_xrate_date,
      bank_to_base_xrate,
      trx_pmt_amount,
      errors_pmt_amount,
      charges_pmt_amount,
      pmt_currency_code,
      pmt_to_base_xrate_type,
      pmt_to_base_xrate_date,
      pmt_to_base_xrate,
      trx_base_amount,
      errors_base_amount,
      charges_base_amount,
      matched_flag,
      rev_pmt_hist_id,
      creation_date,
      created_by,
      last_update_date,
      last_updated_by,
      last_update_login,
      program_update_date,
      program_application_id,
      program_id,
      accounting_event_id,
      request_id,
      org_id,
      related_event_id ) -- Bug 5015973  -- Bug 5658623: Adding hint
    ( select  /*+ Leading(xeg) index(ac ap_checks_u1) */
          ap_payment_history_s.NEXTVAL, -- payment_history_id
          ac.check_id, -- check_id
          trunc(p_accounting_date), -- accounting_date  bug6602676
          l_event_type, -- transaction_type
          'N', -- posted_flag
          NULL, -- trx_bank_amount
          NULL, -- errors_bank_amount
          NULL, -- charges_bank_amount
          NULL, -- bank_currency_code
          NULL, -- bank_to_base_xrate_type
          NULL, -- bank_to_base_xrate_date
          NULL, -- bank_to_base_xrate
          ac.amount, -- trx_pmt_amount
          NULL, -- errors_pmt_amount
          NULL, -- charges_pmt_amount
          ac.currency_code, -- pmt_currency_code
          ac.exchange_rate_type, -- pmt_to_base_xrate_type
          ac.exchange_date, -- pmt_to_base_xrate_date
          ac.exchange_rate, -- pmt_to_base_xrate
          NVL(ac.base_amount, ac.amount), -- trx_base_amount
          NULL, -- errors_base_amount
          NULL, -- charges_base_amount
          NULL, -- matched_flag
          NULL, -- rev_pmt_hist_id
          SYSDATE, -- creation_date
          FND_GLOBAL.user_id, -- created_by
          SYSDATE, -- last_update_date
          FND_GLOBAL.user_id, -- last_updated_by
          FND_GLOBAL.login_id, -- last_update_login
          SYSDATE, -- program_update_date
          NULL, -- program_application_id
          NULL, -- program_id
          XEG.event_id, -- accounting_event_id
          NULL, -- request_id
          ac.org_id,  -- org_id
          XEG.event_id  -- related_event_id
     from ap_checks_all ac,
          xla_events_int_gt  xeg
     where ac.completed_pmts_group_id = p_completed_pmts_group_id
       and ac.org_id = p_org_id
       and xeg.source_id_int_1 = ac.check_id);

     IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL ) THEN

        SELECT COUNT(*)
          INTO l_count
          FROM ap_payment_history_all aph,
               ap_checks_all ac
         WHERE aph.check_id = ac.check_id
           AND ac.completed_pmts_group_id = p_completed_pmts_group_id
           AND ac.org_id = p_org_id;

        l_log_msg := 'payment history has record number = '|| l_count;
        FND_LOG.STRING(G_LEVEL_STATEMENT,
                       G_MODULE_NAME || l_procedure_name,
                       l_log_msg);
     END IF;

     IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL ) THEN

         SELECT accounting_event_id
           INTO l_count
           FROM ap_payment_history_all aph,
                ap_checks_all ac
          WHERE aph.check_id = ac.check_id
            AND ac.completed_pmts_group_id = p_completed_pmts_group_id
            AND ac.org_id = p_org_id
            AND rownum = 1;

         l_log_msg := ' one account event_id created = '|| l_count;
         FND_LOG.STRING(G_LEVEL_STATEMENT,
                        G_MODULE_NAME || l_procedure_name,
                        l_log_msg);
     END IF;

  EXCEPTION
  WHEN OTHERS THEN
    l_log_msg := 'unknown exception '||SQLERRM||' when try to insert ap_payment_history table';
    IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL ) THEN
      FND_LOG.STRING(G_LEVEL_STATEMENT,
                     G_MODULE_NAME || l_procedure_name,
                     l_log_msg);
    END IF;
    fnd_file.put_line(FND_FILE.LOG,l_log_msg||'|'||SQLERRM||' - '||systimestamp); --Bug 14279929
    APP_EXCEPTION.RAISE_EXCEPTION(); --14279929

  END;

  l_log_msg := 'After insert ap_payment_history_table and end the procedure';
  IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL ) THEN
    FND_LOG.STRING(G_LEVEL_STATEMENT,
                   G_MODULE_NAME || l_procedure_name,
                   l_log_msg);
  END IF;


EXCEPTION
  WHEN OTHERS THEN

    IF (SQLCODE <> -20001) THEN
       FND_MESSAGE.SET_NAME('SQLAP','AP_DEBUG');
       FND_MESSAGE.SET_TOKEN('ERROR',SQLERRM);
       FND_MESSAGE.SET_TOKEN('CALLING_SEQUENCE',
                    l_curr_calling_sequence);
       FND_MESSAGE.SET_TOKEN('PARAMETERS',
               'p_completed_pmts_group_id = '||p_completed_pmts_group_id
           ||', p_set_of_books_id = '||p_set_of_books_id
           ||', p_org_id = '||p_set_of_books_id
           ||', p_accounting_date = '||p_accounting_date);
       FND_MESSAGE.SET_TOKEN('DEBUG_INFO',l_log_msg);
    END IF;
    fnd_file.put_line(FND_FILE.LOG,l_log_msg||'|'||SQLERRM||' - '||systimestamp); --Bug 14279929
    APP_EXCEPTION.RAISE_EXCEPTION();
END create_payment_batch_events;


/*============================================================================
 |  PROCEDURE -  UPDATE_AWT_INT_DISTS (PUBLIC)
 |
 |  DESCRIPTION
 |     This procedure is called by the payment event creation procs (EXCEPT for
 |     pmt batches). Stamp the event_id on all awt invoice distributions and
 |     on interest invoice distributions that were created by the payment for
 |     which this event is being created.It will also stamp the Payment
 |     Clearing Accounting event_id on the records in AIP when
 |     when_to_acct_pmt = clrg only
 |
 |  PRAMETERS
 |          p_event_type: Event type
 |          p_check_id: Check ID
 |          p_event_id: Event ID
 |          p_calling_sequence: Debug information
 |
 |  KNOWN ISSUES:
 |
 |  NOTES:
 |
 |  MODIFICATION HISTORY
 |  Date         Author             Description of Change
 |
 *===========================================================================*/

PROCEDURE update_awt_int_dists (
                p_event_type       IN    VARCHAR2,
                p_check_id         IN    NUMBER,
                p_event_id         IN    NUMBER,
                p_calling_sequence IN    VARCHAR2)
IS

  l_curr_calling_sequence VARCHAR2(2000);

   -- Logging:
  l_procedure_name CONSTANT VARCHAR2(30) := 'UPDATE_AWT_INT_DISTS';
  l_log_msg        FND_LOG_MESSAGES.MESSAGE_TEXT%TYPE;

BEGIN

  l_curr_calling_sequence :=
    p_calling_sequence || ' -> AP_ACCOUNTING_EVENTS_PKG.UPDATE_AWT_INT_DISTS';

  G_CURRENT_RUNTIME_LEVEL := FND_LOG.G_CURRENT_RUNTIME_LEVEL;

  l_log_msg := 'Begin of procedure '||l_procedure_name;
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
    FND_LOG.STRING(G_LEVEL_PROCEDURE,
                   G_MODULE_NAME || l_procedure_name,
                   l_log_msg);
  END IF;

  IF ( p_event_type IN (PAYMENT_CREATED_TYPE,
                        PAYMENT_ADJUSTED_TYPE,
                        PAYMENT_CANCELLED_TYPE )) THEN

    --Stamp the event_id on all awt invoice distributions that were
    --created by the payment for which this event is being created.

    l_log_msg := 'Update ap_invoice_distribution, set accounting_event_id ='
                   ||to_char(p_event_id);
    IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL ) THEN
       FND_LOG.STRING(G_LEVEL_STATEMENT,
                      G_MODULE_NAME || l_procedure_name,
                      l_log_msg);
    END IF;

    UPDATE ap_invoice_distributions D
    SET    D.accounting_event_id = p_event_id
    WHERE  D.accounting_event_id IS NULL
    AND    D.awt_invoice_payment_id IN
             (SELECT AIP1.invoice_payment_id
              FROM   ap_invoice_payments AIP1
              WHERE  AIP1.accounting_event_id = p_event_id
              AND    AIP1.check_id = p_check_id);

    --Stamp the event_id on all invoice dists that belong to interest
    --invoices created by the payment for which this event is created.

    l_log_msg := 'Update ap_invoice_distribution, set accounting_event_id =
                    '||to_char(p_event_id);
    IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL ) THEN
       FND_LOG.STRING(G_LEVEL_STATEMENT,
                      G_MODULE_NAME || l_procedure_name,
                      l_log_msg);
    END IF;

    UPDATE ap_invoice_distributions_all D
    SET    D.accounting_event_id = p_event_id
    WHERE  D.accounting_event_id IS NULL
    AND    D.invoice_id IN
             (SELECT AI.invoice_id
              FROM   ap_invoice_payments_all AIP2,
                     ap_invoices_all AI
              WHERE  AI.invoice_id = AIP2.invoice_id
              AND    AIP2.check_id = p_check_id
              AND    AIP2.accounting_event_id = p_event_id
              AND    AI.invoice_type_lookup_code = 'INTEREST');

  END IF; --event is Payment, Payment Adjustment or Payment Cancellation

  l_log_msg := 'End of procedure '||l_procedure_name;
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
     FND_LOG.STRING(G_LEVEL_PROCEDURE,
                    G_MODULE_NAME || l_procedure_name,
                    l_log_msg);
  END IF;

EXCEPTION
  WHEN OTHERS THEN
    IF (SQLCODE <> -20001) THEN
       FND_MESSAGE.SET_NAME('SQLAP','AP_DEBUG');
       FND_MESSAGE.SET_TOKEN('ERROR',SQLERRM);
       FND_MESSAGE.SET_TOKEN('CALLING_SEQUENCE',
                    l_curr_calling_sequence);
       FND_MESSAGE.SET_TOKEN('PARAMETERS',
               'p_event_type = '||p_event_type
           ||', p_check_id = '||p_check_id
           ||', p_event_id = '||p_event_id);
       FND_MESSAGE.SET_TOKEN('DEBUG_INFO',l_log_msg);
    END IF;
    APP_EXCEPTION.RAISE_EXCEPTION();
END update_awt_int_dists;


/*============================================================================
 |  PROCEDURE -  BATCH_UPDATE_PAYMENT_INFO (PUBLIC)
 |
 |  DESCRIPTION
 |      This procedure is called from appbip.lpc only when when_to_acct_pmt=
 |      ALWAYS. It is used to update awt and int inv dists with accoutning
 |      event id.
 |
 |  PRAMETERS
 |           p_checkrun_name: Payment batch's name
 |           p_calling_sequence: Debug information
 |
 |  KNOWN ISSUES:
 |
 |  NOTES:
 |
 |  MODIFICATION HISTORY
 |  Date         Author             Description of Change
 |
 *===========================================================================*/
PROCEDURE batch_update_payment_info(
              p_checkrun_name              IN VARCHAR2,
              p_completed_pmts_group_id    IN NUMBER,
              p_org_id                     IN NUMBER,
              p_calling_sequence           IN VARCHAR2 DEFAULT NULL)
IS

  CURSOR get_payment_info IS
    SELECT DISTINCT AC.check_id, /* Added the distinct for bug#8368922 */
           AIP.accounting_event_id  -- Bug3343314
         , AIP.invoice_id   /* Added the distinct for bug#8438184 */
    FROM   ap_checks AC,
           ap_invoice_payments AIP  -- Bug3343314
    WHERE  AC.check_id = AIP.check_id -- Bug3343314
    AND    AC.checkrun_name = p_checkrun_name
    AND    AC.status_lookup_code NOT IN ('OVERFLOW', 'SET UP')
    AND    AC.completed_pmts_group_id = p_completed_pmts_group_id
    AND    AC.org_id = p_org_id
    AND    AIP.posted_flag <> 'Y'; -- Bug3343314

  TYPE t_check_ids IS TABLE OF NUMBER(15) INDEX BY PLS_INTEGER;
  l_check_ids t_check_ids;

  TYPE t_accounting_event_ids IS TABLE OF NUMBER INDEX BY PLS_INTEGER;
  l_accounting_event_ids t_accounting_event_ids;

  /* Added for bug#8438184 Start */
  TYPE t_invoice_ids IS TABLE OF NUMBER(15) INDEX BY PLS_INTEGER;
  l_invoice_ids t_invoice_ids;

  l_auto_calculate_interest_flag ap_system_parameters_all.auto_calculate_interest_flag%TYPE;
  /* Added for bug#8438184 End */

  l_curr_calling_sequence VARCHAR2(2000);

  -- Logging:
  l_procedure_name CONSTANT VARCHAR2(30) := 'BATCH_UPDATE_PAYMENT_INFO';
  l_log_msg        FND_LOG_MESSAGES.MESSAGE_TEXT%TYPE;

BEGIN

  l_curr_calling_sequence := p_calling_sequence ||
           ' -> AP_ACCOUNTING_EVENTS_PKG.BATCH_UPDATE_PAYMENT_INFO';

  G_CURRENT_RUNTIME_LEVEL := FND_LOG.G_CURRENT_RUNTIME_LEVEL;

  l_log_msg := 'Begin of procedure '||l_procedure_name;
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
     FND_LOG.STRING(G_LEVEL_PROCEDURE,
                    G_MODULE_NAME || l_procedure_name,
                    l_log_msg);
  END IF;

  OPEN get_payment_info;
  LOOP

    l_log_msg := 'Get data from cursor get_payment_info';
    IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL ) THEN
       FND_LOG.STRING(G_LEVEL_STATEMENT,
                      G_MODULE_NAME || l_procedure_name,
                      l_log_msg);
    END IF;

    FETCH get_payment_info
    BULK COLLECT INTO
      l_check_ids,
      l_accounting_event_ids,
      l_invoice_ids /* Added for bug#8438184 */
    LIMIT 1000;

    /*
    || -------------------------------------------------------------------
    || Step: 1 We will stamp the Accounting_Event_ID on
    || for AWT Invoice Distributions table that were created by the
    || payment for which this event is being created
    || -------------------------------------------------------------------
    */

    FORALL i IN 1 .. l_check_ids.count

    UPDATE ap_invoice_distributions AID
    SET    AID.accounting_event_id = l_accounting_event_ids(i)
    WHERE  AID.accounting_event_id IS NULL
    AND    AID.awt_invoice_payment_id IN
            (SELECT AIP.invoice_payment_id
             FROM ap_invoice_payments AIP
             WHERE AIP.check_id = l_check_ids(i) AND
             AIP.accounting_event_id = l_accounting_event_ids(i)
             AND AIP.invoice_id = l_invoice_ids(i)); /* Added for bug#8438184 */

    /*
    || -------------------------------------------------------------------
    || Step: 2 We will stamp the Accounting_Event_ID (of the Payment Event)
    || on all Invoice Distributions that belong to interest invoices
    || created by the payment
    || -------------------------------------------------------------------
    */

    /* Added for bug#8438184 Start */
    BEGIN
      SELECT auto_calculate_interest_flag
        INTO l_auto_calculate_interest_flag
        FROM ap_system_parameters_all
       WHERE org_id = p_org_id;
    EXCEPTION
      WHEN OTHERS THEN
      l_auto_calculate_interest_flag := 'Y' ;
    END;

    IF l_auto_calculate_interest_flag = 'Y'
    THEN
    /* Added for bug#8438184 Start */

    FORALL i IN 1 .. l_check_ids.count

    UPDATE ap_invoice_distributions_all AID
    SET AID.accounting_event_id = l_accounting_event_ids(i)
    WHERE AID.accounting_event_id IS NULL
    AND AID.invoice_id IN
                  ( SELECT AI.invoice_id
                    FROM  ap_invoice_payments_all AIP,
                          ap_invoices_all AI
                    WHERE AIP.invoice_id = AI.invoice_id
                    AND   AIP.accounting_event_id = l_accounting_event_ids(i)
                    AND   AIP.check_id = l_check_ids(i)
                    AND   AI.invoice_id= l_invoice_ids(i)  /* Added for bug#8438184 */
                    AND   AI.invoice_type_lookup_code = 'INTEREST');

    END IF; /* Added for bug#8438184 */

    EXIT WHEN get_payment_info%NOTFOUND;
  END LOOP;
  CLOSE get_payment_info;

  l_log_msg := 'End of procedure '||l_procedure_name;
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
     FND_LOG.STRING(G_LEVEL_PROCEDURE,
                    G_MODULE_NAME || l_procedure_name,
                    l_log_msg);
  END IF;

EXCEPTION
  WHEN OTHERS THEN
    IF (get_payment_info%ISOPEN) THEN
      CLOSE get_payment_info;
    END IF;

    IF (SQLCODE <> -20001) THEN
       FND_MESSAGE.SET_NAME('SQLAP','AP_DEBUG');
       FND_MESSAGE.SET_TOKEN('ERROR',SQLERRM);
       FND_MESSAGE.SET_TOKEN('CALLING_SEQUENCE',
                    l_curr_calling_sequence);
       FND_MESSAGE.SET_TOKEN('PARAMETERS',
               ' p_checkrun_name = '|| p_checkrun_name);
       FND_MESSAGE.SET_TOKEN('DEBUG_INFO',l_log_msg);
    END IF;
    fnd_file.put_line(FND_FILE.LOG,l_log_msg||'|'||SQLERRM||' - '||systimestamp); --Bug 14279929
    APP_EXCEPTION.RAISE_EXCEPTION();
END batch_update_payment_info;

/*============================================================================
 |  FUNCTION  -  IS_EVENT_COMPLETE (PRIVATE)
 |
 |  DESCRIPTION
 |
 |
 |  PRAMETERS
 |           p_doc_type:INVOICES_ENTITY or PAYMENT_ENTITY
 |           p_source_id: Invoice ID or Check ID
 |           p_calling_sequence: Debug information
 |
 |  RETURN: VARCHAR2
 |
 |  KNOWN ISSUES:
 |
 |  NOTES:
 |
 |  MODIFICATION HISTORY
 |  Date         Author             Description of Change
 |
 *===========================================================================*/
FUNCTION is_event_complete (p_doc_type IN VARCHAR2,
                            p_source_id IN NUMBER,
                            p_calling_sequence IN VARCHAR2)
RETURN VARCHAR2
IS

  l_count NUMBER := 0;
  l_purch_enc_flag financials_system_parameters.purch_encumbrance_flag%TYPE;

  l_curr_calling_sequence VARCHAR2(2000);

  -- Logging:
  l_procedure_name CONSTANT VARCHAR2(30) := 'BATCH_UPDATE_PAYMENT_INFO';
  l_log_msg        FND_LOG_MESSAGES.MESSAGE_TEXT%TYPE;

BEGIN

  l_curr_calling_sequence :=
    p_calling_sequence || ' -> AP_ACCOUNTING_EVENTS_PKG.IS_EVENT_COMPLETE';

  G_CURRENT_RUNTIME_LEVEL := FND_LOG.G_CURRENT_RUNTIME_LEVEL;

  l_log_msg := 'Begin of procedure '||l_procedure_name;
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
    FND_LOG.STRING(G_LEVEL_PROCEDURE,
                   G_MODULE_NAME || l_procedure_name,
                   l_log_msg);
  END IF;

  IF p_doc_type = INVOICES_ENTITY THEN -- Bug3343314

    BEGIN

      SELECT nvl(purch_encumbrance_flag,'N')
        INTO l_purch_enc_flag
        FROM financials_system_parameters FSP,
             AP_INVOICES INV
       WHERE INV.org_id = FSP.org_id
         AND INV.set_of_books_id = FSP.set_of_books_id -- Bug 5608968 Avoid full index scan of fsp
         AND INV.invoice_id = p_source_id;
    EXCEPTION
           WHEN NO_DATA_FOUND THEN
             l_purch_enc_flag := 'N';
    END;

    IF l_purch_enc_flag = 'N' THEN

      BEGIN
        SELECT  count(*)
        INTO  l_count
        FROM  ap_invoice_distributions AID1
        WHERE AID1.invoice_id = p_source_id
        AND   nvl(AID1.match_status_flag,'N') NOT IN ('T','A');
      EXCEPTION
           WHEN NO_DATA_FOUND THEN
             l_count:= 0;
      END;

    ELSE

      BEGIN
       SELECT  count(*)
         INTO   l_count
         FROM   ap_invoice_distributions AID
        WHERE   AID.invoice_id = p_source_id
        AND     nvl(AID.match_status_flag,'N') <> 'A';
      EXCEPTION
        WHEN NO_DATA_FOUND THEN
           l_count:= 0;
      END;
    END IF;

  ELSIF p_doc_type = PAYMENTS_ENTITY THEN -- Bug3343314

    BEGIN

      SELECT nvl(purch_encumbrance_flag,'N')
        INTO l_purch_enc_flag
        FROM financials_system_parameters FSP,
             AP_CHECKS  AC
       WHERE AC.org_id = FSP.org_id
         AND AC.check_id = p_source_id;
    EXCEPTION
           WHEN NO_DATA_FOUND THEN
             l_purch_enc_flag := 'N';
    END;

    IF l_purch_enc_flag = 'N' THEN

      BEGIN
        SELECT  count(*)
        INTO    l_count
        FROM    ap_invoice_distributions AID1
        WHERE   AID1.invoice_id in (SELECT invoice_id
                                    FROM ap_invoice_payments
                                    WHERE check_id = p_source_id)
        AND     nvl(AID1.match_status_flag,'N') NOT IN ('T','A');
      EXCEPTION
        WHEN NO_DATA_FOUND THEN
           l_count:= 0;
      END;
    ELSE

      BEGIN
        SELECT  count(*)
        INTO    l_count
        FROM    ap_invoice_distributions AID
        WHERE   AID.invoice_id in (SELECT invoice_id
                                   FROM ap_invoice_payments
                                   WHERE check_id = p_source_id)
        AND  nvl(AID.match_status_flag,'N') <> 'A' ;
      EXCEPTION
        WHEN NO_DATA_FOUND THEN
           l_count:= 0;
      END;

    END IF; -- l_purch_enc_flag

  END IF ; -- p_doc_type

  IF l_count = 0  THEN
    IF p_doc_type = INVOICES_ENTITY THEN -- Bug3343314
      BEGIN
        SELECT count(*)
        INTO   l_count
        FROM   ap_holds H, ap_hold_codes C
        WHERE  H.invoice_id = p_source_id
        AND    H.hold_lookup_code = C.hold_lookup_code
        AND    ((H.release_lookup_code IS NULL)
        AND    (C.postable_flag   = 'N'
                         OR C.postable_flag    = 'X'));
      EXCEPTION
       WHEN NO_DATA_FOUND THEN
          l_count:= 0;
      END;

    ELSIF p_doc_type = PAYMENTS_ENTITY THEN -- Bug3343314

      BEGIN

        SELECT count(*)
        INTO l_count
        FROM ap_holds H, ap_hold_codes C
        WHERE  H.invoice_id in (SELECT  invoice_id
                                FROM ap_invoice_payments
                                WHERE check_id = p_source_id)
        AND    H.hold_lookup_code = C.hold_lookup_code
        AND    ((H.release_lookup_code IS NULL)
        AND    (C.postable_flag ='N'
                               OR C.postable_flag    = 'X'));
      EXCEPTION
        WHEN NO_DATA_FOUND THEN
           l_count:= 0;
      END;

    END IF; -- p_doc_type

  END IF; -- l_count = 0

  l_log_msg := 'End of procedure '||l_procedure_name;
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
    FND_LOG.STRING(G_LEVEL_PROCEDURE,
                   G_MODULE_NAME || l_procedure_name,
                   l_log_msg);
  END IF;

  IF l_count = 0 THEN
    Return('CREATED');
  ELSE
    Return('INCOMPLETE');
  END IF; -- l_count = 0

EXCEPTION
  WHEN OTHERS THEN
    IF (SQLCODE <> -20001) THEN
       FND_MESSAGE.SET_NAME('SQLAP','AP_DEBUG');
       FND_MESSAGE.SET_TOKEN('ERROR',SQLERRM);
       FND_MESSAGE.SET_TOKEN('CALLING_SEQUENCE',
                    l_curr_calling_sequence);
       FND_MESSAGE.SET_TOKEN('PARAMETERS',
               'p_doc_type = '||p_doc_type
           ||', p_source_id = '||p_source_id);
       FND_MESSAGE.SET_TOKEN('DEBUG_INFO',l_log_msg);
    END IF;
    APP_EXCEPTION.RAISE_EXCEPTION();
END is_event_complete;

/*============================================================================
 |  FUNCTION  -  DERIVE_PAYMENT_ADJ_EVENT (PRIVATE)
 |
 |  DESCRIPTION
 |    This procedure is used to derive payment adjustment event.
 |
 |  PRAMETERS
 |           p_check_id: Check ID
 |           p_accounting_date: Accounting Date
 |           p_event_type:Event Type
 |           p_accounting_event_id: Derived Accounting Event ID
 |           p_calling_sequence:Debug Information
 |
 |  KNOWN ISSUES:
 |
 |  NOTES:
 |
 |  MODIFICATION HISTORY
 |  Date         Author             Description of Change
 |
 *===========================================================================*/
PROCEDURE derive_payment_adj_event (
             p_check_id            IN    NUMBER,
             p_accounting_date     IN    DATE,
             p_event_type          IN    VARCHAR2,
             p_accounting_event_id OUT   NOCOPY NUMBER,
             p_calling_sequence    IN    VARCHAR2)
IS

  l_legal_entity_id NUMBER(15);
  l_ledger_id NUMBER(15);
  l_org_id NUMBER(15);
  l_event_security_context XLA_EVENTS_PUB_PKG.T_SECURITY;
  l_event_source_info XLA_EVENTS_PUB_PKG.T_EVENT_SOURCE_INFO;
  l_accounting_event_id NUMBER := NULL;
  l_event_info XLA_EVENTS_PUB_PKG.T_EVENT_INFO;

  l_exchange_rate_type VARCHAR2(30);
  l_exchange_rate_date DATE;
  l_exchange_rate      NUMBER;
  l_currency_code      VARCHAR2(15);
  l_creation_date      DATE;
  l_last_updated_by    NUMBER(15);

  CURSOR l_event_cur IS
    SELECT distinct(AIP.accounting_event_id)
    FROM   ap_invoice_payments AIP,
           ap_checks AC
    WHERE  AIP.check_id = p_check_id
    AND    AIP.check_id = AC.check_id
    AND    nvl(AIP.posted_flag, 'N') = 'N'
    AND    AC.void_date IS NULL;

  l_curr_calling_sequence VARCHAR2(2000);

   -- Logging:
  l_procedure_name CONSTANT VARCHAR2(30) := 'BATCH_UPDATE_PAYMENT_INFO';
  l_log_msg        FND_LOG_MESSAGES.MESSAGE_TEXT%TYPE;


BEGIN

  l_curr_calling_sequence := p_calling_sequence ||
             ' -> AP_ACCOUNTING_EVENTS_PKG.DERIVE_PAYMENT_ADJ_EVENT';

  G_CURRENT_RUNTIME_LEVEL := FND_LOG.G_CURRENT_RUNTIME_LEVEL;

  l_log_msg := 'Begin of procedure '||l_procedure_name;
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
    FND_LOG.STRING(G_LEVEL_PROCEDURE,
                   G_MODULE_NAME || l_procedure_name,
                   l_log_msg);
  END IF;

  l_log_msg := 'Before calling procedure get_payment_info';
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
     FND_LOG.STRING(G_LEVEL_PROCEDURE,
                    G_MODULE_NAME || l_procedure_name,
                    l_log_msg);
  END IF;

  get_payment_info
  ( p_check_id => p_check_id,
    p_org_id => l_org_id, -- OUT
    p_legal_entity_id => l_legal_entity_id, -- OUT
    p_ledger_id => l_ledger_id, -- OUT
    p_calling_sequence => l_curr_calling_sequence
  );

  l_log_msg := 'After calling procedure get_payment_info';
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
     FND_LOG.STRING(G_LEVEL_PROCEDURE,
                    G_MODULE_NAME || l_procedure_name,
                    l_log_msg);
  END IF;

 l_log_msg := 'Before calling procedure get_event_security_context';
 IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
     FND_LOG.STRING(G_LEVEL_PROCEDURE,
                    G_MODULE_NAME || l_procedure_name,
                    l_log_msg);
  END IF;

  l_event_security_context :=
    get_event_security_context
    ( p_org_id => l_org_id,
      p_calling_sequence => l_curr_calling_sequence
    );


  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
     l_log_msg := 'After calling procedure get_event_security_context';
     FND_LOG.STRING(G_LEVEL_PROCEDURE,
                    G_MODULE_NAME || l_procedure_name,
                    l_log_msg);
  END IF;

  l_log_msg := 'Before calling procedure get_payment_event_source_info';
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
     FND_LOG.STRING(G_LEVEL_PROCEDURE,
                    G_MODULE_NAME || l_procedure_name,
                    l_log_msg);
  END IF;

  l_event_source_info :=
    get_payment_event_source_info
    ( p_legal_entity_id => l_legal_entity_id,
      p_ledger_id => l_ledger_id,
      p_check_id => p_check_id,
      p_calling_sequence => l_curr_calling_sequence
    );

  l_log_msg := 'After calling procedure get_payment_event_source_info';
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
     FND_LOG.STRING(G_LEVEL_PROCEDURE,
                    G_MODULE_NAME || l_procedure_name,
                    l_log_msg);
  END IF;

  OPEN l_event_cur;
  LOOP

    FETCH l_event_cur INTO
      l_accounting_event_id;
    EXIT WHEN l_event_cur%NOTFOUND;

    l_log_msg := 'Before calling AP_XLA_EVENTS_PKG.GET_EVENT_INFO';
    IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
       FND_LOG.STRING(G_LEVEL_PROCEDURE,
                      G_MODULE_NAME || l_procedure_name,
                      l_log_msg);
    END IF;

    l_event_info :=
      AP_XLA_EVENTS_PKG.GET_EVENT_INFO
      ( p_event_source_info => l_event_source_info,
        p_event_id => l_accounting_event_id,
        p_valuation_method => NULL,
        p_security_context => l_event_security_context,
        p_calling_sequence => l_curr_calling_sequence
      );

    l_log_msg := 'After calling AP_XLA_EVENTS_PKG.GET_EVENT_INFO';
    IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
       FND_LOG.STRING(G_LEVEL_PROCEDURE,
                      G_MODULE_NAME || l_procedure_name,
                      l_log_msg);
    END IF;

    --bug 10336668
    IF ((P_event_type = MANUAL_PAYMENT_ADJUSTED_TYPE AND
         l_event_info.event_type_code = MANUAL_PAYMENT_ADJUSTED_TYPE)
         OR
        (P_event_type = UPGRADED_MAN_PAY_REV_TYPE    AND
         l_event_info.event_type_code = UPGRADED_MAN_PAY_REV_TYPE )
         OR
        (P_event_type = UPGRADED_MAN_PAY_ADJ_TYPE AND
         l_event_info.event_type_code = UPGRADED_MAN_PAY_ADJ_TYPE)
         OR
         (P_event_type = MANUAL_REFUND_ADJUSTED_TYPE AND
         l_event_info.event_type_code = MANUAL_REFUND_ADJUSTED_TYPE)) THEN
            EXIT;
    ELSE
      l_accounting_event_id := NULL;
    END IF;

  END LOOP;
  CLOSE l_event_cur;

  -- If an unaccounted Payment Adjustment event does not already
  -- exist for that date, we will create a new event

  IF (l_accounting_event_id IS NULL) THEN
    l_accounting_event_id := create_payment_event
                              (p_event_type       => p_event_type,
                               p_check_id         => p_check_id,
                               p_event_date       => p_accounting_date,
                               p_calling_sequence => l_curr_calling_sequence);

     SELECT exchange_rate_type,
            exchange_date,
            exchange_rate,
            currency_code,
            creation_date,
            last_updated_by
     INTO   l_exchange_rate_type,
            l_exchange_rate_date,
            l_exchange_rate,
            l_currency_code,
            l_creation_date,
            l_last_updated_by
     FROM   AP_CHECKS AC
     WHERE  AC.check_id = P_check_id;

     l_log_msg := 'Before AP_RECONCILIATION_PKG.INSERT_PAYMENT_HISTORY';
     IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
       FND_LOG.STRING(G_LEVEL_PROCEDURE,
                      G_MODULE_NAME || l_procedure_name,
                      l_log_msg);
     END IF;

     AP_RECONCILIATION_PKG.insert_payment_history
      (
          x_check_id                => p_check_id,
          x_transaction_type        => p_event_type,
          x_accounting_date         => p_accounting_date,
          x_trx_bank_amount         => NULL,
          x_errors_bank_amount      => NULL,
          x_charges_bank_amount     => NULL,
          x_bank_currency_code      => NULL,
          x_bank_to_base_xrate_type => NULL,
          x_bank_to_base_xrate_date => NULL,
          x_bank_to_base_xrate      => NULL,
          x_trx_pmt_amount          => 0,
          x_errors_pmt_amount       => NULL,
          x_charges_pmt_amount      => NULL,
          x_pmt_currency_code       => l_currency_code,
          x_pmt_to_base_xrate_type  => l_exchange_rate_type,
          x_pmt_to_base_xrate_date  => l_exchange_rate_date,
          x_pmt_to_base_xrate       => l_exchange_rate,
          x_trx_base_amount         => 0,
          x_errors_base_amount      => NULL,
          x_charges_base_amount     => NULL,
          x_matched_flag            => NULL,
          x_rev_pmt_hist_id         => NULL,
          x_org_id                  => l_org_id, -- bug 4578865
          x_creation_date           => SYSDATE,
          x_created_by              => l_last_updated_by,
          x_last_update_date        => SYSDATE,
          x_last_updated_by         => l_last_updated_by,
          x_last_update_login       => l_last_updated_by,
          x_program_update_date     => NULL,
          x_program_application_id  => NULL,
          x_program_id              => NULL,
          x_request_id              => NULL,
          x_calling_sequence        => l_curr_calling_sequence,
          x_accounting_event_id     => l_accounting_event_id
      );

      l_log_msg := 'After AP_RECONCILIATION_PKG.INSERT_PAYMENT_HISTORY';
      IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
         FND_LOG.STRING(G_LEVEL_PROCEDURE,
                        G_MODULE_NAME || l_procedure_name,
                        l_log_msg);
      END IF;
  END IF;

  p_accounting_event_id := l_accounting_event_id;

  l_log_msg := 'End of procedure '||l_procedure_name;
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
     FND_LOG.STRING(G_LEVEL_PROCEDURE,
                    G_MODULE_NAME || l_procedure_name,
                    l_log_msg);
  END IF;

EXCEPTION
  WHEN OTHERS THEN
    IF (l_event_cur%ISOPEN) THEN
      CLOSE l_event_cur;
    END IF;
    IF (SQLCODE <> -20001) THEN
       FND_MESSAGE.SET_NAME('SQLAP','AP_DEBUG');
       FND_MESSAGE.SET_TOKEN('ERROR',SQLERRM);
       FND_MESSAGE.SET_TOKEN('CALLING_SEQUENCE',
                    l_curr_calling_sequence);
       FND_MESSAGE.SET_TOKEN('PARAMETERS',
               'p_check_id = '||p_check_id
           ||', p_accounting_date = '||p_accounting_date
           ||', p_event_type = ' ||p_event_type);
       FND_MESSAGE.SET_TOKEN('DEBUG_INFO',l_log_msg);
    END IF;
    APP_EXCEPTION.RAISE_EXCEPTION();
END derive_payment_adj_event;


/*============================================================================
 |  FUNCTION  -  DERIVE_CASCADE_EVENT (PRIVATE)
 |
 |  DESCRIPTION
 |      This procedure creates the cascade events for an invoice adjusted
 |      event. A cascade event is created when there is a corresponding normal
 |      event that has already been accounted. This procedure creates
 |      prepayment application adjusted, payment adjusted, payment maturity
 |      adjusted, and payment clearing adjusted events.
 |
 |  PRAMETERS
 |          p_invoice_id: Invoice Id of the adjusted invoice
 |          p_adj_accounting_event_id: Event ID of the invoice adjustment
 |          p_calling_sequence: Debug information
 |
 |  KNOWN ISSUES:
 |
 |  NOTES:
 |
 |  MODIFICATION HISTORY
 |  Date         Author             Description of Change
 |
 *===========================================================================*/
-- Bug 6996047. Added accounting date parameter
PROCEDURE Derive_Cascade_Events(p_invoice_id IN NUMBER,
                                p_adj_accounting_event_id IN NUMBER,
                                p_accounting_date IN DATE,
                                p_calling_sequence IN VARCHAR2)
IS

  -- Bug 6890810. Added historical flag condition to all the cursors
  -- so that there will not be any adjustment event created for the
  -- adjustments made to the upgraded invoices or payments.

  -- bug9321985, changed all the cursors below to ensure that
  -- cascade events do not get generated for related events in
  -- NO ACTION status
  --
  -- Bug 9257065, added the not exists clause so as to not generate
  -- extra Prepayment Application Adj events for the same Invoice
  -- adjustment event and related prepay app event
  --
  -- bug9038462, PREPAYMENT APPLICATION ADJ cascade event
  -- would be generated if the Prepayment Application Dist
  -- (Invoice Dist) is either Accounted or Encumbered
  --
  -- bug9650381, addded the distinct Keyword in the cursor
  -- sql to avoid multiple prepayment application adjustment
  -- events from getting created in case the Prepayment Appl
  -- event is stamped on Multiple Invoice distributions
  --
  -- bug9973070, added the condition AID.Accounting_Event_id
  -- IS NOT NULL, removed outer join with XLA_EVENTS
  --
  /* Bug 9682862 added exists condition for ap_invoice_distributions table*/

  CURSOR l_prepayment_applications_cur IS
    SELECT DISTINCT
           AID.invoice_id,
           AID.accounting_date,
           AID.invoice_line_number
    FROM   ap_invoice_distributions AID,
           xla_events XE
    WHERE  AID.invoice_id = p_invoice_id
    AND    AID.line_type_lookup_code = 'PREPAY'
    AND    AID.Accounting_Event_ID IS NOT NULL
    AND    AID.Accounting_event_id = XE.event_id
    AND    XE.application_id = 200
    AND    ((XE.event_status_code <> 'N' AND
             AID.posted_flag = 'Y') OR
           AID.encumbered_flag = 'Y')
    AND    AID.amount < 0
    AND    NVL(AID.reversal_flag, 'N') <> 'Y'
    AND    AID.parent_reversal_id IS NULL
    AND    NVL(AID.Historical_Flag, 'N') <> 'Y'
    AND    NOT EXISTS
              (SELECT 1
                 FROM ap_prepay_history_all APPH
                WHERE APPH.invoice_id = AID.invoice_id
                  AND APPH.Invoice_Adjustment_Event_Id = P_Adj_Accounting_Event_Id
                  AND APPH.Related_Prepay_App_Event_Id = AID.Accounting_Event_Id
                  AND APPH.transaction_type = 'PREPAYMENT APPLICATION ADJ'
                  AND nvl(APPH.posted_flag, 'N') <> 'Y')
    AND EXISTS (SELECT 'INVOICE DIST WITH INV ADJ EVENT ID'
                  FROM ap_invoice_distributions aid2
                 WHERE aid2.accounting_event_id=p_adj_accounting_event_id
                   AND aid2.invoice_id = p_invoice_id
                   AND aid2.line_type_lookup_code <> 'AWT'); --bug 9682862

  /* BUG # 7560346 added DISTINCT keyword */
  /* Bug 9103993 added exists condition for ap_invoice_distributions table*/
  /* Bug 9582285 extended the changes done in bug 9103993 to cursors
     l_payment_clearings_cur and l_payment_maturities_cur and added
     AWT decode on top of the Exists clause added by bug 9103993 for all
     cursors*/
--Bug 16674834
  CURSOR l_payments_cur IS
    SELECT aip2.check_id
         , aip2.accounting_date
      FROM (SELECT distinct aip.check_id
                  , aip.accounting_date
                  , xe.entity_id
            FROM  ap_invoice_payments AIP,
                  xla_events XE
            WHERE AIP.invoice_id = p_invoice_id
            AND   AIP.posted_flag = 'Y'
            AND   NVL(AIP.reversal_flag, 'N') <> 'Y'
            AND   AIP.reversal_inv_pmt_id IS NULL
            AND   AIP.Accounting_event_id = XE.event_id
            AND   XE.application_id = 200
            AND   XE.event_status_code <> 'N'
            -- Bug 6890810. Added the subquery
            AND   NOT EXISTS (SELECT 'Either Upgraded Payment OR Unaccounted Pay Adj Exists'
                              FROM   AP_Payment_History APH
                              WHERE  APH.Check_ID = AIP.Check_ID
                              AND    (NVL(APH.Historical_Flag, 'N') = 'Y'
                              OR     (APH.invoice_adjustment_event_id = p_adj_accounting_event_id
                                      AND APH.transaction_type in ( PAYMENT_ADJUSTED_TYPE,REFUND_ADJUSTED_TYPE)  --bug9726978,13963864
                                      AND NVL(APH.posted_flag, 'N') <> 'Y')))  --bug 9226273
            AND NOT EXISTS (SELECT 'Voided but not yet insterted into AIP'  --bug8214188
                              FROM AP_Payment_History APH
                             WHERE APH.Check_ID = AIP.Check_ID
                               AND APH.transaction_type='PAYMENT CANCELLED')
            AND EXISTS (SELECT 'INVOICE DIST WITH INV ADJ EVENT ID'
                   FROM ap_invoice_distributions aid
                   where aid.accounting_event_id=p_adj_accounting_event_id
                   and aid.invoice_id = p_invoice_id
                   and decode(aid.line_type_lookup_code, 'AWT',
                              decode(aid.awt_invoice_payment_id,
                                    NULL, 'N', 'Y'), 'Y') = 'Y') --bug 9582285
            ORDER BY xe.entity_id) aip2;

  /* BUG # 7560346 added DISTINCT keyword */
  --Bug 16674834
  CURSOR l_payment_clearings_cur IS
    SELECT aph2.check_id
         , aph2.accounting_date
      FROM (SELECT DISTINCT APH.check_id,
                   APH.accounting_date,
                   xe.entity_id
            FROM ap_payment_history APH,
                 ap_invoice_payments AIP,
                 xla_events XE
            WHERE AIP.invoice_id = p_invoice_id
            AND   AIP.check_id  = APH.check_id
            AND   APH.transaction_type = 'PAYMENT CLEARING'
            AND   AIP.posted_flag = 'Y'
            AND   APH.Accounting_event_id = XE.event_id
            AND   XE.application_id = 200
            AND   XE.event_status_code <> 'N'
            /* bug # 7604906. If the payment clearing is not
               accounted, payment clearing adjustment should not
               create */
            AND   APH.posted_flag = 'Y'
            /* bug # 7604906 End */
            /* bug 8325874 commented the line below and added subquery */
            --AND   NVL(APH.Historical_Flag, 'N') <> 'Y' -- Bug 6890810
            AND   NOT EXISTS (SELECT 'Either Upgraded Payment OR Unaccounted Pay Adj Exists'
                              FROM   AP_Payment_History APH
                              WHERE  APH.Check_ID = AIP.Check_ID
                              AND    (NVL(APH.Historical_Flag, 'N') = 'Y'
                              OR     (APH.invoice_adjustment_event_id = p_adj_accounting_event_id
                                      AND APH.transaction_type = PAYMENT_CLEARING_ADJUSTED_TYPE
                                      AND NVL(APH.posted_flag, 'N') <> 'Y')))  --bug 9726978
            /* bug # 8325874 End */
            AND   NOT EXISTS (SELECT 'PAYMENT UNCLEARING/CANCELLATION EXISTS'
                              FROM  ap_payment_history APH1
                              WHERE APH1.check_id = AIP.check_id
                              AND   ((APH1.rev_pmt_hist_id = APH.payment_history_id
                                       AND APH1.transaction_type = 'PAYMENT UNCLEARING')
                                    OR APH1.transaction_type = 'PAYMENT CANCELLED'))
            /*bug 8882614, added the OR condition above to prevent clearing adjustment
            events from being created if cancellation event exists*/
            AND EXISTS (SELECT 'INVOICE DIST WITH INV ADJ EVENT ID'
                   FROM ap_invoice_distributions aid
                   where aid.accounting_event_id=p_adj_accounting_event_id
                   and aid.invoice_id = p_invoice_id
                   and decode(aid.line_type_lookup_code, 'AWT',
                              decode(aid.awt_invoice_payment_id,
                                    NULL, 'N', 'Y'), 'Y') = 'Y') --bug 9582285
            ORDER BY xe.entity_id)aph2;                        

  /* BUG # 7560346 added DISTINCT keyword */
  --Bug 16674834
  CURSOR l_payment_maturities_cur IS
    SELECT aph2.check_id
         , aph2.accounting_date
      FROM (SELECT DISTINCT APH.check_id,
                   APH.accounting_date,
                   xe.entity_id
            FROM   ap_payment_history APH,
                   ap_invoice_payments AIP,
                   xla_events XE
            WHERE AIP.invoice_id = p_invoice_id
            AND   AIP.check_id  = APH.check_id
            AND   APH.transaction_type = 'PAYMENT MATURITY'
            AND   AIP.posted_flag = 'Y'
            AND   APH.posted_flag = 'Y'   --bug 9030890
            AND   APH.Accounting_event_id = XE.event_id
            AND   XE.application_id = 200
            AND   XE.event_status_code <> 'N'
            /* bug 8325874 commented the line below and added subquery */
            --AND   NVL(APH.Historical_Flag, 'N') <> 'Y' -- Bug 6890810
            AND   NOT EXISTS (SELECT 'Either Upgraded Payment OR Unaccounted Pay Adj Exists'
                              FROM   AP_Payment_History APH
                              WHERE  APH.Check_ID = AIP.Check_ID
                              AND    (NVL(APH.Historical_Flag, 'N') = 'Y'
                              OR     (APH.invoice_adjustment_event_id = p_adj_accounting_event_id
                                      AND APH.transaction_type = PAYMENT_MATURITY_ADJUSTED_TYPE
                                      AND NVL(APH.posted_flag, 'N') <> 'Y')))  --bug 9726978
            /* bug # 8325874 End */
            AND   NOT EXISTS (SELECT 'PAYMENT MATURITY-REVERSAL/CANCELLATION EXISTS'
                              FROM  ap_payment_history APH1
                              WHERE APH1.check_id = AIP.check_id
                              AND   ((APH1.rev_pmt_hist_id = APH.payment_history_id
                                       AND APH1.transaction_type = 'PAYMENT MATURITY REVERSAL')
                                    OR APH1.transaction_type = 'PAYMENT CANCELLED'))
            /*bug 8882614, added the OR condition above to prevent maturity adjustment
             events from being created if cancellation event exists*/
            AND EXISTS (SELECT 'INVOICE DIST WITH INV ADJ EVENT ID'
                   FROM ap_invoice_distributions aid
                   where aid.accounting_event_id=p_adj_accounting_event_id
                   and aid.invoice_id = p_invoice_id
                   and decode(aid.line_type_lookup_code, 'AWT',
                              decode(aid.awt_invoice_payment_id,
                                    NULL, 'N', 'Y'), 'Y') = 'Y') --bug 9582285
            ORDER BY xe.entity_id )aph2;                        


 -- add cursor for bug fix 5694577
   CURSOR c_get_payment_info (p_check_id NUMBER) IS
    SELECT ac.exchange_rate_type,
           ac.exchange_date,
           ac.exchange_rate,
           ac.currency_code,
           ac.creation_date,
           ac.last_updated_by,
           ac.org_id,
           ac.payment_type_flag,
           asp.automatic_offsets_flag
      FROM ap_checks_all ac,
        ap_system_parameters_all asp   ---7209263 added automatic offsets flag
     WHERE ac.check_id = p_check_id
      AND       ac.org_id=asp.org_id;

  l_accounting_event_id NUMBER;

  TYPE t_invoice_ids IS TABLE OF NUMBER(15) INDEX BY PLS_INTEGER;
  TYPE t_accounting_dates IS TABLE OF DATE INDEX BY PLS_INTEGER;
  TYPE t_invoice_line_numbers IS TABLE OF NUMBER(15) INDEX BY PLS_INTEGER;
  TYPE t_check_ids IS TABLE OF NUMBER(15) INDEX BY PLS_INTEGER;

  l_prepay_app_invoice_ids t_invoice_ids;
  l_prepay_app_accounting_dates t_accounting_dates;
  l_prepay_app_invoice_line_num t_invoice_line_numbers;
  l_pay_check_ids t_check_ids;
  l_pay_accounting_dates t_accounting_dates;
  l_pay_clear_check_ids t_check_ids;
  l_pay_clear_accounting_dates t_accounting_dates;
  l_pay_mat_check_ids t_check_ids;
  l_pay_mat_accounting_dates t_accounting_dates;

  l_exchange_rate_type  VARCHAR2(30);
  l_exchange_rate_date  DATE;
  l_exchange_rate       NUMBER;
  l_currency_code       VARCHAR2(15);
  l_creation_date       DATE;
  l_last_updated_by     NUMBER(15);
  l_adj_accounting_event_id NUMBER := p_adj_accounting_event_id;
  l_debug_info VARCHAR2(240);
  l_curr_calling_sequence VARCHAR2(2000);
  l_org_id              NUMBER;

  -- Logging:
  l_procedure_name CONSTANT VARCHAR2(30) := 'DERIVE_CASCADE_EVENTS';
  l_log_msg        FND_LOG_MESSAGES.MESSAGE_TEXT%TYPE;

  l_event_type     xla_events.event_type_code%TYPE;
  l_pay_type       ap_checks_all.payment_type_flag%TYPE;
  l_automatic_offsets_flag      ap_system_parameters_all.automatic_offsets_flag%TYPE; ---7209263
  l_bank_curr_code              ap_payment_history_all.bank_currency_code%TYPE;                         ---7337949

-- Bug 8276852 Start
  l_bank_to_base_xrate_type     ap_payment_history_all.bank_to_base_xrate_type%TYPE;
  l_bank_to_base_xrate_date     ap_payment_history_all.bank_to_base_xrate_date%TYPE;
  l_bank_to_base_xrate          ap_payment_history_all.bank_to_base_xrate%TYPE;
   l_charges_bank_amount          ap_payment_history_all.charges_bank_amount%TYPE; /*Bug13385106 */
-- Bug 8276852 End

  /* bug12363669 start */
   l_pmt_to_base_xrate_type     ap_payment_history_all.pmt_to_base_xrate_type%type;
   l_pmt_to_base_xrate_date     ap_payment_history_all.pmt_to_base_xrate_date%type;
   l_pmt_to_base_xrate          ap_payment_history_all.pmt_to_base_xrate%type;
  /* bug12363669 end*/

  l_accounting_date         DATE := p_accounting_date;       -- bug 10126192
  l_count                   NUMBER; --bug12569594

BEGIN

  l_curr_calling_sequence :=
    p_calling_sequence || ' -> AP_ACCOUNTING_EVENTS_PKG.DERIVE_CASCADE_EVENTS';

  G_CURRENT_RUNTIME_LEVEL := FND_LOG.G_CURRENT_RUNTIME_LEVEL;

  l_log_msg := 'Begin of procedure '||l_procedure_name;
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
     FND_LOG.STRING(G_LEVEL_PROCEDURE,
                    G_MODULE_NAME || l_procedure_name,
                    l_log_msg);
  END IF;

  l_log_msg := 'Cascade the prepayment application events';
  IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL ) THEN
     l_log_msg := 'Cascade the prepayment application events';
     FND_LOG.STRING(G_LEVEL_STATEMENT,
                    G_MODULE_NAME || l_procedure_name,
                    l_log_msg);
  END IF;

  OPEN l_prepayment_applications_cur;
  LOOP

    FETCH l_prepayment_applications_cur
    BULK COLLECT INTO
      l_prepay_app_invoice_ids,
      l_prepay_app_accounting_dates,
      l_prepay_app_invoice_line_num
    LIMIT 1000;

    FOR i IN 1 ..l_prepay_app_invoice_ids.count LOOP
      
      /* added below query for bug12569594 to see if proration already 
         exists for the adj distributions */

      l_count := 0;
      BEGIN
        SELECT count(1)
	  INTO l_count
	  FROM ap_invoice_distributions_all aid
	 WHERE aid.invoice_id = l_prepay_app_invoice_ids(i)
	   AND aid.accounting_event_id = p_adj_accounting_event_id
	   AND NOT EXISTS
	       (SELECT 1
	          FROM ap_prepay_app_dists apad,
		       ap_prepay_history_all apph
		 WHERE apad.prepay_history_id = apph.prepay_history_id
		   AND apph.transaction_type = 'PREPAYMENT APPLIED'
		   AND apph.invoice_id = l_prepay_app_invoice_ids(i)
		   AND apph.invoice_line_number = l_prepay_app_invoice_line_num(i)
		   AND apad.invoice_distribution_id = aid.invoice_distribution_id)
           AND rownum < 2;

      EXCEPTION
      WHEN OTHERS THEN
        l_count := 0;

      END;

      IF l_count > 0 THEN

        l_log_msg := 'Before calling procedure create_invoice_event';
        IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
           FND_LOG.STRING(G_LEVEL_PROCEDURE,
                          G_MODULE_NAME || l_procedure_name,
                          l_log_msg);
        END IF;
  
        l_log_msg := 'Before calling procedure create_invoice_event';
        IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
           FND_LOG.STRING(G_LEVEL_PROCEDURE,
                          G_MODULE_NAME || l_procedure_name,
                          l_log_msg);
        END IF;
  
        l_accounting_event_id := create_invoice_event
                               (p_event_type => PREPAY_APP_ADJUSTED_TYPE,
                                p_invoice_id => l_prepay_app_invoice_ids(i),
                                p_event_date => p_accounting_date, -- Bug 6996047
                                p_calling_sequence => l_curr_calling_sequence);
  
        l_log_msg := 'After calling procedure create_invoice_event';
        IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
           FND_LOG.STRING(G_LEVEL_PROCEDURE,
                          G_MODULE_NAME || l_procedure_name,
                          l_log_msg);
        END IF;
  
        l_log_msg := 'Before calling procedure Insert_Prepayment_Header';
        IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
           FND_LOG.STRING(G_LEVEL_PROCEDURE,
                          G_MODULE_NAME || l_procedure_name,
                          l_log_msg);
        END IF;
  
        Insert_Prepayment_Header
                     (p_invoice_id => l_prepay_app_invoice_ids(i),
                      p_invoice_line_number => l_prepay_app_invoice_line_num(i),
                      p_accounting_event_id => l_accounting_event_id,
                      p_accounting_date => p_accounting_date, -- Bug 6996047
                      p_invoice_adjustment_id => l_adj_accounting_event_id,
                      p_calling_sequence => l_curr_calling_sequence);
  
        l_log_msg := 'After calling procedure Insert_prepayment_Header';
        IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
           FND_LOG.STRING(G_LEVEL_PROCEDURE,
                          G_MODULE_NAME || l_procedure_name,
                          l_log_msg);
        END IF;

      END IF;
    END LOOP;

    EXIT WHEN l_prepayment_applications_cur%NOTFOUND;
  END LOOP;
  CLOSE l_prepayment_applications_cur;


  IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL ) THEN
     l_log_msg := 'cascade the payment events';
     FND_LOG.STRING(G_LEVEL_STATEMENT,
                    G_MODULE_NAME || l_procedure_name,
                    l_log_msg);
  END IF;


  OPEN l_payments_cur;
  LOOP

    FETCH l_payments_cur
    BULK COLLECT INTO
      l_pay_check_ids,
      l_pay_accounting_dates
    LIMIT 1000;

    FOR i IN 1 .. l_pay_check_ids.count LOOP

      l_log_msg := 'Before calling procedure create_payment_event';
      IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
         FND_LOG.STRING(G_LEVEL_PROCEDURE,
                        G_MODULE_NAME || l_procedure_name,
                        l_log_msg);
      END IF;

      -- bug fix 5694577
      OPEN c_get_payment_info(l_pay_check_ids(i));
      FETCH c_get_payment_info
       INTO l_exchange_rate_type,
            l_exchange_rate_date,
            l_exchange_rate,
            l_currency_code,
            l_creation_date,
            l_last_updated_by,
            l_org_id,
            l_pay_type,
            l_automatic_offsets_flag;           ---7209263
      CLOSE c_get_payment_info;

      IF (l_pay_type = 'R') THEN
        l_event_type := REFUND_ADJUSTED_TYPE;
      ELSE
        l_event_type := PAYMENT_ADJUSTED_TYPE;
      END IF;

      --bug 10126192 start
        IF l_accounting_date < l_pay_accounting_dates(i) THEN
           l_accounting_date := l_pay_accounting_dates(i);
        END IF;
      --bug 10126192 end

      l_accounting_event_id :=
        create_payment_event
        ( p_event_type => l_event_type,
          p_check_id => l_pay_check_ids(i),
          p_event_date => l_accounting_date, -- Bug 10126192,6996047
          p_calling_sequence => l_curr_calling_sequence
        );

       l_log_msg := 'After calling procedure create_payment_event';
       IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
          FND_LOG.STRING(G_LEVEL_PROCEDURE,
                         G_MODULE_NAME || l_procedure_name,
                         l_log_msg);
       END IF;

       l_log_msg := 'Before AP_RECONCILIATION_PKG.INSERT_PAYMENT_HISTORY';
       IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
         FND_LOG.STRING(G_LEVEL_PROCEDURE,
                        G_MODULE_NAME || l_procedure_name,
                        l_log_msg);
       END IF;

       AP_RECONCILIATION_PKG.insert_payment_history
       (
          x_check_id                => l_pay_check_ids(i),
          x_transaction_type        => l_event_type   ,
          x_accounting_date         => l_accounting_date, -- Bug10126192,6996047
          x_trx_bank_amount         => NULL,
          x_errors_bank_amount      => NULL,
          x_charges_bank_amount     => NULL,
          x_bank_currency_code      => NULL,
          x_bank_to_base_xrate_type => NULL,
          x_bank_to_base_xrate_date => NULL,
          x_bank_to_base_xrate      => NULL,
          x_trx_pmt_amount          => 0,
          x_errors_pmt_amount       => NULL,
          x_charges_pmt_amount      => NULL,
          x_pmt_currency_code       => l_currency_code,
          x_pmt_to_base_xrate_type  => l_exchange_rate_type,
          x_pmt_to_base_xrate_date  => l_exchange_rate_date,
          x_pmt_to_base_xrate       => l_exchange_rate,
          x_trx_base_amount         => 0,
          x_errors_base_amount      => NULL,
          x_charges_base_amount     => NULL,
          x_matched_flag            => NULL,
          x_rev_pmt_hist_id         => NULL,
          x_org_id                  => l_org_id,  -- bug 4578865
          x_creation_date           => SYSDATE,
          x_created_by              => l_last_updated_by,
          x_last_update_date        => SYSDATE,
          x_last_updated_by         => l_last_updated_by,
          x_last_update_login       => l_last_updated_by,
          x_program_update_date     => NULL,
          x_program_application_id  => NULL,
          x_program_id              => NULL,
          x_request_id              => NULL,
          x_calling_sequence        => l_curr_calling_sequence,
          x_accounting_event_id     => l_accounting_event_id,
          x_invoice_adjustment_event_id => l_adj_accounting_event_id -- bug fix 5694577
        );

      l_log_msg := 'After AP_RECONCILIATION_PKG.INSERT_PAYMENT_HISTORY';
      IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
         FND_LOG.STRING(G_LEVEL_PROCEDURE,
                        G_MODULE_NAME || l_procedure_name,
                        l_log_msg);
      END IF;


    END LOOP;
    EXIT WHEN l_payments_cur%NOTFOUND;
  END LOOP;
  CLOSE l_payments_cur;

  l_log_msg := 'Cascade the payment clearing events';
  IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL ) THEN
     FND_LOG.STRING(G_LEVEL_STATEMENT,
                    G_MODULE_NAME || l_procedure_name,
                    l_log_msg);
  END IF;

  OPEN l_payment_clearings_cur;
  LOOP

    FETCH l_payment_clearings_cur
    BULK COLLECT INTO
      l_pay_clear_check_ids,
      l_pay_clear_accounting_dates
    LIMIT 1000;

    FOR i IN 1 .. l_pay_clear_check_ids.count LOOP

      l_log_msg := 'Before calling procedure create_payment_event';
      IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
         FND_LOG.STRING(G_LEVEL_PROCEDURE,
                        G_MODULE_NAME || l_procedure_name,
                        l_log_msg);
      END IF;
/* commented here for bug 7607184
---placed here due to 7209263
SELECT AC.exchange_rate_type,
              AC.exchange_date,
              AC.exchange_rate,
              AC.creation_date,
              AC.last_updated_by,
              AC.org_id,
              asp.automatic_offsets_flag
       INTO   l_exchange_rate_type,
              l_exchange_rate_date,
              l_exchange_rate,
              l_creation_date,
              l_last_updated_by,
              l_org_id,
              l_automatic_offsets_flag                          --7209263
       FROM   AP_CHECKS AC, ap_system_parameters_all asp
       WHERE  AC.check_id = l_pay_clear_check_ids(i) --bug 7278341 l_pay_check_ids(i)
       AND      AC.org_id=asp.org_id;
*/
--bug 7607184
OPEN c_get_payment_info(l_pay_clear_check_ids(i));
      FETCH c_get_payment_info
       INTO l_exchange_rate_type,
            l_exchange_rate_date,
            l_exchange_rate,
            l_currency_code,
            l_creation_date,
            l_last_updated_by,
            l_org_id,
            l_pay_type,
            l_automatic_offsets_flag;
      CLOSE c_get_payment_info;

--added to pick bank_currency_code from payment clearing for bug 7337949
-- Bug 8276852 Added XRate Details type, Date, Rate
 SELECT aph.bank_currency_code
        ,aph.bank_to_base_xrate_type
        ,aph.bank_to_base_xrate_date
        ,aph.bank_to_base_xrate
        ,aph.charges_bank_amount  /*Bug13385106 */
        /* bug12363669 start */
        ,aph.pmt_to_base_xrate_type 
        ,aph.pmt_to_base_xrate_date
        ,aph.pmt_to_base_xrate
        /* bug12363669 end */
   INTO l_bank_curr_code
        ,l_bank_to_base_xrate_type
        ,l_bank_to_base_xrate_date
        ,l_bank_to_base_xrate
        ,l_charges_bank_amount  /*Bug13385106 */
        /* bug12363669 start */
        ,l_pmt_to_base_xrate_type
        ,l_pmt_to_base_xrate_date
        ,l_pmt_to_base_xrate
        /* bug12363669 end*/
   FROM ap_payment_history_all APH
  WHERE APH.check_id = l_pay_clear_check_ids(i)
 AND APH.transaction_type = 'PAYMENT CLEARING'
 --added for bug 7614505
 AND   NOT EXISTS( SELECT 'PAYMENT UNCLEARING EXISTS'
                      FROM  ap_payment_history_all APH1
                      WHERE APH1.rev_pmt_hist_id = APH.payment_history_id
                      AND   APH1.transaction_type = 'PAYMENT UNCLEARING'
                      AND   APH1.check_id = APH.check_id);

 ------7337949 ends
-- Bug 8276852 End

      --bug 10126192 start
        IF l_accounting_date < l_pay_clear_accounting_dates(i) THEN
           l_accounting_date := l_pay_clear_accounting_dates(i);
        END IF;
      --bug 10126192 end

      l_accounting_event_id :=
        create_payment_event
        ( p_event_type => PAYMENT_CLEARING_ADJUSTED_TYPE,
          p_check_id => l_pay_clear_check_ids(i),
          p_event_date => l_accounting_date, --Bug 10126192,6996047
          p_calling_sequence => l_curr_calling_sequence);

      IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
         l_log_msg := 'After calling procedure create_payment_event';
         FND_LOG.STRING(G_LEVEL_PROCEDURE,
                        G_MODULE_NAME || l_procedure_name,
                        l_log_msg);
      END IF;

----commented as placed above due to 7209263
     /*  SELECT exchange_rate_type,
              exchange_date,
              exchange_rate,
              creation_date,
              last_updated_by,
              org_id
       INTO   l_exchange_rate_type,
              l_exchange_rate_date,
              l_exchange_rate,
              l_creation_date,
              l_last_updated_by,
              l_org_id
       FROM   AP_CHECKS AC
       WHERE  AC.check_id = l_pay_check_ids(i);
       */


      IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
         l_log_msg := 'Before AP_RECONCILIATION_PKG.INSERT_PAYMENT_HISTORY';
         FND_LOG.STRING(G_LEVEL_PROCEDURE,
                        G_MODULE_NAME || l_procedure_name,
                        l_log_msg);
      END IF;


      AP_RECONCILIATION_PKG.insert_payment_history
     (
        x_check_id                => l_pay_clear_check_ids(i),
        x_transaction_type        => 'PAYMENT CLEARING ADJUSTED',
        x_accounting_date         => l_accounting_date, --Bug 10126192,6996047
        x_trx_bank_amount         => NULL,
        x_errors_bank_amount      => NULL,
        x_charges_bank_amount     =>l_charges_bank_amount, /*Bug13385106 */
        x_bank_currency_code      => l_bank_curr_code,
        x_bank_to_base_xrate_type => l_bank_to_base_xrate_type, --Bug 8276852
        x_bank_to_base_xrate_date => l_bank_to_base_xrate_date, --Bug 8276852
        x_bank_to_base_xrate      => l_bank_to_base_xrate,      --Bug 8276852
        x_trx_pmt_amount          => 0,
        x_errors_pmt_amount       => NULL,
        x_charges_pmt_amount      => NULL,
        x_pmt_currency_code       => l_currency_code,
        x_pmt_to_base_xrate_type  => l_pmt_to_base_xrate_type, --bug12363669
        x_pmt_to_base_xrate_date  => l_pmt_to_base_xrate_date, --bug12363669
        x_pmt_to_base_xrate       => l_pmt_to_base_xrate,      --bug12363669
        x_trx_base_amount         => 0,
        x_errors_base_amount      => NULL,
        x_charges_base_amount     => NULL,
        x_matched_flag            => NULL,
        x_rev_pmt_hist_id         => NULL,
        x_org_id                  => l_org_id,  -- bug 4578865
        x_creation_date           => SYSDATE,
        x_created_by              => l_last_updated_by,
        x_last_update_date        => SYSDATE,
        x_last_updated_by         => l_last_updated_by,
        x_last_update_login       => l_last_updated_by,
        x_program_update_date     => NULL,
        x_program_application_id  => NULL,
        x_program_id              => NULL,
        x_request_id              => NULL,
        x_calling_sequence        => l_curr_calling_sequence,
        x_accounting_event_id     => l_accounting_event_id,
        x_invoice_adjustment_event_id => l_adj_accounting_event_id --bug6710016
      );

      l_log_msg := 'After AP_RECONCILIATION_PKG.INSERT_PAYMENT_HISTORY ';
      IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
         FND_LOG.STRING(G_LEVEL_PROCEDURE,
                        G_MODULE_NAME || l_procedure_name,
                        l_log_msg);
      END IF;


    END LOOP;
    EXIT WHEN l_payment_clearings_cur%NOTFOUND;
  END LOOP;
  CLOSE l_payment_clearings_cur;


  l_log_msg := 'Cascade the payment maturity events';
  IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL ) THEN
     FND_LOG.STRING(G_LEVEL_STATEMENT,
                    G_MODULE_NAME || l_procedure_name,
                    l_log_msg);
  END IF;

  OPEN l_payment_maturities_cur;
  LOOP

    FETCH l_payment_maturities_cur
    BULK COLLECT INTO
      l_pay_mat_check_ids,
      l_pay_mat_accounting_dates
    LIMIT 1000;

    FOR i IN 1 .. l_pay_mat_check_ids.count LOOP




--bug 10114700 added call to c_get_payment_info
OPEN c_get_payment_info(l_pay_mat_check_ids(i));
      FETCH c_get_payment_info
       INTO l_exchange_rate_type,
            l_exchange_rate_date,
            l_exchange_rate,
            l_currency_code,
            l_creation_date,
            l_last_updated_by,
            l_org_id,
            l_pay_type,
            l_automatic_offsets_flag;
      CLOSE c_get_payment_info;

      l_log_msg := 'Before calling procedure create_payment_event';
      IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
         FND_LOG.STRING(G_LEVEL_PROCEDURE,
                        G_MODULE_NAME || l_procedure_name,
                        l_log_msg);
      END IF;

      --bug 10126192 start
        IF l_accounting_date < l_pay_mat_accounting_dates(i) THEN
           l_accounting_date := l_pay_mat_accounting_dates(i);
        END IF;
      --bug 10126192 end

      l_accounting_event_id :=
        create_payment_event
        ( p_event_type => PAYMENT_MATURITY_ADJUSTED_TYPE,
          p_check_id => l_pay_mat_check_ids(i),
          p_event_date => l_accounting_date, --Bug 10126192,6996047
          p_calling_sequence => l_curr_calling_sequence);

      l_log_msg := 'After calling procedure create_payment_event';
      IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
         FND_LOG.STRING(G_LEVEL_PROCEDURE,
                        G_MODULE_NAME || l_procedure_name,
                        l_log_msg);
      END IF;

----Commented due to 7209263
      /* SELECT exchange_rate_type,
              exchange_date,
              exchange_rate,
              creation_date,
              last_updated_by,
              org_id
       INTO   l_exchange_rate_type,
              l_exchange_rate_date,
              l_exchange_rate,
              l_creation_date,
              l_last_updated_by,
              l_org_id
       FROM   AP_CHECKS AC
       WHERE  AC.check_id = l_pay_check_ids(i);
       */

       l_log_msg := 'Before AP_RECONCILIATION_PKG.INSERT_PAYMENT_HISTORY';
       IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
         FND_LOG.STRING(G_LEVEL_PROCEDURE,
                        G_MODULE_NAME || l_procedure_name,
                        l_log_msg);
       END IF;

       AP_RECONCILIATION_PKG.insert_payment_history
       (
          x_check_id                => l_pay_mat_check_ids(i),
          x_transaction_type        => 'PAYMENT MATURITY ADJUSTED',
          x_accounting_date         => l_accounting_date, --Bug 10126192,6996047
          x_trx_bank_amount         => NULL,
          x_errors_bank_amount      => NULL,
          x_charges_bank_amount     => NULL,
          x_bank_currency_code      => NULL,
          x_bank_to_base_xrate_type => NULL,
          x_bank_to_base_xrate_date => NULL,
          x_bank_to_base_xrate      => NULL,
          x_trx_pmt_amount          => 0,
          x_errors_pmt_amount       => NULL,
          x_charges_pmt_amount      => NULL,
          x_pmt_currency_code       => l_currency_code,
          x_pmt_to_base_xrate_type  => l_exchange_rate_type,
          x_pmt_to_base_xrate_date  => l_exchange_rate_date,
          x_pmt_to_base_xrate       => l_exchange_rate,
          x_trx_base_amount         => 0,
          x_errors_base_amount      => NULL,
          x_charges_base_amount     => NULL,
          x_matched_flag            => NULL,
          x_rev_pmt_hist_id         => NULL,
          x_org_id                  => l_org_id,   -- bug 4578865
          x_creation_date           => SYSDATE,
          x_created_by              => l_last_updated_by,
          x_last_update_date        => SYSDATE,
          x_last_updated_by         => l_last_updated_by,
          x_last_update_login       => l_last_updated_by,
          x_program_update_date     => NULL,
          x_program_application_id  => NULL,
          x_program_id              => NULL,
          x_request_id              => NULL,
          x_calling_sequence        => l_curr_calling_sequence,
          x_accounting_event_id     => l_accounting_event_id,
          x_invoice_adjustment_event_id => l_adj_accounting_event_id --bug6710016
        );

       l_log_msg := 'After AP_RECONCILIATION_PKG.INSERT_PAYMENT_HISTORY';
       IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
         FND_LOG.STRING(G_LEVEL_PROCEDURE,
                        G_MODULE_NAME || l_procedure_name,
                        l_log_msg);
       END IF;

    END LOOP;

    EXIT WHEN l_payment_maturities_cur%NOTFOUND;
  END LOOP;
  CLOSE l_payment_maturities_cur;

 l_log_msg := 'End of procedure '||l_procedure_name;
 IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
    FND_LOG.STRING(G_LEVEL_PROCEDURE,
                   G_MODULE_NAME || l_procedure_name,
                   l_log_msg);
 END IF;

EXCEPTION
  WHEN OTHERS THEN
    IF (l_prepayment_applications_cur%ISOPEN) THEN
      CLOSE l_prepayment_applications_cur;
    END IF;
    IF (l_payments_cur%ISOPEN) THEN
      CLOSE l_payments_cur;
    END IF;
    IF (l_payment_clearings_cur%ISOPEN) THEN
      CLOSE l_payment_clearings_cur;
    END IF;
    IF (l_payment_maturities_cur%ISOPEN) THEN
      CLOSE l_payment_maturities_cur;
    END IF;
    IF (SQLCODE <> -20001) THEN
       FND_MESSAGE.SET_NAME('SQLAP','AP_DEBUG');
       FND_MESSAGE.SET_TOKEN('ERROR',SQLERRM);
       FND_MESSAGE.SET_TOKEN('CALLING_SEQUENCE',
                    l_curr_calling_sequence);
       FND_MESSAGE.SET_TOKEN('PARAMETERS',
               'p_invoice_id = '||p_invoice_id
           ||', p_adj_accounting_event_id = '||p_adj_accounting_event_id);
       FND_MESSAGE.SET_TOKEN('DEBUG_INFO',l_log_msg);
    END IF;
    APP_EXCEPTION.RAISE_EXCEPTION();
END derive_cascade_events;



/*============================================================================
 |  PROCEDURE  -  DERIVE_INVOICE_CANCEL_EVENTs(PRIVATE)
 |
 |  DESCRIPTION
 |      This procedure creates invoice cancellation events using the GL date
 |      of the invoice distributions. If there are multiple invoice
 |      distributions with different GL dates, multiple Invoice Cancellation
 |      events will be created.
 |
 |  PRAMETERS
 |          p_invoice_id         IN: Invoice Id of the cancelled invoice
 |          p_calling_sequence   IN: Debug information
 |
 |  KNOWN ISSUES:
 |
 |  NOTES:
 |
 |  MODIFICATION HISTORY
 |  Date         Author             Description of Change
 |
 *===========================================================================*/
PROCEDURE derive_invoice_cancel_events (
                       p_invoice_id       IN   NUMBER,
                       p_calling_sequence IN   VARCHAR2)
IS

  /* bug 11663644 added ap_self_assessed_tax_dist_all */
  CURSOR inv_cancel_event_dists IS
    SELECT accounting_date
    FROM ap_invoice_distributions
    WHERE invoice_id = p_invoice_id
    AND awt_invoice_payment_id IS NULL
    AND prepay_distribution_id IS NULL --prepay_tax_parent_id obsoleted
    AND accounting_event_id IS NULL
    AND cancellation_flag = 'Y'
    GROUP BY accounting_date    
    UNION 
    SELECT accounting_date
    FROM ap_self_assessed_tax_dist_all
    WHERE invoice_id = p_invoice_id
    AND awt_invoice_payment_id IS NULL
    AND prepay_distribution_id IS NULL --prepay_tax_parent_id obsoleted
    AND accounting_event_id IS NULL
    AND cancellation_flag = 'Y'
    GROUP BY accounting_date
    ORDER BY accounting_date;

  /* Bug 11663644  removed cursors no_action_prepay_dists, 
  non_prepay_dist_grp_count and all_unaccounted_dists_cur
  as they are not referred anywhere*/

  TYPE t_accounting_event_dates IS TABLE OF DATE INDEX BY PLS_INTEGER;
  l_accounting_event_dates t_accounting_event_dates;
  l_dist_gl_date_list    t_accounting_event_dates;

  TYPE t_accounting_event_ids IS TABLE OF NUMBER INDEX BY PLS_INTEGER;
  l_accounting_event_ids t_accounting_event_ids;
  l_unaccounted_event_id_list   t_accounting_event_ids;


  l_event_type VARCHAR2(30);
  l_event_class VARCHAR2(30);
  l_accounting_event_id NUMBER; --bug 4352723
  l_processed_dists NUMBER;
  l_dist_dates NUMBER;

  l_legal_entity_id NUMBER(15);
  l_ledger_id NUMBER(15);
  l_org_id NUMBER(15);
  l_event_security_context XLA_EVENTS_PUB_PKG.T_SECURITY;
  l_event_source_info XLA_EVENTS_PUB_PKG.T_EVENT_SOURCE_INFO;
  l_diff_gl_date_dists NUMBER := 0;
  l_transaction_date   AP_INVOICES_ALL.invoice_date%TYPE;

  l_curr_calling_sequence VARCHAR2(2000);

  -- Logging:
  l_procedure_name CONSTANT VARCHAR2(30) := 'DERIVE_INVOICE_CANCEL_EVENT';
  l_log_msg        FND_LOG_MESSAGES.MESSAGE_TEXT%TYPE;

BEGIN

  l_curr_calling_sequence := p_calling_sequence ||
          ' -> AP_ACCOUNTING_EVENTS_PKG.DERIVE_INVOICE_CANCEL_EVENT';

  G_CURRENT_RUNTIME_LEVEL := FND_LOG.G_CURRENT_RUNTIME_LEVEL;

  l_log_msg := 'Begin of procedure '||l_procedure_name;
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
     FND_LOG.STRING(G_LEVEL_PROCEDURE,
                    G_MODULE_NAME || l_procedure_name,
                    l_log_msg);
  END IF;

  SELECT DECODE
    ( AI.invoice_type_lookup_code,
      'CREDIT', CREDIT_MEMO_CANCELLED_TYPE,
      'DEBIT', DEBIT_MEMO_CANCELLED_TYPE,
      'PREPAYMENT', PREPAYMENT_CANCELLED_TYPE,
      INVOICE_CANCELLED_TYPE
    ) event_type,
    DECODE
    ( AI.invoice_type_lookup_code,
      'CREDIT', CREDIT_MEMOS_CLASS,
      'DEBIT', DEBIT_MEMOS_CLASS,
      'PREPAYMENT', PREPAYMENTS_CLASS,
      INVOICES_CLASS
    ) event_class
  INTO l_event_type,
       l_event_class
  FROM ap_invoices_all AI
  WHERE AI.invoice_id = p_invoice_id;

  l_log_msg := 'Before calling procedure create_invoice_event';
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
          FND_LOG.STRING(G_LEVEL_PROCEDURE,
                         G_MODULE_NAME || l_procedure_name,
                         l_log_msg);
  END IF;

  OPEN inv_cancel_event_dists;
  LOOP

    FETCH inv_cancel_event_dists
    BULK COLLECT INTO
      l_accounting_event_dates
    LIMIT 1000;

    FOR i IN 1 ..l_accounting_event_dates.count LOOP
      l_log_msg := 'Before calling procedure create_invoice_event';
      IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
          FND_LOG.STRING(G_LEVEL_PROCEDURE,
                         G_MODULE_NAME || l_procedure_name,
                         l_log_msg);
      END IF;

      l_accounting_event_ids(i) :=
        create_invoice_event
        ( p_event_type => l_event_type,
          p_invoice_id => p_invoice_id,
          p_event_date => l_accounting_event_dates(i),
          p_calling_sequence => l_curr_calling_sequence
        );

      l_log_msg := 'After calling procedure create_invoice_event';
      IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
         FND_LOG.STRING(G_LEVEL_PROCEDURE,
                        G_MODULE_NAME || l_procedure_name,
                        l_log_msg);
      END IF;

    END LOOP;

    FORALL i IN 1 .. l_accounting_event_dates.count
    UPDATE ap_invoice_distributions
    SET accounting_event_id = l_accounting_event_ids(i)
    WHERE invoice_id = p_invoice_id
    AND accounting_date = l_accounting_event_dates(i)
    AND awt_invoice_payment_id IS NULL
    AND line_type_lookup_code <> 'PREPAY'
    AND prepay_distribution_id IS NULL --prepay_tax_parent_id obsoleted
    AND accounting_event_id IS NULL
    AND cancellation_flag = 'Y';

    --------------------------------------------------
    -- bug 5525657
    -- We need to stamp the accounting event id for
    -- self assessed tax distributions
    --------------------------------------------------

    FORALL i IN 1 .. l_accounting_event_dates.count
    UPDATE ap_self_assessed_tax_dist_all
    SET accounting_event_id = l_accounting_event_ids(i)
    WHERE invoice_id = p_invoice_id
    AND accounting_date = l_accounting_event_dates(i)
    AND awt_invoice_payment_id IS NULL
    AND line_type_lookup_code <> 'PREPAY'
    AND prepay_distribution_id IS NULL --prepay_tax_parent_id obsoleted
    AND accounting_event_id IS NULL
    AND cancellation_flag = 'Y';

    EXIT WHEN inv_cancel_event_dists%NOTFOUND;
  END LOOP;
  CLOSE inv_cancel_event_dists;

  -- Bug 4927664 Remove the complete logic trying to figure out
  -- if we need to update events to No Action.

  l_log_msg :='End of procedure '||l_procedure_name;
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
     FND_LOG.STRING(G_LEVEL_PROCEDURE,
                    G_MODULE_NAME || l_procedure_name,
                    l_log_msg);
  END IF;

EXCEPTION
  WHEN OTHERS THEN
    IF (inv_cancel_event_dists%ISOPEN) THEN
      CLOSE inv_cancel_event_dists;
    END IF;  
    IF (SQLCODE <> -20001) THEN
       FND_MESSAGE.SET_NAME('SQLAP','AP_DEBUG');
       FND_MESSAGE.SET_TOKEN('ERROR',SQLERRM);
       FND_MESSAGE.SET_TOKEN('CALLING_SEQUENCE',
                    l_curr_calling_sequence);
       FND_MESSAGE.SET_TOKEN('PARAMETERS',
               'p_invoice_id = '||p_invoice_id);
       FND_MESSAGE.SET_TOKEN('DEBUG_INFO',l_log_msg);
    END IF;
    APP_EXCEPTION.RAISE_EXCEPTION();
END derive_invoice_cancel_events;

/*============================================================================
 |  PROCEDURE  -  NO_ACTION_PMT_EVENT_UPDATE (PRIVATE)
 |
 |  DESCRIPTION
 |    This procedure is used to create 'No Action' event in SLA.'No Action'
 |    status indicate that it is not necessary to create any accounting for a
 |    given event. 'No Action' event will be picked up by the SLA accounting
 |    process, but no accounting lines will be created.
 |
 |  PRAMETERS
 |          p_check_id: Check ID
 |          p_event_type_code: Event Type
 |          p_accounting_date: Accounting date
 |          p_accounting_event_id: Accounting event whose status will be
 |                    stamped as 'No Action'
 |          p_calling_sequence: Debug information
 |
 |  KNOWN ISSUES:
 |
 |  NOTES:
 |
 |  MODIFICATION HISTORY
 |  Date         Author             Description of Change
 |
 *===========================================================================*/
PROCEDURE no_action_pmt_event_update(
                 p_check_id            IN   NUMBER,
                 p_event_type_code     IN   VARCHAR2,
                 p_accounting_date     IN   DATE,
                 p_accounting_event_id IN   NUMBER,
                 p_calling_sequence    IN   VARCHAR2)
IS

  -- Bug 4748638
  CURSOR aip_event_id_count IS
  SELECT accounting_event_id
  FROM   ap_invoice_payments
  WHERE  check_id = p_check_id
  GROUP BY accounting_event_id;

  TYPE event_id_tab_Type IS TABLE OF
       ap_invoice_payments.accounting_event_id%TYPE;
  l_event_id_list        event_id_tab_Type;

  l_legal_entity_id NUMBER(15);
  l_ledger_id NUMBER(15);
  l_org_id NUMBER(15);
  l_event_security_context XLA_EVENTS_PUB_PKG.T_SECURITY;
  l_event_source_info XLA_EVENTS_PUB_PKG.T_EVENT_SOURCE_INFO;

  l_processed_events NUMBER;
  l_event_count NUMBER;
  l_accounting_date DATE;
  l_event_class VARCHAR2(30);

  l_curr_calling_sequence VARCHAR2(2000);

  -- Logging:
  l_procedure_name CONSTANT VARCHAR2(30) := 'NO_ACTION_PMT_EVENT_UPDATE';
  l_log_msg        FND_LOG_MESSAGES.MESSAGE_TEXT%TYPE;

BEGIN

  l_curr_calling_sequence := p_calling_sequence ||
         ' -> AP_ACCOUNTING_EVENTS_PKG.NO_ACTION_PMT_EVENT_UPDATE';

  G_CURRENT_RUNTIME_LEVEL := FND_LOG.G_CURRENT_RUNTIME_LEVEL;

  l_log_msg :='Begin of procedure '||l_procedure_name;
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
     FND_LOG.STRING(G_LEVEL_PROCEDURE,
                    G_MODULE_NAME || l_procedure_name,
                    l_log_msg);
  END IF;

  l_log_msg :='Before calling procedure get_payment_info';
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
     FND_LOG.STRING(G_LEVEL_PROCEDURE,
                    G_MODULE_NAME || l_procedure_name,
                    l_log_msg);
  END IF;

  get_payment_info
  ( p_check_id => p_check_id,
    p_org_id => l_org_id, -- OUT
    p_legal_entity_id => l_legal_entity_id, -- OUT
    p_ledger_id => l_ledger_id, -- OUT
    p_calling_sequence => l_curr_calling_sequence
  );

  l_log_msg :='Procedure get_payment_info executed';
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
     FND_LOG.STRING(G_LEVEL_PROCEDURE,
                    G_MODULE_NAME || l_procedure_name,
                    l_log_msg);
  END IF;

  l_log_msg :='Before calling procedure get_event_security_context';
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
     FND_LOG.STRING(G_LEVEL_PROCEDURE,
                    G_MODULE_NAME || l_procedure_name,
                    l_log_msg);
  END IF;

  l_event_security_context :=
    get_event_security_context
    ( p_org_id => l_org_id,
      p_calling_sequence => l_curr_calling_sequence
    );

  l_log_msg :='After calling procedure get_event_security_context';
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
     FND_LOG.STRING(G_LEVEL_PROCEDURE,
                    G_MODULE_NAME || l_procedure_name,
                    l_log_msg);
  END IF;

  l_log_msg :='Before calling procedure get_payment_event_source_info';
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
     FND_LOG.STRING(G_LEVEL_PROCEDURE,
                    G_MODULE_NAME || l_procedure_name,
                    l_log_msg);
  END IF;

  l_event_source_info :=
    get_payment_event_source_info
    ( p_legal_entity_id => l_legal_entity_id,
      p_ledger_id => l_ledger_id,
      p_check_id => p_check_id,
      p_calling_sequence => l_curr_calling_sequence
    );

  l_log_msg :='After calling procedure get_payment_event_source_info';
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
     FND_LOG.STRING(G_LEVEL_PROCEDURE,
                    G_MODULE_NAME || l_procedure_name,
                    l_log_msg);
  END IF;

  IF (p_event_type_code IN (PAYMENT_CANCELLED_TYPE, REFUND_CANCELLED_TYPE)) THEN
    IF (p_event_type_code = PAYMENT_CANCELLED_TYPE) THEN
      l_event_class := PAYMENTS_CLASS;
    ELSE
      l_event_class := REFUNDS_CLASS;
    END IF;

    SELECT COUNT(*)
    INTO   l_processed_events
    FROM   ap_invoice_payments
    WHERE  posted_flag = 'Y'
    AND    check_id = p_check_id;

    IF (l_processed_events = 0) THEN

      -- If there is more that one event for the payment, it implies that the
      -- payment has been adjusted, and we do not want to update events to
      -- No Action

       OPEN  aip_event_id_count;
       FETCH  aip_event_id_count
       BULK COLLECT INTO l_event_id_list;
       CLOSE  aip_event_id_count;

      IF (l_event_id_list.count <= 1) THEN

        SELECT check_date
        INTO   l_accounting_date
        FROM   ap_checks
        WHERE  check_id = p_check_id;

        IF (l_accounting_date = p_accounting_date) THEN
          -- p_event_type_code is left NULL so that 'PAYMENT CREATED' and
          -- 'PAYMENT CANCELLED' events or 'REFUND CREATED' and
          -- 'REFUND CANCELLED' events will be updated.

          l_log_msg :='Before calling P_XLA_EVENTS_PKG.UPDATE_EVENT_STATUS';
          IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
             FND_LOG.STRING(G_LEVEL_PROCEDURE,
                            G_MODULE_NAME || l_procedure_name,
                            l_log_msg);
          END IF;

          AP_XLA_EVENTS_PKG.UPDATE_EVENT_STATUS
          ( p_event_source_info => l_event_source_info,
            p_event_class_code => l_event_class,
            p_event_type_code => p_event_type_code,
            p_event_date => l_accounting_date,
            p_event_status_code => XLA_EVENTS_PUB_PKG.C_EVENT_NOACTION,
            p_valuation_method => NULL,
            p_security_context => l_event_security_context,
            p_calling_sequence => l_curr_calling_sequence
          );

          l_log_msg :='After calling AP_XLA_EVENTS_PKG.UPDATE_EVENT_STATUS';
          IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
             FND_LOG.STRING(G_LEVEL_PROCEDURE,
                            G_MODULE_NAME || l_procedure_name,
                            l_log_msg);
          END IF;
        END IF;

      END IF;

    END IF;

  ELSIF (p_event_type_code = PAYMENT_UNCLEARED_TYPE) THEN

    /* Get the accounting date of 'clearing' payment history */
    SELECT APH.accounting_date
    INTO   l_accounting_date
    FROM   ap_payment_history APH
    WHERE  APH.payment_history_id =
           (SELECT max(payment_history_id)
            FROM   ap_payment_history APH2
            WHERE  APH2.check_id = p_check_id
            AND    APH2.posted_flag = 'N'
            AND    APH2.transaction_type = 'PAYMENT CLEARING');

    IF (l_accounting_date = p_accounting_date) THEN
      -- p_event_type_code is left NULL so that 'PAYMENT CLEARED' and
      -- 'PAYMENT UNCLEARED' events will be updated.

      l_log_msg :='Before calling AP_XLA_EVENTS_PKG.UPDATE_EVENT_STATUS';
      IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
         FND_LOG.STRING(G_LEVEL_PROCEDURE,
                        G_MODULE_NAME || l_procedure_name,
                        l_log_msg);
      END IF;

      AP_XLA_EVENTS_PKG.UPDATE_EVENT_STATUS
      ( p_event_source_info => l_event_source_info,
        p_event_class_code => RECONCILED_PAYMENTS_CLASS,
        p_event_type_code => p_event_type_code,
        p_event_date => l_accounting_date,
        p_event_status_code => XLA_EVENTS_PUB_PKG.C_EVENT_NOACTION,
        p_valuation_method => NULL,
        p_security_context => l_event_security_context,
        p_calling_sequence => l_curr_calling_sequence
      );

      l_log_msg :='After calling AP_XLA_EVENTS_PKG.UPDATE_EVENT_STATUS';
      IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
         FND_LOG.STRING(G_LEVEL_PROCEDURE,
                        G_MODULE_NAME || l_procedure_name,
                        l_log_msg);
      END IF;
    END IF;

  ELSIF (p_event_type_code = PAYMENT_MATURITY_REVERSED_TYPE) THEN

    SELECT APH.accounting_date
    INTO   l_accounting_date
    FROM   ap_payment_history APH
    WHERE  payment_history_id =
           (SELECT max(payment_history_id)
            FROM   ap_payment_history APH2
            WHERE  APH2.check_id = p_check_id
            AND    APH2.posted_flag = 'N'
            AND    APH2.transaction_type = 'PAYMENT MATURITY');


    IF (l_accounting_date = p_accounting_date) THEN

      l_log_msg :='Before calling AP_XLA_EVENTS_PKG.UPDATE_EVENT_STATUS';
      IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
         FND_LOG.STRING(G_LEVEL_PROCEDURE,
                        G_MODULE_NAME || l_procedure_name,
                        l_log_msg);
      END IF;

      AP_XLA_EVENTS_PKG.UPDATE_EVENT_STATUS
      ( p_event_source_info => l_event_source_info,
        p_event_class_code => FUTURE_DATED_PAYMENTS_CLASS,
        p_event_type_code => p_event_type_code,
        p_event_date => l_accounting_date,
        p_event_status_code => XLA_EVENTS_PUB_PKG.C_EVENT_NOACTION,
        p_valuation_method => NULL,
        p_security_context => l_event_security_context,
        p_calling_sequence => l_curr_calling_sequence
      );

     l_log_msg :='After calling AP_XLA_EVENTS_PKG.UPDATE_EVENT_STATUS';
     IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
        FND_LOG.STRING(G_LEVEL_PROCEDURE,
                       G_MODULE_NAME || l_procedure_name,
                       l_log_msg);
     END IF;
    END IF;

  END IF;

 l_log_msg :='End of procedure '||l_procedure_name;
 IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
     FND_LOG.STRING(G_LEVEL_PROCEDURE,
                    G_MODULE_NAME || l_procedure_name,
                    l_log_msg);
 END IF;

EXCEPTION
  WHEN OTHERS THEN
    IF (SQLCODE <> -20001) THEN
       FND_MESSAGE.SET_NAME('SQLAP','AP_DEBUG');
       FND_MESSAGE.SET_TOKEN('ERROR',SQLERRM);
       FND_MESSAGE.SET_TOKEN('CALLING_SEQUENCE',
                    l_curr_calling_sequence);
       FND_MESSAGE.SET_TOKEN('PARAMETERS',
                'p_check_id = '||p_check_id
            ||', p_event_type_code = '||p_event_type_code
            ||', p_accounting_date = '||p_accounting_date
            ||', p_accounting_event_id = '||p_accounting_event_id);
       FND_MESSAGE.SET_TOKEN('DEBUG_INFO',l_log_msg);
    END IF;
    APP_EXCEPTION.RAISE_EXCEPTION();
END no_action_pmt_event_update;


/*============================================================================
 |  PROCEDURE  -  MULTI_ORG_EVENTS_SWEEP(PUBLIC)
 |
 |  DESCRIPTION
 |      This procedure is called by APXTRSWP.rdf (UPDATE_ACCTG_DATES).This
 |      procedure is used to sweep accounting events from one accounting period
 |      to another.
 |
 |  PRAMETERS
 |         p_ledger_id: Current ledger ID
 |         p_period_name: Old accounting period
 |         p_from_date: The start date of sweeping
 |         p_to_date: The end date of the sweeping
 |         p_sweep_to_date: The new event date
 |         p_calling_sequence: Debug information
 |
 |  KNOWN ISSUES:
 |
 |  NOTES:
 |
 |  MODIFICATION HISTORY
 |  Date         Author             Description of Change
 |
 *===========================================================================*/
PROCEDURE multi_org_events_sweep (
               p_ledger_id        IN    NUMBER,
               p_period_name      IN    VARCHAR2,
               p_from_date        IN    DATE,
               p_to_date          IN    DATE,
               p_sweep_to_date    IN    DATE,
               p_calling_sequence IN    VARCHAR2 )
IS

  TYPE t_event_ids IS TABLE OF NUMBER INDEX BY PLS_INTEGER;
  TYPE t_invoice_ids IS TABLE OF NUMBER(15) INDEX BY PLS_INTEGER;
  TYPE t_check_ids IS TABLE OF NUMBER(15) INDEX BY PLS_INTEGER;
  TYPE t_org_ids IS TABLE OF NUMBER(15) INDEX BY PLS_INTEGER;
  TYPE t_legal_entity_ids IS TABLE OF NUMBER(15) INDEX BY PLS_INTEGER;
  TYPE t_ledger_ids IS TABLE OF NUMBER(15) INDEX BY PLS_INTEGER;

  l_event_ids t_event_ids;
  l_invoice_ids t_invoice_ids;
  l_check_ids t_check_ids;
  l_org_ids t_org_ids;
  l_legal_entity_ids t_legal_entity_ids;
  l_ledger_ids t_ledger_ids;
  l_event_security_context XLA_EVENTS_PUB_PKG.T_SECURITY;
  l_event_source_info XLA_EVENTS_PUB_PKG.T_EVENT_SOURCE_INFO;

  CURSOR l_aid_events_mo_cur_period IS
    SELECT AID.accounting_event_id,
           AID.invoice_id,
           AID.org_id,
           AI.legal_entity_id,
           AID.set_of_books_id ledger_id
    FROM ap_invoice_distributions_all AID,
         ap_invoices_all AI
    WHERE AP_UTILITIES_PKG.GET_CURRENT_GL_DATE(TRUNC(AID.accounting_date), aid.org_id) --bug5956469
                   = p_period_name
    AND aid.posted_flag IN ('N', 'S') -- Bug 6869699
    AND aid.org_id = ai.org_id
    AND AID.org_id IN
        ( SELECT ASP.org_id
          FROM hr_organization_information OI,
               --hr_all_organization_units_tl LE,  --bug6392886
               hr_all_organization_units_tl OU,
               ap_system_parameters_all ASP
          WHERE ASP.org_id = OI.organization_id
          AND   OU.organization_id = OI.organization_id
          AND OI.org_information_context = 'Operating Unit Information'
          AND DECODE(LTRIM(OI.org_information3, '0123456789'), NULL,
                    TO_NUMBER(OI.org_information3), NULL) = p_ledger_id
          --bug6392886
          /*AND DECODE(LTRIM(OI.org_information2, '0123456789'), NULL,
                    TO_NUMBER(OI.org_information2), NULL) = LE.organization_id*/
          AND OU.organization_id = OI.organization_id
          AND OU.language = USERENV('LANG')
          --AND LE.language = USERENV('LANG')
        ) AND AID.invoice_id = AI.invoice_id
          AND AID.accounting_event_id is not NULL;--Bug6320053

  CURSOR l_aid_events_mo_cur_no_period IS
    SELECT AID.accounting_event_id,
           AID.invoice_id,
           AID.org_id,
           AI.legal_entity_id,
           AID.set_of_books_id ledger_id
    FROM ap_invoice_distributions_all AID,
         ap_invoices_all AI
    WHERE AID.accounting_date BETWEEN p_from_date AND p_to_date
    AND   aid.posted_flag IN ('N', 'S') -- Bug 6869699
    AND   aid.org_id = ai.org_id
    AND AID.org_id IN
        ( SELECT ASP.org_id
          FROM hr_organization_information OI,
               --hr_all_organization_units_tl LE,  --bug6392886
               hr_all_organization_units_tl OU,
               ap_system_parameters_all ASP
          WHERE ASP.org_id = OI.organization_id
          AND   OU.organization_id = OI.organization_id
          AND OI.org_information_context = 'Operating Unit Information'
          AND DECODE(LTRIM(OI.org_information3, '0123456789'), NULL,
                    TO_NUMBER(OI.org_information3), NULL) = p_ledger_id
          --bug6392886
          /*AND DECODE(LTRIM(OI.org_information2, '0123456789'), NULL,
                    TO_NUMBER(OI.org_information2), NULL) = LE.organization_id*/
          AND OU.organization_id = OI.organization_id
          AND OU.language = USERENV('LANG')
          --AND LE.language = USERENV('LANG')
        ) AND AID.invoice_id = AI.invoice_id
     AND AID.accounting_event_id is not NULL ;--Bug6320053

  CURSOR l_aph_events_mo_cur_period IS
    SELECT APH.accounting_event_id,
      APH.check_id check_id,
      APH.org_id,
      AC.legal_entity_id,
      ( SELECT AIP.set_of_books_id
        FROM ap_invoice_payments_all AIP
        WHERE AIP.check_id = APH.check_id
        AND ROWNUM = 1
      ) ledger_id
    FROM
      ap_payment_history_all APH,
      ap_checks_all AC
    WHERE
      APH.check_id = AC.check_id
      AND AP_UTILITIES_PKG.GET_CURRENT_GL_DATE(TRUNC(APH.accounting_date), aph.org_id) --bug5956469
          = p_period_name
      AND aph.posted_flag IN ('N', 'S') -- Bug 6869699
      and ac.org_id = aph.org_id
      AND APH.org_id IN
        ( SELECT ASP.org_id
          FROM hr_organization_information OI,
               --hr_all_organization_units_tl LE,  --bug6392886
               hr_all_organization_units_tl OU,
               ap_system_parameters_all ASP
          WHERE ASP.org_id = OI.organization_id
          AND   OU.organization_id = OI.organization_id
          AND OI.org_information_context = 'Operating Unit Information'
          AND DECODE(LTRIM(OI.org_information3,'0123456789'), NULL,
                     TO_NUMBER(OI.org_information3), NULL) = p_ledger_id
          --bug6392886
          /*AND DECODE(LTRIM(OI.org_information2,'0123456789'), NULL,
                  TO_NUMBER(OI.org_information2), NULL) = LE.organization_id */
          AND OU.organization_id = OI.organization_id
          AND OU.language = USERENV('LANG')
          --AND LE.language = USERENV('LANG')
        )
         AND APH.accounting_event_id is not NULL ;--Bug6320053

  CURSOR l_aph_events_mo_cur_no_period IS
    SELECT APH.accounting_event_id,
      APH.check_id check_id,
      APH.org_id,
      AC.legal_entity_id,
      ( SELECT AIP.set_of_books_id
        FROM ap_invoice_payments_all AIP
        WHERE AIP.check_id = APH.check_id
        AND ROWNUM = 1
      ) ledger_id
    FROM
      ap_payment_history_all APH,
      ap_checks_all AC
    WHERE
      APH.check_id = AC.check_id
      AND APH.accounting_date BETWEEN p_from_date AND p_to_date
      AND aph.posted_flag IN ('N', 'S') -- Bug 6869699
      and ac.org_id = aph.org_id
      AND APH.org_id IN
        ( SELECT ASP.org_id
          FROM hr_organization_information OI,
               --hr_all_organization_units_tl LE,  bug6392886
               hr_all_organization_units_tl OU,
               ap_system_parameters_all ASP
          WHERE ASP.org_id = OI.organization_id
          AND   OU.organization_id = OI.organization_id
          AND OI.org_information_context = 'Operating Unit Information'
          AND DECODE(LTRIM(OI.org_information3,'0123456789'), NULL,
                     TO_NUMBER(OI.org_information3), NULL) = p_ledger_id
          --bug6392886
          /*AND DECODE(LTRIM(OI.org_information2,'0123456789'), NULL,
                  TO_NUMBER(OI.org_information2), NULL) = LE.organization_id*/
          AND OU.organization_id = OI.organization_id
          AND OU.language = USERENV('LANG')
          --AND LE.language = USERENV('LANG')
        )
    AND APH.accounting_event_id is not NULL ;--Bug6320053

  l_curr_calling_sequence VARCHAR2(2000);

  -- Logging:
  l_procedure_name CONSTANT VARCHAR2(30) := 'MULTI_ORG_EVENTS_SWEEP';
  l_log_msg        FND_LOG_MESSAGES.MESSAGE_TEXT%TYPE;

BEGIN

  l_curr_calling_sequence :=
   p_calling_sequence || ' -> AP_ACCOUNTING_EVENTS_PKG.MULTI_ORG_EVENTS_SWEEP';

  G_CURRENT_RUNTIME_LEVEL := FND_LOG.G_CURRENT_RUNTIME_LEVEL;

  l_log_msg :='Begin of procedure '||l_procedure_name;
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
     FND_LOG.STRING(G_LEVEL_PROCEDURE,
                    G_MODULE_NAME || l_procedure_name,
                    l_log_msg);
  END IF;

  if (p_period_name is null) then
     OPEN l_aid_events_mo_cur_no_period;
  else
     OPEN l_aid_events_mo_cur_period;
  end if;

  LOOP

    if (p_period_name is null) then
       FETCH l_aid_events_mo_cur_no_period
       BULK COLLECT INTO
         l_event_ids,
         l_invoice_ids,
         l_org_ids,
         l_legal_entity_ids,
         l_ledger_ids
       LIMIT 1000;
    else
       FETCH l_aid_events_mo_cur_period
       BULK COLLECT INTO
         l_event_ids,
         l_invoice_ids,
         l_org_ids,
         l_legal_entity_ids,
         l_ledger_ids
       LIMIT 1000;
    end if;

    FOR i IN 1 .. l_event_ids.count LOOP
      l_log_msg :='Before calling procedure get_event_security_context';
      IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
         FND_LOG.STRING(G_LEVEL_PROCEDURE,
                        G_MODULE_NAME || l_procedure_name,
                        l_log_msg);
      END IF;

      l_event_security_context :=
        get_event_security_context
        ( p_org_id => l_org_ids(i),
          p_calling_sequence => l_curr_calling_sequence
        );

      l_log_msg :='After calling procedure get_event_security_context';
      IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
         FND_LOG.STRING(G_LEVEL_PROCEDURE,
                        G_MODULE_NAME || l_procedure_name,
                        l_log_msg);
      END IF;

      l_log_msg :='Before calling procedure get_invoice_event_source_info';
      IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
         FND_LOG.STRING(G_LEVEL_PROCEDURE,
                        G_MODULE_NAME || l_procedure_name,
                        l_log_msg);
      END IF;

      l_event_source_info :=
        get_invoice_event_source_info
        ( p_legal_entity_id => l_legal_entity_ids(i),
          p_ledger_id => l_ledger_ids(i),
          p_invoice_id => l_invoice_ids(i),
          p_calling_sequence => l_curr_calling_sequence
        );

      l_log_msg :='After calling procedure get_invoice_event_source_info';
      IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
         FND_LOG.STRING(G_LEVEL_PROCEDURE,
                        G_MODULE_NAME || l_procedure_name,
                        l_log_msg);
      END IF;

      l_log_msg :='Before calling procedure AP_XLA_EVENTS_PKG.UPDATE_EVENT';
      IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
         FND_LOG.STRING(G_LEVEL_PROCEDURE,
                        G_MODULE_NAME || l_procedure_name,
                        l_log_msg);
      END IF;

      AP_XLA_EVENTS_PKG.UPDATE_EVENT
      ( p_event_source_info => l_event_source_info,
        p_event_id => l_event_ids(i),
        p_event_type_code => NULL,
        p_event_date => p_sweep_to_date,
        p_event_status_code => NULL,
        p_valuation_method => NULL,
        p_security_context => l_event_security_context,
        p_calling_sequence => l_curr_calling_sequence
      );

     --Bug 6874970
      UPDATE xla_ae_headers aeh
         SET aeh.accounting_date = p_sweep_to_date,
             aeh.period_name = AP_UTILITIES_PKG.get_gl_period_name(
                                                    p_sweep_to_date,
                                                    l_org_ids(i)),
             last_update_date = SYSDATE,
             last_updated_by =  FND_GLOBAL.user_id
       WHERE aeh.event_id = l_event_ids(i)
         AND application_id = 200
         AND gl_transfer_status_code <> 'Y';

      UPDATE xla_ae_lines ael
         SET ael.accounting_date = p_sweep_to_date,
             last_update_date = sysdate,
             last_updated_by =  FND_GLOBAL.user_id
       WHERE ael.ae_header_id in (
            SELECT aeh.ae_header_id
              FROM xla_ae_headers aeh
             WHERE aeh.event_id = l_event_ids(i)
               AND aeh.application_id = 200
               AND aeh.gl_transfer_status_code <> 'Y');

     l_log_msg :='After calling procedure AP_XLA_EVENTS_PKG.UPDATE_EVENT';
     IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
        FND_LOG.STRING(G_LEVEL_PROCEDURE,
                       G_MODULE_NAME || l_procedure_name,
                       l_log_msg);
     END IF;

    END LOOP;

    if (p_period_name is null) then
       EXIT WHEN l_aid_events_mo_cur_no_period%NOTFOUND;
    else
       EXIT WHEN l_aid_events_mo_cur_period%NOTFOUND;
    end if;

  END LOOP;

  if (p_period_name is null) then
     CLOSE l_aid_events_mo_cur_no_period;

     OPEN l_aph_events_mo_cur_no_period;
  else
     CLOSE l_aid_events_mo_cur_period;

     OPEN l_aph_events_mo_cur_period;
  end if;

  LOOP

    if (p_period_name is null) then
       FETCH l_aph_events_mo_cur_no_period
       BULK COLLECT INTO
         l_event_ids,
         l_check_ids,
         l_org_ids,
         l_legal_entity_ids,
         l_ledger_ids
       LIMIT 1000;
    else
       FETCH l_aph_events_mo_cur_period
       BULK COLLECT INTO
         l_event_ids,
         l_check_ids,
         l_org_ids,
         l_legal_entity_ids,
         l_ledger_ids
       LIMIT 1000;
    end if;

    FOR i IN 1 .. l_event_ids.count LOOP


      l_event_security_context :=
        get_event_security_context
        ( p_org_id => l_org_ids(i),
          p_calling_sequence => l_curr_calling_sequence
        );

      l_event_source_info :=
        get_payment_event_source_info
        ( p_legal_entity_id => l_legal_entity_ids(i),
          p_ledger_id => l_ledger_ids(i),
          p_check_id => l_check_ids(i),
          p_calling_sequence => l_curr_calling_sequence
        );

      l_log_msg :='Before calling procedure AP_XLA_EVENTS_PKG.UPDATE_EVENT';
      IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
         FND_LOG.STRING(G_LEVEL_PROCEDURE,
                        G_MODULE_NAME || l_procedure_name,
                        l_log_msg);
      END IF;

      AP_XLA_EVENTS_PKG.UPDATE_EVENT
      ( p_event_source_info => l_event_source_info,
        p_event_id => l_event_ids(i),
        p_event_type_code => NULL,
        p_event_date => p_sweep_to_date,
        p_event_status_code => NULL,
        p_valuation_method => NULL,
        p_security_context => l_event_security_context,
        p_calling_sequence => l_curr_calling_sequence
      );

     --Bug 6874970
      UPDATE xla_ae_headers aeh
         SET aeh.accounting_date = p_sweep_to_date,
             aeh.period_name = AP_UTILITIES_PKG.get_gl_period_name(
                                                    p_sweep_to_date,
                                                    l_org_ids(i)),
             last_update_date = SYSDATE,
             last_updated_by =  FND_GLOBAL.user_id
       WHERE aeh.event_id = l_event_ids(i)
         AND application_id = 200
         AND gl_transfer_status_code <> 'Y';

      UPDATE xla_ae_lines ael
         SET ael.accounting_date = p_sweep_to_date,
             last_update_date = sysdate,
             last_updated_by =  FND_GLOBAL.user_id
       WHERE ael.ae_header_id in (
            SELECT aeh.ae_header_id
              FROM xla_ae_headers aeh
             WHERE aeh.event_id = l_event_ids(i)
               AND aeh.application_id = 200
               AND aeh.gl_transfer_status_code <> 'Y');


      l_log_msg :='After calling procedure AP_XLA_EVENTS_PKG.UPDATE_EVENT';
      IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
         FND_LOG.STRING(G_LEVEL_PROCEDURE,
                        G_MODULE_NAME || l_procedure_name,
                        l_log_msg);
      END IF;
    END LOOP;

    if (p_period_name is null) then
       EXIT WHEN l_aph_events_mo_cur_no_period%NOTFOUND;
    else
       EXIT WHEN l_aph_events_mo_cur_period%NOTFOUND;
    end if;

  END LOOP;

  if (p_period_name is null) then
     CLOSE l_aph_events_mo_cur_no_period;
  else
     CLOSE l_aph_events_mo_cur_period;
  end if;

  l_log_msg :='End of procedure '||l_procedure_name;
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
     FND_LOG.STRING(G_LEVEL_PROCEDURE,
                    G_MODULE_NAME || l_procedure_name,
                    l_log_msg);
  END IF;

EXCEPTION
  WHEN OTHERS THEN

    if (p_period_name is null) then
       IF (l_aid_events_mo_cur_no_period%ISOPEN) THEN
         CLOSE l_aid_events_mo_cur_no_period;
       END IF;

       IF (l_aph_events_mo_cur_no_period%ISOPEN) THEN
         CLOSE l_aph_events_mo_cur_no_period;
       END IF;
    else
       IF (l_aid_events_mo_cur_period%ISOPEN) THEN
         CLOSE l_aid_events_mo_cur_period;
       END IF;

       IF (l_aph_events_mo_cur_period%ISOPEN) THEN
         CLOSE l_aph_events_mo_cur_period;
       END IF;
    end if;

    IF (SQLCODE <> -20001) THEN
       FND_MESSAGE.SET_NAME('SQLAP','AP_DEBUG');
       FND_MESSAGE.SET_TOKEN('ERROR',SQLERRM);
       FND_MESSAGE.SET_TOKEN('CALLING_SEQUENCE',
                    l_curr_calling_sequence);
       FND_MESSAGE.SET_TOKEN('PARAMETERS',
                'p_ledger_id = '||p_ledger_id
            ||', p_period_name = '||p_period_name
            ||', p_from_date = '||p_from_date
            ||', p_to_date = '||p_to_date
            ||', p_sweep_to_date = '||p_sweep_to_date);
       FND_MESSAGE.SET_TOKEN('DEBUG_INFO',l_log_msg);
    END IF;
    APP_EXCEPTION.RAISE_EXCEPTION();
END multi_org_events_sweep;

/*============================================================================
 |  PROCEDURE  -  SINGLE_ORG_EVENTS_SWEEP(PUBLIC)
 |
 |  DESCRIPTION
 |      This procedure is called by APXTRSWP.rdf (UPDATE_ACCTG_DATES).This
 |      procedure is used to sweep accounting events from one accounting period
 |      to another.
 |
 |  PRAMETERS:
 |         p_period_name: Old period's name
 |         p_from_date: The start date to sweep
 |         p_to_date: The end date to sweep
 |         p_sweep_to_date: New event date
 |         p_calling_sequence: Debug information
 |
 |  KNOWN ISSUES:
 |
 |  NOTES:
 |
 |  MODIFICATION HISTORY
 |  Date         Author             Description of Change
 |
 *===========================================================================*/
PROCEDURE single_org_events_sweep(
             p_period_name      IN    VARCHAR2,
             p_from_date        IN    DATE,
             p_to_date          IN    DATE,
             p_sweep_to_date    IN    DATE,
             p_calling_sequence IN    VARCHAR2)
IS

  TYPE t_event_ids IS TABLE OF NUMBER INDEX BY PLS_INTEGER;
  TYPE t_invoice_ids IS TABLE OF NUMBER(15) INDEX BY PLS_INTEGER;
  TYPE t_check_ids IS TABLE OF NUMBER(15) INDEX BY PLS_INTEGER;
  TYPE t_org_ids IS TABLE OF NUMBER(15) INDEX BY PLS_INTEGER;
  TYPE t_legal_entity_ids IS TABLE OF NUMBER(15) INDEX BY PLS_INTEGER;
  TYPE t_ledger_ids IS TABLE OF NUMBER(15) INDEX BY PLS_INTEGER;

  l_event_ids t_event_ids;
  l_invoice_ids t_invoice_ids;
  l_check_ids t_check_ids;
  l_org_ids t_org_ids;
  l_legal_entity_ids t_legal_entity_ids;
  l_ledger_ids t_ledger_ids;
  l_event_security_context XLA_EVENTS_PUB_PKG.T_SECURITY;
  l_event_source_info XLA_EVENTS_PUB_PKG.T_EVENT_SOURCE_INFO;

  CURSOR l_aid_events_cur IS
    SELECT AID.accounting_event_id,
           AID.invoice_id,
           AID.org_id,
           AI.legal_entity_id,
           AID.set_of_books_id ledger_id
    FROM ap_invoice_distributions AID,
         ap_invoices AI
    WHERE ( ( p_period_name IS NULL AND
          AID.accounting_date BETWEEN p_from_date AND p_to_date)
        OR
        ( p_period_name IS NOT NULL AND
          AP_UTILITIES_PKG.GET_CURRENT_GL_DATE(TRUNC(AID.accounting_date), aid.org_id) --bug5956469
              = p_period_name
        )
      )
      AND AID.posted_flag <> 'Y'
      AND AID.invoice_id = AI.invoice_id
      AND AID.accounting_event_id is not NULL;--Bug6320053

  CURSOR l_aph_events_cur_period IS
    SELECT APH.accounting_event_id,
           APH.check_id,
           APH.org_id,
           AC.legal_entity_id,
      ( SELECT AIP.set_of_books_id
        FROM ap_invoice_payments_all AIP
        WHERE AIP.check_id = APH.check_id
        AND ROWNUM = 1
      ) ledger_id
    FROM ap_payment_history_all APH,
      ap_checks_all AC
    WHERE APH.check_id = AC.check_id
    AND   APH.org_id = AC.org_id
    AND   AP_UTILITIES_PKG.GET_CURRENT_GL_DATE(TRUNC(APH.accounting_date), aph.org_id) --bug5956469
                            = p_period_name
    AND   aph.posted_flag IN ('N', 'S') -- Bug 6869699
    AND   APH.accounting_event_id is not NULL;--Bug6320053

  CURSOR l_aph_events_cur_no_period IS
    SELECT APH.accounting_event_id,
           APH.check_id,
           APH.org_id,
           AC.legal_entity_id,
      ( SELECT AIP.set_of_books_id
        FROM ap_invoice_payments_all AIP
        WHERE AIP.check_id = APH.check_id
        AND ROWNUM = 1
      ) ledger_id
    FROM ap_payment_history_all APH,
      ap_checks_all AC
    WHERE APH.check_id = AC.check_id
    AND   APH.org_id = AC.org_id
    AND   APH.accounting_date BETWEEN p_from_date AND p_to_date
    AND   aph.posted_flag IN ('N', 'S') -- Bug 6869699
    AND   APH.accounting_event_id is not NULL;--Bug6320053

  l_curr_calling_sequence VARCHAR2(2000);

  -- Logging:
  l_procedure_name CONSTANT VARCHAR2(30) := 'SINGLE_ORG_EVENTS_SWEEP';
  l_log_msg        FND_LOG_MESSAGES.MESSAGE_TEXT%TYPE;

BEGIN

  l_curr_calling_sequence := p_calling_sequence
                     || ' -> AP_ACCOUNTING_EVENTS_PKG.SINGLE_ORG_EVENTS_SWEEP';

  G_CURRENT_RUNTIME_LEVEL := FND_LOG.G_CURRENT_RUNTIME_LEVEL;

  l_log_msg :='Begin of procedure '||l_procedure_name;
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
     FND_LOG.STRING(G_LEVEL_PROCEDURE,
                    G_MODULE_NAME || l_procedure_name,
                    l_log_msg);
  END IF;

  OPEN l_aid_events_cur;
  LOOP

    FETCH l_aid_events_cur
    BULK COLLECT INTO
      l_event_ids,
      l_invoice_ids,
      l_org_ids,
      l_legal_entity_ids,
      l_ledger_ids
    LIMIT 1000;

    FOR i IN 1 .. l_event_ids.count LOOP

      l_event_security_context :=
        get_event_security_context
        ( p_org_id => l_org_ids(i),
          p_calling_sequence => l_curr_calling_sequence
        );

      l_event_source_info :=
        get_invoice_event_source_info
        ( p_legal_entity_id => l_legal_entity_ids(i),
          p_ledger_id => l_ledger_ids(i),
          p_invoice_id => l_invoice_ids(i),
          p_calling_sequence => l_curr_calling_sequence
        );

      l_log_msg :='Before calling procedure AP_XLA_EVENTS_PKG.UPDATE_EVENT';
      IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
         FND_LOG.STRING(G_LEVEL_PROCEDURE,
                        G_MODULE_NAME || l_procedure_name,
                        l_log_msg);
      END IF;

      AP_XLA_EVENTS_PKG.UPDATE_EVENT
      ( p_event_source_info => l_event_source_info,
        p_event_id => l_event_ids(i),
        p_event_type_code => NULL,
        p_event_date => p_sweep_to_date,
        p_event_status_code => NULL,
        p_valuation_method => NULL,
        p_security_context => l_event_security_context,
        p_calling_sequence => l_curr_calling_sequence
      );

     --Bug 6874970
      UPDATE xla_ae_headers aeh
         SET aeh.accounting_date = p_sweep_to_date,
             aeh.period_name = AP_UTILITIES_PKG.get_gl_period_name(
                                                    p_sweep_to_date,
                                                    l_org_ids(i)),
             last_update_date = SYSDATE,
             last_updated_by =  FND_GLOBAL.user_id
       WHERE aeh.event_id = l_event_ids(i)
         AND application_id = 200
         AND gl_transfer_status_code <> 'Y';

      UPDATE xla_ae_lines ael
         SET ael.accounting_date = p_sweep_to_date,
             last_update_date = sysdate,
             last_updated_by =  FND_GLOBAL.user_id
       WHERE ael.ae_header_id in (
            SELECT aeh.ae_header_id
              FROM xla_ae_headers aeh
             WHERE aeh.event_id = l_event_ids(i)
               AND aeh.application_id = 200
               AND aeh.gl_transfer_status_code <> 'Y');

     l_log_msg :='After calling procedure AP_XLA_EVENTS_PKG.UPDATE_EVENT';
     IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
        FND_LOG.STRING(G_LEVEL_PROCEDURE,
                       G_MODULE_NAME || l_procedure_name,
                       l_log_msg);
    END IF;

    END LOOP;

    EXIT WHEN l_aid_events_cur%NOTFOUND;
  END LOOP;
  CLOSE l_aid_events_cur;

  if (p_period_name is null) then
     OPEN l_aph_events_cur_no_period;
  else
     OPEN l_aph_events_cur_period;
  end if;

  LOOP

    if (p_period_name is null) then

       FETCH l_aph_events_cur_no_period
       BULK COLLECT INTO
         l_event_ids,
         l_check_ids,
         l_org_ids,
         l_legal_entity_ids,
         l_ledger_ids
       LIMIT 1000;

    else

       FETCH l_aph_events_cur_period
       BULK COLLECT INTO
         l_event_ids,
         l_check_ids,
         l_org_ids,
         l_legal_entity_ids,
         l_ledger_ids
       LIMIT 1000;

    end if;

    FOR i IN 1 .. l_event_ids.count LOOP

      l_event_security_context :=
        get_event_security_context
        ( p_org_id => l_org_ids(i),
          p_calling_sequence => l_curr_calling_sequence
        );

      l_event_source_info :=
        get_payment_event_source_info
        ( p_legal_entity_id => l_legal_entity_ids(i),
          p_ledger_id => l_ledger_ids(i),
          p_check_id => l_check_ids(i),
          p_calling_sequence => l_curr_calling_sequence
        );

      l_log_msg :='After calling procedure AP_XLA_EVENTS_PKG.UPDATE_EVENT';
      IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
         FND_LOG.STRING(G_LEVEL_PROCEDURE,
                        G_MODULE_NAME || l_procedure_name,
                        l_log_msg);
      END IF;

      AP_XLA_EVENTS_PKG.UPDATE_EVENT
      ( p_event_source_info => l_event_source_info,
        p_event_id => l_event_ids(i),
        p_event_type_code => NULL,
        p_event_date => p_sweep_to_date,
        p_event_status_code => NULL,
        p_valuation_method => NULL,
        p_security_context => l_event_security_context,
        p_calling_sequence => l_curr_calling_sequence
      );

     --Bug 6874970
      UPDATE xla_ae_headers aeh
         SET aeh.accounting_date = p_sweep_to_date,
             aeh.period_name = AP_UTILITIES_PKG.get_gl_period_name(
                                                    p_sweep_to_date,
                                                    l_org_ids(i)),
             last_update_date = SYSDATE,
             last_updated_by =  FND_GLOBAL.user_id
       WHERE aeh.event_id = l_event_ids(i)
         AND application_id = 200
         AND gl_transfer_status_code <> 'Y';

      UPDATE xla_ae_lines ael
         SET ael.accounting_date = p_sweep_to_date,
             last_update_date = sysdate,
             last_updated_by =  FND_GLOBAL.user_id
       WHERE ael.ae_header_id in (
            SELECT aeh.ae_header_id
              FROM xla_ae_headers aeh
             WHERE aeh.event_id = l_event_ids(i)
               AND aeh.application_id = 200
               AND aeh.gl_transfer_status_code <> 'Y');

     l_log_msg :='After calling procedure AP_XLA_EVENTS_PKG.UPDATE_EVENT';
     IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
        FND_LOG.STRING(G_LEVEL_PROCEDURE,
                       G_MODULE_NAME || l_procedure_name,
                       l_log_msg);
     END IF;

    END LOOP;

    if (p_period_name is null) then
       EXIT WHEN l_aph_events_cur_no_period%NOTFOUND;
    else
       EXIT WHEN l_aph_events_cur_period%NOTFOUND;
    end if;

  END LOOP;

  if (p_period_name is null) then
     CLOSE l_aph_events_cur_no_period;
  else
     CLOSE l_aph_events_cur_period;
  end if;

  l_log_msg :='End of procedure '||l_procedure_name;
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
     FND_LOG.STRING(G_LEVEL_PROCEDURE,
                    G_MODULE_NAME || l_procedure_name,
                    l_log_msg);
  END IF;

EXCEPTION
  WHEN OTHERS THEN
    IF (l_aid_events_cur%ISOPEN) THEN
      CLOSE l_aid_events_cur;
    END IF;

    if (p_period_name is null) then
       IF (l_aph_events_cur_no_period%ISOPEN) THEN
         CLOSE l_aph_events_cur_no_period;
       END IF;
    else
       IF (l_aph_events_cur_period%ISOPEN) THEN
         CLOSE l_aph_events_cur_period;
       END IF;
    end if;

    IF (SQLCODE <> -20001) THEN
       FND_MESSAGE.SET_NAME('SQLAP','AP_DEBUG');
       FND_MESSAGE.SET_TOKEN('ERROR',SQLERRM);
       FND_MESSAGE.SET_TOKEN('CALLING_SEQUENCE',
                    l_curr_calling_sequence);
       FND_MESSAGE.SET_TOKEN('PARAMETERS',
                'p_period_name = '||p_period_name
            ||', p_from_date = '||p_from_date
            ||', p_to_date = '||p_to_date
            ||', p_sweep_to_date = '||p_sweep_to_date);
       FND_MESSAGE.SET_TOKEN('DEBUG_INFO',l_log_msg);
    END IF;
    APP_EXCEPTION.RAISE_EXCEPTION();
END single_org_events_sweep;


/*============================================================================
 |  FUNCTION  -  GET_EVENT_CLASS(PRIVATE)
 |
 |  DESCRIPTION
 |    This procedure is used to get the event class of a particula event
 |    type.
 |
 |  PRAMETERS:
 |         p_event_type: Event Type
 |         p_calling_sequence: Debug information
 |
 |  RETURN: VARCHAR2
 |
 |  KNOWN ISSUES:
 |
 |  NOTES:
 |
 |  MODIFICATION HISTORY
 |  Date         Author             Description of Change
 |
 *===========================================================================*/
FUNCTION get_event_class(
              p_event_type       IN    VARCHAR2,
              p_calling_sequence IN    VARCHAR2)
RETURN VARCHAR2
IS

  l_curr_calling_sequence VARCHAR2(2000);

   -- Logging:
  l_procedure_name CONSTANT VARCHAR2(30) := 'GET_CLASS_CODE';
  l_log_msg        FND_LOG_MESSAGES.MESSAGE_TEXT%TYPE;

BEGIN

  l_curr_calling_sequence :=
    p_calling_sequence || ' -> AP_ACCOUNTING_EVENTS_PKG.GET_CLASS_CODE';

  G_CURRENT_RUNTIME_LEVEL := FND_LOG.G_CURRENT_RUNTIME_LEVEL;

  l_log_msg :='Begin of procedure '||l_procedure_name;
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
     FND_LOG.STRING(G_LEVEL_PROCEDURE,
                    G_MODULE_NAME || l_procedure_name,
                    l_log_msg);
  END IF;

  IF ( p_event_type IN ( INVOICE_VALIDATED_TYPE,
                         INVOICE_ADJUSTED_TYPE,
                         INVOICE_CANCELLED_TYPE)) THEN
    RETURN INVOICES_CLASS;
  END IF;

  IF ( p_event_type IN ( CREDIT_MEMO_VALIDATED_TYPE,
                         CREDIT_MEMO_ADJUSTED_TYPE,
                         CREDIT_MEMO_CANCELLED_TYPE)) THEN
    RETURN CREDIT_MEMOS_CLASS;
  END IF;

  IF ( p_event_type IN ( DEBIT_MEMO_VALIDATED_TYPE,
                         DEBIT_MEMO_ADJUSTED_TYPE,
                         DEBIT_MEMO_CANCELLED_TYPE)) THEN
     RETURN DEBIT_MEMOS_CLASS;
  END IF;

  IF ( p_event_type IN ( PREPAYMENT_VALIDATED_TYPE,
                         PREPAYMENT_ADJUSTED_TYPE,
                         PREPAYMENT_CANCELLED_TYPE)) THEN
    RETURN PREPAYMENTS_CLASS;
  END IF;

  IF ( p_event_type IN ( PREPAYMENT_APPLIED_TYPE,
                         PREPAYMENT_UNAPPLIED_TYPE,
                         PREPAY_APP_ADJUSTED_TYPE)) THEN
    RETURN PREPAYMENT_APPLICATIONS_CLASS;
  END IF;

  IF ( p_event_type IN ( PAYMENT_CREATED_TYPE,
                         PAYMENT_CANCELLED_TYPE,
                         MANUAL_PAYMENT_ADJUSTED_TYPE,
                         PAYMENT_ADJUSTED_TYPE)) THEN
    RETURN PAYMENTS_CLASS;
  END IF;
  --bug 10336668
  IF ( p_event_type IN ( REFUND_RECORDED_TYPE,
                         REFUND_CANCELLED_TYPE,
                         REFUND_ADJUSTED_TYPE,
                         MANUAL_REFUND_ADJUSTED_TYPE)) THEN
    RETURN REFUNDS_CLASS;
  END IF;

  IF
  (
    p_event_type IN
      (
        PAYMENT_MATURED_TYPE,
        PAYMENT_MATURITY_REVERSED_TYPE,
        PAYMENT_MATURITY_ADJUSTED_TYPE
      )
  )
  THEN
    RETURN FUTURE_DATED_PAYMENTS_CLASS;
  END IF;

  IF
  (
    p_event_type IN
      (
        PAYMENT_CLEARED_TYPE,
        PAYMENT_UNCLEARED_TYPE,
        PAYMENT_CLEARING_ADJUSTED_TYPE
      )
  )
  THEN
    RETURN RECONCILED_PAYMENTS_CLASS;
  END IF;


  FND_MESSAGE.SET_TOKEN('ERROR',SQLERRM);
  FND_MESSAGE.SET_TOKEN('CALLING_SEQUENCE',
                    l_curr_calling_sequence);
  FND_MESSAGE.SET_TOKEN('PARAMETERS',
                'p_event_type = '||p_event_type );
  FND_MESSAGE.SET_TOKEN('DEBUG_INFO',l_log_msg);

  APP_EXCEPTION.RAISE_EXCEPTION();
  RETURN NULL; -- unreachable

END get_event_class;


/*============================================================================
 |  FUNCTION  -  GET_EVENT_SECURITY_CONTEXT(PRIVATE)
 |
 |  DESCRIPTION
 |    This function is used to get the event security context.
 |
 |  PRAMETERS:
 |         p_org_id: Organization ID
 |         p_calling_sequence: Debug information
 |
 |  RETURN: XLA_EVENTS_PUB_PKG.T_SECURITY
 |
 |  KNOWN ISSUES:
 |
 |  NOTES:
 |
 |  MODIFICATION HISTORY
 |  Date         Author             Description of Change
 |
 *===========================================================================*/
FUNCTION get_event_security_context(
               p_org_id           IN NUMBER,
               p_calling_sequence IN VARCHAR2)
RETURN XLA_EVENTS_PUB_PKG.T_SECURITY
IS

  l_event_security_context XLA_EVENTS_PUB_PKG.T_SECURITY;

  -- Logging:
  l_procedure_name CONSTANT VARCHAR2(30) := 'GET_EVENT_SECURITY_CONTEXT';
  l_log_msg        FND_LOG_MESSAGES.MESSAGE_TEXT%TYPE;

BEGIN

  G_CURRENT_RUNTIME_LEVEL := FND_LOG.G_CURRENT_RUNTIME_LEVEL;

  l_log_msg :='Begin of procedure '||l_procedure_name;
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
     FND_LOG.STRING(G_LEVEL_PROCEDURE,
                    G_MODULE_NAME || l_procedure_name,
                    l_log_msg);
  END IF;

  l_event_security_context.security_id_int_1 := p_org_id;

  l_log_msg :='End of procedure '||l_procedure_name;
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
     FND_LOG.STRING(G_LEVEL_PROCEDURE,
                    G_MODULE_NAME || l_procedure_name,
                    l_log_msg);
  END IF;

  RETURN l_event_security_context;

END get_event_security_context;

/*============================================================================
 |  FUNCTION  -  GET_INVOICE_EVENT_SOURCE_INFO(PRIVATE)
 |
 |  DESCRIPTION
 |    This function is used to get invoice event source information
 |
 |  PRAMETERS:
 |         p_legal_entity_id: Legal entity ID
 |         p_ledger_id: Ledger ID
 |         p_invoice_id: Invoice ID
 |         p_calling_sequence: Debug information
 |
 |  RETURN: XLA_EVENTS_PUB_PKG.T_EVENT_SOURCE_INFO
 |
 |  KNOWN ISSUES:
 |
 |  NOTES:
 |
 |  MODIFICATION HISTORY
 |  Date         Author             Description of Change
 |
 *===========================================================================*/
FUNCTION get_invoice_event_source_info(
                p_legal_entity_id  IN   NUMBER,
                p_ledger_id        IN   NUMBER,
                p_invoice_id       IN   NUMBER,
                p_calling_sequence IN   VARCHAR2)
RETURN XLA_EVENTS_PUB_PKG.T_EVENT_SOURCE_INFO
IS

  l_invoice_num VARCHAR2(50);
  l_event_source_info XLA_EVENTS_PUB_PKG.T_EVENT_SOURCE_INFO;

  -- Logging:
  l_procedure_name CONSTANT VARCHAR2(30) := 'GET_EVENT_SOURCE_INFO';
  l_log_msg        FND_LOG_MESSAGES.MESSAGE_TEXT%TYPE;

BEGIN

  G_CURRENT_RUNTIME_LEVEL := FND_LOG.G_CURRENT_RUNTIME_LEVEL;

  l_log_msg :='Begin of procedure '||l_procedure_name;
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
     FND_LOG.STRING(G_LEVEL_PROCEDURE,
                    G_MODULE_NAME || l_procedure_name,
                    l_log_msg);
  END IF;

  select invoice_num
  into l_invoice_num
  from ap_invoices_all --bug6705052
  where invoice_id = p_invoice_id;

  l_event_source_info.application_id := 200;
  l_event_source_info.legal_entity_id := p_legal_entity_id;
  l_event_source_info.ledger_id := p_ledger_id;
  l_event_source_info.entity_type_code := INVOICES_ENTITY;
  l_event_source_info.transaction_number := l_invoice_num;
  l_event_source_info.source_id_int_1 := p_invoice_id;

  l_log_msg :='End of procedure '||l_procedure_name;
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
     FND_LOG.STRING(G_LEVEL_PROCEDURE,
                    G_MODULE_NAME || l_procedure_name,
                    l_log_msg);
  END IF;

  RETURN l_event_source_info;

END get_invoice_event_source_info;


/*============================================================================
 |  FUNCTION  -  GET_PAYMENT_EVENT_SOURCE_INFO(PRIVATE)
 |
 |  DESCRIPTION
 |    This procedure is used to get payment event source information.
 |
 |  PRAMETERS:
 |         p_legal_entity_id: Legal Entity ID
 |         p_ledger_id: Ledger ID
 |         p_invoice_id: Invoice ID
 |         p_calling_sequence: Debug information
 |
 |  RETURN: XLA_EVENTS_PUB_PKG.T_EVENT_SOURCE_INFO
 |
 |  KNOWN ISSUES:
 |
 |  NOTES:
 |
 |  MODIFICATION HISTORY
 |  Date         Author             Description of Change
 |
 *===========================================================================*/
FUNCTION get_payment_event_source_info(
           p_legal_entity_id    IN   NUMBER,
           p_ledger_id          IN   NUMBER,
           p_check_id           IN   NUMBER,
           p_calling_sequence   IN   VARCHAR2)
RETURN XLA_EVENTS_PUB_PKG.T_EVENT_SOURCE_INFO
IS

  l_check_number      NUMBER(15);
  l_event_source_info XLA_EVENTS_PUB_PKG.T_EVENT_SOURCE_INFO;

  -- Logging:
  l_procedure_name CONSTANT VARCHAR2(30) := 'GET_PAYMENT_EVENT_SOURCE_INFO';
  l_log_msg        FND_LOG_MESSAGES.MESSAGE_TEXT%TYPE;

BEGIN

  G_CURRENT_RUNTIME_LEVEL := FND_LOG.G_CURRENT_RUNTIME_LEVEL;

  l_log_msg :='Begin of procedure '||l_procedure_name;
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
     FND_LOG.STRING(G_LEVEL_PROCEDURE,
                    G_MODULE_NAME || l_procedure_name,
                    l_log_msg);
  END IF;

  select check_number
  into l_check_number
  from ap_checks_all  --bug6705052
  where check_id = p_check_id;

  l_event_source_info.application_id := 200;
  l_event_source_info.legal_entity_id := p_legal_entity_id;
  l_event_source_info.ledger_id := p_ledger_id;
  l_event_source_info.entity_type_code := PAYMENTS_ENTITY;
  l_event_source_info.transaction_number := l_check_number;
  l_event_source_info.source_id_int_1 := p_check_id;

  l_log_msg :='End of procedure '||l_procedure_name;
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
     FND_LOG.STRING(G_LEVEL_PROCEDURE,
                    G_MODULE_NAME || l_procedure_name,
                    l_log_msg);
  END IF;

  RETURN l_event_source_info;

END get_payment_event_source_info;


/*============================================================================
 |  PROCEDURE  -  GET_INVOICE_INFO(PRIVATE)
 |
 |  DESCRIPTION
 |    This procedure is used to get invoice information.
 |
 |  PRAMETERS:
 |         p_invoice_id: Invoice ID
 |         p_org_id: Organization ID
 |         p_legal_entity_id: Legal Entity ID
 |         p_ledger_id: Ledger ID
 |         p_transaction_date: Invoice date
 |         p_calling_sequence: Debug information
 |
 |  KNOWN ISSUES:
 |
 |  NOTES:
 |
 |  MODIFICATION HISTORY
 |  Date         Author             Description of Change
 |
 *===========================================================================*/
PROCEDURE get_invoice_info(
         p_invoice_id         IN         NUMBER,
         p_org_id             OUT NOCOPY NUMBER,
         p_legal_entity_id    OUT NOCOPY NUMBER,
         p_ledger_id          OUT NOCOPY NUMBER,
         p_transaction_date   OUT NOCOPY DATE,
         p_calling_sequence   IN         VARCHAR2)
IS

  l_curr_calling_sequence VARCHAR2(2000);

  -- Logging:
  l_procedure_name CONSTANT VARCHAR2(30) := 'GET_EVENT_SOURCE_INFO';
  l_log_msg        FND_LOG_MESSAGES.MESSAGE_TEXT%TYPE;

BEGIN

  l_curr_calling_sequence :=
    p_calling_sequence || ' -> AP_ACCOUNTING_EVENTS_PKG.GET_INVOICE_INFO';

  G_CURRENT_RUNTIME_LEVEL := FND_LOG.G_CURRENT_RUNTIME_LEVEL;

  l_log_msg :='Begin of procedure '||l_procedure_name;
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
     FND_LOG.STRING(G_LEVEL_PROCEDURE,
                    G_MODULE_NAME || l_procedure_name,
                    l_log_msg);
  END IF;

  SELECT
    AI.org_id,
    AI.legal_entity_id,
    AI.set_of_books_id,
    AI.invoice_date
  INTO
    p_org_id,
    p_legal_entity_id,
    p_ledger_id,
    p_transaction_date
  FROM
    ap_invoices_all AI --bug6705052
  WHERE
    AI.invoice_id = p_invoice_id;

 l_log_msg :='End of procedure '||l_procedure_name;
 IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
     FND_LOG.STRING(G_LEVEL_PROCEDURE,
                    G_MODULE_NAME || l_procedure_name,
                    l_log_msg);
 END IF;

EXCEPTION
  WHEN OTHERS THEN
    IF (SQLCODE <> -20001) THEN
       FND_MESSAGE.SET_TOKEN('ERROR',SQLERRM);
       FND_MESSAGE.SET_TOKEN('CALLING_SEQUENCE',
                    l_curr_calling_sequence);
       FND_MESSAGE.SET_TOKEN('PARAMETERS',
                'p_org_id = '||p_org_id);
       FND_MESSAGE.SET_TOKEN('DEBUG_INFO',l_log_msg);
    END IF;
    APP_EXCEPTION.RAISE_EXCEPTION();
END get_invoice_info;


/*===========================================================================
 |  PROCEDURE  -  GET_PAYMENT_INFO(PRIVATE)
 |
 |  DESCRIPTION
 |    This procedure is used to get payment information.
 |
 |  PRAMETERS:
 |         p_check_id: Check ID
 |         p_org_id: Organization ID
 |         p_legal_entity_id: Legal entity ID
 |         p_ledger_id: Ledger ID
 |         p_calling_sequence: Debug information
 |
 |  KNOWN ISSUES:
 |
 |  NOTES:
 |
 |  MODIFICATION HISTORY
 |  Date         Author             Description of Change
 |
 *==========================================================================*/
PROCEDURE get_payment_info(
            p_check_id         IN NUMBER,
            p_org_id           OUT NOCOPY NUMBER,
            p_legal_entity_id  OUT NOCOPY NUMBER,
            p_ledger_id        OUT NOCOPY NUMBER,
            p_calling_sequence IN VARCHAR2)
IS

  l_curr_calling_sequence VARCHAR2(2000);

   -- Logging:
  l_procedure_name CONSTANT VARCHAR2(30) := 'GET_PAYMENT_INFO';
  l_log_msg        FND_LOG_MESSAGES.MESSAGE_TEXT%TYPE;

BEGIN

  l_curr_calling_sequence :=
    p_calling_sequence || ' -> AP_ACCOUNTING_EVENTS_PKG.GET_PAYMENT_INFO';

  G_CURRENT_RUNTIME_LEVEL := FND_LOG.G_CURRENT_RUNTIME_LEVEL;

  l_log_msg :='Begin of procedure '||l_procedure_name;
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
     FND_LOG.STRING(G_LEVEL_PROCEDURE,
                    G_MODULE_NAME || l_procedure_name,
                    l_log_msg);
  END IF;

  SELECT AC.org_id,
         AC.legal_entity_id,
         ASP.set_of_books_id
  INTO   p_org_id,
         p_legal_entity_id,
         p_ledger_id
  FROM   ap_checks_all AC, --bug6705052
         ap_system_parameters_all ASP --bug6705052
  WHERE  AC.check_id = p_check_id
  AND    nvl(AC.org_id,-99) = nvl(ASP.org_id,-99);

  l_log_msg :='End of procedure '||l_procedure_name;
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
     FND_LOG.STRING(G_LEVEL_PROCEDURE,
                    G_MODULE_NAME || l_procedure_name,
                    l_log_msg);
 END IF;

EXCEPTION
  WHEN OTHERS THEN
    IF (SQLCODE <> -20001) THEN
       FND_MESSAGE.SET_TOKEN('ERROR',SQLERRM);
       FND_MESSAGE.SET_TOKEN('CALLING_SEQUENCE',
                    l_curr_calling_sequence);
       FND_MESSAGE.SET_TOKEN('PARAMETERS',
                'p_check_id = '||p_check_id);
       FND_MESSAGE.SET_TOKEN('DEBUG_INFO',l_log_msg);
    END IF;
    APP_EXCEPTION.RAISE_EXCEPTION();
END get_payment_info;


-- Bug 4996808. Created this procedure to update the accounting_event_id on
-- the prepay header and the prepayment appl dists since the header and dists
-- are created during validation before the events are created.
/*===========================================================================
 |  PROCEDURE  -  UPDATE_PREPAYMENT_HEADER(PRIVATE)
 |
 |  DESCRIPTION
 |    This procedure is used to update prepayment header information
 |
 |  PRAMETERS:
 |         p_invoice_id: Invoice ID
 |         p_invoice_line_number: Invoice Line Number
 |         p_accounting_event_id: Accounting Event ID
 |         p_calling_sequence: Debug information
 |
 *==========================================================================*/
PROCEDURE Update_Prepayment_Header(
               p_invoice_id            IN         NUMBER,
               p_invoice_line_number   IN         NUMBER,
               p_accounting_event_id   IN         NUMBER,
               p_accounting_date       IN         DATE,
               p_transaction_type      IN         VARCHAR2,
               p_calling_sequence      IN         VARCHAR2)
IS

  l_curr_calling_sequence         VARCHAR2(2000);
  TYPE l_prepay_hist_list IS TABLE OF ap_prepay_history.prepay_history_id%TYPE;
  l_prepay_hist_tab               l_prepay_hist_list;
  l_related_prepay_app_event_id   AP_PREPAY_HISTORY_ALL.related_prepay_app_event_id%TYPE;

   -- Logging:
  l_procedure_name CONSTANT VARCHAR2(30) := 'Update_Prepayment_Header';
  l_log_msg        FND_LOG_MESSAGES.MESSAGE_TEXT%TYPE;

BEGIN

  l_curr_calling_sequence := p_calling_sequence ||
            ' -> AP_ACCOUNTING_EVENTS_PKG.UPDATE_PREPAYMENT_HEADER';

  G_CURRENT_RUNTIME_LEVEL := FND_LOG.G_CURRENT_RUNTIME_LEVEL;

  l_log_msg :='Begin of procedure '||l_procedure_name;
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
     FND_LOG.STRING(G_LEVEL_PROCEDURE,
                    G_MODULE_NAME || l_procedure_name,
                    l_log_msg);
  END IF;

  -- Bug 9553684, we are updating the Related_Prepay_App_Event_ID 
  -- on the Prepayment Unapplication Distributions, when updating 
  -- the Accounting Event ID
  --
  SELECT min(accounting_Event_id)
    INTO l_related_prepay_app_event_id
    FROM AP_Invoice_Distributions_All AID
   WHERE AID.line_type_lookup_code = 'PREPAY'
     AND nvl(AID.amount,0) < 0
     AND AID.invoice_id = P_invoice_id
     AND AID.invoice_line_number = P_invoice_line_number;


  UPDATE AP_Prepay_History APH
  SET    Accounting_Event_ID = p_accounting_event_id,
         APH.Related_Prepay_App_Event_ID = 
             CASE
               WHEN APH.transaction_type = 'PREPAYMENT UNAPPLIED' AND
	            APH.related_prepay_app_event_id IS NULL THEN
                 l_related_prepay_app_event_id
               ELSE
                 APH.Related_Prepay_App_Event_ID
             END
  WHERE  Invoice_ID = p_invoice_id
  AND    Invoice_Line_Number = p_invoice_line_number
  AND    Accounting_Date = p_accounting_date
  AND    Transaction_Type = p_transaction_type
  AND    Accounting_Event_ID IS NULL
  RETURNING Prepay_History_ID
  BULK COLLECT INTO l_prepay_hist_tab;


  IF l_prepay_hist_tab.count >0 THEN

     FORALL i IN l_prepay_hist_tab.FIRST..l_prepay_hist_tab.LAST
     UPDATE AP_Prepay_App_Dists APAD
     SET    Accounting_Event_ID = p_accounting_event_id
     WHERE  Prepay_History_ID = l_prepay_hist_tab(i);

  END IF;

  l_log_msg :='End of procedure '||l_procedure_name;

  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
      FND_LOG.STRING(G_LEVEL_PROCEDURE,
                     G_MODULE_NAME || l_procedure_name,
                     l_log_msg);
  END IF;

EXCEPTION
  WHEN OTHERS THEN

    IF (SQLCODE <> -20001) THEN
       FND_MESSAGE.SET_TOKEN('ERROR',SQLERRM);
       FND_MESSAGE.SET_TOKEN('CALLING_SEQUENCE',
                    l_curr_calling_sequence);
       FND_MESSAGE.SET_TOKEN('PARAMETERS',
                'p_invoice_id = '||p_invoice_id);
       FND_MESSAGE.SET_TOKEN('DEBUG_INFO',l_log_msg);
    END IF;
    APP_EXCEPTION.RAISE_EXCEPTION();

END Update_Prepayment_Header;


/*===========================================================================
 |  PROCEDURE  -  INSERT_PREPAYMENT_HEADER(PRIVATE)
 |
 |  DESCRIPTION
 |    This procedure is used to insert prepayment header information
 |
 |  PRAMETERS:
 |         p_invoice_id: Invoice ID
 |         p_invoice_line_number: Invoice Line Number
 |         p_accounting_event_id: Accounting Event ID
 |         p_invoice_adjustment_id: Invoice Adjustment ID
 |         p_calling_sequence: Debug information
 |
 |  KNOWN ISSUES:
 |
 |  NOTES:
 |
 |  MODIFICATION HISTORY
 |  Date         Author             Description of Change
 |
 *==========================================================================*/
PROCEDURE Insert_Prepayment_Header(
               p_invoice_id            IN         NUMBER,
               p_invoice_line_number   IN         NUMBER,
               p_accounting_event_id   IN         NUMBER,
               p_accounting_date       IN         DATE,
               p_invoice_adjustment_id IN         NUMBER,
               p_calling_sequence      IN         VARCHAR2)
IS

  l_curr_calling_sequence         VARCHAR2(2000);
  l_sum_amount                    NUMBER;
  l_transaction_type              VARCHAR2(30);
  l_prepay_invoice_id             NUMBER;
  l_prepay_line_number            NUMBER;
  l_org_id                        NUMBER;
  l_related_prepay_app_event_id   NUMBER;

   -- Logging:
  l_procedure_name CONSTANT VARCHAR2(30) := 'Insert_Prepayment_Header';
  l_log_msg        FND_LOG_MESSAGES.MESSAGE_TEXT%TYPE;

  --bugfix:4936043
  CURSOR prepayment_invoices IS
    SELECT AIL.amount,
           AIL.invoice_id,
           AIL.line_number,
           AIL.org_id
    FROM   ap_invoice_lines AIL,
           ap_invoice_distributions AID,
           ap_invoice_distributions AID1
    WHERE  AID.invoice_id = p_invoice_id
    AND    AID.invoice_line_number = p_invoice_line_number
    AND    AID.line_type_lookup_code = 'PREPAY'
    AND    AID.prepay_distribution_id = AID1.invoice_distribution_id
    AND    AIL.invoice_id = AID1.invoice_id
    AND    AIL.line_number = AID1.invoice_line_number
    GROUP BY ail.invoice_id,ail.line_number,ail.org_id,ail.amount;


BEGIN

  l_curr_calling_sequence := p_calling_sequence ||
            ' -> AP_ACCOUNTING_EVENTS_PKG.INSERT_PREPAYMENT_HEADER';

  G_CURRENT_RUNTIME_LEVEL := FND_LOG.G_CURRENT_RUNTIME_LEVEL;

  l_log_msg :='Begin of procedure '||l_procedure_name;
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
     FND_LOG.STRING(G_LEVEL_PROCEDURE,
                    G_MODULE_NAME || l_procedure_name,
                    l_log_msg);
  END IF;

  BEGIN

    SELECT AIL.amount,
           AIL.prepay_invoice_id,
           AIL.prepay_line_number,
           AIL.org_id
    INTO   l_sum_amount,
           l_prepay_invoice_id,
           l_prepay_line_number,
           l_org_id
    FROM   ap_invoice_lines AIL
    WHERE  AIL.invoice_id = P_invoice_id
    AND    AIL.line_type_lookup_code = 'PREPAY'
    AND    AIL.line_number = P_invoice_line_number;

  --Bugfix:4936043
  --Complex Work: Recoupment
  --Added the following SQL for recoupment logic, as we do not have a parent 'PREPAY' line for the
  --'PREPAY' distributions. For recoupment we create the 'PREPAY' distributions tied to the ITEM line
  --itself.
  EXCEPTION WHEN NO_DATA_FOUND THEN

     OPEN prepayment_invoices;
     LOOP
        FETCH prepayment_invoices into l_sum_amount,
                                       l_prepay_invoice_id,
                                       l_prepay_line_number,
                                       l_org_id;

        EXIT WHEN prepayment_invoices%NOTFOUND;

        BEGIN

            SELECT min(accounting_Event_id)
            INTO   l_related_prepay_app_event_id
            FROM   AP_INVOICE_DISTRIBUTIONS AID
            WHERE  AID.line_type_lookup_code = 'PREPAY'
            AND    (nvl(posted_flag,'N') = 'Y' OR
                    nvl(encumbered_flag, 'N') = 'Y')  --bug9973070
            AND    nvl(AID.amount,0) < 0
            AND    AID.invoice_id = P_invoice_id
            AND    AID.invoice_line_number = P_invoice_line_number;


        EXCEPTION
            WHEN NO_DATA_FOUND THEN
                 l_related_prepay_app_event_id:= null;
        END;

        IF P_invoice_adjustment_id is NOT NULL THEN
            l_transaction_type := PREPAY_APP_ADJUSTED_TYPE;
        ELSIF l_sum_Amount <= 0 THEN
            l_transaction_type := PREPAYMENT_APPLIED_TYPE;
        ELSE
            l_transaction_type := PREPAYMENT_UNAPPLIED_TYPE;
        END IF;

        INSERT INTO AP_PREPAY_HISTORY_ALL
            ( PREPAY_HISTORY_ID
             ,PREPAY_INVOICE_ID
             ,PREPAY_LINE_NUM
             ,ACCOUNTING_EVENT_ID
             ,HISTORICAL_FLAG
             ,INVOICE_ID
             ,INVOICE_LINE_NUMBER
             ,ACCOUNTING_DATE
             ,INVOICE_ADJUSTMENT_EVENT_ID
             ,ORG_ID
             ,POSTED_FLAG
             ,RELATED_PREPAY_APP_EVENT_ID
             ,TRANSACTION_TYPE
             ,LAST_UPDATED_BY
             ,LAST_UPDATE_DATE
             ,LAST_UPDATE_LOGIN
             ,CREATED_BY
             ,CREATION_DATE
             ,PROGRAM_APPLICATION_ID
             ,PROGRAM_ID
             ,PROGRAM_UPDATE_DATE
             ,REQUEST_ID)
         VALUES
             (AP_PREPAY_HISTORY_S.nextval
             ,l_prepay_invoice_id
             ,l_prepay_line_number
             ,p_Accounting_event_id
             ,'N'
             ,P_INVOICE_ID
             ,p_invoice_line_number
             ,p_accounting_date
             ,p_invoice_adjustment_id
             ,l_org_id
             ,'N'
             ,l_related_prepay_app_event_id
             ,L_TRANSACTION_TYPE
             ,FND_GLOBAL.user_id
             ,sysdate
             ,FND_GLOBAL.login_id
             ,FND_GLOBAL.user_id
             ,sysdate
             ,null
             ,null
             ,null
             ,null);

     END LOOP;

     CLOSE prepayment_invoices;

     l_log_msg :='End of procedure '||l_procedure_name;

     IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
       FND_LOG.STRING(G_LEVEL_PROCEDURE,
                      G_MODULE_NAME || l_procedure_name,
                     l_log_msg);
     END IF;

     RETURN;

  END;


  BEGIN

    SELECT min(accounting_Event_id)
    INTO   l_related_prepay_app_event_id
    FROM   AP_INVOICE_DISTRIBUTIONS AID
    WHERE  AID.line_type_lookup_code = 'PREPAY'
    AND    (nvl(posted_flag,'N') = 'Y' OR
            nvl(encumbered_flag, 'N') = 'Y')   --bug9973070
    AND    nvl(AID.amount,0) < 0
    AND    AID.invoice_id = P_invoice_id
    AND    AID.invoice_line_number = P_invoice_line_number;


  EXCEPTION
    WHEN NO_DATA_FOUND THEN
      l_related_prepay_app_event_id:= null;

  END;

  IF P_invoice_adjustment_id is NOT NULL THEN
    l_transaction_type := PREPAY_APP_ADJUSTED_TYPE;

  ELSIF l_sum_Amount <= 0 THEN
    l_transaction_type := PREPAYMENT_APPLIED_TYPE;
  ELSE
    l_transaction_type := PREPAYMENT_UNAPPLIED_TYPE;
  END IF;

    INSERT INTO AP_PREPAY_HISTORY_ALL
      ( PREPAY_HISTORY_ID
       ,PREPAY_INVOICE_ID
       ,PREPAY_LINE_NUM
       ,ACCOUNTING_EVENT_ID
       ,ACCOUNTING_DATE
       ,HISTORICAL_FLAG
       ,INVOICE_ID
       ,INVOICE_ADJUSTMENT_EVENT_ID
       ,ORG_ID
       ,POSTED_FLAG
       ,RELATED_PREPAY_APP_EVENT_ID
       ,TRANSACTION_TYPE
       ,LAST_UPDATED_BY
       ,LAST_UPDATE_DATE
       ,LAST_UPDATE_LOGIN
       ,CREATED_BY
       ,CREATION_DATE
       ,PROGRAM_APPLICATION_ID
       ,PROGRAM_ID
       ,PROGRAM_UPDATE_DATE
       ,REQUEST_ID)
     VALUES
       (AP_PREPAY_HISTORY_S.nextval
        ,l_prepay_invoice_id
        ,l_prepay_line_number
        ,p_Accounting_event_id
        ,p_accounting_date
        ,'N'
        ,P_INVOICE_ID
        ,p_invoice_adjustment_id
        ,l_org_id
        ,'N'
        ,l_related_prepay_app_event_id
        ,L_TRANSACTION_TYPE
        ,FND_GLOBAL.user_id
        ,sysdate
        ,FND_GLOBAL.login_id
        ,FND_GLOBAL.user_id
        ,sysdate
        ,null
        ,null
        ,null
        ,null);

 l_log_msg :='End of procedure '||l_procedure_name;

 IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
     FND_LOG.STRING(G_LEVEL_PROCEDURE,
                    G_MODULE_NAME || l_procedure_name,
                    l_log_msg);
 END IF;

EXCEPTION
  WHEN OTHERS THEN

    IF (SQLCODE <> -20001) THEN
       FND_MESSAGE.SET_TOKEN('ERROR',SQLERRM);
       FND_MESSAGE.SET_TOKEN('CALLING_SEQUENCE',
                    l_curr_calling_sequence);
       FND_MESSAGE.SET_TOKEN('PARAMETERS',
                'p_invoice_id = '||p_invoice_id);
       FND_MESSAGE.SET_TOKEN('DEBUG_INFO',l_log_msg);
    END IF;
    APP_EXCEPTION.RAISE_EXCEPTION();

END Insert_Prepayment_Header;

-- PROCEDURE added for the bug9322013
--
PROCEDURE Set_Prepay_Event_Noaction
       (p_invoice_id            IN        NUMBER,
        p_calling_sequence      IN        VARCHAR2) IS

  CURSOR reversed_prepay_events IS
  SELECT DISTINCT aid.invoice_id,
         aid.accounting_event_id
    FROM ap_invoice_distributions_all aid,
         ap_invoices_all ai
   WHERE ai.invoice_id = p_invoice_id
     AND aid.invoice_id = ai.invoice_id
     --AND ai.cancelled_date IS NOT NULL      --bug9441420
     AND aid.line_type_lookup_code IN ('PREPAY','REC_TAX','NONREC_TAX')
     AND aid.prepay_distribution_id IS NOT NULL
     AND aid.accounting_event_id IS NOT NULL
     AND NVL(aid.posted_flag, 'N') <> 'Y'
     AND NVL(aid.reversal_flag, 'N') = 'Y'
     AND EXISTS
          (SELECT 1
             FROM ap_invoice_distributions_all aid1
            WHERE aid1.invoice_id = aid.invoice_id
              AND aid1.line_type_lookup_code IN ('PREPAY','REC_TAX','NONREC_TAX')
              AND aid1.prepay_distribution_id IS NOT NULL
              AND aid1.accounting_event_id IS NOT NULL
              AND NVL(aid1.posted_flag, 'N') <> 'Y'
              AND NVL(aid1.reversal_flag, 'N') = 'Y'
              AND aid.invoice_distribution_id <> aid1.invoice_distribution_id  /* Bug 10196007 */
              AND NVL(aid1.parent_reversal_id, aid1.invoice_distribution_id) =
                    NVL(aid.parent_reversal_id, aid.invoice_distribution_id))
     AND ((ai.cancelled_date IS NOT NULL AND
           NOT EXISTS
            (SELECT 1
               FROM ap_invoice_distributions_all aid1
              WHERE aid1.invoice_id = ai.invoice_id
                AND aid1.prepay_distribution_id IS NULL
             )
           ) OR  --bug9441420 added OR clause
           (NOT EXISTS
            (SELECT 1
               FROM ap_invoice_payments_all aip,
                    ap_invoice_distributions_all aid_prepay
              WHERE aip.invoice_id = aid_prepay.invoice_id
                AND aid_prepay.invoice_distribution_id = aid.prepay_distribution_id
                AND nvl(aip.reversal_flag, 'N') = 'N'
            ))
          )
  /* RCA bug 14120800*/
  UNION
  SELECT app_aid.invoice_id,
         unapp_aid.accounting_Event_id unapp_event_id
    FROM ap_invoice_distributions_all app_aid,
         xla_events xe_app,
         ap_invoices_all ai,
         ap_invoice_distributions_all unapp_aid,
         xla_events xe_unapp        
   WHERE ai.invoice_id = p_invoice_id
     AND app_aid.prepay_distribution_id IS NOT NULL
     AND app_aid.accounting_event_id = xe_app.event_id
     AND xe_app.application_id = 200    
     AND xe_app.event_type_code = 'PREPAYMENT APPLIED'
     AND xe_app.event_status_code = 'N'
     AND unapp_aid.invoice_id = app_aid.invoice_id 
     AND unapp_aid.prepay_distribution_id IS NOT NULL
     AND unapp_aid.parent_reversal_id IS NOT NULL
     AND app_aid.invoice_distribution_id = unapp_aid.parent_reversal_id
     AND unapp_aid.accounting_event_id = xe_unapp.event_id
     AND xe_unapp.event_type_code = 'PREPAYMENT UNAPPLIED'
     AND xe_unapp.application_id = 200    
     AND xe_unapp.event_status_code = 'U'
     AND app_aid.invoice_id = ai.invoice_id;

  l_org_id                      AP_INVOICES_ALL.org_id%TYPE;
  l_legal_entity_id             AP_INVOICES_ALL.legal_entity_id%TYPE;
  l_ledger_id                   AP_INVOICES_ALL.set_of_books_id%TYPE;
  l_transaction_date            DATE;
  l_curr_calling_sequence       VARCHAR2(4000);
  l_event_source_info           XLA_EVENTS_PUB_PKG.T_EVENT_SOURCE_INFO;
  l_invoice_id                  AP_INVOICES_ALL.invoice_id%TYPE;
  l_accounting_event_id         XLA_EVENTS.event_id%TYPE;
  l_event_security_context      XLA_EVENTS_PUB_PKG.T_SECURITY;
  l_log_msg                     FND_LOG_MESSAGES.MESSAGE_TEXT%TYPE;

  l_procedure_name CONSTANT VARCHAR2(30) := 'Set_Prepay_Event_Noaction';

BEGIN

  l_curr_calling_sequence :=
     'Set_Prepay_Event_Noaction <- '||p_calling_sequence;

  G_CURRENT_RUNTIME_LEVEL := FND_LOG.G_CURRENT_RUNTIME_LEVEL;

  l_log_msg := 'Begin of procedure '|| l_procedure_name;
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
      FND_LOG.STRING(G_LEVEL_PROCEDURE,
                     G_MODULE_NAME||l_procedure_name||'.begin',
                     l_log_msg);
  END IF;

  l_log_msg := 'Before calling procedure get_invoice_info';
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
    FND_LOG.STRING(G_LEVEL_PROCEDURE,
                   G_MODULE_NAME || l_procedure_name,
                   l_log_msg);
  END IF;

  AP_ACCOUNTING_EVENTS_PKG.get_invoice_info
              (p_invoice_id => p_invoice_id,
               p_org_id            => l_org_id,               -- OUT
               p_legal_entity_id   => l_legal_entity_id,      -- OUT
               p_ledger_id         => l_ledger_id,            -- OUT
               p_transaction_date  => l_transaction_date,     -- OUT
               p_calling_sequence  => l_curr_calling_sequence);

  l_log_msg := 'After calling procedure get_insert_info executed';
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
     FND_LOG.STRING(G_LEVEL_PROCEDURE,
                    G_MODULE_NAME || l_procedure_name,
                    l_log_msg);
  END IF;

  l_log_msg :='Before calling proc get_event_security_context';
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
    FND_LOG.STRING(G_LEVEL_PROCEDURE,
                   G_MODULE_NAME || l_procedure_name,
                   l_log_msg);
  END IF;

  l_event_security_context :=
           AP_ACCOUNTING_EVENTS_PKG.get_event_security_context
              (p_org_id           => l_org_id,
               p_calling_sequence => l_curr_calling_sequence);

  l_log_msg := 'After calling proc get_event_security_context';
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
    FND_LOG.STRING(G_LEVEL_PROCEDURE,
                   G_MODULE_NAME || l_procedure_name,
                   l_log_msg);
  END IF;

  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
    l_log_msg := 'Before calling proc get_invoice_event_source_info';
    FND_LOG.STRING(G_LEVEL_PROCEDURE,
                   G_MODULE_NAME || l_procedure_name,
                   l_log_msg);
  END IF;

  l_event_source_info :=
       AP_ACCOUNTING_EVENTS_PKG.get_invoice_event_source_info
         (p_legal_entity_id     => l_legal_entity_id,
          p_ledger_id           => l_ledger_id,
          p_invoice_id          => p_invoice_id,
          p_calling_sequence    => l_curr_calling_sequence);

  l_log_msg := 'After calling proc get_invoice_envent_source_info';
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
    FND_LOG.STRING(G_LEVEL_PROCEDURE,
                   G_MODULE_NAME || l_procedure_name,
                   l_log_msg);
  END IF;

  OPEN reversed_prepay_events;
    LOOP
      FETCH reversed_prepay_events
      INTO l_invoice_id, l_accounting_event_id;

      EXIT WHEN reversed_prepay_events%NOTFOUND;


      l_log_msg := 'Calling the Update event api for accounting event'||
                   l_accounting_event_id;
      IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
        FND_LOG.STRING(G_LEVEL_PROCEDURE,
                       G_MODULE_NAME || l_procedure_name,
                       l_log_msg);
      END IF;

      AP_XLA_EVENTS_PKG.UPDATE_EVENT
        (p_event_source_info => l_event_source_info,
         p_event_id          => l_accounting_event_id,
         p_event_type_code   => NULL,
         p_event_date        => NULL,
         p_event_status_code => XLA_EVENTS_PUB_PKG.C_EVENT_NOACTION,
         p_valuation_method  => NULL,
         p_security_context  => l_event_security_context,
         p_calling_sequence  => l_curr_calling_sequence);

      l_log_msg := 'After Calling the Update event api for accounting event'||
                   l_accounting_event_id;
      IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
        FND_LOG.STRING(G_LEVEL_PROCEDURE,
                       G_MODULE_NAME || l_procedure_name,
                       l_log_msg);
      END IF;


    END LOOP;
  CLOSE reversed_prepay_events;

EXCEPTION
  WHEN OTHERS THEN

    --bug11655195 start
        IF (reversed_prepay_events%ISOPEN) THEN
         CLOSE reversed_prepay_events ;
        END IF;
    --bug11655195 end

    IF (SQLCODE <> -20001) THEN
       FND_MESSAGE.SET_TOKEN('ERROR',SQLERRM);
       FND_MESSAGE.SET_TOKEN('CALLING_SEQUENCE',
                    l_curr_calling_sequence);
       FND_MESSAGE.SET_TOKEN('PARAMETERS',
                'p_invoice_id = '||p_invoice_id);
       FND_MESSAGE.SET_TOKEN('DEBUG_INFO',l_log_msg);
    END IF;
    APP_EXCEPTION.RAISE_EXCEPTION();

END;

END AP_ACCOUNTING_EVENTS_PKG;
/
COMMIT;
EXIT;
