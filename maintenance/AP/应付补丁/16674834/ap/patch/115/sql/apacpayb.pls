REM +==================================================================+
REM |                Copyright (c) 1999, 2014 Oracle Corporation
REM |                   Redwood Shores, California, USA
REM |                        All rights reserved.
REM +==================================================================+
REM |  Name
REM |    apacpayb.pls
REM |
REM |  Description - Package AP_ACCOUNTING_PAY_PKG
REM |  This package is the starting point of the Payment Event processing.
REM |  After selecting payment events for processing, it calls the 
REM |  Payment Distributions and Prepayment Application Distributions Generator
REM |  to create the payment and prepayment appl dists.
REM |
REM |  History
REM |    Created By:  Haritha Redreddy  (05/03/04)
REM +==================================================================+
REM dbdrv: sql ~PROD ~PATH ~FILE none none none package &phase=plb \
REM dbdrv: checkfile(120.12.12010000.41=120.47.12020000.7):~PROD:~PATH:~FILE

SET VERIFY OFF
WHENEVER SQLERROR EXIT FAILURE ROLLBACK
WHENEVER OSERROR EXIT FAILURE ROLLBACK


CREATE OR REPLACE PACKAGE BODY AP_ACCOUNTING_PAY_PKG AS
/* $Header: apacpayb.pls 120.47.12020000.7 2014/03/21 06:16:44 imandal ship $ */

-- Logging Infra
G_CURRENT_RUNTIME_LEVEL      NUMBER                := FND_LOG.G_CURRENT_RUNTIME_LEVEL;
G_LEVEL_UNEXPECTED           CONSTANT NUMBER       := FND_LOG.LEVEL_UNEXPECTED;
G_LEVEL_ERROR                CONSTANT NUMBER       := FND_LOG.LEVEL_ERROR;
G_LEVEL_EXCEPTION            CONSTANT NUMBER       := FND_LOG.LEVEL_EXCEPTION;
G_LEVEL_EVENT                CONSTANT NUMBER       := FND_LOG.LEVEL_EVENT;
G_LEVEL_PROCEDURE            CONSTANT NUMBER       := FND_LOG.LEVEL_PROCEDURE;
G_LEVEL_STATEMENT            CONSTANT NUMBER       := FND_LOG.LEVEL_STATEMENT;
G_MODULE_NAME                CONSTANT VARCHAR2(50) := 'AP.PLSQL.AP_ACCOUNTING_PAY_PKG.';
-- Logging Infra

-------------------------------------------------------------------------------
-- PROCEDURE  Do_Pay_Accounting
-- Selects Payment Events for processing. Calls the Payment Dists and Prepay Appl
-- Dists Generator for creating Payment and Prepay Appl dists. Single point of
-- entry for Payment processing.
--
--------------------------------------------------------------------------------
PROCEDURE Do_Pay_Accounting
     (P_Calling_Sequence     IN   VARCHAR2
     ) IS

  l_xla_event_rec            r_xla_event_info;
  l_curr_calling_sequence    VARCHAR2(2000);
  l_check_curr_code          ap_checks_all.currency_code%type; --8288996
  l_budgetary_control_flag   VARCHAR2(1);
  l_exc_data_mismatch        EXCEPTION; --bug 9936620

  -- bug9716573, added budgetary control flag to the cursor
  -- bug11772495, removed budgetary control flag from the cursor
  -- bug10412623, added entity_id, ledger_id to the cursor
  -- bug12918263, removed entity_id, ledger_id to the cursor

  CURSOR   xla_events_cur IS
  SELECT   Event_ID,
           Event_Type_Code,
           Event_Date,
           Event_Number,
           Event_Status_Code,
           Entity_Code,
           Source_ID_Int_1
  FROM     XLA_Events_GT
  WHERE   (Entity_Code = 'AP_PAYMENTS'
           OR Event_Type_Code IN ('PREPAYMENT APPLIED',
                                  'PREPAYMENT UNAPPLIED',
                                  'PREPAYMENT APPLICATION ADJ'))
  AND      Event_Status_Code <> 'N'
  ORDER BY Entity_id,      --Bug 9784405
           Event_Number;   --Bug 9784405

  -- Logging Infra:
  l_ledger_id     NUMBER;
  l_entity_id     NUMBER;
  l_procedure_name CONSTANT VARCHAR2(30) := 'Do_Pay_Accounting';
  l_log_msg        FND_LOG_MESSAGES.MESSAGE_TEXT%TYPE;

BEGIN

  l_curr_calling_sequence := 'AP_Accounting_Pay_Pkg.Do_Pay_Accounting<- ' ||
                                      p_calling_sequence;

  -- Logging Infra: Setting up runtime level
  G_CURRENT_RUNTIME_LEVEL := FND_LOG.G_CURRENT_RUNTIME_LEVEL;

  -- Logging Infra: Procedure level
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
      l_log_msg := 'Begin of procedure '|| l_procedure_name;
      FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name||'.begin', l_log_msg);
  END IF;


  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
      l_log_msg := 'Calling procedure Prorate_Historical_Dists to create '||
                   'Prepay App dists for the historical non Accounted Dists';
      FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME || l_procedure_name, l_log_msg);
  END IF;

  Prorate_Historical_Dists(l_curr_calling_sequence);

  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
      l_log_msg := 'Calling procedure AP_Acctg_Pay_Dist_Pkg.Primary_Pay_Events';
      FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME || l_procedure_name, l_log_msg);
  END IF;

  -- We need to delete the payment hist distributions and prepay appl hist distributions
  -- which were created during the draft mode of the accounting process
  -------------------------------------------------------------------------------

  Delete_Hist_Dists (l_curr_calling_sequence);


  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
      l_log_msg := 'Procedure AP_Acctg_Pay_Dist_Pkg.Primary_Pay_Events executed';
      FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME || l_procedure_name, l_log_msg);
  END IF;

  OPEN xla_events_cur;
  LOOP
     BEGIN
       FETCH xla_events_cur INTO l_xla_event_rec;
       EXIT WHEN xla_events_cur%NOTFOUND OR
                 xla_events_cur%NOTFOUND IS NULL;


       IF ( G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
           l_log_msg := 'CUR: xla_events_cur: entity_code = '|| l_xla_event_rec.entity_code
                        || ' document_id = ' || l_xla_event_rec.source_id_int_1;
           FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name, l_log_msg);
       END IF;

       -- Get the base currency code into global variable
       IF (l_xla_event_rec.entity_code = 'AP_PAYMENTS') THEN
           BEGIN
              SELECT ASP.Base_Currency_Code, AC.Currency_Code --8288996
              INTO   g_base_currency_code, l_check_curr_code
              FROM   AP_System_Parameters_All ASP,
                     AP_Checks_All AC
              WHERE  AC.Check_ID = l_xla_event_rec.source_id_int_1
              AND    AC.Org_ID = ASP.Org_ID;
           EXCEPTION
              WHEN NO_DATA_FOUND THEN
                 RAISE_APPLICATION_ERROR(-20100, 'check_id_mismatch'); --bug 9936620
           END;
       ELSE

           SELECT ASP.Base_Currency_Code
           INTO   g_base_currency_code
           FROM   AP_System_Parameters_All ASP,
                  AP_Invoices_All AI
           WHERE  AI.Invoice_ID = l_xla_event_rec.source_id_int_1
           AND    AI.Org_ID = ASP.Org_ID;

       END IF;


       -- Based on the event type calling the appropriate event procedures
       -- to create payment and prepayment distributions.
       IF (l_xla_event_rec.event_type_code IN ('PAYMENT CREATED',
                                               'PAYMENT MATURED',
                                               'PAYMENT CLEARED',
                                               'REFUND RECORDED')) THEN

           IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
               l_log_msg := 'Calling procedure AP_Acctg_Pay_Dist_Pkg.Primary_Pay_Events';
               FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME || l_procedure_name, l_log_msg);
           END IF;


           AP_Acctg_Pay_Dist_Pkg.Primary_Pay_Events
                                  (l_xla_event_rec,
                                   l_curr_calling_sequence);


           IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
               l_log_msg := 'Procedure AP_Acctg_Pay_Dist_Pkg.Primary_Pay_Events executed';
               FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME || l_procedure_name, l_log_msg);
           END IF;

       ELSIF l_xla_event_rec.event_type_code IN ('MANUAL PAYMENT ADJUSTED',
                                                 'MANUAL REFUND ADJUSTED',
                                                 'UPGRADED MANUAL PMT ADJUSTED') THEN

             IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
                 l_log_msg := 'Calling procedure AP_Acctg_Pay_Dist_Pkg.Manual_Pay_Adj_Events';
                 FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME || l_procedure_name, l_log_msg);
             END IF;

             AP_Acctg_Pay_Dist_Pkg.Manual_Pay_Adj_Events
                                  (l_xla_event_rec,
                                   l_curr_calling_sequence);

             IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
                 l_log_msg := 'Procedure AP_Acctg_Pay_Dist_Pkg.Manual_Pay_Adj_Events executed';
                 FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME || l_procedure_name, l_log_msg);
             END IF;

       ELSIF l_xla_event_rec.event_type_code IN ('PAYMENT ADJUSTED',
                                                 'PAYMENT MATURITY ADJUSTED',
                                                 'PAYMENT CLEARING ADJUSTED',
                                                 'REFUND ADJUSTED') THEN

             IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
                 l_log_msg := 'Calling procedure AP_Acctg_Pay_Dist_Pkg.Pay_Dist_Cascade_Adj_Events';
                 FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME || l_procedure_name, l_log_msg);
             END IF;

             AP_Acctg_Pay_Dist_Pkg.Pay_Dist_Cascade_Adj_Events
                                  (l_xla_event_rec,
                                   l_curr_calling_sequence);

             IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
                 l_log_msg := 'Procedure AP_Acctg_Pay_Dist_Pkg.Pay_Dist_Cascade_Adj_Events executed';
                 FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME || l_procedure_name, l_log_msg);
             END IF;

       ELSIF l_xla_event_rec.event_type_code IN ('PAYMENT CANCELLED',
                                                 'PAYMENT MATURITY REVERSED',
                                                 'PAYMENT UNCLEARED',
                                                 'REFUND CANCELLED') THEN

             IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
                 l_log_msg := 'Calling procedure AP_Acctg_Pay_Dist_Pkg.Cancel_Primary_Pay_Events';
                 FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME || l_procedure_name, l_log_msg);
             END IF;

             AP_Acctg_Pay_Dist_Pkg.Cancel_Primary_Pay_Events
                                  (l_xla_event_rec,
                                   l_curr_calling_sequence);


             IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
                 l_log_msg := 'Procedure AP_Acctg_Pay_Dist_Pkg.Cancel_Primary_Pay_Events executed';
                 FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME || l_procedure_name, l_log_msg);
             END IF;

       ELSIF l_xla_event_rec.event_type_code IN ('PREPAYMENT APPLICATION ADJ') THEN

             IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
                 l_log_msg := 'Calling procedure AP_Acctg_Prepay_Dist_Pkg.Prepay_Dist_Cascade_Adj';
                 FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME || l_procedure_name, l_log_msg);
             END IF;

             AP_Acctg_Prepay_Dist_Pkg.Prepay_Dist_Cascade_Adj
                                  (l_xla_event_rec,
                                   l_curr_calling_sequence);

             IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
                 l_log_msg := 'Procedure AP_Acctg_Prepay_Dist_Pkg.Prepay_Dist_Cascade_Adj executed';
                 FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME || l_procedure_name, l_log_msg);
             END IF;

       -- bug9716573
       -- added the condition of budgetary control flag to ensure that the
       -- update of gain/loss indicator fires only for Non budgetary control
       -- events.
       --
       ELSIF l_xla_event_rec.event_type_code IN ('PREPAYMENT APPLIED',
                                                 'PREPAYMENT UNAPPLIED') THEN
             -- bug11772495, reverting the fix for bug9716573
	     -- adding the code below to skip executing Update_Gain_Loss_Ind
	     -- for the budgetary control events
	     --
             BEGIN
               SELECT nvl(xe.budgetary_control_flag, 'N')    --BUG12594203
	         INTO l_budgetary_control_flag
	         FROM xla_events xe
		WHERE xe.application_id = 200
		  AND xe.event_id = l_xla_event_rec.event_id;

	       IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
                 l_log_msg := 'Budgetary Control flag for the Event_ID: '||l_xla_event_rec.event_id||
		              ' is: '||l_budgetary_control_flag;
                 FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME || l_procedure_name, l_log_msg);
               END IF;
	     EXCEPTION
               WHEN OTHERS THEN
	         l_budgetary_control_flag := 'N';
	     END;

	     IF l_budgetary_control_flag = 'N' THEN

               IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
                   l_log_msg := 'Calling procedure AP_Acctg_Prepay_Dist_Pkg.Update_Gain_Loss_Ind';
                   FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME || l_procedure_name, l_log_msg);
               END IF;

               AP_Acctg_Prepay_Dist_Pkg.Update_Gain_Loss_Ind
                                    (l_xla_event_rec,
                                     l_curr_calling_sequence);

               IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
                   l_log_msg := 'Procedure AP_Acctg_Prepay_Dist_Pkg.Updated_Gain_Loss_Ind executed';
                   FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME || l_procedure_name, l_log_msg);
               END IF;

	     END IF;

       END IF;

       -- Added 8288996
       -- Bug 17600307 added Payment maturity
       --bug 9495694, uncommented payment_cleared from the following condition
       IF (l_xla_event_rec.event_type_code IN ('PAYMENT CREATED', 'PAYMENT CLEARED'
                                               ,'PAYMENT MATURED')
		   AND g_base_currency_code <> l_check_curr_code ) THEN
         --Bug 8670681

             IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
                 l_log_msg := 'Calling procedure AP_ACCTG_PAY_ROUND_PKG.Final_Cash';
                 FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME || l_procedure_name, l_log_msg);
             END IF;

             AP_ACCTG_PAY_ROUND_PKG.Final_Cash
                                  (l_xla_event_rec,
                                   l_curr_calling_sequence);

             IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
                 l_log_msg := 'Procedure procedure AP_ACCTG_PAY_ROUND_PKG.Final_Cash executed';
                 FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME || l_procedure_name, l_log_msg);
             END IF;

       END IF; --8288996 ends
    EXCEPTION
      WHEN OTHERS THEN
       IF SQLCODE = -20100 THEN
          --10412623, inserting error into xla_accounting_errors
          --Bug 12918263 
          
           SELECT DISTINCT xeg.entity_id
                , xeg.ledger_id
             INTO l_entity_id
                , l_ledger_id
             FROM xla_events_gt xeg
            WHERE xeg.event_id       = l_xla_event_rec.event_id
              AND xeg.application_id = 200;
          
          XLA_ACCOUNTING_ERR_PKG.build_message('SQLAP',
                             'AP_ACCTG_EVENT_SKIPPED',
                              l_entity_id,
                              l_xla_event_rec.event_id,
                              l_ledger_id,
                              NULL,
                              NULL,
                              xla_accounting_pkg.g_parent_request_id);

          l_log_msg := 'Check_id '||l_xla_event_rec.source_id_int_1||
                       ' will not be accounted due to error: '|| SQLERRM;

          fnd_file.put_line(FND_FILE.LOG, l_log_msg);
          IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
              FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME || l_procedure_name, l_log_msg);
          END IF;
        ELSE --start 16770196
           l_log_msg := 'Transaction_Type '|| l_xla_event_rec.entity_code || 
                        ' Transaction_id =' || l_xla_event_rec.source_id_int_1 ||
                        ' Event_id =' || l_xla_event_rec.event_id ||
                        ' will not be acctd due to (else stmt): '|| SQLERRM;                                             
           fnd_file.put_line(FND_FILE.LOG, l_log_msg); 
            IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN 
               FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME || 
                               l_procedure_name, l_log_msg); 
        END IF; --end 16770196    
         END IF;
    END; --end bug 9936620
  END LOOP;
  CLOSE xla_events_cur;

  -- Logging Infra: Procedure level
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
      l_log_msg := 'End of procedure '|| l_procedure_name;
      FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name||'.end', l_log_msg);
  END IF;

  -- Commenting out the commit since the commit is issued during the post processing of the
  -- accounting process
  -- COMMIT;

EXCEPTION

  WHEN OTHERS THEN
    IF (SQLCODE <> -20001) THEN
      FND_MESSAGE.SET_NAME('SQLAP','AP_DEBUG');
      FND_MESSAGE.SET_TOKEN('ERROR',SQLERRM);
      FND_MESSAGE.SET_TOKEN('CALLING_SEQUENCE', l_curr_calling_sequence);
    END IF;
    APP_EXCEPTION.RAISE_EXCEPTION;

END Do_Pay_Accounting;


-------------------------------------------------------------------------------
-- PROCEDURE Delete_Hist_Dists
-- Procedure to delete the payment history distributions and prepayment
-- application distributions.
--
--------------------------------------------------------------------------------
PROCEDURE Delete_Hist_Dists
     (P_Calling_Sequence     IN   VARCHAR2
     ) IS

  l_curr_calling_sequence    VARCHAR2(2000);

  -- Logging Infra:
  l_procedure_name CONSTANT VARCHAR2(30) := 'Do_Pay_Accounting';
  l_log_msg        FND_LOG_MESSAGES.MESSAGE_TEXT%TYPE;

BEGIN

  l_curr_calling_sequence := 'AP_Accounting_Pay_Pkg.Do_Pay_Accounting<- ' ||
                                      p_calling_sequence;

  -- Logging Infra: Procedure level
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
      l_log_msg := 'Begin of procedure '|| l_procedure_name;
      FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name||'.begin', l_log_msg);
  END IF;


  -- Bug 5098657. Added the where condition for both the delete statements
  DELETE FROM AP_Payment_Hist_Dists
  WHERE  Accounting_Event_ID IN
                   (SELECT Event_ID
                    FROM   XLA_Events_GT
                    WHERE  Entity_Code = 'AP_PAYMENTS');

  DELETE FROM AP_Prepay_App_Dists
  WHERE  Accounting_Event_ID IN
                   (SELECT Event_ID
                    FROM   XLA_Events_GT
                    WHERE  Event_Type_Code IN ('PREPAYMENT APPLICATION ADJ'));


EXCEPTION

  WHEN OTHERS THEN
    IF (SQLCODE <> -20001) THEN
      FND_MESSAGE.SET_NAME('SQLAP','AP_DEBUG');
      FND_MESSAGE.SET_TOKEN('ERROR',SQLERRM);
      FND_MESSAGE.SET_TOKEN('CALLING_SEQUENCE', l_curr_calling_sequence);
    END IF;
    APP_EXCEPTION.RAISE_EXCEPTION;

END Delete_Hist_Dists;



-------------------------------------------------------------------------------
-- Function Get_Casc_Pay_Sum
-- This function gets the sum of the payment amount from the payment history
-- distributions for the given invoice distribution which will be used for
-- payment cascase events
--
--------------------------------------------------------------------------------
FUNCTION Get_Casc_Pay_Sum
     (P_Invoice_Distribution_ID    IN    NUMBER
     ,P_Related_Event_ID           IN    NUMBER
     ,P_Invoice_Payment_ID         IN    NUMBER
     ,P_Calling_Sequence           IN    VARCHAR2
     ) RETURN NUMBER IS

  l_curr_calling_sequence       VARCHAR2(2000);
  l_pay_sum                     NUMBER;

BEGIN

  l_curr_calling_sequence := 'AP_ACCOUNTING_PAY_PKG.Get_Casc_Pay_Sum<- ' ||
                                         P_Calling_Sequence;



  SELECT SUM(APHD.Amount)
  INTO   l_pay_sum
  FROM   AP_Payment_Hist_Dists APHD,
         AP_Payment_History_All APH
  WHERE  APHD.Invoice_Distribution_ID = P_Invoice_Distribution_ID
  AND    APHD.Invoice_Payment_ID = P_Invoice_Payment_ID
  AND    APH.Related_Event_ID = P_Related_Event_ID
  AND    APHD.Payment_History_ID = APH.Payment_History_ID
  AND    APH.Posted_Flag <> 'N'                 -- changed for bug 7560247
  AND    APHD.Pay_Dist_Lookup_Code IN ('CASH', 'AWT'); --bug 9495429

  RETURN NVL(l_pay_sum,0);

END Get_Casc_Pay_Sum;


-------------------------------------------------------------------------------
-- Function Get_Casc_Inv_Dist_Sum
-- This function gets the sum of the paid amount in invoice currency from the
-- payment history distributions for the given invoice distribution which will
-- be used for payment cascase events
--
--------------------------------------------------------------------------------
FUNCTION Get_Casc_Inv_Dist_Sum
     (P_Invoice_Distribution_ID    IN    NUMBER
     ,P_Related_Event_ID           IN    NUMBER
     ,P_Invoice_Payment_ID         IN    NUMBER
     ,P_Calling_Sequence           IN    VARCHAR2
     ) RETURN NUMBER IS

  l_curr_calling_sequence       VARCHAR2(2000);
  l_inv_dist_sum                NUMBER;

BEGIN

  l_curr_calling_sequence := 'AP_ACCOUNTING_PAY_PKG.Get_Casc_Pay_Sum<- ' ||
                                         P_Calling_Sequence;



  SELECT SUM(APHD.Invoice_Dist_Amount)
  INTO   l_inv_dist_sum
  FROM   AP_Payment_Hist_Dists APHD,
         AP_Payment_History_All APH
  WHERE  APHD.Invoice_Distribution_ID = P_Invoice_Distribution_ID
  AND    APHD.Invoice_Payment_ID = P_Invoice_Payment_ID
  AND    APH.Related_Event_ID = P_Related_Event_ID
  AND    APHD.Payment_History_ID = APH.Payment_History_ID
  AND    APH.Posted_Flag <> 'N'                 -- changed for bug 7560247
  AND    APHD.Pay_Dist_Lookup_Code IN ('CASH', 'AWT'); --bug 9495429

  RETURN NVL(l_inv_dist_sum,0);

END Get_Casc_Inv_Dist_Sum;



-------------------------------------------------------------------------------
-- Function Get_Casc_Bank_Curr_Sum
-- This function gets the sum of the paid amount in the bank currency from the
-- payment history distributions for the given invoice distribution which will
-- be used for payment cascase events
--
--------------------------------------------------------------------------------
FUNCTION Get_Casc_Bank_Curr_Sum
     (P_Invoice_Distribution_ID    IN    NUMBER
     ,P_Related_Event_ID           IN    NUMBER
     ,P_Invoice_Payment_ID         IN    NUMBER
     ,P_Calling_Sequence           IN    VARCHAR2
     ) RETURN NUMBER IS

  l_curr_calling_sequence       VARCHAR2(2000);
  l_bank_curr_sum               NUMBER;

BEGIN

  l_curr_calling_sequence := 'AP_ACCOUNTING_PAY_PKG.Get_Casc_Pay_Sum<- ' ||
                                         P_Calling_Sequence;


  SELECT SUM(APHD.Bank_Curr_Amount)
  INTO   l_bank_curr_sum
  FROM   AP_Payment_Hist_Dists APHD,
         AP_Payment_History_All APH
  WHERE  APHD.Invoice_Distribution_ID = P_Invoice_Distribution_ID
  AND    APHD.Invoice_Payment_ID = P_Invoice_Payment_ID
  AND    APH.Related_Event_ID = P_Related_Event_ID
  AND    APHD.Payment_History_ID = APH.Payment_History_ID
  AND    APH.Posted_Flag <> 'N'                 -- changed for bug 7560247
  AND    APHD.Pay_Dist_Lookup_Code IN ('CASH', 'AWT'); --bug 9495429

  RETURN NVL(l_bank_curr_sum,0);

END Get_Casc_Bank_Curr_Sum;



-------------------------------------------------------------------------------
-- Function Get_Casc_Prepay_Sum
-- This function gets the sum of the prepayment amount from the prepay appl payment
-- distributions for the given invoice distribution which will be used for
-- prepayment appl cascase events
--
--------------------------------------------------------------------------------
FUNCTION Get_Casc_Prepay_Sum
     (P_Invoice_Distribution_ID    IN    NUMBER
     ,P_Prepay_App_Dist_ID         IN    NUMBER
     ,P_Calling_Sequence           IN    VARCHAR2
     ) RETURN NUMBER IS

  l_curr_calling_sequence       VARCHAR2(2000);
  l_prepay_sum                  NUMBER;

BEGIN

  l_curr_calling_sequence := 'AP_ACCOUNTING_PAY_PKG.Get_Casc_Prepay_Sum<- ' ||
                                         P_Calling_Sequence;


  SELECT SUM(APAD.Amount)
  INTO   l_prepay_sum
  FROM   AP_Prepay_App_Dists APAD
  WHERE  APAD.Invoice_Distribution_ID = P_Invoice_Distribution_ID
  AND    APAD.Prepay_App_Distribution_ID = P_Prepay_App_Dist_ID
  AND    APAD.Prepay_Dist_Lookup_Code IN ('PREPAY APPL', 'PREPAY APPL REC TAX',
                                          'PREPAY APPL NONREC TAX', 'AWT',
                                          'EXCHANGE RATE VARIANCE');

  RETURN NVL(l_prepay_sum,0);

END Get_Casc_Prepay_Sum;


-------------------------------------------------------------------------------
-- Function Get_Casc_Tax_Diff_Sum
-- This function gets the sum of the tax diff amount from the prepay appl payment
-- distributions for the given invoice distribution which will be used for
-- prepayment appl cascase events
--
--------------------------------------------------------------------------------
FUNCTION Get_Casc_Tax_Diff_Sum
     (P_Invoice_Distribution_ID    IN    NUMBER
     ,P_Prepay_App_Dist_ID         IN    NUMBER
     ,P_Calling_Sequence           IN    VARCHAR2
     ) RETURN NUMBER IS

  l_curr_calling_sequence       VARCHAR2(2000);
  l_tax_diff_sum                NUMBER;

BEGIN

  l_curr_calling_sequence := 'AP_ACCOUNTING_PAY_PKG.Get_Casc_Prepay_Sum<- ' ||
                                         P_Calling_Sequence;


  SELECT SUM(APAD.Amount)
  INTO   l_tax_diff_sum
  FROM   AP_Prepay_App_Dists APAD
  WHERE  APAD.Invoice_Distribution_ID = P_Invoice_Distribution_ID
  AND    APAD.Prepay_App_Distribution_ID = P_Prepay_App_Dist_ID
  AND    APAD.Prepay_Dist_Lookup_Code IN ('TAX DIFF');

  RETURN NVL(l_tax_diff_sum,0);

END Get_Casc_Tax_Diff_Sum;



-------------------------------------------------------------------------------
-- Function Get_Casc_Discount_Sum
-- This function gets the sum of the discount amounts from the payment history
-- distributions for the given invoice distribution which will be used for
-- payment cascase events
--
--------------------------------------------------------------------------------
FUNCTION Get_Casc_Discount_Sum
     (P_Invoice_Distribution_ID    IN    NUMBER
     ,P_Related_Event_ID           IN    NUMBER
     ,P_Invoice_Payment_ID         IN    NUMBER
     ,P_Calling_Sequence           IN    VARCHAR2
     ) RETURN NUMBER IS

  l_curr_calling_sequence       VARCHAR2(2000);
  l_discount_sum                NUMBER;

BEGIN

  l_curr_calling_sequence := 'AP_ACCOUNTING_PAY_PKG.Get_Casc_Discount_Sum<- ' ||
                                         P_Calling_Sequence;


  SELECT SUM(APHD.Amount)
  INTO   l_discount_sum
  FROM   AP_Payment_Hist_Dists APHD,
         AP_Payment_History_All APH
  WHERE  APHD.Invoice_Distribution_ID = P_Invoice_Distribution_ID
  AND    APHD.Invoice_Payment_ID = P_Invoice_Payment_ID
  AND    APH.Related_Event_ID = P_Related_Event_ID
  AND    APHD.Payment_History_ID = APH.Payment_History_ID
  AND    APH.Posted_Flag <> 'N'                 -- changed for bug 7560247
  AND    Pay_Dist_Lookup_Code = 'DISCOUNT';

  RETURN NVL(l_discount_sum,0);

END Get_Casc_Discount_Sum;


-------------------------------------------------------------------------------
-- Function Get_Casc_Inv_Dist_Disc_Sum
-- This function gets the sum of the discount amounts from the payment history
-- distributions for the given invoice distribution which will be used for
-- payment cascase events
--
--------------------------------------------------------------------------------
FUNCTION Get_Casc_Inv_Dist_Disc_Sum
     (P_Invoice_Distribution_ID    IN    NUMBER
     ,P_Related_Event_ID           IN    NUMBER
     ,P_Invoice_Payment_ID         IN    NUMBER
     ,P_Calling_Sequence           IN    VARCHAR2
     ) RETURN NUMBER IS

  l_curr_calling_sequence       VARCHAR2(2000);
  l_discount_sum                NUMBER;

BEGIN

  l_curr_calling_sequence := 'AP_ACCOUNTING_PAY_PKG.Get_Casc_Discount_Sum<- ' ||
                                         P_Calling_Sequence;


  SELECT SUM(APHD.Invoice_Dist_Amount)
  INTO   l_discount_sum
  FROM   AP_Payment_Hist_Dists APHD,
         AP_Payment_History_All APH
  WHERE  APHD.Invoice_Distribution_ID = P_Invoice_Distribution_ID
  AND    APHD.Invoice_Payment_ID = P_Invoice_Payment_ID
  AND    APH.Related_Event_ID = P_Related_Event_ID
  AND    APHD.Payment_History_ID = APH.Payment_History_ID
  AND    APH.Posted_Flag <> 'N'                 -- changed for bug 7560247
  AND    Pay_Dist_Lookup_Code = 'DISCOUNT';

  RETURN NVL(l_discount_sum,0);

END Get_Casc_Inv_Dist_Disc_Sum;



-------------------------------------------------------------------------------
-- Function Get_Casc_Bank_Curr_Disc_Sum
-- This function gets the sum of the discount amounts from the payment history
-- distributions for the given invoice distribution which will be used for
-- payment cascase events
--
--------------------------------------------------------------------------------
FUNCTION Get_Casc_Bank_Curr_Disc_Sum
     (P_Invoice_Distribution_ID    IN    NUMBER
     ,P_Related_Event_ID           IN    NUMBER
     ,P_Invoice_Payment_ID         IN    NUMBER
     ,P_Calling_Sequence           IN    VARCHAR2
     ) RETURN NUMBER IS

  l_curr_calling_sequence       VARCHAR2(2000);
  l_discount_sum                NUMBER;

BEGIN

  l_curr_calling_sequence := 'AP_ACCOUNTING_PAY_PKG.Get_Casc_Discount_Sum<- ' ||
                                         P_Calling_Sequence;


  SELECT SUM(APHD.Bank_Curr_Amount)
  INTO   l_discount_sum
  FROM   AP_Payment_Hist_Dists APHD,
         AP_Payment_History_All APH
  WHERE  APHD.Invoice_Distribution_ID = P_Invoice_Distribution_ID
  AND    APHD.Invoice_Payment_ID = P_Invoice_Payment_ID
  AND    APH.Related_Event_ID = P_Related_Event_ID
  AND    APHD.Payment_History_ID = APH.Payment_History_ID
  AND    APH.Posted_Flag <> 'N'                 -- changed for bug 7560247
  AND    Pay_Dist_Lookup_Code = 'DISCOUNT';

  RETURN NVL(l_discount_sum,0);

END Get_Casc_Bank_Curr_Disc_Sum;



-------------------------------------------------------------------------------
-- Procedure Get_Pay_Sum
-- This procedure gets the sum of the payment amount from the payment history
-- distributions for the given invoice distribution
-- Modified history
-- 1. for bug 5570002, modify the condition of APH.posted_flag to "Y"
--------------------------------------------------------------------------------
PROCEDURE Get_Pay_Sum
     (P_Invoice_Distribution_ID    IN          NUMBER
     ,P_Transaction_Type           IN          VARCHAR2
     ,P_Payment_Sum                OUT NOCOPY  NUMBER
     ,P_Inv_Dist_Sum               OUT NOCOPY  NUMBER
     ,P_Bank_Curr_Sum              OUT NOCOPY  NUMBER
     ,P_Calling_Sequence           IN          VARCHAR2
     ) IS

  l_curr_calling_sequence       VARCHAR2(2000);
  l_pay_sum                     NUMBER;
  l_inv_dist_sum                NUMBER;
  l_bank_curr_sum               NUMBER;

BEGIN

  l_curr_calling_sequence := 'AP_ACCOUNTING_PAY_PKG.Get_Pay_Sum<- ' ||
                                         P_Calling_Sequence;

  IF (P_Transaction_Type IN ('PAYMENT CLEARED', 'PAYMENT CLEARING ADJUSTED')) THEN

      SELECT SUM(APHD.Amount),
             SUM(APHD.Invoice_Dist_Amount),
             SUM(APHD.Bank_Curr_Amount)
      INTO   l_pay_sum,
             l_inv_dist_sum,
             l_bank_curr_sum
      FROM   AP_Payment_Hist_Dists APHD,
             AP_Payment_History_All APH
      WHERE  APHD.Invoice_Distribution_ID in ( /*bug8882706*/
			   select p_invoice_distribution_id from dual
                            union
                           -- awt distributions which are applied on the p_invoice_distribution_id
                           select distinct aid_awt.invoice_distribution_id
                             from ap_invoice_distributions_all aid_awt,
                                  ap_invoice_distributions_all aid_item
                            where 1=1
                              and aid_item.invoice_distribution_id = p_invoice_distribution_id
			      and aid_item.line_type_lookup_code <> 'AWT'
                              and aid_awt.invoice_id = aid_item.invoice_id
                              and aid_awt.awt_related_id = aid_item.invoice_distribution_id
                              and aid_awt.line_type_lookup_code = 'AWT'
                             )
      AND    APHD.Pay_Dist_Lookup_Code IN ('CASH', 'DISCOUNT', 'AWT')
      AND    NVL(APH.Posted_Flag, 'N') IN ('Y', 'S')  		--bug 7614480, added status 'S'
      AND    APH.Payment_History_ID = APHD.Payment_History_ID
      AND    APH.Transaction_Type IN ('PAYMENT CLEARING', 'PAYMENT UNCLEARING',
                                      'PAYMENT CLEARING ADJUSTED')

      AND  NOT EXISTS
          (SELECT 'Event Reversed'
             FROM AP_PAYMENT_HISTORY_ALL APH_REL
            WHERE APH_REL.check_id = APH.check_id --bug9282163
              AND NVL(APH_REL.RELATED_EVENT_ID, APH_REL.ACCOUNTING_EVENT_ID) =
                  NVL(APH.RELATED_EVENT_ID, APH.ACCOUNTING_EVENT_ID)
              AND APH_REL.REV_PMT_HIST_ID IS NOT NULL)
     /*Bug 13908641*/
     AND NOT EXISTS
          (SELECT 'Event Reversed'
             FROM AP_PAYMENT_HISTORY_ALL APH_REL
            WHERE APH_REL.check_id = APH.check_id --bug9282163
              AND APH_REL.REV_PMT_HIST_ID = APH.PAYMENT_HISTORY_ID
              AND APH_REL.REV_PMT_HIST_ID IS NOT NULL); 
      --bug8975671, reversed entries and their reversals shouldn't be considered
  ELSIF (P_Transaction_Type IN ('PAYMENT MATURED', 'PAYMENT MATURITY ADJUSTED')) THEN

      SELECT SUM(APHD.Amount),
             SUM(APHD.Invoice_Dist_Amount),
             SUM(APHD.Bank_Curr_Amount)
      INTO   l_pay_sum,
             l_inv_dist_sum,
             l_bank_curr_sum
      FROM   AP_Payment_Hist_Dists APHD,
             AP_Payment_History_All APH
      WHERE  APHD.Invoice_Distribution_ID in ( /*bug8882706*/
			   select p_invoice_distribution_id from dual
                            union
                           -- awt distributions which are applied on p_invoice_distribution_id
                           select distinct aid_awt.invoice_distribution_id
                             from ap_invoice_distributions_all aid_awt,
                                  ap_invoice_distributions_all aid_item
                            where 1=1
                              and aid_item.invoice_distribution_id = p_invoice_distribution_id
			      and aid_item.line_type_lookup_code <> 'AWT'
                              and aid_awt.invoice_id = aid_item.invoice_id
                              and aid_awt.awt_related_id = aid_item.invoice_distribution_id
                              and aid_awt.line_type_lookup_code = 'AWT'
                             )
      AND    APHD.Pay_Dist_Lookup_Code IN ('CASH', 'DISCOUNT', 'AWT') -- bug8882706
      AND    NVL(APH.Posted_Flag, 'N') IN ('Y', 'S')  		--bug 7614480, added status 'S'
      AND    APH.Payment_History_ID = APHD.Payment_History_ID
      AND    APH.Transaction_Type IN ('PAYMENT MATURITY', 'PAYMENT MATURITY REVERSED',
                                      'PAYMENT MATURITY ADJUSTED')
      AND  NOT EXISTS
          (SELECT 'Event Reversed'
             FROM AP_PAYMENT_HISTORY_ALL APH_REL
            WHERE APH_REL.check_id = APH.check_id --bug9282163
              AND NVL(APH_REL.RELATED_EVENT_ID, APH_REL.ACCOUNTING_EVENT_ID) =
                  NVL(APH.RELATED_EVENT_ID, APH.ACCOUNTING_EVENT_ID)
              AND APH_REL.REV_PMT_HIST_ID IS NOT NULL)
     /*Bug 13908641*/
     AND NOT EXISTS
          (SELECT 'Event Reversed'
             FROM AP_PAYMENT_HISTORY_ALL APH_REL
            WHERE APH_REL.check_id = APH.check_id --bug9282163
              AND APH_REL.REV_PMT_HIST_ID = APH.PAYMENT_HISTORY_ID
              AND APH_REL.REV_PMT_HIST_ID IS NOT NULL); 
      --bug8975671, reversed entries and their reversals shouldn't be considered
  ELSE

      SELECT SUM(APHD.Amount),
             SUM(APHD.Invoice_Dist_Amount),
             SUM(APHD.Bank_Curr_Amount)
      INTO   l_pay_sum,
             l_inv_dist_sum,
             l_bank_curr_sum
      FROM   AP_Payment_Hist_Dists APHD,
             AP_Payment_History_All APH
      WHERE  APHD.Invoice_Distribution_ID in ( /*bug 8882706*/
			   select p_invoice_distribution_id from dual
                            union
                           -- awt distributions which are applied on p_invoice_distribution_id
                           select distinct aid_awt.invoice_distribution_id
                             from ap_invoice_distributions_all aid_awt,
                                  ap_invoice_distributions_all aid_item
                            where 1=1
                              and aid_item.invoice_distribution_id = p_invoice_distribution_id
                              and aid_item.line_type_lookup_code <> 'AWT'
                              and aid_awt.invoice_id = aid_item.invoice_id
                              and aid_awt.awt_related_id = aid_item.invoice_distribution_id                                   and aid_awt.line_type_lookup_code = 'AWT'
                             )
      AND    APHD.Pay_Dist_Lookup_Code IN ('CASH', 'DISCOUNT', 'AWT') -- bug8882706
      AND    NVL(APH.Posted_Flag, 'N') IN ('Y', 'S')  		--bug 7614480, added status 'S'
      AND    APH.Payment_History_ID = APHD.Payment_History_ID
      AND    APH.Transaction_Type IN ('PAYMENT CREATED', 'PAYMENT CANCELLED', 'PAYMENT ADJUSTED',
                                      'MANUAL PAYMENT ADJUSTED', 'UPGRADED MANUAL PMT ADJUSTED',
                                      'REFUND RECORDED', 'REFUND ADJUSTED', 'REFUND CANCELLED',
                                      'MANUAL REFUND ADJUSTED')
      AND  NOT EXISTS
          (SELECT 'Event Reversed'
             FROM AP_PAYMENT_HISTORY_ALL APH_REL
            WHERE APH_REL.check_id = APH.check_id --bug9282163
              AND NVL(APH_REL.RELATED_EVENT_ID, APH_REL.ACCOUNTING_EVENT_ID) =
                  NVL(APH.RELATED_EVENT_ID, APH.ACCOUNTING_EVENT_ID)
              AND APH_REL.REV_PMT_HIST_ID IS NOT NULL)
     /*Bug 13908641*/
     AND NOT EXISTS
          (SELECT 'Event Reversed'
             FROM AP_PAYMENT_HISTORY_ALL APH_REL
            WHERE APH_REL.check_id = APH.check_id --bug9282163
              AND APH_REL.REV_PMT_HIST_ID = APH.PAYMENT_HISTORY_ID
              AND APH_REL.REV_PMT_HIST_ID IS NOT NULL); 
      --bug8975671, reversed entries and their reversals shouldn't be considered
  END IF;

  p_payment_sum := NVL(l_pay_sum,0);
  p_inv_dist_sum := NVL(l_inv_dist_sum,0);
  p_bank_curr_sum := NVL(l_bank_curr_sum,0);

EXCEPTION

  WHEN OTHERS THEN
    IF (SQLCODE <> -20001) THEN
      FND_MESSAGE.SET_NAME('SQLAP','AP_DEBUG');
      FND_MESSAGE.SET_TOKEN('ERROR',SQLERRM);
      FND_MESSAGE.SET_TOKEN('CALLING_SEQUENCE', l_curr_calling_sequence);
    END IF;
    APP_EXCEPTION.RAISE_EXCEPTION;

END Get_Pay_Sum;



-------------------------------------------------------------------------------
-- Procedure Get_Pay_Base_Sum
-- This procedure gets the sum of the payment amount from the payment history
-- distributions for the given invoice distribution
-- Added For Bug 9282465
--------------------------------------------------------------------------------
PROCEDURE Get_Pay_Base_Sum
     (P_Invoice_Distribution_ID    IN          NUMBER
     ,P_Transaction_Type           IN          VARCHAR2
     ,P_Payment_Sum                OUT NOCOPY  NUMBER
     ,P_Inv_Dist_Sum               OUT NOCOPY  NUMBER
     ,P_Bank_Curr_Sum              OUT NOCOPY  NUMBER
     ,P_Calling_Sequence           IN          VARCHAR2
     ) IS

  l_curr_calling_sequence       VARCHAR2(2000);
  l_pay_sum                     NUMBER;
  l_inv_dist_sum                NUMBER;
  l_bank_curr_sum               NUMBER;

BEGIN

  l_curr_calling_sequence := 'AP_ACCOUNTING_PAY_PKG.Get_Pay_Base_Sum<- ' ||
                                         P_Calling_Sequence;

  IF (P_Transaction_Type IN ('PAYMENT CLEARED', 'PAYMENT CLEARING ADJUSTED')) THEN

           SELECT SUM( APHD.Paid_Base_Amount )
                , SUM( APHD.Invoice_Dist_Base_Amount )
                , SUM( APHD.Cleared_Base_Amount )
             INTO l_pay_sum
                , l_inv_dist_sum
                , l_bank_curr_sum
             FROM AP_Payment_Hist_Dists APHD
                , AP_Payment_History_All APH
            WHERE APHD.Invoice_Distribution_ID IN
                  (SELECT p_invoice_distribution_id
                      FROM dual
                     UNION
                  SELECT DISTINCT aid_awt.invoice_distribution_id
                      FROM ap_invoice_distributions_all aid_awt
                         , ap_invoice_distributions_all aid_item
                     WHERE 1                                = 1
                       AND aid_item.invoice_distribution_id = p_invoice_distribution_id
                       AND aid_item.line_type_lookup_code  <> 'AWT'
                       AND aid_awt.invoice_id               = aid_item.invoice_id
                       AND aid_awt.awt_related_id           =
                           aid_item.invoice_distribution_id
                       AND aid_awt.line_type_lookup_code = 'AWT'
                  )
              AND APHD.Pay_Dist_Lookup_Code   IN( 'CASH', 'DISCOUNT', 'AWT' )
              AND NVL( APH.Posted_Flag, 'N' ) IN( 'Y', 'S' )
              AND APH.Payment_History_ID       = APHD.Payment_History_ID
              AND APH.Transaction_Type        IN( 'PAYMENT CLEARING',
                  'PAYMENT UNCLEARING', 'PAYMENT CLEARING ADJUSTED' )
              AND NOT EXISTS
                  (SELECT 'Event Reversed'
                      FROM AP_PAYMENT_HISTORY_ALL APH_REL
                     WHERE APH_REL.check_id = APH.check_id
                       AND NVL( APH_REL.RELATED_EVENT_ID, APH_REL.ACCOUNTING_EVENT_ID )
                                                    = NVL( APH.RELATED_EVENT_ID, APH.ACCOUNTING_EVENT_ID )
                       AND APH_REL.REV_PMT_HIST_ID IS NOT NULL
                  );
  ELSIF (P_Transaction_Type IN ('PAYMENT MATURED', 'PAYMENT MATURITY ADJUSTED')) THEN

           SELECT SUM( APHD.Paid_Base_Amount )
                , SUM( APHD.Invoice_Dist_Base_Amount )
                , SUM( APHD.Cleared_Base_Amount )
             INTO l_pay_sum
                , l_inv_dist_sum
                , l_bank_curr_sum
             FROM AP_Payment_Hist_Dists APHD
                , AP_Payment_History_All APH
            WHERE APHD.Invoice_Distribution_ID IN
                  (SELECT p_invoice_distribution_id
                      FROM dual
                     UNION
                  SELECT DISTINCT aid_awt.invoice_distribution_id
                      FROM ap_invoice_distributions_all aid_awt
                         , ap_invoice_distributions_all aid_item
                     WHERE 1                                = 1
                       AND aid_item.invoice_distribution_id = p_invoice_distribution_id
                       AND aid_item.line_type_lookup_code  <> 'AWT'
                       AND aid_awt.invoice_id               = aid_item.invoice_id
                       AND aid_awt.awt_related_id           =
                           aid_item.invoice_distribution_id
                       AND aid_awt.line_type_lookup_code = 'AWT'
                  )
              AND APHD.Pay_Dist_Lookup_Code   IN( 'CASH', 'DISCOUNT', 'AWT' )
              AND NVL( APH.Posted_Flag, 'N' ) IN( 'Y', 'S' )
              AND APH.Payment_History_ID       = APHD.Payment_History_ID
              AND APH.Transaction_Type        IN( 'PAYMENT MATURITY',
                  'PAYMENT MATURITY REVERSED', 'PAYMENT MATURITY ADJUSTED' )
              AND NOT EXISTS
                  (SELECT 'Event Reversed'
                      FROM AP_PAYMENT_HISTORY_ALL APH_REL
                     WHERE APH_REL.check_id = APH.check_id
                       AND NVL( APH_REL.RELATED_EVENT_ID, APH_REL.ACCOUNTING_EVENT_ID )
                                                    = NVL( APH.RELATED_EVENT_ID, APH.ACCOUNTING_EVENT_ID )
                       AND APH_REL.REV_PMT_HIST_ID IS NOT NULL
                  );
  ELSE

           SELECT SUM( APHD.Paid_Base_Amount )
                , SUM( APHD.Invoice_Dist_Base_Amount )
                , SUM( APHD.Cleared_Base_Amount )
             INTO l_pay_sum
                , l_inv_dist_sum
                , l_bank_curr_sum
             FROM AP_Payment_Hist_Dists APHD
                , AP_Payment_History_All APH
            WHERE APHD.Invoice_Distribution_ID IN
                  (SELECT p_invoice_distribution_id
                      FROM dual
                     UNION
                  SELECT DISTINCT aid_awt.invoice_distribution_id
                      FROM ap_invoice_distributions_all aid_awt
                         , ap_invoice_distributions_all aid_item
                     WHERE 1                                = 1
                       AND aid_item.invoice_distribution_id = p_invoice_distribution_id
                       AND aid_item.line_type_lookup_code  <> 'AWT'
                       AND aid_awt.invoice_id               = aid_item.invoice_id
                       AND aid_awt.awt_related_id           =
                           aid_item.invoice_distribution_id
                       AND aid_awt.line_type_lookup_code = 'AWT'
                  )
              AND APHD.Pay_Dist_Lookup_Code   IN( 'CASH', 'DISCOUNT', 'AWT' )
              AND NVL( APH.Posted_Flag, 'N' ) IN( 'Y', 'S' )
              AND APH.Payment_History_ID       = APHD.Payment_History_ID
              AND APH.Transaction_Type        IN( 'PAYMENT CREATED', 'PAYMENT CANCELLED'
                  , 'PAYMENT ADJUSTED', 'MANUAL PAYMENT ADJUSTED',
                  'UPGRADED MANUAL PMT ADJUSTED', 'REFUND RECORDED', 'REFUND ADJUSTED',
                  'REFUND CANCELLED', 'MANUAL REFUND ADJUSTED' )
              AND NOT EXISTS
                  (SELECT 'Event Reversed'
                      FROM AP_PAYMENT_HISTORY_ALL APH_REL
                     WHERE APH_REL.check_id = APH.check_id
                       AND NVL( APH_REL.RELATED_EVENT_ID, APH_REL.ACCOUNTING_EVENT_ID )
                                                    = NVL( APH.RELATED_EVENT_ID, APH.ACCOUNTING_EVENT_ID )
                       AND APH_REL.REV_PMT_HIST_ID IS NOT NULL
                  );
END IF;

  p_payment_sum := NVL(l_pay_sum,0);
  p_inv_dist_sum := NVL(l_inv_dist_sum,0);
  p_bank_curr_sum := NVL(l_bank_curr_sum,0);

EXCEPTION

  WHEN OTHERS THEN
    IF (SQLCODE <> -20001) THEN
      FND_MESSAGE.SET_NAME('SQLAP','AP_DEBUG');
      FND_MESSAGE.SET_TOKEN('ERROR',SQLERRM);
      FND_MESSAGE.SET_TOKEN('CALLING_SEQUENCE', l_curr_calling_sequence);
    END IF;
    APP_EXCEPTION.RAISE_EXCEPTION;

END Get_Pay_Base_Sum;


-------------------------------------------------------------------------------
-- Function Get_Prepay_Sum
-- This function gets the sum of the prepaid amount from the  prepay appl payment
-- distributions for the given invoice distribution
--
--------------------------------------------------------------------------------
FUNCTION Get_Prepay_Sum
      ( P_Invoice_Distribution_ID    IN    NUMBER
       ,P_Calling_Sequence           IN          VARCHAR2
     ) RETURN NUMBER IS

  l_curr_calling_sequence       VARCHAR2(2000);
  l_prepay_sum                  NUMBER;

BEGIN

  l_curr_calling_sequence := 'AP_ACCOUNTING_PAY_PKG.Get_Prepay_Sum<- ' ||
                                         P_Calling_Sequence;


  SELECT SUM(APAD.Amount)
  INTO   l_prepay_sum
  FROM   AP_Prepay_App_Dists APAD,
         AP_Invoice_Distributions_All AID
  WHERE  APAD.Invoice_Distribution_ID in ( /*bug 8882706*/
			   select p_invoice_distribution_id from dual
                            union
			   /* awt distributions which are applied on the p_invoice_distribution_id*/
                           select distinct aid_awt.invoice_distribution_id
                             from ap_invoice_distributions_all aid_awt,
                                  ap_invoice_distributions_all aid_item
                            where 1=1
                              and aid_item.invoice_distribution_id = p_invoice_distribution_id
			      and aid_item.line_type_lookup_code <> 'AWT'
                              and aid_awt.invoice_id = aid_item.invoice_id
                              and aid_awt.awt_related_id = aid_item.invoice_distribution_id
                              and aid_awt.line_type_lookup_code = 'AWT'
                             )
  AND   APAD.Prepay_App_Distribution_ID = AID.Invoice_Distribution_ID
  AND   NVL(AID.Reversal_Flag, 'N') <> 'Y'  --bug9322001
  AND   APAD.Prepay_Dist_Lookup_Code IN ('PREPAY APPL', 'PREPAY APPL REC TAX',
                                          'PREPAY APPL NONREC TAX', 'AWT',
                                          'EXCHANGE RATE VARIANCE');

  RETURN NVL(l_prepay_sum,0);

END Get_Prepay_Sum;


-------------------------------------------------------------------------------
-- Procedure Get_Prepay_Base_Sum
-- This Procedure gets the sum of the prepaid amounts from the
-- prepay appl payment distributions for the given invoice distribution
-- Added For Bug 9282465
--------------------------------------------------------------------------------
PROCEDURE Get_Prepay_Base_Sum
       (P_Invoice_Distribution_ID    IN          NUMBER
       ,P_Paid_Base_Sum              OUT NOCOPY  NUMBER
       ,P_Inv_Dist_Base_Sum          OUT NOCOPY  NUMBER
       ,P_Clr_Base_Curr_Sum          OUT NOCOPY  NUMBER
       ,P_Calling_Sequence           IN          VARCHAR2)IS

  l_curr_calling_sequence       VARCHAR2(2000);

BEGIN

  l_curr_calling_sequence := 'AP_ACCOUNTING_PAY_PKG.Get_Prepay_Base_Sum<- ' ||
                                         P_Calling_Sequence;

   SELECT SUM( APAD.Base_Amount )
       , SUM( APAD.Base_Amt_At_Prepay_XRate )
       , SUM( APAD.Base_Amt_At_Prepay_Clr_XRate )
    INTO P_Inv_Dist_Base_Sum
       , P_Paid_Base_Sum
       , P_Clr_Base_Curr_Sum
    FROM AP_Prepay_App_Dists APAD
   WHERE APAD.Invoice_Distribution_ID IN
         ( SELECT p_invoice_distribution_id FROM dual
            UNION
           SELECT DISTINCT aid_awt.invoice_distribution_id
             FROM ap_invoice_distributions_all aid_awt
                , ap_invoice_distributions_all aid_item
            WHERE 1                                = 1
              AND aid_item.invoice_distribution_id = p_invoice_distribution_id
              AND aid_item.line_type_lookup_code  <> 'AWT'
              AND aid_awt.invoice_id               = aid_item.invoice_id
              AND aid_awt.awt_related_id           = aid_item.invoice_distribution_id
              AND aid_awt.line_type_lookup_code    = 'AWT'
         )
     AND APAD.Prepay_Dist_Lookup_Code IN( 'PREPAY APPL', 'PREPAY APPL REC TAX',
         'PREPAY APPL NONREC TAX', 'AWT', 'EXCHANGE RATE VARIANCE' );
EXCEPTION

  WHEN OTHERS THEN
    IF (SQLCODE <> -20001) THEN
      FND_MESSAGE.SET_NAME('SQLAP','AP_DEBUG');
      FND_MESSAGE.SET_TOKEN('ERROR',SQLERRM);
      FND_MESSAGE.SET_TOKEN('CALLING_SEQUENCE', l_curr_calling_sequence);
    END IF;
    APP_EXCEPTION.RAISE_EXCEPTION;

END Get_Prepay_Base_Sum;


-------------------------------------------------------------------------------
-- Function Is_Final_Payment
-- Function to check if this payment is the final payment for the given
-- invoice.
-- bug 5623129 Note
--   1.added more debug message
--   2. P_Transaction_Type should match to event type.
--     The payment history transaction type is different from event type
--   3. add AND    APH.Posted_Flag = 'Y'  to get accounted paid amount
-- bug 9495429, removed cascade and cancellation transaction_types
--------------------------------------------------------------------------------
FUNCTION Is_Final_Payment
                 (P_Inv_Rec             IN  r_invoices_info
                 ,P_Payment_Amount      IN  NUMBER
                 ,P_Discount_Amount     IN  NUMBER
                 ,P_Prepay_Amount       IN  NUMBER
                 ,P_Transaction_Type    IN  VARCHAR2
                 ,P_calling_sequence    IN  VARCHAR2
                 ) RETURN BOOLEAN IS

  l_paid_acctd_amt           NUMBER;
  l_prepaid_acctd_amt        NUMBER;
  l_total_paid_amt           NUMBER;
  l_total_prepaid_amt        NUMBER;
  l_final_payment            BOOLEAN := FALSE;
  l_inv_inc_prepay_tot       NUMBER;   --bug8613795
  l_inv_inc_prepay_pay       NUMBER;   --bug8613795
  l_curr_calling_sequence    VARCHAR2(2000);
  l_total_awt                NUMBER; --Bug 9166188

  l_procedure_name CONSTANT VARCHAR2(30) := 'is_final_payment';
  l_log_msg        FND_LOG_MESSAGES.MESSAGE_TEXT%TYPE;

BEGIN


  l_curr_calling_sequence := 'AP_ACCOUNTING_PAY_PKG.Is_Final_Payment<-' ||
                                       P_Calling_Sequence;

	-- add this call for Bug10183934
    IF NOT Is_Upgrade_Proration_Exists(
              P_Rounding_Method => G_FINAL_PAY
	  		 ,P_Inv_Rec                     => p_inv_rec
                         ,P_Invoice_Payment_Id          => NULL
                         ,P_Prepay_Appl_Distribution_Id => NULL
			 ,P_Prepay_Distribution_Id      => NULL
			 ,P_Transaction_Type            => p_transaction_type
			 ,P_Calling_Sequence            => l_curr_calling_sequence
			)
    THEN

		  /* We need to get the paid amount for a particular transaction type
			 as payment hist dists stores paid amounts for all types of
			 payment events. */



		   IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
			  l_log_msg := 'Begin of is_ainal_payment function call and passin parameters are' ||
						   'P_Payment_Amount=' || P_Payment_Amount ||
						   'P_Discount_Amount=' ||P_Discount_Amount ||
						   'P_Prepay_Amount =' || P_Prepay_Amount ||
						   'P_Transaction_Type =' || P_Transaction_Type;
			  FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME || l_procedure_name, l_log_msg);
		   END IF;

                  -- Bug16635197 Added Payment Clearing Adjusted
		  IF (P_Transaction_Type IN ('PAYMENT CLEARED','PAYMENT CLEARING ADJUSTED')) THEN

			  /* Getting the sum of payment distributions to check if this is the final
				 payment */

			  -------------------------------------------------------------------------
			  --  bug 5570002
			  -- 1. Take out the Exchange rate variance consideration
			  --   Because for entered amount, it is 0 always
			  -- 2. comment out the "APH.posted_flag" <> 'N' and
			  --    later change to  "APH.posted_flag" = 'Y'
			  -------------------------------------------------------------------------

			  SELECT SUM(--DECODE(APHD.Pay_Dist_Lookup_Code,
								--'EXCHANGE RATE VARIANCE', -1 * APHD.Amount,
								APHD.Amount)
			  INTO   l_paid_acctd_amt
			  FROM   AP_Payment_Hist_Dists APHD,
					 AP_Invoice_Distributions_All AID,
					 AP_Payment_History_All APH
			  WHERE  AID.Invoice_ID = p_inv_rec.invoice_id
			  AND    AID.Invoice_Distribution_ID = APHD.Invoice_Distribution_ID
			  AND    APHD.Pay_Dist_Lookup_Code IN ('CASH', 'DISCOUNT')  --bug 9265516, removed 'AWT'
			  AND    APH.Posted_Flag IN ('Y', 'S')                      --bug 7614480, added status 'S'
			  AND    APH.Payment_History_ID = APHD.Payment_History_ID
			  AND    APH.Transaction_Type IN ('PAYMENT CLEARING')
			  AND    NOT EXISTS(SELECT 'reversed event'
							   FROM AP_PAYMENT_HISTORY_ALL APH_REV
							  WHERE  APH_REV.check_id = APH.check_id --bug9282163
								AND  nvl(aph_rev.related_event_id, aph_rev.accounting_event_id)
										  = nvl(aph.related_event_id, aph.accounting_event_id)
								AND aph_rev.rev_pmt_hist_id IS NOT NULL); --bug 7614480, added not exists


			  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
				 l_log_msg := 'transaction type is payment clearing and ' ||
						   'l_paid_acctd_amt=' || l_paid_acctd_amt;
				 FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME || l_procedure_name, l_log_msg);
			  END IF;
                  -- Bug 16635197 Added Payment Maturity Adjusted
		  ELSIF (P_Transaction_Type IN ('PAYMENT MATURED','PAYMENT MATURITY ADJUSTED')) THEN

			  /* Getting the sum of payment distributions to check if this is the final
				 payment */
			  SELECT SUM(--DECODE(APHD.Pay_Dist_Lookup_Code,
								--'EXCHANGE RATE VARIANCE', -1 * APHD.Amount,
								APHD.Amount)
			  INTO   l_paid_acctd_amt
			  FROM   AP_Payment_Hist_Dists APHD,
					 AP_Invoice_Distributions_All AID,
					 AP_Payment_History_All APH
			  WHERE  AID.Invoice_ID = p_inv_rec.invoice_id
			  AND    AID.Invoice_Distribution_ID = APHD.Invoice_Distribution_ID
			  AND    APHD.Pay_Dist_Lookup_Code IN ('CASH', 'DISCOUNT')  --bug 9265516, removed 'AWT'
			  AND    APH.Posted_Flag IN ('Y', 'S')                      --bug 7614480, added status 'S'
			  AND    APH.Payment_History_ID = APHD.Payment_History_ID
			  AND    APH.Transaction_Type IN ('PAYMENT MATURITY')
			  AND NOT EXISTS(SELECT 'reversed event'
							   FROM AP_PAYMENT_HISTORY_ALL APH_REV
							  WHERE  APH_REV.check_id = APH.check_id --bug9282163
								AND  nvl(aph_rev.related_event_id, aph_rev.accounting_event_id)
										  = nvl(aph.related_event_id, aph.accounting_event_id)
								AND aph_rev.rev_pmt_hist_id IS NOT NULL); --bug 7614480, added not exists

			 IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
				 l_log_msg := 'transaction type is payment matruity and ' ||
						   'l_paid_acctd_amt=' || l_paid_acctd_amt;
				 FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME || l_procedure_name, l_log_msg);
			  END IF;


		  ELSE

			  /* Getting the sum of payment distributions to check if this is the final
				 payment */
			  SELECT SUM(--DECODE(APHD.Pay_Dist_Lookup_Code,
								--'EXCHANGE RATE VARIANCE', -1 * APHD.Amount,
								 APHD.Amount)
			  INTO   l_paid_acctd_amt
			  FROM   AP_Payment_Hist_Dists APHD,
					 AP_Invoice_Distributions_All AID,
					 AP_Payment_History_All APH,
					 AP_INVOICE_PAYMENTS_ALL AIP
			  WHERE  AID.Invoice_ID = p_inv_rec.invoice_id
			  AND    AID.Invoice_Distribution_ID = APHD.Invoice_Distribution_ID
			  AND    APHD.Pay_Dist_Lookup_Code IN ('CASH', 'DISCOUNT')  --bug 9265516, removed 'AWT'
			  AND    APH.Posted_Flag IN ('Y', 'S')                      --bug 7614480, added status 'S'
			  AND    APH.Payment_History_ID = APHD.Payment_History_ID
			  AND    APH.Transaction_Type IN ('PAYMENT CREATED', 'MANUAL PAYMENT ADJUSTED',
											  'UPGRADED MANUAL PMT ADJUSTED', 'REFUND RECORDED',
											  'MANUAL REFUND ADJUSTED')
			  AND    aphd.invoice_payment_id = aip.invoice_payment_id
			  AND    aip.invoice_id = aid.invoice_id
			  AND    aip.check_id = aph.check_id
			  AND    nvl(aip.reversal_flag, 'N') <> 'Y'; --bug 7614480, added not exists

			   IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
				 l_log_msg := 'transaction type is payment created or others ' ||
						   'l_paid_acctd_amt=' || l_paid_acctd_amt;
				 FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME || l_procedure_name, l_log_msg);
			   END IF;

		  END IF;


		  /* Get the total prepaid amount from the ap_prepay_app_dists table */
		  /* bug9322001, changed the where clause to remove conditions on accounting */
		  /* events, and checked only for reversal flag on the Prepay Application dists */
		  SELECT SUM(APAD.Amount)
		  INTO   l_prepaid_acctd_amt
		  FROM   AP_Prepay_App_Dists APAD,
				 AP_Invoice_Distributions_All AID,
			 AP_PREPAY_HISTORY_ALL APPH
		  WHERE  AID.Invoice_ID = p_inv_rec.invoice_id
		  AND    AID.Invoice_Distribution_ID = APAD.Invoice_Distribution_ID
		  AND    APAD.prepay_history_id = APPH.PREPAY_HISTORY_ID
		  AND    APAD.Prepay_Dist_Lookup_Code IN ('PREPAY APPL', 'PREPAY APPL REC TAX',
												  'PREPAY APPL NONREC TAX')  --bug 9265516, removed 'AWT'
		  AND NOT EXISTS( SELECT 'reversed prepay application'
							FROM ap_invoice_distributions_all aidp
						   WHERE aidp.invoice_distribution_id = APAD.prepay_app_distribution_id
							 AND aidp.reversal_flag = 'Y');			--bug 7614480, added not exists


		  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
				 l_log_msg := 'there is a prepay application and  ' ||
						   'l_prepaid_acctd_amt =' || l_prepaid_acctd_amt;
				 FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME || l_procedure_name, l_log_msg);
		  END IF;


		  IF (p_inv_rec.invoice_currency_code <> p_inv_rec.payment_currency_code) THEN

			  l_total_prepaid_amt := GL_Currency_API.Convert_Amount(
											p_inv_rec.invoice_currency_code,
											p_inv_rec.payment_currency_code,
											p_inv_rec.payment_cross_rate_date,
											'EMU FIXED',
											NVL(l_prepaid_acctd_amt,0)
											   + NVL(p_prepay_amount,0));

		  ELSE

			 l_total_prepaid_amt := NVL(l_prepaid_acctd_amt,0) + NVL(p_prepay_amount,0);

		  END IF;

		  -- bug8613795

		  SELECT NVL(SUM(AID.amount), 0)
			INTO l_inv_inc_prepay_tot
			FROM ap_invoice_distributions_all AID
		   WHERE AID.invoice_id = p_inv_rec.invoice_id
			 AND AID.line_type_lookup_code        IN ('PREPAY','REC_TAX','NONREC_TAX')
			 AND AID.prepay_distribution_id       IS NOT NULL
			 AND AID.invoice_includes_prepay_flag = 'Y';
                  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
		    l_log_msg := 'Total amount of distributions having invoice_includes_prepay_flag as Y ' ||
			        'l_inv_inc_prepay_pay =' || l_inv_inc_prepay_pay;
		    FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME || l_procedure_name, l_log_msg);
		  END IF;

		  IF (p_inv_rec.invoice_currency_code <> p_inv_rec.payment_currency_code) THEN

			  l_inv_inc_prepay_pay := GL_Currency_API.Convert_Amount(
											p_inv_rec.invoice_currency_code,
											p_inv_rec.payment_currency_code,
											p_inv_rec.payment_cross_rate_date,
											'EMU FIXED',
											l_inv_inc_prepay_tot);
		  /* Bug 13373457*/									
     	          ELSE
                   l_inv_inc_prepay_pay := l_inv_inc_prepay_tot;
		  END IF;

		--Bug 9166188

		  SELECT nvl(sum(amount),0) into l_total_awt
			FROM ap_invoice_distributions_all aid
		   WHERE aid.invoice_id= p_inv_rec.invoice_id
			 AND aid.line_type_lookup_code ='AWT';

		  IF (p_inv_rec.invoice_currency_code <> p_inv_rec.payment_currency_code) THEN

			  l_total_awt := GL_Currency_API.Convert_Amount(
											p_inv_rec.invoice_currency_code,
											p_inv_rec.payment_currency_code,
											p_inv_rec.payment_cross_rate_date,
											'EMU FIXED',
											l_total_awt);
		  END IF;

		--Bug 9166188



		  l_total_paid_amt := NVL(l_paid_acctd_amt,0) + NVL(p_payment_amount,0)
										  + NVL(p_discount_amount,0);


		  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
			l_log_msg := 'Now total paid amount = l_paid_acctd_amt + p_payment_amount + p_discount_amount and' ||
						 ' l_total_paid_amt =' || l_total_paid_amt ||
						 'compare invoice amount either with ' ||
						 'p_inv_rec.pay_curr_invoice_amount' || p_inv_rec.pay_curr_invoice_amount ||
						 'p_inv_rec.invoice_amount ' || p_inv_rec.invoice_amount ||
						 'l_total_awt '||l_total_awt;

			FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME || l_procedure_name, l_log_msg);
		  END IF;

		  --bug8613795
		  --Bug 9166188
		  IF (nvl(p_inv_rec.pay_curr_invoice_amount, p_inv_rec.invoice_amount) -
				   nvl(l_inv_inc_prepay_pay,0) + nvl(l_total_awt,0)
						  = nvl(l_total_paid_amt,0) - nvl(l_total_prepaid_amt,0)) THEN

			l_final_payment := TRUE;

			IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
			  l_log_msg := 'This is a final payment after comparison';
			  FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME || l_procedure_name, l_log_msg);
			END IF;

		  END IF;

     END IF; -- Is_Upgrade_Proration_Exists

  RETURN l_final_payment;

END Is_Final_Payment;

-------------------------------------------------------------------------------
-- FUNCTION Get_Base_Amount RETURN NUMBER
-- Converts the given amount to base amount depending on the exchange rate type

-- Parameters
   ----------
   -- Amount - Amount to convert
   -- Currency_Code - Currency code to convert from
   -- Base_Currency_Code - Currency Code to convert to
   -- Exchange_Rate_Type - Type of exchange rate
   -- Exchange_Rate_Date - Date the conversion is happening
   -- Exchange_Rate - The Exchange rate between the two currencies
   -- bug 5623129 note
   --   1. add more debug message
-------------------------------------------------------------------------------
FUNCTION Get_Base_Amount
                 (P_amount              IN  NUMBER
                 ,P_currency_code       IN  VARCHAR2
                 ,P_base_currency_code  IN  VARCHAR2
                 ,P_exchange_rate_type  IN  VARCHAR2
                 ,P_exchange_rate_date  IN  DATE
                 ,P_exchange_rate       IN  NUMBER
                 ,P_calling_sequence    IN  VARCHAR2
                 ) RETURN NUMBER IS

  l_base_amount              NUMBER := 0 ;
  l_curr_calling_sequence    VARCHAR2(2000);

  l_procedure_name CONSTANT VARCHAR2(30) := 'Get_Base_Amount';
  l_log_msg        FND_LOG_MESSAGES.MESSAGE_TEXT%TYPE;

BEGIN

  l_curr_calling_sequence := 'AP_ACCOUNTING_PAY_PKG.Get_Base_Amount<-'
                             || P_calling_sequence;



  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
      l_log_msg := 'Begin of get_base_amount and parameters are' ||
                   'p_amount=' || nvl(p_amount, 0) ||
                   'P_currency_code =' || P_currency_code ||
                   'P_base_currency_code =' || P_base_currency_code ||
                   'P_exchange_rate_type =' || P_exchange_rate_type ||
                   'P_exchange_rate_date =' || P_exchange_rate_date ||
                   'P_exchange_rate  =' || P_exchange_rate ;
      FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME || l_procedure_name, l_log_msg);
  END IF;

  IF ( P_currency_code = P_base_currency_code ) THEN

    IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
      l_log_msg := 'base currency code = transaction currency code';
      FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME || l_procedure_name, l_log_msg);

    END IF;

    l_base_amount := AP_UTILITIES_PKG.AP_ROUND_CURRENCY(P_amount,
                                                        P_base_currency_code);

  ELSIF ( P_exchange_rate_type <> 'User'
            AND GL_Currency_API.Is_Fixed_Rate(P_currency_code,
                                    P_base_currency_code,
                                    P_exchange_rate_date) = 'Y' ) THEN

    IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN

      l_log_msg := 'exchange rate type is not user and it is a fixed rate';
      FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME || l_procedure_name, l_log_msg);

    END IF;

    l_base_amount := GL_Currency_API.Convert_Amount(P_currency_code,
                                                    P_base_currency_code,
                                                    P_exchange_rate_date,
                                                    P_exchange_rate_type,
                                                    P_amount) ;
  ELSE

     IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN

      l_log_msg := 'not a fix rate, and not a same currency code';
      FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME || l_procedure_name, l_log_msg);

    END IF;


    l_base_amount := AP_Utilities_Pkg.AP_Round_Currency
                                      (P_amount * NVL(P_exchange_rate, 1),
                                       P_base_currency_code) ;

  END IF;

  RETURN l_base_amount ;

EXCEPTION
  WHEN GL_CURRENCY_API.NO_RATE THEN
    RAISE_APPLICATION_ERROR(-20010, 'Could not find fixed rate between'
       || P_currency_code || ' and ' || P_base_currency_code || ' on '
       || to_char(P_exchange_rate_date) );

END Get_Base_Amount;

-- bug9256922
PROCEDURE Prorate_Historical_Dists (P_calling_sequence VARCHAR2) IS

  CURSOR Aprvd_UnPrtd_His_Inv IS
  SELECT ai.invoice_id
    FROM ap_invoice_distributions_all aid,
         ap_invoices_all ai,
         xla_events_gt xe,
         xla_events xle,
         financials_system_params_all fsp
   WHERE xe.event_type_code IN('PREPAYMENT APPLIED',   'PREPAYMENT UNAPPLIED')
     AND xe.event_status_code NOT IN('N',   'P')
     AND aid.accounting_event_id = xe.event_id
     AND aid.prepay_distribution_id IS NOT NULL
     AND aid.invoice_id = ai.invoice_id
     AND aid.org_id = fsp.org_id
     AND EXISTS
        (SELECT 1
           FROM gl_period_statuses glps
          WHERE glps.application_id = 200
            AND glps.set_of_books_id = ai.set_of_books_id
            AND nvl(glps.adjustment_period_flag,    'N') = 'N'
            AND ai.gl_date BETWEEN glps.start_date
                               AND glps.end_date
            AND glps.migration_status_code = 'U')
     AND xle.application_id = 200
     AND xle.event_id = xe.event_id
     AND xle.upg_batch_id IS NOT NULL
     AND xle.upg_batch_id <> -9999
     AND ((fsp.purch_encumbrance_flag = 'Y' AND
           aid.match_status_flag = 'A') OR
          (fsp.purch_encumbrance_flag = 'N' AND
           aid.match_status_flag IN ('A','T')))
     AND nvl(aid.posted_flag,   'N') <> 'Y'
     AND aid.historical_flag = 'Y';
 

  -- bug 12686836,
  -- the R12 logic for creation of the prepay app dists does not
  -- prorate the prepayment application or unapplication distributions
  -- which are encumbered.
  --
  -- In case an 11i historical prepayment application or unapplication 
  -- encumbered but unaccounted invoice distribution is upgraded to R12,
  -- this API is unable to create the ap_prepay_history_all/ap_prepay_app_dists
  -- data for those prepayment application and unapplication distributions.
  --
  -- Logic will be added to momentarily set the encumbered flag to 'N',
  -- before calling prepay_dist_appl, after that the encumbered flag would
  -- be reset to the original value
  -- 
  CURSOR prepay_dist_encumbered(p_invoice_id NUMBER) IS 
  SELECT aid.invoice_distribution_id
    FROM ap_invoice_distributions_all aid
   WHERE aid.invoice_id = p_invoice_id
     AND aid.line_type_lookup_code IN ('PREPAY','REC_TAX','NONREC_TAX')
     AND (aid.prepay_distribution_id IS NOT NULL OR
          aid.prepay_tax_parent_id IS NOT NULL)
     AND NVL(aid.posted_flag, 'N') <> 'Y'
     AND aid.encumbered_flag = 'Y'
     AND aid.historical_flag = 'Y'
     AND aid.bc_event_id IS NULL;
  

  TYPE num_tab IS TABLE OF NUMBER INDEX BY BINARY_INTEGER;
  l_inv_id_tab             NUM_TAB;
  l_log_msg                FND_LOG_MESSAGES.MESSAGE_TEXT%TYPE;
  l_procedure_name         CONSTANT VARCHAR2(30) := 'Prorate_Historical_Dists';
  --bug12686836
  l_prepay_dist_tab        NUM_TAB;
  l_invoice_id             AP_INVOICES_ALL.Invoice_ID%TYPE;
  l_rowcount                 NUMBER;
  l_debug_info               LONG;
  l_curr_calling_sequence    VARCHAR2(2000);

BEGIN

  l_curr_calling_sequence := 'AP_ACCOUNTING_PAY_PKG.Get_Base_Amount<-'
                             || P_calling_sequence;

  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
    l_log_msg := 'Begin of the Procedure Prorate_Historical_Dists';
    FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME || l_procedure_name, l_log_msg);
  END IF;

  l_debug_info := 'before the loop for invoices begins';
  OPEN Aprvd_UnPrtd_His_Inv;
  LOOP
    FETCH Aprvd_UnPrtd_His_Inv
    BULK COLLECT INTO l_inv_id_tab LIMIT 1000;

    
    IF l_inv_id_tab.COUNT > 0 THEN
  
      l_debug_info := 'more than one invoice fetched for processing';
      FOR i IN l_inv_id_tab.FIRST..l_inv_id_tab.LAST LOOP

        BEGIN

          IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
            l_log_msg := 'Processing the Invoice_id '||l_inv_id_tab(i);
            FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME || l_procedure_name, l_log_msg);
          END IF;
      
          l_debug_info := 'processing the invoice_id '||l_inv_id_tab(i);
          l_invoice_id := l_inv_id_tab(i);

          --bug12686836
          l_debug_info := 'buffering all the encumbered prepayment application and '||
                          'unapplication distributions into l_prepay_dist_tab ';
          OPEN prepay_dist_encumbered(l_invoice_id);
          FETCH prepay_dist_encumbered BULK COLLECT INTO l_prepay_dist_tab;
          CLOSE prepay_dist_encumbered;

         l_debug_info := 'before the savepoint and call to prepay dist appl';

          SAVEPOINT Before_Invoice;

          l_debug_info := 'Updating the encumbered flag on the invoice distributions '||
                          'table to ''N'' ';
          --bug12686836
          FORALL j IN l_prepay_dist_tab.FIRST..l_prepay_dist_tab.LAST
            UPDATE ap_invoice_distributions_all AID
               SET aid.encumbered_flag = 'N'
             WHERE aid.invoice_id = l_invoice_id
               AND aid.invoice_distribution_id = l_prepay_dist_tab(j);

          l_rowcount := SQL%ROWCOUNT;
          IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
            l_log_msg := 'Number of records updated to not encumbered '||l_rowcount;
            FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME || l_procedure_name, l_log_msg);
          END IF;
         
 
          Ap_Acctg_Prepay_Dist_Pkg.Prepay_Dist_Appl
            (l_invoice_id,
             l_curr_calling_sequence);

         l_debug_info := 'after the call to prepay dist appl, next restoring '||
                         'encumbered flag for all the historical encumbered '||
                         'prepayment application and unapplication distributions';
         
          --bug12686836
          FORALL j IN l_prepay_dist_tab.FIRST..l_prepay_dist_tab.LAST
            UPDATE ap_invoice_distributions_all AID
               SET aid.encumbered_flag = 'Y'
             WHERE aid.invoice_id = l_invoice_id
               AND aid.invoice_distribution_id = l_prepay_dist_tab(j);

          l_rowcount := SQL%ROWCOUNT;
          IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
            l_log_msg := 'Number of records updated back to encumbered '||l_rowcount;
            FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME || l_procedure_name, l_log_msg);
          END IF;


        EXCEPTION
          WHEN OTHERS THEN
            IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
              l_log_msg := 'Encountered an Exception '||SQLERRM||
                           'while Processing the Invoice_id '||l_inv_id_tab(i);
              FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME || l_procedure_name, l_log_msg);
            END IF;
            ROLLBACK TO Before_Invoice;
        END;

      END LOOP;
    END IF;
    EXIT WHEN Aprvd_UnPrtd_His_Inv%NOTFOUND;
  END LOOP;
  CLOSE Aprvd_UnPrtd_His_Inv;

  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
    l_log_msg := 'End of the Procedure Prorate_Historical_Dists';
    FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME || l_procedure_name, l_log_msg);
  END IF;


EXCEPTION
  WHEN OTHERS THEN
    IF (SQLCODE <> -20001) THEN
      FND_MESSAGE.SET_NAME('SQLAP','AP_DEBUG');
      FND_MESSAGE.SET_TOKEN('ERROR',SQLERRM);
      FND_MESSAGE.SET_TOKEN('CALLING_SEQUENCE', l_curr_calling_sequence);
    END IF;
  APP_EXCEPTION.RAISE_EXCEPTION;
END;

-------------------------------------------------------------------------------
-- Function Is_Final_Event
-- Function to check if the current event is the final pay/prepay event for the invoice
--
-- 1. check if related unaccounted events(other checks or prepayments for the same invoice)
--    exist that haven't been picked up for accounting
-- 2. if not, then if p_xla_event_rec.event_id IS NULL then it's a prepayment application case,
--    as only in the case of invoice validation event_id is stamped after rounding, so
--    get the maximum distribution for rounding
-- 3. if p_xla_event_rec.event_id IS NOT NULL then get the maximum accounting_event_id
--    from among the related payment, payment adjusted and the prepay adjustment events for rounding
--------------------------------------------------------------------------------

FUNCTION Is_Final_Event
                 (p_inv_rec             IN  r_invoices_info
                 ,p_xla_event_rec       IN  r_xla_event_info
                 ,p_prepay_app_dist_id  IN  AP_INVOICE_DISTRIBUTIONS_ALL.INVOICE_DISTRIBUTION_ID%TYPE
                 ,P_calling_sequence    IN  VARCHAR2
                 ) RETURN BOOLEAN IS

  l_final_event            BOOLEAN := FALSE;
  l_unacctg_events_exist   VARCHAR2(1) := 'Y';
  l_max_prepay_app_dist_id AP_INVOICE_DISTRIBUTIONS_ALL.INVOICE_DISTRIBUTION_ID%TYPE;
  l_acctg_event_id         XLA_EVENTS.EVENT_ID%TYPE;

  l_curr_calling_sequence    VARCHAR2(2000);

  l_procedure_name CONSTANT VARCHAR2(30) := 'is_final_event';
  l_log_msg        FND_LOG_MESSAGES.MESSAGE_TEXT%TYPE;

BEGIN

  l_curr_calling_sequence := 'AP_ACCOUNTING_PAY_PKG.Is_Final_Payment<-' ||
                                       P_Calling_Sequence;

   IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
      l_log_msg := 'Begin of is_final_event function call and parameters are' ||
                   'p_inv_rec.invoice_id =' || p_inv_rec.invoice_id ||
                   'p_xla_event_rec.event_type_code =' || p_xla_event_rec.event_type_code ||
                   'p_prepay_app_dist_id =' || p_prepay_app_dist_id;
      FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME || l_procedure_name, l_log_msg);
   END IF;

  SELECT DECODE(COUNT(*), 0, 'N', 'Y')
  INTO l_unacctg_events_exist
  FROM DUAL
  WHERE EXISTS(
           SELECT 'unreversed, unaccounted payment not selected for accounting'
            FROM ap_payment_history_all aph
               , ap_invoice_payments_all aip
               , xla_event_types_b xet
               , xla_event_types_b xet_rel
               , xla_events xe
           WHERE      aip.invoice_id           = p_inv_rec.invoice_id
                  AND aph.check_id             = aip.check_id
                  AND xe.event_id              = aph.accounting_event_id
                  AND xet.event_type_code      = p_xla_event_rec.event_type_code
                  AND xet_rel.event_class_code = xet.event_class_code
                  AND xet_rel.event_type_code  = xe.event_type_code
                  AND xet.application_id       = 200
                  AND xet_rel.application_id   = 200
                  AND xe.application_id        = 200
                  AND aph.posted_flag          = 'N'
                  AND NOT EXISTS(
                           SELECT 'reversed event'
                             FROM ap_payment_history_all aph_rev
                            WHERE nvl(aph_rev.related_event_id, aph_rev.accounting_event_id)
                                                                 = nvl(aph.related_event_id, aph.accounting_event_id)
                              AND aph_rev.check_id = aph.check_id  /* bug12909730 */
                              AND aph_rev.rev_pmt_hist_id IS NOT NULL)
          UNION
          -- prepay application
           SELECT 'unreversed, unvalidated prepayment application yet to be validated'
            FROM ap_invoice_distributions_all aid,
                 ap_invoices_all ai --added for bug13334090
           WHERE aid.invoice_id = p_inv_rec.invoice_id
                 --AND nvl(aid.match_status_flag, 'N') = 'N' -- commented for bug13334090              
                  AND aid.invoice_id = ai.invoice_id
                  AND AP_INVOICES_UTILITY_PKG.get_approval_status(ai.invoice_id, 
                                                                  ai.invoice_amount, 
                                                                  ai.payment_status_flag, 
                                                                  ai.invoice_type_lookup_code) 
                             NOT IN ('APPROVED', 'CANCELLED', 'AVAILABLE', 'FULL') -- added for bug13334090
                  AND aid.prepay_distribution_id IS NOT NULL
                  AND nvl(aid.reversal_flag, 'N') <> 'Y' 
                  AND nvl(aid.encumbered_flag, 'N') <> 'Y' -- added for bug13334090
                  AND aid.posted_flag <> 'Y'               -- added for bug13334090
                  AND p_xla_event_rec.event_id IS NOT NULL -- added for bug13334090
          UNION
          -- prepay application adjustment
          SELECT 'unreversed, unaccounted prepay adjustment not selected for accounting'
            FROM ap_prepay_history_all apph
           WHERE apph.posted_flag = 'N'
                  AND apph.invoice_id = p_inv_rec.invoice_id
                  AND apph.invoice_adjustment_event_id IS NOT NULL
                  AND NOT EXISTS(
                           SELECT 'reversed event'
                             FROM ap_invoice_distributions_all aid_rel
                            WHERE aid_rel.invoice_id = apph.invoice_id
                              AND aid_rel.accounting_event_id = apph.related_prepay_app_event_id
                              AND aid_rel.reversal_flag = 'Y'));

  IF l_unacctg_events_exist = 'N' THEN
     IF p_xla_event_rec.event_id IS NULL THEN    -- for prepayment application events
                  SELECT MAX(AID.INVOICE_DISTRIBUTION_ID)
                           INTO l_max_prepay_app_dist_id
                    FROM ap_invoice_distributions_all aid
                   WHERE aid.invoice_id              = p_inv_rec.invoice_id
                     --bug13334090 - commented below and added conditions on posted_flag and enc flag
                     --AND aid.match_status_flag       = 'S'
                     AND aid.posted_flag <> 'Y'
                     AND NVL(aid.encumbered_flag, 'N') <> 'Y'
                     AND aid.prepay_distribution_id IS NOT NULL
                     AND NVL(reversal_flag, 'N')    <> 'Y';

     ELSE   -- payments, prepay application adj
                  SELECT MAX(accounting_event_id)
                     INTO l_acctg_event_id
                  FROM
                    (
                   -- payment
                   SELECT MAX(aph.accounting_event_id) accounting_event_id
                     FROM ap_payment_history_all aph
                            , ap_invoice_payments_all aip
                            , xla_event_types_b xet
                            , xla_event_types_b xet_rel
                            , xla_events xe
                            , xla_events_gt xgt
                     WHERE aip.invoice_id           = p_inv_rec.invoice_id
                     AND aph.check_id             = aip.check_id
                     AND xe.event_id              = aph.accounting_event_id
                     AND xet.event_type_code      = p_xla_event_rec.event_type_code
                     AND xet_rel.event_class_code = xet.event_class_code
                     AND xet_rel.event_type_code  = xe.event_type_code
                     AND xgt.event_id             = xe.event_id
                     AND xet.application_id       = 200
                     AND xet_rel.application_id   = 200
                     AND xe.application_id        = 200
                     AND aph.posted_flag          = 'S'
                     AND NOT EXISTS(
                                    SELECT 'reversed event'
                                      FROM ap_payment_history_all aph_rev
                                     WHERE nvl(aph_rev.related_event_id, aph_rev.accounting_event_id)
                                                     = nvl(aph.related_event_id, aph.accounting_event_id)
                                       AND aph_rev.check_id = aph.check_id /* bug12909730 */
                                       AND aph_rev.rev_pmt_hist_id IS NOT NULL)
                  UNION
                    -- prepay adjustment
                     SELECT MAX(apph.accounting_event_id) accounting_event_id
                      FROM ap_prepay_history_all apph,
                           xla_events_gt xgt
                     WHERE xgt.event_id             = apph.accounting_event_id
                       AND apph.posted_flag = 'S'
                       AND apph.invoice_id = p_inv_rec.invoice_id
                       AND apph.invoice_adjustment_event_id IS NOT NULL
                       AND NOT EXISTS(
                                    SELECT 'reversed event'
                                      FROM ap_invoice_distributions_all aid_rel
                                     WHERE aid_rel.invoice_id = apph.invoice_id
                                       AND aid_rel.accounting_event_id = apph.related_prepay_app_event_id
                                       AND aid_rel.reversal_flag = 'Y'));
     END IF;
  ELSE
        -- final payment rounding shouldn't be executed and return
           RETURN FALSE;
  END IF;

  IF p_xla_event_rec.event_id = l_acctg_event_id
     OR  p_prepay_app_dist_id = l_max_prepay_app_dist_id THEN

     l_final_event  := TRUE;
     IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
        l_log_msg := 'Final event='||p_xla_event_rec.event_id||
                     ' OR Final prepay dist='||p_prepay_app_dist_id;
        FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME || l_procedure_name, l_log_msg);
     END IF;
  ELSE

     l_final_event  := FALSE;
     IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
        l_log_msg := 'Event ='||p_xla_event_rec.event_id||' is NOT final event'||
                     ' OR prepay dist='||p_prepay_app_dist_id||' is NOT final prepay dist';
        FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME || l_procedure_name, l_log_msg);
     END IF;
  END IF;

  RETURN l_final_event;

END Is_Final_Event;

/* Bug10183934 - added below procedure to check if there is any upgrade proration
   that particular rounding method is depending on*/
FUNCTION Is_Upgrade_Proration_Exists
( P_Rounding_Method             IN   VARCHAR2
 ,P_Inv_Rec                     IN   r_invoices_info
 ,P_Invoice_Payment_Id          IN   NUMBER
 ,P_Prepay_Appl_Distribution_Id IN   NUMBER
 ,P_Prepay_Distribution_Id      IN   NUMBER
 ,P_Transaction_Type            IN   VARCHAR2
 ,P_Calling_Sequence            IN   VARCHAR2
 ) RETURN BOOLEAN IS

   l_procedure_name CONSTANT VARCHAR2(30) := 'is_upgrade_proration_exists';

   l_curr_calling_sequence    VARCHAR2(2000);
   l_log_msg                  FND_LOG_MESSAGES.MESSAGE_TEXT%TYPE;

   l_upg_appl_actg_for_prep_dist  NUMBER;
   l_upg_pay_actg_for_inv         NUMBER;
   l_upg_appl_actg_for_inv        NUMBER;
   l_upg_pay_all_actg_for_inv     NUMBER;

BEGIN

   l_curr_calling_sequence := 'AP_ACCOUNTING_PAY_PKG.is_upgrade_proration_exists<-' ||P_Calling_Sequence;

   IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
      l_log_msg := 'Check is_upgrade_proration_exists for : '|| P_Rounding_Method;
      FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME || l_procedure_name, l_log_msg);
   END IF;


   IF P_Rounding_Method=G_FINAL_APPL THEN

        -- check for non-reversed, upgrade prepay application accounting w.r.t prepay_distribution_id
        select count(1)
          into l_upg_appl_actg_for_prep_dist
          from ap_invoice_distributions_all aid,
               xla_ae_headers xah
         where aid.prepay_distribution_id = p_prepay_distribution_id
           and nvl(aid.reversal_flag, 'N') <> 'Y'
           and aid.accounting_event_id = xah.event_id
           and aid.set_of_books_id = xah.ledger_id
           and xah.event_type_code IN ('PREPAYMENT APPLIED', 'PREPAYMENT UNAPPLIED')
           and xah.accounting_entry_status_code = 'F'
           and xah.upg_batch_id is not null
           and xah.upg_batch_id <> -9999
           and xah.application_id = 200
           and rownum = 1;

        IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
          l_log_msg := 'l_upg_appl_actg_for_prep_dist = ' ||l_upg_appl_actg_for_prep_dist;
          FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME || l_procedure_name, l_log_msg);
        END IF;

        IF l_upg_appl_actg_for_prep_dist = 0 THEN
           return FALSE;
        ELSE
           return TRUE;
        END IF;

   ELSIF P_Rounding_Method=G_FINAL_PAY THEN

        IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
          l_log_msg := 'p_transaction_type = ' ||p_transaction_type;
          FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME || l_procedure_name, l_log_msg);
        END IF;

        IF (p_transaction_type IN ('PAYMENT CLEARED', 'PAYMENT CLEARING ADJUSTED')) THEN

          -- check for non-reversed, upgrade payment clearing accounting w.r.t invoice_id
          select count(1)
            into l_upg_pay_actg_for_inv
            from ap_payment_history_all aph,
                 ap_system_parameters_all asp,
                 xla_ae_headers xah
           where aph.check_id in
                     (select aip.check_id
                        from ap_invoice_payments_all aip
                       where aip.invoice_id = p_inv_rec.invoice_id
                         and nvl(aip.reversal_flag, 'N') <> 'Y'
                     )
             and aph.transaction_type IN ('PAYMENT CLEARING',
                                          'PAYMENT UNCLEARING',
                                          'PAYMENT CLEARING ADJUSTED')
             and not exists (select 'event reversed'
                               from ap_payment_history_all aph_rel
                              where aph_rel.check_id = aph.check_id
                                and nvl(aph_rel.related_event_id, aph_rel.accounting_event_id) =
                                          nvl(aph.related_event_id, aph.accounting_event_id)
                                and aph_rel.rev_pmt_hist_id is not null)
             and aph.org_id = asp.org_id
             and aph.accounting_event_id = xah.event_id
             and xah.ledger_id = asp.set_of_books_id
             and xah.accounting_entry_status_code = 'F'
             and xah.upg_batch_id is not null
             and xah.upg_batch_id <> -9999
             and xah.application_id = 200
             and rownum = 1;

        ELSIF (p_transaction_type IN ('PAYMENT MATURED', 'PAYMENT MATURITY ADJUSTED')) THEN

          -- check for non-reversed, upgrade payment maturity accounting w.r.t invoice_id
          select count(1)
            into l_upg_pay_actg_for_inv
            from ap_payment_history_all aph,
                 ap_system_parameters_all asp,
                 xla_ae_headers xah
           where aph.check_id in
                     (select aip.check_id
                        from ap_invoice_payments_all aip
                       where aip.invoice_id = p_inv_rec.invoice_id
                         and nvl(aip.reversal_flag, 'N') <> 'Y'
                     )
             and aph.transaction_type IN ('PAYMENT MATURITY',
                                          'PAYMENT MATURITY REVERSED',
                                          'PAYMENT MATURITY ADJUSTED')
             and not exists (select 'event reversed'
                               from ap_payment_history_all aph_rel
                              where aph_rel.check_id = aph.check_id
                                and nvl(aph_rel.related_event_id, aph_rel.accounting_event_id) =
                                          nvl(aph.related_event_id, aph.accounting_event_id)
                                and aph_rel.rev_pmt_hist_id is not null)
             and aph.org_id = asp.org_id
             and aph.accounting_event_id = xah.event_id
             and xah.ledger_id = asp.set_of_books_id
             and xah.accounting_entry_status_code = 'F'
             and xah.upg_batch_id is not null
             and xah.upg_batch_id <> -9999
             and xah.application_id = 200
             and rownum = 1;

        ELSE

          -- check for non-reversed, upgrade payment accounting w.r.t invoice_id
          select count(1)
            into l_upg_pay_actg_for_inv
            from ap_payment_history_all aph,
                 ap_system_parameters_all asp,
                 xla_ae_headers xah
           where aph.check_id in
                     (select aip.check_id
                        from ap_invoice_payments_all aip
                       where aip.invoice_id = p_inv_rec.invoice_id
                         and nvl(aip.reversal_flag, 'N') <> 'Y'
                     )
             and aph.transaction_type IN ('PAYMENT CREATED', 'PAYMENT CANCELLED', 'PAYMENT ADJUSTED',
                                          'MANUAL PAYMENT ADJUSTED', 'UPGRADED MANUAL PMT ADJUSTED',
                                          'REFUND RECORDED', 'REFUND ADJUSTED', 'REFUND CANCELLED',
                                          'MANUAL REFUND ADJUSTED')
             and not exists (select 'event reversed'
                               from ap_payment_history_all aph_rel
                              where aph_rel.check_id = aph.check_id
                                and nvl(aph_rel.related_event_id, aph_rel.accounting_event_id) =
                                          nvl(aph.related_event_id, aph.accounting_event_id)
                                and aph_rel.rev_pmt_hist_id is not null)
             and aph.org_id = asp.org_id
             and aph.accounting_event_id = xah.event_id
             and xah.ledger_id = asp.set_of_books_id
             and xah.accounting_entry_status_code = 'F'
             and xah.upg_batch_id is not null
             and xah.upg_batch_id <> -9999
             and xah.application_id = 200
             and rownum = 1;

        END IF;

        IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
           l_log_msg := 'l_upg_pay_actg_for_inv = ' ||l_upg_pay_actg_for_inv;
           FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME || l_procedure_name, l_log_msg);
        END IF;

        -- check for non-reversed, upgrade prepay application accounting w.r.t invoice_id
        select count(1)
          into l_upg_appl_actg_for_inv
          from ap_invoice_distributions_all aid,
               xla_ae_headers xah
         where aid.invoice_id = p_inv_rec.invoice_id
           and nvl(aid.reversal_flag, 'N') <> 'Y'
           and aid.accounting_event_id = xah.event_id
           and aid.set_of_books_id = xah.ledger_id
           and xah.event_type_code IN ('PREPAYMENT APPLIED', 'PREPAYMENT UNAPPLIED')
           and xah.accounting_entry_status_code = 'F'
           and xah.upg_batch_id is not null
           and xah.upg_batch_id <> -9999
           and xah.application_id = 200
           and rownum = 1;

        IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
          l_log_msg := 'l_upg_appl_actg_for_inv = ' ||l_upg_appl_actg_for_inv;
          FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME || l_procedure_name, l_log_msg);
        END IF;

        IF l_upg_pay_actg_for_inv = 0 and l_upg_appl_actg_for_inv = 0 then
           return FALSE;
        ELSE
           return TRUE;
        END IF;

   ELSIF P_Rounding_Method=G_COMPARE_PAY THEN
        -- check for upgrade payment accounting w.r.t invoice_payment_id
        select count(1)
          into l_upg_pay_all_actg_for_inv
          from ap_payment_history_all aph,
               ap_system_parameters_all asp,
               xla_ae_headers xah
         where aph.check_id in
                   (select aip.check_id
                      from ap_invoice_payments_all aip
                     where aip.invoice_id = p_inv_rec.invoice_id --bug 16829544
                   )
           and aph.org_id = asp.org_id
           and aph.accounting_event_id = xah.event_id
           and xah.ledger_id = asp.set_of_books_id
           and xah.accounting_entry_status_code = 'F'
           and xah.upg_batch_id is not null
           and xah.upg_batch_id <> -9999
           and xah.application_id = 200
           and rownum = 1;

        IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
           l_log_msg := 'l_upg_pay_all_actg_for_inv = ' ||l_upg_pay_all_actg_for_inv;
           FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME || l_procedure_name, l_log_msg);
        END IF;

        IF l_upg_pay_all_actg_for_inv = 0 then
           return FALSE;
        ELSE
           return TRUE;
        END IF;

   ELSIF P_Rounding_Method=G_TOTAL_PAY THEN

	return FALSE;

   ELSIF P_Rounding_Method=G_TOTAL_APPL THEN

	return FALSE;

   END IF;

END Is_Upgrade_Proration_Exists;

END AP_ACCOUNTING_PAY_PKG;
/

COMMIT;
EXIT;
