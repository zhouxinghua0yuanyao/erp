REM +==================================================================+
REM |                Copyright (c) 1999, 2013 Oracle Corporation
REM |                   Redwood Shores, California, USA
REM |                        All rights reserved.
REM +==================================================================+
REM |  Name
REM |    apacrndb.pls
REM |
REM |  Description - Package AP_ACCTG_PAY_ROUND_PKG
REM |  This package is the starting point for the rounding calculations.
REM |  After the Payment Hist Distributions and Prepayment Appl Pay Dists
REM |  are created, this rounding procedure is called to check for different
REM |  types of rounding and insert into the respective tables.
REM |
REM |  History
REM |    Created By:  Haritha Redreddy  (06/04/04)
REM +==================================================================+
REM dbdrv: sql ~PROD ~PATH ~FILE none none none package &phase=plb \
REM dbdrv: checkfile(120.22.12010000.54=120.66.12020000.9):~PROD:~PATH:~FILE

SET VERIFY OFF
WHENEVER SQLERROR EXIT FAILURE ROLLBACK
WHENEVER OSERROR EXIT FAILURE ROLLBACK

CREATE OR REPLACE PACKAGE BODY AP_ACCTG_PAY_ROUND_PKG AS
/* $Header: apacrndb.pls 120.66.12020000.9 2013/12/23 09:33:34 imandal ship $ */

  -- Logging Infra
  G_CURRENT_RUNTIME_LEVEL      NUMBER                := FND_LOG.G_CURRENT_RUNTIME_LEVEL;
  G_LEVEL_UNEXPECTED           CONSTANT NUMBER       := FND_LOG.LEVEL_UNEXPECTED;
  G_LEVEL_ERROR                CONSTANT NUMBER       := FND_LOG.LEVEL_ERROR;
  G_LEVEL_EXCEPTION            CONSTANT NUMBER       := FND_LOG.LEVEL_EXCEPTION;
  G_LEVEL_EVENT                CONSTANT NUMBER       := FND_LOG.LEVEL_EVENT;
  G_LEVEL_PROCEDURE            CONSTANT NUMBER       := FND_LOG.LEVEL_PROCEDURE;
  G_LEVEL_STATEMENT            CONSTANT NUMBER       := FND_LOG.LEVEL_STATEMENT;
  G_MODULE_NAME                CONSTANT VARCHAR2(50) := 'AP.PLSQL.AP_ACCTG_PAY_ROUND_PKG.';
  -- Logging Infra

-------------------------------------------------------------------------------
-- PROCEDURE  Do_Rounding
-- This procedure calls different rounding procedures based on the event type.
-- Single point of entry for the rounding calculations.
--
--------------------------------------------------------------------------------
PROCEDURE Do_Rounding
     (P_XLA_Event_Rec    IN   ap_accounting_pay_pkg.r_xla_event_info
     ,P_Pay_Hist_Rec     IN   ap_accounting_pay_pkg.r_pay_hist_info
     ,P_Clr_Hist_Rec     IN   ap_accounting_pay_pkg.r_pay_hist_info
     ,P_Inv_Rec          IN   ap_accounting_pay_pkg.r_invoices_info
     ,P_Inv_Pay_Rec      IN   ap_acctg_pay_dist_pkg.r_inv_pay_info
     ,P_Prepay_Inv_Rec   IN   ap_accounting_pay_pkg.r_invoices_info
     ,P_Prepay_Hist_Rec  IN   AP_ACCTG_PREPAY_DIST_PKG.r_prepay_hist_info
     ,P_Prepay_Dist_Rec  IN   AP_ACCTG_PREPAY_DIST_PKG.r_prepay_dist_info
     ,P_Calling_Sequence IN   VARCHAR2
     ) IS

  l_curr_calling_sequence     VARCHAR2(2000);
  l_prepay_acctg_amt          NUMBER;
  l_prepay_amt                NUMBER;

  -- Logging Infra:
  l_procedure_name CONSTANT VARCHAR2(30) := 'DO_Rounding';
  l_log_msg        FND_LOG_MESSAGES.MESSAGE_TEXT%TYPE;

  -- bug 6600341
  l_acctg_event_id XLA_EVENTS.EVENT_ID%TYPE;
  exec_final_payment VARCHAR2(1); --bug 7614480
  l_max_prepay_app_dist_id AP_INVOICE_DISTRIBUTIONS_ALL.INVOICE_DISTRIBUTION_ID%TYPE; --bug 7614480

BEGIN

  l_curr_calling_sequence := 'AP_Acctg_Pay_Round_Pkg.Do_Rounding<- ' ||
                                      p_calling_sequence;

  -- Logging Infra: Setting up runtime level
  G_CURRENT_RUNTIME_LEVEL := FND_LOG.G_CURRENT_RUNTIME_LEVEL;

  -- Logging Infra: Procedure level
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
      l_log_msg := 'Begin of procedure '|| l_procedure_name;
      FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name||'.begin', l_log_msg);
  END IF;


  /* Check for rounding only if the invoice currency or payment currency is different than
     the base currency */
  IF (p_inv_rec.invoice_currency_code <> ap_accounting_pay_pkg.g_base_currency_code) OR
     (p_inv_rec.payment_currency_code <> ap_accounting_pay_pkg.g_base_currency_code) THEN

      IF p_prepay_dist_rec.invoice_distribution_id IS NOT NULL THEN

          IF NOT AP_ACCOUNTING_PAY_PKG.Is_Upgrade_Proration_Exists(P_Rounding_Method => AP_ACCOUNTING_PAY_PKG.G_TOTAL_APPL
                                             ,P_Inv_Rec         => p_inv_rec
                                             ,P_Invoice_Payment_id   => NULL
                                             ,P_Prepay_Appl_Distribution_Id => p_prepay_dist_rec.invoice_distribution_id
                                             ,P_Prepay_Distribution_Id => NULL
                                             ,P_Transaction_Type   => p_xla_event_rec.event_type_code
                                             ,P_Calling_Sequence => l_curr_calling_sequence
                                             ) -- add this call for Bug10183934
          THEN

              IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
                  l_log_msg := 'Calling procedure Total_Appl for dist: '
                                    || p_prepay_dist_rec.invoice_distribution_id;
                  FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name, l_log_msg);
              END IF;


              -- Calculate the total application rounding
              Total_Appl(p_xla_event_rec,
                         p_pay_hist_rec,
                         p_clr_hist_rec,
                         p_inv_rec,
                         p_prepay_inv_rec,
                         p_prepay_hist_rec,
                         p_prepay_dist_rec,
                         l_curr_calling_sequence);

              IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
                  l_log_msg := 'Procedure Total_Appl executed';
                  FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name, l_log_msg);
              END IF;

          END IF; -- AP_ACCOUNTING_PAY_PKG.Is_Upgrade_Proration_Exists

          IF NOT AP_ACCOUNTING_PAY_PKG.Is_Upgrade_Proration_Exists(P_Rounding_Method => AP_ACCOUNTING_PAY_PKG.G_FINAL_APPL
                                             ,P_Inv_Rec         => p_inv_rec
                                             ,P_Invoice_Payment_Id => NULL
                                             ,P_Prepay_Appl_Distribution_Id => NULL
                                             ,P_Prepay_Distribution_Id => p_prepay_dist_rec.prepay_distribution_id
                                             ,P_Transaction_Type   => p_xla_event_rec.event_type_code
                                             ,P_Calling_Sequence => l_curr_calling_sequence
                                             ) -- add this call for Bug10183934
          THEN
              /* Get the prepayment app dists amount that has already been accounted */
              SELECT SUM(-1 * APAD.Amount)
              INTO   l_prepay_acctg_amt
              FROM   AP_Prepay_App_Dists APAD,
                     AP_Invoice_Distributions_All AID
              WHERE  APAD.Prepay_App_Distribution_ID = AID.Invoice_Distribution_ID
              AND    AID.Prepay_Distribution_ID = p_prepay_dist_rec.prepay_distribution_id;


              SELECT AID.Amount
              INTO   l_prepay_amt
              FROM   AP_Invoice_Distributions_All AID
              WHERE  AID.Invoice_Distribution_ID = p_prepay_dist_rec.prepay_distribution_id;


              -- Check for final application rounding only if this prepayment has been
              -- fully applied
              IF (l_prepay_acctg_amt = l_prepay_amt) THEN

                  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
                      l_log_msg := 'Calling procedure Final_Appl for prepay dist: '
                                      || p_prepay_dist_rec.prepay_distribution_id;
                      FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name, l_log_msg);
                  END IF;

                  Final_Appl
                     (p_xla_event_rec,
                      p_pay_hist_rec,
                      p_clr_hist_rec,
                      p_inv_rec,
                      p_prepay_inv_rec,
                      p_prepay_hist_rec,
                      p_prepay_dist_rec,
                      l_curr_calling_sequence);

                  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
                      l_log_msg := 'Procedure Final_Appl executed';
                      FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name, l_log_msg);
                  END IF;

              END IF;

          END IF; -- AP_ACCOUNTING_PAY_PKG.Is_Upgrade_Proration_Exists

      ELSE

          IF NOT AP_ACCOUNTING_PAY_PKG.Is_Upgrade_Proration_Exists(P_Rounding_Method => AP_ACCOUNTING_PAY_PKG.G_TOTAL_PAY
                                             ,P_Inv_Rec         => p_inv_rec
                                             ,P_Invoice_Payment_Id     => p_inv_pay_rec.invoice_payment_id
                                             ,P_Prepay_Appl_Distribution_Id => NULL
                                             ,P_Prepay_Distribution_Id => NULL
                                             ,P_Transaction_Type   => p_xla_event_rec.event_type_code
                                             ,P_Calling_Sequence => l_curr_calling_sequence
                                             ) -- add this call for Bug10183934
          THEN

              IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
                  l_log_msg := 'Calling procedure Total_Pay for invoice: '
                                    || p_inv_rec.invoice_id;
                  FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name, l_log_msg);
              END IF;


              -- Calculate the total payment rounding
              Total_Pay(p_xla_event_rec,
                        p_pay_hist_rec,
                        p_inv_rec,
                        p_inv_pay_rec,
                        l_curr_calling_sequence);

              IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
                  l_log_msg := 'Procedure Total_Pay executed';
                  FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name, l_log_msg);
              END IF;

          END IF;

          IF NOT AP_ACCOUNTING_PAY_PKG.Is_Upgrade_Proration_Exists(P_Rounding_Method => AP_ACCOUNTING_PAY_PKG.G_COMPARE_PAY
                                             ,P_Inv_Rec         => p_inv_rec
                                             ,P_Invoice_Payment_Id     => p_inv_pay_rec.invoice_payment_id
                                             ,P_Prepay_Appl_Distribution_Id => NULL
                                             ,P_Prepay_Distribution_Id => NULL
                                             ,P_Transaction_Type   => p_xla_event_rec.event_type_code
                                             ,P_Calling_Sequence => l_curr_calling_sequence
                                             ) -- add this call for Bug10183934
          THEN

              IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
                  l_log_msg := 'Calling procedure Compare_Pay for invoice: '
                                    || p_inv_rec.invoice_id;
                  FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name, l_log_msg);
              END IF;

              -- Calculate the payment to maturity, payment to clearing and
              -- maturity to clearing rounding
              Compare_Pay(p_xla_event_rec,
                          p_pay_hist_rec,
                          p_inv_rec,
                          p_inv_pay_rec,
                          l_curr_calling_sequence);

              IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
                  l_log_msg := 'Procedure Compare_Pay executed';
                  FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name, l_log_msg);
              END IF;

           END IF; -- AP_ACCOUNTING_PAY_PKG.Is_Upgrade_Proration_Exists

      END IF; -- if prepayment event type

          /* Calculating the final payment rounding only when the invoice is fully paid */
          /* bug 9108925 -- the logic to check if the current event is the final pay/prepay event
                 for the invoice has been moved to AP_ACCOUNTING_PAY_PKG, function Is_Final_Event for
                 common reference */
          IF AP_Accounting_Pay_Pkg.Is_Final_Payment(p_inv_rec,
                                                                                                 0, -- payment amt
                                                                                                 0, -- discount taken
                                                                                                 0, -- prepay amount
                                                                                                 p_xla_event_rec.event_type_code,
                                                                                                 l_curr_calling_sequence)
                AND AP_ACCOUNTING_PAY_PKG.Is_Final_Event(p_inv_rec,
                                                                                                 p_xla_event_rec,
                                                                                                 P_Prepay_Dist_Rec.Invoice_Distribution_Id,
                                                                                                 l_curr_calling_sequence) THEN

                   IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
                          l_log_msg := 'Calling procedure Final_Pay for invoice: '
                                                                || p_inv_rec.invoice_id;
                          FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name, l_log_msg);
                   END IF;

                  --bug 9108925, commented the following, as is_final_event would take care of
                  --this condition

                  /*  IF p_xla_event_rec.event_id = l_acctg_event_id
                        OR  p_prepay_dist_rec.invoice_distribution_id = l_max_prepay_app_dist_id THEN */
                        -- bug 6600341 --bug 7614480, added the OR condition

                   -- Calculate the final payment rounding to relieve the liability on the
                   -- invoice fully.
                  Final_Pay(p_xla_event_rec,
                                        p_pay_hist_rec,
                                        p_clr_hist_rec,
                                        p_inv_rec,
                                        p_inv_pay_rec,
                                        p_prepay_inv_rec,
                                        p_prepay_hist_rec,
                                        p_prepay_dist_rec,
                                        l_curr_calling_sequence);

                  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
                          l_log_msg := 'Procedure Final_Pay executed';
                          FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name, l_log_msg);
                  END IF;

          ELSE -- bug 6600341 contd
                  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
                          l_log_msg := 'Procedure Final_Pay for invoice not called: '
                                                                || p_inv_rec.invoice_id;
                          FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name,l_log_msg);
                  END IF;
                  --    END IF; -- bug 6600341 end
          END IF;

  END IF;

  -- Logging Infra: Procedure level
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
      l_log_msg := 'End of procedure '|| l_procedure_name;
      FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name||'.end', l_log_msg);
  END IF;


EXCEPTION
  WHEN OTHERS THEN
     IF (SQLCODE = -20100) THEN
        RAISE_APPLICATION_ERROR(-20100, SQLERRM);
     ELSIF (SQLCODE <> -20001) THEN
        FND_MESSAGE.SET_NAME('SQLAP','AP_DEBUG');
        FND_MESSAGE.SET_TOKEN('ERROR',SQLERRM);
        FND_MESSAGE.SET_TOKEN('CALLING_SEQUENCE', l_curr_calling_sequence);
     END IF;
     APP_EXCEPTION.RAISE_EXCEPTION;
END Do_Rounding;



-------------------------------------------------------------------------------
-- PROCEDURE  Final_Pay
-- This procedure calculates the rounding amount needed to relieve liability
-- when a final payment is made on a foreign currency invoice and creates
-- a final payment rounding if the amount is not fully relieved.
--
--------------------------------------------------------------------------------
PROCEDURE Final_Pay
     (P_XLA_Event_Rec    IN   ap_accounting_pay_pkg.r_xla_event_info
     ,P_Pay_Hist_Rec     IN   ap_accounting_pay_pkg.r_pay_hist_info
     ,P_Clr_Hist_Rec     IN   ap_accounting_pay_pkg.r_pay_hist_info
     ,P_Inv_Rec          IN   ap_accounting_pay_pkg.r_invoices_info
     ,P_Inv_Pay_Rec      IN   ap_acctg_pay_dist_pkg.r_inv_pay_info
     ,P_Prepay_Inv_Rec   IN   ap_accounting_pay_pkg.r_invoices_info
     ,P_Prepay_Hist_Rec  IN   AP_ACCTG_PREPAY_DIST_PKG.r_prepay_hist_info
     ,P_Prepay_Dist_Rec  IN   AP_ACCTG_PREPAY_DIST_PKG.r_prepay_dist_info
     ,P_Calling_Sequence IN   VARCHAR2
     ) IS

  l_curr_calling_sequence      VARCHAR2(2000);
  l_sum_pay_dist_base_amt      NUMBER;
  l_sum_prepay_base_amt        NUMBER;
  l_total_dist_base_amt        NUMBER;


  -- Bug 5570002 - should exclude the TERV if ERV is excluded
  -- Bug 7314656, added historical flag and accounting_event_id
  CURSOR Invoice_Dists
               (P_Invoice_ID    IN   NUMBER
               ) IS
  SELECT AID.Invoice_Distribution_ID,
         AID.Line_Type_Lookup_Code,
         AID.related_id,
         AID.Amount,
         AID.Base_Amount,
         AID.Invoice_Id,
         AID.accounting_event_id,
         AID.historical_flag
  FROM   AP_Invoice_Distributions_All AID,
         Financials_System_Params_All FSP
  WHERE  AID.Invoice_ID = p_invoice_id
  AND    AID.Line_Type_Lookup_Code NOT IN ('PREPAY', 'AWT', 'ERV', 'TERV')
  AND    AID.Prepay_Distribution_ID IS NULL
  AND    AID.Prepay_Tax_Parent_ID IS NULL  -- For tax dists created in R11.5
  AND    AID.Org_ID = FSP.Org_ID
  --Bug6511672
  /*AND    'INVOICE CANCELLED' <> (SELECT event_type_code
                    FROM   xla_events
                                 WHERE event_id =  AID.accounting_event_id)*/
  --bug6614371
  -- Bug 6712649. Added Credit and Debit memo cancelled
  AND NOT EXISTS (SELECT 1
                  FROM   xla_events
                  WHERE  event_id = AID.accounting_event_id
                  AND    event_type_code IN ('INVOICE CANCELLED', 'PREPAYMENT CANCELLED',
                                             'CREDIT MEMO CANCELLED',
                                             'DEBIT MEMO CANCELLED'));
  /*AND  ((NVL(FSP.Purch_Encumbrance_Flag,'N') = 'N'
             AND AID.Match_Status_Flag IN ('T','A'))
         OR
       ((NVL(FSP.Purch_Encumbrance_Flag,'N') = 'Y'
             AND AID.Match_Status_Flag = 'A')));*/ --bug 7614480


  -- bug7314656, cursor added to check if the current
  -- event is an adjustment event with a net balance 0
  -- under no liability posting method.
  CURSOR c_sum_per_event(p_acct_event_id  NUMBER) IS
  SELECT SUM(amount), count(1)
    FROM ap_invoice_distributions_all aid,
         xla_events evnt,
         ap_system_parameters_all asp
   WHERE aid.accounting_event_id = p_acct_event_id
     AND aid.accounting_event_id = evnt.event_id
     AND evnt.event_type_code IN ('INVOICE ADJUSTED',
                                  'CREDIT MEMO ADJUSTED',
                                  'DEBIT MEMO ADJUSTED',
				  'PREPAYMENT ADJUSTED') -- added for bug#9545528 and 12731687
     AND aid.org_id = asp.org_id
     AND (automatic_offsets_flag = 'N' OR
         (automatic_offsets_flag = 'Y' 
           AND NOT EXISTS	--Bug 17446677
	          (SELECT 1
		   FROM xla_ae_lines xal,
		        xla_ae_headers xah
	 	  WHERE xah.application_id = 200
                    AND xal.application_id = 200
		    AND xah.event_id = evnt.event_id
		    AND xal.ae_header_id = xah.ae_header_id
		    AND xal.application_id = xah.application_id
		    AND xal.accounting_class_code = 'LIABILITY')))
     AND aid.historical_flag = 'Y';

  b_generate_pay_dist    BOOLEAN;
  l_sum_per_event        NUMBER;
  l_dist_count_per_event NUMBER;



  l_max_prepay_rec      AP_PREPAY_APP_DISTS%ROWTYPE;
  l_pad_rec             AP_PREPAY_APP_DISTS%ROWTYPE;
  l_max_pay_rec         AP_PAYMENT_HIST_DISTS%ROWTYPE;
  l_pd_rec              AP_PAYMENT_HIST_DISTS%ROWTYPE;
  l_erv_base_amount     AP_INVOICE_DISTRIBUTIONS_ALL.base_amount%type := 0;

  -- Logging Infra:
  l_procedure_name CONSTANT VARCHAR2(30) := 'Final_Pay';
  l_log_msg        FND_LOG_MESSAGES.MESSAGE_TEXT%TYPE;


BEGIN

  l_curr_calling_sequence := 'AP_Acctg_Pay_Round_Pkg.Final_Pay<- ' ||
                                            p_calling_sequence;

  -- Logging Infra: Procedure level
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
      l_log_msg := 'Begin of procedure '|| l_procedure_name;
      FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name||'.begin', l_log_msg);
  END IF;


  FOR l_inv_dist_rec IN Invoice_Dists(p_inv_rec.invoice_id)
  LOOP

    --bug7314656, added the check to see if the current event is
    --a historical adjustment distribution, with the net balance
    --as 0 under automatic offsets off condition
    IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
        l_log_msg := 'Checking b_generate_pay_dist';
        FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name, l_log_msg);
    END IF;

    b_generate_pay_dist := TRUE;
    IF  nvl(l_inv_dist_rec.historical_flag, 'N') ='Y' THEN
      OPEN c_sum_per_event(l_inv_dist_rec.accounting_event_id);
      FETCH c_sum_per_event into l_sum_per_event, l_dist_count_per_event;

      -- > 0 case is to handled the case that only  1 line in adjustment event and itself amount is 0
      If l_dist_count_per_event > 0 AND l_sum_per_event = 0 THEN
         b_generate_pay_dist := FALSE;
      END IF;

      CLOSE c_sum_per_event;

    END IF;

    IF b_generate_pay_dist THEN
     IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
        l_log_msg := 'b_generate_pay_dist = TRUE for Invoice Distribution : ' ||
                      l_inv_dist_rec.invoice_distribution_id;
        FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name||'.begin', l_log_msg);
     END IF;
    ELSE
     IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
        l_log_msg := 'b_generate_pay_dist = FALSE(No error) for Invoice Distribution : ' ||
                      l_inv_dist_rec.invoice_distribution_id;
       FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name||'.begin', l_log_msg);
    END IF;
   END IF ;

   --bug7314656, proceed to calculate the final payment rounding only
   --if the current event is not an adjustment distribution with a net
   --0 balance under automatic offsets off condition
   IF b_generate_pay_dist then

      IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
          l_log_msg := 'Withi CUR loop: Invoice_Dists: Invoice_Dist_ID = '||
                                     l_inv_dist_rec.invoice_distribution_id;
          FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name, l_log_msg);
      END IF;


      -- Get the base amount from the payment hist distributions table for
      -- this invoice distribution.
      IF (p_xla_event_rec.event_type_code IN
                ('PAYMENT CLEARED', 'PAYMENT CLEARING ADJUSTED')) THEN

          -- bug 5570002 modified add the tax erv
          -- Bug 7138115. Added additional join conditions to improve performance
          SELECT SUM(--DECODE(APHD.Pay_Dist_Lookup_Code,
                              -- 'EXCHANGE RATE VARIANCE', -1*APHD.Invoice_Dist_Base_Amount,
                              -- 'TAX EXCHANGE RATE VARIANCE', -1*APHD.Invoice_Dist_Base_Amount,
                               APHD.Invoice_Dist_Base_Amount)
          INTO   l_sum_pay_dist_base_amt
          FROM   AP_Payment_Hist_Dists APHD,
                 AP_Payment_History_All APH,
                 AP_Invoice_Payments_All AIP
          WHERE ((APHD.Invoice_Distribution_ID = l_inv_dist_rec.invoice_distribution_id
          AND     APHD.Pay_Dist_Lookup_Code IN
                       ('CASH', 'DISCOUNT', 'FINAL PAYMENT ROUNDING'))
          OR     (APHD.Pay_Dist_Lookup_Code='AWT'
          AND     APHD.AWT_Related_ID = l_inv_dist_rec.invoice_distribution_id))
          AND    AIP.Invoice_ID = p_inv_rec.invoice_id
          AND    AIP.Check_ID = APH.Check_ID
          AND    APH.Payment_History_ID = APHD.Payment_History_ID
          AND    AIP.Invoice_payment_id = APHD.Invoice_payment_id -- Bug 8722710
          AND    APH.Transaction_Type IN ('PAYMENT CLEARING', 'PAYMENT UNCLEARING',
                                          'PAYMENT CLEARING ADJUSTED')
          -- bug 9257606, ignore the event/payment if reversed
          AND    NVL(AIP.reversal_flag, 'N') <> 'Y'
          AND    NOT EXISTS (SELECT 'Event Reversed'
                               FROM Ap_Payment_History_All APH_REL
                              WHERE APH_REL.check_id = APH.check_id
                                AND NVL(APH_REL.related_event_id, APH_REL.accounting_event_id) =
                                          NVL(APH.related_event_id, APH.accounting_event_id)
                                AND    APH_REL.rev_pmt_hist_id IS NOT NULL);

      ELSIF (p_xla_event_rec.event_type_code IN
                ('PAYMENT MATURED', 'PAYMENT MATURITY ADJUSTED')) THEN

          -- Bug 7138115. Added additional join conditions to improve performance
          SELECT SUM(--DECODE(APHD.Pay_Dist_Lookup_Code,
                       --        'EXCHANGE RATE VARIANCE', -1*APHD.Invoice_Dist_Base_Amount,
                         --      'TAX EXCHANGE RATE VARIANCE', -1*APHD.Invoice_Dist_Base_Amount,
                               APHD.Invoice_Dist_Base_Amount)
          INTO   l_sum_pay_dist_base_amt
          FROM   AP_Payment_Hist_Dists APHD,
                 AP_Payment_History_All APH,
                 AP_Invoice_Payments_All AIP
          WHERE ((APHD.Invoice_Distribution_ID = l_inv_dist_rec.invoice_distribution_id
          AND     APHD.Pay_Dist_Lookup_Code IN
                       ('CASH', 'DISCOUNT', 'FINAL PAYMENT ROUNDING' ))
          OR     (APHD.Pay_Dist_Lookup_Code='AWT'
          AND     APHD.AWT_Related_ID = l_inv_dist_rec.invoice_distribution_id))
          AND    AIP.Invoice_ID = p_inv_rec.invoice_id
          AND    AIP.Check_ID = APH.Check_ID
          AND    APH.Payment_History_ID = APHD.Payment_History_ID
          AND    AIP.Invoice_payment_id = APHD.Invoice_payment_id -- Bug 8722710
          AND    APH.Transaction_Type IN ('PAYMENT MATURITY', 'PAYMENT MATURITY REVERSED',
                                          'PAYMENT MATURITY ADJUSTED')
          -- bug 9257606, ignore the event/payment if reversed
          AND    NVL(AIP.reversal_flag, 'N') <> 'Y'
          AND    NOT EXISTS (SELECT 'Event Reversed'
                               FROM Ap_Payment_History_All APH_REL
                              WHERE APH_REL.check_id = APH.check_id
                                AND NVL(APH_REL.related_event_id, APH_REL.accounting_event_id) =
                                          NVL(APH.related_event_id, APH.accounting_event_id)
                                AND    APH_REL.rev_pmt_hist_id IS NOT NULL);

      ELSE
          -- bug 5570002 modified need to consider Tax erv
          -- Bug 7138115. Added additional join conditions to improve performance
          SELECT SUM(--DECODE(APHD.Pay_Dist_Lookup_Code,
                       --        'EXCHANGE RATE VARIANCE', -1*APHD.Invoice_Dist_Base_Amount,
                         --      'TAX EXCHANGE RATE VARIANCE', -1*APHD.Invoice_Dist_Base_Amount,
                               APHD.Invoice_Dist_Base_Amount)
          INTO   l_sum_pay_dist_base_amt
          FROM   AP_Payment_Hist_Dists APHD,
                 AP_Payment_History_All APH,
                 AP_Invoice_Payments_All AIP
          WHERE ((APHD.Invoice_Distribution_ID = l_inv_dist_rec.invoice_distribution_id
          AND     APHD.Pay_Dist_Lookup_Code IN
                       ('CASH', 'DISCOUNT', 'FINAL PAYMENT ROUNDING' ))
          OR     (APHD.Pay_Dist_Lookup_Code='AWT'
          AND     APHD.AWT_Related_ID = l_inv_dist_rec.invoice_distribution_id))
          AND    AIP.Invoice_ID = p_inv_rec.invoice_id
          AND    AIP.Check_ID = APH.Check_ID
          AND    AIP.Invoice_payment_id = APHD.Invoice_payment_id -- Bug 8722710
          AND    APH.Payment_History_ID = APHD.Payment_History_ID
          AND    APH.Transaction_Type IN ('PAYMENT CREATED', 'PAYMENT CANCELLED', 'PAYMENT ADJUSTED',
                                          'MANUAL PAYMENT ADJUSTED', 'UPGRADED MANUAL PMT ADJUSTED',
                                          'REFUND RECORDED', 'REFUND ADJUSTED', 'REFUND CANCELLED',
                                          'MANUAL REFUND ADJUSTED')   --bug 10336668
          -- bug 9257606, ignore the event/payment if reversed
          AND    NVL(AIP.reversal_flag, 'N') <> 'Y'
          AND    NOT EXISTS (SELECT 'Event Reversed'
                               FROM Ap_Payment_History_All APH_REL
                              WHERE APH_REL.check_id = APH.check_id
                                AND NVL(APH_REL.related_event_id, APH_REL.accounting_event_id) =
                                          NVL(APH.related_event_id, APH.accounting_event_id)
                                AND    APH_REL.rev_pmt_hist_id IS NOT NULL);

      END IF;

      IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
          l_log_msg := 'Value of l_sum_pay_dist_base_amt = '||l_sum_pay_dist_base_amt;
          FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name, l_log_msg);
      END IF;

      -- Bug 7138115. Added additional join conditions to improve performance
      -- bug 9257606, ignore the event, if reversed
      -- bug 9920036, changed the exists clause added by 9257606, to make it
      -- independent of accounting events
      --
      SELECT SUM(APAD.Base_Amount)
      INTO   l_sum_prepay_base_amt
      FROM   AP_Prepay_App_Dists APAD,
             AP_Prepay_History_All APH
      WHERE ((APAD.Invoice_Distribution_ID = l_inv_dist_rec.invoice_distribution_id
      AND     APAD.Prepay_Dist_Lookup_Code IN ('PREPAY APPL', 'PREPAY APPL REC TAX',
                                               'PREPAY APPL NONREC TAX', 'FINAL PAYMENT ROUNDING')) /*Added 'FINAL PAYMENT ROUNDING' for bug 16187488  */
      OR     (APAD.AWT_Related_ID = l_inv_dist_rec.invoice_distribution_id
      AND     APAD.Prepay_Dist_Lookup_Code = 'AWT'))
      AND     APH.Invoice_ID = p_inv_rec.invoice_id
      AND     APH.Prepay_History_ID = APAD.Prepay_History_ID
      AND     NOT EXISTS (SELECT 'reversed'
                            FROM Ap_Invoice_Distributions_All AID
                           WHERE APAD.Prepay_App_Distribution_ID =
                                                        AID.Invoice_Distribution_ID
                             AND AID.reversal_flag = 'Y');

      l_total_dist_base_amt := NVL(l_sum_pay_dist_base_amt,0) - NVL(l_sum_prepay_base_amt,0);

      -- Check if the total of the accounted base amounts is equal to the base amount
      -- for the distribution. If not create a final payment rounding to relieve
      -- the liability completely.

      ---------------------------------------------------------------------------------
      -- bug 5570002
      -- need to find the ERV/TERV amount of the invoice distribution and
      -- exclude them from the invoide distrbution base amount
      ---------------------------------------------------------------------------------

      IF (  l_inv_dist_rec.line_type_lookup_code in ('ITEM', 'NONREC_TAX','ACCRUAL', 'TRV','IPV', 'TIPV') --bug9398335 ,13043111(added IPV),--Bug16794394 (added TIPV)
            AND l_inv_dist_rec.related_id is not NULL )  THEN
         IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
              l_log_msg := 'possible erv exists for invoice dist type ='  ||
                            l_inv_dist_rec.line_type_lookup_code;
              FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name, l_log_msg);
         END IF;
         
        BEGIN

          SELECT NVL(base_amount, 0)
          INTO      l_erv_base_amount
          FROM  ap_invoice_distributions_all AID
          WHERE AID.line_type_lookup_code in ('ERV', 'TERV')
          AND AID.invoice_id =  l_inv_dist_rec.invoice_id
          AND AID.related_id =  l_inv_dist_rec.invoice_distribution_id;
        EXCEPTION
          WHEN OTHERS THEN
            l_erv_base_amount := 0;

        END;

      END IF;

      IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
          l_log_msg := 'l_inv_dist_rec.base_amount = ' ||
                       l_inv_dist_rec.base_amount ||
                       'invoice dist ERV Amount = ' ||
                       l_erv_base_amount ||
                       'and l_total_dist_base_amt' ||
                       l_total_dist_base_amt;
          FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name, l_log_msg);
      END IF;


      IF (  (l_inv_dist_rec.base_amount + l_erv_base_amount)
             <> l_total_dist_base_amt) THEN

          IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
              l_log_msg := 'Total of accounted base amt is not equal to base amount.'
                           || 'Creating a final payment rounding';
              FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name, l_log_msg);
          END IF;


          /* If this is a prepayment type of event then insert the rounding distribution into
             prepayment dists table. Otherwise insert into payment dists table */
          IF (p_prepay_dist_rec.invoice_distribution_id IS NOT NULL) THEN

             -- Get the prepay appl pay dists info for this distribution
             --bug 7614480
          /* SELECT APAD.*
             INTO   l_max_prepay_rec
             FROM   AP_Prepay_App_Dists APAD
             WHERE  APAD.Prepay_App_Distribution_ID = p_prepay_dist_rec.invoice_distribution_id
             AND    APAD.Prepay_History_ID = p_prepay_hist_rec.prepay_history_id
             AND    APAD.Invoice_Distribution_ID = l_inv_dist_rec.invoice_distribution_id
             AND    Rownum = 1;*/

             l_pad_rec.prepay_history_id := p_prepay_hist_rec.prepay_history_id;
             --commented below for bug13334090 and added new sql to derive event id
             IF p_xla_event_rec.event_id IS NOT NULL THEN
                l_pad_rec.accounting_event_id := p_xla_event_rec.event_id; 
             ELSE
               IF p_prepay_hist_rec.prepay_history_id IS NOT NULL THEN
	        select accounting_event_id
	          into l_pad_rec.accounting_event_id
	          from ap_prepay_history_all
	         where prepay_history_id = p_prepay_hist_rec.prepay_history_id;
               END IF;
             END IF;

             l_pad_rec.invoice_distribution_id := l_inv_dist_rec.invoice_distribution_id;
             l_pad_rec.prepay_app_distribution_id := p_prepay_dist_rec.invoice_distribution_id;
             l_pad_rec.prepay_dist_lookup_code := 'FINAL PAYMENT ROUNDING';

           /*l_pad_rec.prepay_exchange_rate := l_max_prepay_rec.prepay_exchange_rate;
             l_pad_rec.prepay_exchange_rate_type := l_max_prepay_rec.prepay_exchange_rate_type;
             l_pad_rec.prepay_exchange_date := l_max_prepay_rec.prepay_exchange_date;
             l_pad_rec.prepay_pay_exchange_rate := l_max_prepay_rec.prepay_pay_exchange_rate;
             l_pad_rec.prepay_pay_exchange_rate_type := l_max_prepay_rec.prepay_pay_exchange_rate_type;
             l_pad_rec.prepay_pay_exchange_date := l_max_prepay_rec.prepay_pay_exchange_date;
             l_pad_rec.prepay_clr_exchange_rate := l_max_prepay_rec.prepay_clr_exchange_rate;
             l_pad_rec.prepay_clr_exchange_rate_type := l_max_prepay_rec.prepay_clr_exchange_rate_type;
             l_pad_rec.prepay_clr_exchange_date := l_max_prepay_rec.prepay_clr_exchange_date;
             l_pad_rec.awt_related_id := l_max_prepay_rec.awt_related_id;*/ --bug 7614480

             l_pad_rec.amount := 0;
             l_pad_rec.base_amount := -(l_inv_dist_rec.base_amount + l_erv_base_amount - l_total_dist_base_amt);  --bug 7614480 bug8889543
             l_pad_rec.base_amt_at_prepay_xrate := 0;
             l_pad_rec.base_amt_at_prepay_pay_xrate := 0;
             l_pad_rec.base_amt_at_prepay_clr_xrate := 0;


             IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
                 l_log_msg := 'Calling procedure '||
                              'AP_ACCTG_PREPAY_DIST_PKG.Prepay_Dist_Insert';
                 FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name, l_log_msg);
             END IF;

             AP_ACCTG_PREPAY_DIST_PKG.Prepay_Dist_Insert
                                              (l_pad_rec,
                                               l_curr_calling_sequence);

             IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
                 l_log_msg := 'Procedure AP_ACCTG_PREPAY_DIST_PKG.Prepay_Dist_Insert '
                                || 'executed';
                 FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name, l_log_msg);
             END IF;

          ELSE

             -- Get the payment hist info
           /*SELECT APHD.*
             INTO   l_max_pay_rec
             FROM   AP_Payment_Hist_Dists APHD
             WHERE  APHD.Payment_History_ID = p_pay_hist_rec.payment_history_id
             AND    APHD.Invoice_Payment_ID = p_inv_pay_rec.invoice_payment_id
             AND    APHD.Accounting_Event_ID = p_xla_event_rec.event_id
             AND    APHD.Invoice_Distribution_ID = l_inv_dist_rec.invoice_distribution_id
             AND    Rownum = 1;*/ --bug 7614480

             l_pd_rec.invoice_distribution_id := l_inv_dist_rec.invoice_distribution_id;
             l_pd_rec.payment_history_id := p_pay_hist_rec.payment_history_id;
             l_pd_rec.invoice_payment_id := p_inv_pay_rec.invoice_payment_id;
             l_pd_rec.invoice_adjustment_event_id := p_pay_hist_rec.invoice_adjustment_event_id;
             l_pd_rec.accounting_event_id := p_xla_event_rec.event_id;
             l_pd_rec.pay_dist_lookup_code := 'FINAL PAYMENT ROUNDING';
             --l_pd_rec.awt_related_id := l_max_pay_rec.awt_related_id;  --bug 7614480

             l_pd_rec.bank_curr_amount := 0;
             l_pd_rec.invoice_dist_base_amount := l_inv_dist_rec.base_amount + l_erv_base_amount - l_total_dist_base_amt; --bug 8889543
             l_pd_rec.amount := 0;
             l_pd_rec.invoice_dist_amount := 0;
             l_pd_rec.paid_base_amount := 0;
             l_pd_rec.cleared_base_amount := 0;
             l_pd_rec.matured_base_amount := 0;

             IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
                 l_log_msg := 'Calling procedure AP_Acctg_Pay_Dist_Pkg.Pay_Dist_Insert';
                 FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name, l_log_msg);
             END IF;


             AP_Acctg_Pay_Dist_Pkg.Pay_Dist_Insert
                                     (l_pd_rec,
                                      l_curr_calling_sequence);

             IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
                 l_log_msg := 'Procedure AP_Acctg_Pay_Dist_Pkg.Pay_Dist_Insert executed';
                 FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name, l_log_msg);
             END IF;


          END IF;
      END IF;
    END IF;
    l_erv_base_amount := 0; --added for bug  8910300 it shuld be zero after after every loop call
  END LOOP;

  -- Logging Infra: Procedure level
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
      l_log_msg := 'End of procedure '|| l_procedure_name;
      FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name||'.end', l_log_msg);
  END IF;


EXCEPTION
  WHEN OTHERS THEN
    IF (SQLCODE <> -20001) THEN
      FND_MESSAGE.SET_NAME('SQLAP','AP_DEBUG');
      FND_MESSAGE.SET_TOKEN('ERROR',SQLERRM);
      FND_MESSAGE.SET_TOKEN('CALLING_SEQUENCE', l_curr_calling_sequence);
    END IF;
    APP_EXCEPTION.RAISE_EXCEPTION;

END Final_Pay;


-------------------------------------------------------------------------------
-- PROCEDURE  Total_Pay
-- This procedure calculates whether the payment his distribution records for
-- the event fully relieve the different base amounts and then create the
-- total payment rounding if the amounts are not fully relieved
--
--------------------------------------------------------------------------------
PROCEDURE Total_Pay
     (P_XLA_Event_Rec    IN   ap_accounting_pay_pkg.r_xla_event_info
     ,P_Pay_Hist_Rec     IN   ap_accounting_pay_pkg.r_pay_hist_info
     ,P_Inv_Rec          IN   ap_accounting_pay_pkg.r_invoices_info
     ,P_Inv_Pay_Rec      IN   ap_acctg_pay_dist_pkg.r_inv_pay_info
     ,P_Calling_Sequence IN   VARCHAR2
     ) IS

  l_curr_calling_sequence        VARCHAR2(2000);
  l_payment_hist_id              NUMBER;
  l_invoice_dist_id              NUMBER;
  l_inv_adj_event_id             NUMBER;

  l_inv_rate_total_amt           NUMBER := 0;
  l_pay_rate_total_amt           NUMBER := 0;
  l_clr_rate_total_amt           NUMBER := 0;
  l_mat_rate_total_amt           NUMBER := 0;
  l_disc_pay_rate_total_amt      NUMBER := 0;
  l_disc_clr_rate_total_amt      NUMBER := 0;
--  l_err_clr_rate_total_amt       NUMBER := 0;--Bug 16636535
--  l_chrg_clr_rate_total_amt      NUMBER := 0;--Bug 16636535

  l_inv_rate_sum_amt             NUMBER := 0;
  l_pay_rate_sum_amt             NUMBER := 0;
  l_clr_rate_sum_amt             NUMBER := 0;
  l_mat_rate_sum_amt             NUMBER := 0;
  l_disc_pay_rate_sum_amt        NUMBER := 0;
  l_disc_clr_rate_sum_amt        NUMBER := 0;
--  l_err_clr_rate_sum_amt         NUMBER := 0;--Bug 16636535
--  l_chrg_clr_rate_sum_amt        NUMBER := 0;--Bug 16636535

  l_inv_rate_diff_amt            NUMBER := 0;
  l_pay_rate_diff_amt            NUMBER := 0;
  l_clr_rate_diff_amt            NUMBER := 0;
  l_mat_rate_diff_amt            NUMBER := 0;
  l_disc_pay_rate_diff_amt       NUMBER := 0;
  l_disc_clr_rate_diff_amt       NUMBER := 0;
  --l_err_clr_rate_diff_amt        NUMBER := 0; --Bug 16636535
  --l_chrg_clr_rate_diff_amt       NUMBER := 0; --Bug 16636535

  l_pd_rec                       AP_PAYMENT_HIST_DISTS%ROWTYPE;

  -- Logging Infra:
  l_procedure_name CONSTANT VARCHAR2(30) := 'Total_Pay';
  l_log_msg        FND_LOG_MESSAGES.MESSAGE_TEXT%TYPE;
  l_inv_base_amt             NUMBER := 0;--Bug6600117
  l_inv_dist_diff_amt            NUMBER := 0;
  l_inv_amt                  NUMBER := 0;
  l_pay_sum_amt                  NUMBER := 0;
  l_inv_rate_sum_full_amt        NUMBER := 0;
  l_max_dist_id                  NUMBER;

  l_clr_rate_rounding_amt   NUMBER :=0;
  l_mat_rate_rounding_amt NUMBER :=0;
  l_pay_rate_rounding_amt NUMBER :=0;
  l_disc_pay_rounding_amt NUMBER :=0;
  l_disc_clr_rounding_amt NUMBER :=0;
  --l_err_clr_rounding_amt  NUMBER :=0;
  --l_chrg_clr_rounding_amt NUMBER :=0;
  l_inv_dist_rounding_amt NUMBER :=0;

  -- bug 8403738
  l_pre_clr_rate_rounding_amt NUMBER :=0;
  l_pre_mat_rate_rounding_amt NUMBER :=0;
  l_pre_pay_rate_rounding_amt NUMBER :=0;
  l_pre_disc_pay_rounding_amt NUMBER :=0;
  l_pre_disc_clr_rounding_amt NUMBER :=0;
--  l_pre_err_clr_rounding_amt  NUMBER :=0; --Bug 16636535
--  l_pre_chrg_clr_rounding_amt NUMBER :=0; --Bug 16636535
  l_pre_inv_dist_rounding_amt NUMBER :=0;

  l_pay_hist_id  NUMBER;
  l_inv_dist_id NUMBER;
  l_inv_dist_amt NUMBER :=0;
  l_inv_dist_base_amt NUMBER :=0;
  l_inv_mat_base_amt NUMBER :=0;
  l_inv_paid_base_amt NUMBER :=0;
  l_inv_clr_base_amt NUMBER :=0;
  
--vasvenka
  l_inv_dist_amt_disc NUMBER :=0;
  l_inv_dist_base_amt_disc NUMBER :=0;
  l_inv_mat_base_amt_disc NUMBER :=0;
  l_inv_paid_base_amt_disc NUMBER :=0;
  l_inv_clr_base_amt_disc NUMBER :=0;  
--vasvenka

  l_rate_type AP_PAYMENT_HISTORY.Pmt_To_Base_XRate_Type%TYPE; --9849243
  l_rate_date AP_PAYMENT_HISTORY.Pmt_To_Base_XRate_Date%TYPE; --9849243
  l_rate      AP_PAYMENT_HISTORY.Pmt_To_Base_XRate%TYPE; --9849243

BEGIN

  l_curr_calling_sequence := 'AP_ACCTG_PAY_ROUND_PKG.Total_Pay<- ' ||
                                          p_calling_sequence;

  -- Logging Infra: Procedure level
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
      l_log_msg := 'Begin of procedure '|| l_procedure_name;
      FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name||'.begin', l_log_msg);
  END IF;

  IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
         l_log_msg := 'query to fetch largest distribution' ||
                 'for xla event id ' ||  p_xla_event_rec.event_id ||
                 'for payment id ' || p_inv_pay_rec.Invoice_Payment_ID ||
           'and invoice id ' ||  p_inv_rec.invoice_id;
         FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name, l_log_msg);
     END IF;
  -- Get the max of the largest distribution for inserting the
  -- total payment rounding
  BEGIN
  SELECT APHD.Payment_History_ID,
         APHD.Invoice_Distribution_ID,
         APHD.Invoice_Adjustment_Event_ID
  INTO   l_payment_hist_id,
         l_invoice_dist_id,
         l_inv_adj_event_id
  FROM   AP_Payment_Hist_Dists APHD
  WHERE  APHD.Invoice_Distribution_ID =
               (SELECT MAX(APHD1.Invoice_Distribution_ID)
                FROM   AP_Payment_Hist_Dists APHD1
                WHERE  APHD1.Accounting_Event_ID = p_xla_event_rec.event_id
                AND    APHD1.Invoice_Payment_ID = p_inv_pay_rec.invoice_payment_id
                AND    APHD1.Invoice_Distribution_ID IN
                               (SELECT AID.Invoice_Distribution_ID
                                FROM   AP_Invoice_Distributions_All AID
                                WHERE  AID.Invoice_ID = p_inv_rec.invoice_id)
                AND    APHD1.Pay_Dist_Lookup_Code not IN('AWT') --8727277
                AND    ABS(APHD1.Amount) =
                               (SELECT MAX(ABS(APHD2.Amount))
                                FROM   AP_Payment_Hist_Dists APHD2
                                WHERE  APHD2.Accounting_Event_ID = p_xla_event_rec.event_id
                                AND    APHD2.Invoice_Payment_ID = p_inv_pay_rec.invoice_payment_id
                                AND    APHD2.Invoice_Distribution_ID IN
                                       (SELECT AID.Invoice_Distribution_ID
                                        FROM   AP_Invoice_Distributions_All AID
                                        WHERE  AID.Invoice_ID = p_inv_rec.invoice_id)
                                        AND    APHD2.Pay_Dist_Lookup_Code not IN('AWT') --8727277
                                        ))
  AND    APHD.Accounting_Event_ID = p_xla_event_rec.event_id
  AND    APHD.Invoice_Payment_ID = p_inv_pay_rec.invoice_payment_id
  AND    Rownum = 1;
  EXCEPTION
     WHEN NO_DATA_FOUND THEN
        RAISE_APPLICATION_ERROR(-20100, l_procedure_name ||
                                ' no_record_in_aphd_while_retrieving_max_dist');
  END; --bug 9936620

  l_pay_rate_total_amt := p_inv_pay_rec.payment_base_amount;
  l_inv_rate_total_amt := p_inv_pay_rec.invoice_base_amount;

  IF p_inv_pay_rec.discount_taken <> 0 THEN

     l_disc_pay_rate_total_amt := AP_Accounting_Pay_Pkg.Get_Base_Amount
                                          (p_inv_pay_rec.discount_taken,
                                           p_pay_hist_rec.pmt_currency_code,
                                           ap_accounting_pay_pkg.g_base_currency_code,
                                           p_pay_hist_rec.pmt_to_base_xrate_type,
                                           p_pay_hist_rec.pmt_to_base_xrate_date,
                                           p_pay_hist_rec.pmt_to_base_xrate,
                                           l_curr_calling_sequence);

  END IF;

  -- Get the sum of the base amounts for each line type from the payment hist dists.
  IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
         l_log_msg := 'query to fetch the sum of base amounts' ||
                 'for related event id ' || p_pay_hist_rec.Related_Event_ID ||
                 'for payment id ' || p_inv_pay_rec.Invoice_Payment_ID ||
           'and invoice id ' ||  p_inv_rec.invoice_id;
         FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name, l_log_msg);
     END IF;

  SELECT SUM(DECODE(APHD.Pay_Dist_Lookup_Code, 'CASH', APHD.Invoice_Dist_Base_Amount, 0)),
         SUM(DECODE(APHD.Pay_Dist_Lookup_Code, 'CASH', APHD.Paid_Base_Amount, 0)),
         SUM(DECODE(APHD.Pay_Dist_Lookup_Code, 'CASH', APHD.Cleared_Base_Amount, 0)),
         SUM(DECODE(APHD.Pay_Dist_Lookup_Code, 'CASH', APHD.Matured_Base_Amount, 0)),
         SUM(DECODE(APHD.Pay_Dist_Lookup_Code, 'DISCOUNT', APHD.Paid_Base_Amount, 0)),
         SUM(DECODE(APHD.Pay_Dist_Lookup_Code, 'DISCOUNT', APHD.Cleared_Base_Amount, 0))
--         SUM(DECODE(APHD.Pay_Dist_Lookup_Code, 'BANK ERROR', APHD.Cleared_Base_Amount, 0)), --Bug 16636535
--         SUM(DECODE(APHD.Pay_Dist_Lookup_Code, 'BANK CHARGE', APHD.Cleared_Base_Amount, 0))
  INTO   l_inv_rate_sum_amt,
         l_pay_rate_sum_amt,
         l_clr_rate_sum_amt,
         l_mat_rate_sum_amt,
         l_disc_pay_rate_sum_amt,
         l_disc_clr_rate_sum_amt
--         l_err_clr_rate_sum_amt,
--         l_chrg_clr_rate_sum_amt
  FROM   AP_Payment_Hist_Dists APHD,
         AP_Payment_History_All APH
  WHERE  APH.Related_Event_ID = p_pay_hist_rec.Related_Event_ID
  AND    APHD.Invoice_Payment_ID = p_inv_pay_rec.Invoice_Payment_ID
  AND    APHD.Payment_History_ID = APH.Payment_History_ID
  AND    APHD.Invoice_Distribution_ID IN
                     (SELECT AID.Invoice_Distribution_ID
                      FROM   AP_Invoice_Distributions_All AID
                      WHERE  AID.Invoice_ID = p_inv_rec.invoice_id);

 IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
      l_log_msg := ' sum of the base amounts for each line type obtained';
      FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name||'.begin', l_log_msg);
  END IF;


  IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
      l_log_msg := 'CASH lines Invoice_Dist_Base_Amount sum= '||
                    l_inv_rate_sum_amt ||
                    'CASH lines Paid_Base_Amount sum= ' ||
                    l_pay_rate_sum_amt ||
        'CASH lines Cleared_Base_Amount sum= '||
        l_clr_rate_sum_amt ||
        'CASH lines Matured_Base_Amount sum= '||
        l_mat_rate_sum_amt;
  FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name, l_log_msg);
  END IF;

 IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
      l_log_msg := 'DISCOUNT lines Paid_Base_Amount sum= ' ||
                    l_disc_pay_rate_sum_amt ||
        'DISCOUNT lines Cleared_Base_Amount sum= '||
        l_disc_clr_rate_sum_amt;
  FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name, l_log_msg);
  END IF;
  
/* --Bug 16636535
  IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
      l_log_msg := 'BANK ERROR Cleared_Base_Amount sum= ' ||
                    l_err_clr_rate_sum_amt ;
  FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name, l_log_msg);
  END IF;

  IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
      l_log_msg := 'BANK CHARGE Cleared_Base_Amount sum= ' ||
                    l_chrg_clr_rate_sum_amt ;
  FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name, l_log_msg);
  END IF;
*/

 /*Bug6600117 used below query to fetch the sum(amount) and sum(base_amount)
  from ap_payment_history  and  AP_Payment_Hist_dists tables to be
  used later in the calculation*/
-- Bug 6649025
  SELECT sum(nvl(APHD.amount,0)),
         max(APHD.Invoice_Distribution_Id)
         ,SUM(DECODE(APHD.Pay_Dist_Lookup_Code, 'CASH', APHD.Invoice_Dist_Base_Amount,
                     'AWT', APHD.Invoice_Dist_Base_Amount,
                     'DISCOUNT', APHD.Invoice_Dist_Base_Amount, 0))
  INTO l_pay_sum_amt,l_max_dist_id,l_inv_rate_sum_full_amt
  FROM  AP_PAYMENT_HIST_DISTS  APHD,
        AP_PAYMENT_HISTORY_ALL APH
  WHERE
        APHD.PAYMENT_HISTORY_ID =APH.PAYMENT_HISTORY_ID
  ANd   APHD.INVOICE_PAYMENT_ID =p_inv_pay_rec.Invoice_Payment_ID --6614295
  AND    APHD.Invoice_Distribution_ID IN
                     (SELECT AID.Invoice_Distribution_ID
                      FROM   AP_Invoice_Distributions_All AID
                      WHERE  AID.Invoice_ID = p_inv_rec.invoice_id);

     IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
         l_log_msg := 'query to fetch the sum(amount) and sum(base_amount)' ||
                 'from ap_payment_history  and  AP_Payment_Hist_dists tables executed' ||
           'for payment id ' || p_inv_pay_rec.Invoice_Payment_ID ||
           'and invoice id ' ||  p_inv_rec.invoice_id;
         FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name, l_log_msg);
     END IF;

     IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
         l_log_msg := 'l_pay_sum_amt ' || l_pay_sum_amt;
         FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name, l_log_msg);
     END IF;

     IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
         l_log_msg := 'l_max_dist_id ' || l_max_dist_id;
         FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name, l_log_msg);
     END IF;


     IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
         l_log_msg := 'l_inv_rate_sum_full_amt ' || l_inv_rate_sum_full_amt;
         FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name, l_log_msg);
     END IF;

  SELECT  sum(AI.invoice_amount) ,sum(AI.base_amount)
  INTO    l_inv_amt,l_inv_base_amt
  FROM    ap_invoices_all AI
  WHERE   AI.invoice_id = p_inv_rec.invoice_id;

  IF (p_xla_event_rec.event_type_code IN ('PAYMENT CLEARED',
                                          'PAYMENT CLEARING ADJUSTED')) THEN

      l_clr_rate_total_amt := AP_Accounting_Pay_Pkg.Get_Base_Amount
                                          (p_inv_pay_rec.amount,
                                           p_pay_hist_rec.pmt_currency_code,
                                           ap_accounting_pay_pkg.g_base_currency_code,
                                           p_pay_hist_rec.pmt_to_base_xrate_type,
                                           p_pay_hist_rec.pmt_to_base_xrate_date,
                                           p_pay_hist_rec.pmt_to_base_xrate,
                                           l_curr_calling_sequence);


      IF p_inv_pay_rec.discount_taken <> 0 THEN

         l_disc_clr_rate_total_amt := AP_Accounting_Pay_Pkg.Get_Base_Amount
                                          (p_inv_pay_rec.discount_taken,
                                           p_pay_hist_rec.pmt_currency_code,
                                           ap_accounting_pay_pkg.g_base_currency_code,
                                           p_pay_hist_rec.pmt_to_base_xrate_type,
                                           p_pay_hist_rec.pmt_to_base_xrate_date,
                                           p_pay_hist_rec.pmt_to_base_xrate,
                                           l_curr_calling_sequence);
      END IF;
      
/* --Bug 16636535
      IF p_pay_hist_rec.errors_bank_amount <> 0 THEN

         l_err_clr_rate_total_amt := AP_Accounting_Pay_Pkg.Get_Base_Amount
                                          (p_pay_hist_rec.errors_bank_amount,
                                           p_pay_hist_rec.pmt_currency_code,
                                           ap_accounting_pay_pkg.g_base_currency_code,
                                           p_pay_hist_rec.pmt_to_base_xrate_type,
                                           p_pay_hist_rec.pmt_to_base_xrate_date,
                                           p_pay_hist_rec.pmt_to_base_xrate,
                                           l_curr_calling_sequence);
      END IF;

      IF p_pay_hist_rec.charges_bank_amount <> 0 THEN

         l_chrg_clr_rate_total_amt := AP_Accounting_Pay_Pkg.Get_Base_Amount
                                          (p_pay_hist_rec.charges_bank_amount,
                                           p_pay_hist_rec.pmt_currency_code,
                                           ap_accounting_pay_pkg.g_base_currency_code,
                                           p_pay_hist_rec.pmt_to_base_xrate_type,
                                           p_pay_hist_rec.pmt_to_base_xrate_date,
                                           p_pay_hist_rec.pmt_to_base_xrate,
                                           l_curr_calling_sequence);

      END IF;
*/      
  END IF; --bug 9765359

  --bug 9765359, replaced ELSIF with IF
  --added event_types CLEARED and CLEARING_ADJUSTED as matured_base_amount
  --is updated for both MATURITY and CLEARING transactions
  IF (p_xla_event_rec.event_type_code IN ('PAYMENT MATURED',
                                          'PAYMENT MATURITY ADJUSTED',
                                          'PAYMENT CLEARED',
                                          'PAYMENT CLEARING ADJUSTED')) THEN

        --9849243 Mat Rate total amt should be calculated with Maturity Exchange Rate
        IF ( ap_accounting_pay_pkg.g_mat_to_base_xrate_type IS NOT NULL AND
             ap_accounting_pay_pkg.g_mat_to_base_xrate_date IS NOT NULL AND
             ap_accounting_pay_pkg.g_mat_to_base_xrate IS NOT NULL) THEN

             l_rate_type := ap_accounting_pay_pkg.g_mat_to_base_xrate_type;
             l_rate_date := ap_accounting_pay_pkg.g_mat_to_base_xrate_date;
             l_rate := ap_accounting_pay_pkg.g_mat_to_base_xrate;
        ELSE
             l_rate_type := p_pay_hist_rec.pmt_to_base_xrate_type;
             l_rate_date := p_pay_hist_rec.pmt_to_base_xrate_date;
             l_rate := p_pay_hist_rec.pmt_to_base_xrate;
        END IF; --9849243 Ends

        l_mat_rate_total_amt := AP_Accounting_Pay_Pkg.Get_Base_Amount
                                          (p_inv_pay_rec.amount,
                                           p_pay_hist_rec.pmt_currency_code,
                                           ap_accounting_pay_pkg.g_base_currency_code,
                                           l_rate_type, --9849243
                                           l_rate_date, --9849243
                                           l_rate, --9849243
                                           l_curr_calling_sequence);

  END IF;

  --bug 9765359,
  --1. removed 'ELSE' and 'END IF' surrounding the code for calculating
  --   l_pay_rate_total_amt and l_disc_pay_rate_total_amt as these need to be
  --   calculated for all types of transactions--CREATION, MATURITY and CLEARING
  --2. l_pay_rate_total_amt is assigned p_inv_pay_rec.payment_base_amount
  --   earlier in this procedure, reassignment is redundant

        /*l_pay_rate_total_amt := AP_Accounting_Pay_Pkg.Get_Base_Amount
                                          (p_inv_pay_rec.amount,
                                           p_pay_hist_rec.pmt_currency_code,
                                           ap_accounting_pay_pkg.g_base_currency_code,
                                           p_pay_hist_rec.pmt_to_base_xrate_type,
                                           p_pay_hist_rec.pmt_to_base_xrate_date,
                                           p_pay_hist_rec.pmt_to_base_xrate,
                                           l_curr_calling_sequence);*/

  IF p_inv_pay_rec.discount_taken <> 0 THEN

     l_disc_pay_rate_total_amt := AP_Accounting_Pay_Pkg.Get_Base_Amount
                                          (p_inv_pay_rec.discount_taken,
                                           p_pay_hist_rec.pmt_currency_code,
                                           ap_accounting_pay_pkg.g_base_currency_code,
                                           p_pay_hist_rec.pmt_to_base_xrate_type,
                                           p_pay_hist_rec.pmt_to_base_xrate_date,
                                           p_pay_hist_rec.pmt_to_base_xrate,
                                           l_curr_calling_sequence);

  END IF;
  --bug 9765359 end

  l_pay_rate_diff_amt := l_pay_rate_total_amt - l_pay_rate_sum_amt;

  /* If the exchange rates between the invoice and payment are same then the base
     amounts and rounding between the invoice and payment should be same */
  /*Bug6600117
      The fractional rounding amount is added to the maximun distribution amount
      in ap_invoice_distributions_all to balance them in respect to the header base amount.
      Same is not done while calculating invoice_dist_Base_Amount in AP_Payment_Hist_Dists
      table.This is giving rise to the unbalance accounting entries in accounting journal*/

  IF (p_pay_hist_rec.pmt_to_base_xrate =
                      p_inv_rec.exchange_rate / p_inv_rec.payment_cross_rate) THEN
      l_inv_rate_diff_amt := l_pay_rate_diff_amt;
  ELSE
      l_inv_rate_diff_amt := l_inv_rate_total_amt - l_inv_rate_sum_amt;

  END IF;

     /* Bug660017calculate the difference between AP_invoice_distribution
        base amount sum   and AP_Payment_Hist_Dists base amount sum */
      l_inv_dist_diff_amt := l_inv_base_amt - l_inv_rate_sum_full_amt;

--Bug6600117

  --If invoice is fully paid and there is fractional unbalance
 /* IF (l_pay_sum_amt =  l_inv_amt)  and NVL(l_inv_dist_diff_amt,0) <> 0 THEN
     IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
         l_log_msg := 'Updating rounding amount for l_inv_dist_diff_amt';
         FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name, l_log_msg);
     END IF;

    Bug660017 If the invoice is paid in full by this payment and there is a
      difference of amount then adjust the maximum distribution with the fractional
       amount*
-- Bug 6649025

--Bug 7270829 - the same update has been done below , hence commenting out this code.
    UPDATE AP_Payment_Hist_Dists APHD
     SET    APHD.invoice_dist_Base_Amount =  APHD.invoice_dist_Base_Amount + NVL(l_inv_dist_diff_amt,0)
     WHERE  APHD.Invoice_Distribution_ID = l_invoice_dist_id -- l_max_dist_id
     AND    APHD.Pay_Dist_Lookup_Code IN ('CASH')
     AND    APHD.Payment_History_ID = l_payment_hist_id
     AND    APHD.Invoice_Payment_ID = p_inv_pay_rec.invoice_payment_id
     AND    APHD.Accounting_Event_ID = p_xla_event_rec.event_id
     AND    APHD.PAYMENT_HIST_DIST_ID= (select max(APHD1.PAYMENT_HIST_DIST_ID)
                                        from AP_Payment_Hist_Dists APHD1
                                        where APHD1.invoice_distribution_id = l_invoice_dist_id); --l_max_dist_id);

--Bug 7270829

     IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
         l_log_msg := 'Updated rounding amount for l_pay_rate_diff_amt';
         FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name, l_log_msg);
     END IF;
  END IF;

 If there is a difference between the total and sum amounts then we will insert
     the difference as the rounding amounts */

  l_clr_rate_diff_amt := l_clr_rate_total_amt - l_clr_rate_sum_amt;
  l_mat_rate_diff_amt := l_mat_rate_total_amt - l_mat_rate_sum_amt;
  l_disc_pay_rate_diff_amt := l_disc_pay_rate_total_amt - l_disc_pay_rate_sum_amt;
  l_disc_clr_rate_diff_amt := l_disc_clr_rate_total_amt - l_disc_clr_rate_sum_amt;
--  l_err_clr_rate_diff_amt := l_err_clr_rate_total_amt - l_err_clr_rate_sum_amt; --Bug 16636535
--  l_chrg_clr_rate_diff_amt := l_chrg_clr_rate_total_amt - l_chrg_clr_rate_sum_amt; --Bug 16636535

  -- Bug fix 6314128 Starts
  -- Handling the difference amount in l_inv_rate_diff_amt
  -- Rule: SUM(AP_PAYMENT_HIST_DISTS.INVOICE_DIST_BASE_AMOUNT) =
  --       AP_INVOICE_PAYMENTS_ALL.INVOICE_BASE_AMOUNT

/*Commenting out as added new code to avoid negative base amounts due to rounding Bug-7156680
  IF NVL(l_inv_rate_diff_amt,0) <> 0 THEN

     IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
         l_log_msg := 'Updating rounding amount for l_inv_rate_diff_amt';
         FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name, l_log_msg);
     END IF;

     UPDATE AP_Payment_Hist_Dists APHD
     SET    APHD.Invoice_Dist_Base_Amount =
                 APHD.Invoice_Dist_Base_Amount + NVL(l_inv_rate_diff_amt,0),
            APHD.Rounding_Amt = l_inv_rate_diff_amt
     WHERE  APHD.Payment_History_ID = l_payment_hist_id
     AND    APHD.Invoice_Distribution_ID = l_invoice_dist_id
     AND    APHD.Invoice_Payment_ID = p_inv_pay_rec.invoice_payment_id
     AND    APHD.Accounting_Event_ID = p_xla_event_rec.event_id
     AND    APHD.Pay_Dist_Lookup_Code = 'CASH';

     IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
         l_log_msg := 'Updated rounding amount for l_inv_rate_diff_amt';
         FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name, l_log_msg);
     END IF;


   END IF;
  -- BUG 6314128 ENDS;

  -- for bug fix 5694577
  -- Added the event_type_code chack and rearranged the if statements
  IF NVL(l_clr_rate_diff_amt,0) <> 0 AND
    (p_xla_event_rec.event_type_code IN ('PAYMENT CLEARED',
                                          'PAYMENT CLEARING ADJUSTED'))
  THEN

     IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
         l_log_msg := 'Updating rounding amount for l_clr_rate_diff_amt';
         FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name, l_log_msg);
     END IF;


     UPDATE AP_Payment_Hist_Dists APHD
     SET    APHD.Cleared_Base_Amount = APHD.Cleared_Base_Amount
                                               + NVL(l_clr_rate_diff_amt,0),
            APHD.Rounding_Amt = l_clr_rate_diff_amt
     WHERE  APHD.Payment_History_ID = l_payment_hist_id
     AND    APHD.Invoice_Distribution_ID = l_invoice_dist_id
     AND    APHD.Invoice_Payment_ID = p_inv_pay_rec.invoice_payment_id
     AND    APHD.Accounting_Event_ID = p_xla_event_rec.event_id
     AND    APHD.Pay_Dist_Lookup_Code = 'CASH';


     IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
         l_log_msg := 'Updated rounding amount for l_clr_rate_diff_amt';
         FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name, l_log_msg);
     END IF;

  ELSIF NVL(l_mat_rate_diff_amt,0) <> 0 AND
    (p_xla_event_rec.event_type_code IN ('PAYMENT MATURED',
                                          'PAYMENT MATURITY ADJUSTED'))
  THEN

     IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
         l_log_msg := 'Updating rounding amount for l_mat_rate_diff_amt';
         FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name, l_log_msg);
     END IF;

     UPDATE AP_Payment_Hist_Dists APHD
     SET    APHD.Matured_Base_Amount = APHD.Matured_Base_Amount
                                                + NVL(l_mat_rate_diff_amt,0),
            APHD.Rounding_Amt = l_mat_rate_diff_amt
     WHERE  APHD.Payment_History_ID = l_payment_hist_id
     AND    APHD.Invoice_Distribution_ID = l_invoice_dist_id
     AND    APHD.Invoice_Payment_ID = p_inv_pay_rec.invoice_payment_id
     AND    APHD.Accounting_Event_ID = p_xla_event_rec.event_id
     AND    APHD.Pay_Dist_Lookup_Code = 'CASH';


     IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
         l_log_msg := 'Updated rounding amount for l_mat_rate_diff_amt';
         FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name, l_log_msg);
     END IF;

  ELSIF NVL(l_pay_rate_diff_amt,0) <> 0 THEN

     IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
         l_log_msg := 'Updating rounding amount for l_pay_rate_diff_amt';
         FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name, l_log_msg);
     END IF;


     UPDATE AP_Payment_Hist_Dists APHD
     SET    APHD.Paid_Base_Amount = APHD.Paid_Base_Amount + NVL(l_pay_rate_diff_amt,0),
            APHD.Rounding_Amt = l_pay_rate_diff_amt
     WHERE  APHD.Payment_History_ID = l_payment_hist_id
     AND    APHD.Invoice_Distribution_ID = l_invoice_dist_id
     AND    APHD.Invoice_Payment_ID = p_inv_pay_rec.invoice_payment_id
     AND    APHD.Accounting_Event_ID = p_xla_event_rec.event_id
     AND    APHD.Pay_Dist_Lookup_Code = 'CASH';


     IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
         l_log_msg := 'Updated rounding amount for l_pay_rate_diff_amt';
         FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name, l_log_msg);
     END IF;

  END IF;

  IF NVL(l_disc_pay_rate_diff_amt,0) <> 0 THEN

     IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
         l_log_msg := 'Updating discount rounding amount for payment';
         FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name, l_log_msg);
     END IF;


     UPDATE AP_Payment_Hist_Dists APHD
     SET    APHD.Paid_Base_Amount = APHD.Paid_Base_Amount
                                            + NVL(l_disc_pay_rate_diff_amt,0),
            APHD.Rounding_Amt = l_disc_pay_rate_diff_amt
     WHERE  APHD.Payment_History_ID = l_payment_hist_id
     AND    APHD.Invoice_Distribution_ID = l_invoice_dist_id
     AND    APHD.Invoice_Payment_ID = p_inv_pay_rec.invoice_payment_id
     AND    APHD.Accounting_Event_ID = p_xla_event_rec.event_id
     AND    APHD.Pay_Dist_Lookup_Code = 'DISCOUNT';


     IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
         l_log_msg := 'Updated discount rounding amount for payment';
         FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name, l_log_msg);
     END IF;


  END IF;

  IF NVL(l_disc_clr_rate_diff_amt,0) <> 0 THEN

     IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
         l_log_msg := 'Updating discount rounding amount for clearing';
         FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name, l_log_msg);
     END IF;


     UPDATE AP_Payment_Hist_Dists APHD
     SET    APHD.Cleared_Base_Amount = APHD.Cleared_Base_Amount
                                               + NVL(l_disc_clr_rate_diff_amt,0),
            APHD.Rounding_Amt = l_disc_clr_rate_diff_amt
     WHERE  APHD.Payment_History_ID = l_payment_hist_id
     AND    APHD.Invoice_Distribution_ID = l_invoice_dist_id
     AND    APHD.Invoice_Payment_ID = p_inv_pay_rec.invoice_payment_id
     AND    APHD.Accounting_Event_ID = p_xla_event_rec.event_id
     AND    APHD.Pay_Dist_Lookup_Code = 'DISCOUNT';


     IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
         l_log_msg := 'Updated discount rounding amount for clearing';
         FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name, l_log_msg);
     END IF;


  END IF;


  IF NVL(l_err_clr_rate_diff_amt,0) <> 0 THEN

     IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
         l_log_msg := 'Updating error rounding amount';
         FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name, l_log_msg);
     END IF;


     UPDATE AP_Payment_Hist_Dists APHD
     SET    APHD.Cleared_Base_Amount = APHD.Cleared_Base_Amount
                                               + NVL(l_err_clr_rate_diff_amt,0),
            APHD.Rounding_Amt = l_err_clr_rate_diff_amt
     WHERE  APHD.Payment_History_ID = l_payment_hist_id
     AND    APHD.Invoice_Distribution_ID = l_invoice_dist_id
     AND    APHD.Invoice_Payment_ID = p_inv_pay_rec.invoice_payment_id
     AND    APHD.Accounting_Event_ID = p_xla_event_rec.event_id
     AND    APHD.Pay_Dist_Lookup_Code = 'BANK ERROR';


     IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
         l_log_msg := 'Updated error rounding amount';
         FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name, l_log_msg);
     END IF;


  END IF;


  IF NVL(l_chrg_clr_rate_diff_amt,0) <> 0 THEN

     IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
         l_log_msg := 'Updating charge rounding amount';
         FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name, l_log_msg);
     END IF;


     UPDATE AP_Payment_Hist_Dists APHD
     SET    APHD.Cleared_Base_Amount = APHD.Cleared_Base_Amount
                                               + NVL(l_chrg_clr_rate_diff_amt,0),
            APHD.Rounding_Amt = l_chrg_clr_rate_diff_amt
     WHERE  APHD.Payment_History_ID = l_payment_hist_id
     AND    APHD.Invoice_Distribution_ID = l_invoice_dist_id
     AND    APHD.Invoice_Payment_ID = p_inv_pay_rec.invoice_payment_id
     AND    APHD.Accounting_Event_ID = p_xla_event_rec.event_id
     AND    APHD.Pay_Dist_Lookup_Code = 'BANK CHARGE';


     IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
         l_log_msg := 'Updated charge rounding amount';
         FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name, l_log_msg);
     END IF;


  END IF;
*/

/*Bug 7156680
Base amount becomes negative when the the rounding amount is larger than the
distribution having maximum amount to avoid this split the rounding amount among the
distributions such that the base amount is never allowed to become negative
The rounding amount is applied to a paritcular distribution if the base amount goes negative
it is set to zero and the rest of the rounding amount is applied to the next distribution
until the complete rounding amount is consumed*/


  l_clr_rate_rounding_amt := NVL(l_clr_rate_diff_amt, 0);
  l_mat_rate_rounding_amt := NVL(l_mat_rate_diff_amt, 0);
  l_pay_rate_rounding_amt := NVL(l_pay_rate_diff_amt, 0);
  l_disc_pay_rounding_amt := NVL(l_disc_pay_rate_diff_amt, 0);
  l_disc_clr_rounding_amt := NVL(l_disc_clr_rate_diff_amt, 0);
--  l_err_clr_rounding_amt  := NVL(l_err_clr_rate_diff_amt, 0); Bug 16636525
--  l_chrg_clr_rounding_amt := NVL(l_chrg_clr_rate_diff_amt, 0); Bug 16636525
  l_inv_dist_rounding_amt := NVL(l_inv_rate_diff_amt, 0);
  l_pay_hist_id           := l_payment_hist_id;
  l_inv_dist_id           := l_invoice_dist_id;

  WHILE (l_clr_rate_rounding_amt <> 0 or l_mat_rate_rounding_amt <> 0 or
        l_disc_pay_rounding_amt <> 0 or l_disc_clr_rounding_amt <> 0 or
        --l_err_clr_rounding_amt <> 0 or l_chrg_clr_rounding_amt <> 0 or Bug 16636525
        l_inv_dist_rounding_amt <> 0
        or l_pay_rate_rounding_amt <> 0) LOOP -- bug 8725482

    SELECT APHD.Invoice_Dist_Amount,
           APHD.Invoice_Dist_Base_Amount,
           APHD.matured_base_amount,
           APHD.paid_base_Amount,
           APHD.cleared_base_amount
      INTO l_inv_dist_amt,
           l_inv_dist_base_amt,
           l_inv_mat_base_amt,
           l_inv_paid_base_amt,
           l_inv_clr_base_amt
      FROM AP_Payment_Hist_Dists APHD
     WHERE APHD.Payment_History_ID = l_pay_hist_id
       AND APHD.Invoice_Distribution_ID = l_inv_dist_id
       AND APHD.Invoice_Payment_ID = p_inv_pay_rec.invoice_payment_id
       AND APHD.Accounting_Event_ID = p_xla_event_rec.event_id
       AND APHD.Pay_Dist_Lookup_Code = 'CASH';

    --8449083
    l_pre_inv_dist_rounding_amt := l_inv_dist_rounding_amt;
    l_pre_clr_rate_rounding_amt := l_clr_rate_rounding_amt;
    l_pre_mat_rate_rounding_amt := l_mat_rate_rounding_amt;
    l_pre_pay_rate_rounding_amt := l_pay_rate_rounding_amt;
    l_pre_disc_pay_rounding_amt := l_disc_pay_rounding_amt;
    l_pre_disc_clr_rounding_amt := l_disc_clr_rounding_amt;
--    l_pre_err_clr_rounding_amt := l_err_clr_rounding_amt;--Bug 16636535
--    l_pre_chrg_clr_rounding_amt := l_chrg_clr_rounding_amt;


   IF NVL(l_inv_dist_rounding_amt, 0) <> 0 THEN --8449083

      IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
        l_log_msg := 'Updating rounding amount for l_inv_rate_diff_amt';
        FND_LOG.STRING(G_LEVEL_STATEMENT,
                       G_MODULE_NAME || l_procedure_name,
                       l_log_msg);
      END IF;

      IF (sign(l_inv_dist_base_amt + l_inv_dist_rounding_amt) <>
         sign(l_inv_dist_amt) and
         (l_inv_dist_base_amt + l_inv_dist_rounding_amt) <> 0) then
        -- bug 8403738
        l_pre_inv_dist_rounding_amt := l_inv_dist_rounding_amt;
        l_inv_dist_rounding_amt := l_inv_dist_base_amt +
                                   l_inv_dist_rounding_amt;

        -- bug 8403738
        if sign(l_pre_inv_dist_rounding_amt) = sign(l_inv_dist_rounding_amt) AND
           abs(l_inv_dist_rounding_amt) >= abs(l_pre_inv_dist_rounding_amt)
        then
          IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN

           l_log_msg := 'Error : Entered into situation which will lead to infinite loop.'||
                        'There might be some data corrption. Check the following transaction';
           FND_LOG.STRING(G_LEVEL_STATEMENT, G_MODULE_NAME || l_procedure_name,
                          l_log_msg);

           l_log_msg := 'event_id = '||p_xla_event_rec.event_id||
                        ', invoice_payment_id = '||p_inv_pay_rec.invoice_payment_id||
                        ', invoice_distribution_id = '||l_inv_dist_id||
                        ', payment_history_id = '||l_pay_hist_id;
           FND_LOG.STRING(G_LEVEL_STATEMENT, G_MODULE_NAME || l_procedure_name,
                          l_log_msg);

           l_log_msg :=  'l_inv_dist_rounding_amt = '||l_inv_dist_rounding_amt||
                         'l_pre_inv_dist_rounding_amt = '||l_pre_inv_dist_rounding_amt;
           FND_LOG.STRING(G_LEVEL_STATEMENT, G_MODULE_NAME || l_procedure_name,
                          l_log_msg);

           l_log_msg := 'rounding failed and exiting loop!!!';
           FND_LOG.STRING(G_LEVEL_STATEMENT, G_MODULE_NAME || l_procedure_name,
                          l_log_msg);
          END IF;

          exit;
        end if;

        IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
          l_log_msg := ' l_inv_dist_rounding_amt' ||
                       to_char(l_inv_dist_rounding_amt);
          FND_LOG.STRING(G_LEVEL_STATEMENT,
                         G_MODULE_NAME || l_procedure_name,
                         l_log_msg);
        END IF;
        UPDATE AP_Payment_Hist_Dists APHD
           SET APHD.Invoice_Dist_Base_Amount = 0,
       APHD.Rounding_Amt = -sign(l_inv_dist_amt)*l_inv_dist_base_amt
         WHERE APHD.Payment_History_ID = l_pay_hist_id
           AND APHD.Invoice_Distribution_ID = l_inv_dist_id
           AND APHD.Invoice_Payment_ID = p_inv_pay_rec.invoice_payment_id
           AND APHD.Accounting_Event_ID = p_xla_event_rec.event_id
           AND APHD.Pay_Dist_Lookup_Code = 'CASH';

      ELSE
        UPDATE AP_Payment_Hist_Dists APHD
           SET APHD.Invoice_Dist_Base_Amount = APHD.Invoice_Dist_Base_Amount +
                                               NVL(l_inv_dist_rounding_amt,
                                                   0),
         APHD.Rounding_Amt=l_inv_dist_rounding_amt
         WHERE APHD.Payment_History_ID = l_pay_hist_id
           AND APHD.Invoice_Distribution_ID = l_inv_dist_id
           AND APHD.Invoice_Payment_ID = p_inv_pay_rec.invoice_payment_id
           AND APHD.Accounting_Event_ID = p_xla_event_rec.event_id
           AND APHD.Pay_Dist_Lookup_Code = 'CASH';
        l_inv_dist_rounding_amt := 0;
        IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
          l_log_msg := ' Rounding complete for l_inv_rate_diff_amt';
          FND_LOG.STRING(G_LEVEL_STATEMENT,
                         G_MODULE_NAME || l_procedure_name,
                         l_log_msg);
        END IF;
      END IF;
      /*End Changes BUG 7156680 Changes done to avoid negative rounding amount*/
      IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL) THEN
        l_log_msg := 'Updated rounding amount for l_inv_rate_diff_amt';
        FND_LOG.STRING(G_LEVEL_PROCEDURE,
                       G_MODULE_NAME || l_procedure_name,
                       l_log_msg);
      END IF;
    END IF;

    /*End Changes BUG 7156680 Changes done to avoid negative rounding amount*/

    -- BUG 6314128 ENDS;

    -- for bug fix 5694577
    -- Added the event_type_code chack and rearranged the if statements
    IF NVL(l_clr_rate_rounding_amt, 0) <> 0 THEN --8449083
       /*AND (p_xla_event_rec.event_type_code IN
        ('PAYMENT CLEARED', 'PAYMENT CLEARING ADJUSTED')) THEN*/  --bug 8735895

      IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
        l_log_msg := 'Updating rounding amount for l_clr_rate_diff_amt';
        FND_LOG.STRING(G_LEVEL_STATEMENT,
                       G_MODULE_NAME || l_procedure_name,
                       l_log_msg);
      END IF;

      IF (sign(l_inv_clr_base_amt + l_clr_rate_rounding_amt) <>
         sign(l_inv_dist_amt) and
         (l_inv_clr_base_amt + l_clr_rate_rounding_amt) <> 0) then
        -- bug 8403738
        l_pre_clr_rate_rounding_amt := l_clr_rate_rounding_amt;
        l_clr_rate_rounding_amt := l_inv_clr_base_amt +
                                   l_clr_rate_rounding_amt;

        -- bug 8403738
        if sign(l_pre_clr_rate_rounding_amt) = sign(l_clr_rate_rounding_amt)
           AND abs(l_clr_rate_rounding_amt) >= abs(l_pre_clr_rate_rounding_amt)
        then
          IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN

           l_log_msg := 'Error : Entered into situation which will lead to infinite loop.'||
                        'There might be some data corrption. Check the following transaction';
           FND_LOG.STRING(G_LEVEL_STATEMENT, G_MODULE_NAME || l_procedure_name,
                          l_log_msg);

           l_log_msg := 'event_id = '||p_xla_event_rec.event_id||
                        ', invoice_payment_id = '||p_inv_pay_rec.invoice_payment_id||
                        ', invoice_distribution_id = '||l_inv_dist_id||
                        ', payment_history_id = '||l_pay_hist_id;
           FND_LOG.STRING(G_LEVEL_STATEMENT, G_MODULE_NAME || l_procedure_name,
                          l_log_msg);

           l_log_msg :=  'l_clr_rate_rounding_amt = '||l_clr_rate_rounding_amt||
                         'l_pre_clr_rate_rounding_amt = '||l_pre_clr_rate_rounding_amt;
           FND_LOG.STRING(G_LEVEL_STATEMENT, G_MODULE_NAME || l_procedure_name,
                          l_log_msg);

           l_log_msg := 'rounding failed and exiting loop!!!';
           FND_LOG.STRING(G_LEVEL_STATEMENT, G_MODULE_NAME || l_procedure_name,
                          l_log_msg);
          END IF;

           exit;
        end if;

        UPDATE AP_Payment_Hist_Dists APHD
           SET APHD.Cleared_Base_Amount = 0,
            APHD.Rounding_Amt=-sign(l_inv_clr_base_amt)*l_inv_clr_base_amt
         WHERE APHD.Payment_History_ID = l_pay_hist_id
           AND APHD.Invoice_Distribution_ID = l_inv_dist_id
           AND APHD.Invoice_Payment_ID = p_inv_pay_rec.invoice_payment_id
           AND APHD.Accounting_Event_ID = p_xla_event_rec.event_id
           AND APHD.Pay_Dist_Lookup_Code = 'CASH';
      ELSE
        UPDATE AP_Payment_Hist_Dists APHD
           SET APHD.Cleared_Base_Amount = APHD.Cleared_Base_Amount +
                                          NVL(l_clr_rate_rounding_amt, 0),
           APHD.Rounding_Amt=l_clr_rate_rounding_amt
         WHERE APHD.Payment_History_ID = l_pay_hist_id
           AND APHD.Invoice_Distribution_ID = l_inv_dist_id
           AND APHD.Invoice_Payment_ID = p_inv_pay_rec.invoice_payment_id
           AND APHD.Accounting_Event_ID = p_xla_event_rec.event_id
           AND APHD.Pay_Dist_Lookup_Code = 'CASH';
        l_clr_rate_rounding_amt := 0;
      END IF;
      IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL) THEN
        l_log_msg := 'Updated rounding amount for l_clr_rate_diff_amt';
        FND_LOG.STRING(G_LEVEL_PROCEDURE,
                       G_MODULE_NAME || l_procedure_name,
                       l_log_msg);
      END IF;
    END IF;            --bug 8735895

    IF NVL(l_mat_rate_rounding_amt, 0) <> 0 THEN --8449083
         /*AND (p_xla_event_rec.event_type_code IN
           ('PAYMENT MATURED', 'PAYMENT MATURITY ADJUSTED')) THEN*/  --bug 8735895

      IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
        l_log_msg := 'Updating rounding amount for l_mat_rate_diff_amt';
        FND_LOG.STRING(G_LEVEL_STATEMENT,
                       G_MODULE_NAME || l_procedure_name,
                       l_log_msg);
      END IF;
      IF (sign(l_mat_rate_rounding_amt + l_inv_mat_base_amt) <>
         sign(l_inv_dist_amt) and
         (l_mat_rate_rounding_amt + l_inv_mat_base_amt) <> 0) then
        -- bug 8403738
        l_pre_mat_rate_rounding_amt := l_mat_rate_rounding_amt;
        l_mat_rate_rounding_amt := l_inv_mat_base_amt +
                                   l_mat_rate_rounding_amt;

        -- bug 8403738
        if sign(l_pre_mat_rate_rounding_amt) = sign(l_mat_rate_rounding_amt)
           AND abs(l_mat_rate_rounding_amt) >= abs(l_pre_mat_rate_rounding_amt)
        then
          IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN

           l_log_msg := 'Error : Entered into situation which will lead to infinite loop.'||
                        'There might be some data corrption. Check the following transaction';
           FND_LOG.STRING(G_LEVEL_STATEMENT, G_MODULE_NAME || l_procedure_name,
                          l_log_msg);

           l_log_msg := 'event_id = '||p_xla_event_rec.event_id||
                        ', invoice_payment_id = '||p_inv_pay_rec.invoice_payment_id||
                        ', invoice_distribution_id = '||l_inv_dist_id||
                        ', payment_history_id = '||l_pay_hist_id;
           FND_LOG.STRING(G_LEVEL_STATEMENT, G_MODULE_NAME || l_procedure_name,
                          l_log_msg);

           l_log_msg :=  'l_mat_rate_rounding_amt = '||l_mat_rate_rounding_amt||
                         'l_pre_mat_rate_rounding_amt = '||l_pre_mat_rate_rounding_amt;
           FND_LOG.STRING(G_LEVEL_STATEMENT, G_MODULE_NAME || l_procedure_name,
                          l_log_msg);

           l_log_msg := 'rounding failed and exiting loop!!!';
           FND_LOG.STRING(G_LEVEL_STATEMENT, G_MODULE_NAME || l_procedure_name,
                          l_log_msg);
          END IF;

          exit;
        end if;

        UPDATE AP_Payment_Hist_Dists APHD
           SET APHD.Matured_Base_Amount = 0,
           APHD.Rounding_Amt=-sign(l_inv_mat_base_amt )*l_inv_mat_base_amt
         WHERE APHD.Payment_History_ID = l_pay_hist_id
           AND APHD.Invoice_Distribution_ID = l_inv_dist_id
           AND APHD.Invoice_Payment_ID = p_inv_pay_rec.invoice_payment_id
           AND APHD.Accounting_Event_ID = p_xla_event_rec.event_id
           AND APHD.Pay_Dist_Lookup_Code = 'CASH';
      ELSE
        UPDATE AP_Payment_Hist_Dists APHD
           SET APHD.Matured_Base_Amount = APHD.Matured_Base_Amount +
                                          NVL(l_mat_rate_rounding_amt, 0),
         APHD.Rounding_Amt=l_mat_rate_rounding_amt
         WHERE APHD.Payment_History_ID = l_pay_hist_id
           AND APHD.Invoice_Distribution_ID = l_inv_dist_id
           AND APHD.Invoice_Payment_ID = p_inv_pay_rec.invoice_payment_id
           AND APHD.Accounting_Event_ID = p_xla_event_rec.event_id
           AND APHD.Pay_Dist_Lookup_Code = 'CASH';
        l_mat_rate_rounding_amt := 0;
      END IF;

      IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL) THEN
        l_log_msg := 'Updated rounding amount for l_mat_rate_diff_amt';
        FND_LOG.STRING(G_LEVEL_PROCEDURE,
                       G_MODULE_NAME || l_procedure_name,
                       l_log_msg);
      END IF;
    END IF;            --bug 8735895

    IF NVL(l_pay_rate_rounding_amt, 0) <> 0 THEN --8449083

      IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
        l_log_msg := 'Updating rounding amount for l_pay_rate_diff_amt';
        FND_LOG.STRING(G_LEVEL_STATEMENT,
                       G_MODULE_NAME || l_procedure_name,
                       l_log_msg);
      END IF;
      IF (sign(l_pay_rate_rounding_amt + l_inv_paid_base_amt) <>
         sign(l_inv_dist_amt) and
         (l_pay_rate_rounding_amt + l_inv_paid_base_amt) <> 0) then
        -- bug 8403738
        l_pre_pay_rate_rounding_amt := l_pay_rate_rounding_amt;
        l_pay_rate_rounding_amt := l_inv_paid_base_amt +
                                   l_pay_rate_rounding_amt;

        -- bug 8403738
        if sign(l_pre_pay_rate_rounding_amt) = sign(l_pay_rate_rounding_amt)
           AND abs(l_pay_rate_rounding_amt) >= abs(l_pre_pay_rate_rounding_amt)
        then
          IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN

           l_log_msg := 'Error : Entered into situation which will lead to infinite loop.'||
                        'There might be some data corrption. Check the following transaction';
           FND_LOG.STRING(G_LEVEL_STATEMENT, G_MODULE_NAME || l_procedure_name,
                          l_log_msg);

           l_log_msg := 'event_id = '||p_xla_event_rec.event_id||
                        ', invoice_payment_id = '||p_inv_pay_rec.invoice_payment_id||
                        ', invoice_distribution_id = '||l_inv_dist_id||
                        ', payment_history_id = '||l_pay_hist_id;
           FND_LOG.STRING(G_LEVEL_STATEMENT, G_MODULE_NAME || l_procedure_name,
                          l_log_msg);

           l_log_msg :=  'l_pay_rate_rounding_amt = '||l_pay_rate_rounding_amt||
                         'l_pre_pay_rate_rounding_amt = '||l_pre_pay_rate_rounding_amt;
           FND_LOG.STRING(G_LEVEL_STATEMENT, G_MODULE_NAME || l_procedure_name,
                          l_log_msg);

           l_log_msg := 'rounding failed and exiting loop!!!';
           FND_LOG.STRING(G_LEVEL_STATEMENT, G_MODULE_NAME || l_procedure_name,
                          l_log_msg);
          END IF;

          exit;
        end if;

        UPDATE AP_Payment_Hist_Dists APHD
           SET APHD.Paid_Base_Amount = 0,
           APHD.rounding_amt=-sign(l_inv_paid_base_amt)*l_inv_paid_base_amt
         WHERE APHD.Payment_History_ID = l_pay_hist_id
           AND APHD.Invoice_Distribution_ID = l_inv_dist_id
           AND APHD.Invoice_Payment_ID = p_inv_pay_rec.invoice_payment_id
           AND APHD.Accounting_Event_ID = p_xla_event_rec.event_id
           AND APHD.Pay_Dist_Lookup_Code = 'CASH';
      ELSE
        UPDATE AP_Payment_Hist_Dists APHD
           SET APHD.Paid_Base_Amount = APHD.Paid_Base_Amount +
                                       NVL(l_pay_rate_rounding_amt, 0),
         APHD.Rounding_amt=l_pay_rate_rounding_amt
         WHERE APHD.Payment_History_ID = l_pay_hist_id
           AND APHD.Invoice_Distribution_ID = l_inv_dist_id
           AND APHD.Invoice_Payment_ID = p_inv_pay_rec.invoice_payment_id
           AND APHD.Accounting_Event_ID = p_xla_event_rec.event_id
           AND APHD.Pay_Dist_Lookup_Code = 'CASH';
        l_pay_rate_rounding_amt := 0;
      END IF;

      IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL) THEN
        l_log_msg := 'Updated rounding amount for l_pay_rate_diff_amt';
        FND_LOG.STRING(G_LEVEL_PROCEDURE,
                       G_MODULE_NAME || l_procedure_name,
                       l_log_msg);
      END IF;

    END IF;

    IF NVL(l_disc_pay_rounding_amt, 0) <> 0 THEN --8449083
    --vasvenka
    BEGIN
    
    SELECT APHD.Invoice_Dist_Amount,
           APHD.Invoice_Dist_Base_Amount,
           APHD.matured_base_amount,
           APHD.paid_base_Amount,
           APHD.cleared_base_amount
      INTO l_inv_dist_amt_disc,
           l_inv_dist_base_amt_disc,
           l_inv_mat_base_amt_disc,
           l_inv_paid_base_amt_disc,
           l_inv_clr_base_amt_disc
      FROM AP_Payment_Hist_Dists APHD
     WHERE APHD.Payment_History_ID = l_pay_hist_id
       AND APHD.Invoice_Distribution_ID = l_inv_dist_id
       AND APHD.Invoice_Payment_ID = p_inv_pay_rec.invoice_payment_id
       AND APHD.Accounting_Event_ID = p_xla_event_rec.event_id
       AND APHD.Pay_Dist_Lookup_Code = 'DISCOUNT';
       
     EXCEPTION WHEN OTHERS THEN
       IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
          l_log_msg := 'exception when select discount row amount values for payment'||SQLERRM;
          l_log_msg := l_log_msg||'Exiting loop';
          FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name,l_log_msg);
       END IF;
     EXIT;
     END;
     
      IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
        l_log_msg := 'Updating discount rounding amount for payment';
        FND_LOG.STRING(G_LEVEL_STATEMENT,
                       G_MODULE_NAME || l_procedure_name,
                       l_log_msg);
      END IF;
      IF (sign(l_disc_pay_rounding_amt + l_inv_paid_base_amt_disc) <>
         sign(l_inv_dist_amt_disc) and
         (l_disc_pay_rounding_amt + l_inv_paid_base_amt_disc) <> 0) then
        -- bug 8403738
        l_pre_disc_pay_rounding_amt := l_disc_pay_rounding_amt;
        l_disc_pay_rounding_amt := l_inv_paid_base_amt_disc +
                                   l_disc_pay_rounding_amt;

        -- bug 8403738
        if sign(l_pre_disc_pay_rounding_amt) = sign(l_disc_pay_rounding_amt)
           AND abs(l_disc_pay_rounding_amt) >= abs(l_pre_disc_pay_rounding_amt)
        then
          IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN

           l_log_msg := 'Error : Entered into situation which will lead to infinite loop.'||
                        'There might be some data corrption. Check the following transaction';
           FND_LOG.STRING(G_LEVEL_STATEMENT, G_MODULE_NAME || l_procedure_name,
                          l_log_msg);

           l_log_msg := 'event_id = '||p_xla_event_rec.event_id||
                        ', invoice_payment_id = '||p_inv_pay_rec.invoice_payment_id||
                        ', invoice_distribution_id = '||l_inv_dist_id||
                        ', payment_history_id = '||l_pay_hist_id;
           FND_LOG.STRING(G_LEVEL_STATEMENT, G_MODULE_NAME || l_procedure_name,
                          l_log_msg);

           l_log_msg :=  'l_disc_pay_rounding_amt = '||l_disc_pay_rounding_amt||
                         'l_pre_disc_pay_rounding_amt = '||l_pre_disc_pay_rounding_amt;
           FND_LOG.STRING(G_LEVEL_STATEMENT, G_MODULE_NAME || l_procedure_name,
                          l_log_msg);

           l_log_msg := 'rounding failed and exiting loop!!!';
           FND_LOG.STRING(G_LEVEL_STATEMENT, G_MODULE_NAME || l_procedure_name,
                          l_log_msg);
          END IF;

          exit;
        end if;

        UPDATE AP_Payment_Hist_Dists APHD
           SET APHD.Paid_Base_Amount = 0,
           APHD.Rounding_amt=-sign(l_inv_paid_base_amt_disc)*l_inv_paid_base_amt_disc
         WHERE APHD.Payment_History_ID = l_pay_hist_id
           AND APHD.Invoice_Distribution_ID = l_inv_dist_id
           AND APHD.Invoice_Payment_ID = p_inv_pay_rec.invoice_payment_id
           AND APHD.Accounting_Event_ID = p_xla_event_rec.event_id
           AND APHD.Pay_Dist_Lookup_Code = 'DISCOUNT';
      ELSE
        UPDATE AP_Payment_Hist_Dists APHD
           SET APHD.Paid_Base_Amount = APHD.Paid_Base_Amount +
                                       NVL(l_disc_pay_rounding_amt, 0),
             APHD.Rounding_amt=l_disc_pay_rounding_amt
         WHERE APHD.Payment_History_ID = l_pay_hist_id
           AND APHD.Invoice_Distribution_ID = l_inv_dist_id
           AND APHD.Invoice_Payment_ID = p_inv_pay_rec.invoice_payment_id
           AND APHD.Accounting_Event_ID = p_xla_event_rec.event_id
           AND APHD.Pay_Dist_Lookup_Code = 'DISCOUNT';
        l_disc_pay_rounding_amt := 0;
      END IF;

      IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL) THEN
        l_log_msg := 'Updated discount rounding amount for payment';
        FND_LOG.STRING(G_LEVEL_PROCEDURE,
                       G_MODULE_NAME || l_procedure_name,
                       l_log_msg);
      END IF;

    END IF;

    IF NVL(l_disc_clr_rounding_amt, 0) <> 0 THEN --8449083
    --vasvenka
    BEGIN
    
    SELECT APHD.Invoice_Dist_Amount,
           APHD.Invoice_Dist_Base_Amount,
           APHD.matured_base_amount,
           APHD.paid_base_Amount,
           APHD.cleared_base_amount
      INTO l_inv_dist_amt_disc,
           l_inv_dist_base_amt_disc,
           l_inv_mat_base_amt_disc,
           l_inv_paid_base_amt_disc,
           l_inv_clr_base_amt_disc
      FROM AP_Payment_Hist_Dists APHD
     WHERE APHD.Payment_History_ID = l_pay_hist_id
       AND APHD.Invoice_Distribution_ID = l_inv_dist_id
       AND APHD.Invoice_Payment_ID = p_inv_pay_rec.invoice_payment_id
       AND APHD.Accounting_Event_ID = p_xla_event_rec.event_id
       AND APHD.Pay_Dist_Lookup_Code = 'DISCOUNT';
       
     EXCEPTION WHEN OTHERS THEN
      IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
          l_log_msg := 'exception when select discount row amount values for payment'||SQLERRM;
          l_log_msg := l_log_msg||'Exiting loop';
          FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name,l_log_msg);
      END IF;
     EXIT;
     END;
     
      IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
        l_log_msg := 'Updating discount rounding amount for clearing';
        FND_LOG.STRING(G_LEVEL_STATEMENT,
                       G_MODULE_NAME || l_procedure_name,
                       l_log_msg);
      END IF;
      IF (sign(l_disc_clr_rounding_amt + l_inv_clr_base_amt_disc) <>
         sign(l_inv_dist_amt_disc) and
         (l_disc_clr_rounding_amt + l_inv_clr_base_amt_disc) <> 0) then
        -- bug 8403738
        l_pre_disc_clr_rounding_amt := l_disc_clr_rounding_amt;
        l_disc_clr_rounding_amt := l_inv_clr_base_amt_disc +
                                   l_disc_clr_rounding_amt;

        -- bug 8403738
        if sign(l_pre_disc_clr_rounding_amt) = sign(l_disc_clr_rounding_amt)
           AND abs(l_disc_clr_rounding_amt) >= abs(l_pre_disc_clr_rounding_amt)
        then
          IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN

           l_log_msg := 'Error : Entered into situation which will lead to infinite loop.'||
                        'There might be some data corrption. Check the following transaction';
           FND_LOG.STRING(G_LEVEL_STATEMENT, G_MODULE_NAME || l_procedure_name,
                          l_log_msg);

           l_log_msg := 'event_id = '||p_xla_event_rec.event_id||
                        ', invoice_payment_id = '||p_inv_pay_rec.invoice_payment_id||
                        ', invoice_distribution_id = '||l_inv_dist_id||
                        ', payment_history_id = '||l_pay_hist_id;
           FND_LOG.STRING(G_LEVEL_STATEMENT, G_MODULE_NAME || l_procedure_name,
                          l_log_msg);

           l_log_msg :=  'l_disc_clr_rounding_amt = '||l_disc_clr_rounding_amt||
                         'l_pre_disc_clr_rounding_amt = '||l_pre_disc_clr_rounding_amt;
           FND_LOG.STRING(G_LEVEL_STATEMENT, G_MODULE_NAME || l_procedure_name,
                          l_log_msg);

           l_log_msg := 'rounding failed and exiting loop!!!';
           FND_LOG.STRING(G_LEVEL_STATEMENT, G_MODULE_NAME || l_procedure_name,
                          l_log_msg);
          END IF;

          exit;
        end if;

        UPDATE AP_Payment_Hist_Dists APHD
           SET APHD.Cleared_Base_Amount = 0,
           APHD.Rounding_amt=-sign(l_inv_clr_base_amt_disc)*l_inv_clr_base_amt_disc
         WHERE APHD.Payment_History_ID = l_pay_hist_id
           AND APHD.Invoice_Distribution_ID = l_inv_dist_id
           AND APHD.Invoice_Payment_ID = p_inv_pay_rec.invoice_payment_id
           AND APHD.Accounting_Event_ID = p_xla_event_rec.event_id
           AND APHD.Pay_Dist_Lookup_Code = 'DISCOUNT';
      ELSE
        UPDATE AP_Payment_Hist_Dists APHD
           SET APHD.Cleared_Base_Amount = APHD.Cleared_Base_Amount +
                                          NVL(l_disc_clr_rounding_amt, 0),
         APHD.Rounding_Amt=l_disc_clr_rounding_amt
         WHERE APHD.Payment_History_ID = l_pay_hist_id
           AND APHD.Invoice_Distribution_ID = l_inv_dist_id
           AND APHD.Invoice_Payment_ID = p_inv_pay_rec.invoice_payment_id
           AND APHD.Accounting_Event_ID = p_xla_event_rec.event_id
           AND APHD.Pay_Dist_Lookup_Code = 'DISCOUNT';
        l_disc_clr_rounding_amt := 0;
      END IF;

      IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL) THEN
        l_log_msg := 'Updated discount rounding amount for clearing';
        FND_LOG.STRING(G_LEVEL_PROCEDURE,
                       G_MODULE_NAME || l_procedure_name,
                       l_log_msg);
      END IF;

    END IF;

--Bug 16636525
/*
    IF NVL(l_err_clr_rounding_amt, 0) <> 0 THEN --8449083

      IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
        l_log_msg := 'Updating error rounding amount';
        FND_LOG.STRING(G_LEVEL_STATEMENT,
                       G_MODULE_NAME || l_procedure_name,
                       l_log_msg);
      END IF;

      IF (sign(l_err_clr_rounding_amt + l_inv_clr_base_amt) <>
         sign(l_inv_dist_amt) and
         (l_err_clr_rounding_amt + l_inv_clr_base_amt) <> 0) then
        -- bug 8403738
        l_pre_err_clr_rounding_amt := l_err_clr_rounding_amt;
        l_err_clr_rounding_amt := l_inv_clr_base_amt +
                                  l_err_clr_rounding_amt;

        -- bug 8403738
        if sign(l_pre_err_clr_rounding_amt) = sign(l_err_clr_rounding_amt)
           AND abs(l_err_clr_rounding_amt) >= abs(l_pre_err_clr_rounding_amt)
        then
          IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN

           l_log_msg := 'Error : Entered into situation which will lead to infinite loop.'||
                        'There might be some data corrption. Check the following transaction';
           FND_LOG.STRING(G_LEVEL_STATEMENT, G_MODULE_NAME || l_procedure_name,
                          l_log_msg);

           l_log_msg := 'event_id = '||p_xla_event_rec.event_id||
                        ', invoice_payment_id = '||p_inv_pay_rec.invoice_payment_id||
                        ', invoice_distribution_id = '||l_inv_dist_id||
                        ', payment_history_id = '||l_pay_hist_id;
           FND_LOG.STRING(G_LEVEL_STATEMENT, G_MODULE_NAME || l_procedure_name,
                          l_log_msg);

           l_log_msg :=  'l_err_clr_rounding_amt = '||l_err_clr_rounding_amt||
                         'l_pre_err_clr_rounding_amt = '||l_pre_err_clr_rounding_amt;
           FND_LOG.STRING(G_LEVEL_STATEMENT, G_MODULE_NAME || l_procedure_name,
                          l_log_msg);

           l_log_msg := 'rounding failed and exiting loop!!!';
           FND_LOG.STRING(G_LEVEL_STATEMENT, G_MODULE_NAME || l_procedure_name,
                          l_log_msg);
          END IF;

          exit;
        end if;

        UPDATE AP_Payment_Hist_Dists APHD
           SET APHD.Cleared_Base_Amount = 0,
           APHD.Rounding_amt=-sign(l_inv_clr_base_amt)*l_inv_clr_base_amt
         WHERE APHD.Payment_History_ID = l_pay_hist_id
           AND APHD.Invoice_Distribution_ID = l_inv_dist_id
           AND APHD.Invoice_Payment_ID = p_inv_pay_rec.invoice_payment_id
           AND APHD.Accounting_Event_ID = p_xla_event_rec.event_id
           AND APHD.Pay_Dist_Lookup_Code = 'BANK ERROR';
      ELSE
        UPDATE AP_Payment_Hist_Dists APHD
           SET APHD.Cleared_Base_Amount = APHD.Cleared_Base_Amount +
                                          NVL(l_err_clr_rounding_amt, 0),
         APHD.Rounding_amt=l_err_clr_rounding_amt
         WHERE APHD.Payment_History_ID = l_pay_hist_id
           AND APHD.Invoice_Distribution_ID = l_inv_dist_id
           AND APHD.Invoice_Payment_ID = p_inv_pay_rec.invoice_payment_id
           AND APHD.Accounting_Event_ID = p_xla_event_rec.event_id
           AND APHD.Pay_Dist_Lookup_Code = 'BANK ERROR';
        l_err_clr_rounding_amt := 0;
      END IF;

      IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL) THEN
        l_log_msg := 'Updated error rounding amount';
        FND_LOG.STRING(G_LEVEL_PROCEDURE,
                       G_MODULE_NAME || l_procedure_name,
                       l_log_msg);
      END IF;

    END IF;

    IF NVL(l_chrg_clr_rounding_amt, 0) <> 0 THEN --8449083

      IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
        l_log_msg := 'Updating charge rounding amount';
        FND_LOG.STRING(G_LEVEL_STATEMENT,
                       G_MODULE_NAME || l_procedure_name,
                       l_log_msg);
      END IF;

      IF (sign(l_chrg_clr_rounding_amt + l_inv_clr_base_amt) <>
         sign(l_inv_dist_amt) and
         (l_chrg_clr_rounding_amt + l_inv_clr_base_amt) <> 0) then
        -- bug 8403738
        l_pre_chrg_clr_rounding_amt := l_chrg_clr_rounding_amt;
        l_chrg_clr_rounding_amt := l_inv_clr_base_amt +
                                   l_chrg_clr_rounding_amt;

        -- bug 8403738
        if sign(l_pre_chrg_clr_rounding_amt) = sign(l_chrg_clr_rounding_amt)
           AND abs(l_chrg_clr_rounding_amt) >= abs(l_pre_chrg_clr_rounding_amt)
        then
          IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN

           l_log_msg := 'Error : Entered into situation which will lead to infinite loop.'||
                        'There might be some data corrption. Check the following transaction';
           FND_LOG.STRING(G_LEVEL_STATEMENT, G_MODULE_NAME || l_procedure_name,
                          l_log_msg);

           l_log_msg := 'event_id = '||p_xla_event_rec.event_id||
                        ', invoice_payment_id = '||p_inv_pay_rec.invoice_payment_id||
                        ', invoice_distribution_id = '||l_inv_dist_id||
                        ', payment_history_id = '||l_pay_hist_id;
           FND_LOG.STRING(G_LEVEL_STATEMENT, G_MODULE_NAME || l_procedure_name,
                          l_log_msg);

           l_log_msg :=  'l_chrg_clr_rounding_amt = '||l_chrg_clr_rounding_amt||
                         'l_pre_chrg_clr_rounding_amt = '||l_pre_chrg_clr_rounding_amt;
           FND_LOG.STRING(G_LEVEL_STATEMENT, G_MODULE_NAME || l_procedure_name,
                          l_log_msg);

           l_log_msg := 'rounding failed and exiting loop!!!';
           FND_LOG.STRING(G_LEVEL_STATEMENT, G_MODULE_NAME || l_procedure_name,
                          l_log_msg);
          END IF;

          exit;
        end if;

        UPDATE AP_Payment_Hist_Dists APHD
           SET APHD.Cleared_Base_Amount = 0,
           APHD.Rounding_amt=-sign(l_inv_clr_base_amt)*l_inv_clr_base_amt
         WHERE APHD.Payment_History_ID = l_pay_hist_id
           AND APHD.Invoice_Distribution_ID =  l_inv_dist_id
           AND APHD.Invoice_Payment_ID = p_inv_pay_rec.invoice_payment_id
           AND APHD.Accounting_Event_ID = p_xla_event_rec.event_id
           AND APHD.Pay_Dist_Lookup_Code = 'BANK CHARGE';
      ELSE
        UPDATE AP_Payment_Hist_Dists APHD
           SET APHD.Cleared_Base_Amount = APHD.Cleared_Base_Amount +
                                          NVL(l_chrg_clr_rounding_amt, 0),
               APHD.Rounding_amt=l_chrg_clr_rounding_amt
         WHERE APHD.Payment_History_ID = l_pay_hist_id
           AND APHD.Invoice_Distribution_ID = l_inv_dist_id
           AND APHD.Invoice_Payment_ID = p_inv_pay_rec.invoice_payment_id
           AND APHD.Accounting_Event_ID = p_xla_event_rec.event_id
           AND APHD.Pay_Dist_Lookup_Code = 'BANK CHARGE';
        l_chrg_clr_rounding_amt := 0;
      END IF;

      IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL) THEN
        l_log_msg := 'Updated charge rounding amount';
        FND_LOG.STRING(G_LEVEL_PROCEDURE,
                       G_MODULE_NAME || l_procedure_name,
                       l_log_msg);
      END IF;

    END IF;
*/--Bug 16636525
    --8449083 added
    IF ( l_pre_inv_dist_rounding_amt <= l_inv_dist_rounding_amt OR
         l_pre_clr_rate_rounding_amt <= l_clr_rate_rounding_amt OR
         l_pre_mat_rate_rounding_amt <= l_mat_rate_rounding_amt OR
         l_pre_pay_rate_rounding_amt <= l_pay_rate_rounding_amt OR
         l_pre_disc_pay_rounding_amt <= l_disc_pay_rounding_amt OR
         l_pre_disc_clr_rounding_amt <= l_disc_clr_rounding_amt OR
         --l_pre_err_clr_rounding_amt <= l_err_clr_rounding_amt OR Bug 16636525
         --l_pre_chrg_clr_rounding_amt <= l_chrg_clr_rounding_amt OR
         sign(l_pre_inv_dist_rounding_amt) <> sign(l_inv_dist_rounding_amt) OR
         sign(l_pre_clr_rate_rounding_amt) <> sign(l_clr_rate_rounding_amt) OR
         sign(l_pre_mat_rate_rounding_amt) <> sign(l_mat_rate_rounding_amt) OR
         sign(l_pre_pay_rate_rounding_amt) <> sign(l_pay_rate_rounding_amt) OR
         sign(l_pre_disc_pay_rounding_amt) <> sign(l_disc_pay_rounding_amt) OR
         sign(l_pre_disc_clr_rounding_amt) <> sign(l_disc_clr_rounding_amt)
         --sign(l_pre_err_clr_rounding_amt) <> sign(l_err_clr_rounding_amt) OR Bug 16636525
         --sign(l_pre_chrg_clr_rounding_amt) <> sign(l_chrg_clr_rounding_amt) 
         ) THEN

         EXIT;
    END IF;


   --bug  8267525
   IF (l_clr_rate_rounding_amt <> 0 or l_mat_rate_rounding_amt <> 0 or
        l_disc_pay_rounding_amt <> 0 or l_disc_clr_rounding_amt <> 0 or
        --l_err_clr_rounding_amt <> 0 or l_chrg_clr_rounding_amt <> 0 or Bug 16636525
        l_inv_dist_rounding_amt <> 0) THEN
   BEGIN
   SELECT APHD.Payment_History_ID,
          APHD.Invoice_Distribution_ID
    INTO l_pay_hist_id, l_inv_dist_id
    FROM AP_Payment_Hist_Dists APHD
   WHERE APHD.Invoice_Distribution_ID =
         (SELECT MAX(APHD1.Invoice_Distribution_ID)
            FROM AP_Payment_Hist_Dists APHD1
           WHERE APHD1.Accounting_Event_ID = p_xla_event_rec.event_id
             AND APHD1.Invoice_Payment_ID = p_inv_pay_rec.invoice_payment_id
             AND APHD1.Rounding_Amt is NULL
             AND APHD1.Invoice_Distribution_ID IN
                 (SELECT AID.Invoice_Distribution_ID
                    FROM AP_Invoice_Distributions_All AID
                   WHERE AID.Invoice_ID = p_inv_rec.invoice_id)
             AND ABS(APHD1.Amount) =
                 (SELECT MAX(ABS(APHD2.Amount))
                    FROM AP_Payment_Hist_Dists APHD2
                   WHERE APHD2.Accounting_Event_ID = p_xla_event_rec.event_id
                     AND APHD2.Invoice_Payment_ID =
                         p_inv_pay_rec.invoice_payment_id
                     AND APHD2.Rounding_Amt is NULL
                     AND APHD2.Invoice_Distribution_ID IN
                         (SELECT AID.Invoice_Distribution_ID
                            FROM AP_Invoice_Distributions_All AID
                           WHERE AID.Invoice_ID = p_inv_rec.invoice_id)))
     AND APHD.Rounding_Amt is NULL
     AND APHD.Accounting_Event_ID = p_xla_event_rec.event_id
     AND APHD.Invoice_Payment_ID = p_inv_pay_rec.invoice_payment_id
     AND Rownum = 1;
     EXCEPTION
        WHEN NO_DATA_FOUND THEN
           RAISE_APPLICATION_ERROR(-20100, l_procedure_name||
                                  ' no_record_in_aphd_while_retrieving_max_dist');
     END;
   END IF;  --bug  8267525
  END LOOP;

  -- Logging Infra: Procedure level
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
      l_log_msg := 'End of procedure '|| l_procedure_name;
      FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name||'.end', l_log_msg);
  END IF;


EXCEPTION
  WHEN OTHERS THEN
    IF (SQLCODE = -20100) THEN
       RAISE_APPLICATION_ERROR(-20100, SQLERRM);
    ELSIF (SQLCODE <> -20001) THEN
      FND_MESSAGE.SET_NAME('SQLAP','AP_DEBUG');
      FND_MESSAGE.SET_TOKEN('ERROR',SQLERRM);
      FND_MESSAGE.SET_TOKEN('CALLING_SEQUENCE', l_curr_calling_sequence);
    END IF;
    APP_EXCEPTION.RAISE_EXCEPTION;

END Total_Pay;



-------------------------------------------------------------------------------
-- PROCEDURE  Compare_Pay
-- This procedure calculates the rounding amount needed to relieve base
-- amounts between events.  The following types of rounding will be calculated:
--
-- PAYMENT TO MATURITY ROUNDING
-- PAYMENT TO CLEARING ROUNDING
-- MATURITY TO CLEARING ROUNDING
--
--------------------------------------------------------------------------------
PROCEDURE Compare_Pay
     (P_XLA_Event_Rec    IN   ap_accounting_pay_pkg.r_xla_event_info
     ,P_Pay_Hist_Rec     IN   ap_accounting_pay_pkg.r_pay_hist_info
     ,P_Inv_Rec          IN   ap_accounting_pay_pkg.r_invoices_info
     ,P_Inv_Pay_Rec      IN   ap_acctg_pay_dist_pkg.r_inv_pay_info
     ,P_Calling_Sequence IN   VARCHAR2
     ) IS

  l_curr_calling_sequence    VARCHAR2(2000);
  l_sum_pay_paid_base_amt    NUMBER;
  l_sum_mat_paid_base_amt    NUMBER;
  l_sum_clr_paid_base_amt    NUMBER;
  l_sum_mat_mat_base_amt     NUMBER;
  l_sum_clr_mat_base_amt     NUMBER;

  l_diff_mat_paid_base_amt   NUMBER;
  l_diff_clr_paid_base_amt   NUMBER;
  l_diff_clr_mat_base_amt    NUMBER;


  l_max_pd_rec               AP_PAYMENT_HIST_DISTS%ROWTYPE;
  l_pd_rec                   AP_PAYMENT_HIST_DISTS%ROWTYPE;

  -- Logging Infra:
  l_procedure_name CONSTANT VARCHAR2(30) := 'Compare_Pay';
  l_log_msg        FND_LOG_MESSAGES.MESSAGE_TEXT%TYPE;

BEGIN

  l_curr_calling_sequence := 'AP_ACCTG_PAY_ROUND_PKG.Compare_Pay<- ' ||
                                           P_Calling_Sequence;


  -- Logging Infra: Procedure level
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
      l_log_msg := 'Begin of procedure '|| l_procedure_name;
      FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name||'.begin', l_log_msg);
  END IF;


  -- Getting the max of the largest distribution for inserting the rounding
  -- distribution
  BEGIN
  SELECT APHD.*
  INTO   l_max_pd_rec
  FROM   AP_Payment_Hist_Dists APHD
  WHERE  APHD.Invoice_Distribution_ID =
               (SELECT MAX(APHD1.Invoice_Distribution_ID)
                FROM   AP_Payment_Hist_Dists APHD1
                WHERE  APHD1.Accounting_Event_ID = p_xla_event_rec.event_id
                AND    APHD1.Invoice_payment_id = P_Inv_Pay_Rec.Invoice_payment_id -- Bug 8722710
                AND    APHD1.Invoice_Distribution_ID IN
                               (SELECT AID.Invoice_Distribution_ID
                                FROM   AP_Invoice_Distributions_All AID
                                WHERE  AID.Invoice_ID = p_inv_rec.invoice_id)
                AND    ABS(APHD1.Amount) =
                               (SELECT MAX(ABS(APHD2.Amount))
                                FROM   AP_Payment_Hist_Dists APHD2
                                WHERE  APHD2.Accounting_Event_ID = p_xla_event_rec.event_id
                                AND    APHD2.Invoice_payment_id = P_Inv_Pay_Rec.Invoice_payment_id -- Bug 8722710
                                AND    APHD2.Invoice_Distribution_ID IN
                                       (SELECT AID.Invoice_Distribution_ID
                                        FROM   AP_Invoice_Distributions_All AID
                                        WHERE  AID.Invoice_ID = p_inv_rec.invoice_id)))
  AND   APHD.Accounting_Event_ID = p_xla_event_rec.event_id
  AND   APHD.Invoice_payment_id = P_Inv_Pay_Rec.Invoice_payment_id -- Bug 8722710
  AND   Rownum = 1;
  EXCEPTION
     WHEN NO_DATA_FOUND THEN
        RAISE_APPLICATION_ERROR(-20100, l_procedure_name||
                                ' no_record_in_aphd_while_retrieving_max_dist');
  END; --bug 9936620

  -- Get the paid base amounts for the payment event
  SELECT SUM(APHD.Paid_Base_Amount)
  INTO   l_sum_pay_paid_base_amt
  FROM   AP_Payment_Hist_Dists APHD,
         AP_Payment_History_All APH,
         AP_Payment_History_All APH1
  WHERE  APH1.Payment_History_ID = ap_accounting_pay_pkg.g_pay_pmt_history_id
  AND    APH.Related_Event_ID = APH1.Accounting_Event_ID
  AND    APHD.Payment_History_ID = APH.Payment_History_ID
  AND    APHD.Pay_Dist_Lookup_Code IN ('CASH')
  AND    APHD.Invoice_payment_id = P_Inv_Pay_Rec.Invoice_payment_id -- Bug 8722710
  AND    APHD.Invoice_Distribution_ID IN
                     (SELECT AID.Invoice_Distribution_ID
                      FROM   AP_Invoice_Distributions_All AID
                      WHERE  AID.Invoice_ID = p_inv_rec.invoice_id);


  -- Get the paid base amounts for the payment maturity event
  SELECT SUM(APHD.Paid_Base_Amount),
         SUM(APHD.Matured_Base_Amount)
  INTO   l_sum_mat_paid_base_amt,
         l_sum_mat_mat_base_amt
  FROM   AP_Payment_Hist_Dists APHD,
         AP_Payment_History_All APH,
         AP_Payment_History_All APH1
  WHERE  APH1.Payment_History_ID = ap_accounting_pay_pkg.g_mat_pmt_history_id
  AND    APH.Related_Event_ID = APH1.Accounting_Event_ID
  AND    APHD.Payment_History_ID = APH.Payment_History_ID
  AND    APHD.Pay_Dist_Lookup_Code IN ('CASH')
  AND    APHD.Invoice_payment_id = P_Inv_Pay_Rec.Invoice_payment_id -- Bug 8722710
  AND    APHD.Invoice_Distribution_ID IN
                     (SELECT AID.Invoice_Distribution_ID
                      FROM   AP_Invoice_Distributions_All AID
                      WHERE  AID.Invoice_ID = p_inv_rec.invoice_id);


  -- Get the paid base amounts for the payment clearing event
  -- Bug 6678474. Backing out the fix for bug 6621586 since it is not right fix.
  -- Here we are calculating the rounding difference for the same currency amounts
  -- but in prior events.
  -- For eg. any difference between the paid base amount in payment created event
  -- and payment cleared event will be calculated as Payment to Clearing rounding
  SELECT SUM(APHD.Paid_Base_Amount),
         SUM(APHD.Matured_Base_Amount)
  INTO   l_sum_clr_paid_base_amt,
         l_sum_clr_mat_base_amt
  FROM   AP_Payment_Hist_Dists APHD,
         AP_Payment_History_All APH,
         AP_Payment_History_All APH1
  WHERE  APH1.Payment_History_ID = ap_accounting_pay_pkg.g_clr_pmt_history_id
  AND    APH.Related_Event_ID = APH1.Accounting_Event_ID
  AND    APHD.Payment_History_ID = APH.Payment_History_ID
  AND    APHD.Pay_Dist_Lookup_Code IN ('CASH')
  AND    APHD.Invoice_payment_id = P_Inv_Pay_Rec.Invoice_payment_id -- Bug 8722710
  AND    APHD.Invoice_Distribution_ID IN
                     (SELECT AID.Invoice_Distribution_ID
                      FROM   AP_Invoice_Distributions_All AID
                      WHERE  AID.Invoice_ID = p_inv_rec.invoice_id);

  /* If there is any difference between the paid and maturity base amounts between
     this event and the prior event then we will insert the appropriate rounding
     distribution */
  l_diff_mat_paid_base_amt := l_sum_pay_paid_base_amt -
                                 NVL(l_sum_mat_paid_base_amt, l_sum_pay_paid_base_amt);

  l_diff_clr_paid_base_amt := l_sum_pay_paid_base_amt -
                                 NVL(l_sum_clr_paid_base_amt, l_sum_pay_paid_base_amt);

  l_diff_clr_mat_base_amt := NVL(l_sum_mat_mat_base_amt, l_sum_clr_mat_base_amt) -
                                 NVL(l_sum_clr_mat_base_amt, l_sum_mat_mat_base_amt);

  l_pd_rec.invoice_distribution_id := l_max_pd_rec.invoice_distribution_id;
  l_pd_rec.payment_history_id := l_max_pd_rec.payment_history_id;
  l_pd_rec.invoice_payment_id := l_max_pd_rec.invoice_payment_id;
  l_pd_rec.invoice_adjustment_event_id := l_max_pd_rec.invoice_adjustment_event_id;
  l_pd_rec.accounting_event_id := p_xla_event_rec.event_id;
  l_pd_rec.awt_related_id := l_max_pd_rec.awt_related_id;

  l_pd_rec.amount := 0;
  l_pd_rec.invoice_dist_amount := 0;
  l_pd_rec.bank_curr_amount := 0;
  l_pd_rec.invoice_dist_base_amount := 0;

  IF l_diff_mat_paid_base_amt <> 0 THEN

     IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
         l_log_msg := 'Inserting future payment rounding';
         FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name, l_log_msg);
     END IF;


     l_pd_rec.pay_dist_lookup_code := 'FUTURE PAYMENT ROUNDING';

     l_pd_rec.paid_base_amount := l_diff_mat_paid_base_amt;
     l_pd_rec.cleared_base_amount := 0;
     l_pd_rec.matured_base_amount := 0;

     IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
         l_log_msg := 'Calling procedure AP_Acctg_Pay_Dist_Pkg.Pay_Dist_Insert';
         FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name, l_log_msg);
     END IF;

     AP_Acctg_Pay_Dist_Pkg.Pay_Dist_Insert
                            (l_pd_rec,
                             l_curr_calling_sequence);

     IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
         l_log_msg := 'Procedure AP_Acctg_Pay_Dist_Pkg.Pay_Dist_Insert executed';
         FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name, l_log_msg);
     END IF;


  END IF;

  IF l_diff_clr_paid_base_amt <> 0 THEN

     IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
         l_log_msg := 'Inserting payment to clearing rounding';
         FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name, l_log_msg);
     END IF;

     l_pd_rec.pay_dist_lookup_code := 'PAYMENT TO CLEARING ROUNDING';

     l_pd_rec.paid_base_amount := l_diff_clr_paid_base_amt;
     l_pd_rec.cleared_base_amount := 0;
     l_pd_rec.matured_base_amount := 0;

     IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
         l_log_msg := 'Calling procedure AP_Acctg_Pay_Dist_Pkg.Pay_Dist_Insert';
         FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name, l_log_msg);
     END IF;

     AP_Acctg_Pay_Dist_Pkg.Pay_Dist_Insert
                            (l_pd_rec,
                             l_curr_calling_sequence);

     IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
         l_log_msg := 'Procedure AP_Acctg_Pay_Dist_Pkg.Pay_Dist_Insert executed';
         FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name, l_log_msg);
     END IF;


  END IF;

  IF l_diff_clr_mat_base_amt <> 0 THEN


     IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
         l_log_msg := 'Inserting maturity to clearing rounding';
         FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name, l_log_msg);
     END IF;

     l_pd_rec.pay_dist_lookup_code := 'MATURITY TO CLEARING ROUNDING';

     l_pd_rec.paid_base_amount := 0;
     l_pd_rec.cleared_base_amount := 0;
     l_pd_rec.matured_base_amount := l_diff_clr_mat_base_amt;

     IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
         l_log_msg := 'Calling procedure AP_Acctg_Pay_Dist_Pkg.Pay_Dist_Insert';
         FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name, l_log_msg);
     END IF;

     AP_Acctg_Pay_Dist_Pkg.Pay_Dist_Insert
                            (l_pd_rec,
                             l_curr_calling_sequence);

     IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
         l_log_msg := 'Procedure AP_Acctg_Pay_Dist_Pkg.Pay_Dist_Insert executed';
         FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name, l_log_msg);
     END IF;


  END IF;

  -- Logging Infra: Procedure level
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
      l_log_msg := 'End of procedure '|| l_procedure_name;
      FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name||'.end', l_log_msg);
  END IF;


EXCEPTION
  WHEN OTHERS THEN
    IF (SQLCODE = -20100) THEN
      RAISE_APPLICATION_ERROR(-20100, SQLERRM);
    ELSIF (SQLCODE <> -20001) THEN
      FND_MESSAGE.SET_NAME('SQLAP','AP_DEBUG');
      FND_MESSAGE.SET_TOKEN('ERROR',SQLERRM);
      FND_MESSAGE.SET_TOKEN('CALLING_SEQUENCE', l_curr_calling_sequence);
    END IF;
    APP_EXCEPTION.RAISE_EXCEPTION;

END Compare_Pay;



-------------------------------------------------------------------------------
-- PROCEDURE  Total_Appl
-- This procedure calculates rounding to confirm that each prepayment
-- application invoice distribution and its tax difference have been fully
-- relieved by their corresponding records in APAD
--
--------------------------------------------------------------------------------
PROCEDURE Total_Appl
     (P_XLA_Event_Rec    IN   ap_accounting_pay_pkg.r_xla_event_info
     ,P_Pay_Hist_Rec     IN   ap_accounting_pay_pkg.r_pay_hist_info
     ,P_Clr_Hist_Rec     IN   ap_accounting_pay_pkg.r_pay_hist_info
     ,P_Inv_Rec          IN   ap_accounting_pay_pkg.r_invoices_info
     ,P_Prepay_Inv_Rec   IN   ap_accounting_pay_pkg.r_invoices_info
     ,P_Prepay_Hist_Rec  IN   AP_ACCTG_PREPAY_DIST_PKG.r_prepay_hist_info
     ,P_Prepay_Dist_Rec  IN   AP_ACCTG_PREPAY_DIST_PKG.r_prepay_dist_info
     ,P_Calling_Sequence IN   VARCHAR2
     ) IS

  l_curr_calling_sequence        VARCHAR2(2000);
  l_prepay_pay_amt               NUMBER;
  l_prepay_pay_tax_diff          NUMBER;

  l_tot_inv_rate_amt             NUMBER;
  l_tot_prepay_rate_amt          NUMBER;
  l_tot_prepay_pay_rate_amt      NUMBER;
  l_tot_prepay_clr_rate_amt      NUMBER;
  l_td_tot_inv_rate_amt          NUMBER;
  l_td_tot_prepay_rate_amt       NUMBER;
  l_td_tot_prepay_pay_rate_amt   NUMBER;
  l_td_tot_prepay_clr_rate_amt   NUMBER;

  l_sum_inv_rate_amt             NUMBER;
  l_sum_prepay_rate_amt          NUMBER;
  l_sum_prepay_pay_rate_amt      NUMBER;
  l_sum_prepay_clr_rate_amt      NUMBER;
  l_td_sum_inv_rate_amt          NUMBER;
  l_td_sum_prepay_rate_amt       NUMBER;
  l_td_sum_prepay_pay_rate_amt   NUMBER;
  l_td_sum_prepay_clr_rate_amt   NUMBER;

  l_diff_inv_rate_amt            NUMBER;
  l_diff_prepay_rate_amt         NUMBER;
  l_diff_prepay_pay_rate_amt     NUMBER;
  l_diff_prepay_clr_rate_amt     NUMBER;
  l_td_diff_inv_rate_amt         NUMBER;
  l_td_diff_prepay_rate_amt      NUMBER;
  l_td_diff_prepay_pay_rate_amt  NUMBER;
  l_td_diff_prepay_clr_rate_amt  NUMBER;

  l_max_prepay_rec               AP_PREPAY_APP_DISTS%ROWTYPE;
  l_pad_rec                      AP_PREPAY_APP_DISTS%ROWTYPE;

  -- Logging Infra:
  l_procedure_name CONSTANT VARCHAR2(30) := 'Total_Appl';
  l_log_msg        FND_LOG_MESSAGES.MESSAGE_TEXT%TYPE;

BEGIN

  l_curr_calling_sequence := 'AP_Acctg_Pay_Round_Pkg.Total_Appl<- ' ||
                                           p_calling_sequence;

  -- Logging Infra: Procedure level
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
      l_log_msg := 'Begin of procedure '|| l_procedure_name;
      FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name||'.begin', l_log_msg);
  END IF;

  -- Get the max of the largest distribution for inserting the rounding line
  BEGIN
  SELECT APAD.*
  INTO   l_max_prepay_rec
  FROM   AP_Prepay_App_Dists APAD
  WHERE  Prepay_App_Distribution_ID = p_prepay_dist_rec.invoice_distribution_id
  AND    APAD.Prepay_History_ID = p_prepay_hist_rec.prepay_history_id
  AND    Invoice_Distribution_ID =
                (SELECT MAX(APAD1.Invoice_Distribution_ID)
                 FROM   AP_Prepay_App_Dists APAD1
                 WHERE  APAD1.Prepay_App_Distribution_ID =
                                        p_prepay_dist_rec.invoice_distribution_id
                 AND    APAD1.Prepay_History_ID = p_prepay_hist_rec.prepay_history_id
                 AND    ABS(APAD1.Amount) =
                                  (SELECT MAX(ABS(APAD2.Amount))
                                   FROM   AP_Prepay_App_Dists APAD2
                                   WHERE  APAD2.Prepay_App_Distribution_ID =
                                               p_prepay_dist_rec.invoice_distribution_id
                                   AND    APAD2.Prepay_History_ID = p_prepay_hist_rec.prepay_history_id))
  AND    Rownum = 1;
  EXCEPTION
     WHEN NO_DATA_FOUND THEN
        RAISE_APPLICATION_ERROR(-20100, l_procedure_name||
                                ' no_record_in_APAD_while_retrieving_max_dist');
  END; --bug 9936620

  l_tot_inv_rate_amt := AP_Accounting_Pay_Pkg.Get_Base_Amount
                                     (p_prepay_dist_rec.amount,
                                      p_inv_rec.invoice_currency_code,
                                      ap_accounting_pay_pkg.g_base_currency_code,
                                      p_inv_rec.exchange_rate_type,
                                      p_inv_rec.exchange_date,
                                      p_inv_rec.exchange_rate,
                                      l_curr_calling_sequence);


  l_tot_prepay_rate_amt := AP_Accounting_Pay_Pkg.Get_Base_Amount
                                     (p_prepay_dist_rec.amount,
                                      p_inv_rec.invoice_currency_code,
                                      ap_accounting_pay_pkg.g_base_currency_code,
                                      l_max_prepay_rec.prepay_exchange_rate_type,
                                      l_max_prepay_rec.prepay_exchange_date,
                                      l_max_prepay_rec.prepay_exchange_rate,
                                      l_curr_calling_sequence);


  -- Converting the prepay amount into payment currency
  IF (p_inv_rec.invoice_currency_code <> p_pay_hist_rec.pmt_currency_code) THEN
      l_prepay_pay_amt := p_prepay_dist_rec.amount * p_inv_rec.payment_cross_rate;
      l_prepay_pay_tax_diff := p_prepay_dist_rec.prepay_tax_diff_amount
                                                 * p_inv_rec.payment_cross_rate;
  ELSE
      l_prepay_pay_amt := p_prepay_dist_rec.amount;
      l_prepay_pay_tax_diff := p_prepay_dist_rec.prepay_tax_diff_amount;
  END IF;


  l_tot_prepay_pay_rate_amt := AP_Accounting_Pay_Pkg.Get_Base_Amount
                                     (l_prepay_pay_amt,
                                      p_inv_rec.payment_currency_code,
                                      ap_accounting_pay_pkg.g_base_currency_code,
                                      l_max_prepay_rec.prepay_pay_exchange_rate_type,
                                      l_max_prepay_rec.prepay_pay_exchange_date,
                                      l_max_prepay_rec.prepay_pay_exchange_rate,
                                      l_curr_calling_sequence);

  l_td_tot_inv_rate_amt := AP_Accounting_Pay_Pkg.Get_Base_Amount
                                     (p_prepay_dist_rec.prepay_tax_diff_amount,
                                      p_inv_rec.invoice_currency_code,
                                      ap_accounting_pay_pkg.g_base_currency_code,
                                      p_inv_rec.exchange_rate_type,
                                      p_inv_rec.exchange_date,
                                      p_inv_rec.exchange_rate,
                                      l_curr_calling_sequence);


  l_td_tot_prepay_rate_amt := AP_Accounting_Pay_Pkg.Get_Base_Amount
                                     (p_prepay_dist_rec.prepay_tax_diff_amount,
                                      p_inv_rec.invoice_currency_code,
                                      ap_accounting_pay_pkg.g_base_currency_code,
                                      l_max_prepay_rec.prepay_exchange_rate_type,
                                      l_max_prepay_rec.prepay_exchange_date,
                                      l_max_prepay_rec.prepay_exchange_rate,
                                      l_curr_calling_sequence);


  l_td_tot_prepay_pay_rate_amt := AP_Accounting_Pay_Pkg.Get_Base_Amount
                                     (l_prepay_pay_tax_diff,
                                      p_inv_rec.payment_currency_code,
                                      ap_accounting_pay_pkg.g_base_currency_code,
                                      l_max_prepay_rec.prepay_pay_exchange_rate_type,
                                      l_max_prepay_rec.prepay_pay_exchange_date,
                                      l_max_prepay_rec.prepay_pay_exchange_rate,
                                      l_curr_calling_sequence);


  IF l_max_prepay_rec.prepay_clr_exchange_rate IS NOT NULL THEN

     l_tot_prepay_clr_rate_amt := AP_Accounting_Pay_Pkg.Get_Base_Amount
                                     (l_prepay_pay_amt,
                                      p_inv_rec.payment_currency_code,
                                      ap_accounting_pay_pkg.g_base_currency_code,
                                      l_max_prepay_rec.prepay_clr_exchange_rate_type,
                                      l_max_prepay_rec.prepay_clr_exchange_date,
                                      l_max_prepay_rec.prepay_clr_exchange_rate,
                                      l_curr_calling_sequence);

     l_td_tot_prepay_clr_rate_amt := AP_Accounting_Pay_Pkg.Get_Base_Amount
                                        (l_prepay_pay_tax_diff,
                                         p_inv_rec.payment_currency_code,
                                         ap_accounting_pay_pkg.g_base_currency_code,
                                         l_max_prepay_rec.prepay_clr_exchange_rate_type,
                                         l_max_prepay_rec.prepay_clr_exchange_date,
                                         l_max_prepay_rec.prepay_clr_exchange_rate,
                                         l_curr_calling_sequence);

  END IF;


  SELECT SUM(DECODE(Prepay_Dist_Lookup_Code, 'PREPAY APPL', Base_Amount,
               'PREPAY APPL REC TAX', Base_Amount, 'PREPAY APPL NONREC TAX', Base_Amount, 0)),
         SUM(DECODE(Prepay_Dist_Lookup_Code, 'PREPAY APPL', Base_Amt_At_Prepay_XRate,
                      'PREPAY APPL REC TAX', Base_Amt_At_Prepay_XRate,
                      'PREPAY APPL NONREC TAX', Base_Amt_At_Prepay_XRate, 0)),
         SUM(DECODE(Prepay_Dist_Lookup_Code, 'PREPAY APPL', Base_Amt_At_Prepay_Pay_XRate,
                      'PREPAY APPL REC TAX', Base_Amt_At_Prepay_Pay_XRate,
                      'PREPAY APPL NONREC TAX', Base_Amt_At_Prepay_Pay_XRate, 0)),
         SUM(DECODE(Prepay_Dist_Lookup_Code, 'PREPAY APPL', Base_Amt_At_Prepay_Clr_XRate,
                      'PREPAY APPL REC TAX', Base_Amt_At_Prepay_Clr_XRate,
                      'PREPAY APPL NONREC TAX', Base_Amt_At_Prepay_Clr_XRate, 0)),
         SUM(DECODE(Prepay_Dist_Lookup_Code, 'TAX DIFF', Base_Amount, 0)),
         SUM(DECODE(Prepay_Dist_Lookup_Code, 'TAX DIFF', Base_Amt_At_Prepay_XRate, 0)),
         SUM(DECODE(Prepay_Dist_Lookup_Code, 'TAX DIFF', Base_Amt_At_Prepay_Pay_XRate, 0)),
         SUM(DECODE(Prepay_Dist_Lookup_Code, 'TAX DIFF', Base_Amt_At_Prepay_Clr_XRate, 0))
  INTO   l_sum_inv_rate_amt,
         l_sum_prepay_rate_amt,
         l_sum_prepay_pay_rate_amt,
         l_sum_prepay_clr_rate_amt,
         l_td_sum_inv_rate_amt,
         l_td_sum_prepay_rate_amt,
         l_td_sum_prepay_pay_rate_amt,
         l_td_sum_prepay_clr_rate_amt
  FROM   AP_Prepay_App_Dists APAD
  WHERE  APAD.Prepay_App_Distribution_ID = p_prepay_dist_rec.invoice_distribution_id;


  /* If there is difference between the total and sum amounts then we will insert the
     difference as the rounding amounts */

  l_diff_inv_rate_amt := NVL(l_tot_inv_rate_amt,l_sum_inv_rate_amt) - l_sum_inv_rate_amt;
  l_diff_prepay_rate_amt := NVL(l_tot_prepay_rate_amt,l_sum_prepay_rate_amt)
                                            - l_sum_prepay_rate_amt;
  l_diff_prepay_pay_rate_amt := NVL(l_tot_prepay_pay_rate_amt,l_sum_prepay_pay_rate_amt)
                                            - l_sum_prepay_pay_rate_amt;
  l_diff_prepay_clr_rate_amt := NVL(l_tot_prepay_clr_rate_amt,l_sum_prepay_clr_rate_amt)
                                            - l_sum_prepay_clr_rate_amt;
  l_td_diff_inv_rate_amt := NVL(l_td_tot_inv_rate_amt,l_td_sum_inv_rate_amt) - l_td_sum_inv_rate_amt;
  l_td_diff_prepay_rate_amt := NVL(l_td_tot_prepay_rate_amt,l_td_sum_prepay_rate_amt)
                                            - l_td_sum_prepay_rate_amt;
  l_td_diff_prepay_pay_rate_amt := NVL(l_td_tot_prepay_pay_rate_amt,l_td_sum_prepay_pay_rate_amt)
                                            - l_td_sum_prepay_pay_rate_amt;
  l_td_diff_prepay_clr_rate_amt := NVL(l_td_tot_prepay_clr_rate_amt,l_td_sum_prepay_clr_rate_amt)
                                            - l_td_sum_prepay_clr_rate_amt;


  IF (l_diff_inv_rate_amt <> 0) OR (l_diff_prepay_rate_amt <> 0) OR
     (l_diff_prepay_pay_rate_amt <> 0) OR (l_diff_prepay_clr_rate_amt <> 0) THEN

      IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
          l_log_msg := 'Updating prepay appl rounding amount';
          FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name, l_log_msg);
      END IF;


      UPDATE AP_Prepay_App_Dists APPD
      SET    Base_Amount = Base_Amount + NVL(l_diff_inv_rate_amt,0),
             Rounding_Amt = l_diff_inv_rate_amt,
             Base_Amt_At_Prepay_XRate = Base_Amt_At_Prepay_XRate
                                           + NVL(l_diff_prepay_rate_amt,0),
             Round_Amt_At_Prepay_XRate = l_diff_prepay_rate_amt,
             Base_Amt_At_Prepay_Pay_XRate = Base_Amt_At_Prepay_Pay_XRate
                                               + NVL(l_diff_prepay_pay_rate_amt,0),
             Round_Amt_At_Prepay_Pay_XRate = l_diff_prepay_pay_rate_amt,
             Base_Amt_At_Prepay_Clr_XRate = Base_Amt_At_Prepay_Clr_XRate
                                               + NVL(l_diff_prepay_clr_rate_amt,0),
             Round_Amt_At_Prepay_Clr_XRate = l_diff_prepay_clr_rate_amt
      WHERE  Prepay_History_ID = p_prepay_hist_rec.prepay_history_id
      AND    Invoice_Distribution_ID = l_max_prepay_rec.invoice_distribution_id
      AND    Prepay_App_Distribution_ID = l_max_prepay_rec.prepay_app_distribution_id
      -- AND    Accounting_Event_ID = p_xla_event_rec.event_id
      AND    Prepay_Dist_Lookup_Code IN ('PREPAY APPL','PREPAY APPL REC TAX',
                                         'PREPAY APPL NONREC TAX');


      IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
          l_log_msg := 'Updated prepay appl rounding amount';
          FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name, l_log_msg);
      END IF;


  END IF;


  IF (l_td_diff_inv_rate_amt <> 0) OR (l_td_diff_prepay_rate_amt <> 0) OR
     (l_td_diff_prepay_pay_rate_amt <> 0) OR (l_td_diff_prepay_clr_rate_amt <> 0) THEN


      IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
          l_log_msg := 'Updating tax diff rounding amount';
          FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name, l_log_msg);
      END IF;

      UPDATE AP_Prepay_App_Dists APPD
      SET    Base_Amount = Base_Amount + NVL(l_td_diff_inv_rate_amt,0),
             Rounding_Amt = l_td_diff_inv_rate_amt,
             Base_Amt_At_Prepay_XRate = Base_Amt_At_Prepay_XRate
                                           + NVL(l_td_diff_prepay_rate_amt,0),
             Round_Amt_At_Prepay_XRate = l_td_diff_prepay_rate_amt,
             Base_Amt_At_Prepay_Pay_XRate = Base_Amt_At_Prepay_Pay_XRate
                                               + NVL(l_td_diff_prepay_pay_rate_amt,0),
             Round_Amt_At_Prepay_Pay_XRate = l_td_diff_prepay_pay_rate_amt,
             Base_Amt_At_Prepay_Clr_XRate = Base_Amt_At_Prepay_Clr_XRate
                                               + NVL(l_td_diff_prepay_clr_rate_amt,0),
             Round_Amt_At_Prepay_Clr_XRate = l_td_diff_prepay_clr_rate_amt
      WHERE  Prepay_History_ID = p_prepay_hist_rec.prepay_history_id
      AND    Invoice_Distribution_ID = l_max_prepay_rec.invoice_distribution_id
      AND    Prepay_App_Distribution_ID = l_max_prepay_rec.prepay_app_distribution_id
      -- AND    Accounting_Event_ID = p_xla_event_rec.event_id
      AND    Prepay_Dist_Lookup_Code IN ('TAX DIFF');


      IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
          l_log_msg := 'Updating tax diff rounding amount';
          FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name, l_log_msg);
      END IF;


  END IF;

  -- Logging Infra: Procedure level
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
      l_log_msg := 'End of procedure '|| l_procedure_name;
      FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name||'.end', l_log_msg);
  END IF;


EXCEPTION
  WHEN OTHERS THEN
    IF (SQLCODE = -20100) THEN
      RAISE_APPLICATION_ERROR(-20100, SQLERRM);
    ELSIF (SQLCODE <> -20001) THEN
      FND_MESSAGE.SET_NAME('SQLAP','AP_DEBUG');
      FND_MESSAGE.SET_TOKEN('ERROR',SQLERRM);
      FND_MESSAGE.SET_TOKEN('CALLING_SEQUENCE', l_curr_calling_sequence);
    END IF;
    APP_EXCEPTION.RAISE_EXCEPTION;

END Total_Appl;


-------------------------------------------------------------------------------
-- PROCEDURE  Final_Appl
-- This procedure calculates the rounding amount to relieve the prepaid
-- expense completely.  This is calculated during final application of a
-- prepayment distribution
--
--------------------------------------------------------------------------------
PROCEDURE Final_Appl
     (P_XLA_Event_Rec    IN   ap_accounting_pay_pkg.r_xla_event_info
     ,P_Pay_Hist_Rec     IN   ap_accounting_pay_pkg.r_pay_hist_info
     ,P_Clr_Hist_Rec     IN   ap_accounting_pay_pkg.r_pay_hist_info
     ,P_Inv_Rec          IN   ap_accounting_pay_pkg.r_invoices_info
     ,P_Prepay_Inv_Rec   IN   ap_accounting_pay_pkg.r_invoices_info
     ,P_Prepay_Hist_Rec  IN   AP_ACCTG_PREPAY_DIST_PKG.r_prepay_hist_info
     ,P_Prepay_Dist_Rec  IN   AP_ACCTG_PREPAY_DIST_PKG.r_prepay_dist_info
     ,P_Calling_Sequence IN   VARCHAR2
     ) IS

  l_curr_calling_sequence      VARCHAR2(2000);
  l_invrate_for_prepay         AP_SYSTEM_PARAMETERS_ALL.INVRATE_FOR_PREPAY_TAX%TYPE;

  l_sum_pay_base_amt           NUMBER;
  l_sum_pay_paid_base_amt      NUMBER;
  l_sum_pay_clrd_base_amt      NUMBER;

  l_sum_prepay_rate_amt        NUMBER;
  l_sum_prepay_pay_rate_amt    NUMBER;
  l_sum_prepay_clr_rate_amt    NUMBER;

  l_diff_prepay_rate_amt       NUMBER;
  l_diff_prepay_pay_rate_amt   NUMBER;
  l_diff_prepay_clr_rate_amt   NUMBER;

  l_max_prepay_rec             AP_PREPAY_APP_DISTS%ROWTYPE;
  l_pad_rec                    AP_PREPAY_APP_DISTS%ROWTYPE;

  -- Logging Infra:
  l_procedure_name CONSTANT VARCHAR2(30) := 'Final_Appl';
  l_log_msg        FND_LOG_MESSAGES.MESSAGE_TEXT%TYPE;

BEGIN

  l_curr_calling_sequence := 'AP_ACCTG_PAY_ROUND_PKG.Final_Appl<- ' ||
                                          p_calling_sequence;

  -- Logging Infra: Procedure level
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
      l_log_msg := 'Begin of procedure '|| l_procedure_name;
      FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name||'.begin', l_log_msg);
  END IF;

  BEGIN

    SELECT NVL(ASP.Invrate_for_prepay_tax, 'N')
      INTO l_invrate_for_prepay
      FROM ap_system_parameters_all ASP,
           ap_prepay_history_all APH
     WHERE ASP.Org_id = APH.Org_id
       AND APH.Prepay_history_id = P_Prepay_Hist_Rec.Prepay_History_ID;

    IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
        l_log_msg := 'l_invrate_for_prepay is: '|| l_invrate_for_prepay||
                     'and P_Prepay_Dist_Rec.Line_type_lookup_code is: '||
                     P_Prepay_Dist_Rec.Line_type_lookup_code;
        FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name, l_log_msg);
    END IF;

    IF l_invrate_for_prepay = 'Y' AND P_Prepay_Dist_Rec.Line_type_lookup_code IN ('REC_TAX','NONREC_TAX') THEN

      IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
          l_log_msg := 'Returning from the Final_Appl procedure';
        FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name, l_log_msg);
      END IF;

      RETURN;
    END IF;

  EXCEPTION
   WHEN OTHERS THEN
     IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
         l_log_msg := 'Encountered an Exception '||SQLERRM||
	              ' while checking the option Invrate_for_prepay_tax ';
         FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name, l_log_msg);
     END IF;
  END;

  -- Getting the max of the largest distribution for inserting the rounding dist
  BEGIN
  SELECT APAD.*
  INTO   l_max_prepay_rec
  FROM   AP_Prepay_App_Dists APAD
  WHERE  Invoice_Distribution_ID IN
        (SELECT MAX(APAD1.Invoice_Distribution_ID)
         FROM   AP_Prepay_App_Dists APAD1
         WHERE  APAD1.Prepay_App_Distribution_ID = p_prepay_dist_rec.invoice_distribution_id
         AND    APAD1.Prepay_History_ID = p_prepay_hist_rec.prepay_history_id
         AND    ABS(APAD1.Amount) =
               (SELECT MAX(ABS(APAD2.Amount))
                FROM   AP_Prepay_App_Dists APAD2
                WHERE  APAD2.Prepay_App_Distribution_ID = p_prepay_dist_rec.invoice_distribution_id
                AND    APAD2.Prepay_History_ID = p_prepay_hist_rec.prepay_history_id))
  AND    APAD.Prepay_App_Distribution_ID = p_prepay_dist_rec.invoice_distribution_id
  AND    APAD.Prepay_History_ID = p_prepay_hist_rec.prepay_history_id
  AND    Rownum = 1;
  EXCEPTION
     WHEN NO_DATA_FOUND THEN
        RAISE_APPLICATION_ERROR(-20100, l_procedure_name||
                                ' no_record_in_APAD_while_retrieving_max_dist');
  END; --bug 9936620

 /* Bug 13791619
  -- Get the paid base amount for the payment event
  SELECT SUM(APHD.Paid_Base_Amount)
  INTO   l_sum_pay_paid_base_amt
  FROM   AP_Payment_Hist_Dists APHD,
         AP_Payment_History_All APH
  WHERE  APH.Related_Event_ID = p_pay_hist_rec.Related_Event_ID
  AND    APHD.Payment_History_ID = APH.Payment_History_ID
  AND    Invoice_Distribution_ID IN
                (SELECT AID.Invoice_Distribution_ID
                 FROM   AP_Invoice_Distributions_All AID
                 WHERE  AID.Prepay_Distribution_ID = p_prepay_dist_rec.prepay_distribution_id)
  AND    Pay_Dist_Lookup_Code IN ('CASH', 'DISCOUNT', 'AWT', 'TOTAL PAYMENT ROUNDING',
                                  'FINAL PAYMENT ROUNDING');
				  
  -- Get the cleared base amount for the payment clearing event
  SELECT SUM(APHD.Cleared_Base_Amount)
  INTO   l_sum_pay_clrd_base_amt
  FROM   AP_Payment_Hist_Dists APHD,
         AP_Payment_History_All APH
  WHERE  APH.Related_Event_ID = p_clr_hist_rec.related_event_id
  AND    APHD.Payment_History_ID = APH.Payment_History_ID
  AND    Invoice_Distribution_ID IN
                (SELECT AID.Invoice_Distribution_ID
                 FROM   AP_Invoice_Distributions_All AID
                 WHERE  AID.Prepay_Distribution_ID = p_prepay_dist_rec.prepay_distribution_id)
  AND    Pay_Dist_Lookup_Code IN ('CASH', 'DISCOUNT', 'AWT', 'TOTAL CLEARING ROUNDING',
                                  'FINAL PAYMENT ROUNDING');
*/

/* Bug 13791619 */
  -- Get the paid base amount for the payment event
  SELECT SUM(APHD.Paid_Base_Amount)
  INTO   l_sum_pay_paid_base_amt
  FROM   AP_Payment_Hist_Dists APHD,
         AP_Payment_History_All APH
  WHERE  APH.Related_Event_ID = p_pay_hist_rec.Related_Event_ID
  AND    APHD.Payment_History_ID = APH.Payment_History_ID
  AND    Invoice_Distribution_ID = p_prepay_dist_rec.prepay_distribution_id
  AND    Pay_Dist_Lookup_Code IN ('CASH', 'DISCOUNT', 'AWT', 'TOTAL PAYMENT ROUNDING',
                                  'FINAL PAYMENT ROUNDING');

  -- Get the cleared base amount for the payment clearing event
  SELECT SUM(APHD.Cleared_Base_Amount)
  INTO   l_sum_pay_clrd_base_amt
  FROM   AP_Payment_Hist_Dists APHD,
         AP_Payment_History_All APH
  WHERE  APH.Related_Event_ID = p_clr_hist_rec.related_event_id
  AND    APHD.Payment_History_ID = APH.Payment_History_ID
  AND    Invoice_Distribution_ID = p_prepay_dist_rec.prepay_distribution_id
  AND    Pay_Dist_Lookup_Code IN ('CASH', 'DISCOUNT', 'AWT', 'TOTAL CLEARING ROUNDING',
                                  'FINAL PAYMENT ROUNDING');



  -- Get the sum of the base amounts for the different prepayment xrates
  SELECT SUM(Base_Amt_At_Prepay_XRate),
         SUM(Base_Amt_At_Prepay_Pay_XRate),
         SUM(Base_Amt_At_Prepay_Clr_XRate)
  INTO   l_sum_prepay_rate_amt,
         l_sum_prepay_pay_rate_amt,
         l_sum_prepay_clr_rate_amt
  FROM   AP_Prepay_App_Dists
  WHERE  Prepay_App_Distribution_ID IN
               (SELECT AID.Invoice_Distribution_ID
                FROM   AP_Invoice_Distributions_All AID
                WHERE  AID.Prepay_Distribution_ID = p_prepay_dist_rec.prepay_distribution_id);


  -- bug9609272, added the corresponding ERV amount to the base amount of
  -- the Prepayment Invoice distribution (which is as per the PO exchange
  -- rate), so as to get a sum as per the Prepayment Invoice Exchange Rate
  --
  SELECT AID.Base_Amount +
         NVL((SELECT SUM(NVL(AID_erv.Base_Amount, 0))
                FROM AP_Invoice_Distributions_All AID_erv
               WHERE AID_erv.Invoice_id = AID.Invoice_id
                 AND AID_erv.line_type_lookup_code IN ('ERV', 'TERV')
                 AND AID_erv.related_id = AID.invoice_distribution_id), 0)
  INTO   l_sum_pay_base_amt
  FROM   AP_Invoice_Distributions_All AID
  WHERE  AID.Invoice_Distribution_ID = p_prepay_dist_rec.prepay_distribution_id;

  l_diff_prepay_rate_amt := -(l_sum_pay_base_amt + l_sum_prepay_rate_amt); -- 8256981
  l_diff_prepay_pay_rate_amt := -(l_sum_pay_paid_base_amt + l_sum_prepay_pay_rate_amt); -- 8256981
  l_diff_prepay_clr_rate_amt := -(l_sum_pay_clrd_base_amt + l_sum_prepay_clr_rate_amt); -- 8256981


  l_pad_rec.prepay_history_id := p_prepay_hist_rec.prepay_history_id;
  l_pad_rec.accounting_event_id := p_xla_event_rec.event_id;
  l_pad_rec.invoice_distribution_id := l_max_prepay_rec.invoice_distribution_id;
  l_pad_rec.prepay_app_distribution_id := l_max_prepay_rec.prepay_app_distribution_id;

  l_pad_rec.prepay_exchange_rate := l_max_prepay_rec.prepay_exchange_rate;
  l_pad_rec.prepay_exchange_rate_type := l_max_prepay_rec.prepay_exchange_rate_type;
  l_pad_rec.prepay_exchange_date := l_max_prepay_rec.prepay_exchange_date;
  l_pad_rec.prepay_pay_exchange_rate := l_max_prepay_rec.prepay_pay_exchange_rate;
  l_pad_rec.prepay_pay_exchange_rate_type := l_max_prepay_rec.prepay_pay_exchange_rate_type;
  l_pad_rec.prepay_pay_exchange_date := l_max_prepay_rec.prepay_pay_exchange_date;
  l_pad_rec.prepay_clr_exchange_rate := l_max_prepay_rec.prepay_clr_exchange_rate;
  l_pad_rec.prepay_clr_exchange_rate_type := l_max_prepay_rec.prepay_clr_exchange_rate_type;
  l_pad_rec.prepay_clr_exchange_date := l_max_prepay_rec.prepay_clr_exchange_date;
  l_pad_rec.awt_related_id := l_max_prepay_rec.awt_related_id;

  l_pad_rec.amount := 0;
  l_pad_rec.base_amount := 0;

  IF (l_diff_prepay_rate_amt <> 0) OR (l_diff_prepay_pay_rate_amt <> 0)
            OR (l_diff_prepay_clr_rate_amt <> 0) THEN

      IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
          l_log_msg := 'Inserting final appl rounding dist';
          FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name, l_log_msg);
      END IF;


      l_pad_rec.prepay_dist_lookup_code := 'FINAL APPL ROUNDING';
      l_pad_rec.base_amt_at_prepay_xrate := l_diff_prepay_rate_amt;
      l_pad_rec.base_amt_at_prepay_pay_xrate := l_diff_prepay_pay_rate_amt;
      l_pad_rec.base_amt_at_prepay_clr_xrate := l_diff_prepay_clr_rate_amt;


      IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
          l_log_msg := 'Calling procedure Prepay_Dist_Insert';
          FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name, l_log_msg);
      END IF;


      AP_ACCTG_PREPAY_DIST_PKG.Prepay_Dist_Insert
                                          (l_pad_rec,
                                           l_curr_calling_sequence);

      IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
          l_log_msg := 'Procedure Prepay_Dist_Insert executed';
          FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name, l_log_msg);
      END IF;


  END IF;

  -- Logging Infra: Procedure level
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
      l_log_msg := 'End of procedure '|| l_procedure_name;
      FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name||'.end', l_log_msg);
  END IF;


EXCEPTION
  WHEN OTHERS THEN
    IF (SQLCODE = -20100) THEN
      RAISE_APPLICATION_ERROR(-20100, SQLERRM);
    ELSIF (SQLCODE <> -20001) THEN
      FND_MESSAGE.SET_NAME('SQLAP','AP_DEBUG');
      FND_MESSAGE.SET_TOKEN('ERROR',SQLERRM);
      FND_MESSAGE.SET_TOKEN('CALLING_SEQUENCE', l_curr_calling_sequence);
    END IF;
    APP_EXCEPTION.RAISE_EXCEPTION;

END Final_Appl;

-------------------------------------------------------------------------------
-- Final_Cash
-- This is procedure is to handle cash rounding where the
-- ap_checks_all.base_amount and
-- sum(ap_invoice_payments_all.payment_base_amount) are not
-- Matching.
-- Bug 8288996
--
--
--------------------------------------------------------------------------------
PROCEDURE Final_Cash
     (P_XLA_Event_Rec    IN   ap_accounting_pay_pkg.r_xla_event_info
     ,P_Calling_Sequence IN   VARCHAR2
     ) IS

  l_curr_calling_sequence    VARCHAR2(2000);
  l_max_pd_rec               AP_PAYMENT_HIST_DISTS%ROWTYPE;
  l_pd_rec                   AP_PAYMENT_HIST_DISTS%ROWTYPE;
  l_ac_base_amount           NUMBER;
  l_transaction_type         VARCHAR2(50);
  l_sum_cash_amt             NUMBER;
  l_ac_amount                NUMBER;
  l_do_cash_rounding         NUMBER;
  l_procedure_name           CONSTANT VARCHAR2(30) := 'Final_Cash';
  l_log_msg                  FND_LOG_MESSAGES.MESSAGE_TEXT%TYPE;
  l_pay_hist_rec             ap_accounting_pay_pkg.r_pay_hist_info;

-- 16823904
  l_clr_pmt_base_rate		NUMBER;
  l_clr_bank_base_rate		NUMBER;
  l_bank_curr			VARCHAR2(50);
  l_pay_curr			VARCHAR2(50);

 -- Bug 17600307
  l_ac_rate    NUMBER;
BEGIN

  l_curr_calling_sequence := 'AP_ACCTG_PAY_ROUND_PKG.Final_Cash<- ' ||
                                           P_Calling_Sequence;


  -- Logging Infra: Procedure level
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
      l_log_msg := 'Begin of procedure '|| l_procedure_name;
      FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name||'.begin', l_log_msg);
  END IF;


  SELECT count(*) into l_do_cash_rounding
        FROM ap_invoice_payments_all aip
   WHERE check_id = P_XLA_Event_Rec.source_id_int_1
         AND NOT EXISTS (SELECT 1
                           FROM ap_payment_hist_dists aphd
                          WHERE aphd.invoice_payment_id = aip.invoice_payment_id);

  IF ( l_do_cash_rounding = 0) THEN -- Now Do the Cash Rounding


  -- Here to handle Cash Rounding
  Begin

   -- 16823904 :fetch payment hist header
     OPEN Ap_Acctg_Pay_Dist_Pkg.Payment_History(p_xla_event_rec.event_id);
     FETCH Ap_Acctg_Pay_Dist_Pkg.Payment_History INTO l_pay_hist_rec;
     CLOSE Ap_Acctg_Pay_Dist_Pkg.Payment_History;	

     /* bug 11906025 and 12900918 ,12911326 modified the derivation logic of l_ac_base_amount */

    -- 16823904 :Altered the HAVING Condition in below select 
	 IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
           l_log_msg := 'Inside final_cash, before running group by sql';
           FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name, l_log_msg);
         END IF;
  
         IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
           l_log_msg := 'l_pay_hist_rec.pmt_currency_code :'|| l_pay_hist_rec.pmt_currency_code||' l_pay_hist_rec.bank_currency_code :'|| l_pay_hist_rec.bank_currency_code;
           FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name, l_log_msg);
         END IF;
	 IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
           l_log_msg := 'l_pay_hist_rec.bank_to_base_xrate :'|| l_pay_hist_rec.bank_to_base_xrate||' l_pay_hist_rec.pmt_to_base_xrate :'|| l_pay_hist_rec.pmt_to_base_xrate;
           FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name, l_log_msg);
         END IF;

           SELECT DECODE(aps.recon_accounting_flag, 'Y',COALESCE(ac.cleared_base_amount,ac.base_amount, 0),NVL(base_amount,0))
                , DECODE(aps.recon_accounting_flag, 'Y', 'CLEARING', DECODE(ac.future_pay_due_date, NULL, 'CREATED', 'MATURITY')) type
                , ac.amount
		, nvl(ac.exchange_rate,1)
             INTO l_ac_base_amount
                , l_transaction_type
                , l_ac_amount
		, l_ac_rate
             FROM ap_checks_all ac
                , ap_system_parameters_all aps
                , ap_invoice_payments_all aip
            WHERE ac.org_id                = aps.org_id
              AND ac.check_id              = P_XLA_Event_Rec.source_id_int_1
              AND aip.check_id             = ac.check_id
              AND ac.currency_code        <> aps.base_currency_code
              AND aip.reversal_inv_pmt_id IS NULL
         GROUP BY ac.check_id
                , ac.amount
                , ac.base_amount
                , ac.future_pay_due_date
                , aps.recon_accounting_flag
                ,ac.cleared_base_amount
                ,ac.cleared_amount
                ,ac.cleared_exchange_rate
                ,ac.cleared_charges_base_amount
                ,ac.cleared_error_base_amount
		,ac.exchange_rate -- bug 17600307
         HAVING ((ABS(NVL(ac.base_amount, 0) -SUM(NVL(aip.payment_base_amount, 0))) > 0)
                  OR
                 (     ac.cleared_amount IS NOT NULL
                   AND ac.cleared_exchange_rate IS NOT NULL
                   AND SUM( AP_Utilities_Pkg.AP_Round_Currency(AIP.amount * DECODE(l_pay_hist_rec.pmt_currency_code,l_pay_hist_rec.bank_currency_code,
                                                                        NVL(l_pay_hist_rec.bank_to_base_xrate,1),NVL(l_pay_hist_rec.pmt_to_base_xrate,1))
                                                                        ,aps.base_currency_code)) 
                                                                        <> (ac.cleared_base_amount - ac.cleared_charges_base_amount - ac.cleared_error_base_amount)
                  ));

     SELECT SUM(DECODE(l_transaction_type, 'CLEARING' , nvl(cleared_base_amount,0),
                           'MATURITY' , nvl(matured_base_amount,0),nvl(paid_base_amount,0)))
           INTO l_sum_cash_amt
           FROM ap_payment_hist_dists aphd,
                ap_payment_history_all aph
          WHERE aph.payment_history_id = aphd.payment_history_id
            AND aph.check_id = P_XLA_Event_Rec.source_id_int_1
                AND aph.accounting_event_id = P_XLA_Event_Rec.event_id
                AND aph.posted_flag <> 'Y'
                AND aphd.pay_dist_lookup_code in ( 'CASH'
                                                 , 'FINAL CASH ROUNDING'
                                                 , 'BANK CHARGE'
                                                 , 'BANK ERROR') -- Bug 13783723
                AND aph.transaction_type = DECODE(l_transaction_type, 'CLEARING' , 'PAYMENT CLEARING',
                           'MATURITY' , 'PAYMENT MATURITY','PAYMENT CREATED');

     IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
           l_log_msg := 'l_ac_base_amount :'|| l_ac_base_amount||' l_sum_cash_amt :'|| l_sum_cash_amt;
           FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name, l_log_msg);
     END IF;

     IF ( l_sum_cash_amt <> l_ac_base_amount AND l_sum_cash_amt <> 0) Then
         --Getting Payment History Header Details
          /*Bug 16823904 not required as fetched above
         SELECT APH.Payment_History_ID,
                APH.Pmt_Currency_Code,
                APH.Bank_Currency_Code,
                APH.Bank_To_Base_XRate_Type,
                APH.Bank_To_Base_XRate_Date,
                NVL(APH.Bank_To_Base_XRate,1)
           INTO l_pay_hist_rec.Payment_History_ID,
                l_pay_hist_rec.Pmt_Currency_Code,
                l_pay_hist_rec.Bank_Currency_Code,
                l_pay_hist_rec.Bank_To_Base_XRate_Type,
                l_pay_hist_rec.Bank_To_Base_XRate_Date,
                l_pay_hist_rec.Bank_To_Base_XRate
           FROM AP_Payment_History_All APH
          WHERE APH.Accounting_Event_ID = p_xla_event_rec.event_id;
        */


                 -- Getting the distribution for inserting the rounding distribution
         BEGIN
         SELECT APHD.*
           INTO l_max_pd_rec
           FROM AP_Payment_Hist_Dists APHD
          WHERE APHD.Accounting_Event_ID = p_xla_event_rec.event_id
                    AND APHD.pay_dist_lookup_code = 'CASH'
            AND Rownum = 1;
         EXCEPTION
            WHEN NO_DATA_FOUND THEN
               RAISE_APPLICATION_ERROR(-20100, l_procedure_name||
                                       ' no_record_in_APHD_while_retrieving_max_dist');
         END; --bug 9936620

         l_pd_rec.invoice_distribution_id := l_max_pd_rec.invoice_distribution_id;
         l_pd_rec.payment_history_id := l_max_pd_rec.payment_history_id;
         l_pd_rec.invoice_payment_id := l_max_pd_rec.invoice_payment_id;
         l_pd_rec.invoice_adjustment_event_id := l_max_pd_rec.invoice_adjustment_event_id;
         l_pd_rec.accounting_event_id := p_xla_event_rec.event_id;
         l_pd_rec.amount := 0;
         l_pd_rec.invoice_dist_amount := 0;
         l_pd_rec.bank_curr_amount := 0;
         l_pd_rec.invoice_dist_base_amount := 0;
         l_pd_rec.pay_dist_lookup_code := 'FINAL CASH ROUNDING';
         l_pd_rec.paid_base_amount := 0;
         l_pd_rec.cleared_base_amount := 0;
         l_pd_rec.matured_base_amount := 0;
         --bug 9495694, removed condition 1=2 for clearing transaction_type and added
         --condition to not go for final cash rounding if clearing is in ledger_currency

             IF l_transaction_type = 'CLEARING' THEN
                -- Bug 16823904 Commented as not needed here anymore
                /*IF l_pay_hist_rec.bank_currency_code <> ap_accounting_pay_pkg.g_base_currency_code Then
                   l_ac_base_amount := AP_Accounting_Pay_Pkg.Get_Base_Amount
                                      (l_ac_amount,
                                       l_pay_hist_rec.pmt_currency_code,
                                       ap_accounting_pay_pkg.g_base_currency_code,
                                       l_pay_hist_rec.bank_to_base_xrate_type,
                                       l_pay_hist_rec.bank_to_base_xrate_date,
                                       l_pay_hist_rec.bank_to_base_xrate,
                                       l_curr_calling_sequence);
                END IF; --bug 9710257, added IF
                */

                     l_pd_rec.cleared_base_amount := l_ac_base_amount - l_sum_cash_amt;

                     IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
                   l_log_msg := 'l_pd_rec.cleared_base_amount :'|| l_pd_rec.cleared_base_amount;
                   FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name, l_log_msg);
             END IF;
		-- Bug 17600307 removed condition 1=2 for Maturity
             ELSIF (l_transaction_type = 'MATURITY' 
		    AND l_pay_hist_rec.PMT_TO_BASE_XRATE IS NOT NULL
		    AND  l_pay_hist_rec.PMT_TO_BASE_XRATE = l_ac_rate ) Then

                     l_pd_rec.matured_base_amount := l_ac_base_amount - l_sum_cash_amt;
         ELSE                    --bug 8880820
             IF ( l_transaction_type = 'CREATED') THEN
                l_pd_rec.paid_base_amount := l_ac_base_amount - l_sum_cash_amt;
             END IF;        -- l_transaction_type
         END IF;                 --bug 8880820

         IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
           l_log_msg := 'Calling proc AP_Acctg_Pay_Dist_Pkg.Pay_Dist_Insert';
           FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name, l_log_msg);
         END IF;

         IF (nvl(l_pd_rec.paid_base_amount,0) <> 0 OR
                     nvl(l_pd_rec.cleared_base_amount,0) <> 0 OR
                         nvl(l_pd_rec.matured_base_amount,0) <> 0 ) Then

             AP_Acctg_Pay_Dist_Pkg.Pay_Dist_Insert
                                  (l_pd_rec,
                                   l_curr_calling_sequence);

                 End IF; -- nvl(l_pd_rec.paid_base_amount,0) <> 0 ....

         IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
           l_log_msg := 'Proc AP_Acctg_Pay_Dist_Pkg.Pay_Dist_Insert executed';
           FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name, l_log_msg);
         END IF;

     END IF; -- l_sum_cash_amt <> l_ac_base_amount

  EXCEPTION
    WHEN OTHERS THEN
    IF (SQLCODE = -20100) THEN
      RAISE_APPLICATION_ERROR(-20100, SQLERRM);
    ELSE
      NULL;
    END IF;
  END;

  END IF; -- l_do_cash_rounding = 0

  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
      l_log_msg := 'End of procedure '|| l_procedure_name;
      FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name||'.end', l_log_msg);
  END IF;

EXCEPTION
  WHEN OTHERS THEN
    IF (SQLCODE = -20100) THEN
      RAISE_APPLICATION_ERROR(-20100, SQLERRM);
    ELSIF (SQLCODE <> -20001) THEN
      FND_MESSAGE.SET_NAME('SQLAP','AP_DEBUG');
      FND_MESSAGE.SET_TOKEN('ERROR',SQLERRM);
      FND_MESSAGE.SET_TOKEN('CALLING_SEQUENCE', l_curr_calling_sequence);
    END IF;
    APP_EXCEPTION.RAISE_EXCEPTION;

END Final_Cash;

END AP_ACCTG_PAY_ROUND_PKG;
/

COMMIT;
EXIT;
