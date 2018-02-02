REM +==================================================================+
REM |                Copyright (c) 1999, 2014 Oracle Corporation
REM |                   Redwood Shores, California, USA
REM |                        All rights reserved.
REM +==================================================================+
REM |  Name
REM |    appredib.pls
REM |
REM |  Description - Package AP_ACCTG_PREPAY_DIST_PKG
REM |  This package is responsible for creating the payment distributions
REM |  for prepayment application related events. The payment distributions
REM |  for these prepayment application events are created based on the
REM |  invoice distributions.
REM |
REM |  History
REM |    Created By:  Haritha Redreddy  (06/01/04)
REM +==================================================================+
REM dbdrv: sql ~PROD ~PATH ~FILE none none none package &phase=plb \
REM dbdrv: checkfile(120.20.12010000.48=120.66.12020000.7)(120.18.12000000.49=120.20.12010000.42):~PROD:~PATH:~FILE

SET VERIFY OFF
WHENEVER SQLERROR EXIT FAILURE ROLLBACK
WHENEVER OSERROR EXIT FAILURE ROLLBACK


CREATE OR REPLACE PACKAGE BODY AP_ACCTG_PREPAY_DIST_PKG AS
/* $Header: appredib.pls 120.66.12020000.7 2014/04/30 20:16:02 vasvenka ship $ */

  G_Total_Dist_Amt             NUMBER := 0;
  G_Total_Prorated_Amt         NUMBER := 0;
  G_Total_Tax_Diff_Amt         NUMBER := 0;
  G_Total_Inv_Amount           NUMBER := 0; --Bug8244163
  G_Total_Dist_Amount          NUMBER := 0; --Bug8244163
  G_Total_awt_amount           NUMBER := 0; --Bug9106549

  -- Logging Infra
  G_CURRENT_RUNTIME_LEVEL      NUMBER                := FND_LOG.G_CURRENT_RUNTIME_LEVEL;
  G_LEVEL_UNEXPECTED           CONSTANT NUMBER       := FND_LOG.LEVEL_UNEXPECTED;
  G_LEVEL_ERROR                CONSTANT NUMBER       := FND_LOG.LEVEL_ERROR;
  G_LEVEL_EXCEPTION            CONSTANT NUMBER       := FND_LOG.LEVEL_EXCEPTION;
  G_LEVEL_EVENT                CONSTANT NUMBER       := FND_LOG.LEVEL_EVENT;
  G_LEVEL_PROCEDURE            CONSTANT NUMBER       := FND_LOG.LEVEL_PROCEDURE;
  G_LEVEL_STATEMENT            CONSTANT NUMBER       := FND_LOG.LEVEL_STATEMENT;
  G_MODULE_NAME                CONSTANT VARCHAR2(50) := 'AP.PLSQL.AP_ACCTG_PREPAY_DIST_PKG.';
  -- Logging Infra


-------------------------------------------------------------------------------
-- PROCEDURE  UPDATE_GAIN_LOSS_IND
-- The purpose of this procedure is to update the gain_loss_indicator on the
-- prepay history table based on the exchange rates of prepayment transactions.
--
--------------------------------------------------------------------------------
PROCEDURE Update_Gain_Loss_Ind
     (P_XLA_Event_Rec      IN   ap_accounting_pay_pkg.r_xla_event_info
     ,P_Calling_Sequence   IN   VARCHAR2
     ) IS

  l_curr_calling_sequence    VARCHAR2(2000);

  -- Logging Infra:
  l_procedure_name              CONSTANT VARCHAR2(30) := 'Update_Gain_Loss_Ind';
  l_log_msg                     FND_LOG_MESSAGES.MESSAGE_TEXT%TYPE;
  l_gain_loss_indicator         ap_prepay_history_all.gain_loss_indicator%type;
  l_gain_loss_indicator_parent  ap_prepay_history_all.gain_loss_indicator%type;  -- bug9175969
  l_reversal_adj                VARCHAR2(1);

BEGIN

  l_curr_calling_sequence := 'AP_Acctg_Prepay_Dist_Pkg.Update_Gain_Loss_Ind<- ' ||
                                      p_calling_sequence;


  -- Logging Infra: Setting up runtime level
  G_CURRENT_RUNTIME_LEVEL := FND_LOG.G_CURRENT_RUNTIME_LEVEL;

  -- Logging Infra: Procedure level
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
      l_log_msg := 'Begin of procedure '|| l_procedure_name;
      FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name||'.begin', l_log_msg);
  END IF;


  IF (P_XLA_Event_Rec.event_type_code ='PREPAYMENT APPLICATION ADJ') then    -- bug9175969

    IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL ) THEN
      l_log_msg := 'Before getting the Gain/Loss Indicator for the '||
                   'Related Prepayment Event ';
      FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME||l_procedure_name, l_log_msg);
    END IF;

    BEGIN

      l_gain_loss_indicator_parent := NULL;

      SELECT aph1.Gain_Loss_Indicator
        INTO l_gain_loss_indicator_parent
        FROM AP_Prepay_History_All aph1,
             AP_Prepay_History_All APH
       WHERE aph1.invoice_id=aph.invoice_id
         AND aph1.accounting_event_id = aph.related_prepay_app_event_id
         AND aph.accounting_event_id = p_xla_event_rec.event_id
         AND rownum=1;

    EXCEPTION
      WHEN OTHERS THEN
        l_gain_loss_indicator_parent := NULL;
    END;

    IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
      l_log_msg := 'The Gain/Loss Indicator Fetched is  '|| l_gain_loss_indicator_parent;
      FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME||l_procedure_name, l_log_msg);
    END IF;

    IF l_gain_loss_indicator_parent IS NOT NULL THEN

      BEGIN
        l_reversal_adj := 'N';
 
        SELECT 'Y' 
          INTO l_reversal_adj
          FROM dual
         WHERE EXISTS
             (SELECT 1
                FROM ap_prepay_history_all apph,
                     ap_prepay_app_dists apad,
                     ap_prepay_app_dists apad_rel,
                     ap_prepay_history_all apph_rel
               WHERE apph.accounting_event_id = P_XLA_Event_Rec.Event_ID
                 AND apph.prepay_history_id = apad.prepay_history_id 
                 AND apad.reversed_prepay_app_dist_id = apad_rel.prepay_app_dist_id
                 AND apad_rel.prepay_history_id = apph_rel.prepay_history_id
                 AND apph_rel.accounting_event_id = apph.related_prepay_app_event_id);

      EXCEPTION
        WHEN OTHERS THEN
          l_reversal_adj := 'N';
      END;

      IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
        l_log_msg := 'The value of l_reversal_adj is: '||l_reversal_adj;
        FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME||l_procedure_name, l_log_msg);
      END IF;

      IF l_reversal_adj = 'N' THEN
         l_gain_loss_indicator_parent := NULL;
      END IF;

      IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
        l_log_msg := 'Final value of l_gain_loss_indicator_parent '||l_gain_loss_indicator_parent;
        FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME||l_procedure_name, l_log_msg);
    END IF;
 
    END IF;

  END IF;

  -- Added by abhsaxen for bug 9032498
  --
  UPDATE AP_Prepay_History_All APH
     SET Gain_Loss_Indicator =
                 (SELECT DECODE(APH.Transaction_Type, 'PREPAYMENT APPLIED',
                           DECODE(SIGN(SUM(APAD.Base_Amount - APAD.Base_Amt_At_Prepay_XRate)),
                                  -1, 'G', 1, 'L', NULL),
                         'PREPAYMENT UNAPPLIED',
                           DECODE(SIGN(SUM(APAD.Base_Amount - APAD.Base_Amt_At_Prepay_XRate)),
                                  1, 'G', -1, 'L', NULL),
                        'PREPAYMENT APPLICATION ADJ',
                            DECODE(SIGN(SUM(APAD.Base_Amount - APAD.Base_Amt_At_Prepay_XRate)),
                                  -1, 'G', 1, 'L',
                                  0, l_gain_loss_indicator_parent))					-- bug9175969
                  FROM   AP_Prepay_App_Dists APAD,
		         AP_System_Parameters_ALL ASP
                  WHERE  ASP.Org_ID = APH.Org_ID
		  AND    APAD.Prepay_History_ID = APH.Prepay_History_ID
                  AND    APAD.Accounting_Event_ID = p_xla_event_rec.event_id
                  AND    APAD.PREPAY_DIST_LOOKUP_CODE NOT IN ('FINAL PAYMENT ROUNDING',   
		                                              'FINAL APPL ROUNDING')			-- bug9716573
                  AND    NOT (NVL(ASP.INVRATE_FOR_PREPAY_TAX, 'N') = 'Y' AND                            -- bug11651946
                              APAD.Prepay_Dist_Lookup_Code LIKE '%TAX%')
                 )							 
   WHERE APH.Accounting_Event_ID = p_xla_event_rec.event_id;


    --bug9464881
    BEGIN

      SELECT aph.gain_loss_indicator
        INTO l_gain_loss_indicator
        FROM ap_prepay_history_all aph
       WHERE APH.Accounting_Event_ID = p_xla_event_rec.event_id;
      
      IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN 
        l_log_msg := 'APH.Gain_Loss_Indicator: '|| nvl(l_gain_loss_indicator,'NULL');
        FND_LOG.STRING(G_LEVEL_STATEMENT, G_MODULE_NAME||l_procedure_name, l_log_msg);
      END IF;
    EXCEPTION
      WHEN OTHERS THEN
        IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
          l_log_msg := ' Encountered an Exception:'||SQLERRM||
                       ' while fetching the gain/loss indicator ';
          FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name, l_log_msg);
        END IF;  
    END;


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

END Update_Gain_Loss_Ind;


-------------------------------------------------------------------------------
-- PROCEDURE Prepay_Hist_Insert
-- The purpose of this procedure is to insert the prepayment history info
-- into the prepayment history table
--
--------------------------------------------------------------------------------
-- Bug 4996808. Inserting the prepay headers instead of in the accounting events
-- procedure
PROCEDURE Prepay_Hist_Insert
     (P_Invoice_ID         IN   NUMBER
     ,P_Calling_Sequence   IN   VARCHAR2
     ) IS

  l_curr_calling_sequence         VARCHAR2(2000);
  l_line_amount                   NUMBER;
  l_transaction_type              VARCHAR2(30);
  l_prepay_invoice_id             NUMBER;
  l_prepay_line_number            NUMBER;
  l_accounting_event_id           NUMBER;  --bug9038462
  l_org_id                        NUMBER;
  l_invoice_line_number           NUMBER;
  l_accounting_date               DATE;
  l_related_prepay_app_event_id   NUMBER;
  l_group_number                  NUMBER;

   -- Logging:
  l_procedure_name CONSTANT VARCHAR2(30) := 'Prepay_Hist_Insert';
  l_log_msg        FND_LOG_MESSAGES.MESSAGE_TEXT%TYPE;


  -- bug9038462,
  -- 1. changed both parts of the union to ensure
  -- creation of a new Prepayment History record for an
  -- Unencumbered and Unaccounted Prepayment Application or
  -- Unapplication record
  --
  -- 2. fetched the accounting_event_id from the Invoice
  -- distribution to be stamped on the Prepay History record
  -- at the time of regeneration
  --
  CURSOR c_prepay_history IS
  SELECT AIL.Line_Number,
         AIL.Amount Amount,
         AIL.Prepay_Invoice_ID,
         AIL.Prepay_Line_Number,
         AID.Accounting_Event_Id,
         AIL.Org_ID,
         AID.Accounting_Date,
         -- 6718967
         DECODE(NVL(AID.Parent_Reversal_ID,-99), -99, 1, 2) Group_Number
  FROM   AP_Invoice_Lines_ALL AIL,
         AP_Invoice_Distributions_All AID
  WHERE  AIL.Invoice_ID = p_invoice_id
  AND    AIL.Line_Type_Lookup_Code = 'PREPAY'
  AND    AIL.Invoice_ID = AID.Invoice_ID
  AND    AIL.Line_Number = AID.Invoice_Line_Number
  --AND    AID.Accounting_Event_ID IS NULL
  AND    nvl(AID.posted_flag, 'N') <> 'Y'
  AND    nvl(AID.encumbered_flag, 'N') <> 'Y'
  GROUP  BY AIL.Invoice_ID, AIL.Line_Number, AIL.Amount, AIL.Prepay_Invoice_ID,
            AIL.Prepay_Line_Number, AIL.Org_ID, AID.Accounting_Date,
            AID.Accounting_Event_Id,
            -- 6718967
            DECODE(NVL(AID.Parent_Reversal_ID,-99), -99, 1, 2)
  UNION
  SELECT AID.Invoice_Line_Number,
         SUM(AID.Amount) Amount,
         AIL1.Invoice_ID,
         AIL1.Line_Number,
         AID.Accounting_Event_Id,
         AIL1.Org_ID,
         AID.Accounting_Date,
         -- 6718967
         DECODE(NVL(AID.Parent_Reversal_ID,-99), -99, 1, 2) Group_Number
  FROM   AP_Invoice_Lines AIL,
         AP_Invoice_Distributions AID,
         AP_Invoice_Lines AIL1,
         AP_Invoice_Distributions AID1
  WHERE  AID.Invoice_ID = p_invoice_id
  AND    AID.Line_Type_Lookup_Code = 'PREPAY'
  AND    AID.Invoice_ID = AIL.Invoice_ID
  AND    AID.Invoice_Line_Number = AIL.Line_Number
  AND    AIL.Line_Type_Lookup_Code <> 'PREPAY'
  --AND    AID.Accounting_Event_ID IS NULL
  AND    NVL(AID.posted_flag, 'N') <> 'Y'
  AND    NVL(AID.encumbered_flag, 'N') <> 'Y'
  AND    AID.Prepay_Distribution_ID = AID1.Invoice_Distribution_ID
  AND    AIL1.Invoice_ID = AID1.Invoice_ID
  AND    AIL1.Line_Number = AID1.Invoice_Line_Number
  GROUP  BY AIL1.Invoice_ID, AIL1.Line_Number, AIL1.Org_ID,
            AID.Invoice_Line_Number, AID.Accounting_Date,
            AID.Accounting_Event_Id,
            -- 6718967
            DECODE(NVL(AID.Parent_Reversal_ID,-99), -99, 1, 2);


BEGIN

  l_curr_calling_sequence := p_calling_sequence ||
            ' -> AP_ACCTG_PREPAY_DISTS_PKG.PREPAY_HIST_INSERT';

  G_CURRENT_RUNTIME_LEVEL := FND_LOG.G_CURRENT_RUNTIME_LEVEL;

  l_log_msg :='Begin of procedure '||l_procedure_name;
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
     FND_LOG.STRING(G_LEVEL_PROCEDURE,
                    G_MODULE_NAME || l_procedure_name,
                    l_log_msg);
  END IF;

  OPEN c_prepay_history;
  LOOP
    FETCH c_prepay_history INTO l_invoice_line_number,
          l_line_amount, l_prepay_invoice_id, l_prepay_line_number,
          l_accounting_event_id, l_org_id, l_accounting_date, l_group_number;
    EXIT WHEN c_prepay_history%NOTFOUND OR
              c_prepay_history%NOTFOUND IS NULL;

    IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
        l_log_msg := 'CUR: C_Prepay_History: prepay_invoice_id = '||
                                           l_prepay_invoice_id
                     || ' Prepay_Line_Number = ' || l_prepay_line_number
                     || ' Invoice_Line_Number = ' ||l_invoice_line_number;
        FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name, l_log_msg);
    END IF;


    BEGIN

      SELECT min(accounting_Event_id)
      INTO   l_related_prepay_app_event_id
      FROM   AP_INVOICE_DISTRIBUTIONS AID
      WHERE  AID.line_type_lookup_code = 'PREPAY'
      AND    nvl(posted_flag,'N') = 'Y'
      AND    nvl(AID.amount,0) < 0
      AND    AID.invoice_id = P_invoice_id
      AND    AID.invoice_line_number = l_invoice_line_number;


    EXCEPTION
      WHEN NO_DATA_FOUND THEN
        l_related_prepay_app_event_id:= null;

    END;

    -- Bug 6718967. Added group number to identify if it is
    -- prepayment applied or unapplied.
    IF l_group_number = 1 THEN
      l_transaction_type := 'PREPAYMENT APPLIED';
    ELSE
      l_transaction_type := 'PREPAYMENT UNAPPLIED';
    END IF;


    INSERT INTO AP_PREPAY_HISTORY_ALL
          (PREPAY_HISTORY_ID
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
   VALUES (AP_PREPAY_HISTORY_S.nextval
          ,l_prepay_invoice_id
          ,l_prepay_line_number
          ,l_accounting_event_id   --bug9038462
          ,'N'
          ,p_invoice_id
          ,l_invoice_line_number
          ,l_accounting_date
          ,NULL
          ,l_org_id
          ,'N'
          ,l_related_prepay_app_event_id
          ,l_transaction_type
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
  CLOSE c_prepay_history;

  l_log_msg :='End of procedure '||l_procedure_name;

  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
      FND_LOG.STRING(G_LEVEL_PROCEDURE,
                     G_MODULE_NAME || l_procedure_name,
                     l_log_msg);
  END IF;

END Prepay_Hist_Insert;



-------------------------------------------------------------------------------
-- PROCEDURE Prepay_Dist_Appl
-- The purpose of this procedure is to prorate the prepayment application
-- amount for all the distributions of the invoice that the prepayment is applied
-- and generate the prepayment application distributions.
--
--------------------------------------------------------------------------------
PROCEDURE Prepay_Dist_Appl
     (P_Invoice_ID         IN   NUMBER
     ,P_Calling_Sequence   IN   VARCHAR2
     ) IS
  l_prepay_hist_id                 AP_PREPAY_HISTORY_ALL.Prepay_History_id%TYPE := -99;
  l_accounting_event_id            XLA_EVENTS.event_id%TYPE := -99;
  l_historical_flag                AP_PREPAY_HISTORY_ALL.Historical_flag%TYPE := 'N';
  l_posted_flag                    AP_PREPAY_HISTORY_ALL.Posted_flag%TYPE := 'N';
  l_upg_batch_id                   XLA_AE_HEADERS.Upg_Batch_Id%TYPE := -99;

  l_curr_calling_sequence    VARCHAR2(2000);
  l_total_paid_amt           NUMBER;
  l_final_payment            BOOLEAN := FALSE;

  l_pay_hist_rec             ap_accounting_pay_pkg.r_pay_hist_info;
  l_clr_hist_rec             ap_accounting_pay_pkg.r_pay_hist_info;
  l_inv_rec                  ap_accounting_pay_pkg.r_invoices_info;
  l_prepay_inv_rec           ap_accounting_pay_pkg.r_invoices_info;
  l_inv_dist_rec             ap_accounting_pay_pkg.r_inv_dist_info;
  l_prepay_hist_rec          r_prepay_hist_info;
  l_prepay_dist_rec          r_prepay_dist_info;
  l_payment_type_flag        AP_CHECKS_ALL.payment_type_flag%TYPE;
  l_pay_trx_type             AP_PAYMENT_HISTORY_ALL.transaction_type%TYPE;

  -- bug9492002
  -- commenting below for bug10183934
  --l_upg_pmt_hist        NUMBER;
  --l_upg_inv_pmts        NUMBER;
  --l_upg_prepay_app      NUMBER;


  -- Logging Infra:
  l_procedure_name CONSTANT VARCHAR2(30) := 'Prepay_Dist_Appl';
  l_log_msg        FND_LOG_MESSAGES.MESSAGE_TEXT%TYPE;

  -- BUG # 7688509
  -- condition: historical_flag =Y
  --         and event is 'INVOICE ADJUSTMENT'
  --         and ap_system_parameter.auto_offset_flag ='N'
  --         and sum of the distributions in the invoice adjustment event is 0
  b_generate_prepay_dist   BOOLEAN;
  l_sum_per_event       NUMBER;
  l_dist_count_per_event       NUMBER;

  CURSOR c_sum_per_event(p_acct_event_id  NUMBER) IS
  SELECT SUM(amount), count(1)
    FROM ap_invoice_distributions_all aid,
         xla_events evnt,
         xla_ae_headers xah,
         ap_system_parameters_all asp
   WHERE aid.accounting_event_id = p_acct_event_id
     AND aid.accounting_event_id = evnt.event_id
     AND evnt.event_type_code in ('INVOICE ADJUSTED', 'CREDIT MEMO ADJUSTED',
                                  'DEBIT MEMO ADJUSTED','PREPAYMENT ADJUSTED') -- 12731687
     AND evnt.event_id = xah.event_id
     AND xah.upg_batch_id IS NOT NULL
     AND aid.org_id = asp.org_id
     AND asp.automatic_offsets_flag = 'N'
     AND aid.historical_flag = 'Y'
     AND evnt.application_id=200;

BEGIN

  l_curr_calling_sequence := 'AP_ACCTG_PREPAY_DIST_PKG.Prepay_Dist_Appl<- ' ||
                                      p_calling_sequence;

  -- Logging Infra: Setting up runtime level
  G_CURRENT_RUNTIME_LEVEL := FND_LOG.G_CURRENT_RUNTIME_LEVEL;

  -- Logging Infra: Procedure level
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
      l_log_msg := 'Begin of procedure '|| l_procedure_name;
      FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name||'.begin', l_log_msg);
  END IF;

  -- Bug Fix 5634515
  -- deleting previous unprocessed prepayment history records for invoice
  delete_hist_dists(P_Invoice_ID,
                    l_curr_calling_sequence);

  -- Logging Infra: Procedure level
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
      l_log_msg := 'calling procedure Prepay_Hist_Insert ';
      FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name, l_log_msg);
  END IF;

  /* Bug 4996808. Inserting into the prepayment history table */
  Prepay_Hist_Insert (P_Invoice_ID,
                      l_curr_calling_sequence);


  /* Get the prepayment history header info */
  OPEN Prepay_History(P_Invoice_ID);
  LOOP
    FETCH Prepay_History INTO l_prepay_hist_rec;
    EXIT WHEN Prepay_History%NOTFOUND OR
              Prepay_History%NOTFOUND IS NULL;



    IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
        l_log_msg := 'CUR: Prepay_History: prepay_history_id = '||
                                           l_prepay_hist_rec.prepay_history_id
                     || ' Prepay_Invoice_ID = ' || l_prepay_hist_rec.Prepay_Invoice_ID
                     || ' Invoice_ID = ' ||l_prepay_hist_rec.Invoice_ID;
        FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name, l_log_msg);
    END IF;

    -- 9322009, added the following code to recreate the incorrect dist
    -- links for the upgraded prepayment application events, so the the
    -- prepayment unapplication for the same created in R12 can get successfully
    -- accounted
    --

    IF l_prepay_hist_rec.transaction_type = 'PREPAYMENT UNAPPLIED' AND
       l_prepay_hist_rec.related_prepay_app_event_id IS NOT NULL THEN

      BEGIN
        SELECT APPH.Prepay_History_ID,
               APPH.Accounting_Event_ID,
               APPH.Posted_Flag,
               NVL(APPH.Historical_Flag, 'N') Historical_Flag,
               XAH.upg_batch_id
          INTO l_prepay_hist_id,
               l_accounting_event_id,
               l_posted_flag,
               l_historical_flag,
               l_upg_batch_id
          FROM ap_prepay_history_all APPH,
               xla_ae_headers XAH,
               ap_system_parameters_all ASP
         WHERE APPH.Invoice_ID = P_Invoice_ID
           AND APPH.accounting_event_id = l_prepay_hist_rec.related_prepay_app_event_id
           AND XAH.application_id = 200
           AND XAH.event_id = APPH.accounting_event_id
           AND ASP.org_id = APPH.org_id
           AND ASP.set_of_books_id = XAH.ledger_id;

      EXCEPTION
        WHEN OTHERS THEN
          l_historical_flag := 'N';
      END;


      IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
        l_log_msg := 'Evaluating if the distribution links should be recreated :- '||
                     ' Prepay_History_Id :'||l_prepay_hist_id||
                     ' Accounting_Event_Id : '||l_accounting_event_id||
                     ' Posted_Flag : '||l_posted_flag||
                     ' historical_flag : '||l_historical_flag||
                     ' upg_batch_id : '||l_upg_batch_id;
        FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name, l_log_msg);
      END IF;



      IF (l_historical_Flag = 'Y' AND
          l_posted_flag = 'Y' AND
          l_upg_batch_id IS NOT NULL AND
          l_upg_batch_id <> -9999) THEN

        IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
          l_log_msg := 'Proceeding to call the Upg_Dist_Links_Insert procedure';
          FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name, l_log_msg);
        END IF;

        SAVEPOINT before_reupgrade;
        BEGIN
          Upg_Dist_Links_Insert
                 (P_Invoice_ID,
                  l_prepay_hist_id,
                  l_accounting_event_id,
                  l_curr_calling_sequence);
        EXCEPTION
          WHEN OTHERS THEN
            IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
              l_log_msg := 'Upg_Dist_Links_Insert encountered exception '||SQLERRM;
              FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name, l_log_msg);
            END IF;

            ROLLBACK TO before_reupgrade;
        END;

      END IF;
    END IF;

    /* Get the standard invoice header info */
    OPEN Invoice_Header(P_Invoice_ID);
    FETCH Invoice_Header INTO l_inv_rec;
    CLOSE Invoice_Header;


    IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
        l_log_msg := 'CUR: Invoice_Header: Invoice_ID = '|| l_prepay_hist_rec.invoice_id;
        FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name, l_log_msg);
    END IF;


    /* Get the prepayment invoice header info */
    OPEN Invoice_Header(l_prepay_hist_rec.prepay_invoice_id);
    FETCH Invoice_Header INTO l_prepay_inv_rec;
    CLOSE Invoice_Header;

    IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
        l_log_msg := 'Check the Payment Type Flag on the Payment for Prepay Invoice_id '||
                     l_prepay_hist_rec.prepay_invoice_id;
        FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name, l_log_msg);
    END IF;

    BEGIN

      l_payment_type_flag := 'Q';

      SELECT DISTINCT ac.payment_type_flag
        INTO l_payment_type_flag
        FROM ap_checks_all ac,
             ap_invoice_payments_all aip
       WHERE ac.check_id = aip.check_id
         AND aip.invoice_id = l_prepay_hist_rec.prepay_invoice_id
         AND rownum < 2;

    EXCEPTION
      WHEN OTHERS THEN
        l_payment_type_flag := 'Q';
        IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
          l_log_msg := ' Encountered an Exception '||SQLERRM||
                       ' when Fetching the Payment Type Flag';
          FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name, l_log_msg);
    END IF;

    END;

    IF l_payment_type_flag = 'R' THEN
       l_pay_trx_type := 'REFUND RECORDED';
    ELSE
       l_pay_trx_type := 'PAYMENT CREATED';
    END IF;

    IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
        l_log_msg := 'Obtained l_pay_trx_type as '||l_pay_trx_type;
        FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name, l_log_msg);
    END IF;

    /* Get the payment history info */
    OPEN Payment_History
              (l_prepay_hist_rec.prepay_invoice_id,
               l_pay_trx_type);
    FETCH Payment_History INTO l_pay_hist_rec;
    CLOSE Payment_History;


    IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
        l_log_msg := 'CUR: Payment_History for payment: Payment_History_ID = '||
                                          l_pay_hist_rec.payment_history_id;
        FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name, l_log_msg);
    END IF;


    /* Get the clearing payment history info */
    OPEN Payment_History
              (l_prepay_hist_rec.prepay_invoice_id,
               'PAYMENT CLEARING');
    FETCH Payment_History INTO l_clr_hist_rec;
    CLOSE Payment_History;


    IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
        l_log_msg := 'CUR: Payment_History for clearing: Payment_History_ID = '||
                                          l_clr_hist_rec.payment_history_id;
        FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name, l_log_msg);
    END IF;


    /* Get the prepay distributions for this event */
    OPEN Prepay_Dists(P_Invoice_ID,
                      l_prepay_hist_rec.invoice_line_number,
                      l_prepay_hist_rec.accounting_date,
                      l_prepay_hist_rec.prepay_history_id);
    LOOP

       FETCH Prepay_Dists INTO l_prepay_dist_rec;
       EXIT WHEN Prepay_Dists%NOTFOUND OR
                 Prepay_Dists%NOTFOUND IS NULL;


       IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
           l_log_msg := 'CUR: Prepay_Dists: Invoice_ID = '||l_prepay_dist_rec.invoice_id
                        ||' Invoice_Distribution_ID = '||l_prepay_dist_rec.invoice_distribution_id
                        ||' Prepay_Distribution_ID = '||l_prepay_dist_rec.prepay_distribution_id;
           FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name, l_log_msg);
       END IF;


       IF l_prepay_dist_rec.parent_reversal_id IS NOT NULL THEN

          IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
              l_log_msg := 'CUR: Prepay_Dists: Invoice_Distribution_ID = '
                           ||l_prepay_dist_rec.invoice_distribution_id
                           ||' Parent_Reversal_ID = '||l_prepay_dist_rec.parent_reversal_id;
              FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name, l_log_msg);
          END IF;

          IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
              l_log_msg := 'Calling procedure Prepay_Dist_Reverse for prepay dist: '
                                || l_prepay_dist_rec.invoice_distribution_id;
              FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name, l_log_msg);
          END IF;


          /* Creating prepayment appl dists for unapplication by reversing the prepay appl
             distributions */
          Prepay_Dist_Reverse
            (l_prepay_hist_rec,
             l_prepay_dist_rec.parent_reversal_id,
             NULL,  -- p_xla_event_rec
             NULL,  -- p_inv_reversal_id
             -- Bug 7134020
             NULL,  -- p_inv_dist_id
             l_prepay_dist_rec.invoice_distribution_id,
             l_curr_calling_sequence);


          IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
              l_log_msg := 'Procedure Prepay_Dist_Reverse executed';
              FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name, l_log_msg);
          END IF;

       ELSE

         -- Bug 9492002, added the sql below to check if there are any upgraded
         -- payments available for the invoice in consideration
         --
         -- commenting below for bug10183934
         --SELECT count(*)
         --INTO   l_upg_inv_pmts
         --FROM   AP_Invoice_Payments_All AIP
         --WHERE  Invoice_ID = p_invoice_id
         --AND    EXISTS (SELECT 'Upg Payment'
         --               FROM   AP_Payment_History_All APH,
         --                      AP_System_Parameters_All ASP,
         --                      XLA_AE_Headers XAH
         --               WHERE  APH.Check_ID = AIP.Check_ID
         --               AND    APH.Historical_Flag = 'Y'
         --               AND    APH.Posted_Flag = 'Y'
         --               AND    APH.Org_id = ASP.Org_id
         --               AND    APH.Accounting_Event_ID = XAH.Event_ID
         --               AND    XAH.Ledger_id = ASP.Set_of_Books_ID
         --               AND    XAH.Application_ID = 200
         --               AND    XAH.Accounting_Entry_Status_Code = 'F'
         --               AND    XAH.Upg_Batch_ID <> -9999
         --               AND    XAH.Upg_Batch_ID IS NOT NULL
         --               AND    Rownum = 1);

         -- Bug9492002, checking if there are any upgaded prepay applications
         -- or Unapplication for the Invoice, which are Historical and have
         -- been accounted in 11i
         --
         -- commenting below for bug10183934
         --SELECT count(*)
         --  INTO l_upg_prepay_app
         --  FROM AP_Invoice_Distributions_ALL AID,
         --       XLA_AE_Headers XAH
         -- WHERE AID.accounting_event_id = XAH.Event_ID
         --   AND XAH.Application_ID = 200
         --   AND XAH.Event_type_Code IN ('PREPAYMENT APPLIED', 'PREPAYMENT UNAPPLIED')
         --   AND XAH.Upg_batch_ID IS NOT NULL
         --   AND XAH.Upg_batch_ID <> -9999
         --   AND XAH.Ledger_ID = AID.Set_of_Books_ID
         --   AND XAH.Accounting_Entry_Status_Code = 'F'
         --   AND NVL(AID.Historical_Flag, 'N') = 'Y'
         --   AND AID.Invoice_id = p_Invoice_id
         --   AND rownum = 1;

         -- Bug9492002, addded the below if clause so as to assign
         -- a false status to the final payment check if there are
         -- any upgraded payments or prepayment applications for the
         -- standard invoice in consideration
         --
         -- commenting below condition for bug10183934
         --IF l_upg_inv_pmts = 0 AND l_upg_prepay_app = 0 THEN

          /* Check if the invoice is fully paid */
          --bug 9108925, added the call to Is_Final_Event
            IF AP_Accounting_Pay_Pkg.Is_Final_Payment
                                       (l_inv_rec,
                                        NULL, -- Payment Amount
                                        NULL, -- Discount Amount
                                        l_prepay_dist_rec.amount,
                                        'PAYMENT CREATED',
                                        l_curr_calling_sequence) THEN
               IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
                 l_log_msg := 'Final payment of Invoice_ID '||l_prepay_dist_rec.invoice_id;
               END IF;
               l_final_payment := AP_ACCOUNTING_PAY_PKG.Is_Final_Event
                                  (l_inv_rec,
                                   NULL,      --p_xla_event_rec
                                   l_prepay_dist_rec.invoice_distribution_id,
                                   l_curr_calling_sequence);
            ELSE
               IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
                  l_log_msg := 'Not Final payment of Invoice_ID '||l_prepay_dist_rec.invoice_id;
               END IF;
               l_final_payment := FALSE;
            END IF;
--         ELSE
--            l_final_payment := FALSE;
--         END IF;

         IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
            IF l_final_payment THEN
               l_log_msg := 'Final pay/prepay event for Invoice_ID '||l_prepay_dist_rec.invoice_id;
            ELSE
               l_log_msg := 'Not final pay/prepay event for Invoice_ID '||l_prepay_dist_rec.invoice_id;
            END IF;
            FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name, l_log_msg);
         END IF;

          --8244163 This query exists 3 places in this package pls make sure that
          --you are modifying in all the places
          SELECT SUM(NVL(AID.Amount,0)),
                 SUM(DECODE(aid.line_type_lookup_code, 'AWT', 0, NVL(AID.Amount,0) ) ),
                 SUM(DECODE(aid.line_type_lookup_code, 'AWT', NVL(AID.Amount,0),0 ) )
            INTO G_Total_Dist_amount,
                 G_Total_Inv_amount,
                 G_Total_awt_amount    --Bug9106549
            FROM AP_Invoice_Distributions_All AID
           WHERE AID.Invoice_ID = p_invoice_id
             AND AID.Line_Type_Lookup_Code <> 'PREPAY'
             AND AID.Prepay_Distribution_ID IS NULL
             AND AID.Prepay_Tax_Parent_ID IS NULL -- For tax dists created in R11.5
             AND AID.AWT_Invoice_Payment_ID IS NULL
             AND NVL(AID.Cancellation_Flag,'N') <> 'Y' -- BUG 6513956
             AND NOT EXISTS (SELECT 1                  --bug fix 6909150
                               FROM xla_events
                              WHERE event_id = AID.accounting_event_id
                                                    AND application_id = 200
                                AND event_type_code IN ('INVOICE CANCELLED',
                                                        'CREDIT MEMO CANCELLED',
                                                        'DEBIT MEMO CANCELLED'));

          OPEN Invoice_Dists(p_invoice_id);
          LOOP

            FETCH Invoice_Dists INTO l_inv_dist_rec;
            EXIT WHEN Invoice_Dists%NOTFOUND OR
                      Invoice_Dists%NOTFOUND IS NULL;

            IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
                l_log_msg := 'CUR: Invoice_Dists: Invoice_Distribution_ID = '
                                     ||l_inv_dist_rec.invoice_distribution_id;
                FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name, l_log_msg);
            END IF;


            IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
                l_log_msg := 'Calling procedure Prepay_Dist_Proc for dist: '
                                  || l_inv_dist_rec.invoice_distribution_id;
                FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name, l_log_msg);
            END IF;

            -- BUG # 7688509
            -- condition: historical_flag =Y
            --         and event is 'INVOICE ADJUSTED'
            --         and ap_system_parameter.auto_offset_flag ='N'
            --         and sum of the distributions in the invoice adjustment event is 0

            b_generate_prepay_dist := TRUE;
            IF  l_inv_dist_rec.historical_flag ='Y' THEN
              OPEN c_sum_per_event(l_inv_dist_rec.accounting_event_id);
              FETCH c_sum_per_event into l_sum_per_event, l_dist_count_per_event;

              -- > 0 case is to handled the case that only  1 line in adjustment event and itself amount is 0
              If l_dist_count_per_event > 0 AND l_sum_per_event = 0 THEN
                b_generate_prepay_dist := FALSE;
              END IF;

              CLOSE c_sum_per_event;

            END IF;

            -- Prorate only those awt distributions that were created during the invoice time
            -- modified the if condition for bug # 7688509
            IF l_inv_dist_rec.awt_invoice_payment_id IS NULL  and b_generate_prepay_dist THEN
               Prepay_Dist_Proc
                         (l_pay_hist_rec,
                          l_clr_hist_rec,
                          l_inv_rec,
                          l_prepay_inv_rec,
                          l_prepay_hist_rec,
                          l_prepay_dist_rec,
                          l_inv_dist_rec,
                          NULL,  -- p_xla_event_rec
                          'A',
                          l_final_payment,
                          l_curr_calling_sequence);
            END IF;


            IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
                l_log_msg := 'Procedure Prepay_Dist_Proc executed';
                FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name, l_log_msg);
            END IF;

          END LOOP;
          CLOSE Invoice_Dists;

          G_Total_Dist_Amt := 0;
          G_Total_Prorated_Amt := 0;
          G_Total_Tax_Diff_Amt := 0;


          IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
              l_log_msg := 'Calling procedure P_Acctg_Pay_Round_Pkg.Do_Rounding for Invoice_ID: '
                                    || l_inv_rec.invoice_id;
              FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name, l_log_msg);
          END IF;

          -- bug 7611160
          SELECT asp.base_currency_code
          INTO ap_accounting_pay_pkg.g_base_currency_code
          FROM ap_system_parameters_all asp,
               ap_invoices_all ai
          WHERE asp.org_id = ai.org_id
            AND ai.invoice_id = l_inv_rec.invoice_id;

         -- Bug 9492002. Do not do rounding calculations if the invoice being paid
         -- has an Upgraded Payment or Upgraded prepayment Applications
         --
         -- commenting below condition for bug10183934
         --IF l_upg_inv_pmts = 0 AND l_upg_prepay_app = 0 THEN
          AP_Acctg_Pay_Round_Pkg.Do_Rounding
                     (NULL, -- p_xla_event_rec
                      l_pay_hist_rec,
                      l_clr_hist_rec,
                      l_inv_rec,
                      NULL, -- l_inv_pay_rec
                      l_prepay_inv_rec,
                      l_prepay_hist_rec,
                      l_prepay_dist_rec,
                      l_curr_calling_sequence);

         --END IF;
          IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
              l_log_msg := 'Procedure P_Acctg_Pay_Round_Pkg.Do_Rounding executed';
              FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name, l_log_msg);
          END IF;

       END IF;
    END LOOP;
    CLOSE Prepay_Dists;

  END LOOP;
  CLOSE Prepay_History;

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

END Prepay_Dist_Appl;


-------------------------------------------------------------------------------
-- PROCEDURE  Prepay_Dist_Cascade_Adj
-- The purpose of this procedure is to prorate the prepayment amount for all the
-- distributions of the invoice that has been adjusted and generate the
-- prepayment application payment distributions.
--
--------------------------------------------------------------------------------
PROCEDURE Prepay_Dist_Cascade_Adj
     (P_XLA_Event_Rec      IN   ap_accounting_pay_pkg.r_xla_event_info
     ,P_Calling_Sequence   IN   VARCHAR2
     ) IS

  l_curr_calling_sequence    VARCHAR2(2000);
  l_inv_adj_amount           NUMBER := 0;
  l_sum_prepaid_amount       NUMBER := 0;
  l_sum_tax_diff_amount      NUMBER := 0;

  l_pay_hist_rec           ap_accounting_pay_pkg.r_pay_hist_info;
  l_clr_hist_rec           ap_accounting_pay_pkg.r_pay_hist_info;
  l_prepay_inv_rec         ap_accounting_pay_pkg.r_invoices_info;
  l_inv_rec                ap_accounting_pay_pkg.r_invoices_info;
  l_prepay_hist_rec        r_prepay_hist_info;
  l_prepay_dist_rec        r_prepay_dist_info;
  l_inv_dist_rec           ap_accounting_pay_pkg.r_inv_dist_info;
  l_rounding_adjust_id     NUMBER; --bug8201141
  --7488981
  l_prepay_dist_cnt           NUMBER;

  -- Logging Infra:
  l_procedure_name CONSTANT VARCHAR2(30) := 'Prepay_Dist_Cascade_Adj';
  l_log_msg        FND_LOG_MESSAGES.MESSAGE_TEXT%TYPE;

  -- Bug 6698125. Added adj cursor to get the prepay history record
  -- related to prepayment adjustment type events.
  CURSOR Prepay_History_Adj
        (P_Invoice_ID    NUMBER,
         P_Event_ID      NUMBER
        ) IS
  SELECT APH.Prepay_History_ID,
         APH.Prepay_Invoice_ID,
         APH.Invoice_ID,
         APH.Invoice_Line_Number,
         APH.Transaction_Type,
         APH.Accounting_Date,
         APH.Invoice_Adjustment_Event_ID,
         APH.Related_Prepay_App_Event_ID
  FROM   AP_Prepay_History_All APH
  WHERE  APH.Invoice_ID = P_Invoice_ID
  AND    APH.Accounting_Event_ID = P_Event_ID;

  CURSOR Inv_Adj_Dists
        (P_Event_ID             NUMBER
        ,P_Invoice_ID           NUMBER) IS
  select aid.*
       , count(1) over() dist_count     --Bug 16674834
       , rownum dist_number             --Bug 16674834
    from (SELECT Distinct AID.Invoice_Distribution_ID,
                 AID.Line_Type_Lookup_Code,
                 AID.Amount,
                 AID.Base_Amount,
                 AID.PO_Distribution_ID,
                 AID.RCV_Transaction_ID,
                 NVL(AID.Reversal_Flag,'N'),
                 AID.Parent_Reversal_ID,
                 AID.AWT_Related_ID,
                 AID.AWT_Invoice_Payment_ID,
                 AID.Quantity_Variance,
                 AID.Base_Quantity_Variance,
                 AID.Amount_Variance,
                 AID.Base_Amount_Variance,
                 AID.Historical_Flag,   -- bug fix 6674279
                 AID.Accounting_Event_Id  -- bug fix 6674279
          FROM   AP_Invoice_Distributions_All AID,
                 AP_Prepay_App_Dists APAD,
                 Financials_System_Params_All FSP
          WHERE  AID.Invoice_ID = P_Invoice_ID
          AND    NVL(AID.Reversal_Flag,'N') <> 'Y'
          AND    NVL(AID.Accounting_Event_ID,-99) <> P_Event_ID
          AND    APAD.Invoice_Distribution_ID = AID.Invoice_Distribution_ID
          AND    FSP.Org_ID = AID.Org_ID
          AND  ((NVL(FSP.Purch_Encumbrance_Flag,'N') = 'N'
                     AND AID.Match_Status_Flag IN ('T','A'))
                 OR
               ((NVL(FSP.Purch_Encumbrance_Flag,'N') = 'Y'
                     AND AID.Match_Status_Flag = 'A')))) aid;

  CURSOR Prepay_Adj_Dists
        (P_Event_ID             NUMBER,
         P_Prepay_History_ID    NUMBER
        ) IS
 (SELECT AID.Invoice_ID,
         AID.Invoice_Distribution_ID,
         AID.Line_Type_Lookup_Code,
         AID.Amount,
         AID.Base_Amount,
         AID.Accounting_Event_ID,
         AID.Prepay_Distribution_ID,
         AID.Prepay_Tax_Diff_Amount,
         AID.Parent_Reversal_ID
  FROM   AP_Invoice_Distributions_All AID
  WHERE  Accounting_Event_ID = P_Event_ID
  AND    Line_Type_Lookup_Code =  'PREPAY'   -- bug 17693931
  AND    EXISTS (SELECT 'Prepay History'
                 FROM   AP_Prepay_History_All APH,
                        AP_Invoice_Distributions_All AID1
                 WHERE  APH.Prepay_History_ID = P_Prepay_History_ID
                 AND    AID1.Invoice_Distribution_ID = AID.Prepay_Distribution_ID
                 AND    AID1.Invoice_ID = APH.Prepay_Invoice_ID
                 AND    AID1.Invoice_Line_Number = APH.Prepay_Line_Num)
  UNION ALL
  SELECT AID.Invoice_ID,
         AID.Invoice_Distribution_ID,
         AID.Line_Type_Lookup_Code,
         AID.Amount,
         AID.Base_Amount,
         AID.Accounting_Event_ID,
         AID.Prepay_Distribution_ID,
         AID.Prepay_Tax_Diff_Amount,
         AID.Parent_Reversal_ID
  FROM   AP_Invoice_Distributions_All AID
  WHERE  Line_Type_Lookup_Code IN ( 'NONREC_TAX','REC_TAX')
  AND    Accounting_Event_ID = P_Event_ID
  AND    Charge_Applicable_To_Dist_ID IN
               (SELECT AID1.Invoice_Distribution_ID
                FROM   AP_Invoice_Distributions_All AID1
                WHERE  Line_Type_Lookup_Code = 'PREPAY'
                AND    Accounting_Event_ID = P_Event_ID
                AND    EXISTS (SELECT 'Prepay History'
                               FROM   AP_Prepay_History_All APH,
                                      AP_Invoice_Distributions_All AID2
                               WHERE  APH.Prepay_History_ID = P_Prepay_History_ID
                               AND    AID2.Invoice_Distribution_ID = AID1.Prepay_Distribution_ID
                               AND    AID2.Invoice_ID = APH.Prepay_Invoice_ID
                               AND    AID2.Invoice_Line_Number = APH.Prepay_Line_Num)));



BEGIN

  l_curr_calling_sequence := 'AP_Acctg_Pay_Dist_Pkg.Prepay_Dist_Cascade_Adj<- ' ||
                                      p_calling_sequence;

  -- Logging Infra: Procedure level
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
      l_log_msg := 'Begin of procedure '|| l_procedure_name;
      FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name||'.begin', l_log_msg);
  END IF;


  /* Get the prepayment history header info */
  OPEN Prepay_History_Adj(P_XLA_Event_Rec.Source_id_int_1,
                          P_XLA_Event_Rec.Event_ID);
  FETCH Prepay_History_Adj INTO l_prepay_hist_rec;
  CLOSE Prepay_History_Adj;

  IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
      l_log_msg := 'CUR: Prepay_History: prepay_history_id = '||
                                         l_prepay_hist_rec.prepay_history_id
                   || ' Prepay_Invoice_ID = ' || l_prepay_hist_rec.Prepay_Invoice_ID
                   || ' Invoice_ID = ' ||l_prepay_hist_rec.Invoice_ID
                   || ' Related_Event_ID = ' ||l_prepay_hist_rec.related_prepay_app_event_id
                   || ' Inv_Adj_Event_ID = ' ||l_prepay_hist_rec.invoice_adjustment_event_id;
      FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name, l_log_msg);
  END IF;


  /* Get the standard invoice header info */
  OPEN Invoice_Header(P_XLA_Event_Rec.source_id_int_1);
  FETCH Invoice_Header INTO l_inv_rec;
  CLOSE Invoice_Header;


  IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
      l_log_msg := 'CUR: Invoice_Header: Invoice_ID = '|| l_prepay_hist_rec.invoice_id;
      FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name, l_log_msg);
  END IF;


  /* Get the prepayment invoice header info */
  OPEN Invoice_Header(l_prepay_hist_rec.prepay_invoice_id);
  FETCH Invoice_Header INTO l_prepay_inv_rec;
  CLOSE Invoice_Header;

  IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
      l_log_msg := 'CUR: Prepay Invoice_Header: Invoice_ID = '|| l_prepay_inv_rec.invoice_id;
      FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name, l_log_msg);
  END IF;


  /* Get the payment history info */
  OPEN Payment_History
              (l_prepay_hist_rec.prepay_invoice_id,
               'PAYMENT CREATED');
  FETCH Payment_History INTO l_pay_hist_rec;
  CLOSE Payment_History;


  IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
      l_log_msg := 'CUR: Payment_History for payment: Payment_History_ID = '||
                                          l_pay_hist_rec.payment_history_id;
      FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name, l_log_msg);
  END IF;


  /* Get the clearing payment history info */
  OPEN Payment_History
              (l_prepay_hist_rec.prepay_invoice_id,
               'PAYMENT CLEARING');
  FETCH Payment_History INTO l_clr_hist_rec;
  CLOSE Payment_History;


  IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
      l_log_msg := 'CUR: Payment_History for clearing: Payment_History_ID = '||
                                          l_clr_hist_rec.payment_history_id;
      FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name, l_log_msg);
  END IF;


  /* Get the prepay dists based on the related event id */
  OPEN Prepay_Adj_Dists(l_prepay_hist_rec.related_prepay_app_event_id,
                        l_prepay_hist_rec.prepay_history_id);
  LOOP

       FETCH Prepay_Adj_Dists INTO l_prepay_dist_rec;
       EXIT WHEN Prepay_Adj_Dists%NOTFOUND OR
                 Prepay_Adj_Dists%NOTFOUND IS NULL;


       IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
           l_log_msg := 'CUR: Prepay_Dists: Invoice_ID = '||l_prepay_dist_rec.invoice_id
                        ||' Invoice_Distribution_ID = '||l_prepay_dist_rec.invoice_distribution_id
                        ||' Prepay_Distribution_ID = '||l_prepay_dist_rec.prepay_distribution_id;
           FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name, l_log_msg);
       END IF;
       --8244163 This query exists 3 places in this package pls make sure that
       --you are modifying in all the places
       SELECT SUM(NVL(AID.Amount,0)),
              SUM(DECODE(aid.line_type_lookup_code, 'AWT', 0, NVL(AID.Amount,0) ) )
         INTO G_Total_Dist_amount,
              G_Total_Inv_amount
         FROM AP_Invoice_Distributions_All AID
        WHERE AID.Invoice_ID = l_prepay_hist_rec.invoice_id
          AND AID.Line_Type_Lookup_Code <> 'PREPAY'
          AND AID.Prepay_Distribution_ID IS NULL
          AND AID.Prepay_Tax_Parent_ID IS NULL -- For tax dists created in R11.5
          AND AID.AWT_Invoice_Payment_ID IS NULL
          AND NVL(AID.Cancellation_Flag,'N') <> 'Y' -- BUG 6513956
          AND NOT EXISTS (SELECT 1                  --bug fix 6909150
                            FROM xla_events
                           WHERE event_id = AID.accounting_event_id
                                         AND application_id = 200
                             AND event_type_code IN ('INVOICE CANCELLED',
                                                     'CREDIT MEMO CANCELLED',
                                                     'DEBIT MEMO CANCELLED'));

       OPEN Invoice_Dists(l_prepay_hist_rec.invoice_id,
                          l_prepay_hist_rec.invoice_adjustment_event_id);
       LOOP

            FETCH Invoice_Dists INTO l_inv_dist_rec;
            EXIT WHEN Invoice_Dists%NOTFOUND OR
                      Invoice_Dists%NOTFOUND IS NULL;


            IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
                l_log_msg := 'CUR: Invoice_Dists: Invoice_Distribution_ID = '
                                     ||l_inv_dist_rec.invoice_distribution_id
                                || ' Reversal_Flag = ' ||l_inv_dist_rec.reversal_flag;
                FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name, l_log_msg);
            END IF;

            -- in bug 7488981 call to prepay_dist_reverse was made  with null parent_reversal_id
            -- therefore the following check is added to check that
            --
            l_prepay_dist_cnt := 0; --7686421
            IF l_inv_dist_rec.parent_reversal_id IS NOT NULL THEN

               SELECT count(*)
               INTO   l_prepay_dist_cnt
               FROM   ap_prepay_app_dists
               WHERE  invoice_distribution_id = l_inv_dist_rec.parent_reversal_id;

            END IF;

            IF l_inv_dist_rec.reversal_flag = 'Y' AND
               l_prepay_dist_cnt > 0 THEN


               IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
                   l_log_msg := 'Calling procedure Prepay_Dist_Reverse for dist: '
                                     || l_inv_dist_rec.invoice_distribution_id;
                   FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name, l_log_msg);
               END IF;

               Prepay_Dist_Reverse
                         (l_prepay_hist_rec,
                          NULL, -- p_prepay_reversal_id
                          p_xla_event_rec, -- Bug 6698125
                          l_inv_dist_rec.parent_reversal_id,
                          l_inv_dist_rec.invoice_distribution_id, -- Bug 7134020
                          l_prepay_dist_rec.invoice_distribution_id,
                          l_curr_calling_sequence);

               IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
                   l_log_msg := 'Procedure Prepay_Dist_Reverse executed';
                   FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name, l_log_msg);
               END IF;

            ELSE

               IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
                   l_log_msg := 'Calling procedure Prepay_Dist_Proc for dist: '
                                     || l_inv_dist_rec.invoice_distribution_id;
                   FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name, l_log_msg);
               END IF;

               -- Prorate only those awt distributions that were created during the invoice time
               IF (l_inv_dist_rec.awt_invoice_payment_id IS NULL) THEN
                   Prepay_Dist_Proc
                         (l_pay_hist_rec,
                          l_clr_hist_rec,
                          l_inv_rec,
                          l_prepay_inv_rec,
                          l_prepay_hist_rec,
                          l_prepay_dist_rec,
                          l_inv_dist_rec,
                          p_xla_event_rec, -- Bug 6698125
                          'C',
                          NULL,
                          l_curr_calling_sequence);
               END IF;

               IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
                   l_log_msg := 'Procedure Prepay_Dist_Proc executed';
                   FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name, l_log_msg);
               END IF;

            END IF;

       END LOOP;
       CLOSE Invoice_Dists;


       SELECT SUM(AID.Amount)
       INTO   l_inv_adj_amount
       FROM   AP_Invoice_Distributions_All AID
       WHERE  AID.Accounting_Event_ID = l_prepay_hist_rec.invoice_adjustment_event_id;

       IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
           l_log_msg := 'l_inv_adj_amount = ' ||l_inv_adj_amount;
           FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name, l_log_msg);
       END IF;

      /* Check if there is any change to the invoice liability. If there is
          a change then we need to adjust the payment hist distributions for the
          old invoice distributions */


       IF l_inv_adj_amount <> 0 THEN

       --8244163 This query exists 3 places in this package pls make sure that
       --you are modifying in all the places
       SELECT SUM(NVL(AID.Amount,0)),
              SUM(DECODE(aid.line_type_lookup_code, 'AWT', 0, NVL(AID.Amount,0) ) )
         INTO G_Total_Dist_amount,
              G_Total_Inv_amount
         FROM AP_Invoice_Distributions_All AID
        WHERE AID.Invoice_ID = l_inv_rec.invoice_id
          AND AID.Line_Type_Lookup_Code <> 'PREPAY'
          AND AID.Prepay_Distribution_ID IS NULL
          AND AID.Prepay_Tax_Parent_ID IS NULL -- For tax dists created in R11.5
          AND AID.AWT_Invoice_Payment_ID IS NULL
          AND NVL(AID.Cancellation_Flag,'N') <> 'Y' -- BUG 6513956
          AND NOT EXISTS (SELECT 1                  --bug fix 6909150
                            FROM xla_events
                           WHERE event_id = AID.accounting_event_id
                                         AND application_id = 200
                             AND event_type_code IN ('INVOICE CANCELLED',
                                                     'CREDIT MEMO CANCELLED',
                                                     'DEBIT MEMO CANCELLED'));

          OPEN Inv_Adj_Dists(l_prepay_hist_rec.invoice_adjustment_event_id,
                             l_inv_rec.invoice_id);
          LOOP

               FETCH Inv_Adj_Dists INTO l_inv_dist_rec;
               EXIT WHEN Inv_Adj_Dists%NOTFOUND OR
                         Inv_Adj_Dists%NOTFOUND IS NULL;


               IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
                   l_log_msg := 'CUR: Inv_Adj_Dists: Invoice_Distribution_ID = '
                                   ||l_inv_dist_rec.invoice_distribution_id;
                   FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name, l_log_msg);
               END IF;

               IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
                   l_log_msg := 'Calling procedure Prepay_Dist_Proc for dist: '
                                    ||l_inv_dist_rec.invoice_distribution_id;
                   FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name, l_log_msg);
               END IF;

               Prepay_Dist_Proc(l_pay_hist_rec,
                                l_clr_hist_rec,
                                l_inv_rec,
                                l_prepay_inv_rec,
                                l_prepay_hist_rec,
                                l_prepay_dist_rec,
                                l_inv_dist_rec,
                                p_xla_event_rec, -- Bug 6698125
                                'C',
                                NULL,
                                l_curr_calling_sequence);

               IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
                   l_log_msg := 'Procedure Prepay_Dist_Proc executed';
                   FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name, l_log_msg);
               END IF;


          END LOOP;
          CLOSE Inv_Adj_Dists;
       END IF;

       SELECT MAX(accounting_event_id) into l_rounding_adjust_id   --8201141
         FROM ap_prepay_history_all apph
        WHERE transaction_type = 'PREPAYMENT APPLICATION ADJ'
          AND posted_flag <> 'Y'
          AND prepay_invoice_id = l_prepay_hist_rec.prepay_invoice_id
          AND invoice_id = l_prepay_hist_rec.invoice_id
          /* bug12858105 - start */
          AND EXISTS (SELECT 1
                        FROM AP_Prepay_App_Dists APAD2
                       WHERE 1=1
                         AND APAD2.Prepay_App_Distribution_ID = l_prepay_dist_rec.invoice_distribution_id
                         AND APAD2.Prepay_Dist_Lookup_Code IN ('PREPAY APPL', 
                                                               'PREPAY APPL REC TAX',
                                                               'PREPAY APPL NONREC TAX')
                         AND APAD2.Reversed_Prepay_App_Dist_Id IS NULL
                         AND NOT EXISTS (SELECT 1
                                           FROM ap_prepay_app_dists apad2_rev,
                                                ap_prepay_history_all apph_rev      
                                          WHERE apad2_rev.reversed_prepay_app_dist_id = APAD2.prepay_app_dist_id
                                            AND apad2_rev.prepay_history_id = apph_rev.prepay_history_id
                                            AND apph_rev.invoice_id = apph.invoice_id)
                      )
          /* bug12858105 - end */
          ;

       IF ( l_rounding_adjust_id = p_xla_event_rec.event_id ) THEN

        -- joined with ap_invoice_distributions_all for the performance issue 7235352
       SELECT /*+ leading(aid) */ SUM(DECODE(APAD.Prepay_Dist_Lookup_Code, 'PREPAY APPL', APAD.Amount,
                                  'PREPAY APPL REC TAX', APAD.Amount,
                                  'PREPAY APPL NONREC TAX', APAD.Amount,  0)),
              SUM(DECODE(APAD.Prepay_Dist_Lookup_Code, 'TAX DIFF', APAD.Amount, 0))
       INTO   l_sum_prepaid_amount,
              l_sum_tax_diff_amount
       FROM   AP_Prepay_App_Dists APAD,
              ap_invoice_distributions_all aid
       WHERE  APAD.Prepay_App_Distribution_ID = l_prepay_dist_rec.invoice_distribution_id
              AND apad.invoice_distribution_id = aid.invoice_distribution_id
              AND aid.invoice_id = l_prepay_dist_rec.invoice_id;


       IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
           l_log_msg := 'Updating the prorated prepaid amounts';
           FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name, l_log_msg);
       END IF;        
      
       IF NVL(l_sum_prepaid_amount, 0) <> l_prepay_dist_rec.amount THEN -- added for bug12858105
      
         -- bug 9240725
      	 IF(l_inv_rec.invoice_currency_code=ap_accounting_pay_pkg.g_base_currency_code) THEN

            UPDATE  AP_Prepay_App_Dists APAD
               SET  APAD.Amount = APAD.Amount -  NVL(l_sum_prepaid_amount,0) + l_prepay_dist_rec.amount,
                    APAD.BASE_AMT_AT_PREPAY_XRATE = APAD.BASE_AMT_AT_PREPAY_XRATE - NVL(l_sum_prepaid_amount,0) + l_prepay_dist_rec.amount,
                    APAD.BASE_AMT_AT_PREPAY_PAY_XRATE=APAD.BASE_AMT_AT_PREPAY_PAY_XRATE - NVL(l_sum_prepaid_amount,0) + l_prepay_dist_rec.amount,
                    APAD.BASE_AMOUNT=APAD.BASE_AMOUNT - NVL(l_sum_prepaid_amount,0) + l_prepay_dist_rec.amount,
                    APAD.BASE_AMT_AT_PREPAY_CLR_XRATE=APAD.BASE_AMT_AT_PREPAY_CLR_XRATE - NVL(l_sum_prepaid_amount,0) + l_prepay_dist_rec.amount, rounding_amt = nvl(l_sum_prepaid_amount, 0) + l_prepay_dist_rec.amount
             WHERE  APAD.Invoice_Distribution_ID =
                      (SELECT MAX(APAD1.Invoice_Distribution_ID)
                         FROM AP_Prepay_App_Dists APAD1
                        WHERE APAD1.Accounting_Event_ID = p_xla_event_rec.event_id
                          AND APAD1.Prepay_App_Distribution_ID = l_prepay_dist_rec.invoice_distribution_id
                          AND APAD1.Prepay_Dist_Lookup_Code IN ('PREPAY APPL', 'PREPAY APPL REC TAX',
                                                                'PREPAY APPL NONREC TAX')
                          AND ABS(APAD1.Amount) =
                                    (SELECT MAX(ABS(APAD2.Amount)) -- added ABS for bug12858105
                                       FROM AP_Prepay_App_Dists APAD2
                                      WHERE APAD2.Accounting_Event_ID = p_xla_event_rec.event_id
                                        AND APAD2.Prepay_App_Distribution_ID
                                                    = l_prepay_dist_rec.invoice_distribution_id
                                        AND APAD2.Prepay_Dist_Lookup_Code IN ('PREPAY APPL', 'PREPAY APPL REC TAX',
                                                                              'PREPAY APPL NONREC TAX')
                                        /* bug12858105 - start */
                                        AND APAD2.Reversed_Prepay_App_Dist_Id IS NULL
                                        AND NOT EXISTS (SELECT 1
                                                          FROM ap_prepay_app_dists apad2_rev,
                                                               ap_prepay_history_all apph      
                                                         WHERE apad2_rev.reversed_prepay_app_dist_id = APAD2.prepay_app_dist_id
                                                           AND apad2_rev.prepay_history_id = apph.prepay_history_id
                                                           AND apph.invoice_id = p_xla_event_rec.source_id_int_1)
                                       /* bug12858105 - end */
                                     )
                          /* bug12858105 - start */
                          AND APAD1.Reversed_Prepay_App_Dist_Id IS NULL
                          AND NOT EXISTS (SELECT 1
                                            FROM ap_prepay_app_dists apad1_rev,
                                                 ap_prepay_history_all apph      
                                           WHERE apad1_rev.reversed_prepay_app_dist_id = APAD1.prepay_app_dist_id
                                             AND apad1_rev.prepay_history_id = apph.prepay_history_id
                                             AND apph.invoice_id = p_xla_event_rec.source_id_int_1)
                          /* bug12858105 - end */
                        )
               AND  APAD.Prepay_Dist_Lookup_Code IN ('PREPAY APPL', 'PREPAY APPL REC TAX',
                                                     'PREPAY APPL NONREC TAX')
               AND  APAD.Accounting_Event_ID = p_xla_event_rec.event_id
               AND  APAD.Prepay_App_Distribution_ID = l_prepay_dist_rec.invoice_distribution_id;


       ELSE

         /* Updating the prorated prepaid amounts for any rounding */
         UPDATE AP_Prepay_App_Dists APAD
         SET    APAD.Amount = APAD.Amount -  NVL(l_sum_prepaid_amount,0) + l_prepay_dist_rec.amount
         WHERE  APAD.Invoice_Distribution_ID =
             (SELECT MAX(APAD1.Invoice_Distribution_ID)
              FROM   AP_Prepay_App_Dists APAD1
              WHERE  APAD1.Accounting_Event_ID = p_xla_event_rec.event_id
              AND    APAD1.Prepay_App_Distribution_ID = l_prepay_dist_rec.invoice_distribution_id
              AND    APAD1.Prepay_Dist_Lookup_Code IN ('PREPAY APPL', 'PREPAY APPL REC TAX',
                                                       'PREPAY APPL NONREC TAX')
              AND    ABS(APAD1.Amount) =
                    (SELECT MAX(ABS(APAD2.Amount)) -- adding ABS for bug12858105
                     FROM   AP_Prepay_App_Dists APAD2
                     WHERE  APAD2.Accounting_Event_ID = p_xla_event_rec.event_id
                     AND    APAD2.Prepay_App_Distribution_ID
                                              = l_prepay_dist_rec.invoice_distribution_id
                     AND    APAD2.Prepay_Dist_Lookup_Code IN ('PREPAY APPL', 'PREPAY APPL REC TAX',
                                                              'PREPAY APPL NONREC TAX')
                     /* bug12858105 - start */
                     AND APAD2.Reversed_Prepay_App_Dist_Id IS NULL
                     AND NOT EXISTS (SELECT 1
                                       FROM ap_prepay_app_dists apad2_rev,
                                            ap_prepay_history_all apph      
                                      WHERE apad2_rev.reversed_prepay_app_dist_id = APAD2.prepay_app_dist_id
                                        AND apad2_rev.prepay_history_id = apph.prepay_history_id
                                        AND apph.invoice_id = p_xla_event_rec.source_id_int_1)
                     /* bug12858105 - end */
                     )
              /* bug12858105 - start */
              AND APAD1.Reversed_Prepay_App_Dist_Id IS NULL
              AND NOT EXISTS (SELECT 1
                                FROM ap_prepay_app_dists apad1_rev,
                                     ap_prepay_history_all apph      
                               WHERE apad1_rev.reversed_prepay_app_dist_id = APAD1.prepay_app_dist_id
                                 AND apad1_rev.prepay_history_id = apph.prepay_history_id
                                 AND apph.invoice_id = p_xla_event_rec.source_id_int_1)
              /* bug12858105 - end */
              )
          AND    APAD.Prepay_Dist_Lookup_Code IN ('PREPAY APPL', 'PREPAY APPL REC TAX',
                                               'PREPAY APPL NONREC TAX')
          AND    APAD.Accounting_Event_ID = p_xla_event_rec.event_id
          AND    APAD.Prepay_App_Distribution_ID = l_prepay_dist_rec.invoice_distribution_id;

       END IF;
   
     END IF;-- NVL(l_sum_prepaid_amount, 0) <> l_prepay_dist_rec.amount
     
     IF l_prepay_dist_rec.prepay_tax_diff_amount <> 0 
        AND NVL(l_sum_tax_diff_amount,0) <> l_prepay_dist_rec.prepay_tax_diff_amount THEN -- added for bug12858105

       IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
           l_log_msg := 'Updating the prorated tax diff amounts';
           FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name, l_log_msg);
       END IF;

      
          /* Updating the prorated tax diff amounts for any rounding */     
          /* -- commented for bug 12858105
           UPDATE AP_Prepay_App_Dists APAD
           SET    APAD.Amount = APAD.Amount -  NVL(l_sum_tax_diff_amount,0) + l_prepay_dist_rec.prepay_tax_diff_amount
           WHERE  APAD.Invoice_Distribution_ID =
                 (SELECT MAX(APAD1.Invoice_Distribution_ID)
                  FROM   AP_Prepay_App_Dists APAD1
                  WHERE  APAD1.Accounting_Event_ID = p_xla_event_rec.event_id
                  AND    APAD1.Prepay_App_Distribution_ID = l_prepay_dist_rec.invoice_distribution_id
                  AND    APAD1.Prepay_Dist_Lookup_Code = 'TAX DIFF'
                  AND    ABS(APAD1.Amount) =
                        (SELECT MAX(APAD2.Amount)
                         FROM   AP_Prepay_App_Dists APAD2
                         WHERE  APAD2.Accounting_Event_ID = p_xla_event_rec.event_id
                         AND    APAD2.Prepay_App_Distribution_ID = l_prepay_dist_rec.invoice_distribution_id
                         AND    APAD2.Prepay_Dist_Lookup_Code = 'TAX DIFF'))
           AND    APAD.Prepay_Dist_Lookup_Code = 'TAX DIFF'
           AND    APAD.Prepay_App_Distribution_ID = l_prepay_dist_rec.invoice_distribution_id
           AND    APAD.Accounting_Event_ID = p_xla_event_rec.event_id; */
           
       /* bug12858105 - start */
       IF(l_inv_rec.invoice_currency_code=ap_accounting_pay_pkg.g_base_currency_code) THEN

            UPDATE  AP_Prepay_App_Dists APAD
               SET  APAD.Amount = APAD.Amount-  NVL(l_sum_tax_diff_amount,0) + l_prepay_dist_rec.prepay_tax_diff_amount ,
                    APAD.BASE_AMT_AT_PREPAY_XRATE = APAD.BASE_AMT_AT_PREPAY_XRATE
                                                    -  NVL(l_sum_tax_diff_amount,0) + l_prepay_dist_rec.prepay_tax_diff_amount,
                    APAD.BASE_AMT_AT_PREPAY_PAY_XRATE=APAD.BASE_AMT_AT_PREPAY_PAY_XRATE
                                                      -  NVL(l_sum_tax_diff_amount,0) + l_prepay_dist_rec.prepay_tax_diff_amount,
                    APAD.BASE_AMOUNT=APAD.BASE_AMOUNT
                                     -  NVL(l_sum_tax_diff_amount,0) + l_prepay_dist_rec.prepay_tax_diff_amount,
                    APAD.BASE_AMT_AT_PREPAY_CLR_XRATE=APAD.BASE_AMT_AT_PREPAY_CLR_XRATE
                                                      - NVL(l_sum_tax_diff_amount,0) + l_prepay_dist_rec.prepay_tax_diff_amount
             WHERE  APAD.Invoice_Distribution_ID =
                      (SELECT MAX(APAD1.Invoice_Distribution_ID)
                         FROM AP_Prepay_App_Dists APAD1
                        WHERE APAD1.Accounting_Event_ID = p_xla_event_rec.event_id
                          AND APAD1.Prepay_App_Distribution_ID = l_prepay_dist_rec.invoice_distribution_id
                          AND APAD1.Prepay_Dist_Lookup_Code IN ('TAX DIFF')
                          AND ABS(APAD1.Amount) =
                                    (SELECT MAX(ABS(APAD2.Amount))
                                       FROM AP_Prepay_App_Dists APAD2
                                      WHERE APAD2.Accounting_Event_ID = p_xla_event_rec.event_id
                                        AND APAD2.Prepay_App_Distribution_ID
                                                    = l_prepay_dist_rec.invoice_distribution_id
                                        AND APAD2.Prepay_Dist_Lookup_Code IN ('TAX DIFF')
                                        AND APAD2.Reversed_Prepay_App_Dist_Id IS NULL
                                        AND NOT EXISTS (SELECT 1
                                                          FROM ap_prepay_app_dists apad2_rev,
                                                               ap_prepay_history_all apph      
                                                         WHERE apad2_rev.reversed_prepay_app_dist_id = APAD2.prepay_app_dist_id
                                                           AND apad2_rev.prepay_history_id = apph.prepay_history_id
                                                           AND apph.invoice_id = p_xla_event_rec.source_id_int_1)
                                           )
                          AND APAD1.Reversed_Prepay_App_Dist_Id IS NULL
                          AND NOT EXISTS (SELECT 1
                                            FROM ap_prepay_app_dists apad1_rev,
                                                 ap_prepay_history_all apph      
                                           WHERE apad1_rev.reversed_prepay_app_dist_id = APAD1.prepay_app_dist_id
                                             AND apad1_rev.prepay_history_id = apph.prepay_history_id
                                             AND apph.invoice_id = p_xla_event_rec.source_id_int_1)
                           )
               AND  APAD.Prepay_Dist_Lookup_Code IN ('TAX DIFF')
               AND  APAD.Accounting_Event_ID = p_xla_event_rec.event_id
               AND  APAD.Prepay_App_Distribution_ID = l_prepay_dist_rec.invoice_distribution_id;


       ELSE

         /* Updating the prorated prepaid amounts for any rounding */
         UPDATE AP_Prepay_App_Dists APAD
         SET    APAD.Amount = APAD.Amount-  NVL(l_sum_tax_diff_amount,0) + l_prepay_dist_rec.prepay_tax_diff_amount
         WHERE  APAD.Invoice_Distribution_ID =
             (SELECT MAX(APAD1.Invoice_Distribution_ID)
              FROM   AP_Prepay_App_Dists APAD1
              WHERE  APAD1.Accounting_Event_ID = p_xla_event_rec.event_id
              AND    APAD1.Prepay_App_Distribution_ID = l_prepay_dist_rec.invoice_distribution_id
              AND    APAD1.Prepay_Dist_Lookup_Code IN ('TAX DIFF')
              AND    ABS(APAD1.Amount) =
                    (SELECT MAX(ABS(APAD2.Amount))
                     FROM   AP_Prepay_App_Dists APAD2
                     WHERE  APAD2.Accounting_Event_ID = p_xla_event_rec.event_id
                     AND    APAD2.Prepay_App_Distribution_ID
                                              = l_prepay_dist_rec.invoice_distribution_id
                     AND    APAD2.Prepay_Dist_Lookup_Code IN ('TAX DIFF')
                     AND APAD2.Reversed_Prepay_App_Dist_Id IS NULL
                     AND NOT EXISTS (SELECT 1
                                       FROM ap_prepay_app_dists apad2_rev,
                                            ap_prepay_history_all apph      
                                      WHERE apad2_rev.reversed_prepay_app_dist_id = APAD2.prepay_app_dist_id
                                        AND apad2_rev.prepay_history_id = apph.prepay_history_id
                                        AND apph.invoice_id = p_xla_event_rec.source_id_int_1)
                                      )
                     AND APAD1.Reversed_Prepay_App_Dist_Id IS NULL
              AND NOT EXISTS (SELECT 1
                                FROM ap_prepay_app_dists apad1_rev,
                                     ap_prepay_history_all apph      
                               WHERE apad1_rev.reversed_prepay_app_dist_id = APAD1.prepay_app_dist_id
                                 AND apad1_rev.prepay_history_id = apph.prepay_history_id
                                 AND apph.invoice_id = p_xla_event_rec.source_id_int_1)
               )
          AND    APAD.Prepay_Dist_Lookup_Code IN ('TAX DIFF')
          AND    APAD.Accounting_Event_ID = p_xla_event_rec.event_id
          AND    APAD.Prepay_App_Distribution_ID = l_prepay_dist_rec.invoice_distribution_id;

       END IF;
       /* bug12858105 - end */
    
     END IF; -- NVL(l_sum_tax_diff_amount, 0) <> l_prepay_dist_rec.prepay_tax_diff_amount
     
     
       IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
           l_log_msg := 'Calling procedure P_Acctg_Pay_Round_Pkg.Do_Rounding for Invoice_ID: '
                                    || l_inv_rec.invoice_id;
           FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name, l_log_msg);
       END IF;

       AP_Acctg_Pay_Round_Pkg.Do_Rounding
                     (NULL, --p_xla_event_rec,
                      l_pay_hist_rec,
                      l_clr_hist_rec,
                      l_inv_rec,
                      NULL, -- l_inv_pay_rec
                      l_prepay_inv_rec,
                      l_prepay_hist_rec,
                      l_prepay_dist_rec,
                      l_curr_calling_sequence);

       IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
           l_log_msg := 'Procedure P_Acctg_Pay_Round_Pkg.Do_Rounding executed';
           FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name, l_log_msg);
       END IF;

     END IF; --l_rounding_adjust_id = p_xla_event_rec.event_id  8201141

  END LOOP;
  CLOSE Prepay_Adj_Dists;

  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
      l_log_msg := 'Calling procedure AP_Acctg_Prepay_Dist_Pkg.Update_Gain_Loss_Ind';
      FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME || l_procedure_name, l_log_msg);
  END IF;

  AP_Acctg_Prepay_Dist_Pkg.Update_Gain_Loss_Ind
              (p_xla_event_rec,
               l_curr_calling_sequence);

  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
      l_log_msg := 'Procedure AP_Acctg_Prepay_Dist_Pkg.Updated_Gain_Loss_Ind executed';
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

END Prepay_Dist_Cascade_Adj;



---------------------------------------------------------------------
-- Procedure Prepay_Dist_Proc
-- This procedure prorates the prepayment application amounts for each
-- distribution and inserts the calculated values into prepayment
-- application distribution table
-- Also calculates ERV
---------------------------------------------------------------------
-- Bug 6698125. Added p_xla_event_rec parameter
PROCEDURE Prepay_Dist_Proc
      (p_pay_hist_rec       IN    ap_accounting_pay_pkg.r_pay_hist_info
      ,p_clr_hist_rec       IN    ap_accounting_pay_pkg.r_pay_hist_info
      ,p_inv_rec            IN    ap_accounting_pay_pkg.r_invoices_info
      ,p_prepay_inv_rec     IN    ap_accounting_pay_pkg.r_invoices_info
      ,p_prepay_hist_rec    IN    r_prepay_hist_info
      ,p_prepay_dist_rec    IN    r_prepay_dist_info
      ,p_inv_dist_rec       IN    ap_accounting_pay_pkg.r_inv_dist_info
      ,p_xla_event_rec      IN    ap_accounting_pay_pkg.r_xla_event_info
      ,p_calc_mode          IN    VARCHAR2
      ,p_final_payment      IN    BOOLEAN
      ,p_calling_sequence   IN    VARCHAR2
      ) IS


  l_curr_calling_sequence       VARCHAR2(2000);
  l_dist_amount                 NUMBER;
  l_prorated_amount             NUMBER;
  l_prorated_base_amount        NUMBER;
  l_inv_dist_amount             NUMBER;
  l_prorated_pay_amt            NUMBER;
  l_prorated_clr_amt            NUMBER;
  l_total_paid_amt              NUMBER;
  l_total_prepaid_amt           NUMBER;
  l_total_inv_dist_amt          NUMBER;
  l_total_bank_curr_amt         NUMBER;
  l_total_dist_amount           NUMBER;
  l_qty_variance                NUMBER;
  l_base_qty_variance           NUMBER;
  l_amt_variance                NUMBER;
  l_base_amt_variance           NUMBER;
  --l_awt_prorated_amt            NUMBER; --8364229 --commenting for bug8882706
  l_pad_rec                     AP_PREPAY_APP_DISTS%ROWTYPE;

  -- Logging Infra:
  l_procedure_name CONSTANT VARCHAR2(30) := 'Prepay_Dist_Proc';
  l_log_msg        FND_LOG_MESSAGES.MESSAGE_TEXT%TYPE;
  l_total_inv_amount            NUMBER; --Bug9106549
  l_total_awt_amount            NUMBER; --Bug9106549


BEGIN

  l_curr_calling_sequence := 'AP_ACCTG_PREPAY_DIST_PKG.Prepay_Dist_Proc<- ' ||
                                              p_calling_sequence;


  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
      l_log_msg := 'Begin of procedure '|| l_procedure_name;
      FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name||'.begin', l_log_msg);
  END IF;
  --Bug 8244163 Calculating l_total_inv_amt = total invoice amt with out AWT / PREPAY lines
  --Replacing p_inv_rec.invoice_amount with g_total_inv_amount. Because invoice_amount will be
  --adjusted when prepayment application is happened with option "prepayment on Invoice"

/*  -- 8244163
  SELECT SUM(NVL(AID.Amount,0))
  INTO   l_total_dist_amount
  FROM   AP_Invoice_Distributions_All AID
  WHERE  AID.Invoice_ID = p_inv_rec.invoice_id
  AND    AID.Line_Type_Lookup_Code <> 'PREPAY'
  AND    AID.Prepay_Distribution_ID IS NULL
  AND    AID.Prepay_Tax_Parent_ID IS NULL -- For tax dists created in R11.5
  AND    AID.AWT_Invoice_Payment_ID IS NULL
  AND    NVL(AID.Cancellation_Flag,'N') <> 'Y' -- BUG 6513956
  --bug fix 6909150
  AND    NOT EXISTS (SELECT 1
                       FROM   xla_events
                       WHERE  event_id = AID.accounting_event_id
                       AND    event_type_code IN ('INVOICE CANCELLED',
                                                  'CREDIT MEMO CANCELLED',
                                                  'DEBIT MEMO CANCELLED'));
*/
  l_total_dist_amount := g_total_dist_amount; --8244163
  l_total_inv_amount  := G_Total_Inv_amount;  --Bug9106549
  l_total_awt_amount  := g_total_awt_amount;  --Bug9106549

  g_total_dist_amt := g_total_dist_amt + p_inv_dist_rec.amount;


  IF (p_calc_mode = 'A') THEN

      -- If this payment is a final payment for the invoice then we should make sure
      -- that the sum of prepay appl dists amount should be equal to the distribution
      -- total. This way the liability is fully relieved.
      IF p_final_payment = TRUE THEN

         IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
             l_log_msg := 'Calling procedure AP_Accounting_Pay_Pkg.Get_Pay_Sum';
             FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name||'.begin', l_log_msg);
         END IF;

         AP_Accounting_Pay_Pkg.Get_Pay_Sum
                     (p_inv_dist_rec.invoice_distribution_id,
                      'PAYMENT CREATED',
                      l_total_paid_amt,
                      l_total_inv_dist_amt,
                      l_total_bank_curr_amt,
                      l_curr_calling_sequence);


         l_total_prepaid_amt := AP_Accounting_Pay_Pkg.Get_Prepay_Sum
                                    (p_inv_dist_rec.invoice_distribution_id,
                                     l_curr_calling_sequence);


         -- Converting the distribution and prepaid amount into payment currency for
         -- cross currency invoices.
         IF (p_inv_rec.invoice_currency_code <> p_inv_rec.payment_currency_code) THEN

             IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
                 l_log_msg := 'Invoice curr diff than payment curr';
                 l_log_msg := l_log_msg || ' Converting l_total_paid_amt to invoice curr';
                 FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name, l_log_msg);
             END IF;

             l_total_paid_amt := GL_Currency_API.Convert_Amount(
                                          p_inv_rec.payment_currency_code,
                                          p_inv_rec.invoice_currency_code,
                                          p_inv_rec.payment_cross_rate_date,
                                          'EMU FIXED',
                                          l_total_paid_amt);

         END IF;


         /* If this payment is a final payment then we should make sure that the
            distributed payment amount equals the distribution amount. This way the
            the liability for the distribution is relieved completely */

         IF (p_inv_dist_rec.line_type_lookup_code = 'AWT') THEN --8364229
                 l_prorated_amount := -1 * (-1*p_inv_dist_rec.amount - l_total_paid_amt +
                                          l_total_prepaid_amt);
         ELSE
           --commenting out the following code for bug8882706 as the same will be handled
           --now in ap_accounting_pay_pkg.get_prepay_sum
            /*SELECT SUM(apad.amount) INTO   l_awt_prorated_amt
              FROM ap_prepay_app_dists apad
             WHERE apad.prepay_dist_lookup_code = 'AWT'
               AND apad.awt_related_id = p_inv_dist_rec.invoice_distribution_id
               AND apad.invoice_distribution_id in
                                 (SELECT invoice_distribution_id
                                    FROM ap_invoice_distributions_all
                                   WHERE invoice_id = p_inv_rec.invoice_id
                                     AND line_type_lookup_code = 'AWT');
            */
             l_prorated_amount := -1 * (p_inv_dist_rec.amount - l_total_paid_amt +
                                         l_total_prepaid_amt );
         END IF; --p_inv_dist_rec.line_type_lookup_code = 'AWT' 8364229 ends

      ELSE

         IF g_total_dist_amt = l_total_dist_amount THEN -- last dist rec

            -- To avoid rounding, massage the last (biggest) line
            l_prorated_amount := p_prepay_dist_rec.amount - g_total_prorated_amt;
         ELSE

            IF g_total_inv_amount = 0 THEN --8244163
               l_prorated_amount := 0;

            ELSE

               IF (p_inv_dist_rec.line_type_lookup_code = 'AWT') THEN
                   l_prorated_amount := AP_UTILITIES_PKG.AP_ROUND_CURRENCY
                                          (p_prepay_dist_rec.amount * (-1*p_inv_dist_rec.amount)
                                                 / l_total_dist_amount,
                                           p_inv_rec.invoice_currency_code);
               ELSE
/*                   l_prorated_amount := AP_UTILITIES_PKG.AP_ROUND_CURRENCY
                                          (p_prepay_dist_rec.amount * p_inv_dist_rec.amount
                                                 / g_total_inv_amount, --8244163
                                           p_inv_rec.invoice_currency_code);
*/
--Bug9106549

                    SELECT  p_inv_dist_rec.amount
                            / l_total_inv_amount
                            * (p_prepay_dist_rec.amount
                                - (
                                     l_total_awt_amount / l_total_dist_amount * p_prepay_dist_rec.amount
                                  )
                               )
                            +
                              nvl(
                                  (select  sum(amount) / l_total_dist_amount *  p_prepay_dist_rec.amount
                                     from ap_invoice_distributions_all aid
                                    where aid.invoice_id=p_inv_rec.invoice_id
                                      and aid.awt_invoice_payment_id is null
                                      and aid.awt_related_id=p_inv_dist_rec.invoice_distribution_id
                                   ), 0)
                           INTO l_prorated_amount
                      from sys.dual ;

                      l_prorated_amount := ap_utilities_pkg.ap_round_currency(l_prorated_amount, p_inv_rec.invoice_currency_code);

               END IF; -- IF AWT line type

            END IF;
         END IF;

      END IF;

      IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
          l_log_msg := 'Value of l_prorated_amount = '|| l_prorated_amount;
          FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name, l_log_msg);
      END IF;


      IF (p_inv_dist_rec.line_type_lookup_code <> 'AWT') THEN
          g_total_prorated_amt := g_total_prorated_amt + l_prorated_amount;
      END IF;


  /* If this is a cascade event then we will create new payment distributions
     for the existing invoice distributions that have already been distributed to
     this payment in order to adjust the payments as a result of adjusting the
     invoice */
  ELSE

      IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
          l_log_msg := 'Calculating prorated amount for cascade adjustment';
          FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name, l_log_msg);
      END IF;

      IF g_total_inv_amount = 0 THEN --8244163
         l_prorated_amount := 0;
      ELSE

         -- In case of cascade events we will recalculate the prorated amount and subtract
         -- this amount from the already calculated amount previously so that this would
         -- give us the amount that needs to be adjusted
         l_prorated_amount := AP_UTILITIES_PKG.AP_ROUND_CURRENCY
                                (((p_inv_dist_rec.amount * p_prepay_dist_rec.amount)
                                       / g_total_inv_amount) --8244163
                                    - AP_Accounting_Pay_Pkg.get_casc_prepay_sum
                                         (p_inv_dist_rec.invoice_distribution_id,
                                          p_prepay_dist_rec.invoice_distribution_id,
                                          l_curr_calling_sequence),
                                   p_inv_rec.invoice_currency_code);

      END IF;
  END IF;


  -- Populate prepay appl dist rec

  l_pad_rec.prepay_history_id := p_prepay_hist_rec.prepay_history_id;

  IF p_inv_dist_rec.line_type_lookup_code = 'AWT' THEN
     l_pad_rec.prepay_dist_lookup_code := 'AWT';
     l_pad_rec.awt_related_id := p_inv_dist_rec.awt_related_id;
  ELSIF p_prepay_dist_rec.line_type_lookup_code = 'NONREC_TAX' THEN
     l_pad_rec.prepay_dist_lookup_code := 'PREPAY APPL NONREC TAX';
  ELSIF p_prepay_dist_rec.line_type_lookup_code = 'REC_TAX' THEN
     l_pad_rec.prepay_dist_lookup_code := 'PREPAY APPL REC TAX';
  ELSE
     l_pad_rec.prepay_dist_lookup_code := 'PREPAY APPL';
  END IF;

  l_pad_rec.invoice_distribution_id := p_inv_dist_rec.invoice_distribution_id;
  l_pad_rec.prepay_app_distribution_id := p_prepay_dist_rec.invoice_distribution_id;

  -- bug9038462, added the below if condition to ensure that the APAD
  -- records have an appropriate Accounting_Event_id in case the
  -- Accounting_Event_id has been already generated, and the APAD
  -- and APH records are being regenerated
  --
  IF p_calc_mode = 'A' THEN
    l_pad_rec.accounting_event_id := p_prepay_dist_rec.accounting_event_id;
  ELSE
    l_pad_rec.accounting_event_id := p_xla_event_rec.event_id;
  END IF;


  l_pad_rec.amount := l_prorated_amount;

  -- bug9271242, added the NVLs in derivation of exchange dates so
  -- as to ensure no difference between Item Expense and Prepaid
  -- Expense for Accounting Prepayment Applications
  --
  l_pad_rec.prepay_exchange_date := nvl(p_prepay_inv_rec.exchange_date,
                                        p_prepay_inv_rec.gl_date);
  l_pad_rec.prepay_pay_exchange_date := nvl(p_pay_hist_rec.pmt_to_base_xrate_date,
                                            p_pay_hist_rec.accounting_date);
  l_pad_rec.prepay_clr_exchange_date := nvl(p_clr_hist_rec.bank_to_base_xrate_date,
                                            p_clr_hist_rec.accounting_date);

  l_pad_rec.prepay_exchange_rate := p_prepay_inv_rec.exchange_rate;
  l_pad_rec.prepay_pay_exchange_rate := p_pay_hist_rec.pmt_to_base_xrate;
  l_pad_rec.prepay_clr_exchange_rate := p_clr_hist_rec.bank_to_base_xrate;

  l_pad_rec.prepay_exchange_rate_type := p_prepay_inv_rec.exchange_rate_type;
  l_pad_rec.prepay_pay_exchange_rate_type := p_pay_hist_rec.pmt_to_base_xrate_type;
  l_pad_rec.prepay_clr_exchange_rate_type := p_clr_hist_rec.bank_to_base_xrate_type;


  l_pad_rec.base_amt_at_prepay_xrate :=  AP_Accounting_Pay_Pkg.Get_Base_Amount
                                              (l_prorated_amount,
                                               p_prepay_inv_rec.invoice_currency_code,
                                               p_inv_rec.base_currency_code,
                                               p_prepay_inv_rec.exchange_rate_type,
                                               p_prepay_inv_rec.exchange_date,
                                               p_prepay_inv_rec.exchange_rate,
                                               l_curr_calling_sequence);


  IF (p_inv_rec.invoice_currency_code <> p_pay_hist_rec.pmt_currency_code) THEN
      l_prorated_pay_amt := AP_UTILITIES_PKG.AP_Round_Currency(
                                  l_prorated_amount * p_inv_rec.payment_cross_rate,
                                  p_pay_hist_rec.pmt_currency_code);
  ELSE
      l_prorated_pay_amt := l_prorated_amount;
  END IF;

  l_pad_rec.base_amt_at_prepay_pay_xrate :=  AP_Accounting_Pay_Pkg.Get_Base_Amount
                                               (l_prorated_pay_amt,
                                                p_pay_hist_rec.pmt_currency_code,
                                                p_inv_rec.base_currency_code,
                                                p_pay_hist_rec.pmt_to_base_xrate_type,
                                                p_pay_hist_rec.pmt_to_base_xrate_date,
                                                p_pay_hist_rec.pmt_to_base_xrate,
                                                l_curr_calling_sequence);

  IF (p_clr_hist_rec.pmt_currency_code <> p_clr_hist_rec.bank_currency_code) THEN

      l_prorated_clr_amt := AP_UTILITIES_PKG.AP_Round_Currency(
                                  l_prorated_pay_amt * p_clr_hist_rec.pmt_to_base_xrate,
                                  p_pay_hist_rec.bank_currency_code);
  ELSE
      l_prorated_clr_amt := l_prorated_pay_amt;
  END IF;

  l_pad_rec.base_amt_at_prepay_clr_xrate :=  AP_Accounting_Pay_Pkg.Get_Base_Amount
                                               (l_prorated_clr_amt,
                                                p_clr_hist_rec.bank_currency_code,
                                                p_inv_rec.base_currency_code,
                                                p_clr_hist_rec.bank_to_base_xrate_type,
                                                p_clr_hist_rec.bank_to_base_xrate_date,
                                                p_clr_hist_rec.bank_to_base_xrate,
                                                l_curr_calling_sequence);


  l_pad_rec.base_amount  := AP_Accounting_Pay_Pkg.Get_Base_Amount
                                   (l_prorated_amount,
                                    p_inv_rec.invoice_currency_code,
                                    p_inv_rec.base_currency_code,
                                    p_inv_rec.exchange_rate_type,
                                    p_inv_rec.exchange_date,
                                    p_inv_rec.exchange_rate,
                                    l_curr_calling_sequence);


  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
      l_log_msg := 'Calling procedure Prepay_Dist_Insert';
      FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name, l_log_msg);
  END IF;


  IF p_inv_dist_rec.quantity_variance IS NOT NULL THEN

     IF p_inv_dist_rec.amount = 0 THEN
        l_qty_variance := 0;
     ELSE
        l_qty_variance := AP_Utilities_PKG.AP_Round_Currency(
                             ((p_inv_dist_rec.quantity_variance * l_prorated_amount) /
                                     p_inv_dist_rec.amount),
                               p_inv_rec.invoice_currency_code);
     END IF;

     IF p_inv_dist_rec.base_amount = 0 THEN
        l_base_qty_variance := 0;
     ELSE
        l_base_qty_variance := AP_Utilities_PKG.AP_Round_Currency(
                                  ((p_inv_dist_rec.base_quantity_variance
                                        * l_pad_rec.base_amount)
                                        / p_inv_dist_rec.base_amount),
                                    p_inv_rec.base_currency_code);

     END IF;
  END IF;

  IF p_inv_dist_rec.amount_variance IS NOT NULL THEN

     IF p_inv_dist_rec.amount = 0 THEN
        l_amt_variance := 0;
     ELSE
        l_amt_variance := AP_Utilities_PKG.AP_Round_Currency(
                             ((p_inv_dist_rec.amount_variance * l_prorated_amount) /
                                     p_inv_dist_rec.amount),
                               p_inv_rec.invoice_currency_code);
     END IF;

     IF p_inv_dist_rec.base_amount = 0 THEN
        l_base_amt_variance := 0;
     ELSE
        l_base_amt_variance := AP_Utilities_PKG.AP_Round_Currency(
                                  ((p_inv_dist_rec.base_amount_variance
                                        * l_pad_rec.base_amount)
                                        / p_inv_dist_rec.base_amount),
                                    p_inv_rec.base_currency_code);
     END IF;
  END IF;

  l_pad_rec.quantity_variance := l_qty_variance;
  l_pad_rec.invoice_base_qty_variance := l_base_qty_variance;
  l_pad_rec.amount_variance := l_amt_variance;
  l_pad_rec.invoice_base_amt_variance := l_base_amt_variance;


  Prepay_Dist_Insert
          (l_pad_rec,
           l_curr_calling_sequence);

  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
      l_log_msg := 'Procedure Prepay_Dist_Insert executed';
      FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name, l_log_msg);
  END IF;


  IF (p_prepay_dist_rec.prepay_tax_diff_amount <> 0) THEN


      IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
          l_log_msg := 'Calling procedure Prepay_Dist_Tax_Diff';
          FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name, l_log_msg);
      END IF;

      -- Creating the tax diff distributions
      Prepay_Dist_Tax_Diff
          (p_pay_hist_rec,
           p_clr_hist_rec,
           p_inv_rec,
           p_prepay_inv_rec,
           p_prepay_hist_rec,
           p_prepay_dist_rec,
           p_inv_dist_rec,
           p_calc_mode,
           l_curr_calling_sequence);

      IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
          l_log_msg := 'Procedure Prepay_Dist_Tax_Diff executed';
          FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name, l_log_msg);
      END IF;


  END IF;


  IF (p_inv_dist_rec.po_distribution_id IS NOT NULL AND
      p_inv_rec.invoice_currency_code <> p_inv_rec.base_currency_code) THEN

      IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
          l_log_msg := 'Calling procedure Prepay_Dist_ERV';
          FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name, l_log_msg);
      END IF;

     -- Creating ERV distributions
     Prepay_Dist_ERV
          (p_pay_hist_rec,
           p_clr_hist_rec,
           p_inv_rec,
           p_prepay_inv_rec,
           p_prepay_hist_rec,
           p_prepay_dist_rec,
           p_inv_dist_rec,
           l_prorated_amount,
           l_curr_calling_sequence);

      IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
          l_log_msg := 'Procedure Prepay_Dist_ERV executed';
          FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name, l_log_msg);
      END IF;

  END IF;

  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
      l_log_msg := 'Procedure Prepay_Dist_Insert executed';
      FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name, l_log_msg);
  END IF;

EXCEPTION
  WHEN OTHERS THEN
    IF (SQLCODE <> -20001) THEN
      FND_MESSAGE.SET_NAME('SQLAP','AP_DEBUG');
      FND_MESSAGE.SET_TOKEN('ERROR',SQLERRM);
      FND_MESSAGE.SET_TOKEN('CALLING_SEQUENCE', l_curr_calling_sequence);
    END IF;
    APP_EXCEPTION.RAISE_EXCEPTION;

END Prepay_Dist_Proc;



---------------------------------------------------------------------
-- Procedure Prepay_Dist_Tax_Diff
-- This procedure prorates the tax difference amounts for each
-- distribution and inserts the calculated values into prepayment
-- application distribution table
---------------------------------------------------------------------

PROCEDURE Prepay_Dist_Tax_Diff
      (p_pay_hist_rec       IN    ap_accounting_pay_pkg.r_pay_hist_info
      ,p_clr_hist_rec       IN    ap_accounting_pay_pkg.r_pay_hist_info
      ,p_inv_rec            IN    ap_accounting_pay_pkg.r_invoices_info
      ,p_prepay_inv_rec     IN    ap_accounting_pay_pkg.r_invoices_info
      ,p_prepay_hist_rec    IN    r_prepay_hist_info
      ,p_prepay_dist_rec    IN    r_prepay_dist_info
      ,p_inv_dist_rec       IN    ap_accounting_pay_pkg.r_inv_dist_info
      ,p_calc_mode          IN    VARCHAR2
      ,p_calling_sequence   IN    VARCHAR2
      ) IS


  l_curr_calling_sequence       VARCHAR2(2000);
  l_prorated_amount             NUMBER;
  l_prorated_pay_amt            NUMBER;
  l_prorated_clr_amt            NUMBER;

  l_pad_rec                     AP_PREPAY_APP_DISTS%ROWTYPE;

  -- Logging Infra:
  l_procedure_name CONSTANT VARCHAR2(30) := 'Prepay_Dist_Tax_Diff';
  l_log_msg        FND_LOG_MESSAGES.MESSAGE_TEXT%TYPE;


BEGIN

  l_curr_calling_sequence := 'AP_ACCTG_PREPAY_DIST_PKG.Prepay_Dist_Tax_Diff<- ' ||
                                              p_calling_sequence;

  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
      l_log_msg := 'Begin of procedure '|| l_procedure_name;
      FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name||'.begin', l_log_msg);
  END IF;


  IF p_calc_mode = 'A' THEN
     IF g_total_dist_amt = g_total_inv_amount THEN -- last dist rec --8244163

        -- To avoid rounding, massage the last (biggest) line
        l_prorated_amount := p_prepay_dist_rec.prepay_tax_diff_amount - g_total_tax_diff_amt;
     ELSE

        IF g_total_inv_amount = 0 THEN --8244163
           l_prorated_amount := 0;

        ELSE
           l_prorated_amount := AP_UTILITIES_PKG.AP_ROUND_CURRENCY
                                  (p_prepay_dist_rec.prepay_tax_diff_amount * p_inv_dist_rec.amount
                                       / g_total_inv_amount,
                                    p_inv_rec.invoice_currency_code);

        END IF;
     END IF;

  ELSE

      IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
          l_log_msg := 'Calculating prorated amount for cascade adjustment';
          FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name, l_log_msg);
      END IF;

      IF g_total_inv_amount = 0 THEN
         l_prorated_amount := 0;
      ELSE

         -- In case of cascade events we will recalculate the prorated amount and subtract
         -- this amount from the already calculated amount previously so that this would
         -- give us the amount that needs to be adjusted
         l_prorated_amount := AP_UTILITIES_PKG.AP_ROUND_CURRENCY
                                (((p_inv_dist_rec.amount * p_prepay_dist_rec.prepay_tax_diff_amount)
                                       / g_total_inv_amount)
                                    - AP_Accounting_Pay_Pkg.get_casc_tax_diff_sum
                                         (p_inv_dist_rec.invoice_distribution_id,
                                          p_prepay_dist_rec.invoice_distribution_id,
                                          l_curr_calling_sequence),
                                   p_inv_rec.invoice_currency_code);

      END IF;
  END IF;

  IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
      l_log_msg := 'Value for l_prorated_amount = '|| l_prorated_amount;
      FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name, l_log_msg);
  END IF;


  g_total_tax_diff_amt := g_total_tax_diff_amt + l_prorated_amount;


  -- Populate prepay appl dist rec

  l_pad_rec.prepay_history_id := p_prepay_hist_rec.prepay_history_id;
  l_pad_rec.prepay_dist_lookup_code := 'TAX DIFF';
  l_pad_rec.invoice_distribution_id := p_inv_dist_rec.invoice_distribution_id;
  l_pad_rec.prepay_app_distribution_id := p_prepay_dist_rec.invoice_distribution_id;
  l_pad_rec.accounting_event_id := p_prepay_dist_rec.accounting_event_id;

  l_pad_rec.amount := l_prorated_amount;

  l_pad_rec.prepay_exchange_date := p_prepay_inv_rec.exchange_date;
  l_pad_rec.prepay_pay_exchange_date := p_pay_hist_rec.pmt_to_base_xrate_date;
  l_pad_rec.prepay_clr_exchange_date := p_clr_hist_rec.bank_to_base_xrate_date;

  l_pad_rec.prepay_exchange_rate := p_prepay_inv_rec.exchange_rate;
  l_pad_rec.prepay_pay_exchange_rate := p_pay_hist_rec.pmt_to_base_xrate;
  l_pad_rec.prepay_clr_exchange_rate := p_clr_hist_rec.bank_to_base_xrate;

  l_pad_rec.prepay_exchange_rate_type := p_prepay_inv_rec.exchange_rate_type;
  l_pad_rec.prepay_pay_exchange_rate_type := p_pay_hist_rec.pmt_to_base_xrate_type;
  l_pad_rec.prepay_clr_exchange_rate_type := p_clr_hist_rec.bank_to_base_xrate_type;


  l_pad_rec.base_amt_at_prepay_xrate :=  AP_Accounting_Pay_Pkg.Get_Base_Amount
                                              (l_prorated_amount,
                                               p_prepay_inv_rec.invoice_currency_code,
                                               p_inv_rec.base_currency_code,
                                               p_prepay_inv_rec.exchange_rate_type,
                                               p_prepay_inv_rec.exchange_date,
                                               p_prepay_inv_rec.exchange_rate,
                                               l_curr_calling_sequence);

  IF (p_inv_rec.invoice_currency_code <> p_pay_hist_rec.pmt_currency_code) THEN
      l_prorated_pay_amt := l_prorated_amount * p_inv_rec.payment_cross_rate;
  ELSE
      l_prorated_pay_amt := l_prorated_amount;
  END IF;


  l_pad_rec.base_amt_at_prepay_pay_xrate :=  AP_Accounting_Pay_Pkg.Get_Base_Amount
                                               (l_prorated_pay_amt,
                                                p_pay_hist_rec.pmt_currency_code,
                                                p_inv_rec.base_currency_code,
                                                p_pay_hist_rec.pmt_to_base_xrate_type,
                                                p_pay_hist_rec.pmt_to_base_xrate_date,
                                                p_pay_hist_rec.pmt_to_base_xrate,
                                                l_curr_calling_sequence);

  IF (p_clr_hist_rec.pmt_currency_code <> p_clr_hist_rec.bank_currency_code) THEN

      l_prorated_clr_amt := AP_UTILITIES_PKG.AP_Round_Currency(
                                  l_prorated_pay_amt * p_clr_hist_rec.pmt_to_base_xrate,
                                  p_pay_hist_rec.bank_currency_code);
  ELSE
      l_prorated_clr_amt := l_prorated_pay_amt;
  END IF;

  l_pad_rec.base_amt_at_prepay_clr_xrate :=  AP_Accounting_Pay_Pkg.Get_Base_Amount
                                               (l_prorated_clr_amt,
                                                p_clr_hist_rec.bank_currency_code,
                                                p_inv_rec.base_currency_code,
                                                p_clr_hist_rec.bank_to_base_xrate_type,
                                                p_clr_hist_rec.bank_to_base_xrate_date,
                                                p_clr_hist_rec.bank_to_base_xrate,
                                                l_curr_calling_sequence);


  l_pad_rec.base_amount  := AP_Accounting_Pay_Pkg.Get_Base_Amount
                                   (l_prorated_amount,
                                    p_inv_rec.invoice_currency_code,
                                    p_inv_rec.base_currency_code,
                                    p_inv_rec.exchange_rate_type,
                                    p_inv_rec.exchange_date,
                                    p_inv_rec.exchange_rate,
                                    l_curr_calling_sequence);


  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
      l_log_msg := 'Calling procedure Prepay_Dist_Insert';
      FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name, l_log_msg);
  END IF;

  Prepay_Dist_Insert
          (l_pad_rec,
           l_curr_calling_sequence);


  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
      l_log_msg := 'Procedure Prepay_Dist_Insert executed';
      FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name, l_log_msg);
  END IF;

EXCEPTION
  WHEN OTHERS THEN
    IF (SQLCODE <> -20001) THEN
      FND_MESSAGE.SET_NAME('SQLAP','AP_DEBUG');
      FND_MESSAGE.SET_TOKEN('ERROR',SQLERRM);
      FND_MESSAGE.SET_TOKEN('CALLING_SEQUENCE', l_curr_calling_sequence);
    END IF;
    APP_EXCEPTION.RAISE_EXCEPTION;

END Prepay_Dist_Tax_Diff;




---------------------------------------------------------------------
-- Procedure Prepay_Dist_ERV
-- This procedure calculates the ERV base amounts for the ERV distributions
-- and inserts the calculated values into prepay appl payment dists table
---------------------------------------------------------------------

PROCEDURE Prepay_Dist_ERV
      (p_pay_hist_rec     IN    ap_accounting_pay_pkg.r_pay_hist_info
      ,p_clr_hist_rec     IN    ap_accounting_pay_pkg.r_pay_hist_info
      ,p_inv_rec          IN    ap_accounting_pay_pkg.r_invoices_info
      ,p_prepay_inv_rec   IN    ap_accounting_pay_pkg.r_invoices_info
      ,p_prepay_hist_rec  IN    r_prepay_hist_info
      ,p_prepay_dist_rec  IN    r_prepay_dist_info
      ,p_inv_dist_rec     IN    ap_accounting_pay_pkg.r_inv_dist_info
      ,p_prorated_amount  IN    NUMBER
      ,p_calling_sequence IN    VARCHAR2
      ) IS

  l_curr_calling_sequence          VARCHAR2(2000);
  l_po_exchange_rate               NUMBER;
  l_po_pay_exchange_rate           NUMBER;
  l_pay_erv_amount                 NUMBER;
  l_clr_erv_amount                 NUMBER;
  l_inv_erv_amount                 NUMBER;
  l_pad_rec                       AP_PREPAY_APP_DISTS%ROWTYPE;

  -- Logging Infra:
  l_procedure_name CONSTANT VARCHAR2(30) := 'Prepay_Dist_ERV';
  l_log_msg        FND_LOG_MESSAGES.MESSAGE_TEXT%TYPE;

BEGIN


  l_curr_calling_sequence := 'AP_ACCTG_PREPAY_DIST_PKG.PrePay_Dist_ERV<- ' ||
                                                 p_calling_sequence;


  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
      l_log_msg := 'Begin of procedure '|| l_procedure_name;
      FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name||'.begin', l_log_msg);
  END IF;


  IF p_inv_dist_rec.rcv_transaction_id IS NOT NULL THEN

     SELECT Currency_Conversion_Rate
     INTO   l_po_exchange_rate
     FROM   rcv_transactions
     WHERE  transaction_id = p_inv_dist_rec.rcv_transaction_id;

  ELSE

     SELECT Rate
     INTO   l_po_exchange_rate
     FROM   PO_Distributions_All
     WHERE  PO_Distribution_ID = p_inv_dist_rec.PO_Distribution_ID;

  END IF;

  IF p_inv_rec.invoice_currency_code <> p_inv_rec.payment_currency_code THEN
     l_po_pay_exchange_rate := l_po_exchange_rate / p_inv_rec.payment_cross_rate;
  ELSE
     l_po_pay_exchange_rate := l_po_exchange_rate;
  END IF;


  IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
      l_log_msg := 'Value of l_po_pay_exchange_rate = '||l_po_pay_exchange_rate;
      FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name, l_log_msg);
  END IF;


  /* For Cash Basis ERV is Difference between Payment Exchange Rate and
     either Receipt Exchange rate or PO distributions exchange rate */

  l_pay_erv_amount := AP_UTILITIES_PKG.AP_ROUND_CURRENCY(
                         (p_pay_hist_rec.pmt_to_base_xrate - l_po_pay_exchange_rate) *
                              p_prorated_amount, p_pay_hist_rec.pmt_currency_code);


  IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
      l_log_msg := 'Value of l_pay_erv_amount = '||l_pay_erv_amount;
      FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name, l_log_msg);
  END IF;


  /* If the payment accounting is at the at the clearing time, then ERV should be
     calculated based on the difference between Prepay payment clearing exchange rate
     and either Receipt Exchange rate or PO distributions exchange rate */

  IF p_clr_hist_rec.pmt_currency_code IS NOT NULL THEN   -- Bug 5701788.
    l_clr_erv_amount := AP_UTILITIES_PKG.AP_ROUND_CURRENCY(
                         (p_clr_hist_rec.pmt_to_base_xrate - l_po_pay_exchange_rate) *
                              p_inv_dist_rec.amount, p_clr_hist_rec.pmt_currency_code);
  END IF;

  IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
      l_log_msg := 'Value of l_clr_erv_amount = '||l_clr_erv_amount;
      FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name, l_log_msg);
  END IF;


  /* In order to back out the encumbrance entries correctly during cash basis
     we need to calculate ERV based on the difference between the Invoice
     Exchange Rate and either Receipt Exchange rate or PO distributions
     exchange rate. This calculated ERV amount will be stored in the
     invoice_dist_base_amount column */

  l_inv_erv_amount := AP_UTILITIES_PKG.AP_ROUND_CURRENCY(
                         (p_inv_rec.exchange_rate - l_po_exchange_rate) *
                              p_prorated_amount, p_inv_rec.invoice_currency_code);

  IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
      l_log_msg := 'Value of l_inv_erv_amount = '||l_inv_erv_amount;
      FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name, l_log_msg);
  END IF;


  IF (p_inv_dist_rec.line_type_lookup_code IN ('NONREC_TAX', 'REC_TAX')) THEN
      l_pad_rec.prepay_dist_lookup_code := 'TAX EXCHANGE RATE VARIANCE';
  ELSE
      l_pad_rec.prepay_dist_lookup_code := 'EXCHANGE RATE VARIANCE';
  END IF;

  l_pad_rec.prepay_history_id := p_prepay_hist_rec.prepay_history_id;
  l_pad_rec.invoice_distribution_id := p_inv_dist_rec.invoice_distribution_id;
  l_pad_rec.prepay_app_distribution_id := p_prepay_dist_rec.invoice_distribution_id;
  l_pad_rec.accounting_event_id := p_prepay_dist_rec.accounting_event_id;

  l_pad_rec.amount := 0;

  l_pad_rec.prepay_exchange_date := p_prepay_inv_rec.exchange_date;
  l_pad_rec.prepay_pay_exchange_date := p_pay_hist_rec.pmt_to_base_xrate_date;
  l_pad_rec.prepay_clr_exchange_date := p_clr_hist_rec.bank_to_base_xrate_date;

  l_pad_rec.prepay_exchange_rate := p_prepay_inv_rec.exchange_rate;
  l_pad_rec.prepay_pay_exchange_rate := p_pay_hist_rec.pmt_to_base_xrate;
  l_pad_rec.prepay_clr_exchange_rate := p_clr_hist_rec.bank_to_base_xrate;

  l_pad_rec.prepay_exchange_rate_type := p_prepay_inv_rec.exchange_rate_type;
  l_pad_rec.prepay_pay_exchange_rate_type := p_pay_hist_rec.pmt_to_base_xrate_type;
  l_pad_rec.prepay_clr_exchange_rate_type := p_clr_hist_rec.bank_to_base_xrate_type;


  l_pad_rec.base_amt_at_prepay_xrate :=  0;
  l_pad_rec.base_amt_at_prepay_pay_xrate := l_pay_erv_amount;
  l_pad_rec.base_amt_at_prepay_clr_xrate := l_clr_erv_amount;
  l_pad_rec.base_amount := 0;

  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
      l_log_msg := 'Calling procedure Prepay_Dist_Insert';
      FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name, l_log_msg);
  END IF;

  Prepay_Dist_Insert
          (l_pad_rec,
           l_curr_calling_sequence);

  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
      l_log_msg := 'Procedure Prepay_Dist_Insert executed';
      FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name, l_log_msg);
  END IF;


EXCEPTION
  WHEN OTHERS THEN
    IF (SQLCODE <> -20001) THEN
      FND_MESSAGE.SET_NAME('SQLAP','AP_DEBUG');
      FND_MESSAGE.SET_TOKEN('ERROR',SQLERRM);
      FND_MESSAGE.SET_TOKEN('CALLING_SEQUENCE', l_curr_calling_sequence);
    END IF;
    APP_EXCEPTION.RAISE_EXCEPTION;

END Prepay_Dist_ERV;



---------------------------------------------------------------------
-- Procedure Prepay_Dist_Reverse
-- This procedure reverses the prepayment application payment distributions
-- of the prepayment unapplications.
--
---------------------------------------------------------------------
-- Bug 6698125. Added p_xla_event_rec parameter
-- Bug 7134020. Added p_inv_dist_id parameter
PROCEDURE Prepay_Dist_Reverse
      (p_prepay_hist_rec       IN    r_prepay_hist_info
      ,p_prepay_reversal_id    IN    NUMBER
      ,P_XLA_Event_Rec         IN    ap_accounting_pay_pkg.r_xla_event_info
      ,p_inv_reversal_id       IN    NUMBER
      ,p_inv_dist_id           IN    NUMBER
      ,p_prepay_inv_dist_id    IN    NUMBER
      ,p_calling_sequence      IN    VARCHAR2
      ) IS

  l_curr_calling_sequence          VARCHAR2(2000);

  -- Logging Infra:
  l_procedure_name CONSTANT VARCHAR2(30) := 'Prepay_Dist_Reverse';
  l_log_msg        FND_LOG_MESSAGES.MESSAGE_TEXT%TYPE;


BEGIN

  l_curr_calling_sequence := 'AP_ACCTG_PAY_DIST_PKG.Prepay_Dist_Reverse<-' ||
                                           p_calling_sequence;

  -- Logging Infra: Procedure level
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
      l_log_msg := 'Begin of procedure '|| l_procedure_name;
      FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name||'.begin', l_log_msg);
  END IF;

  -- Bug 6698125. Added if condition to correctly reverse the prepay app
  -- distributions based on if reversed for prepayment unapplication or
  -- prepayment application adjusted events.

  IF p_prepay_reversal_id IS NOT NULL THEN

     -- bug9038462, modified this Insert into apad which takes
     -- care of the Non-Cascade Prepayment reversals, to stamp
     -- an Accounting_event_id appropriately, if present on the
     -- corresponding Prepayment History Record
     --

     INSERT INTO AP_Prepay_App_Dists
           (Prepay_App_Dist_ID,
            Prepay_Dist_Lookup_Code,
            Invoice_Distribution_ID,
            Prepay_App_Distribution_ID,
            Accounting_Event_ID,
            Prepay_History_ID,
            Prepay_Exchange_Date,
            Prepay_Pay_Exchange_Date,
            Prepay_Clr_Exchange_Date,
            Prepay_Exchange_Rate,
            Prepay_Pay_Exchange_Rate,
            Prepay_Clr_Exchange_Rate,
            Prepay_Exchange_Rate_Type,
            Prepay_Pay_Exchange_Rate_Type,
            Prepay_Clr_Exchange_Rate_Type,
            Reversed_Prepay_App_Dist_ID,
            Amount,
            Base_Amt_At_Prepay_XRate,
            Base_Amt_At_Prepay_Pay_XRate,
            Base_Amt_At_Prepay_Clr_XRate,
            Base_Amount,
            AWT_Related_ID,
            PA_Addition_Flag,
            Quantity_Variance,
            Invoice_Base_Qty_Variance,
            Amount_Variance,
            Invoice_Base_Amt_Variance,
            Created_By,
            Creation_Date,
            Last_Update_Date,
            Last_Updated_By,
            Last_Update_Login,
            Program_Application_ID,
            Program_ID,
            Program_Update_Date,
            Request_ID
           )
     SELECT AP_Prepay_App_Dists_S.nextval,
            APAD.Prepay_Dist_Lookup_Code,
            APAD.Invoice_Distribution_ID,
            p_prepay_inv_dist_id,
            xer.event_id,                 --p_xla_event_rec.event_id,
            p_prepay_hist_rec.prepay_history_id,
            APAD.Prepay_Exchange_Date,
            APAD.Prepay_Pay_Exchange_Date,
            APAD.Prepay_Clr_Exchange_Date,
            APAD.Prepay_Exchange_Rate,
            APAD.Prepay_Pay_Exchange_Rate,
            APAD.Prepay_Clr_Exchange_Rate,
            APAD.Prepay_Exchange_Rate_Type,
            APAD.Prepay_Pay_Exchange_Rate_Type,
            APAD.Prepay_Clr_Exchange_Rate_Type,
            APAD.Prepay_App_Dist_ID,
            -1 * APAD.Amount,
            -1 * APAD.Base_Amt_At_Prepay_XRate,
            -1 * APAD.Base_Amt_At_Prepay_Pay_XRate,
            -1 * APAD.Base_Amt_At_Prepay_Clr_XRate,
            -1 * APAD.Base_Amount,
            APAD.AWT_Related_ID,
            'N',
            APAD.Quantity_Variance,
            APAD.Invoice_Base_Qty_Variance,
            APAD.Amount_Variance,
            APAD.Invoice_Base_Amt_Variance,
            FND_GLOBAL.User_ID,
            SYSDATE,
            SYSDATE,
            FND_GLOBAL.User_ID,
            FND_GLOBAL.User_ID,
            FND_GLOBAL.Prog_Appl_ID,
            FND_GLOBAL.Conc_Program_ID,
            SYSDATE,
            FND_GLOBAL.Conc_Request_ID
     FROM   AP_Prepay_App_Dists APAD,
            ap_prepay_history_all aph,                                 --Bug 9112240
            ap_prepay_history_all aphr,
            xla_events xer
     WHERE  apad.Prepay_App_Distribution_ID = P_Prepay_Reversal_ID
       AND  apad.prepay_history_id          = aph.prepay_history_id  --Bug 9112240
       AND  aphr.prepay_history_id          = p_prepay_hist_rec.prepay_history_id
       AND  aphr.accounting_event_id        = xer.event_id(+)
       AND  xer.application_id(+)           = 200;

  ELSIF p_inv_reversal_id IS NOT NULL THEN

     INSERT INTO AP_Prepay_App_Dists
           (Prepay_App_Dist_ID,
            Prepay_Dist_Lookup_Code,
            Invoice_Distribution_ID,
            Prepay_App_Distribution_ID,
            Accounting_Event_ID,
            Prepay_History_ID,
            Prepay_Exchange_Date,
            Prepay_Pay_Exchange_Date,
            Prepay_Clr_Exchange_Date,
            Prepay_Exchange_Rate,
            Prepay_Pay_Exchange_Rate,
            Prepay_Clr_Exchange_Rate,
            Prepay_Exchange_Rate_Type,
            Prepay_Pay_Exchange_Rate_Type,
            Prepay_Clr_Exchange_Rate_Type,
            Reversed_Prepay_App_Dist_ID,
            Amount,
            Base_Amt_At_Prepay_XRate,
            Base_Amt_At_Prepay_Pay_XRate,
            Base_Amt_At_Prepay_Clr_XRate,
            Base_Amount,
            AWT_Related_ID,
            PA_Addition_Flag,
            Quantity_Variance,
            Invoice_Base_Qty_Variance,
            Amount_Variance,
            Invoice_Base_Amt_Variance,
            Created_By,
            Creation_Date,
            Last_Update_Date,
            Last_Updated_By,
            Last_Update_Login,
            Program_Application_ID,
            Program_ID,
            Program_Update_Date,
            Request_ID
           )
     SELECT AP_Prepay_App_Dists_S.nextval,
            APAD.Prepay_Dist_Lookup_Code,
            p_inv_dist_id, -- Bug 7134020
            APAD.Prepay_App_Distribution_ID,
            p_xla_event_rec.event_id,
            p_prepay_hist_rec.prepay_history_id,
            APAD.Prepay_Exchange_Date,
            APAD.Prepay_Pay_Exchange_Date,
            APAD.Prepay_Clr_Exchange_Date,
            APAD.Prepay_Exchange_Rate,
            APAD.Prepay_Pay_Exchange_Rate,
            APAD.Prepay_Clr_Exchange_Rate,
            APAD.Prepay_Exchange_Rate_Type,
            APAD.Prepay_Pay_Exchange_Rate_Type,
            APAD.Prepay_Clr_Exchange_Rate_Type,
            APAD.Prepay_App_Dist_ID,
            -1 * APAD.Amount,
            -1 * APAD.Base_Amt_At_Prepay_XRate,
            -1 * APAD.Base_Amt_At_Prepay_Pay_XRate,
            -1 * APAD.Base_Amt_At_Prepay_Clr_XRate,
            -1 * APAD.Base_Amount,
            APAD.AWT_Related_ID,
            'N',
            APAD.Quantity_Variance,
            APAD.Invoice_Base_Qty_Variance,
            APAD.Amount_Variance,
            APAD.Invoice_Base_Amt_Variance,
            FND_GLOBAL.User_ID,
            SYSDATE,
            SYSDATE,
            FND_GLOBAL.User_ID,
            FND_GLOBAL.User_ID,
            FND_GLOBAL.Prog_Appl_ID,
            FND_GLOBAL.Conc_Program_ID,
            SYSDATE,
            FND_GLOBAL.Conc_Request_ID
     FROM   AP_Prepay_App_Dists APAD,
            ap_prepay_history_all aph                                 --Bug 9112240
     WHERE  apad.prepay_history_id          = aph.prepay_history_id   --Bug 9112240
       AND  APAD.Prepay_App_Distribution_ID = nvl(p_prepay_inv_dist_id,APAD.Prepay_App_Distribution_ID)   --7686421
       AND  APAD.Invoice_Distribution_Id    = p_inv_reversal_id                                           --bug9440073
     /*AND  APAD.Accounting_Event_Id        = p_prepay_hist_rec.related_prepay_app_event_id; --bug9440073 */
     /*Bug 11872456*/
       AND  nvl(aph.related_prepay_app_event_id, aph.accounting_event_id) = p_prepay_hist_rec.related_prepay_app_event_id;
       
  END IF;

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

END Prepay_Dist_Reverse;



----------------------------------------------------------------------------------
-- PROCEDURE Prepay_Dist_Insert
-- This procedure is used to insert the prepay application payment distributions
-- into the ap_prepay_app_dists table
----------------------------------------------------------------------------------

PROCEDURE Prepay_Dist_Insert
     (P_PAD_Rec           IN     AP_PREPAY_APP_DISTS%ROWTYPE
     ,P_Calling_Sequence  IN     VARCHAR2
     ) IS

  l_curr_calling_sequence      VARCHAR2(2000);

  -- Logging Infra:
  l_procedure_name CONSTANT VARCHAR2(30) := 'Prepay_Dist_Insert';
  l_log_msg        FND_LOG_MESSAGES.MESSAGE_TEXT%TYPE;

BEGIN

  l_curr_calling_sequence := 'AP_ACCTG_PREPAY_DIST_PKG.Prepay_Dist_Insert<- ' ||
                                     P_Calling_Sequence;

  -- Logging Infra: Procedure level
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
      l_log_msg := 'Begin of procedure '|| l_procedure_name;
      FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name||'.begin', l_log_msg);
  END IF;


  INSERT INTO AP_Prepay_App_Dists
        (Prepay_App_Dist_ID,
         Prepay_Dist_Lookup_Code,
         Invoice_Distribution_ID,
         Prepay_App_Distribution_ID,
         Accounting_Event_ID,
         Prepay_History_ID,
         Prepay_Exchange_Date,
         Prepay_Pay_Exchange_Date,
         Prepay_Clr_Exchange_Date,
         Prepay_Exchange_Rate,
         Prepay_Pay_Exchange_Rate,
         Prepay_Clr_Exchange_Rate,
         Prepay_Exchange_Rate_Type,
         Prepay_Pay_Exchange_Rate_Type,
         Prepay_Clr_Exchange_Rate_Type,
         Reversed_Prepay_App_Dist_ID,
         Amount,
         Base_Amt_At_Prepay_XRate,
         Base_Amt_At_Prepay_Pay_XRate,
         Base_Amt_At_Prepay_Clr_XRate,
         Base_Amount,
         AWT_Related_ID,
         PA_Addition_Flag,
         Quantity_Variance,
         Invoice_Base_Qty_Variance,
         Amount_Variance,
         Invoice_Base_Amt_Variance,
         Created_By,
         Creation_Date,
         Last_Update_Date,
         Last_Updated_By,
         Last_Update_Login,
         Program_Application_ID,
         Program_ID,
         Program_Update_Date,
         Request_ID
         )
  VALUES (AP_Prepay_App_Dists_S.nextval,
         P_PAD_Rec.Prepay_Dist_Lookup_Code,
         P_PAD_Rec.Invoice_Distribution_ID,
         P_PAD_Rec.Prepay_App_Distribution_ID,
         P_PAD_Rec.Accounting_Event_ID,
         P_PAD_Rec.Prepay_History_ID,
         P_PAD_Rec.Prepay_Exchange_Date,
         P_PAD_Rec.Prepay_Pay_Exchange_Date,
         P_PAD_Rec.Prepay_Clr_Exchange_Date,
         P_PAD_Rec.Prepay_Exchange_Rate,
         P_PAD_Rec.Prepay_Pay_Exchange_Rate,
         P_PAD_Rec.Prepay_Clr_Exchange_Rate,
         P_PAD_Rec.Prepay_Exchange_Rate_Type,
         P_PAD_Rec.Prepay_Pay_Exchange_Rate_Type,
         P_PAD_Rec.Prepay_Clr_Exchange_Rate_Type,
         P_PAD_Rec.Reversed_Prepay_App_Dist_ID,
         P_PAD_Rec.Amount,
         P_PAD_Rec.Base_Amt_At_Prepay_XRate,
         P_PAD_Rec.Base_Amt_At_Prepay_Pay_XRate,
         P_PAD_Rec.Base_Amt_At_Prepay_Clr_XRate,
         P_PAD_Rec.Base_Amount,
         P_PAD_Rec.AWT_Related_ID,
         'N',
         P_PAD_Rec.Quantity_Variance,
         P_PAD_Rec.Invoice_Base_Qty_Variance,
         P_PAD_Rec.Amount_Variance,
         P_PAD_Rec.Invoice_Base_Amt_Variance,
         FND_GLOBAL.User_ID,
         SYSDATE,
         SYSDATE,
         FND_GLOBAL.User_ID,
         FND_GLOBAL.User_ID,
         FND_GLOBAL.Prog_Appl_ID,
         FND_GLOBAL.Conc_Program_ID,
         SYSDATE,
         FND_GLOBAL.Conc_Request_ID
         );

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

END Prepay_Dist_Insert;

-------------------------------------------------------------------------------
-- PROCEDURE SYNC_POSTED_FLAGS added for Bug 13894680 
-- The purpose of this procedure is to update the incorrect posted flags on the
-- invoice distributions table and prepay history table based on the event status
-- of the prepayment application/unapplication event before apad is deleted and 
-- recreated
--
--------------------------------------------------------------------------------

PROCEDURE Sync_Posted_Flags
         (P_invoice_id           IN   NUMBER,
          P_Calling_Sequence     IN   VARCHAR2)
         IS
   
   l_curr_calling_sequence    VARCHAR2(2000);

  -- Logging Infra:
  l_procedure_name              CONSTANT VARCHAR2(30) := 'SYNC_POSTED_FLAGS';
  l_log_msg                     FND_LOG_MESSAGES.MESSAGE_TEXT%TYPE;
  l_mismatch_exists             INT := 0;
  l_rowcount                    NUMBER;
BEGIN

  l_curr_calling_sequence := 'AP_Acctg_Prepay_Dist_Pkg.Sync_Posted_flags<- ' ||
                                      p_calling_sequence;
  
  -- Logging Infra: Procedure level
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN                                     
    l_log_msg := 'Begin of procedure SYNC_POSTED_FLAGS';
    FND_LOG.STRING(G_LEVEL_PROCEDURE,G_MODULE_NAME || l_procedure_name, l_log_msg);
  END IF;  

  SELECT 1 INTO l_mismatch_exists
  FROM DUAL
  WHERE EXISTS ( SELECT 'Out of sync posted_flag in aid'
                 FROM   ap_invoice_distributions_all aid,
                        xla_events xe
                 WHERE aid.posted_flag <> 'Y'
                 AND aid.prepay_distribution_id IS NOT NULL
                 AND aid.line_type_lookup_code IN ('PREPAY','REC_TAX','NONREC_TAX')
                 AND xe.application_id = 200
                 AND xe.event_id = aid.accounting_event_id
                 AND xe.event_type_code IN ('PREPAYMENT APPLIED', 'PREPAYMENT UNAPPLIED')
                 AND xe.event_status_code = 'P'
                 AND xe.process_status_code = 'P'
                 AND aid.invoice_id = P_invoice_id
                 UNION
                 SELECT 'Out of sync posted_flag in aph'
                 FROM   ap_prepay_history_all aph,
                        xla_events xe
                 WHERE aph.posted_flag <> 'Y'
                 AND xe.application_id = 200
                 AND xe.event_id = aph.accounting_event_id
                 AND xe.event_type_code IN ('PREPAYMENT APPLIED', 'PREPAYMENT UNAPPLIED','PREPAYMENT APPLICATION ADJ')
                 AND xe.event_status_code = 'P'
                 AND xe.process_status_code = 'P'
                 AND aph.invoice_id = P_invoice_id);

  IF l_mismatch_exists = 1 THEN
    BEGIN
      IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
        l_log_msg := 'Mismatch exists in SYNC_POSTED_FLAGS';
        FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name, l_log_msg);
      END IF;  
      
      UPDATE ap_invoice_distributions_all aid
      SET (aid.posted_flag,
           aid.accrual_posted_flag,
           aid.cash_posted_flag) = (SELECT 'Y',
                                    decode(gsob.sla_ledger_cash_basis_flag, 'Y','N','Y'),
                                    decode(gsob.sla_ledger_cash_basis_flag, 'Y','Y','N')
                                    FROM gl_ledgers gsob
                                    WHERE gsob.ledger_id = aid.set_of_books_id)
      WHERE aid.invoice_id = P_invoice_id
      AND aid.posted_flag <> 'Y'
      AND aid.line_type_lookup_code IN ('PREPAY','REC_TAX','NONREC_TAX')
      AND aid.prepay_distribution_id IS NOT NULL
      AND EXISTS (SELECT 1
                  FROM xla_events xe
                  WHERE xe.application_id = 200
                  AND xe.event_status_code = 'P'
                  AND xe.process_status_code = 'P'
                  AND xe.event_id = aid.accounting_event_id);
      
      l_rowcount := SQL%ROWCOUNT; 
      
      IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN            
        l_log_msg := 'Number or records in ap invoice distributions whose posted flags are updated are ' ||l_rowcount ;
        FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name, l_log_msg);
      END IF;  

      UPDATE ap_prepay_history_all aph
      SET aph.posted_flag = 'Y'
      WHERE aph.invoice_id = P_invoice_id
      AND aph.posted_flag <> 'Y'
      AND EXISTS (SELECT 1
                  FROM xla_events xe
                  WHERE xe.application_id = 200
                  AND xe.event_status_code = 'P'
                  AND xe.process_status_code = 'P'
                  AND xe.event_id = aph.accounting_event_id);
                  
      l_rowcount := SQL%ROWCOUNT; 
      
      IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN            
        l_log_msg := 'Number or records in ap prepay history whose posted flags are updated are ' || l_rowcount;
        FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name, l_log_msg);
      END IF;  

    END;
    END IF;
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN  
    l_log_msg := 'End of procedure SYNC_POSTED_FLAGS';
    FND_LOG.STRING(G_LEVEL_PROCEDURE,G_MODULE_NAME || l_procedure_name, l_log_msg);
  END IF;  
  EXCEPTION
     WHEN OTHERS THEN
       IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
         l_log_msg := 'Encountered an exception '||SQLCODE ||'-error '|| SQLERRM ||' in call'|| P_Calling_Sequence;
         FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name, l_log_msg);
       END IF;  

END Sync_Posted_Flags;

--Bug5373620 Added following procedure
-------------------------------------------------------------------------------
-- PROCEDURE Delete_Hist_Dists
-- Procedure to delete the Prepay history distributions and prepayment
-- application distributions.
--
--
-- bug9038462, rewrote the DELETE statements in the procedure to make
-- sure of the regeneration of the Prepayment Application distributions
-- if the corresponding Invoice distribution for prepayment application
-- has not been posted or encumbered
--

--------------------------------------------------------------------------------
PROCEDURE Delete_Hist_Dists
     (P_invoice_id           IN   NUMBER,
      P_Calling_Sequence     IN   VARCHAR2
     ) IS

  l_curr_calling_sequence    VARCHAR2(2000);

  -- Logging Infra:
  l_procedure_name CONSTANT VARCHAR2(30) := 'Delete_Hist_Dists';
  l_log_msg        FND_LOG_MESSAGES.MESSAGE_TEXT%TYPE;

BEGIN

  l_curr_calling_sequence := 'AP_Acctg_Prepay_Dist_Pkg.Delete_hist_dists<- ' ||
                                      p_calling_sequence;

  -- Logging Infra: Procedure level
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
      l_log_msg := 'Begin of procedure '|| l_procedure_name;
      FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name||'.begin', l_log_msg);
  END IF;

  -- Bug fix 5634515
  -- rewrite the query to delete the correct prepay application dist record.

  -- delete from AP_Prepay_history_all is placed after delete from AP_Prepay_App_Dists
  -- due to bug 7264479

  -- Bug fix 5634515
  -- rewrite the query to delete the correct prepay history record.

  -- Logging Infra: Procedure level
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
      l_log_msg := 'Begin of procedure '|| l_procedure_name;
      FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name||'.begin', l_log_msg);
  END IF;
  
  -- Call added in Bug 13894680 to sync posted flags before apad is deleted and recreated
  Sync_Posted_Flags(P_invoice_id,
                    l_curr_calling_sequence);

  --  bug9038462, the previous bug tags have been retained, but for the sake
  -- of code cleanliness, I am removing the old sqls used for deletions. Please
  -- refer to the prior versions if changes have to be compared
  -- 
  DELETE FROM ap_prepay_app_dists apad1
   WHERE apad1.prepay_history_id IN
      (SELECT apph.prepay_history_id
         FROM ap_prepay_history_all apph
        WHERE nvl(apph.posted_flag, 'N') <> 'Y'
          AND apph.invoice_id = p_invoice_id
          AND apph.transaction_type <> 'PREPAYMENT APPLICATION ADJ'  --bug9973070
          AND NOT EXISTS
              (SELECT  /*+ no_unnest */ 1                       --bug12337556
                 FROM ap_prepay_app_dists apad,
                      ap_invoice_distributions_all aid
                WHERE apad.prepay_history_id = apph.prepay_history_id
                  AND apad.prepay_app_distribution_id = aid.invoice_distribution_id
                  AND (aid.posted_flag = 'Y' OR aid.encumbered_flag = 'Y')));

  DELETE FROM ap_prepay_history_all apph1
   WHERE apph1.prepay_history_id IN
      (SELECT apph.prepay_history_id
         FROM ap_prepay_history_all apph
        WHERE nvl(apph.posted_flag, 'N') <> 'Y'
          AND apph.invoice_id = p_invoice_id
          AND apph.transaction_type <> 'PREPAYMENT APPLICATION ADJ'  --bug9973070
          AND NOT EXISTS
              (SELECT  /*+ no_unnest */ 1                       --bug12337556
                 FROM ap_prepay_app_dists apad,
                      ap_invoice_distributions_all aid
                WHERE apad.prepay_history_id = apph.prepay_history_id
                  AND apad.prepay_app_distribution_id = aid.invoice_distribution_id
                  AND (aid.posted_flag = 'Y' OR aid.encumbered_flag = 'Y')));


EXCEPTION

  WHEN OTHERS THEN
    IF (SQLCODE <> -20001) THEN
      FND_MESSAGE.SET_NAME('SQLAP','AP_DEBUG');
      FND_MESSAGE.SET_TOKEN('ERROR',SQLERRM);
      FND_MESSAGE.SET_TOKEN('CALLING_SEQUENCE', l_curr_calling_sequence);
    END IF;
    APP_EXCEPTION.RAISE_EXCEPTION;

END Delete_Hist_Dists;



-- 9322009, added the following procedure to recreate the incorrect dist
-- links for the upgraded prepayment application events, so the the
-- prepayment unapplication for the same created in R12 can get successfully
-- accounted
--
PROCEDURE Upg_Dist_Links_Insert
           (P_Invoice_ID          IN  NUMBER
           ,p_prepay_history_id   IN  NUMBER
           ,p_accounting_event_id IN  NUMBER
           ,p_calling_sequence    IN  VARCHAR2
           ) IS

  l_rowcount                   NUMBER;
  l_curr_calling_sequence      VARCHAR2(2000);

  -- Logging Infra:
  l_procedure_name CONSTANT VARCHAR2(30) := 'Upg_Dist_Links_Insert';
  l_log_msg        FND_LOG_MESSAGES.MESSAGE_TEXT%TYPE;

  /* Bug 18242419 starts */
  l_ae_header_id                XLA_AE_HEADERS.ae_header_id%TYPE;
  l_ae_line_num                 XLA_AE_LINES.ae_line_num%TYPE;
  l_accounting_class_code       XLA_AE_LINES.accounting_class_code%TYPE;
  l_accounted_dr                XLA_AE_LINES.accounted_dr%TYPE;
  l_accounted_cr                XLA_AE_LINES.accounted_cr%TYPE;
  l_account_overlay_source_id   XLA_AE_LINES.account_overlay_source_id%TYPE;
  l_upg_batch_id                XLA_AE_LINES.upg_batch_id%TYPE;
  l_temp_line_num               XLA_DISTRIBUTION_LINKS.temp_line_num%TYPE;
  l_source_dist_id              XLA_DISTRIBUTION_LINKS.source_distribution_id_num_1%TYPE;

  CURSOR rounding_lines(p_event_id NUMBER)
      IS
  SELECT xal.ae_header_id,
         xal.ae_line_num,
         xal.accounting_class_code,
         xal.accounted_dr,
         xal.accounted_cr,
         xal.account_overlay_source_id,
         xal.upg_batch_id
    FROM xla_ae_lines xal,
         xla_ae_headers xah
   WHERE xal.application_id = 200
     AND xah.application_id = 200
     AND xah.ae_header_id = xal.ae_header_id
     AND xal.accounting_class_code = 'ROUNDING'
     AND xah.event_id = p_event_id
     AND NOT EXISTS (SELECT 'Xdl for rounding line'
                       FROM  xla_distribution_links xdl
                      WHERE xdl.application_id = 200
                        AND xdl.ae_header_id = xah.ae_header_id
                        AND xdl.ae_line_num = xal.ae_line_num);
   /* Bug 18242419 ends */  

BEGIN

  l_curr_calling_sequence := 'AP_ACCTG_PREPAY_DIST_PKG.Upg_Dist_Links_Insert<- ' ||
                                     P_Calling_Sequence;


  -- Logging Infra: Procedure level
  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
      l_log_msg := 'Begin of procedure '|| l_procedure_name;
      FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name||
                      '.begin', l_log_msg);
  END IF;

  IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
      l_log_msg := 'Deleting xla_distribution_links';
      FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name, l_log_msg);
  END IF;

  IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
      l_log_msg := ' Printing the details of the parameters '||
                   ' P_Invoice_ID : '||P_Invoice_ID||
                   ' P_Prepay_History_ID : '||P_Prepay_History_ID||
                   ' P_Accounting_Event_ID : '||P_Accounting_Event_ID;
      FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name, l_log_msg);
  END IF;

  DELETE FROM xla_distribution_links
  WHERE  application_id = 200
  AND    ae_header_id IN
              (SELECT ae_header_id
               FROM   xla_ae_headers aeh,
                      ap_prepay_history_all aph
               WHERE  aeh.event_id = aph.accounting_event_id
               AND    aph.accounting_event_id = p_accounting_event_id
               AND    aph.invoice_id = p_invoice_id
               AND    aph.historical_flag = 'Y'
               AND    aeh.upg_batch_id IS NOT NULL
               AND    aeh.upg_batch_id <> -9999)
  AND    upg_batch_id IS NOT NULL
  AND    upg_batch_id <> -9999;

  l_rowcount := SQL%ROWCOUNT;

  IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
      l_log_msg := ' Number of XLA distribution LInks Deleted :'||l_rowcount||
                   ' Now Inserting xla_distribution_links for event '||
                     p_accounting_event_id;
      FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name, l_log_msg);
  END IF;


INSERT INTO XLA_Distribution_Links t1
          (APPLICATION_ID,
           EVENT_ID,
           AE_HEADER_ID,
           AE_LINE_NUM,
           SOURCE_DISTRIBUTION_TYPE,
           SOURCE_DISTRIBUTION_ID_NUM_1,
           STATISTICAL_AMOUNT,
           UNROUNDED_ENTERED_CR,
           UNROUNDED_ENTERED_DR,
           UNROUNDED_ACCOUNTED_CR,
           UNROUNDED_ACCOUNTED_DR,
           REF_AE_HEADER_ID,
           ACCOUNTING_LINE_CODE,
           ACCOUNTING_LINE_TYPE_CODE,
           MERGE_DUPLICATE_CODE,
           TEMP_LINE_NUM,
           REF_EVENT_ID,
           UPG_BATCH_ID,
           LINE_DEFINITION_OWNER_CODE,
           LINE_DEFINITION_CODE,
           EVENT_CLASS_CODE,
           EVENT_TYPE_CODE,
           APPLIED_TO_APPLICATION_ID,
           APPLIED_TO_ENTITY_ID,
           APPLIED_TO_DIST_ID_NUM_1,
           GAIN_OR_LOSS_REF )
SELECT Application_ID,
           Accounting_Event_ID,
           AE_Header_ID,
           AE_Line_Num,
           Source_Distribution_Type,
           Source_Distribution_ID_Num_1,
           NULL Statistical_Amount,
           (CASE
             WHEN Line_Entered_Cr IS NOT NULL THEN
                Decode(Rank_Num, Dist_Count, (Line_Entered_Amt - Sum_Entered_Amt) +
                            Entered_Amt, Entered_Amt)
             ELSE NULL
            END),
           (CASE
             WHEN Line_Entered_Dr IS NOT NULL THEN
                Decode(Rank_Num, Dist_Count, (Line_Entered_Amt - Sum_Entered_Amt) +
                            Entered_Amt, Entered_Amt)
             ELSE NULL
            END),
           (CASE
             WHEN Line_Accounted_Cr IS NOT NULL THEN
                  Decode(Rank_Num, Dist_Count, (Line_Accounted_Amt - Sum_Accounted_Amt) +
                            Accounted_Amt, Accounted_Amt)
             ELSE NULL
            END),
           (CASE
             WHEN Line_Accounted_Dr IS NOT NULL THEN
                  Decode(Rank_Num, Dist_Count, (Line_Accounted_Amt - Sum_Accounted_Amt) +
                            Accounted_Amt, Accounted_Amt)
             ELSE NULL
            END),
           Ref_AE_Header_ID,
           Accounting_Line_Code,
           Accounting_Line_Type_Code,
           Merge_Duplicate_Code,
           Temp_Line_Num,
           Ref_Event_ID,
           Upg_Batch_ID,
           Line_Definition_Owner_Code,
           Line_Definition_Code,
           Event_Class_Code,
           Event_Type_Code,
           APPLIED_TO_APPLICATION_ID,
           APPLIED_TO_ENTITY_ID,
           APPLIED_TO_DIST_ID_NUM_1,
           GAIN_OR_LOSS_REF
    FROM
    (SELECT  Application_ID,
          Accounting_Event_ID,
          AE_Header_ID,
          AE_Line_Num,
          Source_Distribution_Type,
          Source_Distribution_ID_Num_1,
          Statistical_Amount,
          Accounting_Line_Code,
          Accounting_Line_Type_Code,
          Merge_Duplicate_Code,
          Line_Entered_Cr,
          Line_Entered_Dr,
          Line_Accounted_Cr,
          Line_Accounted_Dr,
          Line_Entered_Amt,
          Line_Accounted_Amt,
          Dist_Count,
          Ref_AE_Header_ID,
          Temp_Line_Num,
          Ref_Event_ID,
          Upg_Batch_ID,
          Line_Definition_Owner_Code,
          Line_Definition_Code,
          Event_Class_Code,
          Event_Type_Code,
          APPLIED_TO_APPLICATION_ID,
          APPLIED_TO_ENTITY_ID,
          APPLIED_TO_DIST_ID_NUM_1,
          GAIN_OR_LOSS_REF,
          Rank_Num,
          DECODE(FC.Minimum_Accountable_Unit, NULL,
                 ROUND((Line_Accounted_Amt * Dist_Base_Amount
                       /DECODE(PDivisor_Acct_Amt, 0, 1, PDivisor_Acct_Amt)),
                        FC.Precision),
                  ROUND((Line_Accounted_Amt * Dist_Base_Amount
                        / DECODE(PDivisor_Acct_Amt, 0, 1, PDivisor_Acct_Amt))
                 /FC.Minimum_Accountable_Unit) * FC.Minimum_Accountable_Unit) Accounted_Amt,
           DECODE(FC.Minimum_Accountable_Unit, NULL,
              ROUND((Line_Entered_Amt * Dist_Amount
                    / DECODE(PDivisor_Ent_Amt, 0 ,1, PDivisor_Ent_Amt)), FC.Precision),
              ROUND((Line_Entered_Amt * Dist_Amount
                    / DECODE(PDivisor_Acct_Amt, 0 ,1, PDivisor_Ent_Amt))
                /FC.Minimum_Accountable_Unit) * FC.Minimum_Accountable_Unit) Entered_Amt,
           SUM(DECODE(FC.Minimum_Accountable_Unit, NULL,
              ROUND((Line_Accounted_Amt * Dist_Base_Amount
                     / DECODE(PDivisor_Acct_Amt, 0, 1, PDivisor_Acct_Amt)),
                     FC.Precision),
              ROUND((Line_Accounted_Amt * Dist_Base_Amount
                     / DECODE(PDivisor_Acct_Amt, 0, 1, PDivisor_Acct_Amt))
                /FC.Minimum_Accountable_Unit) * FC.Minimum_Accountable_Unit))
              OVER (PARTITION BY Invoice_Id, Part_Key1, Part_Key2, AE_Line_Num)
                   Sum_Accounted_Amt,
           SUM(DECODE(FC.Minimum_Accountable_Unit, NULL,
                ROUND((Line_Entered_Amt * Dist_Amount
                    / DECODE(PDivisor_Ent_Amt, 0 ,1, PDivisor_Ent_Amt)), FC.Precision),
                ROUND((Line_Entered_Amt * Dist_Amount
                    / DECODE(PDivisor_Ent_Amt, 0 ,1, PDivisor_Ent_Amt))
                 /FC.Minimum_Accountable_Unit) * FC.Minimum_Accountable_Unit))
              OVER (PARTITION BY Invoice_Id, Part_Key1, Part_Key2, AE_Line_Num) Sum_Entered_Amt
     FROM( /*Bug 10016633 Added another wrapper query*/
      SELECT Application_ID,
         Invoice_Id,
         Base_Currency_Code,
         Accounting_Event_ID,
         AE_Header_ID,
         AE_Line_Num,
         Source_Distribution_Type,
         Source_Distribution_ID_Num_1,
         Statistical_Amount,
         Unrounded_Entered_Cr,
         Unrounded_Entered_Dr,
         Unrounded_Accounted_Cr,
         Unrounded_Accounted_Dr,
         Ref_AE_Header_ID,
         Accounting_Line_Code,
         Accounting_Line_Type_Code,
         Merge_Duplicate_Code,
         Line_Entered_Cr,
         Line_Entered_Dr,
         Line_Accounted_Cr,
         Line_Accounted_Dr,
         Line_Entered_Amt,
         Line_Accounted_Amt,
         Dist_Amount,
         Dist_Base_Amount,
         Dist_Count,
         PDivisor_Ent_Amt,
         PDivisor_Acct_Amt,
         Part_Key1,
         Part_Key2,
         /*Bug10016633 Moved temp_line_num logic here
         and using ROW_NUMBER() instead of RANK()*/
         ROW_NUMBER() OVER (PARTITION BY Invoice_ID,
                                 AE_Header_Id
                        ORDER BY  AE_Line_Num,
                                  Invoice_Distribution_ID,
                                  Source_Distribution_ID_Num_1,
                                  Prepay_Dist_Lookup_Code) Temp_Line_Num,
          Rank_Num,
          Ref_Event_ID,
          Upg_Batch_ID,
          Line_Definition_Owner_Code,
          Line_Definition_Code,
          Event_Class_Code,
          Event_Type_Code,
          APPLIED_TO_APPLICATION_ID,
          APPLIED_TO_ENTITY_ID,
          APPLIED_TO_DIST_ID_NUM_1,
          GAIN_OR_LOSS_REF
    FROM
     (
      SELECT 200 Application_ID,
           AI.Invoice_Id Invoice_Id,
           ASP.Base_Currency_Code Base_Currency_Code,
           AEH.Event_ID Accounting_Event_ID,
           AEH.AE_Header_ID AE_Header_ID,
           AEL.AE_Line_Num AE_Line_Num,
           'AP_PREPAY'  Source_Distribution_Type,
           APAD.Prepay_App_Dist_ID Source_Distribution_ID_Num_1,
           0 Statistical_Amount,
           DECODE(SIGN(APAD.Amount), 1, APAD.Amount, NULL) Unrounded_Entered_Cr,
           DECODE(SIGN(APAD.Amount),-1, APAD.Amount, NULL) Unrounded_Entered_Dr,
           DECODE(SIGN(APAD.Base_Amount), 1, APAD.Base_Amount, NULL) Unrounded_Accounted_Cr,
           DECODE(SIGN(APAD.Base_Amount),-1, APAD.Base_Amount, NULL) Unrounded_Accounted_Dr,
           AEH.AE_Header_ID Ref_AE_Header_ID,
           DECODE(AEL.Accounting_Class_Code,
                  'GAIN',           'AP_GAIN_PREPAY_APP',
                  'LOSS',           'AP_LOSS_PREPAY_APP',
                  'LIABILITY',      'AP_LIAB_PREPAY_APP',
                  'PREPAID_EXPENSE','AP_PREPAID_EXP_ACCR_PREPAY_APP',
                  'ROUNDING',       'AP_FINAL_PMT_ROUND_PREPAY_APP',
                  'NRTAX',          'AP_NRTAX_PREPAY_PAY_RATE_APP',
                  'RTAX',           'AP_RECOV_PREPAY_PAY_RATE_APP',
                  'ACCRUAL',        'AP_ACCR_PREPAY_PAY_RATE_APP',
                  'ITEM EXPENSE',   'AP_ITEM_PREPAY_PAY_RATE_APP',
                  'EXCHANGE_RATE_VARIANCE', 'AP_EX_RATE_VAR_PREPAY_PAY_RATE',
                  'IPV',            'AP_IPV_PREPAY_PAY_RATE_APP',
                  'NRTAX',          'AP_NRTAX_PREPAY_PAY_RATE_APP',
                  'RTAX',           'AP_RECOV_PREPAY_PAY_RATE_APP',
                  'FREIGHT',        'AP_FREIGHT_PREPAY_PAY_RATE_APP',
                  'AP_ITEM_PREPAY_PAY_RATE_APP')
                  Accounting_Line_Code,
           'S' Accounting_Line_Type_Code,
           'A' Merge_Duplicate_Code,
           AEL.Entered_Cr Line_Entered_Cr,
           AEL.Entered_Dr Line_Entered_Dr,
           AEL.Accounted_Cr Line_Accounted_Cr,
           AEL.Accounted_Dr Line_Accounted_Dr,
           NVL(AEL.Entered_Cr, AEL.Entered_Dr) Line_Entered_Amt,
           NVL(AEL.Accounted_Cr, AEL.Accounted_Dr) Line_Accounted_Amt,
           AID.Amount Dist_Amount,
           NVL(AID.Base_Amount, AID.Amount) Dist_Base_Amount,
           COUNT(*) OVER (PARTITION BY AI.Invoice_ID,
                                       AEH.AE_Header_Id,
                                       AEL.AE_Line_Num) Dist_Count,
          /* bug 12845564 - start */
          SUM(AID.Amount)
                  OVER (PARTITION BY AI.Invoice_ID,
                                     AEH.ae_header_id,
                                     AEL.AE_Line_Num,
                                     AEL.Account_Overlay_Source_ID) PDivisor_Ent_Amt,
           SUM(NVL(AID.Base_Amount, AID.Amount))
                  OVER (PARTITION BY AI.Invoice_ID,
                                     AEH.ae_header_id,
                                     AEL.AE_Line_Num,
                                    AEL.Account_Overlay_Source_ID) PDivisor_Acct_Amt,
          /* bug 12845564 - end */
           AI.Invoice_ID Part_Key1,
           NVL(AID.old_distribution_id, AID.Invoice_Distribution_ID) Part_Key2, -- bug 12845564
           RANK() OVER (PARTITION BY AI.Invoice_ID,
                                     AEH.AE_Header_Id,
                                     AEL.AE_Line_Num
                        ORDER BY  AEL.AE_Line_Num,
                                  APAD.Invoice_Distribution_ID,
                                  APAD.Prepay_App_Distribution_ID,
                                  APAD.Prepay_Dist_Lookup_Code) Rank_Num,
           AEH.Event_ID Ref_Event_ID,
           AEL.Upg_Batch_ID,
           'S' Line_Definition_Owner_Code,
           'ACCRUAL_INVOICES_ALL' Line_Definition_Code,
           'INVOICES' Event_Class_Code,
           'INVOICES_ALL' Event_Type_Code,
           DECODE(AEL.Accounting_Class_Code, 'LIABILITY' ,200, null) APPLIED_TO_APPLICATION_ID,
           DECODE(AEL.Accounting_Class_Code, 'LIABILITY' ,XTE.Entity_ID, null) APPLIED_TO_ENTITY_ID,
           DECODE(AEL.Accounting_Class_Code, 'LIABILITY' ,AID.Invoice_Distribution_ID, null) APPLIED_TO_DIST_ID_NUM_1,
           '-2222' GAIN_OR_LOSS_REF,
           APAD.Invoice_Distribution_ID,
           APAD.Prepay_Dist_Lookup_Code
    FROM   AP_Invoices_All AI,
           AP_System_Parameters_All ASP,
           XLA_Transaction_Entities_upg XTE,
           XLA_Events XLE,
           AP_Prepay_App_Dists APAD,
           AP_Invoice_Distributions_All AID,
           XLA_AE_Headers AEH,
           XLA_AE_Lines AEL
    WHERE  XLE.event_id = p_accounting_event_id
    AND    AI.Org_Id = ASP.Org_Id
    AND    AI.Invoice_ID = AID.Invoice_ID
    AND    XTE.Application_ID = 200
    AND    AI.Set_Of_Books_ID = XTE.Ledger_ID
    AND    XTE.Entity_Code = 'AP_INVOICES'
    AND    AI.Invoice_ID = NVL(XTE.Source_ID_Int_1,-99)
    AND    XTE.Entity_ID = XLE.Entity_ID
    AND    XLE.Application_ID = 200
    AND    XLE.Event_Type_Code IN ('PREPAYMENT APPLIED',
                                   'PREPAYMENT UNAPPLIED')
    AND    XLE.Event_ID = AEH.Event_ID
    AND    AEH.Application_ID = 200
    AND    AEL.AE_Header_ID = AEH.AE_Header_ID
    AND    AEL.Application_ID = 200
    AND    XLE.Event_ID = APAD.Accounting_Event_ID
    AND    APAD.Invoice_Distribution_ID = AID.Invoice_Distribution_ID
    AND    AEL.Account_Overlay_Source_ID IS NOT NULL
    AND    AID.Old_Distribution_ID = AEL.Account_Overlay_Source_ID
    UNION
    SELECT   200 Application_ID,
           AI.Invoice_id Invoice_Id,
           ASP.Base_Currency_Code Base_Currency_Code,
           AEH.Event_ID Accounting_Event_ID,
           AEH.AE_Header_ID AE_Header_ID,
           AEL.AE_Line_Num AE_Line_Num,
           'AP_PREPAY'  Source_Distribution_Type,
           APAD.Prepay_App_Dist_ID Source_Distribution_ID_Num_1,
           0 Statistical_Amount,
           DECODE(SIGN(APAD.Amount), 1, APAD.Amount, NULL) Unrounded_Entered_Cr,
           DECODE(SIGN(APAD.Amount),-1, APAD.Amount, NULL) Unrounded_Entered_Dr,
           DECODE(SIGN(APAD.Base_Amount), 1, APAD.Base_Amount, NULL) Unrounded_Accounted_Cr,
           DECODE(SIGN(APAD.Base_Amount),-1, APAD.Base_Amount, NULL) Unrounded_Accounted_Dr,
           AEH.AE_Header_ID Ref_AE_Header_ID,
           DECODE(AEL.Accounting_Class_Code,
                  'GAIN',           'AP_GAIN_PREPAY_APP',
                  'LOSS',           'AP_LOSS_PREPAY_APP',
                  'LIABILITY',      'AP_LIAB_PREPAY_APP',
                  'PREPAID_EXPENSE','AP_PREPAID_EXP_ACCR_PREPAY_APP',
                  'ROUNDING',       'AP_FINAL_PMT_ROUND_PREPAY_APP',
                  'NRTAX',          'AP_NRTAX_PREPAY_PAY_RATE_APP',
                  'RTAX',           'AP_RECOV_PREPAY_PAY_RATE_APP',
                  'ACCRUAL',        'AP_ACCR_PREPAY_PAY_RATE_APP',
                  'ITEM EXPENSE',   'AP_ITEM_PREPAY_PAY_RATE_APP',
                  'EXCHANGE_RATE_VARIANCE', 'AP_EX_RATE_VAR_PREPAY_PAY_RATE',
                  'IPV',            'AP_IPV_PREPAY_PAY_RATE_APP',
                  'NRTAX',          'AP_NRTAX_PREPAY_PAY_RATE_APP',
                  'RTAX',           'AP_RECOV_PREPAY_PAY_RATE_APP',
                  'FREIGHT',        'AP_FREIGHT_PREPAY_PAY_RATE_APP',
                  'AP_ITEM_PREPAY_PAY_RATE_APP')
                  Accounting_Line_Code,
           'S' Accounting_Line_Type_Code,
           'A' Merge_Duplicate_Code,
           AEL.Entered_Cr Line_Entered_Cr,
           AEL.Entered_Dr Line_Entered_Dr,
           AEL.Accounted_Cr Line_Accounted_Cr,
           AEL.Accounted_Dr Line_Accounted_Dr,
           NVL(AEL.Entered_Cr, AEL.Entered_Dr) Line_Entered_Amt,
           NVL(AEL.Accounted_Cr, AEL.Accounted_Dr) Line_Accounted_Amt,
           AID.Amount Dist_Amount,
           NVL(AID.Base_Amount, AID.Amount) Dist_Base_Amount,
           COUNT(*) OVER (PARTITION BY AI.Invoice_ID,
                                       AEH.AE_Header_Id,
                                       AEL.AE_Line_Num) Dist_Count,
           SUM(AID.Amount)
                  OVER (PARTITION BY AI.Invoice_ID,
                                     AEH.ae_header_id,
                                     AEL.AE_Line_Num) PDivisor_Ent_Amt,
           SUM(NVL(AID.Base_Amount, AID.Amount))
                  OVER (PARTITION BY AI.Invoice_ID,
                                     AEH.ae_header_id,
                                     AEL.AE_Line_Num) PDivisor_Acct_Amt,
           AI.Invoice_ID Part_Key1,
           1 Part_Key2,
           RANK() OVER (PARTITION BY AI.Invoice_ID,
                                     AEH.AE_Header_Id,
                                     AEL.AE_Line_Num
                        ORDER BY  AEL.AE_Line_Num,
                                  APAD.Invoice_Distribution_ID,
                                  APAD.Prepay_App_Distribution_ID,
                                  APAD.Prepay_Dist_Lookup_Code) Rank_Num,
           AEH.Event_ID Ref_Event_ID,
           AEL.Upg_Batch_ID,
           'S' Line_Definition_Owner_Code,
           'ACCRUAL_INVOICES_ALL' Line_Definition_Code,
           'INVOICES' Event_Class_Code,
           'INVOICES_ALL' Event_Type_Code,
           DECODE(AEL.Accounting_Class_Code, 'LIABILITY' ,200, null) APPLIED_TO_APPLICATION_ID,
           DECODE(AEL.Accounting_Class_Code, 'LIABILITY' ,XTE.Entity_ID, null) APPLIED_TO_ENTITY_ID,
           DECODE(AEL.Accounting_Class_Code, 'LIABILITY' ,AID.Invoice_Distribution_ID, null)
                                                  APPLIED_TO_DIST_ID_NUM_1,
           '-2222' GAIN_OR_LOSS_REF,
                   APAD.Invoice_Distribution_ID,
           APAD.Prepay_Dist_Lookup_Code
    FROM   AP_Invoices_All AI,
           AP_System_Parameters_All ASP,
           XLA_Transaction_Entities_upg XTE,
           XLA_Events XLE,
           AP_Prepay_App_Dists APAD,
           AP_Invoice_Distributions_All AID,
           XLA_AE_Headers AEH,
           XLA_AE_Lines AEL
    WHERE  XLE.event_id = p_accounting_event_id
    AND    AI.Org_Id = ASP.Org_id
    AND    AI.Invoice_ID = AID.Invoice_ID
    AND    XTE.Application_ID = 200
    AND    AI.Set_Of_Books_ID = XTE.Ledger_ID
    AND    XTE.Entity_Code = 'AP_INVOICES'
    AND    AI.Invoice_ID = NVL(XTE.Source_ID_Int_1,-99)
    AND    XTE.Entity_ID = XLE.Entity_ID
    AND    XLE.Application_ID = 200
    AND    XLE.Event_Type_Code IN ('PREPAYMENT APPLIED',
                                   'PREPAYMENT UNAPPLIED')
    AND    XLE.Event_ID = AEH.Event_ID
    AND    AEH.Application_ID = 200
    AND    AEL.AE_Header_ID = AEH.AE_Header_ID
    AND    AEL.Application_ID = 200
    AND    XLE.Event_ID = APAD.Accounting_Event_ID
    AND    APAD.Invoice_Distribution_ID = AID.Invoice_Distribution_ID
    AND    AEL.Account_Overlay_Source_ID IS NULL)) v1,
     Fnd_Currencies FC
    WHERE  FC.Currency_Code = v1.Base_Currency_Code) v2;

  l_rowcount := SQL%ROWCOUNT;

   IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
    null;
   END IF;

  
  IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
      l_log_msg := 'Number of records Inserted in XDL '||l_rowcount;
      FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name, l_log_msg);
  END IF;

  /*Bug 18242419 starts*/
  IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
    l_log_msg := 'Before opening the cursor for inserting '||
                 'XLA_DISTRIBUTION_LINKS for ROUNDING lines';
    FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name, l_log_msg);
  END IF;
 
  OPEN rounding_lines(p_accounting_event_id);
    LOOP
    FETCH rounding_lines INTO l_ae_header_id,
                                l_ae_line_num,
                                l_accounting_class_code,
                                l_accounted_dr,
                                l_accounted_cr,
                                l_account_overlay_source_id,
                                l_upg_batch_id;
    EXIT WHEN rounding_lines%NOTFOUND;

    

      l_log_msg := 'Fetched rounding line for Event_id : '||p_accounting_event_id||
                   ' ae_header_id : '||L_AE_HEADER_ID||' ae_line_num: '||l_ae_line_num||
                   ' accounting_class_code: '||l_accounting_class_code||
                   ' accounted_dr : '||l_accounted_dr||
                   ' accounted_cr : '||L_ACCOUNTED_CR||
                   ' account_overlay_source_id : '||l_account_overlay_source_id;
                   
    IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN               
      FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name, l_log_msg);
    END IF;
   
    IF l_account_overlay_source_id IS NOT NULL  
    THEN
      SELECT max(apad.prepay_app_dist_id)
        INTO l_source_dist_id
        FROM ap_prepay_app_dists apad,
             ap_invoice_distributions_all aid
       WHERE apad.accounting_event_id = p_accounting_event_id
         AND aid.old_distribution_id = l_account_overlay_source_id
         AND apad.prepay_app_distribution_id = aid.invoice_distribution_id
         AND apad.prepay_dist_lookup_code IN ('PREPAY APPL',
                                              'PREPAY APPL REC TAX',
                                               'PREPAY APPL NONREC TAX');
    ELSE
      SELECT max(apad.prepay_app_dist_id)
        INTO l_source_dist_id
        FROM ap_prepay_app_dists apad
       WHERE apad.accounting_event_id = p_accounting_event_id                       
         AND apad.prepay_dist_lookup_code IN ('PREPAY APPL',
                                              'PREPAY APPL REC TAX',
                                              'PREPAY APPL NONREC TAX');              
    END IF;
                                                  
    IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
      l_log_msg := 'Fetched the source_distribution_id_num_1 against which we '||
                   'will make the Adjustment: '||l_source_dist_id;
      FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name, l_log_msg);
    END IF;
  
    SELECT (max(xdl.temp_line_num) + 1)
      INTO l_temp_line_num
      FROM xla_distribution_links xdl             
     WHERE xdl.application_id = 200
       AND xdl.ae_header_id = l_ae_header_id;
         
    IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
      l_log_msg := 'Fetched the next Temp Line Num for inserting Rounding XDL:'||
                   l_temp_line_num||' to avoid a U1 error when inserting xdl';
      FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name, l_log_msg);
    END IF; 
   
    INSERT INTO XLA_Distribution_Links t1
         (APPLICATION_ID,
          EVENT_ID,
          AE_HEADER_ID,
          AE_LINE_NUM,
          SOURCE_DISTRIBUTION_TYPE,
          SOURCE_DISTRIBUTION_ID_NUM_1,
          STATISTICAL_AMOUNT,
          UNROUNDED_ENTERED_CR,
          UNROUNDED_ENTERED_DR,
          UNROUNDED_ACCOUNTED_CR,
          UNROUNDED_ACCOUNTED_DR,
          REF_AE_HEADER_ID,
          ACCOUNTING_LINE_CODE,
          ACCOUNTING_LINE_TYPE_CODE,
          MERGE_DUPLICATE_CODE,
          TEMP_LINE_NUM,
          REF_EVENT_ID,
          UPG_BATCH_ID,
          LINE_DEFINITION_OWNER_CODE,
          LINE_DEFINITION_CODE,
          EVENT_CLASS_CODE,
          EVENT_TYPE_CODE,
          APPLIED_TO_APPLICATION_ID,
          APPLIED_TO_ENTITY_ID,
          APPLIED_TO_DIST_ID_NUM_1,
          GAIN_OR_LOSS_REF 
          )
    VALUES(200,                        --application_id
         p_accounting_event_id,                 --event_id
         l_ae_header_id,             --ae_header_id
         l_ae_line_num,              --ae_line_num,
         'AP_PREPAY',                --source_distribution_type
         l_source_dist_id,           --source_distribution_id_num_1
         NULL,                       --statistical_amount
         0,                          -- unrounded_entered_cr,
         0,                          --unrounded_entered_dr,
         l_accounted_cr,             --unrounded_accounted_cr,
         l_accounted_dr,             --unrounded_accounted_dr,
         l_ae_header_id,             --ref_ae_header_id
         'AP_FINAL_PMT_ROUND_PREPAY_APP', --accounting_line_code,
         'S',                        --accounting_line_type_code,
         'A',                        --merge_duplicate_code,
         l_temp_line_num,            --temp_line_num
         p_accounting_event_id,      --ref_event_id
         l_upg_batch_id,             --upg_batch_id
         'S',                        --line_definition_owner_code
         'ACCRUAL_INVOICES_ALL',     --line_definition_code
         'INVOICES',                 --event_class_code
         'INVOICES_ALL',             --Event_Type_Code
         NULL,                       --applied_to_application_id
         NULL,                       --applied_to_entity_id
         NULL,                       --applied_to_dist_id_num_1
         -2222                       --gain_or_loss_ref
         );

    IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
      l_rowcount := SQL%ROWCOUNT;  
      L_LOG_MSG := 'Number of rounding records inserted into XLA_DISTRIBUTION_LINKS: '
                   ||l_rowcount;
      FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name, l_log_msg);
    END IF; 
  
  END LOOP; --cursor rounding_lines
  CLOSE rounding_lines;
    
  IF (G_LEVEL_STATEMENT >= G_CURRENT_RUNTIME_LEVEL) THEN
    l_log_msg := 'Inserting xdl for rounding lines completed';
    FND_LOG.STRING(G_LEVEL_STATEMENT,G_MODULE_NAME || l_procedure_name, l_log_msg);
  END IF;   
  
  /* Bug 18242419 ends*/

  IF (G_LEVEL_PROCEDURE >= G_CURRENT_RUNTIME_LEVEL ) THEN
      l_log_msg := 'End of procedure '||l_procedure_name;
      FND_LOG.STRING(G_LEVEL_PROCEDURE, G_MODULE_NAME||l_procedure_name||'.end',l_log_msg);
  END IF;


EXCEPTION
  WHEN OTHERS THEN
    IF (SQLCODE <> -20001) THEN
      FND_MESSAGE.SET_NAME('SQLAP','AP_DEBUG');
      FND_MESSAGE.SET_TOKEN('ERROR',SQLERRM);
      FND_MESSAGE.SET_TOKEN('CALLING_SEQUENCE', l_curr_calling_sequence);
    END IF;
    APP_EXCEPTION.RAISE_EXCEPTION;

END Upg_Dist_Links_Insert;
END AP_ACCTG_PREPAY_DIST_PKG;
/

COMMIT;
EXIT;
