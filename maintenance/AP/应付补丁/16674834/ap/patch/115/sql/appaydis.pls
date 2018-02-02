REM +==================================================================+
REM |                Copyright (c) 1999, 2014 Oracle Corporation
REM |                   Redwood Shores, California, USA
REM |                        All rights reserved.
REM +==================================================================+
REM |  Name
REM |    appaydis.pls
REM |
REM |  Description - Package AP_ACCTG_PAY_DIST_PKG
REM |    Server-side stored procedure package for creating payment
REM |    distributions 
REM |
REM |  History
REM |    Created By:  Haritha Redreddy  (05/05/04)
REM +==================================================================+
REM dbdrv: sql ~PROD ~PATH ~FILE none none none package &phase=pls \
REM dbdrv: checkfile(120.16.12010000.10=120.27.12020000.2):~PROD:~PATH:~FILE

SET VERIFY OFF
WHENEVER SQLERROR EXIT FAILURE ROLLBACK
WHENEVER OSERROR EXIT FAILURE ROLLBACK


CREATE OR REPLACE PACKAGE AP_ACCTG_PAY_DIST_PKG AS
/* $Header: appaydis.pls 120.27.12020000.2 2014/04/30 19:36:06 vasvenka ship $ */


  TYPE r_inv_pay_info IS RECORD 
      (Invoice_ID                AP_Invoices.Invoice_ID%TYPE
      ,Invoice_Payment_ID        AP_Invoice_Payments.Invoice_Payment_ID%TYPE
      ,Amount                    AP_Invoice_Payments.Amount%TYPE
      ,Discount_Taken            AP_Invoice_Payments.Discount_Taken%TYPE
      ,Payment_Base_Amount       AP_Invoice_Payments.Payment_Base_Amount%TYPE
      ,Invoice_Base_Amount       AP_Invoice_Payments.Invoice_Base_Amount%TYPE
      ,Exchange_Rate_Type        AP_Invoice_Payments.Exchange_Rate_Type%TYPE
      ,Exchange_Date             AP_Invoice_Payments.Exchange_Date%TYPE
      ,Exchange_Rate             AP_Invoice_Payments.Exchange_Rate%TYPE
      ,Reversal_Flag             AP_Invoice_Payments.Reversal_Flag%TYPE
      ,Reversal_Inv_Pmt_ID       AP_Invoice_Payments.Reversal_Inv_Pmt_ID%TYPE
      );


  -- Cursor to get invoice payments for the given event id
  CURSOR Invoice_Payments
        (P_Event_ID     NUMBER
        ,P_Invoice_ID   NUMBER DEFAULT NULL
        ) IS
  SELECT AIP.Invoice_ID,
         AIP.Invoice_Payment_ID,
         AIP.Amount,
         AIP.Discount_Taken,
         AIP.Payment_Base_Amount,
         AIP.Invoice_Base_Amount,
         AIP.Exchange_Rate_Type,
         AIP.Exchange_Date,
         AIP.Exchange_Rate,
         NVL(AIP.Reversal_Flag,'N'),
         AIP.Reversal_Inv_Pmt_ID
  FROM   AP_Invoice_Payments_All AIP
  WHERE  Accounting_Event_ID = P_Event_ID
  AND    AIP.Invoice_ID = DECODE(P_Invoice_ID, NULL, AIP.Invoice_ID,
                                 P_Invoice_ID);

  -- Cursor to get all the invoice payments that has been paid by 
  -- the check being passed. This cursor will also get all the 
  -- invoice payments that have been adjusted after the payment has
  -- been cleared.
  /*Added 'REFUND CANCELLED' for bug 9531314 */
  CURSOR Clrg_Invoice_Payments
        (P_Check_ID     NUMBER
        ) IS
  SELECT AIP.Invoice_ID,
         AIP.Invoice_Payment_ID,
         AIP.Amount,
         AIP.Discount_Taken,
         AIP.Payment_Base_Amount,
         AIP.Invoice_Base_Amount,
         AIP.Exchange_Rate_Type,
         AIP.Exchange_Date,
         AIP.Exchange_Rate,
         NVL(AIP.Reversal_Flag,'N'),
         AIP.Reversal_Inv_Pmt_ID
  FROM   AP_Invoice_Payments_All AIP
  WHERE  AIP.Check_ID = P_Check_ID
  AND    NVL(AIP.Reversal_Flag, 'N') <> 'Y'     --bug 9072782, uncomment the change done in bug 7029334
  AND    AIP.Accounting_Event_ID NOT IN
                    (SELECT APH.Accounting_Event_ID
                     FROM   AP_Payment_History_All APH
                     WHERE  APH.Check_ID = P_Check_ID
                     AND    APH.Transaction_Type in ('PAYMENT CANCELLED', 'REFUND CANCELLED'));  --bug 7029334 'PAYMENT CANCELLATION'

  -- Cursor to get payment history information
  -- bug 5623129 
  --   add Nvl bank_To_Base_XRate to 1 when bank currency code
  --   is the same as base currency code. pmt exchange rate
  --   is the clear exchange rate. 
  CURSOR Payment_History
        (P_Event_ID      NUMBER
        ) IS
  SELECT APH.Payment_History_ID,
         APH.Accounting_Date,                 -- bug9271242
         APH.Pmt_Currency_Code,
         APH.Pmt_To_Base_XRate_Type,
         APH.Pmt_To_Base_XRate_Date,
         APH.Pmt_To_Base_XRate,
         APH.Bank_Currency_Code,
         APH.Bank_To_Base_XRate_Type,
         APH.Bank_To_Base_XRate_Date,
         NVL(APH.Bank_To_Base_XRate,1),
         APH.Errors_Bank_Amount,
         APH.Charges_Bank_Amount,
         APH.Rev_Pmt_Hist_ID,
         APH.Related_Event_ID,
         APH.Invoice_Adjustment_Event_ID
  FROM   AP_Payment_History_All APH
  WHERE  APH.Accounting_Event_ID = P_Event_ID;


  -- Cursor to get the invoice header information for the given
  -- invoice id
  CURSOR Invoice_Header
        (P_Invoice_ID    NUMBER
        ) IS
  SELECT AI.Invoice_ID,
         AI.GL_Date,                        --bug9271242
         AI.Invoice_Amount,
         AI.Invoice_Currency_Code,
         AI.Payment_Currency_Code,
         ASP.Base_Currency_Code,
         AI.Pay_Curr_Invoice_Amount,
         AI.Payment_Cross_Rate_Type,
         AI.Payment_Cross_Rate_Date,
         AI.Payment_Cross_Rate,
         AI.Exchange_Rate_Type,
         AI.Exchange_Date,
         AI.Exchange_Rate,
         NVL(AI.Disc_Is_Inv_Less_Tax_Flag, ASP.Disc_Is_Inv_Less_Tax_Flag)
                            Disc_Is_Inv_Less_Tax_Flag,
         NVL(AI.Exclude_Freight_From_Discount, 'N')
                            Exclude_Freight_From_Discount
  FROM   AP_Invoices_All AI,
         AP_System_Parameters_All ASP
  WHERE  AI.Org_ID = ASP.Org_ID
  AND    AI.Invoice_ID = P_Invoice_ID;


  -- Cursor to get all the invoice distributions for a particular
  -- invoice         
  -- bug 5570002 need to exclude TERV/ERV/PREPAY 
  CURSOR Invoice_Dists
        (P_Invoice_ID    NUMBER
        ,P_Event_ID      NUMBER    DEFAULT NULL
        ) IS
  SELECT aid.*
       , count(1) over() dist_count     --Bug 16674834
       , rownum dist_number             --Bug 16674834
    from (select AID.Invoice_Distribution_ID,
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
             AID.historical_flag,
             AID.accounting_event_id
      FROM   AP_Invoice_Distributions_All AID,
             Financials_System_Params_All FSP
      WHERE  AID.Invoice_ID = P_Invoice_ID
      AND    NVL(AID.Accounting_Event_ID,-99) = DECODE(P_Event_ID, NULL, 
                              NVL(AID.Accounting_Event_ID,-99), P_Event_ID)
      AND    AID.Line_Type_Lookup_Code NOT IN ('PREPAY', 'ERV', 'TERV')
      AND    AID.Prepay_Distribution_ID IS NULL
      AND    AID.Prepay_Tax_Parent_ID IS NULL  -- For tax dists created in R11.5
      AND    AID.Org_ID = FSP.Org_ID
      AND NOT EXISTS (SELECT 1
                      FROM   xla_events
                      WHERE  event_id = AID.accounting_event_id
                      AND    application_id = 200 --bug 7308385
                      AND    event_type_code IN ('INVOICE CANCELLED', 'PREPAYMENT CANCELLED',
                                                 'CREDIT MEMO CANCELLED',
                                                 'DEBIT MEMO CANCELLED'))
      AND  ((NVL(FSP.Purch_Encumbrance_Flag,'N') = 'N'
                 AND AID.Match_Status_Flag IN ('T','A'))
             OR
           ((NVL(FSP.Purch_Encumbrance_Flag,'N') = 'Y'
                 AND AID.Match_Status_Flag = 'A')))
      ORDER  BY DECODE(AID.Line_Type_Lookup_Code, 'AWT', 1, 2),  --bug 9670808
                DECODE(nvl(aid.reversal_flag,'N'),'Y',1,2),               --Bug 16674834
                abs(AID.Amount), 
                aid.creation_date,
                AID.Invoice_Distribution_ID) aid; --bug 5410819  
      --Bug 8208856 added AID.Invoice_Distribution_ID to the above order by.


  PROCEDURE Primary_Pay_Events
                 (P_XLA_Event_Rec      IN   ap_accounting_pay_pkg.r_xla_event_info
                 ,P_Calling_Sequence   IN   VARCHAR2
                 );

  PROCEDURE Manual_Pay_Adj_Events
                 (P_XLA_Event_Rec      IN   ap_accounting_pay_pkg.r_xla_event_info
                 ,P_Calling_Sequence   IN   VARCHAR2
                 );

  PROCEDURE Cancel_Primary_Pay_Events
                 (P_XLA_Event_Rec      IN   ap_accounting_pay_pkg.r_xla_event_info
                 ,P_Calling_Sequence   IN   VARCHAR2
                 );

  PROCEDURE Pay_Dist_Cascade_Adj_Events
                 (P_XLA_Event_Rec      IN   ap_accounting_pay_pkg.r_xla_event_info
                 ,P_Calling_Sequence   IN   VARCHAR2
                 );


  PROCEDURE Pay_Dist_Proc
                (P_XLA_Event_Rec      IN    ap_accounting_pay_pkg.r_xla_event_info
                ,P_Inv_Pay_Rec        IN    r_inv_pay_info
                ,P_Pay_Hist_Rec       IN    ap_accounting_pay_pkg.r_pay_hist_info
                ,P_Inv_Rec            IN    ap_accounting_pay_pkg.r_invoices_info
                ,P_Inv_Dist_Rec       IN    ap_accounting_pay_pkg.r_inv_dist_info
                ,P_Calc_Mode          IN    VARCHAR2
                ,P_Final_Payment      IN    BOOLEAN
                ,P_Calling_Sequence   IN    VARCHAR2
                );


  PROCEDURE Pay_Dist_Discount
                (P_XLA_Event_Rec    IN    ap_accounting_pay_pkg.r_xla_event_info
                ,P_Inv_Pay_Rec      IN    r_inv_pay_info
                ,P_Pay_Hist_Rec     IN    ap_accounting_pay_pkg.r_pay_hist_info
                ,P_Inv_Rec          IN    ap_accounting_pay_pkg.r_invoices_info
                ,P_Inv_Dist_Rec     IN    ap_accounting_pay_pkg.r_inv_dist_info
                ,P_Calc_Mode        IN    VARCHAR2
                ,P_Disc_Pay_Amount  IN    OUT NOCOPY    NUMBER
                ,P_Disc_Dist_Amount IN    OUT NOCOPY    NUMBER
                ,P_Disc_Bank_Amount IN    OUT NOCOPY    NUMBER
                ,P_Calling_Sequence IN    VARCHAR2
                );

  PROCEDURE Pay_Dist_ERV
                (P_XLA_Event_Rec    IN    ap_accounting_pay_pkg.r_xla_event_info
                ,P_Inv_Pay_Rec      IN    r_inv_pay_info
                ,P_Pay_Hist_Rec     IN    ap_accounting_pay_pkg.r_pay_hist_info
                ,P_Inv_Rec          IN    ap_accounting_pay_pkg.r_invoices_info
                ,P_Inv_Dist_Rec     IN    ap_accounting_pay_pkg.r_inv_dist_info
                ,P_Prorated_Amount  IN    NUMBER
                ,P_Calling_Sequence IN    VARCHAR2
                );

  -- bug 5659368
  PROCEDURE Pay_Dist_Err_Chrg
                 (p_xla_event_rec    IN    ap_accounting_pay_pkg.r_xla_event_info
                 ,p_pay_hist_rec     IN    ap_accounting_pay_pkg.r_pay_hist_info
                 ,p_distribute_mode  IN    VARCHAR2
                 ,p_calling_sequence IN    VARCHAR2
                 );

  -- Bug 6887295. Added parameter p_inv_dist_rec
  PROCEDURE Pay_Dist_Reverse
                 (p_xla_event_rec         IN    ap_accounting_pay_pkg.r_xla_event_info
                 ,p_inv_pay_rec           IN    r_inv_pay_info
                 ,p_pay_hist_rec          IN    ap_accounting_pay_pkg.r_pay_hist_info
                 ,p_reversal_inv_pmt_id   IN    NUMBER
                 ,p_related_event_id      IN    NUMBER
                 ,p_invoice_dist_id       IN    NUMBER
                 ,p_inv_dist_rec          IN    ap_accounting_pay_pkg.r_inv_dist_info
                 ,p_calling_sequence      IN    VARCHAR2
                 );


  PROCEDURE Pay_Dist_Insert
                 (P_PD_Rec             IN   AP_PAYMENT_HIST_DISTS%ROWTYPE
                 ,P_Calling_Sequence   IN   VARCHAR2
                 );

  PROCEDURE Upg_Dist_Links_Insert
                 (p_xla_event_rec       IN   ap_accounting_pay_pkg.r_xla_event_info
                 ,p_payment_history_id  IN   NUMBER
                 ,p_accounting_event_id IN   NUMBER
                 ,p_calling_sequence    IN   VARCHAR2
                 );


END AP_ACCTG_PAY_DIST_PKG;
/

COMMIT;
EXIT;
