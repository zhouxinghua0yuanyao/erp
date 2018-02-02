REM +==================================================================+
REM |                Copyright (c) 1999, 2014 Oracle Corporation
REM |                   Redwood Shores, California, USA
REM |                        All rights reserved.
REM +==================================================================+
REM |  Name
REM |    appredis.pls
REM |
REM |  Description - Package AP_ACCTG_PREPAY_DIST_PKG
REM |    Server-side stored procedure package for creating prepayment
REM |    application distributions 
REM |
REM |  History
REM |    Created By:  Haritha Redreddy  (06/01/04)
REM +==================================================================+
REM dbdrv: sql ~PROD ~PATH ~FILE none none none package &phase=pls \
REM dbdrv: checkfile(120.11.12010000.12=120.25.12020000.2):~PROD:~PATH:~FILE

SET VERIFY OFF
WHENEVER SQLERROR EXIT FAILURE ROLLBACK
WHENEVER OSERROR EXIT FAILURE ROLLBACK


CREATE OR REPLACE PACKAGE AP_ACCTG_PREPAY_DIST_PKG AS
/* $Header: appredis.pls 120.25.12020000.2 2014/04/30 19:50:46 vasvenka ship $ */


  TYPE r_prepay_hist_info IS RECORD
      (Prepay_History_ID            AP_Prepay_History.Prepay_History_ID%TYPE
      ,Prepay_Invoice_ID            AP_Prepay_History.Prepay_Invoice_ID%TYPE
      ,Invoice_ID                   AP_Prepay_History.Invoice_ID%TYPE
      ,Invoice_Line_Number          AP_Prepay_History.Invoice_Line_Number%TYPE
      ,Transaction_Type             AP_Prepay_History.Transaction_Type%TYPE
      ,Accounting_Date              AP_Prepay_History.Accounting_Date%TYPE
      ,Invoice_Adjustment_Event_ID  AP_Prepay_History.Invoice_Adjustment_Event_ID%TYPE
      ,Related_Prepay_App_Event_ID  AP_Prepay_History.Related_Prepay_App_Event_ID%TYPE
      );


  TYPE r_prepay_dist_info IS RECORD 
      (Invoice_ID                AP_Invoice_Distributions.Invoice_ID%TYPE
      ,Invoice_Distribution_ID   AP_Invoice_Distributions.Invoice_Distribution_ID%TYPE
      ,Line_Type_Lookup_Code     AP_Invoice_Distributions.Line_Type_Lookup_Code%TYPE
      ,Amount                    AP_Invoice_Distributions.Amount%TYPE
      ,Base_Amount               AP_Invoice_Distributions.Base_Amount%TYPE
      ,Accounting_Event_ID       AP_Invoice_Distributions.Accounting_Event_ID%TYPE
      ,Prepay_Distribution_ID    AP_Invoice_Distributions.Prepay_Distribution_ID%TYPE
      ,Prepay_Tax_Diff_Amount    AP_Invoice_Distributions.Prepay_Tax_Diff_Amount%TYPE
      ,Parent_Reversal_ID        AP_Invoice_Distributions.Parent_Reversal_ID%TYPE
      );


  -- bug7349279, added the order by condition
  -- to ensure that the prepayment unapplications
  -- are always fetched after the prepayment 
  -- applications
  --
  --
  -- bug9038462, changing this cursor to pick up
  -- Prepay History records which have not yet 
  -- been Accounted or Encumbered
  --
  CURSOR Prepay_History 
        (P_Invoice_ID   NUMBER
        ) IS
  SELECT APH.Prepay_History_ID,
         APH.Prepay_Invoice_ID,
         APH.Invoice_ID,
         APH.Invoice_Line_Number,
         APH.Transaction_Type,
         APH.Accounting_Date,
         APH.Invoice_Adjustment_Event_ID,
         APH.Related_Prepay_App_Event_ID
  FROM   AP_Prepay_History_All APH,
         XLA_Events XE
  WHERE  APH.Invoice_ID = P_Invoice_ID
  --AND    APH.Accounting_Event_ID IS NULL
  AND    nvl(APH.Posted_flag, 'N') <> 'Y'
  AND    XE.application_id(+) = 200
  AND    APH.bc_event_id = XE.event_id(+)
  AND    (nvl(XE.event_id, -99) = -99 OR
          XE.event_status_code <> 'P')
  AND    (nvl(XE.event_id, -99) = -99 OR
          XE.budgetary_control_flag = 'Y')
  ORDER BY transaction_type, Prepay_History_ID;  --Bug 10173936 added Prepay_History_ID


  -- bug9038462, changing this cursor to pick up
  -- Invoice Distributions for Prepayment Applications
  -- which have not yet been Accounted or Encumbered
  --
  CURSOR Prepay_Dists
        (P_Invoice_ID             NUMBER,
         P_Invoice_Line_Number    NUMBER,
         P_Accounting_Date        DATE,
         P_Prepay_History_ID      NUMBER
        ) IS
 (SELECT AID.Invoice_ID,
         AID.Invoice_Distribution_ID Invoice_Distribution_ID,
         AID.Line_Type_Lookup_Code,
         AID.Amount,
         AID.Base_Amount,
         AID.Accounting_Event_ID,
         AID.Prepay_Distribution_ID,
         AID.Prepay_Tax_Diff_Amount,
         AID.Parent_Reversal_ID
  FROM   AP_Invoice_Distributions_All AID
  WHERE  Invoice_ID = P_Invoice_ID
  AND    Invoice_Line_Number = P_Invoice_Line_Number
  AND    Line_Type_Lookup_Code = 'PREPAY'
  AND    Accounting_Date = P_Accounting_Date
  --AND    Accounting_Event_ID IS NULL
  AND    nvl(AID.Posted_flag, 'N') <> 'Y'
  AND    nvl(AID.Encumbered_flag, 'N') <> 'Y'
  AND    EXISTS (SELECT 'Prepay History'
                 FROM   AP_Prepay_History_All APH,
                        AP_Invoice_Distributions_All AID1
                 WHERE  APH.Prepay_History_ID = P_Prepay_History_ID
                 AND    AID1.Invoice_Distribution_ID = AID.Prepay_Distribution_ID
                 AND    AID1.Invoice_ID = APH.Prepay_Invoice_ID
                 AND    AID1.Invoice_Line_Number = APH.Prepay_Line_Num
                 -- Bug 6718967
                 AND    DECODE(APH.Transaction_Type, 'PREPAYMENT APPLIED', 1, 2) =
                             DECODE(NVL(AID.Parent_Reversal_ID,-99), -99, 1, 2))
  UNION ALL
  SELECT AID.Invoice_ID,
         AID.Invoice_Distribution_ID Invoice_Distribution_ID,
         AID.Line_Type_Lookup_Code,
         AID.Amount,
         AID.Base_Amount,
         AID.Accounting_Event_ID,
         AID.Prepay_Distribution_ID,
         AID.Prepay_Tax_Diff_Amount,
         AID.Parent_Reversal_ID
  FROM   AP_Invoice_Distributions_All AID
  WHERE  Invoice_ID = P_Invoice_ID
  AND    Line_Type_Lookup_Code IN ('NONREC_TAX','REC_TAX')
  --AND    Accounting_Event_ID IS NULL
  AND    nvl(AID.Posted_flag, 'N') <> 'Y'
  AND    nvl(AID.Encumbered_flag, 'N') <> 'Y'
  AND    Charge_Applicable_To_Dist_ID IN
               (SELECT AID1.Invoice_Distribution_ID
                FROM   AP_Invoice_Distributions_All AID1
                WHERE  Invoice_ID = P_Invoice_ID
                AND    Invoice_Line_Number = P_Invoice_Line_Number
                AND    Line_Type_Lookup_Code = 'PREPAY'
                AND    Accounting_Date = P_Accounting_Date
                --AND    Accounting_Event_ID IS NULL   Bug 10184420
                AND    EXISTS (SELECT 'Prepay History'
                               FROM   AP_Prepay_History_All APH,
                                      AP_Invoice_Distributions_All AID2
                               WHERE  APH.Prepay_History_ID = P_Prepay_History_ID
                               AND    AID2.Invoice_Distribution_ID = AID1.Prepay_Distribution_ID
                               AND    AID2.Invoice_ID = APH.Prepay_Invoice_ID
                               AND    AID2.Invoice_Line_Number = APH.Prepay_Line_Num
                               -- Bug 6718967
                               AND    DECODE(APH.Transaction_Type, 'PREPAYMENT APPLIED', 1, 2) =
                                        DECODE(NVL(AID.Parent_Reversal_ID,-99), -99, 1, 2))))
  ORDER BY Invoice_Distribution_ID;  --bug 7614480, order by invoice_distribution_id


  CURSOR Invoice_Dists
        (P_Invoice_ID   NUMBER
        ,P_Event_ID     NUMBER    DEFAULT NULL
        ) IS
  select aid.*
       , count(1) over() dist_count     --Bug 16674834
       , rownum dist_number             --Bug 16674834
    from (SELECT AID.Invoice_Distribution_ID,
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
                 AID.Historical_Flag,
                 AID.Accounting_Event_Id
          FROM   AP_Invoice_Distributions_All AID
          WHERE  AID.Invoice_ID = P_Invoice_ID
          AND    NVL(AID.Accounting_Event_ID, -99) = 
                     DECODE(P_Event_ID, NULL, NVL(AID.Accounting_Event_ID, -99),
                            P_Event_ID)
          AND    AID.Line_Type_Lookup_Code NOT IN ('PREPAY', 'ERV','TERV') --Bug 10181254 added TERV
          AND    AID.Prepay_Distribution_ID IS NULL
          AND    AID.Prepay_Tax_Parent_ID IS NULL -- For tax dists created in R11.5
          AND    NVL(AID.Cancellation_Flag,'N') <> 'Y'     -- BUG 6513956 
          -- bug fix 6909150  
          AND NOT EXISTS (SELECT 1
                          FROM   xla_events
                          WHERE  event_id = AID.accounting_event_id
                          AND    event_type_code IN ('INVOICE CANCELLED', 
                                                     'CREDIT MEMO CANCELLED',
                                                     'DEBIT MEMO CANCELLED'))  
          ORDER  BY decode(aid.line_type_lookup_code,'AWT',1,2), --Bug 9166188
                 abs(AID.Amount),
                 AID.Invoice_Distribution_ID) aid;


  CURSOR Payment_History
        (P_Invoice_ID           NUMBER
        ,P_Transaction_Type     VARCHAR2
        ) IS
  SELECT APH.Payment_History_ID,
         APH.Accounting_Date,                           -- bug9271242
         APH.Pmt_Currency_Code,
         APH.Pmt_To_Base_XRate_Type,
         APH.Pmt_To_Base_XRate_Date,
         APH.Pmt_To_Base_XRate,
         APH.Bank_Currency_Code,
         APH.Bank_To_Base_XRate_Type,
         APH.Bank_To_Base_XRate_Date,
         APH.Bank_To_Base_XRate,
         APH.Errors_Bank_Amount,
         APH.Charges_Bank_Amount,
         APH.Rev_Pmt_Hist_ID,
         APH.Related_Event_ID,
         APH.Invoice_Adjustment_Event_ID
  FROM   AP_Payment_History_All APH,
         AP_Invoice_Payments_All AIP
  WHERE  AIP.Invoice_ID = P_Invoice_ID
  AND    AIP.Check_ID   = APH.Check_ID
  AND    APH.Transaction_Type = P_Transaction_Type
  AND    NVL(AIP.Reversal_Flag, 'N') <> 'Y'
  AND    NOT EXISTS (SELECT 'Reversal Exists'
                     FROM   AP_Payment_History_All APH1
                     WHERE  APH1.Rev_Pmt_Hist_ID = APH.Payment_History_ID
                     AND    APH1.Check_ID = APH.Check_ID);


  CURSOR Invoice_Header
        (P_Invoice_ID    NUMBER
        ) IS
  SELECT AI.Invoice_ID,
         AI.GL_Date,                                  -- bug9271242
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
  WHERE  AI.Invoice_ID = P_Invoice_ID
  AND    AI.Org_ID = ASP.Org_ID;


  PROCEDURE Prepay_Dist_Appl
                 (P_Invoice_ID         IN   NUMBER
                 ,P_Calling_Sequence   IN   VARCHAR2
                 );


  PROCEDURE Prepay_Dist_Cascade_Adj
                 (P_XLA_Event_Rec      IN   ap_accounting_pay_pkg.r_xla_event_info
                 ,P_Calling_Sequence   IN   VARCHAR2
                 );


  -- Bug 6698125. Added p_xla_event_rec parameter
  PROCEDURE Prepay_Dist_Proc
                (P_Pay_Hist_Rec       IN    ap_accounting_pay_pkg.r_pay_hist_info
                ,P_Clr_Hist_Rec       IN    ap_accounting_pay_pkg.r_pay_hist_info
                ,P_Inv_Rec            IN    ap_accounting_pay_pkg.r_invoices_info
                ,P_Prepay_Inv_Rec     IN    ap_accounting_pay_pkg.r_invoices_info
                ,P_Prepay_Hist_Rec    IN    r_prepay_hist_info
                ,P_Prepay_Dist_Rec    IN    r_prepay_dist_info
                ,P_Inv_Dist_Rec       IN    ap_accounting_pay_pkg.r_inv_dist_info
                ,P_XLA_Event_Rec      IN    ap_accounting_pay_pkg.r_xla_event_info
                ,P_Calc_Mode          IN    VARCHAR2
                ,P_Final_Payment      IN    BOOLEAN
                ,P_Calling_Sequence   IN    VARCHAR2
                );

  PROCEDURE Prepay_Dist_Tax_Diff
                (P_Pay_Hist_Rec       IN    ap_accounting_pay_pkg.r_pay_hist_info
                ,P_Clr_Hist_Rec       IN    ap_accounting_pay_pkg.r_pay_hist_info
                ,P_Inv_Rec            IN    ap_accounting_pay_pkg.r_invoices_info
                ,P_Prepay_Inv_Rec     IN    ap_accounting_pay_pkg.r_invoices_info
                ,P_Prepay_Hist_Rec    IN    r_prepay_hist_info
                ,P_Prepay_Dist_Rec    IN    r_prepay_dist_info
                ,P_Inv_Dist_Rec       IN    ap_accounting_pay_pkg.r_inv_dist_info
                ,P_Calc_Mode          IN    VARCHAR2
                ,P_Calling_Sequence   IN    VARCHAR2
                );


  PROCEDURE Prepay_Dist_ERV
                (P_Pay_Hist_Rec     IN    ap_accounting_pay_pkg.r_pay_hist_info
                ,P_Clr_Hist_Rec     IN    ap_accounting_pay_pkg.r_pay_hist_info
                ,P_Inv_Rec          IN    ap_accounting_pay_pkg.r_invoices_info
                ,P_Prepay_Inv_Rec   IN    ap_accounting_pay_pkg.r_invoices_info
                ,P_Prepay_Hist_Rec  IN    r_prepay_hist_info
                ,P_Prepay_Dist_Rec  IN    r_prepay_dist_info
                ,P_Inv_Dist_Rec     IN    ap_accounting_pay_pkg.r_inv_dist_info
                ,P_Prorated_Amount  IN    NUMBER
                ,P_Calling_Sequence IN    VARCHAR2
                );


  -- Bug 6698125. Added p_xla_event_rec parameter
  -- Bug 7134020. Added p_inv_dist_id parameter
  PROCEDURE Prepay_Dist_Reverse
                 (P_prepay_hist_rec       IN    r_prepay_hist_info
                 ,p_prepay_reversal_id    IN    NUMBER
                 ,P_XLA_Event_Rec         IN    ap_accounting_pay_pkg.r_xla_event_info
                 ,p_inv_reversal_id       IN    NUMBER
                 ,p_inv_dist_id           IN    NUMBER
                 ,p_prepay_inv_dist_id    IN    NUMBER
                 ,p_calling_sequence      IN    VARCHAR2
                 );

  -- 9322009, added the following procedure to recreate the incorrect dist
  -- links for the upgraded prepayment application events, so the the 
  -- prepayment unapplication for the same created in R12 can get successfully
  -- accounted
  --
  PROCEDURE Upg_Dist_Links_Insert
                 (P_Invoice_ID          IN   NUMBER
                 ,p_prepay_history_id   IN   NUMBER
                 ,p_accounting_event_id IN   NUMBER
                 ,p_calling_sequence    IN   VARCHAR2
                 );

  PROCEDURE Prepay_Dist_Insert
                 (P_PAD_Rec            IN   AP_PREPAY_APP_DISTS%ROWTYPE
                 ,P_Calling_Sequence   IN   VARCHAR2
                 );

  --Bug 5373620 Added following Procedure
  PROCEDURE Delete_Hist_Dists
                 (P_invoice_id                 IN    NUMBER,
                  P_Calling_Sequence           IN    VARCHAR2
                 );

  -- Bug 5394585 Added procedure
  PROCEDURE Update_Gain_Loss_Ind
                 (P_XLA_Event_Rec      IN   ap_accounting_pay_pkg.r_xla_event_info
                 ,P_Calling_Sequence   IN   VARCHAR2
                 );

END AP_ACCTG_PREPAY_DIST_PKG;
/

COMMIT;
EXIT;
