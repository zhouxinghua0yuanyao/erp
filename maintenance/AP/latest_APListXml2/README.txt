
==============================================================================
APList Readme File
==============================================================================

Version  - APListXml.xml 12.x.support.version
Product  - Payables
Release  - R12
Platform - Generic Platform


==============================================================================
Description
==============================================================================

1.  Description

Latest version of the XML file used by the Invoice Data Collection Diagnostic (APList) Test.

2.  Usage

	1.  Log in to server and set the environment.
	
	2.  Backup original file. Navigate to the following Directory on your server:
	
	cd $JAVA_TOP/oracle/apps/ap/diag
	
	and create a copy of the original e.g.
	
	cp APListXml.xml APListXml_copy.xml
	
	3.  Move the new APListXml.xml file obtained from this download to $JAVA_TOP/oracle/apps/ap/diag
	

Note:  R12.2 customers must complete the above steps for both the Patch and Run File System environments.
The "adop fs=clone" issue during the patching process will copy the complete APPL_TOP from patch and paste it on
Run Env so it is required to maintain copy at both places

==============================================================================
Bugs Fixed/Enhancements
==============================================================================
The following bug fixes/enhancments are included in this file

	--table gl_code_combinations taking ccid from PO and xla_ae_lines
	--Undo Accounting xla entries for the Invoice and related Payments.
	--Performance - Wherever we are using the xla related tables need to add application_id=200 condition
	--Performance - xte.source_id_int_1 = XYZ should be replace by nvl(xte.source_id_int_1, -99) = XYZ
	--Performance - Need to add the following condition to possible zx related table. event_class_code in ('STANDARD INVOICES' , 'PREPAYMENT INVOICES', 'EXPENSE REPORTS') 
	--The query for AP_INV_SELECTION_CRITERIA_ALL should be changed
	--ap_invoice_dists_arch table
	--Need the change the ap_payment_hist_dists query to fetch record for all the invoices paid by a check
	--Add Amount, summary_tax_line_id columns in ap_invoice_lines key column and order by line number
	--AP invoice distributions all key column arrange by invoice line number and distribution line number
	--Add summary tax line id and detail tax dist id and historical_flag to AP invoice distributions all key column
	--iby_external_payees_all                               
	--po_vendors_obs  
	--hz_org_contacts
	--po_line_locations_all                                 
	--po_vendor_sites_obs  
	--ap_recurring_payments_all                             
	--IBY_EXT_BANK_ACCOUNTS                                 
	--When invoice is expense report type, ap_expense_report_headers_all
	--When invoice is expense report type, ap_expense_report_lines_all
	--When invoice is expense report type, ap_exp_report_dists_all
	--ap_liability_balance
	--R12 Bank Details
	--all columns for ap_invoice_distributions_all
	--Add withholding tax tables
	--Changed Bank Account and CE Statement Reconcils tables to use ac.ce_bank_acct_use_id 
	--XLA_SUBLEDGERS
	--XLA_LAUNCH_OPTIONS
	--XLA_LEDGER_OPTIONS
	--AP_TAX_CODES_ALL
|       --GL_LEDGER_NORM_SEG_VALS


