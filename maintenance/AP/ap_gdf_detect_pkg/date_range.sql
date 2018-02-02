REM $Id: date_range.sql,v 200.11 2016/04/18 21:10:43 alumpe Exp alumpe $
REM +=========================================================================+
REM |                 Copyright (c) 2001 Oracle Corporation                   |
REM |                    Redwood Shores, California, USA                      |
REM |                         All rights reserved.                            |
REM +=========================================================================+
REM |                                                                         |
REM | FILENAME                                                                |
REM |    date_range.sql                                                       |
REM |                                                                         |
REM | DESCRIPTION                                                             |
REM |    Wrapper SQL to submit the ap_gdf_detect_pkg.main for a date range    |
REM |    operating unit, and transaction type                                 |
REM |                                                                         |
REM | HISTORY                                                                 |
REM | 15-DEC-2011 ALUMPE  Created.                                            |
REM | 28-DEC-2011 ALUMPE  Modified to call process in debug mode.             |
REM | 05-JAN-2011 ALUMPE  Added "SET DEFINE" to avoid undefined bind          |
REM |                     variable error.                                     |
REM | 14-AUG-2011 ALUMPE  Added "exit" to the end of the script.              |
REM | 13-AUG-2015 ALUMPE  Added analyzer bundle menu code and changed version |
REM |                     to required 200.XXX format                          |
REM | 28-SEP-2015 ALUMPE  Updated with latest bundle headers.                 |
REM |                                                                         |
REM |                                                                         |
REM +=========================================================================+
REM ANALYZER_BUNDLE_START
REM
REM COMPAT: 12.0 12.1 12.2
REM
REM MENU_TITLE: Master GDF Diagnostic (MGD) Analyzer for a Date Range
REM
REM MENU_START
REM
REM SQL: Run Master GDF Diagnostic (MGD) Analyzer for a Date Range
REM FNDLOAD: Load Master GDF Diagnostic (MGD) Analyzer for a Date Range as a Concurrent Program
REM
REM MENU_END
REM
REM
REM HELP_START
REM
REM  Master GDF Diagnostic (MGD) Analyzer Help [Doc ID: 1360390.1]
REM
REM  Compatible: 12.0|12.1|12.2
REM
REM  Explanation of available options:
REM
REM    (1) Runs Master GDF Diagnostic as APPS to scan a date range
REM        of AP, Payments, Suppliers and eBTax transactions
REM        o Runs ap_gdf_detect_pkg.main package in "date range" mode
REM        o Creates an HTML report file
REM
REM    (2) Install Master GDF Diagnostic [Date Range] as a concurrent program
REM        o Runs FNDLOAD as APPS
REM        o Defines the analyzer as a concurrent executable/program
REM        o Adds the analyzer to a default request group:
REM          "Payables Reports Only"
REM
REM
REM HELP_END
REM
REM FNDLOAD_START
REM
REM PROD_TOP: AP_TOP
REM PROG_NAME: APGDFVAL
REM DEF_REQ_GROUP: Payables Reports Only
REM APP_NAME: Payables
REM PROG_TEMPLATE: APGDFVAL.ldt
REM PROD_SHORT_NAME: SQLAP
REM
REM FNDLOAD_END
REM
REM DEPENDENCIES_START
REM
REM ap_gdf_detect_pkg.sql
REM
REM DEPENDENCIES_END
REM
REM RUN_OPTS_START
REM
REM RUN_OPTS_END
REM
REM OUTPUT_TYPE: UTL_FILE
REM
REM ANALYZER_BUNDLE_END

SET SERVEROUTPUT ON SIZE 1000000
SET ECHO OFF 
SET VERIFY OFF
SET DEFINE "&"

PROMPT
PROMPT Submitting data validation for a date range.
PROMPT
PROMPT ===========================================================================
PROMPT Enter the start date and end date for the range in DD-MON-YYYY format.
PROMPT NOTE: Both dates must fall in an open period.
PROMPT ===========================================================================
PROMPT
ACCEPT s_date DATE FORMAT 'DD-MON-YYYY' PROMPT 'Enter the START DATE [DD-MON-YYYY]: '
ACCEPT e_date DATE FORMAT 'DD-MON-YYYY' PROMPT 'Enter the   END DATE [DD-MON-YYYY]: '
PROMPT
PROMPT ===========================================================================
PROMPT Enter the org_id(s) for the operating unit(s).  For multiple 
PROMPT operating units, separate with commas.  For all, leave blank.
PROMPT ===========================================================================
PROMPT
ACCEPT ous CHAR PROMPT 'Enter the org_id(s): '
PROMPT
PROMPT ===========================================================================
PROMPT Enter the type of transactions to validate
PROMPT [INVOICE, PAYMENT, SUPPLIER, or ALL]
PROMPT ===========================================================================
PROMPT
ACCEPT trx_type CHAR PROMPT 'Enter the transaction type [ALL]: ' 
PROMPT
PROMPT
PROMPT

DECLARE
  l_start_date DATE := to_date('&s_date'); 
  l_end_date   DATE := to_date('&e_date');
  l_org_ids    VARCHAR2(240) :=  '&ous';
  l_trx_type   VARCHAR2(10) := upper('&trx_type');
BEGIN

  IF l_trx_type is null THEN l_trx_type := 'ALL'; END IF;

  ap_gdf_detect_pkg.main(
      p_start_date => l_start_date,
      p_end_date => l_end_date,
      p_org_ids => l_org_ids,
      p_trx_type => l_trx_type,
      p_max_output_rows => 20,
      p_validations => 'GDF',
      p_debug_mode => 'Y');

EXCEPTION WHEN OTHERS THEN
  dbms_output.put_line('Error encountered: '||sqlerrm);
END;
/
exit
