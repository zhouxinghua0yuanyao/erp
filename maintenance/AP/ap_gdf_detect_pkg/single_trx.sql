REM $Id: single_trx.sql,v 200.10 2016/04/18 21:10:52 alumpe Exp alumpe $
REM +=========================================================================+
REM |                 Copyright (c) 2001 Oracle Corporation                   |
REM |                    Redwood Shores, California, USA                      |
REM |                         All rights reserved.                            |
REM +=========================================================================+
REM |                                                                         |
REM | FILENAME                                                                |
REM |    single_trx.sql                                                       |
REM |                                                                         |
REM | DESCRIPTION                                                             |
REM |    Wrapper SQL to submit the ap_gdf_detect_pkg.main procedure in single |
REM |    transaction mode.                                                    |
REM |                                                                         |
REM | HISTORY                                                                 |
REM | 15-DEC-2011 ALUMPE  Created.                                            |
REM | 28-DEC-2011 ALUMPE  Modified to call process in debug mode.             |
REM | 05-JAN-2011 ALUMPE  Added "SET DEFINE" to avoid undefined bind          |
REM |                     variable error.                                     |
REM | 14-AUG-2011 ALUMPE  Added "exit" to the end of the script.              |
REM | 13-AUG-2015 ALUMPE  Added Analyzer Bundle Menu information              |
REM | 28-SEP-2015 ALUMPE  Updated with latest bundle headers                  |
REM |                                                                         |
REM |                                                                         |
REM |                                                                         |
REM +=========================================================================+
REM ANALYZER_BUNDLE_START
REM
REM COMPAT: 12.0 12.1 12.2
REM
REM MENU_TITLE: Master GDF Diagnostic (MGD) Analyzer for a Single Transaction
REM
REM MENU_START
REM
REM SQL: Run Master GDF Diagnostic (MGD) Analyzer for a Single Transaction
REM FNDLOAD: Load Master GDF Diagnostic (MGD) Analyzer for a Single Transaction as a Concurrent Program
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
REM    (1) Run Master GDF Diagnostic (MGD) Analyzer for a Single Transaction
REM        and eBTax transactions
REM        o Runs ap_gdf_detect_pkg.main package in "single transaction" mode
REM        o Creates an HTML report file
REM
REM    (2) Install Master GDF Diagnostic [Single Transaction]
REM        as a concurrent program
REM      [runs FNDLOAD]
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
REM PROG_NAME: APGDFVAL_SINGLE
REM DEF_REQ_GROUP: Payables Reports Only
REM APP_NAME: Payables
REM PROG_TEMPLATE: APGDFVAL_SINGLE.ldt
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
PROMPT Submitting data validation for single transactions.
PROMPT
PROMPT ===========================================================================
PROMPT Enter the invoice id and/or check id and/or vendor id where prompted.
PROMPT ===========================================================================
PROMPT
ACCEPT inv_id NUMBER DEFAULT 0 -
       PROMPT 'Enter the INVOICE ID or press enter to leave blank: '
ACCEPT pmt_id NUMBER DEFAULT 0 -
       PROMPT 'Enter the CHECK ID   or press enter to leave blank: '
ACCEPT vnd_id NUMBER DEFAULT 0 -
       PROMPT 'Enter the VENDOR ID  or press enter to leave blank: '
PROMPT
PROMPT
PROMPT

DECLARE
  l_inv_id NUMBER := &inv_id; 
  l_pmt_id NUMBER := &pmt_id;
  l_vnd_id NUMBER := &vnd_id;
BEGIN
  IF l_inv_id = 0 THEN l_inv_id := null; END IF;
  IF l_pmt_id = 0 THEN l_pmt_id := null; END IF;
  IF l_vnd_id = 0 THEN l_vnd_id := null; END IF;

  IF l_inv_id is not null OR l_pmt_id is not null OR l_vnd_id is not null THEN
    ap_gdf_detect_pkg.main(
      p_invoice_id => l_inv_id,
      p_check_id => l_pmt_id,
      p_vendor_id => l_vnd_id,
      p_validations => 'ALL',
      p_debug_mode => 'Y');
  ELSE
    dbms_output.put_line('No valid parameters entered.  Process not submitted.');
  END IF;

EXCEPTION WHEN OTHERS THEN
  dbms_output.put_line('Error encountered: '||sqlerrm);
END;
/
exit
