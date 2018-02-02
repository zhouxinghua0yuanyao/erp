REM $Header: iby_dup_payee_sel.sql 120.1.12020000.2 2014/08/21 10:48:30 asarada noship $
REM +=================================================================+
REM |                Copyright (c) 2014, 2014 Oracle Corporation      |
REM |                   Redwood Shores, California, USA               |
REM |                        All rights reserved.                     |
REM +=================================================================+
REM | FILENAME                                                              |
REM |     iby_dup_payee_sel.sql		                                    |
REM |									    |
REM | BACKUP TABLES:                                                        |
REM |     The following backup tables are created by this GDF               |
REM |                                                                       |
REM |       iby_ext_payees_10140168_tmp  -- DRIVER TABLE                    |
REM |       iby_payments_all_10140168    -- IBY_PAYMENTS_ALL		    |
REM |       iby_docs_payable_10140168    -- IBY_DOCS_PAYABLE_ALL	    |
REM |       iby_pmt_instr_uses_10140168  -- IBY_PMT_INSTR_USES_ALL          |
REM |       iby_ext_pty_pmt_mtds_10140168 -- IBY_EXT_PARTY_PMT_MTHDS        |
REM | DESCRIPTION                                                           |
REM |    This script will detect and show the Duplicate Payees in the system|
REM |									    |
REM | HISTORY Created by gmaheswa on 23-Sep-2010                            |
REM +=======================================================================+
REM dbdrv:none
REM $Header: iby_dup_payee_sel.sql 120.1.12020000.2 2014/08/21 10:48:30 asarada noship $

SET SERVEROUTPUT ON
SET VERIFY OFF
WHENEVER SQLERROR EXIT FAILURE ROLLBACK;
WHENEVER OSERROR EXIT FAILURE ROLLBACK;
declare
  l_count            NUMBER; 
  l_file_location    v$parameter.value%TYPE;   
  l_message          VARCHAR2(500);
l_stmt varchar2(4000);

  CURSOR get_dup_payees IS
  SELECT
            A.EXT_PAYEE_ID	,		
	    A.PAYEE_PARTY_ID	,		
	    A.PAYMENT_FUNCTION	,	
	    A.EXCLUSIVE_PAYMENT_FLAG		,
	    A.PARTY_SITE_ID                   ,
	    A.SUPPLIER_SITE_ID                ,
	    A.ORG_ID                          ,
	    A.ORG_TYPE                        ,
	    A.DEFAULT_PAYMENT_METHOD_CODE     ,
	    A.ECE_TP_LOCATION_CODE            ,
	    A.BANK_CHARGE_BEARER              ,
	    A.BANK_INSTRUCTION1_CODE          ,
	    A.BANK_INSTRUCTION2_CODE          ,
	    A.BANK_INSTRUCTION_DETAILS        ,
	    A.PAYMENT_REASON_CODE             ,
	    A.PAYMENT_REASON_COMMENTS         ,
	    A.INACTIVE_DATE                   ,
	    A.PAYMENT_TEXT_MESSAGE1           ,
	    A.PAYMENT_TEXT_MESSAGE2           ,
	    A.PAYMENT_TEXT_MESSAGE3           ,
	    A.DELIVERY_CHANNEL_CODE           ,
	    A.PAYMENT_FORMAT_CODE             ,
	    A.SETTLEMENT_PRIORITY             ,
	    A.REMIT_ADVICE_DELIVERY_METHOD    ,
	    A.REMIT_ADVICE_EMAIL              ,
	    A.REMIT_ADVICE_FAX                
	   FROM iby_external_payees_all a
           WHERE EXISTS (SELECT 'duplicates' 
			 FROM iby_external_payees_all b 
			 WHERE a.payee_party_id = b.payee_party_id 
			 AND a.payment_function = b.payment_function 
			 AND NVL(a.party_site_id, '0') = NVL(b.party_site_id, '0') 
			 AND NVL(a.supplier_site_id, '0') = NVL(b.supplier_site_id, '0') 
			 AND NVL(a.org_id, '0') = NVL(b.org_id, '0') 
			 AND NVL(a.org_type, '0') = NVL(b.org_type, '0') 
			 AND a.ext_payee_id <> b.ext_payee_id
		)
	   ORDER BY a.PAYEE_PARTY_ID, a.last_update_date DESC;

BEGIN
    IBY_DATA_FIX_UTILITY_PKG.Open_Log_File(10140168||'-diag',l_file_location);
    IBY_DATA_FIX_UTILITY_PKG.Print('<html><body>');
	  --------------------------------------------------------------------------
	  -- Step 1: Drop the temporary tables if already exists
	  --------------------------------------------------------------------------
	  BEGIN
	    EXECUTE IMMEDIATE 'Drop table iby_external_payees_10140168';
	  EXCEPTION
	    WHEN OTHERS THEN
		NULL;
	  END;
    
	  BEGIN
	    EXECUTE IMMEDIATE 'Drop table iby_ext_payees_10140168_tmp';
	  EXCEPTION
	    WHEN OTHERS THEN
		NULL;
	  END;
    
	  BEGIN
	    Execute Immediate 'Drop table iby_payments_all_10140168';
	  EXCEPTION
	    WHEN OTHERS THEN
		NULL;
	  END;

	  BEGIN
	    Execute Immediate 'Drop table iby_docs_payable_10140168';
	  EXCEPTION
	    WHEN OTHERS THEN
		NULL;
	  END;

	  BEGIN
	    Execute Immediate 'Drop table iby_pmt_instr_uses_10140168';
	  EXCEPTION
	    WHEN OTHERS THEN
		NULL;
	  END;
	  BEGIN
	    Execute Immediate 'Drop table IBY_EXT_PTY_PMT_MTDS_10140168';
	  EXCEPTION
	    WHEN OTHERS THEN
		NULL;
	  END;

	  BEGIN
	    Execute Immediate 'Drop table IBY_PTY_PMT_MTDS_10140168_TMP';
	  EXCEPTION
	    WHEN OTHERS THEN
		NULL;
	  END;

          BEGIN
		EXECUTE IMMEDIATE ' DROP table iby_instr_uses_10140168_tmp';
	  EXCEPTION
	    WHEN OTHERS THEN
		NULL;
	  END;

	  --------------------------------------------------------------------------
	  --Step 2: Create all temporary tables 
	  --------------------------------------------------------------------------

	  Begin
	    EXECUTE IMMEDIATE
	      'CREATE TABLE iby_ext_payees_10140168_tmp(
		EXT_PAYEE_ID				NUMBER(15)   ,
		PAYEE_PARTY_ID				NUMBER(15)   ,
		PAYMENT_FUNCTION			VARCHAR2(30) ,
		EXCLUSIVE_PAYMENT_FLAG                  VARCHAR2(1),
		PARTY_SITE_ID                           NUMBER(15)   ,
		SUPPLIER_SITE_ID                        NUMBER(15)   ,
		ORG_ID                                  NUMBER(15)   ,
		ORG_TYPE                                VARCHAR2(30) ,
		DEFAULT_PAYMENT_METHOD_CODE             VARCHAR2(30) ,
		ECE_TP_LOCATION_CODE                    VARCHAR2(60) ,
		BANK_CHARGE_BEARER                      VARCHAR2(30) ,
		BANK_INSTRUCTION1_CODE                  VARCHAR2(30) ,
		BANK_INSTRUCTION2_CODE                  VARCHAR2(30) ,
		BANK_INSTRUCTION_DETAILS                VARCHAR2(255),
		PAYMENT_REASON_CODE                     VARCHAR2(30) ,
		PAYMENT_REASON_COMMENTS                 VARCHAR2(240),
		INACTIVE_DATE                           DATE         ,
		PAYMENT_TEXT_MESSAGE1                   VARCHAR2(256),
		PAYMENT_TEXT_MESSAGE2                   VARCHAR2(150),
		PAYMENT_TEXT_MESSAGE3                   VARCHAR2(150),
		DELIVERY_CHANNEL_CODE                   VARCHAR2(30) ,
		PAYMENT_FORMAT_CODE                     VARCHAR2(30) ,
		SETTLEMENT_PRIORITY                     VARCHAR2(30) ,
		REMIT_ADVICE_DELIVERY_METHOD            VARCHAR2(30) ,
		REMIT_ADVICE_EMAIL                      VARCHAR2(255),
		REMIT_ADVICE_FAX                        VARCHAR2(100),
		RANK					NUMBER
	      )';

	  EXCEPTION
	     WHEN OTHERS THEN
	       IBY_DATA_FIX_UTILITY_PKG.Print('could not create iby_ext_payees_10140168_tmp ->'||sqlerrm );
	       RAISE;
	  END;


	  Begin
             EXECUTE IMMEDIATE
	      'CREATE TABLE iby_external_payees_10140168 as SELECT * from iby_external_payees_all where 1=2';
	  EXCEPTION
	     WHEN OTHERS THEN
	       IBY_DATA_FIX_UTILITY_PKG.Print('could not create iby_external_payees_10140168 ->'||sqlerrm );
       	       RAISE;
	  END;

	  Begin
	    EXECUTE IMMEDIATE
	      'CREATE TABLE iby_payments_all_10140168(
		PAYMENT_ID				NUMBER(15)   ,
		OLD_EXT_PAYEE_ID			NUMBER(15)   ,
		NEW_EXT_PAYEE_ID			NUMBER(15)   
	      )';

	  EXCEPTION
	     WHEN OTHERS THEN
	       IBY_DATA_FIX_UTILITY_PKG.Print('could not create iby_payments_all_10140168 ->'||sqlerrm );
	       RAISE;
	  END;

	  Begin
	    EXECUTE IMMEDIATE
	      'CREATE TABLE iby_docs_payable_10140168(
		DOCUMENT_PAYABLE_ID			NUMBER(15)   ,
		OLD_EXT_PAYEE_ID			NUMBER(15)   ,
		NEW_EXT_PAYEE_ID			NUMBER(15)   
	      )';

	  EXCEPTION
	     WHEN OTHERS THEN
	       IBY_DATA_FIX_UTILITY_PKG.Print('could not create iby_docs_payable_10140168 ->'||sqlerrm );
	       RAISE;
	  END;

	    BEGIN
	    EXECUTE IMMEDIATE
	      'CREATE TABLE iby_pmt_instr_uses_10140168(
		INSTRUMENT_PAYMENT_USE_ID		NUMBER(15)   ,
		OLD_EXT_PMT_PARTY_ID			NUMBER(15)   ,
		NEW_EXT_PMT_PARTY_ID			NUMBER(15)   
	      )';

	  EXCEPTION
	     WHEN OTHERS THEN
	       IBY_DATA_FIX_UTILITY_PKG.Print('could not create iby_pmt_instr_uses_10140168 ->'||sqlerrm );
	       RAISE;
	  END;

	  BEGIN
	    EXECUTE IMMEDIATE
	      'CREATE TABLE IBY_EXT_PTY_PMT_MTDS_10140168(
		EXT_PARTY_PMT_MTHD_ID			NUMBER(15)   ,
		OLD_EXT_PMT_PARTY_ID			NUMBER(15)   ,
		NEW_EXT_PMT_PARTY_ID			NUMBER(15)   
	      )';

	  EXCEPTION
	     WHEN OTHERS THEN
	       IBY_DATA_FIX_UTILITY_PKG.Print('could not create iby_pmt_instr_uses_10140168 ->'||sqlerrm );
	       RAISE;
	  END;

	  BEGIN
	  EXECUTE IMMEDIATE
	      'CREATE TABLE IBY_PTY_PMT_MTDS_10140168_TMP (
          EXT_PARTY_PMT_MTHD_ID          NUMBER(15)  ,
          PAYMENT_METHOD_CODE            VARCHAR2(30) ,       
          PAYMENT_FLOW                   VARCHAR2(30)  ,      
          EXT_PMT_PARTY_ID               NUMBER(15)     ,     
          PAYMENT_FUNCTION               VARCHAR2(30)    ,    
          PRIMARY_FLAG                   VARCHAR2(1)      ,   
          INACTIVE_DATE                  DATE         ,
          RANK				NUMBER
        )';
	  EXCEPTION
	     WHEN OTHERS THEN
	       IBY_DATA_FIX_UTILITY_PKG.Print('could not create IBY_PTY_PMT_MTDS_10140168_TMP ->'||sqlerrm );
	       RAISE;
	  END;

	  BEGIN
		EXECUTE IMMEDIATE 
	    	  ' Create table iby_instr_uses_10140168_tmp(
				INSTRUMENT_PAYMENT_USE_ID  NUMBER(15)     ,
				PAYMENT_FLOW               VARCHAR2(30)   ,
				EXT_PMT_PARTY_ID           NUMBER(15)     ,
				INSTRUMENT_TYPE            VARCHAR2(30)   ,
				INSTRUMENT_ID              NUMBER(15)     ,
				PAYMENT_FUNCTION           VARCHAR2(30)   ,
				ORDER_OF_PREFERENCE        NUMBER(15)     ,
				START_DATE                 DATE           ,
				END_DATE                   DATE           ,
				DEBIT_AUTH_FLAG            VARCHAR2(1)    ,
				DEBIT_AUTH_METHOD          VARCHAR2(30)   ,
				DEBIT_AUTH_REFERENCE       VARCHAR2(100)  ,
				DEBIT_AUTH_BEGIN           DATE           ,
				DEBIT_AUTH_END             DATE           ,
				ATTRIBUTE_CATEGORY         VARCHAR2(150)  ,
				ATTRIBUTE1                 VARCHAR2(150)  ,
				ATTRIBUTE2                 VARCHAR2(150)  ,
				ATTRIBUTE3                 VARCHAR2(150)  ,
				ATTRIBUTE4                 VARCHAR2(150)  ,
				ATTRIBUTE5                 VARCHAR2(150)  ,
				ATTRIBUTE6                 VARCHAR2(150)  ,
				ATTRIBUTE7                 VARCHAR2(150)  ,
				ATTRIBUTE8                 VARCHAR2(150)  ,
				ATTRIBUTE9                 VARCHAR2(150)  ,
				ATTRIBUTE10                VARCHAR2(150)  ,
				ATTRIBUTE11                VARCHAR2(150)  ,
				ATTRIBUTE12                VARCHAR2(150)  ,
				ATTRIBUTE13                VARCHAR2(150)  ,
				ATTRIBUTE14                VARCHAR2(150)  ,
				ATTRIBUTE15                VARCHAR2(150)  ,
				RANK			   NUMBER
			)';
	  EXCEPTION
	    WHEN OTHERS THEN
	      IBY_DATA_FIX_UTILITY_PKG.Print('Error while Creating table iby_instr_uses_10140168_tmp ' ||SQLERRM );
	        RAISE;
	  END;
	

	  ------------------------------------------------------------------
	  --  Step 3: Insert all affected transactions in temporary table 
	  ------------------------------------------------------------------

	  /*******  Duplicate Payee Records  ******/
	  BEGIN
 	     EXECUTE IMMEDIATE 'Insert into iby_ext_payees_10140168_tmp
		  (
		    EXT_PAYEE_ID				,
		    PAYEE_PARTY_ID			,
		    PAYMENT_FUNCTION		,
		    EXCLUSIVE_PAYMENT_FLAG      ,
		    PARTY_SITE_ID               ,
		    SUPPLIER_SITE_ID            ,
		    ORG_ID                      ,
		    ORG_TYPE                    ,
		    DEFAULT_PAYMENT_METHOD_CODE ,
		    ECE_TP_LOCATION_CODE        ,
		    BANK_CHARGE_BEARER          ,
		    BANK_INSTRUCTION1_CODE      ,
		    BANK_INSTRUCTION2_CODE      ,
		    BANK_INSTRUCTION_DETAILS    ,
		    PAYMENT_REASON_CODE         ,
		    PAYMENT_REASON_COMMENTS     ,
		    INACTIVE_DATE               ,
		    PAYMENT_TEXT_MESSAGE1       ,
		    PAYMENT_TEXT_MESSAGE2       ,
		    PAYMENT_TEXT_MESSAGE3       ,
		    DELIVERY_CHANNEL_CODE       ,
		    PAYMENT_FORMAT_CODE         ,
		    SETTLEMENT_PRIORITY         ,
		    REMIT_ADVICE_DELIVERY_METHOD,
		    REMIT_ADVICE_EMAIL          ,
		    REMIT_ADVICE_FAX            ,
		    RANK
		   )
		   SELECT
		    A.EXT_PAYEE_ID				,
		    A.PAYEE_PARTY_ID			,
		    A.PAYMENT_FUNCTION		,
		    A.EXCLUSIVE_PAYMENT_FLAG      ,
		    A.PARTY_SITE_ID               ,
		    A.SUPPLIER_SITE_ID            ,
		    A.ORG_ID                      ,
		    A.ORG_TYPE                    ,
		    A.DEFAULT_PAYMENT_METHOD_CODE ,
		    A.ECE_TP_LOCATION_CODE        ,
		    A.BANK_CHARGE_BEARER          ,
		    A.BANK_INSTRUCTION1_CODE      ,
		    A.BANK_INSTRUCTION2_CODE      ,
		    A.BANK_INSTRUCTION_DETAILS    ,
		    A.PAYMENT_REASON_CODE         ,
		    A.PAYMENT_REASON_COMMENTS     ,
		    A.INACTIVE_DATE               ,
		    A.PAYMENT_TEXT_MESSAGE1       ,
		    A.PAYMENT_TEXT_MESSAGE2       ,
		    A.PAYMENT_TEXT_MESSAGE3       ,
		    A.DELIVERY_CHANNEL_CODE       ,
		    A.PAYMENT_FORMAT_CODE         ,
		    A.SETTLEMENT_PRIORITY         ,
		    A.REMIT_ADVICE_DELIVERY_METHOD,
		    A.REMIT_ADVICE_EMAIL          ,
		    A.REMIT_ADVICE_FAX            ,
		    rank() over(partition by A.PAYEE_PARTY_ID, a.payment_function, a.party_site_id, a.supplier_site_id, a.org_id, a.org_type
				order by A.PAYEE_PARTY_ID, a.last_update_date desc, a.ext_payee_id) as ranking 
		   FROM iby_external_payees_all a
		   WHERE EXISTS (SELECT ''duplicates'' 
				 FROM iby_external_payees_all b 
				 WHERE a.payee_party_id = b.payee_party_id 
				 AND a.payment_function = b.payment_function 
				 AND NVL(a.party_site_id, 0) = NVL(b.party_site_id, 0) 
				 AND NVL(a.supplier_site_id, 0) = NVL(b.supplier_site_id, 0) 
				 AND NVL(a.org_id, 0) = NVL(b.org_id, 0) 
				 AND NVL(a.org_type, ''0'') = NVL(b.org_type, ''0'') 
				 AND a.ext_payee_id <> b.ext_payee_id
			)
		   ORDER BY A.PAYEE_PARTY_ID, a.last_update_date DESC, a.ext_payee_id';
	  EXCEPTION
	  WHEN OTHERS THEN
	     IBY_DATA_FIX_UTILITY_PKG.Print('Exception in inserting records into '|| 
				 'iby_ext_payees_10140168_tmp for affected '||
				 ' transactions - '||SQLERRM );
             RAISE;
	  END;  

	  FOR get_dup_payees_rec IN get_dup_payees
	  LOOP
		BEGIN
		     EXECUTE IMMEDIATE 'Insert into iby_payments_all_10140168( PAYMENT_ID , OLD_EXT_PAYEE_ID)
		       (select PAYMENT_ID, EXT_PAYEE_ID from iby_payments_all where ext_payee_id = '||
		       get_dup_payees_rec.EXT_PAYEE_ID ||')';
	      
		EXCEPTION
		   WHEN OTHERS THEN
		       IBY_DATA_FIX_UTILITY_PKG.Print('Exception in inserting records into '|| 
					 'iby_payments_all_10140168 for affected '||
					 ' transactions - '||SQLERRM );
 	           RAISE;
		END;    
		  
		BEGIN
		    EXECUTE IMMEDIATE 'Insert into iby_docs_payable_10140168(
			     DOCUMENT_PAYABLE_ID ,
			     OLD_EXT_PAYEE_ID
			      ) (select document_payable_id , EXT_PAYEE_ID from iby_docs_payable_all where ext_payee_id = '||
				 get_dup_payees_rec.EXT_PAYEE_ID||')';
		EXCEPTION
		    WHEN OTHERS THEN
		       IBY_DATA_FIX_UTILITY_PKG.Print('Exception in inserting records into '|| 
					 'iby_docs_payable_10140168 for affected '||
					 ' transactions - '||SQLERRM );
	            RAISE;
		END;    

		BEGIN
		    EXECUTE IMMEDIATE
		      'Insert into iby_pmt_instr_uses_10140168(
			  instrument_payment_use_id ,
			  OLD_EXT_PMT_PARTY_ID) 
			  (select instrument_payment_use_id , ext_pmt_party_id from iby_pmt_instr_uses_all where ext_pmt_party_id = '||
			       get_dup_payees_rec.EXT_PAYEE_ID ||')';

	        EXCEPTION
		    WHEN OTHERS THEN
		       IBY_DATA_FIX_UTILITY_PKG.Print('Exception in inserting records into '|| 
					 'iby_pmt_instr_uses_10140168 for affected '||
					 ' transactions - '||SQLERRM );
	            RAISE;
		END; 

		BEGIN
		    EXECUTE IMMEDIATE
		      'Insert into IBY_EXT_PTY_PMT_MTDS_10140168(
			  EXT_PARTY_PMT_MTHD_ID ,
			  OLD_EXT_PMT_PARTY_ID
			  ) 
			  (SELECT EXT_PARTY_PMT_MTHD_ID , ext_pmt_party_id  
			   FROM IBY_EXT_PARTY_PMT_MTHDS 
			   WHERE ext_pmt_party_id = '|| 
			       get_dup_payees_rec.EXT_PAYEE_ID ||')';

	        EXCEPTION
		WHEN OTHERS THEN
		       IBY_DATA_FIX_UTILITY_PKG.Print('Exception in inserting records into '|| 
					 'IBY_EXT_PTY_PMT_MTDS_10140168 for affected '||
					 ' transactions - '||SQLERRM );
 		   RAISE;
		END; 

	    END LOOP;
	  ------------------------------------------------------------------
	  --  Step 4: Report all the affected transactions in Log file 
	  ------------------------------------------------------------------
	  /** DISPLAY UNACCOUNTED AFFECTED PAYMENT ADJUSTED TRANSACTIONS **/
	 BEGIN
	  EXECUTE IMMEDIATE  
	      'SELECT count(*) 
		 FROM iby_ext_payees_10140168_tmp' 
		 INTO l_count;
	  EXCEPTION
	     WHEN OTHERS THEN
		  IBY_DATA_FIX_UTILITY_PKG.Print('Exception in selecting count from'|| 
				    ' iby_ext_payees_10140168_tmp => '||SQLERRM );
	       RAISE;
	  END; 

	  IF (l_count > 0) THEN 
	    IBY_DATA_FIX_UTILITY_PKG.Print('Below records are identified as duplicate records. Temporary table storing Duplicate payee information is iby_ext_payees_10140168_tmp.');
	    IBY_DATA_FIX_UTILITY_PKG.Print('A Payee will be considered as duplicate based on payee_party_id, party_site_id, supplier_site_id,ORG_ID,ORG_TYPE and PAYMENT_FUNCTION.');
	    IBY_DATA_FIX_UTILITY_PKG.Print('Fix script will retain the records with Rank 1 and other records and their associations will be deleted.');            
	    IBY_DATA_FIX_UTILITY_PKG.Print('Among the duplicate records, user can change the rank in iby_ext_payees_10140168_tmp '||
										'to retain most suitable record based on other attributes');
    
	    BEGIN
	      IBY_DATA_FIX_UTILITY_PKG.Print_html_table                           
	       ( 'EXT_PAYEE_ID,PAYEE_PARTY_ID,PAYMENT_FUNCTION,EXCLUSIVE_PAYMENT_FLAG,PARTY_SITE_ID,SUPPLIER_SITE_ID,ORG_ID,'||
	         'ORG_TYPE,DEFAULT_PAYMENT_METHOD_CODE,ECE_TP_LOCATION_CODE,BANK_CHARGE_BEARER,BANK_INSTRUCTION1_CODE,BANK_INSTRUCTION2_CODE,'||
		 'BANK_INSTRUCTION_DETAILS,PAYMENT_REASON_CODE,PAYMENT_REASON_COMMENTS,INACTIVE_DATE,PAYMENT_TEXT_MESSAGE1,PAYMENT_TEXT_MESSAGE2,'||
		 'PAYMENT_TEXT_MESSAGE3,DELIVERY_CHANNEL_CODE,PAYMENT_FORMAT_CODE,SETTLEMENT_PRIORITY,REMIT_ADVICE_DELIVERY_METHOD,REMIT_ADVICE_EMAIL,'||
		 'REMIT_ADVICE_FAX,RANK',
		'iby_ext_payees_10140168_tmp',                                
		NULL,                                                         
		'iby_dup_payees_sel_fix.sql');                                              
	    EXCEPTION                                                          
	      WHEN OTHERS THEN
		IBY_DATA_FIX_UTILITY_PKG.Print('Exception in call to '||
				 'IBY_DATA_FIX_UTILITY_PKG.Print_html_table for printing'
				 ||' Duplicate Payees ->'||SQLERRM);
	       RAISE;
	    END;
          ELSE
  	    IBY_DATA_FIX_UTILITY_PKG.Print('Duplicate payee records does not exist.');            
	  END IF;
	  
	  ---------------------------------------------------------------------
	  --  Step 4: User need to follow the  next  to fix the issue
	  ---------------------------------------------------------------------

	  l_message :=  '_______________________________________'||
			'_______________________________________';
	  IBY_DATA_FIX_UTILITY_PKG.Print(l_message);
	  l_message :=  'Following are the next steps to be followed '||
			'to fix the issue, if any';
	  IBY_DATA_FIX_UTILITY_PKG.Print(l_message);
	  
	  l_message :=  '1. To fix the identified transactions run '||
			'iby_dup_payee_fix.sql which is present '||
			'in $IBY_TOP/patch/115/sql.';
	  IBY_DATA_FIX_UTILITY_PKG.Print(l_message);

	  l_message :=  '2. If data is not corrected, contact'||
			' Oracle Support and supply files'||
			' 10140168-diag-HH24:MI:SS.html and 10140168-fix-HH24:MI:SS.html.';

	  IBY_DATA_FIX_UTILITY_PKG.Print(l_message);

	  IBY_DATA_FIX_UTILITY_PKG.Print('</body></html>');
    
	  IBY_DATA_FIX_UTILITY_PKG.Close_Log_Out_Files; 
	  dbms_output.put_line('--------------------------------------------------'||
				 '-----------------------------');
	  dbms_output.put_line(l_file_location||' is the log file created');
	  dbms_output.put_line('--------------------------------------------------'||
				  '-----------------------------');

	EXCEPTION

	   WHEN OTHERS THEN
	      IBY_DATA_FIX_UTILITY_PKG.Print(l_message||sqlerrm);
	      l_message := 'after '||l_message||'';
	      IBY_DATA_FIX_UTILITY_PKG.Print(l_message);

	      l_message := 'Exception :: '||SQLERRM||'';
	      IBY_DATA_FIX_UTILITY_PKG.Print(l_message);

	      APP_EXCEPTION.RAISE_EXCEPTION;
   END;
/
COMMIT;
EXIT;


