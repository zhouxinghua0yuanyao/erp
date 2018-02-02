REM $Header: iby_dup_payee_fix.sql 120.2.12020000.2 2014/08/21 10:49:13 asarada noship $
REM +=================================================================+
REM |                Copyright (c) 2014, 2014 Oracle Corporation      |
REM |                   Redwood Shores, California, USA               |
REM |                        All rights reserved.                     |
REM +=================================================================+
REM | FILENAME                                                              |
REM |     IBY_DUP_PAYEE_FIX.sql						    |
REM |                                                                       |
REM | DESCRIPTION                                                           |
REM |     This script will remove all duplicate payees and their references.|
REM |                                                                       |
REM | BACKUP TABLES:                                                        |
REM |     The following backup tables are created by this GDF               |
REM |                                                                       |
REM |       iby_external_payees_10140168 -- DRIVER TABLE                    |
REM |       iby_payments_all_10140168    -- IBY_PAYMENTS_ALL		    |
REM |       iby_docs_payable_10140168    -- IBY_DOCS_PAYABLE_ALL	    |
REM |       iby_pmt_instr_uses_10140168  -- IBY_PMT_INSTR_USES_ALL          |
REM |                                                                       |
REM |     Duplicate external Payee records will be selected in to  table  |
REM |     iby_ext_payees_10140168_tmp. Records marked with Rank 1 will be   |
REM |	  retained and other records will be deleted from the base table.   |
REM |	  A Payee will be considered as duplicate based on payee_party_id,  |
REM |     party_site_id, supplier_site_id, ORG_ID, ORG_TYPE and PAYMENT_FUNCTION.|
REM |     Among the duplicate records, user can change the rank in          |
REM |     iby_ext_payees_10140168_tmp to retain most suitable record based  |
REM |     on other attributes. 						    |
REM |									    |						
REM |                                                                       |
REM | USAGE                                                                 |
REM |                                                                       |
REM |   The script IBY_DUP_PAYEES_SEL.sql should be executed prior to       |
REM |   execution of this script.                                           |
REM |                                                                       |
REM |                                                                       |
REM |                                                                       |
REM | HISTORY                                                               |
REM |     20-DEC-2010 GMAHESWA Built GDF                                    |
REM +=======================================================================+
REM dbdrv:none
REM $Header: iby_dup_payee_fix.sql 120.2.12020000.2 2014/08/21 10:49:13 asarada noship $

SET VERIFY OFF;
SET SERVEROUTPUT ON;
WHENEVER SQLERROR EXIT FAILURE ROLLBACK;
WHENEVER OSERROR EXIT FAILURE ROLLBACK;

DECLARE
  l_payee_party_id NUMBER(15);
  l_party_site_id NUMBER(15);
  l_supplier_site_id NUMBER(15);
  l_payment_function VARCHAR2(30);
  l_ext_payee_id NUMBER(15);
  l_org_id NUMBER(15);
  l_org_type VARCHAR2(30);
  l_payee_context VARCHAR2(1000);
  type typ_payee_context_rec_tab IS TABLE OF NUMBER(15) INDEX BY VARCHAR2(1000);
  l_payee_context_tbl typ_payee_context_rec_tab;

  p_calling_sequence        VARCHAR2(50) := 'IBY_DUP_PAYEES_FIX.sql';
  l_bug_Id NUMBER := 10140168;
  l_message           VARCHAR2(500); 
  l_debug_info        VARCHAR2(4000);  
  l_file_location     v$parameter.value%type;

  L_MTHD_COUNT NUMBER;
  L_MAX_RANK NUMBER;
  l_instr_count NUMBER(6);

BEGIN
	  IBY_DATA_FIX_UTILITY_PKG.Open_Log_File(10140168||'-fix',l_file_location);
	  IBY_DATA_FIX_UTILITY_PKG.Print('<html><body>');
	  dbms_output.put_line('--------------------------------------------------'||
				 '-----------------------------');
	  dbms_output.put_line(l_file_location||' is the log file created');
	  dbms_output.put_line('--------------------------------------------------'||
				  '-----------------------------');
	  --------------------------------------------------------------------------
	  -- Step 1: Create Mapping table for future reference 
	  --------------------------------------------------------------------------

 	  BEGIN
	    EXECUTE IMMEDIATE 'Drop table iby_ext_payees_10140168_Ref';
	  EXCEPTION
	    WHEN OTHERS THEN
	       NULL;
	  END;

 	  BEGIN
	    EXECUTE IMMEDIATE 'Create table iby_ext_payees_10140168_Ref ( OLD_EXT_PAYEE_ID NUMBER, NEW_EXT_PAYEE_ID NUMBER)';
	  EXCEPTION
	    WHEN OTHERS THEN
	      IBY_DATA_FIX_UTILITY_PKG.Print('could not Create '||
				'iby_ext_payees_10140168_Ref->' ||sqlerrm );
	  END;

 	  BEGIN
	    EXECUTE IMMEDIATE 
		'INSERT INTO iby_ext_payees_10140168_Ref(OLD_EXT_PAYEE_ID, NEW_EXT_PAYEE_ID)
		(SELECT A.EXT_PAYEE_ID , B.EXT_PAYEE_ID 
		FROM iby_ext_payees_10140168_tmp A, iby_ext_payees_10140168_tmp B 
		WHERE A.RANK <> 1 
		AND A.payee_party_id = B.payee_party_id 
		AND A.PAYMENT_FUNCTION = B.PAYMENT_FUNCTION 
		AND nvl(A.party_site_id,0) = nvl(B.PARTY_SITE_ID,0)
  	        AND NVL(a.supplier_site_id, 0) = NVL(b.supplier_site_id, 0) 
		AND nvl(A.ORG_ID,0) = nvl(B.ORG_ID,0)
		AND nvl(A.ORG_TYPE,''X'') = nvl(B.ORG_TYPE,''X'')
		AND B.RANK = 1)';
	  EXCEPTION
	    WHEN OTHERS THEN
	      IBY_DATA_FIX_UTILITY_PKG.Print('Error while inserting data into iby_ext_payees_10140168_Ref ' ||sqlerrm );
              RAISE;
	  END;

	  --------------------------------------------------------------------------
	  -- Step 2: Update all references to point to new payee id.
	  --------------------------------------------------------------------------

 	  BEGIN
	    EXECUTE IMMEDIATE 
		'UPDATE iby_payments_all 
		 SET ext_payee_id = (
			SELECT new_ext_payee_id 
			FROM iby_ext_payees_10140168_Ref 
			WHERE OLD_EXT_PAYEE_ID = ext_payee_id) 
		  WHERE ext_payee_id IN (SELECT OLD_EXT_PAYEE_ID FROM iby_ext_payees_10140168_Ref)';

	  EXCEPTION
	    WHEN OTHERS THEN
	      IBY_DATA_FIX_UTILITY_PKG.Print('Error while updating data in iby_payments_all ' ||SQLERRM );
              RAISE;
	  END;

 	  BEGIN
	    EXECUTE IMMEDIATE 
		'UPDATE iby_payments_all_10140168 bkp
		 SET bkp.new_ext_payee_id = (
			SELECT new_ext_payee_id 
			FROM iby_ext_payees_10140168_Ref ref
			WHERE ref.OLD_EXT_PAYEE_ID = bkp.old_ext_payee_id) 
		  WHERE bkp.old_ext_payee_id IN (SELECT OLD_EXT_PAYEE_ID FROM iby_ext_payees_10140168_Ref)';

	  EXCEPTION
	    WHEN OTHERS THEN
	      IBY_DATA_FIX_UTILITY_PKG.Print('Error while updating data in iby_payments_all_10140168 ' ||SQLERRM );
              RAISE;
	  END;

 	  BEGIN
	    EXECUTE IMMEDIATE 
		'UPDATE iby_docs_payable_all bkp
		 SET bkp.ext_payee_id = (
			SELECT new_ext_payee_id 
			FROM iby_ext_payees_10140168_Ref ref
			WHERE ref.OLD_EXT_PAYEE_ID = bkp.ext_payee_id) 
		  WHERE bkp.ext_payee_id IN (SELECT OLD_EXT_PAYEE_ID FROM iby_ext_payees_10140168_Ref)';

	  EXCEPTION
	    WHEN OTHERS THEN
	      IBY_DATA_FIX_UTILITY_PKG.Print('Error while updating data in iby_docs_payable_all ' ||SQLERRM );
              RAISE;
	  END;
	  
	  BEGIN
	    EXECUTE IMMEDIATE 
		'UPDATE iby_docs_payable_10140168 bkp
		 SET bkp.new_ext_payee_id = (
			SELECT new_ext_payee_id 
			FROM iby_ext_payees_10140168_Ref ref
			WHERE ref.OLD_EXT_PAYEE_ID = bkp.old_ext_payee_id) 
		  WHERE bkp.old_ext_payee_id IN (SELECT OLD_EXT_PAYEE_ID FROM iby_ext_payees_10140168_Ref)';

	  EXCEPTION
	    WHEN OTHERS THEN
	      IBY_DATA_FIX_UTILITY_PKG.Print('Error while updating data in iby_docs_payable_10140168 ' ||SQLERRM );
		RAISE;
	  END;

 	  BEGIN
	    EXECUTE IMMEDIATE 
		'UPDATE iby_pmt_instr_uses_all bkp
		 SET bkp.ext_pmt_party_id = (
			SELECT new_ext_payee_id 
			FROM iby_ext_payees_10140168_Ref ref
			WHERE ref.OLD_EXT_PAYEE_ID = bkp.ext_pmt_party_id) 
		  WHERE bkp.ext_pmt_party_id IN (SELECT OLD_EXT_PAYEE_ID FROM iby_ext_payees_10140168_Ref)';

	  EXCEPTION
	    WHEN OTHERS THEN
	      IBY_DATA_FIX_UTILITY_PKG.Print('Error while updating data in iby_pmt_instr_uses_all ' ||SQLERRM );
              RAISE;
	  END;

	  BEGIN
	    EXECUTE IMMEDIATE 
		'UPDATE iby_pmt_instr_uses_10140168 bkp
		 SET bkp.new_ext_pmt_party_id = (
			SELECT new_ext_payee_id 
			FROM iby_ext_payees_10140168_Ref ref
			WHERE ref.OLD_EXT_PAYEE_ID = bkp.old_ext_pmt_party_id) 
		  WHERE bkp.old_ext_pmt_party_id IN (SELECT OLD_EXT_PAYEE_ID FROM iby_ext_payees_10140168_Ref)';

	  EXCEPTION
	    WHEN OTHERS THEN
	      IBY_DATA_FIX_UTILITY_PKG.Print('Error while updating data in iby_pmt_instr_uses_10140168 ' ||SQLERRM );
              RAISE;
	  END;

 	  BEGIN
	    EXECUTE IMMEDIATE 
		'UPDATE IBY_EXT_PARTY_PMT_MTHDS bkp
		 SET bkp.ext_pmt_party_id = (
			SELECT new_ext_payee_id 
			FROM iby_ext_payees_10140168_Ref ref
			WHERE ref.OLD_EXT_PAYEE_ID = bkp.ext_pmt_party_id) 
		  WHERE bkp.ext_pmt_party_id IN (SELECT OLD_EXT_PAYEE_ID FROM iby_ext_payees_10140168_Ref)';

	  EXCEPTION
	    WHEN OTHERS THEN
	      IBY_DATA_FIX_UTILITY_PKG.Print('Error while updating data in IBY_EXT_PARTY_PMT_MTHDS ' ||SQLERRM );
              RAISE;
	  END;
	  BEGIN
	    EXECUTE IMMEDIATE 
		'UPDATE IBY_EXT_PTY_PMT_MTDS_10140168 bkp
		 SET bkp.new_ext_pmt_party_id = (
			SELECT new_ext_payee_id 
			FROM iby_ext_payees_10140168_Ref ref
			WHERE ref.OLD_EXT_PAYEE_ID = bkp.old_ext_pmt_party_id) 
		  WHERE bkp.old_ext_pmt_party_id IN (SELECT OLD_EXT_PAYEE_ID FROM iby_ext_payees_10140168_Ref)';

	  EXCEPTION
	    WHEN OTHERS THEN
	      IBY_DATA_FIX_UTILITY_PKG.Print('Error while updating data in IBY_EXT_PTY_PMT_MTDS_10140168 ' ||SQLERRM );
              RAISE;
	  END;

	  BEGIN
	    EXECUTE IMMEDIATE 
		'INSERT INTO iby_external_payees_10140168 SELECT * 
		FROM iby_external_payees_all 
		WHERE ext_payee_id IN (SELECT OLD_EXT_PAYEE_ID FROM iby_ext_payees_10140168_Ref)';

	  EXCEPTION
	    WHEN OTHERS THEN
	      IBY_DATA_FIX_UTILITY_PKG.Print('Error while inserting data in iby_external_payees_10140168 ' ||SQLERRM );
              RAISE;
	  END;

 	  BEGIN
	    EXECUTE IMMEDIATE 
		'DELETE FROM iby_external_payees_all WHERE ext_payee_id IN (SELECT OLD_EXT_PAYEE_ID FROM iby_ext_payees_10140168_Ref)';

	  EXCEPTION
	    WHEN OTHERS THEN
	      IBY_DATA_FIX_UTILITY_PKG.Print('Error while deleting data in iby_external_payees_all ' ||SQLERRM );
             RAISE;
	  END;

	  ---------------------------------------------------------------------
	  -- Step 3: Check for Duplicate records in child tables after updates.
	  ---------------------------------------------------------------------
	  /*Start of Checking for Duplicate records in iby_pmt_instr_uses_all*/
	  
	  BEGIN
 	    EXECUTE IMMEDIATE 
	    	  ' SELECT count(*) FROM 
			(SELECT instrument_payment_use_id,payment_flow, ext_pmt_party_id, instrument_type, instrument_id, payment_function, 
			rank() over (PARTITION BY payment_flow, ext_pmt_party_id, instrument_type, instrument_id, payment_function
				     ORDER BY END_DATE DESC, START_DATE, CREATION_DATE, instrument_payment_use_id )RANK
			FROM iby_pmt_instr_uses_all WHERE payment_flow = ''DISBURSEMENTS'')
			WHERE rank > 1' INTO l_instr_count;
	    IF 	l_instr_count > 0 THEN 
	    
		    BEGIN
			EXECUTE IMMEDIATE 
			  'INSERT INTO iby_instr_uses_10140168_tmp (
				INSTRUMENT_PAYMENT_USE_ID ,
				PAYMENT_FLOW             ,
				EXT_PMT_PARTY_ID         ,
				INSTRUMENT_TYPE          ,
				INSTRUMENT_ID            ,
				PAYMENT_FUNCTION         ,
				ORDER_OF_PREFERENCE      ,
				START_DATE               ,
				END_DATE                 ,
				DEBIT_AUTH_FLAG          ,
				DEBIT_AUTH_METHOD        ,
				DEBIT_AUTH_REFERENCE     ,
				DEBIT_AUTH_BEGIN         ,
				DEBIT_AUTH_END           ,
				ATTRIBUTE_CATEGORY       ,
				ATTRIBUTE1               ,
				ATTRIBUTE2               ,
				ATTRIBUTE3               ,
				ATTRIBUTE4               ,
				ATTRIBUTE5               ,
				ATTRIBUTE6               ,
				ATTRIBUTE7               ,
				ATTRIBUTE8               ,
				ATTRIBUTE9               ,
				ATTRIBUTE10              ,
				ATTRIBUTE11              ,
				ATTRIBUTE12              ,
				ATTRIBUTE13              ,
				ATTRIBUTE14              ,
				ATTRIBUTE15              ,
				RANK 
			     )( SELECT * FROM 
			        (SELECT
				a.INSTRUMENT_PAYMENT_USE_ID ,
				a.PAYMENT_FLOW             ,
				a.EXT_PMT_PARTY_ID         ,
				a.INSTRUMENT_TYPE          ,
				a.INSTRUMENT_ID            ,
				a.PAYMENT_FUNCTION         ,
				a.ORDER_OF_PREFERENCE      ,
				a.START_DATE               ,
				a.END_DATE                 ,
				a.DEBIT_AUTH_FLAG          ,
				a.DEBIT_AUTH_METHOD        ,
				a.DEBIT_AUTH_REFERENCE     ,
				a.DEBIT_AUTH_BEGIN         ,
				a.DEBIT_AUTH_END           ,
				a.ATTRIBUTE_CATEGORY       ,
				a.ATTRIBUTE1               ,
				a.ATTRIBUTE2               ,
				a.ATTRIBUTE3               ,
				a.ATTRIBUTE4               ,
				a.ATTRIBUTE5               ,
				a.ATTRIBUTE6               ,
				a.ATTRIBUTE7               ,
				a.ATTRIBUTE8               ,
				a.ATTRIBUTE9               ,
				a.ATTRIBUTE10              ,
				a.ATTRIBUTE11              ,
				a.ATTRIBUTE12              ,
				a.ATTRIBUTE13              ,
				a.ATTRIBUTE14              ,
				a.ATTRIBUTE15       	   ,
				rank() over (PARTITION BY payment_flow, ext_pmt_party_id, instrument_type, instrument_id, payment_function
					     ORDER BY END_DATE DESC, START_DATE, CREATION_DATE, instrument_payment_use_id ) RANK
				FROM iby_pmt_instr_uses_all a 
				WHERE payment_flow = ''DISBURSEMENTS'' 
				AND EXISTS( SELECT ''X'' 
					    FROM iby_pmt_instr_uses_all b 
					    WHERE a.ext_pmt_party_id = b.ext_pmt_party_id 
					    AND a.instrument_type = b.instrument_type 
					    AND a.instrument_id = b.instrument_id 
					    AND a.payment_function = b.payment_function 
					    AND a.instrument_payment_use_id <> b.instrument_payment_use_id)
			      ) WHERE RANK >1)';
		    EXCEPTION
		    WHEN OTHERS THEN
		      IBY_DATA_FIX_UTILITY_PKG.Print('Error while Inserting Duplicate Payment Instructions to iby_instr_uses_10140168_tmp ' ||SQLERRM );
	             RAISE;
		    END;

		    BEGIN
			EXECUTE IMMEDIATE 
			  ' DELETE FROM iby_pmt_instr_uses_all WHERE instrument_payment_use_id IN(
				SELECT instrument_payment_use_id FROM iby_instr_uses_10140168_tmp)';
		    EXCEPTION
		    WHEN OTHERS THEN
		      IBY_DATA_FIX_UTILITY_PKG.Print('Error while Deleting Duplicate records in iby_pmt_instr_uses_all ' ||SQLERRM );
	              RAISE;
		    END;
          END IF;
          EXCEPTION
	    WHEN OTHERS THEN
	      IBY_DATA_FIX_UTILITY_PKG.Print('Exception while Checking for Duplicate records in iby_pmt_instr_uses_all ' ||SQLERRM );
	      RAISE;
          END;

	  /*  Check for duplicate order of preference for a payee in payment instrument uses all */
	  BEGIN
		EXECUTE IMMEDIATE 
		'SELECT nvl(MAX(rank),0) FROM (
		SELECT instrument_payment_use_id,
		rank() over (PARTITION BY payment_flow, ext_pmt_party_id, instrument_type, payment_function, ORDER_OF_PREFERENCE
		ORDER BY instrument_payment_use_id, ORDER_OF_PREFERENCE) rank 
		FROM  iby_pmt_instr_uses_all 
		WHERE ext_pmt_party_id IN (SELECT ext_pmt_party_id FROM 
		(SELECT ext_pmt_party_id, COUNT(ORDER_OF_PREFERENCE) 
		FROM iby_pmt_instr_uses_all 
		WHERE payment_flow = ''DISBURSEMENTS''
		AND NVL(end_date , SYSDATE+1) > SYSDATE
		GROUP BY ext_pmt_party_id, ORDER_OF_PREFERENCE 
		HAVING COUNT( ORDER_OF_PREFERENCE) >1
		)))' INTO l_max_rank;

		FOR I IN 1..L_MAX_RANK
		LOOP
		BEGIN
 		   EXECUTE IMMEDIATE 
			'UPDATE iby_pmt_instr_uses_all a 
			SET ORDER_OF_PREFERENCE = (SELECT MAX(ORDER_OF_PREFERENCE)+1 
						   FROM iby_pmt_instr_uses_all b 
						   WHERE a.ext_pmt_party_id = b.ext_pmt_party_id)
			WHERE a.instrument_payment_use_id IN 
				(SELECT instrument_payment_use_id 
				 FROM (
					SELECT c.instrument_payment_use_id,
					rank() over (PARTITION BY c.payment_flow, c.ext_pmt_party_id, c.instrument_type, c.payment_function, c.ORDER_OF_PREFERENCE
					ORDER BY c.instrument_payment_use_id, c.ORDER_OF_PREFERENCE) rank 
					FROM  iby_pmt_instr_uses_all c
					WHERE c.ext_pmt_party_id IN (SELECT ext_pmt_party_id FROM 
						(SELECT d.ext_pmt_party_id, COUNT(ORDER_OF_PREFERENCE) 
						 FROM iby_pmt_instr_uses_all d
						 WHERE d.payment_flow = ''DISBURSEMENTS''
						 AND NVL(d.end_date , SYSDATE+1) > SYSDATE
						 GROUP BY d.ext_pmt_party_id, d.ORDER_OF_PREFERENCE 
						 HAVING COUNT( d.ORDER_OF_PREFERENCE) >1
						)
				     )
				ORDER BY ext_pmt_party_id)
				WHERE rank >1
			)';
	        EXCEPTION
	          WHEN OTHERS THEN
			IBY_DATA_FIX_UTILITY_PKG.Print('Exception while updating order_of_preference in iby_pmt_instr_uses_all ' ||SQLERRM );
			RAISE;
		END;
	        END LOOP;	

          EXCEPTION
	    WHEN OTHERS THEN
	      IBY_DATA_FIX_UTILITY_PKG.Print('Exception while Checking for Duplicate order of preference for a payee in iby_pmt_instr_uses_all ' ||SQLERRM );
	      RAISE;
          END;
	  /*End of Checking for Duplicate records in iby_pmt_instr_uses_all*/

	  /*Checking for Duplicate records in IBY_EXT_PARTY_PMT_MTHDS*/
	  
	  BEGIN
 	    EXECUTE IMMEDIATE 
	    	  'SELECT count(*) FROM 
			(SELECT mth.*,
			rank() over (PARTITION BY mth.payment_method_code, mth.payment_flow, mth.ext_pmt_party_id, mth.payment_function
				     ORDER BY decode(mth.primary_flag, ''Y'',''A'',''N'',''B'',''C'')) RANK
			FROM IBY_EXT_PARTY_PMT_MTHDS mth WHERE payment_flow = ''DISBURSEMENTS'')
			WHERE rank>1' INTO l_mthd_count;
	    IF 	l_mthd_count > 0 THEN 
		BEGIN
		  EXECUTE IMMEDIATE  'INSERT INTO IBY_PTY_PMT_MTDS_10140168_TMP (SELECT * FROM 
			(SELECT mth.EXT_PARTY_PMT_MTHD_ID ,
				mth.PAYMENT_METHOD_CODE   ,
				mth.PAYMENT_FLOW          ,
				mth.EXT_PMT_PARTY_ID      ,
				mth.PAYMENT_FUNCTION      ,
				mth.PRIMARY_FLAG          ,
				mth.INACTIVE_DATE         ,
				rank() over (PARTITION BY mth.payment_method_code, mth.payment_flow, mth.ext_pmt_party_id, mth.payment_function
				     ORDER BY decode(mth.primary_flag, ''Y'',''A'',''N'',''B'',''C'')) RANK
			FROM IBY_EXT_PARTY_PMT_MTHDS mth WHERE payment_flow = ''DISBURSEMENTS'')
			WHERE rank>1)';	
		EXCEPTION
		    WHEN OTHERS THEN
		      IBY_DATA_FIX_UTILITY_PKG.Print('Error while Inserting duplicate payment methods into IBY_PTY_PMT_MTDS_10140168_TMP' ||SQLERRM );
	              RAISE;
		END;
		
		BEGIN
		  EXECUTE IMMEDIATE  'DELETE FROM IBY_EXT_PARTY_PMT_MTHDS WHERE ext_party_pmt_mthd_id IN (SELECT ext_party_pmt_mthd_id FROM IBY_PTY_PMT_MTDS_10140168_TMP)';	
		EXCEPTION
		    WHEN OTHERS THEN
		      IBY_DATA_FIX_UTILITY_PKG.Print('Error while Deleting duplicate payment methods from IBY_EXT_PARTY_PMT_MTHDS' ||SQLERRM );
	              RAISE;
		END;
	    END IF;

	  EXCEPTION
	    WHEN OTHERS THEN
	      IBY_DATA_FIX_UTILITY_PKG.Print('Error while checking duplicate records from IBY_EXT_PARTY_PMT_MTHDS' ||SQLERRM );
              RAISE;
	  END;
	  
	  /* check for duplicate primary payment methods for a payee */
	  BEGIN
	     EXECUTE IMMEDIATE 
		'UPDATE IBY_EXT_PARTY_PMT_MTHDS SET PRIMARY_FLAG = ''N'' 
		WHERE EXT_PARTY_PMT_MTHD_ID IN (SELECT EXT_PARTY_PMT_MTHD_ID FROM (
		SELECT EXT_PARTY_PMT_MTHD_ID, 
		       RANK() over (PARTITION BY payment_flow, ext_pmt_party_id, payment_function
			            ORDER BY EXT_PARTY_PMT_MTHD_ID) RANK
		FROM IBY_EXT_PARTY_PMT_MTHDS WHERE EXT_PMT_PARTY_ID IN (
			SELECT EXT_PMT_PARTY_ID
			FROM IBY_EXT_PARTY_PMT_MTHDS 
			WHERE PRIMARY_FLAG = ''Y''
			AND NVL(INACTIVE_DATE, SYSDATE+1) > SYSDATE
			GROUP BY  EXT_PMT_PARTY_ID , PRIMARY_FLAG
			HAVING COUNT(PRIMARY_FLAG) > 1)
		)
		WHERE RANK > 1)';
	  EXCEPTION
	    WHEN OTHERS THEN
	      IBY_DATA_FIX_UTILITY_PKG.Print('Error while updating primary flag of IBY_EXT_PARTY_PMT_MTHDS' ||SQLERRM );
	      RAISE;
	  END;
	  /*End of Checking for Duplicate records in IBY_EXT_PARTY_PMT_MTHDS*/

	  /* Correct payee records with invalid org_id and org_type combination */
	  BEGIN
	     EXECUTE IMMEDIATE 
		'UPDATE IBY_EXTERNAL_PAYEES_ALL 
		SET ORG_TYPE = NULL 
		WHERE ORG_TYPE IS NOT NULL 
		AND ORG_ID IS NULL';
	  EXCEPTION
	    WHEN OTHERS THEN
	      IBY_DATA_FIX_UTILITY_PKG.Print('Error while correction org_type data in iby_external_payees_all' ||SQLERRM );
	      RAISE;
	  END;

  /* Print data to log */
  IBY_DATA_FIX_UTILITY_PKG.Print('Following Duplicate Payee Records are deleted');

  IBY_DATA_FIX_UTILITY_PKG.Print_Html_Table(
        'EXT_PAYEE_ID,PAYEE_PARTY_ID,PAYMENT_FUNCTION,EXCLUSIVE_PAYMENT_FLAG,PARTY_SITE_ID,'||
        'SUPPLIER_SITE_ID,ORG_ID,ORG_TYPE,DEFAULT_PAYMENT_METHOD_CODE,ECE_TP_LOCATION_CODE,'||
	'BANK_CHARGE_BEARER,BANK_INSTRUCTION1_CODE,BANK_INSTRUCTION2_CODE,BANK_INSTRUCTION_DETAILS,'||
	'PAYMENT_REASON_CODE,PAYMENT_REASON_COMMENTS,INACTIVE_DATE,PAYMENT_TEXT_MESSAGE1,'||
	'PAYMENT_TEXT_MESSAGE2,PAYMENT_TEXT_MESSAGE3,DELIVERY_CHANNEL_CODE,PAYMENT_FORMAT_CODE,'||
	'SETTLEMENT_PRIORITY,REMIT_ADVICE_DELIVERY_METHOD,REMIT_ADVICE_EMAIL,REMIT_ADVICE_FAX',
        'IBY_EXTERNAL_PAYEES_10140168',
        NULL,
        'iby_dup_payees_sel_fix.sql');
 
  IBY_DATA_FIX_UTILITY_PKG.Print('**************************************<br><br>');
  
  IBY_DATA_FIX_UTILITY_PKG.Print('Payees are replaced as below');

  IBY_DATA_FIX_UTILITY_PKG.Print_Html_Table(
                                    'OLD_EXT_PAYEE_ID,NEW_EXT_PAYEE_ID',
				    'IBY_EXT_PAYEES_10140168_REF',
                                    NULL,
                                    'iby_dup_payees_sel_fix.sql');
  
  IBY_DATA_FIX_UTILITY_PKG.Print('**************************************<br><br>');
  
  IBY_DATA_FIX_UTILITY_PKG.Print('Following Duplicate Instruments are deleted');

  IBY_DATA_FIX_UTILITY_PKG.Print_Html_Table(
        'INSTRUMENT_PAYMENT_USE_ID,PAYMENT_FLOW,EXT_PMT_PARTY_ID,INSTRUMENT_TYPE,INSTRUMENT_ID,PAYMENT_FUNCTION,RANK',
        'IBY_INSTR_USES_10140168_TMP',
        NULL,
        'iby_dup_payees_sel_fix.sql');
 
  IBY_DATA_FIX_UTILITY_PKG.Print('**************************************<br><br>');
  
  IBY_DATA_FIX_UTILITY_PKG.Print('Following Duplicate payment methods are deleted');

  IBY_DATA_FIX_UTILITY_PKG.Print_Html_Table(
        'EXT_PARTY_PMT_MTHD_ID,PAYMENT_METHOD_CODE,PAYMENT_FLOW,EXT_PMT_PARTY_ID,PAYMENT_FUNCTION,PRIMARY_FLAG,RANK',
        'IBY_PTY_PMT_MTDS_10140168_TMP',
        NULL,
        'IBY_DUP_PAYEES_SEL_FIX.sql');
 
  IBY_DATA_FIX_UTILITY_PKG.Print('**************************************<br><br>');

  IBY_DATA_FIX_UTILITY_PKG.Print('The following backup tables are created<br>');
  IBY_DATA_FIX_UTILITY_PKG.Print('Iby_payments_all >> Iby_payments_all_10140168 <br>');
  IBY_DATA_FIX_UTILITY_PKG.Print('Iby_docs_payable_all >> Iby_docs_payable_10140168 <br>');
  IBY_DATA_FIX_UTILITY_PKG.Print('Iby_pmt_instr_uses_all >> Iby_pmt_instr_uses_10140168 <br>');
  IBY_DATA_FIX_UTILITY_PKG.Print('Iby_ext_party_pmt_mthds >> Iby_ext_pty_pmt_mtds_10140168 <br>');
  IBY_DATA_FIX_UTILITY_PKG.Print('Iby_external_payees_all >> Iby_external_payees_10140168 <br>');
 
  IBY_DATA_FIX_UTILITY_PKG.Print('Output file location:' || l_file_location);
  IBY_DATA_FIX_UTILITY_PKG.Print('</body></html>'||'iby_dup_payees_fix.sql');
  
  IBY_DATA_FIX_UTILITY_PKG.Close_Log_Out_Files;
 EXCEPTION
   WHEN OTHERS THEN
         IBY_DATA_FIX_UTILITY_PKG.Print('Exception while running the script  IBY_DUP_PAYEES_FIX.sql: '||SQLERRM);
         RAISE;
END;
/
COMMIT;
EXIT;
