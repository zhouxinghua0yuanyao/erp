REM dbdrv: sql ~PROD ~PATH ~FILE none none none package &phase=plb \
REM dbdrv: checkfile(120.0.12010000.2=120.0.12020000.2):~PROD:~PATH:~FILE
REM ===========================================================================
REM   Copyright (c) 2007, 2013 Oracle Corporation Redwood Shores, California, USA
REM                            All Rights Reserved
REM ===========================================================================
REM
REM   FILENAME:  GMEBCHTKB.pls
REM
REM   DESCRIPTION: GME_GMEBCHTK_XMLP_PKG PL/SQL package body
REM
REM   PUBLIC VARIABLES:
REM   PUBLIC PROCEDURES:
REM   PUBLIC FUNCTIONS:
REM
REM   MODIFICATION HISTORY:
REM
REM   DATE         AUTHOR    Version  Comments
REM ===========================================================================
REM   24/07/2007   DWKRISHN  Created.
REM ===========================================================================
SET VERIFY OFF
WHENEVER SQLERROR EXIT FAILURE ROLLBACK
WHENEVER OSERROR EXIT FAILURE ROLLBACK

  CREATE OR REPLACE PACKAGE BODY GME_GMEBCHTK_XMLP_PKG AS
/* $Header: GMEBCHTKB.pls 120.0.12020000.2 2013/10/23 18:29:52 adeshmuk ship $ */
  FUNCTION RANGECF1FORMULA RETURN VARCHAR2 IS
  l_batch_doc_numbering NUMBER;
  BEGIN
  
   -- Bug 17452692
  SELECT batch_doc_numbering
  INTO l_batch_doc_numbering
  FROM gme_parameters
  WHERE organization_id = P_ORG_ID;
  
    IF FROM_BATCH_NO IS NULL THEN
      IF TO_BATCH_NO IS NULL THEN
        RETURN ('1=1');
      END IF;
    ELSE
      IF TO_BATCH_NO IS NULL THEN
      -- Bug 17452692
      -- Added If condition use LPAD only for auto doc numbering              
        IF l_batch_doc_numbering = 2 THEN
        RETURN ('Lpad(pm_btch_hdr.batch_no,32,''0'')>=''' || LPAD(FROM_BATCH_NO
                   ,32
                   ,'0') || '''');
         ELSE
                return('pm_btch_hdr.batch_no >='''|| from_batch_no||'''');
         END IF;           
      ELSE
       -- Bug 17452692
       -- Added If condition use LPAD only for auto doc numbering    
       IF l_batch_doc_numbering = 2 THEN
        RETURN ('Lpad(pm_btch_hdr.batch_no,32,''0'')>=''' || LPAD(FROM_BATCH_NO
                   ,32
                   ,'0') || ''' and Lpad(pm_btch_hdr.batch_no,32,''0'')<=''' || LPAD(TO_BATCH_NO
                   ,32
                   ,'0') || '''');
       ELSE
            return('pm_btch_hdr.batch_no>='''|| from_batch_no ||''' and pm_btch_hdr.batch_no <='''|| to_batch_no ||'''');
       END IF;                   
      END IF;
    END IF;
    RETURN ('1=1');
  END RANGECF1FORMULA;
  FUNCTION RANGECF2FORMULA RETURN VARCHAR2 IS
  BEGIN
    RETURN NULL;
  END RANGECF2FORMULA;
  FUNCTION ORDCFFORMULA RETURN VARCHAR2 IS
  BEGIN
    IF SORTRETCFFORMULA = 'Batch No' THEN
      RETURN ('pm_btch_hdr.batch_no,pm_btch_hdr.text_code,
             			gem_lookups.meaning desc,pm_matl_dtl.line_no');
    ELSE
      RETURN ('pm_btch_hdr.plan_start_date,pm_btch_hdr.batch_no,pm_btch_hdr.text_code,
             			gem_lookups.meaning desc,pm_matl_dtl.line_no');
    END IF;
    RETURN NULL;
  END ORDCFFORMULA;
  FUNCTION FLAGCFFORMULA(TEXTCODE2 IN NUMBER) RETURN NUMBER IS
  BEGIN
    IF TEXTCODE2 IS NULL THEN
      RETURN (0);
    ELSE
      RETURN (1);
    END IF;
    RETURN NULL;
  END FLAGCFFORMULA;
  FUNCTION AFTERPFORM RETURN BOOLEAN IS
  BEGIN
    IF FROM_BATCH_NO > TO_BATCH_NO THEN
      /*SRW.MESSAGE(100
                 ,GGM_MESSAGE.GET('IC_FROM_LTE_THRU'))*/NULL;
      /*RAISE SRW.PROGRAM_ABORT*/RAISE_APPLICATION_ERROR(-20101,null);
    END IF;
    IF FROM_START_DATE > TO_START_DATE THEN
      /*SRW.MESSAGE(100
                 ,GGM_MESSAGE.GET('IC_FROM_LTE_THRU'))*/NULL;
      /*RAISE SRW.PROGRAM_ABORT*/RAISE_APPLICATION_ERROR(-20101,null);
    END IF;
    IF FROM_BATCH_NO IS NULL THEN
      IF TO_BATCH_NO IS NOT NULL THEN
        /*SRW.MESSAGE(100
                   ,GGM_MESSAGE.GET('IC_FRM_REQD_FOR_THRU'))*/NULL;
        /*RAISE SRW.PROGRAM_ABORT*/RAISE_APPLICATION_ERROR(-20101,null);
      END IF;
    END IF;
    RETURN (TRUE);
  END AFTERPFORM;
  FUNCTION BEFOREREPORT RETURN BOOLEAN IS
  BEGIN
    P_ROWS := 0;
    CNTTEXT := 0;
    P_CONC_REQUEST_ID := FND_GLOBAL.CONC_REQUEST_ID;
    FROM_START_DATE_1 := to_char(FROM_START_DATE, 'DD-MON-YYYY');
    TO_START_DATE_1 := to_char(TO_START_DATE, 'DD-MON-YYYY');
    FROM_START_DATE1 := to_char(FROM_START_DATE, 'DD-MON-YYYY HH24:MI:SS');
    TO_START_DATE1 := to_char(TO_START_DATE, 'DD-MON-YYYY HH24:MI:SS');
    /*SRW.USER_EXIT('FND SRWINIT')*/NULL;
    RETURN (TRUE);
  END BEFOREREPORT;
  FUNCTION TYPE_DESCCFFORMULA(MEANING IN VARCHAR2) RETURN VARCHAR2 IS
  BEGIN
    RETURN (MEANING);
  END TYPE_DESCCFFORMULA;
  FUNCTION SORTRETCFFORMULA RETURN VARCHAR2 IS
    X_SORT1 VARCHAR2(80);
    CURSOR CUR_SELECT IS
      SELECT
        MEANING
      FROM
        GEM_LOOKUP_VALUES
      WHERE LOOKUP_CODE = SORT_BY
        AND LOOKUP_TYPE = 'PM_RIPMBUSR_SORT';
  BEGIN
    OPEN CUR_SELECT;
    FETCH CUR_SELECT
     INTO X_SORT1;
    CLOSE CUR_SELECT;
    RETURN (X_SORT1);
  END SORTRETCFFORMULA;
  PROCEDURE HEADER IS
  BEGIN
    NULL;
  END HEADER;
  FUNCTION AFTERREPORT RETURN BOOLEAN IS
  BEGIN
    /*SRW.USER_EXIT('FND SRWEXIT')*/NULL;
    RETURN (TRUE);
  END AFTERREPORT;
  FUNCTION CF_CONTEXT_ORGFORMULA RETURN VARCHAR2 IS
    CURSOR C_GET_ORG IS
      SELECT
        ORGANIZATION_CODE
      FROM
        MTL_PARAMETERS
      WHERE ORGANIZATION_ID = P_ORG_ID;
    L_ORG VARCHAR2(6);
  BEGIN
    OPEN C_GET_ORG;
    FETCH C_GET_ORG
     INTO L_ORG;
    CLOSE C_GET_ORG;
    L_ORG := '(' || L_ORG || ')';
    RETURN L_ORG;
  END CF_CONTEXT_ORGFORMULA;
  FUNCTION CF_SUBINV_LABELFORMULA(LINE_TYPE IN NUMBER) RETURN CHAR IS
    L_TYPE VARCHAR2(2);
    L_LABEL VARCHAR2(40);
    CURSOR C_GET_MEANING IS
      SELECT
        MEANING
      FROM
        FND_LOOKUP_VALUES_VL
      WHERE VIEW_APPLICATION_ID = 553
        AND LOOKUP_TYPE = 'GME_SUBINVENTORY_TYPE'
        AND LOOKUP_CODE = L_TYPE;
  BEGIN
    IF LINE_TYPE = -1 THEN
      L_TYPE := 'S';
    ELSE
      L_TYPE := 'Y';
    END IF;
    OPEN C_GET_MEANING;
    FETCH C_GET_MEANING
     INTO L_LABEL;
    CLOSE C_GET_MEANING;
    RETURN L_LABEL;
  END CF_SUBINV_LABELFORMULA;
function R_batchFormatTrigger(batch_no1 VARCHAR2) return varchar2 is
 pragma autonomous_transaction;
begin
  update gme_batch_header set print_count = print_count + 1 where organization_id = p_org_id and
						batch_no = batch_no1;
  commit;
  return 'TRUE';
end;
END GME_GMEBCHTK_XMLP_PKG;
 
/
COMMIT;
EXIT;
