REM dbdrv: sql ~PROD ~PATH ~FILE none none none package &phase=plb \
REM dbdrv: checkfile(120.1.12010000.3=120.1.12020000.3)(120.1.12010000.2=120.1.12020000.2):~PROD:~PATH:~FILE
REM ===========================================================================
REM   Copyright (c) 2007, 2016 Oracle Corporation Redwood Shores, California, USA
REM                            All Rights Reserved
REM ===========================================================================
REM
REM   FILENAME:  GMEMUSBVB.pls
REM
REM   DESCRIPTION: GME_GMEMUSBV_XMLP_PKG PL/SQL package body
REM
REM   PUBLIC VARIABLES:
REM   PUBLIC PROCEDURES:
REM   PUBLIC FUNCTIONS:
REM
REM   MODIFICATION HISTORY:
REM
REM   DATE         AUTHOR    Version  Comments
REM ===========================================================================
REM   24-JUL-2007  DWKRISHN  Created.
REM
REM   18-NOV-2016  GMURATOR  Bug 24977868
REM      Increase some variables to match data being fetched.
REM      FUNCTION TRANS_QTYFORMULA_005
REM ===========================================================================
SET VERIFY OFF
WHENEVER SQLERROR EXIT FAILURE ROLLBACK
WHENEVER OSERROR EXIT FAILURE ROLLBACK

  CREATE OR REPLACE PACKAGE BODY GME_GMEMUSBV_XMLP_PKG AS
/* $Header: GMEMUSBVB.pls 120.1.12020000.3 2016/11/18 18:08:14 gmurator ship $ */
  FUNCTION CF_ACCTG_COST(DITEMID IN NUMBER
                        ,ACTUAL_CMPLT_DATE IN DATE) RETURN NUMBER IS
  BEGIN
    DECLARE
      V_COST_BASIS NUMBER(1);
      V_COST NUMBER;
      V_GL_COST_MTHD VARCHAR2(4);
      V_CO_CODE VARCHAR2(4);
      V_RET_VAL NUMBER;
      V_COST_MTHD VARCHAR2(8) := NULL;
      V_CMPNTCLS_ID NUMBER := NULL;
      V_ANALYSIS_CODE VARCHAR2(8) := NULL;
      V_RETREIVE_IND NUMBER := NULL;
      V_COST_CMPNTCLS_ID NUMBER;
      V_COST_ANALYSIS_CODE VARCHAR2(1) := NULL;
      V_ACCTG_COST NUMBER;
      V_STND VARCHAR2(4) := 'STND';
      COST NUMBER;
      V_NUM_ROWS NUMBER;
      L_RETURN_STATUS VARCHAR2(4);
      L_MSG_COUNT NUMBER;
      L_MSG_DATA VARCHAR2(2000);
    BEGIN
      V_RET_VAL := GMF_CMCOMMON.GET_PROCESS_ITEM_COST(P_API_VERSION => 1.0
                                                     ,P_INIT_MSG_LIST => 'T'
                                                     ,P_ORGANIZATION_ID => P_ORG_ID
                                                     ,P_INVENTORY_ITEM_ID => DITEMID
                                                     ,P_TRANSACTION_DATE => TRUNC(ACTUAL_CMPLT_DATE)
                                                     ,P_DETAIL_FLAG => 1
                                                     ,P_COST_METHOD => V_COST_MTHD
                                                     ,P_COST_COMPONENT_CLASS_ID => V_COST_CMPNTCLS_ID
                                                     ,P_COST_ANALYSIS_CODE => V_COST_ANALYSIS_CODE
                                                     ,X_TOTAL_COST => V_ACCTG_COST
                                                     ,X_NO_OF_ROWS => V_NUM_ROWS
                                                     ,X_RETURN_STATUS => L_RETURN_STATUS
                                                     ,X_MSG_COUNT => L_MSG_COUNT
                                                     ,X_MSG_DATA => L_MSG_DATA);
      IF V_RET_VAL = 1 THEN
        V_COST := V_ACCTG_COST;
      ELSE
        V_COST := 0;
      END IF;
      CP_ITEM_COST := V_COST;
      RETURN (V_COST);
    EXCEPTION
      WHEN OTHERS THEN
        RETURN NULL;
    END;
    RETURN NULL;
  END CF_ACCTG_COST;

  FUNCTION CF_ACCTG_COST2FORMULA(TRANS_QTY IN NUMBER) RETURN NUMBER IS
    COST NUMBER;
  BEGIN
    COST := TRANS_QTY * CP_ITEM_COST;
    RETURN (COST);
  END CF_ACCTG_COST2FORMULA;

  FUNCTION CF_QUANTITY_VARIANCE(CF_ACTL_QTY_ITEMUM IN NUMBER
                               ,TRANS_QTY IN NUMBER) RETURN NUMBER IS
  BEGIN
    RETURN (nvl(CF_ACTL_QTY_ITEMUM, 0) - nvl(TRANS_QTY, 0));
  END CF_QUANTITY_VARIANCE;

  FUNCTION CF_VALUE_VARIANCE(BATCHUM IN VARCHAR2
                            ,ITEM_UM IN VARCHAR2
                            ,DITEMID IN NUMBER
                            ,CF_QUANTITY_VARIANCE IN NUMBER
                            ,CF_ACCTG_COST IN NUMBER) RETURN NUMBER IS
    V_QTY_VAR NUMBER;
    MESSAGE_IND NUMBER;
    MESSAGE_TEXT VARCHAR2(2000);
  BEGIN
    IF (BATCHUM <> ITEM_UM) THEN
      V_QTY_VAR := INV_CONVERT.INV_UM_CONVERT(ITEM_ID => DITEMID
                                             ,PRECISION => 5
                                             ,FROM_QUANTITY => NVL(CF_QUANTITY_VARIANCE
                                                ,0)
                                             ,FROM_UNIT => BATCHUM
                                             ,TO_UNIT => ITEM_UM
                                             ,FROM_NAME => NULL
                                             ,TO_NAME => NULL);
    ELSE
      V_QTY_VAR := NVL(CF_QUANTITY_VARIANCE
                      ,0);
    END IF;
    RETURN (CF_ACCTG_COST * V_QTY_VAR);
  EXCEPTION
    WHEN OTHERS THEN
      GME_COMMON_PVT.COUNT_AND_GET(P_ENCODED => 'F'
                                  ,X_DATA => MESSAGE_TEXT
                                  ,X_COUNT => MESSAGE_IND);
      /*SRW.MESSAGE(100
                 ,'Error in UOM Conversion ' || MESSAGE_TEXT)*/NULL;
      /*RAISE SRW.PROGRAM_ABORT*/RAISE_APPLICATION_ERROR(-20101,null);
  END CF_VALUE_VARIANCE;

  FUNCTION CF_VARIANCE_PCT(TRANS_QTY IN NUMBER
                          ,CF_QUANTITY_VARIANCE IN NUMBER) RETURN NUMBER IS
    TEMPFIELD NUMBER;
  BEGIN
    IF ((TRANS_QTY = 0) OR TRANS_QTY IS NULL) THEN
--Bug 21467413, to avoid RTF template converting null to number issue.
--      IF CF_QUANTITY_VARIANCE = 0 THEN
        TEMPFIELD := 0;
--      ELSE
--        TEMPFIELD := NULL;
--      END IF;
    ELSE
      TEMPFIELD := (CF_QUANTITY_VARIANCE / TRANS_QTY) * 100;
    END IF;
    RETURN (TEMPFIELD);
  END CF_VARIANCE_PCT;

  FUNCTION CF_BATCHRANGEFORMULA RETURN VARCHAR2 IS
  BEGIN
    /*SRW.REFERENCE(CP_BATCHRANGE)*/NULL;
    IF FROMBATCH IS NOT NULL AND TOBATCH IS NOT NULL AND LPAD(FROMBATCH
        ,32
        ,'0') = LPAD(TOBATCH
        ,32
        ,'0') THEN
      CP_BATCHRANGE := ' and Lpad(h.batch_no,32,''0'') = ' || '''' || LPAD(FROMBATCH
                           ,32
                           ,'0') || '''';
    ELSIF FROMBATCH IS NOT NULL AND TOBATCH IS NOT NULL THEN
      CP_BATCHRANGE := ' and Lpad(h.batch_no,32,''0'') between ' || '''' || LPAD(FROMBATCH
                           ,32
                           ,'0') || '''' || ' and ' || '''' || LPAD(TOBATCH
                           ,32
                           ,'0') || '''';
    ELSIF FROMBATCH IS NULL AND TOBATCH IS NULL THEN
      CP_BATCHRANGE := ' ';
    ELSIF FROMBATCH IS NOT NULL AND TOBATCH IS NULL THEN
      CP_BATCHRANGE := 'and Lpad(h.batch_no,32,''0'') >= ' || '''' || LPAD(FROMBATCH
                           ,32
                           ,'0') || '''';
    ELSIF FROMBATCH IS NULL AND TOBATCH IS NOT NULL THEN
      CP_BATCHRANGE := 'and Lpad(h.batch_no,32,''0'') <= ' || '''' || LPAD(TOBATCH
                           ,32
                           ,'0') || '''';
    END IF;
    RETURN NULL;
  END CF_BATCHRANGEFORMULA;

  FUNCTION CF_DATERANGEFORMULA RETURN VARCHAR2 IS
  BEGIN
    /*SRW.REFERENCE(CP_DATERANGE)*/NULL;
    RETURN NULL;
  END CF_DATERANGEFORMULA;

  FUNCTION CF_ITEM_RANGE RETURN VARCHAR2 IS
  BEGIN
    /*SRW.REFERENCE(CP_ITEMRANGE)*/NULL;
    IF FROMITEM IS NOT NULL AND TOITEM IS NOT NULL AND FROMITEM = TOITEM THEN
      CP_ITEMRANGE := ' and im.concatenated_segments = ' || '''' || FROMITEM || '''';
    ELSIF FROMITEM IS NOT NULL AND TOITEM IS NOT NULL THEN
      CP_ITEMRANGE := ' and im.concatenated_segments between ' || '''' || FROMITEM || ''' and ' || '''' || TOITEM || '''';
    ELSIF FROMITEM IS NULL AND TOITEM IS NULL THEN
      CP_ITEMRANGE := ' ';
    ELSIF FROMITEM IS NOT NULL AND TOITEM IS NULL THEN
      CP_ITEMRANGE := 'and im.concatenated_segments >= ' || '''' || FROMITEM || '''';
    ELSIF FROMITEM IS NULL AND TOITEM IS NOT NULL THEN
      CP_ITEMRANGE := 'and im.concatenated_segments <= ' || '''' || TOITEM || '''';
    END IF;
    RETURN NULL;
  END CF_ITEM_RANGE;

  FUNCTION CF_WIPCODE RETURN VARCHAR2 IS
    WHSECODE VARCHAR2(50);
  BEGIN
    WHSECODE := NULL;
    RETURN (WHSECODE);
    RETURN NULL;
  EXCEPTION
    WHEN OTHERS THEN
      RETURN (NULL);
  END CF_WIPCODE;

  FUNCTION G_MAINGROUPFILTER RETURN BOOLEAN IS
  BEGIN
    RETURN (TRUE);
  END G_MAINGROUPFILTER;

  FUNCTION CF_ACTL_QTY_ITEMUMFORMULA(BATCHUM IN VARCHAR2
                                    ,FORMUM IN VARCHAR2
                                    ,AQTY IN NUMBER
                                    ,DITEMID IN NUMBER) RETURN NUMBER IS
    V_PLAN_QTY_ITEMUM NUMBER;
    X_RET NUMBER(5);
  BEGIN
    IF (BATCHUM <> FORMUM OR NVL(AQTY
       ,0) <> 0) THEN
      V_PLAN_QTY_ITEMUM := INV_CONVERT.INV_UM_CONVERT(ITEM_ID => DITEMID
                                                     ,PRECISION => 5
                                                     ,FROM_QUANTITY => NVL(AQTY
                                                        ,0)
                                                     ,FROM_UNIT => BATCHUM
                                                     ,TO_UNIT => FORMUM
                                                     ,FROM_NAME => NULL
                                                     ,TO_NAME => NULL);
      RETURN V_PLAN_QTY_ITEMUM;
    ELSE
      RETURN NVL(AQTY
                ,0);
    END IF;
    RETURN NULL;
  END CF_ACTL_QTY_ITEMUMFORMULA;

  FUNCTION CF_BATCH_SIZEFORMULA(RECIPE_VALIDITY_RULE_ID IN NUMBER
                               ,BATCH_ID IN NUMBER) RETURN NUMBER IS
    PLANQTY NUMBER(10,2);
  BEGIN
    IF RECIPE_VALIDITY_RULE_ID IS NOT NULL THEN
      SELECT
        PLAN_QTY
      INTO PLANQTY
      FROM
        GME_MATERIAL_DETAILS
      WHERE BATCH_ID = CF_BATCH_SIZEFORMULA.BATCH_ID
        AND INVENTORY_ITEM_ID = (
        SELECT
          INVENTORY_ITEM_ID
        FROM
          GMD_RECIPE_VALIDITY_RULES
        WHERE RECIPE_VALIDITY_RULE_ID = CF_BATCH_SIZEFORMULA.RECIPE_VALIDITY_RULE_ID );
    ELSE
      SELECT
        PLAN_QTY
      INTO PLANQTY
      FROM
        GME_MATERIAL_DETAILS
      WHERE BATCH_ID = CF_BATCH_SIZEFORMULA.BATCH_ID
        AND LINE_TYPE = 1
        AND LINE_NO = 1;
    END IF;
    RETURN (PLANQTY);
    RETURN NULL;
  EXCEPTION
    WHEN OTHERS THEN
      RETURN (0);
  END CF_BATCH_SIZEFORMULA;

  FUNCTION CF_ITEMUMFORMULA(RECIPE_VALIDITY_RULE_ID IN NUMBER
                           ,BATCH_ID IN NUMBER) RETURN VARCHAR2 IS
    ITEMUM VARCHAR2(4);
  BEGIN
    IF RECIPE_VALIDITY_RULE_ID IS NOT NULL THEN
      SELECT
        DTL_UM
      INTO ITEMUM
      FROM
        GME_MATERIAL_DETAILS
      WHERE BATCH_ID = cf_itemumformula.BATCH_ID
        AND INVENTORY_ITEM_ID = (
        SELECT
          INVENTORY_ITEM_ID
        FROM
          GMD_RECIPE_VALIDITY_RULES
        WHERE RECIPE_VALIDITY_RULE_ID = cf_itemumformula.RECIPE_VALIDITY_RULE_ID );
    ELSE
      SELECT
        DTL_UM
      INTO ITEMUM
      FROM
        GME_MATERIAL_DETAILS
      WHERE BATCH_ID = cf_itemumformula.BATCH_ID
        AND LINE_TYPE = 1
        AND LINE_NO = 1;
    END IF;
    RETURN (ITEMUM);
    RETURN NULL;
  EXCEPTION
    WHEN OTHERS THEN
      RETURN (NULL);
  END CF_ITEMUMFORMULA;

  FUNCTION TRANS_QTYFORMULA_005(FORMULALINE_ID IN NUMBER
                               ,RECIPE_VALIDITY_RULE_ID IN NUMBER
                               ,FORMULA_ID IN NUMBER
                               ,BATCH_ID IN NUMBER) RETURN NUMBER IS
    -- X_ITEM_ID NUMBER(10);     
    X_ITEM_ID NUMBER;  -- Bug 24977868
    P_SCALE_TAB GMD_COMMON_SCALE.SCALE_TAB;
    X_SCALE_TAB GMD_COMMON_SCALE.SCALE_TAB;
    X_RETURN_STATUS VARCHAR2(4);
    errcode1 number;
    errmsg1 varchar2(100);


    CURSOR CUR_FORMULA_EXISTS IS
      SELECT COUNT(1)
        FROM FND_DUAL
       WHERE EXISTS (SELECT FORMULALINE_ID
                       FROM GME_SCALE_DETAIL
                      WHERE FORMULALINE_ID = trans_qtyformula_005.FORMULALINE_ID );
        
    CURSOR CUR_MATL_QTY IS
      SELECT QTY
        FROM GME_SCALE_DETAIL
       WHERE FORMULALINE_ID = trans_qtyformula_005.FORMULALINE_ID;
      
    CURSOR CUR_PRIMARY_PROD IS
      SELECT INVENTORY_ITEM_ID
        FROM GMD_RECIPE_VALIDITY_RULES
       WHERE RECIPE_VALIDITY_RULE_ID = trans_qtyformula_005.RECIPE_VALIDITY_RULE_ID;
      
    CURSOR CUR_ITEM_UM(V_ITEM_ID IN NUMBER) IS
      SELECT PRIMARY_UOM_CODE
        FROM MTL_SYSTEM_ITEMS_B
       WHERE ORGANIZATION_ID = P_ORG_ID
         AND INVENTORY_ITEM_ID = V_ITEM_ID;
         
    CURSOR CUR_FORM_DTL IS
      SELECT QTY, DETAIL_UOM
        FROM FM_MATL_DTL
       WHERE FORMULA_ID = trans_qtyformula_005.FORMULA_ID
         AND ORGANIZATION_ID = P_ORG_ID
         AND INVENTORY_ITEM_ID = X_ITEM_ID
         AND LINE_TYPE = 1;
         
    CURSOR CUR_BATCH_DTL IS
      SELECT PLAN_QTY, DTL_UM
        FROM GME_MATERIAL_DETAILS
       WHERE BATCH_ID = trans_qtyformula_005.BATCH_ID
         AND INVENTORY_ITEM_ID = X_ITEM_ID
         AND LINE_TYPE = 1;
         
    CURSOR CUR_PLAN_QTY IS
      SELECT QTY
        FROM GME_SCALE_DETAIL
       WHERE FORMULALINE_ID = trans_qtyformula_005.FORMULALINE_ID;
       
    CURSOR CUR_SCRAP_FACTOR IS
      SELECT SCRAP_FACTOR
        FROM FM_MATL_DTL
       WHERE FORMULALINE_ID = trans_qtyformula_005.FORMULALINE_ID
         AND FORMULA_ID = trans_qtyformula_005.FORMULA_ID
         AND LINE_TYPE = - 1;
         
    CURSOR CUR_GET_INFO(PBATCH_ID IN NUMBER) IS
      SELECT A.RECIPE_ID, B.ROUTING_ID, B.FORMULA_ID, B.PLAN_START_DATE
        FROM GME_BATCH_HEADER B, GMD_RECIPE_VALIDITY_RULES A
       WHERE B.BATCH_ID = PBATCH_ID
         AND B.RECIPE_VALIDITY_RULE_ID = A.RECIPE_VALIDITY_RULE_ID;
         
    CURSOR CUR_GET_RT_UOM(P_ROUTING_ID IN NUMBER) IS
      SELECT ITEM_UM
        FROM GMD_ROUTINGS_B
       WHERE ROUTING_ID = P_ROUTING_ID;
       
    CURSOR CUR_GET_RTCLASS(PROUTING_ID IN NUMBER) IS
      SELECT ROUTING_CLASS, ITEM_UM
        FROM GMD_ROUTINGS_B
       WHERE ROUTING_ID = PROUTING_ID;
       
    CURSOR CUR_MTL_DETAILS IS
      SELECT *
        FROM GME_SCALE_DETAIL
       WHERE LINE_TYPE in ( 1 , 2 );
       
    CURSOR CUR_MTL_DTLS IS
      SELECT *
        FROM FM_MATL_DTL
       WHERE FORMULA_ID = trans_qtyformula_005.FORMULA_ID
         AND LINE_TYPE in ( 1 , 2 );
         
    L_RECIPE_ID NUMBER;
    L_ORGN_CODE VARCHAR2(4);
    L_FORMULA_ID NUMBER;
    L_ROUTING_ID NUMBER;
    L_ROUTING_CLASS FM_ROUT_HDR.ROUTING_CLASS%TYPE;
    
    L_ROUTING_UOM VARCHAR2(4);   -- Bug 24977868
    
    L_TOTAL_OUTPUT_QTY_C NUMBER := 0;
    L_TOTAL_OUTPUT_QTY_B NUMBER := 0;
    L_TEMP_QTY NUMBER;
    L_COUNT NUMBER;
    L_PLAN_START_DATE DATE;
    L_FORMULA_TBL GMDFMVAL_PUB.FORMULA_DETAIL_TBL;
    X_TRANS_QTY NUMBER := 0;
    X_COUNT NUMBER(5);
    X_ITEM_UM VARCHAR2(4);
    X_QTY NUMBER;
    X_DTL_UM VARCHAR2(4);
    X_CONV_QTY_FORM NUMBER;
    X_CONV_QTY_BATCH NUMBER;
    X_SCALE_FACTOR NUMBER;
    X_SCRAP_FACTOR NUMBER;
    X_RVAR NUMBER(5);
    I NUMBER;
    L_PROCESS_LOSS NUMBER;
    L_NUMBER_OF_FORMULA_LINES NUMBER;
    MESSAGE_TEXT VARCHAR2(200);
    MESSAGE_IND NUMBER;
    PRAGMA AUTONOMOUS_TRANSACTION;
  BEGIN
    IF FORMULALINE_ID > 0 THEN
        OPEN CUR_FORMULA_EXISTS;
       FETCH CUR_FORMULA_EXISTS
        INTO X_COUNT;
       CLOSE CUR_FORMULA_EXISTS;
       
        OPEN CUR_GET_INFO(BATCH_ID);
       FETCH CUR_GET_INFO
        INTO L_RECIPE_ID,L_ROUTING_ID,L_FORMULA_ID,L_PLAN_START_DATE;
       CLOSE CUR_GET_INFO;
       
        OPEN CUR_GET_RT_UOM(L_ROUTING_ID);
       FETCH CUR_GET_RT_UOM
        INTO L_ROUTING_UOM;
       CLOSE CUR_GET_RT_UOM;
       
        OPEN CUR_PRIMARY_PROD;
       FETCH CUR_PRIMARY_PROD
        INTO X_ITEM_ID;
       CLOSE CUR_PRIMARY_PROD;
             
        OPEN CUR_ITEM_UM(X_ITEM_ID);
       FETCH CUR_ITEM_UM
        INTO X_ITEM_UM;
       CLOSE CUR_ITEM_UM;
       
        OPEN CUR_FORM_DTL;
       FETCH CUR_FORM_DTL
        INTO X_QTY,X_DTL_UM;
       CLOSE CUR_FORM_DTL;
      
       IF X_DTL_UM <> X_ITEM_UM THEN
          X_CONV_QTY_FORM := INV_CONVERT.INV_UM_CONVERT(ITEM_ID       => X_ITEM_ID
                                                       ,PRECISION     => 5
                                                       ,FROM_QUANTITY => X_QTY
                                                       ,FROM_UNIT     => X_DTL_UM
                                                       ,TO_UNIT       => X_ITEM_UM
                                                       ,FROM_NAME     => NULL
                                                       ,TO_NAME       => NULL);
       ELSE
          X_CONV_QTY_FORM := X_QTY;
       END IF;
       
        OPEN CUR_BATCH_DTL;
       FETCH CUR_BATCH_DTL
        INTO X_QTY,X_DTL_UM;
       CLOSE CUR_BATCH_DTL;
       IF X_DTL_UM <> X_ITEM_UM THEN
          X_CONV_QTY_BATCH := INV_CONVERT.INV_UM_CONVERT(ITEM_ID       => X_ITEM_ID
                                                        ,PRECISION     => 5
                                                        ,FROM_QUANTITY => X_QTY
                                                        ,FROM_UNIT     => X_DTL_UM
                                                        ,TO_UNIT       => X_ITEM_UM
                                                        ,FROM_NAME     => NULL
                                                        ,TO_NAME       => NULL);
       ELSE
          X_CONV_QTY_BATCH := X_QTY;
       END IF;
       
       IF X_CONV_QTY_FORM > 0 THEN
          X_SCALE_FACTOR := X_CONV_QTY_BATCH / X_CONV_QTY_FORM;
       ELSE
          X_SCALE_FACTOR := 0;
       END IF;
       
       IF L_ROUTING_ID IS NOT NULL THEN
          L_TOTAL_OUTPUT_QTY_C := 0;
          FOR l_material_details IN CUR_MTL_DTLS LOOP
              L_TEMP_QTY := INV_CONVERT.INV_UM_CONVERT(ITEM_ID       => L_MATERIAL_DETAILS.ITEM_ID
                                                      ,PRECISION     => 5
                                                      ,FROM_QUANTITY => L_MATERIAL_DETAILS.QTY
                                                      ,FROM_UNIT     => L_MATERIAL_DETAILS.ITEM_UM
                                                      ,TO_UNIT       => L_ROUTING_UOM
                                                      ,FROM_NAME     => NULL
                                                      ,TO_NAME       => NULL);
             IF L_TEMP_QTY < 0 THEN
                L_TEMP_QTY := 0;
             ELSE
                L_TOTAL_OUTPUT_QTY_C := L_TOTAL_OUTPUT_QTY_C + L_TEMP_QTY;
             END IF;
          END LOOP;
       END IF;
       
       GMDFMVAL_PUB.GET_SUBSTITUTE_ITEMS(PFORMULA_ID        => L_FORMULA_ID
                                        ,PDATE              => L_PLAN_START_DATE
                                        ,XFORMULADETAIL_TBL => L_FORMULA_TBL);
                                        
       FOR i IN 1 .. L_FORMULA_TBL.COUNT LOOP
          P_SCALE_TAB(I).LINE_NO := L_FORMULA_TBL(I).FORMULALINE_ID;
          P_SCALE_TAB(I).LINE_TYPE := L_FORMULA_TBL(I).LINE_TYPE;
          P_SCALE_TAB(I).INVENTORY_ITEM_ID := L_FORMULA_TBL(I).INVENTORY_ITEM_ID;
          P_SCALE_TAB(I).QTY := NVL(L_FORMULA_TBL(I).QTY
                                   ,0);
          P_SCALE_TAB(I).DETAIL_UOM := L_FORMULA_TBL(I).DETAIL_UOM;
          P_SCALE_TAB(I).SCALE_TYPE := L_FORMULA_TBL(I).SCALE_TYPE;
          P_SCALE_TAB(I).SCALE_MULTIPLE := L_FORMULA_TBL(I).SCALE_MULTIPLE;
          P_SCALE_TAB(I).SCALE_ROUNDING_VARIANCE := L_FORMULA_TBL(I).SCALE_ROUNDING_VARIANCE;
          P_SCALE_TAB(I).ROUNDING_DIRECTION := L_FORMULA_TBL(I).ROUNDING_DIRECTION;
          P_SCALE_TAB(I).CONTRIBUTE_YIELD_IND := L_FORMULA_TBL(I).CONTRIBUTE_YIELD_IND;
       END LOOP;
       
       GMD_COMMON_SCALE.SCALE(P_SCALE_TAB     => P_SCALE_TAB
                             ,P_ORGN_ID       => P_ORG_ID
                             ,P_SCALE_FACTOR  => X_SCALE_FACTOR
                             ,P_PRIMARIES     => 'OUTPUTS'
                             ,X_SCALE_TAB     => X_SCALE_TAB
                             ,X_RETURN_STATUS => X_RETURN_STATUS);
                             
       IF L_ROUTING_ID IS NOT NULL THEN
          L_TOTAL_OUTPUT_QTY_B := 0;
          L_COUNT := X_SCALE_TAB.COUNT;
          FOR i IN 1 .. L_COUNT LOOP
             IF X_SCALE_TAB(I).LINE_TYPE in (1,2) THEN
                L_TEMP_QTY := INV_CONVERT.INV_UM_CONVERT(ITEM_ID => X_SCALE_TAB(I).INVENTORY_ITEM_ID
                                                        ,PRECISION     => 5
                                                        ,FROM_QUANTITY => X_SCALE_TAB(I).QTY
                                                        ,FROM_UNIT     => X_SCALE_TAB(I).DETAIL_UOM
                                                        ,TO_UNIT       => L_ROUTING_UOM
                                                        ,FROM_NAME     => NULL
                                                        ,TO_NAME       => NULL);
                IF L_TEMP_QTY < 0 THEN
                   L_TEMP_QTY := 0;
                ELSE
                   L_TOTAL_OUTPUT_QTY_B := L_TOTAL_OUTPUT_QTY_B + L_TEMP_QTY;
                END IF;
             END IF;
          END LOOP;
          
          L_PROCESS_LOSS := GME_COMMON_PVT.GET_PROCESS_LOSS(P_BATCH_ID                   => trans_qtyformula_005.BATCH_ID
                                                           ,P_VALIDITY_RULE_ID           => trans_qtyformula_005.RECIPE_VALIDITY_RULE_ID
                                                           ,P_ORGANIZATION_ID            => P_ORG_ID
                                                           ,P_TOTAL_OUTPUT_QTY_SCALED    => L_TOTAL_OUTPUT_QTY_B
                                                           ,P_TOTAL_OUTPUT_QTY_PRE_SCALE => L_TOTAL_OUTPUT_QTY_C);
          IF (L_PROCESS_LOSS IS NULL) THEN
             GME_COMMON_PVT.COUNT_AND_GET(P_ENCODED => 'F'
                                         ,X_DATA    => MESSAGE_TEXT
                                         ,X_COUNT   => MESSAGE_IND);
             /*SRW.MESSAGE(100
                        ,'Error in the get_process_loss ' || MESSAGE_TEXT)*/NULL;
             /*RAISE SRW.PROGRAM_ABORT*/RAISE_APPLICATION_ERROR(-20101,null);
          END IF;
          
          L_NUMBER_OF_FORMULA_LINES := X_SCALE_TAB.COUNT;
          FOR l_row_count IN 1 .. L_NUMBER_OF_FORMULA_LINES LOOP
             IF X_SCALE_TAB(L_ROW_COUNT).LINE_TYPE < 0 THEN
                IF X_SCALE_TAB(L_ROW_COUNT).SCALE_TYPE = 1 THEN
                   X_SCALE_TAB(L_ROW_COUNT).QTY := X_SCALE_TAB(L_ROW_COUNT).QTY * 100 / (100 - L_PROCESS_LOSS);
                END IF;
             END IF;
          END LOOP;
       END IF;
      
       IF X_RETURN_STATUS = 'S' THEN
          FOR i IN 1 .. X_SCALE_TAB.COUNT LOOP          
             INSERT INTO GME_SCALE_DETAIL ( FORMULALINE_ID,LINE_TYPE,ITEM_ID,QTY,ITEM_UM,SCALE_TYPE,CONV_QTY)
             VALUES   (X_SCALE_TAB(I).LINE_NO
                      ,X_SCALE_TAB(I).LINE_TYPE
                      ,X_SCALE_TAB(I).INVENTORY_ITEM_ID
                      ,X_SCALE_TAB(I).QTY
                      ,X_SCALE_TAB(I).DETAIL_UOM
                      ,X_SCALE_TAB(I).SCALE_TYPE
                      ,X_SCALE_TAB(I).QTY);
          END LOOP;
          commit;
       END IF;
       
        OPEN CUR_PLAN_QTY;
       FETCH CUR_PLAN_QTY
        INTO X_TRANS_QTY;
       CLOSE CUR_PLAN_QTY;
       
        OPEN CUR_SCRAP_FACTOR;
       FETCH CUR_SCRAP_FACTOR
        INTO X_SCRAP_FACTOR;
       CLOSE CUR_SCRAP_FACTOR;
       
       IF X_SCRAP_FACTOR > 0 THEN
          X_TRANS_QTY := X_TRANS_QTY * (1 + X_SCRAP_FACTOR);
       END IF;
    ELSE
       RETURN (0);
    END IF;
    
    RETURN (X_TRANS_QTY);
  EXCEPTION
    WHEN OTHERS THEN

      /*SRW.MESSAGE(100
                 ,'DB error in process loss ' || SQLERRM)*/NULL;

      errcode1 := sqlcode;
      errmsg1 := sqlerrm;
      
      RETURN (0);
  END TRANS_QTYFORMULA_005;

  FUNCTION BEFOREREPORT RETURN BOOLEAN IS
    CO_CODE VARCHAR2(4);
  BEGIN
    P_CONC_REQUEST_ID := FND_GLOBAL.CONC_REQUEST_ID;
    /*SRW.USER_EXIT('FND SRWINIT')*/NULL;
    DELETE FROM GME_SCALE_DETAIL;
    FND_MSG_PUB.INITIALIZE;
    RETURN (TRUE);
    RETURN NULL;
  EXCEPTION
    WHEN OTHERS THEN
      RETURN (TRUE);
  END BEFOREREPORT;

  FUNCTION CF_U_VALUE_VARIANCEFORMULA(CF_ACTL_QTY_ITEMUM IN NUMBER
                                     ,TRANS_QTY IN NUMBER
                                     ,CF_VALUE_VARIANCE IN NUMBER) RETURN NUMBER IS
  BEGIN
    IF (CF_ACTL_QTY_ITEMUM > 0) AND (TRANS_QTY > 0) THEN
      RETURN (CF_VALUE_VARIANCE);
    ELSE
      RETURN (0);
    END IF;
    RETURN NULL;
  END CF_U_VALUE_VARIANCEFORMULA;

  FUNCTION CF_S_VALUE_VARIANCEFORMULA(CF_ACTL_QTY_ITEMUM IN NUMBER
                                     ,TRANS_QTY IN NUMBER
                                     ,CF_VALUE_VARIANCE IN NUMBER) RETURN NUMBER IS
  BEGIN
    IF (CF_ACTL_QTY_ITEMUM = 0) OR (TRANS_QTY = 0) THEN
      RETURN (CF_VALUE_VARIANCE);
    ELSE
      RETURN (0);
    END IF;
    RETURN NULL;
  END CF_S_VALUE_VARIANCEFORMULA;

  FUNCTION CF_TOTAL_VARIANCEFORMULA(CS_TOT_U_VALUE_VARIANCE IN NUMBER
                                   ,CS_TOT_S_VALUE_VARIANCE IN NUMBER) RETURN NUMBER IS
  BEGIN
    RETURN (CS_TOT_U_VALUE_VARIANCE + CS_TOT_S_VALUE_VARIANCE);
  END CF_TOTAL_VARIANCEFORMULA;

  FUNCTION AFTERREPORT RETURN BOOLEAN IS
  BEGIN
    DELETE FROM GME_SCALE_DETAIL;
    /*SRW.USER_EXIT('FND SRWEXIT')*/NULL;
    RETURN (TRUE);
  END AFTERREPORT;

  FUNCTION CF_DELETE_TRANSFORMULA RETURN NUMBER IS
   PRAGMA AUTONOMOUS_TRANSACTION;
  BEGIN
    DELETE FROM GME_SCALE_DETAIL;
    commit;
    RETURN (1);
  END CF_DELETE_TRANSFORMULA;

  PROCEDURE HEADER IS
  BEGIN
    NULL;
  END HEADER;

  FUNCTION CF_UOMFORMULA(FORMUM IN VARCHAR2
                        ,BATCHUM IN VARCHAR2) RETURN CHAR IS
  BEGIN
    IF FORMUM = ' ' THEN
      RETURN BATCHUM;
    ELSE
      RETURN FORMUM;
    END IF;
  END CF_UOMFORMULA;

  FUNCTION CF_CONTEXT_ORGFORMULA RETURN CHAR IS
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

  FUNCTION CP_ITEM_COST_P RETURN NUMBER IS
  BEGIN
    RETURN CP_ITEM_COST;
  END CP_ITEM_COST_P;

  FUNCTION CP_BATCHRANGE_P RETURN VARCHAR2 IS
  BEGIN
    RETURN CP_BATCHRANGE;
  END CP_BATCHRANGE_P;

  FUNCTION CP_DATERANGE_P RETURN VARCHAR2 IS
  BEGIN
    RETURN CP_DATERANGE;
  END CP_DATERANGE_P;

  FUNCTION CP_ITEMRANGE_P RETURN VARCHAR2 IS
  BEGIN
    RETURN CP_ITEMRANGE;
  END CP_ITEMRANGE_P;

END GME_GMEMUSBV_XMLP_PKG;

 
/
COMMIT;
EXIT;

