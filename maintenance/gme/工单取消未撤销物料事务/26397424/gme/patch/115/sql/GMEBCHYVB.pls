REM dbdrv: sql ~PROD ~PATH ~FILE none none none package &phase=plb \
REM dbdrv: checkfile(120.0.12010000.2=120.0.12020000.2):~PROD:~PATH:~FILE
REM ===========================================================================
REM   Copyright (c) 2007, 2017 Oracle Corporation Redwood Shores, California, USA
REM                            All Rights Reserved
REM ===========================================================================
REM
REM   FILENAME:  GMEBCHYVB.pls
REM
REM   DESCRIPTION: GME_GMEBCHYV_XMLP_PKG PL/SQL package body
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

  CREATE OR REPLACE PACKAGE BODY GME_GMEBCHYV_XMLP_PKG AS
/* $Header: GMEBCHYVB.pls 120.0.12020000.2 2017/01/04 03:31:06 sizchen ship $ */
  FUNCTION CF_ACCTG_COST2FORMULA(CF_UOM IN VARCHAR2
                                ,ITEMUM IN VARCHAR2
                                ,DITEMID IN NUMBER
                                ,CF_ACTUAL_QTY_UM IN NUMBER) RETURN NUMBER IS
    COST NUMBER;
    V_QTY NUMBER;
    MESSAGE_IND NUMBER;
    MESSAGE_TEXT VARCHAR2(2000);
  BEGIN
    IF CF_UOM <> ITEMUM THEN
      V_QTY := INV_CONVERT.INV_UM_CONVERT(ITEM_ID => DITEMID
                                         ,PRECISION => 5
                                         ,FROM_QUANTITY => NVL(CF_ACTUAL_QTY_UM
                                            ,0)
                                         ,FROM_UNIT => CF_UOM
                                         ,TO_UNIT => ITEMUM
                                         ,FROM_NAME => NULL
                                         ,TO_NAME => NULL);
    ELSE
      V_QTY := NVL(CF_ACTUAL_QTY_UM
                  ,0);
    END IF;
    COST := V_QTY * CP_ITEM_COST;
    RETURN (COST);
  EXCEPTION
    WHEN OTHERS THEN
      GME_COMMON_PVT.COUNT_AND_GET(P_ENCODED => 'F'
                                  ,X_DATA => MESSAGE_TEXT
                                  ,X_COUNT => MESSAGE_IND);
      /*SRW.MESSAGE(100
                 ,'Error in UOM Conversion ' || MESSAGE_TEXT)*/NULL;
      /*RAISE SRW.PROGRAM_ABORT*/RAISE_APPLICATION_ERROR(-20101,null);
  END CF_ACCTG_COST2FORMULA;

  FUNCTION CF_QUANTITY_VARIANCE(CF_ACTUAL_QTY_UM IN NUMBER
                               ,CF_PLAN_QTY_ITEMUM IN NUMBER) RETURN NUMBER IS
    V_RET_VAL NUMBER;
  BEGIN
    V_RET_VAL := (NVL(CF_ACTUAL_QTY_UM
                    ,0) - NVL(CF_PLAN_QTY_ITEMUM
                    ,0));
    RETURN V_RET_VAL;
  END CF_QUANTITY_VARIANCE;

  FUNCTION CF_VALUE_VARIANCE(CF_ACCTG_COST2 IN NUMBER
                            ,CF_ACCTNG_COST1 IN NUMBER) RETURN NUMBER IS
  BEGIN
    RETURN (NVL(CF_ACCTG_COST2
              ,0) - NVL(CF_ACCTNG_COST1
              ,0));
  END CF_VALUE_VARIANCE;

  FUNCTION CF_VARIANCE_PCT(CF_PLAN_QTY_ITEMUM IN NUMBER
                          ,CF_QUANTITY_VARIANCE IN NUMBER) RETURN NUMBER IS
    TEMPFIELD NUMBER;
  BEGIN
    IF ((CF_PLAN_QTY_ITEMUM = 0) OR CF_PLAN_QTY_ITEMUM IS NULL) THEN
      IF CF_QUANTITY_VARIANCE = 0 THEN
        TEMPFIELD := 0;
      ELSE
        TEMPFIELD := 100;
      END IF;
    ELSE
      TEMPFIELD := (CF_QUANTITY_VARIANCE / CF_PLAN_QTY_ITEMUM) * 100;
    END IF;
    RETURN (TEMPFIELD);
  END CF_VARIANCE_PCT;

  FUNCTION CF_FORMULA_QTY(RECIPE_VALIDITY_RULE_ID IN NUMBER
                         ,BATCH_ID IN NUMBER) RETURN NUMBER IS
    PLANQTY NUMBER;
    ITEMUM VARCHAR2(4);
    ORGID NUMBER;
  BEGIN
    IF RECIPE_VALIDITY_RULE_ID IS NOT NULL THEN
      SELECT
        PLAN_QTY,
        DTL_UM
      INTO PLANQTY,ITEMUM
      FROM
        GME_MATERIAL_DETAILS PMD
      WHERE PMD.LINE_TYPE = 1
        AND PMD.BATCH_ID = cf_formula_qty.BATCH_ID
        AND INVENTORY_ITEM_ID = (
        SELECT
          INVENTORY_ITEM_ID
        FROM
          GMD_RECIPE_VALIDITY_RULES
        WHERE RECIPE_VALIDITY_RULE_ID = cf_formula_qty.RECIPE_VALIDITY_RULE_ID )
        AND ORGANIZATION_ID = P_ORG_ID;
    ELSE
      SELECT
        PLAN_QTY,
        DTL_UM
      INTO PLANQTY,ITEMUM
      FROM
        GME_MATERIAL_DETAILS PMD
      WHERE PMD.LINE_TYPE = 1
        AND PMD.LINE_NO = 1
        AND PMD.BATCH_ID = cf_formula_qty.BATCH_ID
        AND ORGANIZATION_ID = P_ORG_ID;
    END IF;
    BATCHUM := ITEMUM;
    RETURN (PLANQTY);
    RETURN NULL;
  EXCEPTION
    WHEN OTHERS THEN
      BATCHUM := NULL;
      RETURN (NULL);
  END CF_FORMULA_QTY;

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

  FUNCTION CF_PLAN_QTY_ITEMUMFORMULA(FORMULALINE_ID IN NUMBER
                                    ,RECIPE_VALIDITY_RULE_ID IN NUMBER
                                    ,FORMULA_ID IN NUMBER
                                    ,BATCH_ID IN NUMBER) RETURN NUMBER IS
    X_ITEM_ID NUMBER(10);
    P_SCALE_TAB GMD_COMMON_SCALE.SCALE_TAB;
    X_SCALE_TAB GMD_COMMON_SCALE.SCALE_TAB;
    X_RETURN_STATUS VARCHAR2(1);
    MESSAGE_IND NUMBER;
    MESSAGE_TEXT VARCHAR2(2000);
    CURSOR CUR_FORMULA_EXISTS IS
      SELECT
        COUNT(1)
      FROM
        FND_DUAL
      WHERE EXISTS (
        SELECT
          FORMULALINE_ID
        FROM
          GME_SCALE_DETAIL
        WHERE FORMULALINE_ID = cf_plan_qty_itemumformula.FORMULALINE_ID );
    CURSOR CUR_MATL_QTY IS
      SELECT
        QTY
      FROM
        GME_SCALE_DETAIL
      WHERE FORMULALINE_ID = cf_plan_qty_itemumformula.FORMULALINE_ID;
    CURSOR CUR_PRIMARY_PROD IS
      SELECT
        INVENTORY_ITEM_ID
      FROM
        GMD_RECIPE_VALIDITY_RULES
      WHERE RECIPE_VALIDITY_RULE_ID = cf_plan_qty_itemumformula.RECIPE_VALIDITY_RULE_ID;
    CURSOR CUR_ITEM_UM(V_ITEM_ID IN NUMBER) IS
      SELECT
        PRIMARY_UOM_CODE
      FROM
        MTL_SYSTEM_ITEMS_B
      WHERE ORGANIZATION_ID = P_ORG_ID
        AND INVENTORY_ITEM_ID = V_ITEM_ID;
    CURSOR CUR_FORM_DTL IS
      SELECT
        QTY,
        DETAIL_UOM
      FROM
        FM_MATL_DTL
      WHERE FORMULA_ID = cf_plan_qty_itemumformula.FORMULA_ID
        AND ORGANIZATION_ID = P_ORG_ID
        AND INVENTORY_ITEM_ID = X_ITEM_ID
        AND LINE_TYPE = 1;
    CURSOR CUR_BATCH_DTL IS
      SELECT
        PLAN_QTY,
        DTL_UM
      FROM
        GME_MATERIAL_DETAILS
      WHERE BATCH_ID = cf_plan_qty_itemumformula.BATCH_ID
        AND INVENTORY_ITEM_ID = X_ITEM_ID
        AND LINE_TYPE = 1;
    CURSOR CUR_PLAN_QTY IS
      SELECT
        QTY
      FROM
        GME_SCALE_DETAIL
      WHERE FORMULALINE_ID = cf_plan_qty_itemumformula.FORMULALINE_ID;
    CURSOR CUR_SCRAP_FACTOR IS
      SELECT
        SCRAP_FACTOR
      FROM
        FM_MATL_DTL
      WHERE FORMULALINE_ID = cf_plan_qty_itemumformula.FORMULALINE_ID
        AND FORMULA_ID = cf_plan_qty_itemumformula.FORMULA_ID
        AND LINE_TYPE = - 1;
    CURSOR CUR_GET_FORMULA_DETAILS IS
      SELECT
        FORMULALINE_ID,
        QTY,
        DETAIL_UOM,
        SCALE_TYPE,
        LINE_TYPE,
        INVENTORY_ITEM_ID,
        SCALE_MULTIPLE,
        SCALE_ROUNDING_VARIANCE,
        ROUNDING_DIRECTION,
        CONTRIBUTE_YIELD_IND
      FROM
        FM_MATL_DTL
      WHERE FORMULA_ID = cf_plan_qty_itemumformula.FORMULA_ID;
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
    pragma autonomous_transaction;
  BEGIN
    IF FORMULALINE_ID > 0 THEN
      OPEN CUR_FORMULA_EXISTS;
      FETCH CUR_FORMULA_EXISTS
       INTO X_COUNT;
      CLOSE CUR_FORMULA_EXISTS;
      IF X_COUNT = 0 THEN
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
          BEGIN
            X_CONV_QTY_FORM := INV_CONVERT.INV_UM_CONVERT(ITEM_ID => X_ITEM_ID
                                                         ,PRECISION => 5
                                                         ,FROM_QUANTITY => X_QTY
                                                         ,FROM_UNIT => X_DTL_UM
                                                         ,TO_UNIT => X_ITEM_UM
                                                         ,FROM_NAME => NULL
                                                         ,TO_NAME => NULL);
          EXCEPTION
            WHEN OTHERS THEN
              GME_COMMON_PVT.COUNT_AND_GET(P_ENCODED => 'F'
                                          ,X_DATA => MESSAGE_TEXT
                                          ,X_COUNT => MESSAGE_IND);
              /*SRW.MESSAGE(100
                         ,'Error in UOM Conversion ' || MESSAGE_TEXT)*/NULL;
              /*RAISE SRW.PROGRAM_ABORT*/RAISE_APPLICATION_ERROR(-20101,null);
          END;
        ELSE
          X_CONV_QTY_FORM := X_QTY;
        END IF;
        OPEN CUR_BATCH_DTL;
        FETCH CUR_BATCH_DTL
         INTO X_QTY,X_DTL_UM;
        CLOSE CUR_BATCH_DTL;
        IF X_DTL_UM <> X_ITEM_UM THEN
          BEGIN
            X_CONV_QTY_BATCH := INV_CONVERT.INV_UM_CONVERT(ITEM_ID => X_ITEM_ID
                                                          ,PRECISION => 5
                                                          ,FROM_QUANTITY => X_QTY
                                                          ,FROM_UNIT => X_DTL_UM
                                                          ,TO_UNIT => X_ITEM_UM
                                                          ,FROM_NAME => NULL
                                                          ,TO_NAME => NULL);
          EXCEPTION
            WHEN OTHERS THEN
              GME_COMMON_PVT.COUNT_AND_GET(P_ENCODED => 'F'
                                          ,X_DATA => MESSAGE_TEXT
                                          ,X_COUNT => MESSAGE_IND);
              /*SRW.MESSAGE(100
                         ,'Error in UOM Conversion ' || MESSAGE_TEXT)*/NULL;
              /*RAISE SRW.PROGRAM_ABORT*/RAISE_APPLICATION_ERROR(-20101,null);
          END;
        ELSE
          X_CONV_QTY_BATCH := X_QTY;
        END IF;
        IF X_CONV_QTY_FORM > 0 THEN
          X_SCALE_FACTOR := X_CONV_QTY_BATCH / X_CONV_QTY_FORM;
        ELSE
          X_SCALE_FACTOR := 0;
        END IF;
        I := 1;
        FOR fm_rec IN CUR_GET_FORMULA_DETAILS LOOP
          P_SCALE_TAB(I).LINE_NO := FM_REC.FORMULALINE_ID;
          P_SCALE_TAB(I).LINE_TYPE := FM_REC.LINE_TYPE;
          P_SCALE_TAB(I).INVENTORY_ITEM_ID := FM_REC.INVENTORY_ITEM_ID;
          P_SCALE_TAB(I).QTY := FM_REC.QTY;
          P_SCALE_TAB(I).DETAIL_UOM := FM_REC.DETAIL_UOM;
          P_SCALE_TAB(I).SCALE_TYPE := FM_REC.SCALE_TYPE;
          P_SCALE_TAB(I).SCALE_MULTIPLE := FM_REC.SCALE_MULTIPLE;
          P_SCALE_TAB(I).SCALE_ROUNDING_VARIANCE := FM_REC.SCALE_ROUNDING_VARIANCE;
          P_SCALE_TAB(I).ROUNDING_DIRECTION := FM_REC.ROUNDING_DIRECTION;
          P_SCALE_TAB(I).CONTRIBUTE_YIELD_IND := FM_REC.CONTRIBUTE_YIELD_IND;
          I := I + 1;
        END LOOP;
        GMD_COMMON_SCALE.SCALE(P_SCALE_TAB
                              ,P_ORG_ID
                              ,X_SCALE_FACTOR
                              ,'OUTPUTS'
                              ,X_SCALE_TAB
                              ,X_RETURN_STATUS);
        IF X_RETURN_STATUS = 'S' THEN
          FOR i IN 1 .. X_SCALE_TAB.COUNT LOOP
            INSERT INTO GME_SCALE_DETAIL(FORMULALINE_ID,LINE_TYPE,ITEM_ID,QTY,ITEM_UM,SCALE_TYPE,CONV_QTY)
            VALUES   (X_SCALE_TAB(I).LINE_NO
              , X_SCALE_TAB(I).LINE_TYPE
              ,X_SCALE_TAB(I).INVENTORY_ITEM_ID
              , X_SCALE_TAB(I).QTY
              , X_SCALE_TAB(I).DETAIL_UOM
              , X_SCALE_TAB(I).SCALE_TYPE
              , X_SCALE_TAB(I).QTY);
          END LOOP;
          commit;
        END IF;
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
    RETURN NULL;
  EXCEPTION
    WHEN OTHERS THEN
      RETURN (0);
  END CF_PLAN_QTY_ITEMUMFORMULA;

  FUNCTION CF_ACTUAL_QTY_UMFORMULA(MD_BATCHUM IN VARCHAR2
                                  ,MD_FORMUM IN VARCHAR2
                                  ,AQTY IN NUMBER
                                  ,ITEM_ID IN NUMBER) RETURN NUMBER IS
    V_ACTUAL_QTY_ITEMUM NUMBER;
    V_RETVAL NUMBER;
  BEGIN
    IF (MD_BATCHUM <> MD_FORMUM OR NVL(AQTY
       ,0) <> 0) THEN
      V_ACTUAL_QTY_ITEMUM := 0;
      V_ACTUAL_QTY_ITEMUM := INV_CONVERT.INV_UM_CONVERT(ITEM_ID => cf_actual_qty_umformula.ITEM_ID
                                                       ,PRECISION => 5
                                                       ,FROM_QUANTITY => NVL(AQTY
                                                          ,0)
                                                       ,FROM_UNIT => MD_BATCHUM
                                                       ,TO_UNIT => MD_FORMUM
                                                       ,FROM_NAME => NULL
                                                       ,TO_NAME => NULL);
      RETURN V_ACTUAL_QTY_ITEMUM;
    ELSE
      RETURN NVL(AQTY
                ,0);
    END IF;
    RETURN NULL;
  END CF_ACTUAL_QTY_UMFORMULA;

  FUNCTION BEFOREREPORT RETURN BOOLEAN IS
    CO_CODE VARCHAR2(4);
  BEGIN
    P_CONC_REQUEST_ID := FND_GLOBAL.CONC_REQUEST_ID;
    /*SRW.USER_EXIT('FND SRWINIT')*/NULL;
    DELETE FROM GME_SCALE_DETAIL;
    RETURN (TRUE);
    RETURN NULL;
  EXCEPTION
    WHEN OTHERS THEN
      RETURN (TRUE);
  END BEFOREREPORT;

  FUNCTION CF_ACCTNG_COST1FORMULA(DITEMID IN NUMBER
                                 ,ACTUAL_CMPLT_DATE IN DATE
                                 ,CF_UOM IN VARCHAR2
                                 ,ITEMUM IN VARCHAR2
                                 ,CF_PLAN_QTY_ITEMUM IN NUMBER) RETURN NUMBER IS
  BEGIN
    DECLARE
      V_COST_BASIS NUMBER(1);
      V_COST NUMBER;
      V_GL_COST_MTHD cm_mthd_mst.cost_mthd_code%type;  --Bug 24917349 Enlarge size from 4 to 10, because cm_mthd_mst.cost_mthd_code is also varchar2(10)
      V_CO_CODE VARCHAR2(4);
      V_RET_VAL NUMBER;
      V_COST_MTHD VARCHAR2(1) := NULL;
      V_CMPNTCLS_ID NUMBER := NULL;
      V_ANALYSIS_CODE VARCHAR2(1) := NULL;
      V_RETREIVE_IND NUMBER := NULL;
      V_COST_CMPNTCLS_ID NUMBER;
      V_COST_ANALYSIS_CODE VARCHAR2(1) := NULL;
      V_ACCTG_COST NUMBER;
      V_STND VARCHAR2(4) := 'STND';
      COST NUMBER;
      X_NUM_ROWS NUMBER;
      V_QTY NUMBER;
      MESSAGE_IND NUMBER;
      MESSAGE_TEXT VARCHAR2(2000);
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
                                                     ,P_COST_METHOD => V_GL_COST_MTHD
                                                     ,P_COST_COMPONENT_CLASS_ID => V_CMPNTCLS_ID
                                                     ,P_COST_ANALYSIS_CODE => V_ANALYSIS_CODE
                                                     ,X_TOTAL_COST => V_ACCTG_COST
                                                     ,X_NO_OF_ROWS => X_NUM_ROWS
                                                     ,X_RETURN_STATUS => L_RETURN_STATUS
                                                     ,X_MSG_COUNT => L_MSG_COUNT
                                                     ,X_MSG_DATA => L_MSG_DATA);
      IF V_RET_VAL = 1 THEN
        V_COST := V_ACCTG_COST;
      ELSE
        V_COST := 0;
      END IF;
      BEGIN
        IF CF_UOM <> ITEMUM THEN
          V_QTY := INV_CONVERT.INV_UM_CONVERT(ITEM_ID => DITEMID
                                             ,PRECISION => 5
                                             ,FROM_QUANTITY => NVL(CF_PLAN_QTY_ITEMUM
                                                ,0)
                                             ,FROM_UNIT => CF_UOM
                                             ,TO_UNIT => ITEMUM
                                             ,FROM_NAME => NULL
                                             ,TO_NAME => NULL);
        ELSE
          V_QTY := NVL(CF_PLAN_QTY_ITEMUM
                      ,0);
        END IF;
      EXCEPTION
        WHEN OTHERS THEN
          GME_COMMON_PVT.COUNT_AND_GET(P_ENCODED => 'F'
                                      ,X_DATA => MESSAGE_TEXT
                                      ,X_COUNT => MESSAGE_IND);
          /*SRW.MESSAGE(100
                     ,'Error in UOM Conversion ' || MESSAGE_TEXT)*/NULL;
          /*RAISE SRW.PROGRAM_ABORT*/RAISE_APPLICATION_ERROR(-20101,null);
      END;
      COST := V_QTY * V_COST;
      CP_ITEM_COST := V_COST;
      RETURN (COST);
    EXCEPTION
      WHEN OTHERS THEN
        RETURN NULL;
    END;
    RETURN NULL;
  END CF_ACCTNG_COST1FORMULA;

  FUNCTION CF_DELETE_TRANSFORMULA RETURN NUMBER IS
   pragma autonomous_transaction;
  BEGIN
    DELETE FROM GME_SCALE_DETAIL;
    commit;
    RETURN (1);
  END CF_DELETE_TRANSFORMULA;

  FUNCTION AFTERREPORT RETURN BOOLEAN IS
  BEGIN
    DELETE FROM GME_SCALE_DETAIL;
    /*SRW.USER_EXIT('FND SRWEXIT')*/NULL;
    RETURN (TRUE);
  END AFTERREPORT;

  PROCEDURE HEADER IS
  BEGIN
    NULL;
  END HEADER;

  FUNCTION CF_UOMFORMULA(MD_FORMUM IN VARCHAR2
                        ,MD_BATCHUM IN VARCHAR2) RETURN CHAR IS
  BEGIN
    IF MD_FORMUM = ' ' THEN
      RETURN MD_BATCHUM;
    ELSE
      RETURN MD_FORMUM;
    END IF;
  END CF_UOMFORMULA;

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

  FUNCTION BATCHUM_P RETURN VARCHAR2 IS
  BEGIN
    RETURN BATCHUM;
  END BATCHUM_P;

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

END GME_GMEBCHYV_XMLP_PKG;

 
/
COMMIT;
EXIT;
