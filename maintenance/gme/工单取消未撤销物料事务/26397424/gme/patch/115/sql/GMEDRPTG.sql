REM $Header: GMEDRPTG.sql 120.1.12020000.1 2013/07/26 05:19:27 qzeng noship $
REM dbdrv: sql ~PROD ~PATH ~FILE none none none sql &phase=con \
REM dbdrv: checkfile:nocheck
REM +======================================================================+
REM | Copyright (c) 2013 Oracle Corporation Redwood Shores, California, USA|
REM |                       All rights reserved.                           |
REM +======================================================================+
REM NAME
REM   GMEDRPTG.sql 
REM
REM DESCRIPTION
REM   Script to drop the 3 triggers: GME_RESOURCE_TXNS_I1, GME_RESOURCE_TXNS_U1
REM   and GME_RESOURCE_TXNS_D1.  
REM   
REM HISTORY
REM  18-Jul-2013  QZENG          CREATED. 
REM +======================================================================+
REM AD_ERROR_HANDLING: add 4080
REM AD_ERROR_HANDLING: add 4043

SET VERIFY OFF 
WHENEVER SQLERROR EXIT FAILURE ROLLBACK;
WHENEVER OSERROR  EXIT FAILURE ROLLBACK;
            
Drop trigger GME_RESOURCE_TXNS_I1;
Drop trigger GME_RESOURCE_TXNS_U1;
Drop trigger GME_RESOURCE_TXNS_D1;
COMMIT;     
EXIT;
