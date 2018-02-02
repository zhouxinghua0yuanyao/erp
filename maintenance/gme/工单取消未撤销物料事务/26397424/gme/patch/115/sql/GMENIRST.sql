REM $Header: GMENIRST.sql 120.0.12020000.2 2013/07/30 06:42:41 qzeng ship $
REM dbdrv: sql ~PROD ~PATH ~FILE none none none sqlplus &phase=upg+2 \
REM dbdrv: checkfile:nocheck
REM *********************************************************************
REM *
REM * FILE:    GMENIRST.sql
REM *
REM * PURPOSE: This SQL script initialize the resource transaction     
REM *          summary table. It also deletes entry in the obsolete
REM *          GME_BATCH_STEP_RSRC_SUMMARY table. 
REM *
REM * HISTORY:
REM * ========
REM * 04-OCT-2002   Eddie Oumerretane   Created
REM * 07-MAR-2003   Shrikant Nene              
REM *  As GME Migration is becoming part of the Family pack for 11.5.9
REM *  This needs to run after the GME Migration.  Hence changing the 
REM *  phase to dat+1. (GME Migration runs in phase dat).
REM * 18-JUL-2005   Chandrashekar Tiruvidula
REM *  Creating this new file from GMEIRST.sql to change phase
REM * 30-Jul-2013 QZENG Bug17223043   Commentted line which is no needed.
REM **********************************************************************
set echo off
set verify off
set serveroutput off

WHENEVER OSERROR EXIT FAILURE ROLLBACK;
WHENEVER SQLERROR EXIT FAILURE ROLLBACK;


DECLARE
BEGIN 
  --- Delete entries in the obsolete resource summary table table
  DELETE FROM GME_BATCH_STEP_RSRC_SUMMARY;

  --- Initialize GME_RESOURCE_TXNS_SUMMARY
  --GME_INIT_RSRC_TXNS_SUMMARY.Initialize_Rsrc_Txns_Summary; --commented by qzeng bug17223043
END;
/
COMMIT;
EXIT;
