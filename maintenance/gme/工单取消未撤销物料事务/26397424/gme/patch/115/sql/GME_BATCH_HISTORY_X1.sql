REM ---- Create FCET ----
REM dbdrv: sql ~PROD ~PATH ~FILE \
REM dbdrv:   none none none sqlplus &phase=ccet \
REM dbdrv:   checkfile:~PROD:~PATH:~FILE &un_gme
REM ---- Apply FCET ----
REM dbdrv: sql ad patch/115/sql AD_ZD_TABLE_APPLY.sql \
REM dbdrv:   none none none sqlplus &phase=acet \
REM dbdrv:   checkfile:~PROD:~PATH:~FILE:fcet GME_BATCH_HISTORY_F1
REM +======================================================================+ 
REM |    Copyright (c) 2005, 2016 Oracle and/or its affiliates.            | 
REM |                         All rights reserved.                         | 
REM |                           Version 12.0.0                             | 
REM +======================================================================+ 
REM $Header: GME_BATCH_HISTORY_X1.sql 120.0.12020000.1 2016/04/08 10:58:00 maychen noship $

REM | FILENAME                                                              |
REM |     GME_BATCH_HISTORY_X1.sql                                          |
REM |                                                                       |
REM | DESCRIPTION                                                           |
REM | Cross Edition Trigger for GME_BATCH_HISTORY.EVENT_ID#1                |
REM | Update columns EVENT_ID from number(10) to number(15)                 |
REM | NOTES                                                                 |
REM | HISTORY                                                               |
REM | 07-APR-2016 May Chen                                                  |
REM |             Generated for new column EVENT_ID#1                       |
REM +=======================================================================+

SET VERIFY OFF;
WHENEVER SQLERROR EXIT FAILURE ROLLBACK;
WHENEVER OSERROR EXIT FAILURE ROLLBACK;

CREATE OR REPLACE TRIGGER GME_BATCH_HISTORY_F1
  BEFORE INSERT OR UPDATE ON &1..GME_BATCH_HISTORY
  for each row forward crossedition  
  disable
BEGIN
      :new.EVENT_ID#1 := :new.EVENT_ID;
     
END;
/
COMMIT;
EXIT;
