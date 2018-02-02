REM +========================================================================+ 
REM |                Copyright (c) 1999, 2015 Oracle Corporation 
REM |                   Redwood Shores, California, USA 
REM |                        All rights reserved. 
REM +========================================================================+ 
REM |  Name 
REM |    gmeeresgt1.sql  
REM | 
REM |  Description 
REM |    change synonym GME_ERES_GTMP, connect to table GME_ERES_GTMP2
REM |
REM |    GME_ERES_GTMP
REM | 
REM |  Usage Example 
REM |    sqlplus gme/gme @gmeeresgt1.sql gme gme apps apps 
REM | 
REM |  HISTORY                                                                  
REM |    peng.pan  10-NOV-15        Created
REM |    peng.pan  12-NOV-15        Created
REM +========================================================================+ 
REM ad_error_handling: add 955
REM ad_error_handling: add 1430
REM ad_error_handling: add 942
REM dbdrv: sql ~PROD ~PATH ~FILE none none none sql &phase=os \
REM dbdrv: checkfile:~PROD:~PATH:~FILE \
REM dbdrv: &un_gme &pw_gme &un_apps &pw_apps


SET VERIFY OFF 
WHENEVER SQLERROR EXIT FAILURE ROLLBACK;
WHENEVER OSERROR EXIT FAILURE ROLLBACK;

/* $Header: gmeeresgt1.sql 120.0.12020000.4 2015/11/13 08:13:53 penpan noship $ */

REM | Connecting as Apps User

connect &&3/&&4;

REM | Creating a Synonym

CREATE OR REPLACE SYNONYM GME_ERES_GTMP
FOR &&1..GME_ERES_GTMP2;

REM | Connecting as GME User

connect &&1/&&2;

GRANT ALL ON GME_ERES_GTMP2 TO &&3 WITH GRANT OPTION;

commit;

EXIT;
/
