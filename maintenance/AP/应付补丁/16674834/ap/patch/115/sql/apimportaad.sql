REM $Header: apimportaad.sql 120.3.12020000.1 2012/06/27 12:56:52 appldev ship $
REM +======================================================================+
REM |   Copyright (c) 2002 Oracle Corporation Belmont, California, USA     |
REM |                       All rights reserved.                           |
REM +======================================================================+
REM FILENAME
REM       apimportaad.sql
REM
REM DESCRIPTION
REM       This installation script prepares the environment for import
REM       application accounting definitions.
REM
REM HISTORY
REM       06-JUL-06 MSWAMINA Created this file as provided by Wynne 
REM
REM +======================================================================+
REM dbdrv: sql ~PROD ~PATH ~FILE none none none sqlplus &phase=dat \
REM dbdrv: checkfile:nocheck
REM

SET VERIFY OFF
WHENEVER SQLERROR EXIT FAILURE ROLLBACK;
WHENEVER OSERROR EXIT FAILURE ROLLBACK

DECLARE

x_staging_context_code VARCHAR2(30);
x_return_status        VARCHAR2(30);

BEGIN

xla_aad_install_pvt.pre_import
(p_application_id        => 200
,p_amb_context_code      => 'DEFAULT'
,x_return_status         => x_return_status);

END;
/

COMMIT;
EXIT;
