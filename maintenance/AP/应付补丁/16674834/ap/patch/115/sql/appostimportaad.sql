REM $Header: appostimportaad.sql 120.4.12020000.1 2012/06/27 13:20:40 appldev ship $
REM +======================================================================+
REM |   Copyright (c) 2002 Oracle Corporation Belmont, California, USA     |
REM |                       All rights reserved.                           |
REM +======================================================================+
REM FILENAME
REM       appostimportaad.sql
REM
REM DESCRIPTION
REM       This installation script populate the application accounting
REM       definitions to the real table.
REM
REM HISTORY
REM       28-JUN-2006  WYCHAN Created   (from xlapostimportaad.sql)
REM
REM +======================================================================+

REM :__SCM_FILE_METADATA__:<METADATA >
REM :__SCM_FILE_METADATA__: <DEPENDENCIES >
REM :__SCM_FILE_METADATA__:  <DEPENDENCY PRODUCT_FAMILY="FINANCIALS" >
REM :__SCM_FILE_METADATA__:   <MODIFIES TYPE="UPG" >
REM :__SCM_FILE_METADATA__:    <MODIFY NAME="XLA_VALIDATE_AAD" />
REM :__SCM_FILE_METADATA__:   </MODIFIES>
REM :__SCM_FILE_METADATA__:  </DEPENDENCY>
REM :__SCM_FILE_METADATA__: </DEPENDENCIES>
REM :__SCM_FILE_METADATA__:</METADATA>

REM dbdrv: sql ~PROD ~PATH ~FILE none none none sqlplus &phase=upg+70 \
REM dbdrv: checkfile:nocheck
REM

SET VERIFY OFF
WHENEVER SQLERROR EXIT FAILURE ROLLBACK;
WHENEVER OSERROR EXIT FAILURE ROLLBACK

DECLARE

x_return_status        VARCHAR2(30);

BEGIN

xla_aad_install_pvt.post_import
(p_application_id        => 200
,p_amb_context_code      => 'DEFAULT'
,p_import_mode           => 'OVERWRITE'
,p_force_overwrite       => 'Y'
,x_return_status         => x_return_status);

END;
/

COMMIT;
EXIT;
