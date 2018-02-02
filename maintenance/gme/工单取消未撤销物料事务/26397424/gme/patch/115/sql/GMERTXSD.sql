REM dbdrv: sql ~PROD ~PATH ~FILE none none none sqlplus &phase=en \
REM dbdrv: checkfile(120.1.12010000.3=120.2.12020000.2):~PROD:~PATH:~FILE  
SET VERIFY OFF
WHENEVER SQLERROR EXIT FAILURE ROLLBACK;
WHENEVER OSERROR  EXIT FAILURE ROLLBACK;
/* $Header: GMERTXSD.sql 120.2.12020000.2 2013/07/30 06:52:33 qzeng ship $ */
/* ############################################################################ 
#                               Coypright (c) 2002 Oracle Corporation
#                                      Redwood Shores, CA, USA
#                                        All Rights Reserved.
#
# NAME:
#   Trigger GME_RESOURCES_TXNS_D1
# 
#
# FILE_NAME:
# GMERTXSD.sql   
#   If a transaction for a given resource or instance is deleted,
#   then we have to update the changes to the summary table.
#
# OVERVIEW:
#   This Trigger Executes when the row in gme_resroucetxns is deleted.
#	
# 
# HISTORY:
# date            name                      	comment
# -----           ----                      	-------
# 02-Oct-2002     Eddie Oumerretane             created
#
# 17-Feb-2002     G. Muratore     Bug 11712184
#   Change dbdrv command to make sure it is not rerunnable by checking file version.
# 30-Jul-2013 QZENG Bug17223043   Stub the script as no needed.############################################################################# */
REM CREATE OR REPLACE trigger GME_RESOURCE_TXNS_D1
REM BEFORE  DELETE ON GME_RESOURCE_TXNS
REM FOR EACH ROW
REM DECLARE
REM  pawan kumar changed orgn_code to organization_id for bug 4999940
REM   CURSOR Cur_get_resource_id (p_orgn_id NUMBER,
REM                               p_resource  VARCHAR2) IS
REM     SELECT 
REM        resource_id
REM     FROM   
REM        cr_rsrc_dtl
REM     WHERE
REM        resources = p_resource AND
REM        organization_id = p_orgn_id;

REM   l_resource_id NUMBER(15);

REM BEGIN

  /* We are dealing with a pending transaction */
REM   IF :old.completed_ind = 0 THEN

REM     OPEN Cur_get_resource_id (:old.organization_id, 
REM                               :old.resources);
REM     FETCH Cur_get_resource_id INTO l_resource_id;

REM     IF Cur_get_resource_id%FOUND THEN

REM    Update the table by decrementing the number 
REM    of required units for the deleted interval
REM        UPDATE 
REM           gme_resource_txns_summary
REM        SET 
REM           required_units    = required_units - 1,
REM           last_updated_by   = :old.last_updated_by,
REM           last_update_date  = :old.last_update_date,
REM           last_update_login = :old.last_update_login
REM        WHERE 
REM           start_date                     = :old.start_date AND
REM           end_date                       = :old.end_date   AND
REM           resource_id                    = l_resource_id   AND
REM           NVL(instance_id, -1)           = NVL(:old.instance_id, -1) AND
REM           sequence_dependent_ind         = NVL(:old.sequence_dependent_ind, 0);
     

REM          Delete this row in case the required unit is null 
REM        DELETE 
REM           gme_resource_txns_summary
REM        WHERE 
REM           start_date                     = :old.start_date  AND
REM           end_date                       = :old.end_date    AND
REM           resource_id                    = l_resource_id    AND
REM           NVL(instance_id, -1)           = NVL(:old.instance_id, -1) AND
REM           sequence_dependent_ind         = NVL(:old.sequence_dependent_ind, 0) AND
REM           required_units                 <= 0;

REM     END IF;

REM     CLOSE Cur_get_resource_id;

REM   END IF;

REM   EXCEPTION
REM     WHEN OTHERS THEN
REM       FND_MESSAGE.SET_NAME('GME', 'GME_UNEXPECTED_ERROR');
REM       FND_MESSAGE.SET_TOKEN('ERROR', sqlerrm);
REM       APP_EXCEPTION.raise_exception;

REM END GME_RESOURCE_TXNS_D1;
REM /
COMMIT;
EXIT;
