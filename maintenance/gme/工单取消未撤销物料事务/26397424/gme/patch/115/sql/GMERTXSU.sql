REM dbdrv: sql ~PROD ~PATH ~FILE none none none sqlplus &phase=en \
REM dbdrv: checkfile(120.1.12010000.3=120.2.12020000.2):~PROD:~PATH:~FILE  
SET VERIFY OFF
WHENEVER SQLERROR EXIT FAILURE ROLLBACK;
WHENEVER OSERROR  EXIT FAILURE ROLLBACK;
/* $Header: GMERTXSU.sql 120.2.12020000.2 2013/07/30 06:57:55 qzeng ship $ */
/* ############################################################################ 
#                               Coypright (c) 2002 Oracle Corporation
#                                      Redwood Shores, CA, USA
#                                        All Rights Reserved.
#
# NAME:
#   Trigger GME_RESOURCES_TXNS_U1
# 
#
# FILE_NAME:
# GMERTXSI.sql   
#   If a transaction is updated (start/end dates or completed ind), then we 
#   need to update the changes to the summary table.
#
# OVERVIEW:
#   This trigger executes when a row is updated in gme_resrouce_txns.
#	
# 
# HISTORY:
# date            name                      	comment
# -----           ----                      	-------
# 02-Oct-2002     Eddie Oumerretane             created
#
# 17-Feb-2002     G. Muratore     Bug 11712184
#   Change dbdrv command to make sure it is not rerunnable by checking file version.
# 30-Jul-2013 QZENG Bug17223043   Stub the script as no needed
############################################################################# */
REM CREATE OR REPLACE trigger GME_RESOURCE_TXNS_U1
REM BEFORE  UPDATE ON GME_RESOURCE_TXNS
REM FOR EACH ROW
REM DECLARE
REM  pawan kumar changed orgn_code to organization_id for bug 4999940
REM   l_resource_id NUMBER(15);

REM   FUNCTION Get_Resource_Id (p_orgn_id NUMBER,
REM                             p_resource_code VARCHAR2) RETURN NUMBER IS


REM     l_resource_id NUMBER(15);

REM     CURSOR Cur_get_resource_id IS 
REM       SELECT 
REM         resource_id
REM       FROM   
REM         cr_rsrc_dtl
REM       WHERE
REM         resources = p_resource_code AND
REM         organization_id = p_orgn_id;

REM   BEGIN

REM     OPEN Cur_get_resource_id;

REM     FETCH Cur_get_resource_id INTO l_resource_id;

REM     IF Cur_get_resource_id%NOTFOUND THEN
REM       l_resource_id := NULL;
REM     END IF;

REM     CLOSE Cur_get_resource_id;

REM     RETURN l_resource_id;

REM   END Get_Resource_Id;


REM BEGIN

  /* We have changed a pending transaction */
REM   IF :new.completed_ind = 0 AND :old.completed_ind = 0 THEN

REM     l_resource_id := Get_Resource_Id (:new.organization_id, :new.resources);

REM     IF (:old.start_date    <> :new.start_date OR
REM         :old.end_date      <> :new.end_date) AND l_resource_id IS NOT NULL THEN

      REM - First try to update the table by decrementing the number 
      REM - of required units for the old interval.

REM       UPDATE 
REM          gme_resource_txns_summary
REM       SET 
REM          required_units    = required_units - 1,
REM          last_updated_by   = :new.last_updated_by,
REM          last_update_date  = :new.last_update_date,
REM          last_update_login = :new.last_update_login
REM       WHERE 
REM          start_date                     = :old.start_date AND
REM          end_date                       = :old.end_date   AND
REM          resource_id                    = l_resource_id   AND
REM          NVL(instance_id, -1)           = NVL(:old.instance_id, -1) AND
REM          sequence_dependent_ind         = NVL(:old.sequence_dependent_ind, 0);
      
      REM - Next, delete the interval if the required units are null
REM       DELETE
REM          gme_resource_txns_summary
REM       WHERE 
REM          start_date                     = :old.start_date  AND
REM          end_date                       = :old.end_date    AND
REM          resource_id                    = l_resource_id    AND
REM          NVL(instance_id, -1)           = NVL(:old.instance_id, -1) AND
REM          sequence_dependent_ind         = NVL(:old.sequence_dependent_ind, 0) AND
REM          required_units                <= 0;

REM       IF :new.start_date < :new.end_date THEN

        REM - Try to update the table by incrementing the required units
        REM - within the new interval
REM         UPDATE 
REM          gme_resource_txns_summary
REM         SET 
REM          required_units    = required_units + 1,
REM          last_updated_by   = :new.last_updated_by,
REM          last_update_date  = :new.last_update_date,
REM          last_update_login = :new.last_update_login
REM         WHERE 
REM          start_date                     = :new.start_date AND
REM          end_date                       = :new.end_date   AND
REM          resource_id                    = l_resource_id   AND
REM          NVL(instance_id, -1)           = NVL(:new.instance_id, -1) AND
REM          sequence_dependent_ind         = NVL(:new.sequence_dependent_ind, 0);


REM         IF (SQL%NOTFOUND AND
REM           :new.start_date < :new.end_date) THEN

          REM - This new interval does not exist, we need to create it

REM           INSERT INTO gme_resource_txns_summary
REM                     ( resource_id 
REM                      ,instance_id
REM                      ,start_date 
REM                      ,end_date  
REM                      ,required_units 
REM                      ,sequence_dependent_ind
REM                      ,creation_date    
REM                      ,last_update_date
REM                      ,created_by     
REM                      ,last_updated_by 
REM                      ,last_update_login)
REM              VALUES
REM                     ( l_resource_id 
REM                      ,:new.instance_id
REM                      ,:new.start_date 
REM                      ,:new.end_date  
REM                      ,1 
REM                      ,NVL(:new.sequence_dependent_ind, 0)
REM                      ,:new.creation_date    
REM                      ,:new.last_update_date
REM                      ,:new.created_by     
REM                      ,:new.last_updated_by 
REM                      ,:new.last_update_login);
REM         END IF;

REM       END IF;

REM     END IF;
 
  /* We are completing a transaction. This is treated as a delete */
REM  ELSIF :old.completed_ind = 0 AND :new.completed_ind = 1 THEN

REM     l_resource_id := Get_Resource_Id (:new.organization_id, :new.resources);

REM     IF l_resource_id IS NOT NULL THEN

      REM - Update the table by decrementing the number 
      REM - of required units for the deleted interval
REM       UPDATE 
REM          gme_resource_txns_summary
REM       SET 
REM          required_units    = required_units - 1,
REM          last_updated_by   = :new.last_updated_by,
REM          last_update_date  = :new.last_update_date,
REM          last_update_login = :new.last_update_login
REM       WHERE 
REM          start_date                     = :old.start_date AND
REM          end_date                       = :old.end_date   AND
REM          resource_id                    = l_resource_id   AND
REM          NVL(instance_id, -1)           = NVL(:old.instance_id, -1)  AND
REM          sequence_dependent_ind         = NVL(:old.sequence_dependent_ind, 0);

      
      REM - Delete this row in case the required unit is null 
REM       DELETE 
REM          gme_resource_txns_summary
REM       WHERE 
REM          start_date                     = :old.start_date  AND
REM          end_date                       = :old.end_date    AND
REM          resource_id                    = l_resource_id    AND
REM          NVL(instance_id, -1)           = NVL(:old.instance_id, -1) AND
REM          sequence_dependent_ind         = NVL(:old.sequence_dependent_ind, 0) AND
REM          required_units                <= 0;

REM     END IF;

  /* We are reversing a compleed transaction to a pending transaction. */
  /* This is treated as an insert */
REM   ELSIF :old.completed_ind = 1 AND :new.completed_ind = 0 THEN

REM     l_resource_id := Get_Resource_Id (:new.organization_id, :new.resources);

REM     IF :new.start_date < :new.end_date AND l_resource_id IS NOT NULL THEN

      REM - First try to update the table by incrementing the number 
      REM - of required units.
REM       UPDATE 
REM          gme_resource_txns_summary
REM       SET 
REM          required_units    = required_units + 1,
REM          last_updated_by   = :new.last_updated_by,
REM          last_update_date  = :new.last_update_date,
REM          last_update_login = :new.last_update_login
REM       WHERE 
REM          start_date                     = :new.start_date AND
REM          end_date                       = :new.end_date   AND
REM          resource_id                    = l_resource_id   AND
REM          NVL(instance_id, -1)           = NVL(:new.instance_id, -1) AND
REM          sequence_dependent_ind         = NVL(:new.sequence_dependent_ind, 0);


REM       IF (SQL%NOTFOUND) THEN 

        REM - This new interval does not exist, we need to create it

REM         INSERT INTO gme_resource_txns_summary
REM                     ( resource_id 
REM                      ,instance_id
REM                      ,start_date 
REM                      ,end_date  
REM                      ,required_units 
REM                      ,sequence_dependent_ind
REM                      ,creation_date    
REM                      ,last_update_date
REM                      ,created_by     
REM                      ,last_updated_by 
REM                      ,last_update_login)
REM              VALUES
REM                     ( l_resource_id 
REM                      ,:new.instance_id
REM                      ,:new.start_date 
REM                      ,:new.end_date  
REM                      ,1 
REM                      ,NVL(:new.sequence_dependent_ind, 0)
REM                      ,:new.creation_date    
REM                      ,:new.last_update_date
REM                      ,:new.created_by     
REM                      ,:new.last_updated_by 
REM                      ,:new.last_update_login);
REM       END IF;

REM     END IF;

REM   END IF;

REM   EXCEPTION
REM       WHEN OTHERS THEN
REM         FND_MESSAGE.SET_NAME('GME', 'GME_UNEXPECTED_ERROR');
REM         FND_MESSAGE.SET_TOKEN('ERROR', sqlerrm);
REM         APP_EXCEPTION.raise_exception;

REM END GME_RESOURCE_TXNS_U1;
REM /
COMMIT;
EXIT;
