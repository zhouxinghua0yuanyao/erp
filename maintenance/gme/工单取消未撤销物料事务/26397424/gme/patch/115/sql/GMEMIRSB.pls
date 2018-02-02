REM dbdrv: sql ~PROD ~PATH ~FILE none none none package &phase=plb \
REM dbdrv: checkfile(120.0.12010000.2=120.0.12020000.2)(115.2=120.0):~PROD:~PATH:~FILE
Set verify off;
WHENEVER OSERROR EXIT FAILURE ROLLBACK
whenever sqlerror exit failure rollback;

CREATE OR REPLACE PACKAGE BODY GME_INIT_RSRC_TXNS_SUMMARY AS
/* $Header: GMEMIRSB.pls 120.0.12020000.2 2013/07/30 06:40:05 qzeng ship $ */ 
/*============================================================================  
 |                         Copyright (c) 2002 Oracle Corporation                
 |                             Redwood Shores, California, USA                  
 |                                  All rights reserved                         
 =============================================================================  
 |   FILENAME                                                                   
 |      GMEMIRSB.pls                                                            
 |                                                                              
 |   DESCRIPTION                                                                
 |      Package body containing the procedures used to populate the new         
 |      resource transaction summary table.
 |                                                                              
 |   NOTES                                                                      
 |                                                                              
 |   HISTORY                                                                    
 |     03-OCT-2002 Eddie Oumerretane   Created.                                 
 |     04-NOV-2002 Eddie Oumerretane   Added a check to prevent the table 
 |                 from being re-populated if the patch 2565952 has already been
 |                 applied.
 |     30-Jul-2013 QZENG Bug17223043   Stub the procedure as no needed.
 =============================================================================  
*/                                                                              
                                                                                
  PROCEDURE Initialize_Rsrc_Txns_Summary IS


    /*CURSOR Get_Rsrc_Txns_Summary IS
     SELECT 
       COUNT(*)
     FROM
       gme_resource_txns_summary;

    CURSOR Cur_Get_Rsrc_Txns IS

      SELECT
       orgn_code,
       resources,
       instance_id,
       start_date,
       end_date,
       sequence_dependent_ind,
       last_updated_by,
       last_update_login 
      FROM
       gme_resource_txns
      WHERE
       completed_ind = 0 AND
       delete_mark   = 0;

    CURSOR Cur_get_resource_id (p_orgn_code VARCHAR2,
                                p_resource  VARCHAR2) IS
      SELECT
        resource_id
      FROM
        cr_rsrc_dtl
      WHERE
        resources = p_resource AND
        orgn_code = p_orgn_code;

    l_resource_id GME_RESOURCE_TXNS_SUMMARY.resource_id%TYPE;
    l_sysdate     DATE;
    l_count       NUMBER;
    */
  BEGIN                                                                         
                                                                                
    /*OPEN Get_Rsrc_Txns_Summary;
    FETCH Get_Rsrc_Txns_Summary INTO l_count;

    --- If the resource transaction summary table is not empty, then we
    --- assume that patch 2565952 has already been applied and that the
    --- table has alredy been populated.
    IF Get_Rsrc_Txns_Summary%NOTFOUND OR l_count <= 0 THEN

      FOR txns_rec IN  Cur_Get_Rsrc_Txns
      LOOP 
                                                                                
        OPEN Cur_get_resource_id (txns_rec.orgn_code,
                                txns_rec.resources);

        FETCH Cur_get_resource_id INTO l_resource_id;

        IF Cur_get_resource_id%NOTFOUND THEN
          CLOSE Cur_get_resource_id;
        ELSE

          CLOSE Cur_get_resource_id;

          l_sysdate := SYSDATE;


          --- First try to update the table by incrementing the number
          --- of required units.

          UPDATE
             gme_resource_txns_summary
          SET
             required_units    = required_units + 1,
             last_updated_by   = txns_rec.last_updated_by,
             last_update_date  = l_sysdate,
             last_update_login = txns_rec.last_update_login
          WHERE
             start_date                     = txns_rec.start_date AND
             end_date                       = txns_rec.end_date   AND
             resource_id                    = l_resource_id       AND
             NVL(instance_id, -1)           = NVL(txns_rec.instance_id, -1) AND
             sequence_dependent_ind         = NVL(txns_rec.sequence_dependent_ind, 0);

          IF (SQL%NOTFOUND) THEN

            --- This new interval does not exist, we need to create it
  
            INSERT INTO gme_resource_txns_summary
                    ( resource_id
                     ,instance_id
                     ,start_date
                     ,end_date
                     ,required_units
                     ,sequence_dependent_ind
                     ,creation_date
                     ,last_update_date
                     ,created_by
                     ,last_updated_by
                     ,last_update_login)
             VALUES
                    ( l_resource_id
                     ,txns_rec.instance_id
                     ,txns_rec.start_date
                     ,txns_rec.end_date
                     ,1
                     ,NVL(txns_rec.sequence_dependent_ind, 0)
                     ,l_sysdate
                     ,l_sysdate
                     ,txns_rec.last_updated_by
                     ,txns_rec.last_updated_by
                     ,txns_rec.last_update_login);
          END IF;

        END IF;

      END LOOP;

    END IF;

    CLOSE Get_Rsrc_Txns_Summary;

    COMMIT;

    EXCEPTION
      WHEN OTHERS THEN
        CLOSE Get_Rsrc_Txns_Summary;
        ROLLBACK;
        FND_MESSAGE.SET_NAME('GME', 'GME_UNEXPECTED_ERROR');
        FND_MESSAGE.SET_TOKEN('ERROR', sqlerrm);
        APP_EXCEPTION.raise_exception;*/
    NULL;
  END Initialize_Rsrc_Txns_Summary;
                                           
END GME_INIT_RSRC_TXNS_SUMMARY; 
/
COMMIT;
EXIT;
