/* +======================================================================+ */
/* |    Copyright (c) 2005, 2014 Oracle and/or its affiliates.           | */
/* |                         All rights reserved.                         | */
/* |                           Version 12.0.0                             | */
/* +======================================================================+ */
REM dbdrv: sql ~PROD ~PATH ~FILE none none none package &phase=plb \
REM dbdrv: checkfile(120.1.12010000.2=120.1.12020000.2)(115.17=120.1):~PROD:~PATH:~FILE
SET VERIFY OFF
WHENEVER SQLERROR EXIT FAILURE ROLLBACK;
WHENEVER OSERROR EXIT FAILURE ROLLBACK;

CREATE OR REPLACE PACKAGE BODY gme_cancel_step_pvt AS
/*  $Header: GMEVCCSB.pls 120.1.12020000.2 2014/12/04 08:34:23 shalchen ship $    */
   g_debug      VARCHAR2 (5)  := fnd_profile.VALUE ('AFLOG_LEVEL');
   g_pkg_name   VARCHAR2 (30) := 'GME_CANCEL_STEP_PVT';

/*
REM *********************************************************************
REM *
REM * FILE:    GMEVCCSB.pls
REM * PURPOSE: Package Body for the GME step cancel api
REM * AUTHOR:  Pawan Kumar, OPM Development
REM * DATE:    28-April-2005
REM * HISTORY:
REM * ========

REM *  Shaliu Chen     04-DEC-2014  ER 19161894
REM *  Modify cancel_stpe to invoke cancel PO/Requisition program if step is OSP step
REM *
REM *
REM **********************************************************************
*/

   /*======================================================================================
Procedure
  Cancel_Step
Description
  This particular procedure call close the batch steps.
Parameters
  p_batch_step       The batch step row to identify the step.
  p_validation_level    Errors to skip before returning - Default 100
  x_message_count    The number of messages in the message stack
  x_message_list     message stack where the api writes its messages
  x_return_status    outcome of the API call
            S - Success
            E - Error
            U - Unexpected error
======================================================================================*/
   PROCEDURE cancel_step (
      p_batch_step_rec         IN              gme_batch_steps%ROWTYPE
     ,p_update_inventory_ind   IN              VARCHAR2
     ,x_batch_step_rec         OUT NOCOPY      gme_batch_steps%ROWTYPE
     ,x_return_status          OUT NOCOPY      VARCHAR2)
   IS
      /* Miscellaneous */
      l_resource_tab               gme_common_pvt.number_tab;
      l_api_name                   VARCHAR2 (20)             := 'Cancel_step';
      
      /*ER 19161894  Shaliu Chen 18-JUL-2014*/
      l_propagate_change_to_po         NUMBER;
      l_osp_resource_flag              NUMBER;

      cancel_po_req_fail               EXCEPTION;
      l_batch_header_tab               gme_batch_header%ROWTYPE;
      l_message_count                  NUMBER;         
      l_message_data                   VARCHAR2(2000); 
      l_return_status                  VARCHAR2 (1);       
      /*END ER 19161894*/
      /* Exception definitions */
      invalid_step_status          EXCEPTION;
      batch_step_upd_err           EXCEPTION;
      resource_txns_gtmp_del_err   EXCEPTION;
      l_resource_txns              gme_resource_txns_gtmp%ROWTYPE;
      l_resource_txns_tab          gme_common_pvt.resource_transactions_tab;

      CURSOR cur_get_resource_ids (v_batchstep_id NUMBER)
      IS
         SELECT batchstep_resource_id
           FROM gme_batch_step_resources
          WHERE batchstep_id = v_batchstep_id;
          
      /*ER 19161894  Shaliu Chen 18-JUL-2014*/
      CURSOR cur_get_osp_parameter(v_organization_id NUMBER)
      IS
         SELECT propagate_change_to_po
           FROM gme_parameters
          WHERE organization_id = v_organization_id;
          
      CURSOR cur_get_batch_rec (v_batch_id NUMBER)
      IS
        SELECT *
          FROM gme_batch_header
         WHERE batch_id = v_batch_id;              
   BEGIN
      /* Set the save point before processing */
      SAVEPOINT cancel_batch_step;

      IF (g_debug <= gme_debug.g_log_procedure) THEN
         gme_debug.put_line (g_pkg_name || '.' || l_api_name || ':'
                             || 'Entering');
      END IF;

      -- Set the return status to success initially
      x_return_status := fnd_api.g_ret_sts_success;
      -- Initialize output batch step
      x_batch_step_rec := p_batch_step_rec;

      -- remove the resource information for the gme_batch_step_rsrc_summary table
      IF p_update_inventory_ind = 'Y' THEN
         /* Get all the resources associated with the step */
         OPEN cur_get_resource_ids (x_batch_step_rec.batchstep_id);

         FETCH cur_get_resource_ids
         BULK COLLECT INTO l_resource_tab;

         CLOSE cur_get_resource_ids;

         FOR i IN 1 .. l_resource_tab.COUNT LOOP
            l_resource_txns.line_id := l_resource_tab (i);
            gme_resource_engine_pvt.fetch_active_resources
                                      (p_resource_rec       => l_resource_txns
                                      ,x_resource_tbl       => l_resource_txns_tab
                                      ,x_return_status      => x_return_status);

            -- Delete the resource transactions
            FOR j IN 1 .. l_resource_txns_tab.COUNT LOOP
               l_resource_txns_tab (j).action_code := 'DEL';

               IF (g_debug <= gme_debug.g_log_procedure) THEN
                  gme_debug.put_line (   g_pkg_name
                                      || '.'
                                      || l_api_name
                                      || ':'
                                      || ' Calling  resource delete_row');
               END IF;

               IF NOT (gme_resource_txns_gtmp_dbl.update_row
                                   (p_resource_txns      => l_resource_txns_tab
                                                                           (j) ) ) THEN
                  RAISE resource_txns_gtmp_del_err;
               END IF;
            END LOOP;                         /*end for l_resource_txns_tab */
         END LOOP;                                 /*end for l_resource_tab */
      END IF;                            /* IF p_update_inventory_ind = 'Y' */
      
      
     /*
      BEGIN ER 19161894  Shaliu Chen 04-DEC-2014
      Added for OPM Step Level OSP Project
      If Step is OSP step,invoke cancelreqpo to
      cancel corresponding PO/Requisition.
      */
      IF g_debug <= gme_debug.g_log_statement THEN
             gme_debug.put_line (   g_pkg_name
                                 || '.'
                                 || l_api_name
                                 || ' Calling CancelPOReq API');
      END IF;

      BEGIN
        IF gme_osp.check_release_version THEN

           /*
            Check whether the step include Outside resource.
           */
          SELECT count(1)
            INTO l_osp_resource_flag
            FROM gme_batch_step_resources gbsr
           WHERE batch_id = x_batch_step_rec.batch_id
             AND batchstep_id = x_batch_step_rec.batchstep_id
             AND EXISTS (SELECT 1
                           FROM cr_rsrc_dtl crd,
                                cr_rsrc_mst crm
                          WHERE crd.organization_id = gbsr.organization_id
                            AND crd.resources = gbsr.resources
                            AND crd.resources = crm.resources
                            AND crd.purchase_item_id IS NOT NULL
                            AND NVL(crm.outside_process_ind,gme_osp.g_osp_ind_disabled) = gme_osp.g_osp_ind_enabled);

          IF l_osp_resource_flag = 1 THEN

            /*
             Check GME parameters
            */
            OPEN cur_get_batch_rec(x_batch_step_rec.batch_id);
            FETCH cur_get_batch_rec INTO l_batch_header_tab;
            CLOSE cur_get_batch_rec;
            
            OPEN cur_get_osp_parameter(l_batch_header_tab.organization_id);
            FETCH cur_get_osp_parameter INTO l_propagate_change_to_po;
            CLOSE cur_get_osp_parameter;
            /*
             Cancel Open PO linked to the step if propagate_change_to_po
             parameter is Automatic
            */
            IF (NVL(l_propagate_change_to_po,gme_osp.g_propagate_change_manual)
                       = gme_osp.g_propagate_change_automatic) THEN
              /*
                Invoke CanelPOReq  to cancel the PO/Req
                linked to the step.
              */

              gme_osp.cancelPOReq (p_batch_id      => x_batch_step_rec.batch_id,
                                   p_org_id        => l_batch_header_tab.organization_id,
                                   p_batchstep_id  => x_batch_step_rec.batchstep_id,
                                   x_return_status => l_return_status,
                                   x_message_list  => l_message_data,
                                   x_message_count => l_message_count
                                   );

                IF l_return_status <> fnd_api.g_ret_sts_success THEN
                  RAISE cancel_po_req_fail;
                END IF;
            /*
             Throw a warning out if propagate_change_to_po parameter is manual
            */
            ELSIF (NVL(l_propagate_change_to_po,gme_osp.g_propagate_change_manual)
                   = gme_osp.g_propagate_change_manual) THEN
              gme_common_pvt.log_message('GME_PROPAGATE_CHANGE_MANUAL');
            END IF;
          END IF;
        END IF;
      EXCEPTION
        WHEN OTHERS THEN
          l_message_data := 'unexpected error: ' || SQLERRM || 'SQLCODE = ' || SQLCODE;
          gme_debug.put_line('l_errMsg:'||l_message_data);
          RAISE cancel_po_req_fail;
      END;
      /*END ER 19161894*/      

      --  Update the Batch Step Status to Cancel
      x_batch_step_rec.step_status := 5;

      IF (g_debug <= gme_debug.g_log_procedure) THEN
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ':'
                             || ' Calling step update_row');
      END IF;

      IF NOT (gme_batch_steps_dbl.update_row (p_batch_step      => x_batch_step_rec) ) THEN
         RAISE batch_step_upd_err;
      END IF;

      IF (g_debug <= gme_debug.g_log_procedure) THEN
         gme_debug.put_line ('Exiting ' || g_pkg_name || '.' || l_api_name);
      END IF;
   EXCEPTION
      WHEN resource_txns_gtmp_del_err THEN
         IF (g_debug <= gme_debug.g_log_procedure) THEN
            gme_debug.put_line
                         (   g_pkg_name
                          || '.'
                          || l_api_name
                          || ':'
                          || ' cancel_step, error : RESOURCE_TXNS_GTMP_DEL_ERR.');
         END IF;

         ROLLBACK TO SAVEPOINT cancel_batch_step;
      WHEN batch_step_upd_err THEN
         IF (g_debug <= gme_debug.g_log_procedure) THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ':'
                                || ' cancel_step, error : BATCH_STEP_UPD_ERR.');
         END IF;

         ROLLBACK TO SAVEPOINT cancel_batch_step;
      /*ER 19161894  Shaliu Chen 04-Dec-2014*/         
      WHEN cancel_po_req_fail THEN
        IF (g_debug <= gme_debug.g_log_procedure) THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ':'
                                || ' Cancel OSP PO/Req failed');
        END IF;
        ROLLBACK TO SAVEPOINT cancel_batch_step;
        gme_common_pvt.log_message('GME_CANCEL_PO_FAILED');
        x_return_status := fnd_api.g_ret_sts_error;            
      WHEN OTHERS THEN
         IF g_debug <= gme_debug.g_log_unexpected THEN
            gme_debug.put_line (   'When others exception in '
                                || g_pkg_name
                                || '.'
                                || l_api_name
                                || ' Error is '
                                || SQLERRM);
         END IF;

         ROLLBACK TO SAVEPOINT cancel_batch_step;
         x_return_status := fnd_api.g_ret_sts_unexp_error;
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
   END cancel_step;
END gme_cancel_step_pvt;
/

COMMIT ;
EXIT;
--show errors;
