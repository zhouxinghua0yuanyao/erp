/* +======================================================================+ */
/* |    Copyright (c) 2005, 2015 Oracle and/or its affiliates.           | */
/* |                         All rights reserved.                         | */
/* |                           Version 12.0.0                             | */
/* +======================================================================+ */
REM dbdrv: sql ~PROD ~PATH ~FILE none none none package &phase=plb \
REM dbdrv: checkfile(120.5.12010000.3=120.6.12020000.2)(120.5.12000000.2=120.5.12010000.2)(115.5=120.2):~PROD:~PATH:~FILE
SET VERIFY OFF
WHENEVER SQLERROR EXIT FAILURE ROLLBACK;
WHENEVER OSERROR EXIT FAILURE ROLLBACK;

CREATE OR REPLACE PACKAGE BODY gme_terminate_batch_pvt AS
/*  $Header: GMEVTRBB.pls 120.6.12020000.2 2015/05/22 06:41:00 shalchen ship $    */
   g_debug               VARCHAR2 (5)  := fnd_profile.VALUE ('AFLOG_LEVEL');
   g_pkg_name   CONSTANT VARCHAR2 (30) := 'GME_TERMINATE_BATCH_PVT';

/*
REM *********************************************************************
REM *
REM * FILE:    GMEVTRBB.pls
REM * PURPOSE: Package Body for the GME batch terminate api
REM * AUTHOR:  Pawan Kumar
REM * DATE:    2 May 2005
REM * HISTORY:
REM * ========
REM *
REM * G. Muratore    30-Mar-2010 Bug 9478698      
REM *   Make call to GMO conditionally based on enhanced_pi_ind.
REM *   PROCEDURE terminate_batch.
REM *
REM * Shaliu Chen    22-MAY-15  Bug 201102335                        
REM *    modify validation to check whether phantom batch is on hold 
REM *    PROCEDURE:  Terminate_Batch   
REM **********************************************************************
*/

   /*================================================================================
Procedure
  Terminate_Batch
Description
  This procedure terminates the batch.
Parameters
  p_batch_header     The batch header row to identify the batch
  x_batch_header_rec     The batch header row to identify the batch
  x_return_status    outcome of the API call
            S - Success
            E - Error
            U - Unexpected error
History:   
  30-Mar-2010  G. Muratore   Bug 9478698 
     Make call to GMO conditionally based on enhanced_pi_ind.
     
  22-MAY-2015  Shaliu Chen   Bug 201102335                        
    Modify validation to check whether phantom batch is on hold      
================================================================================*/
   PROCEDURE terminate_batch (
      p_batch_header_rec   IN              gme_batch_header%ROWTYPE
     ,x_batch_header_rec   OUT NOCOPY      gme_batch_header%ROWTYPE
     ,x_return_status      OUT NOCOPY      VARCHAR2)
   IS
      /* Cursors */
      CURSOR cur_get_wip_steps (v_batch_id NUMBER)
      IS
         SELECT   *
             FROM gme_batch_steps
            WHERE batch_id = v_batch_id
              AND step_status = 2                  -- terminate only WIP steps
         ORDER BY batchstep_id;

      CURSOR cur_get_pending_steps (v_batch_id NUMBER)
      IS
         SELECT   *
             FROM gme_batch_steps
            WHERE batch_id = v_batch_id
              AND step_status = 1                      -- cancel pending steps
         ORDER BY batchstep_id;

      /* Variable Declarations */
      l_material_details               gme_material_details%ROWTYPE;
      l_material_details_tab           gme_common_pvt.material_details_tab;
      l_batch_header_rec               gme_batch_header%ROWTYPE;
      l_in_batch_header_rec            gme_batch_header%ROWTYPE;
      l_batch_step_rec                 gme_batch_steps%ROWTYPE;
      l_in_batch_step_rec              gme_batch_steps%ROWTYPE;
      l_batch_step_ids                 gme_common_pvt.number_tab;
      l_rsrc_trans_count               NUMBER;
      l_batch_step_tab                 gme_common_pvt.steps_tab;
      m_batch_step_tab                 gme_common_pvt.steps_tab;
      l_message_count                  NUMBER;                  -- 4944024
      l_message_list                   VARCHAR2 (2000);         -- 4944024
      l_api_name              CONSTANT VARCHAR2 (30)      := 'Terminate Batch';
      l_lock_status 		       VARCHAR2(1); 
      l_locked_by_status	       VARCHAR2(1); 
      l_lock_allowed 		       VARCHAR2(1);
      /* Exceptions */
      batch_header_fetch_error         EXCEPTION;
      invalid_batch_status             EXCEPTION;
      dep_material_details_fetch_err   EXCEPTION;
      error_load_trans                 EXCEPTION;
      purge_exception_err              EXCEPTION;
      batch_header_upd_err             EXCEPTION;
      phantom_batch_terminate_error    EXCEPTION;
      phantom_batch_cancel_error       EXCEPTION;
      batch_hist_insert_err            EXCEPTION;
      delete_inv_txns_err              EXCEPTION;
      batch_step_terminate_error       EXCEPTION;
      batch_step_cancel_error          EXCEPTION;
      prod_supply_resv_err             EXCEPTION;
      gmo_lock_error		       EXCEPTION;
      
      
      l_date                           DATE;
   BEGIN
      IF (g_debug <= gme_debug.g_log_procedure) THEN
         gme_debug.put_line (g_pkg_name || '.' || l_api_name || ':'
                             || 'Entering');
      END IF;

      /* Set the success staus to success inititally*/
      x_return_status := fnd_api.g_ret_sts_success;
      /*  Initialize output batch header  */
      x_batch_header_rec := p_batch_header_rec;
      -- Now Examine the ingredient material rows :
      l_material_details.batch_id := x_batch_header_rec.batch_id;

      IF (g_debug <= gme_debug.g_log_procedure) THEN
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ':'
                             || 'Call fetch_tab');
      END IF;

      IF NOT (gme_material_details_dbl.fetch_tab
                                  (p_material_detail      => l_material_details
                                  ,x_material_detail      => l_material_details_tab) ) THEN
         RAISE dep_material_details_fetch_err;
      END IF;

      /*  Load all the transactions and resources to the temporary table */
      /*  for the current batch if the update inventory ind is set for the batch  */
      IF x_batch_header_rec.update_inventory_ind = 'Y' THEN
         gme_trans_engine_util.load_rsrc_trans (x_batch_header_rec
                                               ,l_rsrc_trans_count
                                               ,x_return_status);

         IF x_return_status <> fnd_api.g_ret_sts_success THEN
            RAISE error_load_trans;
         END IF;
      END IF;           /* IF x_batch_header_rec.update_inventory_ind = 'Y' */

      IF (g_debug <= gme_debug.g_log_procedure) THEN
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ':'
                             || 'After fetch_tab tab.count='
                             || l_material_details_tab.COUNT);
      END IF;

      FOR i IN 1 .. l_material_details_tab.COUNT LOOP
         -- IF PHANTOM_ID is not null,
         --    THEN either terminate or cancel the phantom batch based on the batch status
         IF (g_debug <= gme_debug.g_log_procedure) THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ':'
                                || 'phantom_id='
                                || l_material_details_tab (i).phantom_id);
         END IF;

         IF (NVL (l_material_details_tab (i).phantom_id, 0) <> 0) THEN
            l_batch_header_rec.batch_id :=
                                        l_material_details_tab (i).phantom_id;

            IF NOT (gme_batch_header_dbl.fetch_row
                                      (p_batch_header      => l_batch_header_rec
                                      ,x_batch_header      => l_in_batch_header_rec) ) THEN
               RAISE batch_header_fetch_error;
            END IF;
            
            /* Shaliu Chen     22-MAY-2015  Bug 201102335                                                                        
               raise an error if phantom batch is on hold                                      
            */                                                                                 
            IF gme_common_pvt.get_batch_hold_status(l_in_batch_header_rec.batch_id) <> 'R' THEN
              gme_common_pvt.log_message('GME_PHANTOM_ONHOLD',                             
                                         'BATCH_NO',                                           
                                         l_in_batch_header_rec.batch_no);  
              x_return_status := fnd_api.g_ret_sts_error;                                                   
              RAISE phantom_batch_terminate_error;                                                
            END IF;                 

            IF l_in_batch_header_rec.batch_status = 2 THEN
               l_in_batch_header_rec.actual_cmplt_date :=
                                         p_batch_header_rec.actual_cmplt_date;
               -- Recursive call to private API.for terminate
               gme_terminate_batch_pvt.terminate_batch
                                (p_batch_header_rec      => l_in_batch_header_rec
                                ,x_batch_header_rec      => l_batch_header_rec
                                ,x_return_status         => x_return_status);

               IF x_return_status <> fnd_api.g_ret_sts_success THEN
                  RAISE phantom_batch_terminate_error;
               END IF;
            ELSIF l_in_batch_header_rec.batch_status = 1 THEN
               -- Call to cancel batch for the phantom
               gme_cancel_batch_pvt.cancel_batch
                                (p_batch_header_rec      => l_in_batch_header_rec
                                ,x_batch_header_rec      => l_batch_header_rec
                                ,x_return_status         => x_return_status);

               IF x_return_status <> fnd_api.g_ret_sts_success THEN
                  RAISE phantom_batch_cancel_error;
               END IF;
            END IF;                   /* l_in_batch_header_rec.batch_status */
         END IF;                                         /* phantom_id <> 0 */
      END LOOP;

      -- Delete the Inventory transaction under specific conditions
      IF x_batch_header_rec.update_inventory_ind = 'Y' THEN
         gme_cancel_batch_pvt.purge_batch_exceptions
                                   (p_batch_header_rec         => x_batch_header_rec
                                   ,p_delete_invis_mo          => 'F'
                                   ,p_delete_reservations      => 'T'
                                   ,p_delete_trans_pairs       => 'F'
                                   ,x_return_status            => x_return_status);

         IF x_return_status <> fnd_api.g_ret_sts_success THEN
            RAISE purge_exception_err;
         END IF;
      END IF;

      -- Now Examine the batch step data
      IF (g_debug <= gme_debug.g_log_procedure) THEN
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ':'
                             || 'Check routing_id/poc_ind='
                             || x_batch_header_rec.routing_id
                             || '/'
                             || x_batch_header_rec.poc_ind);
      END IF;

      IF x_batch_header_rec.poc_ind = 'Y' THEN
           /* Now update the batch step status to Terminate (5) */
         --  Get batch_step_ids bulk collected for batch
         IF (g_debug <= gme_debug.g_log_procedure) THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ':'
                                || 'After fetch_batch_steps return_status='
                                || x_return_status);
         END IF;

         OPEN cur_get_wip_steps (x_batch_header_rec.batch_id);

         FETCH cur_get_wip_steps
         BULK COLLECT INTO l_batch_step_tab;

         CLOSE cur_get_wip_steps;

         FOR i IN 1 .. l_batch_step_tab.COUNT LOOP
         	
            /* Call terminate step api to terminate the all steps */
            l_batch_step_rec := l_batch_step_tab (i);
            l_in_batch_step_rec := l_batch_step_rec;

            -- Bug 9478698 - Call GMO only if enhanced_pi_ind is activated.
            IF x_batch_header_rec.enhanced_pi_ind = 'Y' THEN
            
               -- Pawan Kumar added for bug 5034336
               -- before terminating the steps we need to check that they are locked
               gmo_vbatch_grp.GET_ENTITY_LOCK_STATUS (
             	      P_API_VERSION 		=> 1.0, 
     	   	      P_INIT_MSG_LIST 	=> FND_API.G_FALSE, 
  	   	      P_COMMIT 		=> FND_API.G_FALSE, 
  	   	      P_VALIDATION_LEVEL 	=> FND_API.G_VALID_LEVEL_FULL, 
                      X_RETURN_STATUS 	=> x_return_status , 
                      X_MSG_COUNT 		=> l_message_count, 
                      X_MSG_DATA 		=> l_message_list, 
                      P_ENTITY_NAME 		=> 'OPERATION', 
  	   	      P_ENTITY_KEY 		=> l_in_batch_step_rec.batchstep_id, 
  	   	      P_REQUESTER 		=> gme_common_pvt.g_user_ident, 
  	   	      X_LOCK_STATUS 		=> l_lock_status, 
  	   	      X_LOCKED_BY_STATUS 	=> l_locked_by_status, 
  	   	      X_LOCK_ALLOWED 		=> l_lock_allowed); 
  		
                IF x_return_status <> fnd_api.g_ret_sts_success THEN
                   RAISE  gmo_lock_error;
                END IF;
                
                IF (g_debug <= gme_debug.g_log_procedure) THEN
                  gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ':'
                                || 'from gmo the lock_status='
                                || l_lock_status);
                END IF;
                
                IF l_lock_status = 'Y' THEN
                   gme_common_pvt.log_message ('GME_STEP_LOCK_ERROR');	
                   RAISE gmo_lock_error;
                END IF;
             END IF;
            
            gme_terminate_step_pvt.terminate_step
               (p_batch_step_rec            => l_in_batch_step_rec
               ,p_update_inventory_ind      => x_batch_header_rec.update_inventory_ind
               ,p_actual_cmplt_date         => p_batch_header_rec.actual_cmplt_date
               ,x_return_status             => x_return_status
               ,x_batch_step_rec            => l_batch_step_rec);

            IF x_return_status <> fnd_api.g_ret_sts_success THEN
               RAISE batch_step_terminate_error;
            END IF;
         END LOOP;

         OPEN cur_get_pending_steps (x_batch_header_rec.batch_id);

         FETCH cur_get_pending_steps
         BULK COLLECT INTO m_batch_step_tab;

         CLOSE cur_get_pending_steps;

         FOR i IN 1 .. m_batch_step_tab.COUNT LOOP
            /* Call cancel step api to cancel steps */
            l_batch_step_rec := m_batch_step_tab (i);
            l_in_batch_step_rec := l_batch_step_rec;
            gme_cancel_step_pvt.cancel_step
               (p_batch_step_rec            => l_in_batch_step_rec
               ,p_update_inventory_ind      => x_batch_header_rec.update_inventory_ind
               ,x_return_status             => x_return_status
               ,x_batch_step_rec            => l_batch_step_rec);

            IF x_return_status <> fnd_api.g_ret_sts_success THEN
               RAISE batch_step_cancel_error;
            END IF;
         END LOOP;
      END IF;

      /* Set up the fields in output structure. */
      x_batch_header_rec.terminated_ind := 1;
      x_batch_header_rec.batch_status := 3;
      x_batch_header_rec.actual_cmplt_date :=
                                          p_batch_header_rec.actual_cmplt_date;

      /* Update the batch step to the database */
      IF (g_debug <= gme_debug.g_log_procedure) THEN
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ':'
                             || 'Call BATCH UPDATE_ROW');
      END IF;

      IF NOT (gme_batch_header_dbl.update_row (x_batch_header_rec) ) THEN
         RAISE batch_header_upd_err;
      END IF;

      /*  Re-retrieve batch header  */
      IF NOT (gme_batch_header_dbl.fetch_row
                                        (p_batch_header      => p_batch_header_rec
                                        ,x_batch_header      => x_batch_header_rec) ) THEN
         RAISE batch_header_fetch_error;
      END IF;

      -- 4944024 BEGIN    
      -- Delete any outstanding reservations against this batch as a supply source   
      IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' Invoking delete_prod_supply_resv for batch header' );
      END IF;

      l_batch_header_rec := x_batch_header_rec;
      gme_supply_res_pvt.delete_batch_prod_supply_resv (
          p_batch_header_rec     => l_batch_header_rec
         ,x_msg_count            => l_message_count
         ,x_msg_data             => l_message_list                           
         ,x_return_status        => x_return_status);                    

      IF x_return_status <> FND_API.G_RET_STS_SUCCESS THEN
        RAISE prod_supply_resv_err;
      END IF;
      -- 4944024 END   

      -- B3297712 - abort any QM sample workflows for this batch.
      abort_wf (p_type         => 'GMDQMSMC'
               ,p_item_id      => x_batch_header_rec.batch_id);

      IF x_batch_header_rec.update_inventory_ind = 'Y' THEN
         /* Insert the event into the batch history table */
         IF NOT gme_common_pvt.create_history
                                             (x_batch_header_rec
                                             ,p_batch_header_rec.batch_status) THEN
            RAISE batch_hist_insert_err;
         END IF;
      END IF;

      IF (g_debug <= gme_debug.g_log_procedure) THEN
         gme_debug.put_line (g_pkg_name || '.' || l_api_name || ':'
                             || 'Exiting');
      END IF;
   EXCEPTION
      WHEN purge_exception_err THEN
         IF (g_debug <= gme_debug.g_log_procedure) THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ':'
                                || '  purge_exception_err.');
         END IF;

         x_return_status := fnd_api.g_ret_sts_error;
      -- 4944024 BEGIN
      WHEN prod_supply_resv_err THEN
         IF (g_debug <= gme_debug.g_log_procedure) THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ':'
                                || '  delete_batch_prod_supply_resv ERROR');
         END IF;
      -- 4944024 END   
      WHEN dep_material_details_fetch_err THEN
         IF (g_debug <= gme_debug.g_log_procedure) THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ':'
                                || '  MATL_DETAILS_FETCH_ERROR.');
         END IF;

         x_return_status := fnd_api.g_ret_sts_error;
      WHEN invalid_batch_status THEN
         IF (g_debug <= gme_debug.g_log_procedure) THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ':'
                                || 'INVALID_BATCH_STATUS.');
         END IF;

         x_return_status := fnd_api.g_ret_sts_error;
         gme_common_pvt.log_message ('GME_INVALID_BSTAT_TERM');
      WHEN batch_header_upd_err THEN
         IF (g_debug <= gme_debug.g_log_procedure) THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ':'
                                || 'BATCH_HEADER_UPD_ERR.');
         END IF;

         x_return_status := fnd_api.g_ret_sts_error;
         gme_common_pvt.log_message ('GME_API_BATCH_HEADER_UPD_ERROR');
      WHEN phantom_batch_terminate_error THEN
         IF (g_debug <= gme_debug.g_log_procedure) THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ':'
                                || 'PHANTOM_BATCH_TERMINATE_ERROR.');
         END IF;
      WHEN phantom_batch_cancel_error THEN
         IF (g_debug <= gme_debug.g_log_procedure) THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ':'
                                || 'PHANTOM_BATCH_CANCEL_ERROR.');
         END IF;
      WHEN gmo_lock_error THEN
         IF (g_debug <= gme_debug.g_log_procedure) THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ':'
                                || 'GMO_LOCK_ERROR.');
         END IF;
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN batch_hist_insert_err THEN
         IF (g_debug <= gme_debug.g_log_procedure) THEN
            gme_debug.put_line ('BATCH_HIST_INSERT_ERR.');
         END IF;

         x_return_status := fnd_api.g_ret_sts_error;
      WHEN delete_inv_txns_err THEN
         IF (g_debug <= gme_debug.g_log_procedure) THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ':'
                                || 'DELETE_INV_TXNS_ERR.');
         END IF;
      WHEN batch_step_terminate_error THEN
         IF (g_debug <= gme_debug.g_log_procedure) THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ':'
                                || 'BATCH_STEP_TERMINATE_ERROR.');
         END IF;
      WHEN batch_step_cancel_error THEN
         IF (g_debug <= gme_debug.g_log_procedure) THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ':'
                                || ' BATCH_STEP_CANCEL_ERROR.');
         END IF;
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
   END terminate_batch;

    /*===============================================================================
   Function
     abort_wf
   Description
     This procedure is used to abort the workflow that is passed in.

   History
     A. Newbury B3297712 created new procedure to abort requested workflow.

   Parameters
     p_type     workflow keyword
     p_item_id           identifier of event to abort
   ==================================================================================*/
   PROCEDURE abort_wf (p_type IN VARCHAR2, p_item_id IN NUMBER)
   IS
   BEGIN
      IF (g_debug <= gme_debug.g_log_procedure) THEN
         gme_debug.put_line
                  (   'before call to wf_engine.abortprocess with batch_id='
                   || TO_CHAR (p_item_id)
                   || ' type='
                   || p_type
                  ,gme_debug.g_log_procedure
                  ,'terminate_batch');
      END IF;

      /* Cancel workflow process */
      wf_engine.abortprocess (itemtype => p_type, itemkey => p_item_id);
   EXCEPTION
      WHEN OTHERS THEN
         IF (g_debug <= gme_debug.g_log_procedure) THEN
            gme_debug.put_line
                         (   ' terminate_batch, error in abort_wf batch_id='
                          || TO_CHAR (p_item_id)
                          || ' type='
                          || p_type
                         ,gme_debug.g_log_error
                         ,'terminate_batch');
         END IF;
   END abort_wf;
END gme_terminate_batch_pvt;
/

COMMIT ;
EXIT;
--show errors;
