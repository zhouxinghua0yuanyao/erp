/* +======================================================================+ */
/* |    Copyright (c) 2005, 2015 Oracle and/or its affiliates.           | */
/* |                         All rights reserved.                         | */
/* |                           Version 12.0.0                             | */
/* +======================================================================+ */
REM dbdrv: sql ~PROD ~PATH ~FILE none none none package &phase=plb \
REM dbdrv: checkfile(120.4.12010000.4=120.5.12020000.3)(120.3.12000000.3=120.4.12010000.2)(115.29=120.1):~PROD:~PATH:~FILE
SET VERIFY OFF
WHENEVER SQLERROR EXIT FAILURE ROLLBACK;
WHENEVER OSERROR EXIT FAILURE ROLLBACK;

CREATE OR REPLACE PACKAGE BODY gme_reopen_batch_pvt AS
/*  $Header: GMEVROBB.pls 120.5.12020000.3 2015/12/02 14:06:30 gmurator ship $    */
   g_debug                VARCHAR2 (5)  := fnd_profile.VALUE ('AFLOG_LEVEL');
    g_pkg_name   CONSTANT VARCHAR2 (30) := 'GME_REOPEN_BATCH_PVT';
   
/*
REM **********************************************************************
REM *                                                                    *
REM * FILE:    GMEVROBB.pls                                              *
REM * PURPOSE: Package Body for the GME batch reopen api                 *
REM * AUTHOR:  Navin Sinha, OPM Development                              *
REM * DATE:    May 19 2005                                               *
REM *                                                                    *
REM * PROCEDURE reopen_batch                                             *
REM * FUNCTION  is_batch_posted                                          *
REM * FUNCTION  is_period_open                                           *
REM * FUNCTION  create_history                                           *
REM *                                                                    *
REM *                                                                    *
REM * HISTORY:                                                           *
REM * ========                                                           *
REM *  Pawan Kumar Corrected the code.                                   *
REM *                                                                    *
REM * 21-JAN-11   G. Muratore    Bug 10634518                            *
REM *   Frontport 11i bug 6853392. Rework using R12 tables and joins per *
REM *   GMF team.  Add conditions on table gmf_period_statuses to see if *
REM *   batch_close_date is in open period.  PROCEDURE: is_period_open   *

REM * 12-MAR-2015 G. Muratore    Bug 19788419
REM *   Reopen invisible move order line records which are closed when the batch is closed.
REM *   PROCEDURE:   reopen_batch

REM * 01-DEC-2015 G. Muratore    Bug 22240879
REM *   Change create history call to pass in correct original status value.
REM *   PROCEDURE:   reopen_batch
REM **********************************************************************
*/
   g_package_name   CONSTANT VARCHAR2 (30) := 'GME_REOPEN_BATCH_PVT';

/*================================================================================
Procedure
  reopen_batch
Description
  This particular procedure call reopen the batch.
Parameters
  p_batch_header_rec    The batch header row to identify the batch
  p_validation_level    Errors to skip before returning - Default 100
  x_batch_header_rec    The batch header row to identify the batch
  x_message_count    The number of messages in the message stack
  x_message_list     message stack where the api writes its messages
  x_return_status    outcome of the API call
            S - Success
            E - Error
            U - Unexpected error
================================================================================*/
   PROCEDURE reopen_batch (
      p_batch_header_rec   IN              gme_batch_header%ROWTYPE
     ,p_reopen_steps       IN              VARCHAR2 := 'F'
     ,x_batch_header_rec   OUT NOCOPY      gme_batch_header%ROWTYPE
     ,x_return_status      OUT NOCOPY      VARCHAR2)
   IS
      /* Cursor to get all the phantom batches except ones
       which are release type of "Automatic by Step" and attached to steps */
      CURSOR cur_matl_phant_ids (v_batch_id NUMBER)
      IS
         SELECT d.phantom_id
           FROM gme_material_details d
          WHERE d.batch_id = v_batch_id
            AND NVL (d.phantom_id, 0) > 0
            AND NOT EXISTS (
                   SELECT 1
                     FROM gme_batch_step_items
                    WHERE material_detail_id = d.material_detail_id
                      AND d.release_type = 3);

      l_api_name              CONSTANT VARCHAR2 (30)         := 'REOPEN_BATCH';
      /* Miscellaneous */
      l_batch_status                   NUMBER;
      l_batch_close_date               DATE;
      l_back_flush                     NUMBER;
      l_error_count                    NUMBER;
      l_row_count                      NUMBER;
      l_ins_history                    gme_batch_history%ROWTYPE;
      l_material_details               gme_material_details%ROWTYPE;
      l_material_details_tab           gme_reopen_batch_pvt.material_details_tab;
      l_phantom_ids                    gme_common_pvt.number_tab;
      l_batch_header                   gme_batch_header%ROWTYPE;
      l_in_batch_header                gme_batch_header%ROWTYPE;
      l_dummy                          NUMBER;
      l_message_count                  NUMBER;
      l_message_list                   VARCHAR2 (2000);
      /*  Added local variables */
      l_mat_cnt                        NUMBER;
      l_rsrc_cnt                       NUMBER;
      l_status                         VARCHAR2 (1);
      l_return_status                  VARCHAR2 (1);
      l_msg_count                      NUMBER;
      l_msg_data                       VARCHAR2 (100);
      l_batch_cost                     BOOLEAN                         := TRUE;
      batch_cost_err                   EXCEPTION;
     -- batch_header_locked_err          EXCEPTION;
      batch_header_fetch_err           EXCEPTION;
      invalid_batch_type               EXCEPTION;
      invalid_batch_status             EXCEPTION;
     -- batch_already_purged             EXCEPTION;
      batch_already_posted             EXCEPTION;
      batch_costed_and_period_closed   EXCEPTION;
     -- marked_for_deletion              EXCEPTION;
      batch_step_reopen_err            EXCEPTION;
      batch_header_upd_err             EXCEPTION;
      batch_hist_insert_err            EXCEPTION;
      phantom_batch_reopen_err         EXCEPTION;
      cant_reopen_migrated_batch       EXCEPTION;

    -- Bug 5903208
    gmf_cost_failure         EXCEPTION;

   BEGIN
      /* Set the return staus to success inititally*/
      x_return_status := fnd_api.g_ret_sts_success;
      
      /*  Initialize output batch header  */
      x_batch_header_rec := p_batch_header_rec ;
      /* x_batch_header_rec gets the batch_id */
   
      --  Validate batch status, report error if batch status is not Closed (4)
      -- To reopen a batch, it has to be :
      --           * not migrated     => migrated_batch_ind <> 'Y'
      --           * a batch          => batch_type = 0
      --           * closed           => batch_status = 4
      --           * not posted to GL => from GME_INVENTORY_TXNS_GTMP via material_detail_id
      --           * not posted to GL => or directly from IC_TRAN_PND via batch_id
      --           * not costed       => actual_cost_ind = N

      -- STEP-1: done at Public level.
      IF (x_batch_header_rec.migrated_batch_ind = 'Y') THEN
         RAISE cant_reopen_migrated_batch;
      END IF;

      -- STEP-2a: Validate Batch Type
      IF (x_batch_header_rec.batch_type <> 0) THEN
         RAISE invalid_batch_type;
      END IF;

      -- STEP-2b: Validate Batch Status
      IF (x_batch_header_rec.batch_status <> 4) THEN
         RAISE invalid_batch_status;
      END IF;

/* Navin: need to uncomment.
    Asked Srikanth to clarify the new logic of Is_batch_purged

  IF x_batch_header_rec.update_inventory_ind = 'Y'
  THEN
    -- STEP-3: Check that transaction are not purged
    IF gme_api_grp_pk2.Is_batch_purged(p_batch_id => x_batch_header_rec.batch_id) = TRUE
    THEN
       RAISE BATCH_ALREADY_PURGED;
    END IF;
  END IF;
*/-- STEP-4: Validate GL Posted Indicator
  /* Since we moved the gl_posted_ind from history to header
     We should not call lines below */
  
      IF x_batch_header_rec.gl_posted_ind = 1 THEN
         RAISE batch_already_posted;
      END IF;

      -- STEP-5a: Validate the Actual Cost Indicator (1/2)
      IF (NVL (x_batch_header_rec.actual_cost_ind, 'N') = 'Y') THEN
         -- Return a WARNING if the period is still open.
         -- Return an ERROR if the period is already closed

         -- STEP-5b: Validate the Actual Cost Indicator (2/2)
         IF (is_period_open (p_batch_id => x_batch_header_rec.batch_id) ) THEN
            gme_common_pvt.log_message ('GME_API_ACTUAL_COST_DONE_ERROR');
         ELSE
            RAISE batch_costed_and_period_closed;
         END IF;

         l_batch_cost :=
            gmf_cmcommon.is_batch_cost_frozen
                                    (p_api_version        => 2
                                    ,p_init_msg_list      => fnd_api.g_false
                                    ,p_commit             => fnd_api.g_false
                                    ,x_return_status      => l_return_status
                                    ,x_msg_count          => l_msg_count
                                    ,x_msg_data           => l_msg_data
                                    ,p_batch_id           => p_batch_header_rec.batch_id);

         IF l_batch_cost = FALSE THEN
            RAISE batch_cost_err;
         END IF;
      END IF;

      /* Load transactions in temporary table
         so that these can be displayed in the E-record */
      gme_trans_engine_util.load_rsrc_trans
                                           (p_batch_row          => x_batch_header_rec
                                           ,x_rsc_row_count      => l_rsrc_cnt
                                           ,x_return_status      => l_status);

      IF (x_return_status <> fnd_api.g_ret_sts_success) THEN
         RAISE fnd_api.g_exc_error;
      END IF;

      /* Now we have to reopen any existing phantoms which are not
         associated step and release type of "Automatic by step" */
      OPEN cur_matl_phant_ids (x_batch_header_rec.batch_id);

      FETCH cur_matl_phant_ids
      BULK COLLECT INTO l_phantom_ids;

      CLOSE cur_matl_phant_ids;

      FOR i IN 1 .. l_phantom_ids.COUNT LOOP
         l_batch_header.batch_id := l_phantom_ids (i);

         /*  Initialize batch header*/
         IF NOT (gme_batch_header_dbl.fetch_row (l_batch_header
                                                ,l_in_batch_header) ) THEN
            RAISE batch_header_fetch_err;
         END IF;

         -- B3184949 Only reopen closed batches (can have cancelled batches when parent batch is terminated
         IF l_in_batch_header.batch_status = 4 THEN
            gme_reopen_batch_pvt.reopen_batch
                                    (p_batch_header_rec      => l_in_batch_header
                                    ,p_reopen_steps          => p_reopen_steps
                                    ,x_batch_header_rec      => l_batch_header
                                    ,x_return_status         => x_return_status);

            IF (x_return_status <> fnd_api.g_ret_sts_success) THEN
               RAISE phantom_batch_reopen_err;
            END IF;
         END IF;                                     /* if batch_status = 4 */
      END LOOP;

      -- Reopen steps only if passed parameter is TRUE
      IF p_reopen_steps = 'T' THEN
         -- Now Examine the batch step(POC data) :
         IF (x_batch_header_rec.poc_ind = 'Y') THEN
            /* Call reopen step api to reopen the all steps */
            gme_reopen_step_pvt.reopen_all_steps
                                   (p_batch_header_rec      => x_batch_header_rec
                                   ,x_return_status         => x_return_status);

            IF (x_return_status <> fnd_api.g_ret_sts_success) THEN
               RAISE batch_step_reopen_err;
            END IF;
         END IF;                       --delete only if we have steps in batch
      END IF;               -- reopen steps only if the parameter indicates so

      -- STEP-6: Remove the batch close date from the batch header
      -- STEP-7: Update the batch header status
      -- STEP-8: Update the actual cost ind to 'N'
      -- Set up the fields in output structure.
      l_batch_close_date := x_batch_header_rec.batch_close_date;
      x_batch_header_rec.batch_status := 3;
      x_batch_header_rec.batch_close_date := NULL;
      x_batch_header_rec.actual_cost_ind := 'N';

      -- STEP-8: Update the batch step to the database
      
      IF NOT (gme_batch_header_dbl.update_row (x_batch_header_rec) ) THEN
         x_batch_header_rec.batch_status := 4;
         x_batch_header_rec.batch_close_date := l_batch_close_date;
         RAISE batch_header_upd_err;
      END IF;

      -- STEP-9: Insert the event into the batch history table
      IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
         gme_debug.put_line (g_pkg_name
                                || '.'
                                || l_api_name
                                || ':'
                                || 'Create history'                        
                            );
      END IF;

      IF x_batch_header_rec.update_inventory_ind = 'Y' THEN
      	 /* Insert the event into the batch history table */
      	 -- Bug 22240879 - change create history call to use explicit  
      	 -- parameter passing and also pass in correct original status.
/*      	 
         IF NOT gme_common_pvt.create_history
                                             (x_batch_header_rec
                                             ,x_batch_header_rec.batch_status) THEN
*/                                             
         IF NOT gme_common_pvt.create_history
                                    (p_batch_header_rec      => x_batch_header_rec
                                    ,p_original_status       => gme_common_pvt.g_batch_closed) THEN
            RAISE batch_hist_insert_err;
         END IF;
      END IF;
         /*IF NOT create_history (x_batch_header_rec) THEN
            x_batch_header_rec.batch_status := 4;
            x_batch_header_rec.batch_close_date := l_batch_close_date;
            RAISE batch_hist_insert_err;
         END IF;
      END IF; */

      /* Update the row who columns */
      x_batch_header_rec.last_update_date := gme_common_pvt.g_timestamp;
      x_batch_header_rec.last_updated_by := gme_common_pvt.g_user_ident;
      x_batch_header_rec.last_update_login := gme_common_pvt.g_login_id;


      -- 
      -- Bug 5903208 - Make call to GMF to revert finalization layers for this batch
      --
      GMF_VIB.Revert_Finalization
      ( p_api_version   =>    1.0,
        p_init_msg_list =>    FND_API.G_FALSE,
        p_batch_id      =>    x_batch_header_rec.batch_id,
        x_return_status =>    x_return_status,
        x_msg_count     =>    l_message_count,
        x_msg_data      =>    l_message_list);
      
      IF x_return_status <> FND_API.G_RET_STS_SUCCESS
      THEN
         RAISE gmf_cost_failure;
      END IF;
      -- End Bug 5903208 

      -- BUG 19788419 - Let's reopen the invisible MO lines.
      UPDATE MTL_TXN_REQUEST_Lines 
      SET line_status = 7 
      WHERE line_id in 
        (SELECT d.move_order_line_id 
         FROM gme_material_details d
         WHERE d.batch_id = x_batch_header_rec.batch_id
           AND d.line_type = gme_common_pvt.g_line_type_ing);                                     
  
      -- STEP-10: End of the process with S(uccess) or W(arning)
      IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
        gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ':'
                                || 'NOrmal end to reopen');
       END IF;
   EXCEPTION
      WHEN   gmf_cost_failure THEN
        -- Bug 5043868
        x_return_status := FND_API.G_RET_STS_ERROR;
    
      WHEN batch_cost_err THEN
         x_return_status := fnd_api.g_ret_sts_error;
     /* WHEN batch_header_locked_err THEN
         x_return_status := fnd_api.g_ret_sts_error;
         gme_common_pvt.log_message ('GME_BATCH_IN_USE');*/
      WHEN batch_header_fetch_err OR fnd_api.g_exc_error THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN cant_reopen_migrated_batch THEN
         x_return_status := fnd_api.g_ret_sts_error;
         gme_common_pvt.log_message ('GME_API_MIG_BATCH_FOR_REOPEN');
      WHEN invalid_batch_type THEN
         x_return_status := fnd_api.g_ret_sts_error;
         gme_common_pvt.log_message ('GME_API_INV_BATCH_TYPE');
      WHEN invalid_batch_status THEN
         x_return_status := fnd_api.g_ret_sts_error;
         gme_common_pvt.log_message ('GME_INVALID_BATCH_STATUS'
                                    ,'PROCESS'
                                    ,'Reopen');
     /* WHEN batch_already_purged THEN
         x_return_status := fnd_api.g_ret_sts_error;
         gme_common_pvt.log_message ('GME_API_TRANSACTIONS_PURGED');*/
      WHEN batch_already_posted THEN
         x_return_status := fnd_api.g_ret_sts_error;
         gme_common_pvt.log_message ('GME_API_GL_POSTED');
      WHEN batch_costed_and_period_closed THEN
         x_return_status := fnd_api.g_ret_sts_error;
         gme_common_pvt.log_message ('GME_API_COST_PERIOD_CLOSED');
     /* WHEN marked_for_deletion THEN
         x_return_status := fnd_api.g_ret_sts_error;
         gme_common_pvt.log_message ('GME_API_MARKED_FOR_DELETION');*/
      WHEN batch_step_reopen_err THEN
         x_return_status := fnd_api.g_ret_sts_error;
         gme_common_pvt.log_message ('GME_API_BATCH_STEP_REOPEN_ERR');
      WHEN batch_header_upd_err THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN batch_hist_insert_err THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN phantom_batch_reopen_err THEN
         NULL;
      WHEN OTHERS THEN
         x_return_status := fnd_api.g_ret_sts_unexp_error;
         fnd_msg_pub.add_exc_msg (g_package_name, l_api_name);
   END reopen_batch;

/*===============================================================================
Function
  is_batch_posted
Description
  This function return TRUE is one record is posted
   for the batch_id or material_detail_id

Parameters
  p_batch_id      Batch header id
  p_material_detail_id  material_detail_id
==================================================================================*/
   FUNCTION is_batch_posted (
      p_batch_id             IN   NUMBER DEFAULT NULL
     ,p_material_detail_id   IN   NUMBER DEFAULT NULL)
      RETURN BOOLEAN
   IS
      /* Local variable definitions */
      l_is_posted            NUMBER;
      l_api_name    CONSTANT VARCHAR2 (30) := 'is_batch_posted';
      batch_already_posted   EXCEPTION;
      others_error           EXCEPTION;

      CURSOR c_posted_in_history (l_batch_id IN NUMBER)
      IS
         SELECT gl_posted_ind
           FROM gme_batch_header
          WHERE gl_posted_ind > 1
            AND batch_status = 4
            AND batch_id = l_batch_id;
   BEGIN
      IF (p_batch_id IS NOT NULL) THEN
         OPEN c_posted_in_history (p_batch_id);

         FETCH c_posted_in_history
          INTO l_is_posted;

         IF (c_posted_in_history%FOUND) THEN
            CLOSE c_posted_in_history;

            RAISE batch_already_posted;
         END IF;

         CLOSE c_posted_in_history;
      ELSE
         RAISE others_error;
      END IF;

      RETURN FALSE;
   EXCEPTION
      WHEN batch_already_posted THEN
         RETURN TRUE;
      WHEN others_error THEN
         RETURN TRUE;
      WHEN OTHERS THEN
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
         RETURN FALSE;
   END is_batch_posted;

/*===============================================================================
Function
  is_period_open
Description
  This function return TRUE if the relevant period is opened

Parameters
  p_batch_id            Batch ID of the batch to be reopened.
==================================================================================*/
   FUNCTION is_period_open (p_batch_id IN NUMBER)
      RETURN BOOLEAN
   IS
      l_cnt_frozen_matls    NUMBER        := 0;
      l_api_name   CONSTANT VARCHAR2 (30) := 'is_period_open';
      period_not_found      EXCEPTION;
      period_not_open       EXCEPTION;

/*
      CURSOR c_get_period_info (l_batch_id IN gme_batch_header.batch_id%TYPE)
      IS
         -- See if any of the matl lines has been costed and frozen.
         SELECT COUNT (1)
           FROM cm_cmpt_dtl cst
               ,cm_acst_led aled
               ,gme_material_details md
               ,gme_batch_header bh
          WHERE bh.batch_id = l_batch_id
            AND bh.batch_id = md.batch_id
            AND md.material_detail_id = aled.transline_id
            AND aled.source_ind = 0
            AND aled.cmpntcost_id = cst.cmpntcost_id
            AND cst.rollover_ind = 1;
*/

      -- Bug 10634518 - Frontport 11i bug 6853392 but also rework using R12 tables and joins per GMF team.
      CURSOR c_get_period_info (l_batch_id IN gme_batch_header.batch_id%TYPE)
      IS
          SELECT COUNT(1)
          FROM cm_cmpt_dtl cst, cm_acst_led aled, gme_material_details md, gme_batch_header bh, gmf_period_statuses d
          WHERE bh.batch_id = l_batch_id
          AND bh.batch_id = md.batch_id
          AND md.material_detail_id = aled.transline_id
          AND aled.source_ind = 0
          AND aled.cmpntcost_id = cst.cmpntcost_id
          AND aled.period_id = d.period_id
          AND bh.batch_close_date >= d.start_date
          AND bh.batch_close_date <= d.end_date
          AND cst.rollover_ind = 1;             
   BEGIN
      OPEN c_get_period_info (p_batch_id);

      FETCH c_get_period_info
       INTO l_cnt_frozen_matls;

      IF (c_get_period_info%NOTFOUND) THEN
         CLOSE c_get_period_info;

         RAISE period_not_found;
      END IF;

      CLOSE c_get_period_info;

      IF (l_cnt_frozen_matls > 0) THEN
         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
         	gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ':'
                                ||'Period not open');
         END IF;

         RAISE period_not_open;
      END IF;

      IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
         gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ':'
                   || 'period Open, so Warning if the batch has been costed'  );
      END IF;

      gme_common_pvt.log_message ('GME_API_COST_PERIOD_OPEN');
      RETURN TRUE;
   EXCEPTION
      WHEN period_not_found THEN
         RETURN FALSE;
      WHEN period_not_open THEN
         RETURN FALSE;
      WHEN OTHERS THEN
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
         RETURN FALSE;
   END is_period_open;
   
END gme_reopen_batch_pvt;
/

-- show errors;
COMMIT ;
EXIT;
