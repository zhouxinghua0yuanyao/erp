/* +======================================================================+ */
/* |    Copyright (c) 2005, 2016 Oracle and/or its affiliates.           | */
/* |                         All rights reserved.                         | */
/* |                           Version 12.0.0                             | */
/* +======================================================================+ */
REM dbdrv: sql ~PROD ~PATH ~FILE none none none package &phase=plb \
REM dbdrv: checkfile(120.6.12010000.4=120.7.12020000.3)(120.6.12000000.2=120.6.12010000.2)(115.23=120.1):~PROD:~PATH:~FILE
SET VERIFY OFF
WHENEVER SQLERROR EXIT FAILURE ROLLBACK;
WHENEVER OSERROR EXIT FAILURE ROLLBACK;

REM ************************************************************************
REM *                                                                      *
REM * FILE:    GMEVURSB.pls                                                *
REM * PURPOSE: Package Body for the GME Unrelease step API                 *
REM * AUTHOR:  A. Newbury                                                  *
REM * DATE:    MAY 2005                                                    *
REM *                                                                      *
REM * HISTORY:                                                             *
REM ************************************************************************

/*************************************************************************
* This file contains the procedure for unreleasing steps in Oracle       *
* Process Manufacturing (OPM). Each procedure has a common set of        *
* parameters to which API-specific parameters are appended.              *
*************************************************************************/

CREATE OR REPLACE PACKAGE BODY gme_unrelease_step_pvt AS
/* $Header: GMEVURSB.pls 120.7.12020000.3 2016/08/05 08:44:54 shalchen ship $ */
   g_debug               VARCHAR2 (5)  := fnd_profile.VALUE ('AFLOG_LEVEL');
   g_pkg_name   CONSTANT VARCHAR2 (30) := 'gme_unrelease_step_pvt';

/*=============================================================================
Procedure
  unrelease_step
Description
  This procedure call unreleases the batch step.
Parameters
  p_batch_step_rec          The batch step row to unrelease
  p_create_resv_pend_lots   Indicates whether to re-create reservations/pending product lots
  p_from_unrelease_batch    Passed as 1 from unrelease batch and 0 from gme_api_main (which means it's an unreleae step)
  x_batch_step_rec          Updated batch step record
  x_return_status           Outcome of the API call
            S - Success
            E - Error
            U - Unexpected error
History:   
  30-Mar-2010  G. Muratore   Bug 9478698 
     Make call to GMO conditionally based on enhanced_pi_ind.
=============================================================================*/
   PROCEDURE unrelease_step (
      p_batch_step_rec          IN              gme_batch_steps%ROWTYPE
     ,p_update_inventory_ind    IN              VARCHAR2
     ,p_create_resv_pend_lots   IN              NUMBER
     ,p_from_unrelease_batch    IN              NUMBER
     ,x_batch_step_rec          OUT NOCOPY      gme_batch_steps%ROWTYPE
     ,x_return_status           OUT NOCOPY      VARCHAR2)
   IS
      CURSOR cur_get_step_materials (
         v_batch_id       gme_batch_header.batch_id%TYPE
        ,v_batchstep_id   gme_batch_steps.batchstep_id%TYPE)
      IS
         SELECT d.*
           FROM gme_batch_step_items i, gme_material_details d
          WHERE d.batch_id = v_batch_id
            AND d.material_detail_id = i.material_detail_id
            AND i.batchstep_id = v_batchstep_id
            AND d.release_type = gme_common_pvt.g_mtl_autobystep_release;

      -- Bug 9478698 - Get enhanced_pi_ind from batch header.
      CURSOR cur_get_batch_info ( 
         v_batch_id       gme_batch_header.batch_id%TYPE)
      IS
         SELECT NVL(enhanced_pi_ind, 0)
           FROM gme_batch_header h
          WHERE batch_id = v_batch_id;

      l_enhanced_pi_ind 		VARCHAR2(1); 
                    
      l_api_name     CONSTANT VARCHAR2 (30)                := 'unrelease_step';
      l_material_detail_tbl   		gme_common_pvt.material_details_tab;
      l_material_detail_rec   		gme_material_details%ROWTYPE;
      l_in_batch_step_rec     		gme_batch_steps%ROWTYPE;
      l_msg_count             		NUMBER;
      l_msg_stack             		VARCHAR2 (2000);
      l_lock_status 		        VARCHAR2(1); 
      l_locked_by_status	        VARCHAR2(1); 
      l_lock_allowed 		        VARCHAR2(1);
      l_return_status                   VARCHAR2(1);
      error_update_row        EXCEPTION;
      error_unrelease_matl    EXCEPTION;
      update_step_qty_error   EXCEPTION;
      gmo_lock_error		      EXCEPTION;
      l_recalculate_count        NUMBER; 
      
      CURSOR check_batch_recalculate_flag(v_batch_id IN NUMBER) IS
        SELECT count(1)
          FROM gme_batch_genealogy
         WHERE batch_id = v_batch_id
           AND NVL(recalculate_yield,'N') <> 'Y';    
             
   BEGIN
      IF NVL (g_debug, gme_debug.g_log_procedure + 1) <=
                                                    gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ' unreleasing batchstep_id='
                             || p_batch_step_rec.batchstep_id);
      END IF;

      /* Set the return status to success initially */
      x_return_status := fnd_api.g_ret_sts_success;
      -- set output structure
      x_batch_step_rec := p_batch_step_rec;
      -- Pawan Kumar added for bug 5034336
      -- check for batch step lock status from gmo

      -- Bug 9478698 - Get enhanced_pi_ind from batch header.
      OPEN cur_get_batch_info (x_batch_step_rec.batch_id);
      FETCH cur_get_batch_info into l_enhanced_pi_ind;
      CLOSE cur_get_batch_info;

      -- Bug 9478698 - Call GMO only if enhanced_pi_ind is activated.
      IF l_enhanced_pi_ind = 'Y' THEN
         gmo_vbatch_grp.GET_ENTITY_LOCK_STATUS (
               	P_API_VERSION 		=> 1.0, 
        	P_INIT_MSG_LIST 	=> FND_API.G_FALSE, 
     		P_COMMIT 		=> FND_API.G_FALSE, 
     		P_VALIDATION_LEVEL 	=> FND_API.G_VALID_LEVEL_FULL, 
                X_RETURN_STATUS 	=> x_return_status , 
                X_MSG_COUNT 		=> l_msg_count, 
                X_MSG_DATA 		=> l_msg_stack, 
                P_ENTITY_NAME 		=> 'OPERATION', 
     		P_ENTITY_KEY 		=> x_batch_step_rec.batchstep_id, 
     		P_REQUESTER 		=> gme_common_pvt.g_user_ident, 
     		X_LOCK_STATUS 		=> l_lock_status, 
     		X_LOCKED_BY_STATUS 	=> l_locked_by_status, 
     		X_LOCK_ALLOWED 		=> l_lock_allowed); 
     		
         IF l_return_status <> fnd_api.g_ret_sts_success THEN
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
      
      -- set step status
      x_batch_step_rec.step_status := gme_common_pvt.g_step_pending;
      gme_common_pvt.g_batch_status_check := fnd_api.g_false;
      -- set actual start date to NULL...
      x_batch_step_rec.actual_start_date := NULL;

      -- reset quality status
      IF (x_batch_step_rec.quality_status <> 1) THEN
         x_batch_step_rec.quality_status := 2;
      END IF;

      -- Update the step
      IF NOT (gme_batch_steps_dbl.update_row (p_batch_step      => x_batch_step_rec) ) THEN
         RAISE error_update_row;
      END IF;

      -- Update WHO columns for output structure
      x_batch_step_rec.last_updated_by := gme_common_pvt.g_user_ident;
      x_batch_step_rec.last_update_date := gme_common_pvt.g_timestamp;
      x_batch_step_rec.last_update_login := gme_common_pvt.g_login_id;

      -- Fetch all the material lines associated to the step if not from unrelease batch
      IF p_from_unrelease_batch = 0 THEN
         OPEN cur_get_step_materials (x_batch_step_rec.batch_id
                                     ,x_batch_step_rec.batchstep_id);

         FETCH cur_get_step_materials
         BULK COLLECT INTO l_material_detail_tbl;

         CLOSE cur_get_step_materials;

         FOR i IN 1 .. l_material_detail_tbl.COUNT LOOP
            l_material_detail_rec := l_material_detail_tbl (i);
            gme_unrelease_batch_pvt.unrelease_material
                         (p_material_detail_rec        => l_material_detail_rec
                         ,p_update_inventory_ind       => p_update_inventory_ind
                         ,p_create_resv_pend_lots      => p_create_resv_pend_lots
                         ,p_from_batch                => FALSE
                         ,x_return_status              => x_return_status);

            IF x_return_status <> fnd_api.g_ret_sts_success THEN
               RAISE error_unrelease_matl;
            END IF;
         END LOOP;
      END IF;                            -- IF p_from_unrelease_batch = 0 THEN

      /* Invoke the update step qty API to update the step quantities and the */
      /* quantities of the succeeding steps                                   */
      l_in_batch_step_rec := x_batch_step_rec;
      gme_update_step_qty_pvt.update_step_qty
                                     (p_batch_step_rec      => l_in_batch_step_rec
                                     ,x_message_count       => l_msg_count
                                     ,x_message_list        => l_msg_stack
                                     ,x_return_status       => x_return_status
                                     ,x_batch_step_rec      => x_batch_step_rec);

      IF x_return_status <> fnd_api.g_ret_sts_success THEN
         RAISE update_step_qty_error;
      END IF;

      -- Remove the actual start date of the activities... set defaults; set WHO columns
      UPDATE gme_batch_step_activities
         SET actual_start_date = NULL
            ,plan_activity_factor = NVL (plan_activity_factor, 0)
            ,last_updated_by = gme_common_pvt.g_user_ident
            ,last_update_date = gme_common_pvt.g_timestamp
            ,last_update_login = gme_common_pvt.g_login_id
       WHERE batchstep_id = x_batch_step_rec.batchstep_id
         AND batch_id = x_batch_step_rec.batch_id;

      -- Remove the actual start date of the resources... set defaults; set WHO columns
      UPDATE gme_batch_step_resources
         SET actual_start_date = NULL
            ,plan_rsrc_count = NVL (plan_rsrc_count, 1)
            ,plan_rsrc_qty = NVL (plan_rsrc_qty, 0)
            ,plan_rsrc_usage = NVL (plan_rsrc_usage, 0)
            ,last_updated_by = gme_common_pvt.g_user_ident
            ,last_update_date = gme_common_pvt.g_timestamp
            ,last_update_login = gme_common_pvt.g_login_id
       WHERE batchstep_id = x_batch_step_rec.batchstep_id
         AND batch_id = x_batch_step_rec.batch_id;

      -- Update plan start date and plan completion date on activity and resources
      -- to that on the step if they are NULL; who columns set above, no need again...
      UPDATE gme_batch_step_activities
         SET plan_start_date = x_batch_step_rec.plan_start_date
       WHERE batchstep_id = x_batch_step_rec.batchstep_id
         AND batch_id = x_batch_step_rec.batch_id
         AND plan_start_date IS NULL;

      UPDATE gme_batch_step_resources
         SET plan_start_date = x_batch_step_rec.plan_start_date
       WHERE batchstep_id = x_batch_step_rec.batchstep_id
         AND batch_id = x_batch_step_rec.batch_id
         AND plan_start_date IS NULL;

      UPDATE gme_batch_step_activities
         SET plan_cmplt_date = x_batch_step_rec.plan_cmplt_date
       WHERE batchstep_id = x_batch_step_rec.batchstep_id
         AND batch_id = x_batch_step_rec.batch_id
         AND plan_cmplt_date IS NULL;

      UPDATE gme_batch_step_resources
         SET plan_cmplt_date = x_batch_step_rec.plan_cmplt_date
       WHERE batchstep_id = x_batch_step_rec.batchstep_id
         AND batch_id = x_batch_step_rec.batch_id
         AND plan_cmplt_date IS NULL;
      --BEGIN BUG 24398647 move YMM logic from main level api to private level api   
      GME_YIELD_CALCULATION_PVT.TRUNC_TRANSFER_STEPS(x_batch_step_rec.batch_id, x_batch_step_rec.BATCHSTEP_ID);
      
      GME_YIELD_CALCULATION_PVT.yield_calc(p_batch_id     => x_batch_step_rec.batch_id);
      
      OPEN check_batch_recalculate_flag(x_batch_step_rec.batch_id);
      FETCH check_batch_recalculate_flag INTO l_recalculate_count;
      IF l_recalculate_count > 0 THEN
        UPDATE gme_batch_genealogy
           SET recalculate_yield = 'Y'
         WHERE batch_id = x_batch_step_rec.batch_id;

      END IF;
      CLOSE check_batch_recalculate_flag;                
      --END BUG 24398647
      IF NVL (g_debug, gme_debug.g_log_procedure + 1) <=
                                                     gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;
   EXCEPTION
      WHEN error_update_row THEN
         gme_common_pvt.log_message ('GME_UNEXPECTED_ERROR', 'ERROR'
                                    ,SQLERRM);
         x_return_status := fnd_api.g_ret_sts_unexp_error;
      WHEN error_unrelease_matl OR update_step_qty_error THEN
         NULL;
      WHEN gmo_lock_error THEN
         IF (g_debug <= gme_debug.g_log_procedure) THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ':'
                                || 'GMO_LOCK_ERROR.');
             
         END IF;
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN OTHERS THEN
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);

         IF g_debug <= gme_debug.g_log_procedure THEN
            gme_debug.put_line (   'Unexpected error: '
                                || g_pkg_name
                                || '.'
                                || l_api_name
                                || ': '
                                || SQLERRM);
         END IF;

         x_return_status := fnd_api.g_ret_sts_unexp_error;
   END unrelease_step;
   
   PROCEDURE validate_step_for_unrelease
               (p_batch_hdr_rec        IN gme_batch_header%ROWTYPE
               ,p_batch_step_rec       IN gme_batch_steps%ROWTYPE
               ,x_return_status        OUT NOCOPY VARCHAR2) IS

      l_api_name   CONSTANT VARCHAR2 (30)           := 'validate_step_for_unrelease';

      CURSOR cur_dep_step (v_batchstep_id gme_batch_steps.batchstep_id%TYPE, v_batch_id NUMBER) IS
         SELECT 1
         FROM   gme_batch_step_dependencies d, gme_batch_steps s
         WHERE  d.batchstep_id = s.batchstep_id AND
                d.batch_id = s.batch_id AND 
                d.dep_step_id = v_batchstep_id AND
                s.batch_id = v_batch_id AND
                s.step_status <> gme_common_pvt.g_step_pending;

      l_is_dep_step_valid         NUMBER;
      
      error_batch_type            EXCEPTION;
      error_batch_status          EXCEPTION;
      error_step_status           EXCEPTION;
      error_dep_step_status       EXCEPTION;
      open_po_req_exists          EXCEPTION;  /*ER 19161894  Shaliu Chen 18-JUL-2014*/

   BEGIN
      IF NVL (g_debug, gme_debug.g_log_procedure + 1) <=
                                                    gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      IF p_batch_hdr_rec.batch_type = gme_common_pvt.g_doc_type_fpo THEN
        RAISE error_batch_type;
      END IF;
      
      IF p_batch_hdr_rec.batch_status <> gme_common_pvt.g_batch_wip THEN
        RAISE error_batch_status;
      END IF;

      IF p_batch_step_rec.step_status <> gme_common_pvt.g_step_wip THEN
        RAISE error_step_status;
      END IF;

      IF p_batch_hdr_rec.automatic_step_calculation = 1 OR
         p_batch_step_rec.steprelease_type = gme_common_pvt.g_auto_step_release OR
         p_batch_hdr_rec.enforce_step_dependency = 1  THEN
        -- return error if any immediate succeeding step is not Pending
        OPEN cur_dep_step(p_batch_step_rec.batchstep_id, p_batch_step_rec.batch_id);
        FETCH cur_dep_step INTO l_is_dep_step_valid;
        IF cur_dep_step%FOUND THEN
          CLOSE cur_dep_step;
          RAISE error_dep_step_status;
        END IF;
        CLOSE cur_dep_step;
      END IF;  -- IF p_batch_hdr_rec.automatic_step_calculation = 1 OR
      
       /*
       BEGIN ER 19161894  Shaliu Chen 18-JUL-2014               
       Added for OPM Step Level OSP Project         
       If there is open PO/Req corresponding to the batch,
       throw an error out
       */
      IF gme_osp.check_release_version THEN
        IF gme_osp.is_OSP_batch(p_batch_id     => p_batch_step_rec.batch_id,
                                p_batchstep_id => p_batch_step_rec.batchstep_id) THEN
                                
          IF gme_osp.po_req_exists(p_batch_id        => p_batch_step_rec.batch_id,
                                   p_organization_id => p_batch_hdr_rec.organization_id,
                                   p_batchstep_id    => p_batch_step_rec.batchstep_id) THEN

            RAISE open_po_req_exists;                            
          END IF;
        
        END IF;
      END IF;      
      /*END ER 19161894*/
      IF NVL (g_debug, gme_debug.g_log_procedure + 1) <=
                                                     gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;
   EXCEPTION
      WHEN error_step_status THEN
        gme_common_pvt.log_message('GME_API_INV_STEP_STAT_UNRELE');
        x_return_status := fnd_api.g_ret_sts_error;
      WHEN error_batch_type OR error_batch_status THEN
        gme_common_pvt.log_message('GME_API_INV_BATCH_UNRELE_STEP');
        x_return_status := fnd_api.g_ret_sts_error;
      WHEN error_dep_step_status THEN
        gme_common_pvt.log_message ('GME_SUCC_DEP_PENDING');
        x_return_status := FND_API.G_RET_STS_ERROR;
      /*ER 19161894  Shaliu Chen 18-JUL-2014*/  
      WHEN open_po_req_exists THEN
        gme_common_pvt.log_message('GME_OPEN_PO_EXISTS');
        x_return_status := fnd_api.g_ret_sts_error;         
      WHEN OTHERS THEN
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);

         IF g_debug <= gme_debug.g_log_procedure THEN
            gme_debug.put_line (   'Unexpected error: '
                                || g_pkg_name
                                || '.'
                                || l_api_name
                                || ': '
                                || SQLERRM);
         END IF;

         x_return_status := fnd_api.g_ret_sts_unexp_error;
   END validate_step_for_unrelease;

END gme_unrelease_step_pvt;
/

COMMIT ;
EXIT;
--show errors;
