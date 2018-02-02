/* +======================================================================+ */
/* |    Copyright (c) 2005, 2016 Oracle and/or its affiliates.           | */
/* |                         All rights reserved.                         | */
/* |                           Version 12.0.0                             | */
/* +======================================================================+ */
REM dbdrv: sql ~PROD ~PATH ~FILE none none none package &phase=plb \
REM dbdrv: checkfile(120.4.12010000.2=120.4.12020000.2):~PROD:~PATH:~FILE 
SET VERIFY OFF
WHENEVER SQLERROR EXIT FAILURE ROLLBACK;
REM **********************************************************************
REM *                                                                    *
REM * FILE:    GMEVRWSB.pls                                              *
REM * PURPOSE: Package Body for the GME UNCERTIFY BATCH STEP routines    *
REM * AUTHOR:  Pawan Kumar, OPM Development                              *
REM * DATE:    May 24th 2005                                             *
REM * HISTORY:                                                           *
REM * ========                                                           *

REM * G. Muratore    03-NOV-2016 Bug 23640627 
REM *   Introduced new p_create_resv_pend_lots parameter.
REM *   PROCEDURE: revert_step and revert_line.
REM **********************************************************************

/*************************************************************************
* This file contains the procedure for uncertifying batch steps in Oracle*
* Process Manufacturing (OPM). Each procedure has a common set of        *
* parameters to which API-specific parameters are appended.              *
*************************************************************************/

CREATE OR REPLACE PACKAGE BODY gme_revert_step_pvt AS
   /* $Header: GMEVRWSB.pls 120.4.12020000.2 2016/11/07 16:54:18 gmurator ship $ */

G_DEBUG VARCHAR2(5) := FND_PROFILE.VALUE('AFLOG_LEVEL');
g_pkg_name   CONSTANT VARCHAR2 (30) := 'gme_revert_step_pvt';  

   /*===========================================================================================
   Procedure
     revert_step
   Description
     This particular procedure call Uncertifies the batch steps.
   Parameters
     p_api_version         For version specific processing - Default 1
     p_validation_level    Errors to skip before returning - Default 100
     p_init_msg_list    Signals wether the message stack should be initialised
     p_commit        Indicator to commit the changes made
     p_batch_step       The batch step row to identify the step.
     x_message_count    The number of messages in the message stack
     x_message_list     message stack where the api writes its messages
     x_return_status    outcome of the API call
               S - Success
               E - Error
               U - Unexpected error
               N - Unallocated Items Found
   =============================================================================================*/

   PROCEDURE revert_step
   (p_batch_step_rec      	   IN  GME_BATCH_STEPS%ROWTYPE        
   ,p_batch_header_rec        IN  GME_BATCH_HEADER%ROWTYPE       
   ,p_create_resv_pend_lots   IN  NUMBER  -- Bug 23640627
   ,x_batch_step_rec          OUT NOCOPY GME_BATCH_STEPS%ROWTYPE 
   ,x_return_status           OUT NOCOPY VARCHAR2) IS
 	
      /* Buffers for database reads/writes */
      l_batch_header_rec             	gme_batch_header%ROWTYPE;
      l_in_batch_header_rec            gme_batch_header%ROWTYPE;
      x_in_batch_step_rec              gme_batch_steps%ROWTYPE;
      l_batch_step_rec			         gme_batch_steps%ROWTYPE;
      l_material_details_tab	         gme_common_pvt.material_details_tab ;
      
      /* Bug 2685645 added batch_id as parama and used in where clause */
      CURSOR cur_dep_steps (v_batchstep_id NUMBER,
                            v_batch_id  NUMBER) IS
         SELECT batchstep_no, s.step_status,d.dep_type,d.standard_delay, s.steprelease_type
         FROM   gme_batch_step_dependencies d, gme_batch_steps s
         WHERE  d.batchstep_id = s.batchstep_id AND
                d.dep_step_id = v_batchstep_id AND
                s.batch_id = v_batch_id AND
                d.batch_id = s.batch_id;
                
      CURSOR Cur_lock_step_materials (v_batchstep_id   NUMBER,
                                     v_batch_id   NUMBER) IS
         SELECT D.*
         FROM   gme_batch_step_items i, gme_material_details d
         WHERE  d.batch_id = v_batch_id AND
                i.material_detail_id = d.material_detail_id AND
                i.batchstep_id = v_batchstep_id AND
                d.release_type = 3
         FOR UPDATE OF actual_qty NOWAIT;
                         
       --          ((d.line_type = -1 AND d.phantom_id IS NOT NULL)
       --        OR (d.line_type <> -1 AND phantom_line_id IS NULL)); 
      /* Exception definitions */    
     
      invalid_dep_step_status     EXCEPTION;
      batch_revert_error       	 EXCEPTION;
      invalid_batch_status        EXCEPTION;
      BATCH_LINES_LOCKED          EXCEPTION;
      REVERT_LINE_FAIL		       EXCEPTION;
      BATCH_STEP_UPD_ERR          EXCEPTION;
      /* Local variables */
      l_return_status             VARCHAR2 (1);
      l_inv_trans_count           NUMBER;
      l_rsrc_trans_count          NUMBER;
      l_api_name         CONSTANT VARCHAR2 (30)   := 'revert_step';
      l_dep_step_rec              Cur_dep_steps%ROWTYPE;
      
   BEGIN
      /* Set the return status to success initially */
      x_return_status := FND_API.G_RET_STS_SUCCESS;
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line (g_pkg_name || '.' || l_api_name || ':'
                             || 'Entering');
      END IF;
     
      l_batch_step_rec        := p_batch_step_rec ;
      l_batch_header_rec      := p_batch_header_rec;
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line (g_pkg_name || '.' || l_api_name || ':'
                             || 'batchstep_id'||l_batch_step_rec.batchstep_id);
      END IF;
      
      /*  The Batch must be in WIP to revert a Batch Step. */
      IF (l_batch_header_rec.batch_status <> 2) THEN
         IF (  g_debug <= gme_debug.g_log_procedure) THEN
            gme_debug.put_line ('batch status'|| l_batch_header_rec.batch_status);
         END IF;
         RAISE invalid_batch_status;
      END IF;
    
      IF l_batch_header_rec.automatic_step_calculation = 1 OR
         l_batch_step_rec.steprelease_type = 2 /*automatic*/  OR
         l_batch_header_rec.enforce_step_dependency = 1 THEN
             
         FOR l_dep_step_rec IN cur_dep_steps (l_batch_step_rec.batchstep_id, l_batch_step_rec.batch_id)
         LOOP
            IF (l_dep_step_rec.dep_type = 0) THEN
               IF l_dep_step_rec.step_status > 1 THEN
                 RAISE invalid_dep_step_status;
               END IF ;/*step_status > 1*/
            END IF;  
         END LOOP; /* FOR dep_step_rec IN Cur_dep_steps */
      END IF;   /* IF ASQC is on OR release_type is automatic */
 
      -- Bug 23640627 - This next IF block will never run for completed batches
      --                because earlier check does not allow status 3 batches to get here.
      
      /* If the profile value for step control is 'Y' then if the batch status   */
      /* is completed then we have to call the revert batch API and then go      */
      /* ahead in revert the step. If the step control profile is set to         */
      /* 'N' then the batch should be in WIP status to revert the step           */
   
      IF GME_common_pvt.g_step_controls_batch_sts_ind = 1 THEN      	
         /* If the batch is certified and the batch is not a phantom */
         IF (l_batch_header_rec.batch_status = 3) AND
            (l_batch_header_rec.parentline_id IS NULL) THEN
            /* Call the revert batch API */
            IF ( g_debug <= gme_debug.g_log_procedure ) THEN
       	      gme_debug.put_line ('revert batch'||GME_common_pvt.g_step_controls_batch_sts_ind);
            END IF;
            l_in_batch_header_rec := l_batch_header_rec;
            gme_revert_batch_pvt.revert_batch
     		     (p_batch_header_rec    	=> l_in_batch_header_rec
      		  ,x_batch_header_rec    	=> l_batch_header_rec
              ,p_create_resv_pend_lots => p_create_resv_pend_lots  -- Bug 23640627
      		  ,x_return_status     	   => l_return_status);

            IF l_return_status <> x_return_status THEN            	
               RAISE batch_revert_error;
            END IF;
         END IF;   /* IF (l_batch_header_rec.batch_status = 3) */
      END IF;   /* IF GME_common_pvt.STEP_CONTROL = 'Y' */
 
      -- remove actual date from step record
      l_batch_step_rec.actual_cmplt_date := NULL;
      
      -- Update step status to WIP 
      l_batch_step_rec.step_status := 2;
      gme_common_pvt.g_batch_status_check := fnd_api.g_false;
      
      -- Update the batch step to the database 
      IF NOT (gme_batch_steps_dbl.update_row (l_batch_step_rec)) THEN
         RAISE batch_step_upd_err;
      END IF;
      
      IF ( g_debug <= gme_debug.g_log_procedure ) THEN
         gme_debug.put_line ('batchstep status'||l_batch_step_rec.step_status);
         gme_debug.put_line ('batch_id'||l_batch_step_rec.batch_id);
         gme_debug.put_line ('batchstep_id'||l_batch_step_rec.batchstep_id);          
      END IF;
      
      OPEN Cur_lock_step_materials(l_batch_step_rec.batchstep_id,l_batch_step_rec.batch_id);
      FETCH Cur_lock_step_materials BULK COLLECT INTO l_material_details_tab;
      
 	   IF sqlcode = -54 THEN
 	      CLOSE Cur_lock_step_materials;
 	      RAISE BATCH_LINES_LOCKED;
 	   END IF;
 	   CLOSE Cur_lock_step_materials;
 	   
 	   IF (g_debug <= gme_debug.g_log_procedure) THEN
          gme_debug.put_line ('l_material_details_tab count'||l_material_details_tab.count);
      END IF;
      
      FOR i IN 1..l_material_details_tab.COUNT LOOP
         IF ((l_material_details_tab(i).line_type = -1 AND l_material_details_tab(i).phantom_id IS NOT NULL) OR  
            (l_material_details_tab(i).line_type <> -1 AND l_material_details_tab(i).phantom_line_id IS NULL)) THEN
            IF (g_debug <= gme_debug.g_log_procedure ) THEN
               gme_debug.put_line ('calling revert for material line '
                                      ||l_material_details_tab(i).material_detail_id);
               gme_debug.put_line ('batch update inventory '
                                      ||l_batch_header_rec.update_inventory_ind);                       
            END IF; 
            
            gme_revert_batch_pvt.revert_line 
		         (p_batch_header_rec    	 => l_batch_header_rec
		         ,p_material_details_rec  => l_material_details_tab(i)
		         ,p_batch_step_rec        => l_batch_step_rec
               ,p_create_resv_pend_lots => p_create_resv_pend_lots  -- Bug 23640627
		         ,x_return_status   	    => l_return_status);
		         
            IF l_return_status <> x_return_status THEN
               RAISE REVERT_LINE_FAIL;
            END IF;	
         END IF;
      END LOOP;
          
      UPDATE gme_batch_step_activities
      SET    actual_cmplt_date = NULL
             ,last_updated_by     = gme_common_pvt.g_user_ident
             ,last_update_date    = gme_common_pvt.g_timestamp
             ,last_update_login   = gme_common_pvt.g_login_id
      WHERE  batchstep_id = l_batch_step_rec.batchstep_id
             AND batch_id = p_batch_header_rec.batch_id;

     
      UPDATE gme_batch_step_resources
      SET    actual_cmplt_date = NULL
             ,last_updated_by     = gme_common_pvt.g_user_ident
             ,last_update_date    = gme_common_pvt.g_timestamp
             ,last_update_login   = gme_common_pvt.g_login_id
      WHERE  batchstep_id = l_batch_step_rec.batchstep_id;
      
      
   EXCEPTION
      
      WHEN invalid_dep_step_status THEN
         x_return_status := FND_API.G_RET_STS_ERROR;
        GME_common_pvt.log_message ('GME_INV_DEP_STEP_STATUS');
     
      WHEN REVERT_LINE_FAIL THEN
         x_return_status := l_return_status;
      WHEN batch_revert_error THEN
         x_return_status := l_return_status;
      WHEN invalid_batch_status THEN
         x_return_status := FND_API.G_RET_STS_ERROR;
        GME_common_pvt.log_message ('GME_API_INV_BATCH_UNCERT_STEP');
     
      WHEN BATCH_LINES_LOCKED THEN
           x_return_status := FND_API.G_RET_STS_ERROR;  
         gme_common_pvt.log_message('GME_API_BATCH_LINES_LOCKED');  
      WHEN OTHERS THEN
        x_return_status := fnd_api.g_ret_sts_unexp_error;

         IF (NVL (g_debug, -1) = gme_debug.g_log_unexpected) THEN
            gme_debug.put_line (g_pkg_name || '.' || l_api_name || ':' || 'WHEN OTHERS:' || SQLERRM);
         END IF;
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
   END revert_step;

END gme_revert_step_pvt;
/
COMMIT;
EXIT;
--SHOW ERRORS;

