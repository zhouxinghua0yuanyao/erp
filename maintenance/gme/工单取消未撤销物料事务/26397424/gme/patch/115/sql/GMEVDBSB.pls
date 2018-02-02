/* +======================================================================+ */
/* |    Copyright (c) 2005, 2015 Oracle and/or its affiliates.           | */
/* |                         All rights reserved.                         | */
/* |                           Version 12.0.0                             | */
/* +======================================================================+ */
REM dbdrv: sql ~PROD ~PATH ~FILE none none none package &phase=plb \
REM dbdrv: checkfile(120.3.12010000.6=120.3.12020000.6)(120.2.12000000.2=120.3)(115.23=120.1):~PROD:~PATH:~FILE
SET VERIFY OFF;
WHENEVER SQLERROR EXIT FAILURE ROLLBACK;
WHENEVER OSERROR EXIT FAILURE ROLLBACK;
REM **********************************************************************
REM *                                                                    *
REM * FILE:    GMEVDBSB.pls                                              *
REM * PURPOSE: Package Body for the GME DELETE BATCH STEP routines       *
REM * AUTHOR:  Thomas Daniel, OPM Development                            *
REM * DATE:    May 04th 2001                                             *
REM * HISTORY:                                                           *
REM * ========                                                           *
REM **********************************************************************

/*************************************************************************
* This file contains the procedure for delete batch step in Oracle       *
* Process Manufacturing (OPM). Each procedure has a common set of        *
* parameters to which API-specific parameters are appended.              *
*************************************************************************/

CREATE OR REPLACE PACKAGE BODY gme_delete_batch_step_pvt AS
/* $Header: GMEVDBSB.pls 120.3.12020000.6 2015/05/25 07:19:45 shalchen ship $ */
   g_debug               VARCHAR2 (5)  := fnd_profile.VALUE ('AFLOG_LEVEL');
   g_pkg_name   CONSTANT VARCHAR2 (30) := 'gme_delete_batch_step_pvt';

/*===========================================================================================
Procedure
  delete_step
Description
  This particular procedure is used to delete the step and its details in a batch.
Parameters
  p_api_version                 For version specific processing - Default 1
  p_validation_level            Errors to skip before returning - Default 100
  p_init_msg_list               Signals wether the message stack should be initialised
  p_commit                      Indicator to commit the changes made
  p_batch_step                  The batch step record.
  x_message_count               The number of messages in the message stack
  x_message_list                message stack where the api writes its messages
  x_return_status               outcome of the API call
                                S - Success
                                E - Error
                                U - Unexpected error
  18-JUL-2014     Shaliu Chen       ER 19161894                                                 
     Modify create_batch to invoke requisition creation program if batch include OSP step
     
  05-MAY-2015    Shaliu Chen       ER 20938455
     Modify for Batch On Hold enhancement,add validation to raise an error 
     if batch is on hold                                        
=============================================================================================*/
   PROCEDURE delete_step (
      p_batch_step_rec   IN              gme_batch_steps%ROWTYPE
     ,x_return_status    OUT NOCOPY      VARCHAR2
     ,p_reroute_flag     IN              BOOLEAN := FALSE)
   IS
      l_api_name            CONSTANT VARCHAR2 (30)           := 'delete_step';
      /* Collections for details etc */
      l_gme_batchstep_activities     gme_batch_step_activities%ROWTYPE;
      l_material_ids                 gme_common_pvt.number_tab;
      l_batch_header                 gme_batch_header%ROWTYPE;
      l_batch_step                   gme_batch_steps%ROWTYPE;
      l_batchstep_dependency         gme_batch_step_dependencies%ROWTYPE;
      l_batchstep_items              gme_batch_step_items%ROWTYPE;
      /* Local variables */
      l_return_status                VARCHAR2 (1);
      l_inv_trans_count              NUMBER;
      l_rsrc_trans_count             NUMBER;
      l_count                        NUMBER;
      l_step_tbl                     gmd_auto_step_calc.step_rec_tbl;
      l_msg_count                    NUMBER;
      l_msg_stack                    VARCHAR2 (2000);
      /*ER 19161894  Shaliu Chen 18-JUL-2014*/
      l_propagate_change_to_po       NUMBER;
      l_osp_resource_flag            NUMBER;      
      /*END ER 19161894*/
      CURSOR cur_get_count (v_batch_id NUMBER, v_batchstep_id NUMBER)
      IS
         SELECT COUNT (1)
           FROM gme_batch_steps
          WHERE batch_id = v_batch_id AND batchstep_id <> v_batchstep_id;

      CURSOR cur_get_dependency (v_batchstep_id NUMBER)
      IS
         SELECT batchstep_id, dep_step_id
           FROM gme_batch_step_dependencies
          WHERE (batch_id = l_batch_header.batch_id)
            AND (batchstep_id = v_batchstep_id OR dep_step_id = v_batchstep_id);

      CURSOR cur_get_item_assoc (v_batchstep_id NUMBER)
      IS
         SELECT material_detail_id
           FROM gme_batch_step_items
          WHERE batchstep_id = v_batchstep_id;
          
      /*ER 19161894  Shaliu Chen 18-JUL-2014*/    
      CURSOR cur_get_osp_parameter(v_organization_id NUMBER)
      IS
         SELECT propagate_change_to_po
           FROM gme_parameters
          WHERE organization_id = v_organization_id;            

      /* Exceptions */
      step_activity_delete_error     EXCEPTION;
      invalid_step                   EXCEPTION;
      error_load_trans               EXCEPTION;
      batch_step_fetch_error         EXCEPTION;
      batch_header_fetch_error       EXCEPTION;
      step_dependency_delete_error   EXCEPTION;
      step_item_delete_error         EXCEPTION;
      step_delete_error              EXCEPTION;
      one_step_reqd                  EXCEPTION;
      error_calc_step_qty            EXCEPTION;
      cancel_po_req_fail             EXCEPTION;  /*ER 19161894  Shaliu Chen 18-JUL-2014*/
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      /* Set the savepoint before proceeding */
      SAVEPOINT delete_batch_step;
      /* Set the return status to success initially */
      x_return_status := fnd_api.g_ret_sts_success;

      /* Initialize local batch step array */
      IF NOT (gme_batch_steps_dbl.fetch_row (p_batch_step_rec, l_batch_step) ) THEN
         RAISE batch_step_fetch_error;
      END IF;

      /* Load the batch header associated with the step */
      l_batch_header.batch_id := l_batch_step.batch_id;

      IF NOT (gme_batch_header_dbl.fetch_row (l_batch_header, l_batch_header) ) THEN
         RAISE batch_header_fetch_error;
      END IF;
      
      /* Shaliu Chen     05-MAY-2015  ER 20938455
         raise an error if batch is on hold
      */
      IF gme_common_pvt.get_batch_hold_status(l_batch_header.batch_id) = 'S' THEN
        gme_common_pvt.log_message('GME_BATCH_ONHOLD_CHECK',
                                   'ACTION_NAME',
                                   'Delete Step');        
        RAISE fnd_api.g_exc_error;        
      END IF;        

      /*  Don't allow the Batch Step to be deleted if the Batch Status  */
      /*  is Cancelled or Closed or Certified  or WIP                   */
      IF (l_batch_step.step_status <> 1) THEN
         RAISE invalid_step;
      END IF;

      /* Don't allow all the steps in the batch to be deleted */
      /*  Load all the transactions and resources to the temporary table */
      /*  for the current batch if the update inventory ind is set for the batch  */
      IF l_batch_header.update_inventory_ind = 'Y' THEN
         gme_trans_engine_util.load_rsrc_trans (p_batch_row     => l_batch_header
                                               ,x_rsc_row_count => l_rsrc_trans_count
                                               ,x_return_status => l_return_status);

         IF l_return_status <> x_return_status THEN
            RAISE error_load_trans;
         END IF;
      END IF;               /* IF x_batch_header.update_inventory_ind = 'Y' */
      
      /*                                    
      BEGIN ER 19161894  Shaliu Chen 18-JUL-2014               
      Added for OPM Step Level OSP Project  
      If step is OSP step,invoke cancelreqpo program to 
      cancel corresponding PO/Requisition
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
            Check whether the batch include Outside resource.
           */
          /*
          BEGIN ER 20809749
          Shaliu Chen 17-APR-2015
          Add condition batchstep_id to support multiple osp steps.
          */           
          SELECT count(1)
            INTO l_osp_resource_flag
            FROM gme_batch_step_resources gbsr
           WHERE batch_id = l_batch_header.batch_id
             AND batchstep_id = l_batch_step.batchstep_id
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
            OPEN cur_get_osp_parameter(l_batch_header.organization_id);
            FETCH cur_get_osp_parameter INTO l_propagate_change_to_po;
            CLOSE cur_get_osp_parameter;
            /*
             Cancel Open PO linked to the batch if propagate_change_to_po
             parameter is Automatic
            */
            IF (NVL(l_propagate_change_to_po,gme_osp.g_propagate_change_manual) 
                       = gme_osp.g_propagate_change_automatic) THEN                
              /*
                Invoke Batch product quantity change to the PO/Req
                linked to the batch.
              */         
              gme_osp.cancelPOReq (p_batch_id      => l_batch_step.batch_id,
                                   p_org_id        => l_batch_header.organization_id,
                                   p_batchstep_id  => l_batch_step.batchstep_id,
                                   x_return_status => l_return_status,
                                   x_message_list  => l_msg_stack,
                                   x_message_count => l_msg_count
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
          l_msg_stack := 'unexpected error: ' || SQLERRM || 'SQLCODE = ' || SQLCODE;
          gme_debug.put_line('l_errMsg:'||l_msg_stack);
          RAISE cancel_po_req_fail;          
      END;         
      /*END ER 19161894*/
      
      /* Delete all the activities attached to the step and the subsequent child nodes */
      IF NOT p_reroute_flag THEN
         OPEN cur_get_count (l_batch_step.batch_id
                            ,l_batch_step.batchstep_id);

         FETCH cur_get_count
          INTO l_count;

         CLOSE cur_get_count;

         IF l_count = 0 THEN
            RAISE one_step_reqd;
         END IF;
      END IF;

      -- Navin: START Changed the logic  --
      -- 1. First delete all the resources transactions associated with the resource
      -- gme_resource_engine_pvt.delete_resource_trans
      UPDATE gme_resource_txns_gtmp
         SET action_code = 'DEL'
       WHERE line_id IN (
                SELECT batchstep_resource_id
                  FROM gme_batch_step_resources
                 WHERE batch_id = l_batch_step.batch_id
                   AND batchstep_id = l_batch_step.batchstep_id)
         AND action_code NOT IN ('REVS', 'REVL', 'DEL')
         AND delete_mark <> 1;

      -- 2. Delete all the process parameters
      DELETE      gme_process_parameters
            WHERE batch_id = l_batch_step.batch_id
              AND batchstep_id = l_batch_step.batchstep_id;

      -- 3. Delete all the resources
      DELETE      gme_batch_step_resources
            WHERE batch_id = l_batch_step.batch_id
              AND batchstep_id = l_batch_step.batchstep_id;

      -- 4. Delete all the activities
      DELETE      gme_batch_step_activities
            WHERE batch_id = l_batch_step.batch_id
              AND batchstep_id = l_batch_step.batchstep_id;

      -- 5. Delete all the chanrges
      DELETE      gme_batch_step_charges
            WHERE batch_id = l_batch_step.batch_id
              AND batchstep_id = l_batch_step.batchstep_id;

      -- Navin: END Changed the logic  --
      FOR l_rec IN cur_get_dependency (l_batch_step.batchstep_id) LOOP
         l_batchstep_dependency.batch_id := l_batch_header.batch_id;
         l_batchstep_dependency.batchstep_id := l_rec.batchstep_id;
         l_batchstep_dependency.dep_step_id := l_rec.dep_step_id;

         IF NOT (gme_batch_step_depend_dbl.delete_row (l_batchstep_dependency) ) THEN
            RAISE step_dependency_delete_error;
         END IF;
      END LOOP;
              /* FOR l_rec IN Cur_get_dependency(l_batch_step.batchstep_id) */

      /* Fetch all the items associated with the step */
      OPEN cur_get_item_assoc (l_batch_step.batchstep_id);

      FETCH cur_get_item_assoc
      BULK COLLECT INTO l_material_ids;

      CLOSE cur_get_item_assoc;

      /* Fetch all the item association details */
      FOR i IN 1 .. l_material_ids.COUNT LOOP
         l_batchstep_items.material_detail_id := l_material_ids (i);

         IF NOT (gme_batch_step_items_dbl.delete_row (l_batchstep_items) ) THEN
            RAISE step_item_delete_error;
         END IF;
      END LOOP;

      IF NOT (gme_batch_steps_dbl.delete_row (l_batch_step) ) THEN
         RAISE step_delete_error;
      END IF;

      --Begin Bug#3109673  P.Raghu
      --While Rerouting a batch, the call 'gmd_auto_step_calc.calc_step_qty' is not required.
      --It is called when the dependent batch step is deleted.
      IF NOT p_reroute_flag THEN
         IF l_batch_header.automatic_step_calculation = 1 THEN
            IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
               gme_debug.put_line ('l_batch_step' || l_batch_step.batchstep_id);
               gme_debug.put_line ('auto step calc...');
            END IF;

            gmd_auto_step_calc.calc_step_qty
                          (p_parent_id              => l_batch_header.batch_id
                          ,p_step_tbl               => l_step_tbl
                          ,p_msg_count              => l_msg_count
                          ,p_msg_stack              => l_msg_stack
                          ,p_return_status          => l_return_status
                          ,p_called_from_batch      => 1
                          ,p_organization_id        => l_batch_header.organization_id);
         END IF;

         IF l_return_status <> x_return_status THEN
            IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
               gme_debug.put_line
                              (fnd_msg_pub.get (p_encoded      => fnd_api.g_false) );
            END IF;

            RAISE error_calc_step_qty;
         END IF;

         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line ('done step calc...');
         END IF;

         /*Bug 2395188  Bharati Satpute  Replaced first,last with count */
         -- Update the plan_step_qty in GME_BATCH_STEPS
         FOR i IN 1 .. l_step_tbl.COUNT LOOP
            UPDATE gme_batch_steps
               SET plan_step_qty = l_step_tbl (i).step_qty
                  ,plan_mass_qty = l_step_tbl (i).step_mass_qty
                  ,mass_ref_um = l_step_tbl (i).step_mass_uom
                  ,plan_volume_qty = l_step_tbl (i).step_vol_qty
                  ,volume_ref_um = l_step_tbl (i).step_vol_uom
             WHERE batch_id = l_batch_header.batch_id
               AND batchstep_no = l_step_tbl (i).step_no;
         END LOOP;          /* FOR i IN l_step_tbl.FIRST .. l_step_tbl.LAST */
      END IF;

      --End Bug#3109673
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;
   EXCEPTION
      WHEN invalid_step THEN
         ROLLBACK TO SAVEPOINT delete_batch_step;
         x_return_status := fnd_api.g_ret_sts_error;
         gme_common_pvt.log_message ('GME_STEP_NOT_PENDING');
      /*   FND_MSG_PUB.COUNT_AND_GET (P_count => x_message_count,
                                 P_data  => x_message_list); */
      WHEN error_calc_step_qty THEN
         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line ('calc step qty error');
         END IF;

         x_return_status := fnd_api.g_ret_sts_error;
      WHEN one_step_reqd THEN
         ROLLBACK TO SAVEPOINT delete_batch_step;
         x_return_status := fnd_api.g_ret_sts_error;
         gme_common_pvt.log_message ('GME_ONE_STEP_REQD');
/*   FND_MSG_PUB.COUNT_AND_GET (P_count => x_message_count,
                               P_data  => x_message_list); */
      WHEN step_activity_delete_error OR error_load_trans OR batch_step_fetch_error OR batch_header_fetch_error OR step_dependency_delete_error OR step_item_delete_error OR step_delete_error
           OR fnd_api.g_exc_error THEN
         x_return_status := fnd_api.g_ret_sts_error;
         ROLLBACK TO SAVEPOINT delete_batch_step;
      /*  FND_MSG_PUB.COUNT_AND_GET (P_count => x_message_count,
                                   P_data  => x_message_list); */
      /*ER 19161894  Shaliu Chen 18-JUL-2014
        If CancelPOReq failed,it does not affect batch complete action.
       */                                   
      WHEN cancel_po_req_fail THEN
        ROLLBACK TO SAVEPOINT delete_batch_step;
        IF (g_debug <= gme_debug.g_log_procedure) THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ':'
                                || ' Cancel OSP PO/Req failed');
        END IF;
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

         x_return_status := fnd_api.g_ret_sts_unexp_error;
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
   END delete_step;

/*===========================================================================================
Procedure
  delete_activity
Description
  This particular procedure is used to delete the activity and its details in a batch.
Parameters
  p_batch_step_activities_rec   The batch step activity record.
  x_return_status               outcome of the API call
                                S - Success
                                E - Error
                                U - Unexpected error
=============================================================================================*/
   PROCEDURE delete_activity (
      p_batch_step_activities_rec   IN              gme_batch_step_activities%ROWTYPE
     ,x_return_status               OUT NOCOPY      VARCHAR2)
   IS
      l_api_name              CONSTANT VARCHAR2 (30)     := 'delete_activity';
      /* Collections for details etc */
      l_batchstep_resource_ids         gme_common_pvt.number_tab;
      l_gme_batchstep_resources        gme_batch_step_resources%ROWTYPE;

      CURSOR cur_step_resource_ids (v_batchstep_activity_id NUMBER)
      IS
         SELECT batchstep_resource_id
           FROM gme_batch_step_resources
          WHERE batchstep_activity_id = v_batchstep_activity_id;

      --Rishi Varma bug 3307549 13-May-2004 start
      CURSOR cur_get_batch_id (v_batchstep_activity_id NUMBER)
      IS
         SELECT batch_id
           FROM gme_batch_step_activities
          WHERE batchstep_activity_id = v_batchstep_activity_id;

      l_batch_id                       NUMBER;
      --Rishi Varma bug 3307549 13-May-2004 end

      /* Local variables */
      l_return_status                  VARCHAR2 (1);
      /* Exception */
      activity_resource_delete_error   EXCEPTION;
      step_activity_delete_error       EXCEPTION;

      -- Bug 5903208
      gmf_cost_failure                 EXCEPTION;
      l_message_count		       NUMBER;
      l_message_list		       VARCHAR2(2000);

   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      /* Set the return status to success initially */
      x_return_status := fnd_api.g_ret_sts_success;

      /* Fetch all the resources associated with the activity */
      OPEN cur_step_resource_ids
                            (p_batch_step_activities_rec.batchstep_activity_id);

      FETCH cur_step_resource_ids
      BULK COLLECT INTO l_batchstep_resource_ids;

      CLOSE cur_step_resource_ids;

      /* Fetch all the activity resources details */
      FOR i IN 1 .. l_batchstep_resource_ids.COUNT LOOP
         l_gme_batchstep_resources.batchstep_resource_id :=
                                                 l_batchstep_resource_ids (i);
         /* Delete all the resources associated with the activity */
         delete_resource (l_gme_batchstep_resources, l_return_status);

         IF l_return_status <> x_return_status THEN
            RAISE activity_resource_delete_error;
         END IF;
      END LOOP;

      --Rishi Varma bug 3307549 13-05-2004 start
      OPEN cur_get_batch_id (p_batch_step_activities_rec.batchstep_activity_id);
      FETCH cur_get_batch_id INTO l_batch_id;
      CLOSE cur_get_batch_id;


      -- 
      -- Bug 5903208 -- call to GMF
      --
      IF l_batchstep_resource_ids.COUNT > 0 THEN
         GMF_VIB.Update_Batch_Requirements
         ( p_api_version   =>    1.0,
           p_init_msg_list =>    FND_API.G_FALSE,
           p_batch_id      =>    l_batch_id,
           x_return_status =>    l_return_status,
           x_msg_count     =>    l_message_count,
           x_msg_data      =>    l_message_list);
         
         IF l_return_status <> FND_API.G_RET_STS_SUCCESS
         THEN
            RAISE gmf_cost_failure;
         END IF;
      END IF;
    
      IF NOT (gme_batch_step_activities_dbl.delete_row
                                                  (p_batch_step_activities_rec) ) THEN
         RAISE step_activity_delete_error;
      END IF;

      gme_batch_step_chg_pvt.set_activity_sequence_num (l_batch_id);

      --Rishi Varma bug 3307549 13-05-2004 end.
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;
   EXCEPTION
      WHEN   gmf_cost_failure THEN
        -- Bug 5903208
        x_return_status := FND_API.G_RET_STS_ERROR;
    
      WHEN step_activity_delete_error OR activity_resource_delete_error THEN
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

         x_return_status := fnd_api.g_ret_sts_unexp_error;
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
   END delete_activity;

/*===========================================================================================
Procedure
  delete_resource
Description
  This particular procedure is used to delete the resource and its details in a batch.
Parameters
  p_batch_step_resources_rec    The batch step resources record.
  x_return_status               outcome of the API call
                                S - Success
                                E - Error
                                U - Unexpected error
  Pawan kumar  Added the process parameters deletion procedure for bug 2509572
  Rishi Varma bug 3307549 18-05-2004
     Added code to remove charges associated with a chargeable resource.
  Rishi Varma 3682311 11-06-2004
     Removed the explicit commit after the clear_charge procedure.
  Rishi Varma 3718176 20-07-2004
     Added code to remove the charges assocaited with a chargeable resource only if its the
     last instance of the resource present in the step.
=============================================================================================*/
   PROCEDURE delete_resource (
      p_batch_step_resources_rec                gme_batch_step_resources%ROWTYPE
     ,x_return_status              OUT NOCOPY   VARCHAR2)
   IS
      /* 2841929: Added parameters to the cursor and passing proper parameters */
      CURSOR cur_get_update_inventory (v_batch_id IN NUMBER)
      IS
         SELECT update_inventory_ind
           FROM gme_batch_header
          WHERE batch_id = v_batch_id;

      --Rishi Varma bug# 3307549 10-05-2004
      CURSOR cur_is_charge_associated (
         v_resources      gme_batch_step_resources.resources%TYPE
        ,v_batchstep_id   NUMBER)
      IS
         SELECT 1
           FROM DUAL
          WHERE EXISTS (
                   SELECT 1
                     FROM gme_batch_step_charges
                    WHERE resources = v_resources
                      AND batchstep_id = v_batchstep_id);
                      
      /*ER 19161894  Shaliu Chen 18-JUL-2014*/                
      CURSOR cur_get_osp_parameter(v_organization_id NUMBER)
      IS
         SELECT propagate_change_to_po
           FROM gme_parameters
          WHERE organization_id = v_organization_id;                       

      l_cur_is_charge_associated       cur_is_charge_associated%ROWTYPE;
      resource_charge_delete_error     EXCEPTION;

      --Rishi Varma B3718176 20-07-2004.
      CURSOR cur_check_last_rsrc_instance (
         v_resources      gme_batch_step_resources.resources%TYPE
        ,v_batchstep_id   NUMBER)
      IS
         SELECT COUNT (1)
           FROM gme_batch_step_resources
          WHERE resources = v_resources AND batchstep_id = v_batchstep_id;

      l_rsrc_count                     NUMBER                            := -1;
      l_api_name              CONSTANT VARCHAR2 (30)      := 'Delete_resource';
      /* Local variables */
      l_return_status                  VARCHAR2 (1);
      l_update_inventory_ind           VARCHAR2 (1);
      l_rsrc_parameters                gme_process_parameters%ROWTYPE;
      l_batch_step_resource            gme_batch_step_resources%ROWTYPE;
      /*ER 19161894  Shaliu Chen 18-JUL-2014*/
      l_propagate_change_to_po         NUMBER;
      l_osp_resource_flag              NUMBER;
      l_message_count                  NUMBER;
      l_message_data                   VARCHAR2(2000); 
      /*END ER 19161894*/
           
      /* Exceptions */
      trans_delete_error               EXCEPTION;
      activity_resource_delete_error   EXCEPTION;
      rsrc_param_delete_error          EXCEPTION;
      cancel_po_req_fail               EXCEPTION;  /*ER 19161894  Shaliu Chen 18-JUL-2014*/
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      /* Set the return status to success initially */
      x_return_status := fnd_api.g_ret_sts_success;

      /* 2841929: populate local variable with the current batchstep record */
      IF NOT (gme_batch_step_resources_dbl.fetch_row
                                                  (p_batch_step_resources_rec
                                                  ,l_batch_step_resource) ) THEN
         RAISE activity_resource_delete_error;
      END IF;

      --Pawan kumar  Added the process parameters deletion procedure for bug 2509572
        /* First delete all the process parameters associated with the resource */
      l_rsrc_parameters.batchstep_resource_id :=
                              p_batch_step_resources_rec.batchstep_resource_id;

      --Rishi Varma bug 3307549 10/05/2004 start
      OPEN cur_is_charge_associated (l_batch_step_resource.resources
                                    ,l_batch_step_resource.batchstep_id);

      FETCH cur_is_charge_associated
       INTO l_cur_is_charge_associated;

      IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
         gme_debug.put_line (   l_api_name
                             || 'batch,step idno. are '
                             || l_batch_step_resource.batch_id
                             || l_batch_step_resource.batchstep_id);
      END IF;

      IF cur_is_charge_associated%FOUND THEN
         CLOSE cur_is_charge_associated;

         --Rishi Varma B3718716 20-07-2004
         /*Deleting the charges associated to the resources only if this is the only instance of
         this resouces in the step which is being deleted*/
         OPEN cur_check_last_rsrc_instance
                                          (l_batch_step_resource.resources
                                          ,l_batch_step_resource.batchstep_id);

         FETCH cur_check_last_rsrc_instance
          INTO l_rsrc_count;

         IF l_rsrc_count = 1 THEN
            CLOSE cur_check_last_rsrc_instance;

            gme_batch_step_chg_pvt.clear_charges
                       (p_batch_id           => l_batch_step_resource.batch_id
                       ,p_batchstep_id       => l_batch_step_resource.batchstep_id
                       ,x_return_status      => l_return_status);

            IF l_return_status <> x_return_status THEN
               RAISE resource_charge_delete_error;
            --Rishi Varma 3682311 11-06-2004
            /*Commented the explicit "COMMIT" as the commit will propogate either from the
            forms or from the public,main api's*/
            --ELSE
            COMMIT;
            END IF;
         ELSE
            CLOSE cur_check_last_rsrc_instance;
         END IF;
      ELSE
         CLOSE cur_is_charge_associated;
      END IF;

      --Rishi Varma bug 3307549 10/05/2004 end
      IF NOT (gme_process_parameters_dbl.delete_all
                                    (p_process_parameters      => l_rsrc_parameters) ) THEN
         RAISE rsrc_param_delete_error;
      END IF;

      /* 2841929: pass proper parameter value */
      OPEN cur_get_update_inventory (l_batch_step_resource.batch_id);

      FETCH cur_get_update_inventory
       INTO l_update_inventory_ind;

      CLOSE cur_get_update_inventory;

      IF l_update_inventory_ind = 'Y' THEN
         /* First delete all the resources transactions associated with the resource */
         gme_delete_batch_step_pvt.delete_resource_transactions
                   (p_batch_step_resources_rec      => p_batch_step_resources_rec
                   ,x_return_status                 => l_return_status);

         IF l_return_status <> x_return_status THEN
            RAISE trans_delete_error;
         END IF;
      END IF;
      
      /*                                    
      BEGIN ER 19161894  Shaliu Chen 18-JUL-2014               
      Added for OPM Step Level OSP Project 
      If resource is OSP resource,invoke cancelreqpo
      program to cancel corresponding PO/Requisition 
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
            Check whether the batch include Outside resource.
           */
          /*
          BEGIN ER 20809749
          Shaliu Chen 17-APR-2015
          Add condition batchstep_id to support multiple osp steps.
          */             
          SELECT count(1)
            INTO l_osp_resource_flag
            FROM gme_batch_step_resources gbsr
           WHERE batch_id = p_batch_step_resources_rec.batch_id
             AND batchstep_id = p_batch_step_resources_rec.batchstep_id
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
            OPEN cur_get_osp_parameter(p_batch_step_resources_rec.organization_id);
            FETCH cur_get_osp_parameter INTO l_propagate_change_to_po;
            CLOSE cur_get_osp_parameter;
            /*
             Cancel Open PO linked to the batch if propagate_change_to_po
             parameter is Automatic
            */
            IF (NVL(l_propagate_change_to_po,gme_osp.g_propagate_change_manual) 
                       = gme_osp.g_propagate_change_automatic) THEN                
              /*
                Invoke Batch product quantity change to the PO/Req
                linked to the batch.
              */      
                      
              gme_osp.cancelPOReq (p_batch_id      => p_batch_step_resources_rec.batch_id,
                                   p_org_id        => p_batch_step_resources_rec.organization_id,
                                   p_batchstep_id  => p_batch_step_resources_rec.batchstep_id,
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
      
      IF NOT (gme_batch_step_resources_dbl.delete_row
                                                   (p_batch_step_resources_rec) ) THEN
         RAISE activity_resource_delete_error;
      END IF;

      --Rishi Varma bug 3307549 13/05/2004
      --Re sequencing the sequence dependent ids after the delete.
      gme_batch_step_chg_pvt.set_sequence_dependent_id
                                               (l_batch_step_resource.batch_id);

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;
   EXCEPTION
      WHEN trans_delete_error OR activity_resource_delete_error OR rsrc_param_delete_error OR resource_charge_delete_error THEN
         x_return_status := fnd_api.g_ret_sts_error;
      /*ER 19161894  Shaliu Chen 18-JUL-2014
        If CancelPOReq failed,it does not affect batch complete action.
       */                                   
      WHEN cancel_po_req_fail THEN
        ROLLBACK TO SAVEPOINT delete_batch_step;
        IF (g_debug <= gme_debug.g_log_procedure) THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ':'
                                || ' Cancel OSP PO/Req failed');
        END IF;
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

         x_return_status := fnd_api.g_ret_sts_unexp_error;
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
   END delete_resource;

/*===========================================================================================
Procedure
  delete_resource_transactions
Description
  This particular procedure is used to delete the resource transactions in a batch.
Parameters
  p_batch_step_resources_rec    The batch step resources record.
  x_return_status               outcome of the API call
                                S - Success
                                E - Error
                                U - Unexpected error
=============================================================================================*/
   PROCEDURE delete_resource_transactions (
      p_batch_step_resources_rec                gme_batch_step_resources%ROWTYPE
     ,x_return_status              OUT NOCOPY   VARCHAR2)
   IS
      l_resource_txns       gme_resource_txns_gtmp%ROWTYPE;
      l_resource_tab        gme_common_pvt.resource_transactions_tab;
      l_api_name   CONSTANT VARCHAR2 (30)   := 'delete_resource_transactions';
      l_return_status       VARCHAR2 (1);
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      /* Set the return status to success initially */
      x_return_status := fnd_api.g_ret_sts_success;
      /* Lets fetch all the active resource transactions for the current resource */
      l_resource_txns.poc_trans_id := NULL;
      l_resource_txns.doc_id := NULL;
      l_resource_txns.line_id :=
                              p_batch_step_resources_rec.batchstep_resource_id;
      gme_resource_engine_pvt.fetch_active_resources
                                           (p_resource_rec       => l_resource_txns
                                           ,x_resource_tbl       => l_resource_tab
                                           ,x_return_status      => l_return_status);

      IF l_return_status <> x_return_status THEN
         RAISE fnd_api.g_exc_error;
      END IF;                      /* IF l_return_status <> x_return_status */

      FOR i IN 1 .. l_resource_tab.COUNT LOOP
         l_resource_txns := l_resource_tab (i);
         gme_resource_engine_pvt.delete_resource_trans
                                          (p_tran_rec           => l_resource_txns
                                          ,x_return_status      => l_return_status);

         IF l_return_status <> x_return_status THEN
            RAISE fnd_api.g_exc_error;
         END IF;
      END LOOP;                         /* FOR i IN 1..l_resource_tab.COUNT */

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;
   EXCEPTION
      WHEN fnd_api.g_exc_error THEN
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

         x_return_status := fnd_api.g_ret_sts_unexp_error;
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
   END delete_resource_transactions;
END gme_delete_batch_step_pvt;
/

COMMIT ;
EXIT;
