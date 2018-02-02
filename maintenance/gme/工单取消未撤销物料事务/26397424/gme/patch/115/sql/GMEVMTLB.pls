/* +======================================================================+ */
/* |    Copyright (c) 2005, 2015 Oracle and/or its affiliates.           | */
/* |                         All rights reserved.                         | */
/* |                           Version 12.0.0                             | */
/* +======================================================================+ */
REM dbdrv: sql ~PROD ~PATH ~FILE none none none package &phase=plb \
REM dbdrv: checkfile(120.28.12010000.22=120.36.12020000.12)(120.25.12000000.16=120.28.12010000.15)(120.25.12000000.10=120.36):~PROD:~PATH:~FILE
SET VERIFY OFF
WHENEVER SQLERROR EXIT FAILURE ROLLBACK;
WHENEVER OSERROR EXIT FAILURE ROLLBACK;
REM ***********************************************************************
REM *                                                                     *
REM * FILE:    GMEVMTLB.pls                                               *
REM * PURPOSE: Package Body for the routines to insert, update and delete *
REM *          records from GME_MATERIAL_DETAILS
REM * AUTHOR:  Antonia Newbury, OPM Development                           *
REM * DATE:    24-Jan-2005                                                *
REM * HISTORY:                                                            *
REM * ========                                                            *
REM * Pawan Kumar 01-10-2006 bug 4742244                                  *
REM * Raising exception after is_dispense_item procedure returns error    *
REM * Chandrashekar Tiruvidula Bug 4867497 added subinventory locator and *
REM * qty logic for phantoms                                              *
REM * SivakumarG Bug#5078853 03-03-2006
REM *  Created procedure validate_material_for_del     
REM * Pawan Kumar  bug 5127489  04-07-2006
REM * Changed from calc_mtl_req_date to material_date_change
REM * Susruth D. Bug#5129153 Changed the data type of 'p_byproduct_type' 
REM * to VARCHAR2 in procedure validate_byproduct_type.Include a check
REM * for revision controlled items for opening up the actual qty field.
REM * Susruth D. Bug#5159393 Added code in update_material_line 
REM * procedure to correct the timestamp for last_update_date before updation.
REM * Sunitha ch. Bug 5353941 Check whether phantom material is associated to 
REM * step and call reschedule batch if it is not associated to step or call
REM * rescedule step for that batch if it is associated to step
REM * Restructured the code and also added the code that will handle  the 
REM * rescheduling batch/step when update yield Type of the Child batch is done
REM * sunitha ch. bug rework#5333521 assign 0 to wip_plan_qty if it is NULL      * 
REM * sunitha ch. bug 5566769 update the revision field of the phantom batch*
REM * Chandu Bug 5681997 if item is not txn enabled then open actual qty *

REM * Chandu Bug 5681997 if item is not txn enabled then open actual qty *
REM * G. Muratore   Bug 8693767    23-JUL-2009
REM *    Add 6 as a valid option to serial type list.

REM * G. Muratore   Bug 10062802   30-AUG-2010
REM *    Add formulaline_id condition to validation when user is deleting a product
REM *    PROCEDURE: validate_material_for_del

REM * A.Mishra      Bug 10046018   06-DEC-2010
REM *    Default subinventory and/or locator properly for newly inserted lines.
REM *    PROCEDURE: insert_material_line

REM * G. Muratore   Bug 12674978   21-JUN-2011
REM *    Rework of 10046018. Default subinventory and/or locator properly 
REM *    only if there is no value for newly inserted lines.
REM *    PROCEDURE: insert_material_line

REM * G. Muratore   Bug 11939155   29-JUL-2011
REM *    Initialize primary_qty to NULL which will force the code to recompute it. 
REM *    PROCEDURE: process_actual_qty

REM * G. Muratore   Bug 12563379   15-NOV-2011
REM *    Do not create multiple new transactions when user overtypes actual qty multiple times. 
REM *    PROCEDURE: process_actual_qty

REM * G. Muratore   Bug 13076579   02-DEC-2011
REM *    Do not update move order lines when user increases plan or wip plan qty.
REM *    This causes problems in picking code later in the process. The picking
REM *    logic will now account for the delta change during picking. 
REM *    PROCEDURE: update_material_line

REM * G. Muratore    Bug 13976194   24-APR-2012
REM *    Update primary_qty when user overtypes actual qty multiple times. 
REM *    PROCEDURE: process_actual_qty

REM * Archana Mundhe Bug 13743650   07-MAY-2012 
REM *    Secondary quantity was not getting calculated correctly when the actual qty was 
REM *    updated directly from material details form. 
REM *    Moved up the code to calculate the secondary qty based on the actual qty. 
REM *    Assigned this secondary qty to the secondary transaction quantity in
REM *    mmt_rec and mmln_rec. 
REM *    PROCEDURE: process_actual_qty. 

REM * G. Muratore    Bug 14612460   18-SEP-2012
REM *    Delete move orders when user overtypes actual qty with zero. 
REM *    PROCEDURE: update_material_line

REM * QZENG          Bug 16457668   07-Mar-2013
REM *    No need to validate some fields when g_bulk_validation_done is Y for these fields are
REM *    validated in open interface
REM * QZENG          Bug 16457668   13-Mar-2013
REM *    Commented out log error message when error_dbl occurred for the error has been logged at
REM *    gme_material_details_dbl

REM * G. Muratore    Bug 18779329   18-JUN-2014
REM *    Do not allow deletions if transactions exist. 
REM *    PROCEDURE: validate_material_for_del

REM * Shaliu Chen     18-JUL-2014  ER 19161894                                                 
REM *    Modify create_batch to invoke requisition creation program if batch include OSP step
 
REM *  Shaliu Chen     09-APR-2015  ER 20809749
REM *    Modify create batch to support multiple osp steps. 

REM * G. Muratore    Bug 21480679   22-Jul-2015
REM *    To be consistent with the form and release batch, we must allow wip plan qty to update
REM *    in a wip batch for an auto by step ingredient associated to pending step.
REM *    PROCEDURE: validate_material_for_ins

REM * G. Muratore    Bug 21817332/21840315   23-Sep-2015
REM *    Recalculate original qty if the uom has changed.
REM *    PROCEDURE: update_material_line
REM ***********************************************************************
/**************************************************************************
* This file contains the procedure for actions on material lines in Oracle*
* Process Manufacturing (OPM). Each procedure has a common set of         *
* parameters to which API-specific parameters are appended.               *
**************************************************************************/

CREATE OR REPLACE PACKAGE BODY gme_material_detail_pvt AS
/* $Header: GMEVMTLB.pls 120.36.12020000.12 2015/09/23 13:50:46 gmurator ship $ */
   g_debug      VARCHAR2 (5)  := fnd_profile.VALUE ('AFLOG_LEVEL');
   g_pkg_name   VARCHAR2 (30) := 'gme_material_detail_pvt';

   PROCEDURE insert_material_line
     (p_batch_header_rec      IN              gme_batch_header%ROWTYPE
     ,p_material_detail_rec   IN              gme_material_details%ROWTYPE
     ,p_batch_step_rec        IN              gme_batch_steps%ROWTYPE
     ,p_trans_id              IN              NUMBER
     ,x_transacted            OUT NOCOPY      VARCHAR2
     ,x_return_status         OUT NOCOPY      VARCHAR2
     ,x_material_detail_rec   OUT NOCOPY      gme_material_details%ROWTYPE)
   IS
      l_api_name       CONSTANT VARCHAR2 (30)       := 'insert_material_line';
      l_material_detail_rec     gme_material_details%ROWTYPE;
      l_batchstep_items         gme_batch_step_items%ROWTYPE;
      l_batch_step_rec          gme_batch_steps%ROWTYPE;
      l_out_material_detail_tbl gme_common_pvt.material_details_tab;
      l_material_detail_tbl     gme_common_pvt.material_details_tab;
      l_recipe_id               NUMBER;
      l_message_count           NUMBER;
      l_message_list            VARCHAR2 (2000);
      l_proc                    VARCHAR2 (100);
      l_rsc_count               NUMBER;
      l_trolin_tbl              inv_move_order_pub.trolin_tbl_type;
      l_return_status           varchar2(1);
      l_config_id               NUMBER;
      
      -- Bug 12674978
      l_subinventory            VARCHAR2 (10);
      l_locator_id              NUMBER;
      
      CURSOR recipe_validity_rule_cursor (v_recipe_validity_rule_id NUMBER)
      IS
         SELECT recipe_id
           FROM gmd_recipe_validity_rules
          WHERE recipe_validity_rule_id = v_recipe_validity_rule_id;

      error_dbl                         EXCEPTION;
      error_processing                  EXCEPTION;
      error_dispensing                  EXCEPTION;

   BEGIN
      IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
        gme_debug.put_line('Entering api '||g_pkg_name||'.'||l_api_name);
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' batch_id='||p_batch_header_rec.batch_id);
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' batchstep_id='||p_batch_step_rec.batchstep_id);
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' line_no='||p_material_detail_rec.line_no);
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' line_type='||p_material_detail_rec.line_type);
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' inventory_item_id='||p_material_detail_rec.inventory_item_id);
      END IF;

      /* Set the return status to success initially */
      x_return_status := fnd_api.g_ret_sts_success;

      -- renumber subsequent lines
      UPDATE gme_material_details
         SET line_no = line_no + 1
            ,last_updated_by     = gme_common_pvt.g_user_ident
            ,last_update_date    = gme_common_pvt.g_timestamp
            ,last_update_login   = gme_common_pvt.g_login_id
       WHERE batch_id            = p_material_detail_rec.batch_id
         AND line_type           = p_material_detail_rec.line_type
         AND line_no            >= p_material_detail_rec.line_no;

      IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
         gme_debug.put_line (g_pkg_name||'.'|| l_api_name||': '||SQL%ROWCOUNT||' records renumbered');
      END IF;

      -- insert new material line
      IF NOT gme_material_details_dbl.insert_row (p_material_detail_rec
                                                 ,x_material_detail_rec) THEN
         l_proc := 'gme_material_details_dbl.insert_row';
         RAISE error_dbl;
      END IF;

      IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
        gme_debug.put_line (g_pkg_name||'.'||l_api_name||' inserted material; material_detail_id= '
                                                       || x_material_detail_rec.material_detail_id);
      END IF;

      -- Bug 10046018 - set the defaults properly.
      IF (x_material_detail_rec.subinventory IS NULL OR x_material_detail_rec.locator_id IS NULL) THEN
         IF (x_material_detail_rec.line_type = -1) THEN
            gme_common_pvt.get_supply_defaults 
                                (p_organization_id        => x_material_detail_rec.organization_id
                                ,p_inventory_item_id      => x_material_detail_rec.inventory_item_id
                                ,x_subinventory           => l_subinventory
                                ,x_locator_id             => l_locator_id
                                ,x_return_status          => l_return_status);
         ELSE
            gme_common_pvt.get_yield_defaults  
                                (p_organization_id        => x_material_detail_rec.organization_id
                                ,p_inventory_item_id      => x_material_detail_rec.inventory_item_id
                                ,p_line_type              => x_material_detail_rec.line_type
                                ,x_subinventory           => l_subinventory
                                ,x_locator_id             => l_locator_id
                                ,x_return_status          => l_return_status);
         END IF;
         
         -- Bug 12674978 - Only set the values if they are null.
         IF x_material_detail_rec.subinventory IS NULL THEN 
            x_material_detail_rec.subinventory := l_subinventory;
         END IF;
            
         IF x_material_detail_rec.locator_id IS NULL THEN 
            x_material_detail_rec.locator_id := l_locator_id;
         END IF;
      END IF;

      IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
        gme_debug.put_line (g_pkg_name||'.'||l_api_name||' default subinventory is '|| x_material_detail_rec.subinventory);
        gme_debug.put_line (g_pkg_name||'.'||l_api_name||' default locator_id is '|| x_material_detail_rec.locator_id);
      END IF;
      -- End Bug 10046018

      --FPbug#4543872 moved this code over here
      -- can call this regardless of batch/step status... will also handle if batch is pending and qty is 0 (will do nothing)
      open_and_process_actual_qty
                         (p_batch_header_rec      => p_batch_header_rec
                         ,p_material_detail_rec   => x_material_detail_rec
                         ,p_batch_step_rec        => p_batch_step_rec
                         ,p_trans_id              => p_trans_id
                         ,p_insert                => FND_API.g_true
                         ,x_transacted            => x_transacted
                         ,x_return_status         => x_return_status);

      IF x_return_status <> fnd_api.g_ret_sts_success THEN
        l_proc := 'open_and_process_actual_qty';
        RAISE error_processing;
      END IF;

      -- item / step association
      IF p_batch_step_rec.batchstep_id IS NOT NULL THEN
         l_batchstep_items.material_detail_id    := x_material_detail_rec.material_detail_id;
         l_batchstep_items.batch_id              := x_material_detail_rec.batch_id;
         l_batchstep_items.batchstep_id          := p_batch_step_rec.batchstep_id;

         IF NOT gme_batch_step_items_dbl.insert_row
                                    (p_batch_step_items      => l_batchstep_items
                                    ,x_batch_step_items      => l_batchstep_items) THEN
            l_proc := 'gme_batch_step_items_dbl.insert_row';
            RAISE error_dbl;
         END IF;
      END IF;

      l_material_detail_rec := x_material_detail_rec;

      gme_common_pvt.calc_mtl_req_date
           (p_batch_header_rec      => p_batch_header_rec
           ,p_batchstep_rec         => p_batch_step_rec
           ,p_mtl_dtl_rec           => l_material_detail_rec
           ,x_mtl_req_date          => x_material_detail_rec.material_requirement_date
           ,x_return_status         => x_return_status);

      IF x_return_status <> fnd_api.g_ret_sts_success THEN
         l_proc := 'gme_common_pvt.calc_mtl_req_date';
         RAISE error_processing;
      END IF;

      IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
        gme_debug.put_line (g_pkg_name||'.'||l_api_name||' material_reqirement_date= '
                                      || TO_CHAR(x_material_detail_rec.material_requirement_date,
                                                 gme_material_detail_pvt.g_date_fmt));
      END IF;
      /* Bug 4866700 added update inventory check */
      IF x_material_detail_rec.line_type = gme_common_pvt.g_line_type_ing AND
         x_material_detail_rec.phantom_type = 0 AND
         p_batch_header_rec.update_inventory_ind = 'Y' THEN
         l_material_detail_tbl (1) := x_material_detail_rec;

         -- add material line into invisible move order
         gme_move_orders_pvt.create_move_order_lines
           (p_move_order_header_id       => p_batch_header_rec.move_order_header_id
           ,p_move_order_type            => gme_common_pvt.g_invis_move_order_type
           ,p_material_details_tbl       => l_material_detail_tbl
           ,x_material_details_tbl       => l_out_material_detail_tbl
           ,x_trolin_tbl                 => l_trolin_tbl
           ,x_return_status              => x_return_status);

         IF x_return_status <> FND_API.G_RET_STS_SUCCESS THEN
            l_proc := 'gme_move_orders_pvt.create_move_order_lines';
            RAISE error_processing;
         END IF;

         x_material_detail_rec := l_out_material_detail_tbl(1);

         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
           gme_debug.put_line (g_pkg_name||'.'||l_api_name||' move_order_line_id= '
                                                          ||x_material_detail_rec.move_order_line_id);
         END IF;

         IF p_batch_header_rec.recipe_validity_rule_id IS NULL THEN  -- LCF batch
           l_recipe_id := NULL;
         ELSE
           OPEN recipe_validity_rule_cursor(p_batch_header_rec.recipe_validity_rule_id);
           FETCH recipe_validity_rule_cursor INTO l_recipe_id;
           CLOSE recipe_validity_rule_cursor;
          --Pawan Kumar add for bug 5365883     
          -- moved the END IF after dispense item processing
                --END IF;
                
                   gmo_dispense_grp.is_dispense_item
                         (p_api_version               => 1.0
                         ,p_init_msg_list             => fnd_api.g_false
                         ,x_return_status             => l_return_status
                         ,x_msg_count                 => l_message_count
                         ,x_msg_data                  => l_message_list
                         ,p_inventory_item_id         => x_material_detail_rec.inventory_item_id
                         ,p_organization_id           => x_material_detail_rec.organization_id
                         ,p_recipe_id                 => l_recipe_id
                         ,x_dispense_required         => x_material_detail_rec.dispense_ind
                         ,x_dispense_config_id        => l_config_id);

             IF l_return_status <> FND_API.G_RET_STS_SUCCESS THEN
                l_proc := 'gmo_dispense_grp.is_dispense_item';
               -- Pawan Kumar 01-10-2006 bug 4742244                              *
               -- Raising exception after is_dispense_item procedure returns error        *
                RAISE error_dispensing;
             END IF;

         -- Pawan Kumar bug 4947535 new code added for GMO changes
             IF (l_return_status = FND_API.G_RET_STS_SUCCESS AND x_material_detail_rec.dispense_ind = 'Y') then
                IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
                  gme_debug.put_line (g_pkg_name||'.'||l_api_name||'Instantiating  material line id'
                  || x_material_detail_rec.material_detail_id);
                  gme_debug.put_line (g_pkg_name||'.'||l_api_name||' config id ' || l_config_id);
                 END IF;
                 GMO_DISPENSE_GRP.INSTANTIATE_DISPENSE_SETUP
                        (p_api_version          => 1.0
                        ,p_dispense_config_id   => l_config_id
                        ,p_entity_name          => GMO_DISPENSE_GRP.G_MATERIAL_LINE_ENTITY
                        ,p_entity_key           => x_material_detail_rec.material_detail_id
                        ,p_init_msg_list        => FND_API.G_FALSE
                        ,x_return_status        => l_return_status
                        ,x_msg_count            => l_message_count
                        ,x_msg_data             => l_message_list);
                  IF l_return_status <> FND_API.G_RET_STS_SUCCESS THEN
                   l_proc := 'gmo_dispense_grp.instantiate_dispense_setup';
                   RAISE error_processing;
                  END IF;
               END IF;
            END IF; -- IF recipe_id null
       END IF;

      IF NOT gme_material_details_dbl.update_row (x_material_detail_rec) THEN
         l_proc := 'gme_material_details_dbl.update_row';
         RAISE error_dbl;
      END IF;

      IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
        gme_debug.put_line (g_pkg_name||'.'||l_api_name||' updated material; material_detail_id= '
                                                       || x_material_detail_rec.material_detail_id);
      END IF;

      -- call gme_trans_engine_util.load_rsrc_trans in preparation for update step qty
      IF p_batch_step_rec.batchstep_id IS NOT NULL THEN
         gme_trans_engine_util.load_rsrc_trans
                                           (p_batch_row          => p_batch_header_rec
                                           ,x_rsc_row_count      => l_rsc_count
                                           ,x_return_status      => x_return_status);

         IF x_return_status <> fnd_api.g_ret_sts_success THEN
            l_proc := 'gme_trans_engine_util.load_rsrc_trans';
            RAISE error_processing;
         END IF;

         gme_update_step_qty_pvt.update_step_qty
                                        (p_batch_step_rec      => p_batch_step_rec
                                        ,x_message_count       => l_message_count
                                        ,x_message_list        => l_message_list
                                        ,x_return_status       => x_return_status
                                        ,x_batch_step_rec      => l_batch_step_rec);

         IF x_return_status <> fnd_api.g_ret_sts_success THEN
            l_proc := 'gme_update_step_qty_pvt.update_step_qty';
            RAISE error_processing;
         END IF;
      END IF;

      IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
        gme_debug.put_line('Exiting api '||g_pkg_name||'.'||l_api_name);
      END IF;

   EXCEPTION
      WHEN error_processing THEN
         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
           gme_debug.put_line (g_pkg_name||'.'||l_api_name|| ': ' || l_proc|| ' error returned');
         END IF;
      WHEN error_dbl THEN
         --commented by qzeng, error message has been logged at lower level
         --gme_common_pvt.log_message ('GME_UNEXPECTED_ERROR', 'ERROR', SQLERRM);
         x_return_status := FND_API.g_ret_sts_unexp_error;

         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
           gme_debug.put_line (g_pkg_name||'.'||l_api_name|| ': '
                                                          || l_proc|| ' unexpected error: '|| SQLERRM);
         END IF;

       WHEN  error_dispensing  THEN
           gme_common_pvt.log_message ('GME_DISPENSE_NON_RESERVE');
           x_return_status := l_return_status;
            IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
           gme_debug.put_line (g_pkg_name||'.'||l_api_name|| ': '
                                                          || l_proc|| 'Dispensing error returned ');
         END IF;
      WHEN OTHERS THEN
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
         IF (NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT) THEN
            gme_debug.put_line ('Unexpected error: '||g_pkg_name||'.'||l_api_name||': '||SQLERRM);
         END IF;
         x_return_status := FND_API.g_ret_sts_unexp_error;
   END insert_material_line;

   PROCEDURE validate_batch_for_matl_ins
        (p_batch_header_rec         IN gme_batch_header%ROWTYPE
        ,p_batch_step_rec           IN gme_batch_steps%ROWTYPE
        ,x_return_status            OUT NOCOPY VARCHAR2) IS

     l_api_name              CONSTANT   VARCHAR2 (30)                := 'validate_batch_for_matl_ins';


     validation_error        EXCEPTION;

   BEGIN
     IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
       gme_debug.put_line('Entering api '||g_pkg_name||'.'||l_api_name);
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' batch_id='||p_batch_header_rec.batch_id);
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' batchstep_id='||p_batch_step_rec.batchstep_id);
     END IF;

     /* Set the return status to success initially */
     x_return_status       := FND_API.G_RET_STS_SUCCESS;

     --Bug#5078853 added check for invalid batch type
     IF p_batch_header_rec.batch_type <> 0 THEN
      gme_common_pvt.log_message ('GME_INV_BATCH_TYPE_OPER');
      RAISE validation_error;
     END IF;

     IF (p_batch_header_rec.batch_status NOT IN
                                            (gme_common_pvt.g_batch_pending
                                            ,gme_common_pvt.g_batch_wip
                                            ,gme_common_pvt.g_batch_completed )) THEN
       gme_common_pvt.log_message ('GME_INV_BATCH_STATUS_OPER');
       RAISE validation_error;
     END IF;

     IF p_batch_step_rec.batchstep_id IS NOT NULL THEN
       IF (p_batch_step_rec.step_status NOT IN (gme_common_pvt.g_step_pending
                                               ,gme_common_pvt.g_step_wip
                                               ,gme_common_pvt.g_step_completed)) THEN
         gme_common_pvt.log_message('PC_STEP_STATUS_ERR');
         RAISE validation_error;
       END IF;
     END IF;

     IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
       gme_debug.put_line('Exiting api '||g_pkg_name||'.'||l_api_name);
     END IF;

   EXCEPTION
   WHEN validation_error THEN
     x_return_status := fnd_api.g_ret_sts_error;
   WHEN OTHERS THEN
     fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
     IF (NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT) THEN
       gme_debug.put_line ('Unexpected error: '||g_pkg_name||'.'||l_api_name||': '||SQLERRM);
     END IF;
     x_return_status := FND_API.g_ret_sts_unexp_error;
   END validate_batch_for_matl_ins;

   /***********************************************************************
    *
    * Following fields are not used and will be ignored/not populated:
    * 1. cost,
    * 2. item_um,
    * 3. item_um2
    * 4. alloc_ind
    *

    *
    * Following fields are not supported to be populated by API
    * 1. text_code
    *
    *
    ***********************************************************************/
   PROCEDURE validate_material_for_ins (
      p_batch_header_rec      IN       gme_batch_header%ROWTYPE
     ,p_material_detail_rec   IN       gme_material_details%ROWTYPE
     ,p_batch_step_rec        IN       gme_batch_steps%ROWTYPE
     ,x_material_detail_rec   OUT NOCOPY     gme_material_details%ROWTYPE
     ,x_return_status         OUT NOCOPY     VARCHAR2)
   IS
      l_item_rec            mtl_system_items_b%ROWTYPE;
      l_status              NUMBER;
      l_subinventory        VARCHAR2(30);
      l_api_name   CONSTANT VARCHAR2 (30)   := 'validate_material_for_ins';
      l_val_proc            VARCHAR2 (100);
      val_error             EXCEPTION;
   BEGIN
      IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
        gme_debug.put_line('Entering api '||g_pkg_name||'.'||l_api_name);
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' batch_id='||p_batch_header_rec.batch_id);
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' batchstep_id='||p_batch_step_rec.batchstep_id);
      END IF;

      /* Set the return status to success initially */
      x_return_status       := FND_API.G_RET_STS_SUCCESS;

      -- set output structure
      x_material_detail_rec := p_material_detail_rec;

      x_material_detail_rec.batch_id             := p_batch_header_rec.batch_id;
      x_material_detail_rec.organization_id      := p_batch_header_rec.organization_id;
      x_material_detail_rec.formulaline_id       := NULL;

      -- Item_ID
      validate_item_id (p_org_id             => p_batch_header_rec.organization_id
                       ,p_item_id            => p_material_detail_rec.inventory_item_id
                       ,x_item_rec           => l_item_rec
                       ,x_return_status      => x_return_status);

      IF x_return_status <> fnd_api.g_ret_sts_success THEN
         l_val_proc := 'validate_item_id';
         RAISE val_error;
      END IF;
      --added by qzeng no need to check if done in bulk validation
      IF NVL(gme_common_pvt.g_bulk_validation_done, 'N') = 'N' THEN
        -- Revision
        validate_revision (p_item_rec           => l_item_rec
                          ,p_revision           => p_material_detail_rec.revision
                          ,x_return_status      => x_return_status);

        IF x_return_status <> fnd_api.g_ret_sts_success THEN
          l_val_proc := 'validate_revision';
          RAISE val_error;
        END IF;
      END IF;

      -- Line_Type
      validate_line_type (p_line_type          => p_material_detail_rec.line_type
                         ,x_return_status      => x_return_status);

      IF x_return_status <> fnd_api.g_ret_sts_success THEN
         l_val_proc := 'validate_line_type';
         RAISE val_error;
      END IF;

      -- ByProduct_Type
      IF p_material_detail_rec.line_type = gme_common_pvt.g_line_type_byprod THEN
         --added by qzeng no need to check if done in bulk validation
         IF NVL(gme_common_pvt.g_bulk_validation_done, 'N') = 'N' THEN
           validate_byproduct_type
                    (p_byproduct_type      => p_material_detail_rec.by_product_type
                    ,x_return_status       => x_return_status);

           IF x_return_status <> fnd_api.g_ret_sts_success THEN
              l_val_proc := 'validate_byproduct_type';
              RAISE val_error;
           END IF;
	 END IF;
      ELSE
         x_material_detail_rec.by_product_type := NULL;
      END IF;

      -- Line_no
      validate_line_no (
        p_line_no            => p_material_detail_rec.line_no
       ,p_line_type          => p_material_detail_rec.line_type
       ,p_batch_id           => p_batch_header_rec.batch_id
       ,x_line_no            => x_material_detail_rec.line_no
       ,x_return_status      => x_return_status);

      IF x_return_status <> fnd_api.g_ret_sts_success THEN
         l_val_proc := 'validate_line_no';
         RAISE val_error;
      END IF;

      -- Dtl_UM
      validate_dtl_um (p_dtl_um             => p_material_detail_rec.dtl_um
                      ,p_primary_uom        => l_item_rec.primary_uom_code
                      ,p_item_id            => p_material_detail_rec.inventory_item_id
                      ,p_org_id             => x_material_detail_rec.organization_id
                      ,x_return_status      => x_return_status);

      IF x_return_status <> fnd_api.g_ret_sts_success THEN
         l_val_proc := 'validate_dtl_um';
         RAISE val_error;
      END IF;

      /* Bug#5078853 moved this code over here */
      -- Release_Type
      -- following should be validated in form
      IF p_material_detail_rec.line_type = gme_common_pvt.g_line_type_byprod AND
            p_material_detail_rec.by_product_type = 'S' THEN
         x_material_detail_rec.release_type := 1;
      ELSIF p_material_detail_rec.release_type IS NULL THEN
         x_material_detail_rec.release_type := gme_common_pvt.g_release_type;
      ELSE
         -- added by qzeng no need to check if done in bulk validation
         IF NVL(gme_common_pvt.g_bulk_validation_done, 'N') = 'N' THEN
            validate_release_type
                          (p_material_detail_rec      => p_material_detail_rec
                          ,p_release_type             => p_material_detail_rec.release_type
                          ,x_return_status            => x_return_status);
            
            IF x_return_status <> fnd_api.g_ret_sts_success THEN
               l_val_proc := 'validate_release_type';
               RAISE val_error;
            END IF;
	      END IF;
      END IF;

      -- Calculate status of material based on release type, association and batch/step status
      l_status := p_batch_header_rec.batch_status;
      IF p_material_detail_rec.release_type = gme_common_pvt.g_mtl_autobystep_release AND
         p_batch_step_rec.batchstep_id IS NOT NULL THEN
         l_status := p_batch_step_rec.step_status;
      END IF;

      -- Plan_Qty
      IF l_status = gme_common_pvt.g_batch_pending THEN
         -- Bug 21480679 - default the plan qty in case it comes in as NULL.
         x_material_detail_rec.plan_qty := NVL(p_material_detail_rec.plan_qty, 0);
      
         validate_plan_qty (p_plan_qty           => p_material_detail_rec.plan_qty
                           ,x_return_status      => x_return_status);

         IF x_return_status <> fnd_api.g_ret_sts_success THEN
            l_val_proc := 'validate_plan_qty';
            RAISE val_error;
         END IF;
      ELSE
         x_material_detail_rec.plan_qty := 0;
      END IF;

      -- WIP_Plan_Qty
      -- Bug 21480679 - to be consistent with the form and release batch, we must allow wip plan qty
      --            even of the step is not released.
      IF l_status <> gme_common_pvt.g_batch_wip AND 
         p_batch_header_rec.batch_status <> gme_common_pvt.g_batch_wip THEN 
         x_material_detail_rec.wip_plan_qty := NULL;
      ELSE
        validate_wip_plan_qty
                        (p_wip_plan_qty       => p_material_detail_rec.wip_plan_qty
                        ,x_return_status      => x_return_status);
        --sunitha ch. bug rework#5333521 assign 0 to wip_plan_qty if it is NULL
        IF p_material_detail_rec.wip_plan_qty IS NULL THEN
          x_material_detail_rec.wip_plan_qty:=0;
        END IF;
        IF x_return_status <> fnd_api.g_ret_sts_success THEN
           l_val_proc := 'validate_wip_plan_qty';
           RAISE val_error;
        END IF;
      END IF;

      -- Actual_Qty
      IF p_material_detail_rec.actual_qty IS NULL OR
         p_material_detail_rec.actual_qty = 0  THEN
        
         x_material_detail_rec.actual_qty := 0;
      ELSE
         IF l_status NOT IN ( gme_common_pvt.g_batch_wip,
                               gme_common_pvt.g_batch_completed ) THEN
           gme_common_pvt.log_message ('GME_INV_STAT_UPD_ACT');
           RAISE val_error;
         END IF;
         validate_actual_qty (p_actual_qty               => x_material_detail_rec.actual_qty
                             ,x_return_status            => x_return_status);

         IF x_return_status <> fnd_api.g_ret_sts_success THEN
            l_val_proc := 'validate_actual_qty';
            RAISE val_error;
         END IF;
      END IF;


     /* Bug#5078853 modified validation for scrap factor */
     -- Scrap_Factor
     IF p_batch_header_rec.batch_status <> gme_common_pvt.g_batch_pending OR
        p_material_detail_rec.scrap_factor IS NULL OR
        p_material_detail_rec.scrap_factor = 0 OR
        p_material_detail_rec.line_type <> gme_common_pvt.g_line_type_ing THEN
        x_material_detail_rec.scrap_factor := 0;
     ELSE
        validate_scrap_factor(p_scrap              => p_material_detail_rec.scrap_factor
                             ,x_return_status      => x_return_status);

        IF x_return_status <> fnd_api.g_ret_sts_success THEN
           l_val_proc := 'validate_scrap_factor';
           RAISE val_error;
        END IF;

        x_material_detail_rec.scrap_factor := p_material_detail_rec.scrap_factor / 100;
        /* nsinghi Bug4911461 Re-work. Modify plan qty to include scrap. */
           x_material_detail_rec.plan_qty := x_material_detail_rec.plan_qty + 
                  (x_material_detail_rec.scrap_factor * x_material_detail_rec.plan_qty);

     END IF;


      -- Scale_Type
      validate_scale_type (p_scale_type               => p_material_detail_rec.scale_type
                          ,x_return_status            => x_return_status);

      IF x_return_status <> fnd_api.g_ret_sts_success THEN
         l_val_proc := 'validate_scale_type';
         RAISE val_error;
      END IF;

      IF p_material_detail_rec.scale_type = 2 THEN  -- integer scaling
        -- Scale_Multiple
        validate_scale_multiple
                        (p_scale_mult         => p_material_detail_rec.scale_multiple
                        ,x_return_status      => x_return_status);

        IF x_return_status <> fnd_api.g_ret_sts_success THEN
           l_val_proc := 'validate_scale_multiple';
           RAISE val_error;
        END IF;

        -- Scale_Rounding_Variance
        validate_scale_round_var
                (p_scale_var          => p_material_detail_rec.scale_rounding_variance
                ,x_return_status      => x_return_status);

        IF x_return_status <> fnd_api.g_ret_sts_success THEN
           l_val_proc := 'validate_scale_round_var';
           RAISE val_error;
        END IF;

        x_material_detail_rec.scale_rounding_variance :=
            p_material_detail_rec.scale_rounding_variance / 100;

        -- Rounding_Direction
        validate_rounding_direction
                     (p_round_dir          => p_material_detail_rec.rounding_direction
                     ,x_return_status      => x_return_status);

        IF x_return_status <> fnd_api.g_ret_sts_success THEN
           l_val_proc := 'validate_rounding_direction';
           RAISE val_error;
        END IF;
      ELSE
        x_material_detail_rec.scale_multiple               := NULL;
        x_material_detail_rec.scale_rounding_variance      := NULL;
        x_material_detail_rec.rounding_direction           := NULL;
      END IF;

      -- Cost_Alloc
      IF x_material_detail_rec.line_type = gme_common_pvt.g_line_type_prod THEN
         -- validate 0 <= cost_alloc <= 1
         -- at save_batch, check that sum(cost_alloc for all products) <= 1
         validate_cost_alloc
                           (p_material_detail_rec  => p_material_detail_rec
                           ,x_return_status      => x_return_status);

         IF x_return_status <> fnd_api.g_ret_sts_success THEN
            l_val_proc := 'validate_cost_alloc';
            RAISE val_error;
         END IF;
      ELSE
         x_material_detail_rec.cost_alloc := NULL;
      END IF;

      -- Phantom_Type
      IF p_batch_header_rec.batch_status = gme_common_pvt.g_batch_pending AND
         x_material_detail_rec.line_type = gme_common_pvt.g_line_type_ing THEN
         validate_phantom_type
                       (p_phantom_type       => p_material_detail_rec.phantom_type
                       ,x_return_status      => x_return_status);

         IF x_return_status <> fnd_api.g_ret_sts_success THEN
           l_val_proc := 'validate_phantom_type';
           RAISE val_error;
         END IF;
      ELSE
         x_material_detail_rec.phantom_type := 0;
      END IF;

      -- Contribute_Yield_Ind
      IF p_material_detail_rec.line_type = gme_common_pvt.g_line_type_ing THEN
         validate_contr_yield_ind
            (p_contr_yield_ind      => p_material_detail_rec.contribute_yield_ind
            ,x_return_status        => x_return_status);

         IF x_return_status <> fnd_api.g_ret_sts_success THEN
            l_val_proc := 'validate_contr_yield_ind';
            RAISE val_error;
         END IF;
      ELSE
         x_material_detail_rec.contribute_yield_ind := 'Y';
      END IF;

      -- Contribute_Step_Qty_Ind
      IF p_batch_step_rec.batchstep_id IS NOT NULL THEN
         validate_contr_step_qty_ind
            (p_contr_step_qty_ind      => p_material_detail_rec.contribute_step_qty_ind
            ,x_return_status           => x_return_status);

         IF x_return_status <> fnd_api.g_ret_sts_success THEN
            l_val_proc := 'validate_contr_step_qty_ind';
            RAISE val_error;
         END IF;
      ELSE
         x_material_detail_rec.contribute_step_qty_ind := NULL;
      END IF;

      -- Subinventory and Locator
      IF p_material_detail_rec.subinventory = fnd_api.g_miss_char THEN
         x_material_detail_rec.subinventory := NULL;
         x_material_detail_rec.locator_id := NULL;
      ELSIF p_material_detail_rec.subinventory IS NULL THEN
         --Bug#5078853 Begin get the default sub inv and locator either from gme_parameters or from Item Master
         IF p_material_detail_rec.line_type = gme_common_pvt.g_line_type_ing THEN
            gme_common_pvt.get_supply_defaults ( p_organization_id   => x_material_detail_rec.organization_id 
                                                ,p_inventory_item_id => p_material_detail_rec.inventory_item_id
                                                ,x_subinventory      => x_material_detail_rec.subinventory
                                                ,x_locator_id        => x_material_detail_rec.locator_id
                                                ,x_return_status     => x_return_status );
            IF x_return_status <> fnd_api.g_ret_sts_success THEN
             l_val_proc := 'get_supply_defaults';
             RAISE val_error;
            END IF;             
         ELSE
            gme_common_pvt.get_yield_defaults ( p_organization_id   => x_material_detail_rec.organization_id 
                                               ,p_inventory_item_id => p_material_detail_rec.inventory_item_id
                                               ,p_line_type         => p_material_detail_rec.line_type
                                               ,x_subinventory      => x_material_detail_rec.subinventory
                                               ,x_locator_id        => x_material_detail_rec.locator_id
                                               ,x_return_status     => x_return_status );
            IF x_return_status <> fnd_api.g_ret_sts_success THEN
             l_val_proc := 'get_yield_defaults';
             RAISE val_error;
            END IF; 
         END IF;
         --Bug#5078853 End
      ELSE                                         -- subinventory is not NULL
         validate_subinventory
                          (p_item_rec           => l_item_rec
                          ,p_subinv             => x_material_detail_rec.subinventory
                          ,x_return_status      => x_return_status);

         IF x_return_status <> fnd_api.g_ret_sts_success THEN
            l_val_proc := 'validate_subinventory';
            RAISE val_error;
         END IF;

         IF p_material_detail_rec.locator_id = fnd_api.g_miss_num THEN
            x_material_detail_rec.locator_id := NULL;
         ELSIF p_material_detail_rec.locator_id IS NULL THEN
            /* Bug#5078853 Begin if we come here then locator id is not being passed from public api
               so get the default locator id to validate with passed sub inv */
            IF p_material_detail_rec.line_type = gme_common_pvt.g_line_type_ing THEN
                gme_common_pvt.get_supply_defaults (  p_organization_id   => x_material_detail_rec.organization_id 
                                                     ,p_inventory_item_id => p_material_detail_rec.inventory_item_id
                                                     ,x_subinventory      => l_subinventory
                                                     ,x_locator_id        => x_material_detail_rec.locator_id
                                                     ,x_return_status     => x_return_status );
                IF x_return_status <> fnd_api.g_ret_sts_success THEN
                  l_val_proc := 'get_supply_defaults';
                  RAISE val_error;
                END IF;
            ELSE
               gme_common_pvt.get_yield_defaults (  p_organization_id   => x_material_detail_rec.organization_id 
                                                   ,p_inventory_item_id => p_material_detail_rec.inventory_item_id
                                                   ,p_line_type         => p_material_detail_rec.line_type
                                                   ,x_subinventory      => l_subinventory
                                                   ,x_locator_id        => x_material_detail_rec.locator_id
                                                   ,x_return_status     => x_return_status );
                IF x_return_status <> fnd_api.g_ret_sts_success THEN
                  l_val_proc := 'get_yield_defaults';
                  RAISE val_error;
                END IF;
            END IF;
            --Bug#5078853 End
         END IF;

         IF x_material_detail_rec.locator_id IS NOT NULL THEN
                              -- subinventory is not NULL, locator is not null
            validate_locator
                          (p_subinv             => x_material_detail_rec.subinventory
                          ,p_locator_id         => x_material_detail_rec.locator_id
                          ,p_item_rec           => l_item_rec
                          ,p_line_type          => x_material_detail_rec.line_type
                          ,x_return_status      => x_return_status);

            IF x_return_status <> fnd_api.g_ret_sts_success THEN
               l_val_proc := 'validate_locator';
               RAISE val_error;
            END IF;
         END IF;        -- IF x_material_detail_rec.locator_id IS NOT NULL ...
      END IF;                   -- IF x_material_detail_rec.subinventory = ...

      /* Bug#5078853 added the following call for flex field validation
       gme_common_pvt.g_flex_validate_prof has to be set in public API to enforce flex field validation */
      gme_validate_flex_fld_pvt.validate_flex_material_details
                      ( p_material_detail_rec   => p_material_detail_rec
                       ,x_material_detail_rec   => x_material_detail_rec
                       ,x_return_status         => x_return_status);
      IF x_return_status <> fnd_api.g_ret_sts_success THEN
       l_val_proc := 'validate_flex_material_details';
       RAISE val_error;
      END IF;

      x_material_detail_rec.move_order_line_id := NULL;
      x_material_detail_rec.phantom_id := NULL;
      x_material_detail_rec.phantom_line_id := NULL;
      x_material_detail_rec.backordered_qty := 0;
      x_material_detail_rec.original_qty := x_material_detail_rec.plan_qty;

      get_converted_qty
                (p_org_id                    => x_material_detail_rec.organization_id
                ,p_item_id                   => x_material_detail_rec.inventory_item_id
                ,p_lot_number                => NULL
                ,p_qty                       => x_material_detail_rec.original_qty
                ,p_from_um                   => x_material_detail_rec.dtl_um
                ,p_to_um                     => l_item_rec.primary_uom_code
                ,x_conv_qty                  => x_material_detail_rec.original_primary_qty
                ,x_return_status             => x_return_status);

      IF x_return_status <> fnd_api.g_ret_sts_success THEN
        l_val_proc := 'get_converted_qty';
        RAISE val_error;
      END IF;

      IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
        gme_debug.put_line (g_pkg_name||'.'||l_api_name||' original_primary_qty= '
                                                       || x_material_detail_rec.original_primary_qty);
      END IF;

      IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
         gme_debug.put_line('Exiting api '||g_pkg_name||'.'||l_api_name);
      END IF;
   EXCEPTION
   WHEN val_error THEN
     IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
       gme_debug.put_line (g_pkg_name||'.'||l_api_name||': validation error from proc: '|| l_val_proc);
     END IF;
     x_return_status := FND_API.g_ret_sts_error;
   WHEN OTHERS THEN
     fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
     IF (NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT) THEN
       gme_debug.put_line ('Unexpected error: '||g_pkg_name||'.'||l_api_name||': '||SQLERRM);
     END IF;
     x_return_status := FND_API.g_ret_sts_unexp_error;
   END validate_material_for_ins;

   PROCEDURE open_and_process_actual_qty (
      p_batch_header_rec      IN       gme_batch_header%ROWTYPE
     ,p_material_detail_rec   IN       gme_material_details%ROWTYPE
     ,p_batch_step_rec        IN       gme_batch_steps%ROWTYPE DEFAULT NULL
     ,p_trans_id              IN       NUMBER
     ,p_insert                IN       VARCHAR2
     ,x_transacted            OUT NOCOPY     VARCHAR2
     ,x_return_status         OUT NOCOPY     VARCHAR2)
   IS
      CURSOR item_no_cursor (v_org_id NUMBER, v_inventory_item_id NUMBER)
      IS
         SELECT concatenated_segments
           FROM mtl_system_items_kfv
          WHERE inventory_item_id = v_inventory_item_id
            AND organization_id = v_org_id;

      l_item_no                     mtl_system_items_kfv.concatenated_segments%TYPE;
      l_trans_id                    NUMBER;
      l_item_rec                    mtl_system_items_b%ROWTYPE;

      open_actual_qty_error         EXCEPTION;
      open_actual_qty_unexp_error   EXCEPTION;
      process_actual_qty_error      EXCEPTION;
      error_get_rec                 EXCEPTION;

      l_api_name           CONSTANT VARCHAR2 (30)     := 'open_and_process_actual_qty';
      l_field_name         CONSTANT VARCHAR2 (20)     := 'actual_qty';


   BEGIN
      IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
        gme_debug.put_line('Entering api '||g_pkg_name||'.'||l_api_name);
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' batch_id='||p_batch_header_rec.batch_id);
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' material_detail_id='||p_material_detail_rec.material_detail_id);
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' batchstep_id='||p_batch_step_rec.batchstep_id);
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' actual_qty='||p_material_detail_rec.actual_qty);
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_trans_id='||p_trans_id);
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_insert='||p_insert);
      END IF;

      /* Set the return status to success initially */
      x_return_status := FND_API.G_RET_STS_SUCCESS;
      x_transacted := FND_API.g_false;

      get_item_rec (p_org_id             => p_batch_header_rec.organization_id
                   ,p_item_id            => p_material_detail_rec.inventory_item_id
                   ,x_item_rec           => l_item_rec
                   ,x_return_status      => x_return_status);

      IF x_return_status <> fnd_api.g_ret_sts_success THEN
         RAISE error_get_rec;
      END IF;

      IF p_trans_id IS NULL THEN
        /* Bug 5441643 Added NVL condition for location control code*/      	
        l_trans_id :=
           open_actual_qty (p_material_detail_rec      => p_material_detail_rec
                           ,p_batch_status             => p_batch_header_rec.batch_status
                           ,p_update_inventory_ind     => p_batch_header_rec.update_inventory_ind
                           ,p_batchstep_id             => p_batch_step_rec.batchstep_id
                           ,p_step_status              => p_batch_step_rec.step_status
                           ,p_lot_control_code         => l_item_rec.lot_control_code
                           ,p_location_control_code    => NVL(l_item_rec.location_control_code,1)
                           ,p_restrict_locators_code   => l_item_rec.restrict_locators_code
                           ,p_insert                   => p_insert);
      ELSE
        l_trans_id := p_trans_id;
      END IF;

      IF l_trans_id = -1 THEN
         IF p_material_detail_rec.actual_qty = 0 THEN
           -- not an error because actual quantity is 0 when the field is closed
           NULL;
         ELSE
           IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
              gme_debug.put_line(g_pkg_name||'.'||l_api_name||': open_actual_qty returned -1; can not update actual qty');
           END IF;
           RAISE open_actual_qty_error;
         END IF;
      ELSIF l_trans_id = -2 THEN
         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line(g_pkg_name||'.'||l_api_name||': open_actual_qty returned -2 raised unexpected error');
         END IF;
         RAISE open_actual_qty_unexp_error;
      END IF;

      IF p_batch_header_rec.update_inventory_ind = 'Y' AND
         (l_trans_id > 0 OR
          (l_trans_id = 0 AND p_material_detail_rec.actual_qty > 0)) THEN
        process_actual_qty (p_batch_header_rec         => p_batch_header_rec
                           ,p_material_detail_rec      => p_material_detail_rec
                           ,p_batch_step_rec           => p_batch_step_rec
                           ,p_trans_id                 => l_trans_id
                           ,p_item_rec                 => l_item_rec
                           ,x_return_status            => x_return_status);

        IF x_return_status <> fnd_api.g_ret_sts_success THEN
          RAISE process_actual_qty_error;
        END IF;

        x_transacted := FND_API.g_true;
      END IF;

      IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
         gme_debug.put_line('Exiting api '||g_pkg_name||'.'||l_api_name);
      END IF;

   EXCEPTION
      WHEN error_get_rec THEN
         NULL;
      WHEN open_actual_qty_error THEN
         OPEN item_no_cursor (p_batch_header_rec.organization_id, l_item_rec.inventory_item_id);
         FETCH item_no_cursor INTO l_item_no;
         CLOSE item_no_cursor;

         gme_common_pvt.log_message ('GME_UPD_ACTUAL_QTY_ERR'
                                    ,'ITEM_NO'
                                    ,l_item_no);
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN open_actual_qty_unexp_error THEN
         x_return_status := fnd_api.g_ret_sts_unexp_error;
      WHEN process_actual_qty_error THEN
         NULL;
      WHEN OTHERS THEN
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);

         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line (   'Unexpected error: '
                                || g_pkg_name
                                || '.'
                                || l_api_name
                                || ': '
                                || SQLERRM);
         END IF;

         x_return_status := fnd_api.g_ret_sts_unexp_error;
   END open_and_process_actual_qty;

   /* **********************
    * open_actual_qty returns following:
    * -2 => unexpected error: caller should raise unexpected error
    * -1 => open actual qty: No
    * 0  => open actual qty: Yes with no transactions; must create a transaction
    * >0 => open actual qty: Yes with transaction id returned; must update existing transaction
    * **********************
    */
   FUNCTION open_actual_qty (
      p_material_detail_rec   IN   gme_material_details%ROWTYPE
     ,p_batch_status          IN   NUMBER
     ,p_update_inventory_ind  IN   VARCHAR2
     ,p_batchstep_id          IN   NUMBER DEFAULT NULL
     ,p_step_status           IN   NUMBER DEFAULT NULL
     ,p_lot_control_code      IN   NUMBER DEFAULT NULL
     ,p_location_control_code IN   NUMBER DEFAULT NULL
     ,p_restrict_locators_code IN  NUMBER DEFAULT NULL
     ,p_insert                IN   VARCHAR2)
      RETURN NUMBER
   IS
      CURSOR cur_get_trans (v_material_detail_id NUMBER, v_batch_id NUMBER)
      IS
         SELECT transaction_id
           FROM mtl_material_transactions
          WHERE transaction_source_id = v_batch_id
            AND trx_source_line_id = v_material_detail_id
            AND transaction_id NOT IN (
                   SELECT transaction_id1
                     FROM gme_transaction_pairs
                    WHERE batch_id = v_batch_id
                      AND material_detail_id = v_material_detail_id
                      AND pair_type = gme_common_pvt.g_pairs_reversal_type);

      CURSOR cur_get_mtl_trxn_lot (v_trans_id NUMBER)
      IS
         SELECT COUNT (1)
           FROM mtl_transaction_lot_numbers
          WHERE transaction_id = v_trans_id;

      CURSOR cur_sub_control (v_org_id NUMBER, v_subinventory VARCHAR2)
      IS
         SELECT locator_type
           FROM mtl_secondary_inventories
          WHERE organization_id = v_org_id
            AND secondary_inventory_name = v_subinventory;
      /* Bug 5441643 Added NVL condition for location control code*/
      /* Bug 5681997 Added mtl_transactions_enabled_flag from item master */
      CURSOR cur_get_item_attrib (v_org_id NUMBER, v_item_id NUMBER) IS
         SELECT lot_control_code, NVL(location_control_code, 1) location_control_code, restrict_locators_code,
                mtl_transactions_enabled_flag
           FROM mtl_system_items_b
          WHERE organization_id = v_org_id
            AND inventory_item_id = v_item_id;

      CURSOR cur_get_step_status (v_step_id NUMBER)
      IS
         SELECT step_status
           FROM gme_batch_steps
          WHERE batchstep_id = v_step_id;
--Bug#5129153 To find out if the item is revision controlled or not Start.
      CURSOR cur_get_rev_code (v_org_id NUMBER,v_item_id NUMBER)
      IS
         SELECT revision_qty_control_code
           FROM mtl_system_items_b
          WHERE organization_id = v_org_id
            AND inventory_item_id = v_item_id;
--Bug#5129153 End.
      l_count_trans          NUMBER;
      l_step_status          NUMBER;
      l_trans_id             NUMBER;
      l_mtl_dtl_rec          gme_material_details%ROWTYPE;
      l_count_lot_trans      NUMBER;
      l_release_type         NUMBER;
      l_sub_locator_type     NUMBER;
      l_txn_action           NUMBER;
      l_eff_locator_control  NUMBER;
      l_lot_control_code     NUMBER;
      l_location_control_code NUMBER;
      l_restrict_locators_code NUMBER;
      l_mtl_txn_enabled_flag VARCHAR2(1);
--Bug#5129153
      l_rev_code             NUMBER;
      l_api_name   CONSTANT  VARCHAR2 (30)                 := 'open_actual_qty';

   BEGIN

      IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
        gme_debug.put_line('Entering api '||g_pkg_name||'.'||l_api_name);
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' batch_id='||p_material_detail_rec.batch_id);
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' material_detail_id='||p_material_detail_rec.material_detail_id);
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_batch_status='||p_batch_status);
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_update_inventory_ind='||p_update_inventory_ind);
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_batchstep_id='||p_batchstep_id);
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_step_status='||p_step_status);
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_lot_control_code='||p_lot_control_code);
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_location_control_code='||p_location_control_code);
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_restrict_locators_code='||p_restrict_locators_code);
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_insert='||p_insert);
      END IF;

      l_mtl_dtl_rec := p_material_detail_rec;

      /* Auto, manual, incremental or auto by step with following number of transactions...
       * subinventory and locator can be specified on the material detail line

       * plain and subinv specified - 0 or 1 transaction
       * locator and locator specifed - 0 or 1 transaction

       * plain and subinv not specified - 1 transaction
       * locator and locator not specifed - 1 transaction

       * lot - 1 transaction
       */

      l_release_type := l_mtl_dtl_rec.release_type;
      IF l_release_type = gme_common_pvt.g_mtl_autobystep_release AND
         p_batchstep_id IS NULL THEN
        l_release_type := gme_common_pvt.g_mtl_auto_release;
      END IF;

      -- if it's auto by step, ensure we have the step status...
      IF l_release_type = gme_common_pvt.g_mtl_autobystep_release THEN
        IF p_step_status IS NULL THEN
          OPEN cur_get_step_status(p_batchstep_id);
          FETCH cur_get_step_status INTO l_step_status;
          CLOSE cur_get_step_status;
        ELSE
          l_step_status := p_step_status;
        END IF;
      END IF;

      -- Not open for products/byproducts that are not in completed state (auto) or
      -- wip/completed (manual/incremental)

      IF     l_mtl_dtl_rec.line_type IN
                (gme_common_pvt.g_line_type_prod
                ,gme_common_pvt.g_line_type_byprod)
         AND (    (l_release_type = gme_common_pvt.g_mtl_autobystep_release
                   AND l_step_status <> gme_common_pvt.g_step_completed)
              OR  (l_release_type  = gme_common_pvt.g_mtl_auto_release
                   AND p_batch_status <> gme_common_pvt.g_batch_completed)
              OR  (l_release_type IN (gme_common_pvt.g_mtl_manual_release,
                                      gme_common_pvt.g_mtl_incremental_release)                   
                   AND p_batch_status NOT IN (gme_common_pvt.g_batch_wip
                                          ,gme_common_pvt.g_batch_completed))
                                       ) THEN
         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line
                    (   g_pkg_name
                     || '.'
                     || l_api_name
                     || 'matl_dtl_id= '
                     || TO_CHAR (l_mtl_dtl_rec.material_detail_id)
                     || ': return -1: product/byproduct not in correct state');
         END IF;

         RETURN -1;
      END IF;

      -- Not open for ingredients that are not in WIP or completed state
      IF     l_mtl_dtl_rec.line_type = gme_common_pvt.g_line_type_ing
         AND (    (l_release_type = gme_common_pvt.g_mtl_autobystep_release
                   AND l_step_status NOT IN
                          (gme_common_pvt.g_step_wip
                          ,gme_common_pvt.g_step_completed) )
              -- following for manual, incremental and auto
                   OR  (p_batch_status NOT IN
                          (gme_common_pvt.g_batch_wip
                          ,gme_common_pvt.g_batch_completed) ) ) THEN                        
         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line
                    (   g_pkg_name
                     || '.'
                     || l_api_name
                     || 'matl_dtl_id= '
                     || TO_CHAR (l_mtl_dtl_rec.material_detail_id)
                     || ': return -1: ingredient not in wip or completed state');
         END IF;

         RETURN -1;
      END IF;

      -- if this is an insert, no need to go frther because there are no transactions...
      -- return 0 to indicate no transactions and that open is allowed

      /* FPbug#4543872 removed 'p_insert = fnd_api.G_TRUE OR' from IF condition
         because even if we are inserting new material line we need to check for other
         conditions like lot control, locator control, subinventory availaility etc. anyway
         no.of transactions will be zero for new material line that is being inserted
      */
      IF ( p_update_inventory_ind = 'N') THEN
        RETURN 0;
      END IF;

      OPEN cur_get_trans (l_mtl_dtl_rec.material_detail_id
                         ,l_mtl_dtl_rec.batch_id);

      FETCH cur_get_trans INTO l_trans_id;

      IF cur_get_trans%FOUND THEN
         l_count_trans := 1;

         FETCH cur_get_trans
          INTO l_trans_id;

         IF cur_get_trans%FOUND THEN
            l_count_trans := 2;
                   -- no need to continue to count how many; too many already
         END IF;
      ELSE
         -- if not found, set trans_id to 0, indicating that there are no transactions
         l_count_trans := 0;
         l_trans_id := 0;
      END IF;

      CLOSE cur_get_trans;

      IF l_count_trans = 2 THEN
         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line
               (   g_pkg_name
                || '.'
                || l_api_name
                || 'matl_dtl_id= '
                || TO_CHAR (l_mtl_dtl_rec.material_detail_id)
                || ': return -1: more than 1 trxn in mtl_material_transactions');
         END IF;

         RETURN -1;
      END IF;
      
      /* Bug 5681997 get value from cursor to variable */
      -- retrieve the item attributes

      OPEN cur_get_item_attrib(l_mtl_dtl_rec.organization_id, l_mtl_dtl_rec.inventory_item_id);
      FETCH cur_get_item_attrib INTO l_lot_control_code, l_location_control_code, l_restrict_locators_code, l_mtl_txn_enabled_flag;
      CLOSE cur_get_item_attrib;
      /* Bug 5681997 if item is not txn enabled then open actual qty */
      IF (l_mtl_txn_enabled_flag = 'N') THEN
      	RETURN 0;
      END IF;
      -- If it's lot control, ensure that there's only 1 transaction and there is only 1 entry
      -- in mtl_transaction_lots
      IF (l_lot_control_code = 2) THEN
         IF l_count_trans = 0 THEN
            IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
               gme_debug.put_line
                  (   g_pkg_name
                   || '.'
                   || l_api_name
                   || 'matl_dtl_id= '
                   || TO_CHAR (l_mtl_dtl_rec.material_detail_id)
                   || ': return -1: lot ctrl with 0 trxn in mtl_material_transactions');
            END IF;

            RETURN -1;
         ELSE
            OPEN cur_get_mtl_trxn_lot (l_trans_id);

            FETCH cur_get_mtl_trxn_lot
             INTO l_count_lot_trans;

            CLOSE cur_get_mtl_trxn_lot;

            -- check MTL_TRANSACTION_LOT_NUMBERS table; join on transaction_id = l_trans_id
            -- if there is more than 1 record there, then can't update these transactions
            -- in essence, this is more than 1 transaction.
            IF l_count_lot_trans > 1 THEN
               IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
                  gme_debug.put_line
                     (   g_pkg_name
                      || '.'
                      || l_api_name
                      || 'matl_dtl_id= '
                      || TO_CHAR (l_mtl_dtl_rec.material_detail_id)
                      || ': return -1: 1 trxn in mtl_material_transactions with more than 1 lot entry in mtl_transaction_lot_numbers'
                      || ' trans_id= '
                      || TO_CHAR (l_trans_id) );
               END IF;

               RETURN -1;
            ELSE
               IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
                  gme_debug.put_line
                     (   g_pkg_name
                      || '.'
                      || l_api_name
                      || 'matl_dtl_id= '
                      || TO_CHAR (l_mtl_dtl_rec.material_detail_id)
                      || ': return trans_id: success lot ctrl with 1 trxn: trans_id= '
                      || TO_CHAR (l_trans_id) );
               END IF;

               RETURN l_trans_id;
            END IF;
         END IF;
      END IF;

      -- At this point, the item is either plain or locator control... if there is 1 transaction,
      -- then actual qty can be updated, if there are none, then we have to check if matl has
      -- subinventory/locator specified on it in order to create the transaction
      IF l_count_trans = 1 THEN
         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line
               (   g_pkg_name
                || '.'
                || l_api_name
                || 'matl_dtl_id= '
                || TO_CHAR (l_mtl_dtl_rec.material_detail_id)
                || ': return trans_id: success plain or locator with 1 trxn: trans_id= '
                || TO_CHAR (l_trans_id) );
         END IF;

         RETURN l_trans_id;
      END IF;

      -- There are no transactions, so a transaction must be created
--Bug#5129153 Check if the item is revision controlled. Start.
      OPEN cur_get_rev_code(l_mtl_dtl_rec.organization_id,l_mtl_dtl_rec.inventory_item_id);
      FETCH cur_get_rev_code INTO l_rev_code;
      CLOSE cur_get_rev_code;
--Bug#5129153 End.
      IF l_mtl_dtl_rec.subinventory IS NULL THEN
         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line
               (   g_pkg_name
                || '.'
                || l_api_name
                || 'matl_dtl_id= '
                || TO_CHAR (l_mtl_dtl_rec.material_detail_id)
                || ': return -1: plain or locator; 0 transactions with subinventory on material NULL');
         END IF;

         RETURN -1;
      ELSE

         OPEN cur_sub_control (l_mtl_dtl_rec.organization_id, l_mtl_dtl_rec.subinventory);
         FETCH cur_sub_control INTO l_sub_locator_type;
         CLOSE cur_sub_control;

         IF (l_mtl_dtl_rec.line_type = gme_common_pvt.g_line_type_ing) THEN
            l_txn_action := gme_common_pvt.g_ing_issue_txn_action;
         ELSIF (l_mtl_dtl_rec.line_type = gme_common_pvt.g_line_type_prod) THEN
            l_txn_action := gme_common_pvt.g_prod_comp_txn_action;
         ELSIF (l_mtl_dtl_rec.line_type = gme_common_pvt.g_line_type_byprod) THEN
            l_txn_action := gme_common_pvt.g_byprod_comp_txn_action;
         END IF;
--Bug#5129153 If item is revision controlled and revision field is NULL then return -1. Start.
         IF l_rev_code = 2 AND l_mtl_dtl_rec.revision IS NULL THEN
            IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line
               (   g_pkg_name
                || '.'
                || l_api_name
                || 'matl_dtl_id= '
                || TO_CHAR (l_mtl_dtl_rec.material_detail_id)
                || ': return -1: Plain item which is revision controlled but has revision field as NULL ');                
             END IF;       
             RETURN -1;
          END IF;
--Bug#5129153 End.
         /* Bug 5441643 Added NVL condition for location control code*/
         l_eff_locator_control :=
               gme_common_pvt.eff_locator_control
                        (p_organization_id        => l_mtl_dtl_rec.organization_id
                        ,p_org_control            => gme_common_pvt.g_org_locator_control
                        ,p_subinventory           => l_mtl_dtl_rec.subinventory
                        ,p_sub_control            => l_sub_locator_type
                        ,p_item_control           => NVL(l_location_control_code,1)
                        ,p_item_loc_restrict      => l_restrict_locators_code
                        ,p_action                 => l_txn_action);

         IF l_eff_locator_control = 1 THEN                         -- No locator control
            IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
               gme_debug.put_line
                  (   g_pkg_name
                   || '.'
                   || l_api_name
                   || 'matl_dtl_id= '
                   || TO_CHAR (l_mtl_dtl_rec.material_detail_id)
                   || ': return trans_id: success plain with 0 trxn and with subinv on matl specified: trans_id= '
                   || TO_CHAR (l_trans_id) );
            END IF;

            RETURN l_trans_id;
         ELSE                                               -- locator control
            -- ensure there is a locator_id on the material
            IF l_mtl_dtl_rec.locator_id IS NULL THEN
               IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
                  gme_debug.put_line
                     (   g_pkg_name
                      || '.'
                      || l_api_name
                      || 'matl_dtl_id= '
                      || TO_CHAR (l_mtl_dtl_rec.material_detail_id)
                      || ': return -1: locator ctrl with 0 transactions and locator_id on material NULL');
               END IF;

               RETURN -1;
            ELSE
               IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
                  gme_debug.put_line
                     (   g_pkg_name
                      || '.'
                      || l_api_name
                      || 'matl_dtl_id= '
                      || TO_CHAR (l_mtl_dtl_rec.material_detail_id)
                      || ': return trans_id: success locator with 0 trxn and with subinv/locator on matl specified: trans_id= '
                      || TO_CHAR (l_trans_id) );
               END IF;

               RETURN l_trans_id;
            END IF;
         END IF;
      END IF;

      -- shouldn't get to this point...
      IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
         gme_debug.put_line
              (   g_pkg_name
               || '.'
               || l_api_name
               || 'matl_dtl_id= '
               || TO_CHAR (l_mtl_dtl_rec.material_detail_id)
               || ': return -1: fall through all conditions; programming error; figure out why code got here');
      END IF;

      RETURN -1;
   -- -1 means you can't create a transaction; 0 means there are no transactions
   -- but you can create transactions; other is trans_id => 1 and only 1 trans exists
   EXCEPTION
      WHEN OTHERS THEN
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);

         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line (   'Unexpected error: '
                                || g_pkg_name
                                || '.'
                                || l_api_name
                                || ': '
                                || SQLERRM);
         END IF;

         RETURN -2;
   END open_actual_qty;

   PROCEDURE process_actual_qty (
      p_batch_header_rec      IN              gme_batch_header%ROWTYPE
     ,p_material_detail_rec   IN              gme_material_details%ROWTYPE
     ,p_batch_step_rec        IN              gme_batch_steps%ROWTYPE
            DEFAULT NULL
     ,p_trans_id              IN              NUMBER
     ,p_item_rec              IN              mtl_system_items_b%ROWTYPE
     ,x_return_status         OUT NOCOPY      VARCHAR2)
   IS

      l_api_name         CONSTANT VARCHAR2 (30)        := 'PROCESS_ACTUAL_QTY';

      l_mmt_rec                   mtl_material_transactions%ROWTYPE;
      l_mmln_tbl                  gme_common_pvt.mtl_trans_lots_num_tbl;

      l_mmti_rec                  mtl_transactions_interface%ROWTYPE;
      l_mmli_tbl                  gme_common_pvt.mtl_trans_lots_inter_tbl;

      l_cnt            NUMBER DEFAULT 0;
      l_secondary_qty  NUMBER;
      l_primary_qty    NUMBER;
      l_multiplier     NUMBER;

      error_trans                 EXCEPTION;

   BEGIN

      IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
        gme_debug.put_line('Entering api '||g_pkg_name||'.'||l_api_name);
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' batch_id='||p_batch_header_rec.batch_id);
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' material_detail_id='||p_material_detail_rec.material_detail_id);
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' batchstep_id='||p_batch_step_rec.batchstep_id);
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' trans_id='||p_trans_id);
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' actual_qty='||p_material_detail_rec.actual_qty);
      END IF;

      x_return_status := fnd_api.g_ret_sts_success;

      -- Begin Bug 13743650 Moved this piece of code up
      IF p_item_rec.secondary_uom_code IS NULL THEN
         l_secondary_qty := NULL;
      ELSE
         l_secondary_qty := p_material_detail_rec.actual_qty;
         IF p_material_detail_rec.dtl_um <> p_item_rec.secondary_uom_code THEN
            l_secondary_qty := 0;
            get_converted_qty (
                      p_org_id                    => p_material_detail_rec.organization_id
                     ,p_item_id                   => p_material_detail_rec.inventory_item_id
                     ,p_lot_number                => NULL
                     ,p_qty                       => p_material_detail_rec.actual_qty
                     ,p_from_um                   => p_material_detail_rec.dtl_um
                     ,p_to_um                     => p_item_rec.secondary_uom_code
                     ,x_conv_qty                  => l_secondary_qty
                     ,x_return_status             => x_return_status);
         END IF;
      END IF;

      -- If the new actual qty is zero we need to delete the transaction
      IF (p_material_detail_rec.actual_qty = 0 AND p_trans_id > 0) THEN

         gme_transactions_pvt.delete_material_txn
              (p_transaction_id       => p_trans_id
              ,p_txns_pair            => NULL
              ,x_return_status        => x_return_status);

         IF x_return_status <> fnd_api.g_ret_sts_success THEN
            IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
               gme_debug.put_line
                            (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ' return '
                             || x_return_status
                             || ' from gme_transactions_pvt.delete_material_txn');
            END IF;

            RAISE error_trans;
         END IF;

         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || 'deleted transaction for trans_id= '
                                || TO_CHAR (p_trans_id) );
         END IF;
      ELSE
         /* Bug 5681997 added condition to touch txns only if enabled */      	
         IF p_trans_id = 0 AND p_item_rec.mtl_transactions_enabled_flag = 'Y' THEN                             -- insert new txn
            -- Bug 12563379
            l_multiplier := 1;
            IF p_material_detail_rec.line_type = -1 THEN
               l_multiplier := -1;            
            END IF;
          
            -- Bug 12563379 - Let's see if a transaction already exists before inserting a new one.
            -- Use the qty check to exclude returns based on multiple overtypes.
            IF (l_multiplier < 0) THEN
               SELECT count(*)
               INTO l_cnt
               FROM mtl_material_transactions_temp
               WHERE transaction_source_id = p_batch_header_rec.batch_id
               AND trx_source_line_id = p_material_detail_rec.material_detail_id
               AND transaction_quantity < 0;
            ELSE
               SELECT count(*)
               INTO l_cnt
               FROM mtl_material_transactions_temp
               WHERE transaction_source_id = p_batch_header_rec.batch_id
               AND trx_source_line_id = p_material_detail_rec.material_detail_id
               AND transaction_quantity > 0;          
            END IF;
            
            IF l_cnt = 1 THEN
               -- Let's update the existing transaction in the temp table.

               -- 13976194 handle primary qty also.               
               l_primary_qty := p_material_detail_rec.actual_qty;
               IF p_material_detail_rec.dtl_um <> p_item_rec.primary_uom_code THEN
                  l_primary_qty := 0;
                  get_converted_qty (
                            p_org_id                    => p_material_detail_rec.organization_id
                           ,p_item_id                   => p_material_detail_rec.inventory_item_id
                           ,p_lot_number                => NULL
                           ,p_qty                       => p_material_detail_rec.actual_qty
                           ,p_from_um                   => p_material_detail_rec.dtl_um
                           ,p_to_um                     => p_item_rec.primary_uom_code
                           ,x_conv_qty                  => l_primary_qty
                           ,x_return_status             => x_return_status);
               END IF;

               IF l_secondary_qty IS NOT NULL THEN
                  l_secondary_qty := l_multiplier * l_secondary_qty;
               END IF;

               -- 13976194 handle primary qty also.
               IF (l_multiplier < 0) THEN
                  UPDATE mtl_material_transactions_temp
                     SET transaction_quantity = (l_multiplier * p_material_detail_rec.actual_qty),
                         primary_quantity = (l_multiplier * l_primary_qty),
                         secondary_transaction_quantity = l_secondary_qty
                   WHERE transaction_source_type_id = 5
                     AND transaction_source_id = p_batch_header_rec.batch_id
                     AND trx_source_line_id = p_material_detail_rec.material_detail_id
                     AND transaction_quantity < 0;
               ELSE
                  UPDATE mtl_material_transactions_temp
                     SET transaction_quantity = (l_multiplier * p_material_detail_rec.actual_qty),
                         primary_quantity = (l_multiplier * l_primary_qty),
                         secondary_transaction_quantity = l_secondary_qty
                   WHERE transaction_source_type_id = 5
                     AND transaction_source_id = p_batch_header_rec.batch_id
                     AND trx_source_line_id = p_material_detail_rec.material_detail_id               
                     AND transaction_quantity > 0;
               END IF;
                    
               -- END Bug 12563379
            ELSE            
               -- construct new transaction; will be plain or locator
               construct_trans_row
                                (p_matl_dtl_rec             => p_material_detail_rec
                                ,p_item_rec                 => p_item_rec
                                ,p_batch_hdr_rec            => p_batch_header_rec
                                ,p_batch_step_rec           => p_batch_step_rec
                                ,x_mmti_rec                 => l_mmti_rec
                                ,x_return_status            => x_return_status);
               
               gme_transactions_pvt.create_material_txn
                  (p_mmti_rec             => l_mmti_rec
                  ,p_mmli_tbl             => l_mmli_tbl
                  ,p_phantom_trans        => 0
                  ,x_return_status        => x_return_status);
               
               IF x_return_status <> fnd_api.g_ret_sts_success THEN
                  IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
                     gme_debug.put_line
                               (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ' return '
                                || x_return_status
                                || ' from gme_transactions_pvt.create_material_txn');
                  END IF;
               
                  RAISE error_trans;
               END IF;
               
               --FPbug#4543872
               IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
                   gme_debug.put_line
                               (   g_pkg_name
                                || '.'
                                || l_api_name
                                ||' transaction header id after create mtl txn'
                                 || gme_common_pvt.g_transaction_header_id);
               END IF; 
            END IF;                  
         ELSE
           /* Bug 5681997 added condition to touch txns only if enabled */
           IF p_item_rec.mtl_transactions_enabled_flag = 'Y' THEN
             -- Actual qty is non zero. Need to update the transaction.
             gme_transactions_pvt.get_transactions
               (p_transaction_id       => p_trans_id
               ,x_mmt_rec              => l_mmt_rec
               ,x_mmln_tbl             => l_mmln_tbl
               ,x_return_status        => x_return_status);
             IF x_return_status <> FND_API.G_RET_STS_SUCCESS THEN
               IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
                  gme_debug.put_line(   g_pkg_name|| '.'|| l_api_name|| ' return '|| x_return_status|| ' from gme_transactions_pvt.get_transactions');
               END IF;
               RAISE error_trans;
             END IF;
             l_mmt_rec.transaction_quantity := p_material_detail_rec.actual_qty;
             -- Bug 13743650 - Assign secondary qty with a value instead of NULL.
             l_mmt_rec.secondary_transaction_quantity := l_secondary_qty;
             
             --FPbug#4543872 Added IF condition to check the count
             IF l_mmln_tbl.COUNT > 0 THEN
               IF l_mmln_tbl(1).lot_number IS NOT NULL THEN

                 -- Bug 11939155 - Initialize primary to null
                 l_mmt_rec.primary_quantity := NULL;
                 l_mmln_tbl(1).primary_quantity := NULL;
               
                 l_mmln_tbl(1).transaction_quantity := p_material_detail_rec.actual_qty;
                 
                  -- Bug 13743650 - Assign secondary qty with a value instead of NULL.
                 l_mmln_tbl(1).secondary_transaction_quantity := l_secondary_qty;
               END IF;
             END IF;
             gme_transactions_pvt.update_material_txn
              (p_mmt_rec         => l_mmt_rec
              ,p_mmln_tbl        => l_mmln_tbl
              ,x_return_status   => x_return_status);

             IF x_return_status <> FND_API.G_RET_STS_SUCCESS THEN
               IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
                 gme_debug.put_line(   g_pkg_name|| '.'|| l_api_name|| ' return '|| x_return_status|| ' from gme_transactions_pvt.create_material_txn');
               END IF;
               RAISE error_trans;
             END IF;
           END IF;
         END IF;
       END IF;

      IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
         gme_debug.put_line('Exiting api '||g_pkg_name||'.'||l_api_name);
      END IF;

   EXCEPTION
      WHEN error_trans THEN
         NULL;
      WHEN OTHERS THEN
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);

         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line (   'Unexpected error: '
                                || g_pkg_name
                                || '.'
                                || l_api_name
                                || ': '
                                || SQLERRM);
         END IF;

         x_return_status := fnd_api.g_ret_sts_unexp_error;
   END process_actual_qty;

   PROCEDURE construct_trans_row (
      p_matl_dtl_rec          IN       gme_material_details%ROWTYPE
     ,p_item_rec              IN       mtl_system_items_b%ROWTYPE
     ,p_batch_hdr_rec         IN       gme_batch_header%ROWTYPE
     ,p_batch_step_rec        IN       gme_batch_steps%ROWTYPE
     ,x_mmti_rec              OUT NOCOPY     mtl_transactions_interface%ROWTYPE
     ,x_return_status         OUT NOCOPY     VARCHAR2)
   IS

      l_api_name   CONSTANT VARCHAR2 (30)              := 'construct_trans_row';

      l_val_proc            VARCHAR2 (30);
      l_release_type        NUMBER;
      l_trans_date          DATE;
      --FPbug#4543872
      l_line_type           NUMBER;

      error_construct        EXCEPTION;
      error_fetch_trans_date EXCEPTION;

   BEGIN
      IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
        gme_debug.put_line('Entering api '||g_pkg_name||'.'||l_api_name);
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' batch_id='||p_batch_hdr_rec.batch_id);
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' material_detail_id='||p_matl_dtl_rec.material_detail_id);
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' batchstep_id='||p_batch_step_rec.batchstep_id);
        --FPbug#4543872
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' line_type='||p_matl_dtl_rec.line_type);
      END IF;

      l_line_type     := p_matl_dtl_rec.line_type;
      x_return_status := fnd_api.g_ret_sts_success;

      x_mmti_rec.source_header_id      := p_matl_dtl_rec.batch_id;
      
      /* FPbug#4543872 Begin
         Initialized the following in mmti record
       */
      x_mmti_rec.transaction_source_id := p_matl_dtl_rec.batch_id;
      x_mmti_rec.organization_id       := p_matl_dtl_rec.organization_id; 

      --getting transaction_type_id depends on the line type
      IF l_line_type = gme_common_pvt.g_line_type_ing THEN
         x_mmti_rec.transaction_type_id := gme_common_pvt.g_ing_issue;
      ELSIF l_line_type = gme_common_pvt.g_line_type_prod THEN
         x_mmti_rec.transaction_type_id := gme_common_pvt.g_prod_completion;
      ELSIF l_line_type = gme_common_pvt.g_line_type_byprod THEN
         x_mmti_rec.transaction_type_id := gme_common_pvt.g_byprod_completion;
      END IF;
      --x_mmti_rec.TRANSACTION_TYPE_ID   := 44;
      x_mmti_rec.subinventory_code     := p_matl_dtl_rec.subinventory;
      x_mmti_rec.locator_id            := p_matl_dtl_rec.locator_id;
      --Bug#5129153 Populate the value of revision field in the mmti record.
      x_mmti_rec.revision              := p_matl_dtl_rec.revision;
      /* FPbug#4543872 End */
      
      /*Bug#5394232 Begin
        if we don't pass any date to this procedure then we have to default the trans date*/
      IF x_mmti_rec.transaction_date IS NULL THEN
        gme_common_pvt.fetch_trans_date(
	     p_material_detail_id => p_matl_dtl_rec.material_detail_id
            ,p_invoke_mode        => 'T'
            ,x_trans_date         => l_trans_date
            ,x_return_status      => x_return_status );

       IF x_return_status <> fnd_api.g_ret_sts_success THEN
         RAISE error_fetch_trans_date;
       END IF;
       --initializing the transaction date according to default rules
       x_mmti_rec.transaction_date := l_trans_date;
      END IF;
      --FPbug#4543872 rework
      --x_mmti_rec.transaction_date      := sysdate;
      --Bug#5394232 End

      x_mmti_rec.trx_source_line_id    := p_matl_dtl_rec.material_detail_id;
      x_mmti_rec.transaction_quantity  := p_matl_dtl_rec.actual_qty;
      x_mmti_rec.transaction_uom       := p_matl_dtl_rec.dtl_um;
      x_mmti_rec.inventory_item_id     := p_matl_dtl_rec.inventory_item_id;
      x_mmti_rec.secondary_uom_code    := p_item_rec.secondary_uom_code;

      -- If item is dual, get 2ary qty
      /* FPbug#4543872 commented out the following IF condition
         and added modified one */
      --IF p_item_rec.dual_uom_control <> 0 THEN
      IF p_matl_dtl_rec.dtl_um <> p_item_rec.secondary_uom_code THEN
         get_converted_qty (
                   p_org_id                    => p_matl_dtl_rec.organization_id
                  ,p_item_id                   => p_matl_dtl_rec.inventory_item_id
                  ,p_lot_number                => NULL
                  ,p_qty                       => p_matl_dtl_rec.actual_qty
                  ,p_from_um                   => p_matl_dtl_rec.dtl_um
                  ,p_to_um                     => p_item_rec.secondary_uom_code
                  ,x_conv_qty                  => x_mmti_rec.secondary_transaction_quantity
                  ,x_return_status             => x_return_status);

         IF x_return_status <> fnd_api.g_ret_sts_success THEN
            l_val_proc := 'get_converted_qty';
            RAISE error_construct;
         END IF;
      END IF;

      -- Calculate Transaction Date
      l_release_type := p_matl_dtl_rec.release_type;
      IF l_release_type = gme_common_pvt.g_mtl_autobystep_release AND
         p_batch_step_rec.batchstep_id IS NULL THEN
        l_release_type := gme_common_pvt.g_mtl_auto_release;
      END IF;

      IF l_release_type = gme_common_pvt.g_mtl_autobystep_release THEN    -- abs... dates come from step
         IF p_batch_step_rec.step_status = gme_common_pvt.g_step_completed THEN
            x_mmti_rec.transaction_date := p_batch_step_rec.actual_cmplt_date;
         ELSE                                                   -- must be WIP
            x_mmti_rec.transaction_date := p_batch_step_rec.actual_start_date;
         END IF;
      ELSE                            -- auto release... dates come from batch
         IF p_batch_hdr_rec.batch_status =
                                             gme_common_pvt.g_batch_completed THEN
            x_mmti_rec.transaction_date :=
                                         p_batch_hdr_rec.actual_cmplt_date;
         ELSE                                                   -- must be WIP
            x_mmti_rec.transaction_date :=
                                         p_batch_hdr_rec.actual_start_date;
         END IF;
      END IF;

      IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ' construct trans with following:');
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ' batch_id:'
                             || TO_CHAR (x_mmti_rec.source_header_id) );
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ' mtl_dtl_id:'
                             || TO_CHAR (x_mmti_rec.trx_source_line_id) );
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ' inventory_item_id:'
                             || TO_CHAR (x_mmti_rec.inventory_item_id) );
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ' transaction_quantity:'
                             || TO_CHAR (x_mmti_rec.transaction_quantity) );
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ' transaction_uom:'
                             || x_mmti_rec.transaction_uom);
         gme_debug.put_line
                          (   g_pkg_name
                           || '.'
                           || l_api_name
                           || ' secondary_transaction_quantity:'
                           || TO_CHAR
                                    (x_mmti_rec.secondary_transaction_quantity) );
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ' secondary_uom_code:'
                             || x_mmti_rec.secondary_uom_code);
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ' transaction_date:'
                             || TO_CHAR (x_mmti_rec.transaction_date, g_date_fmt) );
      END IF;

      IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
         gme_debug.put_line('Exiting api '||g_pkg_name||'.'||l_api_name);
      END IF;

   EXCEPTION
      WHEN error_construct OR error_fetch_trans_date THEN
        IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
          gme_debug.put_line (g_pkg_name||'.'||l_api_name
                                        ||': error from proc: '|| l_val_proc);
        END IF;
      WHEN OTHERS THEN
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);

         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line (   'Unexpected error: '
                                || g_pkg_name
                                || '.'
                                || l_api_name
                                || ': '
                                || SQLERRM);
         END IF;

         x_return_status := fnd_api.g_ret_sts_unexp_error;
   END construct_trans_row;

   PROCEDURE get_converted_qty (
      p_org_id                    IN NUMBER
     ,p_item_id                   IN NUMBER
     ,p_lot_number                IN VARCHAR2 DEFAULT NULL
     ,p_qty                       IN NUMBER
     ,p_from_um                   IN VARCHAR2
     ,p_to_um                     IN VARCHAR2
     ,x_conv_qty                  OUT NOCOPY NUMBER
     ,x_return_status             OUT NOCOPY VARCHAR2) IS

      l_api_name           CONSTANT VARCHAR2 (30)     := 'get_converted_qty';

      um_convert_error     EXCEPTION;

   BEGIN
      IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
        gme_debug.put_line('Entering api '||g_pkg_name||'.'||l_api_name);
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_item_id='||p_item_id);
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_lot_number='||p_lot_number);
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_qty='||p_qty);
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_from_um='||p_from_um);
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_to_um='||p_to_um);
      END IF;

      /* Set the return status to success initially */
      x_return_status       := FND_API.G_RET_STS_SUCCESS;

      IF p_to_um = p_from_um THEN
         x_conv_qty := p_qty;
      ELSE
         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
               gme_debug.put_line
                                 (   g_pkg_name
                                  || '.'
                                  || l_api_name
                                  || ' before call to inv_convert.inv_um_convert');
         END IF;

         x_conv_qty := inv_convert.inv_um_convert
               (item_id              => p_item_id
               ,lot_number           => p_lot_number
               ,organization_id      => p_org_id
               ,precision            => gme_common_pvt.g_precision
               ,from_quantity        => p_qty
               ,from_unit            => p_from_um
               ,to_unit              => p_to_um
               ,from_name            => NULL
               ,to_name              => NULL);

         -- Note: -99999 should be in gme_common_pvt
         IF x_conv_qty = -99999 THEN
               IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
                  gme_debug.put_line
                                (   g_pkg_name
                                 || '.'
                                 || l_api_name
                                 || ' inv_convert.inv_um_convert returned error');
               END IF;

               RAISE um_convert_error;
         END IF;
      END IF;  --  IF p_to_um = p_from_um THEN

      IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
               gme_debug.put_line
                                 (   g_pkg_name
                                  || '.'
                                  || l_api_name
                                  || ' converted qty = '||x_conv_qty);
      END IF;

      IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
         gme_debug.put_line('Exiting api '||g_pkg_name||'.'||l_api_name);
      END IF;
   EXCEPTION
      WHEN um_convert_error THEN
         FND_MESSAGE.SET_NAME('INV','INV_NO_CONVERSION_ERR');
         FND_MESSAGE.SET_TOKEN('PGM_NAME',g_pkg_name||'.'||l_api_name);
         fnd_msg_pub.ADD;
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN OTHERS THEN
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);

         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line (   'Unexpected error: '
                                || g_pkg_name
                                || '.'
                                || l_api_name
                                || ': '
                                || SQLERRM);
         END IF;
         x_return_status := fnd_api.g_ret_sts_unexp_error;
   END get_converted_qty;

   PROCEDURE get_item_rec (
      p_org_id          IN       NUMBER
     ,p_item_id         IN       NUMBER
     ,x_item_rec        OUT NOCOPY     mtl_system_items_b%ROWTYPE
     ,x_return_status   OUT NOCOPY     VARCHAR2)
   IS
      CURSOR cur_get_item_rec (v_org_id NUMBER, v_item_id NUMBER)
      IS
         SELECT *
           FROM mtl_system_items_b
          WHERE inventory_item_id = v_item_id
            AND organization_id = v_org_id;

      error_get_item        EXCEPTION;
      l_api_name   CONSTANT VARCHAR2 (30) := 'get_item_rec';
   BEGIN
      IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
        gme_debug.put_line('Entering api '||g_pkg_name||'.'||l_api_name);
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_org_id='||p_org_id);
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_item_id='||p_item_id);
      END IF;

      x_return_status := fnd_api.g_ret_sts_success;

      IF p_item_id IS NULL THEN
         gme_common_pvt.log_message ('GME_NO_KEYS', 'TABLE_NAME', 'mtl_system_items_b');
         RAISE error_get_item;
      END IF;

      OPEN cur_get_item_rec (p_org_id, p_item_id);
      FETCH cur_get_item_rec INTO x_item_rec;
      CLOSE cur_get_item_rec;

      IF x_item_rec.inventory_item_id IS NULL THEN  -- not found
         gme_common_pvt.log_message ('PM_INVALID_ITEM');

         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line(g_pkg_name||'.'||l_api_name||' no record in mtl_system_items_b: ');
            gme_debug.put_line(g_pkg_name||'.'||l_api_name||'inventory_item_id = ' ||p_item_id );
            gme_debug.put_line(g_pkg_name||'.'||l_api_name||'organization_id = ' ||p_org_id);
         END IF;
         RAISE error_get_item;
      END IF;

      IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
         gme_debug.put_line('Exiting api '||g_pkg_name||'.'||l_api_name);
      END IF;

   EXCEPTION
      WHEN error_get_item THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN OTHERS THEN
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);

         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line (   'Unexpected error: '
                                || g_pkg_name
                                || '.'
                                || l_api_name
                                || ': '
                                || SQLERRM);
         END IF;

         x_return_status := fnd_api.g_ret_sts_unexp_error;
   END get_item_rec;

   PROCEDURE validate_item_id (
      p_org_id          IN       NUMBER
     ,p_item_id         IN       NUMBER
     ,x_item_rec        OUT NOCOPY     mtl_system_items_b%ROWTYPE
     ,x_return_status   OUT NOCOPY     VARCHAR2)
   IS
      l_api_name    CONSTANT VARCHAR2 (30) := 'validate_item_id';
      error_validate         EXCEPTION;
      error_get_rec          EXCEPTION;

      l_segm                 mtl_system_items_kfv.concatenated_segments%TYPE;
      l_field                VARCHAR2(100);

   BEGIN
      IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
        gme_debug.put_line('Entering api '||g_pkg_name||'.'||l_api_name);
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' item_id='||p_item_id);
      END IF;

      /* Set return status to success initially */
      x_return_status := fnd_api.g_ret_sts_success;

      get_item_rec (p_org_id             => p_org_id
                   ,p_item_id            => p_item_id
                   ,x_item_rec           => x_item_rec
                   ,x_return_status      => x_return_status);

      IF x_return_status <> fnd_api.g_ret_sts_success THEN
         RAISE error_get_rec;
      END IF;

      -- process_execution_enabled_flag Y/N
      IF x_item_rec.process_execution_enabled_flag = 'N' THEN
         l_field :=  'process_execution_enabled_flag';
         RAISE error_validate;
      END IF;

      -- eng_item_flag Y/N
      -- lab_batch 1 eng item Y OK
      -- lab_batch 0 eng item Y N
      -- lab_batch 1 eng item N OK
      -- lab_batch 0 eng item N OK
      IF gme_common_pvt.g_lab_ind = 0 AND x_item_rec.eng_item_flag = 'Y' THEN
         l_field :=  'eng_item_flag';
         RAISE error_validate;
      END IF;

      -- inventory_item_flag
      IF x_item_rec.inventory_item_flag = 'N' THEN
         l_field :=  'inventory_item_flag';
         RAISE error_validate;
      END IF;

      -- Bug 8693767 - Add 6 as a valid option to serial type list.
      -- serial_number_control_code 1 = no serial number control
      IF x_item_rec.serial_number_control_code NOT IN (1, 6) THEN
         l_field :=  'serial_number_control_code';
         RAISE error_validate;
      END IF;

      IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
         gme_debug.put_line('Exiting api '||g_pkg_name||'.'||l_api_name);
      END IF;

   EXCEPTION
      WHEN error_get_rec THEN
           NULL;
      WHEN error_validate THEN
           IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
                  gme_debug.put_line
                                (   g_pkg_name
                                 || '.'
                                 || l_api_name
                                 || ' validation failed for item field: '||l_field);
           END IF;

           SELECT concatenated_segments
             INTO l_segm
             FROM mtl_system_items_kfv
            WHERE inventory_item_id = p_item_id
              AND organization_id = p_org_id;
           --Bug#5078853           
           gme_common_pvt.log_message ('GME_INV_ITEM_INSERT', 'ITEM_NO', l_field);
           x_return_status := fnd_api.g_ret_sts_error;

      WHEN OTHERS THEN
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);

         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line (   'Unexpected error: '
                                || g_pkg_name
                                || '.'
                                || l_api_name
                                || ': '
                                || SQLERRM);
         END IF;

         x_return_status := fnd_api.g_ret_sts_unexp_error;
   END validate_item_id;

   PROCEDURE validate_revision (
      p_revision        IN       VARCHAR2
     ,p_item_rec        IN       mtl_system_items_b%ROWTYPE
     ,x_return_status   OUT NOCOPY     VARCHAR2)
   IS
      CURSOR cur_get_revision (
         v_org_id     NUMBER
        ,v_item_id    NUMBER
        ,v_revision   VARCHAR2)
      IS
         SELECT 1
           FROM mtl_item_revisions_b
          WHERE inventory_item_id = v_item_id
            AND organization_id = v_org_id
            AND revision = v_revision;

      l_api_name          CONSTANT VARCHAR2 (30) := 'validate_revision';
      l_is_revision_found          NUMBER;
      error_not_revision_control   EXCEPTION;
      error_revision_not_found     EXCEPTION;

   BEGIN

      IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
        gme_debug.put_line('Entering api '||g_pkg_name||'.'||l_api_name);
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' revision='||p_revision);
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' item_id='||p_item_rec.inventory_item_id);
      END IF;

      x_return_status := fnd_api.g_ret_sts_success;

      IF p_revision IS NULL THEN  -- not required even if revision controlled
         RETURN;
      END IF;

      -- revision_qty_control_code
      --   1=No revision qty control; 2=Under revision qty control
      IF p_item_rec.revision_qty_control_code = 1 THEN
         gme_common_pvt.log_message ('GME_NOT_REV_CTRL');
         RAISE error_not_revision_control;
      END IF;

      OPEN cur_get_revision (p_item_rec.organization_id
                            ,p_item_rec.inventory_item_id
                            ,p_revision);
      FETCH cur_get_revision INTO l_is_revision_found;
      CLOSE cur_get_revision;

      IF l_is_revision_found IS NULL OR l_is_revision_found <> 1 THEN
         gme_common_pvt.log_message ('GME_REV_NOT_DEFD');
         RAISE error_revision_not_found;
      END IF;

      IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
         gme_debug.put_line('Exiting api '||g_pkg_name||'.'||l_api_name);
      END IF;

   EXCEPTION
      WHEN error_not_revision_control OR error_revision_not_found THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN OTHERS THEN
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);

         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line (   'Unexpected error: '
                                || g_pkg_name
                                || '.'
                                || l_api_name
                                || ': '
                                || SQLERRM);
         END IF;

         x_return_status := fnd_api.g_ret_sts_unexp_error;
   END validate_revision;

   PROCEDURE validate_line_type (
      p_line_type       IN       NUMBER
     ,x_return_status   OUT NOCOPY     VARCHAR2)
   IS
      l_api_name   CONSTANT VARCHAR2 (30) := 'validate_line_type';
      validation_error      EXCEPTION;
   BEGIN
      IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
        gme_debug.put_line('Entering api '||g_pkg_name||'.'||l_api_name);
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_line_type='||p_line_type);
      END IF;

      /* Set return status to success initially */
      x_return_status := fnd_api.g_ret_sts_success;

      -- check GMD parameter FM$BYPROD_ACTIVE
      -- value of 1 means Yes; byproducts are available for insert, update and delete
      -- value of 2 means No; byproducts are not available
      -- if this parameter is set to 2 and the material being inserted is a byproduct, then
      -- raise an error
      IF gme_common_pvt.g_byprod_active = 2 AND
         p_line_type = gme_common_pvt.g_line_type_byprod THEN
        fnd_message.set_name ('GMD', 'FM_BYPROD_INACTIVE');
        fnd_msg_pub.ADD;
        RAISE validation_error;
      END IF;

      IF p_line_type NOT IN
            (gme_common_pvt.g_line_type_ing
            ,gme_common_pvt.g_line_type_prod
            ,gme_common_pvt.g_line_type_byprod) THEN
         gme_common_pvt.log_message ('GME_INVALID_LINE_TYPE');
         RAISE validation_error;
      END IF;

      IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
         gme_debug.put_line('Exiting api '||g_pkg_name||'.'||l_api_name);
      END IF;

   EXCEPTION
      WHEN validation_error THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN OTHERS THEN
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);

         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line (   'Unexpected error: '
                                || g_pkg_name
                                || '.'
                                || l_api_name
                                || ': '
                                || SQLERRM);
         END IF;

         x_return_status := fnd_api.g_ret_sts_unexp_error;
   END validate_line_type;
--Bug#5129153 Changed the data type of 'p_byproduct_type' to VARCHAR2.
   PROCEDURE validate_byproduct_type (
      p_byproduct_type   IN       VARCHAR2
     ,x_return_status    OUT NOCOPY     VARCHAR2)
   IS
      CURSOR cur_byprod_type (v_byprod_type VARCHAR2)
      IS
        SELECT 1
          FROM gem_lookup_values
         WHERE lookup_type = 'GMD_BY_PRODUCT_TYPE'
               AND lookup_code = v_byprod_type;

      l_api_name      CONSTANT VARCHAR2 (30) := 'validate_byproduct_type';
      l_exists                 NUMBER;
      invalid_byproduct_type   EXCEPTION;
   BEGIN
      IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
        gme_debug.put_line('Entering api '||g_pkg_name||'.'||l_api_name);
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_byproduct_type='||p_byproduct_type);
      END IF;

      /* Set return status to success initially */
      x_return_status := fnd_api.g_ret_sts_success;

      -- By_product_type of NULL is OK
      IF p_byproduct_type IS NOT NULL THEN
        OPEN cur_byprod_type (p_byproduct_type);
        FETCH cur_byprod_type INTO l_exists;
        CLOSE cur_byprod_type;

        IF l_exists IS NULL OR l_exists <> 1 THEN
           gme_common_pvt.log_message ('GME_INVALID_BYPROD_TYPE');
           RAISE invalid_byproduct_type;
        END IF;
      END IF;

      IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
         gme_debug.put_line('Exiting api '||g_pkg_name||'.'||l_api_name);
      END IF;

   EXCEPTION
      WHEN invalid_byproduct_type THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN OTHERS THEN
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);

         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line (   'Unexpected error: '
                                || g_pkg_name
                                || '.'
                                || l_api_name
                                || ': '
                                || SQLERRM);
         END IF;

         x_return_status := fnd_api.g_ret_sts_unexp_error;
   END validate_byproduct_type;

   PROCEDURE validate_line_no (
      p_line_no            IN    NUMBER
     ,p_line_type          IN    NUMBER
     ,p_batch_id           IN    NUMBER
     ,x_line_no            OUT NOCOPY     NUMBER
     ,x_return_status      OUT NOCOPY     VARCHAR2)
   IS

      CURSOR cur_last_line_no (v_batch_id NUMBER, v_line_type NUMBER)
      IS
         SELECT max(line_no)
           FROM gme_material_details
          WHERE batch_id = v_batch_id
            AND line_type = v_line_type;

      l_api_name   CONSTANT VARCHAR2 (30) := 'validate_line_no';
      invalid_line_no       EXCEPTION;
   BEGIN
      IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
        gme_debug.put_line('Entering api '||g_pkg_name||'.'||l_api_name);
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_line_no='||p_line_no);
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_line_type='||p_line_type);
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_batch_id='||p_batch_id);
      END IF;

      x_return_status := fnd_api.g_ret_sts_success;

      IF p_line_no <= 0 THEN
         gme_common_pvt.log_message ('GME_INVALID_LINE_NUMBER');
         RAISE invalid_line_no;
      END IF;

      OPEN cur_last_line_no(p_batch_id, p_line_type);
      FETCH cur_last_line_no INTO x_line_no;
      CLOSE cur_last_line_no;

      IF p_line_no IS NULL OR p_line_no > x_line_no THEN
        x_line_no := NVL(x_line_no, 0) + 1;
      ELSE
        x_line_no := p_line_no;
      END IF;

      IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
         gme_debug.put_line('Exiting api '||g_pkg_name||'.'||l_api_name);
      END IF;

   EXCEPTION
      WHEN invalid_line_no THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN OTHERS THEN
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);

         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line (   'Unexpected error: '
                                || g_pkg_name
                                || '.'
                                || l_api_name
                                || ': '
                                || SQLERRM);
         END IF;

         x_return_status := fnd_api.g_ret_sts_unexp_error;
   END validate_line_no;

   PROCEDURE validate_dtl_um (
      p_dtl_um          IN       VARCHAR2
     ,p_primary_uom     IN       VARCHAR2
     ,p_item_id         IN       NUMBER
     ,p_org_id          IN       NUMBER
     ,x_return_status   OUT NOCOPY     VARCHAR2)
   IS
      l_api_name    CONSTANT VARCHAR2 (30)               := 'validate_dtl_um';
      l_disable_date         DATE;
      l_qty                  NUMBER;
--      invalid_dtl_um         EXCEPTION;
      disabled_dtl_um        EXCEPTION;
      um_convert_error       EXCEPTION;

      CURSOR cur_get_uom_code_date (v_uom_code VARCHAR2)
      IS
         SELECT disable_date
           FROM mtl_units_of_measure
          WHERE uom_code = v_uom_code;

   BEGIN
      IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
        gme_debug.put_line('Entering api '||g_pkg_name||'.'||l_api_name);
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_dtl_um='||p_dtl_um);
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_primary_uom='||p_primary_uom);
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_item_id='||p_item_id);
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_org_id='||p_org_id);
      END IF;

      x_return_status := fnd_api.g_ret_sts_success;

      OPEN cur_get_uom_code_date (p_dtl_um);
      FETCH cur_get_uom_code_date INTO l_disable_date;
      CLOSE cur_get_uom_code_date;

-- Namit bug#4515560. Disable date can be null for UOM and do not raise exception for it.
/*
      IF l_disable_date IS NULL THEN
         fnd_message.set_name ('GMI', 'IC_UMCODE');
         fnd_msg_pub.ADD;
         RAISE invalid_dtl_um;
      END IF;
*/

      IF l_disable_date <= gme_common_pvt.g_timestamp THEN
         gme_common_pvt.log_message ('GME_UM_DISABLED');
         RAISE disabled_dtl_um;
      END IF;

      get_converted_qty (
        p_org_id                    => p_org_id
       ,p_item_id                   => p_item_id
       ,p_lot_number                => NULL
       ,p_qty                       => 1
       ,p_from_um                   => p_dtl_um
       ,p_to_um                     => p_primary_uom
       ,x_conv_qty                  => l_qty
       ,x_return_status             => x_return_status);

      IF x_return_status <> fnd_api.g_ret_sts_success THEN
         RAISE um_convert_error;
      END IF;

      IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
         gme_debug.put_line('Exiting api '||g_pkg_name||'.'||l_api_name);
      END IF;

   EXCEPTION
--      WHEN invalid_dtl_um OR disabled_dtl_um THEN
      WHEN disabled_dtl_um THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN um_convert_error THEN
         NULL;
      WHEN OTHERS THEN
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);

         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line (   'Unexpected error: '
                                || g_pkg_name
                                || '.'
                                || l_api_name
                                || ': '
                                || SQLERRM);
         END IF;

         x_return_status := fnd_api.g_ret_sts_unexp_error;
   END validate_dtl_um;

   PROCEDURE validate_plan_qty (
      p_plan_qty        IN       NUMBER
     ,x_return_status   OUT NOCOPY     VARCHAR2)
   IS
      val_error    EXCEPTION;
      l_api_name   CONSTANT VARCHAR2 (30) := 'validate_plan_qty';
   BEGIN
      IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
        gme_debug.put_line('Entering api '||g_pkg_name||'.'||l_api_name);
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_plan_qty='||p_plan_qty);
      END IF;

      x_return_status := fnd_api.g_ret_sts_success;

      IF p_plan_qty < 0 THEN
        gme_common_pvt.log_message ('GME_INVALID_PLAN_QTY');
        RAISE val_error;
      END IF;

      IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
         gme_debug.put_line('Exiting api '||g_pkg_name||'.'||l_api_name);
      END IF;

   EXCEPTION
      WHEN val_error THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN OTHERS THEN
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);

         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line (   'Unexpected error: '
                                || g_pkg_name
                                || '.'
                                || l_api_name
                                || ': '
                                || SQLERRM);
         END IF;

         x_return_status := fnd_api.g_ret_sts_unexp_error;
   END validate_plan_qty;

   PROCEDURE validate_wip_plan_qty (
      p_wip_plan_qty    IN       NUMBER
     ,x_return_status   OUT NOCOPY     VARCHAR2)
   IS
      val_error    EXCEPTION;
      l_api_name   CONSTANT VARCHAR2 (30) := 'validate_wip_plan_qty';
   BEGIN
      IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
        gme_debug.put_line('Entering api '||g_pkg_name||'.'||l_api_name);
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_wip_plan_qty='||p_wip_plan_qty);
      END IF;

      x_return_status := fnd_api.g_ret_sts_success;

      IF p_wip_plan_qty < 0 THEN
        gme_common_pvt.log_message ('GME_INVALID_WIP_PLAN_QTY');
        RAISE val_error;
      END IF;

      IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
         gme_debug.put_line('Exiting api '||g_pkg_name||'.'||l_api_name);
      END IF;

   EXCEPTION
      WHEN val_error THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN OTHERS THEN
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);

         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line (   'Unexpected error: '
                                || g_pkg_name
                                || '.'
                                || l_api_name
                                || ': '
                                || SQLERRM);
         END IF;

         x_return_status := fnd_api.g_ret_sts_unexp_error;
   END validate_wip_plan_qty;

   PROCEDURE validate_actual_qty (
      p_actual_qty      IN             NUMBER
     ,x_return_status   OUT NOCOPY     VARCHAR2)
   IS
      val_error    EXCEPTION;
      l_api_name   CONSTANT VARCHAR2 (30) := 'validate_actual_qty';
   BEGIN
      IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
        gme_debug.put_line('Entering api '||g_pkg_name||'.'||l_api_name);
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_actual_qty='||p_actual_qty);
      END IF;

      x_return_status := fnd_api.g_ret_sts_success;

      IF p_actual_qty < 0 THEN
        gme_common_pvt.log_message ('GME_INVALID_ACTUAL_QTY');
        RAISE val_error;
      END IF;

      IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
         gme_debug.put_line('Exiting api '||g_pkg_name||'.'||l_api_name);
      END IF;

   EXCEPTION
      WHEN val_error THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN OTHERS THEN
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);

         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line (   'Unexpected error: '
                                || g_pkg_name
                                || '.'
                                || l_api_name
                                || ': '
                                || SQLERRM);
         END IF;

         x_return_status := fnd_api.g_ret_sts_unexp_error;
   END validate_actual_qty;

   PROCEDURE validate_release_type (
      p_material_detail_rec   IN       gme_material_details%ROWTYPE
     ,p_release_type          IN       NUMBER
     ,x_return_status         OUT NOCOPY     VARCHAR2)
   IS
      CURSOR cur_rel_type (v_rel_type VARCHAR2) IS
        SELECT 1
          FROM gem_lookup_values
         WHERE lookup_type = 'GMD_MATERIAL_RELEASE_TYPE'
               AND lookup_code = v_rel_type;

      l_exists                 NUMBER;
      val_error                EXCEPTION;

      l_api_name   CONSTANT VARCHAR2 (30) := 'validate_release_type';
   BEGIN
      IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
        gme_debug.put_line('Entering api '||g_pkg_name||'.'||l_api_name);
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_release_type='||p_release_type);
      END IF;

      x_return_status := fnd_api.g_ret_sts_success;

      OPEN cur_rel_type (p_release_type);
      FETCH cur_rel_type INTO l_exists;
      CLOSE cur_rel_type;

      IF l_exists IS NULL OR l_exists <> 1 THEN
         gme_common_pvt.log_message ('GME_INVALID_RELEASE_TYPE');
         RAISE val_error;
      END IF;

      IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
         gme_debug.put_line('Exiting api '||g_pkg_name||'.'||l_api_name);
      END IF;

   EXCEPTION
      WHEN val_error THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN OTHERS THEN
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);

         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line (   'Unexpected error: '
                                || g_pkg_name
                                || '.'
                                || l_api_name
                                || ': '
                                || SQLERRM);
         END IF;

         x_return_status := fnd_api.g_ret_sts_unexp_error;
   END validate_release_type;

   PROCEDURE validate_scrap_factor (
      p_scrap           IN       NUMBER
     ,x_return_status   OUT NOCOPY     VARCHAR2)
   IS

      val_error                EXCEPTION;
      l_api_name   CONSTANT VARCHAR2 (30) := 'validate_scrap_factor';
   BEGIN
      IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
        gme_debug.put_line('Entering api '||g_pkg_name||'.'||l_api_name);
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_scrap='||p_scrap);
      END IF;

      x_return_status := fnd_api.g_ret_sts_success;

      IF (p_scrap < 0 OR p_scrap > 1000000) THEN
         gme_common_pvt.log_message ('GME_INVALID_SCRAP_FACTOR');
         RAISE val_error;
      END IF;

      IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
         gme_debug.put_line('Exiting api '||g_pkg_name||'.'||l_api_name);
      END IF;

   EXCEPTION
      WHEN val_error THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN OTHERS THEN
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);

         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line (   'Unexpected error: '
                                || g_pkg_name
                                || '.'
                                || l_api_name
                                || ': '
                                || SQLERRM);
         END IF;

         x_return_status := fnd_api.g_ret_sts_unexp_error;
   END validate_scrap_factor;

   PROCEDURE validate_scale_multiple (
      p_scale_mult      IN       NUMBER
     ,x_return_status   OUT NOCOPY     VARCHAR2)
   IS
      val_error                EXCEPTION;
      l_api_name   CONSTANT VARCHAR2 (30) := 'validate_scale_multiple';
   BEGIN
      IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
        gme_debug.put_line('Entering api '||g_pkg_name||'.'||l_api_name);
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_scale_mult='||p_scale_mult);
      END IF;

      x_return_status := fnd_api.g_ret_sts_success;

      IF p_scale_mult <= 0 THEN
         gme_common_pvt.log_message ('GME_INVALID_SCALE_MULT');
         RAISE val_error;
      END IF;

      IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
         gme_debug.put_line('Exiting api '||g_pkg_name||'.'||l_api_name);
      END IF;

   EXCEPTION
      WHEN val_error THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN OTHERS THEN
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);

         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line (   'Unexpected error: '
                                || g_pkg_name
                                || '.'
                                || l_api_name
                                || ': '
                                || SQLERRM);
         END IF;

         x_return_status := fnd_api.g_ret_sts_unexp_error;
   END validate_scale_multiple;

   PROCEDURE validate_scale_round_var (
      p_scale_var       IN       NUMBER
     ,x_return_status   OUT NOCOPY     VARCHAR2)
   IS
      val_error                EXCEPTION;
      l_api_name   CONSTANT VARCHAR2 (30) := 'validate_scale_round_var';
   BEGIN
      IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
        gme_debug.put_line('Entering api '||g_pkg_name||'.'||l_api_name);
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_scale_var='||p_scale_var);
      END IF;

      x_return_status := fnd_api.g_ret_sts_success;

      IF (p_scale_var < 0 OR p_scale_var > 100) THEN
         gme_common_pvt.log_message ('GME_INVALID_SCALE_ROUND_VAR');
         RAISE val_error;
      END IF;

      IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
         gme_debug.put_line('Exiting api '||g_pkg_name||'.'||l_api_name);
      END IF;

   EXCEPTION
      WHEN val_error THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN OTHERS THEN
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);

         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line (   'Unexpected error: '
                                || g_pkg_name
                                || '.'
                                || l_api_name
                                || ': '
                                || SQLERRM);
         END IF;

         x_return_status := fnd_api.g_ret_sts_unexp_error;
   END validate_scale_round_var;

   PROCEDURE validate_rounding_direction (
      p_round_dir       IN       NUMBER
     ,x_return_status   OUT NOCOPY     VARCHAR2)
   IS
      CURSOR cur_round_dir (v_round_dir VARCHAR2)
      IS
        SELECT 1
          FROM gem_lookup_values
         WHERE lookup_type = 'GMD_ROUNDING_DIRECTION'
               AND lookup_code = v_round_dir;

      l_exists                 NUMBER;
      val_error                EXCEPTION;

      l_api_name   CONSTANT VARCHAR2 (30) := 'validate_rounding_direction';
   BEGIN
      IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
        gme_debug.put_line('Entering api '||g_pkg_name||'.'||l_api_name);
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_round_dir='||p_round_dir);
      END IF;

      x_return_status := fnd_api.g_ret_sts_success;

      OPEN cur_round_dir (p_round_dir);
      FETCH cur_round_dir INTO l_exists;
      CLOSE cur_round_dir;

      IF l_exists IS NULL OR l_exists <> 1 THEN
         gme_common_pvt.log_message ('GME_INVALID_ROUND_DIR');
         RAISE val_error;
      END IF;

      IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
         gme_debug.put_line('Exiting api '||g_pkg_name||'.'||l_api_name);
      END IF;

   EXCEPTION
      WHEN val_error THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN OTHERS THEN
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);

         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line (   'Unexpected error: '
                                || g_pkg_name
                                || '.'
                                || l_api_name
                                || ': '
                                || SQLERRM);
         END IF;

         x_return_status := fnd_api.g_ret_sts_unexp_error;
   END validate_rounding_direction;

   PROCEDURE validate_scale_type (
      p_scale_type            IN       NUMBER
     ,x_return_status         OUT NOCOPY     VARCHAR2)
   IS

      CURSOR cur_scale_type (v_scale_type VARCHAR2)
      IS
        SELECT 1
          FROM gem_lookup_values
         WHERE lookup_type = 'SCALE_TYPE'
               AND lookup_code = v_scale_type;

      l_exists              NUMBER;
      l_api_name   CONSTANT VARCHAR2 (30) := 'validate_scale_type';
      val_error             EXCEPTION;
   BEGIN
      IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
        gme_debug.put_line('Entering api '||g_pkg_name||'.'||l_api_name);
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_scale_type='||p_scale_type);
      END IF;

      x_return_status := fnd_api.g_ret_sts_success;

      OPEN cur_scale_type (p_scale_type);
      FETCH cur_scale_type INTO l_exists;
      CLOSE cur_scale_type;

      IF l_exists IS NULL OR l_exists <> 1 THEN
         gme_common_pvt.log_message ('GME_INVALID_SCALE_TYPE');
         RAISE val_error;
      END IF;

      IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
         gme_debug.put_line('Exiting api '||g_pkg_name||'.'||l_api_name);
      END IF;

   EXCEPTION
      WHEN val_error THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN OTHERS THEN
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);

         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line (   'Unexpected error: '
                                || g_pkg_name
                                || '.'
                                || l_api_name
                                || ': '
                                || SQLERRM);
         END IF;

         x_return_status := fnd_api.g_ret_sts_unexp_error;
   END validate_scale_type;

    --FPBug#4524232 changed parameter to p_material_detail_rec from p_cost_alloc
 PROCEDURE validate_cost_alloc(
      p_material_detail_rec    IN gme_material_details%ROWTYPE      
     ,x_return_status   OUT NOCOPY     VARCHAR2)
   IS
    --FPBug#4524232 Begin
    CURSOR Cur_get_step_status(v_material_detail_id NUMBER) IS
      SELECT steprelease_type,step_status
       FROM  gme_batch_step_items si, gme_batch_steps s
       WHERE si.batchstep_id = s.batchstep_id
         AND si.material_detail_id = v_material_detail_id; 
    CURSOR Cur_get_batch_status(v_batch_id NUMBER) IS
      SELECT batch_status
       FROM  gme_batch_header       
       WHERE batch_id = v_batch_id; 
    CURSOR Cur_get_cost_alloc(v_material_detail_id NUMBER) IS
      SELECT cost_alloc
       FROM  gme_material_details       
       WHERE material_detail_id = v_material_detail_id; 

     l_batch_id    NUMBER;
     l_material_detail_id NUMBER;
     l_status      NUMBER;
     l_step_status NUMBER;
     l_rel_type NUMBER;
     l_cost_alloc NUMBER;
    --FPBug#4524232 End
    val_error    EXCEPTION;
    l_api_name   CONSTANT VARCHAR2 (30) := 'validate_cost_alloc';
   BEGIN
      IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
        gme_debug.put_line('Entering api '||g_pkg_name||'.'||l_api_name);        
      END IF;
      x_return_status := fnd_api.g_ret_sts_success;

      --FPBug#4524232  Begin
      l_batch_id      := p_material_detail_rec.batch_id;
      l_material_detail_id := p_material_detail_rec.material_detail_id;
      
      OPEN Cur_get_batch_status(l_batch_id);
      FETCH Cur_get_batch_status INTO l_status;
      CLOSE Cur_get_batch_status;
      
      IF l_status = gme_common_pvt.g_batch_wip  THEN
       OPEN Cur_get_step_status(l_material_detail_id);
       FETCH Cur_get_step_status INTO l_rel_type,l_step_status;
       CLOSE Cur_get_step_status;
      END IF;
      
      /* For the completed batches and the wip batches where the associated step is 
       completed, the cost allocation is not updatable */
      IF l_status = gme_common_pvt.g_batch_completed OR
        ( l_status = gme_common_pvt.g_batch_wip AND l_rel_type=gme_common_pvt.g_mtl_autobystep_release AND 
          l_step_status = gme_common_pvt.g_step_completed) THEN
        IF l_material_detail_id is NULL THEN
         /* in insert */
         IF p_material_detail_rec.cost_alloc <> 0 THEN
          gme_common_pvt.log_message ('GME_INVALID_COST_ALLOC');
          RAISE val_error; 
         END IF;
        ELSE
         /* in update */
         OPEN Cur_get_cost_alloc(l_material_detail_id);
         FETCH Cur_get_cost_alloc INTO l_cost_alloc;
         CLOSE Cur_get_cost_alloc;
         /*if the passed cost allocation is diff from stored cost alloc raise error */
         IF l_cost_alloc <> p_material_detail_rec.cost_alloc THEN
          gme_common_pvt.log_message ('GME_COST_ALLOC_CANNOT_UPD');
          RAISE val_error; 
         END IF;
        END IF;       
      END IF;
      --commented out the following lines
      /*IF p_cost_alloc < 0 OR p_cost_alloc > 1 THEN
         gme_common_pvt.log_message ('GME_INVALID_COST_ALLOC');
         RAISE val_error;
      END IF; */
      --FPBug#4524232  End
      IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
         gme_debug.put_line('Exiting api '||g_pkg_name||'.'||l_api_name);
      END IF;
   EXCEPTION
      WHEN val_error THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN OTHERS THEN
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line (   'Unexpected error: '
                                || g_pkg_name
                                || '.'
                                || l_api_name
                                || ': '
                                || SQLERRM);
         END IF;
         x_return_status := fnd_api.g_ret_sts_unexp_error;
   END validate_cost_alloc;

   PROCEDURE validate_phantom_type (
      p_phantom_type    IN       NUMBER
     ,x_return_status   OUT NOCOPY     VARCHAR2)
   IS

      CURSOR cur_phantom_type (v_phantom_type VARCHAR2)
      IS
        SELECT 1
          FROM gem_lookup_values
         WHERE lookup_type = 'PHANTOM_TYPE'
               AND lookup_code = v_phantom_type;

      l_exists     NUMBER;
      val_error    EXCEPTION;
      l_api_name   CONSTANT VARCHAR2 (30) := 'validate_phantom_type';
   BEGIN
      IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
        gme_debug.put_line('Entering api '||g_pkg_name||'.'||l_api_name);
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_phantom_type='||p_phantom_type);
      END IF;

      x_return_status := fnd_api.g_ret_sts_success;

      OPEN cur_phantom_type (p_phantom_type);
      FETCH cur_phantom_type INTO l_exists;
      CLOSE cur_phantom_type;

      IF l_exists IS NULL OR l_exists <> 1 THEN
         gme_common_pvt.log_message ('GME_INV_PHANTOM_TYPE');
         RAISE val_error;
      END IF;

      IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
         gme_debug.put_line('Exiting api '||g_pkg_name||'.'||l_api_name);
      END IF;

   EXCEPTION
      WHEN val_error THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN OTHERS THEN
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);

         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line (   'Unexpected error: '
                                || g_pkg_name
                                || '.'
                                || l_api_name
                                || ': '
                                || SQLERRM);
         END IF;

         x_return_status := fnd_api.g_ret_sts_unexp_error;
   END validate_phantom_type;

   PROCEDURE validate_contr_yield_ind (
      p_contr_yield_ind   IN       VARCHAR2 --FPBug#5040865
     ,x_return_status     OUT NOCOPY     VARCHAR2)
   IS
      val_error    EXCEPTION;
      l_api_name   CONSTANT VARCHAR2 (30) := 'validate_contr_yield_ind';
   BEGIN
      IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
        gme_debug.put_line('Entering api '||g_pkg_name||'.'||l_api_name);
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_contr_yield_ind='||p_contr_yield_ind);
      END IF;

      x_return_status := fnd_api.g_ret_sts_success;

      IF p_contr_yield_ind NOT IN ('Y', 'N') THEN
        gme_common_pvt.log_message ('GME_INVALID_CONTR_YIELD');
        RAISE val_error;
      END IF;

      IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
         gme_debug.put_line('Exiting api '||g_pkg_name||'.'||l_api_name);
      END IF;

   EXCEPTION
      WHEN val_error THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN OTHERS THEN
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);

         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line (   'Unexpected error: '
                                || g_pkg_name
                                || '.'
                                || l_api_name
                                || ': '
                                || SQLERRM);
         END IF;

         x_return_status := fnd_api.g_ret_sts_unexp_error;
   END validate_contr_yield_ind;

   PROCEDURE validate_contr_step_qty_ind (
      p_contr_step_qty_ind   IN       VARCHAR2 --FPBug#5040865
     ,x_return_status        OUT NOCOPY     VARCHAR2)
   IS
      val_error    EXCEPTION;

      l_api_name   CONSTANT VARCHAR2 (30) := 'validate_contr_step_qty_ind';
   BEGIN
      IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
        gme_debug.put_line('Entering api '||g_pkg_name||'.'||l_api_name);
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_contr_step_qty_ind='||p_contr_step_qty_ind);
      END IF;

      x_return_status := fnd_api.g_ret_sts_success;

      IF p_contr_step_qty_ind NOT IN ('Y', 'N') THEN
        gme_common_pvt.log_message ('GME_INVALID_CONTR_STEP');
        RAISE val_error;
      END IF;

      IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
         gme_debug.put_line('Exiting api '||g_pkg_name||'.'||l_api_name);
      END IF;

   EXCEPTION
      WHEN val_error THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN OTHERS THEN
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);

         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line (   'Unexpected error: '
                                || g_pkg_name
                                || '.'
                                || l_api_name
                                || ': '
                                || SQLERRM);
         END IF;

         x_return_status := fnd_api.g_ret_sts_unexp_error;
   END validate_contr_step_qty_ind;

   PROCEDURE validate_subinventory (
      p_subinv          IN       VARCHAR2
     ,p_item_rec        IN       mtl_system_items_b%ROWTYPE
     ,x_return_status   OUT NOCOPY     VARCHAR2)
   IS
      l_api_name   CONSTANT VARCHAR2 (30) := 'VALIDATE_SUBINVENTORY';
      sub_not_valid         EXCEPTION;
   BEGIN
      IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
        gme_debug.put_line('Entering api '||g_pkg_name||'.'||l_api_name);
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_subinv='||p_subinv);
      END IF;

      x_return_status := fnd_api.g_ret_sts_success;

      IF p_subinv IS NULL THEN
         NULL;                                                -- not required
      ELSIF gme_common_pvt.check_subinventory
                 (p_organization_id        => p_item_rec.organization_id
                 ,p_subinventory           => p_subinv
                 ,p_inventory_item_id      => p_item_rec.inventory_item_id
                 ,p_restrict_subinv        => p_item_rec.restrict_subinventories_code) THEN
         NULL;
      ELSE
         RAISE sub_not_valid;
      END IF;

      IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
         gme_debug.put_line('Exiting api '||g_pkg_name||'.'||l_api_name);
      END IF;

   EXCEPTION
      WHEN sub_not_valid THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN OTHERS THEN
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);

         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line (   'Unexpected error: '
                                || g_pkg_name
                                || '.'
                                || l_api_name
                                || ': '
                                || SQLERRM);
         END IF;

         x_return_status := fnd_api.g_ret_sts_unexp_error;
   END validate_subinventory;

   PROCEDURE validate_locator (
      p_subinv          IN       VARCHAR2
     ,p_locator_id      IN       NUMBER
     ,p_item_rec        IN       mtl_system_items_b%ROWTYPE
     ,p_line_type       IN       NUMBER
     ,x_return_status   OUT NOCOPY     VARCHAR2)
   IS
      l_api_name   CONSTANT VARCHAR2 (30) := 'VALIDATE_LOCATOR';
      l_txn_action_id       NUMBER;
      l_sub_locator_type    NUMBER;
      loc_not_valid         EXCEPTION;

      CURSOR cur_sub_control (v_org_id NUMBER, v_subinventory VARCHAR2)
      IS
         SELECT locator_type
           FROM mtl_secondary_inventories
          WHERE organization_id = v_org_id
            AND secondary_inventory_name = v_subinventory;
   BEGIN
      IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
        gme_debug.put_line('Entering api '||g_pkg_name||'.'||l_api_name);
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_subinv='||p_subinv);
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_locator_id='||p_locator_id);
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_line_type='||p_line_type);
      END IF;

      x_return_status := fnd_api.g_ret_sts_success;

      IF p_line_type = gme_common_pvt.g_line_type_ing THEN
         l_txn_action_id := gme_common_pvt.g_ing_issue_txn_action;
      ELSIF p_line_type = gme_common_pvt.g_line_type_prod THEN
         l_txn_action_id := gme_common_pvt.g_prod_comp_txn_action;
      ELSIF p_line_type = gme_common_pvt.g_line_type_byprod THEN
         l_txn_action_id := gme_common_pvt.g_byprod_comp_txn_action;
      END IF;

      IF p_locator_id IS NULL THEN
         NULL;                                                -- not required
      ELSE
         OPEN cur_sub_control (p_item_rec.organization_id, p_subinv);
         FETCH cur_sub_control INTO l_sub_locator_type;
         CLOSE cur_sub_control;
         /* Bug 5441643 Added NVL condition for location control code*/
         IF gme_common_pvt.check_locator
                   (p_organization_id        => p_item_rec.organization_id
                   ,p_locator_id             => p_locator_id
                   ,p_subinventory           => p_subinv
                   ,p_inventory_item_id      => p_item_rec.inventory_item_id
                   ,p_org_control            => gme_common_pvt.g_org_locator_control
                   ,p_sub_control            => l_sub_locator_type
                   ,p_item_control           => NVL(p_item_rec.location_control_code,1)
                   ,p_item_loc_restrict      => p_item_rec.restrict_locators_code
                   ,p_org_neg_allowed        => gme_common_pvt.g_allow_neg_inv
                   ,p_txn_action_id          => l_txn_action_id) THEN
            NULL;
         ELSE
            RAISE loc_not_valid;
         END IF;
      END IF;

      IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
         gme_debug.put_line('Exiting api '||g_pkg_name||'.'||l_api_name);
      END IF;

   EXCEPTION
      WHEN loc_not_valid THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN OTHERS THEN
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);

         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line (   'Unexpected error: '
                                || g_pkg_name
                                || '.'
                                || l_api_name
                                || ': '
                                || SQLERRM);
         END IF;

         x_return_status := fnd_api.g_ret_sts_unexp_error;
   END validate_locator;

   PROCEDURE update_material_line (
      p_batch_header_rec             IN              gme_batch_header%ROWTYPE
     ,p_material_detail_rec          IN              gme_material_details%ROWTYPE
     ,p_stored_material_detail_rec   IN              gme_material_details%ROWTYPE
     ,p_batch_step_rec               IN              gme_batch_steps%ROWTYPE
     ,p_scale_phantom                IN              VARCHAR2 := fnd_api.g_false
     ,p_trans_id                     IN              NUMBER
     ,x_transacted                   OUT NOCOPY      VARCHAR2
     ,x_return_status                OUT NOCOPY      VARCHAR2
     ,x_material_detail_rec          OUT NOCOPY      gme_material_details%ROWTYPE)
   IS
      l_api_name    CONSTANT VARCHAR2 (30)          := 'update_material_line';
      l_factor               NUMBER;
      l_old_scrap            NUMBER;
      l_new_scrap            NUMBER;
      l_batch_status         NUMBER;
      l_status               NUMBER;
      l_qty                  NUMBER;
      l_eff_qty              NUMBER;
      l_old_plan_qty         NUMBER;
      l_new_plan_qty         NUMBER;
      l_old_wip_plan         NUMBER;
      l_new_wip_plan         NUMBER;
      l_proc                 VARCHAR2(100);
      l_rsc_count            NUMBER;
      l_message_count        NUMBER;
      l_message_list         VARCHAR2 (2000);

      l_compare_qty          NUMBER;  -- 13076579
      /*                                    
      BEGIN ER 19161894  Shaliu Chen 18-JUL-2014               
      Added for OPM Step Level OSP Project  
      */          
      l_propagate_change_to_po      NUMBER;
      l_osp_resource_flag           NUMBER;  
      l_batchstep_resource_id       NUMBER;    
      /*END ER 19161894*/
      l_ph_batch_header_rec  gme_batch_header%ROWTYPE;
      l_batch_header_rec     gme_batch_header%ROWTYPE;
      l_out_material_detail_tbl gme_common_pvt.material_details_tab;
      l_material_detail_tbl     gme_common_pvt.material_details_tab;
      l_trolin_tbl              inv_move_order_pub.trolin_tbl_type;

      l_mtl_dtl_rec          gme_material_details%ROWTYPE;
      l_ph_mtl_dtl_rec       gme_material_details%ROWTYPE;
      l_db_mtl_dtl_rec       gme_material_details%ROWTYPE;
      l_batch_step_rec       gme_batch_steps%ROWTYPE;
      l_exception_material_tbl gme_common_pvt.exceptions_tab;
      l_ph_batch_step_rec  gme_batch_steps%ROWTYPE;
      l_phantom_batch_header_rec_out  gme_batch_header%ROWTYPE;
      l_step_tbl     gme_reschedule_step_pvt.step_tab;
      x_batch_step_rec gme_batch_steps%ROWTYPE;
      error_dbl              EXCEPTION;
      error_processing       EXCEPTION;      
      qty_sync_fail          EXCEPTION; /*ER 19161894  Shaliu Chen 18-JUL-2014*/
      l_plan_cmplt_date   gme_batch_header.plan_cmplt_date%TYPE;
      
      /*                                    
      ER 19161894  Shaliu Chen 18-JUL-2014               
      Added for OPM Step Level OSP Project  
      */        
      CURSOR cur_get_osp_parameter(v_organization_id NUMBER)
      IS
         SELECT propagate_change_to_po
           FROM gme_parameters
          WHERE organization_id = v_organization_id; 
          
      /*                                    
      BEGIN ER 20809749                    
      Shaliu Chen 09-APR-2015               
      Get all of osp steps  
      */        
      CURSOR cur_get_all_osp_steps(v_batch_id NUMBER)
      IS
       SELECT batch_id,batchstep_id,batchstep_resource_id,organization_id
         FROM gme_batch_step_resources gbsr
        WHERE batch_id = v_batch_id
          AND EXISTS (SELECT 1
                        FROM cr_rsrc_dtl crd,
                             cr_rsrc_mst crm
                       WHERE crd.organization_id = gbsr.organization_id
                         AND crd.resources = gbsr.resources
                         AND crd.resources = crm.resources
                         AND crd.purchase_item_id IS NOT NULL
                         AND NVL(crm.outside_process_ind,gme_osp.g_osp_ind_disabled) = gme_osp.g_osp_ind_enabled);
      /*END ER 20809749*/                  

   BEGIN
      IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
         gme_debug.put_line('Entering api '||g_pkg_name||'.'||l_api_name);
         gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_batch_header_rec.batch_id='||p_batch_header_rec.batch_id);
         gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_material_detail_rec.material_detail_id='||p_material_detail_rec.material_detail_id);
         gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_batch_step_rec.batchstep_id='||p_batch_step_rec.batchstep_id);
         gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_scale_phantom='||p_scale_phantom);
         gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_trans_id='||p_trans_id);
      END IF;

      /* Set return status to success initially */
      x_return_status := fnd_api.g_ret_sts_success;

      l_mtl_dtl_rec := p_material_detail_rec;
      l_db_mtl_dtl_rec := p_stored_material_detail_rec;

      IF (l_mtl_dtl_rec.actual_qty <> l_db_mtl_dtl_rec.actual_qty) THEN
         -- can call this regardless of batch/step status... will also handle if batch is pending and qty is 0 (will do nothing)
         open_and_process_actual_qty
                          (p_batch_header_rec      => p_batch_header_rec
                          ,p_material_detail_rec   => l_mtl_dtl_rec
                          ,p_batch_step_rec        => p_batch_step_rec
                          ,p_trans_id              => NULL
                          ,p_insert                => FND_API.g_false
                          ,x_transacted            => x_transacted
                          ,x_return_status         => x_return_status);
         
         IF x_return_status <> fnd_api.g_ret_sts_success THEN
            l_proc := 'open_and_process_actual_qty';
            RAISE error_processing;
         END IF;
      END IF;

      -- Bug 21817332 - Recalculate original qty if the uom has changed.
      IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
         gme_debug.put_line(g_pkg_name||'.'||l_api_name||' l_mtl_dtl_rec.dtl_um = '||l_mtl_dtl_rec.dtl_um);
         gme_debug.put_line(g_pkg_name||'.'||l_api_name||' l_db_mtl_dtl_rec.dtl_um = '||l_db_mtl_dtl_rec.dtl_um);
         gme_debug.put_line(g_pkg_name||'.'||l_api_name||' l_mtl_dtl_rec.original_qty = '||l_mtl_dtl_rec.original_qty);
      END IF;

      IF (l_mtl_dtl_rec.dtl_um <> l_db_mtl_dtl_rec.dtl_um) THEN
         l_mtl_dtl_rec.original_qty := inv_convert.inv_um_convert(item_id         => l_mtl_dtl_rec.inventory_item_id
                                                            ,organization_id      => l_mtl_dtl_rec.organization_id
                                                            ,precision            => gme_common_pvt.g_precision
                                                            ,from_quantity        => l_mtl_dtl_rec.original_qty
                                                            ,from_unit            => l_db_mtl_dtl_rec.dtl_um
                                                            ,to_unit              => l_mtl_dtl_rec.dtl_um
                                                            ,from_name            => NULL
                                                            ,to_name              => NULL);

         IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
            gme_debug.put_line(g_pkg_name||'.'||l_api_name||'After conversion l_mtl_dtl_rec.original_qty = '||l_mtl_dtl_rec.original_qty);
         END IF;
      END IF;
     
      /*Sunitha Ch. Bug# 5391396  restructured the code and also added the code that will
        handle  the rescheduling batch/step when update yield Type of the Child batch is done.*/

      -- need to compare new and old of plan qty / wip plan qty for
      -- 1. calculating factor to scale phantom batch if p_scale_phantom
      --    is true
      -- 2. if they are different, need to update any move order lines
      --    with new qty
      -- batch_status is used to decide whether to use plan or wip_plan
      l_factor := 1;
      l_batch_status := p_batch_header_rec.batch_status;

      IF l_batch_status = gme_common_pvt.g_batch_pending THEN
         l_qty := l_mtl_dtl_rec.plan_qty;
         --Bug#4965141 check for zero plan qty
         IF l_db_mtl_dtl_rec.plan_qty = 0 THEN
            l_eff_qty := 1;
         ELSE
            l_eff_qty := l_db_mtl_dtl_rec.plan_qty;
         END IF;
         l_factor := l_mtl_dtl_rec.plan_qty / l_eff_qty;
      ELSIF l_batch_status = gme_common_pvt.g_batch_wip THEN
         l_qty := l_mtl_dtl_rec.wip_plan_qty;
         --Bug#4965141 check for zero wip plan qty
         IF NVL(l_db_mtl_dtl_rec.wip_plan_qty,0) = 0 THEN
            l_eff_qty := 1;
         ELSE
            l_eff_qty := l_db_mtl_dtl_rec.wip_plan_qty;
         END IF;
         l_factor := l_mtl_dtl_rec.wip_plan_qty / l_eff_qty;
      END IF;
      
      IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
         gme_debug.put_line(g_pkg_name||'.'||l_api_name||' l_qty='||l_qty);
         gme_debug.put_line(g_pkg_name||'.'||l_api_name||' l_factor='||l_factor);
      END IF;

      l_status := p_batch_header_rec.batch_status;
      IF l_mtl_dtl_rec.release_type = gme_common_pvt.g_mtl_autobystep_release AND
         p_batch_step_rec.batchstep_id IS NOT NULL THEN
         l_status := p_batch_step_rec.step_status;
      END IF;

      -- if scrap was changed and plan/wip plan (based on batch status)
      -- was not changed, then recalculate plan/wip plan
      l_old_scrap := l_db_mtl_dtl_rec.scrap_factor;
      l_new_scrap := l_mtl_dtl_rec.scrap_factor;

      l_old_plan_qty := l_db_mtl_dtl_rec.plan_qty;
      l_new_plan_qty := l_mtl_dtl_rec.plan_qty;

      l_old_wip_plan := l_db_mtl_dtl_rec.wip_plan_qty;
      l_new_wip_plan := l_mtl_dtl_rec.wip_plan_qty;

      IF l_old_scrap <> l_new_scrap AND 
         l_status = gme_common_pvt.g_batch_pending AND 
         l_old_plan_qty = l_new_plan_qty THEN
         
         l_old_plan_qty := x_material_detail_rec.plan_qty / (1 + l_old_scrap);
         x_material_detail_rec.plan_qty := l_old_plan_qty * (1 + x_material_detail_rec.scrap_factor);
      ELSIF l_old_scrap <> l_new_scrap AND 
            l_status = gme_common_pvt.g_batch_wip AND 
            l_old_wip_plan = l_new_wip_plan THEN
            
         l_old_plan_qty := x_material_detail_rec.wip_plan_qty / (1 + l_old_scrap);
         x_material_detail_rec.wip_plan_qty := l_old_plan_qty * (1 + x_material_detail_rec.scrap_factor);
      END IF;
      
      /* 5391396 moved the code up*/
      IF l_mtl_dtl_rec.release_type <> l_db_mtl_dtl_rec.release_type THEN        
         gme_common_pvt.calc_mtl_req_date
                   (p_batch_header_rec      => p_batch_header_rec
                   ,p_batchstep_rec         => p_batch_step_rec
                   ,p_mtl_dtl_rec           => l_mtl_dtl_rec
                   ,x_mtl_req_date          => l_mtl_dtl_rec.material_requirement_date
                   ,x_return_status         => x_return_status);
         
         IF x_return_status <> fnd_api.g_ret_sts_success THEN
            l_proc := 'gme_common_pvt.calc_mtl_req_date';
            RAISE error_processing;
         END IF;

         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
             gme_debug.put_line (   g_pkg_name
                                 || '.'
                                 || l_api_name
                                 || ' after gme_common_pvt.calc_mtl_req_date');
             gme_debug.put_line
                            (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ' material_requirement_date= '
                             || TO_CHAR
                                      (l_mtl_dtl_rec.material_requirement_date
                                      ,'YYYY-MON-DD HH24:MI:SS') );
         END IF;
         
         /* Pawan Kumar bug  5127489 Changed so as to change the move order 
             and reservation dates */
         gme_common_pvt.material_date_change (
           p_material_detail_id   => l_mtl_dtl_rec.material_detail_id
          ,p_material_date        => l_mtl_dtl_rec.material_requirement_date
          ,x_return_status        => x_return_status );
         
         IF x_return_status <> fnd_api.g_ret_sts_success THEN
            l_proc := 'gme_common_pvt.material_date_change';
            RAISE error_processing;
         END IF;
         
          --Bug#5159393 Susruth D. 
         l_mtl_dtl_rec.last_update_date := gme_common_pvt.g_timestamp;
         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line (   g_pkg_name
                                 || '.'
                                 || l_api_name
                                 || ' after gme_common_pvt.material_date_change');
            gme_debug.put_line
                            (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ' material_requirement_date= '
                             || TO_CHAR
                                      (l_mtl_dtl_rec.material_requirement_date
                                      ,'YYYY-MON-DD HH24:MI:SS') );
         END IF;
      END IF;--IF l_mtl_dtl_rec.release_type <> l_db_mtl_dtl_rec.release_type

      /* 5391396 seperate the condition */
         
      -- check if this is a phantom ... if so, update the partner phantom
      /* Bug 4867497 added subinventory locator and qty logic and moved this out of the       
         IF l_mtl_dtl_rec.release_type <> l_db_mtl_dtl_rec.release_type THEN condn */

      -- To Do Put an IF condition to check if material_requirement_date has changed or release_type has changed. 
      -- Only if either has changed, then we need to do the re-schedule stuff. 

      IF l_mtl_dtl_rec.phantom_line_id IS NOT NULL THEN
         l_ph_mtl_dtl_rec.material_detail_id := l_mtl_dtl_rec.phantom_line_id;
         IF NOT gme_material_details_dbl.fetch_row(l_ph_mtl_dtl_rec, l_ph_mtl_dtl_rec) THEN
            l_proc := 'gme_material_details_dbl.fetch_row';
            RAISE error_dbl;
         END IF;
         l_ph_mtl_dtl_rec.material_requirement_date := l_mtl_dtl_rec.material_requirement_date;
         l_ph_mtl_dtl_rec.release_type              := l_mtl_dtl_rec.release_type;
         l_ph_mtl_dtl_rec.subinventory              := l_mtl_dtl_rec.subinventory;
         l_ph_mtl_dtl_rec.locator_id                := l_mtl_dtl_rec.locator_id;
         
	      -- sunitha ch. bug 5566769 update the revision field of the phantom batch
	      l_ph_mtl_dtl_rec.revision                := l_mtl_dtl_rec.revision;
         IF l_status = gme_common_pvt.g_batch_pending AND (l_mtl_dtl_rec.plan_qty <> l_db_mtl_dtl_rec.plan_qty) THEN
            IF (l_mtl_dtl_rec.dtl_um = l_ph_mtl_dtl_rec.dtl_um) THEN
               l_ph_mtl_dtl_rec.plan_qty := l_mtl_dtl_rec.plan_qty;
            ELSE
               l_ph_mtl_dtl_rec.plan_qty := inv_convert.inv_um_convert(item_id              => l_mtl_dtl_rec.inventory_item_id
                                                                  ,organization_id      => l_mtl_dtl_rec.organization_id
                                                                  ,precision            => gme_common_pvt.g_precision
                                                                  ,from_quantity        => l_mtl_dtl_rec.plan_qty
                                                                  ,from_unit            => l_mtl_dtl_rec.dtl_um
                                                                  ,to_unit              => l_ph_mtl_dtl_rec.dtl_um
                                                                  ,from_name            => NULL
                                                                  ,to_name              => NULL);
            END IF;
         ELSIF (NVL(l_mtl_dtl_rec.wip_plan_qty,0) <> NVL(l_db_mtl_dtl_rec.wip_plan_qty,0)) THEN
            IF (l_mtl_dtl_rec.dtl_um = l_ph_mtl_dtl_rec.dtl_um) THEN             
               l_ph_mtl_dtl_rec.wip_plan_qty := l_mtl_dtl_rec.wip_plan_qty;
            ELSE
               l_ph_mtl_dtl_rec.wip_plan_qty := inv_convert.inv_um_convert(item_id             => l_mtl_dtl_rec.inventory_item_id
                                                                         ,organization_id      => l_mtl_dtl_rec.organization_id
                                                                         ,precision            => gme_common_pvt.g_precision
                                                                         ,from_quantity        => l_mtl_dtl_rec.wip_plan_qty
                                                                         ,from_unit            => l_mtl_dtl_rec.dtl_um
                                                                         ,to_unit              => l_ph_mtl_dtl_rec.dtl_um
                                                                         ,from_name            => NULL
                                                                         ,to_name              => NULL);
            END IF;
         END IF;
         /* End Bug 4867497 */

         IF l_db_mtl_dtl_rec.phantom_id IS NOT NULL THEN
            IF (l_db_mtl_dtl_rec.line_type = gme_common_pvt.g_line_type_ing ) THEN
               /*REWORK Sunitha ch. Bug 5353941 Check whether phantom material is associated to 
               step and call reschedule batch if it is not associated to step or call
               rescedule step for that batch if it is associated to step*/
               l_ph_batch_header_rec.batch_id := l_db_mtl_dtl_rec.phantom_id;
               IF NOT gme_batch_header_dbl.fetch_row(l_ph_batch_header_rec, l_ph_batch_header_rec) THEN
                  l_proc := 'gme_batch_header_dbl.fetch_row';
                  RAISE error_dbl;
               END IF;
            
               IF p_scale_phantom = FND_API.G_TRUE AND l_factor <> 1 THEN               
                  gme_scale_batch_pvt.scale_batch
                                (p_batch_header_rec            => l_ph_batch_header_rec
                                ,p_scale_factor                => l_factor
                                ,p_primaries                   => 'OUTPUTS'
                                ,p_qty_type                    => 1
                                ,p_validity_rule_id            => l_ph_batch_header_rec.recipe_validity_rule_id
                                ,p_enforce_vldt_check          => fnd_api.g_true
                                ,p_recalc_dates                => fnd_api.g_false
                                ,p_use_workday_cal             => fnd_api.g_false
                                ,p_contiguity_override         => fnd_api.g_true
                                ,x_exception_material_tbl      => l_exception_material_tbl
                                ,x_batch_header_rec            => l_batch_header_rec
                                ,x_return_status               => x_return_status);
                  
                  IF x_return_status <> fnd_api.g_ret_sts_success THEN
                     l_proc := 'gme_scale_batch_pvt.scale_batch';
                     RAISE error_processing;
                  END IF;
               END IF; --IF l_factor <> 1
            
               IF (l_mtl_dtl_rec.material_requirement_date <> l_db_mtl_dtl_rec.material_requirement_date )  THEN
                  IF(l_mtl_dtl_rec.release_type = gme_common_pvt.g_mtl_autobystep_release AND 
                     gme_common_pvt.is_material_assoc_to_step(l_ph_mtl_dtl_rec.material_detail_id ))  THEN
                  
                     /* REWORK Sunitha Bug 5353941. 
                        Following select statement will only execute for ingredient of parent batch and 
                        never for product of phantom batch as we have the check of line type to be ingredient. */                  
                     l_ph_batch_step_rec.batch_id:=l_db_mtl_dtl_rec.phantom_id;
                  
                     SELECT batchstep_id INTO l_ph_batch_step_rec.batchstep_id
                       FROM gme_batch_step_items
                      WHERE batch_id = l_mtl_dtl_rec.phantom_id
                        AND material_detail_id = l_ph_mtl_dtl_rec.material_detail_id;
                        
                     IF NOT gme_batch_steps_dbl.fetch_row(l_ph_batch_step_rec, l_ph_batch_step_rec) THEN
                        l_proc := 'gme_batch_steps_dbl.fetch_row';
                        RAISE error_dbl;
                     END IF;
                     l_ph_batch_step_rec.plan_cmplt_date := l_mtl_dtl_rec.material_requirement_date;
		               l_ph_batch_step_rec.plan_start_date := NULL;
                     gme_reschedule_step_pvt.reschedule_step
                                   (p_batch_step_rec             => l_ph_batch_step_rec
                                   ,p_source_step_id_tbl         => l_step_tbl
                                   ,p_contiguity_override        => fnd_api.g_true
                                   ,p_reschedule_preceding       => fnd_api.g_true
                                   ,p_reschedule_succeeding      => fnd_api.g_true
                                   ,p_use_workday_cal            => fnd_api.g_false
                                   ,x_batch_step_rec             => x_batch_step_rec
                                   ,x_return_status              => x_return_status);
                  ELSE
                     l_ph_batch_header_rec.plan_cmplt_date := l_mtl_dtl_rec.material_requirement_date;
                     l_ph_batch_header_rec.plan_start_date := NULL;
                     
                     gme_reschedule_batch_pvt.reschedule_batch
                        (p_batch_header_rec         => l_ph_batch_header_rec
                        ,p_use_workday_cal          => fnd_api.g_false
                        ,p_contiguity_override      => fnd_api.g_true
                        ,x_batch_header_rec         => l_phantom_batch_header_rec_out
                        ,x_return_status            => x_return_status);
                  END IF;
               END IF; --l_mtl_dtl_rec.material_requirement_date <> l_db_mtl_dtl_rec.material_requirement_date
               /* End Bug 5353941 REWORK*/
            END IF;--IF l_db_mtl_dtl_rec.line_type = gme_common_pvt.g_line_type_ing
         ELSE -- if it is phantom Product
            /* Sunitha Ch. Bug#5391396  rescheduling batch/step when update yield Type of the Child batch is done */
	         IF (l_mtl_dtl_rec.release_type = gme_common_pvt.g_mtl_autobystep_release AND
	             gme_common_pvt.is_material_assoc_to_step(l_ph_mtl_dtl_rec.material_detail_id )) THEN
	            SELECT plan_start_date
                 INTO  l_plan_cmplt_date
                 FROM gme_batch_steps
                WHERE batch_id = l_ph_mtl_dtl_rec.batch_id
                  AND batchstep_id = (SELECT batchstep_id
                                        FROM gme_batch_step_items
                                       WHERE batch_id = l_ph_mtl_dtl_rec.batch_id
                                         AND material_detail_id = l_ph_mtl_dtl_rec.material_detail_id );             
	         ELSE
	            SELECT plan_start_date
                 INTO l_plan_cmplt_date
                 FROM gme_batch_header
                WHERE batch_id = l_ph_mtl_dtl_rec.batch_id;           
	         END IF;
	         
	         l_mtl_dtl_rec.material_requirement_date:=l_plan_cmplt_date;
            l_ph_mtl_dtl_rec.material_requirement_date := l_mtl_dtl_rec.material_requirement_date;
            
	         IF (l_mtl_dtl_rec.release_type = gme_common_pvt.g_mtl_autobystep_release AND 
               gme_common_pvt.is_material_assoc_to_step(l_mtl_dtl_rec.material_detail_id ))  THEN

               /* Sunitha Bug  5391396 . Following select statement will only execute for ingredient of parent batch and never for product of phantom batch as we have the check of line type to be ingredient. */
               l_batch_step_rec := p_batch_step_rec;
	            l_batch_step_rec.plan_start_date := NULL;
	            l_batch_step_rec.plan_cmplt_date := l_plan_cmplt_date;
               gme_reschedule_step_pvt.reschedule_step
                            (p_batch_step_rec             => l_batch_step_rec
                            ,p_source_step_id_tbl         => l_step_tbl
                            ,p_contiguity_override        => fnd_api.g_true
                            ,p_reschedule_preceding       => fnd_api.g_true
                            ,p_reschedule_succeeding      => fnd_api.g_true
                            ,p_use_workday_cal            => fnd_api.g_false
                            ,x_batch_step_rec             => x_batch_step_rec
                            ,x_return_status              => x_return_status);
            ELSE
               l_batch_header_rec := p_batch_header_rec;
               l_batch_header_rec.plan_start_date := NULL;
	            l_batch_header_rec.plan_cmplt_date := l_plan_cmplt_date;
               gme_reschedule_batch_pvt.reschedule_batch
               (p_batch_header_rec         => l_batch_header_rec
               ,p_use_workday_cal          => fnd_api.g_false
               ,p_contiguity_override      => fnd_api.g_true
               ,x_batch_header_rec         => l_phantom_batch_header_rec_out
               ,x_return_status            => x_return_status);
            END IF;
 	      END IF; --IF l_db_mtl_dtl_rec.phantom_id IS NOT NULL

         /* Sunitha REWORK Bug 5353941. We do not need to call fetch_row, as this will override the updated values of material_requirement_date, release_type, subinventory and locator that have been set above. */
         SELECT last_update_date INTO l_ph_mtl_dtl_rec.last_update_date
           FROM gme_material_details
          WHERE batch_id = l_ph_mtl_dtl_rec.batch_id
            AND material_detail_id = l_ph_mtl_dtl_rec.material_detail_id;

         IF NOT gme_material_details_dbl.update_row (l_ph_mtl_dtl_rec) THEN
            l_proc := 'gme_material_details_dbl.update_row';
            RAISE error_dbl;
         END IF;
      END IF;  -- IF l_mtl_dtl_rec.phantom_line_id IS NOT NULL THEN
      
      -- Bug 13076579 - Restructured following condition.
/*
      IF l_factor <> 1 THEN  -- will only be for pending and WIP
         gme_move_orders_pvt.update_move_order_lines
             (p_batch_id             => l_mtl_dtl_rec.batch_id
             ,p_material_detail_id   => l_mtl_dtl_rec.material_detail_id
             ,p_new_qty              => l_qty
             ,p_new_date             => l_mtl_dtl_rec.material_requirement_date
             ,p_invis_move_line_id   => NULL
             ,x_return_status        => x_return_status);
         
         IF x_return_status <> fnd_api.g_ret_sts_success THEN
            l_proc := 'gme_move_orders_pvt.update_move_order_lines';
            RAISE error_processing;
         END IF;
      END IF;
*/
      IF l_factor <> 1 THEN  -- will only be for pending and WIP
         -- Bug 13076579 - Update move orders only when user is lowering qty from original. 
         -- l_status is the batch_status and l_qty is the new qty.
         l_compare_qty := NVL(l_db_mtl_dtl_rec.wip_plan_qty, 0);
         IF l_status = 1 THEN
            l_compare_qty := l_db_mtl_dtl_rec.plan_qty;
         END IF;
         
         -- Bug 13076579 - Update move orders only when decreasing.
         -- Increase differences will be accounted for when user does picking.
         IF (l_qty < l_compare_qty) THEN
            -- Bug 14612460 - if the new qty is zero then we just need to delete the move orders.
            IF (l_qty > 0) THEN
               gme_move_orders_pvt.update_move_order_lines
                   (p_batch_id             => l_mtl_dtl_rec.batch_id
                   ,p_material_detail_id   => l_mtl_dtl_rec.material_detail_id
                   ,p_new_qty              => l_qty
                   ,p_new_date             => l_mtl_dtl_rec.material_requirement_date
                   ,p_invis_move_line_id   => NULL
                   ,x_return_status        => x_return_status);
            
               IF x_return_status <> fnd_api.g_ret_sts_success THEN
                  l_proc := 'gme_move_orders_pvt.update_move_order_lines';
                  RAISE error_processing;
               END IF;
            ELSE
               gme_move_orders_pvt.delete_move_order_lines
                    (p_organization_id         => l_mtl_dtl_rec.organization_id
                    ,p_batch_id                => l_mtl_dtl_rec.batch_id
                    ,p_material_detail_id      => l_mtl_dtl_rec.material_detail_id
                    ,p_invis_move_line_id      => NULL
                    ,x_return_status           => x_return_status);
             
               IF x_return_status <> fnd_api.g_ret_sts_success THEN
                  l_proc := 'gme_move_orders_pvt.delete_move_order_lines';
                  RAISE error_processing;
               END IF;            
            END IF;
         END IF;
      END IF;
            
      IF l_mtl_dtl_rec.phantom_type <> l_db_mtl_dtl_rec.phantom_type THEN
         IF l_mtl_dtl_rec.phantom_type IN (1,2) THEN  -- phantom ing should not have invisible mo line
            gme_move_orders_pvt.delete_move_order_lines
              (p_organization_id         => p_batch_header_rec.organization_id
              ,p_batch_id                => p_batch_header_rec.batch_id
              ,p_material_detail_id      => l_mtl_dtl_rec.material_detail_id
              ,p_invis_move_line_id      => l_mtl_dtl_rec.move_order_line_id
              ,x_return_status           => x_return_status);
            
            IF x_return_status <> fnd_api.g_ret_sts_success THEN
               l_proc := 'gme_move_orders_pvt.delete_move_order_lines';
               RAISE error_processing;
            END IF;
            
            l_mtl_dtl_rec.move_order_line_id := NULL;
         ELSE   -- phantom type is changed to 0 -> not a phantom, so the invisible mo line must be created
            l_material_detail_tbl (1) := l_mtl_dtl_rec;
            
            -- add material line into invisible move order
            gme_move_orders_pvt.create_move_order_lines
              (p_move_order_header_id       => p_batch_header_rec.move_order_header_id
              ,p_move_order_type            => gme_common_pvt.g_invis_move_order_type
              ,p_material_details_tbl       => l_material_detail_tbl
              ,x_material_details_tbl       => l_out_material_detail_tbl
              ,x_trolin_tbl                 => l_trolin_tbl
              ,x_return_status              => x_return_status);
            
            IF x_return_status <> FND_API.G_RET_STS_SUCCESS THEN
               l_proc := 'gme_move_orders_pvt.create_move_order_lines';
               RAISE error_processing;
            END IF;
            
            l_mtl_dtl_rec := l_out_material_detail_tbl(1);
         END IF;
      END IF;

      -- 4944024 BEGIN 
      -- If there is a decrease in anticipated yield, then reservations associated to this supply
      -- need to be decreased
      -- ========================================================================================
      IF l_mtl_dtl_rec.line_type <> -1 AND
        (l_mtl_dtl_rec.plan_qty < l_db_mtl_dtl_rec.plan_qty OR
         l_mtl_dtl_rec.wip_plan_qty < l_db_mtl_dtl_rec.wip_plan_qty OR
         l_mtl_dtl_rec.dtl_um <> l_db_mtl_dtl_rec.dtl_um) THEN
         IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
            gme_debug.put_line(g_pkg_name||'.'||l_api_name||' Invoking relieve_prod_supply_resv' );
         END IF;
         gme_supply_res_pvt.relieve_prod_supply_resv (
             p_matl_dtl_rec         => l_mtl_dtl_rec
            ,x_msg_count            => l_message_count
            ,x_msg_data             => l_message_list                           
            ,x_return_status        => x_return_status);                    
         
         IF x_return_status <> FND_API.G_RET_STS_SUCCESS THEN
            l_proc := 'gme_reservations_pvt.relieve_prod_supply_resv ';
            RAISE error_processing;
         END IF;
      END IF;
      -- 4944024 KYH END
          
      SELECT last_update_date INTO l_mtl_dtl_rec.last_update_date
        FROM gme_material_details
       WHERE batch_id = l_mtl_dtl_rec.batch_id
         AND material_detail_id =  l_mtl_dtl_rec.material_detail_id;       
      IF NOT gme_material_details_dbl.update_row (l_mtl_dtl_rec) THEN
         l_proc := 'gme_material_details_dbl.update_row';
         RAISE error_dbl;
      -- nsinghi bug#5208923. added the else part.
      ELSE
         gme_common_pvt.get_who(x_user_ident    => x_material_detail_rec.last_updated_by,
                            x_login_id      => x_material_detail_rec.last_update_login,
                            x_timestamp     => x_material_detail_rec.last_update_date,
                            x_return_status => x_return_status);
         IF x_return_status <> FND_API.G_RET_STS_SUCCESS THEN
            l_proc := 'gme_common_pvt.get_who ';
            RAISE error_processing;
         END IF;
      END IF;

      IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ' after gme_material_details_dbl.update_row');
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ' successfully updated material_detail_id= '
                             || TO_CHAR (l_mtl_dtl_rec.material_detail_id) );
      END IF;

      IF p_batch_step_rec.batchstep_id IS NOT NULL THEN
         gme_trans_engine_util.load_rsrc_trans(p_batch_row          => p_batch_header_rec
                                              ,x_rsc_row_count      => l_rsc_count
                                              ,x_return_status      => x_return_status);

         IF x_return_status <> fnd_api.g_ret_sts_success THEN
            l_proc := 'gme_trans_engine_util.load_rsrc_trans';
            RAISE error_processing;
         END IF;
         gme_update_step_qty_pvt.update_step_qty
                                        (p_batch_step_rec      => p_batch_step_rec
                                        ,x_message_count       => l_message_count
                                        ,x_message_list        => l_message_list
                                        ,x_return_status       => x_return_status
                                        ,x_batch_step_rec      => l_batch_step_rec);

         IF x_return_status <> fnd_api.g_ret_sts_success THEN
            l_proc := 'gme_update_step_qty_pvt.update_step_qty';
            RAISE error_processing;
         END IF;

         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line
                (   g_pkg_name
                 || '.'
                 || l_api_name
                 || ': '
                 || ' after gme_update_step_qty_pvt.update_step_qty: successful');
         END IF;
      END IF;
      
      /*                                    
      BEGIN ER 19161894  Shaliu Chen 18-JUL-2014               
      Added for OPM Step Level OSP Project  
      */            
      IF g_debug <= gme_debug.g_log_statement THEN
             gme_debug.put_line (   g_pkg_name
                                 || '.'
                                 || l_api_name
                                 || ' Calling quantity synchronization API');
      END IF;

      BEGIN
        /*
         Check GME parameters
        */
        OPEN cur_get_osp_parameter(p_batch_header_rec.organization_id);
        FETCH cur_get_osp_parameter INTO l_propagate_change_to_po;
        CLOSE cur_get_osp_parameter;

        IF (NVL(l_propagate_change_to_po,gme_osp.g_propagate_change_manual) 
                   = gme_osp.g_propagate_change_automatic)
             AND gme_osp.check_release_version THEN
           /* 
            Check whether the batch include Outside resource.
           */
          SELECT count(1)
            INTO l_osp_resource_flag
            FROM gme_batch_step_resources gbsr
           WHERE batch_id = p_batch_header_rec.batch_id
             AND EXISTS (SELECT 1
                           FROM cr_rsrc_dtl crd,
                                cr_rsrc_mst crm
                          WHERE crd.organization_id = gbsr.organization_id
                            AND crd.resources = gbsr.resources
                            AND crd.resources = crm.resources
                            AND crd.purchase_item_id IS NOT NULL
                            AND NVL(crm.outside_process_ind,gme_osp.g_osp_ind_disabled) = gme_osp.g_osp_ind_enabled);

          /*                                    
           BEGIN ER 20809749                    
           Shaliu Chen 09-APR-2015               
           Change to loop each osp step and invoke requisition creation for each osp step. 
          */                                     
          IF l_osp_resource_flag > 0 THEN
            FOR cur_gaos IN cur_get_all_osp_steps(p_batch_header_rec.batch_id) LOOP 
              l_batchstep_resource_id := cur_gaos.batchstep_resource_id;
              /*
                Invoke Batch product quantity change to the PO/Req
                linked to the batch.
              */                           
              gme_osp.updatePOReqQuantity(p_batch_id                    => p_batch_header_rec.batch_id,
                                           p_organization_id             => p_batch_header_rec.organization_id,
                                           p_batchstep_resource_id       => l_batchstep_resource_id,                              
                                           x_return_status               => x_return_status,
                                           x_message_list                => l_message_list,
                                           x_message_count               => l_message_count);
                                                                                                             
              IF x_return_status <> fnd_api.g_ret_sts_success THEN                       
                RAISE qty_sync_fail; 
              END IF;
            END LOOP;
          END IF;
        END IF;
      EXCEPTION
        WHEN OTHERS THEN
          l_message_list := 'unexpected error: ' || SQLERRM || 'SQLCODE = ' || SQLCODE;
          gme_debug.put_line('l_errMsg:'||l_message_list);
          RAISE qty_sync_fail;          
      END;         
      /*END ER 19161894*/
      IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
         gme_debug.put_line('Exiting api '||g_pkg_name||'.'||l_api_name);
      END IF;

   EXCEPTION
      WHEN error_processing THEN
         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
           gme_debug.put_line (g_pkg_name||'.'||l_api_name|| ': ' || l_proc|| ' error returned');
         END IF;
      WHEN error_dbl THEN
         --commented by qzeng, error message has been logged at lower level
         --gme_common_pvt.log_message ('GME_UNEXPECTED_ERROR', 'ERROR', SQLERRM);
         x_return_status := FND_API.g_ret_sts_unexp_error;

         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
           gme_debug.put_line (g_pkg_name||'.'||l_api_name|| ': '
                                                          || l_proc|| ' unexpected error: '|| SQLERRM);
         END IF;
      /*ER 19161894  Shaliu Chen 18-JUL-2014*/   
      WHEN qty_sync_fail THEN
        gme_common_pvt.log_message('GME_QTY_SYNC_FAILED');
        x_return_status := 'W';           
      WHEN OTHERS THEN
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);

         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line (   'Unexpected error: '
                                || g_pkg_name
                                || '.'
                                || l_api_name
                                || ': '
                                || SQLERRM);
         END IF;

         x_return_status := fnd_api.g_ret_sts_unexp_error;
   END update_material_line;

   --Bug#5078853 removed p_validate_flexfields parameter
   PROCEDURE val_and_pop_material_for_upd (
      p_batch_header_rec             IN       gme_batch_header%ROWTYPE
     ,p_material_detail_rec          IN       gme_material_details%ROWTYPE
     ,p_stored_material_detail_rec   IN       gme_material_details%ROWTYPE
     ,p_batch_step_rec               IN       gme_batch_steps%ROWTYPE
     ,x_material_detail_rec          OUT NOCOPY     gme_material_details%ROWTYPE
     ,x_return_status                OUT NOCOPY     VARCHAR2)
   IS
      l_item_rec            mtl_system_items_b%ROWTYPE;
      l_batch_status        NUMBER;
      l_status              NUMBER;
      l_step_status         NUMBER;
      l_material_detail_rec gme_material_details%ROWTYPE;

      l_scale_type          NUMBER;
      l_scale_round_var     NUMBER;
      l_val_proc            VARCHAR2 (100);
      l_api_name   CONSTANT VARCHAR2 (30)   := 'val_and_pop_material_for_upd';

      l_field               VARCHAR2(100);

      error_no_upd          EXCEPTION;
      val_error             EXCEPTION;
      expected_error        EXCEPTION;
      error_no_null         EXCEPTION;
   BEGIN
      IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
        gme_debug.put_line('Entering api '||g_pkg_name||'.'||l_api_name);
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_batch_header_rec.batch_id='||p_batch_header_rec.batch_id);
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_material_detail_rec.material_detail_id='||p_material_detail_rec.material_detail_id);
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_batch_step_rec.batchstep_id='||p_batch_step_rec.batchstep_id);        
      END IF;

      /* Set return status to success initially */
      x_return_status := fnd_api.g_ret_sts_success;

      -- Following are not supported for update in API and will be ignored...
      -- cost
      -- alloc_ind
      -- text_code
      x_material_detail_rec := p_stored_material_detail_rec;
      l_material_detail_rec := p_material_detail_rec;

      l_batch_status := p_batch_header_rec.batch_status;
      l_step_status := p_batch_step_rec.step_status;

      IF p_material_detail_rec.formulaline_id IS NOT NULL THEN
         gme_common_pvt.log_message ('GME_FORMID_CHG_NOT_ALLOWED');
         RAISE error_no_upd;
      END IF;

      IF p_material_detail_rec.inventory_item_id IS NOT NULL THEN
         gme_common_pvt.log_message ('GME_ITEMID_CHG_NOT_ALLOWED');
         RAISE error_no_upd;
      END IF;
      
      --Bug#5078853
      IF p_material_detail_rec.phantom_line_id IS NOT NULL  OR 
         p_material_detail_rec.backordered_qty IS NOT NULL OR
         p_material_detail_rec.original_primary_qty IS NOT NULL OR
         p_material_detail_rec.move_order_line_id IS NOT NULL THEN
         gme_common_pvt.log_message ('GME_FIELD_CHG_NOT_ALLOWED');
         RAISE error_no_upd;
      END IF;

      get_item_rec
                 (p_org_id             => p_stored_material_detail_rec.organization_id
                 ,p_item_id            => p_stored_material_detail_rec.inventory_item_id
                 ,x_item_rec           => l_item_rec
                 ,x_return_status      => x_return_status);

      IF x_return_status <> fnd_api.g_ret_sts_success THEN
         RAISE expected_error;
      END IF;

      IF p_material_detail_rec.revision IS NOT NULL THEN
         IF p_material_detail_rec.revision = fnd_api.g_miss_char THEN
            x_material_detail_rec.revision := NULL;
         ELSE
            x_material_detail_rec.revision := p_material_detail_rec.revision;
	    --added by qzeng no need to check if done in bulk validation
            IF NVL(gme_common_pvt.g_bulk_validation_done, 'N') = 'N' THEN
              validate_revision
                               (p_revision           => x_material_detail_rec.revision
                               ,p_item_rec           => l_item_rec
                               ,x_return_status      => x_return_status);

              IF x_return_status <> fnd_api.g_ret_sts_success THEN
                l_val_proc := 'validate_revisioin';
                RAISE val_error;
              END IF;
	    END IF;
         END IF;
      END IF;

      IF x_material_detail_rec.line_type = gme_common_pvt.g_line_type_byprod THEN
         IF p_material_detail_rec.by_product_type IS NOT NULL THEN
            IF p_material_detail_rec.by_product_type = fnd_api.g_miss_char THEN
               x_material_detail_rec.by_product_type := NULL;
            ELSE
               x_material_detail_rec.by_product_type :=
                                        p_material_detail_rec.by_product_type;
            END IF;

            validate_byproduct_type
                   (p_byproduct_type      => x_material_detail_rec.by_product_type
                   ,x_return_status       => x_return_status);

            IF x_return_status <> fnd_api.g_ret_sts_success THEN
               l_val_proc := 'validate_byproduct_type';
               RAISE val_error;
            END IF;

         END IF;
      ELSE
         x_material_detail_rec.by_product_type := NULL;
      END IF;

      IF p_material_detail_rec.release_type IS NOT NULL THEN
         IF p_batch_header_rec.batch_status = gme_common_pvt.g_batch_pending THEN
            IF p_material_detail_rec.release_type = fnd_api.g_miss_num THEN
               l_field := 'release_type';
               RAISE error_no_null;
            ELSE
               x_material_detail_rec.release_type :=
                                           p_material_detail_rec.release_type;
               validate_release_type
                       (p_material_detail_rec      => x_material_detail_rec
                       ,p_release_type             => x_material_detail_rec.release_type
                       ,x_return_status            => x_return_status);

               IF x_return_status <> fnd_api.g_ret_sts_success THEN
                  l_val_proc := 'validate_release_type';
                  RAISE val_error;
               END IF;
            END IF;
         ELSE
            gme_common_pvt.log_message ('GME_INV_STAT_UPD_REL');
            RAISE expected_error;
         END IF;
      END IF;

      -- Set status of material based on release type, assocation and step status
      IF     x_material_detail_rec.release_type =
                                      gme_common_pvt.g_mtl_autobystep_release
         AND p_batch_step_rec.batchstep_id IS NOT NULL THEN
         l_status := p_batch_step_rec.step_status;
      ELSE
         l_status := p_batch_header_rec.batch_status;
      END IF;

      IF p_material_detail_rec.plan_qty IS NOT NULL THEN
         IF l_batch_status = gme_common_pvt.g_batch_pending THEN
            IF p_material_detail_rec.plan_qty = fnd_api.g_miss_num THEN
               l_field := 'plan_qty';
               RAISE error_no_null;
            ELSE
               x_material_detail_rec.plan_qty :=
                                               NVL(p_material_detail_rec.plan_qty, 0);
               validate_plan_qty
                               (p_plan_qty           => x_material_detail_rec.plan_qty
                               ,x_return_status      => x_return_status);

               IF x_return_status <> fnd_api.g_ret_sts_success THEN
                  l_val_proc := 'validate_plan_qty';
                  RAISE val_error;
               END IF;
            END IF;
         ELSE
            gme_common_pvt.log_message ('GME_INV_STAT_UPD_PLAN_QTY');
            RAISE expected_error;
         END IF;
      END IF;

      IF p_material_detail_rec.dtl_um IS NOT NULL THEN
         IF l_batch_status = gme_common_pvt.g_batch_pending THEN
            IF p_material_detail_rec.dtl_um = fnd_api.g_miss_char THEN
               l_field := 'dtl_um';
               RAISE error_no_null;
            ELSE
               x_material_detail_rec.dtl_um :=
                                               p_material_detail_rec.dtl_um;

               validate_dtl_um
                      (p_dtl_um             => x_material_detail_rec.dtl_um
                      ,p_primary_uom        => l_item_rec.primary_uom_code
                      ,p_item_id            => p_material_detail_rec.inventory_item_id
                      ,p_org_id             => p_material_detail_rec.organization_id
                      ,x_return_status      => x_return_status);

               IF x_return_status <> fnd_api.g_ret_sts_success THEN
                  l_val_proc := 'validate_dtl_um';
                  RAISE val_error;
               END IF;
            END IF;
         ELSE
            gme_common_pvt.log_message ('GME_INV_STAT_UPD_DTL_UM');
            RAISE expected_error;
         END IF;
      END IF;

      IF p_material_detail_rec.wip_plan_qty IS NOT NULL THEN
         IF p_batch_header_rec.batch_status = gme_common_pvt.g_batch_wip THEN
            IF p_material_detail_rec.wip_plan_qty = fnd_api.g_miss_num THEN
               l_field := 'wip_plan_qty';
               RAISE error_no_null;
            ELSE
               x_material_detail_rec.wip_plan_qty :=
                                           p_material_detail_rec.wip_plan_qty;
               validate_wip_plan_qty
                       (p_wip_plan_qty       => x_material_detail_rec.wip_plan_qty
                       ,x_return_status      => x_return_status);

               IF x_return_status <> fnd_api.g_ret_sts_success THEN
                  l_val_proc := 'validate_wip_plan_qty';
                  RAISE val_error;
               END IF;
            END IF;
         ELSE
            gme_common_pvt.log_message ('GME_INV_STAT_UPD_WIP_PLAN');
            RAISE expected_error;
         END IF;
      END IF;
     
      --Bug#5078853 modified validation for actual qty
      IF p_material_detail_rec.actual_qty IS NOT NULL THEN
        IF p_batch_header_rec.batch_status IN (gme_common_pvt.g_batch_wip,
                                               gme_common_pvt.g_batch_completed )THEN
            IF p_material_detail_rec.actual_qty = fnd_api.g_miss_num THEN
              l_field := 'actual_qty';
              RAISE error_no_null;
            ELSE
              x_material_detail_rec.actual_qty := p_material_detail_rec.actual_qty;
              validate_actual_qty
                             (p_actual_qty         => x_material_detail_rec.actual_qty
                             ,x_return_status      => x_return_status);

              IF x_return_status <> fnd_api.g_ret_sts_success THEN
               l_val_proc := 'validate_actual_qty';
               RAISE val_error;
             END IF;
           END IF;
        ELSE
         gme_common_pvt.log_message ('GME_INV_STAT_UPD_ACT');
         RAISE expected_error;
        END IF; /*status check */
      END IF;

      --Bug#5078853 allow scrap factor to be changed in pending
      -- don't allow NULL
      IF p_batch_header_rec.batch_status = gme_common_pvt.g_batch_pending AND
         p_material_detail_rec.line_type = gme_common_pvt.g_line_type_ing THEN

         IF p_material_detail_rec.scrap_factor IS NOT NULL THEN
           IF p_material_detail_rec.scrap_factor = fnd_api.g_miss_num THEN
             l_field := 'scrap_factor';
             RAISE error_no_null;
           ELSE     
             validate_scrap_factor
                               (p_scrap              => p_material_detail_rec.scrap_factor
                               ,x_return_status      => x_return_status);

             IF x_return_status <> fnd_api.g_ret_sts_success THEN
              l_val_proc := 'validate_scrap_factor';
              RAISE val_error;
             END IF;
            -- scrap is a percent
             x_material_detail_rec.scrap_factor := p_material_detail_rec.scrap_factor / 100;
             /* nsinghi Bug4911461 Re-work. Modify Plan qty to include scrap factor. */
             IF p_material_detail_rec.plan_qty IS NOT NULL THEN
              x_material_detail_rec.plan_qty := p_material_detail_rec.plan_qty + 
                     (x_material_detail_rec.scrap_factor * p_material_detail_rec.plan_qty);
             ELSE 
              x_material_detail_rec.plan_qty := p_stored_material_detail_rec.plan_qty + 
                     (x_material_detail_rec.scrap_factor * p_stored_material_detail_rec.plan_qty);
             END IF;

           END IF; /* miss_num */
         END IF; /*p_material_detail_rec.scrap_factor IS NOT NULL*/
      END IF;

      --Bug#5078853 scale type can be changed in both pending and WIP
      IF p_material_detail_rec.scale_type IS NOT NULL THEN
           IF p_batch_header_rec.batch_status IN ( gme_common_pvt.g_batch_pending,
                                                   gme_common_pvt.g_batch_wip )  THEN         
            -- scale_type can be changed, but not to NULL
            IF p_material_detail_rec.scale_type = fnd_api.g_miss_num  THEN
              l_field := 'scale_type';
              RAISE error_no_null;
            ELSE
              validate_scale_type
                          (p_scale_type               => p_material_detail_rec.scale_type
                          ,x_return_status            => x_return_status);

              IF x_return_status <> fnd_api.g_ret_sts_success THEN
                l_val_proc := 'validate_scale_type';
                RAISE val_error;
              END IF;
              x_material_detail_rec.scale_type := p_material_detail_rec.scale_type;
            END IF;

            -- Following validation belongs in form also
            IF x_material_detail_rec.scale_type = 2 THEN  -- integer scaling
              -- Scale_Multiple
              validate_scale_multiple
                        (p_scale_mult         => p_material_detail_rec.scale_multiple
                        ,x_return_status      => x_return_status);

              IF x_return_status <> fnd_api.g_ret_sts_success THEN
                l_val_proc := 'validate_scale_multiple';
                RAISE val_error;
              END IF;
              x_material_detail_rec.scale_multiple := p_material_detail_rec.scale_multiple;

              -- Scale_Rounding_Variance
              validate_scale_round_var
                (p_scale_var          => p_material_detail_rec.scale_rounding_variance
                ,x_return_status      => x_return_status);

              IF x_return_status <> fnd_api.g_ret_sts_success THEN
                 l_val_proc := 'validate_scale_round_var';
                 RAISE val_error;
              END IF;

              x_material_detail_rec.scale_rounding_variance :=
              p_material_detail_rec.scale_rounding_variance / 100;

              -- Rounding_Direction
              validate_rounding_direction
                     (p_round_dir          => p_material_detail_rec.rounding_direction
                     ,x_return_status      => x_return_status);

              IF x_return_status <> fnd_api.g_ret_sts_success THEN
                l_val_proc := 'validate_rounding_direction';
                RAISE val_error;
              END IF;
              x_material_detail_rec.rounding_direction := p_material_detail_rec.rounding_direction;
            ELSE
              x_material_detail_rec.scale_multiple               := NULL;
              x_material_detail_rec.scale_rounding_variance      := NULL;
              x_material_detail_rec.rounding_direction           := NULL;
            END IF; /* x_material_detail_rec.scale_type = 2 */
          ELSE
           gme_common_pvt.log_message ('GME_INV_STAT_UPD_SCALE_TYPE');
           RAISE expected_error;
          END IF; /* status check */
     END IF; /* p_material_detail_rec.scale_type IS NOT NULL */ 

      -- can change cost alloc for product; ignore other line types
      IF x_material_detail_rec.line_type = gme_common_pvt.g_line_type_prod THEN
         -- validate 0 <= cost_alloc <= 1
         -- at save_batch, check that sum(cost_alloc for all products) <= 1
         validate_cost_alloc
                           (p_material_detail_rec  => p_material_detail_rec
                           ,x_return_status      => x_return_status);

         IF x_return_status <> fnd_api.g_ret_sts_success THEN
            l_val_proc := 'validate_cost_alloc';
            RAISE val_error;
         END IF;
         x_material_detail_rec.cost_alloc := p_material_detail_rec.cost_alloc;
      END IF;

      --Bug#5078853 modified
      -- can change phantom type for ingredient only if not exploded;
      -- error otherwise
      IF p_material_detail_rec.phantom_type IS NOT NULL 
         AND x_material_detail_rec.line_type = gme_common_pvt.g_line_type_ing 
         AND x_material_detail_rec.phantom_id IS NULL 
         AND p_material_detail_rec.phantom_type <> x_material_detail_rec.phantom_type THEN

         IF p_batch_header_rec.batch_status = gme_common_pvt.g_batch_pending THEN
              x_material_detail_rec.phantom_type := p_material_detail_rec.phantom_type;

              validate_phantom_type
                       (p_phantom_type       => x_material_detail_rec.phantom_type
                       ,x_return_status      => x_return_status);

             IF x_return_status <> fnd_api.g_ret_sts_success THEN
               l_val_proc := 'validate_phantom_type';
               RAISE val_error;
             END IF;
             --Bug#5078853 changed to x_material_detail_rec
             validate_phantom_type_change
                        (p_material_detail_rec    => x_material_detail_rec
                        ,x_return_status          => x_return_status);
             IF x_return_status <> fnd_api.g_ret_sts_success THEN
               l_val_proc := 'validate_phantom_type_change';
               RAISE val_error;
             END IF;
        ELSE
          gme_common_pvt.log_message ('GME_INV_STAT_UPD_PHAN_TYPE');
          RAISE expected_error;
        END IF; /* batch status check */
      END IF;

      -- can't update to NULL; only update for ingred, ignore other line type
      IF x_material_detail_rec.line_type = gme_common_pvt.g_line_type_ing THEN
         validate_contr_yield_ind
            (p_contr_yield_ind      => x_material_detail_rec.contribute_yield_ind
            ,x_return_status        => x_return_status);

         IF x_return_status <> fnd_api.g_ret_sts_success THEN
            l_val_proc := 'validate_contr_yield_ind';
            RAISE val_error;
         END IF;
      END IF;

      -- can't update to NULL
      validate_contr_step_qty_ind
         (p_contr_step_qty_ind      => x_material_detail_rec.contribute_step_qty_ind
         ,x_return_status           => x_return_status);

      IF x_return_status <> fnd_api.g_ret_sts_success THEN
         l_val_proc := 'validate_contr_step_qty_ind';
         RAISE val_error;
      END IF;

      IF p_material_detail_rec.subinventory = fnd_api.g_miss_char THEN
         x_material_detail_rec.subinventory := NULL;
         x_material_detail_rec.locator_id := NULL;
      ELSIF p_material_detail_rec.subinventory IS NOT NULL THEN
         x_material_detail_rec.subinventory :=
                                           p_material_detail_rec.subinventory;
         validate_subinventory
                          (p_subinv             => x_material_detail_rec.subinventory
                          ,p_item_rec           => l_item_rec
                          ,x_return_status      => x_return_status);

         IF x_return_status <> fnd_api.g_ret_sts_success THEN
            l_val_proc := 'validate_subinventory';
            RAISE val_error;
         END IF;

         IF p_material_detail_rec.locator_id = fnd_api.g_miss_num THEN
            x_material_detail_rec.locator_id := NULL;
         ELSE
            x_material_detail_rec.locator_id :=
                                             p_material_detail_rec.locator_id;
            validate_locator
                          (p_subinv             => x_material_detail_rec.subinventory
                          ,p_locator_id         => x_material_detail_rec.locator_id
                          ,p_item_rec           => l_item_rec
                          ,p_line_type          => x_material_detail_rec.line_type
                          ,x_return_status      => x_return_status);

            IF x_return_status <> fnd_api.g_ret_sts_success THEN
               l_val_proc := 'validate_locator';
               RAISE val_error;
            END IF;
         END IF;
      ELSE       -- subinv is NULL not changing it, but maybe changing locator
         IF p_material_detail_rec.locator_id = fnd_api.g_miss_num THEN
            x_material_detail_rec.locator_id := NULL;
         ELSE
            IF p_material_detail_rec.locator_id IS NOT NULL THEN
               x_material_detail_rec.locator_id :=
                                             p_material_detail_rec.locator_id;
            END IF;

            -- if locator is null, then x_material_detail_rec.locator
            -- is the value stored in the db, validate that with subinv
            -- as long as they have values
            IF     x_material_detail_rec.subinventory IS NOT NULL
               AND x_material_detail_rec.locator_id IS NOT NULL THEN
               validate_locator
                          (p_subinv             => x_material_detail_rec.subinventory
                          ,p_locator_id         => x_material_detail_rec.locator_id
                          ,p_item_rec           => l_item_rec
                          ,p_line_type          => x_material_detail_rec.line_type
                          ,x_return_status      => x_return_status);

               IF x_return_status <> fnd_api.g_ret_sts_success THEN
                  l_val_proc := 'validate_locator';
                  RAISE val_error;
               END IF;
            END IF;
         END IF;
      END IF;

      /* Bug#5078853 added the following call for flex field validation
         gme_common_pvt.g_flex_validate_prof has to be set in public API to enforce flex field validation 
         l_material_detail_rec will have flex field values passed from public API , if any*/

      l_material_detail_rec.material_detail_id := x_material_detail_rec.material_detail_id;
      gme_validate_flex_fld_pvt.validate_flex_material_details
                      ( p_material_detail_rec   => l_material_detail_rec                                  
                       ,x_material_detail_rec   => x_material_detail_rec
                       ,x_return_status         => x_return_status);

      IF x_return_status <> fnd_api.g_ret_sts_success THEN
         l_val_proc := 'validate_flex_material_detail';
         RAISE val_error;
      END IF;
      
      IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
         gme_debug.put_line('Exiting api '||g_pkg_name||'.'||l_api_name);
      END IF;

   EXCEPTION
   WHEN error_no_null THEN
      gme_common_pvt.log_message ('GME_INVALID_VALUE_SPECIFIED'
                                 ,'FIELD_NAME'
                                 ,l_field);
      x_return_status := fnd_api.g_ret_sts_error;
   WHEN error_no_upd OR expected_error THEN
      x_return_status := fnd_api.g_ret_sts_error;
   WHEN val_error THEN
      IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
        gme_debug.put_line (g_pkg_name||'.'||l_api_name||': validation error from proc: '|| l_val_proc);
      END IF;
   WHEN OTHERS THEN
      fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);

      IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line (   'Unexpected error: '
                                || g_pkg_name
                                || '.'
                                || l_api_name
                                || ': '
                                || SQLERRM);
      END IF;

      x_return_status := fnd_api.g_ret_sts_unexp_error;
   END val_and_pop_material_for_upd;
   
   --Bug#5078853 Procedure Created
   PROCEDURE validate_material_for_del (
      p_batch_header_rec             IN       gme_batch_header%ROWTYPE
     ,p_material_detail_rec          IN       gme_material_details%ROWTYPE     
     ,p_batch_step_rec               IN       gme_batch_steps%ROWTYPE
     ,x_return_status                OUT NOCOPY     VARCHAR2) IS
     
     l_api_name    CONSTANT VARCHAR2 (30)          := 'validate_material_for_del';

     CURSOR c_get_delete(v_org_id NUMBER) IS
      SELECT delete_material_ind
        FROM gme_parameters
       WHERE organization_id = v_org_id;
     
     CURSOR num_detail_lines (v_batch_id NUMBER, v_line_type NUMBER) IS
      SELECT COUNT (*)
        FROM gme_material_details
       WHERE batch_id = v_batch_id AND
             line_type = v_line_type;

     CURSOR c_prim_prod(v_rule_id NUMBER, v_det_id NUMBER) IS
      SELECT 1 
       FROM gmd_recipe_validity_rules
      WHERE recipe_validity_rule_id = v_rule_id
        AND inventory_item_id = (SELECT inventory_item_id
                                   FROM gme_material_details
                                  WHERE material_detail_id = v_det_id);

     -- Bug 18779329 - do not allow deletions if transactions exist.
     CURSOR Cur_get_trans_count(v_batch_id NUMBER,v_material_detail_id NUMBER) IS
      SELECT count(1)
      FROM mtl_material_transactions t, gme_material_details d
      WHERE t.transaction_source_type_id = 5
      AND d.batch_id = v_batch_id
      AND d.material_detail_id = v_material_detail_id
      AND transaction_source_id = d.batch_id
      AND d.material_detail_id = t.trx_source_line_id;
     
     l_trans_count NUMBER;

     /*CURSOR cur_parent_phant (v_batch_id NUMBER, v_item_id NUMBER) IS
      SELECT 1
       FROM  sys.DUAL
      WHERE  EXISTS ( SELECT 1
                       FROM  gme_batch_header h, gmd_recipe_validity_rules r
                      WHERE  h.batch_id = v_batch_id 
                         AND h.recipe_validity_rule_id = r.recipe_validity_rule_id
                         AND r.item_id = v_item_id 
                         AND h.parentline_id > 0); */
     
     l_delete_ind          NUMBER;
     l_dummy               NUMBER;
     l_material_count      NUMBER := 0;

     val_error             EXCEPTION;   
   BEGIN
      IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
        gme_debug.put_line('Entering api '||g_pkg_name||'.'||l_api_name);
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_material_detail_rec.batch_id='||p_material_detail_rec.batch_id);
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_material_detail_rec.material_detail_id='||p_material_detail_rec.material_detail_id);
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_batch_step_rec.batch_step_id='||p_batch_step_rec.batchstep_id);
      END IF;

      /* Set return status to success initially */
      x_return_status := fnd_api.g_ret_sts_success;

      /* validate batch for material deletion */ 
      IF p_batch_header_rec.batch_type <> 0 THEN
        gme_common_pvt.log_message('GME_INV_BATCH_TYPE_OPER');
        RAISE val_error;
      END IF;

      --Fetch allow material deletion profile
      OPEN c_get_delete(p_batch_header_rec.organization_id);
      FETCH c_get_delete INTO l_delete_ind;
      CLOSE c_get_delete;

      l_delete_ind := NVL(l_delete_ind, 1);   

      IF NOT( p_batch_header_rec.batch_status = gme_common_pvt.g_batch_pending OR
              (l_delete_ind = 2 AND 
               p_batch_header_rec.batch_status = gme_common_pvt.g_batch_wip AND
               p_batch_header_rec. automatic_step_calculation = 0) 
            ) THEN
        gme_common_pvt.log_message ('GME_INV_BATCH_STATUS_OPER');
        RAISE val_error;
       END IF;

       /* check step status if material is assocaited to step */ 
       IF p_batch_step_rec.batchstep_id IS NOT NULL THEN
         IF (p_batch_step_rec.step_status NOT IN (gme_common_pvt.g_step_pending
                                                 ,gme_common_pvt.g_step_wip)) THEN
           gme_common_pvt.log_message('PC_STEP_STATUS_ERR');
           RAISE val_error;
         END IF;
       END IF; /* p_batch_step_rec.batchstep_id IS NOT NULL */

       --line can't be deleted if line is ing and has an exploded phantom 
       IF p_material_detail_rec.line_type = gme_common_pvt.g_line_type_ing AND
          p_material_detail_rec.phantom_id IS NOT NULL THEN
          gme_common_pvt.log_message('GME_NO_DEL_PHANT_ING');
          RAISE val_error;     
       END IF; /* exploded phantom check */

       /* if there is only one ingredient or product we should not the delete */
       IF p_material_detail_rec.line_type IN (gme_common_pvt.g_line_type_ing,
                                              gme_common_pvt.g_line_type_prod) THEN
          OPEN num_detail_lines(p_batch_header_rec.batch_id,p_material_detail_rec.line_type);
          FETCH num_detail_lines INTO l_material_count;
          CLOSE num_detail_lines;
          IF l_material_count = 1 THEN
            gme_common_pvt.log_message('GME_ONE_ING_PROD_REQD');
            RAISE val_error;
          END IF;
        END IF; /* number of lines check */

       -- Bug 18779329 - do not allow deletions if transactions exist.
       OPEN Cur_get_trans_count(p_batch_header_rec.batch_id, p_material_detail_rec.material_detail_id);
       FETCH Cur_get_trans_count INTO l_trans_count;
       CLOSE  Cur_get_trans_count;
         
       IF l_trans_count > 0 THEN
          gme_common_pvt.log_message('PM_INVALID_LINE');
          RAISE val_error;
       END IF;

       -- Bug 10062802 - Add formulaline_id condition to validation.
       IF p_material_detail_rec.line_type = gme_common_pvt.g_line_type_prod AND
          p_material_detail_rec.formulaline_id IS NOT NULL THEN 
         --if product is primary product then do not allow deletion
         OPEN c_prim_prod(p_batch_header_rec.recipe_validity_rule_id, p_material_detail_rec.material_detail_id);
         FETCH c_prim_prod INTO l_dummy;
         IF c_prim_prod%FOUND THEN
           CLOSE c_prim_prod;
           gme_common_pvt.log_message('GME_PRIM_PROD_NO_DEL');
           RAISE val_error;
         END IF;
         CLOSE c_prim_prod;

         /*OPEN cur_parent_phant(p_batch_header_rec.batch_id, p_material_detail_rec.inventory_item_id);
         FETCH cur_parent_phant INTO l_dummy;
         IF cur_parent_phant%FOUND THEN
          CLOSE cur_parent_phant;
          gme_common_pvt.log_message('GME_NO_DEL_PHANT_PROD');
          RAISE val_error;
         END IF;
         CLOSE cur_parent_phant; */
        END IF; /* line type check */
         
        IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
         gme_debug.put_line('Exiting api '||g_pkg_name||'.'||l_api_name);
        END IF;  
   EXCEPTION
   WHEN val_error THEN
     x_return_status := fnd_api.g_ret_sts_error;
   WHEN OTHERS THEN
     fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
     IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
       gme_debug.put_line ('Unexpected error: '
                           || g_pkg_name
                           || '.'
                           || l_api_name
                           || ': '
                           || SQLERRM);
      END IF;
      x_return_status := fnd_api.g_ret_sts_unexp_error;
   END validate_material_for_del;

   PROCEDURE validate_phantom_type_change (
      p_material_detail_rec    IN gme_material_details%ROWTYPE
     ,x_return_status          OUT NOCOPY VARCHAR2) IS

      val_error              EXCEPTION;

      l_api_name    CONSTANT VARCHAR2 (30)          := 'validate_phantom_type_change';
   BEGIN
      IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
        gme_debug.put_line('Entering api '||g_pkg_name||'.'||l_api_name);
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_material_detail_rec.batch_id='||p_material_detail_rec.batch_id);
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_material_detail_rec.material_detail_id='||p_material_detail_rec.material_detail_id);
      END IF;

      /* Set return status to success initially */
      x_return_status := fnd_api.g_ret_sts_success;

      IF gme_reservations_pvt.pending_reservations_exist
                             (p_organization_id         => p_material_detail_rec.organization_id
                             ,p_batch_id                => p_material_detail_rec.batch_id
                             ,p_material_detail_id      => p_material_detail_rec.material_detail_id) THEN
        gme_common_pvt.log_message ('GME_PENDING_RSRV_EXIST');
        RAISE val_error;
      END IF;

      IF gme_move_orders_pvt.pending_move_orders_exist
                             (p_organization_id         => p_material_detail_rec.organization_id
                             ,p_batch_id                => p_material_detail_rec.batch_id
                             ,p_material_detail_id      => p_material_detail_rec.material_detail_id) THEN
        gme_common_pvt.log_message ('GME_PENDING_MO_EXIST');
        RAISE val_error;
      END IF;

      IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
         gme_debug.put_line('Exiting api '||g_pkg_name||'.'||l_api_name);
      END IF;

   EXCEPTION
      WHEN val_error THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN OTHERS THEN
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);

         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line (   'Unexpected error: '
                                || g_pkg_name
                                || '.'
                                || l_api_name
                                || ': '
                                || SQLERRM);
         END IF;
         x_return_status := fnd_api.g_ret_sts_unexp_error;
   END validate_phantom_type_change;

   PROCEDURE delete_material_line (
      p_batch_header_rec      IN       gme_batch_header%ROWTYPE
     ,p_material_detail_rec   IN       gme_material_details%ROWTYPE
     ,p_batch_step_rec        IN       gme_batch_steps%ROWTYPE
     ,x_transacted            OUT NOCOPY     VARCHAR2
     ,x_return_status         OUT NOCOPY     VARCHAR2)
   IS

      l_message_count        NUMBER;
      l_message_list         VARCHAR2 (2000);
      l_batch_step_rec       gme_batch_steps%ROWTYPE;
      l_rsc_count            NUMBER;
      l_proc                 VARCHAR2(100);

      l_mmt_tbl              gme_common_pvt.mtl_mat_tran_tbl;

      error_processing       EXCEPTION;
      error_dbl              EXCEPTION;

      l_api_name    CONSTANT VARCHAR2 (30)          := 'delete_material_line';
   BEGIN
      IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
        gme_debug.put_line('Entering api '||g_pkg_name||'.'||l_api_name);
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_batch_header_rec.batch_id='||p_batch_header_rec.batch_id);
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_material_detail_rec.material_detail_id='||p_material_detail_rec.material_detail_id);
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_batch_step_rec.batchstep_id='||p_batch_step_rec.batchstep_id);
      END IF;

      /* Set return status to success initially */
      x_return_status := fnd_api.g_ret_sts_success;

      --Bug#5078853 deleting pending lots if any
      gme_pending_product_lots_pvt.delete_pending_product_lot
            (p_material_detail_id     => p_material_detail_rec.material_detail_id
            ,x_return_status          => x_return_status);

      IF x_return_status <> fnd_api.g_ret_sts_success THEN
           l_proc := 'gme_pending_product_lots.delete_pending_product_lot';
           RAISE error_processing;
      END IF;

      gme_move_orders_pvt.delete_move_order_lines
           (p_organization_id         => p_batch_header_rec.organization_id
           ,p_batch_id                => p_batch_header_rec.batch_id
           ,p_material_detail_id      => p_material_detail_rec.material_detail_id
           ,p_invis_move_line_id      => p_material_detail_rec.move_order_line_id
           ,x_return_status           => x_return_status);

      IF x_return_status <> fnd_api.g_ret_sts_success THEN
           l_proc := 'gme_move_orders_pvt.delete_move_order_lines';
           RAISE error_processing;
      END IF;

      gme_reservations_pvt.delete_material_reservations (
          p_organization_id      => p_batch_header_rec.organization_id
         ,p_batch_id             => p_batch_header_rec.batch_id
         ,p_material_detail_id   => p_material_detail_rec.material_detail_id
         ,x_return_status        => x_return_status);

      -- delete all transactions for this line
      gme_transactions_pvt.get_mat_trans (
          p_mat_det_id      => p_material_detail_rec.material_detail_id
         ,p_batch_id        => p_batch_header_rec.batch_id
         ,x_mmt_tbl         => l_mmt_tbl
         ,x_return_status   => x_return_status);

      IF x_return_status <> fnd_api.g_ret_sts_success THEN
           l_proc := 'gme_transactions_pvt.get_mat_trans';
           RAISE error_processing;
      END IF;

      FOR i in 1..l_mmt_tbl.COUNT LOOP
          gme_transactions_pvt.delete_material_txn (
              p_transaction_id   => l_mmt_tbl(i).transaction_id
             ,p_txns_pair        => NULL
             ,x_return_status    => x_return_status);
          IF x_return_status <> fnd_api.g_ret_sts_success THEN
            l_proc := 'gme_transactions_pvt.delete_material_txn';
            RAISE error_processing;
          END IF;
      END LOOP;

      IF l_mmt_tbl.COUNT > 0 THEN
        x_transacted := fnd_api.G_TRUE;
      ELSE
        x_transacted := fnd_api.G_FALSE;
      END IF;

      -- 4944024 BEGIN    
      IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
          gme_debug.put_line(g_pkg_name||'.'||l_api_name||' line_type is ' ||p_material_detail_rec.line_type );
      END IF;
      -- Delete any reservations against this supply source   
      -- ==================================================
      IF NVL(p_material_detail_rec.line_type,0) <> -1 THEN
        IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
          gme_debug.put_line(g_pkg_name||'.'||l_api_name||' Invoking delete_prod_supply_resv' );
        END IF;
        gme_supply_res_pvt.delete_prod_supply_resv (
            p_matl_dtl_rec         => p_material_detail_rec
           ,x_msg_count            => l_message_count
           ,x_msg_data             => l_message_list                           
           ,x_return_status        => x_return_status);                    

        IF x_return_status <> FND_API.G_RET_STS_SUCCESS THEN
          l_proc := 'gme_reservations_pvt.delete_prod_prod_supply_resv ';
          RAISE error_processing;
        END IF;
      END IF;
      -- 4944024 END   

      IF NOT gme_material_details_dbl.delete_row (p_material_detail_rec) THEN
         l_proc := 'gme_material_details_dbl.delete_row';
         RAISE error_dbl;
      END IF;

      -- renumber subsequent lines
      UPDATE gme_material_details
         SET line_no = line_no - 1
            ,last_updated_by = gme_common_pvt.g_user_ident
            ,last_update_date = gme_common_pvt.g_timestamp
            ,last_update_login = gme_common_pvt.g_login_id
       WHERE batch_id = p_material_detail_rec.batch_id
         AND line_type = p_material_detail_rec.line_type
         AND line_no >= p_material_detail_rec.line_no;

      IF p_batch_step_rec.batchstep_id IS NOT NULL THEN
         DELETE FROM gme_batch_step_items
               WHERE material_detail_id =
                                     p_material_detail_rec.material_detail_id
                 AND batchstep_id = p_batch_step_rec.batchstep_id;

         gme_trans_engine_util.load_rsrc_trans
                                           (p_batch_row          => p_batch_header_rec
                                           ,x_rsc_row_count      => l_rsc_count
                                           ,x_return_status      => x_return_status);

         IF x_return_status <> fnd_api.g_ret_sts_success THEN
           l_proc := 'gme_trans_engine_util.load_rsrc_trans';
           RAISE error_processing;
         END IF;

         gme_update_step_qty_pvt.update_step_qty
                                        (p_batch_step_rec      => p_batch_step_rec
                                        ,x_message_count       => l_message_count
                                        ,x_message_list        => l_message_list
                                        ,x_return_status       => x_return_status
                                        ,x_batch_step_rec      => l_batch_step_rec);

         IF x_return_status <> fnd_api.g_ret_sts_success THEN
           l_proc := 'gme_update_step_qty_pvt.update_step_qty';
           RAISE error_processing;
         END IF;

         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line
                (   g_pkg_name
                 || '.'
                 || l_api_name
                 || ': '
                 || ' after gme_update_step_qty_pvt.update_step_qty: successful');
         END IF;
      END IF;

      IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
         gme_debug.put_line('Exiting api '||g_pkg_name||'.'||l_api_name);
      END IF;

   EXCEPTION
      WHEN error_processing THEN
         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
           gme_debug.put_line (g_pkg_name||'.'||l_api_name|| ': ' || l_proc|| ' error returned');
         END IF;
      WHEN error_dbl THEN
         --commented by qzeng, error message has been logged at lower level
	 --gme_common_pvt.log_message ('GME_UNEXPECTED_ERROR', 'ERROR', SQLERRM);
         x_return_status := FND_API.g_ret_sts_unexp_error;

         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
           gme_debug.put_line (g_pkg_name||'.'||l_api_name|| ': '
                                                          || l_proc|| ' unexpected error: '|| SQLERRM);
         END IF;
      WHEN OTHERS THEN
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);

         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line (   'Unexpected error: '
                                || g_pkg_name
                                || '.'
                                || l_api_name
                                || ': '
                                || SQLERRM);
         END IF;

         x_return_status := fnd_api.g_ret_sts_unexp_error;
   END delete_material_line;
END gme_material_detail_pvt;
/

COMMIT;
EXIT;
--show errors
