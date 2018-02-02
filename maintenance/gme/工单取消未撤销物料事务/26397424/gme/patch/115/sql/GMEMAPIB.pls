/* +======================================================================+ */
/* |    Copyright (c) 2005, 2017 Oracle and/or its affiliates.           | */
/* |                         All rights reserved.                         | */
/* |                           Version 12.0.0                             | */
/* +======================================================================+ */
REM dbdrv: sql ~PROD ~PATH ~FILE none none none package &phase=plb \
REM dbdrv: checkfile(120.33.12010000.32=120.40.12020000.22)(120.30.12000000.17=120.33.12010000.13)(120.30.12000000.16=120.33.12010000.12)(120.30.12000000.14=120.33.12010000.10)(120.30.12000000.8=120.33.12010000.4)(115.28=120.6):~PROD:~PATH:~FILE
SET VERIFY OFF
WHENEVER SQLERROR EXIT FAILURE ROLLBACK;
WHENEVER OSERROR EXIT FAILURE ROLLBACK;
/*
REM **********************************************************************
REM *                                                                    *
REM * FILE:    GMEMAPIB.pls                                              *
REM * PURPOSE: Package Body for the GME MAIN API          routines       *
REM * AUTHOR:  Chandrashekar Tiruvidula, OPM Development                 *
REM * DATE:    August 2nd 2002                                           *
REM * HISTORY:                                                           *
REM * ========
REM*  05-MAY-2006 SivakumarG Bug#5186328                                 *
REM *  New parameter p_ignore_exception added to  release_batch/step and *
REM *  complete_batch/step.                                              *
REM * 08-JUN-2006 SivakumarG Bug#5277982                                 *
REM *  New messages will be shown if IB results in any material exception*
REM * 20-JUN-2006 NamitS Bug#5176319                                     *
REM *  Reset transaction header table before calling unrelease_batch and *
REM *  unrelease_step APIs.    
REM * 14-AUG- 2006 Pawan Kumar bug 5358705                               *
REM *  Added code in scale batch to handle return status of W and C      *
REM * Archana Mundhe Bug 5763818 Modified the code to use ERES constants * 
REM * that are added to gme_common_pvt instead of using the hardcoded    *
REM * ERES event names such as 'oracle.apps.gme...'                      *
REM *  11-JAN-2007 Swapna K Bug#6738476 Added parameter,p_batch_header_rec*
REM *   to the procedure,create_phantom                                  *
REM *  28-Jul-2008 Archana Mundhe Bug 7261728                            *
REM *   Modified procedure create_line_reservations so that message -    *
REM *   INV_INVALID_RESERVATION_QTY from inv_reservation_pub will be     *
REM *    displayed instead of message GME_LINE_HL_RESERVATION_FAIL.      *
REM *
REM *  18-NOV-2008   G. Muratore   Bug 7565054 
REM *     Added parameter p_sum_all_prod_lines to the procedure create_batch

REM *  05-AUG-2009   G. Muratore   Bug 8639523 
REM *     Clear the qty cache just in case any transactions hit the tree.
REM *     A blank error message appeared and/or inventory was driven negative upon
REM *     clicking ok a second time. This was due to the qty tree not being accurate.
REM *     PROCEDURE: incremental_backflush
REM *
REM *  19-MAR-2010   G. Muratore   Bug 8751983 
REM *     Set IB specific global timestamp to potentially be used for negative IB.
REM *     PROCEDURE: incremental_backflush
REM *
REM *  27-OCT-2011   Archana Mundhe Bug 13070352                                                                                   
REM *  Modified procedure delete_material_line. Added code to bypass GMF and 
REM *  ERES call if deleting a line that has been inserted in the same session.        
REM *  
REM *  08-NOV-2011   Archana Mundhe Bug 13070352
REM *  Modified procedure insert_material_line, moved the GMF call after ERES 
REM *  and modified the exception to return a global bvariable instead of ERROR. 
REM *  Modified procedure update_material_line, added missing GMF_VIB call. 
REM *
REM *  03-APR-2012   G. Muratore    Bug 13881792 
REM *     Keep the user entered transaction date when exceptions appear 
REM *     so it can be used later on the form.
REM *     PROCEDURE: incremental_backflush
REM * 
REM *  07-MAY-2012   Archana Mundhe Bug 13743650 
REM *     Modified code in gme_api_main.update_material_line. Added code to Clear 
REM *     the quantity cache after the ROLLBACK TO SAVEPOINT update_material_line1 is issued. 
REM *
REM *  06-JUN-2012   Archana Mundhe Bug 14634379 
REM *     Added code to support New auto detail parameter in create_batch and release_batch.
REM *
REM *  29-SEP-2014   G. Muratore    Bug 19364328 
REM *     Create batch logic should continue even if a shortage is detected.
REM *     PROCEDURE: create_batch

REM *  16-OCT-2014   G. Muratore    Bug 19781489 
REM *     Stop cancel batch from proceeding if there are actuals or stuck transactions.
REM *     Stop close batch from proceeding if there are stuck transactions.
REM *     PROCEDURE: close_batch, cancel_batch.

REM *  03-JUN-2015   G. Muratore    Bug 19811389
REM *     Need to delete any reservations against PROD supply for lines associated to the step.
REM *     This behavior now matches automatic release lines when closing a batch.
REM *     PROCEDURE close_step

REM *  15-JUN-2015   G. Muratore    Bug 21187113
REM *     Call picking after auto detail if explicit move order parameter is activated.
REM *     PROCEDURE create_batch          

REM *  16-OCT-2015   G. Muratore    Bug 21070199 rework of 19781489. 
REM *     Changed tokens to have GME_ prefix.
REM *     In cancel batch, check for stuck transactions first.
REM *     PROCEDURE: close_batch, cancel_batch.

REM *  21-OCT-2015   G. Muratore    Bug 19868921
REM *     Reset partial negative IB global gme_common_pvt.g_ib_going_negative.
REM *     PROCEDURE: incremental_backflush

REM *  29-OCT-2015   Sachin Ingle/Yashwant Jain     Bug 21523465
REM *     New code added for IOT features. Calling API to calculate yield while completing a Batch. 
REM *     PROCEDURE: complete_batch

REM *  29-OCT-2015   Sachin Ingle/Yashwant Jain     Bug 21523465
REM *     New code added for IOT features. Calling API to delete data fromm gme_batch_step_quantity table if step status changed from WIP to Pending. 
REM *     PROCEDURE: unrelease_step 

REM *  05-APR-2016   G. Muratore    Bug 22317010 / 22764488
REM *     Introduced new p_create_resv_pend_lots and p_tolerance parameter.
REM *     Introduced new p_lpn_id parameter for future use. Bugs 13628717/12884831.
REM *     PROCEDURE incremental_backflush

REM *  26-JUL-2016   Shaliu Chen    Bug 24351209
REM *     modify procedure complete_step,unrelease_batch,unrelease_step
REM *     to add batch/step yield API to recalculate batch/step yield

REM *  03-NOV-2016   G. Muratore    Bug 23640627 
REM *     Introduced new p_create_resv_pend_lots parameter.
REM *     PROCEDURE: revert_batch and revert_step.
REM **********************************************************************
*/     
/*************************************************************************
* This file contains procedures for the Process Execution (GME) APIs in  *
* Oracle Process Manufacturing (OPM). Each procedure has a common set of *
* parameters to which API-specific parameters are appended.              *
*************************************************************************/

CREATE OR REPLACE PACKAGE BODY gme_api_main AS
/*  $Header: GMEMAPIB.pls 120.40.12020000.22 2017/05/22 07:46:40 shalchen ship $    */
   g_debug               VARCHAR2 (5)  := fnd_profile.VALUE ('AFLOG_LEVEL');
   g_pkg_name   CONSTANT VARCHAR2 (30) := 'gme_api_main';
   G_STATUS_WARNING     CONSTANT VARCHAR2(1)    := 'W';

/*************************************************************************/
   PROCEDURE create_batch (
      p_validation_level         IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list            IN              VARCHAR2 := fnd_api.g_false
     ,x_message_count            OUT NOCOPY      NUMBER
     ,x_message_list             OUT NOCOPY      VARCHAR2
     ,x_return_status            OUT NOCOPY      VARCHAR2
     ,p_batch_header_rec         IN              gme_batch_header%ROWTYPE
     ,x_batch_header_rec         OUT NOCOPY      gme_batch_header%ROWTYPE
     ,p_batch_size               IN              NUMBER
     ,p_batch_size_uom           IN              VARCHAR2
     ,p_creation_mode            IN              VARCHAR2
     ,p_recipe_id                IN              NUMBER := NULL
     ,p_recipe_no                IN              VARCHAR2 := NULL
     ,p_recipe_version           IN              NUMBER := NULL
     ,p_product_no               IN              VARCHAR2 := NULL
     ,p_product_id               IN              NUMBER := NULL
     ,p_sum_all_prod_lines       IN              VARCHAR2 := 'A'
     ,p_ignore_qty_below_cap     IN              VARCHAR2 := fnd_api.g_true
     ,p_use_workday_cal          IN              VARCHAR2 := fnd_api.g_true
     ,p_contiguity_override      IN              VARCHAR2 := fnd_api.g_false
     ,p_use_least_cost_validity_rule     IN      VARCHAR2 := fnd_api.g_false          
     ,x_exception_material_tbl   OUT NOCOPY      gme_common_pvt.exceptions_tab)
   IS
      l_api_name      CONSTANT VARCHAR2 (30) := 'CREATE_BATCH';
      l_return_status          VARCHAR2(10);  -- Bug 19364328

    -- Bug 21187113 - If auto detail and explicit move orders is on we need this cursor to call picking.
    --    This is the same cursor used inside of auto detail batch.
    CURSOR cur_get_reservable_ings 
    IS
      SELECT d.*
        FROM gme_material_details d, mtl_system_items_b i
       WHERE d.batch_id in (SELECT DISTINCT batch_id
                              FROM gme_material_details
                            START WITH batch_id = x_batch_header_rec.batch_id
                            CONNECT BY batch_id = PRIOR phantom_id)
         AND d.line_type = -1
         AND d.material_requirement_date < SYSDATE + NVL(gme_common_pvt.g_rule_based_resv_horizon, 100000)
         AND i.inventory_item_id = d.inventory_item_id
         AND i.organization_id = d.organization_id
         AND d.subinventory IS NOT NULL          
         AND i.reservable_type = 1
         AND d.phantom_type = 0
      ORDER BY d.batch_id, d.inventory_item_id,d.line_no;       

      l_mat_req_tbl         gme_picking_pvt.mtl_req_tab;
      l_open_qty            NUMBER := 0;
      l_loop_ctr            NUMBER := 0;
      l_conc_request_id     NUMBER;
      
      setup_failure            EXCEPTION;
      batch_creation_failure   EXCEPTION;
      invalid_batch            EXCEPTION;
   BEGIN
      SAVEPOINT create_batch;

      IF (g_debug IS NOT NULL) THEN
         gme_debug.log_initialize ('CreateBatch');
      END IF;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      IF NOT gme_common_pvt.g_setup_done THEN
         gme_common_pvt.g_setup_done :=
                    gme_common_pvt.setup (p_batch_header_rec.organization_id);

         IF NOT gme_common_pvt.g_setup_done THEN
            x_return_status := fnd_api.g_ret_sts_error;
            RAISE setup_failure;
         END IF;
      END IF;

      /* Set the return status to success initially */
      x_return_status := fnd_api.g_ret_sts_success;

      -- Initialize message list and count if needed
      IF p_init_msg_list = fnd_api.g_true THEN
         fnd_msg_pub.initialize;
         gme_common_pvt.g_error_count := 0;
      END IF;

      gme_common_pvt.set_timestamp;
      gme_create_batch_pvt.create_batch
                        (p_validation_level             => p_validation_level
                        ,p_batch_header_rec             => p_batch_header_rec
                        ,p_batch_size                   => p_batch_size
                        ,p_batch_size_uom               => p_batch_size_uom
                        ,p_creation_mode                => p_creation_mode
                        ,p_ignore_qty_below_cap         => p_ignore_qty_below_cap
                        ,p_use_workday_cal              => p_use_workday_cal
                        ,p_contiguity_override          => p_contiguity_override
                        ,p_sum_all_prod_lines           => p_sum_all_prod_lines                                                 
                        ,p_use_least_cost_validity_rule => p_use_least_cost_validity_rule
                        ,x_batch_header_rec             => x_batch_header_rec
                        ,x_exception_material_tbl       => x_exception_material_tbl
                        ,x_return_status                => x_return_status);

      IF (g_debug IS NOT NULL) THEN
         gme_debug.put_line(l_api_name||' return from create batch pvt is ' || x_return_status);
      END IF;

      -- Bug 19364328 - If there is a shortage logic flow should continue.
      -- IF x_return_status <> fnd_api.g_ret_sts_success THEN
      IF x_return_status NOT IN (fnd_api.g_ret_sts_success, gme_common_pvt.g_inv_short_err) THEN
         RAISE batch_creation_failure;
      END IF;            /* IF x_return_status <> FND_API.G_RET_STS_SUCCESS */

      IF x_message_count = 0 THEN
         gme_common_pvt.log_message ('GME_API_BATCH_CREATED');
      END IF;

      gme_common_pvt.count_and_get (x_count        => x_message_count
                                   ,p_encoded      => fnd_api.g_false
                                   ,x_data         => x_message_list);

      IF (g_debug IS NOT NULL) THEN
         gme_debug.put_line ('Completed '
                             || l_api_name
                             || ' at '
                             || TO_CHAR (SYSDATE, 'MM/DD/YYYY HH24:MI:SS') );

         gme_debug.put_line ('gme_common_pvt.g_auto_detail_batch is  '
                             || gme_common_pvt.g_auto_detail_batch);
                             
      END IF;

      -- Bug 14634379 - New auto detail parameter introduced.
      IF gme_common_pvt.g_auto_detail_batch IN (1,3) and x_batch_header_rec.batch_type = 0 THEN
         IF (g_debug IS NOT NULL) THEN
            gme_debug.put_line ('Calling auto detail batch for batch_id '||x_batch_header_rec.batch_id);
         END IF;
      
         -- Bug 19364328 - Use a different return variable in case there was a shortage.
         gme_api_main.auto_detail_batch (p_init_msg_list         => 'T',
                                         x_message_count         => x_message_count,
                                         x_message_list          => x_message_list,
                                         x_return_status         => l_return_status, -- x_return_status,
                                         p_batch_rec             => x_batch_header_rec);                                                  	                                               

         IF g_debug <= gme_debug.g_log_procedure THEN
            gme_debug.put_line ('l_return_status in create batch after auto detail is ' || l_return_status);
         END IF;

         IF (gme_common_pvt.g_create_move_orders_ind = 1) THEN

            IF (g_debug IS NOT NULL) THEN
               gme_debug.put_line ('Calling picking for batch_id '||x_batch_header_rec.batch_id);
            END IF;
         
            -- Bug 21187113 - Now we need to call picking to create move orders and have them fully allocated.
            -- Load up the table and then call picking.
            FOR get_matl_rec IN cur_get_reservable_ings LOOP

               IF (g_debug IS NOT NULL) THEN
                  gme_debug.put_line ('batch_id is '||get_matl_rec.batch_id);
                  gme_debug.put_line ('material_detail_id is '||get_matl_rec.material_detail_id);
                  gme_debug.put_line ('inventory_item_id is '||get_matl_rec.inventory_item_id);
                  gme_debug.put_line ('plan_qty is '||get_matl_rec.plan_qty);
                  gme_debug.put_line ('actual_qty is '||get_matl_rec.actual_qty);                  
                  gme_debug.put_line ('subinventory is '||get_matl_rec.subinventory);
                  gme_debug.put_line ('locator_id is '||get_matl_rec.locator_id);
                  gme_debug.put_line ('dtl_um is '||get_matl_rec.dtl_um);
                  
                  gme_debug.put_line ('material_requirement_date is '||
                     TO_CHAR(get_matl_rec.material_requirement_date, 'DD-MON-YYYY HH24:MI:SS'));

               END IF;
            
               l_open_qty := gme_picking_pvt.get_open_qty(get_matl_rec.organization_id, get_matl_rec.batch_id, 
                                get_matl_rec.material_detail_id, get_matl_rec.inventory_item_id, get_matl_rec.subinventory, 
                                get_matl_rec.plan_qty, get_matl_rec.wip_plan_qty, 
                                get_matl_rec.actual_qty, get_matl_rec.backordered_qty, get_matl_rec.dtl_um);

               IF (g_debug IS NOT NULL) THEN
                  gme_debug.put_line ('l_open_qty is '||l_open_qty);
               END IF;

               -- We do not need to call picking for ingredients that are already fully detailed.                                
               IF l_open_qty > 0 THEN
                  l_loop_ctr := l_loop_ctr + 1;
                  l_mat_req_tbl (l_loop_ctr).open_qty           := l_open_qty;                  
                  l_mat_req_tbl (l_loop_ctr).organization_id    := get_matl_rec.organization_id;
                  l_mat_req_tbl (l_loop_ctr).batch_id           := get_matl_rec.batch_id;
                  l_mat_req_tbl (l_loop_ctr).material_detail_id := get_matl_rec.material_detail_id;
                  l_mat_req_tbl (l_loop_ctr).inventory_item_id  := get_matl_rec.inventory_item_id;
                  l_mat_req_tbl (l_loop_ctr).revision           := get_matl_rec.revision;
                  l_mat_req_tbl (l_loop_ctr).subinventory       := get_matl_rec.subinventory;
                  l_mat_req_tbl (l_loop_ctr).locator_id         := get_matl_rec.locator_id;
                  l_mat_req_tbl (l_loop_ctr).dtl_um             := get_matl_rec.dtl_um;
                  l_mat_req_tbl (l_loop_ctr).mtl_req_date       := get_matl_rec.material_requirement_date;                        
               END IF;
            END LOOP;

            IF (g_debug IS NOT NULL) THEN
               gme_debug.put_line ('CALLING pick_material. number of records is '||l_loop_ctr);
            END IF;

            gme_picking_pvt.pick_material
                                (p_mtl_req_tbl          => l_mat_req_tbl
                                ,p_task_group_id        => TO_NUMBER(NVL(gme_common_pvt.g_pick_slip_grouping_rule_id, 0))
                                ,x_return_status        => l_return_status
                                ,x_conc_request_id      => l_conc_request_id);
            
            IF g_debug <= gme_debug.g_log_statement THEN
               gme_debug.put_line (   g_pkg_name
                                   || '.'
                                   || l_api_name
                                   || ':Return from pick_material is '
                                   || l_return_status);
            END IF;            
            -- If picking errors out we are disregarding.
         END IF;
      END IF;

      -- Bug 19364328 - Moved check here to maintain original logic flow.
      --                Although it does not seem necessary now.
      IF x_return_status NOT IN (fnd_api.g_ret_sts_success, gme_common_pvt.g_inv_short_err) THEN
         RAISE batch_creation_failure;
      END IF;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;
   EXCEPTION
      WHEN batch_creation_failure THEN
         IF x_return_status NOT IN (gme_common_pvt.g_inv_short_err) THEN
            ROLLBACK TO SAVEPOINT create_batch;
            x_batch_header_rec := NULL;
         END IF;

         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
      WHEN setup_failure THEN
         ROLLBACK TO SAVEPOINT create_batch;
         x_batch_header_rec := NULL;
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
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

         ROLLBACK TO SAVEPOINT create_batch;
         x_batch_header_rec := NULL;
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
         x_return_status := fnd_api.g_ret_sts_unexp_error;
   END create_batch;

/*************************************************************************/
   PROCEDURE create_phantom (
      p_validation_level         IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list            IN              VARCHAR2 := fnd_api.g_false
     ,x_message_count            OUT NOCOPY      NUMBER
     ,x_message_list             OUT NOCOPY      VARCHAR2
     ,x_return_status            OUT NOCOPY      VARCHAR2
     ,p_material_detail_rec      IN              gme_material_details%ROWTYPE
     ,p_batch_header_rec         IN              gme_batch_header%ROWTYPE --Bug#6738476
     ,p_batch_no                 IN              VARCHAR2 DEFAULT NULL
     ,x_material_detail_rec      OUT NOCOPY      gme_material_details%ROWTYPE
     ,p_validity_rule_id         IN              NUMBER
     ,p_use_workday_cal          IN              VARCHAR2 := fnd_api.g_true
     ,p_contiguity_override      IN              VARCHAR2 := fnd_api.g_true
     ,p_use_least_cost_validity_rule     IN      VARCHAR2 := fnd_api.g_false  
     ,x_exception_material_tbl   OUT NOCOPY      gme_common_pvt.exceptions_tab)
   IS
      l_api_name        CONSTANT VARCHAR2 (30)            := 'CREATE_PHANTOM';
      setup_failure              EXCEPTION;
      phantom_creation_failure   EXCEPTION;
      l_batch_header             gme_batch_header%ROWTYPE;
   BEGIN
      /* Set the save point initially */
      SAVEPOINT create_phantom;

      IF (g_debug IS NOT NULL) THEN
         gme_debug.log_initialize ('CreatePhantom');
      END IF;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      IF NOT gme_common_pvt.g_setup_done THEN
         gme_common_pvt.g_setup_done :=
                 gme_common_pvt.setup (p_material_detail_rec.organization_id);

         IF NOT gme_common_pvt.g_setup_done THEN
            x_return_status := fnd_api.g_ret_sts_error;
            RAISE setup_failure;
         END IF;
      END IF;

      -- Initialize message list and count if needed
      IF p_init_msg_list = fnd_api.g_true THEN
         fnd_msg_pub.initialize;
         gme_common_pvt.g_error_count := 0;
      END IF;

      /* Set the return status to success initially */
      x_return_status := fnd_api.g_ret_sts_success;
      gme_common_pvt.set_timestamp;
      gme_phantom_pvt.create_phantom
                        (p_material_detail_rec         => p_material_detail_rec
                        ,p_batch_header_rec             => p_batch_header_rec --Bug#6738476
                        ,p_batch_no                    => p_batch_no
                        ,x_material_detail_rec         => x_material_detail_rec
                        ,p_validity_rule_id            => p_validity_rule_id
                        ,p_use_workday_cal             => p_use_workday_cal
                        ,p_contiguity_override         => p_contiguity_override
                        ,p_use_least_cost_validity_rule => p_use_least_cost_validity_rule
                        ,x_exception_material_tbl      => x_exception_material_tbl
                        ,x_return_status               => x_return_status);

      IF x_return_status <> fnd_api.g_ret_sts_success THEN
         RAISE phantom_creation_failure;
      END IF;

      IF (g_debug <= gme_debug.g_log_procedure) THEN
         gme_debug.put_line (   'Completed '
                             || l_api_name
                             || ' at '
                             || TO_CHAR (SYSDATE, 'MM/DD/YYYY HH24:MI:SS') );
      END IF;
   EXCEPTION
      WHEN phantom_creation_failure OR setup_failure THEN
         ROLLBACK TO SAVEPOINT create_phantom;
         x_material_detail_rec := NULL;
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
      WHEN OTHERS THEN
         IF g_debug <= gme_debug.g_log_unexpected THEN
            gme_debug.put_line (   'When others exception in '
                                || g_pkg_name
                                || '.'
                                || l_api_name
                                || ' Error is '
                                || SQLERRM);
         END IF;

         ROLLBACK TO SAVEPOINT create_phantom;
         x_material_detail_rec := NULL;
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
         x_return_status := fnd_api.g_ret_sts_unexp_error;
   END create_phantom;

   PROCEDURE scale_batch (
      p_validation_level         IN              NUMBER
     ,p_init_msg_list            IN              VARCHAR2
     ,p_batch_header_rec         IN              gme_batch_header%ROWTYPE
     ,p_scale_factor             IN              NUMBER
     ,p_primaries                IN              VARCHAR2
     ,p_qty_type                 IN              NUMBER
     ,p_recalc_dates             IN              VARCHAR2
     ,p_use_workday_cal          IN              VARCHAR2
     ,p_contiguity_override      IN              VARCHAR2
     ,x_exception_material_tbl   OUT NOCOPY      gme_common_pvt.exceptions_tab
     ,x_batch_header_rec         OUT NOCOPY      gme_batch_header%ROWTYPE
     ,x_message_count            OUT NOCOPY      NUMBER
     ,x_message_list             OUT NOCOPY      VARCHAR2
     ,x_return_status            OUT NOCOPY      VARCHAR2)
   IS
      l_api_name   CONSTANT VARCHAR2 (30) := 'SCALE_BATCH';
      scale_batch_failed    EXCEPTION;
      batch_save_failed     EXCEPTION;
      batch_fetch_error     EXCEPTION;
      setup_failure         EXCEPTION;
   BEGIN
      /* Set the savepoint before proceeding */
      SAVEPOINT scale_batch;

      IF (g_debug IS NOT NULL) THEN
         gme_debug.log_initialize ('ScaleBatch');
      END IF;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      /* Setup the common constants used accross the apis */
      IF NOT gme_common_pvt.g_setup_done THEN
         gme_common_pvt.g_setup_done :=
                    gme_common_pvt.setup (p_batch_header_rec.organization_id);

         IF NOT gme_common_pvt.g_setup_done THEN
            x_return_status := fnd_api.g_ret_sts_error;
            RAISE setup_failure;
         END IF;
      END IF;


      /* Initialize message list and count if needed */
      IF p_init_msg_list = fnd_api.g_true THEN
         fnd_msg_pub.initialize;
         gme_common_pvt.g_error_count := 0;
      END IF;

      x_batch_header_rec := p_batch_header_rec;
      gme_common_pvt.set_timestamp;
      
      gme_scale_batch_pvt.scale_batch
                        (p_batch_header_rec            => p_batch_header_rec
                        ,p_scale_factor                => p_scale_factor
                        ,p_primaries                   => p_primaries
                        ,p_qty_type                    => p_qty_type
                        ,p_recalc_dates                => p_recalc_dates
                        ,p_use_workday_cal             => p_use_workday_cal
                        ,p_contiguity_override         => p_contiguity_override
                        ,x_exception_material_tbl      => x_exception_material_tbl
                        ,x_batch_header_rec            => x_batch_header_rec
                        ,x_return_status               => x_return_status);
      x_message_count := 0;
      -- pawan kumar bug 5358705 add condition for different return status  'c' and 'w'
      IF x_return_status NOT IN (fnd_api.g_ret_sts_success, 'C', 'W') THEN
         RAISE scale_batch_failed;
      END IF;
       gme_common_pvt.log_message ('GME_SCALE_SUCCESS');
      IF (g_debug <= gme_debug.g_log_procedure) THEN
         gme_debug.put_line (   'Completed '
                             || l_api_name
                             || ' at '
                             || TO_CHAR (SYSDATE, 'MM/DD/YYYY HH24:MI:SS') );
      END IF;
   EXCEPTION
      WHEN setup_failure THEN
         ROLLBACK TO SAVEPOINT scale_batch;
         x_batch_header_rec := NULL;
         x_return_status := fnd_api.g_ret_sts_error;
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
      WHEN scale_batch_failed THEN
         ROLLBACK TO SAVEPOINT scale_batch;
         x_batch_header_rec := NULL;
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
      WHEN batch_save_failed OR batch_fetch_error THEN
         ROLLBACK TO SAVEPOINT scale_batch;
         x_batch_header_rec := NULL;
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
      WHEN OTHERS THEN
         ROLLBACK TO SAVEPOINT scale_batch;
         x_batch_header_rec := NULL;
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
         x_return_status := fnd_api.g_ret_sts_unexp_error;
   END scale_batch;

/*************************************************************************/
   PROCEDURE theoretical_yield_batch (
      p_validation_level   IN              NUMBER
     ,p_init_msg_list      IN              VARCHAR2
     ,p_batch_header_rec   IN              gme_batch_header%ROWTYPE
     ,p_scale_factor       IN              NUMBER
     ,x_message_count      OUT NOCOPY      NUMBER
     ,x_message_list       OUT NOCOPY      VARCHAR2
     ,x_return_status      OUT NOCOPY      VARCHAR2)
   IS
      l_api_name        CONSTANT VARCHAR2 (30) := 'THEORETICAL_YIELD_BATCH';
      theoretical_yield_failed   EXCEPTION;
      setup_failure              EXCEPTION;
      batch_fetch_error          EXCEPTION;
      batch_save_failed          EXCEPTION;
   BEGIN
      /* Set the savepoint before proceeding */
      SAVEPOINT theoretical_yield_batch;

      IF (g_debug IS NOT NULL) THEN
         gme_debug.log_initialize ('TheoreticalYieldBatch');
      END IF;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      /* Setup the common constants used accross the apis */
      IF NOT gme_common_pvt.g_setup_done THEN
         gme_common_pvt.g_setup_done :=
            gme_common_pvt.setup
                              (p_org_id      => p_batch_header_rec.organization_id);

         IF NOT gme_common_pvt.g_setup_done THEN
            RAISE setup_failure;
         END IF;
      END IF;

      /* Initialize message list and count if needed */
      IF p_init_msg_list = fnd_api.g_true THEN
         fnd_msg_pub.initialize;
         gme_common_pvt.g_error_count := 0;
      END IF;

      gme_common_pvt.set_timestamp;
      gme_scale_batch_pvt.theoretical_yield_batch
                                    (p_batch_header_rec      => p_batch_header_rec
                                    ,p_scale_factor          => p_scale_factor
                                    ,x_return_status         => x_return_status);

      IF x_return_status <> fnd_api.g_ret_sts_success THEN
         RAISE theoretical_yield_failed;
      END IF;

      gme_common_pvt.count_and_get (x_count        => x_message_count
                                   ,p_encoded      => fnd_api.g_false
                                   ,x_data         => x_message_list);

      IF (g_debug <= gme_debug.g_log_procedure) THEN
         gme_debug.put_line (   'Completed '
                             || l_api_name
                             || ' at '
                             || TO_CHAR (SYSDATE, 'MM/DD/YYYY HH24:MI:SS') );
      END IF;
   EXCEPTION
      WHEN setup_failure THEN
         ROLLBACK TO SAVEPOINT theoretical_yield_batch;
         x_return_status := fnd_api.g_ret_sts_error;
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
      WHEN theoretical_yield_failed OR batch_save_failed THEN
         ROLLBACK TO SAVEPOINT theoretical_yield_batch;
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
      WHEN OTHERS THEN
         ROLLBACK TO SAVEPOINT theoretical_yield_batch;
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
         x_return_status := fnd_api.g_ret_sts_unexp_error;
   END theoretical_yield_batch;

/*************************************************************************/
   PROCEDURE insert_material_line (
      p_validation_level      IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list         IN              VARCHAR2 := fnd_api.g_false
     ,x_message_count         OUT NOCOPY      NUMBER
     ,x_message_list          OUT NOCOPY      VARCHAR2
     ,x_return_status         OUT NOCOPY      VARCHAR2
     ,p_batch_header_rec      IN              gme_batch_header%ROWTYPE
     ,p_material_detail_rec   IN              gme_material_details%ROWTYPE
     ,p_batch_step_id         IN              NUMBER := NULL
     ,p_trans_id              IN              NUMBER
     ,x_transacted            OUT NOCOPY      VARCHAR2
     ,x_material_detail_rec   OUT NOCOPY      gme_material_details%ROWTYPE)
   IS
      l_api_name    CONSTANT VARCHAR2 (30) := 'insert_material_line_form';
      
      l_batch_step_rec       gme_batch_steps%ROWTYPE;
      setup_failure          EXCEPTION;
      ins_mtl_line_failure   EXCEPTION;

      -- Bug 5903208
      gmf_cost_failure         EXCEPTION;
      l_message_count		   NUMBER;
      l_message_list		   VARCHAR2(2000);
   BEGIN

      /* Set the return status to success initially */
      x_return_status := fnd_api.g_ret_sts_success;

      SAVEPOINT insert_material_line1;

      IF (g_debug IS NOT NULL) THEN
         gme_debug.log_initialize ('InsertMaterialLineForm');
      END IF;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      IF NOT gme_common_pvt.g_setup_done THEN
         gme_common_pvt.g_setup_done :=
                    gme_common_pvt.setup (p_batch_header_rec.organization_id);

         IF NOT gme_common_pvt.g_setup_done THEN
            x_return_status := fnd_api.g_ret_sts_error;
            RAISE setup_failure;
         END IF;
      END IF;

      -- Initialize message list and count if needed
      IF p_init_msg_list = fnd_api.g_true THEN
         fnd_msg_pub.initialize;
         gme_common_pvt.g_error_count := 0;
      END IF;

      gme_common_pvt.set_timestamp;
      
      IF p_batch_step_id IS NOT NULL THEN
        l_batch_step_rec.batchstep_id := p_batch_step_id;
      
        IF NOT gme_batch_steps_dbl.fetch_row(l_batch_step_rec, l_batch_step_rec) THEN
           RAISE fnd_api.g_exc_error;
        END IF;
      END IF;

      insert_material_line    (p_validation_level         => p_validation_level
                              ,p_init_msg_list            => p_init_msg_list
                              ,x_message_count            => x_message_count
                              ,x_message_list             => x_message_list
                              ,x_return_status            => x_return_status
                              ,p_batch_header_rec         => p_batch_header_rec
                              ,p_material_detail_rec      => p_material_detail_rec
                              ,p_batch_step_rec           => l_batch_step_rec
                              ,p_trans_id                 => p_trans_id
                              ,x_transacted               => x_transacted
                              ,x_material_detail_rec      => x_material_detail_rec);
                              
      IF x_return_status <> fnd_api.g_ret_sts_success THEN
         RAISE ins_mtl_line_failure;
      END IF;            /* IF x_return_status <> FND_API.G_RET_STS_SUCCESS */
      
     
      -- NEW
      GME_ERES_PKG.INSERT_EVENT(P_EVENT_NAME              => gme_common_pvt.G_BATCHMTL_ADDED
                               ,P_EVENT_KEY               => x_material_detail_rec.batch_id||'-'||x_material_detail_rec.material_detail_id
                               ,P_USER_KEY_LABEL          => FND_MESSAGE.GET_STRING('GME','GME_PSIG_BATCH_MATL_LABEL')
                               ,P_USER_KEY_VALUE          => gme_common_pvt.g_organization_code ||
                                                             '-'||p_batch_header_rec.batch_no||'-'|| x_material_detail_rec.Line_no 
                                                             ||'-'||GME_ERES_PKG.GET_ITEM_NUMBER(x_material_detail_rec.organization_id,x_material_detail_rec.inventory_item_id)
                               ,P_POST_OP_API             => 'NONE'
                               ,P_PARENT_EVENT            => NULL
                               ,P_PARENT_EVENT_KEY        => NULL
                               ,P_PARENT_ERECORD_ID       => NULL
                               ,X_STATUS                  => x_return_status);
      IF x_return_status <> fnd_api.g_ret_sts_success THEN
         RAISE ins_mtl_line_failure;
      END IF;
                 
      -- Bug 13070352 -- Moved GMF call after ERES. 
      
      -- Bug 5903208 -- call to GMF 
      GMF_VIB.Update_Batch_Requirements
      ( p_api_version   =>    1.0,
        p_init_msg_list =>    FND_API.G_FALSE,
        p_batch_id      =>    p_batch_header_rec.batch_id,
        x_return_status =>    x_return_status,
        x_msg_count     =>    l_message_count,
        x_msg_data      =>    l_message_list);

      IF x_return_status <> FND_API.G_RET_STS_SUCCESS
      THEN         
         RAISE gmf_cost_failure;
      END IF;
                       
      gme_common_pvt.count_and_get (x_count        => x_message_count
                                   ,p_encoded      => fnd_api.g_false
                                   ,x_data         => x_message_list);

      IF (g_debug IS NOT NULL) THEN
         gme_debug.put_line (   'Completed '
                             || l_api_name
                             || ' at '
                             || TO_CHAR (SYSDATE, 'MM/DD/YYYY HH24:MI:SS') );
      END IF;
      IF (x_return_status IS NULL) THEN 
        x_return_status := fnd_api.g_ret_sts_success;
      END IF;
   EXCEPTION
      WHEN   gmf_cost_failure THEN
        -- Bug 5903208
        -- Bug 13070352 - Return g_gmf_vib_err instead of ERROR. 
         x_return_status := gme_common_pvt.g_gmf_vib_err ; 
         gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ':'
                                || 'gmf_cost_failure in insert'  
                                || SQLERRM);
                                
        gme_debug.put_line('gme_common_pvt.g_gmf_vib_err is:' || gme_common_pvt.g_gmf_vib_err);                                
       
      WHEN ins_mtl_line_failure THEN
         IF x_return_status NOT IN (gme_common_pvt.g_inv_short_err) THEN
            ROLLBACK TO SAVEPOINT insert_material_line1;
            x_material_detail_rec := NULL;
         END IF;

         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
      WHEN setup_failure THEN
         ROLLBACK TO SAVEPOINT insert_material_line1;
         x_material_detail_rec := NULL;
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN OTHERS THEN
         ROLLBACK TO SAVEPOINT insert_material_line1;
         x_material_detail_rec := NULL;

         IF (g_debug <= gme_debug.g_log_unexpected) THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ':'
                                || 'When others exception:' 
                                || SQLERRM);
         END IF;

         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
         x_return_status := fnd_api.g_ret_sts_unexp_error;
   END insert_material_line;

/*************************************************************************/
   PROCEDURE insert_material_line (
      p_validation_level      IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list         IN              VARCHAR2 := fnd_api.g_false
     ,x_message_count         OUT NOCOPY      NUMBER
     ,x_message_list          OUT NOCOPY      VARCHAR2
     ,x_return_status         OUT NOCOPY      VARCHAR2
     ,p_batch_header_rec      IN              gme_batch_header%ROWTYPE
     ,p_material_detail_rec   IN              gme_material_details%ROWTYPE
     ,p_batch_step_rec        IN              gme_batch_steps%ROWTYPE
     ,p_trans_id              IN              NUMBER
     ,x_transacted            OUT NOCOPY      VARCHAR2
     ,x_material_detail_rec   OUT NOCOPY      gme_material_details%ROWTYPE)
   IS
      l_api_name    CONSTANT VARCHAR2 (30) := 'insert_material_line';
      setup_failure          EXCEPTION;
      ins_mtl_line_failure   EXCEPTION;
   BEGIN

      /* Set the return status to success initially */
      x_return_status := fnd_api.g_ret_sts_success;

      SAVEPOINT insert_material_line;

      IF (g_debug IS NOT NULL) THEN
         gme_debug.log_initialize ('InsertMaterialLine');
      END IF;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      IF NOT gme_common_pvt.g_setup_done THEN
         gme_common_pvt.g_setup_done :=
                    gme_common_pvt.setup (p_batch_header_rec.organization_id);

         IF NOT gme_common_pvt.g_setup_done THEN
            x_return_status := fnd_api.g_ret_sts_error;
            RAISE setup_failure;
         END IF;
      END IF;

      -- Initialize message list and count if needed
      IF p_init_msg_list = fnd_api.g_true THEN
         fnd_msg_pub.initialize;
         gme_common_pvt.g_error_count := 0;
      END IF;

      gme_common_pvt.set_timestamp;

      gme_material_detail_pvt.insert_material_line
                              (p_batch_header_rec         => p_batch_header_rec
                              ,p_material_detail_rec      => p_material_detail_rec
                              ,p_batch_step_rec           => p_batch_step_rec
                              ,p_trans_id                 => p_trans_id
                              ,x_transacted               => x_transacted
                              ,x_material_detail_rec      => x_material_detail_rec
                              ,x_return_status            => x_return_status);
                              
      IF x_return_status <> fnd_api.g_ret_sts_success THEN
         RAISE ins_mtl_line_failure;
      END IF;            /* IF x_return_status <> FND_API.G_RET_STS_SUCCESS */

      gme_common_pvt.log_message ('GME_MTL_LINE_INSERTED');
    

      gme_common_pvt.count_and_get (x_count        => x_message_count
                                   ,p_encoded      => fnd_api.g_false
                                   ,x_data         => x_message_list);

      IF (g_debug IS NOT NULL) THEN
         gme_debug.put_line (   'Completed '
                             || l_api_name
                             || ' at '
                             || TO_CHAR (SYSDATE, 'MM/DD/YYYY HH24:MI:SS') );
      END IF;
      IF (x_return_status IS NULL) THEN 
        x_return_status := fnd_api.g_ret_sts_success;
      END IF;
   EXCEPTION
      WHEN ins_mtl_line_failure THEN
         IF x_return_status NOT IN (gme_common_pvt.g_inv_short_err) THEN
            ROLLBACK TO SAVEPOINT insert_material_line;
            x_material_detail_rec := NULL;
         END IF;

         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
      WHEN setup_failure THEN
         ROLLBACK TO SAVEPOINT insert_material_line;
         x_material_detail_rec := NULL;
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN OTHERS THEN
         ROLLBACK TO SAVEPOINT insert_material_line;
         x_material_detail_rec := NULL;

         IF (g_debug <= gme_debug.g_log_unexpected) THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ':'
                                || 'When others exception:' 
                                || SQLERRM);
         END IF;

         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
         x_return_status := fnd_api.g_ret_sts_unexp_error;
   END insert_material_line;

/*************************************************************************/
   PROCEDURE update_material_line (
      p_validation_level             IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list                IN              VARCHAR2
            := fnd_api.g_false
     ,x_message_count                OUT NOCOPY      NUMBER
     ,x_message_list                 OUT NOCOPY      VARCHAR2
     ,x_return_status                OUT NOCOPY      VARCHAR2
     ,p_batch_header_rec             IN              gme_batch_header%ROWTYPE
     ,p_material_detail_rec          IN              gme_material_details%ROWTYPE
     ,p_batch_step_id                IN              NUMBER := NULL
     ,p_scale_phantom                IN              VARCHAR2 := fnd_api.g_false
     ,p_trans_id                     IN              NUMBER
     ,x_transacted                   OUT NOCOPY      VARCHAR2
     ,x_material_detail_rec          OUT NOCOPY      gme_material_details%ROWTYPE)
   IS
      l_api_name    CONSTANT VARCHAR2 (30) := 'update_material_line_form';
      
      l_stored_material_detail_rec   gme_material_details%ROWTYPE;
      l_in_batch_step_rec            gme_batch_steps%ROWTYPE;
      l_batch_step_rec               gme_batch_steps%ROWTYPE;
      upd_mtl_line_failure   EXCEPTION;
      setup_failure          EXCEPTION;
      
      -- Bug 13070352
      gmf_cost_failure         EXCEPTION;
      l_message_count	       NUMBER;
      l_message_list	       VARCHAR2(2000);
   BEGIN

      /* Set the return status to success initially */
      x_return_status := fnd_api.g_ret_sts_success;

      SAVEPOINT update_material_line1;

      IF (g_debug IS NOT NULL) THEN
         gme_debug.log_initialize ('UpdateMaterialLineForm');
      END IF;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      IF NOT gme_common_pvt.g_setup_done THEN
         gme_common_pvt.g_setup_done :=
                    gme_common_pvt.setup (p_batch_header_rec.organization_id);

         IF NOT gme_common_pvt.g_setup_done THEN
            x_return_status := fnd_api.g_ret_sts_error;
            RAISE setup_failure;
         END IF;
      END IF;

      -- Initialize message list and count if needed
      IF p_init_msg_list = fnd_api.g_true THEN
         fnd_msg_pub.initialize;
         gme_common_pvt.g_error_count := 0;
      END IF;

      gme_common_pvt.set_timestamp;
      
      IF p_batch_step_id IS NOT NULL THEN
        l_batch_step_rec.batchstep_id := p_batch_step_id;
      
        IF NOT gme_batch_steps_dbl.fetch_row(l_batch_step_rec, l_batch_step_rec) THEN
           RAISE fnd_api.g_exc_error;
        END IF;
      END IF;
      
      l_stored_material_detail_rec.material_detail_id := p_material_detail_rec.material_detail_id;

      IF NOT gme_material_details_dbl.fetch_row
                (l_stored_material_detail_rec,l_stored_material_detail_rec) THEN
         RAISE fnd_api.g_exc_error;
      END IF;

      update_material_line
                (p_validation_level                => p_validation_level
                ,p_init_msg_list                   => p_init_msg_list
                ,x_message_count                   => x_message_count
                ,x_message_list                    => x_message_list
                ,x_return_status                   => x_return_status
                ,p_batch_header_rec                => p_batch_header_rec
                ,p_material_detail_rec             => p_material_detail_rec
                ,p_stored_material_detail_rec      => l_stored_material_detail_rec
                ,p_batch_step_rec                  => l_batch_step_rec
                ,p_scale_phantom                   => p_scale_phantom
                ,p_trans_id                        => p_trans_id
                ,x_transacted                      => x_transacted
                ,x_material_detail_rec             => x_material_detail_rec);

      IF x_return_status <> fnd_api.g_ret_sts_success THEN
         RAISE upd_mtl_line_failure;
      END IF;            /* IF x_return_status <> FND_API.G_RET_STS_SUCCESS */
      
      -- NEW
      GME_ERES_PKG.INSERT_EVENT(P_EVENT_NAME              => gme_common_pvt.G_BATCHMTL_UPDATED
                               ,P_EVENT_KEY               => p_material_detail_rec.batch_id||'-'||p_material_detail_rec.material_detail_id
                               ,P_USER_KEY_LABEL          => FND_MESSAGE.GET_STRING('GME','GME_PSIG_BATCH_MATL_LABEL')
                               ,P_USER_KEY_VALUE          => gme_common_pvt.g_organization_code ||
                                                             '-'||p_batch_header_rec.batch_no||'-'|| p_material_detail_rec.Line_no 
                                                             ||'-'||GME_ERES_PKG.GET_ITEM_NUMBER(p_material_detail_rec.organization_id,p_material_detail_rec.inventory_item_id)
                               ,P_POST_OP_API             => 'NONE'
                               ,P_PARENT_EVENT            => NULL
                               ,P_PARENT_EVENT_KEY        => NULL
                               ,P_PARENT_ERECORD_ID       => NULL
                               ,X_STATUS                  => x_return_status);

      IF x_return_status <> fnd_api.g_ret_sts_success THEN
         RAISE upd_mtl_line_failure;
      END IF;
      
      -- Bug 13070352 - Added GMF call that was missing for UPDATE material line. 
      
      -- Bug 5903208 -- call to GMF 
      GMF_VIB.Update_Batch_Requirements
      ( p_api_version   =>    1.0,
        p_init_msg_list =>    FND_API.G_FALSE,
        p_batch_id      =>    p_batch_header_rec.batch_id,
        x_return_status =>    x_return_status,
        x_msg_count     =>    l_message_count,
        x_msg_data      =>    l_message_list);

      IF x_return_status <> FND_API.G_RET_STS_SUCCESS
      THEN
         RAISE gmf_cost_failure;
      END IF;

      gme_common_pvt.count_and_get (x_count        => x_message_count
                                   ,p_encoded      => fnd_api.g_false
                                   ,x_data         => x_message_list);

      IF (g_debug IS NOT NULL) THEN
         gme_debug.put_line (   'Completed '
                             || l_api_name
                             || ' at '
                             || TO_CHAR (SYSDATE, 'MM/DD/YYYY HH24:MI:SS') );
      END IF;
      IF (x_return_status IS NULL) THEN 
        x_return_status := fnd_api.g_ret_sts_success;
      END IF;
   EXCEPTION
   
       -- Bug 13070352 - Added exception. 
       WHEN   gmf_cost_failure THEN
         gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ':'
                                || 'gmf_cost_failure update' 
                                || SQLERRM);
         gme_debug.put_line('gme_common_pvt.g_gmf_vib_err is:' || gme_common_pvt.g_gmf_vib_err);
                                        
          x_return_status := gme_common_pvt.g_gmf_vib_err; 
          
        
      WHEN upd_mtl_line_failure THEN
         IF x_return_status NOT IN (gme_common_pvt.g_inv_short_err) THEN
            ROLLBACK TO SAVEPOINT update_material_line1;
            
            -- Bug 13743650 - Clear the cache just in case any transactions hit the tree.
            inv_quantity_tree_pub.clear_quantity_cache;
            x_material_detail_rec := NULL;
         END IF;

         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
      WHEN setup_failure THEN
         ROLLBACK TO SAVEPOINT update_material_line1;
         x_material_detail_rec := NULL;
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN OTHERS THEN
         ROLLBACK TO SAVEPOINT update_material_line1;
         x_material_detail_rec := NULL;

         IF (g_debug <= gme_debug.g_log_unexpected) THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ':'
                                || 'When others exception:'
                                || SQLERRM);
         END IF;

         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
         x_return_status := fnd_api.g_ret_sts_unexp_error;
   END update_material_line;

/*************************************************************************/
   PROCEDURE update_material_line (
      p_validation_level             IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list                IN              VARCHAR2
            := fnd_api.g_false
     ,x_message_count                OUT NOCOPY      NUMBER
     ,x_message_list                 OUT NOCOPY      VARCHAR2
     ,x_return_status                OUT NOCOPY      VARCHAR2
     ,p_batch_header_rec             IN              gme_batch_header%ROWTYPE
     ,p_material_detail_rec          IN              gme_material_details%ROWTYPE
     ,p_stored_material_detail_rec   IN              gme_material_details%ROWTYPE
     ,p_batch_step_rec               IN              gme_batch_steps%ROWTYPE
     ,p_scale_phantom                IN              VARCHAR2
            := fnd_api.g_false
     ,p_trans_id                     IN              NUMBER
     ,x_transacted                   OUT NOCOPY      VARCHAR2
     ,x_material_detail_rec          OUT NOCOPY      gme_material_details%ROWTYPE)
   IS
      l_api_name    CONSTANT VARCHAR2 (30) := 'update_material_line';
      upd_mtl_line_failure   EXCEPTION;
      setup_failure          EXCEPTION;
   BEGIN
      SAVEPOINT update_material_line;

      IF (g_debug IS NOT NULL) THEN
         gme_debug.log_initialize ('UpdateMaterialLine');
      END IF;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      IF NOT gme_common_pvt.g_setup_done THEN
         gme_common_pvt.g_setup_done :=
                    gme_common_pvt.setup (p_batch_header_rec.organization_id);

         IF NOT gme_common_pvt.g_setup_done THEN
            x_return_status := fnd_api.g_ret_sts_error;
            RAISE setup_failure;
         END IF;
      END IF;

      /* Set the return status to success initially */
      x_return_status := fnd_api.g_ret_sts_success;

      -- Initialize message list and count if needed
      IF p_init_msg_list = fnd_api.g_true THEN
         fnd_msg_pub.initialize;
         gme_common_pvt.g_error_count := 0;
      END IF;

      gme_common_pvt.set_timestamp;
      gme_material_detail_pvt.update_material_line
                (p_batch_header_rec                => p_batch_header_rec
                ,p_material_detail_rec             => p_material_detail_rec
                ,p_stored_material_detail_rec      => p_stored_material_detail_rec
                ,p_batch_step_rec                  => p_batch_step_rec
                ,p_scale_phantom                   => p_scale_phantom
                ,p_trans_id                        => p_trans_id
                ,x_transacted                      => x_transacted
                ,x_return_status                   => x_return_status
                ,x_material_detail_rec             => x_material_detail_rec);

      IF x_return_status <> fnd_api.g_ret_sts_success THEN
         RAISE upd_mtl_line_failure;
      END IF;            /* IF x_return_status <> FND_API.G_RET_STS_SUCCESS */
      
      gme_common_pvt.log_message ('GME_MTL_LINE_UPDATED');
      

      gme_common_pvt.count_and_get (x_count        => x_message_count
                                   ,p_encoded      => fnd_api.g_false
                                   ,x_data         => x_message_list);

      IF (g_debug IS NOT NULL) THEN
         gme_debug.put_line (   'Completed '
                             || l_api_name
                             || ' at '
                             || TO_CHAR (SYSDATE, 'MM/DD/YYYY HH24:MI:SS') );
      END IF;
      IF (x_return_status IS NULL) THEN 
        x_return_status := fnd_api.g_ret_sts_success;
      END IF;
   EXCEPTION
      WHEN upd_mtl_line_failure THEN
         IF x_return_status NOT IN (gme_common_pvt.g_inv_short_err) THEN
            ROLLBACK TO SAVEPOINT update_material_line;
            x_material_detail_rec := NULL;
         END IF;

         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
      WHEN setup_failure THEN
         ROLLBACK TO SAVEPOINT update_material_line;
         x_material_detail_rec := NULL;
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN OTHERS THEN
         ROLLBACK TO SAVEPOINT update_material_line;
         x_material_detail_rec := NULL;

         IF (g_debug <= gme_debug.g_log_unexpected) THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ':'
                                || 'When others exception:'
                                || SQLERRM);
         END IF;

         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
         x_return_status := fnd_api.g_ret_sts_unexp_error;
   END update_material_line;

/*************************************************************************/
   PROCEDURE delete_material_line (
      p_validation_level      IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list         IN              VARCHAR2 := fnd_api.g_false
     ,x_message_count         OUT NOCOPY      NUMBER
     ,x_message_list          OUT NOCOPY      VARCHAR2
     ,x_return_status         OUT NOCOPY      VARCHAR2
     ,p_batch_header_rec      IN              gme_batch_header%ROWTYPE
     ,p_material_detail_rec   IN              gme_material_details%ROWTYPE
     ,p_batch_step_id         IN              NUMBER := NULL
     ,p_bypass_gmf            IN              VARCHAR2 := 'N'
     ,x_transacted            OUT NOCOPY      VARCHAR2)
   IS
      l_api_name    CONSTANT VARCHAR2 (30) := 'delete_material_line_form';
      
      l_batch_step_rec       gme_batch_steps%ROWTYPE;
      
      del_mtl_line_failure   EXCEPTION;
      setup_failure          EXCEPTION;

      -- Bug 5903208
      gmf_cost_failure         EXCEPTION;
      l_message_count		   NUMBER;
      l_message_list		   VARCHAR2(2000);

   BEGIN
   
      /* Set the return status to success initially */
      x_return_status := fnd_api.g_ret_sts_success;

      SAVEPOINT delete_material_line1;

      IF (g_debug IS NOT NULL) THEN
         gme_debug.log_initialize ('DeleteMaterialLineForm');
      END IF;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      IF NOT gme_common_pvt.g_setup_done THEN
         gme_common_pvt.g_setup_done :=
                    gme_common_pvt.setup (p_batch_header_rec.organization_id);

         IF NOT gme_common_pvt.g_setup_done THEN
            x_return_status := fnd_api.g_ret_sts_error;
            RAISE setup_failure;
         END IF;
      END IF;

      -- Initialize message list and count if needed
      IF p_init_msg_list = fnd_api.g_true THEN
         fnd_msg_pub.initialize;
         gme_common_pvt.g_error_count := 0;
      END IF;

      gme_common_pvt.set_timestamp;
      
      IF p_batch_step_id IS NOT NULL THEN
        l_batch_step_rec.batchstep_id := p_batch_step_id;
      
        IF NOT gme_batch_steps_dbl.fetch_row(l_batch_step_rec, l_batch_step_rec) THEN
           RAISE fnd_api.g_exc_error;
        END IF;
      END IF;

      delete_material_line (
          p_validation_level       => p_validation_level
         ,p_init_msg_list          => p_init_msg_list
         ,x_message_count          => x_message_count
         ,x_message_list           => x_message_list
         ,x_return_status          => x_return_status
         ,p_batch_header_rec       => p_batch_header_rec
         ,p_material_detail_rec    => p_material_detail_rec
         ,p_batch_step_rec         => l_batch_step_rec
         ,x_transacted             => x_transacted);

      IF x_return_status <> fnd_api.g_ret_sts_success THEN
         RAISE del_mtl_line_failure;
      END IF;            /* IF x_return_status <> FND_API.G_RET_STS_SUCCESS */

      -- Bug 13070352 
      -- Bypass GMF and ERES call if deleting a line that has been inserted in the same session. 
      IF (p_bypass_gmf = 'N') THEN
      	 --
         -- Bug 5903208 -- call to GMF
         --
            GMF_VIB.Update_Batch_Requirements
      		( p_api_version   =>    1.0,
        	  p_init_msg_list =>    FND_API.G_FALSE,
        	  p_batch_id      =>    p_batch_header_rec.batch_id,
        	  x_return_status =>    x_return_status,
        	  x_msg_count     =>    l_message_count,
        	  x_msg_data      =>    l_message_list);
       	    IF x_return_status <> FND_API.G_RET_STS_SUCCESS
      	    THEN
                  RAISE gmf_cost_failure;
            END IF;
          -- End Bug 5903208

          -- NEW
      	  GME_ERES_PKG.INSERT_EVENT(P_EVENT_NAME              => gme_common_pvt.G_BATCHMTL_REMOVED
                         ,P_EVENT_KEY               => p_material_detail_rec.batch_id||'-'||p_material_detail_rec.material_detail_id
                         ,P_USER_KEY_LABEL          => FND_MESSAGE.GET_STRING('GME','GME_PSIG_BATCH_MATL_LABEL')
                         ,P_USER_KEY_VALUE          => gme_common_pvt.g_organization_code ||
                                                       '-'||p_batch_header_rec.batch_no||'-'|| p_material_detail_rec.Line_no 
                                                       ||'-'||GME_ERES_PKG.GET_ITEM_NUMBER(p_material_detail_rec.organization_id,p_material_detail_rec.inventory_item_id)
                         ,P_POST_OP_API             => 'NONE'
                         ,P_PARENT_EVENT            => NULL
                         ,P_PARENT_EVENT_KEY        => NULL
                         ,P_PARENT_ERECORD_ID       => NULL
                         ,X_STATUS                  => x_return_status);

          IF x_return_status <> fnd_api.g_ret_sts_success THEN
      	     RAISE del_mtl_line_failure;
          END IF;
      END IF; -- IF p_bypass_gmf

       gme_common_pvt.count_and_get (x_count        => x_message_count
                                    ,p_encoded      => fnd_api.g_false
                                    ,x_data         => x_message_list);

       IF (g_debug IS NOT NULL) THEN
          gme_debug.put_line (   'Completed '
                             || l_api_name
                             || ' at '
                             || TO_CHAR (SYSDATE, 'MM/DD/YYYY HH24:MI:SS') );
       END IF;
                
       IF (x_return_status IS NULL) THEN 
           x_return_status := fnd_api.g_ret_sts_success;
       END IF;
    EXCEPTION
        WHEN   gmf_cost_failure THEN
          -- Bug 5903208
          x_return_status := FND_API.G_RET_STS_ERROR;
    
        WHEN del_mtl_line_failure THEN
           IF x_return_status NOT IN (gme_common_pvt.g_inv_short_err) THEN
              ROLLBACK TO SAVEPOINT delete_material_line1;
           END IF;

           gme_common_pvt.count_and_get (x_count        => x_message_count
                                        ,p_encoded      => fnd_api.g_false
                                        ,x_data         => x_message_list);
        WHEN setup_failure THEN
           ROLLBACK TO SAVEPOINT delete_material_line1;
           gme_common_pvt.count_and_get (x_count        => x_message_count
                                        ,p_encoded      => fnd_api.g_false
                                        ,x_data         => x_message_list);
           x_return_status := fnd_api.g_ret_sts_error;
        WHEN OTHERS THEN
           ROLLBACK TO SAVEPOINT delete_material_line1;

           IF (g_debug <= gme_debug.g_log_unexpected) THEN
              gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ':'
                                || 'When others exception:'
                                || SQLERRM);
           END IF;
 
           gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
           x_return_status := fnd_api.g_ret_sts_unexp_error;
   END delete_material_line;

/*************************************************************************/
   PROCEDURE delete_material_line (
      p_validation_level      IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list         IN              VARCHAR2 := fnd_api.g_false
     ,x_message_count         OUT NOCOPY      NUMBER
     ,x_message_list          OUT NOCOPY      VARCHAR2
     ,x_return_status         OUT NOCOPY      VARCHAR2
     ,p_batch_header_rec      IN              gme_batch_header%ROWTYPE
     ,p_material_detail_rec   IN              gme_material_details%ROWTYPE
     ,p_batch_step_rec        IN              gme_batch_steps%ROWTYPE
     ,x_transacted            OUT NOCOPY      VARCHAR2)
   IS
      l_api_name    CONSTANT VARCHAR2 (30) := 'delete_material_line';
      del_mtl_line_failure   EXCEPTION;
      setup_failure          EXCEPTION;
   BEGIN

      /* Set the return status to success initially */
      x_return_status := fnd_api.g_ret_sts_success;

      SAVEPOINT delete_material_line;

      IF (g_debug IS NOT NULL) THEN
         gme_debug.log_initialize ('DeleteMaterialLine');
      END IF;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      IF NOT gme_common_pvt.g_setup_done THEN
         gme_common_pvt.g_setup_done :=
                    gme_common_pvt.setup (p_batch_header_rec.organization_id);

         IF NOT gme_common_pvt.g_setup_done THEN
            x_return_status := fnd_api.g_ret_sts_error;
            RAISE setup_failure;
         END IF;
      END IF;

      -- Initialize message list and count if needed
      IF p_init_msg_list = fnd_api.g_true THEN
         fnd_msg_pub.initialize;
         gme_common_pvt.g_error_count := 0;
      END IF;

      gme_common_pvt.set_timestamp;
      gme_material_detail_pvt.delete_material_line
                              (p_batch_header_rec         => p_batch_header_rec
                              ,p_material_detail_rec      => p_material_detail_rec
                              ,p_batch_step_rec           => p_batch_step_rec
                              ,x_transacted               => x_transacted
                              ,x_return_status            => x_return_status);

      IF x_return_status <> fnd_api.g_ret_sts_success THEN
         RAISE del_mtl_line_failure;
      END IF;            /* IF x_return_status <> FND_API.G_RET_STS_SUCCESS */

      gme_common_pvt.log_message ('GME_MTL_LINE_DELETED');

      gme_common_pvt.count_and_get (x_count        => x_message_count
                                   ,p_encoded      => fnd_api.g_false
                                   ,x_data         => x_message_list);

      IF (g_debug IS NOT NULL) THEN
         gme_debug.put_line (   'Completed '
                             || l_api_name
                             || ' at '
                             || TO_CHAR (SYSDATE, 'MM/DD/YYYY HH24:MI:SS') );
      END IF;
      IF (x_return_status IS NULL) THEN 
        x_return_status := fnd_api.g_ret_sts_success;
      END IF;
   EXCEPTION
      WHEN del_mtl_line_failure THEN
         IF x_return_status NOT IN (gme_common_pvt.g_inv_short_err) THEN
            ROLLBACK TO SAVEPOINT delete_material_line;
         END IF;

         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
      WHEN setup_failure THEN
         ROLLBACK TO SAVEPOINT delete_material_line;
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN OTHERS THEN
         ROLLBACK TO SAVEPOINT delete_material_line;

         IF (g_debug <= gme_debug.g_log_unexpected) THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ':'
                                || 'When others exception:'
                                || SQLERRM);
         END IF;

         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
         x_return_status := fnd_api.g_ret_sts_unexp_error;
   END delete_material_line;

/*************************************************************************/
   PROCEDURE reschedule_batch (
      p_validation_level      IN              NUMBER
     ,p_init_msg_list         IN              VARCHAR2
     ,p_batch_header_rec      IN              gme_batch_header%ROWTYPE
     ,p_use_workday_cal       IN              VARCHAR2
     ,p_contiguity_override   IN              VARCHAR2
     ,x_message_count         OUT NOCOPY      NUMBER
     ,x_message_list          OUT NOCOPY      VARCHAR2
     ,x_return_status         OUT NOCOPY      VARCHAR2
     ,x_batch_header_rec      OUT NOCOPY      gme_batch_header%ROWTYPE)
   IS
      l_api_name       CONSTANT VARCHAR2 (30) := 'RESCHEDULE_BATCH';
      reschedule_batch_failed   EXCEPTION;
      setup_failure             EXCEPTION;
   BEGIN
      /* Set the savepoint before proceeding */
      SAVEPOINT reschedule_batch;

      IF (g_debug IS NOT NULL) THEN
         gme_debug.log_initialize ('RescheduleBatch');
      END IF;

      IF (NVL (g_debug, 0) IN
                       (gme_debug.g_log_statement, gme_debug.g_log_procedure) ) THEN
         gme_debug.put_line (g_pkg_name || '.' || l_api_name || ':'
                             || 'Entering');
      END IF;

      IF NOT gme_common_pvt.g_setup_done THEN
         gme_common_pvt.g_setup_done :=
                    gme_common_pvt.setup (p_batch_header_rec.organization_id);

         IF NOT gme_common_pvt.g_setup_done THEN
            x_return_status := fnd_api.g_ret_sts_error;
            RAISE setup_failure;
         END IF;
      END IF;

      /* Initialize message list and count if needed */
      IF p_init_msg_list = fnd_api.g_true THEN
         fnd_msg_pub.initialize;
         gme_common_pvt.g_error_count := 0;
      END IF;

      gme_common_pvt.set_timestamp;

      IF (NVL (g_debug, 0) = gme_debug.g_log_statement) THEN
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ':'
                             || 'Calling Pvt Reschedule Batch');
      END IF;

      gme_reschedule_batch_pvt.reschedule_batch
                              (p_batch_header_rec         => p_batch_header_rec
                              ,p_use_workday_cal          => p_use_workday_cal
                              ,p_contiguity_override      => p_contiguity_override
                              ,x_batch_header_rec         => x_batch_header_rec
                              ,x_return_status            => x_return_status);

      IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
         gme_debug.put_line
                       (   'Came back from Pvt Reschedule Batch with status '
                        || x_return_status);
      END IF;
      IF x_return_status NOT IN (fnd_api.g_ret_sts_success, 'C') THEN
         RAISE reschedule_batch_failed;
      END IF;
      
      gme_common_pvt.count_and_get (x_count        => x_message_count
                                   ,p_encoded      => fnd_api.g_false
                                   ,x_data         => x_message_list);

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ':'
                             || 'Exiting with '
                             || x_return_status
                             || ' at '
                             || TO_CHAR (SYSDATE, 'MM/DD/YYYY HH24:MI:SS') );
      END IF;
   EXCEPTION
      WHEN setup_failure THEN
         ROLLBACK TO SAVEPOINT reschedule_batch;
         x_batch_header_rec := NULL;
         x_return_status := fnd_api.g_ret_sts_error;
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
      WHEN reschedule_batch_failed THEN
         ROLLBACK TO SAVEPOINT reschedule_batch;
         x_batch_header_rec := NULL;
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
      WHEN OTHERS THEN
         ROLLBACK TO SAVEPOINT reschedule_batch;
         x_batch_header_rec := NULL;
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
         x_return_status := fnd_api.g_ret_sts_unexp_error;
   END reschedule_batch;

/*************************************************************************/
   PROCEDURE reschedule_step (
      p_validation_level        IN              NUMBER
     ,p_init_msg_list           IN              VARCHAR2
     ,p_batch_header_rec        IN              gme_batch_header%ROWTYPE
     ,p_batch_step_rec          IN              gme_batch_steps%ROWTYPE
     ,p_reschedule_preceding    IN              VARCHAR2
     ,p_reschedule_succeeding   IN              VARCHAR2
     ,p_use_workday_cal         IN              VARCHAR2
     ,p_contiguity_override     IN              VARCHAR2
     ,x_message_count           OUT NOCOPY      NUMBER
     ,x_message_list            OUT NOCOPY      VARCHAR2
     ,x_return_status           OUT NOCOPY      VARCHAR2
     ,x_batch_step_rec          OUT NOCOPY      gme_batch_steps%ROWTYPE)
   IS
      l_api_name      CONSTANT VARCHAR2 (30)             := 'RESCHEDULE_STEP';
      l_diff                   NUMBER                           := 0;
      l_diff_cmplt             NUMBER                           := 0;
      l_batch_step             gme_batch_steps%ROWTYPE;
      l_step_tbl               gme_reschedule_step_pvt.step_tab;
      setup_failure            EXCEPTION;
      reschedule_step_failed   EXCEPTION;
      expected_error           EXCEPTION;
   BEGIN
      /* Set the savepoint before proceeding */
      SAVEPOINT reschedule_batch_step;

      IF (g_debug IS NOT NULL) THEN
         gme_debug.log_initialize ('RescheduleStep');
      END IF;

      IF (NVL (g_debug, 0) IN
                       (gme_debug.g_log_statement, gme_debug.g_log_procedure) ) THEN
         gme_debug.put_line (g_pkg_name || '.' || l_api_name || ':'
                             || 'Entering');
      END IF;

      IF NOT gme_common_pvt.g_setup_done THEN
         gme_common_pvt.g_setup_done :=
                    gme_common_pvt.setup (p_batch_header_rec.organization_id);

         IF NOT gme_common_pvt.g_setup_done THEN
            x_return_status := fnd_api.g_ret_sts_error;
            RAISE setup_failure;
         END IF;
      END IF;

      /* Initialize message list and count if needed */
      IF p_init_msg_list = fnd_api.g_true THEN
         fnd_msg_pub.initialize;
         gme_common_pvt.g_error_count := 0;
      END IF;

      gme_common_pvt.set_timestamp;

      IF (NVL (g_debug, 0) = gme_debug.g_log_statement) THEN
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ':'
                             || 'Calling Pvt Reschedule Step');
      END IF;

      gme_reschedule_step_pvt.reschedule_step
                          (p_batch_step_rec             => p_batch_step_rec
                          ,p_source_step_id_tbl         => l_step_tbl
                          ,p_contiguity_override        => p_contiguity_override
                          ,p_reschedule_preceding       => p_reschedule_preceding
                          ,p_reschedule_succeeding      => p_reschedule_succeeding
                          ,p_use_workday_cal            => p_use_workday_cal
                          ,x_batch_step_rec             => x_batch_step_rec
                          ,x_return_status              => x_return_status);

      IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
         gme_debug.put_line
                        (   'Came back from Pvt Reschedule Step with status '
                         || x_return_status);
      END IF;

      IF x_return_status = fnd_api.g_ret_sts_success THEN
         -- OM-GME integration - call in private layer at the end
         -- need to retrieve batch header record here... it's already retrieved in pvt.
         NULL;
      ELSE
         RAISE reschedule_step_failed;
      END IF;

         gme_common_pvt.log_message ('GME_API_STEP_RESCH');
     

      gme_common_pvt.count_and_get (x_count        => x_message_count
                                   ,p_encoded      => fnd_api.g_false
                                   ,x_data         => x_message_list);

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ':'
                             || 'Exiting with '
                             || x_return_status
                             || ' at '
                             || TO_CHAR (SYSDATE, 'MM/DD/YYYY HH24:MI:SS') );
      END IF;
   EXCEPTION
      WHEN reschedule_step_failed OR setup_failure THEN
         ROLLBACK TO SAVEPOINT reschedule_batch_step;
         x_batch_step_rec := NULL;
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
      WHEN expected_error THEN
         ROLLBACK TO SAVEPOINT reschedule_batch_step;
         x_batch_step_rec := NULL;
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN OTHERS THEN
         ROLLBACK TO SAVEPOINT reschedule_batch_step;
         x_batch_step_rec := NULL;
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
         x_return_status := fnd_api.g_ret_sts_unexp_error;
   END reschedule_step;

/*************************************************************************/
   PROCEDURE create_batch_reservations (
      p_init_msg_list      IN              VARCHAR2 := fnd_api.g_false
     ,p_batch_header_rec   IN              gme_batch_header%ROWTYPE
     ,x_message_count      OUT NOCOPY      NUMBER
     ,x_message_list       OUT NOCOPY      VARCHAR2
     ,x_return_status      OUT NOCOPY      VARCHAR2)
   IS
      l_api_name          CONSTANT VARCHAR2 (30)
                                               := 'CREATE_BATCH_RESERVATIONS';
      setup_failure                EXCEPTION;
      batch_reservations_failure   EXCEPTION;
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      SAVEPOINT create_batch_reservations;

      IF (g_debug IS NOT NULL) THEN
         gme_debug.log_initialize ('CreateBatchReservations');
      END IF;

      IF NOT gme_common_pvt.g_setup_done THEN
         gme_common_pvt.g_setup_done :=
                    gme_common_pvt.setup (p_batch_header_rec.organization_id);

         IF NOT gme_common_pvt.g_setup_done THEN
            RAISE setup_failure;
         END IF;
      END IF;

      /* Set the return status to success initially */
      x_return_status := fnd_api.g_ret_sts_success;

      -- Initialize message list and count if needed
      IF p_init_msg_list = fnd_api.g_true THEN
         fnd_msg_pub.initialize;
         gme_common_pvt.g_error_count := 0;
      END IF;

      gme_common_pvt.set_timestamp;
      gme_reservations_pvt.create_batch_reservations
                                   (p_batch_id           => p_batch_header_rec.batch_id
                                   ,p_timefence          => 1000000
                                   ,x_return_status      => x_return_status);

      IF x_return_status <> fnd_api.g_ret_sts_success THEN
         gme_common_pvt.log_message ('GME_BATCH_HL_RESERVATION_FAIL');
         RAISE batch_reservations_failure;
      END IF;
      
      gme_common_pvt.log_message ('GME_BATCH_HI_RESR_CREATED');
      
      IF (g_debug IS NOT NULL) THEN
         gme_debug.put_line (   'Completed '
                             || l_api_name
                             || ' at '
                             || TO_CHAR (SYSDATE, 'MM/DD/YYYY HH24:MI:SS') );
      END IF;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;
   EXCEPTION
      WHEN batch_reservations_failure THEN
         ROLLBACK TO SAVEPOINT create_batch_reservations;
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
      WHEN OTHERS THEN
         IF g_debug <= gme_debug.g_log_unexpected THEN
            gme_debug.put_line (   'When others exception in '
                                || g_pkg_name
                                || '.'
                                || l_api_name
                                || ' Error is '
                                || SQLERRM);
         END IF;

         ROLLBACK TO SAVEPOINT create_batch_reservations;
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
         x_return_status := fnd_api.g_ret_sts_unexp_error;
   END create_batch_reservations;

   PROCEDURE create_line_reservations (
      p_init_msg_list   IN              VARCHAR2 := fnd_api.g_false
     ,p_matl_dtl_rec    IN              gme_material_details%ROWTYPE
     ,x_message_count   OUT NOCOPY      NUMBER
     ,x_message_list    OUT NOCOPY      VARCHAR2
     ,x_return_status   OUT NOCOPY      VARCHAR2)
   IS
      l_api_name        CONSTANT VARCHAR2 (30) := 'CREATE_LINE_RESERVATIONS';
      l_location_control_code    NUMBER;
      l_restrict_locators_code   NUMBER;
      l_open_qty                 NUMBER;
      /* Bug 5441643 Added NVL condition for location control code*/
      CURSOR cur_get_item (v_org_id NUMBER, v_inventory_item_id NUMBER)
      IS
         SELECT NVL(location_control_code,1) location_control_code, restrict_locators_code
           FROM mtl_system_items_kfv
          WHERE organization_id = v_org_id
            AND inventory_item_id = v_inventory_item_id;

      setup_failure              EXCEPTION;
      get_open_qty_failure       EXCEPTION;
      line_reservation_failure   EXCEPTION;
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      SAVEPOINT create_line_reservations;

      IF (g_debug IS NOT NULL) THEN
         gme_debug.log_initialize ('CreateLineReservations');
      END IF;

      IF NOT gme_common_pvt.g_setup_done THEN
         gme_common_pvt.g_setup_done :=
                        gme_common_pvt.setup (p_matl_dtl_rec.organization_id);

         IF NOT gme_common_pvt.g_setup_done THEN
            RAISE setup_failure;
         END IF;
      END IF;

      /* Set the return status to success initially */
      x_return_status := fnd_api.g_ret_sts_success;

      -- Initialize message list and count if needed
      IF p_init_msg_list = fnd_api.g_true THEN
         fnd_msg_pub.initialize;
         gme_common_pvt.g_error_count := 0;
      END IF;

      gme_common_pvt.set_timestamp;

      OPEN cur_get_item (p_matl_dtl_rec.organization_id
                        ,p_matl_dtl_rec.inventory_item_id);

      FETCH cur_get_item
       INTO l_location_control_code, l_restrict_locators_code;

      CLOSE cur_get_item;

      -- Use Suggestions mode (S) in the called by param to assess the total
      -- unreserved quantity
      /* Bug 5441643 Added NVL condition for location control code*/      
      gme_common_pvt.get_open_qty
                        (p_mtl_dtl_rec                 => p_matl_dtl_rec
                        ,p_called_by                   => 'S'
                        ,p_item_location_control       => NVL(l_location_control_code,1)
                        ,p_item_restrict_locators      => l_restrict_locators_code
                        ,x_open_qty                    => l_open_qty
                        ,x_return_status               => x_return_status);

      IF (g_debug <= gme_debug.g_log_statement) THEN
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ':'
                             || 'get_open_qty returns status: '
                             || x_return_status
                             || 'get_open_qty returns open_qty: '
                             || l_open_qty);
      END IF;

      IF x_return_status <> fnd_api.g_ret_sts_success THEN
         RAISE get_open_qty_failure;
      END IF;

      /* Create a high level reservation (at organization level) for the outstanding qty */
      gme_reservations_pvt.create_material_reservation
                                           (p_matl_dtl_rec       => p_matl_dtl_rec
                                           ,p_resv_qty           => l_open_qty
                                           ,x_return_status      => x_return_status);

      IF (g_debug <= gme_debug.g_log_statement) THEN
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ':'
                             || 'create_material_reservations returns status: '
                             || x_return_status);
      END IF;

      IF x_return_status <> fnd_api.g_ret_sts_success THEN
         -- Bug 7261728
         -- Commented the line below so message - INV_INVALID_RESERVATION_QTY
         -- from inv_reservation_pub will be displayedd. 
         --gme_common_pvt.log_message ('GME_LINE_HL_RESERVATION_FAIL');
         RAISE line_reservation_failure;
      END IF;
      
      gme_common_pvt.log_message ('GME_LINE_HI_RESR_CREATED');
      IF (g_debug IS NOT NULL) THEN
         gme_debug.put_line (   'Completed '
                             || l_api_name
                             || ' at '
                             || TO_CHAR (SYSDATE, 'MM/DD/YYYY HH24:MI:SS') );
      END IF;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;
   EXCEPTION
      WHEN setup_failure OR get_open_qty_failure OR line_reservation_failure THEN
         ROLLBACK TO SAVEPOINT create_line_reservations;
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
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

         ROLLBACK TO SAVEPOINT create_line_reservations;
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
         x_return_status := fnd_api.g_ret_sts_unexp_error;
   END create_line_reservations;

/*************************************************************************/
   PROCEDURE release_batch (
      p_validation_level         IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list            IN              VARCHAR2 := fnd_api.g_false
     ,x_message_count            OUT NOCOPY      NUMBER
     ,x_message_list             OUT NOCOPY      VARCHAR2
     ,x_return_status            OUT NOCOPY      VARCHAR2
     ,p_batch_header_rec         IN              gme_batch_header%ROWTYPE
     ,x_batch_header_rec         OUT NOCOPY      gme_batch_header%ROWTYPE
     ,x_exception_material_tbl   OUT NOCOPY      gme_common_pvt.exceptions_tab
     ,p_ignore_exception         IN              VARCHAR2 := NULL) --Bug#5186328
   IS
      l_api_name     CONSTANT VARCHAR2 (30) := 'RELEASE_BATCH';
      setup_failure           EXCEPTION;
      batch_release_failure   EXCEPTION;
      batch_release_exception  EXCEPTION;
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      SAVEPOINT release_batch;

      IF (g_debug IS NOT NULL) THEN
         gme_debug.log_initialize ('ReleaseBatch');
      END IF;

      IF NOT gme_common_pvt.g_setup_done THEN
         gme_common_pvt.g_setup_done :=
                    gme_common_pvt.setup (p_batch_header_rec.organization_id);

         IF NOT gme_common_pvt.g_setup_done THEN
            x_return_status := fnd_api.g_ret_sts_error;
            RAISE setup_failure;
         END IF;
      END IF;            

      /* Set the return status to success initially */
      x_return_status := fnd_api.g_ret_sts_success;

      -- Initialize message list and count if needed
      IF p_init_msg_list = fnd_api.g_true THEN
         fnd_msg_pub.initialize;
         gme_common_pvt.g_error_count := 0;
      END IF;
      
      -- Bug 14634379 - New auto detail parameter introduced.
      IF gme_common_pvt.g_auto_detail_batch IN (2,3) THEN
         IF (g_debug IS NOT NULL) THEN
            gme_debug.put_line ('Calling auto detail batch for batch_id '||x_batch_header_rec.batch_id);
         END IF;
      
         gme_api_main.auto_detail_batch (p_init_msg_list         => 'T',
                                         x_message_count         => x_message_count,
                                         x_message_list          => x_message_list,
                                         x_return_status         => x_return_status,
                                         p_batch_rec             => p_batch_header_rec); 	 
                                              	 
      END IF;

      gme_common_pvt.set_timestamp;
      gme_release_batch_pvt.release_batch
                         (p_batch_header_rec            => p_batch_header_rec
                         ,p_phantom_product_id          => NULL
                         ,x_batch_header_rec            => x_batch_header_rec
                         ,x_return_status               => x_return_status
                         ,x_exception_material_tbl      => x_exception_material_tbl);
           
      IF x_return_status NOT IN (fnd_api.g_ret_sts_success, gme_common_pvt.g_exceptions_err) THEN        
         RAISE batch_release_failure;
      END IF;            /* IF x_return_status NOT IN */
      
      /*Bug#5186328 rework if return status is X then log message saying batch has exceptions*/
      IF NVL(p_ignore_exception,fnd_api.g_false) = fnd_api.g_false AND 
         x_return_status = gme_common_pvt.g_exceptions_err THEN
         gme_common_pvt.log_message('GME_MATERIAL_EXCEPTIONS');
      ELSE      
       gme_common_pvt.log_message ('GME_API_BATCH_RELEASED');
      END IF;
   
      gme_common_pvt.count_and_get (x_count        => x_message_count
                                   ,p_encoded      => fnd_api.g_false
                                   ,x_data         => x_message_list);     

      IF (g_debug IS NOT NULL) THEN
         gme_debug.put_line (   'Released '
                             || l_api_name
                             || ' at '
                             || TO_CHAR (SYSDATE, 'MM/DD/YYYY HH24:MI:SS') );
      END IF;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;
   EXCEPTION    
      WHEN batch_release_failure THEN
         ROLLBACK TO SAVEPOINT release_batch;      
         x_batch_header_rec := NULL;
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
      WHEN setup_failure THEN
         ROLLBACK TO SAVEPOINT release_batch;
         x_batch_header_rec := NULL;
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
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

         ROLLBACK TO SAVEPOINT release_batch;
         x_batch_header_rec := NULL;
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
         x_return_status := fnd_api.g_ret_sts_unexp_error;
   END release_batch;

/*************************************************************************/
   PROCEDURE release_step (
      p_validation_level         IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list            IN              VARCHAR2 := fnd_api.g_false
     ,x_message_count            OUT NOCOPY      NUMBER
     ,x_message_list             OUT NOCOPY      VARCHAR2
     ,x_return_status            OUT NOCOPY      VARCHAR2
     ,p_batch_step_rec           IN              gme_batch_steps%ROWTYPE
     ,p_batch_header_rec         IN              gme_batch_header%ROWTYPE
     ,x_batch_step_rec           OUT NOCOPY      gme_batch_steps%ROWTYPE
     ,x_exception_material_tbl   OUT NOCOPY      gme_common_pvt.exceptions_tab
     ,p_ignore_exception         IN              VARCHAR2 := NULL) --Bug#5186328
   IS
      l_api_name    CONSTANT VARCHAR2 (30) := 'RELEASE_STEP';
      setup_failure          EXCEPTION;
      step_release_failure   EXCEPTION;
      step_release_exception EXCEPTION;
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      SAVEPOINT release_step;

      IF (g_debug IS NOT NULL) THEN
         gme_debug.log_initialize ('ReleaseStep');
      END IF;

      IF NOT gme_common_pvt.g_setup_done THEN
         gme_common_pvt.g_setup_done :=
                    gme_common_pvt.setup (p_batch_header_rec.organization_id);

         IF NOT gme_common_pvt.g_setup_done THEN
            x_return_status := fnd_api.g_ret_sts_error;
            RAISE setup_failure;
         END IF;
      END IF;

      /* Set the return status to success initially */
      x_return_status := fnd_api.g_ret_sts_success;

      -- Initialize message list and count if needed
      IF p_init_msg_list = fnd_api.g_true THEN
         fnd_msg_pub.initialize;
         gme_common_pvt.g_error_count := 0;
      END IF;

      gme_common_pvt.set_timestamp;
      gme_release_batch_step_pvt.release_step
                        (p_batch_step_rec              => p_batch_step_rec
                        ,p_batch_header_rec            => p_batch_header_rec
                        ,x_batch_step_rec              => x_batch_step_rec
                        ,x_exception_material_tbl      => x_exception_material_tbl
                        ,x_return_status               => x_return_status);
      
      IF x_return_status NOT IN (fnd_api.g_ret_sts_success, gme_common_pvt.g_exceptions_err) THEN
         RAISE step_release_failure;
      END IF;            /* IF x_return_status NOT IN */

      /*Bug#5186328 rework if return status is X then log message saying batch has exceptions*/
      IF NVL(p_ignore_exception,fnd_api.g_false) = fnd_api.g_false AND 
         x_return_status = gme_common_pvt.g_exceptions_err THEN
         gme_common_pvt.log_message('GME_MATERIAL_EXCEPTIONS');
      ELSE      
         gme_common_pvt.log_message ('GME_API_STEP_RELEASED');
      END IF;    
    

      gme_common_pvt.count_and_get (x_count        => x_message_count
                                   ,p_encoded      => fnd_api.g_false
                                   ,x_data         => x_message_list);

      IF (g_debug IS NOT NULL) THEN
         gme_debug.put_line (   'Completed '
                             || l_api_name
                             || ' at '
                             || TO_CHAR (SYSDATE, 'MM/DD/YYYY HH24:MI:SS') );
      END IF;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;
   EXCEPTION
      WHEN step_release_failure THEN
         ROLLBACK TO SAVEPOINT release_step;
         x_batch_step_rec := NULL;
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
      WHEN setup_failure THEN
         ROLLBACK TO SAVEPOINT release_step;
         x_batch_step_rec := NULL;
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
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

         ROLLBACK TO SAVEPOINT release_step;
         x_batch_step_rec := NULL;
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
         x_return_status := fnd_api.g_ret_sts_unexp_error;
   END release_step;

/*************************************************************************/
   PROCEDURE complete_batch (
      p_validation_level         IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list            IN              VARCHAR2 := fnd_api.g_false
     ,x_message_count            OUT NOCOPY      NUMBER
     ,x_message_list             OUT NOCOPY      VARCHAR2
     ,x_return_status            OUT NOCOPY      VARCHAR2
     ,p_batch_header_rec         IN              gme_batch_header%ROWTYPE
     ,x_batch_header_rec         OUT NOCOPY      gme_batch_header%ROWTYPE
     ,x_exception_material_tbl   OUT NOCOPY      gme_common_pvt.exceptions_tab
     ,p_ignore_exception         IN              VARCHAR2 := NULL) --Bug#5186328
   IS
      l_api_name      CONSTANT VARCHAR2 (30) := 'COMPLETE_BATCH';
      setup_failure            EXCEPTION;
      batch_complete_failure   EXCEPTION;
      batch_complete_exception EXCEPTION;
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      SAVEPOINT complete_batch;

      IF (g_debug IS NOT NULL) THEN
         gme_debug.log_initialize ('CompleteBatch');
      END IF;

      IF NOT gme_common_pvt.g_setup_done THEN
         gme_common_pvt.g_setup_done :=
                    gme_common_pvt.setup (p_batch_header_rec.organization_id);

         IF NOT gme_common_pvt.g_setup_done THEN
            x_return_status := fnd_api.g_ret_sts_error;
            RAISE setup_failure;
         END IF;
      END IF;

      /* Set the return status to success initially */
      x_return_status := fnd_api.g_ret_sts_success;

      -- Initialize message list and count if needed
      IF p_init_msg_list = fnd_api.g_true THEN
         fnd_msg_pub.initialize;
         gme_common_pvt.g_error_count := 0;
      END IF;

      gme_common_pvt.set_timestamp;
      gme_complete_batch_pvt.complete_batch
                        (p_batch_header_rec            => p_batch_header_rec
                        ,x_exception_material_tbl      => x_exception_material_tbl
                        ,x_batch_header_rec            => x_batch_header_rec
                        ,x_return_status               => x_return_status);

      IF x_return_status NOT IN(fnd_api.g_ret_sts_success, gme_common_pvt.g_exceptions_err) THEN
         RAISE batch_complete_failure;
      END IF;            /* IF x_return_status NOT IN */

      /*Bug#5186328 rework if return status is X then log message saying batch has exceptions*/
      IF NVL(p_ignore_exception,fnd_api.g_false) = fnd_api.g_false AND 
         x_return_status = gme_common_pvt.g_exceptions_err THEN
         gme_common_pvt.log_message('GME_MATERIAL_EXCEPTIONS');
      ELSE      
         gme_common_pvt.log_message ('GME_API_BATCH_COMPLETED');
      END IF;        
	  
	  --IOT--Bug 21523465- New code added for IOT-YMM Changes.
    --Bug 23067178 - TST1226:BATCH YIELD NOT CALCUATED WHEN BATCH IS COMPLETED BY LAST STEP COMPLE remove the following 
	 -- GME_YIELD_CALCULATION_PVT.compute_yield(p_batch_header_rec.BATCH_ID); 
      
	  gme_common_pvt.count_and_get (x_count        => x_message_count
                                   ,p_encoded      => fnd_api.g_false
                                   ,x_data         => x_message_list);

      IF (g_debug IS NOT NULL) THEN
         gme_debug.put_line (   'Completed '
                             || l_api_name
                             || ' at '
                             || TO_CHAR (SYSDATE, 'MM/DD/YYYY HH24:MI:SS') );
      END IF;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;
   EXCEPTION
      WHEN batch_complete_failure THEN
         ROLLBACK TO SAVEPOINT complete_batch;
         x_batch_header_rec := NULL;
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
      WHEN setup_failure THEN
         ROLLBACK TO SAVEPOINT complete_batch;
         x_batch_header_rec := NULL;
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
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

         ROLLBACK TO SAVEPOINT complete_batch;
         x_batch_header_rec := NULL;
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
         x_return_status := fnd_api.g_ret_sts_unexp_error;
   END complete_batch;

/*************************************************************************/
   PROCEDURE complete_step (
      p_validation_level         IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list            IN              VARCHAR2 := fnd_api.g_false
     ,x_message_count            OUT NOCOPY      NUMBER
     ,x_message_list             OUT NOCOPY      VARCHAR2
     ,x_return_status            OUT NOCOPY      VARCHAR2
     ,p_batch_step_rec           IN              gme_batch_steps%ROWTYPE
     ,p_batch_header_rec         IN              gme_batch_header%ROWTYPE
     ,x_batch_step_rec           OUT NOCOPY      gme_batch_steps%ROWTYPE
     ,x_exception_material_tbl   OUT NOCOPY      gme_common_pvt.exceptions_tab
     ,p_ignore_exception         IN              VARCHAR2 := NULL)  --Bug#5186328
   IS
      l_api_name     CONSTANT VARCHAR2 (30) := 'COMPLETE_STEP';
      setup_failure           EXCEPTION;
      step_complete_failure   EXCEPTION;
      step_complete_exception EXCEPTION;
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      SAVEPOINT complete_step;

      IF (g_debug IS NOT NULL) THEN
         gme_debug.log_initialize ('CompleteStep');
      END IF;

      IF NOT gme_common_pvt.g_setup_done THEN
         gme_common_pvt.g_setup_done :=
                    gme_common_pvt.setup (p_batch_header_rec.organization_id);

         IF NOT gme_common_pvt.g_setup_done THEN
            x_return_status := fnd_api.g_ret_sts_error;
            RAISE setup_failure;
         END IF;
      END IF;

      /* Set the return status to success initially */
      x_return_status := fnd_api.g_ret_sts_success;

      -- Initialize message list and count if needed
      IF p_init_msg_list = fnd_api.g_true THEN
         fnd_msg_pub.initialize;
         gme_common_pvt.g_error_count := 0;
      END IF;

      gme_common_pvt.set_timestamp;
      gme_complete_batch_step_pvt.complete_step
                        (p_batch_step_rec              => p_batch_step_rec
                        ,p_batch_header_rec            => p_batch_header_rec
                        ,x_batch_step_rec              => x_batch_step_rec
                        ,x_exception_material_tbl      => x_exception_material_tbl
                        ,x_return_status               => x_return_status);
      
      IF x_return_status NOT IN (fnd_api.g_ret_sts_success, gme_common_pvt.g_exceptions_err) THEN
         RAISE step_complete_failure;
      END IF;            /* IF x_return_status NOT IN */


      /*Bug#5186328 rework if return status is X then log message saying batch has exceptions*/
      IF NVL(p_ignore_exception,fnd_api.g_false) = fnd_api.g_false AND 
         x_return_status = gme_common_pvt.g_exceptions_err THEN
         gme_common_pvt.log_message('GME_MATERIAL_EXCEPTIONS');
      ELSE      
         gme_common_pvt.log_message ('GME_API_STEP_COMPLETED');
      END IF;                        

      gme_common_pvt.count_and_get (x_count        => x_message_count
                                   ,p_encoded      => fnd_api.g_false
                                   ,x_data         => x_message_list);
                                   
      --Bug 24351209 invoking calculate yield api to calculate yield for step. 
	    GME_YIELD_CALCULATION_PVT.yield_calc(p_batch_id     => p_batch_step_rec.batch_id);                                   

      IF (g_debug IS NOT NULL) THEN
         gme_debug.put_line (   'Completed '
                             || l_api_name
                             || ' at '
                             || TO_CHAR (SYSDATE, 'MM/DD/YYYY HH24:MI:SS') );
      END IF;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;
   EXCEPTION
      WHEN step_complete_failure THEN
         ROLLBACK TO SAVEPOINT complete_step;
         x_batch_step_rec := NULL;
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
      WHEN setup_failure THEN
         ROLLBACK TO SAVEPOINT complete_step;
         x_batch_step_rec := NULL;
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
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

         ROLLBACK TO SAVEPOINT complete_step;
         x_batch_step_rec := NULL;
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
         x_return_status := fnd_api.g_ret_sts_unexp_error;
   END complete_step;

/*************************************************************************/
   PROCEDURE delete_step (
      p_validation_level   IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list      IN              VARCHAR2 := fnd_api.g_false
     ,x_message_count      OUT NOCOPY      NUMBER
     ,x_message_list       OUT NOCOPY      VARCHAR2
     ,x_return_status      OUT NOCOPY      VARCHAR2
     ,p_batch_step_rec     IN              gme_batch_steps%ROWTYPE
     ,p_batch_header_rec   IN              gme_batch_header%ROWTYPE)
   IS
      l_api_name   CONSTANT VARCHAR2 (30) := 'DELETE_STEP';
      delete_step_failed    EXCEPTION;
      batch_save_failed     EXCEPTION;
      setup_failure         EXCEPTION;
   BEGIN
      /* Set the savepoint before proceeding */
      SAVEPOINT delete_step;

      /* Setup the common constants used accross the apis */
      IF (g_debug IS NOT NULL) THEN
         gme_debug.log_initialize ('DeleteStep');
      END IF;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line (g_pkg_name || '.' || l_api_name || ':'
                             || 'Entering');
      END IF;

      /* Set the return status to success initially */
      x_return_status := fnd_api.g_ret_sts_success;

      IF NOT gme_common_pvt.g_setup_done THEN
         gme_common_pvt.g_setup_done :=
                    gme_common_pvt.setup (p_batch_header_rec.organization_id);

         IF NOT gme_common_pvt.g_setup_done THEN
            x_return_status := fnd_api.g_ret_sts_error;
            RAISE setup_failure;
         END IF;
      END IF;
      /* Initialize message list and count if needed */
      IF p_init_msg_list = fnd_api.g_true THEN
         fnd_msg_pub.initialize;
         gme_common_pvt.g_error_count := 0;
      END IF;

      /* Punit Kumar */
      gme_common_pvt.set_timestamp;
      gme_delete_batch_step_pvt.delete_step
                                        (x_return_status       => x_return_status
                                        ,p_batch_step_rec      => p_batch_step_rec
                                        ,p_reroute_flag        => FALSE);

      IF x_return_status = fnd_api.g_ret_sts_success THEN
         NULL;
      ELSE
         RAISE delete_step_failed;
      END IF;
            -- NEW
            GME_ERES_PKG.INSERT_EVENT(P_EVENT_NAME        => gme_common_pvt.G_BATCHSTEP_REMOVED
                               ,P_EVENT_KEY               => p_batch_step_rec.batch_id||'-'||p_batch_step_rec.BATCHSTEP_id
                               ,P_USER_KEY_LABEL          => FND_MESSAGE.GET_STRING('GME','GME_PSIG_BATCH_STEP_LABEL')
                               ,P_USER_KEY_VALUE          => gme_common_pvt.g_organization_code ||
                                                             '-'||p_batch_header_rec.batch_no||'-'|| p_batch_step_rec.BATCHSTEP_NO
                                                             ||'-'||GME_ERES_PKG.GET_OPRN_NO(p_batch_step_rec.OPRN_ID)
                               ,P_POST_OP_API             => 'NONE'
                               ,P_PARENT_EVENT            => NULL
                               ,P_PARENT_EVENT_KEY        => NULL
                               ,P_PARENT_ERECORD_ID       => NULL
                               ,X_STATUS                  => x_return_status);
      IF x_return_status <> fnd_api.g_ret_sts_success THEN
         RAISE delete_step_failed;
      END IF;


     
         gme_common_pvt.log_message ('GME_API_STEP_DELETE');
   

      IF (g_debug <= gme_debug.g_log_procedure) THEN
         gme_debug.put_line (   'Completed '
                             || l_api_name
                             || ' at '
                             || TO_CHAR (SYSDATE, 'MM/DD/YYYY HH24:MI:SS') );
      END IF;

      gme_common_pvt.count_and_get (x_count        => x_message_count
                                   ,p_encoded      => fnd_api.g_false
                                   ,x_data         => x_message_list);
   EXCEPTION
      WHEN setup_failure THEN
         ROLLBACK TO SAVEPOINT delete_step;
         x_return_status := fnd_api.g_ret_sts_error;
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
      WHEN delete_step_failed OR batch_save_failed THEN
         ROLLBACK TO SAVEPOINT delete_step;
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
      WHEN OTHERS THEN
         ROLLBACK TO SAVEPOINT delete_step;
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
         x_return_status := fnd_api.g_ret_sts_unexp_error;
   END delete_step;

/*************************************************************************
  Procedure: insert_step

   Modification History :
   Punit Kumar 07-Apr-2005 Convergence Changes
/*************************************************************************/
   PROCEDURE insert_step (
      p_validation_level   IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list      IN              VARCHAR2 := fnd_api.g_false
     ,x_message_count      OUT NOCOPY      NUMBER
     ,x_message_list       OUT NOCOPY      VARCHAR2
     ,x_return_status      OUT NOCOPY      VARCHAR2
     ,p_batch_header_rec   IN              gme_batch_header%ROWTYPE
     ,p_batch_step_rec     IN              gme_batch_steps%ROWTYPE
     ,x_batch_step         OUT NOCOPY      gme_batch_steps%ROWTYPE)
   IS
      l_api_name   CONSTANT VARCHAR2 (30)              := 'INSERT_STEP';
      l_batch_header        gme_batch_header%ROWTYPE;
      insert_step_failed    EXCEPTION;
      batch_save_failed     EXCEPTION;
      setup_failure         EXCEPTION;

      -- Bug 5903208
      gmf_cost_failure         EXCEPTION;
      l_message_count		   NUMBER;
      l_message_list		   VARCHAR2(2000);

   BEGIN
      /* Set the savepoint before proceeding */
      SAVEPOINT insert_step;

      /* Initialize message list and count if needed */
      IF (g_debug IS NOT NULL) THEN
         gme_debug.log_initialize ('InsertStep');
      END IF;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line (g_pkg_name || '.' || l_api_name || ':'
                             || 'Entering');
      END IF;

      /* Set the return status to success initially */
      x_return_status := fnd_api.g_ret_sts_success;

      IF NOT gme_common_pvt.g_setup_done THEN
         gme_common_pvt.g_setup_done :=
                    gme_common_pvt.setup (p_batch_header_rec.organization_id);

         IF NOT gme_common_pvt.g_setup_done THEN
            x_return_status := fnd_api.g_ret_sts_error;
            RAISE setup_failure;
         END IF;
      END IF;
      IF p_init_msg_list = fnd_api.g_true THEN
         fnd_msg_pub.initialize;
         gme_common_pvt.g_error_count := 0;
      END IF;

      /* Punit Kumar */
      gme_common_pvt.set_timestamp;
      gme_insert_step_pvt.insert_batch_step
                                    (p_gme_batch_header      => p_batch_header_rec
                                    ,p_gme_batch_step        => p_batch_step_rec
                                    ,x_gme_batch_step        => x_batch_step
                                    ,x_return_status         => x_return_status);

      IF x_return_status <> fnd_api.g_ret_sts_success THEN
         RAISE insert_step_failed;
      END IF;

      --
      -- Bug 5903208 -- call to GMF
      --
      GMF_VIB.Update_Batch_Requirements
      ( p_api_version   =>    1.0,
        p_init_msg_list =>    FND_API.G_FALSE,
        p_batch_id      =>    p_batch_header_rec.batch_id,
        x_return_status =>    x_return_status,
        x_msg_count     =>    l_message_count,
        x_msg_data      =>    l_message_list);
      IF x_return_status <> FND_API.G_RET_STS_SUCCESS
      THEN
         RAISE gmf_cost_failure;
      END IF;
      -- End Bug 5903208

      -- NEW
      GME_ERES_PKG.INSERT_EVENT(P_EVENT_NAME              => gme_common_pvt.G_BATCHSTEP_ADDED
                               ,P_EVENT_KEY               => x_batch_step.batch_id||'-'||x_batch_step.BATCHSTEP_id
                               ,P_USER_KEY_LABEL          => FND_MESSAGE.GET_STRING('GME','GME_PSIG_BATCH_STEP_LABEL')
                               ,P_USER_KEY_VALUE          => gme_common_pvt.g_organization_code ||
                                                             '-'||p_batch_header_rec.batch_no||'-'|| x_batch_step.BATCHSTEP_NO
                                                             ||'-'||GME_ERES_PKG.GET_OPRN_NO(x_batch_step.OPRN_ID)
                               ,P_POST_OP_API             => 'NONE'
                               ,P_PARENT_EVENT            => NULL
                               ,P_PARENT_EVENT_KEY        => NULL
                               ,P_PARENT_ERECORD_ID       => NULL
                               ,X_STATUS                  => x_return_status);
      IF x_return_status <> fnd_api.g_ret_sts_success THEN
         RAISE insert_step_failed;
      END IF;
      gme_common_pvt.log_message ('GME_INSERT_STEP');
      IF (g_debug <= gme_debug.g_log_procedure) THEN
         gme_debug.put_line (   'Completed '
                             || l_api_name
                             || ' at '
                             || TO_CHAR (SYSDATE, 'MM/DD/YYYY HH24:MI:SS') );
      END IF;
   EXCEPTION
      WHEN   gmf_cost_failure THEN
        -- Bug 5903208
        x_return_status := FND_API.G_RET_STS_ERROR;
    
      WHEN setup_failure THEN
         ROLLBACK TO SAVEPOINT insert_step;
         x_batch_step := NULL;
         x_return_status := fnd_api.g_ret_sts_error;
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
      WHEN insert_step_failed OR batch_save_failed THEN
         ROLLBACK TO SAVEPOINT insert_step;
         x_batch_step := NULL;
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
      WHEN OTHERS THEN
         ROLLBACK TO SAVEPOINT insert_step;
         x_batch_step := NULL;
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
         x_return_status := fnd_api.g_ret_sts_unexp_error;
   END insert_step;


   PROCEDURE revert_batch (
      p_validation_level       	IN              NUMBER := gme_common_pvt.g_max_errors
     ,p_init_msg_list          	IN              VARCHAR2 := FND_API.G_FALSE 
     ,x_message_count          	OUT NOCOPY      NUMBER
     ,x_message_list           	OUT NOCOPY      VARCHAR2
     ,x_return_status          	OUT NOCOPY      VARCHAR2
     ,p_create_resv_pend_lots    IN              NUMBER DEFAULT 1    -- Bug 23640627
     ,p_batch_header_rec       	IN              gme_batch_header%ROWTYPE
     ,x_batch_header_rec      	OUT NOCOPY 	gme_batch_header%ROWTYPE)
     IS
     
      l_api_name      CONSTANT VARCHAR2 (30) := 'REVERT_BATCH';
      setup_failure            	EXCEPTION;
      batch_revert_failure	      EXCEPTION;
     	
     BEGIN
     
     SAVEPOINT revert_batch;

      IF (g_debug IS NOT NULL) THEN
         gme_debug.log_initialize ('RevertBatch');
      END IF;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line (g_pkg_name || '.' || l_api_name || ':'
                             || 'Entering');
         gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_create_resv_pend_lots is '||p_create_resv_pend_lots);
      END IF;

      /* Set the return status to success initially */
      x_return_status := fnd_api.g_ret_sts_success;

      IF NOT gme_common_pvt.g_setup_done THEN
         gme_common_pvt.g_setup_done :=
                    gme_common_pvt.setup (p_batch_header_rec.organization_id);

         IF NOT gme_common_pvt.g_setup_done THEN
            x_return_status := fnd_api.g_ret_sts_error;
            RAISE setup_failure;
         END IF;
      END IF;

      -- Initialize message list and count if needed
      IF p_init_msg_list = fnd_api.g_true THEN
         fnd_msg_pub.initialize;
         gme_common_pvt.g_error_count := 0;
      END IF;

      gme_common_pvt.set_timestamp;

      IF g_debug <= gme_debug.g_log_statement THEN
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ':'
                             || 'calling revert batch pvt');
      END IF;

      gme_revert_batch_pvt.revert_batch
                                    (p_batch_header_rec      => p_batch_header_rec,
                                     x_batch_header_rec      => x_batch_header_rec,
                                     p_create_resv_pend_lots => p_create_resv_pend_lots,  -- Bug 23640627
                                     x_return_status         => x_return_status);

      IF (g_debug <= gme_debug.g_log_procedure) THEN
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ':'
                             || 'x_return_status='
                             || x_return_status);
      END IF;

      IF x_return_status <> fnd_api.g_ret_sts_success THEN
         RAISE batch_revert_failure;
      END IF;            /* IF x_return_status <> FND_API.G_RET_STS_SUCCESS */

     
      gme_common_pvt.log_message ('GME_API_BATCH_UNCERTIFIED');
    

      gme_common_pvt.count_and_get (x_count        => x_message_count,
                                    p_encoded      => fnd_api.g_false,
                                    x_data         => x_message_list);

      IF (g_debug IS NOT NULL) THEN
         gme_debug.put_line (   'Completed '
                             || l_api_name
                             || ' at '
                             || TO_CHAR (SYSDATE, 'MM/DD/YYYY HH24:MI:SS') );
      END IF;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line (   'Exiting api '
                             || g_pkg_name
                             || '.'
                             || l_api_name
                             || x_return_status);
      END IF;
   EXCEPTION
      WHEN batch_revert_failure THEN
         ROLLBACK TO SAVEPOINT revert_batch;
         x_batch_header_rec := NULL;
         gme_common_pvt.count_and_get (x_count        => x_message_count,
                                       p_encoded      => fnd_api.g_false,
                                       x_data         => x_message_list);
      WHEN setup_failure THEN
         ROLLBACK TO SAVEPOINT revert_batch;
         x_batch_header_rec := NULL;
         gme_common_pvt.count_and_get (x_count        => x_message_count,
                                       p_encoded      => fnd_api.g_false,
                                       x_data         => x_message_list);
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

         ROLLBACK TO SAVEPOINT revert_batch;
         x_batch_header_rec := NULL;
         gme_common_pvt.count_and_get (x_count        => x_message_count,
                                       p_encoded      => fnd_api.g_false,
                                       x_data         => x_message_list);
         x_return_status := fnd_api.g_ret_sts_unexp_error ;
     
     
 END revert_batch;
 
PROCEDURE revert_step (
      p_validation_level       	IN              NUMBER := gme_common_pvt.g_max_errors
     ,p_init_msg_list          	IN              VARCHAR2 := FND_API.G_FALSE 
     ,x_message_count          	OUT NOCOPY      NUMBER
     ,x_message_list           	OUT NOCOPY      VARCHAR2
     ,x_return_status          	OUT NOCOPY      VARCHAR2
     ,p_create_resv_pend_lots    IN              NUMBER DEFAULT 1    -- Bug 23640627
     ,p_batch_step_rec        	IN              gme_batch_steps%ROWTYPE
     ,p_batch_header_rec         IN 	          gme_batch_header%ROWTYPE
     ,x_batch_step_rec        	OUT NOCOPY gme_batch_steps%ROWTYPE)IS
     	
     	
      l_api_name      CONSTANT VARCHAR2 (30) := 'REVERT_STEP';
      setup_failure            	EXCEPTION;
      step_revert_failure	EXCEPTION;
     	
     BEGIN
     
     SAVEPOINT revert_step;

      IF (g_debug IS NOT NULL) THEN
         gme_debug.log_initialize ('RevertStep');
      END IF;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line (g_pkg_name || '.' || l_api_name || ':'
                             || 'Entering');
         gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_create_resv_pend_lots is '||p_create_resv_pend_lots);
      END IF;

      /* Set the return status to success initially */
      x_return_status := fnd_api.g_ret_sts_success;

      IF NOT gme_common_pvt.g_setup_done THEN
         gme_common_pvt.g_setup_done :=
                    gme_common_pvt.setup (p_batch_header_rec.organization_id);

         IF NOT gme_common_pvt.g_setup_done THEN
            x_return_status := fnd_api.g_ret_sts_error;
            RAISE setup_failure;
         END IF;
      END IF;

      -- Initialize message list and count if needed
      IF p_init_msg_list = fnd_api.g_true THEN
         fnd_msg_pub.initialize;
         gme_common_pvt.g_error_count := 0;
      END IF;

      gme_common_pvt.set_timestamp;

      IF g_debug <= gme_debug.g_log_statement THEN
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ':'
                             || 'calling revert step pvt');
      END IF;

      gme_revert_step_pvt.revert_step
                                    (p_batch_step_rec      	   => p_batch_step_rec,
                                     p_batch_header_rec      	=> p_batch_header_rec,
                                     p_create_resv_pend_lots   => p_create_resv_pend_lots,  -- Bug 23640627
                                     x_batch_step_rec      	   => x_batch_step_rec,
                                     x_return_status         	=> x_return_status);

      IF (g_debug <= gme_debug.g_log_procedure) THEN
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ':'
                             || 'x_return_status='
                             || x_return_status);
      END IF;

      IF x_return_status <> fnd_api.g_ret_sts_success THEN
         RAISE step_revert_failure;
      END IF;            /* IF x_return_status <> FND_API.G_RET_STS_SUCCESS */
      
      gme_common_pvt.log_message ('GME_BATCH_STEP_UNCERTIFIED');     

      gme_common_pvt.count_and_get (x_count        => x_message_count,
                                    p_encoded      => fnd_api.g_false,
                                    x_data         => x_message_list);

      IF (g_debug IS NOT NULL) THEN
         gme_debug.put_line (   'Completed '
                             || l_api_name
                             || ' at '
                             || TO_CHAR (SYSDATE, 'MM/DD/YYYY HH24:MI:SS') );
      END IF;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line (   'Exiting api '
                             || g_pkg_name
                             || '.'
                             || l_api_name
                             || x_return_status);
      END IF;
   EXCEPTION
      WHEN step_revert_failure THEN
         ROLLBACK TO SAVEPOINT revert_step;
         x_batch_step_rec := NULL;
         gme_common_pvt.count_and_get (x_count        => x_message_count,
                                       p_encoded      => fnd_api.g_false,
                                       x_data         => x_message_list);
      WHEN setup_failure THEN
         ROLLBACK TO SAVEPOINT revert_step;
         x_batch_step_rec := NULL;
         gme_common_pvt.count_and_get (x_count        => x_message_count,
                                       p_encoded      => fnd_api.g_false,
                                       x_data         => x_message_list);
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

         ROLLBACK TO SAVEPOINT revert_step;
         x_batch_step_rec := NULL;
         gme_common_pvt.count_and_get (x_count        => x_message_count,
                                       p_encoded      => fnd_api.g_false,
                                       x_data         => x_message_list);
         x_return_status := fnd_api.g_ret_sts_unexp_error ;
  END revert_step;
/*************************************************************************/
   PROCEDURE close_batch (
      p_validation_level   IN              NUMBER
     ,p_init_msg_list      IN              VARCHAR2
     ,x_message_count      OUT NOCOPY      NUMBER
     ,x_message_list       OUT NOCOPY      VARCHAR2
     ,x_return_status      OUT NOCOPY      VARCHAR2
     ,p_batch_header_rec   IN              gme_batch_header%ROWTYPE
     ,x_batch_header_rec   OUT NOCOPY      gme_batch_header%ROWTYPE)
   IS
      l_api_name   CONSTANT VARCHAR2 (30) := 'CLOSE_BATCH';

      -- Bug 19781489 - this should come back with zero.         
      CURSOR cur_get_stuck_temp_count IS
      SELECT COUNT(1)
        FROM mtl_material_transactions_temp t, gme_batch_header h
       WHERE t.transaction_source_type_id = 5
         AND h.batch_id = p_batch_header_rec.batch_id
         AND t.organization_id = p_batch_header_rec.organization_id
         AND t.transaction_source_id = h.batch_id;

      -- Bug 19781489 - this should come back with zero.         
      CURSOR cur_get_stuck_inter_count IS
      SELECT COUNT(1)
        FROM mtl_transactions_interface t, gme_batch_header h
       WHERE t.transaction_source_type_id = 5
         AND h.batch_id = p_batch_header_rec.batch_id
         AND t.organization_id = p_batch_header_rec.organization_id
         AND t.transaction_source_id = h.batch_id;        
      
      l_count       NUMBER;

      stuck_inter_trans_exist  EXCEPTION;
      stuck_temp_trans_exist   EXCEPTION;
             
      setup_failure         EXCEPTION;
      batch_close_failure   EXCEPTION;
      batch_save_failed     EXCEPTION;
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      /* Set the savepoint before proceeding */
      SAVEPOINT close_batch;

      IF (g_debug IS NOT NULL) THEN
         gme_debug.log_initialize ('CloseBatch');
      END IF;

      IF NOT gme_common_pvt.g_setup_done THEN
         gme_common_pvt.g_setup_done :=
                    gme_common_pvt.setup (p_batch_header_rec.organization_id);

         IF NOT gme_common_pvt.g_setup_done THEN
            x_return_status := fnd_api.g_ret_sts_error;
            RAISE setup_failure;
         END IF;
      END IF;

      /* Initialize message list and count if needed */
      IF p_init_msg_list = fnd_api.g_true THEN
         fnd_msg_pub.initialize;
         gme_common_pvt.g_error_count := 0;
      END IF;

      gme_common_pvt.set_timestamp;

      -- Bug 19781489 - lets check to see if there are any stuck transactions or actuals.      
      IF g_debug <= gme_debug.g_log_statement THEN
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ': '
                             || 'Check for stuck trans.');
      END IF;

      -- Bug 19781489 - lets check to see if there are any stuck transactions.      
      OPEN  cur_get_stuck_temp_count;
      FETCH cur_get_stuck_temp_count INTO l_count;
      CLOSE cur_get_stuck_temp_count;
      
      IF l_count > 0 THEN
         IF g_debug <= gme_debug.g_log_statement THEN
            gme_debug.put_line ('stuck mmtt trans exist so stop.');
         END IF;      
         gme_common_pvt.log_message ('GME_STUCK_TEMP_TRANS_EXIST');     
         RAISE stuck_temp_trans_exist;
      END IF;

      OPEN  cur_get_stuck_inter_count;
      FETCH cur_get_stuck_inter_count INTO l_count;
      CLOSE cur_get_stuck_inter_count;
      
      IF l_count > 0 THEN
         IF g_debug <= gme_debug.g_log_statement THEN
            gme_debug.put_line ('stuck mti trans exist so stop.');
         END IF;      
         gme_common_pvt.log_message ('GME_STUCK_INTER_TRANS_EXIST');     
         RAISE stuck_inter_trans_exist;
      END IF;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Calling gme_close_batch_pvt.close_batch.');
      END IF;

      gme_close_batch_pvt.close_batch
                                    (p_batch_header_rec      => p_batch_header_rec
                                    ,x_batch_header_rec      => x_batch_header_rec
                                    ,x_return_status         => x_return_status);

      IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
         gme_debug.put_line (   'Came back from Pvt Close Batch with status '
                             || x_return_status);
      END IF;

      IF x_return_status = fnd_api.g_ret_sts_success THEN
         /* This comment has to be removed after this api  becomes available.
         GME_TRANS_ENGINE_PVT.inform_OM
                   ( p_action              => 'DELETE'
                   , p_trans_id            => NULL
                   , p_trans_id_reversed   => NULL
                   , p_gme_batch_hdr       => x_batch_header
                   , p_gme_matl_dtl        => NULL
                   );
         */
         NULL;
      ELSE
         RAISE batch_close_failure;
      END IF;
     
      gme_common_pvt.log_message ('GME_API_BATCH_CLOSED');     

      gme_common_pvt.count_and_get (x_count        => x_message_count
                                   ,p_encoded      => fnd_api.g_false
                                   ,x_data         => x_message_list);

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line (   'Completed '
                             || l_api_name
                             || ' at '
                             || TO_CHAR (SYSDATE, 'MM/DD/YYYY HH24:MI:SS') );
      END IF;
   EXCEPTION
      WHEN setup_failure OR batch_close_failure OR batch_save_failed THEN
         ROLLBACK TO SAVEPOINT close_batch;
         x_batch_header_rec := NULL;
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
      WHEN stuck_inter_trans_exist OR stuck_temp_trans_exist THEN
         ROLLBACK TO SAVEPOINT close_batch;
         x_batch_header_rec := NULL;
         x_return_status := fnd_api.g_ret_sts_error;         
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
      WHEN OTHERS THEN
         ROLLBACK TO SAVEPOINT close_batch;
         x_batch_header_rec := NULL;
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
         x_return_status := fnd_api.g_ret_sts_unexp_error;
   END close_batch;

/*************************************************************************/
   PROCEDURE close_step (
      p_validation_level   IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,                                                       /* Punit Kumar */
      p_init_msg_list      IN              VARCHAR2 := fnd_api.g_false
     ,x_message_count      OUT NOCOPY      NUMBER
     ,x_message_list       OUT NOCOPY      VARCHAR2
     ,x_return_status      OUT NOCOPY      VARCHAR2
     ,p_batch_header_rec   IN              gme_batch_header%ROWTYPE
     ,p_batch_step_rec     IN              gme_batch_steps%ROWTYPE
     ,p_delete_pending     IN              VARCHAR2 := fnd_api.g_false
     ,x_batch_step_rec     OUT NOCOPY      gme_batch_steps%ROWTYPE)
   IS
      l_api_name   CONSTANT VARCHAR2 (30)              := 'CLOSE_STEP';
      setup_failure         EXCEPTION;
      batch_save_failed     EXCEPTION;
      step_close_failed     EXCEPTION;

      -- Bug 19811389 - Need to delete any reservations against PROD supply for lines associated to the step.      
      CURSOR cur_material_ids
      IS
         SELECT m.material_detail_id
           FROM gme_material_details m, gme_batch_step_items i
          WHERE m.material_detail_id = i.material_detail_id
            AND m.line_type <> gme_common_pvt.g_line_type_ing
            AND i.batchstep_id = p_batch_step_rec.batchstep_id;

      l_matl_dtl_rec          gme_material_details%ROWTYPE;
      l_message_count         NUMBER;
      l_message_data          VARCHAR2(2000);
      reservation_delete_err  EXCEPTION;    
        
      l_batch_hdr           gme_batch_header%ROWTYPE;
   BEGIN
      /* Set the savepoint before proceeding */
      SAVEPOINT close_batch_step;

      IF (g_debug IS NOT NULL) THEN
         gme_debug.log_initialize ('CloseStep');
      END IF;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line (g_pkg_name || '.' || l_api_name || ':'
                             || 'Entering');
      END IF;

      /* Set the return status to success initially */
      x_return_status := fnd_api.g_ret_sts_success;

      IF NOT gme_common_pvt.g_setup_done THEN
         gme_common_pvt.g_setup_done :=
                    gme_common_pvt.setup (p_batch_header_rec.organization_id);

         IF NOT gme_common_pvt.g_setup_done THEN
            x_return_status := fnd_api.g_ret_sts_error;
            RAISE setup_failure;
         END IF;
      END IF;
      /* Initialize message list and count if needed */
      IF p_init_msg_list = fnd_api.g_true THEN
         fnd_msg_pub.initialize;
         gme_common_pvt.g_error_count := 0;                 
      END IF;

      IF (g_debug <= gme_debug.g_log_procedure) THEN
         gme_debug.put_line ('Call close_step for step id '||p_batch_step_rec.batchstep_id);
      END IF;

      gme_common_pvt.set_timestamp;                          
      
      gme_close_step_pvt.close_step (p_batch_step_rec      => p_batch_step_rec
                                    ,p_delete_pending      => p_delete_pending
                                    ,x_batch_step_rec      => x_batch_step_rec
                                    ,x_return_status       => x_return_status);

      IF x_return_status = fnd_api.g_ret_sts_success THEN
         NULL;
      ELSE
         RAISE step_close_failed;
      END IF;

      -- Bug 19811389 - Need to delete any reservations against PROD supply
      FOR get_matl_rec IN cur_material_ids LOOP            
         IF (g_debug <= gme_debug.g_log_procedure) THEN
            gme_debug.put_line ('Call delete_prod_supply_resv for material id '||get_matl_rec.material_detail_id);
         END IF;

         l_matl_dtl_rec.material_detail_id := get_matl_rec.material_detail_id;         
         gme_supply_res_pvt.delete_prod_supply_resv (
           p_matl_dtl_rec         => l_matl_dtl_rec
          ,x_msg_count            => l_message_count
          ,x_msg_data             => l_message_data                           
          ,x_return_status        => x_return_status);                    
         
         IF (g_debug <= gme_debug.g_log_procedure) THEN
            gme_debug.put_line (g_pkg_name
                              || '.'
                              || l_api_name
                              || ':'
                              || ' Return from delete_prod_supply_resv indicates '
                              || x_return_status);
         END IF;
         
         IF x_return_status <> FND_API.G_RET_STS_SUCCESS THEN
            RAISE reservation_delete_err;
         END IF;
      END LOOP;

      IF (g_debug <= gme_debug.g_log_procedure) THEN
         gme_debug.put_line (   'Completed '
                             || l_api_name
                             || ' at '
                             || TO_CHAR (SYSDATE, 'MM/DD/YYYY HH24:MI:SS') );
      END IF;

    
      gme_common_pvt.log_message ('GME_BATCH_STEP_CLOSED');
   
    
      gme_common_pvt.count_and_get (x_count        => x_message_count
                                   ,p_encoded      => fnd_api.g_false
                                   ,x_data         => x_message_list);
   EXCEPTION
      WHEN reservation_delete_err THEN
         ROLLBACK TO SAVEPOINT close_batch_step;
         x_batch_step_rec := NULL;
         IF (g_debug <= gme_debug.g_log_procedure) THEN
            gme_debug.put_line (g_pkg_name
                               || '.'
                               || l_api_name
                               || ':'
                               || ' DELETE_PROD_SUPPLY_RESV ERROR');     
         END IF;    
      WHEN setup_failure OR step_close_failed OR batch_save_failed THEN
         ROLLBACK TO SAVEPOINT close_batch_step;
         x_batch_step_rec := NULL;
         /*N Punit Kumar */
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
      WHEN OTHERS THEN
         ROLLBACK TO SAVEPOINT close_batch_step;
         x_batch_step_rec := NULL;
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
         /*N Punit Kumar */
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
         x_return_status := fnd_api.g_ret_sts_unexp_error;
   END close_step;

/*************************************************************************/
   PROCEDURE reopen_batch (
      p_validation_level   IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,                                                      
      p_init_msg_list      IN              VARCHAR2 := fnd_api.g_false
     ,x_message_count      OUT NOCOPY      NUMBER
     ,x_message_list       OUT NOCOPY      VARCHAR2
     ,x_return_status      OUT NOCOPY      VARCHAR2
     ,p_batch_header_rec   IN              gme_batch_header%ROWTYPE
     ,p_reopen_steps       IN              VARCHAR2 := fnd_api.g_false
     ,x_batch_header_rec   OUT NOCOPY      gme_batch_header%ROWTYPE)
   IS
      l_api_name    CONSTANT VARCHAR2 (30) := 'REOPEN_BATCH';
      setup_failure          EXCEPTION;
      batch_save_failed      EXCEPTION;
      batch_reopen_failure   EXCEPTION;
   BEGIN
      /* Set the save point before processing */
      SAVEPOINT reopen_batch;

      IF (g_debug IS NOT NULL) THEN
         gme_debug.log_initialize ('ReopenBatch');
      END IF;

      IF (g_debug <= gme_debug.g_log_procedure) THEN
         gme_debug.put_line (g_pkg_name || '.' || l_api_name || ':'
                             || 'Entering');
      END IF;

      /* Initialize message list and count if needed*/
      IF p_init_msg_list = fnd_api.g_true THEN
         fnd_msg_pub.initialize;
         gme_common_pvt.g_error_count := 0;                 
      END IF;

      /* Set the success staus to success inititally*/
      x_return_status := fnd_api.g_ret_sts_success;
      
      -- Pawan kumar added for bug 4956087
      IF NOT gme_common_pvt.g_setup_done THEN
         gme_common_pvt.g_setup_done :=
                    gme_common_pvt.setup (p_batch_header_rec.organization_id);

         IF NOT gme_common_pvt.g_setup_done THEN
            x_return_status := fnd_api.g_ret_sts_error;
            RAISE setup_failure;
         END IF;
      END IF;
      gme_common_pvt.set_timestamp;

      IF (g_debug <= gme_debug.g_log_procedure) THEN
        gme_debug.put_line (g_pkg_name || '.' || l_api_name || ':'
                             || 'calling private layer');
      END IF;

      gme_reopen_batch_pvt.reopen_batch
                                    (p_batch_header_rec      => p_batch_header_rec
                                    ,p_reopen_steps          => p_reopen_steps
                                    ,x_batch_header_rec      => x_batch_header_rec
                                    ,x_return_status         => x_return_status);

      IF (g_debug <= gme_debug.g_log_procedure) THEN
         gme_debug.put_line (g_pkg_name || '.' || l_api_name || ':'
                             || 'after private layer with sts'||x_return_status);
      END IF;

      IF (x_return_status = fnd_api.g_ret_sts_success) THEN
         NULL;
      ELSE
         RAISE batch_reopen_failure;
      END IF;
       
         gme_common_pvt.log_message ('GME_API_BATCH_REOPENED');
   

      IF (g_debug <= gme_debug.g_log_procedure) THEN
         gme_debug.put_line (   'Completed '
                             || l_api_name
                             || ' at '
                             || TO_CHAR (SYSDATE, 'MM/DD/YYYY HH24:MI:SS') );
      END IF;

   
      gme_common_pvt.count_and_get (x_count        => x_message_count
                                   ,p_encoded      => fnd_api.g_false
                                   ,x_data         => x_message_list);

     
   EXCEPTION
      WHEN setup_failure THEN
         ROLLBACK TO SAVEPOINT reopen_batch;
         x_batch_header_rec := NULL;

         IF (g_debug <= gme_debug.g_log_procedure) THEN
            gme_debug.put_line (g_pkg_name || '.' || l_api_name || ':'
             ||'reopen_batch error : SETUP_FAILURE'
                               );
         END IF;

         x_return_status := fnd_api.g_ret_sts_error;
         /*N Punit Kumar */
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
      WHEN batch_reopen_failure OR batch_save_failed THEN
         ROLLBACK TO SAVEPOINT reopen_batch;
         x_batch_header_rec := NULL;

         IF (g_debug <= gme_debug.g_log_procedure) THEN
            gme_debug.put_line (g_pkg_name || '.' || l_api_name || ':'
               || 'reopen_batch error : BATCH_REOPEN_FAILURE OR BATCH_SAVE_FAILED OR ERROR_CHECK_PHANT.'
              );
         END IF;

         /*N Punit Kumar */
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN OTHERS THEN
         ROLLBACK TO SAVEPOINT reopen_batch;
         x_batch_header_rec := NULL;

         IF (g_debug <= gme_debug.g_log_unexpected) THEN
            gme_debug.put_line (g_pkg_name || '.' || l_api_name || ':'
            ||'reopen_batch error : OTHERS.' || SQLCODE
                               );
         END IF;

         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
        
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
         x_return_status := fnd_api.g_ret_sts_unexp_error;
   END reopen_batch;

/*************************************************************************/
   PROCEDURE reopen_step (
      p_validation_level   IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,                                                       /* Punit Kumar */
      p_init_msg_list      IN              VARCHAR2 := fnd_api.g_false
     ,x_message_count      OUT NOCOPY      NUMBER
     ,x_message_list       OUT NOCOPY      VARCHAR2
     ,x_return_status      OUT NOCOPY      VARCHAR2
     ,p_batch_header_rec   IN              gme_batch_header%ROWTYPE
     ,p_batch_step_rec     IN              gme_batch_steps%ROWTYPE
     ,x_batch_step_rec     OUT NOCOPY      gme_batch_steps%ROWTYPE)
   IS
      l_api_name   CONSTANT VARCHAR2 (30)              := 'REOPEN_STEP';
      setup_failure         EXCEPTION;
      step_save_failed      EXCEPTION;
      step_reopen_failure   EXCEPTION;
      l_batch_header        gme_batch_header%ROWTYPE;
   BEGIN
      -- Set the save point before proceeding
      SAVEPOINT reopen_batch_step;

      /* Initialize message list and count if needed*/
      IF p_init_msg_list = fnd_api.g_true THEN
         fnd_msg_pub.initialize;
         gme_common_pvt.g_error_count := 0;                 /* Punit Kumar */
      END IF;

      IF (g_debug IS NOT NULL) THEN
         gme_debug.log_initialize ('ReopenStep');
      END IF;

      IF (g_debug <= gme_debug.g_log_procedure) THEN
        gme_debug.put_line (g_pkg_name || '.' || l_api_name || ':'
                             || 'entering');
      END IF;
      IF NOT gme_common_pvt.g_setup_done THEN
         gme_common_pvt.g_setup_done :=
                    gme_common_pvt.setup (p_batch_header_rec.organization_id);

         IF NOT gme_common_pvt.g_setup_done THEN
            x_return_status := fnd_api.g_ret_sts_error;
            RAISE setup_failure;
         END IF;
      END IF;
      /* Set the success staus to success inititally*/
      x_return_status := fnd_api.g_ret_sts_success;
      /* Punit Kumar */
      gme_common_pvt.set_timestamp;

      IF (g_debug <= gme_debug.g_log_procedure) THEN
        gme_debug.put_line (g_pkg_name || '.' || l_api_name || ':'
                             || 'calling private layer');
      END IF;

      gme_reopen_step_pvt.reopen_step (p_batch_step_rec      => p_batch_step_rec
                                      ,x_batch_step_rec      => x_batch_step_rec
                                      ,x_return_status       => x_return_status);

      IF (g_debug <= gme_debug.g_log_procedure) THEN
        gme_debug.put_line (g_pkg_name || '.' || l_api_name || ':'
                             || 'existing private layer with status'||x_return_status );
      END IF;

      IF (x_return_status = fnd_api.g_ret_sts_success) THEN
         NULL;
      ELSE
         RAISE step_reopen_failure;
      END IF;

     
         gme_common_pvt.log_message ('GME_API_STEP_REOPENED');
   

      IF (g_debug <= gme_debug.g_log_procedure) THEN
         gme_debug.put_line (   'Completed '
                             || l_api_name
                             || ' at '
                             || TO_CHAR (SYSDATE, 'MM/DD/YYYY HH24:MI:SS') );
      END IF;

      gme_common_pvt.count_and_get (x_count        => x_message_count
                                   ,p_encoded      => fnd_api.g_false
                                   ,x_data         => x_message_list);

      IF (g_debug <= gme_debug.g_log_procedure) THEN
         gme_debug.put_line ('Normal end of Public Reopen_Step.'
                            ,gme_debug.g_log_procedure
                            ,'reopen_batch');
      END IF;
   EXCEPTION
      WHEN setup_failure THEN
         ROLLBACK TO SAVEPOINT reopen_batch_step;
         x_batch_step_rec := NULL;

         IF (g_debug <= gme_debug.g_log_procedure) THEN
             gme_debug.put_line (g_pkg_name || '.' || l_api_name || ':'
                             ||'reopen_step error : SETUP_FAILURE.'
                               );
         END IF;

         x_return_status := fnd_api.g_ret_sts_error;
         /* Punit Kumar */
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
      WHEN step_reopen_failure OR step_save_failed THEN
         ROLLBACK TO SAVEPOINT reopen_batch_step;
         x_batch_step_rec := NULL;

         IF (g_debug <= gme_debug.g_log_procedure) THEN
             gme_debug.put_line (g_pkg_name || '.' || l_api_name || ':'
                             ||
               'reopen_step error : STEP_REOPEN_FAILURE OR STEP_SAVE_FAILED.'
              );
         END IF;

         /* Punit Kumar */
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN OTHERS THEN
         ROLLBACK TO SAVEPOINT reopen_batch_step;
         x_batch_step_rec := NULL;

        IF (g_debug <= gme_debug.g_log_unexpected) THEN
            gme_debug.put_line (g_pkg_name || '.' || l_api_name || ':'
            ||'reopen_step error : OTHERS.' || SQLCODE
                               );
         END IF;

         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
         /* Punit Kumar */
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
         x_return_status := fnd_api.g_ret_sts_unexp_error;
   END reopen_step;

/*================================================================================
    Procedure
      incremental_backflush
    Description
      This procedure is used to incrementally backflush the qty to the material line.

    Parameters
      p_batch_header_rec (R)    The batch header record
      p_material_detail_rec (R) The material detail record
      p_qty (R)                 The quantity to apply incrementally as follows:
      p_qty_type (R)            0 - By increment qty
                                1 - New actual qty
                                2 - % of Plan
      p_trans_date              Transaction date to record for the incremental backflush
      x_exception_material_tab  Table of materials that could not be consumed or yielded
                                for the calculated incremental quantity
      x_return_status           result of the API call
                                S - Success
                                E - Error
                                U - Unexpected Error
                                X - Batch Exception

   HISTORY      
      05-AUG-2009   G. Muratore   Bug 8639523 
        Clear the cache just in case any transactions hit the tree.
        A blank error message appeared and/or inventory was driven negative upon
        clicking ok a second time. This was due to the qty tree not being accurate.
      
      24-Nov-2009   G. Muratore   Bug 8751983 
        Set the IB specific globals to potentially be used for negative IB.
   
      G. Muratore    03-APR-2012   Bug 13881792
         Keep the user entered transaction date when exceptions appear so it can be used later on the form.  
   
      G. Muratore    21-OCT-2015   Bug 19868921
         Reset partial negative IB global gme_common_pvt.g_ib_going_negative.
         
      G. Muratore    05-APR-2016   Bug 22317010 / 22764488
         Introduced new p_create_resv_pend_lots and p_tolerance parameter.
         p_create_resv_pend_lots acceptable values are 1 or zero. This was already
         in place in lower level code, but was never passed in. This allows user to have more
         control on re-creating reservations and pending product lots during negative IB.

         p_tolerance acceptable values are between 1 and .99. This was introduced for handling
         problems caused by rounding over the course of multiple IB's. It will be used only to 
         determine if user is essentially on the last IB and how to relieve a reservation.
         
         Introduced new p_lpn_id parameter for future use. Bugs 13628717/12884831.    
  ================================================================================*/
   PROCEDURE incremental_backflush (
      p_validation_level         IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list            IN              VARCHAR2 := fnd_api.g_false
     ,x_message_count            OUT NOCOPY      NUMBER
     ,x_message_list             OUT NOCOPY      VARCHAR2
     ,x_return_status            OUT NOCOPY      VARCHAR2
     ,p_batch_header_rec         IN              gme_batch_header%ROWTYPE
     ,p_material_detail_rec      IN              gme_material_details%ROWTYPE
     ,p_qty                      IN              NUMBER
     ,p_qty_type                 IN              NUMBER
     ,p_trans_date               IN              DATE
     ,p_create_resv_pend_lots    IN              NUMBER DEFAULT 1 -- Bug 22317010 
     ,p_tolerance                IN              NUMBER DEFAULT NULL -- Bug 22317010 / 22764488
     ,p_lpn_id                   IN              NUMBER DEFAULT NULL -- for future use Bugs 13628717 and 12884831
     ,x_exception_material_tbl   OUT NOCOPY      gme_common_pvt.exceptions_tab )
   IS
      l_api_name            CONSTANT VARCHAR2 (30) := 'INCREMENTAL_BACKFLUSH';
      l_trans_date                   DATE;

      l_backflush_rsrc_usg_ind       NUMBER;

      incremental_backflush_failed   EXCEPTION;
      setup_failure                  EXCEPTION;
   BEGIN
      /* Set the savepoint */
      SAVEPOINT incremental_backflush;
      
      IF (g_debug IS NOT NULL) THEN
         gme_debug.log_initialize ('IncrementalBackflush');
      END IF;
      /* Set the return status to success initially */
      x_return_status := fnd_api.g_ret_sts_success;
      
      /* Setup the common constants used across the apis */
     IF NOT gme_common_pvt.g_setup_done THEN
         gme_common_pvt.g_setup_done :=
                    gme_common_pvt.setup (p_batch_header_rec.organization_id);

         IF NOT gme_common_pvt.g_setup_done THEN
            x_return_status := fnd_api.g_ret_sts_error;
            RAISE setup_failure;
         END IF;
      END IF;
      
      /* Initialize message list and count if needed */
      IF p_init_msg_list = fnd_api.g_true THEN
         fnd_msg_pub.initialize;
         gme_common_pvt.g_error_count := 0;
      END IF;

      gme_common_pvt.set_timestamp;

      l_trans_date := p_trans_date;

      -- Bug 19868921 - Reset partial negative IB global.
      gme_common_pvt.g_ib_going_negative := 0;

      -- 8751983 - Set IB globals to be used later for resource trans reversals during negative IB. 
      IF l_trans_date IS NOT NULL THEN
         gme_common_pvt.g_ib_timestamp_set := 1;
         gme_common_pvt.g_ib_timestamp_date := l_trans_date;
      ELSE        
         l_trans_date := gme_common_pvt.g_timestamp;
         gme_common_pvt.g_ib_timestamp_set := 0;
         gme_common_pvt.g_ib_timestamp_date := NULL;        
      END IF;
      
      
      IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
          gme_debug.put_line('l_trans_date is '||to_char(l_trans_date, 'DD-MON-YYYY HH24:MI:SS'));
          gme_debug.put_line('gme_common_pvt.g_ib_timestamp_set is '||gme_common_pvt.g_ib_timestamp_set);
      END IF;

      -- does backflush resource usage need to be performed?
      IF (p_batch_header_rec.batch_status = gme_common_pvt.g_step_wip AND
           p_material_detail_rec.line_type = gme_common_pvt.g_line_type_prod AND
           gme_common_pvt.g_backflush_rsrc_usg_ind = 1) THEN
        l_backflush_rsrc_usg_ind := 1;
      ELSE
        l_backflush_rsrc_usg_ind := 0;
      END IF;

      gme_incremental_backflush_pvt.incremental_backflush
                        (p_batch_header_rec            => p_batch_header_rec
                        ,p_material_detail_rec         => p_material_detail_rec
                        ,p_qty                         => p_qty
                        ,p_qty_type                    => p_qty_type
                        ,p_trans_date                  => l_trans_date
                        ,p_backflush_rsrc_usg_ind      => l_backflush_rsrc_usg_ind
                        ,p_create_resv_pend_lots       => p_create_resv_pend_lots -- Bug 22317010 
                        ,p_tolerance                   => p_tolerance -- Bug 22317010 / 22764488 
                        ,p_lpn_id                      => p_lpn_id -- for future use Bugs 13628717 and 12884831 
                        ,x_exception_material_tbl      => x_exception_material_tbl
                        ,x_return_status               => x_return_status);

      -- Bug 13881792 - Do not reset flag here. Moved below.
      -- Bug 13017256 - Reset flag.
      -- gme_common_pvt.g_ib_timestamp_set := 0;

      -- Bug 19868921 - Reset partial negative IB global.
      gme_common_pvt.g_ib_going_negative := 0;

      IF x_return_status NOT IN
                 (fnd_api.g_ret_sts_success, gme_common_pvt.g_exceptions_err) THEN
         IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
           gme_debug.put_line (g_pkg_name||'.'||l_api_name||' after gme_incremental_backflush_pvt.incremental_backflush; x_return_status= '||x_return_status);
         END IF;
         
         -- Bug 13017256 - Reset flag.
         gme_common_pvt.g_ib_timestamp_set := 0;         
         RAISE incremental_backflush_failed;
      END IF;    /* IF x_return_status NOT IN */

     /*Bug#5277982 if there are any exceptions then we give message saying IB done with exceptions*/
      IF x_exception_material_tbl.COUNT > 0  THEN
         gme_common_pvt.log_message('GME_IB_EXCEPTIONS');
         -- Bug 13881792
         IF (gme_common_pvt.g_ib_timestamp_set <> 0) THEN
            gme_common_pvt.g_ib_timestamp_set := -1;
         END IF;
      ELSE
         gme_common_pvt.log_message ('GME_API_PARTIAL_CERTIFIED');
         -- Bug 13017256 - Reset flag.
         gme_common_pvt.g_ib_timestamp_set := 0;                  
      END IF;     

      IF (g_debug <= gme_debug.g_log_procedure) THEN
         gme_debug.put_line (   'gme_api_main: Completed '
                             || l_api_name
                             || ' at '
                             || TO_CHAR (SYSDATE, 'MM/DD/YYYY HH24:MI:SS') );
      END IF;

      gme_common_pvt.count_and_get (x_count        => x_message_count
                                   ,p_encoded      => fnd_api.g_false
                                   ,x_data         => x_message_list);
   EXCEPTION
      WHEN setup_failure THEN
         ROLLBACK TO SAVEPOINT incremental_backflush;
         x_return_status := fnd_api.g_ret_sts_error;
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
      WHEN incremental_backflush_failed THEN
         ROLLBACK TO SAVEPOINT incremental_backflush;
         -- Bug 8639523 - Clear the cache just in case any transactions hit the tree.
         inv_quantity_tree_pub.clear_quantity_cache;
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
         IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
           gme_debug.put_line (g_pkg_name||'.'||l_api_name||' in exception block; x_return_status= '||x_return_status);
         END IF;
      WHEN OTHERS THEN
         ROLLBACK TO SAVEPOINT incremental_backflush;
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
         x_return_status := fnd_api.g_ret_sts_unexp_error;
   END incremental_backflush;

    /*================================================================================
     Procedure
       reroute_batch
     Description
       This procedure reroutes batch (typically change the route associated with the batch).

     Parameters
       p_batch_header_rec (R)    The batch header row to identify the batch
                                 Following columns are used from this row.
                                 batch_id  (R)
       p_validity_rule_id (R)    Recipe validity rule id for the new recipe.

       x_batch_header_rec        The batch header that is returned, with all the data
       x_return_status           outcome of the API call
                                 S - Success
                                 E - Error
                                 U - Unexpected Error
                                 C - No continous periods found
   ================================================================================*/
   PROCEDURE reroute_batch (
      p_validation_level      IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list         IN              VARCHAR2 DEFAULT fnd_api.g_false
     ,p_batch_header_rec      IN              gme_batch_header%ROWTYPE
     ,p_validity_rule_id      IN              NUMBER
     ,p_use_workday_cal       IN              VARCHAR2 DEFAULT fnd_api.g_false
     ,p_contiguity_override   IN              VARCHAR2 DEFAULT fnd_api.g_false
     ,x_message_count         OUT NOCOPY      NUMBER
     ,x_message_list          OUT NOCOPY      VARCHAR2
     ,x_return_status         OUT NOCOPY      VARCHAR2
     ,x_batch_header_rec      OUT NOCOPY      gme_batch_header%ROWTYPE)
   IS
      l_api_name    CONSTANT VARCHAR2 (30) := 'REROUTE_BATCH';
      no_continous_periods   EXCEPTION;
      setup_failure          EXCEPTION;
   BEGIN
      /* Set savepoint here */
      SAVEPOINT reroute_batch_main;

      IF (g_debug IS NOT NULL) THEN
         gme_debug.log_initialize ('RerouteBatch');
      END IF;

      IF (NVL (g_debug, 0) IN
                       (gme_debug.g_log_statement, gme_debug.g_log_procedure) ) THEN
         gme_debug.put_line (g_pkg_name || '.' || l_api_name || ':'
                             || 'Entering');
      END IF;

      IF (fnd_api.to_boolean (p_init_msg_list) ) THEN
         fnd_msg_pub.initialize;
         gme_common_pvt.g_error_count := 0;
      END IF;

      /* Set the return status to success initially */
      x_return_status := fnd_api.g_ret_sts_success;

      IF NOT gme_common_pvt.g_setup_done THEN
         gme_common_pvt.g_setup_done :=
                    gme_common_pvt.setup (p_batch_header_rec.organization_id);

         IF NOT gme_common_pvt.g_setup_done THEN
            x_return_status := fnd_api.g_ret_sts_error;
            RAISE setup_failure;
         END IF;
      END IF;

      gme_common_pvt.set_timestamp;
      gme_reroute_batch_pvt.reroute_batch
                              (p_batch_header_rec         => p_batch_header_rec
                              ,p_validity_rule_id         => p_validity_rule_id
                              ,p_use_workday_cal          => p_use_workday_cal
                              ,p_contiguity_override      => p_contiguity_override
                              ,x_return_status            => x_return_status
                              ,x_batch_header_rec         => x_batch_header_rec);

      IF (x_return_status = 'C') THEN
         RAISE no_continous_periods;
      ELSIF (x_return_status = fnd_api.g_ret_sts_error) THEN
         RAISE fnd_api.g_exc_error;
      ELSIF (x_return_status = fnd_api.g_ret_sts_unexp_error) THEN
         RAISE fnd_api.g_exc_unexpected_error;
      ELSE
         --FPBug#5040865 Begin
         IF x_batch_header_rec.batch_type = 0 THEN
           FND_MESSAGE.SET_NAME('GME','GME_BATCH');
         ELSE
           FND_MESSAGE.SET_NAME('GME','GME_FIRM_PLAN_ORDER');
         END IF;
         gme_common_pvt.log_message ('GME_API_BATCH_REROUTED','DOC',FND_MESSAGE.GET);
        --FPBug#5040865 End     
      END IF;

      gme_common_pvt.count_and_get (x_count        => x_message_count
                                   ,p_encoded      => fnd_api.g_false
                                   ,x_data         => x_message_list);

      IF (NVL (g_debug, 0) IN
                       (gme_debug.g_log_statement, gme_debug.g_log_procedure) ) THEN
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ':'
                             || 'Exiting with '
                             || x_return_status);
      END IF;
   EXCEPTION
      WHEN setup_failure THEN
         ROLLBACK TO SAVEPOINT reroute_batch_main;
         x_batch_header_rec := NULL;
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN no_continous_periods THEN
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
      WHEN fnd_api.g_exc_error THEN
         ROLLBACK TO SAVEPOINT reroute_batch_main;
         x_batch_header_rec := NULL;
         x_return_status := fnd_api.g_ret_sts_error;
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
      WHEN fnd_api.g_exc_unexpected_error THEN
         ROLLBACK TO SAVEPOINT reroute_batch_main;
         x_batch_header_rec := NULL;
         x_return_status := fnd_api.g_ret_sts_unexp_error;

         IF (NVL (g_debug, 0) > 0) THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ':'
                                || 'UNEXPECTED:'
                                || SQLERRM);
         END IF;

         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
      WHEN OTHERS THEN
         ROLLBACK TO SAVEPOINT reroute_batch_main;
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
         x_batch_header_rec := NULL;
         x_return_status := fnd_api.g_ret_sts_unexp_error;

         IF (NVL (g_debug, 0) > 0) THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ':'
                                || 'OTHERS:'
                                || SQLERRM);
         END IF;

         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
   END reroute_batch;

   /*================================================================================
     Procedure
       cancel_batch
     Description
       This procedure cancels batch and all the phantom batches.
        It also cancels all the steps.

     Parameters
       p_batch_header (R)        The batch header row to identify the batch
                                 Following columns are used from this row.
                                 batch_id  (R)
       x_batch_header            The batch header that is returned, with all the data
       x_return_status           outcome of the API call
                                 S - Success
                                 E - Error
                                 U - Unexpected Error
   ================================================================================*/
   PROCEDURE cancel_batch (
      p_validation_level   IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list      IN              VARCHAR2 := fnd_api.g_false
     ,x_message_count      OUT NOCOPY      NUMBER
     ,x_message_list       OUT NOCOPY      VARCHAR2
     ,x_return_status      OUT NOCOPY      VARCHAR2
     ,p_batch_header_rec   IN              gme_batch_header%ROWTYPE
     ,x_batch_header_rec   OUT NOCOPY      gme_batch_header%ROWTYPE)
   IS   
      -- Bug 19781489 - this should come back with zero.         
      CURSOR cur_get_stuck_temp_count IS
      SELECT COUNT(1)
        FROM mtl_material_transactions_temp t, gme_batch_header h
       WHERE t.transaction_source_type_id = 5
         AND h.batch_id = p_batch_header_rec.batch_id
         AND t.organization_id = p_batch_header_rec.organization_id
         AND t.transaction_source_id = h.batch_id;

      -- Bug 19781489 - this should come back with zero.         
      CURSOR cur_get_stuck_inter_count IS
      SELECT COUNT(1)
        FROM mtl_transactions_interface t, gme_batch_header h
       WHERE t.transaction_source_type_id = 5
         AND h.batch_id = p_batch_header_rec.batch_id
         AND t.organization_id = p_batch_header_rec.organization_id
         AND t.transaction_source_id = h.batch_id;        
         
      -- Bug 19781489 - this should come back with zero.         
      CURSOR cur_get_bad_actual_count IS
      SELECT COUNT(1)
        FROM gme_material_details
       WHERE actual_qty > 0
         AND batch_id = p_batch_header_rec.batch_id;
         
      l_api_name    CONSTANT VARCHAR2 (30) := 'CANCEL_BATCH';
      l_count       NUMBER;
      
      setup_failure          EXCEPTION;
      batch_cancel_failure   EXCEPTION;
      
      actual_qty_exist         EXCEPTION;
      stuck_inter_trans_exist  EXCEPTION;
      stuck_temp_trans_exist   EXCEPTION;

   BEGIN
      SAVEPOINT cancel_batch;

      IF (g_debug IS NOT NULL) THEN
         gme_debug.log_initialize ('CancelBatch');
      END IF;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line (g_pkg_name || '.' || l_api_name || ':'
                             || 'Entering');
      END IF;

      /* Set the return status to success initially */
      x_return_status := fnd_api.g_ret_sts_success;

      IF NOT gme_common_pvt.g_setup_done THEN
         gme_common_pvt.g_setup_done :=
                    gme_common_pvt.setup (p_batch_header_rec.organization_id);

         IF NOT gme_common_pvt.g_setup_done THEN
            x_return_status := fnd_api.g_ret_sts_error;
            RAISE setup_failure;
         END IF;
      END IF;

      -- Initialize message list and count if needed
      IF p_init_msg_list = fnd_api.g_true THEN
         fnd_msg_pub.initialize;
         gme_common_pvt.g_error_count := 0;
      END IF;

      gme_common_pvt.set_timestamp;

      -- Bug 19781489 - lets check to see if there are any stuck transactions or actuals.      
      IF g_debug <= gme_debug.g_log_statement THEN
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ': '
                             || 'Check actuals and stuck trans.');
      END IF;      
      
      OPEN  cur_get_stuck_temp_count;
      FETCH cur_get_stuck_temp_count INTO l_count;
      CLOSE cur_get_stuck_temp_count;
      
      IF l_count > 0 THEN
         IF g_debug <= gme_debug.g_log_statement THEN
            gme_debug.put_line ('stuck mmtt trans exist so stop.');
         END IF;      
         gme_common_pvt.log_message ('GME_STUCK_TEMP_TRANS_EXIST');     
         RAISE stuck_temp_trans_exist;
      END IF;

      OPEN  cur_get_stuck_inter_count;
      FETCH cur_get_stuck_inter_count INTO l_count;
      CLOSE cur_get_stuck_inter_count;
      
      IF l_count > 0 THEN
         IF g_debug <= gme_debug.g_log_statement THEN
            gme_debug.put_line ('stuck mti trans exist so stop.');
         END IF;      
         gme_common_pvt.log_message ('GME_STUCK_INTER_TRANS_EXIST');     
         RAISE stuck_inter_trans_exist;
      END IF;

      OPEN  cur_get_bad_actual_count;
      FETCH cur_get_bad_actual_count INTO l_count;
      CLOSE cur_get_bad_actual_count;
      
      IF l_count > 0 THEN
         IF g_debug <= gme_debug.g_log_statement THEN
            gme_debug.put_line ('actuals exist so stop.');
         END IF;      
         gme_common_pvt.log_message ('GME_ACTUAL_QTY_EXIST');     
         RAISE actual_qty_exist;
      END IF;
            
      IF g_debug <= gme_debug.g_log_statement THEN
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ':'
                             || 'calling pvt cancel');
      END IF;

      gme_cancel_batch_pvt.cancel_batch
                                    (p_batch_header_rec      => p_batch_header_rec
                                    ,x_batch_header_rec      => x_batch_header_rec
                                    ,x_return_status         => x_return_status);

      IF (g_debug <= gme_debug.g_log_procedure) THEN
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ':'
                             || 'x_return_status='
                             || x_return_status);
      END IF;

      IF x_return_status <> fnd_api.g_ret_sts_success THEN
         RAISE batch_cancel_failure;
      END IF;            /* IF x_return_status <> FND_API.G_RET_STS_SUCCESS */

      --FPBug#5040865 Begin
      IF x_batch_header_rec.batch_type = 0 THEN
         FND_MESSAGE.SET_NAME('GME','GME_BATCH');
      ELSE
         FND_MESSAGE.SET_NAME('GME','GME_FIRM_PLAN_ORDER');
      END IF;
      gme_common_pvt.log_message ('GME_API_BATCH_CANCELLED','DOC',FND_MESSAGE.GET);
      --FPBug#5040865 End

      gme_common_pvt.count_and_get (x_count        => x_message_count
                                   ,p_encoded      => fnd_api.g_false
                                   ,x_data         => x_message_list);

      IF (g_debug IS NOT NULL) THEN
         gme_debug.put_line (   'Completed '
                             || l_api_name
                             || ' at '
                             || TO_CHAR (SYSDATE, 'MM/DD/YYYY HH24:MI:SS') );
      END IF;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line (   'Exiting api '
                             || g_pkg_name
                             || '.'
                             || l_api_name
                             || x_return_status);
      END IF;
   EXCEPTION
      WHEN batch_cancel_failure THEN
         ROLLBACK TO SAVEPOINT cancel_batch;
         x_batch_header_rec := NULL;
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
      WHEN actual_qty_exist OR stuck_inter_trans_exist OR stuck_temp_trans_exist THEN
         ROLLBACK TO SAVEPOINT cancel_batch;
         x_batch_header_rec := NULL;
         x_return_status := fnd_api.g_ret_sts_error;
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);                  
      WHEN setup_failure THEN
         ROLLBACK TO SAVEPOINT cancel_batch;
         x_batch_header_rec := NULL;
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
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

         ROLLBACK TO SAVEPOINT cancel_batch;
         x_batch_header_rec := NULL;
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
         x_return_status := fnd_api.g_ret_sts_unexp_error;
   END cancel_batch;

    /*================================================================================
     Procedure
       terminate_batch

     Description
       This procedure terminates batch and all the phantom batches.
        It also terminates all the steps.

     Parameters
       p_batch_header (R)        The batch header row to identify the batch
                                 Following columns are used from this row.
                                 batch_id  (R)
       p_reason_name             Reason to terminate the batch
       x_batch_header            The batch header that is returned, with all the data
       x_return_status           outcome of the API call
                                 S - Success
                                 E - Error
                                 U - Unexpected Error
   ================================================================================*/
   PROCEDURE terminate_batch (
      p_validation_level   IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list      IN              VARCHAR2 := fnd_api.g_false
     ,x_message_count      OUT NOCOPY      NUMBER
     ,x_message_list       OUT NOCOPY      VARCHAR2
     ,x_return_status      OUT NOCOPY      VARCHAR2
     ,p_batch_header_rec   IN              gme_batch_header%ROWTYPE
     ,x_batch_header_rec   OUT NOCOPY      gme_batch_header%ROWTYPE)
   IS
      l_api_name       CONSTANT VARCHAR2 (30) := 'TERMINATE_BATCH';
      setup_failure             EXCEPTION;
      batch_terminate_failure   EXCEPTION;
   BEGIN
      /* Set the save point before processing */
      SAVEPOINT terminate_batch;

      IF (g_debug IS NOT NULL) THEN
         gme_debug.log_initialize ('TerminateBatch');
      END IF;

      IF (g_debug <= gme_debug.g_log_procedure) THEN
         gme_debug.put_line (g_pkg_name || '.' || l_api_name || ':'
                             || 'Entering');
      END IF;

      /* Set the return status to success initially */
      x_return_status := fnd_api.g_ret_sts_success;

      IF NOT gme_common_pvt.g_setup_done THEN
         gme_common_pvt.g_setup_done :=
                    gme_common_pvt.setup (p_batch_header_rec.organization_id);

         IF NOT gme_common_pvt.g_setup_done THEN
            x_return_status := fnd_api.g_ret_sts_error;
            RAISE setup_failure;
         END IF;
      END IF;

      -- Initialize message list and count if needed
      IF p_init_msg_list = fnd_api.g_true THEN
         fnd_msg_pub.initialize;
         gme_common_pvt.g_error_count := 0;
      END IF;

      gme_common_pvt.set_timestamp;

      IF g_debug <= gme_debug.g_log_statement THEN
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ':'
                             || 'Call Private Terminate_Batch');
      END IF;

      gme_terminate_batch_pvt.terminate_batch
                                    (p_batch_header_rec      => p_batch_header_rec
                                    ,x_batch_header_rec      => x_batch_header_rec
                                    ,x_return_status         => x_return_status);

      IF (g_debug <= gme_debug.g_log_procedure) THEN
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ':'
                             || 'x_return_status='
                             || x_return_status);
      END IF;

      IF x_return_status <> fnd_api.g_ret_sts_success THEN
         RAISE batch_terminate_failure;
      END IF;            /* IF x_return_status <> FND_API.G_RET_STS_SUCCESS */

         gme_common_pvt.log_message ('GME_API_BATCH_TERMINATED');
    

      gme_common_pvt.count_and_get (x_count        => x_message_count
                                   ,p_encoded      => fnd_api.g_false
                                   ,x_data         => x_message_list);

      IF (g_debug IS NOT NULL) THEN
         gme_debug.put_line (   'Completed '
                             || l_api_name
                             || ' at '
                             || TO_CHAR (SYSDATE, 'MM/DD/YYYY HH24:MI:SS') );
      END IF;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ':'
                             || 'Exiting api with return status='
                             || x_return_status);
      END IF;
   EXCEPTION
      WHEN setup_failure THEN
         ROLLBACK TO SAVEPOINT terminate_batch;
         x_batch_header_rec := NULL;

         IF (g_debug <= gme_debug.g_log_procedure) THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ':'
                                || 'SETUP_FAILURE.');
         END IF;

         x_return_status := fnd_api.g_ret_sts_error;
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
      WHEN batch_terminate_failure THEN
         ROLLBACK TO SAVEPOINT terminate_batch;
         x_batch_header_rec := NULL;

         IF (g_debug <= gme_debug.g_log_procedure) THEN
            gme_debug.put_line
                             (   g_pkg_name
                              || '.'
                              || l_api_name
                              || ':'
                              || 'BATCH_TERMINATE_FAILURE OR BATCH_SAVE_FAILED.');
         END IF;

         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
      WHEN OTHERS THEN
         ROLLBACK TO SAVEPOINT terminate_batch;
         x_batch_header_rec := NULL;

         IF (g_debug <= gme_debug.g_log_unexpected) THEN
            gme_debug.put_line (   'When others exception in '
                                || g_pkg_name
                                || '.'
                                || l_api_name
                                || ' Error is '
                                || SQLERRM);
         END IF;

         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
         x_return_status := fnd_api.g_ret_sts_unexp_error;
   END terminate_batch;

/*************************************************************************/
   PROCEDURE unrelease_batch (
      p_validation_level        IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list           IN              VARCHAR2 := fnd_api.g_false
     ,x_message_count           OUT NOCOPY      NUMBER
     ,x_message_list            OUT NOCOPY      VARCHAR2
     ,x_return_status           OUT NOCOPY      VARCHAR2
     ,p_batch_header_rec        IN              gme_batch_header%ROWTYPE
     ,x_batch_header_rec        OUT NOCOPY      gme_batch_header%ROWTYPE
     ,p_create_resv_pend_lots   IN              NUMBER)
   IS
      l_api_name       CONSTANT VARCHAR2 (30) := 'UNRELEASE_BATCH';
      setup_failure             EXCEPTION;
      batch_unrelease_failure   EXCEPTION;
      l_recalculate_count          NUMBER;
      
      --BUG 24351209 check recalculate flag
      CURSOR check_batch_recalculate_flag(v_batch_id IN NUMBER) IS
        SELECT count(1)
          FROM gme_batch_genealogy
         WHERE batch_id = v_batch_id
           AND NVL(recalculate_yield,'N') <> 'Y';      
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      SAVEPOINT unrelease_batch;

      IF (g_debug IS NOT NULL) THEN
         gme_debug.log_initialize ('UnreleaseBatch');
      END IF;

      IF NOT gme_common_pvt.g_setup_done THEN
         gme_common_pvt.g_setup_done :=
                    gme_common_pvt.setup (p_batch_header_rec.organization_id);

         IF NOT gme_common_pvt.g_setup_done THEN
            x_return_status := fnd_api.g_ret_sts_error;
            RAISE setup_failure;
         END IF;
      END IF;

      /* Set the return status to success initially */
      x_return_status := fnd_api.g_ret_sts_success;

      -- Initialize message list and count if needed
      IF p_init_msg_list = fnd_api.g_true THEN
         fnd_msg_pub.initialize;
         gme_common_pvt.g_error_count := 0;
      END IF;

      gme_common_pvt.set_timestamp;
      gme_common_pvt.reset_txn_hdr_tbl; -- nsinghi bug#5176319
      gme_unrelease_batch_pvt.unrelease_batch
                          (p_batch_header_rec           => p_batch_header_rec
                          ,p_create_resv_pend_lots      => p_create_resv_pend_lots
                          ,x_batch_header_rec           => x_batch_header_rec
                          ,x_return_status              => x_return_status);

      IF x_return_status <> fnd_api.g_ret_sts_success THEN
         RAISE batch_unrelease_failure;
      END IF;            /* IF x_return_status <> FND_API.G_RET_STS_SUCCESS */
   
         gme_common_pvt.log_message ('GME_API_BATCH_UNRELEASED');
    
      gme_common_pvt.count_and_get (x_count        => x_message_count
                                   ,p_encoded      => fnd_api.g_false
                                   ,x_data         => x_message_list);
      
      --BUG 24351209 invoking calculate yield api to calculate yield for batch                             
	    GME_YIELD_CALCULATION_PVT.calc_batch_yield(p_batch_header_rec.batch_id);   
      --BUG 24351209  update recalculate yield flag to Y so that
      --comput yield concurrent program can capture this batch.
      OPEN check_batch_recalculate_flag(p_batch_header_rec.batch_id);
      FETCH check_batch_recalculate_flag INTO l_recalculate_count;
      IF l_recalculate_count > 0 THEN
        UPDATE gme_batch_genealogy
           SET recalculate_yield = 'Y'
         WHERE batch_id = p_batch_header_rec.batch_id;

      END IF;
      CLOSE check_batch_recalculate_flag;                                      

      IF (g_debug IS NOT NULL) THEN
         gme_debug.put_line (   'Completed '
                             || l_api_name
                             || ' at '
                             || TO_CHAR (SYSDATE, 'MM/DD/YYYY HH24:MI:SS') );
      END IF;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;
   EXCEPTION
      WHEN batch_unrelease_failure THEN
         ROLLBACK TO SAVEPOINT unrelease_batch;
         x_batch_header_rec := NULL;
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
      WHEN setup_failure THEN
         ROLLBACK TO SAVEPOINT unrelease_batch;
         x_batch_header_rec := NULL;
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
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

         ROLLBACK TO SAVEPOINT unrelease_batch;
         x_batch_header_rec := NULL;
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
         x_return_status := fnd_api.g_ret_sts_unexp_error;
   END unrelease_batch;

/*************************************************************************/
   PROCEDURE unrelease_step (
      p_validation_level        IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list           IN              VARCHAR2 := fnd_api.g_false
     ,x_message_count           OUT NOCOPY      NUMBER
     ,x_message_list            OUT NOCOPY      VARCHAR2
     ,x_return_status           OUT NOCOPY      VARCHAR2
     ,p_batch_step_rec          IN              gme_batch_steps%ROWTYPE
     ,p_batch_header_rec        IN              gme_batch_header%ROWTYPE
     ,x_batch_step_rec          OUT NOCOPY      gme_batch_steps%ROWTYPE
     ,p_create_resv_pend_lots   IN              NUMBER)
   IS
      l_api_name      CONSTANT VARCHAR2 (30) := 'UNRELEASE_STEP';
      setup_failure            EXCEPTION;
      step_unrelease_failure   EXCEPTION;
      
      
      --BUG 24351209 check recalculate flag
      CURSOR check_batch_recalculate_flag(v_batch_id IN NUMBER) IS
        SELECT count(1)
          FROM gme_batch_genealogy
         WHERE batch_id = v_batch_id
           AND NVL(recalculate_yield,'N') <> 'Y'; 
                        
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      SAVEPOINT unrelease_step;

      IF (g_debug IS NOT NULL) THEN
         gme_debug.log_initialize ('UnreleaseStep');
      END IF;

      IF NOT gme_common_pvt.g_setup_done THEN
         gme_common_pvt.g_setup_done :=
                    gme_common_pvt.setup (p_batch_header_rec.organization_id);

         IF NOT gme_common_pvt.g_setup_done THEN
            x_return_status := fnd_api.g_ret_sts_error;
            RAISE setup_failure;
         END IF;
      END IF;

      /* Set the return status to success initially */
      x_return_status := fnd_api.g_ret_sts_success;

      -- Initialize message list and count if needed
      IF p_init_msg_list = fnd_api.g_true THEN
         fnd_msg_pub.initialize;
         gme_common_pvt.g_error_count := 0;
      END IF;

      gme_common_pvt.set_timestamp;
      gme_common_pvt.reset_txn_hdr_tbl; -- nsinghi bug#5176319
      gme_unrelease_step_pvt.unrelease_step
           (p_batch_step_rec             => p_batch_step_rec
           ,p_update_inventory_ind       => p_batch_header_rec.update_inventory_ind
           ,p_create_resv_pend_lots      => p_create_resv_pend_lots
           ,p_from_unrelease_batch       => 0
           ,x_batch_step_rec             => x_batch_step_rec
           ,x_return_status              => x_return_status);

      IF x_return_status <> fnd_api.g_ret_sts_success THEN
         RAISE step_unrelease_failure;
      END IF;            /* IF x_return_status <> FND_API.G_RET_STS_SUCCESS */

      --IOT--Bug 21523465- New code added for IOT-YMM Changes. 
      --BEGIN BUG 24398647 comment YMM logic to move from main level api to private level api  
      --GME_YIELD_CALCULATION_PVT.TRUNC_TRANSFER_STEPS(p_batch_step_rec.batch_id, p_batch_step_rec.BATCHSTEP_ID);
      --BUG 24351209 invoking calculate yield api to calculate yield for step.
	    --GME_YIELD_CALCULATION_PVT.yield_calc(p_batch_id     => p_batch_step_rec.batch_id);
      
      --BUG 24351209  update recalculate yield flag to Y so that
      --comput yield concurrent program can capture this batch.
      /*OPEN check_batch_recalculate_flag(p_batch_step_rec.batch_id);
      FETCH check_batch_recalculate_flag INTO l_recalculate_count;
      IF l_recalculate_count > 0 THEN
        UPDATE gme_batch_genealogy
           SET recalculate_yield = 'Y'
         WHERE batch_id = p_batch_step_rec.batch_id;

      END IF;
      CLOSE check_batch_recalculate_flag; */
                  
      gme_common_pvt.log_message ('GME_BATCH_STEP_UNRELEASED');
     
      gme_common_pvt.count_and_get (x_count        => x_message_count
                                   ,p_encoded      => fnd_api.g_false
                                   ,x_data         => x_message_list);

      IF (g_debug IS NOT NULL) THEN
         gme_debug.put_line (   'Completed '
                             || l_api_name
                             || ' at '
                             || TO_CHAR (SYSDATE, 'MM/DD/YYYY HH24:MI:SS') );
      END IF;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;
   EXCEPTION
      WHEN step_unrelease_failure THEN
         ROLLBACK TO SAVEPOINT unrelease_step;
         x_batch_step_rec := NULL;
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
      WHEN setup_failure THEN
         ROLLBACK TO SAVEPOINT unrelease_step;
         x_batch_step_rec := NULL;
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
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

         ROLLBACK TO SAVEPOINT unrelease_step;
         x_batch_step_rec := NULL;
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
         x_return_status := fnd_api.g_ret_sts_unexp_error;
   END unrelease_step;

/*************************************************************************/
   PROCEDURE auto_detail_line (
      p_init_msg_list         IN              VARCHAR2 := fnd_api.g_false
     ,x_message_count         OUT NOCOPY      NUMBER
     ,x_message_list          OUT NOCOPY      VARCHAR2
     ,x_return_status         OUT NOCOPY      VARCHAR2
     ,p_material_detail_rec   IN              gme_material_details%ROWTYPE)
   IS
      l_api_name   CONSTANT VARCHAR2 (30) := 'AUTO_DETAIL_LINE';
      setup_failure         EXCEPTION;
      auto_detail_failure   EXCEPTION;
   BEGIN
      /* Set the save point initially */
      SAVEPOINT auto_detail_line;

      IF (g_debug IS NOT NULL) THEN
         gme_debug.log_initialize ('AutoDetailLine');
      END IF;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      IF NOT gme_common_pvt.g_setup_done THEN
         gme_common_pvt.g_setup_done :=
                 gme_common_pvt.setup (p_material_detail_rec.organization_id);

         IF NOT gme_common_pvt.g_setup_done THEN
            x_return_status := fnd_api.g_ret_sts_error;
            RAISE setup_failure;
         END IF;
      END IF;

      -- Initialize message list and count if needed
      IF p_init_msg_list = fnd_api.g_true THEN
         fnd_msg_pub.initialize;
         gme_common_pvt.g_error_count := 0;
      END IF;

      /* Set the return status to success initially */
      x_return_status := fnd_api.g_ret_sts_success;
      /* Set the timestamp  */
      gme_common_pvt.set_timestamp;
      gme_reservations_pvt.auto_detail_line
                             (p_material_details_rec      => p_material_detail_rec
                             ,x_return_status             => x_return_status);

      IF x_return_status <> fnd_api.g_ret_sts_success THEN
         RAISE auto_detail_failure;
      END IF;
   
       gme_common_pvt.log_message ('GME_BATCH_AUTO_DETAIL_LINE');
     
      IF (g_debug <= gme_debug.g_log_procedure) THEN
         gme_debug.put_line (   'Completed '
                             || l_api_name
                             || ' at '
                             || TO_CHAR (SYSDATE, 'MM/DD/YYYY HH24:MI:SS') );
      END IF;
   EXCEPTION
      WHEN auto_detail_failure OR setup_failure THEN
         ROLLBACK TO SAVEPOINT auto_detail_line;
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
      WHEN OTHERS THEN
         IF g_debug <= gme_debug.g_log_unexpected THEN
            gme_debug.put_line (   'When others exception in '
                                || g_pkg_name
                                || '.'
                                || l_api_name
                                || ' Error is '
                                || SQLERRM);
         END IF;

         ROLLBACK TO SAVEPOINT auto_detail_line;
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
         x_return_status := fnd_api.g_ret_sts_unexp_error;
   END auto_detail_line;
   /*************************************************************************/
   PROCEDURE auto_detail_batch(
      p_init_msg_list            IN              VARCHAR2 := FND_API.G_FALSE,
      x_message_count            OUT NOCOPY      NUMBER,
      x_message_list             OUT NOCOPY      VARCHAR2,
      x_return_status            OUT NOCOPY      VARCHAR2,
      p_batch_rec                IN              gme_batch_header%ROWTYPE) IS

      l_api_name        CONSTANT VARCHAR2 (30)   := 'AUTO_DETAIL_BATCH';
      

      setup_failure              EXCEPTION;
      auto_detail_failure        EXCEPTION;
   BEGIN
      /* Set the save point initially */
      SAVEPOINT auto_detail_batch;

      IF (g_debug IS NOT NULL) THEN
         gme_debug.log_initialize ('AutoDetailBatch');
      END IF;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.' || l_api_name);
      END IF;

      IF NOT gme_common_pvt.g_setup_done THEN
         gme_common_pvt.g_setup_done :=
                 gme_common_pvt.setup (p_batch_rec.organization_id);

         IF NOT gme_common_pvt.g_setup_done THEN
            x_return_status := fnd_api.g_ret_sts_error;
            RAISE setup_failure;
         END IF;
      END IF;

      -- Initialize message list and count if needed
      IF p_init_msg_list = fnd_api.g_true THEN
         fnd_msg_pub.initialize;
         gme_common_pvt.g_error_count := 0;
      END IF;

      /* Set the return status to success initially */
      x_return_status := fnd_api.g_ret_sts_success;

      /* Set the timestamp  */
      gme_common_pvt.set_timestamp;

      gme_reservations_pvt.auto_detail_batch(p_batch_rec => p_batch_rec             
                                            ,x_return_status => x_return_status);

      IF x_return_status <> fnd_api.g_ret_sts_success THEN
         RAISE auto_detail_failure;
      END IF;
      gme_common_pvt.log_message ('GME_BATCH_AUTO_DETAIL_BATCH');
      IF (g_debug <= gme_debug.g_log_procedure) THEN
         gme_debug.put_line (   'Completed '
                             || l_api_name
                             || ' at '
                             || TO_CHAR (SYSDATE, 'MM/DD/YYYY HH24:MI:SS') );
      END IF;
   EXCEPTION
      WHEN auto_detail_failure OR setup_failure THEN
         ROLLBACK TO SAVEPOINT auto_detail_batch;
         gme_common_pvt.count_and_get (x_count        => x_message_count,
                                       p_encoded      => fnd_api.g_false,
                                       x_data         => x_message_list);
      WHEN OTHERS THEN
         IF g_debug <= gme_debug.g_log_unexpected THEN
            gme_debug.put_line (   'When others exception in '
                                || g_pkg_name
                                || '.'
                                || l_api_name
                                || ' Error is '
                                || SQLERRM);
         END IF;

         ROLLBACK TO SAVEPOINT auto_detail_batch;
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
         gme_common_pvt.count_and_get (x_count        => x_message_count,
                                       p_encoded      => fnd_api.g_false,
                                       x_data         => x_message_list);
         x_return_status := fnd_api.g_ret_sts_unexp_error;
   END auto_detail_batch;
   
/*************************************************************************/
   PROCEDURE create_pending_product_lot (
      p_validation_level        IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list           IN              VARCHAR2 := fnd_api.g_false
     ,x_message_count           OUT NOCOPY      NUMBER
     ,x_message_list            OUT NOCOPY      VARCHAR2
     ,x_return_status           OUT NOCOPY      VARCHAR2
     ,p_org_id                  IN              NUMBER
     ,p_pending_product_lots_rec IN  gme_pending_product_lots%ROWTYPE
     ,x_pending_product_lots_rec OUT NOCOPY  gme_pending_product_lots%ROWTYPE)
   IS
      l_api_name       CONSTANT VARCHAR2 (30) := 'create_pending_product_lot';
      setup_failure             EXCEPTION;
      create_pp_lot_failure     EXCEPTION;
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      SAVEPOINT create_pp_lot;

      IF (g_debug IS NOT NULL) THEN
         gme_debug.log_initialize ('CreatePendingProdLot');
      END IF;

      IF NOT gme_common_pvt.g_setup_done THEN
         gme_common_pvt.g_setup_done :=
                    gme_common_pvt.setup (p_org_id);

         IF NOT gme_common_pvt.g_setup_done THEN
            x_return_status := fnd_api.g_ret_sts_error;
            RAISE setup_failure;
         END IF;
      END IF;

      /* Set the return status to success initially */
      x_return_status := fnd_api.g_ret_sts_success;

      -- Initialize message list and count if needed
      IF p_init_msg_list = fnd_api.g_true THEN
         fnd_msg_pub.initialize;
         gme_common_pvt.g_error_count := 0;
      END IF;

      gme_common_pvt.set_timestamp;
      gme_pending_product_lots_pvt.create_pending_product_lot
                          (p_pending_product_lots_rec      => p_pending_product_lots_rec
                          ,x_pending_product_lots_rec      => x_pending_product_lots_rec
                          ,x_return_status                 => x_return_status);

      IF x_return_status <> fnd_api.g_ret_sts_success THEN
         RAISE create_pp_lot_failure;
      END IF;            /* IF x_return_status <> FND_API.G_RET_STS_SUCCESS */

    
      gme_common_pvt.log_message ('GME_API_PP_LOT_CREATED');
   
      gme_common_pvt.count_and_get (x_count        => x_message_count
                                   ,p_encoded      => fnd_api.g_false
                                   ,x_data         => x_message_list);

      IF (g_debug IS NOT NULL) THEN
         gme_debug.put_line (   'Completed '
                             || l_api_name
                             || ' at '
                             || TO_CHAR (SYSDATE, 'MM/DD/YYYY HH24:MI:SS') );
      END IF;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;
   EXCEPTION
      WHEN create_pp_lot_failure THEN
         ROLLBACK TO SAVEPOINT create_pp_lot;
         x_pending_product_lots_rec := NULL;
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
      WHEN setup_failure THEN
         ROLLBACK TO SAVEPOINT create_pp_lot;
         x_pending_product_lots_rec := NULL;
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
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

         ROLLBACK TO SAVEPOINT create_pp_lot;
         x_pending_product_lots_rec := NULL;
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
         x_return_status := fnd_api.g_ret_sts_unexp_error;
   END create_pending_product_lot;

   PROCEDURE update_pending_product_lot (
      p_validation_level           IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list              IN              VARCHAR2 := fnd_api.g_false
     ,x_message_count              OUT NOCOPY      NUMBER
     ,x_message_list               OUT NOCOPY      VARCHAR2
     ,x_return_status              OUT NOCOPY      VARCHAR2
     ,p_org_id                     IN              NUMBER
     ,p_pending_product_lots_rec   IN  gme_pending_product_lots%ROWTYPE
     ,x_pending_product_lots_rec   IN  OUT NOCOPY  gme_pending_product_lots%ROWTYPE)
   IS
      l_api_name       CONSTANT VARCHAR2 (30) := 'update_pending_product_lot';
      setup_failure             EXCEPTION;
      update_pp_lot_failure     EXCEPTION;
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      SAVEPOINT update_pp_lot;

      IF (g_debug IS NOT NULL) THEN
         gme_debug.log_initialize ('UpdatePendingProdLot');
      END IF;

      IF NOT gme_common_pvt.g_setup_done THEN
         gme_common_pvt.g_setup_done :=
                    gme_common_pvt.setup (p_org_id);

         IF NOT gme_common_pvt.g_setup_done THEN
            x_return_status := fnd_api.g_ret_sts_error;
            RAISE setup_failure;
         END IF;
      END IF;

      /* Set the return status to success initially */
      x_return_status := fnd_api.g_ret_sts_success;

      -- Initialize message list and count if needed
      IF p_init_msg_list = fnd_api.g_true THEN
         fnd_msg_pub.initialize;
         gme_common_pvt.g_error_count := 0;
      END IF;

      gme_common_pvt.set_timestamp;
      gme_pending_product_lots_pvt.update_pending_product_lot
                          (p_pending_product_lots_rec      => p_pending_product_lots_rec
                          ,x_pending_product_lots_rec      => x_pending_product_lots_rec
                          ,x_return_status                 => x_return_status);

      IF x_return_status <> fnd_api.g_ret_sts_success THEN
         RAISE update_pp_lot_failure;
      END IF;            /* IF x_return_status <> FND_API.G_RET_STS_SUCCESS */

     
     gme_common_pvt.log_message ('GME_API_PP_LOT_UPDATED');
     

      gme_common_pvt.count_and_get (x_count        => x_message_count
                                   ,p_encoded      => fnd_api.g_false
                                   ,x_data         => x_message_list);

      IF (g_debug IS NOT NULL) THEN
         gme_debug.put_line (   'Completed '
                             || l_api_name
                             || ' at '
                             || TO_CHAR (SYSDATE, 'MM/DD/YYYY HH24:MI:SS') );
      END IF;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;
   EXCEPTION
      WHEN update_pp_lot_failure THEN
         ROLLBACK TO SAVEPOINT update_pp_lot;
         x_pending_product_lots_rec := NULL;
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
      WHEN setup_failure THEN
         ROLLBACK TO SAVEPOINT update_pp_lot;
         x_pending_product_lots_rec := NULL;
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
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

         ROLLBACK TO SAVEPOINT update_pp_lot;
         x_pending_product_lots_rec := NULL;
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
         x_return_status := fnd_api.g_ret_sts_unexp_error;
   END update_pending_product_lot;
   
   PROCEDURE delete_pending_product_lot (
      p_validation_level           IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list              IN              VARCHAR2 := fnd_api.g_false
     ,x_message_count              OUT NOCOPY      NUMBER
     ,x_message_list               OUT NOCOPY      VARCHAR2
     ,x_return_status              OUT NOCOPY      VARCHAR2
     ,p_org_id                     IN              NUMBER
     ,p_pending_product_lots_rec   IN  gme_pending_product_lots%ROWTYPE)
   IS
      l_api_name       CONSTANT VARCHAR2 (30) := 'delete_pending_product_lot';
      setup_failure             EXCEPTION;
      delete_pp_lot_failure     EXCEPTION;
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      SAVEPOINT delete_pp_lot;

      IF (g_debug IS NOT NULL) THEN
         gme_debug.log_initialize ('DeletePendingProdLot');
      END IF;

      IF NOT gme_common_pvt.g_setup_done THEN
         gme_common_pvt.g_setup_done :=
                    gme_common_pvt.setup (p_org_id);

         IF NOT gme_common_pvt.g_setup_done THEN
            x_return_status := fnd_api.g_ret_sts_error;
            RAISE setup_failure;
         END IF;
      END IF;

      /* Set the return status to success initially */
      x_return_status := fnd_api.g_ret_sts_success;

      -- Initialize message list and count if needed
      IF p_init_msg_list = fnd_api.g_true THEN
         fnd_msg_pub.initialize;
         gme_common_pvt.g_error_count := 0;
      END IF;

      gme_common_pvt.set_timestamp;
      gme_pending_product_lots_pvt.delete_pending_product_lot
                          (p_pending_product_lots_rec      => p_pending_product_lots_rec
                          ,x_return_status                 => x_return_status);

      IF x_return_status <> fnd_api.g_ret_sts_success THEN
         RAISE delete_pp_lot_failure;
      END IF;            /* IF x_return_status <> FND_API.G_RET_STS_SUCCESS */

         gme_common_pvt.log_message ('GME_API_PP_LOT_DELETED');
 

      gme_common_pvt.count_and_get (x_count        => x_message_count
                                   ,p_encoded      => fnd_api.g_false
                                   ,x_data         => x_message_list);

      IF (g_debug IS NOT NULL) THEN
         gme_debug.put_line (   'Completed '
                             || l_api_name
                             || ' at '
                             || TO_CHAR (SYSDATE, 'MM/DD/YYYY HH24:MI:SS') );
      END IF;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;
   EXCEPTION
      WHEN delete_pp_lot_failure THEN
         ROLLBACK TO SAVEPOINT delete_pp_lot;
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
      WHEN setup_failure THEN
         ROLLBACK TO SAVEPOINT delete_pp_lot;
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
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

         ROLLBACK TO SAVEPOINT delete_pp_lot;
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
         x_return_status := fnd_api.g_ret_sts_unexp_error;
   END delete_pending_product_lot;
   

 ------------------------------------------------------------------
   --GME BATCH STEP QUANTITY API
  -------------------------------------------------------------------

    /*###############################################################
  # name
  # insert_batch_step_quantity
  # synopsis
  # proc  insert_batch_step_quantity
  # description
  #     to insert GME Batch Step Quantity for step transfer details.
  # created by srmacha
  ###############################################################*/
  PROCEDURE insert_batch_step_quantity(
      p_batch_id                IN NUMBER,
      p_batch_step_quantity_tbl IN gme_api_pub.batch_step_quantity_tbl,
      x_return_status    IN OUT NOCOPY VARCHAR2,
      x_msg_count        IN OUT NOCOPY NUMBER,
      x_msg_data         IN OUT NOCOPY VARCHAR2,
      p_commit           IN OUT NOCOPY VARCHAR2)
  IS
---------User defined exceptions-----------------------------------------------
  e_invalid_transaction_date EXCEPTION;
  e_same_step_record EXCEPTION;
  e_batch_status_invalid EXCEPTION;
  e_batch_from_quantity_invalid EXCEPTION;
  e_batch_id_not_found EXCEPTION;
  e_from_step_id_not_found EXCEPTION;
  e_to_step_id_not_found EXCEPTION;
  e_uom_conv_fail EXCEPTION;

---------End user defined exceptions------------------------------------------
----Local Variables
  l_valid_date BOOLEAN :=FALSE;
  l_transaction_date DATE;
  l_batch_id NUMBER;
  l_batch_step_id NUMBER;
  l_quantity_out NUMBER;
  l_quantity_in NUMBER;
  l_from_uom varchar2(4);
  l_to_uom varchar2(4);
  l_to_batch_step_id NUMBER;
  l_batch_status NUMBER;
  l_batch_step_status NUMBER;
  l_last_update_login NUMBER;
  l_actual_step_start_date DATE;
  l_api_name   CONSTANT VARCHAR2 (30)   := 'insert_batch_step_quantity';
  l_batch_step_trnsfr_qty_rec gme_yield_calculation_pvt.batch_step_trnsfr_quantity_rec;
  l_quantity_in_conv NUMBER;
  l_frm_uom_cls varchar2(10);
  l_to_uom_cls varchar2(10);
  l_mass_class varchar2(100);
  l_vol_class varchar2(100);
  l_org_id NUMBER;
  BEGIN
  l_batch_id := p_batch_id;
  ------------------------------------------------------------------------
    --To check  input records available or not
  -------------------------------------------------------------------------
    IF p_batch_step_quantity_tbl.COUNT > 0
    THEN

    FOR record_val IN p_batch_step_quantity_tbl.FIRST .. p_batch_step_quantity_tbl.LAST
    LOOP
     Begin

    l_batch_step_id:= p_batch_step_quantity_tbl(record_val).from_step_id;
    l_transaction_date:= p_batch_step_quantity_tbl(record_val).transaction_date;
    l_to_batch_step_id:= p_batch_step_quantity_tbl(record_val).to_step_id;
    l_quantity_out := p_batch_step_quantity_tbl(record_val).step_qty_in_from_uom;
    --if no conversion required default value for qnty in
    l_quantity_in := p_batch_step_quantity_tbl(record_val).step_qty_in_to_uom;
--------batch id null validation
        IF l_batch_id IS NULL THEN
        RAISE e_batch_id_not_found;
        END IF;
-------from batch step id null validation
        IF l_batch_step_id IS NULL THEN
        RAISE e_from_step_id_not_found;
        END IF;

--------to batch step id null validation
        IF l_to_batch_step_id IS NULL THEN
        RAISE e_to_step_id_not_found;
        END IF;
------identifying batch status ,batch step status and applying validation
    select batch_status into l_batch_status from GME_BATCH_HEADER where batch_id=l_batch_id;

    IF l_batch_status NOT IN (2,3) THEN
    RAISE e_batch_status_invalid;
    END IF;

    select step_status into l_batch_step_status from GME_BATCH_STEPS where batch_id=l_batch_id AND batchstep_id=l_batch_step_id;

    IF l_batch_step_status NOT IN (2,3) THEN
    RAISE e_batch_status_invalid;
    END IF;
-------------------------------------------------------------------------------

----------Validation to check for ,to step and from step id or equal-------
    IF l_batch_step_id=l_to_batch_step_id THEN
    RAISE e_same_step_record;
    END IF;
----------END validation-----------------------------------------------------

--------------------------------------------------------------------------------
--    Transaction date validation
--      If transaction date is null or not passed then update with current date
-------------------------------------------------------------------------------
    IF l_transaction_date IS NULL THEN
    l_transaction_date := SYSDATE;
    END IF;
    l_valid_date  := GME_YIELD_CALCULATION_PVT.validate_trans_date(l_transaction_date ,l_batch_id ,l_batch_step_id,l_batch_step_status);
    IF NOT l_valid_date THEN
    RAISE e_invalid_transaction_date;
    END IF;
-----------End Transaction validation------------------------------------------

--------------------------------------------------------------------------------
--retrieving to ,from step UOM if both are not same convering into to step UOM
--and calculating quantity IN
--------------------------------------------------------------------------------
    IF l_quantity_out IS NULL or l_quantity_out < 0 THEN
    RAISE e_batch_from_quantity_invalid;
    END IF;

     select step_qty_um into l_from_uom from gme_batch_steps where batch_id=l_batch_id and batchstep_id=l_batch_step_id;

     select step_qty_um into l_to_uom from gme_batch_steps where batch_id=l_batch_id and batchstep_id=l_to_batch_step_id;
      
      --getting the from,to uom classes.
      SELECT  UPPER (UOM_CLASS) INTO l_frm_uom_cls FROM MTL_UNITS_OF_MEASURE
          WHERE UOM_CODE = l_from_uom;
      SELECT  UPPER (UOM_CLASS) INTO l_to_uom_cls FROM MTL_UNITS_OF_MEASURE
          WHERE UOM_CODE = l_to_uom;
          
      --if both UOM classes are not same inter class
      IF l_frm_uom_cls <> l_to_uom_cls THEN 
        l_quantity_in_conv := GME_YIELD_CALCULATION_PVT.batchsteps_qty_conversion(l_quantity_out,l_from_uom,l_to_uom,l_batch_id);
        --if any error in conversion and user not passed to_qty then raise error
        IF l_quantity_in_conv = -99999 AND l_quantity_in IS NULL THEN 
        RAISE e_uom_conv_fail;
        ELSIF l_quantity_in_conv <> -99999 THEN
        l_quantity_in :=l_quantity_in_conv;
        END IF;
        -- If both UOM classes are same intra classes
      ELSIF l_frm_uom_cls = l_to_uom_cls  THEN
      --This is to identify MASS,VOLUME  type UOMs.                 
          select organization_id into l_org_id from gme_batch_header where batch_id=l_batch_id;
    
           SELECT gpd.parameter_value INTO l_mass_class FROM GMD_PARAMETERS_HDR gph,GMD_PARAMETERS_DTL gpd
           WHERE gph.organization_id =l_org_id AND gph.parameter_id = gpd.parameter_id 
           AND gpd.parameter_name = 'GMD_MASS_UM_TYPE';
           
           SELECT gpd.parameter_value INTO l_vol_class FROM GMD_PARAMETERS_HDR gph,
           GMD_PARAMETERS_DTL gpd WHERE gph.organization_id = l_org_id 
           AND gph.parameter_id = gpd.parameter_id AND gpd.parameter_name = 'GMD_VOLUME_UM_TYPE';
           --convert qty for MASS and VOLUME
           IF l_frm_uom_cls = l_mass_class OR l_frm_uom_cls = l_vol_class THEN
            l_quantity_in_conv := GME_YIELD_CALCULATION_PVT.batchsteps_qty_conversion(l_quantity_out,l_from_uom,l_to_uom,l_batch_id);
            --if any error in conversion and user not passed to_qty then raise error
            IF l_quantity_in_conv = -99999 AND l_quantity_in IS NULL THEN 
            RAISE e_uom_conv_fail;
            ELSIF l_quantity_in_conv <> -99999 THEN
            l_quantity_in :=l_quantity_in_conv;
            END IF;
          --if qty is not MASS , VOLUME no conversion assign user passed  to_qty ,if it is null then raise exception
          ELSIF l_quantity_in IS NULL THEN
          RAISE e_uom_conv_fail; 
          END IF;                
       END IF;
        
------------------------------------------------------------------------------

          l_batch_step_trnsfr_qty_rec.batch_id :=l_batch_id ;
          l_batch_step_trnsfr_qty_rec.from_step_id:=l_batch_step_id;
    			l_batch_step_trnsfr_qty_rec.to_step_id:=l_to_batch_step_id;
    			l_batch_step_trnsfr_qty_rec.step_qty_in_from_uom:=l_quantity_out;
          l_batch_step_trnsfr_qty_rec.step_qty_in_to_uom:=l_quantity_in ;
    			l_batch_step_trnsfr_qty_rec.transaction_date:=l_transaction_date;
 --calling insert row to gme batch step quantity through gme_yield_calculation_pvt
      gme_yield_calculation_pvt.insert_batch_step_quantity(
                p_batch_step_trnsfr_qty_rec=>l_batch_step_trnsfr_qty_rec);

    Exception
    WHEN e_invalid_transaction_date THEN
      x_return_status   :=  G_STATUS_WARNING;
      gme_common_pvt.log_message ('GME_TRANSACTION_DATE_INVALID','BATCHID', l_batch_id,'FRMSTPID', l_batch_step_id,'TOSTPID', l_to_batch_step_id);
      gme_common_pvt.count_and_get (x_count        => x_msg_count
                                  ,p_encoded      => fnd_api.g_false
                                  ,x_data         => x_msg_data);
    WHEN e_same_step_record THEN
      x_return_status   :=  G_STATUS_WARNING;
      gme_common_pvt.log_message ('GME_SAME_STEP_ID_NOT_ALLOWED','BATCHID', l_batch_id,'FRMSTPID', l_batch_step_id);
      gme_common_pvt.count_and_get (x_count        => x_msg_count
                                  ,p_encoded      => fnd_api.g_false
                                  ,x_data         => x_msg_data);
     WHEN e_batch_status_invalid THEN
      x_return_status   :=  G_STATUS_WARNING;
      gme_common_pvt.log_message ('GME_BATCH_STATUS_INVALID','BATCHID', l_batch_id,'FRMSTPID', l_batch_step_id);
      gme_common_pvt.count_and_get (x_count        => x_msg_count
                                  ,p_encoded      => fnd_api.g_false
                                  ,x_data         => x_msg_data);
      WHEN e_batch_from_quantity_invalid THEN
      x_return_status   :=  G_STATUS_WARNING;
      gme_common_pvt.log_message ('GME_BATCH_FROM_QTY_INVALID','BATCHID', l_batch_id,'FRMSTPID', l_batch_step_id,'TOSTPID', l_to_batch_step_id);
      gme_common_pvt.count_and_get (x_count        => x_msg_count
                                  ,p_encoded      => fnd_api.g_false
                                  ,x_data         => x_msg_data);
       WHEN e_batch_id_not_found THEN
       x_return_status   :=  G_STATUS_WARNING;
       gme_common_pvt.log_message ('GME_BATCH_ID_NOT_FOUND');
       gme_common_pvt.count_and_get (x_count        => x_msg_count
                                  ,p_encoded      => fnd_api.g_false
                                  ,x_data         => x_msg_data);
        WHEN e_from_step_id_not_found THEN
          x_return_status   :=  G_STATUS_WARNING;
          gme_common_pvt.log_message ('GME_BATCH_FRM_STP_ID_NOT_FOUND','BATCHID', l_batch_id);
          gme_common_pvt.count_and_get (x_count        => x_msg_count
                                  ,p_encoded      => fnd_api.g_false
                                  ,x_data         => x_msg_data);
         WHEN e_to_step_id_not_found THEN
          x_return_status   :=  G_STATUS_WARNING;
          gme_common_pvt.log_message ('GME_BATCH_TO_STEP_ID_NOT_FOUND','BATCHID', l_batch_id,'FRMSTPID', l_batch_step_id);
          gme_common_pvt.count_and_get (x_count        => x_msg_count
                                  ,p_encoded      => fnd_api.g_false
                                  ,x_data         => x_msg_data);
         WHEN e_uom_conv_fail THEN
          x_return_status   :=  G_STATUS_WARNING;
          gme_common_pvt.log_message ('GME_BATCH_STEP_UOM_FAIL','BATCHID', l_batch_id,'FRMSTPID', l_batch_step_id,'TOSTPID', l_to_batch_step_id);
          gme_common_pvt.count_and_get (x_count        => x_msg_count
                                  ,p_encoded      => fnd_api.g_false
                                  ,x_data         => x_msg_data);
    END;
    END LOOP;
   -------------------------------------------------------------------------
   --      yield calculation
   -------------------------------------------------------------------------
    gme_yield_calculation_pvt.compute_yield(l_batch_id);

   END IF;
   
 Exception
    WHEN OTHERS THEN
       p_commit :=fnd_api.g_false;
       x_return_status   := fnd_api.g_ret_sts_unexp_error;
       fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
       gme_common_pvt.count_and_get (x_count        => x_msg_count
                                  ,p_encoded      => fnd_api.g_false
                                  ,x_data         => x_msg_data);

  END insert_batch_step_quantity;


     /*###############################################################
  # name
  # update_batch_step_quantity
  # synopsis
  # proc  update_batch_step_quantity
  # description
  #     to update GME Batch Step Quantity for step transfer details.
  # created by
  ###############################################################*/
   PROCEDURE update_batch_step_quantity(
      p_batch_id                IN NUMBER,
      p_batch_step_quantity_tbl IN gme_api_pub.batch_step_quantity_tbl,
      x_return_status    IN OUT NOCOPY VARCHAR2,
      x_msg_count        IN OUT NOCOPY NUMBER,
      x_msg_data         IN OUT NOCOPY VARCHAR2,
      p_commit           IN OUT NOCOPY VARCHAR2) IS

-------User defined exceptions----------------------------------------------
      e_invalid_transaction_date EXCEPTION;
      e_same_step_record EXCEPTION;
      e_batch_status_invalid EXCEPTION;
      e_batch_from_quantity_invalid EXCEPTION;
      e_batch_id_not_found EXCEPTION;
      e_from_step_id_not_found EXCEPTION;
      e_to_step_id_not_found EXCEPTION;
      e_uom_conv_fail EXCEPTION;

------End user defined exceptions--------------------------------------------

-----------local variables
      l_valid_date BOOLEAN :=FALSE;
      l_transaction_date DATE;
      l_batch_id NUMBER;
      l_batch_step_id NUMBER;
      l_quantity_out NUMBER;
      l_quantity_in NUMBER;
      l_from_uom varchar2(4);
      l_to_uom varchar2(4);
      l_to_batch_step_id NUMBER;
      l_same_step_id_flag boolean := FALSE;
      l_batch_status NUMBER;
      l_batch_step_status NUMBER;
      l_actual_step_start_date DATE;
      l_api_name   CONSTANT VARCHAR2 (30)   := 'update_batch_step_quantity';
      l_quantity_in_conv NUMBER;
      l_frm_uom_cls varchar2(10);
      l_to_uom_cls varchar2(10);
      l_mass_class varchar2(100);
      l_vol_class varchar2(100);
      l_org_id NUMBER;
      l_batch_step_trnsfr_qty_rec gme_yield_calculation_pvt.batch_step_trnsfr_quantity_rec;

  BEGIN
    l_batch_id        := p_batch_id;

     IF p_batch_step_quantity_tbl.COUNT > 0 
    THEN
    FOR record_indx IN p_batch_step_quantity_tbl.FIRST .. p_batch_step_quantity_tbl.LAST
    LOOP

    Begin

    l_batch_step_id   := p_batch_step_quantity_tbl(record_indx).from_step_id;
    l_transaction_date:= p_batch_step_quantity_tbl(record_indx).transaction_date;
    l_to_batch_step_id:= p_batch_step_quantity_tbl(record_indx).to_step_id;
    l_quantity_out    := p_batch_step_quantity_tbl(record_indx).step_qty_in_from_uom;
    l_quantity_in     := p_batch_step_quantity_tbl(record_indx).step_qty_in_to_uom;
--------batch id null validation
        IF l_batch_id IS NULL THEN
        RAISE e_batch_id_not_found;
        END IF;
-------from batch step id null validation
        IF l_batch_step_id IS NULL THEN
        RAISE e_from_step_id_not_found;
        END IF;

--------to batch step id null validation
        IF l_to_batch_step_id IS NULL THEN
        RAISE e_to_step_id_not_found;
        END IF;
------------- identifying batch status ,batch step status and applying validation-----------------
    select batch_status into l_batch_status from GME_BATCH_HEADER where batch_id=l_batch_id;
    IF l_batch_status NOT IN (2,3)  THEN
    RAISE e_batch_status_invalid;
    END IF;

    select step_status into l_batch_step_status from GME_BATCH_STEPS where batch_id=l_batch_id AND
    batchstep_id=l_batch_step_id;

    IF l_batch_step_status NOT IN (2,3) THEN
    RAISE e_batch_status_invalid;
    END IF;
-------------------------------------------------------------------------------

-----validation is to check whether to and from batch step id's are same
    IF l_batch_step_id=l_to_batch_step_id THEN
    RAISE e_same_step_record;
    END IF;
-------------------------------------------------------------------------------

--      Transaction date validation
--      If transaction date is null or not passed then update with current date
-------------------------------------------------------------------------------
    IF l_transaction_date IS NULL THEN
    l_transaction_date := SYSDATE;
    END IF;
     l_valid_date  := GME_YIELD_CALCULATION_PVT.validate_trans_date(l_transaction_date ,l_batch_id ,l_batch_step_id,l_batch_step_status );
     IF NOT l_valid_date THEN
     RAISE e_invalid_transaction_date;
     END IF;
-----End Transaction validation------------------------------------------------

--------------------------------------------------------------------------------
--retrieving to ,from step UOM if both are not same convering into to step UOM
--and calculating quantity IN
-------------------------------------------------------------------------------

    IF l_quantity_out IS NULL or l_quantity_out < 0 THEN
    RAISE e_batch_from_quantity_invalid;
    END IF;

  select step_qty_um into l_from_uom from gme_batch_steps where batch_id=l_batch_id and batchstep_id=l_batch_step_id;

  select step_qty_um into l_to_uom from gme_batch_steps where batch_id=l_batch_id and batchstep_id=l_to_batch_step_id;

      --getting the from,to uom classes.
      SELECT  UPPER (UOM_CLASS) INTO l_frm_uom_cls FROM MTL_UNITS_OF_MEASURE
      WHERE UOM_CODE = l_from_uom;
      SELECT  UPPER (UOM_CLASS) INTO l_to_uom_cls FROM MTL_UNITS_OF_MEASURE
      WHERE UOM_CODE = l_to_uom;
          
      --if both UOM classes are not same inter class
      IF l_frm_uom_cls <> l_to_uom_cls THEN 
        l_quantity_in_conv := GME_YIELD_CALCULATION_PVT.batchsteps_qty_conversion(l_quantity_out,l_from_uom,l_to_uom,l_batch_id);
        --if any error in conversion and user not passed to_qty then raise error
        IF l_quantity_in_conv = -99999 AND l_quantity_in IS NULL THEN 
        RAISE e_uom_conv_fail;
        ELSIF l_quantity_in_conv <> -99999 THEN
        l_quantity_in :=l_quantity_in_conv;
        END IF;
        -- If both UOM classes are same intra classes
      ELSIF l_frm_uom_cls = l_to_uom_cls  THEN
      --This is to identify MASS,VOLUME  type UOMs.                 
          select organization_id into l_org_id from gme_batch_header where batch_id=l_batch_id;
    
           SELECT gpd.parameter_value INTO l_mass_class FROM GMD_PARAMETERS_HDR gph,GMD_PARAMETERS_DTL gpd
           WHERE gph.organization_id =l_org_id AND gph.parameter_id = gpd.parameter_id
           AND gpd.parameter_name = 'GMD_MASS_UM_TYPE';
           
           SELECT gpd.parameter_value INTO l_vol_class FROM GMD_PARAMETERS_HDR gph,
           GMD_PARAMETERS_DTL gpd WHERE gph.organization_id = l_org_id
           AND gph.parameter_id = gpd.parameter_id AND gpd.parameter_name = 'GMD_VOLUME_UM_TYPE';
           --convert qty for MASS , VOLUME
           IF l_frm_uom_cls = l_mass_class OR l_frm_uom_cls = l_vol_class THEN
            l_quantity_in_conv := GME_YIELD_CALCULATION_PVT.batchsteps_qty_conversion(l_quantity_out,l_from_uom,l_to_uom,l_batch_id);
            --if any error in conversion and user not passed to_qty then raise error
            IF l_quantity_in_conv = -99999 AND l_quantity_in IS NULL THEN 
            RAISE e_uom_conv_fail;
            ELSIF l_quantity_in_conv <> -99999 THEN
            l_quantity_in :=l_quantity_in_conv;
            END IF;
          --if qty is not MASS , VOLUME no conversion assign user passed  to_qty ,if it is null then raise exception
          ELSIF l_quantity_in IS NULL THEN
          RAISE e_uom_conv_fail; 
          END IF;
                 
      END IF;
 -----------------------------------------------------------------------
      
      
       l_batch_step_trnsfr_qty_rec.batch_id :=l_batch_id ;
       l_batch_step_trnsfr_qty_rec.from_step_id:=l_batch_step_id;
       l_batch_step_trnsfr_qty_rec.to_step_id:=l_to_batch_step_id;
       l_batch_step_trnsfr_qty_rec.step_qty_in_from_uom:=l_quantity_out;
       l_batch_step_trnsfr_qty_rec.step_qty_in_to_uom:=l_quantity_in ;
       l_batch_step_trnsfr_qty_rec.transaction_date:=l_transaction_date;
 --calling update row to gme batch step quantity through gme_yield_calculation_pvt
      gme_yield_calculation_pvt.update_batch_step_quantity(
                p_batch_step_trnsfr_qty_rec=>l_batch_step_trnsfr_qty_rec);

     Exception
        WHEN e_invalid_transaction_date THEN
          x_return_status   :=  G_STATUS_WARNING;      
          gme_common_pvt.log_message ('GME_TRANSACTION_DATE_INVALID','BATCHID', l_batch_id,'FRMSTPID', l_batch_step_id,'TOSTPID', l_to_batch_step_id);
          gme_common_pvt.count_and_get (x_count        => x_msg_count
                                  ,p_encoded      => fnd_api.g_false
                                  ,x_data         => x_msg_data);

        WHEN e_same_step_record THEN
          x_return_status   :=  G_STATUS_WARNING;
          gme_common_pvt.log_message ('GME_SAME_STEP_ID_NOT_ALLOWED','BATCHID', l_batch_id,'FRMSTPID', l_batch_step_id);
          gme_common_pvt.count_and_get (x_count        => x_msg_count
                                  ,p_encoded      => fnd_api.g_false
                                  ,x_data         => x_msg_data);
        WHEN e_batch_status_invalid THEN
          x_return_status   :=  G_STATUS_WARNING;
          gme_common_pvt.log_message ('GME_BATCH_STATUS_INVALID','BATCHID', l_batch_id,'FRMSTPID', l_batch_step_id);
          gme_common_pvt.count_and_get (x_count        => x_msg_count
                                  ,p_encoded      => fnd_api.g_false
                                  ,x_data         => x_msg_data);
        WHEN e_batch_from_quantity_invalid THEN
          x_return_status   :=  G_STATUS_WARNING;
          gme_common_pvt.log_message ('GME_BATCH_FROM_QTY_INVALID','BATCHID', l_batch_id,'FRMSTPID', l_batch_step_id,'TOSTPID', l_to_batch_step_id);
          gme_common_pvt.count_and_get (x_count        => x_msg_count
                                  ,p_encoded      => fnd_api.g_false
                                  ,x_data         => x_msg_data);
        WHEN e_batch_id_not_found THEN
          x_return_status   :=  G_STATUS_WARNING;
          gme_common_pvt.log_message ('GME_BATCH_ID_NOT_FOUND');
          gme_common_pvt.count_and_get (x_count        => x_msg_count
                                  ,p_encoded      => fnd_api.g_false
                                  ,x_data         => x_msg_data);
        WHEN e_from_step_id_not_found THEN
          x_return_status   :=  G_STATUS_WARNING;
          gme_common_pvt.log_message ('GME_BATCH_FRM_STP_ID_NOT_FOUND','BATCHID', l_batch_id);
          gme_common_pvt.count_and_get (x_count        => x_msg_count
                                  ,p_encoded      => fnd_api.g_false
                                  ,x_data         => x_msg_data);
         WHEN e_to_step_id_not_found THEN
          x_return_status   :=  G_STATUS_WARNING;
          gme_common_pvt.log_message ('GME_BATCH_TO_STEP_ID_NOT_FOUND','BATCHID', l_batch_id,'FRMSTPID', l_batch_step_id);
          gme_common_pvt.count_and_get (x_count        => x_msg_count
                                  ,p_encoded      => fnd_api.g_false
                                  ,x_data         => x_msg_data);
        WHEN e_uom_conv_fail THEN
          x_return_status   :=  G_STATUS_WARNING;
          gme_common_pvt.log_message ('GME_BATCH_STEP_UOM_FAIL','BATCHID', l_batch_id,'FRMSTPID', l_batch_step_id,'TOSTPID', l_to_batch_step_id);
          gme_common_pvt.count_and_get (x_count        => x_msg_count
                                  ,p_encoded      => fnd_api.g_false
                                  ,x_data         => x_msg_data);
      END;
    END LOOP;
    -------------------------------------------------------------------------
   --      yield calculation
   -------------------------------------------------------------------------
   gme_yield_calculation_pvt.compute_yield(l_batch_id);
   END IF;

   Exception
    WHEN OTHERS THEN
       p_commit :=fnd_api.g_false;
       x_return_status   := fnd_api.g_ret_sts_unexp_error;
       fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
       gme_common_pvt.count_and_get (x_count        => x_msg_count
                                  ,p_encoded      => fnd_api.g_false
                                  ,x_data         => x_msg_data);

  END update_batch_step_quantity;
    /*###############################################################
  # name
  # delete_batch_step_quantity
  # synopsis
  # procedure  delete_batch_step_quantity
  # description
  #     to delete GME Batch Step Quantity
  # created by
  ###############################################################*/
  PROCEDURE delete_batch_step_quantity(
      p_batch_id                IN NUMBER,
      p_batch_step_quantity_tbl IN gme_api_pub.batch_step_quantity_tbl,
      x_return_status    IN OUT NOCOPY VARCHAR2,
      x_msg_count        IN OUT NOCOPY NUMBER,
      x_msg_data         IN OUT NOCOPY VARCHAR2,
      p_commit           IN OUT NOCOPY VARCHAR2) IS

--------------User defined exceptions--------------
      e_batch_id_not_found EXCEPTION;
      e_from_step_id_not_found EXCEPTION;
      e_to_step_id_not_found EXCEPTION;
      e_batch_status_invalid EXCEPTION;
--------local variables
      l_batch_id NUMBER;
      l_from_batchstep_id NUMBER;
      l_to_batchstep_id NUMBER;
      l_batch_step_status NUMBER;
      l_batch_status NUMBER;
      l_api_name   CONSTANT VARCHAR2 (30)   := 'delete_batch_step_quantity';
      l_batch_step_trnsfr_qty_rec gme_yield_calculation_pvt.batch_step_trnsfr_quantity_rec;
      
  BEGIN
    l_batch_id        := p_batch_id;

    IF p_batch_step_quantity_tbl.COUNT > 0 
    THEN
    FOR record_indx IN p_batch_step_quantity_tbl.FIRST .. p_batch_step_quantity_tbl.LAST
    LOOP
    Begin
        l_from_batchstep_id :=p_batch_step_quantity_tbl(record_indx).from_step_id;
        l_to_batchstep_id :=p_batch_step_quantity_tbl(record_indx).to_step_id;

--------batch id null validation
        IF l_batch_id IS NULL THEN
        RAISE e_batch_id_not_found;
        END IF;
-------from batch step id null validation
        IF l_from_batchstep_id IS NULL THEN
        RAISE e_from_step_id_not_found;
        END IF;

--------to batch step id null validation
        IF l_to_batchstep_id IS NULL THEN
        RAISE e_to_step_id_not_found;
        END IF;

--------identifying batch status ,batch step status and applying validation-----------------
    select batch_status into l_batch_status from GME_BATCH_HEADER where batch_id=l_batch_id;
    IF l_batch_status NOT IN (2,3)  THEN
    RAISE e_batch_status_invalid;
    END IF;

    select step_status into l_batch_step_status from GME_BATCH_STEPS where batch_id=l_batch_id AND
    batchstep_id=l_from_batchstep_id;

    IF l_batch_step_status NOT IN (2,3) THEN
    RAISE e_batch_status_invalid;
    END IF;
-------------------------------------------------------------------------------
------------------------------------------------------------------------------
       l_batch_step_trnsfr_qty_rec.batch_id :=l_batch_id ;
       l_batch_step_trnsfr_qty_rec.from_step_id:=l_from_batchstep_id;
       l_batch_step_trnsfr_qty_rec.to_step_id:=l_to_batchstep_id;
    --calling delete row to gme batch step quantity through gme_yield_calculation_pvt
      gme_yield_calculation_pvt.delete_batch_step_quantity(
                p_batch_step_trnsfr_qty_rec=>l_batch_step_trnsfr_qty_rec);

      Exception
        WHEN e_batch_id_not_found THEN
          x_return_status   :=  G_STATUS_WARNING;
          gme_common_pvt.log_message ('GME_BATCH_ID_NOT_FOUND');
          gme_common_pvt.count_and_get (x_count        => x_msg_count
                                  ,p_encoded      => fnd_api.g_false
                                  ,x_data         => x_msg_data);
        WHEN e_from_step_id_not_found THEN
          x_return_status   :=  G_STATUS_WARNING;
          gme_common_pvt.log_message ('GME_BATCH_FRM_STP_ID_NOT_FOUND','BATCHID', l_batch_id);
          gme_common_pvt.count_and_get (x_count        => x_msg_count
                                  ,p_encoded      => fnd_api.g_false
                                  ,x_data         => x_msg_data);
         WHEN e_to_step_id_not_found THEN
          x_return_status   :=  G_STATUS_WARNING;
          gme_common_pvt.log_message ('GME_BATCH_TO_STEP_ID_NOT_FOUND','BATCHID', l_batch_id,'FRMSTPID', l_from_batchstep_id);
          gme_common_pvt.count_and_get (x_count        => x_msg_count
                                  ,p_encoded      => fnd_api.g_false
                                  ,x_data         => x_msg_data);
      END;
    END LOOP;
   ----------------------------------------------------------------------------
   --      yield calculation
   -------------------------------------------------------------------------
   gme_yield_calculation_pvt.compute_yield(l_batch_id);
    END IF;
    
 Exception
    WHEN OTHERS THEN
       p_commit :=fnd_api.g_false;
       x_return_status   := fnd_api.g_ret_sts_unexp_error;
       fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
       gme_common_pvt.count_and_get (x_count        => x_msg_count
                                  ,p_encoded      => fnd_api.g_false
                                  ,x_data         => x_msg_data);

  END delete_batch_step_quantity;

   /*************************************************************************/
   PROCEDURE ASSOCIATE_BATCHSTEP_ITEM (
      p_api_version              IN              NUMBER
     ,p_commit                   IN              VARCHAR2 := fnd_api.g_false
     ,p_org_code                 IN              VARCHAR2 := NULL
	 ,p_batch_no                 IN              VARCHAR2 := NULL
	 ,p_batchstep_no             IN              NUMBER := NULL
	 ,p_line_type             	 IN              NUMBER := NULL
	 ,p_line_no              	 IN              NUMBER
	 ,x_message_count            OUT NOCOPY      NUMBER
     ,x_message_list             OUT NOCOPY      VARCHAR2
     ,x_return_status            OUT NOCOPY      VARCHAR2
     )
   IS
      l_api_name       			CONSTANT VARCHAR2 (30)               := 'ASSOCIATE_BATCHSTEP_ITEM';
      l_return_status           VARCHAR2 (1);


	  l_organization_id			NUMBER;
	  l_batch_id				NUMBER;
	  l_batch_status			NUMBER;
	  l_batchstep_id			NUMBER;
	  l_mtrl_dtl_id				NUMBER;
	  l_actual_qty				NUMBER;
	  l_batch_step_items   		gme_batch_step_items%ROWTYPE;


	  x_eres_message_count      NUMBER;
      x_eres_message_list       VARCHAR2(2000);
      x_eres_return_status      VARCHAR2(10);

      
	  CURSOR cur_validate_batch_no (v_batch_no VARCHAR2, v_org_id NUMBER)
      IS
         select batch_id, batch_status
			 from gme_batch_header
			 where batch_no = v_batch_no
			 and organization_id = v_org_id;

	  CURSOR cur_validate_batchstep_no (v_batchstep_no NUMBER, v_batch_id NUMBER)
      IS
		select batchstep_id from gme_batch_steps
			where batch_id = v_batch_id
			and batchstep_no = v_batchstep_no;

      CURSOR cur_material_dtl (v_org_id NUMBER, v_batch_id NUMBER, v_line_type NUMBER, v_line_no NUMBER)
      IS
         select material_detail_id, actual_qty from gme_material_details
			WHERE batch_id = v_batch_id
			and line_type = v_line_type
			and line_no = v_line_no
			and organization_id = v_org_id;



   BEGIN

      /* Set the return status to success initially */
      x_return_status := fnd_api.g_ret_sts_success;

	  IF NOT gme_common_pvt.g_setup_done THEN
         RAISE fnd_api.g_exc_error;
      ELSE
         l_organization_id := gme_common_pvt.g_organization_id;
      END IF;
	  
      IF (p_org_code IS NULL) THEN
         fnd_message.set_name ('INV', 'INV_ORG_REQUIRED');
         fnd_msg_pub.ADD;
         RAISE fnd_api.g_exc_error;
      END IF;


	  OPEN cur_validate_batch_no
                               (p_batch_no, l_organization_id);

           FETCH cur_validate_batch_no
            INTO l_batch_id, l_batch_status;

           IF cur_validate_batch_no%NOTFOUND THEN
              gme_common_pvt.log_message
                              ('GME_BATCH_NOT_FOUND');		--TODO
              RAISE fnd_api.g_exc_error;
           END IF;

		   IF l_batch_status = 4 THEN
		      gme_common_pvt.log_message
                              ('GME_BSI_UPDT_NOTALWD_BSTS');		--TODO
              RAISE fnd_api.g_exc_error;
           END IF;

      CLOSE cur_validate_batch_no;


	  OPEN cur_validate_batchstep_no
                               (p_batchstep_no, l_batch_id);

           FETCH cur_validate_batchstep_no
            INTO l_batchstep_id;

           IF cur_validate_batchstep_no%NOTFOUND THEN
              gme_common_pvt.log_message
                              ('GME_BATCHSTEP_NOT_FOUND');		--TODO
              RAISE fnd_api.g_exc_error;
           END IF;
      CLOSE cur_validate_batchstep_no;

	  l_mtrl_dtl_id := NULL;
	  OPEN cur_material_dtl(l_organization_id, l_batch_id, p_line_type, p_line_no);
           FETCH cur_material_dtl
            INTO l_mtrl_dtl_id, l_actual_qty;

		   IF cur_material_dtl%NOTFOUND THEN
              gme_common_pvt.log_message
                              ('GME_MTRL_LINE_NOT_FOUND');		--TODO
              RAISE fnd_api.g_exc_error;
           END IF;

		   IF l_actual_qty > 0 THEN
		      gme_common_pvt.log_message
                              ('GME_BSI_UPDT_NOTALWD_ACTQTY');		--TODO
              RAISE fnd_api.g_exc_error;
           END IF;

      CLOSE cur_material_dtl;

      IF g_debug <= gme_debug.g_log_statement THEN
         gme_debug.put_line ('Finished parameter validation');
      END IF;

	  l_batch_step_items.material_detail_id := l_mtrl_dtl_id;

	  IF NOT gme_batch_step_items_dbl.fetch_row
						(p_batch_step_items      => l_batch_step_items
						,x_batch_step_items      => l_batch_step_items) THEN

		  l_batch_step_items.material_detail_id := l_mtrl_dtl_id;
		  l_batch_step_items.batch_id := l_batch_id;
		  l_batch_step_items.batchstep_id := l_batchstep_id;

		  IF NOT gme_batch_step_items_dbl.insert_row
			  		    (p_batch_step_items      => l_batch_step_items
				    	,x_batch_step_items      => l_batch_step_items) THEN
			  gme_common_pvt.log_message
                              ('GME_ERROR_INSERT_STEPITEM');		--TODO
			  RAISE fnd_api.g_exc_error;
		  END IF;

	  ELSE
		  l_batch_step_items.material_detail_id := l_mtrl_dtl_id;
		  l_batch_step_items.batch_id := l_batch_id;
		  l_batch_step_items.batchstep_id := l_batchstep_id;

		  IF NOT gme_batch_step_items_dbl.update_row
			  		    (p_batch_step_items      => l_batch_step_items) THEN
			  gme_common_pvt.log_message
                              ('GME_ERROR_UPDATE_STEPITEM');		--TODO
			  RAISE fnd_api.g_exc_error;
		  END IF;
	  END IF;


      IF (p_commit = fnd_api.g_true) THEN
         COMMIT;
      END IF;

      gme_common_pvt.log_message ('GME_STEP_ITEM_ASSOCIATED');
      gme_common_pvt.count_and_get (x_count        => x_message_count
                                   ,p_encoded      => fnd_api.g_false
                                   ,x_data         => x_message_list);

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line (   'Completed ' || l_api_name || ' at '
                             || TO_CHAR (SYSDATE, 'MM/DD/YYYY HH24:MI:SS') );
      END IF;
   EXCEPTION
      WHEN fnd_api.g_exc_error THEN
         x_return_status := fnd_api.g_ret_sts_error;
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
   END ASSOCIATE_BATCHSTEP_ITEM;
   
   
   
  PROCEDURE update_batch_heat(
    p_batch_id             IN   NUMBER,
    p_heat_number          IN   VARCHAR2,
    x_return_status        OUT  NOCOPY VARCHAR2,
    x_message_count        OUT  NOCOPY NUMBER,
    x_message_list         OUT  NOCOPY VARCHAR2
    ) IS
    
    l_api_name    CONSTANT VARCHAR2 (30) := 'UPDATE_BATCH_HEAT';
    l_batch_status         NUMBER;
    l_terminated_id        NUMBER;
    l_batch_id             NUMBER;
    l_parent_batch_id      NUMBER;
    l_count                NUMBER;
    l_is_parent_batch      NUMBER;
    l_is_unique_heat       NUMBER;
    l_batch_no             VARCHAR2(32);
    l_organization_code    VARCHAR2(3);
    l_old_heat_number      VARCHAR2(100);
    l_allow_duplicate      BOOLEAN :=FALSE;
    l_is_parent_heat       NUMBER;
    
    batch_id_invalid       EXCEPTION;
    batch_status_invalid   EXCEPTION;
    batch_is_terminated    EXCEPTION;
    heat_number_invalid    EXCEPTION;
    
    --get batch_status and terminated_ind
    CURSOR get_batch_status(v_batch_id NUMBER) IS
      SELECT gbh.batch_status,gbh.terminated_ind,gbh.batch_no,mp.organization_code
        FROM gme_batch_header gbh,
             mtl_parameters mp
       WHERE gbh.batch_id = v_batch_id
         AND gbh.organization_id = mp.organization_id;
       
   --check whether batch exist in the gme_batch_genealogy table    
   CURSOR check_genealogy_exist(v_batch_id NUMBER) IS
     SELECT count(1)
       FROM gme_batch_genealogy
      WHERE batch_id = v_batch_id; 
   
   --get parent batch id   
   CURSOR get_parent_batch(v_batch_id NUMBER) IS
     SELECT parent_batch_id
       FROM gme_batch_genealogy
      WHERE batch_id = v_batch_id; 
      
   --check batch is parent batch   
   CURSOR parent_batch_check(v_batch_id NUMBER) IS
     SELECT count(1)
       FROM gme_batch_genealogy
      WHERE parent_batch_id = v_batch_id; 
   
   --check whether heat number is unique    
   CURSOR heat_unique_single_check(v_batch_id NUMBER, v_heat_number VARCHAR2) IS
     SELECT count(1)
       FROM gme_batch_genealogy       
      WHERE batch_id <> v_batch_id
        AND heat_number IS NOT NULL
        AND heat_number = v_heat_number;  
   
   --get all of child batch   
   CURSOR get_child_batches(v_batch_id NUMBER) IS
     SELECT DISTINCT batch_id
       FROM gme_batch_genealogy
      START WITH batch_id = v_batch_id
      CONNECT BY PRIOR batch_id = parent_batch_id
      ORDER BY LEVEL;
      
   -- check whether heat number is unique or only exist in the parent batches.   
   CURSOR heat_unique_multi_check(v_batch_id NUMBER, v_heat_number VARCHAR2) IS           
     SELECT count(1)
       FROM gme_batch_genealogy       
      WHERE heat_number IS NOT NULL
        AND heat_number = v_heat_number
        AND batch_id NOT IN ( SELECT DISTINCT batch_id
                                FROM gme_batch_genealogy
                               START WITH batch_id = v_batch_id
                             CONNECT BY NOCYCLE PRIOR parent_batch_id = batch_id); 
                             
   --get old heat number
   CURSOR get_old_heat_number(v_batch_id NUMBER) IS
     SELECT NVL(heat_number,'x')
       FROM gme_batch_genealogy 
      WHERE batch_id = v_batch_id
        AND rownum = 1;   
        
   --check whether heat number is heat number of any of parent batch
   CURSOR parent_heat_check(v_batch_id NUMBER, v_heat_number VARCHAR2) IS
     SELECT count(1)
       FROM gme_batch_genealogy
      WHERE heat_number IS NOT NULL
        AND heat_number = v_heat_number
        AND batch_id IN ( SELECT DISTINCT batch_id
                            FROM gme_batch_genealogy
                           START WITH batch_id = v_batch_id
                         CONNECT BY NOCYCLE PRIOR parent_batch_id = batch_id)                                               
        AND batch_id <> v_batch_id;
    
  BEGIN   
     
    
  --check current batch status
  OPEN get_batch_status(p_batch_id);
  FETCH get_batch_status INTO l_batch_status,l_terminated_id,l_batch_no,l_organization_code;
  IF get_batch_status%NOTFOUND THEN
    CLOSE get_batch_status;
    RAISE batch_id_invalid;
  ELSE
    CLOSE get_batch_status;
    IF l_batch_status NOT IN  (gme_common_pvt.g_batch_pending,
                               gme_common_pvt.g_batch_wip,
                               gme_common_pvt.g_batch_completed) THEN
      RAISE batch_status_invalid;
    END IF;
    
    IF l_terminated_id = 1 THEN
      RAISE batch_is_terminated; 
    END IF;  
  END IF;    
  
  --BUG 26027932
  --get old heat number
  OPEN get_old_heat_number(p_batch_id);
  FETCH get_old_heat_number INTO l_old_heat_number;
  CLOSE get_old_heat_number;
  --BUG 26027932
  --invoke hook to check whether heat can be duplicated.
  l_allow_duplicate := GME_YIELD_CUSTOMIZATION_PVT.is_duplicate_heat_allowed(p_organization_code => l_organization_code, 
                                                                             p_batch_no          => l_batch_no,
                                                                             p_heat_number       => p_heat_number);  

  --check current exist
  OPEN check_genealogy_exist(p_batch_id);
  FETCH check_genealogy_exist INTO l_count;
  CLOSE check_genealogy_exist;
  
  
  IF l_count = 1 THEN
    --get parent batch id information
    OPEN get_parent_batch(p_batch_id);
    FETCH get_parent_batch INTO l_parent_batch_id;
    CLOSE get_parent_batch;
    
    --parent batch id is -1, that mean single batch or is root batch
    IF l_parent_batch_id = -1 THEN
      --check batch is root batch
      OPEN parent_batch_check(p_batch_id);
      FETCH parent_batch_check INTO l_is_parent_batch;
      CLOSE parent_batch_check;
      
      --single batch
      IF l_is_parent_batch =0 THEN 
        --validate heat_number is unique
        OPEN heat_unique_single_check(p_batch_id,p_heat_number);
        FETCH heat_unique_single_check INTO l_is_unique_heat;
        CLOSE heat_unique_single_check;
        
        IF l_is_unique_heat = 0 THEN
          --update heat number 
          UPDATE gme_batch_genealogy
             SET heat_number = p_heat_number
           WHERE batch_id = p_batch_id;           
        
        ELSE
          --BUG 26027932
          --invoke hook to check whether heat can be duplidated.
          IF l_allow_duplicate THEN
            UPDATE gme_batch_genealogy
               SET heat_number = p_heat_number,
                   dup_override = 'Y',
                   session_id = USERENV('SESSIONID'),
                   user_id = fnd_global.user_id
             WHERE batch_id = p_batch_id;                                                                   
            
          ELSE
            RAISE heat_number_invalid;
            
          END IF;
        
        END IF;
              
      --is root batch,update  all of child batches in hierarchy
      ELSE
        --validate heat_number is unique
        OPEN heat_unique_single_check(p_batch_id,p_heat_number);
        FETCH heat_unique_single_check INTO l_is_unique_heat;
        CLOSE heat_unique_single_check;
        
        IF l_is_unique_heat = 0 THEN
          --update heat number for current batch and all of child batches.
          UPDATE gme_batch_genealogy
             SET heat_number = p_heat_number
           WHERE batch_id in ( SELECT DISTINCT batch_id
                                 FROM gme_batch_genealogy
                                START WITH batch_id = p_batch_id
                                CONNECT BY NOCYCLE PRIOR batch_id = parent_batch_id)
             AND NVL(heat_number,'x') = l_old_heat_number;        
        
        ELSE
          --BUG 26027932
          --invoke hook to check whether heat can be duplidated.
          IF l_allow_duplicate THEN
                                                                   
            UPDATE gme_batch_genealogy
               SET heat_number = p_heat_number,
                   dup_override = 'Y',
                   session_id = USERENV('SESSIONID'),
                   user_id = fnd_global.user_id                   
             WHERE batch_id in ( SELECT DISTINCT batch_id
                                   FROM gme_batch_genealogy
                                  START WITH batch_id = p_batch_id
                                  CONNECT BY NOCYCLE PRIOR batch_id = parent_batch_id)
               AND NVL(heat_number,'x') = l_old_heat_number;                                                                   
          ELSE
                                                                               
            RAISE heat_number_invalid;
          END IF;
        
        END IF;         
      
      END IF;
    
    --parent batch id is not -1, that mean have single parent batch
    ELSE 
      --validate heat number is unique or only exist in parent batch
      OPEN heat_unique_multi_check(p_batch_id,p_heat_number);
      FETCH heat_unique_multi_check INTO l_is_unique_heat;
      CLOSE heat_unique_multi_check;
      
      --check whether heat number is heat number of parent batch.
      OPEN parent_heat_check(p_batch_id,p_heat_number);
      FETCH parent_heat_check INTO l_is_parent_heat;
      CLOSE parent_heat_check; 
      
      IF l_is_unique_heat = 0 OR l_is_parent_heat > 0  THEN
        --update heat number for current batch and all of child batches. 
          UPDATE gme_batch_genealogy
             SET heat_number = p_heat_number
           WHERE batch_id in ( SELECT DISTINCT batch_id
                                 FROM gme_batch_genealogy
                                START WITH batch_id = p_batch_id
                                CONNECT BY NOCYCLE PRIOR batch_id = parent_batch_id)
             AND NVL(heat_number,'x') = l_old_heat_number;           
        
      ELSE
        --BUG 26027932
        --invoke hook to check whether heat can be duplidated.
        IF l_allow_duplicate THEN 
          UPDATE gme_batch_genealogy
             SET heat_number = p_heat_number,
                 dup_override = 'Y',
                 session_id = USERENV('SESSIONID'),
                 user_id = fnd_global.user_id
           WHERE batch_id in ( SELECT DISTINCT batch_id
                                 FROM gme_batch_genealogy
                                START WITH batch_id = p_batch_id
                                CONNECT BY NOCYCLE PRIOR batch_id = parent_batch_id)
             AND NVL(heat_number,'x') = l_old_heat_number;                                                                 
                                                                 
        ELSE                                                                        
          RAISE heat_number_invalid;
        END IF;
        
      END IF;  
    
    
    END IF;       
  
  --have multiple parent batch
  ELSIF l_count > 1 THEN     
    --validate heat number is unique or only exist in parent batch
    OPEN heat_unique_multi_check(p_batch_id,p_heat_number);
    FETCH heat_unique_multi_check INTO l_is_unique_heat;
    CLOSE heat_unique_multi_check;
    
    --check whether heat number is heat number of parent batch.
    OPEN parent_heat_check(p_batch_id,p_heat_number);
    FETCH parent_heat_check INTO l_is_parent_heat;
    CLOSE parent_heat_check;     
      
    IF l_is_unique_heat = 0 OR l_is_parent_heat > 0 THEN
      --update heat number for current batch and all of child batches. 
      UPDATE gme_batch_genealogy
         SET heat_number = p_heat_number
       WHERE batch_id in ( SELECT DISTINCT batch_id
                             FROM gme_batch_genealogy
                            START WITH batch_id = p_batch_id
                            CONNECT BY NOCYCLE PRIOR batch_id = parent_batch_id)
         AND NVL(heat_number,'x') = l_old_heat_number;         
        
    ELSE
      --BUG 26027932
      --invoke hook to check whether heat can be duplidated.
      IF l_allow_duplicate THEN
        --update heat number for current batch and all of child batches. 
        UPDATE gme_batch_genealogy
           SET heat_number = p_heat_number,
               dup_override = 'Y',
               session_id = USERENV('SESSIONID'),
               user_id = fnd_global.user_id
         WHERE batch_id in ( SELECT DISTINCT batch_id
                               FROM gme_batch_genealogy
                              START WITH batch_id = p_batch_id
                              CONNECT BY NOCYCLE PRIOR batch_id = parent_batch_id)
           AND NVL(heat_number,'x') = l_old_heat_number;                                                                 
      ELSE     
        RAISE heat_number_invalid;
      END IF;
        
    END IF;  
    
  END IF; 
     
  
  EXCEPTION
    WHEN batch_id_invalid THEN  
      x_return_status   :=  fnd_api.g_ret_sts_error;
      gme_common_pvt.log_message ('GME_BATCH_ID_NOT_FOUND');
      gme_common_pvt.count_and_get ( x_count        => x_message_count
                                    ,p_encoded      => fnd_api.g_false
                                    ,x_data         => x_message_list);  
    WHEN batch_status_invalid THEN
      x_return_status   :=  fnd_api.g_ret_sts_error;
      gme_common_pvt.log_message ('GME_INVALID_BATCH_STATUS','PROCESS','update_batch_heat');
      gme_common_pvt.count_and_get ( x_count        => x_message_count
                                    ,p_encoded      => fnd_api.g_false
                                    ,x_data         => x_message_list); 
                              
                              
   WHEN batch_is_terminated THEN
     x_return_status   :=  fnd_api.g_ret_sts_error;                             
     gme_common_pvt.log_message ('GME_API_BATCH_TERMINATED');
      gme_common_pvt.count_and_get ( x_count        => x_message_count
                                    ,p_encoded      => fnd_api.g_false
                                    ,x_data         => x_message_list);                                
   WHEN heat_number_invalid THEN
     x_return_status   :=  fnd_api.g_ret_sts_error;                               
     gme_common_pvt.log_message ('GME_HEAT_NUMBER_EXISTS');
      gme_common_pvt.count_and_get ( x_count        => x_message_count
                                    ,p_encoded      => fnd_api.g_false
                                    ,x_data         => x_message_list);      
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
       gme_common_pvt.count_and_get (x_count        => x_message_count
                                    ,p_encoded      => fnd_api.g_false
                                    ,x_data         => x_message_list);       
  
  
  END update_batch_heat;   
  
 
END gme_api_main;
/
COMMIT ;
EXIT;
 -- show errors;
