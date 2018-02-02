/* +======================================================================+ */
/* |    Copyright (c) 2005, 2017 Oracle and/or its affiliates.           | */
/* |                         All rights reserved.                         | */
/* |                           Version 12.0.0                             | */
/* +======================================================================+ */
REM dbdrv: sql ~PROD ~PATH ~FILE none none none package &phase=plb \
REM dbdrv: checkfile(120.18.12010000.12=120.23.12020000.6)(120.14.12000000.11=120.18.12010000.8)(115.32=120.1):~PROD:~PATH:~FILE
SET VERIFY OFF
WHENEVER SQLERROR EXIT FAILURE ROLLBACK;
WHENEVER OSERROR EXIT FAILURE ROLLBACK;

REM **********************************************************************
REM *                                                                    *
REM * FILE:    GMEVURBB.pls                                              *
REM * PURPOSE: Package Body for the GME UNRELEASE BATCH routines         *
REM * AUTHOR:  A. Newbury, OPM Development                               *
REM * DATE:    May 2005                                                  *
REM * HISTORY:                                                           *
REM * ========                                                           *
REM * Archana Mundhe 30-May-2008 Bug 6437252 - LPN Support               * 
REM * Added lpn_id to gme_reservations_pvt.create_material_reservation   *
REM * call in procedure create_resv_pplot                                *

REM * G. Muratore    29-Jun-2009 Bug 8607365  
REM *   No need to do negative inventory checking for phantom prods or ingredients.
REM *   PROCEDURE: revert_material_full

REM * Apeksha Mishra 07-Aug-2009 Bug 8672422   
REM *  We need to refetch step to  make sure it's not already processed

REM * G. Muratore    23-Jun-2010 Bug 11855868 / 11846735
REM *   Do not pass in event id to create history. This issue was found while
REM *   debugging 11846735. g_transaction_header_id value was too large and
REM *   it really did not get used anyway.
REM *   PROCEDURE: unrelease_batch

REM * Shaliu Chen    18-JUL-2014  ER 19161894                                                 
REM *   Modify create batch to invoke requisition creation program if batch include OSP step  
 
REM * QZENG          22-MAY-2015 Bug 21101876
REM *   Modified unrelease_material, raise error when phantom batch has been
REM *   onhold.

REM * G. Muratore    29-Sep-2015 Bug 19868921
REM *   Initialize new field rsrv_quantity in memory table g_mat_txn_hdr_tbl to NULL or
REM *   correct quantity. Reservation code will now use this value if initialized.
REM *   PROCEDURE: revert_material_full AND create_matl_resv_pplot
REM **********************************************************************

/*************************************************************************
* This file contains the procedure for unreleasing batches in Oracle     *
* Process Manufacturing (OPM). Each procedure has a common set of        *
* parameters to which API-specific parameters are appended.    
*************************************************************************/

CREATE OR REPLACE PACKAGE BODY gme_unrelease_batch_pvt AS
/* $Header: GMEVURBB.pls 120.23.12020000.6 2017/05/15 17:28:39 gmurator ship $ */
   g_debug               VARCHAR2 (5)  := fnd_profile.VALUE ('AFLOG_LEVEL');
/* Global Variables */
   g_pkg_name   CONSTANT VARCHAR2 (30) := 'GME_UNRELEASE_BATCH_PVT';

/*===========================================================================================
Procedure
  unrelease_batch
Description
  This particular procedure handles unreleasing of the batch.
Parameters
  p_batch_header_rec     The batch header record to unrelease
  x_batch_header_rec     The batch header out row that was unreleased
  p_create_resv_pend_lots Indicates whether to recreate reservations/pending product lots

            S - Success
            E - Error
            U - Unexpected error
 History

=============================================================================================*/
   PROCEDURE unrelease_batch (
      p_batch_header_rec        IN              gme_batch_header%ROWTYPE
     ,p_create_resv_pend_lots   IN              NUMBER
     ,x_batch_header_rec        OUT NOCOPY      gme_batch_header%ROWTYPE
     ,x_return_status           OUT NOCOPY      VARCHAR2)
   IS
      l_api_name     CONSTANT VARCHAR2 (30)              := 'unrelease_batch';

      CURSOR cur_get_steps (v_batch_id NUMBER)
      IS
         SELECT *
           FROM gme_batch_steps
          WHERE batch_id = v_batch_id;

      CURSOR cur_get_and_lock_mtls (v_batch_id NUMBER)
      IS
         SELECT *
           FROM gme_material_details
          WHERE batch_id = v_batch_id
            FOR UPDATE OF actual_qty NOWAIT;

      l_material_detail_tbl   gme_common_pvt.material_details_tab;
      l_material_detail_rec   gme_material_details%ROWTYPE;
      l_batch_step_tbl        gme_common_pvt.steps_tab;
      l_batch_step_rec        gme_batch_steps%ROWTYPE;
      l_in_batch_step_rec     gme_batch_steps%ROWTYPE;

      error_update_row        EXCEPTION;
      error_unrelease_matl    EXCEPTION;
      error_unrelease_step    EXCEPTION;
      error_mtls_locked       EXCEPTION;

      -- Bug 5903208
      l_message_count               NUMBER;
      l_message_list                VARCHAR2(2000);
      gmf_cost_failure              EXCEPTION;

   BEGIN
      IF NVL (g_debug, gme_debug.g_log_procedure + 1) <=
                                                    gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ' unreleasing batch_id='
                             || p_batch_header_rec.batch_id);
      END IF;

      /* Set the return status to success initially */
      x_return_status := fnd_api.g_ret_sts_success;

      -- set output structure
      x_batch_header_rec := p_batch_header_rec;

      -- Fetch and lock all the material lines of the batch
      OPEN cur_get_and_lock_mtls (x_batch_header_rec.batch_id);
      FETCH cur_get_and_lock_mtls
      BULK COLLECT INTO l_material_detail_tbl;

      IF SQLCODE = -54
      THEN
         CLOSE cur_get_and_lock_mtls;
         RAISE error_mtls_locked;
      END IF;
      CLOSE cur_get_and_lock_mtls;

      -- set batch status
      x_batch_header_rec.batch_status := gme_common_pvt.g_batch_pending;
      gme_common_pvt.g_batch_status_check := fnd_api.g_false;
      -- set actual start date to NULL...
      x_batch_header_rec.actual_start_date := NULL;

      -- Update the batch header
      IF NOT gme_batch_header_dbl.update_row
                                         (p_batch_header      => x_batch_header_rec) THEN
         RAISE error_update_row;
      END IF;

      -- Update WHO columns for output structure
      x_batch_header_rec.last_updated_by := gme_common_pvt.g_user_ident;
      x_batch_header_rec.last_update_date := gme_common_pvt.g_timestamp;
      x_batch_header_rec.last_update_login := gme_common_pvt.g_login_id;

      FOR i IN 1 .. l_material_detail_tbl.COUNT LOOP
         l_material_detail_rec := l_material_detail_tbl (i);
         unrelease_material
            (p_material_detail_rec        => l_material_detail_rec
            ,p_update_inventory_ind       => x_batch_header_rec.update_inventory_ind
            ,p_create_resv_pend_lots      => p_create_resv_pend_lots
            ,p_from_batch                 => TRUE
            ,x_return_status              => x_return_status);

         IF x_return_status <> fnd_api.g_ret_sts_success THEN
            RAISE error_unrelease_matl;
         END IF;
      END LOOP;

      -- Fetch all the steps of the batch
      OPEN cur_get_steps (x_batch_header_rec.batch_id);

      FETCH cur_get_steps
      BULK COLLECT INTO l_batch_step_tbl;

      CLOSE cur_get_steps;

      -- Unrelease steps associated with the batch
      FOR i IN 1 .. l_batch_step_tbl.COUNT LOOP
         l_batch_step_rec := l_batch_step_tbl (i);
         -- l_in_batch_step_rec := l_batch_step_rec;

         -- 8672422 - We need to refetch step here and make sure it's not already processed
         -- as unrelease step also processes dependent steps.
         IF (gme_batch_steps_dbl.fetch_row (l_batch_step_rec, l_in_batch_step_rec)) THEN
            IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
               gme_debug.put_line ('In DB date is '||TO_CHAR(l_in_batch_step_rec.last_update_date, 'DD-MON-YYYY HH24:MI:SS'));
               gme_debug.put_line ('In step status is '||l_in_batch_step_rec.step_status);
            END IF;
            --IF l_in_batch_step_rec.step_status = 1 THEN
               -- If the step is already pending go to next step.
               --continue;
            --END IF;
         END IF;

         gme_unrelease_step_pvt.unrelease_step
            (p_batch_step_rec             => l_in_batch_step_rec
            ,p_update_inventory_ind       => x_batch_header_rec.update_inventory_ind
            ,p_create_resv_pend_lots      => NULL
            ,p_from_unrelease_batch       => 1
            ,x_batch_step_rec             => l_batch_step_rec
            ,x_return_status              => x_return_status);

         IF x_return_status <> fnd_api.g_ret_sts_success THEN
            RAISE error_unrelease_step;
         END IF;
      END LOOP;                   -- FOR i IN 1 .. l_batch_step_tbl.COUNT LOOP


      -- 
      -- Bug 5903208 - GMF Call to delete batch requirements
      --
      GMF_VIB.Delete_Batch_Requirements
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

      -- Bug 11846735 - Do not pass in a value for event_id.
      IF NOT gme_common_pvt.create_history
                              (p_batch_header_rec      => x_batch_header_rec
                              ,p_original_status       => gme_common_pvt.g_batch_wip) THEN
                              -- ,p_event_id              => gme_common_pvt.g_transaction_header_id) THEN
         IF g_debug <= gme_debug.g_log_procedure THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ' create history returned error');
         END IF;
      END IF;

      IF NVL (g_debug, gme_debug.g_log_procedure + 1) <=
                                                     gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;
   EXCEPTION
      WHEN   gmf_cost_failure THEN
        -- Bug 5043868
        x_return_status := FND_API.G_RET_STS_ERROR;
    
      WHEN error_mtls_locked THEN
         gme_common_pvt.log_message ('GME_API_BATCH_LINES_LOCKED');
         x_return_status := FND_API.G_RET_STS_ERROR;
      WHEN error_update_row THEN
         gme_common_pvt.log_message ('GME_UNEXPECTED_ERROR', 'ERROR'
                                    ,SQLERRM);
         x_return_status := fnd_api.g_ret_sts_unexp_error;
      WHEN error_unrelease_matl OR error_unrelease_step THEN
         NULL;
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
   END unrelease_batch;

   PROCEDURE unrelease_material (
      p_material_detail_rec     IN       gme_material_details%ROWTYPE
     ,p_update_inventory_ind    IN       VARCHAR2
     ,p_create_resv_pend_lots   IN       NUMBER
     ,p_from_batch              IN       BOOLEAN
     ,x_return_status           OUT NOCOPY VARCHAR2)
   IS
      l_api_name      CONSTANT VARCHAR2 (30)          := 'unrelease_material';
      l_phantom_batch_rec      gme_batch_header%ROWTYPE;
      l_in_phantom_batch_rec   gme_batch_header%ROWTYPE;
      l_item_rec               mtl_system_items_b%ROWTYPE;
      l_material_detail_rec    gme_material_details%ROWTYPE;

      l_exception_material_tbl gme_common_pvt.exceptions_tab;
      l_actual_qty             NUMBER;
      -- Bug 21101876, Added by QZENG, for onhold
      l_batch_no               gme_batch_header.batch_no%TYPE;

      error_unrelease_batch    EXCEPTION;
      error_fetch_batch        EXCEPTION;
      error_get_item           EXCEPTION;
      error_revert_material    EXCEPTION;
      error_update_row         EXCEPTION;
      -- Bug 21101876, Added by QZENG, for onhold
      error_phantom_batch_hold  EXCEPTION;
   BEGIN
      IF NVL (g_debug, gme_debug.g_log_procedure + 1) <=
                                                    gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ' unrelease material material_detail_id='
                             || p_material_detail_rec.material_detail_id);
      END IF;

      -- Set the return status to success initially
      x_return_status := fnd_api.g_ret_sts_success;
      l_material_detail_rec := p_material_detail_rec;

      IF l_material_detail_rec.phantom_id IS NOT NULL THEN
                                     -- phantom -> unrelease the phantom batch
         l_phantom_batch_rec.batch_id := l_material_detail_rec.phantom_id;

         IF NOT gme_batch_header_dbl.fetch_row (l_phantom_batch_rec
                                               ,l_phantom_batch_rec) THEN
            RAISE error_fetch_batch;
         END IF;

         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line
                          (   g_pkg_name
                           || '.'
                           || l_api_name
                           || ' found phantom ingredient material_detail_id='
                           || l_material_detail_rec.material_detail_id);
         END IF;

         IF l_phantom_batch_rec.batch_status = gme_common_pvt.g_batch_wip THEN
            -- Bug 21101876, Added by QZENG, check batch hold type for the
            -- phantom batch
            IF gme_common_pvt.GET_BATCH_HOLD_STATUS(l_material_detail_rec.phantom_id) <> 'R' THEN
               l_batch_no := l_phantom_batch_rec.batch_no;
               raise error_phantom_batch_hold;
            END IF;
            -- End Bug 21101876

            l_in_phantom_batch_rec := l_phantom_batch_rec;
            unrelease_batch
                         (p_batch_header_rec           => l_in_phantom_batch_rec
                         ,p_create_resv_pend_lots      => p_create_resv_pend_lots
                         ,x_batch_header_rec           => l_phantom_batch_rec
                         ,x_return_status              => x_return_status);

            IF x_return_status <> fnd_api.g_ret_sts_success THEN
               RAISE error_unrelease_batch;
            END IF;
         END IF;
      -- IF l_phantom_batch_rec.batch_status = gme_common_pvt.g_batch_wip THEN
      ELSE
  -- not a phantom ingredient;
  -- phantom ingredient trxn will be deleted when phantom product is processed
         gme_material_detail_pvt.get_item_rec
                       (p_org_id             => l_material_detail_rec.organization_id
                       ,p_item_id            => l_material_detail_rec.inventory_item_id
                       ,x_item_rec           => l_item_rec
                       ,x_return_status      => x_return_status);

         IF x_return_status <> fnd_api.g_ret_sts_success THEN
            RAISE error_get_item;
         END IF;

         IF     p_update_inventory_ind = 'Y'
            AND l_item_rec.mtl_transactions_enabled_flag = 'Y'
            AND l_material_detail_rec.actual_qty <> 0 THEN
            -- delete all transactions for this material
            
            revert_material_full
                         (p_material_detail_rec        => l_material_detail_rec
                         ,p_create_resv_pend_lots      => p_create_resv_pend_lots
                         ,p_ignore_transactable        => FALSE
                         ,x_actual_qty                 => l_actual_qty
                         ,x_exception_material_tbl     => l_exception_material_tbl
                         ,x_return_status              => x_return_status);

            -- here, there's no need to look at l_actual_qty and l_exception_material_tbl
            -- because if l_actual_qty is anything other than 0, we would get back error in
            -- return status and raise an exception; also, l_exception_material_tbl won't
            -- contain anything because of same reason; if the transactions can't be reversed,
            -- it's an error here

            IF x_return_status <> fnd_api.g_ret_sts_success THEN
               RAISE error_revert_material;
            END IF;
         END IF;       -- IF x_batch_header_rec.update_inventory_ind = 'Y' AND
      END IF;          -- IF l_material_detail_rec.phantom_id IS NOT NULL THEN

      l_material_detail_rec.actual_qty := 0;

      IF p_from_batch THEN
        l_material_detail_rec.wip_plan_qty := NULL;
      END IF;

      IF NOT gme_material_details_dbl.update_row (l_material_detail_rec) THEN
         RAISE error_update_row;
      END IF;

      IF NVL (g_debug, gme_debug.g_log_procedure + 1) <=
                                                     gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;
   EXCEPTION
      WHEN error_update_row OR error_fetch_batch THEN
         gme_common_pvt.log_message ('GME_UNEXPECTED_ERROR', 'ERROR'
                                    ,SQLERRM);
         x_return_status := fnd_api.g_ret_sts_unexp_error;
      WHEN error_get_item OR error_unrelease_batch OR error_revert_material THEN
         NULL;
      -- Bug 21101876, Added by QZENG, raise error when phantom batch is onhold.
      WHEN error_phantom_batch_hold THEN
        gme_common_pvt.log_message ('GME_PHANTOM_ONHOLD', 'BATCH_NO', l_batch_no);
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
   END unrelease_material;

   PROCEDURE revert_material_full (
      p_material_detail_rec     IN            gme_material_details%ROWTYPE
     ,p_create_resv_pend_lots   IN            NUMBER
     ,p_ignore_transactable     IN            BOOLEAN DEFAULT FALSE
     ,x_actual_qty              OUT NOCOPY    NUMBER
     ,x_exception_material_tbl  IN OUT NOCOPY gme_common_pvt.exceptions_tab
     ,x_return_status           OUT NOCOPY    VARCHAR2)
   IS
      l_api_name   CONSTANT VARCHAR2 (30)           := 'revert_material_full';
      l_mmt_tbl             gme_common_pvt.mtl_mat_tran_tbl;
      l_mmt_rec             mtl_material_transactions%ROWTYPE;
      l_mmln_tbl            gme_common_pvt.mtl_trans_lots_num_tbl;
      l_sequence            NUMBER;
      l_pplot_rec           gme_pending_product_lots%ROWTYPE;
      l_out_pplot_rec       gme_pending_product_lots%ROWTYPE;
      l_return_status       VARCHAR2(1);

      -- Bug 13017256
      l_trans_date          DATE;

      error_get_trans       EXCEPTION;
      error_del_trans       EXCEPTION;
      l_txn_hdr_tbl_cnt     NUMBER; -- nsinghi bug#5176319
      /* Bug 5021522 Added cursor */
      /* Bug 5754914 Get reservable_type */
       CURSOR Cur_item_details(v_organization_id   NUMBER,
                               v_inventory_item_id NUMBER) IS
        SELECT i.lot_control_code, i.concatenated_segments, i.inventory_item_id, i.reservable_type
        FROM   mtl_system_items_kfv i
        WHERE  i.organization_id = v_organization_id
               AND i.inventory_item_id = v_inventory_item_id;
      l_item_rec   Cur_item_details%ROWTYPE;

      -- Bug 12836004
      l_lpn_subinv          VARCHAR2(100);
      l_lpn_no              VARCHAR2(100);
      l_lpn_loc             NUMBER;
      l_lpn_context         NUMBER;
      
   BEGIN
      IF NVL (g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'|| l_api_name);
         gme_debug.put_line (g_pkg_name|| '.'|| l_api_name|| ' material_detail_id='|| p_material_detail_rec.material_detail_id);
         gme_debug.put_line (g_pkg_name|| '.'|| l_api_name|| ' p_create_resv_pend_lots='|| p_create_resv_pend_lots);
         IF p_ignore_transactable THEN
            gme_debug.put_line (g_pkg_name|| '.'|| l_api_name|| ' p_ignore_transactable IS TRUE');
         ELSE
            gme_debug.put_line (g_pkg_name|| '.'|| l_api_name|| ' p_ignore_transactable IS FALSE');
         END IF;
      END IF;

      -- Set the return status to success initially
      x_return_status := fnd_api.g_ret_sts_success;
      gme_transactions_pvt.get_mat_trans
                    (p_mat_det_id         => p_material_detail_rec.material_detail_id
                    ,p_batch_id           => p_material_detail_rec.batch_id
                    ,x_mmt_tbl            => l_mmt_tbl
                    ,x_return_status      => x_return_status);

      IF x_return_status <> fnd_api.g_ret_sts_success THEN
         RAISE error_get_trans;
      END IF;

      -- Bug 13017256 - Let's initialize the variable with the user entered date.
      l_trans_date := NULL;
      IF gme_common_pvt.g_ib_timestamp_set > 0 THEN
         l_trans_date := NVL(gme_common_pvt.g_ib_timestamp_date, gme_common_pvt.g_timestamp);
      END IF;

      x_actual_qty := p_material_detail_rec.actual_qty;
      IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
         gme_debug.put_line(g_pkg_name|| '.'|| l_api_name || ' actual_qty = '||x_actual_qty);
      END IF;

      FOR i IN 1 .. l_mmt_tbl.COUNT LOOP
         l_mmt_rec := l_mmt_tbl (i);
         
         /* Bug 5021522 Added logic to get item details */
         IF (NVL(l_item_rec.inventory_item_id,0) <> l_mmt_rec.inventory_item_id) THEN
            OPEN Cur_item_details(l_mmt_rec.organization_id, l_mmt_rec.inventory_item_id);
            FETCH Cur_item_details INTO l_item_rec;
            CLOSE Cur_item_details;
         END IF;
         /* End Bug 5021522 */
         
         IF (l_item_rec.lot_control_code = 2) THEN
            gme_transactions_pvt.get_lot_trans
                (p_transaction_id     => l_mmt_rec.transaction_id
                ,x_mmln_tbl           => l_mmln_tbl
                ,x_return_status      => x_return_status);
               
            IF x_return_status <> fnd_api.g_ret_sts_success THEN
               RAISE error_get_trans;
            END IF;
         END IF;
        
         /* Bug 5021522 Added logic to see if inventory will go negative when opposite txn is created */
         IF (l_mmt_rec.transaction_type_id IN (gme_common_pvt.g_ing_return, gme_common_pvt.g_prod_completion, gme_common_pvt.g_byprod_completion)) THEN
            -- Bug 8607365 - No need to do negative inventory checking for phantom prods or ingredients.
            IF (NVL(p_material_detail_rec.phantom_line_id, 0) = 0) THEN
               IF check_inv_negative(p_mmt_rec   => l_mmt_rec,
                                     p_mmln_tbl  => l_mmln_tbl,
                                     p_item_no   => l_item_rec.concatenated_segments) THEN
                  RAISE fnd_api.g_exc_error;
               END IF;
            END IF;
         END IF;
         
         /* Bug 12836004 Added logic to see if LPN would get associated with a diff locator if an opp txn is created */
         IF (g_debug <= gme_debug.g_log_statement) THEN
              gme_debug.put_line (   g_pkg_name
                              || '.'
                              || l_api_name
                              || ':'
                              || 'Checking the current the LPN subinv and loc for lpn :'||l_mmt_rec.lpn_id);
         END IF;

         IF (l_mmt_rec.lpn_id IS NOT NULL AND
             l_mmt_rec.subinventory_code IS NOT NULL AND
             GME_TRANSACTIONS_PVT.check_lpn_subinv_loc
                                 (p_lpn_id     =>  l_mmt_rec.lpn_id,
                                  p_in_subinv  =>  l_mmt_rec.subinventory_code,
                                  p_in_locid   =>  l_mmt_rec.locator_id,
                                  x_out_subinv =>  l_lpn_subinv,
                                  x_out_locId  =>  l_lpn_loc,
                                  x_out_lpnno  =>  l_lpn_no,
                                  x_context    =>  l_lpn_context) = FALSE) THEN
            -- The current Lpn locator is diff. WMS functionality doesnt allow an LPN to have an onhand in 2 diff locators.
            -- So raise an error.
            IF (g_debug <= gme_debug.g_log_statement) THEN
               gme_debug.put_line (   g_pkg_name
                           || '.'
                           || l_api_name
                           || ':'
                           || 'Raising an error ');
            
               gme_debug.put_line (   g_pkg_name
                           || '.'
                           || l_api_name
                           || ':'
                           || 'Original txn subinv :'
                           || l_mmt_rec.subinventory_code);
            
               gme_debug.put_line (   g_pkg_name
                           || '.'
                           || l_api_name
                           || ':'
                           || 'Original txn Locator id :'
                           || l_mmt_rec.locator_id);
            
               gme_debug.put_line (   g_pkg_name
                           || '.'
                           || l_api_name
                           || ':'
                           || 'Current LPN subinv : '
                           || l_lpn_subinv);
            
               gme_debug.put_line (   g_pkg_name
                           || '.'
                           || l_api_name
                           || ':'
                           || 'Current LPN locator id '
                           || l_lpn_loc);
            END IF;

            gme_common_pvt.log_message
             ( p_message_code => 'GME_LPN_LOC_MISMATCH'
              ,p_product_code => 'GME'
              ,p_token1_name  => 'ITEM_NO'
              ,p_token1_value => l_item_rec.concatenated_segments
              ,p_token2_name  => 'LPN_NO'
              ,p_token2_value => l_lpn_no
             );
            RAISE fnd_api.g_exc_error;
         END IF;
         
         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
             gme_debug.put_line(g_pkg_name|| '.'|| l_api_name||
                               ' calling gme_transactions_pvt.delete_material_txn with trxn_id= '||l_mmt_rec.transaction_id);
         END IF;

         -- Bug 13017256 - Let's pass in user entered date. It will only get  
         -- used if necessary. This should only be set during negative IB.
         
         -- delete this transaction, reduce the qty to decrement
         gme_transactions_pvt.delete_material_txn
                             (p_transaction_id      => l_mmt_rec.transaction_id
                             ,p_trans_date          => l_trans_date
                             ,p_txns_pair           => NULL
                             ,x_return_status       => x_return_status);

         IF x_return_status <> fnd_api.g_ret_sts_success THEN
            IF x_return_status = gme_common_pvt.g_not_transactable AND p_ignore_transactable THEN
               -- don't do anything... move to the next...
               IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
                   gme_debug.put_line(g_pkg_name|| '.'|| l_api_name
                              || ' gme_transactions_pvt.delete_material_txn returned '||l_return_status
                              || ' but p_ignore_transactable is set to TRUE; so moving to the next transaction');
               END IF;
            ELSE
               IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
                   gme_debug.put_line(g_pkg_name|| '.'|| l_api_name
                              || ' gme_transactions_pvt.delete_material_txn returned '||l_return_status
                              || ' but p_ignore_transactable is set to FALSE; raising exception');
               END IF;
               RAISE error_del_trans;
            END IF;
         ELSE  -- delete was successful; recreate resv / pplot if requested         
            IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
               gme_debug.put_line(g_pkg_name|| '.'|| l_api_name || ' delete loop; actual_qty = '||x_actual_qty);
               gme_debug.put_line(g_pkg_name|| '.'|| l_api_name || ' delete loop; trans qty fromm mmt = '||l_mmt_rec.transaction_quantity);
            END IF;
            
            -- Pawan Kumar bug 5483071 added following if condition
            IF (p_material_detail_rec.line_type = -1 ) THEN
               x_actual_qty := x_actual_qty + (l_mmt_rec.transaction_quantity);
            ELSE
               x_actual_qty := x_actual_qty - (l_mmt_rec.transaction_quantity) ;
            END IF; 
            
            IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
               gme_debug.put_line(g_pkg_name|| '.'|| l_api_name || 'after trxn delete loop; actual_qty = '||x_actual_qty);
            END IF;
         
            IF p_create_resv_pend_lots = 1 THEN
               -- nsinghi bug#5176319. Do not already create reservation. Reservation will be created in gme_post_process after onhand is increased due to wip return.
               /* Bug 5754914 Added reservable_type condition */
               IF p_material_detail_rec.line_type = gme_common_pvt.g_line_type_ing AND l_item_rec.reservable_type = 1 THEN
            
	               l_txn_hdr_tbl_cnt := gme_common_pvt.g_mat_txn_hdr_tbl.COUNT;
                  gme_common_pvt.g_mat_txn_hdr_tbl(l_txn_hdr_tbl_cnt).txn_header_id := l_mmt_rec.transaction_id;
                  gme_common_pvt.g_mat_txn_hdr_tbl(l_txn_hdr_tbl_cnt).material_dtl_id := p_material_detail_rec.material_detail_id;
                  
                  -- Bug 19868921 - Initialize rsrv_quantity to NULL which means rereserve full quantity.		
                  gme_common_pvt.g_mat_txn_hdr_tbl(l_txn_hdr_tbl_cnt).rsrv_quantity := NULL;		
                  
                  IF (NVL (g_debug, 0) = gme_debug.g_log_statement) THEN
                     gme_debug.put_line (   g_pkg_name
                                        || '.'
                                        || l_api_name
                                        || ':'
                                        ||'gme_common_pvt.g_mat_txn_hdr_tbl('
                                        ||l_txn_hdr_tbl_cnt||') = '
                                        || l_mmt_rec.transaction_id);
                  END IF;
               ELSE    -- product or by-product
                  create_resv_pplot
                           (p_material_detail_rec    => p_material_detail_rec
                           ,p_mmt_rec                => l_mmt_rec
                           ,p_mmln_tbl               => l_mmln_tbl
                           ,x_return_status          => l_return_status);
               END IF;  -- IF l_material_detail_rec.line_type = gme_common_pvt.g_line_type_ing         
               -- don't error out if the recreate fails
            END IF;
         END IF;
      END LOOP;  -- FOR i IN 1 .. l_mmt_tbl.COUNT LOOP

      IF x_actual_qty <> 0 THEN
         -- create batch exception
         gme_release_batch_pvt.create_batch_exception
                     (p_material_dtl_rec         => p_material_detail_rec
                     ,p_pending_move_order_ind   => NULL
                     ,p_pending_rsrv_ind         => NULL
                     ,p_transacted_qty           => p_material_detail_rec.actual_qty - x_actual_qty
                     ,p_exception_qty            => x_actual_qty
                     ,p_force_unconsumed         => fnd_api.g_true
                     ,x_exception_material_tbl   => x_exception_material_tbl
                     ,x_return_status            => x_return_status);
      END IF;

      IF NVL (g_debug, gme_debug.g_log_procedure + 1) <=
                                                     gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name|| ' with return_status = '||x_return_status||' and x_actual_qty='||x_actual_qty);
      END IF;
   EXCEPTION
     WHEN fnd_api.g_exc_error THEN
       x_return_status := fnd_api.g_ret_sts_error;
      WHEN error_get_trans OR error_del_trans THEN
         NULL;
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
   END revert_material_full;

   -- nsinghi bug#5176319. Created this proc. It will create ingredient reservation during batch/step unrelease.
   -- Bug 6997483  01-May-2008 Archana Mundhe Added parameter transaction_id.  
   -- The transaction_id will be pased by GME_transactions_PVT.gme_post_process  
   -- and is the ingredient return/reversal transaction id. 
   PROCEDURE create_matl_resv_pplot ( 
                p_material_dtl_id IN NUMBER,
                p_transaction_id  IN NUMBER,
                x_return_status OUT NOCOPY VARCHAR2)
   IS
     l_mat_dtl_rec       gme_material_details%ROWTYPE;
     l_mmt_rec           mtl_material_transactions%ROWTYPE;
     l_new_mmt_rec       mtl_material_transactions%ROWTYPE; -- Bug 6997483
     l_mmln_rec          gme_common_pvt.mtl_trans_lots_num_tbl;
     l_trans_hdr_id      NUMBER;
     l_api_name   CONSTANT VARCHAR2 (30)            := 'CREATE_MATL_RESV_PPLOT';
     
     -- Bug 6997483      
     l_new_transaction_id       NUMBER; 
     CURSOR cur_get_transaction (v_transaction_id NUMBER) 
      IS 
         SELECT * 
         FROM mtl_material_transactions mmt 
         WHERE transaction_id = v_transaction_id;
          
   BEGIN
     IF (NVL (g_debug, 0) IN (gme_debug.g_log_statement, gme_debug.g_log_procedure) ) THEN
        gme_debug.put_line (g_pkg_name || '.' || l_api_name || ':' || 'Entering');
     END IF;

     x_return_status := fnd_api.g_ret_sts_success;

     IF (NVL (g_debug, 0) = gme_debug.g_log_statement) THEN
        gme_debug.put_line (   g_pkg_name|| '.'|| l_api_name|| ':'||'p_material_dtl_id = '
                 ||p_material_dtl_id);
        gme_debug.put_line (   g_pkg_name|| '.'|| l_api_name|| ':'||'gme_common_pvt.g_mat_txn_hdr_tbl.COUNT = '
                 ||gme_common_pvt.g_mat_txn_hdr_tbl.COUNT);
     END IF;

     IF gme_common_pvt.g_mat_txn_hdr_tbl.COUNT > 0 THEN
        FOR cnt IN gme_common_pvt.g_mat_txn_hdr_tbl.FIRST..gme_common_pvt.g_mat_txn_hdr_tbl.LAST
        LOOP
           IF gme_common_pvt.g_mat_txn_hdr_tbl(cnt).material_dtl_id = p_material_dtl_id THEN
	           IF (NVL (g_debug, 0) = gme_debug.g_log_statement) THEN
                 gme_debug.put_line (   g_pkg_name|| '.'|| l_api_name|| ':'||'gme_common_pvt.g_mat_txn_hdr_tbl('||cnt||').txn_header_id = '
                       ||gme_common_pvt.g_mat_txn_hdr_tbl(cnt).txn_header_id);
                 gme_debug.put_line (   g_pkg_name|| '.'|| l_api_name|| ':'||'gme_common_pvt.g_mat_txn_hdr_tbl('||cnt||').material_dtl_id = '
                       ||gme_common_pvt.g_mat_txn_hdr_tbl(cnt).material_dtl_id);
              END IF;

              l_mat_dtl_rec.material_detail_id := gme_common_pvt.g_mat_txn_hdr_tbl(cnt).material_dtl_id;

              IF NOT gme_material_details_dbl.fetch_row
                 (p_material_detail      => l_mat_dtl_rec
                 ,x_material_detail      => l_mat_dtl_rec) THEN
                 RAISE fnd_api.g_exc_error;
              END IF;

	           gme_transactions_pvt.get_mmt_transactions (
                    p_transaction_id   => gme_common_pvt.g_mat_txn_hdr_tbl(cnt).txn_header_id
                   ,x_mmt_rec          => l_mmt_rec
                   ,x_mmln_tbl         => l_mmln_rec
                   ,x_return_status    => x_return_status);

              IF l_mat_dtl_rec.line_type = gme_common_pvt.g_line_type_ing THEN
                 -- Bug 6997483 
                 -- Get the mmt details for the ing return/reversal transaction. 
                 l_new_transaction_id := p_transaction_id; 
                 OPEN cur_get_transaction (l_new_transaction_id); 
                 FETCH cur_get_transaction INTO l_new_mmt_rec; 
                 CLOSE cur_get_transaction;  
    
                 IF (NVL (g_debug, 0) = gme_debug.g_log_statement) THEN 
                    gme_debug.put_line('Ing issue transaction id is ' || l_mmt_rec.transaction_id); 
                    gme_debug.put_line('Ing Return transaction id is ' || l_new_mmt_rec.transaction_id); 
                    gme_debug.put_line('source line id is ' || l_new_mmt_rec.source_line_id); 
                 END IF;
                 
                 -- Bug 6997483 
                 -- Call create_resv_pplot only if the source_line_id of the ing return/reversal transaction  
                 -- matches the transaction_id of the original ing issue transaction.  
                 -- This will avoid creating multiple reservations during unrelease.      
                 IF l_mmt_rec.transaction_id = l_new_mmt_rec.source_line_id THEN
                    -- Bug 19868921 - Use rsrv_quantity for partial negative IB.
                    IF (NVL (g_debug, 0) = gme_debug.g_log_statement) THEN 
                       gme_debug.put_line('l_mmln_rec.count is ' || l_mmln_rec.count); 
                    END IF;
                    
                    IF l_mmln_rec.count > 0 THEN
                       IF l_mmln_rec.count = 1 THEN
                          IF (NVL (g_debug, 0) = gme_debug.g_log_statement) THEN 
                             gme_debug.put_line('LOT CONTROLLED');
                             gme_debug.put_line('l_mmln_rec(1).transaction_quantity is ' || l_mmln_rec(1).transaction_quantity); 
                             gme_debug.put_line('rsrv_quantity is ' || gme_common_pvt.g_mat_txn_hdr_tbl(cnt).rsrv_quantity); 
                             gme_debug.put_line('transaction_quantity is ' || l_mmln_rec(1).transaction_quantity); 
                          END IF;
                          
                          l_mmln_rec(1).transaction_quantity := NVL(gme_common_pvt.g_mat_txn_hdr_tbl(cnt).rsrv_quantity, 
                                                                 l_mmln_rec(1).transaction_quantity);
                                                                 
                          IF (gme_common_pvt.g_mat_txn_hdr_tbl(cnt).rsrv_quantity IS NOT NULL) THEN                                       
                             l_mmln_rec(1).secondary_transaction_quantity := NULL; -- force central code to rederive it.
                          END IF; 
                                                 
                          IF (NVL (g_debug, 0) = gme_debug.g_log_statement) THEN 
                             gme_debug.put_line('AFTER l_mmln_rec(1).transaction_quantity is ' || l_mmln_rec(1).transaction_quantity); 
                          END IF;
                       ELSE
                          -- Logic not in place to handle multiple lots for one transactions (aka lot/serial).
                          NULL;
                       END IF;                        
                    ELSE
                       IF (NVL (g_debug, 0) = gme_debug.g_log_statement) THEN 
                          gme_debug.put_line('NOT LOT CONTROLLED');
                          gme_debug.put_line('l_mmt_rec.transaction_quantity is ' || l_mmt_rec.transaction_quantity); 
                          gme_debug.put_line('rsrv_quantity is ' || gme_common_pvt.g_mat_txn_hdr_tbl(cnt).rsrv_quantity); 
                       END IF;
                       
                       l_mmt_rec.transaction_quantity := NVL(gme_common_pvt.g_mat_txn_hdr_tbl(cnt).rsrv_quantity, 
                                                             l_mmt_rec.transaction_quantity); 

                       IF (gme_common_pvt.g_mat_txn_hdr_tbl(cnt).rsrv_quantity IS NOT NULL) THEN                                       
                          l_mmt_rec.secondary_transaction_quantity := NULL; -- force central code to rederive it.
                       END IF;                        
                                                                              
                       IF (NVL (g_debug, 0) = gme_debug.g_log_statement) THEN 
                          gme_debug.put_line('AFTER l_mmt_rec.transaction_quantity is ' || l_mmt_rec.transaction_quantity); 
                       END IF;
                    END IF;                                               
                                                                               
                    create_resv_pplot (
                        p_material_detail_rec     => l_mat_dtl_rec
                       ,p_mmt_rec                 => l_mmt_rec
                       ,p_mmln_tbl                => l_mmln_rec
                       ,x_return_status           => x_return_status);
	              END IF;
              END IF;  -- IF l_mat_dtl_rec.line_type = gme_common_pvt.g_line_type_ing THEN
           END IF; 
        END LOOP;
     END IF;
   END create_matl_resv_pplot;

   PROCEDURE create_resv_pplot (
      p_material_detail_rec     IN       gme_material_details%ROWTYPE
     ,p_mmt_rec                 IN       mtl_material_transactions%ROWTYPE
     ,p_mmln_tbl                IN       gme_common_pvt.mtl_trans_lots_num_tbl
     ,x_return_status           OUT NOCOPY VARCHAR2)
   IS
      l_api_name   CONSTANT VARCHAR2 (30)           := 'create_resv_pplot';

      l_mmt_rec             mtl_material_transactions%ROWTYPE;
      l_mmln_tbl            gme_common_pvt.mtl_trans_lots_num_tbl;
      l_material_detail_rec gme_material_details%ROWTYPE;

      l_sequence            NUMBER;
      l_pplot_rec           gme_pending_product_lots%ROWTYPE;
      l_out_pplot_rec       gme_pending_product_lots%ROWTYPE;
      l_return_status       VARCHAR2(1);

   BEGIN
      IF NVL (g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'|| l_api_name);
      END IF;

      -- Set the return status to success initially
      x_return_status := fnd_api.g_ret_sts_success;

      l_mmt_rec := p_mmt_rec;
      l_mmln_tbl := p_mmln_tbl;
      l_material_detail_rec := p_material_detail_rec;

      IF l_material_detail_rec.line_type = gme_common_pvt.g_line_type_ing THEN
        l_material_detail_rec.revision := l_mmt_rec.revision;
        -- following loop will execute if this is lot control item
        FOR j in 1 .. l_mmln_tbl.count LOOP
          -- put following if condition in for negative IB, could be passing 0 quantity lots;
          IF l_mmln_tbl (j).transaction_quantity <> 0 THEN
             gme_reservations_pvt.create_material_reservation
                                   (p_matl_dtl_rec    => l_material_detail_rec
                                   ,p_resv_qty        => ABS(l_mmln_tbl (j).transaction_quantity)
                                   ,p_sec_resv_qty    => ABS(l_mmln_tbl (j).secondary_transaction_quantity)
                                   ,p_resv_um         => l_mmt_rec.transaction_uom
                                   ,p_subinventory    => l_mmt_rec.subinventory_code
                                   ,p_locator_id      => l_mmt_rec.locator_id
                                   ,p_lpn_id          => l_mmt_rec.lpn_id -- Bug 6437252
                                   ,p_lot_number      => l_mmln_tbl (j).lot_number
                                   ,x_return_status   => l_return_status);
             IF l_return_status <> fnd_api.g_ret_sts_success THEN
                x_return_status := l_return_status;
                -- don't error out if the reservation was not created... just set the return status
             END IF;
          END IF;
        END LOOP;
        -- following if block will execute if not lot control
        IF l_mmln_tbl.count = 0 THEN
           gme_reservations_pvt.create_material_reservation
                                   (p_matl_dtl_rec    => l_material_detail_rec
                                   ,p_resv_qty        => ABS(l_mmt_rec.transaction_quantity)
                                   ,p_sec_resv_qty    => ABS(l_mmt_rec.secondary_transaction_quantity)
                                   ,p_resv_um         => l_mmt_rec.transaction_uom
                                   ,p_subinventory    => l_mmt_rec.subinventory_code
                                   ,p_locator_id      => l_mmt_rec.locator_id
                                   ,p_lot_number      => NULL
                                   ,x_return_status   => l_return_status);
           IF l_return_status <> fnd_api.g_ret_sts_success THEN
              x_return_status := l_return_status;
              -- don't error out if the reservation was not created... just set the return status
           END IF;
        END IF;
      ELSE    -- product or by-product
        -- only need to recreate if this is lot control; not lot control; nothing to recreate
        -- also, if the transaction was in a different subinventory (then that on the material),
        -- that information will be lost, because the lots are being recreated, and when the
        -- transaction is constructed with these recreated pending product lots, the subinventory
        -- on the material will be used. (since pplots does not carry subinv)
        FOR j in 1 .. l_mmln_tbl.count LOOP
               l_pplot_rec.batch_id := l_material_detail_rec.batch_id;
               l_pplot_rec.material_detail_id := l_material_detail_rec.material_detail_id;
               -- don't pass sequence... let it be assigned
               l_pplot_rec.revision := l_mmt_rec.revision;
               l_pplot_rec.quantity := ABS(l_mmln_tbl (j).transaction_quantity);
               l_pplot_rec.secondary_quantity := l_mmln_tbl (j).secondary_transaction_quantity;
               l_pplot_rec.reason_id := l_mmln_tbl (j).reason_id;
               l_pplot_rec.lot_number := l_mmln_tbl (j).lot_number;

               l_sequence := gme_pending_product_lots_pvt.get_last_sequence
                        (p_matl_dtl_id      => l_pplot_rec.material_detail_id
                        ,x_return_status    => l_return_status);
               IF NVL (g_debug, -1) <= gme_debug.g_log_statement THEN
                  gme_debug.put_line (g_pkg_name || '.' || l_api_name||' return_status from lot_qty '|| l_mmln_tbl (j).transaction_quantity);
                  gme_debug.put_line (g_pkg_name || '.' || l_api_name||' return_status from get_sequence '||l_return_status);
               END IF;
               l_sequence := l_sequence + gme_pending_product_lots_pvt.g_sequence_increment;
               l_pplot_rec.sequence := l_sequence;

               gme_pending_product_lots_pvt.create_pending_product_lot
                   (p_pending_product_lots_rec   => l_pplot_rec
                   ,x_pending_product_lots_rec   => l_out_pplot_rec
                   ,x_return_status              => l_return_status);
               IF l_return_status <> fnd_api.g_ret_sts_success THEN
                  x_return_status := l_return_status;
                  -- don't error out if the reservation was not created... just set the return status
               END IF;
        END LOOP;
      END IF;  -- IF l_material_detail_rec.line_type = gme_common_pvt.g_line_type_ing

      IF NVL (g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name|| ' with return_status='||x_return_status);
      END IF;

   EXCEPTION
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
   END create_resv_pplot;

   PROCEDURE validate_batch_for_unrelease
               (p_batch_hdr_rec  IN gme_batch_header%ROWTYPE
               ,x_return_status  OUT NOCOPY VARCHAR2) IS

      l_api_name   CONSTANT VARCHAR2 (30)           := 'validate_batch_for_unrelease';

      CURSOR cur_is_step_status_valid (v_batch_id NUMBER) IS
      SELECT count(1)
      FROM   gme_batch_steps
      WHERE  step_status NOT IN (gme_common_pvt.g_step_pending, gme_common_pvt.g_step_wip)
      AND    batch_id = v_batch_id;

      l_is_step_status_valid      NUMBER;

      error_batch_type            EXCEPTION;
      error_batch_status          EXCEPTION;
      error_step_status           EXCEPTION;
      error_phantom               EXCEPTION;
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

      IF p_batch_hdr_rec.parentline_id IS NOT NULL THEN
        RAISE error_phantom;
      END IF;

      IF p_batch_hdr_rec.batch_status <> gme_common_pvt.g_batch_wip THEN
        RAISE error_batch_status;
      END IF;

      OPEN cur_is_step_status_valid(p_batch_hdr_rec.batch_id);
      FETCH cur_is_step_status_valid INTO l_is_step_status_valid;
      CLOSE cur_is_step_status_valid;

      IF l_is_step_status_valid > 0 THEN
        RAISE error_step_status;
      END IF;
      /*                                    
      BEGIN ER 19161894  Shaliu Chen 18-JUL-2014               
      Added for OPM Step Level OSP Project  
      If there is open PO/Req corresponding to the batch,
      throw an error out
      */        
      IF gme_osp.check_release_version THEN        
        IF gme_osp.is_OSP_batch(p_batch_id => p_batch_hdr_rec.batch_id) THEN

          IF gme_osp.po_req_exists(p_batch_id        => p_batch_hdr_rec.batch_id,
                                   p_organization_id => p_batch_hdr_rec.organization_id) THEN
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
      WHEN error_phantom THEN
        gme_common_pvt.log_message ('PM_INVALID_PHANTOM_ACTION');
        x_return_status := FND_API.G_RET_STS_ERROR;
      WHEN error_batch_type OR error_batch_status THEN
        gme_common_pvt.log_message('GME_API_INVALID_BATCH_UNREL');
        x_return_status := fnd_api.g_ret_sts_error;
      WHEN error_step_status THEN
        gme_common_pvt.log_message('GME_API_INVALID_STEP_UNREL');
        x_return_status := fnd_api.g_ret_sts_error;
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
   END validate_batch_for_unrelease;

   /* Bug 5021522 added function RETURNS TRUE if inv will go negative and org control does not allow it */
   FUNCTION check_inv_negative(p_mmt_rec            IN mtl_material_transactions%ROWTYPE,
                               p_mmln_tbl           IN gme_common_pvt.mtl_trans_lots_num_tbl,
                               p_org_neg_control    IN NUMBER DEFAULT gme_common_pvt.g_allow_neg_inv,
                               p_item_no            IN VARCHAR2) RETURN BOOLEAN IS
     l_api_name       CONSTANT VARCHAR2(30) := 'check_inv_negative';
     l_return_status  VARCHAR2(1);
     l_msg_data       VARCHAR2(2000);
     l_msg_cnt        NUMBER;
     l_qoh            NUMBER;
     l_rqoh           NUMBER;
     l_qr             NUMBER;
     l_qs             NUMBER;
     l_att            NUMBER;
     l_atr            NUMBER;
     l_sqoh           NUMBER;
     l_srqoh          NUMBER;
     l_sqr            NUMBER;
     l_sqs            NUMBER;
     l_satt           NUMBER;
     l_satr           NUMBER;
   BEGIN
     IF NVL (g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
        gme_debug.put_line ('Entering api ' || g_pkg_name || '.'|| l_api_name);
     END IF;
     
     IF (p_mmln_tbl.COUNT > 0) THEN
        FOR i IN 1..p_mmln_tbl.COUNT LOOP
           gme_transactions_pvt.query_quantities(x_return_status           => l_return_status,
                                                 x_msg_count               => l_msg_cnt,
                                                 x_msg_data                => l_msg_data,
                                                 p_organization_id         => p_mmt_rec.organization_id,
                                                 p_inventory_item_id       => p_mmt_rec.inventory_item_id,
                                                 p_tree_mode               => gme_common_pvt.g_tree_transaction_mode,
                                                 p_grade_code              => NULL,
                                                 p_revision                => p_mmt_rec.revision,
                                                 p_lot_number              => p_mmln_tbl(i).lot_number,
                                                 p_subinventory_code       => p_mmt_rec.subinventory_code,
                                                 p_locator_id              => p_mmt_rec.locator_id,
                                                 x_qoh                     => l_qoh,
                                                 x_rqoh                    => l_rqoh,
                                                 x_qr                      => l_qr,
                                                 x_qs                      => l_qs,
                                                 x_att                     => l_att,
                                                 x_atr                     => l_atr,
                                                 x_sqoh                    => l_sqoh,
                                                 x_srqoh                   => l_srqoh,
                                                 x_sqr                     => l_sqr,
                                                 x_sqs                     => l_sqs,
                                                 x_satt                    => l_satt,
                                                 x_satr                    => l_satr);
                                                 
           IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
              gme_debug.put_line('NVL(l_att,0) = '||NVL(l_att,0));
              gme_debug.put_line('NVL(ABS(p_mmln_tbl(i).primary_quantity),0) = '||NVL(ABS(p_mmln_tbl(i).primary_quantity),0));
              gme_debug.put_line('NVL(l_qr,0) = '||NVL(l_qr,0));
           END IF;
           
           IF (NVL(l_att,0) < NVL(ABS(p_mmln_tbl(i).primary_quantity),0)) THEN
              IF (p_org_neg_control = 2) THEN --org does not allow negative inventory
                 gme_common_pvt.log_message
                   ( p_message_code => 'GME_ITEM_NEG_INVENTORY'
                    ,p_product_code => 'GME'
                    ,p_token1_name  => 'ITEM_NO'
                    ,p_token1_value => p_item_no
                   );
                  
                 IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
                    gme_debug.put_line('error. onhand will be driven negative for lot '||p_mmln_tbl(i).lot_number||' which the org does not allow');
                 END IF;
               
                 RETURN TRUE;
               -- Bug 25883843 - Comment out the else path. This should be handled by inventory engine if it's a problem.
                                 
              /* ELSIF     (p_org_neg_control = 1)  --org allows negative inventory
                    AND (l_qr > 0) THEN
                 gme_common_pvt.log_message
                   ( p_message_code => 'GME_NEG_INV_WHEN_RSRVTNS_EXIST'
                    ,p_product_code => 'GME'
                    ,p_token1_name  => 'ITEM_NO'
                    ,p_token1_value => p_item_no
                   );
                   
                 IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
                    gme_debug.put_line('error. onhand will be driven negative for lot '||p_mmln_tbl(i).lot_number||' which org allows but there are reservations');
                 END IF;
                 RETURN TRUE; */
              END IF;
           END IF;
        END LOOP;
     ELSE
        gme_transactions_pvt.query_quantities(x_return_status           => l_return_status,
                                              x_msg_count               => l_msg_cnt,
                                              x_msg_data                => l_msg_data,
                                              p_organization_id         => p_mmt_rec.organization_id,
                                              p_inventory_item_id       => p_mmt_rec.inventory_item_id,
                                              p_tree_mode               => gme_common_pvt.g_tree_transaction_mode,
                                              p_grade_code              => NULL,
                                              p_revision                => p_mmt_rec.revision,
                                              p_lot_number              => NULL,
                                              p_subinventory_code       => p_mmt_rec.subinventory_code,
                                              p_locator_id              => p_mmt_rec.locator_id,
                                              x_qoh                     => l_qoh,
                                              x_rqoh                    => l_rqoh,
                                              x_qr                      => l_qr,
                                              x_qs                      => l_qs,
                                              x_att                     => l_att,
                                              x_atr                     => l_atr,
                                              x_sqoh                    => l_sqoh,
                                              x_srqoh                   => l_srqoh,
                                              x_sqr                     => l_sqr,
                                              x_sqs                     => l_sqs,
                                              x_satt                    => l_satt,
                                              x_satr                    => l_satr);
                                              
        IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
           gme_debug.put_line('NVL(l_att,0) = '||NVL(l_att,0));
           gme_debug.put_line('NVL(ABS(p_mmt_rec.primary_quantity),0) = '||NVL(ABS(p_mmt_rec.primary_quantity),0));
           gme_debug.put_line('NVL(l_qr,0) = '||NVL(l_qr,0));
        END IF;
        
        IF (NVL(l_att,0) < NVL(ABS(p_mmt_rec.primary_quantity),0)) THEN
           IF (p_org_neg_control = 2) THEN --org does not allow negative inventory
              gme_common_pvt.log_message
                ( p_message_code => 'GME_ITEM_NEG_INVENTORY'
                 ,p_product_code => 'GME'
                 ,p_token1_name  => 'ITEM_NO'
                 ,p_token1_value => p_item_no
                );
                
              IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
                 gme_debug.put_line('error. onhand will be driven negative which the org does not allow');
              END IF;
              RETURN TRUE;
              
           -- Bug 25883843 - Comment out the else path. This should be handled by inventory engine if it's a problem.
           /* ELSIF     (p_org_neg_control = 1)  --org  allows negative inventory
                 AND (l_qr > 0) THEN
              gme_common_pvt.log_message
                ( p_message_code => 'GME_NEG_INV_WHEN_RSRVTNS_EXIST'
                 ,p_product_code => 'GME'
                 ,p_token1_name  => 'ITEM_NO'
                 ,p_token1_value => p_item_no
                );
                
              IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
                 gme_debug.put_line('error. onhand will be driven negative which org allows but there are reservations');
              END IF;
              RETURN TRUE; */
           END IF;
        END IF;
     END IF;
     RETURN FALSE;
   EXCEPTION
     WHEN OTHERS THEN
       fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
       IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line('Unexpected error: '|| g_pkg_name|| '.'|| l_api_name|| ': '|| SQLERRM);
       END IF;
       RETURN FALSE;
   END check_inv_negative;

END gme_unrelease_batch_pvt;
/

COMMIT ;
-- show errors;
EXIT;
