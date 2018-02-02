/* +======================================================================+ */
/* |    Copyright (c) 2005, 2015 Oracle and/or its affiliates.           | */
/* |                         All rights reserved.                         | */
/* |                           Version 12.0.0                             | */
/* +======================================================================+ */
REM dbdrv: sql ~PROD ~PATH ~FILE none none none package &phase=plb \
REM dbdrv: checkfile(120.8.12010000.11=120.14.12020000.5)(120.7.12000000.11=120.8.12010000.9)(120.7.12000000.8=120.8.12010000.6)(120.7.12000000.6=120.8.12010000.4)(115.30=120.1):~PROD:~PATH:~FILE
SET VERIFY OFF
WHENEVER SQLERROR EXIT FAILURE ROLLBACK;
WHENEVER OSERROR EXIT FAILURE ROLLBACK;
REM *********************************************************************
REM *                                                                    *
REM * FILE:    GMEVCLBB.pls                                              *
REM * PURPOSE: Package Body for the GME batch close api                  *
REM * AUTHOR:  Bharati Satpute, OPM Development                          *
REM * DATE:    February 8th 2001                                         *
REM * HISTORY:                                                           *
REM * ========                                                           *
REM * 30-MAY-2006 Pawan Kumar               Bug 5256361                  *
REM *  Bypass dynamic cost allocation logic if total output is zero.      *
REM *  08-JUL-2008 Swapna K                 Bug 7208400
REM *   Added code to validate the batch close date in the check_close_date*
REM *    procedure.                                                        *
           
REM *  02-JUL-2009 G. Muratore              Bug 8636368
REM *   Enforce step dependency validation is not required when closing steps.
REM *   PROCEDURE: close_batch

REM *  04-OCT-2010 G. Muratore      Bug 10100973
REM *           Remove orphan move order line records if they exist. 
REM *   PROCEDURE: close_batch

REM *  15-FEB-2011 G. Muratore      Bug 10161288/11067065
REM *    Remove any remaining open move order line records if they exist. 
REM *    PROCEDURE:   close_batch

REM *  06-JUN-2011 G. Muratore      Bug 12546780 rework of 10161288
REM *    Do not remove invisible  move order line records if they exist.
REM *    This will be done in GMF code at the time the batch is final posted. 
REM *    PROCEDURE:   close_batch

REM *  01-JUN-2012 G. Muratore      Bug 14123348
REM *    Comment out code returning an error when ther is no real failure..
REM *    PROCEDURE:   fetch_batch_steps

REM *  23-FEB-2015 G. Muratore      Bug 20416843
REM *    Check of date should include all transactions, even reversals.
REM *    PROCEDURE:   check_close_date

REM *  12-MAR-2015 G. Muratore      Bug 19788419
REM *    Close invisible move order line records. They will be reopened if batch is reopened.
REM *    PROCEDURE:   close_batch
REM **********************************************************************

CREATE OR REPLACE PACKAGE BODY gme_close_batch_pvt AS
/* $Header: GMEVCLBB.pls 120.14.12020000.5 2015/03/12 15:16:43 gmurator ship $ */
   g_debug               VARCHAR2 (5)  := fnd_profile.VALUE ('AFLOG_LEVEL');
   g_pkg_name   CONSTANT VARCHAR2 (30) := 'GME_CLOSE_BATCH_PVT';

/*================================================================================
Procedure
  Close_Batch
Description
  This particular procedure call close the batch.
Parameters
  p_batch_header                The batch header row to identify the batch
  x_return_status               outcome of the API call
                                S - Success
                                E - Error
                                U - Unexpected error
History
  13JUN02  Chandrashekar Tiruvidula Bug 2377670
           Added call to function check_close_date to validate close date.
  16JUL02  Bharati Satpute    Bug2395188
           Added check for enforce step dependency
  08-AUG-2002 Shrikant Nene Bug# 2495557
           Close the "not yet closed" phantom batches.
           
  02-JUL-2009   G. Muratore    Bug 8636368
           Enforce step dependency validation is not required when closing steps.
           
  04-OCT-2010 G. Muratore      Bug 10100973
           Remove orphan move order line records if they exist. 
           
  15-FEB-2011 G. Muratore      Bug 10161288/11067065
           Remove any remaining open move order line records if they exist.
                      
  06-JUN-2011 G. Muratore      Bug 12546780 rework of 10161288
           Do not remove invisible  move order line records if they exist.
           This will be done in GMF code at the time the batch is final posted.                      
================================================================================*/
   PROCEDURE close_batch (
      p_batch_header_rec   IN              gme_batch_header%ROWTYPE
     ,x_batch_header_rec   OUT NOCOPY      gme_batch_header%ROWTYPE
     ,x_return_status      OUT NOCOPY      VARCHAR2)
   IS
      /* Miscellaneous */
      l_api_name        CONSTANT VARCHAR2 (30)               := 'close_batch';
      l_batch_status             NUMBER;
      l_back_flush               NUMBER;
      l_error_count              NUMBER;
      l_row_count                NUMBER;
      l_return_status            VARCHAR2 (1);
      l_ins_history              gme_batch_history%ROWTYPE;
      l_batch_step               gme_batch_steps%ROWTYPE;
      l_batch_steps_tab          gme_close_batch_pvt.step_details_tab;
      l_batch_header             gme_batch_header%ROWTYPE;
      l_in_batch_header          gme_batch_header%ROWTYPE;
      l_material_detail_ids      gme_common_pvt.number_tab;
      l_message_count            NUMBER;
      l_message_list             VARCHAR2 (2000);
      l_step_count               NUMBER;
      l_batch_header_rec         gme_batch_header%ROWTYPE;         

       /*siva FPbug#4684029*/
      l_gmf_cost_alloc_calc      NUMBER;
      l_total_plan_output        NUMBER;
      l_total_wip_plan_output    NUMBER;
      l_total_actual_output      NUMBER;
      l_uom			 VARCHAR2(3);
      l_qty			 NUMBER;

      marked_for_deletion        EXCEPTION;
      batch_header_upd_err       EXCEPTION;
      batch_lines_locked         EXCEPTION;
      batch_step_fetch_error     EXCEPTION;
      batch_step_close_error     EXCEPTION;
      invalid_step_status        EXCEPTION;
      invalid_batch_status       EXCEPTION;
      batch_header_fetch_error   EXCEPTION;
      dep_batch_step_fetch_err   EXCEPTION;
      batch_status_invalid       EXCEPTION;
      batch_status_closed        EXCEPTION;
      expected_error             EXCEPTION;
      unexpected_error           EXCEPTION;
      batch_hist_insert_err      EXCEPTION;
      close_phant_fail           EXCEPTION;
      batch_close_date_err       EXCEPTION;
      steps_not_closed           EXCEPTION;
      error_processing           EXCEPTION;    -- 4944024

      /*siva FPbug#4684029*/
      ERROR_IN_GET_TOTAL_QTY     EXCEPTION;
      MATERIAL_DETAIL_UPD_ERR	 EXCEPTION;

      -- Bug 5903208
      gmf_cost_failure           EXCEPTION;

      -- Bug 10100973
      l_material_details_tab     gme_common_pvt.material_details_tab;
      l_material_details_rec     gme_material_details%ROWTYPE;
      material_details_fetch_err EXCEPTION;

      -- Bug 10161288
      purge_exception_err        EXCEPTION;

      /* Get only the phantom ingredients which are not close already */
      CURSOR cur_get_phant
      IS
         SELECT phantom_id
           FROM gme_material_details d, gme_batch_header h
          WHERE d.batch_id = x_batch_header_rec.batch_id
            AND phantom_id IS NOT NULL
            AND h.batch_id = d.phantom_id
            AND h.batch_status <> 4;

      CURSOR cur_lock_material_lines (v_batch_id NUMBER)
      IS
         SELECT material_detail_id
                  FROM gme_material_details
                 WHERE batch_id = v_batch_id
         FOR UPDATE OF actual_qty NOWAIT;        
         
      CURSOR cur_gme_batch_steps
      IS
         SELECT COUNT (*)
           FROM gme_batch_steps
          WHERE step_status < 4 AND batch_id = x_batch_header_rec.batch_id;

      /* siva FPbug#4684029 */
      CURSOR cur_get_prod (v_batch_id NUMBER) 
      IS
        SELECT *
          FROM gme_material_details
         WHERE batch_id = v_batch_id
           AND line_type = 1;
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      /* Set the success staus to success inititally*/
      x_return_status := fnd_api.g_ret_sts_success;

      /*  Initialize output batch header*/
      IF NOT (gme_batch_header_dbl.fetch_row (p_batch_header_rec
                                             ,x_batch_header_rec) ) THEN
         RAISE batch_header_fetch_error;
      END IF;

      IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
         gme_debug.put_line (   'Closing batch '
                             || x_batch_header_rec.batch_no
                             || ' at '
                             || TO_CHAR (p_batch_header_rec.batch_close_date
                                        ,'DD-MON-YYYY HH24:MI:SS') );
      END IF;

      /*  Validate batch status, report error if batch is not certified */
      IF x_batch_header_rec.batch_status <> 3 THEN
         RAISE invalid_batch_status;
      END IF;

      /*  Report error that batch is marked for deletion */
      IF p_batch_header_rec.delete_mark = 1 THEN
         RAISE marked_for_deletion;
      END IF;

      -- 4944024 BEGIN    
      -- Delete any reservations against this batch as a supply source   
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
        RAISE error_processing;
      END IF;
      -- 4944024 END   


      -- Bug 8636368 - This following validation is not required when closing steps.
      /* Bharati Satpute Bug2395188 Added check for enforce step dependency */
      /* IF x_batch_header_rec.poc_ind = 'Y' THEN
         IF x_batch_header_rec.enforce_step_dependency = 1 THEN
            OPEN cur_gme_batch_steps;

            FETCH cur_gme_batch_steps
             INTO l_step_count;

            CLOSE cur_gme_batch_steps;

            IF l_step_count > 0 THEN
               RAISE steps_not_closed; */
            -- END IF;                                       /*l_step_count > 0*/
         -- END IF;                               /*enforce_step_dependency = 1*/
      -- END IF;                                                /*poc_ind = 'Y'*/

      /* Let us check if we have the lock for all the material lines */
      OPEN cur_lock_material_lines (x_batch_header_rec.batch_id);

      FETCH cur_lock_material_lines
      BULK COLLECT INTO l_material_detail_ids;

      IF SQLCODE = -54 THEN
         RAISE batch_lines_locked;
      END IF;

      CLOSE cur_lock_material_lines;

      -- Bug 10100973 - Let's remove Open orphan Move order records if they exist.
      l_material_details_rec.batch_id := x_batch_header_rec.batch_id;    
      IF NOT (gme_material_details_dbl.fetch_tab
                                 (p_material_detail      => l_material_details_rec
                                 ,x_material_detail      => l_material_details_tab) ) THEN
         RAISE material_details_fetch_err;
      END IF;

      FOR i IN 1 .. l_material_details_tab.COUNT LOOP     
         DELETE FROM mtl_txn_request_lines l
            WHERE organization_id = x_batch_header_rec.organization_id
              AND transaction_source_type_id = gme_common_pvt.g_txn_source_type
              AND txn_source_id = x_batch_header_rec.batch_id
              AND txn_source_line_id = l_material_details_tab(i).material_detail_id
              AND line_status = 7
              AND NOT EXISTS (SELECT 1
                              FROM mtl_txn_request_headers mtrh
                              WHERE mtrh.header_id = l.header_id)
              AND NOT EXISTS (SELECT 1
                              FROM mtl_material_transactions_temp mmtt
                              where l.line_id = mmtt.move_order_line_id);
      END LOOP;
      -- End Bug 10100973

      -- Bug 10161288 - Added following block to remove any open MO line.
      IF x_batch_header_rec.update_inventory_ind = 'Y' THEN
         IF (g_debug <= gme_debug.g_log_procedure) THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ':'
                                || 'calling purge exceptions for batch_id='
                                || x_batch_header_rec.batch_id);
         END IF;

         -- Bug 12546780 - Do not delete invisible move orders.
         -- Delete all move-orders excluding invisible move orders
         gme_cancel_batch_pvt.purge_batch_exceptions 
                                (p_batch_header_rec         => x_batch_header_rec
                                ,p_delete_reservations      => 'T'
                                ,x_return_status            => x_return_status);

         IF x_return_status <> fnd_api.g_ret_sts_success THEN
            RAISE purge_exception_err;
         END IF;
         
         -- BUG 19788419 - Let's close the invisible MO lines.
         UPDATE MTL_TXN_REQUEST_Lines 
         SET line_status = 5 
         WHERE line_id in 
           (SELECT d.move_order_line_id 
            FROM gme_material_details d
            WHERE d.batch_id = x_batch_header_rec.batch_id
              AND d.line_type = gme_common_pvt.g_line_type_ing);                                     
         
      END IF;

      -- BUG 11067065 - Let's close any remaining open putaway MO line.
      UPDATE MTL_TXN_REQUEST_Lines 
      SET line_status = 5 
      WHERE line_id in 
        (SELECT mtrl.line_id 
         FROM MTL_TXN_REQUEST_Lines mtrl, mtl_txn_request_headers mtrh
         WHERE mtrl.TRANSACTION_SOURCE_TYPE_ID = 5 
         AND mtrl.LINE_STATUS = 7 
         AND mtrl.LPN_ID IS NOT NULL 
         AND mtrh.header_id = mtrl.header_id 
         AND mtrh.ORGANIZATION_ID = mtrl.organization_id 
         AND mtrl.txn_source_id = x_batch_header_rec.batch_id 
         AND mtrl.ORGANIZATION_ID = x_batch_header_rec.organization_id
         AND mtrh.MOVE_ORDER_TYPE = 6); 
      
      /*siva FPbug#4684029 getting profile option value */
      l_gmf_cost_alloc_calc :=  FND_PROFILE.VALUE('GMF_COST_ALLOC_CALC');

      /* If batch close date is not provided use sysdate */
      IF p_batch_header_rec.batch_close_date IS NULL THEN
         x_batch_header_rec.batch_close_date := gme_common_pvt.g_timestamp;
      ELSE
         x_batch_header_rec.batch_close_date :=
                                          p_batch_header_rec.batch_close_date;
      END IF;

      /* Check batch close date is not less than step close or transaction dates */
      IF NOT (gme_close_batch_pvt.check_close_date (x_batch_header_rec) ) THEN
         RAISE batch_close_date_err;
      END IF;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Calling Batch Header DBL.Update Row ');
      END IF;

      /* Set up the fields in output structure. */
      x_batch_header_rec.batch_status := 4;

      /* Update the batch step to the database */
      IF NOT (gme_batch_header_dbl.update_row (x_batch_header_rec) ) THEN
         RAISE batch_header_upd_err;
      END IF;

      /* Insert the event into the batch history table */
      IF x_batch_header_rec.update_inventory_ind = 'Y' THEN
         IF g_debug <= gme_debug.g_log_procedure THEN
            gme_debug.put_line ('Calling Create History.');
         END IF;

         IF NOT gme_common_pvt.create_history
                                    (p_batch_header_rec      => x_batch_header_rec
                                    ,p_original_status       => 3) THEN
            RAISE batch_hist_insert_err;
         END IF;
/*    IF NOT create_history(x_batch_header_rec) THEN
      RAISE BATCH_HIST_INSERT_ERR;
    END IF;
*/
      END IF;

      /* Now we have to close any associated batches */
      FOR l_rec IN cur_get_phant LOOP
         l_batch_header.batch_id := l_rec.phantom_id;

         /*  Initialize batch header*/
         IF NOT (gme_batch_header_dbl.fetch_row (l_batch_header
                                                ,l_in_batch_header) ) THEN
            RAISE batch_header_fetch_error;
         END IF;

         /*  Close only completed phantom batches; can have cancelled phantom batches if the batch was terminated */
         IF l_in_batch_header.batch_status = 3 THEN
            /* x_batch_header_rec has either sysdate or the user passed-in date */
            l_in_batch_header.batch_close_date :=
                                          x_batch_header_rec.batch_close_date;
            gme_close_batch_pvt.close_batch
                                    (p_batch_header_rec      => l_in_batch_header
                                    ,x_batch_header_rec      => l_batch_header
                                    ,x_return_status         => l_return_status);

            IF l_return_status <> x_return_status THEN
               RAISE close_phant_fail;
            END IF;
         END IF;                                        /* batch_status = 3 */
      END LOOP;

      IF x_batch_header_rec.poc_ind = 'Y' THEN
         /* Now update the batch step status to close */
         gme_close_batch_pvt.fetch_batch_steps (x_batch_header_rec.batch_id
                                               ,NULL
                                               ,l_batch_steps_tab
                                               ,l_return_status);

         IF l_return_status <> x_return_status THEN
            RAISE dep_batch_step_fetch_err;
         END IF;

         FOR i IN 1 .. l_batch_steps_tab.COUNT LOOP
            /* Call Close step api to close the all steps */
            IF (l_batch_steps_tab (i).step_status = 3) THEN
               /* x_batch_header_rec has either sysdate or the user passed-in date */
               l_batch_steps_tab (i).step_close_date :=
                                          x_batch_header_rec.batch_close_date;
               gme_close_step_pvt.close_step
                                   (p_batch_step_rec      => l_batch_steps_tab
                                                                           (i)
                                   ,x_batch_step_rec      => l_batch_step
                                   ,x_return_status       => l_return_status);

               IF l_return_status <> x_return_status THEN
                  RAISE batch_step_close_error;
               END IF;
            END IF;
         END LOOP;
      END IF;

      /* Update the row who columns */
      x_batch_header_rec.last_update_date := gme_common_pvt.g_timestamp;
      x_batch_header_rec.last_updated_by := gme_common_pvt.g_user_ident;
      x_batch_header_rec.last_update_login := gme_common_pvt.g_login_id;

      /*siva FPBug#4684029 BEGIN
       calculate costs if dynamic allocation of cost is enabled through profile*/     
      IF l_gmf_cost_alloc_calc = 1 THEN
       /* get total plan and actual quantities if cost allocation is dynamic */
       gme_api_grp.get_total_qty(
  			        x_batch_header_rec.batch_id,
			        1,
			        NULL,
			        l_total_plan_output,
				l_total_wip_plan_output, --Bug#5111078 get_total_qty signature is changed
			        l_total_actual_output,
			        l_uom,
			        l_return_status
                               );
       IF l_return_status <> FND_API.G_RET_STS_SUCCESS THEN
        RAISE ERROR_IN_GET_TOTAL_QTY;
       ELSE    
       	-- Pawan Added for bug 5256361
       	 IF l_total_actual_output > 0 THEN        
           /* fetch product lines */
           FOR l_product_rec IN cur_get_prod (x_batch_header_rec.batch_id) LOOP
            IF l_product_rec.dtl_um <> l_uom THEN
              l_qty := inv_convert.inv_um_convert(
                                                 l_product_rec.inventory_item_id,
                                                 5,
                                                 l_product_rec.actual_qty,
                                                 l_product_rec.dtl_um,
                                                 l_uom,
	                                         NULL,
                                                 NULL);
            ELSE
              l_qty := l_product_rec.actual_qty;
            END IF;
            /* dynamic cost factor is ratio of actual qty to total product qty */
	    l_product_rec.cost_alloc := l_qty/l_total_actual_output;
           
            /* updating the material detail record with new cost factor*/
            IF NOT gme_material_details_dbl.update_row(l_product_rec) THEN
              RAISE MATERIAL_DETAIL_UPD_ERR;
            END IF;
           END LOOP;
         END IF;
       END IF;
     END IF;  /* profile condition IF*/

     /* FPBug#4684029 END */


     -- 
     -- Bug 5903208 - call to GMF
     -- 
     GMF_VIB.Finalize_VIB_Details
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

   EXCEPTION
      WHEN   gmf_cost_failure THEN
        -- Bug 5903208
        x_return_status := FND_API.G_RET_STS_ERROR;
    
      WHEN batch_header_fetch_error THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN invalid_batch_status THEN
         x_return_status := fnd_api.g_ret_sts_error;
         gme_common_pvt.log_message ('GME_INVALID_BATCH_STATUS'
                                    ,'PROCESS'
                                    ,'Close');
      WHEN batch_lines_locked OR app_exception.record_lock_exception THEN
         x_return_status := fnd_api.g_ret_sts_error;
         gme_common_pvt.log_message ('GME_API_BATCH_LINES_LOCKED');
      WHEN marked_for_deletion THEN
         x_return_status := fnd_api.g_ret_sts_error;
         gme_common_pvt.log_message ('GME_API_MARKED_FOR_DELETION');
      WHEN steps_not_closed THEN
         x_return_status := fnd_api.g_ret_sts_error;
         gme_common_pvt.log_message ('GME_API_STEPS_NOT_CLOSE');
      --FPBug#4684029 added MATERIAL_DETAIL_UPD_ERR exception
      WHEN batch_header_upd_err OR batch_close_date_err OR material_detail_upd_err THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN batch_hist_insert_err THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN dep_batch_step_fetch_err THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN batch_step_close_error THEN
         x_return_status := fnd_api.g_ret_sts_error;
      --FPBug#4684029
      WHEN error_in_get_total_qty THEN
         x_return_status := l_return_status;
      WHEN close_phant_fail THEN
         x_return_status := l_return_status;
      WHEN error_processing THEN    -- 4944024 BEGIN
         IF g_debug <= gme_debug.g_log_procedure THEN
            gme_debug.put_line ('Terminating due to error deleteing reservations against this batch supply.');
         END IF;    
         -- 4944024 END
      WHEN material_details_fetch_err THEN
         IF (g_debug <= gme_debug.g_log_procedure) THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ':'
                                || 'MATERIAL_FETCH_ERROR');
         END IF;
 
         x_return_status := fnd_api.g_ret_sts_error; 
      WHEN purge_exception_err THEN
         IF (g_debug <= gme_debug.g_log_procedure) THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ':'
                                || 'purge_exception_err');
         END IF;                 
      WHEN OTHERS THEN
         x_return_status := fnd_api.g_ret_sts_unexp_error;
         fnd_msg_pub.add_exc_msg ('GME_CLOSE_BATCH_PVT', 'CLOSE_BATCH');
   END close_batch;

/*===============================================================================
Function
  Create_History
Description
  This procedure is used to record an entry into the batch history table.

Parameters
  p_batch_header_rec            Batch header row
==================================================================================*/
   FUNCTION create_history (p_batch_header_rec IN gme_batch_header%ROWTYPE)
      RETURN BOOLEAN
   IS
      /* Local variable definitions */
      l_return_status       VARCHAR2 (1);
      /* Buffers for database reads/writes */
      l_ins_history         gme_batch_history%ROWTYPE;
      l_api_name   CONSTANT VARCHAR2 (30)               := 'CREATE_HISTORY';
   BEGIN
      l_ins_history.batch_id := p_batch_header_rec.batch_id;
      l_ins_history.orig_status := 3;
      l_ins_history.new_status := 4;
      /*
      l_ins_history.orig_wip_whse := p_batch_header_rec.wip_whse_code;
      l_ins_history.new_wip_whse := p_batch_header_rec.wip_whse_code;
      */
      l_ins_history.gl_posted_ind := 0;

      IF NOT (gme_batch_history_dbl.insert_row (l_ins_history, l_ins_history) ) THEN
         RETURN FALSE;
      ELSE
         RETURN TRUE;
      END IF;
   --Bug2804440
   EXCEPTION
      WHEN OTHERS THEN
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
         RETURN FALSE;
   --End Bug2804440
   END create_history;

/*===============================================================================
Function
  check_close_date
Description
  Created for BUG 2377670.
  This procedure is used to check the batch close date against all steps
  and transactions.

Parameters
  p_batch_header_rec            Batch header row
History

  23-FEB-2015 G. Muratore      Bug 20416843
           Check of date should include all transactions, even reversals.
==================================================================================*/
   FUNCTION check_close_date (p_batch_header_rec IN gme_batch_header%ROWTYPE)
      RETURN BOOLEAN
   IS
      CURSOR cur_batch_steps (v_batch_id NUMBER)
      IS
         SELECT step_close_date
           FROM gme_batch_steps
          WHERE batch_id IN (
                            SELECT DISTINCT batch_id
                                       FROM gme_material_details
                                 START WITH batch_id =
                                                  p_batch_header_rec.batch_id
                                 CONNECT BY batch_id = PRIOR phantom_id)
            AND NVL (step_close_date, p_batch_header_rec.batch_close_date) >
                                           p_batch_header_rec.batch_close_date;

      CURSOR cur_get_batches (v_batch_id NUMBER)
      IS
         SELECT DISTINCT batch_id
                    FROM gme_material_details
              START WITH batch_id = v_batch_id
              CONNECT BY batch_id = PRIOR phantom_id;

      x_batch_tbl           gme_common_pvt.number_tab;
      x_batch_row           gme_batch_header%ROWTYPE;
      x_mat_cnt             NUMBER;
      x_rsrc_cnt            NUMBER;
      x_status              VARCHAR2 (10);
      x_date                DATE;
      l_api_name   CONSTANT VARCHAR2 (30)              := 'CHECK_CLOSE_DATE';
      
      
      x_material_tbl gme_common_pvt.number_tab;

      CURSOR cur_rsrc_txns (v_batch_id NUMBER)
      IS
         SELECT trans_date
           FROM gme_resource_txns_gtmp
          WHERE doc_type = 'PROD'
            AND doc_id = v_batch_id
            AND completed_ind = 1
            AND delete_mark = 0
            AND action_code NOT IN ('DEL', 'REVS', 'REVL')
            AND NVL (trans_date, p_batch_header_rec.batch_close_date) >
                                           p_batch_header_rec.batch_close_date
             AND ROWNUM = 1;

    /*  CURSOR cur_inventory_txns (v_batch_id NUMBER)
      IS
         SELECT trans_date
           FROM gme_inventory_txns_gtmp
          WHERE doc_type = 'PROD'
            AND doc_id = v_batch_id
            AND completed_ind = 1
            AND transaction_no <> 2
            AND action_code NOT IN ('DELP', 'DELC')
            AND NVL (trans_date, p_batch_header_rec.batch_close_date) >
                                           p_batch_header_rec.batch_close_date; */
      /* Bug#7208400 The below two cursors are added to validate the batch close date
         against the transactions of the materials in a batch */
      CURSOR cur_get_materials(v_batch_id NUMBER)
      IS
        SELECT material_detail_id
        from gme_material_details
        WHERE batch_id = v_batch_id;
        
        
     -- Bug 20416843 - check of date should include all transactions, even reversals.
     CURSOR cur_inventory_txns (v_mat_det_id NUMBER, v_batch_id NUMBER,
                            v_txn_source_type NUMBER, v_pairs_reversal_type NUMBER)
      IS
      SELECT transaction_date
           FROM mtl_material_transactions mmt
          WHERE trx_source_line_id = v_mat_det_id
            AND transaction_source_id = v_batch_id
            AND transaction_source_type_id = v_txn_source_type
            -- AND NOT EXISTS ( SELECT /*+ no_unnest */
            --             transaction_id1
            --          FROM gme_transaction_pairs
            --         WHERE transaction_id1 = mmt.transaction_id
            --           AND pair_type = v_pairs_reversal_type)
          AND NVL (transaction_date, p_batch_header_rec.batch_close_date) >
                                           p_batch_header_rec.batch_close_date
          AND ROWNUM = 1;       

      CURSOR cur_batch_cmplt_date (v_batch_id NUMBER)
      IS
         SELECT actual_cmplt_date
           FROM gme_batch_header
          WHERE batch_id IN (
                             SELECT DISTINCT batch_id
                                        FROM gme_material_details
                                  START WITH batch_id =
                                                   p_batch_header_rec.batch_id
                                  CONNECT BY batch_id = PRIOR phantom_id)
            AND NVL (actual_cmplt_date, p_batch_header_rec.batch_close_date) >
                                           p_batch_header_rec.batch_close_date;
   BEGIN
      /* Check if batch close date is greater than all batch step close dates */
      OPEN cur_batch_steps (p_batch_header_rec.batch_id);

      FETCH cur_batch_steps
       INTO x_date;

      IF (cur_batch_steps%FOUND) THEN
         CLOSE cur_batch_steps;

         gme_common_pvt.log_message ('GME_CLOSE_STEP_DATE_ERR'
                                    ,'D1'
                                    ,fnd_date.date_to_displaydt (x_date) );
         RETURN FALSE;
      END IF;

      CLOSE cur_batch_steps;

      IF (p_batch_header_rec.update_inventory_ind = 'Y') THEN
         OPEN cur_get_batches (p_batch_header_rec.batch_id);

         FETCH cur_get_batches
         BULK COLLECT INTO x_batch_tbl;

         CLOSE cur_get_batches;

         FOR i IN 1 .. x_batch_tbl.COUNT LOOP
            x_batch_row.batch_id := x_batch_tbl (i);

            IF NOT gme_batch_header_dbl.fetch_row (x_batch_row, x_batch_row) THEN
               RETURN FALSE;
            END IF;

            gme_trans_engine_util.load_rsrc_trans
                                               (p_batch_row          => x_batch_row
                                               ,x_rsc_row_count      => x_rsrc_cnt
                                               ,x_return_status      => x_status);

            IF (NVL (x_rsrc_cnt, 0) > 0) THEN
               /* Check if the batch close date is greater than all completed resource transaction dates */
               OPEN cur_rsrc_txns (x_batch_tbl (i) );

               FETCH cur_rsrc_txns
                INTO x_date;

               IF (cur_rsrc_txns%FOUND) THEN
                  CLOSE cur_rsrc_txns;

                  gme_common_pvt.log_message
                                         ('GME_CLOSE_RSRC_DATE_ERR'
                                         ,'D1'
                                         ,fnd_date.date_to_displaydt (x_date) );
                  RETURN FALSE;
               END IF;

               CLOSE cur_rsrc_txns;
            END IF;
           /*bug#7208400 start */
           OPEN cur_get_materials (x_batch_row.batch_id);

           FETCH cur_get_materials
           BULK COLLECT INTO x_material_tbl;

           CLOSE cur_get_materials;
           
           FOR j IN 1 .. x_material_tbl.COUNT LOOP
  /* Check if the batch close date is greater than all completed inventory transaction dates */
               OPEN cur_inventory_txns (x_material_tbl (j),x_batch_tbl (i),gme_common_pvt.g_txn_source_type, gme_common_pvt.g_pairs_reversal_type );

               FETCH cur_inventory_txns
                INTO x_date;
               IF (cur_inventory_txns%FOUND) THEN
                  CLOSE cur_inventory_txns;
                 gme_common_pvt.log_message('GME_CLOSE_INVEN_DATE_ERR', 'D1', fnd_date.date_to_displayDT(X_date));
                 RETURN FALSE;
               END IF;

               CLOSE cur_inventory_txns;
           END LOOP;
         /*bug#7208400 End */
         END LOOP;
      END IF;


      /* Check if batch close date is greater than all batch complete dates */
      OPEN cur_batch_cmplt_date (p_batch_header_rec.batch_id);

      FETCH cur_batch_cmplt_date
       INTO x_date;

      IF (cur_batch_cmplt_date%FOUND) THEN
         CLOSE cur_batch_cmplt_date;

         gme_common_pvt.log_message ('GME_CLOSE_CMPLT_DATE_ERR'
                                    ,'D1'
                                    ,fnd_date.date_to_displaydt (x_date) );
         RETURN FALSE;
      END IF;

      CLOSE cur_batch_cmplt_date;

      RETURN TRUE;
   EXCEPTION
      WHEN OTHERS THEN
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
         RETURN FALSE;
END check_close_date;

   PROCEDURE fetch_batch_steps (
      p_batch_id        IN              NUMBER
     ,p_batchstep_id    IN              NUMBER
     ,x_step_tbl        OUT NOCOPY      step_details_tab
     ,x_return_status   OUT NOCOPY      VARCHAR2)
   IS
      /* Local variables.
      ==================*/
      l_num_steps             NUMBER                    := 0;
      l_routing_id            NUMBER;
      l_step_id               NUMBER;

      /* Cursor Definitions.
      =====================*/
      CURSOR cur_get_routing (v_batch_id NUMBER)
      IS
         SELECT routing_id
           FROM gme_batch_header
          WHERE batch_id = v_batch_id;

      CURSOR cur_get_steps (v_batch_id NUMBER, v_batchstep_id NUMBER)
      IS
         SELECT     d.dep_step_id
               FROM gme_batch_step_dependencies d
              WHERE d.batch_id = v_batch_id
         START WITH (     (d.batch_id = v_batch_id)
                     AND (    (v_batchstep_id IS NULL)
                          OR (batchstep_id = v_batchstep_id) ) )
         CONNECT BY d.batch_id = PRIOR d.batch_id
                AND d.batchstep_id = PRIOR d.dep_step_id
           GROUP BY d.dep_step_id
           ORDER BY MAX (LEVEL) DESC;

      CURSOR cur_get_step_rec (v_batch_id NUMBER, v_step_id NUMBER)
      IS
         SELECT *
           FROM gme_batch_steps
          WHERE batch_id = v_batch_id AND batchstep_id = v_step_id;

      CURSOR cur_get_final_steps (v_batch_id NUMBER)
      IS
         SELECT   *
             FROM gme_batch_steps s
            WHERE s.batch_id = v_batch_id
              AND s.batchstep_id NOT IN (SELECT dep_step_id
                                           FROM gme_batch_step_dependencies
                                          WHERE batch_id = v_batch_id)
         ORDER BY batchstep_no;

      /* Buffer records for database read.
      ===================================*/
      l_step_rec              gme_batch_steps%ROWTYPE;
      /* Exceptions.
      =====================*/
      no_routing_associated   EXCEPTION;
      step_details_missing    EXCEPTION;
      circular_reference      EXCEPTION;
      PRAGMA EXCEPTION_INIT (circular_reference, -01436);
   BEGIN
      x_return_status := fnd_api.g_ret_sts_success;
      
      /*Added by Shalchen 02/28/2013
        Add IF condition to bypass routing validation for batch open interface project*/
      IF NVL(gme_common_pvt.g_bulk_validation_done,'N') = 'N' THEN  
        /* Fetch the routing for the batch passed in */
        OPEN cur_get_routing (p_batch_id);

        FETCH cur_get_routing
         INTO l_routing_id;

        IF cur_get_routing%NOTFOUND THEN
           CLOSE cur_get_routing;

           RAISE no_routing_associated;
        END IF;

        CLOSE cur_get_routing;
      END IF;
      /* Get the routing steps from the PM dependency table */
      OPEN cur_get_steps (p_batch_id, p_batchstep_id);

      FETCH cur_get_steps
       INTO l_step_id;

      /* Add the steps to the pl/sql table */
      WHILE cur_get_steps%FOUND LOOP
         l_num_steps := l_num_steps + 1;

         /* Get the step details */
         OPEN cur_get_step_rec (p_batch_id, l_step_id);

         FETCH cur_get_step_rec
          INTO l_step_rec;

         CLOSE cur_get_step_rec;

         x_step_tbl (l_num_steps) := l_step_rec;

         FETCH cur_get_steps
          INTO l_step_id;
      END LOOP;                                /* WHILE Cur_get_steps%FOUND */

      CLOSE cur_get_steps;

      -- Bug 14123348 - This block is here to fetch any steps which have no dependencies.
      -- If there aren't any it's ok...  It is not a failure.
      
      /* Populate the pl/sql table with the final steps based on the dependencies */
      /* only if it is being called for the entire batch */
      IF p_batchstep_id IS NULL THEN
         OPEN cur_get_final_steps (p_batch_id);

         FETCH cur_get_final_steps
          INTO l_step_rec;

         IF cur_get_final_steps%FOUND THEN
            WHILE cur_get_final_steps%FOUND LOOP
               l_num_steps := l_num_steps + 1;
               x_step_tbl (l_num_steps) := l_step_rec;

               FETCH cur_get_final_steps
                INTO l_step_rec;
            END LOOP;
         -- Bug 14123348 - Comment out else path as this is not really a failure.
         -- ELSE
            -- CLOSE cur_get_final_steps;

            -- RAISE step_details_missing;
         END IF;

         CLOSE cur_get_final_steps;
      END IF;                                  /* IF p_batchstep_id IS NULL */
   EXCEPTION
      WHEN no_routing_associated THEN
         x_return_status := fnd_api.g_ret_sts_error;
         fnd_message.set_name ('GMD', 'GMD_NO_ROUTING_ASSOCIATED');
         fnd_msg_pub.ADD;
      WHEN step_details_missing THEN
         x_return_status := fnd_api.g_ret_sts_error;
         fnd_message.set_name ('GMD', 'GME_STEP_DETAILS_MISSING');
         fnd_msg_pub.ADD;
      WHEN circular_reference THEN
         fnd_message.set_name ('GMD', 'GMD_CIRCULAR_DEPEN_DETECT');
         fnd_msg_pub.ADD;
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN OTHERS THEN
         x_return_status := fnd_api.g_ret_sts_unexp_error;
         fnd_message.set_name ('GMD', 'GMD_UNEXPECTED_ERROR');
         fnd_message.set_token ('ERROR', SQLERRM);
         fnd_msg_pub.ADD;
   END fetch_batch_steps;
END gme_close_batch_pvt;
/

COMMIT ;
EXIT;
--show error;
