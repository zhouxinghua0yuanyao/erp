/* +======================================================================+ */
/* |    Copyright (c) 2005, 2016 Oracle and/or its affiliates.           | */
/* |                         All rights reserved.                         | */
/* |                           Version 12.0.0                             | */
/* +======================================================================+ */
REM dbdrv: sql ~PROD ~PATH ~FILE none none none package &phase=plb \
REM dbdrv: checkfile(120.8.12010000.6=120.10.12020000.4)(120.8.12000000.4=120.8.12010000.4)(120.8.12000000.3=120.10):~PROD:~PATH:~FILE 
SET VERIFY OFF
WHENEVER SQLERROR EXIT FAILURE ROLLBACK;
WHENEVER OSERROR EXIT FAILURE ROLLBACK;
REM **********************************************************************
REM *                                                                    *
REM * FILE:    GMEVRWBB.pls                                              *
REM * PURPOSE: Package Body for the GME REVERT BATCH routines         	 *
REM * AUTHOR:  Pawan Kumar, OPM Development                            	 *
REM * DATE:    May 19th 2005                                             *
REM * HISTORY:                                                           *
REM * ========                                                           *

REM * G. Muratore    23-Jun-2010 Bug 11855868 / 11846735
REM *   Do not pass in event id to create history. This issue was found while
REM *   debugging 11846735. g_transaction_header_id value was too large and
REM *   it really did not get used anyway.
REM *   PROCEDURE: revert_batch

REM * QZENG          22-MAY-2015 Bug 21101876
REM *   Modified in revert_line, not to revert the phantom batch when it
REM *   has been hold.

REM * G. Muratore    03-NOV-2016 Bug 23640627 
REM *   Introduced new p_create_resv_pend_lots parameter.
REM *   PROCEDURE: revert_step and revert_line.
REM **********************************************************************

/*************************************************************************
* This file contains the procedure for reverting batches  to WIP state   *
*in Oracle Process Manufacturing (OPM. Each procedure has a common set of*
* parameters to which API-specific parameters are appended.              *
*************************************************************************/

CREATE OR REPLACE PACKAGE BODY gme_revert_batch_pvt AS
/* $Header: GMEVRWBB.pls 120.10.12020000.4 2016/11/07 16:52:42 gmurator ship $ */

G_DEBUG VARCHAR2(5) := FND_PROFILE.VALUE('AFLOG_LEVEL');

/* Global Variables */
G_table_name	VARCHAR2(80) DEFAULT 'GME_MATERIAL_DETAILS';
g_pkg_name   CONSTANT VARCHAR2(30)  := 'GME_REVERT_BATCH';
 
/*===========================================================================================
Procedure
  revert_batch
Description
  This particular procedure handles revert batch to WIP.
Parameters
  p_batch_header		The batch header row to identify the batch
  x_return_status		outcome of the API call
  				S - Success
  				E - Error
  				U - Unexpected error  
=============================================================================================*/

PROCEDURE revert_batch
(p_batch_header_rec      IN  GME_BATCH_HEADER%ROWTYPE
,x_batch_header_rec      OUT NOCOPY GME_BATCH_HEADER%ROWTYPE
,p_create_resv_pend_lots IN  NUMBER    -- Bug 23640627
,x_return_status         OUT NOCOPY VARCHAR2) IS

  CURSOR Cur_lock_batch_materials (v_batch_id NUMBER) IS
    SELECT *
    FROM   gme_material_details
    WHERE  batch_id = v_batch_id
    FOR UPDATE OF actual_qty NOWAIT;
    
   CURSOR Cur_revert_materials (v_batch_id NUMBER) IS
    SELECT *
    FROM   gme_material_details
    WHERE  batch_id = v_batch_id
    AND ((line_type = -1 AND phantom_id IS NOT NULL) OR  
             (line_type <> -1  AND phantom_line_id IS NULL ))
     ORDER BY line_no ;
  
  /* Buffers for database reads/writes */
  
  l_batch_header_rec		   gme_batch_header%ROWTYPE;
  l_material_details_tab	gme_common_pvt.material_details_tab ;
  
  /* Exception definitions */
  BATCH_LINES_LOCKED		        EXCEPTION;
  MATERIAL_DETAIL_FETCH_ERROR   EXCEPTION;
 
  BATCH_HIST_INSERT_ERR		     EXCEPTION;
  BATCH_HDR_UPDATE_ERROR  	     EXCEPTION;
  REVERT_LINE_FAIL		        EXCEPTION;
     
  /* Local variables */
  l_return_status	   VARCHAR2(1);
  l_count		      NUMBER(5) DEFAULT 0;
  l_inv_trans_count	NUMBER;
  l_rsrc_trans_count	NUMBER;
  l_message_count	   NUMBER;
  l_message_list	   VARCHAR2(2000);
  
  l_api_name         CONSTANT VARCHAR2 (30)   := 'revert_batch';
BEGIN
  /* Set the return status to success initially */
  x_return_status := FND_API.G_RET_STS_SUCCESS;
  
  IF (g_debug <= gme_debug.g_log_statement) THEN
         gme_debug.put_line (g_pkg_name || '.' || l_api_name || ':'
                             || 'Entering');
  END IF;
  
  l_batch_header_rec := p_batch_header_rec;
  
  /* Lock all the material lines associated with the batch */
  OPEN Cur_lock_batch_materials(l_batch_header_rec.batch_id);
  FETCH Cur_lock_batch_materials BULK COLLECT INTO l_material_details_tab;
  IF sqlcode = -54 THEN
     CLOSE Cur_lock_batch_materials;
     RAISE BATCH_LINES_LOCKED;
  END IF;
  CLOSE Cur_lock_batch_materials;
 
  /* Mark batch as 'WIP' */
  l_batch_header_rec.batch_status := 2;
  gme_common_pvt.g_batch_status_check := fnd_api.g_false;
  
  /* Fetch all the material lines associated with the batch */
  FOR i IN 1..l_material_details_tab.COUNT LOOP
     IF (g_debug <= gme_debug.g_log_statement) THEN
         gme_debug.put_line (g_pkg_name || '.' || l_api_name || ':'
                             || 'material line count'||l_material_details_tab.COUNT );
     END IF;  
     
     /* We should not revert the phantom product, so added the check below to see   */
     /* if the current line being process is a phantom product, if it is not a phantom */
     /* product then we add it to the queue of lines for revert                  */
     IF 
      ((l_material_details_tab(i).line_type = -1 
         AND l_material_details_tab(i).phantom_id IS NOT NULL)
       OR  
          (l_material_details_tab(i).line_type <> -1 
          AND l_material_details_tab(i).phantom_line_id IS NULL 
          AND (gme_common_pvt.is_material_auto_release
          (l_material_details_tab(i).material_detail_id) = 0 )
         )) THEN
         
        IF (g_debug <= gme_debug.g_log_statement) THEN
           gme_debug.put_line (g_pkg_name || '.' || l_api_name || ':'
                             || ' calling revert line for:'||l_material_details_tab(i).material_detail_id );
        END IF;
          
        revert_line 
		  (p_batch_header_rec    	=> l_batch_header_rec
		  ,p_material_details_rec  => l_material_details_tab(i)
		  ,p_batch_step_rec        => NULL
        ,p_create_resv_pend_lots => p_create_resv_pend_lots  -- Bug 23640627
		  ,x_return_status   	   => l_return_status);
		  
        IF (g_debug <= gme_debug.g_log_statement) THEN
           gme_debug.put_line (g_pkg_name || '.' || l_api_name || ':'
                               || 'return status from revert line:'||l_return_status);
        END IF;
          	
        IF l_return_status <> x_return_status THEN
           RAISE REVERT_LINE_FAIL;
        END IF;	      
     END IF;     
  END LOOP;

  IF l_batch_header_rec.update_inventory_ind = 'Y'
  THEN
     -- Bug 11846735 - Do not pass in a value for event_id.
     IF NOT gme_common_pvt.create_history (l_batch_header_rec, p_batch_header_rec.batch_status)
                                           -- , gme_common_pvt.g_transaction_header_id)
        THEN
        RAISE batch_hist_insert_err;
     END IF;
  END IF;

  /* Remove the actual completion date of the batch */
  l_batch_header_rec.actual_cmplt_date := NULL;
  
  /* Update the batch header row to the database */
  IF NOT GME_BATCH_HEADER_DBL.update_row(l_batch_header_rec) THEN
     RAISE BATCH_HDR_UPDATE_ERROR;
  END IF;
  
  x_batch_header_rec := l_batch_header_rec ;
  
  /* Update the row who columns */
  x_batch_header_rec.last_update_date  := gme_common_pvt.g_timestamp;
  x_batch_header_rec.last_updated_by   := gme_common_pvt.g_user_ident;
  x_batch_header_rec.last_update_login := gme_common_pvt.g_login_id;

EXCEPTION  
  
  WHEN BATCH_LINES_LOCKED THEN
    x_return_status := FND_API.G_RET_STS_ERROR;  
    gme_common_pvt.log_message('GME_API_BATCH_LINES_LOCKED');        
  
  WHEN  REVERT_LINE_FAIL THEN
    x_return_status := l_return_status;  
         
  WHEN BATCH_HIST_INSERT_ERR THEN
    x_return_status := FND_API.G_RET_STS_ERROR;  
  WHEN BATCH_HDR_UPDATE_ERROR THEN
    x_return_status := FND_API.G_RET_STS_ERROR;
  WHEN app_exception.record_lock_exception THEN
    x_return_status := FND_API.G_RET_STS_ERROR;
    gme_common_pvt.log_message('GME_RECORD_LOCKED','TABLE_NAME', g_table_name, 'KEY',to_char(p_batch_header_rec.batch_id));
  WHEN OTHERS THEN
     x_return_status := fnd_api.g_ret_sts_unexp_error;

         IF (NVL (g_debug, -1) = gme_debug.g_log_unexpected) THEN
            gme_debug.put_line (g_pkg_name || '.' || l_api_name || ':' || 'WHEN OTHERS:' || SQLERRM);
         END IF;

         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);     
END revert_batch;
  
  
/*===========================================================================================
Procedure
  revert_line
Description
  This procedure traverses the list of Transactions for the Material
  Detail line and reverses the completed transactions and posts a pending transaction

Parameters
  p_batch_header		Batch header row
  p_material_detail		Material line detail record
  x_return_status		outcome of the API call
  				S - Success
  				E - Error
  				U - Unexpected error  
=============================================================================================*/

PROCEDURE revert_line 
(p_batch_header_rec      IN GME_BATCH_HEADER%ROWTYPE
,p_material_details_rec  IN gme_material_details%ROWTYPE
,p_batch_step_rec        IN gme_batch_steps%ROWTYPE
,p_create_resv_pend_lots IN  NUMBER    -- Bug 23640627
,x_return_status 	       OUT NOCOPY VARCHAR2)IS

  /* Local variable definitions */
  l_return_status 	         VARCHAR2(1);
  l_step_id                   NUMBER;
  l_step_date                 DATE;
  l_api_name                  CONSTANT VARCHAR2 (30)   := 'revert_line';
  l_rel_type                  NUMBER;
  l_batch_header_rec		      gme_batch_header%ROWTYPE;
  l_material_details_rec	   gme_material_details%ROWTYPE;
  l_ph_batch_header_rec		   gme_batch_header%ROWTYPE; 
  l_batch_step_rec            gme_batch_steps%ROWTYPE; 
  l_in_batch_step_rec         gme_batch_steps%ROWTYPE; 
  l_rsrc_trans_count          NUMBER;
  l_message_count		         NUMBER;
  l_message_list		         VARCHAR2(2000);
  l_exception_material_tbl    gme_common_pvt.exceptions_tab;
  l_actual_qty    		      NUMBER;
  
  -- Bug 21101876, Added by QZENG, for onhold
  l_batch_no                  gme_batch_header.batch_no%TYPE;
  
  CURSOR Cur_get_step (V_material_detail_id NUMBER) IS
    SELECT batchstep_id
    FROM   gme_batch_step_items
    WHERE  material_detail_id = V_material_detail_id;
    
  /* Exception definition */
  UPDATE_STEP_QTY_ERROR		   EXCEPTION;
  ERROR_UPD_MATERIAL_DETAIL	EXCEPTION;  
  ERROR_LOAD_TRANS		      EXCEPTION;
  BATCH_HEADER_FETCH_ERROR	   EXCEPTION;
  REVERT_PHANT_FAIL		      EXCEPTION;
  REVERT_MATL_ERROR		      EXCEPTION;
  
  -- Bug 21101876, Added by QZENG, for onhold
  error_phantom_batch_hold    EXCEPTION;
  
  /* Bug 5001915 Get item control for transactions */
  CURSOR Cur_get_item(V_org_id            NUMBER,
                      V_inventory_item_id NUMBER) IS
    SELECT mtl_transactions_enabled_flag
    FROM   mtl_system_items_b
    WHERE  organization_id = V_org_id
           AND inventory_item_id = V_inventory_item_id;
           
  l_txn_enabled_flag          VARCHAR2(1);
BEGIN

  /* Inititialize the return status to success */
  x_return_status := FND_API.G_RET_STS_SUCCESS;
  IF (g_debug <= gme_debug.g_log_statement) THEN
     gme_debug.put_line (g_pkg_name || '.' || l_api_name || ':'
                             || 'Entering');
  END IF;
  
  IF p_material_details_rec.line_type = -1 THEN
     l_ph_batch_header_rec.batch_id := p_material_details_rec.phantom_id  ;
     
     /* Initialize output batch header */
     IF NOT (GME_BATCH_HEADER_DBL.fetch_row(l_ph_batch_header_rec, l_ph_batch_header_rec)) THEN
        RAISE BATCH_HEADER_FETCH_ERROR;
     END IF;
     
     -- revert_batch for phantom batch
     IF (g_debug <= gme_debug.g_log_statement) THEN
         gme_debug.put_line (g_pkg_name || '.' || l_api_name || ':'
                             || 'calling for phantom:'||l_ph_batch_header_rec.batch_id );
     END IF;
     
     -- Bug 21101876, Added by QZENG, check batch hold type for the phantm
     -- batch
     IF gme_common_pvt.GET_BATCH_HOLD_STATUS(p_material_details_rec.phantom_id) <> 'R' THEN
        l_batch_no := l_ph_batch_header_rec.batch_no;
        raise error_phantom_batch_hold;
     END IF;
     -- End Bug 21101876
     
     revert_batch(p_batch_header_rec      => l_ph_batch_header_rec
                 ,x_batch_header_rec     	=> l_batch_header_rec
                 ,p_create_resv_pend_lots => p_create_resv_pend_lots  -- Bug 23640627
                 ,x_return_status     	   => x_return_status);
      
     IF x_return_status <> FND_API.G_RET_STS_SUCCESS THEN
        RAISE REVERT_PHANT_FAIL;
     END IF;
  ELSE
     l_batch_header_rec := p_batch_header_rec ;
     l_material_details_rec := p_material_details_rec ;
     IF l_batch_header_rec.update_inventory_ind = 'Y' THEN
        IF (g_debug <= gme_debug.g_log_statement) THEN
           gme_debug.put_line (g_pkg_name || '.' || l_api_name || ':'
                             || 'calling revert mat:'||p_material_details_rec.material_detail_id);
        END IF;
        
        /* Bug 5001915 get values for item control and added if condition before calling revert mtl */
        OPEN Cur_get_item(p_material_details_rec.organization_id, p_material_details_rec.inventory_item_id);
        FETCH Cur_get_item INTO l_txn_enabled_flag;
        CLOSE Cur_get_item;
        
        IF l_txn_enabled_flag = 'Y' THEN
      	  gme_unrelease_batch_pvt.revert_material_full
      	   (p_material_detail_rec 	      => p_material_details_rec 
      	   -- ,p_create_resv_pend_lots 	=> 1
      	   ,p_create_resv_pend_lots 	   => p_create_resv_pend_lots -- Bug 23640627
      	   ,x_actual_qty                 => l_actual_qty   
      	   ,x_exception_material_tbl     => l_exception_material_tbl
      	   ,x_return_status 		         => x_return_status );
      	   
      	  IF x_return_status <> FND_API.G_RET_STS_SUCCESS THEN
              RAISE REVERT_MATL_ERROR;
           END IF;
        END IF;
        
        /* Default the actual qty to zero */
        l_material_details_rec.actual_qty := 0;

        /* Update the material detail row to the database */
        IF NOT GME_MATERIAL_DETAILS_DBL.update_row (l_material_details_rec) THEN
           RAISE ERROR_UPD_MATERIAL_DETAIL;
        END IF;
        
        IF l_batch_header_rec.POC_IND= 'Y'  THEN
           IF l_batch_header_rec.automatic_step_calculation = 1 THEN
              OPEN Cur_get_step (l_material_details_rec.material_detail_id);
              FETCH Cur_get_step INTO l_batch_step_rec.batchstep_id;
              CLOSE Cur_get_step;
              IF l_batch_step_rec.batchstep_id IS NOT NULL  THEN
                 /* Invoke the update step qty API to update the step quantities and the */
                 /* quantities of the succeeding steps  */
                 
                 
                 GME_TRANS_ENGINE_UTIL.load_rsrc_trans (l_batch_header_rec
                                                       ,l_rsrc_trans_count
                                                       ,l_return_status);
                 IF l_return_status <> x_return_status THEN
                    RAISE ERROR_LOAD_TRANS;
                 END IF;
                 
                 /* Bug 5021522 Added l_in_batch_step_rec new rec type because of NOCOPY */
                 l_in_batch_step_rec := l_batch_step_rec;
                 GME_UPDATE_STEP_QTY_pvt.update_step_qty (P_batch_step_rec  => l_in_batch_step_rec
                                                         ,x_message_count   => l_message_count
                                                         ,x_message_list    => l_message_list
                                                         ,x_return_status   => l_return_status
                                                         ,x_batch_step_rec	 => l_batch_step_rec);
                                                         
                 IF l_return_status <> x_return_status THEN
                    RAISE UPDATE_STEP_QTY_ERROR;
                 END IF;                               
              END IF; /*l_batch_step_rec.batchstep_id*/
           END IF; /*IF x_batch_header.automatic_step_calculation = 1 */
        END IF; /* POC_IND*/
     END IF;  /* update_inventory_ind*/
  END IF;/* line_type */
   
EXCEPTION

  WHEN REVERT_MATL_ERROR THEN
      NULL;
  WHEN BATCH_HEADER_FETCH_ERROR THEN
    x_return_status := FND_API.G_RET_STS_ERROR;  
    GME_common_pvt.log_message('GME_API_BATCH_FETCH_ERROR');        
  WHEN REVERT_PHANT_FAIL THEN
      NULL;
  WHEN ERROR_LOAD_TRANS OR UPDATE_STEP_QTY_ERROR THEN
    x_return_status := l_return_status;  
  -- Bug 21101876, Added by QZENG, raise error when phantom batch is onhold.
  WHEN error_phantom_batch_hold THEN
    gme_common_pvt.log_message ('GME_PHANTOM_ONHOLD', 'BATCH_NO', l_batch_no);
    x_return_status := fnd_api.g_ret_sts_error;
  WHEN OTHERS THEN
     x_return_status := fnd_api.g_ret_sts_unexp_error;

     IF (NVL (g_debug, -1) = gme_debug.g_log_unexpected) THEN
        gme_debug.put_line (g_pkg_name || '.' || l_api_name || ':' || 'WHEN OTHERS:' || SQLERRM);
     END IF;

         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
END revert_line;  	
  


END gme_revert_batch_pvt;
/
COMMIT;
EXIT;
--SHOW ERRORS;
