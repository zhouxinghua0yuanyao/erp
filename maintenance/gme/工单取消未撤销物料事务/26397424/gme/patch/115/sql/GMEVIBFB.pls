/* +======================================================================+ */
/* |    Copyright (c) 2005, 2017 Oracle and/or its affiliates.           | */
/* |                         All rights reserved.                         | */
/* |                           Version 12.0.0                             | */
/* +======================================================================+ */
ReM dbdrv: sql ~PROD ~PATH ~FILE none none none package &phase=plb \
REM dbdrv: checkfile(120.13.12010000.35=120.28.12020000.19)(120.13.12000000.19=120.13.12010000.19)(120.13.12000000.17=120.29):~PROD:~PATH:~FILE 
SET VERIFY OFF
WHENEVER SQLERROR EXIT FAILURE ROLLBACK;
WHENEVER OSERROR EXIT FAILURE ROLLBACK;
CREATE OR REPLACE PACKAGE BODY gme_incremental_backflush_pvt AS
/* $Header: GMEVIBFB.pls 120.28.12020000.19 2017/05/15 17:29:28 gmurator ship $ 

 This file contains the procedure for partial certifying batches in OPM.   

 **********************************************************************
 *                                                                    *
 * FILE:    GMEVIBFB.pls                                              *
 * PURPOSE: Package Body for GME_INCREMENTAL_BACKFLUSH_PVT routines   *
 * AUTHOR:  A. Newbury                                                *
 * DATE:    June 2005                                                 *
 * CONTENTS:                                                          *
 *                                                                    *
 * HISTORY:                                                           *
 * ========                                                           *

 * G. Muratore    02-Jul-2009 Bug 8639523  
 *   No need to do negative inventory checking for phantom prods or ingredients.
 *   PROCEDURE: revert_material_partial 

 * G. Muratore    26-Jan-2010 Bug 10378355  
 *   Institute new algorithm for deriving factor to be applied to resources.
 *   This is to account for the fact that wip plan qty concept does not exist
 *   on the resource side.
 *   PROCEDURE: derive_factor 
 
 * G. Muratore    21-Nov-2013   Bug 17436612     
 *   Introduce some special processing to handle scenarios where all incremental 
 *   material quantities are not in the same proportion, particularly with phantoms.  
 *   PROCEDURE: incremental_backflush 
 
 * G. Muratore    01-Jul-2014   Bug 18925101     
 *   Timestamp the resource transactions with same date used for material transactions.   
 *   PROCEDURE: incremental_backflush 
 
 * G. Muratore    16-Jul-2014   Bug 19185162     
 *   Pass in calculated factor for resources to also be used for the phantom batch.   
 *   PROCEDURE: incremental_backflush 
 
 * G. Muratore    10-Nov-2014 Bug 18846633  
 *   Changed code to respect value of p_create_resv_pend_lots parameter passed in.
 *   Currently code is hard coded to pass 1 but that will change in the near future.
 *   PROCEDURE: revert_material_partial
 
 * G. Muratore    29-Sep-2015 Bug 19868921
 *   Initialize new field rsrv_quantity in memory table g_mat_txn_hdr_tbl to NULL or
 *   correct quantity. Reservation code will now use this value if initialized.
 *   PROCEDURE: revert_material_partial
 
 * G. Muratore    14-Oct-2015 Bug 19868921
 *   Additional fix to introduce global that indicates 
 *   partial negative IB to avoid status check later.

 * G. Muratore    01-Feb-2016 Bug 22567974     
 *   This is a fine tuning of all the previous rounding issues: 22217179/22287048/22570661/21078209
 *   The code is made more precise to determine whether the new actual is essentially equal to the wip plan then
 *   default to wip plan qty. This change works in tandem with changes in GMEVRLBB.pls during positive IB. 
 *   In GMEVRLBB.pls, the same check is essentially there at the reservation level.
 *   PROCEDURE: incremental_backflush 
    
 * G. Muratore    04-APR-2016 Bug 22317010 / 22764488
 *   Introduced new p_create_resv_pend_lots and p_tolerance parameter.
 *   p_create_resv_pend_lots acceptable values are 1 or zero. This was already
 *   in place in lower level code, but was never passed in. 

 *   p_tolerance acceptable values are between 1 and .99. This was introduced for handling
 *   problems caused by rounding over the course of multiple IB's. 
 *   Introduced new p_lpn_id parameter for future use. Bugs 13628717/12884831. 
 *   PROCEDURE: incremental_backflush
   
 * G. Muratore    23-Jun-2016 Bug 23216605     
 *   This is a fine tuning of all the previous rounding issues: 22217179/22287048/22570661/21078209
 *   The code now sets a flag to check precision only when the new actual is equal to the wip plan
 *   for the item driving IB. Prior to this change, the precision check was done everytime which was not necessary.
 *   PROCEDURE: incremental_backflush 
 **********************************************************************
*/

/*  Global variables   */
G_PKG_NAME  CONSTANT  VARCHAR2(30):='GME_INCREMENTAL_BACKFLUSH_PVT';
G_DEBUG VARCHAR2(5) := FND_PROFILE.VALUE('AFLOG_LEVEL');
 
/* ===========================================================================================
Procedure
  incremental_backflush
Description
  This procedure performs incremental backflush for a batch based on a material detail
Parameters
  p_batch_header_rec            The batch header record
  p_material_detail_rec         The material detail record to base IB from
  p_qty                         Incremental qty
  p_qty_type                    Incremental qty type:
                                  increment qty (0),
                                  new act qty   (1) or
                                  % wip_plan    (2)
  p_trans_date                  Date for transactions
  p_resource_backflush          This is calculated in gme_api_main because for phantom
                                IB, the material detail always comes in as a product.
                                The resource backflushing should be carried through from
                                parent to child batches as determined by the material that
                                the backflush was originated from, not from each iteration
                                of the backflushing.  Therefore, as a central point, gme_api_main
                                is the best place to determine this (for use by form and pub) and
                                this will be passed down through each phantom iteration (not recalculated).

  p_create_resv_pend_lots       Used for negative IB only. 
                                (0) Do not recreate reservations or pending product lots.
                                (1) Recreate reservations or pending product lots.
   
  p_tolerance                   p_tolerance acceptable values are between 1 and .99.
  
  p_lpn_id                      Parameter for future use on yields. Bugs 13628717/12884831.    
                                         
  x_return_status               Outcome of the API call
                                S - Success
                                E - Error
                                U - Unexpected error
                                X - Batch Exception occurred

History
 Sivakumar.G FPBug#4667093 05-NOV-2005
   Changed code to respect new gme parameter, when deriving factor to be used against the detail lines.
   Also, introduced code that respects integer scaling setting once new actual is derived.
   
 Sivakumar.G Bug#5111078 29-MAR-2006
  validate_material_for_IB is changed for not allowing IB for Lab Batches with update inventory off. And
  plan_qty = 0 check is replaced by wip_plan_qty

 G. Muratore     12-Feb-2009   Bug 7709971 
  Back out and rework 7286054 as now phantom ingredient gets double posting. Reinstated elsif 
  phantom ingredients are reconciled by the phantom prod yield.
  Note: We could not duplicate this problem with lot control items even with 7286054 in place.
  New fix works for all item types.  
  
 G. Muratore     04-Mar-2009   Bug 8267588 
  Once a new_actual is derived for a given line, round it to 5 decimal places. 

 G. Muratore     21-May-2009   Bug 8508788 
  Update any new transaction that was created for this item/step and all dependent steps with the
  trans_date passed in by the user or sysdate. 
  Also, reworked 8516257 which was an additional rework of bug Bug 7709971 and 7286054. Typo was corrected.    

 G. Muratore     02-Jun-2009   Bug 8508788 - Backout resource txn date piece of previous fix by commenting the lines.
  We may need to reinstitute this code as part of a bigger fix down the road after getting full design.
  
 G. Muratore     19-Jun-2009   Bug 8508788 
  Reinstating the original fix with PM approval. One modification. The trans_date passed in, or sysdate, 
  will be used only if it is later than the actual start date of the resource.  
  
 G. Muratore     19-MAR-2010   Bug 8751983     
  Changed order by clause to fetch material transactions so that for negative IB
  they are processed in reverse trans order. Also, stamp resource transactions 
  affected by IB with user entered IB date. Additional issue addressed here is bug 9072371.  
  Do not reverse lot transactions if it will lead to a negative inventory balance. 
  PROCEDURE: revert_material_partial
  
 G. Muratore     09-APR-2010   Bug 9560022     
  Round new actual before comparing to original actual.
    
 G. Muratore     05-MAY-2010   Bug 9628831     
  Do not update the actual quantity for non transactable material items.
  
 G. Muratore     21-Nov-2013   Bug 17436612     
  Introduce some special processing to handle scenarios where all incremental material
  quantities are not in the same proportion, particularly with phantoms.    
  
 G. Muratore     21-Nov-2013   Bug 19185162     
  Pass in calculated factor for resources to also be used for the phantom batch. 
  
 G. Muratore     19-May-2015   Bug 21078209     
  Due to some rounding issues if the new actual is essentially equal to the wip plan then
  default to wip plan qty. 
  
 Shaliu Chen     18-Jun-2015   Bug 21208206     
  set global variable g_atuo_resource_txn_flag to 'Y' to identify the resource txn 
  record is created by incremental backflush action.  
    
 G. Muratore     01-Feb-2016   Bug 22567974     
  This is a fine tuning of all the previous rounding issues: 22217179/22287048/22570661/21078209
  The code is made more precise to determine whether the new actual is essentially equal to the wip plan then
  default to wip plan qty. This change works in tandem with changes in GMEVRLBB.pls during positive IB. 
  In GMEVRLBB.pls, the same check is essentially there at the reservation level.

 G. Muratore     04-APR-2016   Bug 22317010 / 22764488
  Introduced new p_create_resv_pend_lots and p_tolerance parameter.
  p_create_resv_pend_lots acceptable values are 1 or zero. This was already
  in place in lower level code, but was never passed in. This allows user to have more
  control on re-creating reservations and pending product lots during negative IB.

  p_tolerance acceptable values are between 1 and .99. This was introduced for handling
  problems caused by rounding over the course of multiple IB's. It will be used only to 
  determine if user is essentially on the last IB and how to relieve a reservation. This
  is a fine tuning of 22567974 which gives tolerance control to the customer instead of hard coding.
   
  Introduced new p_lpn_id parameter for future use. Bugs 13628717/12884831.   
========================================================================================================*/
  PROCEDURE incremental_backflush
    (p_batch_header_rec           IN GME_BATCH_HEADER%ROWTYPE
    ,p_material_detail_rec        IN GME_MATERIAL_DETAILS%ROWTYPE
    ,p_qty                        IN NUMBER 
    ,p_qty_type                   IN NUMBER
    ,p_trans_date                 IN DATE
    ,p_backflush_rsrc_usg_ind     IN NUMBER
    ,p_create_resv_pend_lots      IN NUMBER DEFAULT 1 -- Bug 22317010 
    ,p_tolerance                  IN NUMBER DEFAULT .9997 -- Bug 22317010 / 22764488
    ,p_lpn_id                     IN NUMBER DEFAULT NULL -- for future use Bugs 13628717 and 12884831
    ,x_exception_material_tbl     IN OUT NOCOPY gme_common_pvt.exceptions_tab
    ,x_return_status              OUT NOCOPY VARCHAR2) IS

    l_api_name   CONSTANT VARCHAR2(30) := 'incremental_backflush';

    CURSOR Cur_fetch_incr_mat_dtl(v_batch_id NUMBER, v_matl_dtl_id NUMBER) IS
      SELECT * 
      FROM   gme_material_details
      WHERE  batch_id = v_batch_id
      AND    (release_type = gme_common_pvt.g_mtl_incremental_release
              OR
              (release_type = gme_common_pvt.g_mtl_manual_release AND material_detail_id = v_matl_dtl_id)
             )
      AND    wip_plan_qty <> 0  
      ORDER BY line_type,line_no;

    CURSOR Cur_assoc_step(v_batch_id NUMBER) IS
      SELECT *
      FROM   gme_batch_steps
      WHERE  batchstep_id IN (SELECT DISTINCT batchstep_id
                              FROM   gme_batch_step_items
                              WHERE  batch_id = v_batch_id);

    CURSOR Cur_prod_assoc(V_batch_id NUMBER, V_material_detail_id NUMBER) IS
      SELECT s.*
      FROM   gme_batch_step_items m, gme_batch_steps s
      WHERE  m.batch_id = V_batch_id
      AND    m.material_detail_id = V_material_detail_id
      AND    s.batch_id = m.batch_id
      AND    s.batchstep_id = m.batchstep_id
      AND    s.step_status = gme_common_pvt.g_step_wip;
   
    --FPBug#4667093 get the IB Factor settings
    CURSOR Cur_ib_factor(V_org_id VARCHAR2) IS
     SELECT ib_factor_ind
       FROM gme_parameters
      WHERE organization_id = V_org_id;

    l_item_rec                    mtl_system_items%ROWTYPE;
    
    l_phantom_batch_rec           gme_batch_header%ROWTYPE;
    l_batch_header_rec            gme_batch_header%ROWTYPE;
    l_material_detail_rec         gme_material_details%ROWTYPE;
    l_in_material_detail_rec      gme_material_details%ROWTYPE;
    l_phantom_material_rec        gme_material_details%ROWTYPE;
    l_material_detail_tbl         gme_common_pvt.material_details_tab;

    l_batch_step_rec              gme_batch_steps%ROWTYPE;
    l_in_batch_step_rec           gme_batch_steps%ROWTYPE;
    l_step_tbl                    gme_common_pvt.steps_tab;
    
    l_incr_qty                    NUMBER;
    l_decr_qty                    NUMBER;
    l_incr_factor                 NUMBER;
    l_incr_factor_res             NUMBER;
    
    l_incr_factor_neg             NUMBER;  -- Bug 17436612

    l_check_prec                  NUMBER;  -- Bug 23216605
    
    l_new_actual                  NUMBER;
    l_actual_qty                  NUMBER;
    l_upd_material                BOOLEAN;
    l_lot_divisible_flag          VARCHAR2(1);
    
    l_return_status               VARCHAR2 (1);
    l_msg_count                   NUMBER;
    l_msg_stack                   VARCHAR2 (2000);

    --FPBug#4667093
    l_hold_new_actual             NUMBER;
    l_ib_factor                   NUMBER := 0;
    l_scale_rec                   gmd_common_scale.scale_rec;
    l_scale_rec_out               gmd_common_scale.scale_rec;

    -- Bug 22317010
    l_create_resv_pend_lots       NUMBER;
    
    -- Bug 22567974
    l_precision                   NUMBER;    
    l_numerator                   NUMBER;    
    l_denominator                 NUMBER;    
    
    qty_create_negative_actual    EXCEPTION;
    error_derive_factor           EXCEPTION;
    error_get_item                EXCEPTION;
    error_revert_matl_full        EXCEPTION;
    ERROR_UPDATING_STEPS          EXCEPTION;
    error_phantom_backflush       EXCEPTION;
    error_fetch_batch             EXCEPTION;
    error_fetch_matl              EXCEPTION;
    error_update_row              EXCEPTION;
    error_consum_yield            EXCEPTION;
    update_step_qty_error         EXCEPTION;
    error_revert_matl_part        EXCEPTION;
    error_cant_go_neg             EXCEPTION;


  BEGIN
    IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
       gme_debug.put_line('Entering api '||g_pkg_name||'.'||l_api_name);
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' batch_id = '||p_batch_header_rec.batch_id);
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' material_detail_id = '||p_material_detail_rec.material_detail_id);
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' line_no = '||p_material_detail_rec.line_no);
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' line_type = '||p_material_detail_rec.line_type);
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_qty = '||p_qty);
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_qty_type = '||p_qty_type);
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_backflush_rsrc_usg_ind = '||p_backflush_rsrc_usg_ind);
       IF p_trans_date IS NULL THEN
          gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_trans_date IS NULL');
       ELSE
          gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_trans_date = '||to_char(p_trans_date, 'DD-MON-YYYY HH24:MI:SS'));
       END IF;
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' gme_common_pvt.g_move_to_temp = '||gme_common_pvt.g_move_to_temp);
       
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_create_resv_pend_lots = '||p_create_resv_pend_lots);
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_tolerance = '||p_tolerance);
       
       IF p_lpn_id IS NULL THEN
          gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_lpn_id IS NULL');
       ELSE
          gme_debug.put_line(g_pkg_name||'.'||l_api_name||' Currently not used p_lpn_id = '||p_lpn_id);
       END IF;
       
    END IF;

    -- Bug 22317010
    l_create_resv_pend_lots := NVL(p_create_resv_pend_lots, 1);
    IF l_create_resv_pend_lots NOT IN (0, 1) THEN
       l_create_resv_pend_lots := 1;
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' Use l_create_resv_pend_lots = 1');    
    END IF;
    
    /* Set the return status to success initially */
    x_return_status       := FND_API.G_RET_STS_SUCCESS;
    
    -- set local variables for batch and detail...
    l_batch_header_rec       := p_batch_header_rec;
    l_in_material_detail_rec := p_material_detail_rec;
    
    --  BUG 21208206 - set global variable g_atuo_resource_txn_flag to 'Y' to identify
    --                 the resource txn record is created by incremental backflush action.
 
    IF NVL(gme_common_pvt.g_atuo_resource_txn_flag,'N') <> 'Y' THEN
       gme_common_pvt.g_atuo_resource_txn_flag := 'Y';
    END IF;     

    --FPBug#4667093 Begin
    OPEN Cur_ib_factor(l_batch_header_rec.organization_id);
    FETCH Cur_ib_factor INTO l_ib_factor;
    CLOSE Cur_ib_factor;
    --FPBug#4667093 End

    -- derive factor
    IF nvl(l_batch_header_rec.parentline_id,0) = 0 THEN
       --FPBug#4667093 added p_gme_ib_factor parameter with value 0
       derive_factor
           (p_material_detail_rec       => l_in_material_detail_rec
           ,p_qty                       => p_qty
           ,p_qty_type                  => p_qty_type
	        ,p_gme_ib_factor             => 0
           ,x_pct_plan                  => l_incr_factor
           ,x_pct_plan_res              => l_incr_factor_res
           ,x_return_status             => l_return_status);
       
       --FPBug#4667093 Begin
       IF ( NVL(G_DEBUG,-1) <= GME_DEBUG.G_LOG_STATEMENT ) THEN
          gme_debug.put_line (g_pkg_name||'.'||l_api_name||' Single Product:l_incr_factor = '||l_incr_factor);
       END IF;
       
       /* add the incr qty to old actual to come up with new actual
          as l_incr_factor now depicts the percent increase from previous actual 
        */
       
       -- Bug 17436612 use rounding
       l_hold_new_actual := ROUND(l_in_material_detail_rec.actual_qty +
                       ((l_in_material_detail_rec.wip_plan_qty * l_incr_factor) / 100),35); 
       
       /* if IB is driven off by product line and ib factor calculaion is total products */
       IF l_in_material_detail_rec.line_type =  1 AND l_ib_factor = 1 THEN
          derive_factor
           (p_material_detail_rec       => l_in_material_detail_rec
           ,p_qty                       => p_qty
           ,p_qty_type                  => p_qty_type
	        ,p_gme_ib_factor             => 1
           ,x_pct_plan                  => l_incr_factor
           ,x_pct_plan_res              => l_incr_factor_res
           ,x_return_status             => l_return_status);
       END IF;
       
       IF l_return_status <> FND_API.G_RET_STS_SUCCESS THEN
          RAISE error_derive_factor;
       END IF;
       
       IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
          gme_debug.put_line (g_pkg_name||'.'||l_api_name||' l_incr_factor = '||l_incr_factor);
          gme_debug.put_line (g_pkg_name||'.'||l_api_name||' l_incr_factor_res = '||l_incr_factor_res);
	       gme_debug.put_line (g_pkg_name||'.'||l_api_name||' l_hold_new_actual = '||l_hold_new_actual);
	       gme_debug.put_line (g_pkg_name||'.'||l_api_name||' l_in_material_detail_rec.actual_qty = '||l_in_material_detail_rec.actual_qty);
	       gme_debug.put_line (g_pkg_name||'.'||l_api_name||' l_in_material_detail_rec.wip_plan_qty = '||l_in_material_detail_rec.wip_plan_qty);
       END IF;
       --FPBug#4667093 End
    ELSE
       -- if this is a phantom batch, the qty passed is the incr factor as already calculated
       -- in the parent batch
       l_incr_factor := p_qty;     
       
       -- 19185162 If this is a phantom batch, the qty type passed is the 
       -- res factor as already calculated in the parent batch.
       l_incr_factor_res := p_qty_type;     

       --FPBug#4667093 Begin
       /* add the incr qty to old actual to come up with new actual
          as l_incr_factor now depicts the percent increase from previous actual 
        */
        
       -- Bug 17436612 use rounding
       l_hold_new_actual := ROUND(l_in_material_detail_rec.actual_qty +
                       ((l_in_material_detail_rec.wip_plan_qty * l_incr_factor) / 100), 35); 
       
       IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
          gme_debug.put_line (g_pkg_name||'.'||l_api_name||' phantom batch; l_incr_factor = '||l_incr_factor);
          gme_debug.put_line (g_pkg_name||'.'||l_api_name||' l_hold_new_actual = '||l_hold_new_actual);
	       gme_debug.put_line (g_pkg_name||'.'||l_api_name||' l_in_material_detail_rec.actual_qty = '||l_in_material_detail_rec.actual_qty);
	       gme_debug.put_line (g_pkg_name||'.'||l_api_name||' l_in_material_detail_rec.wip_plan_qty = '||l_in_material_detail_rec.wip_plan_qty);
       END IF;

       -- Bug 17436612 We can assume that IB initiated at parent level is driving to zero. 
       IF l_hold_new_actual < 0 THEN
          l_hold_new_actual := 0;
       END IF;

       IF l_hold_new_actual > 0 THEN
          -- Bug 22567974 - lets check rounding scenarios.
          -- Get precision of wip plan so we can round new actual to same precision and compare.
          l_precision := length(l_in_material_detail_rec.wip_plan_qty - trunc(l_in_material_detail_rec.wip_plan_qty));
          
          -- Default fraction variables to the most common scenario.         
          l_numerator := l_in_material_detail_rec.wip_plan_qty;    
          l_denominator := l_hold_new_actual; 
                    
          -- Change Default fraction variables to make denominator the larger quantity.         
          IF l_numerator > l_denominator THEN
             l_numerator := l_hold_new_actual;    
             l_denominator := l_in_material_detail_rec.wip_plan_qty;           
          END IF;

          IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
             gme_debug.put_line ('l_numerator FOR precision check rounding issue is '||l_numerator);
             gme_debug.put_line ('l_denominator FOR precision check rounding issue is '||l_denominator);
          END IF;

          IF ROUND(l_hold_new_actual, l_precision) = l_in_material_detail_rec.wip_plan_qty OR
             (ROUND(l_numerator/l_denominator, l_precision) >= .9997 AND
              ROUND(l_numerator/l_denominator, l_precision) <= 1) THEN          
             l_hold_new_actual := l_in_material_detail_rec.wip_plan_qty;        
          END IF; 
             
          IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
             gme_debug.put_line ('l_hold_new_actual for phantom after precision check rounding issue is '||l_hold_new_actual);
          END IF;
       END IF;
       
       --FPBug#4667093 End      
    END IF;

    -- Bug 17436612 Let's store pure factor from the initiating row 
    --              in case all incr rows are not in same proportion.
    l_incr_factor_neg := NULL;
    IF l_hold_new_actual >= 0 AND l_incr_factor < 0 THEN
       l_incr_factor_neg := l_hold_new_actual/l_in_material_detail_rec.wip_plan_qty;
    END IF;

    IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
       gme_debug.put_line (' l_incr_factor_neg = '||l_incr_factor_neg);
    END IF;

    -- Bug 23216605 - Let's set flag on whether to do precision or not.
    --                This check will only be done when user is exactly at 100 % on item driving IB.
    l_check_prec := 0;
    IF l_hold_new_actual = l_in_material_detail_rec.wip_plan_qty THEN
       l_check_prec := 1;
    END IF;   

    IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
       gme_debug.put_line (' l_check_prec= '||l_check_prec);
    END IF;
    
    -- Fetch all the incremental material lines of the batch and the passed in material line if manual
    OPEN Cur_fetch_incr_mat_dtl(l_in_material_detail_rec.batch_id
                               ,l_in_material_detail_rec.material_detail_id);
    FETCH Cur_fetch_incr_mat_dtl BULK COLLECT INTO l_material_detail_tbl;
    CLOSE Cur_fetch_incr_mat_dtl;

    FOR i IN 1 .. l_material_detail_tbl.COUNT LOOP
       l_material_detail_rec := l_material_detail_tbl (i);
       
       IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
          gme_debug.put_line (g_pkg_name||'.'||l_api_name||' -------------------------------------');
          gme_debug.put_line (g_pkg_name||'.'||l_api_name||' TOP of material detail loop; processing material_detail_id: '||l_material_detail_rec.material_detail_id);
       END IF;

       -- Bug 25883843 - Initialize this variable each time loop starts to avoid inadvertant updates.
       l_upd_material := FALSE;
       
       --FPBug#4667093 add IF condition 
       /* if material line is same as the line which drives IB then don't calculate new actual */
       IF l_material_detail_rec.material_detail_id = l_in_material_detail_rec.material_detail_id THEN
          l_new_actual := l_hold_new_actual;
	       IF ( NVL(G_DEBUG,-1) <= GME_DEBUG.G_LOG_STATEMENT ) THEN
             gme_debug.put_line (g_pkg_name||'.'||l_api_name||' IB Driven new actual: '||l_new_actual);
          END IF;
       ELSE
          IF (l_material_detail_rec.scale_type = 0) THEN
             -- set actual qty for fixed line only if it is zero
             -- fixed items will have already been set the first time through
             IF (l_material_detail_rec.actual_qty = 0) THEN
                l_new_actual := l_material_detail_rec.wip_plan_qty;
                
                IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
                   gme_debug.put_line (g_pkg_name||'.'||l_api_name||' fixed scale: new actual: '||l_new_actual);
                END IF;
             ELSE
                l_new_actual := l_material_detail_rec.actual_qty;
                IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
                   gme_debug.put_line (g_pkg_name||'.'||l_api_name||' fixed scale: same actual: '||l_new_actual);
                END IF;
             END IF; /* l_material_detail_rec.actual_qty = 0 */
          ELSE
             IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
                gme_debug.put_line (g_pkg_name||'.'||l_api_name||' actual_qty = '||l_material_detail_rec.actual_qty);
                gme_debug.put_line (g_pkg_name||'.'||l_api_name||' wip_plan_qty = '||l_material_detail_rec.wip_plan_qty);
             END IF;
          
             -- Bug 17436612 set actual to zero if user is decrementing full amount.
             IF l_hold_new_actual = 0 THEN
                l_new_actual := 0;
             ELSE
	             -- add the incr qty to old actual to come up with new actual
                -- as p_incr_factor now depicts the percent increase/decrease from previous actual.
                l_new_actual := l_material_detail_rec.actual_qty +
                             ((l_material_detail_rec.wip_plan_qty * l_incr_factor) / 100); 

                IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
                   gme_debug.put_line ('First calculation of l_new_actual is '||l_new_actual);
                END IF;                      

                -- Bug 23216605 - Let's check flag on whether to do precision or not.
                IF l_check_prec = 1 THEN

                   -- Bug 21078209 - If these are essentially equal, then there must have been a rounding  
                   -- issue therefore user must be trying to do IB for the remaining quantity.
                   
                   -- Examples: Reservation should be for full amount respectively.
                   -- For first condition is wip plan of .16667 then IB for 50 % and 50 % again.
                   -- For second condition is wip plan of .00011 then IB for 50 % and 50 % again.
                   
                   -- Bug 22567974 - lets check more rounding scenarios.
                   -- Default fraction variables to the most common scenario.         
                   l_numerator := l_material_detail_rec.wip_plan_qty;    
                   l_denominator := l_new_actual; 
                             
                   -- Change Default fraction variables to make denominator the larger quantity.         
                   IF l_numerator > l_denominator THEN
                      l_numerator := l_new_actual;    
                      l_denominator := l_material_detail_rec.wip_plan_qty;           
                   END IF;
                   
                   IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
                      gme_debug.put_line ('l_numerator FOR precision check rounding issue is '||l_numerator);
                      gme_debug.put_line ('l_denominator FOR precision check rounding issue is '||l_denominator);
                   END IF;                
                    
                   -- IF ((l_material_detail_rec.wip_plan_qty/l_new_actual > .999 AND
                      -- round(l_material_detail_rec.wip_plan_qty/l_new_actual, 5) <= 1) OR                    
                   
                   -- Bug 22217179/22287048 - increased precision of check to 999.
                   -- Bug 22567974/22570661 - and round to second condition in first check.
                   -- We may need to increase .999 to .9997 someday.
                   IF ((l_numerator/l_denominator > .999 AND
                      round(l_numerator/l_denominator, 5) <= 1) OR                    
                      (l_new_actual < 1 AND TRUNC(ABS(l_material_detail_rec.wip_plan_qty - l_new_actual), 5) = 0)) THEN
                      l_new_actual := l_material_detail_rec.wip_plan_qty;
                      IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
                         gme_debug.put_line ('l_new_actual after rounding issue is '||l_new_actual);
                      END IF;
                   ELSE                    
                      -- Bug 22567974 - lets check another rounding scenario.
                      -- get precision of wip plan so we can round new actual to same precision and compare.
                      l_precision := length(l_material_detail_rec.wip_plan_qty - trunc(l_material_detail_rec.wip_plan_qty));
                      IF ROUND(l_new_actual, l_precision) = l_material_detail_rec.wip_plan_qty THEN
                         l_new_actual := l_material_detail_rec.wip_plan_qty;
                      END IF;
                      
                      IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
                         gme_debug.put_line ('l_new_actual after precision check rounding issue is '||l_new_actual);
                      END IF;
                   END IF;
                END IF; -- Bug 23216605   l_check_prec = 1
             
                -- Bug 17436612 Let's do an additional check in case all incr rows are not in same proportion.
                IF l_new_actual < 0 AND l_incr_factor_neg IS NOT NULL THEN
                   l_new_actual := l_material_detail_rec.wip_plan_qty * l_incr_factor_neg;        
             
                   IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
                      gme_debug.put_line ('l_new_actual second calc is '||l_new_actual);
                   END IF;                      
                END IF;                                                    
             END IF;                          
                          
             IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
                gme_debug.put_line (g_pkg_name||'.'||l_api_name||' not fixed scale: new actual: '||l_new_actual);
             END IF;

             --FPBug#4667093 Begin
             IF (l_material_detail_rec.scale_type = 2) THEN
                 l_scale_rec.scale_rounding_variance := l_material_detail_rec.scale_rounding_variance;
                 l_scale_rec.qty := l_new_actual;
                 l_scale_rec.scale_multiple := l_material_detail_rec.scale_multiple;
                 l_scale_rec.rounding_direction := l_material_detail_rec.rounding_direction;
           
	             IF (NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT) THEN
                   gme_debug.put_line('values going to int mult scale are: ');
                   gme_debug.put_line('qty '||l_scale_rec.qty);
                   gme_debug.put_line('rnd variance '||l_scale_rec.scale_rounding_variance);
                   gme_debug.put_line('multiple '||l_scale_rec.scale_multiple);
                   gme_debug.put_line('rnd direction '||l_scale_rec.rounding_direction);
                END IF;
          
	             gmd_common_scale.integer_multiple_scale
                 (  p_scale_rec          => l_scale_rec
                   ,x_scale_rec          => l_scale_rec_out
                   ,x_return_status      => l_return_status
                 );
    
	             IF l_return_status <> FND_API.G_RET_STS_SUCCESS THEN
                   RAISE error_cant_go_neg;
                END IF;

                l_new_actual := l_scale_rec_out.qty;

                IF (NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT) THEN
                   gme_debug.put_line('new actual after integer scaling is '||l_new_actual);
                END IF;
            END IF; /* l_material_detail_rec.scale_type = 2 */
	         --FPBug#4667093 End
         END IF;  -- IF (l_material_detail_rec.scale_type = 0)
      END IF; /*l_material_detail_rec.material_detail_id = p_material_detail_rec.material_detail_id*/

      -- Bug 8267588 - Round the actual qty to 5 decimal places.                 
      l_new_actual := ROUND(l_new_actual, 5);
      IF (NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT) THEN
         gme_debug.put_line('new actual after rounding is '||l_new_actual);
      END IF;
      
      -- Bug 9560022 - Moved this check so that it happens after rounding is done.
      IF l_new_actual < 0 THEN
         IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
            gme_debug.put_line (g_pkg_name||'.'||l_api_name||' raising error; new actual= '||l_new_actual);
         END IF;
         RAISE error_cant_go_neg;
      END IF;

      l_incr_qty := ROUND(l_new_actual - l_material_detail_rec.actual_qty, 5);
  
      gme_material_detail_pvt.get_item_rec
                        (p_org_id                => l_material_detail_rec.organization_id
                        ,p_item_id               => l_material_detail_rec.inventory_item_id
                        ,x_item_rec              => l_item_rec
                        ,x_return_status         => l_return_status);
                        
      IF l_return_status <> FND_API.G_RET_STS_SUCCESS THEN
         x_return_status := l_return_status;
         RAISE error_get_item;
      END IF;
      
      IF l_item_rec.lot_control_code = 2 AND NVL(l_item_rec.lot_divisible_flag,'Y') = 'N' THEN
         l_lot_divisible_flag := 'N';
      ELSE
         l_lot_divisible_flag := 'Y';
      END IF;

      IF l_material_detail_rec.phantom_id IS NOT NULL THEN
         l_phantom_batch_rec.batch_id := l_material_detail_rec.phantom_id;
         
         IF NOT gme_batch_header_dbl.fetch_row(l_phantom_batch_rec, l_phantom_batch_rec) THEN
            RAISE error_fetch_batch;
         END IF;
         
         l_phantom_material_rec.material_detail_id := l_material_detail_rec.phantom_line_id;
         
         IF NOT gme_material_details_dbl.fetch_row(l_phantom_material_rec, l_phantom_material_rec) THEN
            RAISE error_fetch_matl;
         END IF;
         
         IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
            gme_debug.put_line (g_pkg_name||'.'||l_api_name||' processing phantom batch with batch_id/material_detail_id: '||l_phantom_batch_rec.batch_id||'/'||l_phantom_material_rec.material_detail_id);
         END IF;
         
         incremental_backflush
             (p_batch_header_rec        => l_phantom_batch_rec
             ,p_material_detail_rec     => l_phantom_material_rec
             ,p_qty                     => l_incr_factor
             ,p_qty_type                => l_incr_factor_res -- 19185162 pass in factor for resources.
             ,p_trans_date              => p_trans_date
             ,p_backflush_rsrc_usg_ind  => p_backflush_rsrc_usg_ind
             ,x_exception_material_tbl  => x_exception_material_tbl
             ,x_return_status           => l_return_status);
         
         IF l_return_status NOT IN (FND_API.G_RET_STS_SUCCESS, gme_common_pvt.g_exceptions_err) THEN
            x_return_status := l_return_status;
            RAISE error_phantom_backflush;
         END IF;
         
         IF l_return_status = gme_common_pvt.g_exceptions_err THEN
            x_return_status := gme_common_pvt.g_exceptions_err;
         END IF;
        
         -- Bug 7709971 back out 7286054 as now phantom ingredient gets double posting.
         -- Reinstated elsif since phantom ingredients are reconciled by the phantom prod yield.
        
      --       END IF; /*Bug#7286054 Moved the end if before to the actual qty
      -- calculation so that the logic would become applicable for the phantom item also*/
                  
      ELSIF l_batch_header_rec.update_inventory_ind = 'Y' AND
            l_item_rec.mtl_transactions_enabled_flag = 'Y' THEN
         IF l_new_actual = 0 THEN
            -- full revert
            IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
               gme_debug.put_line (g_pkg_name||'.'||l_api_name||' new actual = 0; calling gme_unrelease_batch_pvt.revert_material_full');
            END IF;
          
            -- Bug 8751983 - Set to 2 if user is going negative.
            IF gme_common_pvt.g_ib_timestamp_set = 1 THEN
               gme_common_pvt.g_ib_timestamp_set := 2;
            END IF;
          
            gme_unrelease_batch_pvt.revert_material_full
                (p_material_detail_rec     => l_material_detail_rec
                -- ,p_create_resv_pend_lots   => 1
                ,p_create_resv_pend_lots   => l_create_resv_pend_lots -- Bug 22317010 
                ,p_ignore_transactable     => TRUE
                ,x_actual_qty              => l_actual_qty
                ,x_exception_material_tbl  => x_exception_material_tbl
                ,x_return_status           => l_return_status);
                
            IF l_return_status NOT IN (FND_API.G_RET_STS_SUCCESS, gme_common_pvt.g_exceptions_err) THEN
               x_return_status := l_return_status;
               RAISE error_revert_matl_full;
            END IF;
          
            IF l_return_status = gme_common_pvt.g_exceptions_err THEN
               x_return_status := gme_common_pvt.g_exceptions_err;
            END IF;
          
            l_upd_material := TRUE;
         ELSIF l_incr_qty < 0 THEN
            -- partial revert
            l_decr_qty := -1 * l_incr_qty;
            IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
              gme_debug.put_line (g_pkg_name||'.'||l_api_name||' call revert_material_partial: incr_qty/decr_qty= '||l_incr_qty||'/'||l_decr_qty);
            END IF;
            
            -- Bug 8751983 - Set to 2 if user is going negative.
            IF gme_common_pvt.g_ib_timestamp_set = 1 THEN
               gme_common_pvt.g_ib_timestamp_set := 2;
            END IF;
            
            -- Bug 19868921 - New global indicates partial negative IB.
            -- Set to avoid status check later.
            gme_common_pvt.g_ib_going_negative := 1;
            
            revert_material_partial
                (p_material_detail_rec        => l_material_detail_rec
                ,p_qty                        => l_decr_qty
                ,p_lot_control_code           => l_item_rec.lot_control_code
                -- ,p_create_resv_pend_lots   => 1
                ,p_create_resv_pend_lots      => l_create_resv_pend_lots -- Bug 22317010 
                ,p_lot_divisible_flag         => l_lot_divisible_flag
                ,x_actual_qty                 => l_actual_qty
                ,x_exception_material_tbl     => x_exception_material_tbl
                ,x_return_status              => l_return_status);
                
            IF l_return_status NOT IN (FND_API.G_RET_STS_SUCCESS, gme_common_pvt.g_exceptions_err) THEN
               x_return_status := l_return_status;
               RAISE error_revert_matl_part;
            END IF;

            -- Bug 19868921 - Reset new global.
            gme_common_pvt.g_ib_going_negative := 0;
            
            IF l_return_status = gme_common_pvt.g_exceptions_err THEN
               x_return_status := gme_common_pvt.g_exceptions_err;
            END IF;
            l_upd_material := TRUE;
         ELSIF l_incr_qty = 0 THEN
            -- nothing to do
            IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
               gme_debug.put_line (g_pkg_name||'.'||l_api_name||' incr_qty calculated to 0... nothing to do');
            END IF;
            l_upd_material := FALSE;
         ELSE
            -- consume or yield based on line type
            IF l_material_detail_rec.line_type = gme_common_pvt.g_line_type_ing THEN
              IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
                 gme_debug.put_line (g_pkg_name||'.'||l_api_name||' calling gme_release_batch_pvt.consume_material with target_qty = '||l_new_actual);
              END IF;
              gme_release_batch_pvt.consume_material
                          (p_material_dtl_rec       => l_material_detail_rec
                          ,p_trans_date             => p_trans_date
                          ,p_item_rec               => l_item_rec
                          ,p_consume_qty            => l_new_actual
                          ,p_called_by              => 'IB'  -- Bug 22217179/21078209 - This is specific to IB
                          ,p_tolerance              => p_tolerance   -- Bug 22317010 / 22764488 - Added for IB             
                          ,x_exception_material_tbl => x_exception_material_tbl
                          ,x_actual_qty             => l_actual_qty
                          ,x_return_status          => l_return_status);
            ELSE
              IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
                 gme_debug.put_line (g_pkg_name||'.'||l_api_name||' calling gme_complete_batch_pvt.yield_material with target_qty= '||l_new_actual);
              END IF;
              gme_complete_batch_pvt.yield_material
                          (p_material_dtl_rec      => l_material_detail_rec
                          ,p_yield_qty             => l_new_actual
                          ,p_trans_date            => p_trans_date
                          ,p_item_rec              => l_item_rec
                          ,p_force_unconsumed      => fnd_api.g_true
                          ,x_exception_material_tbl => x_exception_material_tbl
                          ,x_actual_qty            => l_actual_qty
                          ,x_return_status         => l_return_status);
            END IF;
            
            IF l_return_status NOT IN (FND_API.G_RET_STS_SUCCESS, gme_common_pvt.g_exceptions_err) THEN
               x_return_status := l_return_status;
               IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
                  gme_debug.put_line (g_pkg_name||'.'||l_api_name||' raising exception with x_return_status= '||x_return_status);
               END IF;
               RAISE error_consum_yield;
            END IF;
            
            IF l_return_status = gme_common_pvt.g_exceptions_err THEN
               x_return_status := gme_common_pvt.g_exceptions_err;
            END IF;          
            l_upd_material := TRUE;
         END IF; -- IF l_new_actual = 0 THEN
        
         IF l_upd_material THEN
            l_material_detail_rec.actual_qty := l_actual_qty;
            
            IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
               gme_debug.put_line (g_pkg_name||'.'||l_api_name||' calling update material actual qty = '||l_material_detail_rec.actual_qty);
            END IF;
            
            IF NOT gme_material_details_dbl.update_row (l_material_detail_rec) THEN
               RAISE error_update_row;
            END IF;
         
            -- Bug 7709971 - When yielding or changing a phantom product then we must update the parent ingredient.
            IF nvl(l_batch_header_rec.parentline_id,0) <> 0 AND
               l_material_detail_rec.material_detail_id = l_in_material_detail_rec.material_detail_id THEN       
               -- Update the parent ingredient actual_qty. Phantom product updated accounted for. 
               -- in previous update. The transactions would have been taken care of.
                IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
                   gme_debug.put_line (g_pkg_name||'.'||l_api_name||'Updating actual qty for parent ingred to '||l_actual_qty);
                   gme_debug.put_line ('parent line is '||l_batch_header_rec.parentline_id);
                END IF;
            
               UPDATE gme_material_details
                  SET actual_qty = l_actual_qty,
                      last_updated_by = gme_common_pvt.g_user_ident, 
                      last_update_date = gme_common_pvt.g_timestamp, 
                      last_update_login = gme_common_pvt.g_login_id
                WHERE material_detail_id = l_batch_header_rec.parentline_id;
            END IF;                                 
         END IF;  -- IF l_upd_material THEN
      ELSE  -- lab batches or non transactable materials
         -- Bug 9628831 - Only update the actual quantity for lab batches, not for non transactable material items.
         -- Back out 9628831.
         -- IF l_batch_header_rec.laboratory_ind = 1 THEN     
         l_material_detail_rec.actual_qty := l_new_actual;
         
         IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
            gme_debug.put_line (g_pkg_name||'.'||l_api_name||' calling update material actual qty = '||l_material_detail_rec.actual_qty);
         END IF;
         
         IF NOT gme_material_details_dbl.update_row (l_material_detail_rec) THEN
            RAISE error_update_row;
         END IF;
         
         -- Bug 8516257 - Additional rework of bug Bug 7709971 and 7286054
         -- When yielding or changing a phantom product then we must update the parent ingredient.
         IF nvl(l_batch_header_rec.parentline_id, 0) <> 0 AND
            l_material_detail_rec.material_detail_id = l_in_material_detail_rec.material_detail_id THEN       
            -- Update the parent ingredient actual_qty. Phantom product updated accounted for. 
            -- in previous update. The transactions would have been taken care of.
             IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
                gme_debug.put_line (g_pkg_name||'.'||l_api_name||'Updating actual qty for parent ingred to '||l_new_actual);
                gme_debug.put_line ('parent line is '||l_batch_header_rec.parentline_id);
             END IF;
            
            -- Bug 8508788 - Let's update the parent ingredient with the correct actual qty.             
            UPDATE gme_material_details
               SET actual_qty = l_new_actual,
                   last_updated_by = gme_common_pvt.g_user_ident, 
                   last_update_date = gme_common_pvt.g_timestamp, 
                   last_update_login = gme_common_pvt.g_login_id
             WHERE material_detail_id = l_batch_header_rec.parentline_id;
         END IF;                                         
         -- END IF; -- IF l_batch_header_rec.laboratory_ind = 1                                       
      END IF; -- IF l_material_detail_rec.phantom_id IS NOT NULL THEN
    END LOOP;  -- FOR i IN 1 .. l_material_detail_tbl.COUNT LOOP

    IF l_batch_header_rec.automatic_step_calculation = 1  THEN
      -- Update POC Data if steps are associated to a material line
      -- First fetch all the steps associated to a batch
      OPEN Cur_assoc_step(l_batch_header_rec.batch_id);
      FETCH Cur_assoc_step BULK COLLECT INTO l_step_tbl;
      CLOSE Cur_assoc_step;

      FOR i IN 1..l_step_tbl.count LOOP      
        gme_update_step_qty_pvt.update_step_qty
          (p_batch_step_rec         => l_step_tbl(i)
          ,x_message_count          => l_msg_count
          ,x_message_list           => l_msg_stack
          ,x_return_status          => l_return_status
          ,x_batch_step_rec         => l_batch_step_rec);
            
        IF l_return_status <> FND_API.G_RET_STS_SUCCESS THEN
          RAISE update_step_qty_error;
        END IF;
      END LOOP;
    ELSIF p_backflush_rsrc_usg_ind = 1 THEN
      -- if p_backflush_rsrc_usg_ind is set to 1, this means that the material passed in
      -- is a product; this holds for parent/phantom batches as well as non phantom batches
      OPEN Cur_prod_assoc(l_batch_header_rec.batch_id, l_in_material_detail_rec.material_detail_id);
      FETCH Cur_prod_assoc INTO l_in_batch_step_rec;
      CLOSE Cur_prod_assoc;

      IF l_in_batch_step_rec.batchstep_id IS NOT NULL THEN      
          gme_update_step_qty_pvt.update_step_qty
            (p_batch_step_rec         => l_in_batch_step_rec
            ,p_backflush_factor       => l_incr_factor_res/100
            ,x_message_count          => l_msg_count
            ,x_message_list           => l_msg_stack
            ,x_return_status          => l_return_status          
            ,x_batch_step_rec         => l_batch_step_rec);

          IF l_return_status <> FND_API.G_RET_STS_SUCCESS THEN
            RAISE update_step_qty_error;
          END IF;

        
        -- Get all previous steps of the product step
        update_dependent_steps
            (p_batchstep_id     => l_in_batch_step_rec.batchstep_id
            ,p_backflush_factor => l_incr_factor_res/100
            ,x_return_status    => l_return_status);

        IF l_return_status <> FND_API.G_RET_STS_SUCCESS THEN
          RAISE ERROR_UPDATING_STEPS;
        END IF;  

        -- Bug 18925101 - We should timestamp the resource transactions with same date used for material transactions.
        -- IF gme_common_pvt.g_ib_timestamp_set > 0 THEN
           -- Bug 8508788 - Update any new transaction that was created for this item/step and all dependent steps
           -- with the date value passed in by the user or sysdate if it is later than the start date of the resource.
                                 
           -- Bug 8751983 - Added DEL action to where clause so we stamp reversals also.
           -- Transaction engine will decide whether to keep this date or not.
           update gme_resource_txns_gtmp
           set trans_date = p_trans_date
           where poc_trans_id in 
              (select t.poc_trans_id
               FROM   gme_batch_steps s, gme_batch_step_activities a, gme_batch_step_resources r, gme_resource_txns_gtmp t
               WHERE  s.batch_id = l_batch_header_rec.batch_id
               -- Comment out following update as original fix was just for one step.
               -- AND    a.batchstep_id = l_in_batch_step_rec.batchstep_id
               AND    a.batchstep_id = s.batchstep_id
               AND    r.batchstep_activity_id = a.batchstep_activity_id   
               AND    t.action_code = 'DEL'
               AND    t.line_id = r.batchstep_resource_id
               AND    p_trans_date >= r.actual_start_date);  
               

           -- Bug 8751983 - We do not want to stamp new transactions caused by negative IB at this time.
           -- New transaction should maintain original trans date being reversed, unless it is in a closed period.

           -- Bug 18925101 - timestamp New resource transactions except for negative IB.
           -- IF gme_common_pvt.g_ib_timestamp_set = 1 THEN
           IF gme_common_pvt.g_ib_timestamp_set <> 2 THEN
              update gme_resource_txns_gtmp
              set trans_date = p_trans_date
              where poc_trans_id in 
                 (select t.poc_trans_id
                  FROM   gme_batch_steps s, gme_batch_step_activities a, gme_batch_step_resources r, gme_resource_txns_gtmp t
                  WHERE  s.batch_id = l_batch_header_rec.batch_id
                  -- Comment out following update as original fix was just for one step.
                  -- AND    a.batchstep_id = l_in_batch_step_rec.batchstep_id
                  AND    a.batchstep_id = s.batchstep_id
                  AND    r.batchstep_activity_id = a.batchstep_activity_id   
                  AND    t.action_code = 'ADD'
                  AND    t.line_id = r.batchstep_resource_id
                  AND    p_trans_date >= r.actual_start_date);  
           END IF;                                                                
         -- END IF;  -- IF gme_common_pvt.g_ib_timestamp_set > 0
      END IF;  -- IF l_in_batch_step_rec.batchstep_id IS NOT NULL THEN
    END IF;  -- IF l_batch_header_rec.automatic_step_calculation = 1  THEN

    IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
       gme_debug.put_line('Exiting api '||g_pkg_name||'.'||l_api_name||' for batch_id= '||p_batch_header_rec.batch_id||' and x_return_status= '||x_return_status);
    END IF;

  EXCEPTION
  WHEN error_fetch_batch OR error_fetch_matl OR error_update_row THEN 
    gme_common_pvt.log_message ('GME_UNEXPECTED_ERROR', 'ERROR', SQLERRM);
    x_return_status := FND_API.g_ret_sts_unexp_error;
  WHEN error_cant_go_neg THEN
    gme_common_pvt.log_message ('GME_API_ACTUAL_CANT_GO_NEG');
    x_return_status := FND_API.g_ret_sts_error;
  WHEN error_derive_factor OR error_get_item OR error_phantom_backflush OR 
       error_revert_matl_full OR error_consum_yield OR error_revert_matl_part THEN
    NULL;
  WHEN update_step_qty_error OR ERROR_UPDATING_STEPS THEN
    x_return_status := l_return_status;
  WHEN OTHERS THEN
    fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
    IF g_debug <= gme_debug.g_log_procedure THEN
      gme_debug.put_line ('Unexpected error: '||g_pkg_name||'.'||l_api_name||': '||SQLERRM);
    END IF;
    x_return_status := FND_API.g_ret_sts_unexp_error;    
  END incremental_backflush;
  

  /*FPBug#4667093 Code added to consider new gme parameter for calculating factor */
  PROCEDURE derive_factor
    (p_material_detail_rec   IN         gme_material_details%ROWTYPE
    ,p_qty                   IN         NUMBER
    ,p_qty_type              IN         NUMBER
    ,p_gme_ib_factor         IN         NUMBER DEFAULT 0
    ,x_pct_plan              OUT NOCOPY NUMBER
    ,x_pct_plan_res          OUT NOCOPY NUMBER
    ,x_return_status         OUT NOCOPY VARCHAR2) IS

    l_required_qty             NUMBER;
    l_old_actual_qty           NUMBER;
    l_new_actual               NUMBER;
    l_api_name        CONSTANT VARCHAR2 (30)   := 'derive_factor';

    --FPBug#4667093    
    l_gme_ib_factor            NUMBER;    
    l_actual_qty               NUMBER;
    l_plan_qty                 NUMBER;
    l_uom                      VARCHAR2(3);
    l_return_status            VARCHAR2(1);
    
    l_incr_qty                 NUMBER;

    ERROR_IN_GET_TOTAL_QTY     EXCEPTION;  

    -- p_qty_type 0 increment qty
    -- p_qty_type 1 new act qty
    -- p_qty_type 2 % plan

  BEGIN

    IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
       gme_debug.put_line('Entering api '||g_pkg_name||'.'||l_api_name);
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' material_detail_id = '||p_material_detail_rec.material_detail_id);
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_qty = '||p_qty);
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_qty_type = '||p_qty_type);
       --FPBug#4667093 rework
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||'p_gme_ib_factor = '||p_gme_ib_factor||' line type='||p_material_detail_rec.line_type);
    END IF;

    /* Set the return status to success initially */
    x_return_status       := FND_API.G_RET_STS_SUCCESS;

    l_old_actual_qty := p_material_detail_rec.actual_qty;
    
    --FPBug#4667093 Begin    
    l_gme_ib_factor := p_gme_ib_factor;
    IF l_gme_ib_factor = 1 AND p_material_detail_rec.line_type = 1 THEN     
       gme_api_grp.get_total_qty(
                    p_batch_id           => p_material_detail_rec.batch_id
                   ,p_line_type          => 1
                   ,p_uom                => p_material_detail_rec.dtl_um
                   ,x_total_plan_qty     => l_plan_qty      
		             /*Bug#5111078 we should take wip plan qty while deriving the factor */
		             ,x_total_wip_plan_qty => l_required_qty  
                   ,x_total_actual_qty   => l_actual_qty
                   ,x_uom                => l_uom
                   ,x_return_status      => l_return_status );
       
       IF l_return_status <> FND_API.G_RET_STS_SUCCESS THEN
          RAISE ERROR_IN_GET_TOTAL_QTY;
       END IF;
    ELSE
       l_plan_qty := p_material_detail_rec.plan_qty;      
       l_required_qty := p_material_detail_rec.wip_plan_qty;
       
       -- Bug 22217179 - corrected this during testing.      
       IF l_plan_qty = 0 THEN
          l_plan_qty := l_required_qty; -- Need for inserts into batch where plan qty = 0.
       END IF;      
    END IF;

    IF nvl(g_debug,-1) <= gme_debug.g_log_statement THEN
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||'l_required_qty: '||l_required_qty);
    END IF;
    --FPBug#4667093 End

    -- x_pct_plan is the percent to be applied to WIP plan to come up with the incremental qty
    -- to add to actual; examples in each

    -- Note: l_required_qty = wip plan qty or sum of wip plan qty for all prods.
    IF p_qty_type = 0 THEN
       -- p_qty : 20  --> increment qty
       -- wip_plan_qty : 100
       -- x_pct_plan : 20 %
       -- incremental qty : wip_plan_qty * x_pct_plan --> 20% * 100 = 20; this is applied to each of the items
       -- actual qty : 40
       -- new actual : 60
       
       x_pct_plan :=  (p_qty / l_required_qty ) * 100;
       
       -- Bug 10378355 - Account for change between plan and wip plan qty. 
       -- Steps do not have wip plan qty's so % has to account for that.
       -- x_pct_plan_res := ( p_qty  / l_required_qty ) * 100;
       
       -- New % is    ((incr qty / wip_plan) * (wip_plan / plan)) * 100
       x_pct_plan_res := (( p_qty  / l_required_qty ) * (l_required_qty / l_plan_qty)) * 100;            
    ELSIF p_qty_type = 1 THEN
       -- p_qty : 60  --> new actual qty
       -- actual_qty : 40
       -- wip_plan_qty : 100
       -- x_pct_plan : (60 - 40)/100 = 20%
       -- incremental qty : wip_plan_qty * x_pct_plan --> 20% * 100 = 20; this is applied to each of the items
       -- actual qty : 40
       -- new actual : 60
       
       l_incr_qty := p_qty - l_old_actual_qty;
       -- x_pct_plan := ((p_qty - l_old_actual_qty)/l_required_qty) * 100; 
       x_pct_plan := (l_incr_qty/l_required_qty) * 100; 
       
       -- Bug 10378355 - Account for change between plan and wip plan qty. 
       -- Steps do not have wip plan qty's so % has to account for that.
       -- x_pct_plan_res := ((p_qty - l_old_actual_qty)/l_required_qty) * 100;
       
       -- New % is    ((incr qty / wip_plan) * (wip_plan / plan)) * 100
       x_pct_plan_res := (( l_incr_qty  / l_required_qty ) * (l_required_qty / l_plan_qty)) * 100;                  
    ELSIF p_qty_type = 2 THEN
       -- p_qty : 60%  --> % wip plan
       -- wip_plan_qty : 100
       -- new actual qty : 60% * 100 = 60
       -- x_pct_plan : (60 - 40) / 100 = 20%
       -- incremental qty : wip_plan_qty * x_pct_plan --> 20% * 100 = 20; this is applied to each of the items
       -- actual qty : 40
       -- new actual : 60
       
       l_new_actual := (l_required_qty * p_qty) / 100;
       l_incr_qty := l_new_actual - l_old_actual_qty;
       
       -- x_pct_plan := ((l_new_actual - l_old_actual_qty)/l_required_qty) * 100;
       x_pct_plan := (l_incr_qty/l_required_qty) * 100;
       
       -- Bug 10378355 - Account for change between plan and wip plan qty. 
       -- Steps do not have wip plan qty's so % has to account for that.
       -- x_pct_plan_res := p_qty - ((l_old_actual_qty/l_required_qty))*100 ; -- This may have been wrong anyway !
       
       -- New % is    ((incr qty / wip_plan) * (wip_plan / plan)) * 100
       x_pct_plan_res := (( l_incr_qty  / l_required_qty ) * (l_required_qty / l_plan_qty)) * 100;                  
    END IF;
    
    IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
       gme_debug.put_line (g_pkg_name||'.'||l_api_name||' x_pct_plan = '||x_pct_plan);
    END IF;
    
    IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
       gme_debug.put_line('Exiting api '||g_pkg_name||'.'||l_api_name||' with x_return_status = '||x_return_status);
    END IF;

  EXCEPTION
    --FPBug#4667093
    WHEN ERROR_IN_GET_TOTAL_QTY THEN
      x_return_status := l_return_status;
    WHEN OTHERS THEN
      fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
      IF g_debug <= gme_debug.g_log_procedure THEN
        gme_debug.put_line ('Unexpected error: '||g_pkg_name||'.'||l_api_name||': '||SQLERRM);
      END IF;
      x_return_status := FND_API.g_ret_sts_unexp_error;    
  END derive_factor;

  PROCEDURE update_dependent_steps(p_batchstep_id     IN  NUMBER
                                  ,p_backflush_factor IN  NUMBER
                                  ,x_return_status    OUT NOCOPY VARCHAR2) IS
                                  
    l_api_name        CONSTANT VARCHAR2 (30)   := 'update_dependent_steps';
    
    CURSOR Cur_prev_steps(V_batchstep_id NUMBER) IS
      SELECT d.dep_step_id, d.dep_type, s.step_status
      FROM   gme_batch_step_dependencies d, gme_batch_steps s
      WHERE  d.batchstep_id = V_batchstep_id
      AND    s.batchstep_id = d.dep_step_id;
      
    l_batch_step_rec           GME_BATCH_STEPS%ROWTYPE;
    l_in_batch_step_rec        GME_BATCH_STEPS%ROWTYPE;

    l_message_count        NUMBER;
    l_backflush_factor     NUMBER;
    l_message_list         VARCHAR2(2000);
    

    error_updating_steps   EXCEPTION;

  BEGIN
    IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
      gme_debug.put_line('Entering api '||g_pkg_name||'.'||l_api_name);
      gme_debug.put_line(g_pkg_name||'.'||l_api_name||' batchstep_id='||p_batchstep_id);
    END IF;

    -- Set the return status to success initially
    x_return_status := FND_API.G_RET_STS_SUCCESS;

    -- Get all previous depenedent steps for current step

    FOR get_rec IN Cur_prev_steps(p_batchstep_id) LOOP
      -- If dependency is start to start and step is WIP then apply factor and process steps that
      -- this step is dependent on, Pending and completed steps will not be touched
      -- If dependency is finish to start and step is WIP then apply 100% of plan to actual and process steps that
      -- this step is dependent on, Pending and completed steps will not be touched
      IF (get_rec.step_status = gme_common_pvt.g_step_wip) THEN
        IF (get_rec.dep_type = gme_common_pvt.g_dep_type_finish_start) THEN
          l_backflush_factor := 1;
        ELSE
          l_backflush_factor := p_backflush_factor;
        END IF;
        l_in_batch_step_rec.batchstep_id := get_rec.dep_step_id;
 
          gme_update_step_qty_pvt.update_step_qty
              (p_batch_step_rec          => l_in_batch_step_rec
              ,x_message_count           => l_message_count
              ,x_message_list            => l_message_list
              ,x_return_status           => x_return_status
              ,x_batch_step_rec          => l_batch_step_rec
              ,p_backflush_factor        => l_backflush_factor
              ,p_dependency_type         => get_rec.dep_type);

          IF x_return_status <> FND_API.G_RET_STS_SUCCESS THEN
            RAISE error_updating_steps;
          END IF; 
        update_dependent_steps
            (p_batchstep_id     => get_rec.dep_step_id
            ,p_backflush_factor => p_backflush_factor
            ,x_return_status    => x_return_status);

        IF x_return_status <> FND_API.G_RET_STS_SUCCESS THEN
          RAISE error_updating_steps;
        END IF;
      END IF;
    END LOOP;

    IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
      gme_debug.put_line('Exiting api '||g_pkg_name||'.'||l_api_name);
    END IF;

  EXCEPTION
    WHEN error_updating_steps THEN
      NULL;
    WHEN OTHERS THEN
      fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
      IF g_debug <= gme_debug.g_log_procedure THEN
        gme_debug.put_line ('Unexpected error: '||g_pkg_name||'.'||l_api_name||': '||SQLERRM);
      END IF;
      x_return_status := FND_API.g_ret_sts_unexp_error;    
  END update_dependent_steps;

  PROCEDURE revert_material_partial
    (p_material_detail_rec        IN gme_material_details%ROWTYPE
    ,p_qty                        IN NUMBER
    ,p_lot_control_code           IN NUMBER  -- 1 = not lot control; 2 = lot control
    ,p_create_resv_pend_lots      IN NUMBER
    ,p_lot_divisible_flag         IN VARCHAR2
    ,x_actual_qty                 OUT NOCOPY NUMBER
    ,x_exception_material_tbl     IN OUT NOCOPY gme_common_pvt.exceptions_tab
    ,x_return_status              OUT NOCOPY VARCHAR2) IS

    CURSOR cur_lot_qty
        (v_item_id           IN   NUMBER
        ,v_organization_id   IN   NUMBER
        ,v_batch_id          IN   NUMBER
        ,v_mat_det_id        IN   NUMBER)
      IS
           SELECT lot_number, SUM (l.transaction_quantity) sum_trx
             FROM mtl_material_transactions m, mtl_transaction_lot_numbers l
            WHERE l.transaction_id = m.transaction_id
              AND m.inventory_item_id = v_item_id
              AND m.organization_id = v_organization_id
              AND m.transaction_source_id = v_batch_id
              AND m.trx_source_line_id = v_mat_det_id
              AND m.transaction_source_type_id = gme_common_pvt.g_txn_source_type
         GROUP BY l.lot_number;

    TYPE lot_qty_tab IS TABLE OF NUMBER INDEX BY mtl_transaction_lot_numbers.lot_number%TYPE;

    l_lot_qty_tab     lot_qty_tab;
    l_last_lot_qty_tab lot_qty_tab;
    
    l_qty             NUMBER;
    l_last_qty        NUMBER;
    
    l_decr_qty        NUMBER;
    l_total_decr_qty  NUMBER;
    
    l_trxn_type       NUMBER;
    l_trxn_qty        NUMBER;
    l_trxn_sum        NUMBER;
    l_temp_qty        NUMBER;
    l_whole_qty       BOOLEAN;
    l_return_status   VARCHAR2(1);
    l_trxn_success    BOOLEAN;

    i                 NUMBER;
    j                 NUMBER;
    k                 NUMBER;

    -- Bug 13017256
    l_trans_date      DATE;

    l_mmt_rec         mtl_material_transactions%ROWTYPE;
    l_mmt_tbl         gme_common_pvt.mtl_mat_tran_tbl;

    l_mmln_tbl        gme_common_pvt.mtl_trans_lots_num_tbl;
    l_mmln_tbl_orig   gme_common_pvt.mtl_trans_lots_num_tbl;
    l_mmln_tbl_new    gme_common_pvt.mtl_trans_lots_num_tbl;
    
    -- Bug 9072371 add this table to allow checking for one lot.
    l_mmln_tbl_lot    gme_common_pvt.mtl_trans_lots_num_tbl;
    ln                mtl_transaction_lot_numbers.lot_number%TYPE;
    l_lot_number      mtl_transaction_lot_numbers.lot_number%TYPE;
    /* Jalaj Srivastava Bug 5021522*/
    l_item_no         varchar2(2000);
    inv_negative      EXCEPTION;

    l_reservable_type NUMBER; -- Bug 19868921
    l_txn_hdr_tbl_cnt NUMBER; 
    
    l_api_name        CONSTANT VARCHAR2 (30)   := 'revert_material_partial';
         
    error_get_lot       EXCEPTION;
    error_get_trans     EXCEPTION;
    error_trans         EXCEPTION;
    
    l_lot_record_used NUMBER;
    l_skip_lot        NUMBER;
  BEGIN

    IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
       gme_debug.put_line('Entering api '||g_pkg_name||'.'||l_api_name);
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' material_detail_id = '||p_material_detail_rec.material_detail_id);
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_qty = '||p_qty);
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_lot_control_code = '||p_lot_control_code);
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_create_resv_pend_lots = '||p_create_resv_pend_lots);
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_lot_divisible_flag = '||p_lot_divisible_flag);
    END IF;

    -- Set the return status to success initially
    x_return_status := FND_API.G_RET_STS_SUCCESS;
    
    l_qty := p_qty;
    l_total_decr_qty := 0;

    -- Bug 13017256 - Let's initialize the variable with the user entered date.
    l_trans_date := NVL(gme_common_pvt.g_ib_timestamp_date, gme_common_pvt.g_timestamp);

    -- Bug 8751983 - Added order by clause to fetch in reverse trans order.
    gme_transactions_pvt.get_mat_trans
        (p_mat_det_id      => p_material_detail_rec.material_detail_id
        ,p_batch_id        => p_material_detail_rec.batch_id
        ,p_order_by        => 2
        ,x_mmt_tbl         => l_mmt_tbl
        ,x_return_status   => x_return_status);

    IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
       gme_debug.put_line (g_pkg_name||'.'||l_api_name||' gme_transactions_pvt.get_mat_trans returned '||
                           l_mmt_tbl.count||' trxns with return_status = '||x_return_status);
    END IF;

    IF x_return_status <> FND_API.G_RET_STS_SUCCESS THEN
       RAISE error_get_trans;
    END IF;

    -- Make all qties positive for comparison; if updating, the trxn mgr will take care of sign    
    FOR i IN 1..l_mmt_tbl.count LOOP
       l_mmt_tbl(i).transaction_quantity := ABS(l_mmt_tbl(i).transaction_quantity);
    END LOOP;

    -- set the transaction type to consider for deleting/updating
    IF p_material_detail_rec.line_type = gme_common_pvt.g_line_type_prod THEN
       l_trxn_type := gme_common_pvt.g_prod_completion;
    ELSIF p_material_detail_rec.line_type = gme_common_pvt.g_line_type_byprod THEN
       l_trxn_type := gme_common_pvt.g_byprod_completion;
    ELSE
       l_trxn_type := gme_common_pvt.g_ing_issue;
    END IF;
    
    /* Jalaj Srivastava Bug 5021522
       get item_no only for prod/byprod. 
       In IB, there is no ing return transaction*/
    -- Bug 19868921 - retrieve reservable_type also and for all trxn types.       
    -- IF (l_trxn_type IN (gme_common_pvt.g_prod_completion, gme_common_pvt.g_byprod_completion)) THEN
    
       SELECT concatenated_segments, reservable_type
         INTO l_item_no, l_reservable_type
         FROM mtl_system_items_kfv
        WHERE inventory_item_id = p_material_detail_rec.inventory_item_id
          AND organization_id   = p_material_detail_rec.organization_id;
       
    -- END IF;

    IF p_material_detail_rec.line_type = gme_common_pvt.g_line_type_ing THEN
       IF p_lot_divisible_flag = 'N' THEN
          l_whole_qty := TRUE;
          IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
             gme_debug.put_line (g_pkg_name||'.'||l_api_name||' l_whole_qty = TRUE because lot indivisible item');
          END IF;
       ELSE
          l_whole_qty := FALSE;
          IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
             gme_debug.put_line (g_pkg_name||'.'||l_api_name||' l_whole_qty = FALSE because lot is divisible; next check if dispensed');
          END IF;
       END IF;
       
       -- test again for dispensed items
       IF NOT l_whole_qty THEN
          IF NVL(p_material_detail_rec.dispense_ind,'N') = 'Y' THEN
             l_whole_qty := TRUE;
             IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
                gme_debug.put_line (g_pkg_name||'.'||l_api_name||' l_whole_qty = TRUE because dispensed item');
             END IF;
          END IF;
       END IF;
    ELSE -- product and byproduct doesn't apply for either dispensed or lot divisible... don't have to take whole qty
       l_whole_qty := FALSE;
       IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
          gme_debug.put_line (g_pkg_name||'.'||l_api_name||' l_whole_qty = FALSE because prod/byprod');
       END IF;
    END IF;

    IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
       gme_debug.put_line (g_pkg_name||'.'||l_api_name||' l_trxn_type = '||l_trxn_type);
    END IF;

    IF p_lot_control_code = 1 THEN

       IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
          gme_debug.put_line (g_pkg_name||'.'||l_api_name||' ********** NOT LOT CONTROL **********');
       END IF;
       
       i := 1;
       WHILE i <= l_mmt_tbl.count AND l_qty > 0 LOOP
          l_mmt_rec := l_mmt_tbl(i);
          
          IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
             gme_debug.put_line (g_pkg_name||'.'||l_api_name||' in trxn loop; ******************* ');
             gme_debug.put_line (g_pkg_name||'.'||l_api_name||' in trxn loop; processing i = '||i);
             gme_debug.put_line (g_pkg_name||'.'||l_api_name||' in trxn loop; qty to reduce = '||l_qty);
             gme_debug.put_line (g_pkg_name||'.'||l_api_name||' in trxn loop; processing trxns_id = '||l_mmt_rec.transaction_id);
             gme_debug.put_line (g_pkg_name||'.'||l_api_name||' in trxn loop; l_trxn_type = '||l_trxn_type);
             gme_debug.put_line (g_pkg_name||'.'||l_api_name||' in trxn loop; l_mmt_rec.transaction_type_id = '||l_mmt_rec.transaction_type_id);
          END IF;
         
          IF l_mmt_rec.transaction_type_id = l_trxn_type THEN
             l_trxn_qty := l_mmt_rec.transaction_quantity;
             
             IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
                gme_debug.put_line (g_pkg_name||'.'||l_api_name||' in trxn loop; l_trxn_qty = '||l_trxn_qty);
                gme_debug.put_line (g_pkg_name||'.'||l_api_name||' in trxn loop; l_qty = '||l_qty);
             END IF;
             
             IF l_trxn_qty <= l_qty OR l_whole_qty THEN
                /* Jalaj Srivastava Bug 5021522
                   call check_inv_negative only for prod/byprod. in IB, there is no ing return transaction */                   
                IF (l_trxn_type IN (gme_common_pvt.g_prod_completion, gme_common_pvt.g_byprod_completion)) THEN
                   -- Bug 8639523 - No need to do negative inventory checking for phantom prods or ingredients.
                   IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
                      gme_debug.put_line ('p_material_detail_rec.phantom_line_id is '||NVL(p_material_detail_rec.phantom_line_id, 0));
                   END IF;
                   
                   IF (NVL(p_material_detail_rec.phantom_line_id, 0) = 0) THEN
                      IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
                         gme_debug.put_line ('calling gme_unrelease_batch_pvt.check_inv_negative');
                      END IF;
                   
                      IF gme_unrelease_batch_pvt.check_inv_negative
                        ( p_mmt_rec   => l_mmt_rec
                         ,p_mmln_tbl  => l_mmln_tbl
                         ,p_item_no   => l_item_no) THEN
                         
                         IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
                            gme_debug.put_line ('error after calling gme_unrelease_batch_pvt.check_inv_negative');
                         END IF;
                         
                         RAISE inv_negative;
                      END IF;
                   END IF;
                END IF;
             
                IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
                   gme_debug.put_line (g_pkg_name||'.'||l_api_name||' in trxn loop; calling gme_transactions_pvt.delete_material_txn for trxns_id='||l_mmt_rec.transaction_id);
                END IF;
               
                -- Bug 13017256 - Let's pass in user entered date. It will get used if necessary.
                -- delete this transaction, reduce the qty to decrement
                gme_transactions_pvt.delete_material_txn
                  (p_transaction_id       => l_mmt_rec.transaction_id
                  ,p_trans_date           => l_trans_date
                  ,p_txns_pair            => NULL
                  ,x_return_status        => l_return_status);

                IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
                   gme_debug.put_line ('gme_transactions_pvt.delete_material_txn l_return_status = '||l_return_status);
                END IF;
             
                IF l_return_status = gme_common_pvt.g_not_transactable THEN
                   -- do nothing... move on to the next, to try to reduce
                   IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
                      gme_debug.put_line (g_pkg_name||'.'||l_api_name||' in trxn loop; gme_transactions_pvt.delete_material_txn returned '||l_return_status);
                   END IF;
                ELSIF l_return_status <> FND_API.G_RET_STS_SUCCESS THEN
                   x_return_status := l_return_status;
                   RAISE error_trans;
                ELSE -- success
                   l_total_decr_qty := l_total_decr_qty + l_trxn_qty;
                   -- set target qty; need to test in case this is a whole qty revert and the qty reverting is greater than that requested.
                   IF l_trxn_qty < l_qty THEN
                      l_qty := l_qty - l_trxn_qty;
                   ELSE
                      l_qty := 0;
                   END IF;
                 
                   -- Bug 18846633 - Changed code to respect parameter value passed in.
                   IF p_create_resv_pend_lots = 1 THEN
                      -- Bug 19868921 - Initialize rsrv table structure and also the rsrv quantity for ingredients.
                      -- These will get rereserved later in gme_post_process as part of save.	
                      IF p_material_detail_rec.line_type = gme_common_pvt.g_line_type_ing AND l_reservable_type = 1 THEN                      
	                      l_txn_hdr_tbl_cnt := gme_common_pvt.g_mat_txn_hdr_tbl.COUNT;
                         gme_common_pvt.g_mat_txn_hdr_tbl(l_txn_hdr_tbl_cnt).txn_header_id := l_mmt_rec.transaction_id;
                         gme_common_pvt.g_mat_txn_hdr_tbl(l_txn_hdr_tbl_cnt).material_dtl_id := p_material_detail_rec.material_detail_id;                
                         gme_common_pvt.g_mat_txn_hdr_tbl(l_txn_hdr_tbl_cnt).rsrv_quantity := NULL; -- this implies full quantity.
                      ELSE         
                         gme_unrelease_batch_pvt.create_resv_pplot
                                 (p_material_detail_rec    => p_material_detail_rec 
                                 ,p_mmt_rec                => l_mmt_rec
                                 ,p_mmln_tbl               => l_mmln_tbl
                                 ,x_return_status          => l_return_status);
                            -- don't care if resv or pplot was not recreated...
                      END IF;
                   END IF;
                END IF;
             ELSE  -- l_trxn_qty > l_qty AND NOT whole_qty          
                l_mmt_rec.transaction_quantity := l_mmt_rec.transaction_quantity - l_qty;
                l_mmt_rec.secondary_transaction_quantity := NULL;
                l_mmt_rec.primary_quantity := NULL;
                
                -- Bug 13017256 - Let's stamp the trans record with user entered date.            
                IF NOT gme_common_pvt.check_close_period(p_org_id     => l_mmt_rec.organization_id
                                                        ,p_trans_date => l_mmt_rec.transaction_date) THEN
                
                   -- Let's default to timestamp and overwrite if the user entered a different date.
                   l_mmt_rec.transaction_date := l_trans_date;
                END IF;            
                
                /* Jalaj Srivastava Bug 5021522
                   call check_inv_negative only for prod/byprod. 
                   in IB, there is no ing return transaction*/
                IF (l_trxn_type IN (gme_common_pvt.g_prod_completion, gme_common_pvt.g_byprod_completion)) THEN
                   IF gme_unrelease_batch_pvt.check_inv_negative
                     ( p_mmt_rec   => l_mmt_rec
                      ,p_mmln_tbl  => l_mmln_tbl
                      ,p_item_no   => l_item_no) THEN
                      RAISE inv_negative;
                   END IF;
                END IF;
                
                IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
                   gme_debug.put_line (g_pkg_name||'.'||l_api_name||' in trxn loop; calling gme_transactions_pvt.update_material_txn for trxns_id = '||
                                       l_mmt_rec.transaction_id);
                   gme_debug.put_line (g_pkg_name||'.'||l_api_name||' in trxn loop; update trxn with qty = '||l_mmt_rec.transaction_quantity);
                END IF;
          
                gme_transactions_pvt.update_material_txn
                  (p_mmt_rec         => l_mmt_rec
                  ,p_mmln_tbl        => l_mmln_tbl
                  ,x_return_status   => l_return_status);
                  
                IF l_return_status = gme_common_pvt.g_not_transactable THEN
                   -- do nothing... move on to the next, to try to reduce
                   IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
                      gme_debug.put_line (g_pkg_name||'.'||l_api_name||' in trxn loop; gme_transactions_pvt.update_material_txn returned '||l_return_status);
                   END IF;
                ELSIF l_return_status <> FND_API.G_RET_STS_SUCCESS THEN
                   x_return_status := l_return_status;
                   RAISE error_trans;
                ELSE -- success
                   l_total_decr_qty := l_total_decr_qty + l_qty;
                   
                   -- set the transaction qty back to what was decremented... this record won't be used again
                   l_mmt_rec.transaction_quantity := l_qty;
                   l_qty := 0;
                   
                   l_return_status := FND_API.G_RET_STS_SUCCESS;
                   IF l_mmt_rec.secondary_uom_code IS NOT NULL THEN
                      -- also need to set the secondary_transaction_quantity if applicable
                      get_converted_qty (
                          p_org_id                    => p_material_detail_rec.organization_id
                         ,p_item_id                   => p_material_detail_rec.inventory_item_id
                         ,p_lot_number                => NULL
                         ,p_qty                       => l_mmt_rec.transaction_quantity
                         ,p_from_um                   => p_material_detail_rec.dtl_um
                         ,p_to_um                     => l_mmt_rec.secondary_uom_code
                         ,x_conv_qty                  => l_mmt_rec.secondary_transaction_quantity
                         ,x_return_status             => l_return_status);
                   END IF;
                               
                   -- Bug 18846633 - Changed code to respect parameter value passed in.
                   IF l_return_status = FND_API.G_RET_STS_SUCCESS AND p_create_resv_pend_lots = 1 THEN
                      -- Bug 19868921 - Initialize rsrv table structure and also the rsrv quantity for ingredients.
                      -- These will get rereserved later in gme_post_process as part of save.	
                      IF p_material_detail_rec.line_type = gme_common_pvt.g_line_type_ing AND l_reservable_type = 1 THEN                      
	                      l_txn_hdr_tbl_cnt := gme_common_pvt.g_mat_txn_hdr_tbl.COUNT;
                         gme_common_pvt.g_mat_txn_hdr_tbl(l_txn_hdr_tbl_cnt).txn_header_id := l_mmt_rec.transaction_id;
                         gme_common_pvt.g_mat_txn_hdr_tbl(l_txn_hdr_tbl_cnt).material_dtl_id := p_material_detail_rec.material_detail_id;                
                         gme_common_pvt.g_mat_txn_hdr_tbl(l_txn_hdr_tbl_cnt).rsrv_quantity := l_mmt_rec.transaction_quantity;		
                      ELSE         
                         gme_unrelease_batch_pvt.create_resv_pplot
                              (p_material_detail_rec    => p_material_detail_rec 
                              ,p_mmt_rec                => l_mmt_rec
                              ,p_mmln_tbl               => l_mmln_tbl
                              ,x_return_status          => l_return_status);
                      END IF;
                      -- don't care if resv or pplot was not recreated...
                   END IF;          
                END IF;          
             END IF;  -- IF l_trxn_qty <= l_qty THEN
          ELSE
             IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
                gme_debug.put_line (g_pkg_name||'.'||l_api_name||' cant process because not the same transaction_type_id: trxns_id = '||
                                    l_mmt_rec.transaction_id);
             END IF;
          END IF; -- IF l_mmt_rec.transaction_type_id = l_trxn_type THEN
         
          i := i + 1;
       
       END LOOP;
    ELSE  -- p_lot_control_code = 2 which means lot control    
       IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
          gme_debug.put_line (g_pkg_name||'.'||l_api_name||' ********** LOT CONTROL **********');
          gme_debug.put_line (g_pkg_name||'.'||l_api_name||' right before cursor cur_lot_qty');
       END IF;
       
       FOR cur_lot_qty_rec IN cur_lot_qty(
                        p_material_detail_rec.inventory_item_id
                       ,p_material_detail_rec.organization_id
                       ,p_material_detail_rec.batch_id
                       ,p_material_detail_rec.material_detail_id) LOOP
          l_lot_qty_tab(cur_lot_qty_rec.lot_number) := ABS(cur_lot_qty_rec.sum_trx);
          
          IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
             gme_debug.put_line (g_pkg_name||'.'||l_api_name||' lot tab index: lot_number: '||cur_lot_qty_rec.lot_number);
             gme_debug.put_line (g_pkg_name||'.'||l_api_name||' lot tab qty sum: '||l_lot_qty_tab(cur_lot_qty_rec.lot_number));
          END IF;
       END LOOP;
      
       -- loop the trxns checking each lot trxn against lot sum to see if 
       -- that lot trxn can be used.
       -- loop trxn types based on matl type
       -- decrement lot sum and l_qty
       
       i := 1;
       WHILE i <= l_mmt_tbl.count AND l_qty > 0 LOOP
          l_mmt_rec := l_mmt_tbl(i);
          
          IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
             gme_debug.put_line (g_pkg_name||'.'||l_api_name||' in trxn loop; ******************* ');
             gme_debug.put_line (g_pkg_name||'.'||l_api_name||' in trxn loop; processing i = '||i);
             gme_debug.put_line (g_pkg_name||'.'||l_api_name||' in trxn loop; processing trxns_id = '||l_mmt_rec.transaction_id);
             gme_debug.put_line (g_pkg_name||'.'||l_api_name||' in trxn loop; qty to reduce = '||l_qty);
          END IF;
        
          IF l_mmt_rec.transaction_type_id = l_trxn_type THEN
             -- look at each lot in the table
             -- decrement in place what is available according to l_lot_qty_tab
             -- if all qties are 0 delete the transaction
             -- if any lot transactions remain, transaction must be updated
             
             gme_transactions_pvt.get_lot_trans
                   (p_transaction_id      => l_mmt_rec.transaction_id
                   ,x_mmln_tbl            => l_mmln_tbl
                   ,x_return_status       => x_return_status);
             
             IF x_return_status <> FND_API.G_RET_STS_SUCCESS THEN
                RAISE error_get_lot;
             END IF;
             
             -- Make all lot qties positive for comparison; if updating, the trxn mgr will take care of sign    
             FOR i IN 1..l_mmln_tbl.count LOOP
                l_mmln_tbl(i).transaction_quantity := ABS(l_mmln_tbl(i).transaction_quantity);
             END LOOP;
             
             l_decr_qty := 0;
             j:= 1;
             
             IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
                gme_debug.put_line (g_pkg_name||'.'||l_api_name||' before lot loop; l_mmln_tbl.COUNT = '||l_mmln_tbl.COUNT);
             END IF;
            
             -- keep last successful qties; in case update or delete is not successful, and you have to go back
             l_last_lot_qty_tab := l_lot_qty_tab;
             l_last_qty := l_qty;
             l_mmln_tbl_orig := l_mmln_tbl;
             
             -- Bug 9072371 - Check potential negative inventory for each specific lot. 
             l_lot_record_used := 0;
             WHILE j <= l_mmln_tbl.COUNT AND l_qty > 0 LOOP
                IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
                   gme_debug.put_line (g_pkg_name||'.'||l_api_name||' in lot loop; j= '||j);
                   gme_debug.put_line (g_pkg_name||'.'||l_api_name||' in lot loop; l_decr_qty = '||l_decr_qty);
                END IF;
                
                -- Bug 9072371 - Check potential negative inventory for each specific lot. 
                -- Continue to next lot if this one cannot be used.
                l_skip_lot := 0;
                IF (l_trxn_type IN (gme_common_pvt.g_prod_completion, gme_common_pvt.g_byprod_completion)) THEN
                   l_mmln_tbl_lot(1) := l_mmln_tbl(j);
                   IF gme_unrelease_batch_pvt.check_inv_negative
                      ( p_mmt_rec   => l_mmt_rec
                       ,p_mmln_tbl  => l_mmln_tbl_lot
                       ,p_item_no   => l_item_no) THEN
                      l_skip_lot := 1;
                   END IF;
                END IF;

                -- Bug 9072371 - Don't process this lot if it will drive inventory negative. 
                IF l_skip_lot = 0 THEN
                   ln := l_mmln_tbl(j).lot_number;
                   
                   IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
                      gme_debug.put_line (g_pkg_name||'.'||l_api_name||' in lot loop; processing lot number = '||ln);
                      gme_debug.put_line (g_pkg_name||'.'||l_api_name||' in lot loop; sum lot trxn for this lot = '||l_lot_qty_tab(ln));
                      gme_debug.put_line (g_pkg_name||'.'||l_api_name||' in lot loop; l_qty = '||l_qty);
                   END IF;
                   
                   -- if you need to take the entire lot qty, then take it... else, go on to figure out how much you can take
                   IF l_whole_qty THEN
                      l_temp_qty := l_mmln_tbl(j).transaction_quantity;
                   ELSE
                      IF l_mmln_tbl(j).transaction_quantity < l_qty THEN
                         IF l_lot_qty_tab(ln) < l_mmln_tbl(j).transaction_quantity THEN
                            l_temp_qty := l_lot_qty_tab(ln);
                         ELSE
                            l_temp_qty := l_mmln_tbl(j).transaction_quantity;
                         END IF;
                      ELSE  -- l_qty is less than the transaction qty; still need to compare to lot sum
                         IF l_lot_qty_tab(ln) < l_qty THEN
                            l_temp_qty := l_lot_qty_tab(ln);
                         ELSE
                            l_temp_qty := l_qty;
                         END IF;
                      END IF;
                   END IF;
               
                   IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
                      gme_debug.put_line (g_pkg_name||'.'||l_api_name||' in lot loop; reducing lot qty by = '||l_temp_qty);
                   END IF;
                   
                   l_mmln_tbl(j).transaction_quantity := l_mmln_tbl(j).transaction_quantity - l_temp_qty;
                   l_mmln_tbl(j).primary_quantity := NULL;
                   l_mmln_tbl(j).secondary_transaction_quantity := NULL;
                   l_lot_qty_tab(ln) := l_lot_qty_tab(ln) - l_temp_qty;
            
                   -- following can happen if whole qty is taken, decr may be greater than that requested
                   IF l_temp_qty < l_qty THEN
                      l_qty := l_qty - l_temp_qty;
                   ELSE
                      l_qty := 0;
                   END IF;
                   
                   l_decr_qty := l_decr_qty + l_temp_qty;                   
                   
                   -- Bug 9072371 - Set flag which says at least one valid lot record was found. 
                   l_lot_record_used := 1;
                   
                   IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
                      gme_debug.put_line (g_pkg_name||'.'||l_api_name||' in lot loop; l_mmln_tbl(j).transaction_quantity = '||
                                          l_mmln_tbl(j).transaction_quantity);
                      gme_debug.put_line (g_pkg_name||'.'||l_api_name||' in lot loop; l_lot_qty_tab(ln) = '||l_lot_qty_tab(ln));
                      gme_debug.put_line (g_pkg_name||'.'||l_api_name||' in lot loop; l_qty = '||l_qty);
                      gme_debug.put_line (g_pkg_name||'.'||l_api_name||' in lot loop; l_decr_qty = '||l_decr_qty);
                   END IF;
                END IF; -- skip lot check.               
                j := j + 1;
             END LOOP;
          
             IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
                gme_debug.put_line (g_pkg_name||'.'||l_api_name||' after lot loop; l_decr_qty = '||l_decr_qty);
                gme_debug.put_line (g_pkg_name||'.'||l_api_name||' after lot loop; l_mmt_rec.transaction_quantity = '||l_mmt_rec.transaction_quantity);
             END IF;
          
             -- Bug 9072371 - If at least one valid lot record was found then process. 
             IF l_lot_record_used = 1 THEN          
                IF l_decr_qty = l_mmt_rec.transaction_quantity THEN                
                   /* Jalaj Srivastava Bug 5021522
                      call check_inv_negative only for prod/byprod. 
                     in IB, there is no ing return transaction*/
                   IF (l_trxn_type IN (gme_common_pvt.g_prod_completion, gme_common_pvt.g_byprod_completion)) THEN
                      IF gme_unrelease_batch_pvt.check_inv_negative
                         ( p_mmt_rec   => l_mmt_rec
                          ,p_mmln_tbl  => l_mmln_tbl_orig
                          ,p_item_no   => l_item_no) THEN
                         RAISE inv_negative;
                      END IF;
                   END IF;
                   
                   IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
                      gme_debug.put_line (g_pkg_name||'.'||l_api_name||' in trxn loop; calling gme_transactions_pvt.delete_material_txn with trxn_id='||
                                          l_mmt_tbl(i).transaction_id);
                   END IF;
                
                   -- Bug 13017256 - Let's pass in user entered date. It will get used if necessary.
                   -- delete this transaction, reduce the qty to decrement             
                   -- delete the transaction... all lots were used
                   gme_transactions_pvt.delete_material_txn
                     (p_transaction_id       => l_mmt_tbl(i).transaction_id
                     ,p_txns_pair            => NULL
                     ,p_trans_date           => l_trans_date
                     ,x_return_status        => l_return_status);
                   
                   IF l_return_status = gme_common_pvt.g_not_transactable THEN
                      l_trxn_success := FALSE;
                      -- do nothing... move on to the next, to try to reduce
                      IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
                         gme_debug.put_line (g_pkg_name||'.'||l_api_name||' in trxn loop; gme_transactions_pvt.delete_material_txn returned '||l_return_status);
                      END IF;
                   ELSIF l_return_status <> FND_API.G_RET_STS_SUCCESS THEN
                      x_return_status := l_return_status;
                      RAISE error_trans;
                   ELSE
                      l_trxn_success := TRUE;
                      -- Bug 18846633 - Changed code to respect parameter value passed in.
                      IF p_create_resv_pend_lots = 1 THEN
                         -- Bug 19868921 - Initialize rsrv table structure and also the rsrv quantity for ingredients.
                         -- These will get rereserved later in gme_post_process as part of save.	
                         IF p_material_detail_rec.line_type = gme_common_pvt.g_line_type_ing AND l_reservable_type = 1 THEN                      
	                         l_txn_hdr_tbl_cnt := gme_common_pvt.g_mat_txn_hdr_tbl.COUNT;
                            gme_common_pvt.g_mat_txn_hdr_tbl(l_txn_hdr_tbl_cnt).txn_header_id := l_mmt_rec.transaction_id;
                            gme_common_pvt.g_mat_txn_hdr_tbl(l_txn_hdr_tbl_cnt).material_dtl_id := p_material_detail_rec.material_detail_id;                
                            gme_common_pvt.g_mat_txn_hdr_tbl(l_txn_hdr_tbl_cnt).rsrv_quantity := NULL; -- This means full transaction was deleted.
                         ELSE                               
                            -- l_mmt_rec.transaction_quantity was not updated, so l_mmt_rec can be used with no modifications
                            gme_unrelease_batch_pvt.create_resv_pplot
                                    (p_material_detail_rec    => p_material_detail_rec 
                                    ,p_mmt_rec                => l_mmt_rec
                                    ,p_mmln_tbl               => l_mmln_tbl_orig
                                    ,x_return_status          => l_return_status);
                            -- don't care if resv or pplot was not recreated...
                         END IF;
                      END IF; 
                       
                      -- Bug 8751983/9072371 - Add to summary bucket.                 
                      l_total_decr_qty := l_total_decr_qty + l_decr_qty;                 
                   END IF;
                ELSE
                   -- update the transaction; some lots were left
                   -- copy the lots to a new lot table excluding any lot records that are zero
                   k := 1;
                   FOR j IN 1..l_mmln_tbl.COUNT LOOP
                      IF l_mmln_tbl(j).transaction_quantity <> 0 THEN
                         l_mmln_tbl_new(k) := l_mmln_tbl(j);
                         k := k + 1;
                      END IF;
                   END LOOP;               
                   
                   l_mmt_rec.transaction_quantity := l_mmt_rec.transaction_quantity - l_decr_qty;
                   l_mmt_rec.secondary_transaction_quantity := NULL;
                   l_mmt_rec.primary_quantity := NULL;
                   
                   -- Bug 13017256 - Let's stamp the trans record with user entered date.            
                   IF NOT gme_common_pvt.check_close_period(p_org_id     => l_mmt_rec.organization_id
                                                           ,p_trans_date => l_mmt_rec.transaction_date) THEN
                   
                      -- Let's default to timestamp and overwrite if the user entered a different date.
                      l_mmt_rec.transaction_date := l_trans_date;
                   END IF;            
             
                   /* Jalaj Srivastava Bug 5021522
                      call check_inv_negative only for prod/byprod. 
                     in IB, there is no ing return transaction*/
                   IF (l_trxn_type IN (gme_common_pvt.g_prod_completion, gme_common_pvt.g_byprod_completion)) THEN
                      IF gme_unrelease_batch_pvt.check_inv_negative
                         ( p_mmt_rec   => l_mmt_rec
                          ,p_mmln_tbl  => l_mmln_tbl_new
                          ,p_item_no   => l_item_no) THEN
                         RAISE inv_negative;
                      END IF;
                   END IF;
             
                   IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
                      gme_debug.put_line (g_pkg_name||'.'||l_api_name||' quantity decremented from transaction: '||l_decr_qty);
                      
                      FOR j IN 1..l_mmln_tbl.COUNT LOOP
                         gme_debug.put_line (g_pkg_name||'.'||l_api_name||' l_mmln_tbl lot number: '||l_mmln_tbl(j).lot_number);
                         gme_debug.put_line (g_pkg_name||'.'||l_api_name||' l_mmln_tbl lot quantity: '||l_mmln_tbl(j).transaction_quantity);
                      END LOOP;
                      
                      gme_debug.put_line (g_pkg_name||'.'||l_api_name||' new transaction quantity: '||l_mmt_rec.transaction_quantity);
                      
                      FOR j IN 1..l_mmln_tbl_new.COUNT LOOP
                         gme_debug.put_line (g_pkg_name||'.'||l_api_name||' l_mmln_tbl_new lot number: '||l_mmln_tbl_new(j).lot_number);
                         gme_debug.put_line (g_pkg_name||'.'||l_api_name||' l_mmln_tbl_new lot quantity: '||l_mmln_tbl_new(j).transaction_quantity);
                      END LOOP;
                      
                      gme_debug.put_line (g_pkg_name||'.'||l_api_name||' calling gme_transactions_pvt.update_material_txn with updatd l_mmt_rec and new l_mmln_tbl_new');
                   END IF;
               
                   gme_transactions_pvt.update_material_txn
                     (p_mmt_rec         => l_mmt_rec
                     ,p_mmln_tbl        => l_mmln_tbl_new
                     ,x_return_status   => l_return_status);
                   
                   IF l_return_status = gme_common_pvt.g_not_transactable THEN
                      l_trxn_success := FALSE;
                      -- do nothing... move on to the next, to try to reduce
                      IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
                         gme_debug.put_line (g_pkg_name||'.'||l_api_name||' in trxn loop; gme_transactions_pvt.update_material_txn returned '||l_return_status);
                      END IF;
                   ELSIF l_return_status <> FND_API.G_RET_STS_SUCCESS THEN
                      x_return_status := l_return_status;
                      RAISE error_trans;
                   ELSE
                      l_trxn_success := TRUE;
                      l_mmt_rec.transaction_quantity := l_decr_qty;
                      -- also need to set the secondary_transaction_quantity if applicable
                      
                      l_return_status := FND_API.G_RET_STS_SUCCESS;
                      IF l_mmt_rec.secondary_uom_code IS NOT NULL THEN
                         get_converted_qty (
                             p_org_id                    => p_material_detail_rec.organization_id
                            ,p_item_id                   => p_material_detail_rec.inventory_item_id
                            ,p_lot_number                => NULL
                            ,p_qty                       => l_mmt_rec.transaction_quantity
                            ,p_from_um                   => p_material_detail_rec.dtl_um
                            ,p_to_um                     => l_mmt_rec.secondary_uom_code
                            ,x_conv_qty                  => l_mmt_rec.secondary_transaction_quantity
                            ,x_return_status             => l_return_status);
                      END IF;
                      
                      FOR j IN 1..l_mmln_tbl_orig.COUNT LOOP
                         l_mmln_tbl_orig(j).transaction_quantity := l_mmln_tbl_orig(j).transaction_quantity - l_mmln_tbl(j).transaction_quantity;
                         IF l_mmt_rec.secondary_uom_code IS NOT NULL THEN
                            get_converted_qty (
                              p_org_id                    => p_material_detail_rec.organization_id
                             ,p_item_id                   => p_material_detail_rec.inventory_item_id
                             ,p_lot_number                => l_mmln_tbl_orig(j).lot_number
                             ,p_qty                       => l_mmln_tbl_orig(j).transaction_quantity
                             ,p_from_um                   => p_material_detail_rec.dtl_um
                             ,p_to_um                     => l_mmt_rec.secondary_uom_code
                             ,x_conv_qty                  => l_mmln_tbl_orig(j).secondary_transaction_quantity
                             ,x_return_status             => l_return_status);
                         END IF;
                      END LOOP;
                      
                      -- Bug 18846633 - Changed code to respect parameter value passed in.
                      IF l_return_status = FND_API.G_RET_STS_SUCCESS AND p_create_resv_pend_lots = 1 THEN
                         -- To handle lot serial scenarios we will have to rearchitect 19868921 at some point.                          
                         -- Bug 19868921 - Initialize rsrv table structure and also the rsrv quantity for ingredients.
                         -- These will get rereserved later in gme_post_process as part of save.	
                         IF p_material_detail_rec.line_type = gme_common_pvt.g_line_type_ing AND 
                            l_reservable_type = 1 AND l_mmln_tbl.COUNT = 1 THEN                      
	                         l_txn_hdr_tbl_cnt := gme_common_pvt.g_mat_txn_hdr_tbl.COUNT;
                            gme_common_pvt.g_mat_txn_hdr_tbl(l_txn_hdr_tbl_cnt).txn_header_id := l_mmt_rec.transaction_id;
                            gme_common_pvt.g_mat_txn_hdr_tbl(l_txn_hdr_tbl_cnt).material_dtl_id := p_material_detail_rec.material_detail_id;                
                            gme_common_pvt.g_mat_txn_hdr_tbl(l_txn_hdr_tbl_cnt).rsrv_quantity := l_decr_qty ;		
                         ELSE                                                        
                            gme_unrelease_batch_pvt.create_resv_pplot
                                  (p_material_detail_rec    => p_material_detail_rec 
                                  ,p_mmt_rec                => l_mmt_rec
                                  ,p_mmln_tbl               => l_mmln_tbl_orig
                                  ,x_return_status          => l_return_status);
                         END IF; 
                      END IF;
                      -- don't care if resv or pplot was not recreated...               
                   END IF;
             
                   -- if the transactions went through successfully, update the lot sums to reflect the returned lots
                   -- if not, go back to the last successful qties
                   IF l_trxn_success THEN
                      l_last_lot_qty_tab := l_lot_qty_tab;
                      l_last_qty := l_qty;
                     
                      -- Bug 8571983 - Add to summary bucket.
                      l_total_decr_qty := l_total_decr_qty + l_decr_qty;
                   ELSE
                      l_lot_qty_tab := l_last_lot_qty_tab;
                      l_qty := l_last_qty;
                   END IF;
                END IF;  -- IF l_decr_qty = l_mmt_rec.transaction_quantity THEN
             END IF;  -- IF l_lot_record_used = 1 THEN          
          ELSE
             IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
                gme_debug.put_line (g_pkg_name||'.'||l_api_name||' cant process because not the same transaction_type_id: trxns_id='||l_mmt_rec.transaction_id);
             END IF;
          END IF;  -- IF l_mmt_rec.transaction_type_id = l_trxn_type THEN
        
          i := i + 1;
       END LOOP;
    END IF;

    -- actual qty is reduced by the amount requested less what couldn't be reduced
    x_actual_qty := p_material_detail_rec.actual_qty - l_total_decr_qty;

    IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
       gme_debug.put_line (g_pkg_name||'.'||l_api_name||' total decremented = '||l_total_decr_qty);
       gme_debug.put_line (g_pkg_name||'.'||l_api_name||' new actual = '||x_actual_qty);
    END IF;
    
    -- raise exception if couldn't reduce by requested
    IF l_total_decr_qty < p_qty THEN
       gme_release_batch_pvt.create_batch_exception
                   (p_material_dtl_rec         => p_material_detail_rec
                   ,p_pending_move_order_ind   => NULL
                   ,p_pending_rsrv_ind         => NULL
                   ,p_transacted_qty           => l_total_decr_qty
                   ,p_exception_qty            => l_total_decr_qty - p_qty
                   ,p_force_unconsumed         => fnd_api.g_true 
                   ,x_exception_material_tbl   => x_exception_material_tbl
                   ,x_return_status            => x_return_status);
    END IF;

    IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
       gme_debug.put_line('Exiting api '||g_pkg_name||'.'||l_api_name||' with return status = '||x_return_status);
    END IF;

  EXCEPTION
    WHEN error_get_trans OR error_get_lot OR error_trans THEN
      NULL;
    WHEN inv_negative THEN  -- Bug 25883843
      x_return_status := FND_API.g_ret_sts_unexp_error;    
    WHEN OTHERS THEN
      fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
      IF g_debug <= gme_debug.g_log_procedure THEN
        gme_debug.put_line ('Unexpected error: '||g_pkg_name||'.'||l_api_name||': '||SQLERRM);
      END IF;
      x_return_status := FND_API.g_ret_sts_unexp_error;    
  END revert_material_partial;
  
  PROCEDURE validate_material_for_IB(p_material_detail_rec IN gme_material_details%ROWTYPE
                                    ,p_batch_header_rec    IN gme_batch_header%ROWTYPE
                                    ,p_adjust_cmplt        IN VARCHAR2
                                    ,x_return_status       OUT NOCOPY VARCHAR2) IS

    l_api_name        CONSTANT VARCHAR2 (30)   := 'validate_material_for_IB';

    l_step_no           NUMBER;
    l_step_status       NUMBER;

    CURSOR cur_get_step_status(v_material_detail_id NUMBER) IS
      SELECT s.batchstep_no, step_status
      FROM   gme_batch_steps s, gme_batch_step_items i
      WHERE  s.batchstep_id = i.batchstep_id
      AND    i.material_detail_id = v_material_detail_id;
      
    ERROR_INV_ACTION_FPO      EXCEPTION;
    ERROR_INV_BATCH_STATUS    EXCEPTION;
    ERROR_ADJ_CMPLT_NOT_ALLOW EXCEPTION;
    ERROR_INV_WIP_PLAN_QTY    EXCEPTION;
    ERROR_INV_RELEASE_TYPE    EXCEPTION;
    error_step_closed         EXCEPTION;
    error_step_cancelled      EXCEPTION;
    error_inv_action_phantom  EXCEPTION;
    --Bug#5111078
    error_inv_action_lab      EXCEPTION;

  BEGIN
    IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
      gme_debug.put_line('Entering api '||g_pkg_name||'.'||l_api_name);
      gme_debug.put_line(g_pkg_name||'.'||l_api_name||' material_detail_id='||p_material_detail_rec.material_detail_id);
      gme_debug.put_line(g_pkg_name||'.'||l_api_name||' batch_id='||p_batch_header_rec.batch_id);
      gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_adjust_cmplt='||p_adjust_cmplt);
    END IF;

    -- Set the return status to success initially
    x_return_status := FND_API.G_RET_STS_SUCCESS;

    -- IB not allowed for FPO
    IF p_batch_header_rec.batch_type = gme_common_pvt.g_doc_type_fpo THEN
      RAISE ERROR_INV_ACTION_FPO;
    END IF;
    
    --Bug#5111078 IB not allowed for lab batches with update inventory OFF
    IF p_batch_header_rec.update_inventory_ind <> 'Y' THEN
      RAISE error_inv_action_lab;
    END IF;

    -- IB not allowed for phantom batch
    IF NVL(p_batch_header_rec.parentline_id ,0) <> 0 THEN
      RAISE error_inv_action_phantom;
    END IF;

    -- IB allowed if batch status is WIP or Complete
    IF p_batch_header_rec.batch_status NOT IN (gme_common_pvt.g_batch_wip
                                              ,gme_common_pvt.g_batch_completed) THEN
      RAISE ERROR_INV_BATCH_STATUS;
    END IF;

    -- If batch is complete then proceed only if user wants to adjust qty
    IF p_batch_header_rec.batch_status = gme_common_pvt.g_batch_completed THEN
      IF p_adjust_cmplt = fnd_api.g_false THEN
        RAISE ERROR_ADJ_CMPLT_NOT_ALLOW;
      END IF;
    END IF; 

    --Bug#5111078 changed to wip plan qty, wip plan qty can't be zero
    IF p_material_detail_rec.wip_plan_qty = 0 THEN
      RAISE ERROR_INV_WIP_PLAN_QTY;
    END IF;

    -- Check Release Type
    IF p_material_detail_rec.release_type IN (gme_common_pvt.g_mtl_auto_release
                                             ,gme_common_pvt.g_mtl_autobystep_release) THEN
      RAISE ERROR_INV_RELEASE_TYPE;
    END IF;

    -- If the step associated with the material line is closed or cancelled
    OPEN Cur_get_step_status (p_material_detail_rec.material_detail_id);
    FETCH Cur_get_step_status INTO l_step_no, l_step_status;
    CLOSE Cur_get_step_status;
    IF NVL(l_step_status, 0) = gme_common_pvt.g_step_closed THEN
      RAISE error_step_closed;
    END IF;
    IF NVL(l_step_status, 0) = gme_common_pvt.g_step_cancelled THEN
      RAISE error_step_cancelled;
    END IF;
    
    IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
      gme_debug.put_line('Exiting api '||g_pkg_name||'.'||l_api_name);
    END IF;

  EXCEPTION
  WHEN ERROR_INV_ACTION_FPO THEN
    x_return_status := FND_API.G_RET_STS_ERROR;  
    gme_common_pvt.log_message('GME_INV_ACTION_FPO');
  --Bug#5111078
  WHEN error_inv_action_lab THEN
    x_return_status := FND_API.G_RET_STS_ERROR;  
    gme_common_pvt.log_message('GME_IB_FOR_UPDINV_NT_ALWD');
  WHEN ERROR_INV_BATCH_STATUS THEN
    x_return_status := FND_API.G_RET_STS_ERROR;  
    gme_common_pvt.log_message('GME_API_INV_BATCH_STATUS_PC');
  WHEN ERROR_INV_RELEASE_TYPE THEN
    x_return_status := FND_API.G_RET_STS_ERROR;  
    gme_common_pvt.log_message('GME_API_INV_RELEASE_TYPE');
  WHEN ERROR_INV_WIP_PLAN_QTY THEN
    x_return_status := FND_API.G_RET_STS_ERROR;  
    gme_common_pvt.log_message('GME_API_INV_WIP_PLAN_QTY_PC');
  WHEN ERROR_ADJ_CMPLT_NOT_ALLOW THEN
    x_return_status := FND_API.G_RET_STS_ERROR;  
    gme_common_pvt.log_message('GME_API_ASK_ADJUST_CERTIFY');
  WHEN error_step_closed THEN
    x_return_status := FND_API.G_RET_STS_ERROR;  
    GME_COMMON_PVT.log_message ('GME_STEP_CLOSED_ERR', 'STEP_NO', l_step_no);
  WHEN error_step_cancelled THEN
    x_return_status := FND_API.G_RET_STS_ERROR;  
    GME_COMMON_PVT.log_message ('GME_STEP_CANCELLED_ERR', 'STEP_NO', l_step_no);
  WHEN error_inv_action_phantom THEN
    x_return_status := FND_API.G_RET_STS_ERROR;
    gme_common_pvt.log_message('PM_INVALID_PHANTOM_ACTION');
  WHEN OTHERS THEN
    fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
    IF g_debug <= gme_debug.g_log_procedure THEN
      gme_debug.put_line ('Unexpected error: '||g_pkg_name||'.'||l_api_name||': '||SQLERRM);
    END IF;
    x_return_status := FND_API.g_ret_sts_unexp_error;
  END validate_material_for_IB;

  PROCEDURE validate_qty_for_IB (p_qty_type   IN NUMBER
                                ,p_qty        IN NUMBER
                                ,p_actual_qty IN NUMBER
                                ,x_return_status OUT NOCOPY VARCHAR2) IS
    l_api_name        CONSTANT VARCHAR2 (30)   := 'validate_qty_for_IB';
  
    ERROR_INV_INCR_TYPE           EXCEPTION;
    ERROR_QTY_CANT_BE_ZERO        EXCEPTION;
    ERROR_QTY_CREATE_NEG_ACTUAL   EXCEPTION;
  BEGIN
    IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
      gme_debug.put_line('Entering api '||g_pkg_name||'.'||l_api_name);
      gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_qty_type='||p_qty_type);
      gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_qty='||p_qty);
      gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_actual_qty='||p_actual_qty);
    END IF;

    -- Set the return status to success initially
    x_return_status := FND_API.G_RET_STS_SUCCESS;

    -- p_qty_type 0 increment qty
    -- p_qty_type 1 new act qty
    -- p_qty_type 2 % plan

    IF p_qty_type NOT IN (0,1,2) THEN
      RAISE ERROR_INV_INCR_TYPE;
    END IF;

    -- INCREMENTAL
    IF p_qty_type = 0 THEN
      IF p_qty = 0 THEN
        RAISE ERROR_QTY_CANT_BE_ZERO;
      ELSIF p_qty < 0 THEN
        IF ((p_qty * -1 ) > p_actual_qty) THEN
          RAISE ERROR_QTY_CREATE_NEG_ACTUAL ;
        END IF;	
      END IF;
    -- NEW ACTUAL
    ELSIF p_qty_type = 1 THEN
      IF p_qty < 0 THEN
        RAISE ERROR_QTY_CREATE_NEG_ACTUAL;
      END IF;
    -- % PLAN
    ELSIF p_qty_type = 2 THEN
      IF p_qty < 0 THEN
        RAISE ERROR_QTY_CREATE_NEG_ACTUAL;
      END IF;
    END IF;

    IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
      gme_debug.put_line('Exiting api '||g_pkg_name||'.'||l_api_name);
    END IF;

  EXCEPTION
  WHEN ERROR_INV_INCR_TYPE THEN
    x_return_status := FND_API.G_RET_STS_ERROR;  
    gme_common_pvt.log_message( 'GME_API_INVALID_INCR_TYPE');
  WHEN ERROR_QTY_CREATE_NEG_ACTUAL THEN
    x_return_status := FND_API.G_RET_STS_ERROR;  
    gme_common_pvt.log_message( 'GME_API_ACTUAL_CANT_GO_NEG');
  WHEN ERROR_QTY_CANT_BE_ZERO THEN
    x_return_status := FND_API.G_RET_STS_ERROR;  
    gme_common_pvt.log_message( 'GME_API_QTY_CANT_BE_ZERO');
  WHEN OTHERS THEN
    fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
    IF g_debug <= gme_debug.g_log_procedure THEN
      gme_debug.put_line ('Unexpected error: '||g_pkg_name||'.'||l_api_name||': '||SQLERRM);
    END IF;
    x_return_status := FND_API.g_ret_sts_unexp_error;
  END validate_qty_for_IB;
  
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


END gme_incremental_backflush_pvt;
/
COMMIT;
EXIT;
--show errors




