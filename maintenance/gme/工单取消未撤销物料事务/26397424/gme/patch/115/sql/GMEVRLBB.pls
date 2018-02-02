/* +======================================================================+ */
/* |    Copyright (c) 2005, 2016 Oracle and/or its affiliates.           | */
/* |                         All rights reserved.                         | */
/* |                           Version 12.0.0                             | */
/* +======================================================================+ */
REM dbdrv: sql ~PROD ~PATH ~FILE none none none package &phase=plb \
REM dbdrv: checkfile(120.29.12010000.29=120.39.12020000.20)(120.26.12000000.21=120.29.12010000.15)(120.26.12000000.12=120.29.12010000.6)(115.65=120.4):~PROD:~PATH:~FILE 
SET VERIFY OFF 
WHENEVER SQLERROR EXIT FAILURE ROLLBACK;
WHENEVER OSERROR EXIT FAILURE ROLLBACK;
REM ***********************************************************************
REM *                                                                     *
REM * FILE:    GMEVRLBB.pls                                               *
REM * PURPOSE: Package Body for the routines to release a production or   *
REM *          lab batch
REM * AUTHOR:  Antonia Newbury, OPM Development                           *
REM * DATE:    07-Mar-2005                                                *
REM * HISTORY:                                                            *
REM * ========                                                            *
REM * Pawan Kumar 12-21-2005 Bug 4899399 - Call to inform about consumed  *
REM *          reservation after relieveing the same                      *
REM * Pawan Kumar 01-10-2006 bug 4742244 				  *
REM *         check for item which dispensable but non-reservable	  *
REM * A Newbury   01-19-2006                                              *
REM *         change to consume_material to exit immediately if           *
REM *         gme_release_batch_pvt.g_bypass_txn_creation is set to 1     *
REM *         return to caller is successful with output actual qty set to*
REM *         what is passed in from input material detail record actual  *
REM * sunitha ch. Bug 5336007 added call to check_validity_rule_dates     *
REM * Swapna K Bug#6507649 Changed c_return_status to l_return_Status in  *
REM * call, GMF_VIB.Create_Batch_requirements.                            *
REM *                                                                     *
REM * S. Kommineni   11-Feb-2008 Bug 6778968                              
REM *    Added parameter p_called_by to procedure build_and_create_transaction 
REM *    to support it being used by convert detail reservation api.
REM *    'REL' is the default as it is mainly used by release batch. 
REM *    'CVT' for conversion api. This parameter value will dictate the
REM *    qty value we pass into the INV routines to get relieved/converted.
REM * Archana Mundhe  14-May-2008   Bug 6437252 LPN Support.
REM *   Changed constr_mmti_from_reservation to assign lpn_id from reservation 
REM *   to mtl_transactions_interface.lpn_id
REM *                                                                     
REM * G. Muratore     12-Feb-2009   Bug 7709971 
REM *   Changed code to default full qty being consumed in cases where the 
REM *   item is not lot controlled and systems allows negative inventory.
REM *   Procedure: build_and_create_transaction and consume_material
REM * 
REM * A. Mishra       28-Apr-2009   Bug 8468926 
REM    To overwrite the wip plan qty if there is not already a value there
REM    To update the actual qty consumed to WIP planned qty(if edited). 
REM *   Procedure: build_and_create_transaction and consume_material
REM *                                                                     
REM * G. Muratore     04-May-2009   Bug 8277090 
REM *   Changed code to handle partially detailed and High level reservations 
REM *   properly. Existing Loop was activated and corrected to make it work.
REM *   Procedure: consume_material
REM *
REM * Srinivasulu Puri 29-Jul-2009   Bug 8741777 
REM *   changed build_and_create_transaction to correctly assign primary Qty 
REM *   Local variable currently it is l_dtl_qty instead of l_prim_qty
REM *   Procedure: build_and_create_transaction 
REM *                                                                     
REM * G. Muratore     04-May-2009   Bug 12971020 
REM *   Code will now check origination date of lot to make sure it comes 
REM *   after the transaction date. This will stop us from using lots that
REM *   did not exist prior to transaction date.
REM *   Also, included some cleanup and addition of debug messages (12813284)
REM *   Procedure: consume_material
REM *                                                                     
REM * G. Muratore     04-OCT-2011   Bug 12909216 
REM *   Do not call GMF if we have a phantom product for zero qty. 
REM *   Procedure: release_batch
REM *                                                                     
REM * G. Muratore     02-Jul-2012   Bug 13949475 
REM *   Make sure there is some inventory available when processing  
REM *   partial reservations.
REM *   Procedure: consume_material
REM *
REM * Archana Mundhe  07-May-2013   Bug 16423133 
REM *   Modified procedure consume_material. 
REM *   Moved code up so that l_consume_qty is initialized correctly.

REM *  G. Muratore    13-Jan-2014   Bug 18044719
REM *   Add p_lot_control_code parameter when calling reservation_fully_specified. With the
REM *   flexible lot INV enhancement, it is now possible to not specify a lot on a reservation
REM *   for a lot controlled item. This is now treated as a partial level reservation.
REM *   Procedure: consume_material 

REM *  G. Muratore    11-Jun-2014   Bug 18765933
REM *   Update wip_plan_qty and actual_qty using proper uom for parent ingredient and phantom product.
REM *   Procedure: release_batch and process_ingredient.

REM *  Shaliu Chen     18-JUL-2014  ER 19161894                                                 
REM *   Modify create_batch to invoke requisition creation program if batch include OSP step 

REM *  Shaliu Chen     07-APR-2015  ER 20809749
REM *   Modify create batch to support multiple osp steps.   

REM *  QZENG           22-MAY-2015  Bug 21101876
REM *   Modified process_ingredient to not to release the phantom batch when it 
REM *   has been hold.

REM *  Shaliu Chen     16-JUN-2015  BUG 21208206                                                 
REM *    Add validation to check whether acutal_start_date belongs to a period where 
REM *    the batch was placed on hold. 
REM *    PROCEDURE: validate_batch_for_release.

REM *  Shaliu Chen     29-JUN-2015  BUG 21326978                                                 
REM *    correct actual_cmplt_date to actual_start_date
REM *    PROCEDURE: validate_batch_for_release.

REM *  G. Muratore     01-Feb-2016  Bug 22567974     
REM *    This is a fine tuning and add on of all the previous rounding issues: 22217179/21078209
REM *    The code is made more precise to determine whether the quantity needed (consume qty)
REM *    is essentially equal to the reservation quantity remaining then use consume qty for trxn.
REM *    The remaining reservation quantities will be used for relieving the reservation in this case.
REM *    This avoidsUnnecessary conversions which leads to precision loss.
REM *    Procedure: build_and_create_transaction 

REM * G. Muratore      04-APR-2016  Bug 22317010 / 22764488
REM *   p_tolerance acceptable values are between 1 and .99. This was introduced
REM *   for handling problems caused by rounding over the course of multiple IB's. 
REM *   PROCEDURE: build_and_create_transaction and consume_material 

REM * G. Muratore      29-JUL-2016  Bug 24350177
REM *   When consuming a reservation fully during IB, double check primary quantity from reservation
REM *   against qty tree ATT as there could be problems caused by rounding over the course of multiple IB's.
REM *   When the reservation is more than ATT but essentially equal, use ATT value for the transaction logic. 
REM *   PROCEDURE: build_and_create_transaction 
REM ***********************************************************************

/**************************************************************************
* This file contains the procedure for actions on material lines in Oracle*
* Process Manufacturing (OPM). Each procedure has a common set of         *
* parameters to which API-specific parameters are appended.               *
**************************************************************************/

CREATE OR REPLACE PACKAGE BODY gme_release_batch_pvt AS
/* $Header: GMEVRLBB.pls 120.39.12020000.20 2016/08/26 14:16:37 gmurator ship $ */

G_DEBUG VARCHAR2(5) := FND_PROFILE.VALUE('AFLOG_LEVEL');

g_pkg_name VARCHAR2(30)  := 'GME_RELEASE_BATCH_PVT';


  PROCEDURE release_batch
              (p_batch_header_rec           IN  gme_batch_header%ROWTYPE
              ,p_phantom_product_id         IN  NUMBER DEFAULT NULL
              ,p_yield                      IN  BOOLEAN DEFAULT NULL
              ,x_exception_material_tbl     IN  OUT NOCOPY gme_common_pvt.exceptions_tab
              ,x_batch_header_rec           OUT NOCOPY gme_batch_header%ROWTYPE              
              ,x_return_status              OUT NOCOPY VARCHAR2) IS
    

    CURSOR Cur_batch_ingredients(v_batch_id NUMBER) IS
    SELECT *
      FROM gme_material_details
     WHERE batch_id = v_batch_id
       AND line_type = gme_common_pvt.g_line_type_ing;
     
    CURSOR Cur_associated_step(v_matl_dtl_id NUMBER) IS
    SELECT s.*
      FROM gme_batch_steps s, gme_batch_step_items item
     WHERE s.batchstep_id = item.batchstep_id
       AND item.material_detail_id = v_matl_dtl_id;
    /*                                    
    BEGIN ER 19161894                    
    Shaliu Chen 18-JUL-2014               
    Added for OPM Step Level OSP Project  
    */            
    CURSOR cur_get_osp_parameter(v_organization_id NUMBER)
    IS
       SELECT po_creation_time
         FROM gme_parameters
        WHERE organization_id = v_organization_id;   
    /*END ER 19161894*/  
    
    /*                                    
    BEGIN ER 20809749                    
    Shaliu Chen 07-APR-2015               
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
                   
       
    l_api_name               CONSTANT   VARCHAR2 (30)                := 'RELEASE_BATCH';
       
    l_step_rec               gme_batch_steps%ROWTYPE;
    l_matl_dtl_tab           gme_common_pvt.material_details_tab;
    l_matl_dtl_rec           gme_material_details%ROWTYPE;
    l_release_type           NUMBER;
    l_phantom_batch_rec      gme_batch_header%ROWTYPE;
    l_item_rec               mtl_system_items_b%ROWTYPE;
    l_consume                BOOLEAN;
    l_return_status          VARCHAR2(1);
    
    l_actual_qty             NUMBER;
    /*                                    
    BEGIN ER 19161894                    
    Shaliu Chen 18-JUL-2014               
    Added for OPM Step Level OSP Project  
    */       
    l_osp_batch_id                NUMBER;
    l_osp_batchstep_id            NUMBER;
    l_osp_batchstep_resource_id   NUMBER;
    l_osp_organization_id         NUMBER;
    l_osp_resource_flag           NUMBER;
    l_po_creation_time            NUMBER;
    l_errMsg                      VARCHAR2(2000);    
    /*END ER 19161894*/      
    error_update_batch       EXCEPTION;
    error_process_ingredient EXCEPTION;
    error_consume_material   EXCEPTION;
    error_update_row         EXCEPTION;
    error_yield_material     EXCEPTION;
    error_fetch_material     EXCEPTION;
    error_get_item           EXCEPTION;
    error_unexp_phantom	     EXCEPTION;
    /*                                    
    BEGIN ER 19161894                    
    Shaliu Chen 18-JUL-2014               
    Added for OPM Step Level OSP Project  
    */     
    create_requisition_failure     EXCEPTION;     
    /*END ER 19161894*/     
    -- 18765933
    um_convert_error         EXCEPTION;
    l_item_no                mtl_system_items_kfv.concatenated_segments%TYPE;
    
    CURSOR item_no_cursor(v_inventory_item_id NUMBER,
                          v_org_id            NUMBER) IS
    SELECT concatenated_segments
      FROM mtl_system_items_kfv
     WHERE inventory_item_id = v_inventory_item_id
       AND organization_id = v_org_id;

    -- Bug 5903208
    gmf_cost_failure         EXCEPTION;
    l_message_count	     NUMBER;
    l_message_list           VARCHAR2(2000);
    l_tmp		     VARCHAR2(2000);

  BEGIN
    IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
       gme_debug.put_line('Entering api '||g_pkg_name||'.'||l_api_name);
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' batch_id = '||p_batch_header_rec.batch_id);
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_phantom_product_id = '||p_phantom_product_id);
       IF p_yield IS NOT NULL AND p_yield THEN
          gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_yield=TRUE');
       ELSIF p_yield IS NOT NULL THEN
          gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_yield=FALSE');
       ELSE
          gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_yield is NULL');
       END IF;
    END IF;

    /* Set the return status to success initially */
    x_return_status       := FND_API.G_RET_STS_SUCCESS;

    -- set output structure
    x_batch_header_rec := p_batch_header_rec;
    -- call for validate the batch for unexploded phantoms
    check_unexploded_phantom(p_batch_id              => x_batch_header_rec.batch_id
                            ,p_auto_by_step          => 2
                            ,p_batchstep_id          => null
                            ,x_return_status         => l_return_status);
  
    IF l_return_status <> fnd_api.g_ret_sts_success THEN
       RAISE error_unexp_phantom;
    END IF;
    -- set batch status
    x_batch_header_rec.batch_status := gme_common_pvt.g_batch_wip;

    -- set actual start date...
    -- this is expected to be populated and validated (from either user input or timestamp)
    x_batch_header_rec.actual_start_date := p_batch_header_rec.actual_start_date;
    
    -- Update the batch header
    IF NOT gme_batch_header_dbl.update_row (p_batch_header => x_batch_header_rec) THEN
      RAISE error_update_batch;
    END IF;

    -- Update WHO columns for output structure
    x_batch_header_rec.last_updated_by := gme_common_pvt.g_user_ident;
    x_batch_header_rec.last_update_date := gme_common_pvt.g_timestamp;
    x_batch_header_rec.last_update_login := gme_common_pvt.g_login_id;

    -- retrieve all ingredients, don't blindly exclude auto by step, because these can really be auto
    -- if not associated to a step...    
    OPEN Cur_batch_ingredients(p_batch_header_rec.batch_id);
    FETCH Cur_batch_ingredients BULK COLLECT INTO l_matl_dtl_tab;
    CLOSE Cur_batch_ingredients;
    
    FOR i IN 1..l_matl_dtl_tab.COUNT LOOP
       l_matl_dtl_rec := l_matl_dtl_tab(i);
       
       l_release_type := l_matl_dtl_rec.release_type;
       IF l_release_type = gme_common_pvt.g_mtl_autobystep_release THEN
          OPEN Cur_associated_step(l_matl_dtl_rec.material_detail_id);
          FETCH Cur_associated_step INTO l_step_rec;
          IF Cur_associated_step%NOTFOUND THEN
             l_release_type := gme_common_pvt.g_mtl_auto_release;
          END IF;
          CLOSE Cur_associated_step;
       END IF;

       IF l_release_type <> gme_common_pvt.g_mtl_autobystep_release THEN
          IF l_release_type = gme_common_pvt.g_mtl_auto_release THEN
             l_consume := TRUE;
          ELSE
             l_consume := FALSE;
          END IF;
          
          process_ingredient
               (p_material_detail_rec        => l_matl_dtl_rec
               ,p_consume                    => l_consume
               ,p_trans_date                 => x_batch_header_rec.actual_start_date
               ,p_update_inv_ind             => x_batch_header_rec.update_inventory_ind
               ,x_exception_material_tbl     => x_exception_material_tbl
               ,x_return_status              => l_return_status);
       
          IF l_return_status NOT IN (FND_API.G_RET_STS_SUCCESS, gme_common_pvt.g_exceptions_err) THEN
             x_return_status := l_return_status;
             RAISE error_process_ingredient;
          END IF;
           
          IF l_return_status = gme_common_pvt.g_exceptions_err THEN
             x_return_status := gme_common_pvt.g_exceptions_err;
          END IF;
       END IF;  -- IF l_release_type <> gme_common_pvt.g_mtl_autobystep_release THEN
    END LOOP;

    -- Yield the phantom product for this batch... that will also take care of the phantom ingredient    
    IF p_phantom_product_id IS NOT NULL THEN
    
       -- Bug 18765933 -- Moved fetch of phantom prod line outside of yield IF.    
       l_matl_dtl_rec.material_detail_id := p_phantom_product_id;
       IF NOT gme_material_details_dbl.fetch_row(l_matl_dtl_rec, l_matl_dtl_rec) THEN
          RAISE error_fetch_material;
       END IF;

       IF NVL(p_yield, TRUE) THEN
          IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
             gme_debug.put_line(g_pkg_name||'.'||l_api_name||' yielding phantom product');
          END IF;
          
          -- l_matl_dtl_rec is the phantom product line      
          gme_material_detail_pvt.get_item_rec
                          (p_org_id                => l_matl_dtl_rec.organization_id
                          ,p_item_id               => l_matl_dtl_rec.inventory_item_id
                          ,x_item_rec              => l_item_rec
                          ,x_return_status         => l_return_status);
                          
          IF l_return_status <> FND_API.G_RET_STS_SUCCESS THEN
             x_return_status := l_return_status;
             RAISE error_get_item;
          END IF;
       
          IF p_batch_header_rec.update_inventory_ind = 'Y' AND
             l_item_rec.mtl_transactions_enabled_flag = 'Y' THEN
             IF g_debug <= gme_debug.g_log_procedure THEN
                gme_debug.put_line(g_pkg_name||'.'||l_api_name||' processing phantom product material_detail_id='||l_matl_dtl_rec.material_detail_id);
             END IF;
             
             gme_complete_batch_pvt.yield_material
               (p_material_dtl_rec             => l_matl_dtl_rec
               ,p_yield_qty                    => l_matl_dtl_rec.plan_qty
               ,p_trans_date                   => x_batch_header_rec.actual_start_date
               ,p_item_rec                     => l_item_rec
               ,p_force_unconsumed             => fnd_api.g_true
               ,x_exception_material_tbl       => x_exception_material_tbl
               ,x_actual_qty                   => l_actual_qty
               ,x_return_status                => l_return_status);
             
             IF l_return_status NOT IN (FND_API.G_RET_STS_SUCCESS, gme_common_pvt.g_exceptions_err) THEN
                x_return_status := l_return_status;
                RAISE error_yield_material;
             END IF;
             
             IF l_return_status = gme_common_pvt.g_exceptions_err THEN
                x_return_status := gme_common_pvt.g_exceptions_err;
             END IF;
             
             l_matl_dtl_rec.actual_qty := l_actual_qty;
          ELSE
             l_matl_dtl_rec.actual_qty := l_matl_dtl_rec.plan_qty;
          END IF;
       ELSE
          IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
             gme_debug.put_line(g_pkg_name||'.'||l_api_name||' NOT yielding phantom product; set actual to 0');
          END IF;
          l_matl_dtl_rec.actual_qty := 0;
       END IF;

       l_matl_dtl_rec.wip_plan_qty := l_matl_dtl_rec.plan_qty;      

       -- Update the phantom product
       UPDATE gme_material_details
          SET actual_qty = l_matl_dtl_rec.actual_qty,
              wip_plan_qty = l_matl_dtl_rec.plan_qty,
              last_updated_by = gme_common_pvt.g_user_ident, 
              last_update_date = gme_common_pvt.g_timestamp, 
              last_update_login = gme_common_pvt.g_login_id
        WHERE  material_detail_id = l_matl_dtl_rec.material_detail_id;      

       -- Bug 18765933 -- UOM of parent ingredient may not be the same as phantom prod line.
       -- Logic rearranged as we may need to recalculate actual qty.
       IF l_matl_dtl_rec.dtl_um <> p_batch_header_rec.attribute30 THEN
          -- This means that we need to derive the actual_qty using correct uom. 
          l_matl_dtl_rec.actual_qty := inv_convert.inv_um_convert
                (item_id              => l_matl_dtl_rec.inventory_item_id
                ,precision            => gme_common_pvt.g_precision
                ,from_quantity        => l_matl_dtl_rec.actual_qty
                ,from_unit            => l_matl_dtl_rec.dtl_um
                ,to_unit              => p_batch_header_rec.attribute30
                ,from_name            => NULL
                ,to_name              => NULL);
       
          -- Let's see if conversion went wrong.
          IF (l_matl_dtl_rec.actual_qty = -99999) THEN
             IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
                gme_debug.put_line(g_pkg_name||'.'||l_api_name||' '||
                    l_matl_dtl_rec.dtl_um||' TO '||p_batch_header_rec.attribute30||
                    ' - inv_convert.inv_um_convert returned error for item '||
                    l_matl_dtl_rec.inventory_item_id);
             END IF;
             RAISE um_convert_error;
          END IF;      
       END IF;
       
       -- Update the phantom ingredient actual_qty and WIP plan qty... 
       -- the transaction would have been taken care of
       UPDATE gme_material_details
          SET actual_qty = l_matl_dtl_rec.actual_qty,
              wip_plan_qty = plan_qty,
              last_updated_by = gme_common_pvt.g_user_ident, 
              last_update_date = gme_common_pvt.g_timestamp, 
              last_update_login = gme_common_pvt.g_login_id
        WHERE material_detail_id = l_matl_dtl_rec.phantom_line_id;
       
      
    END IF;
    
    UPDATE gme_material_details
       SET wip_plan_qty = plan_qty
     WHERE batch_id = p_batch_header_rec.batch_id
       AND wip_plan_qty is NULL;
    /*                                    
    BEGIN ER 19161894                    
    Shaliu Chen 18-JUL-2014               
    Create a requisition if the batch include Outside resource.
    */            
    IF gme_osp.Check_Release_Version THEN  
      IF g_debug <= gme_debug.g_log_statement THEN
        gme_debug.put_line(   g_pkg_name
                            || '.'
                            || l_api_name
                            || ' Calling requisition creation API');
      END IF;

      BEGIN
        /*
         Check GME parameters
        */
        OPEN cur_get_osp_parameter(x_batch_header_rec.organization_id);
        FETCH cur_get_osp_parameter INTO l_po_creation_time;
        CLOSE cur_get_osp_parameter;

        IF NVL(l_po_creation_time,0) = gme_osp.g_batch_rlease THEN
           /* 
            Check whether the batch include Outside resource.
           */

           SELECT count(1)
             INTO l_osp_resource_flag
             FROM gme_batch_step_resources gbsr
            WHERE batch_id = x_batch_header_rec.batch_id
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
            Shaliu Chen 07-APR-2015               
            Change to loop each osp step and invoke requisition creation for each osp step. 
           */  
           IF (l_osp_resource_flag > 0) THEN
             FOR cur_gaos IN cur_get_all_osp_steps(x_batch_header_rec.batch_id) LOOP
               l_osp_batch_id              := cur_gaos.batch_id;
               l_osp_organization_id       := cur_gaos.organization_id;
               l_osp_batchstep_id          := cur_gaos.batchstep_id;
               l_osp_batchstep_resource_id := cur_gaos.batchstep_resource_id;
              /*
               Invoke requisition creation api.
              */
               gme_osp.create_requisition( P_Batch_Id              => l_osp_batch_id,
                                           P_Organization_Id       => l_osp_organization_id,
                                           P_Batchstep_Id          => l_osp_batchstep_id,
                                           P_Batchstep_Resource_id => l_osp_batchstep_resource_id,
                                           x_return_status         => l_return_status,
                                           x_message_list          => l_errMsg,
                                           x_message_count         => l_message_count);

               IF (l_return_status <> fnd_api.g_ret_sts_success) THEN
                 IF g_debug <= gme_debug.g_log_statement THEN
                   gme_debug.put_line(   g_pkg_name
                                       || '.'
                                       || l_api_name
                                       || ' Calling requisition creation API failed,'||l_errMsg);
                 END IF;
                 RAISE create_requisition_failure;
               END IF;
             END LOOP;
           ELSIF (l_osp_resource_flag >1) THEN
             gme_common_pvt.log_message (p_product_code => 'GME'
                                        ,p_message_code => 'GME_MULTI_OSP_RSRCS');

             RAISE create_requisition_failure;
           END IF;
         END IF;
      EXCEPTION
       /*
        BUG 20427080
        Raise the exception to outermost layer to avoid the error message is overrided.
       */
       WHEN create_requisition_failure THEN
         RAISE;        
       WHEN OTHERS THEN
         l_errMsg := 'unexpected error: ' || SQLERRM || 'SQLCODE = ' || SQLCODE;
         gme_debug.put_line('l_errMsg:'||l_errMsg);
         fnd_message.set_name('FND', 'FND_GENERIC_MESSAGE');
         fnd_message.set_token('MESSAGE', l_errMsg);
         fnd_msg_pub.add;
         RAISE create_requisition_failure;
      END;
    END IF;         
    /*END ER 19161894*/
    IF NOT gme_common_pvt.create_history
                        (p_batch_header_rec      => x_batch_header_rec
                        ,p_original_status       => gme_common_pvt.g_batch_pending
                        ,p_event_id              => NVL(gme_common_pvt.g_transaction_header_id,-9999)) THEN
       IF g_debug <= gme_debug.g_log_procedure THEN
          gme_debug.put_line(g_pkg_name||'.'||l_api_name||' create history returned error');
       END IF;
    END IF;
    

    -- 
    -- Bug 5903208 - Make call to GMF
    --
    -- Bug 12909216 - Do not call GMF if we have a phantom product for zero qty.
    IF p_phantom_product_id IS NULL OR l_matl_dtl_rec.wip_plan_qty <> 0 THEN
       GMF_VIB.Create_Batch_Requirements
       ( p_api_version   =>    1.0,
         p_init_msg_list =>    FND_API.G_FALSE,
         p_batch_id      =>    x_batch_header_rec.batch_id,
         x_return_status =>    l_return_status, --Bug#6507649
         x_msg_count     =>    l_message_count,
         x_msg_data      =>    l_message_list);
       
       IF l_return_status <> FND_API.G_RET_STS_SUCCESS --Bug#6507649 Rework
       THEN
          RAISE gmf_cost_failure;
       END IF;
       -- End Bug 5903208
    END IF;
      
    IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
      gme_debug.put_line('Exiting api '||g_pkg_name||'.'||l_api_name||' for batch_id= '||x_batch_header_rec.batch_id||' and x_return_status= '||l_return_status);
    END IF;

  EXCEPTION
  WHEN um_convert_error THEN
    -- 18765933
    OPEN item_no_cursor(l_matl_dtl_rec.inventory_item_id, l_matl_dtl_rec.organization_id);
    FETCH item_no_cursor INTO l_item_no;
    CLOSE item_no_cursor;
    
    fnd_message.set_name  ('GMI', 'IC_API_UOM_CONVERSION_ERROR');
    fnd_message.set_token ('ITEM_NO', l_item_no);
    fnd_message.set_token ('FROM_UOM',l_matl_dtl_rec.dtl_um);
    fnd_message.set_token ('TO_UOM',p_batch_header_rec.attribute30);
    fnd_msg_pub.ADD;
    x_return_status := FND_API.g_ret_sts_error;
  
  WHEN   gmf_cost_failure THEN
    -- Bug 5903208
    x_return_status := FND_API.G_RET_STS_ERROR;
    
  WHEN  error_update_batch OR error_update_row OR error_fetch_material THEN
    /* Bug 5554841 No need to set messsage it is set by called APIs */
    --gme_common_pvt.log_message ('GME_UNEXPECTED_ERROR', 'ERROR', SQLERRM);
    x_return_status := FND_API.g_ret_sts_unexp_error;
  WHEN  error_process_ingredient OR error_consume_material OR error_yield_material OR error_get_item THEN
    NULL;
  WHEN error_unexp_phantom THEN
      gme_common_pvt.log_message ('PM_UNEXPLODED_PHANTOMS');
      x_return_status := FND_API.G_RET_STS_ERROR;
  /*                                    
  BEGIN ER 19161894                    
  Shaliu Chen 18-JUL-2014               
  Added for OPM Step Level OSP Project  
  */        
  WHEN create_requisition_failure THEN
    x_return_status := fnd_api.g_ret_sts_error;       
  WHEN OTHERS THEN
    fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
    IF g_debug <= gme_debug.g_log_procedure THEN
      gme_debug.put_line('Unexpected error: '||g_pkg_name||'.'||l_api_name||': '||SQLERRM);
    END IF;
    x_return_status := FND_API.g_ret_sts_unexp_error;    
  END release_batch;
  
  PROCEDURE process_ingredient
              (p_material_detail_rec        IN         gme_material_details%ROWTYPE
              ,p_consume                    IN         BOOLEAN
              ,p_trans_date                 IN         DATE
              ,p_update_inv_ind             IN         VARCHAR2
              ,x_exception_material_tbl     IN  OUT NOCOPY  gme_common_pvt.exceptions_tab
              ,x_return_status              OUT NOCOPY VARCHAR2) IS
    

    l_api_name               CONSTANT   VARCHAR2 (30)                := 'process_ingredient';
    
    l_matl_dtl_rec                gme_material_details%ROWTYPE;
    l_in_phantom_batch_rec        gme_batch_header%ROWTYPE;
    l_phantom_batch_rec           gme_batch_header%ROWTYPE;
    l_return_status               VARCHAR2(1);
    l_item_rec                    mtl_system_items_b%ROWTYPE;
    l_actual_qty                  NUMBER;
    l_update_matl                 BOOLEAN;
    -- Bug 21101876, Added by QZENG, for onhold
    l_batch_no                    gme_batch_header.batch_no%TYPE;

    error_update_row              EXCEPTION;
    error_fetch_batch             EXCEPTION;
    error_release_batch           EXCEPTION;
    error_consume_material        EXCEPTION;
    error_get_item                EXCEPTION;
    error_dispense_non_reserve    EXCEPTION;
    -- Bug 21101876, Added by QZENG, for onhold
    error_phantom_batch_hold      EXCEPTION;
  BEGIN

    IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
      gme_debug.put_line('Entering api '||g_pkg_name||'.'||l_api_name);
      gme_debug.put_line(g_pkg_name||'.'||l_api_name||' material_detail_id = '||p_material_detail_rec.material_detail_id);
    END IF;

    /* Set the return status to success initially */
    x_return_status       := FND_API.G_RET_STS_SUCCESS;

    -- Process the ingredients...
    -- 1) release phantom batch ingredient
    -- 2) consume non phantom ingredient
    -- 3) set wip plan qty

    l_matl_dtl_rec := p_material_detail_rec;

    -- if it's a phantom ingredient, then release the batch and pass the phantom line id
    -- which will cause the phantom product to be yielded; don't consume this ingredient
    -- because the ingredient will be taken care of with yield of the product... that's why
    -- consume is in the else of following if statement...

    -- release phantom batch
    IF l_matl_dtl_rec.phantom_id IS NOT NULL THEN  -- phantom -> release the phantom batch
      l_phantom_batch_rec.batch_id := l_matl_dtl_rec.phantom_id;
      IF NOT gme_batch_header_dbl.fetch_row(l_phantom_batch_rec, l_phantom_batch_rec) THEN
        RAISE error_fetch_batch;
      END IF;

      IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' found phantom ingredient material_detail_id = '||l_matl_dtl_rec.material_detail_id||' and phantom batch_id= '||l_phantom_batch_rec.batch_id);
      END IF;
      -- Bug 21101876, Added by QZENG, check batch hold type for the phantom
      -- batch
      IF gme_common_pvt.GET_BATCH_HOLD_STATUS(l_matl_dtl_rec.phantom_id) <> 'R'
      THEN
        l_batch_no := l_phantom_batch_rec.batch_no;
        raise error_phantom_batch_hold;
      END IF;
      -- End Bug 21101876
      -- pass in the phantom line id so that release batch will know to yield that product
      l_in_phantom_batch_rec := l_phantom_batch_rec;
      l_in_phantom_batch_rec.actual_start_date := p_trans_date;

      -- Bug 18765933 pass in this value for potential conversions
      l_in_phantom_batch_rec.attribute30 := l_matl_dtl_rec.dtl_um; 
      
      release_batch
          (p_batch_header_rec                => l_in_phantom_batch_rec
          ,p_phantom_product_id              => l_matl_dtl_rec.phantom_line_id
          ,p_yield                           => p_consume
          ,x_batch_header_rec                => l_phantom_batch_rec
          ,x_return_status                   => l_return_status
          ,x_exception_material_tbl          => x_exception_material_tbl);

      IF l_return_status NOT IN (FND_API.G_RET_STS_SUCCESS, gme_common_pvt.g_exceptions_err) THEN
        x_return_status := l_return_status;
        RAISE error_release_batch;
      END IF;

      IF l_return_status = gme_common_pvt.g_exceptions_err THEN
        x_return_status := gme_common_pvt.g_exceptions_err;
      END IF;
      
      l_update_matl := FALSE;
      
      IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' after release_batch for phantom batch; it returned x_return_status = '||x_return_status);
      END IF;
    ELSIF p_consume THEN
      gme_material_detail_pvt.get_item_rec
                        (p_org_id                => l_matl_dtl_rec.organization_id
                        ,p_item_id               => l_matl_dtl_rec.inventory_item_id
                        ,x_item_rec              => l_item_rec
                        ,x_return_status         => l_return_status);
                        
      IF l_return_status <> FND_API.G_RET_STS_SUCCESS THEN
        x_return_status := l_return_status;
        RAISE error_get_item;
      END IF;

      IF p_update_inv_ind = 'Y' AND
         l_item_rec.mtl_transactions_enabled_flag = 'Y' THEN
        --Pawan Kumar  bug 4742244 --
        -- check for item which dispensable but non-reservable
        IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
          gme_debug.put_line(g_pkg_name||'.'||l_api_name||'disp ind '||l_matl_dtl_rec.dispense_ind);
          gme_debug.put_line(g_pkg_name||'.'||l_api_name||'reservable_type '||l_item_rec.reservable_type);
        END IF;
        IF nvl(l_matl_dtl_rec.dispense_ind, 'N' ) = 'Y' AND 
               l_item_rec.reservable_type <> 1 THEN 
               RAISE error_dispense_non_reserve;
        END IF;   

        consume_material(p_material_dtl_rec    => l_matl_dtl_rec
                        ,p_trans_date          => p_trans_date
                        ,p_item_rec            => l_item_rec
                        ,x_exception_material_tbl      => x_exception_material_tbl
                        ,x_actual_qty          => l_actual_qty
                        ,x_return_status       => l_return_status);
                        
        l_matl_dtl_rec.actual_qty := l_actual_qty;

        IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
          gme_debug.put_line(g_pkg_name||'.'||l_api_name||' after consume_material; it returned actual_qty = '||l_actual_qty);
        END IF;

        IF l_return_status NOT IN (FND_API.G_RET_STS_SUCCESS, gme_common_pvt.g_exceptions_err) THEN
          x_return_status := l_return_status;
          RAISE error_consume_material;
        END IF;
       
        IF l_return_status = gme_common_pvt.g_exceptions_err THEN
          x_return_status := gme_common_pvt.g_exceptions_err;
        END IF;
      ELSE
        l_matl_dtl_rec.actual_qty := l_matl_dtl_rec.plan_qty;
      END IF;
      
      l_update_matl := TRUE;

    ELSE -- ELSIF p_consume
      l_update_matl := TRUE;
      l_matl_dtl_rec.actual_qty := 0;
    END IF;  -- IF l_matl_dtl_rec.phantom_id IS NOT NULL...

   --Bug 8468926   To overwrite the wip plan qty if there is not already a value there
    --IF l_update_matl THEN
     IF l_update_matl and NVL( l_matl_dtl_rec.wip_plan_qty, 0) = 0 THEN
      -- set WIP plan qty
      l_matl_dtl_rec.wip_plan_qty := l_matl_dtl_rec.plan_qty;

      IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' before update_row; actual_qty = '||l_matl_dtl_rec.actual_qty);
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' before update_row; wip_plan_qty = '||l_matl_dtl_rec.wip_plan_qty);
      END IF;

      IF NOT gme_material_details_dbl.update_row (l_matl_dtl_rec) THEN
        RAISE error_update_row;
      END IF;
    END IF;

    IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
      gme_debug.put_line('Exiting api '||g_pkg_name||'.'||l_api_name||' with x_return_status = '||x_return_status);
    END IF;

  EXCEPTION
  WHEN error_update_row OR error_fetch_batch THEN
    gme_common_pvt.log_message ('GME_UNEXPECTED_ERROR', 'ERROR', SQLERRM);
    x_return_status := FND_API.g_ret_sts_unexp_error;
  WHEN  error_release_batch OR error_consume_material OR error_get_item THEN
    NULL;
  WHEN  error_dispense_non_reserve  THEN
    gme_common_pvt.log_message ('GME_DISPENSE_NON_RESERVE');
    x_return_status := fnd_api.g_ret_sts_error;
  -- Bug 21101876, Added by QZENG, raise error when phantom batch is onhold.
  WHEN error_phantom_batch_hold THEN
    gme_common_pvt.log_message ('GME_PHANTOM_ONHOLD', 'BATCH_NO', l_batch_no);
    x_return_status := fnd_api.g_ret_sts_error;
  WHEN OTHERS THEN
    fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
    IF g_debug <= gme_debug.g_log_procedure THEN
      gme_debug.put_line('Unexpected error: '||g_pkg_name||'.'||l_api_name||': '||SQLERRM);
    END IF;
    x_return_status := FND_API.g_ret_sts_unexp_error;
  END process_ingredient;

  -- Note: p_consume_qty is the target actual qty; for incr, it's also the target, not the incr
  PROCEDURE consume_material(p_material_dtl_rec  IN gme_material_details%ROWTYPE
                            ,p_consume_qty       IN NUMBER := NULL
                            ,p_trans_date        IN DATE := NULL
                            ,p_called_by         IN VARCHAR2 DEFAULT 'REL' -- Bug 22217179/21078209/21122837 - This is specific to IB                                        
                            ,p_tolerance         IN NUMBER DEFAULT .9997   -- Bug 22317010 / 22764488 - Added for IB             
                            ,p_item_rec          IN mtl_system_items_b%ROWTYPE
                            ,x_exception_material_tbl    IN OUT NOCOPY gme_common_pvt.exceptions_tab
                            ,x_actual_qty        OUT NOCOPY NUMBER
                            ,x_return_status     OUT NOCOPY VARCHAR2) IS

    l_api_name         CONSTANT   VARCHAR2 (30)                := 'CONSUME_MATERIAL';
    
    l_reservation_rec             mtl_reservations%ROWTYPE;
    l_reservation_tab             gme_common_pvt.reservations_tab;
    i                             NUMBER;
    l_rsrv_type                   NUMBER;
    l_start_actual_qty            NUMBER;

    l_PLR_tab                     gme_common_pvt.reservations_tab;
    j                             NUMBER;
    l_try_PLR                     BOOLEAN;
    l_partial_rec                 mtl_reservations%ROWTYPE;
    l_pending_mo_ind              BOOLEAN := NULL;
    l_pending_rsrv_ind            BOOLEAN := NULL;

    l_consume_qty                 NUMBER;
    l_trans_date                  DATE;
    l_subinv                      VARCHAR2(10);
    l_locator_id                  NUMBER;
    l_revision                    VARCHAR2(3);
    l_return_status               VARCHAR2(1);
    
    l_qoh                         NUMBER;
    l_rqoh                        NUMBER;
    l_qr                          NUMBER;
    l_qs                          NUMBER;
    l_att                         NUMBER;
    l_atr                         NUMBER;
    l_sqoh                        NUMBER;
    l_srqoh                       NUMBER;
    l_sqr                         NUMBER;
    l_sqs                         NUMBER;
    l_satt                        NUMBER;
    l_satr                        NUMBER;
    l_msg_count                   NUMBER;
    l_msg_data                    VARCHAR2(32767);
    
    l_eff_locator_control         NUMBER;

    l_tolerance                   NUMBER;   -- Bug 22317010 / 22764488 - Added for IB             

    l_lot_orig_date               DATE; -- Bug 12971020
    error_bad_trans_date          EXCEPTION;
    
    CURSOR cur_get_item_revision(v_item_id NUMBER, v_org_id NUMBER) IS
    SELECT revision
      FROM mtl_item_revisions_b
     WHERE inventory_item_id = v_item_id
       AND organization_id = v_org_id
       AND effectivity_date <= gme_common_pvt.g_timestamp
     ORDER BY effectivity_date desc;

    error_get_item                EXCEPTION;
    error_build_trxn              EXCEPTION;
    error_get_exception           EXCEPTION;
    error_convert_partial         EXCEPTION;
    error_unexpected              EXCEPTION;
    consume_done                  EXCEPTION;
    error_get_reservations        EXCEPTION;
    no_consume_required           EXCEPTION;

  BEGIN

    IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
       gme_debug.put_line('Entering api '||g_pkg_name||'.'||l_api_name);
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' material_detail_id = '||p_material_dtl_rec.material_detail_id);
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' dispense_ind = '||p_material_dtl_rec.dispense_ind);
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_consume_qty = '||p_consume_qty);
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_trans_date = '||to_char(p_trans_date
                                                                                ,'YYYY-MON-DD HH24:MI:SS'));
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' line_no = '||p_material_dtl_rec.line_no);
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' line_type = '||p_material_dtl_rec.line_type);
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_called_by = '||p_called_by);
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_tolerance passed in = '||p_tolerance);
    END IF;    
    
    /* Set the return status to success initially */
    x_return_status       := FND_API.G_RET_STS_SUCCESS;

    -- set the output actual qty to it's current value...
    x_actual_qty := p_material_dtl_rec.actual_qty;
    
    -- following global is set only for migration purposes, where transactions need not be created,
    IF gme_release_batch_pvt.g_bypass_txn_creation = 1 THEN
       RAISE no_consume_required;
    END IF;

    -- Bug 22317010 / 22764488 - Added for IB
    IF NVL(p_tolerance, .9997) >= .99 AND NVL(p_tolerance, .9997) <= 1 THEN
       l_tolerance := p_tolerance;
    ELSE
       l_tolerance := .9997;
    END IF;

    IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_tolerance used = '||l_tolerance);
    END IF;
    
    l_start_actual_qty := x_actual_qty;
    -- Bug 16423133 - Moved this code up so that l_consume_qty is intialized correctly.
    -- Bug 8468926 - Default consume qty properly.
    -- l_consume_qty := NVL(p_consume_qty, p_material_dtl_rec.plan_qty);    
    IF NVL(p_material_dtl_rec.wip_plan_qty, 0) > 0 THEN
       l_consume_qty := NVL(p_consume_qty, p_material_dtl_rec.wip_plan_qty);
    ELSE
       l_consume_qty := NVL(p_consume_qty, p_material_dtl_rec.plan_qty);
    END IF;
 
    -- Couple of optimizations...
    -- If consume from supply sub is set to Yes and there's no supply sub, then return with exceptions... can't do anything
    -- If consume from supply sub is set to No and there's no supply sub, then consume DLR, and return with exceptions (if appl)
    -- If reservable is set to No don't bother to retrieve the reservations... there aren't any...

    IF gme_common_pvt.g_auto_consume_supply_sub_only = 1 THEN
       IF p_material_dtl_rec.subinventory IS NULL THEN
       	 l_pending_mo_ind := FALSE;  -- can't have move order if sub is NULL
          IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
             gme_debug.put_line(g_pkg_name||'.'||l_api_name||' auto consume from supply sub is ON and subinv on material is NULL; cant consume anything; get exceptions');
          END IF;
          
          RAISE error_get_exception;
       END IF;
    END IF;
    
    l_subinv := p_material_dtl_rec.subinventory;
    l_locator_id := p_material_dtl_rec.locator_id;

    IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' l_subinv = '||l_subinv);
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' l_locator_id = '||l_locator_id);
    END IF;

    -- changes for GMO
    gme_reservations_pvt.get_material_reservations
              (p_organization_id       => p_material_dtl_rec.organization_id
              ,p_batch_id              => p_material_dtl_rec.batch_id
              ,p_material_detail_id    => p_material_dtl_rec.material_detail_id
              ,p_dispense_ind          => nvl(p_material_dtl_rec.dispense_ind,'N')
              ,x_return_status         => l_return_status
              ,x_reservations_tbl      => l_reservation_tab);

    IF l_return_status <> FND_API.G_RET_STS_SUCCESS THEN
       x_return_status := l_return_status;
       RAISE error_get_reservations;
    END IF;

    l_trans_date := NVL(p_trans_date, gme_common_pvt.g_timestamp);

    IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' l_consume_qty = '||l_consume_qty);
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' l_trans_date = '||to_char(p_trans_date
                                                                                ,'YYYY-MON-DD HH24:MI:SS'));
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' x_actual_qty = '||x_actual_qty);
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' number of reservations = '||l_reservation_tab.COUNT);
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' auto consume from supply sub = '||gme_common_pvt.g_auto_consume_supply_sub_only);
    END IF;

    i := 1;
    j := 1;

    WHILE l_consume_qty > x_actual_qty AND i <= l_reservation_tab.COUNT LOOP
       -- Consume all fully specified reservations and mark the Partial ones
       l_reservation_rec := l_reservation_tab(i);
       IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
          gme_debug.put_line(g_pkg_name||'.'||l_api_name||' in reservation loop: reservation_id = '||l_reservation_rec.reservation_id);
          gme_debug.put_line(g_pkg_name||'.'||l_api_name||' in reservation loop: reservation subinventory = '||l_reservation_rec.subinventory_code);
          gme_debug.put_line(g_pkg_name||'.'||l_api_name||' in reservation loop: reservation external_source_line_id = '||l_reservation_rec.external_source_line_id);
       END IF;
       
       /* Bug 5441643 Added NVL condition for location control code*/
       l_rsrv_type := gme_reservations_pvt.reservation_fully_specified
                       (p_reservation_rec         => l_reservation_rec
                       ,p_item_location_control   => NVL(p_item_rec.location_control_code,1)
                       ,p_item_restrict_locators  => p_item_rec.restrict_locators_code
                       ,p_lot_control_code        => p_item_rec.lot_control_code); -- Bug 18044719
       
       IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
          gme_debug.put_line(g_pkg_name||'.'||l_api_name||' after call to gme_reservations_pvt.reservation_fully_specified: l_rsrv_type = '||l_rsrv_type);
       END IF;
       
       IF l_rsrv_type = -1 THEN
          IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
             gme_debug.put_line(g_pkg_name||'.'||l_api_name||' l_rsrv_type passed back as -1 from gme_reservations_pvt.reservation_fully_specified');
             RAISE error_unexpected;
          END IF;
       END IF;

       -- Bug 12971020 - Let's make sure that trans date is after lot origination date.
       IF (l_reservation_rec.lot_number IS NOT NULL) THEN
          SELECT origination_date INTO l_lot_orig_date 
          FROM   MTL_LOT_NUMBERS 
          WHERE  INVENTORY_ITEM_ID = l_reservation_rec.inventory_item_id 
          AND    LOT_NUMBER = l_reservation_rec.lot_number
          AND    ORGANIZATION_ID = l_reservation_rec.organization_id;
       
          IF (p_trans_date < l_lot_orig_date) THEN
             gme_common_pvt.log_message (p_product_code => 'INV', 
                                         p_message_code => 'INV_INT_TDATECODE');          
             RAISE error_bad_trans_date;
          END IF;
       END IF;
       
       --
       -- bug 12695713
       -- Setting the revision before creating the txn
       --
       l_revision := NULL;
       IF p_item_rec.revision_qty_control_code = 2 THEN -- under revision control
          IF p_material_dtl_rec.revision IS NOT NULL THEN
             l_revision := p_material_dtl_rec.revision;
          ELSE
             OPEN cur_get_item_revision(p_material_dtl_rec.inventory_item_id,
                                        p_material_dtl_rec.organization_id);
             FETCH cur_get_item_revision INTO l_revision;
             CLOSE cur_get_item_revision;
          END IF;
       END IF;  -- IF p_revision_qty_control_code = 2

       IF l_rsrv_type IN (0, 2) THEN -- HLR or PLR 
          -- save these for later; if there's not enough DLR, PLR will be filled in and used
          l_PLR_tab(j) := l_reservation_rec;
          j := j + 1;
       ELSE  -- detailed level reservation
          IF (gme_common_pvt.g_auto_consume_supply_sub_only = 0) OR
             (gme_common_pvt.g_auto_consume_supply_sub_only = 1 AND
             l_reservation_rec.subinventory_code = l_subinv) THEN
              	-- GMO Changes
             IF ((NVL(p_material_dtl_rec.dispense_ind,'N') = 'Y' AND
                 l_reservation_rec.external_source_line_id IS NOT NULL ) OR
                 NVL(p_material_dtl_rec.dispense_ind,'N') = 'N' )    THEN
                build_and_create_transaction
                   (p_rsrv_rec              => l_reservation_rec
                   ,p_lot_divisible_flag    => p_item_rec.lot_divisible_flag
                   ,p_dispense_ind          => p_material_dtl_rec.dispense_ind
                   ,p_mtl_dtl_rec           => p_material_dtl_rec
                   ,p_trans_date            => l_trans_date
                   ,p_consume_qty           => l_consume_qty
                   ,p_called_by             => p_called_by   -- Bug 22217179/21078209/21122837 - This still defaults to REL flow but can be different.
                   ,p_tolerance             => l_tolerance   -- Bug 22317010 / 22764488 - Added for IB
                   ,p_revision              => l_revision -- NULL bug 12695713. Passing the revision
                   ,p_secondary_uom_code    => p_item_rec.secondary_uom_code
                   ,x_actual_qty            => x_actual_qty
                   ,x_return_status         => l_return_status);
                 
                IF l_return_status NOT IN (gme_common_pvt.g_not_transactable, FND_API.G_RET_STS_SUCCESS) THEN
                   x_return_status := l_return_status;
                   RAISE error_build_trxn;
                END IF;
             END IF; --  p_material_dtl_rec.dispense_ind = 'Y'
          END IF;  -- IF (gme_common_pvt.g_auto_consume_supply_sub_only = 0) OR...
       END IF;  -- IF l_rsrv_type = ...
       i := i + 1; -- move on to the next reservation
    END LOOP;

    IF x_actual_qty >= l_consume_qty THEN
       IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
          gme_debug.put_line(g_pkg_name||'.'||l_api_name||' consumption complete: x_actual_qty = '||x_actual_qty||' and l_consume_qty = '||l_consume_qty);
       END IF;
       -- done!
       RAISE consume_done;
    END IF;
    
    IF (gme_common_pvt.g_auto_consume_supply_sub_only = 0 AND l_subinv IS NULL) THEN
       IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
          gme_debug.put_line(g_pkg_name||'.'||l_api_name||' exception; qty not met; consume from supply sub is FALSE and material subinv is NULL');
       END IF;
       RAISE error_get_exception;
    END IF;

    -- Changes for GMO
    IF NVL(p_material_dtl_rec.dispense_ind, 'N') = 'Y' THEN
       -- if you get to this point, raise exception; can't process PLR/HLR for dispensed records; nor
       -- can you get available inventory; record must be dispensed to process it
       IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
          gme_debug.put_line(g_pkg_name||'.'||l_api_name||' dispensed item; get exceptions: x_actual_qty = '||x_actual_qty||' and l_consume_qty = '||l_consume_qty);
       END IF;
       RAISE error_get_exception;
    END IF;

    l_pending_mo_ind := gme_move_orders_pvt.pending_move_orders_exist
                                (p_organization_id         => p_material_dtl_rec.organization_id
                                ,p_batch_id                => p_material_dtl_rec.batch_id
                                ,p_material_detail_id      => p_material_dtl_rec.material_detail_id);

    IF p_item_rec.lot_control_code = 2 THEN    -- lot control
       IF gme_common_pvt.g_auto_consume_supply_sub_only = 1 THEN  -- auto consume -> Yes
          IF l_pending_mo_ind THEN
             l_try_PLR := FALSE;
             IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
                gme_debug.put_line(g_pkg_name||'.'||l_api_name||' consume from supply sub ON; pending MO TRUE: l_try_PLR := FALSE; get batch exception');
             END IF;
          ELSE
             l_try_PLR := TRUE;
          END IF;
       ELSE
          l_try_PLR := TRUE;
       END IF;
         
       IF NOT l_try_PLR THEN
          IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
             gme_debug.put_line(g_pkg_name||'.'||l_api_name||' lot control item; get exceptions: x_actual_qty = '||x_actual_qty||' and l_consume_qty = '||l_consume_qty);
          END IF;
          RAISE error_get_exception;
       END IF;
    END IF;

    -- at this point, it's a lot control item with demand not met and no pending move orders OR
    -- a plain, revision or locator ctrl item
    -- try to convert and consume Partial reservations
    
    -- Bug 8277090 - Initialize loop counter.
    i := 1;
    WHILE l_consume_qty > x_actual_qty AND i <= l_PLR_tab.COUNT LOOP
       -- try to convert PLR to DLR
       l_partial_rec := l_PLR_tab(i);
       
       gme_reservations_pvt.convert_partial_to_dlr
                           (p_reservation_rec          => l_partial_rec
                           ,p_material_dtl_rec         => p_material_dtl_rec
                           ,p_item_rec                 => p_item_rec
                           ,x_reservation_rec          => l_reservation_rec
                           ,x_return_status            => l_return_status);
                           
       IF l_return_status <> FND_API.G_RET_STS_SUCCESS THEN
          IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
             gme_debug.put_line(g_pkg_name||'.'||l_api_name||' could not convert partial to dlr for reservation id = '||l_partial_rec.reservation_id||'; moving to next partial');
             
             -- Bug 18044719 -- extra message
             IF (l_partial_rec.lot_number IS NULL) THEN       
                gme_debug.put_line('LOT NUMBER IS NULL');
             ELSE
                gme_debug.put_line('LOT NUMBER IS '||l_partial_rec.lot_number);
             END IF;
          END IF;
       ELSE
          -- Bug 8277090 - Initialize locator_id properly if required.    
          IF l_reservation_rec.locator_id IS NULL AND
             p_material_dtl_rec.locator_id IS NOT NULL THEN
             l_reservation_rec.locator_id := p_material_dtl_rec.locator_id;
          END IF;
          
          IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
             gme_debug.put_line(g_pkg_name||'.'||l_api_name||'Calling query quantities for PLR');
             gme_debug.put_line('inventory_item_id is '||p_material_dtl_rec.inventory_item_id);
             gme_debug.put_line('material_detail_id is '||p_material_dtl_rec.material_detail_id);
          END IF;

          -- Bug 13949475 - Let's see if there is enough inventory.  
          gme_transactions_pvt.query_quantities
          (
             p_api_version_number   	    => 1
            ,p_init_msg_lst         	    => fnd_api.g_false
            ,x_return_status        	    => l_return_status
            ,x_msg_count            	    => l_msg_count
            ,x_msg_data             	    => l_msg_data
            ,p_organization_id             => p_material_dtl_rec.organization_id
            ,p_inventory_item_id           => p_material_dtl_rec.inventory_item_id
            ,p_tree_mode                   => inv_quantity_tree_pub.g_transaction_mode
            ,p_is_serial_control           => FALSE
            ,p_grade_code                  => NULL
            ,p_demand_source_type_id       => gme_common_pvt.g_txn_source_type
            ,p_demand_source_header_id     => p_material_dtl_rec.batch_id
            ,p_demand_source_line_id       => p_material_dtl_rec.material_detail_id
            ,p_demand_source_name          => NULL
            ,p_lot_expiration_date         => NULL
            ,p_revision             	    => l_revision
            ,p_lot_number           	    => NULL
            ,p_subinventory_code    	    => l_reservation_rec.subinventory_code
            ,p_locator_id           	    => l_reservation_rec.locator_id
            ,p_onhand_source		          => inv_quantity_tree_pvt.g_all_subs
            ,x_qoh                  	    => l_qoh
            ,x_rqoh                 	    => l_rqoh
            ,x_qr                   	    => l_qr
            ,x_qs                   	    => l_qs
            ,x_att                  	    => l_att
            ,x_atr                  	    => l_atr
            ,x_sqoh                  	    => l_sqoh
            ,x_srqoh                 	    => l_srqoh
            ,x_sqr                   	    => l_sqr
            ,x_sqs                   	    => l_sqs
            ,x_satt                  	    => l_satt
            ,x_satr                  	    => l_satr
            ,p_transfer_subinventory_code  => NULL
            ,p_cost_group_id		          => NULL
            ,p_lpn_id			             => NULL
            ,p_transfer_locator_id	       => NULL
          );
         
          IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
             gme_debug.put_line(g_pkg_name||'.'||l_api_name||' after query quantities return status = '||l_return_status);
             gme_debug.put_line(g_pkg_name||'.'||l_api_name||' after query quantities att = '||l_att);
             gme_debug.put_line(g_pkg_name||'.'||l_api_name||' after query quantities qoh = '||l_qoh);
             gme_debug.put_line(g_pkg_name||'.'||l_api_name||' after query quantities item_id = '||p_material_dtl_rec.inventory_item_id);
             gme_debug.put_line(g_pkg_name||'.'||l_api_name||' after query quantities sub = '||l_subinv);
             gme_debug.put_line(g_pkg_name||'.'||l_api_name||' after query quantities loc = '||l_reservation_rec.locator_id);
          END IF;

          -- Internal note - down the road this may need to be reconstructed like 7709971 below to consider allow neg inv.
          IF l_return_status = FND_API.G_RET_STS_SUCCESS AND l_att > 0 THEN                     
             build_and_create_transaction
                   (p_rsrv_rec              => l_reservation_rec
                   ,p_lot_divisible_flag    => p_item_rec.lot_divisible_flag
                   ,p_mtl_dtl_rec           => p_material_dtl_rec
                   ,p_trans_date            => l_trans_date
                   ,p_consume_qty           => l_consume_qty
                   ,p_called_by             => p_called_by   -- Bug 22217179/21078209/21122837 - This still defaults to REL flow but can be different.
                   ,p_tolerance             => l_tolerance   -- Bug 22317010 / 22764488 - Added for IB             
                   ,p_revision              => l_revision -- NULL bug 12695713. Passing the revision
                   ,p_secondary_uom_code    => p_item_rec.secondary_uom_code
                   ,x_actual_qty            => x_actual_qty
                   ,x_return_status         => l_return_status);
                   
             IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
                gme_debug.put_line('after build and create');
             END IF;
                   
             IF l_return_status NOT IN (gme_common_pvt.g_not_transactable, FND_API.G_RET_STS_SUCCESS) THEN
                x_return_status := l_return_status;
                RAISE error_build_trxn;
             END IF;
          
             IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
                gme_debug.put_line('after build and create build is successful');
             END IF;             
          END IF;  -- IF l_return_status <> FND_API.G_RET_STS_SUCCESS AND l_att
       END IF;  -- IF l_return_status <> FND_API.G_RET_STS_SUCCESS for gme_reservations_pvt.convert_partial_to_dlr

       i := i + 1; -- move on to the next partial reservation
    END LOOP;

    -- Bug 8277090 - See if we have satisfied consumption qty..    
    IF x_actual_qty >= l_consume_qty THEN
       IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
          gme_debug.put_line(g_pkg_name||'.'||l_api_name||' consumption complete: x_actual_qty = '||x_actual_qty||' and l_consume_qty = '||l_consume_qty);
       END IF;
       -- done!
       RAISE consume_done;
    END IF;
    
    -- If it's lot control and the qty is still not satisfied, get exceptions;
    IF p_item_rec.lot_control_code = 2 THEN    -- lot control
       IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
          gme_debug.put_line(g_pkg_name||'.'||l_api_name||' lot control; DLR and PLR have been exhausted; get exception');
       END IF;
       RAISE error_get_exception;
    END IF;

    -- If it's plain, revision or locator, try to get from supply sub and supply locator
    -- get qty tree rec in subinv/loc
    --
    -- bug 12695713
    -- re-initializing the l_revision variable
    --
    l_revision := NULL;
    IF p_item_rec.revision_qty_control_code = 2 THEN -- under revision control
       IF p_material_dtl_rec.revision IS NOT NULL THEN
          l_revision := p_material_dtl_rec.revision;
       ELSE
          OPEN cur_get_item_revision(p_material_dtl_rec.inventory_item_id,
                                     p_material_dtl_rec.organization_id);
          FETCH cur_get_item_revision INTO l_revision;
          CLOSE cur_get_item_revision;
       END IF;
    END IF;  -- IF p_revision_qty_control_code = 2

    IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' l_revision='||l_revision);
    END IF;

    IF l_locator_id IS NULL THEN
       -- check if it's locator control, we need a locator...
       /* Bug 5441643 Added NVL condition for location control code*/      
       l_eff_locator_control :=
                gme_common_pvt.eff_locator_control
                      (p_organization_id        => p_material_dtl_rec.organization_id
                      ,p_org_control            => gme_common_pvt.g_org_locator_control
                      ,p_subinventory           => p_material_dtl_rec.subinventory
                      ,p_item_control           => NVL(p_item_rec.location_control_code,1)
                      ,p_item_loc_restrict      => p_item_rec.restrict_locators_code
                      ,p_action                 => gme_common_pvt.g_ing_issue_txn_action);
                      
       IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
           gme_debug.put_line(g_pkg_name||'.'||l_api_name||' l_eff_locator_control = '||l_eff_locator_control);
           gme_debug.put_line(g_pkg_name||'.'||l_api_name||' organization_id = '||p_material_dtl_rec.organization_id);
           gme_debug.put_line(g_pkg_name||'.'||l_api_name||' g_org_locator_control = '||gme_common_pvt.g_org_locator_control);
           gme_debug.put_line(g_pkg_name||'.'||l_api_name||' subinventory = '||p_material_dtl_rec.subinventory);
           gme_debug.put_line(g_pkg_name||'.'||l_api_name||' inventory_item_id = '||p_item_rec.inventory_item_id);
           gme_debug.put_line(g_pkg_name||'.'||l_api_name||' location_control_code = '||p_item_rec.location_control_code);
           gme_debug.put_line(g_pkg_name||'.'||l_api_name||' restrict_locators_code = '||p_item_rec.restrict_locators_code);
           gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_action = '||gme_common_pvt.g_ing_issue_txn_action);
       END IF;
       
       IF l_eff_locator_control <> 1 THEN
          IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
             gme_debug.put_line(g_pkg_name||'.'||l_api_name||' locator on material is NULL and material is eff locator control; cant get quantities from inventory; get exceptions');
          END IF;
          RAISE error_get_exception;
       END IF;
    END IF;

    gme_transactions_pvt.query_quantities
    (
       p_api_version_number   	   => 1
      ,p_init_msg_lst         	   => fnd_api.g_false
      ,x_return_status        	   => l_return_status
      ,x_msg_count            	   => l_msg_count
      ,x_msg_data             	   => l_msg_data
      ,p_organization_id            => p_material_dtl_rec.organization_id
      ,p_inventory_item_id          => p_material_dtl_rec.inventory_item_id
      ,p_tree_mode                  => inv_quantity_tree_pub.g_transaction_mode
      ,p_is_serial_control          => FALSE
      ,p_grade_code                 => NULL
      ,p_demand_source_type_id      => gme_common_pvt.g_txn_source_type
      ,p_demand_source_header_id    => p_material_dtl_rec.batch_id
      ,p_demand_source_line_id      => p_material_dtl_rec.material_detail_id
      ,p_demand_source_name         => NULL
      ,p_lot_expiration_date        => NULL
      ,p_revision             	   => l_revision
      ,p_lot_number           	   => NULL
      ,p_subinventory_code    	   => l_subinv
      ,p_locator_id           	   => l_locator_id
      ,p_onhand_source		         => inv_quantity_tree_pvt.g_all_subs
      ,x_qoh                  	   => l_qoh
      ,x_rqoh                 	   => l_rqoh
      ,x_qr                   	   => l_qr
      ,x_qs                   	   => l_qs
      ,x_att                  	   => l_att
      ,x_atr                  	   => l_atr
      ,x_sqoh                  	   => l_sqoh
      ,x_srqoh                 	   => l_srqoh
      ,x_sqr                   	   => l_sqr
      ,x_sqs                   	   => l_sqs
      ,x_satt                  	   => l_satt
      ,x_satr                  	   => l_satr
      ,p_transfer_subinventory_code => NULL
      ,p_cost_group_id		         => NULL
      ,p_lpn_id			            => NULL
      ,p_transfer_locator_id	      => NULL
    );
    
    -- Additional debug messages for Bug 24350177 / 24362821
    IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' Quantities from tree for Plain item');
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' after query quantities return status = '||l_return_status);
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' after query quantities item_id = '||p_material_dtl_rec.inventory_item_id);
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' after query quantities sub = '||l_subinv);
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' after query quantities loc = '||l_locator_id);
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' after query quantities rev = '||l_revision);
       
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' after query quantities l_qoh = '||l_qoh);
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' after query quantities l_rqoh = '||l_rqoh);
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' after query quantities l_qr = '||l_qr);
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' after query quantities l_qs = '||l_qs);
       
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' after query quantities l_att = '||l_att);
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' after query quantities l_atr = '||l_atr);
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' after query quantities l_sqoh = '||l_sqoh);
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' after query quantities l_srqoh = '||l_srqoh);
       
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' after query quantities l_sqr = '||l_sqr);
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' after query quantities l_sqs = '||l_sqs);
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' after query quantities l_satt = '||l_satt);
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' after query quantities l_satr = '||l_satr);

       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' gme_common_pvt.g_allow_neg_inv = '||gme_common_pvt.g_allow_neg_inv);       
    END IF;

    -- Bug 7709971 - Restructure this condition to handle orgs that allow negative inventory.
    -- g_allow_neg_inv:  2 means do not allow neg inv whereas 1 means allow it.
    IF l_return_status = FND_API.G_RET_STS_SUCCESS THEN
       IF l_att > 0 AND gme_common_pvt.g_allow_neg_inv = 2 THEN

          -- Additional debug messages for Bug 24350177 / 24362821
          IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
             gme_debug.put_line(g_pkg_name||'.'||l_api_name||'Negative Inventory is not allowed.');       
          END IF;
       
          build_and_create_transaction
                  (p_rsrv_rec              => NULL
                  ,p_subinv                => l_subinv
                  ,p_locator_id            => l_locator_id
                  ,p_att                   => l_att
                  ,p_satt                  => l_satt
                  ,p_primary_uom_code      => p_item_rec.primary_uom_code
                  ,p_mtl_dtl_rec           => p_material_dtl_rec
                  ,p_trans_date            => l_trans_date
                  ,p_consume_qty           => l_consume_qty
                  ,p_called_by             => p_called_by   -- Bug 22217179/21078209/21122837 - This still defaults to REL flow but can be different.
                  ,p_tolerance             => l_tolerance   -- Bug 22317010 / 22764488 - Added for IB             
                  ,p_revision              => l_revision
                  ,p_secondary_uom_code    => p_item_rec.secondary_uom_code
                  ,x_actual_qty            => x_actual_qty
                  ,x_return_status         => l_return_status);
          
          IF l_return_status NOT IN (gme_common_pvt.g_not_transactable, FND_API.G_RET_STS_SUCCESS) THEN
             x_return_status := l_return_status;
             RAISE error_build_trxn;
          END IF;
       ELSIF gme_common_pvt.g_allow_neg_inv = 1 THEN
          -- If we are here, we are going to build a transaction based on the l_consume_qty
          -- even if this drives inventory negative or if inventory is already negative.

          -- Additional debug messages for Bug 24350177 / 24362821
          IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
             gme_debug.put_line(g_pkg_name||'.'||l_api_name||'Negative Inventory is ALLOWED.');       
          END IF;
          
          -- Let's set the secondary_qty to consumed if the user is working in secondary UOM.
          l_satt := NULL;
          IF (p_item_rec.secondary_uom_code = p_material_dtl_rec.dtl_um
              AND p_item_rec.secondary_uom_code IS NOT NULL) THEN
             l_satt := l_consume_qty;
          END IF;
          
          -- Introduced new value for p_called_by to be used by function being called.
          -- l_consume_qty is always in the detail uom.
          build_and_create_transaction
                  (p_rsrv_rec              => NULL
                  ,p_subinv                => l_subinv
                  ,p_locator_id            => l_locator_id
                  ,p_att                   => l_consume_qty
                  ,p_satt                  => l_satt
                  ,p_primary_uom_code      => p_item_rec.primary_uom_code
                  ,p_mtl_dtl_rec           => p_material_dtl_rec
                  ,p_trans_date            => l_trans_date
                  ,p_consume_qty           => l_consume_qty
                  ,p_called_by             => 'REL2'                                
                  ,p_revision              => l_revision
                  ,p_secondary_uom_code    => p_item_rec.secondary_uom_code
                  ,x_actual_qty            => x_actual_qty
                  ,x_return_status         => l_return_status);
          IF l_return_status NOT IN (gme_common_pvt.g_not_transactable, FND_API.G_RET_STS_SUCCESS) THEN
             x_return_status := l_return_status;
             RAISE error_build_trxn;
          END IF;  
          -- done!
          RAISE consume_done;                                            
       END IF;       
    END IF;

    IF x_actual_qty < l_consume_qty THEN
       RAISE error_get_exception;
    END IF;
    
    IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
       gme_debug.put_line('Exiting api '||g_pkg_name||'.'||l_api_name);
    END IF;

  EXCEPTION
  WHEN error_build_trxn OR error_get_item OR error_convert_partial OR consume_done OR
       error_get_reservations OR no_consume_required THEN
    NULL;
  WHEN error_get_exception THEN
    IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
      gme_debug.put_line(g_pkg_name||'.'||l_api_name||' exception block for get exceptions:');
      gme_debug.put_line(g_pkg_name||'.'||l_api_name||' x_actual_qty = '||x_actual_qty);
      gme_debug.put_line(g_pkg_name||'.'||l_api_name||' l_start_actual_qty = '||l_start_actual_qty);
      gme_debug.put_line(g_pkg_name||'.'||l_api_name||' l_consume_qty = '||l_consume_qty);
      gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_exception_qty = '||(l_consume_qty - x_actual_qty));
    END IF;
    
    create_batch_exception
                    (p_material_dtl_rec         => p_material_dtl_rec
                    ,p_pending_move_order_ind   => l_pending_mo_ind
                    ,p_pending_rsrv_ind         => l_pending_rsrv_ind
                    ,p_transacted_qty           => x_actual_qty - l_start_actual_qty
                    ,p_exception_qty            => l_consume_qty - x_actual_qty
                    ,p_force_unconsumed         => fnd_api.g_true 
                    ,x_exception_material_tbl   => x_exception_material_tbl
                    ,x_return_status            => x_return_status);
  WHEN error_bad_trans_date THEN
    x_return_status := FND_API.g_ret_sts_unexp_error;                    
  WHEN error_unexpected THEN
    x_return_status := FND_API.g_ret_sts_unexp_error;
  WHEN OTHERS THEN
    fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
    IF (NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT) THEN
      gme_debug.put_line('Unexpected error: '||g_pkg_name||'.'||l_api_name||': '||SQLERRM);
    END IF;
    x_return_status := FND_API.g_ret_sts_unexp_error;
  END consume_material;

  PROCEDURE build_and_create_transaction
              (p_rsrv_rec              IN mtl_reservations%ROWTYPE
              ,p_lot_divisible_flag    IN VARCHAR2 DEFAULT NULL  -- required for lot non divisible
              ,p_dispense_ind          IN VARCHAR2 DEFAULT NULL
              ,p_subinv                IN VARCHAR2 DEFAULT NULL
              ,p_locator_id            IN NUMBER DEFAULT NULL
              ,p_att                   IN NUMBER DEFAULT NULL
              ,p_satt                  IN NUMBER DEFAULT NULL
              ,p_primary_uom_code      IN VARCHAR2 DEFAULT NULL
              ,p_mtl_dtl_rec           IN gme_material_details%ROWTYPE
              ,p_trans_date            IN DATE
              ,p_consume_qty           IN NUMBER
              ,p_called_by             IN VARCHAR2 DEFAULT 'REL'              
              ,p_tolerance             IN NUMBER DEFAULT .9997   -- Bug 22317010 / 22764488 - Added for IB             
              ,p_revision              IN VARCHAR2 DEFAULT NULL
              ,p_secondary_uom_code    IN VARCHAR2 DEFAULT NULL
              ,x_actual_qty            IN OUT NOCOPY NUMBER
              ,x_return_status         OUT NOCOPY VARCHAR2) IS
    
    CURSOR item_no_cursor(v_inventory_item_id NUMBER,
                          v_org_id            NUMBER) IS
    SELECT concatenated_segments
      FROM mtl_system_items_kfv
     WHERE inventory_item_id = v_inventory_item_id
       AND organization_id = v_org_id;
             
    l_item_no                mtl_system_items_kfv.concatenated_segments%TYPE;

    l_api_name               CONSTANT   VARCHAR2 (30)                := 'build_and_create_transaction';
         
    l_transaction_rec        mtl_transactions_interface%ROWTYPE; 
    l_lot_rec                gme_common_pvt.mtl_trans_lots_inter_tbl;         
    l_rsrv_mode              BOOLEAN;
    l_trxn_qty               NUMBER;
    l_dtl_qty                NUMBER;
    l_prim_qty               NUMBER;
    l_sec_qty                NUMBER;
    l_whole_qty              BOOLEAN;
    l_from_um                VARCHAR2(3);
    l_to_um                  VARCHAR2(3);
    l_primary_um             VARCHAR2(3);
    l_dtl_um                 VARCHAR2(3);    
    l_return_status          VARCHAR2(1);
    l_lot_divisible_flag     VARCHAR2(1);
    
    -- Bug 22217179
    l_relieve_all            VARCHAR2(1);
    
    -- Bug 22567974
    l_precision              NUMBER;    
    l_numerator              NUMBER;    
    l_denominator            NUMBER;    

    -- Bug 24350177
    l_qoh                         NUMBER;
    l_rqoh                        NUMBER;
    l_qr                          NUMBER;
    l_qs                          NUMBER;
    l_att                         NUMBER;
    l_atr                         NUMBER;
    l_sqoh                        NUMBER;
    l_srqoh                       NUMBER;
    l_sqr                         NUMBER;
    l_sqs                         NUMBER;
    l_satt                        NUMBER;
    l_satr                        NUMBER;

    
    --Bug 4899399
    l_msg_count              NUMBER;
    l_msg_data               VARCHAR2(32767);
    error_build_mmti         EXCEPTION;
    error_get_dtl_qty        EXCEPTION;
    error_create_trxn        EXCEPTION;
    error_relieve_rsrv       EXCEPTION;
    um_convert_error         EXCEPTION;
    dispense_error	        EXCEPTION;
  BEGIN
    IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
       gme_debug.put_line('Entering api '||g_pkg_name||'.'||l_api_name);
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_rsrv_rec.reservation_id = '||p_rsrv_rec.reservation_id);
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_rsrv_rec.lot_number = '||p_rsrv_rec.lot_number);
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_lot_divisible_flag = '||p_lot_divisible_flag);
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_subinv = '||p_subinv);
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_locator_id = '||p_locator_id);
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_att = '||p_att);
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_satt = '||p_satt);
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_primary_uom_code = '||p_primary_uom_code);
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_mtl_dtl_rec.material_detail_id = '||p_mtl_dtl_rec.material_detail_id);
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_mtl_dtl_rec.dtl_um = '||p_mtl_dtl_rec.dtl_um);
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_trans_date = '||to_char(p_trans_date
                                                                          ,'YYYY-MON-DD HH24:MI:SS'));
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_consume_qty = '||p_consume_qty);
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_revision = '||p_revision);
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_secondary_uom_code = '||p_secondary_uom_code);
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' x_actual_qty = '||x_actual_qty);
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_called_by = '||p_called_by);
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_tolerance = '||p_tolerance);       
    END IF;
    
    /* Set the return status to success initially */
    x_return_status       := FND_API.G_RET_STS_SUCCESS;

    IF p_rsrv_rec.reservation_id IS NOT NULL THEN
       l_rsrv_mode := TRUE;
    ELSE
       l_rsrv_mode := FALSE;
    END IF;
    
    IF l_rsrv_mode THEN
       constr_mmti_from_reservation
         (p_rsrv_rec               => p_rsrv_rec
         ,x_mmti_rec               => l_transaction_rec
         ,x_mmli_tbl               => l_lot_rec
         ,x_return_status          => x_return_status);
       --
       -- bug 12695713
       -- If the revision is null which would be becasue the Reservations on the 
       -- GME batch form dont have the revision information, then set the revision
       -- passed
       --
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' after constr_mmti_from_reservation revision='||l_transaction_rec.revision);
       IF (l_transaction_rec.revision IS NULL) THEN
          gme_debug.put_line(g_pkg_name||'.'||l_api_name||' after constr_mmti_from_reservation revision is NULL so setting '||p_revision);
          l_transaction_rec.revision := p_revision;
       END IF;
    ELSE
       constr_mmti_from_qty_tree
         (p_mtl_dtl_rec            => p_mtl_dtl_rec
         ,p_subinv                 => p_subinv
         ,p_locator_id             => p_locator_id
         ,x_mmti_rec               => l_transaction_rec
         ,x_return_status          => x_return_status);
       l_transaction_rec.revision := p_revision;
    END IF;
    
    IF x_return_status <> FND_API.G_RET_STS_SUCCESS THEN
       RAISE error_build_mmti;
    END IF;

    IF l_rsrv_mode THEN
       l_prim_qty := p_rsrv_rec.primary_reservation_quantity;
       l_sec_qty := p_rsrv_rec.secondary_reservation_quantity;

       IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
          gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_rsrv_rec.primary_reservation_quantity = '||l_prim_qty);
          gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_rsrv_rec.secondary_reservation_quantity = '||l_sec_qty);
       END IF;
              
       gme_reservations_pvt.get_reservation_dtl_qty
         (p_reservation_rec    => p_rsrv_rec
         ,p_uom_code           => p_mtl_dtl_rec.dtl_um
         ,x_qty                => l_dtl_qty
         ,x_return_status      => x_return_status);
       
       IF  x_return_status <> FND_API.G_RET_STS_SUCCESS THEN
          IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
             gme_debug.put_line(g_pkg_name||'.'||l_api_name||' gme_reservations_pvt.get_reservation_dtl_qty returned error');
          END IF;
                
          RAISE error_get_dtl_qty;
       END IF;
       
       IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
          gme_debug.put_line(g_pkg_name||'.'||l_api_name||' get_reservation_dtl_qty returned l_dtl_qty = '||l_dtl_qty);
       END IF;
    ELSE
       l_prim_qty := p_att;
       l_sec_qty := p_satt;
       l_dtl_qty := NULL;
       
       l_primary_um := p_primary_uom_code;
       l_dtl_um := p_mtl_dtl_rec.dtl_um;

       -- Bug 7709971 - Introduce this block to derive the values differently.
       IF (p_called_by = 'REL2') THEN  
       
          -- If we are here this means that p_att is in the dtl_uom which could be secondary.
          -- Note: p_att and p_satt will be the same value when detail line is in secondary uom.
          l_dtl_qty := p_att;

          -- Let's derive secondary qty's if necessary.          
          IF (p_secondary_uom_code IS NOT NULL) THEN
             -- If secondary qty is passed in then it means that user is working in secondary qty on the batch.          
             IF (p_satt IS NULL) THEN

                -- Bug 12813284 - Initialize variables for error message.
                l_from_um := l_dtl_um;
                l_to_um := p_secondary_uom_code;
             
                -- We need to derive secondary from the dtl qty
                l_sec_qty := inv_convert.inv_um_convert
                      (item_id              => p_mtl_dtl_rec.inventory_item_id
                      ,precision            => gme_common_pvt.g_precision
                      ,from_quantity        => l_dtl_qty
                      ,from_unit            => l_dtl_um
                      ,to_unit              => p_secondary_uom_code
                      ,from_name            => NULL
                      ,to_name              => NULL);

                -- Let's see if conversion went wrong.
                IF (NVL(l_sec_qty, 0) = -99999) THEN
                   IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
                      gme_debug.put_line(g_pkg_name||'.'||l_api_name||' REL2 - PRIM TO SEC - inv_convert.inv_um_convert returned error');
                   END IF;
                   RAISE um_convert_error;
                END IF;                      
             ELSE
                -- Bug 12813284 - Initialize variables for error message.
                l_from_um := p_secondary_uom_code;
                l_to_um := l_dtl_um;

                -- This means that we need to derive the dtl qty from the secondary qty. 
                l_dtl_qty := inv_convert.inv_um_convert
                      (item_id              => p_mtl_dtl_rec.inventory_item_id
                      ,precision            => gme_common_pvt.g_precision
                      ,from_quantity        => l_sec_qty
                      ,from_unit            => p_secondary_uom_code
                      ,to_unit              => l_dtl_um
                      ,from_name            => NULL
                      ,to_name              => NULL);

                -- Let's see if conversion went wrong.
                IF (l_dtl_qty = -99999) THEN
                   IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
                      gme_debug.put_line(g_pkg_name||'.'||l_api_name||' REL2 - SEC TO PRIM - inv_convert.inv_um_convert returned error');
                   END IF;
                   RAISE um_convert_error;
                END IF;                                            
             END IF;
          END IF;
          
          IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
             gme_debug.put_line(g_pkg_name||'.'||l_api_name||' after call to inv_convert.inv_um_convert');
             gme_debug.put_line(g_pkg_name||'.'||l_api_name||' l_dtl_qty = '||to_char(l_dtl_qty));
             gme_debug.put_line(g_pkg_name||'.'||l_api_name||' l_sec_qty = '||to_char(l_sec_qty));
          END IF;
          -- We now have in hand both the dtl and secondary qty in the correct UOM.
          
          l_prim_qty := l_dtl_qty;
          IF (l_primary_um <> l_dtl_um) THEN      
             l_prim_qty := NULL;
          END IF;
       END IF;  -- p_called_by = 'REL2'
       
       -- Here we always have the secondary qty in the correct UOM if it is dual controlled.
       -- Also, we have either the primary qty or detail qty.
       -- We may have both if it came via REL2 code and primary and dtl uom are the same.
       
       IF (l_prim_qty IS NULL) THEN       
          -- Bug 7709971 - Do not do conversion unnecessarily.
          -- If the primary is NOT the same as the dtl uom then we are trying to calculate
          -- the primary qty since we already have the detail qty.
          l_prim_qty := l_dtl_qty;
          IF (l_primary_um <> l_dtl_um) THEN
          
             -- Bug 12813284 - Initialize variables for error message.
             l_from_um := l_dtl_um;
             l_to_um := l_primary_um;
              
             -- Bug 8741777 changed assignment from  l_dtl_qty to l_prim_qty  
             -- as it is conversion to primary qty
             l_prim_qty := inv_convert.inv_um_convert
                   (item_id              => p_mtl_dtl_rec.inventory_item_id
                   ,precision            => gme_common_pvt.g_precision
                   ,from_quantity        => l_dtl_qty
                   ,from_unit            => l_dtl_um
                   ,to_unit              => l_primary_um
                   ,from_name            => NULL
                   ,to_name              => NULL);
                   
             IF l_prim_qty = -99999 THEN
                IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
                   gme_debug.put_line(g_pkg_name||'.'||l_api_name||' DTL TO PRIM - inv_convert.inv_um_convert returned error');
                END IF;
                RAISE um_convert_error;
             END IF;
             
             IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
                gme_debug.put_line(g_pkg_name||'.'||l_api_name||' DTL TO PRIM - after call to inv_convert.inv_um_convert');
                gme_debug.put_line(g_pkg_name||'.'||l_api_name||' l_prim_qty = '||to_char(l_prim_qty));
             END IF;
          END IF;       
       END IF;       

       IF (l_dtl_qty IS NULL) THEN       
          -- If the primary is NOT the same as the dtl uom then we are trying to calculate
          -- the detail qty since we already have the primary qty.
          l_dtl_qty := l_prim_qty;
          IF (l_primary_um <> l_dtl_um) THEN
          
             -- Bug 12813284 - Initialize variables for error message.
             l_from_um := l_primary_um;
             l_to_um := l_dtl_um;
                
             l_dtl_qty := inv_convert.inv_um_convert
                   (item_id              => p_mtl_dtl_rec.inventory_item_id
                   ,precision            => gme_common_pvt.g_precision
                   ,from_quantity        => l_prim_qty
                   ,from_unit            => l_primary_um
                   ,to_unit              => l_dtl_um
                   ,from_name            => NULL
                   ,to_name              => NULL);
                   
             IF l_dtl_qty = -99999 THEN
                IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
                   gme_debug.put_line(g_pkg_name||'.'||l_api_name||' PRIM TO DTL - inv_convert.inv_um_convert returned error');
                END IF;
                RAISE um_convert_error;
             END IF;
             
             IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
                gme_debug.put_line(g_pkg_name||'.'||l_api_name||' after call to inv_convert.inv_um_convert');
                gme_debug.put_line(g_pkg_name||'.'||l_api_name||' l_dtl_qty = '||to_char(l_dtl_qty));
             END IF;
          END IF;       
       END IF;       
    END IF;

    IF p_rsrv_rec.lot_number IS NOT NULL AND
       NVL(p_lot_divisible_flag,'Y') = 'N' THEN
       l_whole_qty := TRUE;
       IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
          gme_debug.put_line(g_pkg_name||'.'||l_api_name||' l_whole_qty = TRUE because lot indivisible item');
       END IF;
    ELSE
       l_whole_qty := FALSE;
       IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
          gme_debug.put_line(g_pkg_name||'.'||l_api_name||' l_whole_qty = FALSE');
       END IF;
    END IF;
    
    -- test again for dispensed items
    IF NOT l_whole_qty THEN
       IF NVL(p_dispense_ind,'N') = 'Y' THEN
         l_whole_qty := TRUE;
         IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
            gme_debug.put_line(g_pkg_name||'.'||l_api_name||' l_whole_qty = TRUE because dispensed item');
         END IF;
       END IF;
    END IF;

/* Original code
    IF l_dtl_qty <= p_consume_qty - x_actual_qty OR l_whole_qty THEN
      IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' l_trxn_qty := l_dtl_qty');
      END IF;
      l_trxn_qty := l_dtl_qty;
    ELSE
      IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' l_trxn_qty := p_consume_qty - x_actual_qty');
      END IF;
      l_trxn_qty := p_consume_qty - x_actual_qty;
      l_prim_qty := NULL;
      l_sec_qty := NULL;
    END IF;
*/

    -- Bug 6778968 - Restructured code to derive l_trxn_qty. This is the qty passed into the INV api
    -- to relieve the reservation and convert into a transaction. Also, this code is now usable not
    -- only for release batch but also the convert detail rservation api.
    
    -- Default trxn_qty to the most likely value which is from release batch flow.
    -- For IB p_consume_qty is the new actual and x_actual_qty is the old actual. Used to derive delta.
    -- For IB l_dtl_qty is the reservation quantity.
    l_trxn_qty := p_consume_qty - x_actual_qty;
    IF (p_called_by = 'CVT') THEN
       -- If being called from convert detail reservation api, reset the value to qty passed in.
       l_trxn_qty := p_consume_qty;    
    END IF;

    -- Bug 22567974 - let's set some of the variables for rounding and precision equivalency checks.
    l_precision := gme_common_pvt.g_precision;
    IF (p_called_by = 'IB' AND l_rsrv_mode) THEN
       l_precision := length(p_consume_qty - trunc(p_consume_qty));
    END IF;    
    IF (NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' l_precision = '||l_precision);
    END IF;
    
    -- Default fraction variables to the most common scenario.         
    l_numerator := l_dtl_qty;
    l_denominator := l_trxn_qty;
              
    -- Change Default fraction variables to make denominator the larger quantity.         
    IF l_numerator > l_denominator THEN
       l_numerator := l_trxn_qty;
       l_denominator := l_dtl_qty;
    END IF;

    IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
       gme_debug.put_line (g_pkg_name||'.'||l_api_name||' l_numerator FOR precision check rounding issue is '||l_numerator);
       gme_debug.put_line (g_pkg_name||'.'||l_api_name||' l_denominator FOR precision check rounding issue is '||l_denominator);
    END IF;
    
    -- If the reservation qty (l_dtl_qty) has less than what is being requested, consume all of it.
    -- Bug 22567974 - For IB additional check for precision.
    IF l_dtl_qty <= l_trxn_qty OR l_whole_qty OR
       (p_called_by = 'IB' AND l_rsrv_mode AND        
        round(l_numerator/l_denominator, l_precision) >= p_tolerance AND   -- Bug 22317010 / 22764488
        round(l_numerator/l_denominator, l_precision) <= 1) THEN
        -- round(l_dtl_qty/l_trxn_qty, l_precision) >= .9997 AND
        -- round(l_dtl_qty/l_trxn_qty, l_precision) <= 1) THEN
       -- l_trxn_qty := l_dtl_qty;

       -- Bug 22567974 - For IB do more precision validations.
       --                If the precision is this close/equal then keep full trxn since it is most likely a rounding issue.
       IF (p_called_by = 'IB' AND l_rsrv_mode) THEN
          IF round(l_numerator/l_denominator, l_precision) >= p_tolerance AND   -- Bug 22317010 / 22764488
             round(l_numerator/l_denominator, l_precision) <= 1 THEN
             NULL;         
             IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
                gme_debug.put_line(g_pkg_name||'.'||l_api_name||' Rounding or equal so leave l_trxn_qty = p_consume_qty - x_actual_qty');
             END IF;
          ELSE
             l_trxn_qty := l_dtl_qty;
             IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
                gme_debug.put_line(g_pkg_name||'.'||l_api_name||' IB reset l_trxn_qty = l_dtl_qty');
             END IF;
          END IF;                   
                          
          -- Bug 22217179 - initialize whole quantity variable.
          -- Primary and secondary are already set above for reservations.
          l_whole_qty := TRUE;
          IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
             gme_debug.put_line(g_pkg_name||'.'||l_api_name||' Setting l_whole_qty to TRUE for IB.');
          END IF;
          
          -- Bug 24350177 - Let's check precision in case there is a cumulative rounding issue with primary.          
          IF p_mtl_dtl_rec.dtl_um <> p_rsrv_rec.primary_uom_code THEN
             IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
                gme_debug.put_line(' Lets check ATT when consuming full reservation just to make sure. ');
                gme_debug.put_line(' ATT and QOH includes whats already reserved for this line.');
             END IF; 
             
             -- Bug 24350177 - Lets check to make sure there is enough on hand in primary to avoid erroneous error.
             gme_transactions_pvt.query_quantities
             (
                p_api_version_number   	   => 1
               ,p_init_msg_lst         	   => fnd_api.g_false
               ,x_return_status        	   => l_return_status
               ,x_msg_count            	   => l_msg_count
               ,x_msg_data             	   => l_msg_data
               ,p_organization_id            => p_mtl_dtl_rec.organization_id
               ,p_inventory_item_id          => p_mtl_dtl_rec.inventory_item_id
               ,p_tree_mode                  => inv_quantity_tree_pub.g_transaction_mode
               -- ,p_is_serial_control          => FALSE
               ,p_grade_code                 => NULL
               ,p_demand_source_type_id      => gme_common_pvt.g_txn_source_type
               ,p_demand_source_header_id    => p_mtl_dtl_rec.batch_id
               ,p_demand_source_line_id      => p_mtl_dtl_rec.material_detail_id
               -- ,p_demand_source_name         => NULL
               -- ,p_lot_expiration_date        => NULL
               ,p_revision             	   => p_revision
               ,p_lot_number           	   => p_rsrv_rec.lot_number
               ,p_subinventory_code    	   => p_rsrv_rec.subinventory_code
               ,p_locator_id           	   => p_rsrv_rec.locator_id
               -- ,p_onhand_source		         => inv_quantity_tree_pvt.g_all_subs
               ,x_qoh                  	   => l_qoh
               ,x_rqoh                 	   => l_rqoh
               ,x_qr                   	   => l_qr
               ,x_qs                   	   => l_qs
               ,x_att                  	   => l_att
               ,x_atr                  	   => l_atr
               ,x_sqoh                  	   => l_sqoh
               ,x_srqoh                 	   => l_srqoh
               ,x_sqr                   	   => l_sqr
               ,x_sqs                   	   => l_sqs
               ,x_satt                  	   => l_satt
               ,x_satr                  	   => l_satr
             );
             
             IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
                gme_debug.put_line(g_pkg_name||'.'||l_api_name||' after query quantities return status = '||l_return_status);
                gme_debug.put_line(g_pkg_name||'.'||l_api_name||' after query quantities att = '||l_att);
                gme_debug.put_line(g_pkg_name||'.'||l_api_name||' after query quantities atr = '||l_atr);
                gme_debug.put_line(g_pkg_name||'.'||l_api_name||' after query quantities qoh = '||l_qoh);
                gme_debug.put_line(g_pkg_name||'.'||l_api_name||' after query quantities item_id = '||p_mtl_dtl_rec.inventory_item_id);
                gme_debug.put_line(g_pkg_name||'.'||l_api_name||' after query quantities sub = '||p_subinv);
                gme_debug.put_line(g_pkg_name||'.'||l_api_name||' after query quantities loc = '||p_locator_id);
                gme_debug.put_line(g_pkg_name||'.'||l_api_name||' after query quantities rev = '||p_revision);
             END IF;          
          
             -- If it falls into this condition, then it means they are essentially equal.
             IF l_prim_qty > l_att THEN
                IF round(l_att/l_prim_qty, l_precision) >= p_tolerance AND
                   round(l_att/l_prim_qty, l_precision) <= 1 THEN
                   IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
                      gme_debug.put_line(g_pkg_name||'.'||l_api_name||' override primary_qty '||l_prim_qty||' with : '||l_att);
                   END IF;
                   l_prim_qty := l_att;                                   
                END IF;
             END IF;
          END IF;
       ELSE
          l_trxn_qty := l_dtl_qty;
          IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
             gme_debug.put_line(g_pkg_name||'.'||l_api_name||' l_trxn_qty = l_dtl_qty');
          END IF;
       END IF;
    ELSE
       -- Transaction qty is set above. Just set other two variables.
       IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
          IF (p_called_by = 'CVT') THEN
             gme_debug.put_line(g_pkg_name||'.'||l_api_name||' l_trxn_qty = p_consume_qty');
          ELSE
             gme_debug.put_line(g_pkg_name||'.'||l_api_name||' l_trxn_qty = p_consume_qty - x_actual_qty');         
          END IF;
       END IF;       
       
       l_prim_qty := NULL;          
       l_sec_qty := NULL; 
                
       -- Bug 22217179 - initialize corresponding primary or secondary if uom matches.
       IF (p_called_by = 'IB' AND l_rsrv_mode) THEN
          IF p_mtl_dtl_rec.dtl_um = p_rsrv_rec.primary_uom_code THEN
             l_prim_qty := l_trxn_qty;        
          END IF;

          IF p_mtl_dtl_rec.dtl_um = NVL(p_secondary_uom_code,'NOT DUAL') THEN
             l_sec_qty := l_trxn_qty;        
          END IF;           
       END IF;
    END IF;
    
    l_transaction_rec.transaction_type_id := gme_common_pvt.g_ing_issue;
    l_transaction_rec.transaction_date := p_trans_date;
    l_transaction_rec.transaction_quantity := l_trxn_qty;
    l_transaction_rec.secondary_uom_code := p_secondary_uom_code;
    
    IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' transaction_date = '||to_char(l_transaction_rec.transaction_date
                                                                         ,'YYYY-MON-DD HH24:MI:SS'));    
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' transaction_qty = '||l_transaction_rec.transaction_quantity);
    END IF;

    IF l_prim_qty IS NOT NULL THEN
       l_transaction_rec.primary_quantity := l_prim_qty;
       IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
          gme_debug.put_line(g_pkg_name||'.'||l_api_name||' primary_qty is known: '||l_transaction_rec.primary_quantity);
       END IF;
    END IF;
    
    IF l_sec_qty IS NOT NULL THEN 
       l_transaction_rec.secondary_transaction_quantity := l_sec_qty;
       IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
          gme_debug.put_line(g_pkg_name||'.'||l_api_name||' secondary_transaction_quantity is known: '||l_transaction_rec.secondary_transaction_quantity);
       END IF;
    END IF;

    l_transaction_rec.transaction_uom := p_mtl_dtl_rec.dtl_um;
    
    -- if the item is dual, this should be passed in, if not dual, this should be NULL
    l_transaction_rec.secondary_uom_code := p_secondary_uom_code;

    IF l_lot_rec.count > 0 THEN    
       IF l_lot_rec(1).lot_number IS NOT NULL THEN
          l_lot_rec(1).transaction_quantity := l_transaction_rec.transaction_quantity;
          IF l_prim_qty IS NOT NULL THEN
             l_lot_rec(1).primary_quantity := l_prim_qty;
          END IF;
          
          IF l_sec_qty IS NOT NULL THEN 
             l_lot_rec(1).secondary_transaction_quantity := l_sec_qty;
          END IF;
       END IF;
    END IF;

    gme_transactions_pvt.create_material_txn
                        (p_mmti_rec             => l_transaction_rec
                        ,p_mmli_tbl             => l_lot_rec
                        ,x_return_status        => x_return_status);
                        
    IF x_return_status <> FND_API.G_RET_STS_SUCCESS THEN
       IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
          gme_debug.put_line(g_pkg_name||'.'||l_api_name||' gme_transactions_pvt.create_transaction returned '||x_return_status);
       END IF;
       RAISE error_create_trxn;
    END IF;

    x_actual_qty := x_actual_qty + l_trxn_qty;
    
    IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' x_actual_qty = '||x_actual_qty);
    END IF;

    IF l_rsrv_mode THEN
       IF l_prim_qty IS NULL THEN
          -- need to consider lot conversion as well
          l_from_um := p_mtl_dtl_rec.dtl_um;
          l_to_um := p_rsrv_rec.primary_uom_code;
          l_prim_qty := inv_convert.inv_um_convert
              (item_id              => p_mtl_dtl_rec.inventory_item_id
              ,precision            => gme_common_pvt.g_precision
              ,from_quantity        => l_trxn_qty
              ,from_unit            => l_from_um
              ,to_unit              => l_to_um
              ,from_name            => NULL
              ,to_name              => NULL);
              
          IF l_prim_qty = -99999 THEN
             IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
                gme_debug.put_line(g_pkg_name||'.'||l_api_name||' rsrv_mode - DTL to PRIM - inv_convert.inv_um_convert returned error');
             END IF;
             RAISE um_convert_error;
          END IF;
          
          IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
             gme_debug.put_line(g_pkg_name||'.'||l_api_name||' calculated prim qty for call to gme_reservations_pvt.relieve_reservation: l_prim_qty = '||to_char(l_prim_qty));
          END IF;
       END IF;

       -- Bug 22217179 - Initialize relieve all if we are consuming full reservation.
       l_relieve_all := fnd_api.g_false;
       IF l_whole_qty THEN
          l_relieve_all := fnd_api.g_true;
          l_prim_qty := p_rsrv_rec.primary_reservation_quantity;  -- Bug 24350177 - Initialize primary in case it was overwritten by att value.       
       END IF;
       
       IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
          gme_debug.put_line(g_pkg_name||'.'||l_api_name||' l_relieve_all = '||l_relieve_all);
       END IF;
       
       -- Bug 22217179 - Pass in additional parameter values.
       gme_reservations_pvt.relieve_reservation
                                (p_reservation_id              => p_rsrv_rec.reservation_id
                                ,p_prim_relieve_qty            => l_prim_qty
                                ,p_secondary_relieved_quantity => l_sec_qty
                                ,p_relieve_all                 => l_relieve_all
                                ,x_return_status               => x_return_status);
       
       IF x_return_status <> FND_API.G_RET_STS_SUCCESS THEN
          RAISE error_relieve_rsrv;
       END IF;
      
       -- Bug 4899399 - after relieving the reservation, informing the GMO about the transaction.
       IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
          gme_debug.put_line(g_pkg_name||'.'||l_api_name||' dispense_ind = '||p_mtl_dtl_rec.dispense_ind);
          gme_debug.put_line(g_pkg_name||'.'||l_api_name||' trans type_id = '||l_transaction_rec.transaction_type_id);
          gme_debug.put_line(g_pkg_name||'.'||l_api_name||' ext sour line_id = '||p_rsrv_rec.external_source_line_id);   
       END IF;
       
       IF NVL(p_dispense_ind,'N') = 'Y' THEN
          IF l_transaction_rec.transaction_type_id = gme_common_pvt.g_ing_issue THEN 
     	 	   	-- For consume 
     	 	    GMO_DISPENSE_GRP.CHANGE_DISPENSE_STATUS
     	 	   (p_api_version    	=> 1.0,
     	 	    p_init_msg_list 	   => 'F',
     	 	    p_commit	 	      => 'F',
     	 	    x_return_status 	   => l_return_status,
     	 	    x_msg_count 	      => l_msg_count,
     	 	    x_msg_data  	      => l_msg_data,
     	 	    p_dispense_id    	=> p_rsrv_rec.external_source_line_id,
     	 	    p_status_code    	=> 'CNSUMED',
     	 	    p_transaction_id 	=> null
     	 	    ) ;
     	 	    IF l_return_status <> fnd_api.g_ret_sts_success THEN
     	 	       RAISE dispense_error;
     	 	    END IF;
          END IF;
       END IF; 
    END IF;
    
    IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
       gme_debug.put_line('Exiting api '||g_pkg_name||'.'||l_api_name);
    END IF;

  EXCEPTION
  WHEN um_convert_error THEN
    OPEN item_no_cursor(p_mtl_dtl_rec.inventory_item_id, p_mtl_dtl_rec.organization_id);
    FETCH item_no_cursor INTO l_item_no;
    CLOSE item_no_cursor;
    
    fnd_message.set_name  ('GMI', 'IC_API_UOM_CONVERSION_ERROR');
    fnd_message.set_token ('ITEM_NO', l_item_no);
    fnd_message.set_token ('FROM_UOM',l_from_um);
    fnd_message.set_token ('TO_UOM', l_to_um);
    fnd_msg_pub.ADD;
    x_return_status := FND_API.g_ret_sts_error;
  WHEN error_create_trxn OR error_build_mmti OR error_get_dtl_qty OR error_relieve_rsrv THEN
    NULL;
  WHEN dispense_error THEN
       x_return_status := fnd_api.g_ret_sts_error;	
  WHEN OTHERS THEN
    fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
    IF (NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT) THEN
      gme_debug.put_line('Unexpected error: '||g_pkg_name||'.'||l_api_name||': '||SQLERRM);
    END IF;
    x_return_status := FND_API.g_ret_sts_unexp_error;
  END build_and_create_transaction;     

  PROCEDURE  constr_mmti_from_reservation
    (p_rsrv_rec              IN   mtl_reservations%ROWTYPE
    ,x_mmti_rec              OUT  NOCOPY mtl_transactions_interface%ROWTYPE
    ,x_mmli_tbl              OUT  NOCOPY gme_common_pvt.mtl_trans_lots_inter_tbl
    ,x_return_status         OUT  NOCOPY VARCHAR2) IS
    
    l_api_name     CONSTANT VARCHAR2 (30)      := 'CONSTR_MMTI_FROM_RESERVATION';
  BEGIN
  
    IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
      gme_debug.put_line('Entering api '||g_pkg_name||'.'||l_api_name);
      gme_debug.put_line(g_pkg_name||'.'||l_api_name||' reservation_id = '||p_rsrv_rec.reservation_id);
    END IF;

    /* Initially let us assign the return status to success */
    x_return_status := FND_API.g_ret_sts_success;

    -- consturct mtl_transactions_interface
    x_mmti_rec.transaction_source_id        := p_rsrv_rec.demand_source_header_id;  -- batch_id
    x_mmti_rec.trx_source_line_id           := p_rsrv_rec.demand_source_line_id;  -- material_detail_id
    x_mmti_rec.inventory_item_id            := p_rsrv_rec.inventory_item_id;
    x_mmti_rec.organization_id              := p_rsrv_rec.organization_id;
    x_mmti_rec.subinventory_code            := p_rsrv_rec.subinventory_code;
    x_mmti_rec.locator_id                   := p_rsrv_rec.locator_id;
    x_mmti_rec.revision                     := p_rsrv_rec.revision;
    
    x_mmti_rec.transaction_sequence_id      := p_rsrv_rec.reservation_id;

     -- channges for GMO
    x_mmti_rec.transaction_reference 	    := p_rsrv_rec.external_source_line_id ;
    -- construct mtl_transaction_lots_interface
    IF p_rsrv_rec.lot_number IS NOT NULL THEN
      x_mmli_tbl(1).lot_number                   := p_rsrv_rec.lot_number;
    END IF;
    -- Bug 6437252 LPN Support 
    IF p_rsrv_rec.lpn_id IS NOT NULL THEN
      x_mmti_rec.lpn_id                   := p_rsrv_rec.lpn_id;
    END IF;
    IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
      gme_debug.put_line('Exiting api '||g_pkg_name||'.'||l_api_name);
    END IF;


  EXCEPTION
    WHEN FND_API.G_EXC_ERROR THEN
      x_return_status := FND_API.G_RET_STS_ERROR;

    WHEN FND_API.G_EXC_UNEXPECTED_ERROR THEN
      x_return_status := FND_API.G_RET_STS_UNEXP_ERROR;

    WHEN OTHERS THEN
      x_return_status := FND_API.G_RET_STS_UNEXP_ERROR;
      FND_MSG_PUB.Add_Exc_Msg (  G_PKG_NAME , l_api_name);

  END constr_mmti_from_reservation;

  PROCEDURE constr_mmti_from_qty_tree
        (p_mtl_dtl_rec            IN gme_material_details%ROWTYPE
        ,p_subinv                 IN VARCHAR2
        ,p_locator_id             IN NUMBER
        ,x_mmti_rec               OUT  NOCOPY mtl_transactions_interface%ROWTYPE
        ,x_return_status          OUT  NOCOPY VARCHAR2) IS
    
    l_api_name     CONSTANT VARCHAR2 (30)      := 'CONSTR_MMTI_FROM_QTY_TREE';
  BEGIN
  
    IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
      gme_debug.put_line('Entering api '||g_pkg_name||'.'||l_api_name);
      gme_debug.put_line(g_pkg_name||'.'||l_api_name||' material_detail_id='||p_mtl_dtl_rec.material_detail_id);
      gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_subinv='||p_subinv);
      gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_locator_id='||p_locator_id);
    END IF;

    /* Initially let us assign the return status to success */
    x_return_status := FND_API.g_ret_sts_success;

    -- consturct mtl_transactions_interface
    x_mmti_rec.transaction_source_id        := p_mtl_dtl_rec.batch_id;
    x_mmti_rec.trx_source_line_id           := p_mtl_dtl_rec.material_detail_id;
    x_mmti_rec.inventory_item_id            := p_mtl_dtl_rec.inventory_item_id;
    x_mmti_rec.organization_id              := p_mtl_dtl_rec.organization_id;
    x_mmti_rec.subinventory_code            := p_subinv;
    x_mmti_rec.locator_id                   := p_locator_id;

    IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
      gme_debug.put_line('Exiting api '||g_pkg_name||'.'||l_api_name);
    END IF;

  EXCEPTION
    WHEN FND_API.G_EXC_ERROR THEN
      x_return_status := FND_API.G_RET_STS_ERROR;

    WHEN FND_API.G_EXC_UNEXPECTED_ERROR THEN
      x_return_status := FND_API.G_RET_STS_UNEXP_ERROR;

    WHEN OTHERS THEN
      x_return_status := FND_API.G_RET_STS_UNEXP_ERROR;
      FND_MSG_PUB.Add_Exc_Msg (  G_PKG_NAME , l_api_name);

  END constr_mmti_from_qty_tree;
  
  PROCEDURE create_batch_exception
              (p_material_dtl_rec         IN gme_material_details%ROWTYPE
              ,p_pending_move_order_ind   IN BOOLEAN  := NULL
              ,p_pending_rsrv_ind         IN BOOLEAN  := NULL
              ,p_transacted_qty           IN NUMBER   := NULL
              ,p_exception_qty            IN NUMBER   := NULL
              ,p_force_unconsumed         IN VARCHAR2 := fnd_api.g_true
              ,x_exception_material_tbl   IN OUT NOCOPY gme_common_pvt.exceptions_tab
              ,x_return_status            OUT NOCOPY VARCHAR2) IS

    l_api_name        CONSTANT VARCHAR2 (30)   := 'create_batch_exception';

    i                          NUMBER;    
    l_pending_mo_ind           BOOLEAN;
    l_pending_rsrv_ind         BOOLEAN;
    l_display_unconsumed       VARCHAR2(1);
    l_exceptions_rec           gme_exceptions_gtmp%ROWTYPE;
    
    error_insert_exceptions    EXCEPTION;
    error_no_exception         EXCEPTION;
    
  BEGIN
    IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
       gme_debug.put_line('Entering api '||g_pkg_name||'.'||l_api_name);
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' material_detail_id = '||p_material_dtl_rec.material_detail_id);
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' line_no = '||p_material_dtl_rec.line_no);
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' line_typ = '||p_material_dtl_rec.line_type);
       IF p_material_dtl_rec.phantom_line_id IS NOT NULL THEN
          gme_debug.put_line(g_pkg_name||'.'||l_api_name||' this is a PHANTOM');
       END IF;
       
       IF p_pending_move_order_ind IS NULL THEN
          gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_pending_move_order_ind IS NULL');
       ELSIF p_pending_move_order_ind THEN
          gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_pending_move_order_ind = TRUE');
       ELSE
          gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_pending_move_order_ind = FALSE');
       END IF;
       
       IF p_pending_rsrv_ind IS NULL THEN
          gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_pending_rsrv_ind IS NULL');
       ELSIF p_pending_rsrv_ind THEN
          gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_pending_rsrv_ind = TRUE');
       ELSE
          gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_pending_rsrv_ind = FALSE');
       END IF;
       
       IF p_transacted_qty IS NULL THEN
          gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_transacted_qty IS NULL');
       ELSE
          gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_transacted_qty = '||p_transacted_qty);
       END IF;
       
       IF p_exception_qty IS NULL THEN
          gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_exception_qty IS NULL');
       ELSE
          gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_exception_qty = '||p_exception_qty);
       END IF;
       
       IF p_force_unconsumed IS NULL THEN
          gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_force_unconsumed IS NULL');
       ELSE
          gme_debug.put_line(g_pkg_name||'.'||l_api_name||' p_force_unconsumed = '||p_force_unconsumed);
       END IF;      
    END IF;

    x_return_status    := FND_API.G_RET_STS_SUCCESS;

    IF p_force_unconsumed = fnd_api.g_true THEN
       l_display_unconsumed := fnd_api.g_true;
    ELSIF p_force_unconsumed = fnd_api.g_false THEN
       IF gme_common_pvt.g_display_unconsumed_material = 1 THEN
          l_display_unconsumed := fnd_api.g_true;
       ELSE
          l_display_unconsumed := fnd_api.g_false;
       END IF;
    END IF;

    IF p_material_dtl_rec.line_type = gme_common_pvt.g_line_type_ing AND p_material_dtl_rec.phantom_line_id IS NOT NULL THEN
       -- don't report the phantom ingredients; just return; phantom products will be reported
       IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
          gme_debug.put_line(g_pkg_name||'.'||l_api_name||' called for phantom ingredient... returning, only report for phantom product');
       END IF;
       RAISE error_no_exception;
    END IF;

    IF p_pending_move_order_ind IS NULL AND p_material_dtl_rec.line_type = gme_common_pvt.g_line_type_ing THEN
       l_pending_mo_ind := gme_move_orders_pvt.pending_move_orders_exist
                                (p_organization_id         => p_material_dtl_rec.organization_id
                                ,p_batch_id                => p_material_dtl_rec.batch_id
                                ,p_material_detail_id      => p_material_dtl_rec.material_detail_id);
    ELSE
       l_pending_mo_ind := NVL(p_pending_move_order_ind, FALSE);
    END IF;

    IF p_pending_rsrv_ind IS NULL THEN
       IF p_material_dtl_rec.line_type = gme_common_pvt.g_line_type_ing THEN
          l_pending_rsrv_ind:= gme_reservations_pvt.pending_reservations_exist
                                  (p_organization_id         => p_material_dtl_rec.organization_id
                                  ,p_batch_id                => p_material_dtl_rec.batch_id
                                  ,p_material_detail_id      => p_material_dtl_rec.material_detail_id);
       ELSE
          l_pending_rsrv_ind:= gme_pending_product_lots_pvt.pending_product_lot_exist
                                  (p_batch_id                => p_material_dtl_rec.batch_id
                                  ,p_material_detail_id      => p_material_dtl_rec.material_detail_id);
       END IF;
    ELSE
       l_pending_rsrv_ind := p_pending_rsrv_ind;
    END IF;

    l_exceptions_rec.organization_id             := p_material_dtl_rec.organization_id;

    IF l_pending_mo_ind THEN
       l_exceptions_rec.pending_move_order_ind    := 1;
    ELSE
       l_exceptions_rec.pending_move_order_ind    := 0;
    END IF;

    IF l_pending_rsrv_ind THEN
       l_exceptions_rec.pending_reservations_ind  := 1;
    ELSE
       l_exceptions_rec.pending_reservations_ind  := 0;
    END IF;

    l_exceptions_rec.material_detail_id          := p_material_dtl_rec.material_detail_id;
    l_exceptions_rec.batch_id                    := p_material_dtl_rec.batch_id;
    l_exceptions_rec.transacted_qty              := NVL(p_transacted_qty, p_material_dtl_rec.actual_qty);
    l_exceptions_rec.exception_qty               := NVL(p_exception_qty, p_material_dtl_rec.plan_qty - p_material_dtl_rec.actual_qty);
    l_exceptions_rec.exception_qty               := ROUND(l_exceptions_rec.exception_qty, gme_common_pvt.g_precision);

    IF l_pending_rsrv_ind OR l_pending_mo_ind OR
       (l_display_unconsumed = FND_API.g_true AND l_exceptions_rec.exception_qty > 0) OR
       -- next line is for negative IB
       (l_display_unconsumed = FND_API.g_true AND p_exception_qty < 0) THEN
       i := x_exception_material_tbl.COUNT + 1;
       x_exception_material_tbl(i) := l_exceptions_rec;
       
       IF NOT gme_common_pvt.insert_exceptions(p_exception_rec    => l_exceptions_rec) THEN
          RAISE error_insert_exceptions;
       END IF;
       
       x_return_status := gme_common_pvt.g_exceptions_err;
    ELSE
       IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
          gme_debug.put_line(g_pkg_name||'.'||l_api_name||' exception not found');
       END IF;
    END IF;

    IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
       gme_debug.put_line('Exiting api '||g_pkg_name||'.'||l_api_name||' with return status= '||x_return_status);
    END IF;
  EXCEPTION
    WHEN error_insert_exceptions THEN
      x_return_status := FND_API.G_RET_STS_ERROR;
    WHEN error_no_exception THEN
      NULL;
    WHEN OTHERS THEN
      IF nvl(g_debug, gme_debug.g_log_unexpected + 1) <= gme_debug.g_log_unexpected THEN
        gme_debug.put_line('When others exception in '||g_pkg_name||'.'||l_api_name||' Error is ' || SQLERRM);
      END IF;    		
      fnd_msg_pub.add_exc_msg(g_pkg_name, l_api_name);
      x_return_status := FND_API.G_RET_STS_UNEXP_ERROR;
  END create_batch_exception;
  
  /*************************************************************************************/
  /* p_auto_by_step     Include auto by step                                           */
  /*                    0 = check for all but auto by step                             */
  /*                    1 = check only auto by step                                    */
  /*                    2 = check all release types                                    */
  /* p_batchstep_id     used when p_auto_by_step is passed as value = 1                */
  /*************************************************************************************/
  PROCEDURE check_unexploded_phantom(p_batch_id              IN  NUMBER
                                    ,p_auto_by_step          IN  NUMBER
                                    ,p_batchstep_id          IN  NUMBER
                                    ,x_return_status         OUT NOCOPY VARCHAR2) IS
    
    CURSOR cur_get_phantom_ingred(v_batch_id NUMBER) IS
    SELECT *
    FROM   gme_material_details
    WHERE  batch_id = v_batch_id
    AND    line_type = gme_common_pvt.g_line_type_ing
    AND    phantom_type <> 0;
    
    CURSOR Cur_associated_step(v_matl_dtl_id NUMBER) IS
    SELECT s.batchstep_id
      FROM gme_batch_steps s, gme_batch_step_items item
     WHERE s.batchstep_id = item.batchstep_id
       AND item.material_detail_id = v_matl_dtl_id;
      
    l_api_name        CONSTANT VARCHAR2 (30)   := 'check_unexploded_phantom';
      
    l_step_id                NUMBER;

    l_matl_dtl_tab           gme_common_pvt.material_details_tab;
    
    l_matl_dtl_id            NUMBER;
    l_phantom_id             NUMBER;
    l_release_type           NUMBER;
    
    error_unexp_phantom    EXCEPTION;
    error_unexp_downstream EXCEPTION;
    
  BEGIN
    IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
      gme_debug.put_line('Entering api '||g_pkg_name||'.'||l_api_name);
      gme_debug.put_line(g_pkg_name||'.'||l_api_name||' batch_id = '||p_batch_id);
      gme_debug.put_line(g_pkg_name||'.'||l_api_name||' batchstep_id = '||p_batchstep_id);
      gme_debug.put_line(g_pkg_name||'.'||l_api_name||' mode = '||p_auto_by_step);
    END IF;

    x_return_status    := FND_API.G_RET_STS_SUCCESS;
    
    OPEN cur_get_phantom_ingred(p_batch_id);
    FETCH cur_get_phantom_ingred BULK COLLECT INTO l_matl_dtl_tab;
    CLOSE cur_get_phantom_ingred;

    FOR i in 1..l_matl_dtl_tab.COUNT LOOP
      l_matl_dtl_id     := l_matl_dtl_tab(i).material_detail_id;
      l_phantom_id      := l_matl_dtl_tab(i).phantom_id;
      l_release_type    := l_matl_dtl_tab(i).release_type;

      IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' phantom ingredient found: material_detail_id = '||l_matl_dtl_id);
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||' partner phantom batch id: batch_id = '||l_phantom_id);
      END IF;
      
      IF p_auto_by_step IN (0,1) AND l_release_type = gme_common_pvt.g_mtl_autobystep_release THEN
        OPEN Cur_associated_step(l_matl_dtl_id);
        FETCH Cur_associated_step INTO l_step_id;
        IF Cur_associated_step%NOTFOUND THEN
          l_release_type := gme_common_pvt.g_mtl_auto_release;
        END IF;
        CLOSE Cur_associated_step;
      END IF;

      IF ((p_auto_by_step = 0 AND l_release_type <> gme_common_pvt.g_mtl_autobystep_release) OR
          (p_auto_by_step = 1 AND
           l_step_id = p_batchstep_id AND
           l_release_type = gme_common_pvt.g_mtl_autobystep_release) OR
          (p_auto_by_step = 2)) THEN
        IF l_phantom_id IS NULL THEN
          IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
            gme_debug.put_line(g_pkg_name||'.'||l_api_name||' phantom ingredient unexploded: material_detail_id = '||l_matl_dtl_id);
          END IF;
          RAISE error_unexp_phantom;
        END IF;

        -- check that the phantom batch doesn't have any unexploded phantoms...
        -- check for all release types in phantom batch
        check_unexploded_phantom(p_batch_id      => l_phantom_id
                                ,p_auto_by_step  => 2
                                ,p_batchstep_id  => NULL
                                ,x_return_status => x_return_status);
                                
        IF x_return_status <> FND_API.G_RET_STS_SUCCESS THEN
          RAISE error_unexp_downstream;
        END IF;
      END IF;
    END LOOP;

    IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
      gme_debug.put_line('Exiting api '||g_pkg_name||'.'||l_api_name);
    END IF;
      
  EXCEPTION
    WHEN error_unexp_phantom THEN
      gme_common_pvt.log_message ('PM_UNEXPLODED_PHANTOMS');
      x_return_status := FND_API.G_RET_STS_ERROR;
    WHEN error_unexp_downstream THEN
      NULL;
    WHEN OTHERS THEN
      IF nvl(g_debug, gme_debug.g_log_unexpected + 1) <= gme_debug.g_log_unexpected THEN
        gme_debug.put_line('When others exception in '||g_pkg_name||'.'||l_api_name||' Error is ' || SQLERRM);
      END IF;    		
      fnd_msg_pub.add_exc_msg(g_pkg_name, l_api_name);
      x_return_status := FND_API.G_RET_STS_UNEXP_ERROR;
  END check_unexploded_phantom; 
  
  PROCEDURE validate_batch_for_release  (p_batch_header_rec     IN gme_batch_header%ROWTYPE
                                        ,x_batch_header_rec     OUT NOCOPY gme_batch_header%ROWTYPE
                                        ,x_return_status        OUT NOCOPY VARCHAR2) IS

      l_api_name   CONSTANT VARCHAR2 (30)           := 'validate_batch_for_release';

      l_batch_header_rec          gme_batch_header%ROWTYPE;

      CURSOR cur_validity_rule(v_recipe_validity_rule_id NUMBER)
      IS
         SELECT *
          FROM gmd_recipe_validity_rules
          WHERE recipe_validity_rule_id = v_recipe_validity_rule_id;

      CURSOR cur_validity_status_type(v_validity_rule_status VARCHAR2)
      IS
         SELECT status_type
          FROM gmd_status
          WHERE status_code=v_validity_rule_status; 

      l_validity_rule             gmd_recipe_validity_rules%ROWTYPE;
      l_status_type               GMD_STATUS.status_type%TYPE; 

      error_batch_type            EXCEPTION;
      error_batch_status          EXCEPTION;
      error_phantom               EXCEPTION;
      error_future_date           EXCEPTION;
      error_vr_not_found          EXCEPTION;
      error_validity_status       EXCEPTION;
      error_vr_dates              EXCEPTION;
      error_validation            EXCEPTION;
      error_actual_date_onhold    EXCEPTION;

   BEGIN
      IF NVL (g_debug, gme_debug.g_log_procedure + 1) <=
                                                    gme_debug.g_log_procedure THEN
         gme_debug.put_line('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      x_return_status := FND_API.g_ret_sts_success;

      -- set output structure
      x_batch_header_rec := p_batch_header_rec;

      IF p_batch_header_rec.batch_type = gme_common_pvt.g_doc_type_fpo THEN
        RAISE error_batch_type;
      END IF;

      IF p_batch_header_rec.batch_status <> gme_common_pvt.g_batch_pending THEN
        RAISE error_batch_status;
      END IF;

      -- set actual start date if it's not passed
      IF p_batch_header_rec.actual_start_date IS NULL THEN
         x_batch_header_rec.actual_start_date := SYSDATE;     
      --BUG 21326978 Correct actual_cmplt_date to actual_start_date   
      ELSIF p_batch_header_rec.actual_start_date > SYSDATE THEN
         RAISE error_future_date;
      END IF;
      
      /*
        BUG 21208206  16-JUN-2015
        check whether the actual_start_date fall into hold period
      */         
      IF p_batch_header_rec.actual_start_date IS NOT NULL THEN
        IF gme_common_pvt.get_batch_hold_status(p_batch_id => p_batch_header_rec.batch_id,
                                                p_date     => p_batch_header_rec.actual_start_date) <> 'R' THEN
          RAISE error_actual_date_onhold;                                        
        END IF; 
      END IF; 

      IF p_batch_header_rec.parentline_id IS NOT NULL THEN
        RAISE error_phantom;
      END IF;
      /*Added by Shalchen 02/28/2013
        Add IF condition to bypass validity rule and validity rule status
        validition for batch open interface project*/      
      -- check validity rule if it's not NULL; it would be NULL in case of LCF
      IF NVL(gme_common_pvt.g_bulk_validation_done,'N') = 'N' THEN        
        IF p_batch_header_rec.recipe_validity_rule_id IS NOT NULL THEN
          OPEN cur_validity_rule(p_batch_header_rec.recipe_validity_rule_id);
          FETCH cur_validity_rule INTO l_validity_rule;
          CLOSE cur_validity_rule;

          IF l_validity_rule.recipe_validity_rule_id IS NULL THEN  -- not found
             RAISE error_vr_not_found;
          ELSE 
             -- following prevents user from releasing a pending batch
             -- if validity rule is ON_HOLD or OBSOLETE.
             OPEN cur_validity_status_type(l_validity_rule.validity_rule_status);
             FETCH cur_validity_status_type INTO l_status_type;
             CLOSE cur_validity_status_type;

             IF l_status_type IN ('1000' ,'800') THEN
               RAISE error_validity_status;         
             END IF;
          END IF;  -- IF l_validity_rule.recipe_validity_rule_id IS NULL
          
        /*  IF l_validity_rule.start_date > x_batch_header_rec.actual_start_date OR
             (l_validity_rule.end_date IS NOT NULL AND
              l_validity_rule.end_date < x_batch_header_rec.actual_start_date) THEN
            RAISE error_vr_dates;
          END IF;*/
  --sunitha ch. Bug 5336007 aded call to check_validity_rule_dates and passed p_validate_plan_dates_ind=1
  --to validate planned start date against validate rule dates
          IF NOT gme_common_pvt.check_validity_rule_dates (
                                       p_validity_rule_id           =>  p_batch_header_rec.recipe_validity_rule_id
                                       ,p_start_date                =>  p_batch_header_rec.actual_start_date   
                                       ,p_cmplt_date                =>  p_batch_header_rec.actual_cmplt_date 
                                       ,p_batch_header_rec          =>  p_batch_header_rec
                                       ,p_validate_plan_dates_ind   => 1) THEN
            x_return_status := fnd_api.g_ret_sts_error;
            RAISE error_vr_dates;
    END IF;
  -- End Bug 5336007
        END IF;  -- IF p_batch_header_rec.recipe_validity_rule_id IS NOT NULL
      END IF;
      gme_validate_flex_fld_pvt.validate_flex_batch_header
                                       (p_batch_header  => p_batch_header_rec
                                       ,x_batch_header  => x_batch_header_rec
                                       ,x_return_status => x_return_status);

      IF x_return_status <> fnd_api.g_ret_sts_success THEN
        RAISE error_validation;
      END IF;
      
      check_unexploded_phantom(p_batch_id             => p_batch_header_rec.batch_id
                              ,p_auto_by_step         => 0                -- all but auto by step ingredients
                              ,p_batchstep_id         => NULL
                              ,x_return_status        => x_return_status);

      IF x_return_status <> fnd_api.g_ret_sts_success THEN
        RAISE error_validation;
      END IF;

      IF NVL (g_debug, gme_debug.g_log_procedure + 1) <=
                                                     gme_debug.g_log_procedure THEN
         gme_debug.put_line('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;

   EXCEPTION
      WHEN error_validation THEN
        NULL;
      WHEN error_vr_dates THEN
        x_return_status := FND_API.G_RET_STS_ERROR;
      WHEN error_validity_status THEN
        gme_common_pvt.log_message ('GME_VALIDITY_OBSO_OR_ONHOLD');
        x_return_status := FND_API.G_RET_STS_ERROR;
      WHEN error_vr_not_found THEN
        gme_common_pvt.log_message ('GME_API_INVALID_VALIDITY');
        x_return_status := FND_API.G_RET_STS_ERROR;
      WHEN error_phantom THEN
        gme_common_pvt.log_message ('PM_INVALID_PHANTOM_ACTION');
        x_return_status := FND_API.G_RET_STS_ERROR;
      WHEN error_batch_type OR error_batch_status THEN
        gme_common_pvt.log_message('GME_API_INVALID_BATCH_REL');
        x_return_status := fnd_api.g_ret_sts_error;
      WHEN error_future_date THEN
        fnd_message.set_name ('GMA', 'SY_NOFUTUREDATE');
        fnd_msg_pub.ADD;
        x_return_status := fnd_api.g_ret_sts_error;
      WHEN error_actual_date_onhold THEN
         gme_common_pvt.log_message ('GME_ACTUAL_DATE_ONHOLD','Actual Start Date');
         x_return_status := FND_API.G_RET_STS_ERROR;              
      WHEN OTHERS THEN
        fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);

        IF g_debug <= gme_debug.g_log_procedure THEN
            gme_debug.put_line(   'Unexpected error: '
                                || g_pkg_name
                                || '.'
                                || l_api_name
                                || ': '
                                || SQLERRM);
        END IF;

        x_return_status := fnd_api.g_ret_sts_unexp_error;
   END validate_batch_for_release;  

END gme_release_batch_pvt;
/
COMMIT;
EXIT;
--show errors



