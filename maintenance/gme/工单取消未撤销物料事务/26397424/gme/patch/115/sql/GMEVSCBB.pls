/* +======================================================================+ */
/* |    Copyright (c) 2005, 2016 Oracle and/or its affiliates.           | */
/* |                         All rights reserved.                         | */
/* |                           Version 12.0.0                             | */
/* +======================================================================+ */
REM dbdrv: sql ~PROD ~PATH ~FILE none none none package &phase=plb \
REM dbdrv: checkfile(120.10.12010000.16=120.12.12020000.14)(120.8.12000000.9=120.10.12010000.8)(115.57=120.1):~PROD:~PATH:~FILE
SET VERIFY OFF;
WHENEVER SQLERROR EXIT FAILURE ROLLBACK;
WHENEVER OSERROR EXIT FAILURE ROLLBACK;

CREATE OR REPLACE PACKAGE BODY gme_scale_batch_pvt AS
/* $Header: GMEVSCBB.pls 120.12.12020000.14 2016/06/06 14:52:54 gmurator ship $ */
   g_debug               VARCHAR2 (5)  := fnd_profile.VALUE ('AFLOG_LEVEL');
/************************************************************
REM* Oracle Process Manufacturing Process Execution APIs    *
REM*                                                        *
REM* File Name: GMEVSCBB.pls                                *
REM* Contents:  Private layer for Scale batch API           *
REM* Author:    Oracle                                      *
REM* Date:      January 2001                                *
REM* 10Feb05 Navin Sinha                                    *
REM*  Made changes for GME_Scale_Batch_Theoretical_Yield_TD.*
REM* 15Dec05 SivakumarG                                     *
REM*  p_qty_type is replaced by 1 during phantom batch      *
REM*  scaling                                               *
REM* Namit S bug#5674398 12-DEC-2006			    *
REM* Passing the substitution effective date that is fetched* 
REM* based on the parameter Ingredient Substitution Date to *
REM* the parameter, pDate of the procedure,		    *
REM* get_substitute_line_item in scale_batch overloaded procedure. *
REM* 8515551 While making a call for the reschedule batch procedure 
REM* 8515551assigns planned completion date as null, hence the scaling removes 
REM* 8515551resource transactions and hence putting the condition not to make it NULL for WIP cases
REM* QZENG 26-Aug-2013 bug 17307215 in procedure scale_batch, changed compare condition from vr.item_um to 
REM*                                vr.detail_uom.
REM* QZENG 29-Aug-2013 bug 17375024 in procedure scale_batch, round value for the calculated quantity

REM *  G. Muratore    13-JAN-2014   Bug 17675571 / 7263065
REM *     Restructure the scale batch logic when scaling off of formula qty.
REM *     This is in essence a front port of 7263065 with additional rework.

REM *  G. Muratore    24-APR-2014   Bug 17867859
REM *     There was an issue uncovered while debugging this issue which had nothing
REM *     to do with customer reported issue. Customer's issue is resolved by 17675571.
REM *     For this issue we had to use NVL to handle old existing formula data.

REM *  Shaliu Chen    18-JUL-2014  ER 19161894                                                 
REM *     Modify create_batch to invoke requisition creation program if batch include OSP step

REM *  Shaliu Chen    18-OCT-2014  ER 18762054                                                 
REM *     Scale only pending steps.
REM *     Procedure: scale_step_and_rsrc

REM *  Shaliu Chen    09-APR-2015  ER 20809749
REM *     Modify create batch to support multiple osp steps. 

REM *  Shaliu Chen    22-MAY-2015  Bug 201102335                        
REM *     modify validation to check whether phantom batch is on hold 
REM *     PROCEDURE:  scale_batch   

REM *  G. Muratore    06-JUN-2016  Bug 23026619 - rework of 8515551                        
REM *     Reschedule code now works off of plan start date for a wip batch when called from scaling.
REM *     We pass in a new parameter as an indicator to reschedule code.
REM *     PROCEDURE:  scale_batch 
************************************************************/

   /*  Global variables   */
   g_pkg_name   CONSTANT VARCHAR2 (30) := 'GME_SCALE_BATCH_PVT';

   PROCEDURE scale_batch (
      p_batch_header_rec         IN              gme_batch_header%ROWTYPE
     ,p_scale_factor             IN              NUMBER
     ,p_primaries                IN              VARCHAR2
     ,p_qty_type                 IN              NUMBER
     ,p_validity_rule_id         IN              NUMBER
     ,p_enforce_vldt_check       IN              VARCHAR2
     ,p_recalc_dates             IN              VARCHAR2
     ,p_use_workday_cal          IN              VARCHAR2
     ,p_contiguity_override      IN              VARCHAR2
     ,x_exception_material_tbl   OUT NOCOPY      gme_common_pvt.exceptions_tab
     ,x_batch_header_rec         OUT NOCOPY      gme_batch_header%ROWTYPE
     ,x_return_status            OUT NOCOPY      VARCHAR2)
   IS
      CURSOR cur_get_matl (v_batch_id NUMBER)
      IS
         SELECT   *
             FROM gme_material_details
            WHERE batch_id = v_batch_id
         ORDER BY line_no;

      CURSOR cur_get_batch_status (v_batch_id NUMBER)
      IS
         SELECT batch_status
           FROM gme_batch_header
          WHERE batch_id = v_batch_id;

      CURSOR cur_get_step_status (v_material_detail_id NUMBER)
      IS
         SELECT s.batchstep_no, step_status
           FROM gme_batch_steps s, gme_batch_step_items i
          WHERE s.batchstep_id = i.batchstep_id
            AND i.material_detail_id = v_material_detail_id;

      CURSOR cur_check_zero_qty_line (v_batch_id NUMBER, v_batch_status NUMBER)
      IS
         SELECT   1
             FROM gme_material_details
            WHERE batch_id = v_batch_id
         GROUP BY line_type
           HAVING SUM (DECODE (v_batch_status, 1, plan_qty, 2, wip_plan_qty) ) =
                                                                             0
              AND line_type IN (1, -1);

      l_api_name   CONSTANT VARCHAR2 (30)                     := 'SCALE_BATCH';
      l_material_tab        gme_common_pvt.material_details_tab;
      x_material_tbl        gme_common_pvt.material_details_tab;
      i                     NUMBER                              := 0;
      l_batch_status        NUMBER;
      l_step_no             NUMBER;
      l_step_status         NUMBER (5);
      l_dummy               NUMBER;
      sum_qty_zero          EXCEPTION;
      step_closed_err       EXCEPTION;
   BEGIN
      x_return_status := fnd_api.g_ret_sts_success;

      -- fetch batch status
      OPEN cur_get_batch_status (p_batch_header_rec.batch_id);

      FETCH cur_get_batch_status
       INTO l_batch_status;
      CLOSE cur_get_batch_status;
        
            IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line ('Batch status is' || l_batch_status );
         END IF;

      -- check if sum of total qty of prod or ing is 0
      OPEN cur_check_zero_qty_line (p_batch_header_rec.batch_id
                                   ,l_batch_status);

      FETCH cur_check_zero_qty_line
       INTO l_dummy;

      IF cur_check_zero_qty_line%FOUND THEN
         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line ('SCALE Batch : sum of either line type is 0');
         END IF;

         gme_common_pvt.log_message ('GME_SUM_QTY_ZERO');

         CLOSE cur_check_zero_qty_line;

         RAISE sum_qty_zero;
      END IF;

      CLOSE cur_check_zero_qty_line;

      FOR l_rec IN cur_get_matl (p_batch_header_rec.batch_id) LOOP
         /* If any of the steps associated with the material line is closed   */
         IF l_rec.line_type = -1 AND l_rec.scale_type <> 0 THEN
            OPEN cur_get_step_status (l_rec.material_detail_id);

            FETCH cur_get_step_status
             INTO l_step_no, l_step_status;

            CLOSE cur_get_step_status;

            IF NVL (l_step_status, 0) = 4 THEN
               gme_common_pvt.log_message ('GME_STEP_CLOSED_ERR'
                                          ,'STEP_NO'
                                          ,l_step_no);
               RAISE step_closed_err;
            END IF;
         END IF;                             /* IF l_rec.line_type = -1 AND */

         i := i + 1;
         l_material_tab (i) := l_rec;
      END LOOP;

      gme_scale_batch_pvt.scale_batch
                        (p_batch_header_rec            => p_batch_header_rec
                        ,p_material_tbl                => l_material_tab
                        ,p_scale_factor                => p_scale_factor
                        ,p_primaries                   => p_primaries
                        ,p_qty_type                    => p_qty_type
                        ,p_validity_rule_id            => p_validity_rule_id
                        ,p_enforce_vldt_check          => p_enforce_vldt_check
                        ,p_recalc_dates                => p_recalc_dates
                        ,                                             -- Navin
                         p_use_workday_cal             => p_use_workday_cal
                        ,p_contiguity_override         => p_contiguity_override
                        ,x_exception_material_tbl      => x_exception_material_tbl
                        ,x_batch_header_rec            => x_batch_header_rec
                        ,                                             -- Navin
                         x_material_tbl                => x_material_tbl
                        ,x_return_status               => x_return_status);
   EXCEPTION
      WHEN sum_qty_zero OR step_closed_err THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN OTHERS THEN
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
         x_return_status := fnd_api.g_ret_sts_unexp_error;
   END scale_batch;

   /* This procedure is over-loaded, so that user can
      either call it with Just the batch_header or
      bach_header and material_details table
      p_batch_header_rec should have all the columns populated
   */
   PROCEDURE scale_batch (
      p_batch_header_rec         IN              gme_batch_header%ROWTYPE
     ,p_material_tbl             IN              gme_common_pvt.material_details_tab
     ,p_scale_factor             IN              NUMBER
     ,p_primaries                IN              VARCHAR2
     ,p_qty_type                 IN              NUMBER
     ,p_validity_rule_id         IN              NUMBER
     ,p_enforce_vldt_check       IN              VARCHAR2
     ,p_recalc_dates             IN              VARCHAR2
     ,p_use_workday_cal          IN              VARCHAR2
     ,p_contiguity_override      IN              VARCHAR2
     ,x_exception_material_tbl   OUT NOCOPY      gme_common_pvt.exceptions_tab
     ,x_batch_header_rec         OUT NOCOPY      gme_batch_header%ROWTYPE
     ,x_material_tbl             OUT NOCOPY      gme_common_pvt.material_details_tab
     ,x_return_status            OUT NOCOPY      VARCHAR2)
   IS
      /* Local variables */
      l_api_name           CONSTANT VARCHAR2 (30)            := 'SCALE_BATCH';
      l_row_count                   NUMBER;
      l_recipe_validity_rule        gmd_recipe_validity_rules%ROWTYPE;
      l_scale_tab                   gmd_common_scale.scale_tab;
      x_scale_tab                   gmd_common_scale.scale_tab;
      l_batch_header                gme_batch_header%ROWTYPE;
      l_resch_batch_header_rec      gme_batch_header%ROWTYPE;
      l_in_batch_header             gme_batch_header%ROWTYPE;
      l_batch_step                  gme_batch_steps%ROWTYPE;
      l_in_batch_step               gme_batch_steps%ROWTYPE;
      l_return_status               VARCHAR2 (1);
      l_return_status_1             VARCHAR2 (1);
      l_return_status_2             VARCHAR2 (1);
      l_count                       NUMBER;
      l_item_id                     NUMBER (10);
      l_scale_factor                NUMBER;
      l_cnt                         NUMBER                              := 0;
      l_from_uom                    sy_uoms_mst.um_code%TYPE;
      l_to_uom                      sy_uoms_mst.um_code%TYPE;
      l_item_no                     ic_item_mst.item_no%TYPE;
      l_routing_scale_factor        NUMBER;
      before_scale_qty              NUMBER;
      after_scale_qty               NUMBER;

      -- Bug 17675571 / 7263065
      l_rec	                    fm_matl_dtl%ROWTYPE;
      l_use_form_data               NUMBER;
            
      l_list                        VARCHAR2 (2000);
      l_total_prim_prod_qty         NUMBER                              := 0;
      l_temp_qty                    NUMBER                              := 0;
      l_mat_row_count               NUMBER;
      l_rsc_row_count               NUMBER;
      l_validity_rule_id            NUMBER;
      l_enforce_vldt_check          VARCHAR2 (1);
      l_is_charge_associated        NUMBER;
      l_use_workday_cal             VARCHAR2 (1);
      l_reserved_qty                NUMBER;
      l_material_details_qty        NUMBER;
      l_new_mvord_qty               NUMBER;
      l_actual_reserved_qty         NUMBER;
      l_actual_reserved_mvord_qty   NUMBER;
      l_required_qty                NUMBER;
      l_actual_qty                  NUMBER;
      l_exception_qty               NUMBER;
      l_index                       NUMBER                              := 0;
      l_ingr_req_date		    DATE; --nsinghi bug#5674398
      l_message_list                VARCHAR2(2000);
      /*                                    
      BEGIN ER 19161894                    
      Shaliu Chen 18-JUL-2014               
      Added for OPM Step Level OSP Project  
      */                                          
      l_message_count               NUMBER;
      l_propagate_change_to_po      NUMBER;
      l_osp_resource_flag           NUMBER;  
      l_batchstep_resource_id       NUMBER;
      /*END ER 19161894*/    
      -- Exceptions --
      invalid_status                EXCEPTION;
      invalid_validity_rule         EXCEPTION;
      expected_error                EXCEPTION;
      load_trans_failed             EXCEPTION;
      batch_fetch_error             EXCEPTION;
      error_upd_default_tran        EXCEPTION;
      uom_conversion_error          EXCEPTION;
      scale_batch_failed            EXCEPTION;
      scale_phant_fail              EXCEPTION;
      error_updating_steps          EXCEPTION;
      batch_step_fetch_err          EXCEPTION;
      trans_qty_err                 EXCEPTION;
      material_save_failed          EXCEPTION;
      scale_step_and_rsrc_error     EXCEPTION;
      clear_chg_error               EXCEPTION;
      reschedule_batch_fail         EXCEPTION;
      reduce_reservation_fail       EXCEPTION;  -- 4944024
      /*                                    
      ER 19161894 Shaliu Chen 18-JUL-2014               
      Added for OPM Step Level OSP Project  
      */       
      qty_sync_fail                 EXCEPTION;

     --FPBug#4351032
     l_formula_tbl                  GMDFMVAL_PUB.formula_detail_tbl;
     
     -- Database cursors for various tables
      CURSOR recipe_validity_rule_cursor (v_validity_rule_id IN NUMBER)
      IS
         SELECT *
           FROM gmd_recipe_validity_rules
          WHERE recipe_validity_rule_id = v_validity_rule_id;

      --Cursor to get status type of the validity_rule_status.
      CURSOR cur_validity_status_type (v_validity_rule_status VARCHAR2)
      IS
         SELECT status_type
           FROM gmd_status
          WHERE status_code = v_validity_rule_status;

      l_status_type                 gmd_status.status_type%TYPE;

      CURSOR cur_item_no (v_item_id NUMBER, v_org_id NUMBER)
      IS
         SELECT concatenated_segments
           FROM mtl_system_items_kfv
          WHERE inventory_item_id = v_item_id AND organization_id = v_org_id;

      -- Bug 17675571 / 7263065 - reinstate this cursor that was removed by item sub.
      CURSOR cur_get_formula_matl (v_formulaline_id NUMBER)
      IS
         SELECT *
         --SELECT qty, scale_type, scale_multiple, scale_rounding_variance
         --      ,rounding_direction, contribute_yield_ind, item_id, item_um
         --      ,line_no, line_type
           FROM fm_matl_dtl
          WHERE formulaline_id = v_formulaline_id;

      CURSOR cur_get_matl (v_batch_id NUMBER)
      IS
         SELECT   *
             FROM gme_material_details
            WHERE batch_id = v_batch_id
         ORDER BY line_no;

      CURSOR cur_item_step_asso (v_batch_id NUMBER)
      IS
         SELECT DISTINCT batchstep_id
                    FROM gme_batch_step_items
                   WHERE batch_id = v_batch_id;

      CURSOR cur_is_charge_associated (v_batch_id NUMBER)
      IS
         SELECT 1
           FROM DUAL
          WHERE EXISTS (SELECT 1
                          FROM gme_batch_step_charges
                         WHERE batch_id = v_batch_id);

      /*                                    
      BEGIN ER 19161894  Shaliu Chen 18-JUL-2014               
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
      x_return_status := fnd_api.g_ret_sts_success;

      IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
         gme_debug.put_line ('Scale factor is ' || p_scale_factor);
      END IF;

      --Deleting the charges associated with the step on batch scaling.
      OPEN cur_is_charge_associated (p_batch_header_rec.batch_id);

      FETCH cur_is_charge_associated
       INTO l_is_charge_associated;

      IF cur_is_charge_associated%FOUND THEN
         CLOSE cur_is_charge_associated;

         IF NVL (g_debug, -1) = gme_debug.g_log_statement THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || 'before call to clear charges procedure');
         END IF;

         gme_batch_step_chg_pvt.clear_charges
                                   (p_batch_id           => p_batch_header_rec.batch_id
                                   ,x_return_status      => l_return_status);

         IF l_return_status <> x_return_status THEN
            RAISE clear_chg_error;
         END IF;
      ELSE
         CLOSE cur_is_charge_associated;
      END IF;

      IF NVL (g_debug, -1) = gme_debug.g_log_statement THEN
         gme_debug.put_line ('before material loop. p_qty_type is '||p_qty_type);
      END IF;

      FOR i IN 1 .. p_material_tbl.COUNT LOOP
         IF p_qty_type = 0 THEN
            IF p_material_tbl (i).formulaline_id IS NOT NULL THEN

               IF NVL (g_debug, -1) = gme_debug.g_log_statement THEN
                  gme_debug.put_line ('p_material_tbl(i).formulaline_id is '||p_material_tbl(i).formulaline_id);
               END IF;

               -- Bug 17675571 / 7263065 - fetch original values from formula.
               OPEN cur_get_formula_matl (p_material_tbl (i).formulaline_id);
               FETCH cur_get_formula_matl INTO l_rec;
               CLOSE cur_get_formula_matl;

               IF NVL (g_debug, -1) = gme_debug.g_log_statement THEN
                  gme_debug.put_line ('Using formula details');
                  gme_debug.put_line ('p_material_tbl(i).inventory_item_id is '||p_material_tbl(i).inventory_item_id);
                  gme_debug.put_line ('p_material_tbl(i).line_type is '||p_material_tbl(i).line_type);
               END IF;

               -- For now always used fetched values.
               l_scale_tab (i).qty                     := l_rec.qty * (1 + l_rec.scrap_factor);                                           
               l_scale_tab (i).detail_uom              := l_rec.detail_uom;
               l_scale_tab (i).scale_type              := l_rec.scale_type;
               l_scale_tab (i).contribute_yield_ind    := NVL(l_rec.contribute_yield_ind, 'Y'); -- Bug 17867859 - Use NVL to handle old existing formula data.
               l_scale_tab (i).scale_multiple          := l_rec.scale_multiple;
               l_scale_tab (i).line_no                 := l_rec.line_no;
               l_scale_tab (i).line_type               := l_rec.line_type;
               l_scale_tab (i).scale_rounding_variance := l_rec.scale_rounding_variance;
               l_scale_tab (i).rounding_direction      := l_rec.rounding_direction;
               l_scale_tab (i).inventory_item_id       := l_rec.inventory_item_id;

               IF NVL (g_debug, -1) = gme_debug.g_log_statement THEN
                  gme_debug.put_line ('scale tab data is ');
                  gme_debug.put_line ('l_rec.qty is '||l_rec.qty);
                  gme_debug.put_line ('l_rec.qty with scrap is '||l_scale_tab(i).qty);
                  gme_debug.put_line ('l_scale_tab(i).detail_uom is '||l_scale_tab(i).detail_uom);
                  gme_debug.put_line ('l_scale_tab(i).scale_type is '||l_scale_tab(i).scale_type);
                  gme_debug.put_line ('l_scale_tab(i).contribute_yield_ind is '||l_scale_tab(i).contribute_yield_ind);
                  gme_debug.put_line ('l_scale_tab(i).scale_multiple is '||l_scale_tab(i).scale_multiple);
                  gme_debug.put_line ('l_scale_tab(i).scale_rounding_variance is '||l_scale_tab(i).scale_rounding_variance);
                  gme_debug.put_line ('l_scale_tab(i).rounding_direction is '||l_scale_tab(i).rounding_direction);                  
                  gme_debug.put_line ('l_scale_tab(i).inventory_item_id is '||l_scale_tab(i).inventory_item_id);
               END IF;               

/*
               l_use_form_data := 0;
               -- Use original formula values for all non ingredients as they do not qualify for item sub anyway.
               -- For ingredients, use formula values if it was never substituted in the first place.
               IF p_material_tbl (i).inventory_item_id = l_rec.inventory_item_id OR
                  p_material_tbl (i).line_type <> -1 THEN

                  IF NVL (g_debug, -1) = gme_debug.g_log_statement THEN
                     gme_debug.put_line ('Using formula details');
                  END IF;

                  l_use_form_data := 1;
                  l_scale_tab (i).qty                     := l_rec.qty * (1 + l_rec.scrap_factor);                                           
                  l_scale_tab (i).detail_uom              := l_rec.detail_uom;
                  l_scale_tab (i).scale_type              := l_rec.scale_type;
                  l_scale_tab (i).contribute_yield_ind    := l_rec.contribute_yield_ind;                                                                                         
                  l_scale_tab (i).scale_multiple          := l_rec.scale_multiple;
                  l_scale_tab (i).line_no                 := l_rec.line_no;
                  l_scale_tab (i).line_type               := l_rec.line_type;
                  l_scale_tab (i).scale_rounding_variance := l_rec.scale_rounding_variance;
                  l_scale_tab (i).rounding_direction      := l_rec.rounding_direction;
                  l_scale_tab (i).inventory_item_id       := l_rec.inventory_item_id;                                                      
               ELSE     
                  IF NVL (g_debug, -1) = gme_debug.g_log_statement THEN
                     gme_debug.put_line ('Using material details which means item had already been');
                     gme_debug.put_line ('substituted prior to this scaling.');
                  END IF;
                  
                  l_scale_tab (i).qty                     := p_material_tbl(i).plan_qty;                                           
                  l_scale_tab (i).detail_uom              := p_material_tbl(i).dtl_um;
                  l_scale_tab (i).scale_type              := p_material_tbl(i).scale_type;
                  l_scale_tab (i).contribute_yield_ind    := p_material_tbl(i).contribute_yield_ind;
                  l_scale_tab (i).scale_multiple          := p_material_tbl(i).scale_multiple;
                  l_scale_tab (i).line_no                 := p_material_tbl(i).line_no;                                 
                  l_scale_tab (i).line_type               := p_material_tbl(i).line_type;
                  l_scale_tab (i).scale_rounding_variance := p_material_tbl(i).scale_rounding_variance;
                  l_scale_tab (i).rounding_direction      := p_material_tbl(i).rounding_direction;
                  l_scale_tab (i).inventory_item_id       := p_material_tbl(i).inventory_item_id;
               END IF;     
*/
               
               -- Let's do item substitution for ingredients only if the item does not match, 
               -- but before scaling. This means it was previously substituted.
               IF p_material_tbl (i).line_type = -1 AND
                  l_rec.inventory_item_id <> p_material_tbl(i).inventory_item_id THEN 
                  -- nsinghi Bug#5674398 Pass the ingredient substitution date rather than 
                  -- the batch plan start date for pDate parameter
                  l_ingr_req_date := gme_api_grp.get_ingr_sub_date
                                        (p_material_tbl(i).batch_id,
                                         p_material_tbl(i).material_detail_id);

                  IF g_debug <= gme_debug.g_log_statement THEN
 	             gme_debug.put_line('Orig '||p_material_tbl(i).formulaline_id||' '||
 	                                         l_scale_tab(i).inventory_item_id||' '||
                                                 l_scale_tab(i).qty ||' '||
                                                 l_scale_tab(i).detail_uom);                  
                  END IF;
                                
	          gmdfmval_pub.get_substitute_line_item( 
	                            pFormulaline_id      =>  p_material_tbl(i).formulaline_id
                                    ,pItem_id            =>  l_scale_tab(i).inventory_item_id
                                    ,pQty                =>  l_scale_tab(i).qty
                                    ,pUom                =>  l_scale_tab(i).detail_uom
                                    ,pScale_multiple     =>  l_scale_tab(i).scale_multiple
                                    ,pDate               =>  l_ingr_req_date
                                    ,xFormulaDetail_tbl  =>  l_formula_tbl);                  
              
                  -- Bug 18516569 - Use correct variables in debug and also in assignment.                
                  IF g_debug <= gme_debug.g_log_statement THEN                  
                     gme_debug.put_line('New '||p_material_tbl(i).formulaline_id||' '||
                                               l_formula_tbl(1).inventory_item_id||' '||
                                               l_formula_tbl(1).qty ||' '||
                                               l_formula_tbl(1).item_um);
                  END IF;
                  
                  l_scale_tab (i).scale_multiple     := l_formula_tbl(1).scale_multiple;
                  l_scale_tab (i).detail_uom         := l_formula_tbl(1).detail_uom;                  
                  l_scale_tab (i).qty                := l_formula_tbl(1).qty;
                  l_scale_tab (i).inventory_item_id  := l_formula_tbl(1).inventory_item_id; -- Bug 18516569
                  
                  -- Commented out the condition below. If inventory_item_id was substituted in the
                  -- original creation of the batch then it should be substituted again.
                  -- Hence, I don't think we need to compare inventory_item_id.
                  -- IF p_material_tbl(i).inventory_item_id = l_formula_tbl(1).inventory_item_id THEN
                     -- l_scale_tab (i).qty := l_formula_tbl(1).qty;
                  -- ELSE
                     -- l_scale_tab (i).qty := 0;
                  -- END IF;
               END IF;               
               --siva FPBug#4351032 End                   /* cur_get_formula_matl */
            ELSE
               /* This line is added in the batch, not available in the formula */
               IF NVL (g_debug, -1) = gme_debug.g_log_statement THEN
                  gme_debug.put_line('p_material_tbl(i).formulaline_id is NULL which means it was inserted after batch created.');
               END IF;

               l_scale_tab (i).qty                     := 0;
               l_scale_tab (i).detail_uom              := p_material_tbl (i).dtl_um;
               l_scale_tab (i).scale_type              := p_material_tbl (i).scale_type;
               l_scale_tab (i).contribute_yield_ind    := p_material_tbl (i).contribute_yield_ind;
               l_scale_tab (i).scale_multiple          := p_material_tbl (i).scale_multiple;
               l_scale_tab (i).line_no                 := p_material_tbl (i).line_no;
               l_scale_tab (i).line_type               := p_material_tbl (i).line_type;
               l_scale_tab (i).scale_rounding_variance := p_material_tbl (i).scale_rounding_variance;
               l_scale_tab (i).rounding_direction      := p_material_tbl (i).rounding_direction;
               l_scale_tab (i).inventory_item_id       := p_material_tbl (i).inventory_item_id;
            END IF;
         ELSE                                                 /* p_type = 1 */
            IF    p_batch_header_rec.batch_status = 0               /* NEW */
               OR p_batch_header_rec.batch_status = 1 /* PENDING */ THEN
               l_scale_tab (i).qty := p_material_tbl (i).plan_qty;
            ELSIF p_batch_header_rec.batch_status = 2 /* WIP */ THEN
               l_scale_tab (i).qty := p_material_tbl (i).wip_plan_qty;
            ELSE
               gme_common_pvt.log_message ('INVALID_BATCH_STATUS'
                                          ,'PROCESS'
                                          ,'Scaling');
               RAISE invalid_status;
            END IF;

            l_scale_tab (i).detail_uom := p_material_tbl (i).dtl_um;
            l_scale_tab (i).scale_type := p_material_tbl (i).scale_type;
            l_scale_tab (i).contribute_yield_ind :=
                                       p_material_tbl (i).contribute_yield_ind;
            l_scale_tab (i).scale_multiple :=
                                             p_material_tbl (i).scale_multiple;
            l_scale_tab (i).scale_rounding_variance :=
                                    p_material_tbl (i).scale_rounding_variance;
            l_scale_tab (i).rounding_direction :=
                                         p_material_tbl (i).rounding_direction;
            l_scale_tab (i).line_no := p_material_tbl (i).line_no;
            l_scale_tab (i).line_type := p_material_tbl (i).line_type;
            l_scale_tab (i).inventory_item_id :=
                                          p_material_tbl (i).inventory_item_id;
         END IF;                                          /* p_qty_type = 0 */
      END LOOP;                                /* p_material_tbl.COUNT LOOP */

      x_material_tbl := p_material_tbl;
      l_row_count := p_material_tbl.COUNT;
      -- Get validity_rule row for batch
      /* LCF Changes */
      l_validity_rule_id :=
          NVL (p_batch_header_rec.recipe_validity_rule_id, p_validity_rule_id);

      IF NVL (g_debug, -1) = gme_debug.g_log_statement THEN
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || 'Validity Rule ID '
                             || l_validity_rule_id);
      END IF;

      IF l_validity_rule_id IS NOT NULL THEN
         l_enforce_vldt_check := NVL (p_enforce_vldt_check, 'T');

         OPEN recipe_validity_rule_cursor (l_validity_rule_id);

         FETCH recipe_validity_rule_cursor
          INTO l_recipe_validity_rule;

         CLOSE recipe_validity_rule_cursor;

         IF    l_recipe_validity_rule.recipe_validity_rule_id IS NULL
            OR l_recipe_validity_rule.delete_mark = 1 THEN
            -- Report error that the rule passed in is invalid
            -- This is a fatal error and there is no point continuing
            gme_common_pvt.log_message ('GME_API_INVALID_RULE');
            RAISE expected_error;
         ELSE
            --Prevent user from scaling a pending batch whose
            --validity rule is either obsoleted or put on hold. But user is allowed
            --to scale a WIP batch, a message informing user that validity rule is
            --obsoleted/hold is logged though.
            OPEN cur_validity_status_type
                                 (l_recipe_validity_rule.validity_rule_status);

            FETCH cur_validity_status_type
             INTO l_status_type;

            CLOSE cur_validity_status_type;

            IF l_status_type IN ('1000', '800') THEN
               IF     p_batch_header_rec.batch_status = 1
                  AND p_batch_header_rec.batch_type = 0 THEN
                  gme_common_pvt.log_message ('GME_VALIDITY_OBSO_OR_ONHOLD');
                  RAISE expected_error;
               ELSIF     p_batch_header_rec.batch_status = 2
                     AND p_batch_header_rec.batch_type = 0 THEN
                  gme_common_pvt.log_message ('GME_VALIDITY_OBSO_OR_ONHOLD1');
               END IF;
            END IF;
         END IF;
      ELSE
         l_enforce_vldt_check := 'F';

         IF NVL (g_debug, -1) = gme_debug.g_log_statement THEN
            gme_debug.put_line
                        (   g_pkg_name
                         || '.'
                         || l_api_name
                         || 'Validity Rule ID NOT FOUND. This must be LCF batch');
         END IF;
      END IF;                             /* l_validity_rule_id IS NOT NULL */

      -- call for getting the total_output qty before the scaling.
      IF     p_batch_header_rec.batch_status <> 0
         AND p_batch_header_rec.poc_ind = 'Y' THEN
         before_scale_qty :=
            gme_scale_batch_pvt.get_total_qty (p_material_tbl
                                              ,p_batch_header_rec);
      END IF;

      gmd_common_scale.scale (p_scale_tab          => l_scale_tab
                             ,p_orgn_id            => p_batch_header_rec.organization_id
                             ,p_scale_factor       => p_scale_factor
                             ,p_primaries          => p_primaries
                             ,x_scale_tab          => x_scale_tab
                             ,x_return_status      => x_return_status);
                             
      l_total_prim_prod_qty := 0;

      IF x_return_status = fnd_api.g_ret_sts_success THEN
         FOR l_row_number IN 1 .. l_row_count LOOP
            /* Check that scaled quantity is within validity rules */
            IF l_enforce_vldt_check = 'T' THEN
               IF     p_material_tbl (l_row_number).inventory_item_id =
                                     l_recipe_validity_rule.inventory_item_id
                  AND p_material_tbl (l_row_number).line_type = 1 THEN
                  /* Primary Product */
                  /* Convert QTY to validity rule UOM */
                  IF p_material_tbl (l_row_number).dtl_um =
                  --Modified by QZENG, Bug 17307215, compared with detail_uom, item_um is obsoleted from R12
                                               --l_recipe_validity_rule.item_um THEN
                                               l_recipe_validity_rule.detail_uom THEN
                     l_total_prim_prod_qty :=
                        l_total_prim_prod_qty
                        + round(x_scale_tab (l_row_number).qty, 5); --Modified by QZENG, bug 17375024.
                  ELSE
                     l_temp_qty :=
                        inv_convert.inv_um_convert
                             (p_material_tbl (l_row_number).inventory_item_id
                             ,5
                             ,x_scale_tab (l_row_number).qty
                             ,p_material_tbl (l_row_number).dtl_um
                             ,l_recipe_validity_rule.detail_uom
                             ,NULL
                             ,NULL);

                     IF l_temp_qty < 0 THEN
                        IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
                           gme_debug.put_line
                              ('Failed in UOM Conv from formula product UOM to routing UOM');
                        END IF;

                        l_item_id :=
                               p_material_tbl (l_row_number).inventory_item_id;
                        l_from_uom := p_material_tbl (l_row_number).dtl_um;
                        l_to_uom := l_recipe_validity_rule.detail_uom;
                        RAISE uom_conversion_error;
                     ELSE
                        l_total_prim_prod_qty :=
                                           l_total_prim_prod_qty + l_temp_qty;
                     END IF;                              /* l_temp_qty < 0 */
                  END IF;
/* p_material_details (l_row_number).item_um = l_recipe_validity_rule.item_um */
               END IF;
  /* p_material_tbl (l_row_number).item_id = l_recipe_validity_rule.item_id */
            END IF;                          /* l_enforce_vldt_check = TRUE */

            -- Round the qtys to 5 decimal places.
            IF p_batch_header_rec.batch_status = 0 /* NEW */ THEN
               x_material_tbl (l_row_number).original_qty :=
                                    ROUND (x_scale_tab (l_row_number).qty, 5);
               x_material_tbl (l_row_number).plan_qty :=
                                    ROUND (x_scale_tab (l_row_number).qty, 5);
            ELSIF p_batch_header_rec.batch_status = 1 /* PENDING */ THEN
               x_material_tbl (l_row_number).plan_qty :=
                                    ROUND (x_scale_tab (l_row_number).qty, 5);
            ELSE
               x_material_tbl (l_row_number).wip_plan_qty :=
                                    ROUND (x_scale_tab (l_row_number).qty, 5);
            END IF;

            /* If scaling is not called from create batch */
            IF p_batch_header_rec.batch_status <> 0 THEN
               -- 4944024 BEGIN 
               -- If there is a decrease in anticipated yield, 
               -- then reservations associated to this supply
               -- need to be decreased proportionately
               -- ============================================
               IF x_material_tbl(l_row_number).line_type <> -1 THEN
                 IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
                   gme_debug.put_line(g_pkg_name||'.'||l_api_name||' Invoking relieve_prod_supply_resv ');
                 END IF;
                 gme_supply_res_pvt.relieve_prod_supply_resv 
                   (p_matl_dtl_rec         => x_material_tbl(l_row_number)
                   ,x_msg_count            => l_count
                   ,x_msg_data             => l_list                           
                   ,x_return_status        => x_return_status);                    

                 IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
                   gme_debug.put_line(g_pkg_name||'.'||l_api_name||' Relieve_Prod_Supply_Resv returns '||x_return_status);
                 END IF;

                 IF x_return_status <> FND_API.G_RET_STS_SUCCESS THEN
                   RAISE reduce_reservation_fail;
                 END IF;
               END IF;
               -- 4944024 END    
               IF NOT (gme_material_details_dbl.update_row
                                                (x_material_tbl (l_row_number) ) ) THEN
                  RAISE material_save_failed;
               ELSE
                  x_material_tbl (l_row_number).last_update_date :=
                                                   gme_common_pvt.g_timestamp;
                  x_material_tbl (l_row_number).last_updated_by :=
                                                  gme_common_pvt.g_user_ident;
                  x_material_tbl (l_row_number).last_update_login :=
                                                    gme_common_pvt.g_login_id;
               END IF;
            END IF;
         END LOOP;                                          /* l_row_number */

         /* Check the total primary product qty */
         IF l_enforce_vldt_check = 'T' THEN
            IF    l_total_prim_prod_qty > l_recipe_validity_rule.max_qty
               OR l_total_prim_prod_qty < l_recipe_validity_rule.min_qty THEN
               gme_common_pvt.log_message
                                  ('GME_SCALED_QTY_EXCEED_LIMITS'
                                  ,'SCALE_QTY'
                                  ,    TO_CHAR (l_total_prim_prod_qty)
                                    || ' '
                                    || l_recipe_validity_rule.detail_uom
                                  ,'MIN_QTY'
                                  ,    TO_CHAR (l_recipe_validity_rule.min_qty)
                                    || ' '
                                    || l_recipe_validity_rule.detail_uom
                                  ,'MAX_QTY'
                                  ,    TO_CHAR (l_recipe_validity_rule.max_qty)
                                    || ' '
                                    || l_recipe_validity_rule.detail_uom);
               RAISE invalid_validity_rule;
            END IF;
         END IF;                             /* l_enforce_vldt_check = TRUE */

         -- call for getting the total_output qty after the scaling.
         IF     p_batch_header_rec.batch_status <> 0
            AND p_batch_header_rec.poc_ind = 'Y' THEN
            after_scale_qty :=
               gme_scale_batch_pvt.get_total_qty (x_material_tbl
                                                 ,p_batch_header_rec);
         END IF;

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
         l_batch_header.batch_id := p_material_tbl (1).batch_id;

         /* If we are invoking the scale batch from the create batch api then */
         /* we need not adjust the transactions as the create batch API will  */
         /* be doing the same - The batch id is not assigned to the material  */
         /* detail row when the scale batch is invoked in create batch API    */
         IF l_batch_header.batch_id IS NOT NULL THEN
            IF NOT (gme_batch_header_dbl.fetch_row (p_batch_header_rec
                                                   ,l_batch_header) ) THEN
               RAISE batch_fetch_error;
            END IF;

            x_batch_header_rec := l_batch_header;
            /* loading material and resource transactions are done before deleting the
            existing transactions in gme_inventory_txns_gtmp.
            Previously this call was in trans_qty so loading is happening before deletion and api
            is not working fine */
            -- gme_trans_engine_util.load_mat_and_rsc_trans (
            gme_trans_engine_util.load_rsrc_trans
                                          (x_return_status      => x_return_status
                                          ,x_rsc_row_count      => l_rsc_row_count
                                          ,p_batch_row          => p_batch_header_rec);

            IF x_return_status <> fnd_api.g_ret_sts_success THEN
               RAISE load_trans_failed;
            END IF;

            /* Update POC Data if steps are associated to a material line  */
            /* First fetch all the steps associated to a batch(all material lines) */
            /* Checking for the new batch and batch with routing only */
            /* we are using p_batch_header_rec for batch status to check whether batch has been created or not*/
            IF     p_batch_header_rec.batch_status <> 0
               AND l_batch_header.poc_ind = 'Y' THEN
               IF l_batch_header.automatic_step_calculation = 1 THEN
                  FOR l_cur_item_step_asso IN
                     cur_item_step_asso (l_batch_header.batch_id) LOOP
                     l_batch_step.batchstep_id :=
                                            l_cur_item_step_asso.batchstep_id;
                     l_in_batch_step := l_batch_step;
                     gme_update_step_qty_pvt.update_step_qty
                                                            (l_in_batch_step
                                                            ,l_count
                                                            ,l_list
                                                            ,l_return_status
                                                            ,l_batch_step);

                     IF l_return_status <> x_return_status THEN
                        RAISE error_updating_steps;
                     END IF;

                     IF NOT (gme_batch_steps_dbl.fetch_row (l_batch_step
                                                           ,l_batch_step) ) THEN
                        RAISE batch_step_fetch_err;
                     END IF;
                  END LOOP;                         /* l_cur_item_step_asso */
               ELSE       /* l_batch_header.automatic_step_calculation <> 1 */
                  IF     after_scale_qty IS NOT NULL
                     AND before_scale_qty IS NOT NULL THEN
                     l_routing_scale_factor :=
                                           after_scale_qty / before_scale_qty;
                  ELSE
                     l_routing_scale_factor := 1;
                  END IF;

                  scale_step_and_rsrc
                            (p_batch_id                  => l_batch_header.batch_id
                            ,p_routing_scale_factor      => l_routing_scale_factor
                            ,x_return_status             => x_return_status);

                  IF x_return_status <> fnd_api.g_ret_sts_success THEN
                     RAISE scale_step_and_rsrc_error;
                  END IF;
               END IF;                       /*  automatic_step_calculation */
            END IF;                                         /* for new batch*/

            /* Handling return status  */
            IF x_return_status <> fnd_api.g_ret_sts_success THEN
               RAISE trans_qty_err;
            END IF;

            FOR line_index IN 1 .. x_material_tbl.COUNT LOOP
               /* Now we have to scale the phantom batch if it exists
                  and also that the scale type is not FIXED for the line*/
               IF     NVL (x_material_tbl (line_index).phantom_id, 0) > 0
                  AND (x_material_tbl (line_index).scale_type <> 0) THEN
                  IF p_batch_header_rec.batch_status > 1 THEN
                     l_scale_factor :=
                          x_material_tbl (line_index).wip_plan_qty
                        / p_material_tbl (line_index).wip_plan_qty;
                  ELSE
                     l_scale_factor :=
                          x_material_tbl (line_index).plan_qty
                        / p_material_tbl (line_index).plan_qty;
                  END IF;

                  IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
                     gme_debug.put_line
                               (   'Calling scale phantom with Scale factor '
                                || l_scale_factor);
                  END IF;

                  l_in_batch_header.batch_id :=
                                        x_material_tbl (line_index).phantom_id;

                  IF NOT (gme_batch_header_dbl.fetch_row (l_in_batch_header
                                                         ,l_batch_header) ) THEN
                     RAISE batch_fetch_error;
                  END IF;

                  x_batch_header_rec := l_batch_header;
                  
                  /* Shaliu Chen     22-MAY-2015  Bug 201102335                                                                        
                     raise an error if phantom batch is on hold with STOP type                                     
                  */                                                                                 
                  IF gme_common_pvt.get_batch_hold_status(l_batch_header.batch_id) = 'S' THEN
                    gme_common_pvt.log_message('GME_PHANTOM_ONHOLD',                             
                                               'BATCH_NO',                                           
                                               l_batch_header.batch_no); 
                    l_return_status := fnd_api.g_ret_sts_error;                                                
                    RAISE scale_phant_fail;                                                
                  END IF;                       
                  
		  /* FPBug#4648936
		     Phantom batch will be scaled based on plan quantity always */
                  gme_scale_batch_pvt.scale_batch
                     (p_batch_header_rec            => l_batch_header
                     ,p_scale_factor                => l_scale_factor
                     ,p_primaries                   => 'OUTPUTS'
                     ,p_qty_type                    => 1  --p_qty_type
                     ,p_validity_rule_id            => l_batch_header.recipe_validity_rule_id
                     ,p_enforce_vldt_check          => p_enforce_vldt_check
                     ,p_recalc_dates                => p_recalc_dates
                     ,                                                -- Navin
                      p_use_workday_cal             => p_use_workday_cal
                     ,p_contiguity_override         => p_contiguity_override
                     ,x_exception_material_tbl      => x_exception_material_tbl
                     ,x_batch_header_rec            => x_batch_header_rec
                     ,                                                -- Navin
                      x_return_status               => l_return_status);

                  /* Handling return status  */
                  IF l_return_status <> fnd_api.g_ret_sts_success THEN
                     RAISE scale_phant_fail;
                  END IF;
               END IF;
              /* IF NVL (x_material_tbl (line_index).phantom_id, 0) > 0 AND */
            END LOOP;        /* FOR line_index IN 1 .. x_material_tbl.COUNT */
         END IF;             /* IF l_batch_header.batch_id IS NOT NULL THEN */
      ELSE           /* IF x_return_status = FND_API.G_RET_STS_SUCCESS THEN */
         RAISE scale_batch_failed;
      END IF;

      /****
          START 10Feb05 Navin Sinha Made changes for GME_Scale_Batch_Theoretical_Yield_TD.
      *****/
      IF p_batch_header_rec.batch_status <> 0       THEN   
         IF p_recalc_dates = 'T' THEN
            l_use_workday_cal := p_use_workday_cal;
         
            IF gme_common_pvt.g_calendar_code IS NULL
               AND p_use_workday_cal = fnd_api.g_true THEN
               gme_common_pvt.log_message ('GME_NO_WORKDAY_CAL_ASSOCIATED');
               -- Navin: PENDING: NEW MESSAGE TO BE REGISTERED. Workday calendar is not associated to the organization.
               l_use_workday_cal := fnd_api.g_false;
               -- workday_cal cannot be used.
            END IF;
            
            IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
               gme_debug.put_line('calling reschedule_batch '||p_batch_header_rec.batch_id);
            END IF;
             
            -- Call reschedule_batch for plan_start_date for the batch.
            -- Pass the p_use_workday_cal and p_contiguity_override to it.
            -- Navin: PENDING: Verify who is doing the changes to gme_api_reschedule_batch.reschedule_batch
            l_resch_batch_header_rec := p_batch_header_rec ;
            
            -- Bug 23026619 - Back out 8515551. When scaling we always want reschedule to work off of plan start.
            /*8515551 starts*/
            -- IF (p_batch_header_rec.batch_status <> 2)       THEN           
               -- l_resch_batch_header_rec.plan_cmplt_date := null ;          
            -- END IF;
           /*8515551 ends*/
               
            l_resch_batch_header_rec.plan_cmplt_date := NULL; 
           
            IF l_resch_batch_header_rec.parentline_id IS NULL THEN         	
               gme_reschedule_batch_pvt.reschedule_batch           
                                    (p_batch_header_rec         => l_resch_batch_header_rec
                                    ,p_use_workday_cal          => l_use_workday_cal
                                    ,p_contiguity_override      => p_contiguity_override                                                                               
                                    ,p_called_by                => 'SCALING' -- Bug  23026619 - This is specific scaling a wip batch.
                                    ,x_batch_header_rec         => x_batch_header_rec
                                    ,x_return_status            => l_return_status);
               
               -- Check return status and raise error if needed.
               IF l_return_status <> fnd_api.g_ret_sts_success THEN
                  RAISE reschedule_batch_fail;
               END IF;
            END IF; /*  parentline_id IS NOT NULL THEN */
         END IF;                                 /* IF P_recalc_dates = 1 THEN */

         l_index := 0;
         l_return_status_1 := NULL;
         l_return_status_2 := NULL;

         -- Get Material Details Quantity
         -- Loop through all the records of x_material_tbl
         FOR mtl_dtl_index IN 1 .. x_material_tbl.COUNT LOOP
            -- Based on the batch status of pending or WIP, the plan_qty or wip_plan_qty will be considered.
            IF p_batch_header_rec.batch_status = 1 /* Pending */ THEN
               l_material_details_qty :=
                                NVL (x_material_tbl (mtl_dtl_index).plan_qty, 0);
            ELSIF p_batch_header_rec.batch_status = 2 /* WIP */ THEN
               l_material_details_qty :=
                            NVL (x_material_tbl (mtl_dtl_index).wip_plan_qty, 0);
            END IF;
            IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
               gme_debug.put_line('calling scale for mat'||to_char(x_material_tbl.COUNT));
            END IF;
            -- In case of scale down the batch, the user will be informed for the excess actual_qty
            -- or reserved _qty compared to plan_qty or wip_plan_qty.
            -- The return staus to be assigned in such case is: T- if actual_qty is in excess
            -- OR N - if reserved_qty is in excess.
         
            -- Check for actual_qty in case batch is scaled down.
            l_actual_qty := 0;
            l_actual_qty := NVL (x_material_tbl (mtl_dtl_index).actual_qty, 0);
         
            IF l_actual_qty > l_material_details_qty
               AND x_material_tbl (mtl_dtl_index).phantom_type = 0
               AND x_material_tbl (mtl_dtl_index).line_type IN (-1, 1) THEN
                             -- only for ingredents and products, not for phantom
               l_return_status_1 := 'T';
               l_exception_qty := l_actual_qty - l_material_details_qty;
               -- The gme_excpetions_tbl will be populated with the material_detail_line info.
               -- The exception_qty will be excess of reserved_qty or actual_qty from the plan_qty or wip_plan_qty.
               -- It is tbl, as we can have more than one material_detail line. It is populated only if exception exists.
               -- Populat x_exception_material_tbl.
               l_index := l_index + 1;
               x_exception_material_tbl (l_index).organization_id :=
                                  x_material_tbl (mtl_dtl_index).organization_id;
               x_exception_material_tbl (l_index).material_detail_id :=
                               x_material_tbl (mtl_dtl_index).material_detail_id;
               x_exception_material_tbl (l_index).transacted_qty :=
                                                          l_material_details_qty;
               x_exception_material_tbl (l_index).exception_qty :=
                                                                 l_exception_qty;
               x_exception_material_tbl (l_index).batch_id :=
                                         x_material_tbl (mtl_dtl_index).batch_id;
            END IF;                   /* l_actual_qty > l_material_details_qty */
         
            -- Check for reserved_qty in case batch is scaled down.
            l_reserved_qty := 0;
            /* call to gme_common_pvt.get_reserved_qty() once */
         
            -- l_reserved_qty := gme_common_pvt.get_reserved_qty(); --- Pass proper parameters .. Check compilation error.
            gme_reservations_pvt.get_reserved_qty(p_mtl_dtl_rec => x_material_tbl (mtl_dtl_index),
                                                x_reserved_qty  => l_reserved_qty,
                                                x_return_status => l_return_status);
         
            l_required_qty := NVL (l_material_details_qty, 0) - NVL (l_actual_qty, 0);
         
            IF     l_reserved_qty > l_required_qty
               AND x_material_tbl (mtl_dtl_index).phantom_type = 0
               AND x_material_tbl (mtl_dtl_index).line_type = -1 THEN
                                          -- only for ingredents, not for phantom
               l_return_status_2 := 'N';
               l_exception_qty := l_reserved_qty - l_required_qty;
               -- The gme_excpetions_tbl will be populated with the material_detail_line info.
               -- The exception_qty will be excess of reserved_qty or actual_qty from the plan_qty or wip_plan_qty.
               -- It is tbl, as we can have more than one material_detail line. It is populated only if exception exists.
               -- Populat x_exception_material_tbl.
               l_index := l_index + 1;
               x_exception_material_tbl (l_index).organization_id :=
                                  x_material_tbl (mtl_dtl_index).organization_id;
               x_exception_material_tbl (l_index).material_detail_id :=
                               x_material_tbl (mtl_dtl_index).material_detail_id;
               x_exception_material_tbl (l_index).transacted_qty :=
                                                          l_material_details_qty;
               x_exception_material_tbl (l_index).exception_qty :=
                                                                 l_exception_qty;
               x_exception_material_tbl (l_index).batch_id :=
                                         x_material_tbl (mtl_dtl_index).batch_id;
               -- Reset l_required_qty to zero because no additional move order quantity is required.
               l_required_qty := 0;
            ELSE                            /* l_reserved_qty < l_required_qty */
               l_required_qty := l_required_qty - l_reserved_qty;
                                                           -- For Future usages.
            END IF;                 /* l_reserved_qty > l_material_details_qty */
         END LOOP;          /* FOR mtl_dtl_index IN 1 .. x_material_tbl.COUNT  */
      
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
                     gme_osp.updatePOReqQuantity(p_batch_id                     => p_batch_header_rec.batch_id,
                                                  p_organization_id             => p_batch_header_rec.organization_id, 
                                                  p_batchstep_resource_id       => l_batchstep_resource_id,                            
                                                  x_return_status               => l_return_status,
                                                  x_message_list                => l_message_list,
                                                  x_message_count               => l_message_count);
                                                     
                     IF l_return_status <> fnd_api.g_ret_sts_success THEN       
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

         -- Check return_status and populate the error message.
         IF NVL (l_return_status_1, '*') = 'T' THEN
            x_return_status := 'W';
             IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
               gme_debug.put_line('l_return_status_1'||to_char(l_return_status_1));	
               gme_debug.put_line('x_return_status'||to_char(x_return_status));
            END IF;
            gme_common_pvt.log_message ('GME_ACTUAL_QTY_IS_IN_EXCESS');
         -- Navin: PENDING: NEW MESSAGE TO BE REGISTERED.: Actual quantity is in excess.
         END IF;
         
         IF NVL (l_return_status_2, '*') = 'N' THEN
            x_return_status := 'W';
            IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
               gme_debug.put_line('l_return_status_2'||to_char(l_return_status_1));	
               gme_debug.put_line('x_return_status'||to_char(x_return_status));
            END IF;
            gme_common_pvt.log_message ('GME_RESERVED_QTY_IS_IN_EXCESS');
         -- Navin: PENDING: NEW MESSAGE TO BE REGISTERED.: Reserved quantity is in excess.
         END IF;
         
         IF     NVL (l_return_status_1, '*') = 'T'
            AND NVL (l_return_status_2, '*') = 'N' THEN
            x_return_status := 'W';
            gme_common_pvt.log_message ('GME_ACTUAL_RESERV_QTY_EXCESS');
         -- Navin: PENDING: NEW MESSAGE TO BE REGISTERED.: Actual or Reserved quantity is in excess.
         END IF;
      END IF; --  IF    p_batch_header_rec.batch_status <> 0       
   EXCEPTION
      WHEN invalid_status OR expected_error THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN invalid_validity_rule OR scale_step_and_rsrc_error THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN batch_fetch_error THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN scale_phant_fail THEN
         x_return_status := l_return_status;
      WHEN scale_batch_failed OR load_trans_failed OR error_upd_default_tran OR error_updating_steps OR batch_step_fetch_err OR material_save_failed THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN clear_chg_error THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN reduce_reservation_fail THEN                    -- 4944024
         x_return_status := fnd_api.g_ret_sts_error;       -- 4944024
      WHEN trans_qty_err THEN
         gme_common_pvt.log_message ('GME_SCALE_UNMATCH_ALLOC');
      WHEN uom_conversion_error THEN
         OPEN cur_item_no (l_item_id, p_batch_header_rec.organization_id);

         FETCH cur_item_no
          INTO l_item_no;

         CLOSE cur_item_no;

         fnd_message.set_name ('GMI', 'IC_API_UOM_CONVERSION_ERROR');
         fnd_message.set_token ('ITEM_NO', l_item_no);
         fnd_message.set_token ('FROM_UOM', l_from_uom);
         fnd_message.set_token ('TO_UOM', l_to_uom);
         fnd_msg_pub.ADD;
      WHEN reschedule_batch_fail THEN
         x_return_status := 'C';
      /*                                    
      BEGIN ER 19161894  Shaliu Chen 18-JUL-2014               
      Added for OPM Step Level OSP Project  
      */            
      WHEN qty_sync_fail THEN
        gme_common_pvt.log_message('GME_QTY_SYNC_FAILED');
        x_return_status := 'W';          
      WHEN OTHERS THEN
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
         x_return_status := fnd_api.g_ret_sts_unexp_error;
   END scale_batch;

   PROCEDURE theoretical_yield_batch (
      p_batch_header_rec   IN              gme_batch_header%ROWTYPE
     ,p_scale_factor       IN              NUMBER
     ,x_return_status      OUT NOCOPY      VARCHAR2)
   IS
      /* Buffers for database reads/writes */
      l_api_name         CONSTANT VARCHAR2 (30)  := 'THEORETICAL_YIELD_BATCH';
      l_rec                       gme_material_details%ROWTYPE;
      l_material_details          gme_common_pvt.material_details_tab;
      l_material_detail_ids       gme_common_pvt.number_tab;
      l_scale_tab                 gmd_common_scale.scale_tab;
      l_in_scale_tab              gmd_common_scale.scale_tab;
      l_scale_factor              NUMBER;
      x_material_details          gme_common_pvt.material_details_tab;
      l_batch_header              gme_batch_header%ROWTYPE;
      l_routing_scale_factor      NUMBER;
      before_scale_qty            NUMBER;
      after_scale_qty             NUMBER;
      l_batch_step                gme_batch_steps%ROWTYPE;
      l_in_batch_step             gme_batch_steps%ROWTYPE;
      l_return_status             VARCHAR2 (1);
      l_count                     NUMBER;
      l_list                      VARCHAR2 (2000);
      batch_fetch_error           EXCEPTION;
      material_fetch_err          EXCEPTION;
      trans_qty_err               EXCEPTION;
      batch_step_fetch_err        EXCEPTION;
      error_updating_steps        EXCEPTION;
      material_save_failed        EXCEPTION;
      scale_step_and_rsrc_error   EXCEPTION;
      reschedule_batch_fail       EXCEPTION;

      CURSOR cur_batch_materials (v_batch_id NUMBER)
      IS
         SELECT   material_detail_id
             FROM gme_material_details
            WHERE batch_id = v_batch_id
         ORDER BY line_no;

      CURSOR cur_item_step_asso (v_batch_id NUMBER)
      IS
         SELECT DISTINCT batchstep_id
                    FROM gme_batch_step_items
                   WHERE batch_id = v_batch_id;

      CURSOR cur_get_all_steps (v_batch_id NUMBER)
      IS
         SELECT batchstep_id
           FROM gme_batch_steps
          WHERE batch_id = v_batch_id;

      CURSOR cur_get_step_status (v_material_detail_id NUMBER)
      IS
         SELECT s.batchstep_no, step_status
           FROM gme_batch_steps s, gme_batch_step_items i
          WHERE s.batchstep_id = i.batchstep_id
            AND i.material_detail_id = v_material_detail_id;

      l_material_details_qty      NUMBER;
      l_actual_qty                NUMBER;
      l_exception_qty             NUMBER;
      l_index                     NUMBER                              := 0;
      l_step_status               NUMBER (5);
      l_step_no                   NUMBER (10);
      l_message_count             NUMBER;                             -- 4944024
      l_message_list              VARCHAR2 (2000);                    -- 4944024
      reduce_reservation_fail     EXCEPTION;                          -- 4944024
      step_closed_err             EXCEPTION;
   BEGIN
      -- gme_api_grp.set_timestamp;
      l_scale_factor := p_scale_factor;

      /* Set the savepoint before proceeding */
      IF (NOT gme_batch_header_dbl.fetch_row (p_batch_header_rec
                                             ,l_batch_header) ) THEN
         RAISE batch_fetch_error;
      END IF;

      x_return_status := fnd_api.g_ret_sts_success;

      OPEN cur_batch_materials (p_batch_header_rec.batch_id);

      FETCH cur_batch_materials
      BULK COLLECT INTO l_material_detail_ids;

      CLOSE cur_batch_materials;

      FOR i IN 1 .. l_material_detail_ids.COUNT LOOP
         l_rec.material_detail_id := l_material_detail_ids (i);

         IF (NOT gme_material_details_dbl.fetch_row (l_rec, l_rec) ) THEN
            RAISE material_fetch_err;
         END IF;

         /* Protect the user from performing theoretical yield  */
         /* a batch if any of the steps associated with the material line is closed   */
         IF l_rec.line_type <> -1 THEN
            /* Lets check if the step associated with the line is closed */
            OPEN cur_get_step_status (l_rec.material_detail_id);

            FETCH cur_get_step_status
             INTO l_step_no, l_step_status;

            CLOSE cur_get_step_status;

            IF NVL (l_step_status, 0) = 4 THEN
               gme_common_pvt.log_message ('GME_STEP_CLOSED_ERR'
                                          ,'STEP_NO'
                                          ,l_step_no);
               RAISE step_closed_err;
            END IF;                         /* IF NVL(l_step_status, 0) = 4 */
         END IF;                                /* IF l_rec.line_type <> -1 */

         l_material_details (i) := l_rec;
         l_scale_tab (i).line_no := l_rec.line_no;
         l_scale_tab (i).line_type := l_rec.line_type;
         l_scale_tab (i).inventory_item_id := l_rec.inventory_item_id;

         IF l_batch_header.batch_status = 1 THEN
            l_scale_tab (i).qty := l_rec.plan_qty;
         ELSE
            l_scale_tab (i).qty := l_rec.wip_plan_qty;
         END IF;

         l_scale_tab (i).detail_uom := l_rec.dtl_um;
         l_scale_tab (i).scale_type := l_rec.scale_type;
         l_scale_tab (i).contribute_yield_ind := l_rec.contribute_yield_ind;
         l_scale_tab (i).scale_multiple := l_rec.scale_multiple;
         l_scale_tab (i).scale_rounding_variance :=
                                                 l_rec.scale_rounding_variance;
         l_scale_tab (i).rounding_direction := l_rec.rounding_direction;
      END LOOP;

      IF l_batch_header.batch_status <> 0 AND l_batch_header.poc_ind = 'Y' THEN
         before_scale_qty :=
            gme_scale_batch_pvt.get_total_qty (l_material_details
                                              ,l_batch_header);
      END IF;

      l_in_scale_tab := l_scale_tab;
      gmd_common_scale.theoretical_yield
                             (p_scale_tab          => l_in_scale_tab
                             ,p_orgn_id            => p_batch_header_rec.organization_id
                             ,x_scale_tab          => l_scale_tab
                             ,p_scale_factor       => l_scale_factor
                             ,x_return_status      => x_return_status);

      IF x_return_status = fnd_api.g_ret_sts_success THEN
         x_material_details := l_material_details;

         FOR i IN 1 .. l_material_details.COUNT LOOP
            IF l_batch_header.batch_status = 1 THEN
               x_material_details (i).plan_qty := l_scale_tab (i).qty;
            ELSE
               x_material_details (i).wip_plan_qty := l_scale_tab (i).qty;
            END IF;

            IF NOT (gme_material_details_dbl.update_row
                                                       (x_material_details (i) ) ) THEN
               RAISE material_save_failed;
            ELSE
               x_material_details (i).last_update_date :=
                                                   gme_common_pvt.g_timestamp;
               x_material_details (i).last_updated_by :=
                                                  gme_common_pvt.g_user_ident;
               x_material_details (i).last_update_login :=
                                                    gme_common_pvt.g_login_id;
            END IF;
            -- 4944024 BEGIN
            -- If there is a decrease in anticipated yield, 
            -- then reservations associated to this supply
            -- need to be decreased
            -- ===========================================
            IF x_material_details(i).line_type <> -1 THEN
              IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
                gme_debug.put_line(g_pkg_name||'.'||l_api_name||' Invoking relieve_prod_supply_resv ');
              END IF;
              gme_supply_res_pvt.relieve_prod_supply_resv  
                (p_matl_dtl_rec         => x_material_details(i)             
                ,x_msg_count            => l_message_count
                ,x_msg_data             => l_message_list                           
                ,x_return_status        => x_return_status);                    

              IF x_return_status <> FND_API.G_RET_STS_SUCCESS THEN
                RAISE reduce_reservation_fail;
              END IF;
            END IF;
            -- 4944024 END      
         END LOOP;

         IF l_batch_header.batch_status <> 0 AND l_batch_header.poc_ind = 'Y' THEN
            after_scale_qty :=
               gme_scale_batch_pvt.get_total_qty (x_material_details
                                                 ,l_batch_header);
         END IF;

         IF l_batch_header.batch_status <> 0 AND l_batch_header.poc_ind = 'Y' THEN
            IF l_batch_header.automatic_step_calculation = 1 THEN
               IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
                  gme_debug.put_line ('GME:THEOR: Batch has ASQC');
               END IF;

               FOR l_cur_item_step_asso IN
                  cur_item_step_asso (l_batch_header.batch_id) LOOP
                  l_batch_step.batchstep_id :=
                                            l_cur_item_step_asso.batchstep_id;
                  l_in_batch_step := l_batch_step;
                  gme_update_step_qty_pvt.update_step_qty (l_in_batch_step
                                                          ,l_count
                                                          ,l_list
                                                          ,l_return_status
                                                          ,l_batch_step);

                  IF l_return_status <> x_return_status THEN
                     RAISE error_updating_steps;
                  END IF;

                  IF NOT (gme_batch_steps_dbl.fetch_row (l_batch_step
                                                        ,l_batch_step) ) THEN
                     RAISE batch_step_fetch_err;
                  END IF;
               END LOOP;
            ELSE
               IF after_scale_qty IS NOT NULL
                  AND before_scale_qty IS NOT NULL THEN
                  l_routing_scale_factor :=
                                           after_scale_qty / before_scale_qty;
               ELSE
                  l_routing_scale_factor := 1;
               END IF;

               IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
                  gme_debug.put_line
                               (   'GME_THEOR:NOT ASQC:Routing scale factor '
                                || l_routing_scale_factor);
               END IF;

               scale_step_and_rsrc
                            (p_batch_id                  => l_batch_header.batch_id
                            ,p_routing_scale_factor      => l_routing_scale_factor
                            ,x_return_status             => x_return_status);

               IF x_return_status <> fnd_api.g_ret_sts_success THEN
                  RAISE scale_step_and_rsrc_error;
               END IF;
            END IF;                          /*  automatic_step_calculation */
         END IF;                                            /* for new batch*/

         IF x_return_status <> fnd_api.g_ret_sts_success THEN
            RAISE trans_qty_err;
         END IF;
      END IF;

      /****
          START 17Feb05 Navin Sinha Made changes for GME_Scale_Batch_Theoretical_Yield_TD.
      *****/
      l_index := 0;
      l_return_status := NULL;

      -- Loop through all the records of x_material_details
      FOR mtl_dtl_index IN 1 .. x_material_details.COUNT LOOP
         -- Based on the batch status of pending or WIP, the plan_qty or wip_plan_qty will be considered.
         IF p_batch_header_rec.batch_status = 1 THEN               -- Pending
            l_material_details_qty :=
                         NVL (x_material_details (mtl_dtl_index).plan_qty, 0);
         ELSIF p_batch_header_rec.batch_status = 2 THEN                 -- WIP
            l_material_details_qty :=
                     NVL (x_material_details (mtl_dtl_index).wip_plan_qty, 0);
         END IF;

         -- In case of scale down the batch, the user will be informed for the excess actual_qty
         -- compared to plan_qty or wip_plan_qty.
         -- The return staus to be assigned in such case is: T- if actual_qty is in excess.
         l_actual_qty := 0;
         l_actual_qty :=
                        NVL (x_material_details (mtl_dtl_index).actual_qty, 0);

         IF     l_actual_qty > l_material_details_qty
            AND x_material_details (mtl_dtl_index).line_type = 1 THEN
                                                          -- only for products
            l_return_status := 'T';
         END IF;                   /* l_actual_qty > l_material_details_qty */
      END LOOP;      /* FOR mtl_dtl_index IN 1 .. x_material_details.COUNT  */

      IF NVL (l_return_status, '*') = 'T' THEN
         x_return_status := l_return_status;
         gme_common_pvt.log_message ('GME_ACTUAL_QTY_IS_IN_EXCESS');
      -- Navin: PENDING: NEW MESSAGE TO BE REGISTERED.: Actual quantity is in excess.
      END IF;
   /****
    END 17Feb05 Navin Sinha Made changes for GME_Scale_Batch_Theoretical_Yield_TD.
   *****/
   EXCEPTION
      WHEN batch_fetch_error OR material_fetch_err OR material_save_failed OR batch_step_fetch_err THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN step_closed_err OR scale_step_and_rsrc_error OR error_updating_steps THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN trans_qty_err THEN
         gme_common_pvt.log_message ('GME_THEOYLD_UNMATCH_ALLOC');
      WHEN reduce_reservation_fail THEN                    -- 4944024
        IF nvl(g_debug, gme_debug.g_log_procedure + 1) <= gme_debug.g_log_procedure THEN
          gme_debug.put_line(g_pkg_name||'.'||l_api_name||' Failure after relieve_prod_supply_resv ');
        END IF;
      WHEN OTHERS THEN
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
         x_return_status := fnd_api.g_ret_sts_unexp_error;
   END theoretical_yield_batch;

   FUNCTION get_total_qty (
      p_material_tab       IN   gme_common_pvt.material_details_tab
     ,p_batch_header_rec   IN   gme_batch_header%ROWTYPE)
      RETURN NUMBER
   IS
      CURSOR get_rout_uom (v_batch_id NUMBER)
      IS
         SELECT routing_uom
           FROM gmd_routings_b
          WHERE routing_id = (SELECT routing_id
                                FROM gme_batch_header
                               WHERE batch_id = v_batch_id);

      CURSOR cur_item_no (v_item_id NUMBER, v_org_id NUMBER)
      IS
         SELECT concatenated_segments
           FROM mtl_system_items_kfv
          WHERE inventory_item_id = v_item_id AND organization_id = v_org_id;

      l_api_name   CONSTANT VARCHAR2 (30)              := 'GET_TOTAL_QTY';
      l_uom                 sy_uoms_mst.um_code%TYPE;
      l_qty                 NUMBER                     := 0;
      l_item_id             NUMBER;
      l_item_no             VARCHAR2 (32);
      l_from_uom            sy_uoms_mst.um_code%TYPE;
      l_to_uom              sy_uoms_mst.um_code%TYPE;
      l_rout_um             sy_uoms_mst.um_code%TYPE;
      x_return_status       VARCHAR2 (1);
      conversion_failure    EXCEPTION;
      l_plan_wipplan_qty    NUMBER                     := 0;
      l_temp_qty            NUMBER;
      l_um_type             sy_uoms_typ.um_type%TYPE;
      bad_yield_type        EXCEPTION;
      l_batch_header        gme_batch_header%ROWTYPE;
   BEGIN
      x_return_status := fnd_api.g_ret_sts_success;
      --l_um_type := fnd_profile.VALUE ('FM_YIELD_TYPE');
      l_batch_header := p_batch_header_rec;

      OPEN get_rout_uom (l_batch_header.batch_id);

      FETCH get_rout_uom
       INTO l_rout_um;

      CLOSE get_rout_uom;

      FOR l_row_count IN 1 .. p_material_tab.COUNT LOOP
         IF p_material_tab (l_row_count).line_type > 0 THEN
            IF p_batch_header_rec.batch_status = 1 THEN
               l_plan_wipplan_qty := p_material_tab (l_row_count).plan_qty;
            ELSE
               l_plan_wipplan_qty :=
                                    p_material_tab (l_row_count).wip_plan_qty;
            END IF;

            IF p_material_tab (l_row_count).dtl_um = l_rout_um THEN
               l_qty := l_qty + l_plan_wipplan_qty;
            ELSE
               l_temp_qty :=
                  inv_convert.inv_um_convert
                              (p_material_tab (l_row_count).inventory_item_id
                              ,5
                              ,l_plan_wipplan_qty
                              ,p_material_tab (l_row_count).dtl_um
                              ,l_rout_um
                              ,NULL
                              ,NULL);

               IF l_temp_qty < 0 THEN
                  l_item_id := p_material_tab (l_row_count).inventory_item_id;
                  l_from_uom := p_material_tab (l_row_count).dtl_um;
                  l_to_uom := l_rout_um;
                  RAISE conversion_failure;
               ELSE
                  l_qty := l_qty + l_temp_qty;
               END IF;
            END IF;
         END IF;
      END LOOP;

      RETURN l_qty;
   EXCEPTION
      WHEN conversion_failure THEN
         IF l_item_no IS NULL THEN
            OPEN cur_item_no (l_item_id, p_batch_header_rec.organization_id);

            FETCH cur_item_no
             INTO l_item_no;

            CLOSE cur_item_no;
         END IF;

         fnd_message.set_name ('GMI', 'IC_API_UOM_CONVERSION_ERROR');
         fnd_message.set_token ('ITEM_NO', l_item_no);
         fnd_message.set_token ('FROM_UOM', l_from_uom);
         fnd_message.set_token ('TO_UOM', l_to_uom);
         fnd_msg_pub.ADD;

         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line
                        (   'SCALE BATCH:Get_total_qty:Conversion error for '
                         || l_item_no
                         || ' From '
                         || l_from_uom
                         || ' To '
                         || l_to_uom);
         END IF;
      WHEN OTHERS THEN
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);

         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line (SQLERRM);
         END IF;

         x_return_status := 'U';
   END get_total_qty;

   PROCEDURE scale_step_and_rsrc (
      p_batch_id               IN              gme_batch_header.batch_id%TYPE
     ,p_routing_scale_factor   IN              NUMBER
     ,x_return_status          OUT NOCOPY      VARCHAR2)
   IS
      /* Local variables */
      l_api_name         CONSTANT VARCHAR2 (30)      := 'SCALE_STEP_AND_RSRC';
      l_auto_step_calc            NUMBER (5);
      l_batch_step                gme_batch_steps%ROWTYPE;
      l_in_batch_step             gme_batch_steps%ROWTYPE;
      l_batchstep_resource_ids    gme_common_pvt.number_tab;
      l_gme_batchstep_resources   gme_batch_step_resources%ROWTYPE;
      l_gme_resource_txns_gtmp    gme_resource_txns_gtmp%ROWTYPE;
      l_return_status             VARCHAR2 (1);
      l_message_count             NUMBER;
      l_message_list              VARCHAR2 (2000);
      l_charge                    NUMBER;
      l_prev_charge               NUMBER;
      l_update_inventory_ind      VARCHAR2 (1);
      batch_step_fetch_err        EXCEPTION;
      error_updating_steps        EXCEPTION;
      fetch_res_error             EXCEPTION;
      update_res_error            EXCEPTION;
      fetch_res_txn_error         EXCEPTION;
      update_res_txn_error        EXCEPTION;
      error_updating_step_qty     EXCEPTION;
      activity_rsrc_fetch_error   EXCEPTION;

      CURSOR cur_get_batch_info
      IS
         SELECT automatic_step_calculation, update_inventory_ind
           FROM gme_batch_header
          WHERE batch_id = p_batch_id;

      CURSOR cur_get_all_steps (v_batch_id NUMBER)
      IS
         SELECT batchstep_id
           FROM gme_batch_steps
          WHERE batch_id = v_batch_id
            AND step_status = 1; -- Bug 18762054  Scale only pending steps.

      CURSOR cur_get_res (v_step_id gme_batch_steps.batchstep_id%TYPE)
      IS
         SELECT batchstep_resource_id
           FROM gme_batch_step_resources
          WHERE batchstep_id = v_step_id AND scale_type <> 0;

      CURSOR cur_get_res_txns (
         v_batch_id   gme_batch_header.batch_id%TYPE
        ,v_res_id     gme_batch_step_resources.batchstep_resource_id%TYPE)
      IS
         SELECT poc_trans_id
           FROM gme_resource_txns_gtmp
          WHERE doc_id = v_batch_id AND line_id = v_res_id;

      l_cur_get_all_steps         cur_get_all_steps%ROWTYPE;
      l_cur_get_res               cur_get_res%ROWTYPE;
      l_cur_get_res_txns          cur_get_res_txns%ROWTYPE;
   BEGIN
      x_return_status := fnd_api.g_ret_sts_success;

      OPEN cur_get_batch_info;

      FETCH cur_get_batch_info
       INTO l_auto_step_calc, l_update_inventory_ind;

      CLOSE cur_get_batch_info;

      FOR l_cur_get_all_steps IN cur_get_all_steps (p_batch_id) LOOP
         l_batch_step.batchstep_id := l_cur_get_all_steps.batchstep_id;

         IF NOT (gme_batch_steps_dbl.fetch_row (l_batch_step, l_batch_step) ) THEN
            RAISE batch_step_fetch_err;
         END IF;

         -- Round the step qtys to 5 decimal places.
         /* We should scale the step quantities if the step is pending or if the */
         /* step is certified and the batch automatic step calculation is off    */
         IF    (l_batch_step.step_status = 1)
            OR (l_batch_step.step_status = 3 AND l_auto_step_calc = 0) THEN
            IF l_batch_step.step_status = 1 THEN
               l_batch_step.plan_step_qty :=
                  ROUND (l_batch_step.plan_step_qty * p_routing_scale_factor
                        ,5);
               l_batch_step.plan_mass_qty :=
                  ROUND (l_batch_step.plan_mass_qty * p_routing_scale_factor
                        ,5);
               l_batch_step.plan_volume_qty :=
                  ROUND (l_batch_step.plan_volume_qty * p_routing_scale_factor
                        ,5);
            ELSE
               l_batch_step.actual_step_qty :=
                  ROUND (l_batch_step.actual_step_qty * p_routing_scale_factor
                        ,5);
               l_batch_step.actual_mass_qty :=
                  ROUND (l_batch_step.actual_mass_qty * p_routing_scale_factor
                        ,5);
               l_batch_step.actual_volume_qty :=
                  ROUND (  l_batch_step.actual_volume_qty
                         * p_routing_scale_factor
                        ,5);
               l_prev_charge := l_batch_step.actual_charges;
               /* Let us calculate actual charges based on the change to the qty */
               gme_update_step_qty_pvt.calc_charge
                                 (p_step_id            => l_batch_step.batchstep_id
                                 ,p_mass_qty           => l_batch_step.actual_mass_qty
                                 ,p_vol_qty            => l_batch_step.actual_volume_qty
                                 ,x_charge             => l_charge
                                 ,x_return_status      => l_return_status);

               IF l_return_status = x_return_status THEN
                  l_batch_step.actual_charges := l_charge;
               END IF;
            END IF;

            IF NOT (gme_batch_steps_dbl.update_row (l_batch_step) ) THEN
               RAISE error_updating_steps;
            END IF;

            IF l_batch_step.step_status = 1 THEN
               l_in_batch_step := l_batch_step;
               gme_update_step_qty_pvt.update_step_qty
                            (p_batch_step_rec            => l_in_batch_step
                            ,x_batch_step_rec            => l_batch_step
                            ,x_message_count             => l_message_count
                            ,x_message_list              => l_message_list
                            ,x_return_status             => l_return_status
                            ,p_routing_scale_factor      => p_routing_scale_factor);

               IF l_return_status <> x_return_status THEN
                  RAISE error_updating_step_qty;
               END IF;
            ELSE
               /* We cannot invoke the update step qty API for certified non asqc steps */
               /* as it would not update the resources if the actuals already populated */
               /* Fetch all the resources associated with the step */
               OPEN cur_get_res (l_batch_step.batchstep_id);

               FETCH cur_get_res
               BULK COLLECT INTO l_batchstep_resource_ids;

               CLOSE cur_get_res;

               /* Fetch all the activity resources details */
               FOR i IN 1 .. l_batchstep_resource_ids.COUNT LOOP
                  l_gme_batchstep_resources.batchstep_resource_id :=
                                                 l_batchstep_resource_ids (i);

                  IF NOT (gme_batch_step_resources_dbl.fetch_row
                                                   (l_gme_batchstep_resources
                                                   ,l_gme_batchstep_resources) ) THEN
                     RAISE activity_rsrc_fetch_error;
                  END IF;

                  -- Round the resource usage to 5 decimal places.
                  l_gme_batchstep_resources.actual_rsrc_qty :=
                     ROUND (  l_gme_batchstep_resources.actual_rsrc_qty
                            * p_routing_scale_factor
                           ,5);

                  IF l_gme_batchstep_resources.scale_type = 1 THEN
                     l_gme_batchstep_resources.actual_rsrc_usage :=
                        ROUND (  l_gme_batchstep_resources.actual_rsrc_usage
                               * p_routing_scale_factor
                              ,5);
                  ELSIF (l_gme_batchstep_resources.scale_type = 2) THEN
                     IF     NVL (l_gme_batchstep_resources.actual_rsrc_usage
                                ,0) > 0
                        AND NVL (l_prev_charge, 0) > 0 THEN
                        l_gme_batchstep_resources.actual_rsrc_usage :=
                           ROUND
                              ( (   (  l_gme_batchstep_resources.actual_rsrc_usage
                                     / l_prev_charge)
                                 * l_batch_step.actual_charges)
                              ,5);
                     END IF;
                  END IF;

                  IF l_update_inventory_ind = 'Y' THEN
                     gme_update_step_qty_pvt.adjust_actual_usage
                        (p_batch_step_resources_rec      => l_gme_batchstep_resources
                        ,x_return_status                 => l_return_status);

                     IF l_return_status <> x_return_status THEN
                        RAISE update_res_txn_error;
                     END IF;
                  END IF;                /* IF l_update_inventory_ind = 'Y' */

                  IF NOT (gme_batch_step_resources_dbl.update_row
                                                    (l_gme_batchstep_resources) ) THEN
                     RAISE update_res_error;
                  END IF;
               END LOOP;      /* FOR i IN 1..l_batchstep_resource_ids.COUNT */
            END IF;
         END IF;                    /* IF l_batch_step.step_status = 1 THEN */
      END LOOP;
   EXCEPTION
      WHEN error_updating_steps OR activity_rsrc_fetch_error OR update_res_error OR batch_step_fetch_err THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN error_updating_step_qty OR update_res_txn_error THEN
         x_return_status := l_return_status;
      WHEN OTHERS THEN
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);

         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line (SQLERRM);
         END IF;

         x_return_status := fnd_api.g_ret_sts_unexp_error;
   END scale_step_and_rsrc;
END gme_scale_batch_pvt;
/

COMMIT ;
EXIT;
--SHOW errors;
