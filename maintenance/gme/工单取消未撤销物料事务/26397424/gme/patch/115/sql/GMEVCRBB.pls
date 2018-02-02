/* +======================================================================+ */
/* |    Copyright (c) 2005, 2015 Oracle and/or its affiliates.           | */
/* |                         All rights reserved.                         | */
/* |                           Version 12.0.0                             | */
/* +======================================================================+ */
REM dbdrv: sql ~PROD ~PATH ~FILE none none none package &phase=plb \
REM dbdrv: checkfile(120.33.12010000.26=120.54.12020000.9)(120.23.12000000.37=120.33.12010000.22)(120.23.12000000.25=120.33.12010000.10)(115.96=120.5):~PROD:~PATH:~FILE
SET VERIFY OFF
WHENEVER SQLERROR EXIT FAILURE ROLLBACK;
WHENEVER OSERROR EXIT FAILURE ROLLBACK;

CREATE OR REPLACE PACKAGE BODY gme_create_batch_pvt AS
/* $Header: GMEVCRBB.pls 120.54.12020000.9 2015/06/15 16:44:19 gmurator ship $ */
   g_debug               VARCHAR2 (5)
                               := NVL (fnd_profile.VALUE ('AFLOG_LEVEL'), 99);
   g_pkg_name   CONSTANT VARCHAR2 (30) := 'GME_CREATE_BATCH_PVT';

/***********************************************************/ 
/* Oracle Process Manufacturing Process Execution APIs     */
/*                                                         */
/* File Name: GMEVCRBB.pls                                 */
/* Contents:  Private layer for batch creation API         */
/* HISTORY                                                 */
/* SivakumarG 05-APR-2006                                  */
/*  put some debug messages in edit text copy code         */
/*  sunitha bug # 5484529 selecting the sum of the plan    */
/*  quantity without converting it to the UOM that the user entered */
/*  while creating the batch into l_temp_qty .Convert the  */
/*  p_batch_size(user entered product plan quantity        */
/*  to the Routing uom and then Compare it with l_temp_qty.*/
/*  Kapil M. Bug# 5458674                                  */
/*  Changes to Support LCF Batches for GMO                 */
/*  Archana Mundhe Bug 5763818 Modified the code to use    */
/*  ERES constants that are added to gme_common_pvt instead*/
/*  of using the hardcoded ERES event names such as        */
/*  'oracle.apps.gme...'                                   */
/*  Swapna K Bug#6398619 calls to validate_wip_entity are changed */
/*   for the manual doc ordering                             */

/* Please use this new format for comments going forward.

  G. Muratore     12/26/2007  Bug 6665602 Back out fix for 5484529. This fix was 
     incorrect. The select fixed in 5484529 was not correct because it did not convert 
     all product lines to one common uom. Therefore the summation is invalid.
     Bug 5484529 will ahve to be re-addressed in a different way.
  Swapna K Bug#6738476 Added parameter,p_batch_header_rec
    to the procedure call,gme_phantom_pvt.create_phantom 
 10-JAN-2008 Rajesh Patangya Bug # 6752637     
 MTQ Quantity should be calculated based on product in place of just copy from 
 the routing, This is required by PS engine, New Function UPDATE_STEP_MTQ added  

  G. Muratore     07/31/2008  Bug 7265006 Correct fix for 5512352. 
     This fix was incorrect as to where the new condition was used when trying to optimize shortage checking.
     Moved g_no_phant_short_check to shortage check condition as it was originally intended
     for shortage checking only. Unfortunately it stopped all the other logic for phantom batches.
     Now invisible move orders and high level reservations will get created for phantom batches.

  G. Muratore     08/18/2008  Bug 7284242  
     This fix was to correct the algorithm for deriving the scale factor when creating a batch by
     'PRODUCT'.  Prior to this fix the code did not treat fixed scale products differently from
     linear scale products. 
     
  K. swapna       10/21/2008  Bug 7493614
     Moved the call to wf_event.raise call to the end of the create_batch
      procedure as the sample can be created at the end of the batch creation.

  G. Muratore     18-NOV-2008  Bug 7565054 - Rework of 7284242  
     This fix was to correct the algorithm for deriving the scale factor when creating a batch by
     'PRODUCT'. When a batch is created from APS, the Quantity being passed is in reference to the 
     primary product line only. Create batch did not treat it the same way if the same item existed in 
     multiple product lines. Added parameter p_sum_all_prod_lines to the procedure create_batch.
     Parameter values: "A" means sum all the lines i.e. original functionality. "S" means single product summation. 
     The original fix dealt with fixed scale type only which we now know was not the real issue. The
     New paramater will allow APS a way to tell this code to consider only the primary product line.   
     In the future this could be extended to the forms since the parameter now exists.    

  G. Muratore     25-NOV-2008  Bug 7578476 - Some Rework of 4917631 also done. 
     This fix was to correct the algorithm for deriving the original_primary_qty. The user reported
     that it was not considering scrap_factor. That in fact was an issue but also it was not calculated 
     properly if the dtl uom was not the primary, in some cases. Changed the code to correct all areas where 
     original_primary_qty is derived. Note: Removed some redundant cursors and code. Also, made use of
     l_item_masters table array properly to get the correct conversions.

  G. Muratore     05-FEB-2009  Bug 7830838  
     Use the same value of fixed_process_loss_applied from the parent batch so that FPL
     gets properly applied, or not applied, to the phantom batch based on user settings.
     Also includes fix for Bug 7656415. This rounds the plan_qty to make sure it adheres to 5 decimal places.

  G. Muratore     09-FEB-2009  Bug 8226667  
     Changed select from inline calculation to using variables and making sure that 
     denominator cannot be zero. Procedure: update_step_mtq     

  G. Muratore     26-FEB-2009  Bug 7710435  
     Changed call to gme_material_details_dbl.update_row which now accepts a p_called_by parameter. This allows
     us to disregard the timestamp used for record locking. It is not required to lock records, during batch create,  
     which are not committed to the database yet. Sometimes, the timestamp in the database record did not match
     the one in the memory table and therefore updates were failing. Also refetch the material records after the
     last call to update_row to update the memory tables. PROCEDURE: create_batch.   
     
  G. Muratore     09-SEP-2010  Bug 10086349  
     Use precision of 4 instead of 5 to make comparison against batch size requested. 
     This is necessary because of potential precision loss.     
     
  G. Muratore     21-DEC-2010  Bug 10379034 - REWORK OF 10086349  
     Change the comparison against batch size requested so we do not rely on potential rounding issues.      
     
  G. Muratore     24-JAN-2011  Bug 10624995   
     Rearrange logic as to handle integer scale items for non scaled batches during batch creation.
     
  G. Muratore     04-OCT-2011  Bug 12909216 - TWEEK OF 10379034  
     Avoid divide by zero error when qty is zero. This will allow zero phantom batches to be created.   
 
  A. Mishra       09-DEC-2011  Bug 13256866 - 
     Adding the who columns for every update statement.
          
  G. Muratore     09-DEC-2011  Bug 11815699  
     Check to make sure the item is not inactive for process execution based on item status code.                 
          
  G. Muratore     28-FEB-2012  Bug 13785754  
     Changed logic to try 3 times if necessary to get a unique batch number for auto doc numbering.
     This change was needed for customers firing off creation of many batches by many users. Periodically
     there was a key constraint issue on the wip entities table.
          
  G. Muratore     20-MAR-2012  Bug 13811289  
     Make sure all items returned are valid for the specific organization.  

  G. Muratore     23-MAR-2012  Bug 13785754/13815190  
     Changed logic to try 20 times if necessary to get a unique batch number for auto doc numbering.
     
  QZENG           12-Mar-2013  Bug 16457668
     No need to check batch no for verified in bulk validation when used from batch open interface

  G. Muratore     21-MAR-2013  Bug 16474091  
     Changed change precision from 5 to 32 for totals that are used in deriving routing scale factor.

  G. Muratore     27-SEP-2013  Bug 17433833    
     Restructured code to generate the batch number later in the logic.
     
  Shaliu Chen     18-JUL-2014  ER 19161894  
     Modify create_batch to invoke requisition creation program if batch include OSP step  
     
  Shaliu Chen     02-APR-2015  ER 20809749
     Modify create batch to support multiple osp steps.  

  G. Muratore     15-Jun-2015  Bug 21187113  
     Restructured code to bypass create explicit move order if auto detail is also activated.
     If both parameters are set, then we will run auto detail first followed by real picking.
************************************************************************************************************  */

/************************************************************
*                                                           *
* CONSTRUCT_BATCH_HEADER                                    *
*                                                           *
************************************************************/
   FUNCTION construct_batch_header (
      p_batch_header_rec   IN              gme_batch_header%ROWTYPE
     ,x_batch_header_rec   OUT NOCOPY      gme_batch_header%ROWTYPE)
      RETURN BOOLEAN
   IS
      l_count               NUMBER;
      l_api_name   CONSTANT VARCHAR2 (30) := 'CONSTRUCT_BATCH_HEADER';
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      /* Create defaults for mandatory columns that have not been specified */
      l_count := gme_common_pvt.g_error_count;
      x_batch_header_rec := p_batch_header_rec;

      IF x_batch_header_rec.organization_id IS NULL THEN
         fnd_message.set_name ('INV', 'INV_INT_ORGCODE');
         fnd_msg_pub.ADD;
      END IF;

      x_batch_header_rec.prod_id := NVL (x_batch_header_rec.prod_id, 0);
      x_batch_header_rec.prod_sequence :=
                                     NVL (x_batch_header_rec.prod_sequence, 0);
      x_batch_header_rec.plan_start_date :=
          NVL (x_batch_header_rec.plan_start_date, gme_common_pvt.g_timestamp);
      x_batch_header_rec.plan_cmplt_date :=
          NVL (x_batch_header_rec.plan_cmplt_date, gme_common_pvt.g_timestamp);
      x_batch_header_rec.due_date :=
                 NVL (x_batch_header_rec.due_date, gme_common_pvt.g_timestamp);
      x_batch_header_rec.batch_status := gme_common_pvt.G_BATCH_PENDING;
      x_batch_header_rec.priority_value :=
                                    NVL (x_batch_header_rec.priority_value, 0);
      x_batch_header_rec.priority_code :=
                                     NVL (x_batch_header_rec.priority_code, 0);
      x_batch_header_rec.print_count := 0;
      x_batch_header_rec.batch_close_date := NULL;
      x_batch_header_rec.actual_cost_ind := 'N';
      x_batch_header_rec.gl_posted_ind := 0;
      x_batch_header_rec.delete_mark := 0;
      x_batch_header_rec.automatic_step_calculation := 1;

      IF l_count <> gme_common_pvt.g_error_count THEN
         RETURN FALSE;
      ELSE
         RETURN TRUE;
      END IF;
   EXCEPTION
      WHEN OTHERS THEN
         IF g_debug <= gme_debug.g_log_unexpected THEN
            gme_debug.put_line (   'When others exception in '
                                || g_pkg_name
                                || '.'
                                || l_api_name
                                || ' Error is '
                                || SQLERRM);
         END IF;

         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
         RETURN FALSE;
   END construct_batch_header;

   PROCEDURE validate_wip_entity (p_organization_id  IN   NUMBER,
                                  p_batch_no         IN   VARCHAR2,
                                  x_return_status    OUT NOCOPY VARCHAR2) IS
      l_api_name    CONSTANT VARCHAR2 (30) := 'VALIDATE_WIP_ENTITY';
      l_temp        NUMBER;
      duplicate_wip_entity EXCEPTION;
      CURSOR Cur_wip_entity IS
        SELECT 1
        FROM   DUAL
        WHERE  EXISTS (SELECT 1
                       FROM   wip_entities
                       WHERE  organization_id = p_organization_id
                              AND wip_entity_name = p_batch_no);
   BEGIN
     x_return_status := fnd_api.g_ret_sts_success;
     OPEN Cur_wip_entity;
     FETCH Cur_wip_entity INTO l_temp;
     IF (Cur_wip_entity%FOUND) THEN
        CLOSE Cur_wip_entity;           
        RAISE duplicate_wip_entity;
     END IF;
     CLOSE Cur_wip_entity;
   EXCEPTION
      WHEN duplicate_wip_entity THEN
        x_return_status := fnd_api.g_ret_sts_error;
        gme_common_pvt.log_message('GME_WIP_ENTITY_EXISTS');
      WHEN OTHERS THEN
         IF g_debug <= gme_debug.g_log_unexpected THEN
            gme_debug.put_line (   'When others exception in '
                                || g_pkg_name
                                || '.'
                                || l_api_name
                                || ' Error is '
                                || SQLERRM);
         END IF;
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
         x_return_status := fnd_api.g_ret_sts_unexp_error;
   END validate_wip_entity;
   
   PROCEDURE create_batch (
      p_validation_level         IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_batch_header_rec         IN              gme_batch_header%ROWTYPE
     ,p_batch_size               IN              NUMBER
     ,p_batch_size_uom           IN              VARCHAR2
     ,p_creation_mode            IN              VARCHAR2
     ,p_ignore_qty_below_cap     IN              VARCHAR2
            DEFAULT fnd_api.g_true
     ,p_use_workday_cal          IN              VARCHAR2
     ,p_contiguity_override      IN              VARCHAR2
     ,p_is_phantom               IN              VARCHAR2 DEFAULT 'N'
     ,p_sum_all_prod_lines       IN              VARCHAR2 DEFAULT 'A'
     ,p_use_least_cost_validity_rule     IN      VARCHAR2 := fnd_api.g_false          
     ,x_batch_header_rec         OUT NOCOPY      gme_batch_header%ROWTYPE
     ,x_exception_material_tbl   OUT NOCOPY      gme_common_pvt.exceptions_tab
     ,x_return_status            OUT NOCOPY      VARCHAR2)
   IS
      l_in_batch_header              gme_batch_header%ROWTYPE;
      l_formula_master               fm_form_mst%ROWTYPE;
      l_formula_material             fm_matl_dtl%ROWTYPE;
      l_recipe_validity_rule         gmd_recipe_validity_rules%ROWTYPE;
      l_item_master                  mtl_system_items_kfv%ROWTYPE;
      l_gme_material_detail          gme_material_details%ROWTYPE;
      l_text_header                  gme_text_header%ROWTYPE;
      l_text_string                  gme_text_table.text%TYPE;
      l_material_details             gme_common_pvt.material_details_tab;
      p_material_details             gme_common_pvt.material_details_tab;
      l_in_material_details          gme_common_pvt.material_details_tab;
      l_material_details_in          gme_material_details%ROWTYPE;
      l_item_masters                 gme_common_pvt.item_masters_tab;
      l_text_table                   gme_common_pvt.text_tab;
      l_recipe                       gmd_recipes%ROWTYPE;
      l_from_uom                     mtl_units_of_measure.uom_code%TYPE;
      l_to_uom                       mtl_units_of_measure.uom_code%TYPE;
      l_prim_item_um                 mtl_units_of_measure.uom_code%TYPE;
      l_prim_prod_um                 mtl_units_of_measure.uom_code%TYPE;
      l_contig_period_tbl            gmp_calendar_api.contig_period_tbl;
      l_api_name            CONSTANT VARCHAR2 (30)          := 'CREATE_BATCH';
      l_uom                          VARCHAR2 (4);
      l_return_status                VARCHAR2 (1);
      l_message_list                 VARCHAR2 (2000);
      l_production_um                VARCHAR2 (4);
      l_primaries                    VARCHAR2 (15);
      l_item_no                      VARCHAR2 (2000);
      l_prefix                       VARCHAR2 (32);
      l_enhanced_pi_ind              VARCHAR2 (1);
      l_dispense_required            VARCHAR2 (1);
      l_number_of_product_lines      NUMBER;
      l_number_of_byproduct_lines    NUMBER;
      l_number_of_ingredient_lines   NUMBER;
      l_number_of_formula_lines      NUMBER;
      l_number_of_text_lines         NUMBER;
      l_text_code                    NUMBER;
      l_count                        NUMBER                              := 0;
      l_message_count                NUMBER;
      l_doc_numbering                NUMBER;
      l_doc_count                    NUMBER;  -- 13785754                  
      l_process_loss                 NUMBER;
      l_return_code                  NUMBER;
      l_total_input_qty              NUMBER;
      l_total_output_qty             NUMBER;         
      l_total_output_qty_b           NUMBER;
      l_total_output_qty_c           NUMBER;
      l_production_rule_qty          NUMBER;
      l_requested_qty                NUMBER;
      l_temp_qty                     NUMBER;
      l_temp_line_no                 NUMBER;

      phantom_found                  NUMBER;   -- Bug 17433833   

      l_item_id                      NUMBER;
      l_inv_trans_count              NUMBER;
      l_rsc_trans_count              NUMBER;
      l_prim_prod_qty                NUMBER;
      l_error_count                  NUMBER;
      l_error_count_after            NUMBER;
      l_msg_data                     VARCHAR2(2000);
      l_duration                     NUMBER;
      l_cal_count                    NUMBER;
      l_cont_ind                     NUMBER;
      l_config_id                               NUMBER;
      l_excp_tbl_ptr                 NUMBER; -- nsinghi bug#5200395
      l_return                       BOOLEAN;
      l_no_prod_rule_found           BOOLEAN;
      l_prim_prod_found              BOOLEAN;
      l_exception_material_tbl       gme_common_pvt.exceptions_tab;
      l_trolin_tbl                   inv_move_order_pub.trolin_tbl_type;
      l_phantom_exc_material_tbl     gme_common_pvt.exceptions_tab; -- nsinghi bug#5200395
      l_phantom_exc_material_tbl_out gme_common_pvt.exceptions_tab; -- nsinghi bug#5200395
      l_batch_header_rec             gme_batch_header%ROWTYPE;
      l_batchstep_rec                gme_batch_steps%ROWTYPE;
      x_batchstep_rec                gme_batch_steps%ROWTYPE;
      l_in_material_detail           gme_material_details%ROWTYPE;
      l_item_substituted             BOOLEAN;
      l_curr_item_substituted        BOOLEAN;
      l_ingred_req_date              DATE;
      material_dtl_fetch_failure     EXCEPTION;

      --FPBug#4351032      
      l_formula_tbl                  gmdfmval_pub.formula_detail_tbl;
      l_new_item_rec                 mtl_system_items_kfv%ROWTYPE;
      --5698727
      l_in_batch_header1             gme_batch_header%ROWTYPE;
      l_doc_timestamp                VARCHAR2(50);
      l_assignment_type              NUMBER;
      l_document_no                  gme_batch_header.batch_no%TYPE;
      --5698727
      invalid_item_substitute        EXCEPTION; 
      l_gme_batch_header GME_BATCH_HEADER%ROWTYPE;

      -- Bug 10624995
      l_change_made                 NUMBER;
      l_scale_rec                   gmd_common_scale.scale_rec;
      l_scale_rec_out               gmd_common_scale.scale_rec;
      error_in_integer_scale        EXCEPTION;
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

      CURSOR recipe_validity_rule_cursor (v_recipe_validity_rule_id NUMBER)
      IS
         SELECT *
           FROM gmd_recipe_validity_rules
          WHERE recipe_validity_rule_id = NVL (v_recipe_validity_rule_id, -1);

      CURSOR recipe_cursor (v_recipe_id NUMBER)
      IS
         SELECT *
           FROM gmd_recipes
          WHERE recipe_id = v_recipe_id;

      CURSOR formula_details_cursor (v_recipe_id NUMBER)
      IS
         SELECT   a.*
             FROM fm_matl_dtl a, gmd_recipes_b b
            WHERE a.formula_id = b.formula_id AND b.recipe_id = v_recipe_id
         ORDER BY line_no;

      CURSOR fm_text_tbl_cursor (v_text_code NUMBER)
      IS
         SELECT   *
             FROM fm_text_tbl
            WHERE text_code = NVL (v_text_code, -1)
         ORDER BY line_no;
         
      -- pawan kumar changed this cursor for bug 4917631
      
      -- Bug 11815699 - Add inventory_item_status_code column
      -- Bug 7578476 - Fetch primary uom code also.
      CURSOR item_master_cursor (v_inventory_item_id NUMBER, v_org_id NUMBER)
      IS
         SELECT inventory_item_id, concatenated_segments,
                eng_item_flag, process_execution_enabled_flag,
                primary_uom_code, inventory_item_status_code
           FROM mtl_system_items_kfv
          WHERE inventory_item_id = v_inventory_item_id
            AND organization_id = v_org_id;

      -- Bug 13811289 - Fetch item description..
      CURSOR item_master_descr_cursor (v_inventory_item_id NUMBER)
      IS
         SELECT DISTINCT concatenated_segments
           FROM mtl_system_items_kfv
          WHERE inventory_item_id = v_inventory_item_id
            AND rownum = 1;                    
          
      -- Bug 11815699 - Fetch process execution enabled setting for item ststus.
      CURSOR item_status_cursor (v_inventory_item_status_code VARCHAR2)
      IS
         SELECT NVL(st.attribute_value, 'N')
           FROM mtl_stat_attrib_values_all_v st
          WHERE st.inventory_item_status_code = v_inventory_item_status_code
            AND attribute_name = 'MTL_SYSTEM_ITEMS.PROCESS_EXECUTION_ENABLED_FLAG';
            
      l_status_exec_enabled   VARCHAR2(1);
      
      -- Bug 7578476 - Remove redundant cursor introduced by 4917631

/*            
 --  pawan kumar added this cursor for bug 4917631         
      CURSOR item_master_cursor_sub (v_inventory_item_id NUMBER, v_org_id NUMBER)
      IS
         SELECT primary_uom_code, eng_item_flag, process_execution_enabled_flag
           FROM mtl_system_items_kfv
          WHERE inventory_item_id = v_inventory_item_id
            AND organization_id = v_org_id;
*/
            
      CURSOR production_rules_cursor (
         v_inventory_item_id   NUMBER
        ,v_org_id              NUMBER)
      IS
         SELECT std_lot_size, primary_uom_code
           FROM mtl_system_items_b
          WHERE inventory_item_id = v_inventory_item_id
            AND organization_id = v_org_id;

      CURSOR cur_get_doc_ord (v_org_id NUMBER, v_batch_type NUMBER)
      IS
         SELECT DECODE (v_batch_type
                       ,10, fpo_doc_numbering
                       ,batch_doc_numbering) assignment_type
           FROM gme_parameters
          WHERE organization_id = v_org_id;

      CURSOR cur_val_batch (
         v_org_id       NUMBER
        ,v_batch_no     VARCHAR2
        ,v_batch_type   VARCHAR2)
      IS
         SELECT 1
           FROM DUAL
          WHERE EXISTS (
                   SELECT 1
                     FROM gme_batch_header
                    WHERE batch_no = v_batch_no
                      AND organization_id = v_org_id
                      AND batch_type = v_batch_type);

      CURSOR get_prim_prod (v_batch_id NUMBER, v_inventory_item_id NUMBER)
      IS
         SELECT   plan_qty, dtl_um
             FROM gme_material_details
            WHERE batch_id = v_batch_id
              AND inventory_item_id = v_inventory_item_id
              AND line_type = gme_common_pvt.g_line_type_prod
         ORDER BY line_no ASC;
         
      -- Bug 6665602 Back out 5484529...  Reinstate original cursor.        
      --sunitha bug # 5484529 selecting the sum of the plan quantity without converting 
      --it to the user entered UOM while creating the batch
      -- Bug 7565054 - Add v_sum_all_prod_lines paramater to cursor query and also Union.
      CURSOR cur_batch_qty (v_batch_id NUMBER
             ,v_inventory_item_id NUMBER
             ,v_sum_all_prod_lines VARCHAR)
      IS
         SELECT SUM (inv_convert.inv_um_convert (v_inventory_item_id
                                                ,5
                                                ,plan_qty
                                                ,dtl_um
                                                ,p_batch_size_uom
                                                ,NULL
                                                ,NULL) ), 1 as line_no
           FROM gme_material_details
          WHERE batch_id = v_batch_id
            AND inventory_item_id = v_inventory_item_id
            AND line_type = gme_common_pvt.g_line_type_prod
            AND v_sum_all_prod_lines = 'A'
         UNION
         SELECT inv_convert.inv_um_convert (v_inventory_item_id
                                                ,5
                                                ,plan_qty
                                                ,dtl_um
                                                ,p_batch_size_uom
                                                ,NULL
                                                ,NULL), line_no
           FROM gme_material_details
          WHERE batch_id = v_batch_id
            AND inventory_item_id = v_inventory_item_id
            AND line_type = gme_common_pvt.g_line_type_prod
            AND v_sum_all_prod_lines <> 'A'
          ORDER BY line_no;
            
      /*CURSOR cur_batch_qty (v_batch_id NUMBER, v_inventory_item_id NUMBER)
      IS
         SELECT SUM (plan_qty )
           FROM gme_material_details
          WHERE batch_id = v_batch_id
            AND inventory_item_id = v_inventory_item_id
            AND line_type = gme_common_pvt.g_line_type_prod; */

      CURSOR cur_item_no (v_inventory_item_id NUMBER, v_org_id NUMBER)
      IS
         SELECT concatenated_segments
           FROM mtl_system_items_kfv
          WHERE inventory_item_id = v_inventory_item_id
            AND organization_id = v_org_id;

      -- nsinghi bug#5674398 Added the Cursor
      CURSOR c_batchsteps (p_material_detail_id IN NUMBER) IS
         SELECT b.*
           FROM gme_batch_step_items a, gme_batch_steps b
          WHERE a.batchstep_id = b.batchstep_id AND a.material_detail_id = p_material_detail_id;
          
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
      Shaliu Chen 02-APR-2015               
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
         

      error_count_exceeded           EXCEPTION;
      expected_error                 EXCEPTION;
      unexpected_error               EXCEPTION;
      scaling_failure                EXCEPTION;
      invalid_header_values          EXCEPTION;
      invalid_recipe                 EXCEPTION;
      conversion_failure             EXCEPTION;
      steps_creation_failure         EXCEPTION;
      batch_header_fetch_failure     EXCEPTION;
      date_exceed_validity_rule      EXCEPTION;
      error_create_phantom           EXCEPTION;
      validation_failure             EXCEPTION;
      create_by_product_failure      EXCEPTION;
      gme_duplicate_batch            EXCEPTION;
      doc_num_is_not_passed          EXCEPTION;
      inventory_shortage             EXCEPTION;
      error_cont_period              EXCEPTION;
      error_non_contiguious          EXCEPTION;
      error_truncate_date            EXCEPTION;
      create_mo_hdr_err              EXCEPTION;
      create_mo_line_err             EXCEPTION;
      calc_mtl_req_date_err          EXCEPTION;
      gmo_instantiate_err            EXCEPTION;
      wip_entity_err                 EXCEPTION;

      --FPBug#4395561
      create_flex_failure            EXCEPTION;
      Fixed_process_loss_failure     EXCEPTION;
      -- Rajesh Patangya Bug # 6752637 
      update_step_mtq_failure        EXCEPTION;
      /*                                    
      BEGIN ER 19161894                    
      Shaliu Chen 18-JUL-2014               
      Added for OPM Step Level OSP Project  
      */         
      create_requisition_failure     EXCEPTION;
      /*END ER 19161894*/ 
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;
      x_return_status := fnd_api.g_ret_sts_success;
      -- Initialize output batch header
      x_batch_header_rec := p_batch_header_rec;

      IF (    p_creation_mode <> 'RECIPE'
          AND NVL (p_batch_size, 0) = 0
          AND p_is_phantom <> 'Y') THEN
         gme_common_pvt.log_message ('GME_API_QTY_CANT_BE_ZERO');
         RAISE expected_error;
      END IF;

      IF x_batch_header_rec.batch_type NOT IN (0, 10) THEN
         gme_common_pvt.log_message ('GME_INVALID_BATCH_TYPE');
         RAISE expected_error;
      END IF;

      OPEN cur_get_doc_ord (x_batch_header_rec.organization_id
                           ,x_batch_header_rec.batch_type);

      FETCH cur_get_doc_ord
       INTO l_doc_numbering;

      CLOSE cur_get_doc_ord;
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('BEfore document numbering check' || g_pkg_name || '.'
                             || l_api_name);
      END IF;
      IF l_doc_numbering = 1 THEN
         IF x_batch_header_rec.batch_no IS NULL THEN
            RAISE doc_num_is_not_passed;
         ELSE
	    --added by qzeng no need to check if done in bulk validation
	    IF NVL(gme_common_pvt.g_bulk_validation_done, 'N') = 'N' THEN
              OPEN cur_val_batch (x_batch_header_rec.organization_id
                                 ,x_batch_header_rec.batch_no
                                 ,x_batch_header_rec.batch_type);

              FETCH cur_val_batch
               INTO l_count;

              CLOSE cur_val_batch;

              IF l_count > 0 THEN
                RAISE gme_duplicate_batch;
              END IF;
	    END IF;
         END IF;                     /* x_batch_header_rec.batch_no IS NULL */
      END IF;                                     /* IF l_doc_numbering = 1 */
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('before laboratory check' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      IF (gme_common_pvt.g_lab_ind = 1 AND gme_common_pvt.g_plant_ind = 1) THEN
         IF p_batch_header_rec.laboratory_ind = 1 THEN
            x_batch_header_rec.laboratory_ind := 1;
            x_batch_header_rec.update_inventory_ind :=
                                      p_batch_header_rec.update_inventory_ind;
         ELSE
            x_batch_header_rec.laboratory_ind := 0;
            x_batch_header_rec.update_inventory_ind := 'Y';
         END IF;
      ELSIF (gme_common_pvt.g_lab_ind = 1) THEN
         x_batch_header_rec.laboratory_ind := 1;
         x_batch_header_rec.update_inventory_ind :=
                                      p_batch_header_rec.update_inventory_ind;
      ELSIF (gme_common_pvt.g_plant_ind = 1) THEN
         x_batch_header_rec.laboratory_ind := 0;
         x_batch_header_rec.update_inventory_ind := 'Y';
      END IF;
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('After laboratory check' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      --  Validate/fill in batch header columns
      l_in_batch_header := x_batch_header_rec;
      l_return :=
                construct_batch_header (l_in_batch_header, x_batch_header_rec);

      IF NOT l_return THEN
         RAISE invalid_header_values;
      END IF;
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('After contruct batch header' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      IF g_debug <= gme_debug.g_log_statement THEN
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || 'Starting Validity rule processing');
      END IF;

      /* If the batch is being created from LCF
         then there is no pre-defined formula or
         validity rule */
      IF p_creation_mode <> 'LCF' THEN
         -- Validate recipe validity rule
         OPEN recipe_validity_rule_cursor
                                  (x_batch_header_rec.recipe_validity_rule_id);

         FETCH recipe_validity_rule_cursor
          INTO l_recipe_validity_rule;

         CLOSE recipe_validity_rule_cursor;

         -- Load the recipe data.
         OPEN recipe_cursor (l_recipe_validity_rule.recipe_id);

         FETCH recipe_cursor
          INTO l_recipe;

         CLOSE recipe_cursor;

         IF l_recipe.recipe_id IS NULL THEN
            gme_common_pvt.log_message ('GME_API_INVALID_RULE');
            RAISE expected_error;
         END IF;

         IF g_debug <= gme_debug.g_log_statement THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || 'Calling gmd_recipe_data_pub.get_recipe_data');
         END IF;

         gmd_recipe_data_pub.get_recipe_data
              (p_api_version                   => 1
              ,p_init_msg_list                 => fnd_api.g_false
              ,p_recipe_id                     => l_recipe.recipe_id
              ,p_organization_id               => x_batch_header_rec.organization_id
              ,x_return_status                 => l_return_status
              ,x_msg_count                     => l_message_count
              ,x_msg_data                      => l_message_list
              ,x_return_code                   => l_return_code
              ,x_recipe_rout_tbl               => gme_common_pvt.routings
              ,x_recipe_rout_matl_tbl          => gme_common_pvt.routing_materials
              ,x_recipe_step_out               => gme_common_pvt.steps
              ,x_routing_depd_tbl              => gme_common_pvt.step_dependencies
              ,x_oprn_act_out                  => gme_common_pvt.activities
              ,x_oprn_resc_rec                 => gme_common_pvt.resources
              ,x_formula_header_rec            => l_formula_master
              ,x_formula_dtl_tbl               => gme_common_pvt.materials
              ,x_recp_resc_proc_param_tbl      => gme_common_pvt.process_parameters);
         IF g_debug <= gme_debug.g_log_statement THEN
            gme_debug.put_line
                       (   g_pkg_name
                        || '.'
                        || l_api_name
                        || 'Return from gmd_recipe_data_pub.get_recipe_data '
                        || l_return_status);
         END IF;

      ELSE /* p_creation_mode = 'LCF' */
      /* Setup following variables so that rest of the program goes through */
         l_return_status := fnd_api.g_ret_sts_success;
         l_recipe_validity_rule.inventory_item_id :=
                               gme_common_pvt.materials (1).inventory_item_id;
         l_formula_master.text_code := NULL;
         l_formula_master.scale_type := 0;
         l_recipe.calculate_step_quantity := 0;
         IF gme_common_pvt.routings.COUNT > 0 THEN
            l_recipe.routing_id := gme_common_pvt.routings(1).routing_id;
         END IF;
      END IF; /* IF p_creation_mode <> 'LCF' THEN */

      IF (g_debug = gme_debug.g_log_statement) THEN
         gme_debug.put_line (   g_pkg_name
                             || ':Creation mode '
                             || p_creation_mode
                             || ' Batch size '
                             || p_batch_size
                             || ' '
                             || p_batch_size_uom);
         gme_debug.put_line ('Return Status was ' || l_return_status);
         gme_debug.put_line ('return_code = ' || TO_CHAR (l_return_code) );
         gme_debug.put_line (   '# of Steps = '
                             || TO_CHAR (gme_common_pvt.steps.COUNT) );
         gme_debug.put_line (   '# of Acts  = '
                             || TO_CHAR (gme_common_pvt.activities.COUNT) );
         gme_debug.put_line (   '# of Rsrc  = '
                             || TO_CHAR (gme_common_pvt.resources.COUNT) );
         gme_debug.put_line (   '# of Matl =  '
                             || TO_CHAR
                                       (gme_common_pvt.routing_materials.COUNT) );
         gme_debug.put_line (   '# of Depd = '
                             || TO_CHAR
                                       (gme_common_pvt.step_dependencies.COUNT) );
         gme_debug.put_line (   '# of Lines = '
                             || TO_CHAR (gme_common_pvt.materials.COUNT) );
      END IF;

      IF l_return_status <> fnd_api.g_ret_sts_success THEN
         IF l_return_code = 4 THEN
            -- Recipe does not have a routing, not an error
            NULL;
         ELSIF l_return_code = 8 THEN
            fnd_message.set_name ('GMD', 'GMD_CIRCULAR_DEPEN_DETECT');
            fnd_msg_pub.ADD;
            RAISE invalid_recipe;
         ELSE
            gme_common_pvt.log_message ('GME_BAD_RECIPE_RETRIEVAL');
            RAISE invalid_recipe;
         END IF;
      END IF;

      IF gme_common_pvt.routings.COUNT > 0 THEN
         x_batch_header_rec.enforce_step_dependency :=
                 NVL (gme_common_pvt.routings (1).enforce_step_dependency, 0);
      END IF;

      IF p_creation_mode <> 'LCF' THEN
         IF    l_formula_master.formula_id IS NULL
            OR l_formula_master.delete_mark = 1
            OR l_formula_master.inactive_ind = 1 THEN
            gme_common_pvt.log_message ('GME_API_INVALID_FORMULA');
            RAISE expected_error;
         END IF;
      END IF;                                /* IF p_creation_mode <> 'LCF' */

      -- Retrieve and validate formula details
      l_number_of_formula_lines := 0;
      l_total_input_qty := 0;
      l_total_output_qty := 0;

      IF g_debug <= gme_debug.g_log_statement THEN
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || 'Start material processing');
      END IF;

      FOR i IN 1 .. gme_common_pvt.materials.COUNT LOOP
         l_formula_material := gme_common_pvt.materials (i);
         
         -- Bug 11815699 - get item status code.
         -- pawan kumar changed this cursor for bug 4917631
         -- Bug 7578476 - Fetch primary uom code also.
         OPEN item_master_cursor (l_formula_material.inventory_item_id
                                 ,x_batch_header_rec.organization_id);
         FETCH item_master_cursor
          INTO l_item_master.inventory_item_id, l_item_master.concatenated_segments,
               l_item_master.eng_item_flag, l_item_master.process_execution_enabled_flag,
               l_item_master.primary_uom_code, l_item_master.inventory_item_status_code;

         -- Bug 13811289 - Make sure this is valid for the specific organization.
         IF item_master_cursor%NOTFOUND THEN
            CLOSE item_master_cursor;
            
            -- Bug 13811289 - Get item number for any non specific organization.
            OPEN item_master_descr_cursor (l_formula_material.inventory_item_id);
            FETCH item_master_descr_cursor INTO l_item_master.concatenated_segments;      
            CLOSE item_master_descr_cursor;


            gme_common_pvt.log_message (p_product_code => 'GMD'
                                       ,p_message_code => 'GMD_RCP_ITEMORG_NOT_FOUND'
                                       ,p_token1_name  => 'ORGN'
                                       ,p_token1_value => gme_common_pvt.g_organization_code
                                       ,p_token2_name  => 'ITEM'
                                       ,p_token2_value => l_item_master.concatenated_segments);                                       
                                          
            RAISE error_count_exceeded;
         END IF;

         CLOSE item_master_cursor;
         
         -- Bug 11815699 - fetch process execution enabled associated to item status code.
         l_status_exec_enabled := 'Y';
         IF l_item_master.inventory_item_status_code IS NOT NULL THEN
            OPEN  item_status_cursor (l_item_master.inventory_item_status_code);
            FETCH item_status_cursor INTO l_status_exec_enabled;
            CLOSE item_status_cursor;
         END IF;
         
         
         -- Bug 11815699 - consider process exceution enabled associated to item status code also.
         IF    l_item_master.inventory_item_id IS NULL
            OR l_item_master.process_execution_enabled_flag = 'N'
            OR l_status_exec_enabled = 'N'
            OR (    l_item_master.eng_item_flag = 'Y'
                AND x_batch_header_rec.laboratory_ind = 0) THEN
            gme_common_pvt.log_message ('GME_API_INVALID_ITEM'
                                       ,'ITEM_NO'
                                       ,l_item_master.concatenated_segments);
            RAISE error_count_exceeded;
         ELSE
            l_number_of_formula_lines := l_number_of_formula_lines + 1;
            l_item_masters (l_number_of_formula_lines) := l_item_master;

            IF l_formula_material.line_type > 0 THEN
               IF l_formula_material.line_type =
                                              gme_common_pvt.g_line_type_prod THEN
                  l_number_of_product_lines := l_number_of_product_lines + 1;

                  
                  
               ELSE
                  l_number_of_byproduct_lines :=
                                              l_number_of_byproduct_lines + 1;
               END IF;
            ELSE
               l_number_of_ingredient_lines :=
                                             l_number_of_ingredient_lines + 1;
            END IF;                     /* l_formula_material.line_type > 0 */
            
          
            -- Construct the material detail row
          
            gme_common_pvt.construct_material_detail
                              (p_formula_detail_rec       => l_formula_material
                              ,p_item_master_rec          => l_item_master
                              ,p_batch_header_rec         => x_batch_header_rec
                              ,x_material_detail_rec      => l_gme_material_detail
                              ,x_return_status            => l_return_status);
            
            IF l_return_status = fnd_api.g_ret_sts_success THEN
                
               l_material_details (l_number_of_formula_lines) :=
                                                        l_gme_material_detail;

               -- Bug 7578476 - Initialize original_qty properly.                                        
               -- l_material_details (l_number_of_formula_lines).original_qty :=
               --                                        l_formula_material.qty;
                                                       
               l_material_details (l_number_of_formula_lines).original_qty :=
                        l_material_details (l_number_of_formula_lines).plan_qty;

               -- Bug 7578476 - Use correct primary UOM to derive original_primary_qty.                                        
               IF (l_material_details (l_number_of_formula_lines).dtl_um <>
                                                l_item_masters(l_number_of_formula_lines).primary_uom_code) THEN
                  l_temp_qty :=
                     inv_convert.inv_um_convert
                        (l_material_details (l_number_of_formula_lines).inventory_item_id
                        ,5
                        ,l_material_details (l_number_of_formula_lines).original_qty
                        ,l_material_details (l_number_of_formula_lines).dtl_um
                        ,l_item_masters (l_number_of_formula_lines).primary_uom_code
                        -- ,l_item_master.primary_uom_code
                        ,NULL
                        ,NULL);
               ELSE
                  l_temp_qty :=
                     l_material_details (l_number_of_formula_lines).original_qty;
               END IF;

               l_material_details (l_number_of_formula_lines).original_primary_qty :=
                                                                    l_temp_qty;
            /*  --Pawan  changes for GMO
              IF  (l_material_details (l_number_of_formula_lines).line_type  =
                  gme_common_pvt.g_line_type_ing )
              AND (l_material_details (l_number_of_formula_lines).phantom_type = 0 ) THEN 
                           
                GMO_DISPENSE_GRP.IS_DISPENSE_ITEM (p_api_version   => 1.0,
                            p_init_msg_list       => fnd_api.g_false ,
                            x_return_status       => l_return_status ,
                            x_msg_count           => l_message_count ,
                            x_msg_data            => l_message_list ,
                            p_inventory_item_id   => l_formula_material.inventory_item_id,
                            p_organization_id     => x_batch_header_rec.organization_id,
                            p_recipe_id           => l_recipe.recipe_id,
                            x_dispense_required   => l_dispense_required,
                            x_dispense_config_id  => l_config_id);
                             
                IF l_return_status = fnd_api.g_ret_sts_success THEN
                    l_material_details (l_number_of_formula_lines).dispense_ind :=  l_dispense_required ; 
                END IF;
                
             END IF;
              -- GMO changes end   */                                                            
            ELSE
               RAISE error_count_exceeded;
            END IF;
         END IF;              /* l_item_master.inventory_item_id IS NULL OR */
      END LOOP;             /* l_formula_material IN formula_details_cursor */

      IF g_debug <= gme_debug.g_log_statement THEN
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || 'Finished material processing');
      END IF;

      IF    l_number_of_formula_lines = 0
         OR l_number_of_ingredient_lines = 0
         OR l_number_of_product_lines = 0 THEN
         gme_common_pvt.log_message ('GME_API_INVALID_FORMULA');
         RAISE error_count_exceeded;
      END IF;

      -- We now need to sort out the scaling. If the formula is marked as scalable
      -- we have to work out the output and input quantities. The columns on
      -- the total_output_qty and total_input_qty on the formula header express
      -- the totals in the YIELD_UOM of the system. However these are not always
      -- useful and we have to work out other quantities on an as-required basis.
      IF l_formula_master.scale_type = 1 THEN
         -- The formula is scalable (at least at the header level anyway)
         -- The first thing we need to do is find out the scale factor
         IF p_creation_mode = 'OUTPUT' THEN
            -- Batch is being created by Total Output.
            -- Scale Factor is the requested batch size divided by the recipe
            -- total output qty
            l_total_output_qty := 0;
            l_uom := p_batch_size_uom;
            gmd_common_val.calculate_total_qty
                                  (formula_id            => l_formula_master.formula_id
                                  ,x_product_qty         => l_total_output_qty
                                  ,x_ingredient_qty      => l_total_input_qty
                                  ,x_uom                 => l_uom
                                  ,x_return_status       => l_return_status
                                  ,x_msg_count           => l_message_count
                                  ,x_msg_data            => l_message_list);

            /* If the above procedure is not able to calculate one of the 2 total qty's it returns status of Q. We need to
            check if the total_output_qty is calculated correctly and not worry about if the total_input is notcalculated. */
            IF     (l_return_status <> fnd_api.g_ret_sts_success)
               AND (l_return_status <> 'Q') THEN
               RAISE expected_error;
            END IF;

            IF (l_total_output_qty IS NULL AND l_return_status = 'Q') THEN
               fnd_message.set_name ('GMD', 'GMD_ERR_CALC_OUTPUT');
               fnd_message.set_token ('UOM', l_uom);
               fnd_msg_pub.ADD;
               RAISE expected_error;
            END IF;

            gmd_validity_rules.get_output_ratio
                        (p_formula_id          => l_formula_master.formula_id
                        ,p_batch_output        => p_batch_size
                        ,p_formula_output      => l_total_output_qty
                        ,p_yield_um            => l_uom
                        ,x_return_status       => l_return_status
                        ,x_output_ratio        => gme_common_pvt.g_batch_scale_factor);

            IF l_return_status <> fnd_api.g_ret_sts_success THEN
               RAISE expected_error;
            END IF;

            l_primaries := 'OUTPUTS';
         ELSIF p_creation_mode = 'PRODUCT' THEN
            -- Batch is being created by Product and Quantity. Similar to above, but
            -- the scale factor is the requested batch size divided by the quantity of
            -- this item produced by the formula, so we need to calulate this first.
            -- Although unlikely the specified product could occur multiple times
            -- and be both a product and a byproduct.
            l_total_output_qty := 0;          
            l_uom := p_batch_size_uom;

            /* Let's convert formula product quantity in the requested UOM */
            FOR loop_count IN 1 .. l_number_of_formula_lines LOOP
               IF     l_material_details (loop_count).line_type =
                                              gme_common_pvt.g_line_type_prod
                  AND l_material_details (loop_count).inventory_item_id =
                                      l_recipe_validity_rule.inventory_item_id THEN
                  
                  -- bug#5373369 nsinghi rework. While determining the scale factor, give higher precision, 
                  -- otherwise when converting back to batch UOM, there is decimal dust issue and batch 
                  -- creation fails.
                  l_temp_qty :=
                     inv_convert.inv_um_convert
                           (l_material_details (loop_count).inventory_item_id
                           ,32 --5
                           ,l_material_details (loop_count).plan_qty
                           ,l_material_details (loop_count).dtl_um
                           ,l_uom
                           ,NULL
                           ,NULL);

                  IF l_temp_qty < 0 THEN
                     IF (g_debug = gme_debug.g_log_statement) THEN
                        gme_debug.put_line
                           ('Failed in UOM Conv from formula product UOM to requested UOM');
                     END IF;

                     l_item_id :=
                             l_material_details (loop_count).inventory_item_id;
                     l_from_uom := l_material_details (loop_count).dtl_um;
                     l_to_uom := l_uom;
                     RAISE conversion_failure;
                  ELSE
                     l_total_output_qty := l_total_output_qty + l_temp_qty;
                     -- Bug 7565054 - Rework of bug 7284242.
                     -- If p_sum_all_prod_lines is not 'A' then sum only the first matching product line.
                     EXIT WHEN p_sum_all_prod_lines <> 'A'; -- Exit the loop. Only sum the first matching product
                  END IF;
               END IF;
                  /* l_material_details(loop_count).line_type IN (1, 2) AND */
            END LOOP;       /* loop_count IN 1 .. l_number_of_formula_lines */

            l_primaries := 'OUTPUTS';

            IF l_total_output_qty = 0 THEN
               gme_common_pvt.log_message ('GME_API_PROD_QTY_CANT_ZERO');
               RAISE expected_error;
            END IF;

            -- Bug 7565054 - Back out 7284242
            -- reinstate original calculation but total output may not include all product lines now.
            gme_common_pvt.g_batch_scale_factor := 
                                             p_batch_size / l_total_output_qty;
                                                                                          
         ELSIF p_creation_mode = 'RECIPE' THEN
            -- Batch is being created by Recipe and Version
            -- If there is a production rule in force we must find the batch size
            -- allowable for this plant/product. The scale factor is then this size
            -- divided by the formula's output of this product. If there isn't a
            -- production rule then the formula as it stands is what we will
            -- create the batch with, and scaling is thus not needed.
            OPEN production_rules_cursor
                                   (l_recipe_validity_rule.inventory_item_id
                                   ,x_batch_header_rec.organization_id);

            FETCH production_rules_cursor
             INTO l_production_rule_qty, l_production_um;

            IF production_rules_cursor%NOTFOUND THEN
               l_primaries := 'OUTPUTS';
               gme_common_pvt.g_batch_scale_factor := 1;

               IF (g_debug = gme_debug.g_log_statement) THEN
                  gme_debug.put_line
                                ('CREATING BY RECIPE/VERSION WITHOUT SCALING');
               END IF;
            ELSE
               l_total_output_qty := 0;

               FOR loop_count IN 1 .. l_number_of_formula_lines LOOP
                  IF     l_material_details (loop_count).line_type IN
                            (gme_common_pvt.g_line_type_prod
                            ,gme_common_pvt.g_line_type_byprod)
                     AND l_material_details (loop_count).inventory_item_id =
                                      l_recipe_validity_rule.inventory_item_id THEN

                     -- bug#5373369 nsinghi rework. While determining the scale factor, give higher precision, otherwise when converting back to batch UOM, there is decimal dust issue and batch creation fails.

                     l_temp_qty :=
                        inv_convert.inv_um_convert
                           (l_material_details (loop_count).inventory_item_id
                           ,32 --5
                           ,l_material_details (loop_count).plan_qty
                           ,l_material_details (loop_count).dtl_um
                           ,l_production_um
                           ,NULL
                           ,NULL);

                     IF l_temp_qty < 0 THEN
                        IF (g_debug = gme_debug.g_log_statement) THEN
                           gme_debug.put_line
                              ('Failed in UOM Conv from formula product UOM to requested UOM');
                        END IF;

                        l_item_id :=
                             l_material_details (loop_count).inventory_item_id;
                        l_from_uom := l_material_details (loop_count).dtl_um;
                        l_to_uom := l_production_um;
                        RAISE conversion_failure;
                     ELSE
                        l_total_output_qty := l_total_output_qty + l_temp_qty;
                     END IF;
                  END IF;
                  /* l_material_details(loop_count).line_type IN (1, 2) AND */
               END LOOP;    /* loop_count IN 1 .. l_number_of_formula_lines */

               IF l_total_output_qty = 0 THEN
                  gme_common_pvt.log_message ('GME_API_PROD_QTY_CANT_ZERO');
                  RAISE expected_error;
               END IF;

               gme_common_pvt.g_batch_scale_factor :=
                                    l_production_rule_qty / l_total_output_qty;
            END IF;                  /* IF production_rules_cursor%NOTFOUND */

            CLOSE production_rules_cursor;

            l_primaries := 'OUTPUTS';
         ELSIF p_creation_mode = 'INPUT' THEN
            -- Batch is being created by Total Input. The scale factor is the requested batch size divided by the formula's
            -- total input. We should be changing all the ingredients to the batch size UOM
            l_total_input_qty := 0;
            l_uom := p_batch_size_uom;
            gmd_common_val.calculate_total_qty
                                  (formula_id            => l_formula_master.formula_id
                                  ,x_product_qty         => l_total_output_qty
                                  ,x_ingredient_qty      => l_total_input_qty
                                  ,x_uom                 => l_uom
                                  ,x_return_status       => l_return_status
                                  ,x_msg_count           => l_message_count
                                  ,x_msg_data            => l_message_list);

            IF     (l_return_status <> fnd_api.g_ret_sts_success)
               AND (l_return_status <> 'Q') THEN
               RAISE expected_error;
            END IF;

            IF (l_total_input_qty IS NULL AND l_return_status = 'Q') THEN
               fnd_message.set_name ('GMD', 'GMD_ERR_CALC_OUTPUT');
               fnd_message.set_token ('UOM', l_uom);
               fnd_msg_pub.ADD;
               RAISE expected_error;
            END IF;

            gmd_validity_rules.get_batchformula_ratio
                  (p_formula_id              => l_formula_master.formula_id
                  ,p_batch_input             => p_batch_size
                  ,p_formula_input           => l_total_input_qty
                  ,p_yield_um                => l_uom
                  ,x_return_status           => l_return_status
                  ,x_batchformula_ratio      => gme_common_pvt.g_batch_scale_factor);

            IF l_return_status <> fnd_api.g_ret_sts_success THEN
               RAISE expected_error;
            END IF;

            l_primaries := 'INPUTS';
         ELSIF p_creation_mode = 'LCF' THEN
            -- No need to do anything, but this is a valid creation mode
            NULL;
         ELSE
            -- Batch is being created by means unknown
            gme_common_pvt.log_message ('GME_API_UNSUPPORTED_MODE'
                                       ,'MODE'
                                       ,p_creation_mode);
            RAISE expected_error;
         END IF;

         IF gme_common_pvt.routings.COUNT <> 0 THEN
            l_total_output_qty_c := 0;

            FOR loop_count IN 1 .. l_number_of_formula_lines LOOP
               IF l_material_details (loop_count).line_type IN
                     (gme_common_pvt.g_line_type_prod
                     ,gme_common_pvt.g_line_type_byprod) THEN
                  IF l_material_details (loop_count).dtl_um =
                                      gme_common_pvt.routings (1).routing_uom THEN
                     l_total_output_qty_c :=
                          l_total_output_qty_c
                        + l_material_details (loop_count).plan_qty;
                  ELSE
                     l_temp_qty :=
                        inv_convert.inv_um_convert
                           (l_material_details (loop_count).inventory_item_id
                           ,32 -- Bug 16474091 - change precision from 5 to 32
                           ,l_material_details (loop_count).plan_qty
                           ,l_material_details (loop_count).dtl_um
                           ,gme_common_pvt.routings (1).routing_uom
                           ,NULL
                           ,NULL);

                     IF l_temp_qty < 0 THEN
                        IF (g_debug = gme_debug.g_log_statement) THEN
                           gme_debug.put_line
                              ('Failed in UOM Conv from formula product UOM to routing UOM');
                        END IF;

                        l_item_id :=
                             l_material_details (loop_count).inventory_item_id;
                        l_from_uom := l_material_details (loop_count).dtl_um;
                        l_to_uom := gme_common_pvt.routings (1).routing_uom;
                        RAISE conversion_failure;
                     ELSE
                        l_total_output_qty_c :=
                                            l_total_output_qty_c + l_temp_qty;
                     END IF;
                  END IF;            /* If formula detail UOM = routing UOM */
               END IF;
                      /* l_material_details(loop_count).line_type IN (1, 2) */
            END LOOP;       /* loop_count IN 1 .. l_number_of_formula_lines */

            IF (g_debug = gme_debug.g_log_statement) THEN
               gme_debug.put_line (   'tot_output before scaling is '
                                   || TO_CHAR (l_total_output_qty_c) );
            END IF;
         END IF;                   /* IF gme_common_pvt.routings.COUNT <> 0 */

         -- Bug 10624995 - rearrange logic as to handle integer scale items for non scaled batches.
         l_change_made := 0;
         IF gme_common_pvt.g_batch_scale_factor <> 1 THEN
            IF (g_debug = gme_debug.g_log_statement) THEN
               gme_debug.put_line
                               (   'scale factor is '
                                || TO_CHAR
                                          (gme_common_pvt.g_batch_scale_factor) );
            END IF;

            l_change_made := 1;

            /* Following line is added, because the scale_batch is expecting the
               batch status to be 0. We will reset it to 1 as soon as scaling is done */
            x_batch_header_rec.batch_status := 0;
            /* l_over_allocations parameter is used to handle default lot going  */
            /* negative cases, which would not be a case here as the default lot */
            /* is created after the scale is successfull                         */
            l_in_material_details := l_material_details;
            gme_scale_batch_pvt.scale_batch
               (p_batch_header_rec            => x_batch_header_rec
               ,p_material_tbl                => l_in_material_details
               ,p_scale_factor                => gme_common_pvt.g_batch_scale_factor
               ,p_primaries                   => l_primaries
               ,p_qty_type                    => 1
               ,p_validity_rule_id            => l_recipe_validity_rule.recipe_validity_rule_id
               ,p_enforce_vldt_check          => fnd_api.g_true
               ,p_recalc_dates                => fnd_api.g_false
               ,p_use_workday_cal             => fnd_api.g_false
               ,p_contiguity_override         => fnd_api.g_true
               ,x_material_tbl                => l_material_details
               ,x_exception_material_tbl      => l_exception_material_tbl
               ,x_batch_header_rec            => l_in_batch_header
               ,
            /* Create batch does not need any values from here so dummy variable */
                x_return_status               => x_return_status);
                
            x_batch_header_rec.batch_status := gme_common_pvt.g_batch_pending;

            IF (g_debug = gme_debug.g_log_statement) THEN
               gme_debug.put_line (   'return from scale_batch is: '
                                   || x_return_status);
            END IF;

            IF x_return_status <> fnd_api.g_ret_sts_success THEN
               RAISE scaling_failure;
            ELSE
               FOR loop_count IN 1 .. l_number_of_formula_lines LOOP
                  l_material_details (loop_count).plan_qty :=
                         ROUND (l_material_details (loop_count).plan_qty, 32);
                  l_material_details (loop_count).original_qty :=
                     ROUND (l_material_details (loop_count).original_qty, 32);
                     
                  -- Bug 7578476 - Use already loaded primary UOM to derive original_primary_qty.
                  -- Remove unnecessary fetch using cursor production_rules_cursor.                                                          
                  IF (l_material_details (loop_count).dtl_um <> l_item_masters(loop_count).primary_uom_code) THEN
                    l_temp_qty := inv_convert.inv_um_convert
                                 (l_material_details (loop_count).inventory_item_id
                                 ,5
                                 ,l_material_details (loop_count).original_qty
                                 ,l_material_details (loop_count).dtl_um
                                 ,l_item_masters(loop_count).primary_uom_code
                                 ,NULL
                                 ,NULL);
                 ELSE
                    l_temp_qty := l_material_details (loop_count).original_qty;
                 END IF;
                 l_material_details (loop_count).original_primary_qty := l_temp_qty;
               END LOOP;
            END IF;
         -- Bug 10624995 - Handle integer scale items if the batch does not require scaling.
         ELSE
            -- Let's make sure integer scale items are correct. 
            FOR loop_count IN 1 .. l_number_of_formula_lines LOOP        
               IF (l_material_details (loop_count).scale_type = 2) THEN
                   l_change_made := 1;
                   l_scale_rec.scale_rounding_variance := l_material_details (loop_count).scale_rounding_variance;
                   l_scale_rec.qty := l_material_details (loop_count).plan_qty;
                   l_scale_rec.scale_multiple := l_material_details (loop_count).scale_multiple;
                   l_scale_rec.rounding_direction := l_material_details (loop_count).rounding_direction;
               
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
                      RAISE error_in_integer_scale;
                   END IF;
               
                   l_material_details (loop_count).plan_qty := l_scale_rec_out.qty;
                   l_material_details (loop_count).original_qty := l_scale_rec_out.qty;
               
                   IF (NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT) THEN
                      gme_debug.put_line('plan actual after integer scaling is '||l_material_details (loop_count).plan_qty);
                   END IF;
               END IF; /* l_material_detail_rec.scale_type = 2 */
            END LOOP;            
            -- End herbal life fix.        
         END IF;             /* IF gme_common_pvt.g_batch_scale_factor <> 1 */

         -- If scaling or integer logic made a change then calculate values for process loss.
         IF l_change_made = 1 THEN
            -- We now need to calculate the total scaled formula output in the Routing  
            -- UOM so that we can look up a few things in the process loss table.
            IF gme_common_pvt.routings.COUNT <> 0 THEN
               l_total_output_qty_b := 0;

               FOR loop_count IN 1 .. l_number_of_formula_lines LOOP
                  IF l_material_details (loop_count).line_type IN
                        (gme_common_pvt.g_line_type_prod
                        ,gme_common_pvt.g_line_type_byprod) THEN
                     IF l_material_details (loop_count).dtl_um =
                                      gme_common_pvt.routings (1).routing_uom THEN
                        l_total_output_qty_b :=
                             l_total_output_qty_b
                           + l_material_details (loop_count).plan_qty;
                     ELSE
                        l_temp_qty :=
                           inv_convert.inv_um_convert
                              (l_material_details (loop_count).inventory_item_id
                              ,32  -- Bug 16474091 - change precision from 5 to 32
                              ,l_material_details (loop_count).plan_qty
                              ,l_material_details (loop_count).dtl_um
                              ,gme_common_pvt.routings (1).routing_uom
                              ,NULL
                              ,NULL);

                        IF l_temp_qty < 0 THEN
                           IF (g_debug = gme_debug.g_log_statement) THEN
                              gme_debug.put_line
                                 ('Failed in UOM Conv from formula product UOM to routing UOM');
                           END IF;

                           l_item_id :=
                              l_material_details (loop_count).inventory_item_id;
                           l_from_uom :=
                                        l_material_details (loop_count).dtl_um;
                           l_to_uom := gme_common_pvt.routings (1).routing_uom;
                           RAISE conversion_failure;
                        ELSE
                           l_total_output_qty_b :=
                                            l_total_output_qty_b + l_temp_qty;
                        END IF;                           /* l_temp_qty > 0 */
                     END IF;        /* IF routing and formula uoms are same */
                  END IF;                               /* If line type > 0 */
               END LOOP;

               IF (g_debug = gme_debug.g_log_statement) THEN
                  gme_debug.put_line (   'Total Output after scaling is '
                                      || TO_CHAR (l_total_output_qty_b) );
               END IF;
            END IF;             /* IF gme_common_pvt.routings.COUNT <> 0 */
         END IF;                /* IF l_change_made */

         IF p_creation_mode <> 'LCF' THEN
            -- Batch has been scaled OK so we now need to determine and apply the process loss.
            l_process_loss :=
               gme_common_pvt.get_process_loss
                  (p_validity_rule_id                => l_recipe_validity_rule.recipe_validity_rule_id
                  ,p_organization_id                 => x_batch_header_rec.organization_id
                  ,p_total_output_qty_scaled         => l_total_output_qty_b
                  ,p_total_output_qty_pre_scale      => l_total_output_qty_c);

            IF (l_process_loss IS NULL) THEN
               RAISE expected_error;
            END IF;

            -- Now that the process loss is known, we need to gross up the scalable
            -- ingredients (for creation by Total Output, Product or Recipe) or
            -- scale down the scalable products (for creation by Total Input).
            FOR l_row_count IN 1 .. l_number_of_formula_lines LOOP
               IF l_material_details (l_row_count).line_type < 0 THEN
                  IF     p_creation_mode IN ('PRODUCT', 'OUTPUT', 'RECIPE')
                     AND l_material_details (l_row_count).scale_type = 1 THEN
                     l_material_details (l_row_count).plan_qty :=
                          l_material_details (l_row_count).plan_qty
                        * 100
                        / (100 - l_process_loss);
                  END IF;
               ELSE       /* l_material_details(l_row_count).line_type >= 0 */
                  IF p_creation_mode = 'INPUT' THEN
                     l_material_details (l_row_count).plan_qty :=
                          l_material_details (l_row_count).plan_qty
                        * (100 - l_process_loss)
                        / 100;
                  END IF;
               END IF;  /* IF l_material_details(l_row_count).line_type < 0 */

               -- Bug 7656415 - Let's round the plan qty.
               l_material_details (l_row_count).plan_qty :=
                                   ROUND(l_material_details (l_row_count).plan_qty,5);

               l_material_details (l_row_count).original_qty :=
                                     l_material_details (l_row_count).plan_qty;
                                     
               -- 7578476 - Initialize original_primary_qty properly.
               IF (l_material_details (l_row_count).dtl_um <>
                                                l_item_masters(l_row_count).primary_uom_code) THEN
                  l_material_details (l_row_count).original_primary_qty :=
                     inv_convert.inv_um_convert
                        (l_material_details (l_row_count).inventory_item_id
                        ,5
                        ,l_material_details (l_row_count).original_qty
                        ,l_material_details (l_row_count).dtl_um
                        ,l_item_masters(l_row_count).primary_uom_code
                        ,NULL
                        ,NULL);
               ELSE
                  l_material_details (l_row_count).original_primary_qty :=
                                      l_material_details (l_row_count).original_qty;
               END IF;                                      
            END LOOP;      /* l_row_count IN 1 .. l_number_of_formula_lines */

            IF (g_debug = gme_debug.g_log_statement) THEN
               gme_debug.put_line (   'Total Output after Process Loss is '
                                   || TO_CHAR (l_total_output_qty_b) );
            END IF;
         END IF;                        /* IF p_creation_mode <> 'LCF' THEN */

         IF gme_common_pvt.routings.COUNT <> 0 THEN
            -- Accumulate the total output of the batch in the ROUTING UOM
            l_requested_qty := 0;

            IF    (l_total_output_qty_b IS NULL OR l_total_output_qty_b = 0)
               OR (p_creation_mode = 'INPUT' AND l_process_loss <> 0) THEN
               l_total_output_qty_b := 0;

               FOR l_row_count IN 1 .. l_number_of_formula_lines LOOP
                  IF l_material_details (l_row_count).line_type > 0 THEN
                     IF l_material_details (l_row_count).dtl_um =
                                      gme_common_pvt.routings (1).routing_uom THEN
                        l_total_output_qty_b :=
                             l_total_output_qty_b
                           + l_material_details (l_row_count).plan_qty;
                     ELSE
                        l_temp_qty :=
                           inv_convert.inv_um_convert
                              (l_material_details (l_row_count).inventory_item_id
                              ,5
                              ,l_material_details (l_row_count).plan_qty
                              ,l_material_details (l_row_count).dtl_um
                              ,gme_common_pvt.routings (1).routing_uom
                              ,NULL
                              ,NULL);

                        IF l_temp_qty < 0 THEN
                           l_item_id :=
                              l_material_details (l_row_count).inventory_item_id;
                           l_from_uom :=
                                      l_material_details (l_row_count).dtl_um;
                           l_to_uom :=
                                      gme_common_pvt.routings (1).routing_uom;
                           RAISE conversion_failure;
                        ELSE
                           l_total_output_qty_b :=
                                            l_total_output_qty_b + l_temp_qty;
                        END IF;
                     END IF;
                  END IF;  -- IF l_material_details(l_row_count).line_type > 0
               END LOOP;      -- l_row_count IN 1 .. l_number_of_formula_lines
            END IF;                         /* b_total_output_qty_b IS NULL */

            IF l_total_output_qty_c = 0 THEN
               gme_common_pvt.log_message ('GME_API_PROD_QTY_CANT_ZERO');
               RAISE expected_error;
            END IF;

            gme_common_pvt.g_routing_scale_factor :=
                                   l_total_output_qty_b / l_total_output_qty_c;

            IF (g_debug = gme_debug.g_log_statement) THEN
               gme_debug.put_line (   'Routing scale factor is '
                                   || gme_common_pvt.g_routing_scale_factor);
            END IF;
         END IF;                   /* IF gme_common_pvt.routings.COUNT <> 0 */
      ELSE                           /* IF l_formula_master.scale_type <> 1 */
         -- Formula cannot be scaled
         gme_common_pvt.g_batch_scale_factor := 1;
         gme_common_pvt.g_routing_scale_factor := 1;
      END IF;

      -- Fixed Process Loss ME

      IF (g_debug = gme_debug.g_log_statement) THEN
          gme_debug.put_line ('Before calling the Fixed Proccess Loss '
                              || p_batch_header_rec.fixed_process_loss_applied);
      END IF;

      IF(p_batch_header_rec.fixed_process_loss_applied = 'Y') THEN
         l_batch_header_rec := x_batch_header_rec;
         l_in_material_details := l_material_details; 
         gme_fpl_pvt.apply_fixed_process_loss
                  (p_batch_header_rec         => l_batch_header_rec
                  ,p_material_tbl             => l_in_material_details
                  ,p_organization_id          => l_batch_header_rec.organization_id
                  ,p_creation_mode            => p_creation_mode
                  ,p_called_from              => 1
                  ,x_batch_header_rec         => x_batch_header_rec
                  ,x_material_tbl             => l_material_details
                  ,x_return_status            => x_return_status
                  );                
            IF (g_debug = gme_debug.g_log_statement) THEN
               gme_debug.put_line ('Return status after apply Fixed Proccess Loss '
                              || x_return_status);
            END IF;  
         IF x_return_status <> fnd_api.g_ret_sts_success THEN
            RAISE Fixed_process_loss_failure;
         ELSE
            IF (g_debug = gme_debug.g_log_statement) THEN
              gme_debug.put_line ('Return from Apply Fixed Proccess Loss is ' || x_return_status);
            END IF;
         END IF; 
      END IF;
        
      -- Fixed Process Loss ME
      IF (g_debug = gme_debug.g_log_statement) THEN
          gme_debug.put_line ('After calling Apply Fixed Proccess Loss return is' || x_return_status);
          gme_debug.put_line ('fnd return is ' || fnd_api.g_ret_sts_success);
      END IF;

      -- If we've found any errors, don't start the updates.
--      IF gme_common_pvt.g_error_count > 0 THEN
  --       RAISE expected_error;
    --  END IF;

      IF g_debug <= gme_debug.g_log_statement THEN
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || 'Start dbl pcrocessing');
      END IF;

      -- Data construction complete so start the database phase
      x_batch_header_rec.formula_id := l_formula_master.formula_id;
      x_batch_header_rec.routing_id := l_recipe.routing_id;
      -- From here on all errors are fatal so the validation level is not checked.
      l_in_batch_header := x_batch_header_rec;
      /*5698727 rework Begin we use timestamp initially as its unique. later we update
        with actual batch_no
      gme_common_pvt.create_document_no (l_in_batch_header
                                        ,x_batch_header_rec);*/
      SELECT trim(TO_CHAR(systimestamp,'DD:MM:YYYY HH24:MI:SS:FF6')) INTO l_doc_timestamp FROM DUAL;

      IF g_debug <= gme_debug.g_log_statement THEN
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || 'Initial Doc No(timestamp): '||l_doc_timestamp);
      END IF;
      
      IF l_doc_numbering = gme_common_pvt.g_auto_doc_numbering THEN
         x_batch_header_rec.batch_no := l_doc_timestamp;
      END IF;
      --5698727  rework End

      IF x_batch_header_rec.batch_no IS NULL THEN
         -- Report error that document number was invalid or could not be generated
         RAISE unexpected_error;
      ELSE
        IF (x_batch_header_rec.batch_type = 0) THEN
          l_prefix := FND_PROFILE.VALUE('GME_BATCH_PREFIX');
        ELSE
          l_prefix := FND_PROFILE.VALUE('GME_FPO_PREFIX');
        END IF;
        /*5698727  rework commented the following code. We will validate the wip entity
          when we are updating the wip_entities table later once the actual doc_no is available */
          /* Bug6398619 we have to validate the batch no for the manual doc ordering as
             the time stamp is  not created for the manual ordering */
          IF l_doc_numbering <> gme_common_pvt.g_auto_doc_numbering THEN
              validate_wip_entity(p_organization_id => x_batch_header_rec.organization_id,
                            p_batch_no        => l_prefix||x_batch_header_rec.batch_no,
                            x_return_status   => l_return_status);
            IF l_return_status <> fnd_api.g_ret_sts_success THEN
              RAISE wip_entity_err;
            END IF;
          END IF;
      END IF;


      l_number_of_text_lines := 0;

      --Bug#5112133 add some debug messages for copy formula text GME Parameter
      IF (g_debug = gme_debug.g_log_statement) THEN
          gme_debug.put_line ('copy_formula_text_ind = '||gme_common_pvt.g_copy_formula_text_ind);            
      END IF;
      -- If the formula has text and we have to copy it then generate
      -- the text header row first as database constraints impose this
      IF     gme_common_pvt.g_copy_formula_text_ind = 1
         AND l_formula_master.text_code IS NOT NULL THEN
         l_return :=
                gme_text_dbl.insert_header_row (l_text_header, l_text_header);

         IF l_return THEN
            x_batch_header_rec.text_code := l_text_header.text_code;
         ELSE
            --Bug#5112133
            IF (g_debug = gme_debug.g_log_statement) THEN
              gme_debug.put_line ('Error in inserting text code = '||l_formula_master.text_code);            
            END IF;
            -- We could not insert the text header. Panic
            RAISE expected_error;
         END IF;
      END IF;

      /* update automatic_step_calculation */
      x_batch_header_rec.automatic_step_calculation :=
                                              l_recipe.calculate_step_quantity;

      IF l_recipe.routing_id IS NOT NULL THEN
         x_batch_header_rec.poc_ind := 'Y';
      ELSE
         x_batch_header_rec.poc_ind := 'N';
      END IF;
      -- Pawan Added code for enhanced_pi_ind--GMO
      IF p_creation_mode <> 'LCF' THEN
         gmd_recipe_fetch_pub.FETCH_ENHANCED_PI_IND (
             p_recipe_id                    => l_recipe.recipe_id
            ,p_recipe_validity_rule_id      => l_recipe_validity_rule.recipe_validity_rule_id
            ,x_enhanced_pi_ind              => l_enhanced_pi_ind
            ,x_return_status               =>  l_return_status);
            IF l_return_status <> fnd_api.g_ret_sts_success THEN
               RAISE expected_error;
            END IF; 
            x_batch_header_rec.ENHANCED_PI_IND  := l_enhanced_pi_ind  ;
       ELSE      
       -- Kapil Bug# 5458674. To Pass the PI Indicator for LCF Batches
        x_batch_header_rec.ENHANCED_PI_IND := p_batch_header_rec.enhanced_pi_ind;  
      END IF; /* IF p_creation_mode <> 'LCF' */

      --FPBug#4395561 Start
      /* call create flex procedure to insert the default values of the BACTH_FLEX
          DFF's segments if they are enabled */
       gme_validate_flex_fld_pvt.create_flex_batch_header(x_batch_header_rec,
                                                          x_batch_header_rec,
                                                          l_return_status);
       IF l_return_status <> FND_API.g_ret_sts_success THEN
         RAISE create_flex_failure;
       END IF;
       --FPBug#4395561 End

      /* This is not a migrated batch, so let's indicate so. */
      x_batch_header_rec.migrated_batch_ind := 'N';
      l_return :=
         gme_batch_header_dbl.insert_row (x_batch_header_rec
                                         ,x_batch_header_rec);

      -- If header was inserted OK, then insert the material details. As
      -- above, if we are copying text, generate and insert the text and codes first
      IF l_return = TRUE THEN
         
         IF x_batch_header_rec.text_code IS NOT NULL THEN
            --Bug#5112133
            IF (g_debug = gme_debug.g_log_statement) THEN
             gme_debug.put_line ('x_batch_header_rec.text_code = '||x_batch_header_rec.text_code);
             gme_debug.put_line ('Inserting formula header text in batch header edit text');
            END IF;
            FOR l_fm_text_tbl_row IN
               fm_text_tbl_cursor (l_formula_master.text_code) LOOP
               l_number_of_text_lines := l_number_of_text_lines + 1;
               l_text_table (l_number_of_text_lines).text_code :=
                                                 x_batch_header_rec.text_code;
               l_text_table (l_number_of_text_lines).line_no :=
                                                    l_fm_text_tbl_row.line_no;
               l_text_table (l_number_of_text_lines).lang_code :=
                                                  l_fm_text_tbl_row.lang_code;
               l_text_table (l_number_of_text_lines).paragraph_code :=
                                             l_fm_text_tbl_row.paragraph_code;
               l_text_table (l_number_of_text_lines).sub_paracode :=
                                               l_fm_text_tbl_row.sub_paracode;

               IF l_fm_text_tbl_row.line_no = -1 THEN
                  -- Start constructing key field for text code.
                  l_text_table (l_number_of_text_lines).text :=
                        'gme_batch_header'
                     || '|'
                     || TO_CHAR (x_batch_header_rec.batch_id)
                     || '|';
               ELSE
                  l_text_table (l_number_of_text_lines).text :=
                                                       l_fm_text_tbl_row.text;
               END IF;
            END LOOP;
         END IF;

         --Bug#5112133
         IF (g_debug = gme_debug.g_log_statement) THEN
          gme_debug.put_line ('l_number_of_formula_lines = '||l_number_of_formula_lines);
         END IF;

         FOR l_row_count IN 1 .. l_number_of_formula_lines LOOP
            -- Create rows for details' text.
            IF (g_debug = gme_debug.g_log_statement) THEN
               gme_debug.put_line ('l text code is  '||l_material_details (l_row_count).text_code);
            END IF;
            IF     l_material_details (l_row_count).text_code IS NOT NULL
               AND gme_common_pvt.g_copy_formula_text_ind = 1 THEN
               l_text_code := l_material_details (l_row_count).text_code;
               l_return :=
                  gme_text_dbl.insert_header_row (l_text_header
                                                 ,l_text_header);

               IF l_return THEN
                  l_material_details (l_row_count).text_code :=
                                                      l_text_header.text_code;

                  -- This fetches using the fm text data based on the fm text code.
                  FOR l_fm_text_tbl_row IN fm_text_tbl_cursor (l_text_code) LOOP
                     l_number_of_text_lines := l_number_of_text_lines + 1;
                     l_text_table (l_number_of_text_lines).text_code :=
                                                      l_text_header.text_code;
                     l_text_table (l_number_of_text_lines).line_no :=
                                                    l_fm_text_tbl_row.line_no;
                     l_text_table (l_number_of_text_lines).lang_code :=
                                                  l_fm_text_tbl_row.lang_code;
                     l_text_table (l_number_of_text_lines).paragraph_code :=
                                             l_fm_text_tbl_row.paragraph_code;
                     l_text_table (l_number_of_text_lines).sub_paracode :=
                                               l_fm_text_tbl_row.sub_paracode;

                     IF l_fm_text_tbl_row.line_no = -1 THEN
                        -- Start constructing key field for text code.
                        l_text_string :=
                              'gme_material_details'
                           || '|'
                           || TO_CHAR
                                     (l_material_details (l_row_count).batch_id)
                           || '|';
                        l_text_string :=
                              l_text_string
                           || l_material_details (l_row_count).line_type
                           || '|';
                        l_text_string :=
                              l_text_string
                           || l_material_details (l_row_count).line_no;
                        l_text_table (l_number_of_text_lines).text :=
                                                                 l_text_string;
                     ELSE
                        l_text_table (l_number_of_text_lines).text :=
                                                       l_fm_text_tbl_row.text;
                     END IF;
                  END LOOP;                           -- text lines loop.  
               ELSE
                  -- We could not insert the text header. Panic
                  --Bug#5112133
                  IF (g_debug = gme_debug.g_log_statement) THEN
                   gme_debug.put_line ('Error in inserting text header');
                  END IF;
                  RAISE expected_error;
               END IF;
            END IF;

            l_material_details (l_row_count).batch_id :=
                                                   x_batch_header_rec.batch_id;

            --FPBug#4395561  Start
             l_return_status := NULL;
             gme_validate_flex_fld_pvt.create_flex_material_details(
                                        l_material_details (l_row_count),
                                        l_material_details (l_row_count),
                                        l_return_status);
             IF l_return_status <> FND_API.g_ret_sts_success THEN
               RAISE create_flex_failure;
             END IF;
             --FPBug#4395561  End
             --Pawan  changes for GMO
              IF  (l_material_details (l_row_count).line_type  =
                  gme_common_pvt.g_line_type_ing )
              AND (l_material_details (l_row_count).phantom_type = 0 )
              --Pawan Kumar add for bug 5365883  
              -- Kapil M. Bug# 5458674. Reverted back the changes to support LCF Batches.
              -- AND (p_creation_mode <> 'LCF') 
              THEN
                           
                GMO_DISPENSE_GRP.IS_DISPENSE_ITEM (p_api_version   => 1.0,
                            p_init_msg_list       => fnd_api.g_false ,
                            x_return_status       => l_return_status ,
                            x_msg_count           => l_message_count ,
                            x_msg_data            => l_message_list ,
                            p_inventory_item_id   => l_material_details (l_row_count).inventory_item_id,
                            p_organization_id     => x_batch_header_rec.organization_id,
                            p_recipe_id           => l_recipe.recipe_id,
                            x_dispense_required   => l_dispense_required,
                            x_dispense_config_id  => l_config_id);
                             
                IF l_return_status IN ('S', 'W') THEN
                    l_material_details (l_row_count).dispense_ind :=  l_dispense_required ; 
                END IF;
                
             END IF;
              -- GMO changes end     
             IF (g_debug = gme_debug.g_log_statement) THEN
               gme_debug.put_line ('MATERTIAL lines to be INSERTED ' || l_row_count);
            END IF;

            l_return :=
               gme_material_details_dbl.insert_row
                                            (l_material_details (l_row_count)
                                            ,l_material_details (l_row_count) );

            IF (g_debug = gme_debug.g_log_statement) THEN
               gme_debug.put_line ('MATERTIAL_INSERTED');
            END IF;

            IF NOT l_return THEN
               RAISE unexpected_error;
            END IF;
           -- Pawan Kumar added for bug 4947535

            IF (l_config_id IS NOT NULL AND l_material_details(l_row_count).dispense_ind = 'Y') then
              IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
                   gme_debug.put_line (g_pkg_name||'.'||l_api_name||'Instantiating  material line id'
                    || l_material_details (l_row_count).material_detail_id);
                   gme_debug.put_line (g_pkg_name||'.'||l_api_name||' config id ' || l_config_id);
              END IF;
             GMO_DISPENSE_GRP.INSTANTIATE_DISPENSE_SETUP
                        (p_api_version          => 1.0
                          ,p_dispense_config_id         => l_config_id
                          ,p_entity_name                => GMO_DISPENSE_GRP.G_MATERIAL_LINE_ENTITY
                          ,p_entity_key                 => l_material_details(l_row_count).material_detail_id
                          ,p_init_msg_list      => FND_API.G_FALSE
                          ,x_return_status      => l_return_status
                          ,x_msg_count          => l_message_count
                          ,x_msg_data           => l_message_list);
              IF l_return_status <> FND_API.G_RET_STS_SUCCESS THEN
              
               RAISE unexpected_error;
              END IF;
            END IF;
            
         END LOOP;

         -- Insert Text detail for all the new text codes, which
         -- hold the data for all the detail lines and the batch header.
         FOR l_row_count IN 1 .. l_text_table.COUNT LOOP
            l_return :=
               gme_text_dbl.insert_text_row (l_text_table (l_row_count)
                                            ,l_text_table (l_row_count) );

            IF NOT l_return THEN
               RAISE unexpected_error;
            END IF;
         END LOOP;
      ELSE
         RAISE unexpected_error;
      END IF;

      -- Bath was created OK, so now try to create the steps (if needed)
      /*Restore the dates as supplied by user If user has suppled plan_start_date, it will be used to calculate other two.
      If user has supplied plan_cmplt_date and plan_start_date is null then plan_cmplt_date will be used to calculate other 2 dates;  */
      x_batch_header_rec.plan_start_date := p_batch_header_rec.plan_start_date;
      x_batch_header_rec.plan_cmplt_date := p_batch_header_rec.plan_cmplt_date;
      x_batch_header_rec.due_date := p_batch_header_rec.due_date;

      SELECT primary_uom_code
        INTO l_prim_item_um
        FROM mtl_system_items_b
       WHERE inventory_item_id = l_recipe_validity_rule.inventory_item_id
         AND organization_id = x_batch_header_rec.organization_id;

      OPEN get_prim_prod (x_batch_header_rec.batch_id
                         ,l_recipe_validity_rule.inventory_item_id);

      FETCH get_prim_prod
       INTO l_prim_prod_qty, l_prim_prod_um;

      IF get_prim_prod%FOUND THEN
         l_prim_prod_found := TRUE;
      ELSE
         l_prim_prod_found := FALSE;
      END IF;

      CLOSE get_prim_prod;

      IF g_debug <= gme_debug.g_log_statement THEN
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || 'Start step processing');
      END IF;

      IF gme_common_pvt.routings.COUNT <> 0 THEN
         gme_create_step_pvt.create_batch_steps
                 (p_recipe_rout_step_tbl      => gme_common_pvt.steps
                 ,p_recipe_rout_act_tbl       => gme_common_pvt.activities
                 ,p_recipe_rout_resc_tbl      => gme_common_pvt.resources
                 ,p_resc_parameters_tbl       => gme_common_pvt.process_parameters
                 ,p_recipe_rout_matl_tbl      => gme_common_pvt.routing_materials
                 ,p_routing_depd_tbl          => gme_common_pvt.step_dependencies
                 ,p_gme_batch_header_rec      => x_batch_header_rec
                 ,p_use_workday_cal           => p_use_workday_cal
                 ,p_contiguity_override       => p_contiguity_override
                 ,p_ignore_qty_below_cap      => p_ignore_qty_below_cap
                 ,x_return_status             => x_return_status);

         IF x_return_status <> fnd_api.g_ret_sts_success THEN
            RAISE steps_creation_failure;
         END IF;

         IF NOT (gme_batch_header_dbl.fetch_row (x_batch_header_rec
                                                ,x_batch_header_rec) ) THEN
            RAISE batch_header_fetch_failure;
         END IF;
      ELSE
         /* routing count = 0... there are no steps, so calculate the dates with prod rules if they exist... */
         IF l_prim_prod_found THEN
            l_temp_qty :=
               inv_convert.inv_um_convert
                                   (l_recipe_validity_rule.inventory_item_id
                                   ,5
                                   ,l_prim_prod_qty
                                   ,l_prim_prod_um
                                   ,l_prim_item_um
                                   ,NULL
                                   ,NULL);

            IF l_temp_qty < 0 THEN
               l_item_id := l_recipe_validity_rule.inventory_item_id;
               l_from_uom := l_prim_prod_um;
               l_to_uom := l_prim_item_um;
               RAISE conversion_failure;
            END IF;

            IF (gme_common_pvt.calc_date_from_prod_rule
                   (p_organization_id        => x_batch_header_rec.organization_id
                   ,p_inventory_item_id      => l_recipe_validity_rule.inventory_item_id
                   ,p_item_qty               => l_temp_qty
                   ,p_start_date             => x_batch_header_rec.plan_start_date
                   ,p_cmplt_date             => x_batch_header_rec.plan_cmplt_date
                   ,x_start_date             => x_batch_header_rec.plan_start_date
                   ,x_cmplt_date             => x_batch_header_rec.plan_cmplt_date) ) THEN
               l_no_prod_rule_found := FALSE;
            ELSE
               l_no_prod_rule_found := TRUE;
            END IF;
         ELSE
            -- prim prod was not found...
            l_no_prod_rule_found := TRUE;
         END IF;

         IF l_no_prod_rule_found THEN
            IF     x_batch_header_rec.plan_start_date IS NOT NULL
               AND x_batch_header_rec.plan_cmplt_date IS NULL THEN
               x_batch_header_rec.plan_cmplt_date :=
                                           x_batch_header_rec.plan_start_date;
            ELSIF     x_batch_header_rec.plan_start_date IS NULL
                  AND x_batch_header_rec.plan_cmplt_date IS NOT NULL THEN
               x_batch_header_rec.plan_start_date :=
                                           x_batch_header_rec.plan_cmplt_date;
            ELSIF     x_batch_header_rec.plan_start_date IS NULL
                  AND x_batch_header_rec.plan_cmplt_date IS NULL THEN
               x_batch_header_rec.plan_start_date :=
                                                   gme_common_pvt.g_timestamp;
               x_batch_header_rec.plan_cmplt_date :=
                                                   gme_common_pvt.g_timestamp;
            END IF;

            IF (g_debug = gme_debug.g_log_statement) THEN
               gme_debug.put_line
                               (   'production rule start_date '
                                || TO_CHAR
                                          (x_batch_header_rec.plan_start_date
                                          ,'DD-MON-YYYY HH24:MI:SS') );
               gme_debug.put_line
                                (   'production rule end_date '
                                 || TO_CHAR
                                          (x_batch_header_rec.plan_cmplt_date
                                          ,'DD-MON-YYYY HH24:MI:SS') );
            END IF;
         ELSE
            IF p_use_workday_cal = fnd_api.g_true THEN
               gmd_recipe_fetch_pub.fetch_contiguous_ind
                  (p_recipe_id                    => l_recipe_validity_rule.recipe_id
                  ,p_orgn_id                      => p_batch_header_rec.organization_id
                  ,p_recipe_validity_rule_id      => l_recipe_validity_rule.recipe_validity_rule_id
                  ,x_contiguous_ind               => l_cont_ind
                  ,x_return_status                => l_return_status);

               IF l_return_status <> fnd_api.g_ret_sts_success THEN
                  IF (g_debug = gme_debug.g_log_statement) THEN
                     gme_debug.put_line
                        (   'GMD_RECIPE_FETCH_PUB.FETCH_CONTIGUOUS_IND returned error '
                         || l_return_status);
                  END IF;

                  l_cont_ind := 0;
               END IF;

               IF (g_debug = gme_debug.g_log_statement) THEN
                  gme_debug.put_line (   'Calendar code found '
                                      || gme_common_pvt.g_calendar_code);
                  gme_debug.put_line ('Cont ID found ' || l_cont_ind);
               END IF;

               l_duration :=
                    (  x_batch_header_rec.plan_cmplt_date
                     - x_batch_header_rec.plan_start_date)
                  * 24;

               IF (g_debug = gme_debug.g_log_statement) THEN
                  gme_debug.put_line ('l duration ' || l_duration);
               END IF;

               IF    p_batch_header_rec.plan_start_date IS NOT NULL
                  OR (    p_batch_header_rec.plan_start_date IS NULL
                      AND p_batch_header_rec.plan_cmplt_date IS NULL) THEN
                  gmp_calendar_api.get_contiguous_periods
                     (p_api_version        => 1
                     ,p_init_msg_list      => FALSE
                     ,p_start_date         => NVL
                                                 (p_batch_header_rec.plan_start_date
                                                 ,x_batch_header_rec.plan_start_date)
                     ,p_end_date           => NULL
                     ,p_calendar_code      => gme_common_pvt.g_calendar_code
                     ,p_duration           => l_duration
                     ,p_output_tbl         => l_contig_period_tbl
                     ,x_return_status      => l_return_status);

                  IF (l_return_status <> x_return_status) THEN
                     RAISE error_cont_period;
                  END IF;

                  l_cal_count := l_contig_period_tbl.COUNT;

                  IF (g_debug = gme_debug.g_log_statement) THEN
                     gme_debug.put_line ('l cal_count ' || l_cal_count);
                     gme_debug.put_line (   'p_contiguity_override '
                                         || p_contiguity_override);
                  END IF;

                  IF l_cont_ind = 1
                     AND p_contiguity_override = fnd_api.g_false THEN
                     IF l_cal_count > 1 THEN
                        RAISE error_non_contiguious;
                     END IF;
                  END IF;

                  x_batch_header_rec.plan_cmplt_date :=
                                    l_contig_period_tbl (l_cal_count).end_date;
               ELSE
                  IF p_batch_header_rec.plan_cmplt_date IS NOT NULL THEN
                     gmp_calendar_api.get_contiguous_periods
                          (p_api_version        => 1
                          ,p_init_msg_list      => FALSE
                          ,p_start_date         => NULL
                          ,p_end_date           => p_batch_header_rec.plan_cmplt_date
                          ,p_calendar_code      => gme_common_pvt.g_calendar_code
                          ,p_duration           => l_duration
                          ,p_output_tbl         => l_contig_period_tbl
                          ,x_return_status      => l_return_status);

                     IF (l_return_status <> x_return_status) THEN
                        RAISE error_cont_period;
                     END IF;

                     l_cal_count := l_contig_period_tbl.COUNT;

                     IF (g_debug = gme_debug.g_log_statement) THEN
                        gme_debug.put_line ('l cal_count ' || l_cal_count);
                        gme_debug.put_line (   'p_contiguity_override '
                                            || p_contiguity_override);
                     END IF;

                     IF     l_cont_ind = 1
                        AND p_contiguity_override = fnd_api.g_false THEN
                        IF l_cal_count > 1 THEN
                           RAISE error_non_contiguious;
                        END IF;
                     END IF;

                     x_batch_header_rec.plan_start_date :=
                                  l_contig_period_tbl (l_cal_count).start_date;
                  END IF;
               END IF;
            END IF;
         END IF;

         UPDATE gme_batch_header
            SET plan_start_date = x_batch_header_rec.plan_start_date
               ,plan_cmplt_date = x_batch_header_rec.plan_cmplt_date
               ,due_date =
                   NVL (x_batch_header_rec.due_date
                       ,x_batch_header_rec.plan_cmplt_date)
               -- Bug 13256866
               ,last_updated_by = gme_common_pvt.g_user_ident
               ,last_update_date = gme_common_pvt.g_timestamp
               ,last_update_login = gme_common_pvt.g_login_id
          WHERE batch_id = x_batch_header_rec.batch_id;
      END IF;

/*
         -- Bug 17433833 Moved batch number generation lower in the logic.
         -- 5698727
         l_in_batch_header1 := x_batch_header_rec ;
         gme_common_pvt.create_document_no (l_in_batch_header1
                                           ,x_batch_header_rec);
         -- 5698727 rework
         -- Bug#6398619 the validation here is required only for the automatic doc numbering
        IF l_doc_numbering = gme_common_pvt.g_auto_doc_numbering THEN        
           
           -- Bug 13785754/13815190 - try 20 times if necessary to get a unique batch number.
           l_doc_count := 1;
           WHILE l_doc_count < 20 LOOP

              validate_wip_entity(p_organization_id => x_batch_header_rec.organization_id,
                                  p_batch_no        => l_prefix||x_batch_header_rec.batch_no,
                                  x_return_status   => l_return_status);
              
              IF l_return_status <> fnd_api.g_ret_sts_success THEN
                 IF l_doc_count > 18 THEN
                    RAISE wip_entity_err;
                 ELSE
                    l_doc_count := l_doc_count + 1;                 
                    gme_common_pvt.create_document_no (l_in_batch_header1
                                                      ,x_batch_header_rec);              
                 END IF;           
              ELSE           
                 -- Force loop to stop.
                 l_doc_count := 99;
              END IF;                     
           END LOOP;         
           
           -- update the batch_no with the actual value
           UPDATE gme_batch_header
              SET batch_no = x_batch_header_rec.batch_no
	         -- Bug 13256866
	         ,last_updated_by = gme_common_pvt.g_user_ident
	         ,last_update_date = gme_common_pvt.g_timestamp
	         ,last_update_login = gme_common_pvt.g_login_id
            WHERE batch_id = x_batch_header_rec.batch_id;
           
           -- 5698727 rework update the wip_entities table with actual batch no
           UPDATE wip_entities
              SET wip_entity_name = l_prefix||x_batch_header_rec.batch_no
            WHERE organization_id = x_batch_header_rec.organization_id
              AND wip_entity_name = l_prefix||l_doc_timestamp;
        END IF;
        -- 5698727
*/

      IF (g_debug = gme_debug.g_log_statement) THEN
         gme_debug.put_line (   'Calculated Start date is '
                             || TO_CHAR (x_batch_header_rec.plan_start_date
                                        ,'DD-MON-YYYY HH24:MI:SS') );
         gme_debug.put_line (   'Calculated Cmplt date is '
                             || TO_CHAR (x_batch_header_rec.plan_cmplt_date
                                        ,'DD-MON-YYYY HH24:MI:SS') );
      END IF;

      IF (    p_batch_header_rec.plan_start_date IS NOT NULL
          AND p_batch_header_rec.plan_cmplt_date IS NOT NULL) THEN
         -- IF routings exists and calculated date is greater than the date entered,
         -- then truncate all the dates to dates entered
         IF (gme_common_pvt.routings.COUNT <> 0) THEN
            IF (x_batch_header_rec.plan_cmplt_date >
                                            p_batch_header_rec.plan_cmplt_date) THEN
               x_batch_header_rec.plan_start_date :=
                                           p_batch_header_rec.plan_start_date;
               x_batch_header_rec.plan_cmplt_date :=
                                           p_batch_header_rec.plan_cmplt_date;
               gme_reschedule_batch_pvt.truncate_date
                                   (p_batch_header_rec      => x_batch_header_rec
                                   ,p_date                  => 1
                                   ,p_batchstep_id          => NULL
                                   ,x_return_status         => l_return_status);

               IF l_return_status <> x_return_status THEN
                  RAISE error_truncate_date;
               END IF;
            -- Routings exists and calculated date is less than date entered,
            -- then updates the dates as entered by the user.
            ELSE
               UPDATE gme_batch_header
                  SET plan_cmplt_date = p_batch_header_rec.plan_cmplt_date
                     ,due_date =
                         NVL (x_batch_header_rec.due_date
                             ,p_batch_header_rec.plan_cmplt_date)
                     ,last_updated_by = gme_common_pvt.g_user_ident
                     ,last_update_date = gme_common_pvt.g_timestamp
                     ,last_update_login = gme_common_pvt.g_login_id
                WHERE batch_id = x_batch_header_rec.batch_id;
            END IF;
         END IF;                    -- End gme_common_pvt.routings.COUNT  <> 0

         UPDATE gme_batch_header
            SET plan_cmplt_date = p_batch_header_rec.plan_cmplt_date
               ,due_date =
                   NVL (x_batch_header_rec.due_date
                       ,p_batch_header_rec.plan_cmplt_date)
               ,last_updated_by = gme_common_pvt.g_user_ident
               ,last_update_date = gme_common_pvt.g_timestamp
               ,last_update_login = gme_common_pvt.g_login_id
          WHERE batch_id = x_batch_header_rec.batch_id;
      END IF;                      -- End start and completion dates not null.

      IF NOT (gme_batch_header_dbl.fetch_row (x_batch_header_rec
                                             ,x_batch_header_rec) ) THEN
         RAISE batch_header_fetch_failure;
      END IF;

      IF g_debug <= gme_debug.g_log_statement THEN
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || 'Calculate mtl req date');
      END IF;

      l_item_substituted := FALSE; 
      FOR l_row_count IN 1 .. l_number_of_formula_lines LOOP
         gme_common_pvt.calc_mtl_req_date
            (p_batch_header_rec      => x_batch_header_rec
            ,p_batchstep_rec         => NULL
            ,p_mtl_dtl_rec           => l_material_details (l_row_count)
            ,x_mtl_req_date          => l_material_details (l_row_count).material_requirement_date
            ,x_return_status         => l_return_status);

         IF (l_return_status <> fnd_api.g_ret_sts_success) THEN
            RAISE calc_mtl_req_date_err;
         ELSE
            --FPBug#4351032 Start       
            /*Material requirement date is in our hands we need to find out whether particular ingredient 
               is effective or not depending on substitution list defined                        
            */
            l_curr_item_substituted := FALSE;  
            IF l_material_details (l_row_count).phantom_type = 0 AND
               l_material_details (l_row_count).line_type = -1 THEN

               -- nsinghi bug#5674398 
               -- Pass the ingredient substitution date rather than the material requirement date. 
               -- At this point, steps table is not updated. Hence need to have similar logic here too 
               -- as in gme_api_grp.get_ingr_sub_date.

               IF gme_common_pvt.g_ingr_sub_date = 2 THEN
                 l_ingred_req_date := l_material_details (l_row_count).material_requirement_date;
               ELSIF gme_common_pvt.g_ingr_sub_date = 1 THEN
                 l_ingred_req_date := NVL(p_batch_header_rec.plan_start_date, x_batch_header_rec.plan_start_date);
               ELSIF gme_common_pvt.g_ingr_sub_date = 3 THEN
                 l_ingred_req_date := NVL(p_batch_header_rec.plan_cmplt_date, x_batch_header_rec.plan_cmplt_date);
               END IF;

               IF g_debug <= gme_debug.g_log_statement THEN
                  gme_debug.put_line (   g_pkg_name||'.'|| l_api_name
                                      || ' l_ingred_req_date '||TO_CHAR(l_ingred_req_date, 'MON-DD-YYYY HH24:MI:SS'));
               END IF;
               
               gmdfmval_pub.get_substitute_line_item (
                       pformulaline_id        => l_material_details (l_row_count).formulaline_id,
                       pitem_id               => l_material_details (l_row_count).inventory_item_id,
                       pqty                   => l_material_details (l_row_count).plan_qty,
                       puom                   => l_material_details (l_row_count).dtl_um,
                       pscale_multiple        => l_material_details (l_row_count).scale_multiple,
-- nsinghi bug#5674398. Pass the ingredient substitution date rather than the material requirement date
--                       pdate                  => l_material_details (l_row_count).material_requirement_date,
                       pdate                  => l_ingred_req_date,
                       xformuladetail_tbl     => l_formula_tbl);
               
               IF l_material_details (l_row_count).inventory_item_id <> l_formula_tbl(1).inventory_item_id THEN 

                 -- Bug 11815699 - get item status code.
                 -- Bug 7578476 - Fetch primary uom code also.
                 -- Remove redundant cursor introduced by 4917631
                 OPEN  item_master_cursor (l_formula_tbl(1).inventory_item_id
                                         ,x_batch_header_rec.organization_id);
                 FETCH item_master_cursor
                 INTO  l_new_item_rec.inventory_item_id, l_new_item_rec.concatenated_segments,
                       l_new_item_rec.eng_item_flag, l_new_item_rec.process_execution_enabled_flag,
                       l_new_item_rec.primary_uom_code, l_item_master.inventory_item_status_code;
               
                 CLOSE item_master_cursor;

                 /* check whether item is process execution enabled and it's not experimental item incase of lab batches */
                  IF l_new_item_rec.PROCESS_EXECUTION_ENABLED_FLAG <> 'Y' OR
                     ( l_new_item_rec.ENG_ITEM_FLAG = 'Y' AND NVL(x_batch_header_rec.laboratory_ind,0) <> 1 ) THEN
                    RAISE invalid_item_substitute;
                  END IF;

                  IF (g_debug <= gme_debug.g_log_statement) THEN
                   gme_debug.put_line('Calculated mtl req date: '||l_material_details (l_row_count).material_requirement_date);
                   gme_debug.put_line('Initial Item ID: '||l_material_details (l_row_count).inventory_item_id);
                   gme_debug.put_line('Initial Plan Qty: '||l_material_details (l_row_count).plan_qty);
                   gme_debug.put_line('Initial Detail UOM: '||l_material_details (l_row_count).dtl_um);
                   gme_debug.put_line('Initial Scale multiple: '||l_material_details (l_row_count).scale_multiple);
                  END IF;

                  l_item_substituted := TRUE; 
                  l_curr_item_substituted := TRUE; 

                  /*reassign new item values to material details */
                  l_material_details (l_row_count).inventory_item_id := l_formula_tbl(1).inventory_item_id;
                  l_material_details (l_row_count).plan_qty := l_formula_tbl(1).qty;
                  l_material_details (l_row_count).original_qty := l_formula_tbl(1).qty;
                  l_material_details (l_row_count).dtl_um := l_formula_tbl(1).detail_uom;
                  l_material_details (l_row_count).scale_multiple := l_formula_tbl(1).scale_multiple;

                  IF (g_debug <= gme_debug.g_log_statement) THEN
                   gme_debug.put_line('Substitued Item ID: '||l_formula_tbl(1).inventory_item_id);
                   gme_debug.put_line('Substitued Plan Qty: '||l_formula_tbl(1).qty);
                   gme_debug.put_line('Substituted Detail UOM: '||l_formula_tbl(1).detail_uom);
                   gme_debug.put_line('Substituted Scale multiple: '||l_formula_tbl(1).scale_multiple);
                  END IF;

                  /*if the new item uom is not the primary uom then calculate the primary quantity*/
                  IF l_formula_tbl(1).detail_uom <> l_new_item_rec.primary_uom_code THEN
                    l_temp_qty := inv_convert.inv_um_convert
                                      (item_id       => l_material_details(l_row_count).inventory_item_id
                                      ,precision     => 5
                                      ,from_quantity => l_material_details(l_row_count).plan_qty
                                      ,from_unit     => l_material_details(l_row_count).dtl_um
                                      ,to_unit       => l_new_item_rec.primary_uom_code
                                      ,from_name     => NULL
                                      ,to_name       => NULL);
                  ELSE
                    l_temp_qty := l_material_details(l_row_count).plan_qty;
                  END IF;
                  
                  IF (g_debug < gme_debug.g_log_statement) THEN
                   gme_debug.put_line('Initial Original Primary Qty: '||l_material_details (l_row_count).original_primary_qty);
                   gme_debug.put_line('Substituted Original Priamry Qty: '||l_temp_qty);
                  END IF;

                  /* assign to original primary qty */
                  l_material_details (l_row_count).original_primary_qty := l_temp_qty;  
               ELSE 
                  l_curr_item_substituted := FALSE;
               END IF; /* item id check */             
            END IF;  /* phantom type and line type check */
             --FPBug#4351032 End           

            -- Bug 7710435 - Added p_called_by parameter.
            l_return :=
                gme_material_details_dbl.update_row (p_material_detail => l_material_details (l_row_count), 
                                                     p_called_by =>'C' );

            /* FPBug#4351032 Updating original primary qty field manually 
               as this update is not allowed in above dbl procedure */
            
            UPDATE gme_material_details 
               SET original_primary_qty = l_material_details (l_row_count).original_primary_qty
             WHERE material_detail_id = l_material_details (l_row_count).material_detail_id;

            -- nsinghi bug#5674398 
            -- if ASQC is on and current material line has been substitued, recalculate step qtys
            IF x_batch_header_rec.automatic_step_calculation = 1 AND l_curr_item_substituted THEN
               OPEN c_batchsteps (l_material_details(l_row_count).material_detail_id);
               FETCH c_batchsteps INTO l_batchstep_rec;
               CLOSE c_batchsteps;

               IF (g_debug <= gme_debug.g_log_statement) THEN
                 gme_debug.put_line ('Before updating step qty after the substitution');
               END IF;
               gme_update_step_qty_pvt.update_step_qty (p_batch_step_rec  => l_batchstep_rec,
                                                      x_message_count     => l_message_count,
                                                      x_message_list      => l_message_list,
                                                      x_return_status     => x_return_status,
                                                      x_batch_step_rec    => x_batchstep_rec
                                                     );
               IF (g_debug <= gme_debug.g_log_statement) THEN
                 gme_debug.put_line ('After update step qty, return status is:' || x_return_status);
               END IF;
               IF x_return_status <> fnd_api.g_ret_sts_success THEN
                 -- We could not update the step qty
                  RAISE unexpected_error;
               END IF;
            END IF;
         END IF;
      END LOOP;

      -- nsinghi bug#5674398 START
      -- If atleast one item is substituted and the batch is ASQC batch
      -- then we will recalculate the start and end dates by calling reschedule batch
      IF NVL (x_batch_header_rec.update_inventory_ind, 'Y') = 'Y' AND l_item_substituted = TRUE THEN
         IF x_batch_header_rec.automatic_step_calculation = 1 THEN
            /* Assigning the user passed dates so that batch gets rescheduled to the passed dates*/

            -- Restore the dates as supplied by user. If user does not provide either start or cmplt dt, use
            -- newly created batch's planned start date to re-schedule.
            IF p_batch_header_rec.plan_start_date IS NULL AND p_batch_header_rec.plan_cmplt_date IS NULL THEN
              x_batch_header_rec.plan_cmplt_date := p_batch_header_rec.plan_cmplt_date;
            ELSE
              x_batch_header_rec.plan_start_date := p_batch_header_rec.plan_start_date;
              x_batch_header_rec.plan_cmplt_date := p_batch_header_rec.plan_cmplt_date;
            END IF;

            l_batch_header_rec := x_batch_header_rec;
            gme_reschedule_batch_pvt.reschedule_batch (
                 p_batch_header_rec     => l_batch_header_rec,
                 p_use_workday_cal      => p_use_workday_cal,
                 p_contiguity_override  => p_contiguity_override,
                 x_batch_header_rec     => x_batch_header_rec,
                 x_return_status        => x_return_status
                 );

            IF (g_debug <= gme_debug.g_log_statement) THEN
               gme_debug.put_line('Batch Planned Start Date '||TO_CHAR(x_batch_header_rec.plan_start_date,'MON-DD-YYYY HH24:MI:SS'));
               gme_debug.put_line('Batch Planned Completion Date '||TO_CHAR(x_batch_header_rec.plan_cmplt_date,'MON-DD-YYYY HH24:MI:SS'));
            END IF;

            IF x_return_status <> FND_API.G_RET_STS_SUCCESS THEN
               RAISE unexpected_error;
            END IF; /* Reschedule of the batch to the user passed dates */
            
            /* nsinghi bug#5674398 Added the FETCH condition. Reschedule_batch will update the 
            material_requirement_date, hence requery material detail records */

            l_in_material_detail.batch_id := x_batch_header_rec.batch_id;
            IF NOT gme_material_details_dbl.fetch_tab(l_in_material_detail, l_material_details) THEN
              RAISE material_dtl_fetch_failure; 
            END IF;
            UPDATE gme_batch_header
               SET due_date =
                   x_batch_header_rec.plan_cmplt_date
                  ,last_updated_by = gme_common_pvt.g_user_ident
                  ,last_update_date = gme_common_pvt.g_timestamp
                  ,last_update_login = gme_common_pvt.g_login_id
             WHERE batch_id = x_batch_header_rec.batch_id;

         END IF;   /* ASQC */
      END IF; /* Update inventory*/
      -- nsinghi bug#5674398 END

      IF (g_debug = gme_debug.g_log_statement) THEN
         gme_debug.put_line ('p_creation_mode=' || p_creation_mode);
         gme_debug.put_line ('prim_prod_qty=' || l_prim_prod_qty);
         gme_debug.put_line ('prim_prod_um=' || l_prim_prod_um);
         gme_debug.put_line ('p_batch_size=' || p_batch_size);
         gme_debug.put_line ('p_batch_size_uom=' || p_batch_size_uom);
         gme_debug.put_line (   'User passed Start date is '
                             || TO_CHAR (p_batch_header_rec.plan_start_date
                                        ,'DD-MON-YYYY HH24:MI:SS') );
         gme_debug.put_line (   'User passed Cmplt date is '
                             || TO_CHAR (p_batch_header_rec.plan_cmplt_date
                                        ,'DD-MON-YYYY HH24:MI:SS') );
      END IF;

      IF l_prim_prod_found AND (p_creation_mode = 'PRODUCT') THEN  
         -- Bug 7565054 - Add paramater p_sum_all_prod_lines to cursor to control summation.      
         OPEN cur_batch_qty (x_batch_header_rec.batch_id
                            ,l_recipe_validity_rule.inventory_item_id
                            ,p_sum_all_prod_lines);
                            
         FETCH cur_batch_qty
          INTO l_temp_qty, l_temp_line_no;

         CLOSE cur_batch_qty;

         IF l_temp_qty < 0 THEN
            l_item_id := l_recipe_validity_rule.inventory_item_id;
            l_from_uom := p_batch_size_uom;
            l_to_uom := l_prim_prod_um;
            RAISE conversion_failure;
         END IF;

         IF (g_debug = gme_debug.g_log_statement) THEN
            gme_debug.put_line ('requested qty in dtl uom=' || l_temp_qty);
            gme_debug.put_line ('requested qty in dtl uom rounded=' || round(l_temp_qty, 4));
         END IF;
         
         --sunitha bug # 5484529 Convert the p_batch_size(user entered product plan quantity
         --to the Routing uom and then Compare it with the l_temp_qty.

         -- Bug 6665602 Back out 5484529... Reinstate original condition.
         -- Bug 10086349 - Use precision of 4 to make comparison.                     
         
         -- Bug 10379034 - Change the comparison so we do not rely on potential rounding issues.
         -- If the batch is 99.9 % of what was requested then let it go thru.
         -- This is a rework of 10086349. Note: l_temp_qty is already in p_batch_size_uom.
         
         -- Bug 12909216 - Avoid divide by sero error when qty is zero. This will allow
         -- zero qty phantom batches to be created.
         IF l_temp_qty <> 0 THEN
            IF ((p_batch_size / l_temp_qty) < .999 ) THEN
            -- IF (ROUND (p_batch_size, 4) <> ROUND (l_temp_qty, 4) ) THEN
            /* IF (ROUND (inv_convert.inv_um_convert (l_recipe_validity_rule.inventory_item_id
                                                ,5
                                                ,p_batch_size
                                                ,p_batch_size_uom
                                                ,l_recipe_validity_rule.detail_uom
                                                ,NULL
                                                ,NULL), 5) <> ROUND (l_temp_qty, 5) ) THEN */
            
               IF (g_debug = gme_debug.g_log_statement) THEN
                  gme_debug.put_line('Batch failed to be created for qty requested with creation by product qty');
               END IF;
            
               RAISE create_by_product_failure;
            END IF;
         END IF;
      ELSIF NOT l_prim_prod_found THEN
         IF (g_debug = gme_debug.g_log_statement) THEN
            gme_debug.put_line ('I could not find the prim product');
         END IF;
      END IF;

      IF (g_debug = gme_debug.g_log_statement) THEN
         gme_debug.put_line ('Checking dates against validity rule');
      END IF;

      IF p_creation_mode <> 'LCF' THEN
         IF (NOT gme_common_pvt.check_validity_rule_dates
                              (l_recipe_validity_rule.recipe_validity_rule_id
                              ,x_batch_header_rec.plan_start_date
                              ,x_batch_header_rec.plan_cmplt_date) ) THEN
            x_return_status := fnd_api.g_ret_sts_error;
            RAISE date_exceed_validity_rule;
         END IF;
      END IF;                           /* IF p_creation_mode <> 'LCF' THEN */

      IF (g_debug = gme_debug.g_log_statement) THEN
         gme_debug.put_line (   'BATCH CREATED, ID = '
                             || TO_CHAR (x_batch_header_rec.batch_id) );
      END IF;

      /* Update WIP entity table to put the primary product id */
      UPDATE wip_entities
         SET primary_item_id = l_recipe_validity_rule.inventory_item_id
       WHERE wip_entity_id = x_batch_header_rec.batch_id;

     -- Rajesh Patangya Bug # 6752637
      /* Now its time to calculate the MTQ for steps based on the product */
      IF gme_common_pvt.routings.COUNT <> 0 THEN
         IF (NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT) THEN
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || 'Calling Update_step_mtq');
         END IF;

         IF NOT update_step_mtq (x_batch_header_rec.batch_id ) THEN
            RAISE update_step_mtq_failure;
         END IF;
      END IF;

-- ==================
      -- Bug 17433833 Moved batch number generation lower in the logic.
      phantom_found := 0;
      FOR l_row_count IN 1 .. l_number_of_formula_lines LOOP
         IF phantom_found = 0 THEN
            /* If it is set as an automatic phantom */
            IF     l_material_details (l_row_count).phantom_type = 1
               AND l_material_details (l_row_count).line_type = -1
               AND l_doc_numbering <> 1 THEN
               phantom_found := 1;
            END IF;
         END IF;
      END LOOP;            /* l_row_count IN 1 .. l_number_of_formula_lines */
                     
      -- 5698727 rework
      -- Bug#6398619 the validation here is required only for the automatic doc numbering
      -- Bug 17433833 If there are phantoms we must generate batch number now.      
      IF l_doc_numbering = gme_common_pvt.g_auto_doc_numbering AND phantom_found = 1 THEN        

         l_in_batch_header1 := x_batch_header_rec ;
         gme_common_pvt.create_document_no (l_in_batch_header1
                                           ,x_batch_header_rec);
         
         -- Bug 13785754/13815190 - try 20 times if necessary to get a unique batch number.
         l_doc_count := 1;
         WHILE l_doc_count < 20 LOOP

            validate_wip_entity(p_organization_id => x_batch_header_rec.organization_id,
                                p_batch_no        => l_prefix||x_batch_header_rec.batch_no,
                                x_return_status   => l_return_status);
            
            IF l_return_status <> fnd_api.g_ret_sts_success THEN
               IF l_doc_count > 18 THEN
                  RAISE wip_entity_err;
               ELSE
                  l_doc_count := l_doc_count + 1;                 
                  gme_common_pvt.create_document_no (l_in_batch_header1
                                                    ,x_batch_header_rec);              
               END IF;           
            ELSE           
               -- Force loop to stop.
               l_doc_count := 99;
            END IF;                     
         END LOOP;         
           
         -- update the batch_no with the actual value
         UPDATE gme_batch_header
            SET batch_no = x_batch_header_rec.batch_no
	         -- Bug 13256866
	         ,last_updated_by = gme_common_pvt.g_user_ident
	         ,last_update_date = gme_common_pvt.g_timestamp
	         ,last_update_login = gme_common_pvt.g_login_id
          WHERE batch_id = x_batch_header_rec.batch_id;
         
         -- 5698727 rework update the wip_entities table with actual batch no
         UPDATE wip_entities
            SET wip_entity_name = l_prefix||x_batch_header_rec.batch_no
          WHERE organization_id = x_batch_header_rec.organization_id
            AND wip_entity_name = l_prefix||l_doc_timestamp;
      END IF;
-- ==================

      IF g_debug <= gme_debug.g_log_statement THEN
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || 'Start phantom processing');
      END IF;

      /* Now its time to create the phantom batch based on the setting of the phantom type - Automatic */
      l_error_count := fnd_msg_pub.count_msg;

      FOR l_row_count IN 1 .. l_number_of_formula_lines LOOP
         /* If it is set as an automatic phantom */
         IF     l_material_details (l_row_count).phantom_type = 1
            AND l_material_details (l_row_count).line_type = -1
            AND l_doc_numbering <> 1 THEN
            l_material_details_in := l_material_details (l_row_count);

-- nsinghi bug#5200395. Changed out var for x_exception_material_tbl to l_phantom_exc_material_tbl_out 
-- from x_exception_material_tbl
            /* Bug 5512352 If creating phantom from here no need to do shortage check since 
               now get_batch_shortages will do it for phantom batches also */
            g_no_phant_short_check := 1;
            
            -- Bug 7830838 - Use the same value from the parent batch so that FPL 
            -- gets properly applied or not applied based on user settings.
            l_gme_batch_header.fixed_process_loss_applied := p_batch_header_rec.fixed_process_loss_applied;
            gme_phantom_pvt.create_phantom
                   (p_material_detail_rec         => l_material_details_in
                   ,p_batch_header_rec            => l_gme_batch_header -- would be null in this case
                   ,x_material_detail_rec         => l_material_details
                                                                  (l_row_count)
                   ,p_validity_rule_id            => NULL
                   ,p_use_workday_cal             => p_use_workday_cal
                   ,p_contiguity_override         => p_contiguity_override
                   ,p_use_least_cost_validity_rule => p_use_least_cost_validity_rule
                   ,x_exception_material_tbl      => l_phantom_exc_material_tbl_out 
                   ,x_return_status               => l_return_status);
            l_error_count_after := fnd_msg_pub.count_msg;
            g_no_phant_short_check := 0;
            -- nsinghi bug#5200395. This will add the exceptions generated by each phantom ingredient to l_phantom_exc_material_tbl. 
            /* Bug 5512352 No need to add structure since phantoms are now adone along with main batch */
            /*
            IF (l_phantom_exc_material_tbl_out.COUNT) > 0 THEN
              l_excp_tbl_ptr := NVL(l_phantom_exc_material_tbl.LAST,0);
              FOR l_phntm_exc_cnt IN l_phantom_exc_material_tbl_out.FIRST..l_phantom_exc_material_tbl_out.LAST
              LOOP
                l_phantom_exc_material_tbl(l_excp_tbl_ptr + l_phntm_exc_cnt) := l_phantom_exc_material_tbl_out(l_phntm_exc_cnt);
              END LOOP;
            END IF;
            */

            IF l_error_count < l_error_count_after THEN
               WHILE l_error_count_after > l_error_count LOOP
                  fnd_msg_pub.delete_msg (p_msg_index      => l_error_count_after);
                  l_error_count_after := l_error_count_after - 1;
               END LOOP;
            END IF;                  /* l_error_count < l_error_count_after */
         END IF;        /* l_material_details(l_row_count).phantom_type = 1 */
      END LOOP;            /* l_row_count IN 1 .. l_number_of_formula_lines */
 

      -- Bug 7265006 - Moved g_no_phant_short_check to shortage check condition as it was originally intended
      -- for shortage checking only. Unfortunately it stopped all the other logic for phantom batches.
      -- Now invisible move orders and high level reservations will get created for phantom batches.
      IF x_batch_header_rec.batch_type = 0 AND NVL (x_batch_header_rec.update_inventory_ind, 'Y') = 'Y' THEN
         /* Check inventory shortages */
         IF (gme_common_pvt.g_check_shortages_ind = 1 AND g_no_phant_short_check = 0) THEN
            IF g_debug <= gme_debug.g_log_statement THEN
               gme_debug.put_line (   g_pkg_name || '.'|| l_api_name|| ' Calling shortages ');
             END IF;
            gme_common_pvt.get_batch_shortages
                     (p_organization_id      => x_batch_header_rec.organization_id
                     ,p_batch_id             => x_batch_header_rec.batch_id
                     ,p_invoke_mode          => 'O'
                     ,p_tree_mode            => gme_common_pvt.g_tree_reservation_mode
                     ,x_return_status        => l_return_status
                     ,x_exception_tbl        => x_exception_material_tbl);
            IF l_return_status <> fnd_api.g_ret_sts_success THEN
               RAISE inventory_shortage;
            END IF;
            -- nsinghi Add the material exceptions from phantom batch to Parent Batch
            /*
            IF (l_phantom_exc_material_tbl.COUNT) > 0 THEN
              l_excp_tbl_ptr := NVL(x_exception_material_tbl.LAST,0);
              FOR l_phntm_exc_cnt IN l_phantom_exc_material_tbl.FIRST..l_phantom_exc_material_tbl.LAST
              LOOP
                x_exception_material_tbl(l_excp_tbl_ptr + l_phntm_exc_cnt) := l_phantom_exc_material_tbl(l_phntm_exc_cnt);
              END LOOP;
            END IF;
            */
            IF (x_exception_material_tbl.COUNT > 0) THEN
               x_return_status := gme_common_pvt.g_inv_short_err;
            END IF;
         END IF;

         /* Create invisible move order */
         IF g_debug <= gme_debug.g_log_statement THEN
            gme_debug.put_line (   g_pkg_name|| '.'|| l_api_name|| ' Creating Invisible Move Order ');
         END IF;

         gme_move_orders_pvt.create_move_order_hdr
            (p_organization_id           => x_batch_header_rec.organization_id
            ,p_move_order_type           => gme_common_pvt.g_invis_move_order_type
            ,x_move_order_header_id      => x_batch_header_rec.move_order_header_id
            ,x_return_status             => l_return_status);

         IF g_debug <= gme_debug.g_log_statement THEN
            gme_debug.put_line (   g_pkg_name|| '.'|| l_api_name|| 'Invisible Move Order is Created');
         END IF;

         IF (l_return_status <> fnd_api.g_ret_sts_success) THEN
            RAISE create_mo_hdr_err;
         ELSE
            IF g_debug <= gme_debug.g_log_statement THEN
               gme_debug.put_line (   g_pkg_name|| '.'|| l_api_name|| ' Creating Invisible Move Order Lines');
            END IF;

            l_in_material_details := l_material_details;
            gme_move_orders_pvt.create_move_order_lines
               (p_move_order_header_id      => x_batch_header_rec.move_order_header_id
               ,p_move_order_type           => gme_common_pvt.g_invis_move_order_type
               ,p_material_details_tbl      => l_in_material_details
               ,x_material_details_tbl      => l_material_details
               ,x_trolin_tbl                => l_trolin_tbl
               ,x_return_status             => l_return_status);

            IF (l_return_status <> fnd_api.g_ret_sts_success) THEN
               RAISE create_mo_line_err;
            END IF;

            IF g_debug <= gme_debug.g_log_statement THEN
               gme_debug.put_line (   g_pkg_name|| '.'|| l_api_name|| 'Invisible Move Order Lines is Created');
            END IF;
         END IF;

         /* Update batch header with move_order_header_id */
         UPDATE gme_batch_header
            SET move_order_header_id = x_batch_header_rec.move_order_header_id
	        -- Bug 13256866
	        ,last_updated_by = gme_common_pvt.g_user_ident
	        ,last_update_date = gme_common_pvt.g_timestamp
	        ,last_update_login = gme_common_pvt.g_login_id
          WHERE batch_id = x_batch_header_rec.batch_id;

         /* Update material details table we have computed material requirement date and move_order_line_id */
         FOR i IN 1 .. l_material_details.COUNT LOOP
            -- Bug 7710435 - Added p_called_by parameter.
            l_return :=
                gme_material_details_dbl.update_row (p_material_detail => l_material_details (i), 
                                                     p_called_by =>'C' );
         END LOOP;

         -- Bug 7710435  - Let's refetch the detail row since the data has been updated.
         l_in_material_detail.batch_id := x_batch_header_rec.batch_id;
         IF NOT gme_material_details_dbl.fetch_tab(l_in_material_detail, l_material_details) THEN
            RAISE material_dtl_fetch_failure; 
         END IF;

         /* Create High Level Reservations */
         IF (gme_common_pvt.g_create_high_level_resv_ind = 1) THEN
            IF g_debug <= gme_debug.g_log_statement THEN
               gme_debug.put_line (   g_pkg_name|| '.'|| l_api_name|| ' Creating reservations for batch id '|| x_batch_header_rec.batch_id);
            END IF;

            gme_reservations_pvt.create_batch_reservations
                       (p_batch_id           => x_batch_header_rec.batch_id
                       ,p_timefence          => gme_common_pvt.g_reservation_timefence
                       ,x_return_status      => l_return_status);

            IF g_debug <= gme_debug.g_log_statement THEN
               gme_debug.put_line (   g_pkg_name
                                   || '.'
                                   || l_api_name
                                   || ' Reservations are created'
                                   || l_return_status);
            END IF;
         END IF;
         
         -- Bug 21187113 - If auto detail is activated for create batch as well as create move order
         --                then bypass this code and call real picking later in the process.
         IF g_debug <= gme_debug.g_log_statement THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ' gme_common_pvt.g_auto_detail_batch is '||gme_common_pvt.g_auto_detail_batch);
         END IF;
         
         /* Create Move Order */
         IF (gme_common_pvt.g_create_move_orders_ind = 1 AND gme_common_pvt.g_auto_detail_batch NOT IN (1,3)) THEN
            IF g_debug <= gme_debug.g_log_statement THEN
               gme_debug.put_line (   g_pkg_name
                                   || '.'
                                   || l_api_name
                                   || ' Creating Explicit Move Order ');
            END IF;

            gme_move_orders_pvt.create_batch_move_order
                                (p_batch_header_rec          => x_batch_header_rec
                                ,p_material_details_tbl      => l_material_details
                                ,x_return_status             => l_return_status);

            IF g_debug <= gme_debug.g_log_statement THEN
               gme_debug.put_line (   g_pkg_name
                                   || '.'
                                   || l_api_name
                                   || ' Explicit Move Order are created '
                                   || l_return_status);
            END IF;
         END IF;
      END IF;
      
-- ==================
      -- Bug 17433833 Moved batch number generation lower in the logic.                     
      -- If there were no phantoms we must generate batch number now.      
      IF l_doc_numbering = gme_common_pvt.g_auto_doc_numbering AND phantom_found = 0 THEN        

         l_in_batch_header1 := x_batch_header_rec ;
         gme_common_pvt.create_document_no (l_in_batch_header1
                                           ,x_batch_header_rec);
         
         -- Bug 13785754/13815190 - try 20 times if necessary to get a unique batch number.
         l_doc_count := 1;
         WHILE l_doc_count < 20 LOOP

            validate_wip_entity(p_organization_id => x_batch_header_rec.organization_id,
                                p_batch_no        => l_prefix||x_batch_header_rec.batch_no,
                                x_return_status   => l_return_status);
            
            IF l_return_status <> fnd_api.g_ret_sts_success THEN
               IF l_doc_count > 18 THEN
                  RAISE wip_entity_err;
               ELSE
                  l_doc_count := l_doc_count + 1;                 
                  gme_common_pvt.create_document_no (l_in_batch_header1
                                                    ,x_batch_header_rec);              
               END IF;           
            ELSE           
               -- Force loop to stop.
               l_doc_count := 99;
            END IF;                     
         END LOOP;         
           
         -- update the batch_no with the actual value
         UPDATE gme_batch_header
            SET batch_no = x_batch_header_rec.batch_no
	         -- Bug 13256866
	         ,last_updated_by = gme_common_pvt.g_user_ident
	         ,last_update_date = gme_common_pvt.g_timestamp
	         ,last_update_login = gme_common_pvt.g_login_id
          WHERE batch_id = x_batch_header_rec.batch_id;
         
         -- 5698727 rework update the wip_entities table with actual batch no
         UPDATE wip_entities
            SET wip_entity_name = l_prefix||x_batch_header_rec.batch_no
          WHERE organization_id = x_batch_header_rec.organization_id
            AND wip_entity_name = l_prefix||l_doc_timestamp;
      END IF;
-- ==================
      
        -- Pawan added code for populating the GMO 
        IF x_batch_header_rec.enhanced_pi_ind = 'Y' THEN
          gmo_vbatch_grp.instantiate_advanced_pi
                                 (P_API_VERSION => 1.0,
                                  P_INIT_MSG_LIST => FND_API.G_FALSE,
                                   P_COMMIT => FND_API.G_FALSE,
                                   P_VALIDATION_LEVEL => FND_API.G_VALID_LEVEL_FULL,
                                   X_RETURN_STATUS => l_return_status,
                                   X_MSG_COUNT => l_message_count,
                                   X_MSG_DATA => l_msg_data,
                                   P_ENTITY_NAME => 'BATCH',
                                   P_ENTITY_KEY => x_batch_header_rec.batch_id) ;
           IF g_debug <= gme_debug.g_log_statement THEN
               gme_debug.put_line (   g_pkg_name
                                   || '.'
                                   || l_api_name
                                   || ' Calling instantiate_advanced_pi ');
            END IF;
           IF l_return_status <> fnd_api.g_ret_sts_success THEN
               RAISE gmo_instantiate_err;
           END IF; 
         END IF ;    
        /*                                    
        BEGIN ER 19161894                    
        Shaliu Chen 18-JUL-2014               
        Create a requisition if the batch include Outside resource. 
        */  
        IF gme_osp.Check_Release_Version THEN             
          IF g_debug <= gme_debug.g_log_statement THEN
             gme_debug.put_line (   g_pkg_name
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

            IF NVL(l_po_creation_time,0) = gme_osp.g_batch_creation THEN
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
                Shaliu Chen 02-APR-2015               
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
                                                x_message_list          => l_msg_data,
                                                x_message_count         => l_message_count);

                   IF (l_return_status <> fnd_api.g_ret_sts_success) THEN
                     IF g_debug <= gme_debug.g_log_statement THEN
                         gme_debug.put_line (   g_pkg_name
                                             || '.'
                                             || l_api_name
                                             || ' Calling requisition creation API failed,'||l_msg_data);
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
       /*Bug#7493614 moved the workflow event call to the end of the batch
            create procedure */
      wf_event.RAISE (p_event_name      => gme_common_pvt.G_BATCH_CREATED
                     ,p_event_key       => x_batch_header_rec.batch_id);
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;
   EXCEPTION
      WHEN doc_num_is_not_passed THEN
         x_return_status := fnd_api.g_ret_sts_error;
         gme_common_pvt.log_message ('GME_DOC_NUM_NOT_PASSED');
      WHEN gme_duplicate_batch THEN
         x_return_status := fnd_api.g_ret_sts_error;
         --FPBug#4898641
         IF x_batch_header_rec.batch_type = 0 THEN
          gme_common_pvt.log_message ('GME_DUP_BATCH');
         ELSE
          gme_common_pvt.log_message ('GME_DUP_FPO');
         END IF;
      WHEN steps_creation_failure THEN
         NULL;
      WHEN error_create_phantom OR invalid_header_values OR invalid_recipe THEN
         x_return_status := l_return_status;
     --FPBug#4351032
      WHEN validation_failure OR expected_error OR scaling_failure OR date_exceed_validity_rule OR batch_header_fetch_failure OR error_count_exceeded OR expected_error OR unexpected_error OR invalid_item_substitute OR material_dtl_fetch_failure THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN error_cont_period THEN
         IF (g_debug = gme_debug.g_log_statement) THEN
            gme_debug.put_line ('Contiguity period ... _failed');
         END IF;

         x_return_status := l_return_status;
      WHEN error_non_contiguious THEN
         IF (g_debug = gme_debug.g_log_statement) THEN
            gme_debug.put_line ('Contiguity period ... not found');
         END IF;

         gme_common_pvt.log_message ('GME_NON_CONTIGUOUS_TIME');
         x_return_status := 'C';
      WHEN conversion_failure THEN
         IF l_item_no IS NULL THEN
            OPEN cur_item_no (l_item_id, x_batch_header_rec.organization_id);

            FETCH cur_item_no
             INTO l_item_no;

            CLOSE cur_item_no;
         END IF;

         x_return_status := fnd_api.g_ret_sts_error;
         fnd_message.set_name ('GMI', 'IC_API_UOM_CONVERSION_ERROR');
         fnd_message.set_token ('ITEM_NO', l_item_no);
         fnd_message.set_token ('FROM_UOM', l_from_uom);
         fnd_message.set_token ('TO_UOM', l_to_uom);
         fnd_msg_pub.ADD;
      WHEN  Fixed_process_loss_failure THEN
          IF (g_debug = gme_debug.g_log_statement) THEN
            gme_debug.put_line ('Failed to apply FPL');
         END IF;
      WHEN create_by_product_failure THEN
         gme_common_pvt.log_message ('GME_API_CREATE_BY_PROD_FAIL');
         x_return_status := fnd_api.g_ret_sts_error;

         IF (g_debug = gme_debug.g_log_statement) THEN
            gme_debug.put_line ('GME_API_CREATE_BY_PROD_FAIL');
         END IF;
      WHEN error_in_integer_scale THEN  -- 10624995
         gme_common_pvt.log_message ('GME_INTEGER_MULTIPLE_SCALE_ERR');
         x_return_status := FND_API.g_ret_sts_error; 
                 
         IF (g_debug = gme_debug.g_log_statement) THEN
            gme_debug.put_line ('Integer scaling failed for non scaled batch..  should not happen !!');
         END IF;
      WHEN inventory_shortage OR create_mo_hdr_err OR create_mo_line_err OR
           calc_mtl_req_date_err OR gmo_instantiate_err OR wip_entity_err THEN
         x_return_status := l_return_status;
      WHEN error_truncate_date THEN
         IF (g_debug = gme_debug.g_log_statement) THEN
            gme_debug.put_line ('truncate date procedure error');
         END IF;

         x_return_status := fnd_api.g_ret_sts_error;
      --FPBug#4395561
      WHEN create_flex_failure THEN
         x_return_status := l_return_status;
         IF (NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT) THEN
            gme_debug.put_line ('Creating the default values of the DFF failure');
         END IF;
     -- Rajesh Patangya Bug # 6752637
      WHEN update_step_mtq_failure THEN
         x_return_status := fnd_api.g_ret_sts_error;
         IF (g_debug = gme_debug.g_log_statement) THEN
            gme_debug.put_line ('MTQ Calculation failure');
         END IF;
      /*                                    
      BEGIN ER 19161894                    
      Shaliu Chen 18-JUL-2014               
      Added for OPM Step Level OSP Project  
      */            
      WHEN create_requisition_failure THEN
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

         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);

         IF (g_debug = gme_debug.g_log_statement) THEN
            gme_debug.put_line (SQLERRM);
         END IF;
         x_return_status := fnd_api.g_ret_sts_unexp_error;
   END create_batch;

 /************************************************************
 *  10-JAN-2008 Rajesh Patangya Bug # 6752637                 *
 *   UPDATE_STEP_MTQ                                          *
 *   MTQ Quantity should be calculated based on product       *
 *************************************************************/

   FUNCTION update_step_mtq (p_batch_id IN NUMBER)
                   RETURN BOOLEAN IS

      l_batch_steps         gme_batch_steps%ROWTYPE;
      l_calculated_mtq      gme_batch_steps.minimum_transfer_qty%TYPE;
      l_api_name            CONSTANT VARCHAR2 (30) := 'UPDATE_STEP_MTQ';
      
      l_numerator           NUMBER;
      l_total_output_qty    NUMBER;
      l_step_qty            NUMBER;

     CURSOR step_mtq_cursor (x_batch_id gme_batch_header.batch_id%TYPE) IS
       SELECT *
       FROM  gme_batch_steps
       WHERE batch_id = x_batch_id ;

     CURSOR calculate_mtq_cursor (x_batch_id gme_batch_header.batch_id%TYPE,
                         x_routingstep_id fm_rout_dtl.routingstep_id%TYPE) IS

     -- Bug 8226667 - Changed select from inline calculation to using variables.
/*
     SELECT
        (frh.routing_qty * NVL(frd.minimum_transfer_qty,0)
        * DECODE(fmd.detail_uom, iim.primary_uom_code, fmd.qty,
          inv_convert.inv_um_convert
                  (fmd.inventory_item_id,
                   5,
                   fmd.qty,
                   fmd.detail_uom,
                   iim.primary_uom_code,
                   NULL,NULL)
                )
         )
        / (ffm.total_output_qty * frd.step_qty) 
*/        
     SELECT
        (frh.routing_qty * NVL(frd.minimum_transfer_qty,0)
        * DECODE(fmd.detail_uom, iim.primary_uom_code, fmd.qty,
          inv_convert.inv_um_convert
                  (fmd.inventory_item_id,
                   5,
                   fmd.qty,
                   fmd.detail_uom,
                   iim.primary_uom_code,
                   NULL,NULL)
                )
         ),
        ffm.total_output_qty, 
        frd.step_qty
      FROM fm_form_mst ffm,
           fm_matl_dtl fmd,
           fm_rout_hdr frh,
           fm_rout_dtl frd,
           mtl_system_items iim,
           gme_batch_header gbh,
           gmd_recipe_validity_rules ffe
      WHERE gbh.batch_id   = x_batch_id
        AND ffm.formula_id = gbh.formula_id
        AND ffm.formula_id = fmd.formula_id
	AND iim.organization_id = NVL(ffe.organization_id,iim.organization_id)
        AND fmd.inventory_item_id = ffe.inventory_item_id
        AND iim.inventory_item_id = fmd.inventory_item_id
	AND iim.organization_id = fmd.organization_id
        AND frh.routing_id = gbh.routing_id
        AND frh.routing_id = frd.routing_id
        AND frd.routingstep_id = l_batch_steps.routingstep_id 
        AND ffe.recipe_validity_rule_id = gbh.recipe_validity_rule_id ;

   BEGIN
        OPEN step_mtq_cursor (p_batch_id);
        LOOP
           FETCH step_mtq_cursor INTO l_batch_steps ;
           EXIT WHEN step_mtq_cursor%NOTFOUND;
           
           IF (NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT) THEN
             gme_debug.put_line (' MTQ = ' || NVL(l_batch_steps.minimum_transfer_qty,0)
             || ' Step_qty = ' || NVL(l_batch_steps.plan_step_qty,0)
             || ' Routingstep_id = ' || l_batch_steps.routingstep_id );
           END IF;
           
           IF NVL(l_batch_steps.minimum_transfer_qty,0) > 0 AND
              NVL(l_batch_steps.plan_step_qty,0) > 0 THEN
           
              -- Bug 8226667 - Changed select from inline calculation to using variables.
              OPEN calculate_mtq_cursor (p_batch_id, l_batch_steps.routingstep_id);
              FETCH calculate_mtq_cursor INTO l_numerator, l_total_output_qty, l_step_qty  ;
              CLOSE calculate_mtq_cursor ;
              
              IF (l_step_qty * l_total_output_qty) > 0 THEN
                 l_calculated_mtq := l_numerator / (l_step_qty * l_total_output_qty);
              
                 UPDATE gme_batch_steps
                   SET minimum_transfer_qty = l_calculated_mtq
                 WHERE batch_id = l_batch_steps.batch_id
                   AND routingstep_id =  l_batch_steps.routingstep_id
                   AND batchstep_id = l_batch_steps.batchstep_id ;
                 
                 IF (NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT) THEN
                     gme_debug.put_line (' Update Calculated MTQ = ' || l_calculated_mtq );
                 END IF;
              ELSE
                 IF (NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT) THEN
                     gme_debug.put_line (' Cannot Calculated MTQ for step id ' || l_batch_steps.routingstep_id);
                     gme_debug.put_line (' step qty is     ' || l_step_qty);
                     gme_debug.put_line (' total output is ' || l_total_output_qty);
                 END IF;
                 NULL;  -- This is here just in case debug is not on.
              END IF;
           END IF;
        END LOOP ;
        CLOSE step_mtq_cursor;

      RETURN TRUE ;

   EXCEPTION
      WHEN no_data_found THEN
         RETURN TRUE ;
      WHEN OTHERS THEN
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
         IF (NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT) THEN
            gme_debug.put_line (SQLERRM);
         END IF;
         RETURN FALSE ;
   END update_step_mtq ;

END gme_create_batch_pvt;
/
-- show errors;
COMMIT ;
EXIT;
