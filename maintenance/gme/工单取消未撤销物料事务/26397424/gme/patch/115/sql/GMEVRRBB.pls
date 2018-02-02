/* +======================================================================+ */
/* |    Copyright (c) 2005, 2015 Oracle and/or its affiliates.           | */
/* |                         All rights reserved.                         | */
/* |                           Version 12.0.0                             | */
/* +======================================================================+ */
REM dbdrv: sql ~PROD ~PATH ~FILE none none none package &phase=plb \
REM dbdrv: checkfile(120.10.12010000.6=120.10.12020000.5)(120.10.12000000.3=120.10.12010000.3)(115.29=120.1):~PROD:~PATH:~FILE
SET VERIFY OFF
WHENEVER SQLERROR EXIT FAILURE ROLLBACK;
WHENEVER OSERROR EXIT FAILURE ROLLBACK;
REM **********************************************************************
REM *                                                                    *
REM * FILE:    GMEVRRB.pls                                               *
REM * PURPOSE: Package Body for the GME REROUTE BATCH routines           *
REM * AUTHOR:  Thomas Daniel, OPM Development                            *
REM * DATE:    May 07th 2001                                             *
REM * HISTORY:                                                           *
REM * ========                                                           *
REM * 07MAY01  Thomas Daniel                                             *
REM *          Created                                                   *
REM * 14-AUG-01 Sukarna Reddy
REM *           Added Code for calculating Routing_Scale_factor.         *
REM * 26-JUL-02 Bharati satpute                                          *
REM *           Added code for enforce step dependency
REM * 05-SEP-02 Pawan kumar  bug 2509572 added code for process parameters
REM * 13-AUG-03 P.Raghu  Bug#3078271                                     *
REM *           Modified code for calculating Routing Scale factor from  *
REM *           Formula Quantities and Batch Quantities                  *
REM *  Pawan Kumar 09-16-2003 Bug 823188                                 *
REM *            Modified the procedures call for shop calendar          *
REM * 15-OCT-03 P.Raghu  Bug#3005804                                     *
REM *           Added code in Exception Handler when conversion_failure  *
REM *           exception is raised.                                     *
REM * 15-APR-03 Chandrashekar Tiruvidula  Bug#3406630                    *
REM *           Added code in validate_validity procedure so that the    *
REM *           sum of the products is calculated and validated against  *
REM *           the validity rule quantities.                            *
REM * 21-04-2004 Rishi Varma Bug 3037513.
REM *          Added code so that the after the rerouting the trans.dates*
REM *           for the batch are updated accordinlgy.
REM * 27-06-2006 Sunitha ch Bug 5353941 .                                * 
REM *           Sunitha Ch Bug 5353941 seperated the check whether the   * 
REM *           material is assciated with step or not and if it is not  *
REM *           assciated then added a select statement to retrieve the  * 
REM *           planed start date of the parent batch into               *
REM *           l_phantom_batch_header_rec.plan_cmplt_date               *
REM * 25-07-2006 Sunitha ch Bug#5391396 included the code to             *
REM *            rescedule_step of the child batch when the product      *
REM *            is associated to step and its release type is autobystep*

REM * 30-09-2010 G. Muratore   Bug 10087071
REM *            Pass in specific organization_id to get recipe. When we 
REM *            passed in NULL it caused some memory issue in gmd code
REM *            when calling gmd_auto_step_calc.calc_step_qty for asqc batches.

REM * 01-DEC-2014 G. Muratore   Bug 19888172
REM *             Refresh batch record from table to avoid stale data.

REM * 03-MAR-2015 G. Muratore   Bug 20615823
REM *             Update the batch header record and then refetch it.
REM **********************************************************************

/*************************************************************************
* This file contains the procedure for rerouting batches in Oracle       *
* Process Manufacturing (OPM). Each procedure has a common set of        *
* parameters to which API-specific parameters are appended.              *
*************************************************************************/

CREATE OR REPLACE PACKAGE BODY gme_reroute_batch_pvt AS
   /* $Header: GMEVRRBB.pls 120.10.12020000.5 2015/03/03 12:48:13 gmurator ship $ */
   g_debug               VARCHAR2 (5)  := fnd_profile.VALUE ('AFLOG_LEVEL');
   g_pkg_name   CONSTANT VARCHAR2 (30) := 'GME_REROUTE_BATCH_PVT';

   /*===========================================================================================
   Procedure
     reroute_batch
   Description
     This particular procedure is used to reroute the batch to a different validity rule.
   Parameters
     p_batch_header                 The batch header record.
     p_validity_rule_id             Validity rule id.
     x_batch_header     Out NOCOPY  record for batch header
     x_return_status    outcome of the API call
               S - Success
               E - Error
               U - Unexpected error
               C - No continous periods found
    History
    Sunitha Ch. Bug 5353941 seperated the check whether the material is assciated with step or not
    and if it is not assciated then added a select statement to retrieve the planed start date of the 
    parent batch into l_phantom_batch_header_rec.plan_cmplt_date
    Susruth Bug#5359091 Finite Scheduled indicator is set back to 0 once the batch is rerouted.
    Sunitha Ch. bug 5353941 REWORK  Check the release type of material also alog 
    with association to step.If release type is automatic then call rescedule batch of the phantom batch
    Sunitha ch Bug#5391396 included the code to  rescedule_step of the child batch when the product 
    is associated to step and its release type is autobystep.
    18-JUL-2014     Shaliu Chen       ER 19161894                                                 
    Modify create_batch to invoke requisition creation program if batch include OSP step     

    01-DEC-2014 G. Muratore   Bug 19888172
                Refresh batch record from table to avoid stale data.    

    03-MAR-2015 G. Muratore   Bug 20615823
                Update the batch header record and then refetch it.
   =============================================================================================*/
   PROCEDURE reroute_batch (
      p_batch_header_rec      IN              gme_batch_header%ROWTYPE
     ,p_validity_rule_id      IN              NUMBER
     ,p_use_workday_cal       IN              VARCHAR2
            DEFAULT fnd_api.g_false
     ,p_contiguity_override   IN              VARCHAR2
            DEFAULT fnd_api.g_false
     ,x_batch_header_rec      OUT NOCOPY      gme_batch_header%ROWTYPE
     ,x_return_status         OUT NOCOPY      VARCHAR2)
   IS
      l_api_name              CONSTANT VARCHAR2 (30)       := 'REROUTE_BATCH';
      l_formula_master                 fm_form_mst%ROWTYPE;
      CURSOR cur_get_recipe (v_validity_rule_id IN NUMBER)
      IS
         SELECT r.recipe_id, r.routing_id, calculate_step_quantity,v.inventory_item_id
           FROM gmd_recipe_validity_rules v, gmd_recipes r
          WHERE recipe_validity_rule_id = v_validity_rule_id
            AND r.recipe_id = v.recipe_id;

      CURSOR cur_get_max_step_date
      IS
         SELECT MAX (plan_cmplt_date)
           FROM gme_batch_steps
          WHERE batch_id = p_batch_header_rec.batch_id;

      CURSOR cur_get_material (v_batch_id NUMBER)
      IS
         SELECT material_detail_id, phantom_id
           FROM gme_material_details
          WHERE batch_id = v_batch_id;
          
      CURSOR get_prim_prod (v_batch_id NUMBER, v_inventory_item_id NUMBER)
      IS
         SELECT   plan_qty, dtl_um
             FROM gme_material_details
            WHERE batch_id = v_batch_id
              AND inventory_item_id = v_inventory_item_id
              AND line_type = gme_common_pvt.g_line_type_prod
         ORDER BY line_no ASC; 
                  
      /*ER 19161894  Shaliu Chen 18-JUL-2014*/   
      CURSOR cur_get_osp_parameter(v_organization_id NUMBER)
      IS
         SELECT propagate_change_to_po
           FROM gme_parameters
          WHERE organization_id = v_organization_id;         

      l_material_detail_ids_tab        gme_common_pvt.number_tab;
      l_material_detail_rec            gme_material_details%ROWTYPE;
      l_return_code                    NUMBER;
      l_recipe_id                      NUMBER;
      l_temp_qty                       NUMBER                        DEFAULT 0;
      l_message_count                  NUMBER;
      l_message_list                   VARCHAR2 (1000);
      l_max_step_date                  DATE;
      l_process_parameters_tab         gmd_recipe_fetch_pub.recp_resc_proc_param_tbl;
      l_gme_material_details           gme_material_details%ROWTYPE;
      l_material_details               gme_common_pvt.material_details_tab;
      l_total_output_qty_b             NUMBER;
      l_total_output_qty_c             NUMBER;
      l_phantom_ids_tab                gme_common_pvt.number_tab;
      l_phantom_associated_to_step     BOOLEAN;
      l_phantom_batch_header_rec       gme_batch_header%ROWTYPE;
      l_phantom_batch_header_rec_out   gme_batch_header%ROWTYPE;
      no_continous_periods             EXCEPTION;
      l_no_steps                       BOOLEAN := FALSE;
      l_inventory_item_id              pls_integer;
      l_prim_item_um                   mtl_units_of_measure.uom_code%TYPE;
      l_prim_prod_um                   mtl_units_of_measure.uom_code%TYPE; 
      l_prim_prod_qty                  NUMBER;     
      l_prim_prod_found                BOOLEAN :=FALSE;
      l_plan_start_date                DATE;
      l_plan_cmplt_date                DATE;
      l_no_prod_rule_found             BOOLEAN;
      l_W_ReturnedAtLeastOnce          BOOLEAN := FALSE;
      l_phantom_batch_step_rec  gme_batch_steps%ROWTYPE;
      l_step_tbl     gme_reschedule_step_pvt.step_tab;
      x_batch_step_rec gme_batch_steps%ROWTYPE;
      error_dbl              EXCEPTION;
      l_proc                 VARCHAR2(100);
      --FPBug#4585491
      l_R_count                       NUMBER := 0;
      l_M_count                       NUMBER := 0;
      l_B_count                       NUMBER := 0;
	  -- enh 13724630, biachen
	    l_msg_count NUMBER;
      l_msg_data  VARCHAR2(2000);
      /*ER 19161894  Shaliu Chen 18-JUL-2014*/
      l_propagate_change_to_po        NUMBER;
      cancel_recreate_po_failed       EXCEPTION;      
      /*END ER 19161894*/
      
   BEGIN
      IF (NVL (g_debug, 0) IN
                       (gme_debug.g_log_statement, gme_debug.g_log_procedure) ) THEN
         gme_debug.put_line (g_pkg_name || '.' || l_api_name || ':'
                             || 'Entering');
      END IF;

      /* Set the return status to success initially */
      x_return_status := fnd_api.g_ret_sts_success;

      /* Get the Batch header */
      IF (NOT gme_common_pvt.get_batch_header
                                    (p_batch_header_rec      => p_batch_header_rec
                                    ,p_org_code              => NULL
                                    ,p_batch_type            => NULL
                                    ,x_batch_header_rec      => x_batch_header_rec) ) THEN
         RAISE fnd_api.g_exc_error;
      END IF;

      /* Validation for the new validity rule */
      gme_reroute_batch_pvt.validate_validity_id
                                    (p_batch_header_rec      => x_batch_header_rec
                                    ,p_validity_rule_id      => p_validity_rule_id
                                    ,x_return_status         => x_return_status);

      IF (x_return_status = fnd_api.g_ret_sts_error) THEN
         RAISE fnd_api.g_exc_error;
      ELSIF (x_return_status = fnd_api.g_ret_sts_unexp_error) THEN
         RAISE fnd_api.g_exc_unexpected_error;
      END IF;

	  -- enh 13724630, biachen. cancel task action
      IF p_batch_header_rec.enhanced_pi_ind = 'Y' THEN
         GMO_VBATCH_GRP.ON_TASK_ACTION(
           P_API_VERSION      => 1.0,
      	   P_VALIDATION_LEVEL => 100,
           P_ENTITY_NAME      => GMO_CONSTANTS_GRP.ENTITY_BATCH,
           P_ENTITY_KEY       => p_batch_header_rec.batch_id,
           P_TASK             => GMO_CONSTANTS_GRP.ACTION_REROUTE,
      	   X_RETURN_STATUS    => x_return_status,
           X_MSG_COUNT        => l_msg_count,
           X_MSG_DATA         => l_msg_data,
      	   P_TASK_ATTRIBUTE   => null,
      	   P_REQUESTER        => fnd_global.user_id
         );
         IF (x_return_status <> fnd_api.G_RET_STS_SUCCESS) THEN
            RAISE fnd_api.g_exc_error;
         END IF;
      END IF; 
	  
      /* Now we have to delete the existing poc data */
      gme_reroute_batch_pvt.delete_all_steps (x_batch_header_rec.batch_id
                                             ,x_return_status);

      IF (x_return_status = fnd_api.g_ret_sts_error) THEN
         RAISE fnd_api.g_exc_error;
      ELSIF (x_return_status = fnd_api.g_ret_sts_unexp_error) THEN
         RAISE fnd_api.g_exc_unexpected_error;
      END IF;

      /* Get the recipe id for the validity rule */
      OPEN cur_get_recipe (p_validity_rule_id);

      FETCH cur_get_recipe
       INTO l_recipe_id, x_batch_header_rec.routing_id
           ,x_batch_header_rec.automatic_step_calculation
           ,l_inventory_item_id;
      CLOSE cur_get_recipe;

      x_batch_header_rec.recipe_validity_rule_id := p_validity_rule_id;

      -- 20615823 - Lets update the batch header record and then refetch it.
      IF x_batch_header_rec.routing_id IS NOT NULL THEN
         x_batch_header_rec.poc_ind := 'Y';
      END IF;
      
      IF NOT gme_batch_header_dbl.update_row (x_batch_header_rec) THEN
         RAISE fnd_api.g_exc_error;
      END IF;
      
      IF NOT gme_batch_header_dbl.fetch_row (x_batch_header_rec, x_batch_header_rec) THEN
               RAISE fnd_api.g_exc_error;
      END IF;
      
      /* Populate the routing information from the recipe */
      --{
      IF x_batch_header_rec.routing_id IS NOT NULL THEN
         -- x_batch_header_rec.poc_ind := 'Y';
         gmd_recipe_data_pub.get_recipe_data
                 (p_api_version                   => 1
                 ,p_init_msg_list                 => fnd_api.g_false
                 ,p_recipe_id                     => l_recipe_id
                 -- ,p_organization_id               => NULL
                 ,p_organization_id               => x_batch_header_rec.organization_id  -- Bug 10087071
                 ,x_return_status                 => x_return_status
                 ,x_msg_count                     => l_message_count
                 ,x_msg_data                      => l_message_list
                 ,x_return_code                   => l_return_code
                 ,x_recipe_rout_tbl               => gme_common_pvt.routings
                 ,x_recipe_rout_matl_tbl          => gme_common_pvt.routing_materials
                 ,x_recipe_step_out               => gme_common_pvt.steps
                 ,x_routing_depd_tbl              => gme_common_pvt.step_dependencies
                 ,x_oprn_act_out                  => gme_common_pvt.activities
                 ,x_oprn_resc_rec                 => gme_common_pvt.resources
                 ,x_recp_resc_proc_param_tbl      => l_process_parameters_tab
                 ,x_formula_header_rec            => l_formula_master
                 ,x_formula_dtl_tbl               => gme_common_pvt.materials);

         IF (x_return_status = fnd_api.g_ret_sts_error) THEN
            RAISE fnd_api.g_exc_error;
         ELSIF (x_return_status = fnd_api.g_ret_sts_unexp_error) THEN
            RAISE fnd_api.g_exc_unexpected_error;
         END IF;

         -- Added code for Enforce Step Dependency
         IF gme_common_pvt.routings.COUNT > 0 THEN
            x_batch_header_rec.enforce_step_dependency :=
                 NVL (gme_common_pvt.routings (1).enforce_step_dependency, 0);
         END IF;
         --{
         IF gme_common_pvt.routings.COUNT <> 0 THEN
            /* Formula quantities calculation */
            l_total_output_qty_c := 0;

            FOR i IN 1 .. gme_common_pvt.materials.COUNT LOOP
               --{
               IF gme_common_pvt.materials (i).line_type IN (1, 2) THEN
                  --{
                  IF gme_common_pvt.materials (i).detail_uom =
                                      gme_common_pvt.routings (1).routing_uom THEN
                     l_total_output_qty_c :=
                          l_total_output_qty_c
                        + gme_common_pvt.materials (i).qty;
                  ELSE
                     l_temp_qty :=
                        inv_convert.inv_um_convert
                           (item_id            => gme_common_pvt.materials (i).inventory_item_id
                           ,PRECISION          => gme_common_pvt.g_precision
                           ,from_quantity      => gme_common_pvt.materials (i).qty
                           ,from_unit          => gme_common_pvt.materials (i).detail_uom
                           ,to_unit            => gme_common_pvt.routings (1).routing_uom
                           ,from_name          => NULL
                           ,to_name            => NULL);

                     IF (l_temp_qty = -99999) THEN
                        gme_common_pvt.log_message
                           (p_message_code      => 'INV_UOM_CONVERSION_ERROR'
                           ,p_token1_name       => 'uom1'
                           ,p_token1_value      => gme_common_pvt.materials
                                                                           (i).detail_uom
                           ,p_token2_name       => 'uom2'
                           ,p_token2_value      => gme_common_pvt.routings (1).routing_uom
                           ,p_token3_name       => 'module'
                           ,p_token3_value      => 'GME_REROUTE_BATCH_PVT.validate_validity_id'
                           ,p_product_code      => 'INV');
                        RAISE fnd_api.g_exc_error;
                     ELSE
                        l_total_output_qty_c :=
                                            l_total_output_qty_c + l_temp_qty;
                     END IF;
                  END IF;--}            /* If formula detail UOM = routing UOM */
               END IF;--}        /* l_material_details (i).line_type IN (1, 2) */
            END LOOP;

            IF l_total_output_qty_c = 0 THEN
               gme_common_pvt.log_message ('GME_API_PROD_QTY_CANT_ZERO');
               RAISE fnd_api.g_exc_error;
            END IF;

            /* Batch Quantities calculation */
            l_gme_material_details.batch_id := x_batch_header_rec.batch_id;
            --{
            IF gme_material_details_dbl.fetch_tab
                                 (p_material_detail      => l_gme_material_details
                                 ,x_material_detail      => l_material_details) THEN
               l_total_output_qty_b := 0;

               FOR l_row_count IN 1 .. l_material_details.COUNT LOOP
                  --{
                  IF l_material_details (l_row_count).line_type > 0 THEN
                     --{
                     IF l_material_details (l_row_count).dtl_um =
                                      gme_common_pvt.routings (1).routing_uom THEN
                        l_total_output_qty_b :=
                             l_total_output_qty_b
                           + l_material_details (l_row_count).plan_qty;
                     ELSE
                        l_temp_qty :=
                           inv_convert.inv_um_convert
                              (item_id            => l_material_details
                                                                  (l_row_count).inventory_item_id
                              ,PRECISION          => gme_common_pvt.g_precision
                              ,from_quantity      => l_material_details
                                                                  (l_row_count).plan_qty
                              ,from_unit          => l_material_details
                                                                  (l_row_count).dtl_um
                              ,to_unit            => gme_common_pvt.routings
                                                                           (1).routing_uom
                              ,from_name          => NULL
                              ,to_name            => NULL);

                        IF (l_temp_qty = -99999) THEN
                           gme_common_pvt.log_message
                              (p_message_code      => 'INV_UOM_CONVERSION_ERROR'
                              ,p_token1_name       => 'uom1'
                              ,p_token1_value      => l_material_details
                                                                  (l_row_count).dtl_um
                              ,p_token2_name       => 'uom2'
                              ,p_token2_value      => gme_common_pvt.routings
                                                                           (1).routing_uom
                              ,p_token3_name       => 'module'
                              ,p_token3_value      => 'GME_REROUTE_BATCH_PVT.validate_validity_id'
                              ,p_product_code      => 'INV');
                           RAISE fnd_api.g_exc_error;
                        ELSE
                           l_total_output_qty_b :=
                                            l_total_output_qty_b + l_temp_qty;
                        END IF;
                     END IF;--}
                  END IF; --} -- IF l_material_details (l_row_count).line_type > 0
               END LOOP;       -- l_row_count IN 1 .. l_material_details.Count

               IF l_total_output_qty_b = 0 THEN
                  gme_common_pvt.log_message ('GME_API_PROD_QTY_CANT_ZERO');
                  RAISE fnd_api.g_exc_error;
               END IF;
            END IF;--}

            gme_common_pvt.g_routing_scale_factor :=
                                   l_total_output_qty_b / l_total_output_qty_c;

            IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
               gme_debug.put_line (   'Routing scale factor is '
                                   || gme_common_pvt.g_routing_scale_factor);
            END IF;
         ELSE
            gme_common_pvt.g_routing_scale_factor := 1;
         END IF;--}

         /* We have all the data needed to create steps, so lets call */
         /* the create step API              */
         gme_create_step_pvt.create_batch_steps
                  (p_recipe_rout_step_tbl      => gme_common_pvt.steps
                  ,p_recipe_rout_act_tbl       => gme_common_pvt.activities
                  ,p_recipe_rout_resc_tbl      => gme_common_pvt.resources
                  ,p_resc_parameters_tbl       => l_process_parameters_tab
                  ,p_recipe_rout_matl_tbl      => gme_common_pvt.routing_materials
                  ,p_routing_depd_tbl          => gme_common_pvt.step_dependencies
                  ,p_gme_batch_header_rec      => x_batch_header_rec
                  ,p_use_workday_cal           => p_use_workday_cal
                  ,p_contiguity_override       => p_contiguity_override
                  ,x_return_status             => x_return_status);

         IF (x_return_status = 'C') THEN
            RAISE no_continous_periods;
         ELSIF (x_return_status = fnd_api.g_ret_sts_error) THEN
            RAISE fnd_api.g_exc_error;
         ELSIF (x_return_status = fnd_api.g_ret_sts_unexp_error) THEN
            RAISE fnd_api.g_exc_unexpected_error;
         END IF;

         -- Create batch steps updates the batch header
         IF (gme_common_pvt.steps.COUNT > 0) THEN
            -- Bug 19888172 - refresh batch record from table.
            IF NOT gme_batch_header_dbl.fetch_row (x_batch_header_rec, x_batch_header_rec) THEN
               RAISE fnd_api.g_exc_error;
            END IF;
            -- x_batch_header_rec.last_update_date := gme_common_pvt.g_timestamp;
         ELSE
            l_no_steps := TRUE;
         END IF;

         OPEN cur_get_max_step_date;

         FETCH cur_get_max_step_date
          INTO l_max_step_date;

         CLOSE cur_get_max_step_date;

         IF l_max_step_date <> x_batch_header_rec.plan_cmplt_date THEN
            x_batch_header_rec.plan_cmplt_date := l_max_step_date;
         END IF;
      ELSE
         x_batch_header_rec.poc_ind := 'N';
         l_no_steps := TRUE;
      END IF;--}
      
      --{               /* IF x_batch_header_rec.routing_id IS NOT NULL */
      IF (l_no_steps) THEN 
        SELECT primary_uom_code
        INTO   l_prim_item_um
        FROM   mtl_system_items_b
        WHERE  inventory_item_id = l_inventory_item_id
        AND    organization_id    = x_batch_header_rec.organization_id;

        OPEN get_prim_prod (x_batch_header_rec.batch_id
                           ,l_inventory_item_id);

        FETCH get_prim_prod INTO l_prim_prod_qty, l_prim_prod_um;

        IF get_prim_prod%FOUND THEN
         l_prim_prod_found := TRUE;
        ELSE
         l_prim_prod_found := FALSE;
        END IF;
        
        CLOSE get_prim_prod;
        --{
        IF l_prim_prod_found THEN
        
          l_temp_qty :=
                           inv_convert.inv_um_convert
                              (item_id            => l_inventory_item_id
                              ,PRECISION          => gme_common_pvt.g_precision
                              ,from_quantity      => l_prim_prod_qty
                              ,from_unit          => l_prim_prod_um
                              ,to_unit            => l_prim_item_um
                              ,from_name          => NULL
                              ,to_name            => NULL);

                        IF (l_temp_qty = -99999) THEN
                           gme_common_pvt.log_message
                              (p_message_code      => 'INV_UOM_CONVERSION_ERROR'
                              ,p_token1_name       => 'uom1'
                              ,p_token1_value      => l_prim_prod_um
                              ,p_token2_name       => 'uom2'
                              ,p_token2_value      => l_prim_item_um
                              ,p_token3_name       => 'module'
                              ,p_token3_value      => 'GME_REROUTE_BATCH_PVT.reroute_batch'
                              ,p_product_code      => 'INV');
                           RAISE fnd_api.g_exc_error;
                        END IF;
        
          IF (gme_common_pvt.calc_date_from_prod_rule
                   (p_organization_id        => x_batch_header_rec.organization_id
                   ,p_inventory_item_id      => l_inventory_item_id
                   ,p_item_qty               => l_temp_qty
                   ,p_start_date             => x_batch_header_rec.plan_start_date
                   ,p_cmplt_date             => x_batch_header_rec.plan_cmplt_date
                   ,x_start_date             => l_plan_start_date
                   ,x_cmplt_date             => l_plan_cmplt_date) ) THEN
               l_no_prod_rule_found := FALSE;
               x_batch_header_rec.plan_start_date := l_plan_start_date;
               x_batch_header_rec.plan_cmplt_date := l_plan_cmplt_date;
            ELSE
               l_no_prod_rule_found := TRUE;
          END IF;
         ELSE
            -- prim prod was not found...
            l_no_prod_rule_found := TRUE;          

         END IF;--}
         --{
         IF l_no_prod_rule_found THEN
            IF     x_batch_header_rec.plan_start_date IS NOT NULL THEN
               --FPBug#4585491 if no production rule is found  plan_completion_date will get default to start date
               --AND x_batch_header_rec.plan_cmplt_date IS NULL THEN
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
         END IF;--}  
      END IF;--}               
      

      OPEN cur_get_material (x_batch_header_rec.batch_id);

      FETCH cur_get_material
      BULK COLLECT INTO l_material_detail_ids_tab, l_phantom_ids_tab;

      CLOSE cur_get_material;

      FOR i IN 1 .. l_material_detail_ids_tab.COUNT LOOP
         l_material_detail_rec.material_detail_id :=
                                                l_material_detail_ids_tab (i);
         l_phantom_associated_to_step := FALSE;
         gme_common_pvt.material_date_change
            (p_material_detail_id      => l_material_detail_rec.material_detail_id
            ,p_material_date           => NULL
            ,x_return_status           => x_return_status);
         IF (g_debug = gme_debug.g_log_statement) THEN
            gme_debug.put_line
                            (   'after call to gme_common_pvt.material_date_change
                                 material_detail_id is='
                                 ||to_char(l_material_detail_ids_tab (i))
                                 ||' return status = '||x_return_status);                                    
         END IF;                                    

         IF (x_return_status = fnd_api.g_ret_sts_error) THEN
           RAISE fnd_api.g_exc_error;
         ELSIF (x_return_status = fnd_api.g_ret_sts_unexp_error) THEN
           RAISE fnd_api.g_exc_unexpected_error;
         END IF;
/*Sunitha Ch Bug 5353941 seperated the check whether the material is assciated with step or not and 
if it is not assciated then added a select statement to retrieve the planed start date of the parent batch into l_phantom_batch_header_rec.plan_cmplt_date*/

         IF ( l_phantom_ids_tab (i) IS NOT NULL ) THEN
  /*Sunitha ch Bug#5391396 included the code to  rescedule_step of the child batch when the product 
    is associated to step and its release type is autobystep.*/
	    IF NOT gme_material_details_dbl.fetch_row(l_material_detail_rec, l_material_detail_rec) THEN
              l_proc := 'gme_material_details_dbl.fetch_row';
              RAISE error_dbl;
            END IF;
	--    IF(l_material_detail_rec.material_requirement_date <> l_db_mtl_dtl_rec.material_requirement_date )  THEN
              l_phantom_batch_header_rec.batch_id:= l_phantom_ids_tab (i);
	      IF(gme_common_pvt.is_material_auto_release(l_material_detail_rec.phantom_line_id)=3 AND 
	            gme_common_pvt.is_material_assoc_to_step
                                                (l_material_detail_rec.phantom_line_id ))  THEN
	         l_phantom_associated_to_step := TRUE;
                 l_phantom_batch_step_rec.batch_id:= l_phantom_ids_tab (i);
                 SELECT batchstep_id INTO l_phantom_batch_step_rec.batchstep_id
                              FROM gme_batch_step_items
                             WHERE batch_id = l_phantom_ids_tab (i)
                               AND material_detail_id =  l_material_detail_rec.phantom_line_id;
                 IF NOT gme_batch_steps_dbl.fetch_row(l_phantom_batch_step_rec, l_phantom_batch_step_rec) THEN
                    l_proc := 'gme_batch_steps_dbl.fetch_row';
                    RAISE error_dbl;
                 END IF;
                 l_phantom_batch_step_rec.plan_cmplt_date:=l_material_detail_rec.material_requirement_date;
		 l_phantom_batch_step_rec.plan_start_date:=NULL;
                 gme_reschedule_step_pvt.reschedule_step
                               (p_batch_step_rec             => l_phantom_batch_step_rec
                               ,p_source_step_id_tbl         => l_step_tbl
                               ,p_contiguity_override        => fnd_api.g_true
                               ,p_reschedule_preceding       => fnd_api.g_true
                               ,p_reschedule_succeeding      => fnd_api.g_true
                               ,p_use_workday_cal            => fnd_api.g_false
                               ,x_batch_step_rec             => x_batch_step_rec
                               ,x_return_status              => x_return_status);
              ELSE
                gme_debug.put_line('sc l_phantom_batch_header_rec.batch_id is '||l_phantom_batch_header_rec.batch_id);
                l_phantom_batch_header_rec.plan_cmplt_date:=l_material_detail_rec.material_requirement_date;
                gme_debug.put_line('sc l_phantom_batch_header_rec.plan_cmplt_date is '||to_char(l_phantom_batch_step_rec.plan_cmplt_date, 'DD-MON-YY HH24:MI:SS'));
                gme_reschedule_batch_pvt.reschedule_batch
                     (p_batch_header_rec         => l_phantom_batch_header_rec
                     ,p_use_workday_cal          => fnd_api.g_false
                     ,p_contiguity_override      => fnd_api.g_true
                     ,x_batch_header_rec         => l_phantom_batch_header_rec_out
                     ,x_return_status            => x_return_status);
                 gme_debug.put_line('sc l_phantom_batch_header_rec_out.plan_cmplt_date is '||to_char(l_phantom_batch_header_rec_out.plan_cmplt_date, 'DD-MON-YY HH24:MI:SS'));
                 gme_debug.put_line('sc l_phantom_batch_header_rec_out.batch_id is '||l_phantom_batch_header_rec_out.batch_id);
              END IF;
          END IF;
         
         --FPBug#4585491 Begin 
         --commented the following code
         /*--this is neccessary so that correct status of W gets send to the form.
         IF (x_return_status = 'W' AND (NOT l_W_ReturnedAtLeastOnce)) THEN
           l_W_ReturnedAtLeastOnce := TRUE;
         END IF;*/

         /*
          The above material_date_change returns different status as described below
          R: When reservations are deleted for a material line
          M: When MO Allocations are deleted for a material line
          B: When Both reservations and material lines are deleted for a material line
         */
         IF x_return_status = 'R' THEN
           l_R_count := l_R_count + 1;
         ELSIF x_return_status = 'M' THEN
           l_M_count := l_M_count + 1;
         ELSIF x_return_status = 'B' THEN
           l_B_count := l_B_count + 1;
         END IF;
         --FPBug#4585491 End
      END LOOP;              /* FOR i IN 1..l_material_detail_ids_tab.COUNT */
      
      --FPBug#4585491 Begin 
      --commented the following code
      /*IF (l_W_ReturnedAtLeastOnce) AND (x_return_status = fnd_api.g_ret_sts_success) THEN
        x_return_status := 'W';
      END IF; */

      /* If any of the reservations are MO allocations deleted then respective message
         will be put on the stack but return status will be 'S' to form or API */
      IF (l_B_count > 0) OR (l_R_count > 0 AND l_M_count > 0) THEN 
       --atleast for one material line MO allocations and reservations are deleted
       gme_common_pvt.log_message('GME_EXPIRED_RESERV_MO_DELETED');
       --x_return_status := 'W';       
      ELSIF l_R_count > 0 THEN
       ----atleast for one material line reservations are deleted
       gme_common_pvt.log_message('GME_EXPIRED_RESERV_DELETED');
       --x_return_status := 'W';
      ELSIF l_M_count > 0 THEN
       ----atleast for one material line MO allocations are deleted
       gme_common_pvt.log_message('GME_EXPIRED_MO_DELETED');
       --x_return_status := 'W';
      END IF;
      x_return_status := fnd_api.g_ret_sts_success;
      --FPBug#4585491 End
      --Susruth Bug#5359091 Finite Scheduled indicator is set back to 0 once the batch is rerouted. start.
      IF x_batch_header_rec.FINITE_SCHEDULED_IND = 1 THEN
         x_batch_header_rec.FINITE_SCHEDULED_IND := 0;
      END IF;
      
      
      -- Bug#5359091 end.
      IF NOT gme_batch_header_dbl.update_row (x_batch_header_rec) THEN
         RAISE fnd_api.g_exc_error;
      END IF;

      -- biachen. for new batchsteps, create instance from pi instruction
      IF x_batch_header_rec.enhanced_pi_ind = 'Y' THEN
        gmo_vbatch_grp.instantiate_advanced_pi
                               ( P_API_VERSION       => 1.0,
                                 P_INIT_MSG_LIST     => FND_API.G_FALSE,
                                 P_COMMIT            => FND_API.G_FALSE,
                                 P_VALIDATION_LEVEL  => FND_API.G_VALID_LEVEL_FULL,
                                 X_RETURN_STATUS     => x_return_status,
                                 X_MSG_COUNT         => l_msg_count,
                                 X_MSG_DATA          => l_msg_data,
                                 P_ENTITY_NAME       => 'BATCH',
                                 P_ENTITY_KEY        => x_batch_header_rec.batch_id) ;
         IF x_return_status <> fnd_api.g_ret_sts_success THEN
             RAISE fnd_api.g_exc_error;
         END IF;
      END IF ;
      
      /*                                    
      BEGIN ER 19161894  Shaliu Chen 18-JUL-2014               
      Added for OPM Step Level OSP Project  
      If batch is OSP batch,invoke reroute batch program to cancel
      or recreate PO/Requisition
      */           
     IF g_debug <= gme_debug.g_log_statement THEN
             gme_debug.put_line (   g_pkg_name
                                 || '.'
                                 || l_api_name
                                 || ' Calling osp reroute batch API');
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
  
          gme_osp.reroute_batch_process(p_batch_id         => x_batch_header_rec.batch_id,
                                        p_organization_id  => x_batch_header_rec.organization_id,
                                        x_return_status    => x_return_status,
                                        x_message_list     => l_msg_data,
                                        x_message_count    => l_msg_count);             
        
                                            
          IF x_return_status <> fnd_api.g_ret_sts_success THEN 
            
            IF x_return_status = 'W' THEN
              IF (NVL (g_debug, 0) IN (gme_debug.g_log_statement, gme_debug.g_log_procedure) ) THEN
                gme_debug.put_line('The GME parameter:Propagate batch changes to Purchasing is Manual');  
              END IF;
              gme_common_pvt.log_message('GME_PROPAGATE_CHANGE_MANUAL'); 
              x_return_status := fnd_api.g_ret_sts_success;  
            ELSE
              RAISE cancel_recreate_po_failed;              
            END IF;                
            
          END IF;
        END IF;
      EXCEPTION
        WHEN OTHERS THEN
          l_msg_data := 'unexpected error: ' || SQLERRM || 'SQLCODE = ' || SQLCODE;
          gme_debug.put_line('l_errMsg:'||l_msg_data);
          fnd_message.set_name('FND', 'FND_GENERIC_MESSAGE');
          fnd_message.set_token('MESSAGE', l_msg_data);
          fnd_msg_pub.add;
          RAISE cancel_recreate_po_failed;          
      END;         		  
	    /*END ER 19161894*/
      
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
      WHEN no_continous_periods THEN
         RETURN;
      WHEN fnd_api.g_exc_error THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN fnd_api.g_exc_unexpected_error THEN
         x_return_status := fnd_api.g_ret_sts_unexp_error;

         IF (NVL (g_debug, 0) > 0) THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ':'
                                || 'UNEXPECTED:'
                                || SQLERRM);
         END IF;
      WHEN error_dbl THEN
         gme_common_pvt.log_message ('GME_UNEXPECTED_ERROR', 'ERROR', SQLERRM);
         x_return_status := FND_API.g_ret_sts_unexp_error;

         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
           gme_debug.put_line (g_pkg_name||'.'||l_api_name|| ': '
                                                          || l_proc|| ' unexpected error: '|| SQLERRM);
         END IF;
      /*ER 19161894  Shaliu Chen 18-JUL-2014*/   
      WHEN cancel_recreate_po_failed THEN
        x_return_status := fnd_api.g_ret_sts_success; 
        
        IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
           gme_debug.put_line('Cancel existing PO/Req and recreate new requisition for OSP batch '
                              ||x_batch_header_rec.batch_no||' failed.');
  
        END IF; 
        
        gme_common_pvt.log_message('GME_CANCEL_PO_FAILED','BATCH_NO',x_batch_header_rec.batch_no);         
      WHEN OTHERS THEN
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
         x_return_status := fnd_api.g_ret_sts_unexp_error;

         IF (NVL (g_debug, 0) > 0) THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ':'
                                || 'OTHERS:'
                                || SQLERRM);
         END IF;
   END reroute_batch;

   /*===========================================================================================
   Procedure
     delete_all_steps
   Description
     This particular procedure is used to delete all the steps in the batch.
   Parameters
     p_batch_id     Batch ID
     x_return_status    outcome of the API call
               S - Success
               E - Error
               U - Unexpected error
   =============================================================================================*/
   PROCEDURE delete_all_steps (
      p_batch_id        IN              NUMBER
     ,x_return_status   OUT NOCOPY      VARCHAR2)
   IS
      l_api_name   CONSTANT VARCHAR2 (30) := 'DELETE_ALL_STEPS';
   BEGIN
      IF (NVL (g_debug, 0) IN
                       (gme_debug.g_log_statement, gme_debug.g_log_procedure) ) THEN
         gme_debug.put_line (g_pkg_name || '.' || l_api_name || ':'
                             || 'Entering');
      END IF;

      /* Set the return status to success initially */
      x_return_status := fnd_api.g_ret_sts_success;

      DELETE FROM gme_process_parameters
            WHERE batch_id = p_batch_id;

      DELETE FROM gme_resource_txns
            WHERE doc_id = p_batch_id;

      DELETE FROM gme_resource_txns_gtmp
            WHERE doc_id = p_batch_id;

      DELETE FROM gme_batch_step_resources
            WHERE batch_id = p_batch_id;

      DELETE FROM gme_batch_step_items
            WHERE batch_id = p_batch_id;

      DELETE FROM gme_batch_step_dependencies
            WHERE batch_id = p_batch_id;

      DELETE FROM gme_batch_step_charges
            WHERE batch_id = p_batch_id;

      DELETE FROM gme_batch_step_activities
            WHERE batch_id = p_batch_id;

      DELETE FROM gme_batch_steps
            WHERE batch_id = p_batch_id;

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
      WHEN fnd_api.g_exc_error THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN fnd_api.g_exc_unexpected_error THEN
         x_return_status := fnd_api.g_ret_sts_unexp_error;

         IF (NVL (g_debug, 0) > 0) THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ':'
                                || 'UNEXPECTED:'
                                || SQLERRM);
         END IF;
      WHEN OTHERS THEN
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
         x_return_status := fnd_api.g_ret_sts_unexp_error;

         IF (NVL (g_debug, 0) > 0) THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ':'
                                || 'OTHERS:'
                                || SQLERRM);
         END IF;
   END delete_all_steps;

   PROCEDURE validate_validity_id_from_pub (
      p_batch_header_rec   IN              gme_batch_header%ROWTYPE
     ,p_validity_rule_id   IN              NUMBER
     ,x_return_status      OUT NOCOPY      VARCHAR2)
   IS
      l_api_name   CONSTANT VARCHAR2 (30)  := 'validate_validity_id_from_pub';

      CURSOR cur_get_validity_rule_details (v_validity_rule_id IN NUMBER)
      IS
         SELECT gr.formula_id, grvr.inventory_item_id
           FROM gmd_recipe_validity_rules grvr, gmd_recipes gr
          WHERE grvr.recipe_validity_rule_id = v_validity_rule_id
            AND gr.recipe_id = grvr.recipe_id;

      CURSOR cur_get_material_details (
         v_batch_id            IN   NUMBER
        ,v_inventory_item_id   IN   NUMBER)
      IS
         SELECT plan_qty, dtl_um
           FROM gme_material_details
          WHERE batch_id = v_batch_id
            AND line_type = 1
            AND inventory_item_id = v_inventory_item_id;

      l_formula_id          NUMBER;
      l_inventory_item_id   NUMBER;
      l_plan_qty            NUMBER;
      l_dtl_um              gme_material_details.dtl_um%TYPE;
   BEGIN
      IF (NVL (g_debug, 0) IN
                       (gme_debug.g_log_statement, gme_debug.g_log_procedure) ) THEN
         gme_debug.put_line (g_pkg_name || '.' || l_api_name || ':'
                             || 'Entering');
      END IF;

      /* Set the return status to success initially */
      x_return_status := fnd_api.g_ret_sts_success;

      /* Don't allow the Batch to be Rerouted if the Batch Status is not pending */
      IF (p_batch_header_rec.batch_status <> 1) THEN
         gme_common_pvt.log_message ('GME_API_INVALID_BATCH_REROUTE');
         RAISE fnd_api.g_exc_error;
      END IF;

      /* LCF Batches may not have a validity rule
         We do not want to allow reroute for these batches */
      IF (p_batch_header_rec.recipe_validity_rule_id IS NULL) THEN
         --Bug#5439736 replaced the message
         gme_common_pvt.log_message ('GME_REROUTE_NOT_FOR_LCF');         
         RAISE fnd_api.g_exc_error;
      END IF;

      /* New validity rule should be different from the existing */
      IF p_batch_header_rec.recipe_validity_rule_id = p_validity_rule_id THEN
         gme_common_pvt.log_message ('GME_API_SAME_VALIDITY_RULE');
         RAISE fnd_api.g_exc_error;
      END IF;

      OPEN cur_get_validity_rule_details (p_validity_rule_id);

      FETCH cur_get_validity_rule_details
       INTO l_formula_id, l_inventory_item_id;

      IF (cur_get_validity_rule_details%NOTFOUND) THEN
         CLOSE cur_get_validity_rule_details;

         gme_common_pvt.log_message ('GME_API_INVALID_VALIDITY');
         RAISE fnd_api.g_exc_error;
      END IF;

      CLOSE cur_get_validity_rule_details;

      /* ????? */
      OPEN cur_get_material_details (p_batch_header_rec.batch_id
                                    ,l_inventory_item_id);

      FETCH cur_get_material_details
       INTO l_plan_qty, l_dtl_um;

      IF (cur_get_material_details%NOTFOUND) THEN
         CLOSE cur_get_material_details;
         --Bug#5439736 replaced the message
         gme_common_pvt.log_message ('GME_REROUTE_NO_PRIM_PROD');
         RAISE fnd_api.g_exc_error;
      END IF;

      CLOSE cur_get_material_details;

      /* whether the new validity rule belongs to the same formula as the batch */
      IF (p_batch_header_rec.formula_id <> l_formula_id) THEN
         gme_common_pvt.log_message ('GME_API_VALIDITY_DIFF_FORM');
         RAISE fnd_api.g_exc_error;
      END IF;

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
      WHEN fnd_api.g_exc_error THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN fnd_api.g_exc_unexpected_error THEN
         x_return_status := fnd_api.g_ret_sts_unexp_error;

         IF (NVL (g_debug, 0) > 0) THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ':'
                                || 'UNEXPECTED:'
                                || SQLERRM);
         END IF;
      WHEN OTHERS THEN
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
         x_return_status := fnd_api.g_ret_sts_unexp_error;

         IF (NVL (g_debug, 0) > 0) THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ':'
                                || 'OTHERS:'
                                || SQLERRM);
         END IF;
   END validate_validity_id_from_pub;

   /*===========================================================================================
   Procedure
     validate_validity_id
   Description
     This particular procedure is used to validate the validity rule id passed in.
   Parameters
     p_validity_rule_id The validity_rule

   =============================================================================================*/
   PROCEDURE validate_validity_id (
      p_batch_header_rec   IN              gme_batch_header%ROWTYPE
     ,p_validity_rule_id   IN              NUMBER
     ,x_return_status      OUT NOCOPY      VARCHAR2)
   IS
      l_api_name   CONSTANT VARCHAR2 (30) := 'validate_validity_id';

      CURSOR cur_validate_validity (
         v_batch_type         IN   NUMBER
        ,v_validity_rule_id   IN   NUMBER
        ,v_qty                IN   NUMBER
        ,v_laboratory_ind     IN   NUMBER)
      IS
         SELECT COUNT (1)
           FROM gmd_recipe_validity_rules v
          WHERE recipe_validity_rule_id = v_validity_rule_id
            AND (    (validity_rule_status BETWEEN 700 AND 799)
                 OR (validity_rule_status BETWEEN 900 AND 999)
                 OR (    v_laboratory_ind = 1
                     AND validity_rule_status BETWEEN 400 AND 699) )
            AND (    (v_batch_type = 0 AND recipe_use = 0)
                 OR (v_batch_type = 10 AND recipe_use IN (0, 1) ) )
            AND delete_mark = 0
            AND inv_min_qty <= v_qty
            AND inv_max_qty >= v_qty;

      CURSOR cur_prod_details (
         v_batch_id           IN   NUMBER
        ,v_validity_rule_id   IN   NUMBER)
      IS
         SELECT d.inventory_item_id, d.organization_id, d.plan_qty
               ,d.dtl_um line_um, msib.primary_uom_code item_um
           FROM gme_material_details d
               ,mtl_system_items_b msib
               ,gmd_recipe_validity_rules v
          WHERE d.batch_id = v_batch_id
            AND d.line_type = 1
            AND msib.inventory_item_id = d.inventory_item_id
            AND msib.organization_id = d.organization_id
            AND v.recipe_validity_rule_id = v_validity_rule_id
            AND d.inventory_item_id = v.inventory_item_id;

      l_prod_qty            NUMBER;
      l_conv_qty            NUMBER;
      l_count               PLS_INTEGER;
   BEGIN
      IF (NVL (g_debug, 0) IN
                       (gme_debug.g_log_statement, gme_debug.g_log_procedure) ) THEN
         gme_debug.put_line (g_pkg_name || '.' || l_api_name || ':'
                             || 'Entering');
      END IF;

      /* Set the return status to success initially */
      x_return_status := fnd_api.g_ret_sts_success;

      /* determine prod qty and passing */
      /* item_id, qty and uom to pass to cursor validate_validity */
      FOR get_rec IN cur_prod_details (p_batch_header_rec.batch_id
                                      ,p_validity_rule_id) LOOP
         IF (get_rec.line_um = get_rec.item_um) THEN
            l_prod_qty := NVL (l_prod_qty, 0) + get_rec.plan_qty;
         ELSE
            l_conv_qty :=
               inv_convert.inv_um_convert
                                    (item_id            => get_rec.inventory_item_id
                                    ,PRECISION          => gme_common_pvt.g_precision
                                    ,from_quantity      => get_rec.plan_qty
                                    ,from_unit          => get_rec.line_um
                                    ,to_unit            => get_rec.item_um
                                    ,from_name          => NULL
                                    ,to_name            => NULL);

            IF (l_conv_qty = -99999) THEN
               gme_common_pvt.log_message
                  (p_message_code      => 'INV_UOM_CONVERSION_ERROR'
                  ,p_token1_name       => 'uom1'
                  ,p_token1_value      => get_rec.line_um
                  ,p_token2_name       => 'uom2'
                  ,p_token2_value      => get_rec.item_um
                  ,p_token3_name       => 'module'
                  ,p_token3_value      => 'GME_REROUTE_BATCH_PVT.validate_validity_id'
                  ,p_product_code      => 'INV');
               RAISE fnd_api.g_exc_error;
            ELSE
               l_prod_qty := NVL (l_prod_qty, 0) + l_conv_qty;
            END IF;
         END IF;
      END LOOP;

      OPEN cur_validate_validity (p_batch_header_rec.batch_type
                                 ,p_validity_rule_id
                                 ,l_prod_qty
                                 ,p_batch_header_rec.laboratory_ind);

      FETCH cur_validate_validity
       INTO l_count;

      CLOSE cur_validate_validity;

      IF (l_count = 0) THEN
         gme_common_pvt.log_message ('GME_API_INVALID_VALIDITY');
         RAISE fnd_api.g_exc_error;
      END IF;

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
      WHEN fnd_api.g_exc_error THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN fnd_api.g_exc_unexpected_error THEN
         x_return_status := fnd_api.g_ret_sts_unexp_error;

         IF (NVL (g_debug, 0) > 0) THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ':'
                                || 'UNEXPECTED:'
                                || SQLERRM);
         END IF;
      WHEN OTHERS THEN
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
         x_return_status := fnd_api.g_ret_sts_unexp_error;

         IF (NVL (g_debug, 0) > 0) THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ':'
                                || 'OTHERS:'
                                || SQLERRM);
         END IF;
   END validate_validity_id;
END gme_reroute_batch_pvt;
/

--COMMIT ;
EXIT;
