/* +======================================================================+ */
/* |    Copyright (c) 2005, 2015 Oracle and/or its affiliates.           | */
/* |                         All rights reserved.                         | */
/* |                           Version 12.0.0                             | */
/* +======================================================================+ */
REM dbdrv: sql ~PROD ~PATH ~FILE none none none package &phase=plb \
REM dbdrv: checkfile(120.2.12010000.5=120.2.12020000.5):~PROD:~PATH:~FILE
SET VERIFY OFF
WHENEVER SQLERROR EXIT FAILURE ROLLBACK;
WHENEVER OSERROR EXIT FAILURE ROLLBACK;

CREATE OR REPLACE PACKAGE BODY gme_fpl_pvt AS
/* $Header: GMEVFPLB.pls 120.2.12020000.5 2015/04/09 10:26:03 shalchen ship $ */
   g_debug               VARCHAR2 (5)  := fnd_profile.VALUE ('AFLOG_LEVEL');
   g_pkg_name   CONSTANT VARCHAR2 (30) := 'GME_FPL_PVT';

/*******************************************************/
/* Oracle Process Manufacturing Process Execution APIs */
/*                                                     */
/* File Name: GMEVFPLB.pls                             */
/* Contents:  GME Fixed Process Loss procedures.       */
/* HISTORY                                             
   Archana Mundhe 03-Apr-2008 Bug 6908375 
      Modified procedure apply_fixed_process_loss to 
      Raise error if new_input_qty is <=0 
  Shaliu Chen     18-JUL-2014  ER 19161894                                                 
     Modify create_batch to invoke requisition creation 
     program if batch include OSP step    
  Shaliu Chen     09-APR-2015  ER 20809749
    Modify create batch to support multiple osp steps.         
***************************************************** */
   
PROCEDURE get_fixed_process_loss (
      p_batch_id                     IN   NUMBER DEFAULT NULL
     ,p_validity_rule_id             IN   NUMBER 
     ,p_organization_id              IN   NUMBER DEFAULT NULL
     ,x_fixed_process_loss           OUT  NOCOPY NUMBER
     ,x_fixed_process_loss_uom       OUT  NOCOPY sy_uoms_mst.uom_code%TYPE
     )
   IS
              
      l_api_name                     VARCHAR2 (30)      := 'GET_FIXED_PROCESS_LOSS';
      l_recipe_id                    NUMBER;
      l_formula_id                   NUMBER;
      l_routing_id                   NUMBER;
      l_validity_rule_id             NUMBER;
      l_organization_id              NUMBER;
      l_routing_class                gmd_routings_b.routing_class%TYPE;
      expected_error                 EXCEPTION;
 
      CURSOR cur_get_info (v_batch_id NUMBER)
      IS
         SELECT b.recipe_validity_rule_id, a.recipe_id, b.organization_id
               ,b.routing_id, b.formula_id
           FROM gme_batch_header b, gmd_recipe_validity_rules a
          WHERE b.batch_id = v_batch_id
            AND b.recipe_validity_rule_id = a.recipe_validity_rule_id;

      CURSOR cur_get_info_from_validity (v_validity_rule_id NUMBER)
      IS
         SELECT r.recipe_id, r.routing_id, r.formula_id
           FROM gmd_recipes_b r, gmd_recipe_validity_rules v
          WHERE v.recipe_validity_rule_id = v_validity_rule_id
            AND v.recipe_id = r.recipe_id;

     CURSOR validity_process_loss_cursor (v_recipe_validity_rule_id NUMBER)
      IS
         SELECT fixed_process_loss, fixed_process_loss_uom	
           FROM gmd_recipe_validity_rules
          WHERE recipe_validity_rule_id = NVL (v_recipe_validity_rule_id, -1);
          
      CURSOR orgn_process_loss_cursor (v_recipe_id NUMBER, v_org_id NUMBER)
      IS
         SELECT fixed_process_loss, fixed_process_loss_uom
           FROM gmd_recipe_process_loss
          WHERE recipe_id = v_recipe_id AND organization_id = v_org_id;

      CURSOR recipe_process_loss_cursor (v_recipe_id NUMBER)
      IS
         SELECT fixed_process_loss, fixed_process_loss_uom
           FROM gmd_recipes
          WHERE recipe_id = v_recipe_id;
      
      CURSOR routing_process_loss_cursor (v_routing_id NUMBER)
      IS
	         SELECT routing_class, fixed_process_loss, fixed_process_loss_uom
	           FROM gmd_routings_b
	          WHERE routing_id = v_routing_id;

     CURSOR routing_cls_process_loss (v_routing_id NUMBER)
      IS
         SELECT b.fixed_process_loss, b.routing_class_uom
           FROM gmd_routings_b a, gmd_routing_class_b b
          WHERE a.routing_id = v_routing_id
            AND a.routing_class = b.routing_class (+);
      
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering1 api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      IF NVL (g_debug, -1) = gme_debug.g_log_statement THEN
         gme_debug.put_line (l_api_name || ': Parameters');
         gme_debug.put_line ('p_batch_id:' || p_batch_id);
         gme_debug.put_line ('p_validity_rule_id:' || p_validity_rule_id);
         gme_debug.put_line ('p_organization_id:' || p_organization_id);
      END IF;
      l_organization_id := p_organization_id; 
      -- Use and validate the batch id if it is passed in. 
      IF (p_batch_id IS NOT NULL) THEN
         OPEN cur_get_info (p_batch_id);

         FETCH cur_get_info
         INTO l_validity_rule_id, l_recipe_id, l_organization_id, l_routing_id ,l_formula_id;
         CLOSE cur_get_info;
         IF (l_recipe_id IS NULL AND l_validity_rule_id IS NOT NULL) THEN
            gme_common_pvt.log_message ('GME_INVALID_BATCH');
            RAISE expected_error;
         END IF;
      ELSIF p_validity_rule_id IS NOT NULL THEN
         -- Validate recipe validity rule
         l_validity_rule_id := p_validity_rule_id;

         OPEN cur_get_info_from_validity (p_validity_rule_id);
         FETCH cur_get_info_from_validity INTO l_recipe_id, l_routing_id, l_formula_id;
         CLOSE cur_get_info_from_validity;

         IF (l_recipe_id IS NULL) THEN
            gme_common_pvt.log_message ('GME_API_INVALID_RULE');
            RAISE expected_error;
         END IF;
      END IF;
      
      -- Do not process LCF batch
      IF (l_validity_rule_id  IS NULL) THEN
          RETURN;
      END IF;
      
      -- Determine the Fixed process loss at VR level.
      OPEN validity_process_loss_cursor (l_validity_rule_id);
      FETCH validity_process_loss_cursor INTO x_fixed_process_loss, x_fixed_process_loss_uom;
      CLOSE validity_process_loss_cursor;

      IF x_fixed_process_loss IS NOT NULL THEN
         RETURN;
      ELSIF NVL (g_debug, -1) = gme_debug.g_log_statement THEN
            gme_debug.put_line (   l_api_name || ': No process_loss at validity level');
      END IF;
         
      -- Determine FPL at recipe Org level 
      OPEN orgn_process_loss_cursor (l_recipe_id, l_organization_id);
      FETCH orgn_process_loss_cursor  INTO x_fixed_process_loss, x_fixed_process_loss_uom;
      CLOSE orgn_process_loss_cursor;

      IF x_fixed_process_loss IS NOT NULL THEN
         return;
      ELSIF NVL (g_debug, -1) = gme_debug.g_log_statement THEN
            gme_debug.put_line (   l_api_name  || ': No fixed process_loss at orgn level');
      END IF;      
            
      -- Determin FPL at Recipe level.
      OPEN recipe_process_loss_cursor (l_recipe_id);
      FETCH recipe_process_loss_cursor INTO x_fixed_process_loss, x_fixed_process_loss_uom;
      CLOSE recipe_process_loss_cursor;
      IF x_fixed_process_loss IS NOT NULL THEN
         return;
      ELSIF NVL (g_debug, -1) = gme_debug.g_log_statement THEN
            gme_debug.put_line (   l_api_name  || 'No fixed process_loss at recipe level');
      END IF;         
         
      -- Determine FPL at routing level.
      IF (l_routing_id IS NOT NULL ) THEN
          OPEN routing_process_loss_cursor (l_routing_id);
          FETCH routing_process_loss_cursor  INTO l_routing_class, x_fixed_process_loss,x_fixed_process_loss_uom ;
          CLOSE routing_process_loss_cursor;
          IF x_fixed_process_loss IS NOT NULL THEN
             return;
          ELSIF NVL (g_debug, -1) = gme_debug.g_log_statement THEN
                gme_debug.put_line (   l_api_name  || 'No fixed process_loss at routing level');
          END IF;    
       END IF;
           
      -- Determine FPL at routing class level.               
      IF l_routing_class IS NOT NULL THEN   
         OPEN routing_cls_process_loss (l_routing_id);
         FETCH routing_cls_process_loss INTO x_fixed_process_loss,x_fixed_process_loss_uom;         
         CLOSE routing_cls_process_loss;
          IF x_fixed_process_loss IS NOT NULL THEN
             return;
          ELSIF NVL (g_debug, -1) = gme_debug.g_log_statement THEN
              gme_debug.put_line (l_api_name || ': No FPL at routing class ');
          END IF;
      END IF;         

      IF NVL (g_debug, -1) = gme_debug.g_log_statement THEN
         gme_debug.put_line (l_api_name  || 'No Fixed Process Loss defined ');
      END IF;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;

   EXCEPTION
      WHEN expected_error THEN       
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line('expected ERROR in gme_fpl_pvt.get_fixed_process_loss');
      END IF;
         RETURN;
      WHEN OTHERS THEN
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);

         IF g_debug <= gme_debug.g_log_unexpected THEN
            gme_debug.put_line (   'When others exception in '
                                || g_pkg_name
                                || '.'
                                || l_api_name
                                || ' Error is '
                                || SQLERRM);
         END IF;
         
   END get_fixed_process_loss;
   
   /******* Procedure to apply fixed process loss   **********/
   
   PROCEDURE apply_fixed_process_loss (       
      p_batch_header_rec         IN              gme_batch_header%ROWTYPE
     ,p_material_tbl             IN              gme_common_pvt.material_details_tab
     ,p_organization_id          IN              NUMBER DEFAULT NULL
     ,p_creation_mode            IN              VARCHAR2
     ,p_called_from              IN              NUMBER DEFAULT 1 /*1 = Create Batch, 2 = Batch details */
     ,x_batch_header_rec         OUT NOCOPY      gme_batch_header%ROWTYPE
     ,x_material_tbl             OUT NOCOPY      gme_common_pvt.material_details_tab
     ,x_return_status            OUT NOCOPY      VARCHAR2
     )
   IS
   l_api_name                     VARCHAR2(30)      := 'APPLY_FIXED_PROCESS_LOSS';
   l_fixed_process_loss           NUMBER;   
   l_fixed_process_loss_uom       sy_uoms_mst.uom_code%TYPE;
   l_item_uom                     sy_uoms_mst.uom_code%TYPE;
   l_new_fpl                      NUMBER;
   l_convert_dtl_uom              NUMBER := 0;
   l_convert_fpl_uom              NUMBER := 0;
   l_new_dtl_qty                  NUMBER;
   TOTAL_OUTPUT_QTY               NUMBER;
   total_input_qty                NUMBER;
   new_input_qty                  NUMBER;
   ACTUAL_OUTPUT_TOTAL            NUMBER;
   l_original_primary_qty         NUMBER;
   l_plan_qty                     NUMBER;
   l_item_no                      VARCHAR2 (2000);
   l_item_id                      NUMBER; 
   l_from_uom                     sy_uoms_mst.uom_code%TYPE;
   l_to_uom                       sy_uoms_mst.uom_code%TYPE;
   l_error_msg                    varchar2(4000);

   conversion_failure             EXCEPTION;
   --Bug 6908375 
   fpl_setup_error                EXCEPTION;
   
   CURSOR get_item_primary_uom (v_inventory_item_id NUMBER, v_org_id NUMBER)
      IS
         SELECT primary_uom_code
         FROM mtl_system_items_kfv
         WHERE inventory_item_id = v_inventory_item_id
         AND organization_id = v_org_id;
         
   CURSOR cur_item_no (v_org_id NUMBER, v_inventory_item_id NUMBER)
      IS
         SELECT concatenated_segments
         FROM mtl_system_items_kfv
         WHERE organization_id = v_org_id
         AND inventory_item_id = v_inventory_item_id;

   BEGIN
   IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.' || l_api_name);
   END IF;

   IF NVL (g_debug, -1) = gme_debug.g_log_statement THEN
      gme_debug.put_line (l_api_name || ': Parameters');
      gme_debug.put_line ('p_batch_id:' || p_batch_header_rec.batch_id);
      gme_debug.put_line ('p_organization_id :' || p_organization_id);
   END IF;
   
   x_return_status := fnd_api.g_ret_sts_success;
   
    -- Initialize output batch header
   x_batch_header_rec := p_batch_header_rec;
   x_material_tbl := p_material_tbl;
   
   gme_fpl_pvt.get_fixed_process_loss
                  (p_batch_id                      => p_batch_header_rec.batch_id
                  ,p_validity_rule_id              => p_batch_header_rec.recipe_validity_rule_id
                  ,p_organization_id               => p_organization_id
                  ,x_fixed_process_loss            => l_fixed_process_loss
                  ,x_fixed_process_loss_uom        => l_fixed_process_loss_uom
                  );
                  
    IF l_fixed_process_loss IS NULL OR l_fixed_process_loss_uom IS NULL THEN
       RETURN;
    END IF;
        
    IF NVL (g_debug, -1) = gme_debug.g_log_statement THEN
      gme_debug.put_line ('Fixed Process Loss :' || l_fixed_process_loss);
      gme_debug.put_line ('Fixed Process Loss UOM :'||l_fixed_process_loss_uom);
      gme_debug.put_line ('p material table count is :'||p_material_tbl.COUNT);
    END IF;

    --- Get Total of product qty's
    total_output_qty :=0;
    FOR i IN 1 .. p_material_tbl.COUNT 
    LOOP
        IF ( p_material_tbl (i).line_type <> -1) THEN
           l_new_dtl_qty := p_material_tbl(i).plan_qty;
           -- IF detail uom is not same as Fixed Process Loss type uom then convert it to Fixed Process Loss uom
           IF p_material_tbl(i).dtl_um <> l_fixed_process_loss_uom THEN
              IF (g_debug = gme_debug.g_log_statement) THEN
                  gme_debug.put_line(' product dtl uom is ' || p_material_tbl(i).dtl_um);
                  gme_debug.put_line(' product plan qty is ' || p_material_tbl(i).plan_qty);
              END IF;
              l_new_dtl_qty  :=  inv_convert.inv_um_convert
                                    (item_id            => p_material_tbl(i).inventory_item_id
                                    ,PRECISION          => gme_common_pvt.g_precision
                                    ,from_quantity      => p_material_tbl(i).plan_qty
                                    ,from_unit          => p_material_tbl(i).dtl_um
                                    ,to_unit            => l_fixed_process_loss_uom
                                    ,from_name          => NULL
                                    ,to_name            => NULL);
              IF l_new_dtl_qty < 0 THEN
                  IF (g_debug = gme_debug.g_log_statement) THEN
                     gme_debug.put_line('Failed in UOM Conv from dtl_um to fixed process loss UOM ');
                  END IF;
                  l_item_id  := p_material_tbl(i).inventory_item_id;
                  l_from_uom := p_material_tbl(i).dtl_um;
                  l_to_uom   := l_fixed_process_loss_uom; 
                  RAISE conversion_failure; 
              END IF;
           END IF;
            IF (g_debug = gme_debug.g_log_statement) THEN
                  gme_debug.put_line(' converted `product dtl uom is ' || l_new_dtl_qty);
              END IF;
           total_output_qty := NVL(total_output_qty,0) + l_new_dtl_qty;        
        END IF;
    END LOOP;
    
    IF NVL (g_debug, -1) = gme_debug.g_log_statement THEN
      gme_debug.put_line ('Total product qty :' || total_output_qty);
    END IF;
    
    --- Get Total of ingredient qty's that contribute to yield and scale type is not fixed
       total_input_qty :=0;
       FOR i IN 1 .. p_material_tbl.COUNT 
       LOOP
          IF ( p_material_tbl (i).line_type = -1
             AND p_material_tbl (i).contribute_yield_ind = 'Y' 
             AND p_material_tbl (i).scale_type <> 0 ) THEN
            l_new_dtl_qty := p_material_tbl(i).plan_qty;
            -- IF detail uom is not same as FM Yield type uom then convert it to FM Yield type uom
             IF p_material_tbl(i).dtl_um <> l_fixed_process_loss_uom THEN
                l_new_dtl_qty  :=  inv_convert.inv_um_convert
                                    (item_id            => p_material_tbl(i).inventory_item_id
                                    ,PRECISION          => gme_common_pvt.g_precision
                                    ,from_quantity      => p_material_tbl(i).plan_qty
                                    ,from_unit          => p_material_tbl(i).dtl_um
                                    ,to_unit            => l_fixed_process_loss_uom
                                    ,from_name          => NULL
                                    ,to_name            => NULL);
               IF l_new_dtl_qty < 0 THEN
                  IF (g_debug = gme_debug.g_log_statement) THEN
                     gme_debug.put_line('Failed in UOM Conv from dtl_um to fixed process loss UOM ');
                  END IF;
                  l_item_id := p_material_tbl(i).inventory_item_id;
                  l_from_uom := p_material_tbl(i).dtl_um;
                  l_to_uom := l_fixed_process_loss_uom; 
                  RAISE conversion_failure; 
               END IF;
             END IF;
             total_input_qty := NVL(total_input_qty,0) + l_new_dtl_qty;        
           END IF;
       END LOOP;
    
    IF NVL (g_debug, -1) = gme_debug.g_log_statement THEN
      gme_debug.put_line ('Total ingredient qty contributing to yield :' || total_input_qty);
    END IF;
       
    IF (p_called_from = 1 AND p_creation_mode = 'INPUT') THEN
       -- Calculate new total input qty after accounting for fixed process loss
       New_input_qty := total_input_qty - l_fixed_process_loss;
   
       Actual_output_total := (total_output_qty * new_input_qty)/total_input_qty;
       -- Bug 6908375 
       -- Raise error if new_input_qty is <=0 
       IF  NVL(New_input_qty,0) <= 0 THEN
           IF NVL (g_debug, -1) = gme_debug.g_log_statement THEN
              gme_debug.put_line ('New total input :' || new_input_qty);
              gme_debug.put_line ('returning');
            END IF;
            RAISE fpl_setup_error;
        END IF;

        IF NVL (g_debug, -1) = gme_debug.g_log_statement THEN
           gme_debug.put_line ('New total input after accounting for Fixed process loss :' || new_input_qty);
           gme_debug.put_line ('Actual total output qty based on new input qty :' || Actual_output_total);
         END IF;
     END IF;
   
    X_material_tbl  := p_material_tbl;
    -- Loop thru material detail lines.        
    FOR i IN 1 .. p_material_tbl.COUNT 
    LOOP
    -- Process only the material details lines that contribute to yield and are not fixed scale type. 
    IF ( p_material_tbl (i).contribute_yield_ind = 'Y' AND p_material_tbl (i).scale_type <> 0) THEN
         
         l_convert_dtl_uom := 0;
         
         l_new_dtl_qty := p_material_tbl(i).plan_qty;
         
         -- Verify if all material detail lines uom is same as the FM yield type uom.
         -- If not convert plan qty to  FM yield type uom.
         IF p_material_tbl(i).dtl_um <> l_fixed_process_loss_uom THEN
            l_new_dtl_qty  :=  inv_convert.inv_um_convert
                                    (item_id            => p_material_tbl(i).inventory_item_id
                                    ,PRECISION          => gme_common_pvt.g_precision
                                    ,from_quantity      => p_material_tbl(i).plan_qty
                                    ,from_unit          => p_material_tbl(i).dtl_um
                                    ,to_unit            => l_fixed_process_loss_uom
                                    ,from_name          => NULL
                                    ,to_name            => NULL);
             IF l_new_dtl_qty < 0 THEN
                  IF (g_debug = gme_debug.g_log_statement) THEN
                     gme_debug.put_line('Failed in UOM Conv from dtl_um to fixed process loss UOM ');
                  END IF;
                  l_item_id := p_material_tbl(i).inventory_item_id;
                  l_from_uom := p_material_tbl(i).dtl_um;
                  l_to_uom := l_fixed_process_loss_uom; 
                  RAISE conversion_failure; 
             END IF;
             l_convert_dtl_uom := 1;
             
         END IF;
         -- Apply fixed process loss to material detail qty.
         -- If creation mode is recipe, output or porduct then inflate the ingredient qty. 
         IF(p_creation_mode IN ('RECIPE', 'PRODUCT', 'OUTPUT') OR p_called_from = 2) THEN
            IF (p_material_tbl(i).line_type = -1) THEN 
                l_new_dtl_qty := 
                     l_new_dtl_qty + 
                     l_new_dtl_qty * (l_fixed_process_loss/total_input_qty);
            END IF;
            -- If creation mode is input then decrease the product qty.         
         ELSIF (p_called_from = 1 AND p_creation_mode = 'INPUT') THEN
            IF (p_material_tbl(i).line_type <> -1) THEN 
               l_new_dtl_qty := 
                  l_new_dtl_qty * (Actual_output_total/total_output_qty);
            END IF;
         END IF;
         
         IF l_convert_dtl_uom = 1 THEN  
            -- Convert plan qty back to material detail uom.
            l_plan_qty  :=  inv_convert.inv_um_convert
                                    (item_id            => p_material_tbl(i).inventory_item_id
                                    ,PRECISION          => gme_common_pvt.g_precision
                                    ,from_quantity      => l_new_dtl_qty
                                    ,from_unit          => l_fixed_process_loss_uom
                                    ,to_unit            => p_material_tbl(i).dtl_um
                                    ,from_name          => NULL
                                    ,to_name            => NULL);
            IF l_plan_qty < 0 THEN
                  IF (g_debug = gme_debug.g_log_statement) THEN
                     gme_debug.put_line('Failed in UOM Conv from fixed process loss UOM to detail uom ');
                  END IF;
                  l_item_id   := p_material_tbl(i).inventory_item_id;
                  l_from_uom  := l_fixed_process_loss_uom;
                  l_to_uom    := p_material_tbl(i).dtl_um;
                  RAISE conversion_failure; 
            ELSE
                  x_material_tbl(i).plan_qty  := ROUND(l_plan_qty,5); 
            END IF;
         ELSE
            x_material_tbl(i).plan_qty :=  ROUND(l_new_dtl_qty,5);                             
         END IF;     
         
         -- Only during Creating a batch apply fixed process loss to original_qty and original_primary_qty. 
         IF (p_called_from = 1) THEN
               x_material_tbl(i).original_qty :=  x_material_tbl(i).plan_qty; 
               
               -- Fetch item uom as Original primary qty is always in item uom. 
               open get_item_primary_uom( p_material_tbl(i).inventory_item_id, p_organization_id);
               FETCH get_item_primary_uom INTO l_item_uom;
               CLOSE get_item_primary_uom;
               
               x_material_tbl(i).original_primary_qty := x_material_tbl(i).plan_qty;
               
               -- If item uom is not same as detail uom, convert!
               IF (l_item_uom <> p_material_tbl(i).dtl_um) THEN
                  l_original_primary_qty  :=  inv_convert.inv_um_convert
                               		     (item_id            => p_material_tbl(i).inventory_item_id
                               		     ,PRECISION          => gme_common_pvt.g_precision
                               		     ,from_quantity      => x_material_tbl(i).original_primary_qty
                               		     ,from_unit          => p_material_tbl(i).dtl_um
                               		     ,to_unit            => l_item_uom 
                               		     ,from_name          => NULL
                               		     ,to_name            => NULL);
                  IF l_original_primary_qty < 0 THEN
                     IF (g_debug = gme_debug.g_log_statement) THEN
                        gme_debug.put_line('Failed in UOM Conv from detail uom to item uom ');
                     END IF;
                     l_item_id  := p_material_tbl(i).inventory_item_id;
                     l_from_uom := p_material_tbl(i).dtl_um;
                     l_to_uom   := l_item_uom;
                     RAISE conversion_failure; 
                  ELSE
                     x_material_tbl(i).original_primary_qty := ROUND(l_original_primary_qty,5);
                  END IF;
               END IF;
         END IF; -- p_called_from = 1
         
    END IF;   -- IF ( p_material_tbl (i).contribute_yield_ind = 'Y' AND p_material_tbl (i).scale_type <> 0)
    END LOOP; -- Loop thru material detail lines.
    
     -- set the indicator on batch header.
     x_batch_header_rec.fixed_process_loss_applied := 'Y';
        
     EXCEPTION
      WHEN conversion_failure THEN
         OPEN cur_item_no (p_organization_id, l_item_id);

         FETCH cur_item_no INTO l_item_no;

         CLOSE cur_item_no;

         x_return_status := fnd_api.g_ret_sts_error;
         fnd_message.set_name ('GMI', 'IC_API_UOM_CONVERSION_ERROR');
         fnd_message.set_token ('ITEM_NO', l_item_no);
         fnd_message.set_token ('FROM_UOM', l_from_uom);
         fnd_message.set_token ('TO_UOM', l_to_uom);
         fnd_msg_pub.ADD;         
     -- Bug 6908375  
     WHEN fpl_setup_error THEN
        x_return_status := fnd_api.g_ret_sts_error;
        FND_MESSAGE.SET_NAME('GME','GME_FPL_SETUP_ERROR');
        APP_EXCEPTION.RAISE_EXCEPTION;
   END;

PROCEDURE FPL_batch_details (
          p_batch_header_rec  IN  gme_batch_header%ROWTYPE
         ,p_called_from       IN  NUMBER DEFAULT 1 /*1 = Create Batch, 2 = Batch details */
         ,p_init_msg_list     IN              VARCHAR2 := fnd_api.g_false
         ,x_message_count     OUT NOCOPY      NUMBER
         ,x_message_list      OUT NOCOPY      VARCHAR2
         ,x_return_status     OUT NOCOPY      VARCHAR2
         )
IS
CURSOR cur_get_matl (v_batch_id NUMBER) IS
  SELECT   *
  FROM gme_material_details
  WHERE batch_id = v_batch_id
  ORDER BY line_no; 
  
/*ER 19161894  Shaliu Chen 18-JUL-2014*/  
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
     
      
  l_api_name           CONSTANT VARCHAR2 (30)            := 'FPL_batch_details';
  x_batch_header_rec    gme_batch_header%ROWTYPE;
  l_material_tbl        gme_common_pvt.material_details_tab;
  x_material_tbl        gme_common_pvt.material_details_tab;
  i                     NUMBER := 0;
  l_row_count           NUMBER;
  err_msg               varchar2(2000);
  /*ER 19161894  Shaliu Chen 18-JUL-2014*/
  l_propagate_change_to_po       NUMBER;
  l_osp_resource_flag            NUMBER;
  l_batchstep_resource_id        NUMBER;
  /*END ER 19161894*/  
  Fixed_process_loss_failure     EXCEPTION;
  material_save_failed           EXCEPTION;
  setup_failure                  EXCEPTION;
  error_update_batch             EXCEPTION;
  qty_sync_fail                  EXCEPTION;  /*ER 19161894  Shaliu Chen 18-JUL-2014*/
BEGIN
      x_return_status := fnd_api.g_ret_sts_success;

      IF (g_debug <> -1) THEN
         gme_debug.log_initialize ('FixedProcessLoss');
      END IF;

      IF NVL (g_debug, -1) = gme_debug.g_log_statement THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name);
      END IF;
      gme_debug.put_line('batch id is ' || p_batch_header_rec.batch_id); 
      FOR l_rec IN cur_get_matl (p_batch_header_rec.batch_id) LOOP
         i := i + 1; 
         l_material_tbl (i) := l_rec;
      END LOOP;
   
      IF NVL (g_debug, -1) = gme_debug.g_log_statement THEN 
         gme_debug.put_line('before call to gme_fpl_pvt.apply_fixed_process_loss'); 
      END IF;
      -- Initialize output batch header
      x_batch_header_rec := p_batch_header_rec;    
      x_material_tbl := l_material_tbl;
      gme_fpl_pvt.apply_fixed_process_loss
                  (p_batch_header_rec         => p_batch_header_rec
                  ,p_material_tbl             => l_material_tbl
                  ,p_organization_id          => p_batch_header_rec.organization_id
                  ,p_creation_mode            => 'OUTPUT'
                  ,p_called_from              => p_called_from
                  ,x_batch_header_rec         => x_batch_header_rec
                  ,x_material_tbl             => x_material_tbl
                  ,x_return_status            => x_return_status
                  );
         IF (g_debug = gme_debug.g_log_statement) THEN
             gme_debug.put_line ('Return from Apply Fixed Proccess Loss is ' || x_return_status);
         END IF;
         IF x_return_status <> fnd_api.g_ret_sts_success THEN
            RAISE Fixed_process_loss_failure;
         END IF;
         IF NOT gme_common_pvt.g_setup_done THEN
         gme_common_pvt.g_setup_done :=
                    gme_common_pvt.setup (p_batch_header_rec.organization_id);

         IF NOT gme_common_pvt.g_setup_done THEN
            x_return_status := fnd_api.g_ret_sts_error;
            RAISE setup_failure;
         END IF;
         END IF;
         gme_common_pvt.set_timestamp;
         -- Update the batch header
         IF NOT gme_batch_header_dbl.update_row 
                 (p_batch_header => x_batch_header_rec) THEN
            RAISE error_update_batch;
         END IF;

         FOR i IN 1..x_material_tbl.COUNT LOOP
            IF (g_debug = gme_debug.g_log_statement) THEN
               gme_debug.put_line('Before calling gme_material_details_dbl.update_row');
            END IF; 
            IF NOT (gme_material_details_dbl.update_row (x_material_tbl (i) )) THEN
               RAISE material_save_failed;
            ELSE             
               x_material_tbl (i).last_update_date := gme_common_pvt.g_timestamp;
               x_material_tbl (i).last_updated_by := gme_common_pvt.g_user_ident;
               x_material_tbl (i).last_update_login := gme_common_pvt.g_login_id;
            END IF; 
        END LOOP;
        
        /*                                    
        BEGIN ER 19161894  Shaliu Chen 18-JUL-2014               
        Added for OPM Step Level OSP Project
        Primary product Plan quantity/WIP plan quantity change,
        Plan resource total usage change will synchronize to 
        corresponding PO/Requisition if 'propagate change to
        po' parameter value is Automatic.  
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
          OPEN cur_get_osp_parameter(x_batch_header_rec.organization_id);
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
             Shaliu Chen 09-APR-2015               
             Change to loop each osp step and invoke requisition creation for each osp step. 
            */                                     
            IF l_osp_resource_flag > 0 THEN
              FOR cur_gaos IN cur_get_all_osp_steps(x_batch_header_rec.batch_id) LOOP 
                l_batchstep_resource_id := cur_gaos.batchstep_resource_id;                           
                /*
                  Invoke Batch product quantity change to the PO/Req
                  linked to the batch.
                */                                                                                                                
                gme_osp.updatePOReqQuantity(p_batch_id                    => x_batch_header_rec.batch_id,
                                            p_organization_id             => x_batch_header_rec.organization_id,  
                                            p_batchstep_resource_id       => l_batchstep_resource_id,                             
                                            x_return_status               => x_return_status,
                                            x_message_list                => x_message_list,
                                            x_message_count               => x_message_count);                                                                                  
                                                                                 
                                              
                IF x_return_status <> fnd_api.g_ret_sts_success THEN       
                  RAISE qty_sync_fail; 
                END IF;
              END LOOP;
            END IF;
          END IF;
        EXCEPTION
          WHEN OTHERS THEN
            RAISE qty_sync_fail;          
        END;     
        /*END ER 19161894*/     
EXCEPTION   
WHEN  Fixed_process_loss_failure THEN
      IF (g_debug = gme_debug.g_log_statement) THEN
          gme_debug.put_line ('Failed to apply FPL');
      END IF;
WHEN error_update_batch THEN
     x_return_status := FND_API.g_ret_sts_unexp_error;
WHEN material_save_failed THEN
      IF (g_debug = gme_debug.g_log_statement) THEN
          gme_debug.put_line ('Material save failed');
      END IF;
       x_return_status := fnd_api.g_ret_sts_error;
WHEN setup_failure THEN
         --ROLLBACK TO SAVEPOINT create_batch;
         --x_batch_header_rec := NULL;
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
         x_return_status := fnd_api.g_ret_sts_error;
/*ER 19161894  Shaliu Chen 18-JUL-2014*/         
WHEN qty_sync_fail THEN
  IF x_message_list IS NULL THEN
    gme_common_pvt.log_message('GME_QTY_SYNC_FAILED');
    gme_common_pvt.count_and_get (x_count        => x_message_count
                                 ,p_encoded      => fnd_api.g_false
                                 ,x_data         => x_message_list);    
  END IF;
  x_return_status := 'W';         
END ;
      
END gme_fpl_pvt;
/

COMMIT ;
EXIT;

--show errors ;
