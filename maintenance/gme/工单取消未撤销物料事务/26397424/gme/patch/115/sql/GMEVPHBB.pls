/* +======================================================================+ */
/* |    Copyright (c) 2005, 2017 Oracle and/or its affiliates.           | */
/* |                         All rights reserved.                         | */
/* |                           Version 12.0.0                             | */
/* +======================================================================+ */
REM dbdrv: sql ~PROD ~PATH ~FILE none none none package &phase=plb \
REM dbdrv: checkfile(120.6.12010000.6=120.7.12020000.5)(120.5.12000000.4=120.6.12010000.3)(115.47=120.1):~PROD:~PATH:~FILE
SET VERIFY OFF
WHENEVER SQLERROR EXIT FAILURE ROLLBACK;
WHENEVER OSERROR EXIT FAILURE ROLLBACK;

CREATE OR REPLACE PACKAGE BODY gme_phantom_pvt AS
/*  $Header: GMEVPHBB.pls 120.7.12020000.5 2017/05/25 07:07:32 maychen ship $    */
/*
REM **********************************************************************
REM *                                                                    *
REM * FILE:    GMEVPHBB.pls                                              *
REM * PURPOSE: Package Body for the GME PHANTOM API         routines     *
REM * AUTHOR:  Thomas Daniel, OPM Development                            *
REM * DATE:    July 10th 2001                                            *
REM * HISTORY:                                                           *
REM * ========           
     04-JUN-2009 G. Muratore     Bug 8490219 
        No need to timestamp batch header record when updating the 
        parentline_id as it was just created. 
        PROCEDURE: create_phantom
        
     07-OCT-2012 G. Muratore     Bug 14681911 
        Refresh the variable data before the next update 
        so that the timestamp is accurate. 
        PROCEDURE: create_phantom
REM **********************************************************************
*/

   /*************************************************************************
   * This file contains procedures for the Phantom Batch APIs for GME in    *
   * Oracle Process Manufacturing (OPM). Each procedure has a common set of *
   * parameters to which API-specific parameters are appended.              *
   *************************************************************************/
   g_pkg_name   CONSTANT VARCHAR2 (30) := 'gme_phantom_pvt';
   g_debug               VARCHAR2 (5)  := fnd_profile.VALUE ('AFLOG_LEVEL');

   /*===========================================================================================
   Procedure
      create_phantom
   Description
     This particular procedure creates the phantom batch for the material detail line.
   Parameters
     p_material_detail     The material detail row to identify the detail line.
     x_material_detail     The material detail out row.
     p_validity_rule_id    Validity rule id for creating the phantom batch.
     p_ignore_shortages            Ignore shortages in the batch auto allocation.
     x_return_status    outcome of the API call
               S - Success
               E - Error
               W - Warning
               U - Unexpected error
   History
     Jun 19 2002 Chandrashekar Tiruvidula Bug# 2378540
     Moved code from within
     Jul 24 2002 Shrikant Nene Bug# 2386578 Added parameter
       p_batch_no
     07-Sep-2006 Namit S. Bug#5436643 Pass Revision Number during create batch
     11-JAN-2007 Swapna K Bug#6738476 Added parameter,p_batch_header_rec
          
     04-JUN-2009 G. Muratore     Bug 8490219 
        No need to timestamp batch header record when updating the parentline_id as it was just created. 
        
     19-MAY-2015  Shaliu Chen     BUG 201102335
        Modify for Batch On Hold enhancement,add validation to raise an error
        if batch is on hold with STOP type.          
   =============================================================================================*/
   PROCEDURE create_phantom (
      p_material_detail_rec      IN              gme_material_details%ROWTYPE
     ,p_batch_header_rec         IN              gme_batch_header%ROWTYPE 
     ,p_batch_no                 IN              VARCHAR2 DEFAULT NULL
     ,x_material_detail_rec      OUT NOCOPY      gme_material_details%ROWTYPE
     ,p_validity_rule_id         IN              NUMBER
     ,p_use_workday_cal          IN              VARCHAR2
     ,p_contiguity_override      IN              VARCHAR2
     ,p_use_least_cost_validity_rule     IN      VARCHAR2 := fnd_api.g_false
     ,x_exception_material_tbl   OUT NOCOPY      gme_common_pvt.exceptions_tab
     ,x_return_status            OUT NOCOPY      VARCHAR2)
   IS
      l_api_name           CONSTANT VARCHAR2 (30)         := 'CREATE_PHANTOM';
      /* Exception definitions */
      material_detail_fetch_error   EXCEPTION;
      batch_header_fetch_error      EXCEPTION;
      batch_upd_error               EXCEPTION;
      not_a_phantom                 EXCEPTION;
      phantom_exists                EXCEPTION;
      validity_fetch_error          EXCEPTION;
      no_validity_found             EXCEPTION;
      batch_creation_failed         EXCEPTION;
      phantom_item_not_found        EXCEPTION;
      /* Local variables */
      l_object_type                 VARCHAR2 (1);
      l_return_code                 NUMBER (5);
      l_validity_rule_id            NUMBER;
      l_msg_count                   NUMBER;
      l_msg_list                    VARCHAR2 (2000);
      l_plan_qty                    NUMBER;
      l_return_status               VARCHAR2 (1);
      l_prod_mtl_dtl_id             NUMBER;
      l_batch_header                gme_batch_header%ROWTYPE;
      l_phant_batch                 gme_batch_header%ROWTYPE;
      l_in_phant_batch              gme_batch_header%ROWTYPE;
      l_material_detail             gme_material_details%ROWTYPE;
      l_in_material_detail          gme_material_details%ROWTYPE;
      l_validity_tbl                gmd_validity_rules.recipe_validity_tbl;
       
      l_exists                      NUMBER;

      CURSOR cur_step_matl_detail (v_material_detail_id NUMBER)
      IS
         SELECT d.plan_start_date, d.plan_cmplt_date
           FROM gme_batch_step_items i, gme_batch_steps d
          WHERE d.batchstep_id = i.batchstep_id
            AND i.material_detail_id = v_material_detail_id;

      l_step_rec                    cur_step_matl_detail%ROWTYPE;

      CURSOR cur_get_phantom_prod (
         v_batch_id            NUMBER
        ,v_inventory_item_id   NUMBER)
      IS
         SELECT   material_detail_id
             FROM gme_material_details
            WHERE batch_id = v_batch_id
              AND inventory_item_id = v_inventory_item_id
              AND line_type = gme_common_pvt.g_line_type_prod
         ORDER BY line_no;
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      /* Set the return status to success initially */
      x_return_status := fnd_api.g_ret_sts_success;

      IF NOT (gme_material_details_dbl.fetch_row (p_material_detail_rec
                                                 ,x_material_detail_rec) ) THEN
         RAISE material_detail_fetch_error;
      END IF;

      l_batch_header.batch_id := x_material_detail_rec.batch_id;

      IF NOT (gme_batch_header_dbl.fetch_row (l_batch_header, l_batch_header) ) THEN
         RAISE batch_header_fetch_error;
      END IF;

        --Bug#6738476 This is just to assign the attirbute information of the flexfields
       l_phant_batch := p_batch_header_rec;
       
      /* Shaliu Chen     19-MAY-2015  BUG 201102335
         raise an error if batch is on hold with STOP type
      */
      IF gme_common_pvt.get_batch_hold_status(l_batch_header.batch_id) = 'S' THEN
        gme_common_pvt.log_message('GME_BATCH_ONHOLD_CHECK',
                                   'ACTION_NAME',
                                   'Create Phantom');
        RAISE fnd_api.g_exc_error;
      END IF;        

      /* Check for phantom specific validations */
      IF x_material_detail_rec.phantom_type = 0 THEN
         gme_common_pvt.log_message ('PM_NOTAPHANTOM');
         RAISE not_a_phantom;
      ELSIF NVL (x_material_detail_rec.phantom_id, 0) > 0 THEN
         gme_common_pvt.log_message ('PM_PHANTOM_EXISTS');
         RAISE phantom_exists;
      END IF;

      /* No need for scrap factor since plan_qty now includes scrap factor */
      l_plan_qty := x_material_detail_rec.plan_qty;

      /* Check for the validity rule passed in */
      IF p_validity_rule_id IS NULL THEN
         IF l_batch_header.laboratory_ind = 1 THEN
            l_object_type := 'L';
         ELSIF l_batch_header.batch_type = gme_common_pvt.g_doc_type_fpo THEN
            l_object_type := 'F';
         ELSE
            l_object_type := 'P';
         END IF;

         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ':'
                                || 'Get VR for phantom ingredient '||x_material_detail_rec.inventory_item_id||' revision '||x_material_detail_rec.revision);
         END IF;

         gmd_val_data_pub.get_val_data
                        (p_api_version              => 1.0
                        ,p_object_type              => l_object_type
                        ,p_item_id                  => x_material_detail_rec.inventory_item_id
                        ,p_product_qty              => l_plan_qty
                        ,p_uom                      => x_material_detail_rec.dtl_um
                        ,p_revision                 => x_material_detail_rec.revision --nsinghi bug#5436643 Pass Revision Number
                        ,p_start_date               => NULL
                        ,p_end_date                 => l_batch_header.plan_start_date
                        ,p_organization_id          => l_batch_header.organization_id
                        ,p_least_cost_validity      => p_use_least_cost_validity_rule
                        ,x_return_status            => x_return_status
                        ,x_msg_count                => l_msg_count
                        ,x_msg_data                 => l_msg_list
                        ,x_return_code              => l_return_code
                        ,x_recipe_validity_out      => l_validity_tbl);

         IF x_return_status <> fnd_api.g_ret_sts_success THEN
            RAISE validity_fetch_error;
         ELSIF l_validity_tbl.COUNT = 0 THEN
            RAISE no_validity_found;
         END IF;

         l_validity_rule_id := l_validity_tbl (1).recipe_validity_rule_id;
      ELSE
         l_validity_rule_id := p_validity_rule_id;
      END IF;                              /* IF p_validity_rule_id IS NULL */
      IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ':'
                             || 'VR for phantom ingredient '||l_validity_rule_id);
      END IF;

      l_phant_batch.organization_id := l_batch_header.organization_id;
      l_phant_batch.plan_cmplt_date := l_batch_header.plan_start_date;
      l_phant_batch.batch_type := l_batch_header.batch_type;
      l_phant_batch.update_inventory_ind :=
                                           l_batch_header.update_inventory_ind;
      l_phant_batch.laboratory_ind := l_batch_header.laboratory_ind;
      l_phant_batch.batch_no := p_batch_no;
      l_phant_batch.recipe_validity_rule_id := l_validity_rule_id;

      IF x_material_detail_rec.release_type IN
            (gme_common_pvt.g_mtl_autobystep_release
            ,gme_common_pvt.g_mtl_incremental_release
            ,gme_common_pvt.g_mtl_manual_release) THEN
         OPEN cur_step_matl_detail (x_material_detail_rec.material_detail_id);

         FETCH cur_step_matl_detail
          INTO l_step_rec;

         IF cur_step_matl_detail%FOUND THEN
            l_phant_batch.plan_cmplt_date := l_step_rec.plan_start_date;
         END IF;

         CLOSE cur_step_matl_detail;
      END IF;

      IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ':'
                             || 'Calling Create_batch');
      END IF;

      l_in_phant_batch := l_phant_batch;
      SAVEPOINT create_auto_phantom;
      gme_create_batch_pvt.create_batch
                        (x_return_status               => x_return_status
                        ,p_batch_header_rec            => l_in_phant_batch
                        ,x_batch_header_rec            => l_phant_batch
                        ,p_batch_size                  => l_plan_qty
                        ,p_batch_size_uom              => x_material_detail_rec.dtl_um
                        ,p_creation_mode               => 'PRODUCT'
                        ,p_use_workday_cal             => p_use_workday_cal
                        ,p_contiguity_override         => p_contiguity_override
                        ,x_exception_material_tbl      => x_exception_material_tbl
                        ,p_is_phantom                  => 'Y');

      IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
         gme_debug.put_line ('create_batch-return_status ' || x_return_status);
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ':'
                             || ' Create_batch returned '
                             || x_return_status);
      END IF;

      IF     x_return_status <> fnd_api.g_ret_sts_success
         AND x_return_status <> gme_common_pvt.g_inv_short_err THEN
         ROLLBACK TO SAVEPOINT create_auto_phantom;
         RAISE batch_creation_failed;
      END IF;

      IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
         gme_debug.put_line ('Update batch_id '|| l_phant_batch.batch_id||'  with parentline_id '||p_material_detail_rec.material_detail_id);
      END IF;

      -- Bug 14681911 - Refresh the variable data before the next update.
      IF NOT (gme_batch_header_dbl.fetch_row (l_phant_batch, l_phant_batch)) THEN
         RAISE batch_header_fetch_error;
      END IF;

      l_phant_batch.parentline_id := p_material_detail_rec.material_detail_id;
      
      -- Bug 8490219 - No need to timestamp this record as it was just created.
      -- This leads to problems if a customized call is made in the interim and timestamp is
      -- changed. Also, the update_row function stamps it properly so this is redundant.
      -- l_phant_batch.last_update_date := gme_common_pvt.g_timestamp;

      IF NOT gme_batch_header_dbl.update_row (p_batch_header      => l_phant_batch) THEN
         RAISE batch_upd_error;
      END IF;

      OPEN cur_get_phantom_prod (l_phant_batch.batch_id
                                ,x_material_detail_rec.inventory_item_id);

      FETCH cur_get_phantom_prod
       INTO l_prod_mtl_dtl_id;

      CLOSE cur_get_phantom_prod;

      IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
         gme_debug.put_line ('Lets Update line ids now');
         gme_debug.put_line ('Update phantom prod material_detail_id '|| l_prod_mtl_dtl_id||'  with phantom_line_id '||p_material_detail_rec.material_detail_id);
      END IF;

      UPDATE gme_material_details
         SET phantom_line_id = p_material_detail_rec.material_detail_id
            ,release_type = x_material_detail_rec.release_type
            ,subinventory = x_material_detail_rec.subinventory
            ,locator_id = x_material_detail_rec.locator_id
            ,last_update_date = gme_common_pvt.g_timestamp
            ,last_updated_by = gme_common_pvt.g_user_ident
            ,last_update_login = gme_common_pvt.g_login_id
       WHERE material_detail_id = l_prod_mtl_dtl_id;

      x_material_detail_rec.phantom_id := l_phant_batch.batch_id;
      x_material_detail_rec.phantom_line_id := l_prod_mtl_dtl_id;

      IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
         gme_debug.put_line (' ');
         gme_debug.put_line ('Update material_detail_id '|| p_material_detail_rec.material_detail_id||'  with phantom_line_id '||l_prod_mtl_dtl_id);
      END IF;

      UPDATE gme_material_details
         SET phantom_line_id = l_prod_mtl_dtl_id
            ,phantom_id = l_phant_batch.batch_id
            ,last_update_date = gme_common_pvt.g_timestamp
            ,last_updated_by = gme_common_pvt.g_user_ident
            ,last_update_login = gme_common_pvt.g_login_id
       WHERE material_detail_id = p_material_detail_rec.material_detail_id;
       
       
     --BUG 25999337   Shaliu Chen 11-MAY-2017
      --insert one record into gme_batch_genealogy if not exist,and set heat number as NULL, parent_batch_id as -1
      SELECT count(1)
        INTO l_exists
        FROM gme_batch_genealogy
       WHERE batch_id = l_phant_batch.batch_id;

      IF l_exists = 0 THEN
        gme_yield_calculation_pvt.batch_heat_insert( p_batch_id        => l_phant_batch.batch_id
                                                    ,p_heat_number     => NULL
                                                    ,p_parent_batch_id => -1 );
      END IF;
      --END BUG 25999337       

      x_return_status := fnd_api.g_ret_sts_success;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;
   EXCEPTION
      WHEN batch_creation_failed OR material_detail_fetch_error OR not_a_phantom OR phantom_exists OR batch_header_fetch_error OR validity_fetch_error OR batch_creation_failed OR batch_upd_error THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN no_validity_found THEN
         x_return_status := 'W';
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
   END create_phantom;

   /*===========================================================================================
   Function
      is_phantom
   Description
     This particular function checks wether the batch passed in is a phantom batch.
   Parameters
     p_batch_header     The batch header row to identify the batch.
     x_return_status    outcome of the API call
               S - Success
               E - Error
               U - Unexpected error
   =============================================================================================*/
   FUNCTION is_phantom (
      p_batch_header    IN              gme_batch_header%ROWTYPE
     ,x_return_status   OUT NOCOPY      VARCHAR2)
      RETURN BOOLEAN
   IS
      l_batch_header        gme_batch_header%ROWTYPE;
      error_fetch_header    EXCEPTION;
      l_api_name   CONSTANT VARCHAR2 (30)              := 'IS_PHANTOM';
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      x_return_status := fnd_api.g_ret_sts_success;

      IF NOT gme_batch_header_dbl.fetch_row (p_batch_header      => p_batch_header
                                            ,x_batch_header      => l_batch_header) THEN
         RAISE error_fetch_header;
      END IF;

      IF NVL (l_batch_header.parentline_id, 0) > 0 THEN
         RETURN TRUE;
      ELSE
         RETURN FALSE;
      END IF;
   EXCEPTION
      WHEN error_fetch_header THEN
         x_return_status := fnd_api.g_ret_sts_error;
         RETURN FALSE;
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
   END is_phantom;

   /*===========================================================================================
   Procedure
      fetch_step_phantoms
   Description
     This particular procedure returns all the phantom IDs of the materials
     associated with the batch step.
   Parameters
     p_batch_id         The batch id of the step.
     p_batchstep_id     Batch step ID of the step.
     x_phantom_ids              The out row with all the phantom IDs.
     x_return_status    outcome of the API call
               S - Success
               E - Error
               U - Unexpected error

   History
     Bug 3045672 - added p_all_release_type_assoc parameter and into cursor
   =============================================================================================*/
   PROCEDURE fetch_step_phantoms (
      p_batch_id                 IN              NUMBER
     ,p_batchstep_id             IN              NUMBER
     ,p_all_release_type_assoc   IN              NUMBER DEFAULT 0
     ,x_phantom_ids              OUT NOCOPY      gme_common_pvt.number_tab
     ,x_return_status            OUT NOCOPY      VARCHAR2)
   IS
      l_api_name   CONSTANT VARCHAR2 (30) := 'FETCH_STEP_PHANTOMS';

      CURSOR cur_step_matl_detail_ids (
         v_batch_id       NUMBER
        ,v_batchstep_id   NUMBER)
      IS
         SELECT d.phantom_id
           FROM gme_batch_step_items i, gme_material_details d
          WHERE d.batch_id = v_batch_id
            AND i.material_detail_id = d.material_detail_id
            AND batchstep_id = v_batchstep_id
            AND (   d.release_type = gme_common_pvt.g_mtl_autobystep_release
                 OR (    d.release_type IN
                            (gme_common_pvt.g_mtl_manual_release
                            ,gme_common_pvt.g_mtl_incremental_release)
                     AND p_all_release_type_assoc = 1) )
            AND NVL (d.phantom_id, 0) > 0;

      no_keys               EXCEPTION;
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      x_return_status := fnd_api.g_ret_sts_success;

      IF (p_batch_id IS NULL) OR (p_batchstep_id IS NULL) THEN
         RAISE no_keys;
      END IF;

      OPEN cur_step_matl_detail_ids (p_batch_id, p_batchstep_id);

      FETCH cur_step_matl_detail_ids
      BULK COLLECT INTO x_phantom_ids;

      CLOSE cur_step_matl_detail_ids;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;
   EXCEPTION
      WHEN no_keys THEN
         x_return_status := fnd_api.g_ret_sts_error;
         gme_common_pvt.log_message ('GME_NO_KEYS'
                                    ,'TABLE_NAME'
                                    ,'FETCH_STEP_PHANTOMS');
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
   END fetch_step_phantoms;

   /*===========================================================================================
   Procedure
      fetch_line_phantoms
   Description
     This particular procedure returns all the phantom IDs of the materials
     associated which are non step release type.
   Parameters
     p_batch_id         The batch id of the step.
     p_include_step     Wether to include phantoms of step release line.
     x_phantom_ids              The out row with all the phantom IDs.
     x_return_status    outcome of the API call
               S - Success
               E - Error
               U - Unexpected error
   =============================================================================================*/
   PROCEDURE fetch_line_phantoms (
      p_batch_id        IN              NUMBER
     ,p_include_step    IN              BOOLEAN DEFAULT TRUE
     ,x_phantom_ids     OUT NOCOPY      gme_common_pvt.number_tab
     ,x_return_status   OUT NOCOPY      VARCHAR2)
   IS
      l_api_name   CONSTANT VARCHAR2 (30) := 'FETCH_LINE_PHANTOMS';

      CURSOR cur_matl_phant_ids (v_batch_id NUMBER, v_release_type NUMBER)
      IS
         SELECT d.phantom_id
           FROM gme_material_details d
          WHERE d.batch_id = v_batch_id
            AND d.release_type <= v_release_type
            AND NVL (d.phantom_id, 0) > 0;

      no_keys               EXCEPTION;
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      x_return_status := fnd_api.g_ret_sts_success;

      IF (p_batch_id IS NULL) THEN
         RAISE no_keys;
      END IF;

      IF p_include_step THEN
         OPEN cur_matl_phant_ids (p_batch_id
                                 ,gme_common_pvt.g_mtl_autobystep_release);
      ELSE
         OPEN cur_matl_phant_ids (p_batch_id
                                 ,gme_common_pvt.g_mtl_incremental_release);
      END IF;

      FETCH cur_matl_phant_ids
      BULK COLLECT INTO x_phantom_ids;

      CLOSE cur_matl_phant_ids;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;
   EXCEPTION
      WHEN no_keys THEN
         x_return_status := fnd_api.g_ret_sts_error;
         gme_common_pvt.log_message ('GME_NO_KEYS'
                                    ,'TABLE_NAME'
                                    ,'FETCH_LINE_PHANTOMS');
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
   END fetch_line_phantoms;
END gme_phantom_pvt;
/

COMMIT ;
EXIT;
