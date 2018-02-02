/* +======================================================================+ */
/* |    Copyright (c) 2005, 2017 Oracle and/or its affiliates.           | */
/* |                         All rights reserved.                         | */
/* |                           Version 12.0.0                             | */
/* +======================================================================+ */
REM dbdrv: sql ~PROD ~PATH ~FILE none none none package &phase=plb \
REM dbdrv: checkfile(120.16.12010000.22=120.25.12020000.12)(120.16.12010000.20=120.25.12020000.10)(115.69=120.1):~PROD:~PATH:~FILE
SET VERIFY OFF
WHENEVER SQLERROR EXIT FAILURE ROLLBACK;
WHENEVER OSERROR EXIT FAILURE ROLLBACK;
CREATE OR REPLACE PACKAGE BODY gme_create_step_pvt AS
/*  $Header: GMEVCRSB.pls 120.25.12020000.12 2017/02/27 14:59:13 sjawaji ship $ */

/*
REM **********************************************************************
REM *                                                                    *
REM * FILE:    GMEVCRSB.pls                                              *
REM * PURPOSE: Package Body for the GME CREATE BATCH STEP routines       *
REM * AUTHOR:  Toni Newbury, OPM Development                             *
REM * DATE:    February 18th 2001                                        *
REM * HISTORY:                                                           *
REM * ========                                                           *
REM * Sivakumar.G 14-NOV-2005 FPBug#4395561                              *
REM *  Added calls to default flex field values of steps, step activities*
REM *  ,step resources,process parameters and resource txns              *
REM * SivakumarG 05-APR-2006                                             *
REM *  Put some debug messages in the edit text copy code                *
REM * SivakumarG 09-JUN-2006 Bug#5231180                                 *
REM *  Modified the code to calculate mass qty and volume qty when we are*
REM *  inserting a new step                                              *
REM * Archana Mundhe Bug 5763818 Modified the code to use ERES constants * 
REM * that are added to gme_common_pvt instead of using the hardcoded    *
REM * ERES event names such as 'oracle.apps.gme...'                      *
REM * Srinivasulu Puri 08-Aug-08 Modified calc_longest_time procedure to *
REM * outer join to prior step details. with out outer join it is        *
REM * restricting the rows to last level only.                           *

REM * G. Muratore    24-Nov-2008   Bug 7341534 
REM *   Frontport of 6774660/5618732 Reinstate rewritten 11i function.
REM *   Renamed calc_longest_time R12 function with "_orig". Hierarchical 
REM *   query does not handle data where there are no step dependencies.
REM *   Also, ported new function get_longest_in_branch.

REM * G. Muratore    28-May-2010   Bug 9694223 
REM *   Don't create step dependencies if the line does not exist in the
REM *   batch details. This can happen when rerouting a batch but where
REM *   original ingredients were removed or new ones added. 

REM * G. Muratore    09-Jul-2010   Bug 9862326 
REM *   Account for all dependent delays when deriving duration of
REM *   a given step when using start to start dependency.
REM *   Procedure: get_longest_in_branch 

REM * G. Muratore    30-SEP-2010   Bug 10087071
REM *   Additional debug messages added.

REM * G. Muratore    27-Jan-2010   Bug 10226452 (This is an 11i frontport)
REM *   Account for all dependent delays when deriving duration of a given step
REM *   when using start to start dependency. Additional work on top of 9862326. 
REM *   PROCEDURE: get_longest_in_branch

REM * G. Muratore    01-Mar-2011   Bug 11818852
REM *   Increase variable size introduced in 10226452.
REM *   PROCEDURE: get_longest_in_branch

REM * G. Muratore    01-Mar-2011   Bug 9212573  11i frontport
REM *   New parameter p_trans_count added to insert_resource_txns.
REM *   This will tell us if there is only one transaction required and
REM *   therefore it can derive the txn usage with a simpler algorithm.
REM *   We had to do this for a customer that has a setup where they 
REM *   measure resources in fractions of a second.
REM *   PROCEDURE: insert_resource_txns

REM * G. Muratore    05-Nov-2012   Bug 14769220
REM *   Initialize uom global for this code to work correctly.
REM *   PROCEDURE: get_usage_in_hours

REM * G. Muratore    28-Aug-2013   Bug 17363801/17187401
REM *   When inserting a step on the fly call new gmd function to fetch proper qc status. 
REM *   PROCEDURE: create_batch_steps

REM * G. Muratore    23-Oct-2013   Bug 16297308
REM *   Reset plan usage to consider actual usage when rescheduling a wip step.
REM *   PROCEDURE: calc_dates and get_max_duration

REM * G. Muratore    20-Nov-2013   Bug 17410850 (fix on top of 16297308)
REM *   Do not update planned start date when rescheduling a wip step.
REM *   PROCEDURE: create_batch_steps  

REM * G. Muratore    12-Dec-2016   Bug 25246717/25241112
REM *   Add batch_id join to Improve performance of query.
REM *   PROCEDURE: get_longest_in_branch  
REM **********************************************************************
* This file contains the procedure for create batch steps in Oracle      *
* Process Manufacturing (OPM). Each procedure has a common set of        *
* parameters to which API-specific parameters are appended.              *
*************************************************************************/
   g_debug               VARCHAR2 (5)  := fnd_profile.VALUE ('AFLOG_LEVEL');
   g_pkg_name   CONSTANT VARCHAR2 (30) := 'gme_create_step_pvt';

   PROCEDURE create_batch_steps (
      p_recipe_rout_step_tbl   IN              gmd_recipe_fetch_pub.recipe_step_tbl
     ,p_recipe_rout_act_tbl    IN              gmd_recipe_fetch_pub.oprn_act_tbl
     ,p_recipe_rout_resc_tbl   IN              gmd_recipe_fetch_pub.oprn_resc_tbl
     ,p_resc_parameters_tbl    IN              gmd_recipe_fetch_pub.recp_resc_proc_param_tbl
     ,p_recipe_rout_matl_tbl   IN              gmd_recipe_fetch_pub.recipe_rout_matl_tbl
     ,p_routing_depd_tbl       IN              gmd_recipe_fetch_pub.routing_depd_tbl
     ,p_gme_batch_header_rec   IN              gme_batch_header%ROWTYPE
     ,p_use_workday_cal        IN              VARCHAR2
     ,p_contiguity_override    IN              VARCHAR2
     ,x_return_status          OUT NOCOPY      VARCHAR2
     ,p_ignore_qty_below_cap   IN              VARCHAR2
            DEFAULT fnd_api.g_true
     ,p_step_start_date        IN              DATE := NULL
     ,p_step_cmplt_date        IN              DATE := NULL
     ,p_step_due_date          IN              DATE := NULL)
   IS
      -- Local variables
      l_gme_batch_steps             gme_create_step_pvt.gme_batch_steps_tab
                                := gme_create_step_pvt.gme_batch_steps_tab
                                                                          ();
      l_gme_batch_step_activities   gme_create_step_pvt.gme_batch_step_activities_tab
                      := gme_create_step_pvt.gme_batch_step_activities_tab
                                                                          ();
      l_gme_batch_step_resources    gme_create_step_pvt.gme_batch_step_resources_tab
                       := gme_create_step_pvt.gme_batch_step_resources_tab
                                                                          ();
      l_gme_rsrc_parameters         gme_create_step_pvt.gme_process_parameters_tab
                         := gme_create_step_pvt.gme_process_parameters_tab
                                                                          ();
      l_gme_batch_step_items        gme_create_step_pvt.gme_batch_step_items_tab
                           := gme_create_step_pvt.gme_batch_step_items_tab
                                                                          ();
      l_gme_batch_step_dep          gme_create_step_pvt.gme_batch_step_dep_tab
                             := gme_create_step_pvt.gme_batch_step_dep_tab
                                                                          ();
      l_step_charge_rsrc_tab        gme_create_step_pvt.step_charge_rsrc_tab;

      TYPE l_batchstep_ids IS TABLE OF NUMBER
         INDEX BY BINARY_INTEGER;

      i                             PLS_INTEGER;
      j                             PLS_INTEGER;
      l_batch_id                    gme_batch_header.batch_id%TYPE;
      l_step_tbl                    gmd_auto_step_calc.step_rec_tbl;
      l_batchstep_id                gme_batch_steps.batchstep_id%TYPE;
      l_last_batchstep_id           gme_batch_steps.batchstep_id%TYPE;
      l_qty                         gme_batch_steps.plan_step_qty%TYPE;
      l_mass_qty                    gme_batch_steps.plan_mass_qty%TYPE;
      l_volume_qty                  gme_batch_steps.plan_volume_qty%TYPE;
      l_plan_charges                gme_batch_steps.plan_charges%TYPE;
      l_charge                      gme_batch_steps.plan_charges%TYPE;
      l_uom_class                   mtl_units_of_measure.uom_class%TYPE;
      l_batchstep_activity_id       gme_batch_step_activities.batchstep_activity_id%TYPE;
      l_activity_factor             gme_batch_step_activities.plan_activity_factor%TYPE;
      l_resources                   gme_batch_step_resources.resources%TYPE;
      l_batchstep_ids_tab           l_batchstep_ids;
      l_total_scale_by_charge       PLS_INTEGER;
      l_count_scale_by_charge       PLS_INTEGER;
      l_return_status               VARCHAR2 (1);
      l_doc_type                    VARCHAR2 (4);
      l_qc_status                   VARCHAR2 (1);
      l_msg_stack                   VARCHAR2 (100);
      l_msg_count                   NUMBER;
      l_gmd_text_code               NUMBER;
      l_text_string                 gme_text_table.text%TYPE;
      l_api_name           CONSTANT VARCHAR2 (30)      := 'CREATE_BATCH_STEP';
      l_gme_text_code               NUMBER;
      l_recipe_id                   NUMBER;
      --Bug#5231180
      l_std_factor                  NUMBER;

      -- Bug 9694223
      l_skip                        NUMBER;
      l_count                       NUMBER;

      error_insert_batch_step       EXCEPTION;
      error_insert_b_step_act       EXCEPTION;
      error_insert_b_step_res       EXCEPTION;
      error_insert_b_res_param      EXCEPTION;
      error_insert_res_txns         EXCEPTION;
      error_insert_b_step_items     EXCEPTION;
      error_insert_b_step_depend    EXCEPTION;
      error_calc_step_qty           EXCEPTION;
      error_calc_dates              EXCEPTION;
      error_calc_charges            EXCEPTION;
      error_create_text             EXCEPTION;
      process_qty_below_cap         EXCEPTION;
      invalid_resource_qty_usage    EXCEPTION;
      error_truncate_date           EXCEPTION;
      --FPBug#4395561
      create_flex_failure           EXCEPTION;
    
      l_step_plan_start_date        DATE;
      l_step_plan_cmplt_date        DATE;
      l_rsrc_start_date             DATE;
      l_rsrc_cmplt_date             DATE;
      l_gme_batch_header            gme_batch_header%ROWTYPE;

      CURSOR cur_get_batch_steps (v_batch_id NUMBER, v_count NUMBER DEFAULT 0)
      IS
         SELECT batchstep_id, plan_step_qty, plan_charges
           FROM gme_batch_steps
          WHERE batch_id = v_batch_id
            AND (   v_count <> 1
                 OR (    v_count = 1
                     AND batchstep_id = (SELECT MAX (batchstep_id)
                                           FROM gme_batch_steps
                                          WHERE batch_id = v_batch_id) ) );

      CURSOR cur_get_activities (v_batch_id NUMBER)
      IS
         SELECT batchstep_activity_id, plan_activity_factor
           FROM gme_batch_step_activities
          WHERE batch_id = v_batch_id;

      CURSOR cur_get_step_activities (
         x_step_id    gme_batch_step_activities.batchstep_id%TYPE
        ,x_batch_id   NUMBER)
      IS
         SELECT batchstep_activity_id, plan_activity_factor
           FROM gme_batch_step_activities
          WHERE batchstep_id = x_step_id AND batch_id = x_batch_id;

      CURSOR cur_get_resources (v_batchstep_activity_id NUMBER)
      IS
         SELECT batchstep_resource_id, plan_rsrc_usage, plan_rsrc_count
           FROM gme_batch_step_resources
          WHERE batchstep_activity_id = v_batchstep_activity_id;

      CURSOR cur_recipe_id (v_recipe_validity_rule_id NUMBER)
      IS
         SELECT recipe_id
           FROM gmd_recipe_validity_rules
          WHERE recipe_validity_rule_id = v_recipe_validity_rule_id;

      CURSOR cur_step_dates (
         v_batch_id       gme_batch_header.batch_id%TYPE
        ,v_batchstep_id   gme_batch_steps.batchstep_id%TYPE)
      IS
         SELECT plan_start_date, plan_cmplt_date
           FROM gme_batch_steps
          WHERE batch_id = v_batch_id AND batchstep_id = v_batchstep_id;

      CURSOR cur_step_plan_cmplt_date (
         v_batch_id       gme_batch_header.batch_id%TYPE
        ,v_batchstep_id   gme_batch_steps.batchstep_id%TYPE)
      IS
         SELECT plan_cmplt_date
           FROM gme_batch_steps
          WHERE batch_id = v_batch_id AND batchstep_id = v_batchstep_id;

      CURSOR cur_is_charge_associated (v_batch_id NUMBER, v_batchstep_id NUMBER)
      IS
         SELECT resources
           FROM gme_batch_step_charges
          WHERE batch_id = v_batch_id
            AND batchstep_id = v_batchstep_id
            AND ROWNUM = 1;

      CURSOR cur_get_resource_dates (
         v_resources      gme_batch_step_resources.resources%TYPE
        ,v_batch_id       NUMBER
        ,v_batchstep_id   NUMBER)
      IS
         SELECT plan_start_date, plan_cmplt_date
           FROM gme_batch_step_resources
          WHERE resources = v_resources
            AND batch_id = v_batch_id
            AND batchstep_id = v_batchstep_id;

      CURSOR cur_get_batchstep_ids (v_batch_id NUMBER)
      IS
         SELECT batchstep_id
           FROM gme_batch_steps
          WHERE batch_id = v_batch_id;
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
         gme_debug.put_line ('BEGIN Create_step');
      END IF;

      x_return_status := fnd_api.g_ret_sts_success;
      l_batch_id := p_gme_batch_header_rec.batch_id;

      IF (p_gme_batch_header_rec.batch_type = gme_common_pvt.g_doc_type_batch) THEN
         l_doc_type := 'PROD';
      ELSE
         l_doc_type := 'FPO';
      END IF;

      OPEN cur_recipe_id (p_gme_batch_header_rec.recipe_validity_rule_id);

      FETCH cur_recipe_id
       INTO l_recipe_id;

      CLOSE cur_recipe_id;

      -- Note that by doing an extend to the PL/SQL table, a record is added to the collection
      -- and all elements are initialized to NULL.  Therefore, no need to initialize NULL elements
      -- explicitly through an assignment statement as that is done in the EXTEND!
      -- Create data for GME_BATCH_STEPS...
      i := p_recipe_rout_step_tbl.FIRST;
      j := 0;
      l_step_charge_rsrc_tab.DELETE ();
     
      --Bug#5112133
      IF (NVL(g_debug, -1) = gme_debug.g_log_statement) THEN
       gme_debug.put_line ('copy_routing_text_ind = '||gme_common_pvt.g_copy_routing_text_ind);
      END IF;
      WHILE i IS NOT NULL LOOP
         l_gme_batch_steps.EXTEND;

         IF (j = 0) THEN
            j := l_gme_batch_steps.FIRST;
         ELSE
            j := l_gme_batch_steps.NEXT (j);
         END IF;

         l_gme_batch_steps (j).batch_id := l_batch_id;
         l_gme_batch_steps (j).routingstep_id :=
                                     p_recipe_rout_step_tbl (i).routingstep_id;
         l_gme_batch_steps (j).batchstep_no :=
                                     p_recipe_rout_step_tbl (i).routingstep_no;
         l_gme_batch_steps (j).oprn_id := p_recipe_rout_step_tbl (i).oprn_id;
         l_gme_batch_steps (j).step_status := gme_common_pvt.g_step_pending;
         l_gme_batch_steps (j).quality_status := 1;
         l_gme_batch_steps (j).delete_mark := 0;
         l_gme_batch_steps (j).step_qty_um :=
                                    p_recipe_rout_step_tbl (i).process_qty_uom;
         l_gme_batch_steps (j).max_step_capacity :=
                                       p_recipe_rout_step_tbl (i).max_capacity;
         l_gme_batch_steps (j).max_step_capacity_um :=
                                       p_recipe_rout_step_tbl (i).capacity_uom;
         l_gme_batch_steps (j).minimum_transfer_qty :=
                               p_recipe_rout_step_tbl (i).minimum_transfer_qty;

                               
         l_gme_batch_steps (j).plan_step_qty :=
                                           p_recipe_rout_step_tbl (i).step_qty;

         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line ('DURING INITIAL CREATION step number is '|| l_gme_batch_steps (j).batchstep_no);
            gme_debug.put_line ('DURING INITIAL CREATION plan step qty from gmd is '|| l_gme_batch_steps (j).plan_step_qty);
         END IF;
                                           
         l_gme_batch_steps (j).plan_start_date := gme_common_pvt.g_timestamp;
         l_gme_batch_steps (j).plan_cmplt_date := gme_common_pvt.g_timestamp;
         l_gme_batch_steps (j).plan_charges := 0;

         IF p_gme_batch_header_rec.enforce_step_dependency = 1 THEN
            l_gme_batch_steps (j).steprelease_type := 1;
         ELSE
            l_gme_batch_steps (j).steprelease_type :=
                                  p_recipe_rout_step_tbl (i).steprelease_type;
         END IF;                              /*enforce_step_dependency = 1 */

         IF p_step_due_date IS NULL THEN
            l_gme_batch_steps (j).due_date :=
                                        l_gme_batch_steps (j).plan_cmplt_date;
         ELSE
            l_gme_batch_steps (j).due_date := p_step_due_date;
         END IF;

         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line (   ' minimum_transfer_qty '
                                || l_gme_batch_steps (j).minimum_transfer_qty);
            gme_debug.put_line (   ' max step capacity '
                                || l_gme_batch_steps (j).max_step_capacity);
            gme_debug.put_line (   ' max step capacity uom  '
                                || l_gme_batch_steps (j).max_step_capacity_um);
         END IF;

         -- Insert text if copy text is on and there is text on the step passed in...
         -- Note...  this should also work for insert step because an operation can have text
         -- and so as long as gmd passes it back in the fetch... this will work (and copy_routing_text = '1')
         IF     (p_recipe_rout_step_tbl (i).text_code IS NOT NULL)
            AND (gme_common_pvt.g_copy_routing_text_ind = 1) THEN
            l_gmd_text_code := p_recipe_rout_step_tbl (i).text_code;
            l_text_string :=
                       'gme_batch_steps' || '|' || TO_CHAR (l_batch_id)
                       || '|';
            l_text_string :=
                          l_text_string || l_gme_batch_steps (j).batchstep_no;
            --Bug#5112133
            IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
               gme_debug.put_line ('text code for step=' || l_gmd_text_code);
               gme_debug.put_line ('key_field for step=' || l_text_string);
               gme_debug.put_line ('calling copy_and_create_text for step line');
            END IF;
            copy_and_create_text (l_gmd_text_code
                                 ,l_text_string
                                 ,l_gme_text_code
                                 ,l_return_status);

            IF (l_return_status <> x_return_status) THEN
               RAISE error_create_text;
            ELSE
               l_gme_batch_steps (j).text_code := l_gme_text_code;
            END IF;
         END IF;

         -- Bug 17363801/17187401 - additional debug messages.
         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line ('calling gmd_gme_int.check_qc');
            gme_debug.put_line ('l_recipe_id is '|| l_recipe_id);
            gme_debug.put_line ('p_gme_batch_header_rec.routing_id is '|| p_gme_batch_header_rec.routing_id);
            gme_debug.put_line ('p_gme_batch_header_rec.organization_id is '|| p_gme_batch_header_rec.organization_id);
         END IF;
                                                                 
         -- Bug 17363801/17187401 - conditionalize call for overloaded function to fetch proper qc status.
         -- If routingstep is null then user is inserting a step on the fly.
         IF l_gme_batch_steps(j).routingstep_id IS NOT NULL THEN
            IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
               gme_debug.put_line ('POINT 1 l_gme_batch_steps(j).routingstep_id is '|| l_gme_batch_steps(j).routingstep_id);
            END IF;
            gmd_gme_int.check_qc(p_routingid       => p_gme_batch_header_rec.routing_id,
                                 p_routingstepid   => l_gme_batch_steps(j).routingstep_id,
                                 p_recipeid        => l_recipe_id,
                                 p_organization_id => p_gme_batch_header_rec.organization_id,
                                 p_resultout       => l_qc_status);
         ELSE
            IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
               gme_debug.put_line ('POINT 2 l_gme_batch_steps(j).oprn_id is '|| l_gme_batch_steps(j).oprn_id);
            END IF;
            gmd_gme_int.check_qc(p_operationid     => l_gme_batch_steps(j).oprn_id,
                                 p_recipeid        => l_recipe_id,
                                 p_organization_id => p_gme_batch_header_rec.organization_id,
                                 p_resultout       => l_qc_status);      
         END IF;
         
         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line ('l_qc_status is '|| l_qc_status);
         END IF;
         
         IF (l_qc_status = 'S') THEN
            l_gme_batch_steps(j).quality_status := 2;
         END IF;

         --FPBug#4395561 Start
         /* call create flex procedure to insert the default values of the BATCH_STEPS_DTL_FLEX
             DFF's segments if they are enabled */
         l_return_status := NULL;
         gme_validate_flex_fld_pvt.create_flex_batch_step (
                                       l_gme_batch_steps (j),
                                       l_gme_batch_steps (j),
                                       l_return_status);
         IF l_return_status <> FND_API.g_ret_sts_success THEN
            RAISE create_flex_failure;
         END IF;
          --FPBug#4395561 End

         IF (gme_batch_steps_dbl.insert_row
                                       (p_batch_step      => l_gme_batch_steps
                                                                           (j)
                                       ,x_batch_step      => l_gme_batch_steps
                                                                           (j) ) =
                                                                          TRUE) THEN
            -- Keep the last batchstep_id produced... this is required for single step insert... since we don't
            -- have the routingstep_id, we need this when linking the activities and resources back to the step.
            l_last_batchstep_id := l_gme_batch_steps (j).batchstep_id;
            l_step_charge_rsrc_tab (l_last_batchstep_id).resources :=
                                         p_recipe_rout_step_tbl (j).resources;

            IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
               gme_debug.put_line (   g_pkg_name
                                   || '.'
                                   || l_api_name
                                   || 'resources name obtained from GMD is'
                                   || p_recipe_rout_step_tbl (j).resources);
            END IF;
         ELSE
            RAISE error_insert_batch_step;
         END IF;

         i := p_recipe_rout_step_tbl.NEXT (i);
      END LOOP;                         /* WHILE i IS NOT NULL; BATCH STEPS */

      IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
         gme_debug.put_line ('Completed batch steps');
         gme_debug.put_line ('GME_BATCH_STEP_ACTIVITIES');
      END IF;

      i := p_recipe_rout_act_tbl.FIRST;
      j := 0;

      WHILE i IS NOT NULL LOOP
         l_gme_batch_step_activities.EXTEND;

         IF (j = 0) THEN
            j := l_gme_batch_step_activities.FIRST;
         ELSE
            j := l_gme_batch_step_activities.NEXT (j);
         END IF;

         l_gme_batch_step_activities (j).batch_id := l_batch_id;
         l_gme_batch_step_activities (j).activity :=
                                            p_recipe_rout_act_tbl (i).activity;
         l_gme_batch_step_activities (j).oprn_line_id :=
                                        p_recipe_rout_act_tbl (i).oprn_line_id;
         l_gme_batch_step_activities (j).offset_interval :=
                                     p_recipe_rout_act_tbl (i).offset_interval;
         l_gme_batch_step_activities (j).plan_activity_factor :=
                                     p_recipe_rout_act_tbl (i).activity_factor;
         l_gme_batch_step_activities (j).sequence_dependent_ind :=
                              p_recipe_rout_act_tbl (i).sequence_dependent_ind;
         l_gme_batch_step_activities (j).material_ind :=
                                        p_recipe_rout_act_tbl (i).material_ind;
         l_gme_batch_step_activities (j).delete_mark := 0;
         l_gme_batch_step_activities (j).break_ind :=
                                           p_recipe_rout_act_tbl (i).break_ind;
         l_gme_batch_step_activities (j).max_break :=
                                           p_recipe_rout_act_tbl (i).max_break;

         -- Link this activity to the batch step it belongs to...
         IF (p_recipe_rout_act_tbl (i).routingstep_id IS NOT NULL) THEN
            SELECT batchstep_id
                  ,plan_start_date
                  ,plan_cmplt_date
              INTO l_gme_batch_step_activities (j).batchstep_id
                  ,l_gme_batch_step_activities (j).plan_start_date
                  ,l_gme_batch_step_activities (j).plan_cmplt_date
              FROM gme_batch_steps
             WHERE batch_id = l_batch_id
               AND routingstep_id = p_recipe_rout_act_tbl (i).routingstep_id;
         ELSE
            SELECT batchstep_id
                  ,plan_start_date
                  ,plan_cmplt_date
              INTO l_gme_batch_step_activities (j).batchstep_id
                  ,l_gme_batch_step_activities (j).plan_start_date
                  ,l_gme_batch_step_activities (j).plan_cmplt_date
              FROM gme_batch_steps
             WHERE batch_id = l_batch_id
                   AND batchstep_id = l_last_batchstep_id;
         END IF;

         -- Insert text if copy text is on and there is text on the activity passed in...
         -- Note...  this should also work for insert step because an operation can have text
         -- and so as long as gmd passes it back in the fetch... this will work (and copy_routing_text = '1')
         IF     (p_recipe_rout_act_tbl (i).text_code IS NOT NULL)
            AND (gme_common_pvt.g_copy_routing_text_ind = 1) THEN
            l_gmd_text_code := p_recipe_rout_act_tbl (i).text_code;
            l_text_string :=
               'gme_batch_step_activities' || '|' || TO_CHAR (l_batch_id)
               || '|';
            l_text_string :=
                  l_text_string
               || l_gme_batch_step_activities (j).batchstep_id
               || '|';
            l_text_string :=
                           l_text_string || p_recipe_rout_act_tbl (i).activity;

            --Bug#5112133
            IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
	       gme_debug.put_line ('text code for activity=' || l_gmd_text_code);
               gme_debug.put_line ('key_field for activity=' || l_text_string);
               gme_debug.put_line ('calling copy_and_create_text for activities');
            END IF;

            copy_and_create_text (l_gmd_text_code
                                 ,l_text_string
                                 ,l_gme_text_code
                                 ,l_return_status);

            IF (l_return_status <> x_return_status) THEN
               RAISE error_create_text;
            ELSE
               l_gme_batch_step_activities (j).text_code := l_gme_text_code;
            END IF;
         END IF;

         --FPBug#4395561 Start
         /* call create flex procedure to insert the default values of the GME_BATCH_STEP_ACTIVITIES_FLEX
            DFF's segments if they are enabled */
         l_return_status:=NULL;
         gme_validate_flex_fld_pvt.create_flex_step_activities (
                                       l_gme_batch_step_activities (j),
                                       l_gme_batch_step_activities (j),
                                       l_return_status);
         IF l_return_status <> FND_API.g_ret_sts_success THEN
            RAISE create_flex_failure;
         END IF;
         --FPBug#4395561 End

         IF (gme_batch_step_activities_dbl.insert_row
                  (p_batch_step_activities      => l_gme_batch_step_activities
                                                                           (j)
                  ,x_batch_step_activities      => l_gme_batch_step_activities
                                                                           (j) ) ) THEN
            NULL;
         ELSE
            RAISE error_insert_b_step_act;
         END IF;

         i := p_recipe_rout_act_tbl.NEXT (i);
      END LOOP;                     /* WHILE i IS NOT NULL; STEP ACTIVITIES */

      IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
         gme_debug.put_line ('Completed GME_BATCH_STEP_ACTIVITIES');
         -- Create data for GME_BATCH_STEP_RESOURCES and GME_RESOURCE_TXNS...
         gme_debug.put_line ('GME_BATCH_STEP_RESOURCES');
      END IF;

      i := p_recipe_rout_resc_tbl.FIRST;
      j := 0;

      WHILE i IS NOT NULL LOOP
         l_gme_batch_step_resources.EXTEND;

         IF (j = 0) THEN
            j := l_gme_batch_step_resources.FIRST;
         ELSE
            j := l_gme_batch_step_resources.NEXT (j);
         END IF;

         l_gme_batch_step_resources (j).batch_id := l_batch_id;
         l_gme_batch_step_resources (j).organization_id :=
                                        p_gme_batch_header_rec.organization_id;
         l_gme_batch_step_resources (j).resources :=
                                          p_recipe_rout_resc_tbl (i).resources;
         l_gme_batch_step_resources (j).cost_analysis_code :=
                                 p_recipe_rout_resc_tbl (i).cost_analysis_code;
         l_gme_batch_step_resources (j).cost_cmpntcls_id :=
                                   p_recipe_rout_resc_tbl (i).cost_cmpntcls_id;
         l_gme_batch_step_resources (j).prim_rsrc_ind :=
                                      p_recipe_rout_resc_tbl (i).prim_rsrc_ind;
         l_gme_batch_step_resources (j).scale_type :=
                                         p_recipe_rout_resc_tbl (i).scale_type;
         l_gme_batch_step_resources (j).plan_rsrc_count :=
                                     p_recipe_rout_resc_tbl (i).resource_count;
         l_gme_batch_step_resources (j).plan_rsrc_qty :=
                                        p_recipe_rout_resc_tbl (i).process_qty;
         l_gme_batch_step_resources (j).original_rsrc_qty :=
                                        p_recipe_rout_resc_tbl (i).process_qty;
         l_gme_batch_step_resources (j).resource_qty_um :=
                                        p_recipe_rout_resc_tbl (i).process_uom;
         l_gme_batch_step_resources (j).plan_rsrc_usage :=
              p_recipe_rout_resc_tbl (i).resource_usage
            * p_recipe_rout_resc_tbl (i).resource_count;
         l_gme_batch_step_resources (j).original_rsrc_usage :=
              p_recipe_rout_resc_tbl (i).resource_usage
            * p_recipe_rout_resc_tbl (i).resource_count;
         l_gme_batch_step_resources (j).usage_um :=
                                           p_recipe_rout_resc_tbl (i).usage_um;
         l_gme_batch_step_resources (j).offset_interval :=
                                    p_recipe_rout_resc_tbl (i).offset_interval;
         l_gme_batch_step_resources (j).capacity_um :=
                                       p_recipe_rout_resc_tbl (i).capacity_uom;
         l_gme_batch_step_resources (j).min_capacity :=
                                       p_recipe_rout_resc_tbl (i).min_capacity;
         l_gme_batch_step_resources (j).max_capacity :=
                                       p_recipe_rout_resc_tbl (i).max_capacity;
         l_gme_batch_step_resources (j).capacity_tolerance :=
                                 p_recipe_rout_resc_tbl (i).capacity_tolerance;
         l_gme_batch_step_resources (j).calculate_charges :=
                                p_recipe_rout_resc_tbl (i).capacity_constraint;
         l_gme_batch_step_resources (j).process_parameter_1 :=
                                p_recipe_rout_resc_tbl (i).process_parameter_1;
         l_gme_batch_step_resources (j).process_parameter_2 :=
                                p_recipe_rout_resc_tbl (i).process_parameter_2;
         l_gme_batch_step_resources (j).process_parameter_3 :=
                                p_recipe_rout_resc_tbl (i).process_parameter_3;
         l_gme_batch_step_resources (j).process_parameter_4 :=
                                p_recipe_rout_resc_tbl (i).process_parameter_4;
         l_gme_batch_step_resources (j).process_parameter_5 :=
                                p_recipe_rout_resc_tbl (i).process_parameter_5;

         IF l_gme_batch_step_resources (j).scale_type <> 0 THEN
            IF     l_gme_batch_step_resources (j).plan_rsrc_qty = 0
               AND l_gme_batch_step_resources (j).plan_rsrc_usage <> 0 THEN
               gme_common_pvt.log_message
                                 ('gme_rsrc_qty_usage_oprn'
                                 ,'RESOURCE'
                                 ,p_recipe_rout_resc_tbl (i).resources
                                 ,'ACTIVITY'
                                 ,p_recipe_rout_resc_tbl (i).activity
                                 ,'STEP_OPRN_NO'
                                 ,    p_recipe_rout_resc_tbl (i).routingstep_no
                                   || ' - '
                                   || p_recipe_rout_resc_tbl (i).oprn_no);
               RAISE invalid_resource_qty_usage;
            END IF;
         END IF;

         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line (   ' plan_rsrc_qty '
                                || l_gme_batch_step_resources (j).plan_rsrc_qty);
         END IF;

         -- Link this resource to the batch step it belongs to...
         IF (p_recipe_rout_resc_tbl (i).routingstep_id IS NOT NULL) THEN
            SELECT batchstep_id
                  ,plan_start_date
                  ,plan_cmplt_date
              INTO l_gme_batch_step_resources (j).batchstep_id
                  ,l_gme_batch_step_resources (j).plan_start_date
                  ,l_gme_batch_step_resources (j).plan_cmplt_date
              FROM gme_batch_steps
             WHERE batch_id = l_batch_id
               AND routingstep_id = p_recipe_rout_resc_tbl (i).routingstep_id;
         ELSE
            SELECT batchstep_id
                  ,plan_start_date
                  ,plan_cmplt_date
              INTO l_gme_batch_step_resources (j).batchstep_id
                  ,l_gme_batch_step_resources (j).plan_start_date
                  ,l_gme_batch_step_resources (j).plan_cmplt_date
              FROM gme_batch_steps
             WHERE batch_id = l_batch_id
                   AND batchstep_id = l_last_batchstep_id;
         END IF;

         -- Link this resource to the activity it belongs to...
         -- Note: To go from resources upto activity, you key on batch_id, batchstep_id and
         -- oprn_line_id -> because oprn can be repeated many times in different steps so, if you have
         -- batchstep_id and then oprn_line_id, you will get a unique row from gme_batch_step_activities...
         SELECT batchstep_activity_id
           INTO l_gme_batch_step_resources (j).batchstep_activity_id
           FROM gme_batch_step_activities
          WHERE batch_id = l_batch_id
            AND batchstep_id = l_gme_batch_step_resources (j).batchstep_id
            AND oprn_line_id = p_recipe_rout_resc_tbl (i).oprn_line_id;

         -- Insert text if copy text is on and there is text on the resource passed in...
         -- Note...  this should also work for insert step because an operation can have text
         -- and so as long as gmd passes it back in the fetch... this will work (and copy_routing_text = '1')
         IF     (p_recipe_rout_resc_tbl (i).text_code IS NOT NULL)
            AND (gme_common_pvt.g_copy_routing_text_ind = '1') THEN
            l_gmd_text_code := p_recipe_rout_resc_tbl (i).text_code;
            l_text_string :=
               'gme_batch_step_resources' || '|' || TO_CHAR (l_batch_id)
               || '|';
            l_text_string :=
                  l_text_string
               || TO_CHAR (l_gme_batch_step_resources (j).batchstep_activity_id)
               || '|';
            l_text_string :=
                         l_text_string || p_recipe_rout_resc_tbl (i).resources;

           --Bug#5112133
            IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
               gme_debug.put_line ('text code for resource=' || l_gmd_text_code);
               gme_debug.put_line ('key_field for resource=' || l_text_string);
	       gme_debug.put_line ('calling copy_and_create_text for resources');
            END IF;

            copy_and_create_text (l_gmd_text_code
                                 ,l_text_string
                                 ,l_gme_text_code
                                 ,l_return_status);

            IF (l_return_status <> x_return_status) THEN
               RAISE error_create_text;
            ELSE
               l_gme_batch_step_resources (j).text_code := l_gme_text_code;
            END IF;
         END IF;
 
         --FPBug#4395561 Start
         /* call create flex procedure to insert the default values of the GME_BATCH_STEP_RESOURCES_FLEX
            DFF's segments if they are enabled */
         l_return_status:=NULL;
         gme_validate_flex_fld_pvt.create_flex_step_resources (
                                       l_gme_batch_step_resources (j),
                                       l_gme_batch_step_resources (j),
                                       l_return_status);
         IF l_return_status <> FND_API.g_ret_sts_success THEN
            RAISE create_flex_failure;
         END IF;
         --FPBug#4395561  End

         IF (gme_batch_step_resources_dbl.insert_row
                    (p_batch_step_resources      => l_gme_batch_step_resources
                                                                           (j)
                    ,x_batch_step_resources      => l_gme_batch_step_resources
                                                                           (j) ) ) THEN
            NULL;
         ELSE
            RAISE error_insert_b_step_res;
         END IF;

         i := p_recipe_rout_resc_tbl.NEXT (i);
      END LOOP;                                     /* WHILE i IS NOT NULL; */

      IF l_gme_batch_step_resources.COUNT > 0 THEN
         gme_batch_step_chg_pvt.set_sequence_dependent_id (l_batch_id);
      END IF;

      IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
         gme_debug.put_line ('Completed GME_BATCH_STEP_RESOURCES');
         gme_debug.put_line ('GME_step_process_parameters');
      END IF;

      FOR m IN 1 .. p_resc_parameters_tbl.COUNT LOOP
         l_gme_rsrc_parameters.EXTEND;
         l_gme_rsrc_parameters (m).batch_id := l_batch_id;
         l_gme_rsrc_parameters (m).resources :=
                                          p_resc_parameters_tbl (m).resources;
         l_gme_rsrc_parameters (m).parameter_id :=
                                       p_resc_parameters_tbl (m).parameter_id;
         l_gme_rsrc_parameters (m).parameter_uom :=
                                              p_resc_parameters_tbl (m).units;
         l_gme_rsrc_parameters (m).target_value :=
                                       p_resc_parameters_tbl (m).target_value;
         l_gme_rsrc_parameters (m).minimum_value :=
                                      p_resc_parameters_tbl (m).minimum_value;
         l_gme_rsrc_parameters (m).maximum_value :=
                                      p_resc_parameters_tbl (m).maximum_value;

         -- Link this process parameters to the batch step it belongs to...
         IF (p_resc_parameters_tbl (m).routingstep_id IS NOT NULL) THEN
            SELECT batchstep_id
              INTO l_gme_rsrc_parameters (m).batchstep_id
              FROM gme_batch_steps
             WHERE batch_id = l_batch_id
               AND routingstep_id = p_resc_parameters_tbl (m).routingstep_id;
         ELSE
            SELECT batchstep_id
              INTO l_gme_rsrc_parameters (m).batchstep_id
              FROM gme_batch_steps
             WHERE batch_id = l_batch_id
                   AND batchstep_id = l_last_batchstep_id;
         END IF;

         -- Link this process parameters to the activity it belongs to...
         SELECT batchstep_activity_id
           INTO l_gme_rsrc_parameters (m).batchstep_activity_id
           FROM gme_batch_step_activities
          WHERE batch_id = l_batch_id
            AND batchstep_id = l_gme_rsrc_parameters (m).batchstep_id
            AND oprn_line_id = p_resc_parameters_tbl (m).oprn_line_id;

         -- Link this process parameters to the resources it belongs to..
         SELECT batchstep_resource_id
           INTO l_gme_rsrc_parameters (m).batchstep_resource_id
           FROM gme_batch_step_resources
          WHERE batch_id = l_batch_id
            AND batchstep_id = l_gme_rsrc_parameters (m).batchstep_id
            AND batchstep_activity_id =
                               l_gme_rsrc_parameters (m).batchstep_activity_id
            AND resources = p_resc_parameters_tbl (m).resources;

         --FPBug#4395561 Start
         /* call create flex procedure to insert the default values of the GME_BATCH_PROC_PARAM_FLEX
            DFF's segments if they are enabled */
          l_return_status:=NULL;
          gme_validate_flex_fld_pvt.create_flex_process_param (
                                       l_gme_rsrc_parameters (m),
                                       l_gme_rsrc_parameters (m),
                                       l_return_status);
          IF l_return_status <> FND_API.g_ret_sts_success THEN
             RAISE create_flex_failure;
          END IF;
         --FPBug#4395561 End


         IF NOT (gme_process_parameters_dbl.insert_row
                           (p_process_parameters      => l_gme_rsrc_parameters
                                                                           (m)
                           ,x_process_parameters      => l_gme_rsrc_parameters
                                                                           (m) ) ) THEN
            RAISE error_insert_b_res_param;
         END IF;
      END LOOP;

      IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
         gme_debug.put_line ('GME_BATCH_STEP_ITEMS');
      END IF;

      -- Create data for GME_BATCH_STEP_ITEMS --> item/step assocations...
      i := p_recipe_rout_matl_tbl.FIRST;
      j := 0;
      l_skip := 0;

      WHILE i IS NOT NULL LOOP
         -- Bug 9694223 - The l_skip variable tells us if the j index was really used or not for the previous item step assoc.
         IF l_skip = 0 THEN
            l_gme_batch_step_items.EXTEND;
            
            IF (j = 0) THEN
               j := l_gme_batch_step_items.FIRST;
            ELSE
               j := l_gme_batch_step_items.NEXT (j);
            END IF;
         ELSE
            l_skip := 0;
         END IF;
                  
         l_gme_batch_step_items (j).batch_id := l_batch_id;

         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line ('batch_id is '||l_batch_id);
            gme_debug.put_line ('formulaline_id is '||p_recipe_rout_matl_tbl (i).formulaline_id);
            gme_debug.put_line ('routingstep_id is '||p_recipe_rout_matl_tbl (i).routingstep_id);
         END IF;
         
         SELECT count(*) 
           INTO l_count
           FROM gme_material_details
          WHERE batch_id = l_batch_id
            AND formulaline_id = p_recipe_rout_matl_tbl (i).formulaline_id;
               
         -- Bug 9694223 - if this record is missing it means that the user is doing a reroute
         -- to a new routing, which has a step association, for an item which has since been deleted
         -- from the batch after it was created.
         IF l_count = 0 THEN
            l_skip := 1;
         ELSE
            SELECT material_detail_id
              INTO l_gme_batch_step_items (j).material_detail_id
              FROM gme_material_details
             WHERE batch_id = l_batch_id
               AND formulaline_id = p_recipe_rout_matl_tbl (i).formulaline_id;

            IF (p_recipe_rout_matl_tbl (i).routingstep_id IS NOT NULL) THEN
               SELECT batchstep_id
                 INTO l_gme_batch_step_items (j).batchstep_id
                 FROM gme_batch_steps
                WHERE batch_id = l_batch_id
                  AND routingstep_id = p_recipe_rout_matl_tbl (i).routingstep_id;
            ELSE
               l_gme_batch_step_items (j).batchstep_id := l_last_batchstep_id;
            END IF;
            
            l_gme_batch_step_items (j).minimum_transfer_qty :=
                                  p_recipe_rout_matl_tbl (i).minimum_transfer_qty;
            l_gme_batch_step_items (j).minimum_delay :=
                                         p_recipe_rout_matl_tbl (i).minimum_delay;
            l_gme_batch_step_items (j).maximum_delay :=
                                     p_recipe_rout_matl_tbl (i).maximum_delay;

            --Rajesh Patangya DFF Enhancement 03Jan2008 Bug# 6195829
            l_gme_batch_step_items(j).ATTRIBUTE_CATEGORY:= p_recipe_rout_matl_tbl(i).ATTRIBUTE_CATEGORY;
            l_gme_batch_step_items(j).ATTRIBUTE1        := p_recipe_rout_matl_tbl(i).ATTRIBUTE1        ;
            l_gme_batch_step_items(j).ATTRIBUTE2        := p_recipe_rout_matl_tbl(i).ATTRIBUTE2        ;
            l_gme_batch_step_items(j).ATTRIBUTE3        := p_recipe_rout_matl_tbl(i).ATTRIBUTE3        ;
            l_gme_batch_step_items(j).ATTRIBUTE4        := p_recipe_rout_matl_tbl(i).ATTRIBUTE4        ;
            l_gme_batch_step_items(j).ATTRIBUTE5        := p_recipe_rout_matl_tbl(i).ATTRIBUTE5        ;
            l_gme_batch_step_items(j).ATTRIBUTE6        := p_recipe_rout_matl_tbl(i).ATTRIBUTE6        ;
            l_gme_batch_step_items(j).ATTRIBUTE7        := p_recipe_rout_matl_tbl(i).ATTRIBUTE7        ;
            l_gme_batch_step_items(j).ATTRIBUTE8        := p_recipe_rout_matl_tbl(i).ATTRIBUTE8        ;
            l_gme_batch_step_items(j).ATTRIBUTE9        := p_recipe_rout_matl_tbl(i).ATTRIBUTE9        ;
            l_gme_batch_step_items(j).ATTRIBUTE10       := p_recipe_rout_matl_tbl(i).ATTRIBUTE10       ;
            l_gme_batch_step_items(j).ATTRIBUTE11       := p_recipe_rout_matl_tbl(i).ATTRIBUTE11       ;
            l_gme_batch_step_items(j).ATTRIBUTE12       := p_recipe_rout_matl_tbl(i).ATTRIBUTE12       ;
            l_gme_batch_step_items(j).ATTRIBUTE13       := p_recipe_rout_matl_tbl(i).ATTRIBUTE13       ;
            l_gme_batch_step_items(j).ATTRIBUTE14       := p_recipe_rout_matl_tbl(i).ATTRIBUTE14       ;
            l_gme_batch_step_items(j).ATTRIBUTE15       := p_recipe_rout_matl_tbl(i).ATTRIBUTE15       ;
            l_gme_batch_step_items(j).ATTRIBUTE16       := p_recipe_rout_matl_tbl(i).ATTRIBUTE16       ;
            l_gme_batch_step_items(j).ATTRIBUTE17       := p_recipe_rout_matl_tbl(i).ATTRIBUTE17       ;
            l_gme_batch_step_items(j).ATTRIBUTE18       := p_recipe_rout_matl_tbl(i).ATTRIBUTE18       ;
            l_gme_batch_step_items(j).ATTRIBUTE19       := p_recipe_rout_matl_tbl(i).ATTRIBUTE19       ;
            l_gme_batch_step_items(j).ATTRIBUTE20       := p_recipe_rout_matl_tbl(i).ATTRIBUTE20       ;
            l_gme_batch_step_items(j).ATTRIBUTE21       := p_recipe_rout_matl_tbl(i).ATTRIBUTE21       ;
            l_gme_batch_step_items(j).ATTRIBUTE22       := p_recipe_rout_matl_tbl(i).ATTRIBUTE22       ;
            l_gme_batch_step_items(j).ATTRIBUTE23       := p_recipe_rout_matl_tbl(i).ATTRIBUTE23       ;
            l_gme_batch_step_items(j).ATTRIBUTE24       := p_recipe_rout_matl_tbl(i).ATTRIBUTE24       ;
            l_gme_batch_step_items(j).ATTRIBUTE25       := p_recipe_rout_matl_tbl(i).ATTRIBUTE25       ;
            l_gme_batch_step_items(j).ATTRIBUTE26       := p_recipe_rout_matl_tbl(i).ATTRIBUTE26       ;
            l_gme_batch_step_items(j).ATTRIBUTE27       := p_recipe_rout_matl_tbl(i).ATTRIBUTE27       ;
            l_gme_batch_step_items(j).ATTRIBUTE28       := p_recipe_rout_matl_tbl(i).ATTRIBUTE28       ;
            l_gme_batch_step_items(j).ATTRIBUTE29       := p_recipe_rout_matl_tbl(i).ATTRIBUTE29       ;
            l_gme_batch_step_items(j).ATTRIBUTE30       := p_recipe_rout_matl_tbl(i).ATTRIBUTE30       ;

     
            -- Bug: 7715293 vpedarla 
            IF     (p_recipe_rout_matl_tbl (i).text_code IS NOT NULL)
               AND (gme_common_pvt.g_copy_routing_text_ind = '1') THEN
                 
               l_gmd_text_code := p_recipe_rout_matl_tbl (i).text_code;
               l_text_string :=
                  'gme_batch_step_items' || '|' || TO_CHAR (l_batch_id)
                  || '|';
               l_text_string :=
                     l_text_string
                  || TO_CHAR (p_recipe_rout_matl_tbl(j).RECIPE_ID)
                  || '|';
               l_text_string :=
                     l_text_string
                  || TO_CHAR (p_recipe_rout_matl_tbl(j).FORMULALINE_ID)
                  || '|';
               l_text_string :=
                            l_text_string||TO_CHAR (p_recipe_rout_matl_tbl(j).ROUTINGSTEP_ID);
            
               -- Bug#5112133
               IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
                  gme_debug.put_line ('text code for step items=' || l_gmd_text_code);
                  gme_debug.put_line ('key_field for step items=' || l_text_string);
	          gme_debug.put_line ('calling copy_and_create_text for step items');
               END IF;
            
               copy_and_create_text (l_gmd_text_code
                                    ,l_text_string
                                    ,l_gme_text_code
                                    ,l_return_status);
            
               IF (l_return_status <> x_return_status) THEN
                  RAISE error_create_text;
               ELSE
                  l_gme_batch_step_items(j).text_code := l_gme_text_code;
               END IF;
            END IF;

            IF (gme_batch_step_items_dbl.insert_row
                               (p_batch_step_items      => l_gme_batch_step_items
                                                                              (j)
                               ,x_batch_step_items      => l_gme_batch_step_items
                                                                              (j) ) ) THEN
               NULL;
            ELSE
               RAISE error_insert_b_step_items;
            END IF;
         END IF; -- l_count = 0
         i := p_recipe_rout_matl_tbl.NEXT (i);
      END LOOP;              /* WHILE i IS NOT NULL ; STEP ITEM ASSOCIATIONS*/

      IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
         gme_debug.put_line ('Completed GME_BATCH_STEP_ITEMS');
         gme_debug.put_line ('GME_BATCH_STEP_DEPENDENCIES');
      END IF;

      -- Create data for GME_BATCH_STEP_DEPENDENCIES
      i := p_routing_depd_tbl.FIRST;
      j := 0;

      WHILE i IS NOT NULL LOOP
         l_gme_batch_step_dep.EXTEND;

         IF (j = 0) THEN
            j := l_gme_batch_step_dep.FIRST;
         ELSE
            j := l_gme_batch_step_dep.NEXT (j);
         END IF;

         l_gme_batch_step_dep (j).batch_id := l_batch_id;

         SELECT batchstep_id
           INTO l_gme_batch_step_dep (j).batchstep_id
           FROM gme_batch_steps
          WHERE batch_id = l_batch_id
            AND batchstep_no = p_routing_depd_tbl (i).routingstep_no;

         SELECT batchstep_id
           INTO l_gme_batch_step_dep (j).dep_step_id
           FROM gme_batch_steps
          WHERE batch_id = l_batch_id
            AND batchstep_no = p_routing_depd_tbl (i).dep_routingstep_no;

         l_gme_batch_step_dep (j).dep_type := p_routing_depd_tbl (i).dep_type;
         l_gme_batch_step_dep (j).rework_code :=
                                            p_routing_depd_tbl (i).rework_code;
         l_gme_batch_step_dep (j).standard_delay :=
                                         p_routing_depd_tbl (i).standard_delay;
         l_gme_batch_step_dep (j).min_delay :=
                                          p_routing_depd_tbl (i).minimum_delay;
         l_gme_batch_step_dep (j).max_delay :=
                                              p_routing_depd_tbl (i).max_delay;
         l_gme_batch_step_dep (j).transfer_qty :=
                                           p_routing_depd_tbl (i).transfer_qty;
         l_gme_batch_step_dep (j).transfer_um :=
                                     p_routing_depd_tbl (i).routingstep_no_uom;
         l_gme_batch_step_dep (j).transfer_percent :=
                                           p_routing_depd_tbl (i).transfer_pct;
         l_gme_batch_step_dep (j).chargeable_ind :=
                                         p_routing_depd_tbl (i).chargeable_ind;

         IF (gme_batch_step_depend_dbl.insert_row (l_gme_batch_step_dep (j)
                                                  ,l_gme_batch_step_dep (j) ) ) THEN
            NULL;
         ELSE
            RAISE error_insert_b_step_depend;
         END IF;

         i := p_routing_depd_tbl.NEXT (i);
      END LOOP;                   /* WHILE i IS NOT NULL ; STEP DEPENDENCIES*/

      IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
         gme_debug.put_line ('Completed GME_BATCH_STEP_DEPENDENCIES');
      END IF;

      -- Now calculate the step quantities...
      IF (    p_recipe_rout_step_tbl.COUNT = 1
          AND p_recipe_rout_step_tbl (1).routingstep_id IS NULL) THEN
         -- This is an insert... the plan_step_qty is filled in above... does actual_step_qty get filled in if this is WIP?
         -- ALSO, don't need to call auto_step_calc, because there are no dependencies tied in with this step... and there
         -- are no item_step_associations either... so do nothing...
         NULL;
      ELSE
         IF (p_gme_batch_header_rec.automatic_step_calculation = 1) THEN
            IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
               gme_debug.put_line ('auto step calc for batch id '||l_batch_id);
               gme_debug.put_line ('CALLING gmd_auto_step_calc.calc_step_qty. Org is '||p_gme_batch_header_rec.organization_id);
            END IF;

            gmd_auto_step_calc.calc_step_qty
                  (p_parent_id              => l_batch_id
                  ,p_step_tbl               => l_step_tbl
                  ,p_msg_count              => l_msg_count
                  ,p_msg_stack              => l_msg_stack
                  ,p_return_status          => l_return_status
                  ,p_called_from_batch      => 1
                  ,p_organization_id        => p_gme_batch_header_rec.organization_id);
         ELSE
            IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
               gme_debug.put_line ('NO auto step calc...');
               gme_debug.put_line ('CALLING calc_step_qty ...');
            END IF;

            calc_step_qty (l_batch_id, l_step_tbl, l_return_status, 1);
         END IF;

         IF l_return_status <> x_return_status THEN
            IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
               gme_debug.put_line
                              (fnd_msg_pub.get (p_encoded      => fnd_api.g_false) );
            END IF;

            RAISE error_calc_step_qty;
         END IF;

         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line ('done step calc...');
            gme_debug.put_line ('GO INTO LOOP TO RESET PLAN STEP QTY... ');
         END IF;

         -- Update the plan_step_qty in GME_BATCH_STEPS
         FOR i IN l_step_tbl.FIRST .. l_step_tbl.LAST LOOP

            IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
               gme_debug.put_line ('step number is '|| l_step_tbl (i).step_no);
               gme_debug.put_line ('PLAN STEP QTY FROM GMD IS '|| l_step_tbl (i).step_qty);
               gme_debug.put_line ('PLAN STEP QTY RNDED FROM GMD IS '|| ROUND (l_step_tbl (i).step_qty, 32));
            END IF;
         
            -- Round the step_qty, step_mass_qty, step_vol_qty to 32 decimal places.
            UPDATE gme_batch_steps
               SET plan_step_qty = ROUND (l_step_tbl (i).step_qty, 32)
                  ,plan_mass_qty = ROUND (l_step_tbl (i).step_mass_qty, 32)
                  ,mass_ref_um = l_step_tbl (i).step_mass_uom
                  ,plan_volume_qty = ROUND (l_step_tbl (i).step_vol_qty, 32)
                  ,volume_ref_um = l_step_tbl (i).step_vol_uom
             WHERE batch_id = l_batch_id
               AND batchstep_no = l_step_tbl (i).step_no;
         END LOOP;          /* FOR i IN l_step_tbl.FIRST .. l_step_tbl.LAST */
      END IF;                        /* IF p_recipe_rout_step_tbl.COUNT = 1 */

      IF (    p_recipe_rout_step_tbl.COUNT = 1
          AND p_recipe_rout_step_tbl (1).routingstep_id IS NULL) THEN
         -- This is an insert... calc charges for the newly inserted step
	 /* Bug#5231180 Begin modified the following code to calculate mass qty and volume qty depends on the
	    step qty uom used */
	 --get the uom class and conversion rate in step qty uom
         SELECT uom_class, conversion_rate
           INTO l_uom_class, l_std_factor
           FROM mtl_uom_conversions
          WHERE uom_code = p_recipe_rout_step_tbl (1).process_qty_uom 
	    AND inventory_item_id = 0;

         l_mass_qty := NULL;
         l_volume_qty := NULL;

        --Bug#5231180 used gme_common_pvt variables rather GMD spec variables
	 --IF l_uom_class = NVL(gmd_auto_step_calc.g_profile_mass_um_type,gme_common_pvt.g_mass_um_type) THEN
	 IF l_uom_class = gme_common_pvt.g_mass_um_type THEN
	    --multiply the step qty with standard factor to get mass qty
            l_mass_qty := p_recipe_rout_step_tbl (1).step_qty * l_std_factor;        
         --ELSIF l_uom_class = NVL(gmd_auto_step_calc.g_profile_volume_um_type,gme_common_pvt.g_volume_um_type) THEN
	 ELSIF l_uom_class = gme_common_pvt.g_volume_um_type THEN
 	    --multiply the step qty with standard factor to get vol qty
            l_volume_qty := p_recipe_rout_step_tbl (1).step_qty * l_std_factor;
         END IF;
	 
         IF NVL (g_debug, -1) = gme_debug.g_log_statement THEN
            gme_debug.put_line
               (   g_pkg_name
                || '.'
                || l_api_name
                || 'resources name passed in case of single step insertion is'
                || p_recipe_rout_step_tbl (1).resources);
            gme_debug.put_line
               (   g_pkg_name
                || '.'
                || l_api_name
		|| ' Mass Qty, Volume Qty: '
		|| l_mass_qty ||'    '||l_volume_qty );
         END IF;
         --Bug#5231180 End
         gme_update_step_qty_pvt.calc_charge
                          (p_step_id            => l_last_batchstep_id
                          ,p_resources          => p_recipe_rout_step_tbl (1).resources
                          ,p_mass_qty           => l_mass_qty
                          ,p_vol_qty            => l_volume_qty
                          ,x_charge             => l_charge
                          ,x_return_status      => l_return_status);

         IF l_return_status <> x_return_status THEN
            RAISE error_calc_charges;
         END IF;

         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line (   ' return status from calc charge '
                                || l_return_status);
            gme_debug.put_line (' charges  ' || TO_CHAR (l_charge) );
         END IF;

         UPDATE gme_batch_steps
            SET plan_charges = l_charge
          WHERE batchstep_id = l_last_batchstep_id;
      ELSE
         update_charges (p_batch_id                  => l_batch_id
                        ,p_step_charge_rsrc_tab      => l_step_charge_rsrc_tab
                        ,x_return_status             => l_return_status);

         IF l_return_status <> x_return_status THEN
            RAISE error_calc_charges;
         END IF;
      END IF;

      OPEN cur_get_batch_steps (l_batch_id, p_recipe_rout_step_tbl.COUNT);

      FETCH cur_get_batch_steps
       INTO l_batchstep_id, l_qty, l_plan_charges;

      WHILE cur_get_batch_steps%FOUND LOOP
         UPDATE gme_batch_step_resources
            SET plan_rsrc_usage =
                        ROUND ( (l_qty / plan_rsrc_qty * plan_rsrc_usage), 32)
          WHERE batchstep_id = l_batchstep_id
            AND scale_type = 1
            AND                      -- scale_type = 1 denotes linear scale...
                plan_rsrc_qty <> 0;

         FETCH cur_get_batch_steps
          INTO l_batchstep_id, l_qty, l_plan_charges;
      END LOOP;                          /* WHILE cur_get_batch_steps%FOUND */

      CLOSE cur_get_batch_steps;

      -- The following variable will accumulate the # of resources that are scale by charge and associated to a step with
      -- a NULL charge.  as long as this variable is > 0, a message will be put to the API stack saying that
      -- at least 1 resource was not scaled by charge because the charge is undefined.
      -- Look at calc_charge for ways of getting a NULL charge...
      l_total_scale_by_charge := 0;

      IF (    p_recipe_rout_step_tbl.COUNT = 1
          AND p_recipe_rout_step_tbl (1).routingstep_id IS NULL) THEN
         -- This is an insert step... deal with only that new step...
         SELECT plan_step_qty, plan_charges
           INTO l_qty, l_plan_charges
           FROM gme_batch_steps
          WHERE batchstep_id = l_last_batchstep_id;

         -- Round the plan_rsrc_qty to 32 decimal places.
         UPDATE gme_batch_step_resources
            SET plan_rsrc_qty = ROUND (l_qty, 32)
          WHERE batchstep_id = l_last_batchstep_id
            AND scale_type <>
                   0
-- if scale_type = 0, then let's leave the plan_rsrc_qty to that passed in by GMD... scale_type = 0 is fixed
            AND plan_rsrc_qty <> 0;

         -- Update the resource quantities for all resources and usage if scale by charge...
         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line (' plan charges ' || TO_CHAR (l_plan_charges) );
         END IF;

         IF l_plan_charges IS NOT NULL THEN
            UPDATE gme_batch_step_resources
               SET plan_rsrc_usage =
                                  ROUND (plan_rsrc_usage * l_plan_charges, 32)
             WHERE batchstep_id = l_last_batchstep_id AND scale_type = 2;
                                  -- scale_type = 2 denotes scale by charge...
         ELSE
            -- if charges is NULL and there exists resources with are scale by charge, these resources are not touched, i.e.
            -- in effect, charge is defaulted to 1... however, let's give user a warning that this has occurred.
            -- Because of the limitation in forms taking only 1 message at a time (?), we will put one message if this occurs,
            -- NOT one message per occurrence.
            SELECT COUNT (1)
              INTO l_count_scale_by_charge
              FROM gme_batch_step_resources
             WHERE batchstep_id = l_last_batchstep_id AND scale_type = 2;
                                  -- scale_type = 2 denotes scale by charge...
         END IF;

         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line (   ' total scale by charge  '
                                || TO_CHAR (l_total_scale_by_charge) );
         END IF;

         l_total_scale_by_charge := l_count_scale_by_charge;

         -- Update the Usage on resources based on the activity factor for only that step...
         OPEN cur_get_step_activities (l_last_batchstep_id, l_batch_id);

         FETCH cur_get_step_activities
          INTO l_batchstep_activity_id, l_activity_factor;

         WHILE cur_get_step_activities%FOUND LOOP
            UPDATE gme_batch_step_resources
               SET plan_rsrc_usage =
                               ROUND (plan_rsrc_usage * l_activity_factor, 32)
                  ,plan_rsrc_qty =
                                 ROUND (plan_rsrc_qty * l_activity_factor, 32)
             WHERE batchstep_activity_id = l_batchstep_activity_id;

            IF p_gme_batch_header_rec.update_inventory_ind = 'Y' THEN
               FOR l_rec IN cur_get_resources (l_batchstep_activity_id) LOOP
                  UPDATE gme_resource_txns
                     SET resource_usage =
                            ROUND (  l_rec.plan_rsrc_usage
                                   / l_rec.plan_rsrc_count
                                  ,32)
                   WHERE doc_id = l_batch_id
                     AND doc_type = l_doc_type
                     AND line_id = l_rec.batchstep_resource_id;
               END LOOP;                                   /* FOR resources */
            END IF;
               /* IF p_gme_batch_header_rec.update_inventory_ind = 'Y' THEN */

            FETCH cur_get_step_activities
             INTO l_batchstep_activity_id, l_activity_factor;
         END LOOP;                   /* WHILE cur_get_step_activities%FOUND */

         CLOSE cur_get_step_activities;
      ELSE
         -- Update the resource quantities for all resources and usage if scale by charge...
         OPEN cur_get_batch_steps (l_batch_id);

         FETCH cur_get_batch_steps
          INTO l_batchstep_id, l_qty, l_plan_charges;

         WHILE cur_get_batch_steps%FOUND LOOP
            UPDATE gme_batch_step_resources
               SET plan_rsrc_qty = ROUND (l_qty, 32)
             WHERE batchstep_id = l_batchstep_id
               AND scale_type <> 0
               AND plan_rsrc_qty <> 0;

            -- if scale_type = 0, then let's leave the plan_rsrc_qty to that passed in by GMD... scale_type = 0 is fixed
            IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
               gme_debug.put_line (   ' plan charges batch steps   '
                                   || TO_CHAR (l_plan_charges) );
            END IF;

            IF l_plan_charges IS NOT NULL THEN
               UPDATE gme_batch_step_resources
                  SET plan_rsrc_usage =
                                  ROUND (plan_rsrc_usage * l_plan_charges, 32)
                WHERE batchstep_id = l_batchstep_id AND scale_type = 2;
                                  -- scale_type = 2 denotes scale by charge...
            ELSE
               -- if charges is NULL and there exists resources with are scale by charge, these resources are not touched, i.e.
               -- in effect, charge is defaulted to 1... however, let's give user a warning that this has occurred.
               -- Because of the limitation in forms taking only 1 message at a time (?), we will put one message if this occurs,
               -- NOT one message per occurrence.
               SELECT COUNT (1)
                 INTO l_count_scale_by_charge
                 FROM gme_batch_step_resources
                WHERE batchstep_id = l_batchstep_id AND scale_type = 2;
                                  -- scale_type = 2 denotes scale by charge...
            END IF;

            l_total_scale_by_charge :=
                             l_total_scale_by_charge + l_count_scale_by_charge;

            IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
               gme_debug.put_line (   ' l_total scale by charge loop  '
                                   || TO_CHAR (l_total_scale_by_charge) );
            END IF;

            FETCH cur_get_batch_steps
             INTO l_batchstep_id, l_qty, l_plan_charges;
         END LOOP;                       /* WHILE cur_get_batch_steps%FOUND */

         CLOSE cur_get_batch_steps;

         -- Update the Usage on resources based on the activity factor...
         OPEN cur_get_activities (l_batch_id);

         FETCH cur_get_activities
          INTO l_batchstep_activity_id, l_activity_factor;

         WHILE cur_get_activities%FOUND LOOP
            UPDATE gme_batch_step_resources
               SET plan_rsrc_usage =
                               ROUND (plan_rsrc_usage * l_activity_factor, 32)
                  ,plan_rsrc_qty =
                                 ROUND (plan_rsrc_qty * l_activity_factor, 32)
             WHERE batchstep_activity_id = l_batchstep_activity_id;

            IF p_gme_batch_header_rec.update_inventory_ind = 'Y' THEN
               FOR l_rec IN cur_get_resources (l_batchstep_activity_id) LOOP
                  UPDATE gme_resource_txns
                     SET resource_usage =
                            ROUND (  l_rec.plan_rsrc_usage
                                   / l_rec.plan_rsrc_count
                                  ,32)
                   WHERE doc_id = l_batch_id
                     AND doc_type = l_doc_type
                     AND line_id = l_rec.batchstep_resource_id;
               END LOOP;                                   /* FOR resources */
            END IF;
               /* IF p_gme_batch_header_rec.update_inventory_ind = 'Y' THEN */

            FETCH cur_get_activities
             INTO l_batchstep_activity_id, l_activity_factor;
         END LOOP;                        /* WHILE cur_get_activities%FOUND */

         CLOSE cur_get_activities;
      END IF;

      /* Lets check if any of the resource quantities are falling below the min capacity */
      IF (p_ignore_qty_below_cap = fnd_api.g_false) THEN
         IF gme_common_pvt.resource_qty_below_capacity
                                                    (p_batch_id      => l_batch_id) THEN
            RAISE process_qty_below_cap;
         END IF;
      END IF;                            /* IF NOT (p_ignore_qty_below_cap) */

      IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
         gme_debug.put_line (   'l_total_scale_by_charge='
                             || l_total_scale_by_charge);
      END IF;

      IF l_total_scale_by_charge > 0 THEN
         -- we are not raising an error, just warn the user.
         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line ('undefined charge warning');
         END IF;

         fnd_message.set_name ('GME', 'GME_UNDEF_CHRG_RSRC_SCALE');
         fnd_msg_pub.ADD;
      END IF;

      IF (    p_recipe_rout_step_tbl.COUNT = 1
          AND p_recipe_rout_step_tbl (1).routingstep_id IS NULL) THEN
         gme_create_step_pvt.calc_dates
                           (p_gme_batch_header_rec      => p_gme_batch_header_rec
                           ,p_use_workday_cal           => p_use_workday_cal
                           ,p_contiguity_override       => p_contiguity_override
                           ,p_return_status             => l_return_status
                           ,p_step_id                   => l_last_batchstep_id
                           ,p_plan_start_date           => p_step_start_date
                           ,p_plan_cmplt_date           => p_step_cmplt_date);

         IF l_return_status <> x_return_status THEN
            RAISE error_calc_dates;
         END IF;
      ELSE
         gme_create_step_pvt.calc_dates
                           (p_gme_batch_header_rec      => p_gme_batch_header_rec
                           ,p_use_workday_cal           => p_use_workday_cal
                           ,p_contiguity_override       => p_contiguity_override
                           ,p_return_status             => l_return_status);
      END IF;

      IF l_return_status <> x_return_status THEN
         RAISE error_calc_dates;
      END IF;

      IF (    p_recipe_rout_step_tbl.COUNT = 1
          AND p_recipe_rout_step_tbl (1).routingstep_id IS NULL) THEN
         OPEN cur_step_dates (p_gme_batch_header_rec.batch_id
                             ,l_last_batchstep_id);

         FETCH cur_step_dates
          INTO l_step_plan_start_date, l_step_plan_cmplt_date;

         CLOSE cur_step_dates;

         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line (   l_api_name
                                || 'start,cmplt dates for step are '
                                || TO_CHAR (l_step_plan_start_date
                                           ,'DD-MON-YYYY HH24:MI:SS')
                                || ' '
                                || TO_CHAR (l_step_plan_cmplt_date
                                           ,'DD-MON-YYYY HH24:MI:SS') );
            gme_debug.put_line
                           (   'user supplied start,cmplt dates for step are'
                            || TO_CHAR (p_step_start_date
                                       ,'DD-MON-YYYY HH24:MI:SS')
                            || ' '
                            || TO_CHAR (p_step_cmplt_date
                                       ,'DD-MON-YYYY HH24:MI:SS') );
            gme_debug.put_line
                          (   'start,cmplt dates for batch are'
                           || TO_CHAR (p_gme_batch_header_rec.plan_start_date
                                      ,'DD-MON-YYYY HH24:MI:SS')
                           || TO_CHAR (p_gme_batch_header_rec.plan_cmplt_date
                                      ,'DD-MON-YYYY HH24:MI:SS') );
         END IF;

         IF     (p_step_cmplt_date IS NOT NULL)
            AND (p_step_cmplt_date <> l_step_plan_cmplt_date) THEN
            IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
               gme_debug.put_line
                        ('p_step_cmplt_date diff from l_step_plan_cmplt_date');
            END IF;

            IF (p_step_cmplt_date > l_step_plan_cmplt_date) THEN
               UPDATE gme_batch_steps
                  SET plan_cmplt_date = p_step_cmplt_date
                     ,last_updated_by = gme_common_pvt.g_user_ident
                     ,last_update_date = gme_common_pvt.g_timestamp
                     ,last_update_login = gme_common_pvt.g_login_id
                WHERE batch_id = p_gme_batch_header_rec.batch_id
                  AND batchstep_id = l_last_batchstep_id;
            ELSIF (p_step_cmplt_date < l_step_plan_cmplt_date) THEN
               l_gme_batch_header.batch_id := p_gme_batch_header_rec.batch_id;
               l_gme_batch_header.batch_type :=
                                            p_gme_batch_header_rec.batch_type;
               l_gme_batch_header.plan_cmplt_date := p_step_cmplt_date;
               gme_reschedule_batch_pvt.truncate_date
                                   (p_batch_header_rec      => l_gme_batch_header
                                   ,p_date                  => 1
                                   ,p_batchstep_id          => l_last_batchstep_id
                                   ,x_return_status         => l_return_status);

               IF l_return_status <> x_return_status THEN
                  RAISE error_truncate_date;
               END IF;
            END IF;      /* IF (p_step_cmplt_date > l_step_plan_cmplt_date) */

            l_step_plan_cmplt_date := p_step_cmplt_date;
         END IF;                           /* p_step_cmplt_date is not null */

         --picking the step values again
         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line
               (   'step start,cmplt dates for step after comparision with user step dates are'
                || TO_CHAR (l_step_plan_start_date, 'DD-MON-YYYY HH24:MI:SS')
                || '  '
                || TO_CHAR (l_step_plan_cmplt_date, 'DD-MON-YYYY HH24:MI:SS') );
         END IF;

         --checking against the batch dates
         IF (l_step_plan_start_date < p_gme_batch_header_rec.plan_start_date) THEN
            IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
               gme_debug.put_line
                         ('l_step_plan_start_date diff from batch start date');
            END IF;

            gme_reschedule_batch_pvt.truncate_date
                                (p_batch_header_rec      => p_gme_batch_header_rec
                                ,p_date                  => 0
                                ,p_batchstep_id          => l_last_batchstep_id
                                ,x_return_status         => l_return_status);

            IF l_return_status <> x_return_status THEN
               RAISE error_truncate_date;
            END IF;
         END IF;
       /* (l_step_plan_start_date < p_gme_batch_header_rec.plan_start_date) */

         IF (l_step_plan_cmplt_date > p_gme_batch_header_rec.plan_cmplt_date) THEN
            IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
               gme_debug.put_line
                         ('l_step_plan_cmplt_date diff from batch cmplt date');
            END IF;

            gme_reschedule_batch_pvt.truncate_date
                                (p_batch_header_rec      => p_gme_batch_header_rec
                                ,p_date                  => 1
                                ,p_batchstep_id          => l_last_batchstep_id
                                ,x_return_status         => l_return_status);

            IF l_return_status <> x_return_status THEN
               RAISE error_truncate_date;
            END IF;
         END IF;
       /* (l_step_plan_cmplt_date > p_gme_batch_header_rec.plan_cmplt_date) */
      END IF;                   /* IF (p_recipe_rout_step_tbl.COUNT = 1 AND */

      OPEN cur_get_batchstep_ids (p_gme_batch_header_rec.batch_id);

      FETCH cur_get_batchstep_ids
      BULK COLLECT INTO l_batchstep_ids_tab;

      CLOSE cur_get_batchstep_ids;

      FOR i IN 1 .. l_batchstep_ids_tab.COUNT LOOP
         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line (   'batch,step ids are'
                                || p_gme_batch_header_rec.batch_id
                                || l_batchstep_ids_tab (i) );
         END IF;

         OPEN cur_is_charge_associated (p_gme_batch_header_rec.batch_id
                                       ,l_batchstep_ids_tab (i) );

         FETCH cur_is_charge_associated
          INTO l_resources;

         IF cur_is_charge_associated%FOUND THEN
            CLOSE cur_is_charge_associated;

            OPEN cur_get_resource_dates (l_resources
                                        ,p_gme_batch_header_rec.batch_id
                                        ,l_batchstep_ids_tab (i) );

            FETCH cur_get_resource_dates
             INTO l_rsrc_start_date, l_rsrc_cmplt_date;

            IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
               gme_debug.put_line (   g_pkg_name
                                   || '.'
                                   || l_api_name
                                   || 'rsrc start,cmplt dates are'
                                   || l_rsrc_start_date
                                   || l_rsrc_cmplt_date);
            END IF;

            CLOSE cur_get_resource_dates;

            UPDATE gme_batch_step_charges
               SET plan_start_date = l_rsrc_start_date
                  ,plan_cmplt_date = l_rsrc_cmplt_date
             WHERE batch_id = p_gme_batch_header_rec.batch_id
               AND batchstep_id = l_batchstep_ids_tab (i);
         ELSE
            CLOSE cur_is_charge_associated;
         END IF;
      END LOOP;

      IF (    p_recipe_rout_step_tbl.COUNT = 1
          AND p_recipe_rout_step_tbl (1).routingstep_id IS NULL) THEN
         wf_event.RAISE (p_event_name      => gme_common_pvt.G_BATCHSTEP_CREATED
                        ,p_event_key       => l_gme_batch_steps (1).batchstep_id);
      END IF;

      IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
         gme_debug.put_line
                       (   'Done with Create Batch steps with return code = '
                        || x_return_status);
      END IF;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;
   EXCEPTION
      WHEN error_insert_batch_step THEN
         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line ('insert batch step error');
         END IF;

         x_return_status := fnd_api.g_ret_sts_error;
      WHEN error_insert_b_step_act THEN
         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line ('insert batch step activity error');
         END IF;

         x_return_status := fnd_api.g_ret_sts_error;
      WHEN error_insert_b_step_res THEN
         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line ('insert batch step resource error');
         END IF;

         x_return_status := fnd_api.g_ret_sts_error;
      WHEN error_insert_res_txns THEN
         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line ('insert resource txns error');
         END IF;

         x_return_status := fnd_api.g_ret_sts_error;
      WHEN error_insert_b_res_param THEN
         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line ('insert resource param error');
         END IF;

         x_return_status := fnd_api.g_ret_sts_error;
      WHEN error_insert_b_step_items THEN
         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line ('insert batch step items error');
         END IF;

         x_return_status := fnd_api.g_ret_sts_error;
      WHEN error_insert_b_step_depend THEN
         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line ('insert batch step depend error');
         END IF;

         x_return_status := fnd_api.g_ret_sts_error;
      WHEN error_calc_step_qty THEN
         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line ('calc step qty error');
         END IF;

         x_return_status := fnd_api.g_ret_sts_error;
      WHEN error_calc_dates THEN
         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line ('calc dates error');
         END IF;

         x_return_status := l_return_status;
      WHEN error_calc_charges THEN
         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line ('calc charge error');
         END IF;

         x_return_status := fnd_api.g_ret_sts_error;
      WHEN error_create_text THEN
         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line ('create text error');
         END IF;

         x_return_status := l_return_status;
      WHEN process_qty_below_cap THEN
         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line ('process qty below capacity');
         END IF;

         x_return_status := fnd_api.g_ret_sts_error;
      WHEN invalid_resource_qty_usage THEN
         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line
                              ('invalild resource process quantity and usage');
         END IF;
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN error_truncate_date THEN
         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line ('truncate date procedure error');
         END IF;

         x_return_status := fnd_api.g_ret_sts_error;     
      --FPBug#4395561 
      WHEN create_flex_failure THEN
         x_return_status := l_return_status;
       IF (NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT) THEN
            gme_debug.put_line ('Creating the default values of the DFF failure');
         END IF;
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
   END create_batch_steps;

   PROCEDURE update_charges (
      p_batch_id               IN              NUMBER
     ,p_step_charge_rsrc_tab   IN              gme_create_step_pvt.step_charge_rsrc_tab
     ,x_return_status          OUT NOCOPY      VARCHAR2)
   IS
      x_charge_tab          charge_tab;
      x_num_steps           PLS_INTEGER;
      l_return_status       VARCHAR2 (1);
      x_charge              gme_batch_steps.plan_charges%TYPE;
      l_api_name   CONSTANT VARCHAR2 (30)                 := 'update charges';

      -- Cursor Definitions
      CURSOR cur_get_steps (v_batch_id NUMBER)
      IS
         SELECT batchstep_id, plan_mass_qty, mass_ref_um, plan_volume_qty
               ,volume_ref_um
           FROM gme_batch_steps
          WHERE batch_id = v_batch_id;

      x_cur_step_rec        cur_get_steps%ROWTYPE;
      error_calc_charge     EXCEPTION;
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      x_return_status := fnd_api.g_ret_sts_success;
      x_num_steps := 0;

      OPEN cur_get_steps (p_batch_id);

      FETCH cur_get_steps
       INTO x_cur_step_rec;

      WHILE cur_get_steps%FOUND LOOP
         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line
               (   g_pkg_name
                || '.'
                || l_api_name
                || 'p_step_charge_rsrc_tab(batchstep_id).resources is '
                || p_step_charge_rsrc_tab (x_cur_step_rec.batchstep_id).resources);
         END IF;

         gme_update_step_qty_pvt.calc_charge
            (p_step_id            => x_cur_step_rec.batchstep_id
            ,p_resources          => p_step_charge_rsrc_tab
                                                  (x_cur_step_rec.batchstep_id).resources
            ,p_mass_qty           => x_cur_step_rec.plan_mass_qty
            ,p_vol_qty            => x_cur_step_rec.plan_volume_qty
            ,x_charge             => x_charge
            ,x_return_status      => l_return_status);

         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line
                        (   ' update charges >calc charges > return charge '
                         || TO_CHAR (x_charge) );
            gme_debug.put_line
                          (   ' update charges >calc charges >batchstep_id  '
                           || TO_CHAR (x_cur_step_rec.batchstep_id) );
         END IF;

         IF l_return_status <> fnd_api.g_ret_sts_success THEN
            RAISE error_calc_charge;
         END IF;

         x_num_steps := x_num_steps + 1;
         x_charge_tab (x_num_steps).step_id := x_cur_step_rec.batchstep_id;
         x_charge_tab (x_num_steps).charge := x_charge;

         FETCH cur_get_steps
          INTO x_cur_step_rec;
      END LOOP;

      FOR i IN 1 .. x_charge_tab.COUNT LOOP
         UPDATE gme_batch_steps
            SET plan_charges = x_charge_tab (i).charge
          WHERE batchstep_id = x_charge_tab (i).step_id;
      END LOOP;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;
   EXCEPTION
      WHEN error_calc_charge THEN
         x_return_status := l_return_status;
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
   END update_charges;

   /*======================================================================
   --  FUNCTION :
   --   get_max_step_date
   --
   --  DESCRIPTION:
   --    This PL/SQL function  is responsible for calculating max step
   --    dates based on shop calendar or otherwise
   --
   --  REQUIREMENTS
   --    p_gme_batch_header_rec non null value.
   --  SYNOPSIS:
   --
   --===================================================================== */
   FUNCTION get_max_step_date (
      p_use_workday_cal    IN   VARCHAR2
     ,p_calendar_code      IN   VARCHAR2
     ,p_batchstep_id       IN   NUMBER
     ,p_batch_id           IN   NUMBER
     ,p_batch_start_date   IN   DATE)
      RETURN DATE
   IS
      CURSOR cur_get_step_date_4_cal (v_step_id NUMBER, v_batch_id NUMBER)
      IS
         SELECT dep_type, r.plan_start_date, r.plan_cmplt_date
               ,standard_delay
           FROM gme_batch_step_dependencies d, gme_batch_steps r
          WHERE d.batch_id = r.batch_id
            AND d.batch_id = v_batch_id
            AND r.batchstep_id = d.dep_step_id
            AND d.batchstep_id = v_step_id;

      CURSOR cur_get_step_date (v_step_id NUMBER, v_batch_id NUMBER)
      IS
         SELECT MAX (DECODE (dep_type
                            ,1, r.plan_start_date + standard_delay / 24
                            ,0, r.plan_cmplt_date + standard_delay / 24) )
           FROM gme_batch_step_dependencies d, gme_batch_steps r
          WHERE d.batch_id = r.batch_id
            AND d.batch_id = v_batch_id
            AND r.batchstep_id = d.dep_step_id
            AND d.batchstep_id = v_step_id;

      l_date                DATE;
      l_max_date            DATE;
      l_plan_start_date     DATE;
      l_api_name   CONSTANT VARCHAR2 (30) := 'get_max_step_date';
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      l_max_date := NULL;

      IF p_use_workday_cal = fnd_api.g_true THEN
         FOR rec IN cur_get_step_date_4_cal (p_batchstep_id, p_batch_id) LOOP
            IF rec.dep_type = 1 THEN
               l_date := rec.plan_start_date;
            ELSE
               l_date := rec.plan_cmplt_date;
            END IF;

            l_plan_start_date :=
               get_working_start_time (p_start_date         => l_date
                                      ,p_offset             => rec.standard_delay
                                      ,p_calendar_code      => p_calendar_code);

            IF l_plan_start_date IS NULL THEN
               RETURN NULL;
            END IF;

            IF l_max_date IS NULL OR l_max_date < l_plan_start_date THEN
               l_max_date := l_plan_start_date;
            END IF;
         END LOOP;
      ELSE                            /* p_use_workday_cal = FND_API.G_TRUE */
         OPEN cur_get_step_date (p_batchstep_id, p_batch_id);

         FETCH cur_get_step_date
          INTO l_date;

         IF (cur_get_step_date%FOUND) AND (l_date IS NOT NULL) THEN
            IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
               gme_debug.put_line (   'GET_MAX_STEP_DATE: from cursor'
                                   || TO_CHAR (l_date
                                              ,'DD-MON-YYYY HH24:MI:SS') );
            END IF;

            l_max_date := l_date;
         ELSE
            IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
               gme_debug.put_line ('No prior step here');
            END IF;
         END IF;

         CLOSE cur_get_step_date;
      END IF;                         /* p_use_workday_cal = FND_API.G_TRUE */

      IF l_max_date IS NULL THEN
         l_max_date := p_batch_start_date;
      END IF;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;

      RETURN l_max_date;
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
         RETURN NULL;
   END get_max_step_date;

   /*======================================================================
   --  FUNCTION :
   --   get_working_start_time
   --
   --  DESCRIPTION:
   --    This PL/SQL function gets the date and checks to see
   --    if the date + offset is a valid working datetime If
   --    it is not working datetime then function finds next
   --    working datetime and returns it.
   --    In case of some error it returns NULL.
   --  REQUIREMENTS
   --    p_start_date    DATE
   --    p_offset        NUMBER  in hours
   --    p_calendar_code        VARCHAR2  Calendar CODE
   --  SYNOPSIS:
   --
   --
   --
   --===================================================================== */
   FUNCTION get_working_start_time (
      p_start_date      IN   DATE
     ,p_offset          IN   NUMBER
     ,p_calendar_code   IN   VARCHAR2)
      RETURN DATE
   IS
      l_contig_period_tbl   gmp_calendar_api.contig_period_tbl;
      l_diff                NUMBER;
      l_start_date          DATE;
      l_cal_count           NUMBER;
      l_return_status       VARCHAR2 (1);
      l_api_name   CONSTANT VARCHAR2 (30)               := 'get_working_time';
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      IF p_start_date IS NULL THEN
         RETURN NULL;
      END IF;

      IF p_offset >= 0 THEN
         IF NVL (g_debug, -1) = gme_debug.g_log_statement THEN
            gme_debug.put_line ('calling get_contiguous_periods from point 1 get_working_time'); -- Bug 13582990
            gme_debug.put_line ('start date passed in is '||TO_CHAR(p_start_date, 'DD-MON-YYYY HH24:MI:SS')); -- Bug 13582990
            gme_debug.put_line ('duration passed in is '||p_offset); -- Bug 13582990
            gme_debug.put_line ('p_calendar_code passed in is '||p_calendar_code); -- Bug 13582990
         END IF;
         gmp_calendar_api.get_contiguous_periods
                                        (p_api_version        => 1
                                        ,p_init_msg_list      => TRUE
                                        ,p_start_date         => p_start_date
                                        ,p_end_date           => NULL
                                        ,p_calendar_code      => p_calendar_code
                                        ,p_duration           => p_offset
                                        ,p_output_tbl         => l_contig_period_tbl
                                        ,x_return_status      => l_return_status);

         IF (l_return_status <> 'S') THEN
            RETURN NULL;
         END IF;

         l_cal_count := l_contig_period_tbl.COUNT;
         l_start_date := l_contig_period_tbl (l_cal_count).end_date;

         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line ('rows returned from get_contiguous_periods is '||l_cal_count); -- Bug 13582990
         END IF;         
      ELSE                                                 /* p_offset >= 0 */
         IF NVL (g_debug, -1) = gme_debug.g_log_statement THEN
            gme_debug.put_line ('calling get_contiguous_periods from point 2 get_working_time'); -- Bug 13582990
            gme_debug.put_line ('END date passed in is '||TO_CHAR(p_start_date, 'DD-MON-YYYY HH24:MI:SS')); -- Bug 13582990
            gme_debug.put_line ('duration passed in is '||ABS(p_offset)); -- Bug 13582990
            gme_debug.put_line ('p_calendar_code passed in is '||p_calendar_code); -- Bug 13582990
         END IF;
         gmp_calendar_api.get_contiguous_periods
                                        (p_api_version        => 1
                                        ,p_init_msg_list      => TRUE
                                        ,p_start_date         => NULL
                                        ,p_end_date           => p_start_date
                                        ,p_calendar_code      => p_calendar_code
                                        ,p_duration           => ABS (p_offset)
                                        ,p_output_tbl         => l_contig_period_tbl
                                        ,x_return_status      => l_return_status);

         IF (l_return_status <> 'S') THEN
            RETURN NULL;
         END IF;

         l_cal_count := l_contig_period_tbl.COUNT;
         l_start_date := l_contig_period_tbl (l_cal_count).start_date;
         
         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line ('rows returned from get_contiguous_periods is '||l_cal_count); -- Bug 13582990
         END IF;         
      END IF;                                              /* p_offset >= 0 */

      IF gmp_calendar_api.is_working_daytime
                                          (p_api_version        => 1
                                          ,p_init_msg_list      => TRUE
                                          ,p_calendar_code      => p_calendar_code
                                          ,p_date               => l_start_date
                                          ,p_ind                => 0
                                          ,x_return_status      => l_return_status) THEN
         RETURN l_start_date;
      ELSE                           /* gmp_calendar_api.is_working_daytime */
         l_diff := 1 / 3600;

         IF NVL (g_debug, -1) = gme_debug.g_log_statement THEN
            gme_debug.put_line (l_api_name || ':l_diff ' || l_diff);
         END IF;

         IF p_offset > 0 THEN
            /* If offset was 0 then the l_start_date is the working date time */
            IF NVL (g_debug, -1) = gme_debug.g_log_statement THEN
               gme_debug.put_line ('calling get_contiguous_periods from point 3 get_working_time'); -- Bug 13582990
               gme_debug.put_line ('start date passed in is '||TO_CHAR(l_start_date, 'DD-MON-YYYY HH24:MI:SS')); -- Bug 13582990
               gme_debug.put_line ('duration passed in is '||l_diff); -- Bug 13582990
               gme_debug.put_line ('p_calendar_code passed in is '||p_calendar_code); -- Bug 13582990
            END IF;
            gmp_calendar_api.get_contiguous_periods
                                        (p_api_version        => 1
                                        ,p_init_msg_list      => TRUE
                                        ,p_start_date         => l_start_date
                                        ,p_end_date           => NULL
                                        ,p_calendar_code      => p_calendar_code
                                        ,p_duration           => l_diff
                                        ,p_output_tbl         => l_contig_period_tbl
                                        ,x_return_status      => l_return_status);

            IF (l_return_status <> 'S') THEN
               RETURN NULL;
            END IF;

            l_cal_count := l_contig_period_tbl.COUNT;
            l_start_date :=
                (l_contig_period_tbl (l_cal_count).end_date - (l_diff / 24) );
                
            IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
               gme_debug.put_line ('rows returned from get_contiguous_periods is '||l_cal_count); -- Bug 13582990
            END IF;         
         ELSE                                               /* p_offset > 0 */
            IF NVL (g_debug, -1) = gme_debug.g_log_statement THEN
               gme_debug.put_line ('calling get_contiguous_periods from point 4 get_working_time'); -- Bug 13582990
               gme_debug.put_line ('END date passed in is '||TO_CHAR(l_start_date, 'DD-MON-YYYY HH24:MI:SS')); -- Bug 13582990
               gme_debug.put_line ('duration passed in is '||l_diff); -- Bug 13582990
               gme_debug.put_line ('p_calendar_code passed in is '||p_calendar_code); -- Bug 13582990
            END IF;
            gmp_calendar_api.get_contiguous_periods
                                        (p_api_version        => 1
                                        ,p_init_msg_list      => TRUE
                                        ,p_start_date         => NULL
                                        ,p_end_date           => l_start_date
                                        ,p_calendar_code      => p_calendar_code
                                        ,p_duration           => l_diff
                                        ,p_output_tbl         => l_contig_period_tbl
                                        ,x_return_status      => l_return_status);

            IF (l_return_status <> 'S') THEN
               RETURN NULL;
            END IF;

            l_cal_count := l_contig_period_tbl.COUNT;
            l_start_date :=
               (l_contig_period_tbl (l_cal_count).start_date + (l_diff / 24) );
               
            IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
               gme_debug.put_line ('rows returned from get_contiguous_periods is '||l_cal_count); -- Bug 13582990
            END IF;         
         END IF;                                           /* p_offset > 0  */
      END IF;                        /* gmp_calendar_api.is_working_daytime */

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;

      RETURN l_start_date;
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
         RETURN NULL;
   END get_working_start_time;

   /*======================================================================
   --  PROCEDURE :
   --   calc_dates
   --
   --  DESCRIPTION:
   --    This PL/SQL procedure  is responsible for calculating dates for the batch
   --
   --  REQUIREMENTS
   --    p_gme_batch_header_rec non null value.
   --  SYNOPSIS:
   --    calc_dates (p_gme_batch_header_rec,p_use_workday_cal, X_return_status);
   --  Pawan Kumar bug 823188 added for shop calendar
   --     Added additional parameters for the shop calendar implementation.

   --  G. Muratore    23-Oct-2013   Bug 16297308
   --     Reset plan usage to consider actual usage when rescheduling a wip step
   --===================================================================== */
   PROCEDURE calc_dates (
      p_gme_batch_header_rec   IN              gme_batch_header%ROWTYPE
     ,p_use_workday_cal        IN              VARCHAR2
     ,p_contiguity_override    IN              VARCHAR2
     ,p_return_status          OUT NOCOPY      VARCHAR2
     ,p_step_id                IN              gme_batch_steps.batchstep_id%TYPE
            DEFAULT NULL
     ,p_plan_start_date        IN              DATE DEFAULT NULL
     ,p_plan_cmplt_date        IN              DATE DEFAULT NULL)
   IS
      x_cur_row                 PLS_INTEGER;
      x_step_tbl                step_tab;
      l_step_duration_tab       step_duration_tab;
      x_act_tab                 activities_tab;
      x_act_row                 gme_batch_step_activities%ROWTYPE;
      x_rsrc_tab                resources_tab;
      x_rsrc_row                gme_batch_step_resources%ROWTYPE;
      x_rsrc_txns_tab           rsrc_txns_tab;
      x_step_row                gme_batch_steps%ROWTYPE;
      x_max_act_date            DATE;
      x_max_rsrc_date           DATE;
      l_return_status           VARCHAR2 (1);
      x_routing_id              gme_batch_header.routing_id%TYPE;
      x_gmd_step_tbl            gmd_auto_step_calc.step_rec_tbl;
      l_batch_id                gme_batch_header.batch_id%TYPE;
      x_batch_duration          NUMBER;
      x_batch_start_date        DATE;
      x_step_start_date         DATE;
      l_rsrc_start_date         DATE;  -- 17410850
      x_step_no                 gme_batch_steps.batchstep_no%TYPE;
      max_cmplt_date            DATE;
      min_start_date            DATE;
      l_usage_hrs               gme_batch_step_resources.plan_rsrc_usage%TYPE;
      l_batchstep_activity_id   NUMBER;
      l_batchstep_id            NUMBER;
      l_cal_count               NUMBER;
      l_contig_period_tbl       gmp_calendar_api.contig_period_tbl;
      l_duration                NUMBER;
      l_activity                VARCHAR2 (80);
      l_api_name       CONSTANT VARCHAR2 (30)                 := 'Calc dates';

      CURSOR cur_get_step_activities (v_step_id NUMBER, v_batch_id NUMBER)
      IS
         SELECT *
           FROM gme_batch_step_activities
          WHERE batch_id = v_batch_id AND batchstep_id = v_step_id;

      CURSOR cur_get_act_rsrcs (v_batchstep_activity_id NUMBER)
      IS
         SELECT *
           FROM gme_batch_step_resources
          WHERE batchstep_activity_id = v_batchstep_activity_id;

      CURSOR cur_get_step_id (v_step NUMBER, v_batch_id NUMBER)
      IS
         SELECT batchstep_id
           FROM gme_batch_steps
          WHERE batch_id = v_batch_id AND batchstep_no = v_step;

      CURSOR cur_get_step (v_batchstep_id NUMBER)
      IS
         SELECT batchstep_no
           FROM gme_batch_steps
          WHERE batchstep_id = v_batchstep_id;

      CURSOR cur_get_activity (v_batchstep_activity_id NUMBER)
      IS
         SELECT activity
           FROM gme_batch_step_activities
          WHERE batchstep_activity_id = v_batchstep_activity_id;

      CURSOR cur_recipe_validity_rule (
         p_recipe_validity_rule_id   gme_batch_header.recipe_validity_rule_id%TYPE)
      IS
         SELECT inventory_item_id
           FROM gmd_recipe_validity_rules
          WHERE recipe_validity_rule_id = p_recipe_validity_rule_id;

      load_steps_failed         EXCEPTION;
      no_activities             EXCEPTION;
      no_resources              EXCEPTION;
      error_insert_res_txns     EXCEPTION;
      error_cont_period         EXCEPTION;
      error_non_contiguious     EXCEPTION;
      l_doc_type                VARCHAR2 (4);
      l_calendar_code           VARCHAR2 (10);
      l_cont_ind                NUMBER;
      l_item_id                 NUMBER;
      l_use_workday_cal         VARCHAR2 (5);
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
         gme_debug.put_line ('start calc_dates');
         gme_debug.put_line ('p_step_id is '||p_step_id); -- Bug 13582990
         gme_debug.put_line ('p_use_workday_cal is '||p_use_workday_cal); -- Bug 13582990
         gme_debug.put_line ('p_contiguity_override is '||p_contiguity_override); -- Bug 13582990
      END IF;

      IF p_plan_start_date IS NOT NULL THEN
         gme_debug.put_line ('p_plan_start_date is '||
                             TO_CHAR(p_plan_start_date,'DD-MON-YYYY HH24:MI:SS'));
      ELSE
         gme_debug.put_line ('p_plan_start_date is NULL');
      END IF;

      IF p_plan_cmplt_date IS NOT NULL THEN
         gme_debug.put_line ('p_plan_cmplt date is '||
                             TO_CHAR(p_plan_cmplt_date,'DD-MON-YYYY HH24:MI:SS'));
      ELSE
         gme_debug.put_line ('p_plan_cmplt date is NULL');
      END IF;     

      p_return_status := fnd_api.g_ret_sts_success;
      l_batch_id := p_gme_batch_header_rec.batch_id;

      IF (p_gme_batch_header_rec.batch_type = 0) THEN
         l_doc_type := 'PROD';
      ELSE
         l_doc_type := 'FPO';
      END IF;

      l_use_workday_cal := p_use_workday_cal;

      IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
         gme_debug.put_line ('batch_id=' || l_batch_id);
      END IF;

      l_calendar_code := gme_common_pvt.g_calendar_code;

      -- Restructured code to get contiguous indicator from GMD like in batch create.
      -- Rogue code was found while working on 16873752
      l_cont_ind := 1;
      IF l_calendar_code IS NULL THEN
         l_use_workday_cal := fnd_api.g_false;
      ELSE
         OPEN cur_recipe_validity_rule
                              (p_gme_batch_header_rec.recipe_validity_rule_id);

         FETCH cur_recipe_validity_rule
          INTO l_item_id;

         CLOSE cur_recipe_validity_rule;

         -- Fetch continguous indicator from gmd instead of hard coding.
         IF l_use_workday_cal = fnd_api.g_true THEN
            gmd_recipe_fetch_pub.fetch_contiguous_ind
               (p_recipe_id                    => 1  -- This is a bogus value for compilation only.
               ,p_orgn_id                      => p_gme_batch_header_rec.organization_id
               ,p_recipe_validity_rule_id      => p_gme_batch_header_rec.recipe_validity_rule_id
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
         END IF;
      END IF;

      IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
         gme_debug.put_line ('cal code is '||l_calendar_code); -- Bug 13582990
         gme_debug.put_line ('Cont IND is ' || l_cont_ind);
      END IF;

      /* If P_step_id is NULL, this means we want to process all steps in the batch.  If        */
      /* it is not NULL, then we only want to calc the dates for that step, i.e. insert_step... */
      IF (p_step_id IS NULL) THEN
         gmd_auto_step_calc.load_steps (l_batch_id
                                       ,1
                                       ,NULL
                                       ,x_gmd_step_tbl
                                       ,x_routing_id
                                       ,l_return_status);

         IF l_return_status <> p_return_status THEN
            RAISE load_steps_failed;
         END IF;

         /* populate X_step_tbl with the step_id that corresponds to that returned from the GMD load_steps  */
         FOR i IN x_gmd_step_tbl.FIRST .. x_gmd_step_tbl.LAST LOOP
            OPEN cur_get_step_id (x_gmd_step_tbl (i).step_no, l_batch_id);

            FETCH cur_get_step_id
             INTO x_step_tbl (i);

            CLOSE cur_get_step_id;
         END LOOP;
      ELSE
         x_step_tbl (1) := p_step_id;
         x_gmd_step_tbl (1).step_id := p_step_id;

         SELECT batchstep_no
           INTO x_gmd_step_tbl (1).step_no
           FROM gme_batch_steps
          WHERE batchstep_id = p_step_id;
      END IF;

      -- Calculate the duration of each step...
      FOR i IN x_step_tbl.FIRST .. x_step_tbl.LAST LOOP
         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line ('step_no ' || x_gmd_step_tbl (i).step_no);
            gme_debug.put_line ('step_id ' || x_gmd_step_tbl (i).step_id);
         END IF;

         l_step_duration_tab (x_gmd_step_tbl (i).step_no) :=
                                 get_max_duration (x_step_tbl (i), l_batch_id);

         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line
                             (   'stepduration '
                              || l_step_duration_tab
                                                    (x_gmd_step_tbl (i).step_no) );
         END IF;
      END LOOP;

      x_batch_start_date := p_gme_batch_header_rec.plan_start_date;
      x_step_start_date := p_plan_start_date;

      IF (p_step_id IS NOT NULL) THEN
         -- This is a step insert... calc the start date with the cmplt date and duration...
         -- If the start date is NOT NULL, then we use that... don't care if planned completion date is filled in... we'll calc anyway
         -- If both dates are null, that's OK too, the code below will use the batch plan start date.
         IF (p_plan_start_date IS NULL) AND (p_plan_cmplt_date IS NOT NULL) THEN
            SELECT batchstep_no
              INTO x_step_no
              FROM gme_batch_steps
             WHERE batchstep_id = p_step_id;

            IF l_use_workday_cal = fnd_api.g_true THEN
               IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
                  gme_debug.put_line ('calling get_contiguous_periods from point 1 calc_dates'); -- Bug 13582990
                  gme_debug.put_line ('plan_cmplt date is not null used for END DATE parm is '||
                                      TO_CHAR(p_plan_cmplt_date,'DD-MON-YYYY HH24:MI:SS'));
                  gme_debug.put_line ('duration passed in is '||l_step_duration_tab(x_step_no)); -- Bug 13582990
                  gme_debug.put_line ('p_calendar_code passed in is '||l_calendar_code); -- Bug 13582990
               END IF;

               gmp_calendar_api.get_contiguous_periods
                                (p_api_version        => 1
                                ,p_init_msg_list      => TRUE
                                ,p_start_date         => NULL
                                ,p_end_date           => p_plan_cmplt_date
                                ,p_calendar_code      => l_calendar_code
                                ,p_duration           => l_step_duration_tab(x_step_no)
                                ,p_output_tbl         => l_contig_period_tbl
                                ,x_return_status      => l_return_status);

               IF (l_return_status <> p_return_status) THEN
                  RAISE error_cont_period;
               END IF;

               l_cal_count := l_contig_period_tbl.COUNT;

               IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
                  gme_debug.put_line ('rows returned from get_contiguous_periods is '||l_cal_count); -- Bug 13582990
               END IF;         

               IF l_cont_ind = 1 AND p_contiguity_override = fnd_api.g_false THEN
                  IF l_cal_count > 1 THEN
                     RAISE error_non_contiguious;
                  END IF;
               END IF;

               x_step_start_date :=
                                  l_contig_period_tbl (l_cal_count).start_date;
            ELSE
               x_step_start_date :=
                      p_plan_cmplt_date - l_step_duration_tab (x_step_no)
                                          / 24;
            END IF;

            IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
               gme_debug.put_line (   'calculated step start date is '
                                   || TO_CHAR (x_step_start_date
                                              ,'DD-MON-YYYY HH24:MI:SS') );
            END IF;
         END IF;
      ELSE
         IF (p_gme_batch_header_rec.plan_start_date IS NOT NULL) THEN
            calc_longest_time (l_batch_id
                              ,l_step_duration_tab
                              ,x_batch_duration
                              ,l_return_status);

            IF l_use_workday_cal = fnd_api.g_true THEN
               IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
                  gme_debug.put_line ('calling get_contiguous_periods from point 2 calc_dates'); -- Bug 13582990                                     
                  gme_debug.put_line ('start date is not null used for START DATE parm is '||
                                      TO_CHAR(p_gme_batch_header_rec.plan_start_date,'DD-MON-YYYY HH24:MI:SS'));
                  gme_debug.put_line ('duration passed in is '||x_batch_duration); -- Bug 13582990
                  gme_debug.put_line ('p_calendar_code passed in is '||l_calendar_code); -- Bug 13582990
               END IF;

               gmp_calendar_api.get_contiguous_periods
                      (p_api_version        => 1
                      ,p_init_msg_list      => TRUE
                      ,p_start_date         => p_gme_batch_header_rec.plan_start_date
                      ,p_end_date           => NULL
                      ,p_calendar_code      => l_calendar_code
                      ,p_duration           => x_batch_duration
                      ,p_output_tbl         => l_contig_period_tbl
                      ,x_return_status      => l_return_status);

               IF (l_return_status <> p_return_status) THEN
                  RAISE error_cont_period;
               END IF;

               l_cal_count := l_contig_period_tbl.COUNT;

               IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
                  gme_debug.put_line ('rows returned from get_contiguous_periods is '||l_cal_count); -- Bug 13582990
               END IF;         

               IF l_cont_ind = 1 AND p_contiguity_override = fnd_api.g_false THEN
                  IF l_cal_count > 1 THEN
                     RAISE error_non_contiguious;
                  END IF;
               END IF;
            END IF;

            x_batch_start_date := p_gme_batch_header_rec.plan_start_date;
         ELSIF     (p_gme_batch_header_rec.plan_start_date IS NULL)
               AND (p_gme_batch_header_rec.plan_cmplt_date IS NOT NULL) THEN
            -- calc longest time for the entire batch if we're only given cmplt date...
            calc_longest_time (l_batch_id
                              ,l_step_duration_tab
                              ,x_batch_duration
                              ,l_return_status);

            IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
               gme_debug.put_line ('duration of batch is ' || x_batch_duration);
               gme_debug.put_line
                            (   'batch completion date is '
                             || TO_CHAR
                                      (p_gme_batch_header_rec.plan_cmplt_date
                                      ,'DD-MON-YYYY HH24:MI:SS') );
            END IF;

            IF l_use_workday_cal = fnd_api.g_true THEN
               IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
                  gme_debug.put_line ('calling get_contiguous_periods from point 3 calc_dates'); -- Bug 13582990
                  -- gme_debug.put_line ('end date null ' || l_use_workday_cal);
                  gme_debug.put_line ('plan_cmplt_date is not null used for END DATE parm is '||
                                      TO_CHAR(p_gme_batch_header_rec.plan_cmplt_date,'DD-MON-YYYY HH24:MI:SS'));
                  gme_debug.put_line ('duration passed in is '||x_batch_duration); -- Bug 13582990
                  gme_debug.put_line ('p_calendar_code passed in is '||l_calendar_code); -- Bug 13582990
               END IF;

               gmp_calendar_api.get_contiguous_periods
                        (p_api_version        => 1
                        ,p_init_msg_list      => TRUE
                        ,p_start_date         => NULL
                        ,p_end_date           => p_gme_batch_header_rec.plan_cmplt_date
                        ,p_calendar_code      => l_calendar_code
                        ,p_duration           => x_batch_duration
                        ,p_output_tbl         => l_contig_period_tbl
                        ,x_return_status      => l_return_status);

               IF (l_return_status <> p_return_status) THEN
                  RAISE error_cont_period;
               END IF;

               l_cal_count := l_contig_period_tbl.COUNT;

               IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
                  gme_debug.put_line ('rows returned from get_contiguous_periods is '||l_cal_count); -- Bug 13582990
               END IF;         

               IF l_cont_ind = 1 AND p_contiguity_override = fnd_api.g_false THEN
                  IF l_cal_count > 1 THEN
                     RAISE error_non_contiguious;
                  END IF;
               END IF;

               x_batch_start_date :=
                                  l_contig_period_tbl (l_cal_count).start_date;
            ELSE
               x_batch_start_date :=
                    p_gme_batch_header_rec.plan_cmplt_date
                  - x_batch_duration / 24;
            END IF;

            IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
               gme_debug.put_line (   'calculated batch start date is '
                                   || TO_CHAR (x_batch_start_date
                                              ,'DD-MON-YYYY HH24:MI:SS') );
            END IF;
         ELSIF     (p_gme_batch_header_rec.plan_start_date IS NULL)
               AND (p_gme_batch_header_rec.plan_cmplt_date IS NULL) THEN
            calc_longest_time (l_batch_id
                              ,l_step_duration_tab
                              ,x_batch_duration
                              ,l_return_status);

            IF l_use_workday_cal = fnd_api.g_true THEN
               IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
                  gme_debug.put_line ('calling get_contiguous_periods from point 4 calc_dates'); -- Bug 13582990
                  gme_debug.put_line ('Both dates null. p_contiguity_override is '|| p_contiguity_override); -- Bug 13582990
                  gme_debug.put_line ('batch completion is '||
                                      TO_CHAR(p_gme_batch_header_rec.plan_cmplt_date,'DD-MON-YYYY HH24:MI:SS')); -- Bug 13582990
                  gme_debug.put_line ('start date passed in gme_common_pvt.g_timestamp is '||
                                      TO_CHAR(gme_common_pvt.g_timestamp,'DD-MON-YYYY HH24:MI:SS')); -- Bug 13582990
                  gme_debug.put_line ('duration passed in is '||x_batch_duration); -- Bug 13582990
                  gme_debug.put_line ('p_calendar_code passed in is '||l_calendar_code); -- Bug 13582990
               END IF;

               gmp_calendar_api.get_contiguous_periods
                                  (p_api_version        => 1
                                  ,p_init_msg_list      => TRUE
                                  ,p_start_date         => gme_common_pvt.g_timestamp
                                  ,p_end_date           => NULL
                                  ,p_calendar_code      => l_calendar_code
                                  ,p_duration           => x_batch_duration
                                  ,p_output_tbl         => l_contig_period_tbl
                                  ,x_return_status      => l_return_status);

               IF (l_return_status <> p_return_status) THEN
                  RAISE error_cont_period;
               END IF;

               l_cal_count := l_contig_period_tbl.COUNT;

               IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
                  gme_debug.put_line ('rows returned from get_contiguous_periods is '||l_cal_count); -- Bug 13582990
               END IF;         

               IF l_cont_ind = 1 AND p_contiguity_override = fnd_api.g_false THEN
                  IF l_cal_count > 1 THEN
                     RAISE error_non_contiguious;
                  END IF;
               END IF;
            END IF;

            x_batch_start_date := gme_common_pvt.g_timestamp;
         END IF;
      END IF;

      /* Calculate the dates for all the rows in X_step_tbl */
      FOR i IN x_step_tbl.FIRST .. x_step_tbl.LAST LOOP
         SELECT *
           INTO x_step_row
           FROM gme_batch_steps
          WHERE batchstep_id = x_step_tbl(i);

         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line ('Inside x_step_tbl loop. Iteration is '||i);
            gme_debug.put_line ('step_no=' || x_step_row.batchstep_no); -- Bug 13582990
            gme_debug.put_line ('step_id=' || x_step_tbl(i) );
         END IF;

         -- For individual steps you can pass in a plan_start date or use the setup data.
         IF (x_step_start_date IS NULL) THEN
            x_step_row.plan_start_date :=
               get_max_step_date (p_use_workday_cal       => p_use_workday_cal
                                 ,p_calendar_code         => l_calendar_code
                                 ,p_batchstep_id          => x_step_tbl (i)
                                 ,p_batch_id              => l_batch_id
                                 ,p_batch_start_date      => x_batch_start_date);
         ELSE
            x_step_row.plan_start_date := x_step_start_date;
         END IF;

         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line (   '1 '
                                || TO_CHAR (x_step_row.plan_start_date
                                           ,'DD-MON-YYYY HH24:MI:SS') );
         END IF;

         x_act_tab.DELETE;
         x_cur_row := 0;

         OPEN cur_get_step_activities (x_step_tbl (i), l_batch_id);

         FETCH cur_get_step_activities
          INTO x_act_row;

         WHILE cur_get_step_activities%FOUND LOOP
            x_cur_row := x_cur_row + 1;
            x_act_tab (x_cur_row) := x_act_row;

            FETCH cur_get_step_activities
             INTO x_act_row;
         END LOOP;

         CLOSE cur_get_step_activities;

         IF x_cur_row = 0 THEN
            l_batchstep_id := x_step_tbl (i);
            RAISE no_activities;
         END IF;

         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line ('number of activities is '||x_cur_row);
         END IF;

         FOR j IN x_act_tab.FIRST .. x_act_tab.LAST LOOP
            IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
               gme_debug.put_line (' activity is '|| x_act_tab(j).activity);
               gme_debug.put_line (' step start date '
                                   || TO_CHAR (x_step_row.plan_start_date
                                              ,'DD-MON-YYYY HH24:MI:SS') );
            END IF;
            
            IF l_use_workday_cal = fnd_api.g_true THEN
               x_act_tab (j).plan_start_date :=
                  get_working_start_time
                                  (p_start_date         => x_step_row.plan_start_date
                                  ,p_offset             => x_act_tab (j).offset_interval
                                  ,p_calendar_code      => l_calendar_code);

               IF x_act_tab (j).plan_start_date IS NULL THEN
                  RAISE error_cont_period;
               END IF;
            ELSE
               x_act_tab (j).plan_start_date :=
                    x_step_row.plan_start_date
                  + x_act_tab (j).offset_interval / 24;
            END IF;

            IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
               gme_debug.put_line ('offset' || x_act_tab (j).offset_interval);
               gme_debug.put_line (   'x_act_tab(j).plan_start_date '
                                   || TO_CHAR (x_act_tab (j).plan_start_date
                                              ,'DD-MON-YYYY HH24:MI:SS') );
            END IF;

            -- Retrieve all resources for this activity
            x_rsrc_tab.DELETE;
            x_cur_row := 0;

            OPEN cur_get_act_rsrcs (x_act_tab (j).batchstep_activity_id);

            FETCH cur_get_act_rsrcs
             INTO x_rsrc_row;

            WHILE cur_get_act_rsrcs%FOUND LOOP
               x_cur_row := x_cur_row + 1;
               x_rsrc_tab (x_cur_row) := x_rsrc_row;

               -- 16297308 - Reset plan in case this is a reschedule of a wip step that has actuals.
               x_rsrc_tab(x_cur_row).plan_rsrc_usage :=
                  x_rsrc_tab(x_cur_row).plan_rsrc_usage - 
                  NVL(x_rsrc_tab(x_cur_row).actual_rsrc_usage, 0);

               FETCH cur_get_act_rsrcs
                INTO x_rsrc_row;
            END LOOP;

            CLOSE cur_get_act_rsrcs;

            IF x_cur_row = 0 THEN
               l_batchstep_activity_id := x_act_tab (j).batchstep_activity_id;
               RAISE no_resources;
            END IF;

            IF p_gme_batch_header_rec.update_inventory_ind = 'Y' THEN
               x_rsrc_txns_tab.DELETE;
               x_cur_row := 0;
            END IF;
            /* IF p_gme_batch_header_rec.update_inventory_ind = 'Y' THEN */

            FOR k IN x_rsrc_tab.FIRST .. x_rsrc_tab.LAST LOOP
            
               -- DO NOT REINSTATE THIS CODE !!
               -- Following variable was put in to be used by 17410850. It turns out that
               -- we do want the dates on the transactions to be changed but not on the
               -- step/act/resource levels. This functionality already happens on the form
               -- for the defautl transaction when creating a separate completed transaction.
               -- 17410850 Hold original date in case we have wip step resched.
               l_rsrc_start_date := x_rsrc_tab (k).plan_start_date; 
               
               IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
                  gme_debug.put_line('l_rsrc_start_date is '|| TO_CHAR(l_rsrc_start_date, 'DD-MON-YYYY HH24:MI:SS') );
               END IF;
                  
               IF l_use_workday_cal = fnd_api.g_true THEN
                  x_rsrc_tab (k).plan_start_date :=
                     get_working_start_time
                               (p_start_date         => x_act_tab (j).plan_start_date
                               ,p_offset             => x_rsrc_tab (k).offset_interval
                               ,p_calendar_code      => l_calendar_code);
               
                  IF x_rsrc_tab (k).plan_start_date IS NULL THEN
                     RAISE error_cont_period;
                  END IF;
               
                  IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
                     gme_debug.put_line
                                    (   'rsrc_start_date '
                                     || TO_CHAR
                                               (x_rsrc_tab (k).plan_start_date
                                               ,'DD-MON-YYYY HH24:MI:SS') );
                  END IF;
               ELSE
                  x_rsrc_tab (k).plan_start_date :=
                       x_act_tab (j).plan_start_date
                     + x_rsrc_tab (k).offset_interval / 24;
               END IF;
               
               get_usage_in_hours (x_rsrc_tab (k).plan_rsrc_usage
                                  ,x_rsrc_tab (k).usage_um
                                  ,l_usage_hrs
                                  ,l_return_status);
                                  
               l_duration := l_usage_hrs / x_rsrc_tab (k).plan_rsrc_count;

               IF     l_use_workday_cal = fnd_api.g_true
                  AND l_calendar_code IS NOT NULL THEN
                  IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
                     gme_debug.put_line ('calling get_contiguous_periods from point 5 calc_dates'); -- Bug 13582990
                     gme_debug.put_line ('start date passed in is '||
                                         TO_CHAR(x_rsrc_tab(k).plan_start_date, 'DD-MON-YYYY HH24:MI:SS')); -- Bug 13582990
                     gme_debug.put_line ('duration passed in is '||l_duration); -- Bug 13582990
                     gme_debug.put_line ('p_calendar_code passed in is '||l_calendar_code); -- Bug 13582990
                  END IF;
                  
                  gmp_calendar_api.get_contiguous_periods
                              (p_api_version        => 1
                              ,p_init_msg_list      => TRUE
                              ,p_start_date         => x_rsrc_tab (k).plan_start_date
                              ,p_end_date           => NULL
                              ,p_calendar_code      => l_calendar_code
                              ,p_duration           => l_duration
                              ,p_output_tbl         => l_contig_period_tbl
                              ,x_return_status      => l_return_status);

                  IF (l_return_status <> p_return_status) THEN
                     RAISE error_cont_period;
                  END IF;

                  -- DO NOT REINSTATE THIS CODE !!
/*                  
                  -- Bug 17410850 - Reset plan_start_date for wip steps.
                  IF (x_step_row.step_status = gme_common_pvt.g_step_wip) THEN         
                     FOR m IN 1 .. l_cal_count LOOP
                        l_contig_period_tbl (m).start_date := l_rsrc_start_date;                     
                     END LOOP;
                  END IF; */                 
               ELSE
                  l_contig_period_tbl (1).start_date :=
                                               x_rsrc_tab (k).plan_start_date;
                  l_contig_period_tbl (1).end_date :=
                             x_rsrc_tab (k).plan_start_date + l_duration / 24;

                  -- DO NOT REINSTATE THIS CODE !!
/*
                  -- Bug 17410850 - Reset plan_start_date for wip steps.
                  IF (x_step_row.step_status = gme_common_pvt.g_step_wip) THEN         
                     l_contig_period_tbl (1).start_date := l_rsrc_start_date;
                  END IF; */
               END IF;

               l_cal_count := l_contig_period_tbl.COUNT;
               
               IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
                  gme_debug.put_line ('rows returned from get_contiguous_periods is '||l_cal_count); -- Bug 13582990
               END IF;
               
               x_rsrc_tab (k).plan_cmplt_date :=
                                    l_contig_period_tbl (l_cal_count).end_date;

               IF p_gme_batch_header_rec.update_inventory_ind = 'Y' THEN
                  FOR m IN 1 .. l_cal_count LOOP

                     IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
                        gme_debug.put_line ('rsrc loop iteration is '||m); -- Bug 13582990
                        gme_debug.put_line ('l_contig_period_tbl(m).start_date is '||
                                            TO_CHAR(l_contig_period_tbl(m).start_date, 'DD-MON-YYYY HH24:MI:SS')); -- Bug 13582990
                                            
                        gme_debug.put_line ('l_contig_period_tbl(m).end_date is '||
                                            TO_CHAR(l_contig_period_tbl(m).end_date, 'DD-MON-YYYY HH24:MI:SS')); -- Bug 13582990
                     END IF;         
                                                                                      
                     x_rsrc_tab (k).plan_start_date := l_contig_period_tbl (m).start_date;
                     x_rsrc_tab (k).plan_cmplt_date := l_contig_period_tbl (m).end_date;
                                             
                     -- Bug 9212573 - Now pass parameter p_trans_count. This will tell 
                     -- underlying routine if there is only one transaction required and
                     -- therefore it can derive the txn usage with a simpler algorithm.
                     insert_resource_txns (p_gme_batch_header_rec => p_gme_batch_header_rec
                                          ,p_doc_type => l_doc_type
                                          ,p_batch_step_resources_rec => x_rsrc_tab (k)
                                          ,p_trans_count => l_cal_count
                                          ,x_return_status => l_return_status);
                                          
                     /* insert_resource_txns (p_gme_batch_header_rec
                                          ,l_doc_type
                                          ,x_rsrc_tab (k)
                                          ,l_return_status); */

                     IF (l_return_status <> p_return_status) THEN
                        RAISE error_insert_res_txns;
                     END IF;
                  END LOOP;

                  x_rsrc_tab (k).plan_start_date :=
                                            l_contig_period_tbl (1).start_date;
               END IF;
               /* IF p_gme_batch_header_rec.update_inventory_ind = 'Y' THEN */
               
               IF k = x_rsrc_tab.FIRST OR x_rsrc_tab (k).plan_cmplt_date > x_max_rsrc_date THEN
                  x_max_rsrc_date := x_rsrc_tab (k).plan_cmplt_date;
               END IF;
            END LOOP;                                      /* FOR resources */

            x_act_tab (j).plan_cmplt_date := x_max_rsrc_date;

            IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
               gme_debug.put_line ('j iteration is '|| j);
               gme_debug.put_line ('x_act_tab(j).plan_cmplt_date is '
                                   || TO_CHAR (x_act_tab (j).plan_cmplt_date
                                              ,'DD-MON-YYYY HH24:MI:SS') );
            END IF;

            IF    j = x_act_tab.FIRST
               OR x_act_tab (j).plan_cmplt_date > x_max_act_date THEN
               x_max_act_date := x_act_tab (j).plan_cmplt_date;
            END IF;

            FOR k IN x_rsrc_tab.FIRST .. x_rsrc_tab.LAST LOOP
               -- Bug 17410850 - Do not update plan_start_date for wip steps.
               IF (x_step_row.step_status = gme_common_pvt.g_step_pending) THEN         
                  UPDATE gme_batch_step_resources
                     SET plan_start_date = x_rsrc_tab (k).plan_start_date
                        ,plan_cmplt_date = x_rsrc_tab (k).plan_cmplt_date
                   WHERE batchstep_resource_id =
                                          x_rsrc_tab (k).batchstep_resource_id;
               ELSE
                  UPDATE gme_batch_step_resources
                     SET plan_cmplt_date = x_rsrc_tab (k).plan_cmplt_date
                   WHERE batchstep_resource_id =
                                          x_rsrc_tab (k).batchstep_resource_id;
               END IF;
            END LOOP;
         END LOOP;             /*  FOR j IN X_act_tab.FIRST..X_act_tab.LAST */

         x_step_row.plan_cmplt_date := x_max_act_date;
         x_step_row.due_date := x_step_row.plan_cmplt_date;

         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line (   '1 '
                                || TO_CHAR (x_step_row.plan_cmplt_date
                                           ,'DD-MON-YYYY HH24:MI:SS') );
            gme_debug.put_line (   'due_date = '
                                || TO_CHAR (x_step_row.due_date
                                           ,'DD-MON-YYYY HH24:MI:SS') );
         END IF;

         FOR k IN x_act_tab.FIRST .. x_act_tab.LAST LOOP

            -- Bug 17410850 - Do not update plan_start_date for wip steps.
            IF (x_step_row.step_status = gme_common_pvt.g_step_pending) THEN         
               UPDATE gme_batch_step_activities
                  SET plan_start_date = x_act_tab (k).plan_start_date
                     ,plan_cmplt_date = x_act_tab (k).plan_cmplt_date
                WHERE batchstep_activity_id = x_act_tab (k).batchstep_activity_id;
            ELSE
               UPDATE gme_batch_step_activities
                  SET plan_cmplt_date = x_act_tab (k).plan_cmplt_date
                WHERE batchstep_activity_id = x_act_tab (k).batchstep_activity_id;            
            END IF;
         END LOOP;

         -- Bug 17410850 - Do not update plan_start_date for wip steps.
         IF (x_step_row.step_status = gme_common_pvt.g_step_pending) THEN         
            UPDATE gme_batch_steps
               SET plan_start_date = x_step_row.plan_start_date
                  ,plan_cmplt_date = x_step_row.plan_cmplt_date
                  ,due_date = x_step_row.due_date
             WHERE batchstep_id = x_step_row.batchstep_id;
         ELSE
            UPDATE gme_batch_steps
               SET plan_cmplt_date = x_step_row.plan_cmplt_date
                  ,due_date = x_step_row.due_date
             WHERE batchstep_id = x_step_row.batchstep_id;      
         END IF;          
      END LOOP;                /*FOR i IN X_step_tbl.FIRST..X_step_tbl.LAST */

      -- Update the batch header dates
      IF (p_step_id IS NULL) THEN
         -- not a single step insert... however, that should also be considered for updating the header dates...
         SELECT MAX (plan_cmplt_date)
           INTO max_cmplt_date
           FROM gme_batch_steps
          WHERE batch_id = l_batch_id;

         SELECT MIN (plan_start_date)
           INTO min_start_date
           FROM gme_batch_steps
          WHERE batch_id = l_batch_id;

         IF p_gme_batch_header_rec.batch_status = 2 THEN
           -- Pawan Kumar made following changes for bug 5015873
           -- min_start_date := p_gme_batch_header_rec.plan_start_date;
            UPDATE gme_batch_header
            SET plan_cmplt_date = max_cmplt_date
               ,last_updated_by = gme_common_pvt.g_user_ident
               ,last_update_date = gme_common_pvt.g_timestamp
               ,last_update_login = gme_common_pvt.g_login_id
            WHERE batch_id = l_batch_id;
         ELSE
            UPDATE gme_batch_header
            SET plan_start_date = min_start_date
               ,plan_cmplt_date = max_cmplt_date
               ,last_updated_by = gme_common_pvt.g_user_ident
               ,last_update_date = gme_common_pvt.g_timestamp
               ,last_update_login = gme_common_pvt.g_login_id
           WHERE batch_id = l_batch_id;
         END IF;

       /*  UPDATE gme_batch_header
            SET plan_start_date = min_start_date
               ,plan_cmplt_date = max_cmplt_date
               ,last_updated_by = gme_common_pvt.g_user_ident
               ,last_update_date = gme_common_pvt.g_timestamp
               ,last_update_login = gme_common_pvt.g_login_id
          WHERE batch_id = l_batch_id;*/

         UPDATE gme_batch_header
            SET due_date = max_cmplt_date
               ,last_updated_by = gme_common_pvt.g_user_ident
               ,last_update_date = gme_common_pvt.g_timestamp
               ,last_update_login = gme_common_pvt.g_login_id
          WHERE batch_id = l_batch_id
                AND due_date = gme_common_pvt.g_timestamp;
      END IF;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;
   EXCEPTION
      WHEN load_steps_failed THEN
         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line ('calc_dates... load_steps_failed');
         END IF;

         p_return_status := fnd_api.g_ret_sts_error;
      WHEN error_cont_period THEN
         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line ('Contiguity period ... _failed');
         END IF;

         p_return_status := l_return_status;
      WHEN error_non_contiguious THEN
         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line ('Contiguity period ... not found');
         END IF;

         gme_common_pvt.log_message ('GME_NON_CONTIGUOUS_TIME');
         p_return_status := 'C';
      WHEN no_activities THEN
         p_return_status := fnd_api.g_ret_sts_error;

         OPEN cur_get_step (l_batchstep_id);

         FETCH cur_get_step
          INTO x_step_no;

         CLOSE cur_get_step;

         gme_common_pvt.log_message ('GME_NO_ACTIVITIES', 'STEPNO'
                                    ,x_step_no);
      WHEN no_resources THEN
         p_return_status := fnd_api.g_ret_sts_error;

         OPEN cur_get_activity (l_batchstep_activity_id);

         FETCH cur_get_activity
          INTO l_activity;

         CLOSE cur_get_activity;

         gme_common_pvt.log_message ('GME_NO_RESOURCES'
                                    ,'ACTIVITY'
                                    ,l_activity);
                                    
      -- add error message handling by michael tou 2013-10-10                           
      WHEN error_insert_res_txns THEN 
         p_return_status := fnd_api.g_ret_sts_error;  

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
         p_return_status := fnd_api.g_ret_sts_unexp_error;
   END calc_dates;

  /*======================================================================
  --  PROCEDURE :
  --   calc_longest_time_orig
  --
  --  DESCRIPTION:
  --    This PL/SQL procedure is responsible for calculating the amount of time
  --    the batch will take based on the steps.  You must calculate the longest
  --    path by taking every branch of the step dep tree and calculating how long
  --    each will take.  The path with the largest time is the path that will determine
  --    the length of time that the batch will take.
  --
  --  REQUIREMENTS
  --    l_batch_id non null value.
  --    l_step_duration_tab  non null value.
  --  HISTORY
  --    Chandrashekar Tiruvidula 20-Dec-2006 Rewrote procedure Bug 5716727
  
  --    G. Muratore              24-Nov-2008 Bug 7341534 
  --       Frontport of 6774660/5618732 Reinstate rewritten 11i function.
  --       Renamed this r 12 function with "_orig". Hierarchical query does not handle data
  --       where there are no step dependencies.  If that is figured out someday, this code
  --       could be reinstated.
  --================================================================================= */
  PROCEDURE calc_longest_time_orig(l_batch_id          IN         gme_batch_header.batch_id%TYPE,
                              l_step_duration_tab IN         step_duration_tab,
                              x_batch_duration    OUT NOCOPY NUMBER,
                              x_return_status     OUT NOCOPY VARCHAR2) IS
    
    CURSOR cur_get_leaf_nodes (v_batch_id NUMBER) IS
      SELECT batchstep_id, batchstep_no
      FROM   gme_batch_steps
      WHERE  batch_id = v_batch_id
             AND batchstep_id NOT IN (SELECT dep_step_id
                                      FROM gme_batch_step_dependencies
                                      WHERE batch_id = v_batch_id);
    TYPE leaf_node_fields IS RECORD(leaf_batchstep_id    NUMBER,
                                    leaf_batchstep_no    NUMBER);
    TYPE leaf_nodes_tab IS TABLE OF leaf_node_fields INDEX BY BINARY_INTEGER;
    l_leaf_nodes_tbl leaf_nodes_tab;
    /* Each record is built up of 14 character strings like this 00020000100001 by the query below
    Characters 1-5 is step_no 00020
    Characters 6-10 is dep_step_no 00010
    Character 11 is dep_type 0
    Characters 12-14 is standard delay 001 
    If there is a dependency like 30 -> 20 -> 10 then the output would be 0003000020000100020000100001 
    If there are multiple branches then we will get multiple records like the above */
    CURSOR Cur_get_branches(v_batch_id NUMBER, v_batchstep_id NUMBER) IS
      SELECT branch, LENGTH(branch) sz
      FROM (SELECT REPLACE(sys_connect_by_path(LPAD(s.batchstep_no, 5, 0)||LPAD(p.batchstep_no, 5, 0 )||dep_type||LPAD(standard_delay, 3,0),' '), ' ', NULL) branch
            FROM (SELECT * FROM gme_batch_step_dependencies WHERE batch_id = v_batch_id) d,
                 (SELECT * FROM gme_batch_steps WHERE batch_id = v_batch_id) s,
                 (SELECT * FROM gme_batch_steps WHERE batch_id = v_batch_id) p
            WHERE s.batchstep_id = d.batchstep_id
                  AND p.batchstep_id(+) = d.dep_step_id
		  AND connect_by_isleaf = 1
            START WITH d.batchstep_id = v_batchstep_id
            CONNECT BY d.batchstep_id = PRIOR d.dep_step_id) x;
    TYPE branches_tab IS TABLE OF Cur_get_branches%ROWTYPE INDEX BY BINARY_INTEGER;
    l_branches_tbl branches_tab;
    l_api_name   CONSTANT VARCHAR2 (30) := 'calc_longest_time';
    m  NUMBER;
    n  NUMBER;
    l_duration        NUMBER := 0;
    l_step_no         NUMBER;
    l_dep_step_no     NUMBER;
    l_dep_type        NUMBER;
    l_standard_delay  NUMBER;
  BEGIN
    IF g_debug <= gme_debug.g_log_procedure THEN
      gme_debug.put_line ('Entering api ' || g_pkg_name || '.'|| l_api_name);
    END IF;
    x_return_status := fnd_api.g_ret_sts_success;
    x_batch_duration := 0;
    IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
      gme_debug.put_line (g_pkg_name || '.'|| l_api_name||' calculate duration');
    END IF;
    OPEN Cur_get_leaf_nodes (l_batch_id);
    FETCH Cur_get_leaf_nodes BULK COLLECT INTO l_leaf_nodes_tbl;
    CLOSE Cur_get_leaf_nodes;
    IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
      gme_debug.put_line (g_pkg_name || '.'|| l_api_name||' number of leaf nodes: ' || l_leaf_nodes_tbl.COUNT);
    END IF;
    FOR i IN 1..l_leaf_nodes_tbl.COUNT LOOP
      OPEN Cur_get_branches(l_batch_id, l_leaf_nodes_tbl(i).leaf_batchstep_id);
      FETCH Cur_get_branches BULK COLLECT INTO l_branches_tbl;
      CLOSE Cur_get_branches;
      IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
        gme_debug.put_line(g_pkg_name || '.'|| l_api_name||' number of branches: ' || l_branches_tbl.COUNT);
      END IF;
      FOR j IN 1..l_branches_tbl.COUNT LOOP
        IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
          gme_debug.put_line(g_pkg_name || '.'|| l_api_name||' current branch: ' || l_branches_tbl(j).branch);
        END IF;
        m := 1;
        n := 5;
        /* Divide by 14 because length of step_no = 5, dep_step_no = 5, dep_type = 1, standard_delay = 3 */
        FOR k IN 1..l_branches_tbl(j).sz/14 LOOP --Start parsing string now
          l_step_no := SUBSTR(l_branches_tbl(j).branch, m, 5);
          m := n + 1;
          n := m + 4;
          l_dep_step_no := SUBSTR(l_branches_tbl(j).branch, m, 5);
          m := n + 1;
          n := m + 0;
          l_dep_type := SUBSTR(l_branches_tbl(j).branch, m, 1);
          m := n + 1;
          n := m + 2;       
          l_standard_delay := SUBSTR(l_branches_tbl(j).branch, m, 3);
          IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line(g_pkg_name || '.'|| l_api_name||' Step->Depstep->Deptype->Delay = '||l_step_no||'->'||l_dep_step_no||'->'||l_dep_type||'->'||l_standard_delay);
          END IF;          
          IF (k = 1) THEN
            l_duration := l_step_duration_tab(l_step_no);
            IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
              gme_debug.put_line(g_pkg_name || '.'|| l_api_name||' step->duration: ' || l_step_no||'->'||l_duration);
            END IF;
          END IF;
          l_duration := l_duration + l_standard_delay;
          IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line(g_pkg_name || '.'|| l_api_name||' depstep->duration: ' || l_dep_step_no||'->'||l_duration);
          END IF;          
          IF (l_dep_type = 1) THEN -- Start to Start
            IF l_step_duration_tab(l_dep_step_no) > l_duration THEN
              l_duration := l_step_duration_tab(l_dep_step_no);
            END IF;
          ELSE
            l_duration := l_duration + l_step_duration_tab(l_dep_step_no);
          END IF;
          m := n + 1;
          n := m + 4;
        END LOOP; -- parsing loop
        IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
          gme_debug.put_line(g_pkg_name || '.'|| l_api_name||' duration of branch: ' || l_duration);
        END IF;
        IF l_duration > x_batch_duration THEN
      	  x_batch_duration := l_duration;
        END IF;
        l_duration := 0;
      END LOOP; -- branches loop
    END LOOP; -- leaf nodes loop
    IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
      gme_debug.put_line(g_pkg_name || '.'|| l_api_name||' duration of batch: ' || x_batch_duration);
    END IF;
  EXCEPTION
    WHEN OTHERS THEN
      IF g_debug <= gme_debug.g_log_unexpected THEN
        gme_debug.put_line('When others exception in '|| g_pkg_name|| '.'|| l_api_name|| ' Error is '|| SQLERRM);
      END IF;
      fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
      x_return_status := fnd_api.g_ret_sts_unexp_error;
  END calc_longest_time_orig;

  /*======================================================================
  --  PROCEDURE :
  --   calc_longest_time
  --
  --  DESCRIPTION:
  --    This PL/SQL procedure is responsible for calculating the amount of time
  --    the batch will take based on the steps.  You must calculate the longest
  --    path by taking every branch of the step dep tree and calculating how long
  --    each will take.  The path with the largest time is the path that will determine
  --    the length of time that the batch will take.
  --
  --  REQUIREMENTS
  --    l_batch_id non null value.
  --    l_step_duration_tab  non null value.
  --  HISTORY
  --    G. Muratore         24-Nov-2008 Bug 7341534 
  --       Frontport of 6774660/5618732 Reinstate rewritten 11i function.
  --       Renamed this r 12 function with "_orig". Hierarchical query does not handle data
  --       where there are no step dependencies.
  --================================================================================= */
   PROCEDURE calc_longest_time (
      l_batch_id            IN              gme_batch_header.batch_id%TYPE,
      l_step_duration_tab   IN              step_duration_tab,
      x_batch_duration      OUT NOCOPY      NUMBER,
      x_return_status       OUT NOCOPY      VARCHAR2
   ) IS
      l_step_no             gme_batch_steps.batchstep_no%TYPE;
      l_dep_step_no         gme_batch_steps.batchstep_no%TYPE;
      l_leaf_nodes          step_tab;

      l_count               NUMBER                              := 0;
      i                     PLS_INTEGER;
      j                     PLS_INTEGER;
      --Bug#5077094
      branch                PLS_INTEGER := 1;
      Currec                PLS_INTEGER;
      reccount               PLS_INTEGER;
      CurrecInitVal          PLS_INTEGER;
      l_branch_batchstep_id  step_tab;
      l_branch_dep_step_id  step_tab;
      l_branch_dep_type     step_tab;
      l_branch_batchstep_no step_tab;
      l_branch_dep_step_no  step_tab;
      l_branch_standard_delay  step_tab;
      l_api_name   CONSTANT VARCHAR2 (30)              := 'Calc longest time';

      CURSOR cur_get_leaf_nodes (v_batch_id NUMBER) IS
         SELECT batchstep_id
           FROM gme_batch_steps
          WHERE batch_id = v_batch_id AND
                batchstep_id NOT IN (SELECT dep_step_id
                                       FROM gme_batch_step_dependencies
                                      WHERE batch_id = v_batch_id);
      CURSOR cur_get_step_no (v_step_id gme_batch_steps.batchstep_id%TYPE) IS
         SELECT batchstep_no
           FROM gme_batch_steps
          WHERE batchstep_id = v_step_id;

      l_brch_leaf_cnt       NUMBER                              := 0;
      l_leaf_nodeid NUMBER                              := 0;

   BEGIN
       x_return_status := FND_API.G_RET_STS_SUCCESS;
      x_batch_duration := 0;
      i := 1;

      IF (NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT) THEN
         gme_debug.put_line ('calculate duration');
      END IF;

      OPEN cur_get_leaf_nodes (l_batch_id);
      FETCH cur_get_leaf_nodes BULK COLLECT INTO l_leaf_nodes;
      CLOSE cur_get_leaf_nodes;

      IF (NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT) THEN
         gme_debug.put_line ('number of leaf nodes: ' || l_leaf_nodes.COUNT);
      END IF;

      FOR i IN l_leaf_nodes.FIRST .. l_leaf_nodes.LAST
      LOOP
         l_count := 0;

         l_count := get_longest_in_branch(l_leaf_nodes (i),l_step_duration_tab);
         IF (l_count > x_batch_duration) THEN
            x_batch_duration := l_count;
         END IF;
      END LOOP;   /* FOR leaf_nodes */

      IF (NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT) THEN
         gme_debug.put_line ('X_batch_duration = ' || x_batch_duration);
      END IF;
   EXCEPTION
      WHEN OTHERS THEN
         -- fnd_msg_pub.add_exc_msg (l_package_name, l_api_name);

         IF (NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT) THEN
            gme_debug.put_line ('ERROR - when others - sqlerrm = ' || SQLERRM);
         END IF;

         x_return_status := FND_API.g_ret_sts_unexp_error;
   END calc_longest_time;

  /*======================================================================
  --  FUNCTION :
  --   get_longest_in_branch
  --
  --  DESCRIPTION:
  --    This PL/SQL procedure is responsible for calculating the maximum amount
  --     of duration of a branch in the step dependency tree structure.
  --    The logic is to pass the leaf node and calculate the the maximum step
  --     duration out of it's subnodes and we continue to follow this recursively.
  --    for the last node having no subnodes( that is having no dependencies)
  --    we take the step duration of that node as such and this is the termination
  --    conidtion for the recursion.
  --
  --  REQUIREMENTS
  --    node non null value (batch step id).
  --    l_step_duration_tab  non null value.
  --  HISTORY
  --    G. Muratore        24-Nov-2008  Bug 7341534 
  --     Frontport of 6774660/5618732 Reinstate rewritten 11i function.
  --     Added this new procedure.
  
  --    G. Muratore        09-Jul-2010  Bug 9862326 
  --     Account for all dependent delays when deriving duration of 
  --     a given step when using start to start dependency.
 
  --    G. Muratore        13-Dec-2010  Bug 10226452  
  --     Account for all dependent delays when deriving duration of 
  --     a given step when using start to start dependency.
  --     The 9862326 fix was not complete and the additional changes are
  --     made here via the additional delay string loop.  
  
  --    G. Muratore        01-Mar-2011  Bug 11818852  
  --     Increase variable size introduced in 10226452.
  
  --    G. Muratore        12-Dec-2016  Bug 25246717/25241112  
  --     Add batch_id join to Improve performance of query.   
  --===================================================================== */
   FUNCTION get_longest_in_branch(
            node IN NUMBER
           ,l_step_duration_tab   IN step_duration_tab

                ) RETURN NUMBER IS
  --  l_step_duration_tab   step_duration_tab;
  StepDuration NUMBER := 0;
  long_in_branch NUMBER := 0;
  longi NUMBER;
  x NUMBER;
  leaf_subnode_stepno NUMBER;
  subnodes_found NUMBER := 0;

  -- Bug 9862326 
  l_start_string           NUMBER;
  l_end_string             NUMBER;
  l_standard_delay         NUMBER;
  l_standard_delay_string  VARCHAR2(4000);
  
  -- Bug 10226452
  l_standard_delay_hold    NUMBER;
  l_dep_type               NUMBER;
  l_batchstep_no           NUMBER;   
  l_dep_step_id            NUMBER;   
      
  -- Bug 9862326 - This cursor will fetch all delays related to a given step including dependent steps.
  CURSOR cur_get_step_delay (v_batch_id NUMBER, v_batchstep_id NUMBER) IS
         SELECT REPLACE(sys_connect_by_path(dep_type||';'||d.dep_step_id||';'||standard_delay||';',' '), ' ', NULL) branch
           FROM (SELECT * FROM gme_batch_step_dependencies WHERE batch_id = v_batch_id) d,
                (SELECT * FROM gme_batch_steps WHERE batch_id = v_batch_id) s,
                (SELECT * FROM gme_batch_steps WHERE batch_id = v_batch_id) p
          WHERE s.batchstep_id = d.batchstep_id
                AND p.batchstep_id(+) = d.dep_step_id
                AND connect_by_isleaf = 1
                START WITH d.batchstep_id = v_batchstep_id		
                CONNECT BY d.batchstep_id = PRIOR d.dep_step_id;

  -- Bug 10226452
  CURSOR cur_get_step_no (v_batch_id NUMBER, v_batchstep_id NUMBER) IS
         SELECT batchstep_no
           FROM gme_batch_steps 
          WHERE batchstep_id = v_batchstep_id;                
BEGIN

  IF (NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT) THEN
     gme_debug.put_line ('entering get_longest_in_branch with batchstep_id '||node);
  END IF;

  -- Bug 25246717/25241112 - Add batch_id join to Improve performance of query. 
  for curnode in (select d.dep_step_id, dep_type, standard_delay, s.batchstep_no, s.batch_id 
                  from gme_batch_step_dependencies d, gme_batch_steps s
                  where s.batchstep_id = d.batchstep_id 
                    and s.batch_id = d.batch_id
                    and d.batchstep_id = node)
  loop
     /* initialize step duration */
     subnodes_found := 1;
     IF (NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT) THEN
        gme_debug.put_line ('entering the loop');
        gme_debug.put_line ('batchstep_no is '||curnode.batchstep_no);
        gme_debug.put_line ('batch_id is '||curnode.batch_id);
        gme_debug.put_line ('duration is '||l_step_duration_tab (curnode.batchstep_no));
        gme_debug.put_line ('delay is '||curnode.standard_delay);
        gme_debug.put_line ('dep_type is '||curnode.dep_type);
     END IF;

     -- This calculation account for duration and delay of the current step.
     StepDuration := l_step_duration_tab (curnode.batchstep_no) + curnode.standard_delay;
     
     IF (curnode.dep_type = 1) THEN -- Start to Start
        IF (NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT) THEN
          gme_debug.put_line ('start to start...');
          gme_debug.put_line ('get all dependent delays in a string');
        END IF;

        -- Bug 9862326 - Must account for delay of dependent steps due to inheritance.
        OPEN cur_get_step_delay(curnode.batch_id, curnode.dep_step_id);
        FETCH cur_get_step_delay INTO l_standard_delay_string;

        -- Bug 10226452 - Restructure this code to loop thru all branches fetched.
        l_standard_delay_hold := 0;

        WHILE cur_get_step_delay%FOUND LOOP
           IF (NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT) THEN
             gme_debug.put_line ('delay string is '||l_standard_delay_string);
           END IF;
           
           -- Sum up all dependent delays for the current step by extrapolating values from string.
           l_standard_delay := 0;
           l_start_string := 1;
           WHILE l_start_string <= LENGTH(l_standard_delay_string) LOOP
              -- Get the dep type
              l_end_string := instr(l_standard_delay_string, ';', l_start_string);
              l_dep_type := SUBSTR(l_standard_delay_string, l_start_string, l_end_string - l_start_string);
              l_start_string := l_end_string + 1;
              
              -- Get the batch step no
              l_end_string := instr(l_standard_delay_string, ';', l_start_string);
              l_dep_step_id := SUBSTR(l_standard_delay_string, l_start_string, l_end_string - l_start_string);
              l_start_string := l_end_string + 1;
              
              -- Get the standard delay and add it in. This value is always added regardless of dep type.
              l_end_string := instr(l_standard_delay_string, ';', l_start_string);              
              l_standard_delay := l_standard_delay + SUBSTR(l_standard_delay_string, l_start_string, l_end_string - l_start_string);
              l_start_string := l_end_string + 1;
              
              -- If it's a finish to start we need to add in duration of the dependent step also.
              IF (l_dep_type = 0) THEN
                 -- We need to get the batch step no because the table is indexed by step no.
                 OPEN cur_get_step_no(curnode.batch_id, l_dep_step_id);
                 FETCH cur_get_step_no INTO l_batchstep_no;
                 CLOSE cur_get_step_no;              
                 
                 l_standard_delay := l_standard_delay + l_step_duration_tab (l_batchstep_no);
              END IF;
              
           END LOOP;    /* WHILE l_start_string IS NOT NULL */

           IF (NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT) THEN
              gme_debug.put_line ('aggregate dependent delay is '||l_standard_delay);
              gme_debug.put_line ('HOLD delay is '||l_standard_delay_hold);
           END IF;
           
           IF l_standard_delay > l_standard_delay_hold THEN
              l_standard_delay_hold := l_standard_delay;
           END IF;   
                      
           FETCH cur_get_step_delay INTO l_standard_delay_string;                   
        END LOOP;    /* WHILE l_start_string IS NOT NULL */
        
        CLOSE cur_get_step_delay;

        StepDuration := StepDuration + l_standard_delay_hold;
        
        x := get_longest_in_branch(curnode.dep_step_id,l_step_duration_tab);
        IF (x > StepDuration) THEN
          longi := x;
        ELSE
          longi := StepDuration;
        END IF;
               
        IF (NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT) THEN
          gme_debug.put_line (
                                'step duration replaced with dep step duration... '
                             || longi
                          );
        END IF;
                 
        IF (NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT) THEN
          gme_debug.put_line ('done start to start...');
        END IF;
     ELSE -- dep_type = 0... thatn's an end to start, so add this in...
        IF (NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT) THEN
          gme_debug.put_line ('end to start...');
        END IF;
     
        longi := StepDuration + get_longest_in_branch(curnode.dep_step_id,l_step_duration_tab);
        IF (NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT) THEN
          gme_debug.put_line (
                            'added duration of dep step and the resulted is'||longi);
        END IF;
     END IF;
     if ( longi > long_in_branch) then
        long_in_branch := longi;
     end if;
  end loop;
  IF subnodes_found = 0 THEN
    select batchstep_no into leaf_subnode_stepno
    from gme_batch_steps
    where batchstep_id = node;
    long_in_branch := l_step_duration_tab(leaf_subnode_stepno);
  END IF;
   IF (NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT) THEN
         gme_debug.put_line ('Weight for node'||node||'-->'||long_in_branch);
   END IF;
    return long_in_branch;
END get_longest_in_branch;

   /*======================================================================
   --  PROCEDURE :
   --   calc_step_qty
   --
   --  DESCRIPTION:
   --    This PL/SQL procedure  is responsible for scaling step
   --    quantities.
   --    This procedure is a modification of GMD_AUTO_STEP_CALC.calc_step_qty.
   --    The GMD_AUTO_STEP_CALC package will be used whereever possible for
   --    underlying routines so as not to duplicate code unnecessarily.
   --    This procedure will keep as close to the original as possible.
   --    This will always be called from a batch, but the parm to specify this
   --    will be kept just in case...
   --
   --  REQUIREMENTS
   --    p_parent_id non null value.
   --    p_step_tbl  non null value.
   --  SYNOPSIS:
   --    calc_step_qty (426, X_step_tbl, X_msg_count, X_msg_stack, X_return_status, 0);
   --
   --
   --===================================================================== */
   PROCEDURE calc_step_qty (
      p_parent_id           IN              NUMBER
     ,p_step_tbl            OUT NOCOPY      gmd_auto_step_calc.step_rec_tbl
     ,p_return_status       OUT NOCOPY      VARCHAR2
     ,p_called_from_batch   IN              NUMBER DEFAULT 1)
   IS
      x_step_rows                      NUMBER;
      x_routing_id                     gme_batch_header.routing_id%TYPE;
      x_step_qty                       NUMBER;
      x_new_factor                     NUMBER;
      x_uom_class                      mtl_units_of_measure.uom_class%TYPE;
      x_scale_factor                   NUMBER;
      x_return_status                  VARCHAR2 (1);
      l_api_name              CONSTANT VARCHAR2 (30)       := 'Calc step qty';

      CURSOR cur_get_std_factor (v_uom_code VARCHAR2)
      IS
         SELECT a.conversion_rate, b.uom_class
           FROM mtl_uom_conversions a, mtl_units_of_measure b
          WHERE a.uom_code = b.uom_code
            AND a.inventory_item_id = 0
            AND b.uom_code = v_uom_code;

      CURSOR cur_get_std_um (v_uom_class VARCHAR2)
      IS
         SELECT uom_code
           FROM mtl_units_of_measure
          WHERE uom_class = v_uom_class AND base_uom_flag = 'Y';

      load_steps_failed                EXCEPTION;
      error_calculating_scale_factor   EXCEPTION;
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      p_return_status := fnd_api.g_ret_sts_success;
      /* Load the steps based into the PL/SQL table P_step_tbl based on the dependencies */
      gmd_auto_step_calc.load_steps (p_parent_id
                                    ,p_called_from_batch
                                    ,NULL
                                    ,p_step_tbl
                                    ,x_routing_id
                                    ,x_return_status);

      IF x_return_status <> p_return_status THEN
         RAISE load_steps_failed;
      END IF;

      IF (gme_common_pvt.g_mass_um_type IS NOT NULL) THEN
         OPEN cur_get_std_um (gme_common_pvt.g_mass_um_type);

         FETCH cur_get_std_um
          INTO gmd_auto_step_calc.g_mass_std_um;

         CLOSE cur_get_std_um;
      END IF;

      IF (gme_common_pvt.g_volume_um_type IS NOT NULL) THEN
         OPEN cur_get_std_um (gme_common_pvt.g_volume_um_type);

         FETCH cur_get_std_um
          INTO gmd_auto_step_calc.g_vol_std_um;

         CLOSE cur_get_std_um;
      END IF;

      x_scale_factor := gme_common_pvt.g_routing_scale_factor;
      x_step_rows := p_step_tbl.COUNT;

      /* Calculate the step quantities for all the rows in X_step_tbl */
      FOR i IN 1 .. x_step_rows LOOP
         SELECT plan_step_qty
           INTO x_step_qty
           FROM gme_batch_steps
          WHERE batchstep_no = p_step_tbl (i).step_no
            AND batch_id = p_parent_id;

         x_step_qty := x_step_qty * x_scale_factor;
         p_step_tbl (i).step_qty := x_step_qty;

         /* Get the std factor and UOM type for the step_qty  */
         OPEN cur_get_std_factor (p_step_tbl (i).step_qty_uom);

         FETCH cur_get_std_factor
          INTO x_new_factor, x_uom_class;

         CLOSE cur_get_std_factor;

         IF x_uom_class = gme_common_pvt.g_mass_um_type THEN
            p_step_tbl (i).step_mass_qty := x_step_qty * x_new_factor;
            p_step_tbl (i).step_mass_uom := gmd_auto_step_calc.g_mass_std_um;
         ELSIF x_uom_class = gme_common_pvt.g_volume_um_type THEN
            p_step_tbl (i).step_vol_qty := x_step_qty * x_new_factor;
            p_step_tbl (i).step_vol_uom := gmd_auto_step_calc.g_vol_std_um;
         END IF;
      END LOOP;                                    /*FOR i IN 1..X_step_rows*/

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;
   EXCEPTION
      WHEN load_steps_failed THEN
         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line ('GME auto step calc -- load steps failed');
         END IF;

         p_return_status := fnd_api.g_ret_sts_error;
      WHEN error_calculating_scale_factor THEN
         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line
                             ('GME_auto step calc -- error calc scale factor');
         END IF;

         p_return_status := fnd_api.g_ret_sts_error;
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
   END calc_step_qty;

   PROCEDURE copy_and_create_text (
      p_gmd_text_code   IN              NUMBER
     ,p_text_string     IN              gme_text_table.text%TYPE
     ,x_gme_text_code   OUT NOCOPY      NUMBER
     ,x_return_status   OUT NOCOPY      VARCHAR2)
   IS
      l_number_of_text_lines   NUMBER;
      l_return                 BOOLEAN;
      l_text_header            gme_text_header%ROWTYPE;
      l_text_table             gme_common_pvt.text_tab;
      l_api_name      CONSTANT VARCHAR2 (30)        := 'copy and create text';
      unexpected_error         EXCEPTION;

      CURSOR fm_text_tbl_cursor (p_text_code fm_text_tbl.text_code%TYPE)
      IS
         SELECT   *
             FROM fm_text_tbl
            WHERE text_code = p_text_code
         ORDER BY line_no;
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
         gme_debug.put_line ('BEGIN copy and create text for '
                             || p_text_string);
      END IF;

      x_return_status := fnd_api.g_ret_sts_success;
      l_text_table.DELETE;
      l_number_of_text_lines := 0;
      l_return :=
                 gme_text_dbl.insert_header_row (l_text_header, l_text_header);

      IF (l_return) THEN
         x_gme_text_code := l_text_header.text_code;

         -- This fetches using the fm text data based on the fm text code.
         FOR l_fm_text_tbl_row IN fm_text_tbl_cursor (p_gmd_text_code) LOOP
            l_number_of_text_lines := l_number_of_text_lines + 1;
            l_text_table (l_number_of_text_lines).text_code :=
                                                              x_gme_text_code;
            l_text_table (l_number_of_text_lines).line_no :=
                                                    l_fm_text_tbl_row.line_no;
            l_text_table (l_number_of_text_lines).lang_code :=
                                                  l_fm_text_tbl_row.lang_code;
            l_text_table (l_number_of_text_lines).paragraph_code :=
                                             l_fm_text_tbl_row.paragraph_code;
            l_text_table (l_number_of_text_lines).sub_paracode :=
                                               l_fm_text_tbl_row.sub_paracode;

            IF (l_fm_text_tbl_row.line_no = -1) THEN
               l_text_table (l_number_of_text_lines).text := p_text_string;
            ELSE
               l_text_table (l_number_of_text_lines).text :=
                                                       l_fm_text_tbl_row.text;

               IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
                  gme_debug.put_line
                                   (   'text for line no '
                                    || l_fm_text_tbl_row.line_no
                                    || ' is '
                                    || l_text_table (l_number_of_text_lines).text);
               END IF;
            END IF;
         END LOOP;
           /* FOR l_fm_text_tbl_row in fm_text_tbl_cursor(l_text_code) LOOP */
      ELSE
         -- We could not insert the text header. Error message pushed on stack in dbl code.
         RAISE unexpected_error;
      END IF;

      -- Insert Text
      FOR l_row_count IN 1 .. l_text_table.COUNT LOOP
         l_return :=
            gme_text_dbl.insert_text_row (l_text_table (l_row_count)
                                         ,l_text_table (l_row_count) );

         IF NOT l_return THEN
            IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
               gme_debug.put_line (   'trouble inserting text for '
                                   || p_text_string);
            END IF;

            -- We could not insert the text info. Error message pushed on stack in dbl code.
            RAISE unexpected_error;
         END IF;

         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line ('inserted text for ' || p_text_string);
         END IF;
      END LOOP;               /* FOR l_row_count IN 1 .. l_text_table.COUNT */

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;
   EXCEPTION
      WHEN unexpected_error THEN
         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line
               ('ERROR - unexpected error in copy and create text while inserting header or dtl');
         END IF;

         x_return_status := fnd_api.g_ret_sts_unexp_error;
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
   END copy_and_create_text;

   PROCEDURE get_usage_in_hours (
      p_plan_rsrc_usage   IN              gme_batch_step_resources.plan_rsrc_usage%TYPE
     ,p_usage_um          IN              gme_batch_step_resources.usage_um%TYPE
     ,x_usage_hrs         OUT NOCOPY      gme_batch_step_resources.plan_rsrc_usage%TYPE
     ,x_return_status     OUT NOCOPY      VARCHAR2)
   IS
      l_hour_um                mtl_units_of_measure.uom_code%TYPE;
      l_hour_uom_class         mtl_units_of_measure.uom_class%TYPE;
      l_hour_std_factor        NUMBER;
      l_usage_uom_class        mtl_units_of_measure.uom_class%TYPE;
      l_usage_std_factor       NUMBER;
      missing_profile_option   EXCEPTION;
      l_api_name      CONSTANT VARCHAR2 (30)          := 'GET_USAGE_IN_HOURS';

      CURSOR get_um_type_and_factor (v_uom_code VARCHAR2)
      IS
         SELECT b.uom_class, a.conversion_rate
           FROM mtl_uom_conversions a, mtl_units_of_measure b
          WHERE a.uom_code = b.uom_code
            AND a.inventory_item_id = 0
            AND b.uom_code = v_uom_code;
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      x_return_status := fnd_api.g_ret_sts_success;

      -- Bug 14769220 - Initialize uom global for this code to work correctly.
      IF NOT gme_common_pvt.g_setup_done THEN
         gme_common_pvt.g_hour_uom_code := fnd_profile.VALUE ('BOM:HOUR_UOM_CODE');
      END IF;
      
      x_usage_hrs := 0;
      l_hour_um := gme_common_pvt.g_hour_uom_code;

      IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
	  gme_debug.put_line(g_pkg_name||'.'||l_api_name||': l_hour_um :'|| l_hour_um); 
	  gme_debug.put_line(g_pkg_name||'.'||l_api_name||': p_plan_rsrc_usage :'|| p_plan_rsrc_usage); 
	  gme_debug.put_line(g_pkg_name||'.'||l_api_name||': p_usage_um :'|| p_usage_um); 	  
      END IF;

      IF l_hour_um = p_usage_um THEN
         x_usage_hrs := p_plan_rsrc_usage;
      ELSE                    /* we have to calculate the usage in hours... */
         OPEN get_um_type_and_factor (l_hour_um);

         FETCH get_um_type_and_factor
          INTO l_hour_uom_class, l_hour_std_factor;

         IF get_um_type_and_factor%NOTFOUND THEN
            x_return_status := fnd_api.g_ret_sts_error;
         END IF;

         CLOSE get_um_type_and_factor;

         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
	     gme_debug.put_line(g_pkg_name||'.'||l_api_name||': l_hour_uom_class :'|| l_hour_uom_class); 
	     gme_debug.put_line(g_pkg_name||'.'||l_api_name||': l_hour_std_factor :'|| l_hour_std_factor); 
         END IF;

         IF x_return_status = fnd_api.g_ret_sts_success THEN
            OPEN get_um_type_and_factor (p_usage_um);

            FETCH get_um_type_and_factor
             INTO l_usage_uom_class, l_usage_std_factor;

            IF get_um_type_and_factor%NOTFOUND THEN
               x_return_status := fnd_api.g_ret_sts_error;
            END IF;

            CLOSE get_um_type_and_factor;

            IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
	        gme_debug.put_line(g_pkg_name||'.'||l_api_name||': l_usage_uom_class :'|| l_usage_uom_class); 
	        gme_debug.put_line(g_pkg_name||'.'||l_api_name||': l_usage_std_factor :'|| l_usage_std_factor); 
            END IF;

            IF x_return_status = fnd_api.g_ret_sts_success THEN
               IF l_usage_uom_class <> l_hour_uom_class THEN
                  x_return_status := fnd_api.g_ret_sts_error;
               END IF;
            END IF;
         END IF;

         IF x_return_status = fnd_api.g_ret_sts_success THEN
            x_usage_hrs :=
                   p_plan_rsrc_usage * l_usage_std_factor / l_hour_std_factor;
         END IF;
      END IF;

      IF g_debug <= gme_debug.g_log_procedure THEN
	 gme_debug.put_line(g_pkg_name||'.'||l_api_name||': x_usage_hrs :'|| x_usage_hrs); 
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
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
         x_return_status := fnd_api.g_ret_sts_unexp_error;
   END get_usage_in_hours;

   FUNCTION get_max_duration (v_step_id IN NUMBER, v_batch_id IN NUMBER)
      RETURN NUMBER
   IS
      CURSOR cur_get_activity (v_step_id NUMBER, v_batch_id NUMBER)
      IS
         SELECT batchstep_activity_id, offset_interval, batchstep_id
               ,batch_id
           FROM gme_batch_step_activities
          WHERE batchstep_id = v_step_id AND batch_id = v_batch_id;

      -- Bug 16297308 - fetch actuals also in case this is a reschedule of a wip step.
      CURSOR cur_get_resource (
         v_step_id       NUMBER
        ,v_batch_id      NUMBER
        ,v_activity_id   NUMBER)
      IS
         SELECT batchstep_resource_id, offset_interval, plan_rsrc_usage
               ,plan_rsrc_count, usage_um, actual_rsrc_usage
           FROM gme_batch_step_resources
          WHERE batchstep_id = v_step_id
            AND batch_id = v_batch_id
            AND batchstep_activity_id = v_activity_id;

      l_hour_um                mtl_units_of_measure.uom_code%TYPE;
      l_rsrc_duration          NUMBER;
      l_act_duration           NUMBER;
      l_max_rsrc_duration      NUMBER;
      l_max_act_duration       NUMBER;
      x_usage_hour             gme_batch_step_resources.plan_rsrc_usage%TYPE;
      l_usage_hrs              gme_batch_step_resources.plan_rsrc_usage%TYPE;
      l_return_status          VARCHAR2 (1);
      missing_profile_option   EXCEPTION;
      l_api_name      CONSTANT VARCHAR2 (30)             := 'get_max_duration';
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      l_hour_um := gme_common_pvt.g_hour_uom_code;

      FOR step_activity IN cur_get_activity (v_step_id, v_batch_id) LOOP
         l_max_rsrc_duration := 0;

         FOR step_resource IN
            cur_get_resource (step_activity.batchstep_id
                             ,step_activity.batch_id
                             ,step_activity.batchstep_activity_id) LOOP

            IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
               gme_debug.put_line ('step_resource.batchstep_resource_id is '|| step_resource.batchstep_resource_id);
               gme_debug.put_line ('step_resource.plan_rsrc_usage is '|| step_resource.plan_rsrc_usage);
               gme_debug.put_line ('step_resource.actual_rsrc_usage is '|| NVL(step_resource.actual_rsrc_usage, 0));
            END IF;
                             
             -- Bug 16297308 - Use actuals also.
            IF step_resource.usage_um = l_hour_um THEN
               x_usage_hour := step_resource.plan_rsrc_usage - NVL(step_resource.actual_rsrc_usage, 0);
            ELSE
               get_usage_in_hours (step_resource.plan_rsrc_usage - NVL(step_resource.actual_rsrc_usage, 0)
                                  ,step_resource.usage_um
                                  ,l_usage_hrs
                                  ,l_return_status);

               IF l_return_status = fnd_api.g_ret_sts_success THEN
                  x_usage_hour := l_usage_hrs;
               ELSE
                  x_usage_hour := 0;
               END IF;
            END IF;

            l_rsrc_duration :=
                 step_resource.offset_interval
               + (x_usage_hour / step_resource.plan_rsrc_count);

            IF l_rsrc_duration > NVL (l_max_rsrc_duration, -1) THEN
               l_max_rsrc_duration := l_rsrc_duration;
            END IF;
         END LOOP;

         l_act_duration := step_activity.offset_interval + l_max_rsrc_duration;

         IF l_act_duration > NVL (l_max_act_duration, -1) THEN
            l_max_act_duration := l_act_duration;
         END IF;
      END LOOP;

      IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
         gme_debug.put_line ('l_max_act_duration ' || l_max_act_duration);
      END IF;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;

      RETURN l_max_act_duration;
   EXCEPTION
      WHEN missing_profile_option THEN
         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line (l_api_name
                                || ' ERROR - missing_profile_option');
         END IF;

         l_max_act_duration := 0;
         RETURN 0;
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
         RETURN 0;
   END get_max_duration;

   PROCEDURE insert_resource_txns (
      p_gme_batch_header_rec       IN              gme_batch_header%ROWTYPE
     ,p_doc_type                   IN              VARCHAR2
     ,p_batch_step_resources_rec   IN              gme_batch_step_resources%ROWTYPE
     ,p_trans_count                IN              NUMBER
     ,x_return_status              OUT NOCOPY      VARCHAR2)
   IS
      l_gme_resource_txns      gme_resource_txns%ROWTYPE;
      l_txn_usage              NUMBER;
      l_usage_time             NUMBER;
      l_hour_um                mtl_units_of_measure.uom_code%TYPE;
      l_api_name      CONSTANT VARCHAR2 (30)        := 'insert_resource_txns';
      error_insert_res_txns    EXCEPTION;
      missing_profile_option   EXCEPTION;
      --FPBug#4395561
      create_flex_failure      EXCEPTION;  
      l_return_status          VARCHAR2(1);
	  l_rsrc_ins_count		   NUMBER;	
	  l_default_ins_id		   NUMBER;
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      l_usage_time :=
           (  p_batch_step_resources_rec.plan_cmplt_date
            - p_batch_step_resources_rec.plan_start_date)
         * 24;
      l_hour_um := gme_common_pvt.g_hour_uom_code;
      l_txn_usage :=
         inv_convert.inv_um_convert
                              (item_id            => 0
                              ,PRECISION          => 5
                              ,from_quantity      => l_usage_time
                              ,from_unit          => l_hour_um
                              ,to_unit            => p_batch_step_resources_rec.usage_um
                              ,from_name          => NULL
                              ,to_name            => NULL);

      -- Bug 9212573 - Add '=' to usage condition and new p_trans_count condition from 11.5.10.
      IF (l_txn_usage <= 0 OR NVL(p_trans_count, 1) = 1) THEN
         l_txn_usage :=
              p_batch_step_resources_rec.plan_rsrc_usage
            / p_batch_step_resources_rec.plan_rsrc_count;
      END IF;

      IF p_gme_batch_header_rec.update_inventory_ind = 'Y' THEN
         l_gme_resource_txns.doc_id := p_batch_step_resources_rec.batch_id;
         l_gme_resource_txns.doc_type := p_doc_type;
         l_gme_resource_txns.organization_id :=
                                       p_gme_batch_header_rec.organization_id;
         l_gme_resource_txns.line_type := 0;
         l_gme_resource_txns.line_id :=
                             p_batch_step_resources_rec.batchstep_resource_id;
         l_gme_resource_txns.resources :=
                                         p_batch_step_resources_rec.resources;
         l_gme_resource_txns.resource_usage := ROUND (l_txn_usage, 32);
         l_gme_resource_txns.trans_qty_um :=
                                          p_batch_step_resources_rec.usage_um;
         l_gme_resource_txns.trans_date :=
                                   p_batch_step_resources_rec.plan_start_date;
         l_gme_resource_txns.completed_ind := 0;
         l_gme_resource_txns.posted_ind := 0;
         l_gme_resource_txns.overrided_protected_ind := 'N';
         l_gme_resource_txns.start_date :=
                                   p_batch_step_resources_rec.plan_start_date;
         l_gme_resource_txns.end_date :=
                                   p_batch_step_resources_rec.plan_cmplt_date;
         l_gme_resource_txns.delete_mark := 0;

		 IF(p_batch_step_resources_rec.plan_rsrc_count = 1) THEN
			SELECT count(*)
			INTO l_rsrc_ins_count
				FROM gmp_resource_instances i, cr_rsrc_dtl r
				WHERE r.resource_id = i.resource_id
				AND r.resources = l_gme_resource_txns.resources;
			IF(l_rsrc_ins_count = 1) THEN
				SELECT instance_id
				INTO l_default_ins_id
					FROM gmp_resource_instances i, cr_rsrc_dtl r
					WHERE r.resource_id = i.resource_id
					AND r.resources = l_gme_resource_txns.resources;
					
				l_gme_resource_txns.instance_id  := l_default_ins_id;
			END IF;
		 END IF;
		 
         FOR i IN 1 .. p_batch_step_resources_rec.plan_rsrc_count LOOP
            IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
               gme_debug.put_line (   g_pkg_name
                                   || '.'
                                   || l_api_name
                                   || ' '
                                   || 'resource transaction # '
                                   || i
                                   || ' end date '
                                   || TO_CHAR (l_gme_resource_txns.end_date
                                              ,'DD-MON-YYYY HH24:MI:SS') );
            END IF;

/* once api is known will add this  GMF_PeriodClose_PUB.Verify_PeriodClose
 IF NOT gme_api_grp.close_period_check_flexible(p_tran_rec     => p_tran_rec,
                                                x_tran_rec     => l_tran_rec_out) THEN
   RAISE FND_API.g_exc_error;
 END IF;

        l_gme_resource_txns.trans_date := l_tran_rec_out.trans_date;
*/

             --FPBug#4395561 Start 
             /*call create flex procedure to insert the default values of the GME_RSRC_TXN_FLEX
               DFF's segments if they are enabled */
             l_return_status:=NULL;
             gme_validate_flex_fld_pvt.create_flex_resource_txns (
                                       l_gme_resource_txns,
                                       l_gme_resource_txns,
                                       l_return_status);
              IF l_return_status <> FND_API.g_ret_sts_success THEN
                RAISE create_flex_failure;
              END IF;
              --FPBug#4395561 End

            IF (gme_resource_txns_dbl.insert_row
                                      (p_resource_txns      => l_gme_resource_txns
                                      ,x_resource_txns      => l_gme_resource_txns) ) THEN
               NULL;
            ELSE
               RAISE error_insert_res_txns;
            END IF;
         END LOOP;
      END IF;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;
   EXCEPTION
      WHEN error_insert_res_txns THEN
         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line ('insert resource txns error');
         END IF;

         --add error message handling by michael tou 2013-10-10
         IF SQLCODE=0 THEN 
            gme_common_pvt.log_message('GME_API_RSRC_TRAN_INS_ERR');
         ELSE 
            fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name); 
         END IF; 

         x_return_status := fnd_api.g_ret_sts_error;
      WHEN missing_profile_option THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN fnd_api.g_exc_error THEN
         x_return_status := fnd_api.g_ret_sts_error;
      --FPBug#4395561
      WHEN create_flex_failure THEN
         x_return_status := l_return_status;
         IF (NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT) THEN
            gme_debug.put_line ('Creating the default values of the DFF failure');
         END IF;
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
   END insert_resource_txns;
END gme_create_step_pvt;
/

COMMIT ;
EXIT;
--show errors;
