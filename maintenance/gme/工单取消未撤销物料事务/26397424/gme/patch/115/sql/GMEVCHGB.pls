/* +======================================================================+ */
/* |    Copyright (c) 2005, 2016 Oracle and/or its affiliates.           | */
/* |                         All rights reserved.                         | */
/* |                           Version 12.0.0                             | */
/* +======================================================================+ */
REM dbdrv: sql ~PROD ~PATH ~FILE none none none package &phase=plb \
REM dbdrv: checkfile(120.1.12010000.3=120.2.12020000.2)(120.1.12000000.2=120.1.12010000.2)(115.7=120.1):~PROD:~PATH:~FILE
SET VERIFY OFF;
WHENEVER SQLERROR EXIT FAILURE ROLLBACK;
WHENEVER OSERROR EXIT FAILURE ROLLBACK;
REM *******************************************************************
REM * FILE:    GMEVCHGB.pls                                              *
REM * PURPOSE: Package Body for the GME_BATCH_STEP_CHG_PVT package       *
REM * AUTHOR:  Rishi Varma, OPM Development                              *
REM * DATE:    May 10,2004                                               *
REM * HISTORY:                                                           *
REM * ========                                                           *
REM * 10May04  Rishi Varma  3307549                                      *
REM *	       Created 						                                  *
REM * Punit Kumar 25 Mar 2005 Convergence changes                        *
REM *
REM * G. Muratore    24-JUL-2009  Bug 8708957 
REM *    Change query to derive sequence dependency ids properly.
REM *    Prior to change, code was still joining to ic_item_mst incorrectly
REM *    to fetch category_id. Now it is joining to mtl_system_items and
REM *    some additional tables to get the correct data.
REM *    PROCEDURE:  set_sequence_dependent_id
REM *
REM * G. Muratore    16-DEC-2016  Bug 25246717/25241112 
REM *    Change query to perform faster by using batch_id or restructure the query.
REM *    PROCEDURE:  set_sequence_dependent_id and set_all_batch_sequences
REM ********************************************************************

/*************************************************************************
* This file contains the procedures for private procedures of the gme_batch
* _step_charges table.
*************************************************************************/

CREATE OR REPLACE PACKAGE BODY gme_batch_step_chg_pvt AS
/* $Header: GMEVCHGB.pls 120.2.12020000.2 2016/12/16 16:13:06 gmurator ship $*/
   g_pkg_name   CONSTANT VARCHAR2 (30) := 'gme_batch_step_chg_pvt';
   g_debug               VARCHAR2 (5)  := fnd_profile.VALUE ('AFLOG_LEVEL');

/*===========================================================================================
   Procedure
      Set_activity_sequence_num
   Description
     This procedure is used to assign the sequence numbers to all the activities of a batch.
   Parameters
     pbatch_id                    the batch whose activities are to be numbered.
 =============================================================================================*/
   PROCEDURE set_activity_sequence_num (pbatch_id IN NUMBER)
   IS
      l_api_name   CONSTANT VARCHAR2 (30) := 'set_activity_sequence_num';

      CURSOR get_activities (v_batch_id NUMBER)
      IS
         SELECT   gsa.batchstep_id
                 ,DECODE (gsa.sequence_dependent_ind, 1, 1, 2) seq_dep_order
                 ,gsa.offset_interval, gsa.activity
                 ,gsa.batchstep_activity_id
                 ,NVL (gsa.sequence_dependent_ind, -1)
                                                      sequence_dependent_ind
             FROM gme_batch_step_activities gsa
            WHERE gsa.batch_id = v_batch_id
         ORDER BY 1, 2, 3, 4, 5;

      v_batch_id            NUMBER        := 0;

      TYPE act_rec IS RECORD (
         batchstep_id            gme_batch_step_activities.batchstep_id%TYPE
        ,seq_dep_order           NUMBER
        ,offset_interval         gme_batch_step_activities.offset_interval%TYPE
        ,activity                gme_batch_step_activities.activity%TYPE
        ,batchstep_activity_id   gme_batch_step_activities.batchstep_activity_id%TYPE
        ,seq_dep_ind             gme_batch_step_activities.sequence_dependent_ind%TYPE
      );

      TYPE act_tbl IS TABLE OF act_rec
         INDEX BY BINARY_INTEGER;

      act_tab               act_tbl;
      act_cnt               NUMBER        := 1;
      seq_num               NUMBER        := 0;
      err_num               NUMBER        := 0;
      found_seq             NUMBER        := 0;
      old_step              NUMBER        := -1;
      lower_seq             NUMBER        := 0;
      upper_seq             NUMBER        := 0;
      x                     NUMBER        := 0;
      y                     NUMBER        := 0;
   BEGIN
/* the seq num will always start at 10 for the first activity after the
   seq dep activity. */
      seq_num := 10;
      old_step := -1;
      v_batch_id := pbatch_id;

      OPEN get_activities (v_batch_id);

      /* select all of the activities for the batch into the table check on the way
         if there was any numbering done already. */
      LOOP
         FETCH get_activities
          INTO act_tab (act_cnt);

         EXIT WHEN get_activities%NOTFOUND;

         IF act_tab (act_cnt).seq_dep_ind > 1 THEN
            found_seq := 1;
         END IF;

         act_cnt := act_cnt + 1;
      END LOOP;

      act_cnt := act_cnt - 1;

      /* loop through the activities to number them if needed */
      FOR i IN 1 .. act_cnt LOOP
         /* if the step changes we can reset the seq num to 100 */
         IF old_step <> act_tab (i).batchstep_id THEN
            seq_num := 100;
         END IF;

         /* this batch has not been numbered so we can just simply number each
            activity incrementing by 100 after each write  */
         IF found_seq = 0 THEN
            /* if the row is the seq dep row it has to be first so we can skip it as the
               number in seq dep ind is already correct */
            IF act_tab (i).seq_dep_ind = 1 THEN
               NULL;
            ELSE
               /* set the seq dep ind to the current seq num */
               act_tab (i).seq_dep_ind := seq_num;

               IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
                  gme_debug.put_line
                                 (   g_pkg_name
                                  || '.'
                                  || l_api_name
                                  || 'found_seq = 0,batchstep_activity_id is'
                                  || act_tab (i).batchstep_activity_id);
                  gme_debug.put_line (   g_pkg_name
                                      || '.'
                                      || l_api_name
                                      || 'seq. dep. id is'
                                      || act_tab (i).seq_dep_ind);
               END IF;

               UPDATE gme_batch_step_activities
                  SET sequence_dependent_ind = act_tab (i).seq_dep_ind
                     ,last_update_date = SYSDATE
                     ,last_updated_by = gme_common_pvt.g_user_ident
                     ,last_update_login = gme_common_pvt.g_login_id
                WHERE batchstep_activity_id =
                                             act_tab (i).batchstep_activity_id;

               IF SQL%NOTFOUND THEN
                  err_num := -1;
                  EXIT;
               END IF;

               /* increment the seq num for each processed except the seq dep activity */
               seq_num := seq_num + 100;
            END IF;
         ELSE
       /* this batch has been numbered so we just need to number the rows that
          have not been numbered yet */
            /* if the row is the seq dep row it has to be first so we can skip it as the
               number in seq dep ind is already correct */
            IF act_tab (i).seq_dep_ind = 1 THEN
               NULL;
            ELSE
               /* the activity needs to be numbered */
               IF act_tab (i).seq_dep_ind = -1 THEN
                  /* first we have to located the previous activity in the step if one
                     exists to get its seq number. If not set the lower limit to 0 */
                  x := i - 1;

                  IF x < 1 THEN
                     lower_seq := 0;
                  ELSIF act_tab (x).batchstep_id <> act_tab (i).batchstep_id THEN
                     lower_seq := 0;
                  ELSE
                     lower_seq := act_tab (x).seq_dep_ind;
                  END IF;

                  /* get the next activity in the step for the seq num if one exists.
                     if one does not exist leave the value for upper as -1 */
                  y := i + 1;
                  upper_seq := -1;

                  FOR j IN y .. act_cnt LOOP
                     IF act_tab (j).batchstep_id <> act_tab (i).batchstep_id THEN
                        EXIT;
                     ELSE
                        IF act_tab (j).seq_dep_ind > -1 THEN
                           upper_seq := act_tab (j).seq_dep_ind;
                           EXIT;
                        END IF;
                     END IF;
                  END LOOP;

                  /* if the upper has been set the it means we have to put the activity
                     before an existing numbered activity. take the midpoint between for
                     the new seq num */
                  IF upper_seq > -1 THEN
                     act_tab (i).seq_dep_ind :=
                                      ROUND ( ( (lower_seq + upper_seq) / 2) );
                  ELSE
                     /* if the upper does not exist the we just need to added a new seq num
                        at the normal increment method */
                     act_tab (i).seq_dep_ind := lower_seq + 100;
                  END IF;

                  IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
                     gme_debug.put_line
                                 (   g_pkg_name
                                  || '.'
                                  || l_api_name
                                  || 'found_seq = 1,batchstep_activity_id is'
                                  || act_tab (i).batchstep_activity_id);
                     gme_debug.put_line (   g_pkg_name
                                         || '.'
                                         || l_api_name
                                         || 'seq. dep. id is'
                                         || act_tab (i).seq_dep_ind);
                  END IF;

                  UPDATE gme_batch_step_activities
                     SET sequence_dependent_ind = act_tab (i).seq_dep_ind
                        ,last_update_date = SYSDATE
                        ,last_updated_by = gme_common_pvt.g_user_ident
                        ,last_update_login =
                                    gme_common_pvt.g_login_id
                                                             ----- Punit Kumar
                   WHERE batchstep_activity_id =
                                             act_tab (i).batchstep_activity_id;

                  IF SQL%NOTFOUND THEN
                     err_num := -1;
                     EXIT;
                  END IF;
               END IF;
            END IF;
         END IF;
      END LOOP;

      CLOSE get_activities;
   END set_activity_sequence_num;

    /*===========================================================================================
     Procedure
        Set_all_batch_activities
     Description
       This procedure is used to assign the sequence numbers to the activities of pending,wip
       batchses.
   =============================================================================================*/
   PROCEDURE set_all_batch_activities
   IS
      l_gme_batch_ids_tab   gme_batch_step_chg_pvt.gme_batch_ids_tab;

      /* select only pending or wip batches */
      CURSOR get_batches
      IS
         SELECT gbh.batch_id
           FROM gme_batch_header gbh
          WHERE gbh.batch_status IN (1, 2);

      v_batch_id            NUMBER                                   := 0;
   BEGIN
      OPEN get_batches;

      FETCH get_batches
      BULK COLLECT INTO l_gme_batch_ids_tab;

      CLOSE get_batches;

      FOR i IN 1 .. l_gme_batch_ids_tab.COUNT LOOP
         set_activity_sequence_num (l_gme_batch_ids_tab (i) );
      END LOOP;

      COMMIT;
   END set_all_batch_activities;

    /*===========================================================================================
     Procedure
        Set_sequence_dependent_id
     Description
       This procedure is used to assign the sequence numbers to the resouces by populating the
       sequence_dependent_id field in the gme_batch_Step_resouces table.
     Parameters
       pbatch_id  - This identifies the batch whose resources are to be numbered.
   =============================================================================================*/
   PROCEDURE set_sequence_dependent_id (pbatch_id IN NUMBER)
   IS
/* this query will select all the resources that need to have the setup applied
   the step resource must be defined at the plant and be scheduled to an
   instance There must be an activity defined in the step as sequence dependent.
   All of the resources in the step will have the setup applied, hence there is
   no join through activity. we just need to confirm that sequences dependencies
   will be applied in the step. The sequence type will be found for operation
   specific or global, hence there will be two outer joins */
      l_api_name       CONSTANT VARCHAR2 (30) := 'set_sequence_dependent_id';

       -- Bug 8708957 - Correct cursor to use mtl_system_items and proper joins also.
      CURSOR get_batch_details (v_batch_id NUMBER)
      IS
         SELECT   gsr.batchstep_resource_id, 1 sds_type, gst.seq_dep_id
             FROM gme_batch_header gbh
                 ,gmd_recipe_validity_rules gvr
                 ,gme_batch_steps gbs
                 ,gme_batch_step_activities gsa
                 ,gme_batch_step_resources gsr
                 ,cr_rsrc_dtl crd
                 ,mtl_system_items iim        -- 8708957 - Change this from ic_item_mst to mtl_system_items
                 ,gmp_sequence_types gst
                 -- 8708957 - two more tables added
                 ,mtl_item_categories mic
                 ,mtl_default_category_sets dcs
            WHERE gbh.batch_id = v_batch_id
              AND gbh.recipe_validity_rule_id = gvr.recipe_validity_rule_id
              AND gbh.batch_id = gbs.batch_id
              AND gbh.batch_id = gsa.batch_id -- Bug 25246717/25241112 - Improves performance of query.
              AND gbh.batch_id = gsr.batch_id -- Bug 25246717/25241112 - Improves performance of query.
              AND gbs.batchstep_id = gsr.batchstep_id
              AND gvr.inventory_item_id = iim.inventory_item_id              -- 8708957 - Changed
              AND gbh.organization_id = iim.organization_id                  -- 8708957 - Added
              AND gbs.batchstep_id = gsa.batchstep_id
              AND gsa.sequence_dependent_ind = 1
              AND gbh.organization_id = crd.organization_id
              AND gsr.resources = crd.resources
              AND crd.schedule_ind = 2
              AND gbs.oprn_id = gst.oprn_id
              -- 8708957 - Following lines added                            
              AND mic.CATEGORY_ID = gst.CATEGORY_ID                 
              AND mic.inventory_item_id = iim.inventory_item_id
              AND gbh.organization_id = mic.organization_id              
              AND dcs.CATEGORY_SET_ID = mic.CATEGORY_SET_ID                                                                           
              -- 8708957 - 14 is the seeded value for Sequence Dependency Class.  DO NOT CHANGE !!   
              AND dcs.FUNCTIONAL_AREA_ID = 14                                 
         UNION ALL
         SELECT   gsr2.batchstep_resource_id, 2 sds_type, gst2.seq_dep_id
             FROM gme_batch_header gbh2
                 ,gmd_recipe_validity_rules gvr2
                 ,gme_batch_steps gbs2
                 ,gme_batch_step_activities gsa2
                 ,gme_batch_step_resources gsr2
                 ,cr_rsrc_dtl crd2
                 ,gmp_sequence_types gst2
                 ,mtl_system_items iim2        -- 8708957 - Change this from ic_item_mst to mtl_system_items
                 -- 8708957 - two more tables added
                 ,mtl_item_categories mic2
                 ,mtl_default_category_sets dcs2
            WHERE gbh2.batch_id = v_batch_id
              AND gbh2.recipe_validity_rule_id = gvr2.recipe_validity_rule_id
              AND gbh2.batch_id = gbs2.batch_id
              AND gbh2.batch_id = gsa2.batch_id -- Bug 25246717/25241112 - Improves performance of query.
              AND gbh2.batch_id = gsr2.batch_id -- Bug 25246717/25241112 - Improves performance of query.
              AND gbs2.batchstep_id = gsr2.batchstep_id
              AND gvr2.inventory_item_id = iim2.inventory_item_id              -- 8708957 - Changed
              AND gbh2.organization_id = iim2.organization_id                  -- 8708957 - Added
              AND gbs2.batchstep_id = gsa2.batchstep_id
              AND gsa2.sequence_dependent_ind = 1
              AND gbh2.organization_id = crd2.organization_id
              AND gsr2.resources = crd2.resources
              AND crd2.schedule_ind = 2
              AND -1 = gst2.oprn_id
              -- 8708957 - Following lines added                            
              AND mic2.CATEGORY_ID = gst2.CATEGORY_ID                 
              AND mic2.inventory_item_id = iim2.inventory_item_id
              AND gbh2.organization_id = mic2.organization_id              
              AND dcs2.CATEGORY_SET_ID = mic2.CATEGORY_SET_ID                                                                           
              -- 8708957 - 14 is the seeded value for Sequence Dependency Class.  DO NOT CHANGE !!                                         UNION ALL
              AND dcs2.FUNCTIONAL_AREA_ID = 14                                               
         ORDER BY 1, 2;
         
-- Bug 8708957 - Comment out original cursor which was incorrectly using ic_item_mst
/*
      CURSOR get_batch_details (v_batch_id NUMBER)
      IS
         SELECT   gsr.batchstep_resource_id, 1 sds_type, gst.seq_dep_id
             FROM gme_batch_header gbh
                 ,gmd_recipe_validity_rules gvr
                 ,gme_batch_steps gbs
                 ,gme_batch_step_activities gsa
                 ,gme_batch_step_resources gsr
                 ,cr_rsrc_dtl crd
                 ,ic_item_mst iim
                 ,gmp_sequence_types gst
            WHERE gbh.batch_id = v_batch_id
              AND gbh.recipe_validity_rule_id = gvr.recipe_validity_rule_id
              AND gbh.batch_id = gbs.batch_id
              AND gbs.batchstep_id = gsr.batchstep_id
              AND gvr.item_id = iim.item_id
              AND iim.seq_category_id IS NOT NULL
              AND gbs.batchstep_id = gsa.batchstep_id
              AND gsa.sequence_dependent_ind = 1
              AND gbh.organization_id = crd.organization_id
              AND gsr.resources = crd.resources
              AND crd.schedule_ind = 2
              AND iim.seq_category_id = gst.category_id
              AND gbs.oprn_id = gst.oprn_id
         UNION ALL
         SELECT   gsr2.batchstep_resource_id, 2 sds_type, gst2.seq_dep_id
             FROM gme_batch_header gbh2
                 ,gmd_recipe_validity_rules gvr2
                 ,gme_batch_steps gbs2
                 ,gme_batch_step_activities gsa2
                 ,gme_batch_step_resources gsr2
                 ,cr_rsrc_dtl crd2
                 ,gmp_sequence_types gst2
                 ,ic_item_mst iim2
            WHERE gbh2.batch_id = v_batch_id
              AND gbh2.recipe_validity_rule_id = gvr2.recipe_validity_rule_id
              AND gbh2.batch_id = gbs2.batch_id
              AND gbs2.batchstep_id = gsr2.batchstep_id
              AND gvr2.item_id = iim2.item_id
              AND iim2.seq_category_id IS NOT NULL
              AND gbs2.batchstep_id = gsa2.batchstep_id
              AND gsa2.sequence_dependent_ind = 1
              AND gbh2.organization_id = crd2.organization_id
              AND gsr2.resources = crd2.resources
              AND crd2.schedule_ind = 2
              AND -1 = gst2.oprn_id
              AND iim2.seq_category_id = gst2.category_id
         ORDER BY 1, 2;
*/
      v_batch_id                NUMBER        := 0;
      v_batchstep_resource_id   NUMBER        := 0;
      v_sds_type                NUMBER        := 0;
      v_seq_dep_id              NUMBER        := 0;
      last_res                  NUMBER        := 0;
   BEGIN
      v_batch_id := pbatch_id;
      last_res := -1;

      OPEN get_batch_details (v_batch_id);

      LOOP
         FETCH get_batch_details
          INTO v_batchstep_resource_id, v_sds_type, v_seq_dep_id;

         EXIT WHEN get_batch_details%NOTFOUND;

         /* if the operation specific type exists use that one */
         /* if the operation specific does not exist use the global */
         /* the operation or global existed apply it to the resource otherwise
            do not update the row and go one to the next row */
         IF    (v_sds_type = 1)
            OR (v_sds_type = 2 AND last_res <> v_batchstep_resource_id) THEN
            last_res := v_batchstep_resource_id;

            IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
               gme_debug.put_line (   g_pkg_name
                                   || '.'
                                   || l_api_name
                                   || 'v_sds_type ='
                                   || v_sds_type);
               gme_debug.put_line (   g_pkg_name
                                   || '.'
                                   || l_api_name
                                   || 'seq_dep_id ='
                                   || v_seq_dep_id);
               gme_debug.put_line (   g_pkg_name
                                   || '.'
                                   || l_api_name
                                   || 'batchstep resource id = '
                                   || v_batchstep_resource_id);
            END IF;

            UPDATE gme_batch_step_resources
               SET sequence_dependent_id = v_seq_dep_id
                  ,last_update_date = SYSDATE
                  ,last_updated_by = gme_common_pvt.g_user_ident
                  ,last_update_login = gme_common_pvt.g_login_id     --- Punit
             WHERE batchstep_resource_id = v_batchstep_resource_id;
         END IF;
      END LOOP;

      CLOSE get_batch_details;
   END set_sequence_dependent_id;

/*===========================================================================================
   Procedure
      set_all_batch_sequences
   Description
     This procedure is used to assign the sequence dependent ids to the resouces of pending,wip
     batches
=============================================================================================*/
   PROCEDURE set_all_batch_sequences
   IS
      l_gme_batch_ids_tab   gme_batch_ids_tab;

        /* this cursor will get just the batches where a sequence dependent activity exists.
      the batch also has to be pending or WIP for this to happen */

      -- Bug 25246717/25241112 - restructure query to perform better.
      CURSOR get_batches
      IS
         SELECT distinct gbh.batch_id
           FROM gme_batch_header gbh, gme_batch_step_activities gsa
          WHERE gbh.batch_status IN (1, 2)
            AND gbh.batch_id = gsa.batch_id
            AND gsa.sequence_dependent_ind = 1;          
                            
/*      
      CURSOR get_batches
      IS
         SELECT gbh.batch_id
           FROM gme_batch_header gbh
          WHERE gbh.batch_status IN (1, 2)
            AND gbh.batch_id IN (
                   SELECT gbh1.batch_id
                     FROM gme_batch_header gbh1
                         ,gme_batch_step_activities gsa
                    WHERE gbh.batch_status IN (1, 2)
                      AND gbh.batch_id = gsa.batch_id
                      AND gsa.sequence_dependent_ind = 1);
*/                      

      v_batch_id            NUMBER            := 0;
   BEGIN
      OPEN get_batches;

      FETCH get_batches
      BULK COLLECT INTO l_gme_batch_ids_tab;

      CLOSE get_batches;

      FOR i IN 1 .. l_gme_batch_ids_tab.COUNT LOOP
         set_sequence_dependent_id (l_gme_batch_ids_tab (i) );
      END LOOP;

      COMMIT;
   END set_all_batch_sequences;

/*===========================================================================================
   Procedure
      Clear_charge_dates
   Description
     This procedure clears the charge dates.If the batchstep_id argument is passed then only the
     charge dates associated to that step will be cleared.If the batchstep_id argument is null,
     then the dates of all charges associated with the p_batch_id parameter will be cleared.

=============================================================================================*/
   PROCEDURE clear_charge_dates (
      p_batch_id        IN              NUMBER
     ,p_batchstep_id    IN              NUMBER DEFAULT NULL
     ,x_return_status   OUT NOCOPY      VARCHAR2)
   IS
      l_api_name          CONSTANT VARCHAR2 (30)      := 'CLEAR_CHARGE_DATES';

      CURSOR cur_get_batchstep_ids (v_batch_id NUMBER)
      IS
         SELECT batchstep_id
           FROM gme_batch_steps
          WHERE batch_id = v_batch_id;

      CURSOR cur_is_charge_associated (
         v_batch_id       NUMBER
        ,v_batchstep_id   NUMBER)
      IS
         SELECT 1
           FROM gme_batch_step_charges
          WHERE batch_id = v_batch_id
            AND batchstep_id = v_batchstep_id
            AND ROWNUM = 1;

      l_gme_batchstep_ids_tab      gme_batch_step_chg_pvt.gme_batchstep_ids_tab;
      l_cur_is_charge_associated   cur_is_charge_associated%ROWTYPE;
      l_return_status              VARCHAR2 (1);
   BEGIN
      /* Set the return status to success initially */
      x_return_status := fnd_api.g_ret_sts_success;

      IF p_batchstep_id IS NOT NULL THEN
         --clear the dates of only charges associated with the step.
         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || 'batch,step ids are'
                                || p_batch_id
                                || p_batchstep_id);
         END IF;

         OPEN cur_is_charge_associated (p_batch_id, p_batchstep_id);

         FETCH cur_is_charge_associated
          INTO l_cur_is_charge_associated;

         IF cur_is_charge_associated%FOUND THEN
            CLOSE cur_is_charge_associated;

            UPDATE gme_batch_step_charges
               SET plan_start_date = NULL
                  ,plan_cmplt_date = NULL
                  ,last_update_date = gme_common_pvt.g_timestamp
                  ,last_updated_by = gme_common_pvt.g_user_ident
                  ,last_update_login = gme_common_pvt.g_login_id     --- Punit
             WHERE batch_id = p_batch_id AND batchstep_id = p_batchstep_id;
         ELSE
            CLOSE cur_is_charge_associated;
         END IF;
      ELSE
         --p_batchstep_id not passed.clear the dates for all charges associated with the batch.
         OPEN cur_get_batchstep_ids (p_batch_id);

         FETCH cur_get_batchstep_ids
         BULK COLLECT INTO l_gme_batchstep_ids_tab;

         CLOSE cur_get_batchstep_ids;

         FOR i IN 1 .. l_gme_batchstep_ids_tab.COUNT LOOP
            IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
               gme_debug.put_line (   g_pkg_name
                                   || l_api_name
                                   || 'batch,step ids are'
                                   || p_batch_id
                                   || l_gme_batchstep_ids_tab (i) );
            END IF;

            OPEN cur_is_charge_associated (p_batch_id
                                          ,l_gme_batchstep_ids_tab (i) );

            FETCH cur_is_charge_associated
             INTO l_cur_is_charge_associated;

            IF cur_is_charge_associated%FOUND THEN
               CLOSE cur_is_charge_associated;

               UPDATE gme_batch_step_charges
                  SET plan_start_date = NULL
                     ,plan_cmplt_date = NULL
                     ,last_update_date = gme_common_pvt.g_timestamp
                     ,last_updated_by = gme_common_pvt.g_user_ident
                     ,last_update_login = gme_common_pvt.g_login_id  --- Punit
                WHERE batch_id = p_batch_id
                  AND batchstep_id = l_gme_batchstep_ids_tab (i);
            ELSE
               CLOSE cur_is_charge_associated;
            END IF;
         END LOOP;
      END IF;
   EXCEPTION
      WHEN OTHERS THEN
         x_return_status := fnd_api.g_ret_sts_unexp_error;
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ':'
                             || ' OTHERS:'
                             || SQLERRM);
   END clear_charge_dates;

/*===========================================================================================
   Procedure
      Clear_charges
   Description
     This procedure clears the charges.If the batchstep_id argument is passed then only the
     charges associated to that step will be cleared.If the batchstep_id argument is null,
     then all charges associated with the p_batch_id parameter will be cleared.

=============================================================================================*/
   PROCEDURE clear_charges (
      p_batch_id        IN              NUMBER
     ,p_batchstep_id    IN              NUMBER DEFAULT NULL
     ,x_return_status   OUT NOCOPY      VARCHAR2)
   IS
      l_api_name          CONSTANT VARCHAR2 (30)           := 'CLEAR_CHARGES';

      CURSOR cur_get_batchstep_ids (v_batch_id NUMBER)
      IS
         SELECT batchstep_id
           FROM gme_batch_steps
          WHERE batch_id = v_batch_id;

      CURSOR cur_is_charge_associated (
         v_batch_id       NUMBER
        ,v_batchstep_id   NUMBER)
      IS
         SELECT 1
           FROM gme_batch_step_charges
          WHERE batch_id = v_batch_id
            AND batchstep_id = v_batchstep_id
            AND ROWNUM = 1;

      l_gme_batchstep_ids_tab      gme_batch_step_chg_pvt.gme_batchstep_ids_tab;
      l_cur_is_charge_associated   cur_is_charge_associated%ROWTYPE;
      l_return_status              VARCHAR2 (1);
      l_batch_step_charges_in      gme_batch_step_charges%ROWTYPE;
      clear_chg_error              EXCEPTION;
   BEGIN
      /* Set the return status to success initially */
      x_return_status := fnd_api.g_ret_sts_success;

      IF p_batchstep_id IS NOT NULL THEN
         --clear only charges associated with the step.
         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ' batch step ids are'
                                || p_batchstep_id
                                || p_batch_id);
         END IF;

         OPEN cur_is_charge_associated (p_batch_id, p_batchstep_id);

         FETCH cur_is_charge_associated
          INTO l_cur_is_charge_associated;

         IF cur_is_charge_associated%FOUND THEN
            CLOSE cur_is_charge_associated;

            l_batch_step_charges_in.batch_id := p_batch_id;
            l_batch_step_charges_in.batchstep_id := p_batchstep_id;

            IF NOT (gme_batch_step_charge_dbl.delete_row
                           (p_batch_step_charges_in      => l_batch_step_charges_in) ) THEN
               RAISE clear_chg_error;
            END IF;
         ELSE
            CLOSE cur_is_charge_associated;
         END IF;
      ELSE
         --p_batchstep_id not passed.clear the dates for all charges associated with the batch.
         OPEN cur_get_batchstep_ids (p_batch_id);

         FETCH cur_get_batchstep_ids
         BULK COLLECT INTO l_gme_batchstep_ids_tab;

         CLOSE cur_get_batchstep_ids;

         FOR i IN 1 .. l_gme_batchstep_ids_tab.COUNT LOOP
            IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
               gme_debug.put_line (   g_pkg_name
                                   || '.'
                                   || l_api_name
                                   || 'batch,step ids are'
                                   || p_batch_id
                                   || l_gme_batchstep_ids_tab (i) );
            END IF;

            OPEN cur_is_charge_associated (p_batch_id
                                          ,l_gme_batchstep_ids_tab (i) );

            FETCH cur_is_charge_associated
             INTO l_cur_is_charge_associated;

            IF cur_is_charge_associated%FOUND THEN
               CLOSE cur_is_charge_associated;

               l_batch_step_charges_in.batch_id := p_batch_id;
               l_batch_step_charges_in.batchstep_id :=
                                                  l_gme_batchstep_ids_tab (i);

               IF NOT (gme_batch_step_charge_dbl.delete_row
                           (p_batch_step_charges_in      => l_batch_step_charges_in) ) THEN
                  RAISE clear_chg_error;
               END IF;
            ELSE
               CLOSE cur_is_charge_associated;
            END IF;
         END LOOP;
      END IF;
   EXCEPTION
      WHEN clear_chg_error THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN OTHERS THEN
         x_return_status := fnd_api.g_ret_sts_unexp_error;
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ':'
                             || ' OTHERS:'
                             || SQLERRM);
   END clear_charges;

/*===========================================================================================
Procedure
 calc_activity_sequence_number
Description
This procedure calcualates the activity_sequence_number of gme_batch_step_charges table
Parameters
  p_batchstep_id           Batchstep id of the step for whose charge details,the activity
                                sequence is to be calculated.
  p_resoures                    Resource associated with the charges for the step represented by
                                p_batchstep_id
  x_act_seq_num                 The resultant activity_sequence_number
  x_return_status    outcome of the API call
            S - Success
            E - Error

 HISTORY
  Rishi Varma B3718176 14-07-2004
       Created.
=============================================================================================*/
   PROCEDURE calc_activity_sequence_number (
      p_batchstep_id    IN              gme_batch_steps.batchstep_id%TYPE
     ,p_resources       IN              gme_batch_step_resources.resources%TYPE
     ,x_act_seq_num     OUT NOCOPY      NUMBER
     ,x_return_status   OUT NOCOPY      VARCHAR2)
   IS
      CURSOR cur_get_seq_dep_ind (
         v_resources      gme_batch_step_resources.resources%TYPE
        ,v_batchstep_id   gme_batch_steps.batchstep_id%TYPE)
      IS
         SELECT MIN (a.sequence_dependent_ind)
           FROM gme_batch_step_activities a, gme_batch_step_resources r
          WHERE r.resources = v_resources
            AND r.batchstep_activity_id = a.batchstep_activity_id
            AND a.batchstep_id = v_batchstep_id
            AND r.batchstep_id = v_batchstep_id
            AND r.scale_type = 2;

      l_api_name   CONSTANT VARCHAR2 (30) := 'CALC_ACTIVITY_SEQUENCE_NUMBER';
      l_activity            VARCHAR2 (20);
   BEGIN
      IF NVL (g_debug, -1) = gme_debug.g_log_statement THEN
         gme_debug.put_line (g_pkg_name || '.' || l_api_name || ' Entering');
      END IF;

      /* Initialise the return status to success*/
      x_return_status := fnd_api.g_ret_sts_success;

      OPEN cur_get_seq_dep_ind (p_resources, p_batchstep_id);

      FETCH cur_get_seq_dep_ind
       INTO x_act_seq_num;

      IF cur_get_seq_dep_ind%NOTFOUND THEN
         CLOSE cur_get_seq_dep_ind;
      END IF;

      CLOSE cur_get_seq_dep_ind;

      IF NVL (g_debug, -1) = gme_debug.g_log_statement THEN
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ' In loop:seq dep ind,activity are'
                             || x_act_seq_num);
      END IF;

      IF x_act_seq_num IS NULL THEN
         x_return_status := fnd_api.g_ret_sts_error;
      END IF;
   END calc_activity_sequence_number;

/*===========================================================================================
Procedure
 populate_charges_table
Description
This procedure populates the charge details table gme_batch_step_charges.
Parameters
  p_batchstep_charges_in         The gme_batch_step_charges%ROWTYPE variable used to populate the
                                table.
  p_no_of_charges               The number of calculated charges.
  p_remaining_quantity          The quantity for the last charge.
  x_return_status    outcome of the API call
            S - Success
            E - Error
            U - Unexpected error
  The p_batchstep_charges parameter should have the following attributes populated;batch_id,
  batchstep_id,charge_quantity,resources,
 HISTORY
  Rishi Varma B3718176 23-07-2004
       Created.
=============================================================================================*/
   PROCEDURE populate_charges_table (
      p_batchstep_charges_in   IN              gme_batch_step_charges%ROWTYPE
     ,p_no_of_charges          IN              NUMBER
     ,p_remaining_quantity     IN              NUMBER
     ,x_return_status          OUT NOCOPY      VARCHAR2)
   IS
      l_api_name          CONSTANT VARCHAR2 (30)  := 'POPULATE_CHARGES_TABLE';
      l_batchstep_charges_in       gme_batch_step_charges%ROWTYPE;
      l_activity_sequence_number   NUMBER;
      l_return_status              VARCHAR2 (1);
      error_act_seq_num_calc       EXCEPTION;
      error_charge_insert          EXCEPTION;
   BEGIN
      x_return_status := fnd_api.g_ret_sts_success;
      l_batchstep_charges_in := p_batchstep_charges_in;
      /*Calculating the activity sequence_number*/
      gme_batch_step_chg_pvt.calc_activity_sequence_number
                      (p_batchstep_id       => l_batchstep_charges_in.batchstep_id
                      ,p_resources          => l_batchstep_charges_in.resources
                      ,x_act_seq_num        => l_activity_sequence_number
                      ,x_return_status      => l_return_status);

      IF x_return_status <> fnd_api.g_ret_sts_success THEN
         IF NVL (g_debug, -1) = gme_debug.g_log_statement THEN
            gme_debug.put_line
                             ('Error in calculating activity sequence number');
         END IF;

         RAISE error_act_seq_num_calc;
      END IF;

      l_batchstep_charges_in.activity_sequence_number :=
                                                    l_activity_sequence_number;

      FOR i IN 1 .. (p_no_of_charges - 1) LOOP
         l_batchstep_charges_in.charge_number := i;

         IF (gme_batch_step_charge_dbl.insert_row
                           (p_batch_step_charges_in      => l_batchstep_charges_in
                           ,x_batch_step_charges         => l_batchstep_charges_in) ) THEN
            IF NVL (g_debug, -1) = gme_debug.g_log_statement THEN
               gme_debug.put_line
                               ('sucessfully inserted into batchstep charges');
            END IF;
         ELSE
            RAISE error_charge_insert;
         END IF;
      END LOOP;

      --Populating the details of the last charge.
      IF p_remaining_quantity <> -1 THEN
         l_batchstep_charges_in.charge_quantity := p_remaining_quantity;
      END IF;

      l_batchstep_charges_in.charge_number := p_no_of_charges;

      IF (gme_batch_step_charge_dbl.insert_row
                           (p_batch_step_charges_in      => l_batchstep_charges_in
                           ,x_batch_step_charges         => l_batchstep_charges_in) ) THEN
         IF NVL (g_debug, -1) = gme_debug.g_log_statement THEN
            gme_debug.put_line ('sucessfully inserted into batchstep charges');
         END IF;
      ELSE
         RAISE error_charge_insert;
      END IF;
   EXCEPTION
      WHEN error_charge_insert OR error_act_seq_num_calc THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN OTHERS THEN
         x_return_status := fnd_api.g_ret_sts_unexp_error;
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ':'
                             || ' OTHERS:'
                             || SQLERRM);
   END;
END gme_batch_step_chg_pvt;
/

COMMIT ;
EXIT;
