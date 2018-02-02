/* +======================================================================+ */
/* |    Copyright (c) 2005, 2015 Oracle and/or its affiliates.           | */
/* |                         All rights reserved.                         | */
/* |                           Version 12.0.0                             | */
/* +======================================================================+ */
REM dbdrv: sql ~PROD ~PATH ~FILE none none none package &phase=plb \
REM dbdrv: checkfile(120.12.12010000.7=120.13.12020000.6)(120.9.12000000.6=120.12.12010000.4)(115.44=120.1):~PROD:~PATH:~FILE
SET VERIFY OFF
WHENEVER SQLERROR EXIT FAILURE ROLLBACK;
WHENEVER OSERROR EXIT FAILURE ROLLBACK;
/*
REM **********************************************************************
REM *                                                                    *
REM * FILE:    GMEVRSSB.pls                                              *
REM * PURPOSE: Package Body for the GME RESCHEDULE BATCH STEP routines   *
REM * AUTHOR:  Navin Kumar Sinha, OPM Development                        *
REM * DATE:    March 02nd 2005                                           *
REM * HISTORY:                                                           *
REM *                                                                    *
REM * 02MAR05  Navin Kumar Sinha                                         *
REM *          Created                                                   *
REM * 15-NOV-2005  Pawan Kumar Bug#454387                                *
REM *         The due date of the batch steps is updated to the old due  *
REM *         date in the procedure reschedule_step.                     *
REM * 17-FEB-2006 SivakumarG Bug#4585491
REM *         Code added to put appropriate message on stack if Reservati-*
REM * ons or MO allocations or both get deleted. The diff status will be *
REM * returned from gme_common_pvt.material_date_change                  *
REM * 03-JUL-2006 SivakumarG Bug#5365527                                 *
REM *         Added check for LCF Batches before checking validity rule  *
REM *         dates.

REM * 12-APR-2010 G. Muratore   Bug 11925400                             *
REM *         Add step status check to the where clause for 2 cursors.   *
REM *         PROCEDURE: reschedule_step                                 *

REM * 27-Nov-2012 G. Muratore   Bug 14550770                             *
REM *         Correct date check for wip batches when rescheduling.      *
REM *         PROCEDURE: reschedule_step                                 *

REM * 16-May-2013 G. Muratore   Bug 16692125                             *
REM *         Additional change missed in 14550770.                      *
REM *         PROCEDURE: reschedule_step                                 *

REM * 20-Nov-2013 G. Muratore   Bug 17410850                             *
REM *         Do not delete already completed transactions and           *
REM *         call calc_dates with Null value for start date when        *
REM *         initiating from a wip step.                                *
REM *         PROCEDURE: reschedule_step                                 *    
REM * 18-JUL-2014 Shaliu Chen       ER 19161894                          *                          
REM *     Modify reschedule_step to invoke need_by_date sync program     *
REM *     if batch include OSP step                                      *
REM *  Shaliu Chen     10-APR-2015  ER 20809749                          *
REM *    Modify reschedule_step to support multiple osp steps.           *
REM **********************************************************************/
/*************************************************************************
* This file contains the procedure for rescheduling batch steps in Oracle*
* Process Manufacturing (OPM). Each procedure has a common set of        *
* parameters to which API-specific parameters are appended.              *
*************************************************************************/

CREATE OR REPLACE PACKAGE BODY gme_reschedule_step_pvt AS
   /* $Header: GMEVRSSB.pls 120.13.12020000.6 2015/04/10 10:21:13 shalchen ship $ */
   g_debug               VARCHAR2 (5)  := fnd_profile.VALUE ('AFLOG_LEVEL');
   g_pkg_name   CONSTANT VARCHAR2 (30) := 'GME_RESCHEDULE_STEP_PVT';

/*================================================================================
        Procedure
        Reschedule Step
        Description
        This particular procedure call reschedules the batch steps.
        Parameters
        p_batch_step_rec        The batch step row to identify the step.
        p_reschedule_preceding  Whether to reschedule preceding dependent steps
        p_reschedule_succeeding Whether to reschedule succeeding dependent steps
        p_source_step_id        Since this is recursive procedure, we need to know,
                                Where it is coming from.  So this really is a table
                                which holds all the steps rescheduled so far.
        p_use_workday_cal       Whether to use workday calendar or not.
                                T - Use it
                                F - Do not use it
        p_contiguity_override   Whether to override contiguity check and reschedule
                                step anyway.
                                T - Override it
                                F - DO not override it.
        x_batch_step_rec        The batch step returned.
        x_return_status         outcome of the API call
                                S - Success
                                E - Error
                                U - Unexpected error
        HISTORY
           12-APR-2010 G. Muratore   Bug 11925400
 
 G. Muratore     27-Nov-2012    Bug 14550770
   Correct date check for wip batches when rescheduling.
              Add step status check to the where clause for 2 cursors.
              
 G. Muratore     20-Nov-2013    Bug 17410850
   Do not delete already completed transactions and call calc_dates 
   with Null value for start date when initiating from a wip step.
 ================================================================================*/
   PROCEDURE reschedule_step (
      p_batch_step_rec          IN              gme_batch_steps%ROWTYPE
     ,p_source_step_id_tbl      IN              step_tab
     ,p_contiguity_override     IN              VARCHAR2
     ,p_reschedule_preceding    IN              VARCHAR2
     ,p_reschedule_succeeding   IN              VARCHAR2
     ,p_use_workday_cal         IN              VARCHAR2
     ,x_batch_step_rec          OUT NOCOPY      gme_batch_steps%ROWTYPE
     ,x_return_status           OUT NOCOPY      VARCHAR2)
   IS
      /* Buffers for database reads/writes */
      l_api_name          CONSTANT VARCHAR2 (30)         := 'RESCHEDULE_STEP';
      l_batch_header_rec           gme_batch_header%ROWTYPE;
      l_batch_header2_rec          gme_batch_header%ROWTYPE;
      l_batch_step_rec             gme_batch_steps%ROWTYPE;
      l_batch_step_m_rec           gme_batch_steps%ROWTYPE;
      l_material_detail_id_tbl     gme_common_pvt.number_tab;
      l_rel_type                   NUMBER;
      l_contig_period_tbl          gmp_calendar_api.contig_period_tbl;
      l_loop_count_get_material    NUMBER;
      --Bug#5606089
      l_batch_step2_rec            gme_batch_steps%ROWTYPE;
      x_batch_step2_rec            gme_batch_steps%ROWTYPE;
      TYPE l_line_type_tbl_typ IS TABLE OF gme_material_details.line_type%TYPE
         INDEX BY BINARY_INTEGER;

      l_line_type_tbl              l_line_type_tbl_typ;
      l_material_date              DATE;
      /* Exception definitions */
      batch_step_fetch_error       EXCEPTION;
      no_dates_passed              EXCEPTION;
      no_date_change               EXCEPTION;
      invalid_step_status          EXCEPTION;
      batch_header_fetch_error     EXCEPTION;
      save_data_error              EXCEPTION;
      child_step_resch_error       EXCEPTION;
      parent_step_resch_error      EXCEPTION;
      prev_step_err                EXCEPTION;
      mtl_dt_chg_error             EXCEPTION;
      date_overlap_error           EXCEPTION;
      trun_date_error              EXCEPTION;
      cal_dates_error              EXCEPTION;
      step_start_date_low          EXCEPTION;
      error_cont_period            EXCEPTION;
      error_non_contiguious        EXCEPTION;
      date_exceed_validity_rule    EXCEPTION;
      invalid_schedule_status      EXCEPTION;
      clear_chg_dates_error        EXCEPTION;
      sync_nbd_failed              EXCEPTION;  /*ER 19161894  Shaliu Chen 18-JUL-2014*/      
      /* Local variables */
      l_return_status              VARCHAR2 (1);
      l_max_end_date               DATE;
      l_min_start_date             DATE;
      l_change                     BOOLEAN                           := FALSE;
      l_doc_type                   VARCHAR2 (4);
      l_cal_count                  NUMBER;
      l_duration                   NUMBER;
      l_date                       DATE;
      /*ER 19161894  Shaliu Chen 18-JUL-2014*/
      l_message_list               VARCHAR2(2000);
      l_message_count              NUMBER;  
      l_propagate_change_to_po     NUMBER;
      l_osp_resource_flag          NUMBER;        
      /*END ER 19161894*/
      
      --FPBug#4585491
      l_R_count                       NUMBER := 0;
      l_M_count                       NUMBER := 0;
      l_B_count                       NUMBER := 0;

      CURSOR cur_get_max (v_batch_id NUMBER)
      IS
         SELECT MAX (plan_cmplt_date)
           FROM gme_batch_steps
          WHERE batch_id = v_batch_id;

      CURSOR cur_get_min (v_batch_id NUMBER)
      IS
         SELECT MIN (plan_start_date)
           FROM gme_batch_steps
          WHERE batch_id = v_batch_id;

      -- Bug 14550770 - Min actual start date of already started steps.
      CURSOR cur_get_min_actual (v_batch_id NUMBER) IS
         SELECT MIN (actual_start_date)
           FROM gme_batch_steps
          WHERE batch_id = v_batch_id
            AND step_status > 1;

      l_act_min_start_date             DATE;
      
      CURSOR cur_get_material (v_batch_id NUMBER)
      IS
         SELECT material_detail_id, line_type
           FROM gme_material_details
          WHERE batch_id = v_batch_id;

      -- Bug 11925400 - Add step status check to the where clause.
      CURSOR cur_get_prec_steps (v_batch_id NUMBER, v_batchstep_id NUMBER)
      IS
         SELECT   s.batchstep_id, d.dep_type, d.standard_delay
                 ,s.step_status
             FROM gme_batch_step_dependencies d, gme_batch_steps s
            WHERE d.batchstep_id = v_batchstep_id
              AND s.batchstep_id = d.dep_step_id
              AND s.batch_id = v_batch_id
              AND d.batch_id = s.batch_id
              AND s.step_status in (1, 2)
         ORDER BY s.plan_start_date;

      -- Bug 11925400 - Add step status check to the where clause.
      CURSOR cur_get_succ_steps (v_batch_id NUMBER, v_batchstep_id NUMBER)
      IS
         SELECT   d.batchstep_id, d.dep_type, d.standard_delay, s.step_status
             FROM gme_batch_step_dependencies d, gme_batch_steps s
            WHERE d.batchstep_id = s.batchstep_id
              AND d.dep_step_id = v_batchstep_id
              AND s.batch_id = v_batch_id
              AND d.batch_id = s.batch_id
              AND s.step_status in (1, 2)
         ORDER BY s.plan_cmplt_date;

      CURSOR cur_get_dep_step_times (v_batchstep_id NUMBER)
      IS
         SELECT batchstep_id, plan_start_date, plan_cmplt_date
           FROM gme_batch_steps
          WHERE batchstep_id = v_batchstep_id;

      CURSOR cur_get_max_date_from_prev (
         v_batch_id       NUMBER
        ,v_batchstep_id   NUMBER)
      IS
         SELECT MAX (  DECODE (d.dep_type
                              ,1, s.plan_start_date
                              ,0, s.plan_cmplt_date)
                     + d.standard_delay / 24) max_date
           FROM gme_batch_step_dependencies d, gme_batch_steps s
          WHERE d.batchstep_id = v_batchstep_id
            AND s.batchstep_id = d.dep_step_id
            AND s.batch_id = v_batch_id
            AND d.batch_id = s.batch_id;

      CURSOR cur_is_charge_associated (v_batch_id NUMBER, v_batchstep_id NUMBER)
      IS
         SELECT 1
           FROM DUAL
          WHERE EXISTS (
                   SELECT 1
                     FROM gme_batch_step_charges
                    WHERE batch_id = v_batch_id
                      AND batchstep_id = v_batchstep_id);
      
      /*ER 19161894  Shaliu Chen 18-JUL-2014*/                
      CURSOR cur_get_osp_parameter(v_organization_id NUMBER)
      IS
         SELECT propagate_change_to_po
           FROM gme_parameters
          WHERE organization_id = v_organization_id;                         

      l_cur_is_charge_associated   cur_is_charge_associated%ROWTYPE;
      l_dep_step_rec               cur_get_succ_steps%ROWTYPE;
      l_max_date                   DATE;
      l_found                      BOOLEAN;
      l_source_step_id_tbl         step_tab;
      l_calendar_code              VARCHAR2 (10);
   BEGIN
      IF (NVL (g_debug, 0) IN
                       (gme_debug.g_log_statement, gme_debug.g_log_procedure) ) THEN
         gme_debug.put_line (g_pkg_name || '.' || l_api_name || ':'
                             || 'Entering');
      END IF;

      /* Set the return status to success initially */
      x_return_status := fnd_api.g_ret_sts_success;

      /* Ensure that either a start_date or end_date has been passed. */
      IF (    p_batch_step_rec.plan_start_date IS NULL
          AND p_batch_step_rec.plan_cmplt_date IS NULL) THEN
         RAISE no_dates_passed;
      END IF;

      x_batch_step_rec := p_batch_step_rec;

      /* The current Step Status must be Pending */
      IF NOT (x_batch_step_rec.step_status in (1,2)) THEN
         RAISE invalid_step_status;
      END IF;

      l_batch_header_rec.batch_id := x_batch_step_rec.batch_id;
      l_calendar_code := gme_common_pvt.g_calendar_code;

      /* Initialize local batch header */
      IF NOT (gme_batch_header_dbl.fetch_row (l_batch_header_rec
                                             ,l_batch_header_rec) ) THEN
         RAISE batch_header_fetch_error;
      END IF;

      IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
         gme_debug.put_line (   'trying to reschedule steps to '
                             || TO_CHAR (p_batch_step_rec.plan_start_date
                                        ,'DD-MON-YYYY HH24:MI:SS') );
         gme_debug.put_line (   'trying to reschedule steps cmp to '
                             || TO_CHAR (p_batch_step_rec.plan_cmplt_date
                                        ,'DD-MON-YYYY HH24:MI:SS') );
         gme_debug.put_line (   'Going to check previous step of '
                             || TO_CHAR (x_batch_step_rec.batchstep_no) );
         gme_debug.put_line (   'p_source_step_id_tbl '
                             || TO_CHAR (p_source_step_id_tbl.COUNT) );
      END IF;

      IF (l_batch_header_rec.batch_type = 0) THEN
         l_doc_type := 'PROD';
      ELSE
         l_doc_type := 'FPO';
      END IF;

      IF l_batch_header_rec.update_inventory_ind = 'Y' THEN
         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line (   'deleting transactions for  '
                                || x_batch_step_rec.batchstep_id);
         END IF;

         -- Bug 17410850 - Do not delete already completed transactions.
         DELETE FROM gme_resource_txns
               WHERE doc_type = l_doc_type
                 AND doc_id = x_batch_step_rec.batch_id
                 AND completed_ind = 0
                 AND line_id IN (
                        SELECT batchstep_resource_id
                          FROM gme_batch_step_resources
                         WHERE batch_id = x_batch_step_rec.batch_id
                           AND batchstep_id = x_batch_step_rec.batchstep_id);

         -- Navin Added as part of Reschedule Batch/Step Build.
         DELETE FROM gme_resource_txns_gtmp
               WHERE doc_type = l_doc_type
                 AND doc_id = x_batch_step_rec.batch_id
                 AND completed_ind = 0
                 AND line_id IN (
                        SELECT batchstep_resource_id
                          FROM gme_batch_step_resources
                         WHERE batch_id = x_batch_step_rec.batch_id
                           AND batchstep_id = x_batch_step_rec.batchstep_id);
      END IF;

      -- Bug 17410850 - Pass in Null if this is a wip step.
      IF (p_batch_step_rec.step_status = gme_common_pvt.g_step_pending) THEN
         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line ('calling calc_dates for a pending step');
         END IF;
         
         gme_create_step_pvt.calc_dates
                          (p_gme_batch_header_rec      => l_batch_header_rec
                          ,p_use_workday_cal           => p_use_workday_cal
                          ,p_contiguity_override       => p_contiguity_override
                          ,p_return_status             => l_return_status
                          ,p_step_id                   => p_batch_step_rec.batchstep_id
                          ,p_plan_start_date           => p_batch_step_rec.plan_start_date
                          ,p_plan_cmplt_date           => p_batch_step_rec.plan_cmplt_date);
      ELSE
         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line ('calling calc_dates for a wip step');
         END IF;
         
         -- For wip step we want dates calculated off of plan_cmplt_date.
         gme_create_step_pvt.calc_dates
                          (p_gme_batch_header_rec      => l_batch_header_rec
                          ,p_use_workday_cal           => p_use_workday_cal
                          ,p_contiguity_override       => p_contiguity_override
                          ,p_return_status             => l_return_status
                          ,p_step_id                   => p_batch_step_rec.batchstep_id
                          ,p_plan_start_date           => NULL
                          ,p_plan_cmplt_date           => p_batch_step_rec.plan_cmplt_date);
      
      END IF;

      IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
         gme_debug.put_line (   'Return status from calc_dates '
                             || l_return_status);
      END IF;

      IF l_return_status <> x_return_status THEN
         RAISE cal_dates_error;
      END IF;

      IF NOT (gme_batch_steps_dbl.fetch_row (p_batch_step_rec
                                            ,x_batch_step_rec) ) THEN
         RAISE batch_step_fetch_error;
      END IF;

      /* System always recalculate plan completion date
         based on plan start date, but If user has passed in both the
         dates then we also need to honor whatever user had passed in,
         that means there will be truncation or gap
      */

      IF NVL (p_batch_step_rec.plan_cmplt_date
             ,x_batch_step_rec.plan_cmplt_date) <
                                              x_batch_step_rec.plan_cmplt_date THEN
         l_batch_header_rec.plan_cmplt_date :=
                                             p_batch_step_rec.plan_cmplt_date;
         gme_reschedule_batch_pvt.truncate_date
                            (p_batch_header_rec      => l_batch_header_rec
                            ,p_batchstep_id          => p_batch_step_rec.batchstep_id
                            ,p_date                  => 1
                            ,x_return_status         => l_return_status);
         x_batch_step_rec.plan_cmplt_date := p_batch_step_rec.plan_cmplt_date;

         IF l_return_status <> x_return_status THEN
            RAISE trun_date_error;
         END IF;
      ELSIF NVL (p_batch_step_rec.plan_cmplt_date
                ,x_batch_step_rec.plan_cmplt_date) >
                                              x_batch_step_rec.plan_cmplt_date THEN
         x_batch_step_rec.plan_cmplt_date := p_batch_step_rec.plan_cmplt_date;

         IF NOT (gme_batch_steps_dbl.update_row (x_batch_step_rec) ) THEN
            RAISE save_data_error;
         END IF;
      END IF;
        --Bug#4543875 (port 4416699) Update the changed step due date in the procedure calc_dates with the original due date 
       UPDATE gme_batch_steps
       SET due_date = p_batch_step_rec.due_date
           ,last_updated_by = gme_common_pvt.g_user_ident
           ,last_update_date = gme_common_pvt.g_timestamp
           ,last_update_login = gme_common_pvt.g_login_id
       WHERE batch_id = p_batch_step_rec.batch_id
       AND batchstep_id = p_batch_step_rec.batchstep_id;
       --Bug#4543875

      IF (NVL (g_debug, 0) = gme_debug.g_log_statement) THEN
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ':'
                             || 'Calling Save_all_data');
      END IF;

      l_batch_step_m_rec := x_batch_step_rec;
      save_all_data (p_batch_step_rec
                    ,p_use_workday_cal
                    ,p_contiguity_override
                    ,x_batch_step_rec.plan_start_date
                    ,x_batch_step_rec.plan_cmplt_date
                    ,l_return_status);

      IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
         gme_debug.put_line (   'Came back from save all data with status '
                             || l_return_status);
      END IF;

      IF l_return_status <> x_return_status THEN
         RAISE save_data_error;
      END IF;

      /*******************************************************************************
      Since this step has been rescheduled now let's reschedule dependent steps
      p_source_step_id_tbl tells us which step we are coming from in the tree.
      Let's assume that we have steps dependency set-up as
       10 - 20 - 30  and 10 - 40  All Finish to start.
      Now user is rescheduling step 20 and dependent steps are getting rescheduled
      as well.  The way logic works is  we are going to call this very procedure
      for all the succeding steps to 20 and all precceding steps to 20.
      That means we call reschedule_step (30, p_source_step_is as 20),
      then in reschedule of 30 we will call succeding steps to 30 there is NONE.
      SO we look at precceding steps to 30 and there is 20, but we do not want to
      reschedule 20 again, because that is where we are coming from.
      This is where p_source_step_id_tbl is used.  If you follow the logic it will
      look like the following:
                              20
                               30
                               10
                                40
      *******************************************************************************/
   --   IF l_batch_header_rec.enforce_step_dependency = 1 THEN
         /* Enforce step dependency on or scheduleing set to TRUE */
         -- rescheduling a batch step  in WIP status when one or more steps is in pending status raises an error.
         FOR i IN 1 .. p_source_step_id_tbl.COUNT LOOP
            l_source_step_id_tbl (i) := p_source_step_id_tbl (i);
         END LOOP;

         /* Call the reschedule step recursive routine for the children of the current step */
         /* Reschedule succeding steps */
         IF p_reschedule_succeeding = fnd_api.g_true  OR 
            l_batch_header_rec.enforce_step_dependency = 1 THEN
            OPEN cur_get_succ_steps (p_batch_step_rec.batch_id
                                    ,p_batch_step_rec.batchstep_id);

            FETCH cur_get_succ_steps
             INTO l_dep_step_rec;

            WHILE cur_get_succ_steps%FOUND LOOP
               l_found := FALSE;

               IF (p_source_step_id_tbl.COUNT > 0) THEN
                  FOR i IN 1 .. p_source_step_id_tbl.COUNT LOOP
                     IF l_dep_step_rec.batchstep_id =
                                                     p_source_step_id_tbl (i) THEN
                        l_found := TRUE;
                        EXIT WHEN l_found;
                     END IF;
                  END LOOP;
               END IF;

               IF     l_found = FALSE
                  AND l_dep_step_rec.step_status <> gme_common_pvt.g_step_wip THEN
                  /* Continue only if the succeeding step is not the same as the step which initiated the reschedule and is not WIP */
                  l_batch_step_rec.batchstep_id :=
                                                  l_dep_step_rec.batchstep_id;
                  l_batch_step_rec.batch_id := p_batch_step_rec.batch_id;
                   --  Rework 4543875 Pawan Kumar added for fetching the data for succeding step
                  IF NOT (gme_batch_steps_dbl.fetch_row (l_batch_step_rec
                                            ,l_batch_step_rec) ) THEN
                      RAISE batch_step_fetch_error;
                  END IF;
                  l_max_date :=
                     gme_create_step_pvt.get_max_step_date
                        (p_use_workday_cal       => p_use_workday_cal
                        ,p_calendar_code         => l_calendar_code
                        ,p_batchstep_id          => l_batch_step_rec.batchstep_id
                        ,p_batch_id              => l_batch_step_m_rec.batch_id
                        ,p_batch_start_date      => l_batch_step_m_rec.plan_start_date);

                  IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
                     gme_debug.put_line
                                 (   'MAX date '
                                  || TO_CHAR (l_max_date
                                             ,'DD-MON-YYYY HH24:MI:SS')
                                  || ' plan start '
                                  || TO_CHAR
                                            (x_batch_step_rec.plan_start_date
                                            ,'DD-MON-YYYY HH24:MI:SS') );
                  END IF;

                  l_batch_step_rec.plan_start_date := l_max_date;
                  l_batch_step_rec.plan_cmplt_date := NULL;
                  l_source_step_id_tbl (l_source_step_id_tbl.COUNT + 1) :=
                                                 p_batch_step_rec.batchstep_id;

                  IF (NVL (g_debug, 0) = gme_debug.g_log_statement) THEN
                     gme_debug.put_line
                            (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ':'
                             || 'Calling Reschedule_step for batchstep_id : '
                             || p_batch_step_rec.batchstep_id);
                  END IF;

                  reschedule_step
                          (p_batch_step_rec             => l_batch_step_rec
                          ,p_source_step_id_tbl         => l_source_step_id_tbl
                          ,p_contiguity_override        => p_contiguity_override
                          ,p_reschedule_preceding       => p_reschedule_preceding
                          ,p_reschedule_succeeding      => p_reschedule_succeeding
                          ,p_use_workday_cal            => p_use_workday_cal
                          ,x_batch_step_rec             => x_batch_step_rec
                          ,x_return_status              => l_return_status);

                  IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
                     gme_debug.put_line
                        (   'Back from reschedule_succeeding_steps with status '
                         || l_return_status
                         || ' for step ID '
                         || p_batch_step_rec.batchstep_id);
                  END IF;

                  IF l_return_status <> x_return_status THEN
                     CLOSE cur_get_succ_steps;

                     RAISE child_step_resch_error;
                  END IF;
               END IF;                                   /* l_found = FALSE */

               FETCH cur_get_succ_steps
                INTO l_dep_step_rec;
            END LOOP;                     /* WHILE cur_get_succ_steps%FOUND */

            CLOSE cur_get_succ_steps;
         END IF;                /* p_reschedule_succeeding = FND_API.G_TRUE */

         /* Reschedule preceding steps */
         IF p_reschedule_preceding = fnd_api.g_true OR 
            l_batch_header_rec.enforce_step_dependency = 1 THEN
            OPEN cur_get_prec_steps (p_batch_step_rec.batch_id
                                    ,p_batch_step_rec.batchstep_id);

            FETCH cur_get_prec_steps
             INTO l_dep_step_rec;

            WHILE cur_get_prec_steps%FOUND LOOP
               l_found := FALSE;

               IF (p_source_step_id_tbl.COUNT > 0) THEN
                  FOR i IN 1 .. p_source_step_id_tbl.COUNT LOOP
                     IF l_dep_step_rec.batchstep_id =
                                                     p_source_step_id_tbl (i) THEN
                        l_found := TRUE;
                        EXIT WHEN l_found;
                     END IF;
                  END LOOP;
               END IF;

               IF     l_found = FALSE
                  AND l_dep_step_rec.step_status <> gme_common_pvt.g_step_wip THEN
                  /* Continue only if the preceeding step is not the same as the step which initiated the reschedule and is not WIP */
                  l_batch_step_rec.batchstep_id :=
                                                  l_dep_step_rec.batchstep_id;
                  l_batch_step_rec.batch_id := p_batch_step_rec.batch_id;
                  --  Rework 4543875 Pawan Kumar added for fetching the data for succeding step
                  IF NOT (gme_batch_steps_dbl.fetch_row (l_batch_step_rec
                                            ,l_batch_step_rec) ) THEN
                      RAISE batch_step_fetch_error;
                  END IF;
                  /* Standard Delay should always be divided by 24 if used in date calculations */
                  IF p_use_workday_cal = fnd_api.g_false THEN
                     IF l_dep_step_rec.dep_type = 0 THEN
                        l_batch_step_rec.plan_cmplt_date :=
                             l_batch_step_m_rec.plan_start_date
                           - l_dep_step_rec.standard_delay / 24;
                        l_batch_step_rec.plan_start_date := NULL;
                     ELSE
                        l_batch_step_rec.plan_start_date :=
                             l_batch_step_m_rec.plan_start_date
                           - l_dep_step_rec.standard_delay / 24;
                        l_batch_step_rec.plan_cmplt_date := NULL;
                     END IF;
                  ELSE
        /* Use workday calendar */
    /* Pass in the plan start date
       of the current step as plan completion date to this procedure as we
       want to find out the start or completion date of the preceeding step */
                     IF l_dep_step_rec.standard_delay >= 0 THEN
                        gmp_calendar_api.get_contiguous_periods
                           (p_api_version        => 1
                           ,p_init_msg_list      => TRUE
                           ,p_start_date         => NULL
                           ,p_end_date           => l_batch_step_m_rec.plan_start_date
                           ,p_calendar_code      => l_calendar_code
                           ,p_duration           => l_dep_step_rec.standard_delay
                           ,p_output_tbl         => l_contig_period_tbl
                           ,x_return_status      => l_return_status);
                        l_cal_count := l_contig_period_tbl.COUNT;
                        l_date := l_contig_period_tbl (l_cal_count).start_date;
                     ELSE                     /* Standard delay is negative */
                        gmp_calendar_api.get_contiguous_periods
                           (p_api_version        => 1
                           ,p_init_msg_list      => TRUE
                           ,p_start_date         => l_batch_step_m_rec.plan_start_date
                           ,p_end_date           => NULL
                           ,p_calendar_code      => l_calendar_code
                           ,p_duration           => ABS
                                                       (l_dep_step_rec.standard_delay)
                           ,p_output_tbl         => l_contig_period_tbl
                           ,x_return_status      => l_return_status);
                        l_cal_count := l_contig_period_tbl.COUNT;
                        l_date := l_contig_period_tbl (l_cal_count).end_date;
                     END IF;          /* l_dep_step_rec.standard_delay >= 0 */

                     IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
                        gme_debug.put_line
                           (   'Called to get the cmplt date for step prior to '
                            || l_batch_step_m_rec.batchstep_id
                            || ' '
                            || TO_CHAR (l_batch_step_m_rec.plan_start_date
                                       ,'DD-MON-YYYY HH24:MI:SS')
                            || ' with duration of '
                            || l_dep_step_rec.standard_delay
                            || ' Got back '
                            || TO_CHAR (l_date, 'DD-MON-YYYY HH24:MI:SS') );
                     END IF;

                     IF l_return_status <> fnd_api.g_ret_sts_success THEN
                        NULL;
                     ELSE
                        IF l_dep_step_rec.dep_type = 0 THEN
                           l_batch_step_rec.plan_cmplt_date := l_date;
                           l_batch_step_rec.plan_start_date := NULL;
                        ELSE
                           l_batch_step_rec.plan_start_date := l_date;
                           l_batch_step_rec.plan_cmplt_date := NULL;
                        END IF;
                     END IF;
                           /* l_return_status <> FND_API.G_RET_STS_SUCCESS  */
                  END IF;                          /* p_use_workday_cal = 0 */

                  l_source_step_id_tbl (l_source_step_id_tbl.COUNT + 1) :=
                                                 p_batch_step_rec.batchstep_id;

                  IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
                     gme_debug.put_line
                                   (   'Calling reschedule for batchstep_id '
                                    || l_batch_step_rec.batchstep_id);
                  END IF;

                  IF (NVL (g_debug, 0) = gme_debug.g_log_statement) THEN
                     gme_debug.put_line
                            (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ':'
                             || 'Calling Reschedule_step for batchstep_id : '
                             || l_batch_step_rec.batchstep_id);
                  END IF;

                  reschedule_step
                          (p_batch_step_rec             => l_batch_step_rec
                          ,p_source_step_id_tbl         => l_source_step_id_tbl
                          ,p_contiguity_override        => p_contiguity_override
                          ,p_reschedule_preceding       => p_reschedule_preceding
                          ,p_reschedule_succeeding      => p_reschedule_succeeding
                          ,p_use_workday_cal            => p_use_workday_cal
                          ,x_batch_step_rec             => x_batch_step_rec
                          ,x_return_status              => l_return_status);

                  IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
                     gme_debug.put_line
                         (   'Back from reschedule source steps with status '
                          || l_return_status
                          || ' Steps IDs COUNT '
                          || l_source_step_id_tbl.COUNT
                          || ' Source Step ID '
                          || l_batch_step_rec.batchstep_id);
                  END IF;

                  IF l_return_status <> x_return_status THEN
                     CLOSE cur_get_prec_steps;

                     RAISE child_step_resch_error;
                  END IF;
               END IF;                                   /* l_found = FALSE */

               FETCH cur_get_prec_steps
                INTO l_dep_step_rec;
            END LOOP;                           /* cur_get_prec_steps%FOUND */

            CLOSE cur_get_prec_steps;
         END IF;                 /* p_reschedule_preceding = FND_API.G_TRUE */
   --   END IF;          /* l_batch_header_rec.enforce_step_dependency = 1 OR */

      IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
         gme_debug.put_line (   'Done with succ as well as prec for '
                             || p_batch_step_rec.batchstep_id);
      END IF;

      IF p_source_step_id_tbl.COUNT = 0 THEN
         /* Update the batch dates etc. only when this procedure is in for the
            main step and not during the recursive calls */
         OPEN cur_get_max (l_batch_header_rec.batch_id);

         FETCH cur_get_max
          INTO l_max_end_date;

         IF l_max_end_date <> l_batch_header_rec.plan_cmplt_date THEN
            l_batch_header_rec.plan_cmplt_date := l_max_end_date;
            l_change := TRUE;
         END IF;

         CLOSE cur_get_max;

         OPEN cur_get_min (l_batch_header_rec.batch_id);

         FETCH cur_get_min
          INTO l_min_start_date;

         CLOSE cur_get_min;

         IF l_batch_header_rec.batch_status = 1 THEN
            IF l_min_start_date <> l_batch_header_rec.plan_start_date THEN
               l_batch_header_rec.plan_start_date := l_min_start_date;
               l_change := TRUE;
            END IF;
         END IF;

         IF l_batch_header_rec.batch_status = 2 THEN
         
            -- Bug 14550770 - Let's fetch the actual start date of already started steps.
            OPEN cur_get_min_actual (l_batch_header_rec.batch_id);
            FETCH cur_get_min_actual INTO l_act_min_start_date;
            IF cur_get_min_actual%NOTFOUND THEN
               l_act_min_start_date := NULL;
            END IF;
            CLOSE cur_get_min_actual;
         
            -- Bug 14550770 - Let's compare the actual start dates.
            IF l_batch_header_rec.enforce_step_dependency = 1 THEN
               -- IF min_start_date < l_batch_header_rec.plan_start_date THEN
               IF NVL(l_act_min_start_date, l_batch_header_rec.actual_start_date) < l_batch_header_rec.actual_start_date THEN
                  RAISE step_start_date_low;
               END IF;
            ELSE          /* l_batch_header_rec.enforce_step_dependency = 1 */
               -- Bug 14550770/16692125 - Let's compare the actual start dates.
               -- IF min_start_date < l_batch_header.plan_start_date THEN
               IF NVL(l_act_min_start_date, l_batch_header_rec.actual_start_date) < l_batch_header_rec.actual_start_date THEN
                  gme_reschedule_batch_pvt.truncate_date
                                   (p_batch_header_rec      => l_batch_header_rec
                                   ,p_date                  => 0
                                   ,x_return_status         => l_return_status);

                  IF l_return_status <> x_return_status THEN
                     RAISE trun_date_error;
                  END IF;
               END IF;
                   /* l_min_start_date < l_batch_header_rec.plan_start_date */
            END IF;       /* l_batch_header_rec.enforce_step_dependency = 1 */
         END IF;                     /* l_batch_header_rec.batch_status = 2 */

         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line
                             (   'New min/max Dates are '
                              || TO_CHAR (l_min_start_date
                                         ,'DD-MON-YYYY HH24:MI:SS')
                              || ' and '
                              || TO_CHAR (l_max_end_date
                                         ,'DD-MON-YYYY HH24:MI:SS')
                              || ' and '
                              || ' Batch dates are '
                              || TO_CHAR (l_batch_header_rec.plan_start_date
                                         ,'DD-MON-YYYY HH24:MI:SS')
                              || ' and '
                              || TO_CHAR (l_batch_header_rec.plan_cmplt_date
                                         ,'DD-MON-YYYY HH24:MI:SS') );
         END IF;

         /* This is to check the contiguity in case of step_start date is passed only*/
         IF     p_use_workday_cal = fnd_api.g_true
            AND p_contiguity_override = fnd_api.g_false THEN
            l_duration :=
                 (  l_batch_header_rec.plan_cmplt_date
                  - l_batch_header_rec.plan_start_date)
               * 24;
            gmp_calendar_api.get_contiguous_periods
                          (p_api_version        => 1
                          ,p_init_msg_list      => TRUE
                          ,p_start_date         => l_batch_header_rec.plan_start_date
                          ,p_end_date           => NULL
                          ,p_calendar_code      => l_calendar_code
                          ,p_duration           => l_duration
                          ,p_output_tbl         => l_contig_period_tbl
                          ,x_return_status      => l_return_status);

            IF (l_return_status <> x_return_status) THEN
               RAISE error_cont_period;
            END IF;

            l_cal_count := l_contig_period_tbl.COUNT;

            IF l_cal_count > 1 THEN
               RAISE error_non_contiguious;
            END IF;
         END IF;

         /* Since we update the batch dates in the code else where
            we want to fetch the row, so that the timestamps are most
            current, which is what is used to make sure that someone
            else has not updated this batch during processing of this
            program execution. That is why l_batch_header_rec is introduced.*/
         IF NOT (gme_batch_header_dbl.fetch_row (l_batch_header_rec
                                                ,l_batch_header2_rec) ) THEN
            RAISE batch_header_fetch_error;
         END IF;

         l_batch_header2_rec.plan_start_date :=
                                            l_batch_header_rec.plan_start_date;
         l_batch_header2_rec.plan_cmplt_date :=
                                            l_batch_header_rec.plan_cmplt_date;

         IF NOT (gme_batch_header_dbl.update_row (l_batch_header2_rec) ) THEN
            RAISE save_data_error;
         END IF;

         --Bug#5365527 added the validity rule check for LCF Batches
	 IF l_batch_header2_rec.recipe_validity_rule_id IS NOT NULL THEN
            -- Checking of batch dates with validity rules dates after teh reschedule
            IF NOT gme_common_pvt.check_validity_rule_dates
                                  (l_batch_header2_rec.recipe_validity_rule_id
                                  ,l_batch_header2_rec.plan_start_date
                                  ,l_batch_header2_rec.plan_cmplt_date) THEN
             x_return_status := fnd_api.g_ret_sts_error;
             RAISE date_exceed_validity_rule;
            END IF;
        ELSE
	     IF (NVL (g_debug, 0) = gme_debug.g_log_statement) THEN
               gme_debug.put_line (   g_pkg_name
                               || '.'
                               || l_api_name
                               || ':'
                               || 'Do not Check Validity Rule Dates as this is LCF batch');
             END IF;
	END IF; /* recipe_validity_rule_id IS NOT NULL */
      END IF;                             /* p_source_step_id_tbl.COUNT = 0 */

      /* Re-query output batch step row */
      IF NOT (gme_batch_steps_dbl.fetch_row (p_batch_step_rec
                                            ,x_batch_step_rec) ) THEN
         RAISE batch_step_fetch_error;
      END IF;

      -- Checking of batch dates with validity rules dates after teh reschedule
      IF l_change = TRUE THEN
         /* Now we have to update the transaction dates of pending transactions */
         /* for the material lines which are not of step release type           */
         OPEN cur_get_material (x_batch_step_rec.batch_id);

         FETCH cur_get_material
         BULK COLLECT INTO l_material_detail_id_tbl, l_line_type_tbl;

         l_loop_count_get_material := cur_get_material%ROWCOUNT;

         CLOSE cur_get_material;

         FOR i IN 1 .. l_loop_count_get_material LOOP
            -- stamp manual and incremental with step dates as well...
            l_rel_type :=
               gme_common_pvt.is_material_auto_release
                                                (l_material_detail_id_tbl (i) );
--Bug#5606089 Start. Added the following code.
       /*     SELECT batchstep_id INTO l_batch_step2_rec.batchstep_id FROM GME_BATCH_STEP_ITEMS 
            WHERE material_detail_id = l_material_detail_id_tbl(i);

            IF NOT (gme_batch_steps_dbl.fetch_row (l_batch_step2_rec
                                            ,x_batch_step2_rec) ) THEN
                   RAISE batch_step_fetch_error;
            END IF;*/
--Bug#5606089 End.
 
-- Modified the if condition.
              IF ( gme_common_pvt.is_material_assoc_to_step (l_material_detail_id_tbl (i) ) = TRUE
                  AND
                  l_rel_type IN
                          (gme_common_pvt.g_mtl_manual_release
                          ,gme_common_pvt.g_mtl_incremental_release
                          ,gme_common_pvt.g_mtl_autobystep_release)) THEN
 
             -- pawan kumar start  bug 5929323  -- moved the fetch of batchstep_id  only when step is assoc    
                
                  SELECT batchstep_id INTO l_batch_step2_rec.batchstep_id FROM GME_BATCH_STEP_ITEMS 
                   WHERE material_detail_id = l_material_detail_id_tbl(i);

                     IF NOT (gme_batch_steps_dbl.fetch_row (l_batch_step2_rec
                                            ,x_batch_step2_rec) ) THEN
                         RAISE batch_step_fetch_error;
                      END IF;
              -- pawan kumar end  bug 5929323 

                                                                       	 
                IF l_line_type_tbl (i) = gme_common_pvt.g_line_type_ing THEN
                  -- Update the material_required_date with the associated plan_start_Date;
                  --Bug#5606089
                  l_material_date := x_batch_step2_rec.plan_start_date;
                  --l_material_date := x_batch_step_rec.plan_start_date;
               ELSE
                  -- Update the material required date with the associated plan cmplt Date;
                  --Bug#5606089
                  l_material_date := x_batch_step2_rec.plan_cmplt_date;
                  --l_material_date := x_batch_step_rec.plan_cmplt_date;
               END IF;
               IF (NVL (g_debug, 0) = gme_debug.g_log_statement) THEN
                  gme_debug.put_line
                          (   'Calling Material Date Change for batchstep_id : '
                           || x_batch_step2_rec.batchstep_id
                           || ' Material_detail_id : '
                           || l_material_detail_id_tbl (i) 
                           || 'for date'||l_material_date );
               END IF;
               gme_common_pvt.material_date_change
                         (p_material_detail_id      => l_material_detail_id_tbl
                                                                           (i)
                         ,p_material_date           => l_material_date
                         ,x_return_status           => l_return_status);

               IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
                  gme_debug.put_line
                       (   'Came back from material_date_change with status for step '
                        || l_return_status);
               END IF;

               --FPBug#4585491 commented out the following lines and added new checks
               /*IF l_return_status <> x_return_status THEN
                  RAISE mtl_dt_chg_error;
               END IF; */
	       
	       IF (l_return_status = fnd_api.g_ret_sts_error) THEN
                 RAISE mtl_dt_chg_error;
               ELSIF (l_return_status = fnd_api.g_ret_sts_unexp_error) THEN
                 RAISE fnd_api.g_exc_unexpected_error;
               END IF; 
               
            ELSE
            	  gme_debug.put_line('3');
               -- Navin Added as part of Reschedule Batch/Step Build.
               IF l_line_type_tbl (i) = gme_common_pvt.g_line_type_ing THEN
                  -- Update the material_required_date with the associated plan_start_Date;
                  l_material_date := l_batch_header_rec.plan_start_date;
               ELSE
                  -- Update the material required date with the associated plan cmplt Date;
                  l_material_date := l_batch_header_rec.plan_cmplt_date;
               END IF;

               IF (NVL (g_debug, 0) = gme_debug.g_log_statement) THEN
                  gme_debug.put_line
                          (   'Calling Material Date Change for batch_id : '
                           || l_batch_header_rec.batch_id
                           || ' Material_detail_id : '
                           || l_material_detail_id_tbl (i)
                           || 'for date'||l_material_date  );
               END IF;

               gme_common_pvt.material_date_change
                         (p_material_detail_id      => l_material_detail_id_tbl
                                                                           (i)
                         ,p_material_date           => l_material_date
                         ,x_return_status           => l_return_status);

               IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
                  gme_debug.put_line
                       (   'Came back from material_date_change with status '
                        || l_return_status);
               END IF;
               --FPBug#4585491 Begin
               /*IF l_return_status <> x_return_status THEN
                  RAISE mtl_dt_chg_error;
               END IF;*/
	       IF (l_return_status = fnd_api.g_ret_sts_error) THEN
                 RAISE mtl_dt_chg_error;
               ELSIF (l_return_status = fnd_api.g_ret_sts_unexp_error) THEN
                 RAISE fnd_api.g_exc_unexpected_error;
               END IF; 
            END IF; /* If material is not associted to steps */
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
         END LOOP;                 /* FOR i IN 1..l_loop_count_get_material */
      END IF;                                            /* l_change = TRUE */

      --FPBug#4585491 Begin
      IF (l_B_count > 0) OR (l_R_count > 0 AND l_M_count > 0) THEN 
       --atleast for one material line MO allocations and reservations are deleted
       gme_common_pvt.log_message('GME_EXPIRED_RESERV_MO_DELETED');
      ELSIF l_R_count > 0 THEN
       ----atleast for one material line reservations are deleted
       gme_common_pvt.log_message('GME_EXPIRED_RESERV_DELETED');
      ELSIF l_M_count > 0 THEN
       ----atleast for one material line MO allocations are deleted
       gme_common_pvt.log_message('GME_EXPIRED_MO_DELETED');
      END IF;
      x_return_status := fnd_api.g_ret_sts_success;      
      --FPBug#4585491 End

      --Clearing the dates of the associated charges.
      OPEN cur_is_charge_associated (l_batch_header_rec.batch_id
                                    ,p_batch_step_rec.batchstep_id);

      FETCH cur_is_charge_associated
       INTO l_cur_is_charge_associated;

      IF cur_is_charge_associated%FOUND THEN
         CLOSE cur_is_charge_associated;

         IF (NVL (g_debug, 0) = gme_debug.g_log_statement) THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ':'
                                || 'Calling Clear charge dates for Batch_id:'
                                || l_batch_header_rec.batch_id
                                || ' Batchstep_id: '
                                || p_batch_step_rec.batchstep_id);
         END IF;

         gme_batch_step_chg_pvt.clear_charge_dates
                             (p_batch_id           => l_batch_header_rec.batch_id
                             ,p_batchstep_id       => p_batch_step_rec.batchstep_id
                             ,x_return_status      => l_return_status);

         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line
                         (   'Came back from Clear charge dates with status '
                          || l_return_status);
         END IF;

         IF l_return_status <> x_return_status THEN
            RAISE clear_chg_dates_error;
         END IF;
      ELSE
         CLOSE cur_is_charge_associated;
      END IF;
      
     /*
        BEGIN ER 19161894  Shaliu Chen 18-JUL-2014               
        Added for OPM Step Level OSP Project  
        1.In the Reschedule_batch API, The plan completion data of step
        would be updated in following 
        gme_create_step_pvt.calc_dates
        truncate_date:If user has passed in both the dates then we also 
                      need to honor whatever user had passed in,that means 
                      there will be truncation or gap
        2.In the Reschedule_step API, The plan completion data of step
        would be updated in following 
        gme_create_step_pvt.calc_dates
        truncate_date:
        gme_batch_steps_dbl.update_row
        There is no common procedure which can add NBD synchronization,
        so we have to add NBD synchronization source code into Reschedule
        batch and Reschedule step respectively.                 
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
        OPEN cur_get_osp_parameter(l_batch_header_rec.organization_id);
        FETCH cur_get_osp_parameter INTO l_propagate_change_to_po;
        CLOSE cur_get_osp_parameter;

        IF (NVL(l_propagate_change_to_po,gme_osp.g_propagate_change_manual) 
                   = gme_osp.g_propagate_change_automatic)
             AND gme_osp.check_release_version THEN
           /* 
            Check whether the batch include Outside resource.
           */
          /*                                    
          BEGIN ER 20809749                    
          Shaliu Chen 10-APR-2015               
          Add condition - batchstep_id to confirm the certain osp step to
          support multiple osp steps.  
          */            
          SELECT count(1)
            INTO l_osp_resource_flag
            FROM gme_batch_step_resources gbsr
           WHERE batch_id = l_batch_header_rec.batch_id
             AND batchstep_id = p_batch_step_rec.batchstep_id
             AND EXISTS (SELECT 1
                           FROM cr_rsrc_dtl crd,
                                cr_rsrc_mst crm
                          WHERE crd.organization_id = gbsr.organization_id
                            AND crd.resources = gbsr.resources
                            AND crd.resources = crm.resources
                            AND crd.purchase_item_id IS NOT NULL
                            AND NVL(crm.outside_process_ind,gme_osp.g_osp_ind_disabled) = gme_osp.g_osp_ind_enabled);
                                 
          IF l_osp_resource_flag = 1 THEN
            /*
              Invoke need_by_date synchronization API.
            */                                       
            gme_osp.updatePOReqNBD(p_batch_id        => l_batch_header_rec.batch_id,
                                   p_batchstep_id    => p_batch_step_rec.batchstep_id,  
                                   p_organization_id => l_batch_header_rec.organization_id,
                                   x_return_status   => x_return_status,
                                   x_message_list    => l_message_list,
                                   x_message_count   => l_message_count);                                                                     
   
                                            
              IF x_return_status <> fnd_api.g_ret_sts_success THEN       
                RAISE sync_nbd_failed; 
              END IF;
          END IF;
        END IF;
      EXCEPTION
        WHEN OTHERS THEN
          l_message_list := 'unexpected error: ' || SQLERRM || 'SQLCODE = ' || SQLCODE;
          gme_debug.put_line('l_errMsg:'||l_message_list);
          RAISE sync_nbd_failed;          
      END; 
      /*END ER 19161894*/         

      --OM-GME integration - NOTIFY_CSR Action (Batch completion date may have changed)
       /*Punit Kumar
       gme_trans_engine_pvt.inform_om
                   ( p_action              => 'NOTIFY_CSR'
                   , p_trans_id            => NULL
                   , p_trans_id_reversed   => NULL
                   , p_gme_batch_hdr       => l_batch_header_rec
                   , p_gme_matl_dtl        => NULL
                   );
         */
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
      WHEN batch_step_fetch_error THEN
         x_return_status := fnd_api.g_ret_sts_error;
         gme_common_pvt.log_message ('GME_API_BATCH_STEP_FETCH_ERR');
      WHEN no_dates_passed THEN
         x_return_status := fnd_api.g_ret_sts_error;
         gme_common_pvt.log_message ('GME_API_RESCH_STEP_NO_DATES');
      WHEN no_date_change THEN
         x_return_status := fnd_api.g_ret_sts_error;
         gme_common_pvt.log_message ('GME_API_RESCH_STEP_NO_DATE_CHG');
      WHEN invalid_step_status THEN
         x_return_status := fnd_api.g_ret_sts_error;
         gme_common_pvt.log_message ('GME_API_INV_STEP_STAT_RESCH');
      WHEN invalid_schedule_status THEN
         x_return_status := fnd_api.g_ret_sts_error;
         gme_common_pvt.log_message ('GME_API_INV_STEP_RESCH');
      WHEN date_overlap_error THEN
         x_return_status := fnd_api.g_ret_sts_error;
         gme_common_pvt.log_message ('GME_STEP_OVERLAP_ERROR');
      WHEN batch_header_fetch_error THEN
         x_return_status := fnd_api.g_ret_sts_error;
         gme_common_pvt.log_message ('GME_API_BATCH_FETCH_ERROR');
      WHEN date_exceed_validity_rule THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN step_start_date_low THEN
         x_return_status := fnd_api.g_ret_sts_error;
         gme_common_pvt.log_message ('GME_ESD_PLAN_DATE');
      WHEN error_cont_period THEN
         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line ('Contiguity period ... _failed');
         END IF;

         x_return_status := l_return_status;
      WHEN error_non_contiguious THEN
         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line ('Contiguity period ... not found');
         END IF;

         gme_common_pvt.log_message ('GME_NON_CONTIGUOUS_TIME');
         x_return_status := 'C';
      WHEN save_data_error OR mtl_dt_chg_error THEN
         x_return_status := l_return_status;
      WHEN child_step_resch_error THEN
         x_return_status := l_return_status;
      WHEN cal_dates_error THEN
         x_return_status := l_return_status;
      WHEN parent_step_resch_error THEN
         x_return_status := l_return_status;
      WHEN prev_step_err THEN
         x_return_status := l_return_status;
      WHEN clear_chg_dates_error THEN
         x_return_status := fnd_api.g_ret_sts_error;
      /*ER 19161894  Shaliu Chen 18-JUL-2014*/   
      WHEN sync_nbd_failed THEN
        IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
          gme_debug.put_line ('Synchronize new step plan completion date to OSP PO failed');
        END IF;   
        gme_common_pvt.log_message('GME_NBD_SYNC_FAILED');                        
        x_return_status := fnd_api.g_ret_sts_error;            
      WHEN OTHERS THEN
         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line (l_api_name || ':OTHERS ' || SQLERRM);
         END IF;

         x_return_status := fnd_api.g_ret_sts_unexp_error;
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
   END reschedule_step;

   /**************************************************************************************
        Procedure
        save_all_data
        Description
        This particular procedure updates all necessary tables.
        Parameters
        p_batch_step_rec       The batch step row to identify the step.
        p_diff             Duration used to reschedule the start date with.
        p_diff_end         Duration used to reschedule the end date with.
        x_return_status    outcome of the API call
               S - Success
               E - Error
               U - Unexpected error
        HISTORY
        G.Kelly     22-Feb-2002  Bug - Rewrote the  code.
        A Newbury   05-Aug-2003  B3045672 Modified cursor to include manual and incremental
        Pawan Kuamr 01-26-2004   For rework of bug 3010444
   ***************************************************************************************/
   PROCEDURE save_all_data (
      p_batch_step_rec        IN              gme_batch_steps%ROWTYPE
     ,p_use_workday_cal       IN              VARCHAR2
     ,p_contiguity_override   IN              VARCHAR2
     ,p_start_date            IN              DATE
     ,p_end_date              IN              DATE
     ,x_return_status         OUT NOCOPY      VARCHAR2)
   IS
      l_api_name         CONSTANT VARCHAR2 (30)            := 'SAVE_ALL_DATA';
      l_material_detail_id_tbl    gme_common_pvt.number_tab;
      l_phantom_ids               gme_common_pvt.number_tab;
      l_return_status             VARCHAR2 (1);
      l_batch_header_rec          gme_batch_header%ROWTYPE;
      l_in_batch_header_rec       gme_batch_header%ROWTYPE;
      x_batch_header_rec          gme_batch_header%ROWTYPE;

      TYPE l_line_type_tbl_typ IS TABLE OF gme_material_details.line_type%TYPE
         INDEX BY BINARY_INTEGER;

      l_line_type_tbl             l_line_type_tbl_typ;
      l_material_date             DATE;
      l_loop_count_get_material   NUMBER;
      /* Exception definitions */
      resched_phant_error         EXCEPTION;
      mtl_dt_chg_error            EXCEPTION;
      invalid_batch               EXCEPTION;
      invalid_prior_dates         EXCEPTION;

      --FPBug#4585491
      l_R_count                       NUMBER := 0;
      l_M_count                       NUMBER := 0;
      l_B_count                       NUMBER := 0;

      CURSOR cur_get_material (v_batch_id NUMBER, v_batchstep_id NUMBER)
      IS
         SELECT material_detail_id, line_type
           FROM gme_material_details det
          WHERE batch_id = v_batch_id
            AND release_type IN (1, 2, 3)
            AND EXISTS (
                   SELECT 1
                     FROM gme_batch_step_items
                    WHERE batch_id = v_batch_id
                      AND batchstep_id = v_batchstep_id
                      AND material_detail_id = det.material_detail_id);
   BEGIN
      IF (NVL (g_debug, 0) IN
                       (gme_debug.g_log_statement, gme_debug.g_log_procedure) ) THEN
         gme_debug.put_line (g_pkg_name || '.' || l_api_name || ':'
                             || 'Entering');
      END IF;

      /* Initialize return status to success */
      x_return_status := fnd_api.g_ret_sts_success;
      l_batch_header_rec.batch_id := p_batch_step_rec.batch_id;

      IF NOT (gme_batch_header_dbl.fetch_row (l_batch_header_rec
                                             ,x_batch_header_rec) ) THEN
         RAISE invalid_batch;
      END IF;

      OPEN cur_get_material (p_batch_step_rec.batch_id
                            ,p_batch_step_rec.batchstep_id);

      FETCH cur_get_material
      BULK COLLECT INTO l_material_detail_id_tbl, l_line_type_tbl;

      l_loop_count_get_material := cur_get_material%ROWCOUNT;

      CLOSE cur_get_material;

      FOR i IN 1 .. l_loop_count_get_material LOOP
         -- Navin Added as part of Reschedule Batch/Step Build.
         IF l_line_type_tbl (i) = gme_common_pvt.g_line_type_ing THEN
            -- Update the material_required_date with the associated plan_start_Date;
            l_material_date := p_start_date;
         ELSE
            -- Update the material required date with the associated plan cmplt Date;
            l_material_date := p_end_date;
         END IF;

         IF (NVL (g_debug, 0) = gme_debug.g_log_statement) THEN
            gme_debug.put_line
                          (   'Calling Material Date Change for batch_id : '
                           || l_batch_header_rec.batch_id
                           || ' Material_detail_id : '
                           || l_material_detail_id_tbl (i) );
         END IF;

         gme_common_pvt.material_date_change
                         (p_material_detail_id      => l_material_detail_id_tbl
                                                                           (i)
                         ,p_material_date           => l_material_date
                         ,x_return_status           => l_return_status);

         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line
                       (   'Came back from Material Date Change with status '
                        || l_return_status);
         END IF;
         --FPBug#4585491 Begin
         /*IF l_return_status <> x_return_status THEN
            RAISE mtl_dt_chg_error;
         END IF;*/

	 IF (l_return_status = fnd_api.g_ret_sts_error) THEN
             RAISE mtl_dt_chg_error;
          ELSIF (l_return_status = fnd_api.g_ret_sts_unexp_error) THEN
             RAISE fnd_api.g_exc_unexpected_error;
          END IF; 

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
      END LOOP;                    /* FOR i IN 1..l_loop_count_get_material */

      --FPBug#4585491 Begin
      IF (l_B_count > 0) OR (l_R_count > 0 AND l_M_count > 0) THEN 
       --atleast for one material line MO allocations and reservations are deleted
       gme_common_pvt.log_message('GME_EXPIRED_RESERV_MO_DELETED');
      ELSIF l_R_count > 0 THEN
       ----atleast for one material line reservations are deleted
       gme_common_pvt.log_message('GME_EXPIRED_RESERV_DELETED');
      ELSIF l_M_count > 0 THEN
       ----atleast for one material line MO allocations are deleted
       gme_common_pvt.log_message('GME_EXPIRED_MO_DELETED');
      END IF;
      x_return_status := fnd_api.g_ret_sts_success;      
      --FPBug#4585491 End


      /* Now we have to reschedule the batch associated with the step */
      /* lines of release type step release                           */
      /* All means manual, incremental and auto by step... NOT AUTO */
      IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
         gme_debug.put_line ('Calling Fetch Step Phantoms.');
      END IF;

      gme_phantom_pvt.fetch_step_phantoms
                             (p_batch_id                    => p_batch_step_rec.batch_id
                             ,p_batchstep_id                => p_batch_step_rec.batchstep_id
                             ,p_all_release_type_assoc      => 1
                             ,x_phantom_ids                 => l_phantom_ids
                             ,x_return_status               => l_return_status);

      IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
         gme_debug.put_line
                        (   'Came back from Fetch Step Phantoms with status '
                         || l_return_status);
      END IF;

      IF l_return_status <> x_return_status THEN
         RAISE resched_phant_error;
      END IF;

      FOR i IN 1 .. l_phantom_ids.COUNT LOOP
         l_batch_header_rec.batch_id := l_phantom_ids (i);
         -- Sending the completion date only as start date for the phantom batch
         l_batch_header_rec.plan_cmplt_date :=
                                             p_batch_step_rec.plan_start_date;
         l_in_batch_header_rec := l_batch_header_rec;
         gme_reschedule_batch_pvt.reschedule_batch
                             (p_batch_header_rec         => l_in_batch_header_rec
                             ,p_use_workday_cal          => p_use_workday_cal
                             ,p_contiguity_override      => p_contiguity_override
                             ,x_batch_header_rec         => l_batch_header_rec
                             ,x_return_status            => l_return_status);

         IF l_return_status <> x_return_status THEN
            RAISE resched_phant_error;
         END IF;
      END LOOP;                            /* i IN 1 .. l_phantom_ids.COUNT */

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
      WHEN resched_phant_error OR mtl_dt_chg_error OR invalid_batch THEN
         x_return_status := l_return_status;
      WHEN invalid_prior_dates THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN OTHERS THEN
         x_return_status := fnd_api.g_ret_sts_unexp_error;
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);

         IF (NVL (g_debug, 0) > 0) THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ':'
                                || ' OTHERS:'
                                || SQLERRM);
         END IF;
   END save_all_data;
END gme_reschedule_step_pvt;
/

COMMIT ;
EXIT;
--SHOW errors;
