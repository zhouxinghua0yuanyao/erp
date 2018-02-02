/* +======================================================================+ */
/* |    Copyright (c) 2005, 2015 Oracle and/or its affiliates.           | */
/* |                         All rights reserved.                         | */
/* |                           Version 12.0.0                             | */
/* +======================================================================+ */
REM dbdrv: sql ~PROD ~PATH ~FILE none none none package &phase=plb \
REM dbdrv: checkfile(120.2.12010000.5=120.2.12020000.5)(120.2.12000000.2=120.2.12010000.2)(115.12=120.1):~PROD:~PATH:~FILE
SET VERIFY OFF;
WHENEVER SQLERROR EXIT FAILURE ROLLBACK;
WHENEVER OSERROR EXIT FAILURE ROLLBACK;
REM****************************************************************
REM                                                               *
REM Package  GME_BATCHSTEP_ACTIVITY_PVT                           *
REM                                                               *
REM Contents INSERT ACTIVITY                                      *
REM         UPDATE ACTIVITY                                       *
REM         DELETE ACTIVITY                                       *
REM                                                               *
REM Use      This is the private layer of the GME Batch Step      *
REM         Activities                                            *
REM                                                               *
REM History                                                       *
REM         Jalaj Srivastava Created                              *
REM                                                               *       
REM  23-SEP-2012  G. Muratore      Bug 14637233                   *
REM    Comment out unnecessary validation when inserting an       *
REM     activity.                                                 *
REM    Procedure: insert_batchstep_activity                       *
REM  05-MAY-2015   Shaliu Chen      ER 20938455                   *
REM    modify public APIs to add validation to return             *
REM    failure if batch is on hold.                               *
REM                                                               *                                                            
REM  30-JUN-2015   Shaliu Chen      BUG 21345006                  *
REM  for step activity update,the fields can be update except     *
REM   actual fileds when batch is on hold with Pause type.        *
REM    Procedure: update_batchstep_activity                       *
REM****************************************************************

CREATE OR REPLACE PACKAGE BODY gme_batchstep_act_pvt AS
/*  $Header: GMEVACTB.pls 120.2.12020000.5 2015/07/01 05:31:07 shalchen ship $ */
/*  Global variables   */
   g_pkg_name   CONSTANT VARCHAR2 (30) := 'GME_BATCHSTEP_ACT_PVT';
   /*  Global variables   */
   g_debug               VARCHAR2 (5)  := fnd_profile.VALUE ('AFLOG_LEVEL');

/*===========================================================================================
   Procedure
      validate_param
   Description
     This procedure is used to validate parameter combo provided to identify a batch step
     passed to activity APIs
   Parameters
     (p_org_code,p_batch_no and step_no )   to uniquely identify a step
     x_return_status              reflects return status of the API
=============================================================================================*/
   PROCEDURE validate_param (
      p_org_code        IN              VARCHAR2 := NULL,
      p_batch_no        IN              VARCHAR2 := NULL,
      p_batchstep_no    IN              NUMBER := NULL,
      p_activity        IN              VARCHAR2 := NULL,
      x_batch_id        OUT NOCOPY      NUMBER,
      x_batchstep_id    OUT NOCOPY      NUMBER,
      x_activity_id     OUT NOCOPY      NUMBER,
      x_batch_status    OUT NOCOPY      NUMBER,
      x_step_status     OUT NOCOPY      NUMBER,
      x_return_status   OUT NOCOPY      VARCHAR2
   )
   IS
      l_api_name   CONSTANT VARCHAR2 (30) := 'validate_param';

      CURSOR cur_get_batch_dtl (v_organization_id NUMBER, v_batch_no VARCHAR2)
      IS
         SELECT batch_id, batch_status
           FROM gme_batch_header
          WHERE organization_id = v_organization_id AND batch_no = v_batch_no;

      CURSOR cur_get_batchstep_dtl (v_batch_id NUMBER, v_batchstep_no NUMBER)
      IS
         SELECT batchstep_id, step_status
           FROM gme_batch_steps
          WHERE batch_id = v_batch_id AND batchstep_no = v_batchstep_no;

      CURSOR cur_get_activity_id (
         v_step_id    NUMBER,
         v_activity   VARCHAR2,
         v_batch_id   NUMBER
      )
      IS
         SELECT batchstep_activity_id
           FROM gme_batch_step_activities
          WHERE batchstep_id = v_step_id
            AND batch_id = v_batch_id
            AND activity = v_activity;
   BEGIN
      IF (NVL (g_debug, 0) IN
                       (gme_debug.g_log_statement, gme_debug.g_log_procedure)
         )
      THEN
         gme_debug.put_line (g_pkg_name || '.' || l_api_name || ':'
                             || 'Entering'
                            );
      END IF;

      /* Set the return status to success initially */
      x_return_status := fnd_api.g_ret_sts_success;

      IF p_org_code IS NULL
      THEN
         gme_common_pvt.log_message ('GME_FIELD_VALUE_REQUIRED',
                                     'FIELD_NAME',
                                     'ORGANIZATION'
                                    );
         RAISE fnd_api.g_exc_error;
      ELSIF p_batch_no IS NULL
      THEN
         gme_common_pvt.log_message ('GME_FIELD_VALUE_REQUIRED',
                                     'FIELD_NAME',
                                     'BATCH NUMBER'
                                    );
         RAISE fnd_api.g_exc_error;
      ELSIF p_batchstep_no IS NULL
      THEN
         gme_common_pvt.log_message ('GME_FIELD_VALUE_REQUIRED',
                                     'FIELD_NAME',
                                     'BATCH STEP NUMBER'
                                    );
         RAISE fnd_api.g_exc_error;
      END IF;

      -- Validate input param one by one to see if it identifies an activity correctly
      OPEN cur_get_batch_dtl (gme_common_pvt.g_organization_id, p_batch_no);
      FETCH cur_get_batch_dtl INTO x_batch_id, x_batch_status;

      IF cur_get_batch_dtl%NOTFOUND
      THEN
         CLOSE cur_get_batch_dtl;
         gme_common_pvt.log_message ('GME_BATCH_NOT_FOUND');
         RAISE fnd_api.g_exc_error;
      END IF;

      CLOSE cur_get_batch_dtl;
      -- use batch_id to fetch batchstep_id
      OPEN cur_get_batchstep_dtl (x_batch_id, p_batchstep_no);
      FETCH cur_get_batchstep_dtl INTO x_batchstep_id, x_step_status;

      IF cur_get_batchstep_dtl%NOTFOUND
      THEN
         CLOSE cur_get_batchstep_dtl;
         gme_common_pvt.log_message ('PC_INV_BATCHSTEP_NO');
         RAISE fnd_api.g_exc_error;
      END IF;

      CLOSE cur_get_batchstep_dtl;

      IF p_activity IS NOT NULL
      THEN
         -- fetch activity_id if activity is provided
         OPEN cur_get_activity_id (x_batchstep_id, p_activity, x_batch_id);
         FETCH cur_get_activity_id INTO x_activity_id;

         IF cur_get_activity_id%NOTFOUND
         THEN
            CLOSE cur_get_activity_id;
            gme_common_pvt.log_message ('GME_STEP_ACTIVITY_NOT_FOUND',
                                        'ACTIVITY',
                                        p_activity,
                                        'STEP_NO',
                                        p_batchstep_no
                                       );
            RAISE fnd_api.g_exc_error;
         END IF;

         CLOSE cur_get_activity_id;
      END IF;

      IF (NVL (g_debug, 0) IN
                       (gme_debug.g_log_statement, gme_debug.g_log_procedure)
         )
      THEN
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ':'
                             || 'Exiting with '
                             || x_return_status
                            );
      END IF;
   EXCEPTION
      WHEN fnd_api.g_exc_error
      THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN fnd_api.g_exc_unexpected_error
      THEN
         x_return_status := fnd_api.g_ret_sts_unexp_error;

         IF (NVL (g_debug, 0) > 0)
         THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ':'
                                || 'UNEXPECTED:'
                                || SQLERRM
                               );
         END IF;
      WHEN OTHERS
      THEN
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
         x_return_status := fnd_api.g_ret_sts_unexp_error;

         IF (NVL (g_debug, 0) > 0)
         THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ':'
                                || 'OTHERS:'
                                || SQLERRM
                               );
         END IF;
   END validate_param;

/*===========================================================================================
   Procedure
      validate_activity_param
   Description
     This procedure is used to validate all parameters passed to insert activity API
   Parameters

     x_return_status              reflects return status of the API
=============================================================================================*/
   PROCEDURE validate_activity_param (
      p_batchstep_activity_rec   IN              gme_batch_step_activities%ROWTYPE,
      p_step_id                  IN              NUMBER,
      p_validate_flexfield       IN              VARCHAR2
            DEFAULT fnd_api.g_false,
      p_action                   IN              VARCHAR2,
      x_batchstep_activity_rec   OUT NOCOPY      gme_batch_step_activities%ROWTYPE,
      x_step_status              OUT NOCOPY      NUMBER,
      x_return_status            OUT NOCOPY      VARCHAR2
   )
   IS
      l_api_name        CONSTANT VARCHAR2 (30)   := 'validate_activity_param';
      l_batch_asqc               NUMBER;
      l_batchstep_activity_rec   gme_batch_step_activities%ROWTYPE;
      l_step_dtl                 gme_batch_steps%ROWTYPE;
      l_dummy                    NUMBER;
      l_activity_factor          NUMBER;
      l_batch_type               NUMBER;

      CURSOR cur_check_activity (v_activity VARCHAR2)
      IS
         SELECT 1
           FROM fm_actv_mst
          WHERE activity = v_activity AND delete_mark = 0;

      CURSOR cur_get_step_dtl (v_step_id NUMBER)
      IS
         SELECT *
           FROM gme_batch_steps
          WHERE batchstep_id = v_step_id;

      CURSOR cur_get_batch_asqc (v_batch_id NUMBER)
      IS
         SELECT automatic_step_calculation
           FROM gme_batch_header
          WHERE batch_id = v_batch_id;

      CURSOR cur_check_fpo (v_step_id NUMBER)
      IS
         SELECT batch_type
           FROM gme_batch_header b, gme_batch_steps s
          WHERE b.batch_id = s.batch_id AND batchstep_id = v_step_id;
   BEGIN
      IF (NVL (g_debug, 0) IN
                       (gme_debug.g_log_statement, gme_debug.g_log_procedure)
         )
      THEN
         gme_debug.put_line (g_pkg_name || '.' || l_api_name || ':'
                             || 'Entering'
                            );
      END IF;

      /* Set the return status to success initially */
      x_return_status := fnd_api.g_ret_sts_success;

      --{    
      IF (p_action = 'INSERT')
      THEN
         -- check FPO
         OPEN cur_check_fpo (p_step_id);
         FETCH cur_check_fpo INTO l_batch_type;

         IF l_batch_type = 10
         THEN
            CLOSE cur_check_fpo;
            gme_common_pvt.log_message ('GME_FPO_ACTV_NO_INS');
            RAISE fnd_api.g_exc_error;
         END IF;

         CLOSE cur_check_fpo;
         -- check activity
         OPEN cur_check_activity (p_batchstep_activity_rec.activity);
         FETCH cur_check_activity INTO l_dummy;

         IF cur_check_activity%NOTFOUND
         THEN
            CLOSE cur_check_activity;
            gme_common_pvt.log_message ('GME_BAD_ACTIVITY');
            RAISE fnd_api.g_exc_error;
         END IF;

         CLOSE cur_check_activity;
         -- Fetch step info
         OPEN cur_get_step_dtl (p_step_id);
         FETCH cur_get_step_dtl INTO l_step_dtl;

         IF cur_get_step_dtl%NOTFOUND
         THEN
            CLOSE cur_get_step_dtl;
            gme_common_pvt.log_message ('GME_BATCH_STEP_NOT_FOUND',
                                        'STEP_ID',
                                        p_step_id
                                       );
            RAISE fnd_api.g_exc_error;
         END IF;

         CLOSE cur_get_step_dtl;

         IF l_step_dtl.step_status IN (4, 5)
         THEN -- Closed or cancelled step
            gme_common_pvt.log_message ('PC_STEP_STATUS_ERR');
            RAISE fnd_api.g_exc_error;
         END IF;

         -- check ASQC property - can't insert into WIP step for ASQC batch
         OPEN cur_get_batch_asqc (l_step_dtl.batch_id);
         FETCH cur_get_batch_asqc INTO l_batch_asqc;
         CLOSE cur_get_batch_asqc;

         IF l_batch_asqc = 1 AND l_step_dtl.step_status = 2
         THEN
            gme_common_pvt.log_message ('GME_INVALID_ASQC_ACTION_ACTV');
            RAISE fnd_api.g_exc_error;
         END IF;

         -- check plan activity factor    
         IF     l_step_dtl.step_status = 1
            AND p_batchstep_activity_rec.plan_activity_factor IS NULL
         THEN
            gme_common_pvt.log_message ('GME_FIELD_VALUE_REQUIRED',
                                        'FIELD_NAME',
                                        'PLAN_ACTIVITY_FACTOR'
                                       );
            RAISE fnd_api.g_exc_error;
         END IF;

         -- check actual activity factor    
         IF     l_step_dtl.step_status IN (2, 3)
            AND p_batchstep_activity_rec.actual_activity_factor IS NULL
         THEN
            gme_common_pvt.log_message ('GME_FIELD_VALUE_REQUIRED',
                                        'FIELD_NAME',
                                        'ACTUAL_ACTIVITY_FACTOR'
                                       );
            RAISE fnd_api.g_exc_error;
         END IF;

         l_batchstep_activity_rec := p_batchstep_activity_rec;
         l_batchstep_activity_rec.material_ind := NULL;

         IF l_step_dtl.step_status IN (2, 3)
         THEN
            l_batchstep_activity_rec.plan_activity_factor := NULL;
         ELSIF l_step_dtl.step_status = 1
         THEN
            l_batchstep_activity_rec.actual_activity_factor := NULL;
         END IF;

         IF l_step_dtl.step_status IN (1, 2)
         THEN
            -- Check that the activity plan dates fall within the step dates.
            IF     p_batchstep_activity_rec.plan_start_date IS NOT NULL
               AND (   p_batchstep_activity_rec.plan_start_date <
                                                    l_step_dtl.plan_start_date
                    OR p_batchstep_activity_rec.plan_start_date >
                                                    l_step_dtl.plan_cmplt_date
                   )
            THEN
               gme_common_pvt.log_message ('GME_ACTV_PLAN_DATE',
                                           'ACTIVITY',
                                           p_batchstep_activity_rec.activity
                                          );
               RAISE fnd_api.g_exc_error;
            END IF;

            IF     p_batchstep_activity_rec.plan_start_date IS NOT NULL
               AND p_batchstep_activity_rec.plan_cmplt_date IS NOT NULL
               AND (p_batchstep_activity_rec.plan_start_date >
                                      p_batchstep_activity_rec.plan_cmplt_date
                   )
            THEN
               gme_common_pvt.log_message ('PM_BADSTARTDATE');
               RAISE fnd_api.g_exc_error;
            END IF;

            IF     p_batchstep_activity_rec.plan_cmplt_date IS NOT NULL
               AND (p_batchstep_activity_rec.plan_cmplt_date >
                                                    l_step_dtl.plan_cmplt_date
                   )
            THEN
               gme_common_pvt.log_message ('GME_ACTV_PLAN_DATE',
                                           'ACTIVITY',
                                           p_batchstep_activity_rec.activity
                                          );
               RAISE fnd_api.g_exc_error;
            END IF;

            -- Default the plan dates if they were not passed in
            IF p_batchstep_activity_rec.plan_start_date IS NULL
            THEN
               l_batchstep_activity_rec.plan_start_date :=
                     l_step_dtl.plan_start_date
                   + NVL (p_batchstep_activity_rec.offset_interval / 24, 0);
            END IF;

            IF p_batchstep_activity_rec.plan_cmplt_date IS NULL
            THEN
               l_batchstep_activity_rec.plan_cmplt_date :=
                                                   l_step_dtl.plan_cmplt_date;
            END IF;
         ELSE
            l_batchstep_activity_rec.plan_start_date := NULL;
            l_batchstep_activity_rec.plan_cmplt_date := NULL;
         END IF;

         IF l_step_dtl.step_status IN (2, 3)
         THEN
            -- Check that actual start date is not in the future
            IF     p_batchstep_activity_rec.actual_start_date IS NOT NULL
               AND p_batchstep_activity_rec.actual_start_date >
                                                    gme_common_pvt.g_timestamp
            THEN
               gme_common_pvt.log_message (p_message_code      => 'SY_NOFUTUREDATE',
                                           p_product_code      => 'GMA'
                                          );
               RAISE fnd_api.g_exc_error;
            END IF;

            -- Check that the activity actual dates fall within the step dates.
            IF     p_batchstep_activity_rec.actual_start_date IS NOT NULL
               AND (   p_batchstep_activity_rec.actual_start_date <
                                                  l_step_dtl.actual_start_date
                    OR p_batchstep_activity_rec.actual_start_date >
                                                  l_step_dtl.actual_cmplt_date
                   )
            THEN
               gme_common_pvt.log_message ('GME_ACTV_ACTUAL_DATE',
                                           'ACTIVITY',
                                           p_batchstep_activity_rec.activity
                                          );
               RAISE fnd_api.g_exc_error;
            END IF;

            -- default actual start date if not supplied
            IF p_batchstep_activity_rec.actual_start_date IS NULL
            THEN
               l_batchstep_activity_rec.actual_start_date :=
                                                 l_step_dtl.actual_start_date;
            END IF;
         ELSE
            l_batchstep_activity_rec.actual_start_date := NULL;
         END IF;

         IF l_step_dtl.step_status = 3
         THEN
            -- Check that actual cmplt date is not in the future
            IF     p_batchstep_activity_rec.actual_cmplt_date IS NOT NULL
               AND p_batchstep_activity_rec.actual_cmplt_date >
                                                    gme_common_pvt.g_timestamp
            THEN
               fnd_message.set_name ('GMA', 'SY_NOFUTUREDATE');
               fnd_msg_pub.ADD;
               RAISE fnd_api.g_exc_error;
            END IF;

            IF     p_batchstep_activity_rec.actual_start_date IS NOT NULL
               AND p_batchstep_activity_rec.actual_cmplt_date IS NOT NULL
               AND (p_batchstep_activity_rec.actual_start_date >
                                    p_batchstep_activity_rec.actual_cmplt_date
                   )
            THEN
               gme_common_pvt.log_message ('PM_BADSTARTDATE');
               RAISE fnd_api.g_exc_error;
            END IF;

            IF     p_batchstep_activity_rec.actual_cmplt_date IS NOT NULL
               AND p_batchstep_activity_rec.actual_cmplt_date >
                                                  l_step_dtl.actual_cmplt_date
            THEN
               gme_common_pvt.log_message ('GME_ACTV_ACTUAL_DATE',
                                           'ACTIVITY',
                                           p_batchstep_activity_rec.activity
                                          );
               RAISE fnd_api.g_exc_error;
            END IF;

            IF p_batchstep_activity_rec.actual_cmplt_date IS NULL
            THEN
               l_batchstep_activity_rec.actual_cmplt_date :=
                                                 l_step_dtl.actual_cmplt_date;
            END IF;
         ELSE
            l_batchstep_activity_rec.actual_cmplt_date := NULL;
         END IF;

         x_step_status := l_step_dtl.step_status;

         -- ensure activity factor is NOT 0
         IF x_step_status = 1
         THEN
            l_activity_factor :=
                                p_batchstep_activity_rec.plan_activity_factor;
         ELSE
            l_activity_factor :=
                              p_batchstep_activity_rec.actual_activity_factor;
         END IF;

         IF l_activity_factor = 0
         THEN
            gme_common_pvt.log_message ('GME_ZERO_ACT_FACT');
            RAISE fnd_api.g_exc_error;
         END IF;

         --{
         IF (    (fnd_api.to_boolean (p_validate_flexfield))
             AND (   p_batchstep_activity_rec.attribute_category IS NOT NULL
                  OR p_batchstep_activity_rec.attribute1 IS NOT NULL
                  OR p_batchstep_activity_rec.attribute2 IS NOT NULL
                  OR p_batchstep_activity_rec.attribute3 IS NOT NULL
                  OR p_batchstep_activity_rec.attribute4 IS NOT NULL
                  OR p_batchstep_activity_rec.attribute5 IS NOT NULL
                  OR p_batchstep_activity_rec.attribute6 IS NOT NULL
                  OR p_batchstep_activity_rec.attribute7 IS NOT NULL
                  OR p_batchstep_activity_rec.attribute8 IS NOT NULL
                  OR p_batchstep_activity_rec.attribute9 IS NOT NULL
                  OR p_batchstep_activity_rec.attribute10 IS NOT NULL
                  OR p_batchstep_activity_rec.attribute11 IS NOT NULL
                  OR p_batchstep_activity_rec.attribute12 IS NOT NULL
                  OR p_batchstep_activity_rec.attribute13 IS NOT NULL
                  OR p_batchstep_activity_rec.attribute14 IS NOT NULL
                  OR p_batchstep_activity_rec.attribute15 IS NOT NULL
                  OR p_batchstep_activity_rec.attribute16 IS NOT NULL
                  OR p_batchstep_activity_rec.attribute17 IS NOT NULL
                  OR p_batchstep_activity_rec.attribute18 IS NOT NULL
                  OR p_batchstep_activity_rec.attribute19 IS NOT NULL
                  OR p_batchstep_activity_rec.attribute20 IS NOT NULL
                  OR p_batchstep_activity_rec.attribute21 IS NOT NULL
                  OR p_batchstep_activity_rec.attribute22 IS NOT NULL
                  OR p_batchstep_activity_rec.attribute23 IS NOT NULL
                  OR p_batchstep_activity_rec.attribute24 IS NOT NULL
                  OR p_batchstep_activity_rec.attribute25 IS NOT NULL
                  OR p_batchstep_activity_rec.attribute26 IS NOT NULL
                  OR p_batchstep_activity_rec.attribute27 IS NOT NULL
                  OR p_batchstep_activity_rec.attribute28 IS NOT NULL
                  OR p_batchstep_activity_rec.attribute29 IS NOT NULL
                  OR p_batchstep_activity_rec.attribute30 IS NOT NULL
                 )
            )
         THEN
            gme_validate_flex_fld_pvt.validate_flex_step_activities (p_step_activities      => p_batchstep_activity_rec,
                                                                     x_step_activities      => x_batchstep_activity_rec,
                                                                     x_return_status        => x_return_status
                                                                    );

            IF (x_return_status = fnd_api.g_ret_sts_error)
            THEN
               RAISE fnd_api.g_exc_error;
            ELSIF (x_return_status = fnd_api.g_ret_sts_unexp_error)
            THEN
               RAISE fnd_api.g_exc_unexpected_error;
            END IF;

            l_batchstep_activity_rec.attribute_category :=
                                   x_batchstep_activity_rec.attribute_category;
            l_batchstep_activity_rec.attribute1 :=
                                           x_batchstep_activity_rec.attribute1;
            l_batchstep_activity_rec.attribute2 :=
                                           x_batchstep_activity_rec.attribute2;
            l_batchstep_activity_rec.attribute3 :=
                                           x_batchstep_activity_rec.attribute3;
            l_batchstep_activity_rec.attribute4 :=
                                           x_batchstep_activity_rec.attribute4;
            l_batchstep_activity_rec.attribute5 :=
                                           x_batchstep_activity_rec.attribute5;
            l_batchstep_activity_rec.attribute6 :=
                                           x_batchstep_activity_rec.attribute6;
            l_batchstep_activity_rec.attribute7 :=
                                           x_batchstep_activity_rec.attribute7;
            l_batchstep_activity_rec.attribute8 :=
                                           x_batchstep_activity_rec.attribute8;
            l_batchstep_activity_rec.attribute9 :=
                                           x_batchstep_activity_rec.attribute9;
            l_batchstep_activity_rec.attribute10 :=
                                          x_batchstep_activity_rec.attribute10;
            l_batchstep_activity_rec.attribute11 :=
                                          x_batchstep_activity_rec.attribute11;
            l_batchstep_activity_rec.attribute12 :=
                                          x_batchstep_activity_rec.attribute12;
            l_batchstep_activity_rec.attribute13 :=
                                          x_batchstep_activity_rec.attribute13;
            l_batchstep_activity_rec.attribute14 :=
                                          x_batchstep_activity_rec.attribute14;
            l_batchstep_activity_rec.attribute15 :=
                                          x_batchstep_activity_rec.attribute15;
            l_batchstep_activity_rec.attribute16 :=
                                          x_batchstep_activity_rec.attribute16;
            l_batchstep_activity_rec.attribute17 :=
                                          x_batchstep_activity_rec.attribute17;
            l_batchstep_activity_rec.attribute18 :=
                                          x_batchstep_activity_rec.attribute18;
            l_batchstep_activity_rec.attribute19 :=
                                          x_batchstep_activity_rec.attribute19;
            l_batchstep_activity_rec.attribute20 :=
                                          x_batchstep_activity_rec.attribute20;
            l_batchstep_activity_rec.attribute21 :=
                                          x_batchstep_activity_rec.attribute21;
            l_batchstep_activity_rec.attribute22 :=
                                          x_batchstep_activity_rec.attribute22;
            l_batchstep_activity_rec.attribute23 :=
                                          x_batchstep_activity_rec.attribute23;
            l_batchstep_activity_rec.attribute24 :=
                                          x_batchstep_activity_rec.attribute24;
            l_batchstep_activity_rec.attribute25 :=
                                          x_batchstep_activity_rec.attribute25;
            l_batchstep_activity_rec.attribute26 :=
                                          x_batchstep_activity_rec.attribute26;
            l_batchstep_activity_rec.attribute27 :=
                                          x_batchstep_activity_rec.attribute27;
            l_batchstep_activity_rec.attribute28 :=
                                          x_batchstep_activity_rec.attribute28;
            l_batchstep_activity_rec.attribute29 :=
                                          x_batchstep_activity_rec.attribute29;
            l_batchstep_activity_rec.attribute30 :=
                                          x_batchstep_activity_rec.attribute30;
         END IF; --}
      END IF; --}  

      x_batchstep_activity_rec := l_batchstep_activity_rec;

      IF (NVL (g_debug, 0) IN
                       (gme_debug.g_log_statement, gme_debug.g_log_procedure)
         )
      THEN
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ':'
                             || 'Exiting with '
                             || x_return_status
                            );
      END IF;
   EXCEPTION
      WHEN fnd_api.g_exc_error
      THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN fnd_api.g_exc_unexpected_error
      THEN
         x_return_status := fnd_api.g_ret_sts_unexp_error;

         IF (NVL (g_debug, 0) > 0)
         THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ':'
                                || 'UNEXPECTED:'
                                || SQLERRM
                               );
         END IF;
      WHEN OTHERS
      THEN
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
         x_return_status := fnd_api.g_ret_sts_unexp_error;

         IF (NVL (g_debug, 0) > 0)
         THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ':'
                                || 'OTHERS:'
                                || SQLERRM
                               );
         END IF;
   END validate_activity_param;

/*===========================================================================================
   Procedure
     insert_batchstep_activity
   Description
     This procedure is used to insert activity for a batch step
   Parameters
     p_batchstep_id OR (p_org_code,p_batch_no,step_no) to uniquely identify a step
     p_batchstep_activity_rec       GME_BATCH_STEP_ACTIVITIES%ROWTYPE   - details of the activity
     p_batchstep_resource_tbl       gme_create_step_pvt.resources_tab   - details of the rsrc
     x_batchstep_activity_rec       GME_BATCH_STEP_ACTIVITIES%ROWTYPE   - returns the newly inserted row
     x_return_status                reflects return status of the API
     
   History
      23-SEP-2012  G. Muratore      Bug 14637233 
        Comment out unnecessary validation when inserting an activity. 
        
      05-MAY-2015  Shaliu Chen       ER 20938455
        Modify for Batch On Hold enhancement,add validation to raise an error 
        if batch is on hold         
=============================================================================================*/
   PROCEDURE insert_batchstep_activity (
      p_batchstep_activity_rec   IN              gme_batch_step_activities%ROWTYPE,
      p_batchstep_resource_tbl   IN              gme_create_step_pvt.resources_tab,
      p_org_code                 IN              VARCHAR2 := NULL,
      p_batch_no                 IN              VARCHAR2 := NULL,
      p_batchstep_no             IN              NUMBER := NULL,
      p_ignore_qty_below_cap     IN              VARCHAR2
            DEFAULT fnd_api.g_false,
      p_validate_flexfield       IN              VARCHAR2
            DEFAULT fnd_api.g_false,
      x_batchstep_activity_rec   OUT NOCOPY      gme_batch_step_activities%ROWTYPE,
      x_return_status            OUT NOCOPY      VARCHAR2
   )
   IS
      l_api_name            CONSTANT VARCHAR2 (30)
                                               := 'insert_batchstep_activity';
      l_batch_id                     NUMBER;
      l_batchstep_id                 NUMBER;
      l_activity_id                  NUMBER;
      l_batch_status                 NUMBER;
      l_step_status                  NUMBER;
      l_organization_id              PLS_INTEGER;
      l_batchstep_activity_rec       gme_batch_step_activities%ROWTYPE;
      l_batchstep_activity_out_rec   gme_batch_step_activities%ROWTYPE;
      l_batchstep_resource_rec       gme_batch_step_resources%ROWTYPE;
      l_batchstep_resource_out_rec   gme_batch_step_resources%ROWTYPE;
      l_prim_rsrc_count              NUMBER;
      l_rsrc_id                      PLS_INTEGER;

      -- Define CURSORS
      CURSOR cur_validate_step (v_step_id NUMBER)
      IS
         SELECT batch_id
           FROM gme_batch_steps
          WHERE batchstep_id = v_step_id;
   BEGIN
      IF (NVL (g_debug, 0) IN
                       (gme_debug.g_log_statement, gme_debug.g_log_procedure)
         )
      THEN
         gme_debug.put_line (g_pkg_name || '.' || l_api_name || ':'
                             || 'Entering'
                            );
      END IF;

      /* Set the return status to success initially */
      x_return_status := fnd_api.g_ret_sts_success;

      IF p_batchstep_activity_rec.activity IS NULL
      THEN
         gme_common_pvt.log_message ('GME_FIELD_VALUE_REQUIRED',
                                     'FIELD_NAME',
                                     'ACTIVITY'
                                    );
         RAISE fnd_api.g_exc_error;
      END IF;

      IF p_batchstep_activity_rec.batchstep_id IS NOT NULL
      THEN
         -- validate the key provided
         l_batchstep_id := p_batchstep_activity_rec.batchstep_id;
         OPEN cur_validate_step (l_batchstep_id);
         FETCH cur_validate_step INTO l_batch_id;

         IF cur_validate_step%NOTFOUND
         THEN
            CLOSE cur_validate_step;
            gme_common_pvt.log_message ('GME_BATCH_STEP_NOT_FOUND',
                                        'STEP_ID',
                                        l_batchstep_id
                                       );
            RAISE fnd_api.g_exc_error;
         END IF;

         CLOSE cur_validate_step;
      ELSE
         -- validate the combination provided
         validate_param (p_org_code           => p_org_code,
                         p_batch_no           => p_batch_no,
                         p_batchstep_no       => p_batchstep_no,
                         p_activity           => p_batchstep_activity_rec.activity,
                         x_batch_id           => l_batch_id,
                         x_batchstep_id       => l_batchstep_id,
                         x_activity_id        => l_activity_id,
                         x_batch_status       => l_batch_status,
                         x_step_status        => l_step_status,
                         x_return_status      => x_return_status
                        );

         IF (NVL (g_debug, 0) = gme_debug.g_log_statement)
         THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ':'
                                || 'after the call to procedure validate_param '
                                || x_return_status
                               );
         END IF;

         IF (x_return_status = fnd_api.g_ret_sts_error)
         THEN
            RAISE fnd_api.g_exc_error;
         ELSIF (x_return_status = fnd_api.g_ret_sts_unexp_error)
         THEN
            RAISE fnd_api.g_exc_unexpected_error;
         END IF;
      END IF;
      
      /* Shaliu Chen     05-MAY-2015  ER 20938455
         raise an error if batch is on hold
      */
      IF gme_common_pvt.get_batch_hold_status(l_batch_id) = 'S' THEN
        gme_common_pvt.log_message('GME_BATCH_ONHOLD_CHECK',
                                   'ACTION_NAME',
                                   'Insert Batchstep Activity');        
        RAISE fnd_api.g_exc_error;        
      END IF;                
        

      validate_activity_param (p_batchstep_activity_rec      => p_batchstep_activity_rec,
                               p_step_id                     => l_batchstep_id,
                               p_validate_flexfield          => p_validate_flexfield,
                               p_action                      => 'INSERT',
                               x_batchstep_activity_rec      => l_batchstep_activity_rec,
                               x_step_status                 => l_step_status,
                               x_return_status               => x_return_status
                              );

      IF (NVL (g_debug, 0) = gme_debug.g_log_statement)
      THEN
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ':'
                             || 'after the call to procedure validate_activity_param '
                             || x_return_status
                            );
      END IF;

      IF (x_return_status = fnd_api.g_ret_sts_error)
      THEN
         RAISE fnd_api.g_exc_error;
      ELSIF (x_return_status = fnd_api.g_ret_sts_unexp_error)
      THEN
         RAISE fnd_api.g_exc_unexpected_error;
      END IF;

      l_batchstep_activity_rec.batch_id := l_batch_id;
      l_batchstep_activity_rec.batchstep_id := l_batchstep_id;
      l_batchstep_activity_rec.oprn_line_id := NULL;
      l_batchstep_activity_rec.offset_interval :=
                             NVL (l_batchstep_activity_rec.offset_interval, 0);
      l_batchstep_activity_rec.delete_mark := 0;

      -- check that there are resources and that there is one (and only one) primary resource
      IF p_batchstep_resource_tbl.COUNT = 0
      THEN
         gme_common_pvt.log_message (p_message_code      => 'GME_RESOURCE_NOT_ATTACH',
                                     p_token1_name       => 'ACTIVITY',
                                     p_token1_value      => p_batchstep_activity_rec.activity,
                                     p_token2_name       => 'STEPNO',
                                     p_token2_value      => p_batchstep_no
                                    );
         RAISE fnd_api.g_exc_error;
      END IF;

      l_prim_rsrc_count := 0;

      FOR i IN 1 .. p_batchstep_resource_tbl.COUNT
      LOOP
      -- Bug 14637233 - Comment following block. There is no need to validate
      -- the existence of the resource in the batch when inserting a new activity.
/*      
         gme_batchstep_rsrc_pvt.validate_param (p_org_code             => p_org_code,
                                                p_batch_no             => p_batch_no,
                                                p_batchstep_no         => p_batchstep_no,
                                                p_activity             => p_batchstep_activity_rec.activity,
                                                p_resource             => p_batchstep_resource_tbl (i
                                                                                                   ).resources,
                                                x_organization_id      => l_organization_id,
                                                x_batch_id             => l_batch_id,
                                                x_batchstep_id         => l_batchstep_id,
                                                x_activity_id          => l_activity_id,
                                                x_rsrc_id              => l_rsrc_id,
                                                x_step_status          => l_step_status,
                                                x_return_status        => x_return_status
                                               );

         IF (x_return_status = fnd_api.g_ret_sts_error)
         THEN
            RAISE fnd_api.g_exc_error;
         ELSIF (x_return_status = fnd_api.g_ret_sts_unexp_error)
         THEN
            RAISE fnd_api.g_exc_unexpected_error;
         END IF;

         gme_batchstep_rsrc_pvt.validate_rsrc_param (p_batchstep_resource_rec      => p_batchstep_resource_tbl (i
                                                                                                               ),
                                                     p_activity_id                 => l_activity_id,
                                                     p_ignore_qty_below_cap        => p_ignore_qty_below_cap,
                                                     p_validate_flexfield          => p_validate_flexfield,
                                                     p_action                      => 'INSERT',
                                                     x_batchstep_resource_rec      => l_batchstep_resource_rec,
                                                     x_step_status                 => l_step_status,
                                                     x_return_status               => x_return_status
                                                    );

         IF (x_return_status = fnd_api.g_ret_sts_error)
         THEN
            RAISE fnd_api.g_exc_error;
         ELSIF (x_return_status = fnd_api.g_ret_sts_unexp_error)
         THEN
            RAISE fnd_api.g_exc_unexpected_error;
         END IF;
*/
         IF NVL (p_batchstep_resource_tbl (i).prim_rsrc_ind, 0) = 1
         THEN
            l_prim_rsrc_count := l_prim_rsrc_count + 1;
         END IF;
      END LOOP;

      IF l_prim_rsrc_count <> 1
      THEN
         gme_common_pvt.log_message ('GME_ONLY_ONE_PRIM_RSRC',
                                     'ACTIVITY',
                                     l_batchstep_activity_rec.activity,
                                     'STEPNO',
                                     p_batchstep_no
                                    );
         RAISE fnd_api.g_exc_error;
      END IF;

      IF (NVL (g_debug, 0) = gme_debug.g_log_statement)
      THEN
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ':'
                             || 'before call to GME_BATCH_STEP_ACTIVITIES_DBL.insert_row batch_id='
                             || l_batchstep_activity_rec.batch_id
                             || ' and step_id ='
                             || l_batchstep_activity_rec.batchstep_id
                            );
      END IF;

      IF NOT (gme_batch_step_activities_dbl.insert_row (l_batchstep_activity_rec,
                                                        l_batchstep_activity_out_rec
                                                       )
             )
      THEN
         RAISE fnd_api.g_exc_error;
      END IF;

      IF (NVL (g_debug, -1) = gme_debug.g_log_statement)
      THEN
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ':'
                             || 'after call to GME_BATCH_STEP_ACTIVITIES_DBL.insert_row activity_id='
                             || l_batchstep_activity_out_rec.batchstep_activity_id
                            );
      END IF;

      gme_batch_step_chg_pvt.set_activity_sequence_num (p_batchstep_activity_rec.batch_id
                                                       );

      FOR i IN 1 .. p_batchstep_resource_tbl.COUNT
      LOOP
         l_batchstep_resource_rec := p_batchstep_resource_tbl (i);
         l_batchstep_resource_rec.batch_id :=
                           l_batchstep_activity_rec.batch_id;
         l_batchstep_resource_rec.batchstep_id :=                           
                           l_batchstep_activity_rec.batchstep_id;
         l_batchstep_resource_rec.batchstep_activity_id :=
                           l_batchstep_activity_out_rec.batchstep_activity_id;
         gme_batchstep_rsrc_pvt.insert_batchstep_rsrc (p_batchstep_resource_rec      => l_batchstep_resource_rec,
                                                       x_batchstep_resource_rec      => l_batchstep_resource_out_rec,
                                                       x_return_status               => x_return_status
                                                      );

         IF (x_return_status = fnd_api.g_ret_sts_error)
         THEN
            RAISE fnd_api.g_exc_error;
         ELSIF (x_return_status = fnd_api.g_ret_sts_unexp_error)
         THEN
            RAISE fnd_api.g_exc_unexpected_error;
         END IF;
      END LOOP;

      x_batchstep_activity_rec := l_batchstep_activity_out_rec;

      IF (NVL (g_debug, 0) IN
                       (gme_debug.g_log_statement, gme_debug.g_log_procedure)
         )
      THEN
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ':'
                             || 'Exiting with '
                             || x_return_status
                            );
      END IF;
   EXCEPTION
      WHEN fnd_api.g_exc_error
      THEN
         x_batchstep_activity_rec := NULL;
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN fnd_api.g_exc_unexpected_error
      THEN
         x_batchstep_activity_rec := NULL;
         x_return_status := fnd_api.g_ret_sts_unexp_error;

         IF (NVL (g_debug, 0) > 0)
         THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ':'
                                || 'UNEXPECTED:'
                                || SQLERRM
                               );
         END IF;
      WHEN OTHERS
      THEN
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
         x_batchstep_activity_rec := NULL;
         x_return_status := fnd_api.g_ret_sts_unexp_error;

         IF (NVL (g_debug, 0) > 0)
         THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ':'
                                || 'OTHERS:'
                                || SQLERRM
                               );
         END IF;
   END insert_batchstep_activity;

/*===========================================================================================
   Procedure
      update_batchstep_activity
   Description
     This procedure is used to update an activity
   Parameters
     p_batchstep_activity_id OR
     (p_org_code,p_batch_no,step_no,activity)   to uniquely identify an activity
     p_batchstep_activity_rec     GME_BATCH_STEP_ACTIVITIES%ROWTYPE  - details of the activity
     x_batchstep_activity_rec     returns the newly updated row for further processing
     x_return_status              reflects return status of the API
   History
=============================================================================================*/
   PROCEDURE update_batchstep_activity (
      p_batchstep_activity_rec   IN              gme_batch_step_activities%ROWTYPE,
      p_org_code                 IN              VARCHAR2 := NULL,
      p_batch_no                 IN              VARCHAR2 := NULL,
      p_batchstep_no             IN              NUMBER := NULL,
      p_validate_flexfield       IN              VARCHAR2
            DEFAULT fnd_api.g_false,
      x_batchstep_activity_rec   OUT NOCOPY      gme_batch_step_activities%ROWTYPE,
      x_return_status            OUT NOCOPY      VARCHAR2
   )
   IS
      l_api_name            CONSTANT VARCHAR2 (30)
                                               := 'update_batchstep_activity';
      l_batch_id                     NUMBER;
      l_activity_id                  NUMBER;
      l_activity                     gme_batch_step_activities.activity%TYPE;
      l_batchstep_id                 NUMBER;
      l_batch_status                 NUMBER;
      l_step_status                  NUMBER;
      l_batch_type                   NUMBER;
      l_field_updated                BOOLEAN                         := FALSE;
      l_upd_plan_date                BOOLEAN                         := FALSE;
      l_upd_act_date                 BOOLEAN                         := FALSE;
      l_update_resources             BOOLEAN                         := FALSE;
      l_flex_validate                BOOLEAN                         := FALSE;
      l_last_update_date             DATE;
      l_batchstep_activity_rec       gme_batch_step_activities%ROWTYPE;
      l_batchstep_activity_out_rec   gme_batch_step_activities%ROWTYPE;
      --Bug 3027004 Mohit Kapoor Added two variables
      l_inv_trans_count              NUMBER;
      l_rsrc_trans_count             NUMBER;

      -- Define CURSORS  

      CURSOR cur_dates (v_act_id NUMBER)
      IS
         SELECT *
           FROM gme_batch_step_resources
          WHERE batchstep_activity_id = v_act_id;

      l_temp_resources               cur_dates%ROWTYPE;
      l_resource                     VARCHAR2 (1000);

      CURSOR cur_get_dtl (v_act_id NUMBER)
      IS
         SELECT a.batch_id, a.batchstep_id, step_status, activity
           FROM gme_batch_step_activities a, gme_batch_steps s
          WHERE batchstep_activity_id = v_act_id
            AND a.batchstep_id = s.batchstep_id;

      CURSOR cur_get_step_dtl (v_step_id NUMBER)
      IS
         SELECT *
           FROM gme_batch_steps
          WHERE batchstep_id = v_step_id;

      l_step_dtl                     gme_batch_steps%ROWTYPE;

      CURSOR cur_get_batch (v_batch_id NUMBER)
      IS
         SELECT *
           FROM gme_batch_header
          WHERE batch_id = v_batch_id;

      l_batch_hdr                    gme_batch_header%ROWTYPE;

      CURSOR cur_check_fpo (v_actv_id NUMBER)
      IS
         SELECT batch_type
           FROM gme_batch_header b, gme_batch_step_activities a
          WHERE b.batch_id = a.batch_id AND batchstep_activity_id = v_actv_id;

      CURSOR cur_lock_actv (v_activity_id NUMBER)
      IS
         SELECT        last_update_date
                  FROM gme_batch_step_activities
                 WHERE batchstep_activity_id = v_activity_id
         FOR UPDATE OF last_update_date NOWAIT;

      l_seq_dep_ind                  gme_batch_step_activities.sequence_dependent_ind%TYPE;
   BEGIN
      IF (NVL (g_debug, 0) IN
                       (gme_debug.g_log_statement, gme_debug.g_log_procedure)
         )
      THEN
         gme_debug.put_line (g_pkg_name || '.' || l_api_name || ':'
                             || 'Entering'
                            );
      END IF;

      /* Set the return status to success initially */
      x_return_status := fnd_api.g_ret_sts_success;

      IF (    p_batchstep_activity_rec.activity IS NULL
          AND p_batchstep_activity_rec.batchstep_activity_id IS NULL
         )
      THEN
         gme_common_pvt.log_message ('GME_FIELD_VALUE_REQUIRED',
                                     'FIELD_NAME',
                                     'ACTIVITY'
                                    );
         RAISE fnd_api.g_exc_error;
      END IF;

      IF p_batchstep_activity_rec.batchstep_activity_id IS NOT NULL
      THEN
         -- validate the key provided
         l_activity_id := p_batchstep_activity_rec.batchstep_activity_id;
         OPEN cur_get_dtl (l_activity_id);
         FETCH cur_get_dtl INTO l_batch_id, l_batchstep_id, l_step_status,
          l_activity;

         IF cur_get_dtl%NOTFOUND
         THEN
            CLOSE cur_get_dtl;
            gme_common_pvt.log_message ('GME_ACTID_NOT_FOUND');
            RAISE fnd_api.g_exc_error;
         END IF;

         CLOSE cur_get_dtl;
      ELSE
         -- validate the combination provided
         l_activity := p_batchstep_activity_rec.activity;
         validate_param (p_org_code           => p_org_code,
                         p_batch_no           => p_batch_no,
                         p_batchstep_no       => p_batchstep_no,
                         p_activity           => l_activity,
                         x_batch_id           => l_batch_id,
                         x_batchstep_id       => l_batchstep_id,
                         x_activity_id        => l_activity_id,
                         x_batch_status       => l_batch_status,
                         x_step_status        => l_step_status,
                         x_return_status      => x_return_status
                        );

         IF (NVL (g_debug, 0) = gme_debug.g_log_statement)
         THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ':'
                                || 'after the call to procedure validate_param '
                                || x_return_status
                               );
         END IF;

         IF (x_return_status = fnd_api.g_ret_sts_error)
         THEN
            RAISE fnd_api.g_exc_error;
         ELSIF (x_return_status = fnd_api.g_ret_sts_unexp_error)
         THEN
            RAISE fnd_api.g_exc_unexpected_error;
         END IF;
      END IF;
             

      -- Check if this is an FPO
      OPEN cur_check_fpo (l_activity_id);
      FETCH cur_check_fpo INTO l_batch_type;

      IF l_batch_type = 10
      THEN
         CLOSE cur_check_fpo;
         gme_common_pvt.log_message ('GME_FPO_ACTV_NO_EDIT');
         RAISE fnd_api.g_exc_error;
      END IF;

      CLOSE cur_check_fpo;
      OPEN cur_lock_actv (l_activity_id);
      FETCH cur_lock_actv INTO l_last_update_date;
      CLOSE cur_lock_actv;

      IF (NVL (g_debug, -1) = gme_debug.g_log_statement)
      THEN
         gme_debug.put_line (' Successfully locked the activity');
      END IF;

      IF NOT (gme_batch_step_activities_dbl.fetch_row (p_batchstep_activity_rec,
                                                       l_batchstep_activity_rec
                                                      )
             )
      THEN
         RAISE fnd_api.g_exc_error;
      END IF;
      
      /* Shaliu Chen     05-MAY-2015  ER 20938455
         raise an error if batch is on hold
      */
      /* Shaliu Chen     30-JUN-2015  BUG 21345006
         correct the validation logic as follows:
         1.prevent to run the api if batch is on hold with Stop type
         2.prevent to update actual fields if batch is on hold with Pause type.
      */         
      IF gme_common_pvt.get_batch_hold_status(l_batch_id) = 'S' THEN
        gme_common_pvt.log_message('GME_BATCH_ONHOLD_CHECK',
                                   'ACTION_NAME',
                                   'Update Batchstep Activity');        
        RAISE fnd_api.g_exc_error;   
      ELSIF gme_common_pvt.get_batch_hold_status(l_batch_id) = 'P' THEN 
        IF ((p_batchstep_activity_rec.actual_start_date IS NOT NULL AND 
            p_batchstep_activity_rec.actual_start_date <> NVL(l_batchstep_activity_rec.actual_start_date,p_batchstep_activity_rec.actual_start_date+1)) OR       
            (p_batchstep_activity_rec.actual_cmplt_date IS NOT NULL AND 
            p_batchstep_activity_rec.actual_cmplt_date <> NVL(l_batchstep_activity_rec.actual_cmplt_date,p_batchstep_activity_rec.actual_cmplt_date+1))) THEN
          gme_common_pvt.log_message('GME_BATCH_ONHOLD_CHECK',
                                     'ACTION_NAME',
                                     'Update Batchstep Activity');        
          RAISE fnd_api.g_exc_error;              
        END IF;                          
      END IF;        
      
      /*
        BUG 21208206  25-JUN-2015
        check whether the actual_start_date and actul_cmplt_date fall into hold period
      */
      IF p_batchstep_activity_rec.actual_start_date IS NOT NULL THEN
        IF gme_common_pvt.get_batch_hold_status(p_batch_id => l_batch_id,
                                                p_date     => p_batchstep_activity_rec.actual_start_date) <> 'R' THEN
          gme_common_pvt.log_message('GME_ACTUAL_DATE_ONHOLD',
                                     'ACTUAL_DATE',
                                     'Actual_Start_Date');        
          RAISE fnd_api.g_exc_error;        
        END IF;                
      END IF;
      
      IF p_batchstep_activity_rec.actual_cmplt_date IS NOT NULL THEN
        IF gme_common_pvt.get_batch_hold_status(p_batch_id => l_batch_id,
                                                p_date     => p_batchstep_activity_rec.actual_cmplt_date) <> 'R' THEN
          gme_common_pvt.log_message('GME_ACTUAL_DATE_ONHOLD',
                                     'ACTUAL_DATE',
                                     'Actual_Completion_Date');        
          RAISE fnd_api.g_exc_error;        
        END IF;                
      END IF;       

      l_seq_dep_ind := l_batchstep_activity_rec.sequence_dependent_ind;

      IF l_step_status IN (4, 5)
      THEN
         gme_common_pvt.log_message ('PC_STEP_STATUS_ERR');
         RAISE fnd_api.g_exc_error;
      END IF;

      IF p_batchstep_activity_rec.offset_interval = fnd_api.g_miss_num
      THEN
         gme_common_pvt.log_message ('GME_FIELD_VALUE_REQUIRED',
                                     'FIELD_NAME',
                                     'offset_interval'
                                    );
         RAISE fnd_api.g_exc_error;
      ELSIF (    p_batchstep_activity_rec.offset_interval IS NOT NULL
             AND (l_batchstep_activity_rec.offset_interval <>
                                      p_batchstep_activity_rec.offset_interval
                 )
            )
      THEN
         l_field_updated := TRUE;
         l_batchstep_activity_rec.offset_interval :=
                                     p_batchstep_activity_rec.offset_interval;
      END IF;

      OPEN cur_get_step_dtl (l_batchstep_id);
      FETCH cur_get_step_dtl INTO l_step_dtl;
      CLOSE cur_get_step_dtl;

      IF l_step_status IN (1, 2)
      THEN
         IF p_batchstep_activity_rec.plan_start_date = fnd_api.g_miss_date
         THEN
            gme_common_pvt.log_message ('GME_FIELD_VALUE_REQUIRED',
                                        'FIELD_NAME',
                                        'plan_start_date'
                                       );
            RAISE fnd_api.g_exc_error;
         ELSIF     p_batchstep_activity_rec.plan_start_date IS NOT NULL
               AND (p_batchstep_activity_rec.plan_start_date <>
                                      l_batchstep_activity_rec.plan_start_date
                   )
         THEN
            -- Check that the activity plan dates fall within the step dates.
            IF    p_batchstep_activity_rec.plan_start_date <
                                                   l_step_dtl.plan_start_date
               OR p_batchstep_activity_rec.plan_start_date >
                                                    l_step_dtl.plan_cmplt_date
            THEN
               gme_common_pvt.log_message ('GME_ACTV_PLAN_DATE',
                                           'ACTIVITY',
                                           l_activity
                                          );
               RAISE fnd_api.g_exc_error;
            END IF;

            l_field_updated := TRUE;
            l_upd_plan_date := TRUE;
            l_batchstep_activity_rec.plan_start_date :=
                                      p_batchstep_activity_rec.plan_start_date;
         END IF;

         IF p_batchstep_activity_rec.plan_cmplt_date = fnd_api.g_miss_date
         THEN
            gme_common_pvt.log_message ('GME_FIELD_VALUE_REQUIRED',
                                        'FIELD_NAME',
                                        'plan_cmplt_date'
                                       );
            RAISE fnd_api.g_exc_error;
         ELSIF     p_batchstep_activity_rec.plan_cmplt_date IS NOT NULL
               AND (p_batchstep_activity_rec.plan_cmplt_date <>
                                      l_batchstep_activity_rec.plan_cmplt_date
                   )
         THEN
            -- Check that the activity plan dates fall within the step dates.
            IF p_batchstep_activity_rec.plan_cmplt_date >
                                                   l_step_dtl.plan_cmplt_date
            THEN
               gme_common_pvt.log_message ('GME_ACTV_PLAN_DATE',
                                           'ACTIVITY',
                                           l_activity
                                          );
               RAISE fnd_api.g_exc_error;
            END IF;

            l_field_updated := TRUE;
            l_upd_plan_date := TRUE;
            l_batchstep_activity_rec.plan_cmplt_date :=
                                      p_batchstep_activity_rec.plan_cmplt_date;
         END IF;

         IF (    l_upd_plan_date
             AND l_batchstep_activity_rec.plan_start_date >
                                      l_batchstep_activity_rec.plan_cmplt_date
            )
         THEN
            gme_common_pvt.log_message ('PM_BADSTARTDATE');
            RAISE fnd_api.g_exc_error;
         END IF;
      END IF;

      IF l_step_dtl.step_status IN (2, 3)
      THEN
         IF p_batchstep_activity_rec.actual_start_date = fnd_api.g_miss_date
         THEN
            gme_common_pvt.log_message ('GME_FIELD_VALUE_REQUIRED',
                                        'FIELD_NAME',
                                        'actual_start_date'
                                       );
            RAISE fnd_api.g_exc_error;
         ELSIF     p_batchstep_activity_rec.actual_start_date IS NOT NULL
               AND (p_batchstep_activity_rec.actual_start_date <>
                                    l_batchstep_activity_rec.actual_start_date
                   )
         THEN
            -- Check that actual start date is not in the future
            IF p_batchstep_activity_rec.actual_start_date >
                                                   gme_common_pvt.g_timestamp
            THEN
               fnd_message.set_name ('GMA', 'SY_NOFUTUREDATE');
               fnd_msg_pub.ADD;
               RAISE fnd_api.g_exc_error;
            END IF;

            -- Check that the activity actual dates fall within the step dates.
            IF (   p_batchstep_activity_rec.actual_start_date <
                                                  l_step_dtl.actual_start_date
                OR p_batchstep_activity_rec.actual_start_date >
                                                  l_step_dtl.actual_cmplt_date
               )
            THEN
               gme_common_pvt.log_message ('GME_ACTV_ACTUAL_DATE',
                                           'ACTIVITY',
                                           l_activity
                                          );
               RAISE fnd_api.g_exc_error;
            END IF;

            l_field_updated := TRUE;
            l_upd_act_date := TRUE;
            l_batchstep_activity_rec.actual_start_date :=
                                    p_batchstep_activity_rec.actual_start_date;
         END IF;
      END IF;

      IF l_step_dtl.step_status = 3
      THEN
         IF p_batchstep_activity_rec.actual_cmplt_date = fnd_api.g_miss_date
         THEN
            gme_common_pvt.log_message ('GME_FIELD_VALUE_REQUIRED',
                                        'FIELD_NAME',
                                        'actual_cmplt_date'
                                       );
            RAISE fnd_api.g_exc_error;
         ELSIF     p_batchstep_activity_rec.actual_cmplt_date IS NOT NULL
               AND (p_batchstep_activity_rec.actual_cmplt_date <>
                                    l_batchstep_activity_rec.actual_cmplt_date
                   )
         THEN
            -- Check that actual cmplt date is not in the future
            IF p_batchstep_activity_rec.actual_cmplt_date >
                                                   gme_common_pvt.g_timestamp
            THEN
               fnd_message.set_name ('GMA', 'SY_NOFUTUREDATE');
               fnd_msg_pub.ADD;
               RAISE fnd_api.g_exc_error;
            END IF;

            IF     p_batchstep_activity_rec.actual_cmplt_date IS NOT NULL
               AND p_batchstep_activity_rec.actual_cmplt_date >
                                                  l_step_dtl.actual_cmplt_date
            THEN
               gme_common_pvt.log_message ('GME_ACTV_ACTUAL_DATE',
                                           'ACTIVITY',
                                           l_activity
                                          );
               RAISE fnd_api.g_exc_error;
            END IF;

            l_field_updated := TRUE;
            l_upd_act_date := TRUE;
            l_batchstep_activity_rec.actual_cmplt_date :=
                                    p_batchstep_activity_rec.actual_cmplt_date;
         END IF;
      END IF;

      IF     l_upd_act_date
         AND (l_batchstep_activity_rec.actual_start_date >
                                    l_batchstep_activity_rec.actual_cmplt_date
             )
      THEN
         gme_common_pvt.log_message ('PM_BADSTARTDATE');
         RAISE fnd_api.g_exc_error;
      END IF;

      -- check that activities dates are within resource dates
      IF l_upd_plan_date OR l_upd_act_date
      THEN
         OPEN cur_dates (l_activity_id);

         LOOP
            FETCH cur_dates INTO l_temp_resources;
            EXIT WHEN cur_dates%NOTFOUND;

            IF    (    l_batchstep_activity_rec.actual_start_date IS NOT NULL
                   AND l_batchstep_activity_rec.actual_start_date >
                                            l_temp_resources.actual_start_date
                  )
               OR (    l_batchstep_activity_rec.actual_cmplt_date IS NOT NULL
                   AND l_batchstep_activity_rec.actual_cmplt_date <
                                            l_temp_resources.actual_cmplt_date
                  )
               OR (    l_batchstep_activity_rec.plan_start_date IS NOT NULL
                   AND l_batchstep_activity_rec.plan_start_date >
                                              l_temp_resources.plan_start_date
                  )
               OR (    l_batchstep_activity_rec.plan_cmplt_date IS NOT NULL
                   AND l_batchstep_activity_rec.plan_cmplt_date <
                                              l_temp_resources.plan_cmplt_date
                  )
            THEN
               l_resource := l_resource || ' ' || l_temp_resources.resources;
            END IF;
         END LOOP;

         CLOSE cur_dates;

         IF l_resource IS NOT NULL
         THEN
            gme_common_pvt.log_message ('GME_RSRC_DATES_NOT_ALLOWED',
                                        'RESOURCE',
                                        'l_resource'
                                       );
            RAISE fnd_api.g_exc_error;
         END IF;
      END IF;

      --fix to g_miss_num and comparison to not NULL for l_batchstep_activity_rec
      IF     p_batchstep_activity_rec.plan_activity_factor =
                                                            fnd_api.g_miss_num
         AND l_batchstep_activity_rec.plan_activity_factor IS NOT NULL
      THEN
         gme_common_pvt.log_message ('GME_PLAN_ACTV_FACT_NOTNULL');
         RAISE fnd_api.g_exc_error;
      END IF;

      IF    (    p_batchstep_activity_rec.plan_activity_factor IS NOT NULL
             AND l_batchstep_activity_rec.plan_activity_factor IS NULL
            )
         OR (p_batchstep_activity_rec.plan_activity_factor <>
                                 l_batchstep_activity_rec.plan_activity_factor
            )
      THEN
         IF l_step_status = 1
         THEN
            l_field_updated := TRUE;
            l_update_resources := TRUE;
            l_batchstep_activity_rec.plan_activity_factor :=
                                p_batchstep_activity_rec.plan_activity_factor;
         END IF;
      END IF;

      --fix to g_miss_num and comparison to not NULL for l_batchstep_activity_rec
      IF     p_batchstep_activity_rec.actual_activity_factor =
                                                            fnd_api.g_miss_num
         AND l_batchstep_activity_rec.actual_activity_factor IS NOT NULL
      THEN
         gme_common_pvt.log_message ('GME_ACTUAL_ACTV_FACT_NOTNULL');
         RAISE fnd_api.g_exc_error;
      END IF;

      IF    (    p_batchstep_activity_rec.actual_activity_factor IS NOT NULL
             AND l_batchstep_activity_rec.actual_activity_factor IS NULL
            )
         OR (p_batchstep_activity_rec.actual_activity_factor <>
                               l_batchstep_activity_rec.actual_activity_factor
            )
      THEN
         IF l_step_status IN (2, 3)
         THEN
            l_field_updated := TRUE;
            l_update_resources := TRUE;
            l_batchstep_activity_rec.actual_activity_factor :=
                              p_batchstep_activity_rec.actual_activity_factor;
         END IF;
      END IF;

      IF (fnd_api.to_boolean (p_validate_flexfield))
      THEN
         IF    NVL (p_batchstep_activity_rec.attribute_category, ' ') <>
                       NVL (l_batchstep_activity_rec.attribute_category, ' ')
            OR NVL (p_batchstep_activity_rec.attribute1, ' ') <>
                                NVL (l_batchstep_activity_rec.attribute1, ' ')
            OR NVL (p_batchstep_activity_rec.attribute2, ' ') <>
                                NVL (l_batchstep_activity_rec.attribute2, ' ')
            OR NVL (p_batchstep_activity_rec.attribute3, ' ') <>
                                NVL (l_batchstep_activity_rec.attribute3, ' ')
            OR NVL (p_batchstep_activity_rec.attribute4, ' ') <>
                                NVL (l_batchstep_activity_rec.attribute4, ' ')
            OR NVL (p_batchstep_activity_rec.attribute5, ' ') <>
                                NVL (l_batchstep_activity_rec.attribute5, ' ')
            OR NVL (p_batchstep_activity_rec.attribute6, ' ') <>
                                NVL (l_batchstep_activity_rec.attribute6, ' ')
            OR NVL (p_batchstep_activity_rec.attribute7, ' ') <>
                                NVL (l_batchstep_activity_rec.attribute7, ' ')
            OR NVL (p_batchstep_activity_rec.attribute8, ' ') <>
                                NVL (l_batchstep_activity_rec.attribute8, ' ')
            OR NVL (p_batchstep_activity_rec.attribute9, ' ') <>
                                NVL (l_batchstep_activity_rec.attribute9, ' ')
            OR NVL (p_batchstep_activity_rec.attribute10, ' ') <>
                               NVL (l_batchstep_activity_rec.attribute10, ' ')
            OR NVL (p_batchstep_activity_rec.attribute11, ' ') <>
                               NVL (l_batchstep_activity_rec.attribute11, ' ')
            OR NVL (p_batchstep_activity_rec.attribute12, ' ') <>
                               NVL (l_batchstep_activity_rec.attribute12, ' ')
            OR NVL (p_batchstep_activity_rec.attribute13, ' ') <>
                               NVL (l_batchstep_activity_rec.attribute13, ' ')
            OR NVL (p_batchstep_activity_rec.attribute14, ' ') <>
                               NVL (l_batchstep_activity_rec.attribute14, ' ')
            OR NVL (p_batchstep_activity_rec.attribute15, ' ') <>
                               NVL (l_batchstep_activity_rec.attribute15, ' ')
            OR NVL (p_batchstep_activity_rec.attribute16, ' ') <>
                               NVL (l_batchstep_activity_rec.attribute16, ' ')
            OR NVL (p_batchstep_activity_rec.attribute17, ' ') <>
                               NVL (l_batchstep_activity_rec.attribute17, ' ')
            OR NVL (p_batchstep_activity_rec.attribute18, ' ') <>
                               NVL (l_batchstep_activity_rec.attribute18, ' ')
            OR NVL (p_batchstep_activity_rec.attribute19, ' ') <>
                               NVL (l_batchstep_activity_rec.attribute19, ' ')
            OR NVL (p_batchstep_activity_rec.attribute20, ' ') <>
                               NVL (l_batchstep_activity_rec.attribute20, ' ')
            OR NVL (p_batchstep_activity_rec.attribute21, ' ') <>
                               NVL (l_batchstep_activity_rec.attribute21, ' ')
            OR NVL (p_batchstep_activity_rec.attribute22, ' ') <>
                               NVL (l_batchstep_activity_rec.attribute22, ' ')
            OR NVL (p_batchstep_activity_rec.attribute23, ' ') <>
                               NVL (l_batchstep_activity_rec.attribute23, ' ')
            OR NVL (p_batchstep_activity_rec.attribute24, ' ') <>
                               NVL (l_batchstep_activity_rec.attribute24, ' ')
            OR NVL (p_batchstep_activity_rec.attribute25, ' ') <>
                               NVL (l_batchstep_activity_rec.attribute25, ' ')
            OR NVL (p_batchstep_activity_rec.attribute26, ' ') <>
                               NVL (l_batchstep_activity_rec.attribute26, ' ')
            OR NVL (p_batchstep_activity_rec.attribute27, ' ') <>
                               NVL (l_batchstep_activity_rec.attribute27, ' ')
            OR NVL (p_batchstep_activity_rec.attribute28, ' ') <>
                               NVL (l_batchstep_activity_rec.attribute28, ' ')
            OR NVL (p_batchstep_activity_rec.attribute29, ' ') <>
                               NVL (l_batchstep_activity_rec.attribute29, ' ')
            OR NVL (p_batchstep_activity_rec.attribute30, ' ') <>
                               NVL (l_batchstep_activity_rec.attribute30, ' ')
         THEN
            l_flex_validate := TRUE;
         END IF;

         IF l_flex_validate
         THEN
            l_field_updated := TRUE;
            gme_validate_flex_fld_pvt.validate_flex_step_activities (p_step_activities      => p_batchstep_activity_rec,
                                                                     x_step_activities      => x_batchstep_activity_rec,
                                                                     x_return_status        => x_return_status
                                                                    );

            IF (NVL (g_debug, -1) = gme_debug.g_log_statement)
            THEN
               gme_debug.put_line (   'gme_validate_flex_fld_pvt.validate_flex_step_activities '
                                   || x_return_status
                                  );
            END IF;

            IF (x_return_status = fnd_api.g_ret_sts_error)
            THEN
               RAISE fnd_api.g_exc_error;
            ELSIF (x_return_status = fnd_api.g_ret_sts_unexp_error)
            THEN
               RAISE fnd_api.g_exc_unexpected_error;
            END IF;

            -- copy all the validated flex fields
            l_batchstep_activity_rec.attribute_category :=
                                   x_batchstep_activity_rec.attribute_category;
            l_batchstep_activity_rec.attribute1 :=
                                           x_batchstep_activity_rec.attribute1;
            l_batchstep_activity_rec.attribute2 :=
                                           x_batchstep_activity_rec.attribute2;
            l_batchstep_activity_rec.attribute3 :=
                                           x_batchstep_activity_rec.attribute3;
            l_batchstep_activity_rec.attribute4 :=
                                           x_batchstep_activity_rec.attribute4;
            l_batchstep_activity_rec.attribute5 :=
                                           x_batchstep_activity_rec.attribute5;
            l_batchstep_activity_rec.attribute6 :=
                                           x_batchstep_activity_rec.attribute6;
            l_batchstep_activity_rec.attribute7 :=
                                           x_batchstep_activity_rec.attribute7;
            l_batchstep_activity_rec.attribute8 :=
                                           x_batchstep_activity_rec.attribute8;
            l_batchstep_activity_rec.attribute9 :=
                                           x_batchstep_activity_rec.attribute9;
            l_batchstep_activity_rec.attribute10 :=
                                          x_batchstep_activity_rec.attribute10;
            l_batchstep_activity_rec.attribute11 :=
                                          x_batchstep_activity_rec.attribute11;
            l_batchstep_activity_rec.attribute12 :=
                                          x_batchstep_activity_rec.attribute12;
            l_batchstep_activity_rec.attribute13 :=
                                          x_batchstep_activity_rec.attribute13;
            l_batchstep_activity_rec.attribute14 :=
                                          x_batchstep_activity_rec.attribute14;
            l_batchstep_activity_rec.attribute15 :=
                                          x_batchstep_activity_rec.attribute15;
            l_batchstep_activity_rec.attribute16 :=
                                          x_batchstep_activity_rec.attribute16;
            l_batchstep_activity_rec.attribute17 :=
                                          x_batchstep_activity_rec.attribute17;
            l_batchstep_activity_rec.attribute18 :=
                                          x_batchstep_activity_rec.attribute18;
            l_batchstep_activity_rec.attribute19 :=
                                          x_batchstep_activity_rec.attribute19;
            l_batchstep_activity_rec.attribute20 :=
                                          x_batchstep_activity_rec.attribute20;
            l_batchstep_activity_rec.attribute21 :=
                                          x_batchstep_activity_rec.attribute21;
            l_batchstep_activity_rec.attribute22 :=
                                          x_batchstep_activity_rec.attribute22;
            l_batchstep_activity_rec.attribute23 :=
                                          x_batchstep_activity_rec.attribute23;
            l_batchstep_activity_rec.attribute24 :=
                                          x_batchstep_activity_rec.attribute24;
            l_batchstep_activity_rec.attribute25 :=
                                          x_batchstep_activity_rec.attribute25;
            l_batchstep_activity_rec.attribute26 :=
                                          x_batchstep_activity_rec.attribute26;
            l_batchstep_activity_rec.attribute27 :=
                                          x_batchstep_activity_rec.attribute27;
            l_batchstep_activity_rec.attribute28 :=
                                          x_batchstep_activity_rec.attribute28;
            l_batchstep_activity_rec.attribute29 :=
                                          x_batchstep_activity_rec.attribute29;
            l_batchstep_activity_rec.attribute30 :=
                                          x_batchstep_activity_rec.attribute30;
         END IF;
      END IF;

      IF NOT (l_field_updated)
      THEN
         gme_common_pvt.log_message ('GME_NO_CHANGE_TO_UPD');
         RAISE fnd_api.g_exc_error;
      END IF;

      IF (NVL (g_debug, -1) = gme_debug.g_log_statement)
      THEN
         gme_debug.put_line ('before update of activity');
      END IF;

      IF l_update_resources
      THEN
         OPEN cur_get_batch (l_batch_id);
         FETCH cur_get_batch INTO l_batch_hdr;
         CLOSE cur_get_batch;

         -- load temp table so that save_batch routine does resource txn consolidation 
         IF l_batch_hdr.update_inventory_ind = 'Y'
         THEN
            gme_trans_engine_util.load_rsrc_trans (p_batch_row          => l_batch_hdr,
                                                   x_rsc_row_count      => l_rsrc_trans_count,
                                                   x_return_status      => x_return_status
                                                   );

            IF (NVL (g_debug, 0) = gme_debug.g_log_statement)
            THEN
               gme_debug.put_line (   g_pkg_name
                                   || '.'
                                   || l_api_name
                                   || ':'
                                   || 'after call to GME_TRANS_ENGINE_UTIL.load_rsrc_trans return status is '
                                   || x_return_status
                                  );
            END IF;

            IF (x_return_status = fnd_api.g_ret_sts_error)
            THEN
               RAISE fnd_api.g_exc_error;
            ELSIF (x_return_status = fnd_api.g_ret_sts_unexp_error)
            THEN
               RAISE fnd_api.g_exc_unexpected_error;
            END IF;
         END IF;

         gme_update_step_qty_pvt.update_resources (p_batch_hdr_rec                 => l_batch_hdr,
                                                   p_batch_step_rec                => l_step_dtl,
                                                   p_batchstep_activities_rec      => l_batchstep_activity_rec,
                                                   x_return_status                 => x_return_status
                                                  );

         IF (NVL (g_debug, 0) = gme_debug.g_log_statement)
         THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ':'
                                || 'after call to GME_UPDATE_STEP_QTY_PVT.update_resources return status is '
                                || x_return_status
                               );
         END IF;

         IF (x_return_status = fnd_api.g_ret_sts_error)
         THEN
            RAISE fnd_api.g_exc_error;
         ELSIF (x_return_status = fnd_api.g_ret_sts_unexp_error)
         THEN
            RAISE fnd_api.g_exc_unexpected_error;
         END IF;
      END IF;

      IF NOT (gme_batch_step_activities_dbl.update_row (l_batchstep_activity_rec
                                                       )
             )
      THEN
         RAISE fnd_api.g_exc_error;
      END IF;

      IF l_seq_dep_ind <> l_batchstep_activity_rec.sequence_dependent_ind
      THEN
         gme_batch_step_chg_pvt.set_activity_sequence_num (l_batchstep_activity_rec.batch_id
                                                          );
      END IF;

      /*Fetching the values from the db again to get the latest seq_dep_ind.*/
      IF NOT (gme_batch_step_activities_dbl.fetch_row (l_batchstep_activity_rec,
                                                       l_batchstep_activity_rec
                                                      )
             )
      THEN
         RAISE fnd_api.g_exc_error;
      END IF;

      x_batchstep_activity_rec := l_batchstep_activity_rec;

      IF (NVL (g_debug, 0) IN
                       (gme_debug.g_log_statement, gme_debug.g_log_procedure)
         )
      THEN
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ':'
                             || 'Exiting with '
                             || x_return_status
                            );
      END IF;
   EXCEPTION
      WHEN app_exception.record_lock_exception
      THEN
         gme_common_pvt.log_message ('GME_RECORD_LOCKED',
                                     'TABLE_NAME',
                                     'GME_BATCH_STEP_ACTIVITIES',
                                     'RECORD',
                                     'Activity',
                                     'KEY',
                                     l_activity
                                    );
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN fnd_api.g_exc_error
      THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN fnd_api.g_exc_unexpected_error
      THEN
         x_return_status := fnd_api.g_ret_sts_unexp_error;

         IF (NVL (g_debug, 0) > 0)
         THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ':'
                                || 'UNEXPECTED:'
                                || SQLERRM
                               );
         END IF;
      WHEN OTHERS
      THEN
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
         x_return_status := fnd_api.g_ret_sts_unexp_error;

         IF (NVL (g_debug, 0) > 0)
         THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ':'
                                || 'OTHERS:'
                                || SQLERRM
                               );
         END IF;
   END update_batchstep_activity;

/*===========================================================================================
   Procedure
      delete_batchstep_activity
   Description
     This procedure is used to delete an activity and all it's children
   Parameters
     p_batchstep_activity_id OR
     (p_org_code,p_batch_no,step_no,activity)   to uniquely identify an activity
     x_return_status              reflects return status of the API
     
      Shaliu Chen     05-MAY-2015  ER 20938455
        Modify for Batch On Hold enhancement,add validation to raise an error 
        if batch is on hold      

=============================================================================================*/
   PROCEDURE delete_batchstep_activity (
      p_batchstep_activity_id   IN              NUMBER := NULL,
      p_org_code                IN              VARCHAR2 := NULL,
      p_batch_no                IN              VARCHAR2 := NULL,
      p_batchstep_no            IN              NUMBER := NULL,
      p_activity                IN              VARCHAR2 := NULL,
      x_return_status           OUT NOCOPY      VARCHAR2
   )
   IS
      l_api_name        CONSTANT VARCHAR2 (30) := 'delete_batchstep_activity';
      l_step_status              NUMBER;
      l_batch_status             NUMBER;
      l_batch_type               NUMBER;
      l_activity_id              NUMBER;
      l_batch_id                 NUMBER;
      l_batchstep_id             NUMBER;
      l_batchstep_activity_rec   gme_batch_step_activities%ROWTYPE;
      x_batchstep_activity_rec   gme_batch_step_activities%ROWTYPE;
      l_cnt                      NUMBER;
      l_activity                 gme_batch_step_activities.activity%TYPE;
      l_last_update_date         DATE;
      l_inv_trans_count          NUMBER;
      l_rsrc_trans_count         NUMBER;
      l_batch_hdr                gme_batch_header%ROWTYPE;

      CURSOR cur_get_batch (v_batch_id NUMBER)
      IS
         SELECT *
           FROM gme_batch_header
          WHERE batch_id = v_batch_id;

      CURSOR cur_get_step_dtl (v_batchstep_act_id NUMBER)
      IS
         SELECT step_status, batch_status, a.batch_id, s.batchstep_id,
                batchstep_activity_id, activity
           FROM gme_batch_steps s,
                gme_batch_step_activities a,
                gme_batch_header b
          WHERE batchstep_activity_id = v_batchstep_act_id
            AND b.batch_id = a.batch_id
            AND s.batchstep_id = a.batchstep_id;

      CURSOR cur_get_activity_count (v_step_id NUMBER)
      IS
         SELECT COUNT (1)
           FROM gme_batch_step_activities
          WHERE batchstep_id = v_step_id;

      CURSOR cur_check_fpo (v_actv_id NUMBER)
      IS
         SELECT batch_type
           FROM gme_batch_header b, gme_batch_step_activities a
          WHERE b.batch_id = a.batch_id AND batchstep_activity_id = v_actv_id;

      CURSOR cur_lock_actv (v_activity_id NUMBER)
      IS
         SELECT        last_update_date
                  FROM gme_batch_step_activities
                 WHERE batchstep_activity_id = v_activity_id
         FOR UPDATE OF last_update_date NOWAIT;
   BEGIN
      IF (NVL (g_debug, 0) IN
                       (gme_debug.g_log_statement, gme_debug.g_log_procedure)
         )
      THEN
         gme_debug.put_line (g_pkg_name || '.' || l_api_name || ':'
                             || 'Entering'
                            );
      END IF;

      /* Set the return status to success initially */
      x_return_status := fnd_api.g_ret_sts_success;

      IF (p_batchstep_activity_id IS NULL AND p_activity IS NULL)
      THEN
         gme_common_pvt.log_message ('GME_FIELD_VALUE_REQUIRED',
                                     'FIELD_NAME',
                                     'ACTIVITY'
                                    );
         RAISE fnd_api.g_exc_error;
      ELSE
         IF p_batchstep_activity_id IS NULL
         THEN
            validate_param (p_org_code           => p_org_code,
                            p_batch_no           => p_batch_no,
                            p_batchstep_no       => p_batchstep_no,
                            p_activity           => p_activity,
                            x_batch_id           => l_batch_id,
                            x_batchstep_id       => l_batchstep_id,
                            x_activity_id        => l_activity_id,
                            x_batch_status       => l_batch_status,
                            x_step_status        => l_step_status,
                            x_return_status      => x_return_status
                           );

            IF (NVL (g_debug, 0) = gme_debug.g_log_statement)
            THEN
               gme_debug.put_line (   g_pkg_name
                                   || '.'
                                   || l_api_name
                                   || ':'
                                   || 'after the call to procedure validate_param '
                                   || x_return_status
                                  );
            END IF;

            IF (x_return_status = fnd_api.g_ret_sts_error)
            THEN
               RAISE fnd_api.g_exc_error;
            ELSIF (x_return_status = fnd_api.g_ret_sts_unexp_error)
            THEN
               RAISE fnd_api.g_exc_unexpected_error;
            END IF;

            l_activity := p_activity;
         ELSE
            l_activity_id := p_batchstep_activity_id;
            OPEN cur_get_step_dtl (l_activity_id);
            FETCH cur_get_step_dtl INTO l_step_status,
             l_batch_status,
             l_batch_id,
             l_batchstep_id,
             l_activity_id,
             l_activity;

            IF cur_get_step_dtl%NOTFOUND
            THEN
               CLOSE cur_get_step_dtl;
               gme_common_pvt.log_message ('GME_ACTID_NOT_FOUND');
               RAISE fnd_api.g_exc_error;
            END IF;

            CLOSE cur_get_step_dtl;
         END IF;
      END IF;
      
      /* Shaliu Chen     05-MAY-2015  ER 20938455
         raise an error if batch is on hold
      */
      IF gme_common_pvt.get_batch_hold_status(l_batch_id) = 'S' THEN
        gme_common_pvt.log_message('GME_BATCH_ONHOLD_CHECK',
                                   'ACTION_NAME',
                                   'Delete Batchstep Activity');        
        RAISE fnd_api.g_exc_error;        
      END IF;       

      -- check FPO
      OPEN cur_check_fpo (l_activity_id);
      FETCH cur_check_fpo INTO l_batch_type;

      IF l_batch_type = 10
      THEN
         CLOSE cur_check_fpo;
         gme_common_pvt.log_message ('GME_FPO_ACTV_NO_DEL');
         RAISE fnd_api.g_exc_error;
      END IF;

      CLOSE cur_check_fpo;
      OPEN cur_lock_actv (l_activity_id);
      FETCH cur_lock_actv INTO l_last_update_date;
      CLOSE cur_lock_actv;

      IF (NVL (g_debug, -1) = gme_debug.g_log_statement)
      THEN
         gme_debug.put_line (' Successfully locked the activity');
      END IF;

      IF l_batch_status <> 1
      THEN
         gme_common_pvt.log_message ('PM_WRONG_STATUS');
         RAISE fnd_api.g_exc_error;
      END IF;

      OPEN cur_get_activity_count (l_batchstep_id);
      FETCH cur_get_activity_count INTO l_cnt;

      IF l_cnt = 1
      THEN
         gme_common_pvt.log_message ('GME_ACTIVITY_NOT_ATTACH');
         RAISE fnd_api.g_exc_error;
      END IF;

      l_batchstep_activity_rec.batch_id := l_batch_id;
      l_batchstep_activity_rec.batchstep_id := l_batchstep_id;
      l_batchstep_activity_rec.batchstep_activity_id := l_activity_id;
      -- load temp table so that save_batch routine does resource txn consolidation 
      OPEN cur_get_batch (l_batch_id);
      FETCH cur_get_batch INTO l_batch_hdr;
      CLOSE cur_get_batch;

      IF l_batch_hdr.update_inventory_ind = 'Y'
      THEN
         gme_trans_engine_util.load_rsrc_trans (p_batch_row          => l_batch_hdr,
                                                x_rsc_row_count      => l_rsrc_trans_count,
                                                x_return_status      => x_return_status
                                                );

         IF (NVL (g_debug, 0) = gme_debug.g_log_statement)
         THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ':'
                                || 'after call to GME_TRANS_ENGINE_UTIL.load_rsrc_trans return status is '
                                || x_return_status
                               );
         END IF;

         IF (x_return_status = fnd_api.g_ret_sts_error)
         THEN
            RAISE fnd_api.g_exc_error;
         ELSIF (x_return_status = fnd_api.g_ret_sts_unexp_error)
         THEN
            RAISE fnd_api.g_exc_unexpected_error;
         END IF;
      END IF;

      gme_delete_batch_step_pvt.delete_activity (p_batch_step_activities_rec      => l_batchstep_activity_rec,
                                                 x_return_status                  => x_return_status
                                                );

      IF (NVL (g_debug, -1) = gme_debug.g_log_statement)
      THEN
         gme_debug.put_line (   'after call to gme_delete_batch_step_pvt.delete_activity return status is '
                             || x_return_status
                            );
      END IF;

      IF (x_return_status = fnd_api.g_ret_sts_error)
      THEN
         RAISE fnd_api.g_exc_error;
      ELSIF (x_return_status = fnd_api.g_ret_sts_unexp_error)
      THEN
         RAISE fnd_api.g_exc_unexpected_error;
      END IF;

      IF (NVL (g_debug, 0) IN
                       (gme_debug.g_log_statement, gme_debug.g_log_procedure)
         )
      THEN
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ':'
                             || 'Exiting with '
                             || x_return_status
                            );
      END IF;
   EXCEPTION
      WHEN app_exception.record_lock_exception
      THEN
         gme_common_pvt.log_message ('GME_RECORD_LOCKED',
                                     'TABLE_NAME',
                                     'GME_BATCH_STEP_ACTIVITIES',
                                     'RECORD',
                                     'Activity',
                                     'KEY',
                                     l_activity
                                    );
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN fnd_api.g_exc_error
      THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN fnd_api.g_exc_unexpected_error
      THEN
         x_return_status := fnd_api.g_ret_sts_unexp_error;

         IF (NVL (g_debug, 0) > 0)
         THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ':'
                                || 'UNEXPECTED:'
                                || SQLERRM
                               );
         END IF;
      WHEN OTHERS
      THEN
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
         x_return_status := fnd_api.g_ret_sts_unexp_error;

         IF (NVL (g_debug, 0) > 0)
         THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ':'
                                || 'OTHERS:'
                                || SQLERRM
                               );
         END IF;
   END delete_batchstep_activity;
END gme_batchstep_act_pvt;
/

COMMIT ;
EXIT;
--show errors;
