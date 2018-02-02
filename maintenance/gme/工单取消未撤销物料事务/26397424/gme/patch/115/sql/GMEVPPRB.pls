/* +======================================================================+ */
/* |    Copyright (c) 2005, 2015 Oracle and/or its affiliates.           | */
/* |                         All rights reserved.                         | */
/* |                           Version 12.0.0                             | */
/* +======================================================================+ */
REM dbdrv: sql ~PROD ~PATH ~FILE none none none package &phase=plb \
REM dbdrv: checkfile(120.3.12010000.5=120.3.12020000.5)(120.3.12000000.3=120.3.12010000.3)(115.13=120.1):~PROD:~PATH:~FILE
SET VERIFY OFF
WHENEVER SQLERROR EXIT FAILURE ROLLBACK;
WHENEVER OSERROR EXIT FAILURE ROLLBACK;
REM **********************************************************************
REM *                                                                    *
REM * FILE:    GMEVPPRS.pls                                              *
REM * PURPOSE: Package Body for the GME PROCESS PARAMETERS routines      *
REM * AUTHOR:  OPM Development                        			 *
REM * DATE:    Apr 05th 2005                                             *
REM * Pawan kumar 17-March-2006 Bug 4896071                              *
REM *  Changes for parameter_um to parameter_uom for 25                  *

REM * QZENG    07-Mar-2013 Bug 16457668                                  *
REM *  No need to run some cursors for these validation are done in open *
REM *  interface                                                         *

REM * G. Muratore   17-Mar-2015 Bug 20567897                             *
REM *  Changed definition of unit variables to PARAMETER_UOM%TYPE        *
REM *  PROCEDURE insert_process_parameter and update_process_parameter   *
REM **********************************************************************

/****************************************************************************
* This file contains the procedure for various actions on process parameters *
* in Oracle Process Manufacturing (OPM). Each procedure has a common set of  *
* parameters to which API-specific parameters are appended.                  *
**************************************************************************/

CREATE OR REPLACE PACKAGE BODY gme_process_parameters_pvt AS
/* $Header: GMEVPPRB.pls 120.3.12020000.5 2015/05/20 08:42:56 shalchen ship $ */
   g_debug               VARCHAR2 (5)  := fnd_profile.VALUE ('AFLOG_LEVEL');
   g_pkg_name   CONSTANT VARCHAR2 (30) := 'GME_PROCESS_PARAMETERS_PVT';

/*===========================================================================================
   Procedure
      insert_process_parameter
   Description
     This particular procedure is used to insert parameter for an resource
   Parameters
     p_process_param_rec.batchstep_resource_id OR
     (p_plant_code,p_batch_no,p_batchstep_no,p_activity,p_resource,p_parameter ) to uniquely
      identify an process parameter
     p_process_param_rec         gme_process_parameters%ROWTYPE  - details of the process parameter
     x_return_status              reflects return status of the API
     
     
      19-MAY-2015  Shaliu Chen     BUG 201102335
        Modify for Batch On Hold enhancement,add validation to raise an error
        if batch is on hold either STOP or PAUSE type.       
=============================================================================================*/
   PROCEDURE insert_process_parameter (
      p_batch_no              IN              VARCHAR2
     ,p_org_code              IN              VARCHAR2
     ,p_validate_flexfields   IN              VARCHAR2
     ,p_batchstep_no          IN              NUMBER
     ,p_activity              IN              VARCHAR2
     ,p_parameter             IN              VARCHAR2
     ,p_process_param_rec     IN              gme_process_parameters%ROWTYPE
     ,x_process_param_rec     OUT NOCOPY      gme_process_parameters%ROWTYPE
     ,x_return_status         OUT NOCOPY      VARCHAR2)
   IS
      CURSOR cur_validate_batch_type (v_resource_id NUMBER)
      IS
         SELECT 1
           FROM gme_batch_header b, gme_batch_step_resources r
          WHERE b.batch_id = r.batch_id
            AND r.batchstep_resource_id = v_resource_id
            AND b.batch_type = 10;

      CURSOR cur_process_parameter (v_parameter VARCHAR2)
      IS
         SELECT parameter_id, parameter_type, minimum_value, maximum_value
               ,units
           FROM gmp_process_parameters
          WHERE parameter_name = v_parameter AND delete_mark = 0;

      CURSOR cur_process_value (v_parameter_id NUMBER, v_value VARCHAR2)
      IS
         SELECT 1
           FROM gmp_parameter_values
          WHERE parameter_id = v_parameter_id AND parameter_value = v_value;

      resource_fetch_error      EXCEPTION;
      input_param_missing       EXCEPTION;
      validate_param_failed     EXCEPTION;
      val_pro_param_failed      EXCEPTION;
      param_details_not_found   EXCEPTION;
      wrong_target_value        EXCEPTION;
      invalid_step_status       EXCEPTION;
      wrong_actual_value        EXCEPTION;
      validation_failure        EXCEPTION;
      l_api_name       CONSTANT VARCHAR2 (30)    := 'INSERT_PROCESS_PARAMETER';
      l_return_status           VARCHAR2 (1)      := fnd_api.g_ret_sts_success;
      l_process_param_rec       gme_process_parameters%ROWTYPE;
      l_resource_id             NUMBER;
      l_activity_id             NUMBER;
      l_batchstep_id            NUMBER;
      l_dummy                   NUMBER;
      l_batch_id                NUMBER;
      l_step_status             NUMBER;
      l_parameter_id            NUMBER;
      l_parameter_type          NUMBER;
      l_minimum_value           NUMBER;
      l_maximum_value           NUMBER;

      l_units                   gme_process_parameters.PARAMETER_UOM%TYPE; -- 20567897       
      -- l_units                VARCHAR2 (4);
      
      v_temp                    NUMBER;
      l_value                   NUMBER;
      l_proc_param_id           NUMBER;
      l_resources               VARCHAR2 (16);
      l_batch_header            gme_batch_header%ROWTYPE;
      l_resource_row            gme_batch_step_resources%ROWTYPE;
   BEGIN
      IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
         gme_debug.put_line ('Calling public Insert process parameters Row');
      END IF;

      x_return_status := fnd_api.g_ret_sts_success;

      IF p_parameter IS NULL THEN
         gme_common_pvt.log_message ('GME_FIELD_VALUE_REQUIRED'
                                    ,'FIELD_NAME'
                                    ,'PROCESS PARAMETER');
         RAISE input_param_missing;
      END IF;

      IF p_process_param_rec.batchstep_resource_id IS NOT NULL THEN
         -- validate the resource provided
         l_resource_row.batchstep_resource_id :=
                                    p_process_param_rec.batchstep_resource_id;

         IF NOT (gme_batch_step_resources_dbl.fetch_row (l_resource_row
                                                        ,l_resource_row) ) THEN
            gme_common_pvt.log_message ('GME_RSRCID_NOT_FOUND'
                                       ,'BATCHSTEP_RSRC_ID'
                                       ,l_resource_row.batchstep_resource_id);
            RAISE resource_fetch_error;
         END IF;

         l_resource_id := l_resource_row.batchstep_resource_id;
         l_activity_id := l_resource_row.batchstep_activity_id;
         l_batchstep_id := l_resource_row.batchstep_id;
         l_batch_id := l_resource_row.batch_id;
         l_resources := l_resource_row.resources;
         --added by qzeng no need to check if done in bulk validation
         IF NVL(gme_common_pvt.g_bulk_validation_done, 'N') = 'N' THEN
           -- make sure resource id does not belong to an FPO
           OPEN cur_validate_batch_type (l_resource_id);

           FETCH cur_validate_batch_type
            INTO l_dummy;

           IF cur_validate_batch_type%FOUND THEN
             CLOSE cur_validate_batch_type;

             gme_common_pvt.log_message ('GME_FPO_PARAM_NO_EDIT');
             RAISE validate_param_failed;
           END IF;

           CLOSE cur_validate_batch_type;
	 END IF;
      ELSE        /*  p_process_param_rec.batchstep_resource_id IS NOT NULL */
         l_resources := p_process_param_rec.resources;
         validate_process_param (p_org_code           => p_org_code
                                ,p_batch_no           => p_batch_no
                                ,p_batchstep_no       => p_batchstep_no
                                ,p_activity           => p_activity
                                ,p_resource           => l_resources
                                ,x_batch_id           => l_batch_id
                                ,x_batchstep_id       => l_batchstep_id
                                ,x_activity_id        => l_activity_id
                                ,x_resource_id        => l_resource_id
                                ,x_parameter_id       => l_parameter_id
                                ,x_proc_param_id      => l_proc_param_id
                                ,x_step_status        => l_step_status
                                ,x_return_status      => l_return_status);

         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line (   'after validate_process_param '
                                || l_return_status);
         END IF;

         IF l_return_status <> 'S' THEN
            RAISE val_pro_param_failed;
         END IF;
      END IF;     /*  p_process_param_rec.batchstep_resource_id IS NOT NULL */
      
      /* Shaliu Chen     19-MAY-2015  BUG 201102335
         raise an error if batch is on hold with either STOP or PAUSE type
      */
      IF gme_common_pvt.get_batch_hold_status(l_batch_id) <> 'R' THEN
        gme_common_pvt.log_message('GME_BATCH_ONHOLD_CHECK',
                                   'ACTION_NAME',
                                   'Insert Process Parameter');
        RAISE fnd_api.g_exc_error;
      END IF;        

      -- Check Step Status
      IF l_step_status IN (4, 5) THEN
         gme_common_pvt.log_message ('PC_STEP_STATUS_ERR');
         RAISE invalid_step_status;
      END IF;

      IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
         gme_debug.put_line ('after step status');
      END IF;

      -- Get process parameters details from GMP tables
      OPEN cur_process_parameter (p_parameter);

      FETCH cur_process_parameter
       INTO l_parameter_id, l_parameter_type, l_minimum_value
           ,l_maximum_value, l_units;

      IF cur_process_parameter%NOTFOUND THEN
         CLOSE cur_process_parameter;

         gme_common_pvt.log_message ('GME_PARAM_NOT_FOUND'
                                    ,'PARAMETER'
                                    ,p_parameter);
         RAISE param_details_not_found;
      END IF;

      CLOSE cur_process_parameter;

      IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
         gme_debug.put_line ('after getting param details ');
      END IF;

      -- Check for target value
      IF p_process_param_rec.target_value IS NOT NULL THEN
         IF l_parameter_type = 2 THEN
            BEGIN
               v_temp := p_process_param_rec.target_value;
            EXCEPTION
               WHEN VALUE_ERROR THEN
                  x_return_status := fnd_api.g_ret_sts_error;
                  gme_common_pvt.log_message
                                          ('GME_INVALID_FIELD'
                                          ,'FIELD'
                                          ,'p_process_param_rec.target_value');
                  RETURN;
            END;
         ELSE
            IF l_parameter_type = 3 THEN
               OPEN cur_process_value (l_parameter_id
                                      ,p_process_param_rec.target_value);

               FETCH cur_process_value
                INTO l_value;

               IF cur_process_value%NOTFOUND THEN
                  CLOSE cur_process_value;

                  gme_common_pvt.log_message
                                          ('GME_INVALID_FIELD'
                                          ,'FIELD'
                                          ,'p_process_param_rec.target_value');
                  RAISE wrong_target_value;
               END IF;

               CLOSE cur_process_value;
            END IF;
         END IF;                                     /* l_parameter_type = 2*/

         --check for target value with minimum value
         IF l_minimum_value IS NOT NULL THEN
            IF (p_process_param_rec.target_value < l_minimum_value) THEN
               fnd_message.set_name ('GMD', 'LM_OUTOFRANGE');
            END IF;
         END IF;                                          /* l_minimum_value*/

         --check for target value with maximum value
         IF l_maximum_value IS NOT NULL THEN
            IF (p_process_param_rec.target_value > l_maximum_value) THEN
               fnd_message.set_name ('GMD', 'LM_OUTOFRANGE');
            END IF;
         END IF;                                          /* l_maximum_value*/
      END IF;                /* p_process_param_rec.target_value IS NOT NULL*/

      --check for actual value
      IF l_step_status IN (2, 3) THEN
         IF p_process_param_rec.actual_value IS NOT NULL THEN
            IF l_parameter_type = 2 THEN
               BEGIN
                  v_temp := p_process_param_rec.actual_value;
               EXCEPTION
                  WHEN VALUE_ERROR THEN
                     gme_common_pvt.log_message
                                          ('GME_INVALID_FIELD'
                                          ,'FIELD'
                                          ,'p_process_param_rec.actual_value');
                     x_return_status := fnd_api.g_ret_sts_error;
                     RETURN;
               END;

               -- check for actual value with minimum value
               IF l_minimum_value IS NOT NULL THEN
                  IF (p_process_param_rec.actual_value < l_minimum_value) THEN
                     fnd_message.set_name ('GMD', 'LM_OUTOFRANGE');
                  END IF;
               END IF;                                    /* l_minimum_value*/

               --check for actual value with maximum value
               IF l_maximum_value IS NOT NULL THEN
                  IF (p_process_param_rec.actual_value > l_maximum_value) THEN
                     fnd_message.set_name ('GMD', 'LM_OUTOFRANGE');
                  END IF;
               END IF;                                    /* l_maximum_value*/
            END IF;                                  /* l_parameter_type = 2*/
         END IF;             /* p_process_param_rec.actual_value IS NOT NULL*/
      END IF;                                      /* l_step_status IN (2,3)*/

      IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
         gme_debug.put_line ('after validate actual value');
      END IF;

      --Validate flex fields
      /* Nsinha as part of GME_Process_Parameter_APIs_TD
         Modify the existing logic and directly call gme_api_validate_flex_fld_pvt.validate_flex_process_param API
         along with newly added IN parameter p_validate_flexfields. */
      gme_validate_flex_fld_pvt.validate_flex_process_param
                              (p_process_param_rec        => p_process_param_rec
                              ,p_validate_flexfields      => p_validate_flexfields
                              ,x_process_param_rec        => l_process_param_rec
                              ,x_return_status            => l_return_status);

      IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
         gme_debug.put_line ('validate flex return ' || l_return_status);
      END IF;

      IF l_return_status <> fnd_api.g_ret_sts_success THEN
         RAISE validation_failure;
      END IF;

      IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
         gme_debug.put_line ('after validate flex fields');
      END IF;

      l_process_param_rec.batch_id := l_batch_id;
      l_process_param_rec.batchstep_id := l_batchstep_id;
      l_process_param_rec.batchstep_activity_id := l_activity_id;
      l_process_param_rec.batchstep_resource_id := l_resource_id;
      l_process_param_rec.resources := l_resources;
      l_process_param_rec.parameter_id := l_parameter_id;
      l_process_param_rec.minimum_value := l_minimum_value;
      l_process_param_rec.maximum_value := l_maximum_value;
      l_process_param_rec.parameter_uom := l_units;

      -- null out values of actual fields for pending step
      IF l_step_status = 1 THEN
         l_process_param_rec.target_value := p_process_param_rec.target_value;
         l_process_param_rec.actual_value := NULL;
      ELSE
         l_process_param_rec.target_value := p_process_param_rec.target_value;
         l_process_param_rec.actual_value := p_process_param_rec.actual_value;
      END IF;

      IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
         gme_debug.put_line ('call insert  ' || x_return_status);
      END IF;

      /* Nsinha as part of GME_Process_Parameter_APIs_TD:
         Replaced the call to gme_api_process_parameters.insert_process_param
         with gme_process_parameters_dbl.insert_row
      */
      IF NOT gme_process_parameters_dbl.insert_row
                                 (p_process_parameters      => l_process_param_rec
                                 ,x_process_parameters      => x_process_param_rec) THEN
         RAISE fnd_api.g_exc_error;
      END IF;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ':'
                             || 'Exiting with '
                             || x_return_status
                             || ' at '
                             || TO_CHAR (SYSDATE, 'MM/DD/YYYY HH24:MI:SS') );
      END IF;
   EXCEPTION
      WHEN input_param_missing OR validate_param_failed OR invalid_step_status OR param_details_not_found OR wrong_target_value THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN resource_fetch_error THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN val_pro_param_failed THEN
         x_return_status := l_return_status;
      WHEN validation_failure THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN OTHERS THEN
         x_return_status := fnd_api.g_ret_sts_unexp_error;
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
   END insert_process_parameter;

/*===========================================================================================
   Procedure
      update_process_parameter
   Description
     This particular procedure is used to insert parameter for an resource
   Parameters
     p_process_param_rec.batchstep_resource_id OR
     (p_plant_code,p_batch_no,p_batchstep_no,p_activity,p_parameter ) to uniquely
      identify an process parameter
     p_process_param_rec         gme_process_parameters%ROWTYPE  - details of the process parameter
     x_return_status              reflects return status of the API
     
      19-MAY-2015  Shaliu Chen     BUG 201102335
        Modify for Batch On Hold enhancement,add validation to raise an error
        if batch is on hold either STOP or PAUSE type.         
=============================================================================================*/
   PROCEDURE update_process_parameter (
      p_batch_no              IN              VARCHAR2 := NULL
     ,p_org_code              IN              VARCHAR2 := NULL
     ,p_validate_flexfields   IN              VARCHAR2 := fnd_api.g_false
     ,p_batchstep_no          IN              NUMBER := NULL
     ,p_activity              IN              VARCHAR2 := NULL
     ,p_parameter             IN              VARCHAR2 := NULL
     ,p_process_param_rec     IN              gme_process_parameters%ROWTYPE
     ,x_process_param_rec     OUT NOCOPY      gme_process_parameters%ROWTYPE
     ,x_return_status         OUT NOCOPY      VARCHAR2)
   IS
      CURSOR cur_validate_parameter (v_proc_param_id NUMBER)
      IS
         SELECT batchstep_id, batch_id
           FROM gme_process_parameters
          WHERE process_param_id = v_proc_param_id;

      CURSOR cur_validate_batch_type (v_proc_param_id NUMBER)
      IS
         SELECT 1
           FROM gme_batch_header b, gme_process_parameters p
          WHERE b.batch_id = p.batch_id
            AND p.process_param_id = v_proc_param_id
            AND b.batch_type = 10;

      CURSOR cur_process_parameter (v_parameter_id NUMBER)
      IS
         SELECT parameter_type
           FROM gmp_process_parameters
          WHERE parameter_id = v_parameter_id AND delete_mark = 0;

      CURSOR cur_process_value (v_parameter_id NUMBER)
      IS
         SELECT parameter_value
           FROM gmp_parameter_values
          WHERE parameter_id = v_parameter_id;

      input_param_missing       EXCEPTION;
      validate_param_failed     EXCEPTION;
      val_pro_param_failed      EXCEPTION;
      wrong_target_value        EXCEPTION;
      invalid_step_status       EXCEPTION;
      wrong_actual_value        EXCEPTION;
      validation_failure        EXCEPTION;
      param_not_found           EXCEPTION;
      update_param_failed       EXCEPTION;
      no_change                 EXCEPTION;
      param_details_not_found   EXCEPTION;
      batch_steps_fetch_error   EXCEPTION;
      l_api_name       CONSTANT VARCHAR2 (30)    := 'UPDATE_PROCESS_PARAMETER';
      l_return_status           VARCHAR2 (1)      := fnd_api.g_ret_sts_success;
      l_process_param_rec       gme_process_parameters%ROWTYPE;
      l_batch_steps             gme_batch_steps%ROWTYPE;
      l_proc_param_id           NUMBER;
      l_resource_id             NUMBER;
      l_activity_id             NUMBER;
      l_batchstep_id            NUMBER;
      l_dummy                   NUMBER;
      l_batch_id                NUMBER;
      l_step_status             NUMBER;
      l_parameter_id            NUMBER;
      l_parameter_type          NUMBER;
      l_minimum_value           NUMBER;
      l_maximum_value           NUMBER;
      
      l_units                   gme_process_parameters.PARAMETER_UOM%TYPE; -- 20567897       
      -- l_units                   VARCHAR2 (4);
      
      v_temp                    NUMBER;
      l_value                   NUMBER;
      l_flex_validate           BOOLEAN                          := FALSE;
      l_field_updated           BOOLEAN                          := FALSE;
   BEGIN
      IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
         gme_debug.put_line ('Calling public update process parameters Row');
      END IF;

      /* Initially let us assign the return status to success */
      x_return_status := fnd_api.g_ret_sts_success;

      IF (p_parameter IS NULL AND p_process_param_rec.process_param_id IS NULL) THEN
         gme_common_pvt.log_message ('GME_FIELD_VALUE_REQUIRED'
                                    ,'FIELD_NAME'
                                    ,'PROCESS PARAMETERS');
         RAISE input_param_missing;
      END IF;

      IF p_process_param_rec.process_param_id IS NOT NULL THEN
         -- validate the process parameter provided
         l_proc_param_id := p_process_param_rec.process_param_id;

         OPEN cur_validate_parameter (l_proc_param_id);

         FETCH cur_validate_parameter
          INTO l_batchstep_id, l_batch_id;

         IF cur_validate_parameter%NOTFOUND THEN
           CLOSE cur_validate_parameter;

           gme_common_pvt.log_message ('GME_PARAM_ID_NOT_FOUND'
                                      ,'PROCESS_PARAM_ID'
                                      ,l_proc_param_id);
           RAISE param_not_found;
         END IF;

         CLOSE cur_validate_parameter;

         /* Fetch the step status*/
         l_batch_steps.batch_id := l_batch_id;
         l_batch_steps.batchstep_id := l_batchstep_id;

         IF NOT (gme_batch_steps_dbl.fetch_row (l_batch_steps, l_batch_steps) ) THEN
           RAISE batch_steps_fetch_error;
         END IF;

         l_step_status := l_batch_steps.step_status;

         --added by qzeng no need to check if done in bulk validation
         IF NVL(gme_common_pvt.g_bulk_validation_done, 'N') = 'N' THEN
           -- make sure process parameter id does not belong to an FPO
           OPEN cur_validate_batch_type (l_proc_param_id);

           FETCH cur_validate_batch_type
            INTO l_dummy;

           IF cur_validate_batch_type%FOUND THEN
             CLOSE cur_validate_batch_type;

             gme_common_pvt.log_message ('GME_FPO_PARAM_NO_EDIT');
             RAISE validate_param_failed;
           END IF;

           CLOSE cur_validate_batch_type;
         END IF;
      ELSE
         validate_process_param (p_org_code           => p_org_code
                                ,p_batch_no           => p_batch_no
                                ,p_batchstep_no       => p_batchstep_no
                                ,p_activity           => p_activity
                                ,p_resource           => p_process_param_rec.resources
                                ,p_parameter          => p_parameter
                                ,x_batch_id           => l_batch_id
                                ,x_batchstep_id       => l_batchstep_id
                                ,x_activity_id        => l_activity_id
                                ,x_resource_id        => l_resource_id
                                ,x_parameter_id       => l_parameter_id
                                ,x_proc_param_id      => l_proc_param_id
                                ,x_step_status        => l_step_status
                                ,x_return_status      => l_return_status);

         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line (   'after validate_process_param '
                                || l_return_status);
         END IF;

         IF l_return_status <> 'S' THEN
            RAISE val_pro_param_failed;
         END IF;
      END IF;
      
      /* Shaliu Chen     19-MAY-2015  BUG 201102335
         raise an error if batch is on hold with either STOP or PAUSE type
      */
      IF gme_common_pvt.get_batch_hold_status(l_batch_id) <> 'R' THEN
        gme_common_pvt.log_message('GME_BATCH_ONHOLD_CHECK',
                                   'ACTION_NAME',
                                   'Update Process Parameter');
        RAISE fnd_api.g_exc_error;
      END IF;       
      
      --added by qzeng no need to check if done in bulk validation
      IF NVL(gme_common_pvt.g_bulk_validation_done, 'N') = 'N' THEN
        IF l_step_status IN (4, 5) THEN
          gme_common_pvt.log_message ('PC_STEP_STATUS_ERR');
          RAISE invalid_step_status;
       END IF;
      END IF;
      l_process_param_rec.process_param_id := l_proc_param_id;

      IF NOT gme_process_parameters_dbl.fetch_row (l_process_param_rec
                                                  ,l_process_param_rec) THEN
         RAISE fnd_api.g_exc_error;
      END IF;

      -- Get process parameters details from GMP tables
      OPEN cur_process_parameter (l_process_param_rec.parameter_id);

      FETCH cur_process_parameter
       INTO l_parameter_type;

      IF cur_process_parameter%NOTFOUND THEN
         CLOSE cur_process_parameter;

         RAISE param_details_not_found;
      END IF;

      CLOSE cur_process_parameter;

      IF l_step_status IN (2, 3) THEN
         IF p_process_param_rec.actual_value = fnd_api.g_miss_char THEN
            gme_common_pvt.log_message ('GME_FIELD_VALUE_REQUIRED'
                                       ,'FIELD_NAME'
                                       ,'ACTUAL_VALUE');
            RAISE input_param_missing;
         ELSIF (       p_process_param_rec.actual_value IS NOT NULL
                   AND ( (NVL (l_process_param_rec.actual_value, '0') <>
                                              p_process_param_rec.actual_value) )
                OR l_process_param_rec.actual_value IS NULL) THEN
            IF l_parameter_type = 2 THEN
               BEGIN
                  v_temp := p_process_param_rec.actual_value;
               EXCEPTION
                  WHEN VALUE_ERROR THEN
                     gme_common_pvt.log_message
                                          ('GME_INVALID_FIELD'
                                          ,'FIELD'
                                          ,'p_process_param_rec.actual_value');
                     x_return_status := fnd_api.g_ret_sts_error;
                     RETURN;
               END;
            END IF;

            l_field_updated := TRUE;
            l_process_param_rec.actual_value :=
                                              p_process_param_rec.actual_value;
         END IF;
      END IF;                                                /*l_step_status*/

      IF p_validate_flexfields = fnd_api.g_true THEN
         l_field_updated := TRUE;
      END IF;

      /* Nsinha as part of GME_Process_Parameter_APIs_TD
         Modify the existing logic and directly call gme_api_validate_flex_fld_pvt.validate_flex_process_param API
         along with newly added IN parameter p_validate_flexfields. */
      gme_validate_flex_fld_pvt.validate_flex_process_param
                              (p_process_param_rec        => p_process_param_rec
                              ,p_validate_flexfields      => p_validate_flexfields
                              ,x_process_param_rec        => l_process_param_rec
                              ,x_return_status            => l_return_status);

      IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
         gme_debug.put_line ('validate flex return ' || l_return_status);
      END IF;

      IF l_return_status <> fnd_api.g_ret_sts_success THEN
         RAISE validation_failure;
      END IF;

      IF NOT (l_field_updated) THEN
         gme_common_pvt.log_message ('GME_NO_CHANGE_TO_UPD');
         RAISE no_change;
      END IF;

      /* Replace the call to gme_api_process_parameters.update_process_param
         with gme_process_parameters_dbl.update_row
      */
      IF NOT gme_process_parameters_dbl.update_row
                                  (p_process_parameters      => l_process_param_rec) THEN
         RAISE fnd_api.g_exc_error;
      END IF;

      x_process_param_rec := l_process_param_rec;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ':'
                             || 'Exiting with '
                             || x_return_status
                             || ' at '
                             || TO_CHAR (SYSDATE, 'MM/DD/YYYY HH24:MI:SS') );
      END IF;
   EXCEPTION
      WHEN input_param_missing OR validate_param_failed OR param_not_found OR invalid_step_status OR param_details_not_found OR batch_steps_fetch_error OR wrong_target_value OR no_change THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN VALUE_ERROR THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN fnd_api.g_exc_error THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN validation_failure THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN val_pro_param_failed OR update_param_failed THEN
         x_return_status := l_return_status;
      WHEN OTHERS THEN
         x_return_status := fnd_api.g_ret_sts_unexp_error;
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
   END update_process_parameter;

    /*===========================================================================================
      Procedure
         delete_process_parameter
      Description
        This particular procedure is used to insert parameter for an resource
      Parameters
        p_process_param_rec.batchstep_resource_id OR
        (p_plant_code,p_batch_no,p_batchstep_no,p_activity,p_parameter ) to uniquely
         identify an process parameter
        p_process_param_rec         gme_process_parameters%ROWTYPE  - details of the process parameter
        x_return_status              reflects return status of the API
        
        
      19-MAY-2015  Shaliu Chen     BUG 201102335
        Modify for Batch On Hold enhancement,add validation to raise an error
        if batch is on hold either STOP or PAUSE type.          
   =============================================================================================*/
   PROCEDURE delete_process_parameter (
      p_batch_no            IN              VARCHAR2
     ,p_org_code            IN              VARCHAR2
     ,p_batchstep_no        IN              NUMBER
     ,p_activity            IN              VARCHAR2
     ,p_parameter           IN              VARCHAR2
     ,p_process_param_rec   IN              gme_process_parameters%ROWTYPE
     ,x_return_status       OUT NOCOPY      VARCHAR2)
   IS
      CURSOR cur_validate_parameter (v_proc_param_id NUMBER)
      IS
         SELECT batchstep_id, batch_id
           FROM gme_process_parameters
          WHERE process_param_id = v_proc_param_id;

      CURSOR cur_validate_batch_type (v_proc_param_id NUMBER)
      IS
         SELECT 1
           FROM gme_batch_header b, gme_process_parameters p
          WHERE b.batch_id = p.batch_id
            AND p.process_param_id = v_proc_param_id
            AND b.batch_type = 10;
     
     /*siva added following cursor */
      CURSOR cur_validate_batch_status(v_batch_id NUMBER)
      IS
         SELECT batch_status
           FROM gme_batch_header
         WHERE batch_id = v_batch_id;

      input_param_missing     EXCEPTION;
      validate_param_failed   EXCEPTION;
      val_pro_param_failed    EXCEPTION;
      wrong_target_value      EXCEPTION;
      invalid_batch_status     EXCEPTION;    
      param_not_found         EXCEPTION;
      delete_param_failed     EXCEPTION;
      l_api_name     CONSTANT VARCHAR2 (30)      := 'DELETE_PROCESS_PARAMETER';
      l_return_status         VARCHAR2 (1)        := fnd_api.g_ret_sts_success;
      l_process_param_rec     gme_process_parameters%ROWTYPE;
      l_proc_param_id         NUMBER;
      l_resource_id           NUMBER;
      l_activity_id           NUMBER;
      l_batchstep_id          NUMBER;
      l_dummy                 NUMBER;
      l_batch_id              NUMBER;
      l_step_status           NUMBER;
      l_parameter_id          NUMBER;
      l_batch_status          NUMBER;
   BEGIN
      IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
         gme_debug.put_line ('Calling public delete process parameters Row');
      END IF;

      IF (p_parameter IS NULL AND p_process_param_rec.process_param_id IS NULL) THEN
         gme_common_pvt.log_message ('GME_FIELD_VALUE_REQUIRED'
                                    ,'FIELD_NAME'
                                    ,'PROCESS PARAMETERS');
         RAISE input_param_missing;
      END IF;

      IF p_process_param_rec.process_param_id IS NOT NULL THEN
         -- validate the process parameter provided
         l_proc_param_id := p_process_param_rec.process_param_id;
         --added by qzeng no need to check if done in bulk validation
         IF NVL(gme_common_pvt.g_bulk_validation_done, 'N') = 'N' THEN
           OPEN cur_validate_parameter (l_proc_param_id);

           FETCH cur_validate_parameter
            INTO l_batchstep_id, l_batch_id;

           IF cur_validate_parameter%NOTFOUND THEN
             CLOSE cur_validate_parameter;

             gme_common_pvt.log_message ('GME_PARAM_ID_NOT_FOUND'
                                        ,'PROCESS_PARAM_ID'
                                        ,l_proc_param_id);
              RAISE param_not_found;
           END IF;

           CLOSE cur_validate_parameter;

           -- make sure process parameter id does not belong to an FPO
           OPEN cur_validate_batch_type (l_proc_param_id);

           FETCH cur_validate_batch_type
            INTO l_dummy;

           IF cur_validate_batch_type%FOUND THEN
             CLOSE cur_validate_batch_type;

             gme_common_pvt.log_message ('GME_FPO_PARAM_NO_EDIT');
             RAISE validate_param_failed;
           END IF;

           CLOSE cur_validate_batch_type;
	 END IF;
      ELSE
         validate_process_param (p_org_code           => p_org_code
                                ,p_batch_no           => p_batch_no
                                ,p_batchstep_no       => p_batchstep_no
                                ,p_activity           => p_activity
                                ,p_resource           => p_process_param_rec.resources
                                ,p_parameter          => p_parameter
                                ,x_batch_id           => l_batch_id
                                ,x_batchstep_id       => l_batchstep_id
                                ,x_activity_id        => l_activity_id
                                ,x_resource_id        => l_resource_id
                                ,x_parameter_id       => l_parameter_id
                                ,x_proc_param_id      => l_proc_param_id
                                ,x_step_status        => l_step_status
                                ,x_return_status      => l_return_status);

         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line (   'after validate_process_param '
                                || l_return_status);
         END IF;

         IF l_return_status <> 'S' THEN
            RAISE val_pro_param_failed;
         END IF;
      END IF;
      
      /* Shaliu Chen     19-MAY-2015  BUG 201102335
         raise an error if batch is on hold with either STOP or PAUSE type
      */
      IF gme_common_pvt.get_batch_hold_status(l_batch_id) <> 'R' THEN
        gme_common_pvt.log_message('GME_BATCH_ONHOLD_CHECK',
                                   'ACTION_NAME',
                                   'Delete Process Parameter');
        RAISE fnd_api.g_exc_error;
      END IF;       
      
     --added by qzeng no need to check if done in bulk validation
     IF NVL(gme_common_pvt.g_bulk_validation_done, 'N') = 'N' THEN
       /* siva begin */
       OPEN cur_validate_batch_status(l_batch_id);
       FETCH cur_validate_batch_status INTO l_batch_status;
       IF cur_validate_batch_status%FOUND THEN
         IF l_batch_status <> 1 THEN
           CLOSE cur_validate_batch_status;
           gme_common_pvt.log_message('PM_WRONG_STATUS');
           RAISE invalid_batch_status;
         END IF;
       END IF;
       CLOSE cur_validate_batch_status;

       /* IF l_step_status <> 1 THEN
          gme_common_pvt.log_message ('PC_STEP_STATUS_ERR');
          RAISE invalid_step_status;
       END IF;*/
       /* siva end */
     END IF;

      l_process_param_rec.process_param_id := l_proc_param_id;
      x_return_status := l_return_status;

      /* Replace the call to gme_api_process_parameters.delete_process_param
         with gme_process_parameters_dbl.delete_row
      */
      IF NOT gme_process_parameters_dbl.delete_row
                                  (p_process_parameters      => l_process_param_rec) THEN
         RAISE fnd_api.g_exc_error;
      END IF;
   EXCEPTION
      WHEN input_param_missing OR validate_param_failed OR param_not_found OR invalid_batch_status THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN val_pro_param_failed OR delete_param_failed THEN
         x_return_status := l_return_status;
      WHEN OTHERS THEN
         x_return_status := fnd_api.g_ret_sts_unexp_error;
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
   END delete_process_parameter;

/*===========================================================================================
   Procedure
      validate_process_param
   Description
     This particular procedure is used to validate parameter combo provided to identify an activity
     passed to rsrc APIs
   Parameters
     (p_plant_code,p_batch_no,step_no and activity )   to uniquely identify an activity
     x_return_status              reflects return status of the API
=============================================================================================*/
   PROCEDURE validate_process_param (
      p_org_code        IN              VARCHAR2
     ,p_batch_no        IN              VARCHAR2
     ,p_batchstep_no    IN              NUMBER
     ,p_activity        IN              VARCHAR2
     ,p_resource        IN              VARCHAR2
     ,p_parameter       IN              VARCHAR2
     ,x_batch_id        OUT NOCOPY      NUMBER
     ,x_batchstep_id    OUT NOCOPY      NUMBER
     ,x_activity_id     OUT NOCOPY      NUMBER
     ,x_resource_id     OUT NOCOPY      NUMBER
     ,x_parameter_id    OUT NOCOPY      NUMBER
     ,x_proc_param_id   OUT NOCOPY      NUMBER
     ,x_step_status     OUT NOCOPY      NUMBER
     ,x_return_status   OUT NOCOPY      VARCHAR2)
   IS
      l_api_name        CONSTANT VARCHAR2 (30)    := 'validate_process_param';
      l_activity_id              NUMBER;
      l_parameter_id             NUMBER;
      l_resource                 VARCHAR2 (16);
      l_step_status              NUMBER;
      l_resource_id              NUMBER;
      l_batch_type               NUMBER;
      l_rsrc_not_found           BOOLEAN;
      l_act_found                BOOLEAN;
      l_count                    NUMBER                     := 0;
      l_act_count                NUMBER                     := 0;
      l_param_no                 NUMBER                     := 0;
      l_param_id                 NUMBER;
      l_batch_header             gme_batch_header%ROWTYPE;
      l_batch_steps              gme_batch_steps%ROWTYPE;

      CURSOR cur_check_act_resource (
         v_step_id    NUMBER
        ,v_activity   VARCHAR2
        ,v_batch_id   NUMBER
        ,v_resource   VARCHAR2)
      IS
         SELECT COUNT (*)
           FROM gme_batch_step_activities a, gme_batch_step_resources r
          WHERE a.batch_id = r.batch_id
            AND a.batchstep_id = r.batchstep_id
            AND a.batchstep_activity_id = r.batchstep_activity_id
            AND a.batch_id = v_batch_id
            AND a.batchstep_id = v_step_id
            AND a.activity = v_activity
            AND r.resources = v_resource;

      CURSOR cur_get_activity_id (
         v_step_id    NUMBER
        ,v_activity   VARCHAR2
        ,v_batch_id   NUMBER)
      IS
         SELECT batchstep_activity_id
           FROM gme_batch_step_activities
          WHERE batchstep_id = v_step_id
            AND batch_id = v_batch_id
            AND activity = v_activity;

      CURSOR cur_fetch_resource_dtl (v_activity_id NUMBER, v_resource VARCHAR2)
      IS
         SELECT batchstep_resource_id
           FROM gme_batch_step_resources
          WHERE batchstep_activity_id = v_activity_id
            AND resources = v_resource;

      CURSOR cur_check_parameter (v_parameter VARCHAR2)
      IS
         SELECT parameter_id
           FROM gmp_process_parameters
          WHERE parameter_name = v_parameter AND delete_mark = 0;

      CURSOR cur_get_parameter (v_resource_id NUMBER, v_parameter VARCHAR2)
      IS
         SELECT process_param_id
           FROM gme_process_parameters p, gmp_process_parameters l
          WHERE p.parameter_id = l.parameter_id
            AND p.batchstep_resource_id = v_resource_id
            AND l.parameter_name = v_parameter
            AND l.delete_mark = 0;

      batch_header_fetch_error   EXCEPTION;
      batch_steps_fetch_error    EXCEPTION;
      stepactivity_not_found     EXCEPTION;
      resource_not_found         EXCEPTION;
      dup_act_rsrc_found         EXCEPTION;
      input_param_missing        EXCEPTION;
      dup_param_found            EXCEPTION;
      param_not_found            EXCEPTION;
      setup_failure              EXCEPTION;
   BEGIN
      /* Initially let us assign the return status to success */
      x_return_status := fnd_api.g_ret_sts_success;

      IF p_activity IS NULL THEN
         gme_common_pvt.log_message ('GME_FIELD_VALUE_REQUIRED'
                                    ,'FIELD_NAME'
                                    ,'ACTIVITY');
         RAISE input_param_missing;
      ELSIF p_resource IS NULL THEN
         gme_common_pvt.log_message ('GME_FIELD_VALUE_REQUIRED'
                                    ,'FIELD_NAME'
                                    ,'RESOURCE');
         RAISE input_param_missing;
      END IF;

      -- Check setup is done. For all profile/parameter values based on orgn_code/organization_id.
      gme_common_pvt.g_setup_done :=
                               gme_common_pvt.setup (p_org_code      => p_org_code);

      IF NOT gme_common_pvt.g_setup_done THEN
         RAISE setup_failure;
      END IF;

      l_batch_header.organization_id := gme_common_pvt.g_organization_id;
      -- Validate input param one by one to see if it identifies a resource/process parameter correctly
      l_batch_header.organization_id := gme_common_pvt.g_organization_id;
      l_batch_header.batch_no := p_batch_no;
      l_batch_header.batch_type := 0;

      IF NOT (gme_batch_header_dbl.fetch_row (l_batch_header, l_batch_header) ) THEN
         RAISE batch_header_fetch_error;
      END IF;

      IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
         gme_debug.put_line ('validation- after batch fetch');
      END IF;

      x_batch_id := l_batch_header.batch_id;
      -- use batch_id to fetch batchstep_id
      l_batch_steps.batch_id := x_batch_id;
      l_batch_steps.batchstep_no := p_batchstep_no;

      IF NOT (gme_batch_steps_dbl.fetch_row (l_batch_steps, l_batch_steps) ) THEN
         RAISE batch_steps_fetch_error;
      END IF;

      IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
         gme_debug.put_line ('validation- after step fetch');
      END IF;

      x_step_status := l_batch_steps.step_status;
      x_batchstep_id := l_batch_steps.batchstep_id;

      FOR step_activity IN cur_get_activity_id (x_batchstep_id
                                               ,p_activity
                                               ,x_batch_id) LOOP
         x_activity_id := step_activity.batchstep_activity_id;
         l_act_found := TRUE;
         l_activity_id := x_activity_id;

         OPEN cur_fetch_resource_dtl (l_activity_id, p_resource);

         FETCH cur_fetch_resource_dtl
          INTO l_resource_id;

         IF cur_fetch_resource_dtl%NOTFOUND THEN
            CLOSE cur_fetch_resource_dtl;

            l_rsrc_not_found := TRUE;
         ELSE
            CLOSE cur_fetch_resource_dtl;

            l_rsrc_not_found := FALSE;
            x_resource_id := l_resource_id;
            EXIT;
         END IF;
      END LOOP;

      IF NOT l_act_found THEN
         gme_common_pvt.log_message ('GME_STEP_ACTIVITY_NOT_FOUND'
                                    ,'ACTIVITY'
                                    ,p_activity
                                    ,'STEP_NO'
                                    ,p_batchstep_no);
         RAISE stepactivity_not_found;
      END IF;

      IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
         gme_debug.put_line ('validation- after activity check');
      END IF;

      -- If resource was not found in any activity then report error
      IF l_rsrc_not_found THEN
         gme_common_pvt.log_message ('GME_RSRC_NOT_FOUND'
                                    ,'RESOURCE'
                                    ,p_resource
                                    ,'ACTIVITY'
                                    ,p_activity);
         RAISE resource_not_found;
      END IF;

      IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
         gme_debug.put_line ('validation- after duplicate resource');
      END IF;

      -- if same activity and resource combination exists then raise error
      OPEN cur_check_act_resource (x_batchstep_id
                                  ,p_activity
                                  ,x_batch_id
                                  ,p_resource);

      FETCH cur_check_act_resource
       INTO l_act_count;

      IF l_act_count > 1 THEN
         CLOSE cur_check_act_resource;

         gme_common_pvt.log_message ('GME_BATCH_SAME_ACT_RSRC_FOUND'
                                    ,'ACTIVITY'
                                    ,p_activity
                                    ,'RESOURCE'
                                    ,p_resource);
         RAISE dup_act_rsrc_found;
      END IF;

      CLOSE cur_check_act_resource;

      IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
         gme_debug.put_line ('validation- after act-resource check');
      END IF;

      -- check teh parameter exists in gmp
      IF p_parameter IS NOT NULL THEN
         OPEN cur_check_parameter (p_parameter);

         FETCH cur_check_parameter
          INTO l_parameter_id;

         IF cur_check_parameter%NOTFOUND THEN
            CLOSE cur_check_parameter;

            gme_common_pvt.log_message ('GME_PARAM_NOT_FOUND'
                                       ,'PARAMETER'
                                       ,p_parameter);
            RAISE param_not_found;
         END IF;

         CLOSE cur_check_parameter;
      END IF;

      x_parameter_id := l_parameter_id;

      -- check for process parameter for update and delete only
      IF p_parameter IS NOT NULL THEN
         gme_debug.put_line (   'validation- after act-resource check'
                             || x_resource_id
                             || 'p_parameter');

         FOR process_param IN cur_get_parameter (x_resource_id, p_parameter) LOOP
            l_count := l_count + 1;
            x_proc_param_id := process_param.process_param_id;
         END LOOP;

         IF l_count > 1 THEN
            gme_common_pvt.log_message ('GME_BATCH_SAME_PARAM_FOUND'
                                       ,'PARAMETER'
                                       ,p_parameter);
            RAISE dup_param_found;
         ELSIF l_count = 0 THEN
            gme_common_pvt.log_message ('GME_PARAM_NOT_FOUND'
                                       ,'PARAMETER'
                                       ,p_parameter);
            RAISE param_not_found;
         END IF;
      END IF;

      IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
         gme_debug.put_line ('validation- after process parameter');
      END IF;

      IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
         gme_debug.put_line ('validation-batch_id' || x_batch_id);
         gme_debug.put_line ('validation-x_batchstep_id' || x_batchstep_id);
         gme_debug.put_line ('validation-x_activity_id' || x_activity_id);
         gme_debug.put_line ('validation-x_resource_id ' || x_resource_id);
         gme_debug.put_line ('validation-x_parameter_id ' || x_parameter_id);
         gme_debug.put_line ('validation-x_step_status  ' || x_step_status);
      END IF;
   EXCEPTION
      WHEN setup_failure THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN batch_header_fetch_error OR batch_steps_fetch_error OR input_param_missing THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN stepactivity_not_found OR resource_not_found OR dup_act_rsrc_found OR dup_param_found OR param_not_found THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN OTHERS THEN
         x_return_status := fnd_api.g_ret_sts_unexp_error;
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
   END validate_process_param;
END gme_process_parameters_pvt;
/

COMMIT ;
EXIT;
--show error
