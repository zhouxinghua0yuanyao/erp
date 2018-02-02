/* +======================================================================+ */
/* |    Copyright (c) 2005, 2017 Oracle and/or its affiliates.           | */
/* |                         All rights reserved.                         | */
/* |                           Version 12.0.0                             | */
/* +======================================================================+ */
REM dbdrv: sql ~PROD ~PATH ~FILE none none none package &phase=plb \
REM dbdrv: checkfile(120.10.12010000.13=120.15.12020000.8)(120.6.12000000.9=120.10.12010000.7)(120.6.12000000.6=120.10.12010000.4):~PROD:~PATH:~FILE
SET VERIFY OFF;
WHENEVER SQLERROR EXIT FAILURE ROLLBACK;
/*
REM **********************************************************************
REM *                                                                    *
REM * FILE:    GMEVUSQB.pls                                              *
REM * PURPOSE: Package Body for the EDIT STEP QTY routines               *
REM * AUTHOR:  Thomas Daniel, OPM Development                            *
REM * DATE:    April 02nd 2001                                           *
REM * HISTORY:                                                           *
REM * ========                                                           *
REM * PURPOSE: Package Body for the EDIT STEP QTY routines               *
REM * AUTHOR:  Thomas Daniel, OPM Development                            *
REM * DATE:    April 02nd 2001                                           *
REM * HISTORY:                                                           *
REM * ========                                                           *
REM *  SivakumarG 09-JUN-2006 Bug#5231180                                *
REM *   Added new procedure recalculate_charges and added p_step_qty and *
REM *   p_max_capacity parameters to calc_charge procedure.              *

REM *   A. Mishra   11-DEC-2009  Bug 8564008      
REM *   The resource usage was not getting converted to hours 
REM *   as the procedure GET_USAGE_IN HOURS was not getting called
REM *   IN the procedure reduce_pending_usage gave a call to this procedure to 
REM *   convert the usage in hours first before computing the transaction dates

REM *  G. Muratore   19-MAR-2010  Bug 8751983     
REM *   Fetch resource transactions in reverse trans order just in case 
REM *   this is being called by negative IB. Also, rework logic so that 
REM *   all the existing resource transactions are not blindly removed.
REM *   Reverse out only what is necessary. Also, stamp any new resource
REM *   transaction caused by Negative IB with the trans_date of the original.
REM *   PROCEDURE: adjust_actual_usage and build_insert_resource_txn 

REM *  G. Muratore   01-SEP-2010  Bug 10051993     
REM *   Pass in the new step qty to calc_char so that charges are calculated properly. 
REM *   PROCEDURE: calculate_quantities  

REM *  G. Muratore   27-JUN-2011  Bug 12568872     
REM *   Initialize charge to 1 if necessary. This usually happens when
REM *   step qty is less than capacity.
REM *   PROCEDURE: calc_charge                

REM * G. Muratore    23-Oct-2013  Bug 16297308
REM *   Use correct variable when calculating usage hrs.
REM *   PROCEDURE: reduce_pending_usage

REM * Shaliu Chen    18-JUL-2014  ER 19161894                                                 
REM *   Modify create_batch to invoke requisition creation program if batch include OSP step

REM * G. Muratore    22-JUN-2017  Bug 26094274
REM *   Compare two usage values using the same precision.
REM *   PROCEDURE: adjust_actual_usage
REM **********************************************************************
*/

/*************************************************************************
* This file contains the procedure for updating step quantities in Oracle*
* Process Manufacturing (OPM). Each procedure has a common set of        *
* parameters to which API-specific parameters are appended.              *
*************************************************************************/

CREATE OR REPLACE PACKAGE BODY gme_update_step_qty_pvt AS
/* $Header: GMEVUSQB.pls 120.15.12020000.8 2017/06/22 17:53:33 gmurator ship $ */
   g_debug                     VARCHAR2 (5)
                                         := fnd_profile.VALUE ('AFLOG_LEVEL');
   g_pkg_name         CONSTANT VARCHAR2 (30) := 'gme_update_step_qty_pvt';
   p_gme_calc_int_rsrc_usage   NUMBER        := NULL;

/*===========================================================================================
Procedure
  update_step_qty
Description
  This particular procedure call changes the current step qty and propogates it.
Parameters
  p_batch_step_rec         The batch step row to identify the step.
  x_message_count    The number of messages in the message stack
  x_message_list     message stack where the api writes its messages
  x_return_status    outcome of the API call
            S - Success
            E - Error
            U - Unexpected error
=============================================================================================*/
   PROCEDURE update_step_qty (
      p_batch_step_rec         IN              gme_batch_steps%ROWTYPE
     ,x_message_count          OUT NOCOPY      NUMBER
     ,x_message_list           OUT NOCOPY      VARCHAR2
     ,x_return_status          OUT NOCOPY      VARCHAR2
     ,x_batch_step_rec         OUT NOCOPY      gme_batch_steps%ROWTYPE
     ,p_routing_scale_factor   IN              NUMBER DEFAULT NULL
     ,p_backflush_factor       IN              NUMBER DEFAULT NULL
     ,p_dependency_type        IN              NUMBER DEFAULT NULL
     ,p_material_step_id       IN              NUMBER DEFAULT NULL)
   IS
      l_api_name        CONSTANT VARCHAR2 (30)           := 'update_step_qty';
      /* Buffers for database reads/writes */
      l_batch_header             gme_batch_header%ROWTYPE;
      l_batch_step               gme_batch_steps%ROWTYPE;
      l_step_tbl                 gmd_auto_step_calc.step_rec_tbl;
      /* Exception definitions */
      batch_step_fetch_error     EXCEPTION;
      invalid_step_status        EXCEPTION;
      batch_header_fetch_error   EXCEPTION;
      invalid_batch_status       EXCEPTION;
      auto_step_calc_error       EXCEPTION;
      calc_quantities_error      EXCEPTION;
      /* Local variables */
      l_return_status            VARCHAR2 (1);
      l_rec                      NUMBER;
      /*ER 19161894  Shaliu Chen 18-JUL-2014*/
      l_osp_batch_flag           BOOLEAN;  
      l_res_txn_exist            BOOLEAN;

      CURSOR dependent_steps (
         v_batch_id       gme_batch_header.batch_id%TYPE
        ,v_batchstep_id   gme_batch_steps.batchstep_id%TYPE)
      IS
         SELECT     d.batchstep_id
               FROM gme_batch_step_dependencies d
              WHERE d.batch_id = v_batch_id
         START WITH (     (d.batch_id = v_batch_id)
                     AND (    (v_batchstep_id IS NULL)
                          OR (dep_step_id = v_batchstep_id) ) )
         CONNECT BY d.batch_id = PRIOR d.batch_id
                AND d.dep_step_id = PRIOR d.batchstep_id
           GROUP BY d.batchstep_id
--Bug#  5606246 Start
           --ORDER BY MAX (LEVEL) ASC;
          UNION
           SELECT p_material_step_id from dual;
--Bug#5606246 end
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name||' with batchstep_id '||p_batch_step_rec.batchstep_id);
      END IF;

      /* Set the savepoint before proceeding */
      SAVEPOINT update_step_qty;
      /* Set the return status to success initially */
      x_return_status := fnd_api.g_ret_sts_success;
      IF g_debug <= gme_debug.g_log_statement THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name||'p_routing_scale_factor ' || p_routing_scale_factor);
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name||'p_backflush_factor ' || p_backflush_factor);
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name||'p_dependency_type ' || p_dependency_type);
      END IF;

      /* Initialize output batch step row */
      IF NOT (gme_batch_steps_dbl.fetch_row (p_batch_step_rec
                                            ,x_batch_step_rec) ) THEN
         RAISE batch_step_fetch_error;
      END IF;

      /*  The current Step Status must allow editing of step qty */
      IF (x_batch_step_rec.step_status = gme_common_pvt.g_step_cancelled) THEN
         RAISE invalid_step_status;
      END IF;

      l_batch_header.batch_id := x_batch_step_rec.batch_id;

      /* Initialize local batch header */
      IF NOT (gme_batch_header_dbl.fetch_row (l_batch_header, l_batch_header) ) THEN
         RAISE batch_header_fetch_error;
      END IF;
      /* Load resource transactions in temp table */
      gme_trans_engine_util.load_rsrc_trans
                                           (p_batch_row          => l_batch_header
                                           ,x_rsc_row_count      => l_rec
                                           ,x_return_status      => l_return_status);

      IF (l_return_status <> fnd_api.g_ret_sts_success) THEN
         RAISE fnd_api.g_exc_error;
      END IF;
      IF g_debug <= gme_debug.g_log_statement THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name||': Resource transactions loaded '||l_rec);
      END IF;
      l_rec := 0;

      /*  The Batch must not be in Cancelled state to edit the step qty. */
      IF (l_batch_header.batch_status = gme_common_pvt.g_batch_cancelled) THEN
         RAISE invalid_batch_status;
      END IF;

      /* If automatic step qty calculation is set for the batch then */
      /* invoke the ASQC routine to calculate the actual step qty */
      IF l_batch_header.automatic_step_calculation = 1 THEN
         /* If the step status is certified or WIP then we have to invoke the automatic */
         /* step qty calculation for the whole batch as the transafer quantities would  */
         /* would affect the succeeding steps. Though the transfer quantities are not   */
         /* passed in WIP we have to calculate for the whole batch as this procedure    */
         /* might be called from uncertify batch wherein we have to decrease the        */
         /* succeeding step quantities.                                                 */

         IF g_debug <= gme_debug.g_log_statement THEN
            gme_debug.put_line ('Calling calc_step_qty. step status is ' ||x_batch_step_rec.step_status);
         END IF;
         
         IF x_batch_step_rec.step_status IN
               (gme_common_pvt.g_step_pending
               ,gme_common_pvt.g_step_wip
               ,gme_common_pvt.g_step_completed) THEN
            gmd_auto_step_calc.calc_step_qty
                         (p_parent_id              => l_batch_header.batch_id
                         ,p_step_tbl               => l_step_tbl
                         ,p_msg_count              => x_message_count
                         ,p_msg_stack              => x_message_list
                         ,p_return_status          => l_return_status
                         ,p_called_from_batch      => 1
                         ,p_step_no                => NULL
                         ,p_organization_id        => l_batch_header.organization_id);
         /* If the step status is not certified then we have to invoke the automatic   */
         /* step qty calculation only for the current step as it does not effect the   */
         /* succeeding step quantities.                                                */
         ELSE
            gmd_auto_step_calc.calc_step_qty
                         (p_parent_id              => l_batch_header.batch_id
                         ,p_step_tbl               => l_step_tbl
                         ,p_msg_count              => x_message_count
                         ,p_msg_stack              => x_message_list
                         ,p_return_status          => l_return_status
                         ,p_called_from_batch      => 1
                         ,p_step_no                => x_batch_step_rec.batchstep_no
                         ,p_organization_id        => l_batch_header.organization_id);
         END IF;               /* IF x_batch_step_rec.step_status IN (2, 3) */

         IF l_return_status <> x_return_status THEN
            RAISE auto_step_calc_error;
         END IF;
         

         /* Get the record number for the current step in the step table */
         l_rec :=
            gmd_auto_step_calc.get_step_rec (x_batch_step_rec.batchstep_no
                                            ,l_step_tbl);

         IF g_debug <= gme_debug.g_log_statement THEN
            gme_debug.put_line ('Back from calc_step_qty. qty is ' ||l_step_tbl (l_rec).step_qty);
            gme_debug.put_line ('MASS qty is ' ||l_step_tbl (l_rec).step_mass_qty);
            gme_debug.put_line ('VOL qty is ' ||l_step_tbl (l_rec).step_vol_qty);
         END IF;

         /* If step status is not in pending then actual quantities have to be updated. */
         IF x_batch_step_rec.step_status > gme_common_pvt.g_step_pending THEN
            /*
            ER 19161894  Shaliu Chen 18-JUL-2014
            Added for OPM Step Level OSP Project
            Add validation to prevent updating actual step qty for osp step.

            */            
            l_osp_batch_flag := FALSE;
            IF gme_osp.check_release_version THEN

              l_osp_batch_flag := gme_osp.is_OSP_batch(p_batch_id      => l_batch_header.batch_id,
                                                       p_batchstep_id  => x_batch_step_rec.batchstep_id);
                                                                                                         
            END IF;
            IF (NOT l_osp_batch_flag) THEN 
              x_batch_step_rec.actual_step_qty := l_step_tbl (l_rec).step_qty;
              x_batch_step_rec.actual_mass_qty :=
                                               l_step_tbl (l_rec).step_mass_qty;
              x_batch_step_rec.actual_volume_qty :=
                                                l_step_tbl (l_rec).step_vol_qty;
            END IF;
         ELSE
            x_batch_step_rec.plan_step_qty := l_step_tbl (l_rec).step_qty;
            x_batch_step_rec.plan_mass_qty :=
                                             l_step_tbl (l_rec).step_mass_qty;
            x_batch_step_rec.plan_volume_qty :=
                                              l_step_tbl (l_rec).step_vol_qty;
         END IF;
      /* If automatic step quantity calculation is not set for the current batch */
      ELSE
         IF x_batch_step_rec.step_status > gme_common_pvt.g_step_wip THEN
                        
            IF x_batch_step_rec.actual_step_qty IS NULL THEN
              
              /*
              ER 19161894  Shaliu Chen 18-JUL-2014
              Added for OPM Step Level OSP Project
              Add validation to prevent updating actual step qty for osp step.
              */  
              l_osp_batch_flag := FALSE;
              IF gme_osp.check_release_version THEN

                l_osp_batch_flag := gme_osp.is_OSP_batch(p_batch_id      => l_batch_header.batch_id,
                                                         p_batchstep_id  => x_batch_step_rec.batchstep_id);
                                                 
              END IF;             
              IF (NOT l_osp_batch_flag) THEN 
                 x_batch_step_rec.actual_step_qty :=
                                                 x_batch_step_rec.plan_step_qty;
                 x_batch_step_rec.actual_mass_qty :=
                                                 x_batch_step_rec.plan_mass_qty;
                 x_batch_step_rec.actual_volume_qty :=
                                               x_batch_step_rec.plan_volume_qty;
              END IF;
            /* If the actual step qty has been enetered by the user then we need */
            /* to recalculate the step mass or volume quantities                 */
            ELSE
               calculate_mass_vol_qty (x_batch_step_rec);
            END IF;
         /* Bug 3545726 backflush actual qty based on factor */
         ELSIF     x_batch_step_rec.step_status = gme_common_pvt.g_step_wip
               AND gme_common_pvt.g_backflush_rsrc_usg_ind = 1
               AND p_backflush_factor IS NOT NULL THEN
            /*
            ER 19161894  Shaliu Chen 18-JUL-2014
            Added for OPM Step Level OSP Project
            If step is OSP step,prevent to update Actual step qty column
            */            
            l_osp_batch_flag := FALSE;
            IF gme_osp.check_release_version THEN

              l_osp_batch_flag := gme_osp.is_OSP_batch(p_batch_id      => l_batch_header.batch_id,
                                                       p_batchstep_id  => x_batch_step_rec.batchstep_id);
            END IF;
            IF NOT l_osp_batch_flag THEN                
              x_batch_step_rec.actual_step_qty :=
                   NVL (x_batch_step_rec.actual_step_qty, 0)
                 + (x_batch_step_rec.plan_step_qty * p_backflush_factor);
              x_batch_step_rec.actual_mass_qty :=
                   NVL (x_batch_step_rec.actual_mass_qty, 0)
                 + (x_batch_step_rec.plan_mass_qty * p_backflush_factor);
              x_batch_step_rec.actual_volume_qty :=
                   NVL (x_batch_step_rec.actual_volume_qty, 0)
                 + (x_batch_step_rec.plan_volume_qty * p_backflush_factor);
            END IF;
         /* If the plan step qty has been changed then we need */
         /* to recalculate the step mass or volume quantities  */
         ELSE
            calculate_mass_vol_qty (x_batch_step_rec);
         END IF;                     /* IF x_batch_step_rec.step_status > 1 */
      END IF;           /* IF l_batch_header.automatic_step_calculation = 1 */

      /* If the step status is pending the null the actual quantities */
      IF x_batch_step_rec.step_status = gme_common_pvt.g_step_pending THEN
         x_batch_step_rec.actual_step_qty := NULL;
         x_batch_step_rec.actual_mass_qty := NULL;
         x_batch_step_rec.actual_volume_qty := NULL;
      END IF;


      calculate_quantities (p_batch_hdr_rec             => l_batch_header
                           ,p_batch_step_rec            => x_batch_step_rec
                           ,x_return_status             => l_return_status
                           ,p_routing_scale_factor      => p_routing_scale_factor
                           ,p_backflush_factor          => p_backflush_factor
                           ,p_dependency_type           => p_dependency_type);

      IF l_return_status <> x_return_status THEN
         RAISE calc_quantities_error;
      END IF;                      /* IF l_return_status <> x_return_status */
      
      IF l_batch_header.automatic_step_calculation = 1 THEN
         /* If the step status is certified or WIP then the transafer quantities */
         /* would affect the succeeding steps.                                   */
         IF x_batch_step_rec.step_status IN
               (gme_common_pvt.g_step_pending
               ,gme_common_pvt.g_step_wip
               ,gme_common_pvt.g_step_completed) THEN
            /* We have to invoke the calculate quantities routine for all the */
            /* succeeding steps.                                              */
            FOR dep_steps IN dependent_steps (l_batch_header.batch_id
                                             ,l_batch_step.batchstep_id) LOOP
               l_batch_step.batchstep_id := dep_steps.batchstep_id;
--Bug#5606089 Filtering out the null row.
 
                 IF l_batch_step.batchstep_id IS NOT NULL THEN
                     /* Initialize output batch step row */
                     IF NOT (gme_batch_steps_dbl.fetch_row (l_batch_step
                                                           ,l_batch_step) ) THEN
                        RAISE batch_step_fetch_error;
                     END IF;

                     /* We need to apply the transfer quantities if only the step status is in */
                     /* confirm with the dependent step status                                 */
                     IF    (     (x_batch_step_rec.step_status >
                                                       gme_common_pvt.g_step_pending)
                            AND (l_batch_step.step_status >
                                                       gme_common_pvt.g_step_pending) )
                        OR (     (x_batch_step_rec.step_status =
                                                       gme_common_pvt.g_step_pending)
                            AND (l_batch_step.step_status =
                                                       gme_common_pvt.g_step_pending) ) THEN
                        /* Get the record number for the current step in the step table */
                        l_rec :=
                           gmd_auto_step_calc.get_step_rec
                                                        (l_batch_step.batchstep_no
                                                        ,l_step_tbl);

                        /* If step status is not in pending then actual quantities have to */
                        /* be updated                                                      */
                        IF l_batch_step.step_status > gme_common_pvt.g_step_pending THEN
                          /*                                    
                          BEGIN ER 19161894  Shaliu Chen 18-JUL-2014               
                          Added for OPM Step Level OSP Project  
                          Add validation to prevent updating actual step qty for osp step.          
                          */     
                           l_osp_batch_flag := FALSE; 
                           IF gme_osp.check_release_version THEN
                      
                            l_osp_batch_flag := gme_osp.is_OSP_batch(p_batch_id      => l_batch_header.batch_id,
                                                                     p_batchstep_id  => l_batch_step.batchstep_id);
                                                                                                                                      
                           END IF;     
                           IF (NOT l_osp_batch_flag) THEN                                                 
                             l_batch_step.actual_step_qty :=
                                                          l_step_tbl (l_rec).step_qty;
                             l_batch_step.actual_mass_qty :=
                                                     l_step_tbl (l_rec).step_mass_qty;
                             l_batch_step.actual_volume_qty :=
                                                      l_step_tbl (l_rec).step_vol_qty;
                          END IF;
                        ELSE
                           l_batch_step.plan_step_qty :=
                                                        l_step_tbl (l_rec).step_qty;
                           l_batch_step.plan_mass_qty :=
                                                   l_step_tbl (l_rec).step_mass_qty;
                           l_batch_step.plan_volume_qty :=
                                                    l_step_tbl (l_rec).step_vol_qty;
                        END IF;                /* IF l_batch_step.step_status > 1 */
                        

                        
                        calculate_quantities (p_batch_hdr_rec       => l_batch_header
                                             ,p_batch_step_rec      => l_batch_step
                                             ,x_return_status       => l_return_status);

                        IF l_return_status <> x_return_status THEN
                           RAISE calc_quantities_error;
                        END IF;          /* IF l_return_status <> x_return_status */

                     END IF;
                END IF;--bUG#5606089
    /* IF ((x_batch_step_rec.step_status > 1) AND (l_batch_step.step_status > 1)) */
            END LOOP;                                      /* FOR  dep_steps*/
         END IF;               /* IF x_batch_step_rec.step_status IN (2, 3) */
      END IF;             /*IF l_batch_header.automatic_step_calculation = 1*/

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;
   EXCEPTION
      WHEN batch_step_fetch_error THEN
         ROLLBACK TO SAVEPOINT update_step_qty;
         x_return_status := fnd_api.g_ret_sts_error;
         fnd_msg_pub.count_and_get (p_count      => x_message_count
                                   ,p_data       => x_message_list);
      WHEN invalid_step_status THEN
         ROLLBACK TO SAVEPOINT update_step_qty;
         x_return_status := fnd_api.g_ret_sts_error;
         gme_common_pvt.log_message ('GME_API_INV_STAT_STEP_EDIT');
         fnd_msg_pub.count_and_get (p_count      => x_message_count
                                   ,p_data       => x_message_list);
      WHEN batch_header_fetch_error THEN
         ROLLBACK TO SAVEPOINT update_step_qty;
         x_return_status := fnd_api.g_ret_sts_error;
         fnd_msg_pub.count_and_get (p_count      => x_message_count
                                   ,p_data       => x_message_list);
      WHEN invalid_batch_status THEN
         ROLLBACK TO SAVEPOINT update_step_qty;
         x_return_status := fnd_api.g_ret_sts_error;
         gme_common_pvt.log_message ('GME_API_INV_BATCH_EDIT_STEP');
         fnd_msg_pub.count_and_get (p_count      => x_message_count
                                   ,p_data       => x_message_list);
      WHEN auto_step_calc_error THEN
         ROLLBACK TO SAVEPOINT update_step_qty;
         x_return_status := l_return_status;
         fnd_msg_pub.count_and_get (p_count      => x_message_count
                                   ,p_data       => x_message_list);
      WHEN calc_quantities_error THEN
         ROLLBACK TO SAVEPOINT update_step_qty;
         x_return_status := l_return_status;
         fnd_msg_pub.count_and_get (p_count      => x_message_count
                                   ,p_data       => x_message_list);
      WHEN OTHERS THEN
         IF g_debug <= gme_debug.g_log_unexpected THEN
            gme_debug.put_line (   'When others exception in '
                                || g_pkg_name
                                || '.'
                                || l_api_name
                                || ' Error is '
                                || SQLERRM);
         END IF;

         ROLLBACK TO SAVEPOINT edit_step_qty;
         x_return_status := fnd_api.g_ret_sts_unexp_error;
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
         fnd_msg_pub.count_and_get (p_count      => x_message_count
                                   ,p_data       => x_message_list);
   END update_step_qty;

/*===========================================================================================
Procedure
  calculate_mass_vol_qty
Description
  This particular procedure converts either the plan step qty or the actual step qty based
  on the step status to its mass or volume quantities based on the step max capacity uom type.
Parameters
  p_batch_step_rec         The batch step row to identify the step.
=============================================================================================*/
   PROCEDURE calculate_mass_vol_qty (
      p_batch_step_rec   IN OUT NOCOPY   gme_batch_steps%ROWTYPE)
   IS
      /* Cursor definition */
      CURSOR cur_get_uom
      IS
         SELECT uom_class, conversion_rate
           FROM mtl_uom_conversions
          WHERE uom_code = p_batch_step_rec.step_qty_um
            AND inventory_item_id = 0;

      /* Local Variables */
      l_um_type             mtl_uom_conversions.uom_class%TYPE;
      l_std_factor          NUMBER;
      l_api_name   CONSTANT VARCHAR2 (30)          := 'calculate_mass_vol_qty';
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      OPEN cur_get_uom;

      FETCH cur_get_uom
       INTO l_um_type, l_std_factor;

      CLOSE cur_get_uom;

      IF l_um_type = gme_common_pvt.g_mass_um_type THEN
         /* If step status is not in pending then actual quantities have to */
         /* be updated                                                      */
         IF p_batch_step_rec.step_status > gme_common_pvt.g_step_pending THEN
            p_batch_step_rec.actual_mass_qty :=
                     NVL (p_batch_step_rec.actual_step_qty * l_std_factor, 0);
         ELSE
            p_batch_step_rec.plan_mass_qty :=
                       NVL (p_batch_step_rec.plan_step_qty * l_std_factor, 0);
         END IF;                     /* IF p_batch_step_rec.step_status > 1 */
      ELSIF l_um_type = gme_common_pvt.g_volume_um_type THEN
         /* If step status is not in pending then actual quantities have to */
         /* be updated                                                      */
         IF p_batch_step_rec.step_status > gme_common_pvt.g_step_pending THEN
            p_batch_step_rec.actual_volume_qty :=
                     NVL (p_batch_step_rec.actual_step_qty * l_std_factor, 0);
         ELSE
            p_batch_step_rec.plan_volume_qty :=
                       NVL (p_batch_step_rec.plan_step_qty * l_std_factor, 0);
         END IF;                     /* IF p_batch_step_rec.step_status > 1 */
      END IF;   /* IF l_um_type = gme_common_pvt.g_mass_um_type */

      IF g_debug <= gme_debug.g_log_procedure THEN
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
   END calculate_mass_vol_qty;

/*===========================================================================================
Procedure
  calculate_quantities
Description
  This particular procedure is used to calculate the resource quantities.
Parameters
  p_batch_step_rec         The batch step row to identify the step.
  x_return_status    outcome of the API call
            S - Success
            E - Error
            U - Unexpected error
HISTORY            
  G. Muratore   19-MAR-2010  Bug 10051993     
     Pass in the new step qty to calc_char so that charges are calculated properly.            
=============================================================================================*/
   PROCEDURE calculate_quantities (
      p_batch_hdr_rec          IN              gme_batch_header%ROWTYPE
     ,p_batch_step_rec         IN OUT NOCOPY   gme_batch_steps%ROWTYPE
     ,x_return_status          OUT NOCOPY      VARCHAR2
     ,p_routing_scale_factor   IN              NUMBER DEFAULT NULL
     ,p_backflush_factor       IN              NUMBER DEFAULT NULL
     ,p_dependency_type        IN              NUMBER DEFAULT NULL)
   IS
      l_api_name      CONSTANT VARCHAR2 (30) := 'calculate__quantities';
      /* Exception definitions */
      compute_resource_error   EXCEPTION;
      batch_step_upd_err       EXCEPTION;
      batch_step_fetch_err     EXCEPTION;
      error_calc_charge        EXCEPTION;
      /* Local variables */
      l_return_status          VARCHAR2 (1);
      l_charges                NUMBER;
      l_orig_charges           NUMBER;
      l_charge_diff            NUMBER;
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      /* Set the return status to success initially */
      x_return_status := fnd_api.g_ret_sts_success;
      
      -- Note the charge_diff value is only used on actuals in update activities.
      -- If someday it is required for pending steps this value will need to be inside the if condition.
      /* Bug 3545726 store original charges */
      l_orig_charges := NVL (p_batch_step_rec.actual_charges, 0);

      -- Bug 10051993 Let's pass in the new step qty.
      IF p_batch_step_rec.step_status > gme_common_pvt.g_step_pending THEN
         calc_charge (p_step_id            => p_batch_step_rec.batchstep_id
                     ,p_mass_qty           => p_batch_step_rec.actual_mass_qty
                     ,p_vol_qty            => p_batch_step_rec.actual_volume_qty
                     ,x_charge             => l_charges
                     ,p_step_qty           => p_batch_step_rec.actual_step_qty -- 10051993
                     ,x_return_status      => l_return_status);

         IF l_return_status <> x_return_status THEN
            RAISE error_calc_charge;
         ELSE
            p_batch_step_rec.actual_charges := l_charges;
         END IF;
      ELSIF p_batch_step_rec.step_status = gme_common_pvt.g_step_pending THEN
         calc_charge (p_step_id            => p_batch_step_rec.batchstep_id
                     ,p_mass_qty           => p_batch_step_rec.plan_mass_qty
                     ,p_vol_qty            => p_batch_step_rec.plan_volume_qty
                     ,p_step_qty           => p_batch_step_rec.plan_step_qty -- 10051993
                     ,x_charge             => l_charges
                     ,x_return_status      => l_return_status);

         IF l_return_status <> x_return_status THEN
            RAISE error_calc_charge;
         ELSE
            p_batch_step_rec.plan_charges := l_charges;
            p_batch_step_rec.actual_charges := NULL;
         END IF;
      END IF;

      /* Now we have to compute the resource usages and quantities */
      l_charge_diff :=
                      NVL (p_batch_step_rec.actual_charges, 0)
                      - l_orig_charges;
                      
                      
      update_activities (p_batch_hdr_rec             => p_batch_hdr_rec
                        ,p_batch_step_rec            => p_batch_step_rec
                        ,x_return_status             => l_return_status
                        ,p_routing_scale_factor      => p_routing_scale_factor
                        ,p_backflush_factor          => p_backflush_factor
                        ,p_charge_diff               => l_charge_diff
                        ,p_dependency_type           => p_dependency_type);

      IF l_return_status <> x_return_status THEN
         RAISE compute_resource_error;
      END IF;

      /* Now update the batch step to the database */
      IF NOT (gme_batch_steps_dbl.update_row (p_batch_step_rec) ) THEN
         RAISE batch_step_upd_err;
      END IF;

      IF NOT (gme_batch_steps_dbl.fetch_row (p_batch_step_rec
                                            ,p_batch_step_rec) ) THEN
         RAISE batch_step_fetch_err;
      END IF;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;
   EXCEPTION
      WHEN compute_resource_error THEN
         x_return_status := l_return_status;
      WHEN batch_step_upd_err OR error_calc_charge OR batch_step_fetch_err THEN
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

         x_return_status := fnd_api.g_ret_sts_unexp_error;
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
   END calculate_quantities;

/*===========================================================================================
Procedure
  calc_charge
Description
  This particular procedure is used to calculate the charges for the step.
Parameters
  p_step_id       The batch step id to identify the step.
  p_mass_qty         Step qty in mass uom type
  p_vol_qty       Step qty in volume uom type
  x_charge        Calculated charge returned to the calling program
  x_return_status    outcome of the API call
            S - Success
            E - Error
            U - Unexpected error
 History
 
   G. Muratore   27-JUN-2011  Bug 12568872     
      Initialize charge to 1 if necessary. This usually happens when step qty is less than capacity.
   =============================================================================================*/
   PROCEDURE calc_charge (
      p_step_id         IN              gme_batch_steps.batchstep_id%TYPE
     ,p_resources       IN              gme_batch_step_resources.resources%TYPE
            DEFAULT NULL
     ,p_mass_qty        IN              gme_batch_steps.plan_mass_qty%TYPE
     ,p_vol_qty         IN              gme_batch_steps.plan_volume_qty%TYPE
     ,p_step_qty        IN              NUMBER DEFAULT NULL      --Bug#5231180
     ,p_max_capacity    IN              NUMBER DEFAULT NULL     --Bug#5231180
     ,x_charge          OUT NOCOPY      gme_batch_steps.plan_charges%TYPE
     ,x_return_status   OUT NOCOPY      VARCHAR2)
   IS
      x_step_no                    gme_batch_steps.batchstep_no%TYPE;
      x_std_factor                 NUMBER;
      x_um_type                    mtl_units_of_measure.uom_class%TYPE;
      x_std_capacity               gme_batch_steps.max_step_capacity%TYPE;
      x_max_cap                    gme_batch_steps.max_step_capacity%TYPE;
      x_max_cap_uom                gme_batch_steps.max_step_capacity_um%TYPE;
      x_step_qty                   gme_batch_steps.plan_step_qty%TYPE;
      x_step_qty_uom               gme_batch_steps.step_qty_um%TYPE;
      x_temp_qty                   NUMBER;
      x_tolerance                  NUMBER;
      x_stdum_tolerance            NUMBER;
      pmass_qty_std                NUMBER;
      l_api_name          CONSTANT VARCHAR2 (30)             := 'calc_charge';

      -- Cursor Definitions
      CURSOR cur_get_max_capacity
      IS
         SELECT max_step_capacity, max_step_capacity_um, batchstep_no
               ,plan_step_qty, step_qty_um
           FROM gme_batch_steps
          WHERE batchstep_id = p_step_id;

      CURSOR cur_get_std_factor (v_um_code VARCHAR2)
      IS
         SELECT uom_class, conversion_rate
           FROM mtl_uom_conversions
          WHERE uom_code = v_um_code AND inventory_item_id = 0;

      CURSOR cur_get_tolerance
      IS
         SELECT NVL (MIN (gbsr.capacity_tolerance), 0)
           FROM gme_batch_steps gbs, gme_batch_step_resources gbsr
          WHERE gbs.batchstep_id = gbsr.batchstep_id
            AND gbsr.calculate_charges = 1
            AND gbsr.batchstep_id = p_step_id;

      CURSOR cur_get_resource_dates (
         v_batchstep_id   NUMBER
        ,v_resources      gme_batch_step_resources.resources%TYPE)
      IS
         SELECT plan_start_date, plan_cmplt_date
           FROM gme_batch_step_resources
          WHERE batchstep_id = v_batchstep_id AND resources = v_resources;

      CURSOR cur_get_batch_id (v_batchstep_id NUMBER)
      IS
         SELECT batch_id
           FROM gme_batch_steps
          WHERE batchstep_id = v_batchstep_id;

      CURSOR cur_get_scale_type (
         v_resources   gme_batch_step_resources.resources%TYPE)
      IS
         SELECT 1
           FROM DUAL
          WHERE EXISTS (
                   SELECT 1
                     FROM gme_batch_step_resources
                    WHERE batchstep_id = p_step_id
                      AND resources = v_resources
                      AND scale_type = 2);

      l_scale_type                 gme_batch_step_resources.scale_type%TYPE;
      l_remaining_quantity         NUMBER;
      l_batch_step_charges_in      gme_batch_step_charges%ROWTYPE;
      l_return_status              VARCHAR2 (1);
      l_charge                     gme_batch_steps.plan_charges%TYPE;
      l_batch_id                   NUMBER;
      l_rsrc_plan_start_date       DATE;
      l_rsrc_plan_cmplt_date       DATE;
      error_charge_insert          EXCEPTION;
      error_step_not_found         EXCEPTION;
      error_calc_charge_conv       EXCEPTION;
      error_max_cap_not_mass_vol   EXCEPTION;
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;      

      /* Initialize the return status to success */
      x_return_status := fnd_api.g_ret_sts_success;
      x_charge := 0;
      l_remaining_quantity := -1;
      
      IF NVL (g_debug, -1) = gme_debug.g_log_statement THEN
         gme_debug.put_line('pmassqty: '||p_mass_qty);
         gme_debug.put_line('pvolqty: '||p_vol_qty);
      END IF;

      /* Get the max capacity and uom for the current step */
      OPEN cur_get_max_capacity;

      FETCH cur_get_max_capacity
       INTO x_max_cap, x_max_cap_uom, x_step_no, x_step_qty, x_step_qty_uom;

      IF cur_get_max_capacity%NOTFOUND THEN
         CLOSE cur_get_max_capacity;      
         RAISE error_step_not_found;
      END IF;
      CLOSE cur_get_max_capacity;      
      
      IF NVL (g_debug, -1) = gme_debug.g_log_statement THEN
         gme_debug.put_line('Max capacity, UOM: '||x_max_cap||', '||x_max_cap_uom);
         gme_debug.put_line('Step Qty, UOM: '||x_step_qty||', '||x_step_qty_uom);
      END IF;      

      /* Bug#5231180 Begin if we pass step qty and max capacity use them otherwise get the values
         from table. these will be passed from recalculate charges procedure */
      x_step_qty := NVL(p_step_qty,x_step_qty);
      x_max_cap  := NVL(p_max_capacity,x_max_cap);

      --added zero step qty check
      IF nvl(x_step_qty ,0) = 0 THEN
         x_charge := 0;
         RETURN;
      END IF; 
     --Bug#5231180 End

      /*Defaulting x_charge to 1 if max_cap returned by cursor is null*/
      IF x_max_cap IS NULL THEN 
         x_charge := 1;
         RETURN;
      END IF;
      
      IF x_max_cap_uom IS NOT NULL THEN
         /* Get the standard factor and uom type from the capacity uom */
         OPEN cur_get_std_factor (x_max_cap_uom);

         FETCH cur_get_std_factor
          INTO x_um_type, x_std_factor;

         CLOSE cur_get_std_factor;

         x_std_capacity := x_max_cap * x_std_factor;

         IF NVL (g_debug, -1) = gme_debug.g_log_statement THEN
	    gme_debug.put_line('x_um_type: '||x_um_type);
	    gme_debug.put_line('Standard Factor: '||x_std_factor);
	    gme_debug.put_line('Standard Capacity: '||x_std_capacity);
	 END IF;

         --Bug2617151
         OPEN cur_get_tolerance;

         FETCH cur_get_tolerance
          INTO x_tolerance;

         CLOSE cur_get_tolerance;

         x_stdum_tolerance := (x_tolerance * x_std_capacity) / 100;   
              
         IF NVL (g_debug, -1) = gme_debug.g_log_statement THEN
	    gme_debug.put_line('x_tolerance: '||x_tolerance);
	    gme_debug.put_line('x_stdum_tolerance: '||x_stdum_tolerance);
	    gme_debug.put_line('gme_common_pvt.g_mass_um_type: '||gme_common_pvt.g_mass_um_type);
	    gme_debug.put_line('gme_common_pvt.g_volume_um_type: '||gme_common_pvt.g_volume_um_type);
	 END IF;
	 
         -- Capacity will be in either mass or volume...
         IF x_um_type = gme_common_pvt.g_mass_um_type THEN         
            IF NVL (g_debug, -1) = gme_debug.g_log_statement THEN            
	       gme_debug.put_line('This is MASS type.');
	    END IF;
	    
            IF (p_mass_qty IS NULL) THEN
               RAISE error_calc_charge_conv;
            END IF;

            IF NVL (g_debug, -1) = gme_debug.g_log_statement THEN
	       gme_debug.put_line('qty is: '||MOD (p_mass_qty, x_std_capacity));
	    END IF;

            IF (MOD (p_mass_qty, x_std_capacity) > x_stdum_tolerance) THEN
               IF NVL (g_debug, -1) = gme_debug.g_log_statement THEN            
	          gme_debug.put_line('MASS point 1A');
	       END IF;
            
               x_charge := CEIL (p_mass_qty / x_std_capacity);
               l_remaining_quantity := MOD (p_mass_qty, x_std_capacity);               
            ELSE
               IF NVL (g_debug, -1) = gme_debug.g_log_statement THEN            
	          gme_debug.put_line('MASS point 1B');
	       END IF;
               x_charge := TRUNC (p_mass_qty / x_std_capacity);
               
               -- Bug 12568872 - initialize charge to 1 if necessary.
               IF x_charge = 0 THEN
                  x_charge := 1;
               END IF;
            END IF;
            
            IF NVL (g_debug, -1) = gme_debug.g_log_statement THEN
	       gme_debug.put_line('x_charge is: '||x_charge);
	       gme_debug.put_line('l_remaining_quantity is: '||l_remaining_quantity);
	    END IF;

         ELSIF x_um_type = gme_common_pvt.g_volume_um_type THEN

            IF NVL (g_debug, -1) = gme_debug.g_log_statement THEN
	       gme_debug.put_line('This is VOLUME type.');
	    END IF;
         
            IF (p_vol_qty IS NULL) THEN
               RAISE error_calc_charge_conv;
            END IF;
            
            IF NVL (g_debug, -1) = gme_debug.g_log_statement THEN
	       gme_debug.put_line('qty is: '||MOD (p_vol_qty, x_std_capacity));
	    END IF;

            IF (MOD (p_vol_qty, x_std_capacity) > x_stdum_tolerance) THEN
               IF NVL (g_debug, -1) = gme_debug.g_log_statement THEN            
	          gme_debug.put_line('VOLUME point 1A');
	       END IF;
               x_charge := CEIL (p_vol_qty / x_std_capacity);
               l_remaining_quantity := MOD (p_vol_qty, x_std_capacity);
            ELSE
               IF NVL (g_debug, -1) = gme_debug.g_log_statement THEN            
	          gme_debug.put_line('VOLUME point 1B');
	       END IF;
               x_charge := TRUNC (p_vol_qty / x_std_capacity);

               -- Bug 12568872 - initialize charge to 1 if necessary.               
               IF x_charge = 0 THEN
                  x_charge := 1;
               END IF;
            END IF;

            IF NVL (g_debug, -1) = gme_debug.g_log_statement THEN
	       gme_debug.put_line('x_charge is: '||x_charge);
	       gme_debug.put_line('l_remaining_quantity is: '||l_remaining_quantity);
	    END IF;

         ELSE
            IF NVL (g_debug, -1) = gme_debug.g_log_statement THEN            
	       gme_debug.put_line('OTHER UOM TYPE');
	    END IF;
         
            x_temp_qty :=
               inv_convert.inv_um_convert
                                    (item_id            => 0
                                    ,PRECISION          => gme_common_pvt.g_precision
                                    ,from_quantity      => x_step_qty
                                    ,from_unit          => x_step_qty_uom
                                    ,to_unit            => x_max_cap_uom
                                    ,from_name          => NULL
                                    ,to_name            => NULL);

            IF (x_temp_qty < 0) THEN
               RAISE error_calc_charge_conv;
            ELSE
  	       /* Bug#5231180 Begin when we come here x_temp_qty is in the same uom as x_max_cap. x_tolerance will be in %
	          so now we have to calculate the % of x_max_cap like we did in the above case. replaced x_tolerance
		  with calculated x_stdum_tolerance in the following IF condition*/
	       x_stdum_tolerance := (x_tolerance * x_max_cap) / 100;
               --calculate the factor
	       x_std_factor := x_temp_qty / x_step_qty ;
               IF (MOD (x_temp_qty, x_max_cap) > x_stdum_tolerance) THEN
	          --Bug#5231180 End
                  x_charge := CEIL (x_temp_qty / x_max_cap);
                  --Rishi Varma bug 3307549 05/05/2004.
                  l_remaining_quantity := MOD (x_temp_qty, x_max_cap);
               ELSE
                  x_charge := TRUNC (x_temp_qty / x_max_cap);

                  -- Bug 12568872 - initialize charge to 1 if necessary.                  
                  IF x_charge = 0 THEN
                     x_charge := 1;
                  END IF;
               END IF;
            END IF;  /* if (x_temp_qty < 0) then  */
            
            IF NVL (g_debug, -1) = gme_debug.g_log_statement THEN
	       gme_debug.put_line('x_charge is: '||x_charge);
	       gme_debug.put_line('l_remaining_quantity is: '||l_remaining_quantity);
	    END IF;            
         END IF;
      END IF;   /*if x_max_cap_uom is not null then */

      /*Checking the scale type of the resource before
      populating the charges table*/
      IF NVL (g_debug, -1) = gme_debug.g_log_statement THEN
         gme_debug.put_line ('p_resources is' || p_resources);
      END IF;      

      IF p_resources IS NOT NULL THEN
         OPEN cur_get_scale_type (p_resources);	 
         FETCH cur_get_scale_type
          INTO l_scale_type;

         CLOSE cur_get_scale_type;

         IF NVL (g_debug, -1) = gme_debug.g_log_statement THEN
            gme_debug.put_line ('l_scale_type is' || l_scale_type);
         END IF;

         IF l_scale_type = 1 THEN
            --Generating the charge details and populating the gme_batch_step_charges table.
            l_charge := x_charge;

            OPEN cur_get_batch_id (p_step_id);

            FETCH cur_get_batch_id
             INTO l_batch_id;

            CLOSE cur_get_batch_id;

            --Converting back the remaining quantity to the step uom.
	    /* Bug#5231180 calculate remaining qty back to step uom. if l_remaining_qty is -1 then
	       there is not remaining qty in the calculation of charges */
            IF x_std_factor <> 1 AND l_remaining_quantity <> -1 THEN
               l_remaining_quantity := (l_remaining_quantity / x_std_factor);
            END IF;

            IF NVL (g_debug, -1) = gme_debug.g_log_statement THEN
               gme_debug.put_line (   'l_remaining_quantity is'
                                   || l_remaining_quantity);
            END IF;

            l_batch_step_charges_in.batch_id := l_batch_id;
            l_batch_step_charges_in.batchstep_id := p_step_id;
            l_batch_step_charges_in.resources := p_resources;
            l_batch_step_charges_in.charge_quantity := x_max_cap;	   
            l_batch_step_charges_in.plan_start_date := l_rsrc_plan_start_date;
            l_batch_step_charges_in.plan_cmplt_date := l_rsrc_plan_cmplt_date;

            --Rishi Varma B3718176 23-07-2004
            gme_batch_step_chg_pvt.populate_charges_table
                           (p_batchstep_charges_in      => l_batch_step_charges_in
                           ,p_no_of_charges             => l_charge
                           ,p_remaining_quantity        => l_remaining_quantity
                           ,x_return_status             => l_return_status);

            IF l_return_status <> fnd_api.g_ret_sts_success THEN
               RAISE error_charge_insert;
            END IF;
         END IF;                                       /*if l_scale_type = 1*/
      END IF;                                    /*if p_resource is not null*/

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;
   EXCEPTION
      WHEN error_step_not_found THEN
         x_return_status := fnd_api.g_ret_sts_error;
         gme_common_pvt.log_message ('GME_BATCH_STEP_NOT_FOUND'
                                    ,'STEP_ID'
                                    ,p_step_id);
      WHEN error_calc_charge_conv THEN
         -- x_return_status := FND_API.G_RET_STS_ERROR;
         gme_common_pvt.log_message ('GME_CALC_CHARGE_CONV_ERROR'
                                    ,'STEP_NO'
                                    ,x_step_no);
      WHEN error_max_cap_not_mass_vol THEN
         gme_common_pvt.log_message
                                 ('GME_MAX_CAP_NOT_MASS_VOL_ERR'
                                 ,'STEP_NO'
                                 ,x_step_no
                                 ,'MASS'
                                 ,gme_common_pvt.g_mass_um_type
                                 ,'VOLUME'
                                 ,gme_common_pvt.g_volume_um_type);
      WHEN error_charge_insert THEN
         gme_common_pvt.log_message ('GME_INSERT_CHARGE_ERROR'
                                    ,'STEP_NO'
                                    ,x_step_no);
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
   END calc_charge;

/*===========================================================================================
Procedure
  update_activities
Description
  This particular procedure is used to update the activities associated with a batch step
Parameters
  p_batch_hdr_rec       Batch Header Record
  p_batch_step_rec         Batch Step Line
  x_return_status    outcome of the API call
            S - Success
            E - Error
            U - Unexpected error
=============================================================================================*/
   PROCEDURE update_activities (
      p_batch_hdr_rec          IN              gme_batch_header%ROWTYPE
     ,p_batch_step_rec         IN              gme_batch_steps%ROWTYPE
     ,x_return_status          OUT NOCOPY      VARCHAR2
     ,p_routing_scale_factor   IN              NUMBER DEFAULT NULL
     ,p_backflush_factor       IN              NUMBER DEFAULT NULL
     ,p_charge_diff            IN              NUMBER
     ,p_dependency_type        IN              NUMBER DEFAULT NULL)
   IS
      l_api_name              CONSTANT VARCHAR2 (30)   := 'update_activities';
      /* Collections for details etc */
      l_batchstep_activity_ids         gme_common_pvt.number_tab;
      l_gme_batchstep_activities       gme_batch_step_activities%ROWTYPE;
      l_gme_calc_int_rsrc_usage_char   VARCHAR2 (10);
      l_user_profile_option_name       VARCHAR2 (100);
      /* Local variables */
      l_return_status                  VARCHAR2 (1);

      CURSOR cur_step_activity_ids
      IS
         SELECT   batchstep_activity_id
             FROM gme_batch_step_activities
            WHERE batch_id = p_batch_step_rec.batch_id
              AND batchstep_id = p_batch_step_rec.batchstep_id
         ORDER BY batchstep_id;

      /* Exceptions */
      step_activity_fetch_error        EXCEPTION;
      step_resource_upd_err            EXCEPTION;
      step_activity_upd_err            EXCEPTION;
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      /* Initialize the return status to success */
      x_return_status := fnd_api.g_ret_sts_success;

      /* Fetch all the activities associated with the batch step */
      OPEN cur_step_activity_ids;

      FETCH cur_step_activity_ids
      BULK COLLECT INTO l_batchstep_activity_ids;

      CLOSE cur_step_activity_ids;

      /* Fetch all the batch step activities details */
      FOR i IN 1 .. l_batchstep_activity_ids.COUNT LOOP
         l_gme_batchstep_activities.batchstep_activity_id :=
                                                 l_batchstep_activity_ids (i);

         IF NOT (gme_batch_step_activities_dbl.fetch_row
                                                  (l_gme_batchstep_activities
                                                  ,l_gme_batchstep_activities) ) THEN
            RAISE step_activity_fetch_error;
         END IF;

         /* If automatic step qty calculation is set for the batch then */
         /* get the profile value for GME_CALC_INT_RSRC_USAGE to determine */
         /* whether activity factor and later on resource qty and usage */
         /* should be calculated for WIP step in ASQC batch             */
         IF p_batch_hdr_rec.automatic_step_calculation = 1 THEN
            IF (p_gme_calc_int_rsrc_usage IS NULL) THEN
               l_gme_calc_int_rsrc_usage_char := NULL;
               l_gme_calc_int_rsrc_usage_char :=
                                   gme_common_pvt.g_calc_interim_rsrc_usg_ind;
               p_gme_calc_int_rsrc_usage := l_gme_calc_int_rsrc_usage_char;
               l_gme_calc_int_rsrc_usage_char := NULL;
            END IF;
         ELSE
            p_gme_calc_int_rsrc_usage := NULL;
         END IF;

         IF (     (    (        (p_batch_step_rec.step_status =
                                                     gme_common_pvt.g_step_wip)
                           AND (NVL (p_gme_calc_int_rsrc_usage, 0) = 1)
                        OR NVL (gme_common_pvt.g_backflush_rsrc_usg_ind, 0) =
                                                                             1)
                   OR (p_batch_step_rec.step_status >
                                                     gme_common_pvt.g_step_wip) )
             AND (NVL (l_gme_batchstep_activities.actual_activity_factor, 0) =
                                                                             0) ) THEN
            l_gme_batchstep_activities.actual_activity_factor :=
                              l_gme_batchstep_activities.plan_activity_factor;
         ELSIF (p_batch_step_rec.step_status = gme_common_pvt.g_step_pending) THEN
            l_gme_batchstep_activities.actual_activity_factor := NULL;
         END IF;

         /* Let us update all the resources attached to the activity */
         gme_update_step_qty_pvt.update_resources
                    (p_batch_hdr_rec                 => p_batch_hdr_rec
                    ,p_batch_step_rec                => p_batch_step_rec
                    ,p_batchstep_activities_rec      => l_gme_batchstep_activities
                    ,x_return_status                 => l_return_status
                    ,p_routing_scale_factor          => p_routing_scale_factor
                    ,p_backflush_factor              => p_backflush_factor
                    ,p_charge_diff                   => p_charge_diff
                    ,p_dependency_type               => p_dependency_type);

         IF l_return_status <> x_return_status THEN
            RAISE step_resource_upd_err;
         END IF;

         /* Save the updated batch step activities row to the database */
         IF NOT (gme_batch_step_activities_dbl.update_row
                                                   (l_gme_batchstep_activities) ) THEN
            RAISE step_activity_upd_err;
         END IF;
      END LOOP;          /* FOR i IN 1..l_batchstep_activity_ids.COUNT LOOP */

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;
   EXCEPTION
      WHEN step_activity_fetch_error THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN step_resource_upd_err THEN
         x_return_status := l_return_status;
      WHEN step_activity_upd_err THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN VALUE_ERROR THEN
         x_return_status := fnd_api.g_ret_sts_error;

         IF l_gme_calc_int_rsrc_usage_char IS NOT NULL THEN
         	l_user_profile_option_name := gme_common_pvt.g_calc_interim_rsrc_usg_ind;
          /*  SELECT user_profile_option_name
              INTO l_user_profile_option_name
              FROM fnd_profile_options_vl
             WHERE application_id = 553
               AND profile_option_name = 'GME_CALC_INT_RSRC_USAGE';*/

            gme_common_pvt.log_message ('GME_INVALID_VALUE_PROFILE'
                                       ,'VALUE'
                                       ,l_gme_calc_int_rsrc_usage_char
                                       ,'PROFILE'
                                       ,l_user_profile_option_name);
         ELSE
            fnd_msg_pub.add_exc_msg ('gme_update_step_qty_pvt'
                                    ,'UPDATE_ACTIVITIES');
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

         x_return_status := fnd_api.g_ret_sts_unexp_error;
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
   END update_activities;

/*===========================================================================================
Procedure
  update_resources
Description
  This particular procedure is used to update the resources associated with a activity
Parameters
  p_batch_hdr_rec       Batch Header Record
  p_batch_step_rec         Batch Step Line
  p_batchstep_activities_rec  Batch Step Activity Line
  x_return_status    outcome of the API call
            S - Success
            E - Error
            U - Unexpected error
 History
=============================================================================================*/
   PROCEDURE update_resources (
      p_batch_hdr_rec              IN              gme_batch_header%ROWTYPE
     ,p_batch_step_rec             IN              gme_batch_steps%ROWTYPE
     ,p_batchstep_activities_rec   IN              gme_batch_step_activities%ROWTYPE
     ,x_return_status              OUT NOCOPY      VARCHAR2
     ,p_routing_scale_factor       IN              NUMBER DEFAULT NULL
     ,p_backflush_factor           IN              NUMBER DEFAULT NULL
     ,p_charge_diff                IN              NUMBER DEFAULT NULL
     ,p_dependency_type            IN              NUMBER DEFAULT NULL)
   IS
      l_api_name             CONSTANT VARCHAR2 (30)     := 'update_resources';
      /* Collections for details etc */
      l_batchstep_resource_ids        gme_common_pvt.number_tab;
      l_gme_batchstep_resources       gme_batch_step_resources%ROWTYPE;
      l_resource_txns                 gme_resource_txns_gtmp%ROWTYPE;
      l_resource_tab                  gme_common_pvt.resource_transactions_tab;
      /* Local variables */
      l_sum_comp_usage                NUMBER;
      l_override_usage                NUMBER;
      l_actual_usage                  NUMBER;
      l_usage                         NUMBER;
      l_process_qty                   NUMBER;
      l_resource_usage                NUMBER;
      l_alloc_usage                   NUMBER;
      l_prev_plan_actv_fact           NUMBER;
      l_prev_actual_actv_fact         NUMBER;
      l_prev_plan_charges             NUMBER;
      l_prev_plan_step_qty            NUMBER;
      l_return_status                 VARCHAR2 (1);
      l_allow_qty_below_cap           NUMBER (5);
      l_copy_plan_to_actual           NUMBER (5);
      /* Bug 2685645 added variable */
      l_doc_type                      VARCHAR2 (4);
      l_actual_resource_usage         NUMBER;
      /*ER 19161894  Shaliu Chen 18-JUL-2014*/
      l_osp_batch_flag                BOOLEAN;
      l_res_txn_exist                 BOOLEAN;
      l_cur_gme_batchstep_resources   gme_batch_step_resources%ROWTYPE;

      CURSOR cur_step_resource_ids (v_batchstep_activity_id NUMBER)
      IS
         SELECT batchstep_resource_id
           FROM gme_batch_step_resources
          WHERE batchstep_activity_id = v_batchstep_activity_id;

      CURSOR cur_sum_override_resource (
         v_batchstep_resource_id   NUMBER
        ,v_doc_type                VARCHAR2
        ,v_doc_id                  NUMBER)
      IS
         SELECT SUM (resource_usage)
           FROM gme_resource_txns
          WHERE line_id = v_batchstep_resource_id
            AND doc_type = v_doc_type
            AND doc_id = v_doc_id
            AND completed_ind = 1
            AND overrided_protected_ind = 'Y';

      CURSOR cur_prev_actv_fact (v_batchstep_activity_id NUMBER)
      IS
         SELECT plan_activity_factor, actual_activity_factor
           FROM gme_batch_step_activities
          WHERE batchstep_activity_id = v_batchstep_activity_id;

      CURSOR cur_prev_plan_charge
      IS
         SELECT plan_charges
           FROM gme_batch_steps
          WHERE batchstep_id = p_batch_step_rec.batchstep_id;

      /* Exceptions */
      activity_resource_fetch_error   EXCEPTION;
      resource_trans_ins_err          EXCEPTION;
      step_resource_upd_err           EXCEPTION;
      error_txns_update               EXCEPTION;
      error_fetch_rsrc_tran           EXCEPTION;
      qty_below_cap_error             EXCEPTION;
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      /* Initialize the return status to success */
      x_return_status := fnd_api.g_ret_sts_success;

      IF (p_batch_hdr_rec.batch_type = 0) THEN
         l_doc_type := 'PROD';
      ELSE
         l_doc_type := 'FPO';
      END IF;

      l_allow_qty_below_cap :=
                             NVL (gme_common_pvt.g_allow_qty_below_min_ind, 1);
      l_copy_plan_to_actual :=
                             NVL (gme_common_pvt.g_def_actual_rsrc_usg_ind, 1);

      /* Fetch all the resources associated with the activity */
      OPEN cur_step_resource_ids
                             (p_batchstep_activities_rec.batchstep_activity_id);

      FETCH cur_step_resource_ids
      BULK COLLECT INTO l_batchstep_resource_ids;

      CLOSE cur_step_resource_ids;

      /* Fetch all the activity resources details */
      FOR i IN 1 .. l_batchstep_resource_ids.COUNT LOOP
         l_gme_batchstep_resources.batchstep_resource_id :=
                                                 l_batchstep_resource_ids (i);

         IF NOT (gme_batch_step_resources_dbl.fetch_row
                                                   (l_gme_batchstep_resources
                                                   ,l_gme_batchstep_resources) ) THEN
            RAISE activity_resource_fetch_error;
         END IF;

         IF (l_gme_batchstep_resources.actual_start_date IS NULL) THEN
            l_gme_batchstep_resources.actual_start_date :=
                                           p_batch_step_rec.actual_start_date;
         END IF;

         /* Get the Operation resource qty and usage for the calculations */
         /* First check for any overriden process qty or resource usage   */
         /* at the recipe level                                           */
         l_process_qty := l_gme_batchstep_resources.original_rsrc_qty;
         l_resource_usage := l_gme_batchstep_resources.original_rsrc_usage;

         /* If automatic step qty calculation is set for the batch then */
         IF p_batch_hdr_rec.automatic_step_calculation = 1 THEN
            /* If the step  status is greater than pending then  we have to calculate the actual quantities
            calculate resource qty and usage for WIP asqc batch if profile is set */
            IF (    (     (p_batch_step_rec.step_status =
                                                     gme_common_pvt.g_step_wip)
                     AND (NVL (p_gme_calc_int_rsrc_usage, 0) = 1) )
                OR (p_batch_step_rec.step_status > gme_common_pvt.g_step_wip) ) THEN
               IF l_process_qty <> 0 THEN
                  l_gme_batchstep_resources.actual_rsrc_qty :=
                       p_batch_step_rec.actual_step_qty
                     * p_batchstep_activities_rec.actual_activity_factor;
               ELSE
                  l_gme_batchstep_resources.actual_rsrc_qty := l_process_qty;
               END IF;

               /* If the resource scale type is linear then */
               IF l_gme_batchstep_resources.scale_type = 1 THEN
                  IF l_process_qty = 0 THEN
                     l_actual_resource_usage := 0;
                  ELSE
                     l_actual_resource_usage :=
                          (   (p_batch_step_rec.actual_step_qty
                               / l_process_qty)
                           * l_resource_usage)
                        * p_batchstep_activities_rec.actual_activity_factor;
                  END IF;

                  l_gme_batchstep_resources.actual_rsrc_usage :=
                                                       l_actual_resource_usage;
               /* If the resource scale type is calculate by charges then */
               ELSIF l_gme_batchstep_resources.scale_type = 2 THEN
                  l_gme_batchstep_resources.actual_rsrc_usage :=
                       l_resource_usage
                     * p_batch_step_rec.actual_charges
                     * p_batchstep_activities_rec.actual_activity_factor;
               /* If the resource scale type is fixed then */
               ELSE
                  l_gme_batchstep_resources.actual_rsrc_usage :=
                       l_resource_usage
                     * p_batchstep_activities_rec.actual_activity_factor;
               END IF;         /*IF l_gme_batchstep_resources.scale_type = 1*/

               /*  Default the Actual Resource Count to the Planned Resource  */
               /*  Count if the Actual Resource Count is zero.                */
               IF     (NVL (l_gme_batchstep_resources.actual_rsrc_count, 0) =
                                                                             0)
                  AND (p_batch_step_rec.step_status >
                                                 gme_common_pvt.g_step_pending) THEN
                  l_gme_batchstep_resources.actual_rsrc_count :=
                                    l_gme_batchstep_resources.plan_rsrc_count;
               END IF;

               IF p_batch_step_rec.step_status = gme_common_pvt.g_step_wip THEN
                  l_gme_batchstep_resources.plan_rsrc_count :=
                           NVL (l_gme_batchstep_resources.plan_rsrc_count, 1);
                  l_gme_batchstep_resources.plan_rsrc_usage :=
                           NVL (l_gme_batchstep_resources.plan_rsrc_usage, 0);
                  l_gme_batchstep_resources.plan_rsrc_qty :=
                             NVL (l_gme_batchstep_resources.plan_rsrc_qty, 0);
               END IF;
            /* If the step status is pending then we have to update the plan quantities */
            ELSIF p_batch_step_rec.step_status = gme_common_pvt.g_step_pending THEN
               l_gme_batchstep_resources.plan_rsrc_qty :=
                    p_batch_step_rec.plan_step_qty
                  * p_batchstep_activities_rec.plan_activity_factor;

               /* If the resource scale type is linear then */
               IF l_gme_batchstep_resources.scale_type = 1 THEN
                  l_gme_batchstep_resources.plan_rsrc_usage :=
                       (   (p_batch_step_rec.plan_step_qty / l_process_qty)
                        * l_resource_usage)
                     * p_batchstep_activities_rec.plan_activity_factor;
               /* If the resource scale type is calculate by charges then */
               ELSIF l_gme_batchstep_resources.scale_type = 2 THEN
                  l_gme_batchstep_resources.plan_rsrc_usage :=
                       l_resource_usage
                     * p_batch_step_rec.plan_charges
                     * p_batchstep_activities_rec.plan_activity_factor;
               /* If the resource scale type is fixed then */
               ELSE
                  l_gme_batchstep_resources.plan_rsrc_usage :=
                       l_resource_usage
                     * p_batchstep_activities_rec.plan_activity_factor;
               END IF;         /*IF l_gme_batchstep_resources.scale_type = 1*/

               l_gme_batchstep_resources.actual_rsrc_usage := NULL;
               l_gme_batchstep_resources.actual_rsrc_qty := NULL;
               l_gme_batchstep_resources.actual_rsrc_count := NULL;
            END IF;                    /*IF p_batch_step_rec.step_status > 1*/
         ELSIF (p_batch_step_rec.step_status > gme_common_pvt.g_step_wip) THEN
            OPEN cur_prev_actv_fact
                            (p_batchstep_activities_rec.batchstep_activity_id);

            FETCH cur_prev_actv_fact
             INTO l_prev_plan_actv_fact, l_prev_actual_actv_fact;

            CLOSE cur_prev_actv_fact;

            /* copy plan to actual only if profile is set moved code calculating usage above */
            /* the calculation for process qty since calculation for process qty depends on usage */
            IF (    NVL (l_gme_batchstep_resources.actual_rsrc_usage, 0) = 0
                AND l_copy_plan_to_actual = 1) THEN
               /* If the resource scale type is linear then */
               IF l_gme_batchstep_resources.scale_type = 1 THEN
                  IF p_batchstep_activities_rec.plan_activity_factor > 0 THEN
                     l_gme_batchstep_resources.actual_rsrc_usage :=
                          (  l_gme_batchstep_resources.plan_rsrc_usage
                           / p_batchstep_activities_rec.plan_activity_factor)
                        * p_batchstep_activities_rec.actual_activity_factor;
                  /* BACKING OUTTHE FIX DECSION HAS TO BE MADE HOW TO IMPLEMENT */
                  /* If actual_rsrc_qty exists then calculate usage based on that */
                  /* IF (NVL(l_gme_batchstep_resources.actual_rsrc_qty, 0) > 0) THEN
                    l_gme_batchstep_resources.actual_rsrc_usage := l_gme_batchstep_resources.actual_rsrc_usage *
                                      (l_gme_batchstep_resources.actual_rsrc_qty / l_gme_batchstep_resources.plan_rsrc_qty);
                  END IF; */
                  ELSE
                     l_gme_batchstep_resources.actual_rsrc_usage :=
                          l_gme_batchstep_resources.plan_rsrc_usage
                        * p_batchstep_activities_rec.actual_activity_factor;
                  END IF;
               /* If the resource scale type is calculate by charges then */
               ELSIF l_gme_batchstep_resources.scale_type = 2 THEN
                  IF p_batch_step_rec.plan_charges <> 0 THEN
                     IF p_batchstep_activities_rec.plan_activity_factor > 0 THEN
                        l_gme_batchstep_resources.actual_rsrc_usage :=
                             (  l_gme_batchstep_resources.plan_rsrc_usage
                              / (  p_batchstep_activities_rec.plan_activity_factor
                                 * p_batch_step_rec.plan_charges) )
                           * p_batchstep_activities_rec.actual_activity_factor
                           * p_batch_step_rec.actual_charges;
                     ELSE
                        l_gme_batchstep_resources.actual_rsrc_usage :=
                             (  l_gme_batchstep_resources.plan_rsrc_usage
                              / p_batch_step_rec.plan_charges)
                           * p_batchstep_activities_rec.actual_activity_factor
                           * p_batch_step_rec.actual_charges;
                     END IF;
                  ELSE
                     l_gme_batchstep_resources.actual_rsrc_usage := 0;
                  END IF;
               /* If the resource scale type is fixed then */
               ELSE
                  l_gme_batchstep_resources.actual_rsrc_usage :=
                                    l_gme_batchstep_resources.plan_rsrc_usage;
               END IF;         /*IF l_gme_batchstep_resources.scale_type = 1*/
            /* copy plan to actual only if profile is set */
            ELSIF     NVL (l_prev_actual_actv_fact, 0) > 0
                  AND l_copy_plan_to_actual = 1 THEN
               l_gme_batchstep_resources.actual_rsrc_usage :=
                    (  l_gme_batchstep_resources.actual_rsrc_usage
                     / l_prev_actual_actv_fact)
                  * p_batchstep_activities_rec.actual_activity_factor;
            END IF;   /*IF (l_gme_batchstep_resources.actual_rsrc_usage = 0)*/

            /* if at end of calculation usage is null assign zero */
            l_gme_batchstep_resources.actual_rsrc_usage :=
                          NVL (l_gme_batchstep_resources.actual_rsrc_usage, 0);

            IF (NVL (l_gme_batchstep_resources.actual_rsrc_qty, 0) = 0) THEN
               IF l_gme_batchstep_resources.scale_type = 0 THEN
                  /* Bug 2506750 copy plan to actual only if profile is set and conditions satisfy */
                  IF (   NVL (l_gme_batchstep_resources.actual_rsrc_usage, 0) >
                                                                             0
                      OR (    NVL
                                 (l_gme_batchstep_resources.actual_rsrc_usage
                                 ,0) = 0
                          AND l_copy_plan_to_actual = 1) ) THEN
                     l_gme_batchstep_resources.actual_rsrc_qty :=
                          l_gme_batchstep_resources.plan_rsrc_qty
                        * p_batchstep_activities_rec.actual_activity_factor;
                  END IF;
               /* copy plan to actual only if profile is set */
               ELSIF (l_copy_plan_to_actual = 1) THEN
                  /* calculate actual_rsrc_qty from plan_rsrc_qty not from step */
                  --l_gme_batchstep_resources.actual_rsrc_qty := p_batch_step_rec.actual_step_qty * p_batchstep_activities_rec.actual_activity_factor;
                  IF NVL (l_prev_actual_actv_fact, 0) > 0 THEN
                     l_gme_batchstep_resources.actual_rsrc_qty :=
                          (  l_gme_batchstep_resources.plan_rsrc_qty
                           * p_batchstep_activities_rec.actual_activity_factor)
                        / l_prev_actual_actv_fact;
                  ELSE
                     l_gme_batchstep_resources.actual_rsrc_qty :=
                          l_gme_batchstep_resources.plan_rsrc_qty
                        * p_batchstep_activities_rec.actual_activity_factor;
                  END IF;
               END IF;
            /* copy plan to actual only if profile is set and conditions satisfy */
            ELSIF     NVL (l_prev_actual_actv_fact, 0) > 0
                  AND l_copy_plan_to_actual = 1 THEN
               l_gme_batchstep_resources.actual_rsrc_qty :=
                    (  l_gme_batchstep_resources.actual_rsrc_qty
                     / l_prev_actual_actv_fact)
                  * p_batchstep_activities_rec.actual_activity_factor;
            END IF;     /*IF (l_gme_batchstep_resources.actual_rsrc_qty = 0)*/

            /* if at end of calculation usage is null assign zero */
            l_gme_batchstep_resources.actual_rsrc_qty :=
                            NVL (l_gme_batchstep_resources.actual_rsrc_qty, 0);

            /*  Default the Actual Resource Count to the Planned Resource  */
            /*  Count if the Actual Resource Count is zero.                */
            IF (NVL (l_gme_batchstep_resources.actual_rsrc_count, 0) = 0) THEN
               l_gme_batchstep_resources.actual_rsrc_count :=
                                    l_gme_batchstep_resources.plan_rsrc_count;
            END IF;

            /* (New Fix) if at end of calculation usage is null assign zero */
            l_gme_batchstep_resources.actual_rsrc_count :=
                          NVL (l_gme_batchstep_resources.actual_rsrc_count, 1);
         ELSIF (p_batch_step_rec.step_status = gme_common_pvt.g_step_pending) THEN
            OPEN cur_prev_actv_fact
                            (p_batchstep_activities_rec.batchstep_activity_id);

            FETCH cur_prev_actv_fact
             INTO l_prev_plan_actv_fact, l_prev_actual_actv_fact;

            CLOSE cur_prev_actv_fact;

            OPEN cur_prev_plan_charge;

            FETCH cur_prev_plan_charge
             INTO l_prev_plan_charges;

            CLOSE cur_prev_plan_charge;

            IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
               gme_debug.put_line (   ' plan rsrc qty:'
                                   || l_gme_batchstep_resources.plan_rsrc_qty
                                   || ' Plan actv:'
                                   || l_prev_plan_actv_fact);
            END IF;

            /* Calculate resource qty correctly when scaling batch */
            IF (    p_routing_scale_factor IS NOT NULL
                AND l_gme_batchstep_resources.scale_type = 1) THEN
               l_gme_batchstep_resources.plan_rsrc_qty :=
                    l_gme_batchstep_resources.plan_rsrc_qty
                  * p_routing_scale_factor;
               l_gme_batchstep_resources.plan_rsrc_usage :=
                    l_gme_batchstep_resources.plan_rsrc_usage
                  * p_routing_scale_factor;
            END IF;

            IF l_prev_plan_actv_fact > 0 THEN
               l_gme_batchstep_resources.plan_rsrc_qty :=
                    (  l_gme_batchstep_resources.plan_rsrc_qty
                     * p_batchstep_activities_rec.plan_activity_factor)
                  / l_prev_plan_actv_fact;
            ELSE
               l_gme_batchstep_resources.plan_rsrc_qty :=
                    l_gme_batchstep_resources.plan_rsrc_qty
                  * p_batchstep_activities_rec.plan_activity_factor;
            END IF;

            IF l_prev_plan_actv_fact > 0 THEN
               l_gme_batchstep_resources.plan_rsrc_usage :=
                    (  l_gme_batchstep_resources.plan_rsrc_usage
                     / l_prev_plan_actv_fact)
                  * p_batchstep_activities_rec.plan_activity_factor;
            ELSE
               l_gme_batchstep_resources.plan_rsrc_usage :=
                    l_gme_batchstep_resources.plan_rsrc_usage
                  * p_batchstep_activities_rec.plan_activity_factor;
            END IF;

            /* If the resource scale type is calculate by charges then */
            IF l_gme_batchstep_resources.scale_type = 2 THEN
               IF     (NVL (l_prev_plan_charges, 0) > 0)
                  AND (NVL (p_batch_step_rec.plan_charges, 0) > 0) THEN
                  l_gme_batchstep_resources.plan_rsrc_usage :=
                       (  l_gme_batchstep_resources.plan_rsrc_usage
                        / l_prev_plan_charges)
                     * p_batch_step_rec.plan_charges;
               ELSIF (NVL (p_batch_step_rec.plan_charges, 0) > 0) THEN
                  l_gme_batchstep_resources.plan_rsrc_usage :=
                       l_gme_batchstep_resources.plan_rsrc_usage
                     * p_batch_step_rec.plan_charges;
               END IF;
            END IF;          /* IF l_gme_batchstep_resources.scale_type = 2 */

            /* set plan values accordingly if unrelease, let above calculations go through and then we will
               see if we need to put zero and count = 1 since count has to be greater than 0 */
            l_gme_batchstep_resources.plan_rsrc_count :=
                            NVL (l_gme_batchstep_resources.plan_rsrc_count, 1);
            l_gme_batchstep_resources.plan_rsrc_usage :=
                            NVL (l_gme_batchstep_resources.plan_rsrc_usage, 0);
            l_gme_batchstep_resources.plan_rsrc_qty :=
                              NVL (l_gme_batchstep_resources.plan_rsrc_qty, 0);
            l_gme_batchstep_resources.actual_rsrc_usage := NULL;
            l_gme_batchstep_resources.actual_rsrc_qty := NULL;
            l_gme_batchstep_resources.actual_rsrc_count := NULL;
         ELSIF p_batch_step_rec.step_status = gme_common_pvt.g_step_wip THEN
            IF (p_backflush_factor IS NOT NULL) THEN
               IF l_gme_batchstep_resources.scale_type = 1 THEN
                                       /* Proportional resource scale type */
                  IF (p_batchstep_activities_rec.plan_activity_factor > 0) THEN
                     IF (   NVL (p_dependency_type, 1) = 1
                         OR NVL (l_gme_batchstep_resources.actual_rsrc_usage
                                ,0) = 0) THEN
                        l_gme_batchstep_resources.actual_rsrc_usage :=
                             NVL
                                (l_gme_batchstep_resources.actual_rsrc_usage
                                ,0)
                           + (   (  l_gme_batchstep_resources.plan_rsrc_usage
                                  * p_batchstep_activities_rec.actual_activity_factor
                                  * p_backflush_factor)
                              / (p_batchstep_activities_rec.plan_activity_factor) );
                     END IF;

                     IF (   NVL (p_dependency_type, 1) = 1
                         OR NVL (l_gme_batchstep_resources.actual_rsrc_qty, 0) =
                                                                             0) THEN
                        l_gme_batchstep_resources.actual_rsrc_qty :=
                             NVL (l_gme_batchstep_resources.actual_rsrc_qty
                                 ,0)
                           + (   (  l_gme_batchstep_resources.plan_rsrc_qty
                                  * p_batchstep_activities_rec.actual_activity_factor
                                  * p_backflush_factor)
                              / (p_batchstep_activities_rec.plan_activity_factor) );
                     END IF;
                  ELSE
                     IF (   NVL (p_dependency_type, 1) = 1
                         OR NVL (l_gme_batchstep_resources.actual_rsrc_usage
                                ,0) = 0) THEN
                        l_gme_batchstep_resources.actual_rsrc_usage :=
                             NVL
                                (l_gme_batchstep_resources.actual_rsrc_usage
                                ,0)
                           + (  l_gme_batchstep_resources.plan_rsrc_usage
                              * p_batchstep_activities_rec.actual_activity_factor
                              * p_backflush_factor);
                     END IF;

                     IF (   NVL (p_dependency_type, 1) = 1
                         OR NVL (l_gme_batchstep_resources.actual_rsrc_qty, 0) =
                                                                             0) THEN
                        l_gme_batchstep_resources.actual_rsrc_qty :=
                             NVL (l_gme_batchstep_resources.actual_rsrc_qty
                                 ,0)
                           + (  l_gme_batchstep_resources.plan_rsrc_qty
                              * p_batchstep_activities_rec.actual_activity_factor
                              * p_backflush_factor);
                     END IF;
                  END IF;

                  IF (l_gme_batchstep_resources.actual_rsrc_usage < 0) THEN
                     l_gme_batchstep_resources.actual_rsrc_usage := 0;
                  END IF;

                  IF (l_gme_batchstep_resources.actual_rsrc_qty < 0) THEN
                     l_gme_batchstep_resources.actual_rsrc_qty := 0;
                  END IF;
               ELSIF l_gme_batchstep_resources.scale_type = 2 THEN
                                           /* By charge resource scale type */
                  IF (p_batch_step_rec.plan_charges <> 0) THEN
                     IF (p_batchstep_activities_rec.plan_activity_factor > 0) THEN
                        IF (   NVL (p_dependency_type, 1) = 1
                            OR NVL
                                  (l_gme_batchstep_resources.actual_rsrc_usage
                                  ,0) = 0) THEN
                           l_gme_batchstep_resources.actual_rsrc_usage :=
                                NVL
                                   (l_gme_batchstep_resources.actual_rsrc_usage
                                   ,0)
                              + (   (  l_gme_batchstep_resources.plan_rsrc_usage
                                     * p_charge_diff
                                     * p_batchstep_activities_rec.actual_activity_factor)
                                 / (  p_batchstep_activities_rec.plan_activity_factor
                                    * p_batch_step_rec.plan_charges) );
                        END IF;

                        IF (   NVL (p_dependency_type, 1) = 1
                            OR NVL (l_gme_batchstep_resources.actual_rsrc_qty
                                   ,0) = 0) THEN
                           l_gme_batchstep_resources.actual_rsrc_qty :=
                                NVL
                                   (l_gme_batchstep_resources.actual_rsrc_qty
                                   ,0)
                              + (   (  l_gme_batchstep_resources.plan_rsrc_qty
                                     * p_charge_diff
                                     * p_batchstep_activities_rec.actual_activity_factor)
                                 / (  p_batchstep_activities_rec.plan_activity_factor
                                    * p_batch_step_rec.plan_charges) );
                        END IF;
                     ELSE
                        IF (   NVL (p_dependency_type, 1) = 1
                            OR NVL
                                  (l_gme_batchstep_resources.actual_rsrc_usage
                                  ,0) = 0) THEN
                           l_gme_batchstep_resources.actual_rsrc_usage :=
                                NVL
                                   (l_gme_batchstep_resources.actual_rsrc_usage
                                   ,0)
                              + (   (  l_gme_batchstep_resources.plan_rsrc_usage
                                     * p_charge_diff
                                     * p_batchstep_activities_rec.actual_activity_factor)
                                 / (p_batch_step_rec.plan_charges) );
                        END IF;

                        IF (   NVL (p_dependency_type, 1) = 1
                            OR NVL (l_gme_batchstep_resources.actual_rsrc_qty
                                   ,0) = 0) THEN
                           l_gme_batchstep_resources.actual_rsrc_qty :=
                                NVL
                                   (l_gme_batchstep_resources.actual_rsrc_qty
                                   ,0)
                              + (   (  l_gme_batchstep_resources.plan_rsrc_qty
                                     * p_charge_diff
                                     * p_batchstep_activities_rec.actual_activity_factor)
                                 / (p_batch_step_rec.plan_charges) );
                        END IF;
                     END IF;
                  END IF;

                  IF (l_gme_batchstep_resources.actual_rsrc_usage < 0) THEN
                     l_gme_batchstep_resources.actual_rsrc_usage := 0;
                  END IF;

                  IF (l_gme_batchstep_resources.actual_rsrc_qty < 0) THEN
                     l_gme_batchstep_resources.actual_rsrc_qty := 0;
                  END IF;
               ELSE                            /* Fixed resource scale type */
                  l_gme_batchstep_resources.actual_rsrc_usage :=
                                    l_gme_batchstep_resources.plan_rsrc_usage;
                  l_gme_batchstep_resources.actual_rsrc_qty :=
                                      l_gme_batchstep_resources.plan_rsrc_qty;
               END IF;
            END IF;

            /*  Default the Actual Resource Count to the Planned Resource  */
            /*  Count if the Actual Resource Count is Null.                */
            IF (    NVL (l_gme_batchstep_resources.actual_rsrc_count, -1) = -1
                AND p_backflush_factor IS NOT NULL) THEN
               l_gme_batchstep_resources.actual_rsrc_count :=
                                    l_gme_batchstep_resources.plan_rsrc_count;
            END IF;

            l_gme_batchstep_resources.plan_rsrc_count :=
                            NVL (l_gme_batchstep_resources.plan_rsrc_count, 1);
            l_gme_batchstep_resources.plan_rsrc_usage :=
                            NVL (l_gme_batchstep_resources.plan_rsrc_usage, 0);
            l_gme_batchstep_resources.plan_rsrc_qty :=
                              NVL (l_gme_batchstep_resources.plan_rsrc_qty, 0);
         END IF;       /* IF p_batch_hdr_rec.automatic_step_calculation = 1 */

         /* We should check for the process qty going below the min capacity based on the profile */
         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line
                         (   ' Allow qty to go below capacity profile value:'
                          || l_allow_qty_below_cap);
         END IF;

         IF l_allow_qty_below_cap = 0 THEN
            IF gme_common_pvt.is_qty_below_capacity
                     (p_batch_step_resources_rec      => l_gme_batchstep_resources) THEN
               RAISE qty_below_cap_error;
            END IF;
         END IF;                            /* IF l_allow_qty_below_cap = 0 */

         /* Only if the update inventory ind is set to 'Y' on the batch header */
         /* then only we will have resource transactions                       */
         IF p_batch_hdr_rec.update_inventory_ind = 'Y' THEN
            IF     (p_batch_step_rec.step_status >
                                                 gme_common_pvt.g_step_pending)
               AND (NVL (l_gme_batchstep_resources.actual_rsrc_usage, -1) >= 0) THEN
               IF p_batch_hdr_rec.automatic_step_calculation = 1 THEN
                  OPEN cur_sum_override_resource
                            (l_gme_batchstep_resources.batchstep_resource_id
                            ,l_doc_type
                            ,p_batch_hdr_rec.batch_id);

                  FETCH cur_sum_override_resource
                   INTO l_override_usage;

                  CLOSE cur_sum_override_resource;

                  l_gme_batchstep_resources.actual_rsrc_usage :=
                       l_gme_batchstep_resources.actual_rsrc_usage
                     + NVL (l_override_usage, 0);
               END IF; /* IF p_batch_hdr_rec.automatic_step_calculation = 1 */
          /* if Actual Start Date and Actual End Date are NULL */
            /*
            ER 19161894  Shaliu Chen 18-JUL-2014
            Added for OPM Step Level OSP Project
            Only meet following 2 scenarios, the resource quantity calculation function can be executed
            1.step is non-osp step
            2.step is osp step,and step status is greater than wip,and there is
              resource txn exist which is generated by receiving po.
            */
            l_osp_batch_flag := FALSE;
            IF gme_osp.check_release_version THEN

              l_osp_batch_flag := gme_osp.is_OSP_batch(p_batch_id      => p_batch_step_rec.batch_id,
                                                       p_batchstep_id  => p_batch_step_rec.batchstep_id);

              l_res_txn_exist := gme_osp.is_resource_txn_exist(p_batch_id      => p_batch_step_rec.batch_id,
                                                               p_batchstep_id  => p_batch_step_rec.batchstep_id);
            END IF;
            IF (NOT l_osp_batch_flag OR
                (l_osp_batch_flag AND
                 l_res_txn_exist AND
                 p_batch_step_rec.step_status > gme_common_pvt.g_step_wip)) THEN
               IF l_gme_batchstep_resources.actual_start_date IS NULL THEN
                  l_gme_batchstep_resources.actual_start_date :=
                                           p_batch_step_rec.actual_start_date;
               END IF;

               IF l_gme_batchstep_resources.actual_cmplt_date IS NULL THEN
                  l_gme_batchstep_resources.actual_cmplt_date :=
                                           p_batch_step_rec.actual_cmplt_date;
               END IF;

               adjust_actual_usage
                     (p_batch_step_resources_rec      => l_gme_batchstep_resources
                     ,x_return_status                 => l_return_status);

               IF l_return_status <> x_return_status THEN
                  RAISE resource_trans_ins_err;
               END IF;
            END IF;
            /* we have to delete the actual transactions and create pending resource transactions */
            ELSIF (   p_batch_step_rec.step_status =
                                                 gme_common_pvt.g_step_pending
                   OR (    p_batch_hdr_rec.automatic_step_calculation = 1
                       AND p_batch_step_rec.step_status =
                                                     gme_common_pvt.g_step_wip) ) THEN
               IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
                  gme_debug.put_line (' invoking pending usage');
               END IF;

               adjust_pending_usage
                     (p_batch_step_resources_rec      => l_gme_batchstep_resources
                     ,x_return_status                 => l_return_status);

               IF l_return_status <> x_return_status THEN
                  RAISE resource_trans_ins_err;
               END IF;
            END IF;            /* IF (p_batch_step_rec.step_status > 1) AND */
         END IF;            /* IF p_batch_header.update_inventory_ind = 'Y' */

         l_gme_batchstep_resources.plan_rsrc_qty :=
                           ROUND (l_gme_batchstep_resources.plan_rsrc_qty, 32);

         l_gme_batchstep_resources.plan_rsrc_usage :=
                         ROUND (l_gme_batchstep_resources.plan_rsrc_usage, 32);
                         
         /*
         ER 19161894  Shaliu Chen 18-JUL-2014
         Added for OPM Step Level OSP Project
         For osp step resource
         1).the actual step resource should only be updated via receiving po
         2).only meet following conditions,the actual process qty and actual resource
            count can be update
            step status is greater than wip
            there is resource txn existing.
         
         */
         l_osp_batch_flag := FALSE;
         IF gme_osp.check_release_version THEN

           l_osp_batch_flag := gme_osp.is_OSP_batch(p_batch_id      => p_batch_step_rec.batch_id,
                                                    p_batchstep_id  => p_batch_step_rec.batchstep_id);

           l_res_txn_exist := gme_osp.is_resource_txn_exist(p_batch_id      => p_batch_step_rec.batch_id,
                                                            p_batchstep_id  => p_batch_step_rec.batchstep_id);
         END IF;
         
         IF NOT (gme_batch_step_resources_dbl.fetch_row
                                                   (l_gme_batchstep_resources
                                                   ,l_cur_gme_batchstep_resources) ) THEN
            RAISE activity_resource_fetch_error;
         END IF; 
                   
         IF (NOT l_osp_batch_flag) THEN           
           l_gme_batchstep_resources.actual_rsrc_usage :=
                         ROUND (l_gme_batchstep_resources.actual_rsrc_usage, 32);
         ELSE
           l_gme_batchstep_resources.actual_rsrc_usage :=
                         ROUND (l_cur_gme_batchstep_resources.actual_rsrc_usage, 32);
         END IF;     
               
         IF (NOT l_osp_batch_flag OR
            (l_osp_batch_flag AND
             l_res_txn_exist AND
             p_batch_step_rec.step_status > gme_common_pvt.g_step_wip)) THEN                         
           l_gme_batchstep_resources.actual_rsrc_qty :=
                           ROUND (l_gme_batchstep_resources.actual_rsrc_qty, 32); 
         ELSE
           l_gme_batchstep_resources.actual_rsrc_qty :=
                           ROUND (l_cur_gme_batchstep_resources.actual_rsrc_qty, 32); 
           l_gme_batchstep_resources.actual_rsrc_count :=
                           ROUND (l_cur_gme_batchstep_resources.actual_rsrc_count, 32);
                           
           IF l_gme_batchstep_resources.actual_rsrc_count = 0 AND 
              l_gme_batchstep_resources.actual_rsrc_usage = 0 AND 
              l_gme_batchstep_resources.actual_rsrc_qty IS NULL THEN
              l_gme_batchstep_resources.actual_rsrc_qty :=0;
           END IF;                                                                
         END IF;                                     

         /* Save the updated batch step resources row to the database */
         IF NOT (gme_batch_step_resources_dbl.update_row
                                                    (l_gme_batchstep_resources) ) THEN
            RAISE step_resource_upd_err;
         END IF;
      END LOOP;          /* FOR i IN 1..l_batchstep_resource_ids.COUNT LOOP */

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;
   EXCEPTION
      WHEN activity_resource_fetch_error THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN resource_trans_ins_err THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN step_resource_upd_err THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN error_fetch_rsrc_tran THEN
         x_return_status := l_return_status;
      WHEN error_txns_update THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN qty_below_cap_error THEN
         x_return_status := fnd_api.g_ret_sts_error;
         gme_common_pvt.log_message ('GME_API_RSRC_QTY_BELOW_CAP'
                                    ,'RESOURCES'
                                    ,l_gme_batchstep_resources.resources);
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
   END update_resources;

/*===========================================================================================
Procedure
  Build_Insert_Resource_Txn
Description
  This particular procedure is used to build a resource transaction row based on the resource
  line row and the usage passed in and insert it.
Parameters
  p_batch_hdr_rec       Batch Header Row
  p_batchstep_resources    Batch Step Resource Line
  p_usage         Usage to be created.
  p_completed        Build a completed or pending transaction.
  x_return_status    outcome of the API call
            S - Success
            E - Error
            U - Unexpected error
HISTORY            
  G. Muratore   19-MAR-2010  Bug 8751983     
     Stamp resource transaction with the trans_date if new p_trans_date parameter passed in.
=============================================================================================*/
   PROCEDURE build_insert_resource_txn (
      p_batch_hdr_rec        IN              gme_batch_header%ROWTYPE
     ,p_batchstep_resource   IN              gme_batch_step_resources%ROWTYPE
     ,p_usage                IN              NUMBER
     ,p_completed            IN              NUMBER DEFAULT 1
     ,p_trans_date           IN              DATE DEFAULT NULL
     ,x_return_status        OUT NOCOPY      VARCHAR2)
   IS
      /* Local Variables */
      l_api_name      CONSTANT VARCHAR2 (30)   := 'Build_Insert_Resource_Txn';

      /* Cursor Definitions */
      CURSOR cur_get_poc_trans_id
      IS
         SELECT gem5_poc_trans_id_s.NEXTVAL
           FROM SYS.DUAL;

      /* Collections for details */
      l_ins_resource_row       gme_resource_txns_gtmp%ROWTYPE;
      l_usage_hrs              gme_batch_step_resources.plan_rsrc_usage%TYPE;
      l_return_status          VARCHAR2 (1);
      /* Exceptions */
      resource_trans_ins_err   EXCEPTION;
--  p_tran_rec            gmi_trans_engine_pub.ictran_rec;
--  l_tran_rec_out        gmi_trans_engine_pub.ictran_rec;
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      /* Initialize the return status to success */
      x_return_status := fnd_api.g_ret_sts_success;

      /* Generate the surrogate key */
      OPEN cur_get_poc_trans_id;

      FETCH cur_get_poc_trans_id
       INTO l_ins_resource_row.poc_trans_id;

      CLOSE cur_get_poc_trans_id;

      /* Assign the difference usage for posting a completed entry */
      l_ins_resource_row.resource_usage := ROUND (p_usage, 32);
      l_ins_resource_row.action_code := 'ADD';
      l_ins_resource_row.organization_id := p_batch_hdr_rec.organization_id;

       /* The appropriate doc type should be assigned to the transaction */
      /* based on the batch type */
      IF p_batch_hdr_rec.batch_type = 10 THEN
         l_ins_resource_row.doc_type := 'FPO';
      ELSE
         l_ins_resource_row.doc_type := 'PROD';
      END IF;

      l_ins_resource_row.doc_id := p_batch_hdr_rec.batch_id;
      l_ins_resource_row.line_type := 0;
      l_ins_resource_row.line_id := p_batchstep_resource.batchstep_resource_id;
      l_ins_resource_row.resources := p_batchstep_resource.resources;
      l_ins_resource_row.trans_um := p_batchstep_resource.usage_um;

      IF p_completed = 1 THEN
         l_ins_resource_row.trans_date :=
            NVL (p_batchstep_resource.actual_start_date
                ,gme_common_pvt.g_timestamp);
         l_ins_resource_row.start_date :=
            NVL (p_batchstep_resource.actual_start_date
                ,gme_common_pvt.g_timestamp);
         gme_create_step_pvt.get_usage_in_hours
                                               (p_usage
                                               ,p_batchstep_resource.usage_um
                                               ,l_usage_hrs
                                               ,l_return_status);

         IF l_return_status = 'S' THEN
            IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
               gme_debug.put_line (   'l_return_status'
                                   || l_return_status
                                   || ' usage'
                                   || l_usage_hrs);
            END IF;

            l_ins_resource_row.end_date :=
                        (l_ins_resource_row.start_date + (l_usage_hrs / 24) );

            IF (l_ins_resource_row.end_date > gme_common_pvt.g_timestamp) THEN
               l_ins_resource_row.end_date := gme_common_pvt.g_timestamp;
            END IF;
         ELSE
            l_ins_resource_row.end_date :=
               NVL (p_batchstep_resource.actual_cmplt_date
                   ,l_ins_resource_row.start_date);
            -- this is not a error, only a warning so we are setting it back to sucess.
            l_return_status := 'S';
         END IF;
      ELSE
         l_ins_resource_row.trans_date :=
            NVL (p_batchstep_resource.plan_start_date
                ,gme_common_pvt.g_timestamp);
         l_ins_resource_row.start_date :=
            NVL (p_batchstep_resource.plan_start_date
                ,gme_common_pvt.g_timestamp);
         l_ins_resource_row.end_date :=
            NVL (p_batchstep_resource.plan_cmplt_date
                ,gme_common_pvt.g_timestamp);
      END IF;

      l_ins_resource_row.completed_ind := p_completed;
      l_ins_resource_row.posted_ind := 0;
      l_ins_resource_row.overrided_protected_ind := 'N';
      l_ins_resource_row.delete_mark := 0;

/*  p_tran_rec.organization_id  := p_batch_hdr_rec.organization_id;
  p_tran_rec.whse_code  := p_batch_hdr_rec.wip_whse_code;
  p_tran_rec.trans_date := l_ins_resource_row.trans_date;
  IF NOT gme_api_grp.close_period_check_flexible
                     (p_tran_rec     => p_tran_rec,
                      x_tran_rec     => l_tran_rec_out)
  THEN
        RAISE FND_API.g_exc_error;
  END IF;
  l_ins_resource_row.trans_date := l_tran_rec_out.trans_date;
*/

      -- Bug 8751983 - Set trans date to value passed in. This is used
      -- when a new resource trans is caused by negative IB logic.
      IF (p_trans_date IS NOT NULL) THEN
         l_ins_resource_row.trans_date := p_trans_date;
      END IF;      
      
      IF NOT (gme_resource_txns_gtmp_dbl.insert_row (l_ins_resource_row
                                                    ,l_ins_resource_row) ) THEN
         RAISE resource_trans_ins_err;
      END IF;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;
   EXCEPTION
      WHEN resource_trans_ins_err THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN fnd_api.g_exc_error THEN
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

         x_return_status := fnd_api.g_ret_sts_unexp_error;
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
   END build_insert_resource_txn;

/*===========================================================================================
Procedure
  adjust_pending_usage
Description
  This particular procedure is used to adjust the pending usage to the plan resource usage
  of the resource
Parameters
  p_batch_step_rec_resources  Batch Step Resource Line
  x_return_status    outcome of the API call
            S - Success
            E - Error
            U - Unexpected error
=============================================================================================*/
   PROCEDURE adjust_pending_usage (
      p_batch_step_resources_rec   IN              gme_batch_step_resources%ROWTYPE
     ,x_return_status              OUT NOCOPY      VARCHAR2)
   IS
      CURSOR cur_sum_usage (v_batchstep_resource_id NUMBER)
      IS
         SELECT NVL (SUM (resource_usage), 0), COUNT (1)
           FROM gme_resource_txns_gtmp
          WHERE line_id = v_batchstep_resource_id AND completed_ind = 0;

      l_api_name   CONSTANT VARCHAR2 (30)           := 'adjust_pending_usage';
      /* Collections for details etc */
      l_batch_hdr           gme_batch_header%ROWTYPE;
      /* Local variables */
      l_alloc_usage         NUMBER;
      l_tot_usage           NUMBER;
      l_tran_count          NUMBER;
      l_return_status       VARCHAR2 (1);
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      /* Initialize return status to success */
      x_return_status := fnd_api.g_ret_sts_success;

      /* Lets get the sum of pending usages to determine if their is a change */
      OPEN cur_sum_usage (p_batch_step_resources_rec.batchstep_resource_id);

      FETCH cur_sum_usage
       INTO l_tot_usage, l_tran_count;

      CLOSE cur_sum_usage;

      IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
         gme_debug.put_line (   ' tot usage:'
                             || l_tot_usage
                             || ' count:'
                             || l_tran_count);
      END IF;

      IF    (l_tot_usage <> p_batch_step_resources_rec.plan_rsrc_usage)
         OR (l_tran_count <> p_batch_step_resources_rec.plan_rsrc_count) THEN
         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line
                      (   ' Pending Plan Usage:'
                       || TO_CHAR (p_batch_step_resources_rec.plan_rsrc_usage)
                       || ' Plan Count:'
                       || TO_CHAR (p_batch_step_resources_rec.plan_rsrc_count) );
         END IF;

         /* Deletes all resource transactions for the current resource */
         gme_delete_batch_step_pvt.delete_resource_transactions
                    (p_batch_step_resources_rec      => p_batch_step_resources_rec
                    ,x_return_status                 => l_return_status);

         IF l_return_status <> x_return_status THEN
            RAISE fnd_api.g_exc_error;
         END IF;

         l_alloc_usage :=
              p_batch_step_resources_rec.plan_rsrc_usage
            / p_batch_step_resources_rec.plan_rsrc_count;

         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line (' Alloc Usage:' || TO_CHAR (l_alloc_usage) );
         END IF;

         l_batch_hdr.batch_id := p_batch_step_resources_rec.batch_id;

         IF NOT gme_batch_header_dbl.fetch_row (p_batch_header      => l_batch_hdr
                                               ,x_batch_header      => l_batch_hdr) THEN
            RAISE fnd_api.g_exc_error;
         END IF;

         FOR i IN 1 .. p_batch_step_resources_rec.plan_rsrc_count LOOP
            build_insert_resource_txn
                         (p_batch_hdr_rec           => l_batch_hdr
                         ,p_batchstep_resource      => p_batch_step_resources_rec
                         ,p_usage                   => l_alloc_usage
                         ,p_completed               => 0
                         ,x_return_status           => l_return_status);

            IF l_return_status <> x_return_status THEN
               RAISE fnd_api.g_exc_error;
            END IF;
         END LOOP; /* FOR i IN 1..l_gme_batchstep_resources.plan_rsrc_count */
      END IF;
          /* IF (l_tot_usage <> p_batch_step_resources_rec.plan_rsrc_usage) */
   EXCEPTION
      WHEN fnd_api.g_exc_error THEN
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

         x_return_status := fnd_api.g_ret_sts_unexp_error;
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
   END adjust_pending_usage;

/*===========================================================================================
Procedure
  adjust_actual_usage
Description
  This particular procedure is used to adjust the actual usage to the actual resource usage
  of the resource

  The following steps identify the code in this procedure:

  Step 1 : If their are any pending transactions then we have to either delete them or adjust
           them based on the step status.

  Step 2 : Check for the total completed usage transactions if it equals to the resource
           line actual usage then we need not do any adjustment we can return.

  Step 3 : If we are here then their is some adjustment we have to make. If the total usage is
           less than the actual resource usage then we perform step 4 else step 5.

  Step 4 : If the total usage is less than the actual usage then, find the difference between
           actual resource usage and the total usage.
           Divide the difference amount with the actual resource count and post one transactions for
           the divided amount for each actual resource count.

  Step 5 : If the actual usage is less than the total usage then, delete all the completed transactions
           which are not override protected and then subtract total override protected usage from the
           actual resource usage and then divide the difference amount with the actual resource count
           and post one transactions for the divided amount for each actual resource count.
Parameters
  p_batch_step_resources_rec  Batch Step Resource Line
  x_return_status    outcome of the API call
            S - Success
            E - Error
            U - Unexpected error
HISTORY:            
  G. Muratore   19-MAR-2010  Bug 8751983     
     Fetch resource transactions in reverse trans order just in case this is being called  
     by negative IB. Also, rework logic so that all the existing resource transactions are 
     not blindly removed. Reverse out only what is necessary. Also, Pass in a trans date 
     for any new resource transaction generated by negative IB logic.
=============================================================================================*/
   PROCEDURE adjust_actual_usage (
      p_batch_step_resources_rec   IN              gme_batch_step_resources%ROWTYPE
     ,x_return_status              OUT NOCOPY      VARCHAR2)
   IS
      l_api_name   CONSTANT VARCHAR2 (30)            := 'adjust_actual_usage';
      
      /*
        BUG 20519330  2-MAR-2015 Shaliu Chen
        make the decimal precision to 32 so that l_sum_comp_usage and 
        p_batch_step_resources_rec.actual_rsrc_usage have same precision 
      */
      CURSOR cur_sum_comp_usage (v_batchstep_resource_id NUMBER)
      IS
         SELECT ROUND(NVL (SUM (resource_usage), 0),32)
           FROM gme_resource_txns_gtmp
          WHERE line_id = v_batchstep_resource_id
            AND completed_ind = 1
            AND action_code <> 'DEL';

      CURSOR cur_count_comp_usage (v_batchstep_resource_id NUMBER)
      IS
         SELECT COUNT (1)
           FROM gme_resource_txns_gtmp
          WHERE line_id = v_batchstep_resource_id
            AND completed_ind = 1
            AND NVL (overrided_protected_ind, 'N') <> 'Y'
            AND action_code <> 'DEL';

      /* Collections for details etc */
      l_resource_txns       gme_resource_txns_gtmp%ROWTYPE;
      l_resource_tab        gme_common_pvt.resource_transactions_tab;
      l_batch_hdr           gme_batch_header%ROWTYPE;
      /* Local variables */
      l_sum_comp_usage      NUMBER;
      l_cnt_comp_usage      NUMBER;
      l_override_usage      NUMBER                                   DEFAULT 0;
      l_actual_usage        NUMBER;
      l_return_status       VARCHAR2 (1);
      
      -- Bug 8751983 - New variables.
      l_sum_comp_usage_loop NUMBER;      
      l_hold_trans_index    NUMBER;
      
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      /* Initialize return status to success */
      x_return_status := fnd_api.g_ret_sts_success;

      IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
         gme_debug.put_line (   ' Adjusting actual usage :'
                             || p_batch_step_resources_rec.actual_rsrc_usage);
      END IF;

      /* Lets fetch all the active resource transactions for the current resource */
      l_resource_txns.poc_trans_id := NULL;
      l_resource_txns.doc_id := NULL;
      l_resource_txns.line_id :=
                              p_batch_step_resources_rec.batchstep_resource_id;
                             
      -- Bug 8751983 - Let's fetch the resource transaction in reverse order
      -- so that if it is a negative IB, We process in LIFO order.
      gme_resource_engine_pvt.fetch_active_resources
                                           (p_resource_rec       => l_resource_txns
                                           ,x_resource_tbl       => l_resource_tab
                                           ,p_calling_mode       => 'ACTUAL_USAGE'
                                           ,x_return_status      => l_return_status);

      IF l_return_status <> x_return_status THEN
         RAISE fnd_api.g_exc_error;
      END IF;

      /* Step 1 : */
      gme_update_step_qty_pvt.reduce_pending_usage
                    (p_batch_step_resources_rec      => p_batch_step_resources_rec
                    ,x_return_status                 => l_return_status);

      IF l_return_status <> x_return_status THEN
         RAISE fnd_api.g_exc_error;
      END IF;

      /*  Calculate the sum of the Completed Transactions  */
      /*  for this Operation detail line.                  */
      OPEN cur_sum_comp_usage
                             (p_batch_step_resources_rec.batchstep_resource_id);

      FETCH cur_sum_comp_usage
       INTO l_sum_comp_usage;

      IF cur_sum_comp_usage%NOTFOUND THEN
         l_sum_comp_usage := 0;
      END IF;

      CLOSE cur_sum_comp_usage;

      IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
         gme_debug.put_line
                     (   ' Completed Usage:'
                      || TO_CHAR (l_sum_comp_usage)
                      || ' Actual Usage:'
                      || TO_CHAR (p_batch_step_resources_rec.actual_rsrc_usage)
                      || ' Resource:'
                      || p_batch_step_resources_rec.resources);
      END IF;

      OPEN cur_count_comp_usage
                             (p_batch_step_resources_rec.batchstep_resource_id);

      FETCH cur_count_comp_usage
       INTO l_cnt_comp_usage;

      CLOSE cur_count_comp_usage;

      IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
         gme_debug.put_line (   ' Completed Transaction Count:'
                             || TO_CHAR (l_cnt_comp_usage) );
      END IF;

      /* Step 2 : */
      -- IF l_sum_comp_usage <> p_batch_step_resources_rec.actual_rsrc_usage THEN
      -- Bug 26094274 - use rounding to avoid erroneous transaction processing.
      IF round(l_sum_comp_usage, 30) <> round(p_batch_step_resources_rec.actual_rsrc_usage, 30) THEN
         l_batch_hdr.batch_id := p_batch_step_resources_rec.batch_id;

         IF NOT gme_batch_header_dbl.fetch_row
                                              (p_batch_header      => l_batch_hdr
                                              ,x_batch_header      => l_batch_hdr) THEN
            RAISE fnd_api.g_exc_error;
         END IF;

         /* Step 3 : */
         IF (l_sum_comp_usage < p_batch_step_resources_rec.actual_rsrc_usage) THEN
            l_actual_usage :=
               p_batch_step_resources_rec.actual_rsrc_usage
               - l_sum_comp_usage;
            l_actual_usage :=
                l_actual_usage / p_batch_step_resources_rec.actual_rsrc_count;

            /* Step 4 : */
            FOR i IN 1 .. p_batch_step_resources_rec.actual_rsrc_count LOOP
               build_insert_resource_txn
                         (p_batch_hdr_rec           => l_batch_hdr
                         ,p_batchstep_resource      => p_batch_step_resources_rec
                         ,p_usage                   => l_actual_usage
                         ,p_completed               => 1
                         ,x_return_status           => l_return_status);

               IF l_return_status <> x_return_status THEN
                  RAISE fnd_api.g_exc_error;
               END IF;
            END LOOP;
                /* FOR i IN 1..p_batch_step_resources_rec.actual_rsrc_count */
         ELSE
            /* Step 5 : */

            -- Bug 8751983 - Rework following loop logic to back out only what's necessary..
            -- Let's calculate how much usage we need to back out.
            l_sum_comp_usage_loop := l_sum_comp_usage - p_batch_step_resources_rec.actual_rsrc_usage;
            
            l_hold_trans_index := 0;
            
            /* Delete all the existing completed transactions */ -- This is the original commment before 8751983.
            FOR i IN 1 .. l_resource_tab.COUNT LOOP
               -- Bug 8751983 - Let's not delete/reverse everything unless we have to.
               IF     (l_resource_tab (i).overrided_protected_ind <> 'Y')
                  AND l_sum_comp_usage_loop > 0
                  AND (l_resource_tab (i).completed_ind = 1) THEN
                  l_resource_txns := l_resource_tab (i);
                  gme_resource_engine_pvt.delete_resource_trans
                                          (p_tran_rec           => l_resource_txns
                                          ,x_return_status      => l_return_status);

                  IF l_return_status <> x_return_status THEN
                     RAISE fnd_api.g_exc_error;
                  END IF;
                  
                  -- Bug 8751983 - Let's subtract out the usage from the trans we just reversed.
                  l_sum_comp_usage_loop := l_sum_comp_usage_loop - l_resource_txns.resource_usage;  

                  -- Keep track of the last transaction that was reversed.
                  l_hold_trans_index := i;
                                                                      
               ELSIF (l_resource_tab (i).overrided_protected_ind = 'Y') THEN
                  l_override_usage :=
                         l_override_usage + l_resource_tab (i).resource_usage;
               END IF;               
                      /* IF l_resource_tab(i).override_protected_ind <> 'Y' */
            END LOOP;                   /* FOR i IN 1..l_resource_tab.COUNT */

            -- Bug 8751983 - let's calculate the new actual usage for the new transaction.
            -- The ELSE path can only be hit if there were no transactions deleted because of override.
            IF (l_sum_comp_usage_loop <> (l_sum_comp_usage - p_batch_step_resources_rec.actual_rsrc_usage)) THEN
               l_actual_usage := ABS(l_sum_comp_usage_loop);
            ELSE           
               l_actual_usage := p_batch_step_resources_rec.actual_rsrc_usage - l_override_usage;
            END IF;
               
            l_actual_usage :=
                 l_actual_usage / p_batch_step_resources_rec.actual_rsrc_count;
            
            -- Bug 8751983 - if there is usage remaining not accounted for then we need to add a transaction.  
            -- Pass in the trans_date of the last transaction to be used on newly added trans.          
            IF l_actual_usage > 0 THEN	
--            IF p_batch_step_resources_rec.actual_rsrc_usage > 0 THEN	
               FOR i IN 1 .. p_batch_step_resources_rec.actual_rsrc_count LOOP
                  build_insert_resource_txn
                            (p_batch_hdr_rec           => l_batch_hdr
                            ,p_batchstep_resource      => p_batch_step_resources_rec
                            ,p_usage                   => l_actual_usage
                            ,p_completed               => 1
                            ,p_trans_date              => l_resource_tab(l_hold_trans_index).trans_date
                            ,x_return_status           => l_return_status);            
                  IF l_return_status <> x_return_status THEN
                     RAISE fnd_api.g_exc_error;
                  END IF;
               END LOOP;
                /* FOR i IN 1..p_batch_step_resources_rec.actual_rsrc_count */
            END IF;          
         END IF;
      /* IF l_sum_comp_usage < p_batch_step_resources_rec.actual_rsrc_usage */
      END IF;
     /* IF l_sum_comp_usage <> p_batch_step_resources_rec.actual_rsrc_usage */

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;
   EXCEPTION
      WHEN fnd_api.g_exc_error THEN
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

         x_return_status := fnd_api.g_ret_sts_unexp_error;
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
   END adjust_actual_usage;

/*===========================================================================================
Procedure
  reduce_pending_usage
Description
  This particular procedure is used to reduce the pending usage based on the actual allocations
  made.
Parameters
  p_batch_step_resources_rec  Batch Step Resource Line
  x_return_status    outcome of the API call
            S - Success
            E - Error
            U - Unexpected error
History
  Pawan Kumar for bug 2393432 Modified procedure reduce_pending_usage
  Namit Singhi. Bug#5609683. Restructuring of code when calculating pending usage.
  
  A. Mishra     11-DEC-2009  Bug 8564008      
     The resource usage was not getting converted to hours 
     as the procedure GET_USAGE_IN HOURS was not getting called
     IN the procedure reduce_pending_usage gave a call to this procudue to 
     convert the usage in hours first before computing the transaction dates

  G. Muratore   23-Oct-2013  Bug 16297308      
     Use correct variable when calculating usage hrs. 
=============================================================================================*/
   PROCEDURE reduce_pending_usage (
      p_batch_step_resources_rec   IN              gme_batch_step_resources%ROWTYPE
     ,x_return_status              OUT NOCOPY      VARCHAR2)
   IS
      l_api_name   CONSTANT VARCHAR2 (30)           := 'reduce_pending_usage';

      CURSOR cur_get_step_status (v_batchstep_id NUMBER)
      IS
         SELECT step_status
           FROM gme_batch_steps
          WHERE batchstep_id = v_batchstep_id;

      -- nsinghi bug#5609683 Modified the cursor to also query resource plan start date and changed cursor name
      CURSOR cur_get_rsrc_dtl (v_batchstep_resource_id NUMBER)
      IS
         SELECT actual_rsrc_usage, plan_start_date
           FROM gme_batch_step_resources
          WHERE batchstep_resource_id = v_batchstep_resource_id;

      CURSOR cur_pend_count (v_batchstep_resource_id NUMBER)
      IS
         SELECT COUNT (1)
           FROM gme_resource_txns_gtmp
          WHERE line_id = v_batchstep_resource_id AND completed_ind = 0;

      /* Collections for details etc */
      l_resource_txns       gme_resource_txns_gtmp%ROWTYPE;
      l_resource_tab        gme_common_pvt.resource_transactions_tab;
      
      /* Local variables */
      l_alloc_usage         NUMBER;
      l_usage               NUMBER;
      l_tot_usage           NUMBER;
      l_tran_count          NUMBER;
      l_return_status       VARCHAR2 (1);
      l_step_status         NUMBER (5);
      l_prev_usage          NUMBER;
      l_rsrc_plan_strt_dt   DATE;
      l_remaining_tran_count NUMBER; 
      l_extra_usage_left     NUMBER := 0; 

      -- 8564008  added the parameters for  the call to gme_create_step_pvt.get_usage_in_hours.
      l_usage_hrs             NUMBER;
      l_usage_conv_ret_status	VARCHAR2(1000);       
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      /* Initialize return status to success */
      x_return_status := fnd_api.g_ret_sts_success;

      /* Let us check the status of the step */
      OPEN cur_get_step_status (p_batch_step_resources_rec.batchstep_id);

      FETCH cur_get_step_status
       INTO l_step_status;

      CLOSE cur_get_step_status;

      /* Lets fetch all the active resource transactions for the current resource */
      l_resource_txns.poc_trans_id := NULL;
      l_resource_txns.doc_id       := NULL;
      l_resource_txns.line_id      := p_batch_step_resources_rec.batchstep_resource_id;
  
      --nsinghi bug#5609683 
      --calling the procedure with additional parameter p_calling_mode.
      --For reducing pending resource usage, we need to fetch the resources in ascending order of resource usage.
      gme_resource_engine_pvt.fetch_active_resources
                                           (p_resource_rec       => l_resource_txns
                                           ,p_calling_mode       => 'REDUCE_USAGE'
                                           ,x_resource_tbl       => l_resource_tab
                                           ,x_return_status      => l_return_status);

      IF l_return_status <> x_return_status THEN
         RAISE fnd_api.g_exc_error;
      END IF;

      IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
         gme_debug.put_line(g_pkg_name||'.'||l_api_name||': Line ID '||l_resource_txns.line_id);                              
         gme_debug.put_line(g_pkg_name||'.'||l_api_name||': Resource Txns Count '||l_resource_tab.COUNT);                              
         gme_debug.put_line(g_pkg_name||'.'||l_api_name||': Step Status '||l_step_status);                              
      END IF;                              

      /* If the step is in WIP then we need to adjust the pending transactions */
      /* so lets calculate the usage to be adjusted                            */
      IF l_step_status = gme_common_pvt.g_step_wip THEN
         /* Lets get the previous actual usage */

         OPEN cur_get_rsrc_dtl
                            (p_batch_step_resources_rec.batchstep_resource_id);

         FETCH cur_get_rsrc_dtl 
          INTO l_prev_usage, l_rsrc_plan_strt_dt;
         CLOSE cur_get_rsrc_dtl;

         /* Lets get the sum of pending usages and the count */
         OPEN cur_pend_count
                            (p_batch_step_resources_rec.batchstep_resource_id);

         FETCH cur_pend_count
          INTO l_tran_count;

         CLOSE cur_pend_count;

         IF l_prev_usage > p_batch_step_resources_rec.plan_rsrc_usage THEN
            l_alloc_usage :=
                 NVL (p_batch_step_resources_rec.actual_rsrc_usage, 0)
               - p_batch_step_resources_rec.plan_rsrc_usage;
               
            -- Bug 8564008
            IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
               gme_debug.put_line(g_pkg_name||'.'||l_api_name||': l_prev_usage > p_batch_step_resources_rec.plan_rsrc_usage l_alloc_usage :'||l_alloc_usage);
            END IF;
         ELSE
            l_alloc_usage :=
                 NVL (p_batch_step_resources_rec.actual_rsrc_usage, 0)
               - NVL (l_prev_usage, 0);

            -- Bug 8564008
            IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
               gme_debug.put_line(g_pkg_name||'.'||l_api_name||': l_alloc_usage :'||l_alloc_usage);
            END IF;
         END IF;

         IF l_tran_count > 0 THEN
            l_alloc_usage := l_alloc_usage / l_tran_count;
         END IF;

         -- Bug 8564008
         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line(g_pkg_name||'.'||l_api_name||': l_alloc_usage :'||l_alloc_usage);
         END IF;
      END IF;                                       /* IF l_step_status = 2 */

      l_remaining_tran_count := l_tran_count; 
      FOR i IN 1 .. l_resource_tab.COUNT LOOP
         -- Bug 8564008
         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line(g_pkg_name||'.'||l_api_name||': Processing the resource transactions loop');
	         gme_debug.put_line(g_pkg_name||'.'||l_api_name||': l_resource_tab (i).completed_ind :'||l_resource_tab (i).completed_ind);
         END IF;
      
         IF l_resource_tab (i).completed_ind = 0 THEN
            l_resource_txns := l_resource_tab (i);

            -- Bug 8564008
            IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
               gme_debug.put_line(g_pkg_name||'.'||l_api_name||': l_step_status :'||l_step_status);
            END IF;

            IF l_step_status = gme_common_pvt.g_step_wip THEN
               l_remaining_tran_count := l_remaining_tran_count - 1; 
               l_usage := l_resource_txns.resource_usage - l_alloc_usage;

            -- Bug 8564008
	         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
               gme_debug.put_line(g_pkg_name||'.'||l_api_name||': l_remaining_tran_count :'||l_remaining_tran_count);
		         gme_debug.put_line(g_pkg_name||'.'||l_api_name||': l_resource_txns.resource_usage :'|| l_resource_txns.resource_usage);
		         gme_debug.put_line(g_pkg_name||'.'||l_api_name||': l_alloc_usage:'|| l_alloc_usage);
		         gme_debug.put_line(g_pkg_name||'.'||l_api_name||': l_usage :'|| l_usage);
            END IF;

            	-- nsinghi bug#5609683 
            /* 	Restructured the usage and strt date calculation based on following code Logic -- 
            
                    If there are multiple pending resource transactions with unequal quantities, 
                    when creating completed transactions with +ve qty, fetch the transactions in ascending 
                    order of resource usage. Deduct the usage from first transaction, if usage < 0, make the 
                    usage 0 and divide the remaining usage equally among the other remaining pending transactions.
            
                    If deleting an existing completed transaction (or reducing qty of existing completed trxn), 
                    increase the pending resource usage. If the start date calculated for pending rsrc txn based on
                    new usage < rsrc planned strt dt, determine the time difference between the dates and add it 
                    equally among the other pending resource txns. Txn completion date will never be greater than
            	rsrc planned completion date as we minus the rsrc usage from txn completion date and hence 
            	completion date remains unchanged.
            */
            
            IF l_usage < 0 THEN
               l_extra_usage_left := l_usage; 
               l_resource_txns.resource_usage := 0;
               l_resource_txns.start_date := l_resource_txns.end_date; 
               IF l_remaining_tran_count > 0 THEN
                  -- l_extra_usage_left would be negative so it should be subtracted from the alloc_usage
                  l_alloc_usage := l_alloc_usage - (l_extra_usage_left/l_remaining_tran_count);
               END IF;
            ELSIF l_usage = 0 THEN
               l_resource_txns.resource_usage := l_usage;

		         -- Bug 8564008
		         -- Since l_resource_txns.resource_usage is set to 0
               -- Hence setting the start date equal to end date
               --
               /*l_resource_txns.start_date := l_resource_txns.end_date
                     - (l_resource_txns.resource_usage / 24); */
               l_resource_txns.start_date := l_resource_txns.end_date;
            ELSIF l_usage > 0 THEN 
               l_resource_txns.resource_usage := l_usage;

               -- Bug 8564008
               IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
                  gme_debug.put_line(g_pkg_name||'.'||l_api_name||': l_usage > 0 processing ');
		            gme_debug.put_line(g_pkg_name||'.'||l_api_name||': l_resource_txns.resource_usage :' || l_resource_txns.resource_usage);
		            gme_debug.put_line(g_pkg_name||'.'||l_api_name||': l_resource_txns.end_date :'|| to_char(l_resource_txns.end_date, 'DD-MON-RRRR HH24:MI:SS'));
		            gme_debug.put_line(g_pkg_name||'.'||l_api_name||': l_resource_txns.TRANS_UM :'|| l_resource_txns.TRANS_UM);
		            gme_debug.put_line(g_pkg_name||'.'||l_api_name||': Modifying the usage using the hours conversion');
               END IF;

               -- Bug 8564008
               -- added the call to gme_create_step_pvt.get_usage_in_hours
               -- for converting the resource usage in hours
               /*l_resource_txns.start_date := l_resource_txns.end_date -
                    (l_resource_txns.resource_usage / 24);*/

		         gme_create_step_pvt.get_usage_in_hours (
		          p_plan_rsrc_usage => l_resource_txns.resource_usage,
		          p_usage_um        => l_resource_txns.TRANS_UM,
		          x_usage_hrs       => l_usage_hrs,
		          x_return_status   => l_usage_conv_ret_status
		         );

               IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
                  gme_debug.put_line(g_pkg_name||'.'||l_api_name||': Modified resource usage is :'||l_usage_hrs);
               END IF;

               -- 8564008
               -- Use new converted variable for date computation.
               l_resource_txns.start_date := l_resource_txns.end_date -
                    (l_usage_hrs / 24);                        
                    
               IF l_resource_txns.start_date < l_rsrc_plan_strt_dt THEN
                  l_extra_usage_left := ROUND(((l_rsrc_plan_strt_dt - l_resource_txns.start_date)*24), 5);
                  l_resource_txns.start_date := l_rsrc_plan_strt_dt;
                  
                  -- Bug 16297308 - Use usage hrs variable instead of usage, which is in days.
                  -- l_resource_txns.resource_usage := l_usage - l_extra_usage_left;
                  l_resource_txns.resource_usage := l_usage_hrs - l_extra_usage_left;
                  IF l_remaining_tran_count > 0 THEN
                     l_alloc_usage := l_alloc_usage - (l_extra_usage_left/l_remaining_tran_count);
                  END IF;
               END IF;
            END IF;

--               l_resource_txns.start_date :=
--                    l_resource_txns.end_date
--                  - (l_resource_txns.resource_usage / 24);

            gme_resource_engine_pvt.update_resource_trans
                                        (p_tran_rec           => l_resource_txns
                                        ,x_return_status      => l_return_status);

            IF l_return_status <> x_return_status THEN
               RAISE fnd_api.g_exc_error;
            END IF;
         ELSIF l_step_status > gme_common_pvt.g_step_wip THEN
               /* If the step is certified then their shouldn't be any further pending transactions */
               gme_resource_engine_pvt.delete_resource_trans
                                          (p_tran_rec           => l_resource_txns
                                          ,x_return_status      => l_return_status);

               IF l_return_status <> x_return_status THEN
                  RAISE fnd_api.g_exc_error;
               END IF;
            END IF;                                 /* IF l_step_status = 2 */
         END IF;                  /* IF l_resource_tab(i).completed_ind = 0 */
      END LOOP;                         /* FOR i IN 1..l_resource_tab.COUNT */

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;
   EXCEPTION
      WHEN fnd_api.g_exc_error THEN
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

         x_return_status := fnd_api.g_ret_sts_unexp_error;
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
   END reduce_pending_usage;


/*===========================================================================================
Procedure
  recalculate_charges
Description
  This particular procedure is used to recalculate charges when step qty is changed. And also used 
  to calculate if there is change in capacity by resource addition/deletion/updation.
Parameters
  p_batchstep_rec  Batch Step Line
  x_batchstep_rec  
  p_cal_type       'R' means change is due to Resource Add/Update/Delete
                   'P' means change is due to operation
  x_return_status  outcome of the API call
            S - Success
            E - Error
            U - Unexpected error
HISTORY
 SivakumarG Bug#5231180 
  Procedure Created
=============================================================================================*/
   PROCEDURE recalculate_charges( p_batchstep_rec IN  gme_batch_steps%ROWTYPE
                                 ,p_cal_type      IN  VARCHAR2
                                 ,x_batchstep_rec OUT  NOCOPY gme_batch_steps%ROWTYPE
                                 ,x_return_status OUT  NOCOPY VARCHAR2 ) 
   IS
    l_api_name                 VARCHAR2(30)  := 'RECALCULATE_CHARGES';

    CURSOR c_get_step_qty(v_step_id NUMBER) IS
     SELECT batchstep_id, step_status, plan_step_qty, actual_step_qty,
           max_step_capacity, max_step_capacity_um, step_qty_um
      FROM gme_batch_steps
     WHERE batchstep_id = v_step_id;
   
   CURSOR c_get_step_resources(v_step_id NUMBER) IS
    SELECT resources 
      FROM gme_batch_step_resources
     WHERE batchstep_id = v_step_id;

   l_step_rec                 c_get_step_qty%ROWTYPE;
   l_resource                 gme_batch_step_resources.resources%TYPE;
   l_batch_header_rec         gme_batch_header%ROWTYPE;
   l_in_step_qty              NUMBER;
   l_step_qty                 NUMBER;
   l_charge                   NUMBER;
   l_mass_qty                 NUMBER;
   l_vol_qty                  NUMBER;
   i                          NUMBER;
   l_max_capacity             NUMBER;
   l_return_status            VARCHAR2(1);
   l_calc_step_qty            BOOLEAN;
   l_uom                      VARCHAR2(3);
   l_gmd_resources            gmd_recipe_fetch_pub.oprn_resc_tbl;
   l_step_resources           gme_common_pvt.resources_tab;

   error_batchstep_id         EXCEPTION;
   error_in_calc_capacity     EXCEPTION;
   error_calc_charge          EXCEPTION;
   error_in_update_step       EXCEPTION;
   invalid_value              EXCEPTION;
   error_in_clear_charges     EXCEPTION;
BEGIN
   IF g_debug <= gme_debug.g_log_procedure THEN
    gme_debug.put_line ('Entering api ' || g_pkg_name || '.'|| l_api_name);
   END IF;

   x_return_status      := fnd_api.g_ret_sts_success;
   x_batchstep_rec      := p_batchstep_rec;

   IF p_batchstep_rec.batchstep_id IS NULL THEN
    /* give error over here */
    RAISE error_batchstep_id;   
   END IF;
   
   /* validate the calculation type */
   IF p_cal_type NOT IN ('R','P') THEN
    IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
      gme_debug.put_line('Invalid value for p_cal_type');
    END IF;
    RAISE invalid_value;
   END IF;
   
   OPEN c_get_step_qty(p_batchstep_rec.batchstep_id);
   FETCH c_get_step_qty INTO l_step_rec;
   CLOSE c_get_step_qty;

   IF l_step_rec.step_status > gme_common_pvt.g_step_pending THEN
     l_step_qty     := l_step_rec.actual_step_qty;
     l_in_step_qty  := x_batchstep_rec.actual_step_qty;
   ELSE
     l_step_qty     := l_step_rec.plan_step_qty;
     l_in_step_qty  := x_batchstep_rec.plan_step_qty;
   END IF;

   l_max_capacity   := l_step_rec.max_step_capacity;

   IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
    gme_debug.put_line('DB Step Qty: '||l_step_qty);
    gme_debug.put_line('Step Qty: '||l_in_step_qty);
    gme_debug.put_line('Max Capacity of the Step: '||l_max_capacity);
   END IF;

   IF p_cal_type = 'P' THEN
     /* if the cal type P that means step update is triggering recalculate charges*/
     IF NVL(l_step_qty,0) <> NVL(l_in_step_qty,0) THEN
        l_calc_step_qty := TRUE;
     ELSE
        l_calc_step_qty := FALSE;
     END IF;
   ELSIF p_cal_type = 'R' THEN
         --NVL(l_max_capacity,0) <> NVL(p_batchstep_rec.max_step_capacity,0) THEN
        l_calc_step_qty := TRUE;
   END IF;
   
   IF l_calc_step_qty THEN

     IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
       gme_debug.put_line('Calculating the Charges');
     END IF;    

     /*if we come here then there is change in the step qty and we have to recalculate charges 
        max step capacity uom and all */
     i := 1;
     OPEN c_get_step_resources(p_batchstep_rec.batchstep_id);
     LOOP
       FETCH c_get_step_resources INTO l_gmd_resources(i).resources;
       EXIT WHEN c_get_step_resources%NOTFOUND;
       i := i+1;
     END LOOP;
     CLOSE c_get_step_resources;

     gme_insert_step_pvt.calc_max_capacity (
       p_recipe_rout_resc   => l_gmd_resources              
      ,p_step_qty_uom       => l_step_rec.step_qty_um
      ,p_capacity_uom       => l_uom
      ,p_max_capacity       => x_batchstep_rec.max_step_capacity
      ,x_resource           => l_resource
      ,x_return_status      => l_return_status); 
   
    IF l_return_status <> fnd_api.g_ret_sts_success THEN
     IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
      gme_debug.put_line('Error in calculating the max capacity');
     END IF;
     RAISE error_in_calc_capacity;
    END IF;

    IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
      gme_debug.put_line('Calculated Max Step Qty: '||x_batchstep_rec.max_step_capacity);
      gme_debug.put_line('Capacity Resource : '||l_resource);
    END IF;

    /*clear the charges */
    /*DELETE FROM gme_batch_step_charges
    WHERE batchstep_id = l_step_rec.batchstep_id; */
    gme_batch_step_chg_pvt.clear_charges( 
         p_batch_id        => x_batchstep_rec.batch_id
        ,p_batchstep_id    => x_batchstep_rec.batchstep_id
        ,x_return_status   => l_return_status);

    IF l_return_status <> fnd_api.g_ret_sts_success THEN
      RAISE error_in_clear_charges;
    END IF;

    /*calculate mass qty and vol qty by calling routine */
    gme_update_step_qty_pvt.calculate_mass_vol_qty(
       p_batch_step_rec => x_batchstep_rec 
    );

    IF l_step_rec.step_status > gme_common_pvt.g_step_pending THEN
     l_mass_qty := x_batchstep_rec.actual_mass_qty;
     l_vol_qty  := x_batchstep_rec.actual_volume_qty;
    ELSE
     l_mass_qty := x_batchstep_rec.plan_mass_qty;
     l_vol_qty  := x_batchstep_rec.plan_volume_qty;
    END IF;

    IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
     gme_debug.put_line('Mass Qty: '||l_mass_qty||' Volume Qty: '||l_vol_qty);
    END IF;

    /* we have the max capacity and resource here */
    gme_update_step_qty_pvt.calc_charge (
       p_step_id      =>   l_step_rec.batchstep_id
      ,p_resources    =>   l_resource
      ,p_mass_qty     =>   l_mass_qty
      ,p_vol_qty      =>   l_vol_qty
      ,p_step_qty     =>   l_in_step_qty
      ,p_max_capacity =>   x_batchstep_rec.max_step_capacity
      ,x_charge       =>   l_charge
      ,x_return_status  => l_return_status); 

    IF l_return_status <> fnd_api.g_ret_sts_success THEN
     IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
      gme_debug.put_line('error in calc_charge');
     END IF;
     RAISE error_calc_charge;
    END IF;
  
    IF l_step_rec.step_status > gme_common_pvt.g_step_pending THEN
     x_batchstep_rec.actual_charges := l_charge;
    ELSE
     x_batchstep_rec.plan_charges := l_charge;
    END IF;
   END IF;  /*l_calc_step_qty */
   /* update the batch step record */
   IF NOT gme_batch_steps_dbl.update_row(p_batch_step => x_batchstep_rec) THEN    
     RAISE error_in_update_step;          
   END IF;

   IF g_debug <= gme_debug.g_log_procedure THEN
    gme_debug.put_line ('Existing api ' || g_pkg_name || '.'|| l_api_name);
   END IF;
EXCEPTION
  WHEN error_batchstep_id OR error_in_calc_capacity OR error_in_clear_charges THEN
    x_return_status := fnd_api.g_ret_sts_error;
  WHEN OTHERS THEN
    IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
     gme_debug.put_line(g_pkg_name || '.'|| l_api_name --> when others '
                       || SQLERRM);
    END IF;
    x_return_status := fnd_api.g_ret_sts_unexp_error;
END recalculate_charges;

END gme_update_step_qty_pvt;
/

COMMIT ;
EXIT;
