/* +======================================================================+ */
/* |    Copyright (c) 2005, 2013 Oracle and/or its affiliates.           | */
/* |                         All rights reserved.                         | */
/* |                           Version 12.0.0                             | */
/* +======================================================================+ */
REM dbdrv: sql ~PROD ~PATH ~FILE none none none package &phase=plb \
REM dbdrv: checkfile(120.5.12010000.4=120.5.12020000.4)(115.23=120.2):~PROD:~PATH:~FILE
SET verify off;
WHENEVER sqlerror exit failure rollback;
WHENEVER OSERROR EXIT FAILURE ROLLBACK;
REM **********************************************************************
REM *                                                                    *
REM * FILE:    GMEGNTRB.pls                                              *
REM * PURPOSE: Package Specification for the resource load graph (gantt) *
REM * AUTHOR:  Eddie Oumerretane                                         *
REM * HISTORY:                                                           *
REM * ========                                                           *
REM * 25FEB02  Eddie Oumerretane                                         *
REM *          Bug # 1919745 Implemented the new resource load summary   *
REM *          table GME_BATCH_STEP_RSRC_SUMMARY. Now resource load data *
REM *          is retrieved from this new table, replacing the           *
REM *          GME_BATCH_STEP_RESOURCES table.                           *
REM *                                                                    *
REM * 28APR02  Eddie Oumerretane                                         *
REM *          Bug # 2348024 Created function R_Pad and replaced         *
REM *          built-in RPAD by this new function everywhere.            *
REM * 04SEP02  Eddie Oumerretane                                         *
REM *          Bug # 2454797. Increased size of all VARCAHR2 used to     *
REM *          build records that are sent to the gantt by database size *
REM *          times 4 in order to accommodate multi-byte character set. *
REM * 10OCT02  Eddie Oumerretane.                                        *
REM *          Bug # 2565952 Replaced table GME_BATCH_STEP_RSRC_SUMMARY  *
REM *          with GME_RESOURCE_TXNS_SUMMARY table.                     *
REM * 19NOV02  Bharati Satpute                                           *
REM *          Bug # 2647652 nocopy changes                              *
REM * 05MAR03  Bharat Satpute Bug 2804440 Added WHEN OTHERS exception    *
REM *          which were not defined                                    *
REM * 12SEP03  Eddie Oumerretane. Added retrieval of enforce step        *
REM *          dependency in Get_Reschedule_Batch_List. This indicator   *
REM *          is used to perform velidations when rescheduling batches. *
REM * 27JUN06  SivakumarG Bug#5350537                                    *
REM *          Modified the procedure to select plan start date in all   *
REM *          cases. So the Reschedule window on resource load page will*
REM *          show plan start date                                      *
REM * 02NOV06  SivakumarG Bug#5550337                                    *
REM *          Code modified not to show phantom batches in reschedule   *
REM *          list invoked from resources page                          *
REM *02AUG13   Shaliu Chen Bug#17240118                                  *
REM *          Trigger on gme_resource_txns have been dropped due to     *
REM *          performance,we have to change cursor in procedure         *
REM *          fetch_resource_load to replace gme_resource_txn_summary   *
REM *          table with a query                                        *
REM **********************************************************************/

CREATE OR REPLACE PACKAGE BODY gme_gantt_rsrc_pkg AS
/* $Header: GMEGNTRB.pls 120.5.12020000.4 2013/08/02 07:33:15 shalchen ship $  */
   g_pkg_name   CONSTANT VARCHAR2 (30) := 'GME_GANTT_RSRC_PKG';

/*======================================================================
 # Return the given string padded on the right with blank characters.
 #======================================================================*/
   FUNCTION r_pad (p_str VARCHAR2, p_display_length INTEGER)
      RETURN VARCHAR2
   IS
      l_actual_length       INTEGER;
      l_api_name   CONSTANT VARCHAR2 (30) := 'R_PAD';
   BEGIN
      IF p_str IS NULL THEN
         RETURN RPAD (' ', p_display_length);
      END IF;

      l_actual_length := LENGTH (RPAD (p_str, p_display_length) );

      IF (l_actual_length < p_display_length) THEN
         l_actual_length :=
                      p_display_length
                      + (p_display_length - l_actual_length);
      END IF;

      RETURN RPAD (p_str, l_actual_length);
   --Bug2804440
   EXCEPTION
      WHEN OTHERS THEN
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
   --End Bug2804440
   END r_pad;

   /**
    * Select the available resources for the current organization and send them one by
    * one to the gantt
    */
   PROCEDURE get_available_plant_resources (
      p_organization_id   IN              NUMBER
     ,x_nb_resources      OUT NOCOPY      NUMBER
     ,x_plant_rsrc_tbl    OUT NOCOPY      plantresourcedetailtabletype)
   IS
      CURSOR get_plant_resources
      IS
         SELECT DISTINCT r.resources, r.resource_desc
                    FROM cr_rsrc_mst r, cr_rsrc_dtl d
                   WHERE d.organization_id = p_organization_id
                     AND d.delete_mark = 0
                     AND r.resources = d.resources
                     AND r.delete_mark = 0
                ORDER BY 1;

      resources             rescode;
      resource_desc         resdesc;
      l_nb_record           BINARY_INTEGER   := 0;
      l_record_ind          BINARY_INTEGER   := 1;
      l_resources_str       VARCHAR2 (64);
      l_resource_desc_str   CR_RSRC_MST_B.RESOURCE_DESC%TYPE; -- bug14480509
      l_rsrc_record         VARCHAR2 (500);-- bug	14693125 add enough size to store resource||resource_desc
      l_rsrc_record_tbl     VARCHAR2 (32000);
      l_api_name   CONSTANT VARCHAR2 (30)   := 'Get_Available_Plant_Resources';
   BEGIN
      x_nb_resources := 0;

      OPEN get_plant_resources;

      IF resources.EXISTS (1) THEN
         resources.DELETE;
      END IF;

      FETCH get_plant_resources
      BULK COLLECT INTO resources, resource_desc;

      IF resources.EXISTS (1) AND resources.COUNT > 0 THEN
         FOR e IN 1 .. resources.COUNT LOOP
            l_resources_str := resources (e);
            l_resource_desc_str := resource_desc (e);
            l_rsrc_record :=
                r_pad (l_resources_str, 16)
                || r_pad (l_resource_desc_str, 240); -- bug14480509

            IF l_nb_record = 0 THEN
               l_rsrc_record_tbl := l_rsrc_record;
               l_nb_record := l_nb_record + 1;
            ELSE
               l_rsrc_record_tbl := l_rsrc_record_tbl || l_rsrc_record;
               l_nb_record := l_nb_record + 1;

               IF l_nb_record >= 14 THEN
                  x_plant_rsrc_tbl (l_record_ind) := l_rsrc_record_tbl;
                  l_record_ind := l_record_ind + 1;
                  l_nb_record := 0;
               END IF;
            END IF;

            x_nb_resources := x_nb_resources + 1;
         END LOOP;

         IF l_nb_record > 0 THEN
            x_plant_rsrc_tbl (l_record_ind) := l_rsrc_record_tbl;
         END IF;
      END IF;

      CLOSE get_plant_resources;
   --Bug2804440
   EXCEPTION
      WHEN OTHERS THEN
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
   --End Bug2804440
   END get_available_plant_resources;

   /**
    * Fetch pending and WIP batches that consume the selected resource at the selected time.
    */
   PROCEDURE get_reschedule_batch_list (
      p_organization_id   IN              NUMBER
     ,p_resource          IN              VARCHAR2
     ,p_from_date         IN              DATE
     ,p_to_date           IN              DATE
     ,x_nb_batches        OUT NOCOPY      NUMBER
     ,x_resch_batch_tbl   OUT NOCOPY      reschbatchdetailtabletype)
   IS
      CURSOR get_product_info (p_batch_id NUMBER)
      IS
         SELECT i.inventory_item_id, i.concatenated_segments
           FROM mtl_system_items_kfv i   --Bug#5550337 changed from ic_item_mst
               ,gme_batch_header b
               ,gmd_recipe_validity_rules v
          WHERE b.batch_id = p_batch_id
            AND b.recipe_validity_rule_id = v.recipe_validity_rule_id
            AND v.inventory_item_id = i.inventory_item_id;

      batch_id                 batchid;
      batch_no                 batchno;
      batch_type               batchtype;
      batch_status             batchstatus;
      start_date               batchdate;
      plan_cmplt_date          batchdate;
      enforce_step_dep         enforcestepdep;
      l_nb_record              BINARY_INTEGER   := 0;
      l_record_ind             BINARY_INTEGER   := 1;
      l_item_no                VARCHAR (128);
      l_item_id                NUMBER;
      l_batch_id_str           VARCHAR2 (10);
      l_batch_status_str       VARCHAR2 (3);
      l_batch_type_str         VARCHAR2 (3);
      l_enforce_step_dep_str   VARCHAR2 (3);
      l_batch_no_str           VARCHAR2 (128);
      l_batch_record           VARCHAR2 (303);
      l_batch_record_tbl       VARCHAR2 (32000);
      l_api_name      CONSTANT VARCHAR2 (30)    := 'Get_Reschedule_Batch_List';

      -- Get all batches that are consuming the resource within the time period selected
      -- by the user.
      CURSOR get_batches_to_resch
      IS
         SELECT   b.batch_id, b.batch_no, b.batch_type, b.batch_status
	          /*Bug#5350537 We have to show planned start date in reschedule window irrespective of batch status
		    commented the following code and seletcing plan start date directly */
	          ,b.plan_start_date
                 /*,DECODE (b.batch_status
                         ,1, b.plan_start_date
                         ,b.actual_start_date) start_date */
                 ,b.plan_cmplt_date, NVL (b.enforce_step_dependency, 0)
             FROM gme_batch_header b
            WHERE b.organization_id = p_organization_id
              AND b.parentline_id IS NULL   --Bug#5550337 not showing phantom batches as we cant reschedule
              AND b.batch_status IN (1, 2)
              AND (    (    b.plan_start_date >= p_from_date
                        AND b.plan_start_date <= p_to_date)
                   OR (    b.plan_cmplt_date >= p_from_date
                       AND b.plan_start_date <= p_to_date) )
              AND EXISTS (
                     SELECT 1
                       FROM gme_batch_steps r, gme_batch_step_resources o
                      WHERE o.batch_id = b.batch_id
                        AND r.batch_id = o.batch_id
                        AND o.batchstep_id = r.batchstep_id
                        AND r.step_status IN (1, 2)
                        AND r.delete_mark = 0
                        AND o.resources = p_resource
                        AND (    (    o.plan_start_date >= p_from_date
                                  AND o.plan_start_date <= p_to_date)
                             OR (    o.plan_cmplt_date >= p_from_date
                                 AND o.plan_start_date <= p_to_date) ) )
         ORDER BY 4;
   BEGIN
      x_nb_batches := 0;

      OPEN get_batches_to_resch;

      IF batch_id.EXISTS (1) THEN
         batch_id.DELETE;
      END IF;

      FETCH get_batches_to_resch
      BULK COLLECT INTO batch_id, batch_no, batch_type, batch_status
            ,start_date, plan_cmplt_date, enforce_step_dep;

      IF batch_id.EXISTS (1) AND batch_id.COUNT > 0 THEN
         FOR e IN 1 .. batch_id.COUNT LOOP
            IF start_date (e) IS NOT NULL THEN
               l_batch_id_str := batch_id (e);
               l_batch_no_str := batch_no (e);
               l_batch_type_str := batch_type (e);
               l_batch_status_str := batch_status (e);
               l_enforce_step_dep_str := enforce_step_dep (e);

               OPEN get_product_info (batch_id (e) );

               FETCH get_product_info
                INTO l_item_id, l_item_no;

               CLOSE get_product_info;

               l_batch_record :=
                     LPAD (l_batch_id_str, 10, '0')
                  || r_pad (l_batch_no_str, 32)
                  || LPAD (l_batch_type_str, 3, '0')
                  || LPAD (l_batch_status_str, 3, '0')
                  || LPAD (TO_CHAR (start_date (e), 'YYYYMMDDHH24MISS')
                          ,14
                          ,'0')
                  || LPAD (TO_CHAR (plan_cmplt_date (e), 'YYYYMMDDHH24MISS')
                          ,14
                          ,'0')
                  || r_pad (l_item_no, 32)
                  || r_pad (l_enforce_step_dep_str, 3);

               IF l_nb_record = 0 THEN
                  l_batch_record_tbl := l_batch_record;
                  l_nb_record := l_nb_record + 1;
               ELSE
                  l_batch_record_tbl := l_batch_record_tbl || l_batch_record;
                  l_nb_record := l_nb_record + 1;

                  IF l_nb_record >= 100 THEN
                     x_resch_batch_tbl (l_record_ind) := l_batch_record_tbl;
                     l_record_ind := l_record_ind + 1;
                     l_nb_record := 0;
                  END IF;
               END IF;

               x_nb_batches := x_nb_batches + 1;
            END IF;
         END LOOP;

         IF l_nb_record > 0 THEN
            x_resch_batch_tbl (l_record_ind) := l_batch_record_tbl;
         END IF;
      END IF;

      CLOSE get_batches_to_resch;
   --Bug2804440
   EXCEPTION
      WHEN OTHERS THEN
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
   --End Bug2804440
   END get_reschedule_batch_list;

   /**
    * Retrieve the resource load data (qty available and scheduled usage)
    *
    * 25FEB02  Eddie Oumerretane
    *          Bug # 1919745 Implemented the new resource load summary
    *          table GME_BATCH_STEP_RSRC_SUMMARY. Now resource load data
    *          is retrieved from this new table, replacing the
    *          GME_BATCH_STEP_RESOURCES table.
    * 10OCT02  Eddie Oumerretane.
    *          Bug # 2565952 Replaced table GME_BATCH_STEP_RSRC_SUMMARY
    *          with GME_RESOURCE_TXNS_SUMMARY table.
    */
   PROCEDURE fetch_resource_load (
      p_resource_code       IN OUT NOCOPY  VARCHAR2
     ,p_organization_id     IN              NUMBER
     ,p_from_date           IN              DATE
     ,p_to_date             IN              DATE
     ,x_resource_desc       OUT NOCOPY      VARCHAR2
     ,x_resource_uom        OUT NOCOPY      VARCHAR2
     ,x_nb_load_interval    OUT NOCOPY      NUMBER
     ,x_nb_avail_interval   OUT NOCOPY      NUMBER
     ,x_rsrc_avail_tbl      OUT NOCOPY      resourceloadtabletype
     ,x_rsrc_load_tbl       OUT NOCOPY      resourceloadtabletype)
   IS
      from_date             resdate;
      end_DATE              resdate;
      rsrc_count            rescount;
      l_rsrc_avail_tbl      gmp_rsrc_avl_pkg.cal_tab2;
      l_resource_id         NUMBER (10);

      -- Get the total scheduled usage for the resource within the current organization
      --BUG 17240118 Shaliu Chen 02/08/2013
      --Replace gme_resource_txns_summary table with a query.
      CURSOR rsrc_load
      IS
/*         SELECT   start_date from_date, end_date end_DATE
                 ,SUM (required_units) rsrc_count
             FROM gme_resource_txns_summary
            WHERE resource_id = l_resource_id
              AND start_date >= p_from_date
              AND start_date <= p_to_date
         GROUP BY start_date, end_date
         UNION
         SELECT   start_date from_date, end_date end_DATE
                 ,SUM (required_units) rsrc_count
             FROM gme_resource_txns_summary
            WHERE resource_id = l_resource_id
              AND end_date >= p_from_date
              AND start_date <= p_to_date
         GROUP BY start_date, end_date
         ORDER BY 1;*/
         
        SELECT  rtxns.start_date from_date   
               ,rtxns.end_date end_date         
               ,COUNT(1) rsrc_count
          FROM  gme_resource_txns rtxns,
                cr_rsrc_dtl rsrc
         WHERE  rtxns.organization_id = rsrc.organization_id
           AND  rtxns.resources = rsrc.resources
           AND  rtxns.completed_ind = 0
           AND  rtxns.delete_mark = 0
           AND  rtxns.start_date < rtxns.end_date
           AND  rsrc.resource_id = l_resource_id
           AND  rtxns.start_date >= p_from_date
           AND  rtxns.start_date <= p_to_date
      GROUP BY  rtxns.start_date   
               ,rtxns.end_date               
         UNION
        SELECT  rtxns.start_date from_date   
               ,rtxns.end_date end_DATE         
               ,COUNT(1) rsrc_count
          FROM  gme_resource_txns rtxns,
                cr_rsrc_dtl rsrc
         WHERE  rtxns.organization_id = rsrc.organization_id
           AND  rtxns.resources = rsrc.resources
           AND  rtxns.completed_ind = 0
           AND  rtxns.delete_mark = 0
           AND  rtxns.start_date < rtxns.end_date
           AND  rsrc.resource_id = l_resource_id
           AND  rtxns.end_date >= p_from_date
           AND  rtxns.start_date <= p_to_date 
      GROUP BY  rtxns.start_date   
               ,rtxns.end_date;               
         
      --END BUG 17240118

      -- Get resource information
      CURSOR get_rsrc_info
      IS
         SELECT d.resource_id, m.std_usage_um, m.resource_desc
           FROM cr_rsrc_mst m, cr_rsrc_dtl d
          WHERE d.organization_id = p_organization_id
            AND d.resources = p_resource_code
            AND d.resources = m.resources;

      l_new_rsrc_ind        VARCHAR2 (1);
      l_start_date          DATE;
      l_end_date            DATE;
      l_assigned_qty        NUMBER;
      l_available           NUMBER;
      l_calendar_id         NUMBER (10);
      l_flag                VARCHAR2 (1);
      l_api_version         NUMBER                    := 1;
      l_init_msg_list       VARCHAR2 (1);
      l_return_status       VARCHAR2 (2);
      l_msg_count           NUMBER (10);
      l_msg_data            VARCHAR2 (2000);
      l_return_code         VARCHAR2 (1);
      l_rsrc_count_str      VARCHAR2 (10);
      l_resource_desc       CR_RSRC_MST_B.RESOURCE_DESC%TYPE; -- bug14480509
      l_resource_um         VARCHAR2 (16);
      l_resource_code       VARCHAR2 (64);
      l_record_ind          BINARY_INTEGER            := 1;
      l_nb_record           BINARY_INTEGER            := 0;
      l_rsrc_load_rec       VARCHAR2 (42);
      l_rsrc_load_rec_tbl   VARCHAR2 (32000);
      l_api_name   CONSTANT VARCHAR2 (30)             := 'fetch_resource_load';
   BEGIN
      x_nb_load_interval := 0;
      x_nb_avail_interval := 0;

      -- Get resource details
      OPEN get_rsrc_info;

      FETCH get_rsrc_info
       INTO l_resource_id, l_resource_um, l_resource_desc;

      IF (get_rsrc_info%FOUND) THEN
         CLOSE get_rsrc_info;

         x_resource_uom := r_pad (l_resource_um, 4);
         x_resource_desc := r_pad (l_resource_desc, 240); -- bug14480509
         l_resource_code := p_resource_code;
         p_resource_code := r_pad (l_resource_code, 16);

         OPEN rsrc_load;

         IF from_date.EXISTS (1) THEN
            from_date.DELETE;
         END IF;

         FETCH rsrc_load
         BULK COLLECT INTO from_date, end_DATE, rsrc_count;

         IF from_date.EXISTS (1) AND from_date.COUNT > 0 THEN
            FOR e IN 1 .. from_date.COUNT LOOP
               l_rsrc_count_str := rsrc_count (e);
               l_rsrc_load_rec :=
                     TO_CHAR (from_date (e), 'YYYYMMDDHH24MISS')
                  || TO_CHAR (end_DATE (e), 'YYYYMMDDHH24MISS')
                  || LPAD (l_rsrc_count_str, 10, '0');

               IF l_nb_record = 0 THEN
                  l_rsrc_load_rec_tbl := l_rsrc_load_rec;
                  l_nb_record := l_nb_record + 1;
               ELSE
                  l_rsrc_load_rec_tbl :=
                                       l_rsrc_load_rec_tbl || l_rsrc_load_rec;
                  l_nb_record := l_nb_record + 1;

                  IF l_nb_record >= 840 THEN
                     x_rsrc_load_tbl (l_record_ind) := l_rsrc_load_rec_tbl;
                     l_record_ind := l_record_ind + 1;
                     l_nb_record := 0;
                  END IF;
               END IF;

               x_nb_load_interval := x_nb_load_interval + 1;
            END LOOP;

            IF l_nb_record > 0 THEN
               x_rsrc_load_tbl (l_record_ind) := l_rsrc_load_rec_tbl;
            END IF;
         END IF;

         CLOSE rsrc_load;

         l_rsrc_load_rec := '';
         l_rsrc_load_rec_tbl := '';
         l_record_ind := 1;
         l_nb_record := 0;
         -- Retrieve the available intervals along with the number  of resources available
         gmp_rsrc_avl_pkg.rsrc_avl (p_api_version        => l_api_version
                                   ,p_init_msg_list      => l_init_msg_list
                                   ,p_resource_id        => l_resource_id
                                   ,p_from_date          => p_from_date
                                   ,p_to_date            => p_to_date
                                   ,x_return_status      => l_return_status
                                   ,x_msg_count          => l_msg_count
                                   ,x_msg_data           => l_msg_data
                                   ,x_return_code        => l_return_code
                                   ,p_rec                => l_rsrc_avail_tbl
                                   ,p_flag               => l_flag);

         FOR i IN 1 .. l_rsrc_avail_tbl.COUNT LOOP
            l_rsrc_count_str := l_rsrc_avail_tbl (i).out_resource_count;
            l_rsrc_load_rec :=
                  TO_CHAR (l_rsrc_avail_tbl (i).out_cal_from_date
                          ,'YYYYMMDDHH24MISS')
               || TO_CHAR (l_rsrc_avail_tbl (i).out_cal_to_date
                          ,'YYYYMMDDHH24MISS')
               || LPAD (l_rsrc_count_str, 10, '0');

            IF l_nb_record = 0 THEN
               l_rsrc_load_rec_tbl := l_rsrc_load_rec;
               l_nb_record := l_nb_record + 1;
            ELSE
               l_rsrc_load_rec_tbl := l_rsrc_load_rec_tbl || l_rsrc_load_rec;
               l_nb_record := l_nb_record + 1;

               IF l_nb_record >= 840 THEN
                  x_rsrc_avail_tbl (l_record_ind) := l_rsrc_load_rec_tbl;
                  l_record_ind := l_record_ind + 1;
                  l_nb_record := 0;
               END IF;
            END IF;

            x_nb_avail_interval := x_nb_avail_interval + 1;
         END LOOP;

         IF l_nb_record > 0 THEN
            x_rsrc_avail_tbl (l_record_ind) := l_rsrc_load_rec_tbl;
         END IF;
      ELSE
         CLOSE get_rsrc_info;
      END IF;
   --Bug2804440
   EXCEPTION
      WHEN OTHERS THEN
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
   --End Bug2804440
   END fetch_resource_load;
END;
/

COMMIT ;
EXIT;
