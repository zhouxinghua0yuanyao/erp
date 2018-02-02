/* +======================================================================+ */
/* |    Copyright (c) 2005, 2017 Oracle and/or its affiliates.           | */
/* |                         All rights reserved.                         | */
/* |                           Version 12.0.0                             | */
/* +======================================================================+ */
/*======================================================================
 # Eddie Oumerretane 21-MAR-02 Bug # 2189705. Retrieve the UOM from
 #                   the GME_MATERIAL_DETAILS in Get_Batch_Properties
 # Eddie Oumerretane 28-MAR-02 Bug # 2284342. Removed tests on start/end
 #                   dates when retrieving operations/activities and resources.
 # Eddie Oumerretane 10-APR-02 Bug # 2313926. Fix the test that ends the 
 #                   the retrieval of batches/FPOs when the pre-determined 
 #                   number of documents are loaded (Get_Next_Batch_Set).
 # Eddie Oumerretane 28-APR-02 Bug # 2348024. Created function R_Pad and replaced
 #                   built-in RPAD by this new function everywhere.
 # Eddie Oumerretane 04-SEP-02 Bug # 2547432. Increase size of all VARCHAR2 used
 #                   to build records that are sent to the gantt by database size * 4
 #                   in order to accommodate multi-byte character set.
 # Bharati Satpute   19NOV02  Bug # 2647652 nocopy changes
 # Eddie Oumerretane 23-JAN-03 Bug 2373154. Added product information in the
 #                   batch/FPO node.
 # Eddie Oumerretane 03-FEB-03 Bug 2303474. Added retrieval of operation
 #                   version in Get_Operations procedure.
 #                   Added retrieval of recipe information in Get_Batch_Properties 
 # Bharati Satpute   03-MAR-03 Bug 2804440 Added WHEN OTHERS exception which were not
 #                   defined 
 # Eddie Oumerretane 07-MAR-03 Bug # 2831884. Remove tests on start/end dates in
 #                   Get_Next_Batch_Set procedure in order to allow WIP batches
 #                   with plan end date < actual start date to display in the tree.
 # Eddie Oumerretane 04-SEP-2003. Created the following procedures for 
 #                   shop calendar support: 
 #                      - Fetch_Shop_Calendar (retrieve the calendar associated 
 #                        to the current plant)
 #                      - Fetch_Work_Non_Work_Days (retrieve working and non
 #                        working days to display in the time browser of the
 #                        gantt chart)
 #                      - Validate_Reschedule_Event (validate start and/or end
 #                        date of step/batch before rescheduling) 
 # Bharati Satpute  21-JAN-2003. Created the function date_to_clientDT for 
 #                               timezone support.
 # Bharati Satpute  27-July-2004 Bug3746919. Replaced the NVL insted of DECODE
 #                       in the get_operation_steps cursor.
 #                      - Added terminated ind to batch and steps
 # Shrikant Nene    20-AUG-2004  Bug3746919
 #                  Changed BatchTermTab to BatchTypeTab in get_operations procedure
 # Shrikant Nene    15-DEC-2004  Bug4068469
 #                  Removed trunc from the set_token call in following procedures
 #                   Fetch_Shop_Calendar;
 #                   Validate_Reschedule_Event;
 # Swapna Kommineni 02-FEB-2005  Bug2479773
 #                   Added Finite_scheduled_ind field also in the fetching of batch details.
 # Swapna Kommineni 17-MAR-2005  Bug2479773 Rework
 #                   Actual start date is sent as planned start date in case it is null.
 # SivakumarG 05-APR-2006 Bug#4867640
 #                  Code modified to fetch batches with in the batch range entered in the form
 #                  p_to_batch_no parameter added to init_session procedure
 # SivakumarG 02-NOV-2006 Bug#5550337
 #                  Code modified to fetch the parentline_id for phantom batches and this is used
 #                  in gantt chart to determine whether the batch is phantom or not.
 # Swapna K   27-AUG-2007 Bug:6375968 
 #                  Initialized variables in the beginning of the procedure Get_Next_Batch_Set.
 #                  6375968 Rework Cahnged l_item_id to l_inventory_item_id
 #                  International Calendar Support 
 # Abhay  25-Mar-2010 Changed fnd_date.date_to_displaydt(p_end_date)
 #                  to fnd_date.date_to_displaydt(p_end_date)
 # Archana Mundhe 29-Oct-2013 Bug 17495349 
 #                   Modified function filter_batch_item to fetch data for specific organization_id 
 #                   to improve performance. 
 # Shaliu Chen 23-Apr-2015 ER 20938455 
 #                   Modify procedure - init_session to add condition to exclude the batch 
 #                   which is on hold.
 
 #==============================================================================================*/
REM dbdrv: sql ~PROD ~PATH ~FILE none none none package &phase=plb \
REM dbdrv: checkfile(120.8.12010000.12=120.11.12020000.9)(120.4.12000000.10=120.8.12010000.7)(120.4.12000000.9=120.8.12010000.6)(120.4.12000000.5=120.8.12010000.2)(115.41=120.2):~PROD:~PATH:~FILE
SET verify off;
WHENEVER sqlerror exit failure rollback;
WHENEVER OSERROR EXIT FAILURE ROLLBACK;


CREATE OR REPLACE PACKAGE BODY gme_gantt_pkg AS
/* $Header: GMEGNTB.pls 120.11.12020000.9 2017/02/08 06:15:34 pecheng ship $  */
   g_pkg_name   CONSTANT VARCHAR2 (30)  := 'GME_GANTT_PKG';
   g_debug               VARCHAR2 (5)   := fnd_profile.VALUE ('AFLOG_LEVEL');
   g_bulk_fetch_limit    NUMBER         := 45;
   g_product_no          VARCHAR2 (32);
   g_ingredient_no       VARCHAR2 (32);
   g_org_code            VARCHAR2(3);
   g_organization_id     NUMBER (10);
   g_mfg_calendar_code   VARCHAR2 (10);
   g_calendar_desc       VARCHAR2 (240);
   g_calendar_start      DATE;
   g_calendar_end        DATE;


   CURSOR Get_Batches_From_Date IS
     Select DISTINCT
        BATCH_ID               ,
        BATCH_NO               ,
        BATCH_STATUS           ,
        BATCH_TYPE             ,
        START_DATE             ,
        END_DATE               ,
        PLAN_START_DATE        ,
        PLAN_CMPLT_DATE        ,
        ACTUAL_START           ,
        ACTUAL_END             ,
        FORMULA_ID             ,
        ROUTING_ID             ,
        ENFORCE_STEP_DEPENDENCY,
        TERMINATED_IND         ,
        FINITE_SCHEDULED_IND   ,
        PARENTLINE_ID
     from GME_GANTT_GTMP
     order by START_DATE;

--   CURSOR get_batches_from_date (
--      p_organization_id    NUMBER
--     ,p_batch_no           VARCHAR2  
--     ,p_to_batch_no        VARCHAR2   --Bug#4867640
--     ,p_batch_type         NUMBER
--     ,p_pending_status     NUMBER
--     ,p_released_status    NUMBER
--     ,p_certified_status   NUMBER
--     ,p_fpo_type           NUMBER
--     ,p_from_date          DATE
--     ,p_resource           VARCHAR2
--     ,p_prim_rsrc_ind      NUMBER
--     ,p_sec_rsrc_ind       NUMBER
--     ,p_aux_rsrc_ind       NUMBER)
--   IS
--      SELECT   b.batch_id, b.batch_no, batch_status, batch_type
--              ,
--               --Bug3315440
--               DECODE (batch_status
--                      ,1, date_to_clientdt (plan_start_date)
--                      ,date_to_clientdt (actual_start_date) ) start_date
--              ,DECODE (batch_status
--                      ,3, date_to_clientdt (actual_cmplt_date)
--                      ,date_to_clientdt (plan_cmplt_date) ) end_date
--              ,date_to_clientdt (plan_start_date)
--              ,date_to_clientdt (plan_cmplt_date)
--              ,DECODE (date_to_clientdt (actual_start_date)
--                      ,NULL, date_to_clientdt (plan_start_date)
--                      ,date_to_clientdt (actual_start_date) ) actual_start
--              ,DECODE (date_to_clientdt (actual_cmplt_date)
--                      ,NULL, date_to_clientdt (plan_cmplt_date)
--                      ,date_to_clientdt (actual_cmplt_date) ) actual_end
--              ,NVL(b.formula_id,0), NVL (routing_id, 0)
--              ,NVL (enforce_step_dependency, 0), NVL (terminated_ind, 0)
--              ,
--               --Bug#2479773 Swapna Kommineni 02-FEB-2005
--               NVL (finite_scheduled_ind, 0),
--               NVL(parentline_id,0)  --Bug#5550337
--          FROM gme_batch_header b
--         WHERE organization_id = p_organization_id
--   	   --Bug#4867640 check for batch_no within the given range
--           AND ((p_to_batch_no IS NULL AND batch_no LIKE p_batch_no) OR
--	        (lpad(batch_no,32,'0') >= lpad(p_batch_no,32,'0') AND
--		 lpad(batch_no,32,'0') <= lpad(p_to_batch_no,32,'0') )
--	       )
--           AND (    (    batch_type = p_batch_type
--                     AND (   batch_status = p_pending_status
--                          OR batch_status = p_released_status
--                          OR batch_status = p_certified_status) )
--                OR (batch_type = p_fpo_type AND batch_status = 1) )
--           AND (    (    batch_status = 1
--                     AND (   plan_start_date >= p_from_date
--                          OR plan_cmplt_date >= p_from_date) )
--                OR (    batch_status = 2
--                    AND (   actual_start_date >= p_from_date
--                         OR plan_cmplt_date >= p_from_date) )
--                OR (    batch_status = 3
--                    AND (   actual_start_date >= p_from_date
--                         OR actual_cmplt_date >= p_from_date) ) )
--           AND (    (    p_resource = '%'
--                     AND p_prim_rsrc_ind = 1
--                     AND p_aux_rsrc_ind = 2
--                     AND p_sec_rsrc_ind = 0)
--                OR (EXISTS (
--                       SELECT 1
--                         FROM gme_batch_step_resources d2
--                        WHERE d2.batch_id = b.batch_id
--                          AND d2.resources LIKE p_resource
--                          AND (   d2.prim_rsrc_ind = p_prim_rsrc_ind
--                               OR d2.prim_rsrc_ind = p_aux_rsrc_ind
--                               OR d2.prim_rsrc_ind = p_sec_rsrc_ind) ) ) )
--      ORDER BY 5;

   CURSOR GET_BATCHES_FROM_TO_DATE IS
     Select DISTINCT
        BATCH_ID               ,
        BATCH_NO               ,
        BATCH_STATUS           ,
        BATCH_TYPE             ,
        START_DATE             ,
        END_DATE               ,
        PLAN_START_DATE        ,
        PLAN_CMPLT_DATE        ,
        ACTUAL_START           ,
        ACTUAL_END             ,
        FORMULA_ID             ,
        ROUTING_ID             ,
        ENFORCE_STEP_DEPENDENCY,
        TERMINATED_IND         ,
        FINITE_SCHEDULED_IND   ,
        PARENTLINE_ID
      from GME_GANTT_GTMP
      order by START_DATE;

--   CURSOR get_batches_from_to_date (
--      p_organization_id    NUMBER
--     ,p_batch_no           VARCHAR2   
--     ,p_to_batch_no        VARCHAR2   --Bug#4867640
--     ,p_batch_type         NUMBER
--     ,p_pending_status     NUMBER
--     ,p_released_status    NUMBER
--     ,p_certified_status   NUMBER
--     ,p_fpo_type           NUMBER
--     ,p_from_date          DATE
--     ,p_to_date            DATE
--     ,p_resource           VARCHAR2
--     ,p_prim_rsrc_ind      NUMBER
--     ,p_sec_rsrc_ind       NUMBER
--     ,p_aux_rsrc_ind       NUMBER)
--   IS
--      SELECT   b.batch_id, batch_no, batch_status, batch_type
--              ,
--               --Bug3315440
--               DECODE (batch_status
--                      ,1, date_to_clientdt (plan_start_date)
--                      ,date_to_clientdt (actual_start_date) ) start_date
--              ,DECODE (batch_status
--                      ,3, date_to_clientdt (actual_cmplt_date)
--                      ,date_to_clientdt (plan_cmplt_date) ) end_date
--              ,date_to_clientdt (plan_start_date)
--              ,date_to_clientdt (plan_cmplt_date)
--              ,DECODE (date_to_clientdt (actual_start_date)
--                      ,NULL, date_to_clientdt (plan_start_date)
--                      ,date_to_clientdt (actual_start_date) ) actual_start
--              ,DECODE (date_to_clientdt (actual_cmplt_date)
--                      ,NULL, date_to_clientdt (plan_cmplt_date)
--                      ,date_to_clientdt (actual_cmplt_date) ) actual_end
--              ,NVL(formula_id,0), NVL (routing_id, 0)
--              ,NVL (enforce_step_dependency, 0), NVL (terminated_ind, 0)
--              ,
--               --Bug#2479773 Swapna Kommineni 02-FEB-2005
--               NVL (finite_scheduled_ind, 0),
--               NVL(parentline_id,0)  --Bug#5550337
--          FROM gme_batch_header b
--         WHERE organization_id = p_organization_id
--           AND ((p_to_batch_no IS NULL AND batch_no LIKE p_batch_no) OR
--	        (lpad(batch_no,32,'0') >= lpad(p_batch_no,32,'0') AND
--		 lpad(batch_no,32,'0') <= lpad(p_to_batch_no,32,'0') )
--	       )
--           AND (    (    batch_type = p_batch_type
--                     AND (   batch_status = p_pending_status
--                          OR batch_status = p_released_status
--                          OR batch_status = p_certified_status) )
--                OR (batch_type = p_fpo_type AND batch_status = 1) )
--           AND (    (    batch_status = 1
--                     AND (    (    plan_start_date >= p_from_date
--                               AND plan_start_date <= p_to_date)
--                          OR (    plan_cmplt_date >= p_from_date
--                              AND plan_start_date <= p_to_date) ) )
--                OR (    batch_status = 2
--                    AND (    (    actual_start_date >= p_from_date
--                              AND actual_start_date <= p_to_date)
--                         OR (    plan_cmplt_date >= p_from_date
--                             AND actual_start_date <= p_to_date) ) )
--                OR (    batch_status = 3
--                    AND (    (    actual_start_date >= p_from_date
--                              AND actual_start_date <= p_to_date)
--                         OR (    actual_cmplt_date >= p_from_date
--                             AND actual_start_date <= p_to_date) ) ) )
--           AND (    (    p_resource = '%'
--                     AND p_prim_rsrc_ind = 1
--                     AND p_aux_rsrc_ind = 2
--                     AND p_sec_rsrc_ind = 0)
--                OR (EXISTS (
--                       SELECT 1
--                         FROM gme_batch_step_resources d2
--                        WHERE d2.batch_id = b.batch_id
--                          AND d2.resources LIKE p_resource
--                          AND (   d2.prim_rsrc_ind = p_prim_rsrc_ind
--                               OR d2.prim_rsrc_ind = p_aux_rsrc_ind
--                               OR d2.prim_rsrc_ind = p_sec_rsrc_ind) ) ) )
--      ORDER BY 5;
      
      /* Moved these cursors to global space, as these are used in
         multiple procedures */ 
      /* Bug 21759414 Changed mtl_system_items_kfv to mtl_system_items_vl */
      CURSOR get_product_info (p_batch_id NUMBER)
      IS
         SELECT i.inventory_item_id, i.concatenated_segments, i.description
           FROM mtl_system_items_vl i
               ,gme_batch_header b
               ,gmd_recipe_validity_rules v
          WHERE b.batch_id = p_batch_id
            AND b.recipe_validity_rule_id = v.recipe_validity_rule_id
            AND v.inventory_item_id = i.inventory_item_id
            AND b.organization_id = i.organization_id;
      /* This cursor is defined for the LCF batch */
      CURSOR get_product_info_lcf (p_batch_id NUMBER)
      IS
         SELECT i.inventory_item_id, i.concatenated_segments, i.description
           FROM mtl_system_items_vl i
               ,gme_batch_header b
               ,gme_material_details m
          WHERE b.batch_id = p_batch_id
            AND b.batch_id = m.batch_id
            AND m.organization_id = i.organization_id
            AND m.inventory_item_id = i.inventory_item_id
            AND m.line_type = 1
            AND m.line_no = 1;

      CURSOR get_product_qty (p_batch_id NUMBER, p_inventory_item_id NUMBER)
      IS
         SELECT   SUM (plan_qty), NVL (SUM (actual_qty), 0), dtl_um
             FROM gme_material_details
            WHERE batch_id = p_batch_id
              AND line_type = 1
              AND inventory_item_id = p_inventory_item_id
         GROUP BY dtl_um;


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

   FUNCTION check_batch(p_batch_type  IN INTEGER ) RETURN BOOLEAN AS
   BEGIN
      IF p_batch_type = 0 THEN
         RETURN TRUE;
      ELSE
         RETURN FALSE;
      END IF;
   END check_batch;
   
   FUNCTION check_fpo(p_fpo_type  IN INTEGER ) RETURN BOOLEAN AS
   BEGIN
      IF p_fpo_type = 10 THEN
         RETURN TRUE;
      ELSE
         RETURN FALSE;
      END IF;
   END check_fpo;
   
   FUNCTION check_pending_status(p_pending_status  IN INTEGER ) RETURN BOOLEAN AS
   BEGIN
      IF p_pending_status = 1 THEN
         RETURN TRUE;
      ELSE
         RETURN FALSE;
      END IF;
   END check_pending_status;
   
   FUNCTION check_released_status(p_released_status  IN INTEGER ) RETURN BOOLEAN AS
   BEGIN
      IF p_released_status = 2 THEN
         RETURN TRUE;
      ELSE
         RETURN FALSE;
      END IF;
   END check_released_status;
   
   FUNCTION check_certified_status(p_certified_status  IN INTEGER ) RETURN BOOLEAN AS
   BEGIN
      IF p_certified_status = 3 THEN
         RETURN TRUE;
      ELSE
         RETURN FALSE;
      END IF;
   END check_certified_status;
   
   PROCEDURE set_where_clause(p_batch_type         IN INTEGER ,
                              p_fpo_type           IN INTEGER,
                              p_pending_status     IN INTEGER,
                              p_released_status    IN INTEGER,
                              p_certified_status   IN INTEGER,
                              p_from_date          IN DATE,
                              p_to_date            IN DATE DEFAULT NULL,
                              p_to_batch_no        IN VARCHAR2 DEFAULT NULL,
                              p_string_to_exec     IN OUT NOCOPY DBMS_SQL.VARCHAR2S) AS

      l_query_count   NUMBER := 0;

      l_column_select_str1   VARCHAR2(200) :=  ' SELECT B.BATCH_ID BATCH_ID, B.BATCH_NO BATCH_NO, BATCH_STATUS, BATCH_TYPE, PLAN_START_DATE, ACTUAL_START_DATE, ';
      l_column_select_str2   VARCHAR2(200) :=  ' ACTUAL_CMPLT_DATE, PLAN_CMPLT_DATE, B.FORMULA_ID FORMULA_ID, NVL(ROUTING_ID, 0) ROUTING_ID, NVL(ENFORCE_STEP_DEPENDENCY, 0) ENFORCE_STEP_DEPENDENCY, ';
      l_column_select_str3   VARCHAR2(200) :=  ' NVL(TERMINATED_IND, 0) TERMINATED_IND , NVL(FINITE_SCHEDULED_IND, 0) FINITE_SCHEDULED_IND, NVL(PARENTLINE_ID,0) PARENTLINE_ID FROM GME_BATCH_HEADER B ';
      l_column_select_str4   VARCHAR2(200) :=  ' WHERE organization_id = :p_organization_id AND ';
      l_column_select_str5   VARCHAR2(200) :=  NULL;
   BEGIN
      IF p_to_batch_no IS NULL THEN
          l_column_select_str5 := ' BATCH_NO LIKE :p_batch_no ';
      ELSE
          l_column_select_str5 := ' (lpad(batch_no,32,''0'') >= lpad(:p_batch_no,32,''0'') AND lpad(batch_no,32,''0'') <= lpad(:p_to_batch_no,32,''0'')) ';
      END IF;

      IF p_to_date IS NULL THEN
         -- Process Batch
         IF check_batch(p_batch_type) THEN
             -- process pending status
              IF check_pending_status(p_pending_status) THEN
                 p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str1;
                 p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str2;
                 p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str3;
                 p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str4;
		 p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str5;
                 l_query_count := l_query_count + 1;
                 p_string_to_exec(p_string_to_exec.LAST+1) := ' AND BATCH_TYPE = 0 AND BATCH_STATUS = 1 AND PLAN_START_DATE >= :p_from_date ';
   
                 -- Add the plan_comp_date check here if the released status is not chosen
                 -- else add it later.
                 IF NOT check_released_status(p_released_status) THEN
                      p_string_to_exec(p_string_to_exec.LAST+1) := 'UNION ALL '|| l_column_select_str1;
                      p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str2;
                      p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str3;
                      p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str4;
		      p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str5;
                      l_query_count := l_query_count + 1;
                      p_string_to_exec(p_string_to_exec.LAST+1) := ' AND BATCH_TYPE = 0 AND BATCH_STATUS = 1 AND PLAN_CMPLT_DATE >= :p_from_date ';
                 END IF;
              END IF;
   
              -- process released status
              IF check_released_status(p_released_status) THEN
                 -- if pending status was not chosen
                 IF NOT check_pending_status(p_pending_status) THEN
                      p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str1;
                      p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str2;
                      p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str3;
                      p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str4;
		      p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str5;
                      l_query_count := l_query_count + 1;
                      p_string_to_exec(p_string_to_exec.LAST+1) := ' AND BATCH_TYPE = 0 AND BATCH_STATUS = 2 AND PLAN_CMPLT_DATE >= :p_from_date ';
                 ELSE
                      --pending status was also chosen
                      p_string_to_exec(p_string_to_exec.LAST+1) := 'UNION ALL '|| l_column_select_str1;
                      p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str2;
                      p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str3;
                      p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str4;
		      p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str5;
                      l_query_count := l_query_count + 1;
                      p_string_to_exec(p_string_to_exec.LAST+1) := ' AND BATCH_TYPE = 0 AND BATCH_STATUS IN (1,2) AND PLAN_CMPLT_DATE >= :p_from_date ';
                 END IF;
   
                 -- Add the actual start date check here if the certified status is not chosen
                 -- else add it later.
                 IF NOT check_certified_status(p_certified_status) THEN
                      p_string_to_exec(p_string_to_exec.LAST+1) := 'UNION ALL '|| l_column_select_str1;
                      p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str2;
                      p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str3;
                      p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str4;
		      p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str5;
                      l_query_count := l_query_count + 1;
                      p_string_to_exec(p_string_to_exec.LAST+1) := ' AND BATCH_TYPE = 0 AND BATCH_STATUS = 2 AND ACTUAL_START_DATE >= :p_from_date ';
                 END IF;
              END IF;
   
              -- process certified status
              IF check_certified_status(p_certified_status) THEN
                 -- if released status was not chosen
                 IF NOT check_released_status(p_released_status) THEN
                      -- no query so far
                      IF l_query_count = 0 THEN
                         p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str1;
                         p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str2;
                         p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str3;
                         p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str4;
			 p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str5;
                      -- prior query so add union all
                      ELSE
                         p_string_to_exec(p_string_to_exec.LAST+1) := 'UNION ALL '|| l_column_select_str1;
                         p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str2;
                         p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str3;
                         p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str4;
			 p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str5;
                      END IF;
   
                      l_query_count := l_query_count + 1;
                      p_string_to_exec(p_string_to_exec.LAST+1) := ' AND BATCH_TYPE = 0 AND BATCH_STATUS = 3 AND ACTUAL_START_DATE >= :p_from_date ';
                 ELSE
                      --released status was also chosen
                      p_string_to_exec(p_string_to_exec.LAST+1) := 'UNION ALL '|| l_column_select_str1;
                      p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str2;
                      p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str3;
                      p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str4;
		      p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str5;
                      l_query_count := l_query_count + 1;
                      p_string_to_exec(p_string_to_exec.LAST+1) := ' AND BATCH_TYPE = 0 AND BATCH_STATUS IN (2,3) AND ACTUAL_START_DATE >= :p_from_date ';
                 END IF;
   
                 -- Add the actual completion date check
                 p_string_to_exec(p_string_to_exec.LAST+1) := 'UNION ALL '|| l_column_select_str1;
                 p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str2;
                 p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str3;
                 p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str4;
		 p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str5;
                 l_query_count := l_query_count + 1;
                 p_string_to_exec(p_string_to_exec.LAST+1) := ' AND BATCH_TYPE = 0 AND BATCH_STATUS = 3 AND ACTUAL_CMPLT_DATE >= :p_from_date ';
              END IF;
         END IF;
   
         -- process FPO
         IF check_fpo(p_fpo_type) THEN
              IF l_query_count = 0 THEN
                 p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str1;
                 p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str2;
                 p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str3;
                 p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str4;
		 p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str5;
              ELSE
                 p_string_to_exec(p_string_to_exec.LAST+1) := 'UNION ALL '|| l_column_select_str1;
                 p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str2;
                 p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str3;
                 p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str4;
		 p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str5;
              END IF;
   
              p_string_to_exec(p_string_to_exec.LAST+1) := ' AND BATCH_TYPE = 10 AND BATCH_STATUS = 1 AND PLAN_START_DATE >= :p_from_date ';
   
              p_string_to_exec(p_string_to_exec.LAST+1) := 'UNION ALL '|| l_column_select_str1;
              p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str2;
              p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str3;
              p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str4;
	      p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str5;
   
              p_string_to_exec(p_string_to_exec.LAST+1) := ' AND BATCH_TYPE = 10 AND BATCH_STATUS = 1 AND PLAN_CMPLT_DATE >= :p_from_date ';
         END IF;
      ELSE
         -- Process Batch
         IF check_batch(p_batch_type) THEN
             -- process pending status
              IF check_pending_status(p_pending_status) THEN
                 p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str1;
                 p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str2;
                 p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str3;
                 p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str4;
		 p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str5;
   
                 l_query_count := l_query_count + 1;
                 p_string_to_exec(p_string_to_exec.LAST+1) := ' AND BATCH_TYPE = 0 AND BATCH_STATUS = 1 AND PLAN_START_DATE >= :p_from_date AND PLAN_START_DATE <= :p_to_date ';
   
                 p_string_to_exec(p_string_to_exec.LAST+1) := 'UNION ALL '|| l_column_select_str1;
                 p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str2;
                 p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str3;
                 p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str4;
		 p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str5;
   
                 l_query_count := l_query_count + 1;
                 p_string_to_exec(p_string_to_exec.LAST+1) := ' AND BATCH_TYPE = 0 AND BATCH_STATUS = 1 AND PLAN_CMPLT_DATE >= :p_from_date AND PLAN_START_DATE <= :p_to_date ';
              END IF;
   
              -- process released status
              IF check_released_status(p_released_status) THEN
                 -- if pending status was not chosen
                 IF l_query_count = 0 THEN
                    p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str1;
                    p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str2;
                    p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str3;
                    p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str4;
		    p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str5;
                 ELSE
                    --pending status was also chosen
                    p_string_to_exec(p_string_to_exec.LAST+1) := 'UNION ALL '|| l_column_select_str1;
                    p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str2;
                    p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str3;
                    p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str4;
		    p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str5;
                 END IF;
   
                 l_query_count := l_query_count + 1;
                 p_string_to_exec(p_string_to_exec.LAST+1) := ' AND BATCH_TYPE = 0 AND BATCH_STATUS = 2 AND PLAN_CMPLT_DATE >= :p_from_date AND ACTUAL_START_DATE <= :p_to_date ';
   
                 -- Add the actual start date check here if the certified status is not chosen
                 -- else add it later.
                 IF NOT check_certified_status(p_certified_status) THEN
                      p_string_to_exec(p_string_to_exec.LAST+1) := 'UNION ALL '|| l_column_select_str1;
                      p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str2;
                      p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str3;
                      p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str4;
		      p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str5;
                      l_query_count := l_query_count + 1;
                      p_string_to_exec(p_string_to_exec.LAST+1) := ' AND BATCH_TYPE = 0 AND BATCH_STATUS = 2 AND ACTUAL_START_DATE >= :p_from_date  AND ACTUAL_START_DATE <= :p_to_date ';
                 END IF;
              END IF;
   
              -- process certified status
              IF check_certified_status(p_certified_status) THEN
                 -- if released status was not chosen
                 IF NOT check_released_status(p_released_status) THEN
                      -- no query so far
                      IF l_query_count = 0 THEN
                         p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str1;
                         p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str2;
                         p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str3;
                         p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str4;
			 p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str5;
                      -- prior query so add union all
                      ELSE
                         p_string_to_exec(p_string_to_exec.LAST+1) := 'UNION ALL '|| l_column_select_str1;
                         p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str2;
                         p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str3;
                         p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str4;
			 p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str5;
                      END IF;
   
                      l_query_count := l_query_count + 1;
                      p_string_to_exec(p_string_to_exec.LAST+1) := ' AND BATCH_TYPE = 0 AND BATCH_STATUS = 3 AND ACTUAL_START_DATE >= :p_from_date  AND ACTUAL_START_DATE <= :p_to_date ';
                 ELSE
                      --released status was also chosen
                      p_string_to_exec(p_string_to_exec.LAST+1) := 'UNION ALL '|| l_column_select_str1;
                      p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str2;
                      p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str3;
                      p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str4;
		      p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str5;
   
                      l_query_count := l_query_count + 1;
                      p_string_to_exec(p_string_to_exec.LAST+1) := ' AND BATCH_TYPE = 0 AND BATCH_STATUS IN (2,3) AND ACTUAL_START_DATE >= :p_from_date  AND ACTUAL_START_DATE <= :p_to_date ';
                 END IF;
   
                 -- Add the actual completion date check
                 p_string_to_exec(p_string_to_exec.LAST+1) := 'UNION ALL '|| l_column_select_str1;
                 p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str2;
                 p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str3;
                 p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str4;
		 p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str5;
   
                 l_query_count := l_query_count + 1;
                 p_string_to_exec(p_string_to_exec.LAST+1) := ' AND BATCH_TYPE = 0 AND BATCH_STATUS = 3 AND ACTUAL_CMPLT_DATE >= :p_from_date  AND ACTUAL_START_DATE <= :p_to_date ';
              END IF;
         END IF;
   
         -- process FPO
         IF check_fpo(p_fpo_type) THEN
              IF l_query_count = 0 THEN
                 p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str1;
                 p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str2;
                 p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str3;
                 p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str4;
		 p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str5;
              ELSE
                 p_string_to_exec(p_string_to_exec.LAST+1) := 'UNION ALL '|| l_column_select_str1;
                 p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str2;
                 p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str3;
                 p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str4;
		 p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str5;
              END IF;
   
              p_string_to_exec(p_string_to_exec.LAST+1) := ' AND BATCH_TYPE = 10 AND BATCH_STATUS = 1 AND PLAN_START_DATE >= :p_from_date AND PLAN_START_DATE <= :p_to_date ';
   
              p_string_to_exec(p_string_to_exec.LAST+1) := 'UNION ALL '|| l_column_select_str1;
              p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str2;
              p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str3;
              p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str4;
	      p_string_to_exec(p_string_to_exec.LAST+1) := l_column_select_str5;
   
              p_string_to_exec(p_string_to_exec.LAST+1) := ' AND BATCH_TYPE = 10 AND BATCH_STATUS = 1 AND PLAN_CMPLT_DATE >= :p_from_date AND PLAN_START_DATE <= :p_to_date ';
         END IF;
      END IF;
   END set_where_clause;

   PROCEDURE init_session (
      p_organization_id    IN   NUMBER
     ,p_org_code           IN   VARCHAR2
     ,p_batch_no           IN   VARCHAR2 
     ,p_to_batch_no        IN   VARCHAR2 --Bug#4867640
     ,p_from_date          IN   DATE
     ,p_to_date            IN   DATE
     ,p_resource           IN   VARCHAR2
     ,p_product_no         IN   VARCHAR2
     ,p_ingredient_no      IN   VARCHAR2
     ,p_prim_rsrc_ind      IN   INTEGER
     ,p_sec_rsrc_ind       IN   INTEGER
     ,p_aux_rsrc_ind       IN   INTEGER
     ,p_batch_type         IN   INTEGER
     ,p_fpo_type           IN   INTEGER
     ,p_released_status    IN   INTEGER
     ,p_pending_status     IN   INTEGER
     ,p_certified_status   IN   INTEGER)
   IS
      l_api_name   CONSTANT VARCHAR2 (30) := 'INIT_SESSION';

      string_to_exec    DBMS_SQL.VARCHAR2S;
      dynamic_cur       PLS_INTEGER := dbms_sql.open_cursor;
      l_ret   NUMBER;
   BEGIN
      GME_DEBUG.LOG_INITIALIZE('ProdScheduler');
   
      IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
           gme_debug.put_line('Inside init session ');
      END IF;
      close_cursors;

      /* 
      IF p_to_date IS NULL THEN
         OPEN get_batches_from_date (p_organization_id
                                    ,p_batch_no   
				    ,p_to_batch_no     --Bug#4867640
                                    ,p_batch_type
                                    ,p_pending_status
                                    ,p_released_status
                                    ,p_certified_status
                                    ,p_fpo_type
                                    ,p_from_date
                                    ,p_resource
                                    ,p_prim_rsrc_ind
                                    ,p_sec_rsrc_ind
                                    ,p_aux_rsrc_ind);
      ELSE
         OPEN get_batches_from_to_date (p_organization_id
                                       ,p_batch_no   
				       ,p_to_batch_no   --Bug#4867640
                                       ,p_batch_type
                                       ,p_pending_status
                                       ,p_released_status
                                       ,p_certified_status
                                       ,p_fpo_type
                                       ,p_from_date
                                       ,p_to_date
                                       ,p_resource
                                       ,p_prim_rsrc_ind
                                       ,p_sec_rsrc_ind
                                       ,p_aux_rsrc_ind);
      END IF;
      */

      DELETE FROM GME_GANTT_GTMP;

      string_to_exec(1) := 'INSERT INTO GME_GANTT_GTMP ';
      string_to_exec(2) := '( SELECT BATCH_ID, BATCH_NO, BATCH_STATUS, BATCH_TYPE, ';
      string_to_exec(3) := ' DECODE(BATCH_STATUS,1, GME_GANTT_PKG.DATE_TO_CLIENTDT(PLAN_START_DATE), GME_GANTT_PKG.DATE_TO_CLIENTDT(ACTUAL_START_DATE)) START_DATE, ';
      string_to_exec(4) := ' DECODE(BATCH_STATUS,3, GME_GANTT_PKG.DATE_TO_CLIENTDT(ACTUAL_CMPLT_DATE), GME_GANTT_PKG.DATE_TO_CLIENTDT(PLAN_CMPLT_DATE)) END_DATE, ';
      string_to_exec(5) := ' GME_GANTT_PKG.DATE_TO_CLIENTDT(PLAN_START_DATE), GME_GANTT_PKG.DATE_TO_CLIENTDT(PLAN_CMPLT_DATE), ';
      string_to_exec(6) := ' DECODE(GME_GANTT_PKG.DATE_TO_CLIENTDT(ACTUAL_START_DATE), NULL, GME_GANTT_PKG.DATE_TO_CLIENTDT(PLAN_START_DATE), GME_GANTT_PKG.DATE_TO_CLIENTDT(ACTUAL_START_DATE)) ACTUAL_START, ';
      string_to_exec(7) := ' DECODE(GME_GANTT_PKG.DATE_TO_CLIENTDT(ACTUAL_CMPLT_DATE), NULL , GME_GANTT_PKG.DATE_TO_CLIENTDT(PLAN_CMPLT_DATE), GME_GANTT_PKG.DATE_TO_CLIENTDT(ACTUAL_CMPLT_DATE)) ACTUAL_END, ';
      string_to_exec(8) := ' FORMULA_ID,  ROUTING_ID, ENFORCE_STEP_DEPENDENCY, TERMINATED_IND, FINITE_SCHEDULED_IND, PARENTLINE_ID FROM (';

      IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
           gme_debug.put_line('Calling set_where_clause');
      END IF;

      set_where_clause(p_batch_type         => p_batch_type,
                       p_fpo_type           => p_fpo_type,
                       p_pending_status     => p_pending_status,
                       p_released_status    => p_released_status,
                       p_certified_status   => p_certified_status,
                       p_from_date          => p_from_date,
                       p_to_date            => p_to_date,
                       p_to_batch_no        => p_to_batch_no,
                       p_string_to_exec     => string_to_exec);

      IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
           gme_debug.put_line('After executing set_where_clause');
      END IF;

      string_to_exec(string_to_exec.LAST +1) := ' ) A  WHERE ( (:p_resource = ''%'' AND :p_prim_rsrc_ind  = 1 AND :p_aux_rsrc_ind      = 2 ';
      string_to_exec(string_to_exec.LAST +1) := ' AND :p_sec_rsrc_ind      = 0) OR (EXISTS (SELECT 1 FROM GME_BATCH_STEP_RESOURCES D2 ';
      string_to_exec(string_to_exec.LAST +1) := ' WHERE D2.BATCH_ID = A.BATCH_ID AND D2.RESOURCES LIKE :p_resource AND (D2.PRIM_RSRC_IND = ';
      string_to_exec(string_to_exec.LAST +1) := ' :p_prim_rsrc_ind OR D2.PRIM_RSRC_IND   = :p_aux_rsrc_ind OR D2.PRIM_RSRC_IND = :p_sec_rsrc_ind ))))';
      /* ER 20938455 23-Apr-2015 Shaliu Chen
         Add following condition to exclude the batch which is on hold
      */      
      string_to_exec(string_to_exec.LAST +1) := ' AND gme_common_pvt.get_batch_hold_status(a.batch_id) = ''R'')';

      IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
        gme_debug.put_line('Dynamic SQL query is ');
        gme_debug.put_line('===============================================================');
        for cntr in 1..string_to_exec.last loop
           gme_debug.put_line(string_to_exec(cntr));
        end loop;
        gme_debug.put_line('===============================================================');
      END IF;

      DBMS_SQL.PARSE(dynamic_cur, string_to_exec, string_to_exec.first, string_to_exec.last, TRUE, DBMS_SQL.NATIVE);

      IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
           gme_debug.put_line('After parsing dynamic SQL query');
      END IF;

      IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
           gme_debug.put_line('Bind variable values');
           gme_debug.put_line('-------------------------------------------');
           gme_debug.put_line('p_organization_id = '|| p_organization_id);
           gme_debug.put_line('p_batch_no   = '|| p_batch_no);
	   gme_debug.put_line('p_to_batch_no   = '|| p_to_batch_no);
           gme_debug.put_line('p_from_date  = '|| to_char(p_from_date, 'DD-MON-YYYY HH24:MI:SS'));
           gme_debug.put_line('p_resource   = '|| p_resource);
           gme_debug.put_line('p_prim_rsrc_ind = '|| p_prim_rsrc_ind);
           gme_debug.put_line('p_aux_rsrc_ind  = '|| p_aux_rsrc_ind);
           gme_debug.put_line('p_sec_rsrc_ind  = '|| p_sec_rsrc_ind);
      END IF;

      DBMS_SQL.BIND_VARIABLE(dynamic_cur, ':p_organization_id'  , p_organization_id);
      DBMS_SQL.BIND_VARIABLE(dynamic_cur, ':p_batch_no'         , p_batch_no);
      IF  p_to_batch_no IS NOT NULL THEN
          DBMS_SQL.BIND_VARIABLE(dynamic_cur, ':p_to_batch_no'      , p_to_batch_no);
      END IF;
      DBMS_SQL.BIND_VARIABLE(dynamic_cur, ':p_from_date'        , p_from_date);
      DBMS_SQL.BIND_VARIABLE(dynamic_cur, ':p_resource'         , p_resource);
      DBMS_SQL.BIND_VARIABLE(dynamic_cur, ':p_prim_rsrc_ind'    , p_prim_rsrc_ind);
      DBMS_SQL.BIND_VARIABLE(dynamic_cur, ':p_aux_rsrc_ind'     , p_aux_rsrc_ind);
      DBMS_SQL.BIND_VARIABLE(dynamic_cur, ':p_sec_rsrc_ind'     , p_sec_rsrc_ind);

      IF p_to_date IS NOT NULL THEN
         IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
              gme_debug.put_line('p_to_date    = '|| to_char(p_to_date, 'DD-MON-YYYY HH24:MI:SS'));
         END IF;

         DBMS_SQL.BIND_VARIABLE(dynamic_cur, ':p_to_date', p_to_date);
      END IF;

      IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
           gme_debug.put_line('-------------------------------------------');
           gme_debug.put_line('After binding variables');
      END IF;

      IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
           gme_debug.put_line('Executing the dynamic SQL');
      END IF;

      l_ret := dbms_sql.execute(dynamic_cur);

      IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
           gme_debug.put_line('After executing dynamic SQL rows returned := '||l_ret);
      END IF;

      IF p_to_date IS NULL THEN
       OPEN Get_Batches_From_Date;
      ELSE
       OPEN Get_Batches_From_To_Date;
      END IF;

      g_product_no := p_product_no;
      g_ingredient_no := p_ingredient_no;
      g_organization_id := p_organization_id;
      g_org_code := p_org_code;
   EXCEPTION
      WHEN OTHERS THEN
         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line ('Exception occurs = ' || SQLERRM);
            fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
         END IF;
   END init_session;

/*======================================================================
 # Eddie Oumerretane 10-APR-02 Bug # 2313926. Fix the test that ends the
 #                   the retrieval of batches/FPOs when the pre-determined
 #                   number of documents are loaded (Get_Next_Batch_Set).
 #======================================================================*/
   PROCEDURE get_next_batch_set (
      x_nb_batches             OUT NOCOPY   NUMBER
     ,x_start_of_first_batch   OUT NOCOPY   DATE
     ,x_batch_table            OUT NOCOPY   gantttableofbatch)
   IS
      l_product_type         NUMBER (1)       := 1;
      l_ingredient_type      NUMBER (1)       := -1;
      batch_id               batchidtab;
      batch_no               batchnotab;
      batch_status           batchstatustab;
      batch_type             batchtypetab;
      start_date             batchdatetab;
      end_date               batchdatetab;
      plan_start_date        batchdatetab;
      plan_cmplt_date        batchdatetab;
      actual_start           batchdatetab;
      actual_end             batchdatetab;
      formula_id             batchidtab;
      routing_id             batchidtab;
      item_no                itemnotab;
      item_um                batchumtab;
      plan_qty               batchqtytab;
      actual_qty             batchqtytab;
      enforce_step_dep       batchtypetab;
      --Bug3746919 bsatpute
      terminated_ind         batchtypetab;
      --Bug#2479773 Swapna Kommineni 02-FEB-2005
      finite_scheduled_ind   batchtypetab;
      parentline_id          batchidtab; --Bug#5550337
      l_nb_record            BINARY_INTEGER   := 0;
      l_record_ind           BINARY_INTEGER   := 1;
      l_batch_id_str         VARCHAR2 (32);
      l_parent_line_id_str   VARCHAR2 (32);  --Bug#5550337
      l_formula_id_str       VARCHAR2 (32);
      l_routing_id_str       VARCHAR2 (32);
      --l_batch_status_str   VARCHAR2(32);
      --l_batch_type_str     VARCHAR2(32);
      --l_enforce_step_dep_str   VARCHAR2(32);
      l_batch_no_str         VARCHAR2 (128);
      l_batch_record         VARCHAR2 (700);
      l_batch_record_tbl     VARCHAR2 (32000);
      l_continue             BOOLEAN;
      l_inventory_item_id    NUMBER (15);
      l_item_no              VARCHAR2 (128);
      l_item_desc            VARCHAR2 (280);
      l_item_um              VARCHAR2 (16);
      l_plan_qty             NUMBER;
      l_actual_qty           NUMBER;
      l_plan_qty_str         VARCHAR2 (40);
      l_actual_qty_str       VARCHAR2 (40);
      l_api_name    CONSTANT VARCHAR2 (30)    := 'GET_NEXT_BATCH_SET';

      /**
       * Check whether the given batch consumes or produce the given item based on the line type.
       */
      FUNCTION filter_batch_item (
         p_batch_id    NUMBER
        ,p_item_no     VARCHAR2
        ,p_line_type   NUMBER)
         RETURN BOOLEAN
      IS
         CURSOR get_ingredient
         IS
            SELECT COUNT (*)
              FROM mtl_system_items_kfv i, gme_material_details m
             WHERE m.batch_id = p_batch_id
               AND m.inventory_item_id = i.inventory_item_id
               AND m.line_type = -1
               AND m.organization_id = i.organization_id
               AND i.organization_id = g_organization_id -- Bug 17495349 
               AND i.concatenated_segments LIKE p_item_no;

         CURSOR get_product
         IS
            SELECT COUNT (*)
              FROM mtl_system_items_kfv i, gme_material_details m
             WHERE m.batch_id = p_batch_id
               AND m.inventory_item_id = i.inventory_item_id
               AND m.organization_id = i.organization_id
               AND m.line_type = 1
               AND i.organization_id = g_organization_id -- Bug 17495349 
               AND i.concatenated_segments LIKE p_item_no;

         l_dummy               NUMBER;
         l_return              BOOLEAN;
         l_api_name   CONSTANT VARCHAR2 (30) := 'FILTER_BATCH_ITEM';
      BEGIN
         l_return := TRUE;
         l_dummy := 0;

         IF p_line_type = 1 THEN
            OPEN get_product;

            FETCH get_product
             INTO l_dummy;

            IF (get_product%NOTFOUND OR l_dummy = 0) THEN
               l_return := FALSE;
            END IF;

            CLOSE get_product;
         ELSE
            OPEN get_ingredient;

            FETCH get_ingredient
             INTO l_dummy;

            IF (get_ingredient%NOTFOUND OR l_dummy = 0) THEN
               l_return := FALSE;
            END IF;

            CLOSE get_ingredient;
         END IF;

         RETURN l_return;
      --Bug2804440
      EXCEPTION
         WHEN OTHERS THEN
            fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
      --End Bug2804440
      END filter_batch_item;

      /**
       * Sub-program used to populate the batch detail table.
       */
      FUNCTION filter_batch (p_batch_id NUMBER)
         RETURN BOOLEAN
      IS
         l_is_batch_valid      BOOLEAN;
         l_api_name   CONSTANT VARCHAR2 (30) := 'FILTER_BATCH';
      BEGIN
         l_is_batch_valid := TRUE;

         IF g_ingredient_no <> '%' THEN
            -- Check whether this batch consumes the specified ingredient
            l_is_batch_valid :=
               filter_batch_item (p_batch_id
                                 ,g_ingredient_no
                                 ,l_ingredient_type);
         END IF;

         IF g_product_no <> '%' AND l_is_batch_valid THEN
            -- Check whether this batch produces the specified product
            l_is_batch_valid :=
                 filter_batch_item (p_batch_id, g_product_no, l_product_type);
         END IF;

         RETURN l_is_batch_valid;
      --Bug2804440
      EXCEPTION
         WHEN OTHERS THEN
            fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
      --End Bug2804440
      END filter_batch;
   BEGIN
      x_nb_batches := 0;
      x_start_of_first_batch := NULL;

/*   Bug: 6375968 Initialized the variables */  
     l_plan_qty := 0;
     l_actual_qty := 0;
     l_item_um := ' ';
     l_inventory_item_id   := 0;
     l_item_no := ' ';


      -- To date has not been entered in the Filter window
      IF get_batches_from_date%ISOPEN THEN
         l_continue := TRUE;

         WHILE (l_continue) LOOP
            IF batch_id.EXISTS (1) THEN
               batch_id.DELETE;
            END IF;

            FETCH get_batches_from_date
            BULK COLLECT INTO batch_id, batch_no, batch_status, batch_type
                  ,start_date, end_date, plan_start_date, plan_cmplt_date
                  ,actual_start, actual_end, formula_id, routing_id
                  ,enforce_step_dep,
                                    --bug3746919 bsatpute
                                    terminated_ind,
                                                   --Bug#2479773 Swapna Kommineni 02-FEB-2005
                                                   finite_scheduled_ind, parentline_id LIMIT g_bulk_fetch_limit;

            IF batch_id.EXISTS (1) AND batch_id.COUNT > 0 THEN
               FOR e IN 1 .. batch_id.COUNT LOOP
                  --- Eddie Oumerretane 07-MAR-03 Bug # 2831884.
                  --- Removed tests on start/end dates below in order to allow WIP batches
                  --- with plan end date < actual start date to display in the tree.
                  IF filter_batch (batch_id (e) ) THEN
                     OPEN get_product_info (batch_id (e) );

                     FETCH get_product_info
                      INTO l_inventory_item_id, l_item_no, l_item_desc;

                     IF get_product_info%NOTFOUND THEN
                        /* This must be an LCF batch */
                        OPEN get_product_info_lcf (batch_id (e) );

                        FETCH get_product_info_lcf
                         INTO l_inventory_item_id, l_item_no, l_item_desc;
                         
                        CLOSE get_product_info_lcf;
                     END IF;   
                        
                     CLOSE get_product_info;

                     OPEN get_product_qty (batch_id (e)
                                          ,l_inventory_item_id);

                     FETCH get_product_qty
                      INTO l_plan_qty, l_actual_qty, l_item_um;

                     CLOSE get_product_qty;

					--Bug 25202346 change decimal from 2 to 5--
                     l_plan_qty_str := ROUND (l_plan_qty, 5);
                     l_actual_qty_str := ROUND (l_actual_qty, 5);
                     l_batch_id_str := batch_id (e);
                     l_formula_id_str := formula_id (e);
                     l_routing_id_str := routing_id (e);
                     --l_batch_type_str   := batch_type(e);
                     l_batch_no_str := batch_no (e);
                     l_parent_line_id_str := parentline_id (e); --Bug#5550337
                     -- l_batch_status_str := batch_status(e);
                     -- l_enforce_step_dep_str := enforce_step_dep(e);
                     l_batch_record :=
                           LPAD (l_batch_id_str, 32, '0')
                        || r_pad (l_batch_no_str, 32)
                        || LPAD (batch_type (e), 5, '0')
                        || R_PAD (g_org_code, 32)
                        || LPAD (batch_status (e), 5, '0')
                        || LPAD (TO_CHAR (plan_start_date (e)
                                         ,'YYYYMMDDHH24MISS')
                                ,32
                                ,'0')
                        || LPAD (TO_CHAR (plan_cmplt_date (e)
                                         ,'YYYYMMDDHH24MISS')
                                ,32
                                ,'0')
                        || LPAD (TO_CHAR (actual_start (e)
                                         ,'YYYYMMDDHH24MISS')
                                ,32
                                ,'0')
                        || LPAD (TO_CHAR (actual_end (e), 'YYYYMMDDHH24MISS')
                                ,32
                                ,'0')
                        || LPAD (TO_CHAR (start_date (e), 'YYYYMMDDHH24MISS')
                                ,32
                                ,'0')
                        || LPAD (TO_CHAR (end_date (e), 'YYYYMMDDHH24MISS')
                                ,32
                                ,'0')
                        || LPAD (l_formula_id_str, 32, '0')
                        || LPAD (l_routing_id_str, 32, '0')
                        || r_pad (l_item_no, 100)
                        || LPAD (l_actual_qty_str, 40, '0')
                        || LPAD (l_plan_qty_str, 40, '0')
                        || r_pad (l_item_um, 4)
                        || LPAD (enforce_step_dep (e), 5, '0')
                        --Bug3746919 bsatpute
                        || LPAD (terminated_ind (e), 5, '0')
                        || LPAD (finite_scheduled_ind (e), 5, '0')
                        ||  LPAD (l_parent_line_id_str, 32, '0'); --Bug#5550337

                     IF l_nb_record = 0 THEN
                        IF x_start_of_first_batch IS NULL THEN
                           x_start_of_first_batch := start_date (e);
                        END IF;

                        l_batch_record_tbl := l_batch_record;
                        l_nb_record := l_nb_record + 1;
                     ELSE
                        l_batch_record_tbl :=
                                         l_batch_record_tbl || l_batch_record;
                        l_nb_record := l_nb_record + 1;

                        IF l_nb_record >= 40 THEN
                           x_batch_table (l_record_ind) := l_batch_record_tbl;
                           l_record_ind := l_record_ind + 1;
                           l_nb_record := 0;
                        END IF;
                     END IF;

                     x_nb_batches := x_nb_batches + 1;
                  END IF;
               END LOOP;
            END IF;

            IF    NOT batch_id.EXISTS (1)
               OR (   batch_id.COUNT <= x_nb_batches
                   OR batch_id.COUNT < g_bulk_fetch_limit) THEN
               -- All selected batches meet the filter criteria or this was the last
               -- set of batches. In this case, send the batches to the gantt.
               -- Otherwise, continue fetching ...
               l_continue := FALSE;
            END IF;
         END LOOP;

         IF l_nb_record > 0 THEN
            x_batch_table (l_record_ind) := l_batch_record_tbl;
         END IF;
      -- To date has been entered in the Filter window
      ELSIF get_batches_from_to_date%ISOPEN THEN
         l_continue := TRUE;

         WHILE (l_continue) LOOP
            IF batch_id.EXISTS (1) THEN
               batch_id.DELETE;
            END IF;

            FETCH get_batches_from_to_date
            BULK COLLECT INTO batch_id, batch_no, batch_status, batch_type
                  ,start_date, end_date, plan_start_date, plan_cmplt_date
                  ,actual_start, actual_end, formula_id, routing_id
                  ,enforce_step_dep,
                                    --bug3746919 bsatpute
                                    terminated_ind,
                                                   --Bug#2479773 Swapna Kommineni 02-FEB-2005
                                                   finite_scheduled_ind, parentline_id LIMIT g_bulk_fetch_limit;

            --Bug3746919
            IF batch_id.EXISTS (1) AND batch_id.COUNT > 0 THEN
               FOR e IN 1 .. batch_id.COUNT LOOP
                  IF filter_batch (batch_id (e) ) THEN
                     OPEN get_product_info (batch_id (e) );

                     FETCH get_product_info
                      INTO l_inventory_item_id, l_item_no, l_item_desc;

                     IF get_product_info%NOTFOUND THEN
                        FETCH get_product_info_lcf
                         INTO l_inventory_item_id, l_item_no, l_item_desc;
                         
                        CLOSE get_product_info_lcf;
                     END IF;   
                        
                     CLOSE get_product_info;

                     OPEN get_product_qty (batch_id (e)
                                          ,l_inventory_item_id);

                     FETCH get_product_qty
                      INTO l_plan_qty, l_actual_qty, l_item_um;

                     CLOSE get_product_qty;
--Bug 25202346 change decimal from 2 to 5--
                     l_plan_qty_str := ROUND (l_plan_qty, 5);
                     l_actual_qty_str := ROUND (l_actual_qty, 5);
                     l_batch_id_str := batch_id (e);
                     l_formula_id_str := formula_id (e);
                     l_routing_id_str := routing_id (e);
                     --l_batch_type_str   := batch_type(e);
                     l_batch_no_str := batch_no (e);
                     l_parent_line_id_str := parentline_id (e); --Bug#5550337
                     --l_batch_status_str := batch_status(e);
                     --l_enforce_step_dep_str := enforce_step_dep(e);
                     l_batch_record :=
                           LPAD (l_batch_id_str, 32, '0')
                        || r_pad (l_batch_no_str, 32)
                        || LPAD (batch_type (e), 5, '0')
                        || LPAD (g_organization_id, 32, '0')
                        || LPAD (batch_status (e), 5, '0')
                        || LPAD (TO_CHAR (plan_start_date (e)
                                         ,'YYYYMMDDHH24MISS')
                                ,32
                                ,'0')
                        || LPAD (TO_CHAR (plan_cmplt_date (e)
                                         ,'YYYYMMDDHH24MISS')
                                ,32
                                ,'0')
                        || LPAD (TO_CHAR (actual_start (e)
                                         ,'YYYYMMDDHH24MISS')
                                ,32
                                ,'0')
                        || LPAD (TO_CHAR (actual_end (e), 'YYYYMMDDHH24MISS')
                                ,32
                                ,'0')
                        || LPAD (TO_CHAR (start_date (e), 'YYYYMMDDHH24MISS')
                                ,32
                                ,'0')
                        || LPAD (TO_CHAR (end_date (e), 'YYYYMMDDHH24MISS')
                                ,32
                                ,'0')
                        || LPAD (l_formula_id_str, 32, '0')
                        || LPAD (l_routing_id_str, 32, '0')
                        || r_pad (l_item_no, 100)
                        || LPAD (l_actual_qty_str, 40, '0')
                        || LPAD (l_plan_qty_str, 40, '0')
                        || r_pad (l_item_um, 4)
                        || LPAD (enforce_step_dep (e), 5, '0')
                        --bug3746919 bsatpute
                        || LPAD (terminated_ind (e), 5, '0')
                        || LPAD (finite_scheduled_ind (e), 5, '0')
                        ||  LPAD (l_parent_line_id_str, 32, '0'); --Bug#5550337

                     IF l_nb_record = 0 THEN
                        IF x_start_of_first_batch IS NULL THEN
                           x_start_of_first_batch := start_date (e);
                        END IF;

                        l_batch_record_tbl := l_batch_record;
                        l_nb_record := l_nb_record + 1;
                     ELSE
                        l_batch_record_tbl :=
                                         l_batch_record_tbl || l_batch_record;
                        l_nb_record := l_nb_record + 1;

                        IF l_nb_record >= 40 THEN
                           x_batch_table (l_record_ind) := l_batch_record_tbl;
                           l_record_ind := l_record_ind + 1;
                           l_nb_record := 0;
                        END IF;
                     END IF;

                     x_nb_batches := x_nb_batches + 1;
                  END IF;
               END LOOP;
            END IF;

            IF    NOT batch_id.EXISTS (1)
               OR (   batch_id.COUNT <= x_nb_batches
                   OR batch_id.COUNT < g_bulk_fetch_limit) THEN
               -- All selected batches meet the filter criteria or this was the last
               -- set of batches. In this case, send the batches to the gantt.
               -- Otherwise, continue fetching ...
               l_continue := FALSE;
            END IF;
         END LOOP;

         IF l_nb_record > 0 THEN
            x_batch_table (l_record_ind) := l_batch_record_tbl;
         END IF;
      END IF;
   EXCEPTION
      WHEN NO_DATA_FOUND THEN
         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line ('no data found ');
         END IF;
      --Bug2804440
      WHEN OTHERS THEN
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
         --End Bug2804440
         close_cursors;
   END get_next_batch_set;

/*======================================================================
 # Eddie Oumerretane 28-MAR-02 Bug # 2284342. Removed tests on start/end
 #                   dates when retrieving operations/activities and resources.
 # Bharati Satpute   Bug3746919. Replaced the NVL insted of DECODE
 #                       in the get_operation_steps cursor.
 #======================================================================*/
   PROCEDURE get_operations (
      p_batch_id           IN              NUMBER
     ,p_resource           IN              VARCHAR2
     ,p_prim_rsrc_ind      IN              NUMBER
     ,p_sec_rsrc_ind       IN              NUMBER
     ,p_aux_rsrc_ind       IN              NUMBER
     ,x_nb_operation       OUT NOCOPY      NUMBER
     ,x_operations_table   OUT NOCOPY      gantttableofoperation)
   IS
      start_date            batchdatetab;
      batchstep_no          batchstepnotab;
      step_status           batchstatustab;
      plan_step_qty         batchqtytab;
      actual_step_qty       batchqtytab;
      plan_start_date       batchdatetab;
      actual_start_date     batchdatetab;
      expct_cmplt_date      batchdatetab;
      actual_cmplt_date     batchdatetab;
      step_close_date       batchdatetab;
      oprn_id               batchidtab;
      oprn_no               batchnotab;
      oprn_vers             batchverstab;
      oprn_desc             batchdesctab;
      oper_class            batchclasstab;
      process_qty_um        batchumtab;
      end_date              batchdatetab;
      --bsatpute bug3746919
      terminated_ind        batchtypetab;

      CURSOR get_operation_steps
      IS
         SELECT DISTINCT
                         --Bug3746919 bsatpute
                         DECODE
                            (step_status
                            ,1, date_to_clientdt (r.plan_start_date)
                            ,5, date_to_clientdt (b.actual_start_date)
                            ,date_to_clientdt (r.actual_start_date) )
                                                                  start_date
                        --NVL(date_to_clientDT(r.actual_start_date),date_to_clientDT(r.plan_start_date))  start_date
                         ,r.batchstep_no, r.step_status, r.plan_step_qty
                        ,DECODE (r.actual_step_qty
                                ,NULL, 0
                                ,r.actual_step_qty) actual_step_qty
                        ,date_to_clientdt (r.plan_start_date)
                        ,NVL (date_to_clientdt (r.actual_start_date)
                             ,date_to_clientdt (r.plan_start_date) )
                        ,date_to_clientdt (r.plan_cmplt_date)
                        ,NVL (date_to_clientdt (r.actual_cmplt_date)
                             ,date_to_clientdt (r.plan_cmplt_date) )
                        ,NVL (date_to_clientdt (r.step_close_date)
                             ,date_to_clientdt (SYSDATE) )
                        ,o.oprn_id, o.oprn_no, NVL (o.oprn_vers, 0)
                        ,o.oprn_desc
                        ,DECODE (o.oprn_class
                                ,NULL, '?'
                                ,o.oprn_class) oper_class
                        ,o.process_qty_um
                        ,DECODE
                            (step_status
                            ,3, date_to_clientdt (r.actual_cmplt_date)
                            ,date_to_clientdt (r.plan_cmplt_date) ) end_date
                        --bug3746919 bsatpute
                         ,NVL (r.terminated_ind, 0)
                    FROM gme_batch_steps r
                        ,gme_batch_step_resources d
                        ,gmd_operations o
                        ,gme_batch_header b
                   WHERE r.batch_id = p_batch_id
                     AND r.batch_id = d.batch_id
                     AND r.batch_id = b.batch_id
                     AND r.oprn_id = o.oprn_id
                     AND r.batchstep_id = d.batchstep_id
                     AND d.resources LIKE p_resource
                     AND (   d.prim_rsrc_ind = p_prim_rsrc_ind
                          OR d.prim_rsrc_ind = p_aux_rsrc_ind
                          OR d.prim_rsrc_ind = p_sec_rsrc_ind)
                ORDER BY 1;

      CURSOR get_oper_class_desc (p_oper_class VARCHAR2)
      IS
         SELECT oprn_class_desc
           FROM gmd_operation_class
          WHERE oprn_class = p_oper_class;

      l_nb_record           BINARY_INTEGER   := 0;
      l_record_ind          BINARY_INTEGER   := 1;
      l_batch_id_str        VARCHAR2 (10);
      l_plan_qty_str        VARCHAR2 (40);
      l_actual_qty_str      VARCHAR2 (40);
      l_batchstep_no_str    VARCHAR2 (10);
      l_step_status_str     VARCHAR2 (3);
      l_oprn_id_str         VARCHAR2 (10);
      l_oprn_no_str         VARCHAR2 (64);
      ---l_oprn_vers_str     VARCHAR2(5);
      l_oprn_desc_str       VARCHAR2 (240);--bug14480509 
      l_class_str           VARCHAR2 (16);
      l_class_desc_str      VARCHAR2 (160);
      l_oper_record         VARCHAR2 (737);
      l_oper_record_tbl     VARCHAR2 (32000);
      l_api_name   CONSTANT VARCHAR2 (30)    := 'GET_OPERATIONS';
   BEGIN
      x_nb_operation := 0;

      OPEN get_operation_steps;

      IF oprn_id.EXISTS (1) THEN
         oprn_id.DELETE;
      END IF;

      FETCH get_operation_steps
      BULK COLLECT INTO start_date, batchstep_no, step_status, plan_step_qty
            ,actual_step_qty, plan_start_date, actual_start_date
            ,expct_cmplt_date, actual_cmplt_date, step_close_date, oprn_id
            ,oprn_no, oprn_vers, oprn_desc, oper_class, process_qty_um
            ,end_date,
                      --bug3746919 bsatpute
                      terminated_ind LIMIT g_bulk_fetch_limit;

      IF oprn_id.EXISTS (1) AND oprn_id.COUNT > 0 THEN
         FOR e IN 1 .. oprn_id.COUNT LOOP
            l_class_desc_str := '?';

            IF (oper_class (e) <> '?') THEN
               OPEN get_oper_class_desc (oper_class (e) );

               FETCH get_oper_class_desc
                INTO l_class_desc_str;

               CLOSE get_oper_class_desc;
            END IF;

            l_batch_id_str := p_batch_id;
            l_oprn_id_str := oprn_id (e);
            l_oprn_no_str := oprn_no (e);
            --- l_oprn_vers_str     := oprn_vers(e);
            l_oprn_desc_str := oprn_desc (e);
            l_class_str := oper_class (e);
            l_batchstep_no_str := batchstep_no (e);
            l_step_status_str := step_status (e);
			--Bug 25202346 change decimal from 2 to 5--
            l_plan_qty_str := ROUND (plan_step_qty (e), 5);
            l_actual_qty_str := ROUND (actual_step_qty (e), 5);
            l_oper_record :=
                  LPAD (l_batch_id_str, 10, '0')
               || LPAD (l_oprn_id_str, 10, '0')
               || r_pad (l_oprn_no_str, 16)
               || r_pad (l_oprn_desc_str, 240)--bug14480509 
               || r_pad (l_class_str, 4)
               || r_pad (l_class_desc_str, 40)
               || LPAD (l_batchstep_no_str, 10, '0')
               || LPAD (l_step_status_str, 3, '0')
               || LPAD (l_plan_qty_str, 40, '0')
               || LPAD (l_actual_qty_str, 40, '0')
               || r_pad (process_qty_um (e), 4)
               || TO_CHAR (plan_start_date (e), 'YYYYMMDDHH24MISS')
               || TO_CHAR (actual_start_date (e), 'YYYYMMDDHH24MISS')
               || TO_CHAR (expct_cmplt_date (e), 'YYYYMMDDHH24MISS')
               || TO_CHAR (actual_cmplt_date (e), 'YYYYMMDDHH24MISS')
               || TO_CHAR (step_close_date (e), 'YYYYMMDDHH24MISS')
               || TO_CHAR (start_date (e), 'YYYYMMDDHH24MISS')
               || TO_CHAR (end_date (e), 'YYYYMMDDHH24MISS')
               || LPAD (oprn_vers (e), 5, '0')
               --bsatpute bug3746919
               || LPAD (terminated_ind (e), 5, '0');

            IF l_nb_record = 0 THEN
               l_oper_record_tbl := l_oper_record;
               l_nb_record := l_nb_record + 1;
            ELSE
               l_oper_record_tbl := l_oper_record_tbl || l_oper_record;
               l_nb_record := l_nb_record + 1;

               IF l_nb_record >= 45 THEN
                  x_operations_table (l_record_ind) := l_oper_record_tbl;
                  l_record_ind := l_record_ind + 1;
                  l_nb_record := 0;
               END IF;
            END IF;

            x_nb_operation := x_nb_operation + 1;
         END LOOP;

         IF l_nb_record > 0 THEN
            x_operations_table (l_record_ind) := l_oper_record_tbl;
         END IF;
      END IF;

      CLOSE get_operation_steps;
   --Bug2804440
   EXCEPTION
      WHEN OTHERS THEN
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
   --End Bug2804440
   END get_operations;

/*======================================================================
 # Eddie Oumerretane 28-MAR-02 Bug # 2284342. Removed tests on start/end
 #                   dates when retrieving operations/activities and resources.
 #======================================================================*/
   PROCEDURE get_activities (
      p_batch_id         IN              NUMBER
     ,p_batchstep_no     IN              NUMBER
     ,p_resource         IN              VARCHAR2
     ,p_prim_rsrc_ind    IN              NUMBER
     ,p_sec_rsrc_ind     IN              NUMBER
     ,p_aux_rsrc_ind     IN              NUMBER
     ,x_nb_activity      OUT NOCOPY      NUMBER
     ,x_activity_table   OUT NOCOPY      gantttableofactivity)
   IS
      activity                activitycodetab;
      activity_desc           batchdesctab;
      cost_analysis_code      costancodetab;
      cost_analysis_desc      batchdesctab;
      start_date              batchdatetab;
      end_date                batchdatetab;

      CURSOR get_activities
      IS
         SELECT   a.activity, a.activity_desc, a.cost_analysis_code
                 ,c.cost_analysis_desc
                 ,MIN
                     (DECODE (r.step_status
                             ,1, date_to_clientdt (o.plan_start_date)
                             ,5, date_to_clientdt (b.actual_start_date)
                             ,date_to_clientdt (o.actual_start_date) ) )
                                                                  start_date
                 ,MAX
                     (DECODE (r.step_status
                             ,3, date_to_clientdt (o.actual_cmplt_date)
                             ,date_to_clientdt (o.plan_cmplt_date) ) )
                                                                    end_date
             FROM gme_batch_step_resources o
                 ,gme_batch_steps r
                 ,gme_batch_step_activities s
                 ,gmd_activities a
                 ,gme_batch_header b
                 ,cm_alys_mst c
            WHERE o.batch_id = p_batch_id
              AND r.batch_id = o.batch_id
              AND s.batch_id = r.batch_id
              AND b.batch_id = r.batch_id
              AND s.delete_mark = 0
              AND o.batchstep_activity_id = s.batchstep_activity_id
              AND r.batchstep_no = p_batchstep_no
              AND o.batchstep_id = r.batchstep_id
              AND s.activity = a.activity
              AND c.cost_analysis_code = a.cost_analysis_code
              AND o.resources LIKE p_resource
              AND (   o.prim_rsrc_ind = p_prim_rsrc_ind
                   OR o.prim_rsrc_ind = p_aux_rsrc_ind
                   OR o.prim_rsrc_ind = p_sec_rsrc_ind)
         GROUP BY a.activity
                 ,a.activity_desc
                 ,a.cost_analysis_code
                 ,c.cost_analysis_desc
         ORDER BY 5;

      l_nb_record             BINARY_INTEGER   := 0;
      l_record_ind            BINARY_INTEGER   := 1;
      l_batch_id_str          VARCHAR2 (10);
      l_batchstep_no_str      VARCHAR2 (10);
      l_activity_str          VARCHAR2 (64);
      l_activity_desc_str     VARCHAR2 (160);
      -- bug14480540
      l_cost_ana_code_str     CM_ALYS_MST.COST_ANALYSIS_CODE%TYPE;
      l_cost_ana_desc_str     CM_ALYS_MST.COST_ANALYSIS_DESC%TYPE;
      l_activity_record       VARCHAR2 (600);
      l_activity_record_tbl   VARCHAR2 (32000);
      l_api_name     CONSTANT VARCHAR2 (30)    := 'GET_ACTIVITIES';
   BEGIN
      x_nb_activity := 0;

      OPEN get_activities;

      IF activity.EXISTS (1) THEN
         activity.DELETE;
      END IF;

      FETCH get_activities
      BULK COLLECT INTO activity, activity_desc, cost_analysis_code
            ,cost_analysis_desc, start_date, end_date;

      IF activity.EXISTS (1) AND activity.COUNT > 0 THEN
         FOR e IN 1 .. activity.COUNT LOOP
            l_batch_id_str := p_batch_id;
            l_batchstep_no_str := p_batchstep_no;
            l_activity_str := activity (e);
            l_activity_desc_str := activity_desc (e);
            l_cost_ana_code_str := cost_analysis_code (e);
            l_cost_ana_desc_str := cost_analysis_desc (e);
            l_activity_record :=
                  LPAD (l_batch_id_str, 10, '0')
               || LPAD (l_batchstep_no_str, 10, '0')
               || r_pad (l_activity_str, 16)
               || r_pad (l_activity_desc_str, 40)
               || r_pad (l_cost_ana_code_str, 4)
               || r_pad (l_cost_ana_desc_str, 40)
               || TO_CHAR (start_date (e), 'YYYYMMDDHH24MISS')
               || TO_CHAR (end_date (e), 'YYYYMMDDHH24MISS');

            IF l_nb_record = 0 THEN
               l_activity_record_tbl := l_activity_record;
               l_nb_record := l_nb_record + 1;
            ELSE
               l_activity_record_tbl :=
                                   l_activity_record_tbl || l_activity_record;
               l_nb_record := l_nb_record + 1;

               IF l_nb_record >= 50 THEN
                  x_activity_table (l_record_ind) := l_activity_record_tbl;
                  l_record_ind := l_record_ind + 1;
                  l_nb_record := 0;
               END IF;
            END IF;

            x_nb_activity := x_nb_activity + 1;
         END LOOP;

         IF l_nb_record > 0 THEN
            x_activity_table (l_record_ind) := l_activity_record_tbl;
         END IF;
      END IF;

      CLOSE get_activities;
   --Bug2804440
   EXCEPTION
      WHEN OTHERS THEN
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
   --End Bug2804440
   END get_activities;

/*======================================================================
 # Eddie Oumerretane 28-MAR-02 Bug # 2284342. Removed tests on start/end
 #                   dates when retrieving operations/activities and resources.
 #======================================================================*/
   PROCEDURE get_resources (
      p_batch_id         IN              NUMBER
     ,p_batchstep_no     IN              NUMBER
     ,p_activity         IN              VARCHAR2
     ,p_resource         IN              VARCHAR2
     ,p_prim_rsrc_ind    IN              NUMBER
     ,p_sec_rsrc_ind     IN              NUMBER
     ,p_aux_rsrc_ind     IN              NUMBER
     ,x_nb_resource      OUT NOCOPY      NUMBER
     ,x_resource_table   OUT NOCOPY      gantttableofresource)
   IS
      resources                 batchresourcestab;
      resource_desc             batchdesctab;
      prim_rsrc_ind             batchintindtab;
      scale_type                batchtypetab;
      plan_rsrc_count           batchqtytab;
      actual_res_count          batchqtytab;
      plan_rsrc_qty             batchqtytab;
      actual_res_qty            batchqtytab;
      plan_rsrc_usage           batchqtytab;
      actual_res_usage          batchqtytab;
      std_usage_um              batchumtab;
      offset_interval           batchqtytab;
      start_date                batchdatetab;
      end_date                  batchdatetab;
      plan_start_date           batchdatetab;
      plan_cmplt_date           batchdatetab;
      actual_start_date         batchdatetab;
      actual_cmplt_date         batchdatetab;
      process_qty_um            batchumtab;
      step_status               batchstatustab;

      CURSOR get_resources
      IS
         SELECT   o.resources, c.resource_desc, o.prim_rsrc_ind
                 ,o.scale_type, NVL (o.plan_rsrc_count, 0)
                 ,DECODE (o.actual_rsrc_count
                         ,NULL, 0
                         ,o.actual_rsrc_count) actual_res_count
                 ,NVL (o.plan_rsrc_qty, 0)
                 ,DECODE (o.actual_rsrc_qty
                         ,NULL, 0
                         ,o.actual_rsrc_qty) actual_res_qty
                 ,NVL (o.plan_rsrc_usage, 0)
                 ,DECODE (o.actual_rsrc_usage
                         ,NULL, 0
                         ,o.actual_rsrc_usage) actual_res_usage
                 ,NVL (c.std_usage_um, ' '), NVL (o.offset_interval, 0)
                 --Swapna K Bug#2479773 17-MAR-2005 Rework
                  /* Null actual start date is sent as planned start date */
                  ,DECODE
                      (r.step_status
                      ,1, date_to_clientdt (o.plan_start_date)
                      ,5, date_to_clientdt (b.actual_start_date)
                      ,NVL (date_to_clientdt (o.actual_start_date)
                           ,date_to_clientdt (o.plan_start_date) ) )
                                                                  start_date
                 ,DECODE (r.step_status
                         ,3, date_to_clientdt (o.actual_cmplt_date)
                         ,date_to_clientdt (o.plan_cmplt_date) ) end_date
                 ,date_to_clientdt (o.plan_start_date)
                 ,date_to_clientdt (o.plan_cmplt_date)
                 ,NVL (date_to_clientdt (o.actual_start_date)
                      ,date_to_clientdt (o.plan_start_date) )
                 ,NVL (date_to_clientdt (o.actual_cmplt_date)
                      ,date_to_clientdt (o.plan_cmplt_date) )
                 ,NVL (f.process_qty_um, ' '), NVL (r.step_status, 0)
             FROM gme_batch_steps r
                 ,gme_batch_step_activities a
                 ,gme_batch_step_resources o
                 ,gmd_operations f
                 ,gme_batch_header b
                 ,cr_rsrc_mst c
            WHERE r.batch_id = p_batch_id
              AND b.batch_id = r.batch_id
              AND r.batchstep_no = p_batchstep_no
              AND a.batchstep_id = r.batchstep_id
              AND a.delete_mark = 0
              AND o.batchstep_id = a.batchstep_id
              AND o.batchstep_activity_id = a.batchstep_activity_id
              AND a.activity = p_activity
              AND f.oprn_id = r.oprn_id
              AND r.batch_id = o.batch_id
              AND o.resources = c.resources
              AND o.resources LIKE p_resource
              AND (   o.prim_rsrc_ind = p_prim_rsrc_ind
                   OR o.prim_rsrc_ind = p_aux_rsrc_ind
                   OR o.prim_rsrc_ind = p_sec_rsrc_ind)
         ORDER BY 13;

      l_nb_record               BINARY_INTEGER    := 0;
      l_record_ind              BINARY_INTEGER    := 1;
      l_batch_id_str            VARCHAR2 (10);
      l_batchstep_no_str        VARCHAR2 (10);
      l_resources_str           VARCHAR2 (64);
      l_resource_desc_str       VARCHAR2 (240);--bug14480509 
      l_prim_rsrc_ind_str       VARCHAR2 (2);
      l_scale_type_str          VARCHAR2 (2);
      l_plan_rsrc_count_str     VARCHAR2 (40);
      l_actual_res_count_str    VARCHAR2 (40);
      l_plan_rsrc_qty_str       VARCHAR2 (40);
      l_actual_rsrc_qty_str     VARCHAR2 (40);
      l_plan_rsrc_usage_str     VARCHAR2 (40);
      l_actual_rsrc_usage_str   VARCHAR2 (40);
      l_std_usage_um_str        VARCHAR2 (16);
      l_offset_interval_str     VARCHAR2 (40);
      l_process_qty_um_str      VARCHAR2 (16);
      l_step_status_str         VARCHAR2 (3);
      l_resource_record         VARCHAR2 (792);
      l_resource_record_tbl     VARCHAR2 (32000);
      l_api_name       CONSTANT VARCHAR2 (30)     := 'GET_RESOURCES';
   BEGIN
      x_nb_resource := 0;

      OPEN get_resources;

      IF resources.EXISTS (1) THEN
         resources.DELETE;
      END IF;

      FETCH get_resources
      BULK COLLECT INTO resources, resource_desc, prim_rsrc_ind, scale_type
            ,plan_rsrc_count, actual_res_count, plan_rsrc_qty, actual_res_qty
            ,plan_rsrc_usage, actual_res_usage, std_usage_um, offset_interval
            ,start_date, end_date, plan_start_date, plan_cmplt_date
            ,actual_start_date, actual_cmplt_date, process_qty_um
            ,step_status;

      IF resources.EXISTS (1) AND resources.COUNT > 0 THEN
         FOR e IN 1 .. resources.COUNT LOOP
            l_batch_id_str := p_batch_id;
            l_batchstep_no_str := p_batchstep_no;
            l_resources_str := resources (e);
            l_resource_desc_str := resource_desc (e);
            l_prim_rsrc_ind_str := prim_rsrc_ind (e);
            l_scale_type_str := scale_type (e);
            l_plan_rsrc_count_str := plan_rsrc_count (e);
            l_actual_res_count_str := actual_res_count (e);
            l_plan_rsrc_qty_str := ROUND (plan_rsrc_qty (e), 3);
            l_actual_rsrc_qty_str := ROUND (actual_res_qty (e), 3);
            l_plan_rsrc_usage_str := ROUND (plan_rsrc_usage (e), 3);
            l_actual_rsrc_usage_str := ROUND (actual_res_usage (e), 3);
            l_std_usage_um_str := std_usage_um (e);
            l_offset_interval_str := offset_interval (e);
            l_step_status_str := step_status (e);
            l_process_qty_um_str := process_qty_um (e);
            l_resource_record :=
                  LPAD (l_batch_id_str, 10, '0')
               || LPAD (l_batchstep_no_str, 10, '0')
               || r_pad (l_resources_str, 16)
               || r_pad (l_resource_desc_str, 240)--bug14480509 
               || LPAD (l_prim_rsrc_ind_str, 2, '0')
               || LPAD (l_scale_type_str, 2, '0')
               || LPAD (l_plan_rsrc_count_str, 40, 0)
               || LPAD (l_actual_res_count_str, 40, 0)
               || LPAD (l_plan_rsrc_qty_str, 40, 0)
               || LPAD (l_actual_rsrc_qty_str, 40, 0)
               || LPAD (l_plan_rsrc_usage_str, 40, 0)
               || LPAD (l_actual_rsrc_usage_str, 40, 0)
               || r_pad (l_std_usage_um_str, 4)
               || LPAD (l_offset_interval_str, 40, 0)
               || LPAD (l_step_status_str, 3, '0')
               || r_pad (l_process_qty_um_str, 4)
               || TO_CHAR (start_date (e), 'YYYYMMDDHH24MISS')
               || TO_CHAR (end_date (e), 'YYYYMMDDHH24MISS')
               || TO_CHAR (plan_start_date (e), 'YYYYMMDDHH24MISS')
               || TO_CHAR (plan_cmplt_date (e), 'YYYYMMDDHH24MISS')
               || TO_CHAR (actual_start_date (e), 'YYYYMMDDHH24MISS')
               || TO_CHAR (actual_cmplt_date (e), 'YYYYMMDDHH24MISS');

            IF l_nb_record = 0 THEN
               l_resource_record_tbl := l_resource_record;
               l_nb_record := l_nb_record + 1;
            ELSE
               l_resource_record_tbl :=
                                   l_resource_record_tbl || l_resource_record;
               l_nb_record := l_nb_record + 1;

               IF l_nb_record >= 40 THEN
                  x_resource_table (l_record_ind) := l_resource_record_tbl;
                  l_record_ind := l_record_ind + 1;
                  l_nb_record := 0;
               END IF;
            END IF;

            x_nb_resource := x_nb_resource + 1;
         END LOOP;

         IF l_nb_record > 0 THEN
            x_resource_table (l_record_ind) := l_resource_record_tbl;
         END IF;
      END IF;

      CLOSE get_resources;
   --Bug2804440
   EXCEPTION
      WHEN OTHERS THEN
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
   --End Bug2804440
   END get_resources;

/*======================================================================
 # Eddie Oumerretane 21-MAR-02 Bug # 2189705. Retrieve the UOM from
 #                   the GME_MATERIAL_DETAILS in Get_Batch_Properties
 #======================================================================*/
   PROCEDURE get_batch_properties (
      p_batch_id               IN              NUMBER
     ,x_batch_properties_str   OUT NOCOPY      VARCHAR2)
   IS
      CURSOR get_ids (p_batch_id NUMBER)
      IS
         SELECT b.formula_id, b.routing_id, b.recipe_validity_rule_id
               ,v.recipe_id
           FROM gme_batch_header b, gmd_recipe_validity_rules v
          WHERE b.batch_id = p_batch_id
            AND b.recipe_validity_rule_id = v.recipe_validity_rule_id(+);

      CURSOR get_routing_info (p_routing_id NUMBER)
      IS
         SELECT routing_no, routing_vers, routing_desc
           FROM gmd_routings
          WHERE routing_id = p_routing_id;

      CURSOR get_formula_info (p_formula_id NUMBER)
      IS
         SELECT formula_no, formula_vers, formula_desc1
           FROM fm_form_mst
          WHERE formula_id = p_formula_id;

      CURSOR get_recipe_info (p_recipe_id NUMBER)
      IS
         SELECT recipe_no, recipe_version, recipe_description
           FROM gmd_recipes
          WHERE recipe_id = p_recipe_id;

      l_routing_id          NUMBER (10);
      l_formula_id          NUMBER (10);
      l_recipe_id           NUMBER (15);
      l_recipe_vr_id        NUMBER (15);
      l_inventory_item_id   NUMBER (15);
      l_routing_no          VARCHAR2 (128) := '?';
      l_routing_vers        NUMBER (5)     := 0;
      l_formula_no          VARCHAR2 (128) := '?';
      l_recipe_no           VARCHAR2 (128) := '?';
      l_formula_vers        NUMBER (5)     := 0;
      l_recipe_vers         NUMBER (5)     := 0;
      l_item_no             VARCHAR2 (128) := '?';
      l_item_desc           VARCHAR2 (280) := '?';
      l_recipe_desc         VARCHAR2 (280) := '?';
      l_formula_desc        VARCHAR2 (280) := '?';
      l_routing_desc        VARCHAR2 (240) := '?'; --bug14480509 
      l_item_um             VARCHAR2 (16)  := '?';
      l_plan_qty            NUMBER         := 0;
      l_actual_qty          NUMBER         := 0;
      l_routing_vers_str    VARCHAR2 (5);
      l_formula_vers_str    VARCHAR2 (5);
      l_plan_qty_str        VARCHAR2 (40);
      l_actual_qty_str      VARCHAR2 (40);
      l_api_name   CONSTANT VARCHAR2 (30)  := 'GET_BATCH_PROPERTIES';
   BEGIN
      OPEN get_ids (p_batch_id);

      FETCH get_ids
       INTO l_formula_id, l_routing_id, l_recipe_vr_id, l_recipe_id;

      CLOSE get_ids;

      IF (l_recipe_id IS NOT NULL AND l_recipe_id > 0) THEN
         OPEN get_recipe_info (l_recipe_id);

         FETCH get_recipe_info
          INTO l_recipe_no, l_recipe_vers, l_recipe_desc;

         CLOSE get_recipe_info;
      END IF;

      IF (l_routing_id IS NOT NULL AND l_routing_id > 0) THEN
         OPEN get_routing_info (l_routing_id);

         FETCH get_routing_info
          INTO l_routing_no, l_routing_vers, l_routing_desc;

         CLOSE get_routing_info;
      END IF;

      IF (l_formula_id IS NOT NULL AND l_formula_id > 0) THEN
         OPEN get_formula_info (l_formula_id);

         FETCH get_formula_info
          INTO l_formula_no, l_formula_vers, l_formula_desc;

         CLOSE get_formula_info;
      END IF;

      OPEN get_product_info (p_batch_id);

      FETCH get_product_info
       INTO l_inventory_item_id, l_item_no, l_item_desc;

      IF get_product_info%NOTFOUND THEN
        /* This must be an LCF batch */
         OPEN get_product_info_lcf (p_batch_id);

         FETCH get_product_info_lcf
          INTO l_inventory_item_id, l_item_no, l_item_desc;
          
         CLOSE get_product_info_lcf;
      END IF;

      CLOSE get_product_info;
         
      OPEN get_product_qty (p_batch_id, l_inventory_item_id);
 
      FETCH get_product_qty
       INTO l_plan_qty, l_actual_qty, l_item_um;

      CLOSE get_product_qty;

      l_routing_vers_str := l_routing_vers;
      l_formula_vers_str := l_formula_vers;
	  --Bug 25202346 change decimal from 2 to 5--
      l_plan_qty_str := ROUND (l_plan_qty, 5);
      l_actual_qty_str := ROUND (l_actual_qty, 5);
      --bug 25202190 ----
       IF(length(r_pad(l_item_desc, 70)) < 70) THEN
      l_item_desc := rpad(l_item_desc, 70);
      END IF;
      --bug 25202190 end--
      x_batch_properties_str :=
            r_pad (l_item_no, 32)
         || r_pad (l_item_desc, 70)
         || LPAD (l_plan_qty_str, 40, '0')
         || LPAD (l_actual_qty_str, 40, '0')
         || r_pad (l_item_um, 4)
         || r_pad (l_formula_no, 32)
         || LPAD (l_formula_vers, 5, '0')
         || r_pad (l_routing_no, 32)
         || LPAD (l_routing_vers, 5, '0')
         || r_pad (l_recipe_no, 32)
         || LPAD (l_recipe_vers, 5, '0')
         || r_pad (l_recipe_desc, 240)--bug14480509
         || r_pad (l_formula_desc, 240)--bug14480509
         || r_pad (l_routing_desc, 240);--bug14480509 
   --Bug2804440
    --bug 25202190----
     IF (length(x_batch_properties_str) < 1017) THEN
         x_batch_properties_str := RPAD (x_batch_properties_str, 1017);
    END IF;
     --bug 25202190 end--
   EXCEPTION
      WHEN OTHERS THEN
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
   --End Bug2804440
   END get_batch_properties;

   PROCEDURE close_cursors
   IS
      l_api_name   CONSTANT VARCHAR2 (30) := 'CLOSE_CURSORS';
   BEGIN
      IF get_batches_from_date%ISOPEN THEN
         CLOSE get_batches_from_date;
      END IF;

      IF get_batches_from_to_date%ISOPEN THEN
         CLOSE get_batches_from_to_date;
      END IF;
   --Bug2804440
   EXCEPTION
      WHEN OTHERS THEN
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
   --End Bug2804440
   END close_cursors;

/*======================================================================
 # Retrieve Shop calendar data for the given organization
 #======================================================================*/
   FUNCTION get_mfg_calendar (p_organization_id IN NUMBER)
      RETURN BOOLEAN
   IS
      CURSOR get_mfg_cal_code
      IS
         SELECT p.calendar_code, c.description
           FROM mtl_parameters p, bom_calendars c
          WHERE p.organization_id = p_organization_id
                AND c.calendar_code = p.calendar_code;
   BEGIN
      OPEN get_mfg_cal_code;

      FETCH get_mfg_cal_code
       INTO g_mfg_calendar_code, g_calendar_desc;

      IF (get_mfg_cal_code%NOTFOUND OR g_mfg_calendar_code IS NULL) THEN
         CLOSE get_mfg_cal_code;

         RETURN FALSE;
      ELSE
         CLOSE get_mfg_cal_code;

         RETURN TRUE;
      END IF;
   END get_mfg_calendar;

/*======================================================================
 # Retrieve Shop calendar assigned to the given organization
 #======================================================================*/
   PROCEDURE fetch_shop_calendar (
      p_organization_id     IN              NUMBER
     ,p_date                IN              DATE
     ,x_calendar_no         OUT NOCOPY      VARCHAR2
     ,x_calendar_desc       OUT NOCOPY      VARCHAR2
     ,x_calendar_start      OUT NOCOPY      VARCHAR2
     ,x_calendar_end        OUT NOCOPY      VARCHAR2
     ,x_calendar_assigned   OUT NOCOPY      VARCHAR2
     ,x_return_code         OUT NOCOPY      VARCHAR2
     ,x_error_msg           OUT NOCOPY      VARCHAR2)
   IS
      -- Get Calendar Date Range
      CURSOR get_calendar_date_range (p_calendar_code VARCHAR2)
      IS
         SELECT MIN (date_to_clientdt (calendar_start_date) )
               ,MAX (date_to_clientdt (calendar_end_date) )
           FROM bom_calendars
          WHERE calendar_code = p_calendar_code;

      l_api_name    CONSTANT VARCHAR2 (30) := 'Fetch_Shop_Calendar';
      inv_calendar_sel       EXCEPTION;
      no_calendar_assigned   EXCEPTION;
   BEGIN
      x_return_code := 'S';
      x_error_msg := ' ';

      IF NOT get_mfg_calendar (p_organization_id) THEN
         RAISE no_calendar_assigned;
      ELSE
         x_calendar_assigned := 'Y';

         OPEN get_calendar_date_range (g_mfg_calendar_code);

         FETCH get_calendar_date_range
          INTO g_calendar_start, g_calendar_end;

         CLOSE get_calendar_date_range;
      END IF;

      IF     (p_date IS NOT NULL)
         AND (p_date < g_calendar_start OR p_date > g_calendar_end) THEN
         RAISE inv_calendar_sel;
      END IF;

      x_calendar_start := TO_CHAR (g_calendar_start, 'YYYYMMDDHH24MISS');
      x_calendar_end := TO_CHAR (g_calendar_end, 'YYYYMMDDHH24MISS');
      x_calendar_no := r_pad (g_mfg_calendar_code, 16);
      x_calendar_desc := r_pad (g_calendar_desc, 40);
   EXCEPTION
      WHEN inv_calendar_sel THEN
         fnd_message.set_name ('GME', 'GME_DATE_OUTSIDE_SHOP_CALENDAR');
         --Bug3315440
         --FND_MESSAGE.SET_TOKEN('SEL_DATE', TO_CHAR(p_date,'DD-MON-YYYY'));
         --FND_MESSAGE.SET_TOKEN('CAL_BEGIN_DATE', TO_CHAR(g_calendar_start,'DD-MON-YYYY'));
         --FND_MESSAGE.SET_TOKEN('CAL_END_DATE', TO_CHAR(g_calendar_end,'DD-MON-YYYY'));
         --Bug4068469: removed trunc from the set_token call
         --FND_MESSAGE.SET_TOKEN('SEL_DATE', trunc(fnd_date.date_to_displayDT(p_date),'DD-MON-YYYY'));
         fnd_message.set_token ('SEL_DATE'
                               ,fnd_date.date_to_displaydt (p_date,2) );
         fnd_message.set_token ('CAL_BEGIN_DATE'
                               ,fnd_date.date_to_displaydt (g_calendar_start,2) );
         fnd_message.set_token ('CAL_END_DATE'
                               ,fnd_date.date_to_displaydt (g_calendar_end,2) );
         x_return_code := 'F';
         x_error_msg := r_pad (fnd_message.get, 1000);
      WHEN no_calendar_assigned THEN
         x_calendar_assigned := 'N';
         fnd_message.set_name ('GMP', 'GME_NO_CAL_ASSIGNED_TO_PLANT');
         x_return_code := 'F';
         x_error_msg := r_pad (fnd_message.get, 1000);
      WHEN OTHERS THEN
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
   END fetch_shop_calendar;

/*======================================================================
 # Retrieve working and non working days for a given date range
 #======================================================================*/
   PROCEDURE fetch_work_non_work_days (
      p_organization_id     IN              NUMBER
     ,p_from_date           IN              DATE
     ,p_to_date             IN              DATE
     ,x_calendar_no         OUT NOCOPY      VARCHAR2
     ,x_calendar_desc       OUT NOCOPY      VARCHAR2
     ,x_calendar_start      OUT NOCOPY      VARCHAR2
     ,x_calendar_end        OUT NOCOPY      VARCHAR2
     ,x_rec_count           OUT NOCOPY      NUMBER
     ,x_shop_cal_tbl        OUT NOCOPY      shopcalendartabletype
     ,x_calendar_assigned   OUT NOCOPY      VARCHAR2
     ,x_return_code         OUT NOCOPY      VARCHAR2
     ,x_error_msg           OUT NOCOPY      VARCHAR2)
   IS
      l_output_tbl             gmp_calendar_api.date_tbl;
      l_nb_record              BINARY_INTEGER            := 0;
      l_record_ind             BINARY_INTEGER            := 1;
      l_cal_rec                VARCHAR2 (15);
      l_cal_rec_tbl            VARCHAR2 (32000);
      l_api_name      CONSTANT VARCHAR2 (30)    := 'Fetch_Work_Non_Work_Days';
      l_max_records   CONSTANT NUMBER                    := 2100;
      inv_calendar_sel         EXCEPTION;
      fetch_cal_error          EXCEPTION;
      no_cal_assigned_to_org   EXCEPTION;
   BEGIN
      x_rec_count := 0;
      x_return_code := 'S';
      x_error_msg := ' ';
      x_calendar_assigned := 'Y';
      --- Get the calendar associated to the given organization
      fetch_shop_calendar (p_organization_id        => p_organization_id
                          ,p_date                   => NULL
                          ,x_calendar_no            => x_calendar_no
                          ,x_calendar_desc          => x_calendar_desc
                          ,x_calendar_start         => x_calendar_start
                          ,x_calendar_end           => x_calendar_end
                          ,x_calendar_assigned      => x_calendar_assigned
                          ,x_return_code            => x_return_code
                          ,x_error_msg              => x_error_msg);

      IF x_return_code <> 'S' THEN
         IF x_calendar_assigned = 'N' THEN
            RAISE no_cal_assigned_to_org;
         ELSE
            RAISE inv_calendar_sel;
         END IF;
      END IF;

      gmp_calendar_api.get_all_dates (p_api_version        => 1
                                     ,p_init_msg_list      => FALSE
                                     ,p_calendar_code      => g_mfg_calendar_code
                                     ,p_start_date         => p_from_date
                                     ,p_end_date           => p_to_date
                                     ,p_output_tbl         => l_output_tbl
                                     ,x_return_status      => x_return_code);

      IF x_return_code <> 'S' THEN
         RAISE fetch_cal_error;
      END IF;

      IF l_output_tbl.COUNT > 0 THEN
         x_rec_count := l_output_tbl.COUNT;

         FOR e IN 1 .. l_output_tbl.COUNT LOOP
            l_cal_rec :=
                  TO_CHAR (l_output_tbl (e).cal_date, 'YYYYMMDDHH24MISS')
               || l_output_tbl (e).is_workday;

            IF l_nb_record = 0 THEN
               l_cal_rec_tbl := l_cal_rec;
               l_nb_record := l_nb_record + 1;
            ELSE
               l_cal_rec_tbl := l_cal_rec_tbl || l_cal_rec;
               l_nb_record := l_nb_record + 1;

               IF l_nb_record >= l_max_records THEN
                  x_shop_cal_tbl (l_record_ind) := l_cal_rec_tbl;
                  l_record_ind := l_record_ind + 1;
                  l_nb_record := 0;
               END IF;
            END IF;
         END LOOP;

         IF l_nb_record > 0 THEN
            x_shop_cal_tbl (l_record_ind) := l_cal_rec_tbl;
         END IF;
      END IF;
   EXCEPTION
      WHEN inv_calendar_sel THEN
         fnd_message.set_name ('GMD', 'GMD_UNEXPECTED_ERROR');
         fnd_message.set_token ('ERROR', SQLERRM);
         x_return_code := 'F';
         x_error_msg := fnd_message.get;
      WHEN fetch_cal_error THEN
         x_error_msg := r_pad (fnd_message.get, 1000);
      WHEN no_cal_assigned_to_org THEN
         x_return_code := 'F';
      WHEN OTHERS THEN
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
   END fetch_work_non_work_days;

/*======================================================================
 # Check whether batch/FPO/Step can be re-scheduled based on shop calendar.
 # Returns:
 #       S : Batch/Step can be rescheduled
 #       C : Prompt the user to reschedule without using shop calendar
 #       P : Prompt the user to reschedule ignoring contiguity constraints
 #       F : Batch/Step cannot be reschedule
 #======================================================================*/
   PROCEDURE validate_reschedule_event (
      p_batch_id          IN              NUMBER
     ,p_organization_id   IN              NUMBER
     ,p_primary_prod_no   IN              VARCHAR2
     ,p_start_date        IN              DATE
     ,p_end_date          IN              DATE
     ,p_entity_type       IN              VARCHAR2
     ,x_return_code       OUT NOCOPY      VARCHAR2
     ,x_error_msg         OUT NOCOPY      VARCHAR2)
   IS
      l_api_name    CONSTANT VARCHAR2 (30)     := 'Validate_Reschedule_Event';
      l_is_working_day       BOOLEAN;
      l_contig_period_tbl    gmp_calendar_api.contig_period_tbl;
      l_batch_duration       NUMBER;
      l_contiguity_ind       NUMBER;
      l_batch_no             VARCHAR2 (32);
      non_work_time          EXCEPTION;
      reschedule_error       EXCEPTION;
      no_calendar_assigned   EXCEPTION;
   BEGIN
      l_is_working_day := TRUE;
      x_return_code := 'S';
      x_error_msg := ' ';

      IF g_mfg_calendar_code IS NULL THEN
         IF NOT get_mfg_calendar (p_organization_id) THEN
            RAISE no_calendar_assigned;
         END IF;
      END IF;

      IF p_start_date IS NOT NULL THEN
         l_is_working_day :=
            gmp_calendar_api.is_working_daytime
                                     (p_api_version        => 1
                                     ,p_init_msg_list      => TRUE
                                     ,p_calendar_code      => g_mfg_calendar_code
                                     ,p_date               => p_start_date
                                     ,p_ind                => 0
                                     ,                           -- Start date
                                      x_return_status      => x_return_code);

         /* For now let's treat this condition as non-working day
            So the folowing code is commented.  If the API returns
            Error or if the date is not working time, give same error
         IF x_return_code <> FND_API.G_RET_STS_SUCCESS THEN
           RAISE RESCHEDULE_ERROR;
         END IF; */

         --- Warn the user that start date is a non working time
         IF NOT l_is_working_day THEN
            fnd_message.set_name ('GME', 'GME_SHOP_NON_WKG');
             --Bug3315440
            -- FND_MESSAGE.SET_TOKEN('PDATE', TO_CHAR(p_start_date,'DD-MON-YYYY HH24 MI SS'));
            --Bug4068469: removed trunc from the set_token call
            fnd_message.set_token ('PDATE'
                                  ,fnd_date.date_to_displaydt (p_start_date,2) );
            RAISE non_work_time;
         END IF;
      END IF;

      IF p_end_date IS NOT NULL THEN
         l_is_working_day :=
            gmp_calendar_api.is_working_daytime
                                     (p_api_version        => 1
                                     ,p_init_msg_list      => TRUE
                                     ,p_calendar_code      => g_mfg_calendar_code
                                     ,p_date               => p_end_date
                                     ,p_ind                => 1
                                     ,                             -- End date
                                      x_return_status      => x_return_code);

         /* For now let's treat this condition as non-working day
            So the folowing code is commented.  If the API returns
            Error or if the date is not working time, give same error
         IF x_return_code <> FND_API.G_RET_STS_SUCCESS THEN
           RAISE RESCHEDULE_ERROR;
         END IF;  */

         --- Warn the user that end date is a non working time
         IF NOT l_is_working_day THEN
            fnd_message.set_name ('GME', 'GME_SHOP_NON_WKG');
             --Bug3315440
            -- FND_MESSAGE.SET_TOKEN('PDATE', TO_CHAR(p_end_date,'DD-MON-YYYY HH24 MI SS'));
            --Bug4068469: removed trunc from the set_token call
            fnd_message.set_token ('PDATE'
                                  ,fnd_date.date_to_displaydt (p_end_date,2) );
            RAISE non_work_time;
         END IF;
      END IF;
   EXCEPTION
      WHEN non_work_time THEN
         x_return_code := 'C';
         x_error_msg := r_pad (fnd_message.get, 1000);
      WHEN reschedule_error THEN
         x_return_code := 'F';
         x_error_msg := r_pad (fnd_message.get, 1000);
      WHEN no_calendar_assigned THEN
         fnd_message.set_name ('GMP', 'GME_NO_CAL_ASSIGNED_TO_PLANT');
         x_return_code := 'F';
         x_error_msg := r_pad (fnd_message.get, 1000);
      WHEN OTHERS THEN
         --Bug4068469: set x_return_code and the x_error_msg
         x_return_code := 'U';
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
         x_error_msg := r_pad (fnd_message.get, 1000);
   END validate_reschedule_event;

/*======================================================================
 # For timezone changes.
 # Returns:
 #       date : date in the client timezone
 # HISTORY
 #       Bharati Satpute   Bug3315440   21-JAN-2004
 #======================================================================*/
   FUNCTION date_to_clientdt (dateval DATE)
      RETURN DATE
   IS
      t_dateval             DATE;
      tz_code               VARCHAR2 (50);
      l_api_name   CONSTANT VARCHAR2 (30) := 'date_to_clientDT';
   BEGIN
      t_dateval := dateval;

      IF fnd_date.timezones_enabled THEN
         tz_code := fnd_date.client_timezone_code;

         IF tz_code <> fnd_date.server_timezone_code THEN
            t_dateval :=
               fnd_timezones_pvt.adjust_datetime
                                              (dateval
                                              ,fnd_date.server_timezone_code
                                              ,fnd_date.client_timezone_code);
         END IF;
      END IF;

      RETURN (t_dateval);
   EXCEPTION
      WHEN OTHERS THEN
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
   END date_to_clientdt;
END;
/
COMMIT ;
EXIT;
