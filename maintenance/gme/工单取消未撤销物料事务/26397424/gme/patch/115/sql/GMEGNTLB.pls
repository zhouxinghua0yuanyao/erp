/* +======================================================================+ */
/* |    Copyright (c) 2005, 2015 Oracle and/or its affiliates.           | */
/* |                         All rights reserved.                         | */
/* |                           Version 12.0.0                             | */
/* +======================================================================+ */
/**==========================================================================================
 #  Eddie Oumerretane 21-03-02 Bug 2187223 Select items directly from the ic_item_mst bable
 #  Bharati Satpute  04-MAR-2003 Bug2804440  Added WHEN OTHERS exception which was not 
 #                   defined in  some procedures /functions  
 #  SivakumarG 10-Jul-2006 Bug#5350221 Added serial control item check in procedure select_items_lov
 # Shaliu Chen 23-Apr-2015 ER 20938455 
 #                   Modify procedure - init_session to add condition to exclude the batch 
 #                   which is on hold. 
 #=========================================================================================*/
REM dbdrv: sql ~PROD ~PATH ~FILE none none none package &phase=plb \
REM dbdrv: checkfile(120.4.12010000.2=120.4.12020000.2)(115.11=120.2):~PROD:~PATH:~FILE
SET verify off;
WHENEVER sqlerror exit failure rollback;

CREATE OR REPLACE PACKAGE BODY gme_gantt_lov_pkg AS
/* $Header: GMEGNTLB.pls 120.4.12020000.2 2015/04/29 07:49:23 shalchen ship $  */
   g_pkg_name   CONSTANT VARCHAR2 (30) := 'GME_GANTT_LOV_PKG';

   /**
    * Select the appropriate batches and send them one by one to the gantt
    */
   PROCEDURE select_batches_lov (
      p_input_field         IN              VARCHAR2
     ,p_current_start_row   IN              INTEGER
     ,p_num_row_to_get      IN              INTEGER
     ,p_pending_status      IN              VARCHAR2
     ,p_released_status     IN              VARCHAR2
     ,p_certified_status    IN              VARCHAR2
     ,p_organization_id     IN              INTEGER
     ,p_batch_type          IN              INTEGER
     ,p_fpo_type            IN              INTEGER
     ,p_from_date           IN              DATE
     ,p_to_date             IN              DATE
     ,x_total_rows          OUT NOCOPY      NUMBER
     ,x_batch_lov_tbl       OUT NOCOPY      batchlovdetailtabletype)
   IS
      CURSOR batch_lov_from_and_to_date
      IS
         SELECT DISTINCT b.batch_id, batch_no, b.organization_id
                        ,batch_status, itd.concatenated_segments
                        ,itd.description
                    FROM gme_batch_header b
                        ,gme_material_details d
                        ,mtl_system_items_vl itd
                   WHERE b.organization_id = p_organization_id
                     AND UPPER (batch_no) LIKE p_input_field
                     AND b.batch_id = d.batch_id
                     AND (    (    batch_type = p_batch_type
                               AND (   batch_status = p_pending_status
                                    OR batch_status = p_released_status
                                    OR batch_status = p_certified_status) )
                          OR (batch_type = p_fpo_type AND batch_status = 1) )
                     AND (    (    batch_status = 1
                               AND (    (    plan_start_date >= p_from_date
                                         AND plan_start_date <= p_to_date)
                                    OR (    plan_cmplt_date >= p_from_date
                                        AND plan_start_date <= p_to_date) ) )
                          OR (    batch_status = 2
                              AND (    (    actual_start_date >= p_from_date
                                        AND actual_start_date <= p_to_date)
                                   OR (    plan_cmplt_date >= p_from_date
                                       AND actual_start_date <= p_to_date) ) )
                          OR (    batch_status = 3
                              AND (    (    actual_start_date >= p_from_date
                                        AND actual_start_date <= p_to_date)
                                   OR (    actual_cmplt_date >= p_from_date
                                       AND actual_start_date <= p_to_date) ) ) )
                     AND d.line_type = 1
                     AND d.inventory_item_id = itd.inventory_item_id
                     AND d.organization_id = itd.organization_id
                      /* ER 20938455 23-Apr-2015 Shaliu Chen
                         Add following condition to exclude the batch which is on hold
                      */                        
                     AND gme_common_pvt.get_batch_hold_status(b.batch_id) = 'R'
                ORDER BY 2;

      CURSOR batch_lov_from_date
      IS
         SELECT DISTINCT b.batch_id, batch_no, b.organization_id
                        ,batch_status, itd.concatenated_segments
                        ,itd.description
                    FROM gme_batch_header b
                        ,gme_material_details d
                        ,mtl_system_items_vl itd
                   WHERE b.organization_id = p_organization_id
                     AND UPPER (batch_no) LIKE p_input_field
                     AND b.batch_id = d.batch_id
                     AND (    (    batch_type = p_batch_type
                               AND (   batch_status = p_pending_status
                                    OR batch_status = p_released_status
                                    OR batch_status = p_certified_status) )
                          OR (batch_type = p_fpo_type AND batch_status = 1) )
                     AND (    (    batch_status = 1
                               AND (   plan_start_date >= p_from_date
                                    OR plan_cmplt_date >= p_from_date) )
                          OR (    batch_status = 2
                              AND (   actual_start_date >= p_from_date
                                   OR plan_cmplt_date >= p_from_date) )
                          OR (    batch_status = 3
                              AND (   actual_start_date >= p_from_date
                                   OR actual_cmplt_date >= p_from_date) ) )
                     AND d.line_type = 1
                     AND d.inventory_item_id = itd.inventory_item_id
                     AND d.organization_id = itd.organization_id
                      /* ER 20938455 23-Apr-2015 Shaliu Chen
                         Add following condition to exclude the batch which is on hold
                      */                         
                     AND gme_common_pvt.get_batch_hold_status(b.batch_id) = 'R'
                ORDER BY 2;

      l_max_rows               BINARY_INTEGER;
      l_current_start_row      BINARY_INTEGER;
      l_temp_batch_lov_table   batchlovdetailtabletype;
      i                        BINARY_INTEGER          := 1;
      j                        BINARY_INTEGER          := 0;
      l_api_name      CONSTANT VARCHAR2 (30)           := 'SELECT_BATCHES_LOV';
   BEGIN
      IF p_to_date IS NOT NULL THEN
         OPEN batch_lov_from_and_to_date;

         LOOP
            FETCH batch_lov_from_and_to_date
             INTO l_temp_batch_lov_table (i);

            EXIT WHEN batch_lov_from_and_to_date%NOTFOUND;
            i := i + 1;
         END LOOP;

         CLOSE batch_lov_from_and_to_date;
      ELSE
         OPEN batch_lov_from_date;

         LOOP
            FETCH batch_lov_from_date
             INTO l_temp_batch_lov_table (i);

            EXIT WHEN batch_lov_from_date%NOTFOUND;
            i := i + 1;
         END LOOP;

         CLOSE batch_lov_from_date;
      END IF;

      x_total_rows := l_temp_batch_lov_table.COUNT;

      IF x_total_rows > 0 THEN
         l_max_rows := p_num_row_to_get + p_current_start_row;

         IF l_max_rows > x_total_rows THEN
            l_max_rows := x_total_rows;
         END IF;

         IF p_current_start_row = 0 THEN
            l_current_start_row := 1;
         ELSE
            l_current_start_row := p_current_start_row;
         END IF;

         FOR i IN l_current_start_row .. l_max_rows LOOP
            j := j + 1;
            x_batch_lov_tbl (j).batch_no :=
                                          l_temp_batch_lov_table (i).batch_no;
            x_batch_lov_tbl (j).concatenated_segments := l_temp_batch_lov_table (i).concatenated_segments;
         END LOOP;
      END IF;
   --Bug2804440
   EXCEPTION
      WHEN OTHERS THEN
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
   --End Bug2804440
   END select_batches_lov;

/**==========================================================================================
 #  Eddie Oumerretane 21-03-02 Bug 2187223 Select items directly from the ic_item_mst bable
 #=========================================================================================*/
   PROCEDURE select_items_lov (
      p_input_field         IN              VARCHAR2
     ,p_current_start_row   IN              INTEGER
     ,p_num_row_to_get      IN              INTEGER
     ,p_line_type           IN              INTEGER
     ,p_organization_id     IN              INTEGER
     ,x_total_rows          OUT NOCOPY      NUMBER
     ,x_item_lov_tbl        OUT NOCOPY      itemlovdetailtabletype)
   IS
      l_line_type             NUMBER;
      l_add_event             VARCHAR2 (100);
      l_display_event         VARCHAR2 (100);
      l_api_name     CONSTANT VARCHAR2 (30)          := 'SELECT_ITEMS_LOV';

/*  Bug 2187223
    CURSOR item_lov IS

        SELECT DISTINCT
               i.item_id,
               i.item_no,
               i.item_desc1
        FROM
               GME_BATCH_HEADER b,
               GME_MATERIAL_DETAILS d,
               ic_item_mst i
        WHERE
               organization_id = p_organization_id             AND
               b.batch_id = d.batch_id               AND
               d.line_type = p_line_type             AND
               d.item_id   = i.item_id               AND
               UPPER(i.item_no) like p_input_field

        ORDER BY 2;
*/

      --FPBug#4911946
      sqlstmt               VARCHAR2(1000);
      TYPE t_genref IS REF CURSOR;
      item_lov  t_genref;

      /*CURSOR item_lov
      IS
         SELECT   i.inventory_item_id, i.concatenated_segments, i.description
             FROM mtl_system_items_vl i
            WHERE i.concatenated_segments LIKE p_input_field
         ORDER BY 2; */

      l_max_rows              BINARY_INTEGER;
      l_current_start_row     BINARY_INTEGER;
      l_temp_item_lov_table   itemlovdetailtabletype;
      i                       BINARY_INTEGER         := 1;
      j                       BINARY_INTEGER         := 0;
      gme_enabled_flag        VARCHAR2(1)            := 'Y';
      serial_control_flag     BINARY_INTEGER         := 1;
   BEGIN
      --FPBug#4911946 Begin
      sqlstmt  :=    'SELECT   i.inventory_item_id, i.concatenated_segments, i.description '
                   ||' FROM mtl_system_items_vl i '
		   ||' WHERE i.concatenated_segments LIKE :p_input_field '
		   ||'   AND organization_id = :org_id '
		   ||'   AND process_execution_enabled_flag = :flag '
		   ||'   AND serial_number_control_code = :serial_flag '    --Bug#5350221
		   ||' ORDER BY 2 ';
      OPEN item_lov FOR sqlstmt 
      USING p_input_field, p_organization_id, gme_enabled_flag, serial_control_flag;
      --FPBug#4911946 End
      LOOP
         FETCH item_lov
          INTO l_temp_item_lov_table (i);

         EXIT WHEN item_lov%NOTFOUND;
         i := i + 1;
      END LOOP;

      CLOSE item_lov;

      x_total_rows := l_temp_item_lov_table.COUNT;

      IF x_total_rows > 0 THEN
         l_max_rows := p_num_row_to_get + p_current_start_row;

         IF l_max_rows > x_total_rows THEN
            l_max_rows := x_total_rows;
         END IF;

         IF p_current_start_row = 0 THEN
            l_current_start_row := 1;
         ELSE
            l_current_start_row := p_current_start_row;
         END IF;

         FOR i IN l_current_start_row .. l_max_rows LOOP
            j := j + 1;
            x_item_lov_tbl (j).concatenated_segments := l_temp_item_lov_table (i).concatenated_segments;
            x_item_lov_tbl (j).description := l_temp_item_lov_table (i).description;
         END LOOP;
      END IF;
   --Bug2804440
   EXCEPTION
      WHEN OTHERS THEN
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
   --End Bug2804440
   END select_items_lov;

   /**
    * Select the appropriate resources and send them one by one to the gantt
    */
   PROCEDURE select_resources_lov (
      p_input_field         IN              VARCHAR2
     ,p_current_start_row   IN              INTEGER
     ,p_num_row_to_get      IN              INTEGER
     ,p_organization_id     IN              INTEGER
     ,x_total_rows          OUT NOCOPY      NUMBER
     ,x_rsrc_lov_tbl        OUT NOCOPY      resourcelovdetailtabletype)
   IS
      CURSOR rsrc_lov
      IS
         SELECT DISTINCT m.resources, m.resource_desc
                    FROM cr_rsrc_mst m, cr_rsrc_dtl d
                   WHERE d.organization_id = p_organization_id
                     AND UPPER (d.resources) LIKE p_input_field
                     AND d.resources = m.resources
                ORDER BY 1;

      l_max_rows              BINARY_INTEGER;
      l_current_start_row     BINARY_INTEGER;
      l_temp_rsrc_lov_table   resourcelovdetailtabletype;
      i                       BINARY_INTEGER             := 1;
      j                       BINARY_INTEGER             := 0;
      l_api_name     CONSTANT VARCHAR2 (30)          := 'SELECT_RESOURCES_LOV';
   BEGIN
      OPEN rsrc_lov;

      LOOP
         FETCH rsrc_lov
          INTO l_temp_rsrc_lov_table (i);

         EXIT WHEN rsrc_lov%NOTFOUND;
         i := i + 1;
      END LOOP;

      CLOSE rsrc_lov;

      x_total_rows := l_temp_rsrc_lov_table.COUNT;

      IF x_total_rows > 0 THEN
         l_max_rows := p_num_row_to_get + p_current_start_row;

         IF l_max_rows > x_total_rows THEN
            l_max_rows := x_total_rows;
         END IF;

         IF p_current_start_row = 0 THEN
            l_current_start_row := 1;
         ELSE
            l_current_start_row := p_current_start_row;
         END IF;

         FOR i IN l_current_start_row .. l_max_rows LOOP
            j := j + 1;
            x_rsrc_lov_tbl (j).resources :=
                                          l_temp_rsrc_lov_table (i).resources;
            x_rsrc_lov_tbl (j).resource_desc :=
                                      l_temp_rsrc_lov_table (i).resource_desc;
         END LOOP;
      END IF;
   --Bug2804440
   EXCEPTION
      WHEN OTHERS THEN
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
   --End Bug2804440
   END select_resources_lov;

   /**
    * Select the vaild organizations for the current operator
    */
   PROCEDURE select_organizations_lov (
      p_input_field         IN              VARCHAR2
     ,p_current_start_row   IN              INTEGER
     ,p_num_row_to_get      IN              INTEGER
     ,p_user_id             IN              NUMBER
     ,x_total_rows          OUT NOCOPY      NUMBER
     ,x_orgn_lov_tbl        OUT NOCOPY      orgnlovdetailtabletype)
   IS
      CURSOR orgn_lov IS
         SELECT m.organization_code, o.name
         FROM   mtl_parameters m, gmd_parameters_hdr gmd, gme_parameters gme, 
	        hr_all_organization_units o --FPBug#4911946
         WHERE  m.organization_code LIKE p_input_field
                AND gmd.organization_id = m.organization_id
                AND gmd.plant_ind = 1
                AND gme.organization_id = m.organization_id
                AND o.organization_id = m.organization_id
         ORDER BY 1;

      l_max_rows              BINARY_INTEGER;
      l_current_start_row     BINARY_INTEGER;
      l_temp_orgn_lov_table   orgnlovdetailtabletype;
      i                       BINARY_INTEGER         := 1;
      j                       BINARY_INTEGER         := 0;
      l_api_name     CONSTANT VARCHAR2 (30)      := 'SELECT_ORGANIZATIONS_LOV';
   BEGIN
      OPEN orgn_lov;

      LOOP
         FETCH orgn_lov
          INTO l_temp_orgn_lov_table (i);

         EXIT WHEN orgn_lov%NOTFOUND;
         i := i + 1;
      END LOOP;

      CLOSE orgn_lov;

      x_total_rows := l_temp_orgn_lov_table.COUNT;

      IF x_total_rows > 0 THEN
         l_max_rows := p_num_row_to_get + p_current_start_row;

         IF l_max_rows > x_total_rows THEN
            l_max_rows := x_total_rows;
         END IF;

         IF p_current_start_row = 0 THEN
            l_current_start_row := 1;
         ELSE
            l_current_start_row := p_current_start_row;
         END IF;

         FOR i IN l_current_start_row .. l_max_rows LOOP
            j := j + 1;
            x_orgn_lov_tbl (j).organization_code :=
                                          l_temp_orgn_lov_table (i).organization_code;
            x_orgn_lov_tbl (j).organization_name :=
                                          l_temp_orgn_lov_table (i).organization_name;
         END LOOP;
      END IF;
   --Bug2804440
   EXCEPTION
      WHEN OTHERS THEN
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
   --End Bug2804440
   END select_organizations_lov;
END;
/

COMMIT ;
EXIT;
