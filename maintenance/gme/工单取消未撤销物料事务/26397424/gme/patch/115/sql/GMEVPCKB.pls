/* +======================================================================+ */
/* |    Copyright (c) 2005, 2015 Oracle and/or its affiliates.           | */
/* |                         All rights reserved.                         | */
/* |                           Version 12.0.0                             | */
/* +======================================================================+ */
REM dbdrv: sql ~PROD ~PATH ~FILE none none none package &phase=plb \
REM dbdrv: checkfile(120.7.12010000.12=120.16.12020000.5)(120.7.12000000.12=120.7.12010000.10)(120.7.12000000.10=120.16):~PROD:~PATH:~FILE
SET VERIFY OFF
WHENEVER SQLERROR EXIT FAILURE ROLLBACK;
WHENEVER OSERROR EXIT FAILURE ROLLBACK;

CREATE OR REPLACE PACKAGE BODY gme_picking_pvt AS
/* $Header: GMEVPCKB.pls 120.16.12020000.5 2015/12/04 18:43:32 gmurator ship $ */
   g_debug                 VARCHAR2 (5)  := fnd_profile.VALUE ('AFLOG_LEVEL');
   g_pkg_name     CONSTANT VARCHAR2 (30) := 'GME_PICKING_PVT';
   g_partially_allocated   BOOLEAN       := FALSE;
   g_fully_allocated       BOOLEAN       := FALSE;
   g_not_allocated         BOOLEAN       := FALSE;
 

/*************************************************************************************************/
/* Oracle Process Manufacturing Process Execution APIs                                           */
/* Contents: GME Picking Procedures.                                                                                              */
/* File Name: GMEVPCKB.pls                                                                       */
/* HISTORY:
/* Susruth D. Bug#5311713 Commented the return status and put the message in the log file        */
/* GME_NO_MATERIALS_SELECTED                                                                     */

-- HALUTHRA   22-SEP-2008   Bug 7383625
--    Added IF condition to populate detailed_quantity
--    in PROCEDURE process_line

-- A.Mishra 15-May-2009     Bug 8481421
--    The fix includes adding the code to also update the Quantity Tree along with
--    the Material reservation, which was missing initially.
--    Procedure Process_line is modified to add the code for updating the quantity tree by calling 
--    the code from the INV side (inv_quantity_tree_pub.update_quantities)

-- G.Muratore    31-Aug-2010     Bug 9941121
--    Pass grouping rule id to create_move_order_hdr procedure.
--    Procedure: pick_material 

-- G.Muratore    22-Jun-2011     Bug 12613813
--    Picking is now also considering the locator value.
--    Procedure: process_line 

-- G.Muratore    02-DEC-2011     Bug 13076579
--    Code is restructured to only create move order header and lines for picking if the 
--    open qty is more than the sum of non detailed qty across all open move order lines.
--    PROCEDURE: pick_material

-- Shaliu Chen     05-MAY-2015  ER 20938455
--    Modify for Batch On Hold enhancement,modify cursor - get_ingredients
--    to exclude the batches which is on hold
/*************************************************************************************************/

   PROCEDURE conc_picking (
      err_buf                OUT NOCOPY      VARCHAR2
     ,ret_code               OUT NOCOPY      VARCHAR2
     ,p_organization_id      IN              NUMBER
     ,p_all_batches          IN              VARCHAR2
     ,                                             -- 1 = All, 2 = Backordered
      p_include_pending      IN              VARCHAR2
     ,p_include_wip          IN              VARCHAR2
     ,p_from_batch           IN              VARCHAR2
     ,p_to_batch             IN              VARCHAR2
     ,p_oprn_no              IN              VARCHAR2
     ,p_oprn_vers            IN              NUMBER
     ,p_product_no           IN              VARCHAR2
     ,p_ingredient_no        IN              VARCHAR2
     ,p_days_forward         IN              NUMBER
     ,p_from_req_date        IN              VARCHAR2
     ,p_to_req_date          IN              VARCHAR2
     ,p_pick_grouping_rule   IN              VARCHAR2
     ,p_print_pick_slip      IN              VARCHAR2 DEFAULT 'N'
     ,p_plan_tasks           IN              VARCHAR2 DEFAULT 'N'
     ,p_sales_order          IN              VARCHAR2)
   IS
      l_api_name   CONSTANT VARCHAR2 (30)               := 'conc_picking';
      l_return_status       VARCHAR2 (1);
      l_where               VARCHAR2 (4000);
      l_sql_stmt            VARCHAR2 (4000);
      l_msg_data            VARCHAR2 (2000);
      l_conc_request_id     NUMBER;
      l_msg_count           NUMBER;

      TYPE l_picking_tab IS TABLE OF gme_ingred_pick_vw%ROWTYPE
         INDEX BY BINARY_INTEGER;

      l_picking_tbl         l_picking_tab;
      l_mat_req_tbl         gme_picking_pvt.mtl_req_tab;
      build_where_err       EXCEPTION;
      pick_material_err     EXCEPTION;

      /* Bug 5212556 Added the following ref cursor etc */
      TYPE pick_ref IS REF CURSOR;
      l_pick_cursor pick_ref;
   BEGIN
      IF g_debug IS NOT NULL THEN
         gme_debug.log_initialize ('ConcPicking');
      END IF;
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.' || l_api_name);
      END IF;
      l_where := 'NVL(open_qty,0) > 0';
      l_where := l_where || ' AND organization_id = :organization_id';
      IF (TO_NUMBER(p_all_batches) = 2) THEN
         l_where := l_where || ' AND NVL(backordered_qty,0) > 0';
      END IF;
      IF (NVL (p_include_pending, 'N') = 'Y') AND (NVL (p_include_wip, 'N') = 'Y') THEN
         l_where := l_where || ' AND batch_status IN (1,2)';
      ELSIF (NVL (p_include_pending, 'N') = 'Y') THEN
         l_where := l_where || ' AND batch_status = 1';
      ELSIF (NVL (p_include_wip, 'N') = 'Y') THEN
         l_where := l_where || ' AND batch_status = 2';
      ELSE
         l_where := l_where || ' AND batch_status NOT IN (1,2)';
      END IF;
      /* Bug 5370563 oprn_no and oprn_vers can be null so added NVL */
      /* ER 20938455 05-MAY-2015 Shaliu Chen
       Add condition to exclude the batch which is on hold with STOP
      */
      l_where := l_where || ' AND LPAD(batch_no, 32, 0) >= LPAD(NVL(:from_batch, batch_no), 32, 0)'
                         || ' AND LPAD(batch_no, 32, 0) <= LPAD(NVL(:to_batch, batch_no), 32, 0)'
                         || ' AND NVL(oprn_no, '' '') LIKE NVL(:oprn_no, NVL(oprn_no, '' ''))'
                         || ' AND NVL(oprn_vers, -1) = NVL(:oprn_vers, NVL(oprn_vers, -1))'
                         || ' AND (:product_no IS NULL OR batch_id IN (SELECT DISTINCT batch_id FROM gme_material_details'
                         || ' WHERE organization_id = :organization_id'
                         || ' AND line_type = 1 AND inventory_item_id IN'
                         || ' (SELECT inventory_item_id FROM mtl_system_items_kfv WHERE organization_id = :organization_id'
                         || ' AND concatenated_segments LIKE :product_no)))'
                         || ' AND (:ingredient_no IS NULL OR batch_id IN (SELECT DISTINCT batch_id FROM gme_material_details'
                         || ' WHERE organization_id = :organization_id'
                         || ' AND line_type = -1 AND inventory_item_id IN'
                         || ' (SELECT inventory_item_id FROM mtl_system_items_kfv WHERE organization_id = :organization_id'
                         || ' AND concatenated_segments LIKE :ingredient_no)))'
                         || ' AND material_requirement_date <= SYSDATE + NVL(:days_forward, 100000)'
                         || ' AND material_requirement_date >= NVL(:from_req_date, material_requirement_date)'
                         || ' AND material_requirement_date <= NVL(:to_req_date, material_requirement_date)'
                         || ' AND gme_common_pvt.get_batch_hold_status(batch_id) <> ''S''';
      l_sql_stmt := 'SELECT * FROM gme_ingred_pick_vw WHERE ' || l_where;
      IF g_debug <= gme_debug.g_log_statement THEN
         gme_debug.put_line (g_pkg_name || '.' || l_api_name || ':'|| l_sql_stmt);
      END IF;
      OPEN l_pick_cursor FOR l_sql_stmt
        USING p_organization_id, p_from_batch, p_to_batch, p_oprn_no, p_oprn_vers,
              p_product_no, p_organization_id, p_organization_id, p_product_no,
              p_ingredient_no, p_organization_id, p_organization_id, p_ingredient_no,
              p_days_forward, fnd_date.canonical_to_date(p_from_req_date),
              fnd_date.canonical_to_date(p_to_req_date);
      FETCH l_pick_cursor BULK COLLECT INTO l_picking_tbl;
      CLOSE l_pick_cursor;
      IF g_debug <= gme_debug.g_log_statement THEN
        gme_debug.put_line(g_pkg_name|| '.'|| l_api_name|| ':No. of records found = '|| l_picking_tbl.COUNT);
      END IF;

      FOR i IN 1 .. l_picking_tbl.COUNT LOOP
         l_mat_req_tbl (i).organization_id :=
                                            l_picking_tbl (i).organization_id;
         l_mat_req_tbl (i).batch_id := l_picking_tbl (i).batch_id;
         l_mat_req_tbl (i).material_detail_id :=
                                         l_picking_tbl (i).material_detail_id;
         l_mat_req_tbl (i).inventory_item_id :=
                                          l_picking_tbl (i).inventory_item_id;
         l_mat_req_tbl (i).revision := l_picking_tbl (i).revision;
         l_mat_req_tbl (i).subinventory := l_picking_tbl (i).subinventory;
         l_mat_req_tbl (i).locator_id := l_picking_tbl (i).locator_id;
         l_mat_req_tbl (i).open_qty := l_picking_tbl (i).open_qty;
         l_mat_req_tbl (i).dtl_um := l_picking_tbl (i).dtl_um;
         l_mat_req_tbl (i).mtl_req_date :=
                                  l_picking_tbl (i).material_requirement_date;
      END LOOP;

      gme_picking_pvt.pick_material
                          (p_mtl_req_tbl          => l_mat_req_tbl
                          ,p_task_group_id        => TO_NUMBER
                                                         (p_pick_grouping_rule)
                          ,p_print_pick_slip      => p_print_pick_slip
                          ,p_plan_tasks           => p_plan_tasks
                          ,x_return_status        => l_return_status
                          ,x_conc_request_id      => l_conc_request_id);

      IF g_debug <= gme_debug.g_log_statement THEN
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ':Return from pick_material is '
                             || l_return_status);
      END IF;

      IF (l_return_status <> fnd_api.g_ret_sts_success) THEN
         RAISE pick_material_err;
      END IF;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;
   EXCEPTION
      WHEN build_where_err OR pick_material_err THEN
         gme_common_pvt.count_and_get (x_count      => l_msg_count
                                      ,x_data       => l_msg_data);
         raise_application_error (-20000, l_msg_data, TRUE);
      WHEN OTHERS THEN
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);

         IF g_debug <= gme_debug.g_log_unexpected THEN
            gme_debug.put_line (   'When others exception in '
                                || g_pkg_name
                                || '.'
                                || l_api_name
                                || ' Error is '
                                || SQLERRM);
         END IF;

         gme_common_pvt.count_and_get (x_count      => l_msg_count
                                      ,x_data       => l_msg_data);
         raise_application_error (-20001, l_msg_data, TRUE);
   END conc_picking;

   -- Added p_called_by parameter to be used for Endeca view.   
   /* Bug 5212556 Added inventory_item_id */
   FUNCTION get_open_qty (
      p_organization_id      IN   NUMBER
     ,p_batch_id             IN   NUMBER
     ,p_material_detail_id   IN   NUMBER
     ,p_inventory_item_id    IN   NUMBER
     ,p_subinventory         IN   VARCHAR2
     ,p_plan_qty             IN   NUMBER
     ,p_wip_plan_qty         IN   NUMBER
     ,p_actual_qty           IN   NUMBER
     ,p_backordered_qty      IN   NUMBER
     ,p_dtl_um               IN   VARCHAR2
     ,p_called_by            IN   VARCHAR2 DEFAULT 'P')
      RETURN NUMBER
   IS
      l_api_name   CONSTANT VARCHAR2 (30)                  := 'get_open_qty';
      l_open_qty            NUMBER                         := 0;
      l_return_status       VARCHAR2 (1);
      l_mtl_dtl_rec         gme_material_details%ROWTYPE;
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      l_mtl_dtl_rec.organization_id    := p_organization_id;
      l_mtl_dtl_rec.batch_id           := p_batch_id;
      l_mtl_dtl_rec.material_detail_id := p_material_detail_id;
      l_mtl_dtl_rec.inventory_item_id  := p_inventory_item_id;
      l_mtl_dtl_rec.subinventory       := p_subinventory;
      l_mtl_dtl_rec.plan_qty           := p_plan_qty;
      l_mtl_dtl_rec.wip_plan_qty       := p_wip_plan_qty;
      l_mtl_dtl_rec.actual_qty         := p_actual_qty;
      l_mtl_dtl_rec.backordered_qty    := p_backordered_qty;
      l_mtl_dtl_rec.dtl_um             := p_dtl_um;
  
      
      -- Added p_called_by parameter to be used for Endeca view.   
      gme_common_pvt.get_open_qty (p_mtl_dtl_rec        => l_mtl_dtl_rec
                                  -- ,p_called_by          => 'P'
                                  ,p_called_by          => p_called_by
                                  ,x_open_qty           => l_open_qty
                                  ,x_return_status      => l_return_status);

      IF (l_return_status <> fnd_api.g_ret_sts_success) THEN
         RETURN 0;
      ELSE
         RETURN l_open_qty;
      END IF;
   EXCEPTION
      WHEN OTHERS THEN
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);

         IF g_debug <= gme_debug.g_log_unexpected THEN
            gme_debug.put_line (   'When others exception in '
                                || g_pkg_name
                                || '.'
                                || l_api_name
                                || ' Error is '
                                || SQLERRM);
         END IF;

         RETURN 0;
   END get_open_qty;

   PROCEDURE pick_material (
      p_mtl_req_tbl       IN              gme_picking_pvt.mtl_req_tab
     ,p_task_group_id     IN              NUMBER
     ,p_print_pick_slip   IN              VARCHAR2 DEFAULT 'N'
     ,p_plan_tasks        IN              VARCHAR2 DEFAULT 'N'
     ,x_return_status     OUT NOCOPY      VARCHAR2
     ,x_conc_request_id   OUT NOCOPY      NUMBER)
   IS
      l_api_name      CONSTANT VARCHAR2 (30)               := 'pick_material';
      l_count                  NUMBER                              := 0;
      l_msg_count              NUMBER;
      l_move_order_header_id   NUMBER;
      l_conc_req_id            NUMBER;
      l_return_status          VARCHAR2 (1);
      l_msg_data               VARCHAR2 (2000);
      l_plan_tasks             BOOLEAN;
      l_mtl_dtl_tbl            gme_common_pvt.material_details_tab;
      l_out_mtl_dtl_tbl        gme_common_pvt.material_details_tab;
      l_trolin_tbl             inv_move_order_pub.trolin_tbl_type;
      l_trolin_rec             inv_move_order_pub.trolin_rec_type;

      CURSOR cur_pending_move_orders (
         v_org_id               NUMBER
        ,v_batch_id             NUMBER
        ,v_material_detail_id   NUMBER)
      IS
         SELECT   l.line_id, l.header_id
             FROM mtl_txn_request_lines l, mtl_txn_request_headers h
            WHERE l.organization_id = v_org_id
              AND transaction_source_type_id =
                                              gme_common_pvt.g_txn_source_type
              AND l.txn_source_id = v_batch_id
              AND l.txn_source_line_id = v_material_detail_id
              -- Bug 13076579 - exclude those that are already fully detailed.
              AND l.quantity <> l.quantity_detailed
              AND l.line_status NOT IN (5, 6)
              AND h.header_id = l.header_id
              AND h.move_order_type NOT IN
                     (gme_common_pvt.g_invis_move_order_type
                     ,inv_globals.g_move_order_put_away)
         ORDER BY l.header_id, l.line_id;

      -- Bug 13076579
      l_index                  NUMBER;
      l_hdr_created            NUMBER;
      l_mo_sum                 NUMBER;
      
      -- Bug 13076579 - get the count of all open move orders to see what is still open.
      CURSOR cur_pending_move_orders_cnt (
         v_org_id               NUMBER
        ,v_batch_id             NUMBER
        ,v_material_detail_id   NUMBER)
      IS
         SELECT   count(1)
             FROM mtl_txn_request_lines l, mtl_txn_request_headers h
            WHERE l.organization_id = v_org_id
              AND transaction_source_type_id =
                                              gme_common_pvt.g_txn_source_type
              AND l.txn_source_id = v_batch_id
              AND l.txn_source_line_id = v_material_detail_id
              AND l.line_status NOT IN (5, 6)
              AND h.header_id = l.header_id
              AND h.move_order_type NOT IN
                     (gme_common_pvt.g_invis_move_order_type
                     ,inv_globals.g_move_order_put_away);
         
         
      -- Bug 13076579 - get the sum across all open move orders to see what is still open.
      CURSOR cur_pending_move_orders_sum (
         v_org_id               NUMBER
        ,v_batch_id             NUMBER
        ,v_material_detail_id   NUMBER)
      IS
         SELECT   NVL(sum(l.quantity - l.quantity_detailed), 0)
             FROM mtl_txn_request_lines l, mtl_txn_request_headers h
            WHERE l.organization_id = v_org_id
              AND transaction_source_type_id =
                                              gme_common_pvt.g_txn_source_type
              AND l.txn_source_id = v_batch_id
              AND l.txn_source_line_id = v_material_detail_id
              AND l.line_status NOT IN (5, 6)
              AND h.header_id = l.header_id
              AND h.move_order_type NOT IN
                     (gme_common_pvt.g_invis_move_order_type
                     ,inv_globals.g_move_order_put_away)
         GROUP BY l.txn_source_line_id;
         
      TYPE pend_lines_tab IS TABLE OF cur_pending_move_orders%ROWTYPE
         INDEX BY BINARY_INTEGER;

      l_pend_lines_tbl         pend_lines_tab;
      no_materials_picked      EXCEPTION;
      create_move_order_err    EXCEPTION;
      setup_failure            EXCEPTION;
      process_line_err         EXCEPTION;
      print_pickslip_err       EXCEPTION;
   BEGIN
      IF g_debug IS NOT NULL THEN
         gme_debug.log_initialize ('PickMaterial');
      END IF;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      x_return_status := fnd_api.g_ret_sts_success;
      l_count := p_mtl_req_tbl.COUNT;

      IF g_debug <= gme_debug.g_log_statement THEN
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ':Material Requirement count is '
                             || l_count);
      END IF;

      IF (l_count = 0) THEN
         RAISE no_materials_picked;
      END IF;

      IF NOT gme_common_pvt.g_setup_done THEN
         gme_common_pvt.g_setup_done :=
            gme_common_pvt.setup
                                (p_org_id      => p_mtl_req_tbl (1).organization_id);

         IF NOT gme_common_pvt.g_setup_done THEN
            RAISE setup_failure;
         END IF;
      END IF;

      IF g_debug <= gme_debug.g_log_statement THEN
         gme_debug.put_line (g_pkg_name || '.' || l_api_name || ':After setup');
      END IF;

      -- Bug 13076579 - Moved creation of MO hdr inside loop to avoid creating it unnecessarily.
/*
      gme_move_orders_pvt.create_move_order_hdr
                       (p_organization_id           => p_mtl_req_tbl (1).organization_id
                       ,p_move_order_type           => gme_common_pvt.g_move_order_type
                       ,p_grouping_rule_id          => p_task_group_id   -- Bug 9941121
                       ,x_move_order_header_id      => l_move_order_header_id
                       ,x_return_status             => l_return_status);

      IF (l_return_status <> fnd_api.g_ret_sts_success) THEN
         RAISE create_move_order_err;
      END IF;

      IF g_debug <= gme_debug.g_log_statement THEN
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ':Move order header created is '
                             || l_move_order_header_id);
      END IF;
*/      

      l_hdr_created := 0;
      l_index := 0;
      
      FOR i IN 1 .. l_count LOOP
         OPEN cur_pending_move_orders_cnt (p_mtl_req_tbl (i).organization_id
                                          ,p_mtl_req_tbl (i).batch_id
                                          ,p_mtl_req_tbl (i).material_detail_id);

         FETCH cur_pending_move_orders_cnt  INTO l_mo_sum;         
         CLOSE cur_pending_move_orders_cnt;

         IF (l_mo_sum > 0) THEN
            -- Bug 13076579 - get the sum across all open move orders.
            OPEN cur_pending_move_orders_sum (p_mtl_req_tbl (i).organization_id
                                             ,p_mtl_req_tbl (i).batch_id
                                             ,p_mtl_req_tbl (i).material_detail_id);
            
            FETCH cur_pending_move_orders_sum  INTO l_mo_sum;         
            CLOSE cur_pending_move_orders_sum;
         END IF;
                  
         IF g_debug <= gme_debug.g_log_statement THEN
            gme_debug.put_line ('iteration is '|| TO_CHAR(i));
            gme_debug.put_line ('material_detail_id is '|| TO_CHAR(p_mtl_req_tbl (i).material_detail_id));
            gme_debug.put_line ('open_qty is '|| TO_CHAR(p_mtl_req_tbl (i).open_qty));
            gme_debug.put_line ('l_mo_sum is '|| TO_CHAR(l_mo_sum));
         END IF;
         
         -- Bug 13076579 - Do not create a move order if the existing MO already accounts for the open qty.
         IF l_mo_sum < p_mtl_req_tbl (i).open_qty THEN
            -- Bug 13076579 - Do not create a move order hdr if not needed.
            IF l_hdr_created = 0 THEN
               gme_move_orders_pvt.create_move_order_hdr
                                (p_organization_id           => p_mtl_req_tbl (1).organization_id
                                ,p_move_order_type           => gme_common_pvt.g_move_order_type
                                ,p_grouping_rule_id          => p_task_group_id   -- Bug 9941121
                                ,x_move_order_header_id      => l_move_order_header_id
                                ,x_return_status             => l_return_status);
               
               IF (l_return_status <> fnd_api.g_ret_sts_success) THEN
                  RAISE create_move_order_err;
               END IF;
               
               IF g_debug <= gme_debug.g_log_statement THEN
                  gme_debug.put_line (   g_pkg_name
                                      || '.'
                                      || l_api_name
                                      || ':Move order header created is '
                                      || l_move_order_header_id);
               END IF;
               l_hdr_created := 1;      
            END IF;
            
            l_index := l_index + 1;
            l_mtl_dtl_tbl (l_index).inventory_item_id :=
                                           p_mtl_req_tbl (i).inventory_item_id;
            l_mtl_dtl_tbl (l_index).organization_id :=
                                             p_mtl_req_tbl (i).organization_id;
            l_mtl_dtl_tbl (l_index).plan_qty := p_mtl_req_tbl (i).open_qty - l_mo_sum; -- Account only for the delta.
            l_mtl_dtl_tbl (l_index).dtl_um := p_mtl_req_tbl (i).dtl_um;
            l_mtl_dtl_tbl (l_index).revision := p_mtl_req_tbl (i).revision;
            l_mtl_dtl_tbl (l_index).batch_id := p_mtl_req_tbl (i).batch_id;
            l_mtl_dtl_tbl (l_index).material_detail_id :=
                                          p_mtl_req_tbl (i).material_detail_id;
            l_mtl_dtl_tbl (l_index).material_requirement_date :=
                                               p_mtl_req_tbl (i).mtl_req_date;
            l_mtl_dtl_tbl (l_index).subinventory := p_mtl_req_tbl (i).subinventory;
            l_mtl_dtl_tbl (l_index).locator_id := p_mtl_req_tbl (i).locator_id;
            l_mtl_dtl_tbl (l_index).line_type := gme_common_pvt.g_line_type_ing;
         END IF;      
      END LOOP;

      -- Try to allocate any other move order lines that exist and are not allocated. These could
      -- be move orders created during batch create or not fully detailed previously by picking.
      FOR i IN 1 .. l_count LOOP
         OPEN cur_pending_move_orders (p_mtl_req_tbl (i).organization_id
                                      ,p_mtl_req_tbl (i).batch_id
                                      ,p_mtl_req_tbl (i).material_detail_id);

         FETCH cur_pending_move_orders
         BULK COLLECT INTO l_pend_lines_tbl;

         CLOSE cur_pending_move_orders;
      END LOOP;

      IF g_debug <= gme_debug.g_log_statement THEN
         gme_debug.put_line
                           (   g_pkg_name
                            || '.'
                            || l_api_name
                            || ':No. of lines to create move_order_lines is '
                            || l_mtl_dtl_tbl.COUNT);
      END IF;

      gme_move_orders_pvt.create_move_order_lines
                       (p_move_order_header_id      => l_move_order_header_id
                       ,p_move_order_type           => gme_common_pvt.g_move_order_type
                       ,p_material_details_tbl      => l_mtl_dtl_tbl
                       ,x_material_details_tbl      => l_out_mtl_dtl_tbl
                       ,x_trolin_tbl                => l_trolin_tbl
                       ,x_return_status             => l_return_status);

      IF (l_return_status <> fnd_api.g_ret_sts_success) THEN
         RAISE create_move_order_err;
      END IF;

      l_count := l_trolin_tbl.COUNT;

      IF g_debug <= gme_debug.g_log_statement THEN
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ':No. Move order lines created is '
                             || l_count);
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ':No. Pending Move order lines is '
                             || l_pend_lines_tbl.COUNT);
      END IF;

      FOR i IN 1 .. l_pend_lines_tbl.COUNT LOOP
         l_count := l_count + 1;
         l_trolin_tbl (l_count) :=
                     inv_trolin_util.query_row (l_pend_lines_tbl (i).line_id);
      END LOOP;

      FOR i IN 1 .. l_trolin_tbl.COUNT LOOP
         gme_picking_pvt.process_line (p_mo_line_rec           => l_trolin_tbl
                                                                           (i)
                                      ,p_grouping_rule_id      => p_task_group_id
                                      ,p_plan_tasks            => p_plan_tasks
                                      ,x_return_status         => l_return_status);

         IF (l_return_status <> fnd_api.g_ret_sts_success) THEN
            RAISE process_line_err;
         END IF;
      END LOOP;

      -- Bug 22128352 - Added debug messages.
      IF g_debug <= gme_debug.g_log_statement THEN
         gme_debug.put_line ('p_print_pick_slip '||p_print_pick_slip);
         
         IF g_fully_allocated THEN
            gme_debug.put_line ('g_fully_allocated is TRUE');
         ELSE
            gme_debug.put_line ('g_fully_allocated is FALSE');
         END IF;
         
         IF g_partially_allocated THEN
            gme_debug.put_line ('g_partially_allocated is TRUE');
         ELSE
            gme_debug.put_line ('g_partially_allocated is FALSE');
         END IF;
      END IF;

      IF (    p_print_pick_slip = 'Y'
          AND (g_fully_allocated OR g_partially_allocated) ) THEN
         IF g_debug <= gme_debug.g_log_statement THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ':Calling print picklsip');
         END IF;

         IF (p_plan_tasks = 'Y') THEN
            l_plan_tasks := TRUE;
         ELSE
            l_plan_tasks := FALSE;
         END IF;

         l_conc_req_id :=
            inv_pr_pick_slip_number.print_pick_slip
                       (x_return_status          => l_return_status
                       ,x_msg_data               => l_msg_data
                       ,x_msg_count              => l_msg_count
                       ,p_organization_id        => p_mtl_req_tbl (1).organization_id
                       ,p_mo_request_number      => l_move_order_header_id
                       ,p_plan_tasks             => l_plan_tasks);

         
         IF g_debug <= gme_debug.g_log_statement THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ':Print picklsip returns '
                                || l_return_status);
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ':Print picklsip message '
                                || l_msg_data);
         END IF;

         IF (l_return_status <> fnd_api.g_ret_sts_success) THEN
            RAISE print_pickslip_err;
         END IF;
     END IF;

      IF (l_conc_req_id IS NOT NULL) THEN
         IF (NOT (g_partially_allocated) AND NOT (g_not_allocated) ) THEN
            gme_common_pvt.log_message
                                 (p_message_code      => 'GME_PICKED_ALL_PRINTED'
                                 ,p_token1_name       => 'MO_NUMBER'
                                 ,p_token1_value      => l_move_order_header_id
                                 ,p_token2_name       => 'CONC_REQUEST_ID'
                                 ,p_token2_value      => l_conc_req_id);
           IF g_debug <= gme_debug.g_log_unexpected THEN
             gme_debug.put_line(fnd_msg_pub.get(FND_MSG_PUB.G_LAST, 'F'));
           END IF;
         ELSIF (g_partially_allocated OR g_fully_allocated) THEN
            gme_common_pvt.log_message
                             (p_message_code      => 'GME_PICKED_PARTIAL_PRINTED'
                             ,p_token1_name       => 'MO_NUMBER'
                             ,p_token1_value      => l_move_order_header_id
                             ,p_token2_name       => 'CONC_REQUEST_ID'
                             ,p_token2_value      => l_conc_req_id);
           IF g_debug <= gme_debug.g_log_unexpected THEN
             gme_debug.put_line(fnd_msg_pub.get(FND_MSG_PUB.G_LAST, 'F'));
           END IF;
         END IF;
      ELSE
         IF (NOT (g_partially_allocated) AND NOT (g_not_allocated) ) THEN
            gme_common_pvt.log_message
                                    (p_message_code      => 'GME_PICKED_ALL'
                                    ,p_token1_name       => 'MO_NUMBER'
                                    ,p_token1_value      => l_move_order_header_id);
           IF g_debug <= gme_debug.g_log_unexpected THEN
             gme_debug.put_line(fnd_msg_pub.get(FND_MSG_PUB.G_LAST, 'F'));
           END IF;
         ELSIF (g_partially_allocated OR g_fully_allocated) THEN
            gme_common_pvt.log_message
                                    (p_message_code      => 'GME_PICKED_PARTIAL'
                                    ,p_token1_name       => 'MO_NUMBER'
                                    ,p_token1_value      => l_move_order_header_id);
           IF g_debug <= gme_debug.g_log_unexpected THEN
             gme_debug.put_line(fnd_msg_pub.get(FND_MSG_PUB.G_LAST, 'F'));
           END IF;
         ELSIF (NOT (g_partially_allocated) AND NOT (g_fully_allocated) ) THEN
            gme_common_pvt.log_message
                                    (p_message_code      => 'GME_PICKED_NO_ALLOC'
                                    ,p_token1_name       => 'MO_NUMBER'
                                    ,p_token1_value      => l_move_order_header_id);
           IF g_debug <= gme_debug.g_log_unexpected THEN
             gme_debug.put_line(fnd_msg_pub.get(FND_MSG_PUB.G_LAST, 'F'));
           END IF;
         END IF;
      END IF;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;
   EXCEPTION
      WHEN no_materials_picked THEN
      --Bug#5311713
         FND_MESSAGE.SET_NAME('GME','GME_NO_MATERIALS_SELECTED');
         FND_FILE.PUT_LINE(FND_FILE.log,FND_MESSAGE.GET);
         --gme_common_pvt.log_message ('GME_NO_MATERIALS_SELECTED');
         --x_return_status := fnd_api.g_ret_sts_error;
      WHEN setup_failure THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN create_move_order_err OR process_line_err OR print_pickslip_err THEN
         x_return_status := l_return_status;
      WHEN OTHERS THEN
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);

         IF g_debug <= gme_debug.g_log_unexpected THEN
            gme_debug.put_line (   'When others exception in '
                                || g_pkg_name
                                || '.'
                                || l_api_name
                                || ' Error is '
                                || SQLERRM);
         END IF;

         x_return_status := fnd_api.g_ret_sts_unexp_error;
   END pick_material;

   PROCEDURE process_line (
      p_mo_line_rec        IN              inv_move_order_pub.trolin_rec_type
     ,p_grouping_rule_id   IN              NUMBER
     ,p_plan_tasks         IN              VARCHAR2 DEFAULT 'N'
     ,x_return_status      OUT NOCOPY      VARCHAR2)
   IS
      l_api_name      CONSTANT VARCHAR2 (30)                := 'process_line';
      
      -- Bug 8481421 - Variables added      
      l_res_ordered_index               NUMBER; -- An index to the elements of the ordered and filtered reservations table.
      l_prev_rsv_detailed_qty           NUMBER; -- The existing qty detailed for a reservation.
      l_prev_rsv_detailed_qty2          NUMBER; -- The existing qty2 detailed for a reservation.
      l_reservation_detailed_qty        NUMBER;
      l_rsv_detailed_qty2               NUMBER;
      l_primary_uom                     VARCHAR2(3); -- The primary UOM for the item
      l_secondary_uom                   VARCHAR2(3); -- The secondary UOM for the item
      l_api_return_status               VARCHAR2(1); -- The return status of APIs called within the Process Line API.
      l_revision_control_code           NUMBER;
      l_lot_control_code                NUMBER;
      l_serial_number_control_code      NUMBER;      
      l_reservation_id                  NUMBER;
      l_rsv_detailed_qty_conv           NUMBER; -- The qty detailed for a reservation. (In reservation UOM)
      l_is_serial_control               BOOLEAN;
      l_is_revision_control             BOOLEAN;
      l_is_lot_control                  BOOLEAN;   
      item_rec  mtl_system_items%ROWTYPE;
      l_mtl_dtl_rec  gme_material_details%ROWTYPE;
      l_qty_on_hand                     NUMBER;
      l_qty_res_on_hand                 NUMBER;
      l_qty_res                         NUMBER;
      l_qty_sug                         NUMBER;
      l_qty_att                         NUMBER;
      l_qty_available_to_reserve        NUMBER;
      l_sec_qty_available_to_reserve    NUMBER; -- The quantity which can still be reserved.
      l_sec_qty_on_hand                 NUMBER; -- The org-wide quantity on-hand
      l_sec_qty_res_on_hand             NUMBER; -- The org-wide reservable quantity on-hand
      l_sec_qty_res                     NUMBER; -- The org-wide quantity reserved
      l_sec_qty_sug                     NUMBER; -- The org-wide quantity suggested
      l_sec_qty_att                     NUMBER; -- The org-wide available to transact
      l_sec_quantity_to_reserve         NUMBER; -- The additional quantity which should be reserved.

      -- End New variables for Bug 8481421

      CURSOR cur_detailed_qty (v_move_order_line_id NUMBER)
      IS
         SELECT SUM (transaction_quantity) qty_detailed
               ,SUM (secondary_transaction_quantity) sec_qty_detailed
           FROM mtl_material_transactions_temp
          WHERE move_order_line_id = v_move_order_line_id;

      CURSOR cur_mmtt (v_move_order_line_id NUMBER)
      IS
         SELECT *
           FROM mtl_material_transactions_temp
          WHERE move_order_line_id = v_move_order_line_id;

      l_count                  NUMBER                                    := 0;
      l_msg_count              NUMBER;
      l_qty_detailed           NUMBER;
      l_sec_qty_detailed       NUMBER;
      l_backordered_qty        NUMBER;
      l_pick_slip_number       NUMBER;
      l_msg_data               VARCHAR2 (2000);
      l_move_order_header_id   NUMBER;
      l_return_status          VARCHAR2 (1);
      l_plan_tasks             BOOLEAN;
      l_mo_line_rec            inv_move_order_pub.trolin_rec_type;
      l_resv_tbl               gme_common_pvt.reservations_tab;
      l_inv_resv_tbl           inv_reservation_global.mtl_reservation_tbl_type;
      create_suggestions_err   EXCEPTION;
      get_pick_slip_err        EXCEPTION;
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;            
      
      x_return_status := fnd_api.g_ret_sts_success;
      gme_reservations_pvt.get_material_reservations
                    (p_organization_id         => p_mo_line_rec.organization_id
                    ,p_batch_id                => p_mo_line_rec.txn_source_id
                    ,p_material_detail_id      => p_mo_line_rec.txn_source_line_id
                    ,x_return_status           => l_return_status
                    ,x_reservations_tbl        => l_resv_tbl);

      IF g_debug <= gme_debug.g_log_statement THEN
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ':No. of reservations is '
                             || l_resv_tbl.COUNT);
      END IF;

      FOR i IN 1 .. l_resv_tbl.COUNT LOOP
         --
         -- bug 10254285
         -- Added NVL condition to handle the partial reservation case
         --
         -- Bug 12613813 - consider locator value also.
         IF (NVL(l_resv_tbl (i).subinventory_code , '-1') <> p_mo_line_rec.to_subinventory_code) OR
            (NVL(l_resv_tbl (i).locator_id , '-1') <> NVL(p_mo_line_rec.to_locator_id, '-1')) THEN
            l_count := l_count + 1;
            l_inv_resv_tbl (l_count).reservation_id :=
                                                l_resv_tbl (i).reservation_id;
            l_inv_resv_tbl (l_count).requirement_date :=
                                              l_resv_tbl (i).requirement_date;
            l_inv_resv_tbl (l_count).organization_id :=
                                               l_resv_tbl (i).organization_id;
            l_inv_resv_tbl (l_count).inventory_item_id :=
                                             l_resv_tbl (i).inventory_item_id;
            l_inv_resv_tbl (l_count).demand_source_type_id :=
                                         l_resv_tbl (i).demand_source_type_id;
            l_inv_resv_tbl (l_count).demand_source_header_id :=
                                       l_resv_tbl (i).demand_source_header_id;
            l_inv_resv_tbl (l_count).demand_source_line_id :=
                                         l_resv_tbl (i).demand_source_line_id;
            l_inv_resv_tbl (l_count).primary_uom_code :=
                                              l_resv_tbl (i).primary_uom_code;
            l_inv_resv_tbl (l_count).secondary_uom_code :=
                                            l_resv_tbl (i).secondary_uom_code;
            l_inv_resv_tbl (l_count).reservation_uom_code :=
                                          l_resv_tbl (i).reservation_uom_code;
            l_inv_resv_tbl (l_count).reservation_quantity :=
                                          l_resv_tbl (i).reservation_quantity;
            l_inv_resv_tbl (l_count).primary_reservation_quantity :=
                                  l_resv_tbl (i).primary_reservation_quantity;
            l_inv_resv_tbl (l_count).secondary_reservation_quantity :=
                                l_resv_tbl (i).secondary_reservation_quantity;
            l_inv_resv_tbl (l_count).detailed_quantity :=
                                             l_resv_tbl (i).detailed_quantity;
            l_inv_resv_tbl (l_count).secondary_detailed_quantity :=
                                   l_resv_tbl (i).secondary_detailed_quantity;
            l_inv_resv_tbl (l_count).supply_source_type_id :=
                                         l_resv_tbl (i).supply_source_type_id;
            l_inv_resv_tbl (l_count).supply_source_header_id :=
                                       l_resv_tbl (i).supply_source_header_id;
            l_inv_resv_tbl (l_count).supply_source_line_id :=
                                         l_resv_tbl (i).supply_source_line_id;
            l_inv_resv_tbl (l_count).revision := l_resv_tbl (i).revision;
            l_inv_resv_tbl (l_count).subinventory_code :=
                                             l_resv_tbl (i).subinventory_code;
            l_inv_resv_tbl (l_count).locator_id := l_resv_tbl (i).locator_id;
            l_inv_resv_tbl (l_count).lot_number := l_resv_tbl (i).lot_number;
            l_inv_resv_tbl (l_count).lpn_id := l_resv_tbl (i).lpn_id;
         END IF;
      END LOOP;

      IF g_debug <= gme_debug.g_log_statement THEN
         gme_debug.put_line
                  (   g_pkg_name
                   || '.'
                   || l_api_name
                   || ':No. of reservations passed to create suggestions is '
                   || l_count);
      END IF;

      IF (p_plan_tasks = 'Y') THEN
         l_plan_tasks := TRUE;
      ELSE
         l_plan_tasks := FALSE;
      END IF;

      wms_engine_pvt.create_suggestions
                         (p_api_version              => 1.0
                         ,p_init_msg_list            => fnd_api.g_false
                         ,p_commit                   => fnd_api.g_false
                         ,p_validation_level         => fnd_api.g_valid_level_none
                         ,x_return_status            => l_return_status
                         ,x_msg_count                => l_msg_count
                         ,x_msg_data                 => l_msg_data
                         ,p_transaction_temp_id      => p_mo_line_rec.line_id
                         ,p_reservations             => l_inv_resv_tbl
                         ,p_suggest_serial           => fnd_api.g_false
                         ,p_simulation_mode          => wms_engine_pvt.g_no_simulation
                         ,p_simulation_id            => NULL
                         ,p_plan_tasks               => l_plan_tasks
                         ,p_quick_pick_flag          => 'N');

      IF g_debug <= gme_debug.g_log_statement THEN
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || 'create suggestions returns '
                             || l_return_status);
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || 'create suggestions mesg '
                             || l_msg_data);
      END IF;

      IF (l_return_status <> fnd_api.g_ret_sts_success) THEN
         RAISE create_suggestions_err;
      END IF;

      OPEN cur_detailed_qty (p_mo_line_rec.line_id);

      FETCH cur_detailed_qty
       INTO l_qty_detailed, l_sec_qty_detailed;

      CLOSE cur_detailed_qty;

      IF g_debug <= gme_debug.g_log_statement THEN
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ':Qty allocated '
                             || l_qty_detailed);
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ':Sec Qty allocated '
                             || l_sec_qty_detailed);
      END IF;

      l_mo_line_rec := p_mo_line_rec;

      IF (NVL (l_qty_detailed, 0) > 0) THEN
         IF g_debug <= gme_debug.g_log_statement THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ':Get mmtt recs and create pick slips ');
         END IF;

         l_mo_line_rec.quantity_detailed := l_qty_detailed;
         l_mo_line_rec.secondary_quantity_detailed := l_sec_qty_detailed;         

         -- Bug 8481421 Let's update the qty tree to reflect the reservations suggested by WMS.
         IF l_inv_resv_tbl.COUNT > 0 THEN
            --initializing the index
            l_res_ordered_index  := l_inv_resv_tbl.FIRST; 
          LOOP            
            --item_rec is required to get all UOM quantities from MTL_SYSTEM_ITEMS
            SELECT *
            INTO item_rec
            FROM MTL_SYSTEM_ITEMS
            WHERE organization_id = l_inv_resv_tbl(l_res_ordered_index).organization_id
            AND inventory_item_id = l_inv_resv_tbl(l_res_ordered_index).inventory_item_id;
         
            l_primary_uom:= item_rec.primary_uom_code;
            l_secondary_uom:= item_rec.secondary_uom_code;
            l_revision_control_code:=item_rec.revision_qty_control_code;
            l_lot_control_code:= item_rec.lot_control_code;
            l_serial_number_control_code:= item_rec.serial_number_control_code;
            -- convert revision/lot control indicators into boolean
            IF l_revision_control_code = 2 THEN
               l_is_revision_control  := TRUE;
            ELSE
               l_is_revision_control  := FALSE;
            END IF;
            --
            IF l_lot_control_code = 2 THEN
               l_is_lot_control  := TRUE;
            ELSE
               l_is_lot_control  := FALSE;
            END IF;
            --
            IF l_serial_number_control_code = 2 THEN
               l_is_serial_control  := TRUE;
            ELSE
               l_is_serial_control  := FALSE;
            END IF;


            l_reservation_id := l_inv_resv_tbl(l_res_ordered_index).reservation_id;
            l_prev_rsv_detailed_qty := nvl(l_inv_resv_tbl(l_res_ordered_index).detailed_quantity,0);     
            l_prev_rsv_detailed_qty2 := nvl(l_inv_resv_tbl(l_res_ordered_index).secondary_detailed_quantity,0);
                
            BEGIN
              SELECT NVL(SUM(ABS(primary_quantity)), 0)
                   , NVL(SUM(ABS(secondary_transaction_quantity)), 0)
                INTO l_reservation_detailed_qty
                   , l_rsv_detailed_qty2
                FROM mtl_material_transactions_temp
               WHERE organization_id = p_mo_line_rec.organization_id
                 AND reservation_id = l_reservation_id;
            EXCEPTION
              WHEN NO_DATA_FOUND THEN
                l_reservation_detailed_qty  := 0;
                l_rsv_detailed_qty2         := 0;
            END;
         
            --update quantity tree                  
            inv_quantity_tree_pub.update_quantities(
                  p_api_version_number          => 1.0
                , p_init_msg_lst                => fnd_api.g_false
                , x_return_status               => l_api_return_status
                , x_msg_count                   => l_msg_count
                , x_msg_data                    => l_msg_data
                , p_organization_id             => p_mo_line_rec.organization_id
                , p_inventory_item_id           => p_mo_line_rec.inventory_item_id
                , p_tree_mode                   => inv_quantity_tree_pub.g_reservation_mode
                , p_is_revision_control         => l_is_revision_control
                , p_is_lot_control              => l_is_lot_control
                , p_is_serial_control           => l_is_serial_control
                , p_demand_source_type_id       => l_inv_resv_tbl(l_res_ordered_index).demand_source_type_id
                , p_demand_source_header_id     => l_inv_resv_tbl(l_res_ordered_index).demand_source_header_id
                , p_demand_source_line_id       => l_inv_resv_tbl(l_res_ordered_index).demand_source_line_id 
                , p_demand_source_name          => NULL
                , p_revision                    => l_inv_resv_tbl(l_res_ordered_index).revision
                , p_lot_number                  => l_inv_resv_tbl(l_res_ordered_index).lot_number
                , p_lot_expiration_date         => SYSDATE
                , p_subinventory_code           => l_inv_resv_tbl(l_res_ordered_index).subinventory_code
                , p_locator_id                  => l_inv_resv_tbl(l_res_ordered_index).locator_id
                , p_primary_quantity            => -(l_reservation_detailed_qty - l_prev_rsv_detailed_qty)
                , p_secondary_quantity          => -(l_rsv_detailed_qty2        - l_prev_rsv_detailed_qty2)
                , p_lpn_id                      => l_inv_resv_tbl(l_res_ordered_index).lpn_id 
                , p_quantity_type               => inv_quantity_tree_pub.g_qr_same_demand
                , x_qoh                         => l_qty_on_hand
                , x_rqoh                        => l_qty_res_on_hand
                , x_qr                          => l_qty_res
                , x_qs                          => l_qty_sug
                , x_att                         => l_qty_att
                , x_atr                         => l_qty_available_to_reserve
                , p_grade_code                  => p_mo_line_rec.grade_code
                , x_sqoh                        => l_sec_qty_on_hand
                , x_srqoh                       => l_sec_qty_res_on_hand
                , x_sqr                         => l_sec_qty_res
                , x_sqs                         => l_sec_qty_sug
                , x_satt                        => l_sec_qty_att
                , x_satr                        => l_sec_qty_available_to_reserve
            );
            IF l_api_return_status <> fnd_api.g_ret_sts_success THEN     
               RAISE fnd_api.g_exc_unexpected_error;
            END IF;
              
            --handle conversion to reservation UOM
            IF l_inv_resv_tbl(l_res_ordered_index).reservation_uom_code IS NULL THEN
               --when missing rsv UOM, assume primary UOM
               l_rsv_detailed_qty_conv := l_reservation_detailed_qty;
            ELSIF l_inv_resv_tbl(l_res_ordered_index).reservation_uom_code = l_primary_uom THEN
               --reservation UOM = primary UOM
               l_rsv_detailed_qty_conv := l_reservation_detailed_qty;
            ELSE
               l_rsv_detailed_qty_conv  := inv_convert.inv_um_convert(
                        item_id                 => p_mo_line_rec.inventory_item_id
                      , PRECISION               => NULL
                      , from_quantity           => l_reservation_detailed_qty
                      , from_unit               => l_primary_uom
                      , to_unit                 => l_inv_resv_tbl(l_res_ordered_index).reservation_uom_code
                      , from_name               => NULL
                      , to_name                 => NULL
               );
                  
               IF (l_rsv_detailed_qty_conv = -99999) THEN             
                  fnd_message.set_name('INV', 'INV-CANNOT CONVERT');
                  fnd_message.set_token('UOM', l_primary_uom);
                  fnd_message.set_token('ROUTINE', 'Pick Release process');
                  fnd_msg_pub.ADD;
                  RAISE fnd_api.g_exc_unexpected_error;
               END IF;
            END IF;       

            UPDATE mtl_reservations
            SET detailed_quantity = l_reservation_detailed_qty
               ,secondary_detailed_quantity = l_rsv_detailed_qty2
            WHERE reservation_id = l_reservation_id;           

            EXIT WHEN l_res_ordered_index = l_inv_resv_tbl.LAST;
            l_res_ordered_index:= l_inv_resv_tbl.NEXT(l_res_ordered_index);
          END LOOP;
         END IF;
         -- Bug 8481421 end of update quantity loop

         FOR get_mmtt IN cur_mmtt (p_mo_line_rec.line_id) LOOP
            IF g_debug <= gme_debug.g_log_statement THEN
               gme_debug.put_line(g_pkg_name || '.' || l_api_name
                   || ' Calling ASSIGNTT with temp id ' || get_mmtt.transaction_temp_id);
            END IF;
          
           
            -- Bug 6778259 - Assign user task properly.
            WMS_RULE_PVT.assigntt
                 (p_api_version                => 1.0,
                  p_task_id                    =>  get_mmtt.transaction_temp_id,
                  x_return_status              => l_return_status,
                  x_msg_count                  => l_msg_count,
                  x_msg_data                   => l_msg_data
                  );

            IF g_debug <= gme_debug.g_log_statement THEN
               gme_debug.put_line(g_pkg_name || '.' || l_api_name
                   || ':Return from ASSIGNTT IS ' || l_return_status);

               IF (l_return_status <> fnd_api.g_ret_sts_success) THEN
                  gme_debug.put_line (   g_pkg_name
                                      || '.'
                                      || l_api_name
                                      || 'temp_id is '
                                      || get_mmtt.transaction_temp_id);
                  gme_debug.put_line (   g_pkg_name
                                      || '.'
                                      || l_api_name
                                      || ':Message is '
                                      || l_msg_data);
               END IF;
            END IF;

            -- Bug 6778259 - Use existing exception to return error.
            IF (l_return_status <> fnd_api.g_ret_sts_success) THEN
               RAISE get_pick_slip_err;
            END IF;

            IF get_mmtt.pick_slip_number IS NULL THEN
               IF g_debug <= gme_debug.g_log_statement THEN
                  gme_debug.put_line
                     (   g_pkg_name
                      || '.'
                      || l_api_name
                      || ':Calling inv_pr_pick_slip_number.get_pick_slip_number');
               END IF;

               inv_pr_pick_slip_number.get_pick_slip_number
                     (p_pick_grouping_rule_id      => p_grouping_rule_id
                     ,p_org_id                     => p_mo_line_rec.organization_id
                     ,p_wip_entity_id              => p_mo_line_rec.txn_source_id
                     ,p_rep_schedule_id            => NULL
                     ,p_operation_seq_num          => p_mo_line_rec.txn_source_line_id
                     ,p_dept_id                    => NULL
                     ,p_push_or_pull               => NULL
                     ,p_supply_subinventory        => get_mmtt.transfer_subinventory
                     ,p_supply_locator_id          => get_mmtt.transfer_to_location
                     ,p_project_id                 => NULL
                     ,p_task_id                    => NULL
                     ,p_src_subinventory           => get_mmtt.subinventory_code
                     ,p_src_locator_id             => get_mmtt.locator_id
                     ,p_inventory_item_id          => p_mo_line_rec.inventory_item_id
                     ,p_revision                   => get_mmtt.revision
                     ,p_lot_number                 => NULL
                     ,x_pick_slip_number           => l_pick_slip_number
                     ,x_api_status                 => l_return_status
                     ,x_error_message              => l_msg_data);

               IF g_debug <= gme_debug.g_log_statement THEN
                  gme_debug.put_line
                     (   g_pkg_name
                      || '.'
                      || l_api_name
                      || ':Return from inv_pr_pick_slip_number.get_pick_slip_number '
                      || l_return_status);
                  gme_debug.put_line (   g_pkg_name
                                      || '.'
                                      || l_api_name
                                      || ':l_pick_slip_number is '
                                      || l_pick_slip_number);
                  gme_debug.put_line (   g_pkg_name
                                      || '.'
                                      || l_api_name
                                      || ':Message is '
                                      || l_msg_data);
               END IF;

               IF (l_return_status <> fnd_api.g_ret_sts_success) THEN
                  RAISE get_pick_slip_err;
               END IF;

               UPDATE mtl_material_transactions_temp
                  SET pick_slip_number = l_pick_slip_number,
                      wip_entity_type = gme_common_pvt.g_wip_entity_type_batch
                WHERE transaction_temp_id = get_mmtt.transaction_temp_id;
            ELSE
              UPDATE mtl_material_transactions_temp
              SET wip_entity_type = gme_common_pvt.g_wip_entity_type_batch
              WHERE transaction_temp_id = get_mmtt.transaction_temp_id;
            END IF;
         END LOOP;
         --End of for loop for get_mmtt IN cur_mmtt (p_mo_line_rec.line_id)

         IF (NVL (l_qty_detailed, 0) < p_mo_line_rec.quantity) THEN
            l_backordered_qty :=
                             p_mo_line_rec.quantity - NVL (l_qty_detailed, 0);
            l_mo_line_rec.quantity := NVL (l_qty_detailed, 0);
            g_partially_allocated := TRUE;
         ELSE
            g_fully_allocated := TRUE;
         END IF;
      ELSE
         IF g_debug <= gme_debug.g_log_statement THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ':Close mo lines since no allocations');
         END IF;

         l_backordered_qty := p_mo_line_rec.quantity;
         l_mo_line_rec.line_status := 5;
         g_not_allocated := TRUE;
      END IF;

      IF g_debug <= gme_debug.g_log_statement THEN
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || 'Updating MO line');
      END IF;

      inv_trolin_util.update_row (l_mo_line_rec);

      IF g_debug <= gme_debug.g_log_statement THEN
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || 'After Updating MO line');
      END IF;

      IF g_debug <= gme_debug.g_log_statement THEN
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || 'l_backordered_qty = '
                             || l_backordered_qty);
      END IF;

      IF (l_backordered_qty > 0) THEN
         UPDATE gme_material_details
            SET backordered_qty = l_backordered_qty
          WHERE material_detail_id = p_mo_line_rec.txn_source_line_id;
      END IF;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;
   EXCEPTION
      WHEN create_suggestions_err OR get_pick_slip_err THEN
         x_return_status := l_return_status;
      WHEN OTHERS THEN
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);

         IF g_debug <= gme_debug.g_log_unexpected THEN
            gme_debug.put_line (   'When others exception in '
                                || g_pkg_name
                                || '.'
                                || l_api_name
                                || ' Error is '
                                || SQLERRM);
         END IF;

         x_return_status := fnd_api.g_ret_sts_unexp_error;
   END process_line;
END gme_picking_pvt;
/

COMMIT ;
EXIT;
