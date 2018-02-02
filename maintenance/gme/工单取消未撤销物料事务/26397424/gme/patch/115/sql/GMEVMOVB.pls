/* +======================================================================+ */
/* |    Copyright (c) 2005, 2015 Oracle and/or its affiliates.           | */
/* |                         All rights reserved.                         | */
/* |                           Version 12.0.0                             | */
/* +======================================================================+ */
REM dbdrv: sql ~PROD ~PATH ~FILE none none none package &phase=plb \
REM dbdrv: checkfile(120.10.12010000.17=120.19.12020000.8)(120.9.12000000.16=120.10.12010000.15)(120.9.12000000.11=120.19):~PROD:~PATH:~FILE 
SET VERIFY OFF
WHENEVER SQLERROR EXIT FAILURE ROLLBACK;
WHENEVER OSERROR EXIT FAILURE ROLLBACK;

CREATE OR REPLACE PACKAGE BODY gme_move_orders_pvt AS
/* $Header: GMEVMOVB.pls 120.19.12020000.8 2015/03/13 17:43:03 gmurator ship $ */
   g_debug               VARCHAR2 (5)  := fnd_profile.VALUE ('AFLOG_LEVEL');
   g_pkg_name   CONSTANT VARCHAR2 (30) := 'GME_MOVE_ORDERS_PVT';

/*************************************************************************************************/
/* Oracle Process Manufacturing Process Execution APIs                                           */
/*                                                                                               */
/* File Name: GMEVMOVB.pls                                                                       */
/* Contents:  GME move order related procedures.                                                 */
/* HISTORY                                                                                       */
/* SivakumarG 06-MAR-2006 Bug#5078853                                                            */
/*  update_move_order_lines procedure modified material line and send to the inv api             */
/*  to process the move order line.
/* Swapna K 11-OCT-2007 Bug#6446877                                                              */
/* update_move_order_lines procedure is changed to fetch the alloc uom of the material           */
/* line and send to the inv api to process the move order line.                                  */

-- G. Muratore       16-OCT-2009  Bug 9028327 
--    Add code to update primary and secondary if necessary.           
--    PROCEDURE update_move_order_lines

-- S. Kommineni      17-OCT-2009  Bug 9028327/9030411 
--    Add same code as previous fix to update UOM, primary and secondary if necessary.           
--    PROCEDURE update_move_order_lines

-- G. Muratore       04-FEB-2010  Bug 9268209 
--    Populate carton grouping id for wms orgs for real move orders only.           
--    PROCEDURE create_move_order_lines

-- G. Muratore       31-AUG-2010  Bug 9941121 
--    Populate grouping_rule_id, in move order record, with the value passed into the new parameter.          
--    PROCEDURE create_move_order_hdr

-- G. Muratore       02-DEC-2011  Bug 13076579 
--    The code was changed to pass back the total quantity already detailed not the overall quantity
--    of the move order. This procedure name is misleading. This only gets called by get_open_qty.
--    get_open_qty then uses the value returned to subtract it from the plan or wip plan.
--    PROCEDURE get_pending_move_order_qty 
--
-- A. Mundhe         02-JAN-2013  Bug 17882329 
--    Modified procedure create_move_order_lines. 
--    Changed transaction_type_id to g_ing_Issue instead of g_backflush_transfer.

-- G. Muratore       05-FEB-2015  Bug 20078417 
--    Make sure invisible mo line is still there before calling delete to avoid erroneous errors.
--    PROCEDURE delete_batch_move_orders

-- G. Muratore       13-MAR-2015  Bug 20635929 
--    Initialize the variables for each loop iteration to avoid problems with non transactable ingredients.
--    PROCEDURE delete_batch_move_orders
/*************************************************************************************************/
   PROCEDURE create_move_order_hdr (
      p_organization_id        IN              NUMBER
     ,p_move_order_type        IN              NUMBER
     ,p_grouping_rule_id       IN              NUMBER DEFAULT 0
     ,x_move_order_header_id   OUT NOCOPY      NUMBER
     ,x_return_status          OUT NOCOPY      VARCHAR2)
   IS
      l_api_name    CONSTANT VARCHAR2 (30)         := 'create_move_order_hdr';
      l_return_status        VARCHAR2 (1);
      l_msg_count            NUMBER;
      l_msg_data             VARCHAR2 (2000);
      l_in_trohdr_rec        inv_move_order_pub.trohdr_rec_type;
      l_in_trohdr_val_rec    inv_move_order_pub.trohdr_val_rec_type;
      l_out_trohdr_rec       inv_move_order_pub.trohdr_rec_type;
      l_out_trohdr_val_rec   inv_move_order_pub.trohdr_val_rec_type;
      create_mo_hdr_err      EXCEPTION;
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      x_return_status := fnd_api.g_ret_sts_success;
      l_in_trohdr_rec.organization_id := p_organization_id;
      l_in_trohdr_rec.move_order_type := p_move_order_type;
      l_in_trohdr_rec.operation := inv_globals.g_opr_create;
      l_in_trohdr_rec.request_number := fnd_api.g_miss_char;
      l_in_trohdr_rec.header_id := fnd_api.g_miss_num;
      l_in_trohdr_rec.creation_date := gme_common_pvt.g_timestamp;
      l_in_trohdr_rec.created_by := gme_common_pvt.g_user_ident;
      l_in_trohdr_rec.last_update_date := gme_common_pvt.g_timestamp;
      l_in_trohdr_rec.last_updated_by := gme_common_pvt.g_user_ident;

      -- Bug 9941121 - Initialize grouping_rule_id properly.
      l_in_trohdr_rec.grouping_rule_id := p_grouping_rule_id;

      IF (g_debug IS NOT NULL) THEN
         gme_debug.put_line
                       ('Calling inv_move_order_pub.create_move_order_header');
      END IF;

      inv_move_order_pub.create_move_order_header
                     (p_api_version_number      => 1.0
                     ,p_init_msg_list           => fnd_api.g_false
                     ,p_return_values           => fnd_api.g_false
                     ,p_commit                  => fnd_api.g_false
                     ,x_return_status           => l_return_status
                     ,x_msg_count               => l_msg_count
                     ,x_msg_data                => l_msg_data
                     ,p_trohdr_rec              => l_in_trohdr_rec
                     ,p_trohdr_val_rec          => l_in_trohdr_val_rec
                     ,x_trohdr_rec              => l_out_trohdr_rec
                     ,x_trohdr_val_rec          => l_out_trohdr_val_rec
                     ,p_validation_flag         => inv_move_order_pub.g_validation_yes);

      IF (l_return_status <> fnd_api.g_ret_sts_success) THEN
         RAISE create_mo_hdr_err;
      ELSE
         IF g_debug <= gme_debug.g_log_statement THEN
            gme_debug.put_line ('Header_id is ' || l_out_trohdr_rec.header_id);
            gme_debug.put_line (   'request_number is '
                                || l_out_trohdr_rec.request_number);
         END IF;

         x_move_order_header_id := l_out_trohdr_rec.header_id;
      END IF;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;
   EXCEPTION
      WHEN create_mo_hdr_err THEN
         IF (g_debug <= gme_debug.g_log_error) THEN
            gme_debug.put_line
                   (   'inv_move_order_pub.create_move_order_header returns '
                    || l_return_status);
            gme_debug.put_line ('error message is ' || l_msg_data);
         END IF;

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
   END create_move_order_hdr;

   PROCEDURE create_move_order_lines (
      p_move_order_header_id   IN              NUMBER
     ,p_move_order_type        IN              NUMBER
     ,p_material_details_tbl   IN              gme_common_pvt.material_details_tab
     ,x_material_details_tbl   OUT NOCOPY      gme_common_pvt.material_details_tab
     ,x_trolin_tbl             OUT NOCOPY      inv_move_order_pub.trolin_tbl_type
     ,x_return_status          OUT NOCOPY      VARCHAR2)
   IS
      l_api_name      CONSTANT VARCHAR2 (30)     := 'create_move_order_lines';
      l_return_status          VARCHAR2 (1);
      l_msg_count              NUMBER;
      l_count                  NUMBER;
      l_temp_qty               NUMBER;
      l_txn_enabled_flag       VARCHAR2 (10);
      l_sec_uom_code           VARCHAR2 (10);
      l_msg_data               VARCHAR2 (2000);
      l_item_no                VARCHAR2 (2000);
      l_material_details_tbl   gme_common_pvt.material_details_tab;
      l_in_trolin_tbl          inv_move_order_pub.trolin_tbl_type;
      l_in_trolin_val_tbl      inv_move_order_pub.trolin_val_tbl_type;
      l_out_trolin_val_tbl     inv_move_order_pub.trolin_val_tbl_type;
      create_mo_line_err       EXCEPTION;

      CURSOR cur_item_mst (v_org_id NUMBER, v_inventory_item_id NUMBER)
      IS
         SELECT mtl_transactions_enabled_flag, secondary_uom_code, segment1
           FROM mtl_system_items_b
          WHERE organization_id = v_org_id
            AND inventory_item_id = v_inventory_item_id;
            
      -- 9268209 
      CURSOR cur_wms_enabled (v_org_id NUMBER)
      IS
         SELECT wms_enabled_flag
	 FROM   mtl_parameters
	 WHERE  organization_id = v_org_id;

      l_wms_enabled_flag  VARCHAR2(1);     
             
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      x_return_status := fnd_api.g_ret_sts_success;

      FOR i IN 1 .. p_material_details_tbl.COUNT LOOP
         IF (p_material_details_tbl (i).line_type = -1) THEN
            OPEN cur_item_mst (p_material_details_tbl (i).organization_id
                              ,p_material_details_tbl (i).inventory_item_id);

            FETCH cur_item_mst
             INTO l_txn_enabled_flag, l_sec_uom_code, l_item_no;

            CLOSE cur_item_mst;

            IF (l_txn_enabled_flag = 'Y' AND NVL(p_material_details_tbl (i).phantom_type,0) = 0) THEN
               l_count := l_material_details_tbl.COUNT + 1;
               l_material_details_tbl (l_count) := p_material_details_tbl (i);
               l_in_trolin_tbl (l_count).operation :=
                                                     inv_globals.g_opr_create;
               l_in_trolin_tbl (l_count).header_id := p_move_order_header_id;
               l_in_trolin_tbl (l_count).inventory_item_id :=
                           l_material_details_tbl (l_count).inventory_item_id;
               l_in_trolin_tbl (l_count).organization_id :=
                             l_material_details_tbl (l_count).organization_id;
               l_in_trolin_tbl (l_count).quantity :=
                                    l_material_details_tbl (l_count).plan_qty;
               l_in_trolin_tbl (l_count).uom_code :=
                                      l_material_details_tbl (l_count).dtl_um;
               l_in_trolin_tbl (l_count).revision :=
                                    l_material_details_tbl (l_count).revision;
               l_in_trolin_tbl (l_count).txn_source_id :=
                                    l_material_details_tbl (l_count).batch_id;
               l_in_trolin_tbl (l_count).txn_source_line_id :=
                          l_material_details_tbl (l_count).material_detail_id;
               l_in_trolin_tbl (l_count).date_required :=
                   l_material_details_tbl (l_count).material_requirement_date;
               l_in_trolin_tbl (l_count).creation_date :=
                                                   gme_common_pvt.g_timestamp;
               l_in_trolin_tbl (l_count).created_by :=
                                                  gme_common_pvt.g_user_ident;
               l_in_trolin_tbl (l_count).last_update_date :=
                                                   gme_common_pvt.g_timestamp;
               l_in_trolin_tbl (l_count).last_updated_by :=
                                                  gme_common_pvt.g_user_ident;                                                  
               /* Bug 17882329 Changed to g_ing_Issue instead of g_backflush_transfer*/                          IF (p_move_order_type = gme_common_pvt.g_invis_move_order_type) THEN
                   l_in_trolin_tbl (l_count).transaction_type_id :=
                                          gme_common_pvt.g_ing_issue; 
               ELSE
                   l_in_trolin_tbl (l_count).transaction_type_id :=
                                          gme_common_pvt.g_backflush_transfer; 
               END IF;

               l_in_trolin_tbl (l_count).transaction_source_type_id :=
                                             gme_common_pvt.g_txn_source_type;

               /* Populate below values only for explicit move orders */
               IF (p_move_order_type <> gme_common_pvt.g_invis_move_order_type) THEN
                  l_in_trolin_tbl (l_count).to_subinventory_code :=
                                l_material_details_tbl (l_count).subinventory;
                  l_in_trolin_tbl (l_count).to_locator_id :=
                                  l_material_details_tbl (l_count).locator_id;
                                  
                  -- 9268209 - Populate carton grouping id for wms orgs. 
                  OPEN cur_wms_enabled (p_material_details_tbl (i).organization_id);

                  FETCH cur_wms_enabled
                   INTO l_wms_enabled_flag;

                  CLOSE cur_wms_enabled;
                  
                  IF NVL(l_wms_enabled_flag,'N') = 'Y' THEN                                                                         
                     select WSH_DELIVERY_GROUP_S.nextval into
                     l_in_trolin_tbl (l_count).carton_grouping_id from dual;                      
                  END IF;	                                
                  
               END IF;
	       IF (l_sec_uom_code IS NOT NULL) THEN
	         IF (l_material_details_tbl (l_count).dtl_um <> l_sec_uom_code) THEN 
                   l_temp_qty := inv_convert.inv_um_convert
                                      (item_id            => l_material_details_tbl (l_count).inventory_item_id
                                      ,PRECISION          => gme_common_pvt.g_precision
                                      ,from_quantity      => l_material_details_tbl (l_count).plan_qty
                                      ,from_unit          => l_material_details_tbl (l_count).dtl_um
                                      ,to_unit            => l_sec_uom_code
                                      ,from_name          => NULL
                                      ,to_name            => NULL);
                   IF l_temp_qty < 0 THEN
                     fnd_message.set_name ('GMI', 'IC_API_UOM_CONVERSION_ERROR');
                     fnd_message.set_token ('ITEM_NO', l_item_no);
                     fnd_message.set_token ('FROM_UOM', l_material_details_tbl (l_count).dtl_um);
                     fnd_message.set_token ('TO_UOM', l_sec_uom_code);                  
                     fnd_msg_pub.add;
                     l_temp_qty := NULL;
                   END IF;
 	         ELSE
		   l_temp_qty := l_material_details_tbl (l_count).plan_qty;
	         END IF;
	         l_in_trolin_tbl (l_count).secondary_quantity := l_temp_qty; 
	         l_in_trolin_tbl (l_count).secondary_uom      := l_sec_uom_code;
               END IF;	         
            ELSE
               x_material_details_tbl (x_material_details_tbl.COUNT + 1) :=
                                                   p_material_details_tbl (i);
            END IF;
         ELSE
            x_material_details_tbl (x_material_details_tbl.COUNT + 1) :=
                                                   p_material_details_tbl (i);
         END IF;
      END LOOP;

      IF g_debug <= gme_debug.g_log_statement THEN
         gme_debug.put_line
            (   'Calling inv_move_order_pub.create_move_order_lines with no. of lines = '
             || l_in_trolin_tbl.COUNT);
      END IF;
      /* Bug 4866700 added check to call only if records are there in table */
      IF (l_in_trolin_tbl.COUNT > 0) THEN
        inv_move_order_pub.create_move_order_lines
                       (p_api_version_number      => 1.0
                       ,p_init_msg_list           => fnd_api.g_false
                       ,p_return_values           => fnd_api.g_false
                       ,p_commit                  => fnd_api.g_false
                       ,x_return_status           => l_return_status
                       ,x_msg_count               => l_msg_count
                       ,x_msg_data                => l_msg_data
                       ,p_trolin_tbl              => l_in_trolin_tbl
                       ,p_trolin_val_tbl          => l_in_trolin_val_tbl
                       ,x_trolin_tbl              => x_trolin_tbl
                       ,x_trolin_val_tbl          => l_out_trolin_val_tbl
                       ,p_validation_flag         => inv_move_order_pub.g_validation_yes);
        
        IF (l_return_status <> fnd_api.g_ret_sts_success) THEN
           RAISE create_mo_line_err;
        ELSE
           IF g_debug <= gme_debug.g_log_statement THEN
              gme_debug.put_line (   'No. of move order lines created = '
                                  || x_trolin_tbl.COUNT);
           END IF;
        
           /* Populate out structure only for invisible move order */
           IF (p_move_order_type = gme_common_pvt.g_invis_move_order_type) THEN
              FOR i IN 1 .. l_material_details_tbl.COUNT LOOP
                 l_material_details_tbl (i).move_order_line_id :=
                                                       x_trolin_tbl (i).line_id;
                 x_material_details_tbl (x_material_details_tbl.COUNT + 1) :=
                                                     l_material_details_tbl (i);
              END LOOP;
           END IF;
        END IF;
      END IF;
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;
   EXCEPTION
      WHEN create_mo_line_err THEN
         IF g_debug <= gme_debug.g_log_error THEN
            gme_debug.put_line
                    (   'inv_move_order_pub.create_move_order_lines returns '
                     || l_return_status);
            gme_debug.put_line ('message count is ' || l_msg_count);

            FOR i IN 1 .. l_msg_count LOOP
               l_msg_data :=
                         fnd_msg_pub.get (p_msg_index      => i
                                         ,p_encoded        => 'T');
               gme_debug.put_line ('error message is ' || l_msg_data);
            END LOOP;
         END IF;

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
   END create_move_order_lines;

   PROCEDURE create_batch_move_order (
      p_batch_header_rec       IN              gme_batch_header%ROWTYPE
     ,p_material_details_tbl   IN              gme_common_pvt.material_details_tab
     ,x_return_status          OUT NOCOPY      VARCHAR2)
   IS
      l_api_name      CONSTANT VARCHAR2 (30)     := 'create_batch_move_order';
      l_return_status          VARCHAR2 (1);
      l_from_uom               VARCHAR2 (3);
      l_to_uom                 VARCHAR2 (3);
      l_item_no                VARCHAR2 (2000);
      l_msg_data               VARCHAR2 (2000);
      l_mtl_txns_enabled_flag  mtl_system_items_kfv.mtl_transactions_enabled_flag%TYPE;
      l_concatenated_segments  mtl_system_items_kfv.concatenated_segments%TYPE; 
      l_primary_uom_code       mtl_system_items_kfv.primary_uom_code%TYPE;

      l_mtl_dtl_tbl            gme_common_pvt.material_details_tab;
      l_mtl_dtl_tbl_out        gme_common_pvt.material_details_tab;
      l_trolin_tbl             inv_move_order_pub.trolin_tbl_type;
      l_is_revision_control    BOOLEAN;
      l_is_lot_control         BOOLEAN;
      l_is_serial_control      BOOLEAN;
      l_plan_qty_prim          NUMBER;
      l_count                  NUMBER;
      l_msg_count              NUMBER;
      l_move_order_header_id   NUMBER;
      l_qoh                    NUMBER;
      l_rqoh                   NUMBER;
      l_qr                     NUMBER;
      l_qs                     NUMBER;
      l_att                    NUMBER;
      l_atr                    NUMBER;
      l_sqoh                   NUMBER;
      l_srqoh                  NUMBER;
      l_sqr                    NUMBER;
      l_sqs                    NUMBER;
      l_satt                   NUMBER;
      l_satr                   NUMBER;
      l_diff_qty               NUMBER;
      CURSOR cur_get_item_info (v_org_id NUMBER, v_inventory_item_id NUMBER)
      IS
         SELECT mtl_transactions_enabled_flag, concatenated_segments, primary_uom_code
           FROM mtl_system_items_kfv
          WHERE inventory_item_id = v_inventory_item_id
            AND organization_id = v_org_id;

      create_mo_err            EXCEPTION;
      uom_conversion_err       EXCEPTION;
      unable_to_query_tree     EXCEPTION;
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;
      
      x_return_status := fnd_api.g_ret_sts_success;
      FOR i IN 1 .. p_material_details_tbl.COUNT LOOP
      
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Iteration of loop is '||i);
         gme_debug.put_line ('line_type is '||p_material_details_tbl (i).line_type);
         gme_debug.put_line ('subinventory is '||p_material_details_tbl (i).subinventory);
         gme_debug.put_line ('material_requirement_date is '||TO_CHAR(p_material_details_tbl (i).material_requirement_date,'DD-MON-YYYY HH24:MI:SS'));
         gme_debug.put_line ('g_move_order_timefence is '||NVL(gme_common_pvt.g_move_order_timefence, 99999));
      END IF;
      
         IF (    p_material_details_tbl (i).line_type = -1
             AND p_material_details_tbl (i).subinventory IS NOT NULL
             AND p_material_details_tbl (i).material_requirement_date <
                    (SYSDATE + NVL (gme_common_pvt.g_move_order_timefence, 0) ) ) THEN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('point 1');
      END IF;
                    
            OPEN cur_get_item_info
                                 (p_material_details_tbl (i).organization_id
                                 ,p_material_details_tbl (i).inventory_item_id);

            FETCH cur_get_item_info
             INTO l_mtl_txns_enabled_flag, l_concatenated_segments, l_primary_uom_code;

            CLOSE cur_get_item_info;
            IF (l_mtl_txns_enabled_flag = 'Y') THEN
               gme_transactions_pvt.query_quantities
                  (x_return_status                => l_return_status
                  ,x_msg_count                    => l_msg_count
                  ,x_msg_data                     => l_msg_data
                  ,p_organization_id              => p_material_details_tbl
                                                                           (i).organization_id
                  ,p_inventory_item_id            => p_material_details_tbl
                                                                           (i).inventory_item_id
                  ,p_tree_mode                    => gme_common_pvt.g_tree_transaction_mode
                  ,p_grade_code                   => NULL
                  ,p_demand_source_header_id      => p_material_details_tbl
                                                                           (i).batch_id
                  ,p_demand_source_line_id        => p_material_details_tbl
                                                                           (i).material_detail_id
                  ,p_revision                     => p_material_details_tbl
                                                                           (i).revision
                  ,p_lot_number                   => NULL
                  ,p_subinventory_code            => p_material_details_tbl
                                                                           (i).subinventory
                  ,p_locator_id                   => NULL
                  ,x_qoh                          => l_qoh
                  ,x_rqoh                         => l_rqoh
                  ,x_qr                           => l_qr
                  ,x_qs                           => l_qs
                  ,x_att                          => l_att
                  ,x_atr                          => l_atr
                  ,x_sqoh                         => l_sqoh
                  ,x_srqoh                        => l_srqoh
                  ,x_sqr                          => l_sqr
                  ,x_sqs                          => l_sqs
                  ,x_satt                         => l_satt
                  ,x_satr                         => l_satr);
               IF (l_return_status = fnd_api.g_ret_sts_success) THEN
                  IF g_debug <= gme_debug.g_log_statement THEN
                     gme_debug.put_line (   'item = '
                                         || l_concatenated_segments);
                     gme_debug.put_line
                                       (   'subinventory = '
                                        || p_material_details_tbl (i).subinventory);
                     gme_debug.put_line ('l_att = ' || l_att);
                  END IF;
                  IF (l_primary_uom_code <>
                                             p_material_details_tbl (i).dtl_um) THEN
                     l_plan_qty_prim :=
                        inv_convert.inv_um_convert
                           (item_id            => p_material_details_tbl (i).inventory_item_id
                           ,PRECISION          => gme_common_pvt.g_precision
                           ,from_quantity      => p_material_details_tbl (i).plan_qty
                           ,from_unit          => p_material_details_tbl (i).dtl_um
                           ,to_unit            => l_primary_uom_code
                           ,from_name          => NULL
                           ,to_name            => NULL);

                     IF (l_plan_qty_prim < 0) THEN
                        l_item_no := l_concatenated_segments;
                        l_from_uom := p_material_details_tbl (i).dtl_um;
                        l_to_uom := l_primary_uom_code;
                        RAISE uom_conversion_err;
                     END IF;
                  ELSE
                     l_plan_qty_prim := p_material_details_tbl (i).plan_qty;
                  END IF;
                  IF (l_att < l_plan_qty_prim) THEN
                     l_count := l_mtl_dtl_tbl.COUNT + 1;
                     l_mtl_dtl_tbl (l_count) := p_material_details_tbl (i);
                     l_diff_qty := l_plan_qty_prim - l_att;
                     
                     IF (l_diff_qty > l_plan_qty_prim) THEN
                       l_diff_qty := l_plan_qty_prim;
                     END IF;
                     IF (l_primary_uom_code <>
                                             p_material_details_tbl (i).dtl_um) THEN
                        l_mtl_dtl_tbl (l_count).plan_qty :=
                           inv_convert.inv_um_convert
                              (item_id            => p_material_details_tbl
                                                                           (i).inventory_item_id
                              ,PRECISION          => gme_common_pvt.g_precision
                              ,from_quantity      => l_diff_qty
                              ,from_unit          => l_primary_uom_code
                              ,to_unit            => p_material_details_tbl
                                                                           (i).dtl_um
                              ,from_name          => NULL
                              ,to_name            => NULL);
                     ELSE
                        l_mtl_dtl_tbl (l_count).plan_qty := l_diff_qty;
                     END IF;
                  END IF;
               ELSE
                  RAISE unable_to_query_tree;
               END IF;
            END IF;
         END IF;
      END LOOP;
      IF (l_count > 0) THEN
         -- Bug 17722332 -- New default parameter for picking rule during batch create.
         gme_move_orders_pvt.create_move_order_hdr
                    (p_organization_id           => p_batch_header_rec.organization_id
                    ,p_move_order_type           => gme_common_pvt.g_move_order_type
                    ,p_grouping_rule_id          => gme_common_pvt.g_pick_slip_grouping_rule_id
                    ,x_move_order_header_id      => l_move_order_header_id
                    ,x_return_status             => l_return_status);

         IF (l_return_status <> fnd_api.g_ret_sts_success) THEN
            RAISE create_mo_err;
         ELSE
            gme_move_orders_pvt.create_move_order_lines
                      (p_move_order_header_id      => l_move_order_header_id
                      ,p_move_order_type           => gme_common_pvt.g_move_order_type
                      ,p_material_details_tbl      => l_mtl_dtl_tbl
                      ,x_material_details_tbl      => l_mtl_dtl_tbl_out
                      ,x_trolin_tbl                => l_trolin_tbl
                      ,x_return_status             => l_return_status);

            IF (l_return_status <> fnd_api.g_ret_sts_success) THEN
               RAISE create_mo_err;
            END IF;
         END IF;
      END IF;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;
   EXCEPTION
      WHEN create_mo_err OR unable_to_query_tree THEN
         x_return_status := l_return_status;
      WHEN uom_conversion_err THEN
         x_return_status := fnd_api.g_ret_sts_error;
         fnd_message.set_name ('GMI', 'IC_API_UOM_CONVERSION_ERROR');
         fnd_message.set_token ('ITEM_NO', l_item_no);
         fnd_message.set_token ('FROM_UOM', l_from_uom);
         fnd_message.set_token ('TO_UOM', l_to_uom);
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
   END create_batch_move_order;

   PROCEDURE get_move_order_lines (
      p_organization_id      IN              NUMBER
     ,p_batch_id             IN              NUMBER
     ,p_material_detail_id   IN              NUMBER
     ,x_mo_line_tbl          OUT NOCOPY      gme_common_pvt.mo_lines_tab
     ,x_return_status        OUT NOCOPY      VARCHAR2)
   IS
      l_api_name   CONSTANT VARCHAR2 (30) := 'get_move_order_lines';

      CURSOR cur_move_order_lines
      IS
         SELECT   l.*
             FROM mtl_txn_request_lines l, mtl_txn_request_headers h
            WHERE l.organization_id = p_organization_id
              AND transaction_source_type_id =
                                              gme_common_pvt.g_txn_source_type
              AND l.txn_source_id = p_batch_id
              AND l.txn_source_line_id = p_material_detail_id
              AND l.line_status NOT IN (5, 6)
              AND h.header_id = l.header_id
              AND h.move_order_type NOT IN
                     (gme_common_pvt.g_invis_move_order_type
                     ,inv_globals.g_move_order_put_away)
         ORDER BY l.header_id, l.line_id;
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      x_return_status := fnd_api.g_ret_sts_success;

      OPEN cur_move_order_lines;

      FETCH cur_move_order_lines
      BULK COLLECT INTO x_mo_line_tbl;

      CLOSE cur_move_order_lines;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name||' No of lines '||x_mo_line_tbl.count);
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
   END get_move_order_lines;

   PROCEDURE delete_move_order_lines (
      p_organization_id        IN              NUMBER
     ,p_batch_id               IN              NUMBER
     ,p_material_detail_id     IN              NUMBER
     ,p_invis_move_line_id     IN              NUMBER DEFAULT NULL
     ,p_invis_move_header_id   IN              NUMBER DEFAULT NULL
     ,x_return_status          OUT NOCOPY      VARCHAR2)
   IS
      l_api_name    CONSTANT VARCHAR2 (30)       := 'delete_move_order_lines';
      l_return_status        VARCHAR2 (1);
      l_row                  NUMBER;
      l_count                NUMBER;
      l_curr_header_id       NUMBER                                 := 0;
      l_msg_count            NUMBER;
      l_msg_data             VARCHAR2 (2000);
      l_trohdr_rec           inv_move_order_pub.trohdr_rec_type;
      l_out_trohdr_rec       inv_move_order_pub.trohdr_rec_type;
      l_out_trohdr_val_rec   inv_move_order_pub.trohdr_val_rec_type;
      l_trolin_val_tbl       inv_move_order_pub.trolin_val_tbl_type;
      l_trolin_tbl           inv_move_order_pub.trolin_tbl_type;
      l_old_trolin_tbl       inv_move_order_pub.trolin_tbl_type;
      l_out_trolin_tbl       inv_move_order_pub.trolin_tbl_type;
      l_line_tbl             gme_common_pvt.mo_lines_tab;
      delete_mo_line_err     EXCEPTION;
      delete_mo_hdr_err      EXCEPTION;
      get_mo_line_err        EXCEPTION;

      CURSOR cur_move_order_lines (v_header_id NUMBER)
      IS
         SELECT COUNT (1)
           FROM mtl_txn_request_lines
          WHERE header_id = v_header_id;
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      x_return_status := fnd_api.g_ret_sts_success;
      gme_move_orders_pvt.get_move_order_lines
                                (p_organization_id         => p_organization_id
                                ,p_batch_id                => p_batch_id
                                ,p_material_detail_id      => p_material_detail_id
                                ,x_mo_line_tbl             => l_line_tbl
                                ,x_return_status           => l_return_status);

      IF (l_return_status <> fnd_api.g_ret_sts_success) THEN
         RAISE get_mo_line_err;
      END IF;

      FOR i IN 1 .. l_line_tbl.COUNT LOOP
         l_trolin_tbl (i).operation := inv_globals.g_opr_delete;
         l_trolin_tbl (i).line_id := l_line_tbl (i).line_id;
         l_trolin_tbl (i).header_id := l_line_tbl (i).header_id;
      END LOOP;

      IF (p_invis_move_line_id IS NOT NULL) THEN
         l_row := l_trolin_tbl.COUNT + 1;
         l_trolin_tbl (l_row).operation := inv_globals.g_opr_delete;
         l_trolin_tbl (l_row).line_id := p_invis_move_line_id;
         l_trolin_tbl (l_row).header_id := p_invis_move_header_id;
      END IF;

      IF (l_trolin_tbl.count = 0) THEN
        IF (g_debug <= gme_debug.g_log_statement) THEN
          gme_debug.put_line('No move order lines to delete');
        END IF;
      	RETURN;
      END IF;
      IF (g_debug <= gme_debug.g_log_statement) THEN
         gme_debug.put_line
                 (   'Calling inv_move_order_pub.process_move_order_line in '
                  || l_api_name);
      END IF;

      inv_move_order_pub.process_move_order_line
                                        (p_api_version_number      => 1.0
                                        ,p_init_msg_list           => fnd_api.g_false
                                        ,p_return_values           => fnd_api.g_false
                                        ,p_commit                  => fnd_api.g_false
                                        ,x_return_status           => l_return_status
                                        ,x_msg_count               => l_msg_count
                                        ,x_msg_data                => l_msg_data
                                        ,p_trolin_tbl              => l_trolin_tbl
                                        ,p_trolin_old_tbl          => l_old_trolin_tbl
                                        ,x_trolin_tbl              => l_out_trolin_tbl);

      IF (l_return_status <> fnd_api.g_ret_sts_success) THEN
         RAISE delete_mo_line_err;
      END IF;

      FOR i IN 1 .. l_trolin_tbl.COUNT LOOP
         IF (l_trolin_tbl (i).header_id IS NOT NULL) THEN
            IF (l_curr_header_id <> l_trolin_tbl (i).header_id) THEN
               l_curr_header_id := l_trolin_tbl (i).header_id;

               OPEN cur_move_order_lines (l_trolin_tbl (i).header_id);

               FETCH cur_move_order_lines
                INTO l_count;

               CLOSE cur_move_order_lines;

               IF (l_count = 0) THEN
                  l_trohdr_rec.header_id := l_trolin_tbl (i).header_id;
                  l_trohdr_rec.operation := inv_globals.g_opr_delete;

                  IF (g_debug <= gme_debug.g_log_statement) THEN
                     gme_debug.put_line
                        (   'Calling inv_move_order_pub.process_move_order in '
                         || l_api_name);
                  END IF;

                  inv_move_order_pub.process_move_order
                                    (p_api_version_number      => 1.0
                                    ,p_init_msg_list           => fnd_api.g_false
                                    ,p_return_values           => fnd_api.g_false
                                    ,p_commit                  => fnd_api.g_false
                                    ,x_return_status           => l_return_status
                                    ,x_msg_count               => l_msg_count
                                    ,x_msg_data                => l_msg_data
                                    ,p_trohdr_rec              => l_trohdr_rec
                                    ,x_trohdr_rec              => l_out_trohdr_rec
                                    ,x_trohdr_val_rec          => l_out_trohdr_val_rec
                                    ,x_trolin_tbl              => l_out_trolin_tbl
                                    ,x_trolin_val_tbl          => l_trolin_val_tbl);

                  IF (l_return_status <> fnd_api.g_ret_sts_success) THEN
                     RAISE delete_mo_hdr_err;
                  END IF;
               END IF;
            END IF;
         END IF;
      END LOOP;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;
   EXCEPTION
      WHEN get_mo_line_err THEN
         x_return_status := l_return_status;
      WHEN delete_mo_line_err THEN
         IF (g_debug IS NOT NULL) THEN
            gme_debug.put_line
                    (   'inv_move_order_pub.process_move_order_line returns '
                     || l_return_status);
            gme_debug.put_line ('error message is ' || l_msg_data);
         END IF;

         x_return_status := l_return_status;
      WHEN delete_mo_hdr_err THEN
         IF (g_debug IS NOT NULL) THEN
            gme_debug.put_line
                         (   'inv_move_order_pub.process_move_order returns '
                          || l_return_status);
            gme_debug.put_line ('error message is ' || l_msg_data);
         END IF;

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
   END delete_move_order_lines;


   PROCEDURE update_move_order_lines (
      p_batch_id             IN              NUMBER
     ,p_material_detail_id   IN              NUMBER
     ,p_new_qty              IN              NUMBER := NULL
     ,p_new_date             IN              DATE := NULL
     ,p_invis_move_line_id   IN              NUMBER DEFAULT NULL
     ,x_return_status        OUT NOCOPY      VARCHAR2)
   IS
      l_api_name   CONSTANT VARCHAR2 (30)        := 'update_move_order_lines';
      l_return_status       VARCHAR2 (1);
      l_cnt                 NUMBER;
      l_curr_qty            NUMBER;
      l_diff_qty            NUMBER;
      l_msg_count           NUMBER;
      l_msg_data            VARCHAR2 (2000);
      l_trolin_tbl          inv_move_order_pub.trolin_tbl_type;
      l_old_trolin_tbl      inv_move_order_pub.trolin_tbl_type;
      l_out_trolin_tbl      inv_move_order_pub.trolin_tbl_type;

      CURSOR cur_move_order_lines
      IS
         SELECT   l.*
             FROM mtl_txn_request_lines l, mtl_txn_request_headers h
            WHERE transaction_source_type_id =
                                             gme_common_pvt.g_txn_source_type
              AND l.txn_source_id = p_batch_id
              AND l.txn_source_line_id = p_material_detail_id
              AND l.line_status NOT IN (5, 6)
              AND h.header_id = l.header_id
              AND h.move_order_type NOT IN
                     (gme_common_pvt.g_invis_move_order_type
                     ,inv_globals.g_move_order_put_away)
         ORDER BY l.creation_date DESC;

      TYPE line_tab IS TABLE OF mtl_txn_request_lines%ROWTYPE
         INDEX BY BINARY_INTEGER;

      -- Bug 9028327 - Add cursor and local vars.
      CURSOR cur_item_mst (v_org_id NUMBER, v_inventory_item_id NUMBER)
      IS
         SELECT primary_uom_code, secondary_uom_code, tracking_quantity_ind
           FROM mtl_system_items_b
          WHERE organization_id = v_org_id
            AND inventory_item_id = v_inventory_item_id;

      l_prim_uom_code          VARCHAR2 (4);
      l_sec_uom_code           VARCHAR2 (4);
      l_inventory_item_id      NUMBER;
      l_organization_id        NUMBER;      
      l_tracking_quantity_ind  VARCHAR2(30);
      
      l_line_tbl            line_tab;
      process_mo_line_err   EXCEPTION;
     /*Bug#6446877 Cursor to fetch the allocation uom for the line */
      CURSOR get_line_uom IS
           select dtl_um, inventory_item_id, organization_id
           from gme_material_details
           where material_detail_id = p_material_detail_id;
      l_line_uom VARCHAR2(4);
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      x_return_status := fnd_api.g_ret_sts_success;
      -- Bug 9028327 - Fetch some additional columns.
      OPEN get_line_uom;
      FETCH get_line_uom INTO l_line_uom, l_inventory_item_id, l_organization_id;
      CLOSE get_line_uom;

      -- Bug 9028327 - Fetch data from item master table for use later.           
      OPEN cur_item_mst (l_organization_id
                        ,l_inventory_item_id);
      FETCH cur_item_mst  INTO l_prim_uom_code, l_sec_uom_code, l_tracking_quantity_ind ;
      CLOSE cur_item_mst;

      IF (p_invis_move_line_id IS NOT NULL) THEN
         l_trolin_tbl (1).operation := inv_globals.g_opr_update;
         l_trolin_tbl (1).line_id := p_invis_move_line_id;
         l_trolin_tbl (1).uom_code := l_line_uom;

         IF (p_new_qty IS NOT NULL) THEN
            l_trolin_tbl (1).quantity := p_new_qty;
            /*Bug#6446877 assign the fetched uom to the trolin_tbl record */
                       
            -- Bug 9028327 - Add code to update primary and secondary if necessary.           
            IF (l_prim_uom_code <> l_line_uom) THEN
               l_trolin_tbl (1).primary_quantity := inv_convert.inv_um_convert
                      (       item_id          => l_inventory_item_id,
                              lot_number       => NULL,
                              organization_id  => l_organization_id,
                              PRECISION        => NULL,
                              from_quantity    => l_trolin_tbl (1).quantity,
                              from_unit        => l_line_uom,
                              to_unit          => l_prim_uom_code,
                              from_name        => NULL,
                              to_name          => NULL
                      );
            ELSE
               l_trolin_tbl (1).primary_quantity := p_new_qty;        
            END IF;
            
            IF (l_tracking_quantity_ind = 'PS') THEN
               IF (l_sec_uom_code = l_line_uom) THEN
                  l_trolin_tbl (1).secondary_quantity := p_new_qty;        
               ELSE
                  l_trolin_tbl (1).secondary_quantity := inv_convert.inv_um_convert
                      (       item_id          => l_inventory_item_id,
                              lot_number       => NULL,
                              organization_id  => l_organization_id,
                              PRECISION        => NULL,
                              from_quantity    => l_trolin_tbl (1).primary_quantity,
                              from_unit        => l_prim_uom_code,
                              to_unit          => l_sec_uom_code,
                              from_name        => NULL,
                              to_name          => NULL
                      );
               END IF;  
            END IF;  -- Bug 9028327 
         END IF;

         IF (p_new_date IS NOT NULL) THEN
            l_trolin_tbl (1).date_required := p_new_date;
         END IF;
         
      ELSE
         OPEN cur_move_order_lines;

         FETCH cur_move_order_lines
         BULK COLLECT INTO l_line_tbl;

         CLOSE cur_move_order_lines;

         IF (p_new_qty IS NOT NULL) THEN
            FOR i IN 1 .. l_line_tbl.COUNT LOOP
               l_curr_qty := NVL (l_curr_qty, 0) + l_line_tbl (i).quantity;
            END LOOP;

            --l_diff_qty := l_curr_qty - p_new_qty;
            --Bug#5078853
	    l_diff_qty := p_new_qty - l_curr_qty; 

            IF (l_diff_qty < 0) THEN
	       /*user is trying to decrease the qty in batch*/
               FOR i IN 1 .. l_line_tbl.COUNT LOOP
                  l_cnt := l_trolin_tbl.COUNT + 1;

                  IF (l_diff_qty >= 0) THEN
                     EXIT;
                  END IF;

                  IF (l_line_tbl (l_cnt).quantity <= ABS (l_diff_qty) ) THEN
                     l_trolin_tbl (l_cnt).operation :=
                                                     inv_globals.g_opr_delete;
                     l_trolin_tbl (l_cnt).line_id := l_line_tbl (i).line_id;
                     l_trolin_tbl (l_cnt).header_id :=
                                                     l_line_tbl (i).header_id;
                     l_diff_qty := l_diff_qty + l_line_tbl (i).quantity;
                  ELSE
                     l_trolin_tbl (l_cnt).operation :=
                                                     inv_globals.g_opr_update;
                     l_trolin_tbl (l_cnt).line_id := l_line_tbl (i).line_id;
                     l_trolin_tbl (l_cnt).header_id :=
                                                     l_line_tbl (i).header_id;
		     --Bug#5078853 replaced l_trolin_tbl (i).quantity
                     l_trolin_tbl (l_cnt).quantity :=
                                       l_line_tbl (i).quantity + l_diff_qty; 
                                       
                     -- Bug 9028327/9030411 - Add code to update UOM.
                     l_trolin_tbl (l_cnt).uom_code := l_line_uom;
                
                     -- Bug 9028327/9030411 - Add code to update primary and secondary if necessary.           
                     IF (l_prim_uom_code <> l_line_uom) THEN
                        l_trolin_tbl (l_cnt).primary_quantity := inv_convert.inv_um_convert
                      (       item_id          => l_inventory_item_id,
                              lot_number       => NULL,
                              organization_id  => l_organization_id,
                              PRECISION        => NULL,
                              from_quantity    => l_trolin_tbl (l_cnt).quantity,
                              from_unit        => l_line_uom,
                              to_unit          => l_prim_uom_code,
                              from_name        => NULL,
                              to_name          => NULL
                      );
                     ELSE
                        l_trolin_tbl (l_cnt).primary_quantity := l_trolin_tbl (l_cnt).quantity;
                     END IF;

                     IF (l_tracking_quantity_ind = 'PS') THEN
                        IF (l_sec_uom_code = l_line_uom) THEN
                           l_trolin_tbl (l_cnt).secondary_quantity := l_trolin_tbl (l_cnt).quantity;
                        ELSE
                           l_trolin_tbl (l_cnt).secondary_quantity := inv_convert.inv_um_convert
                        (       item_id          => l_inventory_item_id,
                                lot_number       => NULL,
                                organization_id  => l_organization_id,
                                PRECISION        => NULL,
                                from_quantity    => l_trolin_tbl (l_cnt).primary_quantity,
                                from_unit        => l_prim_uom_code,
                                to_unit          => l_sec_uom_code,
                                from_name        => NULL,
                                to_name          => NULL
                          );
                       END IF;
                     END IF;  -- Bug 9028327/9030411

                     IF (p_new_date IS NOT NULL) THEN
                        l_trolin_tbl (l_cnt).date_required := p_new_date;
                     END IF;

                     l_line_tbl (i).REFERENCE := ' ';
                  END IF;
               END LOOP;
            ELSIF (l_diff_qty > 0) THEN
   	      /*user is trying to increase the qty in batch*/
               l_trolin_tbl (1).operation := inv_globals.g_opr_update;
               l_trolin_tbl (1).line_id := l_line_tbl (1).line_id;
               l_trolin_tbl (1).header_id := l_line_tbl (1).header_id;
	       --Bug#5078853 replaced l_trolin_tbl (i).quantity
               l_trolin_tbl (1).quantity :=
                                       l_line_tbl (1).quantity + l_diff_qty; 
                                       
               -- Bug 9028327/9030411 - Add code to update UOM.
               l_trolin_tbl (1).uom_code := l_line_uom;
                          
               -- Bug 9028327/9030411 - Add code to update UOM, primary and secondary if necessary.
               IF (l_prim_uom_code <> l_line_uom) THEN
                  l_trolin_tbl (1).primary_quantity := inv_convert.inv_um_convert
                       (       item_id          => l_inventory_item_id,
                               lot_number       => NULL,
                               organization_id  => l_organization_id,
                               PRECISION        => NULL,
                               from_quantity    => l_trolin_tbl (1).quantity,
                               from_unit        => l_line_uom,
                               to_unit          => l_prim_uom_code,
                               from_name        => NULL,
                               to_name          => NULL
                       );
               ELSE
                  l_trolin_tbl (1).primary_quantity := l_trolin_tbl (1).quantity;
               END IF;

               IF (l_tracking_quantity_ind = 'PS') THEN
                  IF (l_sec_uom_code = l_line_uom) THEN
                     l_trolin_tbl (1).secondary_quantity := l_trolin_tbl (1).quantity;
                  ELSE
                     l_trolin_tbl (1).secondary_quantity := inv_convert.inv_um_convert
                         (       item_id          => l_inventory_item_id,
                                 lot_number       => NULL,
                                 organization_id  => l_organization_id,
                                 PRECISION        => NULL,
                                 from_quantity    => l_trolin_tbl (1).primary_quantity,
                                 from_unit        => l_prim_uom_code,
                                 to_unit          => l_sec_uom_code,
                                 from_name        => NULL,
                                 to_name          => NULL
                         );
                  END IF;
               END IF;  -- Bug 9028327/9030411

               IF (p_new_date IS NOT NULL) THEN
                  l_trolin_tbl (1).date_required := p_new_date;
               END IF;

               l_line_tbl (1).REFERENCE := ' ';
            END IF;
         END IF;

         IF (p_new_date IS NOT NULL) THEN
            FOR i IN 1 .. l_line_tbl.COUNT LOOP
               IF (nvl(l_line_tbl (i).REFERENCE,'  ') <> ' ') THEN
                  l_cnt := l_trolin_tbl.COUNT + 1;
                  l_trolin_tbl (l_cnt).operation := inv_globals.g_opr_update;
                  l_trolin_tbl (l_cnt).line_id := l_line_tbl (i).line_id;
                  l_trolin_tbl (l_cnt).header_id := l_line_tbl (i).header_id;
                  l_trolin_tbl (l_cnt).date_required := p_new_date;
               END IF;
            END LOOP;
         END IF;
      END IF;

      IF (g_debug IS NOT NULL) THEN
         gme_debug.put_line
                 (   'Calling inv_move_order_pub.process_move_order_line in '
                  || l_api_name);
      END IF;

      inv_move_order_pub.process_move_order_line
                                        (p_api_version_number      => 1.0
                                        ,p_init_msg_list           => fnd_api.g_false
                                        ,p_return_values           => fnd_api.g_false
                                        ,p_commit                  => fnd_api.g_false
                                        ,x_return_status           => l_return_status
                                        ,x_msg_count               => l_msg_count
                                        ,x_msg_data                => l_msg_data
                                        ,p_trolin_tbl              => l_trolin_tbl
                                        ,p_trolin_old_tbl          => l_old_trolin_tbl
                                        ,x_trolin_tbl              => l_out_trolin_tbl);

      IF (l_return_status <> fnd_api.g_ret_sts_success) THEN
         RAISE process_mo_line_err;
      END IF;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;
   EXCEPTION
      WHEN process_mo_line_err THEN
         IF (g_debug IS NOT NULL) THEN
            gme_debug.put_line
                    (   'inv_move_order_pub.process_move_order_line returns '
                     || l_return_status);
            gme_debug.put_line ('error message is ' || l_msg_data);
         END IF;

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
   END update_move_order_lines;

   FUNCTION pending_move_orders_exist (
      p_organization_id      IN   NUMBER
     ,p_batch_id             IN   NUMBER
     ,p_material_detail_id   IN   NUMBER)
      RETURN BOOLEAN
   IS
      l_api_name   CONSTANT VARCHAR2 (30) := 'pending_move_orders_exist';
      l_exists              BOOLEAN       := FALSE;
      l_dummy               NUMBER;

-- Namit S. Bug4917629 Modified query to not select from sysdual. 
-- This is for perf reasons to reduce the sharable memory.

      CURSOR cur_move_order_lines
      IS
         SELECT 1
                     FROM mtl_txn_request_lines l, mtl_txn_request_headers h
                    WHERE l.txn_source_id = p_batch_id
                      AND l.txn_source_line_id = p_material_detail_id
                      AND l.organization_id = p_organization_id
                      AND l.line_status NOT IN (5, 6)
                      AND h.header_id = l.header_id
                      AND h.move_order_type = gme_common_pvt.g_move_order_type
                      AND ROWNUM = 1;
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      OPEN cur_move_order_lines;

      FETCH cur_move_order_lines
       INTO l_dummy;

      IF (cur_move_order_lines%FOUND) THEN
         l_exists := TRUE;
      END IF;

      CLOSE cur_move_order_lines;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;

      RETURN l_exists;
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
         RETURN FALSE;
   END pending_move_orders_exist;

   PROCEDURE get_pending_move_order_qty (
      p_mtl_dtl_rec     IN              gme_material_details%ROWTYPE
     ,x_pending_qty     OUT NOCOPY      NUMBER
     ,x_return_status   OUT NOCOPY      VARCHAR2)
   IS
      l_api_name   CONSTANT VARCHAR2 (30)     := 'get_pending_move_order_qty';
      l_return_status       VARCHAR2 (1);
      l_primary_uom         VARCHAR2 (3);
      l_secondary_uom       VARCHAR2 (3);
      l_from_uom            VARCHAR2 (3);
      l_to_uom              VARCHAR2 (3);
      l_item_no             VARCHAR2 (2000);
      l_temp_qty            NUMBER;
      l_line_tbl            gme_common_pvt.mo_lines_tab;

      CURSOR cur_item_uoms (v_org_id NUMBER, v_inventory_item_id NUMBER)
      IS
         SELECT primary_uom_code, secondary_uom_code, concatenated_segments
           FROM mtl_system_items_kfv
          WHERE organization_id = v_org_id
            AND inventory_item_id = v_inventory_item_id;

      get_mo_line_err       EXCEPTION;
      uom_conv_error        EXCEPTION;
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      x_return_status := fnd_api.g_ret_sts_success;
      gme_move_orders_pvt.get_move_order_lines
                    (p_organization_id         => p_mtl_dtl_rec.organization_id
                    ,p_batch_id                => p_mtl_dtl_rec.batch_id
                    ,p_material_detail_id      => p_mtl_dtl_rec.material_detail_id
                    ,x_mo_line_tbl             => l_line_tbl
                    ,x_return_status           => l_return_status);

      IF (l_return_status <> fnd_api.g_ret_sts_success) THEN
         RAISE get_mo_line_err;
      END IF;

      FOR i IN 1 .. l_line_tbl.COUNT LOOP
         IF (p_mtl_dtl_rec.dtl_um = l_line_tbl (i).uom_code) THEN
            -- Bug 13076579 Use detailed qty.
            -- l_temp_qty := l_line_tbl (i).quantity;
            l_temp_qty := l_line_tbl (i).quantity_detailed;
         ELSE
            OPEN cur_item_uoms (p_mtl_dtl_rec.organization_id
                               ,p_mtl_dtl_rec.inventory_item_id);

            FETCH cur_item_uoms
             INTO l_primary_uom, l_secondary_uom, l_item_no;

            CLOSE cur_item_uoms;

            IF (p_mtl_dtl_rec.dtl_um = l_secondary_uom) THEN
               l_temp_qty := l_line_tbl (i).secondary_quantity;
            ELSIF (p_mtl_dtl_rec.dtl_um = l_primary_uom) THEN
               l_temp_qty := l_line_tbl (i).primary_quantity;
            ELSE
               -- Bug 13076579 Use detailed qty.
               l_temp_qty :=
                  inv_convert.inv_um_convert
                                 (item_id            => l_line_tbl (i).inventory_item_id
                                 ,PRECISION          => gme_common_pvt.g_precision
                                 -- ,from_quantity      => l_line_tbl (i).quantity_detailed
                                 ,from_quantity      => l_line_tbl (i).quantity
                                 ,from_unit          => l_line_tbl (i).uom_code
                                 ,to_unit            => p_mtl_dtl_rec.dtl_um
                                 ,from_name          => NULL
                                 ,to_name            => NULL);

               IF (l_temp_qty < 0) THEN
                  l_from_uom := l_line_tbl (i).uom_code;
                  l_to_uom := p_mtl_dtl_rec.dtl_um;
                  RAISE uom_conv_error;
               END IF;
            END IF;
         END IF;

         x_pending_qty := NVL (x_pending_qty, 0) + l_temp_qty;
      END LOOP;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;
   EXCEPTION
      WHEN uom_conv_error THEN
         x_return_status := fnd_api.g_ret_sts_error;
         fnd_message.set_name ('GMI', 'IC_API_UOM_CONVERSION_ERROR');
         fnd_message.set_token ('ITEM_NO', l_item_no);
         fnd_message.set_token ('FROM_UOM', l_from_uom);
         fnd_message.set_token ('TO_UOM', l_to_uom);
         fnd_msg_pub.ADD;
      WHEN get_mo_line_err THEN
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
   END get_pending_move_order_qty;

   PROCEDURE delete_batch_move_orders (
      p_organization_id   IN              NUMBER
     ,p_batch_id          IN              NUMBER
     ,p_delete_invis      IN              VARCHAR2 := 'F'
     ,x_return_status     OUT NOCOPY      VARCHAR2)
   IS
      l_api_name   CONSTANT VARCHAR2 (30) := 'delete_batch_move_orders';

      CURSOR cur_batch_lines
      IS
         SELECT h.batch_id, h.move_order_header_id, d.material_detail_id
               ,d.move_order_line_id
           FROM gme_batch_header h, gme_material_details d
          WHERE h.organization_id = p_organization_id
            AND h.batch_id = p_batch_id
            AND d.batch_id = h.batch_id
            AND d.line_type = gme_common_pvt.g_line_type_ing;

      TYPE lines_tab IS TABLE OF cur_batch_lines%ROWTYPE
         INDEX BY BINARY_INTEGER;

      -- Bug 20078417 - lets make sure invisible mo line is still there.
      CURSOR cur_check_mo_line (v_inv_mo_line_id NUMBER)
      IS
         SELECT count(1)
           FROM mtl_txn_request_lines
          WHERE line_id = v_inv_mo_line_id;

      l_count               NUMBER;

      l_lines_tbl           lines_tab;
      l_invis_line_id       NUMBER;
      l_invis_header_id     NUMBER;
      l_return_status       VARCHAR2 (1);
      del_mo_lines_err      EXCEPTION;
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      x_return_status := fnd_api.g_ret_sts_success;

      OPEN cur_batch_lines;

      FETCH cur_batch_lines
      BULK COLLECT INTO l_lines_tbl;

      CLOSE cur_batch_lines;

      FOR i IN 1 .. l_lines_tbl.COUNT LOOP
         IF (p_delete_invis = fnd_api.g_true) THEN
         
            -- Bug 20635929 - Initialize the variables for each loop iteration.
            l_invis_line_id := NULL;
            l_invis_header_id := NULL;
         
            -- Bug 20078417 - lets make sure invisible mo line is still there.
            OPEN cur_check_mo_line (l_lines_tbl (i).move_order_line_id);
            FETCH cur_check_mo_line into l_count;
            CLOSE cur_check_mo_line;
            IF l_count > 0 THEN                  
               l_invis_line_id := l_lines_tbl (i).move_order_line_id;
               l_invis_header_id := l_lines_tbl (i).move_order_header_id;
            END IF;
         END IF;

         delete_move_order_lines
                   (p_organization_id           => p_organization_id
                   ,p_batch_id                  => l_lines_tbl (i).batch_id
                   ,p_material_detail_id        => l_lines_tbl (i).material_detail_id
                   ,p_invis_move_line_id        => l_invis_line_id
                   ,p_invis_move_header_id      => l_invis_header_id
                   ,x_return_status             => l_return_status);

         IF (l_return_status <> fnd_api.g_ret_sts_success) THEN
            RAISE del_mo_lines_err;
         END IF;
      END LOOP;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;
   EXCEPTION
      WHEN del_mo_lines_err THEN
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
   END delete_batch_move_orders;
END gme_move_orders_pvt;
/

COMMIT ;
EXIT;
