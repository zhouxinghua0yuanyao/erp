REM dbdrv: sql ~PROD ~PATH ~FILE none none none package &phase=plb \
REM dbdrv: checkfile(120.6.12010000.8=120.9.12020000.4)(120.6.12010000.7=120.9.12020000.3)(120.3.12000000.2=120.4):~PROD:~PATH:~FILE
SET VERIFY OFF;
WHENEVER SQLERROR EXIT FAILURE ROLLBACK;
WHENEVER OSERROR EXIT FAILURE ROLLBACK;

CREATE OR REPLACE PACKAGE BODY GME_SUPPLY_RES_PVT AS
/*  $Header: GMEORESB.pls 120.9.12020000.4 2017/01/11 20:08:19 gmurator ship $
 +=========================================================================+
 |                Copyright (c) 2000, 2017 Oracle Corporation                    |
 |                        TVP, Reading, England                            |
 |                         All rights reserved                             |
 +=========================================================================+
 | FILENAME                                                                |
 |    GMEORESB.pls                                                         |
 |                                                                         |
 | DESCRIPTION                                                             |
 |     This package contains private utilities relating to OPM Production  |
 |     as a source of supply for Reservations                              |
 |                                                                         |
 |                                                                         |
 | HISTORY                                                                 |
 |     Aug-18-2003  Liping Gao Created                                     |
 |     Archana Mundhe Bug 5763818 Modified the code to use constants       |
 |     that are added to gme_common_pvt instead of using the hardcoded     |
 |     WF event names such as 'oracle.apps.gme...'                         |
 |     srpuri FP Bug 7422975 Added the condition to convert the transaction |   
 |     quantity to the secondary only when the secondary uom code is not null |   
 |     srpuri FP Bug 8343517 Modified procedure                       |   
 |     transfer_reservation_to_inv. Assigned l_mmt_rec.transfer_lpn_id to  |   
 |     l_to_rsv_rec.lpn_id so that lpn is coorectly stamped on the transfer|   
 |     Reservation.                                                        | 
 |
 |   18-Oct-2011  G. Muratore   Bug 12805400                               |
 |      Put in check for indivisible item when creating SO reservation.    |
 |      PROCEDURE: transfer_reservation_to_inv                             |
 |                                                                         |
 |   15-Feb-2012  G. Muratore   Bug 13630492                               |
 |      Allow reservations to be transferred from fpo to batch for other   |
 |      batch demand.  PROCEDURE create_reservation_from_FPO               |
 |                                                                         |   
 |   19-NOV-2014 A. Mundhe Bug 20052174                                    |
 |      Allow reservations to be transferred from fpo to batch for         |
 |      interrnal sales orders.  PROCEDURE create_reservation_from_FPO and |
 |      transfer_reservation_to_inv                                        |
 |                                                                         |
 |   11-Jan-2017  G. Muratore   Bug 25056182                               |
 |      Compare quantities with proper conversion when required.           |
 |      PROCEDURE create_reservation_from_FPO                              |
 +=========================================================================+
  API Name  : GME_SUPPLY_RES_PVT
  Type      : Private
  Function  : This package contains Private procedures used for change management
              of reservations placed against OPM Production as a source of supply
  Pre-reqs  : N/A
  Parameters: Per function

  Current Vers  : 1.0

*/

 G_PKG_NAME  CONSTANT  VARCHAR2(30):='GME_SUPPLY_RES_PVT';
 g_debug               VARCHAR2 (5)  := fnd_profile.VALUE ('AFLOG_LEVEL');

 PROCEDURE create_reservation_from_FPO
 (
    P_FPO_batch_id           IN    NUMBER
  , P_New_batch_id           IN    NUMBER
  , X_return_status          OUT   NOCOPY VARCHAR2
  , X_msg_count              OUT   NOCOPY NUMBER
  , X_msg_data               OUT   NOCOPY VARCHAR2
 ) IS

  l_api_name                 CONSTANT  VARCHAR2 (30)    := 'create_reservation_from_FPO';
  l_fpo_rsv_rec              inv_reservation_global.mtl_reservation_rec_type;
  l_batch_rsv_rec            inv_reservation_global.mtl_reservation_rec_type;
  l_fpo_rsv_tbl              inv_reservation_global.mtl_reservation_tbl_type;
  l_dummy_sn                 inv_reservation_global.serial_number_tbl_type;
  l_fpo_rsv_count            NUMBER;

  l_batch_line_id            NUMBER;
  l_fpo_batch_line_id        NUMBER;
  l_whse_code                VARCHAR2(5);
  l_planned_qty              NUMBER;
  l_fpo_qty                  NUMBER;
  l_planned_qty2             NUMBER;
  l_res_count                NUMBER;
  l_avg_qty                  NUMBER;
  l_avg_qty2                 NUMBER;

  l_inventory_item_id        NUMBER;
  l_reserved_qty             NUMBER;
  l_reserved_qty2            NUMBER;
  l_remaining_qty            NUMBER;
  l_organization_id          NUMBER;
  l_plan_cmplt_date          date;
  l_new_reservation_id       NUMBER;

  -- Bug 25056182
  l_dtl_um                  gme_material_details.dtl_um%TYPE;  

  l_msg_data                 VARCHAR2(2000);
  l_msg_count                NUMBER;
  l_api_return_status        VARCHAR2(1);
  l_api_error_code           NUMBER;
  l_api_error_msg            VARCHAR2(100);

  -- Bug 13630492 - Transfer reservations for batches also. 
  -- Bug 20052174 - Transfers reservations for ISO's. 
  Cursor check_whse (p_batch_line_id IN NUMBER) IS
  Select distinct organization_id
    From mtl_reservations
   Where supply_source_line_id = p_batch_line_id
     and supply_source_type_id = 5
     and demand_source_type_id IN (inv_reservation_global.g_source_type_oe, INV_RESERVATION_GLOBAL.g_source_type_internal_ord, gme_common_pvt.g_txn_source_type)  -- Bug 13630492
     and reservation_quantity <> 0;

  Cursor get_batch_line (p_batch_id IN NUMBER) IS
  Select material_detail_id, inventory_item_id, plan_qty
    From gme_material_details
   where batch_id = p_batch_id
     and line_type <> -1;            -- not ingredient

  Cursor get_res_for_whse (p_organization_id IN NUMBER,
                           p_batch_line_id IN NUMBER) IS
  Select *
    From mtl_reservations
   Where supply_source_line_id = p_batch_line_id
     and supply_source_type_id = 5
     and demand_source_type_id IN (inv_reservation_global.g_source_type_oe, 
                                   INV_RESERVATION_GLOBAL.g_source_type_internal_ord) --  Bug 20052174 Also consider demand source type id for ISO's.
     and organization_id = p_organization_id
     and reservation_quantity <> 0
  Order by requirement_date;

  Cursor get_res_count(p_batch_line_id IN NUMBER) Is
  Select count(1)
    From mtl_reservations
   Where supply_source_line_id = p_batch_line_id
     --and organization_id = p_organization_id
     and supply_source_type_id = 5
     and reservation_quantity <> 0;

  -- Bug 25056182 - pick dtl_um also.
  Cursor get_new_batch_line (p_batch_id  IN NUMBER
                       ,     p_item_id   IN NUMBER) is
  Select material_detail_id, plan_qty, dtl_um
    From gme_material_details
   Where batch_id = p_batch_id
     and inventory_item_id = p_item_id
     and line_type <> -1;
     
  Cursor get_new_batch_cmpt_date (p_batch_id IN NUMBER) is
  Select plan_cmplt_date
    From gme_batch_header
   where batch_id = p_batch_id;

 BEGIN
  x_return_status := FND_API.G_RET_STS_SUCCESS;
  IF g_debug <= gme_debug.g_log_procedure THEN
     gme_debug.put_line ('Entering api '
                        || g_pkg_name || '.'
                        || l_api_name);
     gme_debug.put_line(g_pkg_name||'.'||l_api_name||' FPO batch_id '||p_fpo_batch_id);
     gme_debug.put_line(g_pkg_name||'.'||l_api_name||' BATCH batch_id '||p_new_batch_id);
  END IF;

  /* loop through all the product lines in the batch */
  For batch_line in get_batch_line(p_FPO_batch_id) Loop
     l_fpo_batch_line_id := batch_line.material_detail_id ;
     l_inventory_item_id := batch_line.inventory_item_id ;
     l_fpo_qty           := batch_line.plan_qty;
     IF g_debug <= gme_debug.g_log_procedure THEN
        gme_debug.put_line(g_pkg_name||'.'||l_api_name|| ' FPO batch_line_id '||l_fpo_batch_line_id);
        gme_debug.put_line(g_pkg_name||'.'||l_api_name|| ' FPO inventory_item_id '||l_inventory_item_id);
        gme_debug.put_line(g_pkg_name||'.'||l_api_name|| ' not used in logic FPO fpo_qty '||l_fpo_qty);
     END IF;
     
     /* check reservation exist or not */
     Open get_res_count(l_FPO_batch_line_id);
     Fetch get_res_count into l_res_count;
     Close get_res_count;

     IF g_debug <= gme_debug.g_log_procedure THEN
        gme_debug.put_line(g_pkg_name||'.'||l_api_name ||' FPO reservation count '||l_res_count);
        -- gme_debug.put_line(g_pkg_name||'.'||l_api_name || ' NEW batch_line_id '||l_batch_line_id);
     END IF;

     IF l_res_count = 0 THEN
        goto next_batch_line;
     END IF;

     /* get the new batch_line_id for the created batch */
     Open get_new_batch_line(p_new_batch_id, l_inventory_item_id);
     Fetch get_new_batch_line Into l_batch_line_id, l_planned_qty, l_dtl_um;  -- Bug 25056182
     Close get_new_batch_line;

     IF g_debug <= gme_debug.g_log_procedure THEN
        gme_debug.put_line(g_pkg_name||'.'||l_api_name ||' NEW batch_line_id '||l_batch_line_id);
        gme_debug.put_line(g_pkg_name||'.'||l_api_name ||' l_planned_qty '||l_planned_qty);
        gme_debug.put_line(g_pkg_name||'.'||l_api_name ||' l_dtl_um '||l_dtl_um);
     END IF;

     /* loop to see if different whse may have exist -- only one org in R12*/
     For each_whse in check_whse(l_fpo_batch_line_id) Loop
        l_organization_id := each_whse.organization_id;

        IF g_debug <= gme_debug.g_log_procedure THEN
           gme_debug.put_line(g_pkg_name||'.'||l_api_name ||' reservation org id '||l_organization_id);
           gme_debug.put_line(g_pkg_name||'.'||l_api_name ||' batch inventory_item_id '||l_inventory_item_id);
        END IF;

        Open get_new_batch_cmpt_date (p_new_batch_id) ;
        Fetch get_new_batch_cmpt_date Into l_plan_cmplt_date;
        Close get_new_batch_cmpt_date;
        
        /* process the reservations made in this whse */
        l_fpo_rsv_rec.organization_id         := l_organization_id;
        l_fpo_rsv_rec.inventory_item_id       := l_inventory_item_id;
        l_fpo_rsv_rec.supply_source_type_id   := 5 ;--inv_reservation_global.g_source_type_wip;
        l_fpo_rsv_rec.supply_source_header_id := p_fpo_batch_id;
        l_fpo_rsv_rec.supply_source_line_id   := l_fpo_batch_line_id;
        
        inv_reservation_pub.query_reservation
              (
                p_api_version_number         => 1.0
              , p_init_msg_lst               => fnd_api.g_false
              , x_return_status              => x_return_status
              , x_msg_count                  => x_msg_count
              , x_msg_data                   => x_msg_data
              , p_query_input                => l_fpo_rsv_rec
              , p_sort_by_req_date           => inv_reservation_global.g_query_req_date_asc
              , x_mtl_reservation_tbl        => l_fpo_rsv_tbl
              , x_mtl_reservation_tbl_count  => l_fpo_rsv_count
              , x_error_code                 => l_api_error_code
              );
              
        IF x_return_status = fnd_api.g_ret_sts_error THEN
           RAISE fnd_api.g_exc_error;
        ELSIF x_return_status = fnd_api.g_ret_sts_unexp_error THEN
           RAISE fnd_api.g_exc_unexpected_error;
        END IF;

        IF g_debug <= gme_debug.g_log_procedure THEN
           gme_debug.put_line(g_pkg_name||'.'||l_api_name ||' fpo reservation count  '||l_fpo_rsv_count);
        END IF;

        l_remaining_qty := l_planned_qty;      
          
        -- Bug 25056182 -- Let's convert l_remaining_qty to the reservation uom IF NECESSARY.
        IF l_fpo_rsv_rec.reservation_uom_code <> l_dtl_um THEN
           l_remaining_qty := inv_convert.inv_um_convert(
                  item_id       =>  l_inventory_item_id
                , precision     =>  NULL
                , from_quantity =>  l_remaining_qty
                , from_unit     =>  l_dtl_um
                , to_unit       =>  l_fpo_rsv_tbl(1).reservation_uom_code
                , from_name     =>  NULL
                , to_name       =>  NULL);        
        END IF;

        -- Might need error checking for conversion someday.
        
        IF g_debug <= gme_debug.g_log_procedure THEN
           gme_debug.put_line(g_pkg_name||'.'||l_api_name ||' l_planned_qty after conversion '||l_planned_qty);
           gme_debug.put_line(g_pkg_name||'.'||l_api_name ||' l_remaining_qty after conversion '||l_remaining_qty);
           gme_debug.put_line(g_pkg_name||'.'||l_api_name ||' l_fpo_rsv_tbl(1).reservation_uom_code '||l_fpo_rsv_tbl(1).reservation_uom_code);
           gme_debug.put_line(g_pkg_name||'.'||l_api_name ||' l_dtl_um '||l_dtl_um);
        END IF;

        for i in 1..l_fpo_rsv_count Loop
           l_fpo_rsv_rec := l_fpo_rsv_tbl(i);
           /* check the batch planned cplt date with the scheduled_ship_date
            * if the date is out, skip this record
            */
            
           IF g_debug <= gme_debug.g_log_procedure THEN
              gme_debug.put_line(g_pkg_name||'.'||l_api_name ||' loop iteration is '||i);
              gme_debug.put_line(g_pkg_name||'.'||l_api_name ||' l_remaining_qty in loop is '||l_remaining_qty);
           END IF;
           
           /*IF l_fpo_rsv_rec.requirement_date> l_plan_cmplt_date THEN
              Goto next_res_line;
           END IF;
           */
           
           EXIT WHEN l_remaining_qty <= 0 ;

           IF g_debug <= gme_debug.g_log_procedure THEN
              gme_debug.put_line(g_pkg_name||'.'||l_api_name ||' build res record for the new batch');
              gme_debug.put_line(' l_remaining_qty is '||l_remaining_qty);
              gme_debug.put_line(' l_fpo_rsv_rec.reservation_id is '||l_fpo_rsv_rec.reservation_id);
              gme_debug.put_line(' l_fpo_rsv_rec.reservation_quantity is '||l_fpo_rsv_rec.reservation_quantity);
              gme_debug.put_line(' l_fpo_rsv_rec.reservation_uom_code is '||l_fpo_rsv_rec.reservation_uom_code);
              gme_debug.put_line(' l_fpo_rsv_rec.secondary_reservation_quantity is '||l_fpo_rsv_rec.secondary_reservation_quantity);
           END IF;
           
           /* build reservation rec for the new batch line */
           
           -- Bug 25056182 -- Let's compare to l_remaining_qty.
           -- IF l_fpo_rsv_rec.reservation_quantity <= l_planned_qty THEN
           
           IF l_fpo_rsv_rec.reservation_quantity <= l_remaining_qty THEN
              l_reserved_qty := l_fpo_rsv_rec.reservation_quantity;
              l_reserved_qty2 := l_fpo_rsv_rec.secondary_reservation_quantity;
           ELSE     -- R12 restricting over reserve
              -- l_reserved_qty := l_planned_qty;
              l_reserved_qty := l_remaining_qty;
              IF l_fpo_rsv_rec.secondary_uom_code is not null
                    and l_fpo_rsv_rec.reservation_uom_code <> l_fpo_rsv_rec.secondary_uom_code
              THEN
                 l_reserved_qty2 := inv_convert.inv_um_convert(
                        item_id       =>  l_inventory_item_id
                      , precision     =>  NULL
                      , from_quantity =>  l_reserved_qty
                      , from_unit     =>  l_fpo_rsv_rec.reservation_uom_code
                      , to_unit       =>  l_fpo_rsv_rec.secondary_uom_code
                      , from_name     =>  NULL
                      , to_name       =>  NULL);
              ELSIF l_fpo_rsv_rec.reservation_uom_code = l_fpo_rsv_rec.secondary_uom_code
              THEN
                 l_reserved_qty2 := l_reserved_qty;
              END IF;
           END IF;

           l_batch_rsv_rec := l_fpo_rsv_rec;

           l_batch_rsv_rec.reservation_id                  := NULL;             -- New reservation
           l_batch_rsv_rec.lpn_id                          := NULL;
           l_batch_rsv_rec.subinventory_id                 := NULL;
           l_batch_rsv_rec.lot_number_id                   := NULL;
           l_batch_rsv_rec.supply_source_header_id         := p_new_batch_id;
           l_batch_rsv_rec.supply_source_line_id           := l_batch_line_id;
           l_batch_rsv_rec.reservation_quantity            := l_reserved_qty;
           l_batch_rsv_rec.primary_reservation_quantity    := l_reserved_qty;
           l_batch_rsv_rec.secondary_reservation_quantity  := l_reserved_qty2;
           
           IF l_fpo_rsv_rec.reservation_uom_code <> l_fpo_rsv_rec.primary_uom_code
           THEN
                  l_batch_rsv_rec.primary_reservation_quantity := inv_convert.inv_um_convert(
                        item_id       =>  l_inventory_item_id
                      , precision     =>  NULL
                      , from_quantity =>  l_reserved_qty
                      , from_unit     =>  l_fpo_rsv_rec.reservation_uom_code
                      , to_unit       =>  l_fpo_rsv_rec.primary_uom_code
                      , from_name     =>  NULL
                      , to_name       =>  NULL);
           END IF;

           IF g_debug <= gme_debug.g_log_procedure THEN
              gme_debug.put_line(g_pkg_name||'.'||l_api_name || ' new reserved qty '||l_reserved_qty);
              gme_debug.put_line(g_pkg_name||'.'||l_api_name || ' new reserved qty2 '||l_reserved_qty2);
              gme_debug.put_line(g_pkg_name||'.'||l_api_name || ' calling inv_reservation_pub.transfer_reservation');
           END IF;

           inv_reservation_pub.transfer_reservation
                (p_api_version_number         => 1.0,
                 p_init_msg_lst               => fnd_api.g_false,
                 x_return_status              => x_return_status,
                 x_msg_count                  => x_msg_count,
                 x_msg_data                   => x_msg_data,
                 p_original_rsv_rec           => l_fpo_rsv_tbl(i),
                 p_to_rsv_rec                 => l_batch_rsv_rec,
                 p_original_serial_number     => l_dummy_sn ,
                 p_to_serial_number           => l_dummy_sn ,
                 p_validation_flag            => fnd_api.g_false,
                 x_to_reservation_id          => l_new_reservation_id
                 );

           IF g_debug <= gme_debug.g_log_procedure THEN
              gme_debug.put_line(g_pkg_name||'.'||l_api_name || ' return from inv_reservation_pub.transfer_reservation '||x_return_status);
           END IF;
                 
           IF x_return_status = fnd_api.g_ret_sts_error THEN
              RAISE fnd_api.g_exc_error;
           ELSIF x_return_status = fnd_api.g_ret_sts_unexp_error THEN
              RAISE fnd_api.g_exc_unexpected_error;
           END IF;
           l_remaining_qty := l_remaining_qty - l_reserved_qty;
           <<next_res_line>>
           null;
        END Loop;
     END LOOP;
     <<next_batch_line>>
     null;
  END loop;

  IF g_debug <= gme_debug.g_log_procedure THEN
     gme_debug.put_line(g_pkg_name||'.'||l_api_name || ' calling GME_SUPPLY_RES_PVT.notify_CSR');
  END IF;
  
  /* notify the CSR*/
  GME_SUPPLY_RES_PVT.notify_CSR
    ( P_Batch_id               =>    p_new_batch_id
    , P_fpo_id                 =>    p_fpo_batch_id
    , P_organization_id        =>    l_organization_id
    , P_action_code            =>    'CONVERT_FPO'
    , X_return_status          =>    x_return_status
    , X_msg_cont               =>    x_msg_count
    , X_msg_data               =>    x_msg_data );

 EXCEPTION
  WHEN FND_API.G_EXC_ERROR THEN
    x_return_status := FND_API.G_RET_STS_ERROR;
    /*   Get message count and data*/
    FND_MSG_PUB.count_and_get
     (   p_count  => x_msg_count
       , p_data  => x_msg_data
     );
    IF g_debug <= gme_debug.g_log_procedure THEN
       gme_debug.put_line(g_pkg_name||'.'||l_api_name || ' u EXCEPTION: Expected');
    END IF;
    WHEN OTHERS THEN
      x_return_status := FND_API.G_RET_STS_UNEXP_ERROR;

      FND_MSG_PUB.Add_Exc_Msg (  G_PKG_NAME
                               , 'convert_FPO'
                              );
      /*   Get message count and data*/
      FND_MSG_PUB.count_and_get
       (   p_count  => x_msg_count
         , p_data  => x_msg_data
       );
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line(g_pkg_name||'.'||l_api_name || 'u EXCEPTION: Others');
      END IF;

 END create_reservation_from_FPO;

 PROCEDURE notify_CSR
 (
    P_Batch_id               IN    NUMBER default null
  , P_FPO_id                 IN    NUMBER default null
  , P_Batch_line_id          IN    NUMBER default null
  , P_So_line_id             IN    NUMBER default null
  , P_batch_trans_id         IN    NUMBER default null
  , P_organization_id        IN    NUMBER default null
  , P_action_code            IN    VARCHAR2
  , X_return_status          OUT   NOCOPY VARCHAR2
  , X_msg_cont               OUT   NOCOPY NUMBER
  , X_msg_data               OUT   NOCOPY VARCHAR2
 ) IS
  l_api_name              CONSTANT  VARCHAR2 (30)    := 'Notify_CSR';
  l_csr_id                NUMBER;

  l_batch_id			NUMBER;
  l_so_header_id		NUMBER;
  l_so_line_id			NUMBER;
  l_action_code			VARCHAR2(200);
  l_whse_code			VARCHAR2(4);
  l_batch_type			NUMBER;
  l_no_of_staged_alloc		NUMBER;
  l_no_of_unstaged_alloc	NUMBER;
  l_last_updated_by		NUMBER;
  l_created_by			NUMBER;
  l_session_id			NUMBER;
  l_batch_line_id		NUMBER;
  l_organization_id    NUMBER;
  l_old_header_id		NUMBER;
  l_new_header_id		NUMBER;
  l_lot_number   		VARCHAR2(80);

  l_parameter_list     wf_parameter_list_t :=wf_parameter_list_t( );


  CURSOR So_line_id_for_batch(p_batch_id IN NUMBER) Is
  SELECT Distinct orig_demand_source_line_id
    FROM mtl_reservations
   WHERE supply_source_header_id = p_batch_id
     and supply_source_type_id = 5
     and demand_source_type_id = 2
     and reservation_quantity <> 0;

  CURSOR Get_batch_type(p_batch_id IN NUMBER) IS
   SELECT batch_type
     FROM gme_batch_header
    WHERE batch_id = p_batch_id;


  CURSOR So_line_id_for_batch_line(p_batch_line_id IN NUMBER) Is
  SELECT Distinct orig_demand_source_line_id
    FROM mtl_reservations
   WHERE supply_source_line_id = p_batch_line_id
     and supply_source_type_id = 5
     and demand_source_type_id = 2
     and reservation_quantity <> 0;

  CURSOR get_batch_id_for_line(p_batch_line_id IN NUMBER) Is
  SELECT gl.batch_id,gh.batch_type
    FROM gme_material_details gl,
         gme_batch_header  gh
   WHERE gl.material_detail_id = p_batch_line_id
     and gl.batch_id = gh.batch_id;

   CURSOR CSR_for_so_line(p_so_line_id IN NUMBER) IS
   SELECT last_updated_by, created_by,header_id
     FROM oe_order_lines_all
    WHERE line_id = p_so_line_id;

-- nsinghi perf bug#5212566. Re-written the 2 cursors to ensure that there is no FTS on mtl_reservations.
/*
  Cursor check_mul_line_id1 (p_user_id IN number
                         , p_batch_id IN NUMBER) IS
  Select distinct orig_demand_source_line_id
  From mtl_reservations
  Where created_by = p_user_id
    and orig_supply_source_header_id = p_batch_id
    and demand_source_type_id = 2
    and orig_supply_source_type_id = 5;

  Cursor check_mul_line_id2 (p_user_id IN number
                         , p_batch_id IN NUMBER
                         , p_batch_line_id IN NUMBER) IS
  Select distinct orig_demand_source_line_id
  From mtl_reservations
  Where created_by = p_user_id
    and orig_supply_source_header_id = p_batch_id
    and orig_supply_source_line_id = p_batch_line_id
    and demand_source_type_id = 2
    and orig_supply_source_type_id = 5;
*/
  Cursor check_mul_line_id1 (p_user_id IN number
                         , p_batch_id IN NUMBER) IS
  SELECT DISTINCT orig_demand_source_line_id
  FROM mtl_reservations mr, gme_batch_header gbh
  WHERE mr.created_by = p_user_id
    AND mr.orig_supply_source_header_id = p_batch_id
    AND mr.orig_supply_source_header_id = gbh.batch_id
    AND mr.organization_id = gbh.organization_id
    AND mr.demand_source_type_id = 2
    AND mr.orig_supply_source_type_id = 5;

  Cursor check_mul_line_id2 (p_user_id IN number
                         , p_batch_id IN NUMBER
                         , p_batch_line_id IN NUMBER) IS
  SELECT DISTINCT orig_demand_source_line_id
  FROM mtl_reservations mr, gme_material_details gmd
  WHERE mr.created_by = p_user_id
    AND mr.orig_supply_source_header_id = p_batch_id
    AND mr.orig_supply_source_line_id = p_batch_line_id
    AND mr.inventory_item_id = gmd.inventory_item_id
    AND mr.organization_id = gmd.organization_id
    AND gmd.batch_id = mr.orig_supply_source_header_id
    AND gmd.material_detail_id = mr.orig_supply_source_line_id
    AND mr.demand_source_type_id = 2
    AND mr.orig_supply_source_type_id = 5;

  -- MakeToOrder BEGIN
  CURSOR Get_pending_lot(p_batch_id IN NUMBER, p_batch_line_id IN NUMBER) IS
   SELECT lot_number
     FROM gme_pending_product_lots
    WHERE batch_id = p_batch_id and material_detail_id = p_batch_line_id;
  -- MakeToOrder BEGIN

 BEGIN

  x_return_status := FND_API.G_RET_STS_SUCCESS;

  IF g_debug <= gme_debug.g_log_procedure THEN
     gme_debug.put_line ('Entering api '
                        || g_pkg_name || '.'
                        || l_api_name);
  END IF;
  l_session_id := USERENV('sessionid');
  l_batch_line_id := p_batch_line_id ;
  l_batch_id := p_batch_id;
  l_organization_id := p_organization_id;

  /* will send the work flow */
  IF p_batch_id is not null THEN
     IF g_debug <= gme_debug.g_log_procedure THEN
        gme_debug.put_line(g_pkg_name||'.'||l_api_name || 'Notify CSR : Batch_id is'|| p_batch_id);
     END IF;
     OPEN so_line_id_for_batch(p_batch_id);
     FETCH so_line_id_for_batch INTO l_so_line_id;
     IF(so_line_id_for_batch%NOTFOUND) THEN
       CLOSE so_line_id_for_batch;
        IF g_debug <= gme_debug.g_log_procedure THEN
        gme_debug.put_line(g_pkg_name||'.'||l_api_name || ' so_line_id_for_batch%NOTFOUND, returning from Notify CSR');
        END IF;
       RETURN;
     END IF;

     CLOSE so_line_id_for_batch;

     OPEN get_batch_type(p_batch_id);
     FETCH get_batch_type INTO l_batch_type;
     IF(get_batch_type%NOTFOUND) THEN
       CLOSE get_batch_type;
        IF g_debug <= gme_debug.g_log_procedure THEN
        gme_debug.put_line(g_pkg_name||'.'||l_api_name || ' get_batch_type%NOTFOUND, returning from Notify CSR');
        END IF;
       RETURN;
     END IF;

     CLOSE get_batch_type;

  END IF;

  IF p_batch_line_id is not null THEN
     IF g_debug <= gme_debug.g_log_procedure THEN
        gme_debug.put_line(g_pkg_name||'.'||l_api_name || 'Notify CSR : Batch_line_id is '|| p_batch_line_id);
     END IF;
     OPEN so_line_id_for_batch_line(p_batch_line_id);
     FETCH so_line_id_for_batch_line INTO l_so_line_id;
     IF(so_line_id_for_batch_line%NOTFOUND) THEN
       CLOSE so_line_id_for_batch_line;
        IF g_debug <= gme_debug.g_log_procedure THEN
           gme_debug.put_line(g_pkg_name||'.'||l_api_name || ' so_line_id_for_batch_line%NOTFOUND, returning from Notify CSR');
        END IF;
       RETURN;
     END IF;
     CLOSE so_line_id_for_batch_line;

     OPEN get_batch_id_for_line(p_batch_line_id);
     FETCH get_batch_id_for_line INTO l_batch_id,l_batch_type;
     IF(get_batch_id_for_line%NOTFOUND) THEN
       CLOSE get_batch_id_for_line;
        IF g_debug <= gme_debug.g_log_procedure THEN
           gme_debug.put_line(g_pkg_name||'.'||l_api_name || ' get_batch_id_for_line%NOTFOUND, returning from Notify CSR');
        END IF;
       RETURN;
     END IF;
     CLOSE get_batch_id_for_line;

  END IF;

  IF g_debug <= gme_debug.g_log_procedure THEN
    gme_debug.put_line(g_pkg_name||'.'||l_api_name || 'Notify CSR : so_line_id is'|| l_so_line_id);
    gme_debug.put_line(g_pkg_name||'.'||l_api_name || 'Notify CSR : p_organization_id is'|| l_organization_id);
    gme_debug.put_line(g_pkg_name||'.'||l_api_name || 'Notify CSR : p_action_code is'|| p_action_code);
  END IF;

  l_action_code := p_action_code;

  IF(p_action_code  = 'CANCEL') THEN

    IF(l_batch_type = 10) THEN
       l_action_code := 'CANCEL_FPO';
    ELSE
       l_action_code := 'CANCEL_BATCH';
    END IF;
  END IF;

  IF(p_action_code = 'CMPLT_DATE_CHANGE') THEN
     l_action_code := 'PLAN_COMPL_DATE_CHANGED';
  END IF;

  IF g_debug <= gme_debug.g_log_procedure THEN
     gme_debug.put_line(g_pkg_name||'.'||l_api_name || 'Notify CSR : l_action_code is'|| l_action_code);
  END IF;

  OPEN CSR_for_so_line(l_so_line_id);
  FETCH CSR_for_so_line INTO  l_last_updated_by,l_created_by, l_so_header_id;
  IF(CSR_for_so_line%NOTFOUND) THEN
     CLOSE CSR_for_so_line;
     IF g_debug <= gme_debug.g_log_procedure THEN
        gme_debug.put_line(g_pkg_name||'.'||l_api_name ||' CSR_for_so_line%NOTFOUND, returning from Notify CSR');
     END IF;
     RETURN;
  END IF;
  CLOSE CSR_for_so_line;
  IF g_debug <= gme_debug.g_log_procedure THEN
     gme_debug.put_line(g_pkg_name||'.'||l_api_name || 'Notify CSR : l_last_updated_by is '||l_last_updated_by);
     gme_debug.put_line(g_pkg_name||'.'||l_api_name || 'Initiating the Business Event......');
  END IF;

  --  MakeToOrder BEGIN
  IF p_action_code = 'NEW_BATCH_CREATED' THEN
     IF g_debug <= gme_debug.g_log_procedure THEN
        gme_debug.put_line(g_pkg_name||'.'||l_api_name || 'Retrieve any pending lot for line '||p_batch_line_id);
     END IF;
     OPEN Get_pending_lot(p_batch_id ,p_batch_line_id );
     FETCH Get_pending_lot INTO l_lot_number;
     IF(Get_pending_lot%NOTFOUND) THEN
       IF g_debug <= gme_debug.g_log_procedure THEN
          gme_debug.put_line(g_pkg_name||'.'||l_api_name || ' no pending lot exists for line '|| p_batch_line_id);
       END IF;
     END IF;

     CLOSE Get_pending_lot;
  END IF;
  -- MakeToOrder END

  /*GME_BATCH_WORKFLOW_PKG.Init_wf( p_session_id  => l_session_id
         , p_approver    => l_last_updated_by
         , p_so_header_id=> l_so_header_id
         , p_so_line_id  => l_so_line_id
         , p_batch_id    => l_batch_id
         , p_batch_line_id => NULL
         , p_fpo_id => p_fpo_id
         , p_organization_id   => l_organization_id
         , p_action_code   => l_action_code );
   */
  wf_log_pkg.wf_debug_flag:=TRUE;
  wf_event.AddParameterToList('SESSION_ID', l_session_id,l_parameter_list);
  wf_event.AddParameterToList('APPROVER',l_last_updated_by ,l_parameter_list);
  wf_event.AddParameterToList('SO_HEADER_ID',l_so_header_id ,l_parameter_list);
  wf_event.AddParameterToList('SO_LINE_ID',l_so_line_id ,l_parameter_list);
  wf_event.AddParameterToList('BATCH_ID',l_batch_id ,l_parameter_list);
  wf_event.AddParameterToList('BATCH_LINE_ID',p_batch_line_id ,l_parameter_list);
  wf_event.AddParameterToList('FPO_ID',p_fpo_id ,l_parameter_list);
  wf_event.AddParameterToList('ORGANIZATION_ID',l_organization_id ,l_parameter_list);
  wf_event.AddParameterToList('ACTION_CODE',l_action_code ,l_parameter_list);
  wf_event.AddParameterToList('LOT_NO',l_lot_number,l_parameter_list);
  IF g_debug <= gme_debug.g_log_procedure THEN
        gme_debug.put_line(g_pkg_name||'.'||l_api_name || 'Notify CSR : raising business event ');
        gme_debug.put_line(g_pkg_name||'.'||l_api_name || 'Notify CSR : session_id '|| l_session_id );
  End if;
  wf_event.raise(p_event_name => gme_common_pvt.G_BATCH_RESERVATIONS,
                 p_event_key  => l_session_id,
                 p_parameters => l_parameter_list);

  IF(l_last_updated_by <> l_created_by) THEN

     IF g_debug <= gme_debug.g_log_procedure THEN
        gme_debug.put_line(g_pkg_name||'.'||l_api_name || 'Notify CSR : l_created_by is '||l_created_by);
        gme_debug.put_line(g_pkg_name||'.'||l_api_name || 'Initiating the Workflow......');
     END IF;

     /*GME_BATCH_WORKFLOW_PKG.Init_wf( p_session_id  => l_session_id
            , p_approver    => l_created_by
            , p_so_header_id=> l_so_header_id
            , p_so_line_id  => l_so_line_id
            , p_batch_id    => l_batch_id
            , p_batch_line_id => NULL
            , p_fpo_id => NULL
            , p_organization_id   => l_organization_id
            , p_action_code   => l_action_code );
      */
     wf_event.AddParameterToList('APPROVER',l_created_by ,l_parameter_list);
     wf_event.raise(p_event_name => gme_common_pvt.G_BATCH_RESERVATIONS,
                 p_event_key  => l_session_id,
                 p_parameters => l_parameter_list);
  END IF;
  /* check to see when FPO converts to batches, the CSR for each new batch will get a notification*/
  /* check to see if the same user has multiple sales lines for the reservations */
  /* for each sales order or header_id, one notification is sent */
  IF nvl(l_batch_line_id,0) = 0 THEN
     for mul_line in check_mul_line_id1 (l_last_updated_by, l_batch_id ) Loop
        l_old_header_id := l_so_header_id ;
        l_so_line_id    := mul_line.orig_demand_source_line_id ;
        /* Get the Order and Line Information */
        OPEN CSR_for_so_line(l_so_line_id);
        FETCH CSR_for_so_line INTO  l_last_updated_by,l_created_by, l_new_header_id ;
        CLOSE CSR_for_so_line;
        IF l_new_header_id <> l_old_header_id THEN
           l_so_header_id := l_new_header_id;
           l_old_header_id := l_new_header_id;

           IF g_debug <= gme_debug.g_log_procedure THEN
              gme_debug.put_line(g_pkg_name||'.'||l_api_name || 'Notify CSR : Multiple sales orders, header_id'||l_so_header_id);
              gme_debug.put_line(g_pkg_name||'.'||l_api_name || 'Initiating the Workflow......');
           END IF;
           /*GME_BATCH_WORKFLOW_PKG.Init_wf( p_session_id  => l_session_id
                  , p_approver    => l_last_updated_by
                  , p_so_header_id=> l_so_header_id
                  , p_so_line_id  => l_so_line_id
                  , p_batch_id    => l_batch_id
                  , p_batch_line_id => NULL
                  , p_fpo_id => NULL
                  , p_organization_id   => l_organization_id
                  , p_action_code   => l_action_code );
                  */
           wf_event.AddParameterToList('APPROVER',l_last_updated_by ,l_parameter_list);
           wf_event.AddParameterToList('SO_LINE_ID',l_so_line_id ,l_parameter_list);
           wf_event.AddParameterToList('SO_HEADER_ID',l_so_header_id ,l_parameter_list);
           wf_event.raise(p_event_name => gme_common_pvt.G_BATCH_RESERVATIONS,
                 p_event_key  => l_session_id,
                 p_parameters => l_parameter_list);

           IF(l_last_updated_by <> l_created_by) THEN

              IF g_debug <= gme_debug.g_log_procedure THEN
                 gme_debug.put_line(g_pkg_name||'.'||l_api_name || 'Notify CSR : l_created_by is '||l_created_by);
                 gme_debug.put_line(g_pkg_name||'.'||l_api_name || 'Initiating the Workflow......');
              END IF;

              /*GME_BATCH_WORKFLOW_PKG.Init_wf( p_session_id  => l_session_id
                     , p_approver    => l_created_by
                     , p_so_header_id=> l_so_header_id
                     , p_so_line_id  => l_so_line_id
                     , p_batch_id    => l_batch_id
                     , p_batch_line_id => NULL
                     , p_fpo_id => NULL
                     , p_organization_id   => l_organization_id
                     , p_action_code   => l_action_code );
                     */
              wf_event.AddParameterToList('APPROVER',l_created_by ,l_parameter_list);
              wf_event.raise(p_event_name => gme_common_pvt.G_BATCH_RESERVATIONS,
                 p_event_key  => l_session_id,
                 p_parameters => l_parameter_list);
           END IF;
        END IF;
     END LOOP;
  Else
     for mul_line in check_mul_line_id2 (l_last_updated_by, l_batch_id, l_batch_line_id ) Loop
        l_old_header_id := l_so_header_id ;
        l_so_line_id    := mul_line.orig_demand_source_line_id ;
        OPEN CSR_for_so_line(l_so_line_id);
        FETCH CSR_for_so_line INTO  l_last_updated_by,l_created_by, l_new_header_id ;
        CLOSE CSR_for_so_line;
        IF l_new_header_id <> l_old_header_id THEN
           l_so_header_id := l_new_header_id;
           l_old_header_id := l_new_header_id;

           IF g_debug <= gme_debug.g_log_procedure THEN
              gme_debug.put_line(g_pkg_name||'.'||l_api_name || 'Notify CSR : Multiple sales orders, header_id'||l_so_header_id);
              gme_debug.put_line(g_pkg_name||'.'||l_api_name || 'Initiating the Workflow......');
           END IF;
           /*GME_BATCH_WORKFLOW_PKG.Init_wf( p_session_id  => l_session_id
                  , p_approver    => l_last_updated_by
                  , p_so_header_id=> l_so_header_id
                  , p_so_line_id  => l_so_line_id
                  , p_batch_id    => l_batch_id
                  , p_batch_line_id => NULL
                  , p_fpo_id => NULL
                  , p_organization_id   => l_organization_id
                  , p_action_code   => l_action_code );
                  */
           wf_event.AddParameterToList('APPROVER',l_last_updated_by ,l_parameter_list);
           wf_event.AddParameterToList('SO_LINE_ID',l_so_line_id ,l_parameter_list);
           wf_event.AddParameterToList('SO_HEADER_ID',l_so_header_id ,l_parameter_list);
           wf_event.raise(p_event_name => gme_common_pvt.G_BATCH_RESERVATIONS,
                 p_event_key  => l_session_id,
                 p_parameters => l_parameter_list);

           IF(l_last_updated_by <> l_created_by) THEN

              IF g_debug <= gme_debug.g_log_procedure THEN
                 gme_debug.put_line(g_pkg_name||'.'||l_api_name || 'Notify CSR : l_created_by is '||l_created_by);
                 gme_debug.put_line(g_pkg_name||'.'||l_api_name || 'Initiating the Workflow......');
              END IF;

              /*GME_BATCH_WORKFLOW_PKG.Init_wf( p_session_id  => l_session_id
                     , p_approver    => l_created_by
                     , p_so_header_id=> l_so_header_id
                     , p_so_line_id  => l_so_line_id
                     , p_batch_id    => l_batch_id
                     , p_batch_line_id => NULL
                     , p_fpo_id => NULL
                     , p_organization_id   => l_organization_id
                     , p_action_code   => l_action_code );
                    */
              wf_event.AddParameterToList('APPROVER',l_created_by ,l_parameter_list);
              wf_event.raise(p_event_name => gme_common_pvt.G_BATCH_RESERVATIONS,
                 p_event_key  => l_session_id,
                 p_parameters => l_parameter_list);
           END IF;
        END IF;
     END LOOP;
  END IF;
  l_parameter_list.DELETE;


  IF g_debug <= gme_debug.g_log_procedure THEN
     gme_debug.put_line(g_pkg_name||'.'||l_api_name || 'Exiting Notify_CSR  .............');
  END IF;

EXCEPTION

WHEN OTHERS THEN
  IF g_debug <= gme_debug.g_log_procedure THEN
     gme_debug.put_line(g_pkg_name||'.'||l_api_name || 'WARNING....  In Others Exception in Notify CSR');
     gme_debug.put_line(g_pkg_name||'.'||l_api_name || 'sqlerror'|| SUBSTRB(SQLERRM, 1, 100));
  END IF;

 END notify_CSR;


   PROCEDURE transfer_reservation_to_inv (
      p_matl_dtl_rec           IN              gme_material_details%ROWTYPE
     ,p_transaction_id         IN              NUMBER
     ,x_message_count          OUT NOCOPY      NUMBER
     ,x_message_list           OUT NOCOPY      VARCHAR2
     ,x_return_status          OUT NOCOPY      VARCHAR2)
   IS
      l_api_name   CONSTANT  VARCHAR2 (30)    := 'transfer_reservation_to_inv';
      l_return_status        VARCHAR2 (1);
      l_msg_count            NUMBER;
      l_msg_data             VARCHAR2 (2000);
      l_rsv_tbl              inv_reservation_global.mtl_reservation_tbl_type;
      l_rsv_rec              inv_reservation_global.mtl_reservation_rec_type;
      l_to_rsv_rec           inv_reservation_global.mtl_reservation_rec_type;
      l_rsv_rec_inq          inv_reservation_global.mtl_reservation_rec_type;
      l_mmt_rec              mtl_material_transactions%ROWTYPE;
      l_mmln_tbl             gme_common_pvt.mtl_trans_lots_num_tbl;
      l_rsv_count            NUMBER;
      l_rsv_id               NUMBER;
      l_error_code           NUMBER;
      l_qty_to_transfer      NUMBER := 0;
      l_transfer_quantity    NUMBER := 0;
      l_transfer_primary     NUMBER := 0;
      l_transfer_secondary   NUMBER := 0;
      l_yield_txn_counter    NUMBER;
      l_transfer_complete    VARCHAR2 (1) := 'N';
      l_SO_RSV_exist         VARCHAR2 (1) := 'Y';
      l_dummy_sn             inv_reservation_global.serial_number_tbl_type;
      -- ======================================================================
      l_txn_quantity         NUMBER;
      l_txn_primary          NUMBER;
      l_txn_secondary        NUMBER;
      l_txn_lot              VARCHAR2 (80);
      l_primary_relieved     NUMBER;
      l_secondary_relieved   NUMBER;
      l_primary_remain       NUMBER;
      l_secondary_remain     NUMBER;
      -- ======================================================================

      -- Bug 12805400 
      l_lot_divisible_flag   VARCHAR2(1);      
      l_item_rec             mtl_system_items%ROWTYPE;
      error_get_item           EXCEPTION;
      
      transfer_reservation_err EXCEPTION;
      update_reservation_err   EXCEPTION;
      query_reservation_error  EXCEPTION;
      uom_conversion_error     EXCEPTION;
      get_trans_err            EXCEPTION;
      notify_CSR_err           EXCEPTION;
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api '
                             || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      x_return_status := fnd_api.g_ret_sts_success;

      IF g_debug <= gme_debug.g_log_statement THEN
        gme_debug.put_line
        (   g_pkg_name
        || '.'
        || l_api_name
        || ' Retrieve data for transaction id => '
        || p_transaction_id);
      END IF;

      GME_TRANSACTIONS_PVT.get_transactions (p_transaction_id      => p_transaction_id
                                            ,x_mmt_rec             => l_mmt_rec
                                            ,x_mmln_tbl            => l_mmln_tbl
                                            ,x_return_status       => x_return_status);

      IF x_return_status <> fnd_api.g_ret_sts_success THEN
      	IF (g_debug <= gme_debug.g_log_statement) THEN
          gme_debug.put_line (g_pkg_name||'.'||l_api_name||':'||'Error from get transactions');
       	END IF;
        RAISE get_trans_err;
      END IF;

      -- For each production yield generated, we need to transfer the reservation supply to secure it.
      -- =============================================================================================
      IF NVL(l_mmln_tbl.count,0) > 0 THEN
        l_yield_txn_counter := l_mmln_tbl.count;
      ELSE
        l_yield_txn_counter := 1;
      END IF;
      IF g_debug <= gme_debug.g_log_statement THEN
        gme_debug.put_line
           (   g_pkg_name
           || '.'
           || l_api_name
           || ' Looping through transaction yield for a count of =>  '
           || l_yield_txn_counter);
      END IF;

      FOR x in 1..l_yield_txn_counter LOOP
        IF l_SO_RSV_exist = 'N' THEN
          IF g_debug <= gme_debug.g_log_statement THEN
            gme_debug.put_line
            (   g_pkg_name
            || '.'
            || l_api_name
            || 'No reservations outstanding for this supply line so exit now');
          END IF;
          EXIT;
        END IF;
        IF NVL(l_mmln_tbl.count,0) > 0 THEN
          -- If mmli is populated, use the lot level data
          IF g_debug <= gme_debug.g_log_statement THEN
            gme_debug.put_line
              (   g_pkg_name
               || '.'
               || l_api_name
               || ' Using lot level transaction data, lot is => '
               || l_mmln_tbl(x).lot_number);
          END IF;
          l_txn_quantity := l_mmln_tbl(x).transaction_quantity;
          l_txn_primary  := l_mmln_tbl(x).primary_quantity;
          l_txn_secondary:= l_mmln_tbl(x).secondary_transaction_quantity;
          l_txn_lot      := l_mmln_tbl(x).lot_number;
        ELSE
          -- Use the transaction level data
          IF g_debug <= gme_debug.g_log_statement THEN
            gme_debug.put_line
              (   g_pkg_name
               || '.'
               || l_api_name
               || ' Item is NOT lot controlled       ');
          END IF;
          l_txn_quantity := l_mmt_rec.transaction_quantity;
          l_txn_primary  := l_mmt_rec.primary_quantity;
          l_txn_secondary:= l_mmt_rec.secondary_transaction_quantity;
          l_txn_lot      := NULL;
        END IF;

        IF g_debug <= gme_debug.g_log_statement THEN
          gme_debug.put_line
          (   g_pkg_name
          || '.'
          || l_api_name
          || ' primary transaction quantity to be processed is '
          || l_txn_primary );
        END IF;
        l_transfer_complete := 'N';
        -- LOOP until either:
        --   a) The transaction quantity has been fully transferred from PROD supply to INV supply
        --   OR
        --   b) There are no reservations against PROD supply to transfer
        -- =======================================================================================
        WHILE l_transfer_complete = 'N' LOOP
          -- Query for reserved sales demand against this material detail line.
          -- If reservations exist, we will transfer the supply source from production to inventory
          -- Otherwise, if no sales demand exists, nothing more to do.
          -- =========================================================

          GME_SUPPLY_RES_PVT.query_prod_supply_reservations
             (p_matl_dtl_rec               => p_matl_dtl_rec
             ,x_mtl_reservation_tbl        => l_rsv_tbl
             ,x_mtl_reservation_tbl_count  => l_rsv_count
             ,x_msg_count                  => x_message_count
             ,x_msg_data                   => x_message_list
             ,x_return_status              => x_return_status);

          IF g_debug <= gme_debug.g_log_statement THEN
             gme_debug.put_line
                (   g_pkg_name
                 || '.'
                 || l_api_name
                 || 'Return status from query_prod_supply_reservations is '
                 || x_return_status);
             gme_debug.put_line
                (   g_pkg_name
                 || '.'
                 || l_api_name
                 || ' number of reservations is     '
                 || l_rsv_count);
          END IF;

          IF (x_return_status <> fnd_api.g_ret_sts_success) THEN
            IF g_debug <= gme_debug.g_log_statement THEN
              gme_debug.put_line
                  (   g_pkg_name
                   || '.'
                   || l_api_name
                   || 'Error is :'
                   || x_message_list);
            END IF;
            RAISE query_reservation_error;
          END IF;

          -- if there are no reservations then there is no work to do so exit now
          -- ====================================================================
          IF NVL(l_rsv_count,0) = 0 THEN
            IF g_debug <= gme_debug.g_log_statement THEN
              gme_debug.put_line
                (   g_pkg_name
                 || '.'
                 || l_api_name
                 || 'No SO reservation demand so EXIT now ');
            END IF;
            l_transfer_complete := 'Y';
            EXIT;
          END IF;

          l_rsv_rec := l_rsv_tbl (1);

          -- We will transfer the reserved quantity OR the transaction quantity; whichever is the smaller
          -- ================================================================================================
          IF g_debug <= gme_debug.g_log_statement THEN
            gme_debug.put_line
              (   g_pkg_name
               || '.'
               || l_api_name
               || ' Transaction primary quantity is '
               || l_txn_primary
               || ' Reservation quantity in primary is '
               || l_rsv_rec.primary_reservation_quantity );
          END IF;

          -- Bug 12805400 - Put in check for indivisible item.
          gme_material_detail_pvt.get_item_rec
                            (p_org_id                => l_rsv_rec.organization_id
                            ,p_item_id               => l_rsv_rec.inventory_item_id
                            ,x_item_rec              => l_item_rec
                            ,x_return_status         => l_return_status);

          IF l_return_status <> FND_API.G_RET_STS_SUCCESS THEN
            x_return_status := l_return_status;
            RAISE error_get_item;
          END IF;
          
          l_lot_divisible_flag := 'Y';
          IF l_item_rec.lot_control_code = 2 AND NVL(l_item_rec.lot_divisible_flag,'Y') = 'N' THEN
             l_lot_divisible_flag := 'N';
          END IF;
           
          IF l_txn_primary <= l_rsv_rec.primary_reservation_quantity THEN
            l_transfer_primary := l_txn_primary;
            l_transfer_secondary := l_txn_secondary;
          ELSE
             -- Bug 12805400 - Put in check for indivisible item.
             -- Update the reservation for indivisible items to avoid qty issues.
             -- Take the whole transaction qty if the item is indivisible.
             IF l_lot_divisible_flag = 'N' THEN
                update mtl_reservations
                set reservation_quantity = l_txn_quantity,
                    primary_reservation_quantity = l_txn_primary,
                    secondary_reservation_quantity = l_txn_secondary
                where reservation_id = l_rsv_rec.reservation_id; 
                         
                l_transfer_primary := l_txn_primary;
                l_transfer_secondary := l_txn_secondary;
             ELSE
                l_transfer_primary   := l_rsv_rec.primary_reservation_quantity;
                l_transfer_secondary := l_rsv_rec.secondary_reservation_quantity;
                -- If necessary, Compute the secondary quantity allowing for lot specific conversions
                -- because we are moving from a high level reservation to a detailed reservation.
                -- ==================================================================================
                
                /* Bug#7422975 Added the bwlo conidion to convert the secondary  
                   quantity only when the secondary uom is not null */ 
                
                IF l_txn_lot IS NOT NULL and l_rsv_rec.secondary_uom_code IS NOT NULL THEN
                   l_transfer_secondary := inv_convert.inv_um_convert(
                                        item_id                      => l_rsv_rec.inventory_item_id
                                      , lot_number                   => l_txn_lot
                                      , organization_id              => l_rsv_rec.organization_id
                                      , PRECISION                    => 5
                                      , from_quantity                => l_transfer_primary
                                      , from_unit                    => l_rsv_rec.primary_uom_code
                                      , to_unit                      => l_rsv_rec.secondary_uom_code
                                      , from_name                    => NULL -- from uom name
                                      , to_name                      => NULL -- to uom name
                                      );
                   IF g_debug <= gme_debug.g_log_statement THEN
                     gme_debug.put_line
                       (   g_pkg_name
                        || '.'
                        || l_api_name
                        || ' After UOM conversion the secondary qty is '
                        || l_transfer_secondary );
                   END IF;
                   
                   IF l_transfer_secondary = -99999 THEN
                     -- conversion failed
                     RAISE uom_conversion_error;
                   END IF;
                END IF;
             END IF; -- IF l_lot_divisible_flag = 'N'
          END IF;
          
          IF g_debug <= gme_debug.g_log_statement THEN
            gme_debug.put_line
              (   g_pkg_name
               || '.'
               || l_api_name
               || ' primary quantity to be reserved against INV is'
               || l_transfer_primary );
          END IF;
          -- Set the qty in the reservation UOM if this matches the primary or secondary
          -- else set it to null and the Reservations API will compute it
          -- ============================================================================
          IF l_rsv_rec.primary_uom_code = l_rsv_rec.reservation_uom_code THEN
            l_transfer_quantity := l_transfer_primary;
          ELSIF l_rsv_rec.secondary_uom_code = l_rsv_rec.reservation_uom_code THEN
            l_transfer_quantity := l_transfer_secondary;
          ELSE
            l_transfer_quantity  := NULL;
          END IF;
          IF g_debug <= gme_debug.g_log_statement THEN
            gme_debug.put_line
              (   g_pkg_name
              || '.'
              || l_api_name
              || ' This is the primary quantity we are about to transfer => '
              || l_transfer_primary
              || ' This is the secondary quantity we are about to transfer =>'
              || l_transfer_secondary );
          END IF;

          -- Ensure that the yield inventory is reservable ahead of attempting to transfer
          -- =============================================================================

          IF inv_detail_util_pvt.is_sub_loc_lot_reservable
               (  p_organization_id   => l_rsv_rec.organization_id
               ,  p_inventory_item_id => l_rsv_rec.inventory_item_id
               ,  p_subinventory_code => l_mmt_rec.subinventory_code
               ,  p_locator_id        => l_mmt_rec.locator_id
               ,  p_lot_number        => l_txn_lot) THEN
            -- Target Inventory IS RESERVABLE
            -- Transfer the reservation supply source from JOB to INVENTORY
            -- Use the detailing outlined on the COMPLETION transaction
            -- ============================================================
            IF g_debug <= gme_debug.g_log_statement THEN
              gme_debug.put_line
                (   g_pkg_name
                || '.'
                || l_api_name
                || ' This inventory yield IS reservable so proceed with transfer ');
            END IF;
            l_to_rsv_rec                                := l_rsv_rec;
            l_to_rsv_rec.reservation_id                 := NULL;             -- New reservation
            l_to_rsv_rec.revision                       := l_mmt_rec.revision;
            l_to_rsv_rec.subinventory_code              := l_mmt_rec.subinventory_code;
            l_to_rsv_rec.locator_id                     := l_mmt_rec.locator_id;
            l_to_rsv_rec.lot_number                     := l_txn_lot;
            l_to_rsv_rec.lpn_id                         := l_mmt_rec.transfer_lpn_id; -- Bug 8343517
            l_to_rsv_rec.subinventory_id                := NULL;
            l_to_rsv_rec.lot_number_id                  := NULL;
            l_to_rsv_rec.supply_source_type_id          := inv_reservation_global.g_source_type_inv;
            l_to_rsv_rec.supply_source_header_id        := NULL;
            l_to_rsv_rec.supply_source_line_id          := NULL;
            l_to_rsv_rec.supply_source_name             := NULL;
            l_to_rsv_rec.supply_source_line_detail      := NULL;
            l_to_rsv_rec.reservation_quantity           := l_transfer_quantity;
            l_to_rsv_rec.primary_reservation_quantity   := l_transfer_primary;
            l_to_rsv_rec.secondary_reservation_quantity := l_transfer_secondary;

            IF (g_debug <= gme_debug.g_log_statement) THEN
              gme_debug.put_line ('Calling inv_reservation_pub.transfer_reservation');
            END IF;

            inv_reservation_pub.Transfer_Reservation
              ( p_api_version_number        => 1.0
              , p_init_msg_lst              => FND_API.G_TRUE
              , x_return_status             => x_return_status
              , x_msg_count                 => x_message_count
              , x_msg_data                  => x_message_list
              , p_original_rsv_rec          => l_rsv_rec
              , p_to_rsv_rec                => l_to_rsv_rec
              , p_original_serial_number    => l_dummy_sn
              , p_to_serial_number          => l_dummy_sn
              , p_validation_flag           => FND_API.G_FALSE
              , x_to_reservation_id         => l_rsv_id);

            IF (g_debug <= gme_debug.g_log_statement) THEN
               gme_debug.put_line ('After Calling inv_reservation_pub.transfer_reservation status is '||x_return_status);
            END IF;

            IF (l_return_status IN
              (fnd_api.g_ret_sts_error, fnd_api.g_ret_sts_unexp_error) ) THEN
              RAISE transfer_reservation_err;
            END IF;
          ELSE
            -- Target Inventory IS NOT RESERVABLE so we cannot transfer the reservation
            -- Need to debit the original SUPPLY reservation and notify sales staff is there is sales impact
            -- =============================================================================================
            IF g_debug <= gme_debug.g_log_statement THEN
              gme_debug.put_line
                (   g_pkg_name
                || '.'
                || l_api_name
                || ' This inventory yield IS NOT reservable so CANNOT transfer resv supply for line_id '
                || l_rsv_rec.supply_source_line_id);
            END IF;
            -- If demand is from sales, issue Notification Advising that Reservation Transfer is not possible
            -- ==============================================================================================
            --  Bug 20052174 Also consider demand source type id for ISO's.
            IF l_rsv_rec.demand_source_type_id = inv_reservation_global.g_source_type_oe OR                       l_rsv_rec.demand_source_type_id = INV_RESERVATION_GLOBAL.g_source_type_internal_ord THEN 
              GME_SUPPLY_RES_PVT.notify_CSR
                ( P_Batch_id               =>    NULL
                , P_Batch_line_id          =>    l_rsv_rec.supply_source_line_id
                , P_So_line_id             =>    l_rsv_rec.demand_source_line_id
                , P_batch_trans_id         =>    NULL
                , P_organization_id        =>    p_matl_dtl_rec.organization_id
                , P_action_code            =>    'NON_RSV_STATUS'
                , X_return_status          =>    x_return_status
                , X_msg_cont               =>    x_message_count
                , X_msg_data               =>    x_message_list );

              IF g_debug <= gme_debug.g_log_procedure THEN
                gme_debug.put_line  (  g_pkg_name || '.'
                                   || l_api_name
                                   || ' after calling notify_CSR status is '
                                   || x_return_status );
              END IF;

              IF (x_return_status IN (fnd_api.g_ret_sts_error, fnd_api.g_ret_sts_unexp_error) ) THEN
                RAISE notify_CSR_err;
              END IF;

            END IF;

            IF g_debug <= gme_debug.g_log_statement THEN
              gme_debug.put_line
                (   g_pkg_name
                || '.'
                || l_api_name
                || ' Invoke relieve reservation for quantity of '
                || l_transfer_primary         );
            END IF;
            inv_reservation_pub.relieve_reservation
              ( p_api_version_number          => 1.0
              , p_init_msg_lst                => fnd_api.g_true
              , x_return_status               => x_return_status
              , x_msg_count                   => x_message_count
              , x_msg_data                    => x_message_list
              , p_rsv_rec		      => l_rsv_rec
              , p_primary_relieved_quantity   => l_transfer_primary
              , p_secondary_relieved_quantity => l_transfer_secondary
              , p_relieve_all                 => fnd_api.g_false
              , p_original_serial_number      => l_dummy_sn -- no serial control
              , p_validation_flag             => fnd_api.g_true
              , x_primary_relieved_quantity   => l_primary_relieved
              , x_secondary_relieved_quantity => l_secondary_relieved
              , x_primary_remain_quantity     => l_primary_remain
              , x_secondary_remain_quantity   => l_secondary_remain
              );

            IF g_debug <= gme_debug.g_log_procedure THEN
              gme_debug.put_line  (  g_pkg_name || '.'
                                 || l_api_name
                                 || ' after calling relieve_reservation status is '
                                 || x_return_status );
            END IF;

            IF (x_return_status IN
                       (fnd_api.g_ret_sts_error, fnd_api.g_ret_sts_unexp_error) ) THEN
              RAISE update_reservation_err;
            END IF;
          END IF;        -- End of handling for non reservable inventory

          -- Quantity has processed
          -- Is there outstanding quantity to process for this particular txn quantity
          -- ==========================================================================
          IF l_txn_primary - l_transfer_primary <= 0 THEN
            l_transfer_complete := 'Y';
          ELSE
            l_txn_primary := l_txn_primary - l_transfer_primary;
            l_txn_secondary := l_txn_secondary - l_transfer_secondary;
          END IF;
        END LOOP;   -- WHILE l_l_transfer_complete = 'N'
      END LOOP;   -- Transaction Quantity Processing

      IF g_debug <= gme_debug.g_log_procedure THEN
        gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;

   EXCEPTION
      WHEN uom_conversion_error THEN
         x_return_status := fnd_api.g_ret_sts_error;
         fnd_message.set_name ('GMI', 'IC_API_UOM_CONVERSION_ERROR');
         fnd_message.set_token ('ITEM_NO', l_rsv_rec.inventory_item_id);
         fnd_message.set_token ('FROM_UOM', l_rsv_rec.primary_uom_code);
         fnd_message.set_token ('TO_UOM', l_rsv_rec.secondary_uom_code);
         IF (g_debug <= gme_debug.g_log_error) THEN
            gme_debug.put_line
                        (   'unit of measure conversion error for item '
                         || l_rsv_rec.inventory_item_id
                         || 'from unit  '
                         || l_rsv_rec.primary_uom_code
                         || 'to   unit  '
                         || l_rsv_rec.secondary_uom_code);
         END IF;
      WHEN transfer_reservation_err THEN
         IF (g_debug <= gme_debug.g_log_error) THEN
            gme_debug.put_line
                        (   'terminating due to error in inv_reservation_pub.transfer_reservation ');
            gme_debug.put_line ('error message is ' || x_message_list);
         END IF;
      WHEN update_reservation_err THEN
         IF (g_debug <= gme_debug.g_log_error) THEN
            gme_debug.put_line
                        (   'terminating due to error in inv_reservation_pub.update_reservation ');
            gme_debug.put_line ('error message is ' || x_message_list);
         END IF;
      WHEN query_reservation_error THEN
         IF (g_debug <= gme_debug.g_log_error) THEN
            gme_debug.put_line
                        (   'terminating due to error querying reservations  ');
            gme_debug.put_line ('error message is ' || x_message_list);
         END IF;
      WHEN notify_CSR_err THEN
         IF (g_debug <= gme_debug.g_log_error) THEN
            gme_debug.put_line
                        (   'terminating due to error invoking notify_CSR ');
         END IF;
      WHEN get_trans_err  THEN
         IF (g_debug <= gme_debug.g_log_error) THEN
      	   gme_debug.put_line (g_pkg_name||'.'||l_api_name||':'||'Error from get_transactions');
         END IF;
      WHEN error_get_item  THEN
         IF (g_debug <= gme_debug.g_log_error) THEN
      	   gme_debug.put_line (g_pkg_name||'.'||l_api_name||':'||'Error from get_item_rec');
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

         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
         x_return_status := fnd_api.g_ret_sts_unexp_error;

   END transfer_reservation_to_inv;

   PROCEDURE query_prod_supply_reservations(
      p_matl_dtl_rec               IN              gme_material_details%ROWTYPE
     ,x_mtl_reservation_tbl        OUT NOCOPY      inv_reservation_global.mtl_reservation_tbl_type
     ,x_mtl_reservation_tbl_count  OUT NOCOPY      NUMBER
     ,x_msg_count                  OUT NOCOPY      NUMBER
     ,x_msg_data                   OUT NOCOPY      VARCHAR2
     ,x_return_status              OUT NOCOPY      VARCHAR2)
   IS
      l_api_name   CONSTANT  VARCHAR2 (30)    := 'query_prod_supply_reservations';
      l_return_status        VARCHAR2 (1);
      l_msg_count            NUMBER;
      l_msg_data             VARCHAR2 (2000);
      l_total_primary_demand NUMBER;

      l_rsv_rec_inq         inv_reservation_global.mtl_reservation_rec_type;
      l_error_code          NUMBER;
      l_dummy_sn            inv_reservation_global.serial_number_tbl_type;
      -- ======================================================================
      query_reservation_error  EXCEPTION;
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api '
                             || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      x_return_status := fnd_api.g_ret_sts_success;

      -- If material_detail_id supplied,retrieve the reservations against this supply line
      -- =================================================================================
      IF p_matl_dtl_rec.material_detail_id is NOT NULL THEN
        l_rsv_rec_inq.organization_id := p_matl_dtl_rec.organization_id;
        l_rsv_rec_inq.inventory_item_id := p_matl_dtl_rec.inventory_item_id;
        l_rsv_rec_inq.supply_source_type_id := 5;
        l_rsv_rec_inq.supply_source_header_id := p_matl_dtl_rec.batch_id;
        l_rsv_rec_inq.supply_source_line_id := p_matl_dtl_rec.material_detail_id;
--      l_rsv_rec_inq.demand_source_type_id := 2;

        IF g_debug <= gme_debug.g_log_statement THEN
          gme_debug.put_line
            (   g_pkg_name
             || '.'
             || l_api_name
             || ' Calling inv_reservation_pub.query_reservation for reservations against PROD supply for line'
             || p_matl_dtl_rec.material_detail_id);
        END IF;
      -- If batch_id only supplied,retrieve the reservations against this supply source
      -- ==============================================================================

      ELSE
        l_rsv_rec_inq.supply_source_type_id := 5;
        l_rsv_rec_inq.supply_source_header_id := p_matl_dtl_rec.batch_id;

        IF g_debug <= gme_debug.g_log_statement THEN
          gme_debug.put_line
            (   g_pkg_name
             || '.'
             || l_api_name
             || ' Calling inv_reservation_pub.query_reservation for reservations against PROD supply for batce'
             || p_matl_dtl_rec.batch_id);
        END IF;
      END IF;

      inv_reservation_pub.query_reservation
             (p_api_version_number             => 1.0
             ,p_init_msg_lst                   => fnd_api.g_false
             ,x_return_status                  => x_return_status
             ,x_msg_count                      => x_msg_count
             ,x_msg_data                       => x_msg_data
             ,p_query_input                    => l_rsv_rec_inq
             ,p_lock_records                   => fnd_api.g_false
             ,p_sort_by_req_date               => inv_reservation_global.g_query_req_date_asc
             ,x_mtl_reservation_tbl            => x_mtl_reservation_tbl
             ,x_mtl_reservation_tbl_count      => x_mtl_reservation_tbl_count
             ,x_error_code                     => l_error_code);

      IF g_debug <= gme_debug.g_log_statement THEN
         gme_debug.put_line
            (   g_pkg_name
             || '.'
             || l_api_name
             || 'Return status from inv_reservation_pub.query_reservation is '
             || l_return_status
             || ' Error code is '
             || l_error_code);
      END IF;

      IF (x_return_status <> fnd_api.g_ret_sts_success) THEN
         RAISE query_reservation_error;
      END IF;

      IF g_debug <= gme_debug.g_log_statement THEN
         gme_debug.put_line
            (   g_pkg_name
             || '.'
             || l_api_name
             || ' Number of reservations against this production supply line is '
             || x_mtl_reservation_tbl_count);
      END IF;

      IF g_debug <= gme_debug.g_log_procedure THEN
        gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;

   EXCEPTION
      WHEN query_reservation_error THEN
         IF (g_debug <= gme_debug.g_log_error) THEN
            gme_debug.put_line
                        (   'inv_reservation_pub.query_reservation returns '
                         || x_return_status);
            gme_debug.put_line ('error message is ' || x_msg_data);
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

         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
         x_return_status := fnd_api.g_ret_sts_unexp_error;

   END query_prod_supply_reservations;

   PROCEDURE relieve_prod_supply_resv (
      p_matl_dtl_rec               IN              gme_material_details%ROWTYPE
     ,x_msg_count                  OUT NOCOPY      NUMBER
     ,x_msg_data                   OUT NOCOPY      VARCHAR2
     ,x_return_status              OUT NOCOPY      VARCHAR2)
   IS
      l_api_name   CONSTANT        VARCHAR2 (30)    := 'relieve_prod_supply_resv';
      l_rsv_count                  NUMBER;

      l_primary_to_unreserve       NUMBER;
      l_secondary_to_unreserve     NUMBER;
      l_total_primary_demand       NUMBER;
      l_available_quantity         NUMBER;
      l_rsv_rec                    inv_reservation_global.mtl_reservation_rec_type;
      l_rsv_tbl                    inv_reservation_global.mtl_reservation_tbl_type;
      l_dummy_sn                   inv_reservation_global.serial_number_tbl_type;
      l_primary_relieved           NUMBER;
      l_secondary_relieved         NUMBER;
      l_primary_remain             NUMBER;
      l_secondary_remain           NUMBER;
      -- ======================================================================
      query_reservation_error      EXCEPTION;
      update_reservation_err       EXCEPTION;
      delete_reservation_err       EXCEPTION;
      notify_CSR_err               EXCEPTION;
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api '
                             || g_pkg_name || '.'
                             || l_api_name
                             || ' material_detail_id '
                             || p_matl_dtl_rec.material_detail_id);
      END IF;

      x_return_status := fnd_api.g_ret_sts_success;

      IF p_matl_dtl_rec.line_type not in (1,2) THEN
        RETURN;
      END IF;

      GME_SUPPLY_RES_PVT.query_prod_supply_reservations
             (p_matl_dtl_rec               => p_matl_dtl_rec
             ,x_mtl_reservation_tbl        => l_rsv_tbl
             ,x_mtl_reservation_tbl_count  => l_rsv_count
             ,x_msg_count                  => x_msg_count
             ,x_msg_data                   => x_msg_data
             ,x_return_status              => x_return_status);

      IF g_debug <= gme_debug.g_log_statement THEN
        gme_debug.put_line
          (   g_pkg_name
              || '.'
              || l_api_name
              || 'Return status from query_prod_supply_reservations is '
              || x_return_status);
        gme_debug.put_line
          (   g_pkg_name
              || '.'
              || l_api_name
              || ' number of reservations is     '
              || l_rsv_count);
      END IF;

      IF (x_return_status <> fnd_api.g_ret_sts_success) THEN
        IF g_debug <= gme_debug.g_log_statement THEN
          gme_debug.put_line
          (   g_pkg_name
          || '.'
          || l_api_name
          || 'Error is :'
          || x_msg_data);
        END IF;
        RAISE query_reservation_error;
      END IF;

      IF NVL(l_rsv_count,0) <= 0 THEN
        IF g_debug <= gme_debug.g_log_statement THEN
          gme_debug.put_line
          (   g_pkg_name
          || '.'
          || l_api_name
          || ' No reservations against material detail line as a source of supply so RETURN now ');
        END IF;
        RETURN;
      END IF;

      -- Compute the total reservational demand against this material detail line.
      -- =========================================================================
      FOR I IN 1..l_rsv_tbl.COUNT LOOP
        l_total_primary_demand := NVL(l_total_primary_demand,0) + l_rsv_tbl(I).primary_reservation_quantity;
      END LOOP;

      -- Determine the available supply quantity in the primary UOM
      -- ==========================================================
      l_available_quantity := NVL(p_matl_dtl_rec.wip_plan_qty, p_matl_dtl_rec.plan_qty) - p_matl_dtl_rec.actual_qty;
      IF l_rsv_tbl(1).primary_uom_code <> p_matl_dtl_rec.dtl_um THEN
        l_available_quantity := inv_convert.inv_um_convert
                                 (item_id       => p_matl_dtl_rec.inventory_item_id
                                 ,precision     => 5
                                 ,from_quantity => l_available_quantity
                                 ,from_unit     => p_matl_dtl_rec.dtl_um
                                 ,to_unit       => l_rsv_tbl(1).primary_uom_code
                                 ,from_name     => NULL
                                 ,to_name       => NULL);
      END IF;
      -- Compare supply and demand. If supply meets/exceeds demand, there is nothing to do
      -- =================================================================================
      IF g_debug <= gme_debug.g_log_statement THEN
        gme_debug.put_line
        (   g_pkg_name
        || '.'
        || l_api_name
        || ' Current demand figure => '
        || l_total_primary_demand
        || ' Current supply figure => '
        || l_available_quantity);
      END IF;
      IF l_available_quantity >= l_total_primary_demand THEN
        IF g_debug <= gme_debug.g_log_statement THEN
          gme_debug.put_line
          (   g_pkg_name
          || '.'
          || l_api_name
          || ' Supply exceeds Demand so no adjustments required; return now ');
        END IF;
        RETURN;
      END IF;

      l_primary_to_unreserve := l_total_primary_demand - l_available_quantity;

      FOR I IN 1..l_rsv_tbl.COUNT LOOP

        l_rsv_rec := l_rsv_tbl(I);
        IF g_debug <= gme_debug.g_log_procedure THEN
           gme_debug.put_line  (  g_pkg_name || '.'
                               || l_api_name
                               || ' primary reserved qty '
                               || l_rsv_rec.primary_reservation_quantity);
        END IF;

        -- Notify the sales department that reservations are being relieved
        -- ================================================================
        GME_SUPPLY_RES_PVT.notify_CSR
          ( P_Batch_id               =>    p_matl_dtl_rec.batch_id
          , P_Batch_line_id          =>    p_matl_dtl_rec.material_detail_id
          , P_So_line_id             =>    l_rsv_rec.demand_source_line_id
          , P_batch_trans_id         =>    NULL
          , P_organization_id        =>    p_matl_dtl_rec.organization_id
          , P_action_code            =>    'REDUCE_PLANNED_QTY'
          , X_return_status          =>    x_return_status
          , X_msg_cont               =>    x_msg_count
          , X_msg_data               =>    x_msg_data );

        IF g_debug <= gme_debug.g_log_procedure THEN
          gme_debug.put_line  (  g_pkg_name || '.'
                             || l_api_name
                             || ' after calling notify_CSR status is '
                             || x_return_status );
        END IF;

        IF (x_return_status IN
                   (fnd_api.g_ret_sts_error, fnd_api.g_ret_sts_unexp_error) ) THEN
          RAISE notify_CSR_err;
        END IF;

        IF (l_rsv_rec.primary_reservation_quantity <= l_primary_to_unreserve) THEN
          IF g_debug <= gme_debug.g_log_procedure THEN
            gme_debug.put_line  (  g_pkg_name || '.'
                                || l_api_name
                                || ' about to invoke delete_reservation ');
          END IF;

          inv_reservation_pub.delete_reservation
          ( p_api_version_number      => 1.0
          , p_init_msg_lst            => fnd_api.g_true
          , x_return_status           => x_return_status
          , x_msg_count               => x_msg_count
          , x_msg_data                => x_msg_data
          , p_rsv_rec                 => l_rsv_rec
          , p_serial_number           => l_dummy_sn
          );

          IF g_debug <= gme_debug.g_log_procedure THEN
            gme_debug.put_line  (  g_pkg_name || '.'
                                || l_api_name
                                || ' after calling delete_reservation status is '
                                || x_return_status );
          END IF;


          IF (x_return_status IN
                     (fnd_api.g_ret_sts_error, fnd_api.g_ret_sts_unexp_error) ) THEN
            RAISE delete_reservation_err;
          END IF;

          l_primary_to_unreserve := l_primary_to_unreserve -
                             l_rsv_rec.primary_reservation_quantity;


        ELSE -- res rec qty > l_primary_to_unreserve

          IF l_rsv_rec.secondary_uom_code is not NULL THEN
            l_secondary_to_unreserve := inv_convert.inv_um_convert(
                   item_id           =>  l_rsv_rec.inventory_item_id
                 , lot_number        =>  l_rsv_rec.lot_number
                 , organization_id   =>  l_rsv_rec.organization_id
                 , precision         =>  NULL
                 , from_quantity     =>  l_primary_to_unreserve
                 , from_unit         =>  l_rsv_rec.primary_uom_code
                 , to_unit           =>  l_rsv_rec.secondary_uom_code
                 , from_name         =>  NULL
                 , to_name           =>  NULL);
          END IF;

          IF g_debug <= gme_debug.g_log_procedure THEN
            gme_debug.put_line  (  g_pkg_name || '.'
                               || l_api_name
                               || 'qty to relieve =>  '
                               || l_primary_to_unreserve
                               || ' secondary qty to relieve =>  '
                               || l_secondary_to_unreserve
                               || ' call relieve_reservation  ');
          END IF;

          inv_reservation_pub.relieve_reservation
              ( p_api_version_number          => 1.0
              , p_init_msg_lst                => fnd_api.g_true
              , x_return_status               => x_return_status
              , x_msg_count                   => x_msg_count
              , x_msg_data                    => x_msg_data
              , p_rsv_rec		      => l_rsv_rec
              , p_primary_relieved_quantity   => l_primary_to_unreserve
              , p_secondary_relieved_quantity => l_secondary_to_unreserve
              , p_relieve_all                 => fnd_api.g_false
              , p_original_serial_number      => l_dummy_sn -- no serial control
              , p_validation_flag             => fnd_api.g_true
              , x_primary_relieved_quantity   => l_primary_relieved
              , x_secondary_relieved_quantity => l_secondary_relieved
              , x_primary_remain_quantity     => l_primary_remain
              , x_secondary_remain_quantity   => l_secondary_remain
              );

          IF g_debug <= gme_debug.g_log_procedure THEN
            gme_debug.put_line  (  g_pkg_name || '.'
                               || l_api_name
                               || ' after calling relieve_reservation status is '
                               || x_return_status );
          END IF;

          IF (x_return_status IN
                     (fnd_api.g_ret_sts_error, fnd_api.g_ret_sts_unexp_error) ) THEN
            RAISE update_reservation_err;
          END IF;

          l_primary_to_unreserve := 0;

        END IF;

        IF (l_primary_to_unreserve <= 0) THEN
           -- job done so exit the loop
           EXIT;
        END IF;

      END LOOP;

      IF g_debug <= gme_debug.g_log_procedure THEN
        gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;
   EXCEPTION
      WHEN delete_reservation_err THEN
         IF (g_debug <= gme_debug.g_log_error) THEN
            gme_debug.put_line
                        (   'inv_reservation_pub.delete_reservation failure ');
            gme_debug.put_line ('error message is ' || x_msg_data);
         END IF;
      WHEN update_reservation_err THEN
         IF (g_debug <= gme_debug.g_log_error) THEN
            gme_debug.put_line
                        (   'inv_reservation_pub.relieve_reservation failure ');
            gme_debug.put_line ('error message is ' || x_msg_data);
         END IF;
      WHEN notify_CSR_err THEN
         IF (g_debug <= gme_debug.g_log_error) THEN
            gme_debug.put_line
                        (   'inv_reservation_pub.update_reservation failure ');
            gme_debug.put_line ('error message is ' || x_msg_data);
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

         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
         x_return_status := fnd_api.g_ret_sts_unexp_error;

   END relieve_prod_supply_resv;

   PROCEDURE delete_prod_supply_resv (
      p_matl_dtl_rec               IN              gme_material_details%ROWTYPE
     ,x_msg_count                  OUT NOCOPY      NUMBER
     ,x_msg_data                   OUT NOCOPY      VARCHAR2
     ,x_return_status              OUT NOCOPY      VARCHAR2)
   IS
      l_api_name   CONSTANT  VARCHAR2 (30)    := 'delete_prod_supply_resv';
      l_rsv_count            NUMBER;

      l_matl_dtl_rec         gme_material_details%ROWTYPE;
      l_rsv_rec              inv_reservation_global.mtl_reservation_rec_type;
      l_rsv_tbl              inv_reservation_global.mtl_reservation_tbl_type;
      l_dummy_sn             inv_reservation_global.serial_number_tbl_type;
      -- ======================================================================
      query_reservation_error  EXCEPTION;
      update_reservation_err   EXCEPTION;
      delete_reservation_err   EXCEPTION;
      matl_fetch_error         EXCEPTION;
      notify_CSR_err           EXCEPTION;
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api '
                             || g_pkg_name || '.'
                             || l_api_name
                             || ' material_detail_id '
                             || p_matl_dtl_rec.material_detail_id);
      END IF;

      x_return_status := fnd_api.g_ret_sts_success;

      -- The line may not have been fully populated so retrieve it if necessary
      -- ======================================================================
      IF p_matl_dtl_rec.line_type is NULL THEN
        IF (NOT (gme_material_details_dbl.fetch_row (p_matl_dtl_rec
                                                   ,l_matl_dtl_rec) ) ) THEN
            RAISE matl_fetch_error;
        END IF;
      ELSE
        l_matl_dtl_rec := p_matl_dtl_rec;
      END IF;

      IF NVL(p_matl_dtl_rec.line_type,1) not in (1,2) THEN
        RETURN;
      END IF;

      GME_SUPPLY_RES_PVT.query_prod_supply_reservations
             (p_matl_dtl_rec               => l_matl_dtl_rec
             ,x_mtl_reservation_tbl        => l_rsv_tbl
             ,x_mtl_reservation_tbl_count  => l_rsv_count
             ,x_msg_count                  => x_msg_count
             ,x_msg_data                   => x_msg_data
             ,x_return_status              => x_return_status);

      IF g_debug <= gme_debug.g_log_statement THEN
        gme_debug.put_line
          (   g_pkg_name
              || '.'
              || l_api_name
              || 'Return status from query_prod_supply_reservations is '
              || x_return_status);
        gme_debug.put_line
          (   g_pkg_name
              || '.'
              || l_api_name
              || ' number of reservations is     '
              || l_rsv_count);
      END IF;

      IF (x_return_status <> fnd_api.g_ret_sts_success) THEN
        IF g_debug <= gme_debug.g_log_statement THEN
          gme_debug.put_line
          (   g_pkg_name
          || '.'
          || l_api_name
          || 'Error is :'
          || x_msg_data);
        END IF;
        RAISE query_reservation_error;
      END IF;

      IF NVL(l_rsv_count,0) <= 0 THEN
        IF g_debug <= gme_debug.g_log_statement THEN
          gme_debug.put_line
          (   g_pkg_name
          || '.'
          || l_api_name
          || ' No reservations against material detail line as a source of supply so RETURN now ');
        END IF;
        RETURN;
      END IF;

      -- Delete the reservations against this this material detail line.
      -- =========================================================================
      FOR I IN 1..l_rsv_tbl.COUNT LOOP

        l_rsv_rec := l_rsv_tbl(I);
        -- Set up notifccation ahead of deleting the reservation
        -- This will advise the sales representative of the deletion
        -- =========================================================

        GME_SUPPLY_RES_PVT.notify_CSR
          ( P_Batch_id               =>    l_rsv_rec.supply_source_header_id
          , P_Batch_line_id          =>    l_rsv_rec.supply_source_line_id
          , P_So_line_id             =>    l_rsv_rec.demand_source_line_id
          , P_batch_trans_id         =>    NULL
          , P_organization_id        =>    l_rsv_rec.organization_id
          , P_action_code            =>    'CANCEL_BATCH'
          , X_return_status          =>    x_return_status
          , X_msg_cont               =>    x_msg_count
          , X_msg_data               =>    x_msg_data );

        IF g_debug <= gme_debug.g_log_procedure THEN
          gme_debug.put_line  (  g_pkg_name || '.'
                             || l_api_name
                             || ' after calling notify_CSR status is '
                             || x_return_status );
        END IF;

        IF (x_return_status IN
                   (fnd_api.g_ret_sts_error, fnd_api.g_ret_sts_unexp_error) ) THEN
          RAISE notify_CSR_err;
        END IF;

        IF g_debug <= gme_debug.g_log_procedure THEN
          gme_debug.put_line  (  g_pkg_name || '.'
                              || l_api_name
                              || ' about to invoke delete_reservation for reservation id '
                              || l_rsv_rec.reservation_id);
        END IF;

        inv_reservation_pub.delete_reservation
          ( p_api_version_number      => 1.0
          , p_init_msg_lst            => fnd_api.g_true
          , x_return_status           => x_return_status
          , x_msg_count               => x_msg_count
          , x_msg_data                => x_msg_data
          , p_rsv_rec                 => l_rsv_rec
          , p_serial_number           => l_dummy_sn
          );

        IF g_debug <= gme_debug.g_log_procedure THEN
          gme_debug.put_line  (  g_pkg_name || '.'
                              || l_api_name
                              || ' after calling delete_reservation status is '
                              || x_return_status );
        END IF;


        IF (x_return_status IN
                   (fnd_api.g_ret_sts_error, fnd_api.g_ret_sts_unexp_error) ) THEN
          RAISE delete_reservation_err;
        END IF;

      END LOOP;
      IF g_debug <= gme_debug.g_log_procedure THEN
        gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;

   EXCEPTION
      WHEN delete_reservation_err THEN
         IF (g_debug <= gme_debug.g_log_error) THEN
            gme_debug.put_line
                        (   'inv_reservation_pub.delete_reservation failure ');
            gme_debug.put_line ('error message is ' || x_msg_data);
         END IF;
      WHEN notify_CSR_err THEN
         IF (g_debug <= gme_debug.g_log_error) THEN
            gme_debug.put_line
                        (   'gme_supply_res_pvt.notify_CSR failure ');
            gme_debug.put_line ('error message is ' || x_msg_data);
         END IF;
      WHEN matl_fetch_error THEN
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

         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
         x_return_status := fnd_api.g_ret_sts_unexp_error;

   END delete_prod_supply_resv;

   PROCEDURE delete_batch_prod_supply_resv (
      p_batch_header_rec           IN              gme_batch_header%ROWTYPE
     ,x_msg_count                  OUT NOCOPY      NUMBER
     ,x_msg_data                   OUT NOCOPY      VARCHAR2
     ,x_return_status              OUT NOCOPY      VARCHAR2)
   IS
      l_api_name   CONSTANT  VARCHAR2 (30)    := 'delete_prod_supply_resv';
      l_rsv_count            NUMBER;

      l_matl_dtl_rec         gme_material_details%ROWTYPE;
      l_rsv_rec              inv_reservation_global.mtl_reservation_rec_type;
      l_rsv_tbl              inv_reservation_global.mtl_reservation_tbl_type;
      l_dummy_sn             inv_reservation_global.serial_number_tbl_type;
      -- ======================================================================
      query_reservation_error  EXCEPTION;
      update_reservation_err   EXCEPTION;
      delete_reservation_err   EXCEPTION;
      matl_fetch_error         EXCEPTION;
      notify_CSR_err           EXCEPTION;
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api '
                             || g_pkg_name || '.'
                             || l_api_name
                             || ' batch_id => '
                             || p_batch_header_rec.batch_id);
      END IF;

      x_return_status := fnd_api.g_ret_sts_success;

      -- Query all the reservations agains this batch as a source of supply
      l_matl_dtl_rec.batch_id := p_batch_header_rec.batch_id;
      GME_SUPPLY_RES_PVT.query_prod_supply_reservations
             (p_matl_dtl_rec               => l_matl_dtl_rec
             ,x_mtl_reservation_tbl        => l_rsv_tbl
             ,x_mtl_reservation_tbl_count  => l_rsv_count
             ,x_msg_count                  => x_msg_count
             ,x_msg_data                   => x_msg_data
             ,x_return_status              => x_return_status);

      IF g_debug <= gme_debug.g_log_statement THEN
        gme_debug.put_line
          (   g_pkg_name
              || '.'
              || l_api_name
              || 'Return status from query_prod_supply_reservations is '
              || x_return_status);
        gme_debug.put_line
          (   g_pkg_name
              || '.'
              || l_api_name
              || ' number of reservations is     '
              || l_rsv_count);
      END IF;

      IF (x_return_status <> fnd_api.g_ret_sts_success) THEN
        IF g_debug <= gme_debug.g_log_statement THEN
          gme_debug.put_line
          (   g_pkg_name
          || '.'
          || l_api_name
          || 'Error is :'
          || x_msg_data);
        END IF;
        RAISE query_reservation_error;
      END IF;

      IF NVL(l_rsv_count,0) <= 0 THEN
        IF g_debug <= gme_debug.g_log_statement THEN
          gme_debug.put_line
          (   g_pkg_name
          || '.'
          || l_api_name
          || ' No reservations against batch as a source of supply so RETURN now ');
        END IF;
        RETURN;
      END IF;

      -- Set up notifications ahead of deleting the reservation
      -- This will advise the sales representative of the deletion
      -- =========================================================

      GME_SUPPLY_RES_PVT.notify_CSR
          ( P_Batch_id               =>    p_batch_header_rec.batch_id
          , P_Batch_line_id          =>    NULL
          , P_So_line_id             =>    NULL
          , P_batch_trans_id         =>    NULL
          , P_organization_id        =>    l_rsv_rec.organization_id
          , P_action_code            =>    'CANCEL_BATCH'
          , X_return_status          =>    x_return_status
          , X_msg_cont               =>    x_msg_count
          , X_msg_data               =>    x_msg_data );

      IF g_debug <= gme_debug.g_log_procedure THEN
        gme_debug.put_line  (  g_pkg_name || '.'
                           || l_api_name
                           || ' after calling notify_CSR status is '
                           || x_return_status );
      END IF;

      -- Delete the reservations against this this batch
      -- ===============================================
      FOR I IN 1..l_rsv_tbl.COUNT LOOP

        l_rsv_rec := l_rsv_tbl(I);
        -- Set up notifications ahead of deleting the reservation
        -- This will advise the sales representative of the deletion
        -- =========================================================

        IF (x_return_status IN
                   (fnd_api.g_ret_sts_error, fnd_api.g_ret_sts_unexp_error) ) THEN
          RAISE notify_CSR_err;
        END IF;

        IF g_debug <= gme_debug.g_log_procedure THEN
          gme_debug.put_line  (  g_pkg_name || '.'
                              || l_api_name
                              || ' about to invoke delete_reservation for reservation id '
                              || l_rsv_rec.reservation_id);
        END IF;

        inv_reservation_pub.delete_reservation
          ( p_api_version_number      => 1.0
          , p_init_msg_lst            => fnd_api.g_true
          , x_return_status           => x_return_status
          , x_msg_count               => x_msg_count
          , x_msg_data                => x_msg_data
          , p_rsv_rec                 => l_rsv_rec
          , p_serial_number           => l_dummy_sn
          );

        IF g_debug <= gme_debug.g_log_procedure THEN
          gme_debug.put_line  (  g_pkg_name || '.'
                              || l_api_name
                              || ' after calling delete_reservation status is '
                              || x_return_status );
        END IF;


        IF (x_return_status IN
                   (fnd_api.g_ret_sts_error, fnd_api.g_ret_sts_unexp_error) ) THEN
          RAISE delete_reservation_err;
        END IF;

      END LOOP;
      IF g_debug <= gme_debug.g_log_procedure THEN
        gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;

   EXCEPTION
      WHEN delete_reservation_err THEN
         IF (g_debug <= gme_debug.g_log_error) THEN
            gme_debug.put_line
                        (   'inv_reservation_pub.delete_reservation failure ');
            gme_debug.put_line ('error message is ' || x_msg_data);
         END IF;
      WHEN notify_CSR_err THEN
         IF (g_debug <= gme_debug.g_log_error) THEN
            gme_debug.put_line
                        (   'gme_supply_res_pvt.notify_CSR failure ');
            gme_debug.put_line ('error message is ' || x_msg_data);
         END IF;
      WHEN matl_fetch_error THEN
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

         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
         x_return_status := fnd_api.g_ret_sts_unexp_error;

   END delete_batch_prod_supply_resv;

END GME_SUPPLY_RES_PVT;
/
Commit;
exit;


