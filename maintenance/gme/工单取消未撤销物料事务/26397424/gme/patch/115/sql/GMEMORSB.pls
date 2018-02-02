REM dbdrv: sql ~PROD ~PATH ~FILE none none none package &phase=plb \
REM dbdrv: checkfile(120.12.12010000.2=120.12.12020000.2)(120.11.12000000.2=120.12):~PROD:~PATH:~FILE
SET VERIFY OFF;
WHENEVER SQLERROR EXIT FAILURE ROLLBACK;
WHENEVER OSERROR EXIT FAILURE ROLLBACK;
create or replace PACKAGE BODY GME_MOBILE_RSRV AS
/*  $Header: GMEMORSB.pls 120.12.12020000.2 2015/07/23 14:55:02 adeshmuk ship $     */
/*===========================================================================+
 |      Copyright (c) 2005, 2015 Oracle Corporation, Redwood Shores, CA, USA       |
 |                         All rights reserved.                              |
 |===========================================================================|
 |                                                                           |
 | PL/SQL Package to support the (Java) GME Mobile Application.              |
 | Contains PL/SQL cursors used by the mobile reservation transactions       |
 |                                                                           |
 +===========================================================================+
 |  HISTORY                                                                  |
 |                                                                           |
 | Date          Who               What                                      |
 | ====          ===               ====                                      |
 | 26-Apr-05     Eddie Oumerretane First version                             |
 | 23-Jun-06     Shrikant Nene     Bug 5263908                               |
 | 23-Jun-06     Shrikant Nene     Bug 5263908                               |
 |   Changed Create_Reservation procedure to fetch material detail rec       |
 |   before calling create_reservation API 
 | 07-Jul-15     Jagadiswar Devarla Bug 19619931                             |
 |    Added lpn id in the select statement of Get_Material_Dtl_Reservations  |
 |    procedure.                                                             |
 +===========================================================================*/

  g_debug      VARCHAR2 (5) := fnd_profile.VALUE ('AFLOG_LEVEL');

 /*+========================================================================+
   | PROCEDURE NAME
   |   Fetch_Lot_Reservations
   |
   | USAGE
   |
   | ARGUMENTS
   |
   | RETURNS
   |
   | HISTORY
   |   Created  26-Apr-05 Eddie Oumerretane
   |
   +========================================================================+*/
  PROCEDURE Fetch_Lot_Reservations(p_organization_id     IN         NUMBER,
                                   p_item_id            IN         NUMBER,
                                   p_lot_number          IN         VARCHAR2,
                                   x_return_status       OUT NOCOPY VARCHAR2,
                                   x_error_msg           OUT NOCOPY VARCHAR2,
                                   x_rsrv_cursor         OUT NOCOPY t_genref)
  IS
    l_date_format VARCHAR2(100);
  BEGIN
    x_return_status := FND_API.G_RET_STS_SUCCESS;
    x_error_msg     := ' ';

    FND_PROFILE.GET('MWA_DATE_FORMAT_MASK',l_date_format);

    IF l_date_format IS NULL THEN
      FND_PROFILE.GET('ICX_DATE_FORMAT_MASK',l_date_format);
    END IF;

    OPEN x_rsrv_cursor FOR

      SELECT
        mr.RESERVATION_ID
        ,TO_CHAR(mr.REQUIREMENT_DATE, l_date_format)
        ,mr.PRIMARY_UOM_CODE
        ,mr.RESERVATION_UOM_CODE
        ,NVL(mr.RESERVATION_QUANTITY,0)
        ,NVL(mr.PRIMARY_RESERVATION_QUANTITY,0)
        ,mr.SUBINVENTORY_CODE
        ,mr.SUBINVENTORY_ID
        ,NVL(mr.LOCATOR_ID, -1)
        ,mr.LOT_NUMBER
        ,mr.LOT_NUMBER_ID
        ,NVL(mr.DETAILED_QUANTITY,0)
        ,NVL(mr.SECONDARY_DETAILED_QUANTITY,0)
        ,NVL(mr.SECONDARY_RESERVATION_QUANTITY,0)
        ,mr.SECONDARY_UOM_CODE
        ,mr.inventory_item_id
        ,mr.revision
      FROM   mtl_reservations mr
      WHERE  mr.organization_id = p_organization_id
             AND mr.inventory_item_id = p_item_id
             AND mr.lot_number = p_lot_number;

  EXCEPTION
    WHEN OTHERS THEN
      IF g_debug <= gme_debug.g_log_unexpected THEN
        gme_debug.put_line('When others exception in Fetch Lot Reservation');
      END IF;
      fnd_msg_pub.add_exc_msg('GME_MOBILE_RSRV','fetch_lot_reservations');
      x_return_status := FND_API.G_RET_STS_UNEXP_ERROR;
      x_error_msg     := fnd_message.get;

  END Fetch_Lot_Reservations;

 /*+========================================================================+
   | PROCEDURE NAME
   |   Check_Rsrv_Exist
   |
   | USAGE
   |
   | ARGUMENTS
   |
   | RETURNS
   |
   | HISTORY
   |   Created  26-Apr-05 Eddie Oumerretane
   |
   +========================================================================+*/
  PROCEDURE Check_Rsrv_Exist(p_organization_id     IN         NUMBER,
                             p_batch_id            IN         NUMBER,
                             p_material_detail_id  IN         NUMBER,
                             p_subinventory_code   IN         VARCHAR2,
                             p_locator_id          IN         NUMBER,
                             p_lot_number          IN         VARCHAR2,
                             p_exclude_res_id      IN         NUMBER,
                             x_return_status       OUT NOCOPY VARCHAR2,
                             x_error_msg           OUT NOCOPY VARCHAR2,
                             x_rsrv_cursor         OUT NOCOPY t_genref)
  IS
  BEGIN
    x_return_status := FND_API.G_RET_STS_SUCCESS;
    x_error_msg     := ' ';


    OPEN x_rsrv_cursor FOR

      SELECT
        mr.RESERVATION_ID
      FROM   mtl_reservations mr,
             wms_item_locations_kfv loc
      WHERE  mr.organization_id = p_organization_id
             AND mr.demand_source_type_id = gme_common_pvt.g_txn_source_type
             AND mr.demand_source_header_id = p_batch_id
             AND mr.demand_source_line_id = p_material_detail_id
             AND NVL(mr.subinventory_code, '-1') = NVL(p_subinventory_code, '-1')
             AND NVL(mr.locator_id, -1) = NVL(p_locator_id, -1)
             AND mr.organization_id   = loc.organization_id(+)
             AND mr.subinventory_code = loc.subinventory_code(+)
             AND mr.locator_id     = loc.inventory_location_id(+)
             AND NVL(mr.lot_number, '-1') = NVL(p_lot_number, '-1')
             AND mr.reservation_id <> NVL(p_exclude_res_id, 0)
             AND NOT EXISTS (SELECT 1
                             FROM   mtl_material_transactions_temp
                             WHERE  reservation_id = mr.reservation_id);

  EXCEPTION
    WHEN OTHERS THEN
      IF g_debug <= gme_debug.g_log_unexpected THEN
        gme_debug.put_line('When others exception in Check Rsrv Exist');
      END IF;
      fnd_msg_pub.add_exc_msg('GME_MOBILE_RSRV','Check_Rsrv_Exist');
      x_return_status := FND_API.G_RET_STS_UNEXP_ERROR;
      x_error_msg     := fnd_message.get;

  END Check_Rsrv_Exist;

 /*+========================================================================+
   | PROCEDURE NAME
   |  Get_Material_Reservations
   |
   | USAGE
   |
   | ARGUMENTS
   |
   | RETURNS
   |
   | HISTORY
   |   Created  26-Apr-05 Eddie Oumerretane
   |
   +========================================================================+*/
  PROCEDURE Get_Material_Reservations(p_organization_id     IN         NUMBER,
                                      p_batch_id            IN         NUMBER,
                                      p_material_detail_id  IN         NUMBER,
                                      p_subinventory_code   IN         VARCHAR2,
                                      p_locator_id          IN         NUMBER,
                                      p_lot_number          IN         VARCHAR2,
                                      x_return_status       OUT NOCOPY VARCHAR2,
                                      x_error_msg           OUT NOCOPY VARCHAR2,
                                      x_rsrv_cursor         OUT NOCOPY t_genref)
  IS
    l_date_format VARCHAR2(100);
  BEGIN
    x_return_status := FND_API.G_RET_STS_SUCCESS;
    x_error_msg     := ' ';

    FND_PROFILE.GET('MWA_DATE_FORMAT_MASK',l_date_format);

    IF l_date_format IS NULL THEN
      FND_PROFILE.GET('ICX_DATE_FORMAT_MASK',l_date_format);
    END IF;

    OPEN x_rsrv_cursor FOR

      SELECT
        mr.RESERVATION_ID
        ,TO_CHAR(mr.REQUIREMENT_DATE, l_date_format)
        ,mr.PRIMARY_UOM_CODE
        ,mr.RESERVATION_UOM_CODE
        ,NVL(mr.RESERVATION_QUANTITY,0)
        ,NVL(mr.PRIMARY_RESERVATION_QUANTITY,0)
        ,mr.SUBINVENTORY_CODE
        ,mr.SUBINVENTORY_ID
        ,NVL(mr.LOCATOR_ID, -1)
        ,mr.LOT_NUMBER
        ,mr.LOT_NUMBER_ID
        ,NVL(mr.DETAILED_QUANTITY,0)
        ,NVL(mr.SECONDARY_DETAILED_QUANTITY,0)
        ,NVL(mr.SECONDARY_RESERVATION_QUANTITY,0)
        ,mr.SECONDARY_UOM_CODE
        ,mr.inventory_item_id
        ,loc.concatenated_segments
        ,mr.revision
      FROM   mtl_reservations mr,
             wms_item_locations_kfv loc
      WHERE  mr.organization_id = p_organization_id
             AND mr.demand_source_type_id = gme_common_pvt.g_txn_source_type
             AND mr.demand_source_header_id = p_batch_id
             AND mr.demand_source_line_id = p_material_detail_id
             AND (p_subinventory_code IS NULL OR (mr.subinventory_code = p_subinventory_code))
             AND (p_locator_id IS NULL OR (mr.locator_id = p_locator_id))
             AND mr.organization_id   = loc.organization_id(+)
             AND mr.subinventory_code = loc.subinventory_code(+)
             AND mr.locator_id     = loc.inventory_location_id(+)
             AND (p_lot_number IS NULL OR (mr.lot_number = p_lot_number))
             AND NOT EXISTS (SELECT 1
                             FROM   mtl_material_transactions_temp
                             WHERE  reservation_id = mr.reservation_id)
      ORDER BY mr.requirement_date;

  EXCEPTION
    WHEN OTHERS THEN
      IF g_debug <= gme_debug.g_log_unexpected THEN
        gme_debug.put_line('When others exception in get Mtl Reservation');
      END IF;
      fnd_msg_pub.add_exc_msg('GME_MOBILE_RSRV','get_material_reservations');
      x_return_status := FND_API.G_RET_STS_UNEXP_ERROR;
      x_error_msg     := fnd_message.get;

  END get_material_reservations;



 /* Bug#5663458
  * Created the following procedure. This procedure is to get DLR of material line.
  * Item revision is also considered to determine DLR
  */
  /* Bug#19619931
   * Added mr.LPN_ID in the select statement.
   */
  PROCEDURE Get_Material_Dtl_Reservations(p_organization_id      IN         NUMBER,
                                           p_batch_id            IN         NUMBER,
                                           p_material_detail_id  IN         NUMBER,
                                           p_eff_loccontrol      IN         NUMBER,
                                           p_lotcontrol          IN         NUMBER,
                                           p_revcontrol          IN         NUMBER,
                                           x_return_status       OUT NOCOPY VARCHAR2,
                                           x_error_msg           OUT NOCOPY VARCHAR2,
                                           x_rsrv_cursor         OUT NOCOPY t_genref)
  IS
   l_date_format VARCHAR2(100);
  BEGIN
   x_return_status := FND_API.G_RET_STS_SUCCESS;
    x_error_msg     := ' ';

    FND_PROFILE.GET('MWA_DATE_FORMAT_MASK',l_date_format);

    IF l_date_format IS NULL THEN
      FND_PROFILE.GET('ICX_DATE_FORMAT_MASK',l_date_format);
    END IF;

    OPEN x_rsrv_cursor FOR

      SELECT
        mr.RESERVATION_ID
        ,TO_CHAR(mr.REQUIREMENT_DATE, l_date_format)
        ,mr.PRIMARY_UOM_CODE
        ,mr.RESERVATION_UOM_CODE
        ,NVL(mr.RESERVATION_QUANTITY,0)
        ,NVL(mr.PRIMARY_RESERVATION_QUANTITY,0)
        ,mr.SUBINVENTORY_CODE
        ,mr.SUBINVENTORY_ID
        ,NVL(mr.LOCATOR_ID, -1)
        ,mr.LOT_NUMBER
        ,mr.LOT_NUMBER_ID
        ,NVL(mr.DETAILED_QUANTITY,0)
        ,NVL(mr.SECONDARY_DETAILED_QUANTITY,0)
        ,NVL(mr.SECONDARY_RESERVATION_QUANTITY,0)
        ,mr.SECONDARY_UOM_CODE
        ,mr.inventory_item_id
        ,loc.concatenated_segments
        ,mr.revision
        ,NVL(mr.LPN_ID,-1)--Bug#19619931
      FROM   mtl_reservations mr,
             wms_item_locations_kfv loc
      WHERE  mr.organization_id = p_organization_id
             AND mr.demand_source_type_id = gme_common_pvt.g_txn_source_type
             AND mr.demand_source_header_id = p_batch_id
             AND mr.demand_source_line_id = p_material_detail_id
             AND (mr.subinventory_code IS NOT NULL)
             AND (p_revcontrol = 0 OR (p_revcontrol = 1 AND mr.revision IS NOT NULL))
             AND (p_eff_loccontrol = 0 OR (p_eff_loccontrol = 1 AND mr.locator_id IS NOT NULL))
             AND mr.organization_id   = loc.organization_id(+)
             AND mr.subinventory_code = loc.subinventory_code(+)
             AND mr.locator_id     = loc.inventory_location_id(+)
             AND (p_lotcontrol = 0 OR (p_lotcontrol = 1  AND mr.lot_number IS NOT NULL))
             AND NOT EXISTS (SELECT 1
                             FROM   mtl_material_transactions_temp
                             WHERE  reservation_id = mr.reservation_id)
      ORDER BY mr.requirement_date;

   EXCEPTION
    WHEN OTHERS THEN
      IF g_debug <= gme_debug.g_log_unexpected THEN
        gme_debug.put_line('When others exception in get Mtl detail Reservation');
      END IF;
      fnd_msg_pub.add_exc_msg('GME_MOBILE_RSRV','Get_Material_Detail_Reservations');
      x_return_status := FND_API.G_RET_STS_UNEXP_ERROR;
      x_error_msg     := fnd_message.get;

   END Get_Material_Dtl_Reservations;


 /*+========================================================================+
   | PROCEDURE NAME
   |  Get_Available_Qties
   |
   | USAGE
   |
   | ARGUMENTS
   |
   | RETURNS
   |
   | HISTORY
   |   Created  26-Apr-05 Eddie Oumerretane
   |
   +========================================================================+*/
  PROCEDURE Get_Available_Qties (p_organization_id     IN NUMBER,
                                 p_inventory_item_id   IN NUMBER,
                                 p_revision            IN VARCHAR2,
                                 p_subinventory_code   IN VARCHAR2,
                                 p_locator_id          IN NUMBER,
                                 p_lot_number          IN VARCHAR2,
                                 p_revision_control IN VARCHAR2,
                                 p_lot_control      IN VARCHAR2,
                                 p_tree_mode        IN VARCHAR2,
                                 x_att_qty    OUT NOCOPY NUMBER,
                                 x_sec_att_qty OUT NOCOPY NUMBER,
                                 x_atr_qty    OUT NOCOPY NUMBER,
                                 x_sec_atr_qty OUT NOCOPY NUMBER)
  IS

   l_is_revision_control BOOLEAN := FALSE;
   l_is_lot_control BOOLEAN := FALSE;

   l_return_status VARCHAR2(1);
   l_msg_count     NUMBER(10);
   l_msg_data      VARCHAR2(1000);
   l_qoh           NUMBER;
   l_rqoh          NUMBER;
   l_qr            NUMBER;
   l_qs            NUMBER;
   l_att           NUMBER;
   l_atr           NUMBER;
   l_sqoh           NUMBER;
   l_srqoh          NUMBER;
   l_sqr            NUMBER;
   l_sqs            NUMBER;
   l_satt           NUMBER;

   l_locator_id number;
   l_cost_group_id number;

BEGIN


-- Clearing the quantity cache
   inv_quantity_tree_pub.clear_quantity_cache;


   if upper(p_revision_control) = 'TRUE' then
      l_is_revision_control := TRUE;
   end if;
   if upper(p_lot_control) = 'TRUE' then
      l_is_lot_control := TRUE;
   end if;

   if p_locator_id <= 0 then
      l_locator_id := null;
   else
      l_locator_id := p_locator_id;
   end if;

   Inv_Quantity_Tree_Pub.Query_Quantities (
                p_api_version_number => 1.0,
                p_init_msg_lst       => fnd_api.g_false,
                x_return_status      => l_return_status,
                x_msg_count          => l_msg_count,
                x_msg_data           => l_msg_data,
                p_organization_id    => p_organization_id,
                p_inventory_item_id  => p_inventory_item_id,
                p_tree_mode          => p_tree_mode,
                p_is_revision_control => l_is_revision_control,
                p_is_lot_control     => l_is_lot_control,
                p_is_serial_control  => FALSE,
                p_grade_code         => NULL,
                p_revision           => p_revision,
                p_lot_number         => p_lot_number,
                p_subinventory_code  => p_subinventory_code,
                p_locator_id         => l_locator_id,
                p_cost_group_id      => NULL,
                x_qoh                => l_qoh,
                x_rqoh               => l_rqoh,
                x_qr                 => l_qr,
                x_qs                 => l_qs,
                x_att                => x_att_qty,
                x_atr                => x_atr_qty,
                x_sqoh               => l_sqoh,
                x_srqoh              => l_srqoh,
                x_sqr                => l_sqr,
                x_sqs                => l_sqs,
                x_satt               => x_sec_att_qty,
                x_satr               => x_sec_atr_qty);


  EXCEPTION
    WHEN OTHERS THEN
      IF g_debug <= gme_debug.g_log_unexpected THEN
        gme_debug.put_line('When others exception in get available qties');
      END IF;
      fnd_msg_pub.add_exc_msg('GME_MOBILE_RSRV','get_available_qties');

  END Get_Available_Qties;

 /*+========================================================================+
   | PROCEDURE NAME
   |  Update_Qty_Tree_For_Rsrv
   |
   | USAGE
   |
   | ARGUMENTS
   |
   | RETURNS
   |
   | HISTORY
   |   Created  26-Apr-05 Eddie Oumerretane
   |
   +========================================================================+*/
  PROCEDURE Update_Qty_Tree_For_Rsrv (p_organization_id     IN NUMBER,
                                  p_batch_id            IN NUMBER,
                                  p_material_detail_id  IN NUMBER,
                                  p_inventory_item_id   IN NUMBER,
                                  p_revision            IN VARCHAR2,
                                  p_subinventory_code   IN VARCHAR2,
                                  p_locator_id          IN NUMBER,
                                  p_lot_number          IN VARCHAR2,
                                  p_revision_control    IN VARCHAR2,
                                  p_lot_control         IN VARCHAR2,
                                  p_primary_qty         IN NUMBER,
                                  p_secondary_qty       IN NUMBER,
                                  x_tree_id             OUT NOCOPY NUMBER,
                                  x_atr                 OUT NOCOPY NUMBER,
                                  x_satr                OUT NOCOPY NUMBER,
                                  x_return_status       OUT NOCOPY VARCHAR2,
                                  x_error_msg           OUT NOCOPY VARCHAR2) IS

   l_is_revision_control BOOLEAN := FALSE;
   l_is_lot_control BOOLEAN := FALSE;

   l_primary_qty   NUMBER;
   l_secondary_qty NUMBER;
   l_msg_count     NUMBER(10);
   l_qoh           NUMBER;
   l_rqoh          NUMBER;
   l_qr            NUMBER;
   l_qs            NUMBER;
   l_att           NUMBER;
   l_sqoh          NUMBER;
   l_srqoh         NUMBER;
   l_sqr           NUMBER;
   l_sqs           NUMBER;
   l_satt           NUMBER;
   l_locator_id    NUMBER;
   CREATE_TREE_ERROR EXCEPTION;

  BEGIN


   IF (g_debug IS NOT NULL) THEN
     gme_debug.log_initialize ('MobileCreQtyTree');
   END IF;

   x_return_status := FND_API.G_RET_STS_SUCCESS;
   x_error_msg     := ' ';

   IF upper(p_revision_control) = 'TRUE' THEN
      l_is_revision_control := TRUE;
   END IF;

   IF upper(p_lot_control) = 'TRUE' THEN
      l_is_lot_control := TRUE;
   END IF;

   IF p_locator_id <= 0 THEN
      l_locator_id := null;
   ELSE
      l_locator_id := p_locator_id;
   END IF;

   l_primary_qty   := p_primary_qty * -1;
   l_secondary_qty := p_secondary_qty * -1;

   -- Clearing the quantity cache
   INV_Quantity_Tree_Pub.Clear_Quantity_Cache;


   INV_Quantity_Tree_Grp.Create_Tree
     (
        p_api_version_number      => 1.0
      , p_init_msg_lst            => 'T'
      , x_return_status           => x_return_status
      , x_msg_count               => l_msg_count
      , x_msg_data                => x_error_msg
      , p_organization_id         => p_organization_id
      , p_inventory_item_id       => p_inventory_item_id
      , p_tree_mode               => 1
      , p_is_revision_control     => l_is_revision_control
      , p_is_lot_control          => l_is_lot_control
      , p_is_serial_control       => FALSE
      , p_grade_code              => NULL
      , p_demand_source_type_id   => gme_common_pvt.g_txn_source_type
      , p_demand_source_header_id => p_batch_id
      , p_demand_source_line_id   => p_material_detail_id
      , p_demand_source_name      => NULL
      , p_lot_expiration_date     => SYSDATE
      , x_tree_id                 => x_tree_id
      );


     IF x_return_status <> FND_API.G_RET_STS_SUCCESS THEN
       RAISE CREATE_TREE_ERROR;
     END IF;

     IF (l_primary_qty <> 0) THEN

       INV_Quantity_Tree_Grp.Update_Quantities(
          p_api_version_number         => 1.0,
          p_init_msg_lst               => 'T',
          x_return_status              => x_return_status,
          x_msg_count                  => l_msg_count,
          x_msg_data                   => x_error_msg,
          p_tree_id                    => x_tree_id,
          p_revision                   => p_revision,
          p_lot_number                 => p_lot_number,
          p_subinventory_code          => p_subinventory_code,
          p_locator_id                 => p_locator_id,
          p_primary_quantity           => l_primary_qty,
          p_quantity_type              => 3, --inv_quantity_tree_pvt.g_qr_other_demand
          p_secondary_quantity         => l_secondary_qty,
          x_qoh                        => l_qoh,
          x_rqoh                       => l_rqoh,
          x_qr                         => l_qr,
          x_qs                         => l_qs,
          x_att                        => l_att,
          x_atr                        => x_atr,
          x_sqoh                       => l_sqoh,
          x_srqoh                      => l_srqoh,
          x_sqr                        => l_sqr,
          x_sqs                        => l_sqs,
          x_satt                       => l_satt,
          x_satr                       => x_satr,
          p_containerized              => 0,
          p_lpn_id                     => NULL);

     ELSE

       INV_Quantity_Tree_Grp.Query_Tree(
         p_api_version_number         => 1
       , p_init_msg_lst               => NULL
       , x_return_status              => x_return_status
       , x_msg_count                  => l_msg_count
       , x_msg_data                   => x_error_msg
       , p_tree_id                    => x_tree_id
       , p_revision                   => p_revision
       , p_lot_number                 => p_lot_number
       , p_subinventory_code          => p_subinventory_code
       , p_locator_id                 => p_locator_id
       , p_lpn_id                     => NULL
       , x_qoh                        => l_qoh
       , x_rqoh                       => l_rqoh
       , x_qr                         => l_qr
       , x_qs                         => l_qs
       , x_att                        => l_att
       , x_atr                        => x_atr
       , x_sqoh                       => l_sqoh
       , x_srqoh                      => l_srqoh
       , x_sqr                        => l_sqr
       , x_sqs                        => l_sqs
       , x_satt                       => l_satt
       , x_satr                       => x_satr
       );

     END IF;

     IF x_return_status <> FND_API.G_RET_STS_SUCCESS THEN
       RAISE CREATE_TREE_ERROR;
     END IF;

     INV_Quantity_Tree_Pvt.Prepare_Reservation_Quantities(
                                    x_return_status => x_return_status
                                  , p_tree_id       => x_tree_id);

     IF x_return_status <> FND_API.G_RET_STS_SUCCESS THEN
       fnd_message.set_name('INV', 'ERR_PREPARE_RSV_QTY');
       x_error_msg := fnd_message.get;
     END IF;


  EXCEPTION
    WHEN CREATE_TREE_ERROR THEN
      IF g_debug <= gme_debug.g_log_unexpected THEN
        gme_debug.put_line('Create Qty Tree exception in Update Qty Tree');
      END IF;

    WHEN OTHERS THEN
      IF g_debug <= gme_debug.g_log_unexpected THEN
        gme_debug.put_line('When others exception in Update Qty Tree');
      END IF;
      fnd_msg_pub.add_exc_msg('GME_MOBILE_RSRV','Update_Qty_Tree_For_Rsrv');
      x_return_status := FND_API.G_RET_STS_UNEXP_ERROR;
      x_error_msg     := fnd_message.get;

  END Update_Qty_Tree_For_Rsrv;

 /*+========================================================================+
   | PROCEDURE NAME
   |  Fetch_Atr_Qty
   |
   | USAGE
   |
   | ARGUMENTS
   |
   | RETURNS
   |
   | HISTORY
   |   Created  26-Apr-05 Eddie Oumerretane
   |
   +========================================================================+*/
  PROCEDURE Fetch_Atr_Qty (p_revision            IN VARCHAR2,
                           p_subinventory_code   IN VARCHAR2,
                           p_locator_id          IN NUMBER,
                           p_lot_number          IN VARCHAR2,
                           p_revision_control    IN VARCHAR2,
                           p_lot_control         IN VARCHAR2,
                           p_tree_id             IN NUMBER,
                           x_atr                 OUT NOCOPY NUMBER,
                           x_satr                OUT NOCOPY NUMBER,
                           x_return_status       OUT NOCOPY VARCHAR2,
                           x_error_msg           OUT NOCOPY VARCHAR2) IS

   l_msg_count     NUMBER(10);
   l_qoh           NUMBER;
   l_rqoh          NUMBER;
   l_qr            NUMBER;
   l_qs            NUMBER;
   l_att           NUMBER;
   l_sqoh          NUMBER;
   l_srqoh         NUMBER;
   l_sqr           NUMBER;
   l_sqs           NUMBER;
   l_satt           NUMBER;
   QUERY_TREE_ERROR EXCEPTION;

  BEGIN

    IF (g_debug IS NOT NULL) THEN
      gme_debug.log_initialize ('MobileFetchAtrQty');
    END IF;

    x_return_status := FND_API.G_RET_STS_SUCCESS;
    x_error_msg     := ' ';

    INV_Quantity_Tree_Grp.Query_Tree(
      p_api_version_number         => 1
    , p_init_msg_lst               => NULL
    , x_return_status              => x_return_status
    , x_msg_count                  => l_msg_count
    , x_msg_data                   => x_error_msg
    , p_tree_id                    => p_tree_id
    , p_revision                   => p_revision
    , p_lot_number                 => p_lot_number
    , p_subinventory_code          => p_subinventory_code
    , p_locator_id                 => p_locator_id
    , p_lpn_id                     => NULL
    , x_qoh                        => l_qoh
    , x_rqoh                       => l_rqoh
    , x_qr                         => l_qr
    , x_qs                         => l_qs
    , x_att                        => l_att
    , x_atr                        => x_atr
    , x_sqoh                       => l_sqoh
    , x_srqoh                      => l_srqoh
    , x_sqr                        => l_sqr
    , x_sqs                        => l_sqs
    , x_satt                       => l_satt
    , x_satr                       => x_satr
    );

    IF x_return_status <> FND_API.G_RET_STS_SUCCESS THEN
       RAISE QUERY_TREE_ERROR;
    END IF;

    EXCEPTION
    WHEN QUERY_TREE_ERROR THEN
      IF g_debug <= gme_debug.g_log_unexpected THEN
        gme_debug.put_line('Query Qty Tree exception in Fetch ATR Qty');
      END IF;

    WHEN OTHERS THEN
      IF g_debug <= gme_debug.g_log_unexpected THEN
        gme_debug.put_line('When others exception in Fetch_Atr_Qty');
      END IF;
      fnd_msg_pub.add_exc_msg('GME_MOBILE_RSRV','fetch_Atr_Qty');
      x_return_status := FND_API.G_RET_STS_UNEXP_ERROR;
      x_error_msg     := fnd_message.get;

  END Fetch_Atr_Qty;

 /*+========================================================================+
   | PROCEDURE NAME
   |  Check_UoM_Conv_Deviation
   |
   | USAGE
   |
   | ARGUMENTS
   |
   | RETURNS
   |
   | HISTORY
   |   Created  26-Apr-05 Eddie Oumerretane
   |
   +========================================================================+*/
  PROCEDURE Check_UoM_Conv_Deviation(
                                   p_organization_id     IN  NUMBER
                                 , p_inventory_item_id   IN  NUMBER
                                 , p_lot_number          IN  VARCHAR2
                                 , p_primary_quantity    IN  NUMBER
                                 , p_primary_uom_code    IN  VARCHAR2
                                 , p_secondary_quantity  IN  NUMBER
                                 , p_secondary_uom_code  IN  VARCHAR2
                                 , x_return_status       OUT NOCOPY VARCHAR2
                                 , x_error_msg           OUT NOCOPY VARCHAR2)
  IS

   l_is_valid      NUMBER(1);
   l_msg_index_out NUMBER;

  BEGIN

    x_return_status := FND_API.G_RET_STS_SUCCESS;
    x_error_msg     := '';

    -- Validate the quantities within deviation
    l_is_valid := INV_CONVERT.within_deviation(
                          p_organization_id => p_organization_id
                        , p_inventory_item_id  => p_inventory_item_id
                        , p_lot_number         => p_lot_number
                        , p_precision          => 5
                        , p_quantity           => ABS(p_primary_quantity)
                        , p_uom_code1          => p_primary_uom_code
                        , p_quantity2          => ABS(p_secondary_quantity)
                        , p_uom_code2           => p_secondary_uom_code);
     IF (l_is_valid = 0)
     THEN

       x_return_status := FND_API.G_RET_STS_ERROR;

       FND_MSG_PUB.Get(
                        p_msg_index     => 1,
                        p_data          => x_error_msg,
                        p_encoded       => FND_API.G_FALSE,
                        p_msg_index_out => l_msg_index_out);
     END IF;

  END Check_UoM_Conv_Deviation;

 /*+========================================================================+
   | PROCEDURE NAME
   |  Get_Stacked_Message
   |
   | USAGE
   |
   | ARGUMENTS
   |
   | RETURNS
   |
   | HISTORY
   |   Created  26-Apr-05 Eddie Oumerretane
   |
   +========================================================================+*/
  PROCEDURE Get_Stacked_Messages(x_message OUT NOCOPY VARCHAR2)
  IS
     l_message VARCHAR2(2000);
     l_msg_count NUMBER;
  BEGIN

   fnd_msg_pub.Count_And_Get
     (p_encoded => FND_API.g_false,
      p_count => l_msg_count,
      p_data => l_message
      );


   fnd_msg_pub.delete_msg;

   x_message := l_message;

  EXCEPTION
    WHEN OTHERS THEN
      NULL;

  END Get_Stacked_Messages;

 /*+========================================================================+
   | PROCEDURE NAME
   |  Create_Reservation
   |
   | USAGE
   |
   | ARGUMENTS
   |
   | RETURNS
   |
   | HISTORY
   |   Created  26-Apr-05 Eddie Oumerretane
   | 23-Jun-06     Shrikant Nene     Bug 5263908                               |
   |
   +========================================================================+*/
  PROCEDURE Create_Reservation(p_organization_id        IN NUMBER,
                               p_batch_id               IN NUMBER,
                               p_material_detail_id     IN NUMBER,
                               p_item_id                IN NUMBER,
                               p_revision               IN VARCHAR2,
                               p_subinventory_code      IN VARCHAR2,
                               p_locator_id             IN NUMBER,
                               p_lot_number             IN VARCHAR2,
                               p_reserved_qty           IN NUMBER,
                               p_reserved_uom_code      IN VARCHAR2,
                               p_sec_reserved_qty       IN NUMBER,
                               p_sec_reserved_uom_code  IN VARCHAR2,
                               p_requirement_date       IN DATE,
                               x_return_status          OUT NOCOPY VARCHAR2,
                               x_error_msg              OUT NOCOPY VARCHAR2)
  IS
    l_matl_dtl_rec gme_material_details%ROWTYPE;
    l_msg_count   NUMBER;
  BEGIN

    x_return_status := FND_API.G_RET_STS_SUCCESS;
    x_error_msg     := ' ';

    IF (g_debug IS NOT NULL) THEN
       gme_debug.log_initialize ('MobileCreRsrv');
    END IF;

    l_matl_dtl_rec.material_detail_id        := p_material_detail_id;
    IF NOT gme_material_details_dbl.fetch_row (l_matl_dtl_rec
                                              ,l_matl_dtl_rec) THEN
       RAISE fnd_api.g_exc_error;
    END IF;

    l_matl_dtl_rec.material_requirement_date := p_requirement_date;
    l_matl_dtl_rec.organization_id           := p_organization_id;
    l_matl_dtl_rec.inventory_item_id         := p_item_id;
    l_matl_dtl_rec.batch_id                  := p_batch_id;
    l_matl_dtl_rec.revision                  := p_revision;

    GME_RESERVATIONS_PVT.Create_Material_Reservation(
                             p_matl_dtl_rec  => l_matl_dtl_rec,
                             p_resv_qty      => p_reserved_qty,
                             p_sec_resv_qty  => p_sec_reserved_qty,
                             p_resv_um       => p_reserved_uom_code,
                             p_subinventory  => p_subinventory_code,
                             p_locator_id    => p_locator_id,
                             p_lot_number    => p_lot_number,
                             x_return_status => x_return_status);

    IF x_return_status <> FND_API.G_RET_STS_SUCCESS THEN
      Get_Stacked_Messages(x_message => x_error_msg);
    /* Bug 5438355: Added success message. */
    ELSE
      gme_common_pvt.log_message('GME_RESERVATION_CREATED');
      gme_common_pvt.count_and_get (x_count        => l_msg_count
                                   ,p_encoded      => fnd_api.g_false
                                   ,x_data         => x_error_msg);
      COMMIT;
    END IF;

  EXCEPTION
    WHEN FND_API.G_EXC_ERROR THEN
       x_return_status := fnd_api.g_ret_sts_error;
       gme_common_pvt.count_and_get (x_count        => l_msg_count
                                    ,p_encoded      => fnd_api.g_false
                                    ,x_data         => x_error_msg);
    WHEN OTHERS THEN
      IF g_debug <= gme_debug.g_log_unexpected THEN
        gme_debug.put_line('When others exception in Create Reservation');
      END IF;
      fnd_msg_pub.add_exc_msg('GME_MOBILE_RSRV','create_reservation');
      x_return_status := FND_API.G_RET_STS_UNEXP_ERROR;
      x_error_msg     := fnd_message.get;

  END Create_Reservation;

 /*+========================================================================+
   | PROCEDURE NAME
   |  Update_Reservation
   |
   | USAGE
   |
   | ARGUMENTS
   |
   | RETURNS
   |
   | HISTORY
   |   Created  26-Apr-05 Eddie Oumerretane
   |
   +========================================================================+*/
  PROCEDURE Update_Reservation(p_reservation_id         IN NUMBER,
                               p_revision               IN VARCHAR2,
                               p_subinventory_code      IN VARCHAR2,
                               p_locator_id             IN NUMBER,
                               p_lot_number             IN VARCHAR2,
                               p_reserved_qty           IN NUMBER,
                               p_reserved_uom_code      IN VARCHAR2,
                               p_sec_reserved_qty       IN NUMBER,
                               p_requirement_date       IN DATE,
                               x_return_status          OUT NOCOPY VARCHAR2,
                               x_error_msg              OUT NOCOPY VARCHAR2)

  IS
  BEGIN

    x_return_status := FND_API.G_RET_STS_SUCCESS;
    x_error_msg     := ' ';

    IF (g_debug IS NOT NULL) THEN
       gme_debug.log_initialize ('MobileUpdRsrv');
       gme_debug.put_line('Reservation id    = '||p_reservation_id);
       gme_debug.put_line('Revision          = '||p_revision);
       gme_debug.put_line('Sub inventory     = '||p_subinventory_code);
       gme_debug.put_line('Locator Id        = '||p_locator_id);
       gme_debug.put_line('Lot               = '||p_lot_number);
       gme_debug.put_line('Reserved Qty      = '||p_reserved_qty);
       gme_debug.put_line('Sec Reserved Qty  = '||p_sec_reserved_qty);
       gme_debug.put_line('Reserved UOM      = '||p_reserved_uom_code);
       gme_debug.put_line('Date              = '||p_requirement_date);
    END IF;

    -- Clearing the quantity cache
    INV_Quantity_Tree_Pub.Clear_Quantity_Cache;

    GME_RESERVATIONS_PVT.update_reservation(
                               p_reservation_id => p_reservation_id,
                               p_revision       => p_revision,
                               p_subinventory   => p_subinventory_code,
                               p_locator_id     => p_locator_id,
                               p_lot_number     => p_lot_number,
                               p_new_qty        => p_reserved_qty,
                               p_new_sec_qty    => p_sec_reserved_qty,
                               p_new_uom        => p_reserved_uom_code,
                               p_new_date       => p_requirement_date,
                               x_return_status  => x_return_status);

    IF x_return_status <> FND_API.G_RET_STS_SUCCESS THEN
      Get_Stacked_Messages(x_message => x_error_msg);
    /* Bug 5438355: Added success message. */
    ELSE
      gme_common_pvt.log_message('GME_RESERVATION_UPDATED');
      Get_Stacked_Messages(x_message => x_error_msg);
      COMMIT;
    END IF;

  EXCEPTION
    WHEN OTHERS THEN
      IF g_debug <= gme_debug.g_log_unexpected THEN
        gme_debug.put_line('When others exception in Update Reservation');
      END IF;
      fnd_msg_pub.add_exc_msg('GME_MOBILE_RSRV','update_reservation');
      x_return_status := FND_API.G_RET_STS_UNEXP_ERROR;
      x_error_msg     := fnd_message.get;

  END Update_Reservation;


END GME_MOBILE_RSRV;
/

--show errors;
COMMIT;
EXIT;
