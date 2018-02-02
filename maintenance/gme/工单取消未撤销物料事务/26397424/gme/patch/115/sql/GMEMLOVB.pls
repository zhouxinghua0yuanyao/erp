REM dbdrv: sql ~PROD ~PATH ~FILE none none none package &phase=plb \
REM dbdrv: checkfile(120.33.12010000.19=120.41.12020000.11)(120.30.12000000.13=120.33.12010000.10)(120.30.12000000.12=120.41):~PROD:~PATH:~FILE
SET VERIFY OFF;
WHENEVER SQLERROR EXIT FAILURE ROLLBACK;
WHENEVER OSERROR EXIT FAILURE ROLLBACK;
create or replace PACKAGE BODY GME_MOBILE_LOVS AS
/*  $Header: GMEMLOVB.pls 120.41.12020000.11 2017/03/24 06:23:12 shalchen ship $     */
/*===========================================================================+
 |      Copyright (c) 2005, 2017 Oracle Corporation, Redwood Shores, CA, USA       |
 |                         All rights reserved.                              |
 |===========================================================================|
 |                                                                           |
 | PL/SQL Package to support the (Java) GME Mobile Application.              |
 | Contains PL/SQL cursors used to fetch data in the mobile LOVs.            |
 |                                                                           |
 +===========================================================================+
 |  HISTORY                                                                  |
 |                                                                           |
 | Date          Who               What                                      |
 | ====          ===               ====                                      |
 | 26-Apr-05     Eddie Oumerretane First version                             |
 | 31-May-06     Eddie Oumerretane Bug 5236930 Removed character '%' in where|
 |                                 clause of all LOVs			     |
 | 09-Jun-06     Namit Singhi. Bug#5236906. Defined 2 new procs -            |
 |                 Step_Material_Lot_Line_LoV and Material_Lot_LoV. Show only|
 |                 lot controlled items for Create/Pending Lots.             |
 | 21-Jun-06     Shrikant Nene Bug#5263908. Defined 1 new procs -            |
 |                 Revision_LoV.  Also added the revision field in 3 procs   |
 |                 Subinventory_LoV_Dispense                                 |
 |                 Locator_LoV_Dispense                                      |
 |                 Lot_LoV_Dispense                                          |
 | 21-Aug-06     Shrikant Nene Bug#5263908. Defined 1 new procs -            |
 |                 Pending_Parent_Lot_LoV                                    |
 | 15-Sep-06     SivakumarG  Bug#5261131                                      |
 |                 Added procedure Lot_OnHand_Exp_LoV to filter based on     |
 |                 expiration date                                           |                                                                          |
 |29-JUL-08      Swapna K Bug#7253370                                        |
 |                  Added NVL for the secondary_reservation_quantity in the  |
 |                  Rsrv_Lov procedure.
 |26-OCT-09      Swapna K Bug#8937312
 |                 Added parameter,p_transaction_type to the
 |                 procedure,Lot_OnHand_Exp_LoV and changed the where clause to|
 |                consider the lot status                                     |
 |16-Apr-10      APMISHRA                                                     |
 |                 Modified the cussors in the procedures rsrv_lov and        |
 |                 Dispense_LoV to select LPN number                          |

 |17-Jun-11      Apeksha Mishra   Bug 12562057                                |
 |                 Remove explicit parameter names as it does                 |
 |                 not compile on versions below 11g                          |
 |18-JUL-2014    Shaliu Chen       ER 19161894
 |                 Modify create_batch to invoke requisition creation program |
 |                if batch include OSP step                                   |
 |7-JUL-2015     Jagadiswar Devarla Bug#19619931                              |
 |                 Added lpn id as parameter to Lot_OnHand_Exp_LoV.           |
 |                 Created new procedure Lpn_LoV to fetch lpn LoV.            |     
 |10-JUL-2015    Archana Mundhe Bug 21364641 Modified procedure Dispense_LoV  |
 |10-SEP-2015    Jagadiswar Devarla Bug#21786445                              |
 |                 Created new procedure Return_Lpn_LoV to fetch lpn LoV.     | 
 +===========================================================================*/

  --- For GTIN support
  g_gtin_cross_ref_type VARCHAR2(25) := fnd_profile.value('INV:GTIN_CROSS_REFERENCE_TYPE');
  g_gtin_code_length NUMBER := 14;

  g_debug      VARCHAR2 (5) := fnd_profile.VALUE ('AFLOG_LEVEL');

 /* Bug#5663458 Begin
  * Created the following procedure. This procedure is to get all reservations
  * from material line. Used for Use Rsrv field in mobile
  */
  PROCEDURE Rsrv_LoV
  ( x_batch_cursor        OUT NOCOPY t_genref
  ,  p_org_id             IN  NUMBER
  ,  p_batch_id           IN  NUMBER
  ,  p_material_detail_id IN  NUMBER
  )
  IS
    l_date_format VARCHAR2(100);
  BEGIN
    FND_PROFILE.GET('MWA_DATE_FORMAT_MASK',l_date_format);

    IF l_date_format IS NULL THEN
      FND_PROFILE.GET('ICX_DATE_FORMAT_MASK',l_date_format);
    END IF;

    --
    -- bug 9483781
    -- Modified the cursor definition to also
    -- select the LPN number to be displayed in
    -- in the LOV
    --
    OPEN x_batch_cursor FOR
     SELECT NVL(mr.revision,' '),
            NVL(mr.lot_number,' '),
            NVL(mr.subinventory_code,' '),
            NVL(lo.concatenated_segments,' ') locator,
            mr.reservation_quantity,
            mr.reservation_uom_code,
            TO_CHAR(mr.requirement_date,l_date_format),
            NVL(RTRIM(lpn.LICENSE_PLATE_NUMBER),' '),  /*Bug9483781*/
            nvl(mr.secondary_reservation_quantity,0), /*Bug7041074*/
            mr.reservation_id,
            lo.inventory_location_id,
            NVL(mr.LPN_ID,-1) --Bug#19619931
     FROM  mtl_reservations mr,
           wms_item_locations_kfv lo,
           wms_license_plate_numbers lpn
     WHERE lo.inventory_location_id(+) = mr.locator_id
       AND mr.organization_id = p_org_id
       AND mr.demand_source_type_id = 5
       AND mr.demand_source_header_id = p_batch_id
       AND mr.demand_source_line_id = p_material_detail_id
       AND LPN.lpn_id(+) = mr.lpn_id
       AND NOT EXISTS (SELECT 1
                        FROM  mtl_material_transactions_temp
                        WHERE  reservation_id = mr.reservation_id)
     ORDER BY mr.requirement_date, mr.reservation_id;

  END Rsrv_LoV;

 /*
  * Created the following procedure. This procedure is to get all dispensing records
  * from material line. Used for Use Rsrv field in mobile
  */
   PROCEDURE Dispense_LoV
    ( x_batch_cursor        OUT NOCOPY t_genref
    )IS
    l_date_format VARCHAR2(100);
   BEGIN
    FND_PROFILE.GET('MWA_DATE_FORMAT_MASK',l_date_format);

    IF l_date_format IS NULL THEN
      FND_PROFILE.GET('ICX_DATE_FORMAT_MASK',l_date_format);
    END IF;

    --
    -- bug 9483781
    -- Modified the cursor definition to select
    -- a NULL LPN number. This is done to keep
    -- this and the rsrv_lov in sync
    -- 
    -- Bug 21364641 Added NVL to secondary_dispensed_qty
    OPEN x_batch_cursor FOR
     SELECT NVL(revision,' '),
            NVL(d.lot_number,' '),
            NVL(d.subinventory_code,' '),
            NVL(lo.concatenated_segments,' ') locator,
            d.dispensed_qty,
            d.dispense_uom,
            TO_CHAR(SYSDATE,l_date_format),
            NULL, /*Bug9483781*/
            NVL(d.secondary_dispensed_qty,0),
            d.dispense_id,
            lo.inventory_location_id
     FROM  gme_material_dispensing_gtmp d,
           wms_item_locations_kfv lo
     WHERE lo.inventory_location_id(+) = d.locator_id
       AND lo.subinventory_code(+) = d.subinventory_code
     ORDER BY d.dispense_id;

   END Dispense_LoV;

 /*+========================================================================+
   | PROCEDURE NAME
   |   Batch_LoV
   |
   | USAGE
   |
   | ARGUMENTS
   |   p_org_id
   |   p_statuses
   |   p_batch_no
   |
   | RETURNS
   |   REF cursor x_batch_cursor
   |
   | HISTORY
   |   Created  26-Apr-05 Eddie Oumerretane
   +========================================================================+*/
  PROCEDURE Batch_LoV
  ( x_batch_cursor     OUT NOCOPY t_genref
  ,  p_org_id         IN  NUMBER
  ,  p_statuses       IN  VARCHAR2
  ,  p_batch_no       IN  VARCHAR2
  ) IS

  BEGIN
    IF p_statuses= 'PENDING_WIP'
    THEN
      OPEN x_batch_cursor FOR
        SELECT  batch_no
               ,meaning
               ,batch_status
               ,batch_id
               ,NVL(formula_id,0)
               ,NVL(routing_id,0)
        FROM   gme_batch_header b,
               gem_lookups lkup
        WHERE  organization_id = p_org_id
        AND    batch_type= 0
        AND    batch_status in (1,2)
        --- Bug 5236930 AND    batch_no LIKE LTRIM(RTRIM('%'||p_batch_no||'%'))
        AND    batch_no LIKE p_batch_no
        AND    delete_mark = 0
        AND    lkup.lookup_type = 'GME_BATCH_STATUS'
        AND lkup.lookup_code = batch_status
        AND sysdate BETWEEN NVL(lkup.start_date_active, SYSDATE)
        AND NVL(lkup.end_date_active, sysdate)
        AND lkup.enabled_flag = 'Y'
        AND NVL(b.migrated_batch_ind, 'N') <> 'Y'
        AND NVL(b.migrated_batch_ind, 'N') <> 'Y'
        AND NVL(b.update_inventory_ind,'N') = 'Y' --Bug#5763793
        ORDER BY batch_no DESC;

    ELSIF p_statuses = 'WIP' THEN

      OPEN x_batch_cursor FOR
        SELECT  batch_no
               ,meaning
               ,batch_status
               ,batch_id
               ,NVL(formula_id,0)
               ,NVL(routing_id,0)
        FROM   gme_batch_header b,
               gem_lookups lkup
        WHERE  organization_id = p_org_id
        AND    batch_type= 0
        AND    batch_status = 2
        --- Bug 5236930 AND    batch_no LIKE LTRIM(RTRIM('%'||p_batch_no||'%'))
        AND    batch_no LIKE p_batch_no
        AND    delete_mark = 0
        AND    lkup.lookup_type = 'GME_BATCH_STATUS'
        AND lkup.lookup_code = batch_status
        AND sysdate BETWEEN NVL(lkup.start_date_active, SYSDATE)
        AND NVL(lkup.end_date_active, sysdate)
        AND lkup.enabled_flag = 'Y'
        AND NVL(b.migrated_batch_ind, 'N') <> 'Y'
        AND NVL(b.update_inventory_ind,'N') = 'Y' --Bug#5763793
        ORDER BY batch_no DESC;

    ELSIF p_statuses = 'WIP_COMPLETED' THEN

      OPEN x_batch_cursor FOR
        SELECT  batch_no
               ,meaning
               ,batch_status
               ,batch_id
               ,NVL(formula_id,0)
               ,NVL(routing_id,0)
        FROM   gme_batch_header b,
               gem_lookups lkup
        WHERE  organization_id = p_org_id
        AND    batch_type= 0
        AND    batch_status in (2,3)
        --- Bug 5236930 AND    batch_no LIKE LTRIM(RTRIM('%'||p_batch_no||'%'))
        AND    batch_no LIKE p_batch_no
        AND    delete_mark = 0
        AND    lkup.lookup_type = 'GME_BATCH_STATUS'
        AND lkup.lookup_code = batch_status
        AND sysdate BETWEEN NVL(lkup.start_date_active, SYSDATE)
        AND NVL(lkup.end_date_active, sysdate)
        AND lkup.enabled_flag = 'Y'
        AND NVL(b.migrated_batch_ind, 'N') <> 'Y'
        AND NVL(b.update_inventory_ind,'N') = 'Y' --Bug#5763793
        ORDER BY batch_no DESC;

    ELSIF p_statuses = 'WIP_COMPLETED_LPN' THEN

      OPEN x_batch_cursor FOR
        SELECT  batch_no
               ,meaning
               ,batch_status
               ,batch_id
               ,NVL(formula_id,0)
               ,NVL(routing_id,0)
        FROM   gme_batch_header b,
               gem_lookups lkup
        WHERE  organization_id = p_org_id
        AND    batch_type= 0
        AND    batch_status in (2,3)
        --- Bug 5236930 AND    batch_no LIKE LTRIM(RTRIM('%'||p_batch_no||'%'))
        AND    batch_no LIKE p_batch_no
        AND    delete_mark = 0
        AND    parentline_id IS NULL
        AND    lkup.lookup_type = 'GME_BATCH_STATUS'
        AND lkup.lookup_code = batch_status
        AND sysdate BETWEEN NVL(lkup.start_date_active, SYSDATE)
        AND NVL(lkup.end_date_active, sysdate)
        AND lkup.enabled_flag = 'Y'
        AND NVL(b.migrated_batch_ind, 'N') <> 'Y'
        AND NVL(b.update_inventory_ind,'N') = 'Y' --Bug#5763793
        ORDER BY batch_no DESC;

    ELSIF p_statuses = 'UPDATE_RSRC_USAGE' THEN

      OPEN x_batch_cursor FOR
        SELECT  batch_no
               ,meaning
               ,batch_status
               ,batch_id
               ,NVL(formula_id,0)
               ,NVL(routing_id,0)
        FROM   gme_batch_header b,
               gem_lookups lkup
        WHERE  organization_id = p_org_id
        AND    batch_type= 0
        AND    batch_status in (2,3)
        --- Bug 5236930 AND    batch_no LIKE LTRIM(RTRIM('%'||p_batch_no||'%'))
        AND    batch_no LIKE p_batch_no
        AND    delete_mark = 0
        AND    routing_id IS NOT NULL
        AND    routing_id > 0
        AND    automatic_step_calculation = 0
        AND    automatic_step_calculation IN (0,1)
        AND    lkup.lookup_type = 'GME_BATCH_STATUS'
        AND lkup.lookup_code = batch_status
        AND sysdate BETWEEN NVL(lkup.start_date_active, SYSDATE)
        AND NVL(lkup.end_date_active, sysdate)
        AND lkup.enabled_flag = 'Y'
        AND NVL(b.migrated_batch_ind, 'N') <> 'Y'
        AND NVL(b.update_inventory_ind,'N') = 'Y' --Bug#5763793
        AND    EXISTS
               ( SELECT 1
                 FROM gme_batch_steps
                 WHERE batch_id = b.batch_id
                 AND   step_status in (2,3)
               )
       UNION
        SELECT  batch_no
               ,meaning
               ,batch_status
               ,batch_id
               ,NVL(formula_id,0)
               ,NVL(routing_id,0)
        FROM   gme_batch_header h,
               gem_lookups lkup
        WHERE  organization_id = p_org_id
        AND    batch_type= 0
        AND    batch_status = 3
        --- Bug 5236930 AND    batch_no LIKE LTRIM(RTRIM('%'||p_batch_no||'%'))
        AND    batch_no LIKE p_batch_no
        AND    routing_id IS NOT NULL
        AND    routing_id > 0
        AND    automatic_step_calculation = 1
        AND    lkup.lookup_type = 'BATCH_STATUS'
        AND lkup.lookup_code = batch_status
        AND sysdate BETWEEN NVL(lkup.start_date_active, SYSDATE)
        AND NVL(lkup.end_date_active, sysdate)
        AND lkup.enabled_flag = 'Y'
        AND NVL(h.update_inventory_ind,'N') = 'Y' --Bug#5763793
        AND    EXISTS
               ( SELECT 1
                 FROM gme_batch_steps
                 WHERE batch_id = h.batch_id
                 AND   step_status = 3
               )
        ORDER BY batch_no DESC;

    ELSIF p_statuses= 'PENDING_WIP_WITH_ROUTE'
    THEN
      OPEN x_batch_cursor FOR
        SELECT  batch_no
               ,meaning
               ,batch_status
               ,batch_id
               ,NVL(formula_id,0)
               ,NVL(routing_id,0)
        FROM   gme_batch_header b,
               gem_lookups lkup
        WHERE  organization_id = p_org_id
        AND    batch_type= 0
        AND    batch_status in (1,2)
        --- Bug 5236930 AND    batch_no LIKE LTRIM(RTRIM('%'||p_batch_no||'%'))
        AND    batch_no LIKE p_batch_no
        AND    delete_mark = 0
        AND    routing_id IS NOT NULL
        AND    lkup.lookup_type = 'GME_BATCH_STATUS'
        AND lkup.lookup_code = batch_status
        AND sysdate BETWEEN NVL(lkup.start_date_active, SYSDATE)
        AND NVL(lkup.end_date_active, sysdate)
        AND lkup.enabled_flag = 'Y'
        AND NVL(b.migrated_batch_ind, 'N') <> 'Y'
        AND NVL(b.update_inventory_ind,'N') = 'Y' --Bug#5763793
        ORDER BY batch_no DESC;

    END IF;

  EXCEPTION
    WHEN FND_API.G_EXC_UNEXPECTED_ERROR
    THEN
      NULL;

  END Batch_Lov;

 /*+========================================================================+
   | PROCEDURE NAME
   |   Ingredient_Line_Rsrv_LoV
   |
   | USAGE
   |
   | ARGUMENTS
   |   p_batch_id
   |   p_line_no
   |   p_item_no
   |
   | RETURNS
   |   REF cursor x_line_cursor
   |
   | HISTORY
   |   Created  26-Apr-05 Eddie Oumerretane
   |
   +========================================================================+*/
  PROCEDURE Ingredient_Line_Rsrv_LoV
  (  x_line_cursor     OUT NOCOPY t_genref
  ,  p_batch_id        IN  NUMBER
  ,  p_line_no         IN  VARCHAR2
  ,  p_item_no         IN  VARCHAR2
  )
  IS
  BEGIN

    OPEN x_line_cursor FOR
      SELECT d.line_no,
             i.concatenated_segments,
             i.inventory_item_id,
             d.material_detail_id
      FROM gme_material_details d,
           mtl_system_items_kfv i
      WHERE
          d.batch_id  = p_batch_id
      AND d.line_type = -1
      AND d.phantom_type = 0
      AND i.concatenated_segments = NVL(p_item_no, i.concatenated_segments)
      AND i.inventory_item_id = d.inventory_item_id
      AND i.organization_id   = d.organization_id
      AND i.reservable_type = 1
      AND d.line_no LIKE (p_line_no)
      AND NOT EXISTS
          (SELECT 1
           FROM gme_batch_step_items i2,
                gme_batch_steps s
           WHERE i2.batch_id = d.batch_id
           AND    s.batch_id = d.batch_id
           AND   i2.material_detail_id = d.material_detail_id
           AND   i2.batchstep_id       = s.batchstep_id
           AND s.step_status IN (3,4,5))
      ORDER BY d.line_no;

  END Ingredient_Line_Rsrv_LoV;

 /*+========================================================================+
   | PROCEDURE NAME
   |   Step_Ingredient_Line_LoV
   |
   | USAGE
   |
   | ARGUMENTS
   |   p_batch_id
   |   p_line_no
   |   p_step_no
   |
   | RETURNS
   |   REF cursor x_line_cursor
   |
   | HISTORY
   |   Created  26-Apr-05 Eddie Oumerretane
   |
   +========================================================================+*/
  PROCEDURE Step_Ingredient_Line_LoV
  (  x_line_cursor     OUT NOCOPY t_genref
  ,  p_batch_id        IN  NUMBER
  ,  p_line_no         IN  VARCHAR2
  ,  p_step_no         IN  VARCHAR2
  ) IS
  BEGIN
    Step_Material_Line_LoV(x_line_cursor => x_line_cursor
                           , p_batch_id  => p_batch_id
                           , p_line_no   => p_line_no
                           , p_step_no   => p_step_no
                           , p_line_type => -1);


  END Step_Ingredient_Line_LoV;


   PROCEDURE Line_Step_LoV
  (  x_item_cursor     OUT NOCOPY t_genref
  ,  p_batch_id        IN  NUMBER
  ,  p_line_no         IN  VARCHAR2
  ,  p_item_no         IN  VARCHAR2
  ,  p_step_no         IN  VARCHAR2
  ,  p_line_type       IN  NUMBER
  )
  IS
    l_cross_ref varchar2(204);
  BEGIN

    IF (g_debug IS NOT NULL) THEN
       gme_debug.log_initialize ('MobileLineStepLoV');
    END IF;

    l_cross_ref := lpad(Rtrim(p_item_no, '%'), g_gtin_code_length,'00000000000000');


    gme_debug.put_line('Cross ref type  =  '||g_gtin_cross_ref_type);
    gme_debug.put_line('Cross ref       =  '||l_cross_ref);

    IF p_step_no IS NULL THEN

      OPEN x_item_cursor FOR

      SELECT DISTINCT
       d.line_no,
       i.concatenated_segments,
       i.description,
       i.inventory_item_id,
       d.material_detail_id,
       NVL(i.lot_control_code, 1),
       NVL(i.location_control_code, 1),
       'N', --- gtin_entered_ind
       NULL, --- GTIN UOM code
       NVL(i.restrict_locators_code, 2),
       NVL(i.grade_control_flag, 'N'),
       NVL(i.lot_status_enabled, 'N'),
       NVL(i.lot_divisible_flag, 'N')
      FROM gme_material_details d,
           mtl_system_items_kfv i
      WHERE d.batch_id                    = p_batch_id
      AND d.inventory_item_id             = i.inventory_item_id
      AND d.organization_id               = i.organization_id
      AND d.line_type                     = p_line_type
      AND i.mtl_transactions_enabled_flag = 'Y'
      AND d.line_no LIKE (p_line_no)
      AND i.concatenated_segments = NVL(p_item_no,i.concatenated_segments)

      --- For GTIN support
      UNION

      SELECT DISTINCT
       d.line_no,
       i.concatenated_segments,
       i.description,
       i.inventory_item_id,
       d.material_detail_id,
       NVL(i.lot_control_code, 1),
       NVL(i.location_control_code, 1),
       'Y', --- gtin_entered_ind
       NVL(uom_code, ' '), --- GTIN UOM code
       NVL(i.restrict_locators_code, 2),
       NVL(i.grade_control_flag, 'N'),
       NVL(i.lot_status_enabled, 'N'),
       NVL(i.lot_divisible_flag, 'N')
      FROM
           gme_material_details d,
           mtl_cross_references mcr,
           mtl_system_items_kfv i
      WHERE d.batch_id                    = p_batch_id
      AND d.inventory_item_id             = i.inventory_item_id
      AND d.organization_id               = i.organization_id
      AND d.line_type                     = p_line_type
      AND i.inventory_item_id             = mcr.inventory_item_id
      AND i.mtl_transactions_enabled_flag = 'Y'
      AND d.line_no LIKE (p_line_no)
      AND mcr.cross_reference_type     = g_gtin_cross_ref_type
      AND mcr.cross_reference          LIKE l_cross_ref
      AND (mcr.organization_id         = i.organization_id OR
           mcr.org_independent_flag = 'Y')
      ORDER BY line_no;

    ELSE

      OPEN x_item_cursor FOR

      SELECT DISTINCT
       d.line_no,
       i.concatenated_segments,
       i.description,
       i.inventory_item_id,
       d.material_detail_id,
       NVL(i.lot_control_code, 1),
       NVL(i.location_control_code, 1),
       'N', --- gtin_entered_ind
       NULL, --- GTIN UOM code
       NVL(i.restrict_locators_code, 2),
       NVL(i.grade_control_flag, 'N'),
       NVL(i.lot_status_enabled, 'N'),
       NVL(i.lot_divisible_flag, 'N')
      FROM gme_material_details d,
           gme_batch_step_items si,
           gme_batch_steps      s,
           mtl_system_items_kfv i
      WHERE d.batch_id                    = p_batch_id
      AND d.batch_id                      = si.batch_id
      AND si.material_detail_id           = d.material_detail_id
      AND s.batchstep_no                  = p_step_no
      AND si.batchstep_id                 = s.batchstep_id
      AND d.inventory_item_id             = i.inventory_item_id
      AND d.organization_id               = i.organization_id
      AND d.line_type                     = p_line_type
      AND i.mtl_transactions_enabled_flag = 'Y'
      AND d.line_no LIKE (p_line_no)
      AND i.concatenated_segments = NVL(p_item_no,i.concatenated_segments)

      --- For GTIN support
      UNION

      SELECT DISTINCT
       d.line_no,
       i.concatenated_segments,
       i.description,
       i.inventory_item_id,
       d.material_detail_id,
       NVL(i.lot_control_code, 1),
       NVL(i.location_control_code, 1),
       'Y', --- gtin_entered_ind
       NVL(uom_code, ' '), --- GTIN UOM code
       NVL(i.restrict_locators_code, 2),
       NVL(i.grade_control_flag, 'N'),
       NVL(i.lot_status_enabled, 'N'),
       NVL(i.lot_divisible_flag, 'N')
      FROM
           gme_material_details d,
           mtl_cross_references mcr,
           gme_batch_step_items si,
           gme_batch_steps      s,
           mtl_system_items_kfv i
      WHERE d.batch_id                    = p_batch_id
      AND d.batch_id                      = si.batch_id
      AND si.material_detail_id           = d.material_detail_id
      AND s.batchstep_no                  = p_step_no
      AND si.batchstep_id                 = s.batchstep_id
      AND d.inventory_item_id             = i.inventory_item_id
      AND d.organization_id               = i.organization_id
      AND d.line_type                     = p_line_type
      AND i.inventory_item_id             = mcr.inventory_item_id
      AND i.mtl_transactions_enabled_flag = 'Y'
      AND d.line_no LIKE (p_line_no)
      AND mcr.cross_reference_type     = g_gtin_cross_ref_type
      AND mcr.cross_reference          LIKE l_cross_ref
      AND (mcr.organization_id         = i.organization_id OR mcr.org_independent_flag = 'Y')
      ORDER BY line_no;
    END IF;

  EXCEPTION
    WHEN FND_API.G_EXC_UNEXPECTED_ERROR
    THEN
      NULL;
  END Line_Step_LoV;

 /*+========================================================================+
   | PROCEDURE NAME
   |   Step_Material_Line_LoV
   |
   | USAGE
   |
   | ARGUMENTS
   |   p_batch_id
   |   p_line_no
   |   p_item_no
   |   p_step_no
   |   p_line_type
   |
   | RETURNS
   |   REF cursor x_line_cursor
   |
   | HISTORY
   |   Created  26-Apr-05 Eddie Oumerretane
   |
   +========================================================================+*/
  PROCEDURE Step_Material_Line_LoV
  (  x_line_cursor     OUT NOCOPY t_genref
  ,  p_batch_id        IN  NUMBER
  ,  p_line_no         IN  VARCHAR2
  ,  p_step_no         IN  VARCHAR2
  ,  p_line_type       IN  NUMBER
  )
  IS
  BEGIN

    IF p_step_no IS NOT NULL THEN

      OPEN x_line_cursor FOR
        SELECT d.line_no,
             i.concatenated_segments,
             i.inventory_item_id,
             d.material_detail_id
        FROM gme_material_details d,
           gme_batch_step_items si,
           gme_batch_steps      s,
           mtl_system_items_kfv i
        WHERE
            d.batch_id                      = p_batch_id
        AND d.batch_id                      = si.batch_id
        AND si.material_detail_id           = d.material_detail_id
        AND s.batchstep_no                  = p_step_no
        AND si.batchstep_id                 = s.batchstep_id
        AND d.line_type                     = p_line_type
        AND i.inventory_item_id             = d.inventory_item_id
        AND i.organization_id               = d.organization_id
        AND i.mtl_transactions_enabled_flag = 'Y'
        AND d.line_no LIKE (p_line_no)
        ORDER BY d.line_no;

    ELSE

      OPEN x_line_cursor FOR
        SELECT d.line_no,
             i.concatenated_segments,
             i.inventory_item_id,
             d.material_detail_id
        FROM gme_material_details d,
           mtl_system_items_kfv i
        WHERE
            d.batch_id                      = p_batch_id
        AND d.line_type                     = p_line_type
        AND i.inventory_item_id             = d.inventory_item_id
        AND i.organization_id               = d.organization_id
        AND i.mtl_transactions_enabled_flag = 'Y'
        AND d.line_no                       LIKE (p_line_no)
        ORDER BY d.line_no;

    END IF;

  END Step_Material_Line_LoV;

-- nsinghi bug#5236906. Added this procedure to show only lot controlled items.
 /*+========================================================================+
   | PROCEDURE NAME
   |   Step_Material_Lot_Line_LoV
   |
   | USAGE
   |
   | ARGUMENTS
   |   p_batch_id
   |   p_line_no
   |   p_item_no
   |   p_step_no
   |   p_line_type
   |
   | RETURNS
   |   REF cursor x_line_cursor
   |
   | HISTORY
   |   Created  26-Apr-05 Namit Singhi
   |
   +========================================================================+*/
  PROCEDURE Step_Material_Lot_Line_LoV
  (  x_line_cursor     OUT NOCOPY t_genref
  ,  p_batch_id        IN  NUMBER
  ,  p_line_no         IN  VARCHAR2
  ,  p_step_no         IN  VARCHAR2
  ,  p_line_type       IN  NUMBER
  )
  IS
  BEGIN

    IF p_step_no IS NOT NULL THEN

      OPEN x_line_cursor FOR
        SELECT d.line_no,
             i.concatenated_segments,
             i.inventory_item_id,
             d.material_detail_id
        FROM gme_material_details d,
           gme_batch_step_items si,
           gme_batch_steps      s,
           mtl_system_items_kfv i
        WHERE
            d.batch_id                      = p_batch_id
        AND d.batch_id                      = si.batch_id
        AND si.material_detail_id           = d.material_detail_id
        AND s.batchstep_no                  = p_step_no
        AND si.batchstep_id                 = s.batchstep_id
        AND d.line_type                     = p_line_type
        AND i.inventory_item_id             = d.inventory_item_id
        AND i.organization_id               = d.organization_id
	AND i.lot_control_code		    = 2
        AND i.mtl_transactions_enabled_flag = 'Y'
        AND d.line_no LIKE (p_line_no)
        ORDER BY d.line_no;

    ELSE

      OPEN x_line_cursor FOR
        SELECT d.line_no,
             i.concatenated_segments,
             i.inventory_item_id,
             d.material_detail_id
        FROM gme_material_details d,
           mtl_system_items_kfv i
        WHERE
            d.batch_id                      = p_batch_id
        AND d.line_type                     = p_line_type
        AND i.inventory_item_id             = d.inventory_item_id
        AND i.organization_id               = d.organization_id
	AND i.lot_control_code		    = 2
        AND i.mtl_transactions_enabled_flag = 'Y'
        AND d.line_no                       LIKE (p_line_no)
        ORDER BY d.line_no;

    END IF;

  END Step_Material_Lot_Line_LoV;

 /*+========================================================================+
   | PROCEDURE NAME
   |   Step_Material_Line_For_IB_LoV
   |
   | USAGE
   |
   | ARGUMENTS
   |   p_batch_id
   |   p_line_no
   |   p_item_no
   |   p_step_no
   |   p_line_type
   |
   | RETURNS
   |   REF cursor x_line_cursor
   |
   | HISTORY
   |   Created  06-Oct-05 Eddie Oumerretane
   |
   +========================================================================+*/
  PROCEDURE Step_Material_Line_For_IB_LoV
  (  x_line_cursor     OUT NOCOPY t_genref
  ,  p_batch_id        IN  NUMBER
  ,  p_line_no         IN  VARCHAR2
  ,  p_step_no         IN  VARCHAR2
  ,  p_line_type       IN  NUMBER
  )
  IS
  BEGIN

    IF p_step_no IS NOT NULL THEN

      OPEN x_line_cursor FOR
        SELECT d.line_no,
             i.concatenated_segments,
             i.inventory_item_id,
             d.material_detail_id
        FROM gme_material_details d,
           gme_batch_step_items si,
           gme_batch_steps      s,
           mtl_system_items_kfv i
        WHERE
            d.batch_id                      = p_batch_id
        AND d.batch_id                      = si.batch_id
        AND si.material_detail_id           = d.material_detail_id
        AND s.batchstep_no                  = p_step_no
        AND si.batchstep_id                 = s.batchstep_id
        AND d.line_type                     = p_line_type
        AND i.inventory_item_id             = d.inventory_item_id
        AND i.organization_id               = d.organization_id
        AND i.mtl_transactions_enabled_flag = 'Y'
        AND d.release_type                  IN (1,2) --- Manual/Incremental
        AND d.line_no LIKE (p_line_no)
        ORDER BY d.line_no;

    ELSE

      OPEN x_line_cursor FOR
        SELECT d.line_no,
             i.concatenated_segments,
             i.inventory_item_id,
             d.material_detail_id
        FROM gme_material_details d,
           mtl_system_items_kfv i
        WHERE
            d.batch_id                      = p_batch_id
        AND d.line_type                     = p_line_type
        AND i.inventory_item_id             = d.inventory_item_id
        AND i.organization_id               = d.organization_id
        AND i.mtl_transactions_enabled_flag = 'Y'
        AND d.release_type                  IN (1,2) --- Manual/Incremental
        AND d.line_no                       LIKE (p_line_no)
        ORDER BY d.line_no;

    END IF;

  END Step_Material_Line_For_IB_LoV;

 /*+========================================================================+
   | PROCEDURE NAME
   |   Step_Ingredient_LoV
   |
   | USAGE
   |
   | ARGUMENTS
   |   p_batch_id
   |   p_step_no
   |
   | RETURNS
   |   REF cursor x_step_cursor
   |
   | HISTORY
   |   Created  26-Apr-05 Eddie Oumerretane
   |
   +========================================================================+*/
  PROCEDURE Step_Ingredient_Lov
  (  x_step_cursor     OUT NOCOPY t_genref
  ,  p_batch_id        IN  NUMBER
  ,  p_step_no         IN  VARCHAR2
  )
  IS
  BEGIN

    OPEN x_step_cursor FOR
       SELECT DISTINCT
              s.batchstep_no,
              o.oprn_desc,
              s.batchstep_id
       FROM gme_batch_steps s,
            gme_batch_step_items i,
            gmd_operations o
       WHERE
            i.batch_id     = p_batch_id
        AND s.batch_id     = i.batch_id
        AND i.batchstep_id = s.batchstep_id
        AND s.step_status <> 4
        AND s.batchstep_no LIKE (p_step_no)
        AND s.oprn_id = o.oprn_id
       ORDER BY 1;

  EXCEPTION
    WHEN FND_API.G_EXC_UNEXPECTED_ERROR
    THEN
      NULL;
  END Step_Ingredient_Lov;

 /*+========================================================================+
   | PROCEDURE NAME
   |   Step_Prod_ByProd_LoV
   |
   | USAGE
   |
   | ARGUMENTS
   |   p_batch_id
   |   p_step_no
   |
   | RETURNS
   |   REF cursor x_step_cursor
   |
   | HISTORY
   |   Created  26-Apr-05 Eddie Oumerretane
   |
   +========================================================================+*/
  PROCEDURE Step_Prod_ByProd_LoV
  (  x_step_cursor     OUT NOCOPY t_genref
  ,  p_batch_id        IN  NUMBER
  ,  p_step_no         IN  VARCHAR2
  )
  IS
  BEGIN

    OPEN x_step_cursor FOR
       SELECT DISTINCT
              s.batchstep_no,
              o.oprn_desc,
              s.batchstep_id
       FROM gme_batch_steps s,
            gme_batch_step_items i,
            gmd_operations o,
            gme_material_details d
       WHERE
            i.batch_id     = p_batch_id
        AND s.batch_id     = i.batch_id
        AND i.batchstep_id = s.batchstep_id
        AND i.material_detail_id = d.material_detail_id
        AND d.line_type IN (1,2)
        AND s.step_status <> 4
        AND s.batchstep_no LIKE (p_step_no)
        AND s.oprn_id = o.oprn_id
       ORDER BY 1;

  EXCEPTION
    WHEN FND_API.G_EXC_UNEXPECTED_ERROR
    THEN
      NULL;
  END Step_Prod_ByProd_LoV;

 /*+========================================================================+
   | PROCEDURE NAME
   |   Step_Pend_Lot_LoV
   |
   | USAGE
   |
   | ARGUMENTS
   |   p_batch_id
   |   p_step_no
   |
   | RETURNS
   |   REF cursor x_step_cursor
   |
   | HISTORY
   |   Created  03-Oct-05 Eddie Oumerretane
   |
   +========================================================================+*/
  PROCEDURE Step_Pend_Lot_LoV
  (  x_step_cursor     OUT NOCOPY t_genref
  ,  p_batch_id        IN  NUMBER
  ,  p_step_no         IN  VARCHAR2
  )
  IS
  BEGIN

    OPEN x_step_cursor FOR
       SELECT DISTINCT
              s.batchstep_no,
              o.oprn_desc,
              s.batchstep_id
       FROM gme_batch_steps s,
            gme_batch_step_items i,
            gmd_operations o,
            gme_material_details d
       WHERE
            i.batch_id     = p_batch_id
        AND s.batch_id     = i.batch_id
        AND i.batchstep_id = s.batchstep_id
        AND i.material_detail_id = d.material_detail_id
        AND d.line_type IN (1,2)
        AND s.step_status IN (1,2)
        AND s.batchstep_no LIKE (p_step_no)
        AND s.oprn_id = o.oprn_id
       ORDER BY 1;

  EXCEPTION
    WHEN FND_API.G_EXC_UNEXPECTED_ERROR
    THEN
      NULL;
  END Step_Pend_Lot_LoV;

 /*+========================================================================+
   | PROCEDURE NAME
   |   Step_All_Items_LoV
   |
   | USAGE
   |
   | ARGUMENTS
   |   p_batch_id
   |   p_step_no
   |
   | RETURNS
   |   REF cursor x_step__cursor
   |
   | HISTORY
   |   Created  26-Apr-05 Eddie Oumerretane
   |
   +========================================================================+*/
  PROCEDURE Step_All_Items_LoV
  (  x_step_cursor     OUT NOCOPY t_genref
  ,  p_batch_id        IN  NUMBER
  ,  p_step_no         IN  VARCHAR2
  )
  IS
  BEGIN

    OPEN x_step_cursor FOR
       SELECT DISTINCT
              s.batchstep_no,
              o.oprn_desc,
              s.batchstep_id
       FROM gme_batch_steps s,
            gme_batch_step_items i,
            gmd_operations o,
            gme_material_details d
       WHERE
            i.batch_id     = p_batch_id
        AND s.batch_id     = i.batch_id
        AND i.batchstep_id = s.batchstep_id
        AND i.material_detail_id = d.material_detail_id
        AND s.step_status IN (1,2,3)
        AND s.batchstep_no LIKE (p_step_no)
        AND s.oprn_id = o.oprn_id
       ORDER BY 1;

  EXCEPTION
    WHEN FND_API.G_EXC_UNEXPECTED_ERROR
    THEN
      NULL;
  END Step_All_Items_LoV;

 /*+========================================================================+
   | PROCEDURE NAME
   |   Reason_LoV
   |
   | USAGE
   |
   | ARGUMENTS
   |   p_reason_name
   |
   | RETURNS
   |   REF cursor x_reason_cursor
   |
   | HISTORY
   |   Created  26-Apr-05 Eddie Oumerretane
   |
   +========================================================================+*/
  PROCEDURE Reason_LoV
  (  x_reason_cursor  OUT NOCOPY t_genref
  ,  p_reason_name    IN  VARCHAR2
  )
  IS
  BEGIN

    OPEN x_reason_cursor FOR
      SELECT reason_name, description, reason_id
      FROM  mtl_transaction_reasons
      WHERE reason_name like (p_reason_name)
      AND   NVL(disable_date, SYSDATE+1) > SYSDATE
      ORDER BY reason_name;

  END Reason_LoV;

 /*+========================================================================+
   | PROCEDURE NAME
   |   Activity_LoV
   |
   | USAGE
   |
   | ARGUMENTS
   |   p_organization_id
   |   p_batch_id
   |   p_step_no
   |   p_activity
   |
   | RETURNS
   |   REF cursor x_activity_cursor
   |
   | HISTORY
   |   Created  26-Apr-05 Eddie Oumerretane
   |
   +========================================================================+*/
  PROCEDURE Activity_LoV
  (  x_activity_cursor     OUT NOCOPY t_genref
  ,  p_organization_id IN  NUMBER
  ,  p_batch_id        IN  NUMBER
  ,  p_step_no         IN  VARCHAR2
  ,  p_activity        IN  VARCHAR2
  )
  IS
  BEGIN
    OPEN x_activity_cursor FOR
      SELECT a.activity,
             a.batchstep_activity_id
      FROM   gme_batch_header h,
             gme_batch_steps s,
             gme_batch_step_activities a
      WHERE  h.organization_id = p_organization_id
      AND    h.batch_id =  p_batch_id
      AND    h.batch_type = 0
      AND    h.batch_id = s.batch_id
      AND    s.batchstep_no = p_step_no
      AND    s.batchstep_id = a.batchstep_id
      AND    h.batch_id = a.batch_id
      --- Bug 5236930 AND    a.activity LIKE LTRIM(RTRIM(p_activity||'%'))
      AND    a.activity LIKE p_activity
      ORDER BY a.activity;

  END Activity_LoV;

 /*+========================================================================+
   | PROCEDURE NAME
   |   Resource_LoV
   |
   | USAGE
   |
   | ARGUMENTS
   |   p_org_id
   |   p_batch_id
   |   p_step_no
   |   p_activity_id
   |   p_resource
   |
   | RETURNS
   |   REF cursor x_resource_cursor
   |
   | HISTORY
   |   Created  26-Apr-05 Eddie Oumerretane
   |
   +========================================================================+*/
  PROCEDURE Resource_LoV
  (  x_resource_cursor     OUT NOCOPY t_genref
  ,  p_org_id          IN  NUMBER
  ,  p_batch_id        IN  NUMBER
  ,  p_step_no         IN  NUMBER
  ,  p_activity_id     IN  NUMBER
  ,  p_resource        IN  VARCHAR2
  )
  IS
  BEGIN

    OPEN x_resource_cursor FOR
      SELECT r.resources,
             r.batchstep_resource_id
      FROM   gme_batch_header h,
             gme_batch_steps s,
             gme_batch_step_activities a,
             gme_batch_step_resources r
      WHERE  h.organization_id =  p_org_id
      AND    h.batch_id =  p_batch_id
      AND    h.batch_type = 0
      AND    h.batch_id = s.batch_id
      AND    s.batchstep_no = p_step_no
      AND    s.batchstep_id = a.batchstep_id
      AND    h.batch_id = a.batch_id
      AND    r.batch_id = h.batch_id
      AND    r.batchstep_id = s.batchstep_id
      AND    r.batchstep_activity_id = a.batchstep_activity_id
      AND    r.batchstep_activity_id = p_activity_id
      --- Bug 5236930 AND    r.resources LIKE LTRIM(RTRIM(p_resource||'%'))
      AND    r.resources LIKE p_resource
      ORDER BY r.resources;

  END Resource_LoV;

 /*+========================================================================+
   | PROCEDURE NAME
   |   Ingredient_LoV
   |
   | USAGE
   |
   | ARGUMENTS
   |   p_batch_id
   |   p_line_no
   |   p_item_no
   |
   | RETURNS
   |   REF cursor x_item_cursor
   |
   | HISTORY
   |   Created  26-Apr-05 Eddie Oumerretane
   |
   +========================================================================+*/
  PROCEDURE Ingredient_LoV
  (  x_item_cursor     OUT NOCOPY t_genref
  ,  p_batch_id        IN  NUMBER
  ,  p_line_no         IN  VARCHAR2
  ,  p_item_no         IN  VARCHAR2
  )
  IS

    l_cross_ref varchar2(204);

  BEGIN

    l_cross_ref := lpad(Rtrim(p_item_no, '%'), g_gtin_code_length,'00000000000000');

      OPEN x_item_cursor FOR

      SELECT DISTINCT
       i.concatenated_segments,
       i.description,
       d.line_no,
       i.inventory_item_id,
       d.material_detail_id,
       NVL(i.lot_control_code, 1),
       NVL(i.location_control_code, 1),
       'N', --- gtin_entered_ind
       NULL, --- GTIN UOM code
       NVL(i.restrict_locators_code, 2),
       NVL(i.grade_control_flag, 'N'),
       NVL(i.lot_status_enabled, 'N'),
       NVL(i.lot_divisible_flag, 'N')
      FROM gme_material_details d,
           mtl_system_items_kfv i
      WHERE d.batch_id = p_batch_id
      AND d.inventory_item_id = i.inventory_item_id
      AND d.organization_id = i.organization_id
      AND d.line_no = NVL(p_line_no, d.line_no)
      AND d.line_type  = -1
      AND i.reservable_type = 1
      AND i.concatenated_segments LIKE (p_item_no)

      --- For GTIN support
      UNION

      SELECT DISTINCT
       i.concatenated_segments,
       i.description,
       d.line_no,
       i.inventory_item_id,
       d.material_detail_id,
       NVL(i.lot_control_code, 1),
       NVL(i.location_control_code, 1),
       'Y', --- gtin_entered_ind
       NVL(uom_code, ' '), --- GTIN UOM code
       NVL(i.restrict_locators_code, 2),
       NVL(i.grade_control_flag, 'N'),
       NVL(i.lot_status_enabled, 'N'),
       NVL(i.lot_divisible_flag, 'N')
      FROM
           gme_material_details d,
           mtl_cross_references mcr,
           mtl_system_items_kfv i
      WHERE d.batch_id = p_batch_id
      AND d.inventory_item_id = i.inventory_item_id
      AND d.organization_id = i.organization_id
      AND d.line_no = NVL(p_line_no, d.line_no)
      AND d.line_type  = -1
      AND i.inventory_item_id = mcr.inventory_item_id
      AND i.reservable_type = 1
      AND    mcr.cross_reference_type = g_gtin_cross_ref_type
      AND    mcr.cross_reference      LIKE l_cross_ref
      AND    (mcr.organization_id = i.organization_id
           OR
             mcr.org_independent_flag = 'Y');

  EXCEPTION
    WHEN FND_API.G_EXC_UNEXPECTED_ERROR
    THEN
      NULL;
  END Ingredient_LoV;

 /*+========================================================================+
   | PROCEDURE NAME
   |   Ingredient_Rsrv_LoV
   |
   | USAGE
   |
   | ARGUMENTS
   |   p_batch_id
   |   p_line_no
   |   p_item_no
   |
   | RETURNS
   |   REF cursor x_item_cursor
   |
   | HISTORY
   |   Created  23-Sep-05 Eddie Oumerretane
   |
   +========================================================================+*/
  PROCEDURE Ingredient_Rsrv_LoV
  (  x_item_cursor     OUT NOCOPY t_genref
  ,  p_batch_id        IN  NUMBER
  ,  p_line_no         IN  VARCHAR2
  ,  p_item_no         IN  VARCHAR2
  )
  IS

    l_cross_ref varchar2(204);

  BEGIN

    l_cross_ref := lpad(Rtrim(p_item_no, '%'), g_gtin_code_length,'00000000000000');

      OPEN x_item_cursor FOR

      SELECT DISTINCT
       i.concatenated_segments,
       i.description,
       d.line_no,
       i.inventory_item_id,
       d.material_detail_id,
       NVL(i.lot_control_code, 1),
       NVL(i.location_control_code, 1),
       'N', --- gtin_entered_ind
       NULL, --- GTIN UOM code
       NVL(i.restrict_locators_code, 2),
       NVL(i.grade_control_flag, 'N'),
       NVL(i.lot_status_enabled, 'N'),
       NVL(i.lot_divisible_flag, 'N')
      FROM gme_material_details d,
           mtl_system_items_kfv i
      WHERE d.batch_id = p_batch_id
      AND d.inventory_item_id = i.inventory_item_id
      AND d.organization_id = i.organization_id
      AND d.line_no = NVL(p_line_no, d.line_no)
      AND d.line_type  = -1
      AND d.phantom_type = 0
      AND i.reservable_type = 1
      AND i.concatenated_segments LIKE (p_item_no)

      --- For GTIN support
      UNION

      SELECT DISTINCT
       i.concatenated_segments,
       i.description,
       d.line_no,
       i.inventory_item_id,
       d.material_detail_id,
       NVL(i.lot_control_code, 1),
       NVL(i.location_control_code, 1),
       'Y', --- gtin_entered_ind
       NVL(uom_code, ' '), --- GTIN UOM code
       NVL(i.restrict_locators_code, 2),
       NVL(i.grade_control_flag, 'N'),
       NVL(i.lot_status_enabled, 'N'),
       NVL(i.lot_divisible_flag, 'N')
      FROM
           gme_material_details d,
           mtl_cross_references mcr,
           mtl_system_items_kfv i
      WHERE d.batch_id = p_batch_id
      AND d.inventory_item_id = i.inventory_item_id
      AND d.organization_id = i.organization_id
      AND d.line_no = NVL(p_line_no, d.line_no)
      AND d.line_type  = -1
      AND d.phantom_type = 0
      AND i.inventory_item_id = mcr.inventory_item_id
      AND i.reservable_type = 1
      AND    mcr.cross_reference_type = g_gtin_cross_ref_type
      AND    mcr.cross_reference      LIKE l_cross_ref
      AND    (mcr.organization_id = i.organization_id
           OR
             mcr.org_independent_flag = 'Y');

  EXCEPTION
    WHEN FND_API.G_EXC_UNEXPECTED_ERROR
    THEN
      NULL;
  END Ingredient_Rsrv_LoV;

 /*+========================================================================+
   | PROCEDURE NAME
   |   Ingredient_Step_LoV
   |
   | USAGE
   |
   | ARGUMENTS
   |   p_batch_id
   |   p_line_no
   |   p_item_no
   |   p_step_no
   |
   | RETURNS
   |   REF cursor x_item_cursor
   |
   | HISTORY
   |   Created  26-Apr-05 Eddie Oumerretane
   |
   +========================================================================+*/
  PROCEDURE Ingredient_Step_LoV
  (  x_item_cursor     OUT NOCOPY t_genref
  ,  p_batch_id        IN  NUMBER
  ,  p_line_no         IN  VARCHAR2
  ,  p_item_no         IN  VARCHAR2
  ,  p_step_no         IN  VARCHAR2
  )
  IS

    l_cross_ref varchar2(204);

  BEGIN

    IF (g_debug IS NOT NULL) THEN
       gme_debug.log_initialize ('MobileIngStepLoV');
    END IF;

    l_cross_ref := lpad(Rtrim(p_item_no, '%'), g_gtin_code_length,'00000000000000');

    gme_debug.put_line('Cross ref type  =  '||g_gtin_cross_ref_type);
    gme_debug.put_line('Cross ref       =  '||l_cross_ref);

    IF p_step_no IS NULL THEN

      OPEN x_item_cursor FOR

      SELECT DISTINCT
       i.concatenated_segments,
       i.description,
       d.line_no,
       i.inventory_item_id,
       d.material_detail_id,
       NVL(i.lot_control_code, 1),
       NVL(i.location_control_code, 1),
       'N', --- gtin_entered_ind
       NULL, --- GTIN UOM code
       NVL(i.restrict_locators_code, 2),
       NVL(i.grade_control_flag, 'N'),
       NVL(i.lot_status_enabled, 'N'),
       NVL(i.lot_divisible_flag, 'N')
      FROM gme_material_details d,
           mtl_system_items_kfv i
      WHERE d.batch_id                    = p_batch_id
      AND d.inventory_item_id             = i.inventory_item_id
      AND d.organization_id               = i.organization_id
      AND d.line_no                       = NVL(p_line_no, d.line_no)
      AND d.line_type                     = -1
      AND i.mtl_transactions_enabled_flag = 'Y'
      AND i.concatenated_segments LIKE (p_item_no)

      --- For GTIN support
      UNION

      SELECT DISTINCT
       i.concatenated_segments,
       i.description,
       d.line_no,
       i.inventory_item_id,
       d.material_detail_id,
       NVL(i.lot_control_code, 1),
       NVL(i.location_control_code, 1),
       'Y', --- gtin_entered_ind
       NVL(uom_code, ' '), --- GTIN UOM code
       NVL(i.restrict_locators_code, 2),
       NVL(i.grade_control_flag, 'N'),
       NVL(i.lot_status_enabled, 'N'),
       NVL(i.lot_divisible_flag, 'N')
      FROM
           gme_material_details d,
           mtl_cross_references mcr,
           mtl_system_items_kfv i
      WHERE d.batch_id                    = p_batch_id
      AND d.inventory_item_id             = i.inventory_item_id
      AND d.organization_id               = i.organization_id
      AND d.line_no                       = NVL(p_line_no, d.line_no)
      AND d.line_type                     = -1
      AND i.inventory_item_id             = mcr.inventory_item_id
      AND i.mtl_transactions_enabled_flag = 'Y'
      AND    mcr.cross_reference_type     = g_gtin_cross_ref_type
      AND    mcr.cross_reference          LIKE l_cross_ref
      AND    (mcr.organization_id         = i.organization_id
           OR
             mcr.org_independent_flag = 'Y');

    ELSE

      OPEN x_item_cursor FOR

      SELECT DISTINCT
       i.concatenated_segments,
       i.description,
       d.line_no,
       i.inventory_item_id,
       d.material_detail_id,
       NVL(i.lot_control_code, 1),
       NVL(i.location_control_code, 1),
       'N', --- gtin_entered_ind
       NULL, --- GTIN UOM code
       NVL(i.restrict_locators_code, 2),
       NVL(i.grade_control_flag, 'N'),
       NVL(i.lot_status_enabled, 'N'),
       NVL(i.lot_divisible_flag, 'N')
      FROM gme_material_details d,
           gme_batch_step_items si,
           gme_batch_steps      s,
           mtl_system_items_kfv i
      WHERE d.batch_id                    = p_batch_id
      AND d.batch_id                      = si.batch_id
      AND si.material_detail_id           = d.material_detail_id
      AND s.batchstep_no                  = p_step_no
      AND si.batchstep_id                 = s.batchstep_id
      AND d.inventory_item_id             = i.inventory_item_id
      AND d.organization_id               = i.organization_id
      AND d.line_no                       = NVL(p_line_no, d.line_no)
      AND d.line_type                     = -1
      AND i.mtl_transactions_enabled_flag = 'Y'
      AND i.concatenated_segments         LIKE (p_item_no)

      --- For GTIN support
      UNION

      SELECT DISTINCT
       i.concatenated_segments,
       i.description,
       d.line_no,
       i.inventory_item_id,
       d.material_detail_id,
       NVL(i.lot_control_code, 1),
       NVL(i.location_control_code, 1),
       'Y', --- gtin_entered_ind
       NVL(uom_code, ' '), --- GTIN UOM code
       NVL(i.restrict_locators_code, 2),
       NVL(i.grade_control_flag, 'N'),
       NVL(i.lot_status_enabled, 'N'),
       NVL(i.lot_divisible_flag, 'N')
      FROM
           gme_material_details d,
           mtl_cross_references mcr,
           gme_batch_step_items si,
           gme_batch_steps      s,
           mtl_system_items_kfv i
      WHERE d.batch_id                    = p_batch_id
      AND d.batch_id                      = si.batch_id
      AND si.material_detail_id           = d.material_detail_id
      AND s.batchstep_no                  = p_step_no
      AND si.batchstep_id                 = s.batchstep_id
      AND d.inventory_item_id             = i.inventory_item_id
      AND d.organization_id               = i.organization_id
      AND d.line_no                       = NVL(p_line_no, d.line_no)
      AND d.line_type                     = -1
      AND i.inventory_item_id             = mcr.inventory_item_id
      AND i.mtl_transactions_enabled_flag = 'Y'
      AND    mcr.cross_reference_type     = g_gtin_cross_ref_type
      AND    mcr.cross_reference          LIKE l_cross_ref
      AND    (mcr.organization_id         = i.organization_id
           OR
             mcr.org_independent_flag = 'Y');
    END IF;

  EXCEPTION
    WHEN FND_API.G_EXC_UNEXPECTED_ERROR
    THEN
      NULL;
  END Ingredient_Step_LoV;

 /*+========================================================================+
   | PROCEDURE NAME
   |   Material_Step_LoV
   |
   | USAGE
   |
   | ARGUMENTS
   |   p_batch_id
   |   p_line_no
   |   p_item_no
   |   p_step_no
   |   p_line_type
   |
   | RETURNS
   |   REF cursor x_item_cursor
   |
   | HISTORY
   |   Created  26-Apr-05 Eddie Oumerretane
   |
   +========================================================================+*/
  PROCEDURE Material_Step_LoV
  (  x_item_cursor     OUT NOCOPY t_genref
  ,  p_batch_id        IN  NUMBER
  ,  p_line_no         IN  VARCHAR2
  ,  p_item_no         IN  VARCHAR2
  ,  p_step_no         IN  VARCHAR2
  ,  p_line_type       IN  NUMBER
  )
  IS

    l_cross_ref varchar2(204);

  BEGIN

    IF (g_debug IS NOT NULL) THEN
       gme_debug.log_initialize ('MobileMtlStepLoV');
    END IF;

    l_cross_ref := lpad(Rtrim(p_item_no, '%'), g_gtin_code_length,'00000000000000');


    gme_debug.put_line('Cross ref type  =  '||g_gtin_cross_ref_type);
    gme_debug.put_line('Cross ref       =  '||l_cross_ref);

    IF p_step_no IS NULL THEN

      OPEN x_item_cursor FOR

      SELECT DISTINCT
       i.concatenated_segments,
       i.description,
       d.line_no,
       i.inventory_item_id,
       d.material_detail_id,
       NVL(i.lot_control_code, 1),
       NVL(i.location_control_code, 1),
       'N', --- gtin_entered_ind
       NULL, --- GTIN UOM code
       NVL(i.restrict_locators_code, 2),
       NVL(i.grade_control_flag, 'N'),
       NVL(i.lot_status_enabled, 'N'),
       NVL(i.lot_divisible_flag, 'N')
      FROM gme_material_details d,
           mtl_system_items_kfv i
      WHERE d.batch_id                    = p_batch_id
      AND d.inventory_item_id             = i.inventory_item_id
      AND d.organization_id               = i.organization_id
      AND d.line_no                       = NVL(p_line_no, d.line_no)
      AND d.line_type                     = p_line_type
      AND i.mtl_transactions_enabled_flag = 'Y'
      AND i.concatenated_segments LIKE (p_item_no)

      --- For GTIN support
      UNION

      SELECT DISTINCT
       i.concatenated_segments,
       i.description,
       d.line_no,
       i.inventory_item_id,
       d.material_detail_id,
       NVL(i.lot_control_code, 1),
       NVL(i.location_control_code, 1),
       'Y', --- gtin_entered_ind
       NVL(uom_code, ' '), --- GTIN UOM code
       NVL(i.restrict_locators_code, 2),
       NVL(i.grade_control_flag, 'N'),
       NVL(i.lot_status_enabled, 'N'),
       NVL(i.lot_divisible_flag, 'N')
      FROM
           gme_material_details d,
           mtl_cross_references mcr,
           mtl_system_items_kfv i
      WHERE d.batch_id                    = p_batch_id
      AND d.inventory_item_id             = i.inventory_item_id
      AND d.organization_id               = i.organization_id
      AND d.line_no                       = NVL(p_line_no, d.line_no)
      AND d.line_type                     = p_line_type
      AND i.inventory_item_id             = mcr.inventory_item_id
      AND i.mtl_transactions_enabled_flag = 'Y'
      AND    mcr.cross_reference_type     = g_gtin_cross_ref_type
      AND    mcr.cross_reference          LIKE l_cross_ref
      AND    (mcr.organization_id         = i.organization_id
           OR
             mcr.org_independent_flag = 'Y')
      ORDER BY line_no;

    ELSE

      OPEN x_item_cursor FOR

      SELECT DISTINCT
       i.concatenated_segments,
       i.description,
       d.line_no,
       i.inventory_item_id,
       d.material_detail_id,
       NVL(i.lot_control_code, 1),
       NVL(i.location_control_code, 1),
       'N', --- gtin_entered_ind
       NULL, --- GTIN UOM code
       NVL(i.restrict_locators_code, 2),
       NVL(i.grade_control_flag, 'N'),
       NVL(i.lot_status_enabled, 'N'),
       NVL(i.lot_divisible_flag, 'N')
      FROM gme_material_details d,
           gme_batch_step_items si,
           gme_batch_steps      s,
           mtl_system_items_kfv i
      WHERE d.batch_id                    = p_batch_id
      AND d.batch_id                      = si.batch_id
      AND si.material_detail_id           = d.material_detail_id
      AND s.batchstep_no                  = p_step_no
      AND si.batchstep_id                 = s.batchstep_id
      AND d.inventory_item_id             = i.inventory_item_id
      AND d.organization_id               = i.organization_id
      AND d.line_no                       = NVL(p_line_no, d.line_no)
      AND d.line_type                     = p_line_type
      AND i.mtl_transactions_enabled_flag = 'Y'
      AND i.concatenated_segments         LIKE (p_item_no)

      --- For GTIN support
      UNION

      SELECT DISTINCT
       i.concatenated_segments,
       i.description,
       d.line_no,
       i.inventory_item_id,
       d.material_detail_id,
       NVL(i.lot_control_code, 1),
       NVL(i.location_control_code, 1),
       'Y', --- gtin_entered_ind
       NVL(uom_code, ' '), --- GTIN UOM code
       NVL(i.restrict_locators_code, 2),
       NVL(i.grade_control_flag, 'N'),
       NVL(i.lot_status_enabled, 'N'),
       NVL(i.lot_divisible_flag, 'N')
      FROM
           gme_material_details d,
           mtl_cross_references mcr,
           gme_batch_step_items si,
           gme_batch_steps      s,
           mtl_system_items_kfv i
      WHERE d.batch_id                    = p_batch_id
      AND d.batch_id                      = si.batch_id
      AND si.material_detail_id           = d.material_detail_id
      AND s.batchstep_no                  = p_step_no
      AND si.batchstep_id                 = s.batchstep_id
      AND d.inventory_item_id             = i.inventory_item_id
      AND d.organization_id               = i.organization_id
      AND d.line_no                       = NVL(p_line_no, d.line_no)
      AND d.line_type                     = p_line_type
      AND i.inventory_item_id             = mcr.inventory_item_id
      AND i.mtl_transactions_enabled_flag = 'Y'
      AND    mcr.cross_reference_type     = g_gtin_cross_ref_type
      AND    mcr.cross_reference          LIKE l_cross_ref
      AND    (mcr.organization_id         = i.organization_id
           OR
             mcr.org_independent_flag = 'Y')
      ORDER BY line_no;
    END IF;

  EXCEPTION
    WHEN FND_API.G_EXC_UNEXPECTED_ERROR
    THEN
      NULL;
  END Material_Step_LoV;

-- nsinghi bug#5236906. Added this procedure.
 /*+========================================================================+
   | PROCEDURE NAME
   |   Material_Lot_LoV
   |
   | USAGE
   |
   | ARGUMENTS
   |   p_batch_id
   |   p_line_no
   |   p_item_no
   |   p_step_no
   |   p_line_type
   |
   | RETURNS
   |   REF cursor x_item_cursor
   |
   | HISTORY
   |   Created  26-Apr-05 Namit Singhi
   |
   +========================================================================+*/
  PROCEDURE Material_Lot_LoV
  (  x_item_cursor     OUT NOCOPY t_genref
  ,  p_batch_id        IN  NUMBER
  ,  p_line_no         IN  VARCHAR2
  ,  p_item_no         IN  VARCHAR2
  ,  p_step_no         IN  VARCHAR2
  ,  p_line_type       IN  NUMBER
  )
  IS

    l_cross_ref varchar2(204);

  BEGIN

    IF (g_debug IS NOT NULL) THEN
       gme_debug.log_initialize ('MobileLotProdStepLoV');
    END IF;

    l_cross_ref := lpad(Rtrim(p_item_no, '%'), g_gtin_code_length,'00000000000000');


    gme_debug.put_line('Cross ref type  =  '||g_gtin_cross_ref_type);
    gme_debug.put_line('Cross ref       =  '||l_cross_ref);

    IF p_step_no IS NULL THEN

      OPEN x_item_cursor FOR

      SELECT DISTINCT
       i.concatenated_segments,
       i.description,
       d.line_no,
       i.inventory_item_id,
       d.material_detail_id,
       NVL(i.lot_control_code, 2),
       NVL(i.location_control_code, 1),
       'N', --- gtin_entered_ind
       NULL, --- GTIN UOM code
       NVL(i.restrict_locators_code, 2),
       NVL(i.grade_control_flag, 'N'),
       NVL(i.lot_status_enabled, 'N'),
       NVL(i.lot_divisible_flag, 'N')
      FROM gme_material_details d,
           mtl_system_items_kfv i
      WHERE d.batch_id                    = p_batch_id
      AND d.inventory_item_id             = i.inventory_item_id
      AND d.organization_id               = i.organization_id
      AND d.line_no                       = NVL(p_line_no, d.line_no)
      AND d.line_type                     = p_line_type
      AND i.mtl_transactions_enabled_flag = 'Y'
      AND i.lot_control_code		  = 2
      AND i.concatenated_segments LIKE (p_item_no)

      --- For GTIN support
      UNION

      SELECT DISTINCT
       i.concatenated_segments,
       i.description,
       d.line_no,
       i.inventory_item_id,
       d.material_detail_id,
       NVL(i.lot_control_code, 2),
       NVL(i.location_control_code, 1),
       'Y', --- gtin_entered_ind
       NVL(uom_code, ' '), --- GTIN UOM code
       NVL(i.restrict_locators_code, 2),
       NVL(i.grade_control_flag, 'N'),
       NVL(i.lot_status_enabled, 'N'),
       NVL(i.lot_divisible_flag, 'N')
      FROM
           gme_material_details d,
           mtl_cross_references mcr,
           mtl_system_items_kfv i
      WHERE d.batch_id                    = p_batch_id
      AND d.inventory_item_id             = i.inventory_item_id
      AND d.organization_id               = i.organization_id
      AND d.line_no                       = NVL(p_line_no, d.line_no)
      AND d.line_type                     = p_line_type
      AND i.lot_control_code	          = 2
      AND i.inventory_item_id             = mcr.inventory_item_id
      AND i.mtl_transactions_enabled_flag = 'Y'
      AND    mcr.cross_reference_type     = g_gtin_cross_ref_type
      AND    mcr.cross_reference          LIKE l_cross_ref
      AND    (mcr.organization_id         = i.organization_id
           OR
             mcr.org_independent_flag = 'Y');

    ELSE

      OPEN x_item_cursor FOR

      SELECT DISTINCT
       i.concatenated_segments,
       i.description,
       d.line_no,
       i.inventory_item_id,
       d.material_detail_id,
       NVL(i.lot_control_code, 2),
       NVL(i.location_control_code, 1),
       'N', --- gtin_entered_ind
       NULL, --- GTIN UOM code
       NVL(i.restrict_locators_code, 2),
       NVL(i.grade_control_flag, 'N'),
       NVL(i.lot_status_enabled, 'N'),
       NVL(i.lot_divisible_flag, 'N')
      FROM gme_material_details d,
           gme_batch_step_items si,
           gme_batch_steps      s,
           mtl_system_items_kfv i
      WHERE d.batch_id                    = p_batch_id
      AND d.batch_id                      = si.batch_id
      AND si.material_detail_id           = d.material_detail_id
      AND s.batchstep_no                  = p_step_no
      AND si.batchstep_id                 = s.batchstep_id
      AND d.inventory_item_id             = i.inventory_item_id
      AND d.organization_id               = i.organization_id
      AND d.line_no                       = NVL(p_line_no, d.line_no)
      AND d.line_type                     = p_line_type
      AND i.lot_control_code		  = 2
      AND i.mtl_transactions_enabled_flag = 'Y'
      AND i.concatenated_segments         LIKE (p_item_no)

      --- For GTIN support
      UNION

      SELECT DISTINCT
       i.concatenated_segments,
       i.description,
       d.line_no,
       i.inventory_item_id,
       d.material_detail_id,
       NVL(i.lot_control_code, 2),
       NVL(i.location_control_code, 1),
       'Y', --- gtin_entered_ind
       NVL(uom_code, ' '), --- GTIN UOM code
       NVL(i.restrict_locators_code, 2),
       NVL(i.grade_control_flag, 'N'),
       NVL(i.lot_status_enabled, 'N'),
       NVL(i.lot_divisible_flag, 'N')
      FROM
           gme_material_details d,
           mtl_cross_references mcr,
           gme_batch_step_items si,
           gme_batch_steps      s,
           mtl_system_items_kfv i
      WHERE d.batch_id                    = p_batch_id
      AND d.batch_id                      = si.batch_id
      AND si.material_detail_id           = d.material_detail_id
      AND s.batchstep_no                  = p_step_no
      AND si.batchstep_id                 = s.batchstep_id
      AND d.inventory_item_id             = i.inventory_item_id
      AND d.organization_id               = i.organization_id
      AND d.line_no                       = NVL(p_line_no, d.line_no)
      AND d.line_type                     = p_line_type
      AND i.lot_control_code		  = 2
      AND i.inventory_item_id             = mcr.inventory_item_id
      AND i.mtl_transactions_enabled_flag = 'Y'
      AND    mcr.cross_reference_type     = g_gtin_cross_ref_type
      AND    mcr.cross_reference          LIKE l_cross_ref
      AND    (mcr.organization_id         = i.organization_id
           OR
             mcr.org_independent_flag = 'Y');
    END IF;

  EXCEPTION
    WHEN FND_API.G_EXC_UNEXPECTED_ERROR
    THEN
      NULL;
  END Material_Lot_LoV;

 /*+========================================================================+
   | PROCEDURE NAME
   |   Material_Step_For_IB_LoV
   |
   | USAGE
   |
   | ARGUMENTS
   |   p_batch_id
   |   p_line_no
   |   p_item_no
   |   p_step_no
   |   p_line_type
   |
   | RETURNS
   |   REF cursor x_item_cursor
   |
   | HISTORY
   |   Created  06-Oct-05 Eddie Oumerretane
   |
   +========================================================================+*/
  PROCEDURE Material_Step_For_IB_LoV
  (  x_item_cursor     OUT NOCOPY t_genref
  ,  p_batch_id        IN  NUMBER
  ,  p_line_no         IN  VARCHAR2
  ,  p_item_no         IN  VARCHAR2
  ,  p_step_no         IN  VARCHAR2
  ,  p_line_type       IN  NUMBER
  )
  IS

    l_cross_ref varchar2(204);

  BEGIN

    IF (g_debug IS NOT NULL) THEN
       gme_debug.log_initialize ('MobileMtlStepLoV');
    END IF;

    l_cross_ref := lpad(Rtrim(p_item_no, '%'), g_gtin_code_length,'00000000000000');


    gme_debug.put_line('Cross ref type  =  '||g_gtin_cross_ref_type);
    gme_debug.put_line('Cross ref       =  '||l_cross_ref);

    IF p_step_no IS NULL THEN

      OPEN x_item_cursor FOR

      SELECT DISTINCT
       i.concatenated_segments,
       i.description,
       d.line_no,
       i.inventory_item_id,
       d.material_detail_id,
       NVL(i.lot_control_code, 1),
       NVL(i.location_control_code, 1),
       'N', --- gtin_entered_ind
       NULL, --- GTIN UOM code
       NVL(i.restrict_locators_code, 2),
       NVL(i.grade_control_flag, 'N'),
       NVL(i.lot_status_enabled, 'N'),
       NVL(i.lot_divisible_flag, 'N')
      FROM gme_material_details d,
           mtl_system_items_kfv i
      WHERE d.batch_id                    = p_batch_id
      AND d.inventory_item_id             = i.inventory_item_id
      AND d.organization_id               = i.organization_id
      AND d.line_no                       = NVL(p_line_no, d.line_no)
      AND d.line_type                     = p_line_type
      AND i.mtl_transactions_enabled_flag = 'Y'
      AND d.release_type                  IN (1,2) --- Manual/Incremental
      AND i.concatenated_segments LIKE (p_item_no)
      --- For GTIN support
      UNION

      SELECT DISTINCT
       i.concatenated_segments,
       i.description,
       d.line_no,
       i.inventory_item_id,
       d.material_detail_id,
       NVL(i.lot_control_code, 1),
       NVL(i.location_control_code, 1),
       'Y', --- gtin_entered_ind
       NVL(uom_code, ' '), --- GTIN UOM code
       NVL(i.restrict_locators_code, 2),
       NVL(i.grade_control_flag, 'N'),
       NVL(i.lot_status_enabled, 'N'),
       NVL(i.lot_divisible_flag, 'N')
      FROM
           gme_material_details d,
           mtl_cross_references mcr,
           mtl_system_items_kfv i
      WHERE d.batch_id                    = p_batch_id
      AND d.inventory_item_id             = i.inventory_item_id
      AND d.organization_id               = i.organization_id
      AND d.line_no                       = NVL(p_line_no, d.line_no)
      AND d.line_type                     = p_line_type
      AND i.inventory_item_id             = mcr.inventory_item_id
      AND i.mtl_transactions_enabled_flag = 'Y'
      AND d.release_type                  IN (1,2) --- Manual/Incremental
      AND    mcr.cross_reference_type     = g_gtin_cross_ref_type
      AND    mcr.cross_reference          LIKE l_cross_ref
      AND    (mcr.organization_id         = i.organization_id
           OR
             mcr.org_independent_flag = 'Y');

    ELSE

      OPEN x_item_cursor FOR

      SELECT DISTINCT
       i.concatenated_segments,
       i.description,
       d.line_no,
       i.inventory_item_id,
       d.material_detail_id,
       NVL(i.lot_control_code, 1),
       NVL(i.location_control_code, 1),
       'N', --- gtin_entered_ind
       NULL, --- GTIN UOM code
       NVL(i.restrict_locators_code, 2),
       NVL(i.grade_control_flag, 'N'),
       NVL(i.lot_status_enabled, 'N'),
       NVL(i.lot_divisible_flag, 'N')
      FROM gme_material_details d,
           gme_batch_step_items si,
           gme_batch_steps      s,
           mtl_system_items_kfv i
      WHERE d.batch_id                    = p_batch_id
      AND d.batch_id                      = si.batch_id
      AND si.material_detail_id           = d.material_detail_id
      AND s.batchstep_no                  = p_step_no
      AND si.batchstep_id                 = s.batchstep_id
      AND d.inventory_item_id             = i.inventory_item_id
      AND d.organization_id               = i.organization_id
      AND d.line_no                       = NVL(p_line_no, d.line_no)
      AND d.line_type                     = p_line_type
      AND d.release_type                  IN (1,2) --- Manual/Incremental
      AND i.mtl_transactions_enabled_flag = 'Y'
      AND i.concatenated_segments         LIKE (p_item_no)

      --- For GTIN support
      UNION

      SELECT DISTINCT
       i.concatenated_segments,
       i.description,
       d.line_no,
       i.inventory_item_id,
       d.material_detail_id,
       NVL(i.lot_control_code, 1),
       NVL(i.location_control_code, 1),
       'Y', --- gtin_entered_ind
       NVL(uom_code, ' '), --- GTIN UOM code
       NVL(i.restrict_locators_code, 2),
       NVL(i.grade_control_flag, 'N'),
       NVL(i.lot_status_enabled, 'N'),
       NVL(i.lot_divisible_flag, 'N')
      FROM
           gme_material_details d,
           mtl_cross_references mcr,
           gme_batch_step_items si,
           gme_batch_steps      s,
           mtl_system_items_kfv i
      WHERE d.batch_id                    = p_batch_id
      AND d.batch_id                      = si.batch_id
      AND si.material_detail_id           = d.material_detail_id
      AND s.batchstep_no                  = p_step_no
      AND si.batchstep_id                 = s.batchstep_id
      AND d.inventory_item_id             = i.inventory_item_id
      AND d.organization_id               = i.organization_id
      AND d.line_no                       = NVL(p_line_no, d.line_no)
      AND d.line_type                     = p_line_type
      AND i.inventory_item_id             = mcr.inventory_item_id
      AND d.release_type                  IN (1,2) --- Manual/Incremental
      AND i.mtl_transactions_enabled_flag = 'Y'
      AND    mcr.cross_reference_type     = g_gtin_cross_ref_type
      AND    mcr.cross_reference          LIKE l_cross_ref
      AND    (mcr.organization_id         = i.organization_id
           OR
             mcr.org_independent_flag = 'Y');
    END IF;

  EXCEPTION
    WHEN FND_API.G_EXC_UNEXPECTED_ERROR
    THEN
      NULL;
  END Material_Step_For_IB_LoV;

 /*+========================================================================+
   | PROCEDURE NAME
   |   Step_For_Resource_LoV
   |
   | USAGE
   |
   | ARGUMENTS
   |   p_batch_id
   |   p_step_no
   |
   | RETURNS
   |   REF cursor x_step_cursor
   |
   | HISTORY
   |   Created  26-Apr-05 Eddie Oumerretane
   |            8-Jul-14  Shaliu Chen ER 19161894
                          Add some conditions to filter OSP step out
   +========================================================================+*/
  PROCEDURE Step_For_Resource_LoV
  (  x_step_cursor     OUT NOCOPY t_genref
  ,  p_batch_id        IN  NUMBER
  ,  p_step_no         IN  VARCHAR2
  )
  IS
  BEGIN
    OPEN x_step_cursor FOR
       SELECT s.batchstep_no,
              o.oprn_desc,
              s.batchstep_id
       FROM gme_batch_header h,
            gme_batch_steps s,
            gmd_operations o
       WHERE h.batch_id = p_batch_id
       AND h.batch_type = 0
       AND h.batch_id = s.batch_id
       AND automatic_step_calculation = 0
       --- Bug 5236930 AND to_char(s.batchstep_no) LIKE  LTRIM(RTRIM(p_step_no||'%'))
       AND to_char(s.batchstep_no) LIKE  p_step_no
       AND s.oprn_id = o.oprn_id
       AND s.step_status in (2,3)
       /*ER 19161894  Shaliu Chen 18-JUL-2014*/
       AND NOT EXISTS (
                           SELECT 'OSP Resource Exist'
                             FROM gme_batch_step_resources gbsr,
                                  cr_rsrc_mst crm,
                                  cr_rsrc_dtl crd
                            WHERE gbsr.batchstep_id = s.batchstep_id
                              AND gbsr.resources = crd.resources
                              AND gbsr.organization_id = crd.organization_id
                              AND crd.purchase_item_id IS NOT NULL
                              AND crd.resources = crm.resources
                              AND NVL(crm.outside_process_ind,gme_osp.g_osp_ind_disabled) = gme_osp.g_osp_ind_enabled)

       UNION
       SELECT s.batchstep_no,
              o.oprn_desc,
              s.batchstep_id
       FROM gme_batch_header h,
            gme_batch_steps s,
            gmd_operations o
       WHERE h.batch_id = p_batch_id
       AND h.batch_type = 0
       AND h.batch_id = s.batch_id
       AND automatic_step_calculation = 1
       --- Bug 5236930 AND to_char(s.batchstep_no) LIKE  LTRIM(RTRIM(p_step_no||'%'))
       AND to_char(s.batchstep_no) LIKE  p_step_no
       AND s.oprn_id = o.oprn_id
       AND s.step_status = 3
       /*ER 19161894  Shaliu Chen 18-JUL-2014*/
       AND NOT EXISTS (
                         SELECT 'OSP Resource Exist'
                           FROM gme_batch_step_resources gbsr,
                                cr_rsrc_mst crm,
                                cr_rsrc_dtl crd
                          WHERE gbsr.batchstep_id = s.batchstep_id
                            AND gbsr.resources = crd.resources
                            AND gbsr.organization_id = crd.organization_id
                            AND crd.purchase_item_id IS NOT NULL
                            AND crd.resources = crm.resources
                            AND NVL(crm.outside_process_ind,gme_osp.g_osp_ind_disabled) = gme_osp.g_osp_ind_enabled)
       ORDER BY 1;

  END Step_For_Resource_LoV;

 /*+========================================================================+
   | PROCEDURE NAME
   |   Step_Material_Line_LoV
   |
   | USAGE
   |
   | ARGUMENTS
   |   p_organization_id
   |   p_subinv_code
   |
   | RETURNS
   |   REF cursor x_sub_lov
   |
   | HISTORY
   |   Created  26-Apr-05 Eddie Oumerretane
   |
   +========================================================================+*/
  PROCEDURE Subinventory_Lov(x_sub_lov         OUT NOCOPY t_genref,
                             p_organization_id IN  NUMBER,
                             p_subinv_code     IN  VARCHAR,
                             p_item_id         IN  NUMBER,
                             p_restrict_subinv IN  NUMBER) IS
  BEGIN
   --Bug#5867209 added the following IF condition
    IF p_restrict_subinv = 1 THEN
     OPEN x_sub_lov FOR
      SELECT s.secondary_inventory_name
           , s.description
           , NVL(s.locator_type, 1)
        FROM mtl_secondary_inventories s,
             mtl_item_sub_inventories i
       WHERE s.secondary_inventory_name = i.secondary_inventory
         AND s.organization_id = i.organization_id
         AND s.organization_id = p_organization_id
         AND i.inventory_item_id = p_item_id
         AND NVL(s.disable_date, TRUNC(SYSDATE + 1)) > TRUNC(SYSDATE)
         AND s.secondary_inventory_name LIKE (p_subinv_code)
         AND s.quantity_tracked = 1
       ORDER BY s.secondary_inventory_name;
    ELSE
     OPEN x_sub_lov FOR
      SELECT secondary_inventory_name
           , description
           , NVL(locator_type, 1)
        FROM mtl_secondary_inventories
       WHERE organization_id = p_organization_id
         AND NVL(disable_date, TRUNC(SYSDATE + 1)) > TRUNC(SYSDATE)
         AND secondary_inventory_name LIKE (p_subinv_code)
         AND quantity_tracked = 1
       ORDER BY secondary_inventory_name;
    END IF;

  END Subinventory_Lov;


 /* Bug#5663458
  * Created the following procedure. This procedure is to get subinventoried that have
  * on hand. If the GME parameters is not set we call master LoV directly
  */
  PROCEDURE Subinventory_OnHand_LoV(
    x_sub_lov                OUT    NOCOPY t_genref
  , p_organization_id        IN     NUMBER
  , p_subinventory_code      IN     VARCHAR2
  , p_inventory_item_id      IN     NUMBER
  , p_revision               IN     VARCHAR2
  , p_restrict_subinv        IN     NUMBER
  )
  IS
   CURSOR c_get_locind IS
     SELECT subinv_loc_ind
       FROM gme_parameters
      WHERE organization_id = p_organization_id;

   l_ind NUMBER(1) := 0;
  BEGIN

   OPEN c_get_locind;
   FETCH c_get_locind INTO l_ind;
   CLOSE c_get_locind;

   IF l_ind = 1 THEN
    OPEN x_sub_lov FOR
      SELECT DISTINCT sub.secondary_inventory_name, sub.description,
             NVL(sub.locator_type, 1)
        FROM mtl_secondary_inventories sub, mtl_onhand_sub_v onh
       WHERE sub.organization_id = onh.organization_id
         AND sub.secondary_inventory_name = onh.subinventory_code
         AND NVL(sub.disable_date, TRUNC(SYSDATE + 1)) > TRUNC(SYSDATE)
         AND sub.quantity_tracked = 1
         AND sub.organization_id = p_organization_id
         AND onh.inventory_item_id = p_inventory_item_id
         AND (p_revision IS NULL OR onh.revision = p_revision)
         AND secondary_inventory_name LIKE (p_subinventory_code)
         AND total_qoh > 0;
   ELSE
    Subinventory_Lov(x_sub_lov         => x_sub_lov,
                    p_organization_id => p_organization_id,
                    p_subinv_code     => p_subinventory_code,
                    p_item_id         => p_inventory_item_id,
                    p_restrict_subinv => p_restrict_subinv);
   END IF;
  END Subinventory_OnHand_LoV;


 /*+========================================================================+
   | PROCEDURE NAME
   |   Subinventory_LoV_Dispense
   |
   | USAGE
   |
   | ARGUMENTS
   |   p_organization_id
   |   p_subinv_code
   |
   | RETURNS
   |   REF cursor x_sub_lov
   |
   | HISTORY
   |   Created  26-Apr-05 Eddie Oumerretane
   |
   +========================================================================+*/
  PROCEDURE Subinventory_LoV_Dispense(x_sub_lov         OUT NOCOPY t_genref,
                                      p_organization_id IN NUMBER,
                                      p_subinv_code     IN VARCHAR) IS
  BEGIN

    OPEN x_sub_lov FOR
      SELECT DISTINCT s.secondary_inventory_name
           , s.description
           , NVL(s.locator_type, 1)
           , NVL (b.concatenated_segments, ' ')
           ---,rtrim(substr(INV_UTILITIES.get_conc_segments(p_organization_id, d.locator_id),1,255)) locator
           ,d.lot_number
           ,d.dispensed_qty
           ,NVL(d.secondary_dispensed_qty,0)
           ,d.dispense_uom
           , dispense_id
           ,d.revision
        FROM mtl_secondary_inventories s,
             wms_item_locations_kfv b,
             gme_material_dispensing_gtmp d
       WHERE
             d.subinventory_code LIKE (p_subinv_code)
         AND s.secondary_inventory_name = d.subinventory_code
         AND s.organization_id = p_organization_id
         AND d.locator_id = b.inventory_location_id(+)
         AND b.organization_id (+) = p_organization_id
       ORDER BY s.secondary_inventory_name;

  END Subinventory_LoV_Dispense;

 /*+========================================================================+
   | PROCEDURE NAME
   |   Subinventory_LoV_Rsrv
   |
   | USAGE
   |
   | ARGUMENTS
   |   p_organization_id
   |   p_batch_id
   |   p_material_detail_id
   |   p_item_id
   |   p_subinventory_code
   |   p_locator_id
   |   p_lot_number
   |
   | RETURNS
   |   REF cursor x_sub_lov
   |
   | HISTORY
   |   Created  26-Apr-05 Eddie Oumerretane
   |
   +========================================================================+*/
  PROCEDURE Subinventory_LoV_Rsrv(
    x_sub_lov             OUT    NOCOPY t_genref
  , p_organization_id     IN     NUMBER
  , p_batch_id            IN     NUMBER
  , p_material_detail_id  IN     NUMBER
  , p_item_id             IN     NUMBER
  , p_subinventory_code   IN     VARCHAR2
  , p_locator_id          IN     NUMBER
  , p_lot_number          IN     VARCHAR2
  ) IS

  BEGIN

   OPEN x_sub_lov FOR
      SELECT DISTINCT msi.secondary_inventory_name
           , msi.description
           , NVL(msi.locator_type, 1)
      FROM   mtl_secondary_inventories msi,
             mtl_reservations mr
      WHERE  mr.organization_id = p_organization_id
             AND mr.inventory_item_id = p_item_id
             AND NVL(mr.lot_number,0) = NVL(NVL(p_lot_number, mr.lot_number),0)
             AND demand_source_type_id = gme_common_pvt.g_txn_source_type
             AND demand_source_header_id = p_batch_id
             AND NVL(demand_source_line_id, -1) = NVL(p_material_detail_id, -1)
             AND subinventory_code LIKE (p_subinventory_code)
             AND NVL(mr.locator_id, -1) = NVL(NVL(p_locator_id, mr.locator_id), -1)
             AND subinventory_code = msi.secondary_inventory_name
             AND msi.organization_id = p_organization_id;

  END Subinventory_LoV_Rsrv;



 /* Bug#5663458
  * Created the following procedure. This procedure is to get subinventories from
  * DLR of material line.
  */
  PROCEDURE Ing_Subinventory_LoV_Rsrv(
    x_sub_lov             OUT    NOCOPY t_genref
  , p_organization_id     IN     NUMBER
  , p_batch_id            IN     NUMBER
  , p_material_detail_id  IN     NUMBER
  , p_item_id             IN     NUMBER
  , p_subinventory_code   IN     VARCHAR2
  ) IS

  BEGIN

   OPEN x_sub_lov FOR
      SELECT DISTINCT msi.secondary_inventory_name
           , msi.description
           , NVL(msi.locator_type, 1)
           , mr.revision
      FROM   mtl_secondary_inventories msi,
             mtl_reservations mr
      WHERE  mr.subinventory_code = msi.secondary_inventory_name
             AND mr.organization_id = p_organization_id
             AND mr.inventory_item_id = p_item_id
             AND demand_source_header_id = p_batch_id
             AND demand_source_type_id = gme_common_pvt.g_txn_source_type
             AND demand_source_line_id = p_material_detail_id
             AND subinventory_code LIKE (p_subinventory_code)
             AND msi.organization_id = p_organization_id;

  END Ing_Subinventory_LoV_Rsrv;


 /*+========================================================================+
   | PROCEDURE NAME
   |   Subinventory_Lov_Temp_Rsrv
   |
   | USAGE
   |
   | ARGUMENTS
   |   p_organization_id
   |   p_item_id
   |   p_revision
   |   p_lot_number
   |   p_subinv_code
   |
   | RETURNS
   |   REF cursor x_sub_lov
   |
   | HISTORY
   |   Created  26-Apr-05 Eddie Oumerretane
   |
   +========================================================================+*/
  PROCEDURE Subinventory_Lov_Temp_Rsrv(x_sub_lov    OUT NOCOPY t_genref,
                             p_organization_id IN NUMBER,
                             p_item_id         IN NUMBER,
                             p_revision        IN VARCHAR2,
                             p_lot_number      IN VARCHAR2,
                             p_subinv_code     IN VARCHAR) IS
  BEGIN

    OPEN x_sub_lov FOR

      SELECT t.subinventory_code,
             msi.description,
             NVL(msi.locator_type, 1),
             t.atr
      FROM   mtl_secondary_inventories msi,
             mtl_rsv_quantities_temp  t
      WHERE node_level = 4
       AND    t.organization_id = p_organization_id
       AND    t.inventory_item_id = p_item_id
       AND    nvl(t.revision,1) = nvl(p_revision, 1)
       AND    nvl(t.lot_number,0) = nvl(p_lot_number, 0)
       AND    t.subinventory_code LIKE (p_subinv_code)
       AND    t.subinventory_code = msi.secondary_inventory_name
       AND    msi.organization_id = p_organization_id
       AND    msi.quantity_tracked = 1
       AND    msi.reservable_type=1
      ORDER BY t.subinventory_code, msi.description;

  END Subinventory_Lov_Temp_Rsrv;

 /*+========================================================================+
   | PROCEDURE NAME
   |   Locator_LoV
   |
   | USAGE
   |
   | ARGUMENTS
   |   p_organization_id
   |   p_subinventory_code
   |   p_subinventory_item_id
   |   p_concatenated_segments
   |   p_restrict_locators
   |
   | RETURNS
   |   REF cursor x_locators
   |
   | HISTORY
   |   Created  26-Apr-05 Eddie Oumerretane
   |   Namit S. Bug4917215 01Feb06. Changed queries to dynamic sql.
   |
   +========================================================================+*/
  PROCEDURE Locator_LoV(
    x_locators               OUT    NOCOPY t_genref
  , p_organization_id        IN     NUMBER
  , p_subinventory_code      IN     VARCHAR2
  , p_inventory_item_id      IN     NUMBER
  , p_concatenated_segments  IN     VARCHAR2
  , p_restrict_locators      IN     NUMBER
  ) IS

  sqlstmt VARCHAR2(1000);

  BEGIN

-- Namit S. Bug4917215 01Feb06. Changed both queries to dynamic SQL to improve perf.

    IF p_restrict_locators = 1 THEN

      sqlstmt := ' SELECT a.concatenated_segments, '
             ||' a.description, '
             ||' a.inventory_location_id '
      ||' FROM  wms_item_locations_kfv a, '
             ||' mtl_secondary_locators b '
      ||' WHERE b.organization_id = :p_organization_Id '
             ||' AND   b.inventory_item_id = :p_Inventory_Item_Id '
             ||' AND   b.subinventory_code = :p_Subinventory_Code '
             ||' AND   a.inventory_location_id = b.secondary_locator '
             ||' and nvl(a.disable_date, trunc(SYSDATE+1)) > trunc(SYSDATE) '
             ||' AND   a.concatenated_segments LIKE (:p_concatenated_segments ) '
      ||' ORDER BY 1 ';
      OPEN x_Locators FOR sqlstmt USING p_organization_id, p_Inventory_Item_Id, p_Subinventory_Code, p_concatenated_segments;

    ELSE

      sqlstmt := ' SELECT concatenated_segments, '
             ||' description, '
             ||' inventory_location_id '
      ||' FROM wms_item_locations_kfv '
      ||' WHERE organization_id = :p_organization_id '
             ||' AND subinventory_code = :p_subinventory_code '
             ||' AND NVL(disable_date, TRUNC(SYSDATE + 1)) > TRUNC(SYSDATE) '
             ||' AND concatenated_segments LIKE (:p_concatenated_segments) '
      ||' ORDER BY 1 ';

      OPEN x_Locators FOR sqlstmt USING p_organization_id, p_Subinventory_Code, p_concatenated_segments;

    END IF;

  END Locator_LoV;

 /* Bug#5663458
  * Created the following procedure. This procedure is to get locators that have
  * on hand. If the GME parameters is not set we call master LoV directly
  */
  PROCEDURE Locator_OnHand_LoV(
    x_locators               OUT    NOCOPY t_genref
  , p_organization_id        IN     NUMBER
  , p_subinventory_code      IN     VARCHAR2
  , p_inventory_item_id      IN     NUMBER
  , p_locator                IN     VARCHAR2
  , p_revision               IN     VARCHAR2
  , p_restrict_locators      IN     NUMBER
  )
  IS
   CURSOR c_get_locind IS
     SELECT subinv_loc_ind
       FROM gme_parameters
      WHERE organization_id = p_organization_id;

   l_ind NUMBER(1) := 0;
  BEGIN
   OPEN c_get_locind;
   FETCH c_get_locind INTO l_ind;
   CLOSE c_get_locind;

   IF l_ind = 1 THEN
    OPEN x_locators FOR
     SELECT DISTINCT loc.concatenated_segments locator, loc.description,
            onh.locator_id
       FROM wms_item_locations_kfv loc, mtl_onhand_locator_v onh
      WHERE loc.organization_id = onh.organization_id
        AND loc.inventory_location_id = onh.locator_id
        AND NVL(loc.disable_date, TRUNC(SYSDATE + 1)) > TRUNC(SYSDATE)
        AND loc.organization_id = p_organization_id
        AND onh.inventory_item_id = p_inventory_item_id
        AND (p_revision IS NULL OR onh.revision = p_revision)
        AND onh.subinventory_code = p_subinventory_code
        AND concatenated_segments LIKE (p_locator)
        AND onh.total_qoh > 0 ;
   ELSE
     Locator_LoV(
          x_locators          => x_locators
        , p_organization_id   => p_organization_id
        , p_subinventory_code => p_subinventory_code
        , p_inventory_item_id  => p_inventory_item_id
        , p_concatenated_segments => p_locator
        , p_restrict_locators     => p_restrict_locators
       );
   END IF;
  END Locator_OnHand_LoV;

 /*+========================================================================+
   | PROCEDURE NAME
   |   Locator_LoV_Rsrv
   |
   | USAGE
   |
   | ARGUMENTS
   |   p_organization_id
   |   p_batch_id
   |   p_material_detail_id
   |   p_item_id
   |   p_subinventory_code
   |   p_locator
   |   p_lot_number
   |
   | RETURNS
   |   REF cursor x_locators
   |
   | HISTORY
   |   Created  26-Apr-05 Eddie Oumerretane
   |
   +========================================================================+*/
  PROCEDURE Locator_LoV_Rsrv(
    x_locators               OUT    NOCOPY t_genref
  , p_organization_id     IN     NUMBER
  , p_batch_id            IN     NUMBER
  , p_material_detail_id  IN     NUMBER
  , p_item_id             IN     NUMBER
  , p_subinventory_code   IN     VARCHAR2
  , p_locator             IN     VARCHAR2
  , p_lot_number          IN     VARCHAR2
  ) IS
  BEGIN

    OPEN x_Locators FOR
      SELECT a.concatenated_segments,
             a.description,
             a.inventory_location_id
      FROM  wms_item_locations_kfv a,
            mtl_reservations mr
      WHERE  mr.organization_id = p_organization_id
             AND mr.inventory_item_id = p_item_id
             AND NVL(mr.lot_number,0) = NVL(NVL(p_lot_number, mr.lot_number),0)
             AND mr.demand_source_type_id = gme_common_pvt.g_txn_source_type
             AND mr.demand_source_header_id = p_batch_id
             AND NVL(demand_source_line_id, -1) = NVL(p_material_detail_id, -1)
             AND mr.subinventory_code = p_subinventory_code
             AND a.concatenated_segments LIKE (p_locator)
             AND mr.locator_id = a.inventory_location_id
      ORDER BY 1;

  END Locator_LoV_Rsrv;



 /* Bug#5663458
  * Created the following procedure. This procedure is to get the locators
  * from DLR of selected material line.
  */
  PROCEDURE Ing_Locator_LoV_Rsrv(
    x_locators            OUT    NOCOPY t_genref
  , p_organization_id     IN     NUMBER
  , p_batch_id            IN     NUMBER
  , p_material_detail_id  IN     NUMBER
  , p_item_id             IN     NUMBER
  , p_subinventory_code   IN     VARCHAR2
  , p_locator             IN     VARCHAR2
  ) IS
  BEGIN

    OPEN x_Locators FOR
      SELECT a.concatenated_segments,
             a.description,
             a.inventory_location_id
      FROM  wms_item_locations_kfv a,
            mtl_reservations mr
      WHERE   mr.locator_id = a.inventory_location_id
             AND mr.organization_id = p_organization_id
             AND mr.inventory_item_id = p_item_id
             AND mr.demand_source_type_id = gme_common_pvt.g_txn_source_type
             AND mr.demand_source_header_id = p_batch_id
             AND demand_source_line_id = p_material_detail_id
             AND mr.subinventory_code = p_subinventory_code
             AND a.concatenated_segments LIKE (p_locator)
      ORDER BY 1;

  END Ing_Locator_LoV_Rsrv;

 /*+========================================================================+
   | PROCEDURE NAME
   |   Locator_LoV_Dispense
   |
   | USAGE
   |
   | ARGUMENTS
   |   p_subinv_code
   |   p_locator_code
   |
   | RETURNS
   |   REF cursor x_locator_lov
   |
   | HISTORY
   |   Created  26-Apr-05 Eddie Oumerretane
   |   Namit S. Bug4917215 01Feb06. Changed queries to dynamic sql.
   |
   +========================================================================+*/
  PROCEDURE Locator_LoV_Dispense(
    x_locator_lov         OUT    NOCOPY t_genref
  , p_subinv_code         IN     VARCHAR2
  , p_locator_code        IN     VARCHAR2
  ) IS
  sqlstmt VARCHAR2(1000);

  BEGIN

-- Namit S. Bug4917215 01Feb06. Changed to dynamic SQL to improve perf.
    sqlstmt :=  ' SELECT a.concatenated_segments '
             ||' ,a.description '
             ||' ,a.inventory_location_id '
             ||' ,t.lot_number '
             ||' ,t.dispensed_qty '
             ||' ,NVL(t.secondary_dispensed_qty, 0) '
             ||' ,dispense_uom '
             ||' ,dispense_id '
             ||' ,t.revision '
      ||' FROM  wms_item_locations_kfv a, '
             ||' gme_material_dispensing_gtmp t '
      ||' WHERE '
             ||' t.subinventory_code   = :p_subinv_code '
             ||' AND a.subinventory_code   = t.subinventory_code '
             ||' AND a.concatenated_segments LIKE (:p_locator_code) '
             ||' AND a.inventory_location_id = t.locator_id ';

    OPEN x_locator_lov FOR sqlstmt USING p_subinv_code, p_locator_code;

  END Locator_LoV_Dispense;

 /*+========================================================================+
   | PROCEDURE NAME
   |   Locator_LoV_Temp_Rsrv
   |
   | USAGE
   |
   | ARGUMENTS
   |   p_lot_number
   |   p_organization_id
   |   p_item_id
   |   p_revision
   |   p_subinv_code
   |   p_locator_code
   |
   | RETURNS
   |   REF cursor x_locator_lov
   |
   | HISTORY
   |   Created  26-Apr-05 Eddie Oumerretane
   |
   +========================================================================+*/
  PROCEDURE Locator_LoV_Temp_Rsrv(
    x_locator_lov         OUT    NOCOPY t_genref
  , p_lot_number          IN     VARCHAR2
  , p_organization_id     IN     NUMBER
  , p_item_id             IN     NUMBER
  , p_revision            IN     VARCHAR2
  , p_subinv_code         IN     VARCHAR2
  , p_locator_code        IN     VARCHAR2
  ) IS


  BEGIN

    OPEN x_locator_lov FOR
      SELECT a.concatenated_segments,
             a.description,
             a.inventory_location_id,
            ROUND(SUM(atr),4) atr
      FROM  wms_item_locations_kfv a,
            mtl_rsv_quantities_temp t
     WHERE ((t.node_level = 4
     AND t.subinventory_code not in (select subinventory_code from
                     mtl_rsv_quantities_temp where node_level = 5)) OR
                     (t.node_level = 5))
     AND t.organization_id   = p_organization_id
     AND t.inventory_item_id = p_item_id
     AND nvl(t.revision, 1)  = nvl(p_revision, 1)
     AND t.subinventory_code   = p_subinv_code
     AND a.concatenated_segments LIKE (p_locator_code)
     AND a.inventory_location_id = locator_id
     AND nvl(t.lot_number,0) = nvl(p_lot_number, 0)
     GROUP BY a.concatenated_segments,
              a.description,
              a.inventory_location_id;

  END Locator_LoV_Temp_Rsrv;

 /*+========================================================================+
   | PROCEDURE NAME
   |   Lot_LoV_Rsrv
   |
   | USAGE
   |
   | ARGUMENTS
   |   p_lot_number
   |   p_organization_id
   |   p_batch_id
   |   p_material_detail_id
   |   p_item_id
   |   p_subinventory_code
   |   p_locator_id
   |
   | RETURNS
   |   REF cursor x_lot_num_lov
   |
   | HISTORY
   |   Created  26-Apr-05 Eddie Oumerretane
   |
   +========================================================================+*/
  PROCEDURE Lot_LoV_Rsrv(
    x_lot_num_lov         OUT    NOCOPY t_genref
  , p_lot_number          IN     VARCHAR2
  , p_organization_id     IN     NUMBER
  , p_batch_id            IN     NUMBER
  , p_material_detail_id  IN     NUMBER
  , p_item_id             IN     NUMBER
  , p_subinventory_code   IN     VARCHAR2
  , p_locator_id          IN     NUMBER
  ) IS

   l_date_format VARCHAR2(100);

  BEGIN

  FND_PROFILE.GET('MWA_DATE_FORMAT_MASK',l_date_format);

  IF l_date_format IS NULL THEN
     FND_PROFILE.GET('ICX_DATE_FORMAT_MASK',l_date_format);
  END IF;

  IF p_subinventory_code IS NOT NULL THEN

   OPEN x_lot_num_lov FOR
      SELECT DISTINCT mln.lot_number
         i, TO_CHAR(mln.expiration_date, l_date_format)
         , mln.grade_code
         , mln.parent_lot_number
      FROM   mtl_reservations mr,
             mtl_lot_numbers mln
      WHERE  mr.organization_id = p_organization_id
             AND mr.organization_id = mln.organization_id
             AND mr.inventory_item_id = p_item_id
             AND mr.inventory_item_id = mln.inventory_item_id
             AND mr.lot_number LIKE (p_lot_number)
             AND mr.lot_number = mln.lot_number
             AND demand_source_type_id = gme_common_pvt.g_txn_source_type
             AND demand_source_header_id = p_batch_id
             AND NVL(demand_source_line_id, -1) = NVL(p_material_detail_id, -1)
             AND subinventory_code = p_subinventory_code
             AND NVL(mr.locator_id, -1) = NVL(NVL(p_locator_id, mr.locator_id), -1)
             AND NOT EXISTS (SELECT 1
                             FROM   mtl_material_transactions_temp
                             WHERE  reservation_id = mr.reservation_id);
  ELSE

   OPEN x_lot_num_lov FOR
      SELECT DISTINCT mln.lot_number
         i, TO_CHAR(mln.expiration_date, l_date_format)
         , mln.grade_code
         , mln.parent_lot_number
      FROM   mtl_reservations mr,
             mtl_lot_numbers mln
      WHERE  mr.organization_id = p_organization_id
             AND mr.organization_id = mln.organization_id
             AND mr.inventory_item_id = p_item_id
             AND mr.inventory_item_id = mln.inventory_item_id
             AND mr.lot_number LIKE (p_lot_number)
             AND mr.lot_number = mln.lot_number
             AND demand_source_type_id = gme_common_pvt.g_txn_source_type
             AND demand_source_header_id = p_batch_id
             AND NVL(demand_source_line_id, -1) = NVL(p_material_detail_id, -1)
             AND NOT EXISTS (SELECT 1
                             FROM   mtl_material_transactions_temp
                             WHERE  reservation_id = mr.reservation_id);
   END IF;

  END Lot_LoV_Rsrv;

 /*+========================================================================+
   | PROCEDURE NAME
   |   Lot_LoV_Txn
   |
   | USAGE
   |
   | ARGUMENTS
   |   p_lot_number
   |   p_organization_id
   |   p_batch_id
   |   p_material_detail_id
   |
   | RETURNS
   |   REF cursor x_lot_num_lov
   |
   | HISTORY
   |   Created  26-Apr-05 Eddie Oumerretane
   |
   +========================================================================+*/
  PROCEDURE Lot_LoV_Txn(
    x_lot_num_lov         OUT    NOCOPY t_genref
  , p_lot_number          IN     VARCHAR2
  , p_organization_id     IN     NUMBER
  , p_batch_id            IN     NUMBER
  , p_material_detail_id  IN     NUMBER
  , p_revision            IN     VARCHAR2
  , p_line_type           IN     NUMBER
  ) IS
  BEGIN

    OPEN x_lot_num_lov FOR
      SELECT l.lot_num, ABS(l.txn_qty),ABS(l.txn_sec_qty)
      FROM
         (SELECT   m2.revision,
                   l2.lot_number lot_num,
                   SUM(l2.primary_quantity) txn_qty,
                   SUM(NVL(l2.secondary_transaction_quantity,0)) txn_sec_qty
          FROM mtl_material_transactions m2,
                 mtl_transaction_lot_numbers l2
          WHERE l2.transaction_id = m2.transaction_id
            AND l2.lot_number LIKE (p_lot_number)
            AND m2.organization_id = p_organization_id
            AND m2.transaction_source_id = p_batch_id
            AND m2.trx_source_line_id = p_material_detail_id
            AND m2.transaction_source_type_id = gme_common_pvt.g_txn_source_type
            AND (p_revision IS NULL OR m2.revision = p_revision)
            GROUP BY m2.revision, l2.lot_number) l
     WHERE (p_line_type = -1 and l.txn_qty < 0) OR
           (p_line_type IN (1,2) and l.txn_qty > 0);

  END Lot_Lov_Txn;

 /*+========================================================================+
   | PROCEDURE NAME
   |   Lot_Onhand_LoV
   |
   | USAGE
   |
   | ARGUMENTS
   |   p_lot_number
   |   p_organization_id
   |   p_item_id
   |   p_subinventory_code
   |   p_locator_id
   |
   | RETURNS
   |   REF cursor x_lot_num_lov
   |
   | HISTORY
   |   Created  26-Apr-05 Eddie Oumerretane
   |
   +========================================================================+*/
  PROCEDURE Lot_OnHand_LoV(
    x_lot_num_lov         OUT    NOCOPY t_genref
  , p_lot_number          IN     VARCHAR2
  , p_organization_id     IN     NUMBER
  , p_item_id             IN     NUMBER
  , p_subinventory_code   IN     VARCHAR2
  , p_locator_id          IN     NUMBER
  ) IS

   l_date_format VARCHAR2(100);

  BEGIN

    FND_PROFILE.GET('MWA_DATE_FORMAT_MASK',l_date_format);

    IF l_date_format IS NULL THEN
      FND_PROFILE.GET('ICX_DATE_FORMAT_MASK',l_date_format);
    END IF;

  IF p_subinventory_code IS NULL THEN

    OPEN x_lot_num_lov FOR
    SELECT DISTINCT mln.lot_number
   , TO_CHAR(mln.expiration_date, l_date_format)
   , mln.grade_code
   , mln.parent_lot_number
   FROM mtl_lot_numbers mln
   WHERE mln.organization_id = p_organization_id
   AND mln.inventory_item_id = p_item_id
   AND mln.lot_number LIKE (p_lot_number)
   AND exists (SELECT '1' FROM mtl_onhand_quantities_detail moqd
        WHERE moqd.lot_number = mln.lot_number
        AND moqd.inventory_item_id = mln.inventory_item_id
        AND moqd.organization_id = mln.organization_id);

 ELSE

   OPEN x_lot_num_lov FOR
    SELECT DISTINCT mln.lot_number
   , TO_CHAR(mln.expiration_date, l_date_format)
   , mln.grade_code
   , mln.parent_lot_number
   FROM mtl_lot_numbers mln
   WHERE mln.organization_id = p_organization_id
   AND mln.inventory_item_id = p_item_id
   AND mln.lot_number LIKE (p_lot_number)
   AND exists (SELECT '1' FROM mtl_onhand_quantities_detail moqd
        WHERE moqd.lot_number = mln.lot_number
        AND moqd.inventory_item_id = mln.inventory_item_id
        AND moqd.organization_id = mln.organization_id
        AND moqd.subinventory_code = p_subinventory_code
        AND NVL(moqd.locator_id, -1) = NVL(NVL(p_locator_id, moqd.locator_id), -1));
  END IF;

  END Lot_OnHand_LoV;


 /*+========================================================================+
   | PROCEDURE NAME
   |   Lot_Onhand_Exp_LoV
   |
   | USAGE
   |
   | ARGUMENTS
   |   p_lot_number
   |   p_organization_id
   |   p_item_id
   |   p_subinventory_code
   |   p_locator_id
   |   p_lpn_id
   |
   | RETURNS
   |   REF cursor x_lot_num_lov
   |
   | HISTORY
   |   Created  Bug#5261131 15-Sep-06 SivakumarG

   |   17-Jun-11      Apeksha Mishra   Bug 12562057
   |      Remove explicit parameter names as it does not compile on versions below 11g.
   |   7-Jul-15      Jagadiswar Devarla   Bug 19619931
   |      Added lpn id as last parameter,in the process of adding LPN functionality to IssueIngredient page.
   +========================================================================+*/
   PROCEDURE Lot_OnHand_Exp_LoV(
    x_lot_num_lov         OUT    NOCOPY t_genref
  , p_lot_number          IN     VARCHAR2
  , p_organization_id     IN     NUMBER
  , p_item_id             IN     NUMBER
  , p_subinventory_code   IN     VARCHAR2
  , p_locator_id          IN     NUMBER
  , p_revision            IN     VARCHAR2  --Bug#5867209
  , p_transaction_type    IN     NUMBER --Bug#8937132
  , p_lpn_id              IN     NUMBER --Bug#19619931
  , p_transaction_date    IN     VARCHAR2   --Bug#19619931
  ) IS

   l_date_format VARCHAR2(100);

  BEGIN        
    FND_PROFILE.GET('MWA_DATE_FORMAT_MASK',l_date_format);

    IF l_date_format IS NULL THEN
      FND_PROFILE.GET('ICX_DATE_FORMAT_MASK',l_date_format);
    END IF;

    IF p_subinventory_code IS NULL THEN
       OPEN x_lot_num_lov FOR
        SELECT DISTINCT mln.lot_number
                      , TO_CHAR(mln.expiration_date, l_date_format)
                      , mln.grade_code
                      , mln.parent_lot_number
       FROM mtl_lot_numbers mln
      WHERE mln.organization_id = p_organization_id
        AND mln.inventory_item_id = p_item_id
        AND mln.lot_number LIKE (p_lot_number)
        AND NVL(mln.expiration_date,SYSDATE+1) > SYSDATE --Bug#5092198
        AND mln.origination_date <= TO_DATE(p_transaction_date, 'YYYY/MM/DD HH24:MI:SS') 
	/*Bug#8937132 including the check for the lot status allows the transaction or not*/
	AND (inv_material_status_grp.is_status_applicable(
                           NULL
                          ,NULL
                          ,p_transaction_type
                          ,NULL
                          ,NULL
                          ,p_organization_id
                          ,p_item_id
                          ,NULL
                          ,NULL
                          ,mln.lot_number
                          ,NULL
                          ,'O') = 'Y' )
        AND EXISTS (SELECT '1'
                    FROM mtl_onhand_quantities_detail moqd
                    WHERE moqd.lot_number = mln.lot_number
                      AND moqd.inventory_item_id = mln.inventory_item_id
                      AND moqd.organization_id = mln.organization_id
                      AND (p_revision IS NULL OR revision = p_revision)); --Bug#5867209

    ELSIF p_lpn_id IS NULL THEN  
      OPEN x_lot_num_lov FOR
       SELECT DISTINCT mln.lot_number
                     , TO_CHAR(mln.expiration_date, l_date_format)
                     , mln.grade_code
                     , mln.parent_lot_number
       FROM mtl_lot_numbers mln
      WHERE mln.organization_id = p_organization_id
        AND mln.inventory_item_id = p_item_id
        AND mln.lot_number LIKE (p_lot_number)
        AND NVL(mln.expiration_date,SYSDATE+1) > SYSDATE   --Bug#5092198
        --Bug#19619931
        /*AND  (inv_material_status_grp.is_status_applicable(
                            NULL
                           ,NULL
                           ,p_transaction_type
                           ,NULL
                           ,NULL
                           ,p_organization_id
                           ,p_item_id
                           ,p_subinventory_code
                           ,p_locator_id
                           ,mln.lot_number
                           ,NULL
                        --   ,p_lpn_id --Bug#19619931
                           ,'A') = 'Y')    */
        --Bug#19619931
        AND mln.origination_date <= TO_DATE(p_transaction_date, 'YYYY/MM/DD HH24:MI:SS') 
        AND (inv_material_status_grp.is_status_applicable(
                         p_wms_installed         => NULL
                        ,p_trx_status_enabled    => NULL
                        ,p_trx_type_id           => p_transaction_type
                        ,p_lot_status_enabled    => NULL
                        ,p_serial_status_enabled => NULL
                        ,p_organization_id       => p_organization_id
                        ,p_inventory_item_id     => p_item_id
                        ,p_sub_code              => p_subinventory_code
                        ,p_locator_id            => p_locator_id
                        ,p_lot_number            => mln.lot_number
                        ,p_serial_number         => NULL
                        ,p_object_type           => 'A'
                        ,p_lpn_id                => NULL
                        ,p_default_lot_status_id =>NULL) = 'Y')
        AND EXISTS (SELECT '1'
                    FROM mtl_onhand_quantities_detail moqd
                    WHERE moqd.lot_number = mln.lot_number
                      AND moqd.inventory_item_id = mln.inventory_item_id
                      AND moqd.organization_id = mln.organization_id
                      AND moqd.subinventory_code = p_subinventory_code
                      AND NVL(moqd.locator_id, -1) = NVL(NVL(p_locator_id, moqd.locator_id), -1)
                      AND (p_revision IS NULL OR revision = p_revision)); --Bug#5867209
                      
      ELSIF  p_lpn_id IS NOT NULL THEN         
       OPEN x_lot_num_lov FOR
        SELECT DISTINCT m.lot_number, 
               TO_CHAR(ln.expiration_date, l_date_format),               
               ln.grade_code, 
               ln.parent_lot_number                     
        FROM mtl_lot_issues_val_v m, wms_lpn_contents w, MTL_LOT_NUMBERS LN 
        WHERE m.inventory_item_id = p_item_id
        AND m.organization_id = p_organization_id        
        AND m.subinventory_code = p_subinventory_code
        AND nvl( m.revision,1) = nvl(p_revision, 1)
        AND m.locator_id = p_locator_id
        AND NVL(m.expiration_date,SYSDATE) >= SYSDATE
        AND m.inventory_item_id = LN.inventory_item_id
        AND m.organization_id = LN.organization_id
        AND m.lot_number = LN.lot_number
        AND w.inventory_item_id = LN.inventory_item_id
        AND w.lot_number = LN.lot_number
        AND w.organization_id = LN.organization_id
        AND LN.origination_date <= TO_DATE(p_transaction_date,'YYYY/MM/DD HH24:MI:SS')  
        AND (inv_material_status_grp.is_status_applicable(
                         p_wms_installed         => NULL
                        ,p_trx_status_enabled    => NULL
                        ,p_trx_type_id           => p_transaction_type
                        ,p_lot_status_enabled    => NULL
                        ,p_serial_status_enabled => NULL
                        ,p_organization_id       => p_organization_id
                        ,p_inventory_item_id     => p_item_id
                        ,p_sub_code              => p_subinventory_code
                        ,p_locator_id            => p_locator_id
                        ,p_lot_number            => m.lot_number
                        ,p_serial_number         => NULL
                        ,p_object_type           => 'A'
                        ,p_lpn_id                => p_lpn_id 
                        ,p_default_lot_status_id =>NULL) = 'Y') 
        AND w.parent_lpn_id = p_lpn_id
        AND m.lot_number = w.lot_number
        AND w.organization_id = p_organization_id
        AND w.inventory_item_id = p_item_id;    

        -- Bug 12670639 - Moved the following comment block which was causing an issue at customer site.
        /*Bug#8937132 including the check for the lot status allows the transaction or not*/

        -- Bug 10178907 - Pass in granular level data (subinventory, locator and 'A') so that logic
        -- is accurate for status check. Also mapped parameters to explicit value.
        -- Bug 12562057 - Remove explicit parameter names as it does not compile on versions below 11g.


/*
        AND  (inv_material_status_grp.is_status_applicable(
                            p_wms_installed            => NULL
                           ,p_trx_status_enabled       => NULL
                           ,p_trx_type_id              => p_transaction_type
                           ,p_lot_status_enabled       => NULL
                           ,p_serial_status_enabled    => NULL
                           ,p_organization_id          => p_organization_id
                           ,p_inventory_item_id        => p_item_id
                           ,p_sub_code                 => p_subinventory_code
                           ,p_locator_id               => p_locator_id
                           ,p_lot_number               => mln.lot_number
                           ,p_serial_number            => NULL
                           ,p_object_type              => 'A') = 'Y')
                           */

    END IF;  /*p_subinventory_code IS NULL*/
  END Lot_OnHand_Exp_LoV;

 /*+========================================================================+
   | PROCEDURE NAME
   |   Lot_LoV
   |
   | USAGE
   |
   | ARGUMENTS
   |   p_lot_number
   |   p_organization_id
   |   p_item_id
   |
   | RETURNS
   |   REF cursor x_lot_num_lov
   |
   | HISTORY
   |   Created  26-Apr-05 Eddie Oumerretane
   |   Bug#5261131 entered the expired lots check
   +========================================================================+*/
  PROCEDURE Lot_LoV(
    x_lot_num_lov         OUT    NOCOPY t_genref
  , p_lot_number          IN     VARCHAR2
  , p_organization_id     IN     NUMBER
  , p_item_id             IN     NUMBER
  ) IS

   l_date_format VARCHAR2(100);

  BEGIN

    FND_PROFILE.GET('MWA_DATE_FORMAT_MASK',l_date_format);

    IF l_date_format IS NULL THEN
      FND_PROFILE.GET('ICX_DATE_FORMAT_MASK',l_date_format);
    END IF;

    OPEN x_lot_num_lov FOR
      SELECT DISTINCT mln.lot_number
     , TO_CHAR(mln.expiration_date, l_date_format)
     , mln.grade_code
     , mln.parent_lot_number
     FROM mtl_lot_numbers mln
     WHERE mln.organization_id = p_organization_id
     AND mln.inventory_item_id = p_item_id
     AND mln.lot_number LIKE (p_lot_number)
     AND NVl(mln.expiration_date,SYSDATE+1) > SYSDATE;  --Bug#5092198

  END Lot_LoV;

 /*+========================================================================+
   | PROCEDURE NAME
   |   Lot_LoV_Dispense
   |
   | USAGE
   |
   | ARGUMENTS
   |   p_lot_number
   |   p_subinv_code
   |   p_locator_id
   |
   | RETURNS
   |   REF cursor x_lot_num_lov
   |
   | HISTORY
   |   Created  26-Apr-05 Eddie Oumerretane
   |
   +========================================================================+*/
  PROCEDURE Lot_LoV_Dispense(
    x_lot_num_lov         OUT    NOCOPY t_genref
  , p_lot_number          IN     VARCHAR2
  , p_subinv_code         IN     VARCHAR2
  , p_locator_id          IN     NUMBER
  ) IS


  BEGIN

    OPEN x_lot_num_lov FOR
     SELECT lot_number,
            dispensed_qty,
            NVL(secondary_dispensed_qty,0),
            dispense_uom,
            dispense_id,
            revision
     FROM
            GME_MATERIAL_DISPENSING_GTMP
     WHERE subinventory_code  = NVL(p_subinv_code, subinventory_code)
       AND NVL(locator_id,-1) = NVL(p_locator_id, -1)
       AND lot_number        LIKE (p_lot_number)
     ORDER BY lot_number;

  END Lot_LoV_Dispense;

 /*+========================================================================+
   | PROCEDURE NAME
   |   Lot_LoV_Dispense
   |
   | USAGE
   |
   | ARGUMENTS
   |   p_lot_number
   |   p_organization_id
   |   p_item_id
   |   p_revision
   |   p_subinv_code
   |   p_locator_id
   |
   | RETURNS
   |   REF cursor x_lot_num_lov
   |
   | HISTORY
   |   Created  26-Apr-05 Eddie Oumerretane
   |
   +========================================================================+*/
  PROCEDURE Lot_LoV_Temp_Rsrv(
    x_lot_num_lov         OUT    NOCOPY t_genref
  , p_lot_number          IN     VARCHAR2
  , p_organization_id     IN     NUMBER
  , p_item_id             IN     NUMBER
  , p_revision            IN     VARCHAR2
  , p_subinv_code         IN     VARCHAR2
  , p_locator_id          IN     NUMBER
  ) IS

   l_date_format VARCHAR2(100);

  BEGIN

    FND_PROFILE.GET('MWA_DATE_FORMAT_MASK',l_date_format);

    IF l_date_format IS NULL THEN
      FND_PROFILE.GET('ICX_DATE_FORMAT_MASK',l_date_format);
    END IF;

    OPEN x_lot_num_lov FOR
     SELECT lot_number,
            ---subinventory_code,
            ---decode(t.node_level,4,'',5,rtrim(substr(INV_UTILITIES.get_conc_segments(organization_id, locator_id),1,255)),'') locator,
            ROUND(SUM(atr),4) atr,
            grade_code
            ---locator_id
     FROM mtl_rsv_quantities_temp t
     WHERE ((t.node_level = 4
     AND subinventory_code not in (select subinventory_code from
                     mtl_rsv_quantities_temp where node_level = 5)) OR
                     (t.node_level = 5))
     AND t.organization_id   = p_organization_id
     AND t.inventory_item_id = p_item_id
     AND nvl(t.revision, 1)  = nvl(p_revision, 1)
     ---AND t.revision = nvl(p_revision, t.revision)
     ---AND subinventory_code   = NVL(p_subinv_code, subinventory_code)
     ---AND locator_id          = NVL(p_locator_id, locator_id)
     AND lot_number          LIKE (p_lot_number)
     GROUP BY lot_number, grade_code
     ORDER BY lot_number;

  END Lot_LoV_Temp_Rsrv;


  /* Bug#5663458
  * Created the following procedure. This procedure is to get all pending product lots
  * for a selected material line. Used for Use Pnd Lot field in mobile
  */
  /*BUG 25412645  Shaliu Chen 08-FEB-2016
   *Add new paramter - p_lot_number  
  */
  PROCEDURE PndLot_LoV(
    x_lot_num_lov         OUT    NOCOPY t_genref
  , p_org_id              IN     NUMBER
  , p_batch_id            IN     NUMBER
  , p_material_detail_id  IN     NUMBER
  , p_lot_number          IN     VARCHAR2
  ) IS
  BEGIN

    OPEN x_lot_num_lov FOR
      SELECT lo.sequence,
             NVL(lo.revision,' '),
	          NVL(m.subinventory,' '),
             NVl(loc.concatenated_segments,' ') locator,
             NVL(mln.parent_lot_number,' '),
             lo.lot_number,
             NVL(quantity, 0),
             lo.pending_product_lot_id,
             NVL(m.locator_id,-1)
      FROM gme_material_details m,
           gme_pending_product_lots lo,
	        wms_item_locations_kfv loc,
	        mtl_lot_numbers mln
      WHERE m.batch_id = lo.batch_id
        AND m.material_detail_id = lo.material_detail_id
        AND lo.lot_number LIKE (p_lot_number)   --BUG 25412645
        AND m.locator_id = loc.inventory_location_id(+)
	     AND m.inventory_item_id = mln.inventory_item_id
	     AND lo.lot_number = mln.lot_number
	     AND mln.organization_id = p_org_id
  	     AND m.batch_id = p_batch_id
	     AND m.material_detail_id = p_material_detail_id
     ORDER BY sequence;
  END PndLot_LoV;

 /*+========================================================================+
   | PROCEDURE NAME
   |   Pending_Lot_LoV
   |
   | USAGE
   |
   | ARGUMENTS
   |   p_lot_number
   |   p_batch_id
   |   p_material_detail_id
   |
   | RETURNS
   |   REF cursor x_lot_num_lov
   |
   | HISTORY
   |   Created  26-Apr-05 Eddie Oumerretane
   |
   +========================================================================+*/
  PROCEDURE Pending_Lot_LoV(
    x_lot_num_lov         OUT    NOCOPY t_genref
  , p_lot_number          IN     VARCHAR2
  , p_batch_id            IN     NUMBER
  , p_material_detail_id  IN     NUMBER
  ) IS
  BEGIN
    OPEN x_lot_num_lov FOR
      SELECT DISTINCT lot_number
      FROM
         GME_PENDING_PRODUCT_LOTS
      WHERE
         batch_id = p_batch_id AND
         material_detail_id = NVL(p_material_detail_id, material_detail_id) AND
         lot_number LIKE (p_lot_number)
      ORDER BY 1;

  END Pending_Lot_LoV;

 /* Bug#5663458
  * Created the following procedure to fetch the pending lots with revision
  */
  PROCEDURE Pending_Rev_Lot_LoV(
    x_lot_num_lov         OUT    NOCOPY t_genref
  , p_lot_number          IN     VARCHAR2
  , p_batch_id            IN     NUMBER
  , p_material_detail_id  IN     NUMBER
  , p_revision            IN     VARCHAR2
  ) IS
  BEGIN
    OPEN x_lot_num_lov FOR
      SELECT DISTINCT lot_number, quantity, NVL(secondary_quantity,0),
                      pending_product_lot_id
      FROM
         GME_PENDING_PRODUCT_LOTS
      WHERE
         batch_id = p_batch_id AND
         material_detail_id = NVL(p_material_detail_id, material_detail_id) AND
         lot_number LIKE (p_lot_number) AND
         (p_revision IS NULL OR revision = p_revision)
      ORDER BY 1;

  END Pending_Rev_Lot_LoV;

 /*+========================================================================+
   | PROCEDURE NAME
   |   Pending_Parent_Lot_LoV
   |
   | USAGE
   |
   | ARGUMENTS
   |   p_lot_number
   |   p_batch_id
   |   p_material_detail_id
   |
   | RETURNS
   |   REF cursor x_lot_num_lov
   |
   | HISTORY
   |   Created  21-Aug-06 Shrikant Nene
   |
   +========================================================================+*/
  PROCEDURE Pending_Parent_Lot_LoV(
    x_lot_num_lov         OUT    NOCOPY t_genref
  , p_lot_number          IN     VARCHAR2
  , p_batch_id            IN     NUMBER
  , p_material_detail_id  IN     NUMBER
  ) IS
  BEGIN

    OPEN x_lot_num_lov FOR
      SELECT lot_number
        FROM mtl_lot_numbers l, gme_material_details d
       WHERE l.inventory_item_id =  d.inventory_item_id
         AND l.organization_id = d.organization_id
         AND d.material_detail_id = p_material_detail_id
         AND l.lot_number LIKE (p_lot_number)
      UNION
      SELECT parent_lot_number
        FROM mtl_lot_numbers l, gme_material_details d
       WHERE l.inventory_item_id =  d.inventory_item_id
         AND l.organization_id = d.organization_id
         AND d.material_detail_id = p_material_detail_id
         AND l.parent_lot_number LIKE (p_lot_number)
    ORDER BY 1;

  END Pending_Parent_Lot_LoV;
 /*+========================================================================+
   | PROCEDURE NAME
   |   UoM_LoV
   |
   | USAGE
   |
   | ARGUMENTS
   |   p_uom_code
   |   p_organization_id
   |   p_item_id
   |
   | RETURNS
   |   REF cursor x_uom_lov
   |
   | HISTORY
   |   Created  26-Apr-05 Eddie Oumerretane
   |   Namit S. Bug4917215 01Feb06. Changed queries to dynamic sql.
   |
   +========================================================================+*/
  PROCEDURE UoM_LoV(
    x_uom_lov             OUT    NOCOPY t_genref
  , p_uom_code            IN     VARCHAR2
  , p_organization_id     IN     NUMBER
  , p_item_id             IN     NUMBER
  ) IS

  sqlstmt VARCHAR2(2000);

  BEGIN

-- Namit S. Bug4917215 01Feb06. Used the mtl_item_uoms_view definition query with dynamic sql to improve perf.

  sqlstmt :=  ' SELECT DISTINCT mtluom2.uom_code, '
                  ||' mtluom2.unit_of_measure '
              ||' FROM mtl_system_items_b mtlitm1, '
                  ||' mtl_units_of_measure_tl mtluom2, '
                  ||' mtl_uom_conversions mtlucv '
              ||' WHERE mtlitm1.inventory_item_id = :p_item_id '
                  ||' AND mtlitm1.organization_id = :p_org_id '
                  ||' AND mtluom2.uom_code = mtlucv.uom_code '
                  ||' AND ( mtlucv.inventory_item_id = :p_item_id OR mtlucv.inventory_item_id = 0) '
                  ||' AND mtluom2.language = USERENV('||''''||'LANG'||''''||') '
                  ||' AND (   (       mtlitm1.allowed_units_lookup_code IN (1, 3) '
                           ||' AND mtlucv.inventory_item_id = mtlitm1.inventory_item_id '
                        ||' OR (    mtlucv.inventory_item_id = 0 '
                           ||' AND mtluom2.base_uom_flag = '||''''||'Y'||''''
                           ||' AND mtluom2.uom_class = mtlucv.uom_class '
                           ||' AND mtlucv.uom_class IN ( '
                                   ||' SELECT mtlpri1.uom_class '
                                     ||' FROM mtl_units_of_measure mtlpri1 '
                                   ||' WHERE mtlpri1.uom_code = mtlitm1.primary_uom_code) '
                           ||' ) '
                        ||' OR (    mtlucv.inventory_item_id = 0 '
                           ||' AND mtlucv.uom_code IN ( '
                                   ||' SELECT mtlucc1.to_uom_code '
                                     ||' FROM mtl_uom_class_conversions mtlucc1 '
                                   ||' WHERE mtlucc1.inventory_item_id = mtlitm1.inventory_item_id '
                                     ||' AND NVL (mtlucc1.disable_date, TRUNC (SYSDATE) + 1) > TRUNC (SYSDATE)) '
                           ||' ) '
                       ||' ) '
                    ||' OR (    mtlitm1.allowed_units_lookup_code IN (2, 3) '
                        ||' AND mtlucv.inventory_item_id = 0 '
                        ||' AND (   mtlucv.uom_class IN ( '
                                   ||' SELECT mtlucc.to_uom_class '
                                     ||' FROM mtl_uom_class_conversions mtlucc '
                                   ||' WHERE mtlucc.inventory_item_id = mtlitm1.inventory_item_id '
                                     ||' AND NVL (mtlucc.disable_date, TRUNC (SYSDATE) + 1) > TRUNC (SYSDATE)) '
                             ||' OR mtlucv.uom_class = '
                                   ||' (SELECT mtlpri.uom_class '
                                      ||' FROM mtl_units_of_measure mtlpri '
                                   ||' WHERE mtlpri.uom_code = mtlitm1.primary_uom_code) '
                            ||' ) '
                       ||' ) '
                   ||' ) '
               ||' AND NVL (mtlucv.disable_date, TRUNC (SYSDATE) + 1) > TRUNC (SYSDATE) '
               ||' AND NVL (mtluom2.disable_date, TRUNC (SYSDATE) + 1) > TRUNC (SYSDATE) '
               ||' AND mtluom2.uom_code like (:p_uom_code) '
              ||' ORDER BY 1 ';
    OPEN x_uom_lov FOR sqlstmt USING p_item_id, p_organization_id, p_item_id, p_uom_code;

  END UoM_LoV;


 /*+========================================================================+
   | PROCEDURE NAME
   |   All_Line_Type_LoV
   |
   | USAGE
   |
   | ARGUMENTS
   |   p_organization_id
   |   p_batch_id
   |   p_line_type
   |   p_step_no
   |
   | RETURNS
   |   REF cursor x_line_type_cursor
   |
   | HISTORY
   |   Created  26-Apr-05 Eddie Oumerretane
   |
   +========================================================================+*/
  PROCEDURE All_Line_Type_LoV
  (  x_line_type_cursor OUT NOCOPY t_genref
  ,  p_organization_id IN  NUMBER
  ,  p_batch_id        IN  NUMBER
  ,  p_line_type       IN  VARCHAR2
  ,  p_step_no         IN  VARCHAR2
  )
  IS

    l_line_type VARCHAR2(2);

  BEGIN

    IF p_line_type = '1' OR p_line_type = '1%' THEN
      l_line_type := -1;
    ELSIF p_line_type = '2' OR p_line_type = '2%' THEN
      l_line_type := 1;
    ELSIF p_line_type = '3' OR p_line_type = '3%' THEN
      l_line_type := 2;
    ELSE
      l_line_type := p_line_type;
    END IF;

  IF p_step_no IS NOT NULL THEN

    OPEN x_line_type_cursor FOR
      SELECT distinct decode(d.line_type,-1,'1',+1,'2',+2,'3'),
                      meaning,
                      d.line_type
      FROM gme_batch_header h,
           gme_material_details d,
           gme_batch_step_items si,
           gme_batch_steps      s,
           gem_lookups lkup
      WHERE h.organization_id   = p_organization_id
      AND h.batch_id            = p_batch_id
      AND h.batch_id            = d.batch_id
      AND d.batch_id            = si.batch_id
      AND si.material_detail_id = d.material_detail_id
      AND s.batchstep_no        = p_step_no
        AND si.batchstep_id     = s.batchstep_id
      AND h.batch_type          = 0
      AND lkup.lookup_type      = 'GMD_FORMULA_ITEM_TYPE'
      AND lkup.lookup_code      = d.line_type
      AND sysdate BETWEEN NVL(lkup.start_date_active, SYSDATE)
      AND NVL(lkup.end_date_active, sysdate)
      AND lkup.enabled_flag = 'Y'
      AND d.line_type LIKE (l_line_type)
      ORDER BY 1;

  ELSE

    OPEN x_line_type_cursor FOR
      SELECT distinct decode(d.line_type,-1,'1',+1,'2',+2,'3'),
                      meaning,
                      d.line_type
      FROM gme_batch_header h,
           gme_material_details d,
           gem_lookups lkup
      WHERE h.organization_id   = p_organization_id
      AND h.batch_id            = p_batch_id
      AND h.batch_id = d.batch_id
      AND h.batch_type = 0
      AND lkup.lookup_type = 'GMD_FORMULA_ITEM_TYPE'
      AND lkup.lookup_code = d.line_type
      AND sysdate BETWEEN NVL(lkup.start_date_active, SYSDATE)
      AND NVL(lkup.end_date_active, sysdate)
      AND lkup.enabled_flag = 'Y'
      AND d.line_type LIKE (l_line_type)
      ORDER BY 1;

   END IF;

  END All_Line_Type_LoV;

 /*+========================================================================+
   | PROCEDURE NAME
   |   Prod_Line_Type_LoV
   |
   | USAGE
   |
   | ARGUMENTS
   |   p_organization_id
   |   p_batch_id
   |   p_line_type
   |   p_step_no
   |
   | RETURNS
   |   REF cursor x_line_type_cursor
   |
   | HISTORY
   |   Created  26-Apr-05 Eddie Oumerretane
   |
   +========================================================================+*/
  PROCEDURE Prod_Line_Type_LoV
  (  x_line_type_cursor OUT NOCOPY t_genref
  ,  p_organization_id IN  NUMBER
  ,  p_batch_id        IN  NUMBER
  ,  p_line_type       IN  VARCHAR2
  ,  p_step_no         IN  VARCHAR2
  )
  IS

    l_line_type VARCHAR2(2);

  BEGIN

    IF p_line_type = '1' OR p_line_type = '1%' THEN
      l_line_type := -1;
    ELSIF p_line_type = '2' OR p_line_type = '2%' THEN
      l_line_type := 1;
    ELSIF p_line_type = '3' OR p_line_type = '3%' THEN
      l_line_type := 2;
    ELSE
      l_line_type := p_line_type;
    END IF;

  IF p_step_no IS NOT NULL THEN

    OPEN x_line_type_cursor FOR
      SELECT distinct decode(d.line_type,-1,'1',+1,'2',+2,'3'),
                      meaning,
                      d.line_type
      FROM gme_batch_header h,
           gme_material_details d,
           gme_batch_step_items si,
           gme_batch_steps      s,
           gem_lookups lkup
      WHERE h.organization_id   = p_organization_id
      AND h.batch_id            = p_batch_id
      AND h.batch_id            = d.batch_id
      AND d.batch_id            = si.batch_id
      AND si.material_detail_id = d.material_detail_id
      AND s.batchstep_no        = p_step_no
        AND si.batchstep_id     = s.batchstep_id
      AND h.batch_type          = 0
      AND lkup.lookup_type      = 'GMD_FORMULA_ITEM_TYPE'
      AND lkup.lookup_code      = d.line_type
      AND d.line_type IN (1,2)
      AND sysdate BETWEEN NVL(lkup.start_date_active, SYSDATE)
      AND NVL(lkup.end_date_active, sysdate)
      AND lkup.enabled_flag = 'Y'
      AND d.line_type LIKE (l_line_type)
      ORDER BY 1;

  ELSE

    OPEN x_line_type_cursor FOR
      SELECT distinct decode(d.line_type,-1,'1',+1,'2',+2,'3'),
                      meaning,
                      d.line_type
      FROM gme_batch_header h,
           gme_material_details d,
           gem_lookups lkup
      WHERE h.organization_id   = p_organization_id
      AND h.batch_id            = p_batch_id
      AND h.batch_id = d.batch_id
      AND h.batch_type = 0
      AND lkup.lookup_type = 'GMD_FORMULA_ITEM_TYPE'
      AND lkup.lookup_code = d.line_type
      AND d.line_type IN (1,2)
      AND sysdate BETWEEN NVL(lkup.start_date_active, SYSDATE)
      AND NVL(lkup.end_date_active, sysdate)
      AND lkup.enabled_flag = 'Y'
      AND d.line_type LIKE (l_line_type)
      ORDER BY 1;

   END IF;

  END Prod_Line_Type_Lov;

 /*+========================================================================+
   | PROCEDURE NAME
   |   revision_LoV
   |
   | USAGE
   |
   | ARGUMENTS
   |   p_org_id
   |   p_inventory_item_id
   |   p_revision
   |
   | RETURNS
   |   REF cursor x_revision_cursor
   |
   | HISTORY
   |   Created  19-Jun-06 Shrikant Nene
   |
   +========================================================================+*/
  PROCEDURE Revision_LoV
  (  x_revision_cursor   OUT NOCOPY t_genref
  ,  p_org_id            IN  NUMBER
  ,  p_inventory_item_id IN  NUMBER
  ,  p_revision          IN  VARCHAR2
  )
  IS
  BEGIN

    OPEN x_revision_cursor FOR
      SELECT revision
      FROM  mtl_item_revisions
      WHERE organization_id = p_org_id
      AND   inventory_item_id = p_inventory_item_id
      AND   implementation_date IS NOT NULL
      AND   revision like p_revision
      ORDER BY revision;

  END Revision_LoV;



 /* Bug#5663458 Begin
  * Created the following procedure. This procedure is to get the revisions for a particualr
  * material line. Used in return ingredient and product pages
  */
  PROCEDURE Revision_Txn_Lov
  (  x_revision_cursor     OUT NOCOPY t_genref
  ,  p_org_id              IN  NUMBER
  ,  p_batch_id            IN  NUMBER
  ,  p_material_detail_id  IN  NUMBER
  ,  p_revision            IN  VARCHAR2
  ,  p_line_type           IN  NUMBER
  )
  IS
  BEGIN
   OPEN x_revision_cursor FOR
     SELECT DISTINCT revision
       FROM mtl_material_transactions
       WHERE organization_id = p_org_id
         AND transaction_source_id = p_batch_id
         AND trx_source_line_id = p_material_detail_id
         AND transaction_source_type_id = gme_common_pvt.g_txn_source_type
         AND revision LIKE (p_revision)
      GROUP BY revision
      HAVING (p_line_type = -1 AND SUM(transaction_quantity) < 0) OR
             (p_line_type IN (1,2) AND SUM(transaction_quantity) > 0)--rework
      ORDER BY revision;

  END Revision_Txn_Lov;

 /*
  * Created the following procedure. This procedure is to get the revisions from
  * DLR of material line.
  */
  PROCEDURE Revision_Rsrv_Lov
  (  x_revision_cursor     OUT NOCOPY t_genref
  ,  p_org_id              IN  NUMBER
  ,  p_batch_id            IN  NUMBER
  ,  p_material_detail_id  IN  NUMBER
  ,  p_revision            IN  VARCHAR2
  )
  IS
  BEGIN
   OPEN x_revision_cursor FOR
     SELECT DISTINCT revision
       FROM mtl_reservations
       WHERE organization_id = p_org_id
         AND demand_source_header_id = p_batch_id
         AND demand_source_line_id = p_material_detail_id
         AND demand_source_type_id = 5
         AND revision LIKE (p_revision)
      ORDER BY revision;
  END Revision_Rsrv_Lov;

 /*
  * Created the following procedure. This procedure is to get the revisions from
  * pending product lots of material line.
  */
  PROCEDURE Revision_PndLot_Lov
  (  x_revision_cursor     OUT NOCOPY t_genref
  ,  p_batch_id            IN  NUMBER
  ,  p_material_detail_id  IN  NUMBER
  ,  p_revision            IN  VARCHAR2
  )
  IS
  BEGIN
   OPEN x_revision_cursor FOR
     SELECT DISTINCT revision
       FROM  gme_pending_product_lots
       WHERE batch_id = p_batch_id
         AND material_detail_id = p_material_detail_id
         AND revision LIKE (p_revision)
     ORDER BY revision;
  END Revision_PndLot_Lov;
  /* Bug#5663458 End */

  /*###############################################################
  # DESCRIPTION
  #   Bug 4962372 Resource Instance LOV used in the Start resource transaction
  #   22-MAR-17    Shaliu Chen Bug#25353805
  #                add tracking_number and serial_number 2 column in the cursor
  #                so that those 2 column can be display in the UI     
  ###############################################################*/
  PROCEDURE Resource_Instance_MsT_LoV
  (  x_resource_cursor  OUT NOCOPY t_genref
  ,  p_organization_id IN  NUMBER
  ,  p_resource        IN  VARCHAR2
  ,  p_instance        IN  VARCHAR2 ) IS
  BEGIN
    OPEN x_resource_cursor FOR
     SELECT i.instance_number, i.instance_id,NVL(i.TRACKING_NUMBER,''),NVL(i.SERIAL_NUMBER,'')
     FROM   gmp_resource_instances i, cr_rsrc_dtl r
     WHERE  r.resource_id = i.resource_id
     AND    r.resources = p_resource
     AND    r.organization_id = p_organization_id
     AND    i.instance_number LIKE (p_instance);
  EXCEPTION
    WHEN FND_API.G_EXC_UNEXPECTED_ERROR
    THEN
      NULL;
  END Resource_Instance_Mst_LoV;

  /*###############################################################
  # DESCRIPTION
  #   Bug 4962372 Resource Instance LOV used in the End resource transaction
  ###############################################################*/
  PROCEDURE Resource_Instance_Txn_LoV
  (  x_resource_cursor     OUT NOCOPY t_genref
  ,  p_batch_id               IN  NUMBER
  ,  p_batchstep_resource_id  IN NUMBER
  ,  p_instance               IN  VARCHAR2) IS
  BEGIN
    OPEN x_resource_cursor FOR
     SELECT i.instance_number, i.instance_id
     FROM   gmp_resource_instances i, gme_resource_txns_gtmp t
     WHERE t.doc_id         = p_batch_id
         AND t.line_id        = p_batchstep_resource_id
         AND t.start_date     = t.end_date
         AND action_code NOT IN ('REVS', 'REVL')
         AND t.resource_usage = 0
         AND t.completed_ind  = 1
         AND t.delete_mark    = 0
         AND t.instance_id    = i.instance_id
         AND i.instance_number LIKE (p_instance);
  End Resource_Instance_Txn_LoV;

  /*###############################################################
  # DESCRIPTION
  #     This procedure is for pending steps
  ###############################################################*/
  PROCEDURE step_pending_lov
  (  x_step_cursor     OUT NOCOPY t_genref
  ,  p_batch_id        IN  NUMBER
  ,  p_step_no         IN  VARCHAR2
  ,  p_date_format     IN  VARCHAR2) IS
  BEGIN
    OPEN x_step_cursor FOR
       SELECT gbs.batchstep_no, glk.meaning,gbs.batchstep_id,  gmo.oprn_no, gmo.oprn_vers, gbs.plan_step_qty,
       TO_NUMBER(NULL) act_step_qty, gbs.step_qty_um,
       TO_CHAR(gbs.plan_start_date, p_date_format||gme_mobile_txn.HOUR_MIN_SEC_FORMAT_STRING),
       ' ' act_start_date,
       TO_CHAR(gbs.plan_start_date, p_date_format||gme_mobile_txn.HOUR_MIN_SEC_FORMAT_STRING) plan_comlt_date
       FROM gme_batch_header gbh, gme_batch_steps gbs, gmd_operations gmo, gem_lookups glk
       WHERE gbh.batch_id = p_batch_id
       AND gbh.batch_type = 0
       AND gbh.batch_id = gbs.batch_id
       AND gbs.step_status = 1
       AND glk.lookup_type = 'GME_STEP_STATUS'
       AND glk.lookup_code = TO_CHAR(gbs.step_status)
       AND TO_CHAR(gbs.batchstep_no) LIKE  LTRIM(RTRIM(p_step_no||'%'))
       AND gbs.oprn_id = gmo.oprn_id
       ORDER BY 1;
  EXCEPTION
    WHEN FND_API.G_EXC_UNEXPECTED_ERROR THEN
      NULL;
  END step_pending_lov;

  /*###############################################################
  # DESCRIPTION
  #     This procedure is for pending/wip steps
  # HISTORY   
  #          8-Jul-14  Shaliu Chen ER 19161894
  #          Add some conditions to filter OSP step out  
  ###############################################################*/
  PROCEDURE step_pending_wip_lov
  (  x_step_cursor     OUT NOCOPY t_genref
  ,  p_batch_id        IN  NUMBER
  ,  p_step_no         IN  VARCHAR2
  ,  p_date_format     IN  VARCHAR2) IS
  BEGIN
    OPEN x_step_cursor FOR
       SELECT gbs.batchstep_no, glk.meaning,gbs.batchstep_id,  gmo.oprn_no, gmo.oprn_vers, gbs.plan_step_qty,
       TO_NUMBER(NULL) act_step_qty, gbs.step_qty_um,
       TO_CHAR(gbs.plan_start_date, p_date_format||gme_mobile_txn.HOUR_MIN_SEC_FORMAT_STRING),
       DECODE(gbs.actual_start_date,NULL,' ',TO_CHAR(gbs.actual_start_date, p_date_format||gme_mobile_txn.HOUR_MIN_SEC_FORMAT_STRING)) act_start_date,
       TO_CHAR(gbs.plan_start_date, p_date_format||gme_mobile_txn.HOUR_MIN_SEC_FORMAT_STRING) plan_comlt_date
       FROM gme_batch_header gbh, gme_batch_steps gbs, gmd_operations gmo, gem_lookups glk
       WHERE gbh.batch_id = p_batch_id
       AND gbh.batch_type = 0
       AND gbh.batch_id = gbs.batch_id
       AND gbs.step_status IN (1,2)
       AND glk.lookup_type = 'GME_STEP_STATUS'
       AND glk.lookup_code = TO_CHAR(gbs.step_status)
       AND TO_CHAR(gbs.batchstep_no) LIKE  LTRIM(RTRIM(p_step_no||'%'))
       AND gbs.oprn_id = gmo.oprn_id
       /*ER 19161894  Shaliu Chen 18-JUL-2014*/
       AND NOT EXISTS (
                       SELECT 'OSP Resource Exist'
                         FROM gme_batch_step_resources gbsr,
                              cr_rsrc_mst crm,
                              cr_rsrc_dtl crd
                        WHERE gbsr.batchstep_id = gbs.batchstep_id
                          AND gbsr.resources = crd.resources
                          AND gbsr.organization_id = crd.organization_id
                          AND crd.purchase_item_id IS NOT NULL
                          AND crd.resources = crm.resources
                          AND NVL(crm.outside_process_ind,gme_osp.g_osp_ind_disabled) = gme_osp.g_osp_ind_enabled)
       ORDER BY 1;
  EXCEPTION
    WHEN FND_API.G_EXC_UNEXPECTED_ERROR THEN
      NULL;
  END step_pending_wip_lov;

  /*###############################################################
  # DESCRIPTION
  #   Bug 4962372 Resource LOV used in the Start resource transaction
  ###############################################################*/
  PROCEDURE Resource_Mst_LoV
  (  x_resource_cursor     OUT NOCOPY t_genref
  ,  p_batch_id        IN  NUMBER
  ,  p_step_no         IN  VARCHAR2
  ,  p_activity_id     IN  NUMBER
  ,  p_resource        IN  VARCHAR2
  ,  p_date_format     IN  VARCHAR2) IS
  BEGIN
    OPEN x_resource_cursor FOR
      SELECT
           r.resources
         , r.batchstep_resource_id
         , DECODE(i.instance_number, null, ' ')
         , NVL (i.instance_id,-1)
         , TO_CHAR(r.actual_start_date,p_date_format)
         , TO_CHAR(r.actual_cmplt_date,p_date_format)
         , TO_CHAR(DECODE(s.step_status, 2, r.plan_start_date, 3, r.actual_start_date), p_date_format)
         , u.user_name
         , ROUND(DECODE(s.step_status, 2, r.plan_rsrc_qty, 3, r.actual_rsrc_qty),2)
         , r.resource_qty_um
         , ROUND(DECODE(s.step_status, 2, r.plan_rsrc_usage, 3, r.actual_rsrc_usage),2)
         , r.usage_um
      FROM   gme_batch_header h,
             gme_batch_steps s,
             gme_batch_step_activities a,
             gme_batch_step_resources r,
             gmp_resource_instances i,
             fnd_user u
      WHERE  h.batch_id =  p_batch_id
      AND    h.batch_id = s.batch_id
      AND    s.batchstep_no = p_step_no
      AND    s.batchstep_id = a.batchstep_id
      AND    h.batch_id = a.batch_id
      AND    r.batch_id = h.batch_id
      AND    r.batchstep_id = s.batchstep_id
      AND    r.batchstep_activity_id = a.batchstep_activity_id
      AND    r.batchstep_activity_id = p_activity_id
      AND    r.resources LIKE LTRIM(RTRIM(p_resource||'%'))
      AND    r.batchstep_resource_id = i.resource_id(+)
      AND    u.user_id = r.last_updated_by
      ORDER BY r.resources;

  EXCEPTION
    WHEN FND_API.G_EXC_UNEXPECTED_ERROR
    THEN
      NULL;
  END Resource_MsT_LoV;

  /*###############################################################
  # DESCRIPTION
  #   Bug 4962372 Resource LOV used in the End resource transaction
  ###############################################################*/
  PROCEDURE Resource_Txn_LoV
  (  x_resource_cursor     OUT NOCOPY t_genref
  ,  p_batch_id        IN  NUMBER
  ,  p_step_no         IN  VARCHAR2
  ,  p_activity_id     IN  NUMBER
  ,  p_resource        IN  VARCHAR2
  ,  p_date_format     IN  VARCHAR2) IS
  BEGIN

    OPEN x_resource_cursor FOR
      SELECT
           r.resources
         , r.batchstep_resource_id
         , DECODE(t.instance_id, NULL, ' ', i.instance_number)
         , NVL (t.instance_id,-1)
         , TO_CHAR(r.actual_start_date,p_date_format)
         , TO_CHAR(r.actual_cmplt_date,p_date_format)
         , TO_CHAR(t.start_date, p_date_format)
         , u.user_name
         , ROUND(DECODE(s.step_status, 2, r.plan_rsrc_qty, 3, r.actual_rsrc_qty),2)
         , r.resource_qty_um
         , ROUND(DECODE(s.step_status, 2, r.plan_rsrc_usage, 3, r.actual_rsrc_usage),2)
         , r.usage_um
      FROM   gme_batch_steps s,
             gme_batch_step_activities a,
             gme_batch_step_resources r,
             gmp_resource_instances i,
             gme_resource_txns_gtmp t,
             gme_resource_txns rt,
             fnd_user u
      WHERE
             t.doc_id         = p_batch_id
         AND t.line_id        = r.batchstep_resource_id
         AND t.start_date     = t.end_date
         AND t.resource_usage = 0
         AND t.completed_ind  = 1
         AND t.action_code NOT IN ('REVS', 'REVL')
         AND t.delete_mark    = 0
         AND t.instance_id = i.instance_id(+)
         AND s.batchstep_no = p_step_no
         AND s.batchstep_id = a.batchstep_id
         AND a.batch_id     = t.doc_id
         AND r.batch_id = a.batch_id
         AND r.batchstep_id = s.batchstep_id
         AND r.batchstep_activity_id = a.batchstep_activity_id
         AND r.batchstep_activity_id = p_activity_id
         AND r.resources LIKE LTRIM(RTRIM(p_resource||'%'))
         AND rt.poc_trans_id = t.poc_trans_id
         AND u.user_id = rt.last_updated_by
      ORDER BY r.resources;

  EXCEPTION
    WHEN FND_API.G_EXC_UNEXPECTED_ERROR
    THEN
      NULL;
  END Resource_Txn_LoV;
 /* Bug#19619931
  * Created the following procedure. This procedure is to get the LPN LoV's.
  * Used in Issue ingredient page.
  */
  PROCEDURE Lpn_LoV
  (  x_line_cursor           OUT NOCOPY t_genref
  ,  p_org_id                IN  NUMBER
  ,  p_lpn_no                IN  VARCHAR2
  ,  p_inventory_item_id     IN  NUMBER
  )
  IS
  BEGIN

    OPEN x_line_cursor FOR
        SELECT wlpn.LICENSE_PLATE_NUMBER,
        wlpn.LPN_ID,
        wlpn.LPN_CONTEXT,
        wlpn.SUBINVENTORY_CODE,
        wlpn.LOCATOR_ID,
        milk.concatenated_segments
   FROM WMS_LICENSE_PLATE_NUMBERS wlpn,
        wms_lpn_contents wlc,
        mtl_item_locations_kfv milk
   WHERE LPN_CONTEXT IN (1)
     AND wlpn.lpn_id = wlc.parent_lpn_id (+)
     AND wlpn.organization_id = wlc.organization_id (+)
     AND wlpn.ORGANIZATION_ID= p_org_id
     AND wlpn.organization_id  = milk.organization_id
     AND wlpn.license_plate_number LIKE LTRIM(RTRIM('%'||p_lpn_no||'%'))
     AND wlc.inventory_item_id  = p_inventory_item_id
     AND wlpn.locator_id  = milk.inventory_location_id
     AND wlpn.parent_lpn_id IS NULL;
  END Lpn_LoV;
/* Bug#21786445
  * Created the following procedure. This procedure is to get the Return LPN LoV's.
  * Used in Return ingredient page.
  */
  PROCEDURE Return_Lpn_LoV
  (  x_line_cursor           OUT NOCOPY t_genref
  ,  p_org_id                IN  NUMBER
  ,  p_batch_id              IN  NUMBER
  ,  p_lpn_no                IN  VARCHAR2
  ,  p_material_detail_id    IN  NUMBER
  )
  IS
  BEGIN

    OPEN x_line_cursor FOR
        SELECT wlpn.LICENSE_PLATE_NUMBER,
              mmt.lpn_id,
              mmt.subinventory_code,
              milk.concatenated_segments
           FROM WMS_LICENSE_PLATE_NUMBERS wlpn,
                mtl_material_transactions mmt,
                mtl_item_locations_kfv milk
          WHERE mmt.trx_source_line_id = p_material_detail_id
                AND mmt.transaction_source_id = p_batch_id
                AND mmt.transaction_source_type_id = 5
                AND mmt.TRANSACTION_TYPE_ID = 35
                AND wlpn.ORGANIZATION_ID= p_org_id
                AND wlpn.organization_id  = milk.organization_id
                AND wlpn.license_plate_number LIKE LTRIM(RTRIM('%'||p_lpn_no||'%'))
                AND wlpn.lpn_id=mmt.lpn_id
                AND wlpn.locator_id  = milk.inventory_location_id
                AND NOT EXISTS ( SELECT /*+ no_unnest */ transaction_id1
                     FROM gme_transaction_pairs
                    WHERE transaction_id1 = transaction_id
                      AND pair_type = 1)
            GROUP BY wlpn.LICENSE_PLATE_NUMBER,mmt.lpn_id, mmt.subinventory_code, milk.concatenated_segments; 
  END Return_Lpn_LoV;

END GME_MOBILE_LOVS;
/
--show errors;
COMMIT;
EXIT;
