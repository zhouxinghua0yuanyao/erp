REM dbdrv: sql ~PROD ~PATH ~FILE none none none package &phase=pls \
REM dbdrv: checkfile(120.17.12010000.6=120.19.12020000.4)(120.15.12000000.5=120.17.12010000.3)(120.15.12000000.5=120.19):~PROD:~PATH:~FILE
SET VERIFY OFF;
WHENEVER SQLERROR EXIT FAILURE ROLLBACK;
WHENEVER OSERROR EXIT FAILURE ROLLBACK;
create or replace PACKAGE GME_MOBILE_LOVS AUTHID CURRENT_USER AS
/*  $Header: GMEMLOVS.pls 120.19.12020000.4 2017/02/08 03:15:43 shalchen ship $   */
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
 | 09-Jun-06     Namit Singhi. Bug#5236906. Defined 2 new procs -            |
 |                 Step_Material_Lot_Line_LoV and Material_Lot_LoV. Show only|
 |                 lot controlled items for Create/Pending Lots.             |
 | 21-Jun-06     Shrikant Nene Bug#5263908. Defined 1 new procs -            |
 |                 Revision_LoV.                                             |
 | 21-Aug-06     Shrikant Nene Bug#5263908. Defined 1 new procs -            |
 |                 Pending_Parent_Lot_LoV                                    |
 | 15-Sep-06    SivakumarG  Bug#5261131                                      |
 |                 Added procedure Lot_OnHand_Exp_LoV to filter based on     |
 |                 expiration date                                           |
 |10-SEP-2015    Jagadiswar Devarla Bug#21786445                             |
 |                 Created new procedure Return_Lpn_LoV to fetch lpn LoV.    |
 +===========================================================================*/

  TYPE t_genref IS REF CURSOR;

/*
  TYPE allocation_rec IS RECORD
  ( trans_id         NUMBER
  , line_no          NUMBER
  , line_id          NUMBER
  , item_id          NUMBER
  , item_no          VARCHAR2(32)
  , trans_qty        NUMBER
  , trans_um         VARCHAR2(4)
  , trans_qty2       NUMBER
  , trans_um2        NUMBER
  , whse_code        VARCHAR2(4)
  , lot_id           NUMBER
  , lot_no           VARCHAR2(32)
  , sublot_no        VARCHAR2(32)
  , location         VARCHAR2(32)
  , reason_code      VARCHAR2(4)
  , completed_ind    NUMBER(1)
  , batch_id         NUMBER
  );

  TYPE allocation_tab IS TABLE OF allocation_rec INDEX BY BINARY_INTEGER;

  allocations        allocation_tab;
*/

   PROCEDURE Batch_LoV
  ( x_batch_cursor     OUT NOCOPY t_genref
  ,  p_org_id         IN  NUMBER
  ,  p_statuses       IN  VARCHAR2
  ,  p_batch_no       IN  VARCHAR2
  );

  PROCEDURE Ingredient_Line_Rsrv_LoV
  (  x_line_cursor     OUT NOCOPY t_genref
  ,  p_batch_id        IN  NUMBER
  ,  p_line_no         IN  VARCHAR2
  ,  p_item_no         IN  VARCHAR2
  );

  PROCEDURE Step_Ingredient_Line_LoV
  (  x_line_cursor     OUT NOCOPY t_genref
  ,  p_batch_id        IN  NUMBER
  ,  p_line_no         IN  VARCHAR2
  ,  p_step_no         IN  VARCHAR2
  );

 PROCEDURE Step_Ingredient_Lov
  (  x_step_cursor     OUT NOCOPY t_genref
  ,  p_batch_id        IN  NUMBER
  ,  p_step_no         IN  VARCHAR2
  );

  PROCEDURE Step_Prod_ByProd_LoV
  (  x_step_cursor     OUT NOCOPY t_genref
  ,  p_batch_id        IN  NUMBER
  ,  p_step_no         IN  VARCHAR2
  );

  PROCEDURE Step_Pend_Lot_LoV
  (  x_step_cursor     OUT NOCOPY t_genref
  ,  p_batch_id        IN  NUMBER
  ,  p_step_no         IN  VARCHAR2
  );

  PROCEDURE Step_All_Items_LoV
  (  x_step_cursor     OUT NOCOPY t_genref
  ,  p_batch_id        IN  NUMBER
  ,  p_step_no         IN  VARCHAR2
  );

  PROCEDURE Activity_LoV
  (  x_activity_cursor     OUT NOCOPY t_genref
  ,  p_organization_id IN  NUMBER
  ,  p_batch_id        IN  NUMBER
  ,  p_step_no         IN  VARCHAR2
  ,  p_activity        IN  VARCHAR2
  );

  PROCEDURE Resource_LoV
  (  x_resource_cursor     OUT NOCOPY t_genref
  ,  p_org_id          IN  NUMBER
  ,  p_batch_id        IN  NUMBER
  ,  p_step_no         IN  NUMBER
  ,  p_activity_id     IN  NUMBER
  ,  p_resource        IN  VARCHAR2
  );

  PROCEDURE Ingredient_LoV
  (  x_item_cursor     OUT NOCOPY t_genref
  ,  p_batch_id        IN  NUMBER
  ,  p_line_no         IN  VARCHAR2
  ,  p_item_no         IN  VARCHAR2
  );

  PROCEDURE Ingredient_Rsrv_LoV
  (  x_item_cursor     OUT NOCOPY t_genref
  ,  p_batch_id        IN  NUMBER
  ,  p_line_no         IN  VARCHAR2
  ,  p_item_no         IN  VARCHAR2
  );

  PROCEDURE Ingredient_Step_LoV
  (  x_item_cursor     OUT NOCOPY t_genref
  ,  p_batch_id        IN  NUMBER
  ,  p_line_no         IN  VARCHAR2
  ,  p_item_no         IN  VARCHAR2
  ,  p_step_no         IN  VARCHAR2
  );

 /* Bug#5663458 Begin
  * Created the following procedure
  */
  PROCEDURE Rsrv_LoV(
      x_batch_cursor        OUT NOCOPY t_genref
     ,p_org_id              IN  NUMBER
     ,p_batch_id            IN  NUMBER
     ,p_material_detail_id  IN  NUMBER
   );

 PROCEDURE Dispense_LoV(
      x_batch_cursor        OUT NOCOPY t_genref
   );

 PROCEDURE Subinventory_OnHand_LoV(
      x_sub_lov                OUT    NOCOPY t_genref
    ,p_organization_id        IN     NUMBER
    ,p_subinventory_code      IN     VARCHAR2
    ,p_inventory_item_id      IN     NUMBER
    ,p_revision               IN     VARCHAR2
    ,p_restrict_subinv        IN     NUMBER
  );

  PROCEDURE Locator_OnHand_LoV(
      x_locators               OUT    NOCOPY t_genref
     ,p_organization_id        IN     NUMBER
     ,p_subinventory_code      IN     VARCHAR2
     ,p_inventory_item_id      IN     NUMBER
     ,p_locator                IN     VARCHAR2
     ,p_revision               IN     VARCHAR2
     ,p_restrict_locators      IN     NUMBER
   );

  PROCEDURE Ing_Locator_LoV_Rsrv(
     x_locators               OUT    NOCOPY t_genref
    ,p_organization_id     IN     NUMBER
    ,p_batch_id            IN     NUMBER
    ,p_material_detail_id  IN     NUMBER
    ,p_item_id             IN     NUMBER
    ,p_subinventory_code   IN     VARCHAR2
    ,p_locator             IN     VARCHAR2
  );
  
  --BUG 25412645  Shaliu Chen 08-FEB-2016
  --Add new paramter - p_lot_number
  PROCEDURE PndLot_LoV(
    x_lot_num_lov         OUT    NOCOPY t_genref
   ,p_org_id              IN     NUMBER
   ,p_batch_id            IN     NUMBER
   ,p_material_detail_id  IN     NUMBER
   ,p_lot_number          IN     VARCHAR2
  );

   PROCEDURE Ing_Subinventory_LoV_Rsrv(
    x_sub_lov             OUT    NOCOPY t_genref
   ,p_organization_id     IN     NUMBER
   ,p_batch_id            IN     NUMBER
   ,p_material_detail_id  IN     NUMBER
   ,p_item_id             IN     NUMBER
   ,p_subinventory_code   IN     VARCHAR2
  );

  PROCEDURE Revision_Txn_Lov
  (  x_revision_cursor     OUT NOCOPY t_genref
  ,  p_org_id              IN  NUMBER
  ,  p_batch_id            IN  NUMBER
  ,  p_material_detail_id  IN  NUMBER
  ,  p_revision            IN  VARCHAR2
  ,  p_line_type           IN  NUMBER
  );

  PROCEDURE Revision_Rsrv_Lov
  (  x_revision_cursor     OUT NOCOPY t_genref
  ,  p_org_id              IN  NUMBER
  ,  p_batch_id            IN  NUMBER
  ,  p_material_detail_id  IN  NUMBER
  ,  p_revision            IN  VARCHAR2
  );

  PROCEDURE Revision_PndLot_Lov
  (  x_revision_cursor     OUT NOCOPY t_genref
  ,  p_batch_id            IN  NUMBER
  ,  p_material_detail_id  IN  NUMBER
  ,  p_revision            IN  VARCHAR2
  );

  PROCEDURE Line_Step_LoV
  (  x_item_cursor     OUT NOCOPY t_genref
  ,  p_batch_id        IN  NUMBER
  ,  p_line_no         IN  VARCHAR2
  ,  p_item_no         IN  VARCHAR2
  ,  p_step_no         IN  VARCHAR2
  ,  p_line_type       IN  NUMBER
  );

  PROCEDURE Pending_Rev_Lot_LoV(
    x_lot_num_lov         OUT    NOCOPY t_genref
  , p_lot_number          IN     VARCHAR2
  , p_batch_id            IN     NUMBER
  , p_material_detail_id  IN     NUMBER
  , p_revision            IN     VARCHAR2
  );
  /* Bug#5663458 End */

  PROCEDURE Subinventory_Lov_Temp_Rsrv(x_sub_lov    OUT NOCOPY t_genref,
                                       p_organization_id IN NUMBER,
                                       p_item_id         IN NUMBER,
                                       p_revision        IN VARCHAR2,
                                       p_lot_number      IN VARCHAR2,
                                       p_subinv_code     IN VARCHAR);

   --Bug#5867209 added the p_restrict_subinv argument
  PROCEDURE Subinventory_Lov(x_sub_lov         OUT NOCOPY t_genref,
                             p_organization_id IN NUMBER,
                             p_subinv_code     IN VARCHAR,
                             p_item_id         IN NUMBER,
                             p_restrict_subinv IN NUMBER);


  PROCEDURE Locator_LoV(
    x_locators               OUT    NOCOPY t_genref
  , p_organization_id        IN     NUMBER
  , p_subinventory_code      IN     VARCHAR2
  , p_inventory_item_id      IN     NUMBER
  , p_concatenated_segments  IN     VARCHAR2
  , p_restrict_locators      IN     NUMBER
  );

  PROCEDURE Locator_LoV_Rsrv(
    x_locators               OUT    NOCOPY t_genref
  , p_organization_id     IN     NUMBER
  , p_batch_id            IN     NUMBER
  , p_material_detail_id  IN     NUMBER
  , p_item_id             IN     NUMBER
  , p_subinventory_code   IN     VARCHAR2
  , p_locator             IN     VARCHAR2
  , p_lot_number          IN     VARCHAR2
  );

  PROCEDURE Locator_LoV_Temp_Rsrv(
    x_locator_lov         OUT    NOCOPY t_genref
  , p_lot_number          IN     VARCHAR2
  , p_organization_id     IN     NUMBER
  , p_item_id             IN     NUMBER
  , p_revision            IN     VARCHAR2
  , p_subinv_code         IN     VARCHAR2
  , p_locator_code        IN     VARCHAR2
  );

  PROCEDURE Pending_Lot_LoV(
    x_lot_num_lov         OUT    NOCOPY t_genref
  , p_lot_number          IN     VARCHAR2
  , p_batch_id            IN     NUMBER
  , p_material_detail_id  IN     NUMBER
  );

  PROCEDURE Pending_Parent_Lot_LoV(
    x_lot_num_lov         OUT    NOCOPY t_genref
  , p_lot_number          IN     VARCHAR2
  , p_batch_id            IN     NUMBER
  , p_material_detail_id  IN     NUMBER
  );

  PROCEDURE Lot_Onhand_Lov(
    x_lot_num_lov         OUT    NOCOPY t_genref
  , p_lot_number          IN     VARCHAR2
  , p_organization_id     IN     NUMBER
  , p_item_id             IN     NUMBER
  , p_subinventory_code   IN     VARCHAR2
  , p_locator_id          IN     NUMBER
  );

  /*Bug#5261131 created the following procedure to filter out
    expired lots */
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
  );

  PROCEDURE Lot_Lov(
    x_lot_num_lov         OUT    NOCOPY t_genref
  , p_lot_number          IN     VARCHAR2
  , p_organization_id     IN     NUMBER
  , p_item_id             IN     NUMBER
  );

  PROCEDURE Lot_LoV_Rsrv(
    x_lot_num_lov         OUT    NOCOPY t_genref
  , p_lot_number          IN     VARCHAR2
  , p_organization_id     IN     NUMBER
  , p_batch_id            IN     NUMBER
  , p_material_detail_id  IN     NUMBER
  , p_item_id             IN     NUMBER
  , p_subinventory_code   IN     VARCHAR2
  , p_locator_id          IN     NUMBER);

  PROCEDURE Lot_LoV_Txn(
    x_lot_num_lov         OUT    NOCOPY t_genref
  , p_lot_number          IN     VARCHAR2
  , p_organization_id     IN     NUMBER
  , p_batch_id            IN     NUMBER
  , p_material_detail_id  IN     NUMBER
  , p_revision            IN     VARCHAR2
  , p_line_type           IN     NUMBER
  );

  PROCEDURE UoM_LoV(
    x_uom_lov             OUT    NOCOPY t_genref
  , p_uom_code            IN     VARCHAR2
  , p_organization_id     IN     NUMBER
  , p_item_id             IN     NUMBER
  );

  PROCEDURE Reason_LoV
  (  x_reason_cursor  OUT NOCOPY t_genref
  ,  p_reason_name    IN  VARCHAR2
  );

  PROCEDURE Prod_Line_Type_LoV
  (  x_line_type_cursor OUT NOCOPY t_genref
  ,  p_organization_id IN  NUMBER
  ,  p_batch_id        IN  NUMBER
  ,  p_line_type       IN  VARCHAR2
  ,  p_step_no         IN  VARCHAR2
  );

  PROCEDURE All_Line_Type_LoV
  (  x_line_type_cursor OUT NOCOPY t_genref
  ,  p_organization_id IN  NUMBER
  ,  p_batch_id        IN  NUMBER
  ,  p_line_type       IN  VARCHAR2
  ,  p_step_no         IN  VARCHAR2
  );

  PROCEDURE Material_Step_LoV
  (  x_item_cursor     OUT NOCOPY t_genref
  ,  p_batch_id        IN  NUMBER
  ,  p_line_no         IN  VARCHAR2
  ,  p_item_no         IN  VARCHAR2
  ,  p_step_no         IN  VARCHAR2
  ,  p_line_type       IN  NUMBER
  );

--nsinghi bug#5236906. Added procs Material_Lot_LoV and Step_Material_Lot_Line_LoV
  PROCEDURE Material_Lot_LoV
  (  x_item_cursor     OUT NOCOPY t_genref
  ,  p_batch_id        IN  NUMBER
  ,  p_line_no         IN  VARCHAR2
  ,  p_item_no         IN  VARCHAR2
  ,  p_step_no         IN  VARCHAR2
  ,  p_line_type       IN  NUMBER
  );

  PROCEDURE Step_Material_Lot_Line_LoV
  (  x_line_cursor     OUT NOCOPY t_genref
  ,  p_batch_id        IN  NUMBER
  ,  p_line_no         IN  VARCHAR2
  ,  p_step_no         IN  VARCHAR2
  ,  p_line_type       IN  NUMBER
  );

  PROCEDURE Step_Material_Line_LoV
  (  x_line_cursor     OUT NOCOPY t_genref
  ,  p_batch_id        IN  NUMBER
  ,  p_line_no         IN  VARCHAR2
  ,  p_step_no         IN  VARCHAR2
  ,  p_line_type       IN  NUMBER
  );

  PROCEDURE Step_For_Resource_LoV
  (  x_step_cursor     OUT NOCOPY t_genref
  ,  p_batch_id        IN  NUMBER
  ,  p_step_no         IN  VARCHAR2
  );


  PROCEDURE Subinventory_LoV_Rsrv(
    x_sub_lov             OUT    NOCOPY t_genref
  , p_organization_id     IN     NUMBER
  , p_batch_id            IN     NUMBER
  , p_material_detail_id  IN     NUMBER
  , p_item_id             IN     NUMBER
  , p_subinventory_code   IN     VARCHAR2
  , p_locator_id          IN     NUMBER
  , p_lot_number          IN     VARCHAR2
  );

  PROCEDURE Lot_LoV_Temp_Rsrv(
    x_lot_num_lov         OUT    NOCOPY t_genref
  , p_lot_number          IN     VARCHAR2
  , p_organization_id     IN     NUMBER
  , p_item_id             IN     NUMBER
  , p_revision            IN     VARCHAR2
  , p_subinv_code         IN     VARCHAR2
  , p_locator_id          IN     NUMBER
  );

  PROCEDURE Lot_LoV_Dispense(
    x_lot_num_lov         OUT    NOCOPY t_genref
  , p_lot_number          IN     VARCHAR2
  , p_subinv_code         IN     VARCHAR2
  , p_locator_id          IN     NUMBER
  );

  PROCEDURE Subinventory_LoV_Dispense(x_sub_lov         OUT NOCOPY t_genref,
                                      p_organization_id IN NUMBER,
                                      p_subinv_code     IN VARCHAR);

  PROCEDURE Locator_LoV_Dispense(
    x_locator_lov         OUT    NOCOPY t_genref
  , p_subinv_code         IN     VARCHAR2
  , p_locator_code        IN     VARCHAR2
  );

  PROCEDURE Step_Material_Line_For_IB_LoV
  (  x_line_cursor     OUT NOCOPY t_genref
  ,  p_batch_id        IN  NUMBER
  ,  p_line_no         IN  VARCHAR2
  ,  p_step_no         IN  VARCHAR2
  ,  p_line_type       IN  NUMBER
  );

  PROCEDURE Material_Step_For_IB_LoV
  (  x_item_cursor     OUT NOCOPY t_genref
  ,  p_batch_id        IN  NUMBER
  ,  p_line_no         IN  VARCHAR2
  ,  p_item_no         IN  VARCHAR2
  ,  p_step_no         IN  VARCHAR2
  ,  p_line_type       IN  NUMBER
  );

  PROCEDURE Revision_LoV
  (  x_revision_cursor   OUT NOCOPY t_genref
  ,  p_org_id            IN  NUMBER
  ,  p_inventory_item_id IN  NUMBER
  ,  p_revision          IN  VARCHAR2
  );

  PROCEDURE Resource_Instance_MsT_LoV
  (  x_resource_cursor  OUT NOCOPY t_genref
  ,  p_organization_id IN  NUMBER
  ,  p_resource        IN  VARCHAR2
  ,  p_instance        IN  VARCHAR2 );

  PROCEDURE Resource_Instance_Txn_LoV
  (  x_resource_cursor     OUT NOCOPY t_genref
  ,  p_batch_id               IN  NUMBER
  ,  p_batchstep_resource_id  IN NUMBER
  ,  p_instance               IN  VARCHAR2);

  PROCEDURE step_pending_lov
  (  x_step_cursor     OUT NOCOPY t_genref
  ,  p_batch_id        IN  NUMBER
  ,  p_step_no         IN  VARCHAR2
  ,  p_date_format     IN  VARCHAR2);

  PROCEDURE step_pending_wip_lov
  (  x_step_cursor     OUT NOCOPY t_genref
  ,  p_batch_id        IN  NUMBER
  ,  p_step_no         IN  VARCHAR2
  ,  p_date_format     IN  VARCHAR2);

  PROCEDURE Resource_Mst_LoV
  (  x_resource_cursor     OUT NOCOPY t_genref
  ,  p_batch_id        IN  NUMBER
  ,  p_step_no         IN  VARCHAR2
  ,  p_activity_id     IN  NUMBER
  ,  p_resource        IN  VARCHAR2
  ,  p_date_format     IN  VARCHAR2);

  PROCEDURE Resource_Txn_LoV
  (  x_resource_cursor     OUT NOCOPY t_genref
  ,  p_batch_id        IN  NUMBER
  ,  p_step_no         IN  VARCHAR2
  ,  p_activity_id     IN  NUMBER
  ,  p_resource        IN  VARCHAR2
  ,  p_date_format     IN  VARCHAR2);

  --Bug#19619931
  PROCEDURE Lpn_LoV
  (  x_line_cursor           OUT NOCOPY t_genref
  ,  p_org_id                IN  NUMBER
  ,  p_lpn_no                IN  VARCHAR2
  ,  p_inventory_item_id     IN  NUMBER);

 --Bug#21786445
  PROCEDURE Return_Lpn_LoV
  (  x_line_cursor           OUT NOCOPY t_genref
  ,  p_org_id                IN  NUMBER
  ,  p_batch_id              IN  NUMBER
  ,  p_lpn_no                IN  VARCHAR2
  ,  p_material_detail_id    IN  NUMBER);
  
END GME_MOBILE_LOVS;
/

---show errors;
COMMIT;
EXIT;
