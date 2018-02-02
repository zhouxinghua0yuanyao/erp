REM dbdrv: sql ~PROD ~PATH ~FILE none none none package &phase=pls \
REM dbdrv: checkfile(120.13.12010000.10=120.17.12020000.5)(120.9.12000000.10=120.13.12010000.7)(120.9.12000000.8=120.13.12010000.5)(120.9.12000000.8=120.17):~PROD:~PATH:~FILE
SET VERIFY OFF;
WHENEVER SQLERROR EXIT FAILURE ROLLBACK;
WHENEVER OSERROR EXIT FAILURE ROLLBACK;
create or replace PACKAGE GME_MOBILE_TXN AUTHID CURRENT_USER AS
/*  $Header: GMEMOTXS.pls 120.17.12020000.5 2015/10/22 09:36:28 jdevarla ship $   */
/*===========================================================================+
 |      Copyright (c) 2005, 2015 Oracle Corporation, Redwood Shores, CA, USA       |
 |                         All rights reserved.                              |
 |===========================================================================|
 |                                                                           |
 | PL/SQL Package to support the (Java) GME Mobile Application.              |
 | Contains PL/SQL procedures used by mobile to transact material.           |
 |                                                                           |
 +===========================================================================+
 |  HISTORY                                                                  |
 |                                                                           |
 | Date          Who               What                                      |
 | ====          ===               ====                                      |
 | 13-May-05     Eddie Oumerretane First version                             |
 | 22-Aug-06 Shrikant Nene Bug 5456068                                       |
 |   Added parameter x_dispensed_count to Populate_Dispensing_Table          |
 |                                                                           |
 | 30-Mar-10     APMISHRA          Bug 9367054. Added a new procedure        |
 |                                 print_label to print labels for product   |
 |                                 transactions                              |
 | 15-Apr-10     APMISHRA          Bug 9483781. Modified the procedure       |
 |                                 Create_material_txn to process the LPN    |
 |                                 details existing for a reservation  
 | 07-Jul-15     JDEVARLA          Bug#19619931.Added lpn id as parameter to
 |                                 Create_Lot_Txn and Create_Material_Txn procedure.
 | 15-Sep-15     JDEVARLA          Bug:21786445,Added lpn id as parameter to |
 |                                 Is_Material_Status_Applicable procedure   |
 | 15-Oct-15     JDEVARLA          Bug:21764324,Added parent lot number as   |
 |                                 parameter to Create_Lot_Txn procedure     |
 +===========================================================================*/

  /* Transaction types for GME defined in MaterialTransaction.java */
  g_txn_source_type        NUMBER := 5;
  g_ing_issue              NUMBER := 35;
  g_ing_return             NUMBER := 43;
  g_prod_completion        NUMBER := 44;
  g_prod_return            NUMBER := 17;
  g_byprod_completion      NUMBER := 1002;
  g_byprod_return          NUMBER := 1003;
  l_reservations_tbl       gme_common_pvt.reservations_tab;

  HOUR_MIN_SEC_FORMAT_STRING     CONSTANT VARCHAR2(11) := ' HH24:MI:SS';


  TYPE t_genref IS REF CURSOR;

  PROCEDURE Get_Stacked_Messages(x_message OUT NOCOPY VARCHAR2);

  --
  -- bug 9483781
  -- Added a new parameter p_reservation_id to
  -- fetch and process the LPN details against
  -- it
  -- bug 19619931
  -- Added a new parameter p_lpn_id to
 
  PROCEDURE Create_Material_Txn(p_organization_id        IN NUMBER,
                                p_batch_id               IN NUMBER,
                                p_material_detail_id     IN NUMBER,
                                p_item_id                IN NUMBER,
                                p_revision               IN VARCHAR2,
                                p_subinventory_code      IN VARCHAR2,
                                p_locator_id             IN NUMBER,
                                p_txn_qty                IN NUMBER,
                                p_txn_uom_code           IN VARCHAR2,
                                p_sec_txn_qty            IN NUMBER,
                                p_sec_uom_code           IN VARCHAR2,
                                p_primary_uom_code       IN VARCHAR2,
                                p_txn_primary_qty        IN NUMBER,
                                p_reason_id              IN NUMBER,
                                p_txn_date               IN DATE,
                                p_txn_type_id            IN NUMBER,
                                p_phantom_type           IN NUMBER,
                                p_user_id                IN NUMBER,
                                p_login_id               IN NUMBER,
                                p_dispense_id            IN NUMBER,
                                p_phantom_line_id        IN NUMBER,
                                x_txn_id                 OUT NOCOPY NUMBER,
                                x_txn_type_id            OUT NOCOPY NUMBER,
                                x_txn_header_id          OUT NOCOPY NUMBER,
                                x_return_status          OUT NOCOPY VARCHAR2,
                                x_error_msg              OUT NOCOPY VARCHAR2,
				                        p_reservation_id         IN NUMBER DEFAULT NULL,
                                p_lpn_id                 IN NUMBER DEFAULT NULL ); --Bug#19619931

  -- Bug 6925025
  -- Added parameters subinventory_code and locator_id
  -- Bug#19619931
  -- Added parameter p_lpn_id
  -- Bug#21764324
  -- Added parameter p_parent_lot_number
  PROCEDURE Create_Lot_Txn(p_txn_id        IN NUMBER,
                           p_txn_type_id   IN NUMBER,
                           p_item_id       IN NUMBER,
                           p_lot_number    IN VARCHAR2,
                           p_txn_qty       IN NUMBER,
                           p_txn_prim_qty  IN NUMBER,
                           p_sec_txn_qty   IN NUMBER,
                           p_user_id       IN NUMBER,
                           p_login_id      IN NUMBER,
                           p_subinventory_code    IN  VARCHAR2,
                           p_locator_id           IN  NUMBER,
                           x_return_status OUT NOCOPY VARCHAR2,
                           x_error_msg     OUT NOCOPY VARCHAR2,
                           p_lpn_id        IN NUMBER DEFAULT NULL,
						   p_parent_lot_number IN VARCHAR2 DEFAULT NULL);

  PROCEDURE Validate_Indiv_Lot_Txn(
                               p_organization_id    IN NUMBER,
                               p_item_id                IN NUMBER,
                               p_revision               IN VARCHAR2,
                               p_subinventory_code      IN VARCHAR2,
                               p_locator_id             IN NUMBER,
                               p_lot_number             IN VARCHAR2,
                               p_primary_lot_qty        IN NUMBER,
                               p_transaction_type_id    IN NUMBER,
                               x_return_status          OUT NOCOPY VARCHAR2,
                               x_error_msg              OUT NOCOPY VARCHAR2);

 --Bug:21786445,added parameter LPN ID
 PROCEDURE Is_Material_Status_Applicable(
                           p_transaction_type_id        IN NUMBER,
                           p_lot_status_enabled         IN VARCHAR2,
                           p_organization_id            IN NUMBER,
                           p_inventory_item_id          IN NUMBER,
                           p_sub_code                   IN VARCHAR2,
                           p_locator_id                 IN NUMBER,
                           p_lot_number                 IN VARCHAR2,
                           p_object_type                IN VARCHAR2,
                           x_return_status          OUT NOCOPY VARCHAR2,
                           x_error_msg              OUT NOCOPY VARCHAR2,
						   p_lpn_id                     IN NUMBER);--Bug:21786445

  PROCEDURE Fetch_Material_Transactions(p_organization_id     IN         NUMBER,
                                      p_batch_id            IN         NUMBER,
                                      p_material_detail_id  IN         NUMBER,
                                      p_txn_type_id         IN         NUMBER,
                                      x_return_status       OUT NOCOPY VARCHAR2,
                                      x_error_msg           OUT NOCOPY VARCHAR2,
                                      x_txn_cursor          OUT NOCOPY t_genref);

  PROCEDURE Fetch_Lot_Transactions(
                                    p_transaction_id IN NUMBER,
                                    p_lot_number     IN VARCHAR2,
                                    x_return_status  OUT NOCOPY VARCHAR2,
                                    x_error_msg      OUT NOCOPY VARCHAR2,
                                    x_txn_cursor     OUT NOCOPY t_genref);

  PROCEDURE Process_Interface_Txn( p_txn_header_id IN NUMBER,
                                   p_user_id       IN NUMBER,
                                   p_login_id      IN NUMBER,
                                   x_return_status OUT NOCOPY VARCHAR2,
                                   x_error_msg     OUT NOCOPY VARCHAR2);
  PROCEDURE Validate_Child_Lot (
                         p_org_id                      IN  NUMBER
                       , p_inventory_item_id           IN  NUMBER
                       , p_parent_lot_number           IN  VARCHAR2
                       , p_lot_number                  IN  VARCHAR2
                       , x_return_status               OUT NOCOPY VARCHAR2
                       , x_error_msg                   OUT NOCOPY VARCHAR2);

  PROCEDURE Generate_Lot_Number(
                         p_org_id                      IN  NUMBER
                       , p_inventory_item_id           IN  NUMBER
                       , p_parent_lot_number           IN  VARCHAR2
                       , x_lot_number                  OUT NOCOPY VARCHAR2
                       , x_return_status               OUT NOCOPY VARCHAR2
                       , x_error_msg                   OUT NOCOPY VARCHAR2);



  PROCEDURE Create_Lot(
                       p_org_id                     NUMBER,
                       p_inventory_item_id NUMBER,
                       p_lot_number                IN VARCHAR2,
                       p_expiration_date           IN DATE,
                       p_supplier_lot_number       IN VARCHAR2,
                       p_grade_code                IN VARCHAR2,
                       p_ORIGINATION_DATE          IN DATE,
                       P_STATUS_ID                 IN NUMBER,
                       p_RETEST_DATE               IN DATE,
                       P_MATURITY_DATE             IN DATE,
                       P_LOT_ATTRIBUTE_CATEGORY    IN VARCHAR2,
                       p_c_attribute1              IN VARCHAR2,
                       p_c_attribute2              IN VARCHAR2,
                       p_c_attribute3              IN VARCHAR2,
                       p_c_attribute4              IN VARCHAR2,
                       p_c_attribute5              IN VARCHAR2,
                       p_c_attribute6              IN VARCHAR2,
                       p_c_attribute7              IN VARCHAR2,
                       p_c_attribute8              IN VARCHAR2,
                       p_c_attribute9              IN VARCHAR2,
                       p_c_attribute10             IN VARCHAR2,
                       p_c_attribute11             IN VARCHAR2,
                       p_c_attribute12             IN VARCHAR2,
                       p_c_attribute13             IN VARCHAR2,
                       p_c_attribute14             IN VARCHAR2,
                       p_c_attribute15             IN VARCHAR2,
                       p_c_attribute16             IN VARCHAR2,
                       p_c_attribute17             IN VARCHAR2,
                       p_c_attribute18             IN VARCHAR2,
                       p_c_attribute19             IN VARCHAR2,
                       p_c_attribute20             IN VARCHAR2,
                       p_d_attribute1              IN DATE,
                       p_d_attribute2              IN DATE,
                       p_d_attribute3              IN DATE,
                       p_d_attribute4              IN DATE,
                       p_d_attribute5              IN DATE,
                       p_d_attribute6              IN DATE,
                       p_d_attribute7              IN DATE,
                       p_d_attribute8              IN DATE,
                       p_d_attribute9              IN DATE,
                       p_d_attribute10             IN DATE,
                       p_n_attribute1              IN NUMBER,
                       p_n_attribute2              IN NUMBER,
                       p_n_attribute3              IN NUMBER,
                       p_n_attribute4              IN NUMBER,
                       p_n_attribute5              IN NUMBER,
                       p_n_attribute6              IN NUMBER,
                       p_n_attribute7              IN NUMBER,
                       p_n_attribute8              IN NUMBER,
                       p_n_attribute9              IN NUMBER,
                       p_n_attribute10             IN NUMBER,
                       p_parent_lot_number         IN VARCHAR2,
                       P_ORIGINATION_TYPE          IN NUMBER,
                       P_EXPIRATION_ACTION_DATE    IN DATE,
                       P_EXPIRATION_ACTION_CODE    IN VARCHAR2,
                       P_HOLD_DATE                 IN DATE,
                       P_REASON_ID                 IN NUMBER,
                       P_ATTRIBUTE_CATEGORY        IN VARCHAR2,
                       P_ATTRIBUTE1                IN VARCHAR2,
                       P_ATTRIBUTE2                IN VARCHAR2,
                       P_ATTRIBUTE3                IN VARCHAR2,
                       P_ATTRIBUTE4                IN VARCHAR2,
                       P_ATTRIBUTE5                IN VARCHAR2,
                       P_ATTRIBUTE6                IN VARCHAR2,
                       P_ATTRIBUTE7                IN VARCHAR2,
                       P_ATTRIBUTE8                IN VARCHAR2,
                       P_ATTRIBUTE9                IN VARCHAR2,
                       P_ATTRIBUTE10               IN VARCHAR2,
                       P_ATTRIBUTE11               IN VARCHAR2,
                       P_ATTRIBUTE12               IN VARCHAR2,
                       P_ATTRIBUTE13               IN VARCHAR2,
                       P_ATTRIBUTE14               IN VARCHAR2,
                       P_ATTRIBUTE15               IN VARCHAR2,
                       p_disable_flag              IN NUMBER,
                       p_COLOR                       IN VARCHAR2,
                       p_VOLUME                      IN NUMBER,
                       p_VOLUME_UOM                  IN VARCHAR2,
                       p_PLACE_OF_ORIGIN             IN VARCHAR2,
                       p_BEST_BY_DATE                IN DATE,
                       p_LENGTH                      IN NUMBER,
                       p_LENGTH_UOM                  IN VARCHAR2,
                       p_RECYCLED_CONTENT            IN NUMBER,
                       p_THICKNESS                   IN NUMBER,
                       p_THICKNESS_UOM               IN VARCHAR2,
                       p_WIDTH                       IN NUMBER,
                       p_WIDTH_UOM                   IN VARCHAR2,
                       p_CURL_WRINKLE_FOLD           IN VARCHAR2,
                       p_territory_code            IN VARCHAR2,
                       p_date_code                 IN VARCHAR2,
                       p_change_date               IN DATE,
                       p_age                       IN NUMBER,
                       p_item_size                 IN NUMBER,
                       p_vendor_name               IN VARCHAR2,
                       x_return_status              OUT NOCOPY VARCHAR2,
                       x_error_msg                  OUT NOCOPY VARCHAR2);

  PROCEDURE Validate_Txn_Date(p_org_id        IN  NUMBER,
                              p_txn_date      IN  DATE,
                              x_period_id     OUT NOCOPY NUMBER,
                              x_return_status OUT NOCOPY VARCHAR2,
                              x_error_msg     OUT NOCOPY VARCHAR2);

  PROCEDURE Fetch_Aggregated_Lot_Txns(p_org_id              IN         NUMBER,
                                      p_batch_id            IN         NUMBER,
                                      p_material_detail_id  IN         NUMBER,
                                      p_lot_number          IN  VARCHAR2,
                                      x_return_status       OUT NOCOPY VARCHAR2,
                                      x_error_msg           OUT NOCOPY VARCHAR2,
                                      x_txn_cursor          OUT NOCOPY t_genref);

  /* Bug 5451006: Added to used in return of the revision controlled plain items */
  PROCEDURE Fetch_Aggregated_Rev_Txns(p_org_id              IN         NUMBER,
                                      p_batch_id            IN         NUMBER,
                                      p_material_detail_id  IN         NUMBER,
                                      p_revision            IN         VARCHAR2,
                                      x_aggr_txn_qty        OUT NOCOPY NUMBER,
                                      x_aggr_txn_sec_qty    OUT NOCOPY NUMBER,
                                      x_return_status       OUT NOCOPY VARCHAR2,
                                      x_error_msg           OUT NOCOPY VARCHAR2);

  PROCEDURE Fetch_Aggregated_Txns(p_org_id              IN         NUMBER,
                                  p_batch_id            IN         NUMBER,
                                  p_material_detail_id  IN         NUMBER,
                                  x_aggr_txn_qty        OUT NOCOPY NUMBER,
                                  x_aggr_txn_sec_qty    OUT NOCOPY NUMBER,
                                  x_return_status       OUT NOCOPY VARCHAR2,
                                  x_error_msg           OUT NOCOPY VARCHAR2);

 PROCEDURE Validate_Batch_For_IB (p_organization_id IN NUMBER,
                                  p_batch_id        IN NUMBER,
                                  x_return_status   OUT NOCOPY VARCHAR2,
                                  x_error_msg       OUT NOCOPY VARCHAR2);

  PROCEDURE Validate_Item_For_IB (p_organization_id    IN NUMBER,
                                  p_batch_id           IN NUMBER,
                                  p_material_detail_id IN NUMBER,
                                  x_return_status   OUT NOCOPY VARCHAR2,
                                  x_error_msg       OUT NOCOPY VARCHAR2);


  PROCEDURE Backflush_Material (p_batch_id IN NUMBER,
                                p_material_detail_id IN NUMBER,
                                p_allow_neg_inv      IN NUMBER,
                                p_qty                IN NUMBER,
                                p_qty_type           IN NUMBER,
                                p_trans_date         IN DATE,
                                p_adjust_cmplt       IN VARCHAR2,
                                p_login_id           IN NUMBER,
                                p_user_id            IN NUMBER,
                                p_org_code           IN VARCHAR2,
                                x_return_status      OUT NOCOPY VARCHAR2,
                                x_error_msg          OUT NOCOPY VARCHAR2);

  PROCEDURE Save_Resource_Usage( p_resource_id IN NUMBER
                               , p_usage       IN VARCHAR2
                               , p_count       IN VARCHAR2
                               , p_qty         IN VARCHAR2
                               , p_uname       IN VARCHAR2
                               , p_uid         IN NUMBER
                               , x_result     OUT NOCOPY NUMBER
                               , x_error_msg  OUT NOCOPY VARCHAR2);

  PROCEDURE Fetch_Product_Pending_Lots(p_batch_id           IN   NUMBER,
                                       p_material_detail_id IN  NUMBER,
                                       p_lot_number         IN  VARCHAR2,
                                       x_return_status      OUT NOCOPY VARCHAR2,
                                       x_error_msg          OUT NOCOPY VARCHAR2,
                                       x_lot_cursor         OUT NOCOPY t_genref);

  PROCEDURE Create_Product_Pending_Lot(p_batch_id           IN   NUMBER,
                                       p_material_detail_id IN  NUMBER,
                                       p_lot_number         IN  VARCHAR2,
                                       p_revision           IN  VARCHAR2,
                                       p_sequence           IN  NUMBER,
                                       p_qty                IN  NUMBER,
                                       p_sec_qty            IN  NUMBER,
                                       p_reason_id          IN  NUMBER,
                                       p_user_id            IN  NUMBER,
                                       p_login_id           IN  NUMBER,
                                       p_org_code           IN  VARCHAR2,
                                       p_create_lot         IN  VARCHAR2,
                                       x_return_status      OUT NOCOPY VARCHAR2,
                                       x_error_msg          OUT NOCOPY VARCHAR2);
  PROCEDURE Update_Product_Pending_Lot(p_batch_id           IN  NUMBER,
                                       p_material_detail_id IN  NUMBER,
                                       p_lot_id             IN  NUMBER,
                                       p_lot_number         IN  VARCHAR2,
                                       p_sequence           IN  NUMBER,
                                       p_qty                IN  NUMBER,
                                       p_sec_qty            IN  NUMBER,
                                       p_reason_id          IN  NUMBER,
                                       p_user_id            IN  NUMBER,
                                       p_login_id           IN  NUMBER,
                                       p_org_code           IN  VARCHAR2,
                                       p_last_update_date   IN  VARCHAR2,
                                       p_is_seq_changed     IN  VARCHAR2,
                                       x_return_status      OUT NOCOPY VARCHAR2,
                                       x_error_msg          OUT NOCOPY VARCHAR2);

  PROCEDURE Populate_Dispensing_Table(
    p_material_detail_id   IN NUMBER,
    x_dispensed_count      OUT NOCOPY NUMBER,
    x_return_status        OUT NOCOPY VARCHAR2,
    x_error_msg            OUT NOCOPY VARCHAR2);

  PROCEDURE Delete_Dispensing_Record(
    p_dispense_id          IN NUMBER,
    x_return_status        OUT NOCOPY VARCHAR2,
    x_error_msg            OUT NOCOPY VARCHAR2);

  PROCEDURE Get_Dispensed_Lot_Count(
    p_subinv_code          IN VARCHAR2,
    p_locator_id           IN NUMBER,
    x_lot_count            OUT NOCOPY NUMBER,
    x_return_status        OUT NOCOPY VARCHAR2,
    x_error_msg            OUT NOCOPY VARCHAR2);

  PROCEDURE Query_Qty_Tree (p_organization_id     IN NUMBER,
                            p_inventory_item_id   IN NUMBER,
                            p_revision            IN VARCHAR2,
                            p_subinventory_code   IN VARCHAR2,
                            p_locator_id          IN NUMBER,
                            p_lot_number          IN VARCHAR2,
                            p_revision_control    IN VARCHAR2,
                            p_lot_control         IN VARCHAR2,
                            p_tree_mode           IN VARCHAR2,
                            x_qoh                 OUT NOCOPY NUMBER,
                            x_sqoh                OUT NOCOPY NUMBER,
                            x_rqoh                OUT NOCOPY NUMBER,
                            x_srqoh               OUT NOCOPY NUMBER,
                            x_qr                  OUT NOCOPY NUMBER,
                            x_sqr                 OUT NOCOPY NUMBER,
                            x_qs                  OUT NOCOPY NUMBER,
                            x_sqs                 OUT NOCOPY NUMBER,
                            x_att                 OUT NOCOPY NUMBER,
                            x_satt                OUT NOCOPY NUMBER,
                            x_atr                 OUT NOCOPY NUMBER,
                            x_satr                OUT NOCOPY NUMBER,
                            x_return_status        OUT NOCOPY VARCHAR2,
                            x_error_msg            OUT NOCOPY VARCHAR2);

  PROCEDURE Create_Qty_Tree (p_tree_mode           IN NUMBER,
                             p_organization_id     IN NUMBER,
                            --- p_batch_id            IN NUMBER,
                             ---p_material_detail_id  IN NUMBER,
                             p_inventory_item_id   IN NUMBER,
                             p_revision            IN VARCHAR2,
                             p_subinventory_code   IN VARCHAR2,
                             p_locator_id          IN NUMBER,
                            --- p_lot_number          IN VARCHAR2,
                             p_revision_control    IN VARCHAR2,
                             p_lot_control         IN VARCHAR2,
                             x_tree_id             OUT NOCOPY NUMBER,
                             x_return_status       OUT NOCOPY VARCHAR2,
                             x_error_msg           OUT NOCOPY VARCHAR2);


  PROCEDURE Query_Qty_Tree (---p_organization_id     IN NUMBER,
                            --- p_inventory_item_id   IN NUMBER,
                            p_revision            IN VARCHAR2,
                            p_subinventory_code   IN VARCHAR2,
                            p_locator_id          IN NUMBER,
                            p_lot_number          IN VARCHAR2,
                            ---p_revision_control    IN VARCHAR2,
                            ---p_lot_control         IN VARCHAR2,
                            ---p_tree_mode           IN VARCHAR2,
                            p_tree_id             IN NUMBER,
                            x_qoh                 OUT NOCOPY NUMBER,
                            x_sqoh                OUT NOCOPY NUMBER,
                            x_rqoh                OUT NOCOPY NUMBER,
                            x_srqoh               OUT NOCOPY NUMBER,
                            x_qr                  OUT NOCOPY NUMBER,
                            x_sqr                 OUT NOCOPY NUMBER,
                            x_qs                  OUT NOCOPY NUMBER,
                            x_sqs                 OUT NOCOPY NUMBER,
                            x_att                 OUT NOCOPY NUMBER,
                            x_satt                OUT NOCOPY NUMBER,
                            x_atr                 OUT NOCOPY NUMBER,
                            x_satr                OUT NOCOPY NUMBER,
                            x_return_status       OUT NOCOPY VARCHAR2,
                            x_error_msg           OUT NOCOPY VARCHAR2);

  PROCEDURE Update_Qty_Tree ( p_tree_id             IN NUMBER,
                              p_revision            IN VARCHAR2,
                              p_subinventory_code   IN VARCHAR2,
                              p_locator_id          IN NUMBER,
                              p_lot_number          IN VARCHAR2,
                              p_primary_qty         IN NUMBER,
                              p_secondary_qty       IN NUMBER,
                              p_quantity_type       IN NUMBER,
                              x_return_status       OUT NOCOPY VARCHAR2,
                              x_error_msg           OUT NOCOPY VARCHAR2);

  PROCEDURE Validate_Item_To_Issue(p_organization_id    IN NUMBER,
                                    p_batch_id           IN NUMBER,
                                    p_material_detail_id IN NUMBER,
                                    p_item_id            IN NUMBER,
                                    x_return_status   OUT NOCOPY VARCHAR2,
                                    x_error_msg       OUT NOCOPY VARCHAR2);

  PROCEDURE Validate_Item_To_Return(p_organization_id    IN NUMBER,
                                    p_batch_id           IN NUMBER,
                                    p_material_detail_id IN NUMBER,
                                    p_item_id            IN NUMBER,
                                    x_return_status   OUT NOCOPY VARCHAR2,
                                    x_error_msg       OUT NOCOPY VARCHAR2);

  PROCEDURE Validate_Prod_To_Yield (p_organization_id    IN NUMBER,
                                    p_batch_id           IN NUMBER,
                                    p_material_detail_id IN NUMBER,
                                    p_item_id            IN NUMBER,
                                    x_return_status   OUT NOCOPY VARCHAR2,
                                    x_error_msg       OUT NOCOPY VARCHAR2);

  PROCEDURE Fetch_Issue_Transactions(
                                      p_organization_id     IN         NUMBER,
                                      p_batch_id            IN         NUMBER,
                                      p_material_detail_id  IN         NUMBER,
                                      p_lot_number          IN         VARCHAR2,
                                      x_return_status       OUT NOCOPY VARCHAR2,
                                      x_error_msg           OUT NOCOPY VARCHAR2,
                                      x_txn_cursor          OUT NOCOPY t_genref);

  PROCEDURE Fetch_Yield_Transactions(
                                      p_organization_id     IN         NUMBER,
                                      p_batch_id            IN         NUMBER,
                                      p_material_detail_id  IN         NUMBER,
                                      p_lot_number          IN         VARCHAR2,
                                      p_txn_type_id         IN         NUMBER,
                                      x_return_status       OUT NOCOPY VARCHAR2,
                                      x_error_msg           OUT NOCOPY VARCHAR2,
                                      x_txn_cursor          OUT NOCOPY t_genref);

 PROCEDURE Create_Phantom_Txn (
      p_mmti_trans_id   IN              NUMBER
     ,x_return_status  OUT NOCOPY      VARCHAR2
     ,x_error_msg      OUT NOCOPY      VARCHAR2);

  PROCEDURE complete_step (p_batch_id        IN NUMBER,
                           p_step_id         IN NUMBER,
                           p_act_step_qty    IN NUMBER,
                           p_act_strt_dt     IN VARCHAR2,
                           p_act_complt_dt   IN VARCHAR2,
                           p_date_format     IN VARCHAR2,
                           p_uname           IN VARCHAR2,
                           p_uid             IN NUMBER,
                           x_return_status   OUT NOCOPY VARCHAR2,
                           x_message_count   OUT NOCOPY NUMBER,
                           x_message_list    OUT NOCOPY VARCHAR2);

  PROCEDURE End_Cmplt_Actual_Rsrc_Txn(
      p_trans_id        IN NUMBER
     ,p_organization_id IN NUMBER
     ,p_end_date        IN DATE
     ,p_reason_id       IN NUMBER
     ,p_instance_id     IN NUMBER
     ,p_trans_date      IN DATE
     ,p_uname           IN VARCHAR2
     ,p_uid             IN NUMBER
     ,x_trans_id        OUT NOCOPY NUMBER
     ,x_return_status   OUT NOCOPY VARCHAR2
     ,x_error_msg       OUT NOCOPY VARCHAR2);

  PROCEDURE get_ASQC ( p_batch_id      IN  NUMBER,
                       x_ASQC_status OUT NOCOPY NUMBER);
  PROCEDURE get_system_date(p_date_format     IN VARCHAR2,
                            x_sys_date        OUT NOCOPY VARCHAR2);
  PROCEDURE release_step ( p_batch_id        IN NUMBER,
                           p_step_id         IN NUMBER,
                           p_act_strt_dt     IN VARCHAR2,
                           p_date_format     IN VARCHAR2,
                           p_uname           IN VARCHAR2,
                           p_uid             IN NUMBER,
                           x_return_status OUT NOCOPY VARCHAR2,
                           x_message_count OUT NOCOPY NUMBER,
                           x_message_list  OUT NOCOPY VARCHAR2);
  PROCEDURE Start_Cmplt_Actual_Rsrc_Txn(
      p_resource_id     IN NUMBER
     ,p_organization_id IN NUMBER
     ,p_start_date      IN DATE
     ,p_instance_id     IN NUMBER
     ,p_trans_date      IN DATE
     ,p_reason_id       IN NUMBER
     ,p_uname           IN VARCHAR2
     ,p_uid             IN NUMBER
     ,x_trans_id        OUT NOCOPY NUMBER
     ,x_return_status   OUT NOCOPY VARCHAR2
     ,x_error_msg       OUT NOCOPY VARCHAR2);

  PROCEDURE Validate_Step_Completion_Date (p_start_date IN VARCHAR2,
                                           p_complt_date    IN  VARCHAR2,
                                           p_date_format    IN  VARCHAR2,
                                           p_batch_id      IN  NUMBER,
                                           x_return_status OUT NOCOPY VARCHAR2,
                                           x_error_msg     OUT NOCOPY VARCHAR2);
  PROCEDURE Validate_Step_Qty ( p_step_qty      IN  VARCHAR2,
                                 x_return_status OUT NOCOPY VARCHAR2,
                                 x_error_msg     OUT NOCOPY VARCHAR2);
  PROCEDURE Validate_Step_Start_Date (p_start_date    IN  VARCHAR2,
                                      p_date_format    IN  VARCHAR2,
                                      p_batch_id      IN  NUMBER,
                                      x_return_status OUT NOCOPY VARCHAR2,
                                      x_error_msg     OUT NOCOPY VARCHAR2);

  PROCEDURE check_close_period(p_org_id        IN NUMBER,
                               p_trans_date    IN DATE,
                               x_return_status OUT NOCOPY VARCHAR2,
                               x_message       OUT NOCOPY VARCHAR2);
  PROCEDURE Load_resource_Txns (p_batch_id    IN  NUMBER,
                                x_row_count   OUT NOCOPY NUMBER,
                                x_return_status OUT NOCOPY VARCHAR2);

 /* Bug#5663458
  * Created the following procedures
  */
  PROCEDURE relieve_resvns_pend_lots(p_rsrv_pndlot_id  IN  NUMBER,
                                     p_relieve_qty     IN  NUMBER,
                                     p_sec_qty         IN  NUMBER,
                                     p_line_type       IN  NUMBER,
                                     x_return_status   OUT NOCOPY VARCHAR2,
                                     x_error_msg       OUT NOCOPY VARCHAR2);
 /*
  * Bug 13334961
  * New procedure to relieve the reservations for dispense Items
  *
  */
 PROCEDURE relieve_resvns_disp_items(p_rsrv_pndlot_id  IN  NUMBER,
                                     p_relieve_qty     IN  NUMBER,
                                     p_sec_qty         IN  NUMBER,
                                     p_line_type       IN  NUMBER,
                                     x_return_status   OUT NOCOPY VARCHAR2,
                                     x_error_msg       OUT NOCOPY VARCHAR2);



  PROCEDURE fetch_txn_date(p_material_detail_id   IN          NUMBER,
                           x_trans_date           OUT NOCOPY  VARCHAR2,
                           x_return_status        OUT NOCOPY  VARCHAR2,
                           x_error_msg            OUT NOCOPY  VARCHAR2);

  PROCEDURE Validate_Subinv_Master(p_organization_id     IN         NUMBER,
                                   p_subinventory_code   IN         VARCHAR2,
                                   p_inventory_item_id   IN         NUMBER,
                                   p_restrict_code       IN         NUMBER,
                                   x_locator_type        OUT NOCOPY VARCHAR2,
                                   x_return_status       OUT NOCOPY VARCHAR2,
                                   x_error_message       OUT NOCOPY VARCHAR2);

  PROCEDURE Validate_Locator_Master(p_organization_id    IN          NUMBER,
                                   p_subinventory_code   IN          VARCHAR2,
                                   p_locator_code        IN          VARCHAR2,
                                   p_inventory_item_id   IN          NUMBER,
                                   p_restrict_code       IN          NUMBER,
                                   x_locator_id          OUT NOCOPY  VARCHAR2,
                                   x_return_status       OUT NOCOPY  VARCHAR2,
                                   x_error_message       OUT NOCOPY  VARCHAR2);

  PROCEDURE Fetch_Rev_Product_Pending_Lots(p_batch_id           IN   NUMBER,
                                           p_material_detail_id IN  NUMBER,
                                           p_lot_number         IN  VARCHAR2,
                                           p_rev_control        IN  NUMBER,
                                           x_return_status      OUT NOCOPY VARCHAR2,
                                           x_error_msg          OUT NOCOPY VARCHAR2,
                                           x_lot_cursor         OUT NOCOPY t_genref);
  /* Bug#5663458 End */
 --Bug#5867209 added restricted subinv code
 PROCEDURE Fetch_subinv_locator(p_batch_id            IN         NUMBER,
                                p_material_detail_id  IN         NUMBER,
                                x_subinventory_code   OUT NOCOPY VARCHAR2,
                                x_locator             OUT NOCOPY VARCHAR2,
                                x_locator_id          OUT NOCOPY VARCHAR2,
                                x_return_status       OUT NOCOPY VARCHAR2,
                                x_error_msg           OUT NOCOPY VARCHAR2);

-- nsinghi bug#5209065 START. Added following Procs.
PROCEDURE get_expiration_date
(
   x_expiration_date    OUT NOCOPY DATE
   , x_return_status    OUT NOCOPY VARCHAR2
) ;

PROCEDURE get_exp_action_date
(
   p_expiration_date	IN DATE
   , p_exp_act_interval IN NUMBER
   , x_exp_act_date     OUT NOCOPY DATE
   , x_return_status    OUT NOCOPY VARCHAR2
) ;
-- nsinghi bug#5209065 END.

--
-- Bug 9367054
-- New procedure to print the labels for
-- product transactions
--
PROCEDURE print_label(p_txn_header_id IN NUMBER,
                      x_return_status OUT NOCOPY VARCHAR2,
                      x_error_msg     OUT NOCOPY VARCHAR2);

END GME_MOBILE_TXN;
/
--show errors;
COMMIT;
EXIT;
