REM dbdrv: sql ~PROD ~PATH ~FILE none none none package &phase=plb \
REM dbdrv: checkfile(120.29.12010000.17=120.36.12020000.9)(120.29.12010000.15=120.36.12020000.7)(120.22.12000000.18=120.29.12010000.11)(120.22.12000000.15=120.36):~PROD:~PATH:~FILE
SET VERIFY OFF;
WHENEVER SQLERROR EXIT FAILURE ROLLBACK;
WHENEVER OSERROR EXIT FAILURE ROLLBACK;
create or replace PACKAGE BODY GME_MOBILE_TXN AS
/*  $Header: GMEMOTXB.pls 120.36.12020000.9 2016/03/01 19:53:09 adeshmuk ship $     */
/*===========================================================================+
 |      Copyright (c) 2005, 2016 Oracle Corporation, Redwood Shores, CA, USA       |
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
 | 09-Jun-06     Namit S.          Bug#5236906. Query parent lot too in      |
 |                                 Fetch_Product_Pending_Lots. Modify        |
 |				   Update_Product_Pending_Lot                |
 | 21-Jun-06     Shrikant Nene     Bug#5263908. Added revision in the proc   |
 |                                 Populate_Dispensing_Table.                |
 | 11-Jul-06     Shrikant Nene     Bug#5331639. Changed procedure            |
 |                                 Validate_Item_For_IB                      |
 | 26-Jan-07     Archana Mundhe    Bug 4774944. Modified release_step and    |
 |                                 complete_step procedure. Added call to    |
 |                                 validate step for release and complete.   |
 | 28-Feb-07     Archana Mundhe    Bug 4774944. REWORK Modified release_step |
 |                                 and complete_step procedure. Added code to|
 |                                 check for parameter step controls batch   |
 |                                 status.                                   |
 |                                                                           |
   23-Feb-09     G. Muratore   Bug 8234700. pass in false to process transactions
      and commit after. This more closely mimics how save is done on the form and
      commits all work together. GMF layers are not lost anymore.
 |   09-jun-2009 Srinivasulu Puri backport of Bug 6925025
 |   Added parameters subinventory_code and locator_id.
 |   Pass these parameters to build_txn_inter_lot call.
 |
 | 30-Mar-10     APMISHRA          Bug 9367054. Added a new procedure        |
 |                                 print_label to print labels for product   |
 |                                 transactions                              |
 | 15-Apr-10     APMISHRA          Bug 9483781. Modified the procedure       |
 |                                 Create_material_txn to process the LPN    |
 |                                 details existing for a reservation        |
 | 16-may-12     APMISHRA          Bug 13986776  Added Code to call save batch       |
 |                                                                           |
 | 08-Aug-12     APMISHRA          Bug 14376915  Modified                    |
 |               Validate_Step_Completion_Date to compare the input dates    |
 |               after converting them to the correct format                 |
 | 12-JAN-2015   Archana Mundhe    Bug 19911453 Modified release_step and    |
 |                                 complete_step procedure.gme_api_main calls|
 |                                 should use updated batch_step_out record  |
 |                                 for accurate dates. 
 | 07-Jul-15     JDEVARLA          Bug#19619931.Added lpn id as parameter to
 |                                 Create_Lot_Txn and Create_Material_Txn 
 |                                 procedure.
 | 15-Sep-15     JDEVARLA          Bug:21786445,Added lpn id as parameter to |
 |                                 Is_Material_Status_Applicable procedure   |
 | 15-Oct-15     JDEVARLA          Bug:21764324,Added parent lot number as   |
 |                                 parameter to Create_Lot_Txn procedure     |
 | 01-Mar-2016   Archana Mundhe    Bug 22753342 Modified Validate_Prod_To_Yield|
 |                                 Allow batch yield for a WIP or completed batch only|
 +===========================================================================*/

  g_debug      VARCHAR2 (5) := fnd_profile.VALUE ('AFLOG_LEVEL');
  g_pkg_name   CONSTANT VARCHAR2 (30) := 'GME_MOBILE_TXN';

 /*+========================================================================+
   | PROCEDURE NAME
   |   Get_Txn_Type
   |
   | USAGE
   |
   | ARGUMENTS
   |   p_transaction_type
   |
   | RETURNS
   |   transaction_type_id
   |
   | HISTORY
   |   Created  26-Apr-05 Eddie Oumerretane
   |
   +========================================================================+*/
  FUNCTION Get_Txn_Type(p_transaction_type_id NUMBER) RETURN NUMBER IS
    l_transaction_type_id NUMBER;
  BEGIN

    IF p_transaction_type_id = G_ING_ISSUE THEN
      l_transaction_type_id := GME_COMMON_PVT.g_ing_issue;
    ELSIF p_transaction_type_id = G_ING_RETURN THEN
      l_transaction_type_id := GME_COMMON_PVT.g_ing_return;
    ELSIF p_transaction_type_id = G_PROD_COMPLETION THEN
      l_transaction_type_id := GME_COMMON_PVT.g_prod_completion;
    ELSIF p_transaction_type_id = G_PROD_RETURN THEN
      l_transaction_type_id := GME_COMMON_PVT.g_prod_return;
    ELSIF p_transaction_type_id = G_BYPROD_COMPLETION THEN
      l_transaction_type_id := GME_COMMON_PVT.g_byprod_completion;
    ELSIF p_transaction_type_id = G_BYPROD_RETURN THEN
      l_transaction_type_id := GME_COMMON_PVT.g_byprod_return;
    END IF;

    RETURN l_transaction_type_id;

  END Get_Txn_Type;

 /* Bug#5663458
  * Created the following procedure. This procedure is used to relieve the either
  * reservations or pending lots depends on the line type that we pass
  */
  PROCEDURE relieve_resvns_pend_lots(p_rsrv_pndlot_id  IN  NUMBER,
                                     p_relieve_qty     IN  NUMBER,
                                     p_sec_qty         IN  NUMBER,
                                     p_line_type       IN  NUMBER,
                                     x_return_status   OUT NOCOPY VARCHAR2,
                                     x_error_msg       OUT NOCOPY VARCHAR2)
  IS
    l_count             NUMBER;
    no_rsrv_pndlot_id   EXCEPTION;
  BEGIN
    IF (g_debug IS NOT NULL) THEN
       gme_debug.log_initialize ('RelieveRsrvPndLots');
    END IF;

    x_return_status := FND_API.G_RET_STS_SUCCESS;
    x_error_msg     := ' ';

    IF p_rsrv_pndlot_id IS NULL THEN
      RAISE no_rsrv_pndlot_id;
    END IF;

    IF p_line_type = gme_common_pvt.g_line_type_ing THEN
     --calling gme_reservations_pvt API to relieve reservations
     gme_reservations_pvt.relieve_reservation(p_reservation_id   => p_rsrv_pndlot_id,
                                              p_prim_relieve_qty => p_relieve_qty,
                                              x_return_status    => x_return_status);

     IF x_return_status <> FND_API.G_RET_STS_SUCCESS THEN
      gme_common_pvt.count_and_get (x_count        => l_count
                                   ,p_encoded      => fnd_api.g_false
                                   ,x_data         => x_error_msg);

     END IF;
   ELSE
     gme_pending_product_lots_pvt.relieve_pending_lot(p_pending_lot_id    => p_rsrv_pndlot_id
                                                     ,p_quantity          => p_relieve_qty
                                                     ,p_secondary_quantity=> p_sec_qty
                                                     ,x_return_status     => x_return_status);

     IF x_return_status <> FND_API.G_RET_STS_SUCCESS THEN
      gme_common_pvt.count_and_get (x_count        => l_count
                                   ,p_encoded      => fnd_api.g_false
                                   ,x_data         => x_error_msg);

     END IF;
   END IF;
  EXCEPTION
   WHEN NO_RSRV_PNDLOT_ID THEN
    x_return_status := fnd_api.g_ret_sts_error;
    fnd_message.set_name('GME','INVALID_VALUE');
    x_error_msg     := fnd_message.get;
   WHEN OTHERS THEN
    IF g_debug <= gme_debug.g_log_unexpected THEN
      gme_debug.put_line('When others exception in relieve_resvns_pend_lots');
    END IF;
    fnd_msg_pub.add_exc_msg('GME_MOBILE_TXN','relieve_resvns_pend_lots');
    x_return_status := FND_API.G_RET_STS_UNEXP_ERROR;
    x_error_msg     := fnd_message.get;

  END relieve_resvns_pend_lots;

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
                                     x_error_msg       OUT NOCOPY VARCHAR2) IS
     CURSOR get_reservation_id IS
         SELECT reservation_id from gmo_material_dispenses
         WHERE dispense_id = p_rsrv_pndlot_id;

     l_reservation_id NUMBER;
     l_count             NUMBER;
  BEGIN

     OPEN get_reservation_id;
     FETCH get_reservation_id INTO l_reservation_id;
     CLOSE get_reservation_id;

     relieve_resvns_pend_lots(p_rsrv_pndlot_id => l_reservation_id,
                              p_relieve_qty    => p_relieve_qty,
                              p_sec_qty        => p_sec_qty,
                              p_line_type      => p_line_type,
                              x_return_status  => x_return_status,
                              x_error_msg      => x_error_msg);

     IF x_return_status <> FND_API.G_RET_STS_SUCCESS THEN
      gme_common_pvt.count_and_get (x_count        => l_count
                                   ,p_encoded      => fnd_api.g_false
                                   ,x_data         => x_error_msg);

     END IF;
  EXCEPTION
     WHEN OTHERS THEN
       IF g_debug <= gme_debug.g_log_unexpected THEN
         gme_debug.put_line('When others exception in relieve_resvns_disp_items');
       END IF;
       fnd_msg_pub.add_exc_msg('GME_MOBILE_TXN','relieve_resvns_disp_items');
       x_return_status := FND_API.G_RET_STS_UNEXP_ERROR;
       x_error_msg     := fnd_message.get;
  END relieve_resvns_disp_items;


 /*+========================================================================+
   | PROCEDURE NAME
   |   Get_Stacked_Messages
   |
   | USAGE
   |
   | ARGUMENTS
   |
   | RETURNS
   |   x_message
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
   |   Create_Material_Txns
   |
   | USAGE
   |
   | ARGUMENTS
   |    p_organization_id
   |    p_batch_id
   |    p_material_detail_id
   |    p_item_id
   |    p_revision
   |    p_subinventory_code
   |    p_locator_id
   |    p_txn_qty
   |    p_txn_uom_code
   |    p_sec_txn_qty
   |    p_sec_uom_code
   |    p_primary_uom_code
   |    p_txn_primary_qty
   |    p_reason_id
   |    p_txn_date
   |    p_txn_type_id
   |    p_phantom_type
   |    p_user_id
   |    p_login_id
   |    p_dispense_id
   |    p_reservation_id
   |    p_lpn_id
   |
   | RETURNS
   |   x_message
   |
   | HISTORY
   |   Created  26-Apr-05 Eddie Oumerretane
   |   Modified 15-Apr-10 APMISHRA bug#9483781. Added a new parameter
   |                      p_reservation_id to fetch and process the
   |                      LPN details against it.
   |  Modified 07-Jul-15  JDEVARLA Bug#19619931.Added a new parameter p_lpn_id
   |
   +========================================================================+*/
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
                                p_lpn_id                 IN NUMBER DEFAULT NULL)--Bug#19619931
  IS
    l_count          NUMBER;
    l_assign_phantom NUMBER;
    l_mmti_rec_in    mtl_transactions_interface%ROWTYPE;
    l_mmti_rec_out   mtl_transactions_interface%ROWTYPE;

    --
    -- bug 9483781
    -- Cursor to fetch the LPN_ID against a reservation
    --
    -- Bug#19619931- Cursor get_lpn_for_rsrv IS
    --  Select lpn_id from mtl_reservations
    --    where reservation_id = p_reservation_id;

   -- l_lpn_id NUMBER;
  BEGIN

   -- Clearing the quantity cache
   inv_quantity_tree_pub.clear_quantity_cache;

    IF (g_debug IS NOT NULL) THEN
       gme_debug.log_initialize ('MobileCreTxn');
    END IF;

    gme_common_pvt.g_user_ident := p_user_id;
    gme_common_pvt.g_login_id   := p_login_id;
     -- Bug 19619931
    gme_common_pvt.g_organization_id   := p_organization_id;
    gme_common_pvt.set_timestamp;

    x_return_status := FND_API.G_RET_STS_SUCCESS;
    x_error_msg     := ' ';

    l_mmti_rec_in.transaction_type_id := Get_Txn_Type(p_txn_type_id);

    l_mmti_rec_in.transaction_source_id          := p_batch_id;
    l_mmti_rec_in.trx_source_line_id             := p_material_detail_id;
    l_mmti_rec_in.inventory_item_id              := p_item_id;
    l_mmti_rec_in.revision                       := p_revision;
    l_mmti_rec_in.organization_id                := p_organization_id;
    l_mmti_rec_in.transaction_date               := p_txn_date;
    l_mmti_rec_in.transaction_quantity           := p_txn_qty;
    l_mmti_rec_in.primary_quantity               := p_txn_primary_qty;
    l_mmti_rec_in.reason_id                      := p_reason_id;
    l_mmti_rec_in.secondary_transaction_quantity := p_sec_txn_qty;
    l_mmti_rec_in.secondary_uom_code             := p_sec_uom_code;
    l_mmti_rec_in.transaction_uom                := p_txn_uom_code;
    l_mmti_rec_in.subinventory_code              := p_subinventory_code;
    l_mmti_rec_in.locator_id                     := p_locator_id;
    l_mmti_rec_in.transaction_source_name        := NULL;
    l_mmti_rec_in.transaction_reference          := p_dispense_id;
    l_mmti_rec_in.transaction_action_id          := NULL;
   
    --
    -- bug 9483781
    -- If the reservation exists then fetch the LPN_ID
    -- against it and pass it to the record structure.
    --
    -- Bug#19619931 IF (p_reservation_id IS NOT NULL AND
    --    p_reservation_id > 0 ) THEN
    --    OPEN get_lpn_for_rsrv;
    --    FETCH get_lpn_for_rsrv INTO l_lpn_id;
     --   CLOSE get_lpn_for_rsrv;

      --IF p_lpn_id IS NOT NULL THEN Bug#19619931
           l_mmti_rec_in.lpn_id := p_lpn_id;
        IF p_lpn_id <= 0 THEN --Bug#19619931
           l_mmti_rec_in.lpn_id := NULL;
        END IF;
        
      --  l_mmti_rec_in.lpn_id := NULL; 
 --   END IF;
 gme_debug.put_line ('LPN ID is : '|| p_lpn_id);
    l_assign_phantom := 0;

    IF p_phantom_line_id IS NOT NULL THEN
      -- This is a product of a phantom batch or a phantom ingredient
      l_assign_phantom := 1;
    END IF;

    GME_TRANSACTIONS_PVT.Build_Txn_Inter_Hdr(
                        p_mmti_rec        => l_mmti_rec_in,
                        p_assign_phantom  => l_assign_phantom,
                        x_mmti_rec        => l_mmti_rec_out,
                        x_return_status   => x_return_status);


    IF x_return_status <> FND_API.G_RET_STS_SUCCESS THEN
      -- Bug 5255933 Use GME_COMMON_PVT to get error message
      --x_error_msg     := fnd_message.get;
      GME_COMMON_PVT.count_and_get (x_count        => l_count
                                   ,p_encoded      => fnd_api.g_false
                                   ,x_data         => x_error_msg);

      x_txn_id      := -1;
      x_txn_type_id := -1;
      x_txn_header_id := -1;
    ELSE
      x_txn_id      := l_mmti_rec_out.transaction_interface_id;
      x_txn_type_id := l_mmti_rec_in.transaction_type_id;
      x_txn_header_id := l_mmti_rec_out.transaction_header_id;
    END IF;

  EXCEPTION
    WHEN OTHERS THEN
      IF g_debug <= gme_debug.g_log_unexpected THEN
        gme_debug.put_line('When others exception in Create MAterial Txn');
      END IF;
      fnd_msg_pub.add_exc_msg('GME_MOBILE_TXN','create_material_txn');
      x_return_status := FND_API.G_RET_STS_UNEXP_ERROR;
      x_error_msg     := fnd_message.get;

  END Create_Material_Txn;

 /*+========================================================================+
   | PROCEDURE NAME
   |  Create_Lot_Txn
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
                           p_lpn_id        IN NUMBER DEFAULT NULL,--Bug#19619931
						   p_parent_lot_number    IN VARCHAR2 DEFAULT NULL)--Bug#21764324
  IS
    l_mmli_rec_in  mtl_transaction_lots_interface%ROWTYPE;
    l_mmli_rec_out mtl_transaction_lots_interface%ROWTYPE;
    l_count          NUMBER;
  BEGIN

    IF (g_debug IS NOT NULL) THEN
       gme_debug.log_initialize ('MobileCreLotTxn');
    END IF;

    x_return_status := FND_API.G_RET_STS_SUCCESS;
    x_error_msg     := ' ';

    gme_common_pvt.g_user_ident := p_user_id;
    gme_common_pvt.g_login_id   := p_login_id;
    gme_common_pvt.set_timestamp;

    l_mmli_rec_in.lot_number                     := p_lot_number;
	--Bug#21764324
	l_mmli_rec_in.parent_lot_number              := p_parent_lot_number;
    l_mmli_rec_in.transaction_quantity           := p_txn_qty;
    l_mmli_rec_in.primary_quantity               := p_txn_prim_qty;
    l_mmli_rec_in.secondary_transaction_quantity := p_sec_txn_qty;
       -- Bug 6925025
       -- Added parameters subinventory_code and locator_id

    GME_TRANSACTIONS_PVT.build_txn_inter_lot(
          p_trans_inter_id        => p_txn_id,
          p_transaction_type_id   => p_txn_type_id,
          p_inventory_item_id     => p_item_id,
          p_subinventory_code     => p_subinventory_code,
          p_locator_id            => p_locator_id,
          p_lpn_id                => p_lpn_id,--Bug#19619931
          p_mmli_rec              => l_mmli_rec_in,
          x_mmli_rec              => l_mmli_rec_out,
          x_return_status         => x_return_status
           );

    IF x_return_status <> FND_API.G_RET_STS_SUCCESS THEN
       -- Bug 19619931   
       GME_COMMON_PVT.count_and_get (x_count       => l_count
                                   ,p_encoded      => fnd_api.g_false
                                   ,x_data         => x_error_msg);      
    END IF;

  EXCEPTION
    WHEN OTHERS THEN
      IF g_debug <= gme_debug.g_log_unexpected THEN
        gme_debug.put_line('When others exception in Create Lot Txn');
      END IF;
      fnd_msg_pub.add_exc_msg('GME_MOBILE_TXN','create_lot_txn');
      x_return_status := FND_API.G_RET_STS_UNEXP_ERROR;
      x_error_msg     := fnd_message.get;

  END Create_Lot_Txn;

 /*+========================================================================+
   | PROCEDURE NAME
   |  Validate_Indiv_Lot_Txn
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
  PROCEDURE Validate_Indiv_Lot_Txn(
                               p_organization_id        IN NUMBER,
                               p_item_id                IN NUMBER,
                               p_revision               IN VARCHAR2,
                               p_subinventory_code      IN VARCHAR2,
                               p_locator_id             IN NUMBER,
                               p_lot_number             IN VARCHAR2,
                               p_primary_lot_qty        IN NUMBER,
                               p_transaction_type_id    IN NUMBER,
                               x_return_status          OUT NOCOPY VARCHAR2,
                               x_error_msg              OUT NOCOPY VARCHAR2)
  IS

   l_return_status       VARCHAR2(1);
   l_msg_data            VARCHAR2(3000);
   l_msg_count           NUMBER;
   l_transaction_type_id NUMBER;

  BEGIN

    IF (g_debug IS NOT NULL) THEN
       gme_debug.log_initialize ('MobileValLotTxn');
    END IF;
    x_return_status := FND_API.G_RET_STS_SUCCESS;
    x_error_msg     := ' ';

    l_transaction_type_id := Get_Txn_Type(p_transaction_type_id);

    IF NOT (INV_LOT_API_PUB.validate_lot_indivisible(
                              p_api_version 	    =>1.0
                             ,p_init_msg_list       =>FND_API.G_FALSE
                             ,p_commit 		    =>FND_API.G_FALSE
                             ,p_validation_level    =>FND_API.G_VALID_LEVEL_FULL
                             ,p_transaction_type_id =>l_transaction_type_id
                             ,p_organization_id     =>p_organization_id
                             ,p_inventory_item_id   =>p_item_id
                             ,p_revision            =>p_revision
                             ,p_subinventory_code   =>p_subinventory_code
                             ,p_locator_id          =>p_locator_id
                             ,p_lot_number          =>p_lot_number
                             ,p_primary_quantity    =>p_primary_lot_qty
                             ,p_qoh 	            =>NULL
                             ,p_atr 	            =>NULL
                             ,x_return_status 	    =>l_return_status
                             ,x_msg_count 	    =>l_msg_count
                             ,x_msg_data 	    =>l_msg_data))

    THEN
      GME_COMMON_PVT.Count_And_Get (x_count        => l_msg_count
                                   ,p_encoded      => fnd_api.g_false
                                   ,x_data         => x_error_msg);

      x_return_status := FND_API.G_RET_STS_ERROR;
    END IF;

  EXCEPTION
    WHEN OTHERS THEN
      IF g_debug <= gme_debug.g_log_unexpected THEN
        gme_debug.put_line('When others exception in Validate Indiv Lot Txn');
      END IF;
      fnd_msg_pub.add_exc_msg('GME_MOBILE_TXN','Validate_Indiv_Lot_Txn');
      x_return_status := FND_API.G_RET_STS_UNEXP_ERROR;
      x_error_msg     := fnd_message.get;

  END Validate_Indiv_Lot_Txn;

 /*+========================================================================+
   | PROCEDURE NAME
   |  Is_Material_Status_Applicable
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
						   p_lpn_id                     IN NUMBER)--Bug:21786445
  IS

   l_is_mtl_status_applicable VARCHAR2(1);
   l_transaction_type_id NUMBER;
   l_object_type         VARCHAR2(1);
   l_type                VARCHAR2(100);
   l_item                VARCHAR2(100);
   l_locator             VARCHAR2(100);

  BEGIN

    IF (g_debug IS NOT NULL) THEN
       gme_debug.log_initialize ('MobileValMtlSts');
    END IF;

    
    x_return_status := FND_API.G_RET_STS_SUCCESS;
    x_error_msg     := ' ';

    l_transaction_type_id := Get_Txn_Type(p_transaction_type_id);

    gme_debug.put_line('In Is_Material_Status_Applicable:');
    gme_debug.put_line('p_transaction_type_id is:'|| p_transaction_type_id);
    gme_debug.put_line('p_lot_status_enabled is:'|| p_lot_status_enabled);
    gme_debug.put_line('p_organization_id:'|| p_organization_id );
    gme_debug.put_line('p_inventory_item_id:'|| p_inventory_item_id );
    gme_debug.put_line('p_sub_code:' || p_sub_code );
    gme_debug.put_line('p_locator_id:' || p_locator_id );
    gme_debug.put_line('p_lot_number:'|| p_lot_number );
    gme_debug.put_line('p_object_type:'|| p_object_type );
    gme_debug.put_line('l_transaction_type_id:'|| l_transaction_type_id );
    gme_debug.put_line('p_lpn_id:'|| p_lpn_id);
        
    --- Bug 5255933 use object type 'A'to check material status of all entities
    --- i.e sub, locator, lot, serial
    /*
    IF p_sub_code IS NOT NULL THEN
      l_object_type := 'Z';
    END IF;

    IF p_locator_id IS NOT NULL THEN
      l_object_type := 'L';
    END IF;

    IF p_lot_number IS NOT NULL THEN
      l_object_type := 'O';
    END IF;
    */

    ---l_object_type := 'A';
    gme_debug.put_line('l_is_mtl_status_applicable before:'|| l_is_mtl_status_applicable );

    l_is_mtl_status_applicable :=
      INV_MATERIAL_STATUS_GRP.is_status_applicable(p_wms_installed => NULL,
                           p_trx_status_enabled   => NULL,
                           p_trx_type_id          => l_transaction_type_id,
                           p_lot_status_enabled    =>p_lot_status_enabled,
                           p_serial_status_enabled => 'N',
                           p_organization_id       => p_organization_id,
                           p_inventory_item_id   => p_inventory_item_id,
                           p_sub_code            => p_sub_code,
                           p_locator_id          => p_locator_id,
                           p_lot_number          => p_lot_number,
                           p_serial_number       => NULL,
                           p_object_type         => p_object_type,
                           p_lpn_id              => p_lpn_id);--Bug:21786445

    gme_debug.put_line('l_is_mtl_status_applicable after:'|| l_is_mtl_status_applicable );
     
    IF l_is_mtl_status_applicable <> 'Y' THEN

       SELECT transaction_type_name
         INTO   l_type
         FROM   mtl_transaction_types
         WHERE  transaction_type_id = p_transaction_type_id;

       SELECT substr(concatenated_segments,1,100)
           INTO l_item
           FROM mtl_system_items_kfv
           WHERE organization_id = p_organization_id
             AND inventory_item_id = p_inventory_item_id;

       IF p_object_type = 'O' THEN --- Lot
          gme_debug.put_line('display error GME_MATERIAL_STS_INV_LOT');
          FND_MESSAGE.SET_NAME('GME','GME_MATERIAL_STS_INV_LOT');
          FND_MESSAGE.SET_TOKEN('TRANSTYPE', l_type);
          FND_MESSAGE.SET_TOKEN('ITEM', l_item);
          FND_MESSAGE.SET_TOKEN('LOT', p_lot_number);
       ELSIF p_object_type = 'Z' THEN --- Subinventory
          FND_MESSAGE.SET_NAME('GME','GME_MATERIAL_STS_INV_SUB');
          FND_MESSAGE.SET_TOKEN('TRANSTYPE', l_type);
          FND_MESSAGE.SET_TOKEN('ITEM', l_item);
          FND_MESSAGE.SET_TOKEN('SUBINV', p_sub_code);
       ELSIF p_object_type = 'L' THEN --- Locator
          SELECT substr(concatenated_segments,1,100)
           INTO l_locator
           FROM wms_item_locations_kfv
           WHERE organization_id = p_organization_id
             AND subinventory_code = p_sub_code
             AND inventory_location_id  = p_locator_id;

          FND_MESSAGE.SET_NAME('GME','GME_MATERIAL_STS_INV_SUB_LOC');
          FND_MESSAGE.SET_TOKEN('TRANSTYPE', l_type);
          FND_MESSAGE.SET_TOKEN('SUBINV', p_sub_code);
          FND_MESSAGE.SET_TOKEN('ITEM', l_item); --Bug#5658050
          FND_MESSAGE.SET_TOKEN('LOCN',l_locator);
       END IF;

       x_return_status := FND_API.G_RET_STS_ERROR;
       x_error_msg := fnd_message.get;

    END IF;

  EXCEPTION
    WHEN OTHERS THEN
      IF g_debug <= gme_debug.g_log_unexpected THEN
        gme_debug.put_line('When others exception in Is_Material_Status_Applicable');
      END IF;
      fnd_msg_pub.add_exc_msg('GME_MOBILE_TXN','Is_Material_Status_Applicable');
      x_return_status := FND_API.G_RET_STS_UNEXP_ERROR;
      x_error_msg     := fnd_message.get;

  END Is_Material_Status_Applicable;

 /*+========================================================================+
   | PROCEDURE NAME
   |  Fetch_Aggregated_Txns
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
  PROCEDURE Fetch_Aggregated_Txns(p_org_id              IN         NUMBER,
                                  p_batch_id            IN         NUMBER,
                                  p_material_detail_id  IN         NUMBER,
                                  x_aggr_txn_qty        OUT NOCOPY NUMBER,
                                  x_aggr_txn_sec_qty    OUT NOCOPY NUMBER,
                                  x_return_status       OUT NOCOPY VARCHAR2,
                                  x_error_msg           OUT NOCOPY VARCHAR2)
  IS

   CURSOR Get_Agg_Txn IS
      SELECT   ABS(SUM (NVL(primary_quantity,0))),
               ABS(SUM (NVL(secondary_transaction_quantity,0)))
      FROM  mtl_material_transactions
      WHERE  organization_id = p_org_id
        AND transaction_source_id = p_batch_id
        AND trx_source_line_id = p_material_detail_id
        AND transaction_source_type_id = gme_common_pvt.g_txn_source_type;
  BEGIN

    IF (g_debug IS NOT NULL) THEN
       gme_debug.log_initialize ('MobileFetchAgrTxns');
    END IF;


    x_return_status := FND_API.G_RET_STS_SUCCESS;
    x_error_msg     := ' ';

    OPEN Get_Agg_Txn;
    FETCH Get_Agg_Txn INTO x_aggr_txn_qty, x_aggr_txn_sec_qty;
    CLOSE Get_Agg_Txn;

  EXCEPTION
    WHEN OTHERS THEN
      IF g_debug <= gme_debug.g_log_unexpected THEN
        gme_debug.put_line('When others exception in fetch aggrageted txns');
      END IF;
      fnd_msg_pub.add_exc_msg('GME_MOBILE_TXN','fetch_aggregated_txns');
      x_return_status := FND_API.G_RET_STS_UNEXP_ERROR;
      x_error_msg     := fnd_message.get;

  END Fetch_Aggregated_Txns;

 /*+========================================================================+
   | PROCEDURE NAME
   |  Fetch_Aggregated_Lot_Txns
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
  PROCEDURE Fetch_Aggregated_Lot_Txns(p_org_id              IN         NUMBER,
                                      p_batch_id            IN         NUMBER,
                                      p_material_detail_id  IN         NUMBER,
                                      p_lot_number          IN  VARCHAR2,
                                      x_return_status       OUT NOCOPY VARCHAR2,
                                      x_error_msg           OUT NOCOPY VARCHAR2,
                                      x_txn_cursor          OUT NOCOPY t_genref)
  IS
  BEGIN

    IF (g_debug IS NOT NULL) THEN
       gme_debug.log_initialize ('MobileFetchAgrLotTxns');
    END IF;


    x_return_status := FND_API.G_RET_STS_SUCCESS;
    x_error_msg     := ' ';

    OPEN x_txn_cursor FOR
             SELECT m.revision,lot_number,
                    ABS(SUM (NVL(l.primary_quantity,0))),
                    ABS(SUM (NVL(l.secondary_transaction_quantity,0)))
             FROM  mtl_material_transactions m,
                   mtl_transaction_lot_numbers l
            WHERE l.transaction_id = m.transaction_id
              AND lot_number = NVL(p_lot_number, l.lot_number)
              AND m.organization_id = p_org_id
              AND m.transaction_source_id = p_batch_id
              AND m.trx_source_line_id = p_material_detail_id
              AND m.transaction_source_type_id = gme_common_pvt.g_txn_source_type
         GROUP BY m.revision,l.lot_number;

  EXCEPTION
    WHEN OTHERS THEN
      IF g_debug <= gme_debug.g_log_unexpected THEN
        gme_debug.put_line('When others exception in fetch aggrageted lot txns');
      END IF;
      fnd_msg_pub.add_exc_msg('GME_MOBILE_TXN','fetch_aggregated_lot_txns');
      x_return_status := FND_API.G_RET_STS_UNEXP_ERROR;
      x_error_msg     := fnd_message.get;

  END Fetch_Aggregated_Lot_Txns;

  /* Bug 5451006: Added to used in return of the revision controlled plain items */
 /*+========================================================================+
   | PROCEDURE NAME
   |  Fetch_Aggregated_Rev_Txns
   |
   | USAGE
   |
   | ARGUMENTS
   |
   | RETURNS
   |
   | HISTORY
   |   Created  01-Sep-06 Shrikant Nene
   |
   +========================================================================+*/
  PROCEDURE Fetch_Aggregated_Rev_Txns(p_org_id              IN         NUMBER,
                                      p_batch_id            IN         NUMBER,
                                      p_material_detail_id  IN         NUMBER,
                                      p_revision            IN         VARCHAR2,
                                      x_aggr_txn_qty        OUT NOCOPY NUMBER,
                                      x_aggr_txn_sec_qty    OUT NOCOPY NUMBER,
                                      x_return_status       OUT NOCOPY VARCHAR2,
                                      x_error_msg           OUT NOCOPY VARCHAR2)
  IS
   CURSOR Get_Agg_Txn IS
             SELECT ABS(SUM (NVL(primary_quantity,0))),
                    ABS(SUM (NVL(secondary_transaction_quantity,0)))
             FROM  mtl_material_transactions
            WHERE organization_id = p_org_id
              AND transaction_source_id = p_batch_id
              AND trx_source_line_id = p_material_detail_id
              AND transaction_source_type_id = gme_common_pvt.g_txn_source_type
              AND revision = p_revision
         GROUP BY revision;
  BEGIN

    IF (g_debug IS NOT NULL) THEN
       gme_debug.log_initialize ('MobileFetchAgrRevTxns');
    END IF;


    x_return_status := FND_API.G_RET_STS_SUCCESS;
    x_error_msg     := ' ';

    OPEN Get_Agg_Txn;
    FETCH Get_Agg_Txn INTO x_aggr_txn_qty, x_aggr_txn_sec_qty;
    CLOSE Get_Agg_Txn;


  EXCEPTION
    WHEN OTHERS THEN
      IF g_debug <= gme_debug.g_log_unexpected THEN
        gme_debug.put_line('When others exception in fetch aggrageted lot txns');
      END IF;
      fnd_msg_pub.add_exc_msg('GME_MOBILE_TXN','fetch_aggregated_rev_txns');
      x_return_status := FND_API.G_RET_STS_UNEXP_ERROR;
      x_error_msg     := fnd_message.get;

  END Fetch_Aggregated_Rev_Txns;

 /*+========================================================================+
   | PROCEDURE NAME
   |   Fetch_Material_Transactions
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
  PROCEDURE Fetch_Material_Transactions(p_organization_id     IN         NUMBER,
                                      p_batch_id            IN         NUMBER,
                                      p_material_detail_id  IN         NUMBER,
                                      p_txn_type_id         IN  NUMBER,
                                      x_return_status       OUT NOCOPY VARCHAR2,
                                      x_error_msg           OUT NOCOPY VARCHAR2,
                                      x_txn_cursor          OUT NOCOPY t_genref)
  IS
    l_date_format VARCHAR2(100);
    l_txn_type_id NUMBER;
  BEGIN

    IF (g_debug IS NOT NULL) THEN
       gme_debug.log_initialize ('MobileFetchMtl');
    END IF;

    l_txn_type_id := Get_Txn_Type(p_txn_type_id);

    x_return_status := FND_API.G_RET_STS_SUCCESS;
    x_error_msg     := ' ';

    FND_PROFILE.GET('MWA_DATE_FORMAT_MASK',l_date_format);

    IF l_date_format IS NULL THEN
      FND_PROFILE.GET('ICX_DATE_FORMAT_MASK',l_date_format);
    END IF;

    OPEN x_txn_cursor FOR
       SELECT
             m.transaction_id,
             m.transaction_quantity*(-1),
             m.primary_quantity *(-1),
             m.secondary_transaction_quantity *(-1),
             m.transaction_uom,
             m.subinventory_code,
             m.locator_id,
             m.reason_id,
             l.concatenated_segments,
             TO_CHAR(transaction_date, l_date_format),
             revision
           FROM mtl_material_transactions m,
                wms_item_locations_kfv l
          WHERE trx_source_line_id = NVL(p_material_detail_id, trx_source_line_id)
            AND transaction_source_id = p_batch_id
            AND transaction_type_id = l_txn_type_id
            AND l.inventory_location_id = m.locator_id(+);

  EXCEPTION
    WHEN OTHERS THEN
      IF g_debug <= gme_debug.g_log_unexpected THEN
        gme_debug.put_line('When others exception in fetch Mtl Transactions');
      END IF;
      fnd_msg_pub.add_exc_msg('GME_MOBILE_TXN','fetch_material_transactions');
      x_return_status := FND_API.G_RET_STS_UNEXP_ERROR;
      x_error_msg     := fnd_message.get;

  END Fetch_Material_Transactions;

 /*+========================================================================+
   | PROCEDURE NAME
   |   Fetch_Lot_Transactions
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
  PROCEDURE Fetch_Lot_Transactions(
                                   p_transaction_id IN NUMBER,
                                   p_lot_number     IN VARCHAR2,
                                   x_return_status  OUT NOCOPY VARCHAR2,
                                   x_error_msg      OUT NOCOPY VARCHAR2,
                                   x_txn_cursor     OUT NOCOPY t_genref)
  IS
  BEGIN

    IF (g_debug IS NOT NULL) THEN
       gme_debug.log_initialize ('MobileFetchLotTxn');
    END IF;

    x_return_status := FND_API.G_RET_STS_SUCCESS;
    x_error_msg     := ' ';

    OPEN x_txn_cursor FOR
       SELECT
         lot_number,
         transaction_quantity *(-1),
         primary_quantity *(-1),
         secondary_transaction_quantity *(-1)
        FROM mtl_transaction_lot_numbers
        WHERE transaction_id = p_transaction_id AND
              lot_number = NVL(p_lot_number, lot_number);

  EXCEPTION
    WHEN OTHERS THEN
      IF g_debug <= gme_debug.g_log_unexpected THEN
        gme_debug.put_line('When others exception in fetch Mtl Lot Transactions');
      END IF;
      fnd_msg_pub.add_exc_msg('GME_MOBILE_TXN','fetch_material_lot_transactions');
      x_return_status := FND_API.G_RET_STS_UNEXP_ERROR;
      x_error_msg     := fnd_message.get;

  END Fetch_Lot_Transactions;

 /*+========================================================================+
   | PROCEDURE NAME
   |  Process_Interface_Txn
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
  PROCEDURE Process_Interface_Txn( p_txn_header_id IN NUMBER,
                                   p_user_id       IN NUMBER,
                                   p_login_id      IN NUMBER,
                                   x_return_status OUT NOCOPY VARCHAR2,
                                   x_error_msg     OUT NOCOPY VARCHAR2)
  IS
   l_msg_count      NUMBER;
   l_msg_data       VARCHAR2 (2000);
   l_trans_count    NUMBER;

  BEGIN

    IF (g_debug IS NOT NULL) THEN
       gme_debug.log_initialize ('MobileProcessTxn');
    END IF;

    gme_common_pvt.g_user_ident := p_user_id;
    gme_common_pvt.g_login_id   := p_login_id;
    gme_common_pvt.set_timestamp;

    -- Bug 8234700 - Let's pass in false to process transactions and commit after
    -- This more closely mimics how save is done on the form and commits all work together.
    GME_TRANSACTIONS_PVT.Process_Transactions
                 (p_api_version           => 2.0,
                  p_init_msg_list         => fnd_api.g_false,
--                  p_commit                => fnd_api.g_true,
                  p_commit                => fnd_api.g_false,
                  p_validation_level      => fnd_api.g_valid_level_full,
                  p_table                 => 1, -- Source table is Interface
                  p_header_id             => p_txn_header_id,
                  x_return_status         => x_return_status,
                  x_msg_count             => l_msg_count,
                  x_msg_data              => l_msg_data,
                  x_trans_count           => l_trans_count);

         IF (x_return_status <> fnd_api.g_ret_sts_success) THEN
           --x_error_msg     := fnd_message.get;
           x_error_msg     := l_msg_data;
         /* Bug 5438355: Added success message. */
         ELSE
            gme_common_pvt.log_message('PM_SAVED_CHANGES');
            gme_common_pvt.count_and_get(p_encoded => FND_API.G_FALSE,
                                   x_count   => l_msg_count,
                                   x_data    => x_error_msg);
            -- Bug 8234700 - Let's commit the work done by process transactions.
            COMMIT;
         END IF;

    --- Reseting this global variable. I guess this should be done in
    --- GME_TRANSACTIONS_PVT.Process_Transactions
    GME_COMMON_PVT.g_transaction_header_id := NULL;

  EXCEPTION
    WHEN OTHERS THEN
      IF g_debug <= gme_debug.g_log_unexpected THEN
        gme_debug.put_line('When others exception in Process_Transactions');
      END IF;
      fnd_msg_pub.add_exc_msg('GME_MOBILE_TXN','process_transactions');
      x_return_status := FND_API.G_RET_STS_UNEXP_ERROR;
      x_error_msg     := fnd_message.get;

  END Process_Interface_Txn;

 /*+========================================================================+
   | PROCEDURE NAME
   |  VAlidate_Child_Lot
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
  PROCEDURE Validate_Child_Lot (
                         p_org_id                      IN  NUMBER
                       , p_inventory_item_id           IN  NUMBER
                       , p_parent_lot_number           IN  VARCHAR2
                       , p_lot_number                  IN  VARCHAR2
                       , x_return_status               OUT NOCOPY VARCHAR2
                       , x_error_msg                   OUT NOCOPY VARCHAR2)

IS
 l_api_version     NUMBER ;
 l_init_msg_list   VARCHAR2(50) ;
 l_commit          VARCHAR2 (50) ;
 l_return_status   VARCHAR2 (50) ;
 l_msg_count       NUMBER ;
 l_msg_data        VARCHAR2(3000) ;
BEGIN

    x_return_status := fnd_api.G_RET_STS_SUCCESS;
    l_api_version              := 1.0;
    l_init_msg_list            := fnd_api.g_false;
    l_commit                   := fnd_api.g_false;

    INV_LOT_API_PUB.validate_child_lot (
             x_return_status          =>    l_return_status
           , x_msg_count              =>    l_msg_count
           , x_msg_data               =>    l_msg_data
           , p_api_version            =>    l_api_version
           , p_init_msg_list          =>    l_init_msg_list
           , p_commit                 =>    l_commit
           , p_organization_id        =>    p_org_id
           , p_inventory_item_id      =>    p_inventory_item_id
           , p_parent_lot_number      =>    p_parent_lot_number
           , p_child_lot_number       =>    p_lot_number
          )  ;

      IF l_return_status = FND_API.G_RET_STS_UNEXP_ERROR THEN
           FND_MESSAGE.SET_NAME('INV','INV_PROGRAM_ERROR');
           FND_MESSAGE.SET_TOKEN('PGM_NAME','INV_LOT_API_PUB.VALIDATE_CHILD_LOT');
           fnd_msg_pub.ADD;
           RAISE fnd_api.g_exc_unexpected_error;
      END IF;

      IF l_return_status = FND_API.G_RET_STS_ERROR THEN
           RAISE fnd_api.g_exc_error;
      END IF;



 EXCEPTION
    WHEN NO_DATA_FOUND THEN
      x_return_status  := FND_API.G_RET_STS_ERROR;

      fnd_msg_pub.count_and_get(p_encoded => fnd_api.g_false,
                                  p_count => l_msg_count,
                                   p_data => x_error_msg);
      if( l_msg_count > 1 ) then
          x_error_msg:= fnd_msg_pub.get(l_msg_count, FND_API.G_FALSE);
      end if;

   WHEN FND_API.G_EXC_ERROR THEN
      x_return_status := FND_API.G_RET_STS_ERROR;

      fnd_msg_pub.count_and_get(p_encoded => fnd_api.g_false,
                                   p_count => l_msg_count,
                                    p_data => l_msg_data);
      if( l_msg_count > 1 ) then
          x_error_msg := fnd_msg_pub.get(l_msg_count, FND_API.G_FALSE);
      end if;

   WHEN FND_API.G_EXC_UNEXPECTED_ERROR THEN
       x_return_status := FND_API.G_RET_STS_UNEXP_ERROR;

      fnd_msg_pub.count_and_get(p_encoded => fnd_api.g_false,
                                  p_count => l_msg_count,
                                   p_data => l_msg_data);
      if( l_msg_count > 1 ) then
          x_error_msg := fnd_msg_pub.get(l_msg_count, FND_API.G_FALSE);
      end if;


    WHEN OTHERS THEN
      x_return_status  := fnd_api.g_ret_sts_error;

      fnd_msg_pub.count_and_get(p_encoded => fnd_api.g_false,
                                  p_count => l_msg_count,
                                   p_data => l_msg_data);
      if( l_msg_count > 1 ) then
          x_error_msg := fnd_msg_pub.get(l_msg_count, FND_API.G_FALSE);
      end if;

  END Validate_Child_Lot;

 /*+========================================================================+
   | PROCEDURE NAME
   |  Generate_Lot_Number
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
  PROCEDURE Generate_Lot_Number(
                         p_org_id                      IN  NUMBER
                       , p_inventory_item_id           IN  NUMBER
                       , p_parent_lot_number           IN  VARCHAR2
                       , x_lot_number                  OUT NOCOPY VARCHAR2
                       , x_return_status               OUT NOCOPY VARCHAR2
                       , x_error_msg                   OUT NOCOPY VARCHAR2)
  IS

    l_msg_count       NUMBER;

  BEGIN

    IF (g_debug IS NOT NULL) THEN
       gme_debug.log_initialize ('MobileGenLotNumber');
    END IF;

    x_return_status := fnd_api.G_RET_STS_SUCCESS;
    x_error_msg := '';

    x_lot_number := INV_LOT_API_PUB.auto_gen_lot
                        (p_org_id                => p_org_id,
                         p_inventory_item_id     => p_inventory_item_id,
                         p_lot_generation        => NULL,
                         p_lot_uniqueness        => NULL,
                         p_lot_prefix            => NULL,
                         p_zero_pad              => NULL,
                         p_lot_length            => NULL,
                         p_transaction_date      => NULL,
                         p_revision              => NULL,
                         p_subinventory_code          => NULL,
                         p_locator_id                 => NULL,
                         p_transaction_type_id        => NULL,
                         p_transaction_action_id      => NULL,
                         p_transaction_source_type_id => NULL,
                         p_lot_number                 => NULL,
                         p_api_version                => 1.0,
                         p_init_msg_list              => FND_API.G_FALSE,
                         p_commit                     => FND_API.G_FALSE,
                         p_validation_level           => NULL,
                         p_parent_lot_number          => p_parent_lot_number,
                         x_return_status              => x_return_status,
                         x_msg_count                  => l_msg_count,
                         x_msg_data                    => x_error_msg);

               IF x_return_status <> fnd_api.g_ret_sts_success THEN
                  RAISE fnd_api.g_exc_error;
               END IF;

               IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
                  gme_debug.put_line (   'lot_gen'
                                      || ':'
                                      || 'x_lot_number'
                                      || x_return_status);
               END IF;

   EXCEPTION
      WHEN fnd_api.g_exc_error THEN
         IF g_debug <= gme_debug.g_log_unexpected THEN
            gme_debug.put_line (   'GME_MOBILE_TXNS'
                                || '.'
                                || 'Generate_Lot_Number'
                                || ':'
                                || 'When others exception:'
                                || SQLERRM);
         END IF;

         x_return_status := fnd_api.g_ret_sts_error;

      WHEN OTHERS THEN
         IF g_debug <= gme_debug.g_log_unexpected THEN
            gme_debug.put_line (   'GME_MOBILE_TXNS'
                                || '.'
                                || 'Generate_Lot_Number'
                                || ':'
                                || 'When others exception:'
                                || SQLERRM);
         END IF;

         gme_common_pvt.count_and_get (x_count        => l_msg_count,
                                       p_encoded      => fnd_api.g_false,
                                       x_data         => x_error_msg);
         x_return_status := fnd_api.g_ret_sts_unexp_error;

  END Generate_Lot_Number;

 /*+========================================================================+
   | PROCEDURE NAME
   |  Create_Lot
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
                       p_COLOR                     IN VARCHAR2,
                       p_VOLUME                    IN NUMBER,
                       p_VOLUME_UOM                IN VARCHAR2,
                       p_PLACE_OF_ORIGIN           IN VARCHAR2,
                       p_BEST_BY_DATE              IN DATE,
                       p_LENGTH                    IN NUMBER,
                       p_LENGTH_UOM                IN VARCHAR2,
                       p_RECYCLED_CONTENT          IN NUMBER,
                       p_THICKNESS                 IN NUMBER,
                       p_THICKNESS_UOM             IN VARCHAR2,
                       p_WIDTH                     IN NUMBER,
                       p_WIDTH_UOM                 IN VARCHAR2,
                       p_CURL_WRINKLE_FOLD         IN VARCHAR2,
                       p_territory_code            IN VARCHAR2,
                       p_date_code                 IN VARCHAR2,
                       p_change_date               IN DATE,
                       p_age                       IN NUMBER,
                       p_item_size                 IN NUMBER,
                       p_vendor_name               IN VARCHAR2,
                       x_return_status              OUT NOCOPY VARCHAR2,
                       x_error_msg                  OUT NOCOPY VARCHAR2)

 IS
    l_return_status            VARCHAR2(1)  ;
    l_msg_data                 VARCHAR2(3000)  ;
    l_msg_count                NUMBER    ;
    x_lot_rec                  MTL_LOT_NUMBERS%ROWTYPE;  -- for lot api
    l_in_lot_rec               MTL_LOT_NUMBERS%ROWTYPE;  -- for lot api
    l_api_version              NUMBER;
    l_init_msg_list            VARCHAR2(100);
    l_commit                   VARCHAR2(100);
    l_validation_level         NUMBER;
    l_source                   NUMBER;
    l_row_id ROWID;

   /* Cursor to check if a lot already exists*/
    CURSOR c_lot_exists IS
    SELECT 1
         FROM mtl_lot_numbers
         WHERE organization_id    = p_org_id
         AND   inventory_item_id  = p_inventory_item_id
         AND   lot_number         = p_lot_number ;

   l_lot_count  NUMBER := 0;

BEGIN

    OPEN c_lot_exists;
    FETCH c_lot_exists INTO l_lot_count;
    CLOSE c_lot_exists;


    l_return_status  := NULL;
    l_msg_data       := NULL;
    l_msg_count      := NULL;
    l_source                                 :=   NULL ;
    l_api_version                            :=   1.0;
    l_init_msg_list                          :=   'T';
    l_commit                                 :=   'F';
    l_validation_level                       :=   100;
    l_in_lot_rec.organization_id             :=   p_org_id  ;
    l_in_lot_rec.inventory_item_id           :=   p_inventory_item_id ;
    l_in_lot_rec.expiration_date             :=   p_expiration_date;
    l_in_lot_rec.grade_code                  :=   p_grade_code ;
    l_in_lot_rec.lot_number                  :=   p_lot_number ;
    l_in_lot_rec.parent_lot_number           :=   p_parent_lot_number;
    l_in_lot_rec.origination_date            :=   p_ORIGINATION_DATE;
    l_in_lot_rec.retest_date                 :=   p_RETEST_DATE ;
    l_in_lot_rec.maturity_date               :=   P_MATURITY_DATE;
    l_in_lot_rec.attribute_category          :=   P_ATTRIBUTE_CATEGORY;
    l_in_lot_rec.origination_type            :=   P_ORIGINATION_TYPE;
    l_in_lot_rec.hold_date                   :=   P_HOLD_DATE;
    l_in_lot_rec.expiration_action_code      :=   P_EXPIRATION_ACTION_CODE;
    l_in_lot_rec.expiration_action_date      :=   P_EXPIRATION_ACTION_DATE;
    l_in_lot_rec.status_id                   :=   P_STATUS_ID;
    l_in_lot_rec.supplier_lot_number         :=   P_SUPPLIER_LOT_NUMBER;
    l_in_lot_rec.LOT_ATTRIBUTE_CATEGORY      :=   P_LOT_ATTRIBUTE_CATEGORY;
    l_in_lot_rec.ATTRIBUTE1:= P_ATTRIBUTE1;
    l_in_lot_rec.ATTRIBUTE2:= P_ATTRIBUTE2;
    l_in_lot_rec.ATTRIBUTE3:= P_ATTRIBUTE3;
    l_in_lot_rec.ATTRIBUTE4:= P_ATTRIBUTE4;
    l_in_lot_rec.ATTRIBUTE5:= P_ATTRIBUTE5;
    l_in_lot_rec.ATTRIBUTE6:= P_ATTRIBUTE6;
    l_in_lot_rec.ATTRIBUTE7:= P_ATTRIBUTE7;
    l_in_lot_rec.ATTRIBUTE8:= P_ATTRIBUTE8;
    l_in_lot_rec.ATTRIBUTE9:= P_ATTRIBUTE9;
    l_in_lot_rec.ATTRIBUTE10:= P_ATTRIBUTE10;
    l_in_lot_rec.ATTRIBUTE11:= P_ATTRIBUTE11;
    l_in_lot_rec.ATTRIBUTE12:= P_ATTRIBUTE12;
    l_in_lot_rec.ATTRIBUTE13:= P_ATTRIBUTE13;
    l_in_lot_rec.ATTRIBUTE14:= P_ATTRIBUTE14;
    l_in_lot_rec.ATTRIBUTE15:= P_ATTRIBUTE15;
    l_in_lot_rec.C_ATTRIBUTE1:= P_C_ATTRIBUTE1;
    l_in_lot_rec.C_ATTRIBUTE2:= P_C_ATTRIBUTE2;
    l_in_lot_rec.C_ATTRIBUTE3:= P_C_ATTRIBUTE3;
    l_in_lot_rec.C_ATTRIBUTE4:= P_C_ATTRIBUTE4;
    l_in_lot_rec.C_ATTRIBUTE5:= P_C_ATTRIBUTE5;
    l_in_lot_rec.C_ATTRIBUTE6:= P_C_ATTRIBUTE6;
    l_in_lot_rec.C_ATTRIBUTE7:= P_C_ATTRIBUTE7;
    l_in_lot_rec.C_ATTRIBUTE8:= P_C_ATTRIBUTE8;
    l_in_lot_rec.C_ATTRIBUTE9:= P_C_ATTRIBUTE9;
    l_in_lot_rec.C_ATTRIBUTE10:= P_C_ATTRIBUTE10;
    l_in_lot_rec.C_ATTRIBUTE11:= P_C_ATTRIBUTE11;
    l_in_lot_rec.C_ATTRIBUTE12:= P_C_ATTRIBUTE12;
    l_in_lot_rec.C_ATTRIBUTE13:= P_C_ATTRIBUTE13;
    l_in_lot_rec.C_ATTRIBUTE14:= P_C_ATTRIBUTE14;
    l_in_lot_rec.C_ATTRIBUTE15:= P_C_ATTRIBUTE15;
    l_in_lot_rec.C_ATTRIBUTE16:= P_C_ATTRIBUTE16;
    l_in_lot_rec.C_ATTRIBUTE17:= P_C_ATTRIBUTE17;
    l_in_lot_rec.C_ATTRIBUTE18:= P_C_ATTRIBUTE18;
    l_in_lot_rec.C_ATTRIBUTE19:= P_C_ATTRIBUTE19;
    l_in_lot_rec.C_ATTRIBUTE20:= P_C_ATTRIBUTE20;
    l_in_lot_rec.D_ATTRIBUTE1:= P_D_ATTRIBUTE1;
    l_in_lot_rec.D_ATTRIBUTE2:= P_D_ATTRIBUTE2;
    l_in_lot_rec.D_ATTRIBUTE3:= P_D_ATTRIBUTE3;
    l_in_lot_rec.D_ATTRIBUTE4:= P_D_ATTRIBUTE4;
    l_in_lot_rec.D_ATTRIBUTE5:= P_D_ATTRIBUTE5;
    l_in_lot_rec.D_ATTRIBUTE6:= P_D_ATTRIBUTE6;
    l_in_lot_rec.D_ATTRIBUTE7:= P_D_ATTRIBUTE7;
    l_in_lot_rec.D_ATTRIBUTE8:= P_D_ATTRIBUTE8;
    l_in_lot_rec.D_ATTRIBUTE9:= P_D_ATTRIBUTE9;
    l_in_lot_rec.D_ATTRIBUTE10:= P_D_ATTRIBUTE10;
    l_in_lot_rec.N_ATTRIBUTE1:= P_N_ATTRIBUTE1;
    l_in_lot_rec.N_ATTRIBUTE2:= P_N_ATTRIBUTE2;
    l_in_lot_rec.N_ATTRIBUTE3:= P_N_ATTRIBUTE3;
    l_in_lot_rec.N_ATTRIBUTE4:= P_N_ATTRIBUTE4;
    l_in_lot_rec.N_ATTRIBUTE5:= P_N_ATTRIBUTE5;
    l_in_lot_rec.N_ATTRIBUTE6:= P_N_ATTRIBUTE6;
    l_in_lot_rec.N_ATTRIBUTE7:= P_N_ATTRIBUTE7;
    l_in_lot_rec.N_ATTRIBUTE8:= P_N_ATTRIBUTE8;
    l_in_lot_rec.N_ATTRIBUTE9:= P_N_ATTRIBUTE9;
    l_in_lot_rec.N_ATTRIBUTE10:= P_N_ATTRIBUTE10;
    l_in_lot_rec.disable_flag                :=   p_disable_flag ;
    l_in_lot_rec.date_code                   :=   p_date_code;
    l_in_lot_rec.change_date                 :=   p_change_date ;
    l_in_lot_rec.age                         :=   p_age ;
    l_in_lot_rec.item_size                   :=   p_item_size ;
    l_in_lot_rec.color                       :=   p_color ;
    l_in_lot_rec.volume                      :=   p_volume ;
    l_in_lot_rec.volume_uom                  :=   p_volume_uom ;
    l_in_lot_rec.place_of_origin             :=   p_place_of_origin ;
    l_in_lot_rec.best_by_date                :=   p_best_by_date ;
    l_in_lot_rec.length                      :=   p_length ;
    l_in_lot_rec.length_uom                  :=   p_length_uom ;
    l_in_lot_rec.recycled_content            :=   p_recycled_content ;
    l_in_lot_rec.thickness                   :=   p_thickness ;
    l_in_lot_rec.thickness_uom               :=   p_thickness_uom ;
    l_in_lot_rec.width                       :=   p_width ;
    l_in_lot_rec.width_uom                   :=   p_width_uom ;
    l_in_lot_rec.territory_code              :=   p_territory_code ;
    l_in_lot_rec.vendor_name                 :=   p_vendor_name ;

    l_row_id := NULL;

    IF l_lot_count = 0   THEN

       INV_LOT_API_PUB.Create_Inv_lot(
                x_return_status     =>     l_return_status
              , x_msg_count         =>     l_msg_count
              , x_msg_data          =>     x_error_msg
              , x_lot_rec           =>     x_lot_rec
              , p_lot_rec           =>     l_in_lot_rec
              , p_source            =>     l_source
              , p_api_version       =>     l_api_version
              , p_init_msg_list     =>     l_init_msg_list
              , p_commit            =>     l_commit
              , p_validation_level  =>     l_validation_level
              , p_origin_txn_id     =>     NULL
              , x_row_id            =>     l_row_id
               );

          IF l_return_status <> 'S' THEN
            FND_MSG_PUB.count_and_get
            (   p_count  => l_msg_count
              , p_data   => x_error_msg
            );
          END IF;

    END IF;

    x_return_status   := l_return_status ;

  END Create_Lot;

  /*+========================================================================+
   | PROCEDURE NAME
   |  Validate_Txn_date
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
  PROCEDURE Validate_Txn_Date(p_org_id        IN  NUMBER,
                              p_txn_date      IN  DATE,
                              x_period_id     OUT NOCOPY NUMBER,
                              x_return_status OUT NOCOPY VARCHAR2,
                              x_error_msg     OUT NOCOPY VARCHAR2) IS
   l_open_past_period BOOLEAN;
   DATE_ERROR         EXCEPTION;
 BEGIN

   IF (g_debug IS NOT NULL) THEN
      gme_debug.log_initialize ('MobileValTxnDate');
   END IF;

   x_return_status := FND_API.G_RET_STS_SUCCESS;
   x_error_msg     := ' ';

   --- Check that transaction date is not in the future
   IF p_txn_date > SYSDATE THEN
     fnd_message.set_name('INV', 'INV_INT_TDATEEX');
     RAISE DATE_ERROR;
   END IF;

   --- Check that transaction date falls within an open period
   INVTTMTX.tdatechk(org_id             => p_org_id
                   , transaction_date   => p_txn_date
                   , period_id          => x_period_id
                   , open_past_period   => l_open_past_period);

   IF x_period_id <= 0 THEN
     fnd_message.set_name('INV', 'INV_NO_OPEN_PERIOD');
     RAISE DATE_ERROR;
   END IF;


  EXCEPTION
    WHEN DATE_ERROR THEN
     x_period_id     := -1;
     x_error_msg     := fnd_message.get;
     x_return_status := FND_API.G_RET_STS_ERROR;

    WHEN OTHERS THEN
      IF g_debug <= gme_debug.g_log_unexpected THEN
        gme_debug.put_line('When others exception in Validate Txn Date');
      END IF;
      fnd_msg_pub.add_exc_msg('GME_MOBILE_TXN','Validate_Txn_Date');
      x_return_status := FND_API.G_RET_STS_UNEXP_ERROR;
      x_error_msg     := fnd_message.get;

 END Validate_Txn_Date;

 /*+========================================================================+
   | PROCEDURE NAME
   |  Validate_Batch_For_IB
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
 PROCEDURE Validate_Batch_For_IB (p_organization_id IN NUMBER,
                                  p_batch_id        IN NUMBER,
                                  x_return_status   OUT NOCOPY VARCHAR2,
                                  x_error_msg       OUT NOCOPY VARCHAR2) IS

    CURSOR Cur_batch_no IS
      SELECT b.parentline_id
      FROM   gme_batch_header_vw b
      WHERE  batch_type = 0
      AND    organization_id = p_organization_id
      AND    batch_id = p_batch_id;

    CURSOR cur_get_step_status IS
         SELECT s.batchstep_no
         FROM   gme_batch_steps s,
                gme_batch_step_items i,
                gme_material_details m
         WHERE  m.batch_id = p_batch_id
         AND    s.batch_id = p_batch_id
         AND    i.batch_id = p_batch_id
         AND    m.release_type = 2
         AND    s.batchstep_id = i.batchstep_id
         AND    i.material_detail_id = m.material_detail_id
         AND    s.step_status = 4;

    l_batchstep_no   NUMBER ;
    l_parent_line_id NUMBER;
    BATCH_NOT_VALID  EXCEPTION;

  BEGIN

   IF (g_debug IS NOT NULL) THEN
      gme_debug.log_initialize ('MobileValBatchForIB');
   END IF;

   x_return_status := FND_API.G_RET_STS_SUCCESS;
   x_error_msg     := ' ';

   OPEN Cur_batch_no;
   FETCH Cur_batch_no INTO l_parent_line_id;
   IF Cur_batch_no%NOTFOUND THEN
     FND_MESSAGE.SET_NAME('GME', 'PM_INVBATCHNO');
     FND_MESSAGE.SET_TOKEN('STEP_NO', l_batchstep_no);
     CLOSE Cur_batch_no;
     RAISE BATCH_NOT_VALID;
   END IF;

   CLOSE Cur_batch_no;

   -- 2402919 Dont allow IB on batch that has any incrmental rel
   -- item associated to a closed step.
   OPEN cur_get_step_status;
   FETCH cur_get_step_status INTO l_batchstep_no;
   IF cur_get_step_status%FOUND THEN
     FND_MESSAGE.SET_NAME('GME','GME_STEP_CLOSED_ERR');
     FND_MESSAGE.SET_TOKEN('STEP_NO', l_batchstep_no);
     CLOSE cur_get_step_status;
     RAISE BATCH_NOT_VALID;
   END IF;

   CLOSE cur_get_step_status;

   IF l_parent_line_id > 0 THEN
     FND_MESSAGE.SET_NAME('GME', 'PM_INVALID_PHANTOM_ACTION');
     RAISE BATCH_NOT_VALID;
   END IF;


  EXCEPTION
    WHEN BATCH_NOT_VALID THEN
      x_return_status := FND_API.G_RET_STS_ERROR;
      x_error_msg     := fnd_message.get;

    WHEN OTHERS THEN
      IF g_debug <= gme_debug.g_log_unexpected THEN
        gme_debug.put_line('When others exception in Validate Batch For IB');
      END IF;
      fnd_msg_pub.add_exc_msg('GME_MOBILE_TXN','Validate_Batch_For_IB');
      x_return_status := FND_API.G_RET_STS_UNEXP_ERROR;
      x_error_msg     := fnd_message.get;

  END Validate_Batch_For_IB;

 /*+========================================================================+
   | PROCEDURE NAME
   |  Validate_Item_For_IB
   |
   | USAGE
   |
   | ARGUMENTS
   |
   | RETURNS
   |
   | HISTORY
   |   Created  26-Apr-05 Eddie Oumerretane
   | 11-Jul-06     Shrikant Nene     Bug#5331639. Compare wip_plan_qty instead
   |                                 of plan_qty
   |
   +========================================================================+*/
  PROCEDURE Validate_Item_For_IB (p_organization_id    IN NUMBER,
                                  p_batch_id           IN NUMBER,
                                  p_material_detail_id IN NUMBER,
                                  x_return_status   OUT NOCOPY VARCHAR2,
                                  x_error_msg       OUT NOCOPY VARCHAR2) IS

    CURSOR Cur_batch_det IS
      SELECT release_type,
      -- Bug#5331639. Compare wip_plan_qty instead of plan_qty
             NVL(wip_plan_qty, 0) wip_planned_qty,
             NVL(phantom_type, 0) phantom_ind,
             phantom_line_id
      FROM   gme_material_details
      WHERE  material_detail_id = p_material_detail_id
      AND    organization_id = p_organization_id
      AND    batch_id = p_batch_id;

    l_mat_rec Cur_batch_det%ROWTYPE;
    ITEM_NOT_VALID  EXCEPTION;

  BEGIN

   IF (g_debug IS NOT NULL) THEN
      gme_debug.log_initialize ('MobileValItemForIB');
   END IF;

   x_return_status := FND_API.G_RET_STS_SUCCESS;
   x_error_msg     := ' ';

   OPEN Cur_batch_det;
   FETCH Cur_batch_det INTO l_mat_rec;
   CLOSE Cur_batch_det;

   -- If the ingredient is a phantom and it has not been exploded then we
   -- cannot consume it.

   IF l_mat_rec.phantom_ind <> 0 THEN
     IF l_mat_rec.phantom_line_id IS NULL THEN
       FND_MESSAGE.SET_NAME('GME', 'GME_ISSUE_PHANTOM_NOT_EXPLOD');
       RAISE ITEM_NOT_VALID;
     END IF;
   END IF;

   IF l_mat_rec.release_type IN (0,3) THEN
     FND_MESSAGE.SET_NAME('GME', 'GME_API_INV_RELEASE_TYPE');
     RAISE ITEM_NOT_VALID;
   END IF;

   -- Bug#5331639. Compare wip_plan_qty instead of plan_qty
   IF l_mat_rec.wip_planned_qty = 0 THEN
     FND_MESSAGE.SET_NAME('GME', 'GME_API_INV_PLAN_QTY_PC');
     RAISE ITEM_NOT_VALID;
   END IF;


  EXCEPTION
    WHEN ITEM_NOT_VALID THEN
      x_return_status := FND_API.G_RET_STS_ERROR;
      x_error_msg     := fnd_message.get;

    WHEN OTHERS THEN
      IF g_debug <= gme_debug.g_log_unexpected THEN
        gme_debug.put_line('When others exception in Validate Item For IB');
      END IF;
      fnd_msg_pub.add_exc_msg('GME_MOBILE_TXN','Validate_Item_For_IB');
      x_return_status := FND_API.G_RET_STS_UNEXP_ERROR;
      x_error_msg     := fnd_message.get;

  END Validate_Item_For_IB;


 /*+========================================================================+
   | PROCEDURE NAME
   |  Backflush_Material
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
                                x_error_msg          OUT NOCOPY VARCHAR2) IS

    l_msg_count			NUMBER;
    l_msg_list			VARCHAR2(2000);
    l_msg_data			VARCHAR2(2000);
    l_msg_index			NUMBER;
    l_batch_header     		GME_BATCH_HEADER%ROWTYPE;
    l_material_details 		GME_MATERIAL_DETAILS%ROWTYPE;
    l_exception_material_tab	GME_COMMON_PVT.exceptions_tab;

    BACKFLUSH_ERROR EXCEPTION;

  BEGIN

   IF (g_debug IS NOT NULL) THEN
      gme_debug.log_initialize ('MobileBackflushMtl');
      gme_debug.put_line('BatchId id    =  '||p_batch_id);
      gme_debug.put_line('Mtl DetailId  =  '||p_material_detail_id);
      gme_debug.put_line('Backflush Qty =  '||p_qty);
      gme_debug.put_line('Qty Type      =  '||p_qty_type);
      gme_debug.put_line('Org Code      =  '||p_org_code);
   END IF;

   fnd_profile.put('USER_ID',to_char(p_user_id));
   fnd_profile.put('LOGIN_ID',to_char(p_login_id));

   x_return_status := FND_API.G_RET_STS_SUCCESS;
   x_error_msg     := ' ';

   l_material_details.material_detail_id := p_material_detail_id;
   l_batch_header.batch_id               := p_batch_id;

   GME_API_PUB.Incremental_Backflush
     (p_api_version            => 2.0
     ,p_validation_level       => gme_common_pvt.g_max_errors
     ,p_init_msg_list          => fnd_api.g_false
     ,p_commit                 => fnd_api.g_true
     ,x_message_count          => l_msg_count
     ,x_message_list           => l_msg_list
     ,x_return_status          => x_return_status
     ,p_batch_header_rec       => l_batch_header
     ,p_org_code               => p_org_code
     ,p_material_detail_rec    => l_material_details
     ,p_qty                    => p_qty
     ,p_qty_type               => p_qty_type
     ,p_trans_date             => p_trans_date
     ,p_ignore_exception       => fnd_api.g_false  --- Do not proceed if exceptios
     ,p_adjust_cmplt           => fnd_api.g_false --- Completed batches not supported
     ,x_exception_material_tbl => l_exception_material_tab);

   IF (g_debug IS NOT NULL) THEN
      gme_debug.put_line('Return Status =  '||x_return_status);
      gme_debug.put_line('Error Msg     =  '||l_msg_list);
   END IF;

   IF x_return_status = 'X' THEN
     FND_MESSAGE.SET_NAME('GME','GME_INCR_BACKFLUSH_EXCEPTIONS');
     x_error_msg := FND_MESSAGE.GET;
     RAISE BACKFLUSH_ERROR;
   ELSIF x_return_status <> 'S' THEN
     FND_MSG_PUB.GET(p_msg_index	=> 1,
                     p_data		=> x_error_msg,
                     p_encoded	        => 'F',
		     p_msg_index_out	=> l_msg_index);
     RAISE BACKFLUSH_ERROR;
   /* Bug 5438355: Added success message */
   ELSE
     x_error_msg := l_msg_list;
   END IF;

  EXCEPTION
   WHEN BACKFLUSH_ERROR THEN
      x_return_status := FND_API.G_RET_STS_ERROR;
      ROLLBACK;

   WHEN OTHERS THEN
      IF g_debug <= gme_debug.g_log_unexpected THEN
        gme_debug.put_line('When others exception in Backflush Material');
      END IF;
      fnd_msg_pub.add_exc_msg('GME_MOBILE_TXN','Backflush_Material');
      x_return_status := FND_API.G_RET_STS_UNEXP_ERROR;
      x_error_msg     := fnd_message.get;
      ROLLBACK;

  END Backflush_Material;

 /*+========================================================================+
   | PROCEDURE NAME
   |  Save_Resource_Usage
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
  PROCEDURE Save_Resource_Usage( p_resource_id IN NUMBER
                               , p_usage       IN VARCHAR2
                               , p_count       IN VARCHAR2
                               , p_qty         IN VARCHAR2
                               , p_uname       IN VARCHAR2
                               , p_uid         IN NUMBER
                               , x_result     OUT NOCOPY NUMBER
                               , x_error_msg  OUT NOCOPY VARCHAR2
  )
  IS
    l_msg_count     NUMBER;
    l_msg_list      VARCHAR2(2000);
    l_return_status VARCHAR2(1);

    i               NUMBER;
    message         VARCHAR2(2000);
    dummy           NUMBER;
    l_input_rec     GME_BATCH_STEP_RESOURCES%ROWTYPE;
    l_output_rec    GME_BATCH_STEP_RESOURCES%ROWTYPE;
  BEGIN

     IF (g_debug IS NOT NULL) THEN
       gme_debug.log_initialize ('MobileSaveRsrcUsage');
     END IF;

     fnd_profile.put('USER_ID',to_char(p_uid));
     gme_common_pvt.g_user_ident := p_uid;

     l_input_rec.batchstep_resource_id := p_resource_id;
     l_input_rec.actual_rsrc_count := to_number(p_count);
     l_input_rec.actual_rsrc_usage := to_number(p_usage);
     l_input_rec.actual_rsrc_qty   := to_number(p_qty);
     l_input_rec.last_update_date  := SYSDATE;
     l_input_rec.last_updated_by   := p_uid;


   GME_API_PUB.Update_Batchstep_Resource
     ( p_api_version            => 2.0
     , p_init_msg_list          => FND_API.G_TRUE
     , p_commit                 => FND_API.G_TRUE
     , p_batchstep_resource_rec => l_input_rec
     , x_batchstep_resource_rec => l_output_rec
     , x_message_count          => l_msg_count
     , x_message_list           => l_msg_list
     , x_return_status          => l_return_status
     );

     IF l_msg_count > 0
     THEN
       FOR i IN 1..l_msg_count
       LOOP
         FND_MSG_PUB.get
         ( p_msg_index    => i
         , p_data          => message
         , p_encoded       => fnd_api.g_false
         , p_msg_index_out => dummy
         );

         IF i = 1
         THEN
           x_error_msg := message;
         END IF;

       END LOOP;
     END IF;

     IF l_return_status = 'S' THEN
         x_result := 0;
     ELSIF l_return_status = 'E' THEN
         x_result := -1;
     ELSIF l_return_status = 'U' THEN
         x_result := -2;
     ELSE
         x_result := -3;
     END IF;

  EXCEPTION
   WHEN OTHERS THEN
      IF g_debug <= gme_debug.g_log_unexpected THEN
        gme_debug.put_line('When others exception in Save Resource Usage');
      END IF;
      x_result := -4;
      fnd_msg_pub.add_exc_msg('GME_MOBILE_TXN','Save_Resource_Usage');
      x_error_msg     := fnd_message.get;
      ROLLBACK;

  END Save_Resource_Usage;

 /*+========================================================================+
   | PROCEDURE NAME
   |  Fetch_Product_Pending_Lots
   |
   | USAGE
   |
   | ARGUMENTS
   |
   | RETURNS
   |
   | HISTORY
   |   Created  26-Apr-05 Eddie Oumerretane
   |   Bug#5236906. 09-Jun-06 Namit Singhi. Modified query to give Parent Lot too.
   |
   +========================================================================+*/
  PROCEDURE Fetch_Product_Pending_Lots(p_batch_id           IN   NUMBER,
                                       p_material_detail_id IN  NUMBER,
                                       p_lot_number         IN  VARCHAR2,
                                       x_return_status      OUT NOCOPY VARCHAR2,
                                       x_error_msg          OUT NOCOPY VARCHAR2,
                                       x_lot_cursor         OUT NOCOPY t_genref)
  IS
    l_date_format VARCHAR2(100);
  BEGIN

    IF (g_debug IS NOT NULL) THEN
       gme_debug.log_initialize ('MobileFetchPendingLot');
    END IF;

    x_return_status := FND_API.G_RET_STS_SUCCESS;
    x_error_msg     := ' ';

    FND_PROFILE.GET('MWA_DATE_FORMAT_MASK',l_date_format);

    IF l_date_format IS NULL THEN
      FND_PROFILE.GET('ICX_DATE_FORMAT_MASK',l_date_format);
    END IF;

    OPEN x_lot_cursor FOR
       SELECT
         l.pending_product_lot_id,
         l.lot_number,
	 n.parent_lot_number, --nsinghi bug#5236906. Add this column
         l.revision,
         l.sequence,
         l.quantity,
         NVL(l.secondary_quantity, 0),
         NVL (l.reason_id, -1),
         reason_name,
         TO_CHAR(l.last_update_date, 'MM/DD/YYYY HH24:MI:SS')
    FROM
       gme_pending_product_lots l,
       mtl_transaction_reasons r,
       gme_material_details m, -- nsinghi bug#5236906. Add join to gme_material_details and MLN
       mtl_lot_numbers n
    WHERE
       l.batch_id = p_batch_id AND
       l.material_detail_id = p_material_detail_id AND
       l.lot_number = NVL(p_lot_number, l.lot_number) AND
       m.material_detail_id = l.material_detail_id AND -- nsinghi bug#5236906. Added where conditions
       m.inventory_item_id = n.inventory_item_id AND
       m.organization_id = n.organization_id AND
       l.lot_number = n.lot_number AND
       l.reason_id = r.reason_id(+);

  EXCEPTION
    WHEN OTHERS THEN
      IF g_debug <= gme_debug.g_log_unexpected THEN
        gme_debug.put_line('When others exception in fetch Pending Lots');
      END IF;
      fnd_msg_pub.add_exc_msg('GME_MOBILE_TXN','fetch_pending_product_lots');
      x_return_status := FND_API.G_RET_STS_UNEXP_ERROR;
      x_error_msg     := fnd_message.get;

  END Fetch_Product_Pending_Lots;


  PROCEDURE Fetch_Rev_Product_Pending_Lots(p_batch_id           IN   NUMBER,
                                       p_material_detail_id     IN  NUMBER,
                                       p_lot_number             IN  VARCHAR2,
                                       p_rev_control            IN  NUMBER,
                                       x_return_status          OUT NOCOPY VARCHAR2,
                                       x_error_msg              OUT NOCOPY VARCHAR2,
                                       x_lot_cursor             OUT NOCOPY t_genref)
  IS
    l_date_format VARCHAR2(100);
  BEGIN

    IF (g_debug IS NOT NULL) THEN
       gme_debug.log_initialize ('MobileFetchRevPendingLot');
    END IF;

    x_return_status := FND_API.G_RET_STS_SUCCESS;
    x_error_msg     := ' ';

    FND_PROFILE.GET('MWA_DATE_FORMAT_MASK',l_date_format);

    IF l_date_format IS NULL THEN
      FND_PROFILE.GET('ICX_DATE_FORMAT_MASK',l_date_format);
    END IF;

    OPEN x_lot_cursor FOR
       SELECT
         l.pending_product_lot_id,
         l.lot_number,
	      n.parent_lot_number,
         l.revision,
         l.sequence,
         l.quantity,
         NVL(l.secondary_quantity, 0),
         NVL (l.reason_id, -1),
         reason_name,
         TO_CHAR(l.last_update_date, 'MM/DD/YYYY HH24:MI:SS')
    FROM
       gme_pending_product_lots l,
       mtl_transaction_reasons r,
       gme_material_details m,
       mtl_lot_numbers n
    WHERE
       l.batch_id = p_batch_id AND
       l.material_detail_id = p_material_detail_id AND
       l.lot_number = NVL(p_lot_number, l.lot_number) AND
       (p_rev_control = 0 OR (p_rev_control = 1 AND l.revision IS NOT NULL)) AND
       m.material_detail_id = l.material_detail_id AND
       m.inventory_item_id = n.inventory_item_id AND
       m.organization_id = n.organization_id AND
       l.lot_number = n.lot_number AND
       l.reason_id = r.reason_id(+);

  EXCEPTION
    WHEN OTHERS THEN
      IF g_debug <= gme_debug.g_log_unexpected THEN
        gme_debug.put_line('When others exception in fetch Rev Pending Lots');
      END IF;
      fnd_msg_pub.add_exc_msg('GME_MOBILE_TXN','fetch_rev_pending_product_lots');
      x_return_status := FND_API.G_RET_STS_UNEXP_ERROR;
      x_error_msg     := fnd_message.get;

  END Fetch_Rev_Product_Pending_Lots;

 /*+========================================================================+
   | PROCEDURE NAME
   |  Create_Product_Pending_Lots
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
                                       x_error_msg          OUT NOCOPY VARCHAR2)
  IS
      l_pending_product_lots_rec     gme_pending_product_lots%ROWTYPE;
      l_out_pending_product_lots_rec gme_pending_product_lots%ROWTYPE;
      l_material_detail_rec          gme_material_details%ROWTYPE;
      l_in_material_detail_rec       gme_material_details%ROWTYPE;
      l_batch_header_rec             gme_batch_header%ROWTYPE;
      l_message_count	             NUMBER;
      l_message_list	             VARCHAR2(2000);
      l_create_lot                   VARCHAR2(6);
      l_duom_ctl		     VARCHAR2(30);

    -- nsinghi bug#5236702. Set secondary qty only if item is DUOM controlled.
    CURSOR get_item_duom_ctl IS
      SELECT NVL(tracking_quantity_ind, 'P')
      FROM mtl_system_items_b msi, gme_material_details gmd
      WHERE gmd.inventory_item_id = msi.inventory_item_id AND
            gmd.organization_id = msi.organization_id AND
	    gmd.material_detail_id = p_material_detail_id;

  BEGIN

    IF p_create_lot = 'TRUE' THEN
      l_create_lot := fnd_api.g_true;
    ELSE
      l_create_lot := fnd_api.g_false;
    END IF;

    IF (g_debug IS NOT NULL) THEN
       gme_debug.log_initialize ('MobileCrePendingLot');
       gme_debug.put_line('New lot            = ' || p_create_lot);
       gme_debug.put_line('Material Detail Id = ' || p_material_detail_id);
       gme_debug.put_line('Sequence           = ' || p_sequence);
       gme_debug.put_line('Revision           = ' || p_revision);
       gme_debug.put_line('Lot number         = ' || p_lot_number);
       gme_debug.put_line('Quantity           = ' || p_qty);
       gme_debug.put_line('Sec quantity       = ' || p_sec_qty);
       gme_debug.put_line('Reason_id          = ' || p_reason_id);
       gme_debug.put_line('User Id            = ' || p_user_id);
       gme_debug.put_line('Login Id           = ' || p_login_id);
    END IF;

    x_return_status := FND_API.G_RET_STS_SUCCESS;
    x_error_msg     := ' ';

    fnd_profile.put('USER_ID',to_char(p_user_id));
    fnd_profile.put('LOGIN_ID',to_char(p_login_id));

    -- nsinghi bug#5236702
    OPEN get_item_duom_ctl;
    FETCH get_item_duom_ctl INTO l_duom_ctl;
    CLOSE get_item_duom_ctl;

    ---l_pending_product_lots_rec.batch_id           := p_batch_id;
    l_pending_product_lots_rec.material_detail_id := p_material_detail_id;
    l_pending_product_lots_rec.sequence           := p_sequence;
    l_pending_product_lots_rec.revision           := p_revision;
    l_pending_product_lots_rec.lot_number         := p_lot_number;
    l_pending_product_lots_rec.quantity           := p_qty;
    -- nsinghi bug#5236702 For non-duom item, value for p_sec_qty from mobile layer is 0.0
    -- sec_qty field is defined as double in mobile layer and initialized to 0.0 as double cannot take null.
    IF l_duom_ctl = 'PS' THEN
	l_pending_product_lots_rec.secondary_quantity := p_sec_qty;
    ELSE
	l_pending_product_lots_rec.secondary_quantity := NULL;
    END IF;

    l_pending_product_lots_rec.reason_id          := p_reason_id;

    GME_API_PUB.create_pending_product_lot
     (p_api_version  	 => 2.0
     ,p_validation_level => gme_common_pvt.g_max_errors
     ,p_init_msg_list    => fnd_api.g_true
     ,p_commit           => fnd_api.g_true
     ,x_message_count    => l_message_count
     ,x_message_list     => x_error_msg
     ,x_return_status    => x_return_status
     ,p_batch_header_rec => l_batch_header_rec
     ,p_org_code         => p_org_code
     ,p_create_lot       => l_create_lot
     ,p_material_detail_rec => l_material_detail_rec
     ,p_pending_product_lots_rec => l_pending_product_lots_rec
     ,x_pending_product_lots_rec => l_out_pending_product_lots_rec);


  EXCEPTION
    WHEN OTHERS THEN
        FND_MSG_PUB.count_and_get
        (   p_count  => l_message_count
          , p_data   => x_error_msg
        );
      IF g_debug <= gme_debug.g_log_unexpected THEN
        gme_debug.put_line('When others exception in Create Pending Lots');
      END IF;
      fnd_msg_pub.add_exc_msg('GME_MOBILE_TXN','create_pending_product_lots');
      x_return_status := FND_API.G_RET_STS_UNEXP_ERROR;
      x_error_msg     := fnd_message.get;

  END Create_Product_Pending_Lot;

 /*+========================================================================+
   | PROCEDURE NAME
   |  Update_Product_Pending_Lot
   |
   | USAGE
   |
   | ARGUMENTS
   |
   | RETURNS
   |
   | HISTORY
   |   Created  26-Apr-05 Eddie Oumerretane
   |   Bug#5236906. 09-Jun-06 Namit S. Send Lot Number too when updating Pending Lot.
   |
   +========================================================================+*/
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
                                       x_error_msg          OUT NOCOPY VARCHAR2)
  IS
      l_pending_product_lots_rec     gme_pending_product_lots%ROWTYPE;
      l_out_pending_product_lots_rec gme_pending_product_lots%ROWTYPE;
      l_material_detail_rec          gme_material_details%ROWTYPE;
      l_in_material_detail_rec       gme_material_details%ROWTYPE;
      l_batch_header_rec             gme_batch_header%ROWTYPE;
      l_message_count	             NUMBER;
      l_message_list	             VARCHAR2(2000);
      l_duom_ctl		     VARCHAR2(30);

    -- nsinghi bug#5236702. Set secondary qty only if item is DUOM controlled.
    CURSOR get_item_duom_ctl IS
      SELECT NVL(tracking_quantity_ind, 'P')
      FROM mtl_system_items_b msi, gme_material_details gmd
      WHERE gmd.inventory_item_id = msi.inventory_item_id AND
            gmd.organization_id = msi.organization_id AND
	    gmd.material_detail_id = p_material_detail_id;

  BEGIN

    IF (g_debug IS NOT NULL) THEN
       gme_debug.log_initialize ('MobileUpdatePendingLot');
    END IF;

    x_return_status := FND_API.G_RET_STS_SUCCESS;
    x_error_msg     := ' ';

    fnd_profile.put('USER_ID',to_char(p_user_id));
    fnd_profile.put('LOGIN_ID',to_char(p_login_id));

    IF p_sequence IS NULL THEN
      --- Set sequence to the last value
      l_pending_product_lots_rec.sequence := FND_API.G_MISS_NUM;
    ELSE
      IF p_is_seq_changed = 'Y' THEN
        l_pending_product_lots_rec.sequence := p_sequence;
      ELSE
        l_pending_product_lots_rec.sequence := NULL;
      END IF;
    END IF;

    -- nsinghi bug#5236702
    OPEN get_item_duom_ctl;
    FETCH get_item_duom_ctl INTO l_duom_ctl;
    CLOSE get_item_duom_ctl;

    ---l_pending_product_lots_rec.batch_id               := p_batch_id;
    l_pending_product_lots_rec.lot_number             := p_lot_number; -- nsinghi bug#5236906. Uncommented this line.
    l_pending_product_lots_rec.pending_product_lot_id := p_lot_id;
    l_pending_product_lots_rec.material_detail_id     := p_material_detail_id;
    l_pending_product_lots_rec.quantity               := p_qty;
    -- sec_qty field is defined as double in mobile layer and initialized to 0.0 as double cannot take null.
    IF l_duom_ctl = 'PS' THEN
	l_pending_product_lots_rec.secondary_quantity := p_sec_qty;
    ELSE
	l_pending_product_lots_rec.secondary_quantity := NULL;
    END IF;

    IF p_reason_id IS NULL THEN
      l_pending_product_lots_rec.reason_id := FND_API.G_MISS_NUM;
    ELSE
      l_pending_product_lots_rec.reason_id := p_reason_id;
    END IF;

    l_pending_product_lots_rec.last_update_date := to_date(p_last_update_date, 'MM/DD/YYYY HH24:MI:SS');

    gme_debug.put_line('Update Pending Lot: ');
    gme_debug.put_line('Last Upd Date = '|| to_char(l_pending_product_lots_rec.last_update_date, 'MM/DD/YYYY HH24:MI:SS'));
    gme_debug.put_line('Sequence Chg  = '|| p_is_seq_changed);
    gme_debug.put_line('Sequence      = '|| l_pending_product_lots_rec.sequence);
    gme_debug.put_line('Lot id        = '|| l_pending_product_lots_rec.pending_product_lot_id);
    gme_debug.put_line('Line Id       = '|| l_pending_product_lots_rec.material_detail_id);
    gme_debug.put_line('Qty           = '|| l_pending_product_lots_rec.quantity);
    gme_debug.put_line('Sec Qty       = '|| l_pending_product_lots_rec.secondary_quantity);
    gme_debug.put_line('Reason id     = '|| l_pending_product_lots_rec.reason_id);

    GME_API_PUB.update_pending_product_lot
     (p_api_version  	 => 2.0
     ,p_validation_level => gme_common_pvt.g_max_errors
     ,p_init_msg_list    => fnd_api.g_true
     ,p_commit           => fnd_api.g_true
     ,x_message_count    => l_message_count
     ,x_message_list     => l_message_list
     ,x_return_status    => x_return_status
     ,p_batch_header_rec => l_batch_header_rec
     ,p_org_code         => p_org_code
     ,p_material_detail_rec => l_material_detail_rec
     ,p_pending_product_lots_rec => l_pending_product_lots_rec
     ,x_pending_product_lots_rec => l_out_pending_product_lots_rec);

     IF x_return_status <> 'S' THEN
        FND_MSG_PUB.count_and_get
        (   p_count  => l_message_count
          , p_data   => x_error_msg
        );
     /* Bug 5438355: Added success message */
     ELSE
        x_error_msg := l_message_list;
     END IF;

  EXCEPTION
    WHEN OTHERS THEN
      IF g_debug <= gme_debug.g_log_unexpected THEN
        gme_debug.put_line('When others exception in Update Pending Lots');
        FND_MSG_PUB.count_and_get
        (   p_count  => l_message_count
          , p_data   => x_error_msg
        );
        gme_debug.put_line('Error = '||x_error_msg);
      END IF;
      fnd_msg_pub.add_exc_msg('GME_MOBILE_TXN','update_pending_product_lots');
      x_return_status := FND_API.G_RET_STS_UNEXP_ERROR;

  END Update_Product_Pending_Lot;

 /*+========================================================================+
   | PROCEDURE NAME
   |  Populate_Dispensing_Table
   |
   | USAGE
   |
   | ARGUMENTS
   |
   | RETURNS
   |
   | HISTORY
   |   Created  26-Apr-05 Eddie Oumerretane
   |   22-Aug-06 Shrikant Nene Bug 5456068
   |     Added parameter x_dispensed_count
   |
   +========================================================================+*/
  PROCEDURE Populate_Dispensing_Table(
    p_material_detail_id   IN NUMBER,
    x_dispensed_count      OUT NOCOPY NUMBER,
    x_return_status        OUT NOCOPY VARCHAR2,
    x_error_msg            OUT NOCOPY VARCHAR2)
  IS

  l_return_status VARCHAR2(2);
  l_msg_count NUMBER;
  l_msg_data  VARCHAR2(2000);
  l_index    INTEGER;
  l_nb_rec   NUMBER;
  l_dispensed NUMBER := 0;
  l_rsrv_tab GME_COMMON_PVT.reservations_tab;

  BEGIN

   IF (g_debug IS NOT NULL) THEN
      gme_debug.log_initialize ('MobilePopulateDispTbl');
   END IF;

   x_return_status := FND_API.G_RET_STS_SUCCESS;
   x_error_msg     := ' ';

  -- clear out temp table
  DELETE FROM GME_MATERIAL_DISPENSING_GTMP;

  -- Fetch dispensing data for the given material line

  GMO_DISPENSE_GRP.GET_MATERIAL_DISPENSE_DATA (
                                 p_api_version        => 1.0,
                                 p_init_msg_list      => FND_API.G_FALSE,
                                 x_return_status      => l_return_status,
                                 x_msg_count          => l_msg_count,
                                 x_msg_data           => l_msg_data,
                                 p_material_detail_id => p_material_detail_id,
                                 x_dispense_data      => l_rsrv_tab);

  l_nb_rec := l_rsrv_tab.COUNT;

  --insert into table
  FOR l_index in 1..l_nb_rec
  LOOP

     IF l_rsrv_tab(l_index).external_source_line_id IS NOT NULL THEN

       gme_debug.put_line('Inserting Dispensed record ');
       gme_debug.put_line('ID  =  ' ||
                         l_rsrv_tab(l_index).external_source_line_id);
       gme_debug.put_line('Sub  =  ' ||
                         l_rsrv_tab(l_index).subinventory_code);
       gme_debug.put_line('Locator Id =  ' ||
                         l_rsrv_tab(l_index).locator_id);
       gme_debug.put_line('UOM =  ' ||
                         l_rsrv_tab(l_index).reservation_uom_code);
       gme_debug.put_line('QTY =  ' ||
                         l_rsrv_tab(l_index).reservation_quantity);
       gme_debug.put_line('Sec QTY =  ' ||
                         l_rsrv_tab(l_index).secondary_reservation_quantity);
       gme_debug.put_line('Lot =  ' ||
                         l_rsrv_tab(l_index).lot_number);
       gme_debug.put_line('Revision =  ' ||
                         l_rsrv_tab(l_index).revision);

       INSERT INTO GME_MATERIAL_DISPENSING_GTMP
       (
        DISPENSE_ID
       ,SUBINVENTORY_CODE
       ,LOCATOR_ID
       ,DISPENSE_UOM
       ,DISPENSED_QTY
       ,SECONDARY_DISPENSED_QTY
       ,LOT_NUMBER
       ,REVISION
       )
       VALUES
       (
	  l_rsrv_tab(l_index).external_source_line_id
	 ,l_rsrv_tab(l_index).subinventory_code
	 ,l_rsrv_tab(l_index).locator_id
	 ,l_rsrv_tab(l_index).reservation_uom_code
	 ,l_rsrv_tab(l_index).reservation_quantity
	 ,l_rsrv_tab(l_index).secondary_reservation_quantity
	 ,l_rsrv_tab(l_index).lot_number
	 ,l_rsrv_tab(l_index).revision
       );

       l_dispensed := l_dispensed + 1;
    END IF;

  END LOOP;
  x_dispensed_count := l_dispensed;

  EXCEPTION
    WHEN OTHERS THEN
      IF g_debug <= gme_debug.g_log_unexpected THEN
        gme_debug.put_line('When others exception in Populate_Dispensing_Table');
        gme_debug.put_line(SubStr('Error '||TO_CHAR(SQLCODE)||': '||SQLERRM, 1,
255));

      END IF;
      fnd_msg_pub.add_exc_msg('GME_MOBILE_TXN','Populate_Dispensing_Table');
      x_return_status := FND_API.G_RET_STS_UNEXP_ERROR;
      x_error_msg     := fnd_message.get;

  END Populate_Dispensing_Table;

 /*+========================================================================+
   | PROCEDURE NAME
   |  Delete_Dispensing_Record
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
  PROCEDURE Delete_Dispensing_Record(
    p_dispense_id          IN NUMBER,
    x_return_status        OUT NOCOPY VARCHAR2,
    x_error_msg            OUT NOCOPY VARCHAR2)
  IS

  BEGIN

   IF (g_debug IS NOT NULL) THEN
      gme_debug.log_initialize ('MobileDelDispRec');
   END IF;

   x_return_status := FND_API.G_RET_STS_SUCCESS;
   x_error_msg     := ' ';

   DELETE FROM GME_MATERIAL_DISPENSING_GTMP
   WHERE dispense_id = p_dispense_id;

  EXCEPTION
    WHEN OTHERS THEN
      IF g_debug <= gme_debug.g_log_unexpected THEN
        gme_debug.put_line('When others exception in Delete_Dispensing_Record');
      END IF;
      fnd_msg_pub.add_exc_msg('GME_MOBILE_TXN','Delete_Dispensing_Record');
      x_return_status := FND_API.G_RET_STS_UNEXP_ERROR;
      x_error_msg     := fnd_message.get;

  END Delete_Dispensing_Record;

 /*+========================================================================+
   | PROCEDURE NAME
   |  Get_Dispensed_Lot_Count
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
  PROCEDURE Get_Dispensed_Lot_Count(
    p_subinv_code          IN VARCHAR2,
    p_locator_id           IN NUMBER,
    x_lot_count            OUT NOCOPY NUMBER,
    x_return_status        OUT NOCOPY VARCHAR2,
    x_error_msg            OUT NOCOPY VARCHAR2)
  IS

    CURSOR lot_count IS
      SELECT count(*)
      FROM GME_MATERIAL_DISPENSING_GTMP
      WHERE subinventory_code = p_subinv_code AND
            NVL(locator_id, -1) = NVL(p_locator_id, -1);
  BEGIN

   IF (g_debug IS NOT NULL) THEN
      gme_debug.log_initialize ('MobileCountDispLot');
   END IF;

   x_return_status := FND_API.G_RET_STS_SUCCESS;
   x_error_msg     := ' ';

   OPEN lot_count;
   FETCH lot_count INTO x_lot_count;
   CLOSE lot_count;

  EXCEPTION
    WHEN OTHERS THEN
      IF g_debug <= gme_debug.g_log_unexpected THEN
        gme_debug.put_line('When others exception in Get_Dispensed_Lot_Count');
      END IF;
      fnd_msg_pub.add_exc_msg('GME_MOBILE_TXN','Get_Dispensed_Lot_Count');
      x_return_status := FND_API.G_RET_STS_UNEXP_ERROR;
      x_error_msg     := fnd_message.get;

  END Get_Dispensed_Lot_Count;

 /*+========================================================================+
   | PROCEDURE NAME
   |  Query_Qty_Tree
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
                            x_return_status       OUT NOCOPY VARCHAR2,
                            x_error_msg           OUT NOCOPY VARCHAR2)
  IS

   l_is_revision_control BOOLEAN := FALSE;
   l_is_lot_control BOOLEAN := FALSE;

   l_msg_count     NUMBER(10);
   l_msg_data      VARCHAR2(1000);

   l_locator_id number;
   l_cost_group_id number;

BEGIN

   x_return_status := FND_API.G_RET_STS_SUCCESS;
   x_error_msg     := ' ';

   IF (g_debug IS NOT NULL) THEN
      gme_debug.log_initialize ('MobileQueryQtyTree');
   END IF;

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
                x_return_status      => x_return_status,
                x_msg_count          => l_msg_count,
                x_msg_data           => x_error_msg,
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
                x_qoh                => x_qoh,
                x_rqoh               => x_rqoh,
                x_qr                 => x_qr,
                x_qs                 => x_qs,
                x_att                => x_att,
                x_atr                => x_atr,
                x_sqoh               => x_sqoh,
                x_srqoh              => x_srqoh,
                x_sqr                => x_sqr,
                x_sqs                => x_sqs,
                x_satt               => x_satt,
                x_satr               => x_satr);


  EXCEPTION
    WHEN OTHERS THEN
      IF g_debug <= gme_debug.g_log_unexpected THEN
        gme_debug.put_line('When others exception in Query_Qty_Tree');
      END IF;
      fnd_msg_pub.add_exc_msg('GME_MOBILE_TXN','Query_Qty_Tree');

  END Query_Qty_Tree;

 /*+========================================================================+
   | PROCEDURE NAME
   |  Create_Qty_Tree
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
  PROCEDURE Create_Qty_Tree (p_tree_mode           IN NUMBER,
                             p_organization_id     IN NUMBER,
                             p_inventory_item_id   IN NUMBER,
                             p_revision            IN VARCHAR2,
                             p_subinventory_code   IN VARCHAR2,
                             p_locator_id          IN NUMBER,
                             p_revision_control    IN VARCHAR2,
                             p_lot_control         IN VARCHAR2,
                             x_tree_id             OUT NOCOPY NUMBER,
                             x_return_status       OUT NOCOPY VARCHAR2,
                             x_error_msg           OUT NOCOPY VARCHAR2) IS

   l_is_revision_control BOOLEAN := FALSE;
   l_is_lot_control BOOLEAN := FALSE;

   l_msg_count     NUMBER(10);
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
      , p_tree_mode               => p_tree_mode
      , p_is_revision_control     => l_is_revision_control
      , p_is_lot_control          => l_is_lot_control
      , p_is_serial_control       => FALSE
      , p_grade_code              => NULL
      , p_demand_source_type_id   => gme_common_pvt.g_txn_source_type
      , p_demand_source_name      => NULL
      , p_lot_expiration_date     => SYSDATE
      , p_onhand_source           =>   3
      , x_tree_id                 => x_tree_id
      );

     IF x_return_status <> FND_API.G_RET_STS_SUCCESS THEN
       RAISE CREATE_TREE_ERROR;
     END IF;

  EXCEPTION
    WHEN CREATE_TREE_ERROR THEN
      IF g_debug <= gme_debug.g_log_unexpected THEN
        gme_debug.put_line('Create Qty Tree exception');
      END IF;

    WHEN OTHERS THEN
      IF g_debug <= gme_debug.g_log_unexpected THEN
        gme_debug.put_line('When others exception in Create  Qty Tree');
      END IF;
      fnd_msg_pub.add_exc_msg('GME_MOBILE_TXN','Create_Qty_Tree');
      x_return_status := FND_API.G_RET_STS_UNEXP_ERROR;
      x_error_msg     := fnd_message.get;

  END Create_Qty_Tree;

 /*+========================================================================+
   | PROCEDURE NAME
   |  Query_Qty_Tree
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
  PROCEDURE Query_Qty_Tree (p_revision            IN VARCHAR2,
                            p_subinventory_code   IN VARCHAR2,
                            p_locator_id          IN NUMBER,
                            p_lot_number          IN VARCHAR2,
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
                            x_error_msg           OUT NOCOPY VARCHAR2) IS

   l_msg_count     NUMBER(10);
   QUERY_TREE_ERROR EXCEPTION;

  BEGIN

    IF (g_debug IS NOT NULL) THEN
      gme_debug.log_initialize ('MobileQtyTree');
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
    , x_qoh                        => x_qoh
    , x_rqoh                       => x_rqoh
    , x_qr                         => x_qr
    , x_qs                         => x_qs
    , x_att                        => x_att
    , x_atr                        => x_atr
    , x_sqoh                       => x_sqoh
    , x_srqoh                      => x_srqoh
    , x_sqr                        => x_sqr
    , x_sqs                        => x_sqs
    , x_satt                       => x_satt
    , x_satr                       => x_satr
    );

    IF x_return_status <> FND_API.G_RET_STS_SUCCESS THEN
       RAISE QUERY_TREE_ERROR;
    END IF;

    EXCEPTION
    WHEN QUERY_TREE_ERROR THEN
      IF g_debug <= gme_debug.g_log_unexpected THEN
        gme_debug.put_line('Query Qty Tree exception');
      END IF;

    WHEN OTHERS THEN
      IF g_debug <= gme_debug.g_log_unexpected THEN
        gme_debug.put_line('When others exception in Query_Tree');
      END IF;
      fnd_msg_pub.add_exc_msg('GME_MOBILE_TXN','Query_Tree');
      x_return_status := FND_API.G_RET_STS_UNEXP_ERROR;
      x_error_msg     := fnd_message.get;

  END Query_Qty_Tree;

 /*+========================================================================+
   | PROCEDURE NAME
   |  Update_Qty_Tree
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
  PROCEDURE Update_Qty_Tree ( p_tree_id             IN NUMBER,
                              p_revision            IN VARCHAR2,
                              p_subinventory_code   IN VARCHAR2,
                              p_locator_id          IN NUMBER,
                              p_lot_number          IN VARCHAR2,
                              p_primary_qty         IN NUMBER,
                              p_secondary_qty       IN NUMBER,
                              p_quantity_type       IN NUMBER,
                              x_return_status       OUT NOCOPY VARCHAR2,
                              x_error_msg           OUT NOCOPY VARCHAR2) IS

   l_msg_count     NUMBER(10);
   l_qoh           NUMBER;
   l_satr          NUMBER;
   l_atr           NUMBER;
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
   UPDATE_TREE_ERROR EXCEPTION;

  BEGIN


   IF (g_debug IS NOT NULL) THEN
     gme_debug.log_initialize ('MobileUpdateQtyTree');
   END IF;

   x_return_status := FND_API.G_RET_STS_SUCCESS;
   x_error_msg     := ' ';

   IF p_locator_id <= 0 THEN
      l_locator_id := null;
   ELSE
      l_locator_id := p_locator_id;
   END IF;

   IF (p_primary_qty <> 0) THEN

       gme_debug.put_line('Tree id    =  '||p_tree_id);
       gme_debug.put_line('Qty        =  '||p_primary_qty);
       gme_debug.put_line('Sec Qty    =  '||p_secondary_qty);
       gme_debug.put_line('lot        =  '||p_lot_number);
       gme_debug.put_line('Sub        =  '||p_subinventory_code);
       gme_debug.put_line('Locator id =  '||p_locator_id);
       gme_debug.put_line('revision   =  '||p_revision);

       INV_Quantity_Tree_Grp.Update_Quantities(
          p_api_version_number         => 1.0,
          p_init_msg_lst               => 'T',
          x_return_status              => x_return_status,
          x_msg_count                  => l_msg_count,
          x_msg_data                   => x_error_msg,
          p_tree_id                    => p_tree_id,
          p_revision                   => p_revision,
          p_lot_number                 => p_lot_number,
          p_subinventory_code          => p_subinventory_code,
          p_locator_id                 => p_locator_id,
          p_primary_quantity           => p_primary_qty,
          p_quantity_type              => p_quantity_type,
          p_secondary_quantity         => p_secondary_qty,
          x_qoh                        => l_qoh,
          x_rqoh                       => l_rqoh,
          x_qr                         => l_qr,
          x_qs                         => l_qs,
          x_att                        => l_att,
          x_atr                        => l_atr,
          x_sqoh                       => l_sqoh,
          x_srqoh                      => l_srqoh,
          x_sqr                        => l_sqr,
          x_sqs                        => l_sqs,
          x_satt                       => l_satt,
          x_satr                       => l_satr,
          p_containerized              => 0,
          p_lpn_id                     => NULL);

     gme_debug.put_line('New ATT  =  '||l_att);
     IF x_return_status <> FND_API.G_RET_STS_SUCCESS THEN
       RAISE UPDATE_TREE_ERROR;
     END IF;

   END IF;


  EXCEPTION
    WHEN UPDATE_TREE_ERROR THEN
      IF g_debug <= gme_debug.g_log_unexpected THEN
        gme_debug.put_line('Update Qty Tree exception');
      END IF;

    WHEN OTHERS THEN
      IF g_debug <= gme_debug.g_log_unexpected THEN
        gme_debug.put_line('When others exception in Update Qty Tree');
      END IF;
      fnd_msg_pub.add_exc_msg('GME_MOBILE_TXN','Update_Qty_Tree');
      x_return_status := FND_API.G_RET_STS_UNEXP_ERROR;
      x_error_msg     := fnd_message.get;

  END Update_Qty_Tree;

 /*+========================================================================+
   | PROCEDURE NAME
   |  Is_Lot_Indivisible
   |
   | USAGE
   |
   | ARGUMENTS
   |
   | RETURNS
   |
   | HISTORY
   |   Created  21-Sep-05 Eddie Oumerretane
   |
   +========================================================================+*/
  FUNCTION Is_Lot_Indivisible_item (p_organization_id IN NUMBER,
                                    p_item_id         IN NUMBER) RETURN BOOLEAN
  IS
    CURSOR Get_div_flag IS
     SELECT
       NVL(lot_divisible_flag, 'N'),
       NVL(lot_control_code, 1)
     FROM  mtl_system_items_kfv
     WHERE  inventory_item_id = p_item_id
      AND   organization_id   = p_organization_id;

    l_lot_div_flag     VARCHAR2(1);
    l_lot_control_code NUMBER;

  BEGIN

    OPEN Get_div_flag;
    FETCH Get_div_flag INTO l_lot_div_flag, l_lot_control_code;
    CLOSE Get_div_flag;

    IF l_lot_control_code <> 1 THEN
      IF l_lot_div_flag = 'N' THEN
        RETURN TRUE;
      ELSE
        RETURN FALSE;
      END IF;
    ELSE
      RETURN FALSE;
    END IF;

  END Is_Lot_Indivisible_Item;

 /*+========================================================================+
   | PROCEDURE NAME
   |  Validate_Item_To_Issue
   |
   | USAGE
   |
   | ARGUMENTS
   |
   | RETURNS
   |
   | HISTORY
   |   Created  19-Sep-05 Eddie Oumerretane
   |
   +========================================================================+*/
  PROCEDURE Validate_Item_To_Issue(p_organization_id    IN NUMBER,
                                    p_batch_id           IN NUMBER,
                                    p_material_detail_id IN NUMBER,
                                    p_item_id            IN NUMBER,
                                    x_return_status   OUT NOCOPY VARCHAR2,
                                    x_error_msg       OUT NOCOPY VARCHAR2) IS
    CURSOR Get_Mtl_Dtl IS
      SELECT release_type,
             NVL(phantom_type,0),
             phantom_line_id
        FROM gme_material_details
       WHERE material_detail_id = p_material_detail_id;

    CURSOR Get_pplot_count IS
    SELECT count(*)
      FROM gme_pending_product_lots
     WHERE batch_id = p_batch_id;

    ITEM_NOT_VALID     EXCEPTION;
    l_release_type     NUMBER;
    l_phantom_type     NUMBER;
    l_phantom_line_id  NUMBER;
    l_batchstep_id     NUMBER;
    l_batchstep_status NUMBER;
    l_pplot_count      NUMBER;
    l_is_item_associated_to_step BOOLEAN;

  BEGIN

   IF (g_debug IS NOT NULL) THEN
      gme_debug.log_initialize ('MobileValItemForIssue');
      gme_debug.put_line('Org Id   =  ' || p_organization_id);
      gme_debug.put_line('Batch Id =  ' || p_batch_id);
      gme_debug.put_line('Line Id  =  ' || p_material_detail_id);
      gme_debug.put_line('Item Id  =  ' || p_item_id);
   END IF;

   x_return_status := FND_API.G_RET_STS_SUCCESS;
   x_error_msg     := ' ';

   OPEN Get_Mtl_dtl;
   FETCH Get_Mtl_Dtl INTO l_release_type, l_phantom_type, l_phantom_line_id;
   CLOSE Get_Mtl_Dtl;

   -- If the ingredient is a phantom and it has not been exploded then we
   -- cannot consume it.

   IF l_phantom_type <> 0 THEN
     IF l_phantom_line_id IS NULL THEN
       FND_MESSAGE.SET_NAME('GME', 'GME_ISSUE_PHANTOM_NOT_EXPLOD');
       RAISE ITEM_NOT_VALID;
     END IF;
   END IF;

   -- If the ingredient line has a release type of auto by step then the
   -- associated step must be in WIP or Completed
   IF l_release_type = GME_COMMON_PVT.g_mtl_autobystep_release THEN
     l_is_item_associated_to_step := GME_COMMON_PVT.Get_Assoc_Step(
                     p_material_detail_id => p_material_detail_id
                    ,x_batchstep_id       => l_batchstep_id
                    ,x_batchstep_status   => l_batchstep_status);
     IF l_is_item_associated_to_step AND
        l_batchstep_status <> 2      AND
        l_batchstep_status <> 3 THEN
       FND_MESSAGE.SET_NAME('GME', 'GME_ISSUE_STEP_WIP_COMPLETE');
       RAISE ITEM_NOT_VALID;
     END IF;
   END IF;

   -- If item is lot indivisible then you cannot consume it if there is a
   -- pending product lot for the batch
   /*IF Is_Lot_Indivisible_item(p_organization_id, p_item_id) THEN
     OPEN Get_pplot_count;
     FETCH Get_pplot_count INTO l_pplot_count;
     CLOSE Get_pplot_count;

     IF l_pplot_count > 0 THEN
       FND_MESSAGE.SET_NAME('GME', 'GME_ISSUE_INDIV_PPLOT_EXIST');
       RAISE ITEM_NOT_VALID;
     END IF;
   END IF; */

  EXCEPTION
    WHEN ITEM_NOT_VALID THEN
      x_return_status := FND_API.G_RET_STS_ERROR;
      x_error_msg     := fnd_message.get;

    WHEN OTHERS THEN
      IF g_debug <= gme_debug.g_log_unexpected THEN
        gme_debug.put_line('When others exception in Validate Item To Issue');
      END IF;
      fnd_msg_pub.add_exc_msg('GME_MOBILE_TXN','Validate_Item_To_Issue');
      x_return_status := FND_API.G_RET_STS_UNEXP_ERROR;
      x_error_msg     := fnd_message.get;

  END Validate_Item_To_Issue;

 /*+========================================================================+
   | PROCEDURE NAME
   |  Validate_Item_To_Return
   |
   | USAGE
   |
   | ARGUMENTS
   |
   | RETURNS
   |
   | HISTORY
   |   Created  04-Oct-05 Eddie Oumerretane
   |
   +========================================================================+*/
  PROCEDURE Validate_Item_To_Return(p_organization_id    IN NUMBER,
                                    p_batch_id           IN NUMBER,
                                    p_material_detail_id IN NUMBER,
                                    p_item_id            IN NUMBER,
                                    x_return_status   OUT NOCOPY VARCHAR2,
                                    x_error_msg       OUT NOCOPY VARCHAR2) IS
    CURSOR Get_Mtl_Dtl IS
      SELECT NVL(dispense_ind,'N')
        FROM gme_material_details
       WHERE material_detail_id = p_material_detail_id;

    ITEM_NOT_VALID     EXCEPTION;
    l_dispense_ind     VARCHAR2(2);

  BEGIN

   IF (g_debug IS NOT NULL) THEN
      gme_debug.log_initialize ('MobileValItemToReturn');
      gme_debug.put_line('Org Id   =  ' || p_organization_id);
      gme_debug.put_line('Batch Id =  ' || p_batch_id);
      gme_debug.put_line('Line Id  =  ' || p_material_detail_id);
      gme_debug.put_line('Item Id  =  ' || p_item_id);
   END IF;

   x_return_status := FND_API.G_RET_STS_SUCCESS;
   x_error_msg     := ' ';

   OPEN Get_Mtl_dtl;
   FETCH Get_Mtl_Dtl INTO l_dispense_ind;
   CLOSE Get_Mtl_Dtl;

   -- Cannot return an ingredient that was dispensed

   IF l_dispense_ind = 'Y' THEN
     FND_MESSAGE.SET_NAME('GME', 'GME_RETURN_DISPENSE_ING_ERROR');
     RAISE ITEM_NOT_VALID;
   END IF;

  EXCEPTION
    WHEN ITEM_NOT_VALID THEN
      x_return_status := FND_API.G_RET_STS_ERROR;
      x_error_msg     := fnd_message.get;

    WHEN OTHERS THEN
      IF g_debug <= gme_debug.g_log_unexpected THEN
        gme_debug.put_line('When others exception in Validate Item To Return');
      END IF;
      fnd_msg_pub.add_exc_msg('GME_MOBILE_TXN','Validate_Item_To_Return');
      x_return_status := FND_API.G_RET_STS_UNEXP_ERROR;
      x_error_msg     := fnd_message.get;

  END Validate_Item_To_Return;

 /*+========================================================================+
   | PROCEDURE NAME
   |  Validate_Prod_To_Yield
   |
   | USAGE
   |
   | ARGUMENTS
   |
   | RETURNS
   |
   | HISTORY
   |   Created  19-Sep-05 Eddie Oumerretane
   |
   |   Bug 22753342 01-Mar-216 Archana Mundhe
   |   Allow batch yield for a WIP or completed batch only. 
   +========================================================================+*/
  PROCEDURE Validate_Prod_To_Yield (p_organization_id    IN NUMBER,
                                    p_batch_id           IN NUMBER,
                                    p_material_detail_id IN NUMBER,
                                    p_item_id            IN NUMBER,
                                    x_return_status   OUT NOCOPY VARCHAR2,
                                    x_error_msg       OUT NOCOPY VARCHAR2) IS

    CURSOR Get_Release_Type IS
      SELECT release_type
        FROM gme_material_details
       WHERE material_detail_id = p_material_detail_id;

    CURSOR get_Batch_Status IS
     SELECT batch_status
     FROM gme_batch_header
     WHERE batch_id = p_batch_id;

    l_release_type     NUMBER;
    l_batch_status     NUMBER;
    l_batchstep_id     NUMBER;
    l_batchstep_status NUMBER;
    l_is_item_associated_to_step BOOLEAN;
    ITEM_NOT_VALID  EXCEPTION;

  BEGIN

   IF (g_debug IS NOT NULL) THEN
      gme_debug.log_initialize ('MobileValProdToYield');
      gme_debug.put_line('Org Id   =  ' || p_organization_id);
      gme_debug.put_line('Batch Id =  ' || p_batch_id);
      gme_debug.put_line('Line Id  =  ' || p_material_detail_id);
      gme_debug.put_line('Item Id  =  ' || p_item_id);
   END IF;

   x_return_status := FND_API.G_RET_STS_SUCCESS;
   x_error_msg     := ' ';

   OPEN Get_Batch_Status;
   FETCH Get_Batch_Status INTO l_batch_status;
   CLOSE Get_Batch_Status;

   OPEN Get_Release_Type;
   FETCH Get_Release_Type INTO l_release_type;
   CLOSE Get_Release_Type;

   -- If product release type is Automatic, then batch status must be
   -- Completed
   -- If product release type is Automatic By Step, then:
   --    a) if associated to a step, step status must be Completed
   --    b) if not associated to a step, batch status must be Completed

   IF l_release_type = GME_COMMON_PVT.g_mtl_auto_release THEN
     -- Bug 22753342 Allow batch yield for a WIP or completed batch only.
     IF l_batch_status NOT IN (2,3) THEN
       FND_MESSAGE.SET_NAME('GME', 'GME_YIELD_BATCH_COMPLETE');
       RAISE ITEM_NOT_VALID;
     END IF;
   ELSIF l_release_type = GME_COMMON_PVT.g_mtl_autobystep_release THEN
     l_is_item_associated_to_step := GME_COMMON_PVT.Get_Assoc_Step(
                     p_material_detail_id => p_material_detail_id
                    ,x_batchstep_id       => l_batchstep_id
                    ,x_batchstep_status   => l_batchstep_status);
     IF l_is_item_associated_to_step THEN
       -- Bug 22753342 Allow batch yield for a WIP or completed batch step.
       IF l_batchstep_status NOT IN (2,3) THEN
         FND_MESSAGE.SET_NAME('GME', 'GME_YIELD_STEP_COMPLETE');
         RAISE ITEM_NOT_VALID;
       END IF;
      -- Bug 22753342 Allow batch yield for a WIP or completed batch only.  
     ELSIF l_batch_status NOT IN (2,3) THEN
       FND_MESSAGE.SET_NAME('GME', 'GME_YIELD_BATCH_COMPLETE');
       RAISE ITEM_NOT_VALID;
     END IF;
   END IF;



  EXCEPTION
    WHEN ITEM_NOT_VALID THEN
      x_return_status := FND_API.G_RET_STS_ERROR;
      x_error_msg     := fnd_message.get;

    WHEN OTHERS THEN
      IF g_debug <= gme_debug.g_log_unexpected THEN
        gme_debug.put_line('When others exception in Validate Prod To Yield');
      END IF;
      fnd_msg_pub.add_exc_msg('GME_MOBILE_TXN','Validate_Prod_To_Yield');
      x_return_status := FND_API.G_RET_STS_UNEXP_ERROR;
      x_error_msg     := fnd_message.get;

  END Validate_Prod_To_Yield;

 /*+========================================================================+
   | PROCEDURE NAME
   |  Fetch_Indiv_Lot_Txn
   |
   | USAGE
   |
   | ARGUMENTS
   |
   | RETURNS
   |
   | HISTORY
   |   Created  20-Sep-05 Eddie Oumerretane
   |
   +========================================================================+*/
  PROCEDURE Fetch_Issue_Transactions(
                                      p_organization_id     IN         NUMBER,
                                      p_batch_id            IN         NUMBER,
                                      p_material_detail_id  IN         NUMBER,
                                      p_lot_number          IN         VARCHAR2,
                                      x_return_status       OUT NOCOPY VARCHAR2,
                                      x_error_msg           OUT NOCOPY VARCHAR2,
                                      x_txn_cursor          OUT NOCOPY t_genref)
  IS
    l_date_format VARCHAR2(100);
  BEGIN

    IF (g_debug IS NOT NULL) THEN
       gme_debug.log_initialize ('MobileFetchIssueTxn');
    END IF;

    x_return_status := FND_API.G_RET_STS_SUCCESS;
    x_error_msg     := ' ';

    FND_PROFILE.GET('MWA_DATE_FORMAT_MASK',l_date_format);

    IF l_date_format IS NULL THEN
      FND_PROFILE.GET('ICX_DATE_FORMAT_MASK',l_date_format);
    END IF;

    OPEN x_txn_cursor FOR
       SELECT DISTINCT
             m.transaction_id,
             m.transaction_quantity*(-1),
             m.primary_quantity *(-1),
             m.secondary_transaction_quantity *(-1),
             m.transaction_uom,
             m.subinventory_code,
             m.locator_id,
             m.reason_id,
             lc.concatenated_segments,
             TO_CHAR(m.transaction_date, l_date_format),
             revision
           FROM mtl_material_transactions m,
                mtl_transaction_lot_numbers l,
                wms_item_locations_kfv lc
            WHERE l.transaction_id = m.transaction_id
              AND l.lot_number = NVL(p_lot_number, l.lot_number)
              AND m.organization_id = p_organization_id
              AND m.transaction_source_id = p_batch_id
              AND m.trx_source_line_id = p_material_detail_id
              AND m.transaction_type_id =  GME_COMMON_PVT.g_ing_issue
              AND m.transaction_source_type_id = gme_common_pvt.g_txn_source_type
            AND lc.inventory_location_id(+) = m.locator_id;

  EXCEPTION
    WHEN OTHERS THEN
      IF g_debug <= gme_debug.g_log_unexpected THEN
        gme_debug.put_line('When others exception in fetch Issue Transactions');
      END IF;
      fnd_msg_pub.add_exc_msg('GME_MOBILE_TXN','fetch_issue_transactions');
      x_return_status := FND_API.G_RET_STS_UNEXP_ERROR;
      x_error_msg     := fnd_message.get;

  END Fetch_Issue_Transactions;

 /*+========================================================================+
   | PROCEDURE NAME
   |  Fetch_Yield_Transactions
   |
   | USAGE
   |
   | ARGUMENTS
   |
   | RETURNS
   |
   | HISTORY
   |   Created  20-Sep-05 Eddie Oumerretane
   |
   +========================================================================+*/
  PROCEDURE Fetch_Yield_Transactions(
                                      p_organization_id     IN         NUMBER,
                                      p_batch_id            IN         NUMBER,
                                      p_material_detail_id  IN         NUMBER,
                                      p_lot_number          IN         VARCHAR2,
                                      p_txn_type_id         IN         NUMBER,
                                      x_return_status       OUT NOCOPY VARCHAR2,
                                      x_error_msg           OUT NOCOPY VARCHAR2,
                                      x_txn_cursor          OUT NOCOPY t_genref)
  IS
    l_date_format VARCHAR2(100);
    l_txn_type_id NUMBER;
  BEGIN

    IF (g_debug IS NOT NULL) THEN
       gme_debug.log_initialize ('MobileFetchYieldTxn');
    END IF;

    x_return_status := FND_API.G_RET_STS_SUCCESS;
    x_error_msg     := ' ';

    FND_PROFILE.GET('MWA_DATE_FORMAT_MASK',l_date_format);

    IF l_date_format IS NULL THEN
      FND_PROFILE.GET('ICX_DATE_FORMAT_MASK',l_date_format);
    END IF;

    l_txn_type_id := Get_Txn_Type(p_txn_type_id);

    OPEN x_txn_cursor FOR
       SELECT DISTINCT
             m.transaction_id,
             m.transaction_quantity*(-1),
             m.primary_quantity *(-1),
             m.secondary_transaction_quantity *(-1),
             m.transaction_uom,
             m.subinventory_code,
             m.locator_id,
             m.reason_id,
             lc.concatenated_segments,
             TO_CHAR(m.transaction_date, l_date_format),
             revision
           FROM mtl_material_transactions m,
                mtl_transaction_lot_numbers l,
                wms_item_locations_kfv lc
            WHERE l.transaction_id = m.transaction_id
              AND l.lot_number = NVL(p_lot_number, l.lot_number)
              AND m.organization_id = p_organization_id
              AND m.transaction_source_id = p_batch_id
              AND m.trx_source_line_id = p_material_detail_id
              AND m.transaction_type_id =  l_txn_type_id
              AND m.transaction_source_type_id = gme_common_pvt.g_txn_source_type
            AND lc.inventory_location_id(+) = m.locator_id;

  EXCEPTION
    WHEN OTHERS THEN
      IF g_debug <= gme_debug.g_log_unexpected THEN
        gme_debug.put_line('When others exception in fetch yield Transactions');
      END IF;
      fnd_msg_pub.add_exc_msg('GME_MOBILE_TXN','fetch_yield_transactions');
      x_return_status := FND_API.G_RET_STS_UNEXP_ERROR;
      x_error_msg     := fnd_message.get;

  END Fetch_Yield_Transactions;



   /*+==========================================================================+
   | PROCEDURE NAME
   |   CREATE_PHANTOM_TXN
   |
   | USAGE
   |    create phantom transaction for the transaction passed.
   |
   | ARGUMENTS
   |   p_mmti_trans_id - transaction_id from mmti
   |
   | RETURNS
   |   returns via x_status OUT parameters
   |
   | HISTORY
   |   Created  20-Sep-05
   |
   +==========================================================================+ */
   PROCEDURE Create_Phantom_Txn (
      p_mmti_trans_id   IN              NUMBER
     ,x_return_status   OUT NOCOPY      VARCHAR2
     ,x_error_msg       OUT NOCOPY      VARCHAR2) IS

      CURSOR cur_get_transaction (v_transaction_id NUMBER)
      IS
         SELECT *
           FROM mtl_transactions_interface mmti
          WHERE transaction_interface_id = v_transaction_id;

      CURSOR cur_get_lot_transaction (v_transaction_id NUMBER)
      IS
         SELECT *
           FROM mtl_transaction_lots_interface
          WHERE transaction_interface_id = v_transaction_id;

      l_api_name    CONSTANT VARCHAR2 (30) := 'CREATE_PHANTOM_TXN';
      l_mmti_rec                        mtl_transactions_interface%ROWTYPE;
      l_mmli_tbl                        gme_common_pvt.mtl_trans_lots_inter_tbl;
   BEGIN

      IF (g_debug IS NOT NULL) THEN
        gme_debug.log_initialize ('MobileCreatePhantomTxn');
      END IF;

      --Initially let us assign the return status to success
      x_return_status := fnd_api.g_ret_sts_success;

      IF (g_debug <= gme_debug.g_log_statement) THEN
         gme_debug.put_line (g_pkg_name || '.' || l_api_name || ':'
                             || 'Entering');
      END IF;

      OPEN cur_get_transaction(p_mmti_trans_id);
      FETCH cur_get_transaction INTO l_mmti_rec;
      IF cur_get_transaction%NOTFOUND THEN
         CLOSE cur_get_transaction;
         gme_common_pvt.log_message('GME_NO_TRANS_FOUND');
         RAISE fnd_api.g_exc_error;
      END IF;
      CLOSE cur_get_transaction;

      OPEN cur_get_lot_transaction(p_mmti_trans_id);
      FETCH cur_get_lot_transaction
      BULK COLLECT INTO l_mmli_tbl;
      CLOSE cur_get_lot_transaction;

      IF  l_mmti_rec.transaction_type_id NOT IN
            (gme_common_pvt.g_ing_return
            ,gme_common_pvt.g_prod_completion
            ,gme_common_pvt.g_byprod_completion) THEN


        FOR i IN 1..l_mmli_tbl.COUNT
        LOOP
          l_mmli_tbl(i).transaction_quantity :=
                                       (-1) * l_mmli_tbl(i).transaction_quantity;
          l_mmli_tbl(i).secondary_transaction_quantity :=
                             (-1) * l_mmli_tbl(i).secondary_transaction_quantity;
        END LOOP;

        l_mmti_rec.transaction_quantity :=
                                       (-1) * l_mmti_rec.transaction_quantity;
        l_mmti_rec.secondary_transaction_quantity :=
                             (-1) * l_mmti_rec.secondary_transaction_quantity;

      END IF;

      GME_COMMON_PVT.G_MOVE_TO_TEMP := FND_API.G_FALSE;

      GME_TRANSACTIONS_PVT.Create_Material_Txn

                          (p_mmti_rec           => l_mmti_rec
                          ,p_mmli_tbl           => l_mmli_tbl
                          ,p_phantom_trans      => 2
                          ,x_return_status      => x_return_status);

      IF x_return_status <> fnd_api.g_ret_sts_success THEN
         RAISE fnd_api.g_exc_error;
      END IF;

      IF (g_debug <= gme_debug.g_log_statement) THEN
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ':'
                             || 'Exiting with '
                             || x_return_status);
      END IF;

    EXCEPTION
      WHEN fnd_api.g_exc_error THEN
         x_return_status := fnd_api.g_ret_sts_error;
         x_error_msg     := fnd_message.get;

      WHEN fnd_api.g_exc_unexpected_error THEN
         x_return_status := fnd_api.g_ret_sts_unexp_error;

      WHEN OTHERS THEN
         x_return_status := fnd_api.g_ret_sts_unexp_error;
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
         x_error_msg     := fnd_message.get;
         IF (NVL (g_debug, -1) = gme_debug.g_log_unexpected) THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ':'
                                || 'WHEN OTHERS:'
                                || SQLERRM);
         END IF;

   END Create_Phantom_Txn;

  /*###############################################################
  # DESCRIPTION
  #     This procedure calls the GME complete_step api.
  ###############################################################*/

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
                           x_message_list    OUT NOCOPY VARCHAR2) IS
    l_step_qty           NUMBER;
    l_act_strt_dt        DATE;
    l_act_complt_dt      DATE;
    message              VARCHAR2(2000);
    l_batch_step         gme_batch_steps%ROWTYPE;
    l_batch_step_out     gme_batch_steps%ROWTYPE;
    x_batch_step         gme_batch_steps%ROWTYPE;
    l_batch_header       gme_batch_header%ROWTYPE;
    x_exception_material gme_common_pvt.exceptions_tab;
    step_alloc_error     EXCEPTION;
    expected_error       EXCEPTION;
    validate_step_error  EXCEPTION;
    validate_step_status_error  EXCEPTION;
  BEGIN
    -- Bug 19911453
    gme_debug.log_initialize ('MobileCompleteStep');
    SAVEPOINT complete_step_mobile ;


    x_return_status := FND_API.G_RET_STS_SUCCESS;

    l_act_strt_dt   := TO_DATE(p_act_strt_dt, p_date_format||HOUR_MIN_SEC_FORMAT_STRING);
    l_act_complt_dt := TO_DATE(p_act_complt_dt, p_date_format||HOUR_MIN_SEC_FORMAT_STRING);

     -- Bug 19911453
    gme_debug.put_line('In complete_step..l_act_strt_dt  is:' || l_act_strt_dt);
    gme_debug.put_line('In complete_step..l_act_complt_dt  is:' || l_act_complt_dt);
    gme_debug.put_line('In complete_step..parameter act_strt_dt  is:' || p_act_strt_dt);
    gme_debug.put_line('In complete_step..parameter p_act_complt_dt  is:' || p_act_complt_dt);

    fnd_profile.put('USER_ID',to_char(p_uid));
    gme_common_pvt.g_user_name  := p_uname;
    gme_common_pvt.g_user_ident := p_uid;
    gme_common_pvt.g_login_id   := TO_NUMBER (fnd_profile.VALUE ('LOGIN_ID'));
    gme_common_pvt.g_user_ident := TO_NUMBER (fnd_profile.VALUE ('USER_ID'));

    l_batch_header.batch_id        := p_batch_id;
    IF NOT (gme_batch_header_dbl.fetch_row(l_batch_header, l_batch_header)) THEN
      RAISE expected_error;
    END IF;
    l_batch_step.batch_id          := p_batch_id;
    l_batch_step.batchstep_id      := p_step_id;
    IF NOT (gme_batch_steps_dbl.fetch_row(l_batch_step, l_batch_step)) THEN
      RAISE expected_error;
    END IF;

    l_batch_step.actual_step_qty   := p_act_step_qty;
    l_batch_step.actual_start_date := l_act_strt_dt;
    l_batch_step.actual_cmplt_date := l_act_complt_dt;

    -- Bug 4774944 Rework.
    -- Check for 'Step controls batch status' parameter.
    IF l_batch_header.batch_status = 1 AND
         (gme_common_pvt.g_step_controls_batch_sts_ind <> 1 OR
          l_batch_header.parentline_id IS NOT NULL ) THEN
       RAISE validate_step_status_error;
    END IF;

    -- Bug 4774944
    -- Added call to validate step for complete.
    gme_complete_batch_step_pvt.validate_step_for_complete
                       (p_batch_header_rec     => l_batch_header
                       ,p_batch_step_rec       => l_batch_step
                       ,p_override_quality     => FND_API.G_FALSE
                       ,x_batch_step_rec       => l_batch_step_out
                       ,x_return_status        => x_return_status);

    -- Bug 19911453
    gme_debug.put_line('In complete_step..INPUT l_batch_step.actual_start_date  is:' || l_batch_step.actual_start_date);
    gme_debug.put_line('In complete_step..INPUT l_batch_step.actual_cmplt_date  is:' || l_batch_step.actual_cmplt_date);

    gme_debug.put_line('In complete_step..l_batch_step_out.actual_start_date  is:' || l_batch_step_out.actual_start_date);
    gme_debug.put_line('In complete_step..l_batch_step_out.actual_cmplt_date  is:' || l_batch_step_out.actual_cmplt_date);


    IF x_return_status <> fnd_api.g_ret_sts_success THEN
         RAISE validate_step_error;
    END IF;

    -- Pass l_batch_step_out with updated dates.
    gme_api_main.complete_step (
       p_validation_level            => gme_common_pvt.g_max_errors,
       p_init_msg_list               => FND_API.G_TRUE,
       x_message_count               => x_message_count,
       x_message_list                => x_message_list,
       x_return_status               => x_return_status,
       p_batch_step_rec              => l_batch_step_out,  /* Bug 19911453 */
       p_batch_header_rec            => l_batch_header,
       x_batch_step_rec              => x_batch_step,
       x_exception_material_tbl      => x_exception_material,
       p_ignore_exception            => FND_API.G_FALSE);
    IF x_return_status = 'X' THEN
      RAISE step_alloc_error;
    END IF;
    IF x_return_status = 'S' THEN
      IF p_act_step_qty IS NOT NULL THEN
        SELECT actual_step_qty INTO l_step_qty
        FROM gme_batch_steps
        WHERE batch_id = p_batch_id
        AND batchstep_id = p_step_id;
        IF l_step_qty <> p_act_step_qty THEN /* Update Act Step Qty */
          UPDATE gme_batch_steps
          SET actual_step_qty = p_act_step_qty
          WHERE batch_id = p_batch_id
          AND batchstep_id = p_step_id;
        END IF;
      END IF;

             -- Bug 13986776 - Added Code to call save batch.
       gme_api_pub.save_batch(x_return_status  => x_return_status,
                             p_header_id       => gme_common_pvt.get_txn_header_id,
                             p_table           => 2,
                             p_commit          => 'T',
                             p_clear_qty_cache => 'T');

    END IF;
  EXCEPTION
     WHEN expected_error THEN
       x_return_status := FND_API.G_RET_STS_ERROR;
     WHEN step_alloc_error THEN
     	 ROLLBACK TO SAVEPOINT complete_step_mobile ;
       FND_MESSAGE.SET_NAME('GME', 'GME_API_UNALLOC_MATERIALS');
       x_message_list := FND_MESSAGE.GET;
     WHEN validate_step_error THEN
       gme_common_pvt.count_and_get (x_count        => x_message_count
                                    ,p_encoded      => fnd_api.g_false
                                    ,x_data         => x_message_list);
     WHEN validate_step_status_error THEN
      FND_MESSAGE.SET_NAME('GME','GME_API_INV_BATCH_CMPL_STEP');
      x_message_list := FND_MESSAGE.GET;
     WHEN OTHERS THEN
       x_return_status := FND_API.G_RET_STS_UNEXP_ERROR;
  END complete_step;

  /*###############################################################
  # DESCRIPTION
  #   Bug 4962372 Invoke the End resource transaction API
  ###############################################################*/
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
     ,x_error_msg       OUT NOCOPY VARCHAR2) IS

     l_message_count    NUMBER;
     l_in_rsrc_txn_rec  gme_resource_txns%ROWTYPE;
     l_rsrc_txn_rec     gme_resource_txns%ROWTYPE;
     l_api_name         VARCHAR2(30) := 'End_Cmplt_Actual_Rsrc_Txn';
  BEGIN
    x_return_status := FND_API.G_RET_STS_SUCCESS;
    x_error_msg     := '';
    FND_PROFILE.put('USER_ID',to_char(p_uid));
    gme_common_pvt.g_user_name  := p_uname;
    gme_common_pvt.g_user_ident := p_uid;
    gme_common_pvt.g_login_id   := TO_NUMBER (fnd_profile.VALUE ('LOGIN_ID'));
    gme_common_pvt.g_user_ident := TO_NUMBER (fnd_profile.VALUE ('USER_ID'));

    l_in_rsrc_txn_rec.poc_trans_id := p_trans_id;
    l_in_rsrc_txn_rec.organization_id := p_organization_id;
    l_in_rsrc_txn_rec.trans_date   := p_trans_date;
    l_in_rsrc_txn_rec.instance_id  := p_instance_id;
    l_in_rsrc_txn_rec.end_date     := p_end_date;
    l_in_rsrc_txn_rec.reason_id    := p_reason_id;

    GME_API_PUB.end_cmplt_actual_rsrc_txn (
       p_api_version        => 2.0
      ,p_init_msg_list      => FND_API.G_TRUE
      ,p_commit             => FND_API.G_TRUE
      ,p_instance_no        => NULL
      ,p_reason_name        => NULL
      ,p_rsrc_txn_rec       => l_in_rsrc_txn_rec
      ,x_rsrc_txn_rec       => l_rsrc_txn_rec
      ,x_message_count      => l_message_count
      ,x_message_list       => x_error_msg
      ,x_return_status      => x_return_status);
    x_trans_id := l_rsrc_txn_rec.poc_trans_id;
  EXCEPTION
    WHEN OTHERS THEN
      x_return_status := FND_API.G_RET_STS_UNEXP_ERROR;
      fnd_msg_pub.add_exc_msg(g_pkg_name, l_api_name);
  END End_Cmplt_Actual_Rsrc_Txn;

 /*###############################################################
  # DESCRIPTION
  #     This procedure determines whether batch is ASQC or not.
  ###############################################################*/
  PROCEDURE get_ASQC ( p_batch_id      IN  NUMBER,
                       x_ASQC_status OUT NOCOPY NUMBER) IS
  BEGIN
   SELECT NVL(automatic_step_calculation,0) INTO x_ASQC_status
   FROM gme_batch_header
   WHERE batch_id = p_batch_id;
  EXCEPTION
    WHEN OTHERS THEN NULL;
  END get_ASQC;

  /*###############################################################
  # DESCRIPTION
  #     This procedure returns the system date and time.
  ###############################################################*/
  PROCEDURE get_system_date ( p_date_format     IN VARCHAR2,
                              x_sys_date        OUT NOCOPY VARCHAR2) IS
  BEGIN
   SELECT TO_CHAR(sysdate, p_date_format||HOUR_MIN_SEC_FORMAT_STRING)
   INTO x_sys_date
   FROM sys.DUAL;
  END get_system_date;

  /*###############################################################
  # DESCRIPTION
  #     This procedure calls the GME release_step api.
  ###############################################################*/

  PROCEDURE release_step ( p_batch_id        IN NUMBER,
                           p_step_id         IN NUMBER,
                           p_act_strt_dt     IN VARCHAR2,
                           p_date_format     IN VARCHAR2,
                           p_uname           IN VARCHAR2,
                           p_uid             IN NUMBER,
                           x_return_status OUT NOCOPY VARCHAR2,
                           x_message_count OUT NOCOPY NUMBER,
                           x_message_list  OUT NOCOPY VARCHAR2) IS
    l_step_qty             NUMBER;
    l_count                NUMBER;
    l_act_strt_dt          DATE;
    message                VARCHAR2(2000);
    l_api_name             VARCHAR2(30) := 'release_step';
    l_batch_step           gme_batch_steps%ROWTYPE;
    l_batch_step_out       gme_batch_steps%ROWTYPE;
    x_batch_step           gme_batch_steps%ROWTYPE;
    l_batch_hdr_rec        gme_batch_header%ROWTYPE;
    x_exception_material   gme_common_pvt.exceptions_tab;
    step_alloc_error       EXCEPTION;
    expected_error       EXCEPTION;
    validate_step_error  EXCEPTION;
    validate_step_status_error  EXCEPTION;
  BEGIN

     -- Bug 19911453
    gme_debug.log_initialize ('MobileReleaseStep');


  	  SAVEPOINT release_step_mobile;
    x_return_status := FND_API.G_RET_STS_SUCCESS;

    l_act_strt_dt   := TO_DATE(p_act_strt_dt, p_date_format||HOUR_MIN_SEC_FORMAT_STRING);

    -- Bug 19911453
    gme_debug.put_line('In ReleaseStep..l_act_strt_dt  is:' || l_act_strt_dt);
    gme_debug.put_line('In ReleaseStep..parameter act_strt_dt  is:' || p_act_strt_dt);


    fnd_profile.put('USER_ID',to_char(p_uid));
    gme_common_pvt.g_user_name  := p_uname;
    gme_common_pvt.g_user_ident := p_uid;
    gme_common_pvt.g_login_id   := TO_NUMBER (fnd_profile.VALUE ('LOGIN_ID'));
    gme_common_pvt.g_user_ident := TO_NUMBER (fnd_profile.VALUE ('USER_ID'));

    l_batch_hdr_rec.batch_id       := p_batch_id;
    IF NOT (gme_batch_header_dbl.fetch_row(l_batch_hdr_rec, l_batch_hdr_rec)) THEN
      RAISE expected_error;
    END IF;

    l_batch_step.batch_id          := p_batch_id;
    l_batch_step.batchstep_id      := p_step_id;
    IF NOT (gme_batch_steps_dbl.fetch_row(l_batch_step, l_batch_step)) THEN
      RAISE expected_error;
    END IF;
    l_batch_step.actual_start_date := l_act_strt_dt;

    -- Bug 4774944 Rework.
    -- Check for 'Step controls batch status' parameter.
    IF l_batch_hdr_rec.batch_status = 1 AND
         (gme_common_pvt.g_step_controls_batch_sts_ind <> 1 OR
          l_batch_hdr_rec.parentline_id IS NOT NULL ) THEN
       RAISE validate_step_status_error;
    END IF;

    -- Bug 4774944
    -- Added call to validate step for release.

    gme_release_batch_step_pvt.validate_step_for_release
                       (p_batch_header_rec     => l_batch_hdr_rec
                       ,p_batch_step_rec       => l_batch_step
                       ,x_batch_step_rec       => l_batch_step_out
                       ,x_return_status        => x_return_status);

      -- Bug 19911453
    gme_debug.put_line('In ReleaseStep..Actual Start date  is:' || l_batch_step_out.actual_start_date);


    IF x_return_status <> fnd_api.g_ret_sts_success THEN
       RAISE validate_step_error;
    END IF;

    -- Pass l_batch_step_out with updated dates.
    gme_api_main.release_step (
       p_validation_level            => gme_common_pvt.g_max_errors,
       p_init_msg_list               => FND_API.G_TRUE,
       x_message_count               => x_message_count,
       x_message_list                => x_message_list,
       x_return_status               => x_return_status,
       p_batch_step_rec              => l_batch_step_out, /*  Bug 19911453 */
       p_batch_header_rec            => l_batch_hdr_rec,
       x_batch_step_rec              => x_batch_step,
       x_exception_material_tbl      => x_exception_material,
       p_ignore_exception            => FND_API.G_FALSE);
    IF x_return_status = 'X' THEN
       RAISE step_alloc_error;
    --
    -- bug 10389977
    -- Added the code to perform the transactions on releasing the step
    --
    ELSE
       gme_api_pub.save_batch(x_return_status  => x_return_status,
                             p_header_id       => gme_common_pvt.get_txn_header_id,
                             p_table           => 2,
                             p_commit          => 'T',
                             p_clear_qty_cache => 'T');
    END IF;
  EXCEPTION
     WHEN expected_error THEN
       x_return_status := FND_API.G_RET_STS_ERROR;
    WHEN step_alloc_error THEN
      ROLLBACK TO SAVEPOINT release_step_mobile ;
      FND_MESSAGE.SET_NAME('GME', 'GME_API_UNALLOC_MATERIALS');
      x_message_list := FND_MESSAGE.GET;
    WHEN validate_step_error THEN
     GME_COMMON_PVT.count_and_get (x_count        => l_count
                                  ,p_encoded      => fnd_api.g_false
                                  ,x_data         => x_message_list);

    WHEN validate_step_status_error THEN
      FND_MESSAGE.SET_NAME('GME','GME_API_INV_BATCH_REL_STEP');
      x_message_list := FND_MESSAGE.GET;
    WHEN OTHERS THEN
      x_return_status := FND_API.G_RET_STS_UNEXP_ERROR;
      fnd_msg_pub.add_exc_msg(g_pkg_name, l_api_name);
  END release_step;

  /*###############################################################
  # DESCRIPTION
  #   Bug 4962372 Invoke the Start resource transaction API
  ###############################################################*/
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
     ,x_error_msg       OUT NOCOPY VARCHAR2) IS
     l_message_count NUMBER;
     l_rsrc_txn_rec     gme_resource_txns%ROWTYPE;
     l_in_rsrc_txn_rec  gme_resource_txns%ROWTYPE;
     l_api_name         VARCHAR2(30) := 'Start_Cmplt_Actual_Rsrc_Txn';
  BEGIN
   x_return_status := FND_API.G_RET_STS_SUCCESS;
   x_error_msg     := '';
   FND_PROFILE.put('USER_ID',to_char(p_uid));
   gme_common_pvt.g_user_name  := p_uname;
   gme_common_pvt.g_user_ident := p_uid;
   gme_common_pvt.g_login_id   := TO_NUMBER (fnd_profile.VALUE ('LOGIN_ID'));
   gme_common_pvt.g_user_ident := TO_NUMBER (fnd_profile.VALUE ('USER_ID'));
   l_in_rsrc_txn_rec.line_id         := p_resource_id;
   l_in_rsrc_txn_rec.organization_id := p_organization_id;
   l_in_rsrc_txn_rec.start_date      := p_start_date;
   l_in_rsrc_txn_rec.end_date        := p_start_date;
   l_in_rsrc_txn_rec.instance_id     := p_instance_id;
   l_in_rsrc_txn_rec.trans_date      := p_trans_date;
   l_in_rsrc_txn_rec.reason_id       := p_reason_id;

   GME_API_PUB.start_cmplt_actual_rsrc_txn (
      p_api_version        => 2.0
     ,p_init_msg_list      => FND_API.G_TRUE
     ,p_commit             => FND_API.G_TRUE
     ,p_org_code           => NULL
     ,p_instance_no        => NULL
     ,p_rsrc_txn_rec       => l_in_rsrc_txn_rec
     ,x_rsrc_txn_rec       => l_rsrc_txn_rec
     ,x_message_count      => l_message_count
     ,x_message_list       => x_error_msg
     ,x_return_status      => x_return_status);
    x_trans_id := l_rsrc_txn_rec.poc_trans_id;
  EXCEPTION
    WHEN OTHERS THEN
      x_return_status := FND_API.G_RET_STS_UNEXP_ERROR;
      fnd_msg_pub.add_exc_msg(g_pkg_name, l_api_name);
  END Start_Cmplt_Actual_Rsrc_Txn;

  /*###############################################################
  # DESCRIPTION
  #     This procedure Validates that a date has been entered if
  #     not it will be initialized. Following scenarios are checked
  #     Step Actual Completin Date >= Batch Step Actual Start Date
  #     Actual Completion Date <= Sysdate
  ###############################################################*/
  PROCEDURE Validate_Step_Completion_Date (p_start_date IN VARCHAR2,
                                           p_complt_date    IN  VARCHAR2,
                                           p_date_format    IN  VARCHAR2,
                                           p_batch_id      IN  NUMBER,
                                           x_return_status OUT NOCOPY VARCHAR2,
                                           x_error_msg     OUT NOCOPY VARCHAR2) IS
    l_sysdate    DATE;
    l_status    NUMBER(5);
    l_batch_strt_dt DATE;
    l_complt_date DATE;
    FUTURE_DATE_EX             EXCEPTION;
    INVALID_STEP_COMPLT_DATE   EXCEPTION;
    l_api_name         VARCHAR2(30) := 'Validate_Step_Completion_Date';

    --
    -- bug 14376915
    --
    l_start_date  DATE;
  BEGIN
    x_return_status := FND_API.G_RET_STS_SUCCESS;
    l_sysdate := SYSDATE;
    l_complt_date := TO_DATE(p_complt_date, p_date_format||HOUR_MIN_SEC_FORMAT_STRING);

    --
    -- bug 14376915
    --
    l_start_date  := TO_DATE(p_start_date, p_date_format||HOUR_MIN_SEC_FORMAT_STRING);

    IF l_complt_date > SYSDATE THEN
       RAISE FUTURE_DATE_EX;
    END IF;

    --
    -- bug 14376915
    --
    -- IF p_complt_date < p_start_date THEN
    IF l_complt_date < l_start_date THEN
       RAISE INVALID_STEP_COMPLT_DATE;
    END IF;
  EXCEPTION
    WHEN FUTURE_DATE_EX THEN
        x_return_status := FND_API.G_RET_STS_ERROR;
        FND_MESSAGE.SET_NAME('GMA', 'SY_NOFUTUREDATE');
        x_error_msg := FND_MESSAGE.GET;
    WHEN INVALID_STEP_COMPLT_DATE THEN
        x_return_status := FND_API.G_RET_STS_ERROR;
        FND_MESSAGE.SET_NAME('GME', 'GME_CMPLT_DATE_OUTSIDE');
        x_error_msg := FND_MESSAGE.GET;
    WHEN OTHERS THEN
        x_return_status := FND_API.G_RET_STS_UNEXP_ERROR;
        fnd_msg_pub.add_exc_msg(g_pkg_name, l_api_name);
        x_error_msg := FND_MESSAGE.GET;
  END Validate_Step_Completion_Date;

  /*###############################################################
  # DESCRIPTION
  #     This procedure Validates the step qty entered. It ensures that
  #     the data entered is a number.
  ###############################################################*/
  PROCEDURE Validate_Step_Qty ( p_step_qty      IN  VARCHAR2,
                                 x_return_status OUT NOCOPY VARCHAR2,
                                 x_error_msg     OUT NOCOPY VARCHAR2) IS
    l_step_qty NUMBER;
    l_api_name VARCHAR2(30) := 'Validate_Step_Qty';
  BEGIN
   x_return_status := FND_API.G_RET_STS_SUCCESS;
   l_step_qty := TO_NUMBER(p_step_qty);
  EXCEPTION
    WHEN VALUE_ERROR THEN
      x_return_status := FND_API.G_RET_STS_ERROR;
      FND_MESSAGE.SET_NAME('GME', 'GME_VALUE_ERROR');
      x_error_msg := FND_MESSAGE.GET;
    WHEN OTHERS THEN
      x_return_status := FND_API.G_RET_STS_UNEXP_ERROR;
      fnd_msg_pub.add_exc_msg(g_pkg_name, l_api_name);
      x_error_msg := FND_MESSAGE.GET;
  END Validate_Step_Qty;

  /*###############################################################
  # DESCRIPTION
  #     This procedure Validates that a date has been entered if
  #     not it will be initialized. Following scenarios are checked
  #     Step Rel Actual Start Date >= Batch Actual Start Date
  #     Actual Start Date <= Sysdate
  ###############################################################*/
  PROCEDURE Validate_Step_Start_Date (p_start_date    IN  VARCHAR2,
                                      p_date_format    IN  VARCHAR2,
                                      p_batch_id      IN  NUMBER,
                                      x_return_status OUT NOCOPY VARCHAR2,
                                      x_error_msg     OUT NOCOPY VARCHAR2) IS
    l_sysdate    DATE;
    l_status    NUMBER(5);
    l_batch_strt_dt DATE;
    l_start_date DATE;
    l_api_name         VARCHAR2(30) := 'Validate_Step_Start_Date';
    CURSOR Cur_get_stat IS
      SELECT batch_status
      FROM   gme_batch_header
      WHERE  batch_id = p_batch_id;
    CURSOR Cur_get_batch_start_dt IS
      SELECT actual_start_date
      FROM   gme_batch_header
      WHERE  batch_id = p_batch_id;
    FUTURE_DATE_EX             EXCEPTION;
    INVALID_STEP_START_DATE    EXCEPTION;
  BEGIN
    x_return_status := FND_API.G_RET_STS_SUCCESS;
    l_sysdate := SYSDATE;
    l_start_date := TO_DATE(p_start_date, p_date_format||HOUR_MIN_SEC_FORMAT_STRING);
    IF l_start_date > SYSDATE THEN
       RAISE FUTURE_DATE_EX;
    END IF;
    OPEN Cur_get_stat;
    FETCH Cur_get_stat INTO l_status;
    CLOSE Cur_get_stat;
    IF l_status = 2 THEN
       OPEN Cur_get_batch_start_dt;
       FETCH Cur_get_batch_start_dt INTO l_batch_strt_dt;
       CLOSE Cur_get_batch_start_dt;
       IF l_batch_strt_dt > l_start_date THEN
          RAISE INVALID_STEP_START_DATE;
       END IF;
    END IF;
  EXCEPTION
    WHEN FUTURE_DATE_EX THEN
        x_return_status := FND_API.G_RET_STS_ERROR;
        FND_MESSAGE.SET_NAME('GMA', 'SY_NOFUTUREDATE');
        x_error_msg := FND_MESSAGE.GET;
    WHEN INVALID_STEP_START_DATE THEN
        x_return_status := FND_API.G_RET_STS_ERROR;
        FND_MESSAGE.SET_NAME('GME', 'GME_STEP_START_BATCH_START_ERR');
        x_error_msg := FND_MESSAGE.GET;
    WHEN OTHERS THEN
        x_return_status := FND_API.G_RET_STS_UNEXP_ERROR;
        fnd_msg_pub.add_exc_msg(g_pkg_name, l_api_name);
        x_error_msg := FND_MESSAGE.GET;
  END Validate_Step_Start_Date;

  PROCEDURE check_close_period(p_org_id        IN NUMBER,
                               p_trans_date    IN DATE,
                               x_return_status OUT NOCOPY VARCHAR2,
                               x_message       OUT NOCOPY VARCHAR2) IS
    l_count NUMBER;
  BEGIN
    x_return_status := FND_API.G_RET_STS_SUCCESS;
    IF NOT (gme_common_pvt.check_close_period(p_org_id, p_trans_date)) THEN
      gme_common_pvt.count_and_get(p_encoded => FND_API.G_FALSE,
                                   x_count   => l_count,
                                   x_data    => x_message);
    END IF;
  END check_close_period;
  /*###############################################################
  # DESCRIPTION
  #     This procedure Loads Resource Transactions
  ###############################################################*/
  PROCEDURE Load_resource_Txns (p_batch_id    IN  NUMBER,
                                x_row_count   OUT NOCOPY NUMBER,
                                x_return_status OUT NOCOPY VARCHAR2) IS

     l_batch_rec                gme_batch_header%ROWTYPE;
    expected_error       EXCEPTION;
  BEGIN
      l_batch_rec.batch_id := p_batch_id;
      IF NOT (gme_batch_header_dbl.fetch_row(l_batch_rec, l_batch_rec)) THEN
        RAISE expected_error;
      END IF;

      gme_trans_engine_util.load_rsrc_trans (p_batch_row          => l_batch_rec
                                            ,x_rsc_row_count      => x_row_count
                                            ,x_return_status      => x_return_status);
  EXCEPTION
     WHEN expected_error THEN
       x_return_status := FND_API.G_RET_STS_ERROR;
     WHEN OTHERS THEN
       x_return_status := FND_API.G_RET_STS_UNEXP_ERROR;
  END Load_resource_Txns;

 /* Bug#5663458 Begin
  * Created the following procedure. This procedure is to get transaction date of
  *  material line depends on all release type and step associations
  */
  PROCEDURE fetch_txn_date(p_material_detail_id   IN  NUMBER,
                           x_trans_date           OUT NOCOPY  VARCHAR2,
                           x_return_status        OUT NOCOPY  VARCHAR2,
                           x_error_msg            OUT NOCOPY  VARCHAR2)
  IS
    l_trans_date   DATE;
    l_count        NUMBER;
    l_date_format  VARCHAR2(100);
  BEGIN
    IF (g_debug IS NOT NULL) THEN
    gme_debug.log_initialize ('RelieveRsrvPndLots');
    END IF;

    x_return_status := fnd_api.g_ret_sts_success;
    x_error_msg     := ' ';

    --calling gme_common_pvt routine
    gme_common_pvt.fetch_trans_date(p_material_detail_id => p_material_detail_id,
                                    p_invoke_mode   => 'T',
                                    x_trans_date    => l_trans_date,
                                    x_return_status => x_return_status);

    IF x_return_status <> fnd_api.g_ret_sts_success THEN
       gme_common_pvt.count_and_get (x_count        => l_count
                                    ,p_encoded      => fnd_api.g_false
                                    ,x_data         => x_error_msg);

    END IF;

    --getting the date into either mwa date format mask or ICX date format mask
    FND_PROFILE.GET('MWA_DATE_FORMAT_MASK',l_date_format);

    IF l_date_format IS NULL THEN
      FND_PROFILE.GET('ICX_DATE_FORMAT_MASK',l_date_format);
    END IF;

    x_trans_date := TO_CHAR(l_trans_date,l_date_format||HOUR_MIN_SEC_FORMAT_STRING);

  EXCEPTION
   WHEN OTHERS THEN
     IF g_debug <= gme_debug.g_log_unexpected THEN
       gme_debug.put_line('When others exception in fetch_txn_date');
     END IF;
     fnd_msg_pub.add_exc_msg('GME_MOBILE_TXN','fetch_txn_date');
     x_return_status := fnd_api.g_ret_sts_unexp_error;
     x_error_msg     := fnd_message.get;
  END;

 --Bug#5867209 added restricted subinv code
 PROCEDURE Validate_Subinv_Master(p_organization_id    IN         NUMBER,
                                  p_subinventory_code   IN         VARCHAR2,
                                  p_inventory_item_id   IN         NUMBER,
                                  p_restrict_code       IN         NUMBER,
                                  x_locator_type        OUT NOCOPY VARCHAR2,
                                  x_return_status       OUT NOCOPY VARCHAR2,
                                  x_error_message       OUT NOCOPY VARCHAR2)
  IS
    CURSOR x_sub_lov IS
     SELECT NVL(locator_type, 1)
      FROM mtl_secondary_inventories
     WHERE organization_id = p_organization_id
       AND NVL(disable_date, SYSDATE + 1) > SYSDATE
       AND secondary_inventory_name = p_subinventory_code
       AND quantity_tracked = 1;

  CURSOR x_sub_lov1 IS
    SELECT NVL(s.locator_type, 1)
      FROM mtl_secondary_inventories s,
           mtl_item_sub_inventories i
     WHERE s.secondary_inventory_name = i.secondary_inventory
       AND s.organization_id = i.organization_id
       AND s.organization_id = p_organization_id
       AND i.inventory_item_id = p_inventory_item_id
       AND NVL(s.disable_date, SYSDATE + 1) > SYSDATE
       AND secondary_inventory_name = p_subinventory_code
       AND s.quantity_tracked = 1;

   l_exists          NUMBER;
   ERROR_INV_SUBINV  EXCEPTION;
 BEGIN
   x_return_status := fnd_api.g_ret_sts_success;
   x_error_message := ' ';

   IF p_restrict_code = 1 THEN
    OPEN x_sub_lov1;
    FETCH x_sub_lov1 INTO x_locator_type;
    IF x_sub_lov1%NOTFOUND THEN
     RAISE ERROR_INV_SUBINV;
    END IF;
    CLOSE x_sub_lov1 ;
   ELSE
    OPEN x_sub_lov;
    FETCH x_sub_lov INTO x_locator_type;
    IF x_sub_lov%NOTFOUND THEN
     RAISE ERROR_INV_SUBINV;
    END IF;
    CLOSE x_sub_lov ;
   END IF;

 EXCEPTION
 WHEN ERROR_INV_SUBINV THEN
  x_return_status := fnd_api.g_ret_sts_error;
  FND_MESSAGE.SET_NAME('GME','GME_NOT_VALID_SUBINV');
  x_error_message := FND_MESSAGE.GET;
 END Validate_Subinv_Master;

 --Bug#5867209 added restricted locator code
 PROCEDURE Validate_Locator_Master(p_organization_id     IN          NUMBER,
                                   p_subinventory_code   IN          VARCHAR2,
                                   p_locator_code        IN          VARCHAR2,
                                   p_inventory_item_id   IN          NUMBER,
                                   p_restrict_code       IN          NUMBER,
                                   x_locator_id          OUT NOCOPY  VARCHAR2,
                                   x_return_status       OUT NOCOPY  VARCHAR2,
                                   x_error_message       OUT NOCOPY  VARCHAR2)
  IS
   CURSOR x_loc_lov IS
      SELECT inventory_location_id
        FROM wms_item_locations_kfv
       WHERE organization_id = p_organization_id
         AND NVL(disable_date, SYSDATE + 1) > SYSDATE
         AND subinventory_code = p_subinventory_code
         AND concatenated_segments = p_locator_code;

   CURSOR x_loc_lov1 IS
    SELECT a.inventory_location_id
    FROM  wms_item_locations_kfv a,
          mtl_secondary_locators b
    WHERE b.organization_id = p_organization_Id
     AND  b.inventory_item_id = p_Inventory_Item_Id
     AND  b.subinventory_code = p_Subinventory_Code
     AND  a.inventory_location_id = b.secondary_locator
     AND  NVL(a.disable_date, SYSDATE+1) > SYSDATE
     AND  a.concatenated_segments = p_locator_code;

   l_exists NUMBER;
   ERROR_INVALID_LOCATOR EXCEPTION;
 BEGIN
   x_return_status := fnd_api.g_ret_sts_success;
   x_error_message := ' ';

  IF p_restrict_code = 1 THEN
    OPEN x_loc_lov1;
    FETCH x_loc_lov1 INTO x_locator_id;
    IF x_loc_lov1%NOTFOUND THEN
     RAISE ERROR_INVALID_LOCATOR;
    END IF;
    CLOSE x_loc_lov1 ;
  ELSE
    OPEN x_loc_lov;
    FETCH x_loc_lov INTO x_locator_id;
    IF x_loc_lov%NOTFOUND THEN
     RAISE ERROR_INVALID_LOCATOR;
    END IF;
    CLOSE x_loc_lov ;
  END IF;
 EXCEPTION
  WHEN ERROR_INVALID_LOCATOR THEN
   x_return_status := fnd_api.g_ret_sts_error;
   FND_MESSAGE.SET_NAME('GME','GME_NOT_VALID_LOC');
   x_error_message := FND_MESSAGE.GET;
 END Validate_Locator_Master;
 /* Bug#5663458 End*/

 --Bug#5867209 added the procedure
 PROCEDURE Fetch_subinv_locator(p_batch_id            IN         NUMBER,
                                p_material_detail_id  IN         NUMBER,
                                x_subinventory_code   OUT NOCOPY VARCHAR2,
                                x_locator             OUT NOCOPY VARCHAR2,
                                x_locator_id          OUT NOCOPY VARCHAR2,
                                x_return_status       OUT NOCOPY VARCHAR2,
                                x_error_msg           OUT NOCOPY VARCHAR2)
IS
 CURSOR c_subinv_loc IS
  SELECT subinventory, loc.concatenated_segments,
         m.locator_id
    FROM gme_material_details m, wms_item_locations_kfv loc
   WHERE m.subinventory = loc.subinventory_code (+)
     AND m.locator_id = loc.inventory_location_id (+)
     AND m.batch_id = p_batch_id
     AND m.material_Detail_id = p_material_detail_id;
BEGIN
   IF (g_debug IS NOT NULL) THEN
    gme_debug.log_initialize ('MobileFetchSubinvLoc');
   END IF;


  x_return_status := FND_API.G_RET_STS_SUCCESS;
  x_error_msg     := ' ';

  OPEN c_subinv_loc;
  FETCH c_subinv_loc INTO x_subinventory_code,x_locator,x_locator_id ;
  CLOSE c_subinv_loc;

EXCEPTION
    WHEN OTHERS THEN
      IF g_debug <= gme_debug.g_log_unexpected THEN
        gme_debug.put_line('When others exception in Fetch_subinv_locator');
      END IF;
      fnd_msg_pub.add_exc_msg('GME_MOBILE_TXN','Fetch_subinv_locator');
      x_return_status := FND_API.G_RET_STS_UNEXP_ERROR;
      x_error_msg     := fnd_message.get;
END Fetch_subinv_locator;

-- nsinghi bug#5209065 START. Added following Procs.
PROCEDURE get_expiration_date (
   x_expiration_date    OUT NOCOPY DATE
   , x_return_status    OUT NOCOPY VARCHAR2
   ) IS

  l_mti_txn_tbl           INV_CALCULATE_EXP_DATE.MTI_TAB;
  l_mti_txn_rec           MTL_TRANSACTIONS_INTERFACE%ROWTYPE;
  l_mtli_txn_rec          MTL_TRANSACTION_LOTS_INTERFACE%ROWTYPE;
  l_mmtt_txn_rec          MTL_MATERIAL_TRANSACTIONS_TEMP%ROWTYPE;
  l_mtlt_txn_rec          MTL_TRANSACTION_LOTS_TEMP%ROWTYPE;
  l_lot_expiration_date   DATE;

BEGIN
   x_return_status := FND_API.G_RET_STS_SUCCESS;
   IF (g_debug IS NOT NULL) THEN
    gme_debug.log_initialize ('MobileGetExpDate');
   END IF;

   l_mti_txn_tbl := inv_calculate_exp_date.get_mti_tbl;
   IF l_mti_txn_tbl.COUNT > 0 THEN
      l_mti_txn_rec := l_mti_txn_tbl(0);
      inv_calculate_exp_date.get_lot_expiration_date(
                   p_mtli_lot_rec        => l_mtli_txn_rec
                  ,p_mti_trx_rec         => l_mti_txn_rec
                  ,p_mtlt_lot_rec        => l_mtlt_txn_rec
                  ,p_mmtt_trx_rec        => l_mmtt_txn_rec
                  ,p_table               => 1
                  ,x_lot_expiration_date => l_lot_expiration_date
                  ,x_return_status       => x_return_status);

      inv_calculate_exp_date.purge_mti_tab;
      IF x_return_status <> fnd_api.g_ret_sts_success THEN
         IF g_debug <= gme_debug.g_log_statement THEN
            gme_debug.put_line('Program inv_calculate_exp_date.get_lot_expiration_date has failed with a Unexpected exception');
         END IF;
         FND_MESSAGE.SET_NAME('INV','INV_PROGRAM_ERROR');
         FND_MESSAGE.SET_TOKEN('PROG_NAME','inv_calculate_exp_date.get_lot_expiration_date');
         fnd_msg_pub.ADD;
         RAISE fnd_api.g_exc_unexpected_error;
      END IF;
      IF g_debug = 1 THEN
         gme_debug.put_line('l_lot_expiration_date '||l_lot_expiration_date);
      END IF;
      x_expiration_date := l_lot_expiration_date;
--      ELSE
--           x_expiration_date := x_origination_date + l_get_dft_attr_rec.shelf_life_days;
   END IF;
EXCEPTION
   WHEN OTHERS THEN
      x_return_status := FND_API.G_RET_STS_UNEXP_ERROR;
      IF g_debug <= gme_debug.g_log_unexpected THEN
         gme_debug.put_line('WHEN OTHERS exception : '||SQLERRM);
      END IF;
END get_expiration_date;

PROCEDURE get_exp_action_date (
   p_expiration_date	   IN DATE
   , p_exp_act_interval IN NUMBER
   , x_exp_act_date     OUT NOCOPY DATE
   , x_return_status    OUT NOCOPY VARCHAR2
   ) IS
BEGIN
   x_return_status := FND_API.G_RET_STS_SUCCESS;
   x_exp_act_date := p_expiration_date;

   IF p_expiration_date IS NOT NULL AND p_exp_act_interval IS NOT NULL THEN
      x_exp_act_date := p_expiration_date + p_exp_act_interval;
   END IF;

EXCEPTION
   WHEN OTHERS THEN
      x_return_status := FND_API.G_RET_STS_UNEXP_ERROR;
      IF g_debug <= gme_debug.g_log_unexpected THEN
         gme_debug.put_line('WHEN OTHERS exception : '||SQLERRM);
      END IF;
END get_exp_action_date;
-- nsinghi bug#5209065 END.

--
-- Bug 9367054
-- New procedure to print the labels for
-- product transactions
--
PROCEDURE print_label(p_txn_header_id IN NUMBER,
                      x_return_status OUT NOCOPY VARCHAR2,
                      x_error_msg     OUT NOCOPY VARCHAR2) as

  --
  -- bug 12382498
  -- Querying the transaction_type_id as well
  --
  CURSOR get_details IS
       SELECT
          msi.concatenated_segments item,
          bh.batch_no batch_no,
          mmt.transaction_id,
          mmt.created_by,
          mmt.transaction_type_id
       FROM gme_batch_header_vw bh,
            mtl_parameters mp,
            gme_material_details bl,
            mtl_system_items_vl msi,
            mtl_material_transactions  mmt
      where mmt.transaction_set_id = p_txn_header_id AND
            bh.batch_id            = mmt.TRANSACTION_SOURCE_ID  AND
            bl.material_detail_id  = mmt.TRX_SOURCE_LINE_ID AND
            mmt.organization_id    = mp.organization_id AND
            bh.batch_id            = bl.batch_id AND
            bl.organization_id     = msi.organization_id AND
            bl.inventory_item_id   = msi.inventory_item_id;

  l_context         gmo_label_mgmt_grp.context_table;
  l_item_no         varchar2(2000);
  l_batch_no        varchar2(2000);

  l_label_id        number;

  l_ret_status      varchar2(2000);
  l_msg_count       number;
  l_msg_data        varchar2(2000);

  l_print_status    varchar2(2000);
  l_txn_id          Number;
  l_txn_type_id     NUMBER;
  l_user_id         number;
BEGIN
  x_return_status := FND_API.G_RET_STS_SUCCESS;
  x_error_msg     := ' ';

  OPEN get_details;
  FETCH get_details INTO l_item_no, l_batch_no, l_txn_id, l_user_id, l_txn_type_id;
  CLOSE get_details;

  --
  -- bug 12382498
  -- If the transaction is not a prod/byprod completion
  -- or if the Label Auto printing is switched off, then the
  -- labels are not to be printed
  --
  IF (l_txn_type_id NOT IN (GME_COMMON_PVT.g_prod_completion, GME_COMMON_PVT.g_byprod_completion) OR
      GMO_LABEL_MGMT_GRP.AUTO_PRINT_ENABLED () = FALSE) THEN
    x_return_status := 'S';
    return;
  END IF;

  --
  -- Build the context object
  --
  L_CONTEXT(1).NAME :='GME_BATCH_NO';
  L_CONTEXT(1).VALUE:= l_batch_no;
  L_CONTEXT(1).DISPLAY_SEQUENCE:=1;

  L_CONTEXT(2).NAME :='GME_ITEM_NO';
  L_CONTEXT(2).VALUE:= l_item_no;
  L_CONTEXT(2).DISPLAY_SEQUENCE:=2;

  --
  -- Add the label details to the GMO tables
  --
  GMO_LABEL_MGMT_GRP.PRINT_LABEL
            (P_API_VERSION            => 1,
             P_INIT_MSG_LIST          =>'T',
             X_RETURN_STATUS          => l_ret_status,
             X_MSG_COUNT              => l_msg_count,
             X_MSG_DATA               => l_msg_data,
             P_ENTITY_NAME            => 'GME_PRODUCT',
             P_ENTITY_KEY             => l_txn_id,
             P_WMS_BUSINESS_FLOW_CODE => 39,
             P_LABEL_TYPE             => 14,
             P_TRANSACTION_ID         => l_txn_id,
             P_TRANSACTION_TYPE       => 1,
             P_APPLICATION_SHORT_NAME => 'GME',
             P_REQUESTER              => l_user_id,
             P_CONTEXT                => l_context,
             X_LABEL_ID               => l_label_id);

  --
  -- Post the label to Inventory
  --
  GMO_LABEL_MGMT_GRP.COMPLETE_LABEL_PRINT(
       p_api_version       => 1,
       p_init_msg_list     => 'T',
       x_return_status     => l_ret_status,
       x_msg_count         => l_msg_count,
       x_msg_data          => l_msg_data,
       P_LABEL_ID          => l_label_id,
       P_ERECORD_ID        => NULL,
       P_ERECORD_STATUS    => NULL,
       x_print_status      => l_print_status);

  x_return_status := l_ret_status;
EXCEPTION
  WHEN OTHERS THEN
    x_return_status := FND_API.G_RET_STS_UNEXP_ERROR;
END print_label;

END GME_MOBILE_TXN;
/

--show errors;
COMMIT;
EXIT;
