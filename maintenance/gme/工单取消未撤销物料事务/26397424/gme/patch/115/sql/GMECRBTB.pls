SET VERIFY OFF
WHENEVER SQLERROR EXIT FAILURE ROLLBACK;
WHENEVER OSERROR EXIT FAILURE ROLLBACK;
REM dbdrv: sql ~PROD ~PATH ~FILE none none none package &phase=plb checkfile(120.5.12010000.13=120.9.12020000.8)(120.5.12010000.12=120.9.12020000.7):~PROD:~PATH:~FILE
CREATE OR REPLACE PACKAGE BODY  GME_MAKE_TO_ORDER_PVT AS
/*  $Header: GMECRBTB.pls 120.9.12020000.8 2016/10/28 15:02:18 adeshmuk ship $
 +=========================================================================+
 |                Copyright (c) 2000, 2016 Oracle Corporation                    |
 |                        TVP, Reading, England                            |
 |                         All rights reserved                             |
 +=========================================================================+
 | FILENAME                                                                |
 |    GMECRBTB.pls                                                         |
 |                                                                         |
 | DESCRIPTION                                                             |
 |     This package contains private utilities  relating to OPM            |
 |     reservation.                                                        |
 |                                                                         |
 |                                                                         |
 | HISTORY                                                                 |
 |     Dec, 2007  Srinivasulu Puri Created                                 |
 | 01-APR-2010   G. Muratore    Bug 9355688                                |
 |    Call FND_GLOBAL.apps_initialize with proper values not hard coded -1.|
 |                                                                         |
 | 27-JUN-2011   G. Muratore    Bug 12664771                               |
 |    Do not initialize plan start date.                                   |          
 | 9-FEB-2012    Archana Mundhe    Bug 13359707                            |
 |    Moved fetching of cursor Get_order_info earlier in the code so that  |
 |    the order_no can be used in the debug messgaes. Added several debug  |
 |    messages that will be displayed in the concurrent log so that the    | 
 |    users will know which orders had errors creating the batch, etc.     |
 | 29-Apr-2014   Archana Mundhe    Bug 18329535                            |
 |     Modified code so that item uom is used for uom conversion instead of|
 |     of using uom code from reservation.                                 |
 | 6-JUN-2014   Archana Mundhe    Bug 18960929                             |
 |      Removed all gme debugs and replace with FND debug logging so the   |
 |      messages are seen in the concurrent log.                           |
 | 29-SEP-2014  Archana Mundhe    Bug 19624259                             |
 |      Set source_document_type_id correctly for internal sales order line|
 |                                                                         | 
 | 09-MAR-2015  G. Muratore       Bug 19228370 Rework of 12664771          |
 |      Initialize plan start date if there is no schedule_ship_date.      |
 |      PROCEDURE: create_batch_for_order_line                             |
 | 28-SEP-2015  A. Mundhe         Bug 21890861 Only log detail messages if |
 |      gme debug profile is set at statement.                             |
 | 28-OCT-2016  A. Mundhe         Bug 24949569 Modified code in procedure  |
 | create_batch_for_order_line to consider existing reservations for ISO's |
 +=========================================================================+
  API Name  : GME_MAKE_TO_ORDER_PVT
  Type      : Private
  Function  : This package contains private procedures used to support   
              OPM make to order functionality.                     
              The user defines a make to order rule and the conditions under
              which it will be deployed.  When an order line qualifies for  
              make to order processing, a GME Batch/FPO is created and then
              the sales order line is reserved to it.
  Pre-reqs  : Set up of Make to Order Rules
  Parameters: Per function

  Current Vers  : 1.0

*/
G_DEBUG                       VARCHAR2 (5)  := fnd_profile.VALUE ('AFLOG_LEVEL');

PROCEDURE create_batch_for_order_line
(
--  errbuf          OUT NOCOPY VARCHAR2
-- retcode         OUT NOCOPY VARCHAR2
  p_api_version   IN  NUMBER   := 1.0
 ,p_init_msg_list IN  VARCHAR2 := fnd_api.g_false
 ,p_commit        IN  VARCHAR2 := fnd_api.g_false
 ,p_so_line_id    IN  NUMBER
) IS

-- Standard constants to be used to check for call compatibility.
l_api_version   CONSTANT        NUMBER          := 1.0;
l_api_name      CONSTANT        VARCHAR2(30):= 'create_batch_for_order_line';

-- Local Variables.
l_msg_count                    NUMBER  :=0;
l_msg_data                     VARCHAR2(2000);
l_message	               VARCHAR2(2000);
i		               NUMBER;
j		               NUMBER;
l_return_status                VARCHAR2(1);
l_batch_qty                    NUMBER;
l_batch_qty2                   NUMBER;
l_so_line_id	             NUMBER;
l_res_qty_so_um                NUMBER;

l_batch_header_rec             GME_BATCH_HEADER%ROWTYPE;
l_out_batch_header_rec         GME_BATCH_HEADER%ROWTYPE;
l_material_detail_rec          GME_MATERIAL_DETAILS%ROWTYPE;
l_so_line_rec	               OE_ORDER_LINES_ALL%ROWTYPE;
l_item_rec                     MTL_SYSTEM_ITEMS_B%ROWTYPE;
l_so_batch_rsv_rec             GML_BATCH_OM_UTIL.gme_om_reservation_rec;
l_pending_product_lots_rec     GME_PENDING_PRODUCT_LOTS%ROWTYPE;
l_out_pending_product_lots_rec GME_PENDING_PRODUCT_LOTS%ROWTYPE;
l_rsv_rec                      INV_RESERVATION_GLOBAL.mtl_reservation_rec_type;            
l_in_serial_num                INV_RESERVATION_GLOBAL.serial_number_tbl_type;
l_out_serial_num               INV_RESERVATION_GLOBAL.serial_number_tbl_type;
l_exception_material_tbl       GME_COMMON_PVT.exceptions_tab;
l_rsv_tbl                      GME_COMMON_PVT.reservations_tab;
l_rsv_row_rec                  MTL_RESERVATIONS%ROWTYPE;
l_in_assignments_rec           GME_MTO_RULE_ASSIGNMENTS%ROWTYPE;
l_mto_rules_rec                GME_MTO_RULES%ROWTYPE;
l_mto_assignments_rec          GME_MTO_RULE_ASSIGNMENTS%ROWTYPE;

l_item_id                      NUMBER;
l_primary_uom_code             VARCHAR2(3);
l_secondary_uom_code           VARCHAR2(3);
l_total_primary_rsv_qty        NUMBER;                     
l_ret_status                   NUMBER;
l_item_no                      VARCHAR2(80);               
l_lot_no 	               VARCHAR2(80) := NULL;
ll_lot_no 	               VARCHAR2(80) := NULL;
l_firmed_ind	               NUMBER;
l_user_id	               NUMBER;

-- Bug 9355688 - New variables.
l_resp_id	               NUMBER;
l_resp_appl_id                 NUMBER;

l_reserved_qty                 NUMBER;
l_reserved_qty2                NUMBER;
l_so_line_no                   NUMBER;
l_order_no                     NUMBER; 


x_msg_count                    NUMBER  :=0;
x_msg_data                     VARCHAR2(2000);
x_return_status                VARCHAR2(1);

l_ship_from_org_id             NUMBER;
l_inventory_item_id            NUMBER;
l_temp                         BOOLEAN;

l_quantity_reserved            NUMBER;
l_reservation_id               NUMBER;
l_salesorder_id                NUMBER;
l_count                        NUMBER:=0;
l_tree_id                      INTEGER;

l_qoh                          NUMBER;
l_rqoh                         NUMBER;
l_qr                           NUMBER;
l_qs                           NUMBER;
l_att                          NUMBER;
l_atr                          NUMBER;
l_sqoh                         NUMBER;
l_srqoh                        NUMBER;
l_sqr                          NUMBER;
l_sqs                          NUMBER;
l_satt                         NUMBER;
l_satr                         NUMBER;
l_lpn_id                       NUMBER;
l_atr_qty_in_so_um             NUMBER := 0;

l_is_revision_control          BOOLEAN := FALSE;
l_is_lot_control               BOOLEAN := FALSE;
l_is_serial_control            BOOLEAN := FALSE;

CURSOR cur_inv_reservations (p_line_id IN NUMBER) IS
  SELECT   mr.*
    FROM mtl_reservations mr
    WHERE demand_source_type_id IN (INV_RESERVATION_GLOBAL.g_source_type_oe, INV_RESERVATION_GLOBAL.g_source_type_internal_ord) /* Bug 24949569  */
         AND  supply_source_type_id = INV_RESERVATION_GLOBAL.g_source_type_inv
         AND demand_source_line_id = p_line_id                     
         ORDER BY mr.reservation_id;                                                                    

CURSOR cur_prod_reservations (p_line_id IN NUMBER) IS
  SELECT   count(1)
    FROM mtl_reservations mr
    WHERE demand_source_type_id IN (INV_RESERVATION_GLOBAL.g_source_type_oe, INV_RESERVATION_GLOBAL.g_source_type_internal_ord) /* Bug 24949569  */
         AND  supply_source_type_id = INV_RESERVATION_GLOBAL.g_source_type_wip
         AND demand_source_line_id = p_line_id                     
         ORDER BY mr.reservation_id;                                                                    

CURSOR cur_item(p_organization_id IN NUMBER, p_inventory_item_id IN NUMBER) IS
 SELECT  *              
 FROM    mtl_system_items_b m
 WHERE   m.inventory_item_id = p_inventory_item_id
    AND  m.organization_id = p_organization_id;

CURSOR get_so_line_rec(p_so_line_id IN NUMBER) IS
 SELECT * 
   FROM oe_order_lines_all
  WHERE line_id = p_so_line_id;

CURSOR get_batch_line(p_batch_id IN NUMBER, p_inventory_item_id IN NUMBER) IS
 SELECT *                                            
   FROM gme_material_details
  WHERE batch_id = p_batch_id 
    AND inventory_item_id  = p_inventory_item_id
    AND line_type = gme_common_pvt.g_line_type_prod; 


 CURSOR Get_order_info(p_so_line_id IN NUMBER) IS
  SELECT  ol.line_number, mtl.segment1,
          oh.order_number
    FROM  oe_order_headers_all oh
            ,oe_order_lines_all ol
            ,oe_transaction_types_all ot
            ,mtl_system_items mtl
      WHERE  ol.line_id = p_so_line_id 
        and  ol.header_id = oh.header_id
        and  oh.order_type_id = ot.transaction_type_id
        and  mtl.inventory_item_id = ol.inventory_item_id;
          		 
BEGIN
   l_so_line_id := p_so_line_id;
   /*Int variables
    =========================================*/
   x_return_status := FND_API.G_RET_STS_SUCCESS;
   
   IF (g_debug <= gme_debug.g_log_statement) THEN
     FND_FILE.Put_Line(FND_FILE.LOG,g_pkg_name||'.'||l_api_name||' ENTER API  ');
   END IF;
   /* Standard begin of API savepoint
   ===========================================*/
   IF (g_debug <= gme_debug.g_log_statement) THEN
      FND_FILE.Put_Line(FND_FILE.LOG, 'start OPM processing   ');
   END IF;   
   SAVEPOINT create_batch_for_so_line_SP;

   /*Standard call to check for call compatibility.
   ==============================================*/
   IF NOT FND_API.compatible_api_call (
                                l_api_version,
                                p_api_version,
                                l_api_name,
                                g_pkg_name)
   THEN      
         FND_FILE.Put_Line(FND_FILE.LOG,g_pkg_name||'.'||l_api_name||' compatability call failure ');      
      RAISE FND_API.G_EXC_UNEXPECTED_ERROR;
   END IF;

      
   /* Check p_init_msg_list
   =========================================*/
   IF FND_API.to_boolean(p_init_msg_list)
   THEN
      FND_MSG_PUB.initialize;
   END IF;

   IF (g_debug <= gme_debug.g_log_statement) THEN
      FND_FILE.Put_Line(FND_FILE.LOG, 'After Initialize       ');
      FND_FILE.Put_Line(FND_FILE.LOG,g_pkg_name||'.'||l_api_name||' begin processing ');
   END IF;

   IF( NVL(p_so_line_id,0) = 0 ) THEN   
      FND_FILE.Put_Line(FND_FILE.LOG,g_pkg_name||'.'||l_api_name||' sales order line id parameter is missing ');      
      FND_MESSAGE.Set_Name('GMI','Missing');
      FND_MESSAGE.Set_Token('MISSING', 'so_line_id');
      FND_MSG_PUB.Add;
      RAISE FND_API.G_EXC_ERROR;
   END IF;
   
   /* Bug 13359707 - Moved code up */
   /* Get the Order and Line Information */
  OPEN Get_order_info(p_so_line_id);
  FETCH Get_order_info INTO l_so_line_no,l_item_no,
        l_order_no;
  IF(Get_order_info%NOTFOUND) THEN     
     FND_FILE.Put_Line(FND_FILE.LOG,g_pkg_name||'.'||l_api_name ||'Get_order_info%NOTFOUND');     
  END IF;
  CLOSE Get_order_info;
  
   /* ============ IF this line is already reserved to production supply do not process further =========== */   
   IF (g_debug <= gme_debug.g_log_statement) THEN
      FND_FILE.Put_Line(FND_FILE.LOG,g_pkg_name||'.'||l_api_name||' Check for existing reservations against GME PROD supply ');
   END IF;   
   
   OPEN cur_prod_reservations(p_so_line_id);                   
   FETCH cur_prod_reservations INTO l_count;      
   IF (cur_prod_reservations%NOTFOUND) or (l_count=0) THEN     
     FND_FILE.Put_Line(FND_FILE.LOG,g_pkg_name||'.'||l_api_name || ' No existing reservations against PRODUCTION supply');    
   CLOSE cur_prod_reservations;
   ELSE          
     FND_FILE.Put_Line(FND_FILE.LOG,g_pkg_name||'.'||l_api_name || 'Reservations already against PRODUCTION supply so terminate here');     
     CLOSE cur_prod_reservations;
     RETURN;
   END IF;

   /* ============== Start of Order Line Processing ====================*/   
     IF (g_debug <= gme_debug.g_log_statement) THEN
        FND_FILE.Put_Line(FND_FILE.LOG,g_pkg_name||'.'||l_api_name||'Retrieve the sales order line for line_id '||p_so_line_id);
     END IF;
   OPEN  get_so_line_rec(p_so_line_id);
   FETCH get_so_line_rec INTO l_so_line_rec;

   IF(get_so_line_rec%NOTFOUND) THEN
      CLOSE get_so_line_rec;
      FND_FILE.Put_Line(FND_FILE.LOG,g_pkg_name||'.'||l_api_name||'RETURNING - sales order not found for for line_id '||p_so_line_id);
      RAISE FND_API.G_EXC_ERROR;
   END IF;
   CLOSE get_so_line_rec;

   /* ============== Establish Item Characteristics ====================*/   
   IF (g_debug <= gme_debug.g_log_statement) THEN
      FND_FILE.Put_Line(FND_FILE.LOG,g_pkg_name||'.'||l_api_name||' Retrieve Item for organization_id  '||l_so_line_rec.ship_from_org_id);
      FND_FILE.Put_Line(FND_FILE.LOG,g_pkg_name||'.'||l_api_name||' Retrieve Item for inventory_item_id '||l_so_line_rec.inventory_item_id);
   END IF;   
   OPEN cur_item (l_so_line_rec.ship_from_org_id, l_so_line_rec.inventory_item_id);
   FETCH cur_item INTO l_item_rec;
   IF(cur_item%NOTFOUND) THEN
      CLOSE cur_item;               
      FND_FILE.Put_Line(FND_FILE.LOG,g_pkg_name||'.'||l_api_name||'Failure to retrieve inventory item ');      
      RAISE FND_API.G_EXC_ERROR;
   END IF;
   CLOSE cur_item;

   /* ============== Determine whether Make to Order Rule exists ===============*/
   IF (g_debug <= gme_debug.g_log_statement) THEN
      FND_FILE.Put_Line(FND_FILE.LOG,g_pkg_name||'.'||l_api_name||' Determine whether make to order rule exists ');
   END IF;   

   l_in_assignments_rec.organization_id   := l_so_line_rec.ship_from_org_id; 
   l_in_assignments_rec.inventory_item_id := l_so_line_rec.inventory_item_id; 
   l_in_assignments_rec.item_type         := l_item_rec.item_type;      
   l_in_assignments_rec.customer_id       := l_so_line_rec.sold_to_org_id; 
   l_in_assignments_rec.site_use_id       := l_so_line_rec.ship_to_org_id; 

   GME_MAKE_TO_ORDER_PVT.retrieve_rule(
                              p_mto_assignments_rec    => l_in_assignments_rec                               
                             ,x_mto_rules_rec          => l_mto_rules_rec                                 
                             ,x_mto_assignments_rec    => l_mto_assignments_rec                             
                             ,x_return_status          => l_return_status
	                       ,x_msg_data	             => l_msg_data
			           ,x_msg_count	             => l_msg_count);
 
      IF (g_debug <= gme_debug.g_log_statement) THEN
         FND_FILE.Put_Line(FND_FILE.LOG,g_pkg_name||'.'||l_api_name||' Return status from Retrieve_Rule is '||l_return_status);
         FND_FILE.Put_Line(FND_FILE.LOG,g_pkg_name||'.'||l_api_name||' Rule ID to work with is '||l_mto_assignments_rec.rule_id);
         FND_FILE.Put_Line(FND_FILE.LOG,g_pkg_name||'.'||l_api_name||' Rule being honored is '||l_mto_rules_rec.rule_name);
      END IF;   
  
   IF( l_return_status <> FND_API.G_RET_STS_SUCCESS ) THEN
      FND_FILE.Put_Line(FND_FILE.LOG,' Failure to Get_Rule so terminate processing for order '||l_order_no); -- Bug 13359707       
      FND_FILE.Put_Line(FND_FILE.LOG,g_pkg_name||'.'||l_api_name||' Failure to Get_Rule so terminate processing for line '||p_so_line_id);
      FND_MESSAGE.Set_Name('GMI','Missing');
      FND_MESSAGE.Set_Token('MISSING', 'rule_id');
      FND_MSG_PUB.Add;
      RAISE FND_API.G_EXC_ERROR;
   END IF; 

   IF (NVL(l_mto_rules_rec.rule_id,0) = 0) THEN
     FND_FILE.Put_Line(FND_FILE.LOG,' No MAKE to ORDER rule exists to cover order  '||l_order_no); -- Bug 13359707 
     FND_FILE.Put_Line(FND_FILE.LOG,' No MAKE to ORDER rule exists to cover order line  '||l_so_line_no); -- Bug 13359707       
     RETURN;
   END IF;

   /* ============== Start Processing Make to Order Rule ===============*/
   IF (g_debug <= gme_debug.g_log_statement) THEN
      FND_FILE.Put_Line(FND_FILE.LOG,g_pkg_name||'.'||l_api_name||' Start processing rule definition for '||l_mto_rules_rec.rule_name);
   END IF;
   /* ================== Assess the rule for checking available inventory =================*/
   IF (g_debug <= gme_debug.g_log_statement) THEN 
      FND_FILE.Put_Line(FND_FILE.LOG,g_pkg_name||'.'||l_api_name||' Check Availability set to  '||l_mto_rules_rec.check_availability);
   END IF;   
   IF NVL(l_mto_rules_rec.check_availability,'N') = 'N' THEN
       IF (g_debug <= gme_debug.g_log_statement) THEN
          FND_FILE.Put_Line(FND_FILE.LOG,g_pkg_name||'.'||l_api_name||' No need for Availabilty check create for full order qty');                
       END IF;   
     /* Create a batch for the full order line quantity regardless of onhand availability
     - Relieve any reservations against inventory; must not duplicate demand  
     ==================================================================================*/
     l_batch_qty  := l_so_line_rec.ordered_quantity;
     l_batch_qty2 := l_so_line_rec.ordered_quantity2;
     IF (g_debug <= gme_debug.g_log_statement) THEN
        FND_FILE.Put_Line(FND_FILE.LOG,g_pkg_name||'.'||l_api_name||' Batch quantity needed is '||l_batch_qty);                                 
        FND_FILE.Put_Line(FND_FILE.LOG,g_pkg_name||'.'||l_api_name||' Secondary qty  needed is '||l_batch_qty2);                                 
        FND_FILE.Put_Line(FND_FILE.LOG,g_pkg_name||'.'||l_api_name||' Retrieve any existing inv reservations ');                                      
     END IF;   
     OPEN cur_inv_reservations (l_so_line_id);
        
     FETCH cur_inv_reservations
     BULK COLLECT INTO l_rsv_tbl;
     CLOSE cur_inv_reservations;

     l_count := 1;
     IF (g_debug <= gme_debug.g_log_statement) THEN
        FND_FILE.Put_Line(FND_FILE.LOG,g_pkg_name||'.'||l_api_name||' Loop thru existing reservations for'||l_rsv_tbl.COUNT);                             
     END IF;   
     WHILE l_count <= l_rsv_tbl.COUNT LOOP
       l_rsv_row_rec := l_rsv_tbl(l_count);
       /* Relieve any reservations against INV */
       IF (g_debug <= gme_debug.g_log_statement) THEN
          FND_FILE.Put_Line(FND_FILE.LOG,g_pkg_name||'.'||l_api_name||' Relieve Reservation here for ID '||l_rsv_row_rec.reservation_id);
       END IF;   
       gme_reservations_pvt.relieve_reservation
            (p_reservation_id        => l_rsv_row_rec.reservation_id
            ,p_prim_relieve_qty      => l_rsv_row_rec.primary_reservation_quantity
            ,x_return_status         => x_return_status);
      IF (g_debug <= gme_debug.g_log_statement) THEN
         FND_FILE.Put_Line(FND_FILE.LOG, 
                     g_pkg_name
                  || '.'
                  || l_api_name
                  || ' Return status from relieve_reservation is '
                  || x_return_status);
       END IF;
       IF x_return_status <> fnd_api.g_ret_sts_success THEN
         RAISE FND_API.G_EXC_ERROR;
       END IF;
       l_count := l_count + 1;
     END LOOP;                                       
   END IF;                 

   /* Deal with cases where check on availability is required
   ========================================================= */
  IF NVL(l_mto_rules_rec.check_availability,'N') = 'Y' THEN
       IF (g_debug <= gme_debug.g_log_statement) THEN
         FND_FILE.Put_Line(FND_FILE.LOG,g_pkg_name||'.'||l_api_name||' NEED to check Inventory Availabilty ahead of creating batch');                
         FND_FILE.Put_Line(FND_FILE.LOG,g_pkg_name||'.'||l_api_name||' start point is the order qty ' ||l_so_line_rec.ordered_quantity);             
       END IF;  
    /* Find out how much quantity needs to be manufactured to meet inventory shortfall */
    /* step 1 - what is the current unreserved quantity    
    =======================================================*/
    l_batch_qty  := l_so_line_rec.ordered_quantity;
    l_batch_qty2 := l_so_line_rec.ordered_quantity2;
    
    -- Bug 18329535
    -- Use uom codes from item instead of reservation. 
    l_primary_uom_code   :=  l_item_rec.primary_uom_code;
    l_secondary_uom_code  :=  l_item_rec.secondary_uom_code;
       
   begin
    SELECT   NVL(sum(mr.primary_reservation_quantity),0)
      into l_total_primary_rsv_qty
    FROM mtl_reservations mr
      WHERE demand_source_type_id IN (INV_RESERVATION_GLOBAL.g_source_type_oe , INV_RESERVATION_GLOBAL.g_source_type_internal_ord) /* Bug 24949569  */
        AND  supply_source_type_id = INV_RESERVATION_GLOBAL.g_source_type_inv
        AND demand_source_line_id = l_so_line_rec.line_id;
        
     IF (g_debug <= gme_debug.g_log_statement) THEN   
        FND_FILE.Put_Line(FND_FILE.LOG,'INV_RESERVATION_GLOBAL.g_source_type_oe ' || INV_RESERVATION_GLOBAL.g_source_type_oe);
        FND_FILE.Put_Line(FND_FILE.LOG,'INV_RESERVATION_GLOBAL.g_source_type_inv ' || INV_RESERVATION_GLOBAL.g_source_type_inv);
        FND_FILE.Put_Line(FND_FILE.LOG,'l_so_line_rec.line_id ' || l_so_line_rec.line_id);     
     END IF;   
    -- Bug 18329535   
    IF ( l_total_primary_rsv_qty > 0 ) THEN   
    /* Deduct the reserved quantity from the ordered_quantity
    *     ========================================================= */
      IF l_so_line_rec.order_quantity_uom = l_primary_uom_code THEN
         l_batch_qty  := l_so_line_rec.ordered_quantity - l_total_primary_rsv_qty;
         IF (g_debug <= gme_debug.g_log_statement) THEN
          FND_FILE.Put_Line(FND_FILE.LOG,g_pkg_name||'.'||l_api_name||' 1...l_batch_qty is ' || l_batch_qty);
         END IF; 
      ELSE
      /* Align the UOM to match the order line */
         l_res_qty_so_um :=
            inv_convert.inv_um_convert(l_so_line_rec.inventory_item_id,
	                              null,
                                      l_so_line_rec.ship_from_org_id,
	                              5,
	                              l_total_primary_rsv_qty,
	                              l_primary_uom_code,
   	                              l_so_line_rec.order_quantity_uom,
	                              null,
                                    null);
         l_batch_qty  := l_so_line_rec.ordered_quantity - l_res_qty_so_um;
         IF (g_debug <= gme_debug.g_log_statement) THEN
            FND_FILE.Put_Line(FND_FILE.LOG,g_pkg_name||'.'||l_api_name||' 2... l_batch_qty is ' || l_batch_qty);         
         END IF;
       END IF;
    END IF;   
     exception when no_data_found then       
       null;
   end;
    IF (g_debug <= gme_debug.g_log_statement) THEN
       FND_FILE.Put_Line(FND_FILE.LOG,g_pkg_name||'.'||l_api_name||' Deduct qty already reserved, this is '||l_total_primary_rsv_qty);             
       FND_FILE.Put_Line(FND_FILE.LOG,g_pkg_name||'.'||l_api_name||' Now batch qty required stands at '||l_batch_qty);             
    END IF;   

    IF (l_batch_qty <=  0) THEN
         FND_FILE.Put_Line(FND_FILE.LOG,g_pkg_name||'.'||l_api_name||' Full Order Qty already reserved to INV');             
         FND_FILE.Put_Line(FND_FILE.LOG,g_pkg_name||'.'||l_api_name||' No need for batch creation Return here');             
       RETURN;
    END IF;
     
     
    /* step 2 - Assess the available to reserve.  Create batch for any INV defecit
    ==============================================================================*/
    IF l_item_rec.lot_control_code > 1 THEN
      l_is_lot_control := TRUE;
    END IF; 

    --  set controls for locator and serial
    IF (g_debug <= gme_debug.g_log_statement) THEN
       FND_FILE.Put_Line(FND_FILE.LOG,g_pkg_name||'.'||l_api_name||' Need to assess ATR level so invoke create tree');
    END IF;   

    inv_quantity_tree_pvt.create_tree
         (
           p_api_version_number         => 1.0
         , p_init_msg_lst               => fnd_api.g_true
	 , x_return_status              => l_return_status
	 , x_msg_count                  => x_msg_count
	 , x_msg_data                   => x_msg_data
	 , p_organization_id            => l_so_line_rec.ship_from_org_id                
	 , p_inventory_item_id          => l_so_line_rec.inventory_item_id
	 , p_tree_mode                  => inv_quantity_tree_pvt.g_reservation_mode
	 , p_is_revision_control        => l_is_revision_control
	 , p_is_lot_control             => l_is_lot_control
	 , p_is_serial_control          => l_is_serial_control
	 , p_asset_sub_only             => FALSE
	 , p_include_suggestion         => TRUE
	 , p_demand_source_type_id      => 
l_so_line_rec.source_document_type_id /* Bug 24949569 */
	 , p_demand_source_header_id    => l_so_line_rec.header_id
	 , p_demand_source_line_id      => l_so_line_rec.line_id
	 , p_demand_source_name         => NULL                              
	 , p_demand_source_delivery     => NULL                                          
    , p_lot_expiration_date        => SYSDATE                    
	 , x_tree_id                    => l_tree_id
	 );
    IF (g_debug <= gme_debug.g_log_statement) THEN	      
       FND_FILE.Put_Line(FND_FILE.LOG,g_pkg_name||'.'||l_api_name||' After create tree status returned is '||l_return_status);
    END IF;   
	      
    IF l_return_status = fnd_api.g_ret_sts_error THEN
      RAISE fnd_api.g_exc_error;
    END IF;
	      
    IF l_return_status = fnd_api.g_ret_sts_unexp_error THEN
      RAISE fnd_api.g_exc_unexpected_error;
    END IF;
    IF (g_debug <= gme_debug.g_log_statement) THEN
       FND_FILE.Put_Line(FND_FILE.LOG,g_pkg_name||'.'||l_api_name||' Query tree for ATR next ');
    END IF;   
    inv_quantity_tree_pvt.query_tree(
           p_api_version_number         => 1.0
         , p_init_msg_lst               => fnd_api.g_true
         , x_return_status              => l_return_status
         , x_msg_count                  => l_msg_count
         , x_msg_data                   => l_msg_data
         , p_tree_id                    => l_tree_id
         , p_revision                   => NULL       
         , p_lot_number                 => NULL                   
         , p_subinventory_code          => NULL                         
         , p_locator_id                 => NULL                    
         , x_qoh                        => l_qoh
         , x_rqoh                       => l_rqoh
         , x_qr                         => l_qr
         , x_qs                         => l_qs
         , x_att                        => l_att
         , x_atr                        => l_atr
         , x_sqoh                       => l_sqoh
         , x_srqoh                      => l_srqoh
         , x_sqr                        => l_sqr
         , x_sqs                        => l_sqs
         , x_satt                       => l_satt
         , x_satr                       => l_satr
         , p_lpn_id                     => l_lpn_id
         );
    IF (g_debug <= gme_debug.g_log_statement) THEN
       FND_FILE.Put_Line(FND_FILE.LOG,g_pkg_name||'.'||l_api_name||' After Query tree return status is '||l_return_status);
    END IF;   

    IF l_return_status = fnd_api.g_ret_sts_error THEN
       RAISE fnd_api.g_exc_error;
    END IF;

    IF l_return_status = fnd_api.g_ret_sts_unexp_error THEN
       RAISE fnd_api.g_exc_unexpected_error;
    END IF;
    IF (g_debug <= gme_debug.g_log_statement) THEN   
       FND_FILE.Put_Line(FND_FILE.LOG,'l_atr ' || l_atr);
       FND_FILE.Put_Line(FND_FILE.LOG,'l_att ' || l_att);
       FND_FILE.Put_Line(FND_FILE.LOG,'l_qoh ' || l_qoh);
       FND_FILE.Put_Line(FND_FILE.LOG,'l_rqoh ' || l_rqoh);  
       FND_FILE.Put_Line(FND_FILE.LOG,'l_qr ' || l_qr);
       FND_FILE.Put_Line(FND_FILE.LOG,'l_qs ' || l_qs);
       FND_FILE.Put_Line(FND_FILE.LOG,'l_satr ' || l_satr);
       FND_FILE.Put_Line(FND_FILE.LOG,'l_satt ' || l_satt);
       FND_FILE.Put_Line(FND_FILE.LOG,'l_sqoh ' || l_sqoh);
       FND_FILE.Put_Line(FND_FILE.LOG,'l_srqoh ' || l_srqoh);
       FND_FILE.Put_Line(FND_FILE.LOG,'l_sqr ' || l_sqr);
       FND_FILE.Put_Line(FND_FILE.LOG,'l_sqs ' || l_sqs);
       FND_FILE.Put_Line(FND_FILE.LOG,'l_primary_uom_code ' || l_primary_uom_code);
       FND_FILE.Put_Line(FND_FILE.LOG,'l_secondary_uom_code ' || l_secondary_uom_code);
    END IF;
    -- Bug 18329535
    -- Modifed the code so that uom conversion is not called when primary and secondary uom codes are same. 
    IF l_so_line_rec.order_quantity_uom = l_primary_uom_code THEN 
       l_batch_qty  := l_batch_qty - l_atr;
    ELSIF l_so_line_rec.order_quantity_uom = l_secondary_uom_code THEN           
       l_batch_qty  := l_batch_qty - l_satr;          
    /* Deduct any ATR from the manufacture qty */
    /* Convert atr quantity into Orderline UOM  */
    ELSIF l_so_line_rec.order_quantity_uom <> l_primary_uom_code THEN               
      l_atr_qty_in_so_um := inv_convert.inv_um_convert(
                                                  item_id                       => l_so_line_rec.inventory_item_id
                                                  , ORGANIZATION_ID             => l_so_line_rec.ship_from_org_id
                                                  , LOT_NUMBER                  => null
                                                  , PRECISION                   => 5
                                                  , from_quantity               => l_atr
                                                  , from_unit                   => l_primary_uom_code
                                                  , to_unit                     => l_so_line_rec.order_quantity_uom
                                                  , from_name                   => NULL
                                                  , to_name                     => NULL
                                                  );    
      l_batch_qty  := l_batch_qty - l_atr_qty_in_so_um;
    END IF;   
  END IF;   -- End of check availability
  IF (g_debug <= gme_debug.g_log_statement) THEN
    FND_FILE.Put_Line(FND_FILE.LOG, 'END of section for MUST check availability ');                                  
    FND_FILE.Put_Line(FND_FILE.LOG, 'Batch qty now stands at '||l_batch_qty    );                                   
  END IF;  
  
  /* Now set up data and invoke create_batch  
  ==========================================*/
  IF (l_batch_qty <=  0) THEN
    FND_FILE.Put_Line(FND_FILE.LOG,g_pkg_name||'.'||l_api_name||' Entire requested qty aviaable in inventory so Return now');
    RETURN;
  END IF;

  IF(l_mto_rules_rec. firmed_ind = 'Y') THEN
     l_firmed_ind := 1;
  ELSE
     l_firmed_ind := NULL;
  END IF;

  /* Now populate the batch record so you can call Create Batch API */
  IF (g_debug <= gme_debug.g_log_statement) THEN
     FND_FILE.Put_Line(FND_FILE.LOG,'populate the batch record in readiness for create_batch API');
  END IF;   
  l_batch_header_rec.batch_type := l_mto_rules_rec.batch_type_to_create;
  l_batch_header_rec.organization_id := l_so_line_rec.ship_from_org_id;
  
  -- Bug 12664771 - Do not initialize plan start date.
  -- l_batch_header_rec.plan_start_date := SYSDATE;

  -- Bug 19228370 - Rework of 12664771 - Initialize plan start date if there is no schedule_ship_date.
  IF l_so_line_rec.schedule_ship_date IS NULL THEN
     l_batch_header_rec.plan_start_date := SYSDATE;
  END IF;
  
  l_batch_header_rec.update_inventory_ind := 'Y';
  l_batch_header_rec.due_date   := l_so_line_rec.request_date; 
  l_batch_header_rec.FIRMED_IND := l_firmed_ind;
  l_batch_header_rec.plan_cmplt_date := l_so_line_rec.schedule_ship_date;
  /* Batch Creation user. If diff. from the sales order user then set the user context to this one */
  
  -- Bug 9355688 - Initialize with proper values not hard coded -1.
  -- l_user_id := FND_GLOBAL.user_id;
  -- FND_GLOBAL.apps_initialize(l_user_id,-1,-1);   
  IF (g_debug <= gme_debug.g_log_statement) THEN 
     FND_FILE.Put_Line(FND_FILE.LOG,'before apps init FND_GLOBAL.USER_ID '||FND_GLOBAL.USER_ID);
     FND_FILE.Put_Line(FND_FILE.LOG,'before apps init FND_GLOBAL.RESP_ID '||FND_GLOBAL.RESP_ID);
     FND_FILE.Put_Line(FND_FILE.LOG,'before apps init FND_GLOBAL.RESP_APPL_ID '||FND_GLOBAL.RESP_APPL_ID);
  END IF;
  l_user_id := FND_GLOBAL.USER_ID;
  l_resp_id := FND_GLOBAL.RESP_ID;
  l_resp_appl_id := FND_GLOBAL.RESP_APPL_ID;
  
  FND_GLOBAL.apps_initialize(user_id => l_user_id,
                             resp_id => l_resp_id,
                             resp_appl_id => l_resp_appl_id);
  IF (g_debug <= gme_debug.g_log_statement) THEN                           
     FND_FILE.Put_Line(FND_FILE.LOG,'after apps init FND_GLOBAL.USER_ID '||FND_GLOBAL.USER_ID);
     FND_FILE.Put_Line(FND_FILE.LOG,'after apps init FND_GLOBAL.RESP_ID '||FND_GLOBAL.RESP_ID);
     FND_FILE.Put_Line(FND_FILE.LOG,'after apps init FND_GLOBAL.RESP_APPL_ID '||FND_GLOBAL.RESP_APPL_ID);
  END IF;
  IF l_mto_rules_rec.batch_creation_user is not NULL THEN
    IF (l_mto_rules_rec.batch_creation_user <> l_user_id) THEN
      FND_FILE.Put_Line(FND_FILE.LOG,g_pkg_name||'.'||l_api_name||' Batch creation user is '||l_mto_rules_rec.batch_creation_user);
      -- Bug 9355688 - Initialize with proper values not hard coded -1.
      -- FND_GLOBAL.apps_initialize(l_mto_rules_rec.batch_creation_user,-1,-1); 
      FND_GLOBAL.apps_initialize(user_id => l_mto_rules_rec.batch_creation_user,
                                 resp_id => l_resp_id,
                                 resp_appl_id => l_resp_appl_id);
    END IF;
  END IF;
  
     FND_FILE.Put_Line(FND_FILE.LOG,'Calling Create Batch API'); 
     FND_FILE.Put_Line(FND_FILE.LOG, 'Batch/FPO qty needed is '||l_batch_qty);

  GME_API_PUB.create_batch(
                          p_api_version                       => 2.0,
                          p_validation_level                  => 100,
                          p_init_msg_list                     => fnd_api.g_true,
                          p_commit                            => fnd_api.g_false,
                          x_message_count                     => l_msg_count,             
                          x_message_list                      => l_msg_data,       
                          x_return_status                     => l_return_status,
                          p_org_code                          => NULL, 
                          p_batch_header_rec                  => l_batch_header_rec,
                          x_batch_header_rec                  => l_out_batch_header_rec,
                          p_batch_size                        => l_batch_qty,                           
                          p_batch_size_uom                    => l_so_line_rec.order_quantity_uom,       
                          p_creation_mode                     => 'PRODUCT',
                          p_recipe_id                         => NULL,
                          p_recipe_no                         => NULL,          
                          p_recipe_version                    => NULL,
                          p_product_no                        => NULL,
                          p_item_revision                     => NULL,
                          p_product_id                        => l_so_line_rec.inventory_item_id,        
                          p_ignore_qty_below_cap              => fnd_api.g_true,
                          p_use_workday_cal                   => NULL,
                          p_contiguity_override               => NULL,
                          p_use_least_cost_validity_rule      => fnd_api.g_false,
                          x_exception_material_tbl            => l_exception_material_tbl
                         );
 
 
  FND_FILE.Put_Line(FND_FILE.LOG,' After calling GME_API_PUB.create_batch for order  '||l_order_no || 'and line_no ' || l_so_line_no); -- Bug 13359707 
  FND_FILE.Put_Line(FND_FILE.LOG,' Return Status from Create_Batch API is '||l_return_status); -- Bug 13359707      
  FND_FILE.Put_Line(FND_FILE.LOG,' Message from Create_Batch API is '||l_msg_data); -- Bug 13359707 
  FND_FILE.Put_Line(FND_FILE.LOG,g_pkg_name||'.'||l_api_name||' Batch No from Create_Batch API is '|| l_out_batch_header_rec.batch_no);     
 
   /*Bug#8367071 Added the inventory shortage condition here so that the
 * code proceeds thorugh the resevation etc even when there is material shortage
 * in the batch */
  IF( l_return_status <> FND_API.G_RET_STS_SUCCESS AND l_return_status <> 'V' ) THEN
     j := to_number(NVL(l_msg_count,0));
     FOR i in 1..j
     LOOP

        l_message := fnd_msg_pub.get(p_encoded => FND_API.G_FALSE);
        l_message := replace(l_message, chr(0), ' ');
        FND_FILE.Put_Line(FND_FILE.LOG,l_message);
     END LOOP;
     RAISE fnd_api.g_exc_error;
  END IF;

  /* Now retrieve the batch product line - sales order line must reserve to this supply
  ===================================================================================*/
  IF (g_debug <= gme_debug.g_log_statement) THEN
     FND_FILE.Put_Line(FND_FILE.LOG,g_pkg_name||'.'||l_api_name||' Retrieve product line - must reserve to this supply');
  END IF;
  
  OPEN  get_batch_line(l_out_batch_header_rec.batch_id,l_so_line_rec.inventory_item_id);
  FETCH get_batch_line INTO l_material_detail_rec;

  IF(get_batch_line%NOTFOUND) THEN
     CLOSE get_batch_line;
     FND_FILE.Put_Line(FND_FILE.LOG,'get_batch_line%NOTFOUND, returning from create_batch_for_order_line');
     RAISE fnd_api.g_exc_error;
  END IF;
  CLOSE get_batch_line;
      
  /* Copy Attachments if the rule says so
  ===================================== */
  IF (g_debug <= gme_debug.g_log_statement) THEN
     FND_FILE.Put_Line(FND_FILE.LOG,g_pkg_name||'.'||l_api_name||' analyse copy attachments '||l_mto_rules_rec.copy_attachments);
  END IF;   

  IF (NVL(l_mto_rules_rec.copy_attachments,'N') = 'Y') THEN
    IF (g_debug <= gme_debug.g_log_statement) THEN
       FND_FILE.Put_Line(FND_FILE.LOG,g_pkg_name||'.'||l_api_name||' Invoke copy attachments ');
    END IF;   
    copy_attachments( p_so_line_id => l_so_line_id
			,  p_so_category_id => l_mto_rules_rec.sales_order_attachment
			,  p_batch_id => l_out_batch_header_rec.batch_id
			,  p_batch_category_id => l_mto_rules_rec.batch_attachment
			,  x_return_status => l_return_status); 
    IF (g_debug <= gme_debug.g_log_statement) THEN
       FND_FILE.Put_Line(FND_FILE.LOG,g_pkg_name||'.'||l_api_name||' Return Status from copy attachments '||l_return_status);
    END IF;   
  END IF;     

  /* Need to set up pending lots if the product is lot controlled and the rule requires lot generation
  =======================================================================================================*/
  IF (g_debug <= gme_debug.g_log_statement) THEN
     FND_FILE.Put_Line(FND_FILE.LOG,g_pkg_name||'.'||l_api_name||' analyse auto lot generation'||l_mto_rules_rec.auto_lot_generation);
  END IF;   
  IF l_item_rec.SECONDARY_UOM_CODE is not null then
  l_batch_qty2 := inv_convert.inv_um_convert(l_so_line_rec.inventory_item_id,
	                              null,
                                    l_so_line_rec.ship_from_org_id,
	                              5,
	                              l_batch_qty,
	                              l_so_line_rec.order_quantity_uom,
   	                              l_item_rec.SECONDARY_UOM_CODE,  
	                              null,
                                    null);
  END IF;

  IF l_item_rec.lot_control_code > 1 AND (l_mto_rules_rec.auto_lot_generation = 'Y') THEN
    l_pending_product_lots_rec.material_detail_id := l_material_detail_rec.material_detail_id;
    l_pending_product_lots_rec.sequence           := null;              
    l_pending_product_lots_rec.revision           := l_material_detail_rec.revision;
    l_pending_product_lots_rec.lot_number         := null;                    
    l_pending_product_lots_rec.quantity           := l_batch_qty;
    l_pending_product_lots_rec.secondary_quantity := l_batch_qty2;
      IF (g_debug <= gme_debug.g_log_statement) THEN
         FND_FILE.Put_Line(FND_FILE.LOG,g_pkg_name||'.'||l_api_name||
                      ' Invoke create_pending_product_lot for line_id '||l_material_detail_rec.material_detail_id);
      END IF;                
    GME_API_PUB.create_pending_product_lot
     (p_api_version  	 => 2.0
     ,p_validation_level => gme_common_pvt.g_max_errors
     ,p_init_msg_list    => fnd_api.g_false
     ,p_commit           => fnd_api.g_false
     ,x_message_count    => l_msg_count
     ,x_message_list     => l_msg_data 
     ,x_return_status    => l_return_status
     ,p_batch_header_rec => l_out_batch_header_rec
     ,p_org_code         => NULL        
     ,p_create_lot       => fnd_api.g_true
     ,p_generate_lot     => fnd_api.g_true
     ,p_material_detail_rec => l_material_detail_rec
     ,p_pending_product_lots_rec => l_pending_product_lots_rec
     ,x_pending_product_lots_rec => l_out_pending_product_lots_rec);

    IF (l_return_status <> FND_API.G_RET_STS_SUCCESS) THEN
    
    FND_FILE.Put_Line(FND_FILE.LOG,' After calling GME_API_PUB.create_pending_product_lot for  '||l_order_no || 'and line_no ' || l_so_line_no); -- Bug 13359707 
    FND_FILE.Put_Line(FND_FILE.LOG,' Return Status from create_pending_product_lot is '||l_return_status); -- Bug 13359707      
    FND_FILE.Put_Line(FND_FILE.LOG,' FAILURE to generate pending lot'); -- Bug 13359707 
      
    END IF;
  END IF;  -- End of pending lot generation                                               
     
  FND_FILE.Put_Line(FND_FILE.LOG,'Reserve the batch Quantity to this order line');
  /* Establish a reservation between the order line (demand) and batch product (supply)                        
  ====================================================================================*/
  IF (g_debug <= gme_debug.g_log_statement) THEN
     FND_FILE.Put_Line(FND_FILE.LOG, ' start of Reservation Processing         ');
  END IF;   
  l_rsv_rec:= NULL;
  l_rsv_rec.requirement_date := l_so_line_rec.request_date;
  l_rsv_rec.organization_id := l_so_line_rec.ship_from_org_id;
  l_rsv_rec.inventory_item_id := l_so_line_rec.inventory_item_id;
  
  /* Bug 19624259  */
  IF (l_so_line_rec.source_document_type_id = 10) THEN
      IF (g_debug <= gme_debug.g_log_statement) THEN
         FND_FILE.Put_Line(FND_FILE.LOG,g_pkg_name||'.'||l_api_name||' Internal sales order line');
      END IF;   
      l_rsv_rec.demand_source_type_id := 8; 
  ELSE
      IF (g_debug <= gme_debug.g_log_statement) THEN
         FND_FILE.Put_Line(FND_FILE.LOG,g_pkg_name||'.'||l_api_name||' External sales order line');
      END IF;
      l_rsv_rec.demand_source_type_id := 2;                                                  
  END IF;    
  
  l_rsv_rec.demand_source_header_id := INV_SALESORDER.get_salesorder_for_oeheader(l_so_line_rec.header_id);
  FND_FILE.Put_Line(FND_FILE.LOG, 'l_rsv_rec.demand_source_header_id:' || l_rsv_rec.demand_source_header_id);
  l_rsv_rec.demand_source_line_id := l_so_line_rec.line_id;                        
  l_rsv_rec.reservation_uom_code := l_so_line_rec.order_quantity_uom;
  l_rsv_rec.reservation_quantity := l_batch_qty;
  l_rsv_rec.secondary_reservation_quantity := l_batch_qty2;  
  l_rsv_rec.revision := l_material_detail_rec.revision;   
  l_rsv_rec.subinventory_code := NULL;                   
  l_rsv_rec.locator_id := NULL;              
  l_rsv_rec.lot_number := NULL;                   
  l_rsv_rec.lpn_id := NULL;
  l_rsv_rec.demand_source_name := NULL;
  l_rsv_rec.demand_source_delivery := NULL;
  l_rsv_rec.primary_uom_code := NULL;
  l_rsv_rec.primary_uom_id := NULL;
  l_rsv_rec.secondary_uom_code := l_so_line_rec.ordered_quantity_uom2;
  l_rsv_rec.secondary_uom_id := NULL;
  l_rsv_rec.reservation_uom_id := NULL;
  l_rsv_rec.ship_ready_flag := NULL;
  l_rsv_rec.detailed_quantity := NULL;
  l_rsv_rec.secondary_detailed_quantity := NULL;
  l_rsv_rec.supply_source_type_id := inv_reservation_global.g_source_type_wip;
  l_rsv_rec.supply_source_header_id := l_out_batch_header_rec.batch_id;
  l_rsv_rec.supply_source_line_id := l_material_detail_rec.material_detail_id;
  
   IF (g_debug <= gme_debug.g_log_statement) THEN
     FND_FILE.Put_Line(FND_FILE.LOG,g_pkg_name||'.'||l_api_name||' Invoke INV_RESERVATION_PUB.Create_Reservation ');
   END IF;   

  inv_reservation_pub.create_reservation
                                (p_api_version_number            => 1.0
                                ,p_init_msg_lst                  => fnd_api.g_false
                                ,x_return_status                 => l_return_status
                                ,x_msg_count                     => l_msg_count
                                ,x_msg_data                      => l_msg_data
                                ,p_rsv_rec                       => l_rsv_rec
                                ,p_serial_number                 => l_in_serial_num
                                ,x_serial_number                 => l_out_serial_num
                                ,p_partial_reservation_flag      => fnd_api.g_true
                                ,p_validation_flag               => fnd_api.g_true
                                ,x_quantity_reserved             => l_reserved_qty
                                ,x_reservation_id                => l_reservation_id
                                ,p_partial_rsv_exists            => TRUE);
    IF (g_debug <= gme_debug.g_log_statement) THEN
       FND_FILE.Put_Line(FND_FILE.LOG,' inv_reservation_pub.create_reservation for  '||l_order_no || 'and line_no ' || l_so_line_no); -- Bug 13359707 
       FND_FILE.Put_Line(FND_FILE.LOG,' Return Status from inv_reservation_pub.create_reservation is '||l_return_status); -- Bug 13359707          
  
       FND_FILE.Put_Line(FND_FILE.LOG,   g_pkg_name
                           || '.'
                           || l_api_name
                           || ' inv_reservation_pub.create_reservation returns status of '
                           || l_return_status                        
                           || ' for material_detail_id '
                           || l_material_detail_rec.material_detail_id 
                           || ' qty reserved IS  '
                           || l_reserved_qty );                      
    END IF;
  IF (l_return_status IN
                     (fnd_api.g_ret_sts_error, fnd_api.g_ret_sts_unexp_error) ) THEN
     RAISE FND_API.G_EXC_ERROR;
  END IF;

  /* Notify the customer sales representative of the batch creation                                            
  ===============================================================*/
  IF (g_debug <= gme_debug.g_log_statement) THEN
     FND_FILE.Put_Line(FND_FILE.LOG,g_pkg_name||'.'||l_api_name||' Notify_CSR for batch_id'||l_rsv_rec.supply_source_header_id);
     FND_FILE.Put_Line(FND_FILE.LOG,g_pkg_name||'.'||l_api_name||' Notify_CSR for batch_line_id'||l_rsv_rec.supply_source_line_id);
     FND_FILE.Put_Line(FND_FILE.LOG,g_pkg_name||'.'||l_api_name||' Notify_CSR for sales_line_id'||l_rsv_rec.demand_source_line_id);
  END IF;   
--FND_GLOBAL.apps_initialize(l_user_id,-1,-1);
  GME_SUPPLY_RES_PVT.notify_CSR
              ( P_Batch_id               =>    l_rsv_rec.supply_source_header_id
              , P_Batch_line_id          =>    l_rsv_rec.supply_source_line_id
              , P_So_line_id             =>    l_rsv_rec.demand_source_line_id
              , P_batch_trans_id         =>    NULL                   
              , P_organization_id        =>    l_rsv_rec.organization_id
              , P_action_code            =>    'NEW_BATCH_CREATED'                                     
              , X_return_status          =>    l_return_status
              , X_msg_cont               =>    l_msg_count      
              , X_msg_data               =>    l_msg_data );            

  IF (g_debug <= gme_debug.g_log_statement) THEN
     FND_FILE.Put_Line(FND_FILE.LOG,' Return status from  GME_SUPPLY_RES_PVT.notify_CSR is '||l_return_status); -- Bug 13359707    
  END IF;
  IF l_return_status IN  (fnd_api.g_ret_sts_error, fnd_api.g_ret_sts_unexp_error) THEN
    FND_FILE.Put_Line(FND_FILE.LOG,g_pkg_name||'.'||l_api_name||' Exit after Notifications Failure   ');
    RAISE FND_API.G_EXC_ERROR;
  END IF;

  /* Set the user context back to the original user */
  IF (l_mto_rules_rec.batch_creation_user <> l_user_id) THEN
    -- Bug 9355688 - Initialize with proper values not hard coded -1.
    -- FND_GLOBAL.apps_initialize(l_user_id,-1,-1);
    FND_GLOBAL.apps_initialize(user_id => l_user_id,
                               resp_id => l_resp_id,
                               resp_appl_id => l_resp_appl_id);
  END IF;

   /* Bug 13359707 - Moved earlier in the code */
  /* Get the Order and Line Information
  OPEN Get_order_info(p_so_line_id);
  FETCH Get_order_info INTO l_so_line_no,l_item_no,
        l_order_no;
  IF(Get_order_info%NOTFOUND) THEN     
     FND_FILE.Put_Line(FND_FILE.LOG,g_pkg_name||'.'||l_api_name ||'Get_order_info%NOTFOUND');     
  END IF;
  CLOSE Get_order_info; */
  FND_FILE.Put_Line(FND_FILE.LOG, '***************************************************************************************** ');
  -- Bug 12400843 
  IF (l_batch_header_rec.batch_type = 10) THEN
  	FND_FILE.Put_Line(FND_FILE.LOG, 'FPO '||l_out_batch_header_rec.batch_no ||' created for order ' ||
    	l_order_no ||' line number '||l_so_line_no ||
    	' item is '|| l_item_no  ||' reservation id is '||l_reservation_id);                      
  ELSE
  	FND_FILE.Put_Line(FND_FILE.LOG, 'Batch '||l_out_batch_header_rec.batch_no ||' created for order ' ||
    	l_order_no ||' line number '||l_so_line_no ||
    	' item is '|| l_item_no  ||' reservation id is '||l_reservation_id);                      
  END IF;  
  FND_FILE.Put_Line(FND_FILE.LOG,'Make to Order implemented for rule  '||l_mto_rules_rec.rule_name);
  FND_FILE.Put_Line(FND_FILE.LOG, '***************************************************************************************** ');
 
/* EXCEPTION HANDLING
====================*/
EXCEPTION
    WHEN FND_API.G_EXC_ERROR THEN
    --  ROLLBACK TO SAVEPOINT create_batch_for_so_line_SP;
    FND_FILE.Put_Line(FND_FILE.LOG,'sqlcode : ' ||to_char(sqlcode));
    FND_FILE.Put_Line(FND_FILE.LOG,'sqlerr : '|| SUBSTRB(SQLERRM, 1, 150));
    x_return_status := FND_API.G_RET_STS_ERROR;
--  errbuf := SUBSTRB(SQLERRM, 1, 150);
--  retcode := x_return_status;

      FND_MSG_PUB.Add_Exc_Msg (  g_pkg_name
                               , l_api_name
                              );
      FND_MSG_PUB.Count_AND_GET (  p_encoded => FND_API.G_FALSE
                                 , p_count => x_msg_count
                                 , p_data  => x_msg_data
                                );

    WHEN FND_API.G_EXC_UNEXPECTED_ERROR THEN
     -- ROLLBACK TO SAVEPOINT create_batch_for_so_line_SP;
 FND_FILE.Put_Line(FND_FILE.LOG,'sqlcode : ' ||to_char(sqlcode));
    FND_FILE.Put_Line(FND_FILE.LOG,'sqlerr : '|| SUBSTRB(SQLERRM, 1, 150));

      x_return_status := FND_API.G_RET_STS_UNEXP_ERROR;
 --   errbuf := SUBSTRB(SQLERRM, 1, 150);
 --   retcode := x_return_status;


        FND_MSG_PUB.Add_Exc_Msg (  g_pkg_name
                               , l_api_name
                              );
         FND_MSG_PUB.Count_AND_GET (  p_encoded=> FND_API.G_FALSE
                                 , p_count => x_msg_count
                                 , p_data  => x_msg_data
                                );
    WHEN OTHERS THEN
      --ROLLBACK TO SAVEPOINT create_batch_for_so_line_SP;
      FND_FILE.Put_Line(FND_FILE.LOG,'sqlcode : ' ||to_char(sqlcode));
      FND_FILE.Put_Line(FND_FILE.LOG,'sqlerr : '|| SUBSTRB(SQLERRM, 1, 150));
      
       FND_MSG_PUB.Add_Exc_Msg (  g_pkg_name
                               , l_api_name
                              );

       x_return_status := FND_API.G_RET_STS_UNEXP_ERROR;
--     errbuf := SUBSTRB(SQLERRM, 1, 150);
--     retcode := x_return_status;

      /*   Get message count and data */
      FND_MSG_PUB.count_and_get
       (   p_count  => x_msg_count
         , p_data  => x_msg_data
       );

END create_batch_for_order_line;


PROCEDURE Copy_attachments ( p_so_category_id IN NUMBER
			   , p_so_line_id     IN NUMBER
			   , p_batch_category_id IN NUMBER
			   , p_batch_id	      IN NUMBER
			   , x_return_status OUT NOCOPY VARCHAR2) IS

CURSOR get_attachments(p_so_category_id IN NUMBER
		       , p_so_line_id IN NUMBER) IS
 SELECT *
   FROM fnd_attached_docs_form_VL
  WHERE function_name = decode(0,1,NULL,'OEXOEORD')
    AND function_type = decode(0,1,NULL,'O')
    AND (security_type = 4 OR publish_flag = 'Y')
    AND category_id = p_so_category_id
    AND ((entity_name   = 'OE_ORDER_LINES') AND (pk1_value = p_so_line_id ));


BEGIN
   IF (g_debug <= gme_debug.g_log_statement) THEN
     FND_FILE.Put_Line(FND_FILE.LOG,'Entering copy_attachemnts routine..');
   END IF;  
     FOR so_attchments_row IN get_attachments(p_so_category_id,p_so_line_id) LOOP

         so_attchments_row.row_id := NULL;
         so_attchments_row.document_id := NULL;
         so_attchments_row.attached_document_id :=  so_attchments_row.ATTACHED_DOCUMENT_ID * -1;
      
         FND_ATTACHED_DOCUMENTS_PKG.INSERT_ROW (
 		 X_Rowid                      => so_attchments_row.row_id 
 		,X_attached_document_id       => so_attchments_row.attached_document_id
 		,X_document_id                => so_attchments_row.document_id 
 		,X_creation_date              => so_attchments_row.CREATION_DATE
 		,X_created_by                 => so_attchments_row.CREATED_BY
 		,X_last_update_date           => so_attchments_row.LAST_UPDATE_DATE
 		,X_last_updated_by            => so_attchments_row.LAST_UPDATED_BY
 		,X_last_update_login          => so_attchments_row.LAST_UPDATE_LOGIN
 		,X_seq_num                    => so_attchments_row.SEQ_NUM
	 	,X_entity_name                => 'GME_BATCH_HEADER'
 		,X_column1                    => so_attchments_row.COLUMN1
 		,X_pk1_value                  => p_batch_id
 		,X_pk2_value                  => NULL 
  		,X_pk3_value                  => NULL 
 		,X_pk4_value                  => NULL 
 		,X_pk5_value                  => NULL
 		,X_automatically_added_flag   => so_attchments_row.AUTOMATICALLY_ADDED_FLAG
 		,X_request_id                 => so_attchments_row.REQUEST_ID
 		,X_program_application_id     => so_attchments_row.PROGRAM_APPLICATION_ID
 		,X_program_id                 => so_attchments_row.PROGRAM_ID
 		,X_program_update_date        => so_attchments_row.PROGRAM_UPDATE_DATE
 		,X_Attribute_Category         => so_attchments_row.ATTRIBUTE_CATEGORY
 		,X_Attribute1                 => so_attchments_row.ATTRIBUTE1
 		,X_Attribute2                 => so_attchments_row.ATTRIBUTE2
 		,X_Attribute3                 => so_attchments_row.ATTRIBUTE3
 		,X_Attribute4                 => so_attchments_row.ATTRIBUTE4
 		,X_Attribute5                 => so_attchments_row.ATTRIBUTE5
	 	,X_Attribute6                 => so_attchments_row.ATTRIBUTE6
 		,X_Attribute7                 => so_attchments_row.ATTRIBUTE7
 		,X_Attribute8                 => so_attchments_row.ATTRIBUTE8
 		,X_Attribute9                 => so_attchments_row.ATTRIBUTE9
 		,X_Attribute10                => so_attchments_row.ATTRIBUTE10
 		,X_Attribute11                => so_attchments_row.ATTRIBUTE11
 		,X_Attribute12                => so_attchments_row.ATTRIBUTE12
	 	,X_Attribute13                => so_attchments_row.ATTRIBUTE13
 		,X_Attribute14                => so_attchments_row.ATTRIBUTE14
		,X_Attribute15                => so_attchments_row.ATTRIBUTE15
		, X_datatype_id                => so_attchments_row.DATATYPE_ID
		, X_category_id                => p_batch_category_id
		, X_security_type              => so_attchments_row.SECURITY_TYPE
		, X_security_id                => so_attchments_row.SECURITY_ID
		, X_publish_flag               => so_attchments_row.PUBLISH_FLAG
		, X_image_type                 => so_attchments_row.IMAGE_TYPE
		, X_storage_type               => so_attchments_row.STORAGE_TYPE
		, X_usage_type                 => so_attchments_row.USAGE_TYPE
		, X_language                   => USERENV('LANG') 
		, X_description                => so_attchments_row.DOCUMENT_DESCRIPTION
		, X_file_name                  => so_attchments_row.FILE_NAME
		, X_media_id                   => so_attchments_row.MEDIA_ID
		, X_doc_attribute_Category     => so_attchments_row.DOC_ATTRIBUTE_CATEGORY
		, X_doc_attribute1             => so_attchments_row.DOC_ATTRIBUTE1
		, X_doc_attribute2             => so_attchments_row.DOC_ATTRIBUTE2
		, X_doc_attribute3             => so_attchments_row.DOC_ATTRIBUTE3
		, X_doc_attribute4             => so_attchments_row.DOC_ATTRIBUTE4
		, X_doc_attribute5             => so_attchments_row.DOC_ATTRIBUTE5
		, X_doc_attribute6             => so_attchments_row.DOC_ATTRIBUTE6
		, X_doc_attribute7             => so_attchments_row.DOC_ATTRIBUTE7
		, X_doc_attribute8             => so_attchments_row.DOC_ATTRIBUTE8
		, X_doc_attribute9             => so_attchments_row.DOC_ATTRIBUTE9
		, X_doc_attribute11            => so_attchments_row.DOC_ATTRIBUTE11
		, X_doc_attribute12            => so_attchments_row.DOC_ATTRIBUTE12
		, X_doc_attribute13            => so_attchments_row.DOC_ATTRIBUTE13
		, X_doc_attribute14            => so_attchments_row.DOC_ATTRIBUTE14
		, X_doc_attribute15            => so_attchments_row.DOC_ATTRIBUTE15
		, X_create_doc                 => 'Y' --- Default is 'N'
		);
        x_return_status := FND_API.G_RET_STS_SUCCESS;
     END LOOP;
     IF (g_debug <= gme_debug.g_log_statement) THEN
        FND_FILE.Put_Line(FND_FILE.LOG,'Exiting copy_attachments procedure without any errors');
     END If;   
EXCEPTION
 WHEN OTHERS THEN 
    FND_FILE.Put_Line(FND_FILE.LOG, 'In others exception in copy_attachments');
    x_return_status := FND_API.G_RET_STS_ERROR;
END  Copy_attachments;

FUNCTION line_qualifies_for_MTO (
   p_line_id IN NUMBER
)
   RETURN BOOLEAN
IS
   l_return_status        VARCHAR2 ( 1 );
   l_msg_count            NUMBER;
   l_msg_data             VARCHAR2 ( 240 );
   l_debug                NUMBER := NVL ( FND_PROFILE.VALUE ( 'INV_DEBUG_TRACE' ), 0 );
   l_api_name             CONSTANT        VARCHAR2(30):= 'line_qualifies_for MTO';        
   l_in_assignments_rec   GME_MTO_RULE_ASSIGNMENTS%ROWTYPE;
   l_mto_rules_rec        GME_MTO_RULES%ROWTYPE;
   l_mto_assignments_rec  GME_MTO_RULE_ASSIGNMENTS%ROWTYPE;
   l_order_line_rec	  OE_ORDER_LINES_ALL%ROWTYPE;
   l_item_type            VARCHAR2(30);

   CURSOR get_order_line_rec(p_line_id IN NUMBER) IS
   SELECT * 
   FROM oe_order_lines_all
   WHERE line_id = p_line_id;

   CURSOR cur_item(p_organization_id IN NUMBER, p_inventory_item_id IN NUMBER) IS
   SELECT  item_type      
   FROM    mtl_system_items_b m
   WHERE   m.inventory_item_id = p_inventory_item_id
     AND  m.organization_id = p_organization_id;
BEGIN
    IF (g_debug <= gme_debug.g_log_statement) THEN
       FND_FILE.Put_Line(FND_FILE.LOG,g_pkg_name||'.'||l_api_name||' ENTER FUNCTION '||l_api_name||' for line '||p_line_id);    
       FND_FILE.Put_Line(FND_FILE.LOG, '*** function call to check if order line qualifies for OPM make to order ***' );   
    END IF;
   /* ============== Retrieve Order line data   ====================*/
   OPEN  get_order_line_rec(p_line_id);
   FETCH get_order_line_rec INTO l_order_line_rec;


   IF(get_order_line_rec%NOTFOUND) THEN
      CLOSE get_order_line_rec;
      IF (g_debug <= gme_debug.g_log_statement) THEN
         FND_FILE.Put_Line(FND_FILE.LOG,'get_order_line_rec%NOTFOUND, RETURN now');
      END IF;   
      Return FALSE;
   END IF;
   CLOSE get_order_line_rec;

   /* ============== Establish Item Characteristics ====================*/
   IF (g_debug <= gme_debug.g_log_statement) THEN
      FND_FILE.Put_Line(FND_FILE.LOG,g_pkg_name||'.'||l_api_name||' Retrieve Item for organization_id  '||l_order_line_rec.ship_from_org_id);
      FND_FILE.Put_Line(FND_FILE.LOG,g_pkg_name||'.'||l_api_name||' Retrieve Item for inventory_item_id '||l_order_line_rec.inventory_item_id);
   END IF;   
   OPEN cur_item (l_order_line_rec.ship_from_org_id, l_order_line_rec.inventory_item_id);
   FETCH cur_item INTO l_item_type;
   IF(cur_item%NOTFOUND) THEN
      CLOSE cur_item;                
      FND_FILE.Put_Line(FND_FILE.LOG,'Failure to retrieve item; terminate processing');
      Return FALSE;
   END IF;
   CLOSE cur_item;
   /* ============== Retrieve Rule where one exists ====================*/
   l_in_assignments_rec.organization_id   := l_order_line_rec.ship_from_org_id; 
   l_in_assignments_rec.inventory_item_id := l_order_line_rec.inventory_item_id; 
   l_in_assignments_rec.item_type         := l_item_type;      
   l_in_assignments_rec.customer_id       := l_order_line_rec.sold_to_org_id; 
   l_in_assignments_rec.site_use_id       := l_order_line_rec.ship_to_org_id; 

   GME_MAKE_TO_ORDER_PVT.retrieve_rule(
                         p_mto_assignments_rec    => l_in_assignments_rec                               
                        ,x_mto_rules_rec          => l_mto_rules_rec                                 
                        ,x_mto_assignments_rec    => l_mto_assignments_rec                             
                        ,x_return_status          => l_return_status
	                ,x_msg_data	          => l_msg_data
			,x_msg_count	          => l_msg_count);
 
      IF (g_debug <= gme_debug.g_log_statement) THEN
         FND_FILE.Put_Line(FND_FILE.LOG,g_pkg_name||'.'||l_api_name||' Return status from Retrieve_Rule is '||l_return_status);
         FND_FILE.Put_Line(FND_FILE.LOG,g_pkg_name||'.'||l_api_name||' Rule ID to work with is        '||l_mto_assignments_rec.rule_id);
         FND_FILE.Put_Line(FND_FILE.LOG,g_pkg_name||'.'||l_api_name||' Rule being honored is '||l_mto_rules_rec.rule_name);
      END IF;
   IF( l_return_status <> FND_API.G_RET_STS_SUCCESS ) or (NVL(l_mto_assignments_rec.rule_id,0) = 0) THEN
      Return FALSE;
   ELSE
      Return TRUE;
   END IF;
END line_qualifies_for_MTO;

 PROCEDURE retrieve_rule
 (
    p_mto_assignments_rec    IN    GME_MTO_RULE_ASSIGNMENTS%ROWTYPE
  , x_mto_rules_rec          OUT   NOCOPY GME_MTO_RULES%ROWTYPE
  , x_mto_assignments_rec    OUT   NOCOPY GME_MTO_RULE_ASSIGNMENTS%ROWTYPE
  , x_return_status          OUT   NOCOPY VARCHAR2
  , x_msg_count              OUT   NOCOPY NUMBER
  , x_msg_data               OUT   NOCOPY VARCHAR2
 ) IS
 
  l_api_name                 CONSTANT        VARCHAR2(30):= 'retrieve_rule';
  l_mto_assignments_rec      GME_MTO_RULE_ASSIGNMENTS%ROWTYPE;
  l_mto_rules_rec            GME_MTO_RULES%ROWTYPE;
  
  Cursor get_assignment IS
  Select  decode(site_use_id, null, 0, site_use_id) site_use_id
       ,  decode(customer_id, null, 0, customer_id) customer_id
       ,  decode(inventory_item_id, null, 0, inventory_item_id) item_id
       ,  decode(item_type, null, ' ', item_type) item_type          
       ,  rule_assign_id
       ,  rule_id
       ,  organization_id
  From gme_mto_rule_assignments
  Where organization_id = p_mto_assignments_rec.organization_id
    and (inventory_item_id = p_mto_assignments_rec.inventory_item_id
         or inventory_item_id is null )
    and (item_type = p_mto_assignments_rec.item_type        
         or item_type is null)
    and (customer_id = p_mto_assignments_rec.customer_id
         or customer_id is null)
    and (site_use_id = p_mto_assignments_rec.site_use_id
         or site_use_id is null)
  Order by
    3 desc
  , 1 desc
  , 2 desc
  , 4 desc
  ;
  
  Cursor get_rule IS
  Select  *                                                              
  From gme_mto_rules                    
  Where rule_id = l_mto_assignments_rec.rule_id;

 BEGIN
     IF (g_debug <= gme_debug.g_log_statement) THEN
     FND_FILE.Put_Line(FND_FILE.LOG,g_pkg_name||'.'||l_api_name||' ENTER API  ');
     FND_FILE.Put_Line(FND_FILE.LOG,g_pkg_name||'.'||l_api_name||' organization_id  '||p_mto_assignments_rec.organization_id);
     FND_FILE.Put_Line(FND_FILE.LOG,g_pkg_name||'.'||l_api_name||' inventory_item_id '||p_mto_assignments_rec.inventory_item_id);
     FND_FILE.Put_Line(FND_FILE.LOG,g_pkg_name||'.'||l_api_name||' item_type         '||p_mto_assignments_rec.item_type);
     FND_FILE.Put_Line(FND_FILE.LOG,g_pkg_name||'.'||l_api_name||' customer_id       '||p_mto_assignments_rec.customer_id);
     FND_FILE.Put_Line(FND_FILE.LOG,g_pkg_name||'.'||l_api_name||' site_use_id       '||p_mto_assignments_rec.site_use_id);
     END IF;
  x_return_status := FND_API.G_RET_STS_SUCCESS;
  /* get the most suitable rule first */
  IF (g_debug <= gme_debug.g_log_statement) THEN
     FND_FILE.Put_Line(FND_FILE.LOG, 'IN check Rule');
  END IF;   
  Open get_assignment;
  Fetch get_assignment  
  Into l_mto_assignments_rec.site_use_id
    ,  l_mto_assignments_rec.customer_id
    ,  l_mto_assignments_rec.inventory_item_id
    ,  l_mto_assignments_rec.item_type      
    ,  l_mto_assignments_rec.rule_assign_id
    ,  l_mto_assignments_rec.rule_id 
    ,  l_mto_assignments_rec.organization_id;
  IF(get_assignment%NOTFOUND) THEN
    CLOSE get_assignment;
    IF (g_debug <= gme_debug.g_log_statement) THEN
       FND_FILE.Put_Line(FND_FILE.LOG,'No rule assigment to match input criteria; returning from Retrieve Rule');
    END IF;   
    RETURN;
  END IF;
  Close get_assignment;
  
  x_mto_assignments_rec := l_mto_assignments_rec;

  Open get_rule;               
  Fetch get_rule into l_mto_rules_rec;
   IF(get_rule%NOTFOUND) THEN
      CLOSE get_rule;
      FND_FILE.Put_Line(FND_FILE.LOG,'Error encountered on rule retrieval for Rule ID '||l_mto_assignments_rec.rule_id);
      RAISE FND_API.G_EXC_ERROR;
   END IF;
  Close get_rule;                                                  

  x_mto_rules_rec := l_mto_rules_rec;
 END retrieve_rule;


END GME_MAKE_TO_ORDER_PVT;
/
COMMIT;
-- show errors;
EXIT;
