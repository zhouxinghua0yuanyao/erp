/*=======================================================================+
 |  Copyright (c) 2001, 2016 Oracle Corporation Redwood Shores, California, USA|
 |                          All rights reserved.                         |
 +=======================================================================+
 | FILENAME
 |      GMECRSVB.pls
 |
 | DESCRIPTION
 |
 |      Body of package GME_RESERVE_CONC
 |
 | PROCEDURES:
 |
 |      1. Reserve Eligible:
 |      This procedure is called from RESERVE procedure and this procedure is
 |      used to check if the Line qualifies for Reservation.
 |
 |      2. Create_Reservation:
 |      This is called from the RESERVE procedure and this is used to Create
 |      the Reservation for the line.
 |
 |      3. Make_to_order:
 |      This is the main procedure that is used to reserve any lines that are
 |      scheduled but are not yet Reserved.  Parameters have to be provided
 |      that will be used to refine the selection criteria for the
 |      Order/Lines. "Use Reservation Time fence" can be used to control whether
 |      to reserve all the eligible lines or only those lines that fall within 
 |      the Reservation Time Fence.
 |
 |      4. Calulate_Percentage:
 |      This procedure is called from the RESERVE procedure. This will determine
 |      the pecentage when program is running in a "Fair Share" mode.
 |
 | FUNCTIONS
 |      None
 |
 | NOTES
 |
 | REVISION HISTORY
 |      Date              Name              Description
 |      -----------       --------          -----------
 |      DEC-2007          Srinivasulu Puri    Created
 |      27-SEP-2011       Archana Mundhe      Bug 12784707 
 |      Commented code related to Booked_flag as it is now disabled on the concurrent program parameter list.   
 |      03-FEB-2012       Archana Mundhe      Bug 13458175 
 |      Modified procedure reserve_eligible so that the existing reservations can be verified. 
 |      26-JUN-2014   Archana Mundhe    Bug 18960929                            
 |      Removed all OM debugs and replaced with FND debug logging so the
 |      messages are seen in the concurrent log.     
 |      22-AUG-2014  Archana Mundhe  Bug 18960929
 |      Changed procedure make_to_order such that set_policy_context code is run                   
 |      28-SEP-2015  A. Mundhe         Bug 21890861 Only log detail messages if 
 |      gme debug profile is set at statement level.                             |
 *=======================================================================*/
SET VERIFY OFF
WHENEVER SQLERROR EXIT FAILURE ROLLBACK;
WHENEVER OSERROR EXIT FAILURE ROLLBACK;
REM Added for ARU db drv auto generation
REM dbdrv: sql ~PROD ~PATH ~FILE none none none package &phase=plb checkfile(120.1.12010000.8=120.2.12020000.6):~PROD:~PATH:~FILE

CREATE or REPLACE PACKAGE BODY GME_RESERVE_CONC AS
/* $Header: GMECRSVB.pls 120.2.12020000.6 2016/06/08 18:18:16 adeshmuk ship $ */

--  Global constant holding the package name
 
G_DEBUG                       VARCHAR2 (5)  := fnd_profile.VALUE ('AFLOG_LEVEL');
G_PKG_NAME                    CONSTANT VARCHAR2(30) := 'GME_RESERVE_CONC';
G_SET_ID                      NUMBER;
G_PROGRAM_APPLICATION_ID      NUMBER;
G_PROGRAM_ID                  NUMBER;
G_RESERVATION_MODE            VARCHAR2(30);
G_TOTAL_CONSUMED              NUMBER :=0;
G_CONSUMED_FOR_LOT            NUMBER :=0;
G_TOTAL_CONSUMED2             NUMBER :=0; -- INVCONV
G_CONSUMED_FOR_LOT2           NUMBER :=0;  -- INVCONV


/*----------------------------------------------------------------
PROCEDURE  : Reserve_Eligible
DESCRIPTION: This Procedure is to check if the Line that is being 
             considered needs Reservation 
----------------------------------------------------------------*/
-- this is a clone of Reserve_Eligible, but it does not look at existing reservations
Procedure Reserve_Eligible
 ( p_line_rec                   IN OE_ORDER_PUB.line_rec_type,
   p_use_reservation_time_fence IN VARCHAR2,
   x_return_status              OUT NOCOPY VARCHAR2
 ) 
IS
l_return_status          VARCHAR2(1)  := FND_API.G_RET_STS_SUCCESS;
l_result                 Varchar2(30);
l_scheduling_level_code  VARCHAR2(30) := NULL;
l_out_return_status      VARCHAR2(1)  := FND_API.G_RET_STS_SUCCESS;
l_type_code              VARCHAR2(30);
l_org_id                 NUMBER;
l_time_fence             BOOLEAN;
l_msg_count              NUMBER;
l_msg_data               VARCHAR2(1000);
l_dummy                  VARCHAR2(100);

--
l_debug_level CONSTANT NUMBER := oe_debug_pub.g_debug_level;
--

BEGIN   
   IF (g_debug <= gme_debug.g_log_statement) THEN
       FND_FILE.Put_Line(FND_FILE.LOG,'Inside Reserve Eligible Procedure');   
   END IF;

   /* Check if line is open, if not open ignore the line */
   IF ( p_line_rec.open_flag = 'N' ) THEN      
      FND_FILE.Put_Line(FND_FILE.LOG,'Line is closed, not eligible for reservation');
      l_return_status := FND_API.G_RET_STS_ERROR;

   /* Check if line is shipped, if shipped then ignore the line */
   ELSIF ( nvl(p_line_rec.shipped_quantity, -99) > 0 ) THEN
      FND_FILE.Put_Line(FND_FILE.LOG,'Line is shipped, not eligible for reservation');
      l_return_status := FND_API.G_RET_STS_ERROR;
   END IF;

   -- Bug 13458175 
   -- Uncommented the AND 1=2 condition so that the existing reservations can be verified. 
   IF l_return_status = FND_API.G_RET_STS_SUCCESS 
      AND NVL(g_reservation_mode,'*') <> 'PARTIAL' THEN                                
     /* We need to check for Existing Reservations on the Line */
      BEGIN
      IF (g_debug <= gme_debug.g_log_statement) THEN
         FND_FILE.Put_Line(FND_FILE.LOG,'Before checking Existing Reservations');
      END IF;   
 
         -- Bug 23228636
         -- Modified the cursor below to check if MTO has already been run and batch already created for the SO line. 
         SELECT 'Reservation Exists' 
         INTO l_dummy
         FROM MTL_RESERVATIONS  r, gme_batch_header h, gme_material_details d
         WHERE r.DEMAND_SOURCE_LINE_ID = p_line_rec.line_id
         AND r.inventory_item_id = d.inventory_item_id
         AND r.supply_source_header_id = d.batch_id
         AND r.supply_source_line_id = d.material_detail_id
         AND h.batch_id = d.batch_id
         AND D.LINE_TYPE = 1;        

       IF (g_debug <= gme_debug.g_log_statement) THEN
         FND_FILE.Put_Line(FND_FILE.LOG,'Reservations exists on the line');
       END IF;
         RAISE FND_API.G_EXC_ERROR;
      EXCEPTION
         WHEN FND_API.G_EXC_ERROR THEN
         IF (g_debug <= gme_debug.g_log_statement) THEN
            FND_FILE.Put_Line(FND_FILE.LOG,'In Expected Error for Check Reservation');
         END IF;   
            l_return_status := FND_API.G_RET_STS_ERROR;

         WHEN NO_DATA_FOUND THEN
            null; -- Prior Reservation does not exists and l_return_status is already initialized to success.
         WHEN TOO_MANY_ROWS THEN
            l_return_status := FND_API.G_RET_STS_ERROR; 
      END;
    END IF;
    IF l_return_status = FND_API.G_RET_STS_SUCCESS THEN
    BEGIN
       IF (g_debug <= gme_debug.g_log_statement) THEN
          FND_FILE.Put_Line(FND_FILE.LOG,'Before checking for Staged/Closed deliveries');       
       END IF;

       SELECT 'Staging Exists'
       INTO   l_dummy
       FROM   WSH_DELIVERY_DETAILS
       WHERE  SOURCE_LINE_ID = p_line_rec.line_id
       AND    SOURCE_CODE = 'OE' 
       AND    RELEASED_STATUS IN ('Y', 'C');
       
       FND_FILE.Put_Line(FND_FILE.LOG,'Staged/Closed deliveries exist for the line');

       RAISE FND_API.G_EXC_ERROR;

    EXCEPTION
      WHEN FND_API.G_EXC_ERROR THEN
         IF (g_debug <= gme_debug.g_log_statement) THEN
            FND_FILE.Put_Line(FND_FILE.LOG,'In Expected Error for Checking Staged/Closed deliveries');         
         END IF;
         l_return_status := FND_API.G_RET_STS_ERROR;
      WHEN NO_DATA_FOUND THEN
         NULL;
      WHEN TOO_MANY_ROWS THEN
         l_return_status := FND_API.G_RET_STS_ERROR;
    END;
    END IF;
   IF l_return_status = FND_API.G_RET_STS_SUCCESS THEN

   -- WE NEED TO CHECK FOR THE reservation_time_fence Value.
   -- If the Value of the parameter passed to the concurrent
   -- program is "NO' then we reserve the lines irrespective
   -- of the profile option: OM : Reservation_Time_fence.
   -- By default this parameter will have a value of YES.

        IF (NVL(p_use_reservation_time_fence,'Y') = 'Y' or 
          NVL(p_use_reservation_time_fence,'Yes') = 'Yes') THEN
          IF (g_debug <= gme_debug.g_log_statement) THEN
            FND_FILE.Put_Line(FND_FILE.LOG,'Schedule Ship Date:'||
                                p_line_rec.schedule_ship_date);          
          END IF;                      
 
            IF NOT OE_SCHEDULE_UTIL.Within_Rsv_Time_Fence
                            (p_line_rec.schedule_ship_date, p_line_rec.org_id) THEN              
                IF (g_debug <= gme_debug.g_log_statement) THEN            
                   FND_FILE.Put_Line(FND_FILE.LOG,'The Schedule Date for Line falls
                          beyond reservation Time Fence');
                END IF;
              RAISE FND_API.G_EXC_ERROR ;
          
            END IF;
          END IF;
       
        IF (g_debug <= gme_debug.g_log_statement) THEN
           FND_FILE.Put_Line(FND_FILE.LOG,'check scheduling level  for header:'||p_line_rec.header_id);
           FND_FILE.Put_Line(FND_FILE.LOG,'check scheduling level  for line type:'||p_line_rec.line_type_id);        
        END IF;
        l_scheduling_level_code := OE_SCHEDULE_UTIL.Get_Scheduling_Level
                                        (p_line_rec.header_id
                                        ,p_line_rec.line_type_id);
        IF (g_debug <= gme_debug.g_log_statement) THEN                                        
           FND_FILE.Put_Line(FND_FILE.LOG,'l_scheduling_level_code:'||l_scheduling_level_code);
        END IF;
        IF l_scheduling_level_code is not null AND 
        (l_scheduling_level_code = SCH_LEVEL_ONE
    OR l_scheduling_level_code =  SCH_LEVEL_TWO
    OR l_scheduling_level_code =  SCH_LEVEL_FIVE)
    THEN
           IF p_line_rec.schedule_action_code = OESCH_ACT_RESERVE OR
             (p_line_rec.schedule_status_code is  null AND
             (p_line_rec.schedule_ship_date is NOT NULL OR
              p_line_rec.schedule_arrival_date is NOT NULL))
            THEN
               FND_FILE.Put_Line(FND_FILE.LOG,'Order Type Does not Allow Scheduling');
               RAISE FND_API.G_EXC_ERROR;
            END IF;
        END IF;

   END IF; -- Check for Reservation Exists Clause 
   x_return_status := l_return_status;

      IF (g_debug <= gme_debug.g_log_statement) THEN
         FND_FILE.Put_Line(FND_FILE.LOG,'..Exiting GME_RESERVE_CONC.Need_Reservation' ||
                        l_return_status );
      END IF;
EXCEPTION
  WHEN FND_API.G_EXC_ERROR THEN
       IF (g_debug <= gme_debug.g_log_statement) THEN
          FND_FILE.Put_Line(FND_FILE.LOG,'In Expected Error...in Proc Reserve_Eligible');
       END IF;
     x_return_status := FND_API.G_RET_STS_ERROR;

  WHEN FND_API.G_EXC_UNEXPECTED_ERROR THEN
       IF (g_debug <= gme_debug.g_log_statement) THEN
          FND_FILE.Put_Line(FND_FILE.LOG,'In UnExpected Error...in Proc Reserve_Eligible');
       END IF;   
     x_return_status := FND_API.G_RET_STS_UNEXP_ERROR;

   WHEN OTHERS THEN

     x_return_status := FND_API.G_RET_STS_UNEXP_ERROR;

     IF OE_MSG_PUB.Check_Msg_Level(OE_MSG_PUB.G_MSG_LVL_UNEXP_ERROR)
        THEN
            OE_MSG_PUB.Add_Exc_Msg
            (   G_PKG_NAME
            ,   'Action_Reserve'
            );
     END IF;

End Reserve_Eligible; 


/*----------------------------------------------------------------
PROCEDURE  : Create_Reservation
DESCRIPTION: This Procedure send the line to the Inventory for
             Reservation 
-----------------------------------------------------------------*/
Procedure Create_Reservation
(p_line_rec      IN OE_ORDER_PUB.line_rec_type,
 x_return_status OUT NOCOPY /* file.sql.39 change */ VARCHAR2)
IS
l_return_status         VARCHAR2(1)  := FND_API.G_RET_STS_SUCCESS;
l_reservation_rec       Inv_Reservation_Global.Mtl_Reservation_Rec_Type;
l_msg_count             NUMBER;
l_dummy_sn              Inv_Reservation_Global.Serial_Number_Tbl_Type;
l_msg_data              VARCHAR2(1000);
l_buffer                VARCHAR2(1000);
l_quantity_reserved     NUMBER;
l_quantity_to_reserve   NUMBER;
l_rsv_id                NUMBER;

l_quantity2_reserved 	NUMBER;   
l_quantity2_to_reserve 	NUMBER; 

--
l_debug_level CONSTANT NUMBER := oe_debug_pub.g_debug_level;
--


BEGIN    

IF (g_debug <= gme_debug.g_log_statement) THEN
    FND_FILE.Put_Line(FND_FILE.LOG,'In the Procedure Create Reservation');
    FND_FILE.Put_Line(FND_FILE.LOG,'Before call of Load_INV_Request');
END IF;    
    
    
    IF p_line_rec.ordered_quantity2 = 0 -- INVCONV
     THEN
      l_quantity2_to_reserve := NULL;
    END IF;
    
    
    OE_SCHEDULE_UTIL.Load_Inv_Request
              ( p_line_rec              => p_line_rec
              , p_quantity_to_reserve   => p_line_rec.ordered_quantity
              , p_quantity2_to_reserve  => l_quantity2_to_reserve -- INVCONV
              , x_reservation_rec       => l_reservation_rec);


    -- Call INV with action = RESERVE    
    IF (g_debug <= gme_debug.g_log_statement) THEN
       FND_FILE.Put_Line(FND_FILE.LOG,'Before call of inv_reservation_pub.create_reservation');
    END IF;   

    INV_RESERVATION_PUB.Create_Reservation
               ( p_api_version_number         => 1.0
                , p_init_msg_lst              => FND_API.G_TRUE
                , x_return_status             => l_return_status
                , x_msg_count                 => l_msg_count
                , x_msg_data                  => l_msg_data
                , p_rsv_rec                   => l_reservation_rec
                , p_serial_number             => l_dummy_sn
                , x_serial_number             => l_dummy_sn
                , p_partial_reservation_flag  => FND_API.G_FALSE
                , p_force_reservation_flag    => FND_API.G_FALSE
                , p_validation_flag           => FND_API.G_TRUE
                , x_quantity_reserved         => l_quantity_reserved
                , x_secondary_quantity_reserved => l_quantity2_reserved 
                , x_reservation_id            => l_rsv_id
                );

    IF (g_debug <= gme_debug.g_log_statement) THEN
       FND_FILE.Put_Line(FND_FILE.LOG,'1. After Calling Create Reservation' ||
                                              l_return_status);
       FND_FILE.Put_Line(FND_FILE.LOG,l_msg_data);    
    END IF;   

    IF l_return_status = FND_API.G_RET_STS_UNEXP_ERROR THEN
           IF (g_debug <= gme_debug.g_log_statement) THEN
              FND_FILE.Put_Line(FND_FILE.LOG,'Raising Unexpected error');
           END IF;
           RAISE FND_API.G_EXC_UNEXPECTED_ERROR;

    ELSIF l_return_status = FND_API.G_RET_STS_ERROR THEN
          IF (g_debug <= gme_debug.g_log_statement) THEN
             FND_FILE.Put_Line(FND_FILE.LOG,'Raising Expected error');
          END IF;   
          IF l_msg_data is not null THEN
             fnd_message.set_encoded(l_msg_data);
             l_buffer := fnd_message.get;
             OE_MSG_PUB.Add_text(p_message_text => l_buffer);
             FND_FILE.Put_Line(FND_FILE.LOG,l_msg_data);             
          END IF;
               RAISE FND_API.G_EXC_ERROR;

    END IF;
    IF (g_debug <= gme_debug.g_log_statement) THEN
       FND_FILE.Put_Line(FND_FILE.LOG,'..Exiting GME_RESERVE_CONC.Create_reservation' ||
                        l_return_status);
    END IF;                    
    x_return_status := FND_API.G_RET_STS_SUCCESS;

EXCEPTION
  WHEN FND_API.G_EXC_ERROR THEN     
     IF (g_debug <= gme_debug.g_log_statement) THEN
        FND_FILE.Put_Line(FND_FILE.LOG,'In Expected Error...in Proc Create_Reservation');
     END IF;   
     x_return_status := FND_API.G_RET_STS_ERROR;

  WHEN FND_API.G_EXC_UNEXPECTED_ERROR THEN
     IF (g_debug <= gme_debug.g_log_statement) THEN
        FND_FILE.Put_Line(FND_FILE.LOG,'In Unexpected Error...in Proc Create_Reservation');
     END IF;   
     x_return_status := FND_API.G_RET_STS_UNEXP_ERROR;

   WHEN OTHERS THEN
     IF (g_debug <= gme_debug.g_log_statement) THEN
        FND_FILE.Put_Line(FND_FILE.LOG,'In others error...in Proc Create_Reservation');
     END IF;   
     x_return_status := FND_API.G_RET_STS_UNEXP_ERROR;

END;


/*----------------------------------------------------------------------
PROCEDURE  : OPM_MTO 
DESCRIPTION: Create and Reserve OPM Production Batch Concurrent Request
----------------------------------------------------------------------*/

Procedure Make_to_Order
(ERRBUF                         OUT NOCOPY VARCHAR2,
 RETCODE                        OUT NOCOPY VARCHAR2,
 p_org_id                       IN NUMBER, 
 p_use_reservation_time_fence   IN CHAR,
 p_order_number_low             IN NUMBER,
 p_order_number_high            IN NUMBER,
 p_customer_id                  IN VARCHAR2,
 p_order_type                   IN VARCHAR2,
 p_line_type_id                 IN VARCHAR2,
 p_warehouse                    IN VARCHAR2,
 p_inventory_item_id            IN VARCHAR2,
 p_request_date_low             IN VARCHAR2,
 p_request_date_high            IN VARCHAR2,
 p_schedule_ship_date_low       IN VARCHAR2,
 p_schedule_ship_date_high      IN VARCHAR2,
 p_schedule_arrival_date_low    IN VARCHAR2,
 p_schedule_arrival_date_high   IN VARCHAR2,
 p_ordered_date_low             IN VARCHAR2,
 p_ordered_date_high            IN VARCHAR2,
 p_demand_class_code            IN VARCHAR2,
 p_planning_priority            IN NUMBER,
 p_booked                       IN VARCHAR2   DEFAULT NULL,
 p_line_id                      IN NUMBER 
)IS

l_api_name                      CONSTANT VARCHAR2 (30) := 'Make_to_Order';
l_stmt                          VARCHAR2(4000) :=NULL;
l_line_rec                      OE_ORDER_PUB.line_rec_type;
l_return_status                 VARCHAR2(1)  := FND_API.G_RET_STS_SUCCESS;
l_request_date_low              DATE;
l_request_date_high             DATE;
l_schedule_ship_date_low        DATE;
l_schedule_ship_date_high       DATE;
l_schedule_arrival_date_low     DATE;
l_schedule_arrival_date_high    DATE;
l_ordered_date_low              DATE;
l_ordered_date_high             DATE;
l_line_id                       NUMBER;
l_rsv_tbl                       Rsv_Tbl_Type;
l_temp_rsv_tbl                  Rsv_Tbl_Type;
l_cursor_id                     INTEGER;
l_retval                        INTEGER;
l_set_id                        NUMBER :=0;
l_process_flag                  VARCHAR2(1);
l_request_id                    NUMBER;
l_msg_data                      VARCHAR2(2000);
l_errbuf                        VARCHAR2(2000);
l_retcode                       VARCHAR2(2000);


--
l_debug_level CONSTANT NUMBER := oe_debug_pub.g_debug_level;
--
-- Moac
l_single_org                    BOOLEAN := FALSE;
l_old_org_id                    NUMBER  := -99;
l_org_id                        NUMBER;
l_user_set_id			NUMBER :=0;
l_created_by                    NUMBER;
     
BEGIN
   ERRBUF  := 'Create and Reserve OPM Production Batch Request completed successfully';
   RETCODE := 0;

   FND_PROFILE.Get('CONC_REQUEST_ID', l_request_id);

   FND_FILE.Put_Line(FND_FILE.LOG, 'Parameters:');
   FND_FILE.Put_Line(FND_FILE.LOG, '    ORG_ID      = '||
                                        p_org_id);
   FND_FILE.Put_Line(FND_FILE.LOG, '    Use_reservation_time_fence =  '||
                                        p_use_reservation_time_fence);
   FND_FILE.Put_Line(FND_FILE.LOG, '    order_number_low =  '||
                                        p_order_number_low);
   FND_FILE.Put_Line(FND_FILE.LOG, '    order_number_high = '||
                                        p_order_number_high);
   FND_FILE.Put_Line(FND_FILE.LOG, '    Customer = '||
                                        p_customer_id);
   FND_FILE.Put_Line(FND_FILE.LOG, '    order_type = '||
                                        p_order_type);
   FND_FILE.Put_Line(FND_FILE.LOG, '    Warehouse = '||
                                        p_Warehouse);
   FND_FILE.Put_Line(FND_FILE.LOG, '    request_date_low = '||
                                        p_request_date_low);
   FND_FILE.Put_Line(FND_FILE.LOG, '    request_date_high = '||
                                        p_request_date_high);
   FND_FILE.Put_Line(FND_FILE.LOG, '    schedule_date_low = '||
                                        p_schedule_ship_date_low);
   FND_FILE.Put_Line(FND_FILE.LOG, '    schedule_date_high = '||
                                        p_schedule_ship_date_high);
   FND_FILE.Put_Line(FND_FILE.LOG, '    ordered_date_low = '||
                                        p_ordered_date_low);
   FND_FILE.Put_Line(FND_FILE.LOG, '    ordered_date_high = '||
                                        p_ordered_date_high);
   FND_FILE.Put_Line(FND_FILE.LOG, '    Demand Class = '||
                                        p_demand_class_code);
   FND_FILE.Put_Line(FND_FILE.LOG, '    item = '||
                                        p_inventory_item_id);
   FND_FILE.Put_Line(FND_FILE.LOG, '    Planning Priority = '||
                                        p_Planning_priority);
   FND_FILE.Put_Line(FND_FILE.LOG, '    Booked Flag  = '||
                                        p_booked);
   FND_FILE.Put_Line(FND_FILE.LOG, '    Line ID      = '||
                                        p_line_id     );
   
   IF (g_debug <= gme_debug.g_log_statement) THEN
      FND_FILE.Put_Line(FND_FILE.LOG,'Entering api '||g_pkg_name||'.'||l_api_name);

      FND_FILE.Put_Line(FND_FILE.LOG,'setting dates ');
   END IF;   

   SELECT FND_DATE.Canonical_To_Date(p_request_date_low),
          FND_DATE.Canonical_To_Date(p_request_date_high),
          FND_DATE.Canonical_To_Date(p_schedule_ship_date_low),
          FND_DATE.Canonical_To_Date(p_schedule_ship_date_high),
          FND_DATE.Canonical_To_Date(p_schedule_arrival_date_low),
          FND_DATE.Canonical_To_Date(p_schedule_arrival_date_high),
          FND_DATE.Canonical_To_Date(p_ordered_date_low),
          FND_DATE.Canonical_To_Date(p_ordered_date_high)
   INTO   l_request_date_low,
          l_request_date_high,
          l_schedule_ship_date_low,
          l_schedule_ship_date_high,
          l_schedule_arrival_date_low,
          l_schedule_arrival_date_high,
          l_ordered_date_low,
          l_ordered_date_high
   FROM   DUAL;

   IF (g_debug <= gme_debug.g_log_statement) THEN
      FND_FILE.Put_Line(FND_FILE.LOG,'Before MO Current org id is: ' || MO_GLOBAL.GET_CURRENT_ORG_ID);
      FND_FILE.Put_Line(FND_FILE.LOG,'MO_GLOBAL.get_access_mode is: ' || MO_GLOBAL.get_access_mode);
   END IF;   
   
   -- Moac Start
   IF MO_GLOBAL.get_access_mode =  'S' THEN
	l_single_org := TRUE;	
   END IF;
   -- Bug 18960929 
   -- Changed below to IF instead if elseif so the set_policy_context code is run.  
   IF p_org_id IS NOT NULL THEN
      l_single_org := TRUE;
      MO_GLOBAL.set_policy_context(p_access_mode => 'S', p_org_id  => p_org_id);
   END IF;
   -- Moac End

   IF (g_debug <= gme_debug.g_log_statement) THEN
      FND_FILE.Put_Line(FND_FILE.LOG,'After MO Current org id is: ' || MO_GLOBAL.GET_CURRENT_ORG_ID);
   END IF;   
	

   l_cursor_id := DBMS_SQL.OPEN_CURSOR;
   -- Start constructing retrieval syntax
   IF (g_debug <= gme_debug.g_log_statement) THEN
      FND_FILE.Put_Line(FND_FILE.LOG,'Start building retrieval syntax');
      FND_FILE.Put_Line(FND_FILE.LOG, 'Starting syntax construction');
   END IF;   
   l_stmt := 'SELECT Line_id, l.org_id FROM  OE_ORDER_LINES l, OE_ORDER_HEADERS_ALL h ,MTL_SYSTEM_ITEMS msi ';
   l_stmt := l_stmt|| ' WHERE NVL(h.cancelled_flag,'||'''N'''||') <> ' ||'''Y'''||
     ' AND  h.header_id  = l.header_id'||
     ' AND  h.open_flag  = '||'''Y'''||
     ' AND  NVL(l.cancelled_flag,'||'''N'''||') <> '||'''Y'''||
     ' AND  NVL(l.line_category_code,'||'''ORDER'''||') <> '||'''RETURN''';
     
   -- Bug 12784707 
   -- Booked_flag is disabled on the concurrent program parameter list.   
   -- before this fix, MT0 was running for non booked orders, DRAFT, ENTERED etc.
   -- As inventory does not allow reservations for non-booked orders it  was causing multiple batches being created
   -- without the corresponding reservations. 
   -- Based on feedback from PM disabling the booked_flag parameter from conc program list and commenting the logic 
   -- associated with it. Now the MTO logic will work for booked orders only.
   -- Note: In 11i, MTO also worked for non booked orders and opm allocations/reservation got created successfully. 
   --IF NVL(p_booked,'*') = 'Y' THEN
      l_stmt := l_stmt||' AND  h.booked_flag  = '||'''Y''';
   --ELSIF NVL(p_booked,'*') = 'N' THEN
     -- l_stmt := l_stmt||' AND  h.booked_flag  = '||'''N''';
   --END IF;

   IF p_org_id IS NOT NULL THEN
      l_stmt := l_stmt ||' AND  l.org_id = :org_id'; -- p_org_id
   END IF;

   IF p_line_id IS NOT NULL THEN
      l_stmt := l_stmt ||' AND  l.line_id = :line_id'; -- p_line_id
   END IF;

   IF p_order_number_low IS NOT NULL THEN
      l_stmt := l_stmt ||' AND  h.order_number >=:order_number_low'; -- p_order_number_low
   END IF;
   IF p_order_number_high IS NOT NULL THEN
      l_stmt := l_stmt ||' AND  h.order_number <=:order_number_high'; -- p_order_number_high
   END IF;
   IF p_customer_id IS NOT NULL THEN
      l_stmt := l_stmt ||' AND  h.sold_to_org_id =:customer_id'; --p_customer_id
   END IF;
   IF p_order_type IS NOT NULL THEN
      l_stmt := l_stmt ||' AND  h.order_type_id =:order_type';  --p_order_type
   END IF;
   IF l_ordered_date_low IS NOT NULL THEN
      FND_FILE.Put_Line(FND_FILE.LOG, 'GME Ordered date low here  ');
      l_stmt := l_stmt ||' AND  h.ordered_date >=:ordered_date_low'; --l_ordered_date_low
   END IF;
   IF l_ordered_date_high IS NOT NULL THEN
      l_stmt := l_stmt ||' AND  h.ordered_date <=:ordered_date_high';  --l_ordered_date_high;
   END IF;
   IF p_line_type_id IS NOT NULL THEN
      l_stmt := l_stmt ||' AND l.line_type_id =:line_type_id';   --p_line_type_id
   END IF;
   l_stmt := l_stmt ||' AND l.open_flag  = '||'''Y''';
   IF p_warehouse IS NOT NULL THEN
      l_stmt := l_stmt ||' AND l.ship_from_org_id =:warehouse';  --p_warehouse
   END IF;
   IF l_request_date_low IS NOT NULL THEN
      l_stmt := l_stmt ||' AND l.request_date >=:request_date_low';  --l_request_date_low;
   END IF;
   IF l_request_date_high IS NOT NULL THEN
      l_stmt := l_stmt ||' AND l.request_date <=:request_date_high';  --l_request_date_high
   END IF;
   IF l_schedule_ship_date_low IS NOT NULL THEN
      l_stmt := l_stmt ||' AND l.schedule_ship_date >=:schedule_ship_date_low';  --l_schedule_ship_date_low
   END IF;
   IF l_schedule_ship_date_high IS NOT NULL THEN
      l_stmt := l_stmt ||' AND l.schedule_ship_date <=:schedule_ship_date_high';  --l_schedule_ship_date_high
   END IF;
   IF l_schedule_arrival_date_low IS NOT NULL THEN
      l_stmt := l_stmt ||' AND l.Schedule_Arrival_Date >=:schedule_arrival_date_low';  --l_schedule_arrival_date_low
   END IF;
   IF l_schedule_arrival_date_high IS NOT NULL THEN
      l_stmt := l_stmt ||' AND l.Schedule_Arrival_Date <=:schedule_arrival_date_high';  --l_schedule_arrival_date_high
   END IF;
   IF p_inventory_item_id IS NOT NULL THEN
      l_stmt := l_stmt ||' AND l.inventory_item_id =:inventory_item_id'; -- p_inventory_item_id
   END IF;
   IF p_demand_class_code IS NOT NULL THEN
      l_stmt := l_stmt ||' AND NVL(l.demand_class_code,'||'''-99'''||') =:demand_class_code';  --p_demand_class_code
   END IF;
   IF p_planning_priority IS NOT NULL THEN
      l_stmt := l_stmt ||' AND NVL(l.planning_priority,-99)=:planning_priority';  --p_planning_priority
   END IF;

   /* Investigate partial Reservation */

   l_stmt := l_stmt|| ' AND  l.shipped_quantity  IS NULL'||
     ' AND l.source_type_code  = '||'''INTERNAL'''||
     ' AND NVL(l.shippable_flag,'||'''N'''||')  = '||'''Y'''||
     ' AND l.ship_from_org_id   = msi.organization_id'||
     ' AND l.inventory_item_id  = msi.inventory_item_id'||
     ' AND msi.service_item_flag <> '||'''Y'''||    
     ' AND msi.reservable_type   <> 2';

   IF (g_debug <= gme_debug.g_log_statement) THEN
      FND_FILE.Put_Line(FND_FILE.LOG, 'Main syntax built now add order by clause');
   END IF;   

   l_stmt := l_stmt || ' ORDER BY l.inventory_item_id,l.ship_from_org_id,l.subinventory';
   --FND_FILE.Put_Line(FND_FILE.LOG,substr(l_stmt,1,length(l_stmt)));
   IF (g_debug <= gme_debug.g_log_statement) THEN 
     FND_FILE.Put_Line(FND_FILE.LOG, substr(l_stmt,1,length(l_stmt)));
   END IF;  

   DBMS_SQL.PARSE(l_cursor_id,l_stmt,DBMS_SQL.NATIVE);                     

   FND_FILE.Put_Line(FND_FILE.LOG, '    Query Built     = '||
                                        l_stmt     );
   IF (g_debug <= gme_debug.g_log_statement) THEN                                        
      FND_FILE.Put_Line(FND_FILE.LOG, 'GME parse done now');
      FND_FILE.Put_Line(FND_FILE.LOG, 'Start processing bind variables ');
   END IF;   

   -- ================= BIND VARIABLES ======================
   IF p_org_id IS NOT NULL THEN
      DBMS_SQL.BIND_VARIABLE(l_cursor_id,':org_id',p_org_id);
   END IF;
   IF p_line_id IS NOT NULL THEN
      DBMS_SQL.BIND_VARIABLE(l_cursor_id,':line_id',p_line_id);
   END IF;
   IF p_order_number_low IS NOT NULL THEN
      DBMS_SQL.BIND_VARIABLE(l_cursor_id,':order_number_low',p_order_number_low);
   END IF;
   IF p_order_number_high IS NOT NULL THEN
      DBMS_SQL.BIND_VARIABLE(l_cursor_id,':order_number_high',p_order_number_high);
   END IF;
   IF p_customer_id IS NOT NULL THEN
      DBMS_SQL.BIND_VARIABLE(l_cursor_id,':customer_id',p_customer_id);
   END IF;
   IF p_order_type IS NOT NULL THEN
      DBMS_SQL.BIND_VARIABLE(l_cursor_id,':order_type',p_order_type);
   END IF;
   IF l_ordered_date_low IS NOT NULL THEN
      DBMS_SQL.BIND_VARIABLE(l_cursor_id,':ordered_date_low',l_ordered_date_low);
   END IF;
   IF l_ordered_date_high IS NOT NULL THEN
      DBMS_SQL.BIND_VARIABLE(l_cursor_id,':ordered_date_high',l_ordered_date_high);
   END IF;
   IF p_line_type_id IS NOT NULL THEN
      DBMS_SQL.BIND_VARIABLE(l_cursor_id,':line_type_id',p_line_type_id);
   END IF;
   IF p_warehouse IS NOT NULL THEN
     DBMS_SQL.BIND_VARIABLE(l_cursor_id,':warehouse',p_warehouse);
   END IF;
   IF l_request_date_low IS NOT NULL THEN
    DBMS_SQL.BIND_VARIABLE(l_cursor_id,':request_date_low',l_request_date_low);
   END IF;
   IF l_request_date_high IS NOT NULL THEN
     DBMS_SQL.BIND_VARIABLE(l_cursor_id,':request_date_high',l_request_date_high);
   END IF;
   IF l_schedule_ship_date_low IS NOT NULL THEN
     DBMS_SQL.BIND_VARIABLE(l_cursor_id,':schedule_ship_date_low',l_schedule_ship_date_low);
   END IF;
   IF l_schedule_ship_date_high IS NOT NULL THEN
     DBMS_SQL.BIND_VARIABLE(l_cursor_id,':schedule_ship_date_high',l_schedule_ship_date_high);
   END IF;
   IF l_schedule_arrival_date_low IS NOT NULL THEN
     DBMS_SQL.BIND_VARIABLE(l_cursor_id,':schedule_arrival_date_low',l_schedule_arrival_date_low);
   END IF;
   IF l_schedule_arrival_date_high IS NOT NULL THEN
     DBMS_SQL.BIND_VARIABLE(l_cursor_id,':schedule_arrival_date_high',l_schedule_arrival_date_high);
   END IF;
   IF p_inventory_item_id IS NOT NULL THEN
     DBMS_SQL.BIND_VARIABLE(l_cursor_id,':inventory_item_id',p_inventory_item_id);
   END IF;
   IF p_demand_class_code IS NOT NULL THEN
     DBMS_SQL.BIND_VARIABLE(l_cursor_id,':demand_class_code',p_demand_class_code);
   END IF;
   IF p_planning_priority IS NOT NULL THEN
     DBMS_SQL.BIND_VARIABLE(l_cursor_id,':planning_priority',p_planning_priority);
   END IF;
   --R12.MOAC
   
   IF (g_debug <= gme_debug.g_log_statement) THEN
     FND_FILE.Put_Line(FND_FILE.LOG,'bind variables done');
     FND_FILE.Put_Line(FND_FILE.LOG,'start output variables ');
   END IF;     

   -- ================= OUTPUT VARIABLES ======================
   DBMS_SQL.DEFINE_COLUMN(l_cursor_id,1,l_line_id);               

   -- =================   EXECUTE    ==========================
   IF (g_debug <= gme_debug.g_log_statement) THEN
      FND_FILE.Put_Line(FND_FILE.LOG, substr(l_stmt,1,length(l_stmt)));
      FND_FILE.Put_Line(FND_FILE.LOG, 'EXECUTE data retrieval ');
   END IF;   

   l_retval := DBMS_SQL.EXECUTE(l_cursor_id);

   -- ================= PROCESS ORDER LINES  ==================
   IF (g_debug <= gme_debug.g_log_statement) THEN
      FND_FILE.Put_Line(FND_FILE.LOG,'Start looping through rows here');
   END IF;   
   LOOP
        

     IF DBMS_SQL.FETCH_ROWS(l_cursor_id) = 0 THEN        
        FND_FILE.Put_Line(FND_FILE.LOG, 'Zero order line rows to process so exit');
        EXIT;     
     END IF;
     DBMS_SQL.COLUMN_VALUE(l_cursor_id, 1, l_line_id);     
          
     IF (g_debug <= gme_debug.g_log_statement) THEN     
        FND_FILE.Put_Line(FND_FILE.LOG, '***** Processing Line id '|| l_line_id||' *****');
     END IF;   
     l_return_status := FND_API.G_RET_STS_SUCCESS;
     OE_LINE_UTIL.Lock_Row
            (p_line_id            => l_Line_id,
             p_x_line_rec         => l_line_rec,
             x_return_status      => l_return_status);

     IF l_return_status = FND_API.G_RET_STS_ERROR THEN
          FND_FILE.Put_Line(FND_FILE.LOG,'Lock row returned with error');
     END IF;

     l_line_rec.schedule_action_code := OESCH_ACT_RESERVE; -- do we need to update action code ??
     -- Make to Order Assessment                    
     -- ========================
     IF (g_debug <= gme_debug.g_log_statement) THEN
        FND_FILE.Put_Line(FND_FILE.LOG,'Determine whether this line qualifies for MTO '||l_Line_id); 
     END IF;

     IF GME_MAKE_TO_ORDER_PVT.line_qualifies_for_mto(l_line_rec.line_id) THEN   
       IF (g_debug <= gme_debug.g_log_statement) THEN    
          FND_FILE.Put_Line(FND_FILE.LOG,'Yes this line qualifies for Make to Order ');
          FND_FILE.Put_Line(FND_FILE.LOG,'Now determine if the line is eligible for reservation processing ');
       END IF;   
       Reserve_Eligible(
               p_line_rec                   => l_line_rec
              ,p_use_reservation_time_fence => 'N'               
              ,x_return_status              => l_return_status);

       IF l_return_status <> FND_API.G_RET_STS_SUCCESS THEN
         FND_FILE.Put_Line(FND_FILE.LOG,'This line is not eligible for creating reservations so cannot proceed with MTO');
         GOTO NEXT_RECORD;
       END IF;

       IF (g_debug <= gme_debug.g_log_statement) THEN
          FND_FILE.Put_Line(FND_FILE.LOG,'Lock Row for line_id '||l_Line_id);
       END IF;
       OE_LINE_UTIL.Lock_Row
            (p_line_id            => l_Line_id,
             p_x_line_rec         => l_line_rec,
             x_return_status      => l_return_status);

       IF l_return_status <> FND_API.G_RET_STS_SUCCESS THEN
         FND_FILE.Put_Line(FND_FILE.LOG,'Failure to Lock Row for line_id '||l_Line_id);
         GOTO NEXT_RECORD;
       END IF;

       /* Need to create an OPM batch */
       /* Need to reserve the order line to the new production batch */
       /* ========================================================== */      
          IF (g_debug <= gme_debug.g_log_statement) THEN
             FND_FILE.Put_Line(FND_FILE.LOG,'Proceeding with Make to Order so invoke create batch for order line here');       
          END IF;   
            
       GME_MAKE_TO_ORDER_PVT.create_batch_for_order_line(
                                   p_api_version   => 1.0          
                    --            ,p_init_msg_lst  => FND_API.G_TRUE
                    --            ,p_commit        => FND_API.G_TRUE
                                  ,p_so_line_id    => l_line_id);

       IF l_return_status <> FND_API.G_RET_STS_SUCCESS THEN       -- change create_batch above to return return_status
                 
            FND_FILE.Put_Line(FND_FILE.LOG,'Failure during create batch for order line : line is '||l_line_rec.line_id);
            
            GOTO NEXT_RECORD;         
       END IF;
     ELSE   
       IF (g_debug <= gme_debug.g_log_statement) THEN         
          FND_FILE.Put_Line(FND_FILE.LOG, '***** order line DOES NOT qualify for MAKE to ORDER '|| l_line_id||' *****');       
       END IF;  
     END IF; -- End of Line Qualifies for MAKE to ORDER

     <<NEXT_RECORD>>
     NULL;
   END LOOP;    -- End of lines_cur
   DBMS_SQL.CLOSE_CURSOR(l_cursor_id);
   
   COMMIT; 
   
  <<END_OF_PROCESS>>    
    FND_FILE.Put_Line(FND_FILE.LOG,'End of Processing for MAKE to ORDER');                                              

EXCEPTION
 WHEN OTHERS THEN
      FND_FILE.Put_Line(FND_FILE.LOG,'Inside the When Others Execption');
      FND_FILE.Put_Line(FND_FILE.LOG,substr(sqlerrm, 1, 2000));
END Make_to_order;
/*=============================================================================*/                                                        
PROCEDURE set_parameter_for_wf(                                                   
        p_itemtype        in      VARCHAR2, /* workflow item type */                                                            
        p_itemkey         in      VARCHAR2, /* sales order line id */                                                            
        p_actid           in      number,   /* ID number of WF activity */                                                       
        p_funcmode        in      VARCHAR2, /* execution mode of WF activity */                                                  
        x_result      out NoCopy  VARCHAR2  /* result of activity */                                                             
        )                                                                                                                              
IS                                                                                                                                     
        l_debug_level CONSTANT NUMBER := oe_debug_pub.g_debug_level;
        l_stmt_num        	number := 0;                                                                                     
        l_quantity        	number := 0;                                                                                     
        l_class_code      	number;                                                                                          
        l_wip_group_id    	number;                                                                                          
        l_mfg_org_id      	number;                                                                                           
        l_afas_line_id    	number;                                                                                          
        l_msg_name        	varchar2(30);                                                                                
        l_msg_txt         	varchar2(500);	                                                                                 
	  l_return_status   	varchar2(1);                                                                                     
        l_user_id         	varchar2(30);                                                                                   
        l_msg_count       	number;                                                                                      
        l_hold_result_out 	varchar2(1);                                                                                     
        l_hold_return_status  	varchar2(1);                                                                                     
        l_ato_line_id       	number;                                                                                          
        l_line_id            	number;                                                                                          
        record_locked          	exception;                                                                                       
        pragma exception_init (record_locked, -54);                                                                              
                                                                                                                                       
        l_result 		varchar2(20) := null;                                                                                  
                                                                                                                                       
	l_build_in_wip varchar2(1); --bugfix 2318060                                                                                          
                                                                                                                                       
BEGIN                                                                                                                                  
        savepoint before_process;                                                                                                
        x_result := 'FAILURE' ;                                                                                               
                                                                    
        IF (g_debug <= gme_debug.g_log_statement) THEN                                                                                     
           FND_FILE.Put_Line(FND_FILE.LOG,'set_parameter_work_order_wf: ' || 'Function Mode: ' || p_funcmode);                               
        END IF;   
        OE_STANDARD_WF.Set_Msg_Context(p_actid);                                                                                 
        if (p_funcmode = 'RUN') then                                                                                              
            wf_engine.SetItemAttrNumber(p_itemtype, p_itemkey,                                                                   
                                    'AFAS_LINE_ID', p_itemkey);                                                                   
            x_result := 'SUCCESS' ;                                                                                               
        end if; /* p_funcmode = 'RUN' */                                                                                               
        OE_STANDARD_WF.Save_Messages;                                                                                            
        OE_STANDARD_WF.Clear_Msg_Context;                                                                          
                                                                                                                                       
                                                                                                                                       
EXCEPTION                                                                                                                              
        when FND_API.G_EXC_ERROR then                                                                                                
           	FND_FILE.Put_Line(FND_FILE.LOG,'set_parameter_work_order_wf: ' || 'CTO_WORKFLOW.set_parameter_work_order_wf raised exc error. ' ||       
                            to_char(l_stmt_num) );                                                                                     
           OE_STANDARD_WF.Save_Messages;                                                                                              
           OE_STANDARD_WF.Clear_Msg_Context;                                                                                          
           x_result := 'COMPLETE:INCOMPLETE';                                                                                         
           rollback to savepoint before_process;                                                                                      
	   return;                                                                                                                            
                                                                                                                                    
                                                                                                                                       
        when FND_API.G_EXC_UNEXPECTED_ERROR then                                                                                      
           cto_msg_pub.cto_message('BOM', 'CTO_CREATE_WORK_ORDER_ERROR');                                                             
           	FND_FILE.Put_Line(FND_FILE.LOG,'set_parameter_work_order_wf: ' || 'CTO_WORKFLOW.set_parameter_work_order_wf raised unexc error. ' ||     
                            to_char(l_stmt_num) );                                                                                                                                                                            
           OE_STANDARD_WF.Save_Messages;                                                                                               
           OE_STANDARD_WF.Clear_Msg_Context;                                                                                           
           wf_core.context('CTO_WORKFLOW', 'set_parameter_work_order_wf',                                                              
                           p_itemtype, p_itemkey, to_char(p_actid), p_funcmode);                                                       
           raise;                                                                                                                      
                                                                                                                                       
                                                                                                                                       
         when NO_DATA_FOUND then                                                                                                       
              OE_STANDARD_WF.Save_Messages;                                                                                            
              OE_STANDARD_WF.Clear_Msg_Context;                                                                                        
		-- Set the result to INCOMPLETE so that the wf returns to Create Supply Order Eligible                                               
              x_result := 'COMPLETE:INCOMPLETE';                                                                                       
	      return;                                                                                                                         
                                                                                                                                       
         when OTHERS then                                                                                                              
	                                                        
              	FND_FILE.Put_Line(FND_FILE.LOG,'set_parameter_work_order_wf: ' || 'CTO_WORKFLOW.set_parameter_for_wf: '                        
                               || to_char(l_stmt_num) || ':' ||                                                                        
                               substrb(sqlerrm, 1, 100));                                                                              
              wf_core.context('CTO_WORKFLOW', 'set_parameter_work_order_wf',                                                           
                              p_itemtype, p_itemkey, to_char(p_actid),                                                                 
                              p_funcmode);                                                                                             
                                                                                                                                       
              raise;                                                                                                                   
                                                                                                                                       
END set_parameter_for_wf;                                                                                                       
                                  

END GME_RESERVE_CONC;
/
COMMIT;
-- show errors;
 EXIT;

