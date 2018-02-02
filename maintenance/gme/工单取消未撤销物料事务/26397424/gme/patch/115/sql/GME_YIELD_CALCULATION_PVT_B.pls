REM dbdrv: sql ~PROD ~PATH ~FILE none none none package &phase=plb \
REM dbdrv: checkfile(120.0.12010000.75=120.0.12020000.47):~PROD:~PATH:~FILE 
SET VERIFY OFF
WHENEVER SQLERROR EXIT FAILURE ROLLBACK;
WHENEVER OSERROR  EXIT FAILURE ROLLBACK;
REM +======================================================================+    
REM | Copyright (c) 1999, 2017 Oracle Corporation Redwood Shores, California, USA|    
REM |                       All rights reserved.                           |    
REM +======================================================================+    
REM                                                                             
REM
REM FILE NAME: 		GME_YIELD_CALCULATION_PVT_B.pls
REM
REM PACKAGE NAME:	GME_YIELD_CALCULATION_PVT
REM
REM DESCRIPTION:	This package will act as a gatekeeper to call any IOT package for GME. 
REM
REM
REM   
REM  Sachin Ingle      29-Jul-2015		Initial Implementation 	- This will be a stubbed version.

-- ======================================================================================================

SET VERIFY OFF
WHENEVER SQLERROR EXIT FAILURE ROLLBACK;
WHENEVER OSERROR  EXIT FAILURE ROLLBACK;


create or replace PACKAGE body gme_yield_calculation_pvt AS
  /* $Header: GME_YIELD_CALCULATION_PVT_B.pls 120.0.12020000.47 2017/06/23 07:24:02 shalchen noship $ */


  g_primary_product_item_id  NUMBER; 
  g_org_id NUMBER; 
  g_yield_uom VARCHAR2 (100);
  
  
   --BUG 26248999 Add function get_parent_material_line  of the specified lot
   FUNCTION get_parent_material_line(p_inventory_item_id IN NUMBER,
                                     p_organization_id   IN NUMBER,
                                     p_lot_number        IN VARCHAR2)
   RETURN NUMBER;  

  /*##########################################################################################################################################
  # Name : show_log
  # Description
  #     Show Debug
  ##########################################################################################################################################*/
  PROCEDURE show_log(
      p_log_type     IN NUMBER,
      p_log_pkg_name IN VARCHAR2,
      p_log_details  IN VARCHAR2)
  IS
  BEGIN
     --BUG 23105420 comment dbms_output.put_line statement which is incorrect and not standard. 
    /*IF p_log_type = 1 THEN
      --print log in dbms_out
      dbms_output.put_line (' '||p_log_pkg_name||':'||p_log_details) ;
    ELSE*/
      --print log in fnd_log
      IF (fnd_log.level_procedure >= fnd_log.g_current_runtime_level) THEN
        fnd_log.string (fnd_log.level_procedure, p_log_pkg_name, p_log_details) ;
      END IF;
    --END IF;
  END show_log;
/*##########################################################################################################################################
# Name : conv_uom
# Description
##########################################################################################################################################*/
  FUNCTION conv_uom(
      p_item_id       IN NUMBER DEFAULT NULL,
      p_from_quantity IN NUMBER,
      p_from_unit     IN VARCHAR2,
      p_to_unit       IN VARCHAR2)
    RETURN NUMBER
  IS
    x_rate NUMBER;
    x_qty_primary_product_step_uom NUMBER;

  BEGIN
    --BEGIN BUG 22316514 If qty is zero, return the original value directly. 
    IF p_from_quantity = 0 THEN
      x_rate :=0;
      RETURN x_rate;  
    END IF;
    --END BUG 22316514 
 /*   x_rate := inv_convert.inv_um_convert_new (item_id => p_item_id, PRECISION => 5, from_quantity => p_from_quantity, from_unit => p_from_unit, to_unit => p_to_unit, from_name => NULL, to_name => NULL, capacity_type => 'U') ;
    --BEGIN BUG 22316514 
    IF x_rate = -99999 THEN
      x_rate := p_from_quantity; 
      show_log (g_log_type,'GME_YIELD_CALCULATION_PVT.conv_uom', 'Failed to convert from UOM '||p_from_unit||' to UOM '|| p_to_unit||' for item '||p_item_id);  
    END IF;*/
    
    --convert current item qty from item uom to yield uom
    x_rate := inv_convert.inv_um_convert_new (item_id => p_item_id, PRECISION => 5, 
								    from_quantity => p_from_quantity,
								    from_unit => p_from_unit, 
								    to_unit => g_yield_uom, 
								    from_name => NULL, 
								    to_name => NULL, 
								    capacity_type => 'U') ;
  
    IF x_rate = -99999 THEN
      x_qty_primary_product_step_uom := p_from_quantity;
      show_log (g_log_type,'GME_YIELD_CALCULATION_PVT.conv_uom', 'Failed to convert from UOM '||p_from_unit||' to UOM '|| p_to_unit||' for item '||p_item_id);
   	else 
   	
   	 --convert current item qty from yield item uom to step uom of the primary product

   		x_qty_primary_product_step_uom := inv_convert.inv_um_convert_new (item_id => g_primary_product_item_id,
																		   		 PRECISION => 5,
																		   		 from_quantity => x_rate, 
																		   		 from_unit => g_yield_uom, 
																		   		 to_unit => p_to_unit, 
																		   		 from_name => NULL, 
																		   		 to_name => NULL, 
																		   		 capacity_type => 'U') ;
    
	    --BEGIN BUG 22316514
	    IF x_qty_primary_product_step_uom = -99999 THEN
	      x_qty_primary_product_step_uom := p_from_quantity;
	      show_log (g_log_type,'GME_YIELD_CALCULATION_PVT.conv_uom', 'Failed to convert from UOM '||p_from_unit||' to UOM '|| p_to_unit||' for item '||p_item_id);
	    END IF;
	    
    END IF;
    
    
    
    RETURN x_qty_primary_product_step_uom;
  EXCEPTION
  WHEN OTHERS THEN
    show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.conv_uom', 'Exception is '||SQLERRM) ;
  END conv_uom ;
/*##########################################################################################################################################
# Name : yield_calc
# Description
#     1.0 main procedure to calculate step, step cumulative, step grand cumulative yield
#     1.1 batch number is passed to the procedure.
# created by sachin ingle .kpit on 11/05/2015
##########################################################################################################################################*/
  PROCEDURE yield_calc(
      p_batch_id     IN NUMBER,
      p_batchstep_id IN NUMBER DEFAULT NULL,
      p_source_type  IN VARCHAR2 DEFAULT 'CP',
      p_yield_rec OUT NOCOPY step_yield_rec)
  IS
    CURSOR batchstep_cur
    IS
      SELECT *
      FROM GME_BATCH_STEPS
      WHERE batch_id   = p_batch_id
      AND batchstep_id = NVL (p_batchstep_id, batchstep_id)
      ORDER BY batchstep_no ASC ;
    ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    --calculate material in for grand cumulative yield
    ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    CURSOR material_in_cur (p_batchstep_id NUMBER, p_batch_id NUMBER)
    IS
      SELECT gmd.material_detail_id,
        gbsi.batch_id,
        mmt.transaction_id,
        source_line_id,
        mmt.inventory_item_id,
        mtln.lot_number,
        mmt.organization_id
      FROM MTL_MATERIAL_TRANSACTIONS mmt,
      MTL_TRANSACTION_LOT_NUMBERS mtln,
        GME_MATERIAL_DETAILS gmd,
        GME_BATCH_STEP_ITEMS gbsi
      WHERE gmd.material_detail_id  = gbsi.material_detail_id
      AND gmd.material_detail_id    = mmt.trx_source_line_id -- removed outer join
      AND mmt.transaction_source_id = gmd.batch_id           -- YJ: Join was missing
      AND gmd.line_type             = - 1
      AND gbsi.batch_id = p_batch_id
      AND gbsi.batchstep_id         = p_batchstep_id
      AND mtln.transaction_id(+) = MMT.transaction_id
      and mmt.transaction_type_id in (35,43)
      AND NOT EXISTS
        (SELECT
          /*+ no_unnest */
          transaction_id1
        FROM gme_transaction_pairs
        WHERE transaction_id1 = mmt.transaction_id
        AND pair_type         = 1
        )
      AND gmd.contribute_yield_ind = 'Y' --added for bug 26073710  
    -- for items which have actual qty but do not mate record in MMT
    UNION ALL
    SELECT gmd.material_detail_id,
      gbsi.batch_id,
      NULL TRANSACTION_ID,
      NULL SOURCE_LINE_ID,
      NULL INVENTORY_ITEM_ID,
      null lot_number,
      null organization_id
    FROM gme_material_details gmd ,
      GME_BATCH_STEP_ITEMS gbsi
    WHERE gmd.material_detail_id = gbsi.material_detail_id
    AND gbsi.batchstep_id        =p_batchstep_id
    AND gbsi.batch_id = p_batch_id
    AND gmd.line_type             = - 1
		AND NOT EXISTS
      (SELECT trx_source_line_id
      FROM MTL_MATERIAL_TRANSACTIONS mmt
      WHERE gmd.material_detail_id = mmt.trx_source_line_id
      AND mmt.transaction_source_id   =p_batch_id
      AND mmt.transaction_type_id in (35,43)
      )
    AND gmd.ACTUAL_QTY >0
    AND gmd.contribute_yield_ind = 'Y' --added for bug 26073710
    --- For unassocated material, this query will return only for the first step
    UNION
    SELECT gmd.material_detail_id,
      gmd.batch_id,
      mmt.transaction_id,
      mmt.source_line_id,
      mmt.inventory_item_id,
      mtln.lot_number,
      mmt.organization_id
    FROM MTL_MATERIAL_TRANSACTIONS mmt,
      gme_material_details gmd,
      MTL_TRANSACTION_LOT_NUMBERS mtln
    WHERE batch_id = p_batch_id
    AND gmd.material_detail_id  = mmt.trx_source_line_id
    AND gmd.batch_id            = mmt.transaction_source_id  -- BUG 24457891
    AND mtln.transaction_id(+) = MMT.transaction_id
    AND mmt.transaction_type_id in (35,43)
    AND gmd.line_type             = - 1
    AND NOT exists
      (SELECT 1
      FROM gme_batch_step_items gbsi
      WHERE gbsi.batch_id =p_batch_id
      AND gmd.material_detail_id = gbsi.material_detail_id
    )
    AND gmd.contribute_yield_ind = 'Y' --added for bug 26073710
    AND GME_YIELD_CALCULATION_PVT.GET_FIRST_BATCHSTEP(p_batchstep_id) ='Y'
    AND NOT EXISTS
      (SELECT
        /*+ no_unnest */
        transaction_id1
      FROM gme_transaction_pairs
      WHERE transaction_id1 = mmt.transaction_id
      AND pair_type         = 1
      );

     
     
    v_inter_in     NUMBER;
    v_inter_out    NUMBER;
    v_material_in  NUMBER := 0;
    v_material_out NUMBER;
    v_item_id GME_MATERIAL_DETAILS.inventory_item_id%type;
    v_uom GME_MATERIAL_DETAILS.dtl_um%type;
    v_yield_uom             VARCHAR2 (100) ;
    v_temp_qty              NUMBER := 0;
    v_step_uom              VARCHAR2 (100) ;
    v_total_in              NUMBER ;
    v_total_out             NUMBER;
    v_step_yield            NUMBER;
    v_prev_step_yield       NUMBER;
    v_step_cum_yield        NUMBER;
    v_prev_step_cum_yield   NUMBER;
    v_step_grand_yield      NUMBER;
    v_prev_step_grand_yield NUMBER;
    v_rec_count             NUMBER := 0;
    v_step_cum_input        NUMBER;
    v_grand_step_cum_input  NUMBER;
    v_step_output           NUMBER;
    v_material_cum_yield    NUMBER; -- to store cumulative yield from parent batch steps
    v_actual_qty            NUMBER:=0; -- to store qty
    v_grnd_input            NUMBER := 0;
    v_actual                NUMBER :=0;
    v_step_cnt              NUMBER ;-- to store the count of parent batchstep for the current batch
    v_mmt_count             NUMBER;
    v_mtln_count NUMBER; 
    -- Variables for scrap yield calculations
    v_scrap_out                 NUMBER ;
    v_step_cum_input_2          NUMBER := 0;
    v_step_grand_input          NUMBER ;
    v_step_cum_scrap            NUMBER ;
    v_step_grand_scrap          NUMBER ;
    v_step_scrap_per            NUMBER ;
    v_step_cum_scrap_per        NUMBER ;
    v_step_cum_grand_scrap_per  NUMBER ;
    v_prev_step_scrap_per       NUMBER ;
    v_prev_step_cum_scrap_per   NUMBER ;
    v_prev_step_grand_scrap_per NUMBER ;
    v_grnd_input_2              NUMBER := 0;
    v_actual_2                  NUMBER := 0;
    v_inter_in_from_each_step   NUMBER := 0;
    actual_total_cum_yield      NUMBER ;
    v_total_out2                NUMBER := 0;
    v_scrap_total_cum_pct       NUMBER ;
    v_x                         NUMBER := 0 ;
    v_y                         NUMBER := 0;
    v_z                         NUMBER := 0;
    v_a                         NUMBER := 0;
    v_q                         NUMBER := 0;
    v_step_grand_input_part1    NUMBER := 0;
    v_step_cum_input_part_1     NUMBER := 0;
    --BEGIN BUG 24457891
    v_organization_id           NUMBER;
    v_subinventory_code         VARCHAR2(10);
    v_locator_id                NUMBER;
    v_inventory_item_id         NUMBER;
    v_batch_id                  NUMBER; 
    v_return_status             VARCHAR2(1);
    v_material_cum_yield_h      NUMBER;
    v_scrap_total_cum_pct_h     NUMBER;
    
    
    --END BUG 24457891
    
    --bug 25504743
    v_object_id NUMBER; 
    v_root_object_id NUMBER; 
    
     -- bug 25985221
    v_root_origin_txn_id        NUMBER; 
    
    v_material_detail_id        NUMBER;  --BUG 26248999

    
  BEGIN
  	
  	
  	if GME_YIELD_CALCULATION_PVT.install_check <> 'I' then  --bug 26144153
      gme_debug.put_line ('quit yield_calc GME_YIELD_CALCULATION_PVT.install_check returns N');
    	return; 
  	end if;
    -- Calculate Actual Yield for all step of a batch
    FOR batchstep_rec IN batchstep_cur
    LOOP
      v_actual_qty         := 0; -- to store qty
      v_grnd_input         := 0;
      v_material_cum_yield := 0;
      v_actual_qty         := 0;
      -----------------------------------------------------------------------------------------------------------------------------------
      -- calculate intermediate in
      -----------------------------------------------------------------------------------------------------------------------------------
      SELECT SUM (step_qty_in_to_uom)
      INTO v_inter_in
      FROM gme_batch_steps_quantity
      WHERE to_step_id = batchstep_rec.batchstep_id;
      show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.yield_calc', 'Intermediate In for Step_id '||batchstep_rec.batchstep_id ||' is := '||v_inter_in) ;
      -----------------------------------------------------------------------------------------------------------------------------------
      --calculate intermediate out
      -----------------------------------------------------------------------------------------------------------------------------------
      SELECT SUM (step_qty_in_from_uom) -- Changed for Bug 22471876
      INTO v_inter_out
      FROM gme_batch_steps_quantity
      WHERE from_step_id = batchstep_rec.batchstep_id;
      show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.yield_calc', 'Calculate intermediate out for Step_id '||batchstep_rec.batchstep_id ||' is := '||v_inter_out) ;
      -----------------------------------------------------------------------------------------------------------------------------------
      -- calculate material in for batch step
      -----------------------------------------------------------------------------------------------------------------------------------
      v_material_in := material_in (batchstep_rec.batchstep_id) ;
      show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.yield_calc', ' Material Input for Step_id '||batchstep_rec.batchstep_id ||' is := '||v_material_in) ;
      -----------------------------------------------------------------------------------------------------------------------------------
      -- calculate material out for batch step
      -----------------------------------------------------------------------------------------------------------------------------------
      v_material_out := material_out (batchstep_rec.batchstep_id) ;
      show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.yield_calc', ' Material Output for Step_id '||batchstep_rec.batchstep_id ||' is := '||v_material_out) ;
      v_total_in  := NVL (v_inter_in, 0)  + NVL (v_material_in, 0) ;
      v_total_out := NVL (v_inter_out, 0) + NVL (v_material_out, 0) ;
      -----------------------------------------------------------------------------------------------------------------------------------
      --calculate material in for grand cumulative yield
      ---scrap yield   --get the scrap out for the step
      -----------------------------------------------------------------------------------------------------------------------------------
      v_scrap_out := GME_YIELD_CALCULATION_PVT.scrap_out (batchstep_rec.batchstep_id) ;
      show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.yield_calc', ' Scrap Output for Step_id '||batchstep_rec.batchstep_id ||' is := '||v_scrap_out) ;
      show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.yield_calc', ' Calculate Material input for grand cumulative yield') ;
      -----------------------------------------------------------------------------------------------------------------------------------
      -- loop for material transaction for particular step
      -----------------------------------------------------------------------------------------------------------------------------------
      FOR material_in_rec IN material_in_cur (batchstep_rec.batchstep_id, p_batch_id )
      LOOP
        SELECT COUNT (1)
        INTO v_mmt_count
        FROM mtl_material_transactions mmt
        WHERE mmt.trx_source_line_id  = material_in_rec.material_detail_id
        AND mmt.transaction_source_id = material_in_rec.batch_id;
        IF v_mmt_count                > 0 THEN
          show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.yield_calc', 'material_in_rec.transaction_id: '
          ||material_in_rec.transaction_id||',material_in_cur.material_detail_id :'|| 
           material_in_rec.material_detail_id || ' lot_number: ' || material_in_rec.lot_number) ;
          
            -----------------------------------------------------------------------------------------------------------------------------------
            --the current loop gbs is referencing  to the current step but we need to get yields of previous steps to calculate cumulative yield and grand cumulative yield
            -----------------------------------------------------------------------------------------------------------------------------------
            
            SELECT COUNT(1) 
            INTO v_mtln_count 
            FROM mtl_transaction_lot_numbers 
            WHERE transaction_id = material_in_rec.transaction_id; 
            
            IF v_mtln_count > 0 THEN -- LOT MATERIAL TRANSACTION 
            
            BEGIN
              --BEGIN BUG 24457891	
              SELECT organization_id,subinventory_code,locator_id,inventory_item_id,transaction_source_id
                INTO v_organization_id,v_subinventory_code,v_locator_id,v_inventory_item_id,v_batch_id
                FROM mtl_material_transactions
               WHERE transaction_id = material_in_rec.transaction_id;
              
              gme_yield_customization_pvt.get_lot_yield_hook(
                  p_batch_id                =>v_batch_id,
                  p_organization_id         =>v_organization_id,
                  p_subinventory_code       =>v_subinventory_code,
                  p_locator_id              =>v_locator_id,
                  p_inventory_item_id       =>v_inventory_item_id,
                  p_lot_number              =>material_in_rec.lot_number,
                  x_actual_total_cum_yield  =>v_material_cum_yield_h,
                  x_scrap_total_cum_pct     =>v_scrap_total_cum_pct_h,
                  x_return_status           =>v_return_status);
                  
                  
             	IF NVL(v_material_cum_yield_h,-1) = -1 OR NVL(v_scrap_total_cum_pct_h,-1) = -1 THEN
              --END BUG 24457891

	
	              
	               
	               ---Bug 26248999,v_material_detail_id is the batch material id where the consumed lot is yield from if the lot is generated by batch.
	               
	               v_material_detail_id := get_parent_material_line(p_inventory_item_id =>material_in_rec.inventory_item_id,         
	                                                                 p_organization_id   =>material_in_rec.organization_id, 
	                                                                 p_lot_number        =>material_in_rec.lot_number); 
	                                                                 
	               show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.yield_calc', ' start to fetch actual cumulative yield of current lot/item/org:'
	               ||',material_in_cur.lot_number : '||material_in_rec.lot_number || ',material_in_cur.inventory_item_id : ' 
	               || material_in_rec.inventory_item_id || ' organization_id: ' || material_in_rec.organization_id 
	               || ' v_material_detail_id: ' || v_material_detail_id ) ;
	               
             
	             	IF nvl(v_material_detail_id,0) > 0 THEN 
	             		BEGIN
	             			 
	             			 SELECT actual_total_cum_yield/100,
	                    		 scrap_total_cum_pct / 100
	                    INTO v_material_cum_yield,
	                         v_scrap_total_cum_pct
	                    FROM gme_batch_step_items gbsi,
	                         gme_batch_steps gbs
	                   WHERE gbsi.material_detail_id = v_material_detail_id
	                     AND gbs.batchstep_id       = gbsi.batchstep_id 
	                     AND gbs.batch_id = gbsi.batch_id
	                     AND rownum < 2;
	                     
	                    show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.yield_calc', ' step-item association exists: v_material_cum_yield: ' 
	                    || v_material_cum_yield || ' v_scrap_total_cum_pct: ' || v_scrap_total_cum_pct) ;
	                    
	                EXCEPTION
	                  WHEN NO_DATA_FOUND THEN -- bug 25504743 no item-step association for parent batch.
	                  
	                    BEGIN   
	                    	
	                    	  SELECT actual_total_cum_yield / 100 actual_total_cum_yield,
	                               scrap_total_cum_pct / 100
	                          INTO v_material_cum_yield,
	                               v_scrap_total_cum_pct
	                          FROM gme_batch_steps gbs,
	                               gme_material_details gmd
	                         WHERE gmd.material_detail_id = v_material_detail_id
	                           AND gmd.batch_id = gbs.batch_id 
	                           AND gbs.batchstep_id = (
	                                SELECT  gs.batchstep_id
	                                FROM gme_batch_steps gs
	                                WHERE gs.batchstep_no = (
	                                        SELECT MAX(gb.batchstep_no)
	                                        FROM gme_batch_steps gb
	                                        WHERE gb.batch_id = gbs.batch_id
	                                    )
	                                AND gs.batch_id = gbs.batch_id)                           
	                            AND ROWNUM < 2;                            
	                        
	                       --END BUG 26248999  
	                       show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.yield_calc', ' none step-item association exists: v_material_cum_yield: ' 
	                       || v_material_cum_yield || ' v_scrap_total_cum_pct: ' || v_scrap_total_cum_pct) ;  
	                    	
	                    	
	                    EXCEPTION
	                 			WHEN NO_DATA_FOUND THEN
	                 			
	                 			 show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.yield_calc', 'no batch wip product/byproduct completion txn linked to the current consumed Lot '); 
	                       
	                      v_material_cum_yield := 1;
	                      v_scrap_total_cum_pct := 1 ; 
	                  	END;     	 
	                   
	             		END; 		
	             	
	             	ELSE -- lot is not yield from batch.
	             		 v_material_cum_yield := 1;
	                 v_scrap_total_cum_pct := 1 ; 
	                 
	             	END IF; 	-- nvl(v_material_detail_id,0) > 0
					  	
					  
              END IF;  --  NVL(v_material_cum_yield_h,-1) = -1 OR NVL(v_scrap_total_cum_pct_h,-1) = -1 BUG 24457891	  
              
              --If lot yield from hook procedure is not -1, then use lot yield from hook.
              IF v_material_cum_yield_h <> -1 THEN
                v_material_cum_yield := v_material_cum_yield_h;  
              END IF;
              IF v_scrap_total_cum_pct_h <> -1 THEN
                v_scrap_total_cum_pct := v_scrap_total_cum_pct_h;  
              END IF;                
              --BUG 25504743   Shaliu Chen 14-FEB-2017
              --set to 1 if variables is NULL
              v_material_cum_yield := nvl(v_material_cum_yield, 1); 
              v_scrap_total_cum_pct   := nvl(v_scrap_total_cum_pct, 1);                          
            EXCEPTION
            WHEN too_many_rows THEN
              v_material_cum_yield  := 1;
              v_scrap_total_cum_pct := 1 ;	                    
		
            WHEN OTHERS THEN
              v_material_cum_yield  := 1;
              v_scrap_total_cum_pct := 1 ;
            END;
		      ELSE -- NON LOT MATERIAL TXN;
		       		v_material_cum_yield  := 1;
		          v_scrap_total_cum_pct := 1 ;
		    	END IF;    
		          
          show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.yield_calc', 'v_material_cum_yield: '||v_material_cum_yield) ;
          -----------------------------------------------------------------------------------------------------------------------------------
          -- this query is to get the transacted converted quantity for the specific transaction.
          -----------------------------------------------------------------------------------------------------------------------------------
          BEGIN
            SELECT SUM (conv_uom (mmt.inventory_item_id, decode(mtln.lot_number,null,mmt.transaction_quantity,mtln.transaction_quantity) * -1, mmt.TRANSACTION_UOM, gbs.step_qty_um)) xx
            INTO v_actual_qty
            FROM mtl_object_genealogy mob,
              mtl_material_transactions mmt,
              mtl_transaction_lot_numbers mtln,
              gme_batch_step_items gbsi,
              gme_material_details gmd,
              gme_batch_steps gbs
            WHERE mob.origin_txn_id (+) = mmt.transaction_id
            AND mmt.trx_source_line_id  = gbsi.material_detail_id
            AND gbsi.material_detail_id = gmd.material_detail_id
            AND gbsi.batchstep_id       = gbs.batchstep_id
            AND mmt.transaction_id      = material_in_rec.transaction_id
            AND mmt.transaction_id      = mtln.transaction_id(+)  --BUG 25504743
            AND nvl(mtln.lot_number(+), '-') = nvl(material_in_rec.lot_number, '-') 
            AND mmt.transaction_type_id in (35,43)
            AND gmd.line_type           = - 1;
            
            show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.yield_calc', '1 v_actual_qty : '||v_actual_qty) ;
            IF v_actual_qty                                                               IS NULL THEN
              IF GME_YIELD_CALCULATION_PVT.GET_FIRST_BATCHSTEP(batchstep_rec.batchstep_id) ='Y' THEN
                show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.yield_calc', ' GME_YIELD_CALCULATION_PVT.GET_FIRST_BATCHSTEP(batchstep_rec.batchstep_id) : '||GME_YIELD_CALCULATION_PVT.GET_FIRST_BATCHSTEP(batchstep_rec.batchstep_id)) ;
                v_actual_qty:= GME_YIELD_CALCULATION_PVT.unassociated_material_in(material_in_rec.transaction_id,p_batch_id);
                show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.yield_calc', ' v_actual_qty : '||v_actual_qty) ;
              END IF ;
            END IF ;
          EXCEPTION
          WHEN OTHERS THEN
            show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.yield_calc', ' v_actual_qty : '||v_actual_qty) ;
            v_actual_qty:=0;
          END ;
          -----------------------------------------------------------------------------------------------------------------------------------
          --sac this code checks if there is no transaction for the material_detail_id in mmt but still the actual quantity is displayed in gmd
          -----------------------------------------------------------------------------------------------------------------------------------
        elsif v_mmt_count = 0 THEN
          SELECT actual_qty
          INTO v_actual_qty
          FROM gme_material_details
          WHERE material_detail_id = material_in_rec.material_detail_id;
          v_material_cum_yield    := 1;
          v_scrap_total_cum_pct   := 1 ;
          NULL;
        END IF ;
        show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.yield_calc', ' before divide  v_actual_qty : '||v_actual_qty||', v_grnd_input: '||v_grnd_input) ;
        --bug 26023863
        if v_material_cum_yield <> 0 then 
        	v_actual := (v_actual_qty / v_material_cum_yield) ;
        else 
        	v_actual := 0 ; 
      	end if ; 
        show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.yield_calc', ' v_actual : '||v_actual) ;
        v_grnd_input := v_grnd_input + v_actual;
        show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.yield_calc', 'v_grnd_input : '||v_grnd_input) ;
        -- for scrap calculations
        if v_material_cum_yield <> 0 then 
        	v_actual_2     := (v_actual_qty  / v_material_cum_yield) * v_scrap_total_cum_pct;
        else 
        	v_actual_2     := v_actual_qty * v_scrap_total_cum_pct ;        	
      	end if ; 
        v_grnd_input_2 := v_grnd_input_2 + v_actual_2;
      END LOOP;
      show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.yield_calc', 'total in : '||v_total_in||', total out '||v_total_out||', inter in '||v_inter_in) ;
      show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.yield_calc', 'v_total_in : '||v_total_in||', v_total_out '||v_total_out||',v_step_yield'||v_step_yield) ;
      ------------------------------------------------------------------------------------------------------------------------------------
      -- calculate step yield
      ------------------------------------------------------------------------------------------------------------------------------------
      GME_YIELD_CALCULATION_PVT.step_yield_frm (v_total_in, v_total_out, v_step_yield) ;-- calculate step  yield
      ------------------------------------------------------------------------------------------------------------------------------------
      show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.yield_calc', 'v_step_yield : '||v_step_yield) ;
      -----------------------------------------------------------------------------------------------------------------------------------
      -- code to find previous  step for current step
      -----------------------------------------------------------------------------------------------------------------------------------
      SELECT COUNT (DISTINCT from_step_id)
      INTO v_step_cnt
      FROM gme_batch_steps_quantity
      WHERE to_step_id = batchstep_rec.batchstep_id;
      IF v_step_cnt    = 0 THEN -- when there is no previous step for current step , i.e the current step is the first step in batch
        ------------------------------------------------------------------------------------------------------------------------------------
        -- calculate step cumulative yield
        ------------------------------------------------------------------------------------------------------------------------------------
        v_inter_in              := 0;
        v_prev_step_cum_yield   := 1;
        v_step_cum_input_part_1 :=0;
        GME_YIELD_CALCULATION_PVT.step_cum_yield_frm (v_inter_in, v_step_cum_input_part_1, v_material_in, v_total_out, v_step_cum_yield) ;
        show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.yield_calc', 'v_inter_in : '||v_inter_in||', v_prev_step_cum_yield '||v_prev_step_cum_yield||',v_material_in'||v_material_in|| 
         ',v_total_out :'||v_total_out||',v_step_cum_yield :'||v_step_cum_yield) ;
        ------------------------------------------------------------------------------------------------------------------------------------
        -- calculate grand step cumulative yield
        ------------------------------------------------------------------------------------------------------------------------------------
        v_inter_in              := 0;
        v_prev_step_grand_yield := 1;
        --v_step_grand_input_part1:= SUM((Input to the current step from previous step/Prev Step Cum Yield) )
        -- If there is no prev step, then it will not have any input coming from prev step, So assigning this value as Zero.
        v_step_grand_input_part1 :=0;
        --------------------------------------------------
        --API to calculate Total Cumulative Yield.
        --------------------------------------------------
        --GME_YIELD_CALCULATION_PVT.step_grand_cum_yield (v_inter_in, v_prev_step_grand_yield, v_grnd_input, v_total_out, v_step_grand_yield) ;
        GME_YIELD_CALCULATION_PVT.step_grand_cum_yield (v_inter_in, v_step_grand_input_part1, v_grnd_input, v_total_out, v_step_grand_yield) ;
        show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.yield_calc', 'v_inter_in : '||v_inter_in||', v_prev_step_grand_yield '||v_prev_step_grand_yield) ;
        show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.yield_calc', ' v_grnd_input '||v_grnd_input||',v_total_out :'||v_total_out||',v_step_grand_yield :'||v_step_grand_yield) ;
        ------------------------------------------------------------------------------------------------------------------------------------
        --scrap calculations
        ------------------------------------------------------------------------------------------------------------------------------------
        v_prev_step_cum_yield     := 1;
        v_prev_step_cum_scrap_per := 1;
        GME_YIELD_CALCULATION_PVT.step_scrap_pct (v_inter_in + v_material_in, v_scrap_out, v_step_scrap_per) ; -- Updated for Bug 22390413
        GME_YIELD_CALCULATION_PVT.step_cum_scrap_pct (v_inter_in, v_prev_step_cum_yield, v_material_in, v_prev_step_cum_scrap_per, v_scrap_out, v_step_cum_scrap_per) ;
        GME_YIELD_CALCULATION_PVT.step_grand_cum_scrap_pct (v_inter_in, v_prev_step_grand_yield, v_material_in, v_prev_step_cum_scrap_per, v_grnd_input, v_grnd_input_2, v_scrap_out, 
         v_step_cum_grand_scrap_per) ;  -- Updated for Bug 22390413
        p_yield_rec.actual_step_yield     := v_step_yield    *100;
        p_yield_rec.actual_step_cum_yield := v_step_cum_yield*100;
        show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.yield_calc', 'v_step_yield : '||v_step_yield||', v_step_cum_yield :'||v_step_cum_yield|| ', v_step_grand_yield :'||v_step_grand_yield) ;
      ELSE
        IF v_step_cnt   > 0 THEN -- when there is only one previous step to the current step
          IF v_step_cnt = 1 THEN
            ------------------------------------------------------------------------------------------------------------------------------------
            -- this query gets the yields of previous step for the current step.
            ------------------------------------------------------------------------------------------------------------------------------------
            SELECT actual_total_cum_yield/100,
              actual_step_yield          /100,
              actual_step_cum_yield      /100,
              scrap_cum_pct              /100,
              scrap_total_cum_pct        /100,
              GME_YIELD_CALCULATION_PVT.Get_inter_in_from_each_step (gbs.batchstep_id,batchstep_rec.batchstep_id ) inter_in_from_each_step
            INTO v_prev_step_grand_yield,
              v_prev_step_yield,
              v_prev_step_cum_yield,
              v_prev_step_cum_scrap_per,
              v_prev_step_grand_scrap_per ,
              v_inter_in_from_each_step
            FROM gme_batch_steps gbs
            WHERE gbs.batchstep_id = ( SELECT DISTINCT gbsq.from_step_id 
                                         FROM gme_batch_steps_quantity gbsq
                                        WHERE gbsq.to_step_id = batchstep_rec.batchstep_id); 
                                        
						if (v_prev_step_cum_yield <> 0 )  then --bug 26023863
            	v_step_cum_input_part_1  :=v_step_cum_input_part_1 +(v_inter_in_from_each_step/v_prev_step_cum_yield);
            else 
            	v_step_cum_input_part_1 := v_step_cum_input_part_1 + v_inter_in_from_each_step ;
          	end if; 
          	if v_prev_step_grand_yield <> 0 then -- bug 26023863
            	v_step_grand_input_part1 := v_step_grand_input_part1+ (v_inter_in_from_each_step/ v_prev_step_grand_yield);
            else
            	v_step_grand_input_part1 := v_step_grand_input_part1 + v_inter_in_from_each_step ;
            end if; 	
            show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.yield_calc', 'v_inter_in_from_each_step : '||v_inter_in_from_each_step||',v_prev_step_grand_yield : '||v_prev_step_grand_yield) ;
          ELSE
            IF v_step_cnt > 1 THEN -- when current step has tow parent step then the query takes the step with highest step number to get the previous step yields
              FOR step_rec                 IN
               --BUG 24405122 Add distinct to ignore duplication if there are 2 step transfer records with same from_step and to_step 
              (SELECT DISTINCT gbsq.from_step_id,
                 actual_total_cum_yield/100 actual_total_cum_yield,
                actual_step_yield           /100 actual_step_yield,
                actual_step_cum_yield       /100 actual_step_cum_yield,
                scrap_cum_pct               /100 scrap_cum_pct,
                scrap_total_cum_pct         /100 scrap_total_cum_pct,
                GME_YIELD_CALCULATION_PVT.get_step_total_out_qty (gbs.batchstep_id) total_out,
                GME_YIELD_CALCULATION_PVT.Get_inter_in_from_each_step (gbs.batchstep_id,batchstep_rec.batchstep_id ) inter_in_from_each_step
              FROM gme_batch_steps gbs,
                gme_batch_steps_quantity gbsq
              WHERE gbs.batchstep_id = gbsq.from_step_id
              AND gbsq.to_step_id    = batchstep_rec.batchstep_id
              )
              LOOP
                show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.yield_calc', 'step_rec.inter_in_from_each_step : '||step_rec.inter_in_from_each_step||', step_rec.actual_total_cum_yield '||step_rec.actual_total_cum_yield) ;
                
                if step_rec.actual_step_cum_yield <> 0 then --  Bug 26023863
                	v_step_cum_input_part_1 := v_step_cum_input_part_1+(step_rec.inter_in_from_each_step/step_rec.actual_step_cum_yield);
                else 	
                	v_step_cum_input_part_1 := v_step_cum_input_part_1+ step_rec.inter_in_from_each_step; 
                end if; 	
                --v_step_grand_input_part1:= SUM((Input to the current step from previous step/Prev Step Cum Yield) )
                if step_rec.actual_total_cum_yield <> 0 then --  Bug 26023863
                	v_step_grand_input_part1 := v_step_grand_input_part1+(step_rec.inter_in_from_each_step/ step_rec.actual_total_cum_yield );
                else 	
                	v_step_grand_input_part1 := v_step_grand_input_part1 + step_rec.inter_in_from_each_step; 
                end if; 
                show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.yield_calc', 'v_step_grand_input_part1 : '||v_step_grand_input_part1) ;
                --v_x := (step_rec.actual_step_cum_yield  * step_rec.total_out) + v_x;
                --v_y := (step_rec.actual_total_cum_yield * step_rec.total_out) + v_y;
               
                v_z := (step_rec.scrap_total_cum_pct * step_rec.total_out) + v_z;
                v_q := (step_rec.scrap_cum_pct       * step_rec.total_out) + v_q;
                v_a := step_rec.total_out            + v_a;
                /*show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.yield_calc', 'v_z :'||v_z||', step_rec.scrap_total_cum_pct :'||step_rec.scrap_total_cum_pct||',step_rec.total_out :'||
                step_rec.total_out) ;
                show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.yield_calc', 'v_x :'||v_x||', step_rec.actual_step_cum_yield :'||step_rec.actual_step_cum_yield||',step_rec.total_out :'||
                step_rec.total_out) ;
                show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.yield_calc', 'v_y :'||v_y||', step_rec.actual_step_cum_yield :'||step_rec.actual_total_cum_yield||',step_rec.total_out :'
                ||step_rec.total_out) ;*/
                show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.yield_calc', 'v_a :'||v_a||', v_total_out2 :'||v_total_out2) ;
              END LOOP;
              --v_prev_step_cum_yield       := (v_x / v_a) ;
              --v_prev_step_grand_yield     := (v_y / v_a) ;
              
              if v_a <> 0 then --Bug 26023863
              	v_prev_step_grand_scrap_per := (v_z / v_a) ;
              	v_prev_step_cum_scrap_per   := (v_q / v_a) ;
              else 
              	v_prev_step_grand_scrap_per := 0 ;
              	v_prev_step_cum_scrap_per   := 0 ;
              end if; 	
              
              --show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.yield_calc', 'v_prev_step_cum_yield :'||v_prev_step_cum_yield) ;
              --show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.yield_calc', 'v_prev_step_grand_yield :'||v_prev_step_grand_yield) ;
              show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.yield_calc', 'v_prev_step_grand_scrap_per :'||v_prev_step_grand_scrap_per) ;
              show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.yield_calc', 'v_prev_step_cum_scrap_per :'||v_prev_step_cum_scrap_per) ;
            END IF ;
          END IF ;
          show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.yield_calc', 'v_inter_in : '||v_inter_in||',v_step_grand_input_part1 '||v_step_grand_input_part1) ;
          show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.yield_calc', 'v_grnd_input : '||v_grnd_input||',v_total_out '||v_total_out) ;
          ------------------------------------------------------------------------------------------------------------------------------------
          -- step cumulative yield
          ------------------------------------------------------------------------------------------------------------------------------------
          GME_YIELD_CALCULATION_PVT.step_cum_yield_frm (v_inter_in, v_step_cum_input_part_1, v_material_in, v_total_out, v_step_cum_yield) ;
          ------------------------------------------------------------------------------------------------------------------------------------
          show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.yield_calc', 'v_step_cum_yield : '||v_step_cum_yield||',v_total_out '||v_total_out) ;
          -- grand step cumulative yield
          ------------------------------------------------------------------------------------------------------------------------------------
          --GME_YIELD_CALCULATION_PVT.step_grand_cum_yield (v_inter_in, v_prev_step_grand_yield, v_grnd_input, v_total_out, v_step_grand_yield) ;
          GME_YIELD_CALCULATION_PVT.step_grand_cum_yield (v_inter_in, v_step_grand_input_part1, v_grnd_input, v_total_out, v_step_grand_yield) ;
          show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.yield_calc', 'v_step_grand_yield : '||v_step_grand_yield) ;
          ------------------------------------------------------------------------------------------------------------------------------------
          --scrap calculations
          ------------------------------------------------------------------------------------------------------------------------------------
          GME_YIELD_CALCULATION_PVT.step_scrap_pct (v_inter_in + v_material_in, v_scrap_out, v_step_scrap_per) ; -- Updated for Bug 22390413
          GME_YIELD_CALCULATION_PVT.step_cum_scrap_pct (v_inter_in, v_prev_step_cum_yield, v_material_in, v_prev_step_cum_scrap_per, v_scrap_out, v_step_cum_scrap_per) ;
          GME_YIELD_CALCULATION_PVT.step_grand_cum_scrap_pct (v_inter_in, v_prev_step_grand_yield, v_material_in, v_prev_step_grand_scrap_per, v_grnd_input, v_grnd_input_2, v_scrap_out, 
           v_step_cum_grand_scrap_per) ;  -- Updated for Bug 22390413
        END IF ;
      END IF ;
      -- Resetting Values before next loop.
      v_step_cum_input_part_1 :=0; --05Nov
      v_step_grand_input_part1:=0; --05Nov
      --BUG 25504743 Allow Yield(Step, Cumulative and Grand Cumulative Yields ) Greater than 100%.
      --IF v_step_yield * 100    > 100 THEN
      --  v_step_yield          := 100;
      --ELSE
        v_step_yield := v_step_yield * 100;
      --END IF ;
      --IF v_step_cum_yield * 100 > 100 THEN
      --  v_step_cum_yield       := 100;
      --ELSE
        v_step_cum_yield := v_step_cum_yield * 100;
      --END IF ;
      --IF v_step_grand_yield * 100 > 100 THEN
      --  v_step_grand_yield       := 100;
      --ELSE
        v_step_grand_yield := v_step_grand_yield * 100;
      --END IF ;
      --IF v_step_scrap_per * 100 > 100 THEN
      --  v_step_scrap_per       := 100;
      --ELSE
        v_step_scrap_per := v_step_scrap_per * 100;
      --END IF ;
      --IF v_step_cum_scrap_per * 100 > 100 THEN
      --  v_step_cum_scrap_per       := 100;
      --ELSE
        v_step_cum_scrap_per := v_step_cum_scrap_per * 100;
      --END IF ;
      --IF v_step_cum_grand_scrap_per * 100 > 100 THEN
      --  v_step_cum_grand_scrap_per       := 100;
      --ELSE
        v_step_cum_grand_scrap_per := v_step_cum_grand_scrap_per * 100;
      --END IF ;
      show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.yield_calc', 'v_step_yield : '||v_step_yield||', v_step_cum_yield :'||v_step_cum_yield|| ', v_step_grand_yield :'||v_step_grand_yield) ;
      IF p_source_type = 'CP' THEN
        UPDATE gme_batch_steps
        SET actual_step_yield    = v_step_yield,
          actual_step_cum_yield  = v_step_cum_yield,
          actual_total_cum_yield = v_step_grand_yield,
          scrap_pct              = v_step_scrap_per,
          scrap_cum_pct          = v_step_cum_scrap_per,
          scrap_total_cum_pct    = NULL
          --BUG 25718309 remove who columns
         /* last_update_date       = sysdate,
          last_updated_by        = TO_NUMBER (FND_PROFILE.VALUE ('USER_ID')),
          last_update_login      = TO_NUMBER (FND_PROFILE.VALUE ('LOGIN_ID')) */
        WHERE batchstep_id       = batchstep_rec.batchstep_id;
      END IF ;
      ------------------------------------------------------------------------------------------------------------------------------------
      --assigning values to the record type
      ------------------------------------------------------------------------------------------------------------------------------------
      p_yield_rec.batchstep_id           := batchstep_rec.batchstep_id;
      p_yield_rec.actual_total_cum_yield := v_step_grand_yield;
      p_yield_rec.actual_step_yield      := v_step_yield;
      p_yield_rec.actual_step_cum_yield  := v_step_cum_yield;
      p_yield_rec.scrap_pct              := v_step_scrap_per;
      p_yield_rec.scrap_cum_pct          := v_step_cum_scrap_per;
      p_yield_rec.scrap_total_cum_pct    := v_step_cum_grand_scrap_per;
      p_yield_rec.inter_in               := v_inter_in;
      p_yield_rec.inter_out              := v_inter_out;
      show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.yield_calc', 'p_yield_rec.actual_total_cum_yield  : '||p_yield_rec.actual_total_cum_yield ||', p_yield_rec.actual_step_yield  :'||p_yield_rec.actual_step_yield 
      		|| ', p_yield_rec.actual_step_cum_yield  :'||p_yield_rec.actual_step_cum_yield ) ;
	  
	  -- Added for Bug 22384527
	  fnd_file.put_line(fnd_file.Log,
	    '  Batch Step No. = ' || batchstep_rec.batchstep_no ||
		' Actual Step Yield = ' ||  ROUND(p_yield_rec.actual_step_yield, 5) || 
		' Actual Step Cumulative Yield = ' ||  ROUND(p_yield_rec.actual_step_cum_yield, 5) ||
		' Actual Step Total Cumulative Yield = ' || ROUND(p_yield_rec.actual_total_cum_yield, 5) ||
		' Scrap Percentage = ' || ROUND(p_yield_rec.scrap_pct, 5) ||
		' Scrap Cumulative Percentage = ' || ROUND(p_yield_rec.scrap_cum_pct, 5) ||
		' Scrap Total Cumulative Percentage = ' || ROUND(p_yield_rec.scrap_total_cum_pct, 5));
	  
	  
    END LOOP;
    show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.yield_calc', 'end of calculations for batch yields ') ;
  EXCEPTION
  WHEN OTHERS THEN
    show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.yield_calc', 'Exception is '||SQLERRM) ;
  END yield_calc;
/*###############################################################
# name
# get_inter_in
# synopsis
# func  get_inter_in
# description
#    1.0 calculate inter in for a batch step , pass batchstep id to calculate
# created by sachin ingle .kpit on 23/06/2015
####################################################################*/
  FUNCTION get_inter_in(
      p_batchstep_id IN gme_batch_steps.batchstep_id%type)
    RETURN NUMBER
  IS
    v_inter_in NUMBER ;
  BEGIN
    SELECT SUM (step_qty_in_to_uom)
    INTO v_inter_in
    FROM gme_batch_steps_quantity
    WHERE to_step_id = p_batchstep_id;
    RETURN NVL (v_inter_in, 0) ;
  EXCEPTION
  WHEN OTHERS THEN
    show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.get_inter_in', 'Exception is '||SQLERRM) ;
  END get_inter_in;
/*###############################################################
# name
# get_inter_in
# synopsis
# func  get_inter_in
# description
#    1.0 calculate inter in for a batch step , pass batchstep id to calculate
# created by sachin ingle .kpit on 23/06/2015
####################################################################*/
  FUNCTION Get_inter_in_from_each_step(
      p_from_batchstep_id IN gme_batch_steps.batchstep_id%type,
      p_to_batchstep_id   IN gme_batch_steps.batchstep_id%type)
    RETURN NUMBER
  IS
    v_inter_in NUMBER ;
  BEGIN
    SELECT SUM (step_qty_in_to_uom)
    INTO v_inter_in
    FROM gme_batch_steps_quantity
    WHERE from_step_id = p_from_batchstep_id
    AND to_step_id     =p_to_batchstep_id;
    RETURN NVL (v_inter_in, 0) ;
  EXCEPTION
  WHEN OTHERS THEN
    show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.get_inter_in', 'Exception is '||SQLERRM) ;
  END Get_inter_in_from_each_step;
/*###############################################################
# name
# get_inter_in
# synopsis
# func  get_inter_in
# description
#     calculate inter out  for a batch step, pass batchstep id as parameter
# created by sachin ingle .kpit on 23/06/2015
#################################################################*/
  FUNCTION get_inter_out(
      p_batchstep_id IN gme_batch_steps.batchstep_id%type)
    RETURN NUMBER
  IS
    v_inter_out NUMBER ;
  BEGIN
    SELECT SUM (step_qty_in_from_uom) -- Changed for Bug 22471876
    INTO v_inter_out
    FROM gme_batch_steps_quantity
    WHERE from_step_id = p_batchstep_id;
    RETURN NVL (v_inter_out, 0) ;
  EXCEPTION
  WHEN OTHERS THEN
    show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.get_inter_out', 'Exception is '||SQLERRM) ;
  END get_inter_out;
/*###############################################################
# name
# get_step_total_in_qty
# synopsis
# func  get_step_total_in_qty
# description
#     1.0 total in quantity is intermediate in + material in for the specific batchstep id
# created by sachin ingle .kpit on 11/05/2015
###############################################################*/
  FUNCTION get_step_total_in_qty(
      p_batchstep_id IN gme_batch_steps.batchstep_id%type)
    RETURN NUMBER
  IS
    v_inter_in    NUMBER;
    v_total_in    NUMBER ;
    v_material_in NUMBER := 0;
    v_item_id gme_material_details.inventory_item_id%type;
    v_uom gme_material_details.dtl_um%type;
    v_yield_uom             VARCHAR2 (100) ;
    v_temp_qty              NUMBER := 0;
    v_step_uom              VARCHAR2 (100) ;
    v_check_first_step      VARCHAR2(100);
    v_batch_id              NUMBER ;
    v_unassociated_material NUMBER :=0;
    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    --cursor for material in
    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    --BUG 23716981  20-Oct-2016 Shaliu Chen
    --Add condition batch_id to solve performance issue.
    CURSOR material_in_cur (p_batchstep_id NUMBER,l_batch_id NUMBER)
    IS
      SELECT NVL (c.actual_qty, 0) actual_qty,
        c.inventory_item_id item_id,
        c.dtl_um uom
      FROM gme_batch_step_items a,
        gme_material_details c
      WHERE 1                    = 1
      AND c.batch_id             = a.batch_id
      AND a.batchstep_id         = p_batchstep_id
      AND a.batch_id             = l_batch_id
      AND c.material_detail_id   = a.material_detail_id
      AND c.line_type            = - 1
      AND c.contribute_yield_ind = 'Y';
  BEGIN
    SELECT batch_id
    INTO v_batch_id
    FROM gme_batch_steps
    WHERE batchstep_id = p_batchstep_id;
    -- check if the batchstep is the first step this is for unassociated material.
    v_check_first_step:= GET_FIRST_BATCHSTEP(p_batchstep_id);
    show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.get_step_total_in_qty', 'func / begin  calculation for  total in  ') ;
    --BUG 22980052 Add NVL to avoid entering into exception.
    SELECT NVL(SUM (step_qty_in_to_uom),0)
    INTO v_inter_in
    FROM gme_batch_steps_quantity
    WHERE to_step_id = p_batchstep_id;
    FOR material_in_rec IN material_in_cur (p_batchstep_id,v_batch_id)
    LOOP
      v_temp_qty := material_in_rec.actual_qty;
      v_item_id  := material_in_rec.item_id;
      v_uom      := material_in_rec.uom;
      SELECT step_qty_um
      INTO v_step_uom
      FROM gme_batch_steps
      WHERE batchstep_id = p_batchstep_id;
      v_material_in     := NVL (v_material_in, 0) + conv_uom (v_item_id, v_temp_qty, v_uom, v_step_uom) ;
    END LOOP;
    IF v_check_first_step     ='Y' THEN
      v_unassociated_material:=unassociated_material_in(v_batch_id);
      v_material_in          :=v_material_in+NVL(v_unassociated_material,0);
    END IF ;
    v_total_in := NVL (v_inter_in, 0) + NVL (v_material_in, 0) ;
    RETURN v_total_in;
    show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.get_step_total_in_qty', 'end calculation for  total in  ') ;
  EXCEPTION
  WHEN OTHERS THEN
    --BUG 22980052 return 0 if an exception is occurred.
    RETURN 0;
    show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.get_step_total_in_qty', 'Exception is '||SQLERRM) ;
  END get_step_total_in_qty;
/*###############################################################
# name
# get_step_total_out_qty
# synopsis
# func  get_step_total_out_qty
# description
#     1.0 total out is intermediate  out + material out
# created by sachin ingle .kpit on 11/05/2015
###############################################################*/
  FUNCTION get_step_total_out_qty(
      p_batchstep_id IN gme_batch_steps.batchstep_id%type)
    RETURN NUMBER
  IS
    v_inter_out NUMBER;--(10,5);
    v_total_out NUMBER;-- (10,5);
    --v_material_out      number(10,5):=0;
    v_material_out NUMBER := 0;
    v_item_id gme_material_details.inventory_item_id%type;
    v_uom gme_material_details.dtl_um%type;
    v_yield_uom             VARCHAR2 (100) ;
    v_temp_qty              NUMBER := 0;
    v_step_uom              VARCHAR2 (100) ;
    v_rate                  NUMBER;
    v_check_last_step       VARCHAR2(100);
    v_unassociated_material NUMBER :=0 ;
    v_batch_id              NUMBER;
    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    -- cursor for step item quantity
    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    --BUG 23716981  20-Oct-2016 Shaliu Chen
    --Add condition batch_id to solve performance issue.
    CURSOR material_out_cur (p_batchstep_id NUMBER,l_batch_id NUMBER)
    IS
      SELECT NVL (c.actual_qty, 0) actual_qty,
        c.inventory_item_id item_id,
        c.dtl_um uom
      FROM gme_batch_step_items a,
        gme_material_details c
      WHERE 1                           = 1
      AND c.batch_id                    = a.batch_id
      AND a.batchstep_id                = p_batchstep_id
      AND a.batch_id                    = l_batch_id
      AND c.material_detail_id          = a.material_detail_id
      AND c.line_type                  <> - 1
      AND NVL (by_product_type, '-99') <> 'W';
  BEGIN
    SELECT batch_id
    INTO v_batch_id
    FROM gme_batch_steps
    WHERE batchstep_id = p_batchstep_id;
    -- check if the batchstep is the first step this is for unassociated material.
    v_check_last_step:= GET_LAST_BATCHSTEP(p_batchstep_id);
    show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.get_step_total_out_qty', 'begin  calculation for  total out ') ;
    --BUG 22980052 Add NVL to avoid entering into exception.
    SELECT NVL(SUM (step_qty_in_from_uom),0) -- Changed for Bug 22471876
    INTO v_inter_out
    FROM gme_batch_steps_quantity
    WHERE from_step_id = p_batchstep_id;
    FOR material_out_rec IN material_out_cur (p_batchstep_id,v_batch_id)
    LOOP
      v_temp_qty := material_out_rec.actual_qty;
      v_item_id  := material_out_rec.item_id;
      v_uom      := material_out_rec.uom;
      SELECT step_qty_um
      INTO v_step_uom
      FROM gme_batch_steps
      WHERE batchstep_id = p_batchstep_id;
      
	  v_material_out := NVL (v_material_out, 0) + conv_uom(v_item_id,v_temp_qty,v_uom,v_step_uom); -- Changed For Bug 22383544
    END LOOP;
    IF v_check_last_step      ='Y' THEN
      v_unassociated_material:=unassociated_material_out(v_batch_id);
      v_material_out         :=v_material_out+v_unassociated_material;
    END IF ;
    v_total_out := NVL (v_inter_out, 0) + NVL (v_material_out, 0) ;
    RETURN v_total_out;
    show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.get_step_total_out_qty', 'End  calculation for  total out ') ;
  EXCEPTION
  WHEN OTHERS THEN
    --BUG 22980052 return 0 if an exception is occurred.
    RETURN 0;
    show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.get_step_total_out_qty', 'Exception is '||SQLERRM) ;
  END get_step_total_out_qty;
/*###############################################################
# name
# material_in
# synopsis
# func  material_in
# description
#     1.0 material out is the quantity input (ingredient)  in the particular step.
# created by sachin ingle .kpit on 11/05/2015
###############################################################*/
  FUNCTION material_in(
      p_batchstep_id IN gme_batch_steps.batchstep_id%type)
    RETURN NUMBER
  IS
    v_inter_in    NUMBER := 0;
    v_total_in    NUMBER ;
    v_material_in NUMBER := 0;
    v_item_id gme_material_details.inventory_item_id%type;
    v_uom gme_material_details.dtl_um%type;
    v_yield_uom             VARCHAR2 (100) ;
    v_temp_qty              NUMBER := 0;
    v_step_uom              VARCHAR2 (100) ;
    v_check_first_step      VARCHAR2(100);
    v_unassociated_material NUMBER:=0 ;
    v_batch_id              NUMBER;
    v_conv_qty              NUMBER :=0;
    v_org_id 								NUMBER;
     
    --BUG 23716981  20-Oct-2016 Shaliu Chen
    --Add condition batch_id to solve performance issue.
    CURSOR material_in_cur (p_batchstep_id NUMBER,l_batch_id NUMBER)
    IS
      SELECT NVL (c.actual_qty, 0) actual_qty,
        c.inventory_item_id item_id,
        c.dtl_um uom
      FROM gme_batch_step_items a,
        gme_material_details c
      WHERE 1                    = 1
      AND c.batch_id             = a.batch_id
      AND a.batchstep_id         = p_batchstep_id
      AND a.batch_id             = l_batch_id
      AND c.material_detail_id   = a.material_detail_id
      AND c.line_type            = - 1
      AND c.contribute_yield_ind = 'Y';
  BEGIN
    show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.material_in', 'begin  calculation for  material in ') ;
   
   
   	SELECT gbs.batch_id, gbh.organization_id, 
			grvr.INVENTORY_ITEM_ID
		  INTO v_batch_id, v_org_id, g_primary_product_item_id 
		FROM gme_batch_steps gbs,
			gme_batch_header gbh,
			GMD_RECIPE_VALIDITY_RULES grvr 
		WHERE gbs.batchstep_id = p_batchstep_id
			AND gbs.batch_id = gbh.batch_id
			AND gbh.RECIPE_VALIDITY_RULE_ID = grvr.RECIPE_VALIDITY_RULE_ID;
 
    if nvl(g_org_id, -1) <> v_org_id THEN 
    	g_org_id  := v_org_id;
    	g_yield_uom := get_yield_uom (g_org_id);
    end if; 
   
   
    -- check if the batchstep is the first step this is for unassociated material.
    v_check_first_step:= GET_FIRST_BATCHSTEP(p_batchstep_id);
    show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.material_in', 'v_check_first_step :'||v_check_first_step) ;
    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    -- calculate material in
    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    FOR material_in_rec IN material_in_cur (p_batchstep_id,v_batch_id)
    LOOP
      v_temp_qty := material_in_rec.actual_qty;
      v_item_id  := material_in_rec.item_id;
      v_uom      := material_in_rec.uom;
      SELECT step_qty_um
      INTO v_step_uom
      FROM gme_batch_steps
      WHERE batchstep_id = p_batchstep_id;
      show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.material_in', 'v_temp_qty :'||v_temp_qty) ;
      show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.material_in', 'v_uom :'||v_uom) ;
      show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.material_in', 'v_step_uom :'||v_step_uom) ;
      v_conv_qty   :=conv_uom (v_item_id, v_temp_qty, v_uom, v_step_uom) ;
      IF v_conv_qty = -99999 THEN
        v_conv_qty :=0;
        show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.material_in', 'v_conv_qty :'||v_conv_qty) ;
      END IF ;
      v_material_in := NVL (v_material_in, 0) + v_conv_qty;
      -- if v_material_in =-99999
      -- then v_material_in :=0;
      --   end if ;
    END LOOP;
    show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.material_in', 'v_material_in :'||v_material_in) ;
    IF v_check_first_step     ='Y' THEN
      v_unassociated_material:=unassociated_material_in(v_batch_id);
      show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.material_in', 'v_material_in :'||v_unassociated_material) ;
      v_material_in:=v_material_in+v_unassociated_material;
    END IF ;
    RETURN v_material_in;
    show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.material_in', 'end   calculation for  material in ') ;
  EXCEPTION
  WHEN OTHERS THEN
    show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.material_in', 'Exception is '||SQLERRM) ;
  END material_in;
/*###############################################################
# name
# material_out
# synopsis
# func  material_out
# description
#     1.0 the material coming out of the step is material out (product, by product) dose not include by product of type waste
# created by Sachin Ingle .kpit on 11/05/2015
###############################################################*/
  FUNCTION material_out(
      p_batchstep_id IN gme_batch_steps.batchstep_id%type)
    RETURN NUMBER
  IS
    v_total_out    NUMBER ;
    v_material_out NUMBER := 0;
    v_item_id gme_material_details.inventory_item_id%type;
    v_uom gme_material_details.dtl_um%type;
    v_yield_uom             VARCHAR2 (100) ;
    v_temp_qty              NUMBER := 0;
    v_step_uom              VARCHAR2 (100) ;
    v_check_last_step       VARCHAR2(100);
    v_unassociated_material NUMBER :=0 ;
    v_batch_id              NUMBER;
    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    -- Cursor to get total material out from the current batch step
    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    --BUG 23716981  20-Oct-2016 Shaliu Chen
    --Add condition batch_id to solve performance issue.
    CURSOR material_out_cur (p_batchstep_id IN NUMBER,l_batch_id NUMBER)
    IS
      SELECT NVL (c.actual_qty, 0) actual_qty,
        c.inventory_item_id item_id,
        c.dtl_um uom
      FROM gme_batch_step_items a,
        gme_material_details c
      WHERE 1                           = 1
      AND c.batch_id                    = a.batch_id
      AND a.batchstep_id                = p_batchstep_id
      AND a.batch_id                    = l_batch_id
      AND c.material_detail_id          = a.material_detail_id
      AND c.line_type                   in (1,2) --Bug 22318890 : Fixed the issue of By Product not being considered for yield
      AND NVL (c.by_product_type, 'Y') = 'Y'; -- Bug 22361054  : By-product type Sample, Rework and Waste should not be considered for yield
  BEGIN
    show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.material_out_cur', 'begin  calculation for  material out') ;
    SELECT batch_id
    INTO v_batch_id
    FROM gme_batch_steps
    WHERE batchstep_id = p_batchstep_id;
    -- check if the batchstep is the first step this is for unassociated material.
    v_check_last_step:= GET_LAST_BATCHSTEP(p_batchstep_id);
    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    -- calculate material in
    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    FOR material_out_rec IN material_out_cur (p_batchstep_id,v_batch_id)
    LOOP
      v_temp_qty := material_out_rec.actual_qty;
      v_item_id  := material_out_rec.item_id;
      v_uom      := material_out_rec.uom;
      SELECT step_qty_um
      INTO v_step_uom
      FROM gme_batch_steps
      WHERE batchstep_id = p_batchstep_id;
      v_material_out    := NVL (v_material_out, 0) + conv_uom (v_item_id, v_temp_qty, v_uom, v_step_uom) ;
    END LOOP;
    IF v_check_last_step      ='Y' THEN
      v_unassociated_material:=unassociated_material_out(v_batch_id);
      v_material_out         :=v_material_out+v_unassociated_material;
    END IF ;
    RETURN v_material_out;
    show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.material_out_cur', 'end calculation for material out') ;
  EXCEPTION
  WHEN OTHERS THEN
    show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.material_out', 'Exception is '||SQLERRM) ;
  END material_out;
/*###############################################################
# name
# uom_conversion
# synopsis
# func  uom_conversion
# description
#     uom conversion
# created by Sachin Ingle .kpit on 11/05/2015
###############################################################*/
  FUNCTION uom_conversion(
      item_id  IN NUMBER,
      from_uom IN VARCHAR2,
      org_id   IN NUMBER)
    RETURN NUMBER
  IS
    v_rate   NUMBER ;
    v_to_uom VARCHAR2 (100) ;
  BEGIN
    BEGIN
      show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.uom_conversion', 'Begin uom conversion') ;
      SELECT uom_code
      INTO v_to_uom
      FROM mtl_units_of_measure
      WHERE uom_class =
        (SELECT parameter_value parameter_value
        FROM gmd_parameters_dtl d,
          gmd_parameters_hdr h
        WHERE d.parameter_id  = h.parameter_id
        AND d.parameter_type  = 1
        AND h.organization_id = org_id
        AND parameter_name    = 'fm_yield_type'
        )
      AND base_uom_flag = 'Y';
    EXCEPTION
    WHEN no_data_found THEN
      v_to_uom := NULL;
    END ;
    v_rate := inv_convert.inv_um_convert_new (item_id => item_id, PRECISION => NULL, from_quantity => 1, from_unit => from_uom, to_unit => v_to_uom, from_name => NULL, to_name => NULL, capacity_type => 'u') ;
    IF v_rate = -99999 THEN
      v_rate := 1;
      show_log (g_log_type,'GME_YIELD_CALCULATION_PVT.uom_conversion', 'Failed to convert from UOM '||from_uom||' to UOM '|| v_to_uom||' for item '||item_id);  
    END IF;  
    RETURN v_rate ;
    show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.uom_conversion', 'End uom conversion') ;
  EXCEPTION
  WHEN OTHERS THEN
    show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.uom_conversion', 'Exception is '||SQLERRM) ;
  END uom_conversion;
/*###############################################################
# name
# process_item_unit_cost
# synopsis
# proc  process_item_unit_cost
# description
#     Used in OBIEE Graphs
# created by sachin ingle .kpit on 28/05/2015
###############################################################*/
  FUNCTION process_item_unit_cost(
      p_inventory_item_id IN NUMBER,
      p_organization_id   IN NUMBER,
      p_transaction_date  IN DATE)
    RETURN NUMBER
  IS
    l_total_cost NUMBER;
  BEGIN
    l_total_cost := gmf_cmcommon.process_item_unit_cost (p_inventory_item_id, p_organization_id, p_transaction_date) ;
    RETURN l_total_cost;
  EXCEPTION
  WHEN OTHERS THEN
    show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.process_item_unit_cost', 'Exception is '||SQLERRM) ;
  END process_item_unit_cost;
/*###############################################################
# name
# calc_batch_yield
# synopsis
# proc  calc_batch_yield
# description
#     calculate batch  yield
# created by sachin ingle .kpit on 28/05/2015
###############################################################*/
  PROCEDURE calc_batch_yield(
      p_batch_id IN NUMBER)
  IS
    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    -- cGet the batch step actual converted quantity, into step uom
    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    CURSOR batchstep_cur
    IS
      SELECT batchstep_id,
        conv_uom (NULL, GME_YIELD_CALCULATION_PVT.material_out (batchstep_id), step_qty_um,
        (SELECT muom.uom_code
        FROM gmd_parameters_dtl d,
          gmd_parameters_hdr h,
          mtl_units_of_measure muom,
          gme_batch_header gbh
        WHERE d.parameter_id   = h.parameter_id
        AND d.parameter_type   = 1
        AND h.organization_id  = gbh.organization_id
        AND gbh.batch_id       = p_batch_id
        AND muom.uom_class     = d.parameter_value
        AND parameter_name     = 'FM_YIELD_TYPE'
        AND muom.base_uom_flag = 'Y'
        )) material_out,
        conv_uom (NULL, GME_YIELD_CALCULATION_PVT.material_in (batchstep_id), step_qty_um,
        (SELECT muom.uom_code
        FROM gmd_parameters_dtl d,
          gmd_parameters_hdr h,
          mtl_units_of_measure muom,
          gme_batch_header gbh
        WHERE d.parameter_id   = h.parameter_id
        AND d.parameter_type   = 1
        AND h.organization_id  = gbh.organization_id
        AND gbh.batch_id       = p_batch_id
        AND muom.uom_class     = d.parameter_value
        AND parameter_name     = 'FM_YIELD_TYPE'
        AND muom.base_uom_flag = 'Y'
        )) material_in,
        actual_step_cum_yield
      FROM gme_batch_steps
      WHERE batch_id = p_batch_id;
      --  AND step_status IN(3,4);
      total_in       NUMBER := 0;
      total_out      NUMBER := 0;
      v_batch_yield  NUMBER;
      v_material_out NUMBER := 0;
      v_yield_uom    VARCHAR2 (100) ;
    BEGIN
      show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.calc_batch_yield', 'Begin batch yield calculations ') ;
      FOR batchstep_rec IN batchstep_cur
      LOOP
        total_in  := total_in  + batchstep_rec.material_in;
        total_out := total_out + batchstep_rec.material_out;
      END LOOP;
      IF (total_in = 0) THEN
        v_batch_yield  := 0;
      else 
     	 	v_batch_yield := (total_out / total_in) * 100;
      END IF ;
	  --Added for Bug 22509225
    --BUG 25504743 Allow Yield(Step, Cumulative and Grand Cumulative Yields ) Greater than 100%.
	   --IF v_batch_yield > 100 THEN
     --     v_batch_yield         := 100;
     --  END IF ;
	   
      UPDATE gme_batch_genealogy
      SET batch_yield     = v_batch_yield,
          recalculate_yield = 'N',
        last_update_date  = sysdate,
        last_updated_by   = TO_NUMBER (FND_PROFILE.VALUE ('USER_ID')),
        last_update_login = TO_NUMBER (FND_PROFILE.VALUE ('LOGIN_ID'))
      WHERE batch_id      = p_batch_id;
      show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.calc_batch_yield', 'End batch yield calculations') ;
	  fnd_file.put_line(fnd_file.Log,' *Batch Yield = ' || ROUND(v_batch_yield,5)); -- Added for Bug 22384527
    EXCEPTION
    WHEN OTHERS THEN
      show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.calc_batch_yield', 'Exception is '||SQLERRM) ;
    END calc_batch_yield;
    /*###############################################################
    # name
    # calc_plan_yield
    # synopsis
    # proc  calc_plan_yield
    # description
    #     1.0 calculate step and step cumulate planned yield
    # created by sachin ingle .kpit on 28/05/2015
    ###############################################################*/
    PROCEDURE calc_plan_yield(
        p_batch_id     IN NUMBER,
        p_batchstep_id IN NUMBER DEFAULT NULL,
        p_source_type  IN VARCHAR2 DEFAULT 'CP',
        p_yield_rec OUT nocopy step_yield_rec)
    IS
      v_inter_in                   NUMBER ;
      v_plan_yield                 NUMBER ;
      v_row_cnt                    NUMBER := 0;
      v_prev_yield                 NUMBER := 0;
      v_material_in                NUMBER := 0;
      v_plan_yield_output          NUMBER := 0;
      v_prev_step_actual_cum_yield NUMBER := 0;
      v_plan_cum_yield             NUMBER := 0;
      v_prev_plan_cum_yield        NUMBER := 0;
      v_plan_grand_yield_output    NUMBER := 0;
      v_calc_plan_cum_yield        NUMBER := 0;
      v_parent_step_cnt            NUMBER := 0;
      CURSOR batchstep_cur
      IS
        SELECT gbs.batchstep_id,
		gbs.batchstep_no,
          gbs.batch_id,
          routingstep_id,
          GME_YIELD_CALCULATION_PVT.material_in (gbs.batchstep_id) material_in,
          get_step_total_in_qty (gbs.batchstep_id) total_in,
          actual_step_cum_yield,
          actual_step_yield,
          actual_total_cum_yield
        FROM gme_batch_steps gbs
        WHERE batch_id   = p_batch_id
        AND batchstep_id = NVL (p_batchstep_id, batchstep_id)
        ORDER BY batchstep_no ASC;
    BEGIN
    	
    	
    	if GME_YIELD_CALCULATION_PVT.install_check <> 'I' then  --bug 26144153
      	gme_debug.put_line ('quit yield_calc GME_YIELD_CALCULATION_PVT.install_check returns N');
    		return; 
   		end if; 
    
      show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.calc_plan_yield', 'Begin planned yield calculations ') ;
      FOR batchstep_rec IN batchstep_cur
      LOOP
        -- get inter in
        SELECT SUM (NVL (step_qty_in_to_uom, 0))
        INTO v_inter_in
        FROM gme_batch_steps_quantity
        WHERE to_step_id = batchstep_rec.batchstep_id;
        -- get plan yield form recipe or operations else 100
        -- Yield from Recipe: Begin for recipe level yield calc
        BEGIN
          SELECT oprn_yield
          INTO v_plan_yield
          FROM gmd_recipe_routing_steps
          WHERE recipe_id =
            (SELECT recipe_id
            FROM gmd_recipe_validity_rules
            WHERE recipe_validity_rule_id =
              (SELECT recipe_validity_rule_id
              FROM gme_batch_header
              WHERE batch_id = batchstep_rec.batch_id
              )
            )
          AND routingstep_id = batchstep_rec.routingstep_id;
        EXCEPTION
        WHEN no_data_found THEN
          --  Yield from Routing: Begin for routing level yield calc
          BEGIN
            SELECT NVL (oprn_yield, 100)
            INTO v_plan_yield
            FROM fm_rout_dtl d--, --gmd_operations_vl
            WHERE d.routing_id =
              (SELECT batch_no
              FROM gme_batch_header
              WHERE batch_id = batchstep_rec.batch_id
              )
            AND routingstep_id = batchstep_rec.routingstep_id;
          EXCEPTION
          WHEN OTHERS THEN
            -- Yield from Operation: Begin for operation level yield calc
            BEGIN
              SELECT NVL (oprn_yield, 100)
              INTO v_plan_yield
              FROM gmd_operations_vl
              WHERE oprn_id =
                (SELECT oprn_id
                FROM gme_batch_steps
                WHERE batch_id   = batchstep_rec.batch_id
                AND batchstep_id = batchstep_rec.batchstep_id
                ) ;
            EXCEPTION
				WHEN OTHERS THEN
    
					v_plan_yield := 100;
			END ;
				-- End for operation level yield calc
		END ;
		-- End for routing level yield calc
	END ;
	-- End for recipe level yield calc
       
        -- Query to get Number of Parent Steps for current Step
        SELECT COUNT (DISTINCT from_step_id)
        INTO v_parent_step_cnt
        FROM gme_batch_steps_quantity
        WHERE to_step_id     = batchstep_rec.batchstep_id;
        IF v_parent_step_cnt = 0 THEN -- when there is no previous step for current step , i.e the current step is the first step in batch
          v_prev_yield      := 1;
          v_plan_cum_yield  := v_plan_yield;
          --planned output
          v_plan_yield_output       := (batchstep_rec.material_in) * (v_plan_yield) ;
          IF batchstep_rec.total_in <> 0 THEN --Bug 26023863
            v_calc_plan_cum_yield   := (v_plan_yield_output / batchstep_rec.total_in) / 100;
          ELSE
            v_calc_plan_cum_yield := 0;
          END IF ;
          show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.calc_plan_yield', 'v_plan_yield_output :'||v_plan_yield_output ||',batchstep_rec.total_in '||batchstep_rec.total_in) ;
          show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.calc_plan_yield', 'v_calc_plan_cum_yield :'||v_calc_plan_cum_yield||',batchstep_id '||batchstep_rec.batchstep_id) ;
          --update gme_batch_steps
        ELSE
          IF v_parent_step_cnt   > 0 THEN -- when there is only one previous step to the current step
            IF v_parent_step_cnt = 1 THEN
              BEGIN
                -- this query gets the yields of previous step for the current step.
                -- Adding Distinct to discard multiple transactions against same step
                SELECT DISTINCT actual_step_cum_yield,
                  planned_step_yield,
                  planned_step_cum_yield
                INTO v_prev_step_actual_cum_yield,
                  v_prev_yield,
                  v_prev_plan_cum_yield
                FROM gme_batch_steps gbs,
                  gme_batch_steps_quantity gbsq
                WHERE gbs.batchstep_id = gbsq.from_step_id
                AND gbsq.to_step_id    = batchstep_rec.batchstep_id;
              EXCEPTION
              WHEN OTHERS THEN
                show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.calc_plan_yield', 'Error is getting previous step yield'||SQLERRM) ;
              END;
            ELSE
              IF v_parent_step_cnt > 1 THEN -- when current step has two parent step then the query takes the step with highest step number to get the previous step yields
                SELECT actual_step_cum_yield,
                  planned_step_yield,
                  planned_step_cum_yield
                INTO v_prev_step_actual_cum_yield,
                  v_prev_yield,
                  v_prev_plan_cum_yield
                FROM
                  (SELECT *
                  FROM gme_batch_steps
                  WHERE batchstep_id IN
                    (SELECT from_step_id
                    FROM gme_batch_steps_quantity
                    WHERE to_step_id = batchstep_rec.batchstep_id
                    )
                  ORDER BY batchstep_no DESC
                  )
                WHERE rownum < 2 ;
              END IF ;
            END IF ;
            GME_YIELD_CALCULATION_PVT.step_plan_cum_yield (v_plan_yield, v_prev_plan_cum_yield, v_inter_in, v_prev_step_actual_cum_yield, batchstep_rec.material_in, 
              v_calc_plan_cum_yield) ;
           
            show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.calc_plan_yield', 'v_calc_plan_cum_yield :'||v_calc_plan_cum_yield||',batchstep_id '||batchstep_rec.batchstep_id) ;
          END IF;
        END IF;
        --BUG 25504743 Allow Yield(Step, Cumulative and Grand Cumulative Yields ) Greater than 100%.
        --IF (v_calc_plan_cum_yield * 100) > 100 THEN
        --  v_calc_plan_cum_yield         := 100;
        --ELSE
          v_calc_plan_cum_yield := v_calc_plan_cum_yield * 100;
        --END IF ;
        p_yield_rec.planned_step_yield     := v_plan_yield;
        p_yield_rec.planned_step_cum_yield := v_calc_plan_cum_yield;
        IF p_source_type                    = 'CP' THEN
          UPDATE gme_batch_steps
          SET planned_step_yield   = v_plan_yield,
            planned_step_cum_yield = v_calc_plan_cum_yield
            --BUG 25718309 remove who columns 
            /*last_update_date       = sysdate,
            last_updated_by        = TO_NUMBER (FND_PROFILE.VALUE ('USER_ID')),
            last_update_login      = TO_NUMBER (FND_PROFILE.VALUE ('LOGIN_ID'))*/
          WHERE batchstep_id       = batchstep_rec.batchstep_id;
        END IF ;
		-- Added for Bug 22384527
		fnd_file.put_line(fnd_file.Log,
		'  Batch Step No. = ' || batchstep_rec.batchstep_no ||
		' Planned Step Yield = ' ||  ROUND(p_yield_rec.planned_step_yield,5) || 
		' Planned Step Cumulative Yield = ' ||  ROUND(p_yield_rec.planned_step_cum_yield,5));
      END LOOP;
      show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.calc_plan_yield', 'End planned yield calculations ') ;
    EXCEPTION
    WHEN OTHERS THEN
      show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.calc_plan_yield', 'Exception is '||SQLERRM) ;
    END calc_plan_yield ;
  /*###############################################################
  # name
  # calc_grand_plan_yield
  # synopsis
  # proc  calc_grand_plan_yield
  # description
  #    1.0 calculate grand plan yield
  # created by sachin ingle .kpit on 28/05/2015
  ###############################################################*/
  PROCEDURE calc_grand_plan_yield(
      p_batch_id     IN NUMBER,
      p_batchstep_id IN NUMBER DEFAULT NULL,
      p_source_type  IN VARCHAR2 DEFAULT 'CP',
      p_yield_rec OUT nocopy step_yield_rec)
  IS
    v_inter_in                   NUMBER ;
    v_plan_yield                 NUMBER ;
    v_row_cnt                    NUMBER := 0;
    v_prev_yield                 NUMBER := 0;
    v_material_in                NUMBER := 0;
    v_plan_yield_output          NUMBER := 0;
    v_prev_step_actual_cum_yield NUMBER ;
    v_plan_cum_yield             NUMBER ;
    v_prev_plan_cum_yield        NUMBER;
    v_plan_grand_yield_output    NUMBER := 0;
    v_calc_plan_cum_yield        NUMBER := 0;
    v_mmt_count                  NUMBER ;
    i_plan_yield                 NUMBER ;
    i_plan_cum_yield             NUMBER ;
    i_actual_yield               NUMBER :=0;
    v_actual_qty                 NUMBER ;
    v_plan_total_output_part1    NUMBER :=0;
    v_plan_total_output_part2    NUMBER ;
    --v_grnd_input                   NUMBER := 0;
    v_grnd_output                  NUMBER := 0;
    v_plan_grand_step_yield        NUMBER;
    v_plan_grand_step_yield_output NUMBER;
    v_prev_grand_step_yield        NUMBER;
    v_prev_plan_grand_step_yield   NUMBER;
    v_grand_cum_input              NUMBER := 0;
    --v_cum_input                    NUMBER := 0;
    v_grnd_input                   NUMBER := 0;
    v_grand_cum_input_qty          NUMBER := 0;
    v_total_input_part2            NUMBER := 0;
    v_material_cum_yield           NUMBER := 0;
    v_parent_step_cnt              NUMBER := 0;
    v_planned_total_cum_yield      NUMBER := 0;
    v_prev_planned_total_cum_yield NUMBER := 0;
    v_prev_actual_total_cum_yield  NUMBER := 0;
    v_prev_actual_step_cum_yield   NUMBER := 0;
    v_prev_input                   NUMBER := 0;
    v_prev_total_input             NUMBER := 0;
    v_x                            NUMBER := 0;
    v_y                            NUMBER := 0;
    v_z                            NUMBER := 0;
    v_q                            NUMBER := 0;
    v_a                            NUMBER := 0;
    v_inter_in_from_each_step      NUMBER := 0;
    -- This cursor gets the step details, ordered by step no.
    CURSOR batchstep_cur
    IS
      SELECT gbs.batchstep_id,
        gbs.batch_id,
        routingstep_id,
        GME_YIELD_CALCULATION_PVT.material_in (gbs.batchstep_id) material_in,
        GME_YIELD_CALCULATION_PVT.get_step_total_in_qty (gbs.batchstep_id) total_in,
        GME_YIELD_CALCULATION_PVT.get_step_total_out_qty (gbs.batchstep_id) total_out,
        actual_step_cum_yield,
        actual_step_yield,
        actual_total_cum_yield,
		batchstep_no
      FROM gme_batch_steps gbs
      WHERE batch_id   = p_batch_id
      AND batchstep_id = NVL (p_batchstep_id, batchstep_id)
      ORDER BY batchstep_no ASC;
    -- This cursor gets the material  transaction details from mmt, for each batch step
    CURSOR material_in_cur (cp_batchstep_id NUMBER)
    IS
      SELECT gmd.material_detail_id,
        gmd.batch_id,
        actual_qty,
        transaction_id,
        source_line_id,
        ABS (transaction_quantity),
        mmt.inventory_item_id
      FROM mtl_material_transactions mmt ,
        gme_material_details gmd,
        gme_batch_step_items gbsi
      WHERE gmd.material_detail_id  = gbsi.material_detail_id
      AND gmd.material_detail_id    = mmt.trx_source_line_id -- removed outer join
      AND mmt.transaction_source_id = gmd.batch_id           -- YJ: Join was missing
      AND gbsi.batchstep_id         = cp_batchstep_id
      AND gmd.line_type             = - 1--66200;
    UNION ALL
    SELECT gmd.material_detail_id,
      gmd.batch_id,
      gmd.actual_qty,
      transaction_id,
      source_line_id,
      ABS (transaction_quantity),
      mmt.inventory_item_id
    FROM gme_material_details gmd ,
      mtl_material_transactions mmt
    WHERE gmd.material_detail_id  = mmt.trx_source_line_id -- removed outer join
    AND mmt.transaction_source_id = gmd.batch_id           -- YJ: Join was missing
    AND gmd.line_type             = - 1                    --66200;
    AND batch_id                  =
      (SELECT batch_id FROM gme_batch_steps WHERE batchstep_id =cp_batchstep_id
      )
    AND material_detail_id NOT IN
      (SELECT material_detail_id
      FROM gme_batch_step_items
      WHERE batch_id =
        (SELECT batch_id FROM gme_batch_steps WHERE batchstep_id =cp_batchstep_id
        )
      )
    AND GME_YIELD_CALCULATION_PVT.GET_FIRST_BATCHSTEP(cp_batchstep_id) ='Y';
  BEGIN
  	
  	if GME_YIELD_CALCULATION_PVT.install_check <> 'I' then  --bug 26144153
      gme_debug.put_line ('quit yield_calc GME_YIELD_CALCULATION_PVT.install_check returns N');
    	return; 
    end if; 
    
    
    Show_Log (g_log_type, 'GME_YIELD_CALCULATION_PVT.calc_grand_plan_yield', 'Begin grand planned yield calculations ') ;
    FOR batchstep_rec IN batchstep_cur
    LOOP
      v_grnd_input := 0;
      -- retrieve intermediate in for each batch step
      v_inter_in := GME_YIELD_CALCULATION_PVT.get_inter_in (batchstep_rec.batchstep_id) ;
      -- Yield from Recipe: Begin for recipe level yield calc
      BEGIN
        SELECT oprn_yield / 100
        INTO v_plan_yield
        FROM gmd_recipe_routing_steps
        WHERE recipe_id =
          (SELECT recipe_id
          FROM gmd_recipe_validity_rules
          WHERE recipe_validity_rule_id =
            (SELECT recipe_validity_rule_id
            FROM gme_batch_header
            WHERE batch_id = batchstep_rec.batch_id
            )
          )
        AND routingstep_id = batchstep_rec.routingstep_id;
      EXCEPTION
      WHEN NO_DATA_FOUND THEN
        --  Yield from Routing: Begin for routing level yield calc
        BEGIN
          SELECT NVL (oprn_yield, 100) / 100
          INTO v_plan_yield
          FROM fm_rout_dtl d--, --gmd_operations_vl
          WHERE d.routing_id =
            (SELECT batch_no
            FROM gme_batch_header
            WHERE batch_id = batchstep_rec.batch_id
            )
          AND routingstep_id = batchstep_rec.routingstep_id;
        EXCEPTION
        WHEN NO_DATA_FOUND THEN
          -- Yield from Operation: Begin for operation level yield calc
          BEGIN
            SELECT NVL (oprn_yield, 100) / 100
            INTO v_plan_yield
            FROM gmd_operations_vl
            WHERE oprn_id =
              (SELECT oprn_id
              FROM gme_batch_steps
              WHERE batch_id   = batchstep_rec.batch_id
              AND batchstep_id = batchstep_rec.batchstep_id
              ) ;
          EXCEPTION
          WHEN OTHERS THEN
            v_plan_yield := 1;
          END;
          -- End for operation level yield calc
        END ;
        -- End for routing level yield calc
      END ;
      -- End for recipe level yield call
      FOR material_in_rec IN material_in_cur (batchstep_rec.batchstep_id)
      LOOP
        SELECT COUNT (1)
        INTO v_mmt_count
        FROM mtl_material_transactions mmt
        WHERE --trx_source_line_id=material_detail_id AND
          trx_source_line_id          = material_in_rec.material_detail_id
        AND mmt.transaction_source_id = material_in_rec.batch_id;
        IF v_mmt_count                > 0 THEN
          show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.calc_grand_plan_yield', 'material_in_rec.object_id: '||material_in_rec.material_detail_id) ;
          BEGIN
            SELECT -- gbs.batchstep_id ,
              gbs.actual_total_cum_yield / 100, 
              gbs.actual_total_cum_yield / 100, --Bug 22318890 - Fix for additional issue of Total cum planned yield.
              gbs.PLANNED_TOTAL_CUM_YIELD/ 100, --Bug 22318890 - Fix for additional issue of Total cum planned yield.
              gbs.planned_step_cum_yield / 100
            INTO v_material_cum_yield,
              i_actual_yield,
              i_plan_yield,
              i_plan_cum_yield
              --,  gbs.*
            FROM mtl_object_genealogy mog,
              mtl_material_transactions mmt,
              gme_material_details gmd,
              gme_batch_step_items gbsi,
              gme_batch_steps gbs
            WHERE 1                       = 1
            AND mog.origin_txn_id         = mmt.transaction_id
            AND mmt.trx_source_line_id    = gmd.material_detail_id
            AND mmt.transaction_source_id = gmd.batch_id
            AND gmd.material_detail_id    = gbsi.material_detail_id
            AND gbsi.batchstep_id         = gbs.batchstep_id
            AND mog.parent_object_id      =
              (SELECT mog.object_id
              FROM mtl_material_transactions mmt,
                gme_material_details gmd,
                mtl_object_genealogy mog
              WHERE mmt.transaction_id      = mog.origin_txn_id
              AND mmt.trx_source_line_id    = gmd.material_detail_id
              AND mmt.transaction_source_id = gmd.batch_id
              AND gmd.line_type             = - 1
              AND gmd.material_detail_id    = material_in_rec.material_detail_id
              ) ;
          EXCEPTION
          WHEN no_data_found THEN
            i_actual_yield       :=1;
            i_plan_yield         := 1;
            i_plan_cum_yield     := 1;
            v_material_cum_yield := 1;
          WHEN OTHERS THEN
            -- YJ: Todo: Need to write code, above sub-query returning multiple row and causing error.
            show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.calc_grand_plan_yield', 'Exception in getting actual and planned yield : '||SQLERRM) ;
          END;
          BEGIN
            SELECT SUM (conv_uom (mmt.inventory_item_id, gmd.actual_qty, gmd.dtl_um, gbs.step_qty_um)) xx
            INTO v_actual_qty
            FROM mtl_object_genealogy mob,
              mtl_material_transactions mmt,
              gme_batch_step_items gbsi,
              gme_material_details gmd,
              gme_batch_steps gbs
            WHERE mob.origin_txn_id                                                        = mmt.transaction_id
            AND mmt.trx_source_line_id                                                     = gbsi.material_detail_id
            AND mmt.transaction_source_id                                                  = gmd.batch_id
            AND gbsi.material_detail_id                                                    = gmd.material_detail_id
            AND gbsi.batchstep_id                                                          = gbs.batchstep_id
            AND mmt.trx_source_line_id                                                     = material_in_rec.material_detail_id
            AND gmd.line_type                                                              = - 1;
            IF v_actual_qty                                                               IS NULL THEN
              IF GME_YIELD_CALCULATION_PVT.GET_FIRST_BATCHSTEP(batchstep_rec.batchstep_id) ='Y' THEN
                show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.yield_calc', ' GME_YIELD_CALCULATION_PVT.GET_FIRST_BATCHSTEP(batchstep_rec.batchstep_id) : '||GME_YIELD_CALCULATION_PVT.GET_FIRST_BATCHSTEP(batchstep_rec.batchstep_id)) ;
                v_actual_qty:= GME_YIELD_CALCULATION_PVT.unassociated_material_in(material_in_rec.transaction_id,p_batch_id);
                show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.yield_calc', ' v_actual_qty : '||v_actual_qty) ;
              END IF ;
            END IF ;
          EXCEPTION
          WHEN OTHERS THEN
            show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.calc_grand_plan_yield', 'Exception in getting Actual Qty from MMT: '||SQLERRM) ;
          END;
        elsif v_mmt_count = 0 THEN
          SELECT actual_qty
          INTO v_actual_qty
          FROM gme_material_details
          WHERE material_detail_id = material_in_rec.material_detail_id;
          i_actual_yield          := 1;
          i_plan_yield            := 1;
          i_plan_cum_yield        := 1;
          v_material_cum_yield    := 1;
        END IF ;
        Show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.calc_grand_plan_yield', ' v_actual_qty : '||v_actual_qty||',i_plan_yield :'||
         i_plan_yield||',i_plan_cum_yield'|| i_plan_cum_yield||',v_material_cum_yield:'||v_material_cum_yield) ;
        -- Following formula is used for Output.
        -- Step Plan grand cum Output :=(Actual Input to the current step * (Prev step Plan grand cum Yield/Prev Step Actual grand Cum Yield) + ((external input/incoming actual yield)
        -- * incoming plan yield))* Current Step's plan yield
        -- v_plan_total_output_part2 = ((external input/incoming actual yield)* incoming plan yield))
        
        if i_actual_yield <> 0 then --Bug 26023863
        	v_plan_total_output_part2 := (v_actual_qty / i_actual_yield) * i_plan_yield;
        else 
        	v_plan_total_output_part2 := v_actual_qty * i_plan_yield;	
        end if; 	
        -- Following formula is used for Input
        -- SUM((Input to the current step from previous step/Prev Step Cum Yield) )+ sum( external input/incoming yield)
        -- SUM((0/0 )+ sum( external input/incoming yield)
        if v_material_cum_yield <> 0 then --Bug 26023863
        	v_total_input_part2 := (v_actual_qty / v_material_cum_yield) ;
        else 
        	v_total_input_part2 := v_actual_qty ;
      	end if ;
      	
        show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.calc_grand_plan_yield', ' v_plan_total_output_part2 : '||v_plan_total_output_part2) ;
        v_grnd_output := v_grnd_output + v_plan_total_output_part2;
        v_grnd_input  := v_grnd_input  + v_total_input_part2;
        show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.calc_grand_plan_yield', 'v_grnd_input : '||v_grnd_input) ;
        show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.calc_grand_plan_yield', 'v_grnd_output : '||v_grnd_output) ;
      END LOOP;
      -- This query Counts the parent batch step for the current step.
      SELECT COUNT (DISTINCT from_step_id)
      INTO v_parent_step_cnt
      FROM gme_batch_steps_quantity
      WHERE to_step_id = batchstep_rec.batchstep_id;
      -- When the current batch dose not have any parent batch step  i.e its the first step in
      IF v_parent_step_cnt = 0 THEN
        show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.calc_grand_plan_yield', ', batchstep_rec.batchstep_id :'|| batchstep_rec.batchstep_id) ;
        show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.calc_grand_plan_yield', ' v_grnd_output : '||v_grnd_output||',v_plan_yield'||v_plan_yield) ;
        v_plan_grand_step_yield_output := v_grnd_output * v_plan_yield;
        --assign for previous values
        
        if v_grnd_input <> 0 then --Bug 26023863
       	 v_plan_grand_step_yield := v_plan_grand_step_yield_output / v_grnd_input;
        else  
        	v_plan_grand_step_yield := 0; 
        end if; 
        
        show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.calc_grand_plan_yield', ' v_plan_grand_step_yield : '||v_plan_grand_step_yield) ; --
        -- YJ:Todo: Why do we need SAME update statement 2 time in this API?
        --update gme_batch_step
      ELSE
        -- when the current step have one or more parents
        IF v_parent_step_cnt   > 0 THEN
          IF v_parent_step_cnt = 1 THEN
            show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.calc_grand_plan_yield', ', batchstep_rec.batchstep_id :'|| batchstep_rec.batchstep_id) ;
            BEGIN
              -- this query gets the yields of previous step for the current step.
              -- Adding Distinct to discard multiple transactions against same step
              SELECT DISTINCT planned_total_cum_yield/100,
                actual_total_cum_yield               /100,
                actual_step_cum_yield                / 100,
                gme_yield_calculation_pvt.Get_inter_in_from_each_step (gbs.batchstep_id,batchstep_rec.batchstep_id ) inter_in_from_each_step
              INTO v_prev_planned_total_cum_yield,
                v_prev_actual_total_cum_yield,
                v_prev_actual_step_cum_yield,
                v_inter_in_from_each_step
              FROM gme_batch_steps gbs,
                gme_batch_steps_quantity gbsq
              WHERE gbs.batchstep_id    = gbsq.from_step_id
              AND gbsq.to_step_id       = batchstep_rec.batchstep_id;
              
              if v_prev_actual_total_cum_yield <> 0 then --Bug 26023863
              	v_prev_total_input       := (v_inter_in              / v_prev_actual_total_cum_yield);
              else 
              	v_prev_total_input := v_inter_in ; 
              end if; 
              if v_prev_actual_total_cum_yield <> 0 then  --Bug 26023863
              	v_plan_total_output_part1:= v_inter_in_from_each_step*(v_prev_planned_total_cum_yield/v_prev_actual_total_cum_yield);
              else 
              	v_plan_total_output_part1 := v_inter_in_from_each_step*v_prev_planned_total_cum_yield; 
              end if ;
            EXCEPTION
            WHEN OTHERS THEN
              show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.calc_plan_yield', 'Error is getting previous step yield'||SQLERRM) ;
            END;
          ELSE
            IF v_parent_step_cnt > 1 THEN
              show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.calc_grand_plan_yield', ', batchstep_rec.batchstep_id :'|| batchstep_rec.batchstep_id) ;
              BEGIN
                -- when current step has two parent step then the query takes the step with highest step number to get the previous step yields
                FOR step_dtl_rec         IN
                  --BUG 24405122 Add distinct to ignore duplication if there are 2 step transfer records with same from_step and to_step
                (SELECT DISTINCT gbsq.from_step_id,
				  planned_step_yield/100 planned_step_yield,
				  planned_step_cum_yield  /100 planned_step_cum_yield,
                  planned_total_cum_yield /100 planned_total_cum_yield,
                  actual_step_yield       /100 actual_step_yield,
                  actual_step_cum_yield   /100 actual_step_cum_yield,
                  actual_total_cum_yield  /100 actual_total_cum_yield,
                  gme_yield_calculation_pvt.get_step_total_out_qty (gbs.batchstep_id) total_out,
                  gme_yield_calculation_pvt.Get_inter_in_from_each_step (gbs.batchstep_id,batchstep_rec.batchstep_id ) inter_in_from_each_step
                FROM gme_batch_steps gbs,
                  gme_batch_steps_quantity gbsq
                WHERE gbs.batchstep_id = gbsq.from_step_id
                AND gbsq.to_step_id    = batchstep_rec.batchstep_id
                )
                LOOP
                  --v_prev_input :=SUM((Input to the current step from previous step/Prev Step Cum Yield) )
                  
                  if step_dtl_rec.actual_total_cum_yield <> 0 then --Bug 26023863
                  	v_prev_input := step_dtl_rec.total_out/step_dtl_rec.actual_total_cum_yield;
                  else 
                  	v_prev_input := step_dtl_rec.total_out; 
                  end if; 
                  
                  v_prev_total_input := v_prev_total_input    +v_prev_input;
                  show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.calc_grand_plan_yield', ',v_prev_total_input :'||v_prev_total_input) ;
                  show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.calc_grand_plan_yield', ',batchstep_rec.batchstep_id :'||batchstep_rec.batchstep_id) ;
                  --v_plan_total_output_part1:= Sum(Actual Input to the current step * (Prev step Plan grand cum Yield/Prev Step Actual grand Cum Yield) )
                  if step_dtl_rec.actual_total_cum_yield <> 0 then --Bug 26023863
                  	v_plan_total_output_part1:= v_plan_total_output_part1 + 
											  step_dtl_rec.inter_in_from_each_step*(step_dtl_rec.planned_total_cum_yield/step_dtl_rec.actual_total_cum_yield);
									else 	
										v_plan_total_output_part1 := v_plan_total_output_part1 + step_dtl_rec.inter_in_from_each_step*step_dtl_rec.planned_total_cum_yield;	  
									end if; 			  
                  show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.calc_grand_plan_yield', ',v_plan_total_output_part1 :'||v_plan_total_output_part1) ;
                  -- This holds true for N/W rout. For each step, get yield value using weighted average.
                  -- V_X, v_y and v_z holds weighted output for different yield.
                  -- v_a contains sum of total_output for all steps.
                  -- Weighted yield will be calculated by divinding v_x or v_y or v_z with v_a
                  /*v_x := (step_dtl_rec.actual_step_cum_yield*step_dtl_rec.total_out)    + v_x;
                  v_y := (step_dtl_rec.actual_total_cum_yield*step_dtl_rec.total_out)   + v_y;
                  v_z := (step_dtl_rec.PLANNED_TOTAL_CUM_YIELD* step_dtl_rec.total_out) + v_z;
                  v_a := step_dtl_rec.total_out + v_a;
                  show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.yield_calc', 'v_z :'||v_z||', step_dtl_rec.PLANNED_TOTAL_CUM_YIELD :'||step_dtl_rec.PLANNED_TOTAL_CUM_YIELD||',step_dtl_rec.total_out :'||
                  step_dtl_rec.total_out) ;
                  show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.yield_calc', 'v_x :'||v_x||', step_dtl_rec.actual_step_cum_yield :'||step_dtl_rec.actual_step_cum_yield||',step_dtl_rec.total_out :'||
                  step_dtl_rec.total_out) ;
                  show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.yield_calc', 'v_y :'||v_y||', step_dtl_rec.actual_total_cum_yield :'||step_dtl_rec.actual_total_cum_yield||',step_dtl_rec.total_out :'
                  ||step_dtl_rec.total_out) ;
                  NULL;*/
                END LOOP;
                /*v_prev_actual_step_cum_yield   :=(v_x / v_a); -- This value is not used for now.
                v_prev_actual_total_cum_yield  :=(v_y / v_a);
                v_prev_planned_total_cum_yield :=(v_z / v_a);
                show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.yield_calc', 'v_prev_actual_step_cum_yield :'||v_prev_actual_step_cum_yield) ;
                show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.yield_calc', 'v_prev_actual_total_cum_yield :'||v_prev_actual_total_cum_yield) ;
                show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.yield_calc', 'v_prev_planned_total_cum_yield :'||v_prev_planned_total_cum_yield) ;
                */
              EXCEPTION
              WHEN OTHERS THEN
                show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.calc_grand_plan_yield', 'Exception in getting data in case of multiple parents: '||SQLERRM) ;
              END;
            END IF ;
          END IF ;
          show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.calc_grand_plan_yield', ',v_inter_in :'||v_inter_in) ;
          show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.calc_grand_plan_yield', ',v_prev_planned_total_cum_yield :'||v_prev_planned_total_cum_yield) ;
          show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.calc_grand_plan_yield', ',v_prev_actual_total_cum_yield :'||v_prev_actual_total_cum_yield) ;
          show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.calc_grand_plan_yield', ',v_prev_actual_step_cum_yield :'||v_prev_actual_step_cum_yield) ;
          show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.calc_grand_plan_yield', ' v_prev_plan_grand_step_yield:' ||v_prev_plan_grand_step_yield 
           ||',v_grnd_output :'||v_grnd_output|| 
           ',v_plan_yield:'||v_plan_yield) ;
          --v_plan_grand_step_yield_output := (v_inter_in * (v_prev_planned_total_cum_yield / v_prev_actual_total_cum_yield) + v_grnd_output) * v_plan_yield;
          v_plan_grand_step_yield_output := (v_plan_total_output_part1 + v_grnd_output)*v_plan_yield;
          v_grand_cum_input              := v_prev_total_input         + v_grnd_input;
          show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.calc_grand_plan_yield', ',v_grand_cum_input :'||v_grand_cum_input) ;
          show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.calc_grand_plan_yield', ',v_plan_grand_step_yield_output :'||v_plan_grand_step_yield_output) ;
          
          if v_grand_cum_input <> 0 then --Bug 26023863
         		v_plan_grand_step_yield := v_plan_grand_step_yield_output / v_grand_cum_input;
          else 
          	v_plan_grand_step_yield := 0; 
        	end if ; 
          show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.calc_grand_plan_yield', ' v_plan_grand_step_yield : '||v_plan_grand_step_yield) ;
        END IF;
      END IF;
      v_row_cnt     := v_row_cnt + 1;
      v_grnd_output := 0;
      --update gme_batch_steps
      --BUG 25504743 Allow Yield(Step, Cumulative and Grand Cumulative Yields ) Greater than 100%.
      --IF (v_plan_grand_step_yield * 100) > 100 THEN
      --  v_plan_grand_step_yield         := 100;
      --ELSE
        v_plan_grand_step_yield := v_plan_grand_step_yield * 100;
     -- END IF ;
      p_yield_rec.planned_total_cum_yield := v_plan_grand_step_yield;
      IF p_source_type                     = 'CP' THEN
        UPDATE gme_batch_steps
        SET planned_total_cum_yield = v_plan_grand_step_yield
           --BUG 25718309 remove who columns
          /*last_update_date          = sysdate,
          last_updated_by           = TO_NUMBER (FND_PROFILE.VALUE ('USER_ID')),
          last_update_login         = TO_NUMBER (FND_PROFILE.VALUE ('LOGIN_ID'))*/
        WHERE batchstep_id          = batchstep_rec.batchstep_id;
      END IF ;
	  -- Added for Bug 22384527
	  fnd_file.put_line(fnd_file.Log,
		'  Batch Step No. = ' || batchstep_rec.batchstep_no ||
		' Planned Total Cumulative Yield = ' ||  ROUND(p_yield_rec.planned_total_cum_yield,5));
		
    END LOOP;
    show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.calc_grand_plan_yield', 'End  grand planned yield calculations ') ;
  EXCEPTION
  WHEN OTHERS THEN
    show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.calc_grand_plan_yield', 'Exception is '||SQLERRM) ;
  END calc_grand_plan_yield ;
/*###############################################################
# name
# calc_heat_yield
# synopsis
# proc  calc_heat_yield
# description
#     main procedure to calculate the heat yield.
# created by Sachin Ingle .kpit on 22/06/2015
###############################################################*/
  PROCEDURE calc_heat_yield(
      p_heat_number IN VARCHAR2,
      p_heat_yield OUT NOCOPY NUMBER)
  IS
    v_unconsumned_qty           NUMBER := 0;
    v_lot_number                VARCHAR2 (100) ;
    v_inventory_item_id         NUMBER;
    v_material_detail_id        NUMBER;
    v_batch_id                  NUMBER;
    v_line_type                 NUMBER;
    v_plan_qty                  NUMBER;
    v_actual_qty                NUMBER;
    v_unconsumned_prev_qty      NUMBER := 0;
    v_unconsumned_total_qty     NUMBER := 0;
    v_comsumed_qty              NUMBER := 0;
    v_prev_consumed             NUMBER := 0;
    v_total_consumed            NUMBER := 0;
    v_taotal_uncomsumed         NUMBER := 0;
    v_taotal_prev_uncomsumed    NUMBER := 0;
    v_heat_yield                NUMBER := 0;
    v_actual_total_qty          NUMBER := 0;
    v_yield_uom                 VARCHAR2 (100) ;
    v_converted_transaction_qty NUMBER := 0;
    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    -- this cursor gets the list of batch id for a heat number
    -- the cursor dose not return any row if even a single  batch is in wip status , the heat calculation can only be done if the all the batches in heat are completed
    --
    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    CURSOR batch_struct_cur
    IS
      SELECT batch_id
      FROM gme_batch_genealogy gbg
      WHERE heat_number = p_heat_number
      AND EXISTS
        (SELECT *
        FROM gme_batch_header gbh
        WHERE gbg.batch_id    = gbh.batch_id
        --BUG 24351209 remove batch status restriction
        --AND gbh.batch_status IN (3, 4)
        )
    ORDER BY batch_id ASC;
    CURSOR batchstep_cur (p_batch_id NUMBER)
    IS
      SELECT * FROM gme_batch_steps WHERE batch_id = p_batch_id;
    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    -- this cursor gets the output from the current batch step
    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    CURSOR output_cur (p_batchstep_id NUMBER)
    IS
      SELECT
        (SELECT lot_number
        FROM mtl_lot_numbers
        WHERE gen_object_id = mog.parent_object_id
        ) lot_out,
      (SELECT lot_number FROM mtl_lot_numbers WHERE gen_object_id = mog.object_id
      ) lot_input,
      mmt.inventory_item_id,
      mmt.transaction_uom,
      mmt.transaction_quantity,
      gmd.material_detail_id
    FROM mtl_material_transactions mmt,
      mtl_object_genealogy mog,
      gme_material_details gmd,
      gme_batch_step_items gbsi
    WHERE 1                    = 1
    AND mmt.transaction_id     = mog.origin_txn_id
    AND mmt.trx_source_line_id = gmd.material_detail_id
    AND gmd.material_detail_id = gbsi.material_detail_id
    AND gbsi.batchstep_id      = p_batchstep_id
    AND gmd.line_type          = 1
    AND source_line_id        IS NULL
    AND transaction_id NOT    IN
      (SELECT source_line_id
      FROM mtl_material_transactions mmt,
        gme_batch_step_items gbsi
      WHERE mmt.trx_source_line_id = gbsi.material_detail_id
      AND gbsi.batchstep_id        = p_batchstep_id
      AND source_line_id          IS NOT NULL
      ) ;
    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    -- this cursor gets the output from the current batch step where the batch step is not liked to ingredient
    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    CURSOR output_cur_2 (p_batch_id IN NUMBER)
    IS
      SELECT
        (SELECT lot_number
        FROM mtl_lot_numbers
        WHERE gen_object_id = mog.parent_object_id
        ) lot_out,
      (SELECT lot_number FROM mtl_lot_numbers WHERE gen_object_id = mog.object_id
      ) lot_input,
      mmt.inventory_item_id,
      mmt.transaction_uom,
      mmt.transaction_quantity,
      gmd.material_detail_id
    FROM mtl_material_transactions mmt,
      mtl_object_genealogy mog,
      gme_material_details gmd
    WHERE 1                    = 1
    AND mmt.transaction_id     = mog.origin_txn_id
    AND mmt.trx_source_line_id = gmd.material_detail_id
    AND gmd.line_type          = 1
    AND gmd.batch_id           = p_batch_id
    AND source_line_id        IS NULL
    AND NOT EXISTS
      (SELECT material_detail_id
      FROM gme_batch_step_items gbsi
      WHERE gbsi.material_detail_id = gmd.material_detail_id
      ) ;
    v_output NUMBER ;
  BEGIN
    show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.calc_heat_yield', 'Begin heat  yield calculations ') ;
    FOR batch_struct_rec IN batch_struct_cur
    LOOP
      SELECT muom.uom_code
      INTO v_yield_uom
      FROM gmd_parameters_dtl d,
        gmd_parameters_hdr h,
        mtl_units_of_measure muom,
        gme_batch_header gbh
      WHERE d.parameter_id   = h.parameter_id
      AND d.parameter_type   = 1
      AND h.organization_id  = gbh.organization_id
      AND gbh.batch_id       = batch_struct_rec.batch_id
      AND muom.uom_class     = d.parameter_value
      AND parameter_name     = 'fm_yield_type'
      AND muom.base_uom_flag = 'Y';
      show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.calc_heat_yield', 'for batch id : '||batch_struct_rec.batch_id) ;
      v_unconsumned_qty := 0;
      FOR batchstep_rec IN batchstep_cur (batch_struct_rec.batch_id)
      LOOP
        show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.calc_heat_yield', ' for batchstep  id : '||batchstep_rec.batchstep_id) ;
        v_lot_number            := 0;
        v_inventory_item_id     := 0;
        v_material_detail_id    := 0;
        v_batch_id              := 0;
        v_line_type             := 0;
        v_plan_qty              := 0;
        v_actual_qty            := 0;
        v_unconsumned_total_qty := 0;
        v_unconsumned_qty       := 0;
        v_unconsumned_prev_qty  := 0;
        FOR output_rec IN output_cur (batchstep_rec.batchstep_id)
        LOOP
          v_actual_total_qty := 0;
          BEGIN
            FOR lot_item_rec IN
            (SELECT lot_number,
              mmt.inventory_item_id,
              gmd.material_detail_id,
              gmd.batch_id,
              gmd.line_type,
              gmd.plan_qty,
              mmt.transaction_uom,
              ABS (mmt.transaction_quantity) transaction_quantity--gmd.actual_qty
            FROM mtl_lot_numbers mln,
              mtl_object_genealogy mog,
              mtl_material_transactions mmt,
              gme_material_details gmd
            WHERE mln.gen_object_id       = mog.object_id
            AND mog.origin_txn_id         = mmt.transaction_id
            AND mmt.trx_source_line_id    = gmd.material_detail_id
            AND mmt.transaction_source_id = gmd.batch_id
            AND mln.lot_number            = output_rec.lot_out
            AND mmt.inventory_item_id     = output_rec.inventory_item_id
            )
            LOOP
              v_lot_number         := lot_item_rec.lot_number ;
              v_inventory_item_id  := lot_item_rec.inventory_item_id;
              v_material_detail_id := lot_item_rec.material_detail_id ;
              v_batch_id           := lot_item_rec.batch_id;
              v_line_type          := lot_item_rec.line_type ;
              v_plan_qty           := lot_item_rec.plan_qty ;
              v_actual_qty         := lot_item_rec.transaction_quantity;
              show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.calc_heat_yield', 'v_lot_number :'||lot_item_rec.lot_number||',v_actual_qty :'||lot_item_rec.transaction_quantity||
               ',v_actual_total_qty :'||v_actual_total_qty) ;
              v_actual_total_qty := v_actual_total_qty + conv_uom (lot_item_rec.inventory_item_id, v_actual_qty, lot_item_rec.transaction_uom, v_yield_uom) ;
            END LOOP;
          EXCEPTION
          WHEN no_data_found THEN
            show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.calc_heat_yield', ' no data found for : '||batchstep_rec.batchstep_id) ;
          END;
          show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.calc_heat_yield', 'v_lot_number'||',v_inventory_item_id'||v_inventory_item_id ||', v_material_detail_id :'|| 
           v_material_detail_id) ;
          show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.calc_heat_yield', 'v_batch_id :'||v_batch_id||',v_line_type :'||v_line_type ||',v_plan_qty :'||v_plan_qty||',v_actual_qty :' 
           ||v_actual_qty) ;
          show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.calc_heat_yield', ' lot out  :'||output_rec.lot_out ||',transaction qty :'||output_rec.transaction_quantity) ;
          show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.calc_heat_yield', 'v_lot_number :'||v_lot_number||',v_actual_qty :'||v_actual_qty) ;
          v_converted_transaction_qty   := conv_uom (output_rec.inventory_item_id, output_rec.transaction_quantity, output_rec.transaction_uom, v_yield_uom) ;
          IF v_converted_transaction_qty = - 9999 THEN
            v_converted_transaction_qty := 0;
          END IF ;
          v_unconsumned_qty := v_converted_transaction_qty - NVL (v_actual_total_qty, 0) ; --c
          show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.calc_heat_yield', 'v_unconsumned_qty :'||v_unconsumned_qty) ;
          v_unconsumned_total_qty := v_unconsumned_qty + v_unconsumned_prev_qty;--t
          v_unconsumned_prev_qty  := v_unconsumned_total_qty;                   --p
        END LOOP;
        v_taotal_uncomsumed := NVL (v_taotal_uncomsumed, 0) + NVL (v_unconsumned_total_qty, 0) ;
        show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.calc_heat_yield', 'v_taotal_uncomsumed :'||v_taotal_uncomsumed) ;
      END LOOP;
      BEGIN
        SELECT --sum(abs(mmt.transaction_quantity)),
          SUM (conv_uom (mmt.inventory_item_id, ABS (mmt.transaction_quantity), mmt.transaction_uom, v_yield_uom))
        INTO v_comsumed_qty
        FROM mtl_object_genealogy mob,
          mtl_material_transactions mmt,
          gme_batch_step_items gbsi,
          gme_material_details gmd,
          gme_batch_steps gbs
        WHERE mob.origin_txn_id (+)   = mmt.transaction_id
        AND mmt.trx_source_line_id    = gbsi.material_detail_id
        AND gbsi.material_detail_id   = gmd.material_detail_id
        AND mmt.transaction_source_id = gmd.batch_id
        AND gbsi.batchstep_id         = gbs.batchstep_id
        AND gbsi.batch_id             = batch_struct_rec.batch_id
        AND mmt.transaction_id NOT   IN
          (SELECT DISTINCT
            --gmd.actual_qty,mmt.transaction_quantity, mtln.lot_number,
            mmt.transaction_id
            -- ,mmt.inventory_item_id, mob.object_id , mob.parent_object_id
          FROM mtl_object_genealogy mob,
            mtl_material_transactions mmt,
            mtl_object_genealogy mog,
            mtl_transaction_lot_numbers mtln,
            gme_material_details gmd
          WHERE mob.origin_txn_id       = mmt.transaction_id
          AND mob.object_id             = mog.parent_object_id
          AND mmt.transaction_id        = mtln.transaction_id
          AND mmt.trx_source_line_id    = gmd.material_detail_id
          AND mmt.transaction_source_id = gmd.batch_id
          AND gmd.batch_id              = batch_struct_rec.batch_id
            --  and mmt.trx_source_line_id = material_in_rec.material_detail_id
          AND gmd.line_type = - 1
          )
          --    and mmt.trx_source_line_id = material_in_rec.material_detail_id
        AND gmd.line_type = - 1;
        ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        -- this cursor gets the output from the current batch step where the batch step is not liked to ingredient
        ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        FOR output_rec_2 IN output_cur_2 (batch_struct_rec.batch_id)
        LOOP
          FOR lot_item_rec IN
          (SELECT lot_number,
            mmt.inventory_item_id,
            gmd.material_detail_id,
            gmd.batch_id,
            transaction_uom,
            gmd.line_type,
            gmd.plan_qty,
            ABS (mmt.transaction_quantity) transaction_quantity--gmd.actual_qty
          FROM mtl_lot_numbers mln,
            mtl_object_genealogy mog,
            mtl_material_transactions mmt,
            gme_material_details gmd
          WHERE mln.gen_object_id       = mog.object_id
          AND mog.origin_txn_id         = mmt.transaction_id
          AND mmt.trx_source_line_id    = gmd.material_detail_id
          AND mmt.transaction_source_id = gmd.batch_id
          AND mln.lot_number            = output_rec_2.lot_out
          AND mmt.inventory_item_id     = output_rec_2.inventory_item_id
          )
          LOOP
            v_lot_number         := lot_item_rec.lot_number ;
            v_inventory_item_id  := lot_item_rec.inventory_item_id;
            v_material_detail_id := lot_item_rec.material_detail_id ;
            v_batch_id           := lot_item_rec.batch_id;
            v_line_type          := lot_item_rec.line_type ;
            v_plan_qty           := lot_item_rec.plan_qty ;
            v_actual_qty         := lot_item_rec.transaction_quantity;
            show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.calc_heat_yield', 'v_lot_number :'||lot_item_rec.lot_number||',v_actual_qty :'||lot_item_rec.transaction_quantity|| 
            ',v_actual_total_qty :'||v_actual_total_qty) ;
            v_actual_total_qty := v_actual_total_qty + conv_uom (lot_item_rec.inventory_item_id, v_actual_qty, lot_item_rec.transaction_uom, v_yield_uom) ;
          END LOOP;
          v_converted_transaction_qty   := conv_uom (output_rec_2.inventory_item_id, output_rec_2.transaction_quantity, output_rec_2.transaction_uom, v_yield_uom) ;
          IF v_converted_transaction_qty = - 9999 THEN
            v_converted_transaction_qty := 0;
          END IF ;
          v_unconsumned_qty       := v_converted_transaction_qty - NVL (v_actual_total_qty, 0) ; --c
          v_unconsumned_total_qty := v_unconsumned_qty           + v_unconsumned_prev_qty;       --t
          v_unconsumned_prev_qty  := v_unconsumned_total_qty;                                    --p
        END LOOP;
        v_taotal_uncomsumed := NVL (v_taotal_uncomsumed, 0) + NVL (v_unconsumned_total_qty, 0) ;
        ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        -- this cursor gets the output from the current batch step where the batch step is not liked to ingredient
        ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        SELECT SUM (conv_uom (mmt.inventory_item_id, ABS (mmt.transaction_quantity), mmt.transaction_uom, v_yield_uom))
          --sum(abs(mmt.transaction_quantity))
        INTO v_comsumed_qty
        FROM mtl_object_genealogy mob,
          mtl_material_transactions mmt,
          gme_material_details gmd,
          gme_batch_header gbh
        WHERE mob.origin_txn_id (+)   = mmt.transaction_id
        AND mmt.trx_source_line_id    = gmd.material_detail_id
        AND mmt.transaction_source_id = gmd.batch_id
        AND gmd.batch_id              = gbh.batch_id
        AND gbh.batch_id              = 951102
        AND mmt.transaction_id NOT   IN
          (SELECT DISTINCT
            --gmd.actual_qty,mmt.transaction_quantity, mtln.lot_number,
            mmt.transaction_id
            -- ,mmt.inventory_item_id, mob.object_id , mob.parent_object_id
          FROM mtl_object_genealogy mob,
            mtl_material_transactions mmt,
            mtl_object_genealogy mog,
            mtl_transaction_lot_numbers mtln,
            gme_material_details gmd
          WHERE mob.origin_txn_id       = mmt.transaction_id
          AND mob.object_id             = mog.parent_object_id
          AND mmt.transaction_id        = mtln.transaction_id
          AND mmt.trx_source_line_id    = gmd.material_detail_id
          AND mmt.transaction_source_id = gmd.batch_id
          AND gmd.batch_id              = batch_struct_rec.batch_id
            --  and mmt.trx_source_line_id = material_in_rec.material_detail_id
          AND gmd.line_type = - 1
          )
          --    and mmt.trx_source_line_id = material_in_rec.material_detail_id
        AND gmd.line_type =                           - 1;
        v_total_consumed := NVL (v_total_consumed, 0) + NVL (v_comsumed_qty, 0) ;
      EXCEPTION
      WHEN no_data_found THEN
        show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.calc_heat_yield', 'no data found for batch id : '||batch_struct_rec.batch_id) ;
      END ;
      show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.calc_heat_yield', 'v_comsumed_qty :'||v_comsumed_qty) ;
      v_total_consumed := NVL (v_total_consumed, 0) + NVL (v_comsumed_qty, 0) ;
      show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.calc_heat_yield', 'v_total_consumed :'||v_total_consumed) ;
    END LOOP;
    ------------------------------------------------------------------------------
    -- v_taotal_uncomsumed : total out quantity which is not consumed batch under heat, qty which is in inventory which is not consumed in heat batches is considered total out.
    --  v_total_consumed : total in qty which is consumed in the batch under heat
    ------------------------------------------------------------------------------
    if v_total_consumed <> 0 then 
    	v_heat_yield := v_taotal_uncomsumed / v_total_consumed * 100;
    else 
    
  		v_heat_yield := 0;
  	end if ; 
    show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.calc_heat_yield', 'Begin heat  yield calculations ') ;
    p_heat_yield := v_heat_yield;
    -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    -- update the batch genealogy table
    -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    --   update gme_batch_genealogy
    --  set batch_cumulative_yield =v_heat_yield
    -- where heat_number          =p_heat_number;
    show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.calc_heat_yield', 'End heat  yield calculations ') ;
  EXCEPTION
  WHEN OTHERS THEN
    show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.calc_heat_yield', 'Exception is '||SQLERRM) ;
  END calc_heat_yield;
/*###############################################################
# name
# calc_heat_yield
# synopsis
# function  calc_heat_yield
# description
#     wrapper function to call yield calculation package
# created by sachin ingle .kpit on 3/07/2015
###############################################################*/
  FUNCTION calc_heat_yield(
      p_batch_id IN VARCHAR2)
    RETURN NUMBER
  IS
    v_heat_yield  NUMBER ;
    v_heat_number VARCHAR2 (100) ;
    heat_number   NUMBER := 0;
  BEGIN
    show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.calc_heat_yield', 'Function begin heat  yield calculations ') ;
    BEGIN
      SELECT DISTINCT heat_number
      INTO v_heat_number
      FROM gme_batch_genealogy
      WHERE batch_id = p_batch_id;
    EXCEPTION
    WHEN no_data_found THEN
      heat_number := - 1;
    WHEN too_many_rows THEN
      heat_number := - 2;
    WHEN OTHERS THEN
      heat_number := - 3;
    END ;
    IF heat_number NOT IN ( - 1, - 2, - 3) THEN
      BEGIN
        calc_heat_yield (v_heat_number, v_heat_yield) ;
      EXCEPTION
      WHEN OTHERS THEN
        v_heat_yield := - 99;
        RETURN v_heat_yield;
      END ;
      RETURN v_heat_yield;
    ELSE
      v_heat_yield := - 999;
      RETURN v_heat_yield;
    END IF ;
  EXCEPTION
  WHEN OTHERS THEN
    v_heat_yield := - 9999;
    RETURN v_heat_yield;
    show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.calc_heat_yield', 'Function end heat  yield calculations ') ;
  END calc_heat_yield;
/*###############################################################
# name
# scrap_out
# synopsis
# func  scrap_out
# description
#    1.0 calculate scrap out for the particular step
#    1.1 the scrap out is converted into step uom .
# created by sachin ingle .kpit on 30/06/2015
###############################################################*/
  FUNCTION scrap_out(
      p_batchstep_id IN gme_batch_steps.batchstep_id%type)
    RETURN NUMBER
  IS
    v_total_out    NUMBER (10, 5) ;
    v_material_out NUMBER (10, 5) := 0;
    v_item_id gme_material_details.inventory_item_id%type;
    v_uom gme_material_details.dtl_um%type;
    v_yield_uom VARCHAR2 (100) ;
    v_temp_qty  NUMBER := 0;
    v_step_uom  VARCHAR2 (100) ;
    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    -- this cursor gets the material output transaction details  for  the current batch step
    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    CURSOR material_out_cur (p_batchstep_id NUMBER)
    IS
      SELECT NVL (c.actual_qty, 0) actual_qty,
        c.inventory_item_id item_id,
        c.dtl_um uom
      FROM gme_batch_step_items a,
        gme_material_details c
      WHERE 1                  = 1
      AND c.batch_id           = a.batch_id
      AND a.batchstep_id       = p_batchstep_id
      AND c.material_detail_id = a.material_detail_id
      AND c.line_type          = 2
      AND by_product_type      = 'W'; --Changed for Bug 22390413
  BEGIN
    show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.scrap_out', 'Function begin scrap % calculations ') ;
    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    -- calculate scrap  out
    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    FOR material_out_rec IN material_out_cur (p_batchstep_id)
    LOOP
      v_temp_qty := material_out_rec.actual_qty;
      v_item_id  := material_out_rec.item_id;
      v_uom      := material_out_rec.uom;
      SELECT step_qty_um
      INTO v_step_uom
      FROM gme_batch_steps
      WHERE batchstep_id = p_batchstep_id;
      v_material_out    := NVL (v_material_out, 0) + conv_uom (v_item_id, v_temp_qty, v_uom, v_step_uom) ;
    END LOOP;
    RETURN v_material_out;
    show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.scrap_out', 'Function End scrap % calculations ') ;
  EXCEPTION
  WHEN OTHERS THEN
    show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.scrap_out', 'Exception is '||SQLERRM) ;
  END scrap_out;
/*###############################################################
# name
# calc_scrap_pct
# synopsis
# func  calc_scrap_pct
# description
#     calculate scrap yield for a batch step
# created by sachin ingle .kpit on 30/06/2015
###############################################################*/
  PROCEDURE calc_scrap_pct(
      p_batch_no IN NUMBER)
  IS
    CURSOR batch_cur
    IS
      SELECT * FROM gme_batch_header WHERE batch_no = p_batch_no;
    CURSOR batchstep_cur (cp_batch_id NUMBER)
    IS
      SELECT * FROM gme_batch_steps WHERE batch_id = cp_batch_id;
    CURSOR material_in_cur (p_batchstep_id NUMBER)
    IS
      SELECT gmd.material_detail_id,
        gbsi.batch_id,
        transaction_id,
        source_line_id,
        ABS (transaction_quantity),
        mmt.inventory_item_id,
        actual_qty
      FROM mtl_material_transactions mmt,
        gme_material_details gmd,
        gme_batch_step_items gbsi
      WHERE gmd.material_detail_id = gbsi.material_detail_id
      AND gmd.material_detail_id   = mmt.trx_source_line_id
      AND gmd.batch_id             = mmt.transaction_source_id
      AND gmd.line_type            = - 1
      AND gbsi.batchstep_id        = p_batchstep_id
      AND NOT EXISTS
        (SELECT
          /*+ no_unnest */
          transaction_id1
        FROM gme_transaction_pairs
        WHERE transaction_id1 = mmt.transaction_id
        AND pair_type         = 1
        ) ;
    v_inter_in                  NUMBER ;
    v_inter_out                 NUMBER ;
    v_material_in               NUMBER ;
    v_material_out              NUMBER ;
    v_total_in                  NUMBER ;
    v_scrap_out                 NUMBER ;
    v_row_cnt                   NUMBER := 0;
    v_step_cum_input            NUMBER := 0;
    v_step_grand_input          NUMBER ;
    v_step_cum_scrap            NUMBER ;
    v_step_grand_scrap          NUMBER ;
    v_step_scrap_per            NUMBER ;
    v_step_cum_scrap_per        NUMBER ;
    v_step_cum_grand_scrap_per  NUMBER ;
    v_step_cum_yield            NUMBER ;
    v_prev_step_scrap_per       NUMBER ;
    v_prev_step_cum_scrap_per   NUMBER ;
    v_prev_step_grand_scrap_per NUMBER ;
    v_prev_step_cum_yield       NUMBER ;
    v_grnd_input_2              NUMBER := 0;
    v_actual_2                  NUMBER := 0;
    actual_total_cum_yield      NUMBER ;
    v_material_cum_yield        NUMBER;
    v_mmt_count                 NUMBER ;
    v_actual                    NUMBER ;
    v_actual_qty                NUMBER ;
    v_grnd_input                NUMBER := 0 ;
    v_scrap_total_cum_pct       NUMBER ;
  BEGIN
    show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.calc_scrap_pct', ' begin scrap % calculations ') ;
    FOR batch_rec IN batch_cur
    LOOP
      FOR batchstep_rec IN batchstep_cur (batch_rec.batch_id)
      LOOP
        v_material_cum_yield  := 0;
        v_scrap_total_cum_pct := 0;
        v_actual_qty          := 0;
        v_grnd_input          := 0;
        v_grnd_input_2        := 0;
        v_actual_2            := 0;
        show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.calc_scrap_pct', 'batch step no :'||batchstep_rec.batchstep_no||',batchstep_rec.batchstep_id :'||batchstep_rec.batchstep_id) ;
        -- Get Intermediate In and Scrap Values from Function Call
        v_inter_in  := get_inter_in (batchstep_rec.batchstep_id) ;
        v_scrap_out := scrap_out (batchstep_rec.batchstep_id) ;
        -- Get Material In and Out Values from Function Call
        v_material_in  := material_in (batchstep_rec.batchstep_id) ;
        v_material_out := material_out (batchstep_rec.batchstep_id) ;
        SELECT actual_step_cum_yield
        INTO v_step_cum_yield
        FROM gme_batch_steps
        WHERE batchstep_id = batchstep_rec.batchstep_id;
        FOR material_in_rec IN material_in_cur (batchstep_rec.batchstep_id)
        LOOP
          SELECT COUNT (1)
          INTO v_mmt_count
          FROM mtl_material_transactions mmt
          WHERE mmt.trx_source_line_id  = material_in_rec.material_detail_id
          AND mmt.transaction_source_id = material_in_rec.batch_id;
          show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.calc_scrap_pct', 'v_mmt_count :'||v_mmt_count) ;
          IF v_mmt_count > 0 THEN
            show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.calc_scrap_pct', 'material_in_rec.transaction_id: '||material_in_rec.transaction_id||',material_in_cur.material_detail_id :' 
            ||material_in_rec.material_detail_id) ;
            BEGIN
              SELECT --actual_step_cum_yield, actual_step_yield,
                actual_step_cum_yield / 100,
                scrap_total_cum_pct   / 100
                -- , planned_step_yield, planned_step_cum_yield, planned_total_cum_yield, scrap_pct, scrap_cum_pct, scrap_total_cum_pct
              INTO v_material_cum_yield,
                v_scrap_total_cum_pct
              FROM mtl_object_genealogy mog,
                mtl_object_genealogy mog1,
                mtl_material_transactions mmt,
                gme_batch_step_items gbsi,
                gme_batch_steps gbs
              WHERE 1                    = 1
              AND mog.object_id          = mog1.parent_object_id
              AND mog1.origin_txn_id     = mmt.transaction_id
              AND mmt.trx_source_line_id = gbsi.material_detail_id
              AND gbs.batchstep_id       = gbsi.batchstep_id
              AND mog1.end_date_active  IS NULL
              AND mog.origin_txn_id      = material_in_rec.transaction_id ;
              show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.calc_scrap_pct', 'v_material_cum_yield: '||v_material_cum_yield||',v_scrap_total_cum_pct :'||v_scrap_total_cum_pct) ;
            EXCEPTION
            WHEN no_data_found THEN
              v_material_cum_yield  := 1;
              v_scrap_total_cum_pct := 1 ;
              show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.calc_scrap_pct', ' exception v_material_cum_yield: '||v_material_cum_yield||',v_scrap_total_cum_pct :'|| 
               v_scrap_total_cum_pct) ;
            END;
            SELECT SUM (conv_uom (mmt.inventory_item_id, ABS (mmt.transaction_quantity), gmd.dtl_um, gbs.step_qty_um)) xx
            INTO v_actual_qty
            FROM mtl_object_genealogy mob,
              mtl_material_transactions mmt,
              gme_batch_step_items gbsi,
              gme_material_details gmd,
              gme_batch_steps gbs
            WHERE mob.origin_txn_id (+)   = mmt.transaction_id
            AND mmt.trx_source_line_id    = gbsi.material_detail_id
            AND mmt.transaction_source_id = gmd.batch_id
            AND gbsi.material_detail_id   = gmd.material_detail_id
            AND gbsi.batchstep_id         = gbs.batchstep_id
            AND mmt.transaction_id        = material_in_rec.transaction_id
            AND gmd.line_type             = - 1;
          elsif v_mmt_count               = 0 THEN
            SELECT actual_qty
            INTO v_actual_qty
            FROM gme_material_details
            WHERE material_detail_id = material_in_rec.material_detail_id;
            v_material_cum_yield    := 1;
            v_scrap_total_cum_pct   := 1 ;
            show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.calc_scrap_pct', ' v_mmt_count =0, v_material_cum_yield: '||v_material_cum_yield||',v_scrap_total_cum_pct :'|| 
             v_scrap_total_cum_pct) ;
            NULL;
          END IF ;
          show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.calc_scrap_pct', ' v_actual_qty : '||v_actual_qty) ;
          if v_material_cum_yield <> 0 then 
          	v_actual   := (v_actual_qty / v_material_cum_yield) * v_scrap_total_cum_pct;
          
         		v_actual_2 := (v_actual_qty / v_material_cum_yield) ;
          else 
          	v_actual := v_actual_qty * v_scrap_total_cum_pct  ; 
          	v_actual_2 := v_actual_qty; 
          end if ; 
          show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.calc_scrap_pct', ' v_actual : '||v_actual_2) ;
          show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.calc_scrap_pct', ' v_actual: '||v_actual) ;
          v_grnd_input   := v_grnd_input   + v_actual;
          v_grnd_input_2 := v_grnd_input_2 + v_actual_2;
        END LOOP;--material_in_rec;
        v_total_in           := NVL (v_material_in, 0) + NVL (v_inter_in, 0) ;
        IF v_row_cnt          = 0 THEN
          IF v_scrap_out     <> 0 THEN
            v_step_scrap_per := v_total_in / v_scrap_out;
          ELSE
            IF v_scrap_out      = 0 THEN
              v_step_scrap_per := 0;
            END IF ;
          END IF ;
          v_step_cum_input := (v_inter_in   / 1) + v_material_in;
          v_step_cum_scrap := ( (v_inter_in / 1) * 1) + v_scrap_out;
          show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.calc_scrap_pct', 'v_step_scrap_per :'|| v_step_scrap_per) ;
          show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.calc_scrap_pct', 'v_step_cum_input :'|| v_step_cum_input) ;
          v_step_cum_scrap_per := v_step_cum_scrap / v_step_cum_input;
          show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.calc_scrap_pct', 'v_step_cum_scrap_per :'|| v_step_cum_scrap_per) ;
          v_step_grand_input := (v_inter_in   / 1) + v_grnd_input_2;
          v_step_grand_scrap := ( (v_inter_in / 1) * 1) + v_scrap_out + (v_grnd_input) ;
          show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.calc_scrap_pct', 'v_step_grand_input :'|| v_step_grand_input) ;
          show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.calc_scrap_pct', 'v_step_grand_scrap :'|| v_step_grand_scrap) ;
          if v_step_grand_input <> 0 then 
          	v_step_cum_grand_scrap_per := v_step_grand_scrap / v_step_grand_input;
          else 
          	v_step_cum_grand_scrap_per := 0 ; 
          end if; 
          show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.calc_scrap_pct', v_step_cum_grand_scrap_per) ;
          UPDATE gme_batch_steps
          SET scrap_pct                = v_step_scrap_per,
            scrap_cum_pct              = v_step_cum_scrap_per,
            scrap_total_cum_pct        = NULL
            --BUG 25718309 remove who columns
            /*last_update_date           = sysdate,
            last_updated_by            = TO_NUMBER (FND_PROFILE.VALUE ('USER_ID')),
            last_update_login          = TO_NUMBER (FND_PROFILE.VALUE ('LOGIN_ID'))*/
          WHERE batchstep_id           = batchstep_rec.batchstep_id;
          v_prev_step_scrap_per       := v_step_scrap_per ;
          v_prev_step_cum_scrap_per   := v_step_cum_scrap_per;
          v_prev_step_grand_scrap_per := v_step_cum_grand_scrap_per;
          v_prev_step_cum_yield       := v_step_cum_yield;
        ELSE
          IF v_row_cnt          > 0 THEN
            IF v_scrap_out     <> 0 THEN
              v_step_scrap_per := v_total_in / v_scrap_out;
            ELSE
              IF v_scrap_out      = 0 THEN
                v_step_scrap_per := 0;
              END IF ;
            END IF ;
            if v_prev_step_cum_yield <> 0 then 
	            v_step_cum_input := (v_inter_in   / v_prev_step_cum_yield) + v_material_in;
	            v_step_cum_scrap := ( (v_inter_in / v_prev_step_cum_yield) * v_prev_step_cum_scrap_per) + v_scrap_out;
	            if v_step_cum_input <> 0 then 
	            	v_step_cum_scrap_per := v_step_cum_scrap / v_step_cum_input;
	            else 
	            	v_step_cum_scrap_per := 0 ; 
	          	end if ; 
	            v_step_grand_input := (v_inter_in   / v_prev_step_cum_yield) + v_grnd_input_2;
	            v_step_grand_scrap := ( (v_inter_in / v_prev_step_cum_yield) * v_prev_step_grand_scrap_per) + v_scrap_out + (v_grnd_input) ;
	            
            else 
            	v_step_cum_input := v_inter_in + v_material_in; 
            	v_step_cum_scrap :=v_inter_in * v_prev_step_cum_scrap_per +  v_scrap_out; 
            	v_step_grand_input := v_inter_in + v_grnd_input_2 ; 
            	v_step_grand_scrap :=  v_inter_in + v_scrap_out + (v_grnd_input);
            	
            end if ; 
            	
          	show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.calc_scrap_pct', 'v_step_cum_input :'|| v_step_cum_input) ;
            show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.calc_scrap_pct', 'v_step_cum_scrap :'|| v_step_cum_scrap) ;
            show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.calc_scrap_pct', 'v_step_cum_scrap_per :'|| v_step_cum_scrap_per) ;
            show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.calc_scrap_pct', 'v_step_grand_input :'|| v_step_grand_input) ;
            show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.calc_scrap_pct', 'v_step_grand_scrap :'|| v_step_grand_scrap) ;

						if v_step_grand_input <> 0 then 
            	v_step_cum_grand_scrap_per := v_step_grand_scrap / v_step_grand_input;
            else 
            	v_step_cum_grand_scrap_per := 0 ; 
            end if ; 


            show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.calc_scrap_pct', v_step_cum_grand_scrap_per) ;
            UPDATE gme_batch_steps
            SET scrap_pct                = v_step_scrap_per,
              scrap_cum_pct              = v_step_cum_scrap_per,
              scrap_total_cum_pct        = NULL
              --BUG 25718309 remove who columns
              /*last_update_date           = sysdate,
              last_updated_by            = TO_NUMBER (FND_PROFILE.VALUE ('USER_ID')),
              last_update_login          = TO_NUMBER (FND_PROFILE.VALUE ('LOGIN_ID'))*/
            WHERE batchstep_id           = batchstep_rec.batchstep_id;
            v_prev_step_scrap_per       := v_step_scrap_per ;
            v_prev_step_cum_scrap_per   := v_step_cum_scrap_per;
            v_prev_step_grand_scrap_per := v_step_cum_grand_scrap_per;
            v_prev_step_cum_yield       := v_step_cum_yield;
          END IF;
        END IF;
        v_row_cnt := v_row_cnt + 1;
      END LOOP;--batchstep_rec;=(
    END LOOP;  --batch_rec ;
    show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.calc_scrap_pct', ' end scrap % calculations ') ;
  EXCEPTION
  WHEN OTHERS THEN
    show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.calc_scrap_pct', 'Exception is '||SQLERRM) ;
  END calc_scrap_pct;
/*###############################################################
# name
# iot_get_parent_batch
# synopsis
# func  iot_get_parent_batch
# description
#     get the parent batch id for heat checking
# created by fhariddin shaik .kpit on 15/07/2015
###############################################################*/
  FUNCTION iot_get_parent_batch(
      p_lot_number        IN VARCHAR2,
      p_inventory_item_id IN NUMBER,
      p_organization_id   IN NUMBER)  --BUG 24457891	Add Input parameter p_organization_id
    RETURN NUMBER
  IS
    v_parent_batch NUMBER ;
    v_material_detail_id NUMBER;  --BUG 26248999


  BEGIN
    show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.iot_get_parent_batch', ' Function get parent batch for lot ' || p_lot_number  
    ||' p_inventory_item_id ' || p_inventory_item_id || ' p_organization_id: ' || p_organization_id) ;
    
     v_material_detail_id := get_parent_material_line(p_inventory_item_id =>p_inventory_item_id,          
                                                     p_organization_id   =>p_organization_id,  
                                                     p_lot_number        =>p_lot_number);
                                                
    IF NVL(v_material_detail_id,0) > 0 THEN                                                  
      SELECT batch_id
        INTO v_parent_batch
        FROM gme_material_details
       WHERE material_detail_id = v_material_detail_id; 

    ELSE
      v_parent_batch := -9999;

    END IF;
    
    show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.iot_get_parent_batch', ' v_parent_batch: ' || v_parent_batch ) ;

   
    RETURN v_parent_batch;
  EXCEPTION
  WHEN OTHERS THEN
    show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.iot_get_parent_batch', 'Exception is '||SQLERRM) ;
    return -9999;
  END iot_get_parent_batch;
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Begin of  formula procedures used for calculations and are referred by main procedure
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  PROCEDURE yield_calc(
      p_batch_id     IN NUMBER,
      p_batchstep_id IN NUMBER DEFAULT NULL,
      p_source_type  IN VARCHAR2 DEFAULT 'CP')
  IS
    l_yield_rec step_yield_rec;
  BEGIN
    yield_calc (p_batch_id, p_batchstep_id, p_source_type, l_yield_rec) ;
  EXCEPTION
  WHEN OTHERS THEN
    show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.yield_calc', 'Exception is '||SQLERRM) ;
  END yield_calc;
  PROCEDURE step_yield_frm(
      p_total_in  IN NUMBER,
      p_total_out IN NUMBER,
      p_step_yield OUT NOCOPY NUMBER)
  IS
  BEGIN
    IF p_total_in  <> 0 THEN
      p_step_yield := (p_total_out / p_total_in) ;
    ELSE
      p_step_yield := 0;
    END IF ;
    --BUG 25504743 Allow Yield(Step, Cumulative and Grand Cumulative Yields ) Greater than 100%.
    --IF p_step_yield > 1 THEN
    --  p_step_yield := 1;
    --END IF ;
  EXCEPTION
  WHEN OTHERS THEN
    show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.step_yield_frm', 'Exception is '||SQLERRM) ;
  END step_yield_frm ;
  PROCEDURE step_cum_yield_frm(
      p_inter_in              IN NUMBER,
      p_step_cum_input_part_1 IN NUMBER,
      p_material_in           IN NUMBER,
      p_total_out             IN NUMBER,
      x_step_cum_yield OUT NOCOPY NUMBER)
  IS
    v_step_cum_input NUMBER ;
  BEGIN
    --v_step_cum_input    := (p_inter_in / (p_prev_step_cum_yield )) + NVL (p_material_in, 0) ;
    v_step_cum_input    := p_step_cum_input_part_1+ NVL (p_material_in, 0) ;
    IF v_step_cum_input <> 0 THEN
      x_step_cum_yield  := p_total_out / v_step_cum_input;
    ELSE
      --Bug 24430513 change value from 1 to 0 while input is zero.
      x_step_cum_yield := 0;
    END IF ;
  EXCEPTION
  WHEN OTHERS THEN
    show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.step_cum_yield_frm', 'Exception is '||SQLERRM) ;
  END step_cum_yield_frm ;
  PROCEDURE step_grand_cum_yield(
      p_inter_in               IN NUMBER,
      p_step_grand_input_part1 IN NUMBER,
      p_grand_input            IN NUMBER,
      p_total_out              IN NUMBER,
      p_step_grand_cum_yield OUT NOCOPY NUMBER)
  IS
    v_grand_step_cum_input NUMBER ;
  BEGIN
    -- YJ: Corrected logic, as it was not right.
    v_grand_step_cum_input    := p_step_grand_input_part1 + NVL (p_grand_input, 0) ;
    IF v_grand_step_cum_input <> 0 THEN
      
      p_step_grand_cum_yield  := (p_total_out / v_grand_step_cum_input);
      --p_step_grand_cum_yield :=p_step_grand_cum_yield;
    ELSE
      --Bug 24430513 change value from 1 to 0 while input is zero.
      p_step_grand_cum_yield := 0;
    END IF ;
  EXCEPTION
  WHEN OTHERS THEN
    show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.step_grand_cum_yield', 'Exception is '||SQLERRM) ;
  END step_grand_cum_yield;
  PROCEDURE step_plan_cum_yield(
      p_plan_yield                 IN NUMBER,
      p_prev_step_plan_cum_yield   IN NUMBER,
      p_inter_in                   IN NUMBER,
      p_prev_step_actual_cum_yield IN NUMBER,
      p_material_in                IN NUMBER,
      x_step_plan_cum_yield OUT NOCOPY NUMBER)
  IS
    v_plan_cum_yield      NUMBER ;
    v_plan_yield_output   NUMBER ;
    v_calc_plan_cum_yield NUMBER ;
    v_step_cum_input      NUMBER ;
  BEGIN
  	if p_prev_step_actual_cum_yield <>0 then 
    	v_step_cum_input := (p_inter_in / (p_prev_step_actual_cum_yield / 100)) + NVL (p_material_in, 0) ;
    else 
    	v_step_cum_input :=p_inter_in +  NVL (p_material_in, 0); 
    end if; 
    -- Commented on 20-Oct-2015 for new formula used
    --v_plan_cum_yield       :=p_plan_yield   *p_prev_step_plan_cum_yield;
    --v_plan_yield_output    :=((((p_inter_in /p_prev_step_cum_yield ) * p_prev_step_plan_cum_yield) + p_material_in) * v_plan_cum_yield);
    -- Below Formula Used for Calculation of Planned Cumulative Output
    --(Actual Input to the current step * (Prev step Plan cum Yield/Prev Step Actual Cum Yield) + external input)* Current Steps plan yield
    if p_prev_step_actual_cum_yield <> 0 then 
    	v_plan_yield_output     := (p_inter_in * (p_prev_step_plan_cum_yield / p_prev_step_actual_cum_yield) + p_material_in) * p_plan_yield;
    else 
    	v_plan_yield_output := ( p_inter_in * p_prev_step_plan_cum_yield + p_material_in) * p_plan_yield; 
  	end if ; 
  	
    IF v_step_cum_input     <> 0 THEN
      x_step_plan_cum_yield := (v_plan_yield_output / v_step_cum_input) / 100;
    ELSE
      x_step_plan_cum_yield := 0;
    END IF ;
  EXCEPTION
  WHEN OTHERS THEN
    show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.step_plan_cum_yield', 'Exception is '||SQLERRM) ;
  END step_plan_cum_yield;
  PROCEDURE step_scrap_pct(
      p_total_in  IN NUMBER,
      p_scrap_out IN NUMBER,
      step_scrap_pct OUT NOCOPY NUMBER)
  IS
  BEGIN
    IF p_scrap_out   <> 0 THEN
      step_scrap_pct := p_scrap_out / p_total_in; -- Updated for Bug 22390413
    ELSE
      IF p_scrap_out    = 0 THEN
        step_scrap_pct := 0;
      END IF ;
    END IF;
  EXCEPTION
  WHEN OTHERS THEN
    show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.step_scrap_pct', 'Exception is '||SQLERRM) ;
  END step_scrap_pct ;
  PROCEDURE step_cum_scrap_pct(
      p_inter_in                      NUMBER,
      p_prev_step_cum_yield           NUMBER,
      p_material_in                   NUMBER,
      p_prev_step_cum_scrap_per       NUMBER,
      p_scrap_out                     NUMBER,
      p_step_cum_scrap_pct OUT NOCOPY NUMBER)
  IS
    v_step_cum_input     NUMBER := 0 ;
    v_step_cum_scrap     NUMBER := 0;
    v_step_cum_scrap_pct NUMBER := 0;
  BEGIN
  	
  	if p_prev_step_cum_yield = 0 then 
			v_step_cum_input       := p_inter_in   + p_material_in;
    	v_step_cum_scrap       := ( p_inter_in  * p_prev_step_cum_scrap_per) + p_scrap_out;
    else 
  		v_step_cum_input       := (p_inter_in   / p_prev_step_cum_yield) + p_material_in;
    	v_step_cum_scrap       := ( (p_inter_in / p_prev_step_cum_yield) * p_prev_step_cum_scrap_per) + p_scrap_out;
    
  	end if ; 
   IF v_step_cum_input    <> 0 THEN
      v_step_cum_scrap_pct := v_step_cum_scrap / v_step_cum_input;
    ELSE
      v_step_cum_scrap_pct := 0;
    END IF ;
    p_step_cum_scrap_pct := v_step_cum_scrap_pct;
  EXCEPTION
  WHEN OTHERS THEN
    show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.step_cum_scrap_pct', 'Exception is '||SQLERRM) ;
  END step_cum_scrap_pct ;
  PROCEDURE step_grand_cum_scrap_pct(
      p_inter_in                       NUMBER,
      p_prev_step_cum_yield            NUMBER,
      p_material_in                    NUMBER,
      p_prev_step_grand_scrap_per      NUMBER,
      p_grand_input                    NUMBER,
      p_grand_input_2                  NUMBER,
      p_scrap_out                      NUMBER,
      p_grand_cum_scrap_pct OUT NOCOPY NUMBER)
  IS
    v_step_grand_input NUMBER ;
    v_step_grand_scrap NUMBER ;
  BEGIN
  	if p_prev_step_cum_yield <> 0 then 
  		v_step_grand_input      := (p_inter_in   / p_prev_step_cum_yield) + p_grand_input;
   	 	v_step_grand_scrap      := ( (p_inter_in / p_prev_step_cum_yield) * p_prev_step_grand_scrap_per) + p_scrap_out + (p_grand_input_2) ;
   	else 
   	 	v_step_grand_input      := (p_inter_in   ) + p_grand_input;
   		v_step_grand_scrap      := (p_inter_in  * p_prev_step_grand_scrap_per) + p_scrap_out + (p_grand_input_2) ;
   
   
 	  end if ;
   
    IF v_step_grand_input   <> 0 THEN
      p_grand_cum_scrap_pct := v_step_grand_scrap / v_step_grand_input;
    ELSE
      p_grand_cum_scrap_pct := 0;
    END IF ;
    IF v_step_grand_scrap    = v_step_grand_input THEN
      p_grand_cum_scrap_pct := 0;
    END IF ;
  EXCEPTION
  WHEN OTHERS THEN
    show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.step_grand_cum_scrap_pct', 'Exception is '||SQLERRM) ;
  END step_grand_cum_scrap_pct;
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--  END OF  formula procedures used for calculations and are referred by main procedure
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
/*###############################################################
# name
# update_gem_batch_steps
# synopsis
# proc  update_gem_batch_steps
# description
#     this procedure updates the gme_batch_step , actual , plan and scrap yields . this procedure is exclusive for iot
# created by sachin ingle  .kpit on 29/07/2015
###############################################################*/
  PROCEDURE update_gem_batch_steps(
      p_gme_batch_step_rec IN step_yield_rec,
      x_status OUT NOCOPY VARCHAR2)
  IS
  BEGIN
    show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.update_gem_batch_steps', ' Private API to update gme_batch_steps table ') ;
    UPDATE gme_batch_steps
    SET actual_step_cum_yield = p_gme_batch_step_rec.actual_step_cum_yield,
      actual_step_yield       = p_gme_batch_step_rec.actual_step_yield,
      actual_total_cum_yield  = p_gme_batch_step_rec.actual_total_cum_yield,
      planned_step_yield      = p_gme_batch_step_rec.planned_step_yield,
      planned_step_cum_yield  = p_gme_batch_step_rec.planned_step_cum_yield,
      planned_total_cum_yield = p_gme_batch_step_rec.planned_total_cum_yield,
      scrap_pct               = p_gme_batch_step_rec.scrap_pct,
      scrap_cum_pct           = p_gme_batch_step_rec.scrap_cum_pct,
      scrap_total_cum_pct     = NULL
      --BUG 25718309 remove who columns
      /*last_update_date        = sysdate,
      last_updated_by         = TO_NUMBER (FND_PROFILE.VALUE ('USER_ID')),
      last_update_login       = TO_NUMBER (FND_PROFILE.VALUE ('LOGIN_ID'))*/
    WHERE batchstep_id        = p_gme_batch_step_rec.batchstep_id ;
    x_status                 := 's';
  EXCEPTION
  WHEN OTHERS THEN
    x_status := 'e';
    show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.update_gem_batch_steps', ' end  Private API to update gme_batch_steps table ') ;
  END update_gem_batch_steps;
/*###############################################################
# name
# get_total_product_out
# synopsis
# proc  get_total_product_out
# description
#     this function returns the total out qty for a single batch
# created by sachin ingle
###############################################################*/
  FUNCTION get_total_product_out(
      p_batch_id IN NUMBER)
    RETURN NUMBER
  IS
    v_total_out    NUMBER ;
    v_material_out NUMBER;
    v_item_id gme_material_details.inventory_item_id%type;
    v_uom gme_material_details.dtl_um%type;
    v_yield_uom VARCHAR2 (100) ;
    v_temp_qty  NUMBER := 0;
    v_step_uom  VARCHAR2 (100) ;
    CURSOR material_out_cur (p_batch_id NUMBER)
    IS
      SELECT NVL (c.actual_qty, 0) actual_qty,
        a.batchstep_id,
        c.inventory_item_id item_id,
        c.dtl_um uom
      FROM gme_batch_step_items a,
        gme_material_details c
      WHERE 1                           = 1
      AND c.batch_id                    = a.batch_id
      AND c.batch_id                    = p_batch_id
      AND c.material_detail_id          = a.material_detail_id
      AND c.line_type                   in (1,2) --Bug 22318890 : Fixed the issue of By Product not being considered for yield
      AND NVL (c.by_product_type, 'Y') = 'Y'; -- Bug 22361054  : By-product type Sample, Rework and Waste should not be considered for yield
  BEGIN
    show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.get_total_product_out', 'Begin function get total product out for a batch ') ;
    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    -- calculate material in
    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    FOR material_out_rec IN material_out_cur (p_batch_id)
    LOOP
      v_temp_qty := material_out_rec.actual_qty;
      v_item_id  := material_out_rec.item_id;
      v_uom      := material_out_rec.uom;
      SELECT step_qty_um
      INTO v_step_uom
      FROM gme_batch_steps
      WHERE batchstep_id = material_out_rec.batchstep_id ;
      v_material_out    := NVL (v_material_out, 0) + conv_uom (v_item_id, v_temp_qty, v_uom, v_step_uom) ;
    END LOOP;
    RETURN v_material_out;
    show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.get_total_product_out', 'End function get total product out for a batch ') ;
  EXCEPTION
  WHEN OTHERS THEN
    show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.get_total_product_out', 'Exception is '||SQLERRM) ;
  END get_total_product_out;


/*###############################################################
# name
# batch_heat_insert
# synopsis
# proc  batch_heat_insert
# description
# insert the batch data in gme_batch_genealogy table
# created by fhariddin shaik .kpit on 05/08/2015
###############################################################*/
  PROCEDURE batch_heat_insert(
      p_batch_id        IN gme_batch_genealogy.batch_id%type,
      p_heat_number     IN gme_batch_genealogy.heat_number%type,
      p_parent_batch_id IN gme_batch_genealogy.parent_batch_id%type)
  AS
  
  CURSOR check_batch_yield IS
    SELECT batch_yield
      FROM gme_batch_genealogy
     WHERE batch_id = p_batch_id
       AND batch_yield IS NOT NULL; 
       
  l_batch_yield NUMBER := NULL;      
  
  BEGIN
    show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.batch_heat_insert', 'Begin batch heat insert ') ;
    --check batch yield of the batch is already calculated
    --If yes, then we need to sync the batch yield to new created record.
    OPEN check_batch_yield;
    FETCH check_batch_yield INTO l_batch_yield;
    CLOSE check_batch_yield;  
    
    
    INSERT
    INTO gme_batch_genealogy
      (
        batch_id,
        heat_number,
        parent_batch_id,
        batch_yield,
        creation_date,
        created_by,
        last_update_date,
        last_updated_by,
        last_update_login
      )
      VALUES
      (
        p_batch_id,
        p_heat_number,
        p_parent_batch_id,
        l_batch_yield,
        sysdate,
        NVL (FND_GLOBAL.user_id, -1),
        sysdate,
        NVL (FND_GLOBAL.user_id, -1),
        NVL (FND_GLOBAL.login_id, -1)
      ) ;
  EXCEPTION
  WHEN OTHERS THEN
    show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.batch_heat_insert', 'ENd batch heat insert ') ;
  END batch_heat_insert;
/*###############################################################
# name
# iot_upd_parent_batch
# synopsis
# proc  iot_upd_parent_batch
# description
# procedure is used to update the parent batch id and parent heat number in gme_batch_genealogy table
# created by fhariddin shaik .kpit on 05/08/2015
###############################################################*/
  PROCEDURE iot_upd_parent_batch
    (
      p_batch_id           IN gme_batch_genealogy.batch_id%type,
      p_parent_heat_number IN gme_batch_genealogy.heat_number%type,
      p_parent_batch_id    IN gme_batch_genealogy.parent_batch_id%type
    )
  AS
  BEGIN
    UPDATE gme_batch_genealogy
    SET heat_number     = DECODE(p_parent_heat_number,NULL,heat_number,p_parent_heat_number),  --BUG 24457891
      parent_batch_id   = p_parent_batch_id,
      last_updated_by   = TO_NUMBER (FND_PROFILE.VALUE ('USER_ID')),
      last_update_date  = sysdate,
      last_update_login = TO_NUMBER (FND_PROFILE.VALUE ('LOGIN_ID'))
    WHERE batch_id      = p_batch_id;
  EXCEPTION
  WHEN OTHERS THEN
    show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.iot_upd_parent_batch', 'error while update the heat number in gme_batch_genealogy table') ;
  END iot_upd_parent_batch;
/*###############################################################
# name
# batch_heat_update
# synopsis
# proc  batch_heat_update
# description
# procedure is used to update the parent batch id and parent heat number in gme_batch_genealogy table
# sahcin ingle
###############################################################*/
  PROCEDURE batch_heat_update(
      p_batch_id    IN gme_batch_genealogy.batch_id%type,
      p_heat_number IN gme_batch_genealogy.heat_number%type)
  AS
  BEGIN
    UPDATE gme_batch_genealogy
    SET heat_number     = p_heat_number,
      last_updated_by   = TO_NUMBER (FND_PROFILE.VALUE ('USER_ID')),
      last_update_date  = sysdate,
      last_update_login = TO_NUMBER (FND_PROFILE.VALUE ('LOGIN_ID'))
    WHERE batch_id      = p_batch_id;
  EXCEPTION
  WHEN OTHERS THEN
    show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.iot_upd_parent_batch', 'error while update the heat number in gme_batch_genealogy table') ;
  END;
/*###############################################################
# Name
# compute_yield
# Synopsis
# proc  compute_yield
# description
#this procedure is called by concurrent program.
# sahcin ingle
###############################################################*/
  PROCEDURE COMPUTE_YIELD(
      errbuf OUT NOCOPY  VARCHAR2,
      retcode OUT NOCOPY VARCHAR2,
      p_org_id   IN NUMBER,
      p_batch_no IN VARCHAR2 DEFAULT NULL)
  IS
    p_yield_rec GME_YIELD_CALCULATION_PVT.step_yield_rec;
    p_plan_yield_rec GME_YIELD_CALCULATION_PVT.step_yield_rec;
    p_grand_plan_yield_rec GME_YIELD_CALCULATION_PVT.step_yield_rec;
    l_batch_id NUMBER := NULL ;
    l_root_batch_id     NUMBER;
    l_recalculate_yield VARCHAR2(1);
    l_recalculate_count NUMBER := 0;
    -- Get batch id for passed in batch number
    --BUG 25718309 Add condition organization_id so that only one batch_id returned.
    CURSOR Cur_Batch_Id
    IS
      SELECT batch_id FROM gme_batch_header WHERE batch_no = p_batch_no AND organization_id = p_org_id; 
    -- Check if batch is in Completed or Closed status.
    -- If it is in WIP or Completed status then calculate batch_yield.
    -- If batch_id is not passed then calc will be done for all batches (in completed and closed status) of a given org.
    --BUG 24351209 26-JUL-2016 only capture batches with recalculate_yield is Y when input parameter p_batch_no is NULL. 
    CURSOR batch_cur (p_batch_id IN NUMBER DEFAULT NULL)
    IS
      SELECT batch_id, batch_no
      FROM gme_batch_header
      WHERE 1             = 1
      --BUG 24351209 remove batch status restriction
      --AND batch_status   IN (3,4)
      AND batch_id        = p_batch_id
      AND organization_id = p_org_id
      AND p_batch_id IS NOT NULL
      UNION 
      SELECT DISTINCT gbh.batch_id, batch_no
        FROM gme_batch_header gbh,
             gme_batch_genealogy gbg
       WHERE p_batch_id IS NULL  
         AND NVL(gbg.recalculate_yield,'N') = 'Y'
         AND gbh.batch_id = gbg.batch_id
         AND (p_org_id IS NULL OR gbh.organization_id = p_org_id)
         --BUG 24351209 remove batch status restriction
         --AND batch_status   IN (3,4)
      ORDER BY batch_id; 
    
    --BUG 24351209 26-JUL-2016 get all of child batches starting with v_batch_id   
    CURSOR get_batch_hierarchy(v_batch_id IN NUMBER) IS
      SELECT DISTINCT batch_id,LEVEL
        FROM gme_batch_genealogy
       START WITH batch_id = v_batch_id
       CONNECT BY NOCYCLE PRIOR batch_id = parent_batch_id  --BUG 25987896
       ORDER BY LEVEL;     
    
    --BUG 24351209 26-JUL-2016 get the root parent batch if it also need to recalculate.  
    CURSOR check_parent_batch_exist(v_batch_id IN NUMBER) IS
      SELECT batch_id 
        FROM (       
              SELECT DISTINCT batch_id,LEVEL node_level
                FROM gme_batch_genealogy
               WHERE NVL(recalculate_yield,'N') = 'Y'
               START WITH batch_id = v_batch_id
               CONNECT BY NOCYCLE batch_id = PRIOR parent_batch_id   --BUG 25987896
               ORDER BY LEVEL DESC)
        WHERE rownum = 1;     
        
     --BUG 24351209 26-JUL-2016 get recalculate yield flag   
     CURSOR get_recalculate_yield(v_batch_id IN NUMBER) IS
       SELECT DISTINCT recalculate_yield
         FROM gme_batch_genealogy
        WHERE batch_id = v_batch_id; 
             
  BEGIN
    show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.compute_yield', 'Begin of compute yield procedure called by CP ') ;
    -- Get batch Id
    FOR i IN Cur_Batch_Id
    LOOP
      l_batch_id := i.batch_id;
    END LOOP;
    FOR batch_rec IN batch_cur (l_batch_id)
    LOOP
	    fnd_file.put_line(fnd_file.Log,'-------------------------'); -- Added for Bug 22384527
	    fnd_file.put_line(fnd_file.Log,'For Batch: ' || batch_rec.batch_no); -- Added for Bug 22384527
	    fnd_file.put_line(fnd_file.Log, ' Actual Yield Values'); -- Added for Bug 22384527
      /*BEGIN BUG 24351209 26-JUL-2016
        If a batch is passed, then recalculate yield for that batch. Also since 
        grand cumulative yield of that batch will change yield for downstream/child batches, 
        So it means we need to look into batch hierarchy and not that batch alone for calculation of yield.                                   
      */
      BEGIN
        IF p_batch_no IS NOT NULL THEN
          FOR cur_gbh IN get_batch_hierarchy(batch_rec.batch_id) LOOP
            
            yield_calc (cur_gbh.batch_id, NULL, 'CP', p_yield_rec) ;   
            
            fnd_file.put_line(fnd_file.Log, ' Planned Yield Values'); -- Added for Bug 22384527
            calc_plan_yield (cur_gbh.batch_id, NULL, 'CP', p_plan_yield_rec) ;
            fnd_file.put_line(fnd_file.Log, ' Planned Total Cumulative Yield Values'); -- Added for Bug 22384527         
            
            calc_grand_plan_yield (cur_gbh.batch_id, NULL, 'CP', p_grand_plan_yield_rec) ;
            
            -- Calculate yield of the batch
            calc_batch_yield (cur_gbh.batch_id) ;
            
            --BUG 24351209
            UPDATE gme_batch_genealogy
               SET recalculate_yield = 'N',
                   last_updated_by = fnd_global.user_id,
                   last_update_date = SYSDATE
             WHERE batch_id = cur_gbh.batch_id
               AND NVL(recalculate_yield,'N') = 'Y';   
            --commit for every 100 batches   
            l_recalculate_count := l_recalculate_count +1;
            IF l_recalculate_count > 100 THEN
              COMMIT;
              l_recalculate_count :=0;
              
            END IF;                                       
          END LOOP;    
                  
        ELSE

          OPEN get_recalculate_yield(batch_rec.batch_id);
          FETCH get_recalculate_yield INTO l_recalculate_yield;
          CLOSE get_recalculate_yield;
          --check whether the batc is calculated togther with parent batch in previous batch
          --If yes, than bypass the calculation for this batch        
          IF l_recalculate_yield = 'Y' THEN
            --check whether it has parent batch and the parent batch is marked as recalculated
            --If exist, then start with the root batch and calculate all of batch in the hierarchy
            --in sequence.
            OPEN check_parent_batch_exist(batch_rec.batch_id);
           FETCH check_parent_batch_exist INTO l_root_batch_id;
           CLOSE check_parent_batch_exist;
           
             FOR cur_gbh IN get_batch_hierarchy(l_root_batch_id) LOOP
               yield_calc (cur_gbh.batch_id, NULL, 'CP', p_yield_rec);  
               
               fnd_file.put_line(fnd_file.Log, ' Planned Yield Values'); -- Added for Bug 22384527
               calc_plan_yield (cur_gbh.batch_id, NULL, 'CP', p_plan_yield_rec) ;
               fnd_file.put_line(fnd_file.Log, ' Planned Total Cumulative Yield Values'); -- Added for Bug 22384527         
                  
               calc_grand_plan_yield (cur_gbh.batch_id, NULL, 'CP', p_grand_plan_yield_rec) ;
                  
               -- Calculate yield of the batch
               calc_batch_yield (cur_gbh.batch_id) ;
                  
               --BUG 24351209
               UPDATE gme_batch_genealogy
                  SET recalculate_yield = 'N',
                      last_updated_by = fnd_global.user_id,
                      last_update_date = SYSDATE
                WHERE batch_id = cur_gbh.batch_id
                  AND NVL(recalculate_yield,'N') = 'Y';  
                  
               --commit for every 100 batches   
               l_recalculate_count := l_recalculate_count +1;
               IF l_recalculate_count > 100 THEN
                 COMMIT;
                 l_recalculate_count :=0;                  
               END IF;                                       
            
             END LOOP;
          END IF;
        END IF;
      --BUG 25987896 Add exception so that the compute yeild can continue 
      --even if there is exception for one of batch yield calculation  
      EXCEPTION
        WHEN OTHERS THEN
          show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.compute_yield', 'Exception:'||SQLERRM
                    ||' is occurred for batch yield calculation:'||batch_rec.batch_id);
      
      END;
        --yield_calc (batch_rec.batch_id, NULL, 'CP', p_yield_rec) ;
      --END BUG 24351209
    END LOOP;
    show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.compute_yield', 'End of compute yield procedure called by CP ') ;
  EXCEPTION
  WHEN OTHERS THEN
    show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.compute_yield', 'Exception is '||SQLERRM) ;
  END COMPUTE_YIELD;
/*###############################################################
# name
# compute_yield
# synopsis
# proc  compute_yield
# description
#    This api calculates batch yield, it also calculates actual , planned and scrap yields internally
# sahcin ingle
###############################################################*/
  PROCEDURE compute_yield(
      p_batch_id IN NUMBER DEFAULT NULL)
  IS
    CURSOR batch_cur
    IS
      SELECT batch_id
      FROM gme_batch_header
      WHERE batch_id        = NVL (p_batch_id, batch_id) ;
      --BUG 24351209 remove batch status restriction
      --AND batch_status IN (3,4) 
    p_yield_rec GME_YIELD_CALCULATION_PVT.step_yield_rec;
    p_plan_yield_rec GME_YIELD_CALCULATION_PVT.step_yield_rec;
    p_grand_plan_yield_rec GME_YIELD_CALCULATION_PVT.step_yield_rec;
    v_heat_number VARCHAR2 (100) ;
    v_heat_yield  NUMBER ;
    l_root_batch_id     NUMBER;
    l_recalculate_yield VARCHAR2(1);
    
    
    
    --BUG 24351209 26-JUL-2016 get all of child batches starting with v_batch_id   
    CURSOR get_batch_hierarchy(v_batch_id IN NUMBER) IS
      SELECT DISTINCT batch_id,LEVEL
        FROM gme_batch_genealogy
       START WITH batch_id = v_batch_id
       CONNECT BY NOCYCLE PRIOR batch_id = parent_batch_id  --BUG 25987896
       ORDER BY LEVEL;     
    
    --BUG 24351209 26-JUL-2016 get the root parent batch if it also need to recalculate.  
    CURSOR check_parent_batch_exist(v_batch_id IN NUMBER) IS
      SELECT batch_id 
        FROM (       
              SELECT DISTINCT batch_id,LEVEL node_level
                FROM gme_batch_genealogy
               WHERE NVL(recalculate_yield,'N') = 'Y'
               START WITH batch_id = v_batch_id
               CONNECT BY NOCYCLE batch_id = PRIOR parent_batch_id  --BUG 25987896
               ORDER BY LEVEL DESC)
        WHERE rownum = 1;     
        
     --BUG 24351209 26-JUL-2016 get recalculate yield flag   
     CURSOR get_recalculate_yield(v_batch_id IN NUMBER) IS
       SELECT DISTINCT recalculate_yield
         FROM gme_batch_genealogy
        WHERE batch_id = v_batch_id; 
                
  BEGIN
    show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.compute_yield', 'Begin of compute yield procedure called by CP ') ;
    FOR batch_rec IN batch_cur
    LOOP
      BEGIN        
       /*BEGIN BUG 24351209 26-JUL-2016
          If a batch is passed, then recalculate yield for that batch. Also since 
          grand cumulative yield of that batch will change yield for downstream/child batches, 
          So it means we need to look into batch hierarchy and not that batch alone for calculation of yield.                                   
        */
        IF p_batch_id IS NOT NULL THEN
          FOR cur_gbh IN get_batch_hierarchy(batch_rec.batch_id) LOOP
            yield_calc (cur_gbh.batch_id, NULL, 'CP', p_yield_rec) ;        
          END LOOP;    
                  
        ELSE

          OPEN get_recalculate_yield(batch_rec.batch_id);
          FETCH get_recalculate_yield INTO l_recalculate_yield;
          CLOSE get_recalculate_yield;
          --check whether the batc is calculated togther with parent batch in previous batch
          --If yes, than bypass the calculation for this batch        
          IF l_recalculate_yield = 'Y' THEN
            --check whether it has parent batch and the parent batch is marked as recalculated
            --If exist, then start with the root batch and calculate all of batch in the hierarchy
            --in sequence.
            OPEN check_parent_batch_exist(batch_rec.batch_id);
           FETCH check_parent_batch_exist INTO l_root_batch_id;
           CLOSE check_parent_batch_exist;
           
             FOR cur_gbh IN get_batch_hierarchy(l_root_batch_id) LOOP
               yield_calc (cur_gbh.batch_id, NULL, 'CP', p_yield_rec);          
           
             END LOOP;
          END IF;
        END IF;
        --yield_calc (batch_rec.batch_id, NULL, 'CP', p_yield_rec) ;
        --END BUG 24351209      
      
      
        --yield_calc (batch_rec.batch_id, NULL, 'CP', p_yield_rec) ;
      EXCEPTION
      WHEN OTHERS THEN
        show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.compute_yield', 'Exception while calling yield_calc : '||SQLERRM) ;
      END;
      BEGIN
        calc_plan_yield (batch_rec.batch_id, NULL, 'CP', p_plan_yield_rec) ;
      EXCEPTION
      WHEN OTHERS THEN
        show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.compute_yield', 'Exception while calling calc_plan_yield : '||SQLERRM) ;
      END;
      BEGIN        
         --BEGIN BUG 24351209
        IF p_batch_id IS NOT NULL THEN
          FOR cur_gbh IN get_batch_hierarchy(batch_rec.batch_id) LOOP
            calc_grand_plan_yield (cur_gbh.batch_id, NULL, 'CP', p_grand_plan_yield_rec) ;
            
            UPDATE gme_batch_genealogy
               SET recalculate_yield = 'N',
                   last_updated_by = fnd_global.user_id,
                   last_update_date = SYSDATE
             WHERE batch_id = cur_gbh.batch_id  
               AND batch_id <> batch_rec.batch_id
               AND NVL(recalculate_yield,'N') = 'Y';      
          END LOOP;    
                  
        ELSE
          OPEN get_recalculate_yield(batch_rec.batch_id);
          FETCH get_recalculate_yield INTO l_recalculate_yield;
          CLOSE get_recalculate_yield;
          
          IF l_recalculate_yield = 'Y' THEN
                  
            OPEN check_parent_batch_exist(batch_rec.batch_id);
           FETCH check_parent_batch_exist INTO l_root_batch_id;
           CLOSE check_parent_batch_exist;
           
             FOR cur_gbh IN get_batch_hierarchy(l_root_batch_id) LOOP
               calc_grand_plan_yield (cur_gbh.batch_id, NULL, 'CP', p_grand_plan_yield_rec) ;
             
               UPDATE gme_batch_genealogy
                  SET recalculate_yield = 'N',
                      last_updated_by = fnd_global.user_id,
                      last_update_date = SYSDATE
                WHERE batch_id = cur_gbh.batch_id
                  AND batch_id <> batch_rec.batch_id
                  AND NVL(recalculate_yield,'N') = 'Y';            
           
             END LOOP;
          END IF;
        END IF;
        --calc_grand_plan_yield (batch_rec.batch_id, NULL, 'CP', p_grand_plan_yield_rec) ;
        --END BUG 24351209               
      
      
      
        --calc_grand_plan_yield (batch_rec.batch_id, NULL, 'CP', p_grand_plan_yield_rec) ;
      EXCEPTION
      WHEN OTHERS THEN
        show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.compute_yield', 'Exception while calling calc_grand_plan_yield : '||SQLERRM) ;
      END;
      BEGIN
        calc_batch_yield (batch_rec.batch_id) ;
      EXCEPTION
      WHEN OTHERS THEN
        show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.compute_yield', 'Exception while calling calc_batch_yield : '||SQLERRM) ;
      END;
      
      --BUG 24351209
      UPDATE gme_batch_genealogy
         SET recalculate_yield = 'N',
             last_updated_by = fnd_global.user_id,
             last_update_date = SYSDATE
       WHERE batch_id = batch_rec.batch_id
         AND NVL(recalculate_yield,'N') = 'Y';       
    END LOOP;
    show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.compute_yield', 'End of compute yield procedure called by CP ') ;
  EXCEPTION
  WHEN OTHERS THEN
    show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.compute_yield', 'Exception is '||SQLERRM) ;
  END compute_yield;
/*###############################################################
# name
# validate_heat
# synopsis
# proc  validate_heat
# description
#  used in heat number update  for mmt from
# sahcin ingle
###############################################################*/
  PROCEDURE validate_heat(
      p_batch_id          IN NUMBER,
      p_lot_number        IN VARCHAR2,
      p_inventory_item_id IN NUMBER,
      v_out OUT NOCOPY NUMBER,
      p_heat_rec OUT NOCOPY heat_rec)
  IS
    v_parent_batch_id           NUMBER ;
    v_parent_heat_number        VARCHAR2 (100) ;
    v_current_batch_heat_number VARCHAR2 (100) ;
    v_current_batch_prent_id    NUMBER ;
    v_heat_rec heat_rec;
    v_organization_id           NUMBER;  -- BUG 24457891	    
  BEGIN
    v_organization_id := gme_common_pvt.g_organization_id; -- BUG 24457891
    v_parent_batch_id    := iot_get_parent_batch (p_lot_number, p_inventory_item_id,v_organization_id) ; -- BUG 24457891
    IF v_parent_batch_id <> - 9999 THEN
      BEGIN
        SELECT heat_number
        INTO v_parent_heat_number
        FROM gme_batch_genealogy
        WHERE parent_batch_id NOT IN
          (SELECT parent_batch_id FROM gme_batch_genealogy WHERE batch_id = p_batch_id
          )
        AND parent_batch_id = v_parent_batch_id;
      EXCEPTION
      WHEN no_data_found THEN
        v_parent_heat_number := NULL ;
      END ;
      BEGIN
        SELECT DISTINCT heat_number,
          parent_batch_id
        INTO v_current_batch_heat_number,
          v_current_batch_prent_id
        FROM gme_batch_genealogy
        WHERE batch_id = p_batch_id;
      EXCEPTION
      WHEN no_data_found THEN
        v_current_batch_heat_number := NULL ;
      END ;
      p_heat_rec.l_current_batch_id   := p_batch_id;
      p_heat_rec.l_parent_heat_number := v_parent_heat_number;
      p_heat_rec.l_parent_batch_id    := v_parent_batch_id;
      IF (v_parent_heat_number         = v_current_batch_heat_number) AND (v_current_batch_prent_id = - 1) THEN
        v_out                         := 1;
      ELSE
        IF (v_parent_heat_number <> NVL (v_current_batch_heat_number, 0)) AND (v_current_batch_prent_id = - 1) THEN
          v_out                  := 1;
        ELSE
          IF (v_current_batch_heat_number IS NULL AND v_current_batch_heat_number IS NOT NULL) THEN
            v_out                         := 1;
          ELSE
            IF (v_parent_heat_number <> v_current_batch_heat_number) AND (v_current_batch_prent_id <> - 1) THEN
              v_out                  := 2;
            END IF ;
          END IF ;
        END IF ;
      END IF ;
    END IF ;
  EXCEPTION
  WHEN OTHERS THEN
    show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.validate_heat', 'Exception is '||SQLERRM) ;
  END validate_heat;
/*###############################################################
# name
# check_parent_heat
# synopsis
# proc  check_parent_heat
# description
#  used in gme_transactions_pvt.process_transactios
# sahcin ingle
# p_check : 1 = When current heat number and parent batch heat number is same
# p_check : 2 =  when current heat number and parent batch heat number are different
# p_check : 3 =  when current heat number and parent batch heat number both are null
###############################################################*/
  PROCEDURE check_parent_heat(
      p_batch_id          IN NUMBER,
      p_lot_number        IN VARCHAR2,
      p_inventory_item_id IN NUMBER,
      p_out OUT NOCOPY   VARCHAR2,
      p_check OUT NOCOPY NUMBER)
  IS
    v_parent_batch_id           NUMBER ;
    v_parent_heat_number        VARCHAR2 (100) ;
    v_current_batch_heat_number VARCHAR2 (100) ;
    v_current_batch_prent_id    NUMBER ;
    v_heat_rec heat_rec;
    
  --BEGIN BUG 24457891
    v_count                     NUMBER :=0; 
    v_total_quantity            NUMBER; 
  CURSOR get_all_material_txns(v_batch_id NUMBER) IS
    SELECT mmt.transaction_id,
           mmt.organization_id,
           mmt.transaction_type_id,
           mtln.inventory_item_id,
           mtln.lot_number,
           mmt.transaction_source_type_id
      FROM mtl_material_transactions mmt,
           mtl_transaction_lot_numbers mtln
     WHERE mmt.transaction_source_id = v_batch_id
       AND mmt.transaction_source_type_id = 5
       AND mmt.transaction_type_id IN (35,43)
       AND mmt.transaction_id = mtln.transaction_id
       AND NOT EXISTS
        (SELECT
          /*+ no_unnest */
          transaction_id1
        FROM gme_transaction_pairs
        WHERE transaction_id1 = mmt.transaction_id
        AND pair_type         = 1
        )
     ORDER BY mmt.transaction_source_type_id;
               
       
  CURSOR check_parent_batch_exist(v_batch_id NUMBER, v_parent_batch_id NUMBER) IS
    SELECT count(1)
      FROM gme_batch_genealogy
     WHERE batch_id = v_batch_id 
       AND parent_batch_id = v_parent_batch_id; 
       
  CURSOR check_init_record_exist(v_batch_id NUMBER) IS
    SELECT count(1)
      FROM gme_batch_genealogy
     WHERE batch_id = v_batch_id
       AND parent_batch_id = -1;
       
  CURSOR check_full_return(v_batch_id NUMBER,v_transaction_id NUMBER) IS
    SELECT SUM(mtla.transaction_quantity)
      FROM mtl_material_transactions mmt,
           mtl_transaction_lot_numbers mtla
     WHERE mmt.transaction_source_id = v_batch_id
       AND mmt.transaction_source_type_id = 5
       AND mmt.transaction_type_id IN (35,43)
       AND mmt.transaction_id = mtla.transaction_id
       AND mtla.lot_number IN (SELECT lot_number
                                 FROM mtl_transaction_lot_numbers mtlb
                                WHERE mtlb.transaction_id = v_transaction_id);  
                                
                                
                                   
              
  --END BUG 24457891 
      
  BEGIN
    --BEGIN BUG 24457891 
    FOR cur_gamt IN get_all_material_txns(p_batch_id) LOOP 
      IF cur_gamt.transaction_type_id = 35 THEN
        v_parent_batch_id := iot_get_parent_batch (cur_gamt.lot_number, cur_gamt.inventory_item_id,cur_gamt.organization_id) ;
        
        IF v_parent_batch_id NOT IN (-999,-9999) THEN
          OPEN check_parent_batch_exist(p_batch_id,v_parent_batch_id);
          FETCH check_parent_batch_exist INTO v_count;
          CLOSE check_parent_batch_exist;
          
          IF v_count = 0 THEN
           OPEN check_init_record_exist(p_batch_id);
           FETCH check_init_record_exist INTO v_count;
           CLOSE check_init_record_exist;
           
           IF v_count = 0 THEN
             batch_heat_insert(p_batch_id,NULL,v_parent_batch_id);  
           ELSIF v_count = 1 THEN
             iot_upd_parent_batch (p_batch_id, NULL, v_parent_batch_id);         
           END IF;                  
          END IF;
        END IF;  
                 
      ELSIF cur_gamt.transaction_type_id = 43 THEN
        OPEN check_full_return(p_batch_id,cur_gamt.transaction_id);
        FETCH check_full_return INTO v_total_quantity;
        CLOSE check_full_return;
        
        IF v_total_quantity = 0 THEN
          v_parent_batch_id := iot_get_parent_batch (cur_gamt.lot_number, cur_gamt.inventory_item_id,cur_gamt.organization_id) ;
          
          IF v_parent_batch_id NOT IN (-999,-9999) THEN
            DELETE FROM gme_batch_genealogy
             WHERE batch_id = p_batch_id
               AND parent_batch_id = v_parent_batch_id; 
          END IF;
        
        END IF;
      
      END IF; 
    END LOOP;
    --END BUG 24457891    
    
     
  
    /*v_parent_batch_id := iot_get_parent_batch (p_lot_number, p_inventory_item_id) ;
     dbms_output.put_line ('v_parent_batch_id :'||v_parent_batch_id);
     insert into heat_test values('iot_get_parent_batch p_lot_number ', p_lot_number);
    IF v_parent_batch_id <> - 9999 THEN
 
      BEGIN
        SELECT heat_number
        INTO v_parent_heat_number
        FROM gme_batch_genealogy
        WHERE batch_id = v_parent_batch_id;
      EXCEPTION
      WHEN no_data_found THEN
        v_parent_heat_number := NULL ;
      END GET_PARENT_HEAT_NUMBER;

      BEGIN
        SELECT DISTINCT heat_number,
          parent_batch_id
        INTO v_current_batch_heat_number,
          v_current_batch_prent_id
        FROM gme_batch_genealogy
        WHERE batch_id = p_batch_id;
      EXCEPTION
      WHEN no_data_found THEN
        v_current_batch_heat_number := NULL ;
      END GET_CURRENT_HEAT_DTLS;
       Logic to display Warning messages based on parent and child batch heat number matching
      BUG 24410352 Add the second condition so that parent_batch_id can be updated even if heat number is NULL
      IF v_parent_heat_number IS NULL AND v_current_batch_heat_number is not null THEN
        p_out                 := 'parent heat :'||v_parent_heat_number||', parent batch heat number null ';
        p_check               := 2;
      ELSE
        BUG 24410352 Add the second condition so that parent_batch_id can be updated even if heat number is NULL
        IF (v_parent_heat_number IS NOT NULL AND v_current_batch_heat_number IS NULL) OR 
           (v_parent_heat_number IS NULL AND v_current_batch_heat_number IS NULL AND v_current_batch_prent_id =-1 ) THEN
           p_out                 :='parent heat :'||v_parent_heat_number||' , current heat :'||NVL(v_current_batch_heat_number,'null') ||', warning : the parent batch heat number dose
           not match current batch heat number';
           p_check               :=2;
          GME_YIELD_CALCULATION_PVT.iot_upd_parent_batch (p_batch_id, v_parent_heat_number, v_parent_batch_id) ;
        ELSE
          IF v_parent_heat_number <> NVL (v_current_batch_heat_number, 0) THEN
            p_out                 := 'parent heat :'||v_parent_heat_number||' , current heat :'||NVL (v_current_batch_heat_number, 'null') || ', warning : the parent batch heat number dose not match current batch heat number';
            p_check               := 2;
          ELSE
            IF v_parent_heat_number = v_current_batch_heat_number THEN
              p_out                := 'parent heat :'||v_parent_heat_number||', current heat :'||NVL (v_current_batch_heat_number, 'null') || ',parent batch heat number same as current batch heat number ';
              p_check              := 1;
            ELSE
              IF (v_parent_heat_number IS NULL AND v_current_batch_heat_number IS NULL) THEN
                p_out                  := 'Parent Batch and Current Batch both Heat Number is null';
                p_check                := 3;
              END IF ;
            END IF;
          END IF ;
        END IF ;
      END IF ;
       If Batch is not found for Lot Number (Lot Number is not present in genealogy, lot is not created through batch)
    ELSE
      IF v_parent_batch_id =-999 THEN
        p_out             := 'There are multiple parent batches for the lot number and item';
        p_check           := 4;
      ELSE
        p_out   := 'Lot is not generated through batch, skipping for comparison for heat number';
        p_check := 5;
      END IF ;
    END IF;*/
  EXCEPTION
  WHEN OTHERS THEN
    show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.check_parent_heat', 'Exception is '||SQLERRM) ;
  END check_parent_heat;
/*###############################################################
# name
# get_yield_uom
# synopsis
# proc  get_yield_uom
# description
#  get_yield_uom for yield uom based on org id
# Sachin Ingle
###############################################################*/
  FUNCTION get_yield_uom(
      p_org_id IN NUMBER)
    RETURN VARCHAR2
  IS
    p_yield_uom VARCHAR2 (100) ;
  BEGIN
    SELECT muom.uom_code
    INTO p_yield_uom
    FROM gmd_parameters_dtl d,
      gmd_parameters_hdr h,
      mtl_units_of_measure muom
    WHERE d.parameter_id   = h.parameter_id
    AND d.parameter_type   = 1
    AND h.organization_id  = p_org_id
    AND muom.uom_class     = d.parameter_value
    AND parameter_name     = 'FM_YIELD_TYPE'
    AND muom.base_uom_flag = 'Y';
    RETURN p_yield_uom;
  EXCEPTION
  WHEN no_data_found THEN
    p_yield_uom := 'N/A';
    RETURN p_yield_uom;
  WHEN OTHERS THEN
    show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.get_yield_uom', 'Exception is '||SQLERRM) ;
  END get_yield_uom;
/*###############################################################
# name
# get_parent_heat
# synopsis
# proc  get_parent_heat
# description
#  get_parent_heat gets the parent heat number and parent batch id for particular batch
# Sachin Ingle
###############################################################*/
  PROCEDURE get_parent_heat(
      p_batch_id          IN NUMBER,
      p_lot_number        IN VARCHAR2,
      p_inventory_item_id IN NUMBER,
      x_parent_batch_id OUT NOCOPY NUMBER,
      x_parent_heat OUT NOCOPY     NUMBER)
  IS
    v_parent_batch_id           NUMBER ;
    v_parent_heat_number        VARCHAR2 (100) ;
    v_current_batch_heat_number VARCHAR2 (100) ;
    v_current_batch_prent_id    NUMBER ;
    v_organization_id           NUMBER;  -- BUG 24457891	    
  BEGIN
    v_organization_id := gme_common_pvt.g_organization_id; -- BUG 24457891    
    x_parent_batch_id    := iot_get_parent_batch (p_lot_number, p_inventory_item_id,v_organization_id) ;-- BUG 24457891
    IF v_parent_batch_id <> - 9999 THEN
      BEGIN
        --BUG 24457891 Add DISTINCT to get 1 row
        SELECT DISTINCT heat_number
        INTO x_parent_heat
        FROM gme_batch_genealogy
        WHERE batch_id = v_parent_batch_id;
      EXCEPTION
      WHEN no_data_found THEN
        v_parent_heat_number := - 9999;
      END ;
    END IF ;
  EXCEPTION
  WHEN OTHERS THEN
    show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.get_parent_heat', 'Exception is '||SQLERRM) ;
  END get_parent_heat;
/*###############################################################
# name
# GET_PARENT_HEAT
# synopsis
# func  GET_PARENT_HEAT
# description
#  GET_PARENT_HEAT gets the parent heat number
# Sachin Ingle
###############################################################*/
  FUNCTION get_parent_heat(
      p_lot_number        IN VARCHAR2,
      p_inventory_item_id IN NUMBER)
    RETURN VARCHAR2
  IS
    p_batch_id    NUMBER ;
    p_heat_number VARCHAR2 (100) ;
    v_organization_id           NUMBER;  -- BUG 24457891    
  BEGIN
    v_organization_id := gme_common_pvt.g_organization_id; -- BUG 24457891     
    p_batch_id := iot_get_parent_batch (p_lot_number, p_inventory_item_id,v_organization_id) ;-- BUG 24457891
    --BUG 24457891 Add DISTINCT to get 1 row    
    SELECT DISTINCT heat_number
    INTO p_heat_number
    FROM gme_batch_genealogy
    WHERE batch_id = p_batch_id;
    RETURN p_heat_number;
  EXCEPTION
  WHEN no_data_found THEN
    p_heat_number := - 9999;
    RETURN p_heat_number;
  WHEN OTHERS THEN
    show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.get_parent_heat', 'Exception is '||SQLERRM) ;
    p_heat_number := - 9999;
    RETURN p_heat_number;
  END get_parent_heat;
-- API to check if a product is installed or not.
-- API is now used to verify more that product installation and will return value as 'I' only if we want to introduce YMM features.
  FUNCTION install_check
    RETURN VARCHAR2
  IS
    l_installed      BOOLEAN;
    l_status         VARCHAR2 (10) := 'N';
    l_industry       VARCHAR2 (10) ;
    l_application_id NUMBER;
  BEGIN
    /* SELECT fa.application_id
    INTO  l_application_id
    FROM   fnd_Application fa
    WHERE  fa.application_short_name= p_application_short_name;
    l_installed := fnd_installation.get( appl_id => l_application_id, dep_appl_id => l_application_id,status => l_status, industry => l_industry );
    */
    --YJ: Todo: This variable will get value based on some profile or some other means. We need to revisit this API at later point of time.
    --l_status := 'I';
    RETURN g_install_flag;
  EXCEPTION
  WHEN NO_DATA_FOUND THEN
    l_status := 'N';
    RETURN l_status;
  END install_check;
/*###############################################################
# name
# TRUNC_TRANSFER_STEPS
# synopsis
# proc  TRUNC_TRANSFER_STEPS
# description
#  TRUNC_TRANSFER_STEPS deletes records from gme_batch_steps_quantity table when the step is unreleased.
#  API deletes the records after the step whihc is unrelaesd.
# Sachin Ingle
###############################################################*/
  PROCEDURE TRUNC_TRANSFER_STEPS(
      p_batch_id     IN NUMBER ,
      p_batchstep_id IN NUMBER )
  IS
    v_batch_id NUMBER ;
  BEGIN
    UPDATE gme_batch_steps
    SET PLANNED_STEP_YIELD    = NULL,
      PLANNED_STEP_CUM_YIELD  = NULL,
      PLANNED_TOTAL_CUM_YIELD = NULL,
      ACTUAL_STEP_YIELD       = NULL,
      ACTUAL_STEP_CUM_YIELD   = NULL,
      ACTUAL_TOTAL_CUM_YIELD  = NULL,
      SCRAP_PCT               = NULL,
      SCRAP_CUM_PCT           = NULL,
      SCRAP_TOTAL_CUM_PCT     = NULL
    WHERE batch_id            = p_batch_id
    AND batchstep_id         IN
      (SELECT from_step_id
      FROM gme_batch_steps_quantity
        CONNECT BY prior to_step_id = from_step_id
        START WITH from_step_id     = p_batchstep_id
      );
    DELETE
    FROM gme_batch_steps_quantity
    WHERE batch_id    = p_batch_id
    AND from_step_id IN
      (SELECT from_step_id
      FROM gme_batch_steps_quantity
        CONNECT BY prior to_step_id = from_step_id
        START WITH from_step_id     =p_batchstep_id
      );
  EXCEPTION
  WHEN OTHERS THEN
    show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.get_parent_heat', 'Exception is '||SQLERRM) ;
  END TRUNC_TRANSFER_STEPS;
/*###############################################################
# name
# unassociated_material_in
# synopsis
# func  unassociated_material_in
# description
#     1.0 the unassociated material coming out of the step is material out (product, by product) dose not include by product of type waste
# created by Sachin Ingle .kpit on 11/05/2015
###############################################################*/
  FUNCTION unassociated_material_in(
      p_batch_id NUMBER )
    RETURN NUMBER
  AS
    v_to_uom                VARCHAR2(100);
    v_from_uom              VARCHAR2(100);
    v_unassociated_material NUMBER:=0 ;
    v_batchstep_no          NUMBER ;
    v_batchstep_id          NUMBER ;
    CURSOR cur_material_details
    IS
      SELECT actual_qty,
        dtl_um,
        inventory_item_id item_id
      FROM gme_material_details
      WHERE material_detail_id NOT IN
        (SELECT material_detail_id
        FROM gme_batch_step_items
        WHERE batch_id =p_batch_id
        )
    AND batch_id  =p_batch_id
    AND line_type = - 1 
    AND NVL(contribute_yield_ind,'N') = 'Y';  --BUG 26256886
  BEGIN
    SELECT step_qty_um ,
      batchstep_no,
      batchstep_id
    INTO v_to_uom ,
      v_batchstep_no,
      v_batchstep_id
    FROM gme_batch_steps
    WHERE batchstep_no =
      ( SELECT MIN (batchstep_no) FROM gme_batch_steps WHERE batch_id=p_batch_id
      )
    AND batch_id =p_batch_id ;
    FOR rec_material_details IN cur_material_details
    LOOP
      v_unassociated_material := NVL (v_unassociated_material, 0) + GME_YIELD_CALCULATION_PVT.conv_uom (rec_material_details.item_id, rec_material_details.actual_qty, rec_material_details.dtl_um, v_to_uom) ;
    END LOOP;
    RETURN v_unassociated_material;
  EXCEPTION
  WHEN OTHERS THEN
    v_unassociated_material :=NULL;
    RETURN v_unassociated_material;
  END unassociated_material_in;
/*###############################################################
# name
# GET_FIRST_BATCHSTEP
# synopsis
# func  GET_FIRST_BATCHSTEP
# description
#     1.0 this api retunrs Y if the batchstep is the min step for the specific batch
# created by Sachin Ingle .kpit on 11/05/2015
###############################################################*/
  FUNCTION GET_FIRST_BATCHSTEP(
      p_batchstep_id NUMBER )
    RETURN VARCHAR2
  AS
    v_min_batchstep_id NUMBER ;
    v_flag             VARCHAR2(100);
  BEGIN
    SELECT batchstep_id--,
      -- BATCHSTEP_NO
    INTO v_min_batchstep_id
    FROM gme_batch_steps
    WHERE batchstep_no =
      (SELECT MIN (BATCHSTEP_NO)
      FROM gme_batch_steps
      WHERE batch_id=
        (SELECT batch_id FROM gme_batch_steps WHERE batchstep_id =p_batchstep_id
        )
      )
    AND batch_id =
      (SELECT batch_id FROM gme_batch_steps WHERE batchstep_id =p_batchstep_id
      ) ;
    IF p_batchstep_id=v_min_batchstep_id THEN
      v_flag        :='Y';
      RETURN v_flag;
    ELSE
      v_flag:='N';
      RETURN v_flag;
    END IF ;
  EXCEPTION
  WHEN OTHERS THEN
    v_flag:='E';
    RETURN v_flag;
  END GET_FIRST_BATCHSTEP;
/*###############################################################
# name
# unassociated_material_in
# synopsis
# func  unassociated_material_in
# description
#     1.0 the unassociated material coming out of the step is material out (product, by product) dose not include by product of type waste
# created by Sachin Ingle .kpit on 11/05/2015
###############################################################*/
  FUNCTION unassociated_material_out(
      p_batch_id NUMBER )
    RETURN NUMBER
  AS
    v_to_uom                VARCHAR2(100);
    v_from_uom              VARCHAR2(100);
    v_unassociated_material NUMBER :=0;
    v_batchstep_no          NUMBER ;
    v_batchstep_id          NUMBER ;
    CURSOR cur_material_details
    IS
      SELECT actual_qty,
        dtl_um,
        inventory_item_id item_id
      FROM gme_material_details
      WHERE material_detail_id NOT IN
        (SELECT material_detail_id
        FROM gme_batch_step_items
        WHERE batch_id =p_batch_id
        )
    AND line_type in (1,2) --Bug 22318890 : Fixed the issue of By Product not being considered for yield
	AND NVL (by_product_type, 'Y') = 'Y' -- Bug 22361054  : By-product type Sample, Rework and Waste should not be considered for yield
    AND batch_id  =p_batch_id ;
  BEGIN
    SELECT step_qty_um ,
      batchstep_no,
      batchstep_id
    INTO v_to_uom ,
      v_batchstep_no,
      v_batchstep_id
    FROM gme_batch_steps
    WHERE batchstep_no =
      ( SELECT MAX (batchstep_no) FROM gme_batch_steps WHERE batch_id=p_batch_id
      )
    AND batch_id =p_batch_id ;
    FOR rec_material_details IN cur_material_details
    LOOP
      v_unassociated_material := NVL (v_unassociated_material, 0) + GME_YIELD_CALCULATION_PVT.conv_uom (rec_material_details.item_id, rec_material_details.actual_qty, rec_material_details.dtl_um, v_to_uom) ;
    END LOOP;
    RETURN v_unassociated_material;
  EXCEPTION
  WHEN OTHERS THEN
    v_unassociated_material :=NULL;
    RETURN v_unassociated_material;
  END unassociated_material_out;
/*###############################################################
# name
# GET_FIRST_BATCHSTEP
# synopsis
# func  GET_FIRST_BATCHSTEP
# description
#     1.0 this api retunrs Y if the batchstep is the min step for the specific batch
# created by Sachin Ingle .kpit on 11/05/2015
###############################################################*/
  FUNCTION GET_LAST_BATCHSTEP(
      p_batchstep_id NUMBER )
    RETURN VARCHAR2
  AS
    v_max_batchstep_id NUMBER ;
    v_flag             VARCHAR2(100);
  BEGIN
    SELECT batchstep_id--,
      -- BATCHSTEP_NO
    INTO v_max_batchstep_id
    FROM gme_batch_steps
    WHERE batchstep_no =
      (SELECT MAX (BATCHSTEP_NO)
      FROM gme_batch_steps
      WHERE batch_id=
        (SELECT batch_id FROM gme_batch_steps WHERE batchstep_id =p_batchstep_id
        )
      )
    AND batch_id =
      (SELECT batch_id FROM gme_batch_steps WHERE batchstep_id =p_batchstep_id
      ) ;
    IF p_batchstep_id=v_max_batchstep_id THEN
      v_flag        :='Y';
      RETURN v_flag;
    ELSE
      v_flag:='N';
      RETURN v_flag;
    END IF ;
  EXCEPTION
  WHEN OTHERS THEN
    v_flag:='E';
    RETURN v_flag;
  END GET_LAST_BATCHSTEP;
/*###############################################################
# name
# unassociated_material_in
# synopsis
# func  unassociated_material_in
# description
#     1.0 the unassociated material coming out of the step is material out (product, by product) dose not include by product of type waste
# created by Sachin Ingle .kpit on 11/05/2015
###############################################################*/
  FUNCTION unassociated_material_in(
      p_transaction_id IN NUMBER,
      p_batch_id       IN NUMBER )
    RETURN NUMBER
  AS
    v_to_uom                VARCHAR2(100);
    v_from_uom              VARCHAR2(100);
    v_unassociated_material NUMBER:=0 ;
    v_batchstep_no          NUMBER ;
    v_batchstep_id          NUMBER ;
    CURSOR cur_material_details
    IS
      SELECT ABS(mmt.TRANSACTION_QUANTITY) actual_qty,
        gmd.dtl_um,
        gmd.inventory_item_id item_id
      FROM gme_material_details gmd,
        mtl_material_transactions mmt
      WHERE gmd.material_detail_id = mmt.trx_source_line_id
      AND mmt.transaction_id       = p_transaction_id
      AND material_detail_id NOT  IN
        (SELECT material_detail_id
        FROM gme_batch_step_items
        WHERE batch_id = p_batch_id
        )
    AND batch_id  = p_batch_id
    AND line_type = - 1 
    AND NVL(gmd.contribute_yield_ind,'N') = 'Y';  --BUG 26256886
  BEGIN
    SELECT step_qty_um ,
      batchstep_no,
      batchstep_id
    INTO v_to_uom ,
      v_batchstep_no,
      v_batchstep_id
    FROM gme_batch_steps
    WHERE batchstep_no =
      ( SELECT MIN (batchstep_no) FROM gme_batch_steps WHERE batch_id=p_batch_id
      )
    AND batch_id =p_batch_id ;
    FOR rec_material_details IN cur_material_details
    LOOP
      v_unassociated_material := NVL (v_unassociated_material, 0) + GME_YIELD_CALCULATION_PVT.conv_uom (rec_material_details.item_id, rec_material_details.actual_qty, rec_material_details.dtl_um, v_to_uom) ;
    END LOOP;
    RETURN v_unassociated_material;
  EXCEPTION
  WHEN OTHERS THEN
    v_unassociated_material :=NULL;
    RETURN v_unassociated_material;
  END unassociated_material_in;

/*###############################################################
# name
# cal_ymm_yield 
# description
# Get all the fields for YMM .
###############################################################*/
  PROCEDURE cal_ymm_yield(
      p_batchstep_id IN NUMBER,
      x_material_in OUT NOCOPY NUMBER,
      x_material_out OUT NOCOPY NUMBER,
      x_transfer_in OUT NOCOPY NUMBER,
      x_transfer_out OUT NOCOPY NUMBER )
  IS
    v_material_in NUMBER ;
    v_material_out NUMBER ;
    v_transfer_in NUMBER ;
    v_transfer_out NUMBER ;
  BEGIN
show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.cal_ymm_yield', ' begin ymm yield calculations ') ;
v_material_in  := material_in (p_batchstep_id) ;
v_material_out := material_out (p_batchstep_id) ;
v_transfer_in  := get_inter_in (p_batchstep_id) ;
v_transfer_out := get_inter_out (p_batchstep_id) ;
x_material_in  := v_material_in;
x_material_out := v_material_out;
x_transfer_in  := v_transfer_in;
x_transfer_out := v_transfer_out;
  EXCEPTION
  WHEN OTHERS THEN
    show_log (g_log_type, 'GME_YIELD_CALCULATION_PVT.CAL_YMM_YIELD', 'Exception is '||SQLERRM) ;
  END cal_ymm_yield;
  
  ------------------------------------------------------------------
   --GME BATCH STEP QUANTITY API
  -------------------------------------------------------------------

    /*###############################################################
  # name
  # insert_batch_step_quantity
  # synopsis
  # proc  insert_batch_step_quantity
  # description
  #     to insert GME Batch Step Quantity for step transfer details.
  # created by srmacha
  ###############################################################*/
  PROCEDURE insert_batch_step_quantity(
      p_batch_step_trnsfr_qty_rec  IN  batch_step_trnsfr_quantity_rec)
  IS

----Local Variables
  l_transaction_date DATE;
  l_batch_id NUMBER;
  l_batch_step_id NUMBER;
  l_quantity_out NUMBER;
  l_quantity_in NUMBER;
  l_to_batch_step_id NUMBER;
  


  BEGIN
   l_batch_id := p_batch_step_trnsfr_qty_rec.batch_id;
   l_batch_step_id:= p_batch_step_trnsfr_qty_rec.from_step_id;
    l_transaction_date:= p_batch_step_trnsfr_qty_rec.transaction_date;
    l_to_batch_step_id:= p_batch_step_trnsfr_qty_rec.to_step_id;
    l_quantity_out := p_batch_step_trnsfr_qty_rec.step_qty_in_from_uom;
    l_quantity_in := p_batch_step_trnsfr_qty_rec.step_qty_in_to_uom;
------------------------------------------------------------------------------

      INSERT INTO GME_BATCH_STEPS_QUANTITY
         (trans_id,
          batch_id,
    			from_step_id,
    			to_step_id,
    			step_qty_in_from_uom,
    			step_qty_in_to_uom ,
    			transaction_date ,
    			created_by ,
    			creation_date ,
    			last_updated_by ,
    		  last_update_date,
          last_update_login)
      VALUES (GME_BATCH_STEPS_QUANTITY_S.NEXTVAL,l_batch_id,
				l_batch_step_id,l_to_batch_step_id,
				l_quantity_out,l_quantity_in,
				l_transaction_date,TO_NUMBER (FND_PROFILE.VALUE ('USER_ID')),
				SYSDATE,TO_NUMBER (FND_PROFILE.VALUE ('USER_ID')),
				SYSDATE,TO_NUMBER (FND_PROFILE.VALUE ('LOGIN_ID')));

  END insert_batch_step_quantity;


     /*###############################################################
  # name
  # update_batch_step_quantity
  # synopsis
  # proc  update_batch_step_quantity
  # description
  #     to update GME Batch Step Quantity for step transfer details.
  # created by
  ###############################################################*/
   PROCEDURE update_batch_step_quantity(
       p_batch_step_trnsfr_qty_rec  IN  batch_step_trnsfr_quantity_rec) IS
----Local Variables
  l_transaction_date DATE;
  l_batch_id NUMBER;
  l_batch_step_id NUMBER;
  l_quantity_out NUMBER;
  l_quantity_in NUMBER;
  l_to_batch_step_id NUMBER;
  


  BEGIN
   l_batch_id := p_batch_step_trnsfr_qty_rec.batch_id;
   l_batch_step_id:= p_batch_step_trnsfr_qty_rec.from_step_id;
    l_transaction_date:= p_batch_step_trnsfr_qty_rec.transaction_date;
    l_to_batch_step_id:= p_batch_step_trnsfr_qty_rec.to_step_id;
    l_quantity_out := p_batch_step_trnsfr_qty_rec.step_qty_in_from_uom;
    l_quantity_in := p_batch_step_trnsfr_qty_rec.step_qty_in_to_uom;
------------------------------------------------------------------------------

      UPDATE GME_BATCH_STEPS_QUANTITY
                  SET
                  batch_id = l_batch_id,
                  from_step_id =l_batch_step_id,
                  to_step_id =l_to_batch_step_id,
                  step_qty_in_from_uom = l_quantity_out,
                  step_qty_in_to_uom= l_quantity_in ,
                  transaction_date=l_transaction_date,
                  last_updated_by=TO_NUMBER (FND_PROFILE.VALUE ('USER_ID')),
                  last_update_date=SYSDATE,
                  last_update_login=TO_NUMBER (FND_PROFILE.VALUE ('LOGIN_ID'))
                  WHERE batch_id = l_batch_id AND
                  from_step_id=l_batch_step_id AND
                  to_step_id = l_to_batch_step_id;

  END update_batch_step_quantity;
    /*###############################################################
  # name
  # delete_batch_step_quantity
  # synopsis
  # procedure  delete_batch_step_quantity
  # description
  #     to delete GME Batch Step Quantity
  # created by
  ###############################################################*/
  PROCEDURE delete_batch_step_quantity(
      p_batch_step_trnsfr_qty_rec  IN  batch_step_trnsfr_quantity_rec ) IS

----Local Variables
  l_transaction_date DATE;
  l_batch_id NUMBER;
  l_from_batchstep_id NUMBER;
  l_to_batchstep_id NUMBER;
  

  BEGIN
  l_batch_id := p_batch_step_trnsfr_qty_rec.batch_id;
  l_from_batchstep_id:= p_batch_step_trnsfr_qty_rec.from_step_id;
  l_to_batchstep_id:= p_batch_step_trnsfr_qty_rec.to_step_id;
  
------------------------------------------------------------------------------
             DELETE FROM GME_BATCH_STEPS_QUANTITY
                   WHERE batch_id = l_batch_id AND from_step_id=l_from_batchstep_id AND to_step_id = l_to_batchstep_id;
     
  END delete_batch_step_quantity;
   /*###############################################################
  # name
  # validate_trans_date
  # synopsis
  # Function  validate_trans_date
  # description
  #     to dvalidate batch step quantitybtransfer transaction date.
  # created by
  ###############################################################*/

  FUNCTION validate_trans_date(p_trans_date IN DATE,p_batch_id IN NUMBER,p_batch_step_id IN NUMBER,
                                p_batch_step_status IN NUMBER)
  RETURN BOOLEAN
  IS
  l_actual_step_start_date DATE;
  l_actual_step_cmplt_date DATE;
  BEGIN
  select ACTUAL_START_DATE into l_actual_step_start_date from GME_BATCH_STEPS where batch_id=p_batch_id and batchstep_id=p_batch_step_id;
  --bug 24410338 future date is not allowed.also if batch step is completed it should be between start date and completion date.
  IF p_trans_date < l_actual_step_start_date or p_trans_date > SYSDATE THEN
  RETURN FALSE;
  END IF;
  IF p_batch_step_status = 3 THEN
    select ACTUAL_CMPLT_DATE into l_actual_step_cmplt_date from GME_BATCH_STEPS where batch_id=p_batch_id and batchstep_id=p_batch_step_id;
    IF p_trans_date > l_actual_step_cmplt_date THEN
    RETURN FALSE;
    END IF;
  END IF;
  RETURN TRUE;
  END validate_trans_date;

/*###############################################################
  # name
  # batchsteps_qty_conversion
  # synopsis
  # Function  batchsteps_qty_conversion
  # description
  #     to convert from step quantity into to atep quantity UOM.
  # created by
  ###############################################################*/

  FUNCTION batchsteps_qty_conversion(p_quantity_out IN NUMBER,p_quantity_out_uom IN VARCHAR2,p_quantity_in_uom IN VARCHAR2,p_batch_id IN NUMBER)
   RETURN NUMBER
    IS
    l_conv NUMBER;
    l_org_id NUMBER;
    l_item_id NUMBER;
     BEGIN
     select organization_id into l_org_id from gme_batch_header where batch_id=p_batch_id;
     
     SELECT primary_item_id INTO l_item_id FROM wip_entities WHERE wip_entity_id =p_batch_id and entity_type=10;

    l_conv := inv_convert.inv_um_convert ( item_id => l_item_id,organization_id => l_org_id, precision  => 5, from_quantity => p_quantity_out,
              from_unit => p_quantity_out_uom, to_unit => p_quantity_in_uom, from_name => NULL, to_name => NULL) ;

    RETURN l_conv;
  END BATCHSTEPS_QTY_CONVERSION;




 --BUG 26248999 Add function get_parent_material_line  
 FUNCTION get_parent_material_line(p_inventory_item_id IN NUMBER,
                                   p_organization_id   IN NUMBER,
                                   p_lot_number        IN VARCHAR2)
 RETURN NUMBER
   IS
   
   
    v_material_detail_id          NUMBER:= -9999 ;
    v_object_id             NUMBER;
    v_procedure_name        VARCHAR2(100) := 'GME_YIELD_CALCULATION_PVT.GET_PARENT_MATERIAL_LINE';


    v_wip_cmplt_txn_count   NUMBER := 0;    
    l_count                 NUMBER;  --current processing object index in l_lot_object_tbl
    l_last_index            NUMBER; --last index of the parent object id in l_lot_object_tbl
    l_exist_object boolean := false; 
    
    TYPE LOT_OBJECT_TBL IS   TABLE OF BINARY_INTEGER  INDEX BY BINARY_INTEGER; 
    l_lot_object_tbl        LOT_OBJECT_TBL;
    
       
    CURSOR get_parent_lot_objs ( l_object_id   NUMBER) 
    IS
      SELECT distinct
             object_id,
             object_type,
             origin_txn_id
        FROM mtl_object_genealogy mog
       WHERE parent_object_id = l_object_id
         AND EXISTS (
              SELECT  1 
                FROM mtl_material_transactions mmt
              WHERE mmt.transaction_id = mog.origin_txn_id
              AND mmt.inventory_item_id = p_inventory_item_id)
        ORDER BY object_type DESC,origin_txn_id; --5 Job, 1 -Inv   
 BEGIN
    BEGIN 
      SELECT gen_object_id
        INTO v_object_id
        FROM mtl_lot_numbers mln
       WHERE mln.lot_number = p_lot_number
         AND mln.inventory_item_id = p_inventory_item_id
         AND mln.organization_id = p_organization_id;
         
     EXCEPTION 
       WHEN OTHERS THEN
      
      
       show_log (g_log_type,v_procedure_name, 'Invalid Lot Exit') ;
      
       return -9999;
     END; 

    l_lot_object_tbl(1) := v_object_id; 

    l_count := 0;
    l_last_index := 1;    
    
    WHILE (l_lot_object_tbl.count > l_count) LOOP  -- if there is any unprocessed data in the array
      --show_log (g_log_type,v_procedure_name, 'Current table object count: l_lot_object_tbl.count  ' || l_lot_object_tbl.count) ; 
    
      l_count := l_count + 1;
      v_object_id := l_lot_object_tbl(l_count);
      
      show_log (g_log_type,v_procedure_name, 'Current processing object_id: ' || v_object_id) ;
        
      FOR parent_lot_rec IN get_parent_lot_objs( v_object_id) LOOP
        --show_log (g_log_type,v_procedure_name, 'parent_lot_rec origin_txn_id: ' || parent_lot_rec.origin_txn_id || ' object_id: ' || parent_lot_rec.object_id) ; 
          
        IF parent_lot_rec.object_type = 5 THEN --job/scheudle source 
         SELECT COUNT(1)
           INTO v_wip_cmplt_txn_count
           FROM mtl_material_transactions mmt
          WHERE parent_lot_rec.origin_txn_id = mmt.transaction_id
            AND mmt.transaction_type_id IN (44,1002)
            AND transaction_action_id = 31
            AND transaction_source_type_id = 5
            AND mmt.inventory_item_id = p_inventory_item_id;

          IF ( v_wip_cmplt_txn_count > 0 ) THEN                
            begin 
                SELECT DISTINCT gmd.material_detail_id
                  INTO v_material_detail_id
                  FROM mtl_material_transactions mtl,
                       gme_material_details gmd
                 WHERE mtl.transaction_id = parent_lot_rec.origin_txn_id
                   AND mtl.trx_source_line_id = gmd.material_detail_id
                   AND mtl.transaction_source_id = gmd.batch_id
                   AND ROWNUM < 2;
                            
            exception when others then
              null;
            end; 
                    
            if v_material_detail_id <> -9999 then
              show_log (g_log_type,v_procedure_name, 'FOUND v_material_detail_id ' || v_material_detail_id || ' delete all objects in l_lot_object_tbl ') ;
              l_lot_object_tbl.delete;
              RETURN v_material_detail_id;
            end if; --v_parent_batch <> -9999
                    
          END IF; --v_wip_cmplt_txn_count>0

        END IF;--parent_lot_rec.object_type = 5 
            
            -- check if current l_lot_object_tbl already contains this parent_object_id before adding it to the table
			
        l_exist_object := false; 
        FOR i IN 1 .. l_lot_object_tbl.COUNT LOOP
          if parent_lot_rec.object_id = l_lot_object_tbl(i) then 
            --show_log (g_log_type,v_procedure_name, 'Found  ' || parent_lot_rec.object_id || ' in l_lot_object_tbl index ' || i) ;
            l_exist_object := true; 
            EXIT; 
          end if ; 
        END LOOP;
        if (NOT l_exist_object ) then 
          --show_log (g_log_type,v_procedure_name, 'Add parent object id  ' || parent_lot_rec.object_id) ;        
          l_last_index := l_last_index + 1;
          l_lot_object_tbl( l_last_index) := parent_lot_rec.object_id;
        END IF; 
          	
      END LOOP;
        
      EXIT WHEN (l_lot_object_tbl.count = 0);
        show_log (g_log_type,v_procedure_name,'Compelete processing lot object index '|| l_count || ' , object_id  ' || l_lot_object_tbl(l_count)) ; 
       -- l_lot_object_tbl.delete(l_count); --delete current processing object
    END LOOP;    
    RETURN v_material_detail_id;
 
 END get_parent_material_line;  
 
  
END GME_YIELD_CALCULATION_PVT;  

/
-- SHOW ERRORS PACKAGE GME_YIELD_CALCULATION_PVT;

COMMIT;
EXIT;
