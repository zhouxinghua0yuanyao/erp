REM dbdrv: sql ~PROD ~PATH ~FILE none none none package &phase=pls \
REM dbdrv: checkfile(120.0.12010000.24=120.0.12020000.14):~PROD:~PATH:~FILE 
SET VERIFY OFF
WHENEVER SQLERROR EXIT FAILURE ROLLBACK;
WHENEVER OSERROR  EXIT FAILURE ROLLBACK;
REM +======================================================================+    
REM | Copyright (c) 1999, 2016 Oracle Corporation Redwood Shores, California, USA|    
REM |                       All rights reserved.                           |    
REM +======================================================================+    
REM                                                                             
REM
REM FILE NAME: 		GME_YIELD_CALCULATION_PVT_S.pls
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

CREATE OR REPLACE PACKAGE gme_yield_calculation_pvt AUTHID CURRENT_USER AS
  /* $Header: GME_YIELD_CALCULATION_PVT_S.pls 120.0.12020000.14 2016/12/21 06:07:23 shalchen noship $ */
  g_log_type NUMBER :=1;
  g_install_flag VARCHAR2(1):='I';
  
  Type batch_step_trnsfr_quantity_rec 
  IS 
  RECORD(
    			trans_id NUMBER,
          batch_id NUMBER,
          from_step_id NUMBER,
    			to_step_id NUMBER,
    			step_qty_in_from_uom NUMBER,
          step_qty_in_to_uom NUMBER,
    			transaction_date DATE

   );
type step_details_record
IS
  record
  (
    inventory_item_id       NUMBER ,
    batchstep_id            NUMBER,
    material_detail_id      NUMBER,
    actual_step_cum_yield   NUMBER,
    actual_step_yield       NUMBER,
    actual_total_cum_yield  NUMBER,
    planned_step_yield      NUMBER,
    planned_step_cum_yield  NUMBER,
    planned_total_cum_yield NUMBER,
    scrap_pct               NUMBER,
    scrap_cum_pct           NUMBER,
    scrap_total_cum_pct     NUMBER );
type step_yield_rec
IS
  record
  (
    batchstep_id gme_batch_steps.batchstep_id%type,
    actual_step_yield       NUMBER ,
    actual_step_cum_yield   NUMBER,
    actual_total_cum_yield  NUMBER ,
    planned_step_yield      NUMBER,
    planned_step_cum_yield  NUMBER,
    planned_total_cum_yield NUMBER,
    scrap_pct               NUMBER,
    scrap_cum_pct           NUMBER,
    scrap_total_cum_pct     NUMBER,
    inter_in                NUMBER,
    inter_out               NUMBER );
type heat_rec
IS
  record
  (
    l_current_batch_id   NUMBER ,
    l_parent_heat_number VARCHAR2(100),
    l_parent_batch_id    NUMBER );
  gme_batch_step_rec gme_batch_steps%rowtype ;
  FUNCTION scrap_out(
      p_batchstep_id IN gme_batch_steps.batchstep_id%type)
    RETURN NUMBER;
  FUNCTION uom_conversion(
      item_id  IN NUMBER,
      from_uom IN VARCHAR2,
      org_id   IN NUMBER )
    RETURN NUMBER ;
  PROCEDURE yield_calc(
      p_batch_id     IN NUMBER,
      p_batchstep_id IN NUMBER DEFAULT NULL,
      p_source_type  IN VARCHAR2 DEFAULT 'CP',
      p_yield_rec OUT NOCOPY step_yield_rec );
  PROCEDURE yield_calc(
      p_batch_id     IN NUMBER ,
      p_batchstep_id IN NUMBER DEFAULT NULL,
      p_source_type  IN VARCHAR2 DEFAULT 'CP');
  --procedure yield_calc(p_batch_no number);
  PROCEDURE calc_batch_yield(
      p_batch_id IN NUMBER);
  PROCEDURE calc_plan_yield(
      p_batch_id     IN NUMBER,
      p_batchstep_id IN NUMBER DEFAULT NULL,
      p_source_type  IN VARCHAR2 DEFAULT 'CP',
      p_yield_rec OUT NOCOPY step_yield_rec );
  --procedure calc_plan_yield (p_batch_no in number);
  PROCEDURE calc_grand_plan_yield(
      p_batch_id     IN NUMBER,
      p_batchstep_id IN NUMBER DEFAULT NULL,
      p_source_type  IN VARCHAR2 DEFAULT 'CP',
      p_yield_rec OUT nocopy step_yield_rec);
  PROCEDURE calc_heat_yield(
      p_heat_number IN VARCHAR2,
      p_heat_yield OUT NOCOPY NUMBER );
  FUNCTION calc_heat_yield(
      p_batch_id VARCHAR2)
    RETURN NUMBER ;
  PROCEDURE calc_scrap_pct(
      p_batch_no NUMBER );
  FUNCTION get_step_total_out_qty(
      p_batchstep_id IN gme_batch_steps.batchstep_id%type )
    RETURN NUMBER ;
  FUNCTION get_step_total_in_qty(
      p_batchstep_id IN gme_batch_steps.batchstep_id%type)
    RETURN NUMBER;
  FUNCTION material_in(
      p_batchstep_id IN gme_batch_steps.batchstep_id%type)
    RETURN NUMBER;
  FUNCTION get_inter_in(
      p_batchstep_id IN gme_batch_steps.batchstep_id%type)
    RETURN NUMBER;
  FUNCTION get_inter_out(
      p_batchstep_id IN gme_batch_steps.batchstep_id%type)
    RETURN NUMBER;
  FUNCTION material_out(
      p_batchstep_id IN gme_batch_steps.batchstep_id%type)
    RETURN NUMBER;
  FUNCTION process_item_unit_cost(
      p_inventory_item_id IN NUMBER,
      p_organization_id   IN NUMBER,
      p_transaction_date  IN DATE )
    RETURN NUMBER;
  FUNCTION iot_get_parent_batch(
      p_lot_number        IN VARCHAR2,
      p_inventory_item_id IN NUMBER,
      p_organization_id   IN NUMBER)  --BUG 24457891	Add Input parameter p_organization_id
    RETURN NUMBER;
  -- yield calculation formula procedures
  PROCEDURE step_yield_frm(
      p_total_in  IN NUMBER,
      p_total_out IN NUMBER,
      p_step_yield OUT NOCOPY NUMBER );
  PROCEDURE step_cum_yield_frm(
      p_inter_in              IN NUMBER ,
      p_step_cum_input_part_1 IN NUMBER ,
      p_material_in           IN NUMBER ,
      p_total_out             IN NUMBER ,
      x_step_cum_yield OUT NOCOPY NUMBER );
  PROCEDURE step_grand_cum_yield(
      p_inter_in               IN NUMBER ,
      p_step_grand_input_part1 IN NUMBER ,
      p_grand_input            IN NUMBER ,
      p_total_out              IN NUMBER ,
      p_step_grand_cum_yield OUT NOCOPY NUMBER );
  PROCEDURE step_plan_cum_yield(
      p_plan_yield                 IN NUMBER ,
      p_prev_step_plan_cum_yield   IN NUMBER ,
      p_inter_in                   IN NUMBER ,
      p_prev_step_actual_cum_yield IN NUMBER ,
      p_material_in                IN NUMBER ,
      x_step_plan_cum_yield OUT NOCOPY NUMBER );
  PROCEDURE step_scrap_pct(
      p_total_in  IN NUMBER ,
      p_scrap_out IN NUMBER ,
      step_scrap_pct OUT NOCOPY NUMBER );
  PROCEDURE step_cum_scrap_pct(
      p_inter_in                IN NUMBER ,
      p_prev_step_cum_yield     IN NUMBER ,
      p_material_in             IN NUMBER ,
      p_prev_step_cum_scrap_per IN NUMBER ,
      p_scrap_out               IN NUMBER ,
      p_step_cum_scrap_pct OUT NOCOPY NUMBER );
  PROCEDURE step_grand_cum_scrap_pct(
      p_inter_in                  IN NUMBER ,
      p_prev_step_cum_yield       IN NUMBER ,
      p_material_in               IN NUMBER ,
      p_prev_step_grand_scrap_per IN NUMBER ,
      p_grand_input               IN NUMBER ,
      p_grand_input_2             IN NUMBER ,
      p_scrap_out                 IN NUMBER ,
      p_grand_cum_scrap_pct OUT NOCOPY NUMBER );
  PROCEDURE update_gem_batch_steps(
      p_gme_batch_step_rec IN step_yield_rec,
      x_status OUT NOCOPY VARCHAR2 );
  FUNCTION get_total_product_out(
      p_batch_id IN NUMBER )
    RETURN NUMBER;

  --insert the batch data in gme_batch_genealogy table
  PROCEDURE batch_heat_insert(
      p_batch_id        IN gme_batch_genealogy.batch_id%type,
      p_heat_number     IN gme_batch_genealogy.heat_number%type,
      p_parent_batch_id IN gme_batch_genealogy.parent_batch_id%type);
  PROCEDURE batch_heat_update(
      p_batch_id    IN gme_batch_genealogy.batch_id%type,
      p_heat_number IN gme_batch_genealogy.heat_number%type );
  -- procedure is used to update the parent batch id and parent heat number in gme_batch_genealogy table
  PROCEDURE iot_upd_parent_batch(
      p_batch_id           IN gme_batch_genealogy.batch_id%type,
      p_parent_heat_number IN gme_batch_genealogy.heat_number%type,
      p_parent_batch_id    IN gme_batch_genealogy.parent_batch_id%type );
  FUNCTION conv_uom(
      p_item_id       IN NUMBER DEFAULT NULL ,
      p_from_quantity IN NUMBER ,
      p_from_unit     IN VARCHAR2 ,
      p_to_unit       IN VARCHAR2)
    RETURN NUMBER ;
  PROCEDURE validate_heat(
      p_batch_id          IN NUMBER,
      p_lot_number        IN VARCHAR2,
      p_inventory_item_id IN NUMBER ,
      v_out OUT NOCOPY NUMBER,
      p_heat_rec OUT NOCOPY heat_rec );
  FUNCTION get_yield_uom(
      p_org_id IN NUMBER )
    RETURN VARCHAR2;
  PROCEDURE check_parent_heat(
      p_batch_id          IN NUMBER,
      p_lot_number        IN VARCHAR2,
      p_inventory_item_id IN NUMBER ,
      p_out OUT NOCOPY   VARCHAR2,
      p_check OUT NOCOPY NUMBER );
  PROCEDURE get_parent_heat(
      p_batch_id          IN NUMBER,
      p_lot_number        IN VARCHAR2,
      p_inventory_item_id IN NUMBER ,
      x_parent_batch_id OUT NOCOPY NUMBER,
      x_parent_heat OUT NOCOPY     NUMBER );
  FUNCTION get_parent_heat(
      p_lot_number        IN VARCHAR2,
      p_inventory_item_id IN NUMBER )
    RETURN VARCHAR2;
  PROCEDURE compute_yield(
      errbuf OUT NOCOPY  VARCHAR2,
      retcode OUT NOCOPY VARCHAR2,
      p_org_id   IN NUMBER,
      p_batch_no IN VARCHAR2 DEFAULT NULL );
  PROCEDURE compute_yield(
      p_batch_id IN NUMBER DEFAULT NULL);
  ---API to cal YMM EDR fields   begin--
 PROCEDURE cal_ymm_yield(
      p_batchstep_id IN NUMBER,
      x_material_in OUT NOCOPY NUMBER,
      x_material_out OUT NOCOPY NUMBER,
      x_transfer_in OUT NOCOPY NUMBER,
      x_transfer_out OUT NOCOPY NUMBER); 
 ---API to cal YMM EDR fields end--
  -- API to check if a product is installed or not
  FUNCTION install_check
    RETURN VARCHAR2;
  -- This API is to delete records from transfer block for the step (and its downstream step) which has been unreleased from WIP to Pending
  PROCEDURE TRUNC_TRANSFER_STEPS(
      p_batch_id     IN NUMBER ,
      p_batchstep_id IN NUMBER );
  -- This API is going to give intermediate in for to_step, that is coming from from_step (and not from all the incoming steps)
  FUNCTION Get_inter_in_from_each_step(
      p_from_batchstep_id IN gme_batch_steps.batchstep_id%TYPE,
      p_to_batchstep_id   IN gme_batch_steps.batchstep_id%type)
    RETURN NUMBER;--the unassociated material coming out of the step
  FUNCTION unassociated_material_in(
      p_batch_id NUMBER )
    RETURN NUMBER ;
  -- this api retunrs Y if the batchstep is the min step for the specific batch
  FUNCTION GET_FIRST_BATCHSTEP(
      p_batchstep_id NUMBER )
    RETURN VARCHAR2;
  FUNCTION unassociated_material_out(
      p_batch_id NUMBER )
    RETURN NUMBER ;
  -- this api retunrs Y if the batchstep is the MAX step for the specific batch
  FUNCTION GET_LAST_BATCHSTEP(
      p_batchstep_id NUMBER )
    RETURN VARCHAR2;
  FUNCTION unassociated_material_in(p_transaction_id IN number, p_batch_id IN  number  ) return number;
   ----------------------------------------------------------------------
   -- GME_BATCH_STEP_QUANTITY API
   -------------------------------------------------------------------

    


-- ============================================================================
--  Name        : insert_batch_step_quantity
--  Description : This procedure is used to insert all the batch step transfer details
--                into GME_BATCH_STEPS_QUANTITY table and validates all the required
--                validations before insertion.
--  Parameters:
--        IN    :
--                p_batch_step_trnsfr_qty_rec  IN  batch_step_trnsfr_quantity_rec
--
--  ============================================================================


    PROCEDURE insert_batch_step_quantity(
      p_batch_step_trnsfr_qty_rec  IN  batch_step_trnsfr_quantity_rec);

-- ============================================================================
--  Name        : update_batch_step_quantity
--  Description : This procedure is used to update all the batch step transfer details
--                of GME_BATCH_STEPS_QUANTITY table and validates all the required
--                validations before updation.
--  Parameters:
--        IN    :
--               p_batch_step_trnsfr_qty_rec  IN  batch_step_trnsfr_quantity_rec
--
--        OUT   :
--  ============================================================================

    PROCEDURE update_batch_step_quantity(p_batch_step_trnsfr_qty_rec  IN  batch_step_trnsfr_quantity_rec);

-- ============================================================================
--  Name        : delete_batch_step_quantity
--  Description : This procedure is used to delete all the input batch step transfer details
--                of GME_BATCH_STEPS_QUANTITY table and validates all the required
--                validations before deletion.
--  Parameters:
--        IN    :
--               p_batch_step_trnsfr_qty_rec  IN  batch_step_trnsfr_quantity_rec
--
--        OUT   :
--  ============================================================================

    PROCEDURE delete_batch_step_quantity(
      p_batch_step_trnsfr_qty_rec  IN  batch_step_trnsfr_quantity_rec);

-- ============================================================================
--  Name        : validate_trans_date
--  Description : This function is used to validate to step transaction date with
--                from step actual start date .
--  Parameters:
--        IN    :
--                p_trans_date IN DATE
--                p_batch_id IN NUMBER
--                p_batch_step_id IN NUMBER
--                p_batch_step_status IN NUMBER
--
--  ============================================================================

    FUNCTION validate_trans_date(
    p_trans_date IN DATE,
    p_batch_id IN NUMBER,
    p_batch_step_id IN NUMBER,
    p_batch_step_status IN NUMBER)
    RETURN BOOLEAN;

-- ============================================================================
--  Name        : batchsteps_qty_conversion
--  Description : This function is used to convert from step quantity UOM to to
--                step quantity UOM if bothe are not same UOM's.
--  Parameters:
--        IN    :
--                p_quantity_out IN NUMBER
--                p_quantity_out_uom IN VARCHAR2
--                p_quantity_in_uom IN VARCHAR2
--                p_batch_id IN NUMBER
--
--  ============================================================================
    FUNCTION batchsteps_qty_conversion(
    p_quantity_out IN NUMBER,
    p_quantity_out_uom IN VARCHAR2,
    p_quantity_in_uom IN VARCHAR2,
    p_batch_id IN NUMBER)
    RETURN NUMBER;
    

        
END GME_YIELD_CALCULATION_PVT;




/
-- SHOW ERRORS PACKAGE GME_YIELD_CALCULATION_PVT;

COMMIT;
EXIT;
