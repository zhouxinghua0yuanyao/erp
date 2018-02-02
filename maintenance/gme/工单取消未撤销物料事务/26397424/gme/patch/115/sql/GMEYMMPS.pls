/* +======================================================================+ */
/* |    Copyright (c) 2005, 2017 Oracle and/or its affiliates.            | */
/* |                         All rights reserved.                         | */
/* |                           Version 12.0.0                             | */
/* +======================================================================+ */
REM dbdrv: sql ~PROD ~PATH ~FILE none none none package &phase=pls \
REM dbdrv: checkfile(120.0.12010000.5=120.0.12020000.4):~PROD:~PATH:~FILE
SET VERIFY OFF
WHENEVER SQLERROR EXIT FAILURE ROLLBACK;
WHENEVER OSERROR EXIT FAILURE ROLLBACK;



create or replace package gme_yield_calculation_pub AS
/* $Header: GMEYMMPS.pls 120.0.12020000.4 2017/03/07 08:25:37 shalchen noship $ */
/*#
* This file contains the headers for the Yield Measurement APIs in
* Oracle Process Manufacturing (OPM). Each procedure has a common set of
* parameters to which API-specific parameters are appended.
* @rep:scope public
* @rep:product GME
* @rep:displayname Production Management public api's
* @rep:lifecycle active
* @rep:category BUSINESS_ENTITY GME_YMM
*/

  -- Author  : SHALCHEN
  -- Created : 2017/2/23 13:50:03
  -- Purpose : 
  

/*#
* Material In Function
* This Procedure is used to get the quantity input (ingredient)
* in the particular step.
* @param p_batchstep_id NUMBER.
* @rep:scope public
* @rep:lifecycle active
* @rep:displayname material_in
*/  
  
  -- ============================================================================
  --  Name        : material_in
  --  Description : This Procedure is used to get the quantity input (ingredient)
  --                in the particular step.
  --                
  --  Parameters:
  --        IN    :
  --                p_batchstep_id        -batchstep_id
  --  ============================================================================  
  FUNCTION material_in(p_batchstep_id IN gme_batch_steps.batchstep_id%type)
    RETURN NUMBER;


/*#
* Material Out Function
* This Procedure is used to get the material(product, by product)
* quantity coming out from the particular step,by product of
* type waste don't included.
* @param p_batchstep_id NUMBER.
* @rep:scope public
* @rep:lifecycle active
* @rep:displayname material_out
*/     
  -- ============================================================================
  --  Name        : material_out
  --  Description : This Procedure is used to get the material(product, by product)
  --                quantity coming out from the particular step,by product of 
  --                type waste don't included.
  --  Parameters:
  --        IN    :
  --                p_batchstep_id        -batchstep_id
  --  ============================================================================      
  FUNCTION material_out(p_batchstep_id IN gme_batch_steps.batchstep_id%type)
    RETURN NUMBER;


/*#
* Get Intermediate In Function
* This Procedure is used to get intermediate quantity for particular
* step that transfer from previous steps
* @param p_batchstep_id NUMBER.
* @rep:scope public
* @rep:lifecycle active
* @rep:displayname get_inter_in
*/  
  -- ============================================================================
  --  Name        : get_inter_in
  --  Description : This Procedure is used to get intermediate quantity for particular 
  --                step that transfer from previous steps
  --                
  --  Parameters:
  --        IN    :
  --                p_batchstep_id        -batchstep_id
  --  ============================================================================      
  FUNCTION get_inter_in(p_batchstep_id IN gme_batch_steps.batchstep_id%type)
    RETURN NUMBER;   


/*#
* Get Intermediate Out Function
* This Procedure is used to get intermediate quantity that
* come out of processing the input material in particular step
* @param p_batchstep_id NUMBER.
* @rep:scope public
* @rep:lifecycle active
* @rep:displayname get_inter_out
*/  
  -- ============================================================================
  --  Name        : get_inter_out
  --  Description : This Procedure is used to get intermediate quantity that  
  --                come out of processing the input material in particular step
  --                
  --  Parameters:
  --        IN    :
  --                p_batchstep_id        -batchstep_id
  --  ============================================================================      
  FUNCTION get_inter_out(p_batchstep_id IN gme_batch_steps.batchstep_id%type)
    RETURN NUMBER; 


/*#
* Get Scrap Out Function
* This Procedure is used to get scrap quantity for particular step
* @param p_batchstep_id NUMBER.
* @rep:scope public
* @rep:lifecycle active
* @rep:displayname scrap_out
*/ 
  -- ============================================================================
  --  Name        : scrap_out
  --  Description : This Procedure is used to get scrap quantity for particular 
  --                step 
  --                
  --  Parameters:
  --        IN    :
  --                p_batchstep_id        -batchstep_id
  --  ============================================================================      
  FUNCTION scrap_out(p_batchstep_id IN gme_batch_steps.batchstep_id%type)
    RETURN NUMBER;


/*#
* Get Step Total In Quantity Function
* This Procedure is used to get total in quantity for particular
* step that is sum of intermediate in quantity and material in quantity.
* @param p_batchstep_id NUMBER.
* @rep:scope public
* @rep:lifecycle active
* @rep:displayname get_step_total_in_qty
*/     
  -- ============================================================================
  --  Name        : get_step_total_in_qty
  --  Description : This Procedure is used to get total in quantity for particular
  --                step that is sum of intermediate in quantity and material in
  --                quantity.
  --  Parameters:
  --        IN    :
  --                p_batchstep_id        -batchstep_id
  --  ============================================================================  
  FUNCTION get_step_total_in_qty(p_batchstep_id IN gme_batch_steps.batchstep_id%type)
    RETURN NUMBER;


/*#
* Get Step Total Out Quantity Function
* This Procedure is used to get the total output quantity for 
* particular step that is the sum of intermediate out quantity
* and material out quantity.
* @param p_batchstep_id NUMBER.
* @rep:scope public
* @rep:lifecycle active
* @rep:displayname get_step_total_out_qty
*/     
  -- ============================================================================
  --  Name        : get_step_total_out_qty
  --  Description : This Procedure is used to get the total output quantity for 
  --                particular step that is the sum of intermediate out quantity 
  --                and material out quantity. 
  --  Parameters:
  --        IN    :
  --                p_batchstep_id        -batchstep_id
  --  ============================================================================      
  FUNCTION get_step_total_out_qty(p_batchstep_id IN gme_batch_steps.batchstep_id%type)
    RETURN NUMBER ;    
    
    
/*#
* Insert Batch Step Quantity Procedure
* This procedure is used to insert all the batch step transfer details
* into GME_BATCH_STEPS_QUANTITY table and validates all the required
* validations before insertion.
* @param p_batch_id NUMBER.
* @param p_batch_step_quantity_tbl table of batch step quantity record.
* @param p_commit commit flag - default is 'F'.
* @param x_return_status return status.
* @param x_msg_count # of messages.
* @param x_msg_data List of messages.
* @rep:scope public
* @rep:lifecycle active
* @rep:displayname insert_batch_step_quantity
*/     
-- ============================================================================
--  Name        : insert_batch_step_quantity
--  Description : This procedure is used to insert all the batch step transfer details
--                into GME_BATCH_STEPS_QUANTITY table and validates all the required
--                validations before insertion.
--  Parameters:
--        IN    :
--                p_batch_id                 IN      NUMBER
--                p_batch_step_quantity_tbl  IN      batch_step_quantity_tbl
--                p_commit                   IN      VARCHAR2 := fnd_api.g_false
--                
--        OUT   :
--               x_return_status    OUT NOCOPY VARCHAR2
--               Used to verify whether procedure successfully completed or any 
--               errors ,warnings raised in the procedure and returns the exact status.
--               
--                x_msg_count        OUT NOCOPY NUMBER,
--                x_msg_data         OUT NOCOPY VARCHAR2);
--  ============================================================================


    PROCEDURE insert_batch_step_quantity(
      p_batch_id                IN  NUMBER,
      p_batch_step_quantity_tbl IN  gme_api_pub.batch_step_quantity_tbl,
      p_commit                  IN  VARCHAR2 := fnd_api.g_false,
      x_return_status           OUT NOCOPY VARCHAR2,
      x_msg_count               OUT NOCOPY NUMBER,
      x_msg_data                OUT NOCOPY VARCHAR2);
      
      
      
/*#
* Update Batch Step Quantity Procedure
* This procedure is used to update all the batch step transfer details
* of GME_BATCH_STEPS_QUANTITY table and validates all the required
* validations before updation.
* @param p_batch_id NUMBER.
* @param p_batch_step_quantity_tbl table of batch step quantity record.
* @param p_commit commit flag - default is 'F'.
* @param x_return_status return status.
* @param x_msg_count # of messages.
* @param x_msg_data List of messages.
* @rep:scope public
* @rep:lifecycle active
* @rep:displayname update_batch_step_quantity
*/      
-- ============================================================================
--  Name        : update_batch_step_quantity
--  Description : This procedure is used to update all the batch step transfer details
--                of GME_BATCH_STEPS_QUANTITY table and validates all the required
--                validations before updation.
--  Parameters:
--        IN    :
--                p_batch_id                 IN      NUMBER
--                p_batch_step_quantity_tbl  IN      batch_step_quantity_tbl
--                p_commit                   IN      VARCHAR2 := fnd_api.g_false
--                
--        OUT   :
--               x_return_status    OUT NOCOPY VARCHAR2
--               Used to verify whether procedure successfully completed or any 
--               errors ,warnings raised in the procedure and returns the exact status.
--               
--                x_msg_count        OUT NOCOPY NUMBER,
--                x_msg_data         OUT NOCOPY VARCHAR2);
--  ============================================================================
      
    PROCEDURE update_batch_step_quantity(
      p_batch_id                IN  NUMBER,
      p_batch_step_quantity_tbl IN  gme_api_pub.batch_step_quantity_tbl,
      p_commit                  IN  VARCHAR2 := fnd_api.g_false,
      x_return_status           OUT NOCOPY VARCHAR2,
      x_msg_count               OUT NOCOPY NUMBER,
      x_msg_data                OUT NOCOPY VARCHAR2);
      
      
      
/*#
* Delete Batch Step Quantity Procedure
* This procedure is used to delete all the input batch step transfer details
* of GME_BATCH_STEPS_QUANTITY table and validates all the required
* validations before deletion.
* @param p_batch_id NUMBER.
* @param p_batch_step_quantity_tbl table of batch step quantity record.
* @param p_commit commit flag - default is 'F'.
* @param x_return_status return status.
* @param x_msg_count # of messages.
* @param x_msg_data List of messages.
* @rep:scope public
* @rep:lifecycle active
* @rep:displayname delete_batch_step_quantity
*/        
-- ============================================================================
--  Name        : delete_batch_step_quantity
--  Description : This procedure is used to delete all the input batch step transfer details
--                of GME_BATCH_STEPS_QUANTITY table and validates all the required
--                validations before deletion.
--  Parameters:
--        IN    :
--                p_batch_id                 IN      NUMBER
--                p_batch_step_quantity_tbl  IN      batch_step_quantity_tbl
--                p_commit                   IN      VARCHAR2 := fnd_api.g_false
--                
--        OUT   :
--               x_return_status    OUT NOCOPY VARCHAR2
--               Used to verify whether procedure successfully completed or any 
--               errors ,warnings raised in the procedure and returns the exact status.
--               
--                x_msg_count        OUT NOCOPY NUMBER,
--                x_msg_data         OUT NOCOPY VARCHAR2);
--  ============================================================================
      
    PROCEDURE delete_batch_step_quantity(
      p_batch_id                IN  NUMBER,
      p_batch_step_quantity_tbl IN  gme_api_pub.batch_step_quantity_tbl,
      p_commit                  IN  VARCHAR2 := fnd_api.g_false,
      x_return_status           OUT NOCOPY VARCHAR2,
      x_msg_count               OUT NOCOPY NUMBER,
      x_msg_data                OUT NOCOPY VARCHAR2);
      
      
      
/*#
* Associate Batchstep Item Procedure
* This Procedure is used to associate a batch step to an item.
* @param p_api_version Version of the api.
* @param p_commit commit flag - default is 'F'.
* @param p_org_code Inventory Organization.
* @param p_batch_no batch no.
* @param p_batchstep_no batch step no.
* @param p_line_type material details line type.
* @param p_line_no line no of material detail.
* @param x_message_count # of messages.
* @param x_message_list List of messages.
* @param x_return_status return status.
* @rep:scope public
* @rep:lifecycle active
* @rep:displayname associate_batchstep_item
*/ 
-- ============================================================================
--  Name        : ASSOCIATE_BATCHSTEP_ITEM
--  Description : This Procedure is used to associate a batch step to an item.
--  Parameters:
--        IN    :
--                p_org_code     - Organization Code
--                p_batch_no     - Batch Number
--                p_batchstep_no - Batch Step Number
--                p_line_type	 - Formula line type
--                p_line_no	 	 - Formula line number
--
--  ============================================================================ 

   PROCEDURE ASSOCIATE_BATCHSTEP_ITEM (
      p_api_version              IN              NUMBER
     ,p_commit                   IN              VARCHAR2 := fnd_api.g_false
     ,p_org_code                 IN              VARCHAR2 := NULL
	 ,p_batch_no                   IN              VARCHAR2 := NULL
	 ,p_batchstep_no               IN              NUMBER := NULL
	 ,p_line_type             	   IN              NUMBER := NULL
	 ,p_line_no              	     IN              NUMBER
	 ,x_message_count              OUT NOCOPY      NUMBER
     ,x_message_list             OUT NOCOPY      VARCHAR2
     ,x_return_status            OUT NOCOPY      VARCHAR2
     );


/*#
* Update Batch Heat Procedure
* This Procedure is used to update heat number for the batch
* which already exist in the gme_batch_genealogy table,and push
* down to update child batches of the specified batch.
* @param p_batch_id NUMBER.
* @param p_batch_no batch no.
* @param p_organization_id inventory organization id .
* @param p_heat_number heat number.
* @param p_commit commit flag - default is 'F'.
* @param x_return_status return status.
* @param x_message_count # of messages.
* @param x_message_list List of messages.
* @rep:scope public
* @rep:lifecycle active
* @rep:displayname update_batch_heat
*/ 
-- ============================================================================
--  Name        : update_batch_heat
--  Description : This Procedure is used to update heat number for the batch 
--                which already exist in the gme_batch_genealogy table,and push
--                down to update child batches of the specified batch.
--  Parameters:
--        IN    :
--                p_batch_id        - batch_id, pls pass batch_id or batch_no and organization_id
--                p_batch_no        - Batch Number,, pls pass batch_id or batch_no and organization_id
--                p_organization_id - organization_id
--                p_heat_number	    - new heat number
--
--  ============================================================================      
  PROCEDURE update_batch_heat(
    p_batch_id             IN   NUMBER,
    p_batch_no             IN   VARCHAR2,
    p_organization_id      IN   NUMBER,
    p_heat_number          IN   VARCHAR2,
    p_commit               IN   VARCHAR2 := fnd_api.g_false,
    x_return_status        OUT  NOCOPY VARCHAR2,
    x_message_count        OUT  NOCOPY NUMBER,
    x_message_list         OUT  NOCOPY VARCHAR2
    );                    

end gme_yield_calculation_pub;
/

COMMIT ;
EXIT;
