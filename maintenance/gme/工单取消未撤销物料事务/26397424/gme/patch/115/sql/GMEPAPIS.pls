/* +======================================================================+ */
/* |    Copyright (c) 2005, 2017 Oracle and/or its affiliates.           | */
/* |                         All rights reserved.                         | */
/* |                           Version 12.0.0                             | */
/* +======================================================================+ */
REM dbdrv: sql ~PROD ~PATH ~FILE none none none package &phase=pls \
REM dbdrv: checkfile(120.32.12010000.23=120.39.12020000.15)(115.84=120.7):~PROD:~PATH:~FILE
SET VERIFY OFF
WHENEVER SQLERROR EXIT FAILURE ROLLBACK;
WHENEVER OSERROR EXIT FAILURE ROLLBACK;
/*
REM **********************************************************************
REM *                                                                    *
REM * FILE:    GMEPAPIS.pls                                              *
REM * PURPOSE: Package Specification for the GME PUBLIC API routines     *
REM * AUTHOR:  Paul J Schofield, OPM Development                         *
REM * DATE:    February 1st 2001                                         *
REM * HISTORY:                                                           *
REM * ========                                                           *
REM *  11-JAN-2007 Swapna K Bug#6738476 Added parameter,p_batch_header_rec*
REM *   to the procedure,create_phantom                                  *
REM *  13-FEB-2008 Swana K Bug#6778968 Added comments for the api,       *
REM *   convert_dtl_reservation regarding the usage of p_qty_convert    
REM *  18-NOV-2008   G. Muratore   Bug 7565054 
REM *     Added parameter p_sum_all_prod_lines to the procedure create_batch
 
REM * G. Muratore    15-JUN-2009 Bug 7447757      
REM *   Introduced new api for update actual_step_qty
REM *   PROCEDURE update_batchstep_qty. 

REM * G. Muratore    13-MAY-2010 Bug 9482460      
REM *   Introduced new p_parent_lot parameter for creating pending product lots.
REM *   PROCEDURE create_pending_product_lot. 

REM * G. Muratore    17-MAY-2011 Bug 12418545      
REM *   Introduced new p_bypass_gmf_check parameter. 
REM *   PROCEDURE cancel_batch. 

REM * A. Mundhe      19-SEP-2011 Bug 12967042 - Commented fix for bug 12418545.

REM * G. Muratore    13-Apr-2012 Bug 13532998      
REM *   Introduce trans_date parameter for convert_dtl_reservation api.
REM *   PROCEDURE convert_dtl_reservation.

REM * G. Muratore    06-DEC-2012 Bug 14685438      
REM *   Introduced new p_save_batch parameter.
REM *   Also, introduced new api allowing user to reset_txn_header_id.
REM *   PROCEDURE create_material_txn. 

REM * G. Muratore    01-FEB-2013 Bug 16079623.      
REM *   Introduced new p_save_batch parameter.
REM *   PROCEDURE complete_step and release_step                                                                   

REM * G. Muratore    03-FEB-2015 Bug 20448067
REM *   Introduced new p_save_batch parameter.
REM *   PROCEDURE unrelease_batch

REM * G. Muratore    05-APR-2016 Bug 22317010 / 22764488
REM *   Introduced new p_create_resv_pend_lots and p_tolerance parameter.
REM *   PROCEDURE incremental_backflush

REM * G. Muratore    03-NOV-2016 Bug 23640627 
REM *   Introduced new p_create_resv_pend_lots parameter.
REM *   PROCEDURE: revert_batch and revert_step.
REM **********************************************************************
*/
CREATE OR REPLACE PACKAGE gme_api_pub AS
/* $Header: GMEPAPIS.pls 120.39.12020000.15 2017/03/01 02:50:57 shalchen ship $ */
/*#
* This file contains the headers for the Process Execution (GME) APIs in
* Oracle Process Manufacturing (OPM). Each procedure has a common set of
* parameters to which API-specific parameters are appended.
* @rep:scope public
* @rep:product GME
* @rep:displayname Production Management public api's
* @rep:lifecycle active
* @rep:category BUSINESS_ENTITY GME_BATCH
*/


   
  Type batch_step_quantity_rec IS RECORD(
    			from_step_id NUMBER,
    			to_step_id NUMBER,
    			step_qty_in_from_uom NUMBER,
          step_qty_in_to_uom NUMBER,
    			transaction_date DATE
          
   );
    
  Type batch_step_quantity_tbl IS TABLE OF  batch_step_quantity_rec
   INDEX BY BINARY_INTEGER;

/*#
* Save Batch Procedure 
* This procedure allows the user to save a batch.
* It is used to consolidate all the transactions from the temporary
* tables and write them to the main tables.
* @param p_header_id NUMBER Optional.
* @param p_table NUMBER Optional To indicate which table to be processed by Transaction Manager.
* @param p_clear_qty_cache BOOLEAN default to true to clear qty cache
* @param x_return_status return status
* @param p_commit commit flag - default is 'F'
* @rep:scope public
* @rep:lifecycle active
* @rep:displayname save_batch    
*/
   /*================================================================================
     Procedure
       save_batch
     Description
       This procedure is used to consolidate all the transactions from the temporary
       tables and write them to the main tables.
     Parameters
       p_header_id (O)           The header_id used by the inventory transaction manager.
       p_table     (O)           Table to process by Transaction Manager
                                 1 - temp table
                                 2 - interface table
       x_return_status           outcome of the API call
                                 S - Success
                                 E - Error
                                 U - Unexpected Error
       p_commit                  Indicates whether to commit.  
                                 'T' (FND_API.G_TRUE) - to commit work
                                 'F' (FND_API.G_FALSE) - Not to commit work
                                 This is defaulted 'F'
   ================================================================================*/
  /* Bug 5255959 added p_clear_qty_cache parameter */   
   PROCEDURE save_batch (
      p_header_id       IN              NUMBER DEFAULT NULL
     ,p_table           IN              NUMBER DEFAULT NULL
     ,p_commit          IN              VARCHAR2 := fnd_api.g_false
     ,x_return_status   OUT NOCOPY      VARCHAR2
     --Bug#5584699 Changed the datatype from boolean to varchar2.
     ,p_clear_qty_cache    IN              VARCHAR2 := fnd_api.g_true);
     --,p_clear_qty_cache    IN              BOOLEAN DEFAULT TRUE);

/*#
* Create Batch Procedure
* This procedure creates batch, then check for Items failing allocation and
* inventory shortages.
* @param p_api_version Version of the api
* @param p_validation_level validation level
* @param p_init_msg_list message list flag - default is 'F'
* @param p_commit commit flag - default is 'F'
* @param x_message_count # of messages
* @param x_message_list List of messages
* @param x_return_status return status
* @param p_org_code Inventory Organization (Plant or Lab)
* @param p_batch_header_rec gme_batch_header rowtype
* @param x_batch_header_rec gme_batch_header rowtype
* @param p_batch_size batch size
* @param p_batch_size_uom batch size uom
* @param p_creation_mode creation mode
* @param p_recipe_id recipe id
* @param p_recipe_no recipe no
* @param p_recipe_version recipe version
* @param p_product_no product no
* @param p_item_revision item revision of the product, if revision control
* @param p_product_id product id
* @param p_ignore_qty_below_cap ignore below capacity qty
* @param p_use_workday_cal use shop calendar flag
* @param p_contiguity_override contiguity override flag
* @param p_use_least_cost_validity_rule use least cost validity rule to create batch
* @param x_exception_material_tbl table of exception records
* @rep:scope public
* @rep:lifecycle active
* @rep:displayname create_batch
*/

   /*================================================================================
     Procedure
       create_batch
     Description
       This procedure creates batch, then check for inventory shortages.
     Parameters
       p_org_code (O)            Organization code of the plant or lab
       p_batch_header_rec (R)    The batch header row to identify the batch
                                 Following columns are used from this row.
                                 organization_id  (R)
                                 recipe_validity_rule_id (R)
                                 batch_type (R)
                                 update_inventory_ind (R)
                                 batch_no (R In case of manual document ordering)
                                 plan_start_date (O)
                                 plan_cmplt_date (O)
                                 due_date (O)
                                 wip_whse_code (O)
       p_batch_size (R)          Batch Size (Total input, total output or product quantity)
                                 Batch size is NULL for recipe mode of creation.
       p_batch_size_uom (R)      UOM for p_batch_size
       p_creation_mode (R)       How the batch is created
                                 RECIPE
                                 PRODUCT
                                 TOTAL_OUTPUT
                                 TOTAL_INPUT
       p_recipe_id (O)           Recipe_id for which the batch is to be created.
       p_recipe_no (O)           Recipe number along with recipe_version for which the
                                 batch is to be created.
       p_recipe_version (O)      Version of the recipe for  which the batch is t be created.
       p_product_no (O)          Item number for which the batch is to be created.
       p_item_revision (O)       Item revision of the product if it is revision controlled
       p_product_id (O)          Item ID for which the batch is to be created.
       p_sum_all_prod_lines      "A" sum all prod lines, "S" Primary prod only.
                                 Used when creating by 'PRODUCT' (Introduced for APS).
       p_ignore_qty_below_cap (O)Whether the batch is to be created or not, when resource
                                 quantity goes below minimum capacity of the resource.
                                 'T'  (Default)
                                 'F'
       p_use_workday_cal         VARCHAR2('F','T') - to use workday calendar or not.
       p_contiguity_override     VARCHAR2('F','T') - for contigious period of calendar.
       p_use_least_cost_validity_rule    VARCHAR2('F','T') - for least cost batch validity rule.
       x_batch_header_rec        The batch header that is returned, 
       x_return_status           outcome of the API call
                                 S - Success
                                 E - Error
                                 U - Unexpected Error
                                 V - Inventory shortage exists
       x_exception_material_tbl  Table of exception records
   ================================================================================*/
   PROCEDURE create_batch (
      p_api_version              IN              NUMBER 
     ,p_validation_level         IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list            IN              VARCHAR2 := fnd_api.g_false
     ,p_commit                   IN              VARCHAR2 := fnd_api.g_false
     ,x_message_count            OUT NOCOPY      NUMBER
     ,x_message_list             OUT NOCOPY      VARCHAR2
     ,x_return_status            OUT NOCOPY      VARCHAR2
     ,p_org_code                 IN              VARCHAR2 := NULL
     ,p_batch_header_rec         IN              gme_batch_header%ROWTYPE
     ,x_batch_header_rec         OUT NOCOPY      gme_batch_header%ROWTYPE
     ,p_batch_size               IN              NUMBER := NULL
     ,p_batch_size_uom           IN              VARCHAR2 := NULL
     ,p_creation_mode            IN              VARCHAR2
     ,p_recipe_id                IN              NUMBER := NULL
     ,p_recipe_no                IN              VARCHAR2 := NULL
     ,p_recipe_version           IN              NUMBER := NULL
     ,p_product_no               IN              VARCHAR2 := NULL
     ,p_item_revision            IN              VARCHAR2 := NULL
     ,p_product_id               IN              NUMBER := NULL
     ,p_sum_all_prod_lines       IN              VARCHAR2 := 'A'
     ,p_ignore_qty_below_cap     IN              VARCHAR2 := fnd_api.g_true
     ,p_use_workday_cal          IN              VARCHAR2 := fnd_api.g_true
     ,p_contiguity_override      IN              VARCHAR2 := fnd_api.g_true
     ,p_use_least_cost_validity_rule     IN              VARCHAR2 := fnd_api.g_false     
     ,x_exception_material_tbl   OUT NOCOPY      gme_common_pvt.exceptions_tab);

/*#
* Create Phantom Procedure
* This procedure creates batch, then check for Items failing allocation and
* inventory shortages.
* @param p_api_version Version of the api
* @param p_validation_level validation level
* @param p_init_msg_list message list flag - default is 'F'
* @param p_commit commit flag - default is 'F'
* @param x_message_count # of messages
* @param x_message_list List of messages
* @param x_return_status return status
* @param p_material_detail_rec gme_material_details rowtype
* @param p_org_code Inventory Organization (Plant or Lab)
* @param p_batch_no batch no
* @param x_material_detail_rec gme_material_details rowtype
* @param p_batch_header_rec gme_batch_header row to pass Flexfields
* @param p_validity_rule_id validity rule id
* @param p_use_workday_cal use shop calendar flag
* @param p_contiguity_override contiguity override flag
* @param p_use_least_cost_validity_rule use least cost validity rule
* @param x_exception_material_tbl table of exception records
* @rep:scope public
* @rep:lifecycle active
* @rep:displayname create_phantom
*/
   /*================================================================================
    Procedure
      create_phantom
    Description
      This procedure creates phantom batch based on the validity rule passsed

    Parameters
      p_material_details (R)    The material detail row to identify the material
                                Following columns are used from this row.
                                material_detail_id  (R)
      p_org_code (O)            Organization code of the plant or lab
      p_batch_no         (O)    Batch no, in case of manual document ordering
      p_validity_rule_id (R)    validity rule to use for creating phantom batch
      p_ignore_shortages (R)    Do not check for the inventory shortages
      p_use_workday_cal         VARCHAR2('F','T') - to use workday calendar or not.
      p_contiguity_override     VARCHAR2('F','T') - for contigious period of calendar.
      p_use_least_cost_validity_rule    VARCHAR2('F','T') - for least cost batch validity rule.
      x_material_details_rec    The material detail that is returned, with all the data
                                inventory shortage exists
      x_return_status           outcome of the API call
                                S - Success
                                E - Error
                                U - Unexpected Error
      x_exception_material_tbl  Table of exception records
  ================================================================================*/
   PROCEDURE create_phantom (
      p_api_version              IN              NUMBER 
     ,p_validation_level         IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list            IN              VARCHAR2 := fnd_api.g_false
     ,p_commit                   IN              VARCHAR2 := fnd_api.g_false
     ,x_message_count            OUT NOCOPY      NUMBER
     ,x_message_list             OUT NOCOPY      VARCHAR2
     ,x_return_status            OUT NOCOPY      VARCHAR2
     ,p_material_detail_rec      IN              gme_material_details%ROWTYPE
     ,p_batch_header_rec         IN              gme_batch_header%ROWTYPE --Bug#6738476
     ,p_org_code                 IN              VARCHAR2
     ,p_batch_no                 IN              VARCHAR2 DEFAULT NULL
     ,x_material_detail_rec      OUT NOCOPY      gme_material_details%ROWTYPE
     ,p_validity_rule_id         IN              NUMBER
     ,p_use_workday_cal          IN              VARCHAR2 := fnd_api.g_true
     ,p_contiguity_override      IN              VARCHAR2 := fnd_api.g_true
     ,p_use_least_cost_validity_rule     IN      VARCHAR2 := fnd_api.g_false          
     ,x_exception_material_tbl   OUT NOCOPY      gme_common_pvt.exceptions_tab);

/*#
* Scale Batch Procedure
* This procedure reschedules a batch and all associated phantom batches.
* It also reschedules all the steps, if requested so.
* @param p_api_version Version of the api
* @param p_validation_level validation level
* @param p_init_msg_list message list flag - default is 'F'
* @param p_commit commit flag - default is 'F'
* @param x_message_count # of messages
* @param x_message_list List of messages
* @param x_return_status return status
* @param p_batch_header_rec gme_batch_header rowtype
* @param p_org_code Inventory Organization (Plant or Lab)
* @param p_ignore_exception Ignore exceptions raised during Scale batch
* @param p_use_workday_cal use shop calendar flag
* @param p_contiguity_override contiguity override
* @param p_recalc_dates recalculate dates vased on new resource usage
* @param p_scale_factor factor by which quantities should be scaled
* @param p_qty_type 1 - batch quantities, 0 - formula quantities
* @param p_primaries INPUT scale based in Ingredients, OUTPUT scale based on Products
* @param x_batch_header_rec gme_batch_header rowtype
* @param x_exception_material_tbl table of exception records
* @rep:scope public
* @rep:lifecycle active
* @rep:displayname reschedule_batch
*/
   /*================================================================================
     Procedure
       scale_batch
     Description
       This procedure reschedules batch and all the phantom batches.
        It also reschedules all the steps, if requested so.

     Parameters
       p_batch_header_rec (R)    The batch header row to identify the batch
                                 Following columns are used from this row.
                                 batch_id  (R)
                                 plan_start_date (R)
                                 plan_cmplt_date (R)
       p_org_code (O)            Organization code of the plant or lab
       p_use_workday_cal         VARCHAR2('F','T') - to use workday calendar or not.
       p_contiguity_override     VARCHAR2('F','T') - for contigious period of calendar.
       p_recalc_dates            VARCHAR2('F','T') - recalculate dates or not.
       p_scale_factor (O)        scale factor for scaling
       p_primaries (R)           (INPUT, OUTPUT) scale based on Ingredients or Products
       p_qty_type (O)            (0,1) Use formula quantities or batch quantities
       x_batch_header_rec        The batch header that is returned, with all the data
       x_return_status           outcome of the API call
                                 S - Success
                                 E - Error
                                 U - Unexpected Error
      x_exception_material_tbl  Table of exception records
   ================================================================================*/
   PROCEDURE scale_batch (
      p_api_version              IN              NUMBER 
     ,p_validation_level         IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list            IN              VARCHAR2 := fnd_api.g_false
     ,p_commit                   IN              VARCHAR2 := fnd_api.g_false
     ,p_batch_header_rec         IN              gme_batch_header%ROWTYPE
     ,p_org_code                 IN              VARCHAR2
     ,p_ignore_exception         IN              VARCHAR2 := fnd_api.g_false
     ,p_scale_factor             IN              NUMBER
     ,p_primaries                IN              VARCHAR2
     ,p_qty_type                 IN              NUMBER := 1
     ,p_recalc_dates             IN              VARCHAR2 := fnd_api.g_false
     ,p_use_workday_cal          IN              VARCHAR2 := fnd_api.g_false
     ,p_contiguity_override      IN              VARCHAR2 := fnd_api.g_false
     ,x_exception_material_tbl   OUT NOCOPY      gme_common_pvt.exceptions_tab
     ,x_message_count            OUT NOCOPY      NUMBER
     ,x_message_list             OUT NOCOPY      VARCHAR2
     ,x_return_status            OUT NOCOPY      VARCHAR2
     ,x_batch_header_rec         OUT NOCOPY      gme_batch_header%ROWTYPE);

/*#
* Theoretical Yield Batch Procedure
* This procedure calculates theoretical yield for the batch, and updates the
* quantities for the product lines.
* @param p_api_version Version of the api
* @param p_validation_level validation level
* @param p_init_msg_list message list flag - default is False
* @param x_message_count # of messages
* @param x_message_list List of messages
* @param p_commit commit flag - default is False
* @param x_return_status return status
* @param p_org_code Inventory Organization (Plant or Lab)
* @param p_batch_header_rec gme_batch_header rowtype
* @param p_scale_factor scale factor for theoretical yield
* @rep:scope public
* @rep:lifecycle active
* @rep:displayname theoretical_yield_batch
*/

   /*================================================================================
     Procedure
       theoretical_yield_batch
     Description
       This procedure calculates theoretical yield for the batch, and updates the
        quantities for the product lines.

     Parameters
       p_batch_header (R)        The batch header row to identify the batch
                                 Following columns are used from this row.
                                 batch_id  (R)
       p_org_code (O)            Organization code of the plant or lab
       p_scale_factor (R)        Theoretical yield in fractions

       x_batch_header            The batch header that is returned, with all the data
       x_return_status           outcome of the API call
                                 S - Success
                                 E - Error
                                 U - Unexpected Error
   ================================================================================*/
   PROCEDURE theoretical_yield_batch (
      p_api_version        IN              NUMBER := 2.0
     ,p_validation_level   IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list      IN              VARCHAR2 := fnd_api.g_false
     ,p_commit             IN              VARCHAR2 := fnd_api.g_false
     ,p_batch_header_rec   IN              gme_batch_header%ROWTYPE
     ,p_org_code           IN              VARCHAR2
     ,p_scale_factor       IN              NUMBER
     ,x_message_count      OUT NOCOPY      NUMBER
     ,x_message_list       OUT NOCOPY      VARCHAR2
     ,x_return_status      OUT NOCOPY      VARCHAR2);
/*#
* Update Actual Resource Usage Procedure 
* This procedure is used to insert a completed resource txn for a
* resource.This procedure removes all the existing resource txn for the specified
* resource.This API can be used only bor a WIP or Complete step.
* @param p_api_version Version of the api
* @param p_validation_level validation level
* @param p_init_msg_list message list flag - default is False
* @param p_commit commit flag - default is False
* @param p_org_code Inventory Organization (Plant or Lab)
* @param p_batch_no Batch No
* @param p_batchstep_no Batch Step No
* @param p_activity Activity
* @param p_resource Resource
* @param p_rsrc_txn_rec gme_rsrc_txn_rec rowtype
* @param p_instance_no Instance No
* @param p_reason_name Reason Name from the mtl_transaction_reasons
* @param p_validate_flexfields validate flexfields flag ('T','F')
* @param x_rsrc_txn_rec gme_rsrc_txn_rec rowtype
* @param x_message_count # of messages
* @param x_message_list List of messages
* @param x_return_status return status
* @rep:scope public
* @rep:lifecycle active
* @rep:displayname update_actual_rsrc_usage      
*/

   /*================================================================================
    Procedure
      update_actual_rsrc_usage
    Description
      This procedure is used to insert a completed resource txn for a
      resource.This procedure removes all the existing resource txn for the specified
      resource.This API can be used only bor a WIP or Complete step.

    Parameters
      p_org_code (O)              Organization code of the plant or lab
      p_batch_no (R)              Batch no.
      p_batchstep_no (R)          Batch Step no .
      p_activity (R)              Activity.
      p_resource (R)              Resource.
      p_instance_no (O)           instance no - for WPS use 
      p_reason_name (O)           Reasone Name (reason from mtl_transaction_reasons)
      p_rsrc_txn_rec (R)          gme_resource_txns rowtype 
      p_validate_flexfields (O)   validate flexfield flag ('T','F')
      x_rsrc_txn_rec (R)          gme_resource_txns rowtype 
      x_return_status             outcome of the API call
                                  S - Success
                                  E - Error
                                  U - Unexpected Error
  ================================================================================*/

   PROCEDURE update_actual_rsrc_usage (
      p_api_version           IN              NUMBER := 2.0
     ,p_validation_level      IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list         IN              VARCHAR2 := fnd_api.g_false
     ,p_commit                IN              VARCHAR2 := fnd_api.g_false
     ,p_org_code              IN              VARCHAR2
     ,         /* inventory organization under which the batch was created */
      p_batch_no              IN              VARCHAR2 := NULL
     ,p_batchstep_no          IN              NUMBER := NULL
     ,p_activity              IN              VARCHAR2 := NULL
     ,p_resource              IN              VARCHAR2 := NULL
     ,p_instance_no           IN              NUMBER := NULL
     ,p_reason_name           IN              VARCHAR2 := NULL
     ,p_rsrc_txn_rec          IN              gme_resource_txns%ROWTYPE
     ,p_validate_flexfields   IN              VARCHAR2 := fnd_api.g_false
     ,                     /* indicates whether to validate the flex fields*/
      x_rsrc_txn_rec          IN OUT NOCOPY   gme_resource_txns%ROWTYPE
     ,x_message_count         OUT NOCOPY      NUMBER
     ,x_message_list          OUT NOCOPY      VARCHAR2
     ,x_return_status         OUT NOCOPY      VARCHAR2);

/*#
* Insert Incremental Resource Transaction Procedure 
* This procedure is used to incrementally insert a complete resource txn allocation for a
* resource.This API can be used only for a WIP or Complete step.
* @param p_api_version Version of the api
* @param p_validation_level validation level
* @param p_init_msg_list message list flag - default is False
* @param p_commit commit flag - default is False
* @param p_org_code Inventory Organization (Plant or Lab)
* @param p_batch_no Batch No
* @param p_batchstep_no Batch Step No
* @param p_activity Activity
* @param p_resource Resource
* @param p_rsrc_txn_rec gme_rsrc_txn_rec rowtype
* @param p_instance_no Instance No
* @param p_reason_name Reason Name from the mtl_transaction_reasons
* @param p_validate_flexfields validate flexfields flag ('T','F')
* @param x_rsrc_txn_rec gme_rsrc_txn_rec rowtype
* @param x_message_count # of messages
* @param x_message_list List of messages
* @param x_return_status return status
* @rep:scope public
* @rep:lifecycle active
* @rep:displayname insert_incr_actual_rsrc_txn
*/

   /*================================================================================
    Procedure
      insert_incr_actual_rsrc_txn
    Description
      This procedure is used to incrementally insert a complete resource txn allocation for a
      resource.This API can be used only for a WIP or Complete step.

    Parameters
      p_org_code (O)              Organization code of the plant or lab
      p_batch_no (R)              Batch no.
      p_batchstep_no (R)          Batch Step no .
      p_activity (R)              Activity.
      p_resource (R)              Resource.
      p_instance_no (O)           instance no - for WPS use 
      p_reason_name (O)           Reasone Name (reason from mtl_transaction_reasons)
      p_rsrc_txn_rec (R)          gme_resource_txns rowtype 
      p_validate_flexfields (O)   validate flexfield flag ('T','F')
      x_rsrc_txn_rec (R)          gme_resource_txns rowtype 
      x_return_status             outcome of the API call
                                  S - Success
                                  E - Error
                                  U - Unexpected Error
  ================================================================================*/
   PROCEDURE insert_incr_actual_rsrc_txn (
      p_api_version           IN              NUMBER := 2.0
     ,p_validation_level      IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list         IN              VARCHAR2 := fnd_api.g_false
     ,p_commit                IN              VARCHAR2 := fnd_api.g_false
     ,p_org_code              IN              VARCHAR2
     ,         /* inventory organization under which the batch was created.*/
      p_batch_no              IN              VARCHAR2 := NULL
     ,p_batchstep_no          IN              NUMBER := NULL
     ,p_activity              IN              VARCHAR2 := NULL
     ,p_resource              IN              VARCHAR2 := NULL
     ,p_instance_no           IN              NUMBER := NULL
     ,p_reason_name           IN              VARCHAR2 := NULL
     ,p_rsrc_txn_rec          IN              gme_resource_txns%ROWTYPE
     ,p_validate_flexfields   IN              VARCHAR2 := fnd_api.g_false
     ,                    /* indicates whether to validate the flex fields */
      x_rsrc_txn_rec          IN OUT NOCOPY   gme_resource_txns%ROWTYPE
     ,x_message_count         OUT NOCOPY      NUMBER
     ,x_message_list          OUT NOCOPY      VARCHAR2
     ,x_return_status         OUT NOCOPY      VARCHAR2);

/*#
* Insert Timed Actual Resource Transaction Procedure 
* This procedure is used to insert a complete resource txn allocation for a
* resource.usage is calculated from txn dates supplied.
* This API can be used only bor a WIP or Complete step.
* @param p_api_version Version of the api
* @param p_validation_level validation level
* @param p_init_msg_list message list flag - default is False
* @param p_commit commit flag - default is False
* @param p_org_code Inventory Organization (Plant or Lab)
* @param p_batch_no Batch No
* @param p_batchstep_no Batch Step No
* @param p_activity Activity
* @param p_resource Resource
* @param p_rsrc_txn_rec gme_rsrc_txn_rec rowtype
* @param p_instance_no Instance No
* @param p_reason_name Reason Name from the mtl_transaction_reasons
* @param p_validate_flexfields validate flexfields flag ('T','F')
* @param x_rsrc_txn_rec gme_rsrc_txn_rec rowtype
* @param x_message_count # of messages
* @param x_message_list List of messages
* @param x_return_status return status
* @rep:scope public
* @rep:lifecycle active
* @rep:displayname insert_timed_actual_rsrc_txn
*/

   /*================================================================================
    Procedure
      insert_timed_actual_rsrc_txn
    Description
      This procedure is used to insert a complete resource txn allocation for a
      resource.usage is calculated from txn dates supplied.
      This API can be used only for a WIP or Complete step.

    Parameters
      p_org_code (O)              Organization code of the plant or lab
      p_batch_no (R)              Batch no.
      p_batchstep_no (R)          Batch Step no .
      p_activity (R)              Activity.
      p_resource (R)              Resource.
      p_instance_no (O)           instance no - for WPS use 
      p_reason_name (O)           Reasone Name (reason from mtl_transaction_reasons)
      p_rsrc_txn_rec (R)          gme_resource_txns rowtype 
      p_validate_flexfields (O)   validate flexfield flag ('T','F')
      x_rsrc_txn_rec (R)          gme_resource_txns rowtype 
      x_return_status             outcome of the API call
                                  S - Success
                                  E - Error
                                  U - Unexpected Error
  ================================================================================*/
   PROCEDURE insert_timed_actual_rsrc_txn (
      p_api_version           IN              NUMBER := 2.0
     ,p_validation_level      IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list         IN              VARCHAR2 := fnd_api.g_false
     ,p_commit                IN              VARCHAR2 := fnd_api.g_false
     ,p_org_code              IN              VARCHAR2
     ,        /* inventory organization under which the batch was created. */
      p_batch_no              IN              VARCHAR2 := NULL
     ,p_batchstep_no          IN              NUMBER := NULL
     ,p_activity              IN              VARCHAR2 := NULL
     ,p_resource              IN              VARCHAR2 := NULL
     ,p_instance_no           IN              NUMBER := NULL
     ,p_reason_name           IN              VARCHAR2 := NULL
     ,p_rsrc_txn_rec          IN              gme_resource_txns%ROWTYPE
     ,p_validate_flexfields   IN              VARCHAR2 := fnd_api.g_false
     ,                    /* indicates whether to validate the flex fields */
      x_rsrc_txn_rec          IN OUT NOCOPY   gme_resource_txns%ROWTYPE
     ,x_message_count         OUT NOCOPY      NUMBER
     ,x_message_list          OUT NOCOPY      VARCHAR2
     ,x_return_status         OUT NOCOPY      VARCHAR2);

/*#
* Start Complete Actual Resource Transaction Procedure 
* This procedure is used to start a complete resource txn for a resource.
* usage of the txn is 0 as the txn has just started.end_cmplt_actual_rsrc_txn
* API is used to complete a started txn
* @param p_api_version Version of the api
* @param p_validation_level validation level
* @param p_init_msg_list message list flag - default is False
* @param p_commit commit flag - default is False
* @param p_org_code Inventory Organization (Plant or Lab)
* @param p_batch_no Batch No
* @param p_batchstep_no Batch Step No
* @param p_activity Activity
* @param p_resource Resource
* @param p_rsrc_txn_rec gme_rsrc_txn_rec rowtype
* @param p_instance_no Instance No
* @param p_reason_name Reason Name from the mtl_transaction_reasons
* @param p_validate_flexfields validate flexfields flag ('T','F')
* @param x_rsrc_txn_rec gme_rsrc_txn_rec rowtype
* @param x_message_count # of messages
* @param x_message_list List of messages
* @param x_return_status return status
* @rep:scope public
* @rep:lifecycle active
* @rep:displayname start_cmplt_actual_rsrc_txn
*/

   /*================================================================================
    Procedure
      start_cmplt_actual_rsrc_txn
    Description
      This procedure is used to start a complete resource txn for a resource.
      usage of the txn is 0 as the txn has just started.end_cmplt_actual_rsrc_txn
      API is used to complete a started txn

    Parameters
      p_org_code (O)              Organization code of the plant or lab
      p_batch_no (R)              Batch no.
      p_batchstep_no (R)          Batch Step no .
      p_activity (R)              Activity.
      p_resource (R)              Resource.
      p_instance_no (O)           instance no - for WPS use 
      p_reason_name (O)           Reasone Name (reason from mtl_transaction_reasons)
      p_rsrc_txn_rec (R)          gme_resource_txns rowtype 
      p_validate_flexfields (O)   validate flexfield flag ('T','F')
      x_rsrc_txn_rec (R)          gme_resource_txns rowtype 
      x_return_status             outcome of the API call
                                  S - Success
                                  E - Error
                                  U - Unexpected Error
  ================================================================================*/
     PROCEDURE start_cmplt_actual_rsrc_txn (
      p_api_version           IN              NUMBER := 2.0
     ,p_validation_level      IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list         IN              VARCHAR2 := fnd_api.g_false
     ,p_commit                IN              VARCHAR2 := fnd_api.g_false
     ,p_org_code              IN              VARCHAR2
     ,          /*inventory organization under which the batch was created.*/
      p_batch_no              IN              VARCHAR2 := NULL
     ,p_batchstep_no          IN              NUMBER := NULL
     ,p_activity              IN              VARCHAR2 := NULL
     ,p_resource              IN              VARCHAR2 := NULL
     ,p_instance_no           IN              NUMBER
     ,p_reason_name           IN              VARCHAR2 := NULL
     , /*instance of the resource for which the transaction is being added.*/
      p_rsrc_txn_rec          IN              gme_resource_txns%ROWTYPE
     ,p_validate_flexfields   IN              VARCHAR2 := fnd_api.g_false
     ,                      /*indicates whether to validate the flex fields*/
      x_rsrc_txn_rec          IN OUT NOCOPY   gme_resource_txns%ROWTYPE
     ,x_message_count         OUT NOCOPY      NUMBER
     ,x_message_list          OUT NOCOPY      VARCHAR2
     ,x_return_status         OUT NOCOPY      VARCHAR2);

/*#
* End Complete Actual Resource Transaction Procedure 
* This procedure is used to start a complete resource txn for a resource.
* usage of the txn is 0 as the txn has just started.end_cmplt_actual_rsrc_txn
* API is used to complete a started txn
* @param p_api_version Version of the api
* @param p_validation_level validation level
* @param p_init_msg_list message list flag - default is False
* @param p_commit commit flag - default is False
* @param p_rsrc_txn_rec gme_rsrc_txn_rec rowtype
* @param p_instance_no Instance No
* @param p_reason_name Reason Name from the mtl_transaction_reasons
* @param p_validate_flexfields validate flexfields flag ('T','F')
* @param x_rsrc_txn_rec gme_rsrc_txn_rec rowtype
* @param x_message_count # of messages
* @param x_message_list List of messages
* @param x_return_status return status
* @rep:scope public
* @rep:lifecycle active
* @rep:displayname end_cmplt_actual_rsrc_txn      
*/

   /*================================================================================
    Procedure
      end_cmplt_actual_rsrc_txn
    Description
      This procedure is used to end a rsrc txn that was started earlier using 
      start_cmplt_actual_rsrc_txn API. The usage of specified txn must be 0.

    Parameters
      p_instance_no (O)           instance no - for WPS use 
      p_reason_name (O)           Reasone Name (reason from mtl_transaction_reasons)
      p_rsrc_txn_rec (R)          gme_resource_txns rowtype 
      p_validate_flexfields (O)   validate flexfield flag ('T','F')
      x_rsrc_txn_rec (R)          gme_resource_txns rowtype 
      x_return_status             outcome of the API call
                                  S - Success
                                  E - Error
                                  U - Unexpected Error
  ================================================================================*/
   PROCEDURE end_cmplt_actual_rsrc_txn (
      p_api_version           IN              NUMBER := 2.0
     ,p_validation_level      IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list         IN              VARCHAR2 := fnd_api.g_false
     ,p_commit                IN              VARCHAR2 := fnd_api.g_false
     ,p_instance_no           IN              NUMBER := NULL
     ,p_reason_name           IN              VARCHAR2 := NULL
     ,p_rsrc_txn_rec          IN              gme_resource_txns%ROWTYPE
     ,      /*contains information for resource_transaction to be completed*/
      p_validate_flexfields   IN              VARCHAR2 := fnd_api.g_false
     ,                    /* indicates whether to validate the flex fields */
      x_rsrc_txn_rec          IN OUT NOCOPY   gme_resource_txns%ROWTYPE
     ,x_message_count         OUT NOCOPY      NUMBER
     ,x_message_list          OUT NOCOPY      VARCHAR2
     ,x_return_status         OUT NOCOPY      VARCHAR2);

/*#
* Reschedule Batch Procedure
* This procedure reschedules a batch and all associated phantom batches.
* It also reschedules all the steps, if requested so.
* @param p_api_version Version of the api
* @param p_validation_level validation level
* @param p_init_msg_list message list flag - default is False
* @param p_commit commit flag - default is False
* @param x_message_count # of messages
* @param x_message_list List of messages
* @param x_return_status return status
* @param p_org_code Inventory Organization (Plant or Lab)
* @param p_batch_header_rec gme_batch_header rowtype
* @param p_use_workday_cal use shop calendar flag
* @param p_contiguity_override contiguity override
* @param x_batch_header_rec gme_batch_header rowtype
* @rep:scope public
* @rep:lifecycle active
* @rep:displayname reschedule_batch
*/

   /*================================================================================
     Procedure
       reschedule_batch
     Description
       This procedure reschedules batch and all the phantom batches.
        It also reschedules all the steps, if requested so.

     Parameters
       p_org_code (O)            Organization code of the plant or lab
       p_batch_header (R)        The batch header row to identify the batch
                                 Following columns are used from this row.
                                 batch_id  (R)
                                 plan_start_date (R)
                                 plan_cmplt_date (R)
       p_use_workday_cal         VARCHAR2('F','T') - to use workday calendar or not.
       p_contiguity_override     VARCHAR2('F','T') - for contigious period of calendar.
       x_batch_header            The batch header that is returned, with all the data
       x_return_status           outcome of the API call
                                 S - Success
                                 E - Error
                                 U - Unexpected Error
   ================================================================================*/
   PROCEDURE reschedule_batch (
      p_api_version           IN              NUMBER := 2.0
     ,p_validation_level      IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list         IN              VARCHAR2 := fnd_api.g_false
     ,p_commit                IN              VARCHAR2 := fnd_api.g_false
     ,p_batch_header_rec      IN              gme_batch_header%ROWTYPE
     ,p_use_workday_cal       IN              VARCHAR2 := fnd_api.g_false
     ,p_contiguity_override   IN              VARCHAR2 := fnd_api.g_false
     ,p_org_code              IN              VARCHAR2
     ,x_message_count         OUT NOCOPY      NUMBER
     ,x_message_list          OUT NOCOPY      VARCHAR2
     ,x_return_status         OUT NOCOPY      VARCHAR2
     ,x_batch_header_rec      OUT NOCOPY      gme_batch_header%ROWTYPE);

/*#
* Reschedule Step Procedure
* This procedure reschedules a step and all the subsequent steps, if requested so.
* @param p_api_version Version of the api
* @param p_validation_level validation level
* @param p_init_msg_list message list flag - default is False
* @param p_commit commit flag - default is False
* @param x_message_count # of messages
* @param x_message_list List of messages
* @param x_return_status return status
* @param p_org_code Inventory Organization (Plant or Lab)
* @param p_batch_no batch number
* @param p_batch_type NUMBER (0,10) Batch, FPO
* @param p_batch_step_rec gme_batch_steps rowtype
* @param p_reschedule_preceding reschedule preceding steps flag - default is 'T'
* @param p_reschedule_succeeding reschedule succeeding steps flag - default is 'T'
* @param p_use_workday_cal use shop calendar flag
* @param p_contiguity_override contiguity override
* @param x_batch_step_rec gme_batch_steps rowtype
* @rep:scope public
* @rep:lifecycle active
* @rep:displayname reschedule_step
*/

   /*================================================================================
     Procedure
       reschedule_step
     Description
       This procedure reschedules step and all the subsequent steps, if requested so.

     Parameters
       p_org_code (O)            Organization code of the plant or lab
       p_batch_no (O)            batch no
       p_batch_type (O)          batch type (0/10) Batch/FPO
       p_batch_step_rec (R)      The batch step row to identify the step
                                 Following columns are used from this row.
                                 batchstep_id  (R)
                                 plan_start_date (R)
                                 plan_cmplt_date (R)
       p_reschedule_preceding (O) Whether to reschedule preceding steps.
       p_reschedule_succeeding (O) Whether to reschedule succeeding steps.
       p_use_workday_cal         VARCHAR2('F','T') - to use workday calendar or not.
       p_contiguity_override     VARCHAR2('F','T') - for contigious period of calendar.
       x_batch_step_rec          The batch step that is returned, with all the data
       x_return_status           outcome of the API call
                                 S - Success
                                 E - Error
                                 U - Unexpected Error
   ================================================================================*/
   PROCEDURE reschedule_step (
      p_api_version             IN              NUMBER := 2.0
     ,p_validation_level        IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list           IN              VARCHAR2 := fnd_api.g_false
     ,p_commit                  IN              VARCHAR2 := fnd_api.g_false
     ,p_batch_step_rec          IN              gme_batch_steps%ROWTYPE
     ,p_use_workday_cal         IN              VARCHAR2 := fnd_api.g_false
     ,p_contiguity_override     IN              VARCHAR2 := fnd_api.g_false
     ,p_org_code                IN              VARCHAR2
     ,p_batch_no                IN              VARCHAR2
     ,p_batch_type              IN              NUMBER
     ,p_reschedule_preceding    IN              VARCHAR2 := fnd_api.g_false
     ,p_reschedule_succeeding   IN              VARCHAR2 := fnd_api.g_true
     ,x_message_count           OUT NOCOPY      NUMBER
     ,x_message_list            OUT NOCOPY      VARCHAR2
     ,x_return_status           OUT NOCOPY      VARCHAR2
     ,x_batch_step_rec          OUT NOCOPY      gme_batch_steps%ROWTYPE);

/*#
* Create Batch Reservations Procedure
* This procedure Creaates high level reservations for all the ingredients 
* of a given batch and all associated phantom batches.
* @param p_api_version Version of the api
* @param p_validation_level validation level
* @param p_init_msg_list message list flag - default is False
* @param p_commit commit flag - default is False
* @param x_message_count # of messages
* @param x_message_list List of messages
* @param x_return_status return status
* @param p_org_code Inventory Organization (Plant or Lab)
* @param p_batch_rec gme_batch_header rowtype
* @rep:scope public
* @rep:lifecycle active
* @rep:displayname create_batch_reservations
*/
    /*================================================================================
     Procedure
       create_batch_reservations
     Description
       This procedure creates high level reservations (at organization level) for ingredient
       lines within a batch.
     Parameters
       p_api_version      (O)    Required API version.
       p_init_msg_list    (O)    Instructs TRUE or FALSE (T/F) on whether the message stack
                                 should be initialized
       p_commit           (O)    Instructs TRUE or FALSE (T/F) on whether a COMMIT should
                                 be issued on completion of successful processing
       p_batch_rec        (R)    The batch header row to identify the batch
                                 against which high level reservations will be created
                                 Supply either:
                                 p_batch_rec.batch_no p_batch_rec.batch_type with p_org_code
                                           OR
                                 p_batch_rec.batch_id
       p_org_code         (O)    Must be supplied if p_batch_rec.organization_id is not populated.
                                 Identifies the organization associated with the batch.
       x_message_count           Number of messages accumulated on the stack during processing
       x_message_list            Messages accumulated during processing
       x_return_status           outcome of the API call
                                 S - Success
                                 E - Error
                                 U - Unexpected Error
                                 N - Items failed auto allocation
                                 V - Inventory shortage exists
   ================================================================================*/
   PROCEDURE create_batch_reservations (
      p_api_version      IN              NUMBER := 2.0
     ,p_validation_level IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list    IN              VARCHAR2 := fnd_api.g_false
     ,p_commit           IN              VARCHAR2 := fnd_api.g_false
     ,p_batch_rec        IN              gme_batch_header%ROWTYPE
     ,p_org_code         IN              VARCHAR2
     ,x_message_count    OUT NOCOPY      NUMBER
     ,x_message_list     OUT NOCOPY      VARCHAR2
     ,x_return_status    OUT NOCOPY      VARCHAR2);

/*#
* Create Line Reservations Procedure
* This procedure Creaates high level reservations for the given ingredient line
* @param p_api_version Version of the api
* @param p_validation_level validation level
* @param p_init_msg_list message list flag - default is False
* @param p_commit commit flag - default is False
* @param x_message_count # of messages
* @param x_message_list List of messages
* @param x_return_status return status
* @param p_org_code Inventory Organization (Plant or Lab)
* @param p_batch_no Batch number
* @param p_line_no Line number of the ingredient
* @param p_material_detail_id Identifier for the ingredient line
* @rep:scope public
* @rep:lifecycle active
* @rep:displayname create_line_reservations
*/
    /*================================================================================
     Procedure
       create_line_reservations
     Description
       This procedure creates high level reservations (at organization level) for the input
       batch ingredient line.
     Parameters
       p_api_version      (O)    Required API version.
       p_init_msg_list    (O)    Instructs TRUE or FALSE (T/F) on whether the message stack
                                 should be initialized
       p_commit           (O)    Instructs TRUE or FALSE (T/F) on whether a COMMIT should
                                 be issued on completion of successful processing
       p_material_detail_id (O)  The  identifier for the ingredient material detail row
                                 against which a high level reservation will be created
                                 If this is not supplied, then org_code, batch_no,line_no must be input
       p_org_code         (O)    Organization with which the batch and ingredient line are associated
       p_batch_no         (O)    Owning batch for the ingredient line.
       p_line_no          (O)    Ingredient line.
       x_message_count           Number of messages accumulated on the stack during processing
       x_message_list            Messages accumulated during processing
       x_return_status           outcome of the API call
                                 S - Success
                                 E - Error
                                 U - Unexpected Error
                                 N - Items failed auto allocation
                                 V - Inventory shortage exists
   ================================================================================*/
   PROCEDURE create_line_reservations (
      p_api_version          IN              NUMBER := 2.0
     ,p_validation_level     IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list        IN              VARCHAR2 := fnd_api.g_false
     ,p_commit               IN              VARCHAR2 := fnd_api.g_false
     ,p_material_detail_id   IN              NUMBER
     ,p_org_code             IN              VARCHAR2
     ,p_batch_no             IN              VARCHAR2
     ,p_line_no              IN              NUMBER
     ,x_message_count        OUT NOCOPY      NUMBER
     ,x_message_list         OUT NOCOPY      VARCHAR2
     ,x_return_status        OUT NOCOPY      VARCHAR2);

/*#
* Insert Process Parameter Procedure
* This particular procedure is used to insert parameter for an resource
* @param p_api_version Version of the api
* @param p_validation_level validation level
* @param p_init_msg_list message list flag - default is False
* @param p_commit commit flag - default is False
* @param x_message_count # of messages
* @param x_message_list List of messages
* @param x_return_status return status
* @param p_batch_no Batch Number
* @param p_org_code Inventory Organization (Plant or Lab)
* @param p_validate_flexfields  Indicates whether to validate the flex fields
* @param p_batchstep_no Batch Step Number
* @param p_activity activity
* @param p_parameter process parameter
* @param p_process_param_rec gme_process_parameters row type
* @param x_process_param_rec gme_process_parameters row type
* @rep:scope public
* @rep:lifecycle active
* @rep:displayname insert_process_parameter
*/
/*===========================================================================================
   Procedure
      insert_process_parameter
   Description
     This particular procedure is used to insert parameter for an resource
   Parameters
     p_process_param_rec.batchstep_resource_id (R) OR
     p_org_code (O)              Organization Code (Plant or Lab)
     p_batch_no (O)              Batch Number
     p_batchstep_no (O)          Batch Step Number
     p_activity (O)              Activity
     p_resource (O)              Resource
     p_parameter (O)             Process Parameter
                      All of the above to uniquely identify an process parameter
     p_process_param_rec         Gme_process_parameters%ROWTYPE  - details of the process parameter
     x_process_param_rec         Gme_process_parameters%ROWTYPE  - Output of the API with all the data
     x_return_status             Reflects return status of the API
                                 S - Success
                                 E - Failure
                                 U - Unexpected
=============================================================================================*/
   PROCEDURE insert_process_parameter (
      p_api_version           IN              NUMBER := 2.0
     ,p_validation_level      IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list         IN              VARCHAR2 := fnd_api.g_false
     ,p_commit                IN              VARCHAR2 := fnd_api.g_false
     ,x_message_count         OUT NOCOPY      NUMBER
     ,x_message_list          OUT NOCOPY      VARCHAR2
     ,x_return_status         OUT NOCOPY      VARCHAR2
     ,p_batch_no              IN              VARCHAR2 := NULL
     ,p_org_code              IN              VARCHAR2 := NULL
     ,p_validate_flexfields   IN              VARCHAR2 := fnd_api.g_false
     ,p_batchstep_no          IN              NUMBER := NULL
     ,p_activity              IN              VARCHAR2 := NULL
     ,p_parameter             IN              VARCHAR2 := NULL
     ,p_process_param_rec     IN              gme_process_parameters%ROWTYPE
     ,x_process_param_rec     OUT NOCOPY      gme_process_parameters%ROWTYPE);

/*#
* Update Process Parameter Procedure
* This particular procedure is used to insert parameter for an resource
* @param p_api_version Version of the api
* @param p_validation_level validation level
* @param p_init_msg_list message list flag - default is False
* @param p_commit commit flag - default is False
* @param x_message_count # of messages
* @param x_message_list List of messages
* @param x_return_status return status
* @param p_batch_no Batch Number
* @param p_org_code Inventory Organization (Plant or Lab)
* @param p_validate_flexfields  Indicates whether to validate the flex fields
* @param p_batchstep_no Batch Step Number
* @param p_activity activity
* @param p_parameter process parameter
* @param p_process_param_rec gme_process_parameters row type
* @param x_process_param_rec gme_process_parameters row type
* @rep:scope public
* @rep:lifecycle active
* @rep:displayname update_process_parameter
*/

   /*===========================================================================================
   Procedure
      update_process_parameter
   Description
     This particular procedure is used to insert parameter for an resource
   Parameters
     p_process_param_rec.batchstep_resource_id (R) OR
     p_org_code (O)              Organization Code (Plant or Lab)
     p_batch_no (O)              Batch Number
     p_batchstep_no (O)          Batch Step Number
     p_activity (O)              Activity
     p_resource (O)              Resource
     p_parameter (O)             Process Parameter
                      All of the above to uniquely identify an process parameter
     p_process_param_rec         Gme_process_parameters%ROWTYPE  - details of the process parameter
     x_process_param_rec         Gme_process_parameters%ROWTYPE  - Output of the API with all the data
     x_return_status             Reflects return status of the API
                                 S - Success
                                 E - Failure
                                 U - Unexpected
=============================================================================================*/
   PROCEDURE update_process_parameter (
      p_api_version           IN              NUMBER := 2.0
     ,p_validation_level      IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list         IN              VARCHAR2 := fnd_api.g_false
     ,p_commit                IN              VARCHAR2 := fnd_api.g_false
     ,x_message_count         OUT NOCOPY      NUMBER
     ,x_message_list          OUT NOCOPY      VARCHAR2
     ,x_return_status         OUT NOCOPY      VARCHAR2
     ,p_batch_no              IN              VARCHAR2 := NULL
     ,p_org_code              IN              VARCHAR2 := NULL
     ,p_validate_flexfields   IN              VARCHAR2 := fnd_api.g_false
     ,p_batchstep_no          IN              NUMBER := NULL
     ,p_activity              IN              VARCHAR2 := NULL
     ,p_parameter             IN              VARCHAR2 := NULL
     ,p_process_param_rec     IN              gme_process_parameters%ROWTYPE
     ,x_process_param_rec     OUT NOCOPY      gme_process_parameters%ROWTYPE);

/*#
* Delete Process Parameter Procedure
* This particular procedure is used to delete a parameter for a given resource
* @param p_api_version Version of the api
* @param p_validation_level validation level
* @param p_init_msg_list message list flag - default is False
* @param p_commit commit flag - default is False
* @param x_message_count # of messages
* @param x_message_list List of messages
* @param x_return_status return status
* @param p_org_code Inventory Organization (Plant or Lab)
* @param p_batch_no Batch Number
* @param p_batchstep_no Batch Step Number
* @param p_activity activity
* @param p_parameter parameter
* @param p_process_param_rec gme_process_parameters row type
* @rep:scope public
* @rep:lifecycle active
* @rep:displayname delete_process_parameter
*//*===========================================================================================
   Procedure
      delete_process_parameter
   Description
     This particular procedure is used to insert parameter for an resource
   Parameters
     p_process_param_rec.batchstep_resource_id (R) OR
     p_org_code (O)              Organization Code (Plant or Lab)
     p_batch_no (O)              Batch Number
     p_batchstep_no (O)          Batch Step Number
     p_activity (O)              Activity
     p_resource (O)              Resource
     p_parameter (O)             Process Parameter
                      All of the above to uniquely identify an process parameter
     p_process_param_rec         Gme_process_parameters%ROWTYPE  - details of the process parameter
     x_return_status             Reflects return status of the API
                                 S - Success
                                 E - Failure
                                 U - Unexpected
=============================================================================================*/
   PROCEDURE delete_process_parameter (
      p_api_version         IN              NUMBER := 2.0
     ,p_validation_level    IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list       IN              VARCHAR2 := fnd_api.g_false
     ,p_commit              IN              VARCHAR2 := fnd_api.g_false
     ,x_message_count       OUT NOCOPY      NUMBER
     ,x_message_list        OUT NOCOPY      VARCHAR2
     ,x_return_status       OUT NOCOPY      VARCHAR2
     ,p_batch_no            IN              VARCHAR2 := NULL
     ,p_org_code            IN              VARCHAR2 := NULL
     ,p_batchstep_no        IN              NUMBER := NULL
     ,p_activity            IN              VARCHAR2 := NULL
     ,p_parameter           IN              VARCHAR2 := NULL
     ,p_process_param_rec   IN              gme_process_parameters%ROWTYPE);

/*#
* Insert batch step procedure
* This procedure inserts the new step to the batch
* @param p_api_version Version of the api
* @param p_validation_level validation level
* @param p_init_msg_list message list flag - default is False
* @param p_commit commit flag - default is False
* @param x_message_count # of messages
* @param x_message_list List of messages
* @param x_return_status return status
* @param p_org_code Inventory Organization (Plant or Lab)
* @param p_validate_flexfields validate flex fields flag - default is False
* @param p_oprn_no Operation Number
* @param p_oprn_vers Operation Version
* @param p_batch_header_rec gme_batch_header rowtype
* @param p_batch_step_rec gme_batch_steps rowtype
* @param x_batch_step_rec gme_batch_steps rowtype
* @rep:scope public
* @rep:lifecycle active
* @rep:displayname insert_step
*/

    /*================================================================================
     Procedure
       insert_step
     Description
       This procedure inserts the new step to the batch

     Parameters
       p_org_code                Organization Code of Plan or Lab
       p_batch_header_rec (R)    The batch header to identify unique batch.
       p_batch_step_rec (R)      The batch step row to insert to the batch.
       p_oprn_no                 Operation Number
       p_oprn_vers               Operation Version
       x_batch_step_rec          The batch step that is returned, with all the data
       x_return_status           outcome of the API call
                                 S - Success
                                 E - Error
                                 U - Unexpected Error
    HISTORY
     SivakumarG FPBug#4395561 16-NOV-2005
      Added new argument p_validate_flexfields
   ================================================================================*/
   PROCEDURE insert_step (
      p_api_version        IN              NUMBER := 2.0
     ,p_validation_level   IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list      IN              VARCHAR2 := fnd_api.g_false
     ,p_commit             IN              VARCHAR2
     ,p_org_code           IN              VARCHAR2
     /*FPBug#4395561*/
     ,p_validate_flexfields IN              VARCHAR2 := fnd_api.g_false
     ,          /* The inventory organization under which the batch exists.*/
      p_oprn_no            IN              VARCHAR2
     ,                                /* The operation number for the step */
      p_oprn_vers          IN              NUMBER
     ,                               /* The operation version for the step */
      p_batch_header_rec   IN              gme_batch_header%ROWTYPE
     ,p_batch_step_rec     IN              gme_batch_steps%ROWTYPE
     ,x_batch_step_rec     OUT NOCOPY      gme_batch_steps%ROWTYPE
     ,x_message_count      OUT NOCOPY      NUMBER
     ,x_message_list       OUT NOCOPY      VARCHAR2
     ,x_return_status      OUT NOCOPY      VARCHAR2);

/*#
* Insert Material line procedure
* This procedure inserts  new material line in a batch
* @param p_api_version Version of the api
* @param p_validation_level validation level
* @param p_init_msg_list message list flag - default is False
* @param p_commit commit flag - default is False
* @param x_message_count # of messages
* @param x_message_list List of messages
* @param x_return_status return status
* @param p_batch_header_rec gme_batch_header rowtype
* @param p_material_detail_rec gme_material_details rowtype
* @param p_locator_code Default Consumption/Yield Locator Code
* @param p_org_code Inventory Organization 
* @param p_batchstep_no Batch Step Number
* @param p_validate_flexfields whether to validate flex fields value is 'T' or 'F'
* @param x_material_detail_rec gme_material_details rowtype
* @rep:scope public
* @rep:lifecycle active
* @rep:displayname insert_material_line
*/

/*================================================================================
    Procedure
      insert_material_line
    Description
      This procedure is used to insert a material line 

    Parameters
      p_batch_header_rec (R)          batch for which material line has to be updated
      p_material_detail_rec (R)       material line details line that has to be inserted
      p_locator_code (O)              Default Consumption/Yield Locator Code
      p_org_code (O)                  organization code
      p_batchstep_no(O)               batch step no to be associated with
      p_validate_flexfields (O)       Whether to validate the flexfields
      x_material_detail_rec           inserted material detail record
      x_return_status                 outcome of the API call
                                      S - Success
                                      E - Error
                                      U - Unexpected Error
    HISTORY    
     SivakumarG Bug#5078853 02-MAR-2006
      Procedure Created
================================================================================*/
   PROCEDURE insert_material_line (
      p_api_version           IN           NUMBER := 2.0
     ,p_validation_level      IN           NUMBER := gme_common_pvt.g_max_errors
     ,p_init_msg_list         IN           VARCHAR2 DEFAULT fnd_api.g_false
     ,p_commit                IN           VARCHAR2 DEFAULT fnd_api.g_false    
     ,p_batch_header_rec      IN           gme_batch_header%ROWTYPE
     ,p_material_detail_rec   IN           gme_material_details%ROWTYPE
     ,p_locator_code          IN           VARCHAR2
     ,p_org_code              IN           VARCHAR2
     ,p_batchstep_no          IN           NUMBER := NULL
     ,p_validate_flexfields   IN           VARCHAR2 DEFAULT fnd_api.g_false
     ,x_material_detail_rec   OUT NOCOPY   gme_material_details%ROWTYPE
     ,x_message_count         OUT NOCOPY   NUMBER
     ,x_message_list          OUT NOCOPY   VARCHAR2
     ,x_return_status         OUT NOCOPY   VARCHAR2);


/*#
* Update Material line procedure
* This procedure updates the existing material line in a batch
* @param p_api_version Version of the api
* @param p_validation_level validation level
* @param p_init_msg_list message list flag - default is False
* @param p_commit commit flag - default is False
* @param x_message_count # of messages
* @param x_message_list List of messages
* @param x_return_status return status
* @param p_batch_header_rec gme_batch_header rowtype
* @param p_material_detail_rec gme_material_details rowtype
* @param p_locator_code Default Consumption/Yield Locator Code
* @param p_org_code Inventory Organization 
* @param p_scale_phantom flag to indicate whether only phantom product to be scaled or complete phantom batch default is False
* @param p_validate_flexfields whether to validate flex fields value is 'T' or 'F'
* @param x_material_detail_rec gme_material_details rowtype
* @rep:scope public
* @rep:lifecycle active
* @rep:displayname update_material_line
*/


/*================================================================================
    Procedure
      update_material_line
    Description
      This procedure is used to update a material line 

    Parameters
      p_batch_header_rec (R)          batch for which material line has to be updated
      p_material_detail_rec (R)       material line that has to be updated
      p_locator_code (O)              Default Consumption/Yield Locator Code
      p_org_code (O)                  organization code
      p_scale_phantom (O)             to scale phantom product only or total batch
      p_validate_flexfields (O)       Whether to validate the flexfields
      x_material_detail_rec           updated material detail record
      x_return_status                 outcome of the API call
                                      S - Success
                                      E - Error
                                      U - Unexpected Error
    HISTORY    
     SivakumarG Bug#5078853 02-MAR-2006
      Procedure Created
  ================================================================================*/
    PROCEDURE update_material_line (
      p_api_version           IN           NUMBER := 2.0
     ,p_validation_level      IN           NUMBER := gme_common_pvt.g_max_errors
     ,p_init_msg_list         IN           VARCHAR2 DEFAULT fnd_api.g_false
     ,p_commit                IN           VARCHAR2 DEFAULT fnd_api.g_false    
     ,p_batch_header_rec      IN           gme_batch_header%ROWTYPE
     ,p_material_detail_rec   IN           gme_material_details%ROWTYPE
     ,p_locator_code          IN           VARCHAR2
     ,p_org_code              IN           VARCHAR2
     ,p_scale_phantom         IN           VARCHAR2 DEFAULT fnd_api.g_false
     ,p_validate_flexfields   IN           VARCHAR2 DEFAULT fnd_api.g_false
     ,x_material_detail_rec   OUT NOCOPY   gme_material_details%ROWTYPE
     ,x_message_count         OUT NOCOPY   NUMBER
     ,x_message_list          OUT NOCOPY   VARCHAR2
     ,x_return_status         OUT NOCOPY   VARCHAR2 );
 

/*#
* Delete Material line procedure
* This procedure deltes the existing material line in a batch
* @param p_api_version Version of the api
* @param p_validation_level validation level
* @param p_init_msg_list message list flag - default is False
* @param p_commit commit flag - default is False
* @param x_message_count # of messages
* @param x_message_list List of messages
* @param x_return_status return status
* @param p_batch_header_rec gme_batch_header rowtype
* @param p_material_detail_rec gme_material_details rowtype
* @param p_org_code Inventory Organization 
* @rep:scope public
* @rep:lifecycle active
* @rep:displayname delete_material_line
*/

 /*================================================================================
    Procedure
      delete_material_line
    Description
      This procedure is used to delete a material line 

    Parameters
      p_batch_header_rec (R)          batch for which material line has to be updated
      p_material_detail_rec (R)       material line that has to be updated
      p_org_code (O)                  organization code
      x_return_status                 outcome of the API call
                                      S - Success
                                      E - Error
                                      U - Unexpected Error
    HISTORY    
     SivakumarG Bug#5078853 02-MAR-2006
      Procedure Created
================================================================================*/
   PROCEDURE delete_material_line (
      p_api_version           IN           NUMBER := 2.0
     ,p_validation_level      IN           NUMBER := gme_common_pvt.g_max_errors
     ,p_init_msg_list         IN           VARCHAR2 DEFAULT fnd_api.g_false
     ,p_commit                IN           VARCHAR2 DEFAULT fnd_api.g_false    
     ,p_batch_header_rec      IN           gme_batch_header%ROWTYPE
     ,p_material_detail_rec   IN           gme_material_details%ROWTYPE
     ,p_org_code              IN           VARCHAR2 
     ,x_message_count         OUT NOCOPY   NUMBER
     ,x_message_list          OUT NOCOPY   VARCHAR2
     ,x_return_status         OUT NOCOPY   VARCHAR2);


/*#
* Delete batch step procedure
* This procedure deletes an existing step from a batch
* @param p_api_version Version of the api
* @param p_validation_level validation level
* @param p_init_msg_list message list flag - default is False
* @param p_commit commit flag - default is False
* @param x_message_count # of messages
* @param x_message_list List of messages
* @param x_return_status return status
* @param p_org_code Inventory Organization (Plant or Lab)
* @param p_batch_no Batch Number
* @param p_batch_step_rec gme_batch_steps rowtype
* @rep:scope public
* @rep:lifecycle active
* @rep:displayname delete_step
*/
   /*================================================================================
     Procedure
       delete_step
     Description
       This procedure deletes the step associated with the batch

     Parameters
       p_org_code                Organization_code of Plant or Lab
       p_batch_step_rec (R)      The batch step row to identify the step
                                 Following columns are used from this row.
                                 batchstep_id  (R)
       x_return_status           outcome of the API call
                                 S - Success
                                 E - Error
                                 U - Unexpected Error
   ================================================================================*/
   PROCEDURE delete_step (
      p_api_version        IN              NUMBER := 2.0
     ,p_validation_level   IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list      IN              VARCHAR2 := fnd_api.g_false
     ,p_commit             IN              VARCHAR2
     ,p_org_code           IN              VARCHAR2
     ,p_batch_no           IN              VARCHAR2
     ,x_message_count      OUT NOCOPY      NUMBER
     ,x_message_list       OUT NOCOPY      VARCHAR2
     ,x_return_status      OUT NOCOPY      VARCHAR2
     ,p_batch_step_rec     IN              gme_batch_steps%ROWTYPE);
     
/*#
* Revert Batch Procedure
* This procedure closes batch.
* @param p_continue_lpn_txn Indicates whether to continue processing a batch when product or byproduct has lpn transaction.
* @param p_api_version Version of the api
* @param p_validation_level validation level
* @param p_init_msg_list message list flag - default is False
* @param p_commit commit flag - default is False
* @param x_message_count # of messages
* @param x_message_list List of messages
* @param x_return_status return status
* @param p_org_code Inventory Organization (Plant or Lab)
* @param p_create_resv_pend_lots Create reservations and/or pending product lots during revert batch.
* @param p_batch_header_rec gme_batch_steps rowtype
* @param x_batch_header_rec gme_batch_steps rowtype
* @rep:scope public
* @rep:lifecycle active
* @rep:displayname revert_batch
*/
    /*================================================================================
     Procedure
       revert_batch
     Description
       This procedure revert batch and all the phantom batches.
        It also closes all the steps.

     Parameters
       p_org_code (O)            Organization code of Plant or Lab
       p_batch_header_rec (R)    The batch header row to identify the batch
                                 Following columns are used from this row.
                                 batch_id  (R)
                                 batch_close_date (O)
      p_continue_lpn_txn (O)     Indicates whether to continue processing a batch when product or byproduct has lpn transaction.
       x_batch_header_rec        The batch header that is returned, with all the data
       x_return_status           outcome of the API call
                                 S - Success
                                 E - Error
                                 U - Unexpected Error
   ================================================================================*/
    PROCEDURE revert_batch (
      p_continue_lpn_txn         IN              VARCHAR2 := 'N'
     ,p_api_version            	IN              NUMBER := 2.0
     ,p_validation_level       	IN              NUMBER 
     ,p_init_msg_list          	IN              VARCHAR2  
     ,p_commit                 	IN              VARCHAR2 
     ,x_message_count          	OUT NOCOPY      NUMBER
     ,x_message_list           	OUT NOCOPY      VARCHAR2
     ,x_return_status          	OUT NOCOPY      VARCHAR2
     ,p_org_code              	IN            	 VARCHAR2
     ,p_create_resv_pend_lots    IN              NUMBER DEFAULT 1    -- Bug 23640627
     ,p_batch_header_rec       	IN              gme_batch_header%ROWTYPE
     ,x_batch_header_rec      	OUT NOCOPY 	gme_batch_header%ROWTYPE);
    
/*#
* Revert Batch Step Procedure
* This procedure revert step.
* @param p_api_version Version of the api
* @param p_validation_level validation level
* @param p_init_msg_list message list flag - default is False
* @param p_commit commit flag - default is False
* @param x_message_count # of messages
* @param x_message_list List of messages
* @param x_return_status return status
* @param p_org_code Inventory Organization (Plant or Lab)
* @param p_batch_no Batch Number
* @param p_create_resv_pend_lots Create reservations and/or pending product lots during revert batch.
* @param p_batch_step_rec gme_batch_steps rowtype
* @param x_batch_step_rec gme_batch_steps rowtype
* @rep:scope public
* @rep:lifecycle active
* @rep:displayname revert_step
*/

   /*================================================================================
     Procedure
       revert_step
     Description
       This procedure revert step.

     Parameters
       p_org_code (O)            Organization Code for Plant or Lab
       p_batch_no (O)            Batch Number to uniquely Identify 
       p_batch_step_rec (R)      The batch step row to identify the step
                                 Following columns are used from this row.
                                 batchstep_id  (R)
                                 
       x_batch_step_rec          The batch step that is returned, with all the data
       x_return_status           outcome of the API call
                                 S - Success
                                 E - Error
                                 U - Unexpected Error
   ================================================================================*/
    PROCEDURE revert_step (
     p_api_version            	IN              NUMBER := 2.0
     ,p_validation_level       	IN              NUMBER 
     ,p_init_msg_list          	IN              VARCHAR2 
     ,p_commit                 	IN              VARCHAR2 
     ,x_message_count          	OUT NOCOPY      NUMBER
     ,x_message_list           	OUT NOCOPY      VARCHAR2
     ,x_return_status          	OUT NOCOPY      VARCHAR2
     ,p_org_code              	IN            	 VARCHAR2
     ,p_batch_no   		         IN		          VARCHAR2
     ,p_create_resv_pend_lots    IN              NUMBER DEFAULT 1    -- Bug 23640627
     ,p_batch_step_rec        	IN         gme_batch_steps%ROWTYPE
     ,x_batch_step_rec        	OUT NOCOPY gme_batch_steps%ROWTYPE);
     

/*#
* Close Batch Procedure
* This procedure closes batch.
* @param p_api_version Version of the api
* @param p_validation_level validation level
* @param p_init_msg_list message list flag - default is False
* @param p_commit commit flag - default is False
* @param x_message_count # of messages
* @param x_message_list List of messages
* @param x_return_status return status
* @param p_batch_header_rec gme_batch_steps rowtype
* @param p_org_code Inventory Organization (Plant or Lab)
* @param x_batch_header_rec gme_batch_steps rowtype
* @rep:scope public
* @rep:lifecycle active
* @rep:displayname close_batch
*/
    /*================================================================================
     Procedure
       close_batch
     Description
       This procedure closes batch and all the phantom batches.
        It also closes all the steps.

     Parameters
       p_org_code (O)            Organization code of Plant or Lab
       p_batch_header_rec (R)    The batch header row to identify the batch
                                 Following columns are used from this row.
                                 batch_id  (R)
                                 batch_close_date (O)

       x_batch_header_rec        The batch header that is returned, with all the data
       x_return_status           outcome of the API call
                                 S - Success
                                 E - Error
                                 U - Unexpected Error
   ================================================================================*/
   PROCEDURE close_batch (
      p_api_version        IN              NUMBER := 2.0
     ,p_validation_level   IN              NUMBER
     ,p_init_msg_list      IN              VARCHAR2
     ,p_commit             IN              VARCHAR2
     ,x_message_count      OUT NOCOPY      NUMBER
     ,x_message_list       OUT NOCOPY      VARCHAR2
     ,x_return_status      OUT NOCOPY      VARCHAR2
     ,p_batch_header_rec   IN              gme_batch_header%ROWTYPE
     ,x_batch_header_rec   OUT NOCOPY      gme_batch_header%ROWTYPE
     ,p_org_code           IN              VARCHAR2);

/*#
* Close Batch Step Procedure
* This procedure closes step.
* @param p_api_version Version of the api
* @param p_validation_level validation level
* @param p_init_msg_list message list flag - default is False
* @param p_commit commit flag - default is False
* @param x_message_count # of messages
* @param x_message_list List of messages
* @param x_return_status return status
* @param p_org_code Inventory Organization (Plant or Lab)
* @param p_batch_no Batch Number
* @param p_batch_step_rec gme_batch_steps rowtype
* @param p_delete_pending delete pending flag - default is 'F'
* @param x_batch_step_rec gme_batch_steps rowtype
* @rep:scope public
* @rep:lifecycle active
* @rep:displayname close_step
*/

   /*================================================================================
     Procedure
       close_step
     Description
       This procedure closes step.

     Parameters
       p_org_code (O)            Organization Code for Plant or Lab
       p_batch_no (O)            Batch Number to uniquely Identify 
       p_batch_step_rec (R)      The batch step row to identify the step
                                 Following columns are used from this row.
                                 batchstep_id  (R)
                                 step_close_date (O)
       p_delete_pending (R)      Delete the pending allocations if any for the
                                 material lines associated with the step.

       x_batch_step_rec          The batch step that is returned, with all the data
       x_return_status           outcome of the API call
                                 S - Success
                                 E - Error
                                 U - Unexpected Error
   ================================================================================*/
   PROCEDURE close_step (
      p_api_version        IN              NUMBER := 2
     ,                                                      /* Punit Kumar */
      p_validation_level   IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,                                                      /* Punit Kumar */
      p_init_msg_list      IN              VARCHAR2 := fnd_api.g_false
     ,p_commit             IN              VARCHAR2
     ,x_message_count      OUT NOCOPY      NUMBER
     ,x_message_list       OUT NOCOPY      VARCHAR2
     ,x_return_status      OUT NOCOPY      VARCHAR2
     ,p_batch_step_rec     IN              gme_batch_steps%ROWTYPE
     ,p_delete_pending     IN              VARCHAR2 := fnd_api.g_false
     ,p_org_code           IN              VARCHAR2
     ,p_batch_no           IN              VARCHAR2
     ,x_batch_step_rec     OUT NOCOPY      gme_batch_steps%ROWTYPE);

/*#
* Cancel Batch Procedure
* This procedure cancels a batch and all associated phantom batches.
* It also cancels all the steps.
* @param p_api_version Version of the api
* @param p_validation_level validation level
* @param p_init_msg_list message list flag - default is False
* @param p_commit commit flag - default is False
* @param x_message_count # of messages
* @param x_message_list List of messages
* @param x_return_status return status
* @param p_org_code Inventory Organization (Plant or Lab)
* @param p_batch_header_rec gme_batch_header rowtype
* @param x_batch_header_rec gme_batch_header rowtype
* @rep:scope public
* @rep:lifecycle active
* @rep:displayname cancel_batch
*/
   /*================================================================================
     Procedure
       cancel_batch
     Description
       This procedure cancels batch and all the phantom batches.
        It also cancels all the steps.

     Parameters
       p_batch_header_rec (R)    The batch header row to identify the batch
                                 Following columns are used from this row.
                                 batch_id  (R)
       p_org_code                The name of organization to which this batch belongs
       x_batch_header_rec        The batch header that is returned, with all the data
       x_return_status           outcome of the API call
                                 S - Success
                                 E - Error
                                 U - Unexpected Error
   ================================================================================*/
   PROCEDURE cancel_batch (
      p_api_version        IN              NUMBER := 2.0
     ,p_validation_level   IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list      IN              VARCHAR2 := fnd_api.g_false
     ,p_commit             IN              VARCHAR2 := fnd_api.g_false
     ,x_message_count      OUT NOCOPY      NUMBER
     ,x_message_list       OUT NOCOPY      VARCHAR2
     ,x_return_status      OUT NOCOPY      VARCHAR2
     ,p_org_code           IN              VARCHAR2
     ,p_batch_header_rec   IN              gme_batch_header%ROWTYPE
     ,x_batch_header_rec   OUT NOCOPY      gme_batch_header%ROWTYPE);

   /*#
* Reopen Batch Procedure
* This procedure reopens a batch and all associated phantom batches.
* It also reopens all the steps, if requested
* @param p_api_version Version of the api
* @param p_validation_level validation level
* @param p_init_msg_list message list flag - default is False
* @param p_commit commit flag - default is False
* @param x_message_count # of messages
* @param x_message_list List of messages
* @param x_return_status return status
* @param p_org_code Inventory Organization (Plant or Lab)
* @param p_batch_header_rec gme_batch_header rowtype
* @param p_reopen_steps reopen steps flag - default is 'F'
* @param x_batch_header_rec gme_batch_header rowtype
* @rep:scope public
* @rep:lifecycle active
* @rep:displayname reopen_batch
*/
   /*================================================================================
    Procedure
      reopen_batch
    Description
      This procedure reopens batch and all the phantom batches.
       It also reopens all the steps, if requested so.

    Parameters
      p_org_code                Organization code for the Plant or Lab
      p_batch_header_rec (R)    The batch header row to identify the batch
                                Following columns are used from this row.
                                batch_id  (R)
      p_reopen_steps (O)        Reopen all the steps.

      x_batch_header_rec        The batch header that is returned, with all the data
      x_return_status           outcome of the API call
                                S - Success
                                E - Error
                                U - Unexpected Error
  ================================================================================*/
   PROCEDURE reopen_batch (
      p_api_version        IN              NUMBER := 2
     ,                                                      /* Punit Kumar */
      p_validation_level   IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,                                                      /* Punit Kumar */
      p_init_msg_list      IN              VARCHAR2 := fnd_api.g_false
     ,p_commit             IN              VARCHAR2
     ,x_message_count      OUT NOCOPY      NUMBER
     ,x_message_list       OUT NOCOPY      VARCHAR2
     ,x_return_status      OUT NOCOPY      VARCHAR2
     ,p_batch_header_rec   IN              gme_batch_header%ROWTYPE
     ,p_org_code           IN              VARCHAR2
     ,p_reopen_steps       IN              VARCHAR2 := fnd_api.g_false
     ,x_batch_header_rec   OUT NOCOPY      gme_batch_header%ROWTYPE);

/*#
* Reopen Step Procedure
* This procedure reopens a step.
* @param p_api_version Version of the api
* @param p_validation_level validation level
* @param p_init_msg_list message list flag - default is False
* @param p_commit commit flag - default is False
* @param x_message_count # of messages
* @param x_message_list List of messages
* @param x_return_status return status
* @param p_org_code Inventory Organization (Plant or Lab)
* @param p_batch_header_rec gme_batch_header rowtype
* @param p_batch_step_rec gme_batch_steps rowtype
* @param x_batch_step_rec gme_batch_steps rowtype
* @rep:scope public
* @rep:lifecycle active
* @rep:displayname reopen_step
*/

   /*================================================================================
     Procedure
       reopen_step
     Description
       This procedure reopens step.

     Parameters
       p_batch_step_rec (R)      The batch header row to identify the batch
                                 Following columns are used from this row.
                                 batchstep_id  (R)

       x_batch_step_rec          The batch step that is returned, with all the data
       p_batch_header_rec        The batch header to identify unique batch
       x_return_status           outcome of the API call
                                 S - Success
                                 E - Error
                                 U - Unexpected Error
   ================================================================================*/
   PROCEDURE reopen_step (
      p_api_version        IN              NUMBER := 2
     ,                                                      /* Punit Kumar */
      p_validation_level   IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,                                                      /* Punit Kumar */
      p_init_msg_list      IN              VARCHAR2 := fnd_api.g_false
     ,p_commit             IN              VARCHAR2
     ,x_message_count      OUT NOCOPY      NUMBER
     ,x_message_list       OUT NOCOPY      VARCHAR2
     ,x_return_status      OUT NOCOPY      VARCHAR2
     ,p_org_code           IN              VARCHAR2
     ,p_batch_header_rec   IN              gme_batch_header%ROWTYPE
     ,p_batch_step_rec     IN              gme_batch_steps%ROWTYPE
     ,x_batch_step_rec     OUT NOCOPY      gme_batch_steps%ROWTYPE);

/*#
* Incremental Backflush Procedure
* This particular procedure is used to delete a parameter for a given resource
* @param p_api_version Version of the api
* @param p_validation_level validation level
* @param p_init_msg_list message list flag - default is False
* @param p_commit commit flag - default is False
* @param x_message_count # of messages
* @param x_message_list List of messages
* @param x_return_status return status
* @param p_batch_header_rec gme_batch_header rowtype
* @param p_org_code Organization code that the batch belongs to
* @param p_material_detail_rec gme_material_details rowtype
* @param p_qty Incremental Quantity 
* @param p_qty_type Supplied quantity type (incremental, actual, percentage)
* @param p_trans_date Transaction Date for the incremental backflush
* @param p_ignore_exception Ignore exceptions raised during Incremental Backflush
* @param p_adjust_cmplt Treat the Incremental backflush as adjustment for the completed batch
* @param p_create_resv_pend_lots Create reservations and/or pending product lots during negative incremental backflush.
* @param p_tolerance Used during positive incremental backflush to determine 100 % equivalency to relieve remaining reservation balance.
* @param x_exception_material_tbl table of exception records
* @rep:scope public
* @rep:lifecycle active
* @rep:displayname incremental_backflush
*/
/*===========================================================================================
   Procedure
      incremental_backflush
   Description
     This particular procedure is used to incrementally backflush a batch.

=============================================================================================*/
   PROCEDURE incremental_backflush (
      p_api_version              IN              NUMBER 
     ,p_validation_level         IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list            IN              VARCHAR2 := fnd_api.g_false
     ,p_commit                   IN              VARCHAR2 := fnd_api.g_false
     ,x_message_count            OUT NOCOPY      NUMBER
     ,x_message_list             OUT NOCOPY      VARCHAR2
     ,x_return_status            OUT NOCOPY      VARCHAR2
     ,p_batch_header_rec         IN              gme_batch_header%ROWTYPE
     ,p_org_code                 IN              VARCHAR2
     ,p_material_detail_rec      IN              gme_material_details%ROWTYPE
     ,p_qty                      IN              NUMBER
     ,p_qty_type                 IN              NUMBER := 0
     ,p_trans_date               IN              DATE
     ,p_ignore_exception         IN              VARCHAR2 := fnd_api.g_false
     ,p_adjust_cmplt             IN              VARCHAR2 := fnd_api.g_true
     ,p_create_resv_pend_lots    IN              NUMBER DEFAULT 1 -- Bug 22317010 
     ,p_tolerance                IN              NUMBER DEFAULT NULL -- Bug 22317010 / 22764488
     ,x_exception_material_tbl   OUT NOCOPY      gme_common_pvt.exceptions_tab);
/*#
* Create Material Transaction Procedure
* This procedure Creates a Material Transaction.
* @param p_api_version Version of the api
* @param p_validation_level validation level
* @param p_init_msg_list message list flag - default is False
* @param p_commit commit flag - default is False
* @param p_save_batch Call save batch api - default is True
* @param x_message_count # of messages
* @param x_message_list List of messages
* @param x_return_status return status
* @param p_org_code Inventory Organization (Plant or Lab)
* @param p_mmti_rec mtl_transactions_interface rowtype
* @param p_mmli_tbl table of lot records
* @param p_batch_no Batch Number
* @param p_line_no Line Number
* @param p_line_type Material Type (Ingredient, Product, Byproduct)
* @param p_create_lot If new lot should be created ('T','F')
* @param p_generate_lot If lot should be generated ('T','F')
* @param p_generate_parent_lot If parent lot should be generated ('T','F')
* @param x_mmt_rec mtl_material_transactions rowtype with the data
* @param x_mmln_tbl table of lot records, those were inserted
* @rep:scope public
* @rep:lifecycle active
* @rep:displayname create_material_txn
*/

   /*================================================================================
     Procedure
       create_material_txn
     Description
       This procedure creates material transaction.

     Parameters
       x_return_status           outcome of the API call
                                 S - Success
                                 E - Error
                                 U - Unexpected Error
   ================================================================================*/

   PROCEDURE create_material_txn (
      p_api_version           IN              NUMBER := 2.0
     ,p_validation_level      IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list         IN              VARCHAR2 := fnd_api.g_false
     ,p_commit                IN              VARCHAR2 := fnd_api.g_false
     ,p_save_batch            IN              VARCHAR2 := fnd_api.g_true  -- Bug 14685438
     ,x_message_count         OUT NOCOPY      NUMBER
     ,x_message_list          OUT NOCOPY      VARCHAR2
     ,x_return_status         OUT NOCOPY      VARCHAR2
     ,p_org_code              IN              VARCHAR2 := NULL
     ,p_mmti_rec              IN              mtl_transactions_interface%ROWTYPE
     ,p_mmli_tbl              IN              gme_common_pvt.mtl_trans_lots_inter_tbl
     ,p_batch_no              IN              VARCHAR2 := NULL
     ,p_line_no               IN              NUMBER := NULL
     ,p_line_type             IN              NUMBER := NULL
     ,p_create_lot            IN              VARCHAR2 := NULL
     ,p_generate_lot          IN              VARCHAR2 := NULL
     ,p_generate_parent_lot   IN              VARCHAR2 := NULL
     ,x_mmt_rec               OUT NOCOPY      mtl_material_transactions%ROWTYPE
     ,x_mmln_tbl              OUT NOCOPY      gme_common_pvt.mtl_trans_lots_num_tbl);
/*#
* Update Material Transaction Procedure
* This procedure Creates a Material Transaction.
* @param p_api_version Version of the api
* @param p_validation_level validation level
* @param p_init_msg_list message list flag - default is False
* @param p_commit commit flag - default is False
* @param x_message_count # of messages
* @param x_message_list List of messages
* @param x_return_status return status
* @param p_transaction_id Identifier to the transaction to be updated.
* @param p_mmti_rec mtl_transactions_interface rowtype
* @param p_mmli_tbl table of lot records
* @param p_create_lot If new lot should be created ('T','F')
* @param p_generate_lot If lot should be generated ('T','F')
* @param p_generate_parent_lot If parent lot should be generated ('T','F')
* @param x_mmt_rec mtl_material_transactions rowtype with the data
* @param x_mmln_tbl table of lot records, those were inserted
* @rep:scope public
* @rep:lifecycle active
* @rep:displayname update_material_txn
*/

   /*================================================================================
     Procedure
       update_material_txn
     Description
       This procedure updates material transaction.

     Parameters
       x_return_status           outcome of the API call
                                 S - Success
                                 E - Error
                                 U - Unexpected Error
   ================================================================================*/

   PROCEDURE update_material_txn (
      p_api_version           IN              NUMBER := 2.0
     ,p_validation_level      IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list         IN              VARCHAR2 := fnd_api.g_false
     ,p_commit                IN              VARCHAR2 := fnd_api.g_false
     ,x_message_count         OUT NOCOPY      NUMBER
     ,x_message_list          OUT NOCOPY      VARCHAR2
     ,x_return_status         OUT NOCOPY      VARCHAR2
     ,p_transaction_id        IN              NUMBER
     ,p_mmti_rec              IN              mtl_transactions_interface%ROWTYPE
     ,p_mmli_tbl              IN              gme_common_pvt.mtl_trans_lots_inter_tbl
     ,p_create_lot            IN              VARCHAR2 := NULL
     ,p_generate_lot          IN              VARCHAR2 := NULL
     ,p_generate_parent_lot   IN              VARCHAR2 := NULL
     ,x_mmt_rec               OUT NOCOPY      mtl_material_transactions%ROWTYPE
     ,x_mmln_tbl              OUT NOCOPY      gme_common_pvt.mtl_trans_lots_num_tbl);

/*#
* Delete Material Transaction Procedure
* This procedure deletes a Material Transaction.
* @param p_api_version Version of the api
* @param p_validation_level validation level
* @param p_init_msg_list message list flag - default is False
* @param p_commit commit flag - default is False
* @param x_message_count # of messages
* @param x_message_list List of messages
* @param x_return_status return status
* @param p_transaction_id Identifier to the transaction to be updated.
* @rep:scope public
* @rep:lifecycle active
* @rep:displayname delete_material_txn
*/

   /*================================================================================
     Procedure
       delete_material_txn
     Description
       This procedure updates material transaction.

     Parameters
       x_return_status           outcome of the API call
                                 S - Success
                                 E - Error
                                 U - Unexpected Error
   ================================================================================*/
   PROCEDURE delete_material_txn (
      p_api_version        IN              NUMBER := 2.0
     ,p_validation_level   IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list      IN              VARCHAR2 := fnd_api.g_false
     ,p_commit             IN              VARCHAR2 := fnd_api.g_false
     ,x_message_count      OUT NOCOPY      NUMBER
     ,x_message_list       OUT NOCOPY      VARCHAR2
     ,x_return_status      OUT NOCOPY      VARCHAR2
     ,p_transaction_id     IN              NUMBER);

/*#
* Reroute Batch Procedure
* This procedure reroutes batch (typically change the route associated with the batch).
* @param p_api_version Version of the api
* @param p_validation_level validation level
* @param p_init_msg_list message list flag - default is False
* @param p_commit commit flag - default is False
* @param x_message_count # of messages
* @param x_message_list List of messages
* @param x_return_status return status
* @param p_batch_header_rec gme_batch_header rowtype
* @param p_validity_rule_id validity rule id
* @param x_batch_header_rec gme_batch_header rowtype
* @param p_org_code Inventory Organization (Plant or Lab)
* @param p_use_workday_cal Whether workday calendar will be used for batch date calculations
* @param p_contiguity_override Whether indicates batch needs to be completed in one contiguous block of time
* @rep:scope public
* @rep:lifecycle active
* @rep:displayname reroute_batch
*/

   /*================================================================================
     Procedure
       reroute_batch
     Description
       This procedure reroutes batch (typically change the route associated with the batch).

     Parameters
       p_batch_header_rec (R)    The batch header row to identify the batch
                                 Following columns are used from this row.
                                 batch_id  (R)
       p_org_code                (R) if batch_no is specified instead of batch_id
                                 on batch header row
       p_validity_rule_id (R)    Recipe validity rule id for the new recipe.
       p_use_workday_cal         VARCHAR2('F','T') - to use workday calendar or not.
       p_contiguity_override     VARCHAR2('F','T') - for contigious period of calendar.
       x_batch_header_rec        The batch header that is returned, with all the data
       x_return_status           outcome of the API call
                                 S - Success
                                 E - Error
                                 U - Unexpected Error
                                 C - No continous periods found
   ================================================================================*/
   PROCEDURE reroute_batch (
      p_api_version           IN              NUMBER := 2.0
     ,p_validation_level      IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list         IN              VARCHAR2
            DEFAULT fnd_api.g_false
     ,p_commit                IN              VARCHAR2
            DEFAULT fnd_api.g_false
     ,p_batch_header_rec      IN              gme_batch_header%ROWTYPE
     ,p_validity_rule_id      IN              NUMBER
     ,p_org_code              IN              VARCHAR2
     ,p_use_workday_cal       IN              VARCHAR2
            DEFAULT fnd_api.g_false
     ,p_contiguity_override   IN              VARCHAR2
            DEFAULT fnd_api.g_false
     ,x_message_count         OUT NOCOPY      NUMBER
     ,x_message_list          OUT NOCOPY      VARCHAR2
     ,x_return_status         OUT NOCOPY      VARCHAR2
     ,x_batch_header_rec      OUT NOCOPY      gme_batch_header%ROWTYPE);


/*#
* Terminate Batch Procedure
* This procedure terminates a batch and all associated phantom batches.
* It also terminates all the steps.
* @param p_api_version Version of the api
* @param p_validation_level validation level
* @param p_init_msg_list message list flag - default is False
* @param p_commit commit flag - default is False
* @param x_message_count # of messages
* @param x_message_list List of messages
* @param x_return_status return status
* @param p_org_code Inventory Organization (Plant or Lab)
* @param p_reason_name Reason Code for termination
* @param p_batch_header_rec gme_batch_header rowtype
* @param x_batch_header_rec gme_batch_header rowtype
* @rep:scope public
* @rep:lifecycle active
* @rep:displayname terminate_batch
*/
   /*================================================================================
    Procedure
      terminate_batch

    Description
      This procedure terminates batch and all the phantom batches.
       It also terminates all the steps.

    Parameters
      p_batch_header_rec (R)    The batch header row to identify the batch
                                Following columns are used from this row.
                                batch_id  (R)
      p_org_code (O)            The name of organization to which this batch belongs
      p_reason_name (O)         Reason to terminate the batch
      x_batch_header_rec        The batch header that is returned, with all the data
      x_return_status           outcome of the API call
                                S - Success
                                E - Error
                                U - Unexpected Error
  ================================================================================*/
   PROCEDURE terminate_batch (
      p_api_version        IN              NUMBER := 2.0
     ,p_validation_level   IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list      IN              VARCHAR2 := fnd_api.g_false
     ,p_commit             IN              VARCHAR2 := fnd_api.g_false
     ,x_message_count      OUT NOCOPY      NUMBER
     ,x_message_list       OUT NOCOPY      VARCHAR2
     ,x_return_status      OUT NOCOPY      VARCHAR2
     ,p_org_code           IN              VARCHAR2
     ,p_reason_name        IN              VARCHAR2
     ,p_batch_header_rec   IN              gme_batch_header%ROWTYPE
     ,x_batch_header_rec   OUT NOCOPY      gme_batch_header%ROWTYPE);

/*#
* Convert Detail Reservation Procedure
* This procedure converted detail level reservation into transaction
* It also relieves the reservation.
* @param p_api_version Version of the api
* @param p_validation_level validation level
* @param p_init_msg_list message list flag - default is False
* @param p_commit commit flag - default is False
* @param x_message_count # of messages
* @param x_message_list List of messages
* @param x_return_status return status
* @param p_qty_convert Quantity to be converted
* @param p_reservation_rec mtl_reservations rowtype
* @param p_trans_date User specified transaction date.
* @rep:scope public
* @rep:lifecycle active
* @rep:displayname convert_dtl_reservation
*/
    /*================================================================================
     Procedure
       convert_dtl_reservation
     Description
       This procedure is used to convert a detailed reservation to WIP issue transaction.
       The full reservation quantity or a part quantity can be converted.  The reservation
       will be relieved accordingly.  Where the reservation is fully relieved, it is deleted.
       Where a reservation is at high level or part level and subinventory or locator information
       is missing, there will be an attempt to complete the detailing by defaulting values from
       the material detail line or org params.

     Parameters
       p_api_version     (O)     Requested api version.  Currently at 2.0

       p_init_msg_list   (O)     To initialize the message stack. Default is FALSE

       p_commit    (O)           Indicates whether to commit.  This is defaulted to FALSE

       x_message_count           Number of messages returned by the processing

       x_message_list            Message stack returned by the processing

       x_return_status           outcome of the API call
                                 S - Success
                                 E - Error
                                 U - Unexpected Error

       p_reservation_rec (R)     The reservation row identifying the target reservation
                                 Following columns are used from this row.
                                 reservation_id  (R)

       p_trans_date      (O)     The user can pass in a transaction date otherwise it
                                 will default to sysdate.

       p_qty_convert     (O)     The quantity to be converted to an inventory transaction
                                 This can be the full reservation quantity or part of it.
                                 Reflects the UOM defined for gme_material_details.dtl_um
                                 Default value is the full reservation quantity
      Clarification of how p_qty_convert is used from bug 6778968 going forward.
         If p_qty_convert passes a negative value, it will result in an error.
         When p_qty_convert is equal to or greater than reservation, qty converts the reservation 
         qty to a transaction and passing less than reservation qty creates transaction for
         the p_qty_convert and relieves the reservation for that qty.                       
   ================================================================================*/
   PROCEDURE convert_dtl_reservation (
      p_api_version       IN              NUMBER := 2.0
     ,p_validation_level  IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list     IN              VARCHAR2 := fnd_api.g_false
     ,p_commit            IN              VARCHAR2 := fnd_api.g_false
     ,x_message_count     OUT NOCOPY      NUMBER
     ,x_message_list      OUT NOCOPY      VARCHAR2
     ,x_return_status     OUT NOCOPY      VARCHAR2
     ,p_reservation_rec   IN              mtl_reservations%ROWTYPE
     ,p_trans_date        IN              DATE DEFAULT NULL    -- Bug 13532998
     ,p_qty_convert       IN              NUMBER := NULL);

/*#
* Insert Batch Step Resource Procedure 
* This procedure is used to insert a resource in a batchstep activity.
* @param p_api_version Version of the api
* @param p_validation_level validation level
* @param p_init_msg_list message list flag - default is False
* @param p_commit commit flag - default is False
* @param p_org_code Inventory Organization (Plant or Lab)
* @param p_batch_no Batch Number
* @param p_batchstep_no Batch Step Number
* @param p_activity Activity
* @param p_ignore_qty_below_cap Ignore if peocess qty is below capacity of the resource
* @param p_batchstep_resource_rec gme_batchstep_resource rowtype
* @param p_validate_flexfields validate flexfields flag ('T','F')
* @param x_batchstep_resource_rec gme_batchstep_resource rowtype
* @param x_message_count # of messages
* @param x_message_list List of messages
* @param x_return_status return status
* @rep:scope public
* @rep:lifecycle active
* @rep:displayname insert_batchstep_resource
*/
    /*================================================================================
     Procedure
       insert_batchstep_resource
     Description
       This procedure is used to insert a resource for an activity of a step
       The target placement for the new resource can be identified using the ID
       columns within p_batchstep_resource_rec.
       Alternatively the following keys can be used together
       p_org_code
       p_batch_no
       p_batchstep_no
       p_activity
       p_batchstep_resource_rec.resources

       The procedure will return the newly inserted row including the identifier
       x_batchstep_resource.batchstep_resource_id

     Parameters
       p_api_version               required API version
       p_validation_level          required level of validation
       p_init_msg_list             initialization of message stack
       p_commit                    controls whether or not commit is issued
       p_batchstep_resource_rec    batchstep resource to be inserted for an activity
                                   Ensure that the resources column is populated.
       p_org_code                  organization code
       p_batch_no                  batch number
       p_batchstep_no              batch step number
       p_activity                  batch step activity
       p_ignore_qty_below_cap      controls whether or not rsrc_qty can go below rsrc capacity
       p_validate_flexfield        controls whether or not flexfield validation is instigated
       x_batchstep_resource        returns inserted batchstep resource row
       x_message_count             number of messages on stack
       x_message_list              messages added to the stack during processing
       x_return_status             outcome of the API call
                                   S - Success
                                   E - Error
                                   U - Unexpected Error
   ================================================================================*/
   PROCEDURE insert_batchstep_resource (
      p_api_version              IN              NUMBER 
     ,p_validation_level         IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list            IN              VARCHAR2 := fnd_api.g_false
     ,p_commit                   IN              VARCHAR2 := fnd_api.g_false
     ,p_batchstep_resource_rec   IN              gme_batch_step_resources%ROWTYPE
     ,p_org_code                 IN              VARCHAR2 := NULL
     ,p_batch_no                 IN              VARCHAR2 := NULL
     ,p_batchstep_no             IN              NUMBER := NULL
     ,p_activity                 IN              VARCHAR2 := NULL
     ,p_ignore_qty_below_cap     IN              VARCHAR2 := fnd_api.g_false
     ,p_validate_flexfields      IN              VARCHAR2 := fnd_api.g_false
     ,x_batchstep_resource_rec   OUT NOCOPY      gme_batch_step_resources%ROWTYPE
     ,x_message_count            OUT NOCOPY      NUMBER
     ,x_message_list             OUT NOCOPY      VARCHAR2
     ,x_return_status            OUT NOCOPY      VARCHAR2);

/*#
* Update Batch Step Resource Procedure 
* This procedure is used to insert a resource in a batchstep activity.
* @param p_api_version Version of the api
* @param p_validation_level validation level
* @param p_init_msg_list message list flag - default is False
* @param p_commit commit flag - default is False
* @param p_org_code Inventory Organization (Plant or Lab)
* @param p_batch_no Batch Number
* @param p_batchstep_no Batch Step Number
* @param p_activity Activity
* @param p_ignore_qty_below_cap Ignore if peocess qty is below capacity of the resource
* @param p_batchstep_resource_rec gme_batchstep_resource rowtype
* @param p_validate_flexfields validate flexfields flag ('T','F')
* @param x_batchstep_resource_rec gme_batchstep_resource rowtype
* @param x_message_count # of messages
* @param x_message_list List of messages
* @param x_return_status return status
* @rep:scope public
* @rep:lifecycle active
* @rep:displayname update_batchstep_resource
*/
/*================================================================================
    Procedure
      update_batchstep_resource
    Description
      This procedure is used to update an existing resource of an activity of a step
      Provide either a unique identifier or key field values to target the row for update.

    Parameters
      p_api_version               required API version
      p_validation_level          required level of validation
      p_init_msg_list             initialization of message stack
      p_commit                    controls whether or not commit is issued
      p_batchstep_resource_rec    batchstep resource updates to be applied
                                  provide batchstep_resource_id as a unique identifier for the target row
                                  Alternatively, populate key fields below in conjunction with
                                  resources
      p_org_code                  organization code
      p_batch_no                  batch number
      p_batchstep_no              batch step number
      p_activity                  batch step activity
      p_ignore_qty_below_cap      controls whether or not rsrc_qty can go below rsrc capacity
      p_validate_flexfield        controls whether or not flexfield validation is instigated
      x_batchstep_resource        returns inserted batchstep resource row
      x_message_count             number of messages on stack
      x_message_list              messages added to the stack during processing
      x_return_status             outcome of the API call
                                  S - Success
                                  E - Error
                                  U - Unexpected Error
  ================================================================================*/
   PROCEDURE update_batchstep_resource (
      p_api_version              IN              NUMBER 
     ,p_validation_level         IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list            IN              VARCHAR2 := fnd_api.g_false
     ,p_commit                   IN              VARCHAR2 := fnd_api.g_false
     ,p_batchstep_resource_rec   IN              gme_batch_step_resources%ROWTYPE
     ,p_org_code                 IN              VARCHAR2 := NULL
     ,p_batch_no                 IN              VARCHAR2 := NULL
     ,p_batchstep_no             IN              NUMBER := NULL
     ,p_activity                 IN              VARCHAR2 := NULL
     ,p_ignore_qty_below_cap     IN              VARCHAR2 := fnd_api.g_false
     ,p_validate_flexfields      IN              VARCHAR2 := fnd_api.g_false
     ,x_batchstep_resource_rec   OUT NOCOPY      gme_batch_step_resources%ROWTYPE
     ,x_message_count            OUT NOCOPY      NUMBER
     ,x_message_list             OUT NOCOPY      VARCHAR2
     ,x_return_status            OUT NOCOPY      VARCHAR2);

/*#
* Delete Batch Step Resource Procedure 
* This procedure is used to delete a resource from a batchstep activity.
* @param p_api_version Version of the api
* @param p_validation_level validation level
* @param p_init_msg_list message list flag - default is False
* @param p_commit commit flag - default is False
* @param p_org_code Inventory Organization (Plant or Lab)
* @param p_batch_no Batch Number
* @param p_batchstep_no Batch Step Number
* @param p_activity Activity
* @param p_batchstep_resource_id To uniquely identify gme_batchstep_resource
* @param p_resource Resource Name
* @param x_message_count # of messages
* @param x_message_list List of messages
* @param x_return_status return status
* @rep:scope public
* @rep:lifecycle active
* @rep:displayname delete_batchstep_resource
*/
/*================================================================================
    Procedure
      delete_batchstep_resource
    Description
      This procedure is used to delete an existing resource of an activity of a step
      Provide either a unique identifier or key field values to target the row.

    Parameters
      p_api_version               required API version
      p_validation_level          required level of validation
      p_init_msg_list             initialization of message stack
      p_commit                    controls whether or not commit is issued
      p_batchstep_resource_id     uniquely identifies the batchstep resource for deletion
      p_org_code                  organization code
      p_batch_no                  batch number
      p_batchstep_no              batch step number
      p_activity                  batch step activity
      p_resource                  batch step activity resource
      x_return_status             outcome of the API call
                                  S - Success
                                  E - Error
                                  U - Unexpected Error
  ================================================================================*/
   PROCEDURE delete_batchstep_resource (
      p_api_version             IN              NUMBER := 2.0
     ,p_validation_level        IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list           IN              VARCHAR2 := fnd_api.g_false
     ,p_commit                  IN              VARCHAR2 := fnd_api.g_false
     ,p_batchstep_resource_id   IN              NUMBER := NULL
     ,p_org_code                IN              VARCHAR2 := NULL
     ,p_batch_no                IN              VARCHAR2 := NULL
     ,p_batchstep_no            IN              NUMBER := NULL
     ,p_activity                IN              VARCHAR2 := NULL
     ,p_resource                IN              VARCHAR2 := NULL
     ,x_message_count           OUT NOCOPY      NUMBER
     ,x_message_list            OUT NOCOPY      VARCHAR2
     ,x_return_status           OUT NOCOPY      VARCHAR2);

/*#
* Auto Detail Line Procedure
* This procedure Creates detail level reservations for the given ingredient line
*   as per the picking rules defined.
* @param p_api_version Version of the api
* @param p_validation_level validation level
* @param p_init_msg_list message list flag - default is False
* @param p_commit commit flag - default is False
* @param x_message_count # of messages
* @param x_message_list List of messages
* @param x_return_status return status
* @param p_org_code Inventory Organization (Plant or Lab)
* @param p_batch_no Batch number
* @param p_line_no Line number of the ingredient
* @param p_material_detail_id Identifier for the ingredient line
* @rep:scope public
* @rep:lifecycle active
* @rep:displayname auto_detail_line
*/
    /*================================================================================
     Procedure
       auto_detail_line
     Description
       This procedure is used to generate detailed reservations for an ingredient line.
       The WMS rules engine is called to determine which inventory is reserved according to
       availability and selection rules.
       The ingredeint line MUST be identified either using p_material_detail_id OR
       the combination of p_org_code,p_batch_no,p_line_no

     Parameters
       p_api_version     (O)     Requested api version.  Currently at 2.0
       p_init_msg_list   (O)     To initialize the message stack. Default is FALSE
       p_commit          (O)     Indicates whether to commit.  This is defaulted to FALSE
       x_message_count           Number of messages returned by the processing
       x_message_list            Message stack returned by the processing
       x_return_status           outcome of the API call
                                 S - Success
                                 E - Error
                                 U - Unexpected Error

       p_material_detail_id (O)  Identifier for the ingredient row against which
                                 detailed reservations will be placed.
                                 Must be supplied if org/batch/line below are not.

       p_org_code           (O)  Organization owning the production batch
       p_batch_no           (O)  Production batch
       p_line_no            (O)  Ingredient line within the production batch
   ================================================================================*/
   PROCEDURE auto_detail_line (
      p_api_version          IN              NUMBER := 2.0
     ,p_validation_level     IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list        IN              VARCHAR2 := fnd_api.g_false
     ,p_commit               IN              VARCHAR2 := fnd_api.g_false
     ,x_message_count        OUT NOCOPY      NUMBER
     ,x_message_list         OUT NOCOPY      VARCHAR2
     ,x_return_status        OUT NOCOPY      VARCHAR2
     ,p_material_detail_id   IN              NUMBER
     ,p_org_code             IN              VARCHAR2
     ,p_batch_no             IN              VARCHAR2
     ,p_line_no              IN              NUMBER);
     
/*#
* Auto Detail Batch Reservations Procedure
* This procedure Creates detail level reservations for each ingredient line
*   in the production batch as per the picking rules defined.
* @param p_api_version Version of the api
* @param p_init_msg_list message list flag - default is False
* @param p_commit commit flag - default is False
* @param x_message_count # of messages
* @param x_message_list List of messages
* @param x_return_status return status
* @param p_org_code Inventory Organization                 
* @param p_batch_rec Batch header row
* @rep:scope public
* @rep:lifecycle active
* @rep:displayname auto_detail_batch
*/
   /*================================================================================
    Procedure
      auto_detail_batch                
    Description
      This procedure is used to generate detailed reservations for the ingredient lines 
      of a production batch.     
      The WMS rules engine is called to determine which inventory is reserved according to 
      availability and selection rules.
  
    Parameters
      p_api_version     (O)     Requested api version.  Currently at 2.0                
      p_init_msg_list   (O)     To initialize the message stack. Default is FALSE
      p_commit		(O)     Indicates whether to commit.  This is defaulted to FALSE
      x_message_count           Number of messages returned by the processing 
      x_message_list            Message stack returned by the processing 
      x_return_status           outcome of the API call
                                S - Success
                                E - Error
                                U - Unexpected Error

      p_org_code           (R)  Organization owning the production batch
      p_batch_rec          (R)  Production batch
  ================================================================================*/

  PROCEDURE auto_detail_batch      (p_api_version            IN          NUMBER := 2.0
                                   ,p_init_msg_list          IN          VARCHAR2 := FND_API.G_FALSE
                                   ,p_commit                 IN          VARCHAR2 := FND_API.G_FALSE
                                   ,x_message_count          OUT NOCOPY  NUMBER
                                   ,x_message_list           OUT NOCOPY  VARCHAR2
                                   ,x_return_status          OUT NOCOPY  VARCHAR2
                                   ,p_org_code               IN          VARCHAR2
                                   ,p_batch_rec              IN          GME_BATCH_HEADER%ROWTYPE);
/*#
* Inserts an activity for a step.
* @param p_api_version Version of the api
* @param p_validation_level validation level
* @param p_init_msg_list message list flag - default is False
* @param p_commit commit flag - default is False
* @param x_message_count # of messages
* @param x_message_list List of messages
* @param x_return_status return status
* @param p_batchstep_activity_rec Batch step activity record to identify step activities
* @param p_batchstep_resource_tbl Resources table
* @param p_org_code Organization Code
* @param p_batch_no Batch Number
* @param p_batchstep_no Batch Step Number
* @param p_ignore_qty_below_cap Ignore quantity below cap flag - default is False
* @param p_validate_flexfield Whether to validate the flexfields - default is False
* @param x_batchstep_activity_rec Batch step activity record to identify step activities
* @rep:scope public
* @rep:lifecycle active
* @rep:displayname Insert Batchstep Activity
*/

   /*================================================================================
    Procedure
      insert_batchstep_activity
    Description
      This procedure is used to insert an activity for a step

    Parameters
      p_batchstep_activity_rec (R)    activity to be inserted for a step
      p_batchstep_resource_tbl (R) one or more resources to be inserted for the activity
      p_org_code (O)              organization code
      p_batch_no (O)              batch number
      p_batchstep_no(O)           batch step number
      p_ignore_qty_below_cap (O)  controls - allow rsrc_qty going below rsrc capacity
      p_validate_flexfield (O)    Whether to validate the flexfields
      x_batchstep_activity_rec        newly inserted activity row, can be used for update etc
      x_return_status             outcome of the API call
                                  S - Success
                                  E - Error
                                  U - Unexpected Error
  ================================================================================*/
   PROCEDURE insert_batchstep_activity (
      p_api_version              IN              NUMBER ,
      p_validation_level         IN              NUMBER
            := gme_common_pvt.g_max_errors,
      p_init_msg_list            IN              VARCHAR2
            DEFAULT fnd_api.g_false,
      p_commit                   IN              VARCHAR2
            DEFAULT fnd_api.g_false,
      p_org_code                 IN              VARCHAR2,
      p_batchstep_activity_rec   IN              gme_batch_step_activities%ROWTYPE,
      p_batchstep_resource_tbl   IN              gme_create_step_pvt.resources_tab,
      p_batch_no                 IN              VARCHAR2 := NULL,
      p_batchstep_no             IN              NUMBER := NULL,
      p_ignore_qty_below_cap     IN              VARCHAR2
            DEFAULT fnd_api.g_false,
      p_validate_flexfield       IN              VARCHAR2
            DEFAULT fnd_api.g_false,
      x_batchstep_activity_rec   OUT NOCOPY      gme_batch_step_activities%ROWTYPE,
      x_message_count            OUT NOCOPY      NUMBER,
      x_message_list             OUT NOCOPY      VARCHAR2,
      x_return_status            OUT NOCOPY      VARCHAR2
   );

/*#
* Updates an activity for a step.
* @param p_api_version Version of the api
* @param p_validation_level validation level
* @param p_init_msg_list message list flag - default is False
* @param p_commit commit flag - default is False
* @param x_message_count # of messages
* @param x_message_list List of messages
* @param x_return_status return status
* @param p_batchstep_activity_rec Batch step activity record to identify step activities
* @param p_org_code Organization Code
* @param p_batch_no Batch Number
* @param p_batchstep_no Batch Step Number
* @param p_validate_flexfield Whether to validate the flexfields - default is False
* @param x_batchstep_activity_rec Batch step activity record to identify step activities
* @rep:scope public
* @rep:lifecycle active
* @rep:displayname Update Batchstep Activity
*/

   /*================================================================================
    Procedure
      update_batchstep_activity
    Description
      This procedure is used to update an activity for a step

    Parameters
      p_batchstep_activity (R)    activity to be updated
      p_org_code (O)              organization code
      p_batch_no (O)              batch number
      p_batchstep_no(O)           batch step number
      p_validate_flexfield (O)    Whether to validate the flexfields
      x_batchstep_activity        updated activity row
      x_return_status             outcome of the API call
                                  S - Success
                                  E - Error
                                  U - Unexpected Error
  ================================================================================*/
   PROCEDURE update_batchstep_activity (
      p_api_version              IN              NUMBER ,
      p_validation_level         IN              NUMBER
            := gme_common_pvt.g_max_errors,
      p_init_msg_list            IN              VARCHAR2
            DEFAULT fnd_api.g_false,
      p_commit                   IN              VARCHAR2
            DEFAULT fnd_api.g_false,
      p_org_code                 IN              VARCHAR2,
      p_batchstep_activity_rec   IN              gme_batch_step_activities%ROWTYPE,
      p_batch_no                 IN              VARCHAR2 := NULL,
      p_batchstep_no             IN              NUMBER := NULL,
      p_validate_flexfield       IN              VARCHAR2
            DEFAULT fnd_api.g_false,
      x_batchstep_activity_rec   OUT NOCOPY      gme_batch_step_activities%ROWTYPE,
      x_message_count            OUT NOCOPY      NUMBER,
      x_message_list             OUT NOCOPY      VARCHAR2,
      x_return_status            OUT NOCOPY      VARCHAR2
   );

/*#
* Deletes an activity from a step. Note that proper identification like
* activity id or a combination of organization code, batch number, and batch step
* number must be provided for the batch step to be deleted.
* @param p_api_version Version of the API
* @param p_validation_level Validation level
* @param p_init_msg_list Message list flag - default is False
* @param p_commit Commit flag - default is False
* @param p_batchstep_activity_id Batch step activity id
* @param p_org_code Organization Code
* @param p_batch_no Batch Number
* @param p_batchstep_no Batch Step Number
* @param p_activity Activity
* @param x_message_count Number of messages
* @param x_message_list List of messages
* @param x_return_status Indicates the return status of an API.
* @rep:scope public
* @rep:lifecycle active
* @rep:displayname Delete Batchstep Activity
*/

   /*================================================================================
    Procedure
      delete_batchstep_activity
    Description
      This procedure is used to delete an activity from a step.  Note that either the
      activity_id must be provided or the combination of organization_code, batch_no, batchstep_no,
      and activity in order to uniquely identify an activity to be deleted.

    Parameters
      p_batchstep_activity_id (O) activity_id to be deleted
      p_org_code (O)              organization code
      p_batch_no (O)              batch number
      p_batchstep_no(O)           batch step number
      p_activity(O)               activity
      x_return_status             outcome of the API call
                                  S - Success
                                  E - Error
                                  U - Unexpected Error
  ================================================================================*/
   PROCEDURE delete_batchstep_activity (
      p_api_version             IN              NUMBER := 2.0,
      p_validation_level        IN              NUMBER
            := gme_common_pvt.g_max_errors,
      p_init_msg_list           IN              VARCHAR2
            DEFAULT fnd_api.g_false,
      p_commit                  IN              VARCHAR2
            DEFAULT fnd_api.g_false,
      p_org_code                IN              VARCHAR2,
      p_batchstep_activity_id   IN              NUMBER := NULL,
      p_batch_no                IN              VARCHAR2 := NULL,
      p_batchstep_no            IN              NUMBER := NULL,
      p_activity                IN              VARCHAR2 := NULL,
      x_message_count           OUT NOCOPY      NUMBER,
      x_message_list            OUT NOCOPY      VARCHAR2,
      x_return_status           OUT NOCOPY      VARCHAR2
   );     
/*#
* Convert Fpo Procedure 
* This procedure is used to convert the firm planned order to one of many batches.
* @param p_api_version Version of the api
* @param p_validation_level validation level
* @param p_init_msg_list message list flag - default is False
* @param p_commit commit flag - default is False
* @param x_message_count # of messages
* @param x_message_list List of messages
* @param p_enforce_vldt_check enforce validity check
* @param x_return_status return status
* @param p_org_code organization code 
* @param p_batch_header gme_batch_header rowtype
* @param x_batch_header gme_batch_header rowtype
* @param p_batch_size batch size
* @param p_num_batches number of batches
* @param p_validity_rule_id recipe_validity_rule_id Validity rule id
* @param p_validity_rule_tab recipe_validity_rule_tab record type
* @param p_leadtime leadtime
* @param p_batch_offset batch offset
* @param p_offset_type offset type
* @param p_plan_start_date plan start date
* @param p_plan_cmplt_date plan cmplt date
* @param p_use_shop_cal use shop calendar flag
* @param p_contiguity_override contiguity override
* @param p_use_for_all use validity rule for all batches flag - default is True
* @rep:scope public
* @rep:lifecycle active
* @rep:displayname convert_fpo  
*/

   /*================================================================================
    Procedure
      convert_fpo
    Description
      This procedure is used to convert a firm planned order to one or many batches.

    Parameters
      p_api_version         (O) API version number                 
      p_validation_level    (O) Required level of validation       
      p_init_msg_list       (O) Indicates whether or not message stack should be initialized
      p_commit              (O) T/F to instigate a COMMIT or not                               
      p_enforce_vldt_check  (O) T/F for validity rule verification                              
      p_org_code            (O) Organization Code associated with the batch                     
      p_batch_header        (R) The FPO row to identify the FPO
                                Following columns are used from this row.
                                batch_id  (R) organization_id
      p_batch_size          (R) The size of the batch to be created.
      p_num_batches         (R) Number of batches to be created.
      p_validity_rule_id    (R) The validity rule to be used to create the batch (if
                                converting to 1 batch or p_use_for_all is set to True)
      p_validity_rule_tab   (O) The validity rules tab (if converting to multiple batches)
      p_leadtime            (O) The batch duration in case routing data or
                                production rules does not exists.
      p_batch_offset        (O) The offset time between batches.
      p_offset_type         (O) The offset type.
                                0 - Start to start
                                1 - Finish to start
      p_plan_start_date     (O) The start date of the first batch. Must be provided if plan_cmplt_date is not.
      p_plan_cmplt_date     (O) The completion date of the last batch
      p_use_shop_cal        (O) T/F - to use shop calendar or not.
      p_contiguity_override (O) T/F - for contigious period of calendar.
      p_use_for_all         (O) T/F - Use the same validity rule for all the batches.
      x_return_status           outcome of the API call
                                S - Success
                                E - Error
                                U - Unexpected Error
                                V - Inventory shortages exist
  ================================================================================*/

   PROCEDURE convert_fpo (
      p_api_version          IN         NUMBER := 2.0                     
     ,p_validation_level     IN         NUMBER := gme_common_pvt.g_max_errors
     ,p_init_msg_list        IN         VARCHAR2 := fnd_api.g_false
     ,p_commit               IN         VARCHAR2 := fnd_api.g_false
     ,x_message_count        OUT NOCOPY NUMBER
     ,x_message_list         OUT NOCOPY VARCHAR2
     ,p_enforce_vldt_check   IN         VARCHAR2 := fnd_api.g_true
     ,x_return_status        OUT NOCOPY VARCHAR2
     ,p_org_code             IN         VARCHAR2 := NULL
     ,p_batch_header         IN         gme_batch_header%ROWTYPE
     ,x_batch_header         OUT NOCOPY gme_batch_header%ROWTYPE
     ,p_batch_size           IN         NUMBER
     ,p_num_batches          IN         NUMBER
     ,p_validity_rule_id     IN         NUMBER
     ,p_validity_rule_tab    IN         gme_common_pvt.recipe_validity_rule_tab
     ,p_leadtime             IN         NUMBER DEFAULT 0
     ,p_batch_offset         IN         NUMBER DEFAULT 0
     ,p_offset_type          IN         NUMBER DEFAULT 0
     ,p_plan_start_date      IN         gme_batch_header.plan_start_date%TYPE
     ,p_plan_cmplt_date      IN         gme_batch_header.plan_cmplt_date%TYPE
     ,p_use_shop_cal         IN         VARCHAR2 := fnd_api.g_false                       
     ,p_contiguity_override  IN         VARCHAR2 := fnd_api.g_true                        
     ,p_use_for_all          IN         VARCHAR2 := fnd_api.g_true);
     
/*#
* Create a pending product lot. Note that identification must be provided:
* material detail id or batch_id, line_no, line_type or
* batch_no, org_code, line_no, line_type (batch_type is assumed to be Batch)
* @param p_api_version Version of the API
* @param p_validation_level Validation level
* @param p_init_msg_list Message list flag - default is False
* @param p_commit Commit flag - default is False
* @param p_batch_header_rec Batch header record with optional key specified
* @param p_org_code Organization Code
* @param p_material_detail_rec Material detail record with key combination specified
* @param p_pending_product_lots_rec Pending product lot record with data to populate
* @param p_expiration_date Expiration date for item with expiration control as user-defined.
* @param p_create_lot Create product lot if it does not exist. Product and by-product only.
* @param p_generate_lot Generate product lot if nothing is passed. Product and by-product only.
* @param p_generate_parent_lot Generate parent product lot. Product and by-product only.
* @param p_parent_lot Specify parent lot.
* @param x_pending_product_lots_rec Output Pending product lot record.
* @param x_message_count Number of messages
* @param x_message_list List of messages
* @param x_return_status Indicates the return status of an API.
* @rep:scope public
* @rep:lifecycle active
* @rep:displayname Create Pending Product Lot
*/

   /*================================================================================
    Procedure
      create_pending_product_lot
    Description
      This procedure is used to create a pending product lot record for a specified
      material detail record.
      Following key sequences can be specified:
      material_detail_id OR
      batch_id, line_no, line_type OR
      batch_no, org_code, line_no, line_type (batch_type is assumed to be Batch)

    Parameters
      p_batch_header_rec (O)      batch header record
      p_org_code (O)              organization code
      p_material_detail_rec (O)   material detail record
      p_pending_product_lots_rec (R) pending product lots record
      p_create_lot (O)            This indicates whether to create the lot if the lot does
                                  not exist.  Default to False, do not create the lot.  This
                                  applies to products and by-products only.
      p_expiration_date           If Expiration control is user-defined, this parameter is used 
                                  to provide the expiration date. It is applicable if p_create_lot is True
      p_generate_lot (O)          This indicates to generate a lot if there is nothing passed
                                  in the lot number field of p_pending_product_lots_rec.  Default
                                  to False, do not generate the lot.  This applies to products and
                                  by-products only.
      p_generate_parent_lot (O)   This indicates to generate a parent lot.  Default
                                  to False, do not generate the parent lot.  This applies to products and
                                  by-products only.
      x_return_status             outcome of the API call
                                  S - Success
                                  E - Error
                                  U - Unexpected Error

    History
       G. Muratore    13-MAY-2010 Bug 9482460      
          Introduced new p_parent_lot parameter.
  ================================================================================*/
   
   PROCEDURE create_pending_product_lot
     (p_api_version              IN              NUMBER 
     ,p_validation_level         IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list            IN              VARCHAR2 := fnd_api.g_false
     ,p_commit                   IN              VARCHAR2 := fnd_api.g_false
     ,x_message_count            OUT NOCOPY      NUMBER
     ,x_message_list             OUT NOCOPY      VARCHAR2
     ,x_return_status            OUT NOCOPY      VARCHAR2
     ,p_batch_header_rec         IN              gme_batch_header%ROWTYPE
     ,p_org_code                 IN              VARCHAR2
     ,p_create_lot               IN              VARCHAR2 := fnd_api.g_false
     ,p_generate_lot             IN              VARCHAR2 := fnd_api.g_false
     ,p_generate_parent_lot      IN              VARCHAR2 := fnd_api.g_false
     ,p_parent_lot               IN              mtl_lot_numbers.lot_number%TYPE := NULL
     ,p_material_detail_rec      IN              gme_material_details%ROWTYPE
      /* nsinghi bug#4486074 Added the p_expiration_dt parameter. */
     ,p_expiration_date            IN              mtl_lot_numbers.expiration_date%TYPE := NULL
     ,p_pending_product_lots_rec IN              gme_pending_product_lots%ROWTYPE
     ,x_pending_product_lots_rec OUT NOCOPY      gme_pending_product_lots%ROWTYPE);

/*#
* Update a pending product lot. Note that identification:
* pending product lot id or a combination of keys must be provided.
* @param p_api_version Version of the API
* @param p_validation_level Validation level
* @param p_init_msg_list Message list flag - default is False
* @param p_commit Commit flag - default is False
* @param p_batch_header_rec Batch header record with optional key specified
* @param p_org_code Organization Code
* @param p_material_detail_rec Material detail record with key combination specified
* @param p_pending_product_lots_rec Pending product lot record with data to update
* @param x_pending_product_lots_rec Output Pending product lot record.
* @param x_message_count Number of messages
* @param x_message_list List of messages
* @param x_return_status Indicates the return status of an API.
* @rep:scope public
* @rep:lifecycle active
* @rep:displayname Update Pending Product Lot
*/

   /*================================================================================
    Procedure
      update_pending_product_lot
    Description
      This procedure is used to update a pending product lot record for a specified
      pending product lot record
      Following key sequences can be specified:
      pending_product_lot_id OR
      batch_id, line_no, line_type, sequence OR
      batch_no, org_code, line_no, line_type, sequence (batch_type is assumed to be Batch)

    Parameters
      p_batch_header_rec (O)      batch header record
      p_org_code (O)              organization code
      p_material_detail_rec (O)   material detail record
      p_pending_product_lots_rec (R) pending product lots record
      x_return_status             outcome of the API call
                                  S - Success
                                  E - Error
                                  U - Unexpected Error
  ================================================================================*/

   PROCEDURE update_pending_product_lot
     (p_api_version              IN              NUMBER
     ,p_validation_level         IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list            IN              VARCHAR2 := fnd_api.g_false
     ,p_commit                   IN              VARCHAR2 := fnd_api.g_false
     ,x_message_count            OUT NOCOPY      NUMBER
     ,x_message_list             OUT NOCOPY      VARCHAR2
     ,x_return_status            OUT NOCOPY      VARCHAR2
     ,p_batch_header_rec         IN              gme_batch_header%ROWTYPE
     ,p_org_code                 IN              VARCHAR2
     ,p_material_detail_rec      IN              gme_material_details%ROWTYPE
     ,p_pending_product_lots_rec IN              gme_pending_product_lots%ROWTYPE
     ,x_pending_product_lots_rec OUT NOCOPY      gme_pending_product_lots%ROWTYPE);

/*#
* Delete a pending product lot. Note that identification: 
* pending product lot id or a combination of keys must be provided.
* @param p_api_version Version of the API
* @param p_validation_level Validation level
* @param p_init_msg_list Message list flag - default is False
* @param p_commit Commit flag - default is False
* @param p_batch_header_rec Batch header record with optional key specified
* @param p_org_code Organization Code
* @param p_material_detail_rec Material detail record with key combination specified
* @param p_pending_product_lots_rec Pending product lot record to delete
* @param x_message_count Number of messages
* @param x_message_list List of messages
* @param x_return_status Indicates the return status of an API.
* @rep:scope public
* @rep:lifecycle active
* @rep:displayname Delete Pending Product Lot
*/

   /*================================================================================
    Procedure
      Delete_pending_product_lot
    Description
      This procedure is used to delete a pending product lot record for a specified
      pending product lot record
      Following key sequences can be specified:
      pending_product_lot_id OR
      batch_id, line_no, line_type, sequence OR
      batch_no, org_code, line_no, line_type, sequence (batch_type is assumed to be Batch)

    Parameters
      p_batch_header_rec (O)      batch header record
      p_org_code (O)              organization code
      p_material_detail_rec (O)   material detail record
      p_pending_product_lots_rec (R) pending product lots record
      x_return_status             outcome of the API call
                                  S - Success
                                  E - Error
                                  U - Unexpected Error
  ================================================================================*/

   PROCEDURE delete_pending_product_lot
     (p_api_version              IN              NUMBER 
     ,p_validation_level         IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list            IN              VARCHAR2 := fnd_api.g_false
     ,p_commit                   IN              VARCHAR2 := fnd_api.g_false
     ,x_message_count            OUT NOCOPY      NUMBER
     ,x_message_list             OUT NOCOPY      VARCHAR2
     ,x_return_status            OUT NOCOPY      VARCHAR2
     ,p_batch_header_rec         IN              gme_batch_header%ROWTYPE
     ,p_org_code                 IN              VARCHAR2
     ,p_material_detail_rec      IN              gme_material_details%ROWTYPE
     ,p_pending_product_lots_rec IN              gme_pending_product_lots%ROWTYPE);

/*#
* Unrelease a batch. Note that identification is required:
* p_batch_id or p_batch_no and p_org_code (batch_type is assumed to be batch)
* @param p_api_version Version of the API
* @param p_validation_level Validation level
* @param p_init_msg_list Message list flag - default is False
* @param p_commit Commit flag - default is False
* @param p_save_batch Call save batch api - default is False
* @param p_batch_header_rec Identifies what batch to unrelease
* @param p_org_code Organization Code
* @param p_create_resv_pend_lots Indicates to create Reservations for ingredients and Pending Product Lots for Products
* @param p_continue_lpn_txn Indicates whether to continue processing the batch when product or byproduct is yielded into LPN - default is 'N'
* @param x_batch_header_rec Batch Header output record after unrelease
* @param x_message_count Number of messages
* @param x_message_list List of messages
* @param x_return_status Indicates the return status of an API.
* @rep:scope public
* @rep:lifecycle active
* @rep:displayname Unrelease Batch
*/

   /*================================================================================
    Procedure
      unrelease_batch
    Description
      This procedure is used to unrelease a WIP batch.
      One of the following key sequences must be specified:
      batch_id OR  
      batch_no, org_code  batch_type assumed to be batch

    Parameters
      p_batch_header_rec (O)      batch header to identify the batch to unrelease
      p_org_code (O)              organization code to identigy the batch is conjunction with p_batch_no
      p_create_resv_pend_lots (R) indicates whether to create reservations or pending product lots
                                  depending on the line type.
      p_continue_lpn_txn (O)      indicates wheter to continue processing a batch when product or byproduct has lpn transaction.
      x_batch_header_rec          Output batch header record after unrelease
      x_return_status             outcome of the API call
                                  S - Success
                                  E - Error
                                  U - Unexpected Error
  ================================================================================*/
   PROCEDURE unrelease_batch
     (p_api_version              IN              NUMBER 
     ,p_validation_level         IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list            IN              VARCHAR2 := fnd_api.g_false
     ,p_commit                   IN              VARCHAR2 := fnd_api.g_false
     ,p_save_batch               IN              VARCHAR2 := fnd_api.g_false -- Bug 20448067
     ,x_message_count            OUT NOCOPY      NUMBER
     ,x_message_list             OUT NOCOPY      VARCHAR2
     ,x_return_status            OUT NOCOPY      VARCHAR2
     ,p_batch_header_rec         IN              gme_batch_header%ROWTYPE
     ,p_org_code                 IN              VARCHAR2
     ,p_create_resv_pend_lots    IN              NUMBER
     ,p_continue_lpn_txn         IN              VARCHAR2 := 'N'
     ,x_batch_header_rec         OUT NOCOPY      gme_batch_header%ROWTYPE);

/*#
* Unrelease a step. Note that identification is required:
* p_batchstep_id or p_batch_id, p_batchstep_no or p_batch_no, p_org_code
* and batchstep_no (batch_type is assumed to be batch)
* @param p_api_version Version of the API
* @param p_validation_level Validation level
* @param p_init_msg_list Message list flag - default is False
* @param p_commit Commit flag - default is False
* @param p_batch_step_rec Identifies what step to unrelease
* @param p_batch_no Batch Number
* @param p_org_code Organization Code
* @param p_create_resv_pend_lots Indicates to create Reservations for ingredients and Pending Product Lots for Products associated to the step
* @param x_batch_step_rec Batch Header output record after unrelease
* @param x_message_count Number of messages
* @param x_message_list List of messages
* @param x_return_status Indicates the return status of an API.
* @rep:scope public
* @rep:lifecycle active
* @rep:displayname Unrelease Step
*/

   /*================================================================================
    Procedure
      unrelease_step
    Description
      This procedure is used to unrelease a WIP step.
      One of the following key sequences must be specified:
      batchstep_id OR
      batch_id, batchstep_no OR  
      batch_no, org_code, batchstep_no  batch_type assumed to be batch

    Parameters
      p_batch_step_rec (O)        batch step to identify the step to unrelease.
      p_batch_no (O)              batch number to identify the step
      p_org_code (O)              organization code to identify the step
      p_create_resv_pend_lots (R) indicates whether to create reservations or pending product lots
                                  depending on the line type associated to the step.
      x_batch_step_rec            Output step record after unrelease
      x_return_status             outcome of the API call
                                  S - Success
                                  E - Error
                                  U - Unexpected Error
  ================================================================================*/
   PROCEDURE unrelease_step
     (p_api_version              IN              NUMBER 
     ,p_validation_level         IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list            IN              VARCHAR2 := fnd_api.g_false
     ,p_commit                   IN              VARCHAR2 := fnd_api.g_false
     ,x_message_count            OUT NOCOPY      NUMBER
     ,x_message_list             OUT NOCOPY      VARCHAR2
     ,x_return_status            OUT NOCOPY      VARCHAR2
     ,p_batch_step_rec           IN              gme_batch_steps%ROWTYPE
     ,p_batch_no                 IN              VARCHAR2
     ,p_org_code                 IN              VARCHAR2
     ,p_create_resv_pend_lots    IN              NUMBER
     ,x_batch_step_rec           OUT NOCOPY      gme_batch_steps%ROWTYPE);

/*#
* Complete a batch. Note that identification is required:
* batch_id or batch_no and p_org_code (batch_type is assumed to be batch)
* @param p_api_version Version of the API
* @param p_validation_level Validation level
* @param p_init_msg_list Message list flag - default is False
* @param p_commit Commit flag - default is False
* @param p_batch_header_rec Identifies what batch to complete; also contains actual dates
* @param p_org_code Organization Code
* @param p_ignore_exception Indicates whether to ignore batch exceptions.
* @param p_validate_flexfields Indicates whether to validate flexfields
* @param x_batch_header_rec Batch Header output record after complete
* @param x_exception_material_tbl Contains the exception material that was found.
* @param x_message_count Number of messages
* @param x_message_list List of messages
* @param x_return_status Indicates the return status of an API.
* @rep:scope public
* @rep:lifecycle active
* @rep:displayname Complete Batch
*/

   /*================================================================================
    Procedure
      complete_batch
    Description
      This procedure is used to complete a Pending or WIP batch.
      One of the following key sequences must be specified:
      batch_id OR  
      batch_no, org_code  batch_type assumed to be batch

    Parameters
      p_batch_header_rec (O)      batch header to identify the batch to complete;
      p_org_code (O)              organization code to identify the batch is conjunction with batch_no in p_batch_header_rec
      p_ignore_exception (O)      indicates whether to ignore exceptions; if exceptions are ignored,
                                  x_exception_material_tbl won't be populated ever if exceptions were found
      p_validate_flexfields (O)   indicates whether to validate flexfields... Defaults to fnd_api.g_false;
                                  this is used for direct completion only because of release batch
      x_batch_header_rec          Output batch header record after complete
      x_exception_material_tbl    Batch exceptions found during batch complete. Won't be reported if
                                  p_ignore_exception is set to fnd_api.g_true.
      x_return_status             outcome of the API call
                                  S - Success
                                  E - Error
                                  U - Unexpected Error
                                  X - Batch Exception
  ================================================================================*/
   PROCEDURE complete_batch
     (p_api_version              IN              NUMBER 
     ,p_validation_level         IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list            IN              VARCHAR2 := fnd_api.g_false
     ,p_commit                   IN              VARCHAR2 := fnd_api.g_false
     ,x_message_count            OUT NOCOPY      NUMBER
     ,x_message_list             OUT NOCOPY      VARCHAR2
     ,x_return_status            OUT NOCOPY      VARCHAR2
     ,p_batch_header_rec         IN              gme_batch_header%ROWTYPE
     ,p_org_code                 IN              VARCHAR2
     ,p_ignore_exception         IN              VARCHAR2 := fnd_api.g_false
     ,p_validate_flexfields      IN              VARCHAR2 := fnd_api.g_false
     ,x_batch_header_rec         OUT NOCOPY      gme_batch_header%ROWTYPE
     ,x_exception_material_tbl   OUT NOCOPY      gme_common_pvt.exceptions_tab);

/*#
* Complete a step. Note that identification is required:
* batchstep_id or batch_id, batchstep_no or batch_no, p_org_code and batchstep_no
* (batch_type is assumed to be batch)
* @param p_api_version Version of the API
* @param p_validation_level Validation level
* @param p_init_msg_list Message list flag - default is False
* @param p_commit Commit flag - default is False
* @param p_save_batch Call save batch api - default is True
* @param p_batch_step_rec Identifies what step to complete; also contains actual dates
* @param p_batch_no Batch number to identify step
* @param p_org_code Organization Code
* @param p_ignore_exception Indicates whether to ignore step exceptions.
* @param p_override_quality Indicates whether to override quality status.
* @param p_validate_flexfields Indicates whether to validate flexfields (for pending batch only)
* @param x_batch_step_rec Batch Step output record after complete
* @param x_exception_material_tbl Contains the exception material that was found.
* @param x_message_count Number of messages
* @param x_message_list List of messages
* @param x_return_status Indicates the return status of an API.
* @rep:scope public
* @rep:lifecycle active
* @rep:displayname Complete Step
*/

   /*================================================================================
    Procedure
      complete_step
    Description
      This procedure is used to complete a step
      One of the following key sequences must be specified:
      batchstep_id
      batch_id, batchstep_no OR
      batch_no, org_code, batchstep_no batch_type assumed to be batch

    Parameters
      p_batch_step_rec (O)        batch step to identify the step to complete.
      p_batch_no (O)              batch_no to identify the step
      p_org_code (O)              organization code to identify the step is conjunction with batch_no in p_batch_header_rec
      p_ignore_exception (O)      indicates whether to ignore exceptions; if exceptions are ignored,
                                  x_exception_material_tbl won't be populated ever if exceptions were found
      p_override_quality (O)      Override quality indicator; defaults to fnd_api.g_false
      p_validate_flexfields (O)   indicates whether to validate flexfields... Defaults to fnd_api.g_false;
                                  this is used for direct step completion (pending_batch) only, because of release batch
      x_batch_step_rec            Output batch step record after complete
      x_exception_material_tbl    Material exceptions found in complete step (only those associated to step)
      x_return_status             outcome of the API call
                                  S - Success
                                  E - Error
                                  U - Unexpected Error
                                  X - Batch Exception
  ================================================================================*/
   PROCEDURE complete_step
     (p_api_version              IN              NUMBER 
     ,p_validation_level         IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list            IN              VARCHAR2 := fnd_api.g_false
     ,p_commit                   IN              VARCHAR2 := fnd_api.g_false
     ,p_save_batch               IN              VARCHAR2 := fnd_api.g_true  -- Bug 16079623
     ,x_message_count            OUT NOCOPY      NUMBER
     ,x_message_list             OUT NOCOPY      VARCHAR2
     ,x_return_status            OUT NOCOPY      VARCHAR2
     ,p_batch_step_rec           IN              gme_batch_steps%ROWTYPE
     ,p_batch_no                 IN              VARCHAR2
     ,p_org_code                 IN              VARCHAR2
     ,p_ignore_exception         IN              VARCHAR2 := fnd_api.g_false
     ,p_override_quality         IN              VARCHAR2 := fnd_api.g_false
     ,p_validate_flexfields      IN              VARCHAR2 := fnd_api.g_false
     ,x_batch_step_rec           OUT NOCOPY      gme_batch_steps%ROWTYPE
     ,x_exception_material_tbl   OUT NOCOPY      gme_common_pvt.exceptions_tab);

/*#
* Release a batch. Note that identification is required:
* batch_id or batch_no and p_org_code (batch_type is assumed to be batch)
* @param p_api_version Version of the API
* @param p_validation_level Validation level
* @param p_init_msg_list Message list flag - default is False
* @param p_commit Commit flag - default is False
* @param p_batch_header_rec Identifies what batch to release; also contains actual start date
* @param p_org_code Organization Code
* @param p_ignore_exception Indicates whether to ignore batch exceptions.
* @param p_validate_flexfields Indicates whether to validate flexfields
* @param x_batch_header_rec Batch Header output record after release
* @param x_exception_material_tbl Contains the exception material that was found.
* @param x_message_count Number of messages
* @param x_message_list List of messages
* @param x_return_status Indicates the return status of an API.
* @rep:scope public
* @rep:lifecycle active
* @rep:displayname Release Batch
*/
     
   /*================================================================================
    Procedure
      release_batch
    Description
      This procedure is used to release a batch
      One of the following key sequences must be specified:
      batch_id OR
      batch_no, org_code, batch_type assumed to be batch

    Parameters
      p_batch_header_rec (O)      batch header to identify the batch to release;
      p_org_code (O)              organization code to identify the batch is conjunction with batch_no in p_batch_header_rec
      p_ignore_exception (O)      indicates whether to ignore exceptions; if exceptions are ignored,
                                  x_exception_material_tbl won't be populated even if exceptions were found
      p_validate_flexfields (O)   indicates whether to validate flexfields... Defaults to fnd_api.g_false;
      x_batch_header_rec          Output batch header record after release
      x_exception_material_tbl    Batch exceptions found in release batch
      x_return_status             outcome of the API call
                                  S - Success
                                  E - Error
                                  U - Unexpected Error
                                  X - Batch Exception
  ================================================================================*/
   PROCEDURE release_batch
     (p_api_version              IN              NUMBER 
     ,p_validation_level         IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list            IN              VARCHAR2 := fnd_api.g_false
     ,p_commit                   IN              VARCHAR2 := fnd_api.g_false
     ,x_message_count            OUT NOCOPY      NUMBER
     ,x_message_list             OUT NOCOPY      VARCHAR2
     ,x_return_status            OUT NOCOPY      VARCHAR2
     ,p_batch_header_rec         IN              gme_batch_header%ROWTYPE
     ,p_org_code                 IN              VARCHAR2
     ,p_ignore_exception         IN              VARCHAR2 := fnd_api.g_false
     ,p_validate_flexfields      IN              VARCHAR2 := fnd_api.g_false
     ,x_batch_header_rec         OUT NOCOPY      gme_batch_header%ROWTYPE
     ,x_exception_material_tbl   OUT NOCOPY      gme_common_pvt.exceptions_tab);

/*#
* Release a step. Note that identification is required:
* batchstep_id or batch_id, batchstep_no or batch_no, p_org_code and batchstep_no
* (batch_type is assumed to be batch)
* @param p_api_version Version of the API
* @param p_validation_level Validation level
* @param p_init_msg_list Message list flag - default is False
* @param p_commit Commit flag - default is False
* @param p_save_batch Call save batch api - default is True
* @param p_batch_step_rec Identifies what step to release; also contains actual start date
* @param p_batch_no Batch number to identify step
* @param p_org_code Organization Code
* @param p_ignore_exception Indicates whether to ignore step exceptions.
* @param p_validate_flexfields Indicates whether to validate flexfields (for step or batch (when pending))
* @param x_batch_step_rec Batch Step output record after release
* @param x_exception_material_tbl Contains the exception material that was found.
* @param x_message_count Number of messages
* @param x_message_list List of messages
* @param x_return_status Indicates the return status of an API.
* @rep:scope public
* @rep:lifecycle active
* @rep:displayname Release Step
*/

   /*================================================================================
    Procedure
      release_step
    Description
      This procedure is used to release a step
      One of the following key sequences must be specified:
      batchstep_id
      batch_id, batchstep_no OR
      batch_no, org_code, batchstep_no batch_type assumed to be batch

    Parameters
      p_batch step_rec (O)        batch step to identify the step to release.
      p_batch_no (O)              batch_no to identify the step
      p_org_code (O)              organization code to identify the step is conjunction with batch_no in p_batch_header_rec
      p_ignore_exception (O)      indicates whether to ignore exceptions; if exceptions are ignored,
                                  x_exception_material_tbl won't be populated ever if exceptions were found
      p_validate_flexfields (O)   indicates whether to validate flexfields... Defaults to fnd_api.g_false;
      x_batch_step_rec            Output batch step record after release
      x_exception_material_tbl    Material exceptions found in release step (only those associated to step and processed dep steps)
      x_return_status             outcome of the API call
                                  S - Success
                                  E - Error
                                  U - Unexpected Error
                                  X - Batch Exception
  ================================================================================*/
   PROCEDURE release_step
     (p_api_version              IN              NUMBER 
     ,p_validation_level         IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list            IN              VARCHAR2 := fnd_api.g_false
     ,p_commit                   IN              VARCHAR2 := fnd_api.g_false
     ,p_save_batch               IN              VARCHAR2 := fnd_api.g_true  -- Bug 16079623
     ,x_message_count            OUT NOCOPY      NUMBER
     ,x_message_list             OUT NOCOPY      VARCHAR2
     ,x_return_status            OUT NOCOPY      VARCHAR2
     ,p_batch_step_rec           IN              gme_batch_steps%ROWTYPE
     ,p_batch_no                 IN              VARCHAR2
     ,p_org_code                 IN              VARCHAR2
     ,p_ignore_exception         IN              VARCHAR2 := fnd_api.g_false
     ,p_validate_flexfields      IN              VARCHAR2 := fnd_api.g_false
     ,x_batch_step_rec           OUT NOCOPY      gme_batch_steps%ROWTYPE
     ,x_exception_material_tbl   OUT NOCOPY      gme_common_pvt.exceptions_tab);

/*#
* Process a Group. 
* This API is used to perform mass batch actions on a group of batches. 
* Required parameters are - group_name, org_code, action and on_error_flag.
* group_name and org_code.
* @param p_api_version Version of the API
* @param p_validation_level Validation level
* @param p_init_msg_list Message list flag - default is False
* @param p_commit Commit flag - default is False
* @param p_group_name Group name. 
* @param p_org_code Organization code
* @param p_action Identifies what batch action to perform. Valid values from lookup type GME_BATCH_GROUP_ACTION (3,4,5,6,7,8,9,10,11)
* @param p_on_error_flag Indicates whether to continue or stop processing in case of an error. Valid values are 'STOP' OR 'CONTINUE'
* @param x_message_count Number of messages
* @param x_message_list List of messages
* @param x_return_status Indicates the return status of an API.
* @rep:scope public
* @rep:lifecycle active
* @rep:displayname Process Group
*/
     
   /*================================================================================
    Procedure
      process_group
    Description
      This procedure is used to perform mass batch actions on group of batches. 
      Required parameters are:
      group_name, org_code, action and on_error_flag.

    Parameters
      p_group_name                Group name to identify the group of batches to process;
                                  Set of batches can grouped under a group name which can be created 
                                  from the batch group management form. 
      p_org_code                  organization code to identify the group_name is conjunction with group_name in p_batch_header_rec
      p_action                    Identifies what batch action to perform
                                  Valid values from lookup type GME_BATCH_GROUP_ACTION (3,4,5,6,7,8,9,10,11)
      p_on_error_flag             Indicates whether to continue or stop processing in case of an error. 
                                  Valid values are 'STOP' OR 'CONTINUE'
      x_return_status             outcome of the API call
                                  S - Success
                                  E - Error
                                  U - Unexpected Error
                                  X - Batch Exception
  ================================================================================*/
   PROCEDURE process_group
     (p_api_version              IN              NUMBER 
     ,p_validation_level         IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list            IN              VARCHAR2 := fnd_api.g_false
     ,p_commit                   IN              VARCHAR2 := fnd_api.g_false
     ,p_group_name               IN              VARCHAR2
     ,p_org_code                 IN              VARCHAR2
     ,p_action                   IN              NUMBER
     ,p_on_error_flag            IN              VARCHAR2
     ,x_message_count            OUT NOCOPY      NUMBER
     ,x_message_list             OUT NOCOPY      VARCHAR2
     ,x_return_status            OUT NOCOPY      VARCHAR2
     );
     
   /*================================================================================
    Procedure
      update_batchstep_qty
    Description
      This api procedure is used only for updating actual_step_qty.
      Charges will also be recalculated.
      One of the following key sequences must be specified:
      batchstep_id
      batch_id, batchstep_no OR
      batch_no, org_code, batchstep_no batch_type assumed to be batch

    Parameters
      p_batch step_rec (O)        batch step to identify the step to update.
      p_org_code (O)              organization code to identify the step is conjunction with batch_no in p_batch_header_rec
      p_batch_no (O)              batch_no to identify the step
      p_add (O)                   indicates whether to add to the actual step qty for incremental 
                                  recording or overwrite. Default is 'N' Meaning overwrite.
      x_batch_step_rec            Output batch step record after update
      x_return_status             outcome of the API call
                                  S - Success
                                  E - Error
                                  U - Unexpected Error
                                  X - Batch Exception

    History:
      G. Muratore    15-JUN-2009  Bug 7447757      
         Introduced new api for update actual_step_qty
  ================================================================================*/
   PROCEDURE update_batchstep_qty (
      p_api_version         IN              NUMBER   := 1.0
     ,p_validation_level    IN              NUMBER   := gme_common_pvt.g_max_errors
     ,p_init_msg_list       IN              VARCHAR2 := fnd_api.g_false
     ,p_commit              IN              VARCHAR2
     ,p_org_code            IN              VARCHAR2
     ,p_batch_no            IN              VARCHAR2 := NULL
     ,p_add                 IN              VARCHAR2 := 'N'     
     ,p_batch_step_rec      IN              gme_batch_steps%ROWTYPE
     ,x_batch_step_rec      OUT NOCOPY      gme_batch_steps%ROWTYPE
     ,x_message_count       OUT NOCOPY      NUMBER
     ,x_message_list        OUT NOCOPY      VARCHAR2
     ,x_return_status       OUT NOCOPY      VARCHAR2);

   /*================================================================================
    Procedure
      reset_txn_header_id
    Description
      This api procedure is used for resetting gme_common_pvt.g_transaction_header_id.

    Parameters

    History:
      G. Muratore    06-DEC-2012  Bug 14685438      
         Introduced new api to reset g_transaction_header_id.
  ================================================================================*/
   PROCEDURE reset_txn_header_id;
   



END gme_api_pub;

/
COMMIT ;
EXIT;
