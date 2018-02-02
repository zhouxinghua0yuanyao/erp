/* +======================================================================+ */
/* |    Copyright (c) 2005, 2017 Oracle and/or its affiliates.            | */
/* |                         All rights reserved.                         | */
/* |                           Version 12.0.0                             | */
/* +======================================================================+ */
REM dbdrv: sql ~PROD ~PATH ~FILE none none none package &phase=pls \
REM dbdrv: checkfile(120.11.12010000.8=120.13.12020000.6)(120.10.12000000.5=120.11.12010000.3)(120.10.12000000.4=120.11.12010000.2)(115.15=120.4):~PROD:~PATH:~FILE
SET VERIFY OFF
WHENEVER SQLERROR EXIT FAILURE ROLLBACK;
WHENEVER OSERROR EXIT FAILURE ROLLBACK;
REM **********************************************************************
REM *                                                                    *
REM * FILE:    GMEMAPIS.pls                                              *
REM * PURPOSE: Package Specification for the GME MAIN API routines       *
REM * AUTHOR:  Chandrashekar Tiruvidula, OPM Development                 *
REM * DATE:    August 2nd 2002                                           *
REM * HISTORY:
REM*  05-MAY-2006 SivakumarG Bug#5186328                                 *
REM *  New parameter p_ignore_exception added to  release_batch/step and *
REM *  complete_batch/step.                                              *
REM *  11-JAN-2007 Swapna K Bug#6738476 Added parameter,p_batch_header_rec*
REM *   to the procedure,create_phantom                                  *
REM *
REM *  18-NOV-2008   G. Muratore    Bug 7565054 
REM *     Added parameter p_sum_all_prod_lines to the procedure create_batch
REM *
REM *  27-OCT-2011   Archana Mundhe Bug 13070352                                                                                   
REM *  Modified procedure delete_material_line. Added parameter p_bypass_gmf 

REM *  05-APR-2016   G. Muratore    Bug 22317010 / 22764488
REM *   Introduced new p_create_resv_pend_lots and p_tolerance parameter.
REM *   Introduced new p_lpn_id parameter for future use. Bugs 13628717/12884831.
REM *   PROCEDURE incremental_backflush

REM *  03-NOV-2016   G. Muratore    Bug 23640627 
REM *   Introduced new p_create_resv_pend_lots parameter.
REM *   PROCEDURE: revert_batch and revert_step.
REM **********************************************************************

/*************************************************************************
* This file contains the headers for the Process Execution (GME) APIs in *
* Oracle Process Manufacturing (OPM). Each procedure has a common set of *
* parameters to which API-specific parameters are appended.              *
*************************************************************************/

CREATE OR REPLACE PACKAGE gme_api_main AS
/* $Header: GMEMAPIS.pls 120.13.12020000.6 2017/02/15 03:24:22 shalchen ship $ */

   /*================================================================================
     Procedure
       create_batch
     Description
       This procedure creates batch, then check for Items failing allocation and
            inventory shortages.
     Parameters
       p_batch_header (R)        The batch header row to identify the batch
                                 Following columns are used from this row.
                                 plant_code  (R)
                                 recipe_validity_rule_id (R)
                                 batch_type (R)
                                 update_inventory_ind (R)
                                 batch_no (R In case of manual document ordering)
                                 plan_start_date (O)
                                 plan_cmplt_date (O)
                                 due_date (O)
                                 wip_whse_code (O)
       p_batch_size (R)          Batch Size (Total input, total output or product quantity)
       p_batch_size_uom (R)      UOM for p_batch_size
       p_ignore_shortages (R)    Do not check for the inventory shortages
       p_use_shop_cal            NUMBER(0,1) - to use shop calendar or not.
       p_contiguity_override     NUMBER(0,1) - for contigious period of calendar.
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
       p_product_id (O)          Item ID for which the batch is to be created.
       p_ignore_qty_below_cap (O)Whether the batch is to be created or not, when resource
                                 quantity goes below minimum capacity of the resource.
                                 True  (Default)
                                 False
       p_use_least_cost_validity_rule    VARCHAR2('F','T') - for least cost batch validity rule.                                 

       x_batch_header            The batch header that is returned, with all the data
       x_unallocated_material    Table of materials, if auto allocation failed or
                                 inventory shortage exists
       x_return_status           outcome of the API call
                                 S - Success
                                 E - Error
                                 U - Unexpected Error
                                 N - Items failed auto allocation
                                 V - Inventory shortage exists
   ================================================================================*/
   PROCEDURE create_batch (
      p_validation_level         IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list            IN              VARCHAR2 := fnd_api.g_false
     ,x_message_count            OUT NOCOPY      NUMBER
     ,x_message_list             OUT NOCOPY      VARCHAR2
     ,x_return_status            OUT NOCOPY      VARCHAR2
     ,p_batch_header_rec         IN              gme_batch_header%ROWTYPE
     ,x_batch_header_rec         OUT NOCOPY      gme_batch_header%ROWTYPE
     ,p_batch_size               IN              NUMBER
     ,p_batch_size_uom           IN              VARCHAR2
     ,p_creation_mode            IN              VARCHAR2
     ,p_recipe_id                IN              NUMBER := NULL
     ,p_recipe_no                IN              VARCHAR2 := NULL
     ,p_recipe_version           IN              NUMBER := NULL
     ,p_product_no               IN              VARCHAR2 := NULL
     ,p_product_id               IN              NUMBER := NULL
     ,p_sum_all_prod_lines       IN              VARCHAR2 := 'A'
     ,p_ignore_qty_below_cap     IN              VARCHAR2 := fnd_api.g_true
     ,p_use_workday_cal          IN              VARCHAR2 := fnd_api.g_true
     ,p_contiguity_override      IN              VARCHAR2 := fnd_api.g_false
     ,p_use_least_cost_validity_rule     IN      VARCHAR2 := fnd_api.g_false          
     ,x_exception_material_tbl   OUT NOCOPY      gme_common_pvt.exceptions_tab);

    /*================================================================================
     Procedure
       create_phantom
     Description
       This procedure creates phantom batch based on the validity rule passsed

     Parameters
       p_material_details (R)    The material detail row to identify the material
                                 Following columns are used from this row.
                                 material_detail_id  (R)
       p_batch_no         (O)    Batch no, in case of manual document ordering
       p_validity_rule_id (R)    validity rule to use for creating phantom batch
       p_ignore_shortages (R)    Do not check for the inventory shortages
       p_use_shop_cal            NUMBER(0,1) - to use shop calendar or not.
       p_contiguity_override     NUMBER(0,1) - for contigious period of calendar.
       p_use_least_cost_validity_rule    VARCHAR2('F','T') - for least cost batch validity rule.                                        
       x_material_details        The material detail that is returned, with all the data
       x_unallocated_material    Table of materials, if auto allocation failed or
                                 inventory shortage exists
       x_return_status           outcome of the API call
                                 S - Success
                                 E - Error
                                 U - Unexpected Error
   ================================================================================*/
   PROCEDURE create_phantom (
      p_validation_level         IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list            IN              VARCHAR2 := fnd_api.g_false
     ,x_message_count            OUT NOCOPY      NUMBER
     ,x_message_list             OUT NOCOPY      VARCHAR2
     ,x_return_status            OUT NOCOPY      VARCHAR2
     ,p_material_detail_rec      IN              gme_material_details%ROWTYPE
     ,p_batch_header_rec         IN              gme_batch_header%ROWTYPE --Bug#6738476
     ,p_batch_no                 IN              VARCHAR2 DEFAULT NULL
     ,x_material_detail_rec      OUT NOCOPY      gme_material_details%ROWTYPE
     ,p_validity_rule_id         IN              NUMBER
     ,p_use_workday_cal          IN              VARCHAR2 := fnd_api.g_true
     ,p_contiguity_override      IN              VARCHAR2 := fnd_api.g_true
     ,p_use_least_cost_validity_rule     IN      VARCHAR2 := fnd_api.g_false          
     ,x_exception_material_tbl   OUT NOCOPY      gme_common_pvt.exceptions_tab);

    /*================================================================================
     Procedure
       scale_batch
     Description
       This procedure scaless batch up or down and all the phantom batches.

     Parameters
       p_batch_header (R)        The batch header row to identify the batch
                                 Following columns are used from this row.
                                 batch_id  (R)
       p_scale_factor (R)        How much to scale. (scale multiplier;
                                 to make the twice as much quantity,
                                 scale factor = 2; to reduce quantity to
                                 half scale factor = -0.5.
       p_primaries (R)           Scaling based on product or Ingredients
                                 INPUT  - Ingredients
                                 OUTPUT - Products
       p_qty_type (O)            Whether to use formula quantities or batch quantities
                                 0 - Formula
                                 1 - Batch   (Default)

       x_batch_header            The batch header that is returned, with all the data
       x_return_status           outcome of the API call
                                 S - Success
                                 E - Error
                                 U - Unexpected Error
                                 G - Over allocation exists
   ================================================================================*/
   -- Navin Modified this APIas per GME_Scale_Batch_Theoretical_Yield_TD.
   PROCEDURE scale_batch (
      p_validation_level         IN              NUMBER
     ,p_init_msg_list            IN              VARCHAR2
     ,p_batch_header_rec         IN              gme_batch_header%ROWTYPE
     ,p_scale_factor             IN              NUMBER
     ,p_primaries                IN              VARCHAR2
     ,p_qty_type                 IN              NUMBER
     ,p_recalc_dates             IN              VARCHAR2            -- Navin
     ,p_use_workday_cal          IN              VARCHAR2
     ,p_contiguity_override      IN              VARCHAR2
     ,x_exception_material_tbl   OUT NOCOPY      gme_common_pvt.exceptions_tab
                                                                      --Navin
     ,x_batch_header_rec         OUT NOCOPY      gme_batch_header%ROWTYPE
     ,x_message_count            OUT NOCOPY      NUMBER
     ,x_message_list             OUT NOCOPY      VARCHAR2
     ,x_return_status            OUT NOCOPY      VARCHAR2);

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
       p_scale_factor (R)        Theoretical yield in fractions

       x_batch_header            The batch header that is returned, with all the data
       x_return_status           outcome of the API call
                                 S - Success
                                 E - Error
                                 U - Unexpected Error
   ================================================================================*/
   PROCEDURE theoretical_yield_batch (
      p_validation_level   IN              NUMBER
     ,p_init_msg_list      IN              VARCHAR2
     ,p_batch_header_rec   IN              gme_batch_header%ROWTYPE
     ,p_scale_factor       IN              NUMBER
     ,x_message_count      OUT NOCOPY      NUMBER
     ,x_message_list       OUT NOCOPY      VARCHAR2
     ,x_return_status      OUT NOCOPY      VARCHAR2);

    /*================================================================================
     Procedure
       insert_material_line
     Description
       This procedure is used to insert a material line to a batch.

     Parameters
       p_batch_header_rec (R)        The batch header record to which the material will be inserted
       p_material_detail_rec (R)     The material detail record to insert the material line.
       p_batch_step_rec (O)          The batch step record to which the material line will be associated.
       p_trans_id                    The return from open_actual_qty; should be passed from form; NULL from pub
       
       x_transacted                  Indicates whether transactions were created as a result of inserting matl
                                     FND_API.g_true - Transactions were created
                                     FND_API.g_true - Transactions were not created
       x_material_detail_rec         The material detail record with all the data assigned
       x_return_status               outcome of the API call
                                     S - Success
                                     E - Error
                                     U - Unexpected Error
   ================================================================================*/

   PROCEDURE insert_material_line (
      p_validation_level      IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list         IN              VARCHAR2 := fnd_api.g_false
     ,x_message_count         OUT NOCOPY      NUMBER
     ,x_message_list          OUT NOCOPY      VARCHAR2
     ,x_return_status         OUT NOCOPY      VARCHAR2
     ,p_batch_header_rec      IN              gme_batch_header%ROWTYPE
     ,p_material_detail_rec   IN              gme_material_details%ROWTYPE
     ,p_batch_step_rec        IN              gme_batch_steps%ROWTYPE
     ,p_trans_id              IN              NUMBER
     ,x_transacted            OUT NOCOPY      VARCHAR2
     ,x_material_detail_rec   OUT NOCOPY      gme_material_details%ROWTYPE);

   -- following called from form... streamlined: step record not required
   PROCEDURE insert_material_line (
      p_validation_level      IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list         IN              VARCHAR2 := fnd_api.g_false
     ,x_message_count         OUT NOCOPY      NUMBER
     ,x_message_list          OUT NOCOPY      VARCHAR2
     ,x_return_status         OUT NOCOPY      VARCHAR2
     ,p_batch_header_rec      IN              gme_batch_header%ROWTYPE
     ,p_material_detail_rec   IN              gme_material_details%ROWTYPE
     ,p_batch_step_id         IN              NUMBER := NULL
     ,p_trans_id              IN              NUMBER
     ,x_transacted            OUT NOCOPY      VARCHAR2
     ,x_material_detail_rec   OUT NOCOPY      gme_material_details%ROWTYPE);

   /*================================================================================
      Procedure
        update_material_line
      Description
        This procedure is used to update a material line in a batch.

      Parameters
        p_batch_header_rec (R)            The batch header record to which this material belongs
        p_material_detail_rec (R)         The material detail record with the values to update.
        p_stored_material_detail_rec (R)  The material detail record as it is in the DB.
        p_batch_step_rec (O/R)            If the material is associated to a step, this is the assoc. step
                                          record.  If the material is associated to a step,
                                          this parm is required.
        p_scale_phantom (O)               If the plan/wip plan quantity has been updated,
                                          and the material is a phantom ingredient,
                                          this indicates whether the phantom batch should be scaled.
        p_trans_id                        The return from open_actual_qty; should be passed from form; NULL from pub
        
        x_transacted                      Indicates whether transactions were created as a result of inserting matl
                                          FND_API.g_true - Transactions were created
                                          FND_API.g_true - Transactions were not created
        x_material_detail_rec             The updated material detail record.
        x_return_status                   outcome of the API call
                                          S - Success
                                          E - Error
                                          U - Unexpected Error
    ================================================================================*/

    PROCEDURE update_material_line (
      p_validation_level             IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list                IN              VARCHAR2
            := fnd_api.g_false
     ,x_message_count                OUT NOCOPY      NUMBER
     ,x_message_list                 OUT NOCOPY      VARCHAR2
     ,x_return_status                OUT NOCOPY      VARCHAR2
     ,p_batch_header_rec             IN              gme_batch_header%ROWTYPE
     ,p_material_detail_rec          IN              gme_material_details%ROWTYPE
     ,p_stored_material_detail_rec   IN              gme_material_details%ROWTYPE
     ,p_batch_step_rec               IN              gme_batch_steps%ROWTYPE
     ,p_scale_phantom                IN              VARCHAR2
            := fnd_api.g_false
     ,p_trans_id                     IN              NUMBER
     ,x_transacted                   OUT NOCOPY      VARCHAR2
     ,x_material_detail_rec          OUT NOCOPY      gme_material_details%ROWTYPE);
     
    -- following called from form... streamlined: step record and stored matl dtl record not required
    PROCEDURE update_material_line (
      p_validation_level             IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list                IN              VARCHAR2
            := fnd_api.g_false
     ,x_message_count                OUT NOCOPY      NUMBER
     ,x_message_list                 OUT NOCOPY      VARCHAR2
     ,x_return_status                OUT NOCOPY      VARCHAR2
     ,p_batch_header_rec             IN              gme_batch_header%ROWTYPE
     ,p_material_detail_rec          IN              gme_material_details%ROWTYPE
     ,p_batch_step_id                IN              NUMBER DEFAULT NULL
     ,p_scale_phantom                IN              VARCHAR2 := fnd_api.g_false
     ,p_trans_id                     IN              NUMBER
     ,x_transacted                   OUT NOCOPY      VARCHAR2
     ,x_material_detail_rec          OUT NOCOPY      gme_material_details%ROWTYPE);

    /*================================================================================
     Procedure
       delete_material_line
     Description
       This procedure is used to delete a material line in a batch.

     Parameters
       p_batch_header_rec (R)      The batch header record to which the material belongs.
       p_material_detail_rec (R)   The material detail record to be deleted
       p_batch_step_rec (O/R)      If the material is associated to a step, this is the assoc. step
                                   record.  If the material is associated to a step, this parm is required.
       x_transacted                Indicates whether transactions were created as a result of inserting matl
                                   FND_API.g_true - Transactions were created
                                   FND_API.g_true - Transactions were not created
       x_return_status             outcome of the API call
                                   S - Success
                                   E - Error
                                   U - Unexpected Error
   ================================================================================*/


   PROCEDURE delete_material_line (
      p_validation_level      IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list         IN              VARCHAR2 := fnd_api.g_false
     ,x_message_count         OUT NOCOPY      NUMBER
     ,x_message_list          OUT NOCOPY      VARCHAR2
     ,x_return_status         OUT NOCOPY      VARCHAR2
     ,p_batch_header_rec      IN              gme_batch_header%ROWTYPE
     ,p_material_detail_rec   IN              gme_material_details%ROWTYPE
     ,p_batch_step_rec        IN              gme_batch_steps%ROWTYPE
     ,x_transacted            OUT NOCOPY      VARCHAR2);
     
   -- following called from form... streamlined: step record not required
   PROCEDURE delete_material_line (
      p_validation_level      IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list         IN              VARCHAR2 := fnd_api.g_false
     ,x_message_count         OUT NOCOPY      NUMBER
     ,x_message_list          OUT NOCOPY      VARCHAR2
     ,x_return_status         OUT NOCOPY      VARCHAR2
     ,p_batch_header_rec      IN              gme_batch_header%ROWTYPE
     ,p_material_detail_rec   IN              gme_material_details%ROWTYPE
     ,p_batch_step_id         IN              NUMBER := NULL
     ,p_bypass_gmf            IN              VARCHAR2 := 'N' -- Bug 13070352 
     ,x_transacted            OUT NOCOPY      VARCHAR2);

    /*================================================================================
     Procedure
       reschedule_batch
     Description
       This procedure reschedules batch and all the phantom batches.
        It also reschedules all the steps, if requested so.

     Parameters
       p_batch_header (R)        The batch header row to identify the batch
                                 Following columns are used from this row.
                                 batch_id  (R)
                                 plan_start_date (R)
                                 plan_cmplt_date (R)
       p_use_shop_cal            NUMBER(0,1) - to use shop calendar or not.
       p_contiguity_override     NUMBER(0,1) - for contigious period of calendar.
       x_batch_header            The batch header that is returned, with all the data
       x_return_status           outcome of the API call
                                 S - Success
                                 E - Error
                                 U - Unexpected Error
   ================================================================================*/
   PROCEDURE reschedule_batch (
      p_validation_level      IN              NUMBER
     ,p_init_msg_list         IN              VARCHAR2
     ,p_batch_header_rec      IN              gme_batch_header%ROWTYPE
     ,p_use_workday_cal       IN              VARCHAR2
     ,p_contiguity_override   IN              VARCHAR2
     ,x_message_count         OUT NOCOPY      NUMBER
     ,x_message_list          OUT NOCOPY      VARCHAR2
     ,x_return_status         OUT NOCOPY      VARCHAR2
     ,x_batch_header_rec      OUT NOCOPY      gme_batch_header%ROWTYPE);

    /*=========================================9======================================
     Procedure
       reschedule_step
     Description
       This procedure reschedules step and all the subsequent steps, if requested so.

     Parameters
       p_batch_step (R)          The batch step row to identify the step
                                 Following columns are used from this row.
                                 batchstep_id  (R)
                                 plan_start_date (R)
                                 plan_cmplt_date (R)
       p_batch_header (R)        The batch header row to identify the batch
       p_reschedule_other (O)    Whether to reschedule subsequent steps.
       p_use_shop_cal            NUMBER(0,1) - to use shop calendar or not.
       p_contiguity_override     NUMBER(0,1) - for contigious period of calendar.
       x_batch_step              The batch step that is returned, with all the data
       x_return_status           outcome of the API call
                                 S - Success
                                 E - Error
                                 U - Unexpected Error
   ================================================================================*/
   PROCEDURE reschedule_step (
      p_validation_level        IN              NUMBER
     ,p_init_msg_list           IN              VARCHAR2
     ,p_batch_header_rec        IN              gme_batch_header%ROWTYPE
     ,p_batch_step_rec          IN              gme_batch_steps%ROWTYPE
     ,p_reschedule_preceding    IN              VARCHAR2
     ,p_reschedule_succeeding   IN              VARCHAR2
     ,p_use_workday_cal         IN              VARCHAR2
     ,p_contiguity_override     IN              VARCHAR2
     ,x_message_count           OUT NOCOPY      NUMBER
     ,x_message_list            OUT NOCOPY      VARCHAR2
     ,x_return_status           OUT NOCOPY      VARCHAR2
     ,x_batch_step_rec          OUT NOCOPY      gme_batch_steps%ROWTYPE);

    /*================================================================================
     Procedure
       create_batch_reservations
     Description
       This procedure creates high level reservations (at organization level) for ingredient
       lines within a batch.
     Parameters
       p_init_msg_list    (O)    Instructs TRUE or FALSE (T/F) on whether the message stack
                                 should be initialized
       p_batch_header_rec (R)    The batch header row to identify the batch
                                 against which high level reservations will
                                 be created
                                 Following columns are used from this row.
                                 organization_id  (R)
                                 batch_id (R)
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
      p_init_msg_list      IN              VARCHAR2 := fnd_api.g_false
     ,p_batch_header_rec   IN              gme_batch_header%ROWTYPE
     ,x_message_count      OUT NOCOPY      NUMBER
     ,x_message_list       OUT NOCOPY      VARCHAR2
     ,x_return_status      OUT NOCOPY      VARCHAR2);

    /*================================================================================
     Procedure
       create_line_reservations
     Description
       This procedure creates high level reservations (at organization level) for the input
       batch ingredient line.
     Parameters
       p_init_msg_list    (O)    Instructs TRUE or FALSE (T/F) on whether the message stack
                                 should be initialized
       p_matl_dtl_rec     (R)    The material detail row identifying the ingredient line
                                 within a batch against which a high level reservation will
                                 be created
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
      p_init_msg_list   IN              VARCHAR2 := fnd_api.g_false
     ,p_matl_dtl_rec    IN              gme_material_details%ROWTYPE
     ,x_message_count   OUT NOCOPY      NUMBER
     ,x_message_list    OUT NOCOPY      VARCHAR2
     ,x_return_status   OUT NOCOPY      VARCHAR2);

    /*================================================================================
     Procedure
       release_batch
     Description
       This procedure is used to release a batch.

     Parameters
       p_batch_header_rec (R)      The batch header record to release.
       x_batch_header_rec          The updated batch header record in WIP state.
       x_exception_material_tbl    A table containing all the exception materials found during
                                   the release process and an indication of why they are
                                   considered an.exception.
       x_return_status             outcome of the API call
                                   S - Success
                                   E - Error
                                   U - Unexpected Error
                                   X - Exception
   ================================================================================*/
   PROCEDURE release_batch (
      p_validation_level         IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list            IN              VARCHAR2 := fnd_api.g_false
     ,x_message_count            OUT NOCOPY      NUMBER
     ,x_message_list             OUT NOCOPY      VARCHAR2
     ,x_return_status            OUT NOCOPY      VARCHAR2
     ,p_batch_header_rec         IN              gme_batch_header%ROWTYPE
     ,x_batch_header_rec         OUT NOCOPY      gme_batch_header%ROWTYPE
     ,x_exception_material_tbl   OUT NOCOPY      gme_common_pvt.exceptions_tab
     ,p_ignore_exception         IN              VARCHAR2 := NULL);  --Bug#5186328

    /*================================================================================
     Procedure
       release_step
     Description
       This procedure is used to release a step.

     Parameters
       p_batch_step_rec (R)        The batch step record to release.
       p_batch_header_rec (R)      The batch header record the step belongs to.
       x_batch_step_rec            The updated batch step record in WIP state.
       x_exception_material_tbl    A table containing all the exception materials found during
                                   the release process and an indication of why they are
                                   considered an.exception.

       x_return_status             outcome of the API call
                                   S - Success
                                   E - Error
                                   U - Unexpected Error
                                   X - Exception
   ================================================================================*/
   PROCEDURE release_step (
      p_validation_level         IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list            IN              VARCHAR2 := fnd_api.g_false
     ,x_message_count            OUT NOCOPY      NUMBER
     ,x_message_list             OUT NOCOPY      VARCHAR2
     ,x_return_status            OUT NOCOPY      VARCHAR2
     ,p_batch_step_rec           IN              gme_batch_steps%ROWTYPE
     ,p_batch_header_rec         IN              gme_batch_header%ROWTYPE
     ,x_batch_step_rec           OUT NOCOPY      gme_batch_steps%ROWTYPE
     ,x_exception_material_tbl   OUT NOCOPY      gme_common_pvt.exceptions_tab
     ,p_ignore_exception         IN              VARCHAR2 := NULL); --Bug#5186328

   /*================================================================================
     Procedure
       complete_batch
     Description
       This procedure is used to complete a batch.

     Parameters
       p_batch_header_rec (R)      The batch header record to complete.
       x_batch_header_rec          The updated batch header record in Complete state.
       x_exception_material_tbl    A table containing all the exception materials found during
                                   the complete process and an indication of why they are
                                   considered an.exception.
       x_return_status             outcome of the API call
                                   S - Success
                                   E - Error
                                   U - Unexpected Error
   ================================================================================*/
   PROCEDURE complete_batch (
      p_validation_level         IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list            IN              VARCHAR2 := fnd_api.g_false
     ,x_message_count            OUT NOCOPY      NUMBER
     ,x_message_list             OUT NOCOPY      VARCHAR2
     ,x_return_status            OUT NOCOPY      VARCHAR2
     ,p_batch_header_rec         IN              gme_batch_header%ROWTYPE
     ,x_batch_header_rec         OUT NOCOPY      gme_batch_header%ROWTYPE
     ,x_exception_material_tbl   OUT NOCOPY      gme_common_pvt.exceptions_tab
     ,p_ignore_exception         IN              VARCHAR2 := NULL);  --Bug#5186328

   /*================================================================================
     Procedure
       complete_step
     Description
       This procedure is used to complete a step.

     Parameters
       p_batch_step_rec (R)        The batch step record to complete.
       p_batch_header_rec (R)      The batch header record the step belongs to.
       x_batch_step_rec            The updated batch step record in complete state.
       x_exception_material_tbl    A table containing all the exception materials found during
                                   the complete process and an indication of why they are
                                   considered an.exception.
       x_return_status             outcome of the API call
                                   S - Success
                                   E - Error
                                   U - Unexpected Error
                                   X - Exception

   ================================================================================*/
   PROCEDURE complete_step (
      p_validation_level         IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list            IN              VARCHAR2 := fnd_api.g_false
     ,x_message_count            OUT NOCOPY      NUMBER
     ,x_message_list             OUT NOCOPY      VARCHAR2
     ,x_return_status            OUT NOCOPY      VARCHAR2
     ,p_batch_step_rec           IN              gme_batch_steps%ROWTYPE
     ,p_batch_header_rec         IN              gme_batch_header%ROWTYPE
     ,x_batch_step_rec           OUT NOCOPY      gme_batch_steps%ROWTYPE
     ,x_exception_material_tbl   OUT NOCOPY      gme_common_pvt.exceptions_tab
     ,p_ignore_exception         IN              VARCHAR2 := NULL); --Bug#5186328

    /*================================================================================
     Procedure
       delete_step
     Description
       This procedure deletes the step associated with the batch

     Parameters
       p_batch_step (R)          The batch step row to identify the step
                                 Following columns are used from this row.
                                 batchstep_id  (R)
       x_return_status           outcome of the API call
                                 S - Success
                                 E - Error
                                 U - Unexpected Error
   ================================================================================*/
   PROCEDURE delete_step (
      p_validation_level   IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list      IN              VARCHAR2 := fnd_api.g_false
     ,x_message_count      OUT NOCOPY      NUMBER
     ,x_message_list       OUT NOCOPY      VARCHAR2
     ,x_return_status      OUT NOCOPY      VARCHAR2
     ,p_batch_step_rec     IN              gme_batch_steps%ROWTYPE
     ,p_batch_header_rec   IN              gme_batch_header%ROWTYPE);

    /*================================================================================
     Procedure
       insert_step
     Description
       This procedure inserts the new step to the batch

     Parameters
       p_batch_step (R)          The batch step row to insert to the batch.
       x_batch_step              The batch step that is returned, with all the data
       x_return_status           outcome of the API call
                                 S - Success
                                 E - Error
                                 U - Unexpected Error
   ================================================================================*/
   PROCEDURE insert_step (
      p_validation_level   IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list      IN              VARCHAR2 := fnd_api.g_false
     ,x_message_count      OUT NOCOPY      NUMBER
     ,x_message_list       OUT NOCOPY      VARCHAR2
     ,x_return_status      OUT NOCOPY      VARCHAR2
     ,p_batch_header_rec   IN              gme_batch_header%ROWTYPE
     ,p_batch_step_rec     IN              gme_batch_steps%ROWTYPE
     ,x_batch_step         OUT NOCOPY      gme_batch_steps%ROWTYPE);
   /*================================================================================
    Procedure
      revert_to_wip_batch
    Description
      This procedure reverts a completed batch to WIP and all the phantom batches.

    Parameters
      p_batch_header (R)        The batch header row to identify the batch
                                Following columns are used from this row.
      x_batch_header            The batch header that is returned, with all the data
      x_return_status           outcome of the API call
                                S - Success
                                E - Error
                                U - Unexpected Error
  ================================================================================*/
    PROCEDURE revert_batch (
      p_validation_level       	IN              NUMBER := gme_common_pvt.g_max_errors
     ,p_init_msg_list          	IN              VARCHAR2 := FND_API.G_FALSE 
     ,x_message_count          	OUT NOCOPY      NUMBER
     ,x_message_list           	OUT NOCOPY      VARCHAR2
     ,x_return_status          	OUT NOCOPY      VARCHAR2
     ,p_create_resv_pend_lots    IN              NUMBER DEFAULT 1    -- Bug 23640627
     ,p_batch_header_rec       	IN              gme_batch_header%ROWTYPE
     ,x_batch_header_rec      	OUT NOCOPY 	    gme_batch_header%ROWTYPE);

   /*================================================================================
    Procedure
      revert_step
    Description
      This procedure reverts a step to WIP.

    Parameters
      p_batch_step_rec (R)          The batch step row to identify the step
                                Following columns are used from this row.
                                batchstep_id  (R)
                               
      p_org_code                The name of organization to which this batch belongs
      p_batch_header_rec	batch number to which this step belongs
      x_batch_step_rec          The batch step that is returned, with all the data
      x_return_status           outcome of the API call
                                S - Success
                                E - Error
                                U - Unexpected Error
  ================================================================================*/

   PROCEDURE revert_step (
      p_validation_level       	IN              NUMBER := gme_common_pvt.g_max_errors
     ,p_init_msg_list          	IN              VARCHAR2 := FND_API.G_FALSE 
     ,x_message_count          	OUT NOCOPY      NUMBER
     ,x_message_list           	OUT NOCOPY      VARCHAR2
     ,x_return_status          	OUT NOCOPY      VARCHAR2
     ,p_create_resv_pend_lots    IN              NUMBER DEFAULT 1    -- Bug 23640627
     ,p_batch_step_rec        	IN              gme_batch_steps%ROWTYPE
     ,p_batch_header_rec         IN 	          gme_batch_header%ROWTYPE
     ,x_batch_step_rec        	OUT NOCOPY gme_batch_steps%ROWTYPE);
     
 
     /*================================================================================
     Procedure
       close_batch
     Description
       This procedure closes batch and all the phantom batches.
        It also closes all the steps.

     Parameters
       p_batch_header (R)        The batch header row to identify the batch
                                 Following columns are used from this row.
                                 batch_id  (R)
                                 batch_close_date (O)

       x_batch_header            The batch header that is returned, with all the data
       x_return_status           outcome of the API call
                                 S - Success
                                 E - Error
                                 U - Unexpected Error
   ================================================================================*/
   PROCEDURE close_batch (
      p_validation_level   IN              NUMBER
     ,p_init_msg_list      IN              VARCHAR2
     ,x_message_count      OUT NOCOPY      NUMBER
     ,x_message_list       OUT NOCOPY      VARCHAR2
     ,x_return_status      OUT NOCOPY      VARCHAR2
     ,p_batch_header_rec   IN              gme_batch_header%ROWTYPE
     ,x_batch_header_rec   OUT NOCOPY      gme_batch_header%ROWTYPE);

    /*================================================================================
     Procedure
       close_step
     Description
       This procedure closes step.

     Parameters
       p_batch_step (R)          The batch step row to identify the step
                                 Following columns are used from this row.
                                 batchstep_id  (R)
                                 step_close_date (O)
       p_delete_pending (R)      Delete the pending allocations if any for the
                                 material lines associated with the step.

       x_batch_step              The batch step that is returned, with all the data
       x_return_status           outcome of the API call
                                 S - Success
                                 E - Error
                                 U - Unexpected Error
   ================================================================================*/
   PROCEDURE close_step (
      p_validation_level   IN              NUMBER
            := gme_common_pvt.g_max_errors                  /* Punit Kumar */
     ,p_init_msg_list      IN              VARCHAR2 := fnd_api.g_false
     ,x_message_count      OUT NOCOPY      NUMBER
     ,x_message_list       OUT NOCOPY      VARCHAR2
     ,x_return_status      OUT NOCOPY      VARCHAR2
     ,p_batch_header_rec   IN              gme_batch_header%ROWTYPE
     ,p_batch_step_rec     IN              gme_batch_steps%ROWTYPE
     ,p_delete_pending     IN              VARCHAR2 := fnd_api.g_false
     ,x_batch_step_rec     OUT NOCOPY      gme_batch_steps%ROWTYPE);

    /*================================================================================
     Procedure
       reopen_batch
     Description
       This procedure reopens batch and all the phantom batches.
        It also reopens all the steps, if requested so.

     Parameters
       p_batch_header (R)        The batch header row to identify the batch
                                 Following columns are used from this row.
                                 batch_id  (R)
       p_reopen_steps (O)        Reopen all the steps.

       x_batch_header            The batch header that is returned, with all the data
       x_return_status           outcome of the API call
                                 S - Success
                                 E - Error
                                 U - Unexpected Error
   ================================================================================*/
   PROCEDURE reopen_batch (
      p_validation_level   IN              NUMBER
            := gme_common_pvt.g_max_errors                  /* Punit Kumar */
     ,p_init_msg_list      IN              VARCHAR2 := fnd_api.g_false
     ,x_message_count      OUT NOCOPY      NUMBER
     ,x_message_list       OUT NOCOPY      VARCHAR2
     ,x_return_status      OUT NOCOPY      VARCHAR2
     ,p_batch_header_rec   IN              gme_batch_header%ROWTYPE
     ,p_reopen_steps       IN              VARCHAR2 := fnd_api.g_false
     ,x_batch_header_rec   OUT NOCOPY      gme_batch_header%ROWTYPE);

    /*================================================================================
     Procedure
       reopen_step
     Description
       This procedure reopens step.

     Parameters
       p_batch_step (R)          The batch header row to identify the batch
                                 Following columns are used from this row.
                                 batchstep_id  (R)

       x_batch_header            The batch header that is returned, with all the data
       x_return_status           outcome of the API call
                                 S - Success
                                 E - Error
                                 U - Unexpected Error
   ================================================================================*/
   PROCEDURE reopen_step (
      p_validation_level   IN              NUMBER
            := gme_common_pvt.g_max_errors                  /* Punit Kumar */
     ,p_init_msg_list      IN              VARCHAR2 := fnd_api.g_false
     ,x_message_count      OUT NOCOPY      NUMBER
     ,x_message_list       OUT NOCOPY      VARCHAR2
     ,x_return_status      OUT NOCOPY      VARCHAR2
     ,p_batch_header_rec   IN              gme_batch_header%ROWTYPE
     ,p_batch_step_rec     IN              gme_batch_steps%ROWTYPE
     ,x_batch_step_rec     OUT NOCOPY      gme_batch_steps%ROWTYPE);

    /*================================================================================
     Procedure
       incremental_backflush
     Description
       This procedure is used to incrementally backflush the qty to the material line.

     Parameters
      p_batch_header_rec (R)    The batch header record
      p_material_detail_rec (R) The material detail record
      p_qty (R)                 The quantity to apply incrementally as follows:
      p_qty_type (R)            0 - By increment qty
                                1 - New actual qty
                                2 - % of Plan
      p_trans_date              Transaction date to record for the incremental backflush
      x_exception_material_tab  Table of materials that could not be consumed or yielded
                                for the calculated incremental quantity
      x_return_status           result of the API call
                                S - Success
                                E - Error
                                U - Unexpected Error
                                X - Batch Exception

    HISTORY
   ================================================================================*/
   PROCEDURE incremental_backflush (
      p_validation_level         IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list            IN              VARCHAR2 := fnd_api.g_false
     ,x_message_count            OUT NOCOPY      NUMBER
     ,x_message_list             OUT NOCOPY      VARCHAR2
     ,x_return_status            OUT NOCOPY      VARCHAR2
     ,p_batch_header_rec         IN              gme_batch_header%ROWTYPE
     ,p_material_detail_rec      IN              gme_material_details%ROWTYPE
     ,p_qty                      IN              NUMBER
     ,p_qty_type                 IN              NUMBER
     ,p_trans_date               IN              DATE
     ,p_create_resv_pend_lots    IN              NUMBER DEFAULT 1 -- Bug 22317010 
     ,p_tolerance                IN              NUMBER DEFAULT NULL -- Bug 22317010 / 22764488
     ,p_lpn_id                   IN              NUMBER DEFAULT NULL -- for future use Bugs 13628717 and 12884831
     ,x_exception_material_tbl   OUT NOCOPY      gme_common_pvt.exceptions_tab);


    /*================================================================================
     Procedure
       reroute_batch
     Description
       This procedure reroutes batch (typically changes the routing
       associated with the batch).

     Parameters
       p_batch_header_rec (R)    The batch header row to identify the batch
                                 Following columns are used from this row.
                                 batch_id  (R)
       p_validity_rule_id (R)    Recipe validity rule id for the new recipe.

       x_batch_header            The batch header that is returned, with all the data
       x_return_status           outcome of the API call
                                 S - Success
                                 E - Error
                                 U - Unexpected Error
                                 C - No continous periods found
   ================================================================================*/
   PROCEDURE reroute_batch (
      p_validation_level      IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list         IN              VARCHAR2
            DEFAULT fnd_api.g_false
     ,p_batch_header_rec      IN              gme_batch_header%ROWTYPE
     ,p_validity_rule_id      IN              NUMBER
     ,p_use_workday_cal       IN              VARCHAR2
            DEFAULT fnd_api.g_false
     ,p_contiguity_override   IN              VARCHAR2
            DEFAULT fnd_api.g_false
     ,x_message_count         OUT NOCOPY      NUMBER
     ,x_message_list          OUT NOCOPY      VARCHAR2
     ,x_return_status         OUT NOCOPY      VARCHAR2
     ,x_batch_header_rec      OUT NOCOPY      gme_batch_header%ROWTYPE);

    /*================================================================================
     Procedure
       cancel_batch
     Description
       This procedure cancels batch and all the phantom batches.
        It also cancels all the steps.

     Parameters
       p_batch_header (R)        The batch header row to identify the batch
                                 Following columns are used from this row.
                                 batch_id  (R)
       x_batch_header            The batch header that is returned, with all the data
       x_return_status           outcome of the API call
                                 S - Success
                                 E - Error
                                 U - Unexpected Error
   ================================================================================*/
   PROCEDURE cancel_batch (
      p_validation_level   IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list      IN              VARCHAR2 := fnd_api.g_false
     ,x_message_count      OUT NOCOPY      NUMBER
     ,x_message_list       OUT NOCOPY      VARCHAR2
     ,x_return_status      OUT NOCOPY      VARCHAR2
     ,p_batch_header_rec   IN              gme_batch_header%ROWTYPE
     ,x_batch_header_rec   OUT NOCOPY      gme_batch_header%ROWTYPE);

    /*================================================================================
     Procedure
       terminate_batch

     Description
       This procedure terminates batch and all the phantom batches.
        It also terminates all the steps.

     Parameters
       p_batch_header (R)        The batch header row to identify the batch
                                 Following columns are used from this row.
                                 batch_id  (R)
       p_reason_name             Reason to terminate the batch
       x_batch_header            The batch header that is returned, with all the data
       x_return_status           outcome of the API call
                                 S - Success
                                 E - Error
                                 U - Unexpected Error
   ================================================================================*/
   PROCEDURE terminate_batch (
      p_validation_level   IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list      IN              VARCHAR2 := fnd_api.g_false
     ,x_message_count      OUT NOCOPY      NUMBER
     ,x_message_list       OUT NOCOPY      VARCHAR2
     ,x_return_status      OUT NOCOPY      VARCHAR2
     ,p_batch_header_rec   IN              gme_batch_header%ROWTYPE
     ,x_batch_header_rec   OUT NOCOPY      gme_batch_header%ROWTYPE);

   /*================================================================================
     Procedure
       unrelease_batch
     Description
       This procedure is used to unrelease a batch, it's phantom batches and any WIP steps.

     Parameters
       p_batch_header_rec (R)      The batch header record to unrelease..
       p_create_resv_pend_lots     Create pending product lots / reservations
                                   1 = Yes
                                   0 = No
       x_batch_header_rec          The updated batch header record in Pending state.
       x_return_status             outcome of the API call
                                   S - Success
                                   E - Error
                                   U - Unexpected Error
   ================================================================================*/
   PROCEDURE unrelease_batch (
      p_validation_level        IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list           IN              VARCHAR2 := fnd_api.g_false
     ,x_message_count           OUT NOCOPY      NUMBER
     ,x_message_list            OUT NOCOPY      VARCHAR2
     ,x_return_status           OUT NOCOPY      VARCHAR2
     ,p_batch_header_rec        IN              gme_batch_header%ROWTYPE
     ,x_batch_header_rec        OUT NOCOPY      gme_batch_header%ROWTYPE
     ,p_create_resv_pend_lots   IN              NUMBER);

   /*================================================================================
     Procedure
       unrelease_step
     Description
       This procedure is used to unrelease a step, and assoc phantom batches.

     Parameters
       p_batch_step_rec (R)        The batch step record to unrelease..
       p_batch_header_rec (R)      The batch header record that the step belongs to.
       p_create_resv_pend_lots     Create pending product lots / reservations
                                   1 = Yes
                                   0 = No
       p_update_inventory_ind      Update inventory indicator from the batch header
       x_batch_step_rec            The updated batch step record in Pending state.
       x_return_status             outcome of the API call
                                   S - Success
                                   E - Error
                                   U - Unexpected Error
   ================================================================================*/
   PROCEDURE unrelease_step (
      p_validation_level        IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list           IN              VARCHAR2 := fnd_api.g_false
     ,x_message_count           OUT NOCOPY      NUMBER
     ,x_message_list            OUT NOCOPY      VARCHAR2
     ,x_return_status           OUT NOCOPY      VARCHAR2
     ,p_batch_step_rec          IN              gme_batch_steps%ROWTYPE
     ,p_batch_header_rec        IN              gme_batch_header%ROWTYPE
     ,x_batch_step_rec          OUT NOCOPY      gme_batch_steps%ROWTYPE
     ,p_create_resv_pend_lots   IN              NUMBER);

    /*================================================================================
     Procedure
       auto_detail_line
     Description
       This procedure automatically generates detailed level reservations for an
       ingredient line.  The Rules Engine is used to determine the detailing.
     Parameters
       p_material_detail_rec (R) Ingredient line row
       x_return_status           outcome of the API call
                                 S - Success
                                 E - Error
                                 U - Unexpected Error
   ================================================================================*/
   PROCEDURE auto_detail_line (
      p_init_msg_list         IN              VARCHAR2 := fnd_api.g_false
     ,x_message_count         OUT NOCOPY      NUMBER
     ,x_message_list          OUT NOCOPY      VARCHAR2
     ,x_return_status         OUT NOCOPY      VARCHAR2
     ,p_material_detail_rec   IN              gme_material_details%ROWTYPE);

   /*================================================================================
    Procedure
      auto_detail_batch 
    Description
      This procedure automatically generates detailed level reservations for the
      ingredient lines of a production batch. 
      The Rules Engine is used to determine the detailing.

    Parameters
      p_init_msg_list     (O)   Specifies whether or not message list should be 
                                initialized (T/F)
      x_message_count           Number of messages added to the stack
      x_message_list            Messages generated during processing
      x_return_status           outcome of the API call
                                S - Success
                                E - Error
                                U - Unexpected Error
      p_batch_rec         (R)   Production Batch Header row                                         
  ================================================================================*/

   PROCEDURE auto_detail_batch(
      p_init_msg_list            IN              VARCHAR2 := FND_API.G_FALSE,
      x_message_count            OUT NOCOPY      NUMBER,
      x_message_list             OUT NOCOPY      VARCHAR2,
      x_return_status            OUT NOCOPY      VARCHAR2,
      p_batch_rec                IN              gme_batch_header%ROWTYPE);
      
   /*================================================================================
    Procedure
      create_pending_product_lot 
    Description
      This procedure creates a pending product lot record.

    Parameters
      p_init_msg_list     (O)   Specifies whether or not message list should be 
                                initialized (T/F)
      x_message_count           Number of messages added to the stack
      x_message_list            Messages generated during processing
      x_return_status           outcome of the API call
                                S - Success
                                E - Error
                                U - Unexpected Error
      p_org_id                  Organization ID
      p_pending_product_lots_rec   (R)   Pending Product Lots record to be created.
      x_pending_product_lots_rec         Pending Product Lots record that was created.      
  ================================================================================*/
  
   PROCEDURE create_pending_product_lot (
      p_validation_level        IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list           IN              VARCHAR2 := fnd_api.g_false
     ,x_message_count           OUT NOCOPY      NUMBER
     ,x_message_list            OUT NOCOPY      VARCHAR2
     ,x_return_status           OUT NOCOPY      VARCHAR2
     ,p_org_id                  IN              NUMBER
     ,p_pending_product_lots_rec IN  gme_pending_product_lots%ROWTYPE
     ,x_pending_product_lots_rec OUT NOCOPY  gme_pending_product_lots%ROWTYPE);

   /*================================================================================
    Procedure
      update_pending_product_lot 
    Description
      This procedure updates a pending product lot record.

    Parameters
      p_init_msg_list     (O)   Specifies whether or not message list should be 
                                initialized (T/F)
      x_message_count           Number of messages added to the stack
      x_message_list            Messages generated during processing
      x_return_status           outcome of the API call
                                S - Success
                                E - Error
                                U - Unexpected Error
      p_org_id                  Organization ID
      p_pending_product_lots_rec   (R)   Pending Product Lots record to be updated.
      x_pending_product_lots_rec         Pending Product Lots record that was updated.      
  ================================================================================*/
  
   PROCEDURE update_pending_product_lot (
      p_validation_level           IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list              IN              VARCHAR2 := fnd_api.g_false
     ,x_message_count              OUT NOCOPY      NUMBER
     ,x_message_list               OUT NOCOPY      VARCHAR2
     ,x_return_status              OUT NOCOPY      VARCHAR2
     ,p_org_id                     IN              NUMBER
     ,p_pending_product_lots_rec   IN  gme_pending_product_lots%ROWTYPE
     ,x_pending_product_lots_rec   IN  OUT NOCOPY  gme_pending_product_lots%ROWTYPE);
     
   /*================================================================================
    Procedure
      delete_pending_product_lot 
    Description
      This procedure deletes a pending product lot record.

    Parameters
      p_init_msg_list     (O)   Specifies whether or not message list should be 
                                initialized (T/F)
      x_message_count           Number of messages added to the stack
      x_message_list            Messages generated during processing
      x_return_status           outcome of the API call
                                S - Success
                                E - Error
                                U - Unexpected Error
      p_org_id                  Organization ID
      p_pending_product_lots_rec   (R)   Pending Product Lots record to be deleted.
  ================================================================================*/
  
   PROCEDURE delete_pending_product_lot (
      p_validation_level           IN              NUMBER
            := gme_common_pvt.g_max_errors
     ,p_init_msg_list              IN              VARCHAR2 := fnd_api.g_false
     ,x_message_count              OUT NOCOPY      NUMBER
     ,x_message_list               OUT NOCOPY      VARCHAR2
     ,x_return_status              OUT NOCOPY      VARCHAR2
     ,p_org_id                     IN              NUMBER
     ,p_pending_product_lots_rec   IN  gme_pending_product_lots%ROWTYPE);
     
  
     -- ============================================================================
--  Name        : insert_batch_step_quantity
--  Description : This procedure is used to insert all the batch step transfer details
--                into GME_BATCH_STEPS_QUANTITY table and validates all the required
--                validations before insertion.
--  Parameters:
--        IN    :
--                p_batch_id                 IN      NUMBER
--                p_batch_step_quantity_tbl  IN      batch_step_quantity_tbl
--
--        OUT   :
--               x_return_status    IN OUT NOCOPY VARCHAR2
--               Used to verify whether procedure successfully completed or any
--               errors ,warnings raised in the procedure and returns the exact status.
--
--                x_msg_count       IN OUT NOCOPY NUMBER,
--                x_msg_data         IN OUT NOCOPY VARCHAR2,
--                p_commit          IN OUT NOCOPY VARCHAR2
--  ============================================================================


    PROCEDURE insert_batch_step_quantity(
      p_batch_id                IN NUMBER,
      p_batch_step_quantity_tbl IN gme_api_pub.batch_step_quantity_tbl,
      x_return_status    IN OUT NOCOPY VARCHAR2,
      x_msg_count        IN OUT NOCOPY NUMBER,
      x_msg_data         IN OUT NOCOPY VARCHAR2,
      p_commit           IN OUT NOCOPY VARCHAR2);

-- ============================================================================
--  Name        : update_batch_step_quantity
--  Description : This procedure is used to update all the batch step transfer details
--                of GME_BATCH_STEPS_QUANTITY table and validates all the required
--                validations before updation.
--  Parameters:
--        IN    :
--                p_batch_id                 IN      NUMBER
--                p_batch_step_quantity_tbl  IN      batch_step_quantity_tbl
--
--        OUT   :
--               x_return_status    IN OUT NOCOPY VARCHAR2
--               Used to verify whether procedure successfully completed or any
--               errors ,warnings raised in the procedure and returns the exact status.
--
--                x_msg_count       IN OUT NOCOPY NUMBER,
--                x_msg_data         IN OUT NOCOPY VARCHAR2,
--                p_commit          IN OUT NOCOPY VARCHAR2
--  ============================================================================

    PROCEDURE update_batch_step_quantity(
      p_batch_id                IN NUMBER,
      p_batch_step_quantity_tbl IN gme_api_pub.batch_step_quantity_tbl,
      x_return_status    IN OUT NOCOPY VARCHAR2,
      x_msg_count        IN OUT NOCOPY NUMBER,
      x_msg_data         IN OUT NOCOPY VARCHAR2,
      p_commit           IN OUT NOCOPY VARCHAR2);

-- ============================================================================
--  Name        : delete_batch_step_quantity
--  Description : This procedure is used to delete all the input batch step transfer details
--                of GME_BATCH_STEPS_QUANTITY table and validates all the required
--                validations before deletion.
--  Parameters:
--        IN    :
--                p_batch_id                 IN      NUMBER
--                p_batch_step_quantity_tbl  IN      batch_step_quantity_tbl
--
--        OUT   :
--               x_return_status    IN OUT NOCOPY VARCHAR2
--               Used to verify whether procedure successfully completed or any
--               errors ,warnings raised in the procedure and returns the exact status.
--
--                x_msg_count       IN OUT NOCOPY NUMBER,
--                x_msg_data         IN OUT NOCOPY VARCHAR2,
--                p_commit          IN OUT NOCOPY VARCHAR2
--  ============================================================================

    PROCEDURE delete_batch_step_quantity(
      p_batch_id                IN NUMBER,
      p_batch_step_quantity_tbl IN gme_api_pub.batch_step_quantity_tbl,
      x_return_status    IN OUT NOCOPY VARCHAR2,
      x_msg_count        IN OUT NOCOPY NUMBER,
      x_msg_data         IN OUT NOCOPY VARCHAR2,
      p_commit           IN OUT NOCOPY VARCHAR2);

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
	 ,p_batch_no                 IN              VARCHAR2 := NULL
	 ,p_batchstep_no             IN              NUMBER := NULL
	 ,p_line_type             	 IN              NUMBER := NULL
	 ,p_line_no              	 IN              NUMBER
	 ,x_message_count            OUT NOCOPY      NUMBER
     ,x_message_list             OUT NOCOPY      VARCHAR2
     ,x_return_status            OUT NOCOPY      VARCHAR2
     );
     
     
     
-- ============================================================================
--  Name        : update_batch_heat
--  Description : This Procedure is used to update heat number for the batch 
--                which already exist in the gme_batch_genealogy table,and push
--                down to update child batches of the specified batch.
--  Parameters:
--        IN    :
--                p_batch_id        - batch id
--                p_heat_number	    - heat number
--
--  ============================================================================      
  PROCEDURE update_batch_heat(
    p_batch_id             IN   NUMBER,
    p_heat_number          IN   VARCHAR2,
    x_return_status        OUT  NOCOPY VARCHAR2,
    x_message_count        OUT  NOCOPY NUMBER,
    x_message_list         OUT  NOCOPY VARCHAR2
    );     

END gme_api_main;
/

COMMIT ;
EXIT;
