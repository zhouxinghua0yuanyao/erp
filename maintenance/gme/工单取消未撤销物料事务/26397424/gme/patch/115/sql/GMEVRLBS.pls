/* +======================================================================+ */
/* |    Copyright (c) 2005, 2016 Oracle and/or its affiliates.           | */
/* |                         All rights reserved.                         | */
/* |                           Version 12.0.0                             | */
/* +======================================================================+ */
REM dbdrv: sql ~PROD ~PATH ~FILE none none none package &phase=pls \
REM dbdrv: checkfile(120.9.12010000.4=120.11.12020000.3)(120.9.12000000.3=120.9.12010000.2)(115.12=120.2):~PROD:~PATH:~FILE 
WHENEVER SQLERROR EXIT FAILURE ROLLBACK;
WHENEVER OSERROR EXIT FAILURE ROLLBACK;
REM ***********************************************************************
REM *                                                                     *
REM * FILE:    GMEVRLBS.pls                                               *
REM * PURPOSE: Package Spec for the routines to release a production or   *
REM *          lab batch
REM * AUTHOR:  Antonia Newbury, OPM Development                           *
REM * DATE:    07-Mar-2005                                                *
REM * HISTORY:                                                            *
REM * ========                                                            *
REM * A. Newbury     19-Jan-2006 Added global g_bypass_txn_creation for   *
REM *                            migration purposes; this will be set to  *
REM *                            1 in migration to indicate to            *
REM *                            gme_release_batch_pvt.consume_material & *
REM *                            gme_complete_batch_pvt.yield_material to *
REM *                            not create transactions; this is used for*
REM *                            release batch, release step and complete *
REM *                            step                                     *
REM *                            0 = don't bypass trxn creation (default) *
REM *                            1 = bypass trxn creation (for migration) *
REM *                                                                     *
REM * S. Kommineni   11-Feb-2008 Bug 6778968
REM *    Added parameter p_called_by to procedure build_and_create_transaction 
REM *    to support it being used by convert detail reservation api.
REM *    'REL' is the default as it is mainly used by release batch. 
REM *    'CVT' for conversion api.

REM * G. Muratore    04-APR-2016 Bug 22317010 / 22764488
REM *   p_tolerance acceptable values are between 1 and .99. This was introduced 
REM *   for handling problems caused by rounding over the course of multiple IB's. 
REM *   PROCEDURE: build_and_create_transaction and consume_material 
REM ***********************************************************************

/**************************************************************************
* This file contains the procedure for releasing a batch in Oracle        *
* Process Manufacturing (OPM). Each procedure has a common set of         *
* parameters to which API-specific parameters are appended.               *
**************************************************************************/

CREATE OR REPLACE PACKAGE gme_release_batch_pvt AS
/* $Header: GMEVRLBS.pls 120.11.12020000.3 2016/04/06 18:22:47 gmurator ship $ */

  g_bypass_txn_creation            NUMBER                           := 0;
  
  PROCEDURE release_batch
              (p_batch_header_rec           IN         gme_batch_header%ROWTYPE
              ,p_phantom_product_id         IN         NUMBER DEFAULT NULL
              ,p_yield                      IN         BOOLEAN DEFAULT NULL
              ,x_exception_material_tbl     IN  OUT NOCOPY   gme_common_pvt.exceptions_tab
              ,x_batch_header_rec           OUT NOCOPY gme_batch_header%ROWTYPE              
              ,x_return_status              OUT NOCOPY VARCHAR2);
              
  PROCEDURE process_ingredient
              (p_material_detail_rec        IN         gme_material_details%ROWTYPE
              ,p_consume                    IN         BOOLEAN
              ,p_trans_date                 IN         DATE
              ,p_update_inv_ind             IN         VARCHAR2
              ,x_exception_material_tbl     IN  OUT NOCOPY   gme_common_pvt.exceptions_tab
              ,x_return_status              OUT NOCOPY       VARCHAR2);

  PROCEDURE consume_material(p_material_dtl_rec  IN gme_material_details%ROWTYPE
                            ,p_consume_qty       IN NUMBER := NULL
                            ,p_trans_date        IN DATE := NULL
                            ,p_called_by         IN VARCHAR2 DEFAULT 'REL' -- Bug 22217179/21078209/21122837 - Added for IB             
                            ,p_tolerance         IN NUMBER DEFAULT .9997   -- Bug 22317010 / 22764488 - Added for IB             
                            ,p_item_rec          IN mtl_system_items_b%ROWTYPE
                            ,x_exception_material_tbl    IN OUT NOCOPY gme_common_pvt.exceptions_tab
                            ,x_actual_qty        OUT NOCOPY NUMBER
                            ,x_return_status     OUT NOCOPY VARCHAR2);
                            
  PROCEDURE build_and_create_transaction
              (p_rsrv_rec              IN mtl_reservations%ROWTYPE
              ,p_lot_divisible_flag    IN VARCHAR2 DEFAULT NULL  -- required for lot non divisible
              ,p_dispense_ind          IN VARCHAR2 DEFAULT NULL
              ,p_subinv                IN VARCHAR2 DEFAULT NULL
              ,p_locator_id            IN NUMBER DEFAULT NULL
              ,p_att                   IN NUMBER DEFAULT NULL
              ,p_satt                  IN NUMBER DEFAULT NULL
              ,p_primary_uom_code      IN VARCHAR2 DEFAULT NULL
              ,p_mtl_dtl_rec           IN gme_material_details%ROWTYPE
              ,p_trans_date            IN DATE
              ,p_consume_qty           IN NUMBER
              ,p_called_by             IN VARCHAR2 DEFAULT 'REL' -- Bug 6778968             
              ,p_tolerance             IN NUMBER DEFAULT .9997   -- Bug 22317010 / 22764488 - Added for IB             
              ,p_revision              IN VARCHAR2 DEFAULT NULL
              ,p_secondary_uom_code    IN VARCHAR2 DEFAULT NULL
              ,x_actual_qty            IN OUT NOCOPY NUMBER
              ,x_return_status         OUT NOCOPY VARCHAR2);

  PROCEDURE  constr_mmti_from_reservation
    (p_rsrv_rec              IN   mtl_reservations%ROWTYPE
    ,x_mmti_rec              OUT  NOCOPY mtl_transactions_interface%ROWTYPE
    ,x_mmli_tbl              OUT  NOCOPY gme_common_pvt.mtl_trans_lots_inter_tbl
    ,x_return_status         OUT  NOCOPY VARCHAR2);
    
  PROCEDURE constr_mmti_from_qty_tree
        (p_mtl_dtl_rec            IN gme_material_details%ROWTYPE
        ,p_subinv                 IN VARCHAR2
        ,p_locator_id             IN NUMBER
        ,x_mmti_rec               OUT  NOCOPY mtl_transactions_interface%ROWTYPE
        ,x_return_status          OUT  NOCOPY VARCHAR2);
        
  PROCEDURE create_batch_exception
              (p_material_dtl_rec         IN gme_material_details%ROWTYPE
              ,p_pending_move_order_ind   IN BOOLEAN := NULL
              ,p_pending_rsrv_ind         IN BOOLEAN := NULL
              ,p_transacted_qty           IN NUMBER := NULL
              ,p_exception_qty            IN NUMBER := NULL
              ,p_force_unconsumed         IN VARCHAR2 := fnd_api.g_true
              ,x_exception_material_tbl   IN OUT NOCOPY gme_common_pvt.exceptions_tab
              ,x_return_status            OUT NOCOPY VARCHAR2);
              
  PROCEDURE check_unexploded_phantom(p_batch_id              IN  NUMBER
                                    ,p_auto_by_step          IN  NUMBER
                                    ,p_batchstep_id          IN  NUMBER
                                    ,x_return_status         OUT NOCOPY VARCHAR2);

  PROCEDURE validate_batch_for_release  (p_batch_header_rec     IN gme_batch_header%ROWTYPE
                                        ,x_batch_header_rec     OUT NOCOPY gme_batch_header%ROWTYPE
                                        ,x_return_status        OUT NOCOPY VARCHAR2);


END gme_release_batch_pvt;
/
COMMIT;
EXIT;
--show errors
