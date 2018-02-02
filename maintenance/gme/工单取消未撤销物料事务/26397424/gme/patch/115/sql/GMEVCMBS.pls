/* +======================================================================+ */
/* |    Copyright (c) 2005, 2014 Oracle and/or its affiliates.           | */
/* |                         All rights reserved.                         | */
/* |                           Version 12.0.0                             | */
/* +======================================================================+ */
REM dbdrv: sql ~PROD ~PATH ~FILE none none none package &phase=pls \
REM dbdrv: checkfile(120.4.12010000.2=120.4.12020000.2):~PROD:~PATH:~FILE
SET VERIFY OFF
WHENEVER SQLERROR EXIT FAILURE ROLLBACK;
WHENEVER OSERROR EXIT FAILURE ROLLBACK;
REM ***********************************************************************
REM *                                                                     *
REM * FILE:    GMEVCMBS.pls                                               *
REM * PURPOSE: Package Spec for the routines to complete a production or  *
REM *          lab batch
REM * AUTHOR:  Antonia Newbury, OPM Development                           *
REM * DATE:    07-Mar-2005                                                *
REM * HISTORY:                                                            *
REM * ========                                                            *
REM *                                                                     *
REM ***********************************************************************

/**************************************************************************
* This file contains the procedure for actions on material lines in Oracle*
* Process Manufacturing (OPM). Each procedure has a common set of         *
* parameters to which API-specific parameters are appended.               *
**************************************************************************/

CREATE OR REPLACE PACKAGE gme_complete_batch_pvt AS
/* $Header: GMEVCMBS.pls 120.4.12020000.2 2014/07/21 07:48:54 shalchen ship $ */

  PROCEDURE complete_batch
              (p_batch_header_rec           IN         gme_batch_header%ROWTYPE
              ,x_exception_material_tbl     IN  OUT NOCOPY  gme_common_pvt.exceptions_tab
              ,x_batch_header_rec           OUT NOCOPY gme_batch_header%ROWTYPE              
              ,x_return_status              OUT NOCOPY VARCHAR2);

  PROCEDURE process_material
              (p_material_detail_rec        IN         gme_material_details%ROWTYPE
              ,p_yield                      IN         BOOLEAN
              ,p_trans_date                 IN         DATE
              ,p_update_inv_ind             IN         VARCHAR2
              ,x_exception_material_tbl     IN  OUT NOCOPY  gme_common_pvt.exceptions_tab
              ,x_return_status              OUT NOCOPY      VARCHAR2);

  PROCEDURE yield_material(p_material_dtl_rec  IN gme_material_details%ROWTYPE
                            ,p_yield_qty       IN NUMBER
                            ,p_trans_date      IN DATE
                            ,p_item_rec        IN mtl_system_items_b%ROWTYPE
                            ,p_force_unconsumed IN VARCHAR2
                            ,x_exception_material_tbl    IN OUT NOCOPY gme_common_pvt.exceptions_tab
                            ,x_actual_qty      OUT NOCOPY NUMBER
                            ,x_return_status   OUT NOCOPY VARCHAR2);

  PROCEDURE build_and_create_transaction
              (p_mtl_dtl_rec           IN gme_material_details%ROWTYPE
              ,p_pp_lot_rec            IN gme_pending_product_lots%ROWTYPE
              ,p_subinv                IN VARCHAR2
              ,p_locator_id            IN NUMBER
              ,p_trans_date            IN DATE
              ,p_yield_qty             IN NUMBER
              ,p_revision              IN VARCHAR2 DEFAULT NULL
              ,p_sec_uom_code          IN VARCHAR2 DEFAULT NULL
              ,x_actual_qty            IN OUT NOCOPY NUMBER
              ,x_return_status         OUT NOCOPY VARCHAR2);

  PROCEDURE constr_mmti
    (p_mtl_dtl_rec              IN   gme_material_details%ROWTYPE
    ,p_yield_qty                IN   NUMBER
    ,p_subinv                   IN   VARCHAR2
    ,p_locator_id               IN   NUMBER
    ,p_revision                 IN   VARCHAR2
    ,p_pp_lot_rec               IN   gme_pending_product_lots%ROWTYPE
    ,x_mmti_rec                 OUT  NOCOPY mtl_transactions_interface%ROWTYPE
    ,x_mmli_tbl                 OUT  NOCOPY gme_common_pvt.mtl_trans_lots_inter_tbl
    ,x_sec_qty                  OUT  NOCOPY NUMBER
    ,x_dtl_qty                  OUT  NOCOPY NUMBER
    ,x_return_status            OUT  NOCOPY VARCHAR2);

  /*ER 19161894  Shaliu Chen 18-JUL-2014
    Add input parameter p_ignore_exception
  */
  PROCEDURE validate_batch_for_complete
    (p_batch_header_rec     IN gme_batch_header%ROWTYPE
    ,p_ignore_exception     IN VARCHAR2 DEFAULT 'F'    
    ,x_batch_header_rec     OUT NOCOPY gme_batch_header%ROWTYPE
    ,x_return_status        OUT NOCOPY VARCHAR2);


END gme_complete_batch_pvt;
/
COMMIT;
EXIT;
--show errors
