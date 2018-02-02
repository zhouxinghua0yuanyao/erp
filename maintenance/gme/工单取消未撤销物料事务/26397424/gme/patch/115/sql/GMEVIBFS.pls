/* +======================================================================+ */
/* |    Copyright (c) 2005, 2016 Oracle and/or its affiliates.           | */
/* |                         All rights reserved.                         | */
/* |                           Version 12.0.0                             | */
/* +======================================================================+ */
REM dbdrv: sql ~PROD ~PATH ~FILE none none none package &phase=pls \
REM dbdrv: checkfile(120.3.12010000.2=120.3.12020000.2):~PROD:~PATH:~FILE 
REM ******************************************************************************
REM *                                                                            *
REM * FILE:    GMEVIBFS.pls                                                      *
REM * PURPOSE: Package Specification for the GME Incremental Backflush Routines  *
REM * AUTHOR:  A. Newbury                                                        *
REM * DATE:    June 2005                                                         *
REM * HISTORY:                                                                   *
REM * ========                                                                   *
REM *                                                                            *
REM * G. Muratore    04-APR-2016 Bug 22317010 / 22764488
REM *   Introduced new p_create_resv_pend_lots and p_tolerance parameter.
 
REM *   Introduced new p_lpn_id parameter for future use. Bugs 13628717/12884831. 
REM *   PROCEDURE: incremental_backflush
REM ******************************************************************************

/***********************************************************************************
* This file contains the procedure for incremental backflush of batches in Oracle  *
* Process Manufacturing (OPM). Each procedure has a common set of                  *
* parameters to which API-specific parameters are appended.                        *
************************************************************************************/

CREATE OR REPLACE PACKAGE gme_incremental_backflush_pvt AS
/* $Header: GMEVIBFS.pls 120.3.12020000.2 2016/04/06 18:21:20 gmurator ship $ */

  PROCEDURE incremental_backflush
    (p_batch_header_rec           IN GME_BATCH_HEADER%ROWTYPE
    ,p_material_detail_rec        IN GME_MATERIAL_DETAILS%ROWTYPE
    ,p_qty                        IN NUMBER 
    ,p_qty_type                   IN NUMBER
    ,p_trans_date                 IN DATE
    ,p_backflush_rsrc_usg_ind     IN NUMBER
    ,p_create_resv_pend_lots      IN NUMBER DEFAULT 1 -- Bug 22317010 
    ,p_tolerance                  IN NUMBER DEFAULT .9997 -- Bug 22317010 / 22764488
    ,p_lpn_id                     IN NUMBER DEFAULT NULL -- for future use Bugs 13628717 and 12884831
    ,x_exception_material_tbl     IN OUT NOCOPY gme_common_pvt.exceptions_tab
    ,x_return_status              OUT NOCOPY VARCHAR2);

  PROCEDURE derive_factor
    (p_material_detail_rec   IN  gme_material_details%ROWTYPE
    ,p_qty                   IN  NUMBER
    ,p_qty_type              IN  NUMBER
    --FPBug#4667093
    ,p_gme_ib_factor         IN  NUMBER DEFAULT 0
    ,x_pct_plan              OUT NOCOPY NUMBER
    ,x_pct_plan_res          OUT NOCOPY NUMBER
    ,x_return_status         OUT NOCOPY VARCHAR2);

  PROCEDURE update_dependent_steps(p_batchstep_id     IN  NUMBER
                                  ,p_backflush_factor IN  NUMBER
                                  ,x_return_status    OUT NOCOPY VARCHAR2);

  PROCEDURE revert_material_partial
    (p_material_detail_rec        IN gme_material_details%ROWTYPE
    ,p_qty                        IN NUMBER
    ,p_lot_control_code           IN NUMBER  -- 1 = not lot control; 2 = lot control
    ,p_create_resv_pend_lots      IN NUMBER
    ,p_lot_divisible_flag         IN VARCHAR2
    ,x_actual_qty                 OUT NOCOPY NUMBER
    ,x_exception_material_tbl     IN OUT NOCOPY gme_common_pvt.exceptions_tab
    ,x_return_status              OUT NOCOPY VARCHAR2);

  PROCEDURE validate_material_for_IB(p_material_detail_rec IN gme_material_details%ROWTYPE
                                    ,p_batch_header_rec    IN gme_batch_header%ROWTYPE
                                    ,p_adjust_cmplt        IN VARCHAR2
                                    ,x_return_status       OUT NOCOPY VARCHAR2);

  PROCEDURE validate_qty_for_IB (p_qty_type   IN NUMBER
                                ,p_qty        IN NUMBER
                                ,p_actual_qty IN NUMBER
                                ,x_return_status OUT NOCOPY VARCHAR2);

  PROCEDURE get_converted_qty (
      p_org_id                    IN NUMBER
     ,p_item_id                   IN NUMBER
     ,p_lot_number                IN VARCHAR2 DEFAULT NULL
     ,p_qty                       IN NUMBER
     ,p_from_um                   IN VARCHAR2
     ,p_to_um                     IN VARCHAR2
     ,x_conv_qty                  OUT NOCOPY NUMBER
     ,x_return_status             OUT NOCOPY VARCHAR2);

END gme_incremental_backflush_pvt;
/
commit;
exit;





