/* +======================================================================+ */
/* |    Copyright (c) 2005, 2016 Oracle and/or its affiliates.           | */
/* |                         All rights reserved.                         | */
/* |                           Version 12.0.0                             | */
/* +======================================================================+ */
REM dbdrv: sql ~PROD ~PATH ~FILE none none none package &phase=pls \
REM dbdrv: checkfile(120.0.12010000.2=120.0.12020000.2):~PROD:~PATH:~FILE 
REM **********************************************************************
REM *                                                                    *
REM * FILE:    GMEVRWBS.pls                                              *
REM * PURPOSE: Package Specification for the GME REVERT BATCH routines	 *
REM * AUTHOR:  Pawan Kumar, OPM Development                              *
REM * DATE:    MAY 19th 2005                                             *
REM * HISTORY:                                                           *
REM * ========                                                           *
REM * G. Muratore    03-NOV-2016 Bug 23640627 
REM *   Introduced new p_create_resv_pend_lots parameter.
REM *   PROCEDURE: revert_batch and revert_line.
REM **********************************************************************

/*************************************************************************
* This file contains the procedure for uncertifying batches in Oracle    *
* Process Manufacturing (OPM). Each procedure has a common set of        *
* parameters to which API-specific parameters are appended.              *
*************************************************************************/

CREATE OR REPLACE PACKAGE gme_revert_batch_pvt AS
/* $Header: GMEVRWBS.pls 120.0.12020000.2 2016/11/07 16:51:43 gmurator ship $ */

PROCEDURE revert_batch
(p_batch_header_rec      IN GME_BATCH_HEADER%ROWTYPE
,x_batch_header_rec      OUT NOCOPY GME_BATCH_HEADER%ROWTYPE
,p_create_resv_pend_lots IN  NUMBER    -- Bug 23640627
,x_return_status         OUT NOCOPY VARCHAR2);

PROCEDURE revert_line 
(p_batch_header_rec      IN GME_BATCH_HEADER%ROWTYPE
,p_material_details_rec  IN gme_material_details%ROWTYPE
,p_batch_step_rec        IN gme_batch_steps%ROWTYPE
,p_create_resv_pend_lots IN NUMBER    -- Bug 23640627
,x_return_status         OUT NOCOPY VARCHAR2);


END gme_revert_batch_pvt;
/
COMMIT;
EXIT;
--SHOW ERRORS;
