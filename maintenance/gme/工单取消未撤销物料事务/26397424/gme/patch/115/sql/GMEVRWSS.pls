/* +======================================================================+ */
/* |    Copyright (c) 2005, 2016 Oracle and/or its affiliates.           | */
/* |                         All rights reserved.                         | */
/* |                           Version 12.0.0                             | */
/* +======================================================================+ */
REM dbdrv: sql ~PROD ~PATH ~FILE none none none package &phase=pls \
REM dbdrv: checkfile(120.0.12010000.2=120.0.12020000.2):~PROD:~PATH:~FILE 
SET VERIFY OFF
WHENEVER SQLERROR EXIT FAILURE ROLLBACK;
WHENEVER OSERROR EXIT FAILURE ROLLBACK; 
REM **********************************************************************
REM *                                                                    *
REM * FILE:    GMEVUCSS.pls                                              *
REM * PURPOSE: Package Specification for the GME UNCERTIFY STEP routines *
REM * AUTHOR:  Pawan Kumar, OPM Development                              *
REM * DATE:    May 24th 2001                                             *
REM * HISTORY:                                                           *
REM * ========                                                           *
REM * 24MAY05  Pawan Kumar                                               *
REM *          Created                                                   *
REM *                                                                    *
REM * G. Muratore    03-NOV-2016 Bug 23640627 
REM *   Introduced new p_create_resv_pend_lots parameter.
REM *   PROCEDURE: revert_step.
REM **********************************************************************

/*************************************************************************
* This file contains the procedure for uncertifying steps in Oracle      *
* Process Manufacturing (OPM). Each procedure has a common set of        *
* parameters to which API-specific parameters are appended.              *
*************************************************************************/

CREATE OR REPLACE PACKAGE gme_revert_step_pvt AS
/* $Header: GMEVRWSS.pls 120.0.12020000.2 2016/11/07 16:53:37 gmurator ship $ */

PROCEDURE revert_step
(p_batch_step_rec      	    IN  GME_BATCH_STEPS%ROWTYPE        
 ,p_batch_header_rec        IN  GME_BATCH_HEADER%ROWTYPE       
 ,p_create_resv_pend_lots   IN  NUMBER  -- Bug 23640627
 ,x_batch_step_rec      	 OUT NOCOPY GME_BATCH_STEPS%ROWTYPE 
 ,x_return_status           OUT NOCOPY VARCHAR2);              


END gme_revert_step_pvt;
/
COMMIT;
EXIT;
--SHOW ERRORS;
