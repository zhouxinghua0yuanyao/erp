/* +======================================================================+ */
/* |    Copyright (c) 2005, 2016 Oracle and/or its affiliates.           | */
/* |                         All rights reserved.                         | */
/* |                           Version 12.0.0                             | */
/* +======================================================================+ */
REM dbdrv: sql ~PROD ~PATH ~FILE none none none package &phase=pls \
REM dbdrv: checkfile(120.1.12010000.2=120.1.12020000.2)(115.11=120.1):~PROD:~PATH:~FILE
SET VERIFY OFF
WHENEVER SQLERROR EXIT FAILURE ROLLBACK;
WHENEVER OSERROR EXIT FAILURE ROLLBACK;
REM **********************************************************************
REM *                                                                    *
REM * FILE:    GMEVRSBS.pls                                              *
REM * PURPOSE: Package Spec for the GME RESCHEDULE BATCH routines        *
REM * AUTHOR:  Navin Kumar Sinha, OPM Development                        *
REM * DATE:    March 02nd 2005                                           *
REM * HISTORY:                                                           *
REM * ========                                                           *
REM * 02MAR05  Navin Kumar Sinha                                         *
REM *          Created                                                   *
REM * Punit Kumar 25 Mar 2005 Convergence changes                        *
REM **********************************************************************

/*************************************************************************
* This file contains the procedure for reschedule batch in Oracle        *
* Process Manufacturing (OPM). Each procedure has a common set of        *
* parameters to which API-specific parameters are appended.              *
*************************************************************************/

CREATE OR REPLACE PACKAGE gme_reschedule_batch_pvt AS
/* $Header: GMEVRSBS.pls 120.1.12020000.2 2016/06/06 14:50:13 gmurator ship $ */
   PROCEDURE reschedule_batch (
      p_batch_header_rec      IN              gme_batch_header%ROWTYPE
     ,p_use_workday_cal       IN              VARCHAR2
     ,p_contiguity_override   IN              VARCHAR2
     ,p_called_by             IN              VARCHAR2 DEFAULT 'NORMAL' -- Bug  23026619 - This is specific scaling a wip batch.                                        
     ,x_batch_header_rec      OUT NOCOPY      gme_batch_header%ROWTYPE
     ,x_return_status         OUT NOCOPY      VARCHAR2);

   PROCEDURE truncate_date (
      p_batch_header_rec   IN              gme_batch_header%ROWTYPE
     ,p_date               IN              NUMBER
     ,p_batchstep_id       IN              gme_batch_steps.batchstep_id%TYPE
            DEFAULT NULL
     ,x_return_status      OUT NOCOPY      VARCHAR2);
END gme_reschedule_batch_pvt;
/

COMMIT ;
EXIT;
--SHOW errors;
