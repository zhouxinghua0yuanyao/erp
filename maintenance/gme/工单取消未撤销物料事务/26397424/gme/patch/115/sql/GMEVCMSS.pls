/* +======================================================================+ */
/* |    Copyright (c) 2005, 2014 Oracle and/or its affiliates.           | */
/* |                         All rights reserved.                         | */
/* |                           Version 12.0.0                             | */
/* +======================================================================+ */
REM dbdrv: sql ~PROD ~PATH ~FILE none none none package &phase=pls \
REM dbdrv: checkfile(120.4.12010000.2=120.4.12020000.2)(120.3.12000000.2=120.4):~PROD:~PATH:~FILE 
SET VERIFY OFF
WHENEVER SQLERROR EXIT FAILURE ROLLBACK;
WHENEVER OSERROR EXIT FAILURE ROLLBACK; 
REM **********************************************************************
REM *                                                                    *
REM * FILE:    GMEVCMSS.pls                                              *
REM * PURPOSE: Package Spec for the GME Complete BATCH STEP routines     *
REM * AUTHOR:  A. Newbury                                                *
REM * DATE:    May 2005                                                  *
REM * HISTORY:                                                           *
REM * ========                                                           *
REM * Swapna K Bug#6348353 29-AUG-2007                                   *
REM *  Added the parameter, p_quality_override to the procedure,complete_step_recursive * 
REM **********************************************************************

/*************************************************************************
* This file contains the procedure for certifying batch steps in Oracle  *
* Process Manufacturing (OPM). Each procedure has a common set of        *
* parameters to which API-specific parameters are appended.              *
*************************************************************************/

CREATE OR REPLACE PACKAGE gme_complete_batch_step_pvt AS
/* $Header: GMEVCMSS.pls 120.4.12020000.2 2014/07/21 07:36:56 shalchen ship $ */

  PROCEDURE complete_step
    (p_batch_step_rec            IN GME_BATCH_STEPS%ROWTYPE
    ,p_batch_header_rec          IN gme_batch_header%ROWTYPE
    ,x_batch_step_rec            OUT NOCOPY GME_BATCH_STEPS%ROWTYPE
    ,x_exception_material_tbl    IN OUT NOCOPY gme_common_pvt.exceptions_tab
    ,x_return_status             OUT NOCOPY VARCHAR2);

  PROCEDURE complete_step_recursive
    (p_batch_step_rec             IN       gme_batch_steps%ROWTYPE
    ,p_batch_header_rec           IN       gme_batch_header%ROWTYPE
    ,x_batch_step_rec             OUT NOCOPY      gme_batch_steps%ROWTYPE
    ,x_exception_material_tbl     IN  OUT NOCOPY gme_common_pvt.exceptions_tab
    ,x_return_status              OUT NOCOPY      VARCHAR2
    , p_quality_override     IN  BOOLEAN := FALSE); --Bug#6348353
    
  PROCEDURE complete_step_line
    (p_batch_step_rec            IN              gme_batch_steps%ROWTYPE
    ,x_batch_step_rec            OUT NOCOPY      gme_batch_steps%ROWTYPE
    ,x_exception_material_tbl    IN  OUT NOCOPY  gme_common_pvt.exceptions_tab
    ,x_return_status             OUT NOCOPY      VARCHAR2);
    
  PROCEDURE complete_step_material
              (p_batch_step_rec             IN         gme_batch_steps%ROWTYPE
              ,p_update_inv_ind             IN         VARCHAR2
              ,x_exception_material_tbl     IN  OUT NOCOPY gme_common_pvt.exceptions_tab
              ,x_return_status              OUT NOCOPY VARCHAR2);  
              
  /*                                    
  ER 19161894  Shaliu Chen 18-JUL-2014               
  Added input parameter p_ignore_exception for OPM Step Level OSP Project 
  If p_ignore_exception value is 'T',ingore warning that there is open 
  PO/Requisition existing which is linked to the step.  
  */                                    
              
  PROCEDURE validate_step_for_complete  (p_batch_header_rec     IN gme_batch_header%ROWTYPE
                                        ,p_batch_step_rec       IN gme_batch_steps%ROWTYPE
                                        ,p_override_quality     IN VARCHAR2
                                        ,p_ignore_exception     IN VARCHAR2 DEFAULT 'F'
                                        ,x_batch_step_rec       OUT NOCOPY gme_batch_steps%ROWTYPE
                                        ,x_return_status        OUT NOCOPY VARCHAR2);
                                        
  PROCEDURE validate_step_cmplt_date 
      (p_batch_step_rec       IN  GME_BATCH_STEPS%ROWTYPE
      ,p_batch_header_rec     IN  GME_BATCH_HEADER%ROWTYPE
      ,x_batch_start_date     OUT NOCOPY DATE
      ,x_return_status        OUT NOCOPY VARCHAR2);

  PROCEDURE validate_dependent_steps (p_batch_id           IN NUMBER
                                     ,p_step_id            IN NUMBER
                                     ,p_step_actual_start_date IN DATE
                                     ,x_return_status      OUT NOCOPY VARCHAR2);

END gme_complete_batch_step_pvt;
/
commit;
exit;
