/* +======================================================================+ */
/* |    Copyright (c) 2005, 2015 Oracle and/or its affiliates.           | */
/* |                         All rights reserved.                         | */
/* |                           Version 12.0.0                             | */
/* +======================================================================+ */
REM dbdrv: sql ~PROD ~PATH ~FILE none none none package &phase=pls \
REM dbdrv: checkfile(120.2.12010000.2=120.2.12020000.2)(115.4=120.1):~PROD:~PATH:~FILE
SET VERIFY OFF
WHENEVER SQLERROR EXIT FAILURE ROLLBACK;
WHENEVER OSERROR EXIT FAILURE ROLLBACK;

CREATE OR REPLACE PACKAGE GME_ERES_UTILS AS
/*  $Header: GMEERESS.pls 120.2.12020000.2 2015/05/06 18:02:33 srpuri ship $ */
/*
REM **********************************************************************
REM *                                                                    *
REM * FILE:    GMEERESS.pls                                              *
REM * PURPOSE: Package specification for the GME ERES_UTILS routines     *
REM *          It contens all the routines to support the ERES output    *
REM *          during XML mapping, used by gateway product.              *
REM * AUTHOR:  Shrikant Nene, OPM Development                            *
REM * DATE:    August 18th 2002                                          *
REM * HISTORY:                                                           *
REM * ========                                                           *
REM *********************************************************************
* This file contains the procedure for create batch steps in Oracle      *
* Process Manufacturing (OPM). Each procedure has a common set of        *
* parameters to which API-specific parameters are appended.              *
*************************************************************************/
   PROCEDURE get_batch_number (
      p_batch_id       IN       NUMBER,
      x_batch_number   OUT  NOCOPY    VARCHAR2
   );

   PROCEDURE get_phantom_or_not (p_batch_id       IN       NUMBER,
                                 x_phantom        OUT  NOCOPY    VARCHAR2);


PROCEDURE GET_LOOKUP_VALUE
(    plookup_type       IN VARCHAR2
   , plookup_code       IN VARCHAR2
   , pmeaning           OUT NOCOPY VARCHAR2
) ;

END GME_ERES_UTILS; -- Package Specification GME_ERES_UTILS

/
COMMIT;
EXIT;

