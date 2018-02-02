/* +======================================================================+ */
/* |    Copyright (c) 2005, 2015 Oracle and/or its affiliates.           | */
/* |                         All rights reserved.                         | */
/* |                           Version 12.0.0                             | */
/* +======================================================================+ */
REM dbdrv: sql ~PROD ~PATH ~FILE none none none package &phase=plb \
REM dbdrv: checkfile(120.2.12010000.2=120.2.12020000.2)(115.8=120.1):~PROD:~PATH:~FILE
SET VERIFY OFF
WHENEVER SQLERROR EXIT FAILURE ROLLBACK;
WHENEVER OSERROR EXIT FAILURE ROLLBACK;

CREATE OR REPLACE PACKAGE BODY gme_eres_utils AS
/* $Header: GMEERESB.pls 120.2.12020000.2 2015/05/06 18:03:35 srpuri ship $ */
/*
REM **********************************************************************
REM *                                                                    *
REM * FILE:    GMEERESB.pls                                              *
REM * PURPOSE: Package Body for the GME ERES_UTILS routines              *
REM *          It contens all the routines to support the ERES output    *
REM *          during XML mapping, used by gateway product.              *
REM * AUTHOR:  Shrikant Nene, OPM Development                            *
REM * DATE:    August 18th 2002                                          *
REM **********************************************************************
* This file contains the procedure for create batch steps in Oracle      *
* Process Manufacturing (OPM). Each procedure has a common set of        *
* parameters to which API-specific parameters are appended.              *
*************************************************************************/
   
   PROCEDURE get_phantom_or_not (p_batch_id IN NUMBER, x_phantom OUT NOCOPY VARCHAR2)
   IS
      CURSOR cur_get_phant IS
       SELECT count(1)
       FROM   gme_material_details
       WHERE  phantom_id = p_batch_id
       AND    ROWNUM     = 1;

      l_exists              NUMBER;
   BEGIN
      OPEN  cur_get_phant;
      FETCH cur_get_phant INTO l_exists;
      CLOSE cur_get_phant;

      IF l_exists > 0 THEN
         x_phantom := fnd_message.get_string('GME','GME_PHANTOM');
      ELSE
         x_phantom := NULL;
      END IF;
   END get_phantom_or_not;

   /* This procedure returns the plant code and batch number for a
      given batch_id */
   PROCEDURE get_batch_number (
      p_batch_id       IN              NUMBER
     ,x_batch_number   OUT NOCOPY      VARCHAR2)
   IS
      CURSOR get_doc_number (v_batch_id IN NUMBER)
      IS
         SELECT organization_code || ' ' || batch_no
         FROM   gme_batch_header_vw 
         WHERE  batch_id = v_batch_id;
   BEGIN
      OPEN  get_doc_number (p_batch_id);
      FETCH get_doc_number INTO x_batch_number;
      CLOSE get_doc_number;
   END get_batch_number;

PROCEDURE GET_LOOKUP_VALUE (plookup_type       IN VARCHAR2,
			    plookup_code       IN VARCHAR2,
                            pmeaning           OUT NOCOPY VARCHAR2) IS

CURSOR get_lookup IS
  SELECT meaning
  FROM fnd_lookup_values_vl
  WHERE  lookup_type = plookup_type
  AND    lookup_code = plookup_code;

BEGIN

  OPEN get_lookup;
  FETCH get_lookup INTO pmeaning;
  IF (get_lookup%NOTFOUND) THEN
     pmeaning := ' ';
  END IF;
  CLOSE get_lookup;

END GET_LOOKUP_VALUE;

END gme_eres_utils;                            
/

COMMIT ;
EXIT;

