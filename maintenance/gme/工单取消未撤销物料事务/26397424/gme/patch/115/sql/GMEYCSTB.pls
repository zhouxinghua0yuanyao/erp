/* +======================================================================+ */
/* |    Copyright (c) 2005, 2017 Oracle and/or its affiliates.           | */
/* |                         All rights reserved.                         | */
/* |                           Version 12.0.0                             | */
/* +======================================================================+ */
REM dbdrv: sql ~PROD ~PATH ~FILE none none none package &phase=plb \
REM dbdrv: checkfile(120.0.12010000.4=120.0.12020000.4)(115.2=120.1):~PROD:~PATH:~FILE

create or replace package body GME_YIELD_CUSTOMIZATION_PVT as
/* $Header: GMEYCSTB.pls 120.0.12020000.4 2017/05/22 07:51:03 shalchen noship $ */

  g_debug               VARCHAR2 (5)  := NVL(fnd_profile.VALUE ('AFLOG_LEVEL'),-1);
  g_pkg_name   CONSTANT VARCHAR2 (30) := 'GME_BATCH_ACTION_OPEN_INTF';


/*###############################################################
  # name
  # get_lot_yield_hook
  # synopsis
  #   PROCEDURE get_lot_yield_hook
  # description hook for lot yield, customer can implement this
  # procedure to get lot yield according to their business rule.
  # output parameter x_actual_total_cum_yield and x_scrap_total_cum_pct
  # must return -1 if the procedure can not get correct lot yield.
  # created by
  ###############################################################*/
  PROCEDURE get_lot_yield_hook(
    p_batch_id                IN NUMBER,
    p_organization_id         IN NUMBER,
    p_subinventory_code       IN VARCHAR2,
    p_locator_id              IN NUMBER,
    p_inventory_item_id       IN NUMBER,
    p_lot_number              IN VARCHAR2,
    x_actual_total_cum_yield  OUT NOCOPY NUMBER,
    x_scrap_total_cum_pct     OUT NOCOPY NUMBER,
    x_return_status           OUT NOCOPY VARCHAR2)
  IS
    l_lot_yield VARCHAR2 (100) ;
    l_api_name   CONSTANT VARCHAR2 (30) := 'get_lot_yield_hook';
  BEGIN
    IF g_debug <= gme_debug.g_log_procedure THEN
      gme_debug.put_line ('Entering api ' || g_pkg_name || '.'|| l_api_name);
    END IF;

    -- Custom User Hook Please Write your code here
    /*
    Place for Custom Logic
    */
    x_actual_total_cum_yield := -1;
    x_scrap_total_cum_pct := -1;
    x_return_status := fnd_api.g_ret_sts_error;
    IF g_debug <= gme_debug.g_log_procedure THEN
      gme_debug.put_line ('Exiting api ' || g_pkg_name || '.'|| l_api_name);
    END IF;    
  EXCEPTION
  WHEN OTHERS THEN
    x_actual_total_cum_yield := -1;
    x_scrap_total_cum_pct := -1;
    IF g_debug <= gme_debug.g_log_unexpected THEN
      gme_debug.put_line (   'When others exception in '
                          || g_pkg_name
                          || '.'
                          || l_api_name
                          || ' Error is '
                          || SQLERRM);
    END IF;
  END get_lot_yield_hook;

  
  
  
  /*###############################################################
# name
# generate_heat_number
# synopsis
# func  generate_heat_number
# description
#  generate heat number function , as such there is no logic for that
# created by may chen on 09/09/2016
###############################################################*/
  FUNCTION generate_heat_number(
      p_batch_id IN NUMBER DEFAULT NULL)
    RETURN VARCHAR2
  IS
    l_heat_number VARCHAR2 (100) ;
    l_api_name   CONSTANT VARCHAR2 (30) := 'generate_heat_number';
  BEGIN
    IF g_debug <= gme_debug.g_log_procedure THEN
      gme_debug.put_line ('Entering api ' || g_pkg_name || '.'|| l_api_name);
    END IF;
    -- Custom User Hook Please Write your code here
    /*
    Place for Custom Logic
    */
    RETURN l_heat_number;
    IF g_debug <= gme_debug.g_log_procedure THEN
      gme_debug.put_line ('Exiting api ' || g_pkg_name || '.'|| l_api_name);
    END IF; 
  EXCEPTION
  WHEN OTHERS THEN
    IF g_debug <= gme_debug.g_log_unexpected THEN
      gme_debug.put_line (   'When others exception in '
                          || g_pkg_name
                          || '.'
                          || l_api_name
                          || ' Error is '
                          || SQLERRM);
    END IF;
    RETURN l_heat_number;
  END generate_heat_number;
  
  
  
  /*###############################################################
# name
# is_duplicate_heat_allowed
# synopsis
# func  is_duplicate_heat_allowed
# p_organization_code - organization code
# p_batch_no  - batch no
# p_heat_number - heat number
# description
#  This is the custom hook to derive whether the heat number 
#  can be duplicated among different batches.
# created by shaliu chen on 05/09/2017
###############################################################*/  
  Function is_duplicate_heat_allowed(p_organization_code IN  VARCHAR2,
                                     p_batch_no          IN  VARCHAR2,
                                     p_heat_number       IN  VARCHAR2)
  RETURN BOOLEAN
  IS
  l_return     BOOLEAN := FALSE;
  l_api_name   CONSTANT VARCHAR2 (30) := 'is_duplicate_heat_allowed';
  BEGIN
    IF g_debug <= gme_debug.g_log_procedure THEN
      gme_debug.put_line ('Entering api ' || g_pkg_name || '.'|| l_api_name);
    END IF;
    -- Custom User Hook Please Write your code here
    /*
    Place for Custom Logic
    */    
    RETURN l_return;
    IF g_debug <= gme_debug.g_log_procedure THEN
      gme_debug.put_line ('Exiting api ' || g_pkg_name || '.'|| l_api_name);
    END IF;     
  EXCEPTION
  WHEN OTHERS THEN
    IF g_debug <= gme_debug.g_log_unexpected THEN
      gme_debug.put_line (   'When others exception in '
                          || g_pkg_name
                          || '.'
                          || l_api_name
                          || ' Error is '
                          || SQLERRM);
    END IF;
    RETURN l_return;
  END is_duplicate_heat_allowed;  


end GME_YIELD_CUSTOMIZATION_PVT;
/

COMMIT;
EXIT;
/
