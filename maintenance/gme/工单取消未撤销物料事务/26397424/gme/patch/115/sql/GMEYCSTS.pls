/* +======================================================================+ */
/* |    Copyright (c) 2005, 2017 Oracle and/or its affiliates.           | */
/* |                         All rights reserved.                         | */
/* |                           Version 12.0.0                             | */
/* +======================================================================+ */
REM dbdrv: sql ~PROD ~PATH ~FILE none none none package &phase=pls \
REM dbdrv: checkfile(120.0.12010000.4=120.0.12020000.4)(115.2=120.1):~PROD:~PATH:~FILE

create or replace package GME_YIELD_CUSTOMIZATION_PVT as
/* $Header: GMEYCSTS.pls 120.0.12020000.4 2017/05/22 07:51:41 shalchen noship $ */

  -- Author  : SHALCHEN
  -- Created : 2016/9/2 11:08:18
  -- Purpose : 
  
  PROCEDURE get_lot_yield_hook(
    p_batch_id                IN NUMBER,
    p_organization_id         IN NUMBER,
    p_subinventory_code       IN VARCHAR2,
    p_locator_id              IN NUMBER,
    p_inventory_item_id       IN NUMBER,
    p_lot_number              IN VARCHAR2,
    x_actual_total_cum_yield  OUT NOCOPY NUMBER,
    x_scrap_total_cum_pct     OUT NOCOPY NUMBER,
    x_return_status           OUT NOCOPY VARCHAR2);
    
    
  -- heat generation function
  FUNCTION generate_heat_number(
      p_batch_id IN NUMBER DEFAULT NULL )
    RETURN VARCHAR2;    
    
  --BUG 26027932
  --add hook to check whether heat number can be duplicated.  
  Function is_duplicate_heat_allowed(p_organization_code IN  VARCHAR2,
                                     p_batch_no          IN  VARCHAR2,
                                     p_heat_number       IN  VARCHAR2)
  RETURN BOOLEAN;    

end GME_YIELD_CUSTOMIZATION_PVT;
/

COMMIT;
EXIT;
/
