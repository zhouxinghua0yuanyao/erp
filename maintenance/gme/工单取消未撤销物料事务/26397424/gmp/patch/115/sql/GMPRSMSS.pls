REM dbdrv: sql ~PROD ~PATH ~FILE none none none package &phase=pls  \
REM dbdrv: checkfile(120.0.12010000.3=120.4)(115.6=120.0):~PROD:~PATH:~FILE
/*=======================================================================+
 |  Copyright (c) 1997, 2014 Oracle Corporation Redwood Shores, California, USA|
 |                            All rights reserved.                       |
 +=======================================================================+
 | FILENAME
 | DESCRIPTION
 |   PL/SQL spec for package:  CR_RSRC_MST_PKG
 |   Sridhar Gidugu 02-JAN-2002 Added Utilization and Efficiency Columns 
 |   Kaushek B      01-APR-2008	BUG6922201 replaced X_STD_USAGE_UM with X_STD_USAGE_UOM
 *=======================================================================*/
SET VERIFY OFF;
WHENEVER OSERROR EXIT FAILURE ROLLBACK;
WHENEVER SQLERROR EXIT FAILURE ROLLBACK;

create or replace package CR_RSRC_MST_PKG as
/* $Header: GMPRSMSS.pls 120.4.12020000.2 2014/07/21 02:45:07 biachen ship $ */
-- ER 19161894, OSP, add OUTSIDE_PROCESS_IND
procedure INSERT_ROW (
  X_ROWID in out NOCOPY VARCHAR2,
  X_RESOURCES in VARCHAR2,
  X_RESOURCE_CLASS in VARCHAR2,
  X_TRANS_CNT in NUMBER,
  X_DELETE_MARK in NUMBER,
  X_TEXT_CODE in NUMBER,
  X_MIN_CAPACITY in NUMBER,
  X_MAX_CAPACITY in NUMBER,
  X_CAPACITY_CONSTRAINT in NUMBER,
  X_CAPACITY_UOM in VARCHAR2,
  X_STD_USAGE_UM in VARCHAR2,
  X_COST_CMPNTCLS_ID in NUMBER,
  X_RESOURCE_DESC in VARCHAR2,
  X_CREATION_DATE in DATE,
  X_CREATED_BY in NUMBER,
  X_LAST_UPDATE_DATE in DATE,
  X_LAST_UPDATED_BY in NUMBER,
  X_LAST_UPDATE_LOGIN in NUMBER,
  X_CAPACITY_TOLERANCE IN NUMBER,
  X_UTILIZATION in NUMBER,
  X_EFFICIENCY in NUMBER,
  X_OUTSIDE_PROCESS_IND in NUMBER default 0
);
procedure LOCK_ROW (
  X_RESOURCES in VARCHAR2,
  X_RESOURCE_CLASS in VARCHAR2,
  X_TRANS_CNT in NUMBER,
  X_DELETE_MARK in NUMBER,
  X_TEXT_CODE in NUMBER,
  X_MIN_CAPACITY in NUMBER,
  X_MAX_CAPACITY in NUMBER,
  X_CAPACITY_CONSTRAINT in NUMBER,
  X_CAPACITY_UOM in VARCHAR2,
  X_STD_USAGE_UM in VARCHAR2,
  X_COST_CMPNTCLS_ID in NUMBER,
  X_RESOURCE_DESC in VARCHAR2 ,
  X_CAPACITY_TOLERANCE IN NUMBER, 
  X_UTILIZATION in NUMBER,
  X_EFFICIENCY in NUMBER,
  X_OUTSIDE_PROCESS_IND in NUMBER default 0  
);
procedure UPDATE_ROW (
  X_RESOURCES in VARCHAR2,
  X_RESOURCE_CLASS in VARCHAR2,
  X_TRANS_CNT in NUMBER,
  X_DELETE_MARK in NUMBER,
  X_TEXT_CODE in NUMBER,
  X_MIN_CAPACITY in NUMBER,
  X_MAX_CAPACITY in NUMBER,
  X_CAPACITY_CONSTRAINT in NUMBER,
  X_CAPACITY_UOM in VARCHAR2,
  X_STD_USAGE_UM in VARCHAR2,
  X_COST_CMPNTCLS_ID in NUMBER,
  X_RESOURCE_DESC in VARCHAR2,
  X_LAST_UPDATE_DATE in DATE,
  X_LAST_UPDATED_BY in NUMBER,
  X_LAST_UPDATE_LOGIN in NUMBER,
  X_CAPACITY_TOLERANCE in NUMBER,
  X_UTILIZATION in NUMBER,
  X_EFFICIENCY in NUMBER,
  X_OUTSIDE_PROCESS_IND in NUMBER default 0
);
procedure DELETE_ROW (
  X_RESOURCES in VARCHAR2
);
procedure ADD_LANGUAGE;
end CR_RSRC_MST_PKG;
/
--show errors
commit;
exit;
