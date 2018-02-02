REM dbdrv: sql ~PROD ~PATH ~FILE none none none package &phase=pls \
REM dbdrv: checkfile(120.0.12010000.5=120.0.12020000.5)(115.2=120.1):~PROD:~PATH:~FILE
SET VERIFY OFF
WHENEVER SQLERROR EXIT FAILURE ROLLBACK;
WHENEVER OSERROR EXIT FAILURE ROLLBACK;
REM ***************************************************************************
REM *                                                                         *
REM * FILE:    GMEBTOIS.pls                                                   *
REM * PURPOSE: Package Specification for the GME BATCH OPEN INTERFACE routines*
REM * AUTHOR:  QZENG, OPM Development                                         *
REM * DATE:    March 08 2013                                                  *
REM * HISTORY:                                                                *
REM *  QZENG   11-Apr-2013 Bug#16457668                                       *
REM *   Added code to support create and release batch for load_type 15       *
REM *  QZENG   22-May-2013 Bug#16457668                                       *
REM *   Added code to support new process_status successful with warning      *
REM * ============                                                            *
REM ***************************************************************************
create or replace PACKAGE GME_BATCH_OPEN_INTERFACE AS
/* $Header: GMEBTOIS.pls 120.0.12020000.5 2013/05/29 08:49:44 mtou noship $ */
LOADTYPE_HEADER_RECIPE                CONSTANT NUMBER       := 10;
LOADTYPE_HEADER_RELEASE               CONSTANT NUMBER       := 15;
LOADTYPE_HEADER_DETAIL                CONSTANT NUMBER       := 20;
LOADTYPE_HEADER_UPDATE                CONSTANT NUMBER       := 30;
LOADTYPE_HEADER_RESCHEDULE            CONSTANT NUMBER       := 40;
LOADTYPE_HEADER_REROUTE               CONSTANT NUMBER       := 50;
                                                            
LOADTYPE_DETAIL_ADD                   CONSTANT NUMBER       := 10;
LOADTYPE_DETAIL_UPDATE                CONSTANT NUMBER       := 20;
LOADTYPE_DETAIL_DELETE                CONSTANT NUMBER       := 30;
LOADTYPE_DETAIL_RESHCEDULE            CONSTANT NUMBER       := 40;
                                                            
OBJECTYPE_BATCH                       CONSTANT NUMBER       := 100;
OBJECTYPE_MATERIAL                    CONSTANT NUMBER       := 200;
OBJECTYPE_STEP                        CONSTANT NUMBER       := 300;
OBJECTYPE_ACTIVITY                    CONSTANT NUMBER       := 400;
OBJECTYPE_RESOURCE                    CONSTANT NUMBER       := 500;
OBJECTYPE_STEPITEM                    CONSTANT NUMBER       := 600;
OBJECTYPE_PROCESSPARAMETER            CONSTANT NUMBER       := 700;
OBJECTYPE_RESOURCETXN                 CONSTANT NUMBER       := 800;
                                                            
CREATION_MODE_RECIPE                  CONSTANT NUMBER       := 10;
CREATION_MODE_PRODUCT                 CONSTANT NUMBER       := 20;
CREATION_MODE_OUTPUT                  CONSTANT NUMBER       := 30;
CREATION_MODE_INPUT                   CONSTANT NUMBER       := 40;

ACTION_RELEASE                        CONSTANT NUMBER       := 10;
ACTION_COMPLETE                       CONSTANT NUMBER       := 20;
ACTION_CLOSE                          CONSTANT NUMBER       := 30;
ACTION_REOPEN                         CONSTANT NUMBER       := 40;
ACTION_REVERT                         CONSTANT NUMBER       := 50;
ACTION_UNRELEASE                      CONSTANT NUMBER       := 60;
ACTION_TERMINATE                      CONSTANT NUMBER       := 70;
ACTION_CANCEL                         CONSTANT NUMBER       := 80; 
                                                            
PROCESS_STATUS_PENDING                CONSTANT NUMBER       := 1;
PROCESS_STATUS_RUNNING                CONSTANT NUMBER       := 2;
PROCESS_STATUS_FAILED                 CONSTANT NUMBER       := 3;
PROCESS_STATUS_WARNING                CONSTANT NUMBER       := 5;
PROCESS_STATUS_SUCCEED                CONSTANT NUMBER       := 7;
PROCESS_STATUS_BV_INVALID             CONSTANT NUMBER       := 255;
PROCESS_STATUS_BV_WARNING             CONSTANT NUMBER       := 511;

MESSAGE_TYPE_WARNING                  CONSTANT VARCHAR2(1)  := 'W';
MESSAGE_TYPE_CRITICAL                 CONSTANT VARCHAR2(1)  := 'C';
MESSAGE_TYPE_ERROR                    CONSTANT VARCHAR2(1)  := 'E';


 
PURGE_OPTION_ALL_ROWS                 CONSTANT NUMBER := 0; -- purge all rows
PURGE_OPTION_SUCCESSFUL_ROWS          CONSTANT NUMBER := 1; --purge successful rows
PURGE_OPTION_PENDING_ROWS             CONSTANT NUMBER := 2; --purge pending rows
PURGE_OPTION_WARNING_ROWS             CONSTANT NUMBER := 3; --purge warning rows
PURGE_OPTION_ERROR_ROWS               CONSTANT NUMBER := 4; --purge error rows


ALL_TABLE_NAME_CODE                   CONSTANT NUMBER := 0; --purge all below interface table
GME_BTCH_INTF_CODE                    CONSTANT NUMBER := 1; --purge interface table   GME_BATCH_HEADER_INTERFACE and GME_BATCH_DTLS_INTERFACE
GME_ACTIONS_INTF_CODE                 CONSTANT NUMBER := 2; --purge interface table    GME_BATCH_ACTIONS_INTERFACE
GME_RESOURCE_TXNS_INTF_CODE           CONSTANT NUMBER := 3; -- purge interface table   GME_RESOURCE_TXNS_INTERFACE
 
--Cursor to fetch batch dtl material from interface table to handle
CURSOR CUR_BATCH_DTL_MATERIAL(v_group_id NUMBER) IS
SELECT interface_header_id, interface_id, organization_code, batch_no, item, revision, line_no, line_type, int_organization_id,
       int_batch_id, int_material_detail_id, actual_qty, by_product_type, contribute_step_qty_ind, contribute_yield_ind,
       cost_alloc, dtl_um, int_inventory_item_id, int_locator_id, material_requirement_date, original_primary_qty, 
       original_qty, phantom_type, plan_qty, release_type, rounding_direction, 
       scale_multiple, scale_rounding_variance, scale_type, scrap_factor, subinventory, wip_plan_qty, attribute_category, 
       load_type, attribute1, attribute2, attribute3, attribute4, attribute5, attribute6, attribute7, attribute8, attribute9,
       attribute10, attribute11, attribute12, attribute13, attribute14, attribute15, attribute16, attribute17, attribute18,
       attribute19, attribute20, attribute21, attribute22, attribute23, attribute24, attribute25, attribute26, attribute27,
       attribute28, attribute29, attribute30
FROM   gme_batch_dtls_interface
WHERE  group_id=v_group_id
AND    object_type = OBJECTYPE_MATERIAL
AND    process_status = PROCESS_STATUS_RUNNING
ORDER BY int_organization_id, int_batch_id, load_type, int_material_detail_id, line_type, line_no;

--Cursor to fetch batch dtl step from interface table to handle
CURSOR CUR_BATCH_DTL_STEP(v_group_id NUMBER) IS
SELECT interface_header_id, interface_id, organization_code, batch_no, int_organization_id, int_batch_id, batchstep_no, 
       int_batchstep_id, actual_start_date, actual_cmplt_date, actual_step_qty, due_date, int_oprn_id, contiguity_override,
       plan_cmplt_date, plan_start_date, plan_step_qty, step_close_date, steprelease_type, reschedule_preceding_steps,
       reschedule_succeeding_steps, use_work_day_cal, attribute_category, load_type, attribute1, attribute2, attribute3, 
       attribute4, attribute5, attribute6, attribute7, attribute8, attribute9, attribute10, attribute11, attribute12, 
       attribute13, attribute14, attribute15, attribute16, attribute17, attribute18, attribute19, attribute20, attribute21, 
       attribute22, attribute23, attribute24, attribute25, attribute26, attribute27, attribute28, attribute29, attribute30
FROM   gme_batch_dtls_interface
WHERE  group_id=v_group_id
AND    object_type = OBJECTYPE_STEP
AND    process_status = PROCESS_STATUS_RUNNING
ORDER BY int_organization_id, int_batch_id, load_type;
  
--Cursor to fetch batch dtl activity from interface table to handle
CURSOR CUR_BATCH_DTL_ACTIVITY(v_group_id NUMBER) IS
SELECT interface_header_id, interface_id, organization_code, batch_no, int_organization_id, int_batch_id, activity,
       int_batchstep_id, int_batchstep_activity_id, actual_activity_factor, actual_cmplt_date, actual_start_date, 
       offset_interval, plan_cmplt_date, plan_start_date, plan_activity_factor, sequence_dependent_ind, attribute_category, 
       load_type, attribute1, attribute2, attribute3, attribute4, attribute5, attribute6, attribute7, attribute8, attribute9, 
       attribute10, attribute11, attribute12, attribute13, attribute14, attribute15, attribute16, attribute17, attribute18, 
       attribute19, attribute20, attribute21, attribute22, attribute23, attribute24, attribute25, attribute26, attribute27, 
       attribute28, attribute29, attribute30
FROM   gme_batch_dtls_interface
WHERE  group_id=v_group_id
AND    object_type = OBJECTYPE_ACTIVITY
AND    process_status = PROCESS_STATUS_RUNNING
ORDER BY int_organization_id, int_batch_id, int_batchstep_id, load_type;

--Cursor to fetch batch dtl resource from interface table to handle
CURSOR CUR_BATCH_DTL_RESOURCE(v_group_id NUMBER, v_batchstep_id NUMBER, v_activity VARCHAR2) IS
SELECT interface_header_id, interface_id, organization_code, batch_no, int_organization_id, int_batch_id, resources, 
       activity, int_batchstep_id, int_batchstep_activity_id, int_batchstep_resource_id, actual_cmplt_date, actual_rsrc_count, 
       actual_rsrc_qty, actual_rsrc_usage, actual_start_date, original_rsrc_qty, original_rsrc_usage, offset_interval, 
       plan_cmplt_date, plan_start_date, plan_rsrc_count, plan_rsrc_qty, plan_rsrc_usage, prim_rsrc_ind, resource_qty_um, 
       scale_type, cost_analysis_code, int_cost_cmpntcls_id, attribute_category, load_type, attribute1, attribute2, attribute3, 
       attribute4, attribute5, attribute6, attribute7, attribute8, attribute9, attribute10, attribute11, attribute12, attribute13, 
       attribute14, attribute15, attribute16, attribute17, attribute18, attribute19, attribute20, attribute21, attribute22, attribute23, 
       attribute24, attribute25, attribute26, attribute27, attribute28, attribute29, attribute30
FROM   gme_batch_dtls_interface
WHERE  group_id=v_group_id
AND    ((int_batchstep_id = v_batchstep_id 
         AND activity = v_activity 
         AND v_batchstep_id IS NOT NULL 
         AND v_activity IS NOT NULL
         AND load_type = LOADTYPE_DETAIL_ADD)
         OR  v_batchstep_id IS NULL)
AND    object_type = OBJECTYPE_RESOURCE
AND    process_status = PROCESS_STATUS_RUNNING
ORDER BY int_organization_id, int_batch_id, int_batchstep_id, int_batchstep_activity_id, load_type;
  
--Cursor to fetch batch dtl step item association from interface table to handle
CURSOR CUR_BATCH_DTL_STEPITEM(v_group_id NUMBER) IS
SELECT interface_header_id, interface_id, organization_code, batch_no, int_organization_id, int_batch_id, 
       int_material_detail_id, int_batchstep_id, attribute_category, 
       load_type, attribute1, attribute2, attribute3, attribute4, attribute5, attribute6, attribute7, attribute8, 
       attribute9, attribute10, attribute11, attribute12, attribute13, attribute14, attribute15, attribute16, attribute17, 
       attribute18, attribute19, attribute20, attribute21, attribute22, attribute23, attribute24, attribute25, attribute26, 
       attribute27, attribute28, attribute29, attribute30
FROM   gme_batch_dtls_interface
WHERE  group_id=v_group_id
AND    object_type = OBJECTYPE_STEPITEM
AND    process_status = PROCESS_STATUS_RUNNING
ORDER BY int_organization_id, int_batch_id, int_material_detail_id, load_type;

--Cursor to fetch batch dtl parameter from interface table to handle
CURSOR CUR_BATCH_DTL_PARAMETER(v_group_id NUMBER) IS
SELECT interface_header_id, interface_id, organization_code, batch_no, int_organization_id, int_batch_id, int_parameter_id,
       int_batchstep_id, int_batchstep_activity_id, int_batchstep_resource_id, int_process_param_id, process_parameter,
       target_value, actual_value, attribute_category, load_type, attribute1, attribute2, attribute3, attribute4, attribute5, 
       attribute6, attribute7, attribute8, attribute9, attribute10, attribute11, attribute12, attribute13, attribute14, 
       attribute15, attribute16, attribute17, attribute18, attribute19, attribute20, attribute21, attribute22, attribute23, 
       attribute24, attribute25, attribute26, attribute27, attribute28, attribute29, attribute30
FROM   gme_batch_dtls_interface
WHERE  group_id=v_group_id
AND    object_type = OBJECTYPE_PROCESSPARAMETER
AND    process_status = PROCESS_STATUS_RUNNING
ORDER BY int_organization_id, int_batch_id, int_batchstep_id, int_batchstep_activity_id, int_batchstep_resource_id, int_process_param_id, load_type;

TYPE batch_header_tbl    IS TABLE OF gme_batch_header_interface%ROWTYPE INDEX BY BINARY_INTEGER;
TYPE batch_material_tbl  IS TABLE OF CUR_BATCH_DTL_MATERIAL%ROWTYPE     INDEX BY BINARY_INTEGER;
TYPE batch_step_tbl      IS TABLE OF CUR_BATCH_DTL_STEP%ROWTYPE         INDEX BY BINARY_INTEGER;
TYPE batch_activity_tbl  IS TABLE OF CUR_BATCH_DTL_ACTIVITY%ROWTYPE     INDEX BY BINARY_INTEGER;
TYPE batch_resource_tbl  IS TABLE OF CUR_BATCH_DTL_RESOURCE%ROWTYPE     INDEX BY BINARY_INTEGER;
TYPE batch_step_item_tbl IS TABLE OF CUR_BATCH_DTL_STEPITEM%ROWTYPE     INDEX BY BINARY_INTEGER;
TYPE batch_parameter_tbl IS TABLE OF CUR_BATCH_DTL_PARAMETER%ROWTYPE    INDEX BY BINARY_INTEGER;
TYPE ARRAY IS TABLE OF VARCHAR2(4000);
TYPE T_MESSAGE_REC IS RECORD
     (param  VARCHAR2(30),
      token  VARCHAR2(60));
TYPE T_MESSAGE_TABLE IS TABLE OF T_MESSAGE_REC INDEX BY BINARY_INTEGER;
/*================================================================================
  Procedure
    process_batches
  Description
    This procedure import batches that stored in table gme_batch_header_interface,
    and make changes to details, like material, step, activity, resource, process
    parameter and step item association
  Parameters
    err_buf            error message to concurrent program manager
    ret_code           error code to concurrent program manager
    p_group_id         group id that specifies rows of batches and details
    p_purge_flag       delete or not the successful rows in interface tables
    p_ignore_exception when load_type is 15, used in release batch 
  History
    QZENG    11-Apr-2013 Bug#16457668
      Added new parameter p_ignore_exception to support create and release batch 
      for load_type 15
================================================================================*/
PROCEDURE process_batches(
                           err_buf            OUT NOCOPY VARCHAR2,
                           ret_code           OUT NOCOPY VARCHAR2,
                           p_group_id         IN         NUMBER,
                           p_purge_flag       IN         VARCHAR2 DEFAULT 'Y',
                           p_ignore_exception IN         VARCHAR2 DEFAULT 'N');
						  
/*
  Description:
  This procedure is used to purge interface table, calling by concurrent program  .
  parameter p_concurrent_prg_code and p_purge_option are used to decide how to purge table.
  
  parameter specification: 
    p_concurrent_prg_code = 0 purge all below interface table
    p_concurrent_prg_code = 1  purge interface table   GME_BATCH_HEADER_INTERFACE and GME_BATCH_DTLS_INTERFACE
    p_concurrent_prg_code = 2 purge interface table    GME_BATCH_ACTIONS_INTERFACE
    p_concurrent_prg_code = 3  purge interface table   GME_RESOURCE_TXNS_INTERFACE
    note: table GME_INTF_ERRORS will be purged after purge interface table,
      when purge interface table, table GME_INTF_ERRORS will be purged according to parameter p_purge_option
    p_purge_option = 0 , purge all rows
    p_purge_option = 1 , purge successful rows
    p_purge_option = 2 , purge pending rows
    p_purge_option = 3 , purge warning rows
    p_purge_option = 4 , purge error table
 

*/
PROCEDURE purge_interface_tables(err_buf           OUT NOCOPY VARCHAR2,
                                 ret_code          OUT NOCOPY VARCHAR2, 
                                 p_group_id        IN NUMBER,
                                 p_concurrent_prg_code IN NUMBER DEFAULT 0,
                                 p_purge_option    IN NUMBER DEFAULT 0);  
								 
/*================================================================================
  Procedure
    gme_log_interface_err
  Description
    This procedure insert error message to table gme_intf_errors
  Parameters
    P_GROUP_ID          group id that specifies rows of batches and details
    P_object_type       object type that failed
    P_INTERFACE_ID      interface id that failed, gme_batch_header_interface
                        uses interface_header_id
    P_COLUMN_NAME       column name that failed
    P_MESSAGE_TYPE      message type, E Error, W Warning, C Critical
    P_MESSAGE_TEXT      message text, should be translated
    P_SOURCE_TABLE_NAME table name that failed
    P_PARAM1-P_PARAM6   addtional message parameters
    P_TOKEN1-P_TOKEN6   addtional message fields
  Retrun
    0  succeed to insert the message, others failed to insert the message
  History
================================================================================*/
FUNCTION  gme_log_interface_err (P_GROUP_ID             NUMBER,
                                 P_object_type          VARCHAR2,
                                 P_INTERFACE_ID         NUMBER,
                                 P_APP_NAME             VARCHAR2 DEFAULT NULL,
                                 P_COLUMN_NAME          VARCHAR2 DEFAULT NULL,
                                 P_MESSAGE_TYPE         VARCHAR2 DEFAULT MESSAGE_TYPE_ERROR,
                                 P_MESSAGE_NAME         VARCHAR2,
                                 P_MESSAGE_TEXT         VARCHAR2 DEFAULT NULL,
                                 P_SOURCE_TABLE_NAME    VARCHAR2,
                                 P_PARAM1               VARCHAR2 DEFAULT NULL,
                                 P_PARAM2               VARCHAR2 DEFAULT NULL,
                                 P_PARAM3               VARCHAR2 DEFAULT NULL,
                                 P_PARAM4               VARCHAR2 DEFAULT NULL,
                                 P_PARAM5               VARCHAR2 DEFAULT NULL,
                                 P_PARAM6               VARCHAR2 DEFAULT NULL,
                                 P_TOKEN1               VARCHAR2 DEFAULT NULL,
                                 P_TOKEN2               VARCHAR2 DEFAULT NULL,
                                 P_TOKEN3               VARCHAR2 DEFAULT NULL,
                                 P_TOKEN4               VARCHAR2 DEFAULT NULL,
                                 P_TOKEN5               VARCHAR2 DEFAULT NULL,
                                 P_TOKEN6               VARCHAR2 DEFAULT NULL)   
RETURN INTEGER;

/*================================================================================
  Procedure
    get_error_message_infor
  Description
    This procedure gets error application name, message name, message tokens and 
    values.
	compare p_message_text and error message from api, if match, then get other
    values, else return null.	
  Parameters
    p_message_text    IN  VARCHAR2
    x_app_name        OUT NOCOPY VARCHAR2
    x_message_name    OUT NOCOPY VARCHAR2
    x_message_detail  OUT NOCOPY T_MESSAGE_TABLE
  Return
  History
================================================================================*/
PROCEDURE get_error_message_infor(p_message_text    IN  VARCHAR2 DEFAULT NULL
                                 ,x_app_name        OUT NOCOPY VARCHAR2
                                 ,x_message_name    OUT NOCOPY VARCHAR2
                                 ,x_message_detail  OUT NOCOPY T_MESSAGE_TABLE);
END GME_BATCH_OPEN_INTERFACE;
/
COMMIT ;
EXIT;
