/* +======================================================================+ */
/* |    Copyright (c) 2005, 2015 Oracle and/or its affiliates.           | */
/* |                         All rights reserved.                         | */
/* |                           Version 12.0.0                             | */
/* +======================================================================+ */
REM dbdrv: sql ~PROD ~PATH ~FILE none none none package &phase=plb \
REM dbdrv: checkfile(120.0.12010000.24=120.9.12020000.15)(115.2=120.1):~PROD:~PATH:~FILE
SET VERIFY OFF
WHENEVER SQLERROR EXIT FAILURE ROLLBACK;
WHENEVER OSERROR EXIT FAILURE ROLLBACK;
REM **************************************************************************
REM *                                                                        *
REM * FILE:    GMEBTOIB.pls                                                  *
REM * PURPOSE: Package Body for the GME BATCH OPEN INTERFACE routines        *
REM * AUTHOR:  QZENG, OPM Development                                        *
REM * DATE:    March 08 2013                                                 *
REM * HISTORY: Created for bug#16457668                                      *
REM *  QZENG   14-Mar-2013 bug#16457668                                      *
REM *   Removed update of created_by and creation_date for interface tables  *
REM *   Modified bulk validation for recipe of batch header interface, set to*
REM *   validation error if recipe infor not found when creation mode are 10,*
REM *   30, 40.                                                              *
REM *  QZENG   15-Mar-2013 bug#16457668                                      *
REM *   Added column name  to each bulk validation failed error              *
REM *   Check sql%rowcount for each update in bulk validation                *
REM *   Check sql%rowcount for update of interface tables when begin process *
REM *   batches, and no need to prcess batch header or detail when each one  *
REM *   return 0 rowcount                                                    *
REM *  QZENG   17-Mar-2013 Bug#16457668                                      *
REM *   Added error handler after batch_no duplicated check in batch header  *
REM *   bulk validation                                                      *
REM *  QZENG   22-Mar-2013 Bug#16457668                                      *
REM *   Changed bulk validation for some fields, derived value for resource  *
REM *   usage uom and plan quantity, added log information when no pending   *
REM *   rows, set concurrent program to warning when row failed              *
REM *  QZENG   11-Apr-2013 Bug#16457668                                      *
REM *   Added code to support create and release batch for load_type 15      *
REM *  QZENG   19-Apr-2013 Bug#16457668                                      *
REM *   Added code to insert app_name, message_name, param and token for     *
REM *   error message logged into gme_intf_errors from own code or from gme  *
REM *   APIs                                                                 *
REM *  QZENG   22-May-2013 Bug#16457668                                      *
REM *   Added code to support new column organization_id for table gme_batch_*
REM *   header_interface.                                                    *
REM *   Added code to support new process_status sucessful with warning      *
REM *  QZENG   30-May-2013 Bug#16457668                                      *
REM *   Changed purge handling from concurrent program to directly delete    *
REM *  QZENG   07-Jun-2013 Bug#16457668                                      *
REM *   Changed warning message text to fnd_message GME_INTF_CP_WARN         *
REM *  QZENG   28-Jun-2013 Bug#16457668                                      *
REM *   Added purge for table gme_intf_errors                                *
REM *  QZENG   08-Jul-2013 Bug#16457668                                      *
REM *   Corrected request id for warning messages                            *
REM *  Shaliu Chen 25-Jul-2015 Bug 21208206                                  *
REM *   Modify procedure update_batch_header and update_step to add          *
REM *   actual_start_date and actual_cmplt_date validation for batch on hold *
REM * ============                                                           *
REM **************************************************************************
CREATE OR REPLACE PACKAGE BODY GME_BATCH_OPEN_INTERFACE AS
/* $Header: GMEBTOIB.pls 120.9.12020000.15 2015/06/25 10:26:11 shalchen noship $ */
g_debug               VARCHAR2 (5)  := NVL(fnd_profile.VALUE ('AFLOG_LEVEL'),-1);
g_errorred            BOOLEAN; --identify is there any row failed, true is failed, false is all succeed

/*================================================================================
  Procedure
    get_error_message
  Description
    This procedure gets error message
  Parameters
    p_encoded  default F, can be set T or F
  Return 
    error message text
  History
================================================================================*/
FUNCTION get_error_message(p_encoded VARCHAR2 DEFAULT 'F')
RETURN VARCHAR2
IS
  l_message             VARCHAR2 (2000);
BEGIN
  l_message := fnd_msg_pub.get (p_msg_index          => fnd_msg_pub.g_last,
                                p_encoded            => p_encoded
                  );
  RETURN l_message;
END get_error_message;

/*================================================================================
  Function
    split
  Description
    This function splits string to array
  Parameters
    P_STR       IN VARCHAR2 The string need to be splited
    P_DELIMITER IN VARCHAR2 The delimiter to splite the string
  Return 
    ARRAY of splited substring
  History
================================================================================*/
FUNCTION split(p_str IN VARCHAR2, p_delimiter IN VARCHAR2) 
RETURN ARRAY IS
  j         INT := 0;
  i         INT := 1;
  len       INT := 0;
  len1      INT := 0;
  str       VARCHAR2(4000);
  str_split ARRAY := ARRAY();
BEGIN
  len  := LENGTH(p_str);
  len1 := LENGTH(P_DELIMITER);
  
  WHILE J < LEN LOOP
    J := INSTR(P_STR, p_delimiter, I);  
    IF j = 0 THEN
      j   := len;
      str := SUBSTR(p_str, I);
      str_split.EXTEND;
      str_split(str_split.COUNT) := str;
      
      IF i >= len THEN
        EXIT;
      END IF;
    ELSE
      str := SUBSTR(p_str, i, j - i);
      i   := j + len1;
      str_split.EXTEND;
      str_split(str_split.COUNT) := str;
    END IF;
  END LOOP;
  
  RETURN str_split;
END split;

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
                                 ,x_message_detail  OUT NOCOPY T_MESSAGE_TABLE)
IS
  l_message             VARCHAR2 (2000);
  l_msg_array           ARRAY;
  l_count               NUMBER(1);
BEGIN
  IF x_message_detail.COUNT > 0 THEN
    x_message_detail.DELETE;
  END IF;
  IF p_message_text IS NOT NULL AND p_message_text <> l_message THEN
    x_app_name := NULL;
    x_message_name := NULL;
  END IF;
  l_message := get_error_message('T');
  l_count := 1;
  IF l_message IS NOT NULL THEN
    l_msg_array := SPLIT(l_message, chr(0));
    IF l_msg_array.COUNT > 2 THEN
      x_app_name := l_msg_array(1);
      x_message_name := l_msg_array(2);
	  FOR i IN 1..(l_msg_array.COUNT/3) LOOP
        IF l_msg_array.COUNT - (i * 3) >= 2 THEN
          x_message_detail(l_count).param := SUBSTR(l_msg_array((i * 3) + 1), 1, 30);
          x_message_detail(l_count).token := SUBSTR(l_msg_array((i * 3) + 2), 1, 60);
          IF l_count = 6 THEN
            EXIT;
          ELSE
            l_count := l_count + 1;
          END IF;
        ELSE
          EXIT;
        END IF;
      END LOOP;
    ELSIF l_msg_array.COUNT = 2 THEN
      x_app_name := l_msg_array(1);
      x_message_name := l_msg_array(2);
    ELSIF l_msg_array.COUNT < 2 THEN
      x_app_name := NULL;
      x_message_name := NULL;
    END IF;
  ELSE
    x_app_name := NULL;
    x_message_name := NULL;
  END IF;
EXCEPTION
  WHEN OTHERS THEN
    x_app_name := NULL;
    x_message_name := NULL;
END get_error_message_infor;

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
RETURN INTEGER                            
IS
   l_sysdate         DATE;
   l_user_id         NUMBER := NVL (FND_GLOBAL.user_id, -1);
   l_request_id      NUMBER := NVL (FND_GLOBAL.conc_request_id, -1);
   l_login_id        NUMBER := NVL (FND_GLOBAL.login_id, -1);
BEGIN  
   l_sysdate := SYSDATE;
   INSERT INTO GME_INTF_ERRORS (GROUP_ID,
                                OBJECT_TYPE,
                                INTERFACE_ID,
                                COLUMN_NAME,
                                MSG_APPL_SNAME,
                                MESSAGE_NAME,
                                MESSAGE_TYPE,
                                MESSAGE_TEXT,
                                SOURCE_TABLE_NAME,
                                REQUEST_ID,
                                PARAM1,
                                PARAM2,
                                PARAM3,
                                PARAM4,
                                PARAM5,
                                PARAM6,
                                TOKEN1,
                                TOKEN2,
                                TOKEN3,
                                TOKEN4,
                                TOKEN5,
                                TOKEN6,
                                LAST_UPDATED_BY,
                                CREATION_DATE,
                                CREATED_BY,
                                LAST_UPDATE_LOGIN,
                                LAST_UPDATE_DATE)
     VALUES (P_GROUP_ID,
             P_OBJECT_TYPE,
             P_INTERFACE_ID,
             P_COLUMN_NAME,
             P_APP_NAME,
             P_MESSAGE_NAME,
             P_MESSAGE_TYPE,
             P_MESSAGE_TEXT,
             P_SOURCE_TABLE_NAME,
             l_request_id,
             P_PARAM1,
             P_PARAM2,
             P_PARAM3,
             P_PARAM4,
             P_PARAM5,
             P_PARAM6,
             P_TOKEN1,
             P_TOKEN2,
             P_TOKEN3,
             P_TOKEN4,
             P_TOKEN5,
             P_TOKEN6,
             l_user_id,
             l_sysdate,
             l_user_id,
             l_login_id,
             l_sysdate);      
   RETURN (0);
EXCEPTION
   WHEN OTHERS
   THEN
      IF g_debug >= gme_debug.g_log_statement THEN
        gme_debug.put_line('Error occurred in package gme_batch_open_interface, procedure gme_log_interface_err: ' || SQLERRM);
      END IF;
      RETURN (SQLCODE);
END gme_log_interface_err;

/*================================================================================
  Procedure
    batchheader_bv_error_handle
  Description
    This procedure log error message to table gme_intf_errors for errored or warned
    rows, and mark these rows as failed if errored in bulk validation, when load_type is created from
    recipe, mark related detail rows as failed
  Parameters
    p_group_id          group id that specifies rows of batches
    p_message_name      message name of the error
    p_column_name       column name error occurred
    p_message_type      type of message, E Error, W Warning, C Critical
  History
================================================================================*/
PROCEDURE batchheader_bv_error_handle(p_group_id         IN  NUMBER
                                     ,p_app_name         IN  VARCHAR2 DEFAULT 'GME'
                                     ,p_message_name     IN  VARCHAR2
                                     ,p_column_name      IN  VARCHAR2 DEFAULT NULL
                                     ,p_message_type     IN  VARCHAR2 DEFAULT MESSAGE_TYPE_ERROR
                                     ,p_param1           IN  VARCHAR2 DEFAULT NULL
                                     ,p_param2           IN  VARCHAR2 DEFAULT NULL
                                     ,p_param3           IN  VARCHAR2 DEFAULT NULL
                                     ,p_param4           IN  VARCHAR2 DEFAULT NULL
                                     ,p_param5           IN  VARCHAR2 DEFAULT NULL
                                     ,p_param6           IN  VARCHAR2 DEFAULT NULL
                                     ,p_token1           IN  VARCHAR2 DEFAULT NULL
                                     ,p_token2           IN  VARCHAR2 DEFAULT NULL
                                     ,p_token3           IN  VARCHAR2 DEFAULT NULL
                                     ,p_token4           IN  VARCHAR2 DEFAULT NULL
                                     ,p_token5           IN  VARCHAR2 DEFAULT NULL
                                     ,p_token6           IN  VARCHAR2 DEFAULT NULL)
IS
  l_message_text    VARCHAR2(2000);
  
  l_table_name      CONSTANT VARCHAR2 (30)   := 'GME_BATCH_HEADER_INTERFACE';
  l_object_type     CONSTANT VARCHAR2 (3)    := '100';
  l_login           NUMBER                   := NVL (FND_GLOBAL.login_id, -1);
  l_userid          NUMBER                   := NVL (FND_GLOBAL.user_id, -1);
  l_request_id      NUMBER                   := NVL (FND_GLOBAL.conc_request_id, -1);
  l_sysdate         DATE;
BEGIN
  l_message_text := get_error_message;
  l_sysdate := sysdate;
  IF p_message_type = MESSAGE_TYPE_ERROR OR p_message_type = MESSAGE_TYPE_CRITICAL THEN
    g_errorred := TRUE;
    INSERT INTO gme_intf_errors 
          (group_id, object_type, interface_id, column_name, message_name, message_type, message_text, source_table_name, 
           param1, param2, param3, param4, param5, param6, token1, token2, token3, token4, token5, token6, msg_appl_sname,
           request_id, last_updated_by, creation_date, created_by, last_update_login, last_update_date)
    SELECT group_id, l_object_type, interface_header_id, p_column_name, p_message_name, p_message_type, l_message_text, l_table_name,
           p_param1, p_param2, p_param3, p_param4, p_param5, p_param6, p_token1, p_token2, p_token3, p_token4, p_token5, p_token6, p_app_name,
           l_request_id, l_userid, l_sysdate, l_userid, l_login, l_sysdate
    FROM   gme_batch_header_interface
    WHERE  process_status = PROCESS_STATUS_BV_INVALID
    AND    group_id = p_group_id;
  
    --Only set details to failed when new created batches failed
    UPDATE gme_batch_dtls_interface intfd
    SET process_status = PROCESS_STATUS_FAILED,
        request_id = l_request_id,
        last_updated_by = l_userid,
        last_update_login = l_login,
        last_update_date = l_sysdate
    WHERE interface_header_id =
      (SELECT interface_header_id 
       FROM gme_batch_header_interface intfh
       WHERE intfh.interface_header_id = intfd.interface_header_id
       AND   intfh.process_status = PROCESS_STATUS_BV_INVALID
       AND   intfh.group_id = p_group_id
       AND   intfh.load_type = LOADTYPE_HEADER_RECIPE
       )
    AND process_status = PROCESS_STATUS_RUNNING
    AND load_type = LOADTYPE_HEADER_RECIPE;
  
    UPDATE gme_batch_header_interface
    SET process_status = PROCESS_STATUS_FAILED,
        request_id = l_request_id,
        last_updated_by = l_userid,
        last_update_login = l_login,
        last_update_date = l_sysdate
    WHERE group_id = p_group_id
    AND   process_status = PROCESS_STATUS_BV_INVALID;
  ELSE
    INSERT INTO gme_intf_errors 
          (group_id, object_type, interface_id, column_name, message_name, message_type, message_text, source_table_name, 
           param1, param2, param3, param4, param5, param6, token1, token2, token3, token4, token5, token6, msg_appl_sname,
           request_id, last_updated_by, creation_date, created_by, last_update_login, last_update_date)
    SELECT group_id, l_object_type, interface_header_id, p_column_name, p_message_name, p_message_type, l_message_text, l_table_name,
           p_param1, p_param2, p_param3, p_param4, p_param5, p_param6, p_token1, p_token2, p_token3, p_token4, p_token5, p_token6, p_app_name,
           l_request_id, l_userid, l_sysdate, l_userid, l_login, l_sysdate
    FROM   gme_batch_header_interface
    WHERE  process_status = PROCESS_STATUS_BV_WARNING
    AND    group_id = p_group_id;

    /*UPDATE gme_batch_header_interface
    SET process_status = PROCESS_STATUS_RUNNING,
        request_id = l_request_id,
        last_updated_by = l_userid,
        last_update_login = l_login,
        last_update_date = l_sysdate
    WHERE group_id = p_group_id
    AND   process_status = PROCESS_STATUS_BV_WARNING;*/
  END IF;
  COMMIT;
END batchheader_bv_error_handle;

/*================================================================================
  Procedure
    batchdtls_bv_error_handle
  Description
    This procedure log error message to table gme_intf_errors for errored or warned
    rows, and mark these rows as failed if errored in bulk validation
  Parameters
    p_group_id          group id that specifies rows of batches
    p_message_name      message name of the error
    p_column_name       column name error occurred
    p_message_type      type of message, E Error, W Warning, C Critical
  History
================================================================================*/
PROCEDURE batchdtls_bv_error_handle(p_group_id         IN  NUMBER
                                   ,p_app_name         IN  VARCHAR2 DEFAULT 'GME'
                                   ,p_message_name     IN  VARCHAR2
                                   ,p_column_name      IN  VARCHAR2 DEFAULT NULL
                                   ,p_message_type     IN  VARCHAR2 DEFAULT MESSAGE_TYPE_ERROR
                                   ,p_param1           IN  VARCHAR2 DEFAULT NULL
                                   ,p_param2           IN  VARCHAR2 DEFAULT NULL
                                   ,p_param3           IN  VARCHAR2 DEFAULT NULL
                                   ,p_param4           IN  VARCHAR2 DEFAULT NULL
                                   ,p_param5           IN  VARCHAR2 DEFAULT NULL
                                   ,p_param6           IN  VARCHAR2 DEFAULT NULL
                                   ,p_token1           IN  VARCHAR2 DEFAULT NULL
                                   ,p_token2           IN  VARCHAR2 DEFAULT NULL
                                   ,p_token3           IN  VARCHAR2 DEFAULT NULL
                                   ,p_token4           IN  VARCHAR2 DEFAULT NULL
                                   ,p_token5           IN  VARCHAR2 DEFAULT NULL
                                   ,p_token6           IN  VARCHAR2 DEFAULT NULL)
IS
  l_message_text    VARCHAR2(2000);
  l_table_name      CONSTANT VARCHAR2 (30)   := 'GME_BATCH_DTLS_INTERFACE';
  l_login           NUMBER                   := NVL (FND_GLOBAL.login_id, -1);
  l_userid          NUMBER                   := NVL (FND_GLOBAL.user_id, -1);
  l_request_id      NUMBER                   := NVL (FND_GLOBAL.conc_request_id, -1);
  l_sysdate         DATE;
BEGIN
  l_message_text := get_error_message;
  l_sysdate := sysdate;
  g_errorred := TRUE;
  INSERT INTO gme_intf_errors 
        (group_id, object_type, interface_id, column_name, message_name, message_type, message_text, source_table_name, 
         param1, param2, param3, param4, param5, param6, token1, token2, token3, token4, token5, token6, msg_appl_sname,
         request_id, last_updated_by, creation_date, created_by, last_update_login, last_update_date)
  SELECT group_id, object_type, interface_id, p_column_name, p_message_name, p_message_type, l_message_text, l_table_name,
         p_param1, p_param2, p_param3, p_param4, p_param5, p_param6, p_token1, p_token2, p_token3, p_token4, p_token5, p_token6, p_app_name,
         l_request_id, l_userid, l_sysdate, l_userid, l_login, l_sysdate
  FROM   gme_batch_dtls_interface
  WHERE  process_status = PROCESS_STATUS_BV_INVALID
  AND    group_id = p_group_id;
  
  UPDATE gme_batch_dtls_interface intfd
  SET    process_status = PROCESS_STATUS_FAILED,
         request_id = l_request_id,
         last_updated_by = l_userid,
         last_update_login = l_login,
         last_update_date = l_sysdate
  WHERE  group_id = p_group_id
  AND    process_status = PROCESS_STATUS_BV_INVALID;
  COMMIT;
END batchdtls_bv_error_handle;

/*================================================================================
  Procedure
    bulk_validate_batch_header
  Description
    This procedure bulk validation batch header rows, if failed, then log error 
    message to table gme_intf_errors, and mark these rows as failed, if the row
    load_type is created from recipe, then mark all related detail rows as failed
  Parameters
    P_GROUP_ID          group id that specifies rows of batches
    x_message_list      message text
    x_return_status     status to return back
  History
    QZENG    11-Apr-2013 Bug#16457668
      Added code to support load_type 15
================================================================================*/
PROCEDURE bulk_validate_batch_header(p_group_id      IN         NUMBER
                                    ,x_message_list  OUT NOCOPY VARCHAR2
                                    ,x_return_status OUT NOCOPY VARCHAR2)
IS
BEGIN
  --First set all internal columns to null
  UPDATE gme_batch_header_interface
  SET    int_organization_id = NULL, int_batch_id = NULL,
         int_recipe_id = NULL, int_inventory_item_id = NULL
  WHERE  group_id = p_group_id  
  AND    process_status = PROCESS_STATUS_RUNNING;

  IF sql%rowcount = 0 THEN
    x_return_status := fnd_api.g_ret_sts_success;
    RETURN;
  END IF;
  --Bulk validtion for ORGANIZATION_CODE, ORGANIZATION_ID
  UPDATE gme_batch_header_interface intf
  SET int_organization_id =
      (SELECT m.organization_id
       FROM   mtl_parameters m, gme_parameters g
       WHERE  (m.organization_id = intf.organization_id
               OR (m.organization_code = intf.organization_code AND intf.organization_id IS NULL))
       AND    m.organization_id = g.organization_id)
  WHERE (organization_code IS NOT NULL OR organization_id IS NOT NULL)
  AND   group_id = p_group_id
  AND   process_status = PROCESS_STATUS_RUNNING;

  UPDATE gme_batch_header_interface
  SET    process_status = PROCESS_STATUS_BV_INVALID
  WHERE  int_organization_id IS NULL
  AND    batch_id IS NULL
  AND    group_id = p_group_id
  AND    process_status = PROCESS_STATUS_RUNNING;

  IF sql%rowcount > 0 THEN
    gme_common_pvt.log_message('GME_ORGID_NOTFOUND');
    batchheader_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_ORGID_NOTFOUND');
  END IF;

  --Bulk validtion for INT_ORGANIZATION_ID, BATCH_NO, BATCH_ID
  UPDATE gme_batch_header_interface intf
  SET (int_batch_id, int_organization_id) =
      (SELECT h.batch_id, h.organization_id
       FROM   gme_batch_header h
       WHERE  (h.batch_id = intf.batch_id
               OR (h.organization_id = intf.int_organization_id
                   AND h.batch_no = intf.batch_no
                   AND intf.batch_id IS NULL))
       AND     h.migrated_batch_ind = 'N'
       AND     h.delete_mark = 0
       AND     h.batch_type = 0
       )
  WHERE (batch_no IS NOT NULL OR batch_id IS NOT NULL)
  AND   load_type IN (LOADTYPE_HEADER_UPDATE, LOADTYPE_HEADER_RESCHEDULE, LOADTYPE_HEADER_REROUTE, LOADTYPE_HEADER_RELEASE)
  AND   group_id = p_group_id
  AND   process_status = PROCESS_STATUS_RUNNING;

  UPDATE gme_batch_header_interface
  SET process_status = PROCESS_STATUS_BV_INVALID
  WHERE int_batch_id IS NULL
  AND   (load_type IN (LOADTYPE_HEADER_UPDATE, LOADTYPE_HEADER_RESCHEDULE, LOADTYPE_HEADER_REROUTE)
         OR (load_type = LOADTYPE_HEADER_RELEASE AND batch_id IS NOT NULL))
  AND   group_id = p_group_id
  AND   process_status = PROCESS_STATUS_RUNNING;

  IF sql%rowcount > 0 THEN
    gme_common_pvt.log_message('GME_BATCHID_NOTFOUND');
    batchheader_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_BATCHID_NOTFOUND');
  END IF;

  --Reset int_organization_id for LOADTYPE_HEADER_RELEASE and int_organization_id is NULL
  UPDATE gme_batch_header_interface intf
  SET int_organization_id =
      (SELECT m.organization_id
       FROM   mtl_parameters m
       WHERE  m.organization_id = intf.organization_id
       OR     (m.organization_code = intf.organization_code
               AND intf.organization_id IS NULL))
  WHERE int_organization_id IS NULL
  AND   group_id = p_group_id
  AND   process_status = PROCESS_STATUS_RUNNING;

  --bulk validation for INT_ORGANIZATION_ID, BATCH_NO
  UPDATE gme_batch_header_interface intf
  SET process_status = PROCESS_STATUS_BV_INVALID
  WHERE EXISTS
    (SELECT 1
     FROM  gme_batch_header h
     WHERE (h.batch_no = intf.batch_no
            AND h.organization_id = intf.int_organization_id
            AND h.batch_type = 0
            AND intf.batch_no IS NOT NULL
            AND h.delete_mark = 0)
     )
  AND   (load_type = LOADTYPE_HEADER_RECIPE 
         OR (load_type = LOADTYPE_HEADER_RELEASE AND int_batch_id IS NULL))
  AND   group_id = p_group_id
  AND   process_status = PROCESS_STATUS_RUNNING;

  IF sql%rowcount > 0 THEN
    gme_common_pvt.log_message('GME_DUP_BATCH');
    batchheader_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_DUP_BATCH'
                               ,p_column_name  => 'BATCH_NO');
  END IF;

  UPDATE gme_batch_header_interface intf
  SET process_status = PROCESS_STATUS_BV_INVALID
  WHERE EXISTS
    (SELECT 1
     FROM   gme_parameters p
     WHERE  p.batch_doc_numbering = gme_common_pvt.g_manual_doc_numbering
     AND    p.organization_id = intf.int_organization_id
     AND    intf.batch_no IS NULL)
  AND   (load_type = LOADTYPE_HEADER_RECIPE 
         OR (load_type = LOADTYPE_HEADER_RELEASE AND int_batch_id IS NULL))
  AND   group_id = p_group_id
  AND   process_status = PROCESS_STATUS_RUNNING;

  IF sql%rowcount > 0 THEN
    gme_common_pvt.log_message('GME_INVALID_FIELD', 'FIELD', 'BATCH_NO');
    batchheader_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_INVALID_FIELD'
                               ,p_column_name  => 'BATCH_NO'
                               ,p_param1       => 'FIELD'
                               ,p_token1       => 'BATCH_NO');
  END IF;

  --Bulk validtion for batch status when updating or rescheduling or rerouting batch or releasing batch
  UPDATE gme_batch_header_interface intf
  SET process_status = PROCESS_STATUS_BV_INVALID
  WHERE EXISTS
    (SELECT 1
     FROM   gme_batch_header h
     WHERE  ((h.batch_status IN (gme_common_pvt.g_batch_closed, gme_common_pvt.g_batch_cancelled)
              AND intf.load_type = LOADTYPE_HEADER_UPDATE)
             OR
             (h.batch_status NOT IN (gme_common_pvt.g_batch_pending, gme_common_pvt.g_batch_wip)
              AND intf.load_type = LOADTYPE_HEADER_RESCHEDULE)
             OR
             (h.batch_status <> gme_common_pvt.g_batch_pending
              AND (intf.load_type = LOADTYPE_HEADER_REROUTE OR intf.load_type = LOADTYPE_HEADER_RELEASE)))
     AND    h.batch_id = intf.int_batch_id)
  AND   load_type <> LOADTYPE_HEADER_RECIPE
  AND   group_id = p_group_id
  AND   process_status = PROCESS_STATUS_RUNNING;

  IF sql%rowcount > 0 THEN
    gme_common_pvt.log_message('GME_INV_BATCH_STATUS_OPER');
    batchheader_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_INV_BATCH_STATUS_OPER');
  END IF;
  
  --Bulk validtion for RECIPE_VALIDITY_RULE_ID
  UPDATE gme_batch_header_interface intf
  SET (int_recipe_id) =
      (SELECT vr.recipe_id
       FROM gmd_recipe_validity_rules vr
       WHERE vr.recipe_validity_rule_id = intf.recipe_validity_rule_id
       AND   (vr.validity_rule_status BETWEEN 700 AND 799 
              OR vr.validity_rule_status BETWEEN 900 AND 999)
      )
  WHERE (load_type IN (LOADTYPE_HEADER_RECIPE, LOADTYPE_HEADER_REROUTE)
         OR (load_type = LOADTYPE_HEADER_RELEASE AND int_batch_id IS NULL))
  AND   recipe_validity_rule_id IS NOT NULL
  AND   group_id = p_group_id
  AND   process_status = PROCESS_STATUS_RUNNING;
  
  UPDATE gme_batch_header_interface
  SET process_status = PROCESS_STATUS_BV_INVALID
  WHERE int_recipe_id IS NULL
  AND   (load_type = LOADTYPE_HEADER_REROUTE  
         OR (load_type = LOADTYPE_HEADER_RECIPE AND recipe_validity_rule_id IS NOT NULL)
		 OR (load_type = LOADTYPE_HEADER_RELEASE AND int_batch_id IS NULL AND recipe_validity_rule_id IS NOT NULL))
  AND   group_id = p_group_id
  AND   process_status = PROCESS_STATUS_RUNNING;

  IF sql%rowcount > 0 THEN
    gme_common_pvt.log_message('GME_INVALID_FIELD', 'FIELD', 'RECIPE_VALIDITY_RULE_ID');
    batchheader_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_INVALID_FIELD'
                               ,p_column_name  => 'RECIPE_VALIDITY_RULE_ID'
                               ,p_param1       => 'FIELD'
                               ,p_token1       => 'RECIPE_VALIDITY_RULE_ID');
  END IF;

  UPDATE gme_batch_header_interface
  SET process_status = PROCESS_STATUS_BV_WARNING
  WHERE recipe_validity_rule_id IS NOT NULL
  AND   (recipe_id IS NOT NULL OR recipe_no IS NOT NULL OR recipe_vers IS NOT NULL)
  AND   (load_type = LOADTYPE_HEADER_RECIPE 
         OR (load_type = LOADTYPE_HEADER_RELEASE AND int_batch_id IS NULL))
  AND   group_id = p_group_id
  AND   process_status = PROCESS_STATUS_RUNNING;

  IF sql%rowcount > 0 THEN
    gme_common_pvt.log_message('GME_MORE_RECIPE_INFOR');
    batchheader_bv_error_handle(p_group_id      => p_group_id
                               ,p_message_name  => 'GME_MORE_RECIPE_INFOR'
                               ,p_message_type  => MESSAGE_TYPE_WARNING);
  END IF;

  --bulk validation for RECIPE_ID, RECIPE_NO, RECIPE_VERSION
  UPDATE gme_batch_header_interface intf
  SET (int_recipe_id) =
      (SELECT r.recipe_id
       FROM   gmd_recipes r
       WHERE  (r.recipe_id = intf.recipe_id
               OR (r.recipe_no = intf.recipe_no
                   AND r.recipe_version = intf.recipe_vers
                   AND intf.recipe_id IS NULL))
       AND   (r.recipe_status BETWEEN 700 AND 799
              OR r.recipe_status BETWEEN 900 AND 999)
      )
  WHERE (recipe_id IS NOT NULL
         OR recipe_no IS NOT NULL
         OR recipe_vers IS NOT NULL)
  AND   (load_type = LOADTYPE_HEADER_RECIPE 
         OR (load_type = LOADTYPE_HEADER_RELEASE AND int_batch_id IS NULL))
  AND   recipe_validity_rule_id IS NULL
  AND   int_recipe_id IS NULL
  AND   group_id = p_group_id
  AND   process_status IN (PROCESS_STATUS_RUNNING, PROCESS_STATUS_BV_WARNING);

  UPDATE gme_batch_header_interface
  SET process_status = PROCESS_STATUS_BV_INVALID
  WHERE int_recipe_id IS NULL
  AND   ((recipe_id IS NOT NULL
          OR recipe_no IS NOT NULL
          OR recipe_vers IS NOT NULL)
		 OR creation_mode <> CREATION_MODE_PRODUCT)
  AND   (load_type = LOADTYPE_HEADER_RECIPE 
         OR (load_type = LOADTYPE_HEADER_RELEASE AND int_batch_id IS NULL))
  AND   group_id = p_group_id
  AND   process_status IN (PROCESS_STATUS_RUNNING, PROCESS_STATUS_BV_WARNING);

  IF sql%rowcount > 0 THEN
    gme_common_pvt.log_message('GME_RECIPE_ID_NOTFOUND');
    batchheader_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_RECIPE_ID_NOTFOUND');
  END IF;

  --bulk validation for PLAN_START_DATE, PLAN_CMPLT_DATE
  UPDATE gme_batch_header_interface
  SET process_status = PROCESS_STATUS_BV_INVALID
  WHERE plan_start_date > plan_cmplt_date
  AND   group_id = p_group_id
  AND   process_status IN (PROCESS_STATUS_RUNNING, PROCESS_STATUS_BV_WARNING);

  IF sql%rowcount > 0 THEN
    gme_common_pvt.log_message('GME_CMPLT_START_DT');
    batchheader_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_CMPLT_START_DT');
  END IF;

  --bulk validation for CREATION_MODE
  UPDATE gme_batch_header_interface
  SET process_status = PROCESS_STATUS_BV_INVALID
  WHERE creation_mode IS NULL
  AND   (load_type = LOADTYPE_HEADER_RECIPE 
         OR (load_type = LOADTYPE_HEADER_RELEASE AND int_batch_id IS NULL))
  AND   group_id = p_group_id
  AND   process_status IN (PROCESS_STATUS_RUNNING, PROCESS_STATUS_BV_WARNING);

  IF sql%rowcount > 0 THEN
    gme_common_pvt.log_message('GME_INVALID_FIELD', 'FIELD', 'CREATION_MODE');
    batchheader_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_INVALID_FIELD'
                               ,p_column_name  => 'CREATION_MODE'
                               ,p_param1       => 'FIELD'
                               ,p_token1       => 'CREATION_MODE');
  END IF;

  --bulk validation for BATCH_SIZE
  UPDATE gme_batch_header_interface
  SET process_status = PROCESS_STATUS_BV_INVALID
  WHERE batch_size IS NULL
  AND   creation_mode IN (CREATION_MODE_PRODUCT, CREATION_MODE_OUTPUT, CREATION_MODE_INPUT)
  AND   (load_type = LOADTYPE_HEADER_RECIPE 
         OR (load_type = LOADTYPE_HEADER_RELEASE AND int_batch_id IS NULL))
  AND   group_id = p_group_id
  AND   process_status IN (PROCESS_STATUS_RUNNING, PROCESS_STATUS_BV_WARNING);

  IF sql%rowcount > 0 THEN
    gme_common_pvt.log_message('GME_INVALID_FIELD', 'FIELD', 'BATCH_SIZE');
    batchheader_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_INVALID_FIELD'
                               ,p_column_name  => 'BATCH_SIZE'
                               ,p_param1       => 'FIELD'
                               ,p_token1       => 'BATCH_SIZE');
  END IF;

  --bulk validation for BATCH_SIZE_UOM
  UPDATE gme_batch_header_interface intf
  SET process_status = PROCESS_STATUS_BV_INVALID
  WHERE NOT EXISTS
    (SELECT 1
     FROM mtl_units_of_measure m
     WHERE m.uom_code = intf.batch_size_uom
     AND   (m.disable_date > sysdate OR m.disable_date IS NULL)
    )
  AND   creation_mode IN (CREATION_MODE_PRODUCT, CREATION_MODE_OUTPUT, CREATION_MODE_INPUT)
  AND   (load_type = LOADTYPE_HEADER_RECIPE 
         OR (load_type = LOADTYPE_HEADER_RELEASE AND int_batch_id IS NULL))
  AND   group_id = p_group_id
  AND   process_status IN (PROCESS_STATUS_RUNNING, PROCESS_STATUS_BV_WARNING);

  IF sql%rowcount > 0 THEN
    gme_common_pvt.log_message('GME_INVALID_FIELD', 'FIELD', 'BATCH_SIZE_UOM');
    batchheader_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_INVALID_FIELD'
                               ,p_column_name  => 'BATCH_SIZE_UOM'
                               ,p_param1       => 'FIELD'
                               ,p_token1       => 'BATCH_SIZE_UOM');
  END IF;

  --bulk validation for INVENTORY_ITEM_ID, ITEM
  UPDATE gme_batch_header_interface intf
  SET int_inventory_item_id = 
    (SELECT si.inventory_item_id 
     FROM   mtl_system_items_kfv si
     WHERE  (si.inventory_item_id = intf.inventory_item_id 
             AND si.organization_id = intf.int_organization_id)
     OR     (intf.inventory_item_id IS NULL 
             AND si.concatenated_segments = intf.item
             AND si.organization_id = intf.int_organization_id))
  WHERE (inventory_item_id IS NOT NULL OR ITEM IS NOT NULL)
  AND   creation_mode = CREATION_MODE_PRODUCT
  AND   recipe_validity_rule_id IS NULL
  AND   (load_type = LOADTYPE_HEADER_RECIPE 
         OR (load_type = LOADTYPE_HEADER_RELEASE AND int_batch_id IS NULL))
  AND   group_id = p_group_id
  AND   process_status IN (PROCESS_STATUS_RUNNING, PROCESS_STATUS_BV_WARNING);
  
  UPDATE gme_batch_header_interface
  SET    process_status = PROCESS_STATUS_BV_INVALID
  WHERE  int_inventory_item_id IS NULL
  AND    creation_mode = CREATION_MODE_PRODUCT
  AND    recipe_validity_rule_id IS NULL
  AND    (load_type = LOADTYPE_HEADER_RECIPE 
         OR (load_type = LOADTYPE_HEADER_RELEASE AND int_batch_id IS NULL))
  AND    group_id = p_group_id
  AND   process_status IN (PROCESS_STATUS_RUNNING, PROCESS_STATUS_BV_WARNING);

  IF sql%rowcount > 0 THEN
    gme_common_pvt.log_message('GME_ITEM_ID_NOTFOUND');
    batchheader_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_ITEM_ID_NOTFOUND');
  END IF;
  x_return_status := fnd_api.g_ret_sts_success;
EXCEPTION
  WHEN OTHERS THEN
    x_message_list := SQLERRM;
    x_return_status := fnd_api.G_RET_STS_ERROR;
    IF g_debug >= gme_debug.g_log_statement THEN
      gme_debug.put_line('Errored in package gme_batch_open_interface, procedure bulk_validate_batch_header with error: ' || SQLERRM);
    END IF;
END bulk_validate_batch_header;

/*================================================================================
  Procedure
    bulk_validate_batch_detail
  Description
    This procedure bulk validation batch detail rows, if failed, then log error 
    message to table gme_intf_errors, and mark these rows as failed
  Parameters
    P_GROUP_ID          group id that specifies rows of details
    p_object_type        object type to be validated, 200 - material, 300 - step, 400 - activity, 500 - resources,
                         600 - step item association, 700 - process parameter
    x_message_list       message text
    x_return_status      status to return back
  History
================================================================================*/
PROCEDURE bulk_validate_batch_detail(p_group_id      IN         NUMBER
                                    ,p_object_type   IN         NUMBER DEFAULT NULL
                                    ,x_message_list  OUT NOCOPY VARCHAR2
                                    ,x_return_status OUT NOCOPY VARCHAR2)
IS
BEGIN
  IF p_object_type IS NULL THEN
    --First set all internal columns to null
    UPDATE gme_batch_dtls_interface
    SET    int_organization_id = NULL, int_batch_id = NULL, 
           int_inventory_item_id = NULL, int_locator_id = NULL,
           int_batchstep_id = NULL, int_batchstep_activity_id = NULL,
           int_batchstep_resource_id = NULL, int_process_param_id = NULL,
           int_oprn_id = NULL, int_material_detail_id = NULL,
           int_cost_cmpntcls_id = NULL, int_parameter_id = NULL
    WHERE  group_id = p_group_id  
    AND    process_status = PROCESS_STATUS_RUNNING;

    IF sql%rowcount = 0 THEN
      x_return_status := fnd_api.g_ret_sts_success;
      RETURN;
    END IF;

    --bulk validation for interface_header_id
    UPDATE gme_batch_dtls_interface intd
    SET (int_batch_id, int_organization_id, batch_no) =
      (SELECT int_batch_id, int_organization_id, batch_no
       FROM   gme_batch_header_interface inth
       WHERE  inth.interface_header_id = intd.interface_header_id
       AND    inth.group_id = p_group_id
       AND    inth.process_status IN (PROCESS_STATUS_SUCCEED, PROCESS_STATUS_WARNING)
      )
    WHERE interface_header_id IS NOT NULL
    AND   group_id = p_group_id
    AND   process_status = PROCESS_STATUS_RUNNING
    AND   EXISTS ( --only update batch information when batch_no is automatical created
          SELECT 1 
          FROM   gme_batch_header_interface header, gme_parameters p
          WHERE  intd.interface_header_id = header.interface_header_id 
          AND    header.load_type IN (LOADTYPE_HEADER_RECIPE, LOADTYPE_HEADER_RELEASE)
          AND    header.int_organization_id = p.organization_id
          AND    p.batch_doc_numbering = gme_common_pvt.g_auto_doc_numbering
          );
    
    UPDATE gme_batch_dtls_interface
    SET    process_status = PROCESS_STATUS_BV_INVALID
    WHERE  int_batch_id IS NULL
    AND    interface_header_id IS NOT NULL
    AND    group_id = p_group_id
    AND    process_status = PROCESS_STATUS_RUNNING;

    IF sql%rowcount > 0 THEN
      gme_common_pvt.log_message('GME_INVALID_FIELD', 'FIELD', 'INTERFACE_HEADER_ID');
      batchdtls_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_INVALID_FIELD'
                               ,p_column_name  => 'INTERFACE_HEADER_ID'
                               ,p_param1       => 'FIELD'
                               ,p_token1       => 'INTERFACE_HEADER_ID');
    END IF;
  ELSIF p_object_type = OBJECTYPE_MATERIAL THEN
    --bulk validation for LOAD_TYPE
    UPDATE gme_batch_dtls_interface
    SET    process_status = PROCESS_STATUS_BV_INVALID
    WHERE  load_type NOT IN (LOADTYPE_DETAIL_ADD, LOADTYPE_DETAIL_UPDATE, LOADTYPE_DETAIL_DELETE)
    AND    object_type = OBJECTYPE_MATERIAL
    AND    group_id = p_group_id
    AND    process_status = PROCESS_STATUS_RUNNING;

    IF sql%rowcount > 0 THEN
      gme_common_pvt.log_message('GME_INVALID_FIELD', 'FIELD', 'LOAD_TYPE');
      batchdtls_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_INVALID_FIELD'
                               ,p_column_name  => 'LOAD_TYPE'
                               ,p_param1       => 'FIELD'
                               ,p_token1       => 'LOAD_TYPE');
    END IF;

    UPDATE gme_batch_dtls_interface intf
    SET (int_batch_id, int_organization_id, int_material_detail_id) = 
        (SELECT md.batch_id, md.organization_id, md.material_detail_id
         FROM   gme_material_details md
         WHERE  md.material_detail_id = intf.material_detail_id)
    WHERE material_detail_id IS NOT NULL
    AND   object_type = OBJECTYPE_MATERIAL
    AND   (load_type = LOADTYPE_DETAIL_UPDATE OR load_type = LOADTYPE_DETAIL_DELETE)
    AND   group_id = p_group_id
    AND   process_status = PROCESS_STATUS_RUNNING;
    
    UPDATE gme_batch_dtls_interface intf
    SET int_organization_id = 
        (SELECT mp.organization_id
         FROM   mtl_parameters mp
         WHERE  mp.organization_code = intf.organization_code)
    WHERE organization_code IS NOT NULL
    AND   int_organization_id IS NULL
    AND   object_type = OBJECTYPE_MATERIAL
    AND   group_id = p_group_id
    AND   process_status = PROCESS_STATUS_RUNNING;
    
    UPDATE gme_batch_dtls_interface intf
    SET (int_batch_id, int_material_detail_id) = 
        (SELECT md.batch_id, md.material_detail_id
         FROM   gme_material_details md, gme_batch_header bh
         WHERE (md.batch_id = intf.int_batch_id  
                OR (bh.batch_no = intf.batch_no
                    AND bh.organization_id = intf.int_organization_id
                    AND intf.int_batch_id IS NULL))
         AND bh.migrated_batch_ind = 'N'
         AND bh.delete_mark = 0
         AND bh.batch_type = 0
         AND md.batch_id = bh.batch_id
         AND md.line_no = intf.line_no
         AND md.line_type = intf.line_type)
    WHERE ((int_organization_id IS NOT NULL AND batch_no IS NOT NULL) OR int_batch_id IS NOT NULL)
    AND   line_no IS NOT NULL
    AND   line_type IS NOT NULL
    AND   object_type = OBJECTYPE_MATERIAL
    AND   (load_type = LOADTYPE_DETAIL_UPDATE OR load_type = LOADTYPE_DETAIL_DELETE)
    AND   group_id = p_group_id
    AND   process_status = PROCESS_STATUS_RUNNING;
    
    UPDATE gme_batch_dtls_interface intf
    SET   process_status = PROCESS_STATUS_BV_INVALID 
    WHERE int_material_detail_id IS NULL
    AND   object_type = OBJECTYPE_MATERIAL
    AND   (load_type = LOADTYPE_DETAIL_UPDATE OR load_type = LOADTYPE_DETAIL_DELETE)
    AND   group_id = p_group_id
    AND   process_status = PROCESS_STATUS_RUNNING;

    IF sql%rowcount > 0 THEN
      gme_common_pvt.log_message('GME_MATERIAL_DETAILID_NOTFOUND');
      batchdtls_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_MATERIAL_DETAILID_NOTFOUND');
    END IF;

    UPDATE gme_batch_dtls_interface intf
    SET (int_batch_id, int_organization_id) = 
        (SELECT bh.batch_id, bh.organization_id
         FROM   gme_batch_header bh
         WHERE  (bh.batch_id = intf.batch_id
                 OR (bh.batch_no = intf.batch_no
                     AND bh.organization_id = intf.int_organization_id
                     AND intf.batch_id IS NULL))
         AND bh.batch_type = 0
         AND bh.delete_mark = 0
         AND bh.migrated_batch_ind = 'N')
    WHERE ((int_organization_id IS NOT NULL AND batch_no IS NOT NULL) OR batch_id IS NOT NULL)
    AND   int_batch_id IS NULL
    AND   object_type = OBJECTYPE_MATERIAL
    AND   load_type = LOADTYPE_DETAIL_ADD
    AND   group_id = p_group_id
    AND   process_status = PROCESS_STATUS_RUNNING;

    UPDATE gme_batch_dtls_interface intf
    SET   process_status = PROCESS_STATUS_BV_INVALID 
    WHERE int_batch_id IS NULL
    AND   object_type = OBJECTYPE_MATERIAL
    AND   load_type = LOADTYPE_DETAIL_ADD
    AND   group_id = p_group_id
    AND   process_status = PROCESS_STATUS_RUNNING;

    IF sql%rowcount > 0 THEN
      gme_common_pvt.log_message('GME_BATCHID_NOTFOUND');
      batchdtls_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_BATCHID_NOTFOUND');
    END IF;

    --Bulk validtion for batch status
    UPDATE gme_batch_dtls_interface intf
    SET process_status = PROCESS_STATUS_BV_INVALID
    WHERE EXISTS
      (SELECT 1
       FROM   gme_batch_header h
       WHERE  ((h.batch_status IN (gme_common_pvt.g_batch_closed, gme_common_pvt.g_batch_cancelled)
                AND intf.load_type <> LOADTYPE_DETAIL_DELETE)
               OR
               (h.batch_status NOT IN (gme_common_pvt.g_batch_pending, gme_common_pvt.g_batch_wip)
                AND intf.load_type = LOADTYPE_DETAIL_DELETE))
       AND    h.batch_id = intf.int_batch_id)
    AND   object_type = OBJECTYPE_MATERIAL
    AND   group_id = p_group_id
    AND   process_status = PROCESS_STATUS_RUNNING;

    IF sql%rowcount > 0 THEN
      gme_common_pvt.log_message('GME_INV_BATCH_STATUS_OPER');
      batchdtls_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_INV_BATCH_STATUS_OPER');
    END IF;

    --bulk validation for INVENTORY_ITEM_ID, ITEM
    UPDATE gme_batch_dtls_interface intf
    SET int_inventory_item_id = 
        (SELECT si.inventory_item_id 
         FROM   mtl_system_items_kfv si
         WHERE  (si.inventory_item_id = intf.inventory_item_id 
                 AND si.organization_id = intf.int_organization_id)
         OR     (intf.inventory_item_id IS NULL 
                 AND si.concatenated_segments = intf.item
                 AND si.organization_id = intf.int_organization_id))
    WHERE (inventory_item_id IS NOT NULL OR ITEM IS NOT NULL)
    AND   load_type = LOADTYPE_DETAIL_ADD
    AND   object_type = OBJECTYPE_MATERIAL
    AND   group_id = p_group_id
    AND   process_status = PROCESS_STATUS_RUNNING;
  
    UPDATE gme_batch_dtls_interface
    SET    process_status = PROCESS_STATUS_BV_INVALID
    WHERE  int_inventory_item_id IS NULL
    AND    load_type = LOADTYPE_DETAIL_ADD
    AND    object_type = OBJECTYPE_MATERIAL
    AND    group_id = p_group_id
    AND    process_status = PROCESS_STATUS_RUNNING;

    IF sql%rowcount > 0 THEN
      gme_common_pvt.log_message('GME_ITEM_ID_NOTFOUND');
      batchdtls_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_ITEM_ID_NOTFOUND');
    END IF;

    --bulk validation for REVISION
    UPDATE gme_batch_dtls_interface intf
    SET    process_status = PROCESS_STATUS_BV_INVALID
    WHERE  NOT EXISTS
        (SELECT 1 
         FROM   mtl_item_revisions ir, mtl_system_items si
         WHERE  ir.inventory_item_id = intf.int_inventory_item_id 
         AND    ir.revision  = intf.revision
         AND    ir.inventory_item_id = si.inventory_item_id
         AND    ir.organization_id = si.organization_id
         AND    si.revision_qty_control_code = 1) --1=No revision qty control; 2=Under revision qty control
    AND   int_inventory_item_id IS NOT NULL
    AND   revision IS NOT NULL
    AND   load_type = LOADTYPE_DETAIL_ADD
    AND   object_type = OBJECTYPE_MATERIAL
    AND   group_id = p_group_id
    AND   process_status = PROCESS_STATUS_RUNNING;

    IF sql%rowcount > 0 THEN
      gme_common_pvt.log_message('GME_INVALID_FIELD', 'FIELD', 'REVISION');
      batchdtls_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_INVALID_FIELD'
                               ,p_column_name  => 'REVISION'
                               ,p_param1       => 'FIELD'
                               ,p_token1       => 'REVISION');
    END IF;

    --bulk validation for LINE_NO, LINE_TYPE 
    UPDATE gme_batch_dtls_interface
    SET    process_status = PROCESS_STATUS_BV_INVALID
    WHERE  (line_no IS NULL OR line_type IS NULL)
    AND    load_type = LOADTYPE_DETAIL_ADD
    AND    object_type = OBJECTYPE_MATERIAL
    AND    group_id = p_group_id
    AND    process_status = PROCESS_STATUS_RUNNING;

    IF sql%rowcount > 0 THEN
      gme_common_pvt.log_message('GME_LINE_IDENTIFIER_ERROR');
      batchdtls_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_LINE_IDENTIFIER_ERROR');
    END IF;

    --bulk validation for BY_PRODUCT_TYPE
    UPDATE gme_batch_dtls_interface intf
    SET    process_status = PROCESS_STATUS_BV_INVALID
    WHERE  NOT EXISTS
        (SELECT 1 
         FROM   gem_lookup_values lv
         WHERE  lv.lookup_type = 'GMD_BY_PRODUCT_TYPE' 
         AND    lv.lookup_code = intf.by_product_type)
    AND   line_type = 2
    AND   by_product_type IS NOT NULL
    AND   load_type = LOADTYPE_DETAIL_ADD
    AND   object_type = OBJECTYPE_MATERIAL
    AND   group_id = p_group_id
    AND   process_status = PROCESS_STATUS_RUNNING;

    IF sql%rowcount > 0 THEN
      gme_common_pvt.log_message('GME_INVALID_FIELD', 'FIELD', 'BY_PRODUCT_TYPE');
      batchdtls_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_INVALID_FIELD'
                               ,p_column_name  => 'BY_PRODUCT_TYPE'
                               ,p_param1       => 'FIELD'
                               ,p_token1       => 'BY_PRODUCT_TYPE');
    END IF;

    --bulk validation for DTL_UM
    UPDATE gme_batch_dtls_interface intf
    SET process_status = PROCESS_STATUS_BV_INVALID
    WHERE NOT EXISTS
      (SELECT 1
       FROM   mtl_units_of_measure m
       WHERE  m.uom_code = intf.dtl_um
       AND    (m.disable_date > sysdate OR m.disable_date IS NULL)
    )
    AND   (load_type = LOADTYPE_DETAIL_ADD OR 
            (load_type = LOADTYPE_DETAIL_UPDATE AND dtl_um IS NOT NULL))
    AND   object_type = OBJECTYPE_MATERIAL
    AND   group_id = p_group_id
    AND   process_status = PROCESS_STATUS_RUNNING;

    IF sql%rowcount > 0 THEN
      gme_common_pvt.log_message('GME_INVALID_FIELD', 'FIELD', 'DTL_UM');
      batchdtls_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_INVALID_FIELD'
                               ,p_column_name  => 'DTL_UM'
                               ,p_param1       => 'FIELD'
                               ,p_token1       => 'DTL_UM');
    END IF;

    --bulk validation for SUBINVENTORY
    UPDATE gme_batch_dtls_interface intf
    SET   process_status = PROCESS_STATUS_BV_INVALID 
    WHERE NOT EXISTS
          (
           SELECT 1
           FROM mtl_secondary_inventories si
           WHERE si.secondary_inventory_name = intf.subinventory
           AND   si.organization_id = intf.int_organization_id
          )
    AND   load_type IN (LOADTYPE_DETAIL_ADD, LOADTYPE_DETAIL_UPDATE)
    AND   object_type = OBJECTYPE_MATERIAL
    AND   subinventory IS NOT NULL
    AND   int_locator_id IS NULL
    AND   group_id = p_group_id
    AND   process_status = PROCESS_STATUS_RUNNING;

    IF sql%rowcount > 0 THEN
      gme_common_pvt.log_message('GME_INVALID_FIELD', 'FIELD', 'SUBINVENTORY');
      batchdtls_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_INVALID_FIELD'
                               ,p_column_name  => 'SUBINVENTORY'
                               ,p_param1       => 'FIELD'
                               ,p_token1       => 'SUBINVENTORY');
    END IF;

    --bulk validation for LOCATOR, LOCATOR_ID
    UPDATE gme_batch_dtls_interface intf
    SET int_locator_id = 
        (SELECT l.inventory_location_id
         FROM   mtl_item_locations_kfv l
         WHERE l.inventory_location_id = intf.locator_id
         OR    (l.concatenated_segments = intf.locator
                AND l.organization_id = intf.int_organization_id 
                AND l.subinventory_code = intf.subinventory
                AND intf.locator_id IS NULL)
        )
    WHERE load_type IN (LOADTYPE_DETAIL_ADD, LOADTYPE_DETAIL_UPDATE)
    AND   object_type = OBJECTYPE_MATERIAL
    AND   (locator_id IS NOT NULL OR locator IS NOT NULL)
    AND   group_id = p_group_id
    AND   process_status = PROCESS_STATUS_RUNNING;    
    
    UPDATE gme_batch_dtls_interface
    SET    process_status = PROCESS_STATUS_BV_INVALID
    WHERE  load_type IN (LOADTYPE_DETAIL_ADD, LOADTYPE_DETAIL_UPDATE)
    AND    object_type = OBJECTYPE_MATERIAL
    AND    int_locator_id IS NULL
    AND    (locator_id IS NOT NULL OR locator IS NOT NULL)
    AND    group_id = p_group_id
    AND    process_status = PROCESS_STATUS_RUNNING;

    IF sql%rowcount > 0 THEN
      gme_common_pvt.log_message('GME_LOCATOR_ID_NOTFOUND');
      batchdtls_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_LOCATOR_ID_NOTFOUND');
    END IF;

    --bulk validation for PHANTOM_TYPE
    UPDATE gme_batch_dtls_interface
    SET   process_status = PROCESS_STATUS_BV_INVALID 
    WHERE phantom_type IS NULL
    AND   line_type = gme_common_pvt.g_line_type_ing
    AND   load_type = LOADTYPE_DETAIL_ADD
    AND   object_type = OBJECTYPE_MATERIAL
    AND   group_id = p_group_id
    AND   process_status = PROCESS_STATUS_RUNNING;

    IF sql%rowcount > 0 THEN
      gme_common_pvt.log_message('GME_INV_PHANTOM_TYPE');
      batchdtls_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_INV_PHANTOM_TYPE'
                               ,p_column_name  => 'PHANTOM_TYPE');
    END IF;

    --bulk validation for PLAN_QTY
    UPDATE gme_batch_dtls_interface intf
    SET   process_status = PROCESS_STATUS_BV_INVALID 
    WHERE EXISTS
    (SELECT 1
     FROM   gme_batch_header bh
     WHERE  bh.batch_id =  intf.int_batch_id
     AND    bh.batch_status = gme_common_pvt.g_batch_pending
     AND    intf.plan_qty IS NULL
    )
    AND   object_type = OBJECTYPE_MATERIAL
    AND   load_type = LOADTYPE_DETAIL_ADD
    AND   group_id = p_group_id
    AND   process_status = PROCESS_STATUS_RUNNING;

    IF sql%rowcount > 0 THEN
      gme_common_pvt.log_message('GME_INVALID_PLAN_QTY');
      batchdtls_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_INVALID_PLAN_QTY'
                               ,p_column_name  => 'PLAN_QTY');
    END IF;

    --bulk validation for SCALE_TYPE
    UPDATE gme_batch_dtls_interface
    SET   process_status = PROCESS_STATUS_BV_INVALID 
    WHERE scale_type is null
    AND   object_type = OBJECTYPE_MATERIAL
    AND   load_type = LOADTYPE_DETAIL_ADD
    AND   group_id = p_group_id
    AND   process_status = PROCESS_STATUS_RUNNING;

    IF sql%rowcount > 0 THEN
      gme_common_pvt.log_message('GME_INVALID_SCALE_TYPE');
      batchdtls_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_INVALID_SCALE_TYPE'
                               ,p_column_name  => 'SCALE_TYPE');
    END IF;

    --bulk validation for ROUNDING_DIRECTION
    UPDATE gme_batch_dtls_interface
    SET   process_status = PROCESS_STATUS_BV_INVALID 
    WHERE rounding_direction IS NULL
    AND   scale_type = 2
    AND   object_type = OBJECTYPE_MATERIAL
    AND   load_type = LOADTYPE_DETAIL_ADD
    AND   group_id = p_group_id
    AND   process_status = PROCESS_STATUS_RUNNING;

    IF sql%rowcount > 0 THEN
      gme_common_pvt.log_message('GME_INVALID_ROUND_DIR');
      batchdtls_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_INVALID_ROUND_DIR'
                               ,p_column_name  => 'ROUNDING_DIRECTION');
    END IF;
  ELSIF p_object_type = OBJECTYPE_STEP THEN
    --update the three fields when batchstep_id is valid
    UPDATE gme_batch_dtls_interface intf
    SET (int_batch_id, int_organization_id, int_batchstep_id) = 
        (SELECT bs.batch_id, bh.organization_id, bs.batchstep_id
         FROM   gme_batch_steps bs, gme_batch_header bh
         WHERE  bs.batchstep_id = intf.batchstep_id
         AND    bs.batch_id = bh.batch_id
         AND    bh.migrated_batch_ind = 'N'
         AND    bh.delete_mark = 0
         AND    bs.delete_mark = 0)
    WHERE batchstep_id IS NOT NULL
    AND   object_type = OBJECTYPE_STEP
    AND   (load_type = LOADTYPE_DETAIL_UPDATE OR load_type = LOADTYPE_DETAIL_DELETE)
    AND   group_id = p_group_id
    AND   process_status = PROCESS_STATUS_RUNNING;
    
    UPDATE gme_batch_dtls_interface intf
    SET int_organization_id = 
        (SELECT mp.organization_id
         FROM   mtl_parameters mp
         WHERE  mp.organization_code = intf.organization_code)
    WHERE organization_code IS NOT NULL
    AND   int_organization_id IS NULL
    AND   object_type = OBJECTYPE_STEP
    AND   group_id = p_group_id
    AND   process_status = PROCESS_STATUS_RUNNING;
    
    --update the three fields when int_batch_id or (batch_no, organization_code), and batchstep_no are valid
    UPDATE gme_batch_dtls_interface intf
    SET (int_batch_id, int_batchstep_id) = 
        (SELECT bs.batch_id, bs.batchstep_id
         FROM   gme_batch_steps bs, gme_batch_header bh
         WHERE (bs.batch_id = intf.int_batch_id  
                OR (bh.batch_no = intf.batch_no
                    AND bh.organization_id = intf.int_organization_id
                    AND intf.int_batch_id IS NULL))
         AND bh.migrated_batch_ind = 'N'
         AND bh.delete_mark = 0
         AND bh.batch_type = 0
         AND bs.batch_id = bh.batch_id
         AND bs.batchstep_no = intf.batchstep_no
         AND bs.delete_mark = 0)
    WHERE ((int_organization_id IS NOT NULL AND batch_no IS NOT NULL) OR int_batch_id IS NOT NULL)
    AND   batchstep_no IS NOT NULL
    AND   object_type = OBJECTYPE_STEP
    AND   load_type <> LOADTYPE_DETAIL_ADD
    AND   group_id = p_group_id
    AND   process_status = PROCESS_STATUS_RUNNING;
    
    UPDATE gme_batch_dtls_interface intf
    SET   process_status = PROCESS_STATUS_BV_INVALID 
    WHERE int_batchstep_id IS NULL
    AND   object_type = OBJECTYPE_STEP
    AND   load_type <> LOADTYPE_DETAIL_ADD
    AND   group_id = p_group_id
    AND   process_status = PROCESS_STATUS_RUNNING;

    IF sql%rowcount > 0 THEN
      gme_common_pvt.log_message('GME_BATCHSTEPID_NOTFOUND');
      batchdtls_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_BATCHSTEPID_NOTFOUND');
    END IF;

    --update batch infor when batch no and organization_code are valid
    UPDATE gme_batch_dtls_interface intf
    SET (int_batch_id, int_organization_id) = 
        (SELECT bh.batch_id, bh.organization_id
         FROM   gme_batch_header bh
         WHERE  (bh.batch_id = intf.batch_id
                 OR (bh.batch_no = intf.batch_no
                     AND bh.organization_id = intf.int_organization_id
                     AND intf.batch_id IS NULL))
         AND bh.batch_type = 0
         AND bh.delete_mark = 0
         AND bh.migrated_batch_ind = 'N')
    WHERE ((int_organization_id IS NOT NULL AND batch_no IS NOT NULL) OR batch_id IS NOT NULL)
    AND   int_batch_id IS NULL
    AND   object_type = OBJECTYPE_STEP
    AND   load_type = LOADTYPE_DETAIL_ADD
    AND   group_id = p_group_id
    AND   process_status = PROCESS_STATUS_RUNNING;
    
    UPDATE gme_batch_dtls_interface intf
    SET   process_status = PROCESS_STATUS_BV_INVALID 
    WHERE int_batch_id IS NULL
    AND   object_type = OBJECTYPE_STEP
    AND   load_type = LOADTYPE_DETAIL_ADD
    AND   group_id = p_group_id
    AND   process_status = PROCESS_STATUS_RUNNING;

    IF sql%rowcount > 0 THEN
      gme_common_pvt.log_message('GME_BATCHID_NOTFOUND');
      batchdtls_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_BATCHID_NOTFOUND');
    END IF;

    --bulk validation for BATCHSTEP_NO
    UPDATE gme_batch_dtls_interface intf
    SET process_status = PROCESS_STATUS_BV_INVALID
    WHERE EXISTS
      (SELECT 1
       FROM   gme_batch_steps s
       WHERE  s.batchstep_no = intf.batchstep_no
       AND    s.batch_id = intf.int_batch_id
       AND    s.delete_mark = 0
    )
    AND   load_type = LOADTYPE_DETAIL_ADD
    AND   object_type = OBJECTYPE_STEP
    AND   group_id = p_group_id
    AND   process_status = PROCESS_STATUS_RUNNING;

    IF sql%rowcount > 0 THEN
      gme_common_pvt.log_message('PC_DUPLICATESTEPNO');
      batchdtls_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'PC_DUPLICATESTEPNO'
                               ,p_column_name  => 'BATCHSTEP_NO');
    END IF;

    --Bulk validtion for batch status when inserting or updating step
    UPDATE gme_batch_dtls_interface intf
    SET process_status = PROCESS_STATUS_BV_INVALID
    WHERE EXISTS
      (SELECT 1
       FROM   gme_batch_header h
       WHERE  ((h.batch_status NOT IN (gme_common_pvt.g_batch_pending, gme_common_pvt.g_batch_wip)
                AND intf.load_type = LOADTYPE_DETAIL_ADD)
               OR
               (h.batch_status IN (gme_common_pvt.g_batch_closed, gme_common_pvt.g_batch_cancelled)
                AND intf.load_type = LOADTYPE_DETAIL_UPDATE))
       AND    h.batch_id = intf.int_batch_id)
    AND   object_type = OBJECTYPE_STEP
    AND   load_type IN (LOADTYPE_DETAIL_ADD, LOADTYPE_DETAIL_UPDATE)
    AND   group_id = p_group_id
    AND   process_status = PROCESS_STATUS_RUNNING;

    IF sql%rowcount > 0 THEN
      gme_common_pvt.log_message('GME_INV_BATCH_STATUS_OPER');
      batchdtls_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_INV_BATCH_STATUS_OPER');
    END IF;
	
    --Bulk validtion for step status when updating or deleting step
    UPDATE gme_batch_dtls_interface intf
    SET process_status = PROCESS_STATUS_BV_INVALID
    WHERE EXISTS
      (SELECT 1
       FROM   gme_batch_steps s
       WHERE  ((s.step_status IN (gme_common_pvt.g_step_closed, gme_common_pvt.g_step_cancelled) 
	            AND intf.load_type = LOADTYPE_DETAIL_UPDATE)
			    OR 
			   (s.step_status <> gme_common_pvt.g_step_pending
	            AND intf.load_type = LOADTYPE_DETAIL_DELETE))
       AND    s.batchstep_id = intf.int_batchstep_id)
    AND   object_type = OBJECTYPE_STEP
    AND   load_type IN (LOADTYPE_DETAIL_UPDATE, LOADTYPE_DETAIL_DELETE)
    AND   group_id = p_group_id
    AND   process_status = PROCESS_STATUS_RUNNING;

    IF sql%rowcount > 0 THEN
      gme_common_pvt.log_message('PC_STEP_STATUS_ERR');
      batchdtls_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'PC_STEP_STATUS_ERR');
    END IF;
    
    --bulk validation for OPRN_ID, OPRN_NO, OPRN_VERS
    UPDATE gme_batch_dtls_interface intf
    SET int_oprn_id = 
        (SELECT oprn_id 
         FROM gmd_operations o
         WHERE (o.oprn_id = intf.oprn_id
                OR (o.oprn_no = intf.oprn_no
                    AND o.oprn_vers = intf.oprn_vers
                    AND intf.oprn_id IS NULL))
         AND (o.operation_status BETWEEN 700 AND 799
              OR o.operation_status BETWEEN 900 AND 999))
    WHERE load_type = LOADTYPE_DETAIL_ADD
    AND   object_type = OBJECTYPE_STEP
    AND   group_id = p_group_id
    AND   process_status = PROCESS_STATUS_RUNNING;
    
    UPDATE gme_batch_dtls_interface intf
    SET    process_status = PROCESS_STATUS_BV_INVALID
    WHERE  int_oprn_id IS NULL
    AND    load_type = LOADTYPE_DETAIL_ADD
    AND    object_type = OBJECTYPE_STEP
    AND    group_id = p_group_id
    AND    process_status = PROCESS_STATUS_RUNNING;

    IF sql%rowcount > 0 THEN
      gme_common_pvt.log_message('GME_OPRN_ID_NOTFOUND');
      batchdtls_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_OPRN_ID_NOTFOUND');
    END IF;
    
    --bulk validation for PLAN_START_DATE, PLAN_CMPLT_DATE
    UPDATE gme_batch_dtls_interface
    SET    process_status = PROCESS_STATUS_BV_INVALID
    WHERE  plan_start_date > plan_cmplt_date
    AND    object_type = OBJECTYPE_STEP
    AND    group_id = p_group_id
    AND    process_status = PROCESS_STATUS_RUNNING;

    IF sql%rowcount > 0 THEN
      gme_common_pvt.log_message('GME_CMPLT_START_DT');
      batchdtls_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_CMPLT_START_DT');
    END IF;

    --bulk validation for PLAN_STEP_QTY
    UPDATE gme_batch_dtls_interface intf
    SET    process_status = PROCESS_STATUS_BV_INVALID
    WHERE  EXISTS(
            SELECT 1
            FROM   gme_batch_header bh
            WHERE  bh.batch_id = intf.int_batch_id
            AND    bh.automatic_step_calculation <> 1
           )
    AND    plan_step_qty IS NULL
    AND    load_type = LOADTYPE_DETAIL_ADD
    AND    object_type = OBJECTYPE_STEP
    AND    group_id = p_group_id
    AND    process_status = PROCESS_STATUS_RUNNING;

    IF sql%rowcount > 0 THEN
      gme_common_pvt.log_message('GME_INVALID_PLAN_QTY');
      batchdtls_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_INVALID_PLAN_QTY'
                               ,p_column_name  => 'PLAN_STEP_QTY');
    END IF;

    --bulk validation for STEPRELEASE_TYPE
    UPDATE gme_batch_dtls_interface
    SET    process_status = PROCESS_STATUS_BV_INVALID
    WHERE  steprelease_type IS NULL
    AND    load_type = LOADTYPE_DETAIL_ADD
    AND    object_type = OBJECTYPE_STEP
    AND    group_id = p_group_id
    AND    process_status = PROCESS_STATUS_RUNNING;

    IF sql%rowcount > 0 THEN
      gme_common_pvt.log_message('GME_INVALID_STEPRELEASE');
      batchdtls_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_INVALID_STEPRELEASE'
                               ,p_column_name  => 'STEPRELEASE_TYPE');
    END IF;

    --bulk validation for ACTUAL_START_DATE, ACTUAL_CMPLT_DATE
    UPDATE gme_batch_dtls_interface
    SET    process_status = PROCESS_STATUS_BV_INVALID
    WHERE  actual_start_date > actual_cmplt_date
    AND    load_type = LOADTYPE_DETAIL_UPDATE
    AND    object_type = OBJECTYPE_STEP
    AND    group_id = p_group_id
    AND    process_status = PROCESS_STATUS_RUNNING;

    IF sql%rowcount > 0 THEN
      gme_common_pvt.log_message('GME_ACTUAL_CMPLT_START_DT');
      batchdtls_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_ACTUAL_CMPLT_START_DT');
    END IF;

    --bulk validation for RESCHEDULE_PRECEDING_STEPS
    UPDATE gme_batch_dtls_interface
    SET    process_status = PROCESS_STATUS_BV_INVALID
    WHERE  reschedule_preceding_steps IS NULL
    AND    load_type = LOADTYPE_DETAIL_RESHCEDULE
    AND    object_type = OBJECTYPE_STEP
    AND    group_id = p_group_id
    AND    process_status = PROCESS_STATUS_RUNNING;

    IF sql%rowcount > 0 THEN
      gme_common_pvt.log_message('GME_INVALID_FIELD', 'FIELD', 'RESCHEDULE_PRECEDING_STEPS');
      batchdtls_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_INVALID_FIELD'
                               ,p_column_name  => 'RESCHEDULE_PRECEDING_STEPS'
                               ,p_param1       => 'FIELD'
                               ,p_token1       => 'RESCHEDULE_PRECEDING_STEPS');
    END IF;

    --bulk validation for RESCHEDULE_SUCCEEDING_STEPS
    UPDATE gme_batch_dtls_interface
    SET    process_status = PROCESS_STATUS_BV_INVALID
    WHERE  reschedule_succeeding_steps IS NULL
    AND    load_type = LOADTYPE_DETAIL_RESHCEDULE
    AND    object_type = OBJECTYPE_STEP
    AND    group_id = p_group_id
    AND    process_status = PROCESS_STATUS_RUNNING;

    IF sql%rowcount > 0 THEN
      gme_common_pvt.log_message('GME_INVALID_FIELD', 'FIELD', 'RESCHEDULE_SUCCEEDING_STEPS');
      batchdtls_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_INVALID_FIELD'
                               ,p_column_name  => 'RESCHEDULE_SUCCEEDING_STEPS'
                               ,p_param1       => 'FIELD'
                               ,p_token1       => 'RESCHEDULE_SUCCEEDING_STEPS');
    END IF;

    --bulk validation for USE_WORK_DAY_CAL
    UPDATE gme_batch_dtls_interface
    SET    process_status = PROCESS_STATUS_BV_INVALID
    WHERE  use_work_day_cal IS NULL
    AND    load_type = LOADTYPE_DETAIL_RESHCEDULE
    AND    object_type = OBJECTYPE_STEP
    AND    group_id = p_group_id
    AND    process_status = PROCESS_STATUS_RUNNING;

    IF sql%rowcount > 0 THEN
      gme_common_pvt.log_message('GME_INVALID_FIELD', 'FIELD', 'USE_WORK_DAY_CAL');
      batchdtls_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_INVALID_FIELD'
                               ,p_column_name  => 'USE_WORK_DAY_CAL'
                               ,p_param1       => 'FIELD'
                               ,p_token1       => 'USE_WORK_DAY_CAL');
    END IF;
  ELSIF p_object_type = OBJECTYPE_ACTIVITY THEN
    --bulk validation for LOAD_TYPE
    UPDATE gme_batch_dtls_interface
    SET    process_status = PROCESS_STATUS_BV_INVALID
    WHERE  load_type NOT IN (LOADTYPE_DETAIL_ADD, LOADTYPE_DETAIL_UPDATE, LOADTYPE_DETAIL_DELETE)
    AND    object_type = OBJECTYPE_ACTIVITY
    AND    group_id = p_group_id
    AND    process_status = PROCESS_STATUS_RUNNING;

    IF sql%rowcount > 0 THEN
      gme_common_pvt.log_message('GME_INVALID_FIELD', 'FIELD', 'LOAD_TYPE');
      batchdtls_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_INVALID_FIELD'
                               ,p_column_name  => 'LOAD_TYPE'
                               ,p_param1       => 'FIELD'
                               ,p_token1       => 'LOAD_TYPE');
    END IF;

    --bulk validation for batchstep_activity_id
    UPDATE gme_batch_dtls_interface intf
    SET (int_batch_id, int_organization_id, int_batchstep_id, int_batchstep_activity_id) = 
        (SELECT a.batch_id, b.organization_id, a.batchstep_id, a.batchstep_activity_id
         FROM gme_batch_step_activities a, gme_batch_header b
         WHERE a.batchstep_activity_id = intf.batchstep_activity_id
         AND   a.batch_id = b.batch_id
         AND   b.batch_type = 0
         AND   b.delete_mark = 0
         AND   b.migrated_batch_ind = 'N')
    WHERE (load_type = LOADTYPE_DETAIL_UPDATE OR load_type = LOADTYPE_DETAIL_DELETE)
    AND   object_type = OBJECTYPE_ACTIVITY
    AND   batchstep_activity_id IS NOT NULL
    AND   group_id = p_group_id
    AND   process_status = PROCESS_STATUS_RUNNING;

    UPDATE gme_batch_dtls_interface intf
    SET int_organization_id = 
        (SELECT mp.organization_id
         FROM   mtl_parameters mp
         WHERE  mp.organization_code = intf.organization_code)
    WHERE organization_code IS NOT NULL
    AND   int_organization_id IS NULL
    AND   (object_type = OBJECTYPE_ACTIVITY
           OR (object_type = OBJECTYPE_RESOURCE AND load_type = LOADTYPE_DETAIL_ADD))
    AND   group_id = p_group_id
    AND   process_status = PROCESS_STATUS_RUNNING;
    
    --bulk validation for all batch_no, batchstep_no, activity
    UPDATE gme_batch_dtls_interface intf
       SET (int_batchstep_activity_id, int_batch_id, int_batchstep_id) =
           (SELECT MIN(act.batchstep_activity_id), act.batch_id, act.batchstep_id
              FROM gme_batch_header gbh,
                   gme_batch_steps step,
                   gme_batch_step_activities act
             WHERE (gbh.batch_id = intf.int_batch_id
                    OR (intf.int_organization_id = gbh.organization_id
                        AND intf.batch_no = gbh.batch_no
                        AND intf.int_batch_id IS NULL))
                   AND gbh.batch_type = 0
                   AND gbh.delete_mark = 0
                   AND gbh.migrated_batch_ind = 'N'
                   AND step.batchstep_no = intf.batchstep_no
                   AND step.batch_id = gbh.batch_id
                   AND act.activity = intf.activity
                   AND act.batch_id = gbh.batch_id
                   AND act.batchstep_id = step.batchstep_id
             GROUP BY  act.batch_id, act.batchstep_id)
     WHERE intf.process_status = PROCESS_STATUS_RUNNING
       AND intf.int_batchstep_activity_id IS NULL
       AND intf.object_type = OBJECTYPE_ACTIVITY                                    
       AND intf.load_type IN (LOADTYPE_DETAIL_UPDATE, LOADTYPE_DETAIL_DELETE)
       AND intf.group_id = p_group_id;
    
    UPDATE gme_batch_dtls_interface
    SET    process_status = PROCESS_STATUS_BV_INVALID
    WHERE  int_batchstep_activity_id IS NULL
    AND    load_type IN (LOADTYPE_DETAIL_UPDATE, LOADTYPE_DETAIL_DELETE)
    AND    object_type = OBJECTYPE_ACTIVITY
    AND    group_id = p_group_id
    AND    process_status = PROCESS_STATUS_RUNNING;	

    IF sql%rowcount > 0 THEN
      gme_common_pvt.log_message('GME_STEP_ACTIVITYID_NOTFOUND');
      batchdtls_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_STEP_ACTIVITYID_NOTFOUND');
    END IF;

    --bulk validation for batchstep_id or batchstep_no
    UPDATE gme_batch_dtls_interface intf
       SET (int_batch_id, int_batchstep_id) =
           (SELECT step.batch_id, step.batchstep_id
              FROM gme_batch_header gbh,
                   gme_batch_steps step
             WHERE (step.batchstep_id = intf.batchstep_id 
                    OR ((gbh.batch_id = intf.int_batch_id
                         OR (intf.int_organization_id = gbh.organization_id
                             AND intf.batch_no = gbh.batch_no
                             AND intf.int_batch_id IS NULL))
                        AND intf.batchstep_no = step.batchstep_no
                        AND intf.batchstep_id IS NULL))
               AND gbh.batch_type = 0
               AND gbh.delete_mark = 0
               AND gbh.migrated_batch_ind = 'N'
               AND gbh.batch_id = step.batch_id)
     WHERE intf.process_status = PROCESS_STATUS_RUNNING
       AND intf.int_batchstep_id IS NULL
       AND intf.object_type IN (OBJECTYPE_ACTIVITY, OBJECTYPE_RESOURCE)
       AND intf.load_type = LOADTYPE_DETAIL_ADD
       AND intf.group_id = p_group_id;

    UPDATE gme_batch_dtls_interface
    SET    process_status = PROCESS_STATUS_BV_INVALID
    WHERE  int_batchstep_id IS NULL
    AND    load_type = LOADTYPE_DETAIL_ADD
    AND    object_type IN (OBJECTYPE_ACTIVITY, OBJECTYPE_RESOURCE)
    AND    group_id = p_group_id
    AND    process_status = PROCESS_STATUS_RUNNING;	

    IF sql%rowcount > 0 THEN
      gme_common_pvt.log_message('GME_BATCHSTEPID_NOTFOUND');
      batchdtls_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_BATCHSTEPID_NOTFOUND');
    END IF;

    --Bulk validtion for batch status when deleting activity
    UPDATE gme_batch_dtls_interface intf
    SET process_status = PROCESS_STATUS_BV_INVALID
    WHERE EXISTS
      (SELECT 1
       FROM   gme_batch_header h
       WHERE  h.batch_status <> gme_common_pvt.g_batch_pending
       AND    h.batch_id = intf.int_batch_id)
    AND   object_type = OBJECTYPE_ACTIVITY
    AND   load_type = LOADTYPE_DETAIL_DELETE
    AND   group_id = p_group_id
    AND   process_status = PROCESS_STATUS_RUNNING;

    IF sql%rowcount > 0 THEN
      gme_common_pvt.log_message('GME_INV_BATCH_STATUS_OPER');
      batchdtls_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_INV_BATCH_STATUS_OPER');
    END IF;

    --Bulk validtion for step status when inserting or updating activity
    UPDATE gme_batch_dtls_interface intf
    SET process_status = PROCESS_STATUS_BV_INVALID
    WHERE EXISTS
      (SELECT 1
       FROM   gme_batch_steps s
       WHERE  s.step_status IN (gme_common_pvt.g_step_closed, gme_common_pvt.g_step_cancelled)
       AND    s.batchstep_id = intf.int_batchstep_id)
    AND   object_type = OBJECTYPE_ACTIVITY
    AND   load_type IN (LOADTYPE_DETAIL_ADD, LOADTYPE_DETAIL_UPDATE)
    AND   group_id = p_group_id
    AND   process_status = PROCESS_STATUS_RUNNING;

    IF sql%rowcount > 0 THEN
      gme_common_pvt.log_message('PC_STEP_STATUS_ERR');
      batchdtls_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'PC_STEP_STATUS_ERR');
    END IF;

    --Bulk validtion for step status and batch automatic_step_calculation
    UPDATE gme_batch_dtls_interface intf
    SET process_status = PROCESS_STATUS_BV_INVALID
    WHERE EXISTS
      (SELECT 1
       FROM   gme_batch_steps bs, gme_batch_header bh
       WHERE  bh.batch_id = intf.int_batch_id
       AND    bs.batchstep_id = intf.int_batchstep_id
       AND    bs.step_status = gme_common_pvt.g_step_wip
       AND    bh.automatic_step_calculation = 1
      )
    AND   object_type = OBJECTYPE_ACTIVITY
    AND   load_type = LOADTYPE_DETAIL_ADD
    AND   group_id = p_group_id
    AND   process_status = PROCESS_STATUS_RUNNING;

    IF sql%rowcount > 0 THEN
      gme_common_pvt.log_message('GME_INVALID_ASQC_ACTION_ACTV');
      batchdtls_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_INVALID_ASQC_ACTION_ACTV');
    END IF;

    UPDATE gme_batch_dtls_interface intf
    SET process_status = PROCESS_STATUS_BV_INVALID
    WHERE EXISTS
      (SELECT 1
       FROM   gme_batch_steps bs, gme_batch_header bh
       WHERE  bh.batch_id = intf.int_batch_id
       AND    bs.batchstep_id = intf.int_batchstep_id
       AND    bs.step_status = gme_common_pvt.g_step_wip
       AND    bh.automatic_step_calculation = 1
      )
    AND   object_type = OBJECTYPE_RESOURCE
    AND   load_type = LOADTYPE_DETAIL_ADD
    AND   int_batchstep_id IS NOT NULL
    AND   group_id = p_group_id
    AND   process_status = PROCESS_STATUS_RUNNING;

    IF sql%rowcount > 0 THEN
      gme_common_pvt.log_message('GME_INVALID_ASQC_ACTION');
      batchdtls_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_INVALID_ASQC_ACTION');
    END IF;

    --bulk validation for ACTIVITY
    UPDATE gme_batch_dtls_interface intf
    SET    process_status = PROCESS_STATUS_BV_INVALID
    WHERE  NOT EXISTS
           (
            SELECT 1
            FROM   gmd_activities a
            WHERE  a.activity = intf.activity
            AND    a.delete_mark = 0)
    AND    load_type = LOADTYPE_DETAIL_ADD
    AND    object_type IN (OBJECTYPE_ACTIVITY, OBJECTYPE_RESOURCE)
    AND    int_batchstep_id IS NOT NULL
    AND    group_id = p_group_id
    AND    process_status = PROCESS_STATUS_RUNNING;

    IF sql%rowcount > 0 THEN
      gme_common_pvt.log_message('GME_INVALID_FIELD', 'FIELD', 'ACTIVITY');
      batchdtls_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_INVALID_FIELD'
                               ,p_column_name  => 'ACTIVITY'
                               ,p_param1       => 'FIELD'
                               ,p_token1       => 'ACTIVITY');
    END IF;

    --bulk validation for PLAN_ACTIVITY_FACTOR
    UPDATE gme_batch_dtls_interface intf
    SET    process_status = PROCESS_STATUS_BV_INVALID
    WHERE  EXISTS
    (SELECT 1
     FROM   gme_batch_steps bs
     WHERE  bs.step_status = gme_common_pvt.g_step_pending
     AND    bs.batchstep_id = intf.int_batchstep_id
     AND    NVL(intf.plan_activity_factor, 0) = 0
    )
    AND    load_type = LOADTYPE_DETAIL_ADD
    AND    object_type = OBJECTYPE_ACTIVITY
    AND    group_id = p_group_id
    AND    process_status = PROCESS_STATUS_RUNNING;

    IF sql%rowcount > 0 THEN
      gme_common_pvt.log_message('GME_INVALID_FIELD', 'FIELD', 'PLAN_ACTIVITY_FACTOR');
      batchdtls_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_INVALID_FIELD'
                               ,p_column_name  => 'PLAN_ACTIVITY_FACTOR'
                               ,p_param1       => 'FIELD'
                               ,p_token1       => 'PLAN_ACTIVITY_FACTOR');
    END IF;

    --bulk validation for ACTUAL_ACTIVITY_FACTOR
    UPDATE gme_batch_dtls_interface intf
    SET    process_status = PROCESS_STATUS_BV_INVALID
    WHERE  EXISTS
    (SELECT 1
     FROM   gme_batch_steps bs
     WHERE  bs.step_status IN (gme_common_pvt.g_step_wip, gme_common_pvt.g_step_completed)
     AND    bs.batchstep_id = intf.int_batchstep_id
     AND    NVL(intf.actual_activity_factor, 0) = 0
    )
    AND    load_type = LOADTYPE_DETAIL_ADD
    AND    object_type = OBJECTYPE_ACTIVITY
    AND    group_id = p_group_id
    AND    process_status = PROCESS_STATUS_RUNNING;

    IF sql%rowcount > 0 THEN
      gme_common_pvt.log_message('GME_INVALID_FIELD', 'FIELD', 'ACTUAL_ACTIVITY_FACTOR');
      batchdtls_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_INVALID_FIELD'
                               ,p_column_name  => 'ACTUAL_ACTIVITY_FACTOR'
                               ,p_param1       => 'FIELD'
                               ,p_token1       => 'ACTUAL_ACTIVITY_FACTOR');
    END IF;

    --bulk validation for resources, when no resource added to the activity
    UPDATE gme_batch_dtls_interface intfa
    SET    process_status = PROCESS_STATUS_BV_INVALID
    WHERE  NOT EXISTS
           (SELECT 1
            FROM   gme_batch_dtls_interface intfr
            WHERE  intfa.int_batchstep_id = intfr.int_batchstep_id
            AND    intfa.activity = intfr.activity
            AND    intfr.load_type = LOADTYPE_DETAIL_ADD
            AND    intfr.object_type = OBJECTYPE_RESOURCE
            AND    intfr.process_status = PROCESS_STATUS_RUNNING)
    AND    load_type = LOADTYPE_DETAIL_ADD
    AND    object_type = OBJECTYPE_ACTIVITY
    AND    group_id = p_group_id
    AND    process_status = PROCESS_STATUS_RUNNING;

    IF sql%rowcount > 0 THEN
      gme_common_pvt.log_message('GME_RESOURCE_REQUIRED');
      batchdtls_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_RESOURCE_REQUIRED');
    END IF;

    --bulk validation for PLAN_START_DATE, PLAN_CMPLT_DATE
    UPDATE gme_batch_dtls_interface
    SET    process_status = PROCESS_STATUS_BV_INVALID
    WHERE  plan_start_date > plan_cmplt_date
    AND    object_type IN (OBJECTYPE_ACTIVITY, OBJECTYPE_RESOURCE)
    AND    int_batchstep_id IS NOT NULL
    AND    group_id = p_group_id
    AND    process_status = PROCESS_STATUS_RUNNING;

    IF sql%rowcount > 0 THEN
      gme_common_pvt.log_message('GME_CMPLT_START_DT');
      batchdtls_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_CMPLT_START_DT');
    END IF;

    --bulk validation for ACTUAL_START_DATE, ACTUAL_CMPLT_DATE
    UPDATE gme_batch_dtls_interface
    SET    process_status = PROCESS_STATUS_BV_INVALID
    WHERE  actual_start_date > actual_cmplt_date
    AND    load_type = LOADTYPE_DETAIL_UPDATE
    AND    object_type = OBJECTYPE_ACTIVITY
    AND    group_id = p_group_id
    AND    process_status = PROCESS_STATUS_RUNNING;

    IF sql%rowcount > 0 THEN
      gme_common_pvt.log_message('GME_ACTUAL_CMPLT_START_DT');
      batchdtls_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_ACTUAL_CMPLT_START_DT');
    END IF;

    --bulk validation for adding resources's RESOURCES
    UPDATE gme_batch_dtls_interface intf
    SET    process_status = PROCESS_STATUS_BV_INVALID
    WHERE  NOT EXISTS
           (
            SELECT 1
            FROM   cr_rsrc_mst r
            WHERE  r.resources = intf.resources
            AND    r.delete_mark = 0)
    AND    load_type = LOADTYPE_DETAIL_ADD
    AND    object_type = OBJECTYPE_RESOURCE
    AND    int_batchstep_id IS NOT NULL
    AND    group_id = p_group_id
    AND    process_status = PROCESS_STATUS_RUNNING;

    IF sql%rowcount > 0 THEN
      gme_common_pvt.log_message('GME_INVALID_FIELD', 'FIELD', 'RESOURCES');
      batchdtls_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_INVALID_FIELD'
                               ,p_column_name  => 'RESOURCES'
                               ,p_param1       => 'FIELD'
                               ,p_token1       => 'RESOURCES');
    END IF;
	
    --bulk validation for adding resources's COST_ANALYSIS_CODE
    UPDATE gme_batch_dtls_interface intf
    SET    process_status = PROCESS_STATUS_BV_INVALID
    WHERE  NOT EXISTS
           (SELECT 1
            FROM   cm_alys_mst a
            WHERE  a.cost_analysis_code = intf.cost_analysis_code
            AND    a.delete_mark = 0)
    AND    load_type = LOADTYPE_DETAIL_ADD
    AND    object_type = OBJECTYPE_RESOURCE
    AND    int_batchstep_id IS NOT NULL
    AND    group_id = p_group_id
    AND    process_status = PROCESS_STATUS_RUNNING;	    

    IF sql%rowcount > 0 THEN
      gme_common_pvt.log_message('GME_INVALID_FIELD', 'FIELD', 'COST_ANALYSIS_CODE');
      batchdtls_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_INVALID_FIELD'
                               ,p_column_name  => 'COST_ANALYSIS_CODE'
                               ,p_param1       => 'FIELD'
                               ,p_token1       => 'COST_ANALYSIS_CODE');
    END IF;

    --bulk validation for adding resources's COST_CMPNTCLS_CODE
    UPDATE gme_batch_dtls_interface intf
    SET    int_cost_cmpntcls_id = 
           (SELECT cost_cmpntcls_id
            FROM   cm_cmpt_mst c
            WHERE  c.cost_cmpntcls_code = intf.cost_cmpntcls_code
            AND    c.delete_mark = 0)
    WHERE  load_type = LOADTYPE_DETAIL_ADD
    AND    object_type = OBJECTYPE_RESOURCE
    AND    int_batchstep_id IS NOT NULL
    AND    group_id = p_group_id
    AND    process_status = PROCESS_STATUS_RUNNING;

    UPDATE gme_batch_dtls_interface
    SET    process_status = PROCESS_STATUS_BV_INVALID
    WHERE  int_cost_cmpntcls_id IS NULL
    AND    load_type = LOADTYPE_DETAIL_ADD
    AND    object_type = OBJECTYPE_RESOURCE
    AND    int_batchstep_id IS NOT NULL
    AND    group_id = p_group_id
    AND    process_status = PROCESS_STATUS_RUNNING;

    IF sql%rowcount > 0 THEN
      gme_common_pvt.log_message('GME_INVALID_FIELD', 'FIELD', 'COST_CMPNTCLS_CODE');
      batchdtls_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_INVALID_FIELD'
                               ,p_column_name  => 'COST_CMPNTCLS_CODE'
                               ,p_param1       => 'FIELD'
                               ,p_token1       => 'COST_CMPNTCLS_CODE');
    END IF;

    --bulk validation for adding resources's PRIM_RSRC_IND
    UPDATE gme_batch_dtls_interface
    SET    process_status = PROCESS_STATUS_BV_INVALID
    WHERE  prim_rsrc_ind IS NULL
    AND    load_type = LOADTYPE_DETAIL_ADD
    AND    object_type = OBJECTYPE_RESOURCE
    AND    int_batchstep_id IS NOT NULL
    AND    group_id = p_group_id
    AND    process_status = PROCESS_STATUS_RUNNING;

    IF sql%rowcount > 0 THEN
      gme_common_pvt.log_message('GME_INVALID_FIELD', 'FIELD', 'PRIM_RSRC_IND');
      batchdtls_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_INVALID_FIELD'
                               ,p_column_name  => 'PRIM_RSRC_IND'
                               ,p_param1       => 'FIELD'
                               ,p_token1       => 'PRIM_RSRC_IND');
    END IF;

    --bulk validation for adding resources's SCALE_TYPE
    UPDATE gme_batch_dtls_interface
    SET    process_status = PROCESS_STATUS_BV_INVALID
    WHERE  scale_type IS NULL
    AND    load_type = LOADTYPE_DETAIL_ADD
    AND    object_type = OBJECTYPE_RESOURCE
    AND    int_batchstep_id IS NOT NULL
    AND    group_id = p_group_id
    AND    process_status = PROCESS_STATUS_RUNNING;

    IF sql%rowcount > 0 THEN
      gme_common_pvt.log_message('GME_INVALID_FIELD', 'FIELD', 'SCALE_TYPE');
      batchdtls_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_INVALID_FIELD'
                               ,p_column_name  => 'SCALE_TYPE'
                               ,p_param1       => 'FIELD'
                               ,p_token1       => 'SCALE_TYPE');
    END IF;

    --bulk validation for adding resources's PLAN_RSRC_COUNT
    UPDATE gme_batch_dtls_interface intf
    SET    process_status = PROCESS_STATUS_BV_INVALID
    WHERE  EXISTS
    (SELECT 1
     FROM   gme_batch_steps bs
     WHERE  bs.batchstep_id = intf.int_batchstep_id
     AND    bs.step_status = gme_common_pvt.g_step_pending
     AND    intf.plan_rsrc_count IS NULL
    )
    AND    load_type = LOADTYPE_DETAIL_ADD
    AND    object_type = OBJECTYPE_RESOURCE
    AND    int_batchstep_id IS NOT NULL
    AND    group_id = p_group_id
    AND    process_status = PROCESS_STATUS_RUNNING;

    IF sql%rowcount > 0 THEN
      gme_common_pvt.log_message('GME_INVALID_FIELD', 'FIELD', 'PLAN_RSRC_COUNT');
      batchdtls_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_INVALID_FIELD'
                               ,p_column_name  => 'PLAN_RSRC_COUNT'
                               ,p_param1       => 'FIELD'
                               ,p_token1       => 'PLAN_RSRC_COUNT');
    END IF;

    --bulk validation for adding resources's PLAN_RSRC_USAGE
    UPDATE gme_batch_dtls_interface intf
    SET    process_status = PROCESS_STATUS_BV_INVALID
    WHERE  EXISTS
    (SELECT 1
     FROM   gme_batch_steps bs
     WHERE  bs.batchstep_id = intf.int_batchstep_id
     AND    bs.step_status = gme_common_pvt.g_step_pending
     AND    intf.plan_rsrc_usage IS NULL
    )
    AND    load_type = LOADTYPE_DETAIL_ADD
    AND    object_type = OBJECTYPE_RESOURCE
    AND    int_batchstep_id IS NOT NULL
    AND    group_id = p_group_id
    AND    process_status = PROCESS_STATUS_RUNNING;

    IF sql%rowcount > 0 THEN
      gme_common_pvt.log_message('GME_INVALID_FIELD', 'FIELD', 'PLAN_RSRC_USAGE');
      batchdtls_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_INVALID_FIELD'
                               ,p_column_name  => 'PLAN_RSRC_USAGE'
                               ,p_param1       => 'FIELD'
                               ,p_token1       => 'PLAN_RSRC_USAGE');
    END IF;

    --bulk validation for adding resources's PLAN_RSRC_QTY
    UPDATE gme_batch_dtls_interface intf
    SET    process_status = PROCESS_STATUS_BV_INVALID
    WHERE  EXISTS
    (SELECT 1
     FROM   gme_batch_steps bs, gme_batch_header bh
     WHERE  bs.batchstep_id = intf.int_batchstep_id
     AND    bh.batch_id = intf.int_batch_id
     AND    bs.step_status = gme_common_pvt.g_step_pending
     AND    bh.automatic_step_calculation = 0
     AND    intf.plan_rsrc_qty IS NULL
    )
    AND    load_type = LOADTYPE_DETAIL_ADD
    AND    int_batchstep_id IS NOT NULL
    AND    object_type = OBJECTYPE_RESOURCE
    AND    group_id = p_group_id
    AND    process_status = PROCESS_STATUS_RUNNING;

    IF sql%rowcount > 0 THEN
      gme_common_pvt.log_message('GME_INVALID_FIELD', 'FIELD', 'PLAN_RSRC_QTY');
      batchdtls_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_INVALID_FIELD'
                               ,p_column_name  => 'PLAN_RSRC_QTY'
                               ,p_param1       => 'FIELD'
                               ,p_token1       => 'PLAN_RSRC_QTY');
    END IF;

    --bulk validation for adding resources's ACTUAL_RSRC_COUNT
    UPDATE gme_batch_dtls_interface intf
    SET    process_status = PROCESS_STATUS_BV_INVALID
    WHERE  EXISTS
    (SELECT 1
     FROM   gme_batch_steps bs
     WHERE  bs.batchstep_id = intf.int_batchstep_id
     AND    bs.step_status IN (gme_common_pvt.g_step_wip, gme_common_pvt.g_step_completed)
     AND    intf.actual_rsrc_count IS NULL
    )
    AND    load_type = LOADTYPE_DETAIL_ADD
    AND    object_type = OBJECTYPE_RESOURCE
    AND    int_batchstep_id IS NOT NULL
    AND    group_id = p_group_id
    AND    process_status = PROCESS_STATUS_RUNNING;

    IF sql%rowcount > 0 THEN
      gme_common_pvt.log_message('GME_INVALID_FIELD', 'FIELD', 'ACTUAL_RSRC_COUNT');
      batchdtls_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_INVALID_FIELD'
                               ,p_column_name  => 'ACTUAL_RSRC_COUNT'
                               ,p_param1       => 'FIELD'
                               ,p_token1       => 'ACTUAL_RSRC_COUNT');
    END IF;

    --bulk validation for adding resources's ACTUAL_RSRC_USAGE
    UPDATE gme_batch_dtls_interface intf
    SET    process_status = PROCESS_STATUS_BV_INVALID
    WHERE  EXISTS
    (SELECT 1
     FROM   gme_batch_steps bs
     WHERE  bs.batchstep_id = intf.int_batchstep_id
     AND    bs.step_status IN (gme_common_pvt.g_step_wip, gme_common_pvt.g_step_completed)
     AND    intf.actual_rsrc_usage IS NULL
    )
    AND    load_type = LOADTYPE_DETAIL_ADD
    AND    object_type = OBJECTYPE_RESOURCE
    AND    int_batchstep_id IS NOT NULL
    AND    group_id = p_group_id
    AND    process_status = PROCESS_STATUS_RUNNING;

    IF sql%rowcount > 0 THEN
      gme_common_pvt.log_message('GME_INVALID_FIELD', 'FIELD', 'ACTUAL_RSRC_USAGE');
      batchdtls_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_INVALID_FIELD'
                               ,p_column_name  => 'ACTUAL_RSRC_USAGE'
                               ,p_param1       => 'FIELD'
                               ,p_token1       => 'ACTUAL_RSRC_USAGE');
    END IF;

    --bulk validation for adding resources's ACTUAL_RSRC_QTY
    UPDATE gme_batch_dtls_interface intf
    SET    process_status = PROCESS_STATUS_BV_INVALID
    WHERE  EXISTS
    (SELECT 1
     FROM   gme_batch_steps bs, gme_batch_header bh
     WHERE  bs.batchstep_id = intf.int_batchstep_id
     AND    bh.batch_id = intf.int_batch_id
     AND    bs.step_status IN (gme_common_pvt.g_step_wip, gme_common_pvt.g_step_completed)
     AND    bh.automatic_step_calculation = 0
     AND    intf.actual_rsrc_qty IS NULL
    )
    AND    load_type = LOADTYPE_DETAIL_ADD
    AND    int_batchstep_id IS NOT NULL
    AND    object_type = OBJECTYPE_RESOURCE
    AND    group_id = p_group_id
    AND    process_status = PROCESS_STATUS_RUNNING;

    IF sql%rowcount > 0 THEN
      gme_common_pvt.log_message('GME_INVALID_FIELD', 'FIELD', 'ACTUAL_RSRC_QTY');
      batchdtls_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_INVALID_FIELD'
                               ,p_column_name  => 'ACTUAL_RSRC_QTY'
                               ,p_param1       => 'FIELD'
                               ,p_token1       => 'ACTUAL_RSRC_QTY');
    END IF;
  ELSIF p_object_type = OBJECTYPE_RESOURCE THEN
    --bulk validation for LOAD_TYPE
    UPDATE gme_batch_dtls_interface
    SET    process_status = PROCESS_STATUS_BV_INVALID
    WHERE  load_type NOT IN (LOADTYPE_DETAIL_ADD, LOADTYPE_DETAIL_UPDATE, LOADTYPE_DETAIL_DELETE)
    AND    object_type = OBJECTYPE_RESOURCE
    AND    group_id = p_group_id
    AND    process_status = PROCESS_STATUS_RUNNING;

    IF sql%rowcount > 0 THEN
      gme_common_pvt.log_message('GME_INVALID_FIELD', 'FIELD', 'LOAD_TYPE');
      batchdtls_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_INVALID_FIELD'
                               ,p_column_name  => 'LOAD_TYPE'
                               ,p_param1       => 'FIELD'
                               ,p_token1       => 'LOAD_TYPE');
    END IF;

    --bulk validation for batchstep_resource_id
    UPDATE gme_batch_dtls_interface intf
    SET (int_batch_id, int_organization_id, int_batchstep_id, int_batchstep_activity_id, int_batchstep_resource_id) = 
        (SELECT res.batch_id, bth.organization_id, res.batchstep_id, res.batchstep_activity_id, res.batchstep_resource_id
         FROM   gme_batch_step_resources res, gme_batch_header bth
         WHERE  res.batchstep_resource_id = intf.batchstep_resource_id
         AND    res.batch_id = bth.batch_id
         AND    bth.batch_type = 0
         AND    bth.delete_mark = 0
         AND    bth.migrated_batch_ind = 'N')
    WHERE (load_type = LOADTYPE_DETAIL_UPDATE OR load_type = LOADTYPE_DETAIL_DELETE)
    AND   object_type = OBJECTYPE_RESOURCE
    AND   batchstep_resource_id IS NOT NULL
    AND   group_id = p_group_id
    AND   process_status = PROCESS_STATUS_RUNNING;

    UPDATE gme_batch_dtls_interface intf
    SET int_organization_id = 
        (SELECT mp.organization_id
           FROM mtl_parameters mp
          WHERE mp.organization_code = intf.organization_code)
    WHERE organization_code IS NOT NULL
      AND int_organization_id IS NULL
      AND object_type = OBJECTYPE_RESOURCE
      AND group_id = p_group_id
      AND process_status = PROCESS_STATUS_RUNNING;

    --bulk validation for batch_no, batchstep_no, activity, resource
    UPDATE gme_batch_dtls_interface intf 
    SET   (int_batchstep_resource_id, int_batchstep_activity_id, int_batchstep_id, int_batch_id ) = 
          (SELECT MIN(res.batchstep_resource_id), MIN(res.batchstep_activity_id), res.batchstep_id , res.batch_id
             FROM gme_batch_header gbh ,
                  gme_batch_steps step ,
                  gme_batch_step_activities act,
                  gme_batch_step_resources res
            WHERE (gbh.batch_id = intf.int_batch_id
                   OR (intf.int_organization_id = gbh.organization_id
                       AND intf.batch_no = gbh.batch_no
                       AND intf.int_batch_id IS NULL))
                       AND gbh.batch_type = 0
                       AND gbh.delete_mark = 0
                       AND gbh.migrated_batch_ind = 'N'
                       AND gbh.organization_id = intf.int_organization_id
                       AND step.batchstep_no = intf.batchstep_no
                       AND step.batch_id = gbh.batch_id
                       AND act.activity = intf.activity
                       AND act.batch_id = gbh.batch_id
                       AND act.batchstep_id = step.batchstep_id
                       AND res.resources = intf.resources
                       AND res.batch_id = gbh.batch_id
                       AND res.batchstep_id = step.batchstep_id
                       AND res.batchstep_activity_id = act.batchstep_activity_id
            GROUP BY res.batchstep_id, res.batch_id)
     WHERE intf.process_status = PROCESS_STATUS_RUNNING
       AND intf.int_batchstep_resource_id IS NULL
       AND intf.object_type = OBJECTYPE_RESOURCE
       AND intf.load_type IN (LOADTYPE_DETAIL_UPDATE, LOADTYPE_DETAIL_DELETE)
       AND intf.group_id = p_group_id;
    
    UPDATE gme_batch_dtls_interface
    SET    process_status = PROCESS_STATUS_BV_INVALID
    WHERE  int_batchstep_resource_id IS NULL
    AND    load_type IN (LOADTYPE_DETAIL_UPDATE, LOADTYPE_DETAIL_DELETE)
    AND    object_type = OBJECTYPE_RESOURCE
    AND    group_id = p_group_id
    AND    process_status = PROCESS_STATUS_RUNNING;	

    IF sql%rowcount > 0 THEN
      gme_common_pvt.log_message('GME_STEP_RSCID_NOTFOUND');
      batchdtls_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_STEP_RSCID_NOTFOUND');
    END IF;

    --update the four int ids when batchstep_activity_id is valid
    UPDATE gme_batch_dtls_interface intf
    SET (int_batch_id, int_organization_id, int_batchstep_id, int_batchstep_activity_id) = 
        (SELECT a.batch_id, b.organization_id, a.batchstep_id, a.batchstep_activity_id
         FROM gme_batch_step_activities a, gme_batch_header b
         WHERE a.batchstep_activity_id = intf.batchstep_activity_id
         AND   a.batch_id = b.batch_id
         AND   a.plan_activity_factor <> 0
         AND   b.batch_type = 0
         AND   b.delete_mark = 0
         AND   b.migrated_batch_ind = 'N')
    WHERE load_type = LOADTYPE_DETAIL_ADD
    AND   object_type = OBJECTYPE_RESOURCE
    AND   batchstep_activity_id IS NOT NULL
    AND   group_id = p_group_id
    AND   process_status = PROCESS_STATUS_RUNNING;
    
    UPDATE gme_batch_dtls_interface intf
       SET (int_batchstep_activity_id, int_batch_id, int_batchstep_id) =
           (SELECT MIN(act.batchstep_activity_id), act.batch_id, act.batchstep_id
              FROM gme_batch_header gbh,
                   gme_batch_steps step,
                   gme_batch_step_activities act
             WHERE (gbh.batch_id = intf.int_batch_id
                    OR (intf.int_organization_id = gbh.organization_id
                        AND intf.batch_no = gbh.batch_no
                        AND intf.int_batch_id IS NULL))
                   AND gbh.batch_type = 0
                   AND gbh.delete_mark = 0
                   AND gbh.migrated_batch_ind = 'N'
                   AND step.batchstep_no = intf.batchstep_no
                   AND step.batch_id = gbh.batch_id
                   AND act.activity = intf.activity
                   AND act.batch_id = gbh.batch_id
                   AND act.batchstep_id = step.batchstep_id
                   AND act.plan_activity_factor <> 0
             GROUP BY  act.batch_id, act.batchstep_id)
     WHERE intf.process_status = PROCESS_STATUS_RUNNING
       AND intf.int_batchstep_activity_id IS NULL
       AND intf.object_type = OBJECTYPE_RESOURCE                                  
       AND intf.load_type = LOADTYPE_DETAIL_ADD
       AND intf.group_id = p_group_id;
    
    UPDATE gme_batch_dtls_interface
    SET    process_status = PROCESS_STATUS_BV_INVALID
    WHERE  int_batchstep_activity_id IS NULL
    AND    load_type = LOADTYPE_DETAIL_ADD
    AND    object_type = OBJECTYPE_RESOURCE
    AND    group_id = p_group_id
    AND    process_status = PROCESS_STATUS_RUNNING;	

    IF sql%rowcount > 0 THEN
      gme_common_pvt.log_message('GME_STEP_ACTIVITYID_NOTFOUND');
      batchdtls_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_STEP_ACTIVITYID_NOTFOUND');
    END IF;

    --Bulk validtion for step status
    UPDATE gme_batch_dtls_interface intf
    SET process_status = PROCESS_STATUS_BV_INVALID
    WHERE EXISTS
      (SELECT 1
       FROM   gme_batch_steps s
       WHERE  ((s.step_status IN (gme_common_pvt.g_step_closed, gme_common_pvt.g_step_cancelled) 
                AND intf.load_type IN (LOADTYPE_DETAIL_ADD, LOADTYPE_DETAIL_UPDATE))
               OR 
               (s.step_status <> gme_common_pvt.g_step_pending
                AND intf.load_type = LOADTYPE_DETAIL_DELETE))
       AND    s.batchstep_id = intf.int_batchstep_id)
    AND   object_type = OBJECTYPE_RESOURCE
    AND   group_id = p_group_id
    AND   process_status = PROCESS_STATUS_RUNNING;

    IF sql%rowcount > 0 THEN
      gme_common_pvt.log_message('PC_STEP_STATUS_ERR');
      batchdtls_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'PC_STEP_STATUS_ERR');
    END IF;

    --Bulk validtion for step status and batch automatic_step_calculation
    UPDATE gme_batch_dtls_interface intf
    SET process_status = PROCESS_STATUS_BV_INVALID
    WHERE EXISTS
      (SELECT 1
       FROM   gme_batch_steps bs, gme_batch_header bh
       WHERE  bh.batch_id = intf.int_batch_id
       AND    bs.batchstep_id = intf.int_batchstep_id
       AND    bs.step_status = gme_common_pvt.g_step_wip
       AND    bh.automatic_step_calculation = 1
      )
    AND   object_type = OBJECTYPE_RESOURCE
    AND   load_type = LOADTYPE_DETAIL_ADD
    AND   group_id = p_group_id
    AND   process_status = PROCESS_STATUS_RUNNING;

    IF sql%rowcount > 0 THEN
      gme_common_pvt.log_message('GME_INVALID_ASQC_ACTION');
      batchdtls_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_INVALID_ASQC_ACTION');
    END IF;
	
    UPDATE gme_batch_dtls_interface intf
    SET    process_status = PROCESS_STATUS_BV_INVALID
    WHERE  NOT EXISTS
           (
            SELECT 1
            FROM   cr_rsrc_mst r
            WHERE  r.resources = intf.resources
            AND    r.delete_mark = 0)
    AND    load_type = LOADTYPE_DETAIL_ADD
    AND    object_type = OBJECTYPE_RESOURCE
    AND    group_id = p_group_id
    AND    process_status = PROCESS_STATUS_RUNNING;

    IF sql%rowcount > 0 THEN
      gme_common_pvt.log_message('GME_INVALID_FIELD', 'FIELD', 'RESOURCES');
      batchdtls_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_INVALID_FIELD'
                               ,p_column_name  => 'RESOURCES'
                               ,p_param1       => 'FIELD'
                               ,p_token1       => 'RESOURCES');
    END IF;

    --bulk validation for COST_ANALYSIS_CODE
    UPDATE gme_batch_dtls_interface intf
    SET    process_status = PROCESS_STATUS_BV_INVALID
    WHERE  NOT EXISTS
           (SELECT 1
            FROM   cm_alys_mst a
            WHERE  a.cost_analysis_code = intf.cost_analysis_code
            AND    a.delete_mark = 0)
    AND    (load_type = LOADTYPE_DETAIL_ADD
            OR (load_type = LOADTYPE_DETAIL_UPDATE AND cost_analysis_code IS NOT NULL))
    AND    object_type = OBJECTYPE_RESOURCE
    AND    group_id = p_group_id
    AND    process_status = PROCESS_STATUS_RUNNING;

    IF sql%rowcount > 0 THEN
      gme_common_pvt.log_message('GME_INVALID_FIELD', 'FIELD', 'COST_ANALYSIS_CODE');
      batchdtls_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_INVALID_FIELD'
                               ,p_column_name  => 'COST_ANALYSIS_CODE'
                               ,p_param1       => 'FIELD'
                               ,p_token1       => 'COST_ANALYSIS_CODE');
    END IF;

    --bulk validation for COST_CMPNTCLS_CODE
    UPDATE gme_batch_dtls_interface intf
    SET    int_cost_cmpntcls_id = 
           (SELECT cost_cmpntcls_id
            FROM   cm_cmpt_mst c
            WHERE  c.cost_cmpntcls_code = intf.cost_cmpntcls_code
            AND    c.delete_mark = 0)
    WHERE  (load_type = LOADTYPE_DETAIL_ADD 
            OR (load_type = LOADTYPE_DETAIL_UPDATE AND cost_cmpntcls_code IS NOT NULL))
    AND    object_type = OBJECTYPE_RESOURCE
    AND    group_id = p_group_id
    AND    process_status = PROCESS_STATUS_RUNNING;    

    UPDATE gme_batch_dtls_interface
    SET    process_status = PROCESS_STATUS_BV_INVALID
    WHERE  int_cost_cmpntcls_id IS NULL
    AND    (load_type = LOADTYPE_DETAIL_ADD 
            OR (load_type = LOADTYPE_DETAIL_UPDATE AND cost_cmpntcls_code IS NOT NULL))
    AND    object_type = OBJECTYPE_RESOURCE
    AND    group_id = p_group_id
    AND    process_status = PROCESS_STATUS_RUNNING;

    IF sql%rowcount > 0 THEN
      gme_common_pvt.log_message('GME_INVALID_FIELD', 'FIELD', 'COST_CMPNTCLS_CODE');
      batchdtls_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_INVALID_FIELD'
                               ,p_column_name  => 'COST_CMPNTCLS_CODE'
                               ,p_param1       => 'FIELD'
                               ,p_token1       => 'COST_CMPNTCLS_CODE');
    END IF;

    --bulk validation for PLAN_START_DATE, PLAN_CMPLT_DATE
    UPDATE gme_batch_dtls_interface
    SET    process_status = PROCESS_STATUS_BV_INVALID
    WHERE  plan_start_date > plan_cmplt_date
    AND    object_type = OBJECTYPE_RESOURCE
    AND    group_id = p_group_id
    AND    process_status = PROCESS_STATUS_RUNNING;

    IF sql%rowcount > 0 THEN
      gme_common_pvt.log_message('GME_CMPLT_START_DT');
      batchdtls_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_CMPLT_START_DT');
    END IF;

    --bulk validation for ACTUAL_START_DATE, ACTUAL_CMPLT_DATE
    UPDATE gme_batch_dtls_interface
    SET    process_status = PROCESS_STATUS_BV_INVALID
    WHERE  actual_start_date > actual_cmplt_date
    AND    load_type = LOADTYPE_DETAIL_UPDATE
    AND    object_type = OBJECTYPE_RESOURCE
    AND    group_id = p_group_id
    AND    process_status = PROCESS_STATUS_RUNNING;

    IF sql%rowcount > 0 THEN
      gme_common_pvt.log_message('GME_ACTUAL_CMPLT_START_DT');
      batchdtls_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_ACTUAL_CMPLT_START_DT');
    END IF;

    --bulk validation for PRIM_RSRC_IND
    UPDATE gme_batch_dtls_interface
    SET    process_status = PROCESS_STATUS_BV_INVALID
    WHERE  prim_rsrc_ind IS NULL
    AND    load_type = LOADTYPE_DETAIL_ADD
    AND    object_type = OBJECTYPE_RESOURCE
    AND    group_id = p_group_id
    AND    process_status = PROCESS_STATUS_RUNNING;

    IF sql%rowcount > 0 THEN
      gme_common_pvt.log_message('GME_INVALID_FIELD', 'FIELD', 'PRIM_RSRC_IND');
      batchdtls_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_INVALID_FIELD'
                               ,p_column_name  => 'PRIM_RSRC_IND'
                               ,p_param1       => 'FIELD'
                               ,p_token1       => 'PRIM_RSRC_IND');
    END IF;

    --bulk validation for SCALE_TYPE
    UPDATE gme_batch_dtls_interface
    SET    process_status = PROCESS_STATUS_BV_INVALID
    WHERE  scale_type IS NULL
    AND    load_type = LOADTYPE_DETAIL_ADD
    AND    object_type = OBJECTYPE_RESOURCE
    AND    group_id = p_group_id
    AND    process_status = PROCESS_STATUS_RUNNING;

    IF sql%rowcount > 0 THEN
      gme_common_pvt.log_message('GME_INVALID_FIELD', 'FIELD', 'SCALE_TYPE');
      batchdtls_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_INVALID_FIELD'
                               ,p_column_name  => 'SCALE_TYPE'
                               ,p_param1       => 'FIELD'
                               ,p_token1       => 'SCALE_TYPE');
    END IF;

    --bulk validation for PLAN_RSRC_COUNT
    UPDATE gme_batch_dtls_interface intf
    SET    process_status = PROCESS_STATUS_BV_INVALID
    WHERE  EXISTS
    (SELECT 1
     FROM   gme_batch_steps bs
     WHERE  bs.batchstep_id = intf.int_batchstep_id
     AND    bs.step_status = gme_common_pvt.g_step_pending
     AND    intf.plan_rsrc_count IS NULL
    )
    AND    load_type = LOADTYPE_DETAIL_ADD
    AND    object_type = OBJECTYPE_RESOURCE
    AND    group_id = p_group_id
    AND    process_status = PROCESS_STATUS_RUNNING;

    IF sql%rowcount > 0 THEN
      gme_common_pvt.log_message('GME_INVALID_FIELD', 'FIELD', 'PLAN_RSRC_COUNT');
      batchdtls_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_INVALID_FIELD'
                               ,p_column_name  => 'PLAN_RSRC_COUNT'
                               ,p_param1       => 'FIELD'
                               ,p_token1       => 'PLAN_RSRC_COUNT');
    END IF;

    --bulk validation for PLAN_RSRC_USAGE
    UPDATE gme_batch_dtls_interface intf
    SET    process_status = PROCESS_STATUS_BV_INVALID
    WHERE  EXISTS
    (SELECT 1
     FROM   gme_batch_steps bs
     WHERE  bs.batchstep_id = intf.int_batchstep_id
     AND    bs.step_status = gme_common_pvt.g_step_pending
     AND    intf.plan_rsrc_usage IS NULL
    )
    AND    load_type = LOADTYPE_DETAIL_ADD
    AND    object_type = OBJECTYPE_RESOURCE
    AND    group_id = p_group_id
    AND    process_status = PROCESS_STATUS_RUNNING;

    IF sql%rowcount > 0 THEN
      gme_common_pvt.log_message('GME_INVALID_FIELD', 'FIELD', 'PLAN_RSRC_USAGE');
      batchdtls_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_INVALID_FIELD'
                               ,p_column_name  => 'PLAN_RSRC_USAGE'
                               ,p_param1       => 'FIELD'
                               ,p_token1       => 'PLAN_RSRC_USAGE');
    END IF;

    --bulk validation for PLAN_RSRC_QTY
    UPDATE gme_batch_dtls_interface intf
    SET    process_status = PROCESS_STATUS_BV_INVALID
    WHERE  EXISTS
    (SELECT 1
     FROM   gme_batch_steps bs, gme_batch_header bh
     WHERE  bs.batchstep_id = intf.int_batchstep_id
     AND    bh.batch_id = intf.int_batch_id
     AND    bs.step_status = gme_common_pvt.g_step_pending
     AND    bh.automatic_step_calculation = 0
     AND    intf.plan_rsrc_qty IS NULL
    )
    AND    load_type = LOADTYPE_DETAIL_ADD
    AND    object_type = OBJECTYPE_RESOURCE
    AND    group_id = p_group_id
    AND    process_status = PROCESS_STATUS_RUNNING;

    IF sql%rowcount > 0 THEN
      gme_common_pvt.log_message('GME_INVALID_FIELD', 'FIELD', 'PLAN_RSRC_QTY');
      batchdtls_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_INVALID_FIELD'
                               ,p_column_name  => 'PLAN_RSRC_QTY'
                               ,p_param1       => 'FIELD'
                               ,p_token1       => 'PLAN_RSRC_QTY');
    END IF;

    --bulk validation for ACTUAL_RSRC_COUNT
    UPDATE gme_batch_dtls_interface intf
    SET    process_status = PROCESS_STATUS_BV_INVALID
    WHERE  EXISTS
    (SELECT 1
     FROM   gme_batch_steps bs
     WHERE  bs.batchstep_id = intf.int_batchstep_id
     AND    bs.step_status IN (gme_common_pvt.g_step_wip, gme_common_pvt.g_step_completed)
     AND    intf.actual_rsrc_count IS NULL
    )
    AND    load_type = LOADTYPE_DETAIL_ADD
    AND    object_type = OBJECTYPE_RESOURCE
    AND    group_id = p_group_id
    AND    process_status = PROCESS_STATUS_RUNNING;

    IF sql%rowcount > 0 THEN
      gme_common_pvt.log_message('GME_INVALID_FIELD', 'FIELD', 'ACTUAL_RSRC_COUNT');
      batchdtls_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_INVALID_FIELD'
                               ,p_column_name  => 'ACTUAL_RSRC_COUNT'
                               ,p_param1       => 'FIELD'
                               ,p_token1       => 'ACTUAL_RSRC_COUNT');
    END IF;

    --bulk validation for ACTUAL_RSRC_USAGE
    UPDATE gme_batch_dtls_interface intf
    SET    process_status = PROCESS_STATUS_BV_INVALID
    WHERE  EXISTS
    (SELECT 1
     FROM   gme_batch_steps bs
     WHERE  bs.batchstep_id = intf.int_batchstep_id
     AND    bs.step_status IN (gme_common_pvt.g_step_wip, gme_common_pvt.g_step_completed)
     AND    intf.actual_rsrc_usage IS NULL
    )
    AND    load_type = LOADTYPE_DETAIL_ADD
    AND    object_type = OBJECTYPE_RESOURCE
    AND    group_id = p_group_id
    AND    process_status = PROCESS_STATUS_RUNNING;

    IF sql%rowcount > 0 THEN
      gme_common_pvt.log_message('GME_INVALID_FIELD', 'FIELD', 'ACTUAL_RSRC_USAGE');
      batchdtls_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_INVALID_FIELD'
                               ,p_column_name  => 'ACTUAL_RSRC_USAGE'
                               ,p_param1       => 'FIELD'
                               ,p_token1       => 'ACTUAL_RSRC_USAGE');
    END IF;

    --bulk validation for ACTUAL_RSRC_QTY
    UPDATE gme_batch_dtls_interface intf
    SET    process_status = PROCESS_STATUS_BV_INVALID
    WHERE  EXISTS
    (SELECT 1
     FROM   gme_batch_steps bs, gme_batch_header bh
     WHERE  bs.batchstep_id = intf.int_batchstep_id
     AND    bh.batch_id = intf.int_batch_id
     AND    bs.step_status IN (gme_common_pvt.g_step_wip, gme_common_pvt.g_step_completed)
     AND    bh.automatic_step_calculation = 0
     AND    intf.actual_rsrc_qty IS NULL
    )
    AND    load_type = LOADTYPE_DETAIL_ADD
    AND    object_type = OBJECTYPE_RESOURCE
    AND    group_id = p_group_id
    AND    process_status = PROCESS_STATUS_RUNNING;

    IF sql%rowcount > 0 THEN
      gme_common_pvt.log_message('GME_INVALID_FIELD', 'FIELD', 'ACTUAL_RSRC_QTY');
      batchdtls_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_INVALID_FIELD'
                               ,p_column_name  => 'ACTUAL_RSRC_QTY'
                               ,p_param1       => 'FIELD'
                               ,p_token1       => 'ACTUAL_RSRC_QTY');
    END IF;
  ELSIF p_object_type = OBJECTYPE_STEPITEM THEN
    --bulk validation for LOAD_TYPE
    UPDATE gme_batch_dtls_interface
    SET    process_status = PROCESS_STATUS_BV_INVALID
    WHERE  load_type NOT IN (LOADTYPE_DETAIL_ADD, LOADTYPE_DETAIL_UPDATE, LOADTYPE_DETAIL_DELETE)
    AND    object_type = OBJECTYPE_STEPITEM
    AND    group_id = p_group_id
    AND    process_status = PROCESS_STATUS_RUNNING;

    IF sql%rowcount > 0 THEN
      gme_common_pvt.log_message('GME_INVALID_FIELD', 'FIELD', 'LOAD_TYPE');
      batchdtls_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_INVALID_FIELD'
                               ,p_column_name  => 'LOAD_TYPE'
                               ,p_param1       => 'FIELD'
                               ,p_token1       => 'LOAD_TYPE');
    END IF;
	
    UPDATE gme_batch_dtls_interface intf
    SET int_organization_id = 
        (SELECT mp.organization_id
         FROM   mtl_parameters mp
         WHERE  mp.organization_code = intf.organization_code)
    WHERE organization_code IS NOT NULL
    AND   material_detail_id IS NULL
    AND   object_type = OBJECTYPE_STEPITEM
    AND   group_id = p_group_id
    AND   process_status = PROCESS_STATUS_RUNNING;
	
    --bulk validation for batch_no, line_no, line_type    
    UPDATE gme_batch_dtls_interface intf
    SET (int_batch_id, int_material_detail_id) = 
        (SELECT md.batch_id, md.material_detail_id
         FROM   gme_material_details md, gme_batch_header bh
         WHERE (md.batch_id = intf.int_batch_id  
                OR (bh.batch_no = intf.batch_no
                    AND bh.organization_id = intf.int_organization_id
                    AND intf.int_batch_id IS NULL))
         AND bh.migrated_batch_ind = 'N'
         AND bh.batch_type = 0
         AND bh.delete_mark = 0
         AND md.batch_id = bh.batch_id
         AND md.line_no = intf.line_no
         AND md.line_type = intf.line_type)
    WHERE ((int_organization_id IS NOT NULL AND batch_no IS NOT NULL) OR int_batch_id IS NOT NULL)
    AND   line_no IS NOT NULL
    AND   line_type IS NOT NULL
    AND   object_type = OBJECTYPE_STEPITEM
    AND   material_detail_id IS NULL
    AND   group_id = p_group_id
    AND   process_status = PROCESS_STATUS_RUNNING;

    --bulk validation for material_detail_id
    UPDATE gme_batch_dtls_interface intf
    SET (int_batch_id, int_organization_id, int_material_detail_id, int_batchstep_id) = 
        (SELECT si.batch_id, bh.organization_id, si.material_detail_id, si.batchstep_id
         FROM   gme_batch_step_items si, gme_batch_header bh
         WHERE  (si.material_detail_id = intf.material_detail_id 
                 OR si.material_detail_id = intf.int_material_detail_id) 
         AND    bh.batch_id = si.batch_id
         AND    bh.batch_type = 0
         AND    bh.delete_mark = 0
         AND    bh.migrated_batch_ind = 'N')
    WHERE (material_detail_id IS NOT NULL OR int_material_detail_id IS NOT NULL)
    AND   object_type = OBJECTYPE_STEPITEM
    AND   (load_type = LOADTYPE_DETAIL_UPDATE OR load_type = LOADTYPE_DETAIL_DELETE)
    AND   group_id = p_group_id
    AND   process_status = PROCESS_STATUS_RUNNING;
	
    UPDATE gme_batch_dtls_interface intf
    SET (int_batch_id, int_organization_id, int_material_detail_id) = 
        (SELECT m.batch_id, m.organization_id, m.material_detail_id
         FROM   gme_material_details m, gme_batch_header bh
         WHERE  (m.material_detail_id = intf.material_detail_id 
                 OR m.material_detail_id = intf.int_material_detail_id) 
         AND    bh.batch_id = m.batch_id
         AND    bh.batch_type = 0
         AND    bh.delete_mark = 0
         AND    bh.migrated_batch_ind = 'N')
    WHERE (material_detail_id IS NOT NULL OR int_material_detail_id IS NOT NULL)
    AND   object_type = OBJECTYPE_STEPITEM
    AND   load_type = LOADTYPE_DETAIL_ADD
    AND   group_id = p_group_id
    AND   process_status = PROCESS_STATUS_RUNNING;
    
    UPDATE gme_batch_dtls_interface
    SET   process_status = PROCESS_STATUS_BV_INVALID 
    WHERE int_material_detail_id IS NULL
    AND   object_type = OBJECTYPE_STEPITEM
    AND   group_id = p_group_id
    AND   process_status = PROCESS_STATUS_RUNNING;

    IF sql%rowcount > 0 THEN
      gme_common_pvt.log_message('GME_MATERIAL_DETAILID_NOTFOUND');
      batchdtls_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_MATERIAL_DETAILID_NOTFOUND');
    END IF;

    --bulk validation for batch status
    UPDATE gme_batch_dtls_interface intf
    SET    process_status = PROCESS_STATUS_BV_INVALID
    WHERE  EXISTS
           (SELECT 1
            FROM   gme_batch_header bh
            WHERE  bh.batch_id = intf.int_batch_id
                   AND bh.batch_status <> gme_common_pvt.g_batch_pending)
    AND   object_type = OBJECTYPE_STEPITEM
    AND   group_id = p_group_id
    AND   process_status = PROCESS_STATUS_RUNNING;

    IF sql%rowcount > 0 THEN
      gme_common_pvt.log_message('GME_INV_BATCH_STATUS_OPER');
      batchdtls_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_INV_BATCH_STATUS_OPER');
    END IF;
	
    UPDATE gme_batch_dtls_interface intf
    SET    process_status = PROCESS_STATUS_BV_INVALID
    WHERE  EXISTS
          (SELECT 1
           FROM   gme_batch_step_items si
           WHERE  si.material_detail_id = intf.int_material_detail_id)
    AND    object_type = OBJECTYPE_STEPITEM
    AND    load_type = LOADTYPE_DETAIL_ADD
    AND    group_id = p_group_id
    AND    process_status = PROCESS_STATUS_RUNNING;

    IF sql%rowcount > 0 THEN
      gme_common_pvt.log_message('GME_RECORD_EXISTS');
      batchdtls_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_RECORD_EXISTS');
    END IF;
	
    --bulk validation for batchstep_no or batchstep_id
    UPDATE gme_batch_dtls_interface intf
    SET    int_batchstep_id = 
           (SELECT bs.batchstep_id
            FROM   gme_batch_steps bs
            WHERE  (bs.batchstep_id = intf.batchstep_id
                    OR (bs.batchstep_no = intf.batchstep_no
                        AND intf.batchstep_id IS NULL))
                   AND bs.batch_id = intf.int_batch_id)
    WHERE  object_type = OBJECTYPE_STEPITEM
    AND   (load_type = LOADTYPE_DETAIL_ADD OR load_type = LOADTYPE_DETAIL_UPDATE)
    AND   (batchstep_id IS NOT NULL OR batchstep_no IS NOT NULL)
    AND   group_id = p_group_id
    AND   process_status = PROCESS_STATUS_RUNNING;

    UPDATE gme_batch_dtls_interface
    SET   process_status = PROCESS_STATUS_BV_INVALID 
    WHERE int_batchstep_id IS NULL
    AND   object_type = OBJECTYPE_STEPITEM
    AND   group_id = p_group_id
    AND   process_status = PROCESS_STATUS_RUNNING;

    IF sql%rowcount > 0 THEN
      gme_common_pvt.log_message('GME_BATCHSTEPID_NOTFOUND');
      batchdtls_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_BATCHSTEPID_NOTFOUND');
    END IF;
  ELSIF p_object_type = OBJECTYPE_PROCESSPARAMETER THEN
    --bulk validation for LOAD_TYPE
    UPDATE gme_batch_dtls_interface
    SET    process_status = PROCESS_STATUS_BV_INVALID
    WHERE  load_type NOT IN (LOADTYPE_DETAIL_ADD, LOADTYPE_DETAIL_UPDATE, LOADTYPE_DETAIL_DELETE)
    AND    object_type = OBJECTYPE_PROCESSPARAMETER
    AND    group_id = p_group_id
    AND    process_status = PROCESS_STATUS_RUNNING;

    IF sql%rowcount > 0 THEN
      gme_common_pvt.log_message('GME_INVALID_FIELD', 'FIELD', 'LOAD_TYPE');
      batchdtls_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_INVALID_FIELD'
                               ,p_column_name  => 'LOAD_TYPE'
                               ,p_param1       => 'FIELD'
                               ,p_token1       => 'LOAD_TYPE');
    END IF;

   --bulk validation for process_param_id
    UPDATE gme_batch_dtls_interface intf
    SET (int_batch_id, int_organization_id, int_batchstep_id, int_batchstep_activity_id, int_batchstep_resource_id, int_process_param_id, int_parameter_id) = 
        (SELECT pp.batch_id, bth.organization_id, pp.batchstep_id, pp.batchstep_activity_id, pp.batchstep_resource_id, pp.process_param_id, pp.parameter_id
         FROM   gme_process_parameters pp, gme_batch_header bth
         WHERE  pp.process_param_id = intf.process_param_id
         AND    pp.batch_id = bth.batch_id
         AND    bth.batch_type = 0
         AND    bth.delete_mark = 0
         AND    bth.migrated_batch_ind = 'N')
    WHERE load_type IN (LOADTYPE_DETAIL_UPDATE, LOADTYPE_DETAIL_DELETE)
    AND   object_type = OBJECTYPE_PROCESSPARAMETER
    AND   process_param_id IS NOT NULL
    AND   group_id = p_group_id
    AND   process_status = PROCESS_STATUS_RUNNING;

    UPDATE gme_batch_dtls_interface intf
    SET int_organization_id =
        (SELECT mp.organization_id
           FROM mtl_parameters mp
          WHERE mp.organization_code = intf.organization_code)
    WHERE organization_code IS NOT NULL
      AND int_organization_id IS NULL
      AND object_type = OBJECTYPE_PROCESSPARAMETER
      AND group_id = p_group_id
      AND process_status = PROCESS_STATUS_RUNNING;

    UPDATE gme_batch_dtls_interface intf
    SET (int_parameter_id, process_parameter) =
        (SELECT p.parameter_id, p.parameter_name
           FROM gmp_process_parameters p
          WHERE (p.parameter_id = intf.parameter_id
                 OR (p.parameter_name = intf.process_parameter AND intf.parameter_id IS NULL))
    AND   p.delete_mark = 0)
    WHERE int_parameter_id IS NULL
      AND object_type = OBJECTYPE_PROCESSPARAMETER
      AND group_id = p_group_id
      AND process_status = PROCESS_STATUS_RUNNING;

    UPDATE gme_batch_dtls_interface
    SET    process_status = PROCESS_STATUS_BV_INVALID
    WHERE  int_parameter_id IS NULL
    AND    load_type = LOADTYPE_DETAIL_ADD
    AND    object_type = OBJECTYPE_PROCESSPARAMETER
    AND    group_id = p_group_id
    AND    process_status = PROCESS_STATUS_RUNNING;	

    IF sql%rowcount > 0 THEN
      gme_common_pvt.log_message('GME_PARAMTERID_NOTFOUND');
      batchdtls_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_PARAMTERID_NOTFOUND');
    END IF;

    --bulk validation for batch_no, batchstep_no, activity, resource, parameter
    UPDATE gme_batch_dtls_interface intf 
    SET   (int_process_param_id, int_batchstep_resource_id, int_batchstep_activity_id, int_batchstep_id, int_batch_id) = 
          (SELECT MIN(pp.process_param_id), MIN(pp.batchstep_resource_id), MIN(pp.batchstep_activity_id), pp.batchstep_id, pp.batch_id
             FROM gme_batch_header gbh,
                  gme_batch_steps step,
                  gme_batch_step_activities act,
                  gme_batch_step_resources res,
                  gme_process_parameters pp
            WHERE (gbh.batch_id = intf.int_batch_id
                   OR (intf.int_organization_id = gbh.organization_id
                       AND intf.batch_no = gbh.batch_no
                       AND intf.int_batch_id IS NULL))
              AND gbh.batch_type = 0
              AND gbh.delete_mark = 0
              AND gbh.migrated_batch_ind = 'N'
              AND step.batchstep_no = intf.batchstep_no
              AND step.batch_id = gbh.batch_id
              AND act.activity = intf.activity
              AND act.batch_id = step.batch_id
              AND act.batchstep_id = step.batchstep_id
              AND res.resources = intf.resources
              AND res.batch_id = act.batch_id
              AND res.batchstep_id = act.batchstep_id
              AND res.batchstep_activity_id = act.batchstep_activity_id
              AND pp.parameter_id = intf.int_parameter_id
              AND pp.batch_id = res.batch_id
              AND pp.batchstep_id = res.batchstep_id
              AND pp.batchstep_activity_id = res.batchstep_activity_id
              AND pp.batchstep_resource_id = res.batchstep_resource_id
            GROUP BY pp.batchstep_id, pp.batch_id)
     WHERE process_status = PROCESS_STATUS_RUNNING
       AND int_process_param_id IS NULL
       AND object_type = OBJECTYPE_PROCESSPARAMETER
       AND load_type IN (LOADTYPE_DETAIL_UPDATE, LOADTYPE_DETAIL_DELETE)
       AND group_id = p_group_id;
    
    UPDATE gme_batch_dtls_interface
    SET    process_status = PROCESS_STATUS_BV_INVALID
    WHERE  int_process_param_id IS NULL
    AND    load_type IN (LOADTYPE_DETAIL_UPDATE, LOADTYPE_DETAIL_DELETE)
    AND    object_type = OBJECTYPE_PROCESSPARAMETER
    AND    group_id = p_group_id
    AND    process_status = PROCESS_STATUS_RUNNING;	

    IF sql%rowcount > 0 THEN
      gme_common_pvt.log_message('GME_PROCESS_PARAMID_NOTFOUND');
      batchdtls_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_PROCESS_PARAMID_NOTFOUND');
    END IF;

    --bulk validation for batchstep_resource_id
    UPDATE gme_batch_dtls_interface intf
    SET (int_batch_id, int_organization_id, int_batchstep_id, int_batchstep_activity_id, int_batchstep_resource_id) = 
        (SELECT res.batch_id, bth.organization_id, res.batchstep_id, res.batchstep_activity_id, res.batchstep_resource_id
         FROM   gme_batch_step_resources res, gme_batch_header bth
         WHERE  res.batchstep_resource_id = intf.batchstep_resource_id
         AND    res.batch_id = bth.batch_id
         AND    bth.batch_type = 0
         AND    bth.delete_mark = 0
         AND    bth.migrated_batch_ind = 'N')
    WHERE load_type = LOADTYPE_DETAIL_ADD
    AND   object_type = OBJECTYPE_PROCESSPARAMETER
    AND   batchstep_resource_id IS NOT NULL
    AND   group_id = p_group_id
    AND   process_status = PROCESS_STATUS_RUNNING;

    --bulk validation for batch_no, batchstep_no, activity, resource
    UPDATE gme_batch_dtls_interface intf 
    SET   (int_batchstep_resource_id, int_batchstep_activity_id, int_batchstep_id, int_batch_id ) = 
          (SELECT MIN(res.batchstep_resource_id), MIN(res.batchstep_activity_id), res.batchstep_id , res.batch_id
             FROM gme_batch_header gbh ,
                  gme_batch_steps step ,
                  gme_batch_step_activities act,
                  gme_batch_step_resources res
            WHERE (gbh.batch_id = intf.int_batch_id
                   OR (intf.int_organization_id = gbh.organization_id
                       AND intf.batch_no = gbh.batch_no
                       AND intf.int_batch_id IS NULL))
              AND gbh.batch_type = 0
              AND gbh.delete_mark = 0
              AND gbh.migrated_batch_ind = 'N'
              AND step.batchstep_no = intf.batchstep_no
              AND step.batch_id = gbh.batch_id
              AND act.activity = intf.activity
              AND act.batch_id = step.batch_id
              AND act.batchstep_id = step.batchstep_id
              AND res.resources = intf.resources
              AND res.batch_id = act.batch_id
              AND res.batchstep_id = act.batchstep_id
              AND res.batchstep_activity_id = act.batchstep_activity_id
            GROUP BY res.batchstep_id, res.batch_id)
     WHERE process_status = PROCESS_STATUS_RUNNING
       AND int_batchstep_resource_id IS NULL
       AND object_type = OBJECTYPE_PROCESSPARAMETER
       AND load_type = LOADTYPE_DETAIL_ADD
       AND group_id = p_group_id;
    
    UPDATE gme_batch_dtls_interface
    SET    process_status = PROCESS_STATUS_BV_INVALID
    WHERE  int_batchstep_resource_id IS NULL
    AND    load_type = LOADTYPE_DETAIL_ADD
    AND    object_type = OBJECTYPE_PROCESSPARAMETER
    AND    group_id = p_group_id
    AND    process_status = PROCESS_STATUS_RUNNING;	

    IF sql%rowcount > 0 THEN
      gme_common_pvt.log_message('GME_STEP_RSCID_NOTFOUND');
      batchdtls_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_STEP_RSCID_NOTFOUND');
    END IF;

    --Bulk validtion for step status when inserting or updating process parameter
    UPDATE gme_batch_dtls_interface intf
    SET process_status = PROCESS_STATUS_BV_INVALID
    WHERE EXISTS
      (SELECT 1
       FROM   gme_batch_steps s
       WHERE  s.step_status IN (gme_common_pvt.g_step_closed, gme_common_pvt.g_step_cancelled) 
       AND    s.batchstep_id = intf.int_batchstep_id)
    AND   object_type = OBJECTYPE_PROCESSPARAMETER
    AND   load_type IN (LOADTYPE_DETAIL_ADD, LOADTYPE_DETAIL_UPDATE)
    AND   group_id = p_group_id
    AND   process_status = PROCESS_STATUS_RUNNING;

    IF sql%rowcount > 0 THEN
      gme_common_pvt.log_message('PC_STEP_STATUS_ERR');
      batchdtls_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'PC_STEP_STATUS_ERR');
    END IF;

    --bulk validation for batch status when deleting process parameter
    UPDATE gme_batch_dtls_interface intf
    SET    process_status = PROCESS_STATUS_BV_INVALID
    WHERE  EXISTS
           (SELECT 1
            FROM   gme_batch_header bh
            WHERE  bh.batch_id = intf.int_batch_id
                   AND bh.batch_status <> gme_common_pvt.g_batch_pending)
    AND   object_type = OBJECTYPE_PROCESSPARAMETER
    AND   load_type =LOADTYPE_DETAIL_DELETE
    AND   group_id = p_group_id
    AND   process_status = PROCESS_STATUS_RUNNING;

    IF sql%rowcount > 0 THEN
      gme_common_pvt.log_message('GME_INV_BATCH_STATUS_OPER');
      batchdtls_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_INV_BATCH_STATUS_OPER');
    END IF;

    --bulk validation for targe_value
    UPDATE gme_batch_dtls_interface intf
    SET    process_status = PROCESS_STATUS_BV_INVALID
    WHERE  EXISTS
           (SELECT 1
            FROM gmp_process_parameters p
            WHERE p.parameter_id = intf.int_parameter_id
                  AND((p.minimum_value IS NULL AND p.maximum_value IS NULL 
                          AND intf.target_value IS NULL)
                      OR (p.minimum_value IS NOT NULL 
                          AND intf.target_value < p.minimum_value)
                      OR (p.maximum_value IS NOT NULL 
                          AND intf.target_value > p.maximum_value)))
    AND    load_type = LOADTYPE_DETAIL_ADD
    AND    object_type = OBJECTYPE_PROCESSPARAMETER
    AND    group_id = p_group_id
    AND    process_status = PROCESS_STATUS_RUNNING;

    IF sql%rowcount > 0 THEN
      gme_common_pvt.log_message('GME_INVALID_FIELD', 'FIELD', 'TARGET_VALUE');
      batchdtls_bv_error_handle(p_group_id     => p_group_id
                               ,p_message_name => 'GME_INVALID_FIELD'
                               ,p_column_name  => 'TARGET_VALUE'
                               ,p_param1       => 'FIELD'
                               ,p_token1       => 'TARGET_VALUE');
    END IF;
  END IF;
  x_return_status := fnd_api.g_ret_sts_success;
EXCEPTION
  WHEN OTHERS THEN
    x_message_list := SQLERRM;
    x_return_status := fnd_api.G_RET_STS_ERROR;
    IF g_debug >= gme_debug.g_log_statement THEN
      gme_debug.put_line('Errored in package gme_batch_open_interface, procedure bulk_validate_batch_detail with error: ' || SQLERRM);
    END IF;
END bulk_validate_batch_detail;

/*================================================================================
  Procedure
    update_batch_header
  Description
    This procedure updates batch header fields into table gme_batch_header
  Parameters
    p_batch_header       in parameter for batch_header 
    p_validate_flexfield in parameter to check flexfields or not, default to no
    x_batch_header       out for batch header
    x_message_list       message text
    x_message_name       message name
    x_return_status      status to return back
  Return 
    error message text
  History
    Shaliu Chen 25-Jul-2015 Bug 21208206
    add actual_start_date and actual_cmplt_date validation for batch on hold
================================================================================*/
PROCEDURE update_batch_header(p_batch_header       IN        gme_batch_header%ROWTYPE
                             ,p_validate_flexfield IN        VARCHAR2 DEFAULT fnd_api.g_false
                             ,x_batch_header       OUT NOCOPY  gme_batch_header%ROWTYPE
                             ,x_message_list       OUT NOCOPY  VARCHAR2
                             ,x_message_name       OUT NOCOPY  VARCHAR2
                             ,x_app_name           OUT NOCOPY  VARCHAR2
                             ,x_message_detail     OUT NOCOPY  T_MESSAGE_TABLE
                             ,x_return_status      OUT NOCOPY  VARCHAR2
                             )
IS
  l_batch_header            gme_batch_header%ROWTYPE;
  l_actual_start_step       gme_batch_steps.actual_start_date%TYPE;
  l_actual_cmplt_step       gme_batch_steps.actual_cmplt_date%TYPE;
  l_return                  BOOLEAN;

  --Cursor to fetch min(actual_start_date) from step
  CURSOR CUR_MIN_ACTUAL_START(v_batch_id NUMBER) IS
  SELECT MIN(actual_start_date)
  FROM   gme_batch_steps 
  WHERE  batch_id = v_batch_id;
  
  --Cursor to fetch max(actual_cmplt_date) from step
  CURSOR CUR_MAX_ACTUAL_CMPLT(v_batch_id NUMBER) IS
  SELECT MAX(actual_cmplt_date)
  FROM   gme_batch_steps 
  WHERE  batch_id = v_batch_id;
BEGIN
  IF g_debug <= gme_debug.g_log_statement THEN
    gme_debug.put_line('entered update_batch_header');
  END IF;
  x_app_name := 'GME';
  IF x_message_detail IS NOT NULL AND x_message_detail.COUNT > 1 THEN
    x_message_detail.DELETE;
  END IF;
  l_batch_header := p_batch_header;
  gme_common_pvt.g_setup_done := gme_common_pvt.setup(p_org_id => p_batch_header.organization_id);
  IF NOT gme_common_pvt.g_setup_done THEN
    x_message_list := get_error_message;
    x_return_status := fnd_api.g_ret_sts_error;
    RETURN;
  END IF;
  gme_common_pvt.set_timestamp;
  l_return := gme_common_pvt.get_batch_header(p_batch_header_rec     => p_batch_header
                                             ,p_org_code             => NULL
                                             ,p_batch_type           => NULL
                                             ,x_batch_header_rec     => x_batch_header);
  IF NOT l_return THEN
    x_message_list := get_error_message;
    x_return_status := fnd_api.G_RET_STS_ERROR;
    RETURN;
  END IF;
  --Status is Pendding
  IF x_batch_header.batch_status = gme_common_pvt.g_batch_pending THEN
    l_batch_header.actual_start_date := NULL;
    l_batch_header.actual_cmplt_date := NULL;
  --Status is WIP and type is batch
  ELSIF x_batch_header.batch_status = gme_common_pvt.g_batch_wip THEN
    l_batch_header.actual_cmplt_date := NULL;
  --Status is Completed and type is batch
  ELSIF x_batch_header.batch_status = gme_common_pvt.g_batch_completed THEN
    l_batch_header.firmed_ind := NULL;
  ELSE
    x_message_name := 'GME_INV_BATCH_STATUS_OPER';
    gme_common_pvt.log_message('GME_INV_BATCH_STATUS_OPER');
    x_message_list := get_error_message;
    x_return_status := fnd_api.G_RET_STS_ERROR;
    IF g_debug <= gme_debug.g_log_statement THEN
      gme_debug.put_line('error occurred: ' || x_message_list);
    END IF;
    RETURN;
  END IF;
  
   /*
    BUG 21208206  25-JUN-2015
    check whether the actual_start_date and actul_cmplt_date fall into hold period
   */
  IF l_batch_header.actual_start_date IS NOT NULL THEN
    IF gme_common_pvt.get_batch_hold_status(p_batch_id => l_batch_header.batch_id,
                                            p_date     => l_batch_header.actual_start_date) <> 'R' THEN
      gme_common_pvt.log_message('GME_ACTUAL_DATE_ONHOLD',
                                 'ACTUAL_DATE',
                                 'Actual_Start_Date');        
      RETURN;        
    END IF;                
  END IF;
      
  IF l_batch_header.actual_cmplt_date IS NOT NULL THEN
    IF gme_common_pvt.get_batch_hold_status(p_batch_id => l_batch_header.batch_id,
                                            p_date     => l_batch_header.actual_cmplt_date) <> 'R' THEN
      gme_common_pvt.log_message('GME_ACTUAL_DATE_ONHOLD',
                                 'ACTUAL_DATE',
                                 'Actual_Completion_Date');        
      RETURN;        
    END IF;                
  END IF;    
  
  IF l_batch_header.actual_start_date IS NOT NULL THEN
    IF l_batch_header.actual_start_date > sysdate THEN
      x_message_name := 'SY_NOFUTUREDATE';
      x_app_name := 'GMA';
      gme_common_pvt.log_message(p_message_code => 'SY_NOFUTUREDATE'
                                ,p_product_code => 'GMA');
      x_message_list := get_error_message;
      x_return_status := fnd_api.G_RET_STS_ERROR;
      IF g_debug <= gme_debug.g_log_statement THEN
        gme_debug.put_line('error occurred: ' || x_message_list);
      END IF;
      RETURN;
    END IF;
    OPEN CUR_MIN_ACTUAL_START(l_batch_header.batch_id);
    FETCH CUR_MIN_ACTUAL_START INTO l_actual_start_step;
    CLOSE CUR_MIN_ACTUAL_START;
    IF l_batch_header.actual_start_date > l_actual_start_step THEN
        x_message_name := 'GME_INV_ACT_DATE_RANGE';
        x_message_detail(1).param := 'DATE1';
        x_message_detail(2).param := 'DATE2';
        x_message_detail(1).token := fnd_date.date_to_displayDT(l_batch_header.actual_start_date);
        x_message_detail(2).token := fnd_date.date_to_displayDT(l_actual_start_step);
        gme_common_pvt.log_message('GME_INV_ACT_DATE_RANGE', 
                                   'DATE1', 
                                   fnd_date.date_to_displayDT(l_batch_header.actual_start_date),
                                   'DATE2',
                                   fnd_date.date_to_displayDT(l_actual_start_step));
        x_message_list := get_error_message;
        x_return_status := fnd_api.G_RET_STS_ERROR;
        IF g_debug <= gme_debug.g_log_statement THEN
          gme_debug.put_line('error occurred: ' || x_message_list);
        END IF;
        RETURN;
    END IF;
  END IF;
  IF l_batch_header.actual_cmplt_date IS NOT NULL THEN
    IF l_batch_header.actual_cmplt_date > sysdate THEN
    x_message_name := 'SY_NOFUTUREDATE';
    x_app_name := 'GMA';
    gme_common_pvt.log_message(p_message_code => 'SY_NOFUTUREDATE'
                              ,p_product_code => 'GMA');
      x_message_list := get_error_message;
      x_return_status := fnd_api.G_RET_STS_ERROR;
      IF g_debug <= gme_debug.g_log_statement THEN
        gme_debug.put_line('error occurred: ' || x_message_list);
      END IF;
      RETURN;
    END IF;
    OPEN CUR_MAX_ACTUAL_CMPLT(l_batch_header.batch_id);
    FETCH CUR_MAX_ACTUAL_CMPLT INTO l_actual_cmplt_step;
    CLOSE CUR_MAX_ACTUAL_CMPLT;
    IF l_batch_header.actual_cmplt_date < l_actual_cmplt_step THEN
      x_message_name := 'GME_INV_ACTCMP_DATE_RANGE';
      x_message_detail(1).param := 'DATE1';
      x_message_detail(2).param := 'DATE2';
      x_message_detail(1).token := fnd_date.date_to_displayDT(l_batch_header.actual_cmplt_date);
      x_message_detail(2).token := fnd_date.date_to_displayDT(l_actual_cmplt_step);
      gme_common_pvt.log_message('GME_INV_ACTCMP_DATE_RANGE', 
                                'DATE1', 
                                fnd_date.date_to_displayDT(l_batch_header.actual_cmplt_date),
                                'DATE2',
                                fnd_date.date_to_displayDT(l_actual_cmplt_step));
      x_message_list := get_error_message;
      x_return_status := fnd_api.G_RET_STS_ERROR;
      IF g_debug <= gme_debug.g_log_statement THEN
        gme_debug.put_line('error occurred: ' || x_message_list);
      END IF;
      RETURN;
    END IF;
  END IF;
  IF NVL(l_batch_header.actual_start_date, x_batch_header.actual_start_date) > NVL(l_batch_header.actual_cmplt_date, x_batch_header.actual_cmplt_date) THEN
    x_message_name := 'GME_INVALID_DATE_RANGE';
    x_message_detail(1).param := 'DATE1';
    x_message_detail(2).param := 'DATE2';
    x_message_detail(1).token := 'ACTUAL_CMPLT_DATE';
    x_message_detail(2).token := 'ACTUAL_START_DATE';
    gme_common_pvt.log_message('GME_INVALID_DATE_RANGE', 
                               'DATE1',
                               'ACTUAL_CMPLT_DATE',
                               'DATE2',
                               'ACTUAL_START_DATE');
    x_message_list := get_error_message;
    x_return_status := fnd_api.G_RET_STS_ERROR;
    IF g_debug <= gme_debug.g_log_statement THEN
      gme_debug.put_line('error occurred: ' || x_message_list);
    END IF;
    RETURN;
  END IF;
  x_batch_header.due_date          := NVL(l_batch_header.due_date, x_batch_header.due_date);
  x_batch_header.firmed_ind        := NVL(l_batch_header.firmed_ind, x_batch_header.firmed_ind);
  x_batch_header.actual_start_date := NVL(l_batch_header.actual_start_date, x_batch_header.actual_start_date);
  x_batch_header.actual_cmplt_date := NVL(l_batch_header.actual_cmplt_date, x_batch_header.actual_cmplt_date);

  IF p_validate_flexfield = 'T' THEN
    gme_common_pvt.g_flex_validate_prof := 1;
  ELSE
    gme_common_pvt.g_flex_validate_prof := 0;
  END IF;
  
  IF (fnd_api.to_boolean (p_validate_flexfield) 
      AND (p_batch_header.attribute_category IS NOT NULL 
           OR p_batch_header.attribute1 IS NOT NULL
           OR p_batch_header.attribute2 IS NOT NULL
           OR p_batch_header.attribute3 IS NOT NULL
           OR p_batch_header.attribute4 IS NOT NULL
           OR p_batch_header.attribute5 IS NOT NULL
           OR p_batch_header.attribute6 IS NOT NULL
           OR p_batch_header.attribute7 IS NOT NULL
           OR p_batch_header.attribute8 IS NOT NULL
           OR p_batch_header.attribute9 IS NOT NULL
           OR p_batch_header.attribute10 IS NOT NULL
           OR p_batch_header.attribute11 IS NOT NULL
           OR p_batch_header.attribute12 IS NOT NULL
           OR p_batch_header.attribute13 IS NOT NULL
           OR p_batch_header.attribute14 IS NOT NULL
           OR p_batch_header.attribute15 IS NOT NULL
           OR p_batch_header.attribute16 IS NOT NULL
           OR p_batch_header.attribute17 IS NOT NULL
           OR p_batch_header.attribute18 IS NOT NULL
           OR p_batch_header.attribute19 IS NOT NULL
           OR p_batch_header.attribute20 IS NOT NULL
           OR p_batch_header.attribute21 IS NOT NULL
           OR p_batch_header.attribute22 IS NOT NULL
           OR p_batch_header.attribute23 IS NOT NULL
           OR p_batch_header.attribute24 IS NOT NULL
           OR p_batch_header.attribute25 IS NOT NULL
           OR p_batch_header.attribute26 IS NOT NULL
           OR p_batch_header.attribute27 IS NOT NULL
           OR p_batch_header.attribute28 IS NOT NULL
           OR p_batch_header.attribute29 IS NOT NULL
           OR p_batch_header.attribute30 IS NOT NULL
           OR p_batch_header.attribute31 IS NOT NULL
           OR p_batch_header.attribute32 IS NOT NULL
           OR p_batch_header.attribute33 IS NOT NULL
           OR p_batch_header.attribute34 IS NOT NULL
           OR p_batch_header.attribute35 IS NOT NULL
           OR p_batch_header.attribute36 IS NOT NULL
           OR p_batch_header.attribute37 IS NOT NULL
           OR p_batch_header.attribute38 IS NOT NULL
           OR p_batch_header.attribute39 IS NOT NULL
           OR p_batch_header.attribute40 IS NOT NULL)) THEN
    gme_validate_flex_fld_pvt.validate_flex_batch_header(p_batch_header           => p_batch_header
                                                        ,x_batch_header           => l_batch_header
                                                        ,x_return_status          => x_return_status);
    IF (x_return_status <> fnd_api.g_ret_sts_success) THEN
      x_message_list := get_error_message;
      RETURN;
    END IF;
    x_batch_header.attribute_category := l_batch_header.attribute_category;
    x_batch_header.attribute1         := l_batch_header.attribute1;
    x_batch_header.attribute2         := l_batch_header.attribute2;
    x_batch_header.attribute3         := l_batch_header.attribute3;
    x_batch_header.attribute4         := l_batch_header.attribute4;
    x_batch_header.attribute5         := l_batch_header.attribute5;
    x_batch_header.attribute6         := l_batch_header.attribute6;
    x_batch_header.attribute7         := l_batch_header.attribute7;
    x_batch_header.attribute8         := l_batch_header.attribute8;
    x_batch_header.attribute9         := l_batch_header.attribute9;
    x_batch_header.attribute10        := l_batch_header.attribute10;
    x_batch_header.attribute11        := l_batch_header.attribute11;
    x_batch_header.attribute12        := l_batch_header.attribute12;
    x_batch_header.attribute13        := l_batch_header.attribute13;
    x_batch_header.attribute14        := l_batch_header.attribute14;
    x_batch_header.attribute15        := l_batch_header.attribute15;
    x_batch_header.attribute16        := l_batch_header.attribute16;
    x_batch_header.attribute17        := l_batch_header.attribute17;
    x_batch_header.attribute18        := l_batch_header.attribute18;
    x_batch_header.attribute19        := l_batch_header.attribute19;
    x_batch_header.attribute20        := l_batch_header.attribute20;
    x_batch_header.attribute21        := l_batch_header.attribute21;
    x_batch_header.attribute22        := l_batch_header.attribute22;
    x_batch_header.attribute23        := l_batch_header.attribute23;
    x_batch_header.attribute24        := l_batch_header.attribute24;
    x_batch_header.attribute25        := l_batch_header.attribute25;
    x_batch_header.attribute26        := l_batch_header.attribute26;
    x_batch_header.attribute27        := l_batch_header.attribute27;
    x_batch_header.attribute28        := l_batch_header.attribute28;
    x_batch_header.attribute29        := l_batch_header.attribute29;
    x_batch_header.attribute30        := l_batch_header.attribute30;
    x_batch_header.attribute31        := l_batch_header.attribute31;
    x_batch_header.attribute32        := l_batch_header.attribute32;
    x_batch_header.attribute33        := l_batch_header.attribute33;
    x_batch_header.attribute34        := l_batch_header.attribute34;
    x_batch_header.attribute35        := l_batch_header.attribute35;
    x_batch_header.attribute36        := l_batch_header.attribute36;
    x_batch_header.attribute37        := l_batch_header.attribute37;
    x_batch_header.attribute38        := l_batch_header.attribute38;
    x_batch_header.attribute39        := l_batch_header.attribute39;
    x_batch_header.attribute40        := l_batch_header.attribute40;
  END IF;
  l_return := gme_batch_header_dbl.update_row(p_batch_header => x_batch_header);
  IF NOT l_return THEN
    x_message_list := get_error_message;
    x_return_status := fnd_api.G_RET_STS_ERROR;
    RETURN;
  END IF;
  x_return_status := fnd_api.g_ret_sts_success;
  IF g_debug <= gme_debug.g_log_statement THEN
    gme_debug.put_line('finished update_batch_header');
  END IF;
EXCEPTION
  WHEN OTHERS THEN
    x_message_list := SQLERRM;
    x_return_status := fnd_api.G_RET_STS_ERROR;
    IF g_debug >= gme_debug.g_log_statement THEN
      gme_debug.put_line('Errored in package gme_batch_open_interface, procedure update_batch_header with error: ' || SQLERRM);
    END IF;
END update_batch_header;

/*================================================================================
  Procedure
    update_step
  Description
    This procedure updates step fields into table gme_batch_steps
  Parameters
    p_batch_step         in parameter for batch step 
    p_validate_flexfield in parameter to check flexfields or not, default to no
    x_batch_step         out for batch step
    x_message_list       message text
    x_return_status      status to return back
  Return 
    error message text
  History
    Shaliu Chen 25-Jul-2015 Bug 21208206
    add actual_start_date and actual_cmplt_date validation for batch on hold  
================================================================================*/
PROCEDURE update_step(p_batch_step         IN          gme_batch_steps%ROWTYPE
                     ,p_validate_flexfield IN          VARCHAR2 DEFAULT fnd_api.g_false
                     ,x_batch_step         OUT NOCOPY  gme_batch_steps%ROWTYPE
                     ,x_message_list       OUT NOCOPY  VARCHAR2
                     ,x_message_name       OUT NOCOPY  VARCHAR2
                     ,x_app_name           OUT NOCOPY  VARCHAR2
                     ,x_message_detail     OUT NOCOPY  T_MESSAGE_TABLE
                     ,x_return_status      OUT NOCOPY  VARCHAR2
                     )
IS
  l_actual_start_activity   gme_batch_step_activities.actual_start_date%TYPE;
  l_actual_cmplt_activity   gme_batch_step_activities.actual_cmplt_date%TYPE;
  
  l_batch_step_rec          gme_batch_steps%ROWTYPE;
  l_batch_header            gme_batch_header%ROWTYPE;
  x_message_count           NUMBER;
  l_return                  BOOLEAN;
  
  --Cursor to fetch batch actual_start_date and actual_cmplt_date
  CURSOR CUR_BATCH(v_batch_id NUMBER) IS
  SELECT automatic_step_calculation, enforce_step_dependency, batch_status, actual_start_date, actual_cmplt_date
  FROM   gme_batch_header 
  WHERE  batch_id = v_batch_id;
  
  --Cursor to fetch min(actual_start_date) from activity
  CURSOR CUR_MIN_ACTUAL_START(v_batchstep_id NUMBER) IS
  SELECT MIN(actual_start_date)
  FROM   gme_batch_step_activities 
  WHERE  batchstep_id = v_batchstep_id;
  
  --Cursor to fetch max(actual_cmplt_date) from activity
  CURSOR CUR_MAX_ACTUAL_CMPLT(v_batchstep_id NUMBER) IS
  SELECT MAX(actual_cmplt_date)
  FROM   gme_batch_step_activities 
  WHERE  batchstep_id = v_batchstep_id;
BEGIN
  IF g_debug <= gme_debug.g_log_statement THEN
    gme_debug.put_line('enter update_step');
  END IF;
  x_app_name := 'GME';
  IF x_message_detail IS NOT NULL AND x_message_detail.COUNT > 1 THEN
    x_message_detail.DELETE;
  END IF;
  SAVEPOINT update_step_sp;
  l_batch_step_rec := p_batch_step;

  gme_common_pvt.set_timestamp;
  l_return := gme_common_pvt.get_batch_step(p_batch_step_rec       => p_batch_step
                                           ,p_org_code             => NULL
										   ,p_batch_no             => NULL
										   ,x_batch_step_rec       => x_batch_step
										   ,x_batch_header_rec     => l_batch_header);
  IF NOT l_return THEN
    x_message_list := get_error_message;
    x_return_status := fnd_api.G_RET_STS_ERROR;
    RETURN;
  END IF;
  
  IF l_batch_header.batch_status = gme_common_pvt.g_batch_closed 
     OR l_batch_header.batch_status = gme_common_pvt.g_batch_cancelled THEN
    x_message_name := 'GME_INV_BATCH_STATUS_OPER';
    gme_common_pvt.log_message('GME_INV_BATCH_STATUS_OPER');
    x_message_list := get_error_message;
    x_return_status := fnd_api.G_RET_STS_ERROR;
    IF g_debug <= gme_debug.g_log_statement THEN
      gme_debug.put_line('error occurred: ' || x_message_list);
    END IF;
    RETURN;
  END IF;  
  
  IF x_batch_step.step_status = gme_common_pvt.g_step_closed 
     OR x_batch_step.step_status = gme_common_pvt.g_step_cancelled THEN
    x_message_name := 'GME_API_INV_STAT_STEP_EDIT';
    gme_common_pvt.log_message('GME_API_INV_STAT_STEP_EDIT');
    x_message_list := get_error_message;
    x_return_status := fnd_api.G_RET_STS_ERROR;
    IF g_debug <= gme_debug.g_log_statement THEN
      gme_debug.put_line('error occurred: ' || x_message_list);
    END IF;
    RETURN;
  END IF;  
  
  --Cannot change steprelease_type when enforce_step_dependency is enabled
  IF l_batch_header.enforce_step_dependency = 1 THEN
    l_batch_step_rec.steprelease_type := NULL;
  END IF;
  
  --automatic_step_calculation is enabled, then can only change due_date, actual_cmplt_date and step_close_date
  IF l_batch_header.automatic_step_calculation = 1 THEN
    l_batch_step_rec.steprelease_type := NULL;
    l_batch_step_rec.actual_start_date := NULL;
    l_batch_step_rec.actual_charges := NULL;
    l_batch_step_rec.plan_step_qty := NULL;
    l_batch_step_rec.actual_step_qty := NULL;
    IF x_batch_step.step_status = gme_common_pvt.g_step_pending AND x_batch_step.step_status = gme_common_pvt.g_step_wip THEN
      l_batch_step_rec.step_close_date := NULL;
      l_batch_step_rec.actual_cmplt_date := NULL;
    ELSIF x_batch_step.step_status = gme_common_pvt.g_step_completed THEN
      l_batch_step_rec.step_close_date := NULL;
    ELSIF x_batch_step.step_status = gme_common_pvt.g_step_closed THEN
      l_batch_step_rec.actual_cmplt_date := NULL;
      IF l_batch_header.batch_status = gme_common_pvt.g_batch_completed THEN
        l_batch_step_rec.due_date := NULL;
        l_batch_step_rec.step_close_date := NULL;
      END IF;
    ELSE
     l_batch_step_rec.actual_cmplt_date := NULL;
     l_batch_step_rec.due_date := NULL;
     l_batch_step_rec.step_close_date := NULL;
    END IF;
  ELSE
    IF x_batch_step.step_status = gme_common_pvt.g_step_pending THEN
      --steprelease_type can only be changed when batch status is pending
      IF l_batch_header.batch_status <> gme_common_pvt.g_batch_pending THEN
        l_batch_step_rec.steprelease_type := NULL;
      END IF;
      l_batch_step_rec.actual_start_date := NULL;
      l_batch_step_rec.actual_charges := NULL;
      l_batch_step_rec.actual_step_qty := NULL;
    ELSIF x_batch_step.step_status = gme_common_pvt.g_step_wip THEN
      l_batch_step_rec.steprelease_type := NULL;
      l_batch_step_rec.plan_step_qty := NULL;
      l_batch_step_rec.step_close_date := NULL;
      l_batch_step_rec.actual_cmplt_date := NULL;
    ELSIF x_batch_step.step_status = gme_common_pvt.g_step_completed THEN
      l_batch_step_rec.steprelease_type := NULL;
      l_batch_step_rec.plan_step_qty := NULL;
      l_batch_step_rec.step_close_date := NULL;
    ELSIF x_batch_step.step_status = gme_common_pvt.g_step_closed THEN
      l_batch_step_rec.steprelease_type := NULL;
      l_batch_step_rec.plan_step_qty := NULL;
      l_batch_step_rec.actual_start_date := NULL;
      l_batch_step_rec.actual_charges := NULL;
      l_batch_step_rec.actual_step_qty := NULL;
      l_batch_step_rec.actual_cmplt_date := NULL;
      IF l_batch_header.batch_status = gme_common_pvt.g_batch_completed THEN
        l_batch_step_rec.due_date := NULL;
        l_batch_step_rec.step_close_date := NULL;
      END IF;
    ELSE
      l_batch_step_rec.steprelease_type := NULL;
      l_batch_step_rec.plan_step_qty := NULL;
      l_batch_step_rec.actual_start_date := NULL;
      l_batch_step_rec.actual_charges := NULL;
      l_batch_step_rec.actual_step_qty := NULL;
      l_batch_step_rec.actual_cmplt_date := NULL;
      l_batch_step_rec.due_date := NULL;
      l_batch_step_rec.step_close_date := NULL;
    END IF;
  END IF;
  
   /*
    BUG 21208206  25-JUN-2015
    check whether the actual_start_date and actul_cmplt_date fall into hold period
   */
  IF l_batch_step_rec.actual_start_date IS NOT NULL THEN
    IF gme_common_pvt.get_batch_hold_status(p_batch_id => l_batch_step_rec.batch_id,
                                            p_date     => l_batch_step_rec.actual_start_date) <> 'R' THEN
      gme_common_pvt.log_message('GME_ACTUAL_DATE_ONHOLD',
                                 'ACTUAL_DATE',
                                 'Actual_Start_Date');        
      RETURN;       
    END IF;                
  END IF;
      
  IF l_batch_step_rec.actual_cmplt_date IS NOT NULL THEN
    IF gme_common_pvt.get_batch_hold_status(p_batch_id => l_batch_step_rec.batch_id,
                                            p_date     => l_batch_step_rec.actual_cmplt_date) <> 'R' THEN
      gme_common_pvt.log_message('GME_ACTUAL_DATE_ONHOLD',
                                 'ACTUAL_DATE',
                                 'Actual_Completion_Date');        
      RETURN;        
    END IF;                
  END IF;    
  
  IF l_batch_step_rec.actual_start_date IS NOT NULL THEN
    IF l_batch_step_rec.actual_start_date > sysdate THEN
      x_app_name := 'GMA';
      x_message_name := 'SY_NOFUTUREDATE';
      gme_common_pvt.log_message(p_message_code => 'SY_NOFUTUREDATE'
                                ,p_product_code => 'GMA');
      x_message_list := get_error_message;
      x_return_status := fnd_api.G_RET_STS_ERROR;
      IF g_debug <= gme_debug.g_log_statement THEN
        gme_debug.put_line('error occurred: ' || x_message_list);
      END IF;
      RETURN;
    END IF;
    IF l_batch_step_rec.actual_start_date < l_batch_header.actual_start_date THEN
      x_message_name := 'GME_INV_ACT_DATE_RANGE';
      x_message_detail(1).param := 'DATE1';
      x_message_detail(1).token := fnd_date.date_to_displayDT(l_batch_header.actual_start_date);
      x_message_detail(2).param := 'DATE2';
      x_message_detail(2).token := fnd_date.date_to_displayDT(l_batch_step_rec.actual_start_date);
      gme_common_pvt.log_message('GME_INV_ACT_DATE_RANGE', 
                                 'DATE1', 
                                 fnd_date.date_to_displayDT(l_batch_header.actual_start_date),
                                 'DATE2',
                                 fnd_date.date_to_displayDT(l_batch_step_rec.actual_start_date));
      x_message_list := get_error_message;
      x_return_status := fnd_api.G_RET_STS_ERROR;
      IF g_debug <= gme_debug.g_log_statement THEN
        gme_debug.put_line('error occurred: ' || x_message_list);
      END IF;
      RETURN;
    END IF;
    OPEN CUR_MIN_ACTUAL_START(l_batch_step_rec.batchstep_id);
    FETCH CUR_MIN_ACTUAL_START INTO l_actual_start_activity;
    CLOSE CUR_MIN_ACTUAL_START;
    IF l_batch_step_rec.actual_start_date > l_actual_start_activity THEN
      x_message_name := 'GME_INV_STEP_ACT_DATE_RANGE';
      x_message_detail(1).param := 'DATE1';
      x_message_detail(1).token := fnd_date.date_to_displayDT(l_batch_step_rec.actual_start_date);
      x_message_detail(2).param := 'DATE2';
      x_message_detail(2).token := fnd_date.date_to_displayDT(l_actual_start_activity);
      gme_common_pvt.log_message('GME_INV_STEP_ACT_DATE_RANGE', 
                                'DATE1', 
                                fnd_date.date_to_displayDT(l_batch_step_rec.actual_start_date),
                                'DATE2',
                                fnd_date.date_to_displayDT(l_actual_start_activity));
      x_message_list := get_error_message;
      x_return_status := fnd_api.G_RET_STS_ERROR;
      IF g_debug <= gme_debug.g_log_statement THEN
        gme_debug.put_line('error occurred: ' || x_message_list);
      END IF;
      RETURN;
    END IF;
  END IF;

  IF l_batch_step_rec.actual_cmplt_date IS NOT NULL THEN
    IF l_batch_step_rec.actual_cmplt_date > sysdate THEN
      x_message_name := 'SY_NOFUTUREDATE';
      x_app_name := 'GMA';
      gme_common_pvt.log_message(p_message_code => 'SY_NOFUTUREDATE'
                                ,p_product_code => 'GMA');
      x_message_list := get_error_message;
      x_return_status := fnd_api.G_RET_STS_ERROR;
      IF g_debug <= gme_debug.g_log_statement THEN
        gme_debug.put_line('error occurred: ' || x_message_list);
      END IF;
      RETURN;
    END IF;
    IF l_batch_step_rec.actual_cmplt_date > l_batch_header.actual_cmplt_date THEN
      x_message_name := 'GME_INV_ACTCMP_DATE_RANGE';
      x_message_detail(1).param := 'DATE1';
      x_message_detail(1).token := fnd_date.date_to_displayDT(l_batch_header.actual_cmplt_date);
      x_message_detail(2).param := 'DATE2';
      x_message_detail(2).token := fnd_date.date_to_displayDT(l_batch_step_rec.actual_cmplt_date);
      gme_common_pvt.log_message('GME_INV_ACTCMP_DATE_RANGE', 
                                 'DATE1', 
                                 fnd_date.date_to_displayDT(l_batch_header.actual_cmplt_date),
                                 'DATE2',
                                 fnd_date.date_to_displayDT(l_batch_step_rec.actual_cmplt_date));
      x_message_list := get_error_message;
      x_return_status := fnd_api.G_RET_STS_ERROR;
      IF g_debug <= gme_debug.g_log_statement THEN
        gme_debug.put_line('error occurred: ' || x_message_list);
      END IF;
      RETURN;
    END IF;
    OPEN CUR_MAX_ACTUAL_CMPLT(l_batch_step_rec.batchstep_id);
    FETCH CUR_MAX_ACTUAL_CMPLT INTO l_actual_cmplt_activity;
    CLOSE CUR_MAX_ACTUAL_CMPLT;
    IF l_batch_step_rec.actual_cmplt_date < l_actual_cmplt_activity THEN
      x_message_name := 'GME_INV_STEP_ACTCMP_DATE_RANGE';
      x_message_detail(1).param := 'DATE1';
      x_message_detail(1).token := fnd_date.date_to_displayDT(l_batch_step_rec.actual_cmplt_date);
      x_message_detail(2).param := 'DATE2';
      x_message_detail(2).token := fnd_date.date_to_displayDT(l_actual_cmplt_activity);
      gme_common_pvt.log_message('GME_INV_STEP_ACTCMP_DATE_RANGE', 
                                 'DATE1', 
                                 fnd_date.date_to_displayDT(l_batch_step_rec.actual_cmplt_date),
                                 'DATE2',
                                 fnd_date.date_to_displayDT(l_actual_cmplt_activity));
      x_message_list := get_error_message;
      x_return_status := fnd_api.G_RET_STS_ERROR;
      IF g_debug <= gme_debug.g_log_statement THEN
        gme_debug.put_line('error occurred: ' || x_message_list);
      END IF;
      RETURN;
    END IF;
  END IF;
    
  IF NVL(l_batch_step_rec.actual_start_date, x_batch_step.actual_start_date) > NVL (l_batch_step_rec.actual_cmplt_date, x_batch_step.actual_cmplt_date) THEN
    x_message_name := 'GME_INVALID_DATE_RANGE';
    x_message_detail(1).param := 'DATE1';
    x_message_detail(1).token := 'ACTUAL_CMPLT_DATE';
    x_message_detail(2).param := 'DATE2';
    x_message_detail(2).token := 'ACTUAL_START_DATE';
    gme_common_pvt.log_message('GME_INVALID_DATE_RANGE', 
                               'DATE1', 
                               'ACTUAL_CMPLT_DATE',
                               'DATE2',
                               'ACTUAL_START_DATE');
    x_message_list := get_error_message;
    x_return_status := fnd_api.G_RET_STS_ERROR;
    IF g_debug <= gme_debug.g_log_statement THEN
      gme_debug.put_line('error occurred: ' || x_message_list);
    END IF;
    RETURN;
  END IF;
    
  x_batch_step.due_date          := NVL(l_batch_step_rec.due_date, x_batch_step.due_date);
  x_batch_step.steprelease_type  := NVL(l_batch_step_rec.steprelease_type, x_batch_step.steprelease_type);
  x_batch_step.plan_step_qty     := NVL(l_batch_step_rec.plan_step_qty, x_batch_step.plan_step_qty);
  x_batch_step.actual_start_date := NVL(l_batch_step_rec.actual_start_date, x_batch_step.actual_start_date);
  x_batch_step.actual_charges    := NVL(l_batch_step_rec.actual_charges, x_batch_step.actual_charges);
  x_batch_step.actual_cmplt_date := NVL(l_batch_step_rec.actual_cmplt_date, x_batch_step.actual_cmplt_date);
  x_batch_step.step_close_date   := NVL(l_batch_step_rec.step_close_date, x_batch_step.step_close_date);
  
  IF p_validate_flexfield = 'T' THEN
    gme_common_pvt.g_flex_validate_prof := 1;
  ELSE
    gme_common_pvt.g_flex_validate_prof := 0;
  END IF;
  
  IF (fnd_api.to_boolean (p_validate_flexfield) 
      AND (p_batch_step.attribute_category IS NOT NULL 
           OR p_batch_step.attribute1 IS NOT NULL
           OR p_batch_step.attribute2 IS NOT NULL
           OR p_batch_step.attribute3 IS NOT NULL
           OR p_batch_step.attribute4 IS NOT NULL
           OR p_batch_step.attribute5 IS NOT NULL
           OR p_batch_step.attribute6 IS NOT NULL
           OR p_batch_step.attribute7 IS NOT NULL
           OR p_batch_step.attribute8 IS NOT NULL
           OR p_batch_step.attribute9 IS NOT NULL
           OR p_batch_step.attribute10 IS NOT NULL
           OR p_batch_step.attribute11 IS NOT NULL
           OR p_batch_step.attribute12 IS NOT NULL
           OR p_batch_step.attribute13 IS NOT NULL
           OR p_batch_step.attribute14 IS NOT NULL
           OR p_batch_step.attribute15 IS NOT NULL
           OR p_batch_step.attribute16 IS NOT NULL
           OR p_batch_step.attribute17 IS NOT NULL
           OR p_batch_step.attribute18 IS NOT NULL
           OR p_batch_step.attribute19 IS NOT NULL
           OR p_batch_step.attribute20 IS NOT NULL
           OR p_batch_step.attribute21 IS NOT NULL
           OR p_batch_step.attribute22 IS NOT NULL
           OR p_batch_step.attribute23 IS NOT NULL
           OR p_batch_step.attribute24 IS NOT NULL
           OR p_batch_step.attribute25 IS NOT NULL
           OR p_batch_step.attribute26 IS NOT NULL
           OR p_batch_step.attribute27 IS NOT NULL
           OR p_batch_step.attribute28 IS NOT NULL
           OR p_batch_step.attribute29 IS NOT NULL
           OR p_batch_step.attribute30 IS NOT NULL)) THEN
    gme_validate_flex_fld_pvt.validate_flex_batch_step(p_batch_step           => p_batch_step
                                                      ,x_batch_step           => l_batch_step_rec
                                                      ,x_return_status          => x_return_status);
    IF (x_return_status <> fnd_api.g_ret_sts_success) THEN
      x_message_list := get_error_message;
      RETURN;
    END IF;
    x_batch_step.attribute_category := l_batch_step_rec.attribute_category;
    x_batch_step.attribute1         := l_batch_step_rec.attribute1;
    x_batch_step.attribute2         := l_batch_step_rec.attribute2;
    x_batch_step.attribute3         := l_batch_step_rec.attribute3;
    x_batch_step.attribute4         := l_batch_step_rec.attribute4;
    x_batch_step.attribute5         := l_batch_step_rec.attribute5;
    x_batch_step.attribute6         := l_batch_step_rec.attribute6;
    x_batch_step.attribute7         := l_batch_step_rec.attribute7;
    x_batch_step.attribute8         := l_batch_step_rec.attribute8;
    x_batch_step.attribute9         := l_batch_step_rec.attribute9;
    x_batch_step.attribute10        := l_batch_step_rec.attribute10;
    x_batch_step.attribute11        := l_batch_step_rec.attribute11;
    x_batch_step.attribute12        := l_batch_step_rec.attribute12;
    x_batch_step.attribute13        := l_batch_step_rec.attribute13;
    x_batch_step.attribute14        := l_batch_step_rec.attribute14;
    x_batch_step.attribute15        := l_batch_step_rec.attribute15;
    x_batch_step.attribute16        := l_batch_step_rec.attribute16;
    x_batch_step.attribute17        := l_batch_step_rec.attribute17;
    x_batch_step.attribute18        := l_batch_step_rec.attribute18;
    x_batch_step.attribute19        := l_batch_step_rec.attribute19;
    x_batch_step.attribute20        := l_batch_step_rec.attribute20;
    x_batch_step.attribute21        := l_batch_step_rec.attribute21;
    x_batch_step.attribute22        := l_batch_step_rec.attribute22;
    x_batch_step.attribute23        := l_batch_step_rec.attribute23;
    x_batch_step.attribute24        := l_batch_step_rec.attribute24;
    x_batch_step.attribute25        := l_batch_step_rec.attribute25;
    x_batch_step.attribute26        := l_batch_step_rec.attribute26;
    x_batch_step.attribute27        := l_batch_step_rec.attribute27;
    x_batch_step.attribute28        := l_batch_step_rec.attribute28;
    x_batch_step.attribute29        := l_batch_step_rec.attribute29;
    x_batch_step.attribute30        := l_batch_step_rec.attribute30;
  END IF;
  
  l_return := gme_batch_steps_dbl.update_row(p_batch_step => x_batch_step);
  IF NOT l_return THEN
    x_message_list := get_error_message;
    x_return_status := fnd_api.G_RET_STS_ERROR;
    RETURN;
  END IF;
  
  IF x_batch_step.step_status = gme_common_pvt.g_step_wip OR x_batch_step.step_status = gme_common_pvt.g_step_completed THEN
    IF l_batch_step_rec.actual_step_qty IS NOT NULL AND l_batch_step_rec.actual_step_qty <> x_batch_step.actual_step_qty THEN
      gme_api_pub.update_batchstep_qty(p_init_msg_list          => fnd_api.g_true
                                      ,p_commit                 => fnd_api.g_true
                                      ,p_org_code               => NULL
                                      ,p_batch_step_rec         => l_batch_step_rec
                                      ,x_batch_step_rec         => x_batch_step
                                      ,x_message_count          => x_message_count
                                      ,x_message_list           => x_message_list
                                      ,x_return_status          => x_return_status);
      IF x_return_status <> fnd_api.g_ret_sts_success THEN
        ROLLBACK TO SAVEPOINT update_step_sp;
      END IF;
    END IF;
  END IF;
  x_return_status := fnd_api.G_RET_STS_SUCCESS;
  IF g_debug <= gme_debug.g_log_statement THEN
    gme_debug.put_line('finished update_step');
  END IF;
EXCEPTION
  WHEN OTHERS THEN
    ROLLBACK TO SAVEPOINT update_step_sp;
    x_message_list := SQLERRM;
    x_return_status := fnd_api.G_RET_STS_ERROR;
    IF g_debug >= gme_debug.g_log_statement THEN
      gme_debug.put_line('Errored in package gme_batch_open_interface, procedure update_step with error: ' || SQLERRM);
    END IF;
END update_step;

/*================================================================================
  Procedure
    batch_header_error_handler
  Description
    This procedure handles error occurred for batch header, log error message to 
    table gme_intf_errors, and set the errorred row to failed, if load_type is
    created from recipe, then alse set all related detail rows to failed.
  Parameters
    p_err_message        error message text
    p_err_message_name   error message name
    p_err_message_detail error message params and tokens
    p_group_id           error occurred group_id
    p_interface_id       error occurred interface_header_id
    p_load_type          error occurred load_type
    p_column_name        error occurred column name
  Return 
    error message text
  History
================================================================================*/
PROCEDURE batch_header_error_handler(p_err_message        VARCHAR2
                                    ,p_err_message_name   VARCHAR2 DEFAULT NULL
                                    ,p_app_name           VARCHAR2 DEFAULT NULL
                                    ,p_err_message_detail T_MESSAGE_TABLE
                                    ,p_group_id           NUMBER
                                    ,p_interface_id       NUMBER
                                    ,p_load_type          NUMBER
                                    ,p_column_name        VARCHAR2 DEFAULT NULL
                                    )
IS
  returnCode            NUMBER;
  l_sysdate             DATE;
  l_request_id          CONSTANT NUMBER := NVL (FND_GLOBAL.conc_request_id, -1);
  l_last_updated_by     CONSTANT NUMBER := NVL (FND_GLOBAL.user_id, -1);
  l_last_update_login   CONSTANT NUMBER := NVL (FND_GLOBAL.login_id, -1);
  l_param1              GME_INTF_ERRORS.param1%TYPE;
  l_param2              GME_INTF_ERRORS.param2%TYPE;
  l_param3              GME_INTF_ERRORS.param3%TYPE;
  l_param4              GME_INTF_ERRORS.param4%TYPE;
  l_param5              GME_INTF_ERRORS.param5%TYPE;
  l_param6              GME_INTF_ERRORS.param6%TYPE;
  l_token1              GME_INTF_ERRORS.token1%TYPE;
  l_token2              GME_INTF_ERRORS.token2%TYPE;
  l_token3              GME_INTF_ERRORS.token3%TYPE;
  l_token4              GME_INTF_ERRORS.token4%TYPE;
  l_token5              GME_INTF_ERRORS.token5%TYPE;
  l_token6              GME_INTF_ERRORS.token6%TYPE;
BEGIN
  g_errorred := TRUE;
  IF p_err_message_name IS NOT NULL THEN
    FOR i IN 1..p_err_message_detail.COUNT LOOP
      IF i = 1 THEN
        l_param1 := p_err_message_detail(i).param;
        l_token1 := p_err_message_detail(i).token;
      ELSIF i = 2 THEN
        l_param2 := p_err_message_detail(i).param;
        l_token2 := p_err_message_detail(i).token;
      ELSIF i = 3 THEN
        l_param3 := p_err_message_detail(i).param;
        l_token3 := p_err_message_detail(i).token;
      ELSIF i = 4 THEN
        l_param4 := p_err_message_detail(i).param;
        l_token4 := p_err_message_detail(i).token;
      ELSIF i = 5 THEN
        l_param5 := p_err_message_detail(i).param;
        l_token5 := p_err_message_detail(i).token;
      ELSIF i = 6 THEN
        l_param6 := p_err_message_detail(i).param;
        l_token6 := p_err_message_detail(i).token;
      END IF;
    END LOOP;
  END IF;
  returnCode := gme_log_interface_err
               (P_GROUP_ID            => p_group_id,
                P_object_type         => to_char(OBJECTYPE_BATCH),
                P_INTERFACE_ID        => p_interface_id,
                P_COLUMN_NAME         => p_column_name,
                P_MESSAGE_TYPE        => MESSAGE_TYPE_ERROR,
                P_MESSAGE_NAME        => p_err_message_name,
                P_MESSAGE_TEXT        => p_err_message,
                P_APP_NAME            => p_app_name,
                P_SOURCE_TABLE_NAME   => 'GME_BATCH_HEADER_INTERFACE',
                P_PARAM1              => l_param1,
                P_PARAM2              => l_param2,
                P_PARAM3              => l_param3,
                P_PARAM4              => l_param4,
                P_PARAM5              => l_param5,
                P_PARAM6              => l_param6,
                P_TOKEN1              => l_token1,
                P_TOKEN2              => l_token2,
                P_TOKEN3              => l_token3,
                P_TOKEN4              => l_token4,
                P_TOKEN5              => l_token5,
                P_TOKEN6              => l_token6
                );
  l_sysdate := SYSDATE;
  IF p_load_type = LOADTYPE_HEADER_RECIPE THEN
    --Only set details to failed when created a batch failed
    UPDATE gme_batch_dtls_interface
    SET    process_status = PROCESS_STATUS_FAILED,
           request_id = l_request_id,
           last_updated_by = l_last_updated_by,
           last_update_date = l_sysdate,
           last_update_login = l_last_update_login
    WHERE  interface_header_id = p_interface_id
    AND    group_id = p_group_id;
  END IF;
  UPDATE gme_batch_header_interface
  SET    process_status = PROCESS_STATUS_FAILED,
         request_id = l_request_id,
         last_updated_by = l_last_updated_by,
         last_update_date = l_sysdate,
         last_update_login = l_last_update_login
  WHERE  (interface_header_id = p_interface_id OR p_interface_id = -1)
  AND    group_id = p_group_id;
EXCEPTION
  WHEN OTHERS THEN
    IF g_debug >= gme_debug.g_log_statement THEN
      gme_debug.put_line('error occurred at package gme_batch_open_interface, procedure batch_header_error_handler: ' || SQLERRM);
    END IF;
END batch_header_error_handler;

/*================================================================================
  Procedure
    batch_dtls_error_handler
  Description
    This procedure handles error occurred for batch detail, log error message to 
    table gme_intf_errors, and set the errorred row to failed.
  Parameters
    p_err_message        error message text
    p_err_message_name   error message name
    p_err_message_detail error message params and tokens
    p_group_id           error occurred group_id
    p_interface_id       error occurred interface_id
    p_object_type        error occurred object_type
    p_load_type          error occurred load_type
    p_column_name        error occurred column name
  Return 
    error message text
  History
================================================================================*/
PROCEDURE batch_dtls_error_handler(p_err_message        VARCHAR2
                                  ,p_err_message_name   VARCHAR2 DEFAULT NULL
                                  ,p_app_name           VARCHAR2 DEFAULT NULL
                                  ,p_err_message_detail T_MESSAGE_TABLE
                                  ,p_group_id           NUMBER
                                  ,p_interface_id       NUMBER
                                  ,p_object_type        NUMBER
                                  ,p_load_type          NUMBER
                                  ,p_column_name        VARCHAR2 DEFAULT NULL
                                  )
IS
  returnCode            NUMBER;
  l_sysdate             DATE;
  l_request_id          CONSTANT NUMBER := NVL (FND_GLOBAL.conc_request_id, -1);
  l_last_updated_by     CONSTANT NUMBER := NVL (FND_GLOBAL.user_id, -1);
  l_last_update_login   CONSTANT NUMBER := NVL (FND_GLOBAL.login_id, -1);
  l_param1              GME_INTF_ERRORS.param1%TYPE;
  l_param2              GME_INTF_ERRORS.param2%TYPE;
  l_param3              GME_INTF_ERRORS.param3%TYPE;
  l_param4              GME_INTF_ERRORS.param4%TYPE;
  l_param5              GME_INTF_ERRORS.param5%TYPE;
  l_param6              GME_INTF_ERRORS.param6%TYPE;
  l_token1              GME_INTF_ERRORS.token1%TYPE;
  l_token2              GME_INTF_ERRORS.token2%TYPE;
  l_token3              GME_INTF_ERRORS.token3%TYPE;
  l_token4              GME_INTF_ERRORS.token4%TYPE;
  l_token5              GME_INTF_ERRORS.token5%TYPE;
  l_token6              GME_INTF_ERRORS.token6%TYPE;
BEGIN
  g_errorred := TRUE;
  IF p_err_message_name IS NOT NULL THEN
    FOR i IN 1..p_err_message_detail.COUNT LOOP
      IF i = 1 THEN
        l_param1 := p_err_message_detail(i).param;
        l_token1 := p_err_message_detail(i).token;
      ELSIF i = 2 THEN
        l_param2 := p_err_message_detail(i).param;
        l_token2 := p_err_message_detail(i).token;
      ELSIF i = 3 THEN
        l_param3 := p_err_message_detail(i).param;
        l_token3 := p_err_message_detail(i).token;
      ELSIF i = 4 THEN
        l_param4 := p_err_message_detail(i).param;
        l_token4 := p_err_message_detail(i).token;
      ELSIF i = 5 THEN
        l_param5 := p_err_message_detail(i).param;
        l_token5 := p_err_message_detail(i).token;
      ELSIF i = 6 THEN
        l_param6 := p_err_message_detail(i).param;
        l_token6 := p_err_message_detail(i).token;
      END IF;
    END LOOP;
  END IF;
  returnCode := gme_log_interface_err
               (P_GROUP_ID            => p_group_id,
                P_object_type         => to_char(p_object_type),
                P_INTERFACE_ID        => p_interface_id,
                P_COLUMN_NAME         => p_column_name,
                P_MESSAGE_TYPE        => MESSAGE_TYPE_ERROR,
                P_MESSAGE_NAME        => p_err_message_name,
                P_APP_NAME            => p_app_name,
                P_MESSAGE_TEXT        => p_err_message,
                P_SOURCE_TABLE_NAME   => 'GME_BATCH_DTLS_INTERFACE',
                P_PARAM1              => l_param1,
                P_PARAM2              => l_param2,
                P_PARAM3              => l_param3,
                P_PARAM4              => l_param4,
                P_PARAM5              => l_param5,
                P_PARAM6              => l_param6,
                P_TOKEN1              => l_token1,
                P_TOKEN2              => l_token2,
                P_TOKEN3              => l_token3,
                P_TOKEN4              => l_token4,
                P_TOKEN5              => l_token5,
                P_TOKEN6              => l_token6
                );
  l_sysdate := SYSDATE;
  UPDATE gme_batch_dtls_interface
  SET    process_status = PROCESS_STATUS_FAILED,
         request_id = l_request_id,
         last_updated_by = l_last_updated_by,
         last_update_date = l_sysdate,
         last_update_login = l_last_update_login
  WHERE  (interface_id = p_interface_id
          OR (p_interface_id = -1 AND object_type = p_object_type))
  AND    group_id = p_group_id;
EXCEPTION
  WHEN OTHERS THEN
    IF g_debug >= gme_debug.g_log_statement THEN
      gme_debug.put_line('error occurred at package gme_batch_open_interface, procedure batch_dtls_error_handler: ' || SQLERRM);
    END IF;
END batch_dtls_error_handler;

/*================================================================================
  Procedure
    construct_batch_header
  Description
    This procedure constructs batch header from gme_batch_header_interface
  Parameters
    p_batch_header_int  a row of data from table gme_batch_header_interface
    x_batch_header_rec  a row of data for table gme_batch_header
  History
================================================================================*/
PROCEDURE construct_batch_header(p_batch_header_int     IN         gme_batch_header_interface%ROWTYPE
                                ,x_batch_header_rec     OUT NOCOPY gme_batch_header%ROWTYPE)
IS
BEGIN
  x_batch_header_rec.plant_code                 := p_batch_header_int.organization_code;
  x_batch_header_rec.batch_no                   := p_batch_header_int.batch_no;
  x_batch_header_rec.batch_id                   := p_batch_header_int.int_batch_id;
  x_batch_header_rec.batch_type                 := 0;
  x_batch_header_rec.recipe_validity_rule_id    := p_batch_header_int.recipe_validity_rule_id;
  x_batch_header_rec.plan_start_date            := p_batch_header_int.plan_start_date;
  x_batch_header_rec.actual_start_date          := p_batch_header_int.actual_start_date;
  x_batch_header_rec.due_date                   := p_batch_header_int.due_date;
  x_batch_header_rec.plan_cmplt_date            := p_batch_header_int.plan_cmplt_date;
  x_batch_header_rec.actual_cmplt_date          := p_batch_header_int.actual_cmplt_date;
  x_batch_header_rec.update_inventory_ind       := p_batch_header_int.update_inventory_ind;
  x_batch_header_rec.firmed_ind                 := p_batch_header_int.firmed_ind;
  x_batch_header_rec.laboratory_ind             := p_batch_header_int.laboratory_ind;
  x_batch_header_rec.organization_id            := p_batch_header_int.int_organization_id;
  x_batch_header_rec.fixed_process_loss_applied := p_batch_header_int.apply_fixed_process_loss;
  x_batch_header_rec.attribute_category         := p_batch_header_int.attribute_category;
  x_batch_header_rec.attribute1                 := p_batch_header_int.attribute1;
  x_batch_header_rec.attribute2                 := p_batch_header_int.attribute2;
  x_batch_header_rec.attribute3                 := p_batch_header_int.attribute3;
  x_batch_header_rec.attribute4                 := p_batch_header_int.attribute4;
  x_batch_header_rec.attribute5                 := p_batch_header_int.attribute5;
  x_batch_header_rec.attribute6                 := p_batch_header_int.attribute6;
  x_batch_header_rec.attribute7                 := p_batch_header_int.attribute7;
  x_batch_header_rec.attribute8                 := p_batch_header_int.attribute8;
  x_batch_header_rec.attribute9                 := p_batch_header_int.attribute9;
  x_batch_header_rec.attribute10                := p_batch_header_int.attribute10;
  x_batch_header_rec.attribute11                := p_batch_header_int.attribute11;
  x_batch_header_rec.attribute12                := p_batch_header_int.attribute12;
  x_batch_header_rec.attribute13                := p_batch_header_int.attribute13;
  x_batch_header_rec.attribute14                := p_batch_header_int.attribute14;
  x_batch_header_rec.attribute15                := p_batch_header_int.attribute15;
  x_batch_header_rec.attribute16                := p_batch_header_int.attribute16;
  x_batch_header_rec.attribute17                := p_batch_header_int.attribute17;
  x_batch_header_rec.attribute18                := p_batch_header_int.attribute18;
  x_batch_header_rec.attribute19                := p_batch_header_int.attribute19;
  x_batch_header_rec.attribute20                := p_batch_header_int.attribute20;
  x_batch_header_rec.attribute21                := p_batch_header_int.attribute21;
  x_batch_header_rec.attribute22                := p_batch_header_int.attribute22;
  x_batch_header_rec.attribute23                := p_batch_header_int.attribute23;
  x_batch_header_rec.attribute24                := p_batch_header_int.attribute24;
  x_batch_header_rec.attribute25                := p_batch_header_int.attribute25;
  x_batch_header_rec.attribute26                := p_batch_header_int.attribute26;
  x_batch_header_rec.attribute27                := p_batch_header_int.attribute27;
  x_batch_header_rec.attribute28                := p_batch_header_int.attribute28;
  x_batch_header_rec.attribute29                := p_batch_header_int.attribute29;
  x_batch_header_rec.attribute30                := p_batch_header_int.attribute30;
  x_batch_header_rec.attribute31                := p_batch_header_int.attribute31;
  x_batch_header_rec.attribute32                := p_batch_header_int.attribute32;
  x_batch_header_rec.attribute33                := p_batch_header_int.attribute33;
  x_batch_header_rec.attribute34                := p_batch_header_int.attribute34;
  x_batch_header_rec.attribute35                := p_batch_header_int.attribute35;
  x_batch_header_rec.attribute36                := p_batch_header_int.attribute36;
  x_batch_header_rec.attribute37                := p_batch_header_int.attribute37;
  x_batch_header_rec.attribute38                := p_batch_header_int.attribute38;
  x_batch_header_rec.attribute39                := p_batch_header_int.attribute39;
  x_batch_header_rec.attribute40                := p_batch_header_int.attribute40;
END construct_batch_header;
									 
/*================================================================================
  Procedure
    construct_material
  Description
    This procedure constructs batch material from curosr cur_batch_dtl_material
  Parameters
    p_material_int  a row of data from curosr cur_batch_dtl_material
    x_material_rec  a row of data for table gme_material_details
  History
================================================================================*/
PROCEDURE construct_material(p_material_int     IN         cur_batch_dtl_material%ROWTYPE
                            ,x_material_rec     OUT NOCOPY gme_material_details%ROWTYPE)
IS
BEGIN
  x_material_rec.material_detail_id        := p_material_int.int_material_detail_id;
  x_material_rec.batch_id                  := p_material_int.int_batch_id;
  x_material_rec.line_no                   := p_material_int.line_no;
  x_material_rec.line_type                 := p_material_int.line_type;
  x_material_rec.inventory_item_id         := p_material_int.int_inventory_item_id;
  x_material_rec.plan_qty                  := p_material_int.plan_qty;
  x_material_rec.actual_qty                := p_material_int.actual_qty;
  x_material_rec.release_type              := p_material_int.release_type;
  x_material_rec.scrap_factor              := p_material_int.scrap_factor;
  x_material_rec.scale_type                := p_material_int.scale_type;
  x_material_rec.phantom_type              := p_material_int.phantom_type;
  x_material_rec.cost_alloc                := p_material_int.cost_alloc;
  x_material_rec.rounding_direction        := p_material_int.rounding_direction;
  x_material_rec.scale_rounding_variance   := p_material_int.scale_rounding_variance;
  x_material_rec.scale_multiple            := p_material_int.scale_multiple;
  x_material_rec.contribute_yield_ind      := p_material_int.contribute_yield_ind;
  x_material_rec.wip_plan_qty              := p_material_int.wip_plan_qty;
  x_material_rec.original_qty              := p_material_int.original_qty;
  x_material_rec.by_product_type           := p_material_int.by_product_type;
  x_material_rec.contribute_step_qty_ind   := p_material_int.contribute_step_qty_ind;
  x_material_rec.dtl_um                    := p_material_int.dtl_um;
  x_material_rec.locator_id                := p_material_int.int_locator_id;
  x_material_rec.material_requirement_date := p_material_int.material_requirement_date;
  x_material_rec.organization_id           := p_material_int.int_organization_id;
  x_material_rec.original_primary_qty      := p_material_int.original_primary_qty;
  x_material_rec.revision                  := p_material_int.revision;
  x_material_rec.subinventory              := p_material_int.subinventory;
  x_material_rec.attribute_category        := p_material_int.attribute_category;
  x_material_rec.attribute1                := p_material_int.attribute1;
  x_material_rec.attribute2                := p_material_int.attribute2;
  x_material_rec.attribute3                := p_material_int.attribute3;
  x_material_rec.attribute4                := p_material_int.attribute4;
  x_material_rec.attribute5                := p_material_int.attribute5;
  x_material_rec.attribute6                := p_material_int.attribute6;
  x_material_rec.attribute7                := p_material_int.attribute7;
  x_material_rec.attribute8                := p_material_int.attribute8;
  x_material_rec.attribute9                := p_material_int.attribute9;
  x_material_rec.attribute10               := p_material_int.attribute10;
  x_material_rec.attribute11               := p_material_int.attribute11;
  x_material_rec.attribute12               := p_material_int.attribute12;
  x_material_rec.attribute13               := p_material_int.attribute13;
  x_material_rec.attribute14               := p_material_int.attribute14;
  x_material_rec.attribute15               := p_material_int.attribute15;
  x_material_rec.attribute16               := p_material_int.attribute16;
  x_material_rec.attribute17               := p_material_int.attribute17;
  x_material_rec.attribute18               := p_material_int.attribute18;
  x_material_rec.attribute19               := p_material_int.attribute19;
  x_material_rec.attribute20               := p_material_int.attribute20;
  x_material_rec.attribute21               := p_material_int.attribute21;
  x_material_rec.attribute22               := p_material_int.attribute22;
  x_material_rec.attribute23               := p_material_int.attribute23;
  x_material_rec.attribute24               := p_material_int.attribute24;
  x_material_rec.attribute25               := p_material_int.attribute25;
  x_material_rec.attribute26               := p_material_int.attribute26;
  x_material_rec.attribute27               := p_material_int.attribute27;
  x_material_rec.attribute28               := p_material_int.attribute28;
  x_material_rec.attribute29               := p_material_int.attribute29;
  x_material_rec.attribute30               := p_material_int.attribute30;
END construct_material;
							
/*================================================================================
  Procedure
    construct_step
  Description
    This procedure constructs batch step from curosr cur_batch_dtl_step
  Parameters
    p_step_int  a row of data from curosr cur_batch_dtl_step
    x_step_rec  a row of data for table gme_batch_steps
  History
================================================================================*/
PROCEDURE construct_step(p_step_int     IN         cur_batch_dtl_step%ROWTYPE
                        ,x_step_rec     OUT NOCOPY gme_batch_steps%ROWTYPE)
IS
BEGIN
  x_step_rec.batchstep_id              := p_step_int.int_batchstep_id;
  x_step_rec.batch_id                  := p_step_int.int_batch_id;
  x_step_rec.batchstep_no              := p_step_int.batchstep_no;
  x_step_rec.oprn_id                   := p_step_int.int_oprn_id;
  x_step_rec.plan_step_qty             := p_step_int.plan_step_qty;
  x_step_rec.actual_step_qty           := p_step_int.actual_step_qty;
  x_step_rec.plan_start_date           := p_step_int.plan_start_date;
  x_step_rec.plan_cmplt_date           := p_step_int.plan_cmplt_date;
  x_step_rec.actual_start_date         := p_step_int.actual_start_date;
  x_step_rec.actual_cmplt_date         := p_step_int.actual_cmplt_date;
  x_step_rec.actual_step_qty           := p_step_int.actual_step_qty;
  x_step_rec.due_date                  := p_step_int.due_date;
  x_step_rec.step_close_date           := p_step_int.step_close_date;
  x_step_rec.steprelease_type          := p_step_int.steprelease_type;
  x_step_rec.attribute_category        := p_step_int.attribute_category;
  x_step_rec.attribute1                := p_step_int.attribute1;
  x_step_rec.attribute2                := p_step_int.attribute2;
  x_step_rec.attribute3                := p_step_int.attribute3;
  x_step_rec.attribute4                := p_step_int.attribute4;
  x_step_rec.attribute5                := p_step_int.attribute5;
  x_step_rec.attribute6                := p_step_int.attribute6;
  x_step_rec.attribute7                := p_step_int.attribute7;
  x_step_rec.attribute8                := p_step_int.attribute8;
  x_step_rec.attribute9                := p_step_int.attribute9;
  x_step_rec.attribute10               := p_step_int.attribute10;
  x_step_rec.attribute11               := p_step_int.attribute11;
  x_step_rec.attribute12               := p_step_int.attribute12;
  x_step_rec.attribute13               := p_step_int.attribute13;
  x_step_rec.attribute14               := p_step_int.attribute14;
  x_step_rec.attribute15               := p_step_int.attribute15;
  x_step_rec.attribute16               := p_step_int.attribute16;
  x_step_rec.attribute17               := p_step_int.attribute17;
  x_step_rec.attribute18               := p_step_int.attribute18;
  x_step_rec.attribute19               := p_step_int.attribute19;
  x_step_rec.attribute20               := p_step_int.attribute20;
  x_step_rec.attribute21               := p_step_int.attribute21;
  x_step_rec.attribute22               := p_step_int.attribute22;
  x_step_rec.attribute23               := p_step_int.attribute23;
  x_step_rec.attribute24               := p_step_int.attribute24;
  x_step_rec.attribute25               := p_step_int.attribute25;
  x_step_rec.attribute26               := p_step_int.attribute26;
  x_step_rec.attribute27               := p_step_int.attribute27;
  x_step_rec.attribute28               := p_step_int.attribute28;
  x_step_rec.attribute29               := p_step_int.attribute29;
  x_step_rec.attribute30               := p_step_int.attribute30;
END construct_step;						
						
/*================================================================================
  Procedure
    construct_activity
  Description
    This procedure constructs batch activity from curosr cur_batch_dtl_activity
  Parameters
    p_activity_int  a row of data from curosr cur_batch_dtl_activity
    x_activity_rec  a row of data for table gme_batch_step_activities
  History
================================================================================*/
PROCEDURE construct_activity(p_activity_int     IN         cur_batch_dtl_activity%ROWTYPE
                            ,x_activity_rec     OUT NOCOPY gme_batch_step_activities%ROWTYPE)
IS
BEGIN
  x_activity_rec.batchstep_id              := p_activity_int.int_batchstep_id;
  x_activity_rec.batch_id                  := p_activity_int.int_batch_id;
  x_activity_rec.batchstep_activity_id     := p_activity_int.int_batchstep_activity_id;
  x_activity_rec.activity                  := p_activity_int.activity;
  x_activity_rec.actual_activity_factor    := p_activity_int.actual_activity_factor;
  x_activity_rec.plan_start_date           := p_activity_int.plan_start_date;
  x_activity_rec.plan_cmplt_date           := p_activity_int.plan_cmplt_date;
  x_activity_rec.actual_start_date         := p_activity_int.actual_start_date;
  x_activity_rec.actual_cmplt_date         := p_activity_int.actual_cmplt_date;
  x_activity_rec.offset_interval           := p_activity_int.offset_interval;
  x_activity_rec.plan_activity_factor      := p_activity_int.plan_activity_factor;
  x_activity_rec.sequence_dependent_ind    := p_activity_int.sequence_dependent_ind;
  x_activity_rec.attribute_category        := p_activity_int.attribute_category;
  x_activity_rec.attribute1                := p_activity_int.attribute1;
  x_activity_rec.attribute2                := p_activity_int.attribute2;
  x_activity_rec.attribute3                := p_activity_int.attribute3;
  x_activity_rec.attribute4                := p_activity_int.attribute4;
  x_activity_rec.attribute5                := p_activity_int.attribute5;
  x_activity_rec.attribute6                := p_activity_int.attribute6;
  x_activity_rec.attribute7                := p_activity_int.attribute7;
  x_activity_rec.attribute8                := p_activity_int.attribute8;
  x_activity_rec.attribute9                := p_activity_int.attribute9;
  x_activity_rec.attribute10               := p_activity_int.attribute10;
  x_activity_rec.attribute11               := p_activity_int.attribute11;
  x_activity_rec.attribute12               := p_activity_int.attribute12;
  x_activity_rec.attribute13               := p_activity_int.attribute13;
  x_activity_rec.attribute14               := p_activity_int.attribute14;
  x_activity_rec.attribute15               := p_activity_int.attribute15;
  x_activity_rec.attribute16               := p_activity_int.attribute16;
  x_activity_rec.attribute17               := p_activity_int.attribute17;
  x_activity_rec.attribute18               := p_activity_int.attribute18;
  x_activity_rec.attribute19               := p_activity_int.attribute19;
  x_activity_rec.attribute20               := p_activity_int.attribute20;
  x_activity_rec.attribute21               := p_activity_int.attribute21;
  x_activity_rec.attribute22               := p_activity_int.attribute22;
  x_activity_rec.attribute23               := p_activity_int.attribute23;
  x_activity_rec.attribute24               := p_activity_int.attribute24;
  x_activity_rec.attribute25               := p_activity_int.attribute25;
  x_activity_rec.attribute26               := p_activity_int.attribute26;
  x_activity_rec.attribute27               := p_activity_int.attribute27;
  x_activity_rec.attribute28               := p_activity_int.attribute28;
  x_activity_rec.attribute29               := p_activity_int.attribute29;
  x_activity_rec.attribute30               := p_activity_int.attribute30;
END construct_activity;
							
/*================================================================================
  Procedure
    construct_resource
  Description
    This procedure constructs batch resource from curosr cur_batch_dtl_resource
  Parameters
    p_resource_int  a row of data from curosr cur_batch_dtl_resource
    x_resource_rec  a row of data for table gme_batch_step_resources
  History
================================================================================*/
PROCEDURE construct_resource(p_resource_int     IN         cur_batch_dtl_resource%ROWTYPE
                            ,x_resource_rec     OUT NOCOPY gme_batch_step_resources%ROWTYPE
                            ,x_message_list     OUT NOCOPY VARCHAR2
                            ,x_return_status    OUT NOCOPY VARCHAR2)
IS
  CURSOR cur_get_prim_rsrc_ind(v_batchstep_resource_id NUMBER) IS
  SELECT prim_rsrc_ind
  FROM   gme_batch_step_resources
  WHERE  batchstep_resource_id = v_batchstep_resource_id;
BEGIN
  x_resource_rec.batchstep_id              := p_resource_int.int_batchstep_id;
  x_resource_rec.batch_id                  := p_resource_int.int_batch_id;
  x_resource_rec.batchstep_activity_id     := p_resource_int.int_batchstep_activity_id;
  x_resource_rec.batchstep_resource_id     := p_resource_int.int_batchstep_resource_id;
  x_resource_rec.organization_id           := p_resource_int.int_organization_id;
  x_resource_rec.resources                 := p_resource_int.resources;
  x_resource_rec.cost_analysis_code        := p_resource_int.cost_analysis_code;
  x_resource_rec.cost_cmpntcls_id          := p_resource_int.int_cost_cmpntcls_id;
  x_resource_rec.prim_rsrc_ind             := p_resource_int.prim_rsrc_ind;
  x_resource_rec.scale_type                := p_resource_int.scale_type;
  x_resource_rec.plan_rsrc_count           := p_resource_int.plan_rsrc_count;
  x_resource_rec.actual_rsrc_count         := p_resource_int.actual_rsrc_count;			
  x_resource_rec.actual_rsrc_qty           := p_resource_int.actual_rsrc_qty;	
  x_resource_rec.actual_rsrc_usage         := p_resource_int.actual_rsrc_usage;
  x_resource_rec.original_rsrc_qty         := p_resource_int.original_rsrc_qty;			
  x_resource_rec.original_rsrc_usage       := p_resource_int.original_rsrc_usage;			
  x_resource_rec.plan_rsrc_qty             := p_resource_int.plan_rsrc_qty;
  x_resource_rec.plan_rsrc_usage           := p_resource_int.plan_rsrc_usage;			
  x_resource_rec.resource_qty_um           := p_resource_int.resource_qty_um;		
  x_resource_rec.plan_start_date           := p_resource_int.plan_start_date;
  x_resource_rec.plan_cmplt_date           := p_resource_int.plan_cmplt_date;
  x_resource_rec.actual_start_date         := p_resource_int.actual_start_date;
  x_resource_rec.actual_cmplt_date         := p_resource_int.actual_cmplt_date;
  x_resource_rec.offset_interval           := p_resource_int.offset_interval;
  x_resource_rec.attribute_category        := p_resource_int.attribute_category;
  x_resource_rec.attribute1                := p_resource_int.attribute1;
  x_resource_rec.attribute2                := p_resource_int.attribute2;
  x_resource_rec.attribute3                := p_resource_int.attribute3;
  x_resource_rec.attribute4                := p_resource_int.attribute4;
  x_resource_rec.attribute5                := p_resource_int.attribute5;
  x_resource_rec.attribute6                := p_resource_int.attribute6;
  x_resource_rec.attribute7                := p_resource_int.attribute7;
  x_resource_rec.attribute8                := p_resource_int.attribute8;
  x_resource_rec.attribute9                := p_resource_int.attribute9;
  x_resource_rec.attribute10               := p_resource_int.attribute10;
  x_resource_rec.attribute11               := p_resource_int.attribute11;
  x_resource_rec.attribute12               := p_resource_int.attribute12;
  x_resource_rec.attribute13               := p_resource_int.attribute13;
  x_resource_rec.attribute14               := p_resource_int.attribute14;
  x_resource_rec.attribute15               := p_resource_int.attribute15;
  x_resource_rec.attribute16               := p_resource_int.attribute16;
  x_resource_rec.attribute17               := p_resource_int.attribute17;
  x_resource_rec.attribute18               := p_resource_int.attribute18;
  x_resource_rec.attribute19               := p_resource_int.attribute19;
  x_resource_rec.attribute20               := p_resource_int.attribute20;
  x_resource_rec.attribute21               := p_resource_int.attribute21;
  x_resource_rec.attribute22               := p_resource_int.attribute22;
  x_resource_rec.attribute23               := p_resource_int.attribute23;
  x_resource_rec.attribute24               := p_resource_int.attribute24;
  x_resource_rec.attribute25               := p_resource_int.attribute25;
  x_resource_rec.attribute26               := p_resource_int.attribute26;
  x_resource_rec.attribute27               := p_resource_int.attribute27;
  x_resource_rec.attribute28               := p_resource_int.attribute28;
  x_resource_rec.attribute29               := p_resource_int.attribute29;
  x_resource_rec.attribute30               := p_resource_int.attribute30;
  
  --Need to fetch prim_rsrc_ind, then no error occurred when updating resources
  IF p_resource_int.load_type = LOADTYPE_DETAIL_UPDATE THEN
    OPEN cur_get_prim_rsrc_ind(p_resource_int.int_batchstep_resource_id);
    FETCH cur_get_prim_rsrc_ind INTO x_resource_rec.prim_rsrc_ind;
    CLOSE cur_get_prim_rsrc_ind;
  END IF;
  x_return_status := fnd_api.g_ret_sts_success;
EXCEPTION
  WHEN OTHERS THEN
    x_message_list := SQLERRM;
    x_return_status := fnd_api.G_RET_STS_ERROR;
    IF g_debug >= gme_debug.g_log_statement THEN
      gme_debug.put_line('Errored in package gme_batch_open_interface, procedure construct_resource with error: ' || SQLERRM);
    END IF;
END construct_resource;
							
/*================================================================================
  Procedure
    construct_parameter
  Description
    This procedure constructs batch process parameter from curosr cur_batch_dtl_parameter
  Parameters
    p_parameter_int  a row of data from curosr cur_batch_dtl_parameter
    x_parameter_rec  a row of data for table gme_process_parameters
  History
================================================================================*/
PROCEDURE construct_parameter(p_parameter_int     IN         cur_batch_dtl_parameter%ROWTYPE
                             ,x_parameter_rec     OUT NOCOPY gme_process_parameters%ROWTYPE)
IS
BEGIN
  x_parameter_rec.batchstep_id              := p_parameter_int.int_batchstep_id;
  x_parameter_rec.batch_id                  := p_parameter_int.int_batch_id;
  x_parameter_rec.batchstep_activity_id     := p_parameter_int.int_batchstep_activity_id;
  x_parameter_rec.batchstep_resource_id     := p_parameter_int.int_batchstep_resource_id;
  x_parameter_rec.process_param_id          := p_parameter_int.int_process_param_id;
  x_parameter_rec.parameter_id              := p_parameter_int.int_parameter_id;
  x_parameter_rec.target_value              := p_parameter_int.target_value;
  x_parameter_rec.actual_value              := p_parameter_int.actual_value;
  x_parameter_rec.attribute_category        := p_parameter_int.attribute_category;
  x_parameter_rec.attribute1                := p_parameter_int.attribute1;
  x_parameter_rec.attribute2                := p_parameter_int.attribute2;
  x_parameter_rec.attribute3                := p_parameter_int.attribute3;
  x_parameter_rec.attribute4                := p_parameter_int.attribute4;
  x_parameter_rec.attribute5                := p_parameter_int.attribute5;
  x_parameter_rec.attribute6                := p_parameter_int.attribute6;
  x_parameter_rec.attribute7                := p_parameter_int.attribute7;
  x_parameter_rec.attribute8                := p_parameter_int.attribute8;
  x_parameter_rec.attribute9                := p_parameter_int.attribute9;
  x_parameter_rec.attribute10               := p_parameter_int.attribute10;
  x_parameter_rec.attribute11               := p_parameter_int.attribute11;
  x_parameter_rec.attribute12               := p_parameter_int.attribute12;
  x_parameter_rec.attribute13               := p_parameter_int.attribute13;
  x_parameter_rec.attribute14               := p_parameter_int.attribute14;
  x_parameter_rec.attribute15               := p_parameter_int.attribute15;
  x_parameter_rec.attribute16               := p_parameter_int.attribute16;
  x_parameter_rec.attribute17               := p_parameter_int.attribute17;
  x_parameter_rec.attribute18               := p_parameter_int.attribute18;
  x_parameter_rec.attribute19               := p_parameter_int.attribute19;
  x_parameter_rec.attribute20               := p_parameter_int.attribute20;
  x_parameter_rec.attribute21               := p_parameter_int.attribute21;
  x_parameter_rec.attribute22               := p_parameter_int.attribute22;
  x_parameter_rec.attribute23               := p_parameter_int.attribute23;
  x_parameter_rec.attribute24               := p_parameter_int.attribute24;
  x_parameter_rec.attribute25               := p_parameter_int.attribute25;
  x_parameter_rec.attribute26               := p_parameter_int.attribute26;
  x_parameter_rec.attribute27               := p_parameter_int.attribute27;
  x_parameter_rec.attribute28               := p_parameter_int.attribute28;
  x_parameter_rec.attribute29               := p_parameter_int.attribute29;
  x_parameter_rec.attribute30               := p_parameter_int.attribute30;
END construct_parameter;
							 
/*================================================================================
  Procedure
    construct_step_item
  Description
    This procedure constructs batch step item association from curosr cur_batch_dtl_stepitem
  Parameters
    p_step_item_int  a row of data from curosr cur_batch_dtl_stepitem
    x_step_item_rec  a row of data for table gme_batch_step_items
  History
================================================================================*/
PROCEDURE construct_step_item(p_step_item_int     IN         cur_batch_dtl_stepitem%ROWTYPE
                             ,x_step_item_rec     OUT NOCOPY gme_batch_step_items%ROWTYPE)
IS
BEGIN
  x_step_item_rec.batchstep_id              := p_step_item_int.int_batchstep_id;
  x_step_item_rec.batch_id                  := p_step_item_int.int_batch_id;
  x_step_item_rec.material_detail_id        := p_step_item_int.int_material_detail_id;
  x_step_item_rec.attribute_category        := p_step_item_int.attribute_category;
  x_step_item_rec.attribute1                := p_step_item_int.attribute1;
  x_step_item_rec.attribute2                := p_step_item_int.attribute2;
  x_step_item_rec.attribute3                := p_step_item_int.attribute3;
  x_step_item_rec.attribute4                := p_step_item_int.attribute4;
  x_step_item_rec.attribute5                := p_step_item_int.attribute5;
  x_step_item_rec.attribute6                := p_step_item_int.attribute6;
  x_step_item_rec.attribute7                := p_step_item_int.attribute7;
  x_step_item_rec.attribute8                := p_step_item_int.attribute8;
  x_step_item_rec.attribute9                := p_step_item_int.attribute9;
  x_step_item_rec.attribute10               := p_step_item_int.attribute10;
  x_step_item_rec.attribute11               := p_step_item_int.attribute11;
  x_step_item_rec.attribute12               := p_step_item_int.attribute12;
  x_step_item_rec.attribute13               := p_step_item_int.attribute13;
  x_step_item_rec.attribute14               := p_step_item_int.attribute14;
  x_step_item_rec.attribute15               := p_step_item_int.attribute15;
  x_step_item_rec.attribute16               := p_step_item_int.attribute16;
  x_step_item_rec.attribute17               := p_step_item_int.attribute17;
  x_step_item_rec.attribute18               := p_step_item_int.attribute18;
  x_step_item_rec.attribute19               := p_step_item_int.attribute19;
  x_step_item_rec.attribute20               := p_step_item_int.attribute20;
  x_step_item_rec.attribute21               := p_step_item_int.attribute21;
  x_step_item_rec.attribute22               := p_step_item_int.attribute22;
  x_step_item_rec.attribute23               := p_step_item_int.attribute23;
  x_step_item_rec.attribute24               := p_step_item_int.attribute24;
  x_step_item_rec.attribute25               := p_step_item_int.attribute25;
  x_step_item_rec.attribute26               := p_step_item_int.attribute26;
  x_step_item_rec.attribute27               := p_step_item_int.attribute27;
  x_step_item_rec.attribute28               := p_step_item_int.attribute28;
  x_step_item_rec.attribute29               := p_step_item_int.attribute29;
  x_step_item_rec.attribute30               := p_step_item_int.attribute30;
  x_step_item_rec.material_detail_id        := p_step_item_int.int_material_detail_id;
END construct_step_item;

/*================================================================================
  Procedure
    insert_step_item
  Description
    This procedure inserts a batch step item
  Parameters
    p_batch_step_items  IN         gme_batch_step_items%ROWTYPE
    x_batch_step_items  OUT NOCOPY gme_batch_step_items%ROWTYPE
  History
================================================================================*/
PROCEDURE insert_step_item(p_batch_step_items     IN         gme_batch_step_items%ROWTYPE
                          ,p_validate_flexfield   IN         VARCHAR2
                          ,x_batch_step_items     OUT NOCOPY gme_batch_step_items%ROWTYPE
                          ,x_message_list         OUT NOCOPY VARCHAR2
                          ,x_message_name         OUT NOCOPY VARCHAR2
                          ,x_return_status        OUT NOCOPY VARCHAR2)
IS
  CURSOR cur_batch_status_dates(v_batch_id NUMBER) IS
  SELECT batch_status, plan_start_date, plan_cmplt_date
  FROM   gme_batch_header
  WHERE  batch_id =  v_batch_id;
  
  CURSOR cur_material(v_material_detail_id NUMBER) IS
  SELECT release_type, line_type, material_requirement_date
  FROM   gme_material_details
  WHERE  material_detail_id = v_material_detail_id;
  
  CURSOR cur_step_dates(v_batchstep_id NUMBER) IS
  SELECT plan_start_date,plan_cmplt_date
  FROM   gme_batch_steps
  WHERE  batchstep_id = v_batchstep_id;
  
  l_batch_status              gme_batch_header.batch_status%TYPE;
  l_login                     NUMBER := NVL (FND_GLOBAL.login_id, -1);
  l_userid                    NUMBER := NVL (FND_GLOBAL.user_id, -1);
  l_fetch                     NUMBER;
  l_return                    BOOLEAN;
  l_release_type              gme_material_details.release_type%TYPE;
  l_line_type                 gme_material_details.line_type%TYPE;
  l_material_requirement_date gme_material_details.material_requirement_date%TYPE;
  l_start_date                DATE;
  l_cmplt_date                DATE;
  p_batch_step_rec            gme_batch_steps%ROWTYPE;
  x_batch_step                gme_batch_steps%ROWTYPE;
  X_message_count             NUMBER;
BEGIN
  IF g_debug <= gme_debug.g_log_statement THEN
    gme_debug.put_line('enter insert_step_item');
  END IF;
  SAVEPOINT insert_step_item;
  OPEN cur_batch_status_dates(p_batch_step_items.batch_id);
  FETCH cur_batch_status_dates INTO l_batch_status, l_start_date, l_cmplt_date;
  CLOSE cur_batch_status_dates;
  --cannot insert step item association when batch status is not pending
  IF l_batch_status <> gme_common_pvt.g_batch_pending THEN
    x_message_name := 'GME_INV_BATCH_STATUS_OPER';
    gme_common_pvt.log_message('GME_INV_BATCH_STATUS_OPER');
    x_message_list := get_error_message;
    x_return_status := fnd_api.G_RET_STS_ERROR;
    IF g_debug <= gme_debug.g_log_statement THEN
      gme_debug.put_line('error occurred: ' || x_message_list);
    END IF;
	RETURN;
  END IF;

  IF p_validate_flexfield = 'T' THEN
    gme_common_pvt.g_flex_validate_prof := 1;
  ELSE
    gme_common_pvt.g_flex_validate_prof := 0;
  END IF;
  --call create flex procedure to insert the default values of the GME_BATCH_STEP_MAT_ASSOC DFF's segments if they are enabled
  gme_validate_flex_fld_pvt.create_flex_step_item(p_step_item_rec    => p_batch_step_items
                                                 ,x_step_item_rec    => x_batch_step_items
												 ,x_return_status    => x_return_status);
  IF x_return_status <> FND_API.g_ret_sts_success THEN
    x_message_list := get_error_message;
    x_return_status := fnd_api.G_RET_STS_ERROR;
    RETURN;
  END IF;
												   
  --insert the step item association
  l_return := gme_batch_step_items_dbl.insert_row(x_batch_step_items, x_batch_step_items);
  IF l_return = FALSE THEN
    ROLLBACK TO SAVEPOINT insert_step_item;
    x_message_list := get_error_message;
    x_return_status := fnd_api.G_RET_STS_ERROR;
    RETURN;
  END IF;
  OPEN cur_material(p_batch_step_items.material_detail_id);
  FETCH cur_material INTO l_release_type, l_line_type, l_material_requirement_date;
  CLOSE cur_material;
  IF l_release_type IN (1, 2, 3) THEN
    OPEN cur_step_dates(p_batch_step_items.batchstep_id);
    FETCH cur_step_dates INTO l_start_date, l_cmplt_date;
    CLOSE cur_step_dates;
  END IF;
  IF (l_line_type = -1 ) THEN
    IF (l_material_requirement_date <> l_start_Date) THEN
      gme_common_pvt.material_date_change (
          p_material_detail_id   => p_batch_step_items.material_detail_id
         ,p_material_date        => l_start_Date
         ,x_return_status        => x_return_status );  
    END IF;
  ELSE
    IF (l_material_requirement_date <> l_cmplt_Date) THEN
      gme_common_pvt.material_date_change (
          p_material_detail_id   => p_batch_step_items.material_detail_id
         ,p_material_date        => l_cmplt_Date
         ,x_return_status        => x_return_status );  
    END IF;  
  END IF;
  IF x_return_status <> fnd_api.G_RET_STS_SUCCESS THEN
    ROLLBACK TO SAVEPOINT insert_step_item;
    x_message_list := get_error_message;
    x_return_status := fnd_api.G_RET_STS_ERROR;
    RETURN;  
  END IF;
  p_batch_step_rec.batchstep_id := p_batch_step_items.batchstep_id;
  gme_update_step_qty_pvt.update_step_qty(p_batch_step_rec	   => p_batch_step_rec,
                                          x_message_count      => x_message_count,
                                          x_message_list       => x_message_list,
                                          x_return_status      => x_return_status,
                                          x_batch_step_Rec     => x_batch_step);
  IF x_return_status <> fnd_api.G_RET_STS_SUCCESS THEN
    ROLLBACK TO SAVEPOINT insert_step_item;
    x_message_list := get_error_message;
    x_return_status := fnd_api.G_RET_STS_ERROR;
    RETURN;  
  END IF;
  gme_api_pub.save_batch(p_header_id          => NULL
                        ,p_table              => NULL
                        ,p_commit             => fnd_api.g_false
                        ,x_return_status      => x_return_status);
  IF x_return_status = fnd_api.g_ret_sts_success THEN
    COMMIT;
  ELSE
    ROLLBACK TO SAVEPOINT insert_step_item;
    x_message_list := get_error_message;
    x_return_status := fnd_api.G_RET_STS_ERROR;
    RETURN;
  END IF;
  IF g_debug <= gme_debug.g_log_statement THEN
    gme_debug.put_line('finished insert_step_item');
  END IF;
EXCEPTION
  WHEN OTHERS THEN
    ROLLBACK TO SAVEPOINT insert_step_item;
    x_message_list := SQLERRM;
    x_return_status := fnd_api.G_RET_STS_ERROR;
    IF g_debug >= gme_debug.g_log_statement THEN
      gme_debug.put_line('Errored in package gme_batch_open_interface, procedure insert_step_item with error: ' || SQLERRM);
    END IF;
END insert_step_item;	
						  
/*================================================================================
  Procedure
    update_step_item
  Description
    This procedure updates a batch step item
  Parameters
    p_batch_step_items  IN         gme_batch_step_items%ROWTYPE
    x_batch_step_items  OUT NOCOPY gme_batch_step_items%ROWTYPE
  History
================================================================================*/
PROCEDURE update_step_item(p_batch_step_items     IN         gme_batch_step_items%ROWTYPE
                          ,p_validate_flexfield   IN         VARCHAR2
                          ,x_batch_step_items     OUT NOCOPY gme_batch_step_items%ROWTYPE
                          ,x_message_list         OUT NOCOPY VARCHAR2
                          ,x_message_name         OUT NOCOPY VARCHAR2
                          ,x_return_status        OUT NOCOPY VARCHAR2)
IS
  CURSOR cur_batch_status_dates(v_batch_id NUMBER) IS
  SELECT batch_status, plan_start_date, plan_cmplt_date
  FROM   gme_batch_header
  WHERE  batch_id =  v_batch_id;
  
  CURSOR cur_material(v_material_detail_id NUMBER) IS
  SELECT release_type, line_type, material_requirement_date
  FROM   gme_material_details
  WHERE  material_detail_id = v_material_detail_id;
  
  CURSOR cur_step_dates(v_batchstep_id NUMBER) IS
  SELECT plan_start_date,plan_cmplt_date
  FROM   gme_batch_steps
  WHERE  batchstep_id = v_batchstep_id;
  
  l_batch_status              gme_batch_header.batch_status%TYPE;
  l_login                     NUMBER := NVL (FND_GLOBAL.login_id, -1);
  l_userid                    NUMBER := NVL (FND_GLOBAL.user_id, -1);
  l_fetch                     NUMBER;
  l_return                    BOOLEAN;
  l_release_type              gme_material_details.release_type%TYPE;
  l_line_type                 gme_material_details.line_type%TYPE;
  l_material_requirement_date gme_material_details.material_requirement_date%TYPE;
  l_start_date                DATE;
  l_cmplt_date                DATE;
  p_batch_step_rec            gme_batch_steps%ROWTYPE;
  x_batch_step                gme_batch_steps%ROWTYPE;
  X_message_count             NUMBER;
  l_batch_step_items          gme_batch_step_items%ROWTYPE;
BEGIN
  IF g_debug <= gme_debug.g_log_statement THEN
    gme_debug.put_line('enter update_step_item');
  END IF;
  SAVEPOINT update_step_item;
  OPEN cur_batch_status_dates(p_batch_step_items.batch_id);
  FETCH cur_batch_status_dates INTO l_batch_status, l_start_date, l_cmplt_date;
  CLOSE cur_batch_status_dates;
  x_batch_step_items := p_batch_step_items;
  --cannot update step item association when batch status is not pending
  IF l_batch_status <> gme_common_pvt.g_batch_pending THEN
    x_message_name := 'GME_INV_BATCH_STATUS_OPER';
    gme_common_pvt.log_message('GME_INV_BATCH_STATUS_OPER');
    x_message_list := get_error_message;
    x_return_status := fnd_api.G_RET_STS_ERROR;
    IF g_debug <= gme_debug.g_log_statement THEN
      gme_debug.put_line('error occurred: ' || x_message_list);
    END IF;
  END IF;
  --fetch the exiting step item association
  l_return := gme_batch_step_items_dbl.fetch_row(p_batch_step_items, l_batch_step_items);
  IF l_return = FALSE THEN
    x_message_list := get_error_message;
    x_return_status := fnd_api.G_RET_STS_ERROR;
    RETURN;
  END IF;
  --change the last_update_date due to update_row will compare this field.
  x_batch_step_items.last_update_date  := l_batch_step_items.last_update_date;
  IF p_validate_flexfield = 'T' THEN
    gme_common_pvt.g_flex_validate_prof := 1;
  ELSE
    gme_common_pvt.g_flex_validate_prof := 0;
  END IF;
  IF (fnd_api.to_boolean (p_validate_flexfield) 
      AND (p_batch_step_items.attribute_category IS NOT NULL 
           OR p_batch_step_items.attribute1 IS NOT NULL
           OR p_batch_step_items.attribute2 IS NOT NULL
           OR p_batch_step_items.attribute3 IS NOT NULL
           OR p_batch_step_items.attribute4 IS NOT NULL
           OR p_batch_step_items.attribute5 IS NOT NULL
           OR p_batch_step_items.attribute6 IS NOT NULL
           OR p_batch_step_items.attribute7 IS NOT NULL
           OR p_batch_step_items.attribute8 IS NOT NULL
           OR p_batch_step_items.attribute9 IS NOT NULL
           OR p_batch_step_items.attribute10 IS NOT NULL
           OR p_batch_step_items.attribute11 IS NOT NULL
           OR p_batch_step_items.attribute12 IS NOT NULL
           OR p_batch_step_items.attribute13 IS NOT NULL
           OR p_batch_step_items.attribute14 IS NOT NULL
           OR p_batch_step_items.attribute15 IS NOT NULL
           OR p_batch_step_items.attribute16 IS NOT NULL
           OR p_batch_step_items.attribute17 IS NOT NULL
           OR p_batch_step_items.attribute18 IS NOT NULL
           OR p_batch_step_items.attribute19 IS NOT NULL
           OR p_batch_step_items.attribute20 IS NOT NULL
           OR p_batch_step_items.attribute21 IS NOT NULL
           OR p_batch_step_items.attribute22 IS NOT NULL
           OR p_batch_step_items.attribute23 IS NOT NULL
           OR p_batch_step_items.attribute24 IS NOT NULL
           OR p_batch_step_items.attribute25 IS NOT NULL
           OR p_batch_step_items.attribute26 IS NOT NULL
           OR p_batch_step_items.attribute27 IS NOT NULL
           OR p_batch_step_items.attribute28 IS NOT NULL
           OR p_batch_step_items.attribute29 IS NOT NULL
           OR p_batch_step_items.attribute30 IS NOT NULL)) THEN
    --validate values of the GME_BATCH_STEP_MAT_ASSOC DFF's segments
    gme_validate_flex_fld_pvt.validate_flex_step_item(p_step_item_rec    => p_batch_step_items
                                                     ,x_step_item_rec    => l_batch_step_items
                                                     ,x_return_status    => x_return_status);
    IF x_return_status <> FND_API.g_ret_sts_success THEN
      x_message_list := get_error_message;
      x_return_status := fnd_api.G_RET_STS_ERROR;
      RETURN;
    END IF;
    x_batch_step_items.attribute_category := l_batch_step_items.attribute_category;
    x_batch_step_items.attribute1         := l_batch_step_items.attribute1;
    x_batch_step_items.attribute2         := l_batch_step_items.attribute2;
    x_batch_step_items.attribute3         := l_batch_step_items.attribute3;
    x_batch_step_items.attribute4         := l_batch_step_items.attribute4;
    x_batch_step_items.attribute5         := l_batch_step_items.attribute5;
    x_batch_step_items.attribute6         := l_batch_step_items.attribute6;
    x_batch_step_items.attribute7         := l_batch_step_items.attribute7;
    x_batch_step_items.attribute8         := l_batch_step_items.attribute8;
    x_batch_step_items.attribute9         := l_batch_step_items.attribute9;
    x_batch_step_items.attribute10        := l_batch_step_items.attribute10;
    x_batch_step_items.attribute11        := l_batch_step_items.attribute11;
    x_batch_step_items.attribute12        := l_batch_step_items.attribute12;
    x_batch_step_items.attribute13        := l_batch_step_items.attribute13;
    x_batch_step_items.attribute14        := l_batch_step_items.attribute14;
    x_batch_step_items.attribute15        := l_batch_step_items.attribute15;
    x_batch_step_items.attribute16        := l_batch_step_items.attribute16;
    x_batch_step_items.attribute17        := l_batch_step_items.attribute17;
    x_batch_step_items.attribute18        := l_batch_step_items.attribute18;
    x_batch_step_items.attribute19        := l_batch_step_items.attribute19;
    x_batch_step_items.attribute20        := l_batch_step_items.attribute20;
    x_batch_step_items.attribute21        := l_batch_step_items.attribute21;
    x_batch_step_items.attribute22        := l_batch_step_items.attribute22;
    x_batch_step_items.attribute23        := l_batch_step_items.attribute23;
    x_batch_step_items.attribute24        := l_batch_step_items.attribute24;
    x_batch_step_items.attribute25        := l_batch_step_items.attribute25;
    x_batch_step_items.attribute26        := l_batch_step_items.attribute26;
    x_batch_step_items.attribute27        := l_batch_step_items.attribute27;
    x_batch_step_items.attribute28        := l_batch_step_items.attribute28;
    x_batch_step_items.attribute29        := l_batch_step_items.attribute29;
    x_batch_step_items.attribute30        := l_batch_step_items.attribute30;
  END IF;
  --update the step item association
  l_return := gme_batch_step_items_dbl.update_row(x_batch_step_items);
  IF l_return = FALSE THEN
    x_message_list := get_error_message;
    x_return_status := fnd_api.G_RET_STS_ERROR;
    RETURN;
  END IF;
  --No need to update step quantity and batch when batchstep_id is the same
  IF l_batch_step_items.batchstep_id = p_batch_step_items.batchstep_id THEN
    x_return_status := fnd_api.g_ret_sts_success;
    IF g_debug <= gme_debug.g_log_statement THEN
      gme_debug.put_line('finished update_step_item');
    END IF;
	RETURN;
  END IF;
  OPEN cur_material(p_batch_step_items.material_detail_id);
  FETCH cur_material INTO l_release_type, l_line_type, l_material_requirement_date;
  CLOSE cur_material;
  IF l_release_type IN (1, 2, 3) THEN
    OPEN cur_step_dates(p_batch_step_items.batchstep_id);
    FETCH cur_step_dates INTO l_start_date, l_cmplt_date;
    CLOSE cur_step_dates;
  END IF;
  IF (l_line_type = -1 ) THEN
    IF (l_material_requirement_date <> l_start_Date) THEN
      gme_common_pvt.material_date_change (
          p_material_detail_id   => p_batch_step_items.material_detail_id
         ,p_material_date        => l_start_Date
         ,x_return_status        => x_return_status );  
    END IF;
  ELSE
    IF (l_material_requirement_date <> l_cmplt_Date) THEN
      gme_common_pvt.material_date_change (
          p_material_detail_id   => p_batch_step_items.material_detail_id
         ,p_material_date        => l_cmplt_Date
         ,x_return_status        => x_return_status );  
    END IF;  
  END IF;
  IF x_return_status <> fnd_api.G_RET_STS_SUCCESS THEN
    ROLLBACK TO SAVEPOINT update_step_item;
    x_message_list := get_error_message;
    x_return_status := fnd_api.G_RET_STS_ERROR;
    RETURN;  
  END IF;
  p_batch_step_rec.batchstep_id := p_batch_step_items.batchstep_id;
  gme_update_step_qty_pvt.update_step_qty(p_batch_step_rec	   => p_batch_step_rec,
			  	                          x_message_count      => x_message_count,
                                	      x_message_list       => x_message_list,
			  	                          x_return_status      => x_return_status,
			  	                          x_batch_step_Rec     => x_batch_step);
  IF x_return_status <> fnd_api.G_RET_STS_SUCCESS THEN
    ROLLBACK TO SAVEPOINT update_step_item;
    x_message_list := get_error_message;
    x_return_status := fnd_api.G_RET_STS_ERROR;
    RETURN;  
  END IF;
  gme_api_pub.save_batch(p_header_id          => NULL
                        ,p_table              => NULL
                        ,p_commit             => fnd_api.g_false
                        ,x_return_status      => x_return_status);
  IF x_return_status = fnd_api.g_ret_sts_success THEN
    COMMIT;
  ELSE
    ROLLBACK TO SAVEPOINT update_step_item;
    x_message_list := get_error_message;
    x_return_status := fnd_api.G_RET_STS_ERROR;
    RETURN;
  END IF;
  IF g_debug <= gme_debug.g_log_statement THEN
    gme_debug.put_line('finished update_step_item');
  END IF;
EXCEPTION
  WHEN OTHERS THEN
    ROLLBACK TO SAVEPOINT update_step_item;
    x_message_list := SQLERRM;
    x_return_status := fnd_api.G_RET_STS_ERROR;
    IF g_debug >= gme_debug.g_log_statement THEN
      gme_debug.put_line('Errored in package gme_batch_open_interface, procedure update_step_item with error: ' || SQLERRM);
    END IF;
END update_step_item;
	  
/*================================================================================
  Procedure
    delete_step_item
  Description
    This procedure deletes a batch step item
  Parameters
    p_batch_step_items  IN         gme_batch_step_items%ROWTYPE
  History
================================================================================*/
PROCEDURE delete_step_item(p_batch_step_items   IN          gme_batch_step_items%ROWTYPE
                          ,x_message_list       OUT NOCOPY  VARCHAR2
                          ,x_message_name       OUT NOCOPY  VARCHAR2
                          ,x_return_status      OUT NOCOPY  VARCHAR2)
IS
  CURSOR cur_batch_status_dates(v_batch_id NUMBER) IS
  SELECT batch_status, plan_start_date, plan_cmplt_date
  FROM   gme_batch_header
  WHERE  batch_id =  v_batch_id;
  
  CURSOR cur_material(v_material_detail_id NUMBER) IS
  SELECT line_type, material_requirement_date
  FROM   gme_material_details
  WHERE  material_detail_id = v_material_detail_id;
  
  l_batch_status              gme_batch_header.batch_status%TYPE;
  l_fetch                     NUMBER;
  l_return                    BOOLEAN;
  l_line_type                 gme_material_details.line_type%TYPE;
  l_material_requirement_date gme_material_details.material_requirement_date%TYPE;
  l_start_date                DATE;
  l_cmplt_date                DATE;
  p_batch_step_rec            gme_batch_steps%ROWTYPE;
  x_batch_step                gme_batch_steps%ROWTYPE;
  X_message_count             NUMBER;
  l_batch_step_items          gme_batch_step_items%ROWTYPE;
BEGIN
  IF g_debug <= gme_debug.g_log_statement THEN
    gme_debug.put_line('enter delete_step_item');
  END IF;
  SAVEPOINT delete_step_item;
  OPEN cur_batch_status_dates(p_batch_step_items.batch_id);
  FETCH cur_batch_status_dates INTO l_batch_status, l_start_date, l_cmplt_date;
  CLOSE cur_batch_status_dates;
  --cannot delete step item association when batch status is not pending
  IF l_batch_status <> gme_common_pvt.g_batch_pending THEN
    x_message_name := 'GME_INV_BATCH_STATUS_OPER';
    gme_common_pvt.log_message('GME_INV_BATCH_STATUS_OPER');
    x_message_list := get_error_message;
    x_return_status := fnd_api.G_RET_STS_ERROR;
    IF g_debug <= gme_debug.g_log_statement THEN
      gme_debug.put_line('error occurred: ' || x_message_list);
    END IF;
  END IF;

  --delete the exiting step item association
  l_return := gme_batch_step_items_dbl.delete_row(p_batch_step_items);
  IF l_return = FALSE THEN
    x_message_list := get_error_message;
    x_return_status := fnd_api.G_RET_STS_ERROR;
    RETURN;
  END IF;
  OPEN cur_material(p_batch_step_items.material_detail_id);
  FETCH cur_material INTO l_line_type, l_material_requirement_date;
  CLOSE cur_material;
  IF (l_line_type = -1 ) THEN
    IF (l_material_requirement_date <> l_start_Date) THEN
      gme_common_pvt.material_date_change (
          p_material_detail_id   => p_batch_step_items.material_detail_id
         ,p_material_date        => l_start_Date
         ,x_return_status        => x_return_status );  
    END IF;
  ELSE
    IF (l_material_requirement_date <> l_cmplt_Date) THEN
      gme_common_pvt.material_date_change (
          p_material_detail_id   => p_batch_step_items.material_detail_id
         ,p_material_date        => l_cmplt_Date
         ,x_return_status        => x_return_status );  
    END IF;  
  END IF;
  IF x_return_status <> fnd_api.G_RET_STS_SUCCESS THEN
    ROLLBACK TO SAVEPOINT delete_step_item;
    x_message_list := get_error_message;
    x_return_status := fnd_api.G_RET_STS_ERROR;
    RETURN;  
  END IF;
  p_batch_step_rec.batchstep_id := p_batch_step_items.batchstep_id;
  gme_update_step_qty_pvt.update_step_qty(p_batch_step_rec	   => p_batch_step_rec,
			  	                          x_message_count      => x_message_count,
                                	      x_message_list       => x_message_list,
			  	                          x_return_status      => x_return_status,
			  	                          x_batch_step_Rec     => x_batch_step);
  IF x_return_status <> fnd_api.G_RET_STS_SUCCESS THEN
    ROLLBACK TO SAVEPOINT delete_step_item;
    x_message_list := get_error_message;
    x_return_status := fnd_api.G_RET_STS_ERROR;
    RETURN;  
  END IF;
  gme_api_pub.save_batch(p_header_id          => NULL
                        ,p_table              => NULL
                        ,p_commit             => fnd_api.g_false
                        ,x_return_status      => x_return_status);
  IF x_return_status = fnd_api.g_ret_sts_success THEN
    COMMIT;
  ELSE
    ROLLBACK TO SAVEPOINT delete_step_item;
    x_message_list := get_error_message;
    x_return_status := fnd_api.G_RET_STS_ERROR;
    RETURN;
  END IF;
  IF g_debug <= gme_debug.g_log_statement THEN
    gme_debug.put_line('finished delete_step_item');
  END IF;
EXCEPTION
  WHEN OTHERS THEN
    ROLLBACK TO SAVEPOINT delete_step_item;
    x_message_list := SQLERRM;
    x_return_status := fnd_api.G_RET_STS_ERROR;
    IF g_debug >= gme_debug.g_log_statement THEN
      gme_debug.put_line('Errored in package gme_batch_open_interface, procedure delete_step_item with error: ' || SQLERRM);
    END IF;
END delete_step_item;

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
      Added code to support load_type 15
================================================================================*/
PROCEDURE process_batches (err_buf            OUT NOCOPY VARCHAR2,
                           ret_code           OUT NOCOPY VARCHAR2,
                           p_group_id         IN         NUMBER,
                           p_purge_flag       IN         VARCHAR2 DEFAULT 'Y',
                           p_ignore_exception IN         VARCHAR2 DEFAULT 'N') IS
  x_message_count           NUMBER;
  x_message_list            VARCHAR2 (2000);
  x_message_name            VARCHAR2 (256);
  x_app_name                VARCHAR2 (3);
  x_message_detail          T_MESSAGE_TABLE;
  x_return_status           VARCHAR2 (1) := fnd_api.g_ret_sts_success;
  p_batch_header            gme_batch_header%ROWTYPE;
  x_batch_header            gme_batch_header%ROWTYPE;
  x_exception_material_tbl  gme_common_pvt.exceptions_tab;
  l_creation_mode           VARCHAR2 (10);
  l_ignore_exception        VARCHAR2(1);
  l_process_status          gme_batch_header_interface.process_status%TYPE;

  l_return                  BOOLEAN;
  l_process_header          BOOLEAN;
  l_process_detail          BOOLEAN;
  l_limit                   NUMBER := NVL(fnd_profile.VALUE('GME_BULK_LOAD_SIZE'), 1000);
  l_errorred                BOOLEAN := FALSE;

  l_sysdate                 DATE;
  l_request_id              CONSTANT NUMBER := NVL (FND_GLOBAL.conc_request_id, -1);
  l_user_id                 CONSTANT NUMBER := NVL (FND_GLOBAL.user_id, -1);
  l_login_id                CONSTANT NUMBER := NVL (FND_GLOBAL.login_id, -1);
  l_purge_request_id        NUMBER;

  p_material_detail_rec     gme_material_details%ROWTYPE;
  x_material_detail_rec     gme_material_details%ROWTYPE;
  p_batchstep_rec           gme_batch_steps%ROWTYPE;
  x_batchstep_rec           gme_batch_steps%ROWTYPE;
  p_batchstep_activity_rec  gme_batch_step_activities%ROWTYPE;
  x_batchstep_activity_rec  gme_batch_step_activities%ROWTYPE;
  p_batchstep_resource_rec  gme_batch_step_resources%ROWTYPE;
  x_batchstep_resource_rec  gme_batch_step_resources%ROWTYPE;
  p_process_parameter_rec   gme_process_parameters%ROWTYPE;
  x_process_parameter_rec   gme_process_parameters%ROWTYPE;
  p_batch_step_items        gme_batch_step_items%ROWTYPE;
  x_batch_step_items        gme_batch_step_items%ROWTYPE;

  --Cursor to fetch batch header from interface table to handle
  CURSOR CUR_BATCH_HEADER(v_group_id NUMBER) IS
  SELECT *
  FROM   gme_batch_header_interface
  WHERE  group_id=v_group_id
  AND    process_status IN (PROCESS_STATUS_RUNNING, PROCESS_STATUS_BV_WARNING)
  ORDER BY int_organization_id, int_batch_id, batch_no, load_type;

  CURSOR cur_get_rsrc_dtl (v_resources VARCHAR2, v_organization_id VARCHAR2)
  IS
  SELECT min_capacity, max_capacity, capacity_constraint, capacity_um
         ,usage_uom, capacity_tolerance
    FROM cr_rsrc_dtl
   WHERE resources = v_resources
     AND organization_id = v_organization_id;

  CURSOR cur_get_rsrc_hdr (v_resources VARCHAR2)
  IS
  SELECT min_capacity, max_capacity, capacity_constraint, capacity_um
         ,std_usage_uom, capacity_tolerance
    FROM cr_rsrc_mst
   WHERE resources = v_resources;

  CURSOR cur_get_step_qty_um(v_batchstep_id NUMBER)
  IS
  SELECT step_qty_um
    FROM gme_batch_steps
   WHERE batchstep_id = v_batchstep_id;

  l_min_capacity            NUMBER;
  l_max_capacity            NUMBER;
  l_capacity_constraint     NUMBER;
  l_capacity_um             VARCHAR2 (3);
  l_usage_uom               VARCHAR2 (3);
  l_capacity_tolerance      NUMBER;

  l_batch_header_ifs        batch_header_tbl;
  l_material_ifs            batch_material_tbl;
  l_step_ifs                batch_step_tbl;
  l_activity_ifs            batch_activity_tbl;
  l_resource_ifs            batch_resource_tbl;
  l_step_item_ifs           batch_step_item_tbl;
  l_parameter_ifs           batch_parameter_tbl;
  p_batchstep_resource_tbl  gme_create_step_pvt.resources_tab;
BEGIN
  IF (g_debug <> -1) THEN
    gme_debug.log_initialize ('process_batches');
  END IF;
  IF g_debug <= gme_debug.g_log_statement THEN
    gme_debug.put_line('enter process_batches, Group ID: ' || p_group_id 
                       || ', Purge Successful Rows: ' || p_purge_flag 
                       || ', Ignore Material Exception: ' || p_ignore_exception);
  END IF;
  IF p_group_id IS NOT NULL THEN
    --First change all pending rows to running, next we only handle all running rows
    UPDATE gme_batch_header_interface
    SET    process_status = PROCESS_STATUS_RUNNING
    WHERE  group_id = p_group_id
    AND    process_status = PROCESS_STATUS_PENDING;
    IF sql%rowcount > 0  THEN
      l_process_header := TRUE;
    ELSE
      l_process_header := FALSE;
      IF g_debug <= gme_debug.g_log_statement THEN
        gme_debug.put_line('No pending row in table gme_batch_header_interface');
      END IF;
    END IF;
    UPDATE gme_batch_dtls_interface
    SET    process_status = PROCESS_STATUS_RUNNING
    WHERE  group_id = p_group_id
    AND    process_status = PROCESS_STATUS_PENDING;
    IF sql%rowcount > 0  THEN
      l_process_detail := TRUE;
    ELSE
      l_process_detail := FALSE;
      IF g_debug <= gme_debug.g_log_statement THEN
        gme_debug.put_line('No pending row in table gme_batch_dtls_interface');
      END IF;
    END IF;

    IF l_process_header = FALSE AND l_process_detail = FALSE THEN
      err_buf := 'No pending rows for the group id: ' || p_group_id;
      IF g_debug <= gme_debug.g_log_statement THEN
        gme_debug.put_line('finished process_batches');
      END IF;
      RETURN;
    END IF;
    g_errorred := FALSE; --initialize errorred flag to false, means no error at startup.
    IF g_debug <= gme_debug.g_log_statement THEN
      gme_debug.put_line('changed all pending rows to runing status');
    END IF;
    --set value for g_bulk_validation_done to Y, then in other package we will not to check validation for columns that
    --verified in bulk validation.
    gme_common_pvt.g_bulk_validation_done := 'Y';

    IF l_process_header THEN
      --First bulk validation for batch header rows
      bulk_validate_batch_header(p_group_id      => p_group_id
                                ,x_message_list  => x_message_list
                                ,x_return_status => x_return_status);
      IF x_return_status <> fnd_api.g_ret_sts_success THEN
        batch_header_error_handler(p_err_message        => x_message_list
                                  ,p_err_message_detail => x_message_detail
                                  ,p_group_id           => p_group_id
                                  ,p_load_type          => NULL
                                  ,p_interface_id       => -1);
        IF g_debug <= gme_debug.g_log_statement THEN
          gme_debug.put_line('bulk validate batch header completed with error: ' || x_message_list);
        END IF;
      END IF;
    END IF;

    IF l_process_header AND x_return_status = fnd_api.g_ret_sts_success THEN
      IF g_debug <= gme_debug.g_log_statement THEN
        gme_debug.put_line('bulk validate batch header completed');
        gme_debug.put_line('begin batch header process');
      END IF;
      IF NVL(p_ignore_exception, 'N') = 'Y' THEN
        l_ignore_exception := fnd_api.g_true;
      ELSE
        l_ignore_exception := fnd_api.g_false;
      END IF;
      --Then handle all batch header rows
      OPEN CUR_BATCH_HEADER(p_group_id);
      LOOP
        FETCH CUR_BATCH_HEADER BULK COLLECT INTO l_batch_header_ifs LIMIT l_limit;
        FOR i IN 1..l_batch_header_ifs.COUNT LOOP
        BEGIN
          x_message_name := NULL;
          IF x_message_detail.COUNT > 0 THEN
           x_message_detail.DELETE;
          END IF;
          construct_batch_header(p_batch_header_int  => l_batch_header_ifs(i)
                                ,x_batch_header_rec  => p_batch_header);
          x_return_status := fnd_api.g_ret_sts_success;
          --Create batch from recipe
          IF l_batch_header_ifs(i).load_type = LOADTYPE_HEADER_RECIPE 
             OR l_batch_header_ifs(i).load_type = LOADTYPE_HEADER_RELEASE THEN
            IF l_batch_header_ifs(i).int_batch_id IS NULL THEN
              CASE l_batch_header_ifs(i).creation_mode
                WHEN CREATION_MODE_RECIPE THEN --recipe
                  l_creation_mode := 'RECIPE';
                WHEN CREATION_MODE_PRODUCT THEN --product
                  l_creation_mode := 'PRODUCT';
                WHEN CREATION_MODE_OUTPUT THEN --output
                  l_creation_mode := 'OUTPUT';
                WHEN CREATION_MODE_INPUT THEN --input
                  l_creation_mode := 'INPUT';
              END CASE;
              --set g_flex_validate_prof to 1 to enable validate flexfields
              gme_common_pvt.g_flex_validate_prof := 1;
              --validate batch header flexfields before creation batch, then disable validate flexfields
              --and ignore flexfields errors from subobjects
              gme_validate_flex_fld_pvt.create_flex_batch_header(p_batch_header           => p_batch_header
                                                                ,x_batch_header           => p_batch_header
                                                                ,x_return_status          => x_return_status);
              IF x_return_status = fnd_api.g_ret_sts_success THEN
                --set g_flex_validate_prof to 0 to disable validate flexfields
                gme_common_pvt.g_flex_validate_prof := 0;
                gme_api_pub.create_batch
                               (p_api_version                       => 2.0,
                                p_init_msg_list                     => fnd_api.g_true,
                                p_commit                            => fnd_api.g_true,
                                x_message_count                     => x_message_count,
                                x_message_list                      => x_message_list,
                                x_return_status                     => x_return_status,
                                p_org_code                          => NULL,
                                p_batch_header_rec                  => p_batch_header,
                                x_batch_header_rec                  => x_batch_header,
                                p_batch_size                        => l_batch_header_ifs(i).batch_size,
                                p_batch_size_uom                    => l_batch_header_ifs(i).batch_size_uom,
                                p_creation_mode                     => l_creation_mode,
                                p_recipe_id                         => l_batch_header_ifs(i).int_recipe_id,
                                p_recipe_no                         => NULL,
                                p_recipe_version                    => NULL,
                                p_product_no                        => NULL,
                                p_item_revision                     => l_batch_header_ifs(i).item_revision,
                                p_product_id                        => l_batch_header_ifs(i).int_inventory_item_id,
                                p_ignore_qty_below_cap              => l_batch_header_ifs(i).ignore_qty_below_cap,
                                p_use_workday_cal                   => l_batch_header_ifs(i).use_workday_cal,
                                p_contiguity_override               => l_batch_header_ifs(i).contiguity_override,
                                p_use_least_cost_validity_rule      => l_batch_header_ifs(i).use_least_cost_vr,
                                x_exception_material_tbl            => x_exception_material_tbl
                               );
                IF x_return_status = gme_common_pvt.g_inv_short_err THEN
                    x_return_status := fnd_api.g_ret_sts_success;
                END IF;
                IF x_exception_material_tbl.COUNT > 0 THEN
                  IF g_debug >= gme_debug.g_log_exception THEN
                    FOR i IN x_exception_material_tbl.FIRST .. x_exception_material_tbl.LAST
                    LOOP
                      IF x_exception_material_tbl.EXISTS (i) THEN
                      gme_debug.put_line
                                (   'Material exception when creating batch for batch_id = ' || x_exception_material_tbl (i).batch_id
                                    || ' material_detail_id = ' || x_exception_material_tbl (i).material_detail_id
                                    || ' transacted_qty = ' || x_exception_material_tbl (i).transacted_qty
                                    || ' exception_qty = ' || x_exception_material_tbl (i).exception_qty
                                );
                      END IF;
                    END LOOP;
                  END IF;
                  x_exception_material_tbl.DELETE;
                END IF;
                IF x_return_status = fnd_api.g_ret_sts_success 
                   AND l_batch_header_ifs(i).load_type = LOADTYPE_HEADER_RELEASE THEN
                  p_batch_header := x_batch_header;
                  UPDATE gme_batch_header_interface 
                  SET    int_batch_id = x_batch_header.batch_id,
                         batch_no = x_batch_header.batch_no
                  WHERE  interface_header_id = l_batch_header_ifs(i).interface_header_id;
                  COMMIT;
                END IF;
              ELSE
                x_message_list := get_error_message;
              END IF;
            END IF;
            IF l_batch_header_ifs(i).load_type = LOADTYPE_HEADER_RELEASE
               AND x_return_status = fnd_api.g_ret_sts_success THEN
              --Check if there are any open periods for releasing batch
              IF NOT gme_common_pvt.check_close_period(p_org_id     => p_batch_header.organization_id
                                                      ,p_trans_date => p_batch_header.actual_start_date) THEN
                x_app_name := 'INV';
                x_message_name := 'INV_NO_OPEN_PERIOD';
                gme_common_pvt.log_message('INV','INV_NO_OPEN_PERIOD');
                x_message_list := get_error_message;
                x_return_status := fnd_api.g_ret_sts_error;
              ELSE
                gme_api_pub.release_batch(p_api_version             => 2.0
                                         ,p_validation_level        => gme_common_pvt.g_max_errors
                                         ,p_init_msg_list           => fnd_api.g_true
                                         ,p_commit                  => fnd_api.g_true
                                         ,x_message_count           => x_message_count
                                         ,x_message_list            => x_message_list
                                         ,x_return_status           => x_return_status
                                         ,p_batch_header_rec        => p_batch_header
                                         ,p_org_code                => NULL
                                         ,p_ignore_exception        => l_ignore_exception
                                         ,p_validate_flexfields     => fnd_api.g_true
                                         ,x_batch_header_rec        => x_batch_header
                                         ,x_exception_material_tbl  => x_exception_material_tbl);
                IF x_exception_material_tbl.COUNT > 0 THEN
                    IF g_debug >= gme_debug.g_log_exception THEN
                      FOR i IN x_exception_material_tbl.FIRST .. x_exception_material_tbl.LAST
                      LOOP
                        IF x_exception_material_tbl.EXISTS (i) THEN
                        gme_debug.put_line
                                  (   'Material exception when releasing batch for batch_id = ' || x_exception_material_tbl (i).batch_id
                                      || ' material_detail_id = ' || x_exception_material_tbl (i).material_detail_id
                                      || ' transacted_qty = ' || x_exception_material_tbl (i).transacted_qty
                                      || ' exception_qty = ' || x_exception_material_tbl (i).exception_qty
                                  );
                        END IF;
                      END LOOP;
                    END IF;
                    x_exception_material_tbl.DELETE;
                END IF;
              END IF;
            END IF;			
          --Create Batch from detail
          ELSIF l_batch_header_ifs(i).load_type = LOADTYPE_HEADER_DETAIL THEN
            NULL;
          --Update batch
          ELSIF l_batch_header_ifs(i).load_type = LOADTYPE_HEADER_UPDATE THEN
            update_batch_header(p_batch_header                   => p_batch_header
                               ,p_validate_flexfield             => fnd_api.g_true
                               ,x_batch_header                   => x_batch_header
                               ,x_message_list                   => x_message_list
                               ,x_message_name                   => x_message_name
                               ,x_app_name                       => x_app_name
                               ,x_message_detail                 => x_message_detail
                               ,x_return_status                  => x_return_status
                               );
          --Reschedule batch
          ELSIF l_batch_header_ifs(i).load_type = LOADTYPE_HEADER_RESCHEDULE THEN
            gme_api_pub.reschedule_batch(p_init_msg_list              => fnd_api.g_true,
                                         p_commit                     => fnd_api.g_true,
                                         p_batch_header_rec           => p_batch_header,
                                         p_use_workday_cal            => l_batch_header_ifs(i).use_workday_cal,
                                         p_contiguity_override        => l_batch_header_ifs(i).contiguity_override,
                                         p_org_code                   => NULL,
                                         x_message_count              => x_message_count,
                                         x_message_list               => x_message_list,
                                         x_return_status              => x_return_status,
                                         x_batch_header_rec           => x_batch_header
                                         );
          --Reroute batch
          ELSIF l_batch_header_ifs(i).load_type = LOADTYPE_HEADER_REROUTE THEN
            gme_api_pub.reroute_batch
                              (p_init_msg_list                  => fnd_api.g_true,
                               p_commit                         => fnd_api.g_true,
                               p_batch_header_rec               => p_batch_header,
                               p_validity_rule_id               => l_batch_header_ifs(i).recipe_validity_rule_id,
                               p_org_code                       => NULL,
                               p_use_workday_cal                => l_batch_header_ifs(i).use_workday_cal,
                               p_contiguity_override            => l_batch_header_ifs(i).contiguity_override,
                               x_message_count                  => x_message_count,
                               x_message_list                   => x_message_list,
                               x_return_status                  => x_return_status,
                               x_batch_header_rec               => x_batch_header
                              );
          END IF;
          IF x_return_status = fnd_api.g_ret_sts_success THEN
            l_sysdate := SYSDATE;
            IF l_batch_header_ifs(i).process_status = PROCESS_STATUS_RUNNING THEN
              l_process_status := PROCESS_STATUS_SUCCEED;
            ELSE
              l_process_status := PROCESS_STATUS_WARNING;
            END IF;
            UPDATE gme_batch_header_interface 
            SET    process_status = l_process_status,
                   int_batch_id = x_batch_header.batch_id,
                   batch_no = x_batch_header.batch_no,
                   request_id = l_request_id,
                   last_updated_by = l_user_id,
                   last_update_date = l_sysdate,
                   last_update_login = l_login_id
            WHERE  interface_header_id = l_batch_header_ifs(i).interface_header_id;
          ELSE
            IF x_message_name IS NULL THEN
              get_error_message_infor(p_message_text         => x_message_list
                                     ,x_app_name             => x_app_name
                                     ,x_message_name         => x_message_name
                                     ,x_message_detail       => x_message_detail);
            END IF;
            batch_header_error_handler(p_err_message         => x_message_list
                                      ,p_err_message_name    => x_message_name
									  ,p_app_name            => x_app_name
									  ,p_err_message_detail  => x_message_detail
                                      ,p_group_id            => p_group_id
                                      ,p_load_type           => l_batch_header_ifs(i).load_type
                                      ,p_interface_id        => l_batch_header_ifs(i).interface_header_id);
          END IF;
          EXCEPTION
          WHEN OTHERS THEN
            batch_header_error_handler(p_err_message         => SQLERRM
                                      ,p_group_id            => p_group_id
                                      ,p_err_message_detail  => x_message_detail
                                      ,p_load_type           => l_batch_header_ifs(i).load_type
                                      ,p_interface_id        => l_batch_header_ifs(i).interface_header_id);
          END;
          COMMIT;
        END LOOP;
        EXIT WHEN CUR_BATCH_HEADER%NOTFOUND;
      END LOOP;
      CLOSE CUR_BATCH_HEADER;
      IF g_debug <= gme_debug.g_log_statement THEN
        gme_debug.put_line('end batch header process');
      END IF;
      p_batch_header := NULL;
    END IF;

    IF l_process_detail THEN
      --Handle batch details, first common bulk validate
      bulk_validate_batch_detail(p_group_id            => p_group_id
                                ,p_object_type         => NULL
                                ,x_message_list        => x_message_list
                                ,x_return_status       => x_return_status);
      IF x_return_status <> fnd_api.g_ret_sts_success THEN
        IF g_debug <= gme_debug.g_log_statement THEN
          gme_debug.put_line('common detail bulk validate finished with error: ' || x_message_list);
        END IF;
      END IF;
      --Then bulk validation for material
      bulk_validate_batch_detail(p_group_id            => p_group_id
                                ,p_object_type         => OBJECTYPE_MATERIAL
                                ,x_message_list        => x_message_list
                                ,x_return_status       => x_return_status);
      IF x_return_status <> fnd_api.g_ret_sts_success THEN
        batch_dtls_error_handler(p_err_message         => x_message_list
                                ,p_err_message_detail  => x_message_detail
                                ,p_group_id            => p_group_id
                                ,p_object_type         => OBJECTYPE_MATERIAL
                                ,p_load_type           => NULL
                                ,p_interface_id        => -1);
        IF g_debug <= gme_debug.g_log_statement THEN
          gme_debug.put_line('bulk validate material finished with error: ' || x_message_list);
        END IF;
      END IF;
    END IF;
    IF l_process_detail AND x_return_status = fnd_api.g_ret_sts_success THEN
      IF g_debug <= gme_debug.g_log_statement THEN
        gme_debug.put_line('bulk validate material finished');
        gme_debug.put_line('begin batch material process');
      END IF;
      --Handle all material with sequence insert, update, delete
      OPEN CUR_BATCH_DTL_MATERIAL(p_group_id);
      LOOP
        FETCH CUR_BATCH_DTL_MATERIAL BULK COLLECT INTO l_material_ifs LIMIT l_limit;
        FOR i IN 1..l_material_ifs.COUNT LOOP
        BEGIN
          x_message_name := NULL;
          IF x_message_detail.COUNT > 0 THEN
            x_message_detail.DELETE;
          END IF;
          p_batch_header.batch_id                         := l_material_ifs(i).int_batch_id;
          p_batch_header.organization_id                  := l_material_ifs(i).int_organization_id;
          construct_material(p_material_int  => l_material_ifs(i)
                            ,x_material_rec  => p_material_detail_rec);
          IF l_material_ifs(i).load_type = LOADTYPE_DETAIL_ADD THEN
            gme_api_pub.insert_material_line(p_init_msg_list                  => fnd_api.g_true,
                                             p_commit                         => fnd_api.g_true,
                                             p_batch_header_rec               => p_batch_header,
                                             p_material_detail_rec            => p_material_detail_rec,
                                             p_validate_flexfields            => fnd_api.g_true,
                                             p_locator_code                   => NULL,
                                             p_org_code                       => NULL,
                                             x_material_detail_rec            => x_material_detail_rec,
                                             x_message_count                  => x_message_count,
                                             x_message_list                   => x_message_list,
                                             x_return_status                  => x_return_status);
          ELSIF l_material_ifs(i).load_type = LOADTYPE_DETAIL_UPDATE THEN
            gme_api_pub.update_material_line(p_init_msg_list                  => fnd_api.g_true,
                                             p_commit                         => fnd_api.g_true,
                                             p_batch_header_rec               => p_batch_header,
                                             p_material_detail_rec            => p_material_detail_rec,
                                             p_validate_flexfields            => fnd_api.g_true,
                                             p_locator_code                   => NULL,
                                             p_org_code                       => NULL,
                                             x_material_detail_rec            => x_material_detail_rec,
                                             x_message_count                  => x_message_count,
                                             x_message_list                   => x_message_list,
                                             x_return_status                  => x_return_status);
          ELSIF l_material_ifs(i).load_type = LOADTYPE_DETAIL_DELETE THEN
            gme_api_pub.delete_material_line(p_init_msg_list                  => fnd_api.g_true,
                                             p_commit                         => fnd_api.g_true,
                                             p_batch_header_rec               => p_batch_header,
                                             p_material_detail_rec            => p_material_detail_rec,
                                             p_org_code                       => NULL,
                                             x_message_count                  => x_message_count,
                                             x_message_list                   => x_message_list,
                                             x_return_status                  => x_return_status);
          END IF;
          
          IF x_return_status = fnd_api.g_ret_sts_success THEN
            l_sysdate := SYSDATE;
            UPDATE gme_batch_dtls_interface 
            SET    process_status = PROCESS_STATUS_SUCCEED,
                   request_id = l_request_id,
                   last_updated_by = l_user_id,
                   last_update_date = l_sysdate,
                   last_update_login = l_login_id
            WHERE  interface_id = l_material_ifs(i).interface_id;
          ELSE
            IF x_message_name IS NULL THEN
              get_error_message_infor(p_message_text      => x_message_list
                                     ,x_app_name          => x_app_name
                                     ,x_message_name      => x_message_name
                                     ,x_message_detail    => x_message_detail);
            END IF;
            batch_dtls_error_handler(p_err_message        => x_message_list
                                    ,p_err_message_name   => x_message_name
                                    ,p_app_name           => x_app_name
                                    ,p_err_message_detail => x_message_detail
                                    ,p_group_id           => p_group_id
                                    ,p_object_type        => OBJECTYPE_MATERIAL
                                    ,p_load_type          => l_material_ifs(i).load_type
                                    ,p_interface_id       => l_material_ifs(i).interface_id);
          END IF;
        EXCEPTION
        WHEN OTHERS THEN
          batch_dtls_error_handler(p_err_message        => SQLERRM
                                  ,p_err_message_detail => x_message_detail
                                  ,p_group_id           => p_group_id
                                  ,p_object_type        => OBJECTYPE_MATERIAL
                                  ,p_load_type          => l_material_ifs(i).load_type
                                  ,p_interface_id       => l_material_ifs(i).interface_id);
        END;
        COMMIT;
        END LOOP;
        EXIT WHEN CUR_BATCH_DTL_MATERIAL%NOTFOUND;
      END LOOP;
      CLOSE CUR_BATCH_DTL_MATERIAL;
      IF g_debug <= gme_debug.g_log_statement THEN
        gme_debug.put_line('end batch material process');
      END IF;
    END IF;

    IF l_process_detail THEN
      --Then bulk validation for step
      bulk_validate_batch_detail(p_group_id           => p_group_id
                                ,p_object_type        => OBJECTYPE_STEP
                                ,x_message_list       => x_message_list
                                ,x_return_status      => x_return_status);
      IF x_return_status <> fnd_api.g_ret_sts_success THEN
        batch_dtls_error_handler(p_err_message        => x_message_list
                                ,p_err_message_detail => x_message_detail
                                ,p_group_id           => p_group_id
                                ,p_object_type        => OBJECTYPE_STEP
                                ,p_load_type          => NULL
                                ,p_interface_id       => -1);
        IF g_debug <= gme_debug.g_log_statement THEN
          gme_debug.put_line('bulk validate step finished with error: ' || x_message_list);
        END IF;
      END IF;
    END IF;

    IF l_process_detail AND x_return_status = fnd_api.g_ret_sts_success THEN
      IF g_debug <= gme_debug.g_log_statement THEN
        gme_debug.put_line('bulk validate step finished');
        gme_debug.put_line('begin batch step process');
      END IF;    
      --Handle all step with sequence insert, update, delete, rechedule
      OPEN CUR_BATCH_DTL_STEP(p_group_id);
      LOOP
        FETCH CUR_BATCH_DTL_STEP BULK COLLECT INTO l_step_ifs LIMIT l_limit;
        FOR i IN 1..l_step_ifs.COUNT LOOP
        BEGIN
          x_message_name := NULL;
          IF x_message_detail.COUNT > 0 THEN
            x_message_detail.DELETE;
          END IF;
          p_batch_header.batch_id                   := l_step_ifs(i).int_batch_id;
          p_batch_header.organization_id            := l_step_ifs(i).int_organization_id;
          construct_step(p_step_int  => l_step_ifs(i)
                        ,x_step_rec  => p_batchstep_rec);
          IF l_step_ifs(i).load_type = LOADTYPE_DETAIL_ADD THEN
            x_return_status := fnd_api.g_ret_sts_success;
            --set g_flex_validate_prof to 1 to enable validate flexfields
            gme_common_pvt.g_flex_validate_prof := 1;
            --validate batch step flexfields before creation batch, then disable validate flexfields
            --and ignore flexfields errors from subobjects
            gme_validate_flex_fld_pvt.create_flex_batch_step(p_batch_step           => p_batchstep_rec
                                                            ,x_batch_step           => p_batchstep_rec
                                                            ,x_return_status        => x_return_status);
            IF x_return_status = fnd_api.g_ret_sts_success THEN
              gme_api_pub.insert_step(p_init_msg_list        => fnd_api.g_true
                                     ,p_commit               => fnd_api.g_true
                                     ,p_org_code             => NULL
                                     ,p_validate_flexfields  => fnd_api.g_false
                                     ,p_oprn_no              => NULL
                                     ,p_oprn_vers            => NULL 
                                     ,p_batch_header_rec     => p_batch_header
                                     ,p_batch_step_rec       => p_batchstep_rec
                                     ,x_batch_step_rec       => x_batchstep_rec
                                     ,x_message_count        => x_message_count
                                     ,x_message_list         => x_message_list
                                     ,x_return_status        => x_return_status
                                     );
            ELSE
              x_message_list := get_error_message;
            END IF;
          ELSIF l_step_ifs(i).load_type = LOADTYPE_DETAIL_UPDATE THEN
            --setup first for no org_id provided to update_step
            gme_common_pvt.g_setup_done := gme_common_pvt.setup(p_org_id => l_step_ifs(i).int_organization_id);
            IF NOT gme_common_pvt.g_setup_done THEN
              x_message_list := get_error_message;
              x_return_status := fnd_api.g_ret_sts_error;
            ELSE
              update_step(p_batch_step           => p_batchstep_rec
                         ,p_validate_flexfield   => fnd_api.g_true
                         ,x_batch_step           => x_batchstep_rec
                         ,x_message_list         => x_message_list
                         ,x_message_name         => x_message_name
                         ,x_app_name             => x_app_name
                         ,x_message_detail       => x_message_detail
                         ,x_return_status        => x_return_status
                         );
            END IF;
          ELSIF l_step_ifs(i).load_type = LOADTYPE_DETAIL_DELETE THEN
            --setup first for no org_code provided to gme_api_pub.delete_step
            gme_common_pvt.g_setup_done := gme_common_pvt.setup(p_org_id => l_step_ifs(i).int_organization_id);
            IF NOT gme_common_pvt.g_setup_done THEN
              x_message_list := get_error_message;
              x_return_status := fnd_api.g_ret_sts_error;
            ELSE
              gme_api_pub.delete_step(p_init_msg_list        => fnd_api.g_true
                                     ,p_commit               => fnd_api.g_true
                                     ,p_org_code             => NULL
                                     ,p_batch_no             => NULL
                                     ,p_batch_step_rec       => p_batchstep_rec
                                     ,x_message_count        => x_message_count
                                     ,x_message_list         => x_message_list
                                     ,x_return_status        => x_return_status
                                     );
            END IF;
          ELSIF l_step_ifs(i).load_type = LOADTYPE_DETAIL_RESHCEDULE THEN
            gme_api_pub.reschedule_step(p_init_msg_list         => fnd_api.g_true
                                       ,p_commit                => fnd_api.g_true
                                       ,p_batch_step_rec        => p_batchstep_rec
                                       ,p_use_workday_cal       => l_step_ifs(i).use_work_day_cal
                                       ,p_org_code              => NULL
                                       ,p_batch_no              => NULL
                                       ,p_batch_type            => NULL
                                       ,p_contiguity_override   => l_step_ifs(i).contiguity_override
                                       ,p_reschedule_preceding  => l_step_ifs(i).reschedule_preceding_steps
                                       ,p_reschedule_succeeding => l_step_ifs(i).reschedule_succeeding_steps
                                       ,x_batch_step_rec        => x_batchstep_rec
                                       ,x_message_count         => x_message_count
                                       ,x_message_list          => x_message_list
                                       ,x_return_status         => x_return_status
                                       );
          END IF;
          
          IF x_return_status = fnd_api.g_ret_sts_success THEN
            l_sysdate := SYSDATE;
            UPDATE gme_batch_dtls_interface 
            SET    process_status = PROCESS_STATUS_SUCCEED,
                   request_id = l_request_id,
                   last_updated_by = l_user_id,
                   last_update_date = l_sysdate,
                   last_update_login = l_login_id
            WHERE  interface_id = l_step_ifs(i).interface_id;
          ELSE
            IF x_message_name IS NULL THEN
              get_error_message_infor(p_message_text      => x_message_list
                                     ,x_app_name          => x_app_name
                                     ,x_message_name      => x_message_name
                                     ,x_message_detail    => x_message_detail);
            END IF;
            batch_dtls_error_handler(p_err_message        => x_message_list
                                    ,p_err_message_name   => x_message_name
                                    ,p_app_name           => x_app_name
                                    ,p_err_message_detail => x_message_detail
                                    ,p_group_id           => p_group_id
                                    ,p_object_type        => OBJECTYPE_STEP
                                    ,p_load_type          => l_step_ifs(i).load_type
                                    ,p_interface_id       => l_step_ifs(i).interface_id);
          END IF;
        EXCEPTION
        WHEN OTHERS THEN
          batch_dtls_error_handler(p_err_message        => SQLERRM
                                  ,p_err_message_detail => x_message_detail
                                  ,p_group_id           => p_group_id
                                  ,p_object_type        => OBJECTYPE_STEP
                                  ,p_load_type          => l_step_ifs(i).load_type
                                  ,p_interface_id       => l_step_ifs(i).interface_id);
        END;
        COMMIT;
        END LOOP;
        EXIT WHEN CUR_BATCH_DTL_STEP%NOTFOUND;
      END LOOP;
      CLOSE CUR_BATCH_DTL_STEP;  
      IF g_debug <= gme_debug.g_log_statement THEN
        gme_debug.put_line('end batch step process');
      END IF;
    END IF;

    IF l_process_detail THEN
      --Then bulk validation for activity
      bulk_validate_batch_detail(p_group_id           => p_group_id
                                ,p_object_type        => OBJECTYPE_ACTIVITY
                                ,x_message_list       => x_message_list
                                ,x_return_status      => x_return_status);
      IF x_return_status <> fnd_api.g_ret_sts_success THEN
        batch_dtls_error_handler(p_err_message        => x_message_list
                                ,p_err_message_detail => x_message_detail
                                ,p_group_id           => p_group_id
                                ,p_object_type        => OBJECTYPE_ACTIVITY
                                ,p_load_type          => NULL
                                ,p_interface_id       => -1);
        IF g_debug <= gme_debug.g_log_statement THEN
          gme_debug.put_line('bulk validate activity finished with error: ' || x_message_list);
        END IF;
      END IF;
    END IF;

    IF l_process_detail AND x_return_status = fnd_api.g_ret_sts_success THEN
      IF g_debug <= gme_debug.g_log_statement THEN
        gme_debug.put_line('bulk validate activity finished');
        gme_debug.put_line('begin batch activity process');
      END IF; 
      --Handle all activity with sequence insert, update, delete
      OPEN CUR_BATCH_DTL_ACTIVITY(p_group_id);
      LOOP
        FETCH CUR_BATCH_DTL_ACTIVITY BULK COLLECT INTO l_activity_ifs LIMIT l_limit;
        FOR i IN 1..l_activity_ifs.COUNT LOOP
        BEGIN
          x_message_name := NULL;
          IF x_message_detail.COUNT > 0 THEN
            x_message_detail.DELETE;
          END IF;
          --setup first for no org_code provided to public api
          gme_common_pvt.g_setup_done := gme_common_pvt.setup(p_org_id => l_activity_ifs(i).int_organization_id);
          IF NOT gme_common_pvt.g_setup_done THEN
            x_message_list := get_error_message;
            x_return_status := fnd_api.g_ret_sts_error;
          ELSE
            IF l_activity_ifs(i).load_type <> LOADTYPE_DETAIL_DELETE THEN
              construct_activity(p_activity_int  => l_activity_ifs(i)
                                ,x_activity_rec  => p_batchstep_activity_rec);
            END IF;
            IF l_activity_ifs(i).load_type = LOADTYPE_DETAIL_ADD THEN
              --Insert a activity must insert its related resources
              OPEN CUR_BATCH_DTL_RESOURCE(p_group_id, p_batchstep_activity_rec.batchstep_id, p_batchstep_activity_rec.activity);
              FETCH CUR_BATCH_DTL_RESOURCE BULK COLLECT INTO l_resource_ifs;
              CLOSE CUR_BATCH_DTL_RESOURCE;
              FOR i IN 1..l_resource_ifs.COUNT LOOP
                construct_resource(p_resource_int  => l_resource_ifs(i)
                                  ,x_resource_rec  => p_batchstep_resource_rec
                                  ,x_message_list  => x_message_list
                                  ,x_return_status => x_return_status);
                OPEN cur_get_rsrc_dtl (p_batchstep_resource_rec.resources
                                      ,p_batchstep_resource_rec.organization_id);
                FETCH cur_get_rsrc_dtl into l_min_capacity, l_max_capacity, l_capacity_constraint,
                      l_capacity_um, l_usage_uom, l_capacity_tolerance;
                IF (cur_get_rsrc_dtl%NOTFOUND) THEN
                  OPEN cur_get_rsrc_hdr (p_batchstep_resource_rec.resources);
                  FETCH cur_get_rsrc_hdr INTO l_min_capacity, l_max_capacity, l_capacity_constraint,
                        l_capacity_um, l_usage_uom, l_capacity_tolerance;
                    CLOSE cur_get_rsrc_dtl;
                    CLOSE cur_get_rsrc_hdr;
                END IF;
                CLOSE cur_get_rsrc_dtl;
                OPEN cur_get_step_qty_um(p_batchstep_resource_rec.batchstep_id);
                FETCH cur_get_step_qty_um INTO p_batchstep_resource_rec.resource_qty_um;
                CLOSE cur_get_step_qty_um;
                p_batchstep_resource_rec.usage_um := l_usage_uom;
                p_batchstep_resource_rec.capacity_um := l_capacity_um;
                p_batchstep_resource_rec.min_capacity := l_min_capacity;
                p_batchstep_resource_rec.max_capacity := l_max_capacity;
                p_batchstep_resource_rec.capacity_tolerance := l_capacity_tolerance;
                p_batchstep_resource_tbl(i) := p_batchstep_resource_rec;
              END LOOP;
              gme_api_pub.insert_batchstep_activity(p_api_version             => 2.0
                                                   ,p_init_msg_list           => fnd_api.g_true
                                                   ,p_commit                  => fnd_api.g_true
                                                   ,p_org_code                => NULL
                                                   ,p_batchstep_activity_rec  => p_batchstep_activity_rec
                                                   ,p_batchstep_resource_tbl  => p_batchstep_resource_tbl
                                                   ,p_validate_flexfield      => fnd_api.g_true
                                                   ,x_batchstep_activity_rec  => x_batchstep_activity_rec
                                                   ,x_message_count           => x_message_count
                                                   ,x_message_list            => x_message_list
                                                   ,x_return_status           => x_return_status
                                                   );
            ELSIF l_activity_ifs(i).load_type = LOADTYPE_DETAIL_UPDATE THEN
              gme_api_pub.update_batchstep_activity(p_api_version             => 2.0
                                                   ,p_init_msg_list           => fnd_api.g_true
                                                   ,p_commit                  => fnd_api.g_true
                                                   ,p_org_code                => NULL
                                                   ,p_validate_flexfield      => fnd_api.g_true
                                                   ,p_batchstep_activity_rec  => p_batchstep_activity_rec
                                                   ,x_batchstep_activity_rec  => x_batchstep_activity_rec
                                                   ,x_message_count           => x_message_count
                                                   ,x_message_list            => x_message_list
                                                   ,x_return_status           => x_return_status
                                                   );
            ELSIF l_activity_ifs(i).load_type = LOADTYPE_DETAIL_DELETE THEN
              gme_api_pub.delete_batchstep_activity(p_api_version             => 2.0
                                                   ,p_init_msg_list           => fnd_api.g_true
                                                   ,p_commit                  => fnd_api.g_true
                                                   ,p_org_code                => NULL
                                                   ,p_batchstep_activity_id   => l_activity_ifs(i).int_batchstep_activity_id
                                                   ,x_message_count           => x_message_count
                                                   ,x_message_list            => x_message_list
                                                   ,x_return_status           => x_return_status
                                                   );
            END IF;
          END IF;
          IF x_return_status = fnd_api.g_ret_sts_success THEN
            l_sysdate := SYSDATE;
            UPDATE gme_batch_dtls_interface 
            SET    process_status = PROCESS_STATUS_SUCCEED,
                   request_id = l_request_id,
                   last_updated_by = l_user_id,
                   last_update_date = l_sysdate,
                   last_update_login = l_login_id
            WHERE  interface_id = l_activity_ifs(i).interface_id;
            --set successful flag for its resources when added an activity
            IF l_activity_ifs(i).load_type = LOADTYPE_DETAIL_ADD THEN
              l_sysdate := SYSDATE;
              UPDATE gme_batch_dtls_interface 
              SET    process_status = PROCESS_STATUS_SUCCEED,
                     request_id = l_request_id,
                     last_updated_by = l_user_id,
                     last_update_date = l_sysdate,
                     last_update_login = l_login_id
              WHERE  int_batch_id = l_activity_ifs(i).int_batch_id
              AND    int_batchstep_id = l_activity_ifs(i).int_batchstep_id
              AND    activity = l_activity_ifs(i).activity
              AND    object_type = OBJECTYPE_RESOURCE
              AND    load_type = LOADTYPE_DETAIL_ADD
              AND    process_status = PROCESS_STATUS_RUNNING;
            END IF;
          ELSE
            IF x_message_name IS NULL THEN
              get_error_message_infor(p_message_text      => x_message_list
                                     ,x_app_name          => x_app_name
                                     ,x_message_name      => x_message_name
                                     ,x_message_detail    => x_message_detail);
            END IF;
            batch_dtls_error_handler(p_err_message        => x_message_list
                                    ,p_err_message_name   => x_message_name
                                    ,p_app_name           => x_app_name
                                    ,p_err_message_detail => x_message_detail
                                    ,p_group_id           => p_group_id
                                    ,p_object_type        => OBJECTYPE_ACTIVITY
                                    ,p_load_type          => l_activity_ifs(i).load_type
                                    ,p_interface_id       => l_activity_ifs(i).interface_id);
            --set error flag for its resources when added an activity
            IF l_activity_ifs(i).load_type = LOADTYPE_DETAIL_ADD THEN
              l_sysdate := SYSDATE;
              UPDATE gme_batch_dtls_interface
              SET    process_status = PROCESS_STATUS_FAILED,
                     request_id = l_request_id,
                     last_updated_by = l_user_id,
                     last_update_date = l_sysdate,
                     last_update_login = l_login_id
              WHERE  int_batch_id = l_activity_ifs(i).int_batch_id
              AND    int_batchstep_id = l_activity_ifs(i).int_batchstep_id
              AND    activity = l_activity_ifs(i).activity
              AND    object_type = OBJECTYPE_RESOURCE
              AND    load_type = LOADTYPE_DETAIL_ADD
              AND    process_status = PROCESS_STATUS_RUNNING;
            END IF;
          END IF;
        EXCEPTION
        WHEN OTHERS THEN
          batch_dtls_error_handler(p_err_message        => SQLERRM
                                  ,p_err_message_detail => x_message_detail
                                  ,p_group_id           => p_group_id
                                  ,p_object_type        => OBJECTYPE_ACTIVITY
                                  ,p_load_type          => l_activity_ifs(i).load_type
                                  ,p_interface_id       => l_activity_ifs(i).interface_id);
          --set error flag for its resources when added an activity
          IF l_activity_ifs(i).load_type = LOADTYPE_DETAIL_ADD THEN
            l_sysdate := SYSDATE;
            UPDATE gme_batch_dtls_interface
            SET    process_status = PROCESS_STATUS_FAILED,
                   request_id = l_request_id,
                   last_updated_by = l_user_id,
                   last_update_date = l_sysdate,
                   last_update_login = l_login_id
            WHERE  int_batch_id = l_activity_ifs(i).int_batch_id
            AND    int_batchstep_id = l_activity_ifs(i).int_batchstep_id
            AND    activity = l_activity_ifs(i).activity
            AND    object_type = OBJECTYPE_RESOURCE
            AND    load_type = LOADTYPE_DETAIL_ADD
            AND    process_status = PROCESS_STATUS_RUNNING;
          END IF;
        END;
        IF p_batchstep_resource_tbl.COUNT > 0 THEN
	      p_batchstep_resource_tbl.DELETE;
        END IF;
        COMMIT;
        END LOOP;
        EXIT WHEN CUR_BATCH_DTL_ACTIVITY%NOTFOUND;
      END LOOP;
      CLOSE CUR_BATCH_DTL_ACTIVITY;
      IF g_debug <= gme_debug.g_log_statement THEN
        gme_debug.put_line('end batch activity process');
      END IF;
    END IF;

    IF l_process_detail THEN
      --Then bulk validation for resource
      bulk_validate_batch_detail(p_group_id           => p_group_id
                                ,p_object_type        => OBJECTYPE_RESOURCE
                                ,x_message_list       => x_message_list
                                ,x_return_status      => x_return_status);
      IF x_return_status <> fnd_api.g_ret_sts_success THEN
        batch_dtls_error_handler(p_err_message        => x_message_list
                                ,p_err_message_detail => x_message_detail
                                ,p_group_id           => p_group_id
                                ,p_object_type        => OBJECTYPE_RESOURCE
                                ,p_load_type          => NULL
                                ,p_interface_id       => -1);
        IF g_debug <= gme_debug.g_log_statement THEN
          gme_debug.put_line('bulk validate resource finished with error: ' || x_message_list);
        END IF;
      END IF;
    END IF;

    IF l_process_detail AND x_return_status = fnd_api.g_ret_sts_success THEN
      IF g_debug <= gme_debug.g_log_statement THEN
        gme_debug.put_line('bulk validate resource finished');
        gme_debug.put_line('begin batch resource process');
      END IF;    
      --Handle all resource with sequence insert, update, delete
      OPEN CUR_BATCH_DTL_RESOURCE(p_group_id, NULL, NULL);
      LOOP
        FETCH CUR_BATCH_DTL_RESOURCE BULK COLLECT INTO l_resource_ifs LIMIT l_limit;
        FOR i IN 1..l_resource_ifs.COUNT LOOP
        BEGIN
          x_message_name := NULL;
          IF x_message_detail.COUNT > 0 THEN
            x_message_detail.DELETE;
          END IF;
          IF l_resource_ifs(i).load_type <> LOADTYPE_DETAIL_DELETE THEN
            construct_resource(p_resource_int  => l_resource_ifs(i)
                              ,x_resource_rec  => p_batchstep_resource_rec
                              ,x_message_list  => x_message_list
                              ,x_return_status => x_return_status);
            IF x_return_status = fnd_api.g_ret_sts_success THEN
              IF l_resource_ifs(i).load_type = LOADTYPE_DETAIL_ADD THEN
                gme_api_pub.insert_batchstep_resource(p_api_version             => 2.0
                                                     ,p_init_msg_list           => fnd_api.g_true
                                                     ,p_commit                  => fnd_api.g_true
                                                     ,p_batchstep_resource_rec  => p_batchstep_resource_rec
                                                     ,p_validate_flexfields     => fnd_api.g_true
                                                     ,x_batchstep_resource_rec  => x_batchstep_resource_rec
                                                     ,x_message_count           => x_message_count
                                                     ,x_message_list            => x_message_list
                                                     ,x_return_status           => x_return_status
                                                     );
              ELSIF l_resource_ifs(i).load_type = LOADTYPE_DETAIL_UPDATE THEN
                gme_api_pub.update_batchstep_resource(p_api_version             => 2.0
                                                     ,p_init_msg_list           => fnd_api.g_true
                                                     ,p_commit                  => fnd_api.g_true
                                                     ,p_org_code                => NULL
                                                     ,p_validate_flexfields     => fnd_api.g_true
                                                     ,p_batchstep_resource_rec  => p_batchstep_resource_rec
                                                     ,x_batchstep_resource_rec  => x_batchstep_resource_rec
                                                     ,x_message_count           => x_message_count
                                                     ,x_message_list            => x_message_list
                                                     ,x_return_status           => x_return_status
                                                     );
              END IF;
            END IF;
          ELSE
            gme_api_pub.delete_batchstep_resource(p_api_version             => 2.0
                                                 ,p_init_msg_list           => fnd_api.g_true
                                                 ,p_commit                  => fnd_api.g_true
                                                 ,p_batchstep_resource_id   => l_resource_ifs(i).int_batchstep_resource_id
                                                 ,x_message_count           => x_message_count
                                                 ,x_message_list            => x_message_list
                                                 ,x_return_status           => x_return_status
                                                 );
          END IF;
          
          IF x_return_status = fnd_api.g_ret_sts_success THEN
            l_sysdate := SYSDATE;
            UPDATE gme_batch_dtls_interface 
            SET    process_status = PROCESS_STATUS_SUCCEED,
                   request_id = l_request_id,
                   last_updated_by = l_user_id,
                   last_update_date = l_sysdate,
                   last_update_login = l_login_id
            WHERE  interface_id = l_resource_ifs(i).interface_id;
          ELSE
            IF x_message_name IS NULL THEN
              get_error_message_infor(p_message_text      => x_message_list
                                     ,x_app_name          => x_app_name
                                     ,x_message_name      => x_message_name
                                     ,x_message_detail    => x_message_detail);
            END IF;
            batch_dtls_error_handler(p_err_message        => x_message_list
                                    ,p_err_message_name   => x_message_name
                                    ,p_app_name           => x_app_name
                                    ,p_err_message_detail => x_message_detail
                                    ,p_group_id           => p_group_id
                                    ,p_object_type        => OBJECTYPE_RESOURCE
                                    ,p_load_type          => l_resource_ifs(i).load_type
                                    ,p_interface_id       => l_resource_ifs(i).interface_id);
          END IF;
        EXCEPTION
        WHEN OTHERS THEN
          batch_dtls_error_handler(p_err_message        => SQLERRM
                                  ,p_group_id           => p_group_id
                                  ,p_err_message_detail => x_message_detail
                                  ,p_object_type        => OBJECTYPE_RESOURCE
                                  ,p_load_type          => l_resource_ifs(i).load_type
                                  ,p_interface_id       => l_resource_ifs(i).interface_id);
        END;
        COMMIT;
        END LOOP;
        EXIT WHEN CUR_BATCH_DTL_RESOURCE%NOTFOUND;
      END LOOP;
      CLOSE CUR_BATCH_DTL_RESOURCE;
      IF g_debug <= gme_debug.g_log_statement THEN
        gme_debug.put_line('end batch resource process');
      END IF;
    END IF;

    IF l_process_detail THEN
      --Then bulk validation for step item association
      bulk_validate_batch_detail(p_group_id           => p_group_id
                                ,p_object_type        => OBJECTYPE_STEPITEM
                                ,x_message_list       => x_message_list
                                ,x_return_status      => x_return_status);
      IF x_return_status <> fnd_api.g_ret_sts_success THEN
        batch_dtls_error_handler(p_err_message        => x_message_list
                                ,p_err_message_detail => x_message_detail
                                ,p_group_id           => p_group_id
                                ,p_object_type        => OBJECTYPE_STEPITEM
                                ,p_load_type          => NULL
                                ,p_interface_id       => -1);
        IF g_debug <= gme_debug.g_log_statement THEN
          gme_debug.put_line('bulk validate step item assocaition finished with error: ' || x_message_list);
        END IF;
      END IF;
    END IF;

    IF l_process_detail AND x_return_status = fnd_api.g_ret_sts_success THEN
      IF g_debug <= gme_debug.g_log_statement THEN
        gme_debug.put_line('bulk validate step item assocaition finished');
        gme_debug.put_line('begin step item assocaition process');
      END IF;    
      --Handle all parameter with sequence insert, update, delete
      OPEN CUR_BATCH_DTL_STEPITEM(p_group_id);
      LOOP
        FETCH CUR_BATCH_DTL_STEPITEM BULK COLLECT INTO l_step_item_ifs LIMIT l_limit;
        FOR i IN 1..l_step_item_ifs.COUNT LOOP
        BEGIN
          x_message_name := NULL;
          IF x_message_detail.COUNT > 0 THEN
            x_message_detail.DELETE;
          END IF;
          --setup organization information
          gme_common_pvt.g_setup_done := gme_common_pvt.setup(p_org_id => l_step_item_ifs(i).int_organization_id);
          IF NOT gme_common_pvt.g_setup_done THEN 
            x_message_list := get_error_message;
            x_return_status := fnd_api.g_ret_sts_error;
          ELSE
            construct_step_item(p_step_item_int  => l_step_item_ifs(i)
                               ,x_step_item_rec  => p_batch_step_items);
            gme_common_pvt.set_timestamp;
            IF l_step_item_ifs(i).load_type = LOADTYPE_DETAIL_ADD THEN
              insert_step_item(p_batch_step_items    => p_batch_step_items
                              ,p_validate_flexfield  => fnd_api.g_true
                              ,x_batch_step_items    => x_batch_step_items
                              ,x_message_list        => x_message_list
                              ,x_message_name        => x_message_name
                              ,x_return_status       => x_return_status);
            ELSIF l_step_item_ifs(i).load_type = LOADTYPE_DETAIL_UPDATE THEN
              update_step_item(p_batch_step_items    => p_batch_step_items
                              ,p_validate_flexfield  => fnd_api.g_true
                              ,x_batch_step_items    => x_batch_step_items
                              ,x_message_list        => x_message_list
                              ,x_message_name        => x_message_name
                              ,x_return_status       => x_return_status);
            ELSIF l_step_item_ifs(i).load_type = LOADTYPE_DETAIL_DELETE THEN
              delete_step_item(p_batch_step_items    => p_batch_step_items
                              ,x_message_list        => x_message_list
                              ,x_message_name        => x_message_name
                              ,x_return_status       => x_return_status);
            END IF;
          END IF;
          IF x_return_status = fnd_api.g_ret_sts_success THEN
            l_sysdate := SYSDATE;
            UPDATE gme_batch_dtls_interface
            SET    process_status = PROCESS_STATUS_SUCCEED,
                   request_id = l_request_id,
                   last_updated_by = l_user_id,
                   last_update_date = l_sysdate,
                   last_update_login = l_login_id
            WHERE  interface_id = l_step_item_ifs(i).interface_id;
          ELSE
            IF x_message_name IS NULL THEN
              get_error_message_infor(p_message_text      => x_message_list
                                     ,x_app_name          => x_app_name
                                     ,x_message_name      => x_message_name
                                     ,x_message_detail    => x_message_detail);
            ELSE
              x_app_name := 'GME';
            END IF;
            batch_dtls_error_handler(p_err_message        => x_message_list
                                    ,p_err_message_name   => x_message_name
                                    ,p_app_name           => x_app_name
                                    ,p_err_message_detail => x_message_detail
                                    ,p_group_id           => p_group_id
                                    ,p_object_type        => OBJECTYPE_STEPITEM
                                    ,p_load_type          => l_step_item_ifs(i).load_type
                                    ,p_interface_id       => l_step_item_ifs(i).interface_id);
          END IF;
        EXCEPTION
        WHEN OTHERS THEN
          batch_dtls_error_handler(p_err_message        => SQLERRM
                                  ,p_group_id           => p_group_id
                                  ,p_err_message_detail => x_message_detail
                                  ,p_object_type        => OBJECTYPE_STEPITEM
                                  ,p_load_type          => l_step_item_ifs(i).load_type
                                  ,p_interface_id       => l_step_item_ifs(i).interface_id);
        END;
        COMMIT;
        END LOOP;
        EXIT WHEN CUR_BATCH_DTL_STEPITEM%NOTFOUND;
      END LOOP;
      CLOSE CUR_BATCH_DTL_STEPITEM;
      IF g_debug <= gme_debug.g_log_statement THEN
        gme_debug.put_line('end step item association process');
      END IF;
    END IF;

    IF l_process_detail THEN
      --Then bulk validation for process parameter
      bulk_validate_batch_detail(p_group_id           => p_group_id
                                ,p_object_type        => OBJECTYPE_PROCESSPARAMETER
                                ,x_message_list       => x_message_list
                                ,x_return_status      => x_return_status);
      IF x_return_status <> fnd_api.g_ret_sts_success THEN
        batch_dtls_error_handler(p_err_message        => x_message_list
                                ,p_err_message_detail => x_message_detail
                                ,p_group_id           => p_group_id
                                ,p_object_type        => OBJECTYPE_PROCESSPARAMETER
                                ,p_load_type          => NULL
                                ,p_interface_id       => -1);
        IF g_debug <= gme_debug.g_log_statement THEN
          gme_debug.put_line('bulk validate process parameter finished with error: ' || x_message_list);
        END IF;
      END IF;
    END IF;

    IF l_process_detail AND x_return_status = fnd_api.g_ret_sts_success THEN
      IF g_debug <= gme_debug.g_log_statement THEN
        gme_debug.put_line('bulk validate process parameter finished');
        gme_debug.put_line('begin process parameter process');
      END IF;    
      --Handle all parameter with sequence insert, update, delete
      OPEN CUR_BATCH_DTL_PARAMETER(p_group_id);
      LOOP
        FETCH CUR_BATCH_DTL_PARAMETER BULK COLLECT INTO l_parameter_ifs LIMIT l_limit;
        FOR i IN 1..l_parameter_ifs.COUNT LOOP
        BEGIN
          x_message_name := NULL;
          IF x_message_detail.COUNT > 0 THEN
            x_message_detail.DELETE;
          END IF;
          --setup organization information
          gme_common_pvt.g_setup_done := gme_common_pvt.setup(p_org_id => l_parameter_ifs(i).int_organization_id);
          IF NOT gme_common_pvt.g_setup_done THEN 
            x_message_list := get_error_message;
	  	  x_return_status := fnd_api.g_ret_sts_error;
          ELSE
            construct_parameter(p_parameter_int  => l_parameter_ifs(i)
                               ,x_parameter_rec  => p_process_parameter_rec);
            IF l_parameter_ifs(i).load_type = LOADTYPE_DETAIL_ADD THEN
              gme_api_pub.insert_process_parameter(p_api_version             => 2.0
                                                  ,p_validation_level        => gme_common_pvt.g_max_errors
                                                  ,p_init_msg_list           => fnd_api.g_true
                                                  ,p_commit                  => fnd_api.g_true
                                                  ,p_org_code                => NULL
                                                  ,p_batch_no                => NULL
                                                  ,p_batchstep_no            => NULL
                                                  ,p_activity                => NULL
                                                  ,p_parameter               => l_parameter_ifs(i).process_parameter
                                                  ,p_process_param_rec       => p_process_parameter_rec
                                                  ,p_validate_flexfields     => fnd_api.g_true
                                                  ,x_process_param_rec       => x_process_parameter_rec
                                                  ,x_message_count           => x_message_count
                                                  ,x_message_list            => x_message_list
                                                  ,x_return_status           => x_return_status
                                                  );
            ELSIF l_parameter_ifs(i).load_type = LOADTYPE_DETAIL_UPDATE THEN
              gme_api_pub.update_process_parameter(p_api_version             => 2.0
                                                  ,p_validation_level        => gme_common_pvt.g_max_errors
                                                  ,p_init_msg_list           => fnd_api.g_true
                                                  ,p_commit                  => fnd_api.g_true
                                                  ,p_org_code                => NULL
                                                  ,p_batch_no                => NULL
                                                  ,p_batchstep_no            => NULL
                                                  ,p_activity                => NULL
                                                  ,p_parameter               => NULL
                                                  ,p_validate_flexfields     => fnd_api.g_true
                                                  ,p_process_param_rec       => p_process_parameter_rec
                                                  ,x_process_param_rec       => x_process_parameter_rec
                                                  ,x_message_count           => x_message_count
                                                  ,x_message_list            => x_message_list
                                                  ,x_return_status           => x_return_status
                                                  );
            ELSIF l_parameter_ifs(i).load_type = LOADTYPE_DETAIL_DELETE THEN
              gme_api_pub.delete_process_parameter(p_api_version             => 2.0
                                                  ,p_validation_level        => gme_common_pvt.g_max_errors
                                                  ,p_init_msg_list           => fnd_api.g_true
                                                  ,p_commit                  => fnd_api.g_true
                                                  ,p_org_code                => NULL
                                                  ,p_batch_no                => NULL
                                                  ,p_batchstep_no            => NULL
                                                  ,p_activity                => NULL
                                                  ,p_parameter               => NULL
                                                  ,p_process_param_rec       => p_process_parameter_rec
                                                  ,x_message_count           => x_message_count
                                                  ,x_message_list            => x_message_list
                                                  ,x_return_status           => x_return_status
                                                  );
            END IF;
          END IF;
          IF x_return_status = fnd_api.g_ret_sts_success THEN
            l_sysdate := SYSDATE;
            UPDATE gme_batch_dtls_interface 
            SET    process_status = PROCESS_STATUS_SUCCEED,
                   request_id = l_request_id,
                   last_updated_by = l_user_id,
                   last_update_date = l_sysdate,
                   last_update_login = l_login_id
            WHERE  interface_id = l_parameter_ifs(i).interface_id;
          ELSE
            IF x_message_name IS NULL THEN
              get_error_message_infor(p_message_text      => x_message_list
                                     ,x_app_name          => x_app_name
                                     ,x_message_name      => x_message_name
                                     ,x_message_detail    => x_message_detail);
            END IF;
            batch_dtls_error_handler(p_err_message        => x_message_list
                                    ,p_err_message_name   => x_message_name
                                    ,p_app_name           => x_app_name
                                    ,p_err_message_detail => x_message_detail
                                    ,p_group_id           => p_group_id
                                    ,p_object_type        => OBJECTYPE_PROCESSPARAMETER
                                    ,p_load_type          => l_parameter_ifs(i).load_type
                                    ,p_interface_id       => l_parameter_ifs(i).interface_id);
          END IF;
        EXCEPTION
        WHEN OTHERS THEN
          batch_dtls_error_handler(p_err_message        => SQLERRM
                                  ,p_err_message_detail => x_message_detail
                                  ,p_group_id           => p_group_id
                                  ,p_object_type        => OBJECTYPE_PROCESSPARAMETER
                                  ,p_load_type          => l_parameter_ifs(i).load_type
                                  ,p_interface_id       => l_parameter_ifs(i).interface_id);
        END;
        COMMIT;
        END LOOP;
        EXIT WHEN CUR_BATCH_DTL_PARAMETER%NOTFOUND;
      END LOOP;
      CLOSE CUR_BATCH_DTL_PARAMETER;
      IF g_debug <= gme_debug.g_log_statement THEN
        gme_debug.put_line('end process parameter process');
      END IF;
    END IF;
	
    --set back for g_bulk_validation_done
    gme_common_pvt.g_bulk_validation_done := 'N';
    
    --Set concurrent request status to WARNING if error occurred.
    IF g_errorred THEN
      err_buf := fnd_message.get_string('GME','GME_INTF_CP_WARN');
      fnd_file.put_line(fnd_file.LOG, err_buf);
      l_errorred := TRUE;
    END IF;

    --Delete all successful rows when p_purge_flag enabled
    IF p_purge_flag = 'Y' THEN
      --Modified by qzeng, bug 17452045, delete error messages first, then delete interface table
      --Begin of bug 17452045
      DELETE FROM GME_INTF_ERRORS e
      WHERE EXISTS (SELECT 1
                    FROM   GME_BATCH_HEADER_INTERFACE h
                    WHERE  h.interface_header_id = e.interface_id
                    AND    h.group_id = p_group_id
                    AND    h.process_status = PROCESS_STATUS_SUCCEED)
      AND source_table_name = 'GME_BATCH_HEADER_INTERFACE';

      DELETE FROM GME_INTF_ERRORS e
      WHERE EXISTS (SELECT 1
                    FROM   GME_BATCH_DTLS_INTERFACE d
                    WHERE  d.interface_id = e.interface_id
                    AND    d.group_id = p_group_id
                    AND    d.process_status = PROCESS_STATUS_SUCCEED)
      AND source_table_name = 'GME_BATCH_DTLS_INTERFACE';

      DELETE FROM GME_BATCH_HEADER_INTERFACE
      WHERE  group_id = p_group_id
      AND    process_status = PROCESS_STATUS_SUCCEED;

      DELETE FROM GME_BATCH_DTLS_INTERFACE
      WHERE  group_id = p_group_id
      AND    process_status = PROCESS_STATUS_SUCCEED;
      --End of bug 17452045
    END IF;
    IF l_errorred THEN
      ret_code := '1';
      l_return := FND_CONCURRENT.SET_COMPLETION_STATUS('WARNING',err_buf);
    END IF;
    COMMIT;
  END IF;
  IF g_debug <= gme_debug.g_log_statement THEN
    gme_debug.put_line('finished process_batches');
  END IF;
EXCEPTION
  WHEN NO_DATA_FOUND THEN
    gme_common_pvt.g_bulk_validation_done := 'N';
    IF g_debug >= gme_debug.g_log_statement THEN
      gme_debug.put_line('Error occurred in package gme_batch_open_interface, procedure process_batches: NO_DATA_FOUND');
    END IF;
    err_buf := 'The interface process failed with sql error: NO_DATA_FOUND';
    ret_code := '1';
    l_return := FND_CONCURRENT.SET_COMPLETION_STATUS('ERROR',err_buf);
  WHEN OTHERS THEN
    gme_common_pvt.g_bulk_validation_done := 'N';
    err_buf := 'The interface process failed with sql error: ' || SQLERRM;
    ret_code := '1';
    l_return := FND_CONCURRENT.SET_COMPLETION_STATUS('ERROR',err_buf);
    IF g_debug <= gme_debug.g_log_statement THEN
      gme_debug.put_line('Error occurred in package gme_batch_open_interface, procedure process_batches: ' || SQLERRM);
    END IF;
END process_batches;



procedure log(message in varchar2) is
begin
      fnd_file.put_line(fnd_file.LOG, message);
end log;


/*
  Description:
  
  This private procedure is used to purge interface table, called by purge_interface_tables.
  note: after purge interface table, need to purge table GME_INTF_ERRORS accordingly.
  
  parameter specification: 
  p_purge_option = 0 , purge all rows
  p_purge_option = 1 , purge successful rows
  p_purge_option = 2 , purge pending rows
  p_purge_option = 3 , purge warning rows
  p_purge_option = 4 , purge error table  
*/ 
PROCEDURE purge_table(p_group_id      IN  NUMBER,
                      p_table_name    IN  VARCHAR2,
                      p_purge_option  IN  NUMBER DEFAULT 1,
                      x_return_status OUT NOCOPY VARCHAR2,
					            x_message_list  OUT NOCOPY VARCHAR2) IS
  l_sql                     VARCHAR2(2000);
  l_sql_for_error_tbl      VARCHAR2(2000);
  l_schema                  VARCHAR2(30);
  l_status                  VARCHAR2(1);
  l_industry                VARCHAR2(1);
BEGIN 
  log('enter procedure purge_table, p_table_name: ' || p_table_name || ', p_purge_option: ' || p_purge_option);
    
  if p_purge_option = PURGE_OPTION_ALL_ROWS then
     
      if p_group_id is null then  --if group_id is null , then truncate table when purge_option is all.
           IF FND_INSTALLATION.GET_APP_INFO('GME', l_status, l_industry, l_schema) THEN
                IF l_schema IS NOT NULL THEN
                  l_sql := 'TRUNCATE TABLE ' || l_schema || '.' || p_table_name;  
                  l_sql_for_error_tbl := 'DELETE FROM GME_INTF_ERRORS WHERE source_table_name ='''||p_table_name||'''';
                END IF;
           ELSE
              x_message_list := SQLERRM;
              x_return_status := fnd_api.g_ret_sts_error;
              log('Error when calling FND_INSTALLATION.GET_APP_INFO.');
              RETURN;
           END IF; 
      else --if group_id is not null , then delete table where group_id
          l_sql := 'DELETE FROM ' || p_table_name || ' WHERE group_id = ' || p_group_id;  
          l_sql_for_error_tbl := 'DELETE FROM GME_INTF_ERRORS WHERE source_table_name ='''||p_table_name||''' and group_id='||p_group_id;
      end if;   
  elsif p_purge_option = PURGE_OPTION_SUCCESSFUL_ROWS then
      if p_group_id is null then  
          l_sql := 'DELETE FROM ' || p_table_name || ' where process_status='||PROCESS_STATUS_SUCCEED;
      else
          l_sql := 'DELETE FROM ' || p_table_name || ' where process_status='||PROCESS_STATUS_SUCCEED||' and group_id='||p_group_id;   
      end if;   
  elsif p_purge_option = PURGE_OPTION_PENDING_ROWS then
      if p_group_id is null then  
          l_sql := 'DELETE FROM ' || p_table_name || ' where process_status='||PROCESS_STATUS_PENDING; 
      else
          l_sql := 'DELETE FROM ' || p_table_name || ' where process_status='||PROCESS_STATUS_PENDING||' and group_id='||p_group_id;   
      end if;           
  elsif p_purge_option = PURGE_OPTION_WARNING_ROWS then
      if p_group_id is null then  
          l_sql := 'DELETE FROM ' || p_table_name || ' where process_status='||PROCESS_STATUS_WARNING;
          l_sql_for_error_tbl := 'DELETE FROM GME_INTF_ERRORS WHERE source_table_name ='''||p_table_name||''' and message_type=''W'' ';
      else
          l_sql := 'DELETE FROM ' || p_table_name || ' where process_status='||PROCESS_STATUS_WARNING||' and group_id='||p_group_id;   
          l_sql_for_error_tbl := 'DELETE FROM GME_INTF_ERRORS WHERE source_table_name ='''||p_table_name||''' and group_id='||p_group_id ||' and message_type=''W'' ';
      end if; 
  elsif p_purge_option = PURGE_OPTION_ERROR_ROWS then
      if p_group_id is null then  
          l_sql := 'DELETE FROM ' || p_table_name || ' where process_status='||PROCESS_STATUS_FAILED;
          l_sql_for_error_tbl := 'DELETE FROM GME_INTF_ERRORS WHERE source_table_name ='''||p_table_name||''' and message_type=''E'' ';
      else
          l_sql := 'DELETE FROM ' || p_table_name || ' where process_status='||PROCESS_STATUS_FAILED||' and group_id='||p_group_id;   
          l_sql_for_error_tbl := 'DELETE FROM GME_INTF_ERRORS WHERE source_table_name ='''||p_table_name||''' and group_id='||p_group_id ||' and message_type=''E'' ';
      end if; 
  end if;
  
  log('purge interface sql is:'||l_sql);
  log('purge error table is:'||l_sql_for_error_tbl); 
   --purge interface table
   EXECUTE IMMEDIATE l_sql;
   --purge error table GME_INTF_ERRORS
   if l_sql_for_error_tbl is not null then
      EXECUTE IMMEDIATE l_sql_for_error_tbl;
   end if;
    
  x_return_status := fnd_api.g_ret_sts_success; 
  log('end of procedure purge_table'); 
  commit; 
EXCEPTION
  WHEN OTHERS THEN 
    log('error occurred in pakcage gme_batch_open_interface.purge_table: ' || SQLERRM);
	  x_message_list := SQLERRM;
    x_return_status := fnd_api.g_ret_sts_error;
END purge_table;

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
                                 p_purge_option    IN NUMBER DEFAULT 0) IS
  l_table_name     VARCHAR2(40);
  x_return_status  VARCHAR2(1);
  x_message_list   VARCHAR2(2000);
  l_return         BOOLEAN;

begin 
  log('start purge concurrent program, concurrent program code is:' || p_concurrent_prg_code|| ', Purge Option: ' || p_purge_option);
  IF p_concurrent_prg_code = ALL_TABLE_NAME_CODE THEN
    purge_table(p_group_id, 'GME_BATCH_HEADER_INTERFACE', p_purge_option, x_return_status, x_message_list);
    purge_table(p_group_id, 'GME_BATCH_DTLS_INTERFACE', p_purge_option, x_return_status, x_message_list);
    purge_table(p_group_id, 'GME_BATCH_ACTIONS_INTERFACE', p_purge_option, x_return_status, x_message_list);
    purge_table(p_group_id, 'GME_RESOURCE_TXNS_INTERFACE', p_purge_option, x_return_status, x_message_list); 
  ELSIF p_concurrent_prg_code = GME_BTCH_INTF_CODE THEN
    purge_table(p_group_id, 'GME_BATCH_HEADER_INTERFACE', p_purge_option, x_return_status, x_message_list);
    purge_table(p_group_id, 'GME_BATCH_DTLS_INTERFACE', p_purge_option, x_return_status, x_message_list);

  ELSIF p_concurrent_prg_code = GME_ACTIONS_INTF_CODE  THEN 
    purge_table(p_group_id, 'GME_BATCH_ACTIONS_INTERFACE', p_purge_option, x_return_status, x_message_list);

  ELSIF  p_concurrent_prg_code = GME_RESOURCE_TXNS_INTF_CODE  THEN 
    purge_table(p_group_id, 'GME_RESOURCE_TXNS_INTERFACE', p_purge_option, x_return_status, x_message_list);

  END IF; 
  log('end of purge concurrent program.'); 
  IF x_return_status <> fnd_api.g_ret_sts_success THEN
    ret_code := '1';
    err_buf := 'The interface process failed with error: ' || x_message_list;
    log(err_buf);
    l_return := FND_CONCURRENT.SET_COMPLETION_STATUS('ERROR',err_buf);
  END IF; 
  
end purge_interface_tables; 		  

END GME_BATCH_OPEN_INTERFACE;
/
--show errors;
COMMIT ;
EXIT;
