/* +======================================================================+ */
/* |    Copyright (c) 2005, 2015 Oracle and/or its affiliates.           | */
/* |                         All rights reserved.                         | */
/* |                           Version 12.0.0                             | */
/* +======================================================================+ */
REM dbdrv: sql ~PROD ~PATH ~FILE none none none package &phase=plb \
REM dbdrv: checkfile(120.0.12010000.25=120.2.12020000.23)(115.2=120.1):~PROD:~PATH:~FILE     

CREATE OR REPLACE PACKAGE BODY GME_RESOURCE_TXN_OPEN_INTF AS
/* $Header: GMERTOIB.pls 120.2.12020000.23 2015/05/20 08:40:02 shalchen noship $ */
      g_pkg_name CONSTANT VARCHAR2(30) := 'GME_RESOURCE_TXN_OPEN_INTF';
      g_debug VARCHAR2(5) := NVL(fnd_profile.VALUE('AFLOG_LEVEL'), -1);

      g_validation_has_error BOOLEAN:= FALSE;
      g_create_rs_txn_has_error BOOLEAN := FALSE;
      /*
         this procedure is used to log message for debug  purpose.
      */
      procedure log(message in varchar2) is
      begin
            fnd_file.put_line(fnd_file.LOG, message);
      end log;

      /*
         this procedure is used to log message .
      */
      procedure debug(message in varchar2) is
      begin
            if g_debug <= gme_debug.g_log_procedure then
                  gme_debug.put_line(message);
            end if;
      end debug;

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
      FUNCTION get_error_message(p_encoded VARCHAR2 DEFAULT 'F') RETURN VARCHAR2 IS
            l_message VARCHAR2(2000);
      BEGIN
            l_message := fnd_msg_pub.get(p_msg_index => fnd_msg_pub.g_last, p_encoded => p_encoded);
            RETURN l_message;
      END get_error_message;


/*
    decription:
    this procedure is used to insert error message into
    table GME_INTF_ERRORS with param and token. 

*/
PROCEDURE process_param_token(p_err_message        VARCHAR2
                                    ,p_err_message_name   VARCHAR2 DEFAULT NULL
                                    ,p_app_name           VARCHAR2 DEFAULT NULL
                                    ,p_err_message_detail GME_BATCH_OPEN_INTERFACE.T_MESSAGE_TABLE
                                    ,p_group_id           NUMBER
                                    ,p_interface_id       NUMBER
                                    ,p_column_name        VARCHAR2 DEFAULT NULL
                                    )
IS
  returnCode            NUMBER;
 
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
  returnCode := GME_BATCH_OPEN_INTERFACE.gme_log_interface_err
               (P_GROUP_ID            => p_group_id, 
                P_object_type         => to_char(GME_BATCH_OPEN_INTERFACE.OBJECTYPE_RESOURCETXN),
                P_INTERFACE_ID        => p_interface_id,
                P_COLUMN_NAME         => p_column_name,
                P_MESSAGE_TYPE        => GME_BATCH_OPEN_INTERFACE.MESSAGE_TYPE_ERROR,
                P_MESSAGE_NAME        => p_err_message_name,
                P_MESSAGE_TEXT        => p_err_message,
                P_APP_NAME            => p_app_name,
                P_SOURCE_TABLE_NAME   => 'GME_RESOURCE_TXNS_INTERFACE',
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
   
  
EXCEPTION
  WHEN OTHERS THEN 
      log('error occurred at package GME_RESOURCE_TXN_OPEN_INTF, procedure process_param_token: ' || SQLERRM); 
END process_param_token;




      /*================================================================================
          Procedure
            validation_error_process
          Description
            This procedure is used to log error message into gme_intf_errors table
          
      ================================================================================*/
      PROCEDURE validation_error_process(p_request_id     IN NUMBER,
                                         p_group_id       IN NUMBER,
                                         p_message_name   IN VARCHAR2,
                                         p_column_name    IN VARCHAR2 DEFAULT NULL,
                                         p_message_type   IN VARCHAR2 DEFAULT GME_BATCH_OPEN_INTERFACE.MESSAGE_TYPE_ERROR,
                                         p_message_text   IN VARCHAR2 DEFAULT NULL,
                                         p_param1         IN VARCHAR2 DEFAULT NULL,
                                         p_param2         IN VARCHAR2 DEFAULT NULL,
                                         p_param3         IN VARCHAR2 DEFAULT NULL,
                                         p_param4         IN VARCHAR2 DEFAULT NULL,
                                         p_param5         IN VARCHAR2 DEFAULT NULL,
                                         p_param6         IN VARCHAR2 DEFAULT NULL,
                                         x_return_message OUT NOCOPY VARCHAR2,
                                         x_return_status  OUT NOCOPY VARCHAR2) IS
            -- PRAGMA AUTONOMOUS_TRANSACTION ;
            
           
            l_table_name CONSTANT VARCHAR2(30) := 'GME_RESOURCE_TXNS_INTERFACE';
            l_login   NUMBER := NVL(FND_GLOBAL.login_id, -1);
            l_userid  NUMBER := NVL(FND_GLOBAL.user_id, -1);
            l_conc_status BOOLEAN;
            l_app_short_name varchar2(3)  := fnd_global.application_short_name;
      BEGIN
            g_validation_has_error := TRUE;
            x_return_status := fnd_api.g_ret_sts_success;
      
            INSERT INTO gme_intf_errors
                  (GROUP_ID,
                   object_type,                  
                   interface_id,
                   column_name,
                   message_name,
                   MESSAGE_TYPE,
                   MESSAGE_TEXT,
                   source_table_name,
                   request_id,
                   last_updated_by,
                   creation_date,
                   created_by,
                   last_update_login,
                   last_update_date,
                   MSG_APPL_SNAME)
                  SELECT GROUP_ID, GME_BATCH_OPEN_INTERFACE.OBJECTYPE_RESOURCETXN,interface_id, p_column_name, p_message_name, p_message_type, p_message_text, l_table_name, p_request_id, l_userid, SYSDATE, l_userid, l_login, SYSDATE,l_app_short_name
                    FROM GME_RESOURCE_TXNS_INTERFACE grti
                   WHERE grti.process_status = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_BV_INVALID
                     AND (grti.GROUP_ID = p_group_id OR (p_group_id IS NULL AND grti.request_id = p_request_id));
         
         

           
            UPDATE GME_RESOURCE_TXNS_INTERFACE
               SET process_status    = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_FAILED,
                   last_updated_by   = l_userid,
                   creation_date     = SYSDATE,
                   created_by        = l_userid,
                   last_update_login = l_login,
                   last_update_date  = SYSDATE,
                   request_id = p_request_id
             WHERE (GROUP_ID = p_group_id OR (p_group_id IS NULL AND request_id = p_request_id))
               AND process_status = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_BV_INVALID;
               
               
            COMMIT;
      EXCEPTION
            WHEN OTHERS THEN
                  x_return_status  := fnd_api.g_ret_sts_error;
                  x_return_message := 'Error occurs while logging error message:' || SQLCODE || ',error message:' || SQLERRM;
            
      END validation_error_process;

      /*================================================================================
          Procedure
            resource_txns_bulk_validation
          Description
            This procedure is used to validate resource transactions record, check
            whether it's valid or not. If not, then log error message for error record.
            
            Shaliu Chen     05-MAY-2015  ER 20938455
              Modify for Batch On Hold enhancement,add validation to raise an error 
              if batch is on hold             
            
      ================================================================================*/
      PROCEDURE resource_txns_bulk_validation(p_group_id IN NUMBER, p_request_id IN NUMBER, x_return_message OUT NOCOPY VARCHAR2, x_return_status OUT NOCOPY VARCHAR2) IS
            l_request_id NUMBER := FND_GLOBAL.CONC_REQUEST_ID;
            l_conc_status BOOLEAN;
      BEGIN
            x_return_status := fnd_api.g_ret_sts_success;
      
            /*Validate if batchstep_resource_id is valid*/
            UPDATE GME_RESOURCE_TXNS_INTERFACE intf
               SET int_batchstep_resource_id = (SELECT MIN(res.batchstep_resource_id)
                                                  FROM gme_batch_header gbh, gme_batch_steps step, gme_batch_step_activities act, gme_batch_step_resources res
                                                 WHERE intf.int_organization_id = gbh.organization_id
                                                   AND intf.batch_no = gbh.batch_no
                                                   AND gbh.batch_type = 0
                                                   AND gbh.batch_id = step.batch_id
                                                   AND intf.batch_step_no = step.batchstep_no
                                                   AND step.batchstep_id = act.batchstep_id
                                                   AND act.activity = intf.activity
                                                   AND act.batch_id = gbh.batch_id
                                                   AND act.batchstep_activity_id = res.batchstep_activity_id
                                                   AND res.resources = intf.resources
                                                 GROUP BY RES.BATCHSTEP_ID, res.BATCH_ID)
             WHERE intf.process_status = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_RUNNING
               AND intf.batchstep_resource_id IS NULL
               AND intf.GROUP_ID = p_group_id;
      
            UPDATE GME_RESOURCE_TXNS_INTERFACE intf
               SET process_status = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_BV_INVALID
             WHERE int_batchstep_resource_id IS NULL
               AND intf.process_status = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_RUNNING
               AND intf.GROUP_ID = p_group_id;
      
            IF SQL%ROWCOUNT > 0 THEN
                  validation_error_process(p_request_id     => l_request_id,
                                           p_group_id       => p_group_id,
                                           p_message_name   => 'GME_STEP_RSCID_NOTFOUND',
                                           p_column_name    => 'INT_BATCHSTEP_RESOURCE_ID',
                                           p_message_type   => GME_BATCH_OPEN_INTERFACE.MESSAGE_TYPE_ERROR,
                                           p_message_text   => fnd_message.get_string('GME', 'GME_STEP_RSCID_NOTFOUND'),
                                           x_return_message => x_return_message,
                                           x_return_status  => x_return_status);
            
                  IF x_return_status <> fnd_api.g_ret_sts_success THEN
                        log(x_return_message);
                        return;
                  END IF;
            END IF;
            
            /* ER 20938455 05-MAY-2015 Shaliu Chen
               Add validation that resource txn import is not allowed if batch is on hold
            */            
            UPDATE GME_RESOURCE_TXNS_INTERFACE intf
               SET process_status = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_BV_INVALID
             WHERE EXISTS (SELECT 1
                      FROM gme_batch_header h
                     WHERE intf.int_batch_id = h.batch_id
                       AND gme_common_pvt.validate_resource_txn_onhold(p_batch_id   => h.batch_id,
                                                                       p_trans_date => intf.trans_date,
                                                                       p_start_date => intf.start_date,
                                                                       p_end_date   => intf.end_date) <> 'R')                       
               AND intf.process_status = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_RUNNING
               AND intf.GROUP_ID = p_group_id;  
               
            IF SQL%ROWCOUNT > 0 THEN
                  validation_error_process(p_request_id     => l_request_id,
                                           p_group_id       => p_group_id,
                                           p_message_name   => 'GME_ONHOLD_TRANS_DATE',
                                           p_column_name    => NULL,
                                           p_message_type   => GME_BATCH_OPEN_INTERFACE.MESSAGE_TYPE_ERROR,
                                           p_message_text   => fnd_message.get_string('GME', 'GME_ONHOLD_TRANS_DATE'),
                                           x_return_message => x_return_message,
                                           x_return_status  => x_return_status);
            
                  IF x_return_status <> fnd_api.g_ret_sts_success THEN
                        log(x_return_message);
                        return;
                  END IF;
            END IF;                       
      
            --note:after above steps, all int_batchstep_resource_id is available.
            --note:after above steps, all int_batch_id is available
      
            --validate batch_type, batch_type=10 fpo is not allowed.
            UPDATE GME_RESOURCE_TXNS_INTERFACE intf
               SET process_status = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_BV_INVALID
             WHERE EXISTS (SELECT 1
                      FROM gme_batch_header h
                     WHERE intf.int_batch_id = h.batch_id
                       AND h.batch_type = 10)
               AND intf.process_status = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_RUNNING
               AND intf.GROUP_ID = p_group_id;
      
            IF SQL%ROWCOUNT > 0 THEN
                  validation_error_process(p_request_id     => l_request_id,
                                           p_group_id       => p_group_id,
                                           p_message_name   => 'GME_RTXN_FOR_FPO_NT_ALWD',
                                           p_column_name    => NULL,
                                           p_message_type   => GME_BATCH_OPEN_INTERFACE.MESSAGE_TYPE_ERROR,
                                           p_message_text   => fnd_message.get_string('GME', 'GME_RTXN_FOR_FPO_NT_ALWD'),
                                           x_return_message => x_return_message,
                                           x_return_status  => x_return_status);
                  IF x_return_status <> fnd_api.g_ret_sts_success THEN
                        return;
                  END IF;
            END IF;
      
            --validate update_inventory_ind
            /* we cannot insert txns if the batch does not support txns*/
            UPDATE GME_RESOURCE_TXNS_INTERFACE intf
               SET process_status = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_BV_INVALID
             WHERE EXISTS (SELECT 1
                      FROM gme_batch_header h
                     WHERE intf.int_batch_id = h.batch_id
                       AND h.update_inventory_ind = 'N')
               AND intf.process_status = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_RUNNING
               AND intf.GROUP_ID = p_group_id;
      
            IF SQL%ROWCOUNT > 0 THEN
                  validation_error_process(p_request_id     => l_request_id,
                                           p_group_id       => p_group_id,
                                           p_message_name   => 'GME_RTXN_FOR_UPDINV_NT_ALWD',
                                           p_column_name    => NULL,
                                           p_message_type   => GME_BATCH_OPEN_INTERFACE.MESSAGE_TYPE_ERROR,
                                           p_message_text   => fnd_message.get_string('GME', 'GME_RTXN_FOR_UPDINV_NT_ALWD'),
                                           x_return_message => x_return_message,
                                           x_return_status  => x_return_status);
            
                  IF x_return_status <> fnd_api.g_ret_sts_success THEN
                        return;
                  END IF;
            END IF;
      
            /*Validate batch step status, only WIP/COMPLETE batch step is supported to add resource txn  */
            /*
              BUG 19792828 11-Oct-2014 shalchen
              Add condition to validate whether batch has been terminated.if batch is terminate,
              resource txn import is not allowed.
            */
            UPDATE GME_RESOURCE_TXNS_INTERFACE intf
               SET process_status = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_BV_INVALID
             WHERE EXISTS (SELECT 1
                      FROM gme_batch_steps step, gme_batch_step_resources res,gme_batch_header gbh
                     WHERE intf.int_batchstep_resource_id = res.batchstep_resource_id
                       AND res.batchstep_id = step.batchstep_id
                       AND step.batch_id = gbh.batch_id
                       AND (step.step_status NOT IN (2, 3)
                       OR NVL(gbh.terminated_ind,0) = 1))
               AND intf.process_status = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_RUNNING
               AND intf.GROUP_ID = p_group_id;
      
            IF SQL%ROWCOUNT > 0 THEN
                  validation_error_process(p_request_id     => l_request_id,
                                           p_group_id       => p_group_id,
                                           p_message_name   => 'PC_STEP_STATUS_ERR',
                                           p_column_name    => NULL,
                                           p_message_type   => GME_BATCH_OPEN_INTERFACE.MESSAGE_TYPE_ERROR,
                                           p_message_text   => fnd_message.get_string('GME', 'PC_STEP_STATUS_ERR'),
                                           x_return_message => x_return_message,
                                           x_return_status  => x_return_status);
            
                  IF x_return_status <> fnd_api.g_ret_sts_success THEN
                        log(x_return_message);
                        return;
                  END IF;
            END IF;
      
            /*Cannot edit resource transaction if step status is released and
            Automatic Step Quantity Calculation is selected*/
            /*                                    
            BEGIN ER 19161894                    
            Shaliu Chen 11-SEP-2014               
            Modified to ignore resource txn created for osp step. 
            */               
            UPDATE GME_RESOURCE_TXNS_INTERFACE intf
               SET process_status = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_BV_INVALID
             WHERE EXISTS (SELECT 1
                      FROM gme_batch_steps step, gme_batch_step_resources res, gme_batch_header head
                     WHERE intf.int_batchstep_resource_id = res.batchstep_resource_id
                       AND res.batchstep_id = step.batchstep_id
                       AND intf.int_batch_id = head.batch_id
                       AND step.step_status = 2
                       AND head.automatic_step_calculation = 1)
               AND intf.process_status = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_RUNNING
               AND intf.GROUP_ID = p_group_id
               AND intf.rcv_transaction_id IS NULL;
      
            IF SQL%ROWCOUNT > 0 THEN
                  validation_error_process(p_request_id     => l_request_id,
                                           p_group_id       => p_group_id,
                                           p_message_name   => 'GME_INV_STEP_STATUS_ASQC',
                                           p_column_name    => NULL,
                                           p_message_type   => GME_BATCH_OPEN_INTERFACE.MESSAGE_TYPE_ERROR,
                                           p_message_text   => fnd_message.get_string('GME', 'GME_INV_STEP_STATUS_ASQC'),
                                           x_return_message => x_return_message,
                                           x_return_status  => x_return_status);
            
                  IF x_return_status <> fnd_api.g_ret_sts_success THEN
                        log(x_return_message);
                        return;
                  END IF;
            END IF;
      
            /*Validate reason_id.  Reason does not exist in mtl_transaction_reasons*/
            UPDATE GME_RESOURCE_TXNS_INTERFACE intf
               SET process_status = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_BV_INVALID
             WHERE NOT EXISTS (SELECT 1
                      FROM mtl_transaction_reasons re
                     WHERE re.reason_id = intf.reason_id
                       AND NVL(re.disable_date, SYSDATE + 1) > SYSDATE)
               AND intf.process_status = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_RUNNING
               AND reason_id IS NOT NULL
               AND intf.GROUP_ID = p_group_id;
      
            IF SQL%ROWCOUNT > 0 THEN
                  validation_error_process(p_request_id     => l_request_id,
                                           p_group_id       => p_group_id,
                                           p_message_name   => 'INV_LOTC_REASONID_INVALID',
                                           p_column_name    => 'REASON_ID',
                                           p_message_type   => GME_BATCH_OPEN_INTERFACE.MESSAGE_TYPE_ERROR,
                                           p_message_text   => fnd_message.get_string('INV', 'INV_LOTC_REASONID_INVALID'),
                                           x_return_message => x_return_message,
                                           x_return_status  => x_return_status);
            
                  IF x_return_status <> fnd_api.g_ret_sts_success THEN
                        log(x_return_message);
                        return;
                  END IF;
            END IF;
      
            /*Validate reason_name.  Reason does not exist in mtl_transaction_reasons*/
            UPDATE GME_RESOURCE_TXNS_INTERFACE intf
               SET process_status = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_BV_INVALID
             WHERE NOT EXISTS (SELECT 1
                      FROM mtl_transaction_reasons re
                     WHERE re.reason_name = intf.reason_name
                       AND NVL(re.disable_date, SYSDATE + 1) > SYSDATE)
               AND intf.process_status = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_RUNNING
               AND reason_id IS NULL
               AND reason_name IS NOT NULL
               AND intf.GROUP_ID = p_group_id;
      
            IF SQL%ROWCOUNT > 0 THEN
                  validation_error_process(p_request_id     => l_request_id,
                                           p_group_id       => p_group_id,
                                           p_message_name   => 'GME_INVALID_REASON_NAME',
                                           p_column_name    => 'REASON_NAME',
                                           p_message_type   => GME_BATCH_OPEN_INTERFACE.MESSAGE_TYPE_ERROR,
                                           p_message_text   => fnd_message.get_string('GME', 'GME_INVALID_REASON_NAME'),
                                           x_return_message => x_return_message,
                                           x_return_status  => x_return_status);
            
                  IF x_return_status <> fnd_api.g_ret_sts_success THEN
                        log(x_return_message);
                        return;
                  END IF;
            END IF;
      
            /*
              preparing reason_id for later use,
              this is abstracted from private API,
              for merging two API to one,
              which will be used in individual worker.  
            */
            UPDATE GME_RESOURCE_TXNS_INTERFACE intf
               SET reason_id = (SELECT mtr.reason_id
                                  FROM mtl_transaction_reasons mtr
                                 WHERE NVL(mtr.disable_date, SYSDATE + 1) > SYSDATE
                                   AND mtr.reason_name = intf.reason_name
                                   AND ROWNUM = 1)
             WHERE intf.reason_id IS NULL
               AND intf.reason_name IS NOT NULL
               and intf.GROUP_ID = p_group_id
               AND INTF.process_status = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_RUNNING;
      
            /*Validate instance_id.  instance_id does not exist in gmp_resource_instances*/
            UPDATE GME_RESOURCE_TXNS_INTERFACE intf
               SET process_status = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_BV_INVALID
             WHERE NOT EXISTS (SELECT 1
                      FROM gmp_resource_instances inst, cr_rsrc_dtl r, gme_batch_step_resources gbsr
                     WHERE intf.int_batchstep_resource_id = gbsr.batchstep_resource_id
                       AND gbsr.resources = r.resources
                       AND r.resource_id = inst.resource_id
                       AND intf.instance_id = inst.instance_id)
               AND intf.process_status = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_RUNNING
               AND instance_id IS NOT NULL
               AND intf.GROUP_ID = p_group_id;
      
            IF SQL%ROWCOUNT > 0 THEN
                  validation_error_process(p_request_id     => l_request_id,
                                           p_group_id       => p_group_id,
                                           p_message_name   => 'GME_BAD_INSTANCE_ID',
                                           p_column_name    => 'INSTANCE_ID',
                                           p_message_type   => GME_BATCH_OPEN_INTERFACE.MESSAGE_TYPE_ERROR,
                                           p_message_text   => fnd_message.get_string('GME', 'GME_BAD_INSTANCE_ID'),
                                           x_return_message => x_return_message,
                                           x_return_status  => x_return_status);
            
                  IF x_return_status <> fnd_api.g_ret_sts_success THEN
                        log(x_return_message);
                        return;
                  END IF;
            END IF;
      
            /*Validate RESOURCE_INSTANCE_NO.  RESOURCE_INSTANCE_NO does not exist in gmp_resource_instances*/
      
            UPDATE GME_RESOURCE_TXNS_INTERFACE intf
               SET process_status = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_BV_INVALID
             WHERE NOT EXISTS (SELECT 1
                      FROM gmp_resource_instances inst, cr_rsrc_dtl r, gme_batch_step_resources gbsr
                     WHERE intf.int_batchstep_resource_id = gbsr.batchstep_resource_id
                       AND gbsr.resources = r.resources
                       AND r.resource_id = inst.resource_id
                       AND intf.RESOURCE_INSTANCE_NO = inst.instance_number)
               AND intf.process_status = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_RUNNING
               AND instance_id IS NULL
               AND RESOURCE_INSTANCE_NO IS NOT NULL
               AND intf.GROUP_ID = p_group_id;
      
            IF SQL%ROWCOUNT > 0 THEN
                  validation_error_process(p_request_id     => l_request_id,
                                           p_group_id       => p_group_id,
                                           p_message_name   => 'GME_BAD_INSTANCE_NO',
                                           p_column_name    => 'RESOURCE_INSTANCE_NO',
                                           p_message_type   => GME_BATCH_OPEN_INTERFACE.MESSAGE_TYPE_ERROR,
                                           p_message_text   => fnd_message.get_string('GME', 'GME_BAD_INSTANCE_NO'),
                                           x_return_message => x_return_message,
                                           x_return_status  => x_return_status);
            
                  IF x_return_status <> fnd_api.g_ret_sts_success THEN
                        log(x_return_message);
                        return;
                  END IF;
            END IF;
      
            /*START_DATE must be provided; TRANSATION_DATE must be provided;
            END_DATE must be provided for non-zero resource usage;*/
            UPDATE GME_RESOURCE_TXNS_INTERFACE INTF
               SET PROCESS_STATUS = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_BV_INVALID
             WHERE (INTF.TRANS_DATE IS NULL OR INTF.START_DATE IS NULL OR (END_DATE IS NULL AND INTF.RESOURCE_USAGE <> 0))
               AND INTF.process_status = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_RUNNING
               AND intf.GROUP_ID = p_group_id;
      
            IF SQL%ROWCOUNT > 0 THEN
                  validation_error_process(p_request_id     => l_request_id,
                                           p_group_id       => p_group_id,
                                           p_message_name   => 'GME_INVALID_RES_DATE',
                                           p_column_name    => NULL,
                                           p_message_type   => GME_BATCH_OPEN_INTERFACE.MESSAGE_TYPE_ERROR,
                                           p_message_text   => fnd_message.get_string('GME', 'GME_INVALID_RES_DATE'),
                                           x_return_message => x_return_message,
                                           x_return_status  => x_return_status);
            
                  IF x_return_status <> fnd_api.g_ret_sts_success THEN
                        log(x_return_message);
                        return;
                  END IF;
            END IF;
      
            /*VALIDATE TRANS_DATE WITH ACTUAL_START_DATE. */
            UPDATE GME_RESOURCE_TXNS_INTERFACE INTF
               SET PROCESS_STATUS = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_BV_INVALID
             WHERE EXISTS (SELECT 1
                      FROM GME_BATCH_STEP_RESOURCES RES
                     WHERE INTF.TRANS_DATE < RES.ACTUAL_START_DATE
                       AND INTF.INT_BATCHSTEP_RESOURCE_ID = RES.BATCHSTEP_RESOURCE_ID)
               AND INTF.process_status = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_RUNNING
               AND intf.GROUP_ID = p_group_id;
      
            IF SQL%ROWCOUNT > 0 THEN
                  validation_error_process(p_request_id     => l_request_id,
                                           p_group_id       => p_group_id,
                                           p_message_name   => 'GME_BAD_TRANS_DATE',
                                           p_column_name    => 'TRANS_DATE',
                                           p_message_type   => GME_BATCH_OPEN_INTERFACE.MESSAGE_TYPE_ERROR,
                                           p_message_text   => fnd_message.get_string('GME', 'GME_BAD_TRANS_DATE'),
                                           x_return_message => x_return_message,
                                           x_return_status  => x_return_status);
            
                  IF x_return_status <> fnd_api.g_ret_sts_success THEN
                        log(x_return_message);
                        return;
                  END IF;
            END IF;
      
            /*VALIDATE TRANS_DATE WITH SYS_DATE. */
            UPDATE GME_RESOURCE_TXNS_INTERFACE INTF
               SET PROCESS_STATUS = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_BV_INVALID
             WHERE INTF.TRANS_DATE > GME_COMMON_PVT.G_TIMESTAMP
               AND INTF.process_status = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_RUNNING
               AND intf.GROUP_ID = p_group_id;
      
            IF SQL%ROWCOUNT > 0 THEN
                  validation_error_process(p_request_id     => l_request_id,
                                           p_group_id       => p_group_id,
                                           p_message_name   => 'GME_BAD_TRANS_SYS_DATE',
                                           p_column_name    => 'TRANS_DATE',
                                           p_message_type   => GME_BATCH_OPEN_INTERFACE.MESSAGE_TYPE_ERROR,
                                           p_message_text   => fnd_message.get_string('GME', 'GME_BAD_TRANS_SYS_DATE'),
                                           x_return_message => x_return_message,
                                           x_return_status  => x_return_status);
            
                  IF x_return_status <> fnd_api.g_ret_sts_success THEN
                        log(x_return_message);
                        return;
                  END IF;
            END IF;
      
            /*VALIDATE TRANS_DATE WITH START_DATE.*/
      
            UPDATE GME_RESOURCE_TXNS_INTERFACE INTF
               SET PROCESS_STATUS = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_BV_INVALID
             WHERE INTF.TRANS_DATE < INTF.START_DATE
               AND INTF.process_status = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_RUNNING
               AND intf.GROUP_ID = p_group_id;
      
            IF SQL%ROWCOUNT > 0 THEN
                  validation_error_process(p_request_id     => l_request_id,
                                           p_group_id       => p_group_id,
                                           p_message_name   => 'GME_BAD_TRANS_STRT_DATE',
                                           p_column_name    => 'TRANS_DATE',
                                           p_message_type   => GME_BATCH_OPEN_INTERFACE.MESSAGE_TYPE_ERROR,
                                           p_message_text   => fnd_message.get_string('GME', 'GME_BAD_TRANS_STRT_DATE'),
                                           x_return_message => x_return_message,
                                           x_return_status  => x_return_status);
            
                  IF x_return_status <> fnd_api.g_ret_sts_success THEN
                        log(x_return_message);
                        return;
                  END IF;
            END IF;
      
            /*VALIDATE TRANS_DATE WITH END_DATE. */
            UPDATE GME_RESOURCE_TXNS_INTERFACE INTF
               SET PROCESS_STATUS = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_BV_INVALID
             WHERE INTF.TRANS_DATE > END_DATE
               AND INTF.process_status = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_RUNNING
               AND intf.GROUP_ID = p_group_id;
      
            IF SQL%ROWCOUNT > 0 THEN
                  validation_error_process(p_request_id     => l_request_id,
                                           p_group_id       => p_group_id,
                                           p_message_name   => 'GME_BAD_TRANS_END_DATE',
                                           p_column_name    => 'TRANS_DATE',
                                           p_message_type   => GME_BATCH_OPEN_INTERFACE.MESSAGE_TYPE_ERROR,
                                           p_message_text   => fnd_message.get_string('GME', 'GME_BAD_TRANS_END_DATE'),
                                           x_return_message => x_return_message,
                                           x_return_status  => x_return_status);
            
                  IF x_return_status <> fnd_api.g_ret_sts_success THEN
                        log(x_return_message);
                        return;
                  END IF;
            END IF;
      
            /*VALIDATE START_DATE WITH ACTUAL_START_DATE. */
            UPDATE GME_RESOURCE_TXNS_INTERFACE INTF
               SET PROCESS_STATUS = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_BV_INVALID
             WHERE EXISTS (SELECT 1
                      FROM GME_BATCH_STEP_RESOURCES RES
                     WHERE INTF.START_DATE < RES.ACTUAL_START_DATE
                       AND INTF.INT_BATCHSTEP_RESOURCE_ID = RES.BATCHSTEP_RESOURCE_ID)
               AND INTF.process_status = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_RUNNING
               AND intf.GROUP_ID = p_group_id;
      
            IF SQL%ROWCOUNT > 0 THEN
                  validation_error_process(p_request_id     => l_request_id,
                                           p_group_id       => p_group_id,
                                           p_message_name   => 'GME_BAD_START_DATE',
                                           p_column_name    => 'START_DATE',
                                           p_message_type   => GME_BATCH_OPEN_INTERFACE.MESSAGE_TYPE_ERROR,
                                           p_message_text   => fnd_message.get_string('GME', 'GME_BAD_START_DATE'),
                                           x_return_message => x_return_message,
                                           x_return_status  => x_return_status);
            
                  IF x_return_status <> fnd_api.g_ret_sts_success THEN
                        log(x_return_message);
                        return;
                  END IF;
            END IF;
      
            /*VALIDATE START_DATE WITH SYS_DATE. */
      
            UPDATE GME_RESOURCE_TXNS_INTERFACE INTF
               SET PROCESS_STATUS = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_BV_INVALID
             WHERE INTF.START_DATE > GME_COMMON_PVT.G_TIMESTAMP
               AND INTF.process_status = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_RUNNING
               AND intf.GROUP_ID = p_group_id;
      
            IF SQL%ROWCOUNT > 0 THEN
                  validation_error_process(p_request_id     => l_request_id,
                                           p_group_id       => p_group_id,
                                           p_message_name   => 'GME_BAD_START_END_DATE',
                                           p_column_name    => 'START_DATE',
                                           p_message_type   => GME_BATCH_OPEN_INTERFACE.MESSAGE_TYPE_ERROR,
                                           p_message_text   => fnd_message.get_string('GME', 'GME_BAD_START_END_DATE'),
                                           x_return_message => x_return_message,
                                           x_return_status  => x_return_status);
            
                  IF x_return_status <> fnd_api.g_ret_sts_success THEN
                        log(x_return_message);
                        return;
                  END IF;
            END IF;
      
            /*VALIDATE END_DATE WITH ACTUAL_START_DATE. */
      
            UPDATE GME_RESOURCE_TXNS_INTERFACE INTF
               SET PROCESS_STATUS = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_BV_INVALID
             WHERE INTF.END_DATE < INTF.START_DATE
               AND INTF.process_status = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_RUNNING
               AND intf.GROUP_ID = p_group_id;
      
            IF SQL%ROWCOUNT > 0 THEN
                  validation_error_process(p_request_id     => l_request_id,
                                           p_group_id       => p_group_id,
                                           p_message_name   => 'PM_BADENDDATE',
                                           p_column_name    => 'END_DATE',
                                           p_message_type   => GME_BATCH_OPEN_INTERFACE.MESSAGE_TYPE_ERROR,
                                           p_message_text   => fnd_message.get_string('GME', 'PM_BADENDDATE'),
                                           x_return_message => x_return_message,
                                           x_return_status  => x_return_status);
            
                  IF x_return_status <> fnd_api.g_ret_sts_success THEN
                        return;
                  END IF;
            END IF;
      
            /*VALIDATE END_DATE WITH SYS_DATE. */
      
            UPDATE GME_RESOURCE_TXNS_INTERFACE INTF
               SET PROCESS_STATUS = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_BV_INVALID
             WHERE INTF.END_DATE > GME_COMMON_PVT.G_TIMESTAMP
               AND INTF.process_status = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_RUNNING
               AND intf.GROUP_ID = p_group_id;
      
            IF SQL%ROWCOUNT > 0 THEN
                  validation_error_process(p_request_id     => l_request_id,
                                           p_group_id       => p_group_id,
                                           p_message_name   => 'GME_BAD_END_DATE',
                                           p_column_name    => 'END_DATE',
                                           p_message_type   => GME_BATCH_OPEN_INTERFACE.MESSAGE_TYPE_ERROR,
                                           p_message_text   => fnd_message.get_string('GME', 'GME_BAD_END_DATE'),
                                           x_return_message => x_return_message,
                                           x_return_status  => x_return_status);
            
                  IF x_return_status <> fnd_api.g_ret_sts_success THEN
                        log(x_return_message);
                        return;
                  END IF;
            END IF;
           
            /*                                    
            BEGIN ER 19161894                    
            Shaliu Chen 18-JUL-2014               
            Added for OPM Step Level OSP Project  
            */        
            UPDATE GME_RESOURCE_TXNS_INTERFACE INTF
               SET PROCESS_STATUS = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_BV_INVALID
             WHERE NOT EXISTS (SELECT 1
                                 FROM RCV_TRANSACTIONS RT
                                WHERE RT.TRANSACTION_ID = INTF.RCV_TRANSACTION_ID)
               AND INTF.RCV_TRANSACTION_ID IS NOT NULL
               AND INTF.PROCESS_STATUS = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_RUNNING
               AND INTF.GROUP_ID = P_GROUP_ID;
               
            IF SQL%ROWCOUNT > 0 THEN
                  validation_error_process(p_request_id     => l_request_id,
                                           p_group_id       => p_group_id,
                                           p_message_name   => 'GME_INVALID_RCV_TRANS_ID',
                                           p_column_name    => 'RCV_TRANSACTION_ID',
                                           p_message_type   => GME_BATCH_OPEN_INTERFACE.MESSAGE_TYPE_ERROR,
                                           p_message_text   => fnd_message.get_string('GME', 'GME_INVALID_RCV_TRANS_ID'),
                                           x_return_message => x_return_message,
                                           x_return_status  => x_return_status);

                  IF x_return_status <> fnd_api.g_ret_sts_success THEN
                        log(x_return_message);
                        return;
                  END IF;
            END IF;               
                                
                                
            UPDATE GME_RESOURCE_TXNS_INTERFACE INTF
               SET PROCESS_STATUS = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_BV_INVALID
             WHERE NOT EXISTS ( SELECT 1
                                  FROM PO_DISTRIBUTIONS_ALL PD
                                 WHERE PD.PO_HEADER_ID = INTF.PO_HEADER_ID
                                   AND PD.WIP_ENTITY_ID IS NOT NULL)
               AND INTF.PO_HEADER_ID IS NOT NULL
               AND INTF.PROCESS_STATUS = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_RUNNING
               AND INTF.GROUP_ID = P_GROUP_ID;  
               
            IF SQL%ROWCOUNT > 0 THEN
                  validation_error_process(p_request_id     => l_request_id,
                                           p_group_id       => p_group_id,
                                           p_message_name   => 'GME_INVALID_PO_HEADER_ID',
                                           p_column_name    => 'PO_HEADER_ID',
                                           p_message_type   => GME_BATCH_OPEN_INTERFACE.MESSAGE_TYPE_ERROR,
                                           p_message_text   => fnd_message.get_string('GME', 'GME_INVALID_PO_HEADER_ID'),
                                           x_return_message => x_return_message,
                                           x_return_status  => x_return_status);

                  IF x_return_status <> fnd_api.g_ret_sts_success THEN
                        log(x_return_message);
                        return;
                  END IF;
            END IF;               
               
            UPDATE GME_RESOURCE_TXNS_INTERFACE INTF
               SET PROCESS_STATUS = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_BV_INVALID
             WHERE NOT EXISTS ( SELECT 1
                                  FROM PO_DISTRIBUTIONS_ALL PD
                                 WHERE PD.PO_LINE_ID = INTF.PO_LINE_ID
                                   AND PD.WIP_ENTITY_ID IS NOT NULL)
               AND INTF.PO_LINE_ID IS NOT NULL
               AND INTF.PROCESS_STATUS = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_RUNNING
               AND INTF.GROUP_ID = P_GROUP_ID;  
               
            IF SQL%ROWCOUNT > 0 THEN
                  validation_error_process(p_request_id     => l_request_id,
                                           p_group_id       => p_group_id,
                                           p_message_name   => 'GME_INVALID_PO_LINE_ID',
                                           p_column_name    => 'PO_LINE_ID',
                                           p_message_type   => GME_BATCH_OPEN_INTERFACE.MESSAGE_TYPE_ERROR,
                                           p_message_text   => fnd_message.get_string('GME', 'GME_INVALID_PO_LINE_ID'),
                                           x_return_message => x_return_message,
                                           x_return_status  => x_return_status);

                  IF x_return_status <> fnd_api.g_ret_sts_success THEN
                        log(x_return_message);
                        return;
                  END IF;
            END IF;       
            /*END ER 19161894*/
        IF g_validation_has_error then  
            l_conc_status := FND_CONCURRENT.SET_COMPLETION_STATUS('WARNING', fnd_message.get_string('GME', 'GME_INTF_CP_WARN'));
            log('Warning:Data validation failed, please check in table gme_intf_errors. request_id='||p_request_id);
         end if;
         
         
      EXCEPTION
            WHEN OTHERS THEN
                  x_return_status  := fnd_api.g_ret_sts_error;
                  x_return_message := 'Error occus in ' || g_pkg_name || '.resource_txns_bulk_validation, error message: ' || SUBSTR(SQLERRM, 1, 240);
      END resource_txns_bulk_validation;

      /* 
        
        DESCRIPTION
           This procedure is used to launch resource transaction worker.
         And it will be invoke in process_resource_transactions
         calling by using  FND_REQUEST.SUBMIT_REQUEST(... program     => 'GMERCSW'...); 
           
      */
PROCEDURE launch_rsc_txn_worker(err_buf OUT NOCOPY VARCHAR2, ret_code OUT NOCOPY VARCHAR2, p_group_id IN NUMBER, p_purge_flag IN VARCHAR2 DEFAULT 'Y') IS
            x_message_count NUMBER;
            x_message_list  VARCHAR2(2000);
            x_app_name VARCHAR2(3);
            x_message_name            VARCHAR2 (256);
            x_message_detail          GME_BATCH_OPEN_INTERFACE.T_MESSAGE_TABLE;
            
            
            x_return_status VARCHAR2(2000);
      
            l_rsrc_txns_rec gme_resource_txns%ROWTYPE;
            x_rsrc_txn_rec  gme_resource_txns%ROWTYPE;
      
            --Cusor to fetch resource transaction
            /*                                    
            BEGIN ER 19161894                    
            Shaliu Chen 18-JUL-2014               
            Modified by Shaliu for OSP Project
            Add Interface_Id column into Order By to 
            make sure that the parent resoruce txn will be 
            imported before correction/return resource txn
            if they are in the same group.
            */                
            CURSOR C_RSRC_TXNS_INTERFACE(v_group_id NUMBER) IS
                  SELECT *
                    FROM GME_RESOURCE_TXNS_INTERFACE
                   WHERE GROUP_ID = v_group_id
                     AND process_status = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_RUNNING
                   ORDER BY INT_ORGANIZATION_ID, INT_BATCH_ID,INTERFACE_ID;
      
            rec_rsrc_txns GME_RESOURCE_TXNS_INTERFACE%ROWTYPE;
      
            l_message VARCHAR2(2000);
      
            returnCode NUMBER;
      
            l_request_id  NUMBER;
            l_conc_status BOOLEAN;
      
            TYPE rsrc_txns_table_type IS TABLE OF GME_RESOURCE_TXNS_INTERFACE%ROWTYPE INDEX BY BINARY_INTEGER;
      
            rsrc_txns_table  rsrc_txns_table_type;
            l_bulk_load_size NUMBER;
      
            l_login  NUMBER := NVL(FND_GLOBAL.login_id, -1);
            l_userid NUMBER := NVL(FND_GLOBAL.user_id, -1);
            
            l_app_short_name varchar2(3)  := fnd_global.application_short_name;
          
                                     
            l_sql varchar2(2000);   
            /*                                    
            BEGIN ER 19161894                    
            Shaliu Chen 18-JUL-2014               
            Added for OPM Step Level OSP Project  
            */             
            l_batchstep_id          NUMBER;
            l_plan_rsrc_usage       NUMBER;
            l_actual_rsrc_count     NUMBER;  
            l_actual_rsrc_usage     NUMBER;      
            l_in_batch_step_rec     gme_batch_steps%ROWTYPE;
            l_out_batch_step_rec    gme_batch_steps%ROWTYPE;    
            batch_step_fetch_error  EXCEPTION;
            update_step_qty_error   EXCEPTION;                    
            /*END ER 19161894*/
      BEGIN
            IF (g_debug <> -1) THEN
                  gme_debug.log_initialize('baim');
                  fnd_file.put_line(fnd_file.LOG, gme_debug.G_FILE_NAME);
            END IF;
            
            g_validation_has_error := FALSE;
            
            l_request_id := FND_GLOBAL.CONC_REQUEST_ID;
            log('worker is starting. request_id is:' || l_request_id);
            log('begin resource_txns_bulk_validation ');
            resource_txns_bulk_validation(p_group_id => p_group_id, p_request_id => l_request_id, x_return_message => err_buf, x_return_status => ret_code);
      
            if x_return_status <> fnd_api.G_RET_STS_SUCCESS THEN
                  l_conc_status := FND_CONCURRENT.SET_COMPLETION_STATUS('ERROR', err_buf);
                  log(err_buf);
                  RETURN;
            END IF;
      
            log('end resource_txns_bulk_validation ');
            l_bulk_load_size := NVL(fnd_profile.VALUE('GME_BULK_LOAD_SIZE'), 1000); -- default value 1000
      
            OPEN C_RSRC_TXNS_INTERFACE(p_group_id);
      
            LOOP
                  FETCH C_RSRC_TXNS_INTERFACE BULK COLLECT
                        INTO rsrc_txns_table LIMIT l_bulk_load_size;
            
                  IF rsrc_txns_table.COUNT > 0 THEN
                  
                        FOR i IN rsrc_txns_table.FIRST .. rsrc_txns_table.LAST LOOP
                              rec_rsrc_txns := rsrc_txns_table(i);
                              -- start of bookmark#2
                        
                              l_rsrc_txns_rec.orgn_code          := rec_rsrc_txns.organization_code;
                              l_rsrc_txns_rec.doc_id             := rec_rsrc_txns.int_BATCH_ID;
                              l_rsrc_txns_rec.line_id            := rec_rsrc_txns.INT_BATCHSTEP_RESOURCE_ID;
                              l_rsrc_txns_rec.resources          := rec_rsrc_txns.resources;
                              l_rsrc_txns_rec.resource_usage     := rec_rsrc_txns.resource_usage;
                              l_rsrc_txns_rec.trans_date         := rec_rsrc_txns.trans_date;
                              l_rsrc_txns_rec.reason_code        := rec_rsrc_txns.reason_name;
                              l_rsrc_txns_rec.start_date         := rec_rsrc_txns.start_date;
                              l_rsrc_txns_rec.end_date           := rec_rsrc_txns.end_date;
                              l_rsrc_txns_rec.creation_date      := rec_rsrc_txns.creation_date;
                              l_rsrc_txns_rec.last_update_date   := rec_rsrc_txns.last_update_date;
                              l_rsrc_txns_rec.created_by         := rec_rsrc_txns.created_by;
                              l_rsrc_txns_rec.last_updated_by    := rec_rsrc_txns.last_updated_by;
                              l_rsrc_txns_rec.LAST_UPDATE_LOGIN  := rec_rsrc_txns.LAST_UPDATE_LOGIN;
                              l_rsrc_txns_rec.ATTRIBUTE_CATEGORY := rec_rsrc_txns.ATTRIBUTE_CATEGORY;
                              l_rsrc_txns_rec.REQUEST_ID         := rec_rsrc_txns.REQUEST_ID;
                              l_rsrc_txns_rec.INSTANCE_ID        := rec_rsrc_txns.INSTANCE_ID;
                              l_rsrc_txns_rec.ORGANIZATION_ID    := rec_rsrc_txns.int_organization_id;
                              l_rsrc_txns_rec.TRANS_QTY_UM       := rec_rsrc_txns.TRANS_QTY_UM;
                              l_rsrc_txns_rec.REASON_ID          := rec_rsrc_txns.REASON_ID;
                              l_rsrc_txns_rec.ATTRIBUTE1         := rec_rsrc_txns.ATTRIBUTE1;
                              l_rsrc_txns_rec.ATTRIBUTE2         := rec_rsrc_txns.ATTRIBUTE2;
                              l_rsrc_txns_rec.ATTRIBUTE3         := rec_rsrc_txns.ATTRIBUTE3;
                              l_rsrc_txns_rec.ATTRIBUTE4         := rec_rsrc_txns.ATTRIBUTE4;
                              l_rsrc_txns_rec.ATTRIBUTE5         := rec_rsrc_txns.ATTRIBUTE5;
                              l_rsrc_txns_rec.ATTRIBUTE6         := rec_rsrc_txns.ATTRIBUTE6;
                              l_rsrc_txns_rec.ATTRIBUTE7         := rec_rsrc_txns.ATTRIBUTE7;
                              l_rsrc_txns_rec.ATTRIBUTE8         := rec_rsrc_txns.ATTRIBUTE8;
                              l_rsrc_txns_rec.ATTRIBUTE9         := rec_rsrc_txns.ATTRIBUTE9;
                              l_rsrc_txns_rec.ATTRIBUTE10        := rec_rsrc_txns.ATTRIBUTE10;
                              l_rsrc_txns_rec.ATTRIBUTE11        := rec_rsrc_txns.ATTRIBUTE11;
                              l_rsrc_txns_rec.ATTRIBUTE12        := rec_rsrc_txns.ATTRIBUTE12;
                              l_rsrc_txns_rec.ATTRIBUTE13        := rec_rsrc_txns.ATTRIBUTE13;
                              l_rsrc_txns_rec.ATTRIBUTE14        := rec_rsrc_txns.ATTRIBUTE14;
                              l_rsrc_txns_rec.ATTRIBUTE15        := rec_rsrc_txns.ATTRIBUTE15;
                              l_rsrc_txns_rec.ATTRIBUTE16        := rec_rsrc_txns.ATTRIBUTE16;
                              l_rsrc_txns_rec.ATTRIBUTE17        := rec_rsrc_txns.ATTRIBUTE17;
                              l_rsrc_txns_rec.ATTRIBUTE18        := rec_rsrc_txns.ATTRIBUTE18;
                              l_rsrc_txns_rec.ATTRIBUTE19        := rec_rsrc_txns.ATTRIBUTE19;
                              l_rsrc_txns_rec.ATTRIBUTE20        := rec_rsrc_txns.ATTRIBUTE20;
                              l_rsrc_txns_rec.ATTRIBUTE21        := rec_rsrc_txns.ATTRIBUTE21;
                              l_rsrc_txns_rec.ATTRIBUTE22        := rec_rsrc_txns.ATTRIBUTE22;
                              l_rsrc_txns_rec.ATTRIBUTE23        := rec_rsrc_txns.ATTRIBUTE23;
                              l_rsrc_txns_rec.ATTRIBUTE24        := rec_rsrc_txns.ATTRIBUTE24;
                              l_rsrc_txns_rec.ATTRIBUTE25        := rec_rsrc_txns.ATTRIBUTE25;
                              l_rsrc_txns_rec.ATTRIBUTE26        := rec_rsrc_txns.ATTRIBUTE26;
                              l_rsrc_txns_rec.ATTRIBUTE27        := rec_rsrc_txns.ATTRIBUTE27;
                              l_rsrc_txns_rec.ATTRIBUTE28        := rec_rsrc_txns.ATTRIBUTE28;
                              l_rsrc_txns_rec.ATTRIBUTE29        := rec_rsrc_txns.ATTRIBUTE29;
                              l_rsrc_txns_rec.ATTRIBUTE30        := rec_rsrc_txns.ATTRIBUTE30;
                              /*                                    
                              BEGIN ER 19161894                    
                              Shaliu Chen 18-JUL-2014               
                              populate new column added for OPM OSP  
                              */                                  
                              l_rsrc_txns_rec.cost_source              := rec_rsrc_txns.cost_source             ;
                              l_rsrc_txns_rec.po_header_id             := rec_rsrc_txns.po_header_id            ;
                              l_rsrc_txns_rec.po_line_id               := rec_rsrc_txns.po_line_id              ;
                              l_rsrc_txns_rec.actual_resource_rate     := rec_rsrc_txns.actual_resource_rate    ;
                              l_rsrc_txns_rec.currency_code            := rec_rsrc_txns.currency_code           ;
                              l_rsrc_txns_rec.currency_conversion_date := rec_rsrc_txns.currency_conversion_date;
                              l_rsrc_txns_rec.currency_conversion_type := rec_rsrc_txns.currency_conversion_type;
                              l_rsrc_txns_rec.currency_conversion_rate := rec_rsrc_txns.currency_conversion_rate;
                              l_rsrc_txns_rec.rcv_transaction_id       := rec_rsrc_txns.rcv_transaction_id      ;                              
                              /*END ER 19161894*/
                              --calling API to create resource transactions.
                              GME_RESOURCE_TXN_OPEN_INTF.insert_actual_rsrc_txn(p_init_msg_list         => fnd_api.g_true,
                                                                                p_commit                => fnd_api.g_true,
                                                                                p_org_code              => rec_rsrc_txns.organization_code,
                                                                                p_rsrc_txn_rec          => l_rsrc_txns_rec,
                                                                                p_batchstep_resource_id => rec_rsrc_txns.INT_BATCHSTEP_RESOURCE_ID,
                                                                                x_rsrc_txn_rec          => x_rsrc_txn_rec,
                                                                                x_message_count         => x_message_count,
                                                                                x_message_list          => x_message_list,
                                                                                x_return_status         => x_return_status);
                        
                              IF (x_return_status = fnd_api.g_ret_sts_success) THEN
                                BEGIN
                                    UPDATE GME_RESOURCE_TXNS_INTERFACE
                                       SET PROCESS_STATUS    = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_SUCCEED,
                                           request_id        = FND_GLOBAL.CONC_REQUEST_ID,
                                           last_updated_by   = l_userid,
                                           creation_date     = SYSDATE,
                                           created_by        = l_userid,
                                           last_update_login = l_login,
                                           last_update_date  = SYSDATE
                                     WHERE INTERFACE_ID = rec_rsrc_txns.INTERFACE_ID;
                                     
                                      /*
                                      BEGIN ER 19161894                    
                                      Shaliu Chen 18-JUL-2014                                            
                                      update actual step qty after resource txn imported
                                     */
                                     IF l_rsrc_txns_rec.rcv_transaction_id IS NOT NULL THEN 
                                      
                                       SELECT batchstep_id,plan_rsrc_usage,actual_rsrc_count,actual_rsrc_usage
                                         INTO l_batchstep_id,l_plan_rsrc_usage,l_actual_rsrc_count,l_actual_rsrc_usage
                                         FROM gme_batch_step_resources
                                        WHERE batchstep_resource_id = rec_rsrc_txns.int_batchstep_resource_id;
                                        
/*                                       \*
                                        If Actual Resource Count is NULL,then assign plan resource count to it
                                       *\ 
                                       IF ((l_actual_rsrc_count IS NULL OR l_actual_rsrc_count = 0) AND
                                            l_actual_rsrc_usage <> 0) THEN 
                                         UPDATE gme_batch_step_resources
                                            SET actual_rsrc_count = plan_rsrc_count
                                          WHERE batchstep_resource_id = rec_rsrc_txns.int_batchstep_resource_id;
                                       ELSIF (l_actual_rsrc_usage =0 AND l_actual_rsrc_count >0) THEN                                       
                                       \*
                                         If actual resource usage has been reversed to zero,
                                         then also update actual resource count value to zero.
                                       *\
                                         UPDATE gme_batch_step_resources
                                            SET actual_rsrc_count = 0
                                          WHERE batchstep_resource_id = rec_rsrc_txns.int_batchstep_resource_id;                                                                                      
                                       END IF;*/ 
                                       
                                       IF (l_actual_rsrc_usage <> 0 AND l_actual_rsrc_usage IS NOT NULL) THEN
                                         /*
                                          If Actual Resource Count is NULL,then assign plan resource count to it
                                         */                                          
                                         IF (l_actual_rsrc_count IS NULL OR l_actual_rsrc_count = 0) THEN
                                           
                                           UPDATE gme_batch_step_resources
                                              SET actual_rsrc_count = plan_rsrc_count
                                            WHERE batchstep_resource_id = rec_rsrc_txns.int_batchstep_resource_id;                                            
                                         END IF;
                                       ELSIF l_actual_rsrc_usage = 0 THEN
                                         /*
                                           If actual resource usage has been reversed to zero,
                                           then also update actual resource count value to zero.
                                         */                                       
                                         UPDATE gme_batch_step_resources
                                            SET actual_rsrc_count = NULL,
                                                actual_rsrc_usage = NULL                                                
                                          WHERE batchstep_resource_id = rec_rsrc_txns.int_batchstep_resource_id; 
                                       ELSIF (l_actual_rsrc_usage IS NULL AND l_actual_rsrc_count IS NOT NULL) THEN
                                         UPDATE gme_batch_step_resources
                                            SET actual_rsrc_count = NULL                                            
                                          WHERE batchstep_resource_id = rec_rsrc_txns.int_batchstep_resource_id;                                                                               
                                       END IF;  

                                       UPDATE gme_batch_steps
                                          SET actual_step_qty = NVL(actual_step_qty,0)+(rec_rsrc_txns.resource_usage*NVL(plan_step_qty,0)/l_plan_rsrc_usage)
                                        WHERE batchstep_id = l_batchstep_id; 
                                        
                                       l_in_batch_step_rec.batchstep_id := l_batchstep_id;
                                        
                                       IF NOT (gme_batch_steps_dbl.fetch_row(l_in_batch_step_rec, l_in_batch_step_rec)) THEN
                                         RAISE batch_step_fetch_error;
                                       END IF;                                      
                                        
                                       gme_update_step_qty_pvt.update_step_qty
                                              (p_batch_step_rec         => l_in_batch_step_rec
                                              ,x_message_count          => x_message_count
                                              ,x_message_list           => x_message_list
                                              ,x_return_status          => x_return_status
                                              ,x_batch_step_rec         => l_out_batch_step_rec);   
                                              
                                       IF x_return_status <> FND_API.G_RET_STS_SUCCESS THEN
                                         RAISE update_step_qty_error;
                                       END IF;                                                                                                               
                                     END IF;    
                                EXCEPTION
                                  WHEN batch_step_fetch_error or update_step_qty_error THEN
                                    returnCode := GME_BATCH_OPEN_INTERFACE.gme_log_interface_err(P_GROUP_ID          => rec_rsrc_txns.group_id,
                                                                                                 P_object_type       => NULL,
                                                                                                 P_INTERFACE_ID      => rec_rsrc_txns.interface_id,
                                                                                                 P_COLUMN_NAME       => NULL,
                                                                                                 P_MESSAGE_TYPE      => GME_BATCH_OPEN_INTERFACE.MESSAGE_TYPE_ERROR,
                                                                                                 P_MESSAGE_NAME      => NULL,
                                                                                                 P_MESSAGE_TEXT      => x_message_list,
                                                                                                 P_SOURCE_TABLE_NAME => 'GME_RESOURCE_TXNS_INTERFACE',
                                                                                                 P_APP_NAME          => l_app_short_name
                                                                                                 );  
                                                                                                   
                                    UPDATE GME_RESOURCE_TXNS_INTERFACE
                                       SET process_status    = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_FAILED,
                                           request_id        = FND_GLOBAL.CONC_REQUEST_ID,
                                           last_updated_by   = l_userid,
                                           creation_date     = SYSDATE,
                                           created_by        = l_userid,
                                           last_update_login = l_login,
                                           last_update_date  = SYSDATE
                                     WHERE INTERFACE_ID = rec_rsrc_txns.INTERFACE_ID;
                                  WHEN OTHERS THEN
                                    returnCode := GME_BATCH_OPEN_INTERFACE.gme_log_interface_err(P_GROUP_ID          => rec_rsrc_txns.group_id,
                                                                                                 P_object_type       => NULL,
                                                                                                 P_INTERFACE_ID      => rec_rsrc_txns.interface_id,
                                                                                                 P_COLUMN_NAME       => NULL,
                                                                                                 P_MESSAGE_TYPE      => GME_BATCH_OPEN_INTERFACE.MESSAGE_TYPE_ERROR,
                                                                                                 P_MESSAGE_NAME      => NULL,
                                                                                                 P_MESSAGE_TEXT      => SQLERRM,
                                                                                                 P_SOURCE_TABLE_NAME => 'GME_RESOURCE_TXNS_INTERFACE',
                                                                                                 P_APP_NAME          => l_app_short_name
                                                                                                 );  
                                                                                                   
                                    UPDATE GME_RESOURCE_TXNS_INTERFACE
                                       SET process_status    = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_FAILED,
                                           request_id        = FND_GLOBAL.CONC_REQUEST_ID,
                                           last_updated_by   = l_userid,
                                           creation_date     = SYSDATE,
                                           created_by        = l_userid,
                                           last_update_login = l_login,
                                           last_update_date  = SYSDATE
                                     WHERE INTERFACE_ID = rec_rsrc_txns.INTERFACE_ID;                                                                                                                                                                         
                                    
                                END;                                                                                
                                                                              
                               /*END ER 19161894*/                                    
                              ELSE
                                    l_message := get_error_message;
                                    log('error occured when creating resource transactions, the error is:' || l_message);
                                    g_create_rs_txn_has_error := TRUE;
                                     
                                  GME_BATCH_OPEN_INTERFACE.get_error_message_infor(
                                    p_message_text         => x_message_list
                                     ,x_app_name             => x_app_name
                                     ,x_message_name         => x_message_name
                                     ,x_message_detail       => x_message_detail); 
                                     
                                    process_param_token(p_err_message         => l_message
                                      ,p_err_message_name    => x_message_name
                                    ,p_app_name            => x_app_name
                                    ,p_err_message_detail  => x_message_detail
                                      ,p_group_id            => rec_rsrc_txns.GROUP_ID
                                      ,p_interface_id        => rec_rsrc_txns.interface_id);
                                      
                                     /*
                                      
                                    returnCode := GME_BATCH_OPEN_INTERFACE.gme_log_interface_err(P_GROUP_ID          => rec_rsrc_txns.GROUP_ID,
                                                                                                 P_object_type       => NULL,
                                                                                                 P_INTERFACE_ID      => rec_rsrc_txns.interface_id,
                                                                                                 P_COLUMN_NAME       => NULL,
                                                                                                 P_MESSAGE_TYPE      => GME_BATCH_OPEN_INTERFACE.MESSAGE_TYPE_ERROR,
                                                                                                 P_MESSAGE_NAME      => NULL,
                                                                                                 P_MESSAGE_TEXT      => l_message,
                                                                                                 P_SOURCE_TABLE_NAME => 'GME_RESOURCE_TXNS_INTERFACE');
                                   */
                                    UPDATE GME_RESOURCE_TXNS_INTERFACE
                                       SET process_status    = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_FAILED,
                                           request_id        = FND_GLOBAL.CONC_REQUEST_ID,
                                           last_updated_by   = l_userid,
                                           creation_date     = SYSDATE,
                                           created_by        = l_userid,
                                           last_update_login = l_login,
                                           last_update_date  = SYSDATE
                                     WHERE INTERFACE_ID = rec_rsrc_txns.INTERFACE_ID;
                              
                                    --l_conc_status := FND_CONCURRENT.SET_COMPLETION_STATUS('WARNING', x_message_list);
                                     
                              END IF;
                              ----------end of bookmark#2
                        END LOOP;
                  END IF;
            
                  EXIT WHEN C_RSRC_TXNS_INTERFACE%NOTFOUND;
            END LOOP;
            
            
            IF g_create_rs_txn_has_error THEN
               l_conc_status := FND_CONCURRENT.SET_COMPLETION_STATUS('WARNING', fnd_message.get_string('GME', 'GME_INTF_CP_WARN'));
            END IF;
            
            
            CLOSE C_RSRC_TXNS_INTERFACE;
      
            log('worker is finished. request_id is:' || l_request_id);
      
            IF p_purge_flag = 'Y' THEN 
                 --Modified by michael, bug 17452045, delete error messages first, then delete interface table
                  --Begin of bug 17452045
                  DELETE FROM GME_INTF_ERRORS e
                  WHERE EXISTS (SELECT 1
                                FROM   gme_resource_txns_interface res
                                WHERE  res.interface_id = e.interface_id
                                AND    res.group_id = p_group_id
                                AND    res.process_status = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_SUCCEED)
                  AND source_table_name = 'GME_RESOURCE_TXNS_INTERFACE';

                   
                  DELETE FROM GME_RESOURCE_TXNS_INTERFACE
                  WHERE  group_id = p_group_id
                  AND    process_status = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_SUCCEED; 
                  --End of bug 17452045  
            END IF;
            
            
            COMMIT;
      EXCEPTION
            WHEN OTHERS THEN
                  returnCode := GME_BATCH_OPEN_INTERFACE.gme_log_interface_err(P_GROUP_ID          => p_group_id,
                                                                                 P_object_type       => NULL,
                                                                                 P_INTERFACE_ID      => '-1',
                                                                                 P_COLUMN_NAME       => NULL,
                                                                                 P_MESSAGE_TYPE      => GME_BATCH_OPEN_INTERFACE.MESSAGE_TYPE_ERROR,
                                                                                 P_MESSAGE_NAME      => NULL,
                                                                                 P_MESSAGE_TEXT      => SQLERRM,
                                                                                 P_SOURCE_TABLE_NAME => 'GME_RESOURCE_TXNS_INTERFACE',
                                                                                 P_APP_NAME => l_app_short_name
                                                                                 );
            
                  l_conc_status := FND_CONCURRENT.SET_COMPLETION_STATUS('ERROR', SQLERRM);
                   log(SQLERRM);
                  COMMIT;
                  RAISE;
 END launch_rsc_txn_worker;

      /* 
         
        DESCRIPTION
          this procedure is used to derive internal id:int_batch_id, int_batchstep_resource_id, int_organization_id
       
      */
      PROCEDURE derive_int_id_columns(p_request_id IN NUMBER, x_return_message OUT NOCOPY VARCHAR2, x_return_status OUT NOCOPY VARCHAR2) IS
      BEGIN
            x_return_status := fnd_api.g_ret_sts_success;
            --scenario:batchstep_resource_id is not null 
            UPDATE GME_RESOURCE_TXNS_INTERFACE intf
               SET (int_batch_id, int_batchstep_resource_id, int_organization_id) = (SELECT gbsr.batch_id, gbsr.batchstep_resource_id, gbsr.organization_id
                                                                                       FROM gme_batch_step_resources gbsr
                                                                                      WHERE gbsr.batchstep_resource_id = intf.batchstep_resource_id)
             WHERE intf.batchstep_resource_id IS NOT NULL
               AND intf.process_status = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_RUNNING
               AND request_id = p_request_id;
      
            --scenario:batchstep_resource_id is null 
            UPDATE GME_RESOURCE_TXNS_INTERFACE intf
               SET (int_batch_id, int_organization_id) = (SELECT gbh.batch_id, mp.organization_id
                                                            FROM gme_batch_header gbh, mtl_parameters mp
                                                           WHERE gbh.batch_no = intf.batch_no
                                                             AND gbh.organization_id = mp.organization_id
                                                             AND mp.organization_code = intf.organization_code
                                                             AND gbh.batch_type = 0)
             WHERE intf.BATCHSTEP_RESOURCE_ID IS NULL
               AND intf.process_status = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_RUNNING
               AND request_id = p_request_id;
      
            UPDATE GME_RESOURCE_TXNS_INTERFACE intf
               SET process_status = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_BV_INVALID
             WHERE int_batch_id IS NULL
               AND intf.process_status = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_RUNNING
               AND request_id = p_request_id;
      
            IF SQL%ROWCOUNT > 0 THEN
                  validation_error_process(p_request_id     => p_request_id,
                                           p_group_id       => NULL,
                                           p_message_name   => 'GME_BATCHID_NOTFOUND',
                                           p_column_name    => NULL,
                                           p_message_type   => GME_BATCH_OPEN_INTERFACE.MESSAGE_TYPE_ERROR,
                                           p_message_text   => fnd_message.get_string('GME', 'GME_BATCHID_NOTFOUND'),
                                           x_return_message => x_return_message,
                                           x_return_status  => x_return_status);
            
                  IF x_return_status <> fnd_api.g_ret_sts_success THEN
                        log(x_return_message);
                        RETURN;
                  END IF;
            END IF;
           
         
      EXCEPTION
            WHEN OTHERS THEN
                  x_return_status  := fnd_api.G_RET_STS_ERROR;
                  x_return_message := 'Error occurs while logging error message:' || SQLCODE || ',error message:' || SQLERRM;
      END derive_int_id_columns;







      /* 
      #  
      # DESCRIPTION
      #    this procedure is resource transaction manager.
      #   major purpose: 1.Multiple Worker Division for resource transaction 
      #                  2.launch worker
      # HISTORY
      #  Created : 1/24/2013  michael  
      */
      PROCEDURE process_resource_transactions(err_buf                  OUT NOCOPY VARCHAR2,
                                              ret_code                 OUT NOCOPY VARCHAR2,
                                              p_purge_flag             IN VARCHAR2 DEFAULT 'Y',
                                              p_max_no_of_worker       IN NUMBER DEFAULT 5,
                                              p_max_rows_of_per_worker IN NUMBER DEFAULT 1000) IS
            l_user_id       NUMBER;
            l_return_msg    VARCHAR2(2000);
            l_return_status VARCHAR2(100);
      
            l_conc_status           BOOLEAN;
            l_group_id              NUMBER;
            l_no_of_rows_processing NUMBER := 0;
            l_no_distinct_batch     NUMBER := 0;
            l_no_of_workers         NUMBER := 0;
            l_worker_index          NUMBER;
            l_worker_request_id     NUMBER;
      
            CURSOR cursor_distinct_batch(v_request_id NUMBER) IS
                  SELECT DISTINCT int_batch_id
                    FROM GME_RESOURCE_TXNS_INTERFACE a
                   WHERE process_status = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_RUNNING
                     AND request_id = v_request_id
                   ORDER BY int_batch_id;
      
            l_int_batch_id           NUMBER(10);
            l_request_id             NUMBER;
            l_batch_count_per_worker NUMBER;
            l_mod_val                NUMBER;
            row_count                NUMBER := 0;
      
            l_add_one        NUMBER;
            returnCode       NUMBER;
            x_return_message VARCHAR2(2000);
      
            x_return_status VARCHAR2(10);
           l_app_short_name varchar2(3)  := fnd_global.application_short_name;
      BEGIN
            g_validation_has_error := FALSE;
            
            fnd_file.put_line(fnd_file.log,'The input parameter of Purge Successful Rows is'||p_purge_flag);
            fnd_file.put_line(fnd_file.log,'The input parameter of Max Number of Workers is'||p_max_no_of_worker);
            fnd_file.put_line(fnd_file.log,'The input parameter of Optimal Number of Rows Per Worker is'||p_max_rows_of_per_worker);        
      
            IF (g_debug <> -1) THEN
                  gme_debug.log_initialize();
            END IF;
      
            fnd_msg_pub.initialize;
      
            l_request_id := FND_GLOBAL.CONC_REQUEST_ID;
            l_user_id    := fnd_global.user_id;
      
            log('The request id of resource transaction manager is:' || l_request_id);
            log('The user id of resource transaction manager is:' || l_user_id);
      
            UPDATE GME_RESOURCE_TXNS_INTERFACE
               SET process_status = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_RUNNING, request_id = l_request_id, last_update_date = SYSDATE, last_updated_by = l_user_id, GROUP_ID = NULL
             WHERE process_status = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_PENDING;
      
            /*avoid contention with other running Batch Action Import Manager concurrent requests. */
            COMMIT;
      
            derive_int_id_columns(p_request_id => l_request_id, x_return_message => l_return_msg, x_return_status => l_return_status);
      
            IF l_return_status <> fnd_api.g_ret_sts_success THEN
                  log(l_return_msg);
                  l_conc_status := FND_CONCURRENT.SET_COMPLETION_STATUS('ERROR', l_return_msg);
                  log(l_return_msg);
                  RETURN;
            END IF;
            
            IF g_validation_has_error then  
            	l_conc_status := FND_CONCURRENT.SET_COMPLETION_STATUS('WARNING', fnd_message.get_string('GME', 'GME_INTF_CP_WARN'));           
        		end if;
      
            log('finish derive internal id');
      
            log(' multiple worker division and lauch worker starting ...');
            /* Multiple worker division logic*/
      
            l_no_of_workers := 0;
      
            SELECT COUNT(*)
              INTO l_no_of_rows_processing
              FROM GME_RESOURCE_TXNS_INTERFACE
             WHERE process_status = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_RUNNING
               AND int_batch_id IS NOT NULL
               AND request_id = l_request_id;
      
            IF l_no_of_rows_processing =0 THEN
                 log('no record is pending for processing, exiting concurrent program.');
                  return;
            END IF;
             
            
            IF l_no_of_rows_processing <= p_max_rows_of_per_worker THEN
                  l_no_of_workers := 1;
            END IF;
      
            IF l_no_of_workers = 1 THEN
                  --single worker scenario  --start of  bookmark#1
                   /* l_group_id := GME_INTERFACE_GROUP_ID_S.NEXTVAL; 
         Bug 19854404, fix the issue not supported by 10g
         */ 

        select GME_INTERFACE_GROUP_ID_S.NEXTVAL into  l_group_id from dual; 

                  UPDATE GME_RESOURCE_TXNS_INTERFACE
                     SET GROUP_ID = l_group_id
                   WHERE process_status = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_RUNNING
                     AND int_batch_id IS NOT NULL
                     AND request_id = l_request_id;
                  --lauch worker here by calling worker current program
                  log('launch one workker,and group_id=' || l_group_id);
                  l_worker_request_id := FND_REQUEST.SUBMIT_REQUEST(application => 'GME', program => 'GMERCSW', sub_request => FALSE, argument1 => l_group_id, argument2 => p_purge_flag); --this equals to call procedure launch_rsc_txn_worker
            
                  IF l_worker_request_id = 0 THEN
                        ROLLBACK;
                        l_conc_status := FND_CONCURRENT.SET_COMPLETION_STATUS('ERROR', fnd_message.get_string('GME', 'GME_LAUNCH_WORKER_FAIL'));
                        log(fnd_message.get_string('GME', 'GME_LAUNCH_WORKER_FAIL'));
                        returnCode    := GME_BATCH_OPEN_INTERFACE. gme_log_interface_err(P_GROUP_ID          => l_group_id,
                                                                                           P_object_type       => NULL,
                                                                                           P_INTERFACE_ID      => '-1',
                                                                                           P_COLUMN_NAME       => NULL,
                                                                                           P_MESSAGE_TYPE      => GME_BATCH_OPEN_INTERFACE.MESSAGE_TYPE_ERROR,
                                                                                           P_MESSAGE_NAME      => NULL,
                                                                                           P_MESSAGE_TEXT      => fnd_message.get_string('GME', 'GME_LAUNCH_WORKER_FAIL'),
                                                                                           P_SOURCE_TABLE_NAME => 'GME_RESOURCE_TXNS_INTERFACE',
                                                                                           P_APP_NAME => l_app_short_name);
                  END IF;
            ELSE
                  -- multi workers scenario
                  /*Get no. of distinct batches being processed*/
                  SELECT COUNT(DISTINCT int_batch_id)
                    INTO l_no_distinct_batch
                    FROM GME_RESOURCE_TXNS_INTERFACE
                   WHERE process_status = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_RUNNING
                     AND int_batch_id IS NOT NULL
                     AND request_id = l_request_id;
                  log('count of distinct batches being processed is:' || l_no_distinct_batch);
                  /*
                  no. of workers to be launched = MIN(p_max_no_of_worker, CEIL (l_no_of_rows_processing / p_max_rows_of_per_worker ))
                  */
            
                  l_no_of_workers := p_max_no_of_worker;
                  IF l_no_of_rows_processing < p_max_no_of_worker * p_max_rows_of_per_worker THEN
                        l_no_of_workers := CEIL(l_no_of_rows_processing / p_max_rows_of_per_worker);
                  END IF;
            
                  IF l_no_distinct_batch <= l_no_of_workers THEN
                        --batch count is less than max no of workers.  
                        
                        l_no_of_workers := l_no_distinct_batch;
                     log('batch count is less than max no of workers, l_no_distinct_batch=' || l_no_distinct_batch );
                     log('the number of worker  to be lauched is: ' || l_no_of_workers);
                        OPEN cursor_distinct_batch(l_request_id);
                  
                        LOOP
                              FETCH cursor_distinct_batch
                                    INTO l_int_batch_id;
                        
                              IF cursor_distinct_batch%NOTFOUND THEN
                                    EXIT;
                              END IF;
                        
                               /* l_group_id := GME_INTERFACE_GROUP_ID_S.NEXTVAL; 
         Bug 19854404, fix the issue not supported by 10g
         */ 

        select GME_INTERFACE_GROUP_ID_S.NEXTVAL into  l_group_id from dual; 
                        
                              UPDATE GME_RESOURCE_TXNS_INTERFACE
                                 SET GROUP_ID = l_group_id
                               WHERE int_batch_id = l_int_batch_id
                                 AND process_status = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_RUNNING
                                 AND request_id = l_request_id;
                              --lauch worker here by calling worker current program
                              l_worker_request_id := FND_REQUEST.SUBMIT_REQUEST(application => 'GME', program => 'GMERCSW', sub_request => FALSE, argument1 => l_group_id, argument2 => p_purge_flag); --this equals to call procedure launch_rsc_txn_worker
                        
                              IF l_worker_request_id = 0 THEN
                                    ROLLBACK; 
                                    l_conc_status := FND_CONCURRENT.SET_COMPLETION_STATUS('ERROR', fnd_message.get_string('GME', 'GME_LAUNCH_WORKER_FAIL'));
                                    log(fnd_message.get_string('GME', 'GME_LAUNCH_WORKER_FAIL'));
                                    returnCode    := GME_BATCH_OPEN_INTERFACE. gme_log_interface_err(p_group_id          => l_group_id,
                                                                                                       p_object_type       => NULL,
                                                                                                       p_interface_id      => '-1',
                                                                                                       p_column_name       => NULL,
                                                                                                       p_message_type      => GME_BATCH_OPEN_INTERFACE.MESSAGE_TYPE_ERROR,
                                                                                                       p_message_name      => NULL,
                                                                                                       p_message_text      => fnd_message.get_string('GME', 'GME_LAUNCH_WORKER_FAIL'),
                                                                                                       p_source_table_name => 'GME_RESOURCE_TXNS_INTERFACE',
                                                                                                       P_APP_NAME => l_app_short_name);
                              END IF;
                        END LOOP;
                  
                        CLOSE cursor_distinct_batch;
                  
                  ELSE
                        --batch count is more than max no of workers.
                        --start of  bookmark#2
                        log('batch count is more than max no of workers, l_no_distinct_batch=' || l_no_distinct_batch || ' l_no_of_workers:=' || l_no_of_workers);
                        log('the number of worker to be lauched is'||l_no_of_workers);
                        l_batch_count_per_worker := FLOOR(l_no_distinct_batch / l_no_of_workers);
                  
                        l_mod_val := MOD(l_no_distinct_batch, l_no_of_workers);
                  
                        FOR l_worker_index IN 1 .. l_no_of_workers LOOP
                              --for each worker,assign one group_id
                               /* l_group_id := GME_INTERFACE_GROUP_ID_S.NEXTVAL; 
         Bug 19854404, fix the issue not supported by 10g
         */ 

        select GME_INTERFACE_GROUP_ID_S.NEXTVAL into  l_group_id from dual; 
                        
                              IF l_worker_index <= l_mod_val THEN
                                    l_add_one := 1;
                              ELSE
                                    l_add_one := 0;
                              END IF;
                        
                              UPDATE GME_RESOURCE_TXNS_INTERFACE
                                 SET GROUP_ID = l_group_id
                               WHERE int_batch_id IN (SELECT int_batch_id
                                                        FROM (SELECT DISTINCT int_batch_id
                                                                FROM GME_RESOURCE_TXNS_INTERFACE a
                                                               WHERE process_status = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_RUNNING
                                                                 AND GROUP_ID IS NULL
                                                                 AND request_id = l_request_id
                                                               ORDER BY int_batch_id)
                                                       WHERE ROWNUM <= (l_batch_count_per_worker + l_add_one))
                                 AND process_status = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_RUNNING
                                 AND request_id = l_request_id
                                 AND GROUP_ID IS NULL;
                              --lauch worker here by calling worker current program
                        
                              log('lauching multiple worker , group_id:=' || l_group_id);
                              l_worker_request_id := FND_REQUEST.SUBMIT_REQUEST(application => 'GME', program => 'GMERCSW', sub_request => FALSE, argument1 => l_group_id, argument2 => p_purge_flag); --this equals to call procedure launch_rsc_txn_worker
                        
                              IF l_worker_request_id = 0 THEN
                                    ROLLBACK;
                                    l_conc_status := FND_CONCURRENT.SET_COMPLETION_STATUS('ERROR', fnd_message.get_string('GME', 'GME_LAUNCH_WORKER_FAIL'));
                                    log(fnd_message.get_string('GME', 'GME_LAUNCH_WORKER_FAIL'));
                                    returnCode    := GME_BATCH_OPEN_INTERFACE. gme_log_interface_err(P_GROUP_ID          => l_group_id,
                                                                                                       P_object_type       => NULL,
                                                                                                       P_INTERFACE_ID      => '-1',
                                                                                                       P_COLUMN_NAME       => NULL,
                                                                                                       P_MESSAGE_TYPE      => GME_BATCH_OPEN_INTERFACE.MESSAGE_TYPE_ERROR,
                                                                                                       P_MESSAGE_NAME      => NULL,
                                                                                                       P_MESSAGE_TEXT      => fnd_message.get_string('GME', 'GME_LAUNCH_WORKER_FAIL'),
                                                                                                       P_SOURCE_TABLE_NAME => 'GME_RESOURCE_TXNS_INTERFACE',
                                                                                                       P_APP_NAME => l_app_short_name);
                              END IF;
                        END LOOP;
                  END IF; --end of #bookmark2
            END IF; --end of #bookmark1
            log(' multiple worker division and lauch worker are ended.');
            
           
            COMMIT;
      EXCEPTION
            WHEN OTHERS THEN
                  ret_code      := 1;
                  err_buf       := g_pkg_name || '.process_resource_transactions' || SUBSTR(SQLERRM, 1, 240);
                  l_conc_status := FND_CONCURRENT.SET_COMPLETION_STATUS('ERROR', err_buf);
                  log(err_buf);
                  returnCode    := GME_BATCH_OPEN_INTERFACE.gme_log_interface_err(P_GROUP_ID          => '-1',
                                                                                    P_object_type       => NULL,
                                                                                    P_INTERFACE_ID      => '-1',
                                                                                    P_COLUMN_NAME       => NULL,
                                                                                    P_MESSAGE_TYPE      => GME_BATCH_OPEN_INTERFACE.MESSAGE_TYPE_ERROR,
                                                                                    P_MESSAGE_NAME      => NULL,
                                                                                    P_MESSAGE_TEXT      => err_buf,
                                                                                    P_SOURCE_TABLE_NAME => 'GME_RESOURCE_TXNS_INTERFACE',
                                                                                    P_APP_NAME => l_app_short_name);
                  COMMIT;
                  raise;
      END process_resource_transactions;

      /*##########################################################################
      # PROCEDURE
      #   insert_actual_rsrc_txn
      # DESCRIPTION
      # insert resource transactions.
      # HISTORY
      #  Created : 1/24/2013  michael
      #     this procedure is modified from procedure
      #    gme_api_pub.insert_incr_actual_rsrc_txn and gme_api_pub.insert_timed_actual_rsrc_txn,
      #    This procedure combine these two procedure to this one procedure.
      #########################################################################*/

      PROCEDURE insert_actual_rsrc_txn(p_init_msg_list         IN VARCHAR2 := fnd_api.g_false,
                                       p_commit                IN VARCHAR2 := fnd_api.g_false,
                                       p_org_code              IN VARCHAR2,
                                       p_rsrc_txn_rec          IN gme_resource_txns%ROWTYPE,
                                       p_batchstep_resource_id IN NUMBER,
                                       x_rsrc_txn_rec          IN OUT NOCOPY gme_resource_txns%ROWTYPE,
                                       x_message_count         OUT NOCOPY NUMBER,
                                       x_message_list          OUT NOCOPY VARCHAR2,
                                       x_return_status         OUT NOCOPY VARCHAR2) IS
            l_batch_header           gme_batch_header%ROWTYPE;
            l_step_resources         gme_batch_step_resources%ROWTYPE;
            l_batch_step_rsrc_rec_in gme_batch_step_resources%ROWTYPE;
            l_batch_step_rsrc_rec    gme_batch_step_resources%ROWTYPE;
      
            l_step_status      NUMBER;
            l_rsrc_trans_count NUMBER;
            l_instance_id      NUMBER;
            l_reason_id        NUMBER;
            l_line_id          NUMBER;
      
            l_return_status VARCHAR2(2);
            l_resources     VARCHAR2(16);
            l_usage_uom     VARCHAR2(4);
            l_txn_usage     NUMBER;
            l_usage_time    NUMBER;
            l_hour_um       sy_uoms_mst.um_code%TYPE;
            l_rsrc_txn_rec  gme_resource_txns%ROWTYPE;
      
            l_trans_date DATE;
            /*                                    
            BEGIN ER 19161894                    
            Shaliu Chen 18-JUL-2014               
            Added for OPM Step Level OSP Project  
            */               
            l_rsrc_txn_count NUMBER;
            l_new_rsrc_txn_usage NUMBER;
            l_parent_rsrc_txn_rec  gme_resource_txns%ROWTYPE;
            l_reverse_rsrc_txn_rec gme_resource_txns%ROWTYPE;
            
            CURSOR get_parent_resource_txn(v_rcv_txn_id NUMBER,v_doc_id NUMBER) IS
              SELECT *
                FROM gme_resource_txns
               WHERE doc_id = v_doc_id
                 AND NVL(rcv_transaction_id,0) = v_rcv_txn_id
                 AND reverse_id IS NULL;                        
            /*END ER 19161894*/
            
            CURSOR cur_fetch_resource_dtl(v_line_id NUMBER) IS
                  SELECT resources, usage_um FROM gme_batch_step_resources WHERE batchstep_resource_id = v_line_id;
      
            validation_failure EXCEPTION;
            uom_conversion_err EXCEPTION;
            missing_profile_option EXCEPTION;
            rsrc_fetch_err EXCEPTION;
            rsrc_update_err EXCEPTION;
            rsrc_txn_insert_err EXCEPTION;
            reduce_pend_usage_err EXCEPTION;
            error_load_trans EXCEPTION;
      
            gmf_cost_failure EXCEPTION;
            rsrc_txn_fetch_err EXCEPTION;
            rsrcid_not_found EXCEPTION;
            /*                                    
            BEGIN ER 19161894                    
            Shaliu Chen 18-JUL-2014               
            Added for OPM Step Level OSP Project  
            */                
            parent_rsrc_txn_not_found     EXCEPTION;
            multi_parent_rsrc_txn         EXCEPTION;
            rsrc_txn_update_err           EXCEPTION;
            /*END ER 19161894*/            
            l_msg_count NUMBER;
            l_msg_data  VARCHAR2(2000);
            batch_hdr_fetch_err EXCEPTION;
            l_period_id INTEGER;
      BEGIN
            /* Set the savepoint */
            SAVEPOINT sp_insert_rsrc_txn;
      
            /* Set the return status to success initially */
            x_return_status := fnd_api.g_ret_sts_success;
      
            /* Initialize message list and count if needed */
            IF p_init_msg_list = fnd_api.g_true THEN
                  fnd_msg_pub.initialize;
                  gme_common_pvt.g_error_count := 0;
            END IF;
      
            l_rsrc_txn_rec := p_rsrc_txn_rec;
      
            gme_common_pvt.g_setup_done := gme_common_pvt.setup(p_org_id => l_rsrc_txn_rec.organization_id, p_org_code => p_org_code);
      
            IF NOT gme_common_pvt.g_setup_done THEN
                  RAISE fnd_api.g_exc_error;
            ELSE
                  l_rsrc_txn_rec.organization_id := gme_common_pvt.g_organization_id;
            END IF;
      
            gme_common_pvt.set_timestamp;
      
            ----------------------calling private API start -------------------------------------------
      
            gme_validate_flex_fld_pvt. validate_rsrc_txn_flex(p_resource_txn_rec => p_rsrc_txn_rec, x_resource_txn_rec => x_rsrc_txn_rec, x_return_status => x_return_status);
      
            IF x_return_status <> fnd_api.g_ret_sts_success THEN
                  RAISE validation_failure;
            END IF;
      
            l_rsrc_txn_rec := x_rsrc_txn_rec;
      
            /*---replacement for validate_rsrc_txn_param begin----*/
      
            l_batch_step_rsrc_rec_in.batchstep_resource_id := p_batchstep_resource_id;
      
            IF NOT gme_common_pvt.get_batchstep_rsrc(p_batchstep_rsrc_rec => l_batch_step_rsrc_rec_in,
                                                     p_org_code           => NULL,
                                                     p_batch_no           => NULL,
                                                     p_batchstep_no       => NULL,
                                                     p_activity           => NULL,
                                                     p_resource           => NULL,
                                                     x_batchstep_rsrc_rec => l_batch_step_rsrc_rec) THEN
                  RAISE rsrcid_not_found;
            END IF;
      
            l_line_id               := l_batch_step_rsrc_rec.batchstep_resource_id;
            l_batch_header.batch_id := l_batch_step_rsrc_rec.batch_id;
      
            IF NOT gme_batch_header_dbl. fetch_row(p_batch_header => l_batch_header, x_batch_header => l_batch_header) THEN
                  RAISE batch_hdr_fetch_err;
            END IF;
      
            l_instance_id := p_rsrc_txn_rec.instance_id;
            l_reason_id   := p_rsrc_txn_rec.reason_id;
      
            IF NOT gme_common_pvt.
             close_period_check_flexible(p_org_id => l_batch_step_rsrc_rec.organization_id, p_trans_date => p_rsrc_txn_rec.trans_date, x_trans_date => l_trans_date, x_period_id => l_period_id) THEN
                  RAISE fnd_api.g_exc_error;
            END IF;
      
            /*---replacement for validate_rsrc_txn_param end----*/
      
            IF p_rsrc_txn_rec.resource_usage IS NULL THEN
                  --resource_usage is null condition.
                  /*  V. Ajay Kumar  Bug 3041705. Multiplied the difference of
                  dates by 24 to convert it to hours */
                  l_usage_time := (p_rsrc_txn_rec.end_date - p_rsrc_txn_rec.start_date) * 24;
                  l_hour_um    := fnd_profile. value_specific(NAME => 'BOM:HOUR_UOM_CODE', user_id => gme_common_pvt.g_user_ident);
            
                  IF (l_hour_um IS NULL) THEN
                        gme_common_pvt. log_message('GME_API_UNABLE_TO_GET_CONSTANT', 'CONSTANT_NAME', 'BOM:HOUR_UOM_CODE');
                        RAISE missing_profile_option;
                  END IF;
            END IF;
      
            OPEN cur_fetch_resource_dtl(l_line_id);
      
            FETCH cur_fetch_resource_dtl
                  INTO l_resources, l_usage_uom;
      
            CLOSE cur_fetch_resource_dtl;
      
            IF p_rsrc_txn_rec.resource_usage IS NULL THEN
                  --resource_usage is null condition.
                  /* siva from_name and to_name made null*/
                  IF l_hour_um <> l_rsrc_txn_rec.trans_qty_um THEN
                        l_txn_usage := inv_convert.inv_um_convert(item_id       => 0,
                                                                  PRECISION     => 5,
                                                                  from_quantity => l_usage_time,
                                                                  from_unit     => l_hour_um,
                                                                  to_unit       => l_rsrc_txn_rec.trans_qty_um,
                                                                  from_name     => NULL,
                                                                  to_name       => NULL);
                  
                        IF (l_txn_usage = -99999) THEN
                              gme_common_pvt.log_message('GME_RSRC_USG_NT_CNV_SYUOM', 'SY_UOM', l_hour_um, 'RSRC_USG_UOM', l_usage_uom);
                              RAISE uom_conversion_err;
                        END IF;
                  ELSE
                        l_txn_usage := l_usage_time;
                  END IF;
            END IF;
      
            -- construct record for insertion
            l_rsrc_txn_rec.doc_id := l_batch_header.batch_id;
            /*  hard coding doc_type to PROD as rsrc txn can only exist for batchesi not FPO
            later if we decide to have this functionality for FPOs we would have to change code to
            get batch_type from l_batch_header variable */
            l_rsrc_txn_rec.doc_type        := 'PROD';
            l_rsrc_txn_rec.line_type       := 0;
            l_rsrc_txn_rec.organization_id := l_batch_header.organization_id;
            l_rsrc_txn_rec.line_id         := l_line_id;
            l_rsrc_txn_rec.event_id        := gme_common_pvt.g_transaction_header_id;
            l_rsrc_txn_rec.resources       := l_resources;
      
            IF p_rsrc_txn_rec.resource_usage IS NULL THEN
                  l_rsrc_txn_rec.resource_usage := l_txn_usage;
            ELSE
                  --resource_usage is null condition.
                  l_rsrc_txn_rec.resource_usage := p_rsrc_txn_rec.resource_usage;
            END IF;
      
            l_rsrc_txn_rec.trans_qty_um  := l_usage_uom;
            l_rsrc_txn_rec.trans_date    := l_trans_date;
            l_rsrc_txn_rec.completed_ind := 1;
            l_rsrc_txn_rec.posted_ind    := 0;
      
            --Bug#6154309 only for ASQC batches overrided_protected_ind should be yes.
            /*
             ER 19161894                    
             Shaliu Chen 18-JUL-2014              
             Modified by Shaliu for OSP Project
             Only the batch is ASQC enabled and the batch is Non-OSP batch,
             overrided_protected_ind column value is Y,otherwise,the 
             overrided_protected_ind column value is N
            */            
            IF (l_batch_header.automatic_step_calculation = 1 AND
                NOT gme_osp.is_osp_batch(p_batch_id     => l_batch_step_rsrc_rec.batch_id,
                                         p_batchstep_id => l_batch_step_rsrc_rec.batchstep_id))THEN
                  l_rsrc_txn_rec.overrided_protected_ind := 'Y';
            ELSE
                  l_rsrc_txn_rec.overrided_protected_ind := 'N';
            END IF;
      
            l_rsrc_txn_rec.reason_id   := l_reason_id;
            l_rsrc_txn_rec.start_date  := p_rsrc_txn_rec.start_date;
            l_rsrc_txn_rec.end_date    := p_rsrc_txn_rec.end_date;
            l_rsrc_txn_rec.delete_mark := 0;
            l_rsrc_txn_rec.text_code   := 0;
      
            IF p_rsrc_txn_rec.instance_id IS NOT NULL THEN
                  l_rsrc_txn_rec.instance_id := p_rsrc_txn_rec.instance_id;
            ELSE
                  l_rsrc_txn_rec.instance_id := l_instance_id;
            END IF;
      
            l_rsrc_txn_rec.sequence_dependent_ind := 0;
            
           /*
              Populate new columns added for OPM OSP
            */
            IF p_rsrc_txn_rec.rcv_transaction_id IS NOT NULL THEN
              l_rsrc_txn_rec.cost_source              := p_rsrc_txn_rec.cost_source             ;
              l_rsrc_txn_rec.po_header_id             := p_rsrc_txn_rec.po_header_id            ;
              l_rsrc_txn_rec.po_line_id               := p_rsrc_txn_rec.po_line_id              ;
              l_rsrc_txn_rec.actual_resource_rate     := p_rsrc_txn_rec.actual_resource_rate    ;
              l_rsrc_txn_rec.currency_code            := p_rsrc_txn_rec.currency_code           ;
              l_rsrc_txn_rec.currency_conversion_date := p_rsrc_txn_rec.currency_conversion_date;
              l_rsrc_txn_rec.currency_conversion_type := p_rsrc_txn_rec.currency_conversion_type;
              l_rsrc_txn_rec.currency_conversion_rate := p_rsrc_txn_rec.currency_conversion_rate;
              l_rsrc_txn_rec.rcv_transaction_id       := p_rsrc_txn_rec.rcv_transaction_id      ;
              
              /*
               check there is same transaction_id record in the gme_resource_txns table
               If have,that means the importing record is generated by receipt correction
               or receipt return actions.
              */
              SELECT count(1)
                INTO l_rsrc_txn_count
                FROM gme_resource_txns
               WHERE NVL(rcv_transaction_id,0) = p_rsrc_txn_rec.rcv_transaction_id;
               
               
            ELSE
              l_rsrc_txn_count := 0;   
              
            END IF;
            /*
             If the record generated by receipt correction or receipt
             return , we need to import 2 records into gme_resource_txns table
             instead of the correction/return qty is positive or negative,
             the first record is used to reverse the parent resource 
             txn record exist in the gme_resource_txns table, the other
             record is new resource txn record with new resource usage
             after receipt correction/return. 
            */
            IF (p_rsrc_txn_rec.rcv_transaction_id IS NOT NULL 
                AND l_rsrc_txn_count >=1) THEN
                
              OPEN get_parent_resource_txn(p_rsrc_txn_rec.rcv_transaction_id,l_rsrc_txn_rec.doc_id);
              FETCH get_parent_resource_txn INTO l_parent_rsrc_txn_rec;
              IF get_parent_resource_txn%NOTFOUND THEN
                gme_common_pvt.log_message('GME_PARENT_RSRC_TXN_NOT_FOUND');
                RAISE parent_rsrc_txn_not_found;  
              END IF;
              IF get_parent_resource_txn%ROWCOUNT >1 THEN
                gme_common_pvt.log_message('GME_MULTI_PARENT_RSRC_TXN');
                RAISE multi_parent_rsrc_txn; 
              END IF;
              l_reverse_rsrc_txn_rec := l_parent_rsrc_txn_rec;
              
              l_reverse_rsrc_txn_rec.resource_usage           := -1*l_parent_rsrc_txn_rec.resource_usage;
              /*
               for reversal resource txn,the trans_date and start_date is equal to the parent resource
               txn trans_date and start_date.
              */
              l_reverse_rsrc_txn_rec.end_date                 := l_rsrc_txn_rec.end_date                ;
              l_reverse_rsrc_txn_rec.reverse_id               := l_parent_rsrc_txn_rec.poc_trans_id     ;              
              l_reverse_rsrc_txn_rec.cost_source              := p_rsrc_txn_rec.cost_source             ;
              l_reverse_rsrc_txn_rec.po_header_id             := p_rsrc_txn_rec.po_header_id            ;
              l_reverse_rsrc_txn_rec.po_line_id               := p_rsrc_txn_rec.po_line_id              ;
              l_reverse_rsrc_txn_rec.actual_resource_rate     := p_rsrc_txn_rec.actual_resource_rate    ;
              l_reverse_rsrc_txn_rec.currency_code            := p_rsrc_txn_rec.currency_code           ;
              l_reverse_rsrc_txn_rec.currency_conversion_date := p_rsrc_txn_rec.currency_conversion_date;
              l_reverse_rsrc_txn_rec.currency_conversion_type := p_rsrc_txn_rec.currency_conversion_type;
              l_reverse_rsrc_txn_rec.currency_conversion_rate := p_rsrc_txn_rec.currency_conversion_rate;
              l_reverse_rsrc_txn_rec.rcv_transaction_id       := p_rsrc_txn_rec.rcv_transaction_id      ;              
              
              /*
               insert reverse resource txn record
              */
              IF NOT (gme_resource_txns_dbl.insert_row(l_reverse_rsrc_txn_rec, x_rsrc_txn_rec)) THEN
                RAISE rsrc_txn_insert_err;
              END IF;  
              
              --l_rsrc_txn_rec := x_rsrc_txn_rec;

              IF NOT (gme_resource_txns_dbl.fetch_row(x_rsrc_txn_rec, x_rsrc_txn_rec)) THEN
                    RAISE rsrc_txn_fetch_err;
              END IF;

              GMF_LAYERS.Create_Resource_Layers(p_api_version   => 1.0,
                                                p_init_msg_list => FND_API.G_FALSE,
                                                p_rsrc_rec      => x_rsrc_txn_rec,
                                                p_doc_qty       => x_rsrc_txn_rec.resource_usage,
                                                p_doc_um        => x_rsrc_txn_rec.trans_qty_um,
                                                x_return_status => l_return_status,
                                                x_msg_count     => l_msg_count,
                                                x_msg_data      => l_msg_data);

              IF l_return_status <> FND_API.G_RET_STS_SUCCESS THEN
                    RAISE gmf_cost_failure;
              END IF;

              l_step_resources.batchstep_resource_id := x_rsrc_txn_rec.line_id;

              IF NOT gme_batch_step_resources_dbl. fetch_row(p_batch_step_resources => l_step_resources, x_batch_step_resources => l_step_resources) THEN
                    RAISE rsrc_fetch_err;
              END IF;

              IF p_rsrc_txn_rec.resource_usage IS NULL THEN
                    --resource_usage is null condition.
                    l_step_resources.actual_rsrc_usage := NVL(l_step_resources.actual_rsrc_usage, 0) + l_txn_usage;
              ELSE
                    l_step_resources.actual_rsrc_usage := NVL(l_step_resources.actual_rsrc_usage, 0) + x_rsrc_txn_rec.resource_usage;
              END IF;

              IF l_step_status = 2 THEN
                    /* Lets now load the transactions associated with the batch into the temporary tblle */
                    gme_trans_engine_util. load_rsrc_trans(p_batch_row => l_batch_header, x_rsc_row_count => l_rsrc_trans_count, x_return_status => l_return_status);

                    IF l_return_status <> x_return_status THEN
                          RAISE error_load_trans;
                    END IF;

                    gme_update_step_qty_pvt. reduce_pending_usage(p_batch_step_resources_rec => l_step_resources, x_return_status => x_return_status);

                    IF x_return_status <> 'S' THEN
                          RAISE reduce_pend_usage_err;
                    END IF;
              END IF;

              IF NOT gme_batch_step_resources_dbl.update_row(p_batch_step_resources => l_step_resources) THEN
                    RAISE rsrc_update_err;
              END IF;              
              
              /*              
               populate reverse_id column for parent resource txn record
              */                  
              l_parent_rsrc_txn_rec.reverse_id := x_rsrc_txn_rec.poc_trans_id;
                        
              IF NOT (gme_resource_txns_dbl.update_row(l_parent_rsrc_txn_rec)) THEN
                RAISE rsrc_txn_update_err;
              END IF;
              
              /*               
              insert new resource txn record
              */       
              l_rsrc_txn_rec.resource_usage := l_parent_rsrc_txn_rec.resource_usage+l_rsrc_txn_rec.resource_usage;
                          
              IF NOT (gme_resource_txns_dbl.insert_row(l_rsrc_txn_rec, x_rsrc_txn_rec)) THEN
                RAISE rsrc_txn_insert_err;
              END IF;                
                       
              
            ELSE            
      
              IF NOT (gme_resource_txns_dbl.insert_row(l_rsrc_txn_rec, x_rsrc_txn_rec)) THEN
                    RAISE rsrc_txn_insert_err;
              END IF;
            END IF;
            /*END ER 19161894*/
            l_rsrc_txn_rec := x_rsrc_txn_rec;
      
            -- Bug 9506856  - Fetch the record directly from the DB to get all column values.
            IF NOT (gme_resource_txns_dbl.fetch_row(l_rsrc_txn_rec, l_rsrc_txn_rec)) THEN
                  RAISE rsrc_txn_fetch_err;
            END IF;
      
            -- Bug 9506856  - Make call to GMF for actual costing
            GMF_LAYERS.Create_Resource_Layers(p_api_version   => 1.0,
                                              p_init_msg_list => FND_API.G_FALSE,
                                              p_rsrc_rec      => l_rsrc_txn_rec,
                                              p_doc_qty       => l_rsrc_txn_rec.resource_usage,
                                              p_doc_um        => l_rsrc_txn_rec.trans_qty_um,
                                              x_return_status => l_return_status,
                                              x_msg_count     => l_msg_count,
                                              x_msg_data      => l_msg_data);
      
            IF l_return_status <> FND_API.G_RET_STS_SUCCESS THEN
                  RAISE gmf_cost_failure;
            END IF;
      
            -- update resource actual count and usage
            -- l_step_resources.batchstep_resource_id := l_resource_txns.line_id;
            l_step_resources.batchstep_resource_id := l_rsrc_txn_rec.line_id;
      
            IF NOT gme_batch_step_resources_dbl. fetch_row(p_batch_step_resources => l_step_resources, x_batch_step_resources => l_step_resources) THEN
                  RAISE rsrc_fetch_err;
            END IF;
      
            IF p_rsrc_txn_rec.resource_usage IS NULL THEN
                  --resource_usage is null condition.
                  l_step_resources.actual_rsrc_usage := NVL(l_step_resources.actual_rsrc_usage, 0) + l_txn_usage;
            ELSE
                  l_step_resources.actual_rsrc_usage := NVL(l_step_resources.actual_rsrc_usage, 0) + l_rsrc_txn_rec.resource_usage;
            END IF;
      
            IF l_step_status = 2 THEN
                  /* Lets now load the transactions associated with the batch into the temporary tblle */
                  gme_trans_engine_util. load_rsrc_trans(p_batch_row => l_batch_header, x_rsc_row_count => l_rsrc_trans_count, x_return_status => l_return_status);
            
                  IF l_return_status <> x_return_status THEN
                        RAISE error_load_trans;
                  END IF;
            
                  gme_update_step_qty_pvt. reduce_pending_usage(p_batch_step_resources_rec => l_step_resources, x_return_status => x_return_status);
            
                  IF x_return_status <> 'S' THEN
                        RAISE reduce_pend_usage_err;
                  END IF;
            END IF;
      
            IF NOT gme_batch_step_resources_dbl.update_row(p_batch_step_resources => l_step_resources) THEN
                  RAISE rsrc_update_err;
            END IF;
      
            ----------------------calling private API end-------------------------------------------
      
            IF x_return_status = fnd_api.g_ret_sts_success THEN
                  IF p_commit = fnd_api.g_true THEN
                        gme_api_pub.save_batch(p_header_id => NULL, p_table => NULL, p_commit => fnd_api.g_false, x_return_status => x_return_status);
                  
                        IF x_return_status = fnd_api.g_ret_sts_success THEN
                              COMMIT;
                        ELSE
                              RAISE fnd_api.g_exc_error;
                        END IF;
                  END IF;
            ELSE
                  RAISE fnd_api.g_exc_error;
            END IF;
      
            gme_common_pvt. count_and_get(x_count => x_message_count, p_encoded => fnd_api.g_false, x_data => x_message_list);
      EXCEPTION
            WHEN fnd_api.g_exc_error OR validation_failure OR gmf_cost_failure OR rsrc_txn_fetch_err OR missing_profile_option OR error_load_trans OR rsrc_fetch_err OR rsrc_update_err OR
                 reduce_pend_usage_err THEN
                  ROLLBACK TO SAVEPOINT sp_insert_rsrc_txn;
                  x_return_status := fnd_api.g_ret_sts_error;
                  gme_common_pvt.count_and_get(x_count => x_message_count, p_encoded => fnd_api.g_false, x_data => x_message_list);
            /*                                    
            ER 19161894                    
            Shaliu Chen 18-JUL-2014               
            Added for OPM Step Level OSP Project  
            */                   
            WHEN parent_rsrc_txn_not_found OR multi_parent_rsrc_txn OR rsrc_txn_update_err THEN
                ROLLBACK TO SAVEPOINT sp_insert_rsrc_txn;
                x_return_status := fnd_api.g_ret_sts_error;   
                gme_common_pvt.count_and_get(x_count => x_message_count, p_encoded => fnd_api.g_false, x_data => x_message_list);     
                  
            WHEN OTHERS THEN
                  ROLLBACK TO SAVEPOINT sp_insert_rsrc_txn;
                  x_return_status := fnd_api.g_ret_sts_error;
                  gme_common_pvt.count_and_get(x_count => x_message_count, p_encoded => fnd_api.g_false, x_data => x_message_list);
      END insert_actual_rsrc_txn;

END GME_RESOURCE_TXN_OPEN_INTF;
/
COMMIT ;
EXIT;   
