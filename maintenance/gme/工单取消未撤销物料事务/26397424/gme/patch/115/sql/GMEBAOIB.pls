/* +======================================================================+ */
/* |    Copyright (c) 2005, 2015 Oracle and/or its affiliates.           | */
/* |                         All rights reserved.                         | */
/* |                           Version 12.0.0                             | */
/* +======================================================================+ */
REM dbdrv: sql ~PROD ~PATH ~FILE none none none package &phase=plb \
REM dbdrv: checkfile(120.0.12010000.22=120.2.12020000.20)(115.2=120.1):~PROD:~PATH:~FILE     

create or replace package body GME_BATCH_ACTION_OPEN_INTF AS
/* $Header: GMEBAOIB.pls 120.2.12020000.20 2015/02/04 05:56:54 shalchen noship $ */
  g_debug               VARCHAR2 (5)  := NVL(fnd_profile.VALUE ('AFLOG_LEVEL'),-1);
  g_pkg_name   CONSTANT VARCHAR2 (30) := 'GME_BATCH_ACTION_OPEN_INTF';

 /*================================================================================
  Procedure
    process_batch_actions
  Description
    This procedure is the main program for OPM batch/step actions import open 
    interface,it's used for deriving int columns for GME_BATCH_ACTIONS_INTERFACE
    table,and sumbitting Batch/Step Actions Import Worker concurrent programs.

  Parameters
    p_purge_flag              purge flag,there are 2 values for this paramter
                              Y and N,Y stand for the concurrent program will
                              purge the records which have been import successfully.
                              N stand for the concurrent program will not purge
                              any record.
    p_max_no_of_worker        the max numbers of import worker 
    p_max_rows_of_per_worker  the max process rows for each worker.

  History:
    Shalchen       06-DEC-2012  Creation
================================================================================*/

PROCEDURE process_batch_actions(err_buf                   OUT NOCOPY VARCHAR2,
                                ret_code                  OUT NOCOPY VARCHAR2,
                                p_purge_flag              IN VARCHAR2 DEFAULT 'Y',
                                p_max_no_of_worker        IN NUMBER DEFAULT 5,
                                p_max_rows_of_per_worker  IN NUMBER DEFAULT 1000                                
                                 ) IS
l_request_id              NUMBER;  
l_worker_request_id       NUMBER; 
l_user_id                 NUMBER; 
l_no_of_rows_processing   NUMBER;
l_no_of_workers           NUMBER;
l_no_distinct_batch       NUMBER;
l_int_batch_id            NUMBER;
l_batch_count_per_worker  NUMBER;
l_mod_val                 NUMBER;
l_add_one                 NUMBER;
returnCode                NUMBER;
l_group_id                gme_batch_actions_interface.group_id%TYPE;
l_message_detail          GME_BATCH_OPEN_INTERFACE.T_MESSAGE_TABLE;

l_conc_status             BOOLEAN;

x_return_message          VARCHAR2(2000);
x_return_status           VARCHAR2(100);

submit_request_exception EXCEPTION;
   

CURSOR cursor_distinct_batch (v_request_id NUMBER) IS
   SELECT DISTINCT int_batch_id
     FROM gme_batch_actions_interface
    WHERE process_status IN (GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_RUNNING,GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_BV_WARNING)
      AND request_id = v_request_id
      AND int_batch_id IS NOT NULL
 ORDER BY int_batch_id;   

 
                                 
BEGIN
  fnd_file.put_line(fnd_file.log,'The input parameter of Purge Successful Rows is:'||p_purge_flag);
  fnd_file.put_line(fnd_file.log,'The input parameter of Max Number of Workers is:'||p_max_no_of_worker);
  fnd_file.put_line(fnd_file.log,'The input parameter of Optimal Number of Rows Per Worker is:'||p_max_rows_of_per_worker);

  IF (g_debug <> -1) THEN
     gme_debug.log_initialize ();
  END IF;
   
  
  fnd_msg_pub.initialize;
  
  
  l_request_id := fnd_global.conc_request_id(); 
  l_user_id    := fnd_global.user_id;
  IF g_debug <= gme_debug.g_log_procedure THEN
    gme_debug.put_line('The request id of batch action import manager is:'||l_request_id);
    gme_debug.put_line('The user id which launch batch action import manager is:'||l_user_id);    
  END IF;
  
  --set process_status to 2  to delimit the records which will be processed this time.
  UPDATE gme_batch_actions_interface
     SET process_status   = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_RUNNING,
         request_id       = l_request_id,
         last_update_date = SYSDATE,
         last_updated_by  = l_user_id,
         group_id         = NULL
   WHERE process_status =GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_PENDING;
  --avoid contention with other running Batch Action Import Manager concurrent requests. 
  COMMIT; 
  IF g_debug <= gme_debug.g_log_procedure THEN
    gme_debug.put_line('The total number to be processed this time is'||sql%rowcount); 
  END IF;
  
  --derive int_batch_id and int_batchstep_id and populate into int_batch_id col and int_batchstep_id col
  IF g_debug <= gme_debug.g_log_procedure THEN
    gme_debug.put_line('Entering api:'||g_pkg_name||'.'|| 'derive_id_column');
   
  END IF;  
  derive_int_id_columns(l_request_id,
                        x_return_message ,
                        x_return_status);
  IF x_return_status <> fnd_api.g_ret_sts_success THEN
    IF (g_debug <= gme_debug.g_log_statement) THEN
      gme_debug.put_line (g_pkg_name || '.derive_id_column'
                        || ':derive int_batch_id and int_batchstep_id column value error');
    END IF;
         
    RAISE fnd_api.g_exc_error;        
  END IF;

  SELECT count(1)
    INTO l_no_of_rows_processing
    FROM gme_batch_actions_interface
   WHERE process_status IN (GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_RUNNING,GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_BV_WARNING)
     AND int_batch_id IS NOT NULL
     AND request_id = l_request_id;

  --If there is not any record need to process,set concurrent program status as WARNING and return.   
  IF l_no_of_rows_processing = 0 THEN

   l_conc_status := fnd_concurrent.set_completion_status('NORMAL', '');
   Commit;
   fnd_file.put_line (fnd_file.LOG, 'There is no more records to be processed');      
   Return;
  END IF;
 
  IF g_debug <= gme_debug.g_log_procedure THEN
    gme_debug.put_line('the records need to process in this program is:'||l_no_of_rows_processing||' rows');
  END IF;     
  
  l_no_of_workers := 0; 
 
  --if the process records is less than max rows per worker,then set worker number to 1   
  IF l_no_of_rows_processing <= p_max_rows_of_per_worker THEN
    l_no_of_workers := 1;
 
    IF g_debug <= gme_debug.g_log_procedure THEN
      gme_debug.put_line('the process records is less than max rows per worker,set worker number to 1');
    END IF;  

    /* l_group_id := GME_INTERFACE_GROUP_ID_S.NEXTVAL; 
         Bug 19854404, fix the issue not supported by 10g
         */ 
    select GME_INTERFACE_GROUP_ID_S.NEXTVAL into  l_group_id from dual;
     
    UPDATE gme_batch_actions_interface
       SET group_id = l_group_id
     WHERE process_status IN (GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_RUNNING,GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_BV_WARNING)
       AND int_batch_id IS NOT NULL
       AND group_id IS NULL
       AND request_id = l_request_id;  
  
     l_worker_request_id := fnd_request.submit_request (application => 'GME',
                                                        program     => 'GMEBACTW',
                                                        sub_request => FALSE,
                                                        argument1   => l_group_id,
                                                        argument2   => p_purge_flag);  
     IF l_worker_request_id = 0 THEN

        ROLLBACK;
        l_conc_status := fnd_concurrent.set_completion_status 
                                  ('ERROR', 
                                   fnd_message.get_string('GME','GME_LAUNCH_WORKER_FAIL'));
        returnCode :=gme_log_interface_err (
              p_group_id  => l_group_id,
              P_object_type => -1,
              p_interface_id => -1,
              p_column_name => NULL,
              p_message_type => GME_BATCH_OPEN_INTERFACE.MESSAGE_TYPE_ERROR,
              p_message_name => 'GME_LAUNCH_WORKER_FAIL',
              p_source_table_name => 'GME_BATCH_ACTIONS_INTERFACE',
              p_err_message_detail => l_message_detail);
     ELSE
       IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line('Set group_id to:'||l_group_id||' for all of records need to process');
       END IF; 
       
       COMMIT;
     END IF;                                                             
      
              
  ELSE  -- multi workers
    /*Get no. of distinct batches being processed*/
    SELECT COUNT (DISTINCT int_batch_id)
      INTO l_no_distinct_batch
      FROM gme_batch_actions_interface
     WHERE process_status IN (GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_RUNNING,GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_BV_WARNING)
       AND int_batch_id IS NOT NULL
       AND request_id = l_request_id;
           
    l_no_of_workers := p_max_no_of_worker;
    
    IF l_no_of_rows_processing < p_max_no_of_worker * p_max_rows_of_per_worker
    THEN
       l_no_of_workers := CEIL (l_no_of_rows_processing / p_max_rows_of_per_worker);
    END IF;   
    
    IF l_no_distinct_batch <= l_no_of_workers THEN
      --batch count is less than max no of workers. 
      l_no_of_workers := l_no_distinct_batch;
      OPEN cursor_distinct_batch (l_request_id);
      LOOP     
        FETCH cursor_distinct_batch INTO l_int_batch_id;
        EXIT WHEN cursor_distinct_batch%NOTFOUND;
        
        /* l_group_id := GME_INTERFACE_GROUP_ID_S.NEXTVAL; 
         Bug 19854404, fix the issue not supported by 10g
         */ 

        select GME_INTERFACE_GROUP_ID_S.NEXTVAL into  l_group_id from dual; 
        
        UPDATE gme_batch_actions_interface
           SET group_id = l_group_id
         WHERE int_batch_id = l_int_batch_id
           AND process_status IN (GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_RUNNING,GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_BV_WARNING)
           AND request_id = l_request_id;  
           
        l_worker_request_id :=fnd_request.submit_request (application => 'GME',
                                                          program     => 'GMEBACTW',
                                                          sub_request => FALSE,
                                                          argument1   => l_group_id,
                                                          argument2   => p_purge_flag);   
        IF l_worker_request_id = 0 THEN
          ROLLBACK;
          l_conc_status := fnd_concurrent.set_completion_status
                                  ('ERROR', 
                                   fnd_message.get_string('GME','GME_LAUNCH_WORKER_FAIL'));          
          returnCode :=gme_log_interface_err (
                p_group_id  => l_group_id,
                P_object_type => -1,
                p_interface_id => -1,
                p_column_name => NULL,
                p_message_type => GME_BATCH_OPEN_INTERFACE.MESSAGE_TYPE_ERROR,
                p_message_name => 'GME_LAUNCH_WORKER_FAIL',
                p_source_table_name => 'GME_BATCH_ACTIONS_INTERFACE',
                p_err_message_detail => l_message_detail);
        ELSE
          IF g_debug <= gme_debug.g_log_procedure THEN
            gme_debug.put_line('Set group_id to:'||l_group_id||' for all of records need to process');
          END IF;          
          COMMIT;
       END IF;
     END LOOP;
     CLOSE cursor_distinct_batch;
   ELSE            
     l_batch_count_per_worker := FLOOR (l_no_distinct_batch / l_no_of_workers);
     l_mod_val := MOD (l_no_distinct_batch, l_no_of_workers);  
     FOR l_worker_index IN 1 .. l_no_of_workers LOOP
         IF l_worker_index <= l_mod_val THEN
           l_add_one := 1;
         ELSE
           l_add_one := 0;
         END IF;
          
         /* l_group_id := GME_INTERFACE_GROUP_ID_S.NEXTVAL; 
         Bug 19854404, fix the issue not supported by 10g
         */ 

         select GME_INTERFACE_GROUP_ID_S.NEXTVAL into  l_group_id from dual;

         UPDATE gme_batch_actions_interface
            SET group_id = l_group_id
          WHERE int_batch_id IN (SELECT int_batch_id
                                   FROM ( SELECT DISTINCT int_batch_id
                                            FROM gme_batch_actions_interface a
                                           WHERE process_status IN (GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_RUNNING,
                                                                    GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_BV_WARNING)
                                             AND group_id IS NULL
                                             AND request_id = l_request_id
                                        ORDER BY int_batch_id)
                                      WHERE ROWNUM <= (l_batch_count_per_worker + l_add_one))
                AND process_status IN (GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_RUNNING,GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_BV_WARNING)
                AND request_id = l_request_id
                AND group_id IS NULL; 

         l_worker_request_id :=fnd_request.submit_request (application => 'GME',
                                                           program     => 'GMEBACTW',
                                                           sub_request => FALSE,
                                                           argument1   => l_group_id,
                                                           argument2   => p_purge_flag);   
         IF l_worker_request_id = 0 THEN
           ROLLBACK;
           l_conc_status := fnd_concurrent.set_completion_status 
                                  ('ERROR', 
                                   fnd_message.get_string('GME','GME_LAUNCH_WORKER_FAIL'));              
           returnCode :=gme_log_interface_err (
                 p_group_id  => l_group_id,
                 P_object_type => -1,
                 p_interface_id => -1,
                 p_column_name => NULL,
                 p_message_type => GME_BATCH_OPEN_INTERFACE.MESSAGE_TYPE_ERROR,
                 p_message_name => 'GME_LAUNCH_WORKER_FAIL',
                 p_source_table_name => 'GME_BATCH_ACTIONS_INTERFACE',
                 p_err_message_detail => l_message_detail);
         ELSE
           IF g_debug <= gme_debug.g_log_procedure THEN
             gme_debug.put_line('Set group_id to:'||l_group_id||' for all of records need to process');
           END IF;          
           COMMIT;
        END IF; 
      END LOOP;
    END IF;
  END IF;                                                                                               
  
  
  EXCEPTION
    WHEN fnd_api.g_exc_error THEN     
      ROLLBACK;
      fnd_file.put_line (fnd_file.LOG, 'derive int_batch_id and int_batchstep_id column value error');
      
    WHEN submit_request_exception THEN
      ROLLBACK;
      fnd_file.put_line (fnd_file.LOG, 'There is error while submit child batch action process request');
      
    WHEN OTHERS THEN
      ROLLBACK;
      fnd_file.put_line (fnd_file.LOG, 'There is error occurred,error code:'||sqlcode||
                         ',errror message:'||sqlerrm);
END;             

 /*================================================================================
  Procedure
    derive_int_id_columns
  Description
    This procedure is used for deriving int columns value for GME_BATCH_ACTIONS_INTERFACE
    table.

  Parameters
    p_request_id   The request id of the Manager concurrent request.
  History:
    Shalchen       06-DEC-2012  Creation
================================================================================*/

PROCEDURE derive_int_id_columns(p_request_id            IN             NUMBER
                               ,x_return_message       OUT NOCOPY      VARCHAR2
                               ,x_return_status        OUT NOCOPY      VARCHAR2)
   IS
l_user_id    NUMBER;
BEGIN
  x_return_status := fnd_api.g_ret_sts_success;
  
  l_user_id    := fnd_global.user_id;  

  SAVEPOINT derive_int_id_columns; 
  
  IF g_debug <= gme_debug.g_log_procedure THEN
    gme_debug.put_line('Start to derive int_batch_id while batch_id is not null for batch action type'); 
  END IF;
    
  UPDATE gme_batch_actions_interface gbai
     SET (int_batch_id,int_organization_id) = (SELECT gbh.batch_id,gbh.organization_id
                                                FROM gme_batch_header gbh
                                               WHERE gbai.batch_id = gbh.batch_id
                                                 AND NVL(gbh.delete_mark,0)=0)
   WHERE process_status = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_RUNNING
     AND request_id     = p_request_id
     AND object_type = GME_BATCH_OPEN_INTERFACE.OBJECTYPE_BATCH
     AND gbai.batch_id IS NOT NULL;

   IF g_debug <= gme_debug.g_log_procedure THEN
     gme_debug.put_line('Start to derive int_batch_id while batch_id is null for batch action type  '); 
   END IF;  
   
   UPDATE gme_batch_actions_interface gbai
      SET (int_batch_id,int_organization_id) = (SELECT gbh.batch_id,gbh.organization_id
                                                  FROM gme_batch_header gbh,
                                                       mtl_parameters mp
                                                 WHERE gbh.organization_id = mp.organization_id
                                                   AND NVL(gbh.delete_mark,0)=0
                                                   AND gbh.batch_type = 0
                                                   AND gbai.batch_no = gbh.batch_no
                                                   AND gbai.organization_code = mp.organization_code)
    WHERE process_status = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_RUNNING
      AND request_id     = p_request_id
      AND object_type = GME_BATCH_OPEN_INTERFACE.OBJECTYPE_BATCH
      AND gbai.batch_id IS NULL;
 
   IF g_debug <= gme_debug.g_log_procedure THEN
     gme_debug.put_line('Check whether both BATCH_ID column and BATCHSTEP_ID column have value'); 
   END IF;  
         
   UPDATE gme_batch_actions_interface gbai
      SET process_status = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_BV_WARNING
    WHERE process_status = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_RUNNING
      AND request_id     = p_request_id
      AND object_type    = GME_BATCH_OPEN_INTERFACE.OBJECTYPE_STEP
      AND gbai.batchstep_id IS NOT NULL
      AND gbai.batch_id IS NOT NULL;
         
   IF sql%rowcount >0 THEN                             
                                                 
     validation_error_process(p_request_id     => p_request_id,
                              p_group_id       => NULL,
                              p_message_name   => 'GME_BATCHID_IGNORED',
                              p_column_name    => 'BATCH_ID',
                              p_message_type   => GME_BATCH_OPEN_INTERFACE.MESSAGE_TYPE_WARNING,                              
                              x_return_message => x_return_message,
                              x_return_status  => x_return_status);   

     IF x_return_status <> fnd_api.g_ret_sts_success THEN
       ROLLBACK TO SAVEPOINT derive_int_id_columns;
       fnd_file.put_line (fnd_file.LOG, x_return_message);
       Return; 
     END IF;                             
   END IF;      
      
   IF g_debug <= gme_debug.g_log_procedure THEN
     gme_debug.put_line('Start to derive int_batch_id and int_batchstep_id while batchstep_id is not null'); 
   END IF;  

   UPDATE gme_batch_actions_interface gbai
      SET (int_batch_id,int_batchstep_id,int_organization_id) = (SELECT gbs.batch_id,gbs.batchstep_id,gbh.organization_id
                                                                   FROM gme_batch_steps gbs,
                                                                        gme_batch_header gbh
                                                                  WHERE gbai.batchstep_id = gbs.batchstep_id                                                                   
                                                                    AND NVL(gbs.delete_mark,0)=0
                                                                    AND gbs.batch_id = gbh.batch_id
                                                                    AND NVL(gbh.delete_mark,0)=0)
    WHERE process_status IN (GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_RUNNING,GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_BV_WARNING)
      AND request_id     = p_request_id
      AND object_type = GME_BATCH_OPEN_INTERFACE.OBJECTYPE_STEP
      AND gbai.batchstep_id IS NOT NULL; 
      
   IF g_debug <= gme_debug.g_log_procedure THEN
     gme_debug.put_line('Start to derive int_batch_id and int_batchstep_id while batchstep_id is null '); 
   END IF;  
 
   UPDATE gme_batch_actions_interface gbai
      SET (int_batch_id,int_batchstep_id,int_organization_id) = (SELECT gbs.batch_id,gbs.batchstep_id,gbh.organization_id
                                                                   FROM gme_batch_steps gbs,
                                                                        gme_batch_header gbh,
                                                                        mtl_parameters mp
                                                                  WHERE gbs.batch_id = gbh.batch_id
                                                                    AND gbh.organization_id = mp.organization_id
                                                                    AND NVL(gbs.delete_mark,0)=0
                                                                    AND NVL(gbh.delete_mark,0)=0
                                                                    AND gbai.batchstep_no = gbs.batchstep_no
                                                                    AND gbai.batch_no = gbh.batch_no
                                                                    and gbh.batch_type = 0
                                                                    and gbai.organization_code = mp.organization_code)
                                                
    WHERE process_status = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_RUNNING
      AND request_id     = p_request_id
      AND object_type = GME_BATCH_OPEN_INTERFACE.OBJECTYPE_STEP
      AND gbai.batchstep_id IS NULL; 

   IF g_debug <= gme_debug.g_log_procedure THEN
     gme_debug.put_line('Mark the records as error rows which have not derived int_batch_id successfully for batch action type'); 
   END IF;  
     
   --mark as error rows if int_batch_id column is NULL  
   UPDATE gme_batch_actions_interface 
      SET process_status = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_BV_INVALID
    WHERE process_status = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_RUNNING
      AND object_type = GME_BATCH_OPEN_INTERFACE.OBJECTYPE_BATCH
      AND request_id  = p_request_id
      AND int_batch_id IS NULL;
  
  --log error message into gme_intf_errors table for the records which have not 
  --derived int_batch_id value.
  IF sql%rowcount >0 THEN
 
    validation_error_process(p_request_id     => p_request_id,
                             p_group_id       => NULL,
                             p_message_name   => 'GME_BATCHID_NOTFOUND',
                             p_column_name    => 'INT_BATCH_ID',
                             p_message_type   => GME_BATCH_OPEN_INTERFACE.MESSAGE_TYPE_ERROR,
                             x_return_message => x_return_message,
                             x_return_status  => x_return_status);
    IF x_return_status <> fnd_api.g_ret_sts_success THEN
      ROLLBACK TO SAVEPOINT derive_int_id_columns;
      fnd_file.put_line (fnd_file.LOG, x_return_message);
      Return; 
    END IF;                             
  END IF;       
      
   IF g_debug <= gme_debug.g_log_procedure THEN
     gme_debug.put_line('Mark the records as error rows which have not derived int_batch_id successfully for step action type'); 
   END IF;  

   --mark as error rows if int_batch_id or int_batchstep_id is NULL.     
   UPDATE gme_batch_actions_interface 
      SET process_status = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_BV_INVALID
    WHERE process_status IN (GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_RUNNING,GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_BV_WARNING)
      AND object_type = GME_BATCH_OPEN_INTERFACE.OBJECTYPE_STEP
      AND request_id  = p_request_id
      AND (int_batch_id IS NULL OR int_batchstep_id IS NULL);
     
  --log error message into gme_intf_errors table for the records which have not 
  --derived int_batch_id or int_batchstep_id value.     
  IF sql%rowcount >0 THEN
    gme_common_pvt.log_message ('GME_BATCHSTEPID_NOTFOUND');
    validation_error_process(p_request_id     => p_request_id,
                             p_group_id       => NULL,
                             p_message_name   => 'GME_BATCHSTEPID_NOTFOUND',
                             p_column_name    => 'INT_BATCHSTEP_ID',
                             p_message_type   => GME_BATCH_OPEN_INTERFACE.MESSAGE_TYPE_ERROR,
                             x_return_message => x_return_message,
                             x_return_status  => x_return_status);
    IF x_return_status <> fnd_api.g_ret_sts_success THEN
      ROLLBACK TO SAVEPOINT derive_int_id_columns;
      fnd_file.put_line (fnd_file.LOG, x_return_message);
      Return; 
    END IF;                             
  END IF;        
    
   IF g_debug <= gme_debug.g_log_procedure THEN
     gme_debug.put_line('Update process_status of error records to finial error status'); 
   END IF;     
       
    UPDATE gme_batch_actions_interface
       SET process_status = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_FAILED,
           last_update_date = SYSDATE,
           last_updated_by  = l_user_id
     WHERE process_status = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_BV_INVALID
       AND request_id = p_request_id;
                                                                                                                                                                                                                         
  EXCEPTION
    WHEN OTHERS THEN
      x_return_status := fnd_api.g_ret_sts_error;
      x_return_message:= 'Deriving int_batch_id and int_batchstep_id have error,the error is:'
                          ||sqlcode||', '||sqlerrm;                      
      
END;   

/*================================================================================
  Procedure
    batch_actions_bulk_validation
  Description
    This procedure is used for making bulk validations for the records of
    GME_BATCH_ACTIONS_INTERFACE table.

  Parameters
    p_group_id    The corresponding group id for the worker which invoking the procedure
    p_request_id  The corresponding request id for the worker which invoking the procedure
  History:
    Shalchen       06-DEC-2012  Creation
================================================================================*/
PROCEDURE batch_actions_bulk_validation( p_group_id              IN NUMBER
                                        ,p_request_id            IN NUMBER
                                        ,x_return_message       OUT NOCOPY      VARCHAR2
                                        ,x_return_status        OUT NOCOPY      VARCHAR2) IS
                                    
BEGIN
  x_return_status := fnd_api.g_ret_sts_success;
  IF g_debug <= gme_debug.g_log_procedure THEN
    gme_debug.put_line('Entering api:'||g_pkg_name||'.'|| 'batch_actions_bulk_validation');
  END IF;  

   
  /*validate whether object_type column and action column combination is valid.
    while object_type is step action,action column value should not be 70 or 80.*/
  IF g_debug <= gme_debug.g_log_procedure THEN
    gme_debug.put_line('Start to validate whether object_type column and action column combination is valid');
  END IF;
     
  UPDATE gme_batch_actions_interface
     SET process_status = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_BV_INVALID
   WHERE group_id = p_group_id
     AND process_status IN (GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_RUNNING,
                            GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_BV_WARNING)
     AND object_type = GME_BATCH_OPEN_INTERFACE.OBJECTYPE_STEP
     AND action in (GME_BATCH_OPEN_INTERFACE.ACTION_TERMINATE,GME_BATCH_OPEN_INTERFACE.ACTION_CANCEL);
     
  IF sql%rowcount >0 THEN
    gme_common_pvt.log_message ('GME_INVALID_STEPACTION');
      
    validation_error_process(p_request_id     => p_request_id,
                             p_group_id       => p_group_id,
                             p_message_name   => 'GME_INVALID_STEPACTION',
                             p_column_name    => 'ACTION',
                             p_message_type   => GME_BATCH_OPEN_INTERFACE.MESSAGE_TYPE_ERROR,
                             x_return_message => x_return_message,
                             x_return_status  => x_return_status);   

    IF x_return_status <> fnd_api.g_ret_sts_success THEN
      return;
    END IF;                             
  END IF;
     
     
  /*Validate CLOSE_DATE column value.If 'OBJECT_TYPE' is Batch Action,  CLOSE_DATE 
    should larger than 'ACTUAL_CMPLT_DATE' column value in the GME_BATCH_HEADER table
    if 'OBJECT_TYPE' is Batch Action,  CLOSE_DATE should larger than 'ACTUAL_CMPLT_DATE' 
    column value in the GME_BATCH_STEPS table*/
  IF g_debug <= gme_debug.g_log_procedure THEN
    gme_debug.put_line('Start to validate whether CLOSE_DATE column value is valid');
  END IF;  
  
 
  UPDATE gme_batch_actions_interface gbai
     SET process_status = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_BV_INVALID
   WHERE group_id = p_group_id
     AND process_status IN (GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_RUNNING,
                            GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_BV_WARNING)    
     AND object_type = GME_BATCH_OPEN_INTERFACE.OBJECTYPE_BATCH
     AND action = GME_BATCH_OPEN_INTERFACE.ACTION_CLOSE
     AND close_date IS NOT NULL
     AND close_date <  (SELECT NVL(actual_cmplt_date,gbai.close_date) 
                          FROM gme_batch_header gbh
                         WHERE gbai.int_batch_id = gbh.batch_id);
       
  IF sql%rowcount >0 THEN
    gme_common_pvt.log_message ('GME_INVALID_DATE_RANGE');
    validation_error_process(p_request_id     => p_request_id,
                             p_group_id       => p_group_id,
                             p_message_name   => 'GME_INVALID_DATE_RANGE',
                             p_column_name    => 'CLOSE_DATE',
                             p_message_type   => GME_BATCH_OPEN_INTERFACE.MESSAGE_TYPE_ERROR,
                             p_param1         => 'CLOSE_DATE',
                             p_param2         => 'ACTUAL_CMPLT_DATE',
                             x_return_message => x_return_message,
                             x_return_status  => x_return_status);       

    IF x_return_status <> fnd_api.g_ret_sts_success THEN
      return;
    END IF;     
  END IF;
                                         
                                    
                                      

                             
  UPDATE gme_batch_actions_interface gbai
     SET process_status = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_BV_INVALID
   WHERE group_id = p_group_id
     AND process_status IN (GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_RUNNING,
                            GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_BV_WARNING)   
     AND object_type = GME_BATCH_OPEN_INTERFACE.OBJECTYPE_STEP
     AND action = GME_BATCH_OPEN_INTERFACE.ACTION_CLOSE
     AND close_date IS NOT NULL
     AND close_date < (SELECT NVL(actual_cmplt_date,gbai.close_date) 
                         FROM gme_batch_steps gbs
                        WHERE gbai.int_batchstep_id = gbs.batchstep_id);                                      
                                        
  IF sql%rowcount >0 THEN
    gme_common_pvt.log_message ('GME_INVALID_DATE_RANGE');
    validation_error_process(p_request_id     => p_request_id,
                             p_group_id       => p_group_id,
                             p_message_name   => 'GME_INVALID_DATE_RANGE',
                             p_column_name    => 'CLOSE_DATE',
                             p_message_type   => GME_BATCH_OPEN_INTERFACE.MESSAGE_TYPE_ERROR,
                             p_param1         => 'CLOSE_DATE',
                             p_param2         => 'ACTUAL_CMPLT_DATE',
                             x_return_message => x_return_message,
                             x_return_status  => x_return_status);        
    IF x_return_status <> fnd_api.g_ret_sts_success THEN
      return;
    END IF;     
  END IF;  
                                             
                                      
  /*Validate whether TERMINATE_REASON_ID column and TERMINATE_REASON_CODE column is valid*/
  IF g_debug <= gme_debug.g_log_procedure THEN
    gme_debug.put_line('Start to validate whether TERMINATE_REASON_ID column and TERMINATE_REASON_CODE column value is valid');
  END IF;
  
 
  UPDATE gme_batch_actions_interface gbai
     SET process_status = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_BV_INVALID
   WHERE group_id = p_group_id
     AND process_status IN (GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_RUNNING,
                            GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_BV_WARNING)
     AND action = GME_BATCH_OPEN_INTERFACE.ACTION_TERMINATE
     AND terminate_reason_id IS NOT NULL
     AND  NOT EXISTS (SELECT 1
                        FROM mtl_transaction_reasons mtr
                       WHERE gbai.terminate_reason_id = mtr.reason_id);                           
                            
  IF sql%rowcount >0 THEN
    gme_common_pvt.log_message (p_message_code => 'INV_LOTC_REASONID_INVALID', p_product_code => 'INV');
    validation_error_process(p_request_id     => p_request_id,
                             p_group_id       => p_group_id,
                             p_application    => 'INV',
                             p_message_name   => 'INV_LOTC_REASONID_INVALID',
                             p_column_name    => 'TERMINATE_REASON_ID',
                             p_message_type   => GME_BATCH_OPEN_INTERFACE.MESSAGE_TYPE_ERROR,                               
                             x_return_message => x_return_message,
                             x_return_status  => x_return_status);
    IF x_return_status <> fnd_api.g_ret_sts_success THEN
      return;
    END IF;     
  END IF; 
      
  UPDATE gme_batch_actions_interface gbai
     SET process_status = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_BV_INVALID
   WHERE group_id = p_group_id
     AND process_status IN (GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_RUNNING,
                            GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_BV_WARNING)
     AND action = GME_BATCH_OPEN_INTERFACE.ACTION_TERMINATE
     AND terminate_reason_code IS NOT NULL
     AND  NOT EXISTS (SELECT 1
                        FROM mtl_transaction_reasons mtr
                       WHERE gbai.terminate_reason_code = mtr.reason_name);                            
                            
  IF sql%rowcount >0 THEN
    gme_common_pvt.log_message (p_message_code => 'INV_LOTC_REASONID_INVALID', p_product_code => 'INV');
    validation_error_process(p_request_id     => p_request_id,
                             p_group_id       => p_group_id,
                             p_application    => 'INV',
                             p_message_name   => 'INV_LOTC_REASONID_INVALID',
                             p_column_name    => 'TERMINATE_REASON_ID',
                             p_message_type   => GME_BATCH_OPEN_INTERFACE.MESSAGE_TYPE_ERROR,                               
                             x_return_message => x_return_message,
                             x_return_status  => x_return_status);
    IF x_return_status <> fnd_api.g_ret_sts_success THEN
      return;
    END IF;     
  END IF;
 
  
  --derive int_terminate_reason_id value

  UPDATE gme_batch_actions_interface gbai
     SET int_terminate_reason_id =(SELECT reason_id
                                     FROM mtl_transaction_reasons mtr
                                    WHERE ((gbai.terminate_reason_id = mtr.reason_id AND gbai.terminate_reason_id IS NOT NULL)
                                       OR (gbai.terminate_reason_code = mtr.reason_name AND gbai.terminate_reason_code IS NOT NULL )))                                     
   WHERE group_id = p_group_id
     AND process_status IN (GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_RUNNING,
                            GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_BV_WARNING)
     AND action = GME_BATCH_OPEN_INTERFACE.ACTION_TERMINATE
     AND (terminate_reason_id IS NOT NULL or terminate_reason_code IS NOT NULL)
     AND int_terminate_reason_id IS NULL;
     
                                                                   
                                             
  /*Validate ACTUAL_CMPLT_DATE column value
    validate whether ACTUAL_CMPLT_DATE is > ACTUAL_START_DATE and ACTUAL_CMPLT_DATEis <= SYSDATE*/
  IF g_debug <= gme_debug.g_log_procedure THEN
    gme_debug.put_line('Start to validate whether ACTUAL_CMPLT_DATE column value is valid');
  END IF;
  
    
  UPDATE gme_batch_actions_interface gbai
     SET process_status = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_BV_INVALID
   WHERE group_id = p_group_id
     AND process_status IN (GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_RUNNING,
                            GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_BV_WARNING)  
     AND object_type = GME_BATCH_OPEN_INTERFACE.OBJECTYPE_BATCH
     AND action = GME_BATCH_OPEN_INTERFACE.ACTION_COMPLETE
     AND gbai.actual_cmplt_date IS NOT NULL     
     AND (gbai.actual_cmplt_date < (SELECT NVL(actual_start_date,gbai.actual_cmplt_date)
                                      FROM gme_batch_header gbh
                                     WHERE gbai.int_batch_id = gbh.batch_id)
          OR gbai.actual_cmplt_date > SYSDATE);             
            
  IF sql%rowcount >0 THEN
    gme_common_pvt.log_message ('GME_INVALID_DATE_RANGE');
    validation_error_process(p_request_id     => p_request_id,
                             p_group_id       => p_group_id,
                             p_message_name   => 'GME_INVALID_DATE_RANGE',
                             p_column_name    => 'ACTUAL_CMPLT_DATE',
                             p_message_type   => GME_BATCH_OPEN_INTERFACE.MESSAGE_TYPE_ERROR,
                             p_param1         => 'ACTUAL_CMPLT_DATE',
                             p_param2         => 'ACTUAL_START_DATE',
                             x_return_message => x_return_message,
                             x_return_status  => x_return_status);  
                                      
    IF x_return_status <> fnd_api.g_ret_sts_success THEN
      return;
    END IF;     
  END IF;
            
          

  UPDATE gme_batch_actions_interface gbai
     SET process_status = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_BV_INVALID
   WHERE group_id = p_group_id
     AND process_status IN (GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_RUNNING,
                            GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_BV_WARNING)    
     AND object_type = GME_BATCH_OPEN_INTERFACE.OBJECTYPE_STEP
     AND action = GME_BATCH_OPEN_INTERFACE.ACTION_COMPLETE
     AND gbai.actual_cmplt_date IS NOT NULL
     AND (gbai.actual_cmplt_date < (SELECT NVL (actual_start_date,gbai.actual_cmplt_date) 
                                     FROM gme_batch_steps gbs
                                    WHERE gbai.int_batchstep_id = gbs.batchstep_id)
          OR gbai.actual_cmplt_date > SYSDATE);                                                                    

  IF sql%rowcount >0 THEN
    gme_common_pvt.log_message ('GME_INVALID_DATE_RANGE');
      
    validation_error_process(p_request_id     => p_request_id,
                             p_group_id       => p_group_id,
                             p_message_name   => 'GME_INVALID_DATE_RANGE',
                             p_column_name    => 'ACTUAL_CMPLT_DATE',
                             p_message_type   => GME_BATCH_OPEN_INTERFACE.MESSAGE_TYPE_ERROR,
                             p_param1         => 'ACTUAL_CMPLT_DATE',
                             p_param2         => 'ACTUAL_START_DATE',
                             x_return_message => x_return_message,
                             x_return_status  => x_return_status);  
                                     
    IF x_return_status <> fnd_api.g_ret_sts_success THEN
      return;
    END IF;           
  END IF;
   
  
/*  --set process_status to PROCESS_STATUS_FAILED if process_status is PROCESS_STATUS_BV_INVALID
  --And populate conucrrent worker request id to request_id column.
  LOOP 
    UPDATE gme_batch_actions_interface gbai
       SET process_status = PROCESS_STATUS_FAILED,
           request_id     = p_request_id
     WHERE group_id       = p_group_id 
       AND process_status = PROCESS_STATUS_BV_INVALID
       AND rownum < l_bulk_load_size;
    EXIT WHEN sql%rowcount = 0;
  END LOOP; */    
  
  /*Validate Validity rule if it's not NULL while action is Release and Complete*/
  IF g_debug <= gme_debug.g_log_procedure THEN
    gme_debug.put_line('Start to validate whether Validity rule is valid while action is RELEASE or COMPLETE');
  END IF;
  
  UPDATE gme_batch_actions_interface gbai
     SET process_status = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_BV_INVALID
   WHERE group_id = p_group_id
     AND process_status IN (GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_RUNNING,
                            GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_BV_WARNING)
     AND (action = GME_BATCH_OPEN_INTERFACE.ACTION_RELEASE OR action = GME_BATCH_OPEN_INTERFACE.ACTION_COMPLETE)
     AND NOT EXISTS ( SELECT 1
                        FROM gme_batch_header gbh,
                             gmd_recipe_validity_rules grvr
                       WHERE gbh.batch_id = gbai.int_batch_id
                         AND gbh.recipe_validity_rule_id = grvr.recipe_validity_rule_id
                         AND NVL(grvr.delete_mark,0) = 0);
                         
  IF sql%rowcount >0 THEN
    gme_common_pvt.log_message ('GME_API_INVALID_VALIDITY');
      
    validation_error_process(p_request_id     => p_request_id,
                             p_group_id       => p_group_id,
                             p_message_name   => 'GME_API_INVALID_VALIDITY',
                             p_message_type   => GME_BATCH_OPEN_INTERFACE.MESSAGE_TYPE_ERROR,
                             x_return_message => x_return_message,
                             x_return_status  => x_return_status);  
                                     
    IF x_return_status <> fnd_api.g_ret_sts_success THEN
      return;
    END IF;           
  END IF;
  
  /*Validate Validity rule status if it's not NULL while action is Release and Complete*/
  IF g_debug <= gme_debug.g_log_procedure THEN
    gme_debug.put_line('Start to validate whether Validity rule status is valid while action is RELEASE or COMPLETE');
  END IF;                           
 
  UPDATE gme_batch_actions_interface gbai
     SET process_status = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_BV_INVALID
   WHERE group_id = p_group_id
     AND process_status IN (GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_RUNNING,
                            GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_BV_WARNING)
     AND (action = GME_BATCH_OPEN_INTERFACE.ACTION_RELEASE OR action = GME_BATCH_OPEN_INTERFACE.ACTION_COMPLETE)
     /*
       BUG 20287720  16/01/2015   Shaliu Chen
       Only release batch action and complete batch action
       for pending batch is NOT allowed if validity rule status 
       is Obsoleted or Onhold.
      */                     
     AND  EXISTS ( SELECT 1
                        FROM gme_batch_header gbh,
                             gmd_recipe_validity_rules grvr,
                             gmd_status gs
                       WHERE gbh.batch_id = gbai.int_batch_id
                         AND gbh.batch_status = gme_common_pvt.g_batch_pending
                         AND gbai.object_type = GME_BATCH_OPEN_INTERFACE.OBJECTYPE_BATCH
                         AND gbh.recipe_validity_rule_id = grvr.recipe_validity_rule_id
                         AND NVL(grvr.delete_mark,0) = 0
                         AND gs.status_code = grvr.validity_rule_status
                         AND gs.STATUS_CODE in ('1000','800'));
     /* END BUG 20287720 */                           
                         
  IF sql%rowcount >0 THEN
    gme_common_pvt.log_message ('GME_VALIDITY_OBSO_OR_ONHOLD');
      
    validation_error_process(p_request_id     => p_request_id,
                             p_group_id       => p_group_id,
                             p_message_name   => 'GME_VALIDITY_OBSO_OR_ONHOLD',
                             p_message_type   => GME_BATCH_OPEN_INTERFACE.MESSAGE_TYPE_ERROR,
                             x_return_message => x_return_message,
                             x_return_status  => x_return_status);  
                                     
    IF x_return_status <> fnd_api.g_ret_sts_success THEN
      return;
    END IF;           
  END IF;
  
  /*Check whether batch actul_start_date within the validity rule dates.*/
  IF g_debug <= gme_debug.g_log_procedure THEN
    gme_debug.put_line('Start to validate whether batch actul_start_date within the validity rule dates');
  END IF; 
  UPDATE gme_batch_actions_interface gbai
     SET process_status = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_BV_INVALID
   WHERE group_id = p_group_id
     AND process_status IN (GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_RUNNING,
                            GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_BV_WARNING)
     AND action = GME_BATCH_OPEN_INTERFACE.ACTION_RELEASE
     AND  EXISTS ( SELECT 1
                     FROM gme_batch_header gbh,
                          gmd_recipe_validity_rules grvr
                    WHERE gbh.batch_id = gbai.int_batch_id
                      AND gbh.recipe_validity_rule_id = grvr.recipe_validity_rule_id
                      AND NVL(grvr.delete_mark,0) = 0
                      AND gbai.actual_start_date IS NOT NULL
                      AND (gbai.actual_start_date < grvr.start_date OR
                           gbai.actual_start_date > NVL(grvr.end_date,gbai.actual_start_date)));
                           
  IF sql%rowcount >0 THEN
    gme_common_pvt.log_message ('GME_INVAL_VAL_RULE_DATES');
      
    validation_error_process(p_request_id     => p_request_id,
                             p_group_id       => p_group_id,
                             p_message_name   => 'GME_INVAL_VAL_RULE_DATES',
                             p_message_type   => GME_BATCH_OPEN_INTERFACE.MESSAGE_TYPE_ERROR,
                             x_return_message => x_return_message,
                             x_return_status  => x_return_status);  
                                     
    IF x_return_status <> fnd_api.g_ret_sts_success THEN
      return;
    END IF;           
  END IF;                           
   
  
  /*Check whether LPN txns exist while action is Revert to WIP and object type is Batch*/
  IF g_debug <= gme_debug.g_log_procedure THEN
    gme_debug.put_line('Start to validate whether LPN txns exist while action is Revert 
                        to WIP and object type is Batch');
  END IF;      
  
  UPDATE gme_batch_actions_interface gbai
     SET process_status = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_BV_INVALID
   WHERE group_id = p_group_id
     AND process_status IN (GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_RUNNING,
                            GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_BV_WARNING)
     AND action = GME_BATCH_OPEN_INTERFACE.ACTION_REVERT
     AND object_type = GME_BATCH_OPEN_INTERFACE.OBJECTYPE_BATCH
     AND nvl(continue_lpn_txn,'N') = 'N'
     AND EXISTS (SELECT 1
                   FROM mtl_material_transactions mmt,
                        gme_material_details mtl
                  WHERE NVL(mmt.transfer_lpn_id,0) > 0
                    AND mmt.transaction_source_id = gbai.int_batch_id
                    AND mmt.transaction_action_id = 31
                    AND mmt.transaction_type_id IN (44, 1002)
                    AND mmt.transaction_source_id = mtl.batch_id
                    AND mmt.inventory_item_id = mtl.inventory_item_id
                    AND mmt.transaction_id NOT IN (
                            SELECT transaction_id1
                              FROM gme_transaction_pairs
                             WHERE batch_id = gbai.int_batch_id
                               AND pair_type = 1));
                                      
  IF sql%rowcount >0 THEN
    gme_common_pvt.log_message ('GME_LPN_TRANS_EXISTS');
      
    validation_error_process(p_request_id     => p_request_id,
                             p_group_id       => p_group_id,
                             p_message_name   => 'GME_LPN_TRANS_EXISTS',
                             p_message_type   => GME_BATCH_OPEN_INTERFACE.MESSAGE_TYPE_ERROR,
                             x_return_message => x_return_message,
                             x_return_status  => x_return_status);  
                                     
    IF x_return_status <> fnd_api.g_ret_sts_success THEN
      return;
    END IF;           
  END IF;          
  
  /*Check whether routing is valid while action is CLOSE and object type is Batch*/
  IF g_debug <= gme_debug.g_log_procedure THEN
    gme_debug.put_line('Start to validate whether there is a routing associate with batch
                        while action is CLOSE and object type is Batch');                        
  END IF;   
        
  UPDATE gme_batch_actions_interface gbai
     SET process_status = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_BV_INVALID
   WHERE group_id = p_group_id
     AND process_status IN (GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_RUNNING,
                            GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_BV_WARNING) 
     AND action = GME_BATCH_OPEN_INTERFACE.ACTION_CLOSE
     AND object_type = GME_BATCH_OPEN_INTERFACE.OBJECTYPE_BATCH
     AND EXISTS ( SELECT 1
                    FROM gme_batch_header gbh
                   WHERE gbh.batch_id = gbai.int_batch_id
                     AND gbh.routing_id IS NULL
                     AND gbh.poc_ind = 'Y'); 
                     
  IF sql%rowcount >0 THEN
    gme_common_pvt.log_message ('GMD_NO_ROUTING_ASSOCIATED');
      
    validation_error_process(p_request_id     => p_request_id,
                             p_group_id       => p_group_id,
                             p_message_name   => 'GMD_NO_ROUTING_ASSOCIATED',
                             p_message_type   => GME_BATCH_OPEN_INTERFACE.MESSAGE_TYPE_ERROR,
                             x_return_message => x_return_message,
                             x_return_status  => x_return_status);  
                                     
    IF x_return_status <> fnd_api.g_ret_sts_success THEN
      return;
    END IF;           
  END IF;                                            
                                                                                                                                                                                   
  EXCEPTION
    WHEN OTHERS THEN
      x_return_status := fnd_api.g_ret_sts_error;
      x_return_message := 'There is error occurred in the bulk validation procedure,error code:'||sqlcode
                           ||',error message:'||sqlerrm;
      fnd_file.put_line (fnd_file.LOG, x_return_message);
      Return;       
END;  


 /*================================================================================
  Procedure
    launch_action_worker
  Description
    This procedure is used for invoking batch/step actions public API to import
    the records in the GME_BATCH_ACTIONS_INTERFACE table line by line.

  Parameters
    p_group_id    The corresponding group id for the worker which invoking the procedure
    p_purge_flag  It's same with the process_batch_actions procedure.

  History:
    Shalchen       06-DEC-2012  Creation
    Shalchen       11-FEB-2014  BUG 18096615  some record can not process if there are 
                                one more same record in the same group,add LOOP to traverse
                                every record. 
    Shalchen       02-FEB-2015  BUG 20448067 Introduced new p_save_batch parameter for 
                                unrelease_batch api so that material txn can be processed 
                                even if p_commit is false.
================================================================================*/

PROCEDURE launch_action_worker(err_buf          OUT NOCOPY VARCHAR2,
                               ret_code         OUT NOCOPY VARCHAR2,
                               p_group_id       IN NUMBER,
                               p_purge_flag     IN VARCHAR2) IS
                               

  
  /*Get all of batch and step action records first,sort by int_organization_id,
    int_batch_id and object_type and action combination sequence*/                                   
  CURSOR cur_get_sorted_import_data IS
    SELECT DISTINCT
             int_organization_id,
             int_batch_id,
             object_type,
             action,
             DECODE (Object_type || '-' || Action,
                     GME_BATCH_OPEN_INTERFACE.OBJECTYPE_BATCH||'-'||GME_BATCH_OPEN_INTERFACE.ACTION_RELEASE  ,1,
                     GME_BATCH_OPEN_INTERFACE.OBJECTYPE_STEP ||'-'||GME_BATCH_OPEN_INTERFACE.ACTION_RELEASE  ,2,
                     GME_BATCH_OPEN_INTERFACE.OBJECTYPE_STEP ||'-'||GME_BATCH_OPEN_INTERFACE.ACTION_COMPLETE ,3,
                     GME_BATCH_OPEN_INTERFACE.OBJECTYPE_BATCH||'-'||GME_BATCH_OPEN_INTERFACE.ACTION_COMPLETE ,4,
                     GME_BATCH_OPEN_INTERFACE.OBJECTYPE_STEP ||'-'||GME_BATCH_OPEN_INTERFACE.ACTION_CLOSE    ,5,
                     GME_BATCH_OPEN_INTERFACE.OBJECTYPE_BATCH||'-'||GME_BATCH_OPEN_INTERFACE.ACTION_CLOSE    ,6,
                     GME_BATCH_OPEN_INTERFACE.OBJECTYPE_BATCH||'-'||GME_BATCH_OPEN_INTERFACE.ACTION_REOPEN   ,7,
                     GME_BATCH_OPEN_INTERFACE.OBJECTYPE_STEP ||'-'||GME_BATCH_OPEN_INTERFACE.ACTION_REOPEN   ,8,
                     GME_BATCH_OPEN_INTERFACE.OBJECTYPE_BATCH||'-'||GME_BATCH_OPEN_INTERFACE.ACTION_REVERT   ,9,
                     GME_BATCH_OPEN_INTERFACE.OBJECTYPE_STEP ||'-'||GME_BATCH_OPEN_INTERFACE.ACTION_REVERT   ,10,
                     GME_BATCH_OPEN_INTERFACE.OBJECTYPE_STEP ||'-'||GME_BATCH_OPEN_INTERFACE.ACTION_UNRELEASE,11,
                     GME_BATCH_OPEN_INTERFACE.OBJECTYPE_BATCH||'-'||GME_BATCH_OPEN_INTERFACE.ACTION_UNRELEASE,12,
                     GME_BATCH_OPEN_INTERFACE.OBJECTYPE_BATCH||'-'||GME_BATCH_OPEN_INTERFACE.ACTION_TERMINATE,13,
                     GME_BATCH_OPEN_INTERFACE.OBJECTYPE_BATCH||'-'||GME_BATCH_OPEN_INTERFACE.ACTION_CANCEL   ,14)   action_seq
     FROM gme_batch_actions_interface
    WHERE GROUP_ID = p_group_id       
      AND process_status IN (GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_RUNNING,
                             GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_BV_WARNING)
    ORDER BY int_organization_id, int_batch_id, action_seq;
     
  CURSOR Cur_get_batch_header(v_batch_id NUMBER) IS
    SELECT *
      FROM gme_batch_header
     WHERE batch_id = v_batch_id;
     
  CURSOR cur_get_interface_record(v_group_id NUMBER,v_int_organization_id NUMBER,v_int_batch_id NUMBER,v_object_type NUMBER,v_action NUMBER) IS
    SELECT *
      FROM gme_batch_actions_interface
     WHERE group_id = v_group_id
       AND int_organization_id = v_int_organization_id     
       AND int_batch_id = v_int_batch_id
       AND object_type = v_object_type
       AND action = v_action
       --BUG 18096615
       --Add this conidition to avoid to fetch same record in case there are one more same records every time.
       AND process_status in  (GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_RUNNING,
                               GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_BV_WARNING);       
       
  CURSOR cur_get_step_interface_record(v_interface_id NUMBER) IS
    SELECT *
      FROM gme_batch_actions_interface
     WHERE interface_id = v_interface_id;    
       
       
     
  CURSOR cur_sorted_steps(v_group_id NUMBER,v_int_batch_id NUMBER,v_object_type NUMBER,v_action NUMBER) IS
    SELECT gbai.interface_id
      FROM gme_batch_actions_interface gbai,
           gme_batch_steps gbs
     WHERE gbai.int_batch_id = v_int_batch_id
       AND gbai.group_id     = v_group_id
       AND gbai.object_type  = v_object_type
       AND gbai.action       = v_action
       AND gbai.int_batchstep_id = gbs.batchstep_id
       --BUG 18096615
       --Add this conidition to avoid to fetch same record in case there are one more same records every time.      
       AND process_status in  (GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_RUNNING,
                               GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_BV_WARNING)       
     ORDER BY gbs.batchstep_no;  
         
/*    SELECT a.interface_id,b.level1
      FROM gme_batch_actions_interface a,      
    (SELECT gbsd.dep_step_id,MAX(level) AS level1
       FROM gme_batch_step_dependencies gbsd
      WHERE gbsd.batch_id = v_int_batch_id
      START WITH gbsd.batchstep_id IN (SELECT gbai.batchstep_id
                                         FROM gme_batch_actions_interface gbai
                                        WHERE gbai.batchstep_id IS NOT NULL
                                          AND gbai.int_batch_id = v_int_batch_id
                                          AND gbai.group_id     = v_group_id
                                          AND gbai.object_type  = v_object_type
                                          AND gbai.action       = v_action)                                           
    CONNECT BY gbsd.batchstep_id = PRIOR gbsd.dep_step_id
      GROUP BY dep_step_id) b
      WHERE a.int_batch_id = v_int_batch_id
        AND a.group_id     = v_group_id
        AND a.object_type  = v_object_type
        AND a.action       = v_action
        AND a.int_batchstep_id = b.dep_step_id
    UNION ALL
    SELECT ga.interface_id, 0 AS Level1
      FROM gme_batch_actions_interface ga
     WHERE ga.int_batch_id = v_int_batch_id
       AND ga.group_id     = v_group_id
       AND ga.object_type  = v_object_type
       AND ga.action       = v_action
       AND ga.batchstep_id NOT IN
                                 (SELECT dep_step_id
                                    FROM gme_batch_step_dependencies gs
                                   WHERE gs.batch_id = ga.int_batch_id)
    ORDER BY 2 DESC;  */   
   
              
       
     
  l_request_id         NUMBER;
  l_purge_request_id   NUMBER;   
  l_user_id            NUMBER;
  l_object_type        NUMBER;
  l_action             NUMBER;
  l_msg_cnt            NUMBER;
  l_reason_id          NUMBER;
  l_bulk_load_size     NUMBER;     
  l_int_batchstep_id       NUMBER;
  l_msg_data           VARCHAR2(2000);
  l_return_status      VARCHAR2(1);
  l_int_organization_id    NUMBER;
  l_int_batch_id           NUMBER;
  l_interface_id       NUMBER;
  l_create_resv_pend_lots NUMBER;
  l_batch_header       gme_batch_header%ROWTYPE;
  l_ignore_exception   VARCHAR2(1);
  l_reopen_steps       VARCHAR2(1);
  l_continue_lpn_txn   VARCHAR2(1);
  l_batch_header_out   gme_batch_header%ROWTYPE;
  l_exception_tbl      gme_common_pvt.exceptions_tab;
  l_step_rec           gme_batch_steps%ROWTYPE;
  l_step_rec_out       gme_batch_steps%ROWTYPE;
  l_batch_actions_interface gme_batch_actions_interface%ROWTYPE;  
  l_log_error_result        NUMBER;
  l_failed_count            NUMBER;
  l_actual_start_date       DATE;
  l_actual_cmplt_date       DATE;
  l_close_date              DATE;
  l_process_status          VARCHAR2(32);
  
  l_conc_status             BOOLEAN;
  l_error_msg_count         NUMBER;
  l_critical_msg_count      NUMBER; 
  
  l_app_name               VARCHAR2(3);
  l_msg_name               VARCHAR2(256);
  l_message_detail         GME_BATCH_OPEN_INTERFACE.T_MESSAGE_TABLE;
  
  resource_txns_check_exception EXCEPTION;
  bulk_validation_exception     EXCEPTION;
  error_close_period            EXCEPTION;
                                     
  BEGIN
  fnd_file.put_line(fnd_file.log,'The input parameter of Group Id is'||p_group_id);  
  fnd_file.put_line(fnd_file.log,'The input parameter of Purge Successful Rows is'||p_purge_flag);
  IF (g_debug <> -1) THEN
     gme_debug.log_initialize ();
  END IF;    
    
    fnd_msg_pub.initialize;
    
    ret_code := fnd_api.g_ret_sts_success;
    l_failed_count := 0;
    l_request_id := fnd_global.conc_request_id(); 
    l_user_id    := fnd_global.user_id;    
    IF g_debug <= gme_debug.g_log_procedure THEN
      gme_debug.put_line('Entering api:'||g_pkg_name||'.'|| 'batch_actions_worker');
    END IF;

    --Start to bulk validation process
    IF g_debug <= gme_debug.g_log_procedure THEN
      gme_debug.put_line('Start to bulk validation process for group_id:'||p_group_id);
    END IF;     
    
    l_bulk_load_size := NVL(fnd_profile.value('GME_BULK_LOAD_SIZE'), 1000);
      
    IF (l_bulk_load_size <=0) THEN
      ret_code := fnd_api.g_ret_sts_error;
      err_buf  := 'The profile GME: Interface Bulk Load Limit Size value is not correct,Please correct
                           profile value.';
      fnd_file.put_line (fnd_file.LOG, err_buf);                     
      IF g_debug <= gme_debug.g_log_procedure THEN
        gme_debug.put_line(err_buf);
      END IF;       
      RETURN;  
    END IF;
    
    --Set global variable to Y to bypass some validations in the public API 
    IF NVL(gme_common_pvt.g_bulk_validation_done,'N')<> 'Y' THEN
      gme_common_pvt.g_bulk_validation_done := 'Y';
    END IF;
              
    batch_actions_bulk_validation(p_group_id       => p_group_id,
                                  p_request_id     => l_request_id,
                                  x_return_message => err_buf,
                                  x_return_status  => ret_code);   
    /*make sure all of error insert statements in the bulk validation procedure
      have been committed to db even if the procedure--batch_actions_bulk_validation
      return FAILED*/   
    COMMIT;
                                
    IF ret_code <> fnd_api.g_ret_sts_success THEN
      fnd_file.put_line (fnd_file.LOG, err_buf);
      
      l_log_error_result := gme_log_interface_err
                               (P_GROUP_ID           => p_group_id,
                                P_object_type        => -1,
                                P_INTERFACE_ID       => -1,
                                P_COLUMN_NAME        => NULL,
                                P_MESSAGE_TYPE       => GME_BATCH_OPEN_INTERFACE.MESSAGE_TYPE_CRITICAL,
                                P_MESSAGE_NAME       => NULL,
                                P_MESSAGE_TEXT       => err_buf,
                                P_SOURCE_TABLE_NAME  => 'GME_BATCH_ACTIONS_INTERFACE',
                                p_err_message_detail => l_message_detail);
                                       
      RAISE bulk_validation_exception;
    ELSE
      IF g_debug <= gme_debug.g_log_procedure THEN
        gme_debug.put_line('All of records in the group_id:'||p_group_id||
                           ' passed the bulk validation successfully');
      END IF;                        
    END IF;                                
   
    IF g_debug <= gme_debug.g_log_procedure THEN
      gme_debug.put_line('Start to process all of batch action and step action records in the group_id:'
                         ||p_group_id);
    END IF;  
    
    l_failed_count := 0;
    OPEN cur_get_sorted_import_data;
    --IF cur_get_rest_import_data%FOUND THEN
      LOOP
        FETCH cur_get_sorted_import_data BULK COLLECT INTO gme_dist_batch_action_tbl LIMIT l_bulk_load_size;
          IF (gme_dist_batch_action_tbl.count =0) THEN
            fnd_file.put_line(fnd_file.LOG,'There is no more batch/step action record to be processed ');
            EXIT;               
          END IF;
              
          FOR i in gme_dist_batch_action_tbl.FIRST..gme_dist_batch_action_tbl.LAST LOOP
            BEGIN
              
              l_int_organization_id := gme_dist_batch_action_tbl(i).int_organization_id;
              l_int_batch_id := gme_dist_batch_action_tbl(i).int_batch_id;          
              l_object_type := gme_dist_batch_action_tbl(i).object_type;
              l_action      := gme_dist_batch_action_tbl(i).action;

              
              IF l_object_type = GME_BATCH_OPEN_INTERFACE.OBJECTYPE_BATCH THEN
                l_app_name    := NULL;
                l_msg_name    := NULL;
                
                 
                --BUG 18096615
                IF cur_get_interface_record%ISOPEN THEN
                  CLOSE cur_get_interface_record;  
                END IF;                  
                OPEN cur_get_interface_record(p_group_id,l_int_organization_id,l_int_batch_id,l_object_type,l_action);
                --BUG 18096615
                --Add LOOP because it's probably there are same record in the same group due to repeated insert.
                LOOP
                  FETCH cur_get_interface_record INTO l_batch_actions_interface;
                  EXIT WHEN cur_get_interface_record%NOTFOUND;
                
                  l_interface_id := l_batch_actions_interface.interface_id;           
                  l_continue_lpn_txn := l_batch_actions_interface.continue_lpn_txn;
                  l_create_resv_pend_lots := l_batch_actions_interface.create_resv_pend_lots;
                  l_actual_start_date := l_batch_actions_interface.actual_start_date;
                  l_actual_cmplt_date := l_batch_actions_interface.actual_cmplt_date;
                  l_close_date        := l_batch_actions_interface.close_date;
                  l_process_status    := l_batch_actions_interface.process_status;
                  --convert Y/N to T/F for ignore_material_exception column
                  IF (l_batch_actions_interface.ignore_material_exception = 'Y')THEN
                    l_ignore_exception := 'T';
                  ELSIF (l_batch_actions_interface.ignore_material_exception = 'N') THEN
                    l_ignore_exception := 'F';      
                  END IF;
                  --convert Y/N to T/F for reopen_steps column
                  IF (l_batch_actions_interface.reopen_steps = 'Y')THEN
                    l_reopen_steps := 'T';
                  ELSIF (l_batch_actions_interface.reopen_steps = 'N')THEN
                    l_reopen_steps := 'F';      
                  END IF;                 
                  
                  OPEN Cur_get_batch_header(l_int_batch_id);
                  FETCH Cur_get_batch_header INTO l_batch_header;
                  CLOSE Cur_get_batch_header;  
                  
                 IF l_action = GME_BATCH_OPEN_INTERFACE.ACTION_RELEASE THEN 
                   l_batch_header.actual_start_date := l_actual_start_date;
                   IF NOT gme_common_pvt.check_close_period(p_org_id     => l_batch_header.organization_id
                                                           ,p_trans_date => l_batch_header.actual_start_date) THEN
                      RAISE error_close_period;
                    END IF;                 
             
                   gme_api_pub.release_batch(p_api_version              => 2.0,
                                             p_validation_level         => gme_common_pvt.g_max_errors,
                                             p_init_msg_list            => fnd_api.g_true,
                                             p_commit                   => fnd_api.g_false,--commit later
                                             x_message_count            => l_msg_cnt,
                                             x_message_list             => l_msg_data,
                                             x_return_status            => l_return_status,
                                             p_batch_header_rec         => l_batch_header,
                                             p_org_code                 => NULL,
                                             p_ignore_exception         => l_ignore_exception,
                                             p_validate_flexfields      => fnd_api.g_true, --set DFF validation flag to TRUE
                                             x_batch_header_rec         => l_batch_header_out,
                                             x_exception_material_tbl   => l_exception_tbl);                                                        
                        
                 ELSIF l_action = GME_BATCH_OPEN_INTERFACE.ACTION_COMPLETE THEN 
                   
                   l_batch_header.actual_cmplt_date := l_actual_cmplt_date; 
                   
                   IF NOT gme_common_pvt.check_close_period(p_org_id     => l_batch_header.organization_id
                                                           ,p_trans_date => l_batch_header.actual_cmplt_date) THEN
                      RAISE error_close_period;
                    END IF;                   
                 
                   gme_api_pub.complete_batch(p_api_version              => 2.0,
                                             p_validation_level         => gme_common_pvt.g_max_errors,
                                             p_init_msg_list            => fnd_api.g_false,
                                             p_commit                   => fnd_api.g_false,
                                             x_message_count            => l_msg_cnt,
                                             x_message_list             => l_msg_data,
                                             x_return_status            => l_return_status,
                                             p_batch_header_rec         => l_batch_header,
                                             p_org_code                 => NULL,
                                             p_ignore_exception         => l_ignore_exception,
                                             p_validate_flexfields      => fnd_api.g_true,
                                             x_batch_header_rec         => l_batch_header_out,
                                             x_exception_material_tbl   => l_exception_tbl);
                       
                 ELSIF l_action = GME_BATCH_OPEN_INTERFACE.ACTION_CLOSE THEN  --Close Batch       
                   --Check whether there is unprocessed resource txn.
                   check_unprocessed_resc_trxn(p_object_type       => l_object_type,
                                               p_int_batch_id      => l_int_batch_id,                                       
                                               p_int_batchstep_id  => NULL,
                                               x_return_message    => l_msg_data,
                                               x_return_status     => l_return_status);
                   IF l_return_status <> fnd_api.g_ret_sts_success THEN
                     RAISE resource_txns_check_exception;                 
                     
                   END IF;
                   
                   l_batch_header.batch_close_date := l_close_date; 
                   
                   IF NOT gme_common_pvt.check_close_period(p_org_id     => l_batch_header.organization_id
                                                           ,p_trans_date => l_batch_header.batch_close_date) THEN
                      RAISE error_close_period;
                    END IF;                    
                 
                   gme_api_pub.close_batch  (p_api_version              => 2.0,
                                             p_validation_level         => gme_common_pvt.g_max_errors,
                                             p_init_msg_list            => fnd_api.g_false,
                                             p_commit                   => fnd_api.g_false,
                                             x_message_count            => l_msg_cnt,
                                             x_message_list             => l_msg_data,
                                             x_return_status            => l_return_status,
                                             p_batch_header_rec         => l_batch_header,
                                             x_batch_header_rec         => l_batch_header_out,
                                             p_org_code                 => NULL);
                            
                 ELSIF l_action = GME_BATCH_OPEN_INTERFACE.ACTION_REOPEN THEN  --Reopen Batch
                 
                   gme_api_pub.reopen_batch (p_api_version           => 2,                                                                                               
                                             p_validation_level      => gme_common_pvt.g_max_errors,                  
                                             p_init_msg_list         => fnd_api.g_false,                  
                                             p_commit                => fnd_api.g_false,                                                                                                
                                             x_message_count         => l_msg_cnt,          
                                             x_message_list          => l_msg_data,       
                                             x_return_status         => l_return_status,      
                                             p_org_code              => NULL,                 
                                             p_batch_header_rec      => l_batch_header,
                                             p_reopen_steps          => l_reopen_steps,       
                                             x_batch_header_rec      => l_batch_header_out    
                                            );           

                 ELSIF l_action = GME_BATCH_OPEN_INTERFACE.ACTION_REVERT THEN  --Revert to WIP               
                                             
                  gme_api_pub.revert_batch                                                                           
                                             (p_continue_lpn_txn      => l_continue_lpn_txn,
                                              p_api_version           => 2.0,                  
                                              p_validation_level      => gme_common_pvt.g_max_errors,                  
                                              p_init_msg_list         => fnd_api.g_false,                  
                                              p_commit                => fnd_api.g_false,                  
                                              x_message_count         => l_msg_cnt,      
                                              x_message_list          => l_msg_data,       
                                              x_return_status         => l_return_status,      
                                              p_org_code              => NULL,                 
                                              p_batch_header_rec      => l_batch_header,
                                              x_batch_header_rec      => l_batch_header_out    
                                             );                                       
                                             
                 ELSIF l_action = GME_BATCH_OPEN_INTERFACE.ACTION_UNRELEASE THEN  --Unrelease batch
                  gme_api_pub.unrelease_batch
                                          (p_api_version                => 2.0,
                                           p_validation_level           => gme_common_pvt.g_max_errors,
                                           p_init_msg_list              => fnd_api.g_false,
                                           p_commit                     => fnd_api.g_false,
                                           p_save_batch                 => fnd_api.g_true,  --BUG 20448067
                                           x_message_count              => l_msg_cnt,
                                           x_message_list               => l_msg_data,
                                           x_return_status              => l_return_status,
                                           p_batch_header_rec           => l_batch_header,
                                           p_org_code                   => NULL,
                                           p_create_resv_pend_lots      => l_create_resv_pend_lots,
                                           p_continue_lpn_txn           => l_continue_lpn_txn,
                                           x_batch_header_rec           => l_batch_header_out
                                          );        
                 
                 ELSIF l_action = GME_BATCH_OPEN_INTERFACE.ACTION_TERMINATE THEN  --Terminate Batch
                 
                  l_reason_id := l_batch_actions_interface.int_terminate_reason_id;
                  l_batch_header.terminate_reason_id := l_reason_id;
                  l_batch_header.actual_cmplt_date := l_actual_cmplt_date;
                            
                   gme_api_pub.terminate_batch(p_api_version              => 2.0,
                                             p_validation_level         => gme_common_pvt.g_max_errors,
                                             p_init_msg_list            => fnd_api.g_false,
                                             p_commit                   => fnd_api.g_false,
                                             x_message_count            => l_msg_cnt,
                                             x_message_list             => l_msg_data,
                                             x_return_status            => l_return_status,
                                             p_org_code                 => NULL,
                                             p_reason_name              => NULL,
                                             p_batch_header_rec         => l_batch_header,
                                             x_batch_header_rec         => l_batch_header_out);       
                 ELSIF l_action = GME_BATCH_OPEN_INTERFACE.ACTION_CANCEL THEN  --Cancel batch     
                                             
                   gme_api_pub.cancel_batch  (p_api_version              => 2.0,
                                             p_validation_level         => gme_common_pvt.g_max_errors,
                                             p_init_msg_list            => fnd_api.g_false,
                                             p_commit                   => fnd_api.g_true,
                                             x_message_count            => l_msg_cnt,
                                             x_message_list             => l_msg_data,
                                             x_return_status            => l_return_status,
                                             p_org_code                 => NULL,                                   
                                             p_batch_header_rec         => l_batch_header,
                                             x_batch_header_rec         => l_batch_header_out);       
                 END IF; 
                 
                 IF (l_return_status <> fnd_api.g_ret_sts_success) THEN
                   IF l_msg_data IS NULL THEN
                     l_msg_data := fnd_msg_pub.get (p_msg_index          => fnd_msg_pub.g_last,
                                                    p_encoded            => 'F');
                   END IF;    
                   IF (l_msg_data IS NOT NULL  and l_msg_name IS NULL) THEN
                     gme_batch_open_interface.get_error_message_infor(p_message_text         => l_msg_data
                                                                     ,x_app_name             => l_app_name
                                                                     ,x_message_name         => l_msg_name
                                                                     ,x_message_detail       => l_message_detail);                   
                     
                   END IF;           
                   l_log_error_result  :=  gme_log_interface_err
                                            (P_GROUP_ID           => p_group_id,
                                             P_object_type        => l_object_type,
                                             P_INTERFACE_ID       => l_interface_id,
                                             P_COLUMN_NAME        => NULL,
                                             P_MESSAGE_TYPE       => GME_BATCH_OPEN_INTERFACE.MESSAGE_TYPE_ERROR,
                                             P_MESSAGE_NAME       => l_msg_name,
                                             P_MESSAGE_TEXT       => l_msg_data,
                                             P_APPLICATION        => l_app_name,
                                             P_SOURCE_TABLE_NAME  => 'GME_BATCH_ACTIONS_INTERFACE',
                                             P_ERR_MESSAGE_DETAIL => l_message_detail
                                             );        
                   UPDATE gme_batch_actions_interface
                       SET last_update_date = SYSDATE      ,
                           last_updated_by  = l_user_id    ,
                           request_id       = l_request_id ,
                           process_status   = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_FAILED                
                     WHERE interface_id     = l_interface_id; 
                      
                    l_failed_count := l_failed_count+1; 
                 ELSE
                    IF l_process_status = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_RUNNING THEN
                      UPDATE gme_batch_actions_interface
                         SET last_update_date = SYSDATE,      
                             last_updated_by  = l_user_id    ,
                             request_id       = l_request_id ,
                             process_status   = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_SUCCEED                
                       WHERE interface_id     = l_interface_id;  
                    ELSIF l_process_status = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_BV_WARNING THEN
                      UPDATE gme_batch_actions_interface
                         SET last_update_date = SYSDATE,      
                             last_updated_by  = l_user_id,    
                             process_status   = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_WARNING                
                       WHERE interface_id     = l_interface_id;                    
                    END IF;                          
                 END IF;             
                 COMMIT;  
               END LOOP;
               CLOSE cur_get_interface_record; 
                               
              ELSIF l_object_type = GME_BATCH_OPEN_INTERFACE.OBJECTYPE_STEP THEN
                FOR cur_ss IN cur_sorted_steps(p_group_id,l_int_batch_id,l_object_type,l_action)  LOOP
                  BEGIN
                  
                    l_interface_id := cur_ss.interface_id;
                    l_app_name := NULL;
                    l_msg_name := NULL; 
                    
                    OPEN cur_get_step_interface_record(l_interface_id);
                    FETCH cur_get_step_interface_record INTO l_batch_actions_interface;
                    CLOSE cur_get_step_interface_record;

                    l_int_batchstep_id := l_batch_actions_interface.int_batchstep_id;           
                    l_continue_lpn_txn := l_batch_actions_interface.continue_lpn_txn;
                    l_create_resv_pend_lots := l_batch_actions_interface.create_resv_pend_lots;
                    l_actual_start_date := l_batch_actions_interface.actual_start_date;
                    l_actual_cmplt_date := l_batch_actions_interface.actual_cmplt_date;
                    l_close_date        := l_batch_actions_interface.close_date;
                    l_process_status    := l_batch_actions_interface.process_status;
                    --convert Y/N to T/F for ignore_material_exception column
                    IF (l_batch_actions_interface.ignore_material_exception = 'Y')THEN
                      l_ignore_exception := 'T';
                    ELSIF (l_batch_actions_interface.ignore_material_exception = 'N') THEN
                      l_ignore_exception := 'F';      
                    END IF;
                    --convert Y/N to T/F for reopen_steps column
                    IF (l_batch_actions_interface.reopen_steps = 'Y')THEN
                      l_reopen_steps := 'T';
                    ELSIF (l_batch_actions_interface.reopen_steps = 'N')THEN
                      l_reopen_steps := 'F';      
                    END IF;                     
                
                    l_step_rec.batchstep_id := l_int_batchstep_id;  
           
                    IF l_action = GME_BATCH_OPEN_INTERFACE.ACTION_RELEASE THEN
                      --gme_api_pub.release_step
                     l_step_rec.actual_start_date := l_actual_start_date; 
                      
                     gme_api_pub.release_step (p_api_version              => 2.0,
                                               p_validation_level         => gme_common_pvt.g_max_errors,
                                               p_init_msg_list            => fnd_api.g_false,
                                               p_commit                   => fnd_api.g_false,
                                               x_message_count            => l_msg_cnt,
                                               x_message_list             => l_msg_data,
                                               x_return_status            => l_return_status,
                                               p_batch_step_rec           => l_step_rec,
                                               p_batch_no                 => NULL,
                                               p_org_code                 => NULL,
                                               p_ignore_exception         => l_ignore_exception,
                                               p_validate_flexfields      => fnd_api.g_true,
                                               x_batch_step_rec           => l_step_rec_out,
                                               x_exception_material_tbl   => l_exception_tbl);
                  

                    ELSIF l_action = GME_BATCH_OPEN_INTERFACE.ACTION_COMPLETE THEN
                      l_step_rec.actual_cmplt_date := l_actual_cmplt_date;
                      --gme_api_pub.complete_step
                     gme_api_pub.complete_step(p_api_version              => 2.0,
                                               p_validation_level         => gme_common_pvt.g_max_errors,
                                               p_init_msg_list            => fnd_api.g_false,
                                               p_commit                   => fnd_api.g_false,
                                               x_message_count            => l_msg_cnt,
                                               x_message_list             => l_msg_data,
                                               x_return_status            => l_return_status,
                                               p_batch_step_rec           => l_step_rec,
                                               p_batch_no                 => NULL,
                                               p_org_code                 => NULL,
                                               p_ignore_exception         => l_ignore_exception,
                                               p_validate_flexfields      => fnd_api.g_true,
                                               p_override_quality         => fnd_api.g_false, --hard default value
                                               x_batch_step_rec           => l_step_rec_out,
                                               x_exception_material_tbl   => l_exception_tbl);
                    ELSIF l_action = GME_BATCH_OPEN_INTERFACE.ACTION_CLOSE THEN
                      l_step_rec.step_close_date := l_close_date;
                     --Check whether there is unprocessed resource txn.
                    check_unprocessed_resc_trxn(p_object_type       => l_object_type,
                                                p_int_batch_id      => NULL,                                       
                                                p_int_batchstep_id  => l_int_batchstep_id,
                                                x_return_message    => l_msg_data,
                                                x_return_status     => l_return_status);
                     IF l_return_status <> fnd_api.g_ret_sts_success THEN
                       RAISE resource_txns_check_exception;             
                     END IF; 

                     gme_api_pub.close_step   (p_api_version              => 2.0,
                                               p_validation_level         => gme_common_pvt.g_max_errors,
                                               p_init_msg_list            => fnd_api.g_false,
                                               p_commit                   => fnd_api.g_false,
                                               x_message_count            => l_msg_cnt,
                                               x_message_list             => l_msg_data,
                                               x_return_status            => l_return_status,
                                               p_batch_step_rec           => l_step_rec,
                                               p_delete_pending           => fnd_api.g_false, --hard default value
                                               p_org_code                 => NULL,
                                               p_batch_no                 => NULL,                                   
                                               x_batch_step_rec           => l_step_rec_out);
                   ELSIF l_action = GME_BATCH_OPEN_INTERFACE.ACTION_REOPEN THEN
                    gme_api_pub.reopen_step (
                          p_api_version        => 2.0,                           
                         p_validation_level   => gme_common_pvt.g_max_errors,   
                         p_init_msg_list      => fnd_api.g_false,               
                         p_commit             => fnd_api.g_false,                
                         x_message_count      => l_msg_cnt,                     
                         x_message_list       => l_msg_data,                    
                         x_return_status      => l_return_status,               
                         p_org_code           => NULL,                    
                         p_batch_header_rec   => NULL,                          
                         p_batch_step_rec     => l_step_rec,                          
                         x_batch_step_rec     => l_step_rec_out);              

                   ELSIF l_action = GME_BATCH_OPEN_INTERFACE.ACTION_REVERT THEN
                    gme_api_pub.revert_step (
                         p_api_version          => 2.0,                              
                         p_validation_level    => gme_common_pvt.g_max_errors,      
                         p_init_msg_list       => fnd_api.g_false,                  
                         p_commit              => fnd_api.g_false,                   
                         x_message_count       => l_msg_cnt,                        
                         x_message_list        => l_msg_data,                       
                         x_return_status       => l_return_status,                  
                         p_org_code            => NULL,                       
                         p_batch_no   		      => NULL,                             
                         p_batch_step_rec      => l_step_rec,                             
                         x_batch_step_rec      => l_step_rec_out);         
                   ELSIF l_action = GME_BATCH_OPEN_INTERFACE.ACTION_UNRELEASE THEN    
                      gme_api_pub.unrelease_step(
                         p_api_version              => 2.0,
                         p_validation_level         => gme_common_pvt.g_max_errors,
                         p_init_msg_list            => fnd_api.g_false,
                         p_commit                   => fnd_api.g_false,
                         x_message_count            => l_msg_cnt,
                         x_message_list             => l_msg_data,
                         x_return_status            => l_return_status,
                         p_batch_step_rec           => l_step_rec,
                         p_batch_no                 => NULL,
                         p_org_code                 => NULL,
                         p_create_resv_pend_lots    => l_create_resv_pend_lots, 
                         x_batch_step_rec           => l_step_rec_out);                                
                    END IF;
                    
                    IF (l_return_status <> fnd_api.g_ret_sts_success) THEN
                      IF l_msg_data IS NULL THEN
                        l_msg_data := fnd_msg_pub.get (p_msg_index          => fnd_msg_pub.g_last,
                                                       p_encoded            => 'F');
                      END IF;
                      IF (l_msg_data IS NOT NULL  and l_msg_name IS NULL) THEN
                        gme_batch_open_interface.get_error_message_infor(p_message_text         => l_msg_data
                                                                        ,x_app_name             => l_app_name
                                                                        ,x_message_name         => l_msg_name
                                                                        ,x_message_detail       => l_message_detail);                   
                         
                      END IF;                                        
                      l_log_error_result  := gme_log_interface_err
                                               (P_GROUP_ID           => p_group_id,
                                                P_object_type        => l_object_type,
                                                P_INTERFACE_ID       => l_interface_id,
                                                P_COLUMN_NAME        => NULL,
                                                P_MESSAGE_TYPE       => GME_BATCH_OPEN_INTERFACE.MESSAGE_TYPE_ERROR,
                                                P_MESSAGE_NAME       => l_msg_name,
                                                P_MESSAGE_TEXT       => l_msg_data,
                                                P_APPLICATION        => l_app_name,
                                                P_SOURCE_TABLE_NAME  => 'GME_BATCH_ACTIONS_INTERFACE',
                                                P_ERR_MESSAGE_DETAIL => l_message_detail
                                                );        
                      UPDATE gme_batch_actions_interface
                          SET last_update_date = SYSDATE      ,
                              last_updated_by  = l_user_id    ,
                              request_id       = l_request_id ,
                              process_status   = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_FAILED                
                        WHERE interface_id     = l_interface_id; 
                       
                       l_failed_count := l_failed_count+1; 
                    ELSE
                      IF l_process_status = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_RUNNING THEN
                         UPDATE gme_batch_actions_interface
                            SET last_update_date = SYSDATE,      
                                last_updated_by  = l_user_id    ,
                                request_id       = l_request_id ,
                                process_status   = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_SUCCEED                
                          WHERE interface_id     = l_interface_id;  
                      ELSIF l_process_status = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_BV_WARNING THEN
                         UPDATE gme_batch_actions_interface
                            SET last_update_date = SYSDATE,      
                                last_updated_by  = l_user_id,    
                                process_status   = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_WARNING                
                          WHERE interface_id     = l_interface_id;  
                      END IF;                                                
                    END IF;             
                    COMMIT;
                  EXCEPTION
                    WHEN resource_txns_check_exception THEN  
                      l_msg_data := fnd_message.get_string('GME','GME_UNPROCESSED_RESOURCE_TXNS');               
                      fnd_file.put_line (fnd_file.LOG,l_msg_data);
                      l_message_detail.delete;
                      l_log_error_result := gme_log_interface_err
                                               (P_GROUP_ID           => p_group_id,
                                                P_object_type        => l_object_type,
                                                P_INTERFACE_ID       => l_interface_id,
                                                P_COLUMN_NAME        => NULL,
                                                P_MESSAGE_TYPE       => GME_BATCH_OPEN_INTERFACE.MESSAGE_TYPE_ERROR,
                                                P_MESSAGE_NAME       => 'GME_UNPROCESSED_RESOURCE_TXNS',
                                                P_SOURCE_TABLE_NAME  => 'GME_BATCH_ACTIONS_INTERFACE',
                                                p_err_message_detail => l_message_detail);  
                                                  
                       UPDATE gme_batch_actions_interface
                          SET last_update_date = SYSDATE      ,
                              last_updated_by  = l_user_id    ,
                              request_id       = l_request_id ,
                              process_status   = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_FAILED                
                        WHERE interface_id     = l_interface_id; 
                         
                       l_failed_count := l_failed_count+1;   
                                                               

                    WHEN OTHERS THEN
                      l_msg_data := 'There is exception occurred while processing batch/step actions,error code:'
                                    ||sqlcode||'error message:'||sqlerrm;            
                      fnd_file.put_line (fnd_file.LOG,l_msg_data);
                      l_message_detail.delete;
                      l_log_error_result := gme_log_interface_err
                                               (P_GROUP_ID           => p_group_id,
                                                P_object_type        => l_object_type,
                                                P_INTERFACE_ID       => l_interface_id,
                                                P_COLUMN_NAME        => NULL,
                                                P_MESSAGE_TYPE       => GME_BATCH_OPEN_INTERFACE.MESSAGE_TYPE_CRITICAL,
                                                P_MESSAGE_NAME       => NULL,
                                                P_MESSAGE_TEXT       => l_msg_data,
                                                P_SOURCE_TABLE_NAME  => 'GME_BATCH_ACTIONS_INTERFACE',
                                                p_err_message_detail => l_message_detail);  
                                                  
                       UPDATE gme_batch_actions_interface
                          SET last_update_date = SYSDATE      ,
                              last_updated_by  = l_user_id    ,
                              request_id       = l_request_id ,
                              process_status   = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_FAILED                
                        WHERE interface_id     = l_interface_id; 
                         
                       l_failed_count := l_failed_count+1;             
                      fnd_file.put_line (fnd_file.LOG, 'There is exception occurred while processing batch/step
                                         actions,error code:'||sqlcode||'error message:'||sqlerrm);                    
                  END;                     
                END LOOP;     
                END IF;
              
            EXCEPTION
              WHEN error_close_period THEN
                l_msg_data := fnd_message.get_string('INV','INV_NO_OPEN_PERIOD');  
                l_message_detail.delete;
                l_log_error_result := gme_log_interface_err
                                         (P_GROUP_ID           => p_group_id,
                                          P_object_type        => l_object_type,
                                          P_INTERFACE_ID       => l_interface_id,
                                          P_COLUMN_NAME        => NULL,
                                          P_MESSAGE_TYPE       => GME_BATCH_OPEN_INTERFACE.MESSAGE_TYPE_ERROR,
                                          P_MESSAGE_NAME       => 'INV_NO_OPEN_PERIOD',
                                          P_MESSAGE_TEXT       => l_msg_data,
                                          P_APPLICATION        => 'INV',
                                          P_SOURCE_TABLE_NAME  => 'GME_BATCH_ACTIONS_INTERFACE',
                                          p_err_message_detail => l_message_detail);

                 UPDATE gme_batch_actions_interface
                    SET last_update_date = SYSDATE      ,
                        last_updated_by  = l_user_id    ,
                        request_id       = l_request_id ,
                        process_status   = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_FAILED                
                  WHERE interface_id     = l_interface_id; 
                   
                 l_failed_count := l_failed_count+1;                                                           
                
              WHEN resource_txns_check_exception THEN  
                l_msg_data := fnd_message.get_string('GME','GME_UNPROCESSED_RESOURCE_TXNS');               
                fnd_file.put_line (fnd_file.LOG,l_msg_data);
                l_message_detail.delete;
                l_log_error_result := gme_log_interface_err
                                         (P_GROUP_ID           => p_group_id,
                                          P_object_type        => l_object_type,
                                          P_INTERFACE_ID       => l_interface_id,
                                          P_COLUMN_NAME        => NULL,
                                          P_MESSAGE_TYPE       => GME_BATCH_OPEN_INTERFACE.MESSAGE_TYPE_ERROR,
                                          P_MESSAGE_NAME       => 'GME_UNPROCESSED_RESOURCE_TXNS',
                                          P_SOURCE_TABLE_NAME  => 'GME_BATCH_ACTIONS_INTERFACE',
                                          p_err_message_detail => l_message_detail);  
                                            
                 UPDATE gme_batch_actions_interface
                    SET last_update_date = SYSDATE      ,
                        last_updated_by  = l_user_id    ,
                        request_id       = l_request_id ,
                        process_status   = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_FAILED                
                  WHERE interface_id     = l_interface_id; 
                   
                 l_failed_count := l_failed_count+1;                                                

              WHEN OTHERS THEN
                l_msg_data := 'There is exception occurred while processing batch/step actions,error code:'
                              ||sqlcode||'error message:'||sqlerrm;            
                fnd_file.put_line (fnd_file.LOG,l_msg_data);
                l_message_detail.delete;
                l_log_error_result := gme_log_interface_err
                                         (P_GROUP_ID           => p_group_id,
                                          P_object_type        => l_object_type,
                                          P_INTERFACE_ID       => l_interface_id,
                                          P_COLUMN_NAME        => NULL,
                                          P_MESSAGE_TYPE       => GME_BATCH_OPEN_INTERFACE.MESSAGE_TYPE_CRITICAL,
                                          P_MESSAGE_NAME       => NULL,
                                          P_MESSAGE_TEXT       => l_msg_data,
                                          P_SOURCE_TABLE_NAME  => 'GME_BATCH_ACTIONS_INTERFACE',
                                          p_err_message_detail => l_message_detail);  
                                            
                 UPDATE gme_batch_actions_interface
                    SET last_update_date = SYSDATE      ,
                        last_updated_by  = l_user_id    ,
                        request_id       = l_request_id ,
                        process_status   = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_FAILED                
                  WHERE interface_id     = l_interface_id; 
                   
                 l_failed_count := l_failed_count+1;   
                                            
                fnd_file.put_line (fnd_file.LOG, 'There is exception occurred while processing batch/step
                                   actions,error code:'||sqlcode||'error message:'||sqlerrm);
             END; 
         END LOOP;
         EXIT WHEN cur_get_sorted_import_data%NOTFOUND;
      END LOOP;
    --END IF;
    CLOSE cur_get_sorted_import_data;
       
    IF g_debug <= gme_debug.g_log_procedure THEN
      gme_debug.put_line('all of batch action and step action records in the group_id:'||p_group_id||
                         ' have been processed,the failed records number:'||l_failed_count);
    END IF;  
    
    IF p_purge_flag = 'Y' THEN 
      --Modified by Shaliu Chen, bug 17452045, delete error messages first, then delete interface table
      DELETE FROM gme_intf_errors gie
       WHERE source_table_name = 'GME_BATCH_ACTIONS_INTERFACE'
         AND EXISTS (SELECT 1
                           FROM gme_batch_actions_interface gbai
                          WHERE gie.interface_id = gbai.interface_id
                            AND gbai.group_id = p_group_id
                            AND gbai.process_status = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_SUCCEED );     
    
      DELETE FROM gme_batch_actions_interface
       WHERE group_id = p_group_id
         AND process_status = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_SUCCEED;                                                
         
      COMMIT;
      
    END IF;

       
/*    --purge gme_batch_actions_interface after all of records have been processed.       
    IF p_purge_flag = 'Y' THEN
      --calling PURGE concurrent program.
      l_purge_request_id := fnd_request.submit_request(application => 'GME',
                                                       program     => 'GMEPRGI',
                                                       sub_request => FALSE,
                                                       argument1   => p_group_id,
                                                       argument2   => GME_BATCH_OPEN_INTERFACE.GME_ACTIONS_INTF_CODE,
                                                       argument3   => GME_BATCH_OPEN_INTERFACE.PURGE_OPTION_SUCCESSFUL_ROWS);
      IF l_purge_request_id = 0 THEN
        
        
        l_conc_status := fnd_concurrent.set_completion_status 
                                  ('WARNING', 
                                   fnd_message.get_string('GME','GME_LAUNCH_PURGE_FAIL'));
        l_message_detail.delete;                           
        l_log_error_result :=gme_log_interface_err (
                                                    p_group_id  => p_group_id,
                                                    P_object_type => -1,
                                                    p_interface_id => -1,
                                                    p_column_name => NULL,
                                                    p_message_type => GME_BATCH_OPEN_INTERFACE.MESSAGE_TYPE_ERROR,
                                                    p_message_name => 'GME_LAUNCH_PURGE_FAIL',
                                                    p_source_table_name => 'GME_BATCH_ACTIONS_INTERFACE',
                                                    p_err_message_detail => l_message_detail); 
      ELSE
        COMMIT;     

      END IF;
    END IF;   */ 
    
    /*If there is more than 1 record in the GME_INTF_ERRORS table,then set concurrent
      ,then set concurrent request status to WARNING*/
    SELECT count(1)
      INTO l_error_msg_count
      FROM GME_INTF_ERRORS
     WHERE group_id = p_group_id
       AND message_type = GME_BATCH_OPEN_INTERFACE.MESSAGE_TYPE_ERROR;
       
    SELECT count(1)
      INTO l_critical_msg_count
      FROM GME_INTF_ERRORS
     WHERE group_id = p_group_id
       AND message_type = GME_BATCH_OPEN_INTERFACE.MESSAGE_TYPE_CRITICAL;          
    IF l_critical_msg_count >0 THEN       
      l_conc_status := FND_CONCURRENT.SET_COMPLETION_STATUS('ERROR',''); 
    ELSE
      IF l_error_msg_count >0 THEN
        l_conc_status := FND_CONCURRENT.SET_COMPLETION_STATUS('WARNING',fnd_message.get_string('GME','GME_INTF_CP_WARN')); 
      END IF;
    END IF;      
  EXCEPTION
    --If bulk validation procedure throw out exception,set concurrent request status to ERROR.       
    WHEN bulk_validation_exception THEN
      l_conc_status := FND_CONCURRENT.SET_COMPLETION_STATUS('ERROR','');
    WHEN OTHERS THEN
      ret_code := fnd_api.g_ret_sts_error;
      err_buf := 'There is exception occurred in the batch_actions_worker procedure,
                           error code:'||sqlcode||',error message:'||sqlerrm;
      l_conc_status := FND_CONCURRENT.SET_COMPLETION_STATUS('ERROR','');                       
  END;  
 

 /*================================================================================
  Procedure
    get_message
  Description
    This procedure is used for getting the user language message text accroding 
    to Fnd Message Name.

  Parameters

  History:
    Shalchen       06-DEC-2012  Creation
================================================================================*/
FUNCTION GET_MESSAGE (msg_name                   VARCHAR2,
                      PARAM1                     VARCHAR2 DEFAULT NULL,
                      PARAM2                     VARCHAR2 DEFAULT NULL,
                      PARAM3                     VARCHAR2 DEFAULT NULL,
                      PARAM4                     VARCHAR2 DEFAULT NULL,
                      PARAM5                     VARCHAR2 DEFAULT NULL,
                      PARAM6                     VARCHAR2 DEFAULT NULL,
                      TOKEN1                     VARCHAR2 DEFAULT NULL,
                      TOKEN2                     VARCHAR2 DEFAULT NULL,
                      TOKEN3                     VARCHAR2 DEFAULT NULL,
                      TOKEN4                     VARCHAR2 DEFAULT NULL,
                      TOKEN5                     VARCHAR2 DEFAULT NULL,
                      TOKEN6                     VARCHAR2 DEFAULT NULL,
                      ERROR_TEXT      OUT NOCOPY VARCHAR2)
   RETURN INTEGER
IS
BEGIN
   FND_MESSAGE.SET_NAME ('GME', SUBSTRB (msg_name, 1, 30));

   IF PARAM1 IS NOT NULL    THEN
      FND_MESSAGE.set_token (PARAM1, TOKEN1);
   END IF;

   IF PARAM2 IS NOT NULL    THEN
      FND_MESSAGE.set_token (PARAM2, TOKEN2);
   END IF;

   IF PARAM3 IS NOT NULL    THEN
      FND_MESSAGE.set_token (PARAM3, TOKEN3);
   END IF;

   IF PARAM4 IS NOT NULL    THEN
      FND_MESSAGE.set_token (PARAM4, TOKEN4);
   END IF;

   IF PARAM5 IS NOT NULL    THEN
      FND_MESSAGE.set_token (PARAM5, TOKEN5);
   END IF;

   IF PARAM6 IS NOT NULL    THEN
      FND_MESSAGE.set_token (PARAM6, TOKEN6);
   END IF;

   ERROR_TEXT := FND_MESSAGE.GET;
   RETURN (0);

EXCEPTION

   when OTHERS then
   
      gme_common_pvt.log_message(SUBSTRB('get_message: ' || SQLERRM, 1, 240));
      return (1);

END get_message;

 /*================================================================================
  Procedure
    gme_log_interface_err
  Description
    This procedure is used for inserting a record into GME_INTF_ERRORS table to 
    record error messages.

  Parameters

  History:
    Shalchen       06-DEC-2012  Creation
================================================================================*/
FUNCTION  gme_log_interface_err (P_GROUP_ID             NUMBER,
                                P_object_type          VARCHAR2,
                                P_INTERFACE_ID         NUMBER,
                                P_COLUMN_NAME          VARCHAR2 DEFAULT NULL,
                                P_MESSAGE_TYPE         VARCHAR2 DEFAULT 'E',
                                P_MESSAGE_NAME         VARCHAR2,
                                P_MESSAGE_TEXT         VARCHAR2 DEFAULT NULL,
                                P_APPLICATION          VARCHAR2 DEFAULT 'GME',
                                P_SOURCE_TABLE_NAME    VARCHAR2,
                                P_ERR_MESSAGE_DETAIL   GME_BATCH_OPEN_INTERFACE.T_MESSAGE_TABLE,
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
     translated_text   fnd_new_messages.MESSAGE_TEXT%TYPE;
   l_sysdate         DATE := SYSDATE;
   returnCode NUMBER; 
   l_param1  VARCHAR2(30);
   l_param2  VARCHAR2(30);
   l_param3  VARCHAR2(30);
   l_param4  VARCHAR2(30);
   l_param5  VARCHAR2(30);
   l_param6  VARCHAR2(30);
   l_token1  VARCHAR2(60);
   l_token2  VARCHAR2(60);
   l_token3  VARCHAR2(60);
   l_token4  VARCHAR2(60);
   l_token5  VARCHAR2(60);
   l_token6  VARCHAR2(60);
   
   
BEGIN
    
     translated_text:= P_MESSAGE_TEXT; 
   IF P_MESSAGE_TEXT IS NULL AND P_MESSAGE_NAME IS NOT NULL THEN 
       returnCode := GET_MESSAGE(P_MESSAGE_NAME, 
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
         translated_text   
         );
   END IF; 
   
   IF P_ERR_MESSAGE_DETAIL.COUNT > 0 THEN
     FOR i in 1..P_ERR_MESSAGE_DETAIL.COUNT LOOP
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
   ELSE
     l_param1 := p_param1;   
     l_param2 := p_param2;
     l_param3 := p_param3;
     l_param4 := p_param4;
     l_param5 := p_param5;
     l_param6 := p_param6;
     
     l_token1 := p_token1;
     l_token2 := p_token2;
     l_token3 := p_token3;
     l_token4 := p_token4;
     l_token5 := p_token5;
     l_token6 := p_token6;
   END IF;
   
   
   INSERT INTO GME_INTF_ERRORS (GROUP_ID,
                             OBJECT_TYPE,
                             INTERFACE_ID,
                             COLUMN_NAME,
                             MESSAGE_NAME,
                             MESSAGE_TYPE,
                             MESSAGE_TEXT,
                             MSG_APPL_SNAME,
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
             P_MESSAGE_NAME,
             P_MESSAGE_TYPE,
             translated_text,
             P_APPLICATION,
             P_SOURCE_TABLE_NAME,
             NVL (FND_GLOBAL.conc_request_id, -1),
             L_PARAM1,
             L_PARAM2,
             L_PARAM3,
             L_PARAM4,
             L_PARAM5,
             L_PARAM6,
             L_TOKEN1,
             L_TOKEN2,
             L_TOKEN3,
             L_TOKEN4,
             L_TOKEN5,
             L_TOKEN6,
             NVL (FND_GLOBAL.user_id, -1),
             l_sysdate,
             NVL (FND_GLOBAL.user_id, -1),
             NVL (FND_GLOBAL.login_id, -1),
             l_sysdate);      
   RETURN (0);
EXCEPTION
   WHEN OTHERS
   THEN
      gme_common_pvt.log_message(SUBSTRB('gme_log_interface_err: ' || SQLERRM, 1, 240));
      RETURN (SQLCODE);
END gme_log_interface_err;


 /*================================================================================
  Procedure
    validation_error_process
  Description
    This procedure is used for inserting error records into GME_INTF_ERRORS table to 
    record the errors generated by batch_actions_bulk_validation procedure.

  Parameters

  History:
    Shalchen       06-DEC-2012  Creation
================================================================================*/
PROCEDURE validation_error_process   (p_request_id       IN  NUMBER
                                     ,p_group_id         IN  NUMBER
                                     ,p_application      IN  VARCHAR2 DEFAULT 'GME'
                                     ,p_message_name	   IN  VARCHAR2
                                     ,p_column_name      IN  VARCHAR2 DEFAULT NULL
                                     ,p_message_type     IN  VARCHAR2 DEFAULT 'E'
                                     ,p_message_text     IN  VARCHAR2 DEFAULT NULL
                                     ,p_param1           IN  VARCHAR2 DEFAULT NULL
                                     ,p_param2           IN  VARCHAR2 DEFAULT NULL
                                     ,p_param3           IN  VARCHAR2 DEFAULT NULL
                                     ,p_param4           IN  VARCHAR2 DEFAULT NULL
                                     ,p_param5           IN  VARCHAR2 DEFAULT NULL
                                     ,p_param6           IN  VARCHAR2 DEFAULT NULL                                    
                                     ,x_return_message       OUT NOCOPY      VARCHAR2
                                     ,x_return_status        OUT NOCOPY      VARCHAR2)
IS
  l_message_text     VARCHAR2(2000);
  l_message_count    NUMBER;
  l_get_msg_result   NUMBER;
  l_table_name       CONSTANT VARCHAR2 (30)   := 'GME_BATCH_ACTIONS_INTERFACE';
  l_object_type      CONSTANT VARCHAR2 (3)    := '100';
  l_login            NUMBER                   := NVL (FND_GLOBAL.login_id, -1);
  l_userid           NUMBER                   := NVL (FND_GLOBAL.user_id, -1);
  l_request_id       NUMBER                   := NVL (FND_GLOBAL.conc_request_id,-1);
BEGIN
  x_return_status := fnd_api.g_ret_sts_success;                               
                               
  IF (p_message_name = 'GME_INVALID_DATE_RANGE') THEN
    l_get_msg_result:= get_message(msg_name   => p_message_name   ,
                                   param1     => 'DATE1'          ,
                                   param2     => 'DATE2'          ,
                                   token1     => p_param1         ,
                                   token2     => p_param2         ,
                                   error_text => l_message_text   );      
      INSERT INTO gme_intf_errors (group_id, 
                                 object_type, 
                                 interface_id, 
                                 msg_appl_sname,
                                 column_name, 
                                 message_name, 
                                 message_type, 
                                 message_text, 
                                 source_table_name, 
	                               request_id, 
                                 param1,
                                 param2,                                
                                 token1,
                                 token2,
                                 last_updated_by, 
                                 creation_date, 
                                 created_by, 
                                 last_update_login, 
                                 last_update_date)
           SELECT                group_id, 
                                 object_type, 
                                 interface_id, 
                                 p_application,
                                 p_column_name, 
                                 p_message_name, 
                                 p_message_type, 
                                 l_message_text, 
                                 l_table_name,
                                 l_request_id, 
                                 'DATE1',
                                 'DATE2',
                                 p_param1||'('||decode(p_column_name,'CLOSE_DATE',gbai.close_date,gbai.actual_cmplt_date)||')',
                                 p_param2||'('||decode(p_column_name,'CLOSE_DATE',gbh.actual_cmplt_date,gbh.actual_start_date)||')',
                                 l_userid, 
                                 SYSDATE, 
                                 l_userid, 
                                 l_login, 
                                 SYSDATE
             FROM   gme_batch_actions_interface gbai,
                    gme_batch_header gbh
            WHERE   gbai.process_status = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_BV_INVALID
              AND   gbai.group_id = p_group_id
              AND   gbai.object_type = GME_BATCH_OPEN_INTERFACE.OBJECTYPE_BATCH
              AND   gbai.int_batch_id = gbh.batch_id;
              
      INSERT INTO gme_intf_errors (group_id, 
                                 object_type, 
                                 interface_id,
                                 msg_appl_sname, 
                                 column_name, 
                                 message_name, 
                                 message_type, 
                                 message_text, 
                                 source_table_name, 
	                               request_id, 
                                 param1,
                                 param2,                                
                                 token1,
                                 token2,
                                 last_updated_by, 
                                 creation_date, 
                                 created_by, 
                                 last_update_login, 
                                 last_update_date)
           SELECT                group_id, 
                                 object_type, 
                                 interface_id, 
                                 p_application,
                                 p_column_name, 
                                 p_message_name, 
                                 p_message_type, 
                                 l_message_text, 
                                 l_table_name,
                                 l_request_id, 
                                 'DATE1',
                                 'DATE2',
                                 p_param1||'('||decode(p_column_name,'CLOSE_DATE',gbai.close_date,gbai.actual_cmplt_date)||')',
                                 p_param2||'('||decode(p_column_name,'CLOSE_DATE',gbs.actual_cmplt_date,gbs.actual_start_date)||')',
                                 l_userid, 
                                 SYSDATE, 
                                 l_userid, 
                                 l_login, 
                                 SYSDATE
             FROM   gme_batch_actions_interface gbai,
                    gme_batch_steps gbs
            WHERE   gbai.process_status = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_BV_INVALID
              AND   gbai.group_id = p_group_id
              AND   gbai.object_type = GME_BATCH_OPEN_INTERFACE.OBJECTYPE_STEP
              AND   gbai.int_batchstep_id = gbs.batchstep_id;    
    ELSIF (p_message_name = 'GME_BATCHID_IGNORED') THEN
      l_get_msg_result:= get_message(msg_name   => p_message_name   ,
                                     error_text => l_message_text   ); 
                                     
      INSERT INTO gme_intf_errors (group_id, 
                                 object_type, 
                                 interface_id, 
                                 msg_appl_sname,
                                 column_name, 
                                 message_name, 
                                 message_type, 
                                 message_text, 
                                 source_table_name, 
	                               request_id, 
                                 last_updated_by, 
                                 creation_date, 
                                 created_by, 
                                 last_update_login, 
                                 last_update_date)
           SELECT                group_id, 
                                 object_type, 
                                 interface_id, 
                                 p_application,
                                 p_column_name,
                                 p_message_name, 
                                 p_message_type, 
                                 l_message_text, 
                                 l_table_name,
                                 l_request_id, 
                                 l_userid, 
                                 SYSDATE, 
                                 l_userid, 
                                 l_login, 
                                 SYSDATE
             FROM   gme_batch_actions_interface gbai
            WHERE   gbai.process_status = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_BV_WARNING
              AND   (gbai.group_id = p_group_id 
                     OR (gbai.group_id IS NULL AND gbai.request_id = p_request_id));                                                    
    ELSE
      l_get_msg_result:= get_message(msg_name   => p_message_name   ,
                                     error_text => l_message_text   ); 
                                         
      INSERT INTO gme_intf_errors (group_id, 
                                 object_type, 
                                 interface_id, 
                                 msg_appl_sname,
                                 column_name, 
                                 message_name, 
                                 message_type, 
                                 message_text, 
                                 source_table_name, 
	                               request_id, 
                                 last_updated_by, 
                                 creation_date, 
                                 created_by, 
                                 last_update_login, 
                                 last_update_date)
           SELECT                group_id, 
                                 object_type, 
                                 interface_id, 
                                 p_application,
                                 p_column_name,
                                 p_message_name, 
                                 p_message_type, 
                                 l_message_text, 
                                 l_table_name,
                                 l_request_id, 
                                 l_userid, 
                                 SYSDATE, 
                                 l_userid, 
                                 l_login, 
                                 SYSDATE
             FROM   gme_batch_actions_interface gbai
            WHERE   gbai.process_status = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_BV_INVALID
              AND   (gbai.group_id = p_group_id 
                     OR (gbai.group_id IS NULL AND gbai.request_id = p_request_id));
    END IF;
  
    UPDATE gme_batch_actions_interface
       SET process_status   = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_FAILED,
           request_id       = p_request_id ,
           last_update_date = SYSDATE      ,
           last_updated_by  = l_userid               
       
     WHERE (group_id = p_group_id
            OR (group_id IS NULL AND request_id = p_request_id))  
       AND process_status = GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_BV_INVALID;
    
    
       
EXCEPTION
  WHEN OTHERS THEN
    x_return_status := fnd_api.g_ret_sts_error;
    x_return_message := 'There is error occurred while log error message,error code:'||sqlcode||',error message:'||sqlerrm;        
    fnd_file.put_line (fnd_file.LOG, x_return_message);
    Return; 
       
END validation_error_process;


 /*================================================================================
  Procedure
    check_unprocessed_resc_trxn
  Description
    This procedure is used for checking whether there is any unprocessed resource
    transaction in the gme_resource_txns_interface table before Close Batch/Step 
    actions.

  Parameters
    p_object_type       The corresponding object type for the batch/step 
                        action record which is checking
    p_int_batch_id      The corresponding batch id for the batch/step 
                        action record which is checking
    p_int_batchstep_id The corresponding batchstep id for the batch/step
                       action record which is checking  
  History:
    Shalchen       06-DEC-2012  Creation
================================================================================*/
PROCEDURE check_unprocessed_resc_trxn(p_object_type           IN NUMBER
                                     ,p_int_batch_id          IN NUMBER
                                     ,p_int_batchstep_id      IN NUMBER
                                     ,x_return_message       OUT NOCOPY      VARCHAR2
                                     ,x_return_status        OUT NOCOPY      VARCHAR2)
IS 
  l_first_count  NUMBER;
  l_second_count NUMBER;
BEGIN
  x_return_status := fnd_api.g_ret_sts_success;
  l_first_count := 0;
  l_second_count := 0;
  IF(p_object_type = GME_BATCH_OPEN_INTERFACE.OBJECTYPE_BATCH) THEN
    SELECT count(1)
      INTO l_first_count
      FROM gme_resource_txns_interface grti,
           gme_batch_step_resources gbsr
     WHERE grti.batchstep_resource_id = gbsr.batchstep_resource_id
       AND grti.process_status in (1,2,3)
       AND gbsr.batch_id = p_int_batch_id;    
       
    SELECT count(1)
      INTO l_second_count
      FROM gme_resource_txns_interface grti,
           gme_batch_header gbh,
           mtl_parameters mp
     WHERE grti.process_status in (1,2,3)
       AND grti.batch_no = gbh.batch_no
       AND grti.organization_code = mp.organization_code
       AND mp.organization_id = gbh.organization_id
       AND gbh.batch_id = p_int_batch_id;        
  
  ELSIF (p_object_type = GME_BATCH_OPEN_INTERFACE.OBJECTYPE_STEP) THEN
    SELECT count(1)
      INTO l_first_count    
      FROM gme_resource_txns_interface grti,
           gme_batch_step_resources gbsr
     WHERE grti.batchstep_resource_id = gbsr.batchstep_resource_id
       AND grti.process_status in (1,2,3)
       AND gbsr.batch_id = p_int_batch_id
       AND gbsr.batchstep_id = p_int_batchstep_id;  
       
    SELECT count(1)
      INTO l_second_count    
      FROM gme_resource_txns_interface grti,
           gme_batch_steps gbs,
           gme_batch_header gbh,
           mtl_parameters mp      
     WHERE grti.process_status in (1,2,3)
       AND grti.batch_step_no = gbs.batchstep_no
       AND grti.batch_no = gbh.batch_no
       AND grti.organization_code = mp.organization_code
       AND gbs.batch_id = gbh.batch_id
       AND gbh.organization_id = mp.organization_id
       AND gbs.batchstep_id = p_int_batchstep_id;          
  
  END IF;
  
  IF (l_first_count >0 OR l_second_count >0) THEN
    x_return_status := fnd_api.g_ret_sts_error;
  ELSE
    x_return_status := fnd_api.g_ret_sts_success;
  END IF;
    
EXCEPTION
  WHEN OTHERS THEN  
    x_return_status := fnd_api.g_ret_sts_error;
    x_return_message := 'There is error occurred while checking unprocessed resource transaction for close action,
                         error code:'||sqlcode||',error message:'||sqlerrm;        
    fnd_file.put_line (fnd_file.LOG, x_return_message);
    Return;     
END;                                         
  


end GME_BATCH_ACTION_OPEN_INTF;

/       
COMMIT ;
EXIT;   
