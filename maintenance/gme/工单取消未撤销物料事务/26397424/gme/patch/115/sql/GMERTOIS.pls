/* +======================================================================+ */
/* |    Copyright (c) 2005, 2014 Oracle and/or its affiliates.           | */
/* |                         All rights reserved.                         | */
/* |                           Version 12.0.0                             | */
/* +======================================================================+ */
REM dbdrv: sql ~PROD ~PATH ~FILE none none none package &phase=pls \
REM dbdrv: checkfile(120.0.12010000.2=120.0.12020000.2)(115.2=120.1):~PROD:~PATH:~FILE     

create or replace package GME_RESOURCE_TXN_OPEN_INTF AS
/* $Header: GMERTOIS.pls 120.0.12020000.2 2014/07/25 09:35:45 shalchen noship $ */

      -- Author  : MICHAEL
      -- Package Created : 1/24/2013 3:12:52 PM
      -- Purpose : process resource transaction for open interface project

      ALL_TABLE_NAME_CODE         CONSTANT NUMBER := 0; --purge all below interface table
      GME_BTCH_INTF_CODE          CONSTANT NUMBER := 1; --purge interface table   GME_BATCH_HEADER_INTERFACE and GME_BATCH_DTLS_INTERFACE
      GME_ACTIONS_INTF_CODE       CONSTANT NUMBER := 2; --purge interface table    GME_BATCH_ACTIONS_INTERFACE
      GME_RESOURCE_TXNS_INTF_CODE CONSTANT NUMBER := 3; -- purge interface table   GME_RESOURCE_TXNS_INTERFACE
      GME_INTF_ERRORS_CODE        CONSTANT NUMBER := 4; -- purge interface table   GME_INTF_ERRORS  

      PURGE_OPTION_ALL_ROWS        CONSTANT NUMBER := 0; -- purge all rows
      PURGE_OPTION_SUCCESSFUL_ROWS CONSTANT NUMBER := 1; --purge successful rows

      /*  MESSAGE_TYPE_WARNING  CONSTANT VARCHAR2(1) := 'W';
      MESSAGE_TYPE_CRITICAL CONSTANT VARCHAR2(1) := 'C';
      MESSAGE_TYPE_ERROR    CONSTANT VARCHAR2(1) := 'E';
      
      PROCESS_STATUS_PENDING CONSTANT NUMBER := 1;
      PROCESS_STATUS_RUNNING CONSTANT NUMBER := 2;
      PROCESS_STATUS_FAILED  CONSTANT NUMBER := 3;
      PROCESS_STATUS_SUCCEED CONSTANT NUMBER := 7;
      
      PROCESS_STATUS_BV_INVALID CONSTANT NUMBER := 255;
      PROCESS_STATUS_BV_WARNING CONSTANT NUMBER := 511;*/
      

      /*ER 19161894  Shaliu Chen 18-JUL-2014
        Added for OPM OSP Project
      */

      TYPE Res_Rec_Type IS RECORD
      (
      INTERFACE_ID              NUMBER       ,
      ORGANIZATION_CODE         VARCHAR2(3)  ,
      BATCH_NO                  VARCHAR2(32) ,
      BATCH_STEP_NO             NUMBER       ,
      ACTIVITY                  VARCHAR2(16) ,
      RESOURCES                 VARCHAR2(16) ,
      RESOURCE_INSTANCE_NO      NUMBER       ,
      BATCH_ID                  NUMBER       ,
      INT_BATCH_ID              NUMBER       ,
      BATCHSTEP_RESOURCE_ID     NUMBER       ,
      INT_BATCHSTEP_RESOURCE_ID NUMBER       ,
      RESOURCE_USAGE            NUMBER       ,
      TRANS_DATE                DATE         ,
      REASON_NAME               VARCHAR2(30) ,
      START_DATE                DATE         ,
      END_DATE                  DATE         ,
      INSTANCE_ID               NUMBER       ,
      INT_RESOURCE_INSTANCE_ID  NUMBER       ,
      INT_ORGANIZATION_ID       NUMBER       ,
      TRANS_QTY_UM              VARCHAR2(3)  ,
      REASON_ID                 NUMBER       ,
      CREATION_DATE             DATE         ,
      LAST_UPDATE_DATE          DATE         ,
      CREATED_BY                NUMBER       ,
      LAST_UPDATED_BY           NUMBER       ,
      LAST_UPDATE_LOGIN         NUMBER       ,
      PROCESS_PHASE             VARCHAR2(32) ,
      PROCESS_STATUS            VARCHAR2(32) ,
      REQUEST_ID                NUMBER       ,
      GROUP_ID                  NUMBER       ,
      COST_SOURCE               NUMBER       ,
      PO_HEADER_ID              NUMBER       ,
      PO_LINE_ID                NUMBER       ,
      ACTUAL_RESOURCE_RATE      NUMBER       ,
      CURRENCY_CODE             VARCHAR2(15) ,
      CURRENCY_CONVERSION_DATE  DATE         ,
      CURRENCY_CONVERSION_TYPE  VARCHAR2(30) ,
      CURRENCY_CONVERSION_RATE  NUMBER       ,
      RCV_TRANSACTION_ID        NUMBER       );
      /*END ER 19161894*/

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
                                         x_return_status  OUT NOCOPY VARCHAR2);

      /* 
      # PROCEDURE validate_resource_transactions
      # DESCRIPTION
      #    bulk validate resource transaction table GME_RESOURCE_TXNS_INTERFACE.
      # HISTORY
      #  Created : 1/24/2013  michael  
      */
      PROCEDURE resource_txns_bulk_validation(p_group_id IN NUMBER, p_request_id IN NUMBER, x_return_message OUT NOCOPY VARCHAR2, x_return_status OUT NOCOPY VARCHAR2);

      /* 
      #  
      # DESCRIPTION
      #    this procedure is used to launch resource transaction worker.
      # HISTORY
      #  Created : 1/24/2013  michael  
      */
      PROCEDURE launch_rsc_txn_worker(err_buf OUT NOCOPY VARCHAR2, ret_code OUT NOCOPY VARCHAR2, p_group_id IN NUMBER, p_purge_flag IN VARCHAR2 DEFAULT 'Y');

      /* 
      #  
      # DESCRIPTION
      #    this procedure is used to derive internal id:int_batch_id, int_batchstep_resource_id, int_organization_id
      # HISTORY
      #  Created : 1/24/2013  michael  
      */
      PROCEDURE derive_int_id_columns(p_request_id IN NUMBER, x_return_message OUT NOCOPY VARCHAR2, x_return_status OUT NOCOPY VARCHAR2);

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
                                              p_max_rows_of_per_worker IN NUMBER DEFAULT 1000);

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
                                       x_return_status         OUT NOCOPY VARCHAR2);



end GME_RESOURCE_TXN_OPEN_INTF;

/       
COMMIT ;
EXIT;   
