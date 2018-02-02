/* +======================================================================+ */
/* |    Copyright (c) 2005, 2016 Oracle and/or its affiliates.           | */
/* |                         All rights reserved.                         | */
/* |                           Version 12.0.0                             | */
/* +======================================================================+ */
REM dbdrv: sql ~PROD ~PATH ~FILE none none none package &phase=plb \
REM dbdrv: checkfile(120.16.12010000.17=120.20.12020000.13)(120.11.12000000.11=120.16.12010000.7)(120.11.12000000.10=120.16.12010000.6)(120.11.12000000.9=120.16.12010000.5)(115.57=120.2):~PROD:~PATH:~FILE
SET VERIFY OFF;
WHENEVER SQLERROR EXIT FAILURE ROLLBACK;
WHENEVER OSERROR EXIT FAILURE ROLLBACK;

CREATE OR REPLACE PACKAGE BODY gme_resource_engine_pvt AS
/*  $Header: GMEVRXNB.pls 120.20.12020000.13 2016/12/21 09:21:26 shalchen ship $
 *****************************************************************
 *                                                               *
 * Package  GME_RESOURCE_ENGINE_PVT                              *
 *                                                               *
 * Contents CREATE_PENDING_TRANS                                 *
 *          CREATE_COMPLETED_TRANS                               *
 *          UPDATE_PENDING_TRANS                                 *
 *          DELETE_PENDING_TRANS                                 *
 *          FETCH_ALL_RESOURCES                                  *
 *          BUILD_RESOURCE_TRAN               *
 *     FETCH_ACTIVE_RESOURCES           *
 *                                                               *
 * Use      This is the private layer of the GME Resource        *
 *          Transaction Processor.                               *
 *                                                               *
 * History
 * 17-JAN-2006 Susruth D. Bug#4917189                            *
 *       Added Update statements instead of dbl for updating     *
 *       the reverse_id in CONSOLIDATE_BATCH_RESOURCES.          *
 * 09-JUN-2006 SivakumarG Bug#5231180                            *
 *       Code added to resource_dtl_process to recalculate charge*
 *       if there is any insertion/updation/deletion if resouces *
 *  27-SEP-2007 Swapna K Bug#6154309
 *    Added the condition to check for ASQC batch before assigning the *
 *     overrided_protected_ind column of the resource transactions.    *
 *  07-NOV-2007 Swapna K Bug#6607524
 *     Changed the hardcoded organization id to that of the batch      *
 *      header's organization id in resource_dtl_process procedure     *
 
 *  G. Muratore   19-MAR-2010  Bug 8751983     
 *     Added logic to allow a different sort for resource transactions.
 *     Also, use IB date entered by user for resource transaction reversals when necessary.
 *     PROCEDURES: consolidate_batch_resources and fetch_active_resources
 
 *  G. Muratore   20-APR-2010  Bug 9506856     
 *     Make call to insert data to GMF layer tables since these apis insert
 *     directly into gme transaction tables bypassing the gtmp table.
 *     PROCEDURES: insert_incr_actual_rsrc_txn and insert_timed_actual_rsrc_txn
 
 *  G. Muratore   20-APR-2010  Bug 12576806     
 *     Initialize organization_id when inserting a new resource.
 *     PROCEDURES: resource_dtl_process
 
 *  G. Muratore   14-NOV-2011  Bug 13345631     
 *     Date Validations commented out to allow transactions after completion date.
 *     PROCEDURES: validate_rsrc_txn_param
   
 *  G. Muratore   21-MAR-2013  Bug 16327528     
 *     Use a different function to check for closed period.
 *     PROCEDURES: consolidate_batch_resources     

 *  G. Muratore   13-MAY-2013  Bug 16325776     
 *     Avoid unnecessary reversals even for completed transactions.
 *     PROCEDURES: consolidate_batch_resources   
  
 *  Shaliu Chen     18-JUL-2014  ER 19161894                                                 
 *     Modify resource_dtl_process to sync plan total usage change to corresponding PO    
 
 *  Shaliu Chen     10-APR-2015  ER 20809749
 *     Modify resource_dtl_process to support multiple osp steps. 
 
 *  Shaliu Chen     05-MAY-2015  ER 20938455
 *     Modify for Batch On Hold enhancement,add validation to raise an error 
 *     if batch is on hold  
 
 *  Shaliu Chen     18-MAY-2015  BUG 201102335
 *     Modify for Batch On Hold enhancement,add validation into procedure
 *    consolidate_batch_resources ot raise error if batch is on hold
 
 *  Shaliu Chen     16-JUN-2015  BUG 21208206
 *     Modify for Batch On Hold enhancement,set resource txn date,resource
 *     start date and resource end date are all equal to sysdate if the 
 *     resource txn is created automatically by complete step/batch action
 *     or incremental backflush action and the resource txn period overlap
 *     with the batch hold period.
 *     PROCEDURES: consolidate_batch_resources 
 
  *  Shaliu Chen     16-JUN-2016  BUG 24422344
  *  Modify  start_cmplt_actual_rsrc_txn tp add validation for resource instance ER
  *
  
  *  Shaliu Chen     21-DEC-2016  BUG 25259887
  *  Modify validate_rsrc_txn_param to get correct instance_id
  *  
 *****************************************************************
*/
/*  Global variables   */
   g_debug               VARCHAR2 (5)  := fnd_profile.VALUE ('AFLOG_LEVEL');
   g_pkg_name   CONSTANT VARCHAR2 (30) := 'GME_RESOURCE_ENGINE_PVT';

/*===========================================================================================
Procedure
  fetch_all_resources
Description
  This particular procedure is used to fetch all the resource transactions for a particular
  trans id or a batch id
Parameters
  p_resource_rec     The resource transaction rec
  x_resource_tbl     All the resource transactions pertaining to the criteria.
  x_return_status    outcome of the API call
            S - Success
            E - Error
            U - Unexpected error
=============================================================================================*/
   PROCEDURE fetch_all_resources (
      p_resource_rec    IN              gme_resource_txns_gtmp%ROWTYPE
     ,x_resource_tbl    OUT NOCOPY      gme_common_pvt.resource_transactions_tab
     ,x_return_status   OUT NOCOPY      VARCHAR2
     ,p_active_trans    IN              NUMBER DEFAULT 0          -- B3425554
                                                        )
   IS
      TYPE query_ref IS REF CURSOR;

      get_rsrc              query_ref;
      l_where               VARCHAR2 (2000);
      l_api_name   CONSTANT VARCHAR2 (30)   := 'FETCH_ALL_TRANS';
      l_return_status       VARCHAR2 (1)    := fnd_api.g_ret_sts_success;
      l_line_index          BINARY_INTEGER  := 1;
      l_cursor              BINARY_INTEGER;
      l_debug               VARCHAR2 (2000);
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      /*  Initialize API return status to sucess  */
      x_return_status := fnd_api.g_ret_sts_success;

      -- Determine if any of the key values are present
      IF (    (p_resource_rec.poc_trans_id IS NOT NULL)
          OR (p_resource_rec.poc_trans_id <> fnd_api.g_miss_num) ) THEN
         l_debug := 'Build Where Clause Cursor Ref 1';
         l_where := 'POC_TRANS_ID =:poc_trans_id ';
         l_cursor := 1;
      ELSIF (    (p_resource_rec.doc_id IS NOT NULL)
             OR (p_resource_rec.doc_id <> fnd_api.g_miss_num) ) THEN
         l_debug := 'Build Where Clause Cursor Ref 2';
         l_where := ' DOC_ID =:doc_id';
         l_cursor := 2;
      ELSE
         l_debug := 'Report An Error';
      END IF;

-- B3425554 only fetch changed trxns  performance enhancenment
      IF p_active_trans = 1 THEN
         l_where :=
                   l_where || ' AND action_code IN (''ADD'',''DEL'',''UPD'')';
      END IF;

      IF l_cursor = 1 THEN
         OPEN get_rsrc
          FOR    ' SELECT * FROM GME_RESOURCE_TXNS_GTMP
            WHERE '
              || l_where USING p_resource_rec.poc_trans_id;
      ELSIF l_cursor = 2 THEN
         OPEN get_rsrc
          FOR    ' SELECT * FROM GME_RESOURCE_TXNS_GTMP
            WHERE '
              || l_where USING p_resource_rec.doc_id;
      END IF;

      LOOP
         FETCH get_rsrc
          INTO x_resource_tbl (l_line_index);

         EXIT WHEN get_rsrc%NOTFOUND;
         l_line_index := l_line_index + 1;
      END LOOP;

      CLOSE get_rsrc;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;
   EXCEPTION
      WHEN fnd_api.g_exc_error THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN fnd_api.g_exc_unexpected_error THEN
         x_return_status := fnd_api.g_ret_sts_unexp_error;

         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line ('IN UN ' || SQLERRM);
         END IF;
      WHEN OTHERS THEN
         IF g_debug <= gme_debug.g_log_unexpected THEN
            gme_debug.put_line (   'When others exception in '
                                || g_pkg_name
                                || '.'
                                || l_api_name
                                || ' Error is '
                                || SQLERRM);
         END IF;

         x_return_status := fnd_api.g_ret_sts_unexp_error;
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
   END fetch_all_resources;

/*===========================================================================================
Procedure
  create_resource_trans
Description
  This particular procedure is used to create pending or completed resource transactions
Parameters
  p_resource_rec     The resource transaction rec
  x_tran_rec         The resource transaction rec with the updated poc trans id
  x_return_status    outcome of the API call
            S - Success
            E - Error
            U - Unexpected error
=============================================================================================*/
   PROCEDURE create_resource_trans (
      p_tran_rec        IN              gme_resource_txns_gtmp%ROWTYPE
     ,x_tran_rec        OUT NOCOPY      gme_resource_txns_gtmp%ROWTYPE
     ,x_return_status   OUT NOCOPY      VARCHAR2)
   IS
      l_api_name   CONSTANT VARCHAR2 (30)          := 'CREATE_RESOURCE_TRANS';
      l_return_status       VARCHAR2 (1)         := fnd_api.g_ret_sts_success;
      l_gme_tran_rec        gme_resource_txns_gtmp%ROWTYPE;
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      l_gme_tran_rec := p_tran_rec;

      /* Now Call the INSERT rec DML Layer */
      IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
         gme_debug.put_line ('Calling Insert Resource TXNS rec');
         gme_debug.put_line ('Resources +> ' || p_tran_rec.resources);
      END IF;

      l_gme_tran_rec.action_code := 'ADD';

      IF NOT gme_resource_txns_gtmp_dbl.insert_row
                                           (p_resource_txns      => l_gme_tran_rec
                                           ,x_resource_txns      => x_tran_rec) THEN
         RAISE fnd_api.g_exc_error;
      END IF;

      x_return_status := l_return_status;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;
   EXCEPTION
      WHEN fnd_api.g_exc_error THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN fnd_api.g_exc_unexpected_error THEN
         x_return_status := fnd_api.g_ret_sts_unexp_error;
      WHEN OTHERS THEN
         IF g_debug <= gme_debug.g_log_unexpected THEN
            gme_debug.put_line (   'When others exception in '
                                || g_pkg_name
                                || '.'
                                || l_api_name
                                || ' Error is '
                                || SQLERRM);
         END IF;

         x_return_status := fnd_api.g_ret_sts_unexp_error;
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
   END create_resource_trans;

/*===========================================================================================
Procedure
  delete_resource_trans
Description
  This particular procedure is used to delete pending or completed resource transactions
Parameters
  p_resource_rec     The resource transaction rec
  x_return_status    outcome of the API call
            S - Success
            E - Error
            U - Unexpected error
=============================================================================================*/
   PROCEDURE delete_resource_trans (
      p_tran_rec        IN              gme_resource_txns_gtmp%ROWTYPE
     ,x_return_status   OUT NOCOPY      VARCHAR2)
   IS
      l_gme_tran_rec        gme_resource_txns_gtmp%ROWTYPE;
      l_api_name   CONSTANT VARCHAR2 (30)          := 'DELETE_RESOURCE_TRANS';
      l_return_status       VARCHAR2 (1)         := fnd_api.g_ret_sts_success;
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      l_gme_tran_rec := p_tran_rec;

      IF p_tran_rec.action_code = 'ADD' THEN
         DELETE FROM gme_resource_txns_gtmp
               WHERE poc_trans_id = p_tran_rec.poc_trans_id;
      ELSE
         l_gme_tran_rec.action_code := 'DEL';

         IF NOT gme_resource_txns_gtmp_dbl.update_row
                                           (p_resource_txns      => l_gme_tran_rec) THEN
            RAISE fnd_api.g_exc_error;
         END IF;
      END IF;

      x_return_status := l_return_status;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;
   EXCEPTION
      WHEN fnd_api.g_exc_error THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN fnd_api.g_exc_unexpected_error THEN
         x_return_status := fnd_api.g_ret_sts_unexp_error;
      WHEN OTHERS THEN
         IF g_debug <= gme_debug.g_log_unexpected THEN
            gme_debug.put_line (   'When others exception in '
                                || g_pkg_name
                                || '.'
                                || l_api_name
                                || ' Error is '
                                || SQLERRM);
         END IF;

         x_return_status := fnd_api.g_ret_sts_unexp_error;
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
   END delete_resource_trans;

/*===========================================================================================
Procedure
  update_resource_trans
Description
  This particular procedure is used to update pending or completed resource transactions
Parameters
  p_resource_rec     The resource transaction rec
  x_return_status    outcome of the API call
            S - Success
            E - Error
            U - Unexpected error
=============================================================================================*/
   PROCEDURE update_resource_trans (
      p_tran_rec        IN              gme_resource_txns_gtmp%ROWTYPE
     ,x_return_status   OUT NOCOPY      VARCHAR2)
   IS
      l_api_name   CONSTANT VARCHAR2 (30)          := 'UPDATE_RESOURCE_TRANS';
      l_return_status       VARCHAR2 (1)         := fnd_api.g_ret_sts_success;
      l_gme_tran_rec        gme_resource_txns_gtmp%ROWTYPE;
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      l_gme_tran_rec := p_tran_rec;

      IF p_tran_rec.action_code = 'NONE' THEN
         l_gme_tran_rec.action_code := 'UPD';
      END IF;

      /* Now Call the UPDATE rec DML Layer */
      IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
         gme_debug.put_line (   'Calling UPDATE rec trans id:'
                             || TO_CHAR (p_tran_rec.poc_trans_id) );
      END IF;

      IF NOT gme_resource_txns_gtmp_dbl.update_row
                                            (p_resource_txns      => l_gme_tran_rec) THEN
         RAISE fnd_api.g_exc_error;
      END IF;

      x_return_status := l_return_status;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;
   EXCEPTION
      WHEN fnd_api.g_exc_error THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN fnd_api.g_exc_unexpected_error THEN
         x_return_status := fnd_api.g_ret_sts_unexp_error;
      WHEN OTHERS THEN
         IF g_debug <= gme_debug.g_log_unexpected THEN
            gme_debug.put_line (   'When others exception in '
                                || g_pkg_name
                                || '.'
                                || l_api_name
                                || ' Error is '
                                || SQLERRM);
         END IF;

         x_return_status := fnd_api.g_ret_sts_unexp_error;
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
   END update_resource_trans;

/*===========================================================================================
Procedure
  consolidate_batch_resources
Description
  This particular procedure is used to consolidate all the resource transactions from the
  temporary tblle and post them into the main tblle.
Parameters
  p_batch_id         The batch_id for which the resources have to be consolidated
  x_return_status    outcome of the API call
            S - Success
            E - Error
            U - Unexpected error

History
  10MAR04 P.Raghu Bug#3479771
          Modified consolidate_batch_resources procedure
          such that reverse txns are created for completed txns.
          for updation and deletion with posated_ind=0.
  14-MAR-2004 Shrikant Nene 3570630
        Modified CONSOLIDATE_BATCH_RESOURCES procedure keep the
        attributes of the resource transactions.  Currently if
        these attributes are updated by users manually and then
        if they change the transaction on the screen, attributes
        are lost.
  10-AUG-2004 Rishi Varma B3818266/3759970
        Added code to populate the reverse_id.
  02-SEP-2004 Rishi Varma B3856541
         Made changes for the rsrc txns in closed period ME.
  17-JAN-2006 Susruth D. Bug#4917189
         Added Update statements instead of dbl for updating the reverse_id
         in CONSOLIDATE_BATCH_RESOURCES.
         
  19-MAR-2010  G. Muratore     Bug 8751983     
         Use IB date entered by user for resource transaction reversals when necessary.
                  
  21-MAR-2013  G. Muratore     Bug 16327528     
         Use a different function to check for closed period
                  
  13-MAY-2013  G. Muratore     Bug 16325776     
         Avoid unnecessary reversals even for completed transactions.
         
  18-MAY-2015  Shaliu Chen    Bug 201102335
        Add validation whether the resource txn can be inserted/updated/deleted
        if batch is on hold with PAUSE type.
          
  16-JUN-2015  Shaliu Chen    BUG 21208206
       set resource txn date,resource start date and resource end date are all equal 
       to sysdate if the resource txn is created automatically by complete step/batch 
       action or incremental backflush action and the resource txn period overlap with 
       the batch hold period.
=============================================================================================*/
   PROCEDURE consolidate_batch_resources (
      p_batch_id        IN              NUMBER
     ,x_return_status   OUT NOCOPY      VARCHAR2)
   IS
      l_api_name   CONSTANT VARCHAR2 (30)    := 'CONSOLIDATE_BATCH_RESOURCES';
      l_return_status       VARCHAR2 (1);
      l_msg_data            VARCHAR2 (2000);
      l_msg_count           NUMBER;

      CURSOR cur_get_prev_rec (v_poc_id NUMBER)
      IS
         SELECT *
           FROM gme_resource_txns
          WHERE poc_trans_id = v_poc_id;

      l_debug               VARCHAR2 (2000);
      l_prev_rec            cur_get_prev_rec%ROWTYPE;
      l_resource_rec        gme_resource_txns_gtmp%ROWTYPE;
      l_tran_rec            gme_resource_txns%ROWTYPE;
      l_resource_tbl        gme_common_pvt.resource_transactions_tab;
      l_trans_date          DATE;
      l_hold_ib_date        DATE;
      l_org_id              NUMBER;
      l_period_id           INTEGER;

      l_in_tran_rec         gme_resource_txns%ROWTYPE;

      CURSOR cur_get_org_id (v_batch_id NUMBER)
      IS
         SELECT organization_id
           FROM gme_batch_header
          WHERE batch_id = v_batch_id;

      -- Bug 16325776
      l_reverse             NUMBER;
      l_xla_cnt             NUMBER;

      CURSOR cur_get_xla_rec (v_trans_id NUMBER)
      IS
        SELECT count(*)
          FROM gmf_xla_extract_headers 
         WHERE event_class_code = 'BATCH_RESOURCE' 
           AND transaction_id = v_trans_id
           AND rownum = 1;


      insert_failure        EXCEPTION;
      update_failure        EXCEPTION;

      -- Bug 5903208
      gmf_cost_failure         EXCEPTION;
      l_message_count		   NUMBER;
      l_message_list		   VARCHAR2(2000);

   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
         gme_debug.display_resource_gtmp (NULL, NULL, p_batch_id);
      END IF;

      l_debug := 'Doc Id = > ' || p_batch_id;

      l_resource_rec.doc_id := p_batch_id;
      fetch_all_resources (p_resource_rec       => l_resource_rec
                          ,x_resource_tbl       => l_resource_tbl
                          ,x_return_status      => l_return_status
                          ,p_active_trans       => 1);

      l_debug := 'Number Of recs => ' || TO_CHAR (l_resource_tbl.COUNT);

      OPEN cur_get_org_id (p_batch_id);

      FETCH cur_get_org_id
       INTO l_org_id;

      CLOSE cur_get_org_id;

      IF (l_resource_tbl.COUNT < 1) THEN
         l_debug := 'No recs found ';
      END IF;

      FOR i IN 1 .. l_resource_tbl.COUNT LOOP
         l_resource_rec := l_resource_tbl (i);

         IF (l_resource_rec.action_code <> 'NONE') THEN
            build_resource_tran (l_resource_rec, l_tran_rec);
         END IF;

         IF (l_resource_rec.action_code = 'ADD') THEN
         
            -- This value is one of the following:
            -- 1. The trans_date entered by the user on the form or defaulted by the logic or
            -- 2. It could also be created by a negative IB scenario. In this case it will have original 
            --    trans_date of last resource transaction that was reversed.
            -- 3. For positive IB, it will be the user enter IB date or case 1 above.
            l_trans_date := l_tran_rec.trans_date;

            -- 8751983 - comment out flexible call as it doesn't seem to work properly.
/*            
            IF NOT gme_common_pvt.close_period_check_flexible
                                      (p_org_id          => l_org_id
                                      ,p_trans_date      => l_trans_date
                                      ,x_trans_date      => l_tran_rec.trans_date
                                      ,x_period_id       => l_period_id) THEN                                      
               RAISE fnd_api.g_exc_error;
            END IF;
*/

            -- Bug 8751983/8922059 - Let's default to timestamp if old transaction is in a closed period
            IF NOT gme_common_pvt.check_close_period(p_org_id     => l_org_id
                                                    ,p_trans_date => l_trans_date) THEN
               -- If it falls in here it means that the transaction date was in a closed period.
               -- So we must stamp the date to either sysdate or user entered IB date.               
               
               -- Bug 8751983 - Let's do some special date processing if this transaction is due to negative IB.
               -- If g_ib_timestamp_set = 2 it means that this new trans is created during negative IB.
               IF (gme_common_pvt.g_ib_timestamp_set = 2 AND gme_common_pvt.g_ib_timestamp_date <> l_trans_date) THEN
                  l_tran_rec.trans_date := gme_common_pvt.g_ib_timestamp_date;
               ELSE
                  -- Let's default to timestamp and overwrite if the user entered a different date.
                  l_tran_rec.trans_date := gme_common_pvt.g_timestamp;                     
               END IF;                  
            END IF;
            
            /* Shaliu Chen     18-MAY-2015  Bug 201102335
               1.resource txn can not be inserted/updated/deleted if batch is on hold with STOP type.
               2.only backdated resource txn can be inserted/updated/deleted if batch is on hold with PAUSE type.
            */
            IF (gme_common_pvt.validate_resource_txn_onhold(p_batch_id   => p_batch_id,
                                                           p_trans_date => l_tran_rec.trans_date,
                                                           p_start_date => l_tran_rec.start_date,
                                                           p_end_date   => l_tran_rec.end_date) <> 'R' 
                AND NVL(l_tran_rec.completed_ind,0) <> 0)  THEN
               /*BUG 21208206
                 set resource txn date,resource start date and resource end date are 
                 all equal to sysdate if the resource txn is created automatically 
               */                
              IF NVL(gme_common_pvt.g_atuo_resource_txn_flag,'N') = 'Y' THEN
                 l_tran_rec.trans_date := gme_common_pvt.g_timestamp; 
                 l_tran_rec.start_date := gme_common_pvt.g_timestamp; 
                 l_tran_rec.end_date   := gme_common_pvt.g_timestamp;     
              ELSE                                                           
                gme_common_pvt.log_message('GME_ONHOLD_TRANS_DATE');
                RAISE fnd_api.g_exc_error;
              END IF;                                                           
            END IF;
           

            --Rishi Varma B3856541 02-09-2004 end
            IF NOT gme_resource_txns_dbl.insert_row (l_tran_rec, l_tran_rec) THEN
               RAISE insert_failure;
            END IF;

	    IF l_tran_rec.completed_ind = 1
	    THEN
	      --
              -- Bug 5903208 - Make call to GMF for actual costing
	      --
	      GMF_LAYERS.Create_Resource_Layers
              ( p_api_version   =>    1.0,
                p_init_msg_list =>    FND_API.G_FALSE,
                p_rsrc_rec      =>    l_tran_rec,
                p_doc_qty       =>    l_tran_rec.resource_usage,
                p_doc_um        =>    l_tran_rec.trans_qty_um,
                x_return_status =>    l_return_status,
                x_msg_count     =>    l_msg_count,
                x_msg_data      =>    l_msg_data);

              IF l_return_status <> FND_API.G_RET_STS_SUCCESS
              THEN
                 RAISE gmf_cost_failure;
              END IF;
	      -- End Bug 5903208
	    END IF;

         ELSIF (l_resource_rec.action_code = 'UPD') THEN
            OPEN cur_get_prev_rec (l_resource_rec.poc_trans_id);

            FETCH cur_get_prev_rec
             INTO l_prev_rec;

            CLOSE cur_get_prev_rec;

            l_in_tran_rec := l_prev_rec;

            l_reverse := 1;  -- Default behavior is to reverse.
            -- Bug 16325776 Let's try to avoid reversal if possible.
            IF l_prev_rec.completed_ind = 1 THEN            
               IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
                  gme_debug.put_line('l_prev_rec.resource_usage is '|| l_prev_rec.resource_usage);
                  gme_debug.put_line('l_resource_rec.resource_usage is '|| l_resource_rec.resource_usage);

                  gme_debug.put_line('l_prev_rec.trans_date is  '|| TO_CHAR(l_prev_rec.trans_date, 'DD-MON-YYYY HH24:MI:SS'));
                  gme_debug.put_line('l_resource_rec.trans_date is  '|| TO_CHAR(l_resource_rec.trans_date, 'DD-MON-YYYY HH24:MI:SS'));

                  gme_debug.put_line('l_prev_rec.start_date is  '|| TO_CHAR(l_prev_rec.start_date, 'DD-MON-YYYY HH24:MI:SS'));
                  gme_debug.put_line('l_resource_rec.start_date is  '|| TO_CHAR(l_resource_rec.start_date, 'DD-MON-YYYY HH24:MI:SS'));

                  gme_debug.put_line('l_prev_rec.end_date is    '|| TO_CHAR(l_prev_rec.end_date, 'DD-MON-YYYY HH24:MI:SS'));
                  gme_debug.put_line('l_resource_rec.end_date is    '|| TO_CHAR(l_resource_rec.end_date, 'DD-MON-YYYY HH24:MI:SS'));

                  gme_debug.put_line('l_prev_rec.instance_id is '|| l_prev_rec.instance_id);
                  gme_debug.put_line('l_resource_rec.instance_id is '|| l_resource_rec.instance_id);
                  
                  gme_debug.put_line('l_prev_rec.reason_id is   '|| l_prev_rec.reason_id);               
                  gme_debug.put_line('l_resource_rec.reason_id is   '|| l_resource_rec.reason_id);
               END IF;

               -- Let's check to see if only the reason id changed.
               IF (l_prev_rec.resource_usage = l_resource_rec.resource_usage AND
                   l_prev_rec.trans_date     = l_resource_rec.trans_date AND
                   l_prev_rec.start_date     = l_resource_rec.start_date AND
                   l_prev_rec.end_date       = l_resource_rec.end_date AND
                   NVL(l_prev_rec.instance_id, -999)    = NVL(l_resource_rec.instance_id, -999) AND
                   NVL(l_prev_rec.reason_id, -999)      <> NVL(l_resource_rec.reason_id, -999)) THEN

                  -- Let's make sure preprocessor hasn't been run for this transaction.               
                  OPEN cur_get_xla_rec (l_resource_rec.poc_trans_id);
                  FETCH cur_get_xla_rec INTO l_xla_cnt;
                  CLOSE cur_get_xla_rec;
               
                  IF l_xla_cnt = 0 THEN                                     
                     l_reverse := 0;
                  END IF;
               END IF;
            END IF;

            IF l_prev_rec.completed_ind = 1 AND l_reverse = 1 THEN
               /* Reverse the completed transaction */
               l_prev_rec.resource_usage := (-1) * l_prev_rec.resource_usage;
               l_prev_rec.posted_ind := 0;

               /*Populating the reverse_id of the reversed transaction with the poc_trans_id
                 of the original transaction*/
               l_prev_rec.reverse_id := l_in_tran_rec.poc_trans_id;

               IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
                  gme_debug.put_line
                     (   g_pkg_name
                      || '.'
                      || l_api_name
                      || ' UPD action code: poc_trans_id of the original transaction is '
                      || l_in_tran_rec.poc_trans_id);
               END IF;

               l_trans_date := l_prev_rec.trans_date;

               -- Bug 16327528 - Comment out this check as it does not seem to work.
               -- IF NOT gme_common_pvt.close_period_check_flexible
                                       -- (p_org_id          => l_org_id
                                       -- ,p_trans_date      => l_trans_date
                                       -- ,x_trans_date      => l_prev_rec.trans_date
                                       -- ,x_period_id       => l_period_id) THEN
                  -- RAISE fnd_api.g_exc_error;
               -- END IF;

               -- Bug 16327528 - Check for closed period.

               IF NOT gme_common_pvt.check_close_period(p_org_id     => l_org_id
                                                       ,p_trans_date => l_trans_date) THEN
               -- If it falls in here it means that the transaction date was in a closed period.
               -- So we must stamp the date to either sysdate or user entered IB date.               
                  -- Let's default to timestamp and overwrite if the user entered a different date.
                  l_prev_rec.trans_date := gme_common_pvt.g_timestamp;                     
               END IF;             
  

               IF NOT gme_resource_txns_dbl.insert_row (l_prev_rec
                                                       ,l_prev_rec) THEN
                  RAISE insert_failure;
               ELSE
               --Bug#4917189 Susruth D. Added below updated to make sure only we update the reverse_id but not any
               -- other column for the original txn which is reversed.
                 -- l_in_tran_rec.reverse_id := l_prev_rec.poc_trans_id;

                    IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
                     gme_debug.put_line
                        (   g_pkg_name
                         || '.'
                         || l_api_name
                         || ' UPD action code: poc_trans_id of the reversal txn is '
                         || l_prev_rec.poc_trans_id);
                  END IF;
                 /*
                  IF NOT gme_resource_txns_dbl.update_row (l_in_tran_rec) THEN
                     RAISE update_failure;
                  END IF;*/
                  UPDATE gme_resource_txns
                  SET reverse_id = l_prev_rec.poc_trans_id
                  WHERE poc_trans_id = l_in_tran_rec.poc_trans_id;


	          --
                  -- Bug 5903208 - Make call to GMF for actual costing
	          --
	          GMF_LAYERS.Create_Resource_Layers
                  ( p_api_version   =>    1.0,
                    p_init_msg_list =>    FND_API.G_FALSE,
                    p_rsrc_rec      =>    l_prev_rec,
                    p_doc_qty       =>    l_prev_rec.resource_usage,
                    p_doc_um        =>    l_prev_rec.trans_qty_um,
                    x_return_status =>    l_return_status,
                    x_msg_count     =>    l_msg_count,
                    x_msg_data      =>    l_msg_data);

                  IF l_return_status <> FND_API.G_RET_STS_SUCCESS
                  THEN
                     RAISE gmf_cost_failure;
                  END IF;
	          -- End Bug 5903208

               END IF;

               /* Post the actual transaction */
               l_tran_rec.posted_ind := 0;
               l_trans_date := l_tran_rec.trans_date;

               IF NOT gme_common_pvt.close_period_check_flexible
                                       (p_org_id          => l_org_id
                                       ,p_trans_date      => l_trans_date
                                       ,x_trans_date      => l_tran_rec.trans_date
                                       ,x_period_id       => l_period_id) THEN
                  RAISE fnd_api.g_exc_error;
               END IF;
               
                /* Shaliu Chen     18-MAY-2015  Bug 201102335
                   1.resource txn can not be inserted/updated/deleted if batch is on hold with STOP type.
                   2.only backdated resource txn can be inserted/updated/deleted if batch is on hold with PAUSE type.
                */
               IF gme_common_pvt.validate_resource_txn_onhold(p_batch_id   => p_batch_id,
                                                              p_trans_date => l_tran_rec.trans_date,
                                                              p_start_date => l_tran_rec.start_date,
                                                              p_end_date   => l_tran_rec.end_date) <> 'R' THEN
                                                              
               /*BUG 21208206
                 set resource txn date,resource start date and resource end date are 
                 all equal to sysdate if the resource txn is created automatically 
               */                
              IF NVL(gme_common_pvt.g_atuo_resource_txn_flag,'N') = 'Y' THEN
                 l_tran_rec.trans_date := gme_common_pvt.g_timestamp; 
                 l_tran_rec.start_date := gme_common_pvt.g_timestamp; 
                 l_tran_rec.end_date   := gme_common_pvt.g_timestamp;     
              ELSE                                                                                                                                                 
                 gme_common_pvt.log_message('GME_ONHOLD_TRANS_DATE');
                 RAISE fnd_api.g_exc_error;  
              END IF;                                                         
               END IF;                       
                              

               IF NOT gme_resource_txns_dbl.insert_row (l_tran_rec
                                                       ,l_tran_rec) THEN
                  RAISE insert_failure;
               END IF;


	       --
               -- Bug 5903208 - Make call to GMF for actual costing
	       --
	       GMF_LAYERS.Create_Resource_Layers
               ( p_api_version   =>    1.0,
                 p_init_msg_list =>    FND_API.G_FALSE,
                 p_rsrc_rec      =>    l_tran_rec,
                 p_doc_qty       =>    l_tran_rec.resource_usage,
                 p_doc_um        =>    l_tran_rec.trans_qty_um,
                 x_return_status =>    l_return_status,
                 x_msg_count     =>    l_msg_count,
                 x_msg_data      =>    l_msg_data);

               IF l_return_status <> FND_API.G_RET_STS_SUCCESS
               THEN
                  RAISE gmf_cost_failure;
               END IF;
	       -- End Bug 5903208

            ELSIF NOT gme_resource_txns_dbl.update_row (l_tran_rec) THEN
                 RAISE update_failure;
            END IF;
         ELSIF (l_resource_rec.action_code = 'DEL') THEN
            IF l_resource_rec.completed_ind = 1 THEN
               IF l_tran_rec.resource_usage = 0 THEN
                  l_tran_rec.delete_mark := 1;

                  IF NOT gme_resource_txns_dbl.delete_row (l_tran_rec) THEN
                     RAISE update_failure;
                  END IF;
               ELSE
                  l_tran_rec.resource_usage :=
                                             (-1) * l_tran_rec.resource_usage;
                  l_tran_rec.posted_ind := 0;

                  /*Populating the reverse_id of the reversal transaction with the poc_trans_id
                    of the original transaction*/
                  OPEN cur_get_prev_rec (l_resource_rec.poc_trans_id);

                  FETCH cur_get_prev_rec
                   INTO l_in_tran_rec;

                  CLOSE cur_get_prev_rec;

                  IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
                     gme_debug.put_line
                        (   g_pkg_name
                         || '.'
                         || l_api_name
                         || ' DEL action code: poc_trans_id of the original transaction is '
                         || l_in_tran_rec.poc_trans_id);
                  END IF;

                  IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
                     gme_debug.put_line('IN DEL dates');
                     gme_debug.put_line('l_tran_rec.trans_date is '||l_tran_rec.trans_date);
                     gme_debug.put_line('ORIGINAL l_in_tran_rec.trans_date is '||l_in_tran_rec.trans_date);
                     gme_debug.put_line('gme_common_pvt.g_ib_timestamp_set is '||gme_common_pvt.g_ib_timestamp_set);
                  END IF;

                  -- Note: l_in_tran_rec.trans_date is the original trans record prior to any change from DB
                  --       l_tran_rec.trans_date is the current data changed by the user on form or IB                  

                  -- Bug 8751983 - If the global is set it implies that we are hitting this code due
                  -- to negative IB and also that the user passed in a date during IB which is stamped
                  -- on the 'DEL' action transactions.
                  -- Now we will use that date only if the original resource trans date is in a closed period.
                  IF gme_common_pvt.g_ib_timestamp_set > 0 THEN
                     l_hold_ib_date := l_tran_rec.trans_date;
                     l_tran_rec.trans_date := l_in_tran_rec.trans_date;
                  END IF;
                   
                  -- l_trans_date is now either the original date from the db or from the form.
                  l_trans_date := l_tran_rec.trans_date;
                  l_tran_rec.reverse_id := l_tran_rec.poc_trans_id;                  

                  -- 8751983 - comment out flexible call as it doesn't seem to work properly.
/*                  
                  -- Check to make sure date is not in a closed period otherwise stamp with sysdate.
                  IF NOT gme_common_pvt.close_period_check_flexible
                                       (p_org_id          => l_org_id
                                       ,p_trans_date      => l_trans_date
                                       ,x_trans_date      => l_tran_rec.trans_date
                                       ,x_period_id       => l_period_id) THEN
                                       
                     RAISE fnd_api.g_exc_error;
                  END IF;
*/                  
                  -- Bug 8751983 - Let's default to timestamp if old transaction is in a closed period
                  IF NOT gme_common_pvt.check_close_period(p_org_id     => l_org_id
                                                          ,p_trans_date => l_trans_date) THEN
                     -- Use IB date only if the old transaction was in a closed
                     -- period (previous check) and if the user actually entered a date during IB.
                     -- If the dates are the same it means user did not enter a date during IB.
                     IF (gme_common_pvt.g_ib_timestamp_set > 0 AND l_hold_ib_date <> l_trans_date) THEN
                        l_tran_rec.trans_date := l_hold_ib_date;
                     ELSE
                        -- Let's default to timestamp and overwrite if the user entered a different date.
                        l_tran_rec.trans_date := gme_common_pvt.g_timestamp;                     
                     END IF;                  
                  END IF;
                  
                    /* Shaliu Chen     18-MAY-2015  Bug 201102335
                       1.resource txn can not be inserted/updated/deleted if batch is on hold with STOP type.
                       2.only backdated resource txn can be inserted/updated/deleted if batch is on hold with PAUSE type.
                    */
                    
                  IF gme_common_pvt.validate_resource_txn_onhold(p_batch_id   => p_batch_id,
                                                                 p_trans_date => l_tran_rec.trans_date,
                                                                 p_start_date => l_tran_rec.start_date,
                                                                 p_end_date   => l_tran_rec.end_date) <> 'R' THEN
                    
                     /*BUG 21208206
                       set resource txn date,resource start date and resource end date are 
                       all equal to sysdate if the resource txn is created automatically 
                     */                
                    IF NVL(gme_common_pvt.g_atuo_resource_txn_flag,'N') = 'Y' THEN
                       l_tran_rec.trans_date := gme_common_pvt.g_timestamp; 
                       l_tran_rec.start_date := gme_common_pvt.g_timestamp; 
                       l_tran_rec.end_date   := gme_common_pvt.g_timestamp;      
                    ELSE                                                                                     
                      gme_common_pvt.log_message('GME_ONHOLD_TRANS_DATE');
                      RAISE fnd_api.g_exc_error; 
                    END IF;                                                          
                  END IF;                     

                  IF NOT gme_resource_txns_dbl.insert_row (l_tran_rec
                                                          ,l_tran_rec) THEN
                     RAISE insert_failure;
                  ELSE
                   --Bug#4917189 Susruth D. Added below updated to make sure only we update the reverse_id but not any
                   -- other column for the original txn which is reversed.
                     /*l_in_tran_rec.reverse_id := l_tran_rec.poc_trans_id;*/

                     IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
                        gme_debug.put_line
                           (   g_pkg_name
                            || '.'
                            || l_api_name
                            || ' DEL action code: poc_trans_id of the reversal txn is '
                            || l_tran_rec.poc_trans_id);
                     END IF;

                     /*IF NOT gme_resource_txns_dbl.update_row (l_in_tran_rec) THEN
                        RAISE update_failure;
                     END IF;*/
                     UPDATE gme_resource_txns
                     SET reverse_id = l_tran_rec.poc_trans_id
                     WHERE poc_trans_id = l_in_tran_rec.poc_trans_id;

	             --
                     -- Bug 5903208 - Make call to GMF for actual costing
	             --
	             GMF_LAYERS.Create_Resource_Layers
                     ( p_api_version   =>    1.0,
                       p_init_msg_list =>    FND_API.G_FALSE,
                       p_rsrc_rec      =>    l_tran_rec,
                       p_doc_qty       =>    l_tran_rec.resource_usage,
                       p_doc_um        =>    l_tran_rec.trans_qty_um,
                       x_return_status =>    l_return_status,
                       x_msg_count     =>    l_msg_count,
                       x_msg_data      =>    l_msg_data);

                     IF l_return_status <> FND_API.G_RET_STS_SUCCESS
                     THEN
                        RAISE gmf_cost_failure;
                     END IF;
	             -- End Bug 5903208

                  END IF;
               END IF;
            ELSE
               IF NOT gme_resource_txns_dbl.delete_row (l_tran_rec) THEN
                  RAISE update_failure;
               END IF;
            END IF;
         END IF;
      END LOOP;
      --Bug 21208206 Reset the global variables.
      IF NVL(gme_common_pvt.g_atuo_resource_txn_flag,'N') = 'Y' THEN
        gme_common_pvt.g_atuo_resource_txn_flag := 'N';  
      END IF;

      -- Bug 8751983 - Reset the global variables.
      gme_common_pvt.g_ib_timestamp_set  := 0;
      gme_common_pvt.g_ib_timestamp_date := NULL;

      IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
         gme_debug.display_resource_gtmp (NULL, NULL, p_batch_id);
      END IF;

      x_return_status := l_return_status;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;
   EXCEPTION
      WHEN fnd_api.g_exc_error THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN insert_failure THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN update_failure THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN OTHERS THEN
         IF g_debug <= gme_debug.g_log_unexpected THEN
            gme_debug.put_line (   'When others exception in '
                                || g_pkg_name
                                || '.'
                                || l_api_name
                                || ' Error is '
                                || SQLERRM);
         END IF;

         x_return_status := fnd_api.g_ret_sts_unexp_error;
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
   END consolidate_batch_resources;

/*===========================================================================================
Procedure
  build_resource_tran
Description
  This particular procedure is used to build a transaction rec from the temporary tblle rec.
Parameters
  p_tmp_rec       The resource transaction rec of the temporary tblle
  p_resource_rec     The resource transaction rec.
=============================================================================================*/
   PROCEDURE build_resource_tran (
      p_tmp_rec        IN              gme_resource_txns_gtmp%ROWTYPE
     ,p_resource_rec   OUT NOCOPY      gme_resource_txns%ROWTYPE)
   IS
      l_api_name   CONSTANT VARCHAR2 (30) := 'build_resource_tran';
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      p_resource_rec.organization_id 	:= p_tmp_rec.organization_id;
      p_resource_rec.poc_trans_id 	:= p_tmp_rec.poc_trans_id;
      p_resource_rec.orgn_code 		:= p_tmp_rec.orgn_code;
      p_resource_rec.doc_type 		:= p_tmp_rec.doc_type;
      p_resource_rec.doc_id 		:= p_tmp_rec.doc_id;
      p_resource_rec.line_type 		:= p_tmp_rec.line_type;
      p_resource_rec.line_id 		:= p_tmp_rec.line_id;
      p_resource_rec.resources 		:= p_tmp_rec.resources;
      p_resource_rec.resource_usage 	:= p_tmp_rec.resource_usage;
      p_resource_rec.trans_qty_um 	:= p_tmp_rec.trans_um;
      p_resource_rec.trans_date 	:= p_tmp_rec.trans_date;
      p_resource_rec.completed_ind 	:= p_tmp_rec.completed_ind;
      p_resource_rec.event_id 		:= p_tmp_rec.event_id;
      p_resource_rec.posted_ind 	:= p_tmp_rec.posted_ind;
      p_resource_rec.overrided_protected_ind
      					:= p_tmp_rec.overrided_protected_ind;
      p_resource_rec.reason_code 	:= p_tmp_rec.reason_code;
      p_resource_rec.reason_id 		:= p_tmp_rec.reason_id;
      p_resource_rec.start_date 	:= p_tmp_rec.start_date;
      p_resource_rec.end_date 		:= p_tmp_rec.end_date;
      p_resource_rec.creation_date 	:= gme_common_pvt.g_timestamp;
      p_resource_rec.last_update_date 	:= gme_common_pvt.g_timestamp;
      p_resource_rec.created_by 	:= gme_common_pvt.g_user_ident;
      p_resource_rec.last_updated_by 	:= gme_common_pvt.g_user_ident;
      p_resource_rec.last_update_login 	:= gme_common_pvt.g_login_id;
      p_resource_rec.delete_mark 	:= p_tmp_rec.delete_mark;
      p_resource_rec.text_code 		:= p_tmp_rec.text_code;
      p_resource_rec.sequence_dependent_ind
      					:= p_tmp_rec.sequence_dependent_ind;
      p_resource_rec.instance_id 	:= p_tmp_rec.instance_id;
      p_resource_rec.instance_id 	:= p_tmp_rec.instance_id;
      --  Pawan Added attribute for flex field implemenation
      p_resource_rec.attribute_category := p_tmp_rec.attribute_category;
      p_resource_rec.attribute1 	:= p_tmp_rec.attribute1;
      p_resource_rec.attribute2 	:= p_tmp_rec.attribute2;
      p_resource_rec.attribute3 	:= p_tmp_rec.attribute3;
      p_resource_rec.attribute4 	:= p_tmp_rec.attribute4;
      p_resource_rec.attribute5 	:= p_tmp_rec.attribute5;
      p_resource_rec.attribute6 	:= p_tmp_rec.attribute6;
      p_resource_rec.attribute7 	:= p_tmp_rec.attribute7;
      p_resource_rec.attribute8 	:= p_tmp_rec.attribute8;
      p_resource_rec.attribute9 	:= p_tmp_rec.attribute9;
      p_resource_rec.attribute10 	:= p_tmp_rec.attribute10;
      p_resource_rec.attribute11 	:= p_tmp_rec.attribute11;
      p_resource_rec.attribute12 	:= p_tmp_rec.attribute12;
      p_resource_rec.attribute13 	:= p_tmp_rec.attribute13;
      p_resource_rec.attribute14 	:= p_tmp_rec.attribute14;
      p_resource_rec.attribute15 	:= p_tmp_rec.attribute15;
      p_resource_rec.attribute16 	:= p_tmp_rec.attribute16;
      p_resource_rec.attribute17 	:= p_tmp_rec.attribute17;
      p_resource_rec.attribute18 	:= p_tmp_rec.attribute18;
      p_resource_rec.attribute19 	:= p_tmp_rec.attribute19;
      p_resource_rec.attribute20 	:= p_tmp_rec.attribute20;
      p_resource_rec.attribute21 	:= p_tmp_rec.attribute21;
      p_resource_rec.attribute22 	:= p_tmp_rec.attribute22;
      p_resource_rec.attribute23 	:= p_tmp_rec.attribute23;
      p_resource_rec.attribute24 	:= p_tmp_rec.attribute24;
      p_resource_rec.attribute25 	:= p_tmp_rec.attribute25;
      p_resource_rec.attribute26 	:= p_tmp_rec.attribute26;
      p_resource_rec.attribute27 	:= p_tmp_rec.attribute27;
      p_resource_rec.attribute28 	:= p_tmp_rec.attribute28;
      p_resource_rec.attribute29 	:= p_tmp_rec.attribute29;
      p_resource_rec.attribute30 	:= p_tmp_rec.attribute30;







      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;
   --Bug2804440
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

         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
   --End Bug2804440
   END build_resource_tran;

/*===========================================================================================
Procedure
  fetch_active_resources
Description
  This particular procedure is used to fetch the active resources for a particular line or doc
  or trans id from the temporary tblle.
Parameters
  p_resource_rec     The resource transaction rec
  x_resource_tbl     All the resource transactions pertaining to the criteria.
  x_return_status    outcome of the API call
            S - Success
            E - Error
            U - Unexpected error
History
  Namit Singhi. Bug#5609683. Added parameter p_calling_mode.
  
  G. Muratore   19-MAR-2010  Bug 8751983     
     Added logic to process value ACTUAL_USAGE for parameter p_calling_mode. This will allow 
     us to fetch the resource transactions in a different order for negative IB.
=============================================================================================*/
   PROCEDURE fetch_active_resources (
      p_resource_rec    IN              gme_resource_txns_gtmp%ROWTYPE
     ,p_calling_mode    IN              VARCHAR2 DEFAULT NULL --bug#5609683
     ,x_resource_tbl    OUT NOCOPY      gme_common_pvt.resource_transactions_tab
     ,x_return_status   OUT NOCOPY      VARCHAR2)
   IS
      TYPE query_ref IS REF CURSOR;

      get_rsrc              query_ref;
      l_where               VARCHAR2 (2000);
      l_api_name   CONSTANT VARCHAR2 (30)   := 'FETCH_ACTIVE_RESOURCES';
      l_return_status       VARCHAR2 (1)    := fnd_api.g_ret_sts_success;
      l_line_index          BINARY_INTEGER  := 1;
      l_cursor              BINARY_INTEGER;
      bad_keys              EXCEPTION;
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      /*  Initialize API return status to sucess  */
      x_return_status := fnd_api.g_ret_sts_success;

      -- Determine if any of the key values are present
      IF (p_resource_rec.poc_trans_id IS NOT NULL) THEN
         l_where := 'POC_TRANS_ID =:poc_trans_id ';
         l_cursor := 1;
      ELSIF (p_resource_rec.line_id IS NOT NULL) THEN
         l_where := ' LINE_ID =:line_id';
         l_cursor := 2;
      ELSIF (p_resource_rec.doc_id IS NOT NULL) THEN
         l_where := ' DOC_ID =:doc_id';
         l_cursor := 3;
      ELSE
         RAISE bad_keys;
      END IF;

      l_where :=
            l_where
         || ' AND action_code NOT IN ('
         || ''''
         || 'REVS'
         || ''''
         || ','
         || ''''
         || 'REVL'
         || ''''
         || ','
         || ''''
         || 'DEL'
         || ''''
         || ')'
--nsinghi bug#5609683
-- If called from gme_update_step_qty_pvt.reduce_pending_usage, then we need to order the resources by usage ASC
-- so that the remaining usage gets factored in for the remaining pending resource txn.
--         || ' AND delete_mark <> 1 ORDER BY completed_ind, poc_trans_id ';
         || ' AND delete_mark <> 1 ';

      IF p_calling_mode = 'REDUCE_USAGE' THEN
        l_where := l_where || ' ORDER BY completed_ind, resource_usage, poc_trans_id ';
      ELSE
         -- Bug 8751983 - Sort by latest transaction for -IB
         IF p_calling_mode = 'ACTUAL_USAGE' THEN
           l_where := l_where || ' ORDER BY completed_ind, poc_trans_id DESC, resource_usage';
         ELSE
           l_where := l_where || ' ORDER BY completed_ind, poc_trans_id ';
         END IF;
      END IF;

      IF l_cursor = 1 THEN
         OPEN get_rsrc
          FOR    ' SELECT * FROM GME_RESOURCE_TXNS_GTMP
            WHERE '
              || l_where USING p_resource_rec.poc_trans_id;
      ELSIF l_cursor = 2 THEN
         OPEN get_rsrc
          FOR    ' SELECT * FROM GME_RESOURCE_TXNS_GTMP
            WHERE '
              || l_where USING p_resource_rec.line_id;
      ELSIF l_cursor = 3 THEN
         OPEN get_rsrc
          FOR    ' SELECT * FROM GME_RESOURCE_TXNS_GTMP
            WHERE '
              || l_where USING p_resource_rec.doc_id;
      END IF;

      LOOP
         FETCH get_rsrc
          INTO x_resource_tbl (l_line_index);

         EXIT WHEN get_rsrc%NOTFOUND;
         l_line_index := l_line_index + 1;
      END LOOP;

      CLOSE get_rsrc;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;
   EXCEPTION
      WHEN fnd_api.g_exc_error THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN fnd_api.g_exc_unexpected_error THEN
         x_return_status := fnd_api.g_ret_sts_unexp_error;
      WHEN bad_keys THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN OTHERS THEN
         IF g_debug <= gme_debug.g_log_unexpected THEN
            gme_debug.put_line (   'When others exception in '
                                || g_pkg_name
                                || '.'
                                || l_api_name
                                || ' Error is '
                                || SQLERRM);
         END IF;

         x_return_status := fnd_api.g_ret_sts_unexp_error;
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
   END fetch_active_resources;

/*===========================================================================================
Procedure
  resource_dtl_process
Description
  This particular procedure is used to process the resource detail.
Parameters
  p_step_resources_rec     The batch step resource rec to identify the resource
  p_action_code         Action to peform the resource rec
  x_step_resources_rec     The  batch step resources rec.
  x_return_status    outcome of the API call
            S - Success
            E - Error
            U - Unexpected error
HISTORY
  Bharati Satpute   Bug2188136  21/03/2002 Added code for action_code 'INSERT'

=============================================================================================*/
   PROCEDURE resource_dtl_process (
      p_step_resources_rec   IN              gme_batch_step_resources%ROWTYPE
     ,p_action_code          IN              VARCHAR2
     ,p_check_prim_rsrc      IN              BOOLEAN := FALSE
     ,x_step_resources_rec   OUT NOCOPY      gme_batch_step_resources%ROWTYPE
     ,x_return_status        OUT NOCOPY      VARCHAR2)
   IS
      l_api_name   CONSTANT VARCHAR2 (30)           := 'resource_dtl_process';
      l_batch_header        gme_batch_header%ROWTYPE;
      l_batch_step          gme_batch_steps%ROWTYPE;
      l_step_activity       gme_batch_step_activities%ROWTYPE;
      l_step_resources      gme_batch_step_resources%ROWTYPE;
      l_resource_txns       gme_resource_txns_gtmp%ROWTYPE;
      l_resource_tbl        gme_common_pvt.resource_transactions_tab;
      l_return_status       VARCHAR2 (1);
      l_rsrc_usage          NUMBER;
      l_rsrc_count          NUMBER;
      l_tot_usage           NUMBER;
      l_completed           NUMBER (5);

      x_batch_step          gme_batch_steps%ROWTYPE;

      CURSOR cur_sum_usage (v_batchstep_resource_id NUMBER, v_completed NUMBER)
      IS
         SELECT NVL (SUM (resource_usage), 0)
           FROM gme_resource_txns_gtmp
          WHERE line_id = v_batchstep_resource_id
            AND completed_ind = v_completed
            AND (v_completed = 1 OR sequence_dependent_ind = 0);

     --Bug#5231180 Begin added the following cursors
     CURSOR cur_get_rsrc (v_rsrc cr_rsrc_mst.resources%TYPE)
     IS
      SELECT max_capacity, capacity_um
        FROM cr_rsrc_mst
        WHERE resources = v_rsrc
        AND capacity_constraint = 1;

     CURSOR cur_get_charge_rsrc(v_step_id gme_batch_steps.batchstep_id%TYPE, v_rsrc cr_rsrc_mst.resources%TYPE)
     IS
      SELECT 1
        FROM DUAL
       WHERE EXISTS (SELECT 1
                      FROM gme_batch_step_charges
		     WHERE batchstep_id = v_step_id
		       AND resources    = v_rsrc);
           
     /*ER 19161894  Shaliu Chen 18-JUL-2014*/      
     CURSOR cur_get_osp_parameter(v_organization_id NUMBER)
      IS
         SELECT propagate_change_to_po
           FROM gme_parameters
          WHERE organization_id = v_organization_id; 
                     
      --Bug#5231180 End
     l_rsrc_rec             cur_get_rsrc%ROWTYPE;
     l_exists               NUMBER;
     l_temp_qty             NUMBER;
     /*ER 19161894  Shaliu Chen 18-JUL-2014*/
     l_propagate_change_to_po  NUMBER;
     l_osp_resource_flag       NUMBER;
     /*END ER 19161894*/     
     error_in_conversion    EXCEPTION;
     error_in_clear_charges EXCEPTION;
     qty_sync_fail          EXCEPTION;  /*ER 19161894  Shaliu Chen 18-JUL-2014*/

     -- Bug 5903208
     gmf_cost_failure         EXCEPTION;
     l_message_count		   NUMBER;
     l_message_list		   VARCHAR2(2000);

   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      /* There is no public layer for this package so adding initialize right here *
      IF ( NVL(G_DEBUG,-1) = GME_DEBUG.G_LOG_STATEMENT ) THEN
        gme_debug.log_initialize('RESOURCE_TXN');
      END IF;

      /* Initialize return status to success */
      x_return_status := fnd_api.g_ret_sts_success;
      x_step_resources_rec := p_step_resources_rec;
      l_batch_step.batchstep_id := x_step_resources_rec.batchstep_id;

      IF NOT gme_batch_steps_dbl.fetch_row (p_batch_step      => l_batch_step
                                           ,x_batch_step      => l_batch_step) THEN
         RAISE fnd_api.g_exc_error;
      END IF;

      l_step_activity.batchstep_activity_id :=
                                    p_step_resources_rec.batchstep_activity_id;

      IF NOT gme_batch_step_activities_dbl.fetch_row
                                  (p_batch_step_activities      => l_step_activity
                                  ,x_batch_step_activities      => l_step_activity) THEN
         RAISE fnd_api.g_exc_error;
      END IF;

      l_batch_header.batch_id := x_step_resources_rec.batch_id;

      IF NOT gme_batch_header_dbl.fetch_row (p_batch_header      => l_batch_header
                                            ,x_batch_header      => l_batch_header) THEN
         RAISE fnd_api.g_exc_error;
      END IF;

      /* If automatic step qty calculation is set for the batch then */
      /* we have to compute the usage fields                         */
      IF     (p_action_code = 'INSERT')
         AND (l_batch_header.automatic_step_calculation = 1) THEN
         /* If the step  status is greater than pending then */
         /* we have to calculate the actual quantities       */
         IF l_batch_step.step_status > 2 THEN
            x_step_resources_rec.actual_rsrc_qty :=
                                                 l_batch_step.actual_step_qty;
            x_step_resources_rec.original_rsrc_qty :=
                                                 l_batch_step.actual_step_qty;
            x_step_resources_rec.original_rsrc_usage :=
                                       x_step_resources_rec.actual_rsrc_usage;

            /* If the resource scale type is calculate by charges then */
            IF x_step_resources_rec.scale_type = 2 THEN
               x_step_resources_rec.original_rsrc_usage :=
                    (  x_step_resources_rec.actual_rsrc_usage
                     / l_step_activity.actual_activity_factor)
                  / l_batch_step.actual_charges;
               x_step_resources_rec.original_rsrc_qty :=
                  (   (x_step_resources_rec.actual_rsrc_qty)
                   / l_step_activity.actual_activity_factor);
            /* If the resource scale type is linear/fixed then */
            ELSE
               x_step_resources_rec.original_rsrc_usage :=
                    (x_step_resources_rec.actual_rsrc_usage)
                  / l_step_activity.actual_activity_factor;
               x_step_resources_rec.original_rsrc_qty :=
                    (x_step_resources_rec.actual_rsrc_qty)
                  / l_step_activity.actual_activity_factor;
            END IF;            /*IF l_gme_batchstep_resources.scale_type = 1*/
         /* If the step status is pending then we have to update the plan quantities */
         ELSIF l_batch_step.step_status = 1 THEN
            x_step_resources_rec.plan_rsrc_qty := l_batch_step.plan_step_qty;
            x_step_resources_rec.original_rsrc_qty :=
                                                   l_batch_step.plan_step_qty;
            x_step_resources_rec.original_rsrc_usage :=
                                         x_step_resources_rec.plan_rsrc_usage;

            IF x_step_resources_rec.scale_type = 2 THEN
               x_step_resources_rec.original_rsrc_usage :=
                    (  x_step_resources_rec.plan_rsrc_usage
                     / l_step_activity.plan_activity_factor)
                  / l_batch_step.plan_charges;
               x_step_resources_rec.original_rsrc_qty :=
                  (  x_step_resources_rec.plan_rsrc_qty
                   / l_step_activity.plan_activity_factor);
            /* If the resource scale type is linear/fix then */
            ELSE
               x_step_resources_rec.original_rsrc_usage :=
                  (   (x_step_resources_rec.plan_rsrc_usage)
                   / l_step_activity.plan_activity_factor);
               x_step_resources_rec.original_rsrc_qty :=
                    (x_step_resources_rec.plan_rsrc_qty)
                  / l_step_activity.plan_activity_factor;
            END IF;            /*IF l_gme_batchstep_resources.scale_type = 1*/
         END IF;                           /*IF l_batch_step.step_status > 2*/
      END IF;                                /* IF p_action_code = 'INSERT' */

      IF (l_batch_step.step_status = 1) THEN
         x_step_resources_rec.actual_rsrc_usage := NULL;
         x_step_resources_rec.actual_rsrc_qty := NULL;
         x_step_resources_rec.actual_rsrc_count := NULL;
      END IF;                          /* IF (l_batch_step.step_status = 1) */

      --rishi 3446787 03/03/04 added the p_check_prim_rsrc parameter
      IF p_action_code IN ('INSERT', 'UPDATE') THEN
         validate_resource (p_batch_step_rec          => l_batch_step
                           ,p_step_activity_rec       => l_step_activity
                           ,p_step_resources_rec      => x_step_resources_rec
                           ,p_check_prim_rsrc         => p_check_prim_rsrc
                           ,x_return_status           => l_return_status);

         IF l_return_status <> x_return_status THEN
            RAISE fnd_api.g_exc_error;
         END IF;
      END IF;

      /* Since this procedure is called from the forms
         and there is no public layer for this we can call
         set_timestamp here */
      gme_common_pvt.set_timestamp;

      IF p_action_code = 'INSERT' THEN
         -- Shikha Nagar - added check to prevent second instance of primary rsrc from getting inserted
         -- for an activity
         --rishi 3446787 03/03/04
         -- commented the call to check_primary_resorce as it is already being called
         -- by validate_resource above.
         /*
         GME_RESOURCE_ENGINE_PVT.check_primary_resource (p_step_resources_rec => x_step_resources_rec
                                                        ,x_return_status  => l_return_status);
         IF l_return_status <> x_return_status THEN
           RAISE FND_API.g_exc_error;
         END IF;
         */
         
         -- Bug 12576806 - Initialize org id.
         x_step_resources_rec.organization_id := l_batch_header.organization_id;
         
         IF NOT (gme_batch_step_resources_dbl.insert_row
                              (p_batch_step_resources      => x_step_resources_rec
                              ,x_batch_step_resources      => x_step_resources_rec) ) THEN
            RAISE fnd_api.g_exc_error;
         END IF;
      END IF;

      IF p_action_code IN ('INSERT', 'UPDATE') THEN
         IF l_batch_header.update_inventory_ind = 'Y' THEN
            IF l_batch_step.step_status = 1 THEN
               gme_update_step_qty_pvt.adjust_pending_usage
                         (p_batch_step_resources_rec      => x_step_resources_rec
                         ,x_return_status                 => l_return_status);

               IF l_return_status <> x_return_status THEN
                  RAISE fnd_api.g_exc_error;
               END IF;
            ELSIF     l_batch_step.step_status > 1
                  AND NVL (x_step_resources_rec.actual_rsrc_usage, -1) >= 0 THEN
               gme_update_step_qty_pvt.adjust_actual_usage
                         (p_batch_step_resources_rec      => x_step_resources_rec
                         ,x_return_status                 => l_return_status);

               IF l_return_status <> x_return_status THEN
                  RAISE fnd_api.g_exc_error;
               END IF;
            END IF;                      /* IF l_batch_step.step_status = 1 */
         END IF;            /* IF l_batch_header.update_inventory_ind = 'Y' */

	 --
         -- Bug 5903208 - Make call to GMF for actual costing data recording
         --
         IF p_action_code = 'INSERT' THEN
            GMF_VIB.Update_Batch_Requirements
            ( p_api_version   =>    1.0,
              p_init_msg_list =>    FND_API.G_FALSE,
              p_batch_id      =>    l_batch_header.batch_id,
              x_return_status =>    l_return_status,
              x_msg_count     =>    l_message_count,
              x_msg_data      =>    l_message_list);

            IF l_return_status <> FND_API.G_RET_STS_SUCCESS
            THEN
               RAISE gmf_cost_failure;
            END IF;
         END IF;
	 -- End Bug 5903208

      ELSIF p_action_code = 'DELETE' THEN
         /* Remove this call as per Resource TD page 57                               */
         /* Call to be made from GME_API_PUB.save_batch                               */
         /*
         GME_RESOURCE_ENGINE_PVT.check_primary_resource (p_step_resources_rec => x_step_resources_rec
                                                        ,x_return_status  => l_return_status);
         IF l_return_status <> x_return_status THEN
           RAISE FND_API.g_exc_error;
         END IF;
         */
         gme_delete_batch_step_pvt.delete_resource
                         (p_batch_step_resources_rec      => x_step_resources_rec
                         ,x_return_status                 => l_return_status);

         IF l_return_status <> x_return_status THEN
            RAISE fnd_api.g_exc_error;
         END IF;

         -- Bug 5043868 - Make call to GMF for actual costing data
         -- recording when deleting a resource from the form or api.
         GMF_VIB.Update_Batch_Requirements
         ( p_api_version   =>    1.0,
           p_init_msg_list =>    FND_API.G_FALSE,
           p_batch_id      =>    l_batch_header.batch_id,
           x_return_status =>    l_return_status,
           x_msg_count     =>    l_message_count,
           x_msg_data      =>    l_message_list);

         IF l_return_status <> FND_API.G_RET_STS_SUCCESS
         THEN
            RAISE gmf_cost_failure;
         END IF;
	 -- End Bug 5903208

      END IF;                   /* IF p_action_code IN ('INSERT', 'UPDATE') */

      /* Update has to be done after updating the transactions as the adjust actual */
      /* usage logic depends on the previous actual usage to deduce the usage to be */
      /* deducted from the pending resource transactions                            */
      IF p_action_code = 'UPDATE' THEN
         IF NOT (gme_batch_step_resources_dbl.update_row
                               (p_batch_step_resources      => x_step_resources_rec) ) THEN
            RAISE fnd_api.g_exc_error;
         END IF;

         x_step_resources_rec.last_updated_by := gme_common_pvt.g_user_ident;
         x_step_resources_rec.last_update_date := gme_common_pvt.g_timestamp;
      END IF;

      /*Bug#5231180 resource insertion and deletion might result in the max step capacity
        if the resource is capacity constrained then we need to recalculate charges
	if the resource is not capacity constrained then there is no need to recalculate charges */
        /*Bug#6607524 Changed the hard coded 1381 to the batch header's organization id*/
       gme_common_pvt.g_setup_done :=
         gme_common_pvt.setup (p_org_id        => l_batch_header.ORGANIZATION_ID --1381
                              ,p_org_code      => NULL);

       IF NOT gme_common_pvt.g_setup_done THEN
         RAISE fnd_api.g_exc_error;
       END IF;

       OPEN cur_get_rsrc(x_step_resources_rec.resources);
       FETCH cur_get_rsrc INTO l_rsrc_rec;
       IF cur_get_rsrc%FOUND THEN
         l_exists := 1;
       END IF;
       CLOSE cur_get_rsrc;

     IF l_exists = 1 THEN     /*capacity constrained resource */
          IF l_rsrc_rec.capacity_um <> l_batch_step.step_qty_um THEN
	    l_temp_qty := inv_convert.inv_um_convert(item_id => 0
                                        ,PRECISION          => gme_common_pvt.g_precision
                                        ,from_quantity      => l_rsrc_rec.max_capacity
                                        ,from_unit          => l_rsrc_rec.capacity_um
                                        ,to_unit            => l_batch_step.step_qty_um
                                        ,from_name          => NULL
                                        ,to_name            => NULL);
           IF l_temp_qty < 0 THEN
	     RAISE error_in_conversion;
	   END IF;
	   IF l_temp_qty < l_batch_step.max_step_capacity THEN
              l_batch_step.max_step_capacity := l_temp_qty;
	   END IF;
	  END IF;

         IF p_action_code = 'UPDATE' THEN
	   /* check whether resource is the one that determines charges */
	   OPEN cur_get_charge_rsrc(l_batch_step.batchstep_id,x_step_resources_rec.resources);
	   FETCH cur_get_charge_rsrc INTO l_exists;
	   CLOSE cur_get_charge_rsrc;
	   IF l_exists = 1 AND x_step_resources_rec.scale_type <> 2 THEN
	     /* this rsrc is determining resource since scale type is not By Charge delete the charge details */
	     gme_batch_step_chg_pvt.clear_charges(
                      p_batch_id        => l_batch_step.batch_id
                     ,p_batchstep_id    => l_batch_step.batchstep_id
                     ,x_return_status   => l_return_status);

             IF l_return_status <> fnd_api.g_ret_sts_success THEN
              RAISE error_in_clear_charges;
             END IF;
	   END IF;

         ELSIF p_action_code IN ('INSERT', 'DELETE') THEN
	   --call recalculate charges procedure with R as p_cal_type
	   gme_update_step_qty_pvt.recalculate_charges(
	       p_batchstep_rec => l_batch_step
              ,p_cal_type      => 'R'
              ,x_batchstep_rec => x_batch_step
              ,x_return_status => l_return_status );

	   IF l_return_status <> fnd_api.g_ret_sts_success THEN
            RAISE fnd_api.g_exc_error;
           END IF;
         END IF;  /*action code check */
      END IF; /*p_step_resources_rec.capacity_constraint = 1 */
      /*Bug#5231180 End */
      
      /*                                    
      BEGIN ER 19161894  Shaliu Chen 18-JUL-2014               
      Added for OPM Step Level OSP Project  
      If plan resource total usage is changed,then invoke quantity 
      change synchronization program for OSP resource.
      */        
     IF p_action_code = 'UPDATE' THEN
        
        IF g_debug <= gme_debug.g_log_statement THEN
               gme_debug.put_line (   g_pkg_name
                                   || '.'
                                   || l_api_name
                                   || ' Calling quantity synchronization API');
        END IF;

        BEGIN
          /*
           Check GME parameters
          */
          OPEN cur_get_osp_parameter(l_batch_header.organization_id);
          FETCH cur_get_osp_parameter INTO l_propagate_change_to_po;
          CLOSE cur_get_osp_parameter;

          IF (NVL(l_propagate_change_to_po,gme_osp.g_propagate_change_manual) 
                     = gme_osp.g_propagate_change_automatic)
               AND gme_osp.check_release_version THEN
             /* 
              Check whether the batch include Outside resource.
             */
             /*                                    
              BEGIN ER 20809749                    
              Shaliu Chen 10-APR-2015               
              Add condition - batchstep_resource_id to confirm 
              the certain osp step to support multiple osp step.
             */    
            SELECT count(1)
              INTO l_osp_resource_flag
              FROM gme_batch_step_resources gbsr
             WHERE batch_id = l_batch_header.batch_id
               AND batchstep_resource_id = x_step_resources_rec.batchstep_resource_id
               AND EXISTS (SELECT 1
                             FROM cr_rsrc_dtl crd,
                                  cr_rsrc_mst crm
                            WHERE crd.organization_id = gbsr.organization_id
                              AND crd.resources = gbsr.resources
                              AND crd.resources = crm.resources
                              AND crd.purchase_item_id IS NOT NULL
                              AND NVL(crm.outside_process_ind,gme_osp.g_osp_ind_disabled) = gme_osp.g_osp_ind_enabled);
                                   
            IF l_osp_resource_flag = 1 THEN
              /*
                Invoke Batch product quantity change to the PO/Req
                linked to the batch.
              */                                                                      
              gme_osp.updatePOReqQuantity(p_batch_id                    => l_batch_header.batch_id,
                                          p_organization_id             => l_batch_header.organization_id,
                                          p_batchstep_resource_id       => x_step_resources_rec.batchstep_resource_id,                             
                                          x_return_status               => l_return_status,
                                          x_message_list                => l_message_list,
                                          x_message_count               => l_message_count);                                           
                                                                                 
                                              
                IF x_return_status <> fnd_api.g_ret_sts_success THEN       
                  RAISE qty_sync_fail; 
                END IF;
            END IF;
          END IF;
        EXCEPTION
          WHEN OTHERS THEN
            l_message_list := 'unexpected error: ' || SQLERRM || 'SQLCODE = ' || SQLCODE;
            gme_debug.put_line('l_errMsg:'||l_message_list);
            fnd_message.set_name('FND', 'FND_GENERIC_MESSAGE');
            fnd_message.set_token('MESSAGE', l_message_list);
            fnd_msg_pub.add;
            RAISE qty_sync_fail;          
        END;                                              
      END IF;      

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;
   EXCEPTION
      WHEN   gmf_cost_failure THEN
         -- Bug 5903208
         x_return_status := FND_API.G_RET_STS_ERROR;

      WHEN fnd_api.g_exc_error THEN
         x_return_status := fnd_api.g_ret_sts_error;
      /*ER 19161894  Shaliu Chen 18-JUL-2014*/   
      WHEN qty_sync_fail THEN
        x_return_status := 'W';            
      WHEN OTHERS THEN
         IF g_debug <= gme_debug.g_log_unexpected THEN
            gme_debug.put_line (   'When others exception in '
                                || g_pkg_name
                                || '.'
                                || l_api_name
                                || ' Error is '
                                || SQLERRM);
         END IF;

         x_return_status := fnd_api.g_ret_sts_unexp_error;
         fnd_msg_pub.add_exc_msg ('GME_RESOURCE_ENGINE_PVT'
                                 ,'resource_dtl_process');
   END resource_dtl_process;

/*===========================================================================================
Procedure
  validate_resource
Description
  This particular procedure is used to check for the validity of the fields.
Parameters
  p_step_resources_rec     The batch step resource rec to identify the resource
  x_return_status    outcome of the API call
            S - Success
            E - Error
            U - Unexpected error
History
 Bharati Satpute  Bug 2165993  1/09/2002 Incomprehensible error message when inserting a resource
=============================================================================================*/
   PROCEDURE validate_resource (
      p_batch_step_rec       IN              gme_batch_steps%ROWTYPE
     ,p_step_activity_rec    IN              gme_batch_step_activities%ROWTYPE
     ,p_step_resources_rec   IN              gme_batch_step_resources%ROWTYPE
     ,p_check_prim_rsrc      IN              BOOLEAN := FALSE
     ,x_return_status        OUT NOCOPY      VARCHAR2)
   IS
      l_api_name   CONSTANT VARCHAR2 (30) := 'validate_resource';
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      /* Initialize return status to success */
      x_return_status := fnd_api.g_ret_sts_success;

      IF (    p_batch_step_rec.step_status = 1
          AND p_step_resources_rec.plan_start_date IS NULL) THEN
         gme_common_pvt.log_message ('GME_START_DATE_REQD');
         RAISE fnd_api.g_exc_error;
      END IF;

      IF     p_batch_step_rec.step_status = 1
         AND p_step_resources_rec.plan_rsrc_usage IS NULL THEN
         gme_common_pvt.log_message ('GME_PLAN_RSRC_REQD');
         RAISE fnd_api.g_exc_error;
      END IF;

      IF (    p_batch_step_rec.step_status = 1
          AND p_step_resources_rec.plan_rsrc_qty IS NULL) THEN
         gme_common_pvt.log_message ('GME_RSRC_QTY_REQD');
         RAISE fnd_api.g_exc_error;
      END IF;

      IF (    p_batch_step_rec.step_status = 1
          AND p_step_resources_rec.plan_rsrc_count IS NULL) THEN
         gme_common_pvt.log_message ('GME_RSRC_COUNT_REQD');
         RAISE fnd_api.g_exc_error;
      END IF;

      IF (    p_batch_step_rec.step_status > 2
          AND p_step_resources_rec.actual_rsrc_qty IS NULL) THEN
         gme_common_pvt.log_message ('GME_RSRC_QTY_REQD');
         RAISE fnd_api.g_exc_error;
      END IF;

      IF (    p_batch_step_rec.step_status > 2
          AND p_step_resources_rec.actual_rsrc_count IS NULL) THEN
         gme_common_pvt.log_message ('GME_RSRC_COUNT_REQD');
         RAISE fnd_api.g_exc_error;
      END IF;

      /* Let us check for the validitiy of the plan and actual dates */
      IF     p_step_resources_rec.plan_start_date IS NOT NULL
         AND p_step_resources_rec.plan_cmplt_date IS NOT NULL THEN
         IF p_step_resources_rec.plan_start_date >
                                         p_step_resources_rec.plan_cmplt_date THEN
            gme_common_pvt.log_message ('PM_BADSTARTDATE');
            RAISE fnd_api.g_exc_error;
         END IF;
      END IF;

      IF     p_step_resources_rec.actual_start_date IS NOT NULL
         AND p_step_resources_rec.actual_cmplt_date IS NOT NULL THEN
         IF p_step_resources_rec.actual_start_date >
                                       p_step_resources_rec.actual_cmplt_date THEN
            gme_common_pvt.log_message ('PM_BADSTARTDATE');
            RAISE fnd_api.g_exc_error;
         END IF;
      END IF;

      IF     p_step_resources_rec.plan_start_date IS NOT NULL
         AND (   p_step_resources_rec.plan_start_date <
                                           p_step_activity_rec.plan_start_date
              OR p_step_resources_rec.plan_start_date >
                                           p_step_activity_rec.plan_cmplt_date) THEN
         gme_common_pvt.log_message
            ('GME_RSRC_PLAN_DATE'
            ,'START_DATE'
            ,fnd_date.date_to_displaydt (p_step_activity_rec.plan_start_date)
            ,'END_DATE'
            ,fnd_date.date_to_displaydt (p_step_activity_rec.plan_cmplt_date) );
         RAISE fnd_api.g_exc_error;
      END IF;

      IF     p_step_resources_rec.plan_cmplt_date IS NOT NULL
         AND p_step_resources_rec.plan_cmplt_date >
                                           p_step_activity_rec.plan_cmplt_date THEN
         gme_common_pvt.log_message
            ('GME_RSRC_PLAN_DATE'
            ,'START_DATE'
            ,fnd_date.date_to_displaydt (p_step_activity_rec.plan_start_date)
            ,'END_DATE'
            ,fnd_date.date_to_displaydt (p_step_activity_rec.plan_cmplt_date) );
         RAISE fnd_api.g_exc_error;
      END IF;

      IF     p_step_resources_rec.actual_start_date IS NOT NULL
         AND (   p_step_resources_rec.actual_start_date <
                                         p_step_activity_rec.actual_start_date
              OR p_step_resources_rec.actual_start_date >
                                         p_step_activity_rec.actual_cmplt_date) THEN
         gme_common_pvt.log_message
            ('GME_RSRC_ACTUAL_DATE'
            ,'START_DATE'
            ,fnd_date.date_to_displaydt (p_step_activity_rec.actual_start_date)
            ,'END_DATE'
            ,fnd_date.date_to_displaydt
                                 (NVL (p_step_activity_rec.actual_cmplt_date
                                      ,p_step_activity_rec.actual_start_date) ) );
         RAISE fnd_api.g_exc_error;
      END IF;

      IF     p_step_resources_rec.actual_cmplt_date IS NOT NULL
         AND p_step_resources_rec.actual_cmplt_date >
                                         p_step_activity_rec.actual_cmplt_date THEN
         gme_common_pvt.log_message
            ('GME_RSRC_ACTUAL_DATE'
            ,'START_DATE'
            ,fnd_date.date_to_displaydt (p_step_activity_rec.actual_start_date)
            ,'END_DATE'
            ,fnd_date.date_to_displaydt (p_step_activity_rec.actual_cmplt_date) );
         RAISE fnd_api.g_exc_error;
      END IF;

      /* Only one primary resource is allowed per activity let us check for it now */
      /* Remove this call as per Resource TD page 57                               */
      /* Call to be made from GME_API_PUB.save_batch                               */
      /*
      IF p_check_prim_rsrc THEN
        check_primary_resource (p_step_resources_rec => p_step_resources_rec
                               ,x_return_status  => x_return_status);
      END IF;
      */
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;
   EXCEPTION
      WHEN fnd_api.g_exc_error THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN OTHERS THEN
         IF g_debug <= gme_debug.g_log_unexpected THEN
            gme_debug.put_line (   'When others exception in '
                                || g_pkg_name
                                || '.'
                                || l_api_name
                                || ' Error is '
                                || SQLERRM);
         END IF;

         x_return_status := fnd_api.g_ret_sts_unexp_error;
         fnd_msg_pub.add_exc_msg ('GME_RESOURCE_ENGINE_PVT'
                                 ,'VALIDATE_RESOURCE');
   END validate_resource;

/*===========================================================================================
Procedure
  check_primary_resource
Description
  This particular procedure is used to check for the existence of one and only one primary
  resource associated with the activity.
Parameters
  p_step_resources_rec     The batch step resource rec to identify the resource
  x_return_status    outcome of the API call
            S - Success
            E - Error
            U - Unexpected error
=============================================================================================*/
   PROCEDURE check_primary_resource (
      p_step_resources_rec   IN              gme_batch_step_resources%ROWTYPE
     ,x_return_status        OUT NOCOPY      VARCHAR2)
   IS
      l_api_name   CONSTANT VARCHAR2 (30)         := 'check_primary_resource';

      CURSOR cur_get_prim_rsrc (
         v_batchstep_activity_id   NUMBER
        ,v_batchstep_resource_id   NUMBER)
      IS
         SELECT COUNT (1)
           FROM gme_batch_step_resources
          WHERE batchstep_resource_id <> NVL (v_batchstep_resource_id, -1)
            AND batchstep_activity_id = v_batchstep_activity_id
            AND prim_rsrc_ind = 1;

      --Rishi 3446787/3020345 start
      CURSOR cur_get_activity (v_batchstep_activity_id NUMBER)
      IS
         SELECT activity
           FROM gme_batch_step_activities
          WHERE batchstep_activity_id = v_batchstep_activity_id;

      CURSOR cur_get_step_id (v_batchstep_activity_id NUMBER)
      IS
         SELECT batchstep_id
           FROM gme_batch_step_activities
          WHERE batchstep_activity_id = v_batchstep_activity_id;

      CURSOR cur_get_batchstep_no (v_batchstep_id NUMBER)
      IS
         SELECT batchstep_no
           FROM gme_batch_steps
          WHERE batchstep_id = v_batchstep_id;

      l_activity            gme_batch_step_activities.activity%TYPE;
      l_batchstep_no        gme_batch_steps.batchstep_no%TYPE;
      l_step_id             gme_batch_steps.batchstep_id%TYPE;
      --Rishi 3446787/3020345 end
      l_count               NUMBER (5);
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      /* Initialize return status to success */
      x_return_status := fnd_api.g_ret_sts_success;

      OPEN cur_get_prim_rsrc (p_step_resources_rec.batchstep_activity_id
                             ,p_step_resources_rec.batchstep_resource_id);

      FETCH cur_get_prim_rsrc
       INTO l_count;

      CLOSE cur_get_prim_rsrc;

      --Rishi 3446787/3020345 start
      OPEN cur_get_activity (p_step_resources_rec.batchstep_activity_id);

      FETCH cur_get_activity
       INTO l_activity;

      CLOSE cur_get_activity;

      OPEN cur_get_step_id (p_step_resources_rec.batchstep_activity_id);

      FETCH cur_get_step_id
       INTO l_step_id;

      CLOSE cur_get_step_id;

      OPEN cur_get_batchstep_no (l_step_id);

      FETCH cur_get_batchstep_no
       INTO l_batchstep_no;

      CLOSE cur_get_batchstep_no;

      --Rishi 3446787/3020345 end
      IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
         gme_debug.put_line (   ' Activity:'
                             || p_step_resources_rec.batchstep_activity_id
                             || ' Resource:'
                             || p_step_resources_rec.batchstep_resource_id
                             || ' Count:'
                             || l_count);
      END IF;

      IF (l_count > 0) AND (NVL (p_step_resources_rec.prim_rsrc_ind, 0) = 1) THEN
         fnd_message.set_name ('GME', 'GME_ONE_PRIM_RSRC');
         --Rishi 3446787/3020345
         fnd_message.set_token ('ACTIVITY', l_activity);
         fnd_message.set_token ('STEPNO', l_batchstep_no);
         fnd_msg_pub.ADD;
         RAISE fnd_api.g_exc_error;
      ELSIF (l_count = 0) AND NVL (p_step_resources_rec.prim_rsrc_ind, 0) = 0 THEN
         fnd_message.set_name ('GME', 'GME_MIN_ONE_PRIM_RSRC');
         fnd_message.set_token ('ACTIVITY', l_activity);
         fnd_message.set_token ('STEPNO', l_batchstep_no);
         --Rishi 3446787/3020345 end
         fnd_msg_pub.ADD;
         RAISE fnd_api.g_exc_error;
      END IF;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;
   EXCEPTION
      WHEN fnd_api.g_exc_error THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN OTHERS THEN
         IF g_debug <= gme_debug.g_log_unexpected THEN
            gme_debug.put_line (   'When others exception in '
                                || g_pkg_name
                                || '.'
                                || l_api_name
                                || ' Error is '
                                || SQLERRM);
         END IF;

         x_return_status := fnd_api.g_ret_sts_unexp_error;
         fnd_msg_pub.add_exc_msg ('GME_RESOURCE_ENGINE_PVT'
                                 ,'CHECK_PRIMARY_RESOURCE');
   END check_primary_resource;

/*===========================================================================================
Procedure
  check_primary_resource
Description
  This procedure is used to check for the existence of one and only one primary resource.
Parameters
  p_batch_id                 Batch row identifier
  p_batchstep_id                Batchstep row identifier
  x_return_status    outcome of the API call
            S - Success
            E - Error
            U - Unexpected error
=============================================================================================*/
   PROCEDURE check_primary_resource (
      p_batch_id        IN              NUMBER
     ,p_batchstep_id    IN              NUMBER
     ,x_return_status   OUT NOCOPY      VARCHAR2)
   IS
      CURSOR cur_get_batch_steps
      IS
         SELECT batch_id, batchstep_no, batchstep_id
           FROM gme_batch_steps
          WHERE batch_id = p_batch_id
            AND batchstep_id = NVL (p_batchstep_id, batchstep_id);

      CURSOR cur_get_batchstep_activities (
         v_batch_id       NUMBER
        ,v_batchstep_id   NUMBER)
      IS
         SELECT activity, batchstep_activity_id
           FROM gme_batch_step_activities
          WHERE batch_id = v_batch_id AND batchstep_id = v_batchstep_id;

      CURSOR cur_get_prim_rsrc_count (v_batchstep_activity_id NUMBER)
      IS
         SELECT COUNT (1)
           FROM gme_batch_step_resources
          WHERE batchstep_activity_id = v_batchstep_activity_id
            AND prim_rsrc_ind = 1;

      l_count   NUMBER (5);
   BEGIN
      /* Initialize return status to success */
      x_return_status := fnd_api.g_ret_sts_success;

      FOR batchsteps IN cur_get_batch_steps LOOP
         FOR activities IN
            cur_get_batchstep_activities (p_batch_id
                                         ,batchsteps.batchstep_id) LOOP
            IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
               gme_debug.put_line (   ' Activity:'
                                   || activities.batchstep_activity_id);
            END IF;

            OPEN cur_get_prim_rsrc_count (activities.batchstep_activity_id);

            FETCH cur_get_prim_rsrc_count
             INTO l_count;

            CLOSE cur_get_prim_rsrc_count;

            IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
               gme_debug.put_line (' Count:' || l_count);
            END IF;

            IF (l_count > 1) THEN
               fnd_message.set_name ('GME', 'GME_ONE_PRIM_RSRC');
               fnd_message.set_token ('ACTIVITY', activities.activity);
               fnd_message.set_token ('STEPNO', batchsteps.batchstep_no);
               fnd_msg_pub.ADD;
               RAISE fnd_api.g_exc_error;
            ELSIF (l_count = 0) THEN
               fnd_message.set_name ('GME', 'GME_MIN_ONE_PRIM_RSRC');
               fnd_message.set_token ('ACTIVITY', activities.activity);
               fnd_message.set_token ('STEPNO', batchsteps.batchstep_no);
               fnd_msg_pub.ADD;
               RAISE fnd_api.g_exc_error;
            END IF;
         END LOOP;
      END LOOP;
   EXCEPTION
      WHEN fnd_api.g_exc_error THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN OTHERS THEN
         x_return_status := fnd_api.g_ret_sts_unexp_error;
         fnd_msg_pub.add_exc_msg ('GME_RESOURCE_ENGINE_PVT'
                                 ,'CHECK_PRIMARY_RESOURCE');
   END check_primary_resource;

/*===========================================================================================
Procedure
  get_resource_usage
Description
  This particular procedure is used to get the resource usage based on the transactions.
Parameters
  p_step_resources_rec     The batch step resource rec to identify the resource
  x_step_resources_rec     The batch step resource rec to identify the resource
  x_return_status    outcome of the API call
            S - Success
            E - Error
            U - Unexpected error
            Pawan kumar added code for bug 2294055
=============================================================================================*/
   PROCEDURE get_resource_usage (
      p_step_resources_rec   IN              gme_batch_step_resources%ROWTYPE
     ,x_step_resources_rec   OUT NOCOPY      gme_batch_step_resources%ROWTYPE
     ,x_return_status        OUT NOCOPY      VARCHAR2)
   IS
      l_api_name   CONSTANT VARCHAR2 (30) := 'get_resource_usage';

      CURSOR cur_get_stat
      IS
         SELECT step_status
           FROM gme_batch_steps
          WHERE batchstep_id = p_step_resources_rec.batchstep_id;

      /* Bug 2651477 added delete_mark condition */
      CURSOR cur_get_usage (v_completed_ind NUMBER)
      IS
         SELECT SUM (resource_usage)
           FROM gme_resource_txns_gtmp
          WHERE line_id = p_step_resources_rec.batchstep_resource_id
            AND completed_ind = v_completed_ind
            AND (v_completed_ind = 1 OR sequence_dependent_ind = 0)
            AND action_code <> 'DEL'
            AND NVL (delete_mark, 0) <> 1;

      l_status              NUMBER (5);
      l_tot_usage           NUMBER;
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      /* Initialize return status to success */
      x_return_status := fnd_api.g_ret_sts_success;
      x_step_resources_rec.batchstep_resource_id :=
                                    p_step_resources_rec.batchstep_resource_id;

      IF NOT gme_batch_step_resources_dbl.fetch_row
                              (p_batch_step_resources      => x_step_resources_rec
                              ,x_batch_step_resources      => x_step_resources_rec) THEN
         RAISE fnd_api.g_exc_error;
      END IF;

      OPEN cur_get_stat;

      FETCH cur_get_stat
       INTO l_status;

      CLOSE cur_get_stat;

      IF l_status = 1 THEN
         OPEN cur_get_usage (0);

         FETCH cur_get_usage
          INTO l_tot_usage;

         CLOSE cur_get_usage;

         x_step_resources_rec.plan_rsrc_usage := l_tot_usage;
      ELSE
         OPEN cur_get_usage (1);

         FETCH cur_get_usage
          INTO l_tot_usage;

         CLOSE cur_get_usage;

         x_step_resources_rec.actual_rsrc_usage := l_tot_usage;
      END IF;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;
   EXCEPTION
      WHEN fnd_api.g_exc_error THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN OTHERS THEN
         IF g_debug <= gme_debug.g_log_unexpected THEN
            gme_debug.put_line (   'When others exception in '
                                || g_pkg_name
                                || '.'
                                || l_api_name
                                || ' Error is '
                                || SQLERRM);
         END IF;

         x_return_status := fnd_api.g_ret_sts_unexp_error;
         fnd_msg_pub.add_exc_msg ('GME_RESOURCE_ENGINE_PVT'
                                 ,'GET_RESOURCE_USAGE');
   END get_resource_usage;

/*===========================================================================================
   Procedure
      validate_rsrc_txn_param
   Description
     This particular procedure is used to validate all parameters passed to rsrc txn APIs
   Parameters
     p_batchstep_rsrc_id OR
     (p_plant_code,p_batch_no,step_no,activity and resource)   to uniquely identify a resource
     p_trans_date                 transaction date of resource txn
     p_start_date                 start date of resource txn
     p_end_date                   end date of resource txn
     p_usage                      resource usage of the txns and resource
     p_reason_code                reason to insert a completed rsrc txn
     p_instance_id                instance_id of the instance of rsrc txn(for WPS)
     p_instance_no                instance_no of the instance of rsrc txn(for WPS)
     x_return_status              reflects return status of the API
   HISTORY
     09JULY03 BUG#3041697  V. Ajay Kumar
     Port bug 2965879 to 11.5.10K.
     Modified code such that an error is not raised if the
     reason code is not entered.
     02-SEP-2004 Rishi Varma  B3856541
     Made changes for the rsrc txns in closed period ME.Added an extra parameter x_trans_date
     and replaced the gmi call with call to the new grp layer procedure.
=============================================================================================*/
   PROCEDURE validate_rsrc_txn_param (
      p_called_from         IN              NUMBER
     ,p_batchstep_rsrc_id   IN              NUMBER
     ,p_org_code            IN              VARCHAR2
     ,p_batch_no            IN              VARCHAR2 := NULL
     ,p_batchstep_no        IN              NUMBER := NULL
     ,p_activity            IN              VARCHAR2 := NULL
     ,p_resource            IN              VARCHAR2 := NULL
     ,p_trans_date          IN              DATE
     ,p_start_date          IN              DATE
     ,p_end_date            IN              DATE
     ,p_usage               IN              NUMBER
     ,p_reason_name         IN              VARCHAR2
     ,p_reason_id           IN              NUMBER
     ,p_instance_id         IN              NUMBER
     ,p_instance_no         IN              NUMBER
     ,x_line_id             OUT NOCOPY      NUMBER
     ,x_step_status         OUT NOCOPY      NUMBER
     ,x_batch_header_rec    OUT NOCOPY      gme_batch_header%ROWTYPE
     ,x_instance_id         OUT NOCOPY      NUMBER
     ,x_reason_id           OUT NOCOPY      NUMBER
     ,x_return_status       OUT NOCOPY      VARCHAR2
     ,
      --Rishi Varma B3856541 02-09-2004 start
      x_trans_date          OUT NOCOPY      DATE)
   IS
      CURSOR cur_get_batch_id (v_org_id VARCHAR2, v_batch_no VARCHAR2)
      IS
         SELECT batch_id
           FROM gme_batch_header
         WHERE  organization_id = v_org_id
            AND batch_no = v_batch_no
            AND batch_type = 0;

      CURSOR cur_get_batchstep_id (v_batch_id NUMBER, v_batchstep_no NUMBER)
      IS
         SELECT batchstep_id
           FROM gme_batch_steps
          WHERE batch_id = v_batch_id AND batchstep_no = v_batchstep_no;

      CURSOR cur_get_batch_details (v_resource_id NUMBER)
      IS
         SELECT a.batch_id, a.resources
           FROM gme_batch_step_resources a
          WHERE a.batchstep_resource_id = v_resource_id;

      /* Bug 2685645 added batch_id param and used in where clause */
      CURSOR cur_get_activity_id (
         v_step_id    NUMBER
        ,v_activity   VARCHAR2
        ,v_batch_id   NUMBER)
      IS
         SELECT batchstep_activity_id
           FROM gme_batch_step_activities
          WHERE batchstep_id = v_step_id
            AND batch_id = v_batch_id
            AND activity = v_activity;

      CURSOR cur_fetch_resource_dtl (v_activity_id NUMBER, v_resource VARCHAR2)
      IS
         SELECT batchstep_resource_id, resources
           FROM gme_batch_step_resources
          WHERE batchstep_activity_id = v_activity_id
            AND resources = v_resource;

      CURSOR cur_get_step_status (v_line_id NUMBER)
      IS
         SELECT step_status
           FROM gme_batch_steps a, gme_batch_step_resources b
          WHERE a.batchstep_id = b.batchstep_id
            AND b.batchstep_resource_id = v_line_id;

      CURSOR cur_get_instance_id (v_instance_no NUMBER, v_resource VARCHAR2)
      IS
         SELECT instance_id
           FROM gmp_resource_instances i, cr_rsrc_dtl r
          WHERE r.resource_id = i.resource_id
            AND r.resources = v_resource
            AND instance_number = v_instance_no;

      CURSOR cur_validate_instance_id (
         v_instance_id   NUMBER
        ,v_resource      VARCHAR2)
      IS
         SELECT 1
           FROM gmp_resource_instances i, cr_rsrc_dtl r
          WHERE r.resource_id = i.resource_id
            AND r.resources = v_resource
            AND instance_id = v_instance_id;

      CURSOR cur_get_rsrc_actual_dates (v_line_id NUMBER)
      IS
         SELECT actual_start_date, actual_cmplt_date
           FROM gme_batch_step_resources
          WHERE batchstep_resource_id = v_line_id;

      CURSOR cur_validate_reason_id (v_reason_id NUMBER)
      IS
         SELECT reason_id
           FROM mtl_transaction_reasons
          WHERE NVL (disable_date, SYSDATE + 1) > SYSDATE
            AND reason_id = v_reason_id;

      CURSOR cur_get_reason_id (v_reason_name VARCHAR2)
      IS
         SELECT reason_id
           FROM mtl_transaction_reasons
          WHERE NVL (disable_date, SYSDATE + 1) > SYSDATE
            AND reason_name = v_reason_name;

      l_api_name           CONSTANT VARCHAR2 (30) := 'validate_rsrc_txn_param';
      l_batch_header                gme_batch_header%ROWTYPE;
      l_tran_rec                    gmi_trans_engine_pub.ictran_rec;
      l_step_status                 NUMBER;
      l_batch_id                    NUMBER;
      l_batchstep_id                NUMBER;
      l_activity_id                 NUMBER;
      l_dummy                       NUMBER;
      l_instance_no                 NUMBER;
      l_instance_id                 NUMBER                          := NULL;
      l_rsrc_actual_start_date      DATE;
      l_rsrc_actual_cmplt_date      DATE;
      l_return_status               VARCHAR2 (2);
      l_line_id                     NUMBER;
      l_overrided_protected_ind     VARCHAR2 (1);
      l_resource                    VARCHAR2 (16);
      l_batch_step_rsrc_rec_in      gme_batch_step_resources%ROWTYPE;
      l_batch_step_rsrc_rec         gme_batch_step_resources%ROWTYPE;
      --Rishi Varma 02-09-2004 B3856541
      /* start , Punit Kumar */
      /*
      p_tran_rec            gmi_trans_engine_pub.ictran_rec;
      l_tran_rec_out        gmi_trans_engine_pub.ictran_rec;
      */

      /*start , Punit Kumar */
      l_org_id                      NUMBER;
      l_period_id                   INTEGER;
      /* end */
      rtxn_for_fpo_not_allowed      EXCEPTION;
      batch_hdr_fetch_err           EXCEPTION;
      neg_usage_not_allowed         EXCEPTION;
      invalid_step_status           EXCEPTION;
      invalid_reason_code           EXCEPTION;
      invalid_date                  EXCEPTION;
      invalid_instance_id           EXCEPTION;
      invalid_instance_no           EXCEPTION;
      batch_not_found               EXCEPTION;
      rsrcid_not_found              EXCEPTION;
      batchstep_not_found           EXCEPTION;
      stepactivity_not_found        EXCEPTION;
      resource_not_found            EXCEPTION;
      close_period_err              EXCEPTION;
      step_status_asqc_error        EXCEPTION;
      asqc_update_rsrc_api_error    EXCEPTION;
      asqc_ovrd_end_txn_api_error   EXCEPTION;
      rtxn_for_updinv_not_allowed   EXCEPTION;
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      /* Initially let us assign the return status to success */
      x_return_status := fnd_api.g_ret_sts_success;

      l_batch_step_rsrc_rec_in.batchstep_resource_id := p_batchstep_rsrc_id;
      IF NOT gme_common_pvt.get_batchstep_rsrc (
           p_batchstep_rsrc_rec   => l_batch_step_rsrc_rec_in
          ,p_org_code             => p_org_code
          ,p_batch_no             => p_batch_no
          ,p_batchstep_no         => p_batchstep_no
          ,p_activity             => p_activity
          ,p_resource             => p_resource
          ,x_batchstep_rsrc_rec   => l_batch_step_rsrc_rec) THEN
         raise rsrcid_not_found;
      END IF;
/*
      IF p_batchstep_rsrc_id IS NOT NULL THEN
         l_line_id := p_batchstep_rsrc_id;

         OPEN cur_get_batch_details (l_line_id);

         FETCH cur_get_batch_details
          INTO l_batch_id, l_resource;

         IF cur_get_batch_details%NOTFOUND THEN
            gme_common_pvt.log_message ('GME_RSRCID_NOT_FOUND'
                                       ,'BATCHSTEP_RSRC_ID'
                                       ,l_line_id);
            RAISE rsrcid_not_found;
         END IF;

         CLOSE cur_get_batch_details;

         IF p_called_from = 5 THEN
            -- This portion of the code would be executed only when we call this validation
            -- procedure from end_cmplt_actual_rsrc_txn
            -- B2498487 value of overrided_protected_ind is passed in p_resoure param
            l_overrided_protected_ind := p_resource;
         END IF;
      ELSE
         IF (   p_org_id IS NULL
             OR p_batch_no IS NULL
             OR p_batchstep_no IS NULL
             OR p_activity IS NULL
             OR p_resource IS NULL) THEN
            gme_common_pvt.log_message ('INPUT_PARMS_MISS'
                                       ,'PROC'
                                       , l_api_name || '.' || g_pkg_name);
            RAISE fnd_api.g_exc_error;
         ELSE
            -- Validate input param one by one to see if it identifies a resource correctly
            OPEN cur_get_batch_id (p_org_id, p_batch_no);

            FETCH cur_get_batch_id
             INTO l_batch_id;

            IF cur_get_batch_id%NOTFOUND THEN
               gme_common_pvt.log_message ('GME_BATCH_NOT_FOUND');
               RAISE batch_not_found;
            END IF;

            CLOSE cur_get_batch_id;

            -- use batch_id to fetch batchstep_id
            OPEN cur_get_batchstep_id (l_batch_id, p_batchstep_no);

            FETCH cur_get_batchstep_id
             INTO l_batchstep_id;

            IF cur_get_batchstep_id%NOTFOUND THEN
               gme_common_pvt.log_message ('GME_BATCH_STEP_NOT_FOUND'
                                          ,'STEP_ID'
                                          ,p_batchstep_no);
               RAISE batchstep_not_found;
            END IF;

            CLOSE cur_get_batchstep_id;

            -- fetch activity_id
            OPEN cur_get_activity_id (l_batchstep_id, p_activity, l_batch_id);

            FETCH cur_get_activity_id
             INTO l_activity_id;

            IF cur_get_activity_id%NOTFOUND THEN
               gme_common_pvt.log_message ('GME_STEP_ACTIVITY_NOT_FOUND'
                                          ,'ACTIVITY'
                                          ,p_activity
                                          ,'STEP_NO'
                                          ,p_batchstep_no);
               RAISE stepactivity_not_found;
            END IF;

            CLOSE cur_get_activity_id;

            -- fetch resource id
            OPEN cur_fetch_resource_dtl (l_activity_id, p_resource);

            FETCH cur_fetch_resource_dtl
             INTO l_line_id, l_resource;

            IF cur_fetch_resource_dtl%NOTFOUND THEN
               gme_common_pvt.log_message ('GME_RSRC_NOT_FOUND'
                                          ,'RESOURCE'
                                          ,p_resource
                                          ,'ACTIVITY'
                                          ,p_activity);
               RAISE resource_not_found;
            END IF;

            CLOSE cur_fetch_resource_dtl;
         END IF;                  -- if plant_code etc input param is NOT NULL
      END IF;                        -- if p_batchstep_resource_id is NOT NULL
*/
      x_line_id := l_batch_step_rsrc_rec.batchstep_resource_id;

      /* V. Ajay Kumar  BUG#3041697  Removed the check for reason code */
      IF (    (p_trans_date IS NULL AND p_called_from <> 5)
          OR ( (p_start_date IS NULL) AND (p_batchstep_rsrc_id <> -1) )
          OR ( (p_end_date IS NULL) AND (p_usage <> 0) ) ) THEN
         gme_common_pvt.log_message ('INPUT_PARMS_MISS'
                                    ,'PROC'
                                    , l_api_name || '.' || g_pkg_name);
         RAISE fnd_api.g_exc_error;
      END IF;

      l_batch_header.batch_id := l_batch_step_rsrc_rec.batch_id;

      IF NOT gme_batch_header_dbl.fetch_row (p_batch_header      => l_batch_header
                                            ,x_batch_header      => l_batch_header) THEN
         RAISE batch_hdr_fetch_err;
      END IF;

      /* We cannot insert allocations for an FPO */
      IF l_batch_header.batch_type = 10 THEN
         gme_common_pvt.log_message ('GME_RTXN_FOR_FPO_NT_ALWD');
         RAISE rtxn_for_fpo_not_allowed;
      END IF;

      /* We cannot insert txns if the batch does not support txns*/
      IF l_batch_header.update_inventory_ind = 'N' THEN
         gme_common_pvt.log_message ('GME_RTXN_FOR_UPDINV_NT_ALWD');
         RAISE rtxn_for_updinv_not_allowed;
      END IF;

      IF p_usage IS NOT NULL THEN
         IF p_usage < 0 THEN
            gme_common_pvt.log_message ('GME_NEG_USAGE_NT_ALWD');
            RAISE neg_usage_not_allowed;
         END IF;
      END IF;

      OPEN cur_get_step_status (l_batch_step_rsrc_rec.batchstep_resource_id);

      FETCH cur_get_step_status
       INTO l_step_status;

      CLOSE cur_get_step_status;

      IF l_step_status NOT IN (2, 3) THEN
         gme_common_pvt.log_message ('PC_STEP_STATUS_ERR');
         RAISE invalid_step_status;
      END IF;

      x_step_status := l_step_status;

      IF (l_batch_header.automatic_step_calculation = 1) THEN
         IF p_called_from = 1 THEN
            gme_common_pvt.log_message ('GME_UPD_RSRC_NT_WRK_ASQCBTCH');
            RAISE asqc_update_rsrc_api_error;
         END IF;

         IF (l_step_status = 2) THEN
            gme_common_pvt.log_message ('GME_INV_STEP_STATUS_ASQC');
            RAISE step_status_asqc_error;
         END IF;

         -- Shikha Nagar B2498487 Added code to prevent ending txn
         -- for automatically generated ASQC txn
         IF (    p_called_from = 5
             AND l_step_status = 3
             AND l_overrided_protected_ind = 'N') THEN
            gme_common_pvt.log_message ('GME_NT_END_ASQC_OVRD_TXN');
            RAISE asqc_ovrd_end_txn_api_error;
         END IF;
      END IF;                            /* If ASQC is on */
                                 /* Lets validate the reason code passed in */

      IF p_reason_id IS NOT NULL THEN
         OPEN cur_validate_reason_id(p_reason_id);
         FETCH cur_validate_reason_id
          INTO x_reason_id;
         IF cur_validate_reason_id%NOTFOUND THEN
            CLOSE cur_validate_reason_id;
            gme_common_pvt.log_message(p_product_code => 'INV'
                                       ,p_message_code => 'INV_LOTC_REASONID_INVALID');
            RAISE FND_API.g_exc_error;
         END IF;
         CLOSE cur_validate_reason_id;
      ELSIF p_reason_name IS NOT NULL THEN
         OPEN cur_get_reason_id(p_reason_name);
         FETCH cur_get_reason_id
          INTO x_reason_id;
         IF cur_get_reason_id%NOTFOUND THEN
            CLOSE cur_get_reason_id;
            gme_common_pvt.log_message('GME_INVALID_REASON_NAME');
            RAISE FND_API.g_exc_error;
         ELSE
            FETCH cur_get_reason_id
             INTO x_reason_id;
            IF cur_get_reason_id%NOTFOUND THEN
               CLOSE cur_get_reason_id;
            ELSE
               CLOSE cur_get_reason_id;
               gme_common_pvt.log_message('GME_REASON_NAME_NOT_UNIQUE');
               RAISE FND_API.g_exc_error;
            END IF;
         END IF;
      END IF;  /* IF p_reason_name IS NOT NULL */

      /* Lets validate the instance id/no passed in */
      IF p_instance_id IS NOT NULL THEN
         OPEN cur_validate_instance_id (p_instance_id, l_batch_step_rsrc_rec.resources);

         FETCH cur_validate_instance_id
          INTO l_dummy;

         IF cur_validate_instance_id%NOTFOUND THEN
            gme_common_pvt.log_message ('GME_INVALID_INSTANCE_ID'
                                       ,'INSTANCE_ID'
                                       ,p_instance_id);

            CLOSE cur_validate_instance_id;

            RAISE invalid_instance_id;
         END IF;

         CLOSE cur_validate_instance_id;

         x_instance_id := p_instance_id;
      ELSIF p_instance_no IS NOT NULL THEN
         OPEN cur_get_instance_id (p_instance_no, l_batch_step_rsrc_rec.resources);

         FETCH cur_get_instance_id
          INTO l_instance_id;

         IF cur_get_instance_id%NOTFOUND THEN
            gme_common_pvt.log_message ('GME_INVALID_INSTANCE_NO'
                                       ,'INSTANCE_NO'
                                       ,p_instance_no);

            CLOSE cur_get_instance_id;

            RAISE invalid_instance_no;
         END IF;

         CLOSE cur_get_instance_id;
         --BUG 25259887 Shaliu Chen 
         --Move following statement into IF range.
         x_instance_id := l_instance_id;
      END IF;

      

      OPEN cur_get_rsrc_actual_dates (l_batch_step_rsrc_rec.batchstep_resource_id);

      FETCH cur_get_rsrc_actual_dates
       INTO l_rsrc_actual_start_date, l_rsrc_actual_cmplt_date;

      CLOSE cur_get_rsrc_actual_dates;

      -- Validate trans_date
      IF p_trans_date < l_rsrc_actual_start_date THEN
         gme_common_pvt.log_message ('GME_BAD_TRANS_DATE');
         RAISE invalid_date;
      ELSIF p_trans_date > gme_common_pvt.g_timestamp THEN
         gme_common_pvt.log_message ('GME_BAD_TRANS_SYS_DATE');
         RAISE invalid_date;
      -- 13345631 - This validation should not occur once the batch is created.
      -- ELSIF l_rsrc_actual_cmplt_date IS NOT NULL THEN
      --    IF p_trans_date > l_rsrc_actual_cmplt_date THEN
      --       gme_common_pvt.log_message ('GME_BAD_TRANS_CMPLT_DATE');
      --       RAISE invalid_date;
      --    END IF;
      ELSIF p_start_date IS NOT NULL THEN
         IF p_trans_date < p_start_date THEN
            gme_common_pvt.log_message ('GME_BAD_TRANS_STRT_DATE');
            RAISE invalid_date;
         END IF;
      ELSIF p_end_date IS NOT NULL THEN
         IF p_trans_date > p_end_date THEN
            gme_common_pvt.log_message ('GME_BAD_TRANS_END_DATE');
            RAISE invalid_date;
         END IF;
      END IF;

      --Rishi Varma 02-09-2004 B3856541 start
      /*Commenting the gmi call and adding call to the check_period_check_flexible
      procedure for checking resource txns. in closed periods*/
      /*l_tran_rec.trans_date := p_trans_date;
      l_tran_rec.orgn_code  := l_batch_header.plant_code;
      l_tran_rec.whse_code  := l_batch_header.wip_whse_code;
      IF NOT GMI_TRANS_ENGINE_PVT.close_period_check
         ( p_tran_rec   => l_tran_rec,
           p_retry_flag => 1,
           x_tran_rec   => l_tran_rec)
      THEN
        --Bug3315440
        -- gme_common_pvt.log_message('GME_DATE_IN_CLSD_PRD','TRANS_DATE',p_trans_date);
           gme_common_pvt.log_message('GME_DATE_IN_CLSD_PRD','TRANS_DATE',fnd_date.date_to_displayDT(p_trans_date));
        RAISE close_period_err;
      END IF;
      */

      ---- p_tran_rec.orgn_code := l_batch_header.plant_code;
       ----p_tran_rec.orgn_code := l_batch_header.ORGANIZATION_ID;
       ----p_tran_rec.whse_code := l_batch_header.wip_whse_code;

      /* start , Punit Kumar */
      /*
      p_tran_rec.trans_date := p_trans_date;
      IF NOT gme_api_grp.close_period_check_flexible
                       (p_tran_rec     => p_tran_rec,
                        x_tran_rec     => l_tran_rec_out)
      THEN
           RAISE FND_API.g_exc_error;
      END IF;
      x_trans_date := l_tran_rec_out.trans_date;
      --Rishi Varma 02-09-2004 B3856541 end
      */
      IF NOT gme_common_pvt.close_period_check_flexible
                                                (p_org_id          => l_batch_step_rsrc_rec.organization_id
                                                ,p_trans_date      => p_trans_date
                                                ,x_trans_date      => x_trans_date
                                                ,x_period_id       => l_period_id) THEN
         RAISE fnd_api.g_exc_error;
      END IF;

      /* end */

      -- Validate start_date
      IF p_start_date < l_rsrc_actual_start_date THEN
         gme_common_pvt.log_message ('GME_BAD_START_DATE');
         RAISE invalid_date;
      ELSIF p_start_date > gme_common_pvt.g_timestamp THEN
         gme_common_pvt.log_message ('GME_BAD_START_END_DATE');
         RAISE invalid_date;
      -- 13345631 - This validation should not occur once the batch is created.
      -- ELSIF l_rsrc_actual_cmplt_date IS NOT NULL THEN
      --    IF p_start_date > l_rsrc_actual_cmplt_date THEN
      --       gme_common_pvt.log_message ('GME_BAD_START_CMPLT_DATE');
      --       RAISE invalid_date;
      --    END IF;
      END IF;

      -- Validate END DATE
      IF p_end_date < p_start_date THEN
         gme_common_pvt.log_message ('PM_BADENDDATE');
         RAISE invalid_date;
      ELSIF p_end_date > gme_common_pvt.g_timestamp THEN
         gme_common_pvt.log_message ('GME_BAD_END_DATE');
         RAISE invalid_date;
      -- 13345631 - This validation should not occur once the batch is created.
      -- ELSIF l_rsrc_actual_cmplt_date IS NOT NULL THEN
      --    IF p_end_date > l_rsrc_actual_cmplt_date THEN
      --       gme_common_pvt.log_message ('GME_BAD_END_CMPLT_DATE');
      --       RAISE invalid_date;
      --    END IF;
      END IF;

      x_batch_header_rec := l_batch_header;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;
   EXCEPTION
      WHEN invalid_date OR close_period_err OR step_status_asqc_error OR asqc_update_rsrc_api_error OR asqc_ovrd_end_txn_api_error OR fnd_api.g_exc_error THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN invalid_instance_id OR invalid_instance_no THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN neg_usage_not_allowed OR batch_hdr_fetch_err OR rsrcid_not_found THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN batch_not_found OR batchstep_not_found OR invalid_step_status THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN rtxn_for_fpo_not_allowed THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN stepactivity_not_found OR resource_not_found OR rtxn_for_updinv_not_allowed THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN invalid_reason_code THEN
         fnd_message.set_name ('GMA', 'SY_REASONCODE');
         fnd_msg_pub.ADD;
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN OTHERS THEN
         IF g_debug <= gme_debug.g_log_unexpected THEN
            gme_debug.put_line (   'When others exception in '
                                || g_pkg_name
                                || '.'
                                || l_api_name
                                || ' Error is '
                                || SQLERRM);
         END IF;

         x_return_status := fnd_api.g_ret_sts_unexp_error;
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
   END validate_rsrc_txn_param;

/*===========================================================================================
   Procedure
      update_actual_resource_usage
   Description
     This particular procedure is used to insert completed rsrc txn rec for a resource and deletes
     all other existing rsrc txns.
   Parameters
     (p_org_code,p_batch_no,step_no,activity and resource)   to uniquely identify a resource
     p_trans_date                 transaction date of resource txn
     p_start_date                 start date of resource txn
     p_end_date                   end date of resource txn
     p_usage                      resource usage of the txns and resource
     p_reason_code                reason to insert a completed rsrc txn
     p_instance_id                instance_id of the instance of rsrc txn(for WPS)
     p_instance_no                instance_no of the instance of rsrc txn(for WPS)
     x_return_status              reflects return status of the API
     02-SEP-04 Rishi Varma B3856541
      Added the new parameter to the validate_rsrc_param procedure call.
     01-OCT-04 Rishi Varma 3896510/3865212
       Made changes for bug 3896510

       16-March-2005 Punit Kumar
       Convergence changes
       
     05-MAY-2015     Shaliu Chen      ER 20938455
     Modify for Batch On Hold enhancement,add validation to raise an error 
     if batch is on hold          
=============================================================================================*/
   PROCEDURE update_actual_resource_usage (
      p_org_code        IN              VARCHAR2
     ,p_batch_no        IN              VARCHAR2 := NULL
     ,p_batchstep_no    IN              NUMBER := NULL
     ,p_activity        IN              VARCHAR2 := NULL
     ,p_resource        IN              VARCHAR2 := NULL
     ,p_reason_name     IN              VARCHAR2,
      p_instance_no     IN              NUMBER
     ,p_rsrc_txn_rec    IN              gme_resource_txns%ROWTYPE
     ,x_rsrc_txn_rec    IN OUT NOCOPY   gme_resource_txns%ROWTYPE
     ,x_return_status   OUT NOCOPY      VARCHAR2)
   IS
      l_api_name   CONSTANT VARCHAR2 (30)       := 'UPDATE_ACTUAL_RSRC_USAGE';
      l_resource_txns       gme_resource_txns_gtmp%ROWTYPE;
      l_batch_header        gme_batch_header%ROWTYPE;
      l_step_resources      gme_batch_step_resources%ROWTYPE;
      l_resource_tbl        gme_common_pvt.resource_transactions_tab;
      l_line_id             NUMBER;
      l_instance_id         NUMBER;
      l_reason_id           NUMBER;
      l_inv_trans_count     NUMBER;
      l_rsrc_trans_count    NUMBER;
      l_step_status         NUMBER;
      l_return_status       VARCHAR2 (2);
      l_resources           VARCHAR2 (16);
      l_usage_uom           VARCHAR2 (4);
      --Rishi Varma 02-09-2004 B3856541
      l_trans_date          DATE;
      l_rsrc_txn_rec        gme_resource_txns_gtmp%ROWTYPE;
      l_tran_rec            gme_resource_txns_gtmp%ROWTYPE;

      CURSOR cur_fetch_resource_dtl (v_line_id NUMBER)
      IS
         SELECT resources, usage_um
           FROM gme_batch_step_resources
          WHERE batchstep_resource_id = v_line_id;

      validation_failure    EXCEPTION;
      rsrc_fetch_err        EXCEPTION;
      rsrc_update_err       EXCEPTION;
      rsrc_txn_insert_err   EXCEPTION;
      error_load_trans      EXCEPTION;
      update_rsrc_txn_err   EXCEPTION;
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      /* Initially let us assign the return status to success */
      x_return_status := fnd_api.g_ret_sts_success;

       /*siva commented following IF condition to allow insertion of flexfileds
        without validation when p_validate_flexfields is FALSE */
      --IF gme_common_pvt.g_flex_validate_prof = 1 THEN
         /*Validate Flexfields using the new procedure gme_api_validate_flex_fld_pvt.validate_rsrc_txn_flex */
         gme_validate_flex_fld_pvt.validate_rsrc_txn_flex
                                       (p_resource_txn_rec      => p_rsrc_txn_rec
                                       ,x_resource_txn_rec      => x_rsrc_txn_rec
                                       ,x_return_status         => l_return_status);

         IF l_return_status <> fnd_api.g_ret_sts_success THEN
            RAISE validation_failure;
         END IF;
      --END IF;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line (   'before calling validate_rsrc_txn_param '
                             || g_pkg_name
                             || '.'
                             || l_api_name);
      END IF;

      validate_rsrc_txn_param (p_called_from            => 1
                              ,p_batchstep_rsrc_id      => p_rsrc_txn_rec.line_id
                              ,p_org_code               => p_org_code
                              ,p_batch_no               => p_batch_no
                              ,p_batchstep_no           => p_batchstep_no
                              ,p_activity               => p_activity
                              ,p_resource               => p_resource
                              ,p_trans_date             => p_rsrc_txn_rec.trans_date
                              ,p_start_date             => p_rsrc_txn_rec.start_date
                              ,p_end_date               => p_rsrc_txn_rec.end_date
                              ,p_usage                  => p_rsrc_txn_rec.resource_usage
                              ,p_reason_name            => p_reason_name
                              ,p_reason_id              => p_rsrc_txn_rec.reason_id
                              ,p_instance_id            => p_rsrc_txn_rec.instance_id
                              ,p_instance_no            => p_instance_no
                              ,x_line_id                => l_line_id
                              ,x_step_status            => l_step_status
                              ,x_batch_header_rec       => l_batch_header
                              ,x_instance_id            => l_instance_id
                              ,x_reason_id              => l_reason_id
                              ,x_return_status          => x_return_status
                              ,x_trans_date             => l_trans_date);

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line (   'after calling validate_rsrc_txn_param '
                             || g_pkg_name
                             || '.'
                             || l_api_name);
      END IF;

      IF x_return_status <> fnd_api.g_ret_sts_success THEN
         RAISE validation_failure;
      END IF;
      
      /* Shaliu Chen     05-MAY-2015  ER 20938455
         raise an error if batch is on hold
      */
      IF gme_common_pvt.get_batch_hold_status(l_batch_header.batch_id) <> 'R' THEN
        gme_common_pvt.log_message('GME_BATCH_ONHOLD_CHECK',
                                   'ACTION_NAME',
                                   'update_actual_rsrc_usage');        
        RAISE validation_failure;        
      END IF;        

      OPEN cur_fetch_resource_dtl (l_line_id);

      FETCH cur_fetch_resource_dtl
       INTO l_resources, l_usage_uom;

      CLOSE cur_fetch_resource_dtl;

      /* Lets now load the transactions associated with the batch into the temporary tblle */
      gme_trans_engine_util.load_rsrc_trans
                                       (p_batch_row          => l_batch_header
                                       ,x_rsc_row_count      => l_rsrc_trans_count
                                       ,x_return_status      => l_return_status);

      IF l_return_status <> x_return_status THEN
         RAISE error_load_trans;
      END IF;
      IF g_debug <= gme_debug.g_log_statement THEN
         gme_debug.put_line(g_pkg_name||'.'||l_api_name||' Transactions Loaded '||l_rsrc_trans_count);
      END IF;

      l_rsrc_txn_rec.line_id := l_line_id;
      gme_resource_engine_pvt.fetch_active_resources
                                           (p_resource_rec       => l_rsrc_txn_rec
                                           ,x_resource_tbl       => l_resource_tbl
                                           ,x_return_status      => l_return_status);

      IF l_return_status <> x_return_status THEN
         RAISE fnd_api.g_exc_error;
      END IF;

      -- delete all rec in l_resource_tbl
      FOR i IN 1 .. l_resource_tbl.COUNT LOOP
         delete_resource_trans (p_tran_rec           => l_resource_tbl (i)
                               ,x_return_status      => l_return_status);

         IF l_return_status <> 'S' THEN
            RAISE update_rsrc_txn_err;
         END IF;
      END LOOP;

      -- construct record for insertion
      l_rsrc_txn_rec.doc_id := l_batch_header.batch_id;
      l_rsrc_txn_rec.doc_type := 'PROD';
      l_rsrc_txn_rec.line_type := 0;
      l_rsrc_txn_rec.organization_id := l_batch_header.organization_id;
      l_rsrc_txn_rec.orgn_code := NULL;
      l_rsrc_txn_rec.event_id := gme_common_pvt.g_transaction_header_id;
      l_rsrc_txn_rec.line_id := l_line_id;
      l_rsrc_txn_rec.resources := l_resources;
      l_rsrc_txn_rec.resource_usage := p_rsrc_txn_rec.resource_usage;
      l_rsrc_txn_rec.trans_um := l_usage_uom;
      l_rsrc_txn_rec.trans_date := p_rsrc_txn_rec.trans_date;
      l_rsrc_txn_rec.completed_ind := 1;
      l_rsrc_txn_rec.posted_ind := 0;
      --Bug#6154309 only for ASQC batches overrided_protected_ind should be yes.
      IF l_batch_header.automatic_step_calculation = 1 THEN
        l_rsrc_txn_rec.overrided_protected_ind := 'Y';
      ELSE
        l_rsrc_txn_rec.overrided_protected_ind  := 'N';
      END IF;
      l_rsrc_txn_rec.reason_id := l_reason_id;
      l_rsrc_txn_rec.start_date := p_rsrc_txn_rec.start_date;
      l_rsrc_txn_rec.end_date := p_rsrc_txn_rec.end_date;
      l_rsrc_txn_rec.action_code := 'ADD';
      l_rsrc_txn_rec.delete_mark := 0;
      l_rsrc_txn_rec.text_code := 0;

      IF p_rsrc_txn_rec.instance_id IS NOT NULL THEN
         l_rsrc_txn_rec.instance_id := p_rsrc_txn_rec.instance_id;
      ELSE
         l_rsrc_txn_rec.instance_id := l_instance_id;
      END IF;

      l_rsrc_txn_rec.sequence_dependent_ind := 0;
      --siva copying  flex-fields
     --IF gme_common_pvt.g_flex_validate_prof = 1 THEN
      l_rsrc_txn_rec.attribute_category := x_rsrc_txn_rec.attribute_category;
      l_rsrc_txn_rec.attribute1 	:= x_rsrc_txn_rec.attribute1;
      l_rsrc_txn_rec.attribute2 	:= x_rsrc_txn_rec.attribute2;
      l_rsrc_txn_rec.attribute3 	:= x_rsrc_txn_rec.attribute3;
      l_rsrc_txn_rec.attribute4 	:= x_rsrc_txn_rec.attribute4;
      l_rsrc_txn_rec.attribute5 	:= x_rsrc_txn_rec.attribute5;
      l_rsrc_txn_rec.attribute6 	:= x_rsrc_txn_rec.attribute6;
      l_rsrc_txn_rec.attribute7 	:= x_rsrc_txn_rec.attribute7;
      l_rsrc_txn_rec.attribute8 	:= x_rsrc_txn_rec.attribute8;
      l_rsrc_txn_rec.attribute9 	:= x_rsrc_txn_rec.attribute9;
      l_rsrc_txn_rec.attribute10 	:= x_rsrc_txn_rec.attribute10;
      l_rsrc_txn_rec.attribute11 	:= x_rsrc_txn_rec.attribute11;
      l_rsrc_txn_rec.attribute12 	:= x_rsrc_txn_rec.attribute12;
      l_rsrc_txn_rec.attribute13 	:= x_rsrc_txn_rec.attribute13;
      l_rsrc_txn_rec.attribute14 	:= x_rsrc_txn_rec.attribute14;
      l_rsrc_txn_rec.attribute15 	:= x_rsrc_txn_rec.attribute15;
      l_rsrc_txn_rec.attribute16 	:= x_rsrc_txn_rec.attribute16;
      l_rsrc_txn_rec.attribute17 	:= x_rsrc_txn_rec.attribute17;
      l_rsrc_txn_rec.attribute18 	:= x_rsrc_txn_rec.attribute18;
      l_rsrc_txn_rec.attribute19 	:= x_rsrc_txn_rec.attribute19;
      l_rsrc_txn_rec.attribute20 	:= x_rsrc_txn_rec.attribute20;
      l_rsrc_txn_rec.attribute21 	:= x_rsrc_txn_rec.attribute21;
      l_rsrc_txn_rec.attribute22 	:= x_rsrc_txn_rec.attribute22;
      l_rsrc_txn_rec.attribute23 	:= x_rsrc_txn_rec.attribute23;
      l_rsrc_txn_rec.attribute24 	:= x_rsrc_txn_rec.attribute24;
      l_rsrc_txn_rec.attribute25 	:= x_rsrc_txn_rec.attribute25;
      l_rsrc_txn_rec.attribute26 	:= x_rsrc_txn_rec.attribute26;
      l_rsrc_txn_rec.attribute27 	:= x_rsrc_txn_rec.attribute27;
      l_rsrc_txn_rec.attribute28 	:= x_rsrc_txn_rec.attribute28;
      l_rsrc_txn_rec.attribute29 	:= x_rsrc_txn_rec.attribute29;
      l_rsrc_txn_rec.attribute30 	:= x_rsrc_txn_rec.attribute30;
   --END IF;
      IF NOT (gme_resource_txns_gtmp_dbl.insert_row (l_rsrc_txn_rec
                                                    ,l_rsrc_txn_rec) ) THEN
         RAISE rsrc_txn_insert_err;
      END IF;

      IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
         --Rishi Varma 3896510/3865212
         /*Replaced hardcoded value '157' for batch_id with l_batch_header.batch_id*/
         gme_debug.display_resource_gtmp (NULL, NULL
                                         ,l_batch_header.batch_id);
      END IF;

      l_step_resources.batchstep_resource_id := l_rsrc_txn_rec.line_id;
      IF NOT gme_batch_step_resources_dbl.fetch_row
                                  (p_batch_step_resources      => l_step_resources
                                  ,x_batch_step_resources      => l_step_resources) THEN
         RAISE rsrc_fetch_err;
      END IF;

      l_step_resources.actual_rsrc_count := 1;
      l_step_resources.actual_rsrc_usage := l_rsrc_txn_rec.resource_usage;

      IF NOT gme_batch_step_resources_dbl.update_row
                                   (p_batch_step_resources      => l_step_resources) THEN
         RAISE rsrc_update_err;
      END IF;

      --22APR02  Pawan Kumar bug 2912743 added line_id. Assigning the value of poc_trans_id
      x_rsrc_txn_rec.poc_trans_id := l_rsrc_txn_rec.poc_trans_id;
      x_rsrc_txn_rec.doc_id := l_rsrc_txn_rec.doc_id;
      x_rsrc_txn_rec.line_id := l_rsrc_txn_rec.line_id;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;
   EXCEPTION
      WHEN validation_failure OR error_load_trans THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN update_rsrc_txn_err OR rsrc_txn_insert_err THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN rsrc_fetch_err OR rsrc_update_err THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN OTHERS THEN
         IF g_debug <= gme_debug.g_log_unexpected THEN
            gme_debug.put_line (   'When others exception in '
                                || g_pkg_name
                                || '.'
                                || l_api_name
                                || ' Error is '
                                || SQLERRM);
         END IF;

         x_return_status := fnd_api.g_ret_sts_unexp_error;
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
   END update_actual_resource_usage;

/*===========================================================================================
   Procedure
      insert_incr_actual_rsrc_txn
   Description
     This particular procedure is used to insert incrementally a completed rsrc txn rec
   Parameters
     (p_org_code,p_batch_no,step_no,activity and resource)   to uniquely identify a resource
     p_trans_date                 transaction date of resource txn
     p_start_date                 start date of resource txn
     p_end_date                   end date of resource txn
     p_usage                      resource usage of the txns and resource
     p_reason_code                reason to insert a completed rsrc txn
     p_instance_id                instance_id of the instance of rsrc txn(for WPS)
     p_instance_no                instance_no of the instance of rsrc txn(for WPS)
     x_return_status              reflects return status of the API
     02-SEP-04 Rishi Varma B3856541
      Added the new parameter to the validate_rsrc_param procedure call.

      16-March-2005  Punit Kumar
      Convergenc changes
      
     20-APR-2010    G. Muratore     Bug 9506856
        Make call to insert data to GMF layer tables since this api inserts
        directly into gme transaction tables bypassing the gtmp table.
        
     05-MAY-2015     Shaliu Chen      ER 20938455
     Modify for Batch On Hold enhancement,add validation to raise an error 
     if batch is on hold  
       
     25-MAY-2015    Shaliu Chen       BUG 21139512
     Correct the validation added for batch on hold.          
=============================================================================================*/
   PROCEDURE insert_incr_actual_rsrc_txn (
      p_org_code        IN              VARCHAR2
     /*inventory organization under which the batch was created.*/
     ,p_batch_no        IN              VARCHAR2 := NULL
     ,p_batchstep_no    IN              NUMBER := NULL
     ,p_activity        IN              VARCHAR2 := NULL
     ,p_resource        IN              VARCHAR2 := NULL
     ,p_reason_name     IN              VARCHAR2
     ,p_instance_no     IN              NUMBER
     ,p_rsrc_txn_rec    IN              gme_resource_txns%ROWTYPE
     ,x_rsrc_txn_rec    IN OUT NOCOPY   gme_resource_txns%ROWTYPE
     ,x_return_status   OUT NOCOPY      VARCHAR2)
   IS
      l_api_name        CONSTANT VARCHAR2 (30)
                                             := 'insert_incr_actual_rsrc_txn';
      l_resource_txns            gme_resource_txns%ROWTYPE;
      l_batch_header             gme_batch_header%ROWTYPE;
      l_step_resources           gme_batch_step_resources%ROWTYPE;
      l_resource_tbl             gme_common_pvt.resource_transactions_tab;
      l_step_status              NUMBER;
      l_activity_id              NUMBER;
      l_inv_trans_count          NUMBER;
      l_rsrc_trans_count         NUMBER;
      l_dummy                    NUMBER;
      l_instance_no              NUMBER;
      l_instance_id              NUMBER;
      l_reason_id                NUMBER;
      l_line_id                  NUMBER;
      l_rsrc_actual_start_date   DATE;
      l_rsrc_actual_cmplt_date   DATE;
      l_return_status            VARCHAR2 (2);
      l_resources                VARCHAR2 (16);
      l_usage_uom                VARCHAR2 (4);
      --Rishi Varma B3856541 02-09-2004
      l_trans_date               DATE;
      l_rsrc_txn_rec             gme_resource_txns%ROWTYPE;
      /*ER 19161894  Shaliu Chen 18-JUL-2014*/
      l_batch_id                 NUMBER;
      l_batchstep_id             NUMBER;      
      /*END ER 19161894*/
      /*ER 19161894  Shaliu Chen 18-JUL-2014
        Add 2 column batch_id and batchstep_id
      */
      CURSOR cur_fetch_resource_dtl (v_line_id NUMBER)
      IS
         ----SELECT resources,usage_uom
         SELECT batch_id,batchstep_id,resources, usage_um
           FROM gme_batch_step_resources
          WHERE batchstep_resource_id = v_line_id;

      validation_failure         EXCEPTION;
      rsrc_fetch_err             EXCEPTION;
      rsrc_update_err            EXCEPTION;
      rsrc_txn_insert_err        EXCEPTION;
      reduce_pend_usage_err      EXCEPTION;
      error_load_trans           EXCEPTION;

      -- Bug 9506856
      gmf_cost_failure           EXCEPTION;
      rsrc_txn_fetch_err         EXCEPTION; 
      is_osp_rsrc_err            EXCEPTION; /*ER 19161894  Shaliu Chen 18-JUL-2014*/    
      l_msg_count                NUMBER;
      l_msg_data		 VARCHAR2(2000);
      
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      /* Initially let us assign the return status to success */
      x_return_status := fnd_api.g_ret_sts_success;

      /*siva commented following IF condition to allow insertion of flexfileds
        without validation when p_validate_flexfields is FALSE */
      --IF gme_common_pvt.g_flex_validate_prof = 1 THEN
         /*Validate Flexfields using the new procedure gme_api_validate_flex_fld_pvt.validate_rsrc_txn_flex */
         gme_validate_flex_fld_pvt.validate_rsrc_txn_flex
                                       (p_resource_txn_rec      => p_rsrc_txn_rec
                                       ,x_resource_txn_rec      => x_rsrc_txn_rec
                                       ,x_return_status         => x_return_status);

         IF x_return_status <> fnd_api.g_ret_sts_success THEN
            RAISE validation_failure;
         END IF;
      --END IF;

      l_rsrc_txn_rec := x_rsrc_txn_rec;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line (   'before calling validate_rsrc_txn_param '
                             || g_pkg_name
                             || '.'
                             || l_api_name);
      END IF;

      validate_rsrc_txn_param (p_called_from            => 2
                              ,p_batchstep_rsrc_id      => p_rsrc_txn_rec.line_id
                              ,p_org_code               => p_org_code
                              ,p_batch_no               => p_batch_no
                              ,p_batchstep_no           => p_batchstep_no
                              ,p_activity               => p_activity
                              ,p_resource               => p_resource
                              ,p_trans_date             => p_rsrc_txn_rec.trans_date
                              ,p_start_date             => p_rsrc_txn_rec.start_date
                              ,p_end_date               => p_rsrc_txn_rec.end_date
                              ,p_usage                  => p_rsrc_txn_rec.resource_usage
                              ,p_reason_id              => p_rsrc_txn_rec.reason_id
                              ,p_reason_name            => p_reason_name
                              ,p_instance_id            => p_rsrc_txn_rec.instance_id
                              ,p_instance_no            => p_instance_no
                              ,x_line_id                => l_line_id
                              ,x_step_status            => l_step_status
                              ,x_batch_header_rec       => l_batch_header
                              ,x_instance_id            => l_instance_id
                              ,x_reason_id              => l_reason_id
                              ,x_return_status          => x_return_status
                              ,x_trans_date             => l_trans_date);

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line (   'after calling validate_rsrc_txn_param '
                             || g_pkg_name
                             || '.'
                             || l_api_name);
      END IF;

      IF x_return_status <> fnd_api.g_ret_sts_success THEN
         RAISE validation_failure;
      END IF;
      /*ER 19161894  Shaliu Chen 18-JUL-2014
        fetch the 2 new added columns:batch_id,batchstep_id
      */
      OPEN cur_fetch_resource_dtl (l_line_id);

      FETCH cur_fetch_resource_dtl
       INTO l_batch_id,l_batchstep_id,l_resources, l_usage_uom;

      CLOSE cur_fetch_resource_dtl;
      
      /*                                    
      BEGIN ER 19161894  Shaliu Chen 18-JUL-2014               
      Added for OPM Step Level OSP Project  
      For OSP batch,it's not allowed to create resource txn
      via public API.
      */         
      IF gme_osp.check_release_version THEN
        IF gme_osp.is_osp_batch(p_batch_id     => l_batch_id,
                                p_batchstep_id => l_batchstep_id) THEN
          gme_common_pvt.log_message(p_product_code => 'GME'
                                    ,p_message_code => 'GME_RSRC_TXN_NOT_ALLOWED');                              
          RAISE is_osp_rsrc_err;                      
        END IF; 
      END IF;       
      /*END ER 19161894*/
        

      -- construct record for insertion
      l_rsrc_txn_rec.doc_id := l_batch_header.batch_id;
      /*  hard coding doc_type to PROD as rsrc txn can only exist for batchesi not FPO
          later if we decide to have this functionality for FPOs we would have to change code to
          get batch_type from l_batch_header variable */
      l_rsrc_txn_rec.doc_type := 'PROD';
      l_rsrc_txn_rec.line_type := 0;
      l_rsrc_txn_rec.organization_id := l_batch_header.organization_id;
      l_rsrc_txn_rec.line_id := l_line_id;
      l_rsrc_txn_rec.event_id := gme_common_pvt.g_transaction_header_id;
      l_rsrc_txn_rec.resources := l_resources;
      l_rsrc_txn_rec.resource_usage := p_rsrc_txn_rec.resource_usage;
      l_rsrc_txn_rec.trans_qty_um := l_usage_uom;
      l_rsrc_txn_rec.trans_date := l_trans_date;
      l_rsrc_txn_rec.completed_ind := 1;
      l_rsrc_txn_rec.posted_ind := 0;
      --Bug#6154309 only for ASQC batches overrided_protected_ind should be yes.
      IF l_batch_header.automatic_step_calculation = 1 THEN
        l_rsrc_txn_rec.overrided_protected_ind := 'Y';
      ELSE
        l_rsrc_txn_rec.overrided_protected_ind  := 'N';
      END IF;
      l_rsrc_txn_rec.reason_id := l_reason_id;
      l_rsrc_txn_rec.start_date := p_rsrc_txn_rec.start_date;
      l_rsrc_txn_rec.end_date := p_rsrc_txn_rec.end_date;
      l_rsrc_txn_rec.delete_mark := 0;
      l_rsrc_txn_rec.text_code := 0;

      IF p_rsrc_txn_rec.instance_id IS NOT NULL THEN
         l_rsrc_txn_rec.instance_id := p_rsrc_txn_rec.instance_id;
      ELSE
         l_rsrc_txn_rec.instance_id := l_instance_id;
      END IF;

      l_rsrc_txn_rec.sequence_dependent_ind := 0;
      
      /* Shaliu Chen     25-MAY-2015  Bug 21139512
         1.resource txn can not be inserted if batch is on hold with STOP type.
         2.only backdated resource txn can be inserted if batch is on hold with PAUSE type.
      */
      IF  gme_common_pvt.validate_resource_txn_onhold(p_batch_id   => l_rsrc_txn_rec.doc_id,
                                                      p_trans_date => l_rsrc_txn_rec.trans_date,
                                                      p_start_date => l_rsrc_txn_rec.start_date,
                                                      p_end_date   => l_rsrc_txn_rec.end_date) <> 'R' THEN

        gme_common_pvt.log_message('GME_ONHOLD_TRANS_DATE');
        RAISE validation_failure;
      END IF;         

      IF NOT (gme_resource_txns_dbl.insert_row (l_rsrc_txn_rec
                                               ,x_rsrc_txn_rec) ) THEN
         RAISE rsrc_txn_insert_err;
      END IF;

      l_rsrc_txn_rec := x_rsrc_txn_rec;
      
   
      
      -- Bug 9506856  - Fetch the record directly from the DB to get all column values.
      IF NOT (gme_resource_txns_dbl.fetch_row (l_rsrc_txn_rec, l_rsrc_txn_rec) ) THEN
         RAISE rsrc_txn_fetch_err;
      END IF;

      -- Bug 9506856  - Make call to GMF for actual costing
      GMF_LAYERS.Create_Resource_Layers
       (p_api_version   =>    1.0,
        p_init_msg_list =>    FND_API.G_FALSE,
        p_rsrc_rec      =>    l_rsrc_txn_rec,
        p_doc_qty       =>    l_rsrc_txn_rec.resource_usage,
        p_doc_um        =>    l_rsrc_txn_rec.trans_qty_um,
        x_return_status =>    l_return_status,
        x_msg_count     =>    l_msg_count,
        x_msg_data      =>    l_msg_data);

      IF l_return_status <> FND_API.G_RET_STS_SUCCESS
      THEN
         RAISE gmf_cost_failure;
      END IF;
      
      -- update resource actual count and usage
      -- l_step_resources.batchstep_resource_id := l_resource_txns.line_id;
      l_step_resources.batchstep_resource_id := l_rsrc_txn_rec.line_id;

      IF NOT gme_batch_step_resources_dbl.fetch_row
                                  (p_batch_step_resources      => l_step_resources
                                  ,x_batch_step_resources      => l_step_resources) THEN
         RAISE rsrc_fetch_err;
      END IF;

      l_step_resources.actual_rsrc_usage :=
           NVL (l_step_resources.actual_rsrc_usage, 0)
         + l_rsrc_txn_rec.resource_usage;

      IF l_step_status = 2 THEN
         /* Lets now load the transactions associated with the batch into the temporary tblle */
         gme_trans_engine_util.load_rsrc_trans
                                      (p_batch_row          => l_batch_header
                                      ,x_rsc_row_count      => l_rsrc_trans_count
                                      ,x_return_status      => l_return_status);

         IF l_return_status <> x_return_status THEN
            RAISE error_load_trans;
         END IF;

         gme_update_step_qty_pvt.reduce_pending_usage
                              (p_batch_step_resources_rec      => l_step_resources
                              ,x_return_status                 => x_return_status);

         IF x_return_status <> 'S' THEN
            RAISE reduce_pend_usage_err;
         END IF;
      END IF;

      IF NOT gme_batch_step_resources_dbl.update_row
                                   (p_batch_step_resources      => l_step_resources) THEN
         RAISE rsrc_update_err;
      END IF;

       --22APR02  Pawan Kumar bug 2912743 added poc_trans_id
      -- Assigning the value of poc_trans_id
      ---x_poc_trans_id := l_resource_txns.poc_trans_id;
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;
   EXCEPTION
      WHEN gmf_cost_failure OR rsrc_txn_fetch_err THEN
         -- Bug 9506856
         x_return_status := FND_API.G_RET_STS_ERROR;   
      WHEN validation_failure OR error_load_trans OR rsrc_txn_insert_err THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN rsrc_fetch_err OR rsrc_update_err OR reduce_pend_usage_err THEN
         x_return_status := fnd_api.g_ret_sts_error;
      /*ER 19161894  Shaliu Chen 18-JUL-2014*/    
      WHEN is_osp_rsrc_err THEN
        x_return_status := fnd_api.g_ret_sts_error;           
      WHEN OTHERS THEN
         IF g_debug <= gme_debug.g_log_unexpected THEN
            gme_debug.put_line (   'When others exception in '
                                || g_pkg_name
                                || '.'
                                || l_api_name
                                || ' Error is '
                                || SQLERRM);
         END IF;

         x_return_status := fnd_api.g_ret_sts_unexp_error;
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
   END insert_incr_actual_rsrc_txn;

/*==== =======================================================================================
   Procedure
      insert_timed_actual_rsrc_txn
   Description
     This particular procedure is used to insert completed rsrc txn rec and calculates the usage             from provided txn dates
   Parameters
     (p_org_code,p_batch_no,step_no,activity and resource)   to uniquely identify a resource
     p_trans_date                 transaction date of resource txn
     p_start_date                 start date of resource txn
     p_end_date                   end date of resource txn
     p_reason_code                reason to insert a completed rsrc txn
     p_instance_id                instance_id of the instance of rsrc txn(for WPS)
     p_instance_no                instance_no of the instance of rsrc txn(for WPS)
     x_return_status              reflects return status of the API
   History
     09JULY03 BUG#3041705  V. Ajay Kumar
     Port bug 2965882 to 11.5.10K.
     Modified code such that the difference in start date and end date
     is calculated in hours.
     02-SEP-04 Rishi Varma B3856541
      Added the new parameter to the validate_rsrc_param procedure call.

      15-March-2005 Punit Kumar
      Convergence changes
      
     16-APR-2010    G. Muratore     Bug 9506856
        Make call to insert data to GMF layer tables since this api inserts
        directly into gme transaction tables bypassing the gtmp table.
     18-JUL-2014    Shaliu Chen     ER 19161894
        Modify create_batch to invoke requisition creation program if batch include OSP step   
        
     05-MAY-2015     Shaliu Chen      ER 20938455
     Modify for Batch On Hold enhancement,add validation to raise an error 
     if batch is on hold  
       
     25-MAY-2015    Shaliu Chen       BUG 21139512
     Correct the validation added for batch on hold.               
=============================================================================================*/
   PROCEDURE insert_timed_actual_rsrc_txn (
      p_org_code        IN              VARCHAR2
     ,p_batch_no        IN              VARCHAR2 := NULL
     ,p_batchstep_no    IN              NUMBER := NULL
     ,p_activity        IN              VARCHAR2 := NULL
     ,p_resource        IN              VARCHAR2 := NULL
     ,p_reason_name     IN              VARCHAR2,
      p_instance_no     IN              NUMBER
     ,p_rsrc_txn_rec    IN              gme_resource_txns%ROWTYPE
     ,x_rsrc_txn_rec    IN OUT NOCOPY   gme_resource_txns%ROWTYPE
     ,x_return_status   OUT NOCOPY      VARCHAR2)
   IS
      l_api_name      CONSTANT VARCHAR2 (30)
                                            := 'insert_timed_actual_rsrc_txn';
      l_resource_txns          gme_resource_txns%ROWTYPE;
      l_batch_header           gme_batch_header%ROWTYPE;
      l_step_resources         gme_batch_step_resources%ROWTYPE;
      l_resource_tbl           gme_common_pvt.resource_transactions_tab;
      l_line_id                NUMBER;
      l_inv_trans_count        NUMBER;
      l_rsrc_trans_count       NUMBER;
      l_step_status            NUMBER;
      l_return_status          VARCHAR2 (2);
      l_resources              VARCHAR2 (16);
      l_usage_uom              VARCHAR2 (4);
      l_txn_usage              NUMBER;
      l_instance_id            NUMBER;
      l_reason_id              NUMBER;
      l_usage_time             NUMBER;
      l_hour_um                sy_uoms_mst.um_code%TYPE;
      l_rsrc_txn_rec           gme_resource_txns%ROWTYPE;
      /*ER 19161894  Shaliu Chen 18-JUL-2014*/
      l_batch_id                 NUMBER;
      l_batchstep_id             NUMBER; 
      /*END ER 19161894*/       


      --Rishi Varma B3856541 02-09-2004
      l_trans_date             DATE;
      
      /*ER 19161894  Shaliu Chen 18-JUL-2014
        Add 2 column batch_id and batchstep_id
       */
      CURSOR cur_fetch_resource_dtl (v_line_id NUMBER)
      IS
         --- SELECT resources,usage_uom
         SELECT batch_id,batchstep_id,resources, usage_um
           FROM gme_batch_step_resources
          WHERE batchstep_resource_id = v_line_id;

      validation_failure       EXCEPTION;
      uom_conversion_err       EXCEPTION;
      missing_profile_option   EXCEPTION;
      rsrc_fetch_err           EXCEPTION;
      rsrc_update_err          EXCEPTION;
      rsrc_txn_insert_err      EXCEPTION;
      reduce_pend_usage_err    EXCEPTION;
      error_load_trans         EXCEPTION;

      -- Bug 9506856
      gmf_cost_failure         EXCEPTION;
      rsrc_txn_fetch_err       EXCEPTION; 
      is_osp_rsrc_err            EXCEPTION;  /*ER 19161894  Shaliu Chen 18-JUL-2014*/     
      l_msg_count              NUMBER;
      l_msg_data	       VARCHAR2(2000);
      
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      /* Initially let us assign the return status to success */
      x_return_status := fnd_api.g_ret_sts_success;

       /*siva commented following IF condition to allow insertion of flexfileds
        without validation when p_validate_flexfields is FALSE */
      --IF gme_common_pvt.g_flex_validate_prof = 1 THEN
         /*Validate Flexfields using the new procedure gme_api_validate_flex_fld_pvt.validate_rsrc_txn_flex */
         gme_validate_flex_fld_pvt.validate_rsrc_txn_flex
                                       (p_resource_txn_rec      => p_rsrc_txn_rec
                                       ,x_resource_txn_rec      => x_rsrc_txn_rec
                                       ,x_return_status         => l_return_status);

         IF l_return_status <> fnd_api.g_ret_sts_success THEN
            RAISE validation_failure;
         END IF;
        --END IF;
      l_rsrc_txn_rec := x_rsrc_txn_rec;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line (   'before calling validate_rsrc_txn_param '
                             || g_pkg_name
                             || '.'
                             || l_api_name);
      END IF;

      validate_rsrc_txn_param (p_called_from            => 3
                              ,p_batchstep_rsrc_id      => p_rsrc_txn_rec.line_id
                              ,p_org_code               => p_org_code
                              ,p_batch_no               => p_batch_no
                              ,p_batchstep_no           => p_batchstep_no
                              ,p_activity               => p_activity
                              ,p_resource               => p_resource
                              ,p_trans_date             => p_rsrc_txn_rec.trans_date
                              ,p_start_date             => p_rsrc_txn_rec.start_date
                              ,p_end_date               => p_rsrc_txn_rec.end_date
                              ,p_usage                  => NULL
                              ,p_reason_id              => p_rsrc_txn_rec.reason_id
                              ,p_reason_name            => p_reason_name
                              ,p_instance_id            => p_rsrc_txn_rec.instance_id
                              ,p_instance_no            => p_instance_no
                              ,x_line_id                => l_line_id
                              ,x_step_status            => l_step_status
                              ,x_batch_header_rec       => l_batch_header
                              ,x_instance_id            => l_instance_id
                              ,x_reason_id              => l_reason_id
                              ,x_return_status          => x_return_status
                              ,
                               --Rishi Varma B3856541 02-09-2004
                               x_trans_date             => l_trans_date);

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line (   'after calling validate_rsrc_txn_param '
                             || g_pkg_name
                             || '.'
                             || l_api_name);
      END IF;

      IF x_return_status <> fnd_api.g_ret_sts_success THEN
         RAISE validation_failure;
      END IF;

      /*  V. Ajay Kumar  Bug 3041705. Multiplied the difference of
         dates by 24 to convert it to hours */
      l_usage_time :=
                    (p_rsrc_txn_rec.end_date - p_rsrc_txn_rec.start_date) * 24;
      l_hour_um :=
         fnd_profile.value_specific (NAME         => 'BOM:HOUR_UOM_CODE'
                                    ,user_id      => gme_common_pvt.g_user_ident);

      IF (l_hour_um IS NULL) THEN
         gme_common_pvt.log_message ('GME_API_UNABLE_TO_GET_CONSTANT'
                                    ,'CONSTANT_NAME'
                                    ,'BOM:HOUR_UOM_CODE');
         RAISE missing_profile_option;
      END IF;
      
      /*ER 19161894  Shaliu Chen 18-JUL-2014
        fetch the 2 new added columns:batch_id,batchstep_id
      */
      OPEN cur_fetch_resource_dtl (l_line_id);

      FETCH cur_fetch_resource_dtl
       INTO l_batch_id,l_batchstep_id,l_resources, l_usage_uom;

      CLOSE cur_fetch_resource_dtl;
      /*                                    
      BEGIN ER 19161894  Shaliu Chen 18-JUL-2014               
      Added for OPM Step Level OSP Project  
      For OSP batch,it's not allowed to create resource txn
      via public API.      
      */                                    
      
      IF gme_osp.check_release_version THEN
        IF gme_osp.is_osp_batch(p_batch_id     => l_batch_id,
                                p_batchstep_id => l_batchstep_id) THEN
          gme_common_pvt.log_message(p_product_code => 'GME'
                                    ,p_message_code => 'GME_RSRC_TXN_NOT_ALLOWED');                              
          RAISE is_osp_rsrc_err;                      
        END IF;
      END IF;   
      /*END ER 19161894*/  
               

      /* siva from_name and to_name made null*/
      IF l_hour_um <> l_rsrc_txn_rec.trans_qty_um THEN
         l_txn_usage :=
            inv_convert.inv_um_convert (item_id            => 0
                                       ,PRECISION          => 5
                                       ,from_quantity      => l_usage_time
                                       ,from_unit          => l_hour_um
                                       ,to_unit            => l_rsrc_txn_rec.trans_qty_um
                                       ,from_name          => NULL
                                       ,to_name            => NULL);

         IF (l_txn_usage = -99999) THEN
            gme_common_pvt.log_message ('GME_RSRC_USG_NT_CNV_SYUOM'
                                       ,'SY_UOM'
                                       ,l_hour_um
                                       ,'RSRC_USG_UOM'
                                       ,l_usage_uom);
            RAISE uom_conversion_err;
         END IF;
      ELSE
         l_txn_usage := l_usage_time;
      END IF;

      -- construct record for insertion
      ----l_resource_txns.doc_id  := l_batch_header.batch_id;
      -- hard coding doc_type to PROD as rsrc txn can only exist for batchesi not FPO
      -- later if we decide to have this functionality for FPOs we would have to change code to
      -- get batch_type from l_batch_header variable
      l_rsrc_txn_rec.doc_id := l_batch_header.batch_id;
      l_rsrc_txn_rec.doc_type := 'PROD';
      l_rsrc_txn_rec.line_type := 0;
      l_rsrc_txn_rec.organization_id := l_batch_header.organization_id;
      l_rsrc_txn_rec.orgn_code := NULL;
      l_rsrc_txn_rec.line_id := l_line_id;
      l_rsrc_txn_rec.event_id := gme_common_pvt.g_transaction_header_id;
      l_rsrc_txn_rec.resources := l_resources;
      l_rsrc_txn_rec.resource_usage := l_txn_usage;
      l_rsrc_txn_rec.trans_qty_um := l_usage_uom;
      l_rsrc_txn_rec.trans_date := l_trans_date;
      l_rsrc_txn_rec.completed_ind := 1;
      l_rsrc_txn_rec.posted_ind := 0;
      --Bug#6154309 only for ASQC batches overrided_protected_ind should be yes.
      IF l_batch_header.automatic_step_calculation = 1 THEN
        l_rsrc_txn_rec.overrided_protected_ind  := 'Y';
      ELSE
        l_rsrc_txn_rec.overrided_protected_ind  := 'N';
      END IF;
      l_rsrc_txn_rec.reason_id := l_reason_id;
      l_rsrc_txn_rec.start_date := p_rsrc_txn_rec.start_date;
      l_rsrc_txn_rec.end_date := p_rsrc_txn_rec.end_date;
      l_rsrc_txn_rec.delete_mark := 0;
      l_rsrc_txn_rec.text_code := 0;

      IF p_rsrc_txn_rec.instance_id IS NOT NULL THEN
         l_rsrc_txn_rec.instance_id := p_rsrc_txn_rec.instance_id;
      ELSE
         l_rsrc_txn_rec.instance_id := l_instance_id;
      END IF;

      l_rsrc_txn_rec.sequence_dependent_ind := 0;
      
      /* Shaliu Chen     25-MAY-2015  Bug 21139512
         1.resource txn can not be inserted if batch is on hold with STOP type.
         2.only backdated resource txn can be inserted if batch is on hold with PAUSE type.
      */
      IF  gme_common_pvt.validate_resource_txn_onhold(p_batch_id   => l_rsrc_txn_rec.doc_id,
                                                      p_trans_date => l_rsrc_txn_rec.trans_date,
                                                      p_start_date => l_rsrc_txn_rec.start_date,
                                                      p_end_date   => l_rsrc_txn_rec.end_date) <> 'R' THEN

        gme_common_pvt.log_message('GME_ONHOLD_TRANS_DATE');
        RAISE validation_failure;
      END IF;       

      IF NOT (gme_resource_txns_dbl.insert_row (l_rsrc_txn_rec
                                               ,x_rsrc_txn_rec) ) THEN
         RAISE rsrc_txn_insert_err;
      END IF;

      l_rsrc_txn_rec := x_rsrc_txn_rec;
      
      -- Bug 9506856  - Fetch the record directly from the DB to get all column values.
      IF NOT (gme_resource_txns_dbl.fetch_row (l_rsrc_txn_rec, l_rsrc_txn_rec) ) THEN
         RAISE rsrc_txn_fetch_err;
      END IF;
      
      -- Bug 9506856  - Make call to GMF for actual costing
      GMF_LAYERS.Create_Resource_Layers
       (p_api_version   =>    1.0,
        p_init_msg_list =>    FND_API.G_FALSE,
        p_rsrc_rec      =>    l_rsrc_txn_rec,
        p_doc_qty       =>    l_rsrc_txn_rec.resource_usage,
        p_doc_um        =>    l_rsrc_txn_rec.trans_qty_um,
        x_return_status =>    l_return_status,
        x_msg_count     =>    l_msg_count,
        x_msg_data      =>    l_msg_data);

      IF l_return_status <> FND_API.G_RET_STS_SUCCESS
      THEN
         RAISE gmf_cost_failure;
      END IF;      
      
      -- update resource actual count and usage
      --l_step_resources.batchstep_resource_id := l_resource_txns.line_id;
      l_step_resources.batchstep_resource_id := l_rsrc_txn_rec.line_id;

      IF NOT gme_batch_step_resources_dbl.fetch_row
                                  (p_batch_step_resources      => l_step_resources
                                  ,x_batch_step_resources      => l_step_resources) THEN
         RAISE rsrc_fetch_err;
      END IF;

      l_step_resources.actual_rsrc_usage :=
                      NVL (l_step_resources.actual_rsrc_usage, 0)
                      + l_txn_usage;

      IF l_step_status = 2 THEN
         /* Lets now load the transactions associated with the batch into the temporary tblle */
         gme_trans_engine_util.load_rsrc_trans
                                      (p_batch_row          => l_batch_header
                                      ,x_rsc_row_count      => l_rsrc_trans_count
                                      ,x_return_status      => l_return_status);

         IF l_return_status <> x_return_status THEN
            RAISE error_load_trans;
         END IF;

         gme_update_step_qty_pvt.reduce_pending_usage
                              (p_batch_step_resources_rec      => l_step_resources
                              ,x_return_status                 => x_return_status);

         IF x_return_status <> 'S' THEN
            RAISE reduce_pend_usage_err;
         END IF;
      END IF;

      IF NOT gme_batch_step_resources_dbl.update_row
                                   (p_batch_step_resources      => l_step_resources) THEN
         RAISE rsrc_update_err;
      END IF;

      --22APR02  Pawan Kumar bug 2912743 added poc_trans_id
      -- Assigning the value of poc_trans_id

      ---x_poc_trans_id := l_resource_txns.poc_trans_id;
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;
   EXCEPTION
      WHEN gmf_cost_failure OR rsrc_txn_fetch_err THEN
         -- Bug 9506856
         x_return_status := FND_API.G_RET_STS_ERROR;   
      WHEN validation_failure OR missing_profile_option OR error_load_trans OR rsrc_txn_insert_err THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN rsrc_fetch_err OR rsrc_update_err OR reduce_pend_usage_err THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN uom_conversion_err THEN
         x_return_status := fnd_api.g_ret_sts_error;
      /*ER 19161894  Shaliu Chen 18-JUL-2014*/         
      WHEN is_osp_rsrc_err THEN
         x_return_status := fnd_api.g_ret_sts_error;         
      WHEN OTHERS THEN
         IF g_debug <= gme_debug.g_log_unexpected THEN
            gme_debug.put_line (   'When others exception in '
                                || g_pkg_name
                                || '.'
                                || l_api_name
                                || ' Error is '
                                || SQLERRM);
         END IF;

         x_return_status := fnd_api.g_ret_sts_unexp_error;
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
   END insert_timed_actual_rsrc_txn;

/*==== =======================================================================================
   Procedure
      start_cmplt_actual_rsrc_txn
   Description
     This particular procedure is used to insert a start completed rsrc txn rec
   Parameters
     (p_org_code,p_batch_no,step_no,activity and resource)   to uniquely identify a resource
     p_trans_date                 transaction date of resource txn
     p_start_date                 start date of resource txn
     p_reason_code                reason to insert a completed rsrc txn
     p_instance_id                instance_id of the instance of rsrc txn(for WPS)
     p_instance_no                instance_no of the instance of rsrc txn(for WPS)
     x_return_status              reflects return status of the API
     02-SEP-04 Rishi Varma B3856541
      Added the new parameter to the validate_rsrc_param procedure call.
      
     05-MAY-2015     Shaliu Chen      ER 20938455
     Modify for Batch On Hold enhancement,add validation to raise an error 
     if batch is on hold          
=============================================================================================*/
   PROCEDURE start_cmplt_actual_rsrc_txn (
      /* inventory organization under which the batch was created */
      p_org_code        IN              VARCHAR2
     ,p_batch_no        IN              VARCHAR2 := NULL
     ,p_batchstep_no    IN              NUMBER := NULL
     ,p_activity        IN              VARCHAR2 := NULL
     ,p_resource        IN              VARCHAR2 := NULL
     ,p_reason_name     IN              VARCHAR2
     ,p_instance_no     IN              NUMBER
     ,p_rsrc_txn_rec    IN              gme_resource_txns%ROWTYPE
     ,x_rsrc_txn_rec    IN OUT NOCOPY   gme_resource_txns%ROWTYPE
     ,x_return_status   OUT NOCOPY      VARCHAR2)
   IS
      l_api_name   CONSTANT VARCHAR2 (30)    := 'start_cmplt_actual_rsrc_txn';
      l_resource_txns       gme_resource_txns%ROWTYPE;
      l_batch_header        gme_batch_header%ROWTYPE;
      l_line_id             NUMBER;
      l_step_status         NUMBER;
      l_return_status       VARCHAR2 (2);
      l_resources           VARCHAR2 (16);
      l_usage_uom           VARCHAR2 (4);
      l_instance_id         NUMBER;
      l_reason_id           NUMBER;
      /*ER 19161894  Shaliu Chen 18-JUL-2014*/
      l_batch_id                 NUMBER;
      l_batchstep_id             NUMBER;      
      /*END ER 19161894*/
      l_rsrc_txn_rec        gme_resource_txns%ROWTYPE;
      l_count               NUMBER;  --BUG 24422344
      
      /*ER 19161894  Shaliu Chen 18-JUL-2014
        Add 2 column batch_id and batchstep_id
       */
      CURSOR cur_fetch_resource_dtl (v_line_id NUMBER)
      IS
         ---SELECT resources,usage_uom
         SELECT batch_id,batchstep_id,resources, usage_um
           FROM gme_batch_step_resources
          WHERE batchstep_resource_id = v_line_id;

      --Rishi Varma B3856541 02-09-2004 start
      l_trans_date          DATE;
      validation_failure    EXCEPTION;
      rsrc_txn_insert_err   EXCEPTION;
      is_osp_rsrc_err            EXCEPTION;  /*ER 19161894  Shaliu Chen 18-JUL-2014*/
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      /* Initially let us assign the return status to success */
      x_return_status := fnd_api.g_ret_sts_success;

      /*siva commented following IF condition to allow insertion of flexfileds
        without validation when p_validate_flexfields is FALSE */
     -- IF gme_common_pvt.g_flex_validate_prof = 1 THEN
         --Validate Flexfields using the new procedure
         gme_validate_flex_fld_pvt.validate_rsrc_txn_flex
                                       (p_resource_txn_rec      => p_rsrc_txn_rec
                                       ,x_resource_txn_rec      => x_rsrc_txn_rec
                                       ,x_return_status         => x_return_status);

         IF x_return_status <> fnd_api.g_ret_sts_success THEN
            RAISE validation_failure;
         END IF;
      --END IF;
      /* siva added the following line */
      l_rsrc_txn_rec := x_rsrc_txn_rec;


      /* Initially let us assign the return status to success */
      l_return_status := fnd_api.g_ret_sts_success;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line (   'before calling validate_rsrc_txn_param '
                             || g_pkg_name
                             || '.'
                             || l_api_name);
      END IF;

      validate_rsrc_txn_param (p_called_from            => 4
                              ,p_batchstep_rsrc_id      => p_rsrc_txn_rec.line_id
                              ,p_org_code               => p_org_code
                              ,p_batch_no               => p_batch_no
                              ,p_batchstep_no           => p_batchstep_no
                              ,p_activity               => p_activity
                              ,p_resource               => p_resource
                              ,p_trans_date             => p_rsrc_txn_rec.trans_date
                              ,p_start_date             => p_rsrc_txn_rec.start_date
                              ,p_end_date               => p_rsrc_txn_rec.end_date
                              ,p_usage                  => 0
                              ,p_reason_id              => p_rsrc_txn_rec.reason_id
                              ,p_reason_name            => p_reason_name
                              ,p_instance_id            => p_rsrc_txn_rec.instance_id
                              ,p_instance_no            => p_instance_no
                              ,x_line_id                => l_line_id
                              ,x_step_status            => l_step_status
                              ,x_batch_header_rec       => l_batch_header
                              ,x_instance_id            => l_instance_id
                              ,x_reason_id              => l_reason_id
                              ,x_return_status          => l_return_status
                              ,x_trans_date             => l_trans_date);

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line (   'after calling validate_rsrc_txn_param '
                             || g_pkg_name
                             || '.'
                             || l_api_name);
      END IF;

      IF l_return_status <> fnd_api.g_ret_sts_success THEN
         RAISE validation_failure;
      END IF;
      /*ER 19161894  Shaliu Chen 18-JUL-2014
        fetch the new added 2 columns:batch_id,batchstep_id
       */
      OPEN cur_fetch_resource_dtl (l_line_id);

      FETCH cur_fetch_resource_dtl
       INTO l_batch_id,l_batchstep_id,l_resources, l_usage_uom;

      CLOSE cur_fetch_resource_dtl;
      /*                                    
      BEGIN ER 19161894  Shaliu Chen 18-JUL-2014               
      Added for OPM Step Level OSP Project  
      For OSP batch,it's not allowed to create resource txn
      via public API.      
      */            
      IF gme_osp.check_release_version THEN                                                                      
        IF gme_osp.is_osp_batch(p_batch_id     => l_batch_id,                                                    
                                p_batchstep_id => l_batchstep_id) THEN                                           
          gme_common_pvt.log_message(p_product_code => 'GME'                                                     
                                    ,p_message_code => 'GME_RSRC_TXN_NOT_ALLOWED');                              
          RAISE is_osp_rsrc_err;                                                                                                
        END IF;                                                                                                  
      END IF; 
      /*END ER 19161894*/          
      
      -- construct record for insertion
      l_rsrc_txn_rec.doc_id := l_batch_header.batch_id;
      l_rsrc_txn_rec.doc_type := 'PROD';
      l_rsrc_txn_rec.line_type := 0;
      l_rsrc_txn_rec.organization_id := l_batch_header.organization_id;
      l_rsrc_txn_rec.orgn_code := NULL;
      l_rsrc_txn_rec.line_id := l_line_id;
      l_rsrc_txn_rec.event_id := gme_common_pvt.g_transaction_header_id;
      l_rsrc_txn_rec.resources := l_resources;
      l_rsrc_txn_rec.resource_usage := 0;
      l_rsrc_txn_rec.trans_qty_um := l_usage_uom;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line (g_pkg_name||'.'||l_api_name||' l_trans_date ' || l_trans_date);
         gme_debug.put_line (g_pkg_name||'.'||l_api_name||' p_rsrc_txn_rec.trans_date ' || p_rsrc_txn_rec.trans_date);
      END IF;

      IF l_trans_date IS NOT NULL THEN
         l_rsrc_txn_rec.trans_date := l_trans_date;
      ELSE
         l_rsrc_txn_rec.trans_date := p_rsrc_txn_rec.trans_date;
      END IF;

      l_rsrc_txn_rec.completed_ind := 1;
      l_rsrc_txn_rec.posted_ind := 0;
      --Bug#6154309 only for ASQC batches overrided_protected_ind should be yes.
      IF l_batch_header.automatic_step_calculation = 1 THEN
        l_rsrc_txn_rec.overrided_protected_ind  := 'Y';
      ELSE
        l_rsrc_txn_rec.overrided_protected_ind  := 'N';
      END IF;
      l_rsrc_txn_rec.reason_id := l_reason_id;
      l_rsrc_txn_rec.start_date := p_rsrc_txn_rec.start_date;
      l_rsrc_txn_rec.end_date := p_rsrc_txn_rec.end_date;
      l_rsrc_txn_rec.delete_mark := 0;
      l_rsrc_txn_rec.text_code := 0;
      IF p_rsrc_txn_rec.instance_id IS NOT NULL THEN
         l_rsrc_txn_rec.instance_id := p_rsrc_txn_rec.instance_id;
      ELSE
         l_rsrc_txn_rec.instance_id := l_instance_id;
      END IF;
      
      --BEGIN BUG 24422344   06-DEC-2016 Shaliu Chen
      --Add validation for resource instance ER
      --There is no two start resource transactions can have same instance number;
      IF l_rsrc_txn_rec.instance_id IS NOT NULL THEN 
        SELECT count(1)
          INTO l_count
          FROM gme_resource_txns grt
         WHERE grt.doc_id = l_rsrc_txn_rec.doc_id
           AND grt.line_id = l_rsrc_txn_rec.line_id
           AND NVL(grt.instance_id,-1) = l_rsrc_txn_rec.instance_id
           AND grt.completed_ind = 1
           AND grt.resource_usage = 0;
           
        IF l_count > 0 THEN
          gme_common_pvt.log_message('GME_DUPL_RES_INST');
          RAISE validation_failure;          
        END IF;        
      END IF;
      --END BUG 24422344

      l_rsrc_txn_rec.sequence_dependent_ind := 0;
      
      /* Shaliu Chen     25-MAY-2015  Bug 21139512
         1.resource txn can not be inserted if batch is on hold with STOP type.
         2.only backdated resource txn can be inserted if batch is on hold with PAUSE type.
      */
      IF  gme_common_pvt.validate_resource_txn_onhold(p_batch_id   => l_rsrc_txn_rec.doc_id,
                                                      p_trans_date => l_rsrc_txn_rec.trans_date,
                                                      p_start_date => l_rsrc_txn_rec.start_date,
                                                      p_end_date   => NVL(l_rsrc_txn_rec.end_date,l_rsrc_txn_rec.start_date)) <> 'R' THEN

        gme_common_pvt.log_message('GME_ONHOLD_TRANS_DATE');
        RAISE validation_failure;
      END IF;       

      IF NOT (gme_resource_txns_dbl.insert_row (l_rsrc_txn_rec
                                               ,x_rsrc_txn_rec) ) THEN
         RAISE rsrc_txn_insert_err;
      END IF;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name|| ' with return status '||x_return_status);
      END IF;
   EXCEPTION
      WHEN validation_failure OR rsrc_txn_insert_err THEN
         x_return_status := fnd_api.g_ret_sts_error;
      /*ER 19161894  Shaliu Chen 18-JUL-2014*/   
      WHEN is_osp_rsrc_err THEN
        x_return_status := fnd_api.g_ret_sts_error;           
      WHEN OTHERS THEN
         IF g_debug <= gme_debug.g_log_unexpected THEN
            gme_debug.put_line (   'When others exception in '
                                || g_pkg_name
                                || '.'
                                || l_api_name
                                || ' Error is '
                                || SQLERRM);
         END IF;

         x_return_status := fnd_api.g_ret_sts_unexp_error;
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
   END start_cmplt_actual_rsrc_txn;

/*==== =======================================================================================
   Procedure
      end_cmplt_actual_rsrc_txn
   Description
     This particular procedure is used to end a started completed rsrc txn rec and calculates
     the usage from rsrc txn dates
   Parameters
     p_poc_trans_id               id to uniquely identify a resource txn
     p_trans_date                 transaction date of resource txn
     p_end_date                   end date of resource txn
     p_reason_name                reason to insert a completed rsrc txn
     p_instance_id                instance_id of the instance of rsrc txn(for WPS)
     p_instance_no                instance_no of the instance of rsrc txn(for WPS)
     x_return_status              reflects return status of the API
   History
     09JULY03 BUG#3041705  V. Ajay Kumar
     Port bug 2965882 to 11.5.10K.
     Modified code such that the difference in start date and end date
     is calculated in hours.
     RajaSekhar Reddy 21-MAY-2004 BUG#3610141
     Added code  to assign plan_rsrc_count to actual_rsrc_count if actual_rsrc_count is NULL.
     02-SEP-04 Rishi Varma B3856541
     Added the new parameter to the validate_rsrc_param procedure call.

     14th March 2005 Punit kumar
     Convergence changes
     
     05-MAY-2015     Shaliu Chen      ER 20938455
     Modify for Batch On Hold enhancement,add validation to raise an error 
     if batch is on hold      
=============================================================================================*/
   PROCEDURE end_cmplt_actual_rsrc_txn (
      p_rsrc_txn_rec    IN              gme_resource_txns%ROWTYPE
     ,p_reason_name     IN              VARCHAR2
     ,p_instance_no     IN              NUMBER
     ,x_rsrc_txn_rec    IN OUT NOCOPY   gme_resource_txns%ROWTYPE
     ,x_return_status   OUT NOCOPY      VARCHAR2)
   IS
      l_api_name      CONSTANT VARCHAR2 (30)   := 'end_cmplt_actual_rsrc_txn';
      l_resource_txns          gme_resource_txns%ROWTYPE;
      l_batch_header           gme_batch_header%ROWTYPE;
      l_step_resources         gme_batch_step_resources%ROWTYPE;
      l_resource_tbl           gme_common_pvt.resource_transactions_tab;
      --Begin Bug# 3479669
      l_tran_rec               gme_resource_txns%ROWTYPE;
      --End Bug# 3479669
      l_step_status            NUMBER;
      l_return_status          VARCHAR2 (2);
      l_txn_usage              NUMBER;
      l_instance_id            NUMBER;
      l_reason_id              NUMBER;
      l_dummy                  NUMBER;
      l_usage_time             NUMBER;
      l_inv_trans_count        NUMBER;
      l_line_id                NUMBER;
      l_rsrc_trans_count       NUMBER;
      l_hour_um                sy_uoms_mst.um_code%TYPE;
      l_trans_date             DATE;
      l_rsrc_txn_rec           gme_resource_txns%ROWTYPE;

      --siva added following variables
      x_trans_date             DATE;
      x_reason_id              NUMBER;
      x_instance_id            NUMBER;
      /*ER 19161894  Shaliu Chen 18-JUL-2014*/
      l_batch_id                 NUMBER;
      l_batchstep_id             NUMBER;        
      /*END ER 19161894*/
      CURSOR cur_validate_poc_trans_id (v_poc_trans_id NUMBER)
      IS
         SELECT 1
           FROM gme_resource_txns
          WHERE poc_trans_id = v_poc_trans_id;
      /*ER 19161894  Shaliu Chen 18-JUL-2014
        Add cursor to get batch_id and batchstep_id
      */    
      CURSOR cur_fetch_resource_dtl (v_line_id NUMBER)
      IS
         SELECT batch_id,batchstep_id
           FROM gme_batch_step_resources
          WHERE batchstep_resource_id = v_line_id;            

      invalid_poc_trans_id     EXCEPTION;
      validation_failure       EXCEPTION;
      uom_conversion_err       EXCEPTION;
      missing_profile_option   EXCEPTION;
      rsrc_txn_fetch_err       EXCEPTION;
      rsrc_fetch_err           EXCEPTION;
      rsrc_update_err          EXCEPTION;
      rsrc_txn_upd_err         EXCEPTION;
      reduce_pend_usage_err    EXCEPTION;
      error_load_trans         EXCEPTION;
      invalid_txn_for_end      EXCEPTION;
      expected_error           EXCEPTION;
      rsrc_txn_ins_err         EXCEPTION;
      is_osp_rsrc_err          EXCEPTION;   /*ER 19161894  Shaliu Chen 18-JUL-2014*/
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      /* Initially let us assign the return status to success */
      x_return_status := fnd_api.g_ret_sts_success;

      -- validate poc_trans_id right here as validate_rsrc_txn_param doesnt validate it

      OPEN cur_validate_poc_trans_id (p_rsrc_txn_rec.poc_trans_id);

      FETCH cur_validate_poc_trans_id
       INTO l_dummy;

      IF cur_validate_poc_trans_id%NOTFOUND THEN
         gme_common_pvt.log_message ('GME_INV_POC_TRANS_ID'
                                    ,'POC_TRANS_ID'
                                    ,p_rsrc_txn_rec.poc_trans_id);
         RAISE invalid_poc_trans_id;
      END IF;

      CLOSE cur_validate_poc_trans_id;

       /*siva commented following IF condition to allow insertion of flexfileds
        without validation when p_validate_flexfields is FALSE */
      --IF gme_common_pvt.g_flex_validate_prof = 1 THEN
         /* Validate Flexfields using the new procedure  */
         gme_validate_flex_fld_pvt.validate_rsrc_txn_flex
                                       (p_resource_txn_rec      => p_rsrc_txn_rec
                                       ,x_resource_txn_rec      => x_rsrc_txn_rec
                                       ,x_return_status         => x_return_status);

         IF x_return_status <> fnd_api.g_ret_sts_success THEN
            RAISE validation_failure;
         END IF;
      --END IF;

      l_rsrc_txn_rec.poc_trans_id := p_rsrc_txn_rec.poc_trans_id;

      IF NOT (gme_resource_txns_dbl.fetch_row (l_rsrc_txn_rec, l_rsrc_txn_rec) ) THEN
         RAISE rsrc_txn_fetch_err;
      END IF;
      
      
      /*                                    
      BEGIN ER 19161894  Shaliu Chen 18-JUL-2014               
      Added for OPM Step Level OSP Project  
      For OSP batch,it's not allowed to create resource txn
      via public API.      
      */              
      OPEN cur_fetch_resource_dtl (l_rsrc_txn_rec.line_id);

      FETCH cur_fetch_resource_dtl
       INTO l_batch_id,l_batchstep_id;

      CLOSE cur_fetch_resource_dtl;
      
                                                                                                                  
      IF gme_osp.check_release_version THEN                                                                      
        IF gme_osp.is_osp_batch(p_batch_id     => l_batch_id,                                                    
                                p_batchstep_id => l_batchstep_id) THEN                                           
          gme_common_pvt.log_message(p_product_code => 'GME'                                                     
                                    ,p_message_code => 'GME_RSRC_TXN_NOT_ALLOWED');                              
          RAISE is_osp_rsrc_err;                                                                                                
        END IF;                                                                                                  
      END IF;         
      /*END ER 19161894*/
                    

      IF (    (l_rsrc_txn_rec.resource_usage <> 0)
          OR (l_rsrc_txn_rec.start_date <> l_rsrc_txn_rec.end_date)
          OR (l_rsrc_txn_rec.completed_ind <> 1)
          OR (l_rsrc_txn_rec.delete_mark <> 0) ) THEN
         gme_common_pvt.log_message ('GME_INVALID_TXN_FOR_END');
         RAISE invalid_txn_for_end;
      END IF;

      /*  V. Ajay Kumar  Bug 3041705. Multiplied the difference of
         dates by 24 to convert it to hours */
      l_usage_time :=
                    (p_rsrc_txn_rec.end_date - l_rsrc_txn_rec.start_date) * 24;

      IF l_usage_time < 0 THEN
         gme_common_pvt.log_message ('PM_BADENDDATE');
         RAISE expected_error;
      END IF;

      l_hour_um :=
         fnd_profile.value_specific (NAME         => 'BOM:HOUR_UOM_CODE'
                                    ,user_id      => gme_common_pvt.g_user_ident);

      IF (l_hour_um IS NULL) THEN
         gme_common_pvt.log_message ('GME_API_UNABLE_TO_GET_CONSTANT'
                                    ,'CONSTANT_NAME'
                                    ,'BOM:HOUR_UOM_CODE');
         RAISE missing_profile_option;
      END IF;

      /* siva from_name and to_name made NULL */
      IF l_hour_um <> l_rsrc_txn_rec.trans_qty_um THEN
         l_txn_usage :=
              inv_convert.inv_um_convert (item_id            => 0
                                         ,PRECISION          => 5
                                         ,from_quantity      => l_usage_time
                                         ,from_unit          => l_hour_um
                                         ,to_unit            => l_rsrc_txn_rec.trans_qty_um
                                         ,from_name          => NULL
                                         ,to_name            => NULL);

         IF (l_txn_usage = -99999) THEN
            gme_common_pvt.log_message ('GME_RSRC_USG_NT_CNV_SYUOM'
                                       ,'SY_UOM'
                                       ,l_hour_um
                                       ,'RSRC_USG_UOM'
                                       ,l_rsrc_txn_rec.trans_qty_um);
            RAISE uom_conversion_err;
         END IF;
      ELSE
         l_txn_usage := l_usage_time;
      END IF;

      -- following procedure is being called to validate trans_date, end_date, reason_code
      -- instance_id and instance_no
      -- since we are not passing poc_trans_id to validate procedure so we cannot figure out
      -- batch_id there so using one of the params that is not used by current procedure to pass
      -- batch_id to validations procedure to fetch batch rec.
      -- using p_batchstep_rsrc_id to pass batch_id
      -- B2498487 passing value of overrided_protected_ind value to validations procedure
      -- so that we dont have to fetch it there and can directly work on it
      -- using p_resource  to pass overrided_protected_ind value

      IF p_rsrc_txn_rec.trans_date IS NULL THEN
         l_trans_date := l_rsrc_txn_rec.trans_date;
      ELSE
         l_trans_date := p_rsrc_txn_rec.trans_date;
      END IF;

     /* siva added following code begin */
     IF p_rsrc_txn_rec.reason_id IS NULL THEN
       l_reason_id := l_rsrc_txn_rec.reason_id;
     ELSE
       l_reason_id := p_rsrc_txn_rec.reason_id;
     END IF;

     IF p_rsrc_txn_rec.instance_id IS NULL THEN
      l_instance_id := l_rsrc_txn_rec.instance_id;
     ELSE
      l_instance_id := p_rsrc_txn_rec.instance_id;
     END IF;
     /* siva end */

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line (   'before calling validate_rsrc_txn_param '
                             || g_pkg_name
                             || '.'
                             || l_api_name);
      END IF;

      validate_rsrc_txn_param (p_called_from            => 5
                              ,p_batchstep_rsrc_id      => l_rsrc_txn_rec.line_id
                              ,p_org_code               => NULL
                              ,p_batch_no               => NULL
                              ,p_batchstep_no           => NULL
                              ,p_activity               => NULL
                              ,p_resource               => l_rsrc_txn_rec.resources
                             /* siva passing l_trans_date instead of l_rsrc_txn_rec.trans_date */
                              ,p_trans_date             => l_trans_date
                              ,p_start_date             => l_rsrc_txn_rec.start_date
                              ,p_end_date               => l_rsrc_txn_rec.end_date
                              ,p_usage                  => l_txn_usage
                              ,p_reason_id              => l_reason_id
                              ,p_reason_name            => p_reason_name
                              ,p_instance_id            => l_instance_id
                              ,p_instance_no            => p_instance_no
                              ,x_line_id                => l_line_id
                              ,x_step_status            => l_step_status
                              ,x_batch_header_rec       => l_batch_header
                              ,x_instance_id            => x_instance_id
                              ,x_reason_id              => x_reason_id
                              ,x_return_status          => x_return_status
                              ,x_trans_date             => x_trans_date);

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line (   'after calling validate_rsrc_txn_param '
                             || g_pkg_name
                             || '.'
                             || l_api_name);
      END IF;

      IF x_return_status <> fnd_api.g_ret_sts_success THEN
         RAISE validation_failure;
      END IF;

      -- construct record for updation
      l_rsrc_txn_rec.resource_usage := l_txn_usage;
      /* siva changed to x_trans_date, x_reason_id,x_instance_id */
      l_rsrc_txn_rec.trans_date := x_trans_date;
      l_rsrc_txn_rec.reason_id := x_reason_id;
      l_rsrc_txn_rec.end_date := p_rsrc_txn_rec.end_date;
      l_rsrc_txn_rec.instance_id := x_instance_id;

      --siva copying flex field values
     --IF gme_common_pvt.g_flex_validate_prof = 1 THEN
      l_rsrc_txn_rec.attribute_category := x_rsrc_txn_rec.attribute_category;
      l_rsrc_txn_rec.attribute1 	:= x_rsrc_txn_rec.attribute1;
      l_rsrc_txn_rec.attribute2 	:= x_rsrc_txn_rec.attribute2;
      l_rsrc_txn_rec.attribute3 	:= x_rsrc_txn_rec.attribute3;
      l_rsrc_txn_rec.attribute4 	:= x_rsrc_txn_rec.attribute4;
      l_rsrc_txn_rec.attribute5 	:= x_rsrc_txn_rec.attribute5;
      l_rsrc_txn_rec.attribute6 	:= x_rsrc_txn_rec.attribute6;
      l_rsrc_txn_rec.attribute7 	:= x_rsrc_txn_rec.attribute7;
      l_rsrc_txn_rec.attribute8 	:= x_rsrc_txn_rec.attribute8;
      l_rsrc_txn_rec.attribute9 	:= x_rsrc_txn_rec.attribute9;
      l_rsrc_txn_rec.attribute10 	:= x_rsrc_txn_rec.attribute10;
      l_rsrc_txn_rec.attribute11 	:= x_rsrc_txn_rec.attribute11;
      l_rsrc_txn_rec.attribute12 	:= x_rsrc_txn_rec.attribute12;
      l_rsrc_txn_rec.attribute13 	:= x_rsrc_txn_rec.attribute13;
      l_rsrc_txn_rec.attribute14 	:= x_rsrc_txn_rec.attribute14;
      l_rsrc_txn_rec.attribute15 	:= x_rsrc_txn_rec.attribute15;
      l_rsrc_txn_rec.attribute16 	:= x_rsrc_txn_rec.attribute16;
      l_rsrc_txn_rec.attribute17 	:= x_rsrc_txn_rec.attribute17;
      l_rsrc_txn_rec.attribute18 	:= x_rsrc_txn_rec.attribute18;
      l_rsrc_txn_rec.attribute19 	:= x_rsrc_txn_rec.attribute19;
      l_rsrc_txn_rec.attribute20 	:= x_rsrc_txn_rec.attribute20;
      l_rsrc_txn_rec.attribute21 	:= x_rsrc_txn_rec.attribute21;
      l_rsrc_txn_rec.attribute22 	:= x_rsrc_txn_rec.attribute22;
      l_rsrc_txn_rec.attribute23 	:= x_rsrc_txn_rec.attribute23;
      l_rsrc_txn_rec.attribute24 	:= x_rsrc_txn_rec.attribute24;
      l_rsrc_txn_rec.attribute25 	:= x_rsrc_txn_rec.attribute25;
      l_rsrc_txn_rec.attribute26 	:= x_rsrc_txn_rec.attribute26;
      l_rsrc_txn_rec.attribute27 	:= x_rsrc_txn_rec.attribute27;
      l_rsrc_txn_rec.attribute28 	:= x_rsrc_txn_rec.attribute28;
      l_rsrc_txn_rec.attribute29 	:= x_rsrc_txn_rec.attribute29;
      l_rsrc_txn_rec.attribute30 	:= x_rsrc_txn_rec.attribute30;
   -- END IF;

      --Begin Bug#3479669
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;
      
      /* Shaliu Chen     25-MAY-2015  Bug 21139512
         1.resource txn can not be inserted if batch is on hold with STOP type.
         2.only backdated resource txn can be inserted if batch is on hold with PAUSE type.
      */
      IF gme_common_pvt.validate_resource_txn_onhold(p_batch_id   => l_rsrc_txn_rec.doc_id,
                                                     p_trans_date => l_rsrc_txn_rec.trans_date,
                                                     p_start_date => l_rsrc_txn_rec.start_date,
                                                     p_end_date   => l_rsrc_txn_rec.end_date) <> 'R' THEN

        gme_common_pvt.log_message('GME_ONHOLD_TRANS_DATE');
        RAISE validation_failure;
      END IF;       

      IF l_rsrc_txn_rec.posted_ind = 1 THEN
         l_rsrc_txn_rec.posted_ind := 0;

         IF NOT (gme_resource_txns_dbl.insert_row (l_rsrc_txn_rec, l_tran_rec) ) THEN
            RAISE rsrc_txn_ins_err;
         END IF;
         x_rsrc_txn_rec  := l_tran_rec ;
      ELSE
         --End Bug#3479669

         ---IF NOT (GME_RESOURCE_TXNS_DBL.update_row(l_resource_txns)) THEN
         IF NOT (gme_resource_txns_dbl.update_row (l_rsrc_txn_rec) ) THEN
            RAISE rsrc_txn_upd_err;
         END IF;

         x_rsrc_txn_rec  := l_rsrc_txn_rec ;

         --Begin Bug#3479669
         IF g_debug <= gme_debug.g_log_procedure THEN
            gme_debug.put_line (   'Entering api '
                                || g_pkg_name
                                || '.'
                                || l_api_name);
         END IF;
      END IF;

      ----x_poc_trans_id := l_tran_rec.poc_trans_id;
      --End Bug#3479669

      -- update resource actual count and usage
      ---l_step_resources.batchstep_resource_id := l_resource_txns.line_id;
      l_step_resources.batchstep_resource_id := l_rsrc_txn_rec.line_id;

      IF NOT gme_batch_step_resources_dbl.fetch_row
                                  (p_batch_step_resources      => l_step_resources
                                  ,x_batch_step_resources      => l_step_resources) THEN
         RAISE rsrc_fetch_err;
      END IF;

      l_step_resources.actual_rsrc_usage :=
                      NVL (l_step_resources.actual_rsrc_usage, 0)
                      + l_txn_usage;

      --BEGIN BUG#3610141 RajaSekhar
      IF l_step_resources.actual_rsrc_count IS NULL THEN
         l_step_resources.actual_rsrc_count :=
                                             l_step_resources.plan_rsrc_count;
      END IF;

      --END BUG#3610141
      IF l_step_status = 2 THEN
         /* Lets now load the transactions associated with the batch into the temporary tblle */
         gme_trans_engine_util.load_rsrc_trans
                                      (p_batch_row          => l_batch_header
                                      ,x_rsc_row_count      => l_rsrc_trans_count
                                      ,x_return_status      => l_return_status);

         IF l_return_status <> x_return_status THEN
            RAISE error_load_trans;
         END IF;

         gme_update_step_qty_pvt.reduce_pending_usage
                              (p_batch_step_resources_rec      => l_step_resources
                              ,x_return_status                 => x_return_status);

         IF x_return_status <> 'S' THEN
            RAISE reduce_pend_usage_err;
         END IF;
      END IF;

      IF NOT gme_batch_step_resources_dbl.update_row
                                   (p_batch_step_resources      => l_step_resources) THEN
         RAISE rsrc_update_err;
      END IF;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;
   EXCEPTION
      WHEN expected_error OR missing_profile_option THEN
         x_return_status := fnd_api.g_ret_sts_error;
      --Begin Bug#3479669
      --Added rsrc_txn_ins_err exception
      WHEN error_load_trans OR invalid_poc_trans_id OR rsrc_txn_upd_err OR rsrc_txn_ins_err THEN
         --End Bug#3479669
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN validation_failure THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN uom_conversion_err OR invalid_txn_for_end THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN rsrc_fetch_err OR rsrc_txn_fetch_err OR rsrc_update_err OR reduce_pend_usage_err THEN
         x_return_status := fnd_api.g_ret_sts_error;
      /*ER 19161894  Shaliu Chen 18-JUL-2014*/   
      WHEN is_osp_rsrc_err THEN
         x_return_status := fnd_api.g_ret_sts_error;         
      WHEN OTHERS THEN
         IF g_debug <= gme_debug.g_log_unexpected THEN
            gme_debug.put_line (   'When others exception in '
                                || g_pkg_name
                                || '.'
                                || l_api_name
                                || ' Error is '
                                || SQLERRM);
         END IF;

         x_return_status := fnd_api.g_ret_sts_unexp_error;
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
   END end_cmplt_actual_rsrc_txn;
END gme_resource_engine_pvt;
/
--show errors;
COMMIT ;
EXIT;
