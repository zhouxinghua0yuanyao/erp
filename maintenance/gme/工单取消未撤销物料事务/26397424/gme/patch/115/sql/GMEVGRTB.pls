REM dbdrv: sql ~PROD ~PATH ~FILE none none none package &phase=plb \
REM dbdrv: checkfile(120.2.12010000.4=120.2.12020000.2)(120.2.12000000.2=120.2.12010000.2)(115.15=120.1):~PROD:~PATH:~FILE
SET VERIFY OFF
WHENEVER SQLERROR EXIT FAILURE ROLLBACK;

CREATE OR REPLACE PACKAGE BODY gme_resource_txns_dbl AS
/* $Header: GMEVGRTB.pls 120.2.12020000.2 2014/07/22 05:39:37 shalchen ship $ */

   /* Global Variables */
   g_table_name   VARCHAR2 (80) DEFAULT 'GME_RESOURCE_TXNS';

/*============================================================================
 |                         Copyright (c) 2001, 2014 Oracle Corporation
 |                                 TVP, Reading
 |                                  All rights reserved
 =============================================================================
 |   FILENAME
 |      GMEVGRTB.pls
 |
 |   DESCRIPTION
 |
 |
 |
 |
 |   NOTES
 |
 |   HISTORY
 |   12-MAR-01 Thomas Daniel   Created
 |
 |      - insert_row
 |      - fetch_row
 |      - update_row
 |      - lock_row
 |   10-AUG-04  Rishi Varma  B3818266/3759970
 |              Added the reverse_id field in the insert,fetch and
 |              update routines.
 |  Shaliu Chen     18-JUL-2014  ER 19161894                                                 
 |    Modify create_batch to invoke requisition creation program if batch include OSP step  
 |
 |
 =============================================================================
*/

   /* Api start of comments
 +============================================================================
 |   FUNCTION NAME
 |      insert_row
 |
 |   TYPE
 |      Private
 |   USAGE
 |      Insert_Row will insert a row in gme_resource_txns
 |
 |
 |   DESCRIPTION
 |      Insert_Row will insert a row in gme_resource_txns
 |
 |
 |
 |   PARAMETERS
 |     p_resource_txns IN            gme_resource_txns%ROWTYPE
 |     x_resource_txns IN OUT NOCOPY gme_resource_txns%ROWTYPE
 |
 |   RETURNS
 |      BOOLEAN
 |   HISTORY
 |   12-MAR-01 Thomas Daniel   Created
 |   10-AUG-04  Rishi Varma  B3818266/3759970
 |              Added the reverse_id field.
 |
 |
 |
 +=============================================================================
 Api end of comments
*/
   FUNCTION insert_row (
      p_resource_txns   IN              gme_resource_txns%ROWTYPE
     ,x_resource_txns   IN OUT NOCOPY   gme_resource_txns%ROWTYPE)
      RETURN BOOLEAN
   IS
   BEGIN
      x_resource_txns := p_resource_txns;

      INSERT INTO gme_resource_txns
                  (poc_trans_id
                  ,organization_id
                  ,doc_type, doc_id
                  ,line_type, line_id
                  ,resources
                  ,resource_usage
                  ,trans_qty_um, trans_date
                  ,completed_ind, event_id
                  ,posted_ind
                  ,overrided_protected_ind
                  ,reason_code,reason_id, start_date
                  ,end_date, creation_date
                  ,last_update_date, created_by
                  ,last_updated_by, last_update_login
                  ,delete_mark, text_code
                  ,instance_id
                  ,sequence_dependent_ind
                  ,attribute1, attribute2
                  ,attribute3, attribute4
                  ,attribute5, attribute6
                  ,attribute7, attribute8
                  ,attribute9, attribute10
                  ,attribute11, attribute12
                  ,attribute13, attribute14
                  ,attribute15, attribute16
                  ,attribute17, attribute18
                  ,attribute19, attribute20
                  ,attribute21, attribute22
                  ,attribute23, attribute24
                  ,attribute25, attribute26
                  ,attribute27, attribute28
                  ,attribute29, attribute30
                  ,attribute_category
                  ,program_id
                  ,program_application_id
                  ,request_id
                  ,program_update_date
                  --10-AUG-04  Rishi Varma  B3818266/3759970
      ,            reverse_id
                  /*                                    
                  BEGIN ER 19161894                    
                  Shaliu Chen 18-JUL-2014               
                  Added for OPM Step Level OSP Project  
                  */            
                  ,cost_source
                  ,po_header_id
                  ,po_line_id
                  ,actual_resource_rate
                  ,currency_code
                  ,currency_conversion_date
                  ,currency_conversion_type
                  ,currency_conversion_rate
                  ,rcv_transaction_id
                  /*END ER 19161894*/)
           VALUES (gem5_poc_trans_id_s.NEXTVAL
                  ,x_resource_txns.organization_id
                  ,x_resource_txns.doc_type, x_resource_txns.doc_id
                  ,x_resource_txns.line_type, x_resource_txns.line_id
                  ,x_resource_txns.resources
                  ,x_resource_txns.resource_usage
                  ,x_resource_txns.trans_qty_um, x_resource_txns.trans_date
                  ,x_resource_txns.completed_ind, x_resource_txns.event_id
                  ,x_resource_txns.posted_ind
                  ,x_resource_txns.overrided_protected_ind
                  ,x_resource_txns.reason_code, x_resource_txns.reason_id, x_resource_txns.start_date
                  ,x_resource_txns.end_date, gme_common_pvt.g_timestamp
                  ,gme_common_pvt.g_timestamp, gme_common_pvt.g_user_ident
                  ,gme_common_pvt.g_user_ident, gme_common_pvt.g_login_id
                  ,x_resource_txns.delete_mark, x_resource_txns.text_code
                  ,x_resource_txns.instance_id
                  ,x_resource_txns.sequence_dependent_ind
                  ,x_resource_txns.attribute1, x_resource_txns.attribute2
                  ,x_resource_txns.attribute3, x_resource_txns.attribute4
                  ,x_resource_txns.attribute5, x_resource_txns.attribute6
                  ,x_resource_txns.attribute7, x_resource_txns.attribute8
                  ,x_resource_txns.attribute9, x_resource_txns.attribute10
                  ,x_resource_txns.attribute11, x_resource_txns.attribute12
                  ,x_resource_txns.attribute13, x_resource_txns.attribute14
                  ,x_resource_txns.attribute15, x_resource_txns.attribute16
                  ,x_resource_txns.attribute17, x_resource_txns.attribute18
                  ,x_resource_txns.attribute19, x_resource_txns.attribute20
                  ,x_resource_txns.attribute21, x_resource_txns.attribute22
                  ,x_resource_txns.attribute23, x_resource_txns.attribute24
                  ,x_resource_txns.attribute25, x_resource_txns.attribute26
                  ,x_resource_txns.attribute27, x_resource_txns.attribute28
                  ,x_resource_txns.attribute29, x_resource_txns.attribute30
                  ,x_resource_txns.attribute_category
                  ,x_resource_txns.program_id
                  ,x_resource_txns.program_application_id
                  ,x_resource_txns.request_id
                  ,x_resource_txns.program_update_date
                  --10-AUG-04  Rishi Varma  B3818266/3759970
      ,            x_resource_txns.reverse_id
                  /*                                    
                  BEGIN ER 19161894                    
                  Shaliu Chen 18-JUL-2014               
                  Added for OPM Step Level OSP Project  
                  */           
                  ,x_resource_txns.cost_source
                  ,x_resource_txns.po_header_id
                  ,x_resource_txns.po_line_id
                  ,x_resource_txns.actual_resource_rate
                  ,x_resource_txns.currency_code
                  ,x_resource_txns.currency_conversion_date
                  ,x_resource_txns.currency_conversion_type
                  ,x_resource_txns.currency_conversion_rate
                  ,x_resource_txns.rcv_transaction_id 
                  /*END ER 19161894*/)
        RETURNING poc_trans_id
             INTO x_resource_txns.poc_trans_id;

      IF SQL%FOUND THEN
         RETURN TRUE;
      ELSE
         RETURN FALSE;
      END IF;
   EXCEPTION
      WHEN OTHERS THEN
         gme_common_pvt.log_message ('GME_UNEXPECTED_ERROR', 'ERROR'
                                    ,SQLERRM);
         RETURN FALSE;
   END insert_row;

/* Api start of comments
 +============================================================================
 |   FUNCTION NAME
 |      fetch_row
 |
 |   TYPE
 |      Private
 |   USAGE
 |      Fetch_Row will fetch a row in gme_resource_txns
 |
 |
 |   DESCRIPTION
 |      Fetch_Row will fetch a row in gme_resource_txns
 |
 |
 |
 |   PARAMETERS
 |     p_resource_txns IN            gme_resource_txns%ROWTYPE
 |     x_resource_txns IN OUT NOCOPY gme_resource_txns%ROWTYPE
 |
 |   RETURNS
 |      BOOLEAN
 |   HISTORY
 |   12-MAR-01 Thomas Daniel   Created
 |   10-AUG-04  Rishi Varma  B3818266/3759970
 |              Added the reverse_id field.
 |
 |
 |
 +=============================================================================
 Api end of comments
*/
   FUNCTION fetch_row (
      p_resource_txns   IN              gme_resource_txns%ROWTYPE
     ,x_resource_txns   IN OUT NOCOPY   gme_resource_txns%ROWTYPE)
      RETURN BOOLEAN
   IS
   BEGIN
      IF p_resource_txns.poc_trans_id IS NOT NULL THEN
         SELECT poc_trans_id
               ,organization_id, doc_type
               ,doc_id, line_type
               ,line_id, resources
               ,resource_usage
               ,trans_qty_um, trans_date
               ,completed_ind, event_id
               ,posted_ind
               ,overrided_protected_ind
               ,reason_code,reason_id, start_date
               ,end_date, creation_date
               ,last_update_date
               ,created_by, last_updated_by
               ,last_update_login
               ,delete_mark, text_code
               ,instance_id
               ,sequence_dependent_ind
               ,attribute1, attribute2
               ,attribute3, attribute4
               ,attribute5, attribute6
               ,attribute7, attribute8
               ,attribute9, attribute10
               ,attribute11, attribute12
               ,attribute13, attribute14
               ,attribute15, attribute16
               ,attribute17, attribute18
               ,attribute19, attribute20
               ,attribute21, attribute22
               ,attribute23, attribute24
               ,attribute25, attribute26
               ,attribute27, attribute28
               ,attribute29, attribute30
               ,attribute_category
               ,program_id
               ,program_application_id
               ,request_id
               ,program_update_date
               --10-AUG-04  Rishi Varma  B3818266/3759970
         ,      reverse_id
           INTO x_resource_txns.poc_trans_id
               ,x_resource_txns.organization_id, x_resource_txns.doc_type
               ,x_resource_txns.doc_id, x_resource_txns.line_type
               ,x_resource_txns.line_id, x_resource_txns.resources
               ,x_resource_txns.resource_usage
               ,x_resource_txns.trans_qty_um, x_resource_txns.trans_date
               ,x_resource_txns.completed_ind, x_resource_txns.event_id
               ,x_resource_txns.posted_ind
               ,x_resource_txns.overrided_protected_ind
               ,x_resource_txns.reason_code, x_resource_txns.reason_id, x_resource_txns.start_date
               ,x_resource_txns.end_date, x_resource_txns.creation_date
               ,x_resource_txns.last_update_date
               ,x_resource_txns.created_by, x_resource_txns.last_updated_by
               ,x_resource_txns.last_update_login
               ,x_resource_txns.delete_mark, x_resource_txns.text_code
               ,x_resource_txns.instance_id
               ,x_resource_txns.sequence_dependent_ind
               ,x_resource_txns.attribute1, x_resource_txns.attribute2
               ,x_resource_txns.attribute3, x_resource_txns.attribute4
               ,x_resource_txns.attribute5, x_resource_txns.attribute6
               ,x_resource_txns.attribute7, x_resource_txns.attribute8
               ,x_resource_txns.attribute9, x_resource_txns.attribute10
               ,x_resource_txns.attribute11, x_resource_txns.attribute12
               ,x_resource_txns.attribute13, x_resource_txns.attribute14
               ,x_resource_txns.attribute15, x_resource_txns.attribute16
               ,x_resource_txns.attribute17, x_resource_txns.attribute18
               ,x_resource_txns.attribute19, x_resource_txns.attribute20
               ,x_resource_txns.attribute21, x_resource_txns.attribute22
               ,x_resource_txns.attribute23, x_resource_txns.attribute24
               ,x_resource_txns.attribute25, x_resource_txns.attribute26
               ,x_resource_txns.attribute27, x_resource_txns.attribute28
               ,x_resource_txns.attribute29, x_resource_txns.attribute30
               ,x_resource_txns.attribute_category
               ,x_resource_txns.program_id
               ,x_resource_txns.program_application_id
               ,x_resource_txns.request_id
               ,x_resource_txns.program_update_date
               --10-AUG-04  Rishi Varma  B3818266/3759970
         ,      x_resource_txns.reverse_id
           FROM gme_resource_txns
          WHERE poc_trans_id = p_resource_txns.poc_trans_id;
      ELSE
         gme_common_pvt.log_message ('GME_NO_KEYS'
                                    ,'TABLE_NAME'
                                    ,g_table_name);
         RETURN FALSE;
      END IF;

      RETURN TRUE;
   EXCEPTION
      WHEN NO_DATA_FOUND THEN
         gme_common_pvt.log_message ('GME_NO_DATA_FOUND'
                                    ,'TABLE_NAME'
                                    ,g_table_name);
         RETURN FALSE;
      WHEN OTHERS THEN
         gme_common_pvt.log_message ('GME_UNEXPECTED_ERROR', 'ERROR'
                                    ,SQLERRM);
         RETURN FALSE;
   END fetch_row;

/* Api start of comments
 +============================================================================
 |   FUNCTION NAME
 |      delete_row
 |
 |   TYPE
 |      Private
 |   USAGE
 |      Delete_Row will delete a row in gme_resource_txns
 |
 |
 |   DESCRIPTION
 |      Delete_Row will delete a row in gme_resource_txns
 |
 |
 |
 |   PARAMETERS
 |     p_resource_txns IN  gme_resource_txns%ROWTYPE
 |
 |   RETURNS
 |      BOOLEAN
 |   HISTORY
 |   12-MAR-01 Thomas Daniel   Created
 |   26-AUG-02  Bharati Satpute  Bug 2404126
 |   Added error message 'GME_RECORD_CHANGED'                                 |
 |
 +=============================================================================
 Api end of comments
*/
   FUNCTION delete_row (p_resource_txns IN gme_resource_txns%ROWTYPE)
      RETURN BOOLEAN
   IS
      l_dummy                NUMBER    := 0;
      locked_by_other_user   EXCEPTION;
      PRAGMA EXCEPTION_INIT (locked_by_other_user, -54);
   BEGIN
      IF p_resource_txns.poc_trans_id IS NOT NULL THEN
         SELECT     1
               INTO l_dummy
               FROM gme_resource_txns
              WHERE poc_trans_id = p_resource_txns.poc_trans_id
         FOR UPDATE NOWAIT;

         DELETE FROM gme_resource_txns
               WHERE poc_trans_id = p_resource_txns.poc_trans_id;
      ELSE
         gme_common_pvt.log_message ('GME_NO_KEYS'
                                    ,'TABLE_NAME'
                                    ,g_table_name);
         RETURN FALSE;
      END IF;

      IF SQL%FOUND THEN
         RETURN TRUE;
      ELSE
         IF l_dummy = 0 THEN
            gme_common_pvt.log_message ('GME_NO_DATA_FOUND'
                                       ,'TABLE_NAME'
                                       ,g_table_name);
         ELSE
            gme_common_pvt.log_message ('GME_RECORD_CHANGED'
                                       ,'TABLE_NAME'
                                       ,g_table_name);
         END IF;

         RETURN FALSE;
      END IF;
   EXCEPTION
      WHEN NO_DATA_FOUND THEN
         IF l_dummy = 0 THEN
            gme_common_pvt.log_message ('GME_NO_DATA_FOUND'
                                       ,'TABLE_NAME'
                                       ,g_table_name);
         ELSE
            gme_common_pvt.log_message ('GME_RECORD_CHANGED'
                                       ,'TABLE_NAME'
                                       ,g_table_name);
         END IF;

         RETURN FALSE;
      WHEN locked_by_other_user THEN
         gme_common_pvt.log_message ('GME_RECORD_LOCKED'
                                    ,'TABLE_NAME'
                                    ,g_table_name
                                    ,'RECORD'
                                    ,'Resource'
                                    ,'KEY'
                                    ,p_resource_txns.resources);
         RETURN FALSE;
      WHEN OTHERS THEN
         gme_common_pvt.log_message ('GME_UNEXPECTED_ERROR', 'ERROR'
                                    ,SQLERRM);
         RETURN FALSE;
   END delete_row;

/* Api start of comments
 +============================================================================
 |   FUNCTION NAME
 |      update_row
 |
 |   TYPE
 |      Private
 |   USAGE
 |      Update_Row will update a row in gme_resource_txns
 |
 |
 |   DESCRIPTION
 |      Update_Row will update a row in gme_resource_txns
 |
 |
 |
 |   PARAMETERS
 |     p_resource_txns IN  gme_resource_txns%ROWTYPE
 |
 |   RETURNS
 |      BOOLEAN
 |   HISTORY
 |   12-MAR-01 Thomas Daniel   Created
 |   26-AUG-02  Bharati Satpute  Bug 2404126
 |   Added error message 'GME_RECORD_CHANGED'
 |   10-AUG-04  Rishi Varma  B3818266/3759970
 |              Added the reverse_id field.
 |
 |
 +=============================================================================
 Api end of comments
*/
   FUNCTION update_row (p_resource_txns IN gme_resource_txns%ROWTYPE)
      RETURN BOOLEAN
   IS
      l_dummy                NUMBER    := 0;
      locked_by_other_user   EXCEPTION;
      PRAGMA EXCEPTION_INIT (locked_by_other_user, -54);
   BEGIN
      IF p_resource_txns.poc_trans_id IS NOT NULL THEN
         SELECT     1
               INTO l_dummy
               FROM gme_resource_txns
              WHERE poc_trans_id = p_resource_txns.poc_trans_id
         FOR UPDATE NOWAIT;

         UPDATE gme_resource_txns
            SET organization_id = p_resource_txns.organization_id
               ,doc_type = p_resource_txns.doc_type
               ,doc_id = p_resource_txns.doc_id
               ,line_type = p_resource_txns.line_type
               ,line_id = p_resource_txns.line_id
               ,resources = p_resource_txns.resources
               ,resource_usage = p_resource_txns.resource_usage
               ,trans_qty_um = p_resource_txns.trans_qty_um
               ,trans_date = p_resource_txns.trans_date
               ,completed_ind = p_resource_txns.completed_ind
               ,event_id = p_resource_txns.event_id
               ,posted_ind = p_resource_txns.posted_ind
               ,overrided_protected_ind =
                                       p_resource_txns.overrided_protected_ind
               ,reason_code = p_resource_txns.reason_code
               ,reason_id = p_resource_txns.reason_id
               ,start_date = p_resource_txns.start_date
               ,end_date = p_resource_txns.end_date
               ,last_update_date = gme_common_pvt.g_timestamp
               ,last_updated_by = gme_common_pvt.g_user_ident
               ,last_update_login = gme_common_pvt.g_login_id
               ,delete_mark = p_resource_txns.delete_mark
               ,text_code = p_resource_txns.text_code
               ,instance_id = p_resource_txns.instance_id
               ,sequence_dependent_ind =
                                        p_resource_txns.sequence_dependent_ind
               ,attribute1 = p_resource_txns.attribute1
               ,attribute2 = p_resource_txns.attribute2
               ,attribute3 = p_resource_txns.attribute3
               ,attribute4 = p_resource_txns.attribute4
               ,attribute5 = p_resource_txns.attribute5
               ,attribute6 = p_resource_txns.attribute6
               ,attribute7 = p_resource_txns.attribute7
               ,attribute8 = p_resource_txns.attribute8
               ,attribute9 = p_resource_txns.attribute9
               ,attribute10 = p_resource_txns.attribute10
               ,attribute11 = p_resource_txns.attribute11
               ,attribute12 = p_resource_txns.attribute12
               ,attribute13 = p_resource_txns.attribute13
               ,attribute14 = p_resource_txns.attribute14
               ,attribute15 = p_resource_txns.attribute15
               ,attribute16 = p_resource_txns.attribute16
               ,attribute17 = p_resource_txns.attribute17
               ,attribute18 = p_resource_txns.attribute18
               ,attribute19 = p_resource_txns.attribute19
               ,attribute20 = p_resource_txns.attribute20
               ,attribute21 = p_resource_txns.attribute21
               ,attribute22 = p_resource_txns.attribute22
               ,attribute23 = p_resource_txns.attribute23
               ,attribute24 = p_resource_txns.attribute24
               ,attribute25 = p_resource_txns.attribute25
               ,attribute26 = p_resource_txns.attribute26
               ,attribute27 = p_resource_txns.attribute27
               ,attribute28 = p_resource_txns.attribute28
               ,attribute29 = p_resource_txns.attribute29
               ,attribute30 = p_resource_txns.attribute30
               ,attribute_category = p_resource_txns.attribute_category
               ,program_id = p_resource_txns.program_id
               ,program_application_id =
                                        p_resource_txns.program_application_id
               ,request_id = p_resource_txns.request_id
               ,program_update_date = p_resource_txns.program_update_date
               --10-AUG-04  Rishi Varma  B3818266/3759970
         ,      reverse_id = p_resource_txns.reverse_id
          WHERE poc_trans_id = p_resource_txns.poc_trans_id;
      ELSE
         gme_common_pvt.log_message ('GME_NO_KEYS'
                                    ,'TABLE_NAME'
                                    ,g_table_name);
         RETURN FALSE;
      END IF;

      IF SQL%FOUND THEN
         RETURN TRUE;
      ELSE
         IF l_dummy = 0 THEN
            gme_common_pvt.log_message ('GME_NO_DATA_FOUND'
                                       ,'TABLE_NAME'
                                       ,g_table_name);
         ELSE
            gme_common_pvt.log_message ('GME_RECORD_CHANGED'
                                       ,'TABLE_NAME'
                                       ,g_table_name);
         END IF;

         RETURN FALSE;
      END IF;
   EXCEPTION
      WHEN NO_DATA_FOUND THEN
         IF l_dummy = 0 THEN
            gme_common_pvt.log_message ('GME_NO_DATA_FOUND'
                                       ,'TABLE_NAME'
                                       ,g_table_name);
         ELSE
            gme_common_pvt.log_message ('GME_RECORD_CHANGED'
                                       ,'TABLE_NAME'
                                       ,g_table_name);
         END IF;

         RETURN FALSE;
      WHEN locked_by_other_user THEN
         gme_common_pvt.log_message ('GME_RECORD_LOCKED'
                                    ,'TABLE_NAME'
                                    ,g_table_name
                                    ,'RECORD'
                                    ,'Resource'
                                    ,'KEY'
                                    ,p_resource_txns.resources);
         RETURN FALSE;
      WHEN OTHERS THEN
         gme_common_pvt.log_message ('GME_UNEXPECTED_ERROR', 'ERROR'
                                    ,SQLERRM);
         RETURN FALSE;
   END update_row;

/* Api start of comments
 +============================================================================
 |   FUNCTION NAME
 |      lock_row
 |
 |   TYPE
 |      Private
 |   USAGE
 |      Lock_Row will lock a row in gme_resource_txns
 |
 |
 |   DESCRIPTION
 |      Lock_Row will lock a row in gme_resource_txns
 |
 |
 |
 |   PARAMETERS
 |     p_resource_txns IN  gme_resource_txns%ROWTYPE
 |
 |   RETURNS
 |      BOOLEAN
 |   HISTORY
 |   12-MAR-01 Thomas Daniel   Created
 |
 |
 |
 +=============================================================================
 Api end of comments
*/
   FUNCTION lock_row (p_resource_txns IN gme_resource_txns%ROWTYPE)
      RETURN BOOLEAN
   IS
      l_dummy   NUMBER;
   BEGIN
      IF p_resource_txns.poc_trans_id IS NOT NULL THEN
         SELECT     1
               INTO l_dummy
               FROM gme_resource_txns
              WHERE poc_trans_id = p_resource_txns.poc_trans_id
         FOR UPDATE NOWAIT;
      END IF;

      RETURN TRUE;
   EXCEPTION
      WHEN app_exception.record_lock_exception THEN
         gme_common_pvt.log_message ('GME_RECORD_LOCKED'
                                    ,'TABLE_NAME'
                                    ,g_table_name
                                    ,'RECORD'
                                    ,'Resource'
                                    ,'KEY'
                                    ,p_resource_txns.resources);
         RETURN FALSE;
      WHEN OTHERS THEN
         RETURN FALSE;
   END lock_row;
END gme_resource_txns_dbl;
/

COMMIT ;
EXIT;
