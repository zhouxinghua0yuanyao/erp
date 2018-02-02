/* +======================================================================+ */
/* |    Copyright (c) 2005, 2017 Oracle and/or its affiliates.           | */
/* |                         All rights reserved.                         | */
/* |                           Version 12.0.0                             | */
/* +======================================================================+ */
REM dbdrv: sql ~PROD ~PATH ~FILE none none none package &phase=plb \
REM dbdrv: checkfile(120.36.12010000.44=120.61.12020000.23)(120.32.12000000.40=120.36.12010000.31)(120.32.12000000.35=120.36.12010000.26)(120.32.12000000.34=120.36.12010000.25)(120.32.12000000.31=120.62):~PROD:~PATH:~FILE
SET VERIFY OFF
WHENEVER SQLERROR EXIT FAILURE ROLLBACK;
WHENEVER OSERROR EXIT FAILURE ROLLBACK;

CREATE OR REPLACE PACKAGE BODY gme_common_pvt AS
/* $Header: GMEVCMNB.pls 120.61.12020000.23 2017/05/03 01:55:54 maychen ship $ */
   g_debug               VARCHAR2 (5)  := fnd_profile.VALUE ('AFLOG_LEVEL');
   g_pkg_name   CONSTANT VARCHAR2 (30) := 'GME_COMMON_PVT';

/************************************************************************************************
 Oracle Process Manufacturing Process Execution APIs                                           
                                                                                               
 File Name: GMEVCMNB.pls                                                                       
 Contents:  GME common procedures.                                                             
 HISTORY     
                                                                                   
 SivakumarG Bug#4585491 16-FEB-06                                                              
   Used return status B, M and R instead of W3,W2 and W1 to represent Both Reservations and     
   MO allocations are deleted,Only Reservations deleted,Only MO allocations deleted respectively 
 Pawan Kumar -- bug 5138929 2nd may 2006
   Added  procedure check_close_period
 Susruth D. Bug#5311713 Rounded the wip_plan_qty and plan_qty to 5 decimal places.
 Namit S -- bug 5176319 20 Jun 2006								
   Added  procedure reset_txn_hdr_tbl								
 Sunitha ch. Bug#5336007 checked the parameter p_validate_plan_dates_ind.If it is 1 then        
   validate planned start date only.If it is 0 then validate both                               
 SivakumarG Bug#Bug#5394232 26-Jun-2006                                                        
   Added new procedure fetch_trans_date that can give us the default transaction date by        
   considering release types batch actions and all                                              
 Sunitha ch. Bug 5404329 Changed the condition fron AND to OR                                  
 Sunitha ch. bug 5581523  removed the default value of the l_qty variable                      
 SivakumarG Bug#5618961 07-Oct-2006                                                            
   Modified the get_process_loss procedure to consider the qty correctly for getting correct   
   theoretical process loss from routing class table                                           
 Swapna K Bug#7157383 16-JUL-08
   Modified validate_material_detail procedure to comment the validation for
   the phantom batch
 Swapna K Bug#7346653 16-JUL-08
   Commented the procedure reservation_fully_specified in the get_open_qty
   procedure.
 Swapna K Bug#7385805 13-OCT-2008
   Commented the code to divide the leadtime by 24 as the leadtime from Item
   master is already defined in days.

  G. Muratore    01-DEC-2008   Bug 7562848
   Added parameter p_check_phantom to allow calling routine to bypass phantom check.
   Many api's call validate_batch, but not all the api's are invalid for phantom batches.
   This can be used in the future by other api's when it is determined that they are valid
   for phantoms.   Procedure: Validate_batch
  
   Aditi Sharma  21-May-2009  Bug 7496141
   Added two condition in the query(i.e TRANSACTION_SOURCE_ID NOT IN (l_mtl_dtl_rec.batch_id) AND 
   TRX_SOURCE_LINE_ID NOT IN (l_mtl_dtl_rec.material_detail_id)) which fetches the total qty 
   allocated to find out org level shortages.

  A.Mishra 01-JUL-2009 Bug -- Bug 8582412 
   Moved cursor outside of if - we need unusable for both calculations of ATR and ATT.
   Basically we are now disregarding qty's for lots that have a status which is not useable for production.  
   Prior to  fix it was doing it only for ATT but now it is doing for ATR also.

  G. Muratore    11-MAR-2010   Bug 9442109
   Pass in material_requirement_date as p_lot_expiration_date parameter when making the call to query
   quantities. The INV function will use this value to exclude lots which aill be expired by this date.
   Procedure: get_batch_shortages
   
  G. Muratore    13-MAY-2010   Bug 9666304
   Insert att variable value into exception table using NVL.
   Procedure: insert_exceptions

   Kishore   22-Jul-2010  Bug No.9924437
     Added code in get_open_qty when p_called_by is 'Z'. As WMS is not considering allocations
     while doing reservations, added code to exclude allocations from planned qty before sending to WMS.
   
  G. Muratore    03-AUG-2010   Bug 9938452
   Renamed procedure set_move_to_temp_off to set_move_to_temp_toggle.
   Also, added parameter to allow toggling of the global variable g_move_to_temp.
   PROCEDURE: set_move_to_temp_toggle
   
  G. Muratore    22-Jun-2011   Bug 12613813
   Pass in true for new parameter when calling get_reserved_qty for picking only.
   This will direct get_open_qty to consider locator as well when summing the reservations.
   PROCEDURE: get_open_qty
      
  G. Muratore    14-NOV-2011   Bug 13356510
   Commented out validation code that was incorrectly checking for phantom batch.
   PROCEDURE: Validate_batch_step   
          
  G. Muratore    20-MAR-2012  Bug 13811289  
   Make sure validity rule is valid for the current organization.     
   PROCEDURE: Validate_validity_rule         
          
  G. Muratore    28-MAR-2012  Bug 13611486  
   Backout calculation introduced in 8582412. There is missing functionality when calling
   INV to get att/atr. It is not considering the transaction type in tandem with material status.
   This fix will be included in patch 13795581 and bug 13611486 will be transferred to INV to 
   see if they can correct the functionality.
   PROCEDURE: get_batch_shortages     
   
  G. Muratore    03-APR-2012   Bug 13881792
   During IB, return the user entered transaction date when exceptions appear.
   Procedure: fetch_trans_date and reset_txn_header_id.
   
  G. Muratore    16-APR-2012   Bug 13946067
   Introduce new table and joins to make use of existing index.
   Procedure: resource_qty_below_capacity.
   
  G. Muratore    19-APR-2012   Bug 13721430
   Calculate leadtime properly by considering lot size.
   Procedure: calc_date_from_prod_rule.   
          
  G. Muratore    01-OCT-2012  Bug 14488987  
   Backout calculation introduced in 7462542 for release 12.1 and above. 
   INV code already considers Move orders in the newer versions of the code.
   PROCEDURE: get_batch_shortages        
   
  A. Mundhe      15-NOV-2012  Bug 15847423
     Modified function setup to return true only if the org_id passed is same as the g_organization_id.

  G. Muratore    07-MAY-2013  Bug 16100060
   Refine fix made in 13611486. The opening of unnecessary cursor is now commented out.
   PROCEDURE: get_batch_shortages     

  A. Mundhe      06-JUN-2013  Bug 14634379 
   Introduce new parameter to control when auto detail logic is used.
   FUNCTION setup
     
  Shaliu Chen    10-OCT-2013
   Modify to avoid exception handling in the innermost layer functions overrided by outermost layer functions.
   
  Srinivas Puri  16-Jun-2014
   Modidifed to fix ERES calls for bug 18230984. 
   1. Payload parameter 8 needs to be a valid eRecord ID or -1
   2. Call Ackn only when a eRecord is generated.
   
  G. Muratore    09-DEC-2014  Bug 20102866 
   Move setting of GLOBAL user id outside of organization check as this is independent of organization.
   FUNCTION setup
   
  G. Muratore    19-MAR-2015  Bug 20691997 
   Pass in secondary qty when updating a reservation.
   PROCEDURE material_date_change 
   
  G. Muratore    08-Sep-2015  Bug 21653652
   Use full reserved quantity for picking if it is more than the plan qty.
   This is needed when the customer over reserves on purpose.
   PROCEDURE: get_open_qty

  G. Muratore    21-OCT-2015  Bug 19868921
   Reset partial negative IB global.
   Procedure: reset_txn_header_id.

  G. Muratore    20-OCT-2016  Bug 23176130
   New parm to control auto detailing of phantom batches. Default is 0.
   FUNCTION setup   
***********************************************************************************************/

   FUNCTION get_txn_header_id
      RETURN NUMBER
   IS
   BEGIN
      RETURN gme_common_pvt.g_transaction_header_id;
   END get_txn_header_id;

   PROCEDURE set_txn_header_id(p_txn_header_id NUMBER) IS
   BEGIN
     gme_common_pvt.g_transaction_header_id := p_txn_header_id;
   END set_txn_header_id;
   
   PROCEDURE reset_txn_header_id IS
   BEGIN
     gme_common_pvt.g_transaction_header_id := NULL;

     -- Bug 13881792 - Reset IB timestamp set global.
     gme_common_pvt.g_ib_timestamp_set := 0;

     -- Bug 19868921 - Reset partial negative IB global.
     gme_common_pvt.g_ib_going_negative := 0;
     
   END reset_txn_header_id;

   FUNCTION get_timestamp
      RETURN DATE
   IS
   BEGIN
      RETURN gme_common_pvt.g_timestamp;
   END get_timestamp;

   -- Bug 9938452 - renamed procedure set_move_to_temp_off to set_move_to_temp_toggle.
   -- Also, added parameter to allow toggling of the global variable.
   PROCEDURE set_move_to_temp_toggle(p_true IN NUMBER DEFAULT 0)
   IS
   BEGIN
      IF p_true = 1 THEN
         gme_common_pvt.g_move_to_temp := fnd_api.g_true;
      ELSE
         gme_common_pvt.g_move_to_temp := fnd_api.g_false;
      END IF;
   END set_move_to_temp_toggle;

-- nsinghi bug#5176319. Added this proc.
/*   This procedure will take care of deleting the table g_mat_txn_hdr_tbl. This table should be 
truncated before and after unrelease_step and unrelease_batch. The after part is taken care of 
gme_transactions_pvt.
*/
   PROCEDURE reset_txn_hdr_tbl IS
   BEGIN
     IF gme_common_pvt.g_mat_txn_hdr_tbl.COUNT > 0 THEN
       gme_common_pvt.g_mat_txn_hdr_tbl.DELETE;
     END IF;
   END reset_txn_hdr_tbl;

   PROCEDURE log_message (
      p_message_code   IN   VARCHAR2
     ,p_token1_name    IN   VARCHAR2 := NULL
     ,p_token1_value   IN   VARCHAR2 := NULL
     ,p_token2_name    IN   VARCHAR2 := NULL
     ,p_token2_value   IN   VARCHAR2 := NULL
     ,p_token3_name    IN   VARCHAR2 := NULL
     ,p_token3_value   IN   VARCHAR2 := NULL
     ,p_token4_name    IN   VARCHAR2 := NULL
     ,p_token4_value   IN   VARCHAR2 := NULL
     ,p_token5_name    IN   VARCHAR2 := NULL
     ,p_token5_value   IN   VARCHAR2 := NULL
     --FPBug#4351032 Added new token and value
     ,p_token6_name    IN   VARCHAR2 := NULL
     ,p_token6_value   IN   VARCHAR2 := NULL
     ,p_product_code   IN   VARCHAR2 := 'GME')
   IS
      l_api_name   CONSTANT VARCHAR2 (30) := 'log_message';
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      fnd_message.set_name (p_product_code, p_message_code);

      IF p_token1_name IS NOT NULL THEN
         fnd_message.set_token (p_token1_name, p_token1_value);
      END IF;

      IF p_token2_name IS NOT NULL THEN
         fnd_message.set_token (p_token2_name, p_token2_value);
      END IF;

      IF p_token3_name IS NOT NULL THEN
         fnd_message.set_token (p_token3_name, p_token3_value);
      END IF;

      IF p_token4_name IS NOT NULL THEN
         fnd_message.set_token (p_token4_name, p_token4_value);
      END IF;

      IF p_token5_name IS NOT NULL THEN
         fnd_message.set_token (p_token5_name, p_token5_value);
      END IF;
      --FPBug#4351032
      IF p_token6_name IS NOT NULL THEN
         fnd_message.set_token (p_token6_name, p_token6_value);
      END IF;

      fnd_msg_pub.ADD;

      IF g_debug <= gme_debug.g_log_procedure THEN
         --FPBug#4351032 modified to include new token and value
         gme_debug.put_line (   'FROM LOG_MESSAGE '
                             || p_message_code
                             || '/'
                             || p_token1_name
                             || '/'
                             || p_token1_value
                             || '/'
                             || p_token2_name
                             || '/'
                             || p_token2_value
                             || '/'
                             || p_token3_name
                             || '/'
                             || p_token3_value
                             || '/'
                             || p_token4_name
                             || '/'
                             || p_token4_value
                             || '/'
                             || p_token5_name
                             || '/'
                             || p_token5_value
			     || '/'
                             || p_token6_name
                             || '/'
                             || p_token6_value);
      END IF;

      gme_common_pvt.g_error_count := gme_common_pvt.g_error_count + 1;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;
   EXCEPTION
      WHEN OTHERS THEN
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);

         IF g_debug <= gme_debug.g_log_unexpected THEN
            gme_debug.put_line (   'When others exception in '
                                || g_pkg_name
                                || '.'
                                || l_api_name
                                || ' Error is '
                                || SQLERRM);
         END IF;
   END log_message;

   FUNCTION setup (p_org_id IN NUMBER := NULL, p_org_code IN VARCHAR2 := NULL)
      RETURN BOOLEAN
   IS
      CURSOR cur_get_gme_params (v_org_id NUMBER)
      IS
         SELECT *
           FROM gme_parameters
          WHERE organization_id = v_org_id;

      CURSOR cur_get_inv_params (v_org_id NUMBER, v_org_code VARCHAR2)
      IS
         SELECT organization_code, calendar_code, negative_inv_receipt_code
               ,stock_locator_control_code, organization_id
           FROM mtl_parameters
          WHERE (organization_id = v_org_id)
             OR (v_org_id IS NULL AND organization_code = v_org_code);

      CURSOR cur_get_user (v_user_id NUMBER)
      IS
         SELECT 1
           FROM DUAL
          WHERE EXISTS (SELECT 1
                          FROM fnd_user
                         WHERE user_id = v_user_id);

      l_gmd_params_rec         gmd_parameters_dtl_pkg.parameter_rec_type;
      l_cur_get_org_params     cur_get_gme_params%ROWTYPE;
      l_dummy                  NUMBER;
      l_return_status          VARCHAR2 (1);
      l_api_name      CONSTANT VARCHAR2 (30)                        := 'setup';
      invalid_user             EXCEPTION;
      gme_params_not_defined   EXCEPTION;
      gmd_params_not_defined   EXCEPTION;
      inv_params_not_found     EXCEPTION;
      missing_profile_option   EXCEPTION;
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      -- Bug 20102866 - Move setting of user id outside of org check as this is independent of org.
      gme_common_pvt.g_login_id   := TO_NUMBER (fnd_profile.VALUE ('LOGIN_ID'));
      gme_common_pvt.g_user_ident := TO_NUMBER (fnd_profile.VALUE ('USER_ID'));
      
      -- Bug 25183093
      if nvl(gme_common_pvt.g_user_ident, -1) = -1 then 
          gme_common_pvt.g_user_ident := TO_NUMBER (fnd_global.user_id); 
      end if;   

      OPEN cur_get_user (gme_common_pvt.g_user_ident);

      FETCH cur_get_user
       INTO l_dummy;

      IF (cur_get_user%NOTFOUND) THEN
         CLOSE cur_get_user;      	
         RAISE invalid_user;
      END IF;

      CLOSE cur_get_user;

      IF NVL (gme_common_pvt.g_user_ident, -1) = -1 THEN
         log_message ('GME_API_INVALID_USER_NAME'
                     ,'USER_NAME'
                     ,gme_common_pvt.g_user_name);
         RAISE missing_profile_option;
      END IF;
      
      -- Bug 15847423
      -- Return true only if the org_id passed is same as the g_organization_id.
      IF (gme_common_pvt.g_setup_done) THEN 
          IF  NVL(p_org_id,-999) = gme_common_pvt.g_organization_id THEN
              RETURN TRUE;
          END IF;
      END IF;

      OPEN cur_get_inv_params (p_org_id, p_org_code);

      FETCH cur_get_inv_params
       INTO gme_common_pvt.g_organization_code
           ,gme_common_pvt.g_calendar_code, gme_common_pvt.g_allow_neg_inv
           ,gme_common_pvt.g_org_locator_control
           ,gme_common_pvt.g_organization_id;

      IF (cur_get_inv_params%NOTFOUND) THEN
         CLOSE cur_get_inv_params;

         RAISE inv_params_not_found;
      END IF;

      CLOSE cur_get_inv_params;

      gmd_api_grp.fetch_parm_values
                               (p_orgn_id            => gme_common_pvt.g_organization_id
                               ,x_out_rec            => l_gmd_params_rec
                               ,x_return_status      => l_return_status);

      /*Assign the gmd params values to package variables */
      IF l_return_status = fnd_api.g_ret_sts_success THEN
         gme_common_pvt.g_plant_ind := l_gmd_params_rec.plant_ind;
         gme_common_pvt.g_lab_ind := l_gmd_params_rec.lab_ind;
         gme_common_pvt.g_yield_type := l_gmd_params_rec.fm_yield_type;
         gme_common_pvt.g_release_type := l_gmd_params_rec.steprelease_type;
         gme_common_pvt.g_byprod_active :=
                                        l_gmd_params_rec.gmd_byproduct_active;
         gme_common_pvt.g_mass_um_type := l_gmd_params_rec.gmd_mass_um_type;
         gme_common_pvt.g_volume_um_type := l_gmd_params_rec.gmd_volume_um_type;
      ELSE
         RAISE gmd_params_not_defined;
      END IF;

      OPEN cur_get_gme_params (gme_common_pvt.g_organization_id);

      FETCH cur_get_gme_params
       INTO l_cur_get_org_params;

      IF cur_get_gme_params%NOTFOUND THEN
         CLOSE cur_get_gme_params;

         RAISE gme_params_not_defined;
      END IF;

      CLOSE cur_get_gme_params;

      gme_common_pvt.g_auto_consume_supply_sub_only :=
                             l_cur_get_org_params.auto_consume_supply_sub_only;
      gme_common_pvt.g_subinv_loc_ind :=
                             l_cur_get_org_params.subinv_loc_ind;   --Bug#5663458
      gme_common_pvt.g_supply_subinventory :=
                                      l_cur_get_org_params.supply_subinventory;
      gme_common_pvt.g_supply_locator_id :=
                                        l_cur_get_org_params.supply_locator_id;
      gme_common_pvt.g_yield_subinventory :=
                                       l_cur_get_org_params.yield_subinventory;
      gme_common_pvt.g_yield_locator_id :=
                                         l_cur_get_org_params.yield_locator_id;
      gme_common_pvt.g_delete_material_ind :=
                                      l_cur_get_org_params.delete_material_ind;
      gme_common_pvt.g_validate_plan_dates_ind :=
                                  l_cur_get_org_params.validate_plan_dates_ind;
      --Bug#5111078
      gme_common_pvt.g_ib_factor_ind :=
                                  l_cur_get_org_params.ib_factor_ind;
      --nsinghi bug#5674398 Added the global variable for ingr subs date type
      -- Default it to Material Requirement Date as in R12, default ingr sub happens using mat req date.
      gme_common_pvt.g_ingr_sub_date :=
                                  NVL(l_cur_get_org_params.ingr_sub_date, 2);

      gme_common_pvt.g_display_unconsumed_material :=
                              l_cur_get_org_params.display_unconsumed_material;
      gme_common_pvt.g_step_controls_batch_sts_ind :=
                              l_cur_get_org_params.step_controls_batch_sts_ind;
      gme_common_pvt.g_backflush_rsrc_usg_ind :=
                                   l_cur_get_org_params.backflush_rsrc_usg_ind;
      gme_common_pvt.g_def_actual_rsrc_usg_ind :=
                                  l_cur_get_org_params.def_actual_rsrc_usg_ind;
      gme_common_pvt.g_calc_interim_rsrc_usg_ind :=
                                l_cur_get_org_params.calc_interim_rsrc_usg_ind;
      gme_common_pvt.g_allow_qty_below_min_ind :=
                                  l_cur_get_org_params.allow_qty_below_min_ind;
      gme_common_pvt.g_display_non_work_days_ind :=
                                l_cur_get_org_params.display_non_work_days_ind;
      gme_common_pvt.g_check_shortages_ind :=
                                      l_cur_get_org_params.check_shortages_ind;
      gme_common_pvt.g_copy_formula_text_ind :=
                                    l_cur_get_org_params.copy_formula_text_ind;
      gme_common_pvt.g_copy_routing_text_ind :=
                                    l_cur_get_org_params.copy_routing_text_ind;
      gme_common_pvt.g_create_high_level_resv_ind :=
                               l_cur_get_org_params.create_high_level_resv_ind;
      gme_common_pvt.g_create_move_orders_ind :=
                                   l_cur_get_org_params.create_move_orders_ind;
      gme_common_pvt.g_reservation_timefence :=
                                    l_cur_get_org_params.reservation_timefence;
      gme_common_pvt.g_move_order_timefence :=
                                     l_cur_get_org_params.move_order_timefence;
      gme_common_pvt.g_batch_doc_numbering :=
                                      l_cur_get_org_params.batch_doc_numbering;
      gme_common_pvt.g_batch_no_last_assigned :=
                                   l_cur_get_org_params.batch_no_last_assigned;
      gme_common_pvt.g_fpo_doc_numbering :=
                                        l_cur_get_org_params.fpo_doc_numbering;
      gme_common_pvt.g_fpo_no_last_assigned :=
                                     l_cur_get_org_params.fpo_no_last_assigned;
      gme_common_pvt.g_rule_based_resv_horizon :=
                                  l_cur_get_org_params.rule_based_resv_horizon;
      gme_common_pvt.g_hour_uom_code :=
                                       fnd_profile.VALUE ('BOM:HOUR_UOM_CODE');

      -- Bug 14634379 - New auto detail parameter introduced.
      gme_common_pvt.g_auto_detail_batch :=
                             NVL(l_cur_get_org_params.auto_detail_batch_ind, 0);

      -- Bug 17722332 -- New default parameter for picking rule
      gme_common_pvt.g_pick_slip_grouping_rule_id :=
                             NVL(l_cur_get_org_params.pick_slip_grouping_rule_id, 0);

      -- Bug 23176130 - New parm to control auto detailing of phantom batches. Default is 0.
      gme_common_pvt.g_auto_detail_phantom_batches :=
                             NVL(l_cur_get_org_params.auto_detail_phantom_batches, 0);                             

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;

      RETURN TRUE;
   EXCEPTION
      WHEN invalid_user THEN
         fnd_message.set_name ('INV', 'INV_INT_USRCODE');
         fnd_msg_pub.ADD;
         RETURN FALSE;
      WHEN missing_profile_option THEN
         RETURN FALSE;
      WHEN inv_params_not_found THEN
         fnd_message.set_name ('INV', 'INV-NO ORG INFORMATION');
         fnd_msg_pub.ADD;
         RETURN FALSE;
      WHEN gmd_params_not_defined THEN
         RETURN FALSE;
      WHEN gme_params_not_defined THEN
         gme_common_pvt.log_message ('GME_DEFINE_GME_PARAMETERS');
         RETURN FALSE;
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
         RETURN FALSE;
   END setup;

   PROCEDURE set_timestamp
   IS
      l_api_name   CONSTANT VARCHAR2 (30) := 'set_timestamp';
   BEGIN
      gme_common_pvt.g_timestamp := SYSDATE;
   EXCEPTION
      WHEN OTHERS THEN
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);

         IF g_debug <= gme_debug.g_log_unexpected THEN
            gme_debug.put_line (   'When others exception in '
                                || g_pkg_name
                                || '.'
                                || l_api_name
                                || ' Error is '
                                || SQLERRM);
         END IF;
   END set_timestamp;

   PROCEDURE set_who
   IS
      l_api_name   CONSTANT VARCHAR2 (30) := 'set_who';
   BEGIN
      gme_common_pvt.g_user_ident := fnd_profile.VALUE ('USER_ID');
      gme_common_pvt.g_login_id := fnd_profile.VALUE ('LOGIN_ID');
      gme_common_pvt.set_timestamp;
   EXCEPTION
      WHEN OTHERS THEN
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);

         IF g_debug <= gme_debug.g_log_unexpected THEN
            gme_debug.put_line (   'When others exception in '
                                || g_pkg_name
                                || '.'
                                || l_api_name
                                || ' Error is '
                                || SQLERRM);
         END IF;
   END set_who;

   PROCEDURE count_and_get (
      p_encoded   IN              VARCHAR2 := 'T'
     ,x_count     OUT NOCOPY      NUMBER
     ,x_data      OUT NOCOPY      VARCHAR2)
   IS
      l_msg_count           NUMBER;
      l_api_name   CONSTANT VARCHAR2 (30) := 'count_and_get';
   BEGIN
      l_msg_count := fnd_msg_pub.count_msg;
      x_data :=
         fnd_msg_pub.get (p_msg_index      => fnd_msg_pub.g_last
                         ,p_encoded        => p_encoded);
      x_count := l_msg_count;
   EXCEPTION
      WHEN OTHERS THEN
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);

         IF g_debug <= gme_debug.g_log_unexpected THEN
            gme_debug.put_line (   'When others exception in '
                                || g_pkg_name
                                || '.'
                                || l_api_name
                                || ' Error is '
                                || SQLERRM);
         END IF;
   END count_and_get;

   PROCEDURE get_who (
      x_user_ident      OUT NOCOPY   NUMBER
     ,x_login_id        OUT NOCOPY   NUMBER
     ,x_timestamp       OUT NOCOPY   DATE
     ,x_return_status   OUT NOCOPY   VARCHAR2)
   IS
      l_message_count       NUMBER;
      l_message_list        VARCHAR2 (2000);
      l_api_name   CONSTANT VARCHAR2 (30)   := 'GET_WHO';
   BEGIN
      /* Set the return status to success initially */
      x_return_status := fnd_api.g_ret_sts_success;

      IF gme_common_pvt.g_user_ident IS NULL THEN
         set_who;
      END IF;

      x_user_ident := gme_common_pvt.g_user_ident;
      x_login_id := gme_common_pvt.g_login_id;
      x_timestamp := gme_common_pvt.g_timestamp;
   EXCEPTION
      WHEN OTHERS THEN
         x_return_status := fnd_api.g_ret_sts_unexp_error;
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
         fnd_msg_pub.count_and_get (p_count      => l_message_count
                                   ,p_data       => l_message_list);

         IF g_debug <= gme_debug.g_log_unexpected THEN
            gme_debug.put_line (   'When others exception in '
                                || g_pkg_name
                                || '.'
                                || l_api_name
                                || ' Error is '
                                || SQLERRM);
         END IF;
   END get_who;

   PROCEDURE get_supply_defaults (
      p_organization_id     IN              NUMBER
     ,p_inventory_item_id   IN              NUMBER
     ,x_subinventory        OUT NOCOPY      VARCHAR2
     ,x_locator_id          OUT NOCOPY      NUMBER
     ,x_return_status       OUT NOCOPY      VARCHAR2)
   IS
      l_api_name     CONSTANT VARCHAR2 (30) := 'get_supply_defaults';
      l_eff_locator_control   NUMBER;
      l_restrict_subinv       NUMBER;
      l_item_loc_control      NUMBER;
      l_restrict_locators     NUMBER;
      l_sub_locator_type      NUMBER;
      /* Bug 5441643 Added NVL condition for location control code*/
      CURSOR cur_supply_defaults (v_org_id NUMBER, v_inventory_item_id NUMBER)
      IS
         SELECT NVL (i.process_supply_subinventory
                    ,g.supply_subinventory)
               ,NVL (i.process_supply_locator_id
                    ,g.supply_locator_id)
               ,i.restrict_subinventories_code, NVL(i.location_control_code,1)
               ,i.restrict_locators_code
           FROM mtl_system_items_b i, gme_parameters g
          WHERE i.organization_id = v_org_id
            AND i.inventory_item_id = v_inventory_item_id
            AND g.organization_id = v_org_id;

      CURSOR cur_sub_control (v_org_id NUMBER, v_subinventory VARCHAR2)
      IS
         SELECT locator_type
           FROM mtl_secondary_inventories
          WHERE organization_id = v_org_id
            AND secondary_inventory_name = v_subinventory;
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      x_return_status := fnd_api.g_ret_sts_success;

      OPEN cur_supply_defaults (p_organization_id, p_inventory_item_id);

      FETCH cur_supply_defaults
       INTO x_subinventory, x_locator_id, l_restrict_subinv
           ,l_item_loc_control, l_restrict_locators;

      CLOSE cur_supply_defaults;

      IF (x_subinventory IS NOT NULL) THEN
         IF NOT (gme_common_pvt.check_subinventory
                                  (p_organization_id        => p_organization_id
                                  ,p_subinventory           => x_subinventory
                                  ,p_inventory_item_id      => p_inventory_item_id
                                  ,p_restrict_subinv        => l_restrict_subinv) ) THEN
            x_subinventory := NULL;
            x_locator_id := NULL;
            RETURN;
         END IF;
      END IF;

      OPEN cur_sub_control (p_organization_id, x_subinventory);

      FETCH cur_sub_control
       INTO l_sub_locator_type;

      CLOSE cur_sub_control;

      l_eff_locator_control :=
         gme_common_pvt.eff_locator_control
                       (p_organization_id        => p_organization_id
                       ,p_org_control            => gme_common_pvt.g_org_locator_control
                       ,p_subinventory           => x_subinventory
                       ,p_sub_control            => l_sub_locator_type
                       ,p_item_control           => l_item_loc_control
                       ,p_item_loc_restrict      => l_restrict_locators
                       ,p_action                 => gme_common_pvt.g_ing_issue_txn_action);

      IF (l_eff_locator_control = 1) THEN
         x_locator_id := NULL;
         RETURN;
      END IF;

      IF (x_locator_id IS NOT NULL) THEN
         OPEN cur_sub_control (p_organization_id, x_subinventory);

         FETCH cur_sub_control
          INTO l_sub_locator_type;

         CLOSE cur_sub_control;

         IF NOT (gme_common_pvt.check_locator
                     (p_organization_id        => p_organization_id
                     ,p_locator_id             => x_locator_id
                     ,p_subinventory           => x_subinventory
                     ,p_inventory_item_id      => p_inventory_item_id
                     ,p_org_control            => gme_common_pvt.g_org_locator_control
                     ,p_sub_control            => l_sub_locator_type
                     ,p_item_control           => l_item_loc_control
                     ,p_item_loc_restrict      => l_restrict_locators
                     ,p_org_neg_allowed        => gme_common_pvt.g_allow_neg_inv
                     ,p_txn_action_id          => gme_common_pvt.g_ing_issue_txn_action) ) THEN
            x_locator_id := NULL;
         END IF;
      END IF;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;
   EXCEPTION
      WHEN OTHERS THEN
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);

         IF g_debug <= gme_debug.g_log_unexpected THEN
            gme_debug.put_line (   'When others exception in '
                                || g_pkg_name
                                || '.'
                                || l_api_name
                                || ' Error is '
                                || SQLERRM);
         END IF;

         x_return_status := fnd_api.g_ret_sts_unexp_error;
   END get_supply_defaults;

   PROCEDURE get_yield_defaults (
      p_organization_id     IN              NUMBER
     ,p_inventory_item_id   IN              NUMBER
     ,p_line_type           IN              NUMBER
     ,x_subinventory        OUT NOCOPY      VARCHAR2
     ,x_locator_id          OUT NOCOPY      NUMBER
     ,x_return_status       OUT NOCOPY      VARCHAR2)
   IS
      l_api_name     CONSTANT VARCHAR2 (30) := 'get_yield_defaults';
      l_txn_action            NUMBER;
      l_eff_locator_control   NUMBER;
      l_restrict_subinv       NUMBER;
      l_item_loc_control      NUMBER;
      l_restrict_locators     NUMBER;
      l_sub_locator_type      NUMBER;
      /* Bug 5441643 Added NVL condition for location control code*/
      CURSOR cur_yield_defaults (v_org_id NUMBER, v_inventory_item_id NUMBER)
      IS
         SELECT NVL (i.process_yield_subinventory
                    ,g.yield_subinventory)
               ,NVL (i.process_yield_locator_id
                    ,g.yield_locator_id)
               ,i.restrict_subinventories_code, NVL(i.location_control_code,1)
               ,i.restrict_locators_code
           FROM mtl_system_items_b i, gme_parameters g
          WHERE i.organization_id = v_org_id
            AND i.inventory_item_id = v_inventory_item_id
            AND g.organization_id = v_org_id;

      CURSOR cur_sub_control (v_org_id NUMBER, v_subinventory VARCHAR2)
      IS
         SELECT locator_type
           FROM mtl_secondary_inventories
          WHERE organization_id = v_org_id
            AND secondary_inventory_name = v_subinventory;
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      x_return_status := fnd_api.g_ret_sts_success;

      OPEN cur_yield_defaults (p_organization_id, p_inventory_item_id);

      FETCH cur_yield_defaults
       INTO x_subinventory, x_locator_id, l_restrict_subinv
           ,l_item_loc_control, l_restrict_locators;

      CLOSE cur_yield_defaults;

      IF (x_subinventory IS NOT NULL) THEN
         IF NOT (gme_common_pvt.check_subinventory
                                  (p_organization_id        => p_organization_id
                                  ,p_subinventory           => x_subinventory
                                  ,p_inventory_item_id      => p_inventory_item_id
                                  ,p_restrict_subinv        => l_restrict_subinv) ) THEN
            x_subinventory := NULL;
            x_locator_id := NULL;
            RETURN;
         END IF;
      END IF;

      IF (p_line_type = g_line_type_prod) THEN
         l_txn_action := gme_common_pvt.g_prod_comp_txn_action;
      ELSIF (p_line_type = g_line_type_prod) THEN
         l_txn_action := gme_common_pvt.g_byprod_comp_txn_action;
      END IF;

      OPEN cur_sub_control (p_organization_id, x_subinventory);

      FETCH cur_sub_control
       INTO l_sub_locator_type;

      CLOSE cur_sub_control;

      l_eff_locator_control :=
         gme_common_pvt.eff_locator_control
                       (p_organization_id        => p_organization_id
                       ,p_org_control            => gme_common_pvt.g_org_locator_control
                       ,p_subinventory           => x_subinventory
                       ,p_sub_control            => l_sub_locator_type
                       ,p_item_control           => l_item_loc_control
                       ,p_item_loc_restrict      => l_restrict_locators
                       ,p_action                 => l_txn_action);

      IF (l_eff_locator_control = 1) THEN
         x_locator_id := NULL;
         RETURN;
      END IF;

      IF (x_locator_id IS NOT NULL) THEN
         IF NOT (gme_common_pvt.check_locator
                     (p_organization_id        => p_organization_id
                     ,p_locator_id             => x_locator_id
                     ,p_subinventory           => x_subinventory
                     ,p_inventory_item_id      => p_inventory_item_id
                     ,p_org_control            => gme_common_pvt.g_org_locator_control
                     ,p_sub_control            => l_sub_locator_type
                     ,p_item_control           => l_item_loc_control
                     ,p_item_loc_restrict      => l_restrict_locators
                     ,p_org_neg_allowed        => gme_common_pvt.g_allow_neg_inv
                     ,p_txn_action_id          => gme_common_pvt.g_ing_issue_txn_action) ) THEN
            x_locator_id := NULL;
         END IF;
      END IF;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;
   EXCEPTION
      WHEN OTHERS THEN
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);

         IF g_debug <= gme_debug.g_log_unexpected THEN
            gme_debug.put_line (   'When others exception in '
                                || g_pkg_name
                                || '.'
                                || l_api_name
                                || ' Error is '
                                || SQLERRM);
         END IF;

         x_return_status := fnd_api.g_ret_sts_unexp_error;
   END get_yield_defaults;

   PROCEDURE construct_material_detail (
      p_formula_detail_rec    IN              fm_matl_dtl%ROWTYPE
     ,p_item_master_rec       IN              mtl_system_items_kfv%ROWTYPE
     ,p_batch_header_rec      IN              gme_batch_header%ROWTYPE
     ,x_material_detail_rec   OUT NOCOPY      gme_material_details%ROWTYPE
     ,x_return_status         OUT NOCOPY      VARCHAR2)
   IS
      l_api_name   CONSTANT VARCHAR2 (30) := 'construct_material_detail';
      l_return_status       VARCHAR2 (1);
      get_defaults_err      EXCEPTION;
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      x_material_detail_rec.formulaline_id :=
                                           p_formula_detail_rec.formulaline_id;
      x_material_detail_rec.line_no := p_formula_detail_rec.line_no;
      x_material_detail_rec.inventory_item_id :=
                                        p_formula_detail_rec.inventory_item_id;
      x_material_detail_rec.line_type := p_formula_detail_rec.line_type;
      x_material_detail_rec.plan_qty :=
            p_formula_detail_rec.qty
            * (1 + p_formula_detail_rec.scrap_factor);
      x_material_detail_rec.dtl_um := p_formula_detail_rec.detail_uom;
      x_material_detail_rec.actual_qty := 0;
      x_material_detail_rec.release_type := p_formula_detail_rec.release_type;
      x_material_detail_rec.scrap_factor := p_formula_detail_rec.scrap_factor;
      x_material_detail_rec.scale_type := p_formula_detail_rec.scale_type;
      x_material_detail_rec.phantom_type := p_formula_detail_rec.phantom_type;
      x_material_detail_rec.cost_alloc := p_formula_detail_rec.cost_alloc;
      x_material_detail_rec.text_code := p_formula_detail_rec.text_code;
      x_material_detail_rec.contribute_yield_ind :=
                          NVL (p_formula_detail_rec.contribute_yield_ind, 'Y');
      x_material_detail_rec.contribute_step_qty_ind :=
                                  p_formula_detail_rec.contribute_step_qty_ind;
      x_material_detail_rec.scale_multiple :=
                                           p_formula_detail_rec.scale_multiple;
      x_material_detail_rec.scale_rounding_variance :=
                                  p_formula_detail_rec.scale_rounding_variance;
      x_material_detail_rec.rounding_direction :=
                                       p_formula_detail_rec.rounding_direction;
      x_material_detail_rec.by_product_type :=
                                          p_formula_detail_rec.by_product_type;
      x_material_detail_rec.organization_id :=
                                            p_batch_header_rec.organization_id;
      x_material_detail_rec.revision := p_formula_detail_rec.revision;

      IF (x_material_detail_rec.line_type = -1) THEN
         get_supply_defaults
             (p_organization_id        => x_material_detail_rec.organization_id
             ,p_inventory_item_id      => x_material_detail_rec.inventory_item_id
             ,x_subinventory           => x_material_detail_rec.subinventory
             ,x_locator_id             => x_material_detail_rec.locator_id
             ,x_return_status          => l_return_status);
      ELSE
         get_yield_defaults
             (p_organization_id        => x_material_detail_rec.organization_id
             ,p_inventory_item_id      => x_material_detail_rec.inventory_item_id
             ,p_line_type              => x_material_detail_rec.line_type
             ,x_subinventory           => x_material_detail_rec.subinventory
             ,x_locator_id             => x_material_detail_rec.locator_id
             ,x_return_status          => l_return_status);
      END IF;

      IF (l_return_status <> fnd_api.g_ret_sts_success) THEN
         RAISE get_defaults_err;
      END IF;

      x_return_status := fnd_api.g_ret_sts_success;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;
   EXCEPTION
      WHEN get_defaults_err THEN
         x_return_status := l_return_status;
      WHEN OTHERS THEN
         gme_common_pvt.log_message ('GME_API_MATL_DTL_SETUP_FAILURE');

         IF g_debug <= gme_debug.g_log_unexpected THEN
            gme_debug.put_line (   'When others exception in '
                                || g_pkg_name
                                || '.'
                                || l_api_name
                                || ' Error is '
                                || SQLERRM);
         END IF;

         x_return_status := fnd_api.g_ret_sts_unexp_error;
   END construct_material_detail;

   FUNCTION get_process_loss (
      p_batch_id                     IN   NUMBER DEFAULT NULL
     ,p_validity_rule_id             IN   NUMBER DEFAULT NULL
     ,p_organization_id              IN   NUMBER DEFAULT NULL
     ,p_total_output_qty_scaled      IN   NUMBER
     ,p_total_output_qty_pre_scale   IN   NUMBER)
      RETURN NUMBER
   IS
      l_api_name                     VARCHAR2 (30)      := 'GET_PROCESS_LOSS';
      l_process_loss_a               NUMBER;
      l_process_loss_b               NUMBER;
      l_process_loss_c               NUMBER;
      l_process_loss                 NUMBER;
      l_recipe_id                    NUMBER;
      l_formula_id                   NUMBER;
      l_routing_id                   NUMBER;
      l_total_output_qty_pre_scale   NUMBER                           := 0;
      l_total_output_qty_scaled      NUMBER                           := 0;
      l_validity_rule_id             NUMBER;
      l_plant_type                   NUMBER;
      l_plant_code                   VARCHAR2 (4);
      l_routing_class                gmd_routings_b.routing_class%TYPE;
      /*Bug#5618961 Begin changed to gmd_routings_b.routing_uom from gmd_routings_b.item_um
        and declared few variables */
      l_uom                          gmd_routings_b.routing_uom%TYPE;
      l_routing_class_uom            gmd_routings_b.routing_uom%TYPE;
      l_routing_uom                  gmd_routings_b.routing_uom%TYPE;
      l_qty                          NUMBER;
      l_routing_process_loss         NUMBER;
      l_routing_qty                  NUMBER;
      l_item_id                      NUMBER;
      --Bug#5618961 End
      expected_error                 EXCEPTION;


      CURSOR cur_get_info (v_batch_id NUMBER)
      IS
         SELECT b.recipe_validity_rule_id, a.recipe_id, b.organization_id
               ,b.routing_id, b.formula_id
           FROM gme_batch_header b, gmd_recipe_validity_rules a
          WHERE b.batch_id = v_batch_id
            AND b.recipe_validity_rule_id = a.recipe_validity_rule_id;

      CURSOR cur_get_info_from_validity (v_validity_rule_id NUMBER)
      IS
         SELECT r.recipe_id, r.routing_id, r.formula_id
           FROM gmd_recipes_b r, gmd_recipe_validity_rules v
          WHERE v.recipe_validity_rule_id = v_validity_rule_id
            AND v.recipe_id = r.recipe_id;

      /*Bug#5618961 cursor modified to select routing class uom instead of 
        selecting routing uom*/
      CURSOR cur_get_rtclass (v_routing_id NUMBER)
      IS
         SELECT a.routing_class,routing_uom, b.routing_class_uom, --item_um
                process_loss, routing_qty
           FROM fm_rout_hdr a, gmd_routing_class_b b
          WHERE a.routing_id = v_routing_id
            AND a.routing_class = b.routing_class (+);

      CURSOR orgn_process_loss_cursor (v_recipe_id NUMBER, v_org_id NUMBER)
      IS
         SELECT process_loss
           FROM gmd_recipe_process_loss
          WHERE recipe_id = v_recipe_id AND organization_id = v_org_id;

      CURSOR recipe_process_loss_cursor (v_recipe_id NUMBER)
      IS
         SELECT planned_process_loss
           FROM gmd_recipes
          WHERE recipe_id = v_recipe_id;
      
      /*Bug#5618961 cursor modified to select routing qty and uom */
      CURSOR routing_process_loss_cursor (v_routing_id NUMBER)
      IS
         SELECT process_loss, routing_qty, routing_uom
           FROM fm_rout_hdr
          WHERE routing_id = v_routing_id;

      CURSOR class_process_loss_cursor (v_routing_class VARCHAR2, v_qty NUMBER)
      IS
         SELECT   process_loss
             FROM gmd_process_loss
            WHERE v_qty <= NVL (max_quantity, v_qty)
              AND routing_class = v_routing_class
         ORDER BY max_quantity;

      --Bug#5618961 cursor modified to select std qty, uom and inventory item id
      CURSOR validity_process_loss_cursor (v_recipe_validity_rule_id NUMBER)
      IS
         SELECT planned_process_loss, std_qty, detail_uom, inventory_item_id    	
           FROM gmd_recipe_validity_rules
          WHERE recipe_validity_rule_id = NVL (v_recipe_validity_rule_id, -1);
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      l_total_output_qty_pre_scale := p_total_output_qty_pre_scale;
      l_total_output_qty_scaled := p_total_output_qty_scaled;

      IF NVL (g_debug, -1) = gme_debug.g_log_statement THEN
         gme_debug.put_line (l_api_name || ': Parameters');
         gme_debug.put_line ('p_batch_id:' || p_batch_id);
         gme_debug.put_line ('p_validity_rule_id:' || p_validity_rule_id);
         gme_debug.put_line ('p_organization_id:' || p_organization_id);
         gme_debug.put_line (   'p__total_output_qty_scaled:'
                             || p_total_output_qty_scaled);
         gme_debug.put_line (   'p__total_output_qty_pre_scale:'
                             || p_total_output_qty_pre_scale);
      END IF;

      /* Use and validate the batch id if it is passed in. */
      IF (p_batch_id IS NOT NULL) THEN
         OPEN cur_get_info (p_batch_id);

         FETCH cur_get_info
          INTO l_validity_rule_id, l_recipe_id, l_plant_code, l_routing_id
              ,l_formula_id;

         CLOSE cur_get_info;

         IF l_recipe_id IS NULL THEN
            gme_common_pvt.log_message ('GME_INVALID_BATCH');
            RAISE expected_error;
         END IF;
      ELSIF p_validity_rule_id IS NOT NULL THEN
         -- Validate recipe validity rule
         l_validity_rule_id := p_validity_rule_id;

         OPEN cur_get_info_from_validity (p_validity_rule_id);

         FETCH cur_get_info_from_validity
          INTO l_recipe_id, l_routing_id, l_formula_id;

         CLOSE cur_get_info_from_validity;

         IF l_recipe_id IS NULL THEN
            gme_common_pvt.log_message ('GME_API_INVALID_RULE');
            RAISE expected_error;
         END IF;
      END IF;

      /* Fetch the routing class for the given routing id. */
      IF (l_routing_id IS NOT NULL) THEN
         OPEN cur_get_rtclass (l_routing_id);
         --Bug#5618961 fetching l_routing_class_uom too
         FETCH cur_get_rtclass
          INTO l_routing_class, l_routing_uom, l_routing_class_uom, l_routing_process_loss, l_routing_qty;         

         CLOSE cur_get_rtclass;
      ELSE
         IF NVL (g_debug, -1) = gme_debug.g_log_statement THEN
            gme_debug.put_line (l_api_name || ': No routing defined');
         END IF;
      END IF;

      -- Determine the process loss.
      l_process_loss_a := NULL;

      --Pawan kumar added this code for bug 2473858  for validity rule project
      OPEN validity_process_loss_cursor (l_validity_rule_id);

      FETCH validity_process_loss_cursor
       INTO l_process_loss_a, l_qty, l_uom, l_item_id;

      CLOSE validity_process_loss_cursor;

      IF l_process_loss_a IS NULL THEN
         IF NVL (g_debug, -1) = gme_debug.g_log_statement THEN
            gme_debug.put_line (   l_api_name
                                || ': No process_loss at validity level');
         END IF;

         --Bug#5618961 initializing the variables if we did not find process loss VR level
         l_uom := l_routing_uom;
         l_qty := NULL;
         l_item_id := 0;

         OPEN orgn_process_loss_cursor (l_recipe_id, l_plant_code);

         FETCH orgn_process_loss_cursor
          INTO l_process_loss_a;

         CLOSE orgn_process_loss_cursor;

         IF l_process_loss_a IS NULL THEN
            IF NVL (g_debug, -1) = gme_debug.g_log_statement THEN
               gme_debug.put_line (   l_api_name
                                   || ': No process_loss at orgn level');
            END IF;

            OPEN recipe_process_loss_cursor (l_recipe_id);

            FETCH recipe_process_loss_cursor
             INTO l_process_loss_a;

            CLOSE recipe_process_loss_cursor;

            IF l_process_loss_a IS NULL THEN
               IF NVL (g_debug, -1) = gme_debug.g_log_statement THEN
                  gme_debug.put_line (   l_api_name
                                      || 'No process_loss at recipe level');
               END IF;

               --Bug#5618961 Begin commented the following cursor code as we already fetch the values
               /*OPEN routing_process_loss_cursor (l_routing_id);

               FETCH routing_process_loss_cursor
                INTO l_process_loss_a;

               CLOSE routing_process_loss_cursor; */

               l_process_loss_a := NVL (l_routing_process_loss, 0);
               l_qty := l_routing_qty;
               l_uom := l_routing_uom;
               --Bug#5618961 End
            END IF;                /* l_process_loss_a IS NULL @recipe level*/
         END IF;                     /* l_process_loss_a IS NULL @orgn level*/
      END IF;               /* l_process_loss_a IS NULL @validity rule level*/

      -- Initialize l_process_loss here one time. It will have a value even if it is zero.
      l_process_loss := l_process_loss_a;

      --Bug#5618961 Begin assigning the actual qty tht has to be considered
      IF l_qty IS NOT NULL THEN
        IF NVL (g_debug, -1) = gme_debug.g_log_statement THEN
          gme_debug.put_line(l_api_name|| 'Qty to be considered for theoretical process loss'|| l_qty);
        END IF;
        l_total_output_qty_pre_scale := l_qty;
      END IF;

      IF (l_routing_id IS NOT NULL AND l_process_loss > 0 AND 
          l_routing_class IS NOT NULL) THEN
        
        IF l_uom <> l_routing_class_uom THEN
           l_qty :=  inv_convert.inv_um_convert
                                    (item_id            => l_item_id
                                    ,PRECISION          => gme_common_pvt.g_precision
                                    ,from_quantity      => l_total_output_qty_pre_scale
                                    ,from_unit          => l_uom
                                    ,to_unit            => l_routing_class_uom
                                    ,from_name          => NULL
                                    ,to_name            => NULL);
           IF l_qty < 0 THEN
             IF NVL (g_debug, -1) = gme_debug.g_log_statement THEN
                gme_debug.put_line(   l_api_name
                    || 'No process_loss at routing class level for output B'
                    || l_total_output_qty_pre_scale);
             END IF;
           ELSE
             --assign the qty to pre scale i.e in routing class uom
             l_total_output_qty_pre_scale := l_qty;
             IF NVL (g_debug, -1) = gme_debug.g_log_statement THEN
              gme_debug.put_line(l_api_name|| 'Before scaled qty after converting to routing calss uom'|| l_qty);
             END IF;
           END IF; /*l_qty < 0 */
        END IF; /*l_uom <> l_routing_class_uom*/

        IF l_routing_uom <> l_routing_class_uom THEN
           l_qty :=  inv_convert.inv_um_convert
                                    (item_id            => 0
                                    ,PRECISION          => gme_common_pvt.g_precision
                                    ,from_quantity      => l_total_output_qty_scaled
                                    ,from_unit          => l_routing_uom
                                    ,to_unit            => l_routing_class_uom
                                    ,from_name          => NULL
                                    ,to_name            => NULL);
           IF l_qty < 0 THEN
             IF NVL (g_debug, -1) = gme_debug.g_log_statement THEN
                gme_debug.put_line(   l_api_name
                    || 'No process_loss at routing class level for output B'
                    || p_total_output_qty_scaled);
             END IF;
           ELSE
             --assign the qty to pre scale i.e in routing class uom
             l_total_output_qty_scaled := l_qty;
             IF NVL (g_debug, -1) = gme_debug.g_log_statement THEN
              gme_debug.put_line(l_api_name|| 'after scaled Qty after converting to routing calss uom'|| l_qty);
             END IF;
           END IF; /*l_qty < 0 */
        END IF; /*l_uom <> l_routing_class_uom*/

      END IF;  /*l_routing_id IS NOT NUL */

      --Bug#5618961 End

      -- The following IF condition will override l_process_loss only when necessary.
      IF (l_routing_id IS NOT NULL) AND l_process_loss > 0 THEN
         IF l_routing_class IS NOT NULL THEN
            l_process_loss_b := NULL;

            OPEN class_process_loss_cursor
                                          (l_routing_class
                                          ,NVL (l_total_output_qty_scaled
                                               ,l_total_output_qty_pre_scale) );

            FETCH class_process_loss_cursor
             INTO l_process_loss_b;

            CLOSE class_process_loss_cursor;

            IF NVL (l_process_loss_b, 0) = 0 THEN
               IF NVL (g_debug, -1) = gme_debug.g_log_statement THEN
                  gme_debug.put_line
                     (   l_api_name
                      || 'No process_loss at routing class level for output B'
                      || p_total_output_qty_scaled);
               END IF;
            ELSE
               l_process_loss_c := NULL;

               OPEN class_process_loss_cursor (l_routing_class
                                              ,l_total_output_qty_pre_scale);

               FETCH class_process_loss_cursor
                INTO l_process_loss_c;

               CLOSE class_process_loss_cursor;

               IF NVL (l_process_loss_c, 0) = 0 THEN
                  IF NVL (g_debug, -1) = gme_debug.g_log_statement THEN
                     gme_debug.put_line
                        (   l_api_name
                         || 'No process_loss at routing class level for output C'
                         || p_total_output_qty_pre_scale);
                  END IF;
               ELSE
                  /* If it makes it here then it found a loss for both the pre and post scale quantities. */
                  l_process_loss :=
                         l_process_loss * l_process_loss_b / l_process_loss_c;

                  IF NVL (g_debug, -1) = gme_debug.g_log_statement THEN
                     gme_debug.put_line (   l_api_name
                                         || 'Process_loss a'
                                         || l_process_loss_a
                                         || 'Process_loss b'
                                         || l_process_loss_b
                                         || 'Process_loss c'
                                         || l_process_loss_c);
                  END IF;
               END IF;                     /* NVL (l_process_loss_c, 0) = 0 */
            END IF;                        /* NVL (l_process_loss_b, 0) = 0 */
         END IF;                             /* l_routing_class IS NOT NULL */
      END IF;          /* (l_routing_id IS NOT NULL AND l_process_loss > 0) */

      IF NVL (g_debug, -1) = gme_debug.g_log_statement THEN
         gme_debug.put_line ('Process Loss is ' || l_process_loss);
      END IF;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;

      RETURN (l_process_loss);
   EXCEPTION
      WHEN expected_error THEN
         RETURN (NULL);
      WHEN OTHERS THEN
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);

         IF g_debug <= gme_debug.g_log_unexpected THEN
            gme_debug.put_line (   'When others exception in '
                                || g_pkg_name
                                || '.'
                                || l_api_name
                                || ' Error is '
                                || SQLERRM);
         END IF;

         RETURN (NULL);
   END get_process_loss;

   PROCEDURE create_document_no (
      p_batch_header_rec   IN              gme_batch_header%ROWTYPE
     ,x_batch_header_rec   IN OUT NOCOPY   gme_batch_header%ROWTYPE)
   IS
      l_assignment_type     NUMBER;
      l_document_no         gme_batch_header.batch_no%TYPE;
      invalid_doc_no        EXCEPTION;
      l_api_name   CONSTANT VARCHAR2 (30)             := 'create_document_no';
      PRAGMA       AUTONOMOUS_TRANSACTION;
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      x_batch_header_rec := p_batch_header_rec;

      IF (x_batch_header_rec.batch_type = gme_common_pvt.g_doc_type_batch) THEN
         SELECT        batch_doc_numbering, batch_no_last_assigned + 1
                  INTO l_assignment_type, l_document_no
                  FROM gme_parameters
                 WHERE organization_id = x_batch_header_rec.organization_id
         FOR UPDATE OF batch_no_last_assigned NOWAIT;
      ELSE
         SELECT        fpo_doc_numbering, fpo_no_last_assigned + 1
                  INTO l_assignment_type, l_document_no
                  FROM gme_parameters
                 WHERE organization_id = x_batch_header_rec.organization_id
         FOR UPDATE OF batch_no_last_assigned NOWAIT;
      END IF;

      IF l_assignment_type = gme_common_pvt.g_auto_doc_numbering THEN
         IF l_document_no IS NULL THEN
            RAISE invalid_doc_no;
         END IF;

         IF (x_batch_header_rec.batch_type = gme_common_pvt.g_doc_type_batch) THEN
            UPDATE gme_parameters
               SET batch_no_last_assigned = batch_no_last_assigned + 1
             WHERE organization_id = p_batch_header_rec.organization_id;
         ELSE
            UPDATE gme_parameters
               SET fpo_no_last_assigned = fpo_no_last_assigned + 1
             WHERE organization_id = p_batch_header_rec.organization_id;
         END IF;

         x_batch_header_rec.batch_no := l_document_no;
         COMMIT;
      ELSE
      	 ROLLBACK;
      END IF;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;
   EXCEPTION
      WHEN invalid_doc_no THEN
         x_batch_header_rec.batch_no := NULL;
         fnd_message.set_name ('GME', 'GME_INVALID_AUTO_DOC');
         fnd_msg_pub.ADD;
      	 ROLLBACK;
      WHEN app_exception.record_lock_exception THEN
         gme_common_pvt.log_message ('GME_DOC_SEQ_LOCK'
                                    ,'S1'
                                    ,p_batch_header_rec.organization_id);
      	 ROLLBACK;
      WHEN NO_DATA_FOUND THEN
         x_batch_header_rec.batch_no := NULL;
         fnd_message.set_name ('GMA', 'SY_NODOCSEQREC');
         fnd_msg_pub.ADD;
      	 ROLLBACK;
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
         x_batch_header_rec.batch_no := NULL;
      	 ROLLBACK;
   END create_document_no;

   FUNCTION calc_date_from_prod_rule (
      p_organization_id     IN              NUMBER
     ,p_inventory_item_id   IN              NUMBER
     ,p_item_qty            IN              NUMBER
     ,p_start_date          IN              DATE
     ,p_cmplt_date          IN              DATE
     ,x_start_date          IN OUT NOCOPY   DATE
     ,x_cmplt_date          IN OUT NOCOPY   DATE)
      RETURN BOOLEAN
   IS
      -- Bug 13721430 - Add std lot size.   
      CURSOR cur_common_rules (v_org_id NUMBER, v_inventory_item_id NUMBER)
      IS
         SELECT std_lot_size, fixed_lead_time, variable_lead_time
           FROM mtl_system_items_b
          WHERE organization_id = v_org_id
            AND inventory_item_id = v_inventory_item_id;

      l_rule_rec            cur_common_rules%ROWTYPE;
      l_total_leadtime      NUMBER;
      l_api_name   CONSTANT VARCHAR2 (30)        := 'calc_date_from_prod_rule';
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      OPEN cur_common_rules (p_organization_id, p_inventory_item_id);

      FETCH cur_common_rules
       INTO l_rule_rec;

      CLOSE cur_common_rules;

      IF (    NVL (l_rule_rec.fixed_lead_time, 0) = 0
          AND NVL (l_rule_rec.variable_lead_time, 0) = 0) THEN
         RETURN FALSE;
      END IF;

      -- Bug 13721430 - Add std lot size to calculation. p_item_qty is already passed
      -- in using primary uom and std lot size is considered to be in primary uom.
      l_total_leadtime := ROUND (NVL(l_rule_rec.fixed_lead_time, 0)
                          + (NVL(l_rule_rec.variable_lead_time, 0) * p_item_qty / NVL(l_rule_rec.std_lot_size, 1)) ,5);

      -- l_total_leadtime :=
         -- ROUND (  NVL (l_rule_rec.fixed_lead_time, 0)
         --        + NVL (l_rule_rec.variable_lead_time, 0) * p_item_qty
         --       ,5);
         
--      l_total_leadtime := ROUND (l_total_leadtime / 24, 5); /*Bug#7385805 */

      IF p_start_date IS NOT NULL THEN
         x_cmplt_date := p_start_date + l_total_leadtime;
         x_start_date := p_start_date;
      ELSIF p_cmplt_date IS NOT NULL THEN
         x_start_date := p_cmplt_date - l_total_leadtime;
         x_cmplt_date := p_cmplt_date;
      ELSE
         -- if you get here, that means that a rule was found, but no dates were passed in...
         -- let's default start date to sysdate and work in the prod rule...
         x_start_date := gme_common_pvt.g_timestamp;
         x_cmplt_date := x_start_date + l_total_leadtime;
      END IF;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;

      RETURN TRUE;
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
         RETURN FALSE;
   END calc_date_from_prod_rule;

   PROCEDURE calc_mtl_req_date (
      p_batch_header_rec   IN              gme_batch_header%ROWTYPE
            DEFAULT NULL
     ,p_batchstep_rec      IN              gme_batch_steps%ROWTYPE
            DEFAULT NULL
     ,p_mtl_dtl_rec        IN              gme_material_details%ROWTYPE
     ,x_mtl_req_date       OUT NOCOPY      DATE
     ,x_return_status      OUT NOCOPY      VARCHAR2)
   IS
      l_batchstep_id        NUMBER;
      l_step_start_date     DATE;
      l_step_cmplt_date     DATE;
      l_batch_start_date    DATE;
      l_batch_cmplt_date    DATE;
      l_api_name   CONSTANT VARCHAR2 (30) := 'calc_mtl_req_date';

      CURSOR cur_get_associated_step (v_material_detail_id NUMBER)
      IS
         SELECT batchstep_id
           FROM gme_batch_step_items
          WHERE material_detail_id = v_material_detail_id;

      CURSOR cur_fetch_step_dates (v_batchstep_id NUMBER)
      IS
         SELECT plan_start_date, plan_cmplt_date
           FROM gme_batch_steps
          WHERE batchstep_id = v_batchstep_id;

      CURSOR cur_fetch_batch_dates (v_batch_id NUMBER)
      IS
         SELECT plan_start_date, plan_cmplt_date
           FROM gme_batch_header
          WHERE batch_id = v_batch_id;
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      x_return_status := fnd_api.g_ret_sts_success;

      IF p_mtl_dtl_rec.release_type IN
            (gme_common_pvt.g_mtl_manual_release
            ,gme_common_pvt.g_mtl_incremental_release
            ,gme_common_pvt.g_mtl_autobystep_release) THEN
         IF (p_batchstep_rec.batchstep_id IS NULL) THEN
            OPEN cur_get_associated_step (p_mtl_dtl_rec.material_detail_id);

            FETCH cur_get_associated_step
             INTO l_batchstep_id;

            CLOSE cur_get_associated_step;
         ELSE
            l_batchstep_id := p_batchstep_rec.batchstep_id;
         END IF;
      END IF;

      IF l_batchstep_id IS NOT NULL THEN
         IF (   p_batchstep_rec.plan_start_date IS NULL
             OR p_batchstep_rec.plan_cmplt_date IS NULL) THEN
            OPEN cur_fetch_step_dates (l_batchstep_id);

            FETCH cur_fetch_step_dates
             INTO l_step_start_date, l_step_cmplt_date;

            CLOSE cur_fetch_step_dates;
         ELSE
            l_step_start_date := p_batchstep_rec.plan_start_date;
            l_step_cmplt_date := p_batchstep_rec.plan_cmplt_date;
         END IF;

         IF p_mtl_dtl_rec.line_type = gme_common_pvt.g_line_type_ing THEN
            x_mtl_req_date := l_step_start_date;
         ELSE
            x_mtl_req_date := l_step_cmplt_date;
         END IF;
      ELSE
         IF (   p_batch_header_rec.plan_start_date IS NULL
             OR p_batch_header_rec.plan_cmplt_date IS NULL) THEN
            OPEN cur_fetch_batch_dates (p_mtl_dtl_rec.batch_id);

            FETCH cur_fetch_batch_dates
             INTO l_batch_start_date, l_batch_cmplt_date;

            CLOSE cur_fetch_batch_dates;
         ELSE
            l_batch_start_date := p_batch_header_rec.plan_start_date;
            l_batch_cmplt_date := p_batch_header_rec.plan_cmplt_date;
         END IF;

         IF p_mtl_dtl_rec.line_type = gme_common_pvt.g_line_type_ing THEN
            x_mtl_req_date := l_batch_start_date;
         ELSE
            x_mtl_req_date := l_batch_cmplt_date;
         END IF;
      END IF;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
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

         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
         x_return_status := fnd_api.g_ret_sts_unexp_error;
   END calc_mtl_req_date;

   FUNCTION is_qty_below_capacity (
      p_batch_step_resources_rec   IN   gme_batch_step_resources%ROWTYPE)
      RETURN BOOLEAN
   IS
      CURSOR cur_get_um_type (v_uom_code VARCHAR2)
      IS
         SELECT b.uom_class, a.conversion_rate
           FROM mtl_uom_conversions a, mtl_units_of_measure b
          WHERE a.uom_code = b.uom_code
            AND a.inventory_item_id = 0
            AND b.uom_code = v_uom_code;

      CURSOR cur_get_activity_factor (v_batchstep_activity_id NUMBER)
      IS
         SELECT plan_activity_factor, actual_activity_factor
           FROM gme_batch_step_activities
          WHERE batchstep_activity_id = v_batchstep_activity_id;

      l_resource_qty             NUMBER;
      l_min_capacity             NUMBER;
      l_cap_um_type              sy_uoms_typ.um_type%TYPE;
      l_qty_um_type              sy_uoms_typ.um_type%TYPE;
      l_cap_std_factor           NUMBER;
      l_qty_std_factor           NUMBER;
      l_plan_activity_factor     NUMBER;
      l_actual_activity_factor   NUMBER;
      l_api_name        CONSTANT VARCHAR2 (30)      := 'is_qty_below_capacity';
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      /* If capacities are not defined for the resource then we have nothing to check against so return true */
      IF    (p_batch_step_resources_rec.min_capacity IS NULL)
         OR (p_batch_step_resources_rec.capacity_um IS NULL) THEN
         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line
                             (' Resource min capacity or capacity um is null');
            RETURN FALSE;
         END IF;
      END IF;

      IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
         gme_debug.put_line (   ' Resource min capacity:'
                             || p_batch_step_resources_rec.min_capacity
                             || ' Capacity UM:'
                             || p_batch_step_resources_rec.capacity_um
                             || ' Resource UOM:'
                             || p_batch_step_resources_rec.resource_qty_um);
      END IF;

      /* Let us also get the activity factors as the resource */
      /* quantities include the activity factor in them       */
      OPEN cur_get_activity_factor
                             (p_batch_step_resources_rec.batchstep_activity_id);

      FETCH cur_get_activity_factor
       INTO l_plan_activity_factor, l_actual_activity_factor;

      CLOSE cur_get_activity_factor;

      IF l_actual_activity_factor IS NULL THEN
         l_actual_activity_factor := l_plan_activity_factor;
      END IF;

      /* Let us determine the quantity to check against the min capacity */
      IF NVL (p_batch_step_resources_rec.actual_rsrc_qty, -1) <> -1 THEN
         IF l_actual_activity_factor > 0 THEN
            l_resource_qty :=
                 p_batch_step_resources_rec.actual_rsrc_qty
               / l_actual_activity_factor;

            IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
               gme_debug.put_line (   ' Resource  Qty by Actual:'
                                   || l_resource_qty
                                   || ' Activity Factor:'
                                   || l_actual_activity_factor);
            END IF;
         ELSE
            /* If the activitiy is not being used then we need not check the resource */
            RETURN FALSE;
         END IF;
      ELSE
         IF l_plan_activity_factor > 0 THEN
            l_resource_qty :=
                 p_batch_step_resources_rec.plan_rsrc_qty
               / l_plan_activity_factor;

            IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
               gme_debug.put_line (   ' Resource  Qty by Plan:'
                                   || l_resource_qty
                                   || ' Activity Factor:'
                                   || l_plan_activity_factor);
            END IF;
         ELSE
            /* If the activitiy is not being used then we need not check the resource */
            RETURN FALSE;
         END IF;
      END IF;
            /* IF NVL(p_batch_step_resources_rec.actual_rsrc_qty, -1) <> -1 */
    /* If the uoms are the same then we can do a direct comparison */

      IF p_batch_step_resources_rec.capacity_um =
                                    p_batch_step_resources_rec.resource_qty_um THEN
         IF l_resource_qty < p_batch_step_resources_rec.min_capacity THEN
            RETURN TRUE;
         ELSE
            RETURN FALSE;
         END IF;
             /* IF l_resource_qty < p_batch_step_resources_rec.min_capacity */
      ELSE
         /* Lets convert the capacity to the resource qty uom */
         /* Since their is no item associated we cannot do a conversion between */
         /* inter um types. let us first fetch the um types to determine that   */
         OPEN cur_get_um_type (p_batch_step_resources_rec.capacity_um);

         FETCH cur_get_um_type
          INTO l_cap_um_type, l_cap_std_factor;

         CLOSE cur_get_um_type;

         OPEN cur_get_um_type (p_batch_step_resources_rec.resource_qty_um);

         FETCH cur_get_um_type
          INTO l_qty_um_type, l_qty_std_factor;

         CLOSE cur_get_um_type;

         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line (   ' Capacity uom type:'
                                || l_cap_um_type
                                || ' Std Factor:'
                                || l_cap_std_factor
                                || ' Qty uom type:'
                                || l_qty_um_type
                                || ' Qty Std Factor:'
                                || l_qty_std_factor);
         END IF;

         /* We have everything - let us check now */
         IF l_cap_um_type <> l_qty_um_type THEN
            RETURN FALSE;
         ELSE
            l_min_capacity :=
                 p_batch_step_resources_rec.min_capacity
               * (l_cap_std_factor / l_qty_std_factor);

            IF l_resource_qty < l_min_capacity THEN
               RETURN TRUE;
            ELSE
               RETURN FALSE;
            END IF;                   /* IF l_resource_qty < l_min_capacity */
         END IF;                       /* IF l_cap_um_type <> l_qty_um_type */
      END IF;
/* IF p_batch_step_resources_rec.capacity_uom = p_batch_step_resources_rec.process_qty_um */

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
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

         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
         RETURN FALSE;
   END is_qty_below_capacity;

   FUNCTION resource_qty_below_capacity (p_batch_id IN NUMBER)
      RETURN BOOLEAN
   IS
      CURSOR cur_get_resources_old
      IS
         SELECT batchstep_resource_id
           FROM gme_batch_step_resources
          WHERE batch_id = p_batch_id;

      -- Bug 13946067 - Introduce new table and joins to make use of existing index.
      CURSOR cur_get_resources
      IS
         SELECT batchstep_resource_id
           FROM gme_batch_step_resources r, gme_batch_steps s
          WHERE r.batch_id = p_batch_id
            AND s.batch_id = p_batch_id
            AND s.batchstep_id = r.batchstep_id;

      l_resource_ids           gme_common_pvt.number_tab;
      l_batch_step_resources   gme_batch_step_resources%ROWTYPE;
      l_found                  NUMBER (5)                         DEFAULT 0;
      l_resources              VARCHAR2 (2000);
      l_api_name      CONSTANT VARCHAR2 (30) := 'resource_qty_below_capacity';
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      OPEN cur_get_resources;

      FETCH cur_get_resources
      BULK COLLECT INTO l_resource_ids;

      CLOSE cur_get_resources;

      FOR i IN 1 .. l_resource_ids.COUNT LOOP
         l_batch_step_resources.batchstep_resource_id := l_resource_ids (i);

         IF gme_batch_step_resources_dbl.fetch_row
                           (p_batch_step_resources      => l_batch_step_resources
                           ,x_batch_step_resources      => l_batch_step_resources) THEN
            IF is_qty_below_capacity
                        (p_batch_step_resources_rec      => l_batch_step_resources) THEN
               l_found := 1;

               IF l_resources IS NULL THEN
                  l_resources := l_batch_step_resources.resources;
               ELSE
                  l_resources :=
                       l_resources || ',' || l_batch_step_resources.resources;
               END IF;                            /* IF l_resources IS NULL */
            END IF;
/* IF is_qty_below_capacity (p_batch_step_resources => l_batch_step_resources) */
         END IF;
/* IF GME_BATCH_STEP_RESOURCES_DBL.fetch_row (p_batch_step_resources => l_batch_step_resources */
      END LOOP;
     /* FOR i IN 1..l_resource_ids.COUNT */
/* If we have found atleast one resource falling below capacity then return true */

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;

      IF l_found = 1 THEN
         gme_common_pvt.log_message ('GME_API_RSRC_QTY_BELOW_CAP'
                                    ,'RESOURCES'
                                    ,l_resources);
         RETURN TRUE;
      ELSE
         RETURN FALSE;
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

         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
         RETURN FALSE;
   END resource_qty_below_capacity;

   FUNCTION eff_locator_control (
      p_organization_id     IN   NUMBER
     ,p_subinventory        IN   VARCHAR2 DEFAULT NULL
     ,p_inventory_item_id   IN   NUMBER DEFAULT NULL
     ,p_org_control         IN   NUMBER DEFAULT NULL
     ,p_sub_control         IN   NUMBER DEFAULT NULL
     ,p_item_control        IN   NUMBER DEFAULT NULL
     ,p_item_loc_restrict   IN   NUMBER DEFAULT NULL
     ,p_org_neg_allowed     IN   NUMBER DEFAULT NULL
     ,p_action              IN   NUMBER)
      RETURN NUMBER
   IS
      l_org_control                    NUMBER;
      l_org_neg_allowed                NUMBER;
      l_sub_control                    NUMBER;
      l_item_control                   NUMBER;
      l_item_loc_restrict              NUMBER;
      l_eff_control                    NUMBER;
      l_return_status                  VARCHAR2 (10);
      l_msg_count                      NUMBER;
      l_msg_data                       VARCHAR2 (2000);
      l_api_name              CONSTANT VARCHAR2 (30) := 'eff_locator_control';

      CURSOR cur_org_details (v_org_id NUMBER)
      IS
         SELECT negative_inv_receipt_code, stock_locator_control_code
           FROM mtl_parameters
          WHERE organization_id = v_org_id;

      CURSOR cur_subinventory_details (
         v_org_id         NUMBER
        ,v_subinventory   VARCHAR2)
      IS
         SELECT NVL (locator_type, 1)
           FROM mtl_secondary_inventories
          WHERE organization_id = v_org_id
            AND secondary_inventory_name = v_subinventory;
      /* Bug 5441643 Added NVL condition for location control code*/
      CURSOR cur_item_details (v_org_id NUMBER, v_inventory_item_id NUMBER)
      IS
         SELECT NVL(location_control_code,1), restrict_locators_code
           FROM mtl_system_items_b
          WHERE organization_id = v_org_id
            AND inventory_item_id = v_inventory_item_id;

      org_info_not_provided            EXCEPTION;
      subinventory_info_not_provided   EXCEPTION;
      item_info_not_provided           EXCEPTION;
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      IF (p_org_control IS NULL OR p_org_neg_allowed IS NULL) THEN
         IF (p_organization_id IS NOT NULL) THEN
            OPEN cur_org_details (p_organization_id);

            FETCH cur_org_details
             INTO l_org_neg_allowed, l_org_control;

            CLOSE cur_org_details;
         ELSE
            RAISE org_info_not_provided;
         END IF;
      ELSE
         l_org_neg_allowed := p_org_neg_allowed;
         l_org_control := p_org_control;
      END IF;

      IF (p_sub_control IS NULL) THEN
         IF (p_subinventory IS NOT NULL) THEN
            OPEN cur_subinventory_details (p_organization_id, p_subinventory);

            FETCH cur_subinventory_details
             INTO l_sub_control;

            CLOSE cur_subinventory_details;
         ELSE
            RAISE subinventory_info_not_provided;
         END IF;
      ELSE
         l_sub_control := p_sub_control;
      END IF;

      IF (p_item_control IS NULL OR p_item_loc_restrict IS NULL) THEN
         IF (p_inventory_item_id IS NOT NULL) THEN
            OPEN cur_item_details (p_organization_id, p_inventory_item_id);

            FETCH cur_item_details
             INTO l_item_control, l_item_loc_restrict;

            CLOSE cur_item_details;
         ELSE
            RAISE item_info_not_provided;
         END IF;
      ELSE
         l_item_control := p_item_control;
         l_item_loc_restrict := p_item_loc_restrict;
      END IF;

      l_eff_control :=
         inv_globals.locator_control
                                  (x_return_status          => l_return_status
                                  ,x_msg_count              => l_msg_count
                                  ,x_msg_data               => l_msg_data
                                  ,p_org_control            => l_org_control
                                  ,p_sub_control            => l_sub_control
                                  ,p_item_control           => l_item_control
                                  ,p_item_loc_restrict      => l_item_loc_restrict
                                  ,p_org_neg_allowed        => l_org_neg_allowed
                                  ,p_action                 => p_action);

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;

      RETURN l_eff_control;
   EXCEPTION
      WHEN org_info_not_provided THEN
         RETURN -1;
      WHEN subinventory_info_not_provided THEN
         RETURN -1;
      WHEN item_info_not_provided THEN
         RETURN -1;
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
         RETURN -1;
   END eff_locator_control;

   FUNCTION check_locator (
      p_organization_id     IN   NUMBER
     ,p_locator_id          IN   NUMBER
     ,p_subinventory        IN   VARCHAR2
     ,p_inventory_item_id   IN   NUMBER
     ,p_org_control         IN   NUMBER
     ,p_sub_control         IN   NUMBER
     ,p_item_control        IN   NUMBER
     ,p_item_loc_restrict   IN   NUMBER
     ,p_org_neg_allowed     IN   NUMBER
     ,p_txn_action_id       IN   NUMBER)
      RETURN BOOLEAN
   IS
      l_api_name   CONSTANT VARCHAR2 (30)        := 'check_locator';
      l_locator             inv_validate.LOCATOR;
      l_org                 inv_validate.org;
      l_item                inv_validate.item;
      l_sub                 inv_validate.sub;
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      IF g_debug <= gme_debug.g_log_statement THEN
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || 'p_organization_id '
                             || p_organization_id);
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || 'p_locator_id '
                             || p_locator_id);
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || 'p_subinventory '
                             || p_subinventory);
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || 'p_inventory_item_id '
                             || p_inventory_item_id);
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || 'p_txn_action_id '
                             || p_txn_action_id);
      END IF;

      l_locator.inventory_location_id := p_locator_id;
      l_org.organization_id := p_organization_id;
      l_org.stock_locator_control_code := p_org_control;
      l_org.negative_inv_receipt_code := p_org_neg_allowed;
      l_item.inventory_item_id := p_inventory_item_id;
      /* Bug 5441643 Added NVL condition for location control code*/
      l_item.location_control_code := NVL(p_item_control,1);
      l_item.restrict_locators_code := p_item_loc_restrict;
      l_sub.secondary_inventory_name := p_subinventory;
      l_sub.locator_type := p_sub_control;

      IF (inv_validate.check_locator (p_locator              => l_locator
                                     ,p_org                  => l_org
                                     ,p_item                 => l_item
                                     ,p_sub                  => l_sub
                                     ,p_project_id           => NULL
                                     ,p_task_id              => NULL
                                     ,p_txn_action_id        => p_txn_action_id
                                     ,p_is_from_locator      => NULL
                                     ,p_dynamic_ok           => TRUE) =
                                                                inv_validate.f) THEN
         fnd_message.set_name ('INV', 'INV_INVALID_LOCATION');
         fnd_msg_pub.ADD;

         IF g_debug <= gme_debug.g_log_statement THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ' Locator IS invalid');
         END IF;

         RETURN FALSE;
      END IF;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;

      RETURN TRUE;
   EXCEPTION
      WHEN OTHERS THEN
         IF g_debug <= gme_debug.g_log_unexpected THEN
            gme_debug.put_line (   'WHEN OTHERS EXCEPTION IN '
                                || g_pkg_name
                                || '.'
                                || l_api_name
                                || ' Error IS '
                                || SQLERRM);
         END IF;

         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
         RETURN FALSE;
   END check_locator;

   FUNCTION check_subinventory (
      p_organization_id     IN   NUMBER
     ,p_subinventory        IN   VARCHAR2
     ,p_inventory_item_id   IN   NUMBER
     ,p_restrict_subinv     IN   NUMBER DEFAULT NULL)
      RETURN BOOLEAN
   IS
      l_api_name     CONSTANT VARCHAR2 (30)     := 'check_subinventory';
      l_org                   inv_validate.org;
      l_item                  inv_validate.item;
      l_sub                   inv_validate.sub;

      CURSOR cur_item_control
      IS
         SELECT restrict_subinventories_code
           FROM mtl_system_items_b
          WHERE organization_id = p_organization_id
            AND inventory_item_id = p_inventory_item_id;

      l_item_restict_subinv   NUMBER;
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      IF g_debug <= gme_debug.g_log_statement THEN
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || 'p_organization_id '
                             || p_organization_id);
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || 'p_subinventory '
                             || p_subinventory);
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || 'p_inventory_item_id '
                             || p_inventory_item_id);
      END IF;

      l_org.organization_id := p_organization_id;
      l_item.inventory_item_id := p_inventory_item_id;
      l_sub.secondary_inventory_name := p_subinventory;

      IF (p_restrict_subinv IS NULL) THEN
         OPEN cur_item_control;

         FETCH cur_item_control
          INTO l_item_restict_subinv;

         CLOSE cur_item_control;
      ELSE
         l_item_restict_subinv := p_restrict_subinv;
      END IF;

      IF (l_item_restict_subinv = 1) THEN
         IF (inv_validate.subinventory (p_org       => l_org
                                       ,p_item      => l_item
                                       ,p_sub       => l_sub) = inv_validate.f) THEN
            IF g_debug <= gme_debug.g_log_statement THEN
               gme_debug.put_line (   g_pkg_name
                                   || '.'
                                   || l_api_name
                                   || ' Subinventory is invalid');
            END IF;

            RETURN FALSE;
         END IF;
      ELSE
         IF (inv_validate.subinventory (p_org => l_org, p_sub => l_sub) =
                                                                inv_validate.f) THEN
            IF g_debug <= gme_debug.g_log_statement THEN
               gme_debug.put_line (   g_pkg_name
                                   || '.'
                                   || l_api_name
                                   || ' Subinventory is invalid');
            END IF;

            RETURN FALSE;
         END IF;
      END IF;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;

      RETURN TRUE;
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
         RETURN FALSE;
   END check_subinventory;

   FUNCTION validate_validity_rule (
      p_validity_rule_id   IN   NUMBER
     ,p_organization_id    IN   NUMBER
     ,p_prim_product_id    IN   NUMBER
     ,p_qty                IN   NUMBER
     ,p_uom                IN   VARCHAR2
     ,p_object_type        IN   VARCHAR2
     ,p_start_date         IN   DATE
     ,p_cmplt_date         IN   DATE
     ,p_creation_mode      IN   VARCHAR2 DEFAULT NULL)
      RETURN BOOLEAN
   IS
      CURSOR get_validity_rule (v_validity_rule_id NUMBER)
      IS
         SELECT *
           FROM gmd_recipe_validity_rules
          WHERE recipe_validity_rule_id = v_validity_rule_id;

      CURSOR cur_item_no (v_org_id NUMBER, v_inventory_item_id NUMBER)
      IS
         SELECT concatenated_segments
           FROM mtl_system_items_kfv
          WHERE organization_id = v_org_id
            AND inventory_item_id = v_inventory_item_id;

      l_validity_rec           gmd_recipe_validity_rules%ROWTYPE;
      --Sunitha ch. bug 5581523  removed the default value of the l_qty variable 
      --l_qty                    NUMBER                                 := 0;
      l_qty                    NUMBER;
      l_item_id                NUMBER                                 := NULL;
      l_from_uom               VARCHAR2 (4);
      l_to_uom                 VARCHAR2 (4);
      l_item_no                VARCHAR2 (80);
      l_start_date             DATE;
      l_cmplt_date             DATE;
      l_recipe_use             NUMBER;
      l_msg_count              NUMBER;
      l_msg_data               VARCHAR2 (2000);
      l_return_code            NUMBER;
      l_return_status          VARCHAR2 (1);
      l_total_input            NUMBER;
      l_total_output           NUMBER;
      l_recipe_validity_tbl    gmd_validity_rules.recipe_validity_tbl;
      l_api_name      CONSTANT VARCHAR2 (30)       := 'Validate_validity_rule';
      -- Bug 9914962
      l_status_type             VARCHAR2 (10);
      /* EXCEPTION Definitions */
      uom_conversion_failure   EXCEPTION;
      validation_failure       EXCEPTION;
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      OPEN get_validity_rule (p_validity_rule_id);

      FETCH get_validity_rule
       INTO l_validity_rec;

      IF get_validity_rule%NOTFOUND THEN
         CLOSE get_validity_rule;

         gme_common_pvt.log_message ('GME_API_INVALID_VALIDITY');
         RETURN FALSE;
      END IF;

      CLOSE get_validity_rule;

      -- Bug 13811289 - Make sure this is valid for the specific organization.
      IF NVL(l_validity_rec.organization_id, p_organization_id) <> p_organization_id THEN
         gme_common_pvt.log_message ('GME_API_INVALID_VALIDITY');
         RETURN FALSE;
      END IF;
      
      IF l_validity_rec.delete_mark = 1 THEN
         gme_common_pvt.log_message ('GME_API_INVALID_VALIDITY');
         RETURN FALSE;
      ELSIF l_validity_rec.inventory_item_id <> p_prim_product_id THEN
         gme_common_pvt.log_message ('GME_INVALID_VALIDITY_RULE_PROD');
         RETURN FALSE;
      ELSIF p_creation_mode NOT IN ('OUTPUT', 'INPUT') THEN
         IF p_uom <> l_validity_rec.detail_uom THEN
            l_qty :=
               inv_convert.inv_um_convert
                                    (item_id            => p_prim_product_id
                                    ,PRECISION          => gme_common_pvt.g_precision
                                    ,from_quantity      => p_qty
                                    ,from_unit          => p_uom
                                    ,to_unit            => l_validity_rec.detail_uom
                                    ,from_name          => NULL
                                    ,to_name            => NULL);

            IF (l_qty < 0) THEN
               l_item_id := p_prim_product_id;
               l_from_uom := p_uom;
               l_to_uom := l_validity_rec.detail_uom;
               RAISE uom_conversion_failure;
            END IF;
         ELSE
            l_qty := p_qty;
         END IF;

         IF l_qty < l_validity_rec.min_qty OR l_qty > l_validity_rec.max_qty THEN
            gme_common_pvt.log_message ('GME_INVALID_VALIDITY_RULE_QTY');
            RETURN FALSE;
         END IF;

         l_qty := p_qty;
      END IF;

      IF p_object_type = 'F' /* FPO */ THEN
         IF l_validity_rec.recipe_use NOT IN
                                           (0, 1) /* Production , Planning */ THEN
            gme_common_pvt.log_message ('GME_INVALID_VALIDITY_RULE_USE');
            RETURN FALSE;
         END IF;

         /* BUG 5021736 added between logic */
         IF NOT(l_validity_rec.validity_rule_status between 700 and 799 OR l_validity_rec.validity_rule_status  between 900 and 999) THEN
            gme_common_pvt.log_message ('GME_INVALID_VALIDITY_RULE_STAT');
            RETURN FALSE;
         END IF;

         l_recipe_use := 0;
      ELSIF p_object_type = 'P' /* Production Batch */ THEN
         IF l_validity_rec.recipe_use <> 0 /* Production */ THEN
            gme_common_pvt.log_message ('GME_INVALID_VALIDITY_RULE_USE');
            RETURN FALSE;
         END IF;
         /* BUG 5021736 added between logic */
         IF NOT(l_validity_rec.validity_rule_status between 700 and 799 OR l_validity_rec.validity_rule_status  between 900 and 999) THEN
            gme_common_pvt.log_message ('GME_INVALID_VALIDITY_RULE_STAT');
            RETURN FALSE;
         END IF;

         l_recipe_use := 1;
      ELSIF p_object_type = 'L' /* Lab Batch */ THEN
         IF l_validity_rec.recipe_use <> 0 /* Production */ THEN
            gme_common_pvt.log_message ('GME_INVALID_VALIDITY_RULE_USE');
            RETURN FALSE;
         END IF;
         /* BUG 5021736 added between logic */
         IF NOT(l_validity_rec.validity_rule_status between 400 and 499 OR
                l_validity_rec.validity_rule_status  between 500 and 599 OR
                l_validity_rec.validity_rule_status between 700 and 799 OR
                l_validity_rec.validity_rule_status  between 900 and 999 OR
                l_validity_rec.validity_rule_status  between 600 and 699 ) THEN
            gme_common_pvt.log_message('GME_INVALID_VALIDITY_RULE_STAT');
            RETURN FALSE;
         END IF; 

         l_recipe_use := 1;
      END IF;

      /* If both dates are passed ten check them against validity rule
         If only one date is passed then check that date only
         If both dates are not passed then assume both as sysdate and check */
      l_start_date := p_start_date;
      l_cmplt_date := p_cmplt_date;

      IF NOT gme_common_pvt.check_validity_rule_dates
                                    (p_validity_rule_id      => p_validity_rule_id
                                    ,p_start_date            => l_start_date
                                    ,p_cmplt_date            => l_cmplt_date) THEN
         RETURN FALSE;
      END IF;

      IF p_start_date IS NULL AND p_cmplt_date IS NULL THEN
         l_start_date := SYSDATE;
         l_cmplt_date := SYSDATE;
      ELSIF p_start_date IS NULL THEN
         l_start_date := l_cmplt_date;
      ELSIF p_cmplt_date IS NULL THEN
         l_cmplt_date := l_start_date;
      END IF;

      /* Validating for creating as Recipe validity rule or product and quantity */
      IF p_creation_mode NOT IN ('OUTPUT', 'INPUT') THEN
         RETURN TRUE;
      END IF;

      IF p_creation_mode = 'OUTPUT' THEN
         l_total_output := p_qty;
      ELSE                                       /* p_creation mode = INPUT */
         l_total_input := p_qty;
      END IF;

      gmd_val_data_pub.get_val_data
                               (p_api_version              => 1
                               ,p_init_msg_list            => fnd_api.g_false
                               ,p_object_type              => p_object_type
                               ,p_recipe_no                => NULL
                               ,p_recipe_version           => NULL
                               ,p_recipe_id                => NULL
                               ,p_total_input              => l_total_input
                               ,p_total_output             => l_total_output
                               ,p_formula_id               => NULL
                               ,p_item_id                  => p_prim_product_id
                               ,p_item_no                  => NULL
                               ,p_product_qty              => l_qty
                               ,p_uom                      => p_uom
                               ,p_recipe_use               => l_recipe_use
                               ,p_organization_id          => p_organization_id
                               ,p_start_date               => l_start_date
                               ,p_end_date                 => l_cmplt_date
                               ,p_status_type              => l_status_type  /* Bug 9914962 */
                               --,p_status_type              => '700'
                               ,p_validity_rule_id         => p_validity_rule_id
                               ,x_return_status            => l_return_status
                               ,x_msg_count                => l_msg_count
                               ,x_msg_data                 => l_msg_data
                               ,x_return_code              => l_return_code
                               ,x_recipe_validity_out      => l_recipe_validity_tbl);

      IF    l_return_status <> fnd_api.g_ret_sts_success
         OR l_recipe_validity_tbl.COUNT <= 0 THEN
         RAISE validation_failure;
      END IF;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;

      RETURN TRUE;
   EXCEPTION
      WHEN uom_conversion_failure THEN
         IF l_item_no IS NULL THEN
            OPEN cur_item_no (p_organization_id, l_item_id);

            FETCH cur_item_no
             INTO l_item_no;

            CLOSE cur_item_no;
         END IF;

         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line
                           ('UOM conversion failed in validate_validity_rule');
         END IF;

         fnd_message.set_name ('GMI', 'IC_API_UOM_CONVERSION_ERROR');
         fnd_message.set_token ('ITEM_NO', l_item_no);
         fnd_message.set_token ('FROM_UOM', l_from_uom);
         fnd_message.set_token ('TO_UOM', l_to_uom);
         fnd_msg_pub.ADD;
         RETURN FALSE;
      WHEN validation_failure THEN
         gme_common_pvt.log_message ('GME_INVALID_VALIDITY_RULE_QTY');
         RETURN FALSE;
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
         RETURN FALSE;
   END validate_validity_rule;

   PROCEDURE get_batch_shortages (
      p_organization_id   IN              NUMBER
     ,p_batch_id          IN              NUMBER
     ,p_invoke_mode       IN              VARCHAR2
     ,p_tree_mode         IN              NUMBER
     ,x_return_status     OUT NOCOPY      VARCHAR2
     ,x_exception_tbl     OUT NOCOPY      gme_common_pvt.exceptions_tab) IS
      /* Bug 5212165 added line_no in the order by */
      CURSOR cur_get_materials IS
        SELECT   d.*
        FROM gme_material_details d, mtl_system_items_b i
        WHERE d.batch_id IN (SELECT DISTINCT batch_id
                             FROM gme_material_details
                             START WITH batch_id = p_batch_id
                             CONNECT BY batch_id = PRIOR phantom_id)
              AND d.line_type = -1
              AND d.phantom_type = 0
              AND d.actual_qty < NVL (d.wip_plan_qty, d.plan_qty)
              AND (p_invoke_mode = 'O' OR (p_invoke_mode = 'S' AND d.subinventory IS NOT NULL))
              AND i.organization_id = d.organization_id
              AND i.inventory_item_id = d.inventory_item_id
              AND i.stock_enabled_flag = 'Y'
              AND i.mtl_transactions_enabled_flag = 'Y'
        ORDER BY d.inventory_item_id, d.batch_id, d.revision, d.line_no;

      --FPbug#4912179 modified query to select required columns only
      /* Bug 5441643 Added NVL condition for location control code*/      
      CURSOR cur_get_item (v_org_id NUMBER, v_inventory_item_id NUMBER)
      IS
         SELECT concatenated_segments, NVL(location_control_code,1) location_control_code, 
                restrict_locators_code, primary_uom_code, 
                reservable_type 
           FROM mtl_system_items_kfv
          WHERE organization_id = v_org_id
            AND inventory_item_id = v_inventory_item_id;

      CURSOR cur_get_onhand (
         v_org_id              NUMBER
        ,v_inventory_item_id   NUMBER
        ,v_sub_code            VARCHAR2
        ,v_revision            VARCHAR2)
      IS
         SELECT NVL (SUM (primary_transaction_quantity), 0) onhand
           FROM mtl_onhand_quantities_detail
          WHERE organization_id = v_org_id
            AND inventory_item_id = v_inventory_item_id
            AND (v_revision IS NULL OR revision = v_revision)
            AND (p_invoke_mode = 'O' OR (subinventory_code = v_sub_code) )
            AND (inv_material_status_grp.is_status_applicable
                                                  (NULL
                                                  ,NULL
                                                  ,gme_common_pvt.g_ing_issue
                                                  ,NULL
                                                  ,NULL
                                                  ,v_org_id
                                                  ,inventory_item_id
                                                  ,subinventory_code
                                                  ,locator_id
                                                  ,lot_number
                                                  ,NULL
                                                  ,'A') = 'N');

      -- Bug 14488987 - Get patch level.
      CURSOR  cur_patch_level IS
           SELECT 1
             FROM fnd_product_installations
            WHERE application_id = 553 -- GME product
              AND patch_level like 'R12.GME.A.%'; -- This indicates that installed GME product level is 12.0

      l_dummy                NUMBER;
                                                  

      l_api_name    CONSTANT VARCHAR2 (30)            := 'get_batch_shortages';
      l_qoh                  NUMBER;
      l_rqoh                 NUMBER;
      l_qr                   NUMBER;
      l_qs                   NUMBER;
      l_att                  NUMBER;
      l_atr                  NUMBER;
      l_sqoh                 NUMBER;
      l_srqoh                NUMBER;
      l_sqr                  NUMBER;
      l_sqs                  NUMBER;
      l_satt                 NUMBER;
      l_satr                 NUMBER;
      l_tree_mode            NUMBER;
      l_unusable_qty         NUMBER;
      l_open_qty             NUMBER;
      l_open_qty_prim        NUMBER;
      l_msg_count            NUMBER;
      l_msg_data             VARCHAR2 (2000);
      l_subinventory         VARCHAR2 (10);
      l_return_status        VARCHAR2 (1);
      l_mtl_dtl_rec          gme_material_details%ROWTYPE;
      --FPbug#4912179
      --l_item_rec             mtl_system_items_kfv%ROWTYPE;
      l_item_rec             cur_get_item%ROWTYPE;
      l_exception_rec        gme_exceptions_gtmp%ROWTYPE;
      setup_failure          EXCEPTION;
      get_open_qty_err       EXCEPTION;
      uom_conv_error         EXCEPTION;
      exception_ins_err      EXCEPTION;
      unable_to_query_tree   EXCEPTION;
      TYPE qty_rec IS RECORD (onhand NUMBER, available NUMBER);
      TYPE qty_tab IS TABLE OF qty_rec INDEX BY VARCHAR2(25);
      l_qty_tbl  qty_tab;
      TYPE item_rec IS RECORD (concatenated_segments VARCHAR2(2000), location_control_code NUMBER, 
                               restrict_locators_code NUMBER, primary_uom_code VARCHAR2(3), reservable_type NUMBER);
      TYPE item_tab IS TABLE OF item_rec INDEX BY BINARY_INTEGER;
      l_item_tbl  item_tab;
      l_item_hash VARCHAR2(25);
      l_mtl_hash  VARCHAR2(25);
      l_item_onhand    NUMBER;
      l_item_available NUMBER;
      l_mtl_onhand     NUMBER;
      l_mtl_available  NUMBER;
      l_temp_qty       NUMBER;
      l_count          NUMBER := 0;
      l_allocated_qty  NUMBER := 0;
      --bug 25548444 add--
       l_sign NUMBER := 1;
   BEGIN
      IF (g_debug IS NOT NULL) THEN
         gme_debug.log_initialize ('BatchShortages');
      END IF;
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'|| l_api_name);
         gme_debug.put_line ('p_tree_mode '||p_tree_mode);
      END IF;
      x_return_status := fnd_api.g_ret_sts_success;
      IF NOT (gme_common_pvt.g_setup_done) THEN
        IF NOT (gme_common_pvt.setup (p_org_id => p_organization_id) ) THEN
          RAISE setup_failure;
        END IF;
      END IF;
      
      /* Bug 5256543 If get shortages is called multiple times then records have to be cleared */
      DELETE FROM gme_exceptions_gtmp;
      
      OPEN cur_get_materials;
      LOOP
      	 /* Bug 5512352 l_exception_rec needs to reset for each material */
      	 l_exception_rec := NULL;
         FETCH cur_get_materials INTO l_mtl_dtl_rec;
         IF (cur_get_materials%NOTFOUND) THEN
            EXIT;
         END IF;
         BEGIN
           l_item_rec.concatenated_segments  := l_item_tbl(l_mtl_dtl_rec.inventory_item_id).concatenated_segments;
           l_item_rec.location_control_code  := l_item_tbl(l_mtl_dtl_rec.inventory_item_id).location_control_code;
           l_item_rec.restrict_locators_code := l_item_tbl(l_mtl_dtl_rec.inventory_item_id).restrict_locators_code;
           l_item_rec.primary_uom_code       := l_item_tbl(l_mtl_dtl_rec.inventory_item_id).primary_uom_code;
           l_item_rec.reservable_type        := l_item_tbl(l_mtl_dtl_rec.inventory_item_id).reservable_type;      
         EXCEPTION
           WHEN NO_DATA_FOUND THEN
             OPEN cur_get_item (l_mtl_dtl_rec.organization_id, l_mtl_dtl_rec.inventory_item_id);
             FETCH cur_get_item INTO l_item_tbl(l_mtl_dtl_rec.inventory_item_id).concatenated_segments, 
                                     l_item_tbl(l_mtl_dtl_rec.inventory_item_id).location_control_code, 
                                     l_item_tbl(l_mtl_dtl_rec.inventory_item_id).restrict_locators_code,
                                     l_item_tbl(l_mtl_dtl_rec.inventory_item_id).primary_uom_code, 
                                     l_item_tbl(l_mtl_dtl_rec.inventory_item_id).reservable_type;
             CLOSE cur_get_item;
             l_item_rec.concatenated_segments  := l_item_tbl(l_mtl_dtl_rec.inventory_item_id).concatenated_segments;
             l_item_rec.location_control_code  := l_item_tbl(l_mtl_dtl_rec.inventory_item_id).location_control_code;
             l_item_rec.restrict_locators_code := l_item_tbl(l_mtl_dtl_rec.inventory_item_id).restrict_locators_code;
             l_item_rec.primary_uom_code       := l_item_tbl(l_mtl_dtl_rec.inventory_item_id).primary_uom_code;
             l_item_rec.reservable_type        := l_item_tbl(l_mtl_dtl_rec.inventory_item_id).reservable_type;
         END;

         /* Bug 5441643 Added NVL condition for location control code*/
         get_open_qty
               (p_mtl_dtl_rec                 => l_mtl_dtl_rec
               ,p_called_by                   => 'S'
               ,p_item_location_control       => NVL(l_item_rec.location_control_code,1)
               ,p_item_restrict_locators      => l_item_rec.restrict_locators_code
               ,x_open_qty                    => l_open_qty
               ,x_return_status               => l_return_status);
               --  Bug 25548444 start--
               IF l_open_qty < 0 then
                 l_sign := -1;
                ----l_open_qty := l_open_qty * l_sign;--
                END IF;
                
            ---Bug 25548444 end--
         IF (l_return_status <> fnd_api.g_ret_sts_success) THEN
           RAISE get_open_qty_err;
         ELSE
           IF (l_mtl_dtl_rec.dtl_um = l_item_rec.primary_uom_code) THEN
             l_open_qty_prim := l_open_qty;
           ELSE
            --  Bug 25548444 change l_open_qty to abs(l_open_qty)--
             l_temp_qty := inv_convert.inv_um_convert
                                 (item_id            => l_mtl_dtl_rec.inventory_item_id
                                 ,PRECISION          => gme_common_pvt.g_precision
                                 ,from_quantity      => abs(l_open_qty)
                                 ,from_unit          => l_mtl_dtl_rec.dtl_um
                                 ,to_unit            => l_item_rec.primary_uom_code
                                 ,from_name          => NULL
                                 ,to_name            => NULL);
             IF (l_temp_qty < 0) THEN
               RAISE uom_conv_error;
             ELSE
             --- Bug 25548444 start--
             l_temp_qty := l_temp_qty * l_sign;
            ---Bug 25548444 end--
               l_open_qty_prim := l_temp_qty;
             END IF;
           END IF;
         END IF;
         IF g_debug <= gme_debug.g_log_statement THEN
           gme_debug.put_line ('item = ' || l_item_rec.concatenated_segments);
           gme_debug.put_line ('l_open_qty_prim = ' || l_open_qty_prim);
         END IF;
         IF (l_open_qty_prim > 0) THEN
           IF (p_invoke_mode = 'S') THEN
             l_subinventory := l_mtl_dtl_rec.subinventory;
           ELSE
             l_subinventory := NULL;
           END IF;
           l_item_hash := RPAD(l_mtl_dtl_rec.inventory_item_id, 12, 'X')||'***'||NVL(l_subinventory, '##########');
           l_mtl_hash  := RPAD(l_mtl_dtl_rec.inventory_item_id, 12, 'X')||NVL(RPAD(l_mtl_dtl_rec.revision, 3, '*'), '***')||NVL(RPAD(l_subinventory, 10, '#'), '##########');
           IF g_debug <= gme_debug.g_log_statement THEN
             gme_debug.put_line ('l_item_hash = ' || l_item_hash);
             gme_debug.put_line ('l_mtl_hash  = ' || l_mtl_hash);
           END IF;
           BEGIN
             l_item_onhand    := l_qty_tbl(l_item_hash).onhand;
             l_item_available := l_qty_tbl(l_item_hash).available;
             IF g_debug <= gme_debug.g_log_statement THEN
               gme_debug.put_line ('found l_item_onhand = ' || l_item_onhand);
               gme_debug.put_line ('found l_item_available  = ' || l_item_available);
             END IF;
           EXCEPTION
             WHEN NO_DATA_FOUND THEN
               IF p_tree_mode = g_tree_transaction_mode OR l_item_rec.reservable_type = 2 THEN
                 l_tree_mode := g_tree_transaction_mode;
               ELSE
                 l_tree_mode := g_tree_reservation_mode;
               END IF;
               gme_transactions_pvt.query_quantities
                     (x_return_status                => l_return_status
                     ,x_msg_count                    => l_msg_count
                     ,x_msg_data                     => l_msg_data
                     ,p_organization_id              => l_mtl_dtl_rec.organization_id
                     ,p_inventory_item_id            => l_mtl_dtl_rec.inventory_item_id
                     ,p_tree_mode                    => l_tree_mode
                     ,p_grade_code                   => NULL
                     ,p_demand_source_header_id      => l_mtl_dtl_rec.batch_id
                     ,p_demand_source_line_id        => l_mtl_dtl_rec.material_detail_id
                     ,p_lot_expiration_date          => l_mtl_dtl_rec.material_requirement_date   -- 9442109
                     ,p_revision                     => NULL
                     ,p_lot_number                   => NULL
                     ,p_subinventory_code            => l_subinventory
                     ,p_locator_id                   => NULL
                     ,x_qoh                          => l_qoh
                     ,x_rqoh                         => l_rqoh
                     ,x_qr                           => l_qr
                     ,x_qs                           => l_qs
                     ,x_att                          => l_att
                     ,x_atr                          => l_atr
                     ,x_sqoh                         => l_sqoh
                     ,x_srqoh                        => l_srqoh
                     ,x_sqr                          => l_sqr
                     ,x_sqs                          => l_sqs
                     ,x_satt                         => l_satt
                     ,x_satr                         => l_satr);
               IF (l_return_status = fnd_api.g_ret_sts_success) THEN
                 IF g_debug <= gme_debug.g_log_statement THEN
                   gme_debug.put_line('Item level qtys');
                   gme_debug.put_line('l_qoh = ' || l_qoh);
                   gme_debug.put_line('l_att = ' || l_att);
                   gme_debug.put_line('l_atr = ' || l_atr);
                 END IF;
                 l_qty_tbl(l_item_hash).onhand := l_qoh;

                 -- Bug 13611486 - This backs out fix done in 8582412. INV team must provide a way of 
                 -- accurately getting atr/att for a given trans type i.e. wip issue and consider status.
                 
                 -- Bug 8582412 - Moved cursor outside of if - we need unusable for both calculations.
                 -- Bug 16100060 - Why bother doing the fetch if we are not going to use it.
                 -- OPEN cur_get_onhand (l_mtl_dtl_rec.organization_id, l_mtl_dtl_rec.inventory_item_id, null, null);
                 -- FETCH cur_get_onhand INTO l_unusable_qty;
                 -- CLOSE cur_get_onhand;
                 
                 l_unusable_qty := 0; -- Bug 13611486 - This effectively disregards previous fetch.
                 
                 IF (l_tree_mode = g_tree_transaction_mode) THEN
                   -- OPEN cur_get_onhand (l_mtl_dtl_rec.organization_id, l_mtl_dtl_rec.inventory_item_id, null, null);
                   -- FETCH cur_get_onhand INTO l_unusable_qty;
                   -- CLOSE cur_get_onhand;
                   -- l_qty_tbl(l_item_hash).available := l_att;
                   -- l_qty_tbl(l_item_hash).available := l_qty_tbl(l_item_hash).available - NVL(l_unusable_qty,0);
                   l_qty_tbl(l_item_hash).available := l_att - NVL(l_unusable_qty,0);
                 ELSE
                   l_qty_tbl(l_item_hash).available := l_atr - NVL(l_unusable_qty,0);
                 END IF;
               ELSE
                 RAISE unable_to_query_tree;
               END IF;
               l_item_onhand    := l_qty_tbl(l_item_hash).onhand;
               l_item_available := l_qty_tbl(l_item_hash).available;
           END;
           IF (l_item_hash <> l_mtl_hash) THEN
              IF g_debug <= gme_debug.g_log_statement THEN
                gme_debug.put_line('Item level and mtl level hashes are diff');
              END IF;
             BEGIN
               l_mtl_onhand    := l_qty_tbl(l_mtl_hash).onhand;
               l_mtl_available := l_qty_tbl(l_mtl_hash).available;
               IF g_debug <= gme_debug.g_log_statement THEN
                 gme_debug.put_line ('found l_mtl_onhand = ' || l_mtl_onhand);
                 gme_debug.put_line ('found l_mtl_available  = ' || l_mtl_available);
               END IF;
             EXCEPTION
               WHEN NO_DATA_FOUND THEN
                 IF p_tree_mode = g_tree_transaction_mode OR l_item_rec.reservable_type = 2 THEN
                   l_tree_mode := g_tree_transaction_mode;
                 ELSE
                   l_tree_mode := g_tree_reservation_mode;
                 END IF;
                 gme_transactions_pvt.query_quantities
                       (x_return_status                => l_return_status
                       ,x_msg_count                    => l_msg_count
                       ,x_msg_data                     => l_msg_data
                       ,p_organization_id              => l_mtl_dtl_rec.organization_id
                       ,p_inventory_item_id            => l_mtl_dtl_rec.inventory_item_id
                       ,p_tree_mode                    => l_tree_mode
                       ,p_grade_code                   => NULL
                       ,p_demand_source_header_id      => l_mtl_dtl_rec.batch_id
                       ,p_demand_source_line_id        => l_mtl_dtl_rec.material_detail_id
                       ,p_lot_expiration_date          => l_mtl_dtl_rec.material_requirement_date   -- 9442109
                       ,p_revision                     => l_mtl_dtl_rec.revision
                       ,p_lot_number                   => NULL
                       ,p_subinventory_code            => l_subinventory
                       ,p_locator_id                   => NULL
                       ,x_qoh                          => l_qoh
                       ,x_rqoh                         => l_rqoh
                       ,x_qr                           => l_qr
                       ,x_qs                           => l_qs
                       ,x_att                          => l_att
                       ,x_atr                          => l_atr
                       ,x_sqoh                         => l_sqoh
                       ,x_srqoh                        => l_srqoh
                       ,x_sqr                          => l_sqr
                       ,x_sqs                          => l_sqs
                       ,x_satt                         => l_satt
                       ,x_satr                         => l_satr);
                 IF (l_return_status = fnd_api.g_ret_sts_success) THEN
                   IF g_debug <= gme_debug.g_log_statement THEN
                     gme_debug.put_line('Item/Revision/Sub level qtys');
                     gme_debug.put_line('l_qoh = ' || l_qoh);
                     gme_debug.put_line('l_att = ' || l_att);
                     gme_debug.put_line('l_atr = ' || l_atr);
                   END IF;
                   l_qty_tbl(l_mtl_hash).onhand := l_qoh;
                   IF (l_tree_mode = g_tree_transaction_mode) THEN
                     l_qty_tbl(l_mtl_hash).available := l_att;

                     -- Bug 13611486 - This backs out fix done in 8582412. INV team must provide a way of 
                     -- accurately getting atr/att for a given trans type i.e. wip issue and consider status.                     
                     -- Bug 16100060 - Why bother doing the fetch if we are not going to use it.
                     -- OPEN cur_get_onhand (l_mtl_dtl_rec.organization_id
                     --                     ,l_mtl_dtl_rec.inventory_item_id
                     --                     ,l_mtl_dtl_rec.subinventory
                     --                     ,l_mtl_dtl_rec.revision);
                     -- FETCH cur_get_onhand INTO l_unusable_qty;
                     -- CLOSE cur_get_onhand;

                     l_unusable_qty := 0; -- Bug 13611486 - This effectively disregards previous fetch.
                     
                     l_qty_tbl(l_mtl_hash).available := l_qty_tbl(l_mtl_hash).available - NVL(l_unusable_qty,0);
                   ELSE
                     l_qty_tbl(l_mtl_hash).available := l_atr;
                   END IF;
                 ELSE
                   RAISE unable_to_query_tree;
                 END IF;
                 l_mtl_onhand    := l_qty_tbl(l_mtl_hash).onhand;
                 l_mtl_available := l_qty_tbl(l_mtl_hash).available;
             END;
           ELSE
             IF g_debug <= gme_debug.g_log_statement THEN
               gme_debug.put_line('Item level and mtl level hashes are same');
               gme_debug.put_line('Invoke mode is '||p_invoke_mode);
               gme_debug.put_line('onhand is '||l_item_onhand);
               gme_debug.put_line('avail is '||l_item_available);
             END IF;
             l_mtl_onhand    := l_item_onhand;
             l_mtl_available := l_item_available;
           END IF; /* IF (l_item_hash <> l_mtl_hash) THEN */


           /*************************************************************************
            Customer bug 7462542 Consider the Manufacturing Order allocations as
            part of unavailable inventory.
            This issue occurs only at org lvel. So we need to get sum of move order
            allocation at org level where no reservation exists.
            *************************************************************************/

           -- Bug 14488987 - Move orders are already accounted for in qty tree in release 12.1 and above.
           -- Leave logic of bug 7462542 in place for release 12.0 only.         
           OPEN cur_patch_level;           
           FETCH cur_patch_level
            INTO l_dummy;
           
           IF (cur_patch_level%NOTFOUND) THEN
              l_dummy := 0;      	
           END IF;
           
           CLOSE cur_patch_level; 
            
           IF p_invoke_mode = 'O' and l_dummy = 1 then
             BEGIN
               SELECT   SUM(l.primary_quantity) into l_allocated_qty
               FROM mtl_material_transactions_temp t,
                    mtl_txn_request_lines l, 
                    mtl_txn_request_headers h
               WHERE t.move_order_line_id = l.line_id
                 AND t.move_order_header_id = h.header_id
                 AND t.organization_id = l_mtl_dtl_rec.organization_id
                 AND t.inventory_item_id = l_mtl_dtl_rec.inventory_item_id
                 AND t.reservation_id not in (select reservation_id from mtl_reservations
                                             where demand_source_header_id = l_mtl_dtl_rec.batch_id	and 
                                                   demand_source_line_id = l_mtl_dtl_rec.material_detail_id and
                                                   demand_Source_type_id = gme_common_pvt.g_txn_source_type)
                 AND TRANSACTION_SOURCE_ID NOT IN (l_mtl_dtl_rec.batch_id)  --bug 7496141
                 AND TRX_SOURCE_LINE_ID NOT IN (l_mtl_dtl_rec.material_detail_id) --bug 7496141
		 AND h.move_order_type = gme_common_pvt.g_txn_source_type
                 AND l.line_status NOT IN (5, 6)
                 AND h.header_id = l.header_id
                 AND h.move_order_type NOT IN
                        (gme_common_pvt.g_invis_move_order_type
                        ,inv_globals.g_move_order_put_away);
                        
             IF g_debug <= gme_debug.g_log_statement THEN
               gme_debug.put_line('Additional Material Allocated to Move Order:'||l_allocated_qty);
             END IF;

             EXCEPTION WHEN NO_DATA_FOUND THEN
                 l_allocated_qty := 0;
             END;
             l_mtl_available := l_mtl_available - nvl(l_allocated_qty,0); 
             IF g_debug <= gme_debug.g_log_statement THEN
               gme_debug.put_line('Material Available before deducting additional Move Order Allocation:'||(l_mtl_available + nvl(l_allocated_qty,0)));
               gme_debug.put_line('Material Available after deducting additional Move Order Allocation:'||l_mtl_available );
             END IF;

           END IF;
           
           IF (l_open_qty_prim > l_mtl_available) THEN
             l_exception_rec.organization_id    := l_mtl_dtl_rec.organization_id;
             l_exception_rec.onhand_qty         := l_qoh;
             l_exception_rec.material_detail_id := l_mtl_dtl_rec.material_detail_id;
             IF l_tree_mode = g_tree_transaction_mode THEN
               l_exception_rec.att := l_mtl_available;
             ELSE
               l_exception_rec.atr := l_mtl_available;
             END IF;
             IF NOT (insert_exceptions (l_exception_rec) ) THEN
                RAISE exception_ins_err;
             END IF;
             l_count := l_count + 1;
             x_exception_tbl (l_count) := l_exception_rec;
           END IF;
           
           l_qty_tbl(l_mtl_hash).available  := l_qty_tbl(l_mtl_hash).available - l_open_qty_prim;
           IF (l_item_hash <> l_mtl_hash) THEN
             l_qty_tbl(l_item_hash).available := l_qty_tbl(l_item_hash).available - l_open_qty_prim;
           END IF;
         END IF; /* IF (l_open_qty_prim > 0) THEN */
      END LOOP;
      CLOSE cur_get_materials;
      IF g_debug <= gme_debug.g_log_procedure THEN
        gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;
   EXCEPTION
      WHEN setup_failure THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN get_open_qty_err OR unable_to_query_tree THEN
         x_return_status := l_return_status;
         IF (cur_get_materials%ISOPEN) THEN
           CLOSE cur_get_materials;
         END IF;
      WHEN uom_conv_error THEN
         x_return_status := fnd_api.g_ret_sts_error;
         fnd_message.set_name ('GMI', 'IC_API_UOM_CONVERSION_ERROR');
         fnd_message.set_token ('ITEM_NO', l_item_rec.concatenated_segments);
         fnd_message.set_token ('FROM_UOM', l_mtl_dtl_rec.dtl_um);
         fnd_message.set_token ('TO_UOM', l_item_rec.primary_uom_code);
         IF (cur_get_materials%ISOPEN) THEN
           CLOSE cur_get_materials;
         END IF;
         fnd_msg_pub.add;
      WHEN exception_ins_err THEN
         x_return_status := fnd_api.g_ret_sts_unexp_error;
         IF (cur_get_materials%ISOPEN) THEN
           CLOSE cur_get_materials;
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
         IF (cur_get_materials%ISOPEN) THEN
           CLOSE cur_get_materials;
         END IF;
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
         x_return_status := fnd_api.g_ret_sts_unexp_error;
   END get_batch_shortages;

   PROCEDURE get_open_qty (
      p_mtl_dtl_rec              IN              gme_material_details%ROWTYPE
     ,p_called_by                IN              VARCHAR2
     ,                            /* P- picking, R-reservation, S-shortages, Z-from Auto-Detail line */
      p_item_location_control    IN              NUMBER DEFAULT NULL
     ,p_item_restrict_locators   IN              NUMBER DEFAULT NULL
     ,x_open_qty                 OUT NOCOPY      NUMBER
     ,x_return_status            OUT NOCOPY      VARCHAR2)
   IS
      l_api_name        CONSTANT VARCHAR2 (30)              := 'get_open_qty';
      l_return_status            VARCHAR2 (1);
      l_reserved_qty             NUMBER                          := 0;

      -- Bug 21653652
      l_full_reserved_qty        NUMBER                          := 0;
      
      l_mo_line_qty              NUMBER                          := 0;
      l_temp_qty                 NUMBER                          := 0;
      /* Bug 5441643 Added NVL condition for location control code*/
      CURSOR cur_item_controls (v_org_id NUMBER, v_inventory_item_id NUMBER)
      IS
         SELECT NVL(location_control_code,1), restrict_locators_code
           FROM mtl_system_items_b
          WHERE organization_id = v_org_id
            AND inventory_item_id = v_inventory_item_id;

      l_item_loc_control         NUMBER;
      l_item_restrict_locators   NUMBER;
      l_resv_tbl                 gme_common_pvt.reservations_tab;
      get_reserved_qty_err       EXCEPTION;
      get_reservations_err       EXCEPTION;
      get_pending_qty_err        EXCEPTION;
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
         gme_debug.put_line ('Being called by ' || p_called_by);        
      END IF;

      x_return_status := fnd_api.g_ret_sts_success;

      -- All qty should be in the detail line uom.
      -- all allocations excludes the invisible move orders
      IF p_called_by = 'S' THEN
         gme_reservations_pvt.get_reserved_qty
                                          (p_mtl_dtl_rec        => p_mtl_dtl_rec
                                          ,x_reserved_qty       => l_reserved_qty
                                          ,x_return_status      => l_return_status);

         IF (l_return_status <> fnd_api.g_ret_sts_success) THEN
            RAISE get_reserved_qty_err;
         END IF;

         x_open_qty :=
              NVL (p_mtl_dtl_rec.wip_plan_qty, p_mtl_dtl_rec.plan_qty)
            - (p_mtl_dtl_rec.actual_qty + NVL (l_reserved_qty, 0) );

         IF g_debug <= gme_debug.g_log_statement THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ' Calculated open qty for shortages is '
                                || x_open_qty);
         END IF;
      ELSIF p_called_by = 'P' THEN

         -- Bug 21653652 - get total for all detailed reservations.
         gme_reservations_pvt.get_reserved_qty
                                          (p_mtl_dtl_rec        => p_mtl_dtl_rec
                                          ,x_reserved_qty       => l_full_reserved_qty
                                          ,x_return_status      => l_return_status);                                          
      
         -- Bug 12613813 - pass in true for p_locator_only parameter so picking can consider locator also.
         gme_reservations_pvt.get_reserved_qty
                                        (p_mtl_dtl_rec          => p_mtl_dtl_rec
                                        ,p_supply_sub_only      => fnd_api.g_true
                                        ,p_locator_only         => fnd_api.g_true
                                        ,x_reserved_qty         => l_reserved_qty
                                        ,x_return_status        => l_return_status);

         IF (l_return_status <> fnd_api.g_ret_sts_success) THEN
            RAISE get_reserved_qty_err;
         END IF;

         gme_move_orders_pvt.get_pending_move_order_qty
                                           (p_mtl_dtl_rec        => p_mtl_dtl_rec
                                           ,x_pending_qty        => l_mo_line_qty
                                           ,x_return_status      => l_return_status);

         IF (l_return_status <> fnd_api.g_ret_sts_success) THEN
            RAISE get_pending_qty_err;
         END IF;

         IF g_debug <= gme_debug.g_log_statement THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ' p_mtl_dtl_rec.plan_qty = '
                                || p_mtl_dtl_rec.plan_qty);
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ' p_mtl_dtl_rec.wip_plan_qty = '
                                || p_mtl_dtl_rec.wip_plan_qty);
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ' p_mtl_dtl_rec.actual_qty = '
                                || p_mtl_dtl_rec.actual_qty);
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ' l_reserved_qty = '
                                || NVL (l_reserved_qty, 0) );
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ' l_full_reserved_qty = '
                                || NVL (l_full_reserved_qty, 0) );
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ' p_mtl_dtl_rec.backordered_qty = '
                                || NVL (p_mtl_dtl_rec.backordered_qty, 0) );
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ' l_mo_line_qty = '
                                || NVL (l_mo_line_qty, 0) );
         END IF;

         -- x_open_qty := NVL(p_mtl_dtl_rec.wip_plan_qty, p_mtl_dtl_rec.plan_qty)
         --                - (p_mtl_dtl_rec.actual_qty + NVL(l_reserved_qty,0) 
         --                - NVL(p_mtl_dtl_rec.backordered_qty,0) + NVL(l_mo_line_qty,0));
         -- BUG# 5311713 Rounded the wip_plan_qty and plan_qty to 5 decimal places.

         -- Bug 21653652 - Use full reserved (as starting point) if it is greater than the planned qty.
         IF l_full_reserved_qty > ROUND(NVL (p_mtl_dtl_rec.wip_plan_qty, p_mtl_dtl_rec.plan_qty),5) THEN
            x_open_qty := l_full_reserved_qty
               - (p_mtl_dtl_rec.actual_qty + NVL(l_reserved_qty, 0) + NVL(l_mo_line_qty, 0));
         ELSE
            x_open_qty :=
                 ROUND(NVL (p_mtl_dtl_rec.wip_plan_qty, p_mtl_dtl_rec.plan_qty),5)
               - (p_mtl_dtl_rec.actual_qty + NVL(l_reserved_qty, 0) + NVL(l_mo_line_qty, 0));
         END IF;

         IF g_debug <= gme_debug.g_log_statement THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ' Calculated open qty for picking is '
                                || x_open_qty);
         END IF;
      ELSIF p_called_by = 'R' THEN
         gme_reservations_pvt.get_material_reservations
                   (p_organization_id         => p_mtl_dtl_rec.organization_id
                   ,p_batch_id                => p_mtl_dtl_rec.batch_id
                   ,p_material_detail_id      => p_mtl_dtl_rec.material_detail_id
                   ,x_return_status           => l_return_status
                   ,x_reservations_tbl        => l_resv_tbl);

         IF (l_return_status <> fnd_api.g_ret_sts_success) THEN
            RAISE get_reservations_err;
         END IF;

         FOR i IN 1 .. l_resv_tbl.COUNT LOOP
            IF (   p_item_location_control IS NULL
                OR p_item_restrict_locators IS NULL) THEN
               OPEN cur_item_controls (p_mtl_dtl_rec.organization_id
                                      ,p_mtl_dtl_rec.inventory_item_id);

               FETCH cur_item_controls
                INTO l_item_loc_control, l_item_restrict_locators;

               CLOSE cur_item_controls;
            ELSE
               l_item_loc_control := p_item_location_control;
               l_item_restrict_locators := p_item_restrict_locators;
            END IF;

            -- Assess fully detailed (1) and part detailed (2) reservations
            /*Bug#7346653 commenting the below if condition as the open qty
               should not include the the already created high level reservation as this is
               causing the extra HLR to be created from the gme_reservations_pvt.auto_detail_line */
           /* IF (gme_reservations_pvt.reservation_fully_specified
                         (p_reservation_rec             => l_resv_tbl (i)
                         ,p_item_location_control       => l_item_loc_control
                         ,p_item_restrict_locators      => l_item_restrict_locators) in (1,2)) THEN */

               gme_reservations_pvt.get_reservation_dtl_qty
                                         (p_reservation_rec      => l_resv_tbl
                                                                           (i)
                                         ,p_uom_code             => p_mtl_dtl_rec.dtl_um
                                         ,x_qty                  => l_temp_qty
                                         ,x_return_status        => l_return_status);

               IF (l_return_status <> fnd_api.g_ret_sts_success) THEN
                  RAISE get_reserved_qty_err;
               END IF;
               IF g_debug <= gme_debug.g_log_statement THEN
                  gme_debug.put_line (   g_pkg_name
                                      || '.'
                                      || l_api_name
                                      || ' get_reservation_dtl_qty returns qty of '
                                      || l_temp_qty);
               END IF;

               l_reserved_qty := NVL (l_reserved_qty, 0) + NVL (l_temp_qty, 0);
               IF g_debug <= gme_debug.g_log_statement THEN
                  gme_debug.put_line (   g_pkg_name
                                      || '.'
                                      || l_api_name
                                      || ' so total reserved qty computes to  '
                                      || l_reserved_qty);
               END IF;


--            END IF;
         END LOOP;
         IF g_debug <= gme_debug.g_log_statement THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ' wip_plan_qty   =>  '
                                || p_mtl_dtl_rec.wip_plan_qty);
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ' plan_qty       =>  '
                                || p_mtl_dtl_rec.plan_qty);
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ' actual_qty     =>  '
                                || p_mtl_dtl_rec.actual_qty);
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ' reserved_qty   =>  '
                                || l_reserved_qty);
         END IF;

         x_open_qty :=
              NVL (p_mtl_dtl_rec.wip_plan_qty, p_mtl_dtl_rec.plan_qty)
            - (p_mtl_dtl_rec.actual_qty + l_reserved_qty);

         IF g_debug <= gme_debug.g_log_statement THEN
            gme_debug.put_line
                              (   g_pkg_name
                               || '.'
                               || l_api_name
                               || ' Calculated open qty for reservations is '
                               || x_open_qty);
         END IF;
         /* Added below ELSIF in bug 9924437*/
      ELSIF p_called_by = 'Z' THEN
         gme_reservations_pvt.get_material_res
                   (p_organization_id         => p_mtl_dtl_rec.organization_id
                   ,p_batch_id                => p_mtl_dtl_rec.batch_id
                   ,p_material_detail_id      => p_mtl_dtl_rec.material_detail_id
                   ,x_return_status           => l_return_status
                   ,x_reservations_tbl        => l_resv_tbl);

         IF (l_return_status <> fnd_api.g_ret_sts_success) THEN
            RAISE get_reservations_err;
         END IF;

         FOR i IN 1 .. l_resv_tbl.COUNT LOOP
            IF (   p_item_location_control IS NULL
                OR p_item_restrict_locators IS NULL) THEN
               OPEN cur_item_controls (p_mtl_dtl_rec.organization_id
                                      ,p_mtl_dtl_rec.inventory_item_id);

               FETCH cur_item_controls
                INTO l_item_loc_control, l_item_restrict_locators;

               CLOSE cur_item_controls;
            ELSE
               l_item_loc_control := p_item_location_control;
               l_item_restrict_locators := p_item_restrict_locators;
            END IF;

               gme_reservations_pvt.get_reservation_dtl_qty
                                         (p_reservation_rec      => l_resv_tbl
                                                                           (i)
                                         ,p_uom_code             => p_mtl_dtl_rec.dtl_um
                                         ,x_qty                  => l_temp_qty
                                         ,x_return_status        => l_return_status);

               IF (l_return_status <> fnd_api.g_ret_sts_success) THEN
                  RAISE get_reserved_qty_err;
               END IF;
               IF g_debug <= gme_debug.g_log_statement THEN
                  gme_debug.put_line (   g_pkg_name
                                      || '.'
                                      || l_api_name
                                      || ' get_reservation_dtl_qty returns qty of '
                                      || l_temp_qty);
               END IF;

               l_reserved_qty := NVL (l_reserved_qty, 0) + NVL (l_temp_qty, 0);
               IF g_debug <= gme_debug.g_log_statement THEN
                  gme_debug.put_line (   g_pkg_name
                                      || '.'
                                      || l_api_name
                                      || ' so total reserved qty computes to  '
                                      || l_reserved_qty);
               END IF;

         END LOOP;
         IF g_debug <= gme_debug.g_log_statement THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ' wip_plan_qty   =>  '
                                || p_mtl_dtl_rec.wip_plan_qty);
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ' plan_qty       =>  '
                                || p_mtl_dtl_rec.plan_qty);
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ' actual_qty     =>  '
                                || p_mtl_dtl_rec.actual_qty);
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ' reserved_qty   =>  '
                                || l_reserved_qty);
         END IF;

         x_open_qty :=
              NVL (p_mtl_dtl_rec.wip_plan_qty, p_mtl_dtl_rec.plan_qty)
            - (p_mtl_dtl_rec.actual_qty + l_reserved_qty);

         IF g_debug <= gme_debug.g_log_statement THEN
            gme_debug.put_line
                              (   g_pkg_name
                               || '.'
                               || l_api_name
                               || ' Calculated open qty for reservations is '
                               || x_open_qty);
         END IF;   
         /*End of Bug No.9924437 */
      ELSE
         x_open_qty := 0;
      END IF;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;
   EXCEPTION
      WHEN get_reserved_qty_err OR get_pending_qty_err THEN
         x_return_status := l_return_status;
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
         x_return_status := fnd_api.g_ret_sts_unexp_error;
   END get_open_qty;

   FUNCTION insert_exceptions (p_exception_rec IN gme_exceptions_gtmp%ROWTYPE)
      RETURN BOOLEAN
   IS
      l_api_name   CONSTANT VARCHAR2 (30) := 'insert_exceptions';
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      -- Bug 9666304 - NVL on att variable.
      INSERT INTO gme_exceptions_gtmp
                  (organization_id
                  ,pending_move_order_ind
                  ,pending_reservations_ind
                  ,onhand_qty, att
                  ,atr, material_detail_id
                  ,transacted_qty
                  ,exception_qty, batch_id)
           VALUES (p_exception_rec.organization_id
                  ,p_exception_rec.pending_move_order_ind
                  ,p_exception_rec.pending_reservations_ind
                  ,p_exception_rec.onhand_qty, NVL(p_exception_rec.att, 0)
                  ,p_exception_rec.atr, p_exception_rec.material_detail_id
                  ,p_exception_rec.transacted_qty
                  ,p_exception_rec.exception_qty, p_exception_rec.batch_id);

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;

      RETURN TRUE;
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
         RETURN FALSE;
   END insert_exceptions;

   FUNCTION populate_temp_from_struct (
      p_exception_tbl   IN   gme_common_pvt.exceptions_tab)
      RETURN BOOLEAN
   IS
      l_api_name   CONSTANT VARCHAR2 (30) := 'populate_temp_from_struct';
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      FOR i IN 1 .. p_exception_tbl.COUNT LOOP
         IF NOT (insert_exceptions (p_exception_tbl (i) ) ) THEN
            RETURN FALSE;
         END IF;
      END LOOP;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;

      RETURN TRUE;
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
         RETURN FALSE;
   END populate_temp_from_struct;

   FUNCTION is_material_assoc_to_step (
      p_material_detail_id   IN   gme_material_details.material_detail_id%TYPE)
      RETURN BOOLEAN
   IS
      l_assoc_count         NUMBER;
      l_is_assoc            BOOLEAN;
      l_api_name   CONSTANT VARCHAR2 (30) := 'is_material_assoc_to_step';
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      SELECT COUNT (1)
        INTO l_assoc_count
        FROM DUAL
       WHERE EXISTS (SELECT 1
                       FROM gme_batch_step_items
                      WHERE material_detail_id = p_material_detail_id);

      IF l_assoc_count = 0 THEN
         l_is_assoc := FALSE;
      ELSE
         l_is_assoc := TRUE;
      END IF;

      RETURN l_is_assoc;
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
         l_is_assoc := FALSE;
         RETURN l_is_assoc;
   END is_material_assoc_to_step;

   FUNCTION get_assoc_step (
      p_material_detail_id   IN              gme_material_details.material_detail_id%TYPE
     ,x_batchstep_id         OUT NOCOPY      NUMBER
     ,x_batchstep_status     OUT NOCOPY      NUMBER)
      RETURN BOOLEAN
   IS
      l_api_name   CONSTANT VARCHAR2 (30) := 'get_assoc_step';
      no_assoc_step         EXCEPTION;

      CURSOR get_assoc_step (v_material_detail_id IN NUMBER)
      IS
         SELECT a.batchstep_id, s.step_status
           FROM gme_batch_step_items a, gme_batch_steps s
          WHERE a.material_detail_id = v_material_detail_id
            AND a.batchstep_id = s.batchstep_id
            AND a.batch_id = s.batch_id;
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      OPEN get_assoc_step (p_material_detail_id);

      FETCH get_assoc_step
       INTO x_batchstep_id, x_batchstep_status;

      IF get_assoc_step%NOTFOUND THEN
         CLOSE get_assoc_step;

         RAISE no_assoc_step;
      END IF;

      CLOSE get_assoc_step;

      RETURN TRUE;
   EXCEPTION
      WHEN no_assoc_step THEN
         IF g_debug <= gme_debug.g_log_unexpected THEN
            gme_debug.put_line (   'no associate steps '
                                || g_pkg_name
                                || '.'
                                || l_api_name
                                || ' Error is '
                                || SQLERRM);
         END IF;
         --FPBug#4585491 commented out the following line
         --fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
         RETURN FALSE;
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
         RETURN FALSE;
   END get_assoc_step;

   FUNCTION get_batch_header (
      p_batch_header_rec   IN              gme_batch_header%ROWTYPE
     ,p_org_code           IN              VARCHAR2
     ,p_batch_type         IN              NUMBER
     ,x_batch_header_rec   OUT NOCOPY      gme_batch_header%ROWTYPE)
      RETURN BOOLEAN
   IS
      CURSOR get_organization (p_org_code IN VARCHAR2)
      IS
         SELECT organization_id
           FROM mtl_parameters
          WHERE organization_code = p_org_code;

      l_batch_header_rec         gme_batch_header%ROWTYPE;
      l_api_name                 VARCHAR2 (30)          := 'GET_BATCH_HEADER';
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      l_batch_header_rec := p_batch_header_rec;

      IF p_batch_header_rec.batch_id IS NULL THEN
         IF p_org_code IS NULL AND p_batch_header_rec.organization_id IS NULL THEN
            gme_common_pvt.log_message ('GME_NO_KEYS'
                                        ,'TABLE_NAME'
                                        ,'GME_BATCH_HEADER'); 
            RAISE fnd_api.g_exc_error;
         ELSIF p_org_code IS NOT NULL THEN
            OPEN get_organization (p_org_code);

            FETCH get_organization
             INTO l_batch_header_rec.organization_id;

            IF get_organization%NOTFOUND THEN
               CLOSE get_organization;
               gme_common_pvt.log_message (p_message_code => 'IC_ORGNCODERR'
                                           ,p_product_code => 'GMI');
               RAISE fnd_api.g_exc_error;
            END IF;
            CLOSE get_organization;
         END IF;

         IF NVL (p_batch_header_rec.batch_type, p_batch_type) <> p_batch_type THEN
            gme_common_pvt.log_message ('GME_INVALID_BATCH_TYPE');
            RAISE fnd_api.g_exc_error;
         ELSE
            l_batch_header_rec.batch_type :=
                            NVL (p_batch_header_rec.batch_type, p_batch_type);
         END IF;
      END IF;

      IF NOT (gme_batch_header_dbl.fetch_row (l_batch_header_rec
                                             ,x_batch_header_rec) ) THEN
         RAISE fnd_api.g_exc_error;
      END IF;

      IF x_batch_header_rec.batch_type <> p_batch_type THEN
         gme_common_pvt.log_message ('GME_INVALID_BATCH_TYPE');
         RAISE fnd_api.g_exc_error;
      END IF;

      RETURN TRUE;
   EXCEPTION
      WHEN fnd_api.g_exc_error THEN
         IF g_debug <= gme_debug.g_log_error THEN
            gme_debug.put_line (   'Expected error '
                                || g_pkg_name
                                || '.'
                                || l_api_name);
         END IF;

         RETURN FALSE;
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
         RETURN FALSE;
   END get_batch_header;

   FUNCTION get_batch_step (
      p_batch_step_rec     IN              gme_batch_steps%ROWTYPE
     ,p_org_code           IN              VARCHAR2
     ,p_batch_no           IN              VARCHAR2
     ,x_batch_step_rec     OUT NOCOPY      gme_batch_steps%ROWTYPE
     ,x_batch_header_rec   OUT NOCOPY      gme_batch_header%ROWTYPE)
      RETURN BOOLEAN
   IS

      l_batch_header_rec_in      gme_batch_header%ROWTYPE;
      l_batch_header_rec         gme_batch_header%ROWTYPE;
      l_batch_step_rec           gme_batch_steps%ROWTYPE;
      l_api_name                 VARCHAR2 (30)          := 'GET_BATCH_STEP';

   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;
      l_batch_step_rec := p_batch_step_rec;
      IF p_batch_step_rec.batchstep_id IS NULL THEN
         l_batch_header_rec_in.batch_no := p_batch_no;
         l_batch_header_rec_in.batch_id := p_batch_step_rec.batch_id;
         l_batch_header_rec_in.batch_type := gme_common_pvt.g_doc_type_batch;
         IF NOT gme_common_pvt.get_batch_header (
                 p_batch_header_rec   =>  l_batch_header_rec_in
                ,p_org_code           =>  p_org_code
                ,p_batch_type         =>  gme_common_pvt.g_doc_type_batch
                ,x_batch_header_rec   =>  l_batch_header_rec) THEN
            RETURN FALSE;
         ELSE
            l_batch_step_rec.batch_id := l_batch_header_rec.batch_id;
         END IF;
      END IF;
      
      IF NOT (gme_batch_steps_dbl.fetch_row (l_batch_step_rec
                                             ,l_batch_step_rec) ) THEN
         RAISE fnd_api.g_exc_error;
      END IF;

      l_batch_header_rec.batch_id := l_batch_step_rec.batch_id;
      IF NOT (gme_batch_header_dbl.fetch_row (l_batch_header_rec
                                             ,l_batch_header_rec) ) THEN
         RAISE fnd_api.g_exc_error;
      END IF;

      x_batch_header_rec := l_batch_header_rec;
      x_batch_step_rec := l_batch_step_rec;
      RETURN TRUE;
   EXCEPTION
      WHEN fnd_api.g_exc_error THEN
         IF g_debug <= gme_debug.g_log_error THEN
            gme_debug.put_line (   'Expected error '
                                || g_pkg_name
                                || '.'
                                || l_api_name);
         END IF;

         RETURN FALSE;
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
         RETURN FALSE;
   END get_batch_step;
   
   FUNCTION create_history (
      p_batch_header_rec   IN   gme_batch_header%ROWTYPE
     ,p_original_status    IN   NUMBER
     ,p_event_id           IN   NUMBER DEFAULT NULL)
      RETURN BOOLEAN
   IS
      l_ins_history         gme_batch_history%ROWTYPE;
      l_api_name   CONSTANT VARCHAR2 (30)               := 'CREATE_HISTORY';
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      l_ins_history.batch_id := p_batch_header_rec.batch_id;
      l_ins_history.orig_status := p_original_status;
      l_ins_history.new_status := p_batch_header_rec.batch_status;
      l_ins_history.event_id := p_event_id;
      l_ins_history.gl_posted_ind := 0;

      IF NOT (gme_batch_history_dbl.insert_row (l_ins_history, l_ins_history) ) THEN
         RETURN FALSE;
      ELSE
         RETURN TRUE;
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

         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
         RETURN FALSE;
   END create_history;

   FUNCTION close_period_check_flexible (
      p_org_id       IN              NUMBER
     ,p_trans_date   IN              DATE
     ,x_trans_date   OUT NOCOPY      DATE
     ,x_period_id    OUT NOCOPY      INTEGER)
      RETURN BOOLEAN
   IS
      l_period_id          INTEGER;
      l_open_past_period   BOOLEAN;
      l_api_name           VARCHAR2 (100) := 'close_period_check_flexible';
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      invttmtx.tdatechk (org_id                => p_org_id
                        ,transaction_date      => p_trans_date
                        ,period_id             => l_period_id
                        ,open_past_period      => l_open_past_period);

      IF l_open_past_period = FALSE THEN
         invttmtx.tdatechk (org_id                => p_org_id
                           ,transaction_date      => gme_common_pvt.g_timestamp
                           ,period_id             => l_period_id
                           ,open_past_period      => l_open_past_period);

         IF l_open_past_period = FALSE THEN
            RETURN FALSE;
         ELSE
            x_trans_date := gme_common_pvt.g_timestamp;
            x_period_id := l_period_id;
         END IF;
      ELSE
         x_trans_date := p_trans_date;
         x_period_id := l_period_id;
      END IF;

      RETURN TRUE;
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
         RETURN FALSE;
   END close_period_check_flexible;

   FUNCTION get_batchstep_rsrc (
      p_batchstep_rsrc_rec   IN              gme_batch_step_resources%ROWTYPE
     ,p_org_code             IN              VARCHAR2
     ,p_batch_no             IN              VARCHAR2
     ,p_batchstep_no         IN              NUMBER
     ,p_activity             IN              VARCHAR2
     ,p_resource             IN              VARCHAR2
     ,x_batchstep_rsrc_rec   OUT NOCOPY      gme_batch_step_resources%ROWTYPE)
      RETURN BOOLEAN
   IS
      CURSOR get_organization (l_org_code IN VARCHAR2)
      IS
         SELECT organization_id
           FROM mtl_parameters
          WHERE organization_code = p_org_code;

      CURSOR get_batch_id (l_org_id IN NUMBER, l_batch_no IN VARCHAR2)
      IS
         SELECT batch_id
           FROM gme_batch_header
          WHERE organization_id = l_org_id AND batch_no = l_batch_no;

      CURSOR get_batchstep_id (l_batch_id IN NUMBER, l_batchstep_no IN NUMBER)
      IS
         SELECT batchstep_id
           FROM gme_batch_steps
          WHERE batch_id = l_batch_id AND batchstep_no = l_batchstep_no;

      /* Join of gme_batch_step_resources and
      gme_batch_step_activities to get  unique BATCHSTEP_RESOURCE_ID */
      CURSOR gme_batchstep_resource_id (
         l_organization_id   IN   NUMBER
        ,l_batch_id          IN   NUMBER
        ,l_batchstep_id      IN   NUMBER
        ,l_activity          IN   VARCHAR2
        ,l_resource          IN   VARCHAR2)
      IS
         SELECT gbsr.batchstep_resource_id
           FROM gme_batch_step_resources gbsr
               ,gme_batch_step_activities gbsa
          WHERE gbsr.organization_id = l_organization_id
            AND gbsr.batch_id = l_batch_id
            AND gbsr.batchstep_id = l_batchstep_id
            AND gbsa.batch_id = l_batch_id
            AND gbsa.batchstep_id = l_batchstep_id
            AND gbsa.activity = l_activity
            AND gbsr.batchstep_activity_id = gbsa.batchstep_activity_id
            AND gbsr.resources = l_resource;

      CURSOR c_gbsr (l_batchstep_resource_id IN NUMBER)
      IS
         SELECT *
           FROM gme_batch_step_resources
          WHERE batchstep_resource_id = l_batchstep_resource_id;

      l_api_name          CONSTANT VARCHAR2 (30) := 'GET_BATCHSTEP_RSRC';
      l_organization_id            NUMBER;
      l_batch_id                   NUMBER;
      l_batchstep_id               NUMBER;
      l_batchstep_activity_id      NUMBER;
      l_batchstep_resource_id      NUMBER;
      btchstep_rsrc_fetch_err      EXCEPTION;
      invalid_organization         EXCEPTION;
      invalid_batch                EXCEPTION;
      invalid_batchstep            EXCEPTION;
      invalid_batchstep_activity   EXCEPTION;
      invalid_batchstep_resource   EXCEPTION;
      invalid_record               EXCEPTION;
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line (   'Entering FUNCTION '
                             || g_pkg_name
                             || '.'
                             || l_api_name);
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ' input organization_id  => '
                             || p_batchstep_rsrc_rec.organization_id);
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ' input batchstep rsrc id => '
                             || p_batchstep_rsrc_rec.organization_id);
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ' input org_code          => '
                             || p_org_code);
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ' input batch_no          => '
                             || p_batch_no);
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ' input batchstep_no      => '
                             || p_batchstep_no);
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ' input activity          => '
                             || p_activity);
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ' input resource          => '
                             || p_batchstep_rsrc_rec.resources);
      END IF;

      IF p_batchstep_rsrc_rec.batchstep_resource_id IS NOT NULL THEN
         OPEN c_gbsr (p_batchstep_rsrc_rec.batchstep_resource_id);

         FETCH c_gbsr
          INTO x_batchstep_rsrc_rec;

         IF c_gbsr%NOTFOUND THEN
            CLOSE c_gbsr;
            RAISE invalid_record;
         END IF;

         CLOSE c_gbsr;

         RETURN TRUE;
      ELSIF    (    p_org_code IS NULL
                AND p_batchstep_rsrc_rec.organization_id IS NULL)
            OR (p_batch_no IS NULL AND p_batchstep_rsrc_rec.batch_id IS NULL)
            OR (    p_batchstep_no IS NULL
                AND p_batchstep_rsrc_rec.batchstep_id IS NULL)
            OR (    p_activity IS NULL
                AND p_batchstep_rsrc_rec.batchstep_activity_id IS NULL)
            OR (    p_resource IS NULL
                AND p_batchstep_rsrc_rec.batchstep_resource_id IS NULL) THEN
         RAISE btchstep_rsrc_fetch_err;
      END IF;

      IF p_batchstep_rsrc_rec.organization_id IS NULL THEN
         IF g_debug <= gme_debug.g_log_statement THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ' retrieve org_id using '
                                || p_org_code);
         END IF;

         OPEN get_organization (p_org_code);

         FETCH get_organization
          INTO l_organization_id;

         IF get_organization%NOTFOUND THEN
            CLOSE get_organization;
            RAISE invalid_organization;
         END IF;

         CLOSE get_organization;
      ELSE
         l_organization_id := p_batchstep_rsrc_rec.organization_id;
      END IF;

      IF p_batchstep_rsrc_rec.batch_id IS NULL THEN
         IF g_debug <= gme_debug.g_log_statement THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ' retrieve batch_id using '
                                || p_batch_no);
         END IF;

         OPEN get_batch_id (l_organization_id, p_batch_no);

         FETCH get_batch_id
          INTO l_batch_id;

         IF get_batch_id%NOTFOUND THEN
            CLOSE get_batch_id;
            RAISE invalid_batch;
         END IF;

         CLOSE get_batch_id;
      ELSE
         l_batch_id := p_batchstep_rsrc_rec.batch_id;
      END IF;

      IF p_batchstep_rsrc_rec.batchstep_id IS NULL THEN
         IF g_debug <= gme_debug.g_log_statement THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ' retrieve batchstep_id using '
                                || p_batchstep_no);
         END IF;

         OPEN get_batchstep_id (l_batch_id, p_batchstep_no);

         FETCH get_batchstep_id
          INTO l_batchstep_id;

         IF get_batchstep_id%NOTFOUND THEN
            CLOSE get_batchstep_id;
            RAISE invalid_batchstep;
         END IF;

         CLOSE get_batchstep_id;
      ELSE
         l_batchstep_id := p_batchstep_rsrc_rec.batchstep_id;
      END IF;

      IF p_batchstep_rsrc_rec.batchstep_resource_id IS NULL THEN
         IF g_debug <= gme_debug.g_log_statement THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ' retrieve resource_id using org_id '
                                || l_organization_id
                                || ' and batch_id '
                                || l_batch_id
                                || ' and batchstep_id '
                                || l_batchstep_id
                                || ' and activity '
                                || p_activity
                                || ' and resource '
                                || p_resource);
         END IF;

         OPEN gme_batchstep_resource_id (l_organization_id
                                        ,l_batch_id
                                        ,l_batchstep_id
                                        ,p_activity
                                        ,p_resource);

         FETCH gme_batchstep_resource_id
          INTO l_batchstep_resource_id;

         IF gme_batchstep_resource_id%NOTFOUND THEN
            CLOSE gme_batchstep_resource_id;
            RAISE invalid_batchstep_resource;
         ELSE                               ---GME_BATCHSTEP_RESOURCE_ID%FOUND
            CLOSE gme_batchstep_resource_id;

            OPEN c_gbsr (l_batchstep_resource_id);

            FETCH c_gbsr
             INTO x_batchstep_rsrc_rec;

            IF c_gbsr%NOTFOUND THEN
               CLOSE c_gbsr;
               RAISE invalid_record;
            END IF;

            CLOSE c_gbsr;

            RETURN TRUE;
         END IF;                   ----GME_BATCHSTEP_RESOURCE_ID%NOTFOUND THEN
      END IF;  -----IF p_batchstep_rsrc_rec.BATCHSTEP_RESOURCE_ID IS NULL THEN

      RETURN TRUE;
   EXCEPTION
      WHEN invalid_record OR btchstep_rsrc_fetch_err OR invalid_organization OR invalid_batch OR invalid_batchstep OR invalid_batchstep_activity OR invalid_batchstep_resource THEN
         IF g_debug <= gme_debug.g_log_unexpected THEN
            gme_debug.put_line
               (   ' user defined exception in gme_common_pvt.get_resource function'
                || SQLERRM);
         END IF;
         --add by michael 2013-10-10 
         --Add message handing
         gme_common_pvt.log_message('GME_BATCHID_NOTFOUND');
         RETURN FALSE;
      WHEN OTHERS THEN
         IF g_debug <= gme_debug.g_log_unexpected THEN
            gme_debug.put_line
               (   ' when others: exception in gme_common_pvt.get_resource function'
                || SQLERRM);
         END IF;
         --add by michael 2013-10-10 
         --Add message handing         
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name); 
         RETURN FALSE;
   END get_batchstep_rsrc;

  /*======================================================================
--  PROCEDURE :
--   Check_validity_rule
--
--  DESCRIPTION:
--       This procedure validates the validity rule dates only
--
--  SYNOPSIS:
--  Pawan Kumar created for bug 3088739 to check validity rule dates */
--  Navin: changed based on GME_Reschedule_Batch_Step_TD
--  SivakumarG Bug#5111748 corrected the parameter value checking before
--  checking validity rule dates
--  Sunitha ch. Bug#5336007 checked the parameter p_validate_plan_dates_ind.If it is 1 then
--  validate planned start date only.If it is 0 then validate both
--  Sunitha ch. Bug 5404329 Changed the condition fron AND to OR 
--===================================================================== */
   FUNCTION check_validity_rule_dates (
      p_validity_rule_id   IN   NUMBER
     ,p_start_date         IN   DATE
     ,p_cmplt_date         IN   DATE
     ,p_batch_header_rec   IN   gme_batch_header%ROWTYPE
     ,p_validate_plan_dates_ind  IN NUMBER DEFAULT 0)
      RETURN BOOLEAN
   IS
      /* Cusror definitions */
      CURSOR get_validity_rule (v_validity_rule_id NUMBER)
      IS
         SELECT *
           FROM gmd_recipe_validity_rules
          WHERE recipe_validity_rule_id = v_validity_rule_id;

      -- Check if the validity rule status is either 'OBSOLETE' or 'ON HOLD'.
      CURSOR cur_get_status_type (v_validity_rule_id NUMBER)
      IS
         SELECT status_type
           FROM gmd_status gs, gmd_recipe_validity_rules grvr
          WHERE grvr.recipe_validity_rule_id = v_validity_rule_id
            AND status_code = grvr.validity_rule_status;

      /* Local variables */
      l_validity_rec          gmd_recipe_validity_rules%ROWTYPE;
      l_start_date            DATE;
      l_cmplt_date            DATE;
      l_status_type           gmd_recipe_validity_rules.validity_rule_status%TYPE;
      l_api_name     CONSTANT VARCHAR2 (30)        := 'VALIDATE_VALIDITY_RULE';
      /* EXCEPTION Definitions */
      invalid_validity_rule   EXCEPTION;
   BEGIN
      IF (NVL (g_debug, 0) IN
                       (gme_debug.g_log_statement, gme_debug.g_log_procedure) ) THEN
         gme_debug.put_line (g_pkg_name || '.' || l_api_name || ':'
                             || 'Entering');
      END IF;

      OPEN get_validity_rule (p_validity_rule_id);

      FETCH get_validity_rule INTO l_validity_rec;

      IF get_validity_rule%NOTFOUND THEN
         CLOSE get_validity_rule;
         gme_common_pvt.log_message ('GME_API_INVALID_VALIDITY');
         RETURN FALSE;
      END IF;

      CLOSE get_validity_rule;

      IF l_validity_rec.delete_mark = 1 THEN
         gme_common_pvt.log_message ('GME_API_INVALID_VALIDITY');
         RETURN FALSE;
      END IF;

      l_start_date := p_start_date;
      l_cmplt_date := p_cmplt_date;

      IF     p_start_date IS NULL
         AND NVL (gme_common_pvt.g_validate_plan_dates_ind, 0) = 1 THEN
         /* Validate only start date against validity rules AND start date is not passed */
         RETURN TRUE;
      END IF;

      IF p_start_date IS NULL AND p_cmplt_date IS NULL THEN
         l_start_date := SYSDATE;
         l_cmplt_date := SYSDATE;
      ELSIF p_start_date IS NULL THEN
         l_start_date := l_cmplt_date;
      ELSIF p_cmplt_date IS NULL THEN
         l_cmplt_date := l_start_date;
      END IF;

      /* Bug#5111748 
         parameter value 0: validate both planned dates
	                 1: validate planned start date only
       */	
      --Sunitha ch. Bug#5336007 checked the parameter p_validate_plan_dates_ind.If it is 1 then
      --validate planned start date only.If it is 0 then validate both
      --Sunitha ch. Bug#5404329 Changed the condition from AND to OR and changed the log message to 
      --GME_INVAL_VAL_RULE_DATES when validating for actual start date 
      IF NVL (gme_common_pvt.g_validate_plan_dates_ind, 0) = 1 OR NVL (p_validate_plan_dates_ind, 0) = 1 THEN
         IF    l_start_date < l_validity_rec.start_date
            OR l_start_date > NVL (l_validity_rec.end_date, l_start_date) THEN
	    IF  NVL (p_validate_plan_dates_ind, 0) = 1 THEN
              gme_common_pvt.log_message ('GME_INVAL_VAL_RULE_DATES');
	    ELSE
              gme_common_pvt.log_message ('GME_DATES_EXCEED_VALDTY_RULE');
	    END IF;
            RETURN FALSE;
         END IF;
      ELSE
         /* Validate both planned dates against validity rules*/
         IF    l_start_date < l_validity_rec.start_date
            OR l_start_date > NVL (l_validity_rec.end_date, l_start_date)
            OR l_cmplt_date < l_validity_rec.start_date
            OR l_cmplt_date > NVL (l_validity_rec.end_date, l_cmplt_date) THEN
            gme_common_pvt.log_message ('GME_DATES_EXCEED_VALDTY_RULE');
            RETURN FALSE;
         END IF;
      END IF;

      -- Navin: Check if the validity rule status is either 'OBSOLETE' or 'ON HOLD'.
      IF p_batch_header_rec.batch_id IS NOT NULL THEN
         OPEN cur_get_status_type (p_validity_rule_id);

         FETCH cur_get_status_type
          INTO l_status_type;

         CLOSE cur_get_status_type;

         IF l_status_type IN ('1000', '800') THEN
            IF     p_batch_header_rec.batch_status =
                                               gme_common_pvt.g_batch_pending
               AND p_batch_header_rec.batch_type = 0 THEN
               gme_common_pvt.log_message ('GME_VALIDITY_OBSO_OR_ONHOLD');
               RAISE invalid_validity_rule;
            ELSIF     p_batch_header_rec.batch_status =
                                                    gme_common_pvt.g_batch_wip
                  AND p_batch_header_rec.batch_type = 0 THEN
               gme_common_pvt.log_message ('GME_VALIDITY_OBSO_OR_ONHOLD1');
            END IF;
         END IF;                        /* l_status_type IN ('1000', '800') */
      END IF;                    /* p_batch_header_rec.batch_id IS NOT NULL */

      IF (NVL (g_debug, 0) IN
                       (gme_debug.g_log_statement, gme_debug.g_log_procedure) ) THEN
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ':'
                             || 'Exiting with TRUE');
      END IF;

      RETURN TRUE;
   EXCEPTION
      WHEN invalid_validity_rule THEN
        --logged the message before throw out exception,following exception will overried that message.
         --fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
         RETURN FALSE;
      WHEN OTHERS THEN
         IF (NVL (g_debug, 0) > 0) THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ':'
                                || ' OTHERS:'
                                || SQLERRM);
         END IF;

         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
         RETURN FALSE;
   END check_validity_rule_dates;

   /*======================================================================
   --  PROCEDURE :
   --   material_date_change
   --
   --  DESCRIPTION:
   --       This procedure will update the material required date of the material
   --       whose material_detail_id is passed based on the consumption/yield type
   --       and its step association. It will then update the required dates on
   --       any pending reservations, move order allocations as per the
   --       new material required date.
   --
   --       If any of the lots allocated to reservations, move order allocations
   --       happen to expire, this procedure will give appropriate error message to the user.
   --
   --  SYNOPSIS:
   --  02-Mar-2005 Navin Sinha : Created as part of changed based on GME_Reschedule_Batch_Step_TD build.
   --   
   --  G. Muratore   19-MAR-2015  Bug 20691997 
   --     Pass in secondary qty when updating a reservation.     
   --===================================================================== */

   /*Navin: Added new procedure to update the material required date*/
   PROCEDURE material_date_change (
      p_material_detail_id   IN              NUMBER
     ,p_material_date        IN              DATE
     ,x_return_status        OUT NOCOPY      VARCHAR2)
   IS
      l_api_name         CONSTANT VARCHAR2 (30)   := 'MATERIAL_DATE_CHANGE';
      reserv_mo_err               EXCEPTION;
      reserv_err                  EXCEPTION;
      mo_err                      EXCEPTION;
      reservation_api_failed      EXCEPTION;
      process_move_order_failed   EXCEPTION;
      delete_allocations_failed   EXCEPTION;
      query_reservations_error    EXCEPTION;      -- 4944024
      notify_CSR_err              EXCEPTION;      -- 4944024

      /* Bug 5016399 Added conditions organization_id and material details table for better performance */
      /* Bug#5590100 selecting only required columns and added join condition to match batch_id to avoid
         FTS and for better performance */       
      CURSOR cur_get_mo_lines
      IS
         SELECT mtrl.line_id, mtrl.inventory_item_id 
           FROM mtl_txn_request_lines mtrl, mtl_txn_request_headers mtrh, gme_material_details d
          WHERE d.material_detail_id = p_material_detail_id
            AND mtrl.organization_id = d.organization_id
            AND mtrh.organization_id = d.organization_id
            AND mtrl.inventory_item_id = d.inventory_item_id
            AND mtrl.txn_source_id = d.batch_id  
            AND mtrl.header_id = mtrh.header_id
            AND mtrl.txn_source_line_id = p_material_detail_id
            AND mtrh.move_order_type = gme_common_pvt.g_move_order_type;
      
      --FPbug#4912179 query modified to improve performance
      CURSOR cur_get_mo_allocations (p_line_id NUMBER)
      IS
         SELECT mtlt.* 
         FROM mtl_transaction_lots_temp mtlt,    
	      mtl_material_transactions_temp  mmtt 
         WHERE mmtt.move_order_line_id = p_line_id 
           AND mmtt.transaction_temp_id = mtlt.transaction_temp_id; 
           
	 /*SELECT mtlt.*
           FROM mtl_txn_request_lines mtrl
               ,mtl_transaction_lots_temp mtlt
               ,mtl_material_transactions_temp mmtt
          WHERE mtrl.line_id = p_line_id
            AND mtrl.txn_source_line_id = p_material_detail_id
            AND mtrl.line_id = mmtt.move_order_line_id
            AND mmtt.transaction_temp_id = mtlt.transaction_temp_id; */

      CURSOR cur_get_lot_date (p_item_id NUMBER, p_lot_number VARCHAR2)
      IS
         SELECT lots.expiration_date
           FROM mtl_lot_numbers lots
          WHERE lots.inventory_item_id = p_item_id
            AND lots.lot_number = p_lot_number;

      TYPE l_mo_lines_tbl_typ IS TABLE OF mtl_txn_request_lines%ROWTYPE
         INDEX BY BINARY_INTEGER;

      TYPE l_mo_line_allocations_tbl_typ IS TABLE OF mtl_transaction_lots_temp%ROWTYPE
         INDEX BY BINARY_INTEGER;

      --Bug#5590100 Begin
      --l_mo_lines_tbl              l_mo_lines_tbl_typ; commented the existing line
      TYPE mo_lines IS RECORD (
        line_id           NUMBER,
        inventory_item_id NUMBER
       );
      TYPE mo_lines_tab IS TABLE OF mo_lines INDEX BY BINARY_INTEGER;
      l_mo_lines_tbl              mo_lines_tab;
      
      --Bug#5590100 End
      l_mo_line_allocations_tbl   l_mo_line_allocations_tbl_typ;
      l_reservations_deleted      NUMBER                                  := 0;
      l_mo_deleted                NUMBER                                  := 0;
      l_mo_alloc_deleted          NUMBER                                  := 0;
      l_size                      NUMBER;
      l_loop_count_mo_lines       NUMBER;
      l_loop_count_mo_alloc       NUMBER;
      l_msg_count                 NUMBER;
      l_shelf_life_days           NUMBER;
      l_return_status             VARCHAR2 (1);
      l_msg_data                  VARCHAR2 (2000);
      l_material_date             DATE;
      l_lot_expiration_date       DATE;
      l_material_detail_rec       gme_material_details%ROWTYPE;
      l_batch_header_rec          gme_batch_header%ROWTYPE;
      
      l_rsv                       inv_reservation_global.mtl_reservation_rec_type;
      ---l_rsv_array                inv_reservation_global.mtl_reservation_tbl_type;  --------- Punit Kumar
      
      l_rsv_array                 gme_common_pvt.reservations_tab;
      l_to_rsv_rec                inv_reservation_global.mtl_reservation_rec_type;
      l_rsv_tbl                   inv_reservation_global.mtl_reservation_tbl_type;  -- 4944024               
      l_rsv_count                 NUMBER;    -- 4944024
      
      /* Punit Kumar */
      l_batchstep_rec             gme_batch_steps%ROWTYPE;
   BEGIN
      IF (NVL (g_debug, 0) IN
                       (gme_debug.g_log_statement, gme_debug.g_log_procedure) ) THEN
         gme_debug.put_line (g_pkg_name || '.' || l_api_name || ':'
                             || 'Entering');
         gme_debug.put_line (   'Value of p_material_detail_id : '
                             || p_material_detail_id
                             || ', p_material_date: '
                             || TO_CHAR (p_material_date
                                        ,'MM/DD/YYYY HH24:MI:SS') );
      END IF;

      /* Initially set the return status to success */
      x_return_status := fnd_api.g_ret_sts_success;
      -- Fetch material details.
      l_material_detail_rec.material_detail_id := p_material_detail_id;

      IF NOT gme_material_details_dbl.fetch_row (l_material_detail_rec
                                                ,l_material_detail_rec) THEN
         RAISE fnd_api.g_exc_error;
      END IF;

      l_batch_header_rec.batch_id := l_material_detail_rec.batch_id;

      IF NOT (gme_batch_header_dbl.fetch_row (l_batch_header_rec
                                             ,l_batch_header_rec) ) THEN
         RAISE fnd_api.g_exc_error;
      END IF;

      IF p_material_date IS NOT NULL THEN
         l_material_date := p_material_date;

         IF (NVL (g_debug, 0) IN
                       (gme_debug.g_log_statement, gme_debug.g_log_procedure) ) THEN
            gme_debug.put_line
               (   'Update existing Material Required Date : '
                || TO_CHAR (l_material_detail_rec.material_requirement_date
                           ,'MM/DD/YYYY HH24:MI:SS')
                || ' in Gme_material_details with new Material Required Date : '
                || TO_CHAR (p_material_date, 'MM/DD/YYYY HH24:MI:SS') );
         END IF;
      ELSE                                       /* p_material_date IS NULL */
         IF (NVL (g_debug, 0) IN
                       (gme_debug.g_log_statement, gme_debug.g_log_procedure) ) THEN
            gme_debug.put_line
               ('p_material_date is null calling gme_common_pvt.calc_mtl_req_date');
         END IF;

         IF get_assoc_step (p_material_detail_id
                           ,l_batchstep_rec.batchstep_id
                           ,l_batchstep_rec.step_status) THEN
            IF NOT gme_batch_steps_dbl.fetch_row (l_batchstep_rec
                                                 ,l_batchstep_rec) THEN
               RAISE fnd_api.g_exc_error;
            END IF;
         END IF;

         gme_common_pvt.calc_mtl_req_date
                                    (p_batch_header_rec      => l_batch_header_rec
                                    ,p_batchstep_rec         => l_batchstep_rec
                                    ,p_mtl_dtl_rec           => l_material_detail_rec
                                    ,x_mtl_req_date          => l_material_date
                                    ,x_return_status         => l_return_status);

         IF (NVL (g_debug, 0) IN
                       (gme_debug.g_log_statement, gme_debug.g_log_procedure) ) THEN
            gme_debug.put_line
               (   'Came back from gme_common_pvt.calc_mtl_req_date with status '
                || l_return_status);
         END IF;

         -- Error handling for deduce_material_date call.
         IF l_return_status <> fnd_api.g_ret_sts_success THEN
            RAISE fnd_api.g_exc_error;
         END IF;
      END IF;                                /* p_material_date IS NOT NULL */

      -- Update new material_required_date in gme_material_details.
      l_material_detail_rec.material_requirement_date := l_material_date;

      IF NOT gme_material_details_dbl.update_row
                                   (p_material_detail      => l_material_detail_rec) THEN
         RAISE fnd_api.g_exc_error;
      END IF;

      IF (NVL (g_debug, 0) IN
                       (gme_debug.g_log_statement, gme_debug.g_log_procedure) ) THEN
         gme_debug.put_line ('Came back from update of gme_material_details');
      END IF;

      IF (NVL (g_debug, 0) IN
                       (gme_debug.g_log_statement, gme_debug.g_log_procedure) ) THEN
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ':'
                             || 'Exiting with '
                             || x_return_status);
      END IF;

      /* BUG 4944024 BEGIN */
      /* Change of date a supply line could influence associated sales reservations so */
      /* we need to notify the sales representatve accordingly                         */
      /* ==============================================================================*/
      IF (NVL (g_debug, 0) IN
                       (gme_debug.g_log_statement, gme_debug.g_log_procedure) ) THEN
         gme_debug.put_line (' Assessing line type which is '||l_material_detail_rec.line_type);
      END IF;
      
      IF l_material_detail_rec.line_type <> gme_common_pvt.g_line_type_ing THEN /* -1 */ 
         IF (NVL (g_debug, 0) IN
                        (gme_debug.g_log_statement, gme_debug.g_log_procedure) ) THEN
            gme_debug.put_line (' Line is a source of supply so invoke Query_Prod_Supply_Reservations');
         END IF;
         
         GME_SUPPLY_RES_PVT.query_prod_supply_reservations 
           (p_matl_dtl_rec               => l_material_detail_rec                                    
           ,x_mtl_reservation_tbl        => l_rsv_tbl                                                             
           ,x_mtl_reservation_tbl_count  => l_rsv_count                  
           ,x_msg_count                  => l_msg_count                  
           ,x_msg_data                   => l_msg_data                         
           ,x_return_status              => l_return_status);         

         IF g_debug <= gme_debug.g_log_procedure THEN
            gme_debug.put_line
               (   g_pkg_name
                || '.'
                || l_api_name
                || 'Return status from query_prod_supply_reservations is '
                || l_return_status);
            gme_debug.put_line 
               (   g_pkg_name
                || '.'
                || l_api_name
                || ' number of reservations is     '
                || l_rsv_count);                    
         END IF;
         
         IF (l_return_status <> fnd_api.g_ret_sts_success) THEN
            RAISE query_reservations_error;
         END IF;
         
         FOR k IN 1 .. l_rsv_count LOOP
            gme_debug.put_line ('source line id is   => '||  l_rsv_tbl(k).demand_source_line_id);
            -- Issue notifications for any impacted sales order lines
            IF l_rsv_tbl(k).demand_source_type_id = 2 THEN -- sales
               IF g_debug <= gme_debug.g_log_procedure THEN
                  gme_debug.put_line ('Demand is from Sales ');
               END IF;
              
               GME_SUPPLY_RES_PVT.notify_CSR
                 ( P_Batch_id               =>    l_rsv_tbl(k).supply_source_header_id
                 , P_Batch_line_id          =>    l_rsv_tbl(k).supply_source_line_id
                 , P_So_line_id             =>    l_rsv_tbl(k).demand_source_line_id
                 , P_batch_trans_id         =>    NULL                   
                 , P_organization_id        =>    l_rsv_tbl(k).organization_id
                 , P_action_code            =>    'CHANGE_PLANNED_COMPLETION_DATE'
                 , X_return_status          =>    x_return_status
                 , X_msg_cont               =>    l_msg_count      
                 , X_msg_data               =>    l_msg_data );            
               
               IF g_debug <= gme_debug.g_log_procedure THEN
                  gme_debug.put_line  (  g_pkg_name || '.' 
                                     || l_api_name          
                                     || ' after calling notify_CSR  for date change status is '
                                     || x_return_status );
               END IF;
               
               IF (x_return_status IN
                          (fnd_api.g_ret_sts_error, fnd_api.g_ret_sts_unexp_error) ) THEN
                  RAISE notify_CSR_err;
               END IF;
            END IF;
         END LOOP;                                                                 
      END IF;
      /* BUG 4944024 END  */
      
      /*
       * No further processing of reservations and Move Orders are required in folllowing cases:
       *   1. batch_header_rec.update_inventory_ind is not set.
       *   2. if line_type is not Ingredient.
      */
      IF l_batch_header_rec.update_inventory_ind <> 'Y'
         OR l_material_detail_rec.line_type <>
                                        gme_common_pvt.g_line_type_ing /* -1 */ THEN
         IF (NVL (g_debug, 0) IN
                       (gme_debug.g_log_statement, gme_debug.g_log_procedure) ) THEN
            gme_debug.put_line
               (   'Returning no further processing of reservations and Move Orders are required. '
                || 'Because update_inventory_ind : '
                || l_batch_header_rec.update_inventory_ind
                || ' line_type : '
                || l_material_detail_rec.line_type);
         END IF;
         RETURN;
      END IF;

      /* Query reservations for the material_detail_id by calling Query_reservation. */
      l_rsv.demand_source_type_id := gme_common_pvt.g_txn_source_type;

      IF (NVL (g_debug, 0) IN
                       (gme_debug.g_log_statement, gme_debug.g_log_procedure) ) THEN
         gme_debug.put_line ('Calling Query Reservation.');
      END IF;

      /* start ,Punit Kumar  */
      gme_reservations_pvt.get_material_reservations
            (p_organization_id         => l_material_detail_rec.organization_id
            ,p_batch_id                => l_material_detail_rec.batch_id
            ,p_material_detail_id      => l_material_detail_rec.material_detail_id
            ,x_return_status           => l_return_status
            ,x_reservations_tbl        => l_rsv_array);
            
      l_size := l_rsv_array.COUNT;
      /* end */
      
      IF (NVL (g_debug, 0) IN
                       (gme_debug.g_log_statement, gme_debug.g_log_procedure) ) THEN
         gme_debug.put_line
                          (   'Came back from Query Reservation with status '
                           || l_return_status);
      END IF;

      IF l_size > 0 AND l_return_status = fnd_api.g_ret_sts_success THEN
         /* pending reservations found; check for expired lots*/
         FOR j IN 1 .. l_size LOOP
            -- Check for lot expiration.
            OPEN cur_get_lot_date (l_rsv_array (j).inventory_item_id
                                  ,l_rsv_array (j).lot_number);
            
            FETCH cur_get_lot_date
             INTO l_lot_expiration_date;
            
            CLOSE cur_get_lot_date;

            IF l_lot_expiration_date <= l_material_date THEN
               l_reservations_deleted := 1;
            
               /* Delete the reservation by calling
                * The parameters that will be assigned to identity the reservations
                * to be deleted are the same as used for querying the reservations*/
               IF (NVL (g_debug, 0) IN
                       (gme_debug.g_log_statement, gme_debug.g_log_procedure) ) THEN
                  gme_debug.put_line ('Calling Delete Reservation.');
               END IF;
            
               gme_reservations_pvt.delete_reservation
                           (p_reservation_id      => l_rsv_array (j).reservation_id
                           ,x_return_status       => l_return_status);
            
               IF (NVL (g_debug, 0) IN
                       (gme_debug.g_log_statement, gme_debug.g_log_procedure) ) THEN
                  gme_debug.put_line
                         (   'Came back from Delete Reservation with status '
                          || l_return_status);
               END IF;
            
               -- Error handling for delete_reservation call.
               IF l_return_status <> fnd_api.g_ret_sts_success THEN
                  RAISE reservation_api_failed;
               END IF;
            ELSE
               /* l_lot_expiration_date <= l_material_date */
               IF (NVL (g_debug, 0) IN
                       (gme_debug.g_log_statement, gme_debug.g_log_procedure) ) THEN
                  gme_debug.put_line
                     ('Lot expiration date > material date + l_shelf_life_days');
               END IF;
               
               /* For remaining reserved lots, update the material required
                  date with the l_material_date. Assign the new required date to
                  the corresponding parameter of p_to_rsv_rec */
               IF (NVL (g_debug, 0) IN
                       (gme_debug.g_log_statement, gme_debug.g_log_procedure) ) THEN
                  gme_debug.put_line
                       (   'Calling Update Reservation for reservation_id: '
                        || l_rsv_array (j).reservation_id
                        || ' requirement_date: '
                        || TO_CHAR (l_material_date, 'MM/DD/YYYY HH24:MI:SS') );
               END IF;

               gme_reservations_pvt.update_reservation
                          (p_reservation_id      => l_rsv_array (j).reservation_id
                          ,p_revision            => l_rsv_array (j).revision
                          ,p_subinventory        => l_rsv_array (j).subinventory_code
                          ,p_locator_id          => l_rsv_array (j).locator_id
                          ,p_lot_number          => l_rsv_array (j).lot_number
                          ,p_new_qty             => l_rsv_array (j).reservation_quantity
                          ,p_new_uom             => l_rsv_array (j).reservation_uom_code
                          ,p_new_sec_qty         => l_rsv_array (j).secondary_reservation_quantity -- 20691997
                          ,p_new_date            => l_material_date
                          ,x_return_status       => l_return_status);
               
               IF (NVL (g_debug, 0) IN
                       (gme_debug.g_log_statement, gme_debug.g_log_procedure) ) THEN
                  gme_debug.put_line
                         (   'Came back from Update Reservation with status '
                          || l_return_status);
               END IF;

               -- Error handling for update reservation call.
               IF l_return_status <> fnd_api.g_ret_sts_success THEN
                  RAISE reservation_api_failed;
               END IF;
            END IF;
            /* l_lot_expiration_date <= l_material_date*/
         END LOOP;
         --FPBug#4585491            /*  j in 1..l_size */
      ELSIF l_return_status <>  fnd_api.g_ret_sts_success THEN    /* If querying of reservations threw an error */
         RAISE reservation_api_failed;
      END IF; /* l_size > 0 and l_return_status = fnd_api.g_ret_sts_success */

      OPEN cur_get_mo_lines;

      FETCH cur_get_mo_lines
      BULK COLLECT INTO l_mo_lines_tbl;

      l_loop_count_mo_lines := cur_get_mo_lines%ROWCOUNT;

      CLOSE cur_get_mo_lines;

      FOR k IN 1 .. l_loop_count_mo_lines LOOP
         -- Update MO lines
         IF (NVL (g_debug, 0) IN
                       (gme_debug.g_log_statement, gme_debug.g_log_procedure) ) THEN
            gme_debug.put_line
               (   'Calling Process move order line for txn_source_line_id : '
                || l_material_detail_rec.material_detail_id
                || ' Date_required : '
                || TO_CHAR (l_material_date, 'MM/DD/YYYY HH24:MI:SS') );
         END IF;

         gme_move_orders_pvt.update_move_order_lines
            (p_batch_id                => l_material_detail_rec.batch_id
            ,p_material_detail_id      => l_material_detail_rec.material_detail_id
            ,p_new_qty                 => NULL
            ,p_new_date                => l_material_date
            ,p_invis_move_line_id      => NULL
            ,x_return_status           => l_return_status);

         IF (NVL (g_debug, 0) IN
                       (gme_debug.g_log_statement, gme_debug.g_log_procedure) ) THEN
            gme_debug.put_line
                    (   'Came back from Process move order line with status '
                     || l_return_status);
         END IF;

         -- Error handling for process_move_order_line.
         IF l_return_status <> fnd_api.g_ret_sts_success THEN
            RAISE process_move_order_failed;
         END IF;

         /* For each of the mo lines, get the corresponding allocations */
         OPEN cur_get_mo_allocations (l_mo_lines_tbl (k).line_id);

         FETCH cur_get_mo_allocations
         BULK COLLECT INTO l_mo_line_allocations_tbl;

         l_loop_count_mo_alloc := cur_get_mo_allocations%ROWCOUNT;

         CLOSE cur_get_mo_allocations;

         FOR j IN 1 .. l_loop_count_mo_alloc LOOP
            -- Check for lot expiration.
            OPEN cur_get_lot_date (l_mo_lines_tbl (k).inventory_item_id
                                  ,l_mo_line_allocations_tbl (j).lot_number);

            FETCH cur_get_lot_date
             INTO l_lot_expiration_date;

            CLOSE cur_get_lot_date;

            IF l_lot_expiration_date <= l_material_date THEN
               l_mo_deleted := 1;

               -- Delete mo alloacations
               IF (NVL (g_debug, 0) IN
                       (gme_debug.g_log_statement, gme_debug.g_log_procedure) ) THEN
                  gme_debug.put_line
                            (   'Delete mo alloacations for mo_line_id: '
                             || l_mo_line_allocations_tbl (j).transaction_temp_id);
               END IF;

               inv_mo_line_detail_util.delete_allocations
                  (x_return_status            => l_return_status
                  ,x_msg_data                 => l_msg_data
                  ,x_msg_count                => l_msg_count
                  ,p_mo_line_id               => l_mo_lines_tbl (k).line_id
                  ,p_transaction_temp_id      => l_mo_line_allocations_tbl (j).transaction_temp_id);

               IF (NVL (g_debug, 0) IN
                       (gme_debug.g_log_statement, gme_debug.g_log_procedure) ) THEN
                  gme_debug.put_line
                         (   'Came back from Delete Allocations with status '
                          || l_return_status);
               END IF;

               -- Error handling for process_move_order_line.
               IF l_return_status <> fnd_api.g_ret_sts_success THEN
                  RAISE delete_allocations_failed;
               END IF;
            END IF;
         END LOOP;                         /* j in 1..l_loop_count_mo_alloc */
      END LOOP;                            /* k IN 1..l_loop_count_mo_lines */

      /*Give error message to user based on the values of the 2 flags */
      IF l_reservations_deleted = 1 AND l_mo_deleted = 1 THEN
         RAISE reserv_mo_err;
      ELSIF l_reservations_deleted = 1 AND l_mo_deleted = 0 THEN
         RAISE reserv_err;
      ELSIF l_reservations_deleted = 0 AND l_mo_deleted = 1 THEN
         RAISE mo_err;
      END IF;

      IF (NVL (g_debug, 0) IN
                       (gme_debug.g_log_statement, gme_debug.g_log_procedure) ) THEN
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ':'
                             || 'Exiting with '
                             || x_return_status);
      END IF;
   EXCEPTION
      WHEN fnd_api.g_exc_error THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN reserv_mo_err THEN
         /* GME_EXPIRED_RESERV_MO_DELETED: Due to requirement date change,
             some reserved lots and move orders allocations expired and are hence deleted.
         */
         --correct message will be set in the calling proc.
	 --FPBug#4585491 changed status to B from W3
         x_return_status := 'B';
      WHEN reserv_err THEN
        --  Due to requirement date change, some reserved lots expired and are hence deleted.
         --correct message will be set in the calling proc.
	 --FPBug#4585491 changed status to R from W1
         x_return_status := 'R';
      WHEN mo_err THEN
         --  Due to requirement date change, some move orders allocations expired and are hence deleted.
         --correct message will be set in the calling proc.
         --FPBug#4585491 changed status to M from W2
         x_return_status := 'M';
      WHEN reservation_api_failed OR process_move_order_failed OR delete_allocations_failed THEN
         x_return_status := l_return_status;
      WHEN query_reservations_error OR notify_CSR_err THEN                 -- 4944024  BEGIN
         IF g_debug <= gme_debug.g_log_statement THEN
           gme_debug.put_line 
             (   g_pkg_name
                || '.'
                || l_api_name
                || 'Error is :'
                || l_msg_data);                                                  
         END IF;                                                           -- 4944024  END
      WHEN OTHERS THEN
         x_return_status := fnd_api.g_ret_sts_unexp_error;
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);

         IF (NVL (g_debug, 0) > 0) THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ':'
                                || ' OTHERS:'
                                || SQLERRM);
         END IF;
   END material_date_change;

   /*======================================================================
   --  PROCEDURE :
   --   Is_material_auto_release
   --
   --  DESCRIPTION:
   --       This procedure determines whether a material is auto release
   --       A material can be auto release if the release_type is auto
   --       or auto by step with no step dependencies.
   --  REQUIREMENTS
   --       Pass in the material_detail_id of the material that you are
   --       interested in.
   --
   --  SYNOPSIS:
   --
   --===================================================================== */
   FUNCTION is_material_auto_release (
      p_line_id   IN   gme_material_details.material_detail_id%TYPE)
      RETURN NUMBER
   IS
      l_release_type        gme_material_details.release_type%TYPE;
      l_dep_count           NUMBER;
      l_is_auto             NUMBER;
      l_api_name   CONSTANT VARCHAR2 (30)       := 'IS_MATERIAL_AUTO_RELEASE';
   BEGIN
      IF (NVL (g_debug, 0) IN
                       (gme_debug.g_log_statement, gme_debug.g_log_procedure) ) THEN
         gme_debug.put_line (g_pkg_name || '.' || l_api_name || ':'
                             || 'Entering');
      END IF;

      SELECT release_type
        INTO l_release_type
        FROM gme_material_details
       WHERE material_detail_id = p_line_id;

      l_is_auto := l_release_type;

      IF l_release_type = 0 THEN
         l_is_auto := 0;
      ELSIF l_release_type = 3 THEN
         SELECT COUNT (1)
           INTO l_dep_count
           FROM DUAL
          WHERE EXISTS (SELECT 1
                          FROM gme_batch_step_items
                         WHERE material_detail_id = p_line_id);

         IF l_dep_count = 0 THEN
            l_is_auto := 0;
         ELSE
            l_is_auto := 3;
         END IF;
      END IF;                                        /* l_release_type = 0  */

      IF (NVL (g_debug, 0) IN
                       (gme_debug.g_log_statement, gme_debug.g_log_procedure) ) THEN
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ':'
                             || 'Exiting with l_is_auto: '
                             || l_is_auto);
      END IF;

      RETURN l_is_auto;
   EXCEPTION
      WHEN OTHERS THEN
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);

         IF (NVL (g_debug, 0) > 0) THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ':'
                                || ' OTHERS:'
                                || SQLERRM);
         END IF;

         IF l_is_auto <> 0 OR l_is_auto <> 3 THEN
            l_is_auto := 9;
         END IF;

         RETURN l_is_auto;
   END is_material_auto_release;

   FUNCTION get_material_detail (
      p_material_detail_rec   IN              gme_material_details%ROWTYPE
     ,p_org_code              IN              VARCHAR2
     ,p_batch_no              IN              VARCHAR2
     ,p_batch_type            IN              NUMBER
     ,x_batch_header_rec      OUT NOCOPY      gme_batch_header%ROWTYPE
     ,x_material_detail_rec   OUT NOCOPY      gme_material_details%ROWTYPE)
      RETURN BOOLEAN
   IS
      l_batch_header_rec            gme_batch_header%ROWTYPE;
      l_material_detail_rec         gme_material_details%ROWTYPE;
      l_api_name                    VARCHAR2 (30)    := 'GET_MATERIAL_DETAIL';
      material_detail_fetch_error   EXCEPTION;
      batch_header_fetch_error      EXCEPTION;
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      IF     p_org_code IS NOT NULL
         AND p_batch_type IS NOT NULL
         AND p_batch_no IS NOT NULL THEN
         l_batch_header_rec.batch_no := p_batch_no;
         l_batch_header_rec.batch_type := p_batch_type;

         IF NOT gme_common_pvt.get_batch_header
                                   (p_batch_header_rec      => l_batch_header_rec
                                   ,p_org_code              => p_org_code
                                   ,p_batch_type            => p_batch_type
                                   ,x_batch_header_rec      => x_batch_header_rec) THEN
            RAISE batch_header_fetch_error;
         END IF;
      END IF;

      l_material_detail_rec := p_material_detail_rec;
      l_material_detail_rec.batch_id := x_batch_header_rec.batch_id;

      IF NOT (gme_material_details_dbl.fetch_row (l_material_detail_rec
                                                 ,x_material_detail_rec) ) THEN
         RAISE material_detail_fetch_error;
      END IF;

      l_batch_header_rec.batch_id := x_material_detail_rec.batch_id;

      IF NOT (gme_batch_header_dbl.fetch_row (l_batch_header_rec
                                             ,x_batch_header_rec) ) THEN
         RAISE batch_header_fetch_error;
      END IF;

      RETURN TRUE;
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
         RETURN FALSE;
   END get_material_detail;
   -- Pawan Kumar Added for bug 5138929
    FUNCTION check_close_period (
      p_org_id       IN              NUMBER
     ,p_trans_date   IN              DATE)
      RETURN BOOLEAN
   IS
      l_period_id          INTEGER;
      l_open_past_period   BOOLEAN;
      l_api_name           VARCHAR2 (100) := 'check_close_period';
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      invttmtx.tdatechk (org_id                => p_org_id
                        ,transaction_date      => p_trans_date
                        ,period_id             => l_period_id
                        ,open_past_period      => l_open_past_period);
       IF (l_period_id = 0) THEN
         FND_MESSAGE.SET_NAME('INV','INV_NO_OPEN_PERIOD');
         FND_MSG_PUB.Add;
         RETURN FALSE;  
       ELSIF (l_period_id = -1) THEN
         FND_MESSAGE.SET_NAME('INV', 'INV_RETRIEVE_PERIOD');
         FND_MSG_PUB.Add;
         RETURN FALSE;  
       ELSIF (l_period_id > 0) THEN
         RETURN TRUE;     
       END IF;
       RETURN TRUE;
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
         RETURN FALSE;
   END check_close_period;
   
    /*======================================================================
   --  PROCEDURE :
   --   Validate_batch
   --
   --  DESCRIPTION:
   --       This procedure fetch and validates basic batch attributes
   --       
   --  REQUIREMENTS
   --       Pass in the batch_id of the batch to fetched and vaildated.
   --
   --  SYNOPSIS:
   --
   --  G. Muratore    01-DEC-2008   Bug 7562848
   --     Added parameter to allow calling routine to bypass phantom check.
   --===================================================================== */
   
   Procedure Validate_batch (
      p_batch_header_rec   IN              gme_batch_header%ROWTYPE
     ,p_org_code           IN              VARCHAR2
     ,p_batch_type         IN              NUMBER
     ,p_check_phantom      IN              VARCHAR2 DEFAULT 'Y'
     ,x_batch_header_rec   OUT NOCOPY      gme_batch_header%ROWTYPE
     ,x_message_count      OUT NOCOPY      NUMBER
     ,x_message_list       OUT NOCOPY      VARCHAR2
     ,x_return_status      OUT NOCOPY      VARCHAR2)
     
     IS
     	l_api_name           VARCHAR2 (100) := 'validate_batch';
     	l_batch_header_rec            gme_batch_header%ROWTYPE;
     	
     	batch_header_fetch_error      EXCEPTION;
     BEGIN 
       IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
       END IF;
       /* Set the return status to success initially */
      x_return_status := fnd_api.g_ret_sts_success;
      l_batch_header_rec := p_batch_header_rec;
      
     /* IF (p_batch_header_rec.organization_id IS NULL AND p_org_code IS NULL) THEN
         gme_common_pvt.log_message(p_product_code => 'INV'
                                       ,p_message_code => 'INV_ORG_REQUIRED');
         RAISE fnd_api.g_exc_error;
      END IF; */
      IF p_batch_header_rec.batch_no IS NULL AND p_batch_header_rec.batch_id IS NULL THEN
         gme_common_pvt.log_message ('GME_MISSING_BATCH_IDENTIFIER');
         RAISE fnd_api.g_exc_error;
      END IF;
      IF NOT gme_common_pvt.get_batch_header
                                   (p_batch_header_rec      => p_batch_header_rec
                                   ,p_org_code              => p_org_code
                                   ,p_batch_type            => p_batch_type
                                   ,x_batch_header_rec      => l_batch_header_rec) THEN
          IF (g_debug = gme_debug.g_log_statement) THEN
             gme_debug.put_line (g_pkg_name||'.'||l_api_name|| ': batch not found ');
          END IF;
          --Comment following message which will overried the messages throw out from get_batch_header function
          --gme_common_pvt.log_message ('GME_BATCH_NOT_FOUND');
          RAISE batch_header_fetch_error;
       END IF;
                	
      /* Check for phantom batch */
      -- Bug 7562848 - Bypass phantom check if calling routine makes that request.
      IF NVL (l_batch_header_rec.parentline_id, 0) > 0 AND p_check_phantom = 'Y' THEN
         gme_common_pvt.log_message ('PM_INVALID_PHANTOM_ACTION');
         RAISE fnd_api.g_exc_error;
      END IF;
      
      /* Check for migrated batch */
      IF NVL (l_batch_header_rec.migrated_batch_ind, 'Y') = 'Y' THEN
         gme_common_pvt.log_message ('GME_MIGRATED_BATCH');
         RAISE fnd_api.g_exc_error;
      END IF;
      gme_common_pvt.g_error_count := 0;
      gme_common_pvt.g_setup_done :=
         gme_common_pvt.setup (p_org_id        => l_batch_header_rec.organization_id
                              ,p_org_code      => p_org_code);

      IF NOT gme_common_pvt.g_setup_done THEN
      	  IF (g_debug = gme_debug.g_log_statement) THEN
             gme_debug.put_line (g_pkg_name||'.'||l_api_name|| ':set up problem ');
          END IF;
         RAISE fnd_api.g_exc_error;
      ELSE
         l_batch_header_rec.organization_id :=
                                             gme_common_pvt.g_organization_id;
      END IF;
      x_batch_header_rec := l_batch_header_rec ;
      
      IF g_debug <= gme_debug.g_log_statement THEN
         gme_debug.put_line (   g_pkg_name || '.' || l_api_name || ':' || 
         'exiting batch_header_id = ' || x_batch_header_rec.batch_id); 
         gme_debug.put_line (   g_pkg_name || '.' || l_api_name || ':' || 
         'exiting organization_id = ' || x_batch_header_rec.organization_id);
      END IF;
     EXCEPTION 
     WHEN fnd_api.g_exc_error THEN
         x_return_status := fnd_api.g_ret_sts_error;
     WHEN  batch_header_fetch_error   THEN 
     	  x_return_status := fnd_api.g_ret_sts_error;
     WHEN OTHERS THEN
         IF g_debug <= gme_debug.g_log_unexpected THEN
            gme_debug.put_line (   'When others exception in '
                                || g_pkg_name|| '.'||l_api_name||'Error is '
                                || SQLERRM);
         END IF;
          x_return_status := fnd_api.g_ret_sts_unexp_error;
          fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
          	
     END Validate_batch;
     /*======================================================================
   --  PROCEDURE :
   --   Validate_material_detail
   --
   --  DESCRIPTION:
   --       This procedure fetch and validates basic material attributes
   --       
   --  REQUIREMENTS
   --       Pass in the material_id of the batch to fetched and vaildated.
   --
   --  SYNOPSIS:
   --
   --===================================================================== */
   
   Procedure Validate_material_detail (
      p_material_detail_rec   	IN              gme_material_details%ROWTYPE
     ,p_org_code              	IN              VARCHAR2
     ,p_batch_no              	IN              VARCHAR2
     ,p_batch_type            	IN              NUMBER
     ,x_batch_header_rec      	OUT NOCOPY      gme_batch_header%ROWTYPE
     ,x_material_detail_rec   	OUT NOCOPY      gme_material_details%ROWTYPE
     ,x_message_count      	OUT NOCOPY      NUMBER
     ,x_message_list       	OUT NOCOPY      VARCHAR2
     ,x_return_status      	OUT NOCOPY      VARCHAR2)
     
     IS
     	l_api_name           VARCHAR2 (100) := 'validate_material_detail';
     	l_batch_header_rec            	gme_batch_header%ROWTYPE;
     	l_material_detail_rec       	gme_material_details%ROWTYPE;
     	material_fetch_error      	EXCEPTION;
     BEGIN 
         IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;
      
      /* Set the return status to success initially */
      x_return_status := fnd_api.g_ret_sts_success;
      l_material_detail_rec 	:= p_material_detail_rec;
     
      
      /*IF (p_material_detail_rec.organization_id IS NULL AND p_org_code IS NULL) THEN
         gme_common_pvt.log_message(p_product_code => 'INV'
                                       ,p_message_code => 'INV_ORG_REQUIRED');
         RAISE fnd_api.g_exc_error;
      END IF; */
      IF NOT gme_common_pvt.get_material_detail
                          (p_material_detail_rec      => p_material_detail_rec
                          ,p_org_code                 => p_org_code
                          ,p_batch_no                 => p_batch_no
                          ,p_batch_type               => p_batch_type
                          ,x_batch_header_rec         => l_batch_header_rec
                          ,x_material_detail_rec      => l_material_detail_rec) THEN
                          	
         IF (g_debug = gme_debug.g_log_statement) THEN
             gme_debug.put_line (g_pkg_name||'.'||l_api_name|| ': batch not found ');
          END IF;
         
          RAISE material_fetch_error;                 	
         
      END IF;
      
      /* Bug#7157383 Commented the below code as this validation is not correct
       * in this place */                	
      /* Check for phantom batch 
      IF NVL (l_batch_header_rec.parentline_id, 0) > 0 THEN
         gme_common_pvt.log_message ('PM_INVALID_PHANTOM_ACTION');
         RAISE fnd_api.g_exc_error;
      END IF; */
      /* Check for migrated batch */
     IF NVL (l_batch_header_rec.migrated_batch_ind, 'Y') = 'Y' THEN
         gme_common_pvt.log_message ('GME_MIGRATED_BATCH');
         RAISE fnd_api.g_exc_error;
      END IF;
       gme_common_pvt.g_error_count := 0;
       gme_common_pvt.g_setup_done :=
          gme_common_pvt.setup (p_org_id        => l_batch_header_rec.organization_id
                               ,p_org_code      => p_org_code);
       
       IF NOT gme_common_pvt.g_setup_done THEN
       	  IF (g_debug = gme_debug.g_log_statement) THEN
              gme_debug.put_line (g_pkg_name||'.'||l_api_name|| ':set up problem ');
           END IF;
          RAISE fnd_api.g_exc_error;
       ELSE
          l_batch_header_rec.organization_id :=
                                              gme_common_pvt.g_organization_id;
       END IF;
      x_batch_header_rec := l_batch_header_rec ;
      x_material_detail_rec  := l_material_detail_rec ;
      IF g_debug <= gme_debug.g_log_statement THEN
         gme_debug.put_line (   g_pkg_name || '.' || l_api_name || ':' || 
         'exiting batch_header_id = ' || x_batch_header_rec.batch_id); 
         gme_debug.put_line (   g_pkg_name || '.' || l_api_name || ':' || 
         'exiting organization_id = ' || x_batch_header_rec.organization_id);
         gme_debug.put_line (   g_pkg_name || '.' || l_api_name || ':' || 
         'exiting mat det_id = ' || x_material_detail_rec.material_detail_id);
      END IF;
      
     EXCEPTION 
     WHEN fnd_api.g_exc_error THEN
         x_return_status := fnd_api.g_ret_sts_error;
     WHEN  material_fetch_error   THEN 
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
          gme_common_pvt.count_and_get (x_count        => x_message_count
                                       ,p_encoded      => fnd_api.g_false
                                       ,x_data         => x_message_list);	
     END Validate_material_detail;
     /*======================================================================
   --  PROCEDURE :
   --   Validate_batch_step
   --
   --  DESCRIPTION:
   --       This procedure fetch and validates basic batch step attributes
   --       
   --  REQUIREMENTS
   --       Pass in the batchstep_id of the batch to fetched and validated.
   --
   --  SYNOPSIS:
   --
   --  HISTORY:
   --       14-NOV-2011  G. Muratore    Bug 13356510 
   --       Commented out validation code that was incorrectly checking for phantom batch.
   --===================================================================== */   
   Procedure Validate_batch_step (
      p_batch_step_rec     IN              gme_batch_steps%ROWTYPE
     ,p_org_code           IN              VARCHAR2
     ,p_batch_no           IN              VARCHAR2
     ,x_batch_step_rec     OUT NOCOPY      gme_batch_steps%ROWTYPE
     ,x_batch_header_rec   OUT NOCOPY      gme_batch_header%ROWTYPE
     ,x_message_count      	OUT NOCOPY      NUMBER
     ,x_message_list       	OUT NOCOPY      VARCHAR2
     ,x_return_status      	OUT NOCOPY      VARCHAR2)
    
     IS
     	l_api_name           VARCHAR2 (100) := 'validate_batch_step';
     	l_batch_header_rec            	gme_batch_header%ROWTYPE;
     	l_batch_step_rec            	gme_batch_steps%ROWTYPE;
     	step_header_fetch_error      	EXCEPTION;
     BEGIN 
        IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      	END IF;
       	x_return_status := fnd_api.g_ret_sts_success;
      	     
      
        IF NOT gme_common_pvt.get_batch_step (
                 p_batch_step_rec     =>      p_batch_step_rec
                ,p_org_code           =>      p_org_code
                ,p_batch_no           =>      p_batch_no
                ,x_batch_step_rec     =>      l_batch_step_rec
                ,x_batch_header_rec   =>      l_batch_header_rec) THEN
            RAISE Step_header_fetch_error;
         END IF;
         
      /* Bug 13356510 - Commented the below code as this validation is not correct. */               	
      /* Check for phantom batch */
      -- IF NVL (l_batch_header_rec.parentline_id, 0) > 0 THEN
         -- gme_common_pvt.log_message ('PM_INVALID_PHANTOM_ACTION');
         -- RAISE fnd_api.g_exc_error;
      -- END IF;
      
      /* Check for migrated batch */
      IF NVL (l_batch_header_rec.migrated_batch_ind, 'Y') = 'Y' THEN
         gme_common_pvt.log_message ('GME_MIGRATED_BATCH');
         RAISE fnd_api.g_exc_error;
      END IF;
      
       gme_common_pvt.g_error_count := 0;
       gme_common_pvt.g_setup_done :=
          gme_common_pvt.setup (p_org_id        => l_batch_header_rec.organization_id
                               ,p_org_code      => p_org_code);
       
       IF NOT gme_common_pvt.g_setup_done THEN
       	  IF (g_debug = gme_debug.g_log_statement) THEN
              gme_debug.put_line (g_pkg_name||'.'||l_api_name|| ':set up problem ');
           END IF;
          RAISE fnd_api.g_exc_error;
       ELSE
          l_batch_header_rec.organization_id :=
                                              gme_common_pvt.g_organization_id;
       END IF;
       x_batch_header_rec := l_batch_header_rec ;
       x_batch_step_rec   := l_batch_step_rec;
      IF g_debug <= gme_debug.g_log_statement THEN
         gme_debug.put_line (   g_pkg_name || '.' || l_api_name || ':' || 
         'exiting batch_header_id = ' || x_batch_header_rec.batch_id); 
         gme_debug.put_line (   g_pkg_name || '.' || l_api_name || ':' || 
         'exiting organization_id = ' || x_batch_header_rec.organization_id);
        
      END IF;
      IF g_debug <= gme_debug.g_log_statement THEN
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ' BatchStep ID:  '
                             || x_batch_step_rec.batchstep_id);
      END IF;
     EXCEPTION
     WHEN fnd_api.g_exc_error THEN
         x_return_status := fnd_api.g_ret_sts_error;
     WHEN  Step_header_fetch_error   THEN 
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
          gme_common_pvt.count_and_get (x_count        => x_message_count
                                       ,p_encoded      => fnd_api.g_false
                                       ,x_data         => x_message_list);	
     END Validate_batch_step;

   /* Bug#5394232 Added the following procedure to default transaction date 
      p_invoke_mode values are 'E' and 'T'
        E - To get the default transaction date during any batch/step action and results 
	    in Material Exceptions.
	T - To get the default transaction date during manual transactions. This one has to be
	    be used from wrappers also
     */
   PROCEDURE fetch_trans_date (   
      p_material_detail_id IN            NUMBER
     ,p_invoke_mode        IN            VARCHAR2 DEFAULT 'T'
     ,x_trans_date         OUT NOCOPY    DATE
     ,x_return_status      OUT NOCOPY    VARCHAR2)
   IS
      l_api_name   CONSTANT VARCHAR2 (30) := 'fetch_trans_date';

      l_batchstep_id        NUMBER;
      l_ing_trans_date      DATE;
      l_prod_trans_date     DATE;
      l_trans_date          DATE;
      l_step_actual_start   DATE;
      l_step_actual_cmplt   DATE;

      l_material_detail_rec gme_material_details%ROWTYPE;
      l_batch_header_rec    gme_batch_header%ROWTYPE;

      error_fetch_material      EXCEPTION;
      error_fetch_batch_details EXCEPTION; 
      error_close_period        EXCEPTION;

      CURSOR cur_get_associated_step (v_material_detail_id NUMBER)
      IS
         SELECT batchstep_id
           FROM gme_batch_step_items
          WHERE material_detail_id = v_material_detail_id;

      CURSOR cur_fetch_step_dates (v_batchstep_id NUMBER)
      IS
         SELECT actual_start_date, actual_cmplt_date
           FROM gme_batch_steps
          WHERE batchstep_id = v_batchstep_id;
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'|| l_api_name);
      END IF;

      /* initializing the return status*/
      x_return_status := fnd_api.g_ret_sts_success;

      /* fetch the material detail record */
      l_material_detail_rec.material_detail_id := p_material_detail_id;

      IF NOT (gme_material_details_dbl.fetch_row(l_material_detail_rec, l_material_detail_rec)) THEN
         IF g_debug <= gme_debug.g_log_statement THEN
	   gme_debug.put_line ('Entering api ' || g_pkg_name || '.'||'Error in Fetching the material detail record');
	 END IF;
	 RAISE error_fetch_material;
      END IF;

      /*fetch the batch details*/
      l_batch_header_rec.batch_id := l_material_detail_rec.batch_id;
      IF NOT (gme_batch_header_dbl.fetch_row(l_batch_header_rec, l_batch_header_rec)) THEN
         IF g_debug <= gme_debug.g_log_statement THEN
	   gme_debug.put_line ('Entering api ' || g_pkg_name || '.'||'Error in Fetching the Batch Details');
	 END IF;
	 RAISE error_fetch_batch_details;
      END IF;

      IF l_material_detail_rec.release_type IN (gme_common_pvt.g_mtl_autobystep_release,
                                                gme_common_pvt.g_mtl_auto_release ) THEN 
          /*default ing and prod dates to batch actual dated*/
          l_ing_trans_date  :=  l_batch_header_rec.actual_start_date;
          l_prod_trans_date :=  l_batch_header_rec.actual_cmplt_date;

         IF l_material_detail_rec.release_type = gme_common_pvt.g_mtl_autobystep_release THEN
           /* check whether material line is associated to step */
           OPEN cur_get_associated_step (l_material_detail_rec.material_detail_id);
           FETCH cur_get_associated_step INTO l_batchstep_id;
           CLOSE cur_get_associated_step;
	
           IF l_batchstep_id IS NOT NULL THEN
            OPEN cur_fetch_step_dates (l_batchstep_id);
            FETCH cur_fetch_step_dates INTO l_step_actual_start, l_step_actual_cmplt;
            CLOSE cur_fetch_step_dates; 
	    
      	    l_ing_trans_date := l_step_actual_start;
            l_prod_trans_date := l_step_actual_cmplt;
           END IF;
	 END IF; /* auto by step check */

         IF l_material_detail_rec.line_type = -1 THEN    /*ingredient*/
	   l_trans_date := l_ing_trans_date;
	 ELSE
	   l_trans_date := l_prod_trans_date;
	 END IF; 
      ELSE
         /* Manual and Incremental Release Types.
	    Transacting manually it has to be defaulted to SYSDATE */
         IF p_invoke_mode = 'E' THEN
            -- l_trans_date := l_batch_header_rec.actual_cmplt_date;
            -- Bug 13881792 - If exception happened during IB then use the date.
            IF (gme_common_pvt.g_ib_timestamp_set = -1) THEN
               l_trans_date := gme_common_pvt.g_ib_timestamp_date;
            ELSE
               l_trans_date := l_batch_header_rec.actual_cmplt_date;
            END IF;
	 END IF;
      END IF; /*l_material_detail_rec.release_type*/

      /* check whether the date is in closed period or not */
      IF NOT gme_common_pvt.check_close_period(p_org_id     => l_batch_header_rec.organization_id
                                              ,p_trans_date => l_trans_date) THEN
        l_trans_date := SYSDATE;
      END IF;

      x_trans_date := NVL(l_trans_date,SYSDATE);

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
      END IF;
   EXCEPTION
      WHEN error_fetch_material OR error_fetch_batch_details THEN
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

         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
         x_return_status := fnd_api.g_ret_sts_unexp_error;
   END fetch_trans_date;
/*======================================================================
 * --  PROCEDURE :
 * --   check_oprn_effectivity_dates
 * --
 * --  DESCRIPTION:
 * --       This procedure validates the operation effectivity dates while
 * inserting
 * --         the step.
 * --
 * --  SYNOPSIS:
 * --    bug 6408612
 * --  Swapna K created for bug 6408612 to check operation effectivity dates */
--===================================================================== */
FUNCTION check_oprn_effectivity_dates (
     p_oprn_id         IN NUMBER
    ,p_start_date               IN DATE
    ,p_cmplt_date               IN DATE

    )
    RETURN BOOLEAN    IS

      /* Cusror definitions */
      Cursor get_oprn_effectivity (v_oprn_id NUMBER) IS
        SELECT *
        FROM   gmd_operations
        WHERE  oprn_id = v_oprn_id;


      /* Local variables */
      l_oprn_rec            GMD_OPERATIONS%ROWTYPE;
      l_start_date              DATE;
      l_cmplt_date              DATE;

      l_api_name       CONSTANT VARCHAR2 (30)   :=
'check_oprn_effectivity_dates';

      /* EXCEPTION Definitions */

BEGIN
      /* oprn_id would have been already validated by the time this function
 * is called so no need of when no-data-found */
        OPEN get_oprn_effectivity(p_oprn_id);
        FETCH get_oprn_effectivity INTO l_oprn_rec;
        CLOSE get_oprn_effectivity;
        l_start_date := p_start_date;
        l_cmplt_date := p_cmplt_date;

       --BAsed on the profile value 'GME:Validate plan dates' check the dates
         IF  NVL (gme_common_pvt.g_validate_plan_dates_ind, 0) = 0 THEN
      	/* Validate both planned dates against operation effectivity dates*/
      	   IF l_start_date < l_oprn_rec.EFFECTIVE_START_DATE OR
             l_start_date > nvl(l_oprn_rec.EFFECTIVE_END_DATE,l_start_date) OR
             l_cmplt_date < l_oprn_rec.EFFECTIVE_START_DATE OR
             l_cmplt_date > nvl(l_oprn_rec.EFFECTIVE_END_DATE,l_cmplt_date) THEN
             gme_common_pvt.log_message('GME_DATES_EXCEED_OPER_VALIDTY');
             RETURN FALSE;
           END IF;
        ELSE
      	   IF l_start_date < l_oprn_rec.EFFECTIVE_START_DATE OR
            l_start_date > nvl(l_oprn_rec.EFFECTIVE_END_DATE,l_start_date) THEN
              gme_common_pvt.log_message('GME_DATES_EXCEED_OPER_VALIDTY');
              RETURN FALSE;
           END IF;
         END IF;
         RETURN TRUE;
    EXCEPTION
        WHEN OTHERS THEN
            fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
            RETURN FALSE;

   END check_oprn_effectivity_dates;

     /*
     * Create a new procedure to support CBR for ASCP.
     *
     */
     PROCEDURE ERES_CALL(
                     p_key_label             IN VARCHAR2
                    ,p_key_value             IN VARCHAR2
                    ,p_event_name            IN VARCHAR2
                    ,p_event_key             IN VARCHAR2
                    ,p_parent_event_name     IN VARCHAR2
                    ,p_parent_event_key      IN VARCHAR2
                    ,p_raise_event           IN VARCHAR2 DEFAULT 'Y'
                    ,x_eres_message_count    OUT NOCOPY NUMBER
                    ,x_eres_message_list     OUT NOCOPY VARCHAR2
                    ,x_eres_return_status    OUT NOCOPY VARCHAR2
                      ) IS

     l_payload edr_eres_event_pub.ERES_EVENT_REC_TYPE;
     l_child_erecords edr_eres_event_pub.ERECORD_ID_TBL_TYPE;
     l_init_msg_list VARCHAR2(50);
     l_validation_level NUMBER;
     batch_eres_fail EXCEPTION;
     l_erecord_id NUMBER;

     BEGIN

    l_payload.EVENT_NAME := p_event_name;
    l_payload.EVENT_KEY := p_event_key;

    l_payload.PARAM_NAME_1 := 'PSIG_USER_KEY_LABEL';
    l_payload.PARAM_VALUE_1 := p_key_label;

    l_payload.PARAM_NAME_2 := 'PSIG_USER_KEY_VALUE';
    l_payload.PARAM_VALUE_2 := p_key_value;

    l_payload.PARAM_NAME_3 := 'PSIG_TRANSACTION_AUDIT_ID';
    l_payload.PARAM_VALUE_3 := -1;

    l_payload.PARAM_NAME_4 := '#WF_SOURCE_APPLICATION_TYPE';
    l_payload.PARAM_VALUE_4 := 'API';

    l_payload.PARAM_NAME_5 := '#WF_SIGN_REQUESTER';
    l_payload.PARAM_VALUE_5 := FND_GLOBAL.USER_NAME;

    l_payload.PARAM_NAME_6 := 'PARENT_EVENT_NAME';
    l_payload.PARAM_VALUE_6 := p_parent_event_name;

    l_payload.PARAM_NAME_7 := 'PARENT_EVENT_KEY';
    l_payload.PARAM_VALUE_7 := p_parent_event_key;

    IF( p_parent_event_name IS NOT NULL AND p_parent_event_key IS NOT NULL) THEN
    l_payload.PARAM_NAME_8 := 'PARENT_ERECORD_ID';
    l_payload.PARAM_VALUE_8  := nvl(gma_standard.GET_ERECORD_ID(p_parent_event_name,p_parent_event_key),-1);
    -- nvl is added for above parameter as for EDR Requirement for bug bug 18230984
    END IF;

     edr_eres_event_pub.RAISE_ERES_EVENT(
       p_api_version => 1.0
      ,p_init_msg_list => l_init_msg_list
      ,p_validation_level => l_validation_level
      ,x_return_status => x_eres_return_status
      ,x_msg_count => x_eres_message_count
      ,x_msg_data => x_eres_message_list
      ,p_child_erecords => l_child_erecords
      ,x_event=>l_payload
     );

     IF(x_eres_return_status<>'S') THEN
     RAISE batch_eres_fail;

     ELSE

      l_erecord_id := gma_standard.GET_ERECORD_ID(p_event_name,p_event_key);
      -- if condition is added for bug 18230984
      if nvl(l_erecord_id,0)>0 
      THEN
      -- First send the acknowledgement as ERROR from the base transaction
     		 EDR_TRANS_ACKN_PUB.SEND_ACKN
      		( p_api_version       => 1.0,
        	p_init_msg_list     => 'T',
          x_return_status     => x_eres_return_status,
          x_msg_count         => x_eres_message_count,
          x_msg_data          => x_eres_message_list,
          p_event_name        => p_event_name,
          p_event_key         => p_event_key,
          p_erecord_id        => l_erecord_id,
          p_trans_status      => 'ERROR',         -- Acknowledgement is ERROR
          p_ackn_by           => '',
          p_ackn_note         => '',
          p_autonomous_commit => 'T'              -- Ackn is AUTONOMOUS
          );

          EDR_TRANS_ACKN_PUB.SEND_ACKN
          ( p_api_version       => 1.0,
            p_init_msg_list     => 'T',
            x_return_status     => x_eres_return_status,
            x_msg_count         => x_eres_message_count,
            x_msg_data          => x_eres_message_list,
            p_event_name        => p_event_name,    
            p_event_key         => p_event_key,
            p_erecord_id        => l_erecord_id,
            p_trans_status      => 'SUCCESS',         -- Acknowledgement is SUCCESS
            p_ackn_by           => '',
            p_ackn_note         => '',
            p_autonomous_commit => 'F'                -- Ackn is NON-AUTONOMOUS
              );

         END IF;
      END IF;
     EXCEPTION
     WHEN batch_eres_fail THEN
     gme_debug.put_line (   'Batch ERES Failure exception in '
                                || g_pkg_name
                                || '.'
                                || 'ERES_CALL'
                                || ' Error is :'
                                || SQLERRM);

     END ERES_CALL;
     
     --Added by QZENG ER 20985387, Get the current onhold status, R: Run, S: Stop, P: Pause
     FUNCTION GET_BATCH_HOLD_STATUS(p_batch_id IN NUMBER, p_date IN DATE DEFAULT SYSDATE) RETURN VARCHAR2
     IS
     l_hold_status VARCHAR2(1);
     CURSOR get_batch_onhold(v_batch_id NUMBER, v_date DATE)
         IS
     SELECT HOLD_TYPE
       FROM GME_BATCH_ONHOLD_DTLS
      WHERE BATCH_ID=v_batch_id
        AND NVL(START_DATE,SYSDATE) <= v_date
        AND NVL(END_DATE,SYSDATE) >= v_date
        AND rownum = 1 
     ORDER BY HOLD_ID DESC;
     BEGIN
       OPEN get_batch_onhold(p_batch_id,p_date);
       IF get_batch_onhold%NOTFOUND THEN
         l_hold_status := 'R';
       ELSE
         FETCH get_batch_onhold INTO l_hold_status;
       END IF;
       CLOSE get_batch_onhold;
       IF l_hold_status IS NULL THEN
         l_hold_status := 'R';
       END IF;
       RETURN l_hold_status;
     END GET_BATCH_HOLD_STATUS;
     
     
    /*======================================================================
     * --  PROCEDURE :
     * --   validate_resource_txn_onhold
     * --
     * --  DESCRIPTION:
     * --  Added for Batch On Hold enhancement,it's used to check whether
     * --  the transaction date of resource txn is within the period of held batch,
     * --  and also check whether there is overlap between the period of resource txn       
     * --  and the period of held batch
     * --  SYNOPSIS:
     * --    
     * --  
    --===================================================================== */     
    FUNCTION validate_resource_txn_onhold(p_batch_id    IN NUMBER,
                                           p_trans_date  IN DATE DEFAULT SYSDATE,
                                           p_start_date  IN DATE DEFAULT SYSDATE,
                                           p_end_date    IN DATE DEFAULT SYSDATE) RETURN VARCHAR2
     IS
       l_hold_status  VARCHAR2(1);

       CURSOR get_batch_onhold(v_batch_id NUMBER, v_date DATE)
           IS
       SELECT hold_type
         FROM gme_batch_onhold_dtls
        WHERE batch_id=v_batch_id
          AND NVL(start_date,SYSDATE) <= v_date
          AND NVL(end_date,SYSDATE) >= v_date
          AND rownum = 1
       ORDER BY HOLD_ID DESC;

       CURSOR validate_hold_start_date(v_batch_id NUMBER,v_start_date DATE,v_end_date DATE)
           IS
        SELECT hold_type
          FROM gme_batch_onhold_dtls
         WHERE batch_id=v_batch_id
           AND NVL(start_date,SYSDATE) >= v_start_date
           AND NVL(start_date,SYSDATE) <= v_end_date
           AND rownum = 1;



     BEGIN
       --initial l_hold_status
       l_hold_status := 'R';
       --check current hold type,if the current hold type is S,return the hold type directly
       OPEN get_batch_onhold(p_batch_id,SYSDATE);
       FETCH get_batch_onhold INTO l_hold_status;
       CLOSE get_batch_onhold;
       IF l_hold_status = 'S' THEN
         RETURN l_hold_status;
       END IF;

       /*if current hold type is R or P,then check:
         1.whether trans_date is within the period of held batch or not,if exists,return the hold type directly.
         2.whether start_date is within the period of held batch or not,if exists,return the hold type directly.
         3.whether end_date is within the period of held batch or not,if exists,return the hold type directly.
         4.if trans_date,start_date,end_date are not within the period of the held batch,we need to check whether
           the start_date of the held record is within the period of the resource txn,if exists,return the hold type directly.
      */
      --1.
       OPEN get_batch_onhold(p_batch_id,p_trans_date);
       FETCH get_batch_onhold INTO l_hold_status;
       IF get_batch_onhold%FOUND THEN
         CLOSE get_batch_onhold;
         RETURN l_hold_status;
       END IF;
       CLOSE get_batch_onhold;
      --2.
       OPEN get_batch_onhold(p_batch_id,p_start_date);
       FETCH get_batch_onhold INTO l_hold_status;
       IF get_batch_onhold%FOUND THEN
         CLOSE get_batch_onhold;
         RETURN l_hold_status;
       END IF;
       CLOSE get_batch_onhold;
      --3.
       OPEN get_batch_onhold(p_batch_id,p_end_date);
       FETCH get_batch_onhold INTO l_hold_status;
       IF get_batch_onhold%FOUND THEN
         CLOSE get_batch_onhold;
         RETURN l_hold_status;
       END IF;
       CLOSE get_batch_onhold;
      --4.
       OPEN validate_hold_start_date(p_batch_id,p_start_date,p_end_date);
       FETCH validate_hold_start_date INTO l_hold_status;
       IF validate_hold_start_date%FOUND THEN
         CLOSE validate_hold_start_date;
         RETURN l_hold_status;

       END IF;
       CLOSE validate_hold_start_date;
       --If all of validation above are passed,return R
       RETURN 'R';

     EXCEPTION
       WHEN OTHERS THEN
         RETURN l_hold_status;

     END validate_resource_txn_onhold;     
END gme_common_pvt;
/

COMMIT ;
EXIT;
--show errors;

