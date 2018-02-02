/* +======================================================================+ */
/* |    Copyright (c) 2005, 2016 Oracle and/or its affiliates.           | */
/* |                         All rights reserved.                         | */
/* |                           Version 12.0.0                             | */
/* +======================================================================+ */
REM dbdrv: sql ~PROD ~PATH ~FILE none none none package &phase=pls \
REM dbdrv: checkfile(120.17.12010000.18=120.23.12020000.12)(120.13.12000000.16=120.17.12010000.12)(120.13.12000000.13=120.17.12010000.9)(120.13.12000000.9=120.17.12010000.5)(120.13.12000000.11=120.23):~PROD:~PATH:~FILE

SET VERIFY OFF
WHENEVER SQLERROR EXIT FAILURE ROLLBACK;
WHENEVER OSERROR EXIT FAILURE ROLLBACK;

CREATE OR REPLACE PACKAGE gme_common_pvt AUTHID CURRENT_USER AS
/* $Header: GMEVCMNS.pls 120.23.12020000.12 2016/10/21 17:04:44 gmurator ship $ */

   --Package/GLOBAL variables for GME
   /* APPS related global variables */
   g_login_id                           NUMBER (10);
   g_user_ident                         NUMBER (10);
   g_user_name                          VARCHAR2 (240);
   /* INV related org parameters */
   g_organization_code                  VARCHAR2 (3);
   g_organization_id                    NUMBER (10);
   g_calendar_code                      VARCHAR2 (10);
   g_allow_neg_inv                      NUMBER (1);
   g_org_locator_control                NUMBER;
   /* GMD related org parameters */
   g_plant_ind                          NUMBER (1);
   g_lab_ind                            NUMBER (1);
   g_yield_type                         VARCHAR2 (10);
   g_release_type                       NUMBER (5);
   g_byprod_active                      NUMBER (1);
   g_mass_um_type                       VARCHAR2(10);  
   g_volume_um_type                     VARCHAR2(10); 
   /* GME related org parameters */
   g_auto_consume_supply_sub_only       NUMBER (1);
   g_subinv_loc_ind                     NUMBER (1);  --Bug#5663458
   g_supply_subinventory                VARCHAR2 (10);
   g_supply_locator_id                  NUMBER (10);
   g_yield_subinventory                 VARCHAR2 (10);
   g_yield_locator_id                   NUMBER (10);
   g_delete_material_ind                NUMBER (1);
   g_validate_plan_dates_ind            NUMBER (1);
   
   --Bug#5111078
   g_ib_factor_ind                      NUMBER (1);
   g_display_unconsumed_material        NUMBER (1);
   g_step_controls_batch_sts_ind        NUMBER (1);
   g_backflush_rsrc_usg_ind             NUMBER (1);
   g_def_actual_rsrc_usg_ind            NUMBER (1);
   g_calc_interim_rsrc_usg_ind          NUMBER (1);
   g_allow_qty_below_min_ind            NUMBER (1);
   g_display_non_work_days_ind          NUMBER (1);
   g_check_shortages_ind                NUMBER (1);
   g_copy_formula_text_ind              NUMBER (1);
   g_copy_routing_text_ind              NUMBER (1);
   g_create_high_level_resv_ind         NUMBER (1);
   g_create_move_orders_ind             NUMBER (1);
   g_reservation_timefence              NUMBER (5);
   g_move_order_timefence               NUMBER (5);
   g_batch_doc_numbering                NUMBER (1);
   g_batch_no_last_assigned             NUMBER (32);
   g_fpo_doc_numbering                  NUMBER (1);
   g_fpo_no_last_assigned               NUMBER (32);
   g_rule_based_resv_horizon            NUMBER (5);
   --nsinghi bug#5674398 Added the global variable for ingr subs date type
   g_ingr_sub_date			NUMBER (1);

   --added by qzeng for bug 16457668 for bulk validation for batch open interface
   g_bulk_validation_done               VARCHAR(1)                     := 'N';

   -- Bug 14634379 - New auto detail parameter introduced.
   g_auto_detail_batch                  NUMBER (1);

   -- Bug 17722332 -- New parameter for picking rule
   g_pick_slip_grouping_rule_id         NUMBER;
   
   /*Bug 21208206  
     Add this global variable for batch on hold enhancement to identify whether the
     resource txn record is created automatically by program including complete step
     action and incremental backflush
   */
   g_atuo_resource_txn_flag             VARCHAR(1)                     := 'N';        

   -- Bug 23176130 - New parm to control auto detailing of phantom batches.
   g_auto_detail_phantom_batches        NUMBER (1);
   
   /* Global Variables */
   g_max_errors                CONSTANT NUMBER                         := 100;
   g_exceptions_err            CONSTANT VARCHAR2 (1)                   := 'X';
   g_inv_short_err             CONSTANT VARCHAR2 (1)                   := 'V';
   g_not_transactable          CONSTANT VARCHAR2 (1)                   := 'T';
   g_setup_done                         BOOLEAN                      := FALSE;
   g_timestamp                          DATE;
   
   -- Bug 13070352
   g_gmf_vib_err               CONSTANT VARCHAR2 (10)                  := 'GMF_ERR';
   
   -- Bug 8751983 - New globals for IB. To be used for resource reversals.
   g_ib_timestamp_set                   NUMBER                           := 0;
   g_ib_timestamp_date                  DATE;

   -- Bug 19868921 - New global for IB. To be used for status checking during partial negative IB.
   g_ib_going_negative                  NUMBER                           := 0;

   g_transaction_header_id              NUMBER;
   g_error_count                        NUMBER;
   g_batch_scale_factor                 NUMBER;
   g_routing_scale_factor               NUMBER;
   g_doc_type_batch            CONSTANT NUMBER                           := 0;
   g_doc_type_fpo              CONSTANT NUMBER                          := 10;
   g_line_type_ing             CONSTANT NUMBER                          := -1;
   g_line_type_prod            CONSTANT NUMBER                           := 1;
   g_line_type_byprod          CONSTANT NUMBER                           := 2;
   g_hour_uom_code                      VARCHAR2 (3);
   g_precision                 CONSTANT NUMBER                           := 5;
   g_pairs_reversal_type       CONSTANT NUMBER                           := 1;
   g_pairs_phantom_type        CONSTANT NUMBER                           := 2;
   g_check_primary_rsrc                 NUMBER                           := 0;
   g_flex_validate_prof                 NUMBER                           := 0;
   /* Transaction types for GME */
   g_txn_source_type           CONSTANT NUMBER                           := 5;
   g_ing_issue                 CONSTANT NUMBER                          := 35;
   g_ing_return                CONSTANT NUMBER                          := 43;
   g_prod_completion           CONSTANT NUMBER                          := 44;
   g_prod_return               CONSTANT NUMBER                          := 17;
   g_byprod_completion         CONSTANT NUMBER                        := 1002;
   g_byprod_return             CONSTANT NUMBER                        := 1003;
   /* Transaction actions */
   g_ing_issue_txn_action      CONSTANT NUMBER                           := 1;
   g_ing_ret_txn_action        CONSTANT NUMBER                          := 27;
   g_prod_comp_txn_action      CONSTANT NUMBER                          := 31;
   g_prod_ret_txn_action       CONSTANT NUMBER                          := 32;
   g_byprod_comp_txn_action    CONSTANT NUMBER                          := 31;
   g_byprod_ret_txn_action     CONSTANT NUMBER                          := 32;
   g_move_to_temp                       VARCHAR2 (1)        := fnd_api.g_true;
   g_batch_status_check                 VARCHAR2 (1)        := fnd_api.g_true;
   g_temp_table                CONSTANT NUMBER                           := 2;
   g_interface_table           CONSTANT NUMBER                           := 1;
   /* Wip Entity Types for GME */
   g_wip_entity_type_batch     CONSTANT NUMBER                          := 10;
   g_wip_entity_type_fpo       CONSTANT NUMBER                           := 9;
   /* Tree modes */
   g_tree_reservation_mode     CONSTANT NUMBER                           := 1;
   g_tree_transaction_mode     CONSTANT NUMBER                           := 2;
   /* Move Order Types for GME */
   g_move_order_type           CONSTANT NUMBER                           := 5;
   g_invis_move_order_type     CONSTANT NUMBER                           := 8;
   g_backflush_transfer        CONSTANT NUMBER                          := 51;
   /* Batch Status */
   g_batch_pending             CONSTANT NUMBER                           := 1;
   g_batch_wip                 CONSTANT NUMBER                           := 2;
   g_batch_completed           CONSTANT NUMBER                           := 3;
   g_batch_closed              CONSTANT NUMBER                           := 4;
   g_batch_cancelled           CONSTANT NUMBER                          := -1;
   /* Step Status */
   g_step_pending              CONSTANT NUMBER                           := 1;
   g_step_wip                  CONSTANT NUMBER                           := 2;
   g_step_completed            CONSTANT NUMBER                           := 3;
   g_step_closed               CONSTANT NUMBER                           := 4;
   g_step_cancelled            CONSTANT NUMBER                           := 5;
   /* Material release type */
   g_mtl_auto_release          CONSTANT NUMBER                           := 0;
   g_mtl_manual_release        CONSTANT NUMBER                           := 1;
   g_mtl_incremental_release   CONSTANT NUMBER                           := 2;
   g_mtl_autobystep_release    CONSTANT NUMBER                           := 3;
   /* Step release types */
   g_auto_step_release         CONSTANT NUMBER                           := 2;
   g_manual_step_release       CONSTANT NUMBER                           := 1;
   /* Document numbering */
   g_auto_doc_numbering        CONSTANT NUMBER                           := 2;
   g_manual_doc_numbering      CONSTANT NUMBER                           := 1;
   /* Phantom definitions */
   g_auto_phantom              CONSTANT NUMBER                           := 1;
   g_manual_phantom            CONSTANT NUMBER                           := 2;
   /* Step dependency types */
   g_dep_type_finish_start     CONSTANT NUMBER                           := 0;
   g_dep_type_start_start      CONSTANT NUMBER                           := 1;
   
   /* Bug 5763818 - Added eres constants */
   g_batch_created       CONSTANT VARCHAR2(50)  := 'oracle.apps.gme.batch.created';
   g_batch_reservations  CONSTANT VARCHAR2(50)  := 'oracle.apps.gme.batch.reservations';
   
   g_batchmtl_added      CONSTANT VARCHAR2(50) := 'oracle.apps.gme.batchmtl.added';
   g_batchmtl_updated    CONSTANT VARCHAR2(50) := 'oracle.apps.gme.batchmtl.updated';
   g_batchmtl_removed    CONSTANT VARCHAR2(50) := 'oracle.apps.gme.batchmtl.removed';
   
   g_batchstep_created   CONSTANT VARCHAR2(50) := 'oracle.apps.gme.batchstep.created';
   g_batchstep_removed   CONSTANT VARCHAR2(50) := 'oracle.apps.gme.batchstep.removed';
   g_batchstep_added     CONSTANT VARCHAR2(50) := 'oracle.apps.gme.batchstep.added';
   g_batchstep_update    CONSTANT VARCHAR2(50) := 'oracle.apps.gme.batchstep.update';
   
   g_resource_added      CONSTANT VARCHAR2(50) := 'oracle.apps.gme.resource.added';
   g_resource_removed    CONSTANT VARCHAR2(50) := 'oracle.apps.gme.resource.removed';
   g_resource_update     CONSTANT VARCHAR2(50) := 'oracle.apps.gme.resource.update';
   
   g_activity_added      CONSTANT VARCHAR2(50) := 'oracle.apps.gme.activity.added';
   g_activity_removed    CONSTANT VARCHAR2(50) := 'oracle.apps.gme.activity.removed';
   g_activity_updated    CONSTANT VARCHAR2(50) := 'oracle.apps.gme.activity.updated';
   
   g_bstep_rel_wf        CONSTANT VARCHAR2(50) := 'oracle.apps.gme.bstep.rel.wf';
   
   /* GME row and table types */
   TYPE p_field IS RECORD (
      p_value   VARCHAR2 (50)
   );
   
-- nsinghi bug#5176319.
/* Material Transaction Header ID table for all materials within a batch. 
This table has been created to store the transaction_ids and corresponding material_detail_ids used during
unrelease_batch and unrelease_step. The issue was, during unrelease, the onhands should get updated first and 
then reservations should be recreated. To do this, it was required to have a pl/sql table that stores transaction_ids
and the material for which the reservation had to be created. Hence g_mat_txn_hdr_tbl has been created for the same.
*/
   -- Bug 19868921 - Added rsrv_quantity for partial negative IB.
   TYPE mat_txn_id_rec IS RECORD (
      txn_header_id   NUMBER,
      material_dtl_id NUMBER,
      rsrv_quantity   NUMBER
   );
   
   TYPE txn_hdr_tab IS TABLE OF mat_txn_id_rec INDEX BY BINARY_INTEGER;
   g_mat_txn_hdr_tbl	    txn_hdr_tab; 
--   g_txn_hdr_tbl_cnt	    NUMBER; -- nsinghi bug#5176319.

   TYPE field_values_tab IS TABLE OF p_field
      INDEX BY BINARY_INTEGER;

   TYPE batch_headers_tab IS TABLE OF gme_batch_header%ROWTYPE
      INDEX BY BINARY_INTEGER;

   TYPE material_details_tab IS TABLE OF gme_material_details%ROWTYPE
      INDEX BY BINARY_INTEGER;

   TYPE steps_tab IS TABLE OF gme_batch_steps%ROWTYPE
      INDEX BY BINARY_INTEGER;

   TYPE resource_txns_tab IS TABLE OF gme_resource_txns_gtmp%ROWTYPE
      INDEX BY BINARY_INTEGER;

   TYPE resources_tab IS TABLE OF gme_batch_step_resources%ROWTYPE
      INDEX BY BINARY_INTEGER;

   TYPE activities_tab IS TABLE OF gme_batch_step_activities%ROWTYPE
      INDEX BY BINARY_INTEGER;

   TYPE item_masters_tab IS TABLE OF mtl_system_items_kfv%ROWTYPE
      INDEX BY BINARY_INTEGER;

   TYPE exceptions_tab IS TABLE OF gme_exceptions_gtmp%ROWTYPE
      INDEX BY BINARY_INTEGER;

   TYPE recipe_validity_rule_tab IS TABLE OF gmd_recipe_validity_rules%ROWTYPE
      INDEX BY BINARY_INTEGER;

   TYPE mtl_trans_lots_inter_tbl IS TABLE OF mtl_transaction_lots_interface%ROWTYPE
      INDEX BY BINARY_INTEGER;

   TYPE mtl_trans_lots_num_tbl IS TABLE OF mtl_transaction_lot_numbers%ROWTYPE
      INDEX BY BINARY_INTEGER;

   TYPE mtl_mat_tran_tbl IS TABLE OF mtl_material_transactions%ROWTYPE
      INDEX BY BINARY_INTEGER;

   TYPE mtl_tran_int_tbl IS TABLE OF mtl_transactions_interface%ROWTYPE
      INDEX BY BINARY_INTEGER;

   TYPE reservations_tab IS TABLE OF mtl_reservations%ROWTYPE
      INDEX BY BINARY_INTEGER;

   TYPE mo_lines_tab IS TABLE OF mtl_txn_request_lines%ROWTYPE
      INDEX BY BINARY_INTEGER;

   TYPE resource_transactions_tab IS TABLE OF gme_resource_txns_gtmp%ROWTYPE
      INDEX BY BINARY_INTEGER;

   TYPE pending_lots_tab IS TABLE OF gme_pending_product_lots%ROWTYPE
      INDEX BY BINARY_INTEGER;

   TYPE text_tab IS TABLE OF gme_text_table%ROWTYPE
      INDEX BY BINARY_INTEGER;

   TYPE number_tab IS TABLE OF NUMBER
      INDEX BY BINARY_INTEGER;

   --Buffers for recipe upload
   routings                             gmd_recipe_fetch_pub.recipe_rout_tbl;
   routing_materials                    gmd_recipe_fetch_pub.recipe_rout_matl_tbl;
   steps                                gmd_recipe_fetch_pub.recipe_step_tbl;
   step_dependencies                    gmd_recipe_fetch_pub.routing_depd_tbl;
   activities                           gmd_recipe_fetch_pub.oprn_act_tbl;
   resources                            gmd_recipe_fetch_pub.oprn_resc_tbl;
   materials                            gmdfmval_pub.formula_detail_tbl;
   process_parameters                   gmd_recipe_fetch_pub.recp_resc_proc_param_tbl;

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
     ,p_product_code   IN   VARCHAR2 := 'GME');

   FUNCTION setup (p_org_id IN NUMBER := NULL, p_org_code IN VARCHAR2 := NULL)
      RETURN BOOLEAN;

   PROCEDURE set_timestamp;

   PROCEDURE set_who;

   PROCEDURE set_txn_header_id(p_txn_header_id NUMBER);
   
   PROCEDURE reset_txn_header_id;

   PROCEDURE reset_txn_hdr_tbl; --nsinghi bug#5176319.
   
   FUNCTION get_txn_header_id
      RETURN NUMBER;

   -- Bug 9938452 - Renamed procedure set_move_to_temp_off to set_move_to_temp_toggle.
   PROCEDURE set_move_to_temp_toggle(p_true IN NUMBER DEFAULT 0);

   FUNCTION get_timestamp
      RETURN DATE;

   PROCEDURE count_and_get (
      p_encoded   IN              VARCHAR2 := 'T'
     ,x_count     OUT NOCOPY      NUMBER
     ,x_data      OUT NOCOPY      VARCHAR2);

   PROCEDURE get_who (
      x_user_ident      OUT NOCOPY   NUMBER
     ,x_login_id        OUT NOCOPY   NUMBER
     ,x_timestamp       OUT NOCOPY   DATE
     ,x_return_status   OUT NOCOPY   VARCHAR2);

   PROCEDURE get_supply_defaults (
      p_organization_id     IN              NUMBER
     ,p_inventory_item_id   IN              NUMBER
     ,x_subinventory        OUT NOCOPY      VARCHAR2
     ,x_locator_id          OUT NOCOPY      NUMBER
     ,x_return_status       OUT NOCOPY      VARCHAR2);

   PROCEDURE get_yield_defaults (
      p_organization_id     IN              NUMBER
     ,p_inventory_item_id   IN              NUMBER
     ,p_line_type           IN              NUMBER
     ,x_subinventory        OUT NOCOPY      VARCHAR2
     ,x_locator_id          OUT NOCOPY      NUMBER
     ,x_return_status       OUT NOCOPY      VARCHAR2);

   PROCEDURE construct_material_detail (
      p_formula_detail_rec    IN              fm_matl_dtl%ROWTYPE
     ,p_item_master_rec       IN              mtl_system_items_kfv%ROWTYPE
     ,p_batch_header_rec      IN              gme_batch_header%ROWTYPE
     ,x_material_detail_rec   OUT NOCOPY      gme_material_details%ROWTYPE
     ,x_return_status         OUT NOCOPY      VARCHAR2);

   FUNCTION get_process_loss (
      p_batch_id                     IN   NUMBER DEFAULT NULL
     ,p_validity_rule_id             IN   NUMBER DEFAULT NULL
     ,p_organization_id              IN   NUMBER DEFAULT NULL
     ,p_total_output_qty_scaled      IN   NUMBER
     ,p_total_output_qty_pre_scale   IN   NUMBER)
      RETURN NUMBER;

   PROCEDURE create_document_no (
      p_batch_header_rec   IN              gme_batch_header%ROWTYPE
     ,x_batch_header_rec   IN OUT NOCOPY   gme_batch_header%ROWTYPE);

   FUNCTION calc_date_from_prod_rule (
      p_organization_id     IN              NUMBER
     ,p_inventory_item_id   IN              NUMBER
     ,p_item_qty            IN              NUMBER
     ,p_start_date          IN              DATE
     ,p_cmplt_date          IN              DATE
     ,x_start_date          IN OUT NOCOPY   DATE
     ,x_cmplt_date          IN OUT NOCOPY   DATE)
      RETURN BOOLEAN;

   PROCEDURE calc_mtl_req_date (
      p_batch_header_rec   IN              gme_batch_header%ROWTYPE
            DEFAULT NULL
     ,p_batchstep_rec      IN              gme_batch_steps%ROWTYPE
            DEFAULT NULL
     ,p_mtl_dtl_rec        IN              gme_material_details%ROWTYPE
     ,x_mtl_req_date       OUT NOCOPY      DATE
     ,x_return_status      OUT NOCOPY      VARCHAR2);

   FUNCTION is_qty_below_capacity (
      p_batch_step_resources_rec   IN   gme_batch_step_resources%ROWTYPE)
      RETURN BOOLEAN;

   FUNCTION resource_qty_below_capacity (p_batch_id IN NUMBER)
      RETURN BOOLEAN;

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
      RETURN NUMBER;

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
      RETURN BOOLEAN;

   FUNCTION check_subinventory (
      p_organization_id     IN   NUMBER
     ,p_subinventory        IN   VARCHAR2
     ,p_inventory_item_id   IN   NUMBER
     ,p_restrict_subinv     IN   NUMBER DEFAULT NULL)
      RETURN BOOLEAN;

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
      RETURN BOOLEAN;

   PROCEDURE get_batch_shortages (
      p_organization_id   IN              NUMBER
     ,p_batch_id          IN              NUMBER
     ,p_invoke_mode       IN              VARCHAR2
     ,p_tree_mode         IN              NUMBER
     ,x_return_status     OUT NOCOPY      VARCHAR2
     ,x_exception_tbl     OUT NOCOPY      gme_common_pvt.exceptions_tab);

   PROCEDURE get_open_qty (
      p_mtl_dtl_rec              IN              gme_material_details%ROWTYPE
     ,p_called_by                IN              VARCHAR2
     ,                            /* P- picking, R-reservation, S-shortages */
      p_item_location_control    IN              NUMBER DEFAULT NULL
     ,p_item_restrict_locators   IN              NUMBER DEFAULT NULL
     ,x_open_qty                 OUT NOCOPY      NUMBER
     ,x_return_status            OUT NOCOPY      VARCHAR2);

   FUNCTION insert_exceptions (p_exception_rec IN gme_exceptions_gtmp%ROWTYPE)
      RETURN BOOLEAN;

   FUNCTION populate_temp_from_struct (
      p_exception_tbl   IN   gme_common_pvt.exceptions_tab)
      RETURN BOOLEAN;

   FUNCTION is_material_assoc_to_step (
      p_material_detail_id   IN   gme_material_details.material_detail_id%TYPE)
      RETURN BOOLEAN;

   FUNCTION get_batch_header (
      p_batch_header_rec   IN              gme_batch_header%ROWTYPE
     ,p_org_code           IN              VARCHAR2
     ,p_batch_type         IN              NUMBER
     ,x_batch_header_rec   OUT NOCOPY      gme_batch_header%ROWTYPE)
      RETURN BOOLEAN;

   FUNCTION get_batch_step (
      p_batch_step_rec     IN              gme_batch_steps%ROWTYPE
     ,p_org_code           IN              VARCHAR2
     ,p_batch_no           IN              VARCHAR2
     ,x_batch_step_rec     OUT NOCOPY      gme_batch_steps%ROWTYPE
     ,x_batch_header_rec   OUT NOCOPY      gme_batch_header%ROWTYPE)
      RETURN BOOLEAN;

   FUNCTION create_history (
      p_batch_header_rec   IN   gme_batch_header%ROWTYPE
     ,p_original_status    IN   NUMBER
     ,p_event_id           IN   NUMBER DEFAULT NULL)
      RETURN BOOLEAN;

   FUNCTION close_period_check_flexible (
      p_org_id       IN              NUMBER
     ,p_trans_date   IN              DATE
     ,x_trans_date   OUT NOCOPY      DATE
     ,x_period_id    OUT NOCOPY      INTEGER)
      RETURN BOOLEAN;

   FUNCTION get_batchstep_rsrc (
      p_batchstep_rsrc_rec   IN              gme_batch_step_resources%ROWTYPE
     ,p_org_code             IN              VARCHAR2
     ,p_batch_no             IN              VARCHAR2
     ,p_batchstep_no         IN              NUMBER
     ,p_activity             IN              VARCHAR2
     ,p_resource             IN              VARCHAR2
     ,x_batchstep_rsrc_rec   OUT NOCOPY      gme_batch_step_resources%ROWTYPE)
      RETURN BOOLEAN;

   FUNCTION check_validity_rule_dates (
      p_validity_rule_id   IN   NUMBER
     ,p_start_date         IN   DATE
     ,p_cmplt_date         IN   DATE
     ,p_batch_header_rec   IN   gme_batch_header%ROWTYPE DEFAULT NULL
--Sunitha Ch. Bug 5336007 added the parameter  p_validate_plan_dates_ind that indicates whether to validate
--value -0  Validate Only  start date
--Value  -1 Validate Both the dates
     ,p_validate_plan_dates_ind  IN NUMBER DEFAULT 0)
      RETURN BOOLEAN;
  /*B6408612 --Added function for checking the operation efffectivity dates */
   FUNCTION check_oprn_effectivity_dates (
     p_oprn_id         IN NUMBER
    ,p_start_date               IN DATE
    ,p_cmplt_date               IN DATE

    )
    RETURN BOOLEAN;

   PROCEDURE material_date_change (
      p_material_detail_id   IN              NUMBER
     ,p_material_date        IN              DATE
     ,x_return_status        OUT NOCOPY      VARCHAR2);

   FUNCTION is_material_auto_release (
      p_line_id   IN   gme_material_details.material_detail_id%TYPE)
      RETURN NUMBER;

   FUNCTION get_assoc_step (
      p_material_detail_id   IN              gme_material_details.material_detail_id%TYPE
     ,x_batchstep_id         OUT NOCOPY      NUMBER
     ,x_batchstep_status     OUT NOCOPY      NUMBER)
      RETURN BOOLEAN;

   FUNCTION get_material_detail (
      p_material_detail_rec   IN              gme_material_details%ROWTYPE
     ,p_org_code              IN              VARCHAR2
     ,p_batch_no              IN              VARCHAR2
     ,p_batch_type            IN              NUMBER
     ,x_batch_header_rec      OUT NOCOPY      gme_batch_header%ROWTYPE
     ,x_material_detail_rec   OUT NOCOPY      gme_material_details%ROWTYPE)
      RETURN BOOLEAN;
    -- Pawan Kumar Added for bug 5138929
     FUNCTION check_close_period (
      p_org_id       IN              NUMBER
     ,p_trans_date   IN              DATE)
      RETURN BOOLEAN;

     -- Bug 7562848 - Added parameter to bypass phantom check.      
     Procedure Validate_batch (
      p_batch_header_rec   IN              gme_batch_header%ROWTYPE
     ,p_org_code           IN              VARCHAR2
     ,p_batch_type         IN              NUMBER
     ,p_check_phantom      IN              VARCHAR2 DEFAULT 'Y'     
     ,x_batch_header_rec   OUT NOCOPY      gme_batch_header%ROWTYPE
     ,x_message_count      OUT NOCOPY      NUMBER
     ,x_message_list       OUT NOCOPY      VARCHAR2
     ,x_return_status      OUT NOCOPY      VARCHAR2);
     
      Procedure Validate_material_detail (
      p_material_detail_rec   	IN              gme_material_details%ROWTYPE
     ,p_org_code              	IN              VARCHAR2
     ,p_batch_no              	IN              VARCHAR2
     ,p_batch_type            	IN              NUMBER
     ,x_batch_header_rec      	OUT NOCOPY      gme_batch_header%ROWTYPE
     ,x_material_detail_rec   	OUT NOCOPY      gme_material_details%ROWTYPE
     ,x_message_count      	OUT NOCOPY      NUMBER
     ,x_message_list       	OUT NOCOPY      VARCHAR2
     ,x_return_status      	OUT NOCOPY      VARCHAR2);
     Procedure Validate_batch_step (
      p_batch_step_rec     IN              gme_batch_steps%ROWTYPE
     ,p_org_code           IN              VARCHAR2
     ,p_batch_no           IN              VARCHAR2
     ,x_batch_step_rec     OUT NOCOPY      gme_batch_steps%ROWTYPE
     ,x_batch_header_rec   OUT NOCOPY      gme_batch_header%ROWTYPE
     ,x_message_count      OUT NOCOPY      NUMBER
     ,x_message_list       OUT NOCOPY      VARCHAR2
     ,x_return_status      OUT NOCOPY      VARCHAR2);

     --Bug#5394232 to default transaction date
     PROCEDURE fetch_trans_date (     
      p_material_detail_id IN            NUMBER
     ,p_invoke_mode        IN            VARCHAR2 DEFAULT 'T'
     ,x_trans_date         OUT NOCOPY    DATE
     ,x_return_status      OUT NOCOPY    VARCHAR2);

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
                      );
  --Added by QZENG ER 20985387, Get the current onhold status, R: Run, S: Stop, P: Pause
  FUNCTION GET_BATCH_HOLD_STATUS(p_batch_id IN NUMBER, p_date IN DATE DEFAULT SYSDATE) RETURN VARCHAR2;
  
  /*
    BUG 201102335  19-MAY-2015 Shaliu Chen
    Added for Batch On Hold enhancement,it's used to check whether
    the transaction date of resource txn is within the period of held batch,
    and also check whether there is overlap between the period of resource txn
    and the period of held batch
  */
  FUNCTION validate_resource_txn_onhold(p_batch_id    IN NUMBER,
                                       p_trans_date  IN DATE DEFAULT SYSDATE,
                                       p_start_date  IN DATE DEFAULT SYSDATE,
                                       p_end_date    IN DATE DEFAULT SYSDATE) RETURN VARCHAR2;  
END gme_common_pvt;
/
--SHOW ERRORS;
COMMIT ;
EXIT;
