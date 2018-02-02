/* +======================================================================+ */
/* |    Copyright (c) 2005, 2015 Oracle and/or its affiliates.           | */
/* |                         All rights reserved.                         | */
/* |                           Version 12.0.0                             | */
/* +======================================================================+ */
REM dbdrv: sql ~PROD ~PATH ~FILE none none none package &phase=plb \
REM dbdrv: checkfile(120.34.12010000.9=120.41.12020000.4)(120.30.12000000.11=120.34.12010000.7)(115.153=120.10):~PROD:~PATH:~FILE
SET VERIFY OFF
WHENEVER SQLERROR EXIT FAILURE ROLLBACK;
WHENEVER OSERROR EXIT FAILURE ROLLBACK;
REM **********************************************************************
REM *                                                                    *
REM * FILE:    GMEGAPIB.pls                                              *
REM * PURPOSE: Package body for GME group level APIs                     *
REM * AUTHOR:  Pawan Kumar, OPM Development                            *
REM * DATE:                                                              *
REM * HISTORY:                                                           *
REM * ========                                                           *
REM * Swapna K Bug#6778968 Added a parameter, p_called_by to the procedure *
REM *   validate_supply_demand                                             *
REM
REM   G. Muratore    05-SEP-2008   Bug 7352169
REM      Do not call auto detail line during item substitution.
REM      PROCEDURE substitute_ingredients
REM
REM   G. Muratore    29-SEP-2009   Bug 8841650
REM      Initialize user and timestamp when called from an outside process.
REM      PROCEDURE gme_post_process_txns 
REM
REM   G. Muratore    15-SEP-2010   Bug 10182779
REM      Allow access for materials associated to steps for completed
REM      steps if they are not autobystep. Also added check for closed steps.
REM      PROCEDURE get_available_supply_demand 

REM   G. Muratore    13-Jan-2014   Bug 18044719
REM     Introduce p_lot_control_code parameter to account for reservations without a lot. 
REM     With the flexible lot INV enhancement, it is now possible to not specify a lot on 
REM     a reservation for a lot controlled item.
REM     FUNCTION IS_RESERVATION_FULLY_SPECIFIED 

REM   G. Muratore    03-JUN-2015   Bug 19811389 - rework of 10182779
REM      Allow access for materials associated to completed steps.
REM      This behavior now matches automatic release lines in a completed batch.
REM      PROCEDURE get_available_supply_demand
REM **************************************************************************

CREATE OR REPLACE PACKAGE BODY gme_api_grp AS
   /* $Header: GMEGAPIB.pls 120.41.12020000.4 2015/06/08 13:51:04 gmurator ship $ */
   g_debug               VARCHAR2 (5)  := fnd_profile.VALUE ('AFLOG_LEVEL');
   g_pkg_name   CONSTANT VARCHAR2 (30) := 'GME_API_GRP';
  
   PROCEDURE gme_pre_process_txns (
      p_header_id       IN              NUMBER
     ,x_return_status   OUT NOCOPY      VARCHAR2)
   IS
      l_api_name   CONSTANT VARCHAR2 (30) := 'GME_PRE_PROCESS_TXNS';
      l_return_status       VARCHAR2 (1);
   BEGIN
      -- Initially let us assign the return status to success
      x_return_status := fnd_api.g_ret_sts_success;

      IF (g_debug <= gme_debug.g_log_statement) THEN
         gme_debug.put_line (g_pkg_name || '.' || l_api_name || ':'
                             || 'Entering');
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ':'
                             || 'header_id is :'
                             || p_header_id);
      END IF;

      IF p_header_id IS NOT NULL THEN
         gme_transactions_pvt.gme_pre_process
                                        (p_transaction_hdr_id      => p_header_id
                                        ,x_return_status           => l_return_status);

         IF l_return_status <> fnd_api.g_ret_sts_success THEN
            RAISE fnd_api.g_exc_error;
         END IF;
      END IF;
      x_return_status := l_return_status;
      IF (g_debug <= gme_debug.g_log_statement) THEN
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ':'
                             || 'Exiting with '
                             || l_return_status);
      END IF;
      
   EXCEPTION
      WHEN fnd_api.g_exc_error THEN
         x_return_status := l_return_status;
      WHEN fnd_api.g_exc_unexpected_error THEN
         x_return_status := fnd_api.g_ret_sts_unexp_error;
      WHEN OTHERS THEN
         x_return_status := fnd_api.g_ret_sts_unexp_error;

         IF (NVL (g_debug, -1) = gme_debug.g_log_unexpected) THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ':'
                                || 'WHEN OTHERS:'
                                || SQLERRM);
         END IF;

         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
   END;

   PROCEDURE gme_post_process_txns (
      p_transaction_id   IN              NUMBER
     ,x_return_status    OUT NOCOPY      VARCHAR2
     ,x_message_data     OUT NOCOPY      VARCHAR2)
   IS
      l_api_name   CONSTANT VARCHAR2 (30)      := 'gme_post_process_txns';
      l_return_status       VARCHAR2 (1);

      CURSOR get_txns (v_header_id IN NUMBER)
      IS
         SELECT *
           FROM mtl_material_transactions
          WHERE transaction_set_id = v_header_id;

      l_txn_rec             get_txns%ROWTYPE;
   BEGIN
      x_return_status := fnd_api.g_ret_sts_success;

      IF (g_debug <= gme_debug.g_log_statement) THEN
         gme_debug.put_line (g_pkg_name || '.' || l_api_name || ':'
                             || 'Entering');
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ':'
                             || 'transaction_id'
                             || p_transaction_id);
      END IF;

      -- Bug 8841650 - Initialize user and timestamp just in case this is 
      --               called from an outside process like transact move order.
      IF gme_common_pvt.g_user_ident IS NULL THEN
         gme_common_pvt.set_who;
      END IF;

      gme_transactions_pvt.gme_post_process
                                        (p_transaction_id      => p_transaction_id
                                        ,x_return_status       => l_return_status);
                                        
         IF l_return_status <> fnd_api.g_ret_sts_success THEN
            RAISE fnd_api.g_exc_error;
         END IF;
                                      
      x_return_status := l_return_status;                               
      IF (g_debug <= gme_debug.g_log_statement) THEN
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ':'
                             || 'Exiting with '
                             || l_return_status);
      END IF;
   EXCEPTION
      WHEN fnd_api.g_exc_error THEN
         x_return_status := l_return_status;
      WHEN fnd_api.g_exc_unexpected_error THEN
         x_return_status := fnd_api.g_ret_sts_unexp_error;
      WHEN OTHERS THEN
         x_return_status := fnd_api.g_ret_sts_unexp_error;

         IF (NVL (g_debug, -1) = gme_debug.g_log_unexpected) THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ':'
                                || 'WHEN OTHERS:'
                                || SQLERRM);
         END IF;

         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
   END gme_post_process_txns;

   PROCEDURE update_material_date (
      p_material_detail_id   IN              NUMBER
     ,p_material_date        IN              DATE
     ,x_return_status        OUT NOCOPY      VARCHAR2)
   IS
   BEGIN
   
      gme_common_pvt.set_who ;
      gme_common_pvt.material_date_change
                               (p_material_detail_id      => p_material_detail_id
                               ,p_material_date           => p_material_date
                               ,x_return_status           => x_return_status);
      /* FPBug#4585491
         this procedure may return R, B or M depends on whether reservations deleted
         or MO allocations deleted or both
       */
      IF x_return_status in ('R','B','M') THEN
        x_return_status := fnd_api.g_ret_sts_success;
      END IF;
   END update_material_date;
   /*Bug#6778968 Added the new parameter, p_called_by */
   PROCEDURE validate_supply_demand    
      (  x_return_status             OUT NOCOPY VARCHAR2
      ,  x_msg_count                 OUT NOCOPY NUMBER 
      ,  x_msg_data                  OUT NOCOPY VARCHAR2 
      ,  x_valid_status              OUT NOCOPY VARCHAR2
      ,  p_organization_id           IN         NUMBER
      ,  p_item_id                   IN         NUMBER
      ,  p_supply_demand_code        IN         NUMBER
      ,  p_supply_demand_type_id     IN         NUMBER                    
      ,  p_supply_demand_header_id   IN         NUMBER
      ,  p_supply_demand_line_id     IN         NUMBER
      ,  p_supply_demand_line_detail IN         NUMBER DEFAULT FND_API.G_MISS_NUM
      ,  p_demand_ship_date          IN         DATE
      ,  p_expected_receipt_date     IN         DATE
      ,  p_called_by                 IN         VARCHAR2 DEFAULT 'VAL'
      ,  p_api_version_number        IN         NUMBER DEFAULT 1.0
      ,  p_init_msg_lst              IN         VARCHAR2 DEFAULT FND_API.G_FALSE 
      ) IS
    l_api_name                       CONSTANT VARCHAR2 (30) := 'VALIDATE_SUPPLY_DEMAND';

    l_material_details_rec           GME_MATERIAL_DETAILS%ROWTYPE;
    l_batch_header_rec               GME_BATCH_HEADER%ROWTYPE;
    l_step_status                    NUMBER;
    l_mat_status                     NUMBER;
    l_step_id                        NUMBER;

    invalid_version                  EXCEPTION;
    input_param_missing              EXCEPTION;
    validation_error                 EXCEPTION;
    fetch_failure                    EXCEPTION;
    supply_demand_error              EXCEPTION;

   BEGIN
    
    IF g_debug <= gme_debug.g_log_procedure THEN
      gme_debug.put_line('Entering api '||g_pkg_name||'.'||l_api_name);
    END IF;

    /* Set the return status to success initially */
    x_return_status := FND_API.G_RET_STS_SUCCESS;


    /* Set the valid status to YES initially */
    x_valid_status := 'Y';                                

    IF p_init_msg_lst  = FND_API.G_TRUE THEN
      fnd_msg_pub.initialize;
    END IF;

    IF NOT FND_API.compatible_api_call(1.0, p_api_version_number, 'validate_supply_demand', g_pkg_name ) THEN
      x_return_status := FND_API.G_RET_STS_ERROR;
      RAISE invalid_version;
    END IF;    

    /* Ensure mandatory inputs supplied */
    IF g_debug <= gme_debug.g_log_statement THEN
      gme_debug.put_line(g_pkg_name||'.'||l_api_name||' validate for mandatory input parameters ');
    END IF;
    IF p_organization_id IS NULL THEN
      gme_common_pvt.log_message('GME_FIELD_VALUE_REQUIRED','FIELD_NAME', 'ORGANIZATION_ID');
      RAISE input_param_missing;
    ELSIF p_item_id IS NULL THEN
      gme_common_pvt.log_message('GME_FIELD_VALUE_REQUIRED','FIELD_NAME', 'ITEM_ID');
      RAISE input_param_missing;
    ELSIF p_supply_demand_code IS NULL THEN
      gme_common_pvt.log_message('GME_FIELD_VALUE_REQUIRED','FIELD_NAME', 'SUPPLY_DEMAND_CODE');
      RAISE input_param_missing;
    ELSIF p_supply_demand_type_id IS NULL THEN
      gme_common_pvt.log_message('GME_FIELD_VALUE_REQUIRED','FIELD_NAME', 'SUPPLY_DEMAND_TYPE_ID');
      RAISE input_param_missing;
    ELSIF p_supply_demand_header_id IS NULL THEN
      gme_common_pvt.log_message('GME_FIELD_VALUE_REQUIRED','FIELD_NAME', 'SUPPLY_DEMAND_HEADER_ID');
      RAISE input_param_missing;
    ELSIF p_supply_demand_line_id IS NULL THEN
      gme_common_pvt.log_message('GME_FIELD_VALUE_REQUIRED','FIELD_NAME', 'SUPPLY_DEMAND_LINE_ID');
      RAISE input_param_missing;
    END IF;

    /* Retrieve batch header row */
    l_batch_header_rec.batch_id := p_supply_demand_header_id;
    IF NOT (gme_batch_header_dbl.fetch_row (l_batch_header_rec, l_batch_header_rec)) THEN
      IF g_debug <= gme_debug.g_log_statement THEN
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||
          'Fetch failure against gme_batch_header using id of  '||p_supply_demand_header_id);
      END IF;
      RAISE fetch_failure;
    END IF;
    IF g_debug <= gme_debug.g_log_statement THEN
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' Batch Status '||l_batch_header_rec.batch_status);
    END IF;

    /* Verify that update_inventory is allowed for the Batch */
    IF l_batch_header_rec.update_inventory_ind <> 'Y' THEN
      gme_common_pvt.log_message('GME_INVENTORY_UPDATE_BLOCKED');                            
      RAISE validation_error;
    END IF;

    /* Verify that Batch is in either Pending or WIP status */
    IF l_batch_header_rec.batch_status NOT IN (gme_common_pvt.g_batch_pending, 
                                               gme_common_pvt.g_batch_wip) THEN
      gme_common_pvt.log_message('GME_INVALID_BATCH_STATUS', 'PROCESS', 'RESERVATION');
      RAISE validation_error;
    END IF;

    /* Verify that Batch is WIP status for conversion api calls. */
    IF (l_batch_header_rec.batch_status <> gme_common_pvt.g_batch_wip AND
       p_called_by = 'CVT') THEN
       gme_common_pvt.log_message('GME_INVALID_BATCH_STATUS', 'CONVERT', 'RESERVATION');
       RAISE validation_error;
    END IF;

    /* Reservation OR conversion not permitted for FPOs */
    IF l_batch_header_rec.batch_type = gme_common_pvt.g_doc_type_fpo THEN
      gme_common_pvt.log_message('GME_FPO_RESERVATION_ERROR');
      RAISE validation_error;
    END IF;

    /* Retrieve material details record */
    l_material_details_rec.material_detail_id := p_supply_demand_line_id;
    IF (NOT(gme_material_details_dbl.fetch_row(l_material_details_rec, l_material_details_rec))) THEN
      IF g_debug <= gme_debug.g_log_statement THEN
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||
          'Fetch failure against gme_material_details using id of  '||p_supply_demand_line_id);
      END IF;
      RAISE fetch_failure;
    END IF;

    /* Verify that the supplied organization_id and item are consistent with the material details row */
    IF p_organization_id <> l_material_details_rec.organization_id THEN 
      --Bug#5439736 replaced the following message      
      gme_common_pvt.log_message('GME_INCONSISTENT_FIELD','FIELD_NAME','ORGANIZATION_ID');
      RAISE validation_error;
    ELSIF p_item_id <> l_material_details_rec.inventory_item_id THEN
      --Bug#5439736      
      gme_common_pvt.log_message('GME_INCONSISTENT_FIELD','FIELD_NAME','INVENTORY_ITEM_ID');
      RAISE validation_error;
    END IF;

    /* Verify that Reservation is not for phantom ingredient */
    IF l_material_details_rec.phantom_type IN (1,2) THEN
      gme_common_pvt.log_message('GME_INVALID_RSV_FOR_PHANTOM');                                     
      RAISE validation_error;
    END IF;

    /* Verify that Reservation is not for sample by-product */
    IF l_material_details_rec.line_type = gme_common_pvt.g_line_type_byprod AND
      l_material_details_rec.by_product_type = 'Y' THEN
      gme_common_pvt.log_message('GME_INVALID_RSV_FOR_BYPROD');                                         
      RAISE validation_error;           
    END IF;

    IF g_debug <= gme_debug.g_log_statement THEN
       gme_debug.put_line(g_pkg_name||'.'||l_api_name||' Material Release Type '||l_material_details_rec.release_type);
    END IF;
    /* Verify p_supply_demand_code and line_type. */
    IF p_supply_demand_code = 2 /* Demand */ AND 
      l_material_details_rec.line_type <> gme_common_pvt.g_line_type_ing THEN
      gme_common_pvt.log_message('GME_INVALID_DEMAND_LINE');                                            
      RAISE supply_demand_error; 
    ELSIF p_supply_demand_code = 1 /* Supply */ AND 
      l_material_details_rec.line_type <> gme_common_pvt.g_line_type_prod THEN
      gme_common_pvt.log_message('GME_INVALID_SUPPLY_LINE');                                       
      RAISE supply_demand_error; 
    ELSIF NVL(p_supply_demand_code,0) NOT IN (1,2) THEN          
      gme_common_pvt.log_message('GME_INVALID_SUPPLY_DEMAND');                                       
      RAISE supply_demand_error; 
    ELSIF p_supply_demand_code = 2 /* Demand */ AND 
          l_material_details_rec.line_type = gme_common_pvt.g_line_type_ing THEN
      IF l_material_details_rec.release_type NOT IN ( gme_common_pvt.g_mtl_manual_release,gme_common_pvt.g_mtl_incremental_release) THEN
         l_mat_status := gme_common_pvt.is_material_auto_release(l_material_details_rec.material_detail_id);
      ELSE
         l_mat_status := l_material_details_rec.release_type;
      END IF;
      --Bug#4604943 following code is commented out 
      /* BUG 4604943 BEGIN - check for auto AND autobystep here 
      IF l_mat_status in ( gme_common_pvt.g_mtl_auto_release ,gme_common_pvt.g_mtl_autobystep_release) AND
         l_batch_header_rec.batch_status = gme_common_pvt.g_batch_wip THEN
         IF g_debug <= gme_debug.g_log_statement THEN
           gme_debug.put_line(g_pkg_name||'.'||l_api_name||' Automatic Release Material in WIP batch not a valid demand'); 
         END IF;
         gme_common_pvt.log_message('GME_INVALID_DEMAND_LINE');                                            
         RAISE supply_demand_error; 
       BUG 4604943 END 
      ELS */
      IF l_mat_status = gme_common_pvt.g_mtl_auto_release AND
         l_batch_header_rec.batch_status = gme_common_pvt.g_batch_wip THEN
         IF p_called_by <> 'CVT' THEN         
            IF g_debug <= gme_debug.g_log_statement THEN
               gme_debug.put_line(g_pkg_name||'.'||l_api_name||' Automatic Release Material in WIP batch not a valid demand'); 
            END IF;
            gme_common_pvt.log_message('GME_INVALID_DEMAND_LINE');                                            
            RAISE supply_demand_error;
         END IF; 
      ELSIF l_mat_status = gme_common_pvt.g_mtl_autobystep_release THEN
          IF NOT gme_common_pvt.get_assoc_step(l_material_details_rec.material_detail_id,l_step_id,l_step_status) THEN   
             IF g_debug <= gme_debug.g_log_statement THEN
                gme_debug.put_line(g_pkg_name||'.'||l_api_name||' Error in get_assoc_step'); 
             END IF;
             gme_common_pvt.log_message('GME_INVALID_DEMAND_LINE');                                            
             RAISE supply_demand_error; 
          ELSIF l_step_id IS NOT NULL THEN
             /* for conversions of reservations, the step status should not be pending and
                for creating the reservations, the step status should be greater than pending */
             IF (p_called_by = 'CVT' and NVL(l_step_status,-1) <> gme_common_pvt.g_step_wip) 
               OR (p_called_by <> 'CVT' and NVL(l_step_status,-1) >= gme_common_pvt.g_step_wip) THEN
                IF g_debug <= gme_debug.g_log_statement THEN
                  gme_debug.put_line(g_pkg_name||'.'||l_api_name||' Automatic By Step Material in step: '||l_step_id||
                                     ' with status of '||l_step_status||'  not a valid demand'); 
               END IF;
               gme_common_pvt.log_message('GME_INVALID_DEMAND_LINE');                                            
               RAISE supply_demand_error; 
             END IF;
          END IF; -- IF l_step_id IS NOT NULL AND NVL(l_step_status,-1) = gme_common_pvt.g_step_wip THEN
       END IF; -- IF l_mat_status = gme_common_pvt.g_mtl_auto_release AND
    END IF;

    IF g_debug <= gme_debug.g_log_procedure THEN
      gme_debug.put_line ('Completed '|| l_api_name|| ' at '|| TO_CHAR (SYSDATE, 'MM/DD/YYYY HH24:MI:SS'));
    END IF;
    EXCEPTION
    /* Exception handling  */      
    WHEN invalid_version OR input_param_missing OR validation_error 
      OR fetch_failure OR supply_demand_error THEN
      x_return_status := FND_API.g_ret_sts_error;
      x_valid_status := 'N';                                
      gme_common_pvt.count_and_get(x_count   => x_msg_count,
                                   p_encoded => FND_API.g_false,
                                   x_data    => x_msg_data);
    WHEN OTHERS THEN
      IF g_debug <= gme_debug.g_log_unexpected THEN
        gme_debug.put_line(g_pkg_name|| '.'|| l_api_name|| ':'|| 'When others exception:'|| SQLERRM);
      END IF;
      x_return_status := FND_API.g_ret_sts_unexp_error;
      x_valid_status := 'N';                                
      gme_common_pvt.count_and_get(x_count   => x_msg_count,
                                   p_encoded => FND_API.g_false,
                                   x_data    => x_msg_data);
   END validate_supply_demand;

   PROCEDURE get_available_supply_demand    
      (  x_return_status             OUT NOCOPY VARCHAR2
      ,  x_msg_count                 OUT NOCOPY NUMBER 
      ,  x_msg_data                  OUT NOCOPY VARCHAR2 
      ,  x_available_quantity        OUT NOCOPY NUMBER 
      ,  x_source_uom_code           OUT NOCOPY VARCHAR2 
      ,  x_source_primary_uom_code   OUT NOCOPY VARCHAR2 
      ,  p_organization_id           IN         NUMBER DEFAULT NULL
      ,  p_item_id                   IN         NUMBER DEFAULT NULL
      ,  p_revision                  IN         VARCHAR2 DEFAULT NULL
      ,  p_lot_number                IN         VARCHAR2 DEFAULT NULL
      ,  p_subinventory_code         IN         VARCHAR2 DEFAULT NULL
      ,  p_locator_id                IN         NUMBER DEFAULT NULL
      ,  p_supply_demand_code        IN         NUMBER
      ,  p_supply_demand_type_id     IN         NUMBER                   
      ,  p_supply_demand_header_id   IN         NUMBER
      ,  p_supply_demand_line_id     IN         NUMBER
      ,  p_supply_demand_line_detail IN         NUMBER DEFAULT FND_API.G_MISS_NUM
      ,  p_lpn_id                    IN         NUMBER DEFAULT FND_API.G_MISS_NUM
      ,  p_project_id                IN         NUMBER DEFAULT NULL
      ,  p_task_id                   IN         NUMBER DEFAULT NULL
      ,  p_api_version_number        IN         NUMBER DEFAULT 1.0
      ,  p_init_msg_lst              IN         VARCHAR2 DEFAULT FND_API.G_FALSE 
      ) IS
    l_api_name                       CONSTANT VARCHAR2 (30) := 'GET_AVAILABLE_SUPPLY_DEMAND';

    l_material_details_rec           GME_MATERIAL_DETAILS%ROWTYPE;
    l_batch_header_rec               GME_BATCH_HEADER%ROWTYPE;
    l_step_status                    NUMBER(5);
    l_primary_uom_code               VARCHAR2(3);
    l_available_quantity             NUMBER; 

    CURSOR cur_get_step_status (v_material_detail_id NUMBER) IS
       SELECT step_status
         FROM gme_batch_steps s, 
              gme_batch_step_items i
         WHERE s.batchstep_id = i.batchstep_id 
            AND i.material_detail_id = v_material_detail_id;

    CURSOR cur_item(v_org_id NUMBER, v_inventory_item_id NUMBER) IS
       SELECT primary_uom_code                         
         FROM   mtl_system_items_b
       WHERE  organization_id = V_org_id
         AND inventory_item_id = V_inventory_item_id;


    invalid_version                  EXCEPTION;
    input_param_missing              EXCEPTION;
    validation_error                 EXCEPTION;
    fetch_failure                    EXCEPTION;

   BEGIN
    
    IF g_debug <= gme_debug.g_log_procedure THEN
      gme_debug.put_line('Entering api '||g_pkg_name||'.'||l_api_name);
    END IF;

    /* Set the return status to success initially */
    x_return_status := FND_API.G_RET_STS_SUCCESS;

    IF p_init_msg_lst  = FND_API.G_TRUE THEN
      fnd_msg_pub.initialize;
    END IF;

    IF NOT FND_API.compatible_api_call(1.0, p_api_version_number, 'validate_supply_demand', g_pkg_name ) THEN
      x_return_status := FND_API.G_RET_STS_ERROR;
      RAISE invalid_version;
    END IF;

    /* Ensure mandatory inputs supplied */
    IF g_debug <= gme_debug.g_log_statement THEN
      gme_debug.put_line(g_pkg_name||'.'||l_api_name||' validate for mandatory input parameters ');
    END IF;
    IF p_supply_demand_code IS NULL THEN
      gme_common_pvt.log_message('GME_FIELD_VALUE_REQUIRED','FIELD_NAME', 'SUPPLY_DEMAND_CODE');
      RAISE input_param_missing;
    ELSIF p_supply_demand_type_id IS NULL THEN
      gme_common_pvt.log_message('GME_FIELD_VALUE_REQUIRED','FIELD_NAME', 'SUPPLY_DEMAND_TYPE_ID');
      RAISE input_param_missing;
    ELSIF p_supply_demand_header_id IS NULL THEN
      gme_common_pvt.log_message('GME_FIELD_VALUE_REQUIRED','FIELD_NAME', 'SUPPLY_DEMAND_HEADER_ID');
      RAISE input_param_missing;
    ELSIF p_supply_demand_line_id IS NULL THEN
      gme_common_pvt.log_message('GME_FIELD_VALUE_REQUIRED','FIELD_NAME', 'SUPPLY_DEMAND_LINE_ID');
      RAISE input_param_missing;
    END IF;

    /* Retrieve batch header */
    l_batch_header_rec.batch_id := p_supply_demand_header_id;
    IF NOT (gme_batch_header_dbl.fetch_row (l_batch_header_rec, l_batch_header_rec)) THEN
      IF g_debug <= gme_debug.g_log_statement THEN
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||
          'Fetch failure against gme_batch_header using id of  '||p_supply_demand_header_id);
      END IF;
      RAISE fetch_failure;
    END IF;

    /* For Batch in Completed status , return 0*/
    IF l_batch_header_rec.batch_status = gme_common_pvt.g_batch_completed THEN
      x_available_quantity := 0;
      IF g_debug <= gme_debug.g_log_statement THEN
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||
          'Batch status is completed so return available of 0  ');
      END IF;
      RETURN;
    END IF;

    /* Retrieve material details record */
    l_material_details_rec.material_detail_id := p_supply_demand_line_id;
    IF (NOT(gme_material_details_dbl.fetch_row(l_material_details_rec, l_material_details_rec))) THEN
      IF g_debug <= gme_debug.g_log_statement THEN
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||
          'Fetch failure against gme_material_details using id of  '||p_supply_demand_line_id);
      END IF;
      RAISE fetch_failure;
    END IF;

    /* IF line is associated to step and step is completed then return Zero */
    OPEN cur_get_step_status (l_material_details_rec.material_detail_id);
    FETCH cur_get_step_status INTO l_step_status;
    CLOSE cur_get_step_status;

    -- Bug 10182779 -- Allow access for materials associated to steps for completed  
    -- steps if they are not autobystep. Also added check for closed steps.
    
    -- Bug 19811389 -- rework of 10182779. Allow access for materials associated to completed steps.      
    -- IF (NVL(l_step_status, 0) = gme_common_pvt.g_step_completed AND l_material_details_rec.release_type = 3) OR 
    IF NVL(l_step_status, 0) = gme_common_pvt.g_step_closed THEN
       x_available_quantity := 0;
       RETURN;
    END IF;

    /* If supplied,verify that the supplied organization_id and item are consistent with the material details row */
    IF p_organization_id is NOT NULL THEN
      IF p_organization_id <> l_material_details_rec.organization_id THEN 
        gme_common_pvt.log_message('GME_INCONSISTENT_FIELD','FIELD_NAME','ORGANIZATION_ID'); 
        RAISE validation_error;
      END IF;
    END IF;

    IF p_item_id is NOT NULL THEN
      IF p_item_id <> l_material_details_rec.inventory_item_id THEN
        gme_common_pvt.log_message('GME_INCONSISTENT_FIELD','FIELD_NAME','INVENTORY_ITEM_ID'); 
        RAISE validation_error;
      END IF;
    END IF;
   
    /* Now retrieve the primary UOM code for the item */
    OPEN cur_item (l_material_details_rec.organization_id,l_material_details_rec.inventory_item_id);
    FETCH cur_item INTO l_primary_uom_code;     
    CLOSE cur_item;                       
    IF g_debug <= gme_debug.g_log_statement THEN
      gme_debug.put_line(g_pkg_name||'.'||l_api_name||' primary uom code '||l_primary_uom_code);   
    END IF;

    /* Return the uom code and primary uom code   */
    x_source_uom_code := l_material_details_rec.dtl_um;
    x_source_primary_uom_code := l_primary_uom_code;                     

    /* Return the available supply demand quantity.*/
    /* The value will be same for p_supply_demand_code as Demand or Supply */
    IF (g_debug <= gme_debug.g_log_unexpected) THEN
      gme_debug.put_line (   g_pkg_name
                          || '.'
                          || l_api_name
                          || ':'
                          || 'Compute available qty from these figues:' 
                          || ' wip_plan_qty => ' 
                          || l_material_details_rec.wip_plan_qty
                          || ' plan_qty     => ' 
                          || l_material_details_rec.plan_qty
                          || ' actual_qty   => ' 
                          || l_material_details_rec.actual_qty);
    END IF;
    IF p_supply_demand_code = 2 THEN
      x_available_quantity := 1000000000000;
    ELSE
      l_available_quantity := NVL(l_material_details_rec.wip_plan_qty, l_material_details_rec.plan_qty) - l_material_details_rec.actual_qty;
      -- To conform to INV standards, round to 5 decimal places
      x_available_quantity := ROUND(l_available_quantity,5);
    END IF;
    IF g_debug <= gme_debug.g_log_procedure THEN
      gme_debug.put_line ('Available quantity computes as '|| x_available_quantity);
      gme_debug.put_line ('Completed '|| l_api_name|| ' at '|| TO_CHAR (SYSDATE, 'MM/DD/YYYY HH24:MI:SS'));
    END IF;
    RETURN;
  EXCEPTION
    /* Exception handling  */      
    WHEN invalid_version OR input_param_missing OR validation_error OR fetch_failure  THEN
      x_return_status := FND_API.g_ret_sts_error;
      gme_common_pvt.count_and_get(x_count   => x_msg_count,
                                   p_encoded => FND_API.g_false,
                                   x_data    => x_msg_data);
    WHEN OTHERS THEN
      IF g_debug <= gme_debug.g_log_unexpected THEN
        gme_debug.put_line(g_pkg_name|| '.'|| l_api_name|| ':'|| 'When others exception:'|| SQLERRM);
      END IF;
      x_return_status := FND_API.g_ret_sts_unexp_error;
      gme_common_pvt.count_and_get(x_count   => x_msg_count,
                                   p_encoded => FND_API.g_false,
                                   x_data    => x_msg_data);
  END get_available_supply_demand; 

   PROCEDURE update_step_quality_status (
      p_batchstep_id     IN              NUMBER
     ,p_org_id           IN              NUMBER
     ,p_quality_status   IN              NUMBER
     ,x_return_status    OUT NOCOPY      VARCHAR2)
   IS
      l_batch_step   gme_batch_steps%ROWTYPE;
      expected_err   EXCEPTION;
   BEGIN
      IF NOT gme_common_pvt.g_setup_done THEN
         gme_common_pvt.g_setup_done := gme_common_pvt.setup (p_org_id);

         IF NOT gme_common_pvt.g_setup_done THEN
            x_return_status := fnd_api.g_ret_sts_error;
            RAISE expected_err;
         END IF;
      END IF;

      gme_common_pvt.set_timestamp;
      l_batch_step.batchstep_id := p_batchstep_id;

      IF (NOT (gme_batch_steps_dbl.fetch_row (l_batch_step, l_batch_step) ) ) THEN
         RAISE expected_err;
      END IF;

      IF (   p_quality_status IS NULL
          OR p_quality_status < 1
          OR p_quality_status > 6) THEN
         gme_common_pvt.log_message ('GME_INV_STEP_QUALITY_STATUS');
         RAISE expected_err;
      END IF;

      IF (l_batch_step.step_status > 2) THEN
         gme_common_pvt.log_message ('PC_STEP_STATUS_ERR');
         RAISE expected_err;
      END IF;

      IF    (l_batch_step.step_status = 1 AND p_quality_status > 2)
         OR (l_batch_step.step_status = 2 AND p_quality_status <= 2) THEN
         gme_common_pvt.log_message ('GME_INV_STEP_STATUS_QUALITY');
         RAISE expected_err;
      END IF;

      l_batch_step.quality_status := p_quality_status;

      IF (NOT (gme_batch_steps_dbl.update_row (l_batch_step) ) ) THEN
         RAISE expected_err;
      END IF;
   EXCEPTION
      WHEN expected_err THEN
         x_return_status := fnd_api.g_ret_sts_error;
      WHEN OTHERS THEN
         fnd_msg_pub.add_exc_msg ('GME_API_GRP'
                                 ,'UPDATE_STEP_QUALITY_STATUS');
         x_return_status := fnd_api.g_ret_sts_unexp_error;
   END update_step_quality_status;
   
   
    PROCEDURE get_batch_shortages (
      p_api_version_number     IN               NUMBER DEFAULT 1.0
     ,p_init_msg_list          IN               VARCHAR2 DEFAULT FND_API.G_FALSE 
     ,x_msg_count                 OUT NOCOPY NUMBER 
     ,x_msg_data                  OUT NOCOPY VARCHAR2 
     ,p_organization_id         IN              NUMBER
     ,p_batch_id                IN              NUMBER
     ,p_invoke_mode             IN              VARCHAR2
     ,p_tree_mode         IN              NUMBER
     ,x_return_status     OUT NOCOPY      VARCHAR2
     ,x_exception_tbl     OUT NOCOPY      gme_common_pvt.exceptions_tab)
   IS
      l_api_name   CONSTANT VARCHAR2 (30) := 'get_batch_shortages';
   BEGIN
      -- Initially let us assign the return status to success
      x_return_status := fnd_api.g_ret_sts_success;

      IF (g_debug <= gme_debug.g_log_statement) THEN
         gme_debug.put_line (g_pkg_name || '.' || l_api_name || ':'
                             || 'Entering');
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ':'
                             || 'batch_id is :'
                             || p_batch_id);
      END IF;

      IF p_batch_id IS NOT NULL THEN
      gme_common_pvt.get_batch_shortages (
      p_organization_id         =>     p_organization_id 
     ,p_batch_id                =>     p_batch_id 
     ,p_invoke_mode             =>   p_invoke_mode
     ,p_tree_mode         =>          p_tree_mode
     ,x_return_status     =>      x_return_status
     ,x_exception_tbl     =>      x_exception_tbl);

         IF x_return_status <> fnd_api.g_ret_sts_success THEN
            RAISE fnd_api.g_exc_error;
         END IF;
      END IF;

      IF (g_debug <= gme_debug.g_log_statement) THEN
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ':'
                             || 'Exiting with '
                             || x_return_status);
      END IF;
   EXCEPTION
      WHEN fnd_api.g_exc_error THEN
          gme_common_pvt.count_and_get (x_count        => x_msg_count 
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_msg_data);
      
      WHEN OTHERS THEN
         x_return_status := fnd_api.g_ret_sts_unexp_error;

         IF (NVL (g_debug, -1) = gme_debug.g_log_unexpected) THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ':'
                                || 'WHEN OTHERS:'
                                || SQLERRM);
         END IF;
          gme_common_pvt.count_and_get (x_count        => x_msg_count 
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_msg_data);
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
   END;
    PROCEDURE get_material_reservations  (
      p_api_version_number     IN               NUMBER DEFAULT 1.0
     ,p_init_msg_list          IN               VARCHAR2 DEFAULT FND_API.G_FALSE 
     ,x_msg_count              OUT NOCOPY NUMBER 
     ,x_msg_data               OUT NOCOPY VARCHAR2 
     ,p_organization_id         IN              NUMBER
     ,p_batch_id                IN              NUMBER
     ,p_material_detail_id   IN              NUMBER
     ,x_return_status        OUT NOCOPY      VARCHAR2
     ,x_reservations_tbl     OUT NOCOPY      gme_common_pvt.reservations_tab)
 
   IS
      l_api_name   CONSTANT VARCHAR2 (30) := 'get_material_reservations';
   BEGIN
      -- Initially let us assign the return status to success
      x_return_status := fnd_api.g_ret_sts_success;

      IF (g_debug <= gme_debug.g_log_statement) THEN
         gme_debug.put_line (g_pkg_name || '.' || l_api_name || ':'
                             || 'Entering');
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ':'
                             || 'batch_id is :'
                             || p_batch_id);
      END IF;

      IF p_batch_id IS NOT NULL THEN
             gme_reservations_pvt.get_material_reservations (
         p_organization_id      =>     p_organization_id        
        ,p_batch_id             =>     p_batch_id         
        ,p_material_detail_id   =>     p_material_detail_id       
        ,x_return_status        =>     x_return_status 
        ,x_reservations_tbl    =>      x_reservations_tbl);
 
         IF x_return_status <> fnd_api.g_ret_sts_success THEN
            RAISE fnd_api.g_exc_error;
         END IF;
      END IF;

      IF (g_debug <= gme_debug.g_log_statement) THEN
         gme_debug.put_line (   g_pkg_name
                             || '.'
                             || l_api_name
                             || ':'
                             || 'Exiting with '
                             || x_return_status);
      END IF;
   EXCEPTION
      WHEN fnd_api.g_exc_error THEN
        gme_common_pvt.count_and_get (x_count        => x_msg_count 
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_msg_data);
     
      WHEN OTHERS THEN
         x_return_status := fnd_api.g_ret_sts_unexp_error;

         IF (NVL (g_debug, -1) = gme_debug.g_log_unexpected) THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ':'
                                || 'WHEN OTHERS:'
                                || SQLERRM);
         END IF;
          gme_common_pvt.count_and_get (x_count        => x_msg_count 
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_msg_data);
         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
   END get_material_reservations;
   
   PROCEDURE create_lcf_batch (
      p_api_version            IN         NUMBER DEFAULT 1.0
     ,p_init_msg_list          IN         VARCHAR2 DEFAULT FND_API.G_FALSE 
     ,p_commit                 IN         VARCHAR2 DEFAULT FND_API.G_FALSE
     ,x_message_count          OUT NOCOPY NUMBER 
     ,x_message_list           OUT NOCOPY VARCHAR2 
     ,x_return_status          OUT NOCOPY VARCHAR2
     ,p_batch_header_rec       IN         gme_batch_header%rowtype
     ,p_formula_dtl_tbl        IN         gmdfmval_pub.formula_detail_tbl
     ,p_recipe_rout_tbl        IN         gmd_recipe_fetch_pub.recipe_rout_tbl
     ,p_recipe_step_tbl        IN         gmd_recipe_fetch_pub.recipe_step_tbl
     ,p_routing_depd_tbl       IN         gmd_recipe_fetch_pub.routing_depd_tbl
     ,p_oprn_act_tbl           IN         gmd_recipe_fetch_pub.oprn_act_tbl  
     ,p_oprn_resc_tbl          IN         gmd_recipe_fetch_pub.oprn_resc_tbl
     ,p_proc_param_tbl         IN         gmd_recipe_fetch_pub.recp_resc_proc_param_tbl
     ,p_use_workday_cal        IN         VARCHAR2 DEFAULT FND_API.G_TRUE
     ,p_contiguity_override    IN         VARCHAR2 DEFAULT FND_API.G_TRUE
     ,x_batch_header_rec       OUT NOCOPY gme_batch_header%rowtype
     ,x_exception_material_tbl OUT NOCOPY gme_common_pvt.exceptions_tab
      ) IS
        
      l_api_name            CONSTANT VARCHAR2(30) := 'CREATE_LCF_BATCH';
    BEGIN
        IF g_debug <= gme_debug.g_log_procedure THEN
           gme_debug.log_initialize('CreateLCFBatch');
        END IF;

        IF g_debug <= gme_debug.g_log_procedure THEN
           gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                               || l_api_name);
        END IF;
        IF p_init_msg_list = fnd_api.g_true THEN
           fnd_msg_pub.initialize;
           gme_common_pvt.g_error_count := 0;
        END IF;
        /* Make sure we are call compatible */
        IF NOT fnd_api.compatible_api_call (1
                                           ,p_api_version
                                           ,'create_lcf_batch'
                                           ,g_pkg_name) THEN
           x_return_status := fnd_api.g_ret_sts_error;
           gme_common_pvt.log_message ('GME_INVALID_API_VERSION');
           RAISE fnd_api.g_exc_error;
        END IF;
        /* Setup the common constants used accross the apis */
        IF g_debug <= gme_debug.g_log_procedure THEN
           gme_debug.put_line ('Calling gme_common_pvt.setup.');
        END IF;
        
        gme_common_pvt.g_setup_done :=
           gme_common_pvt.setup (p_org_id        => p_batch_header_rec.organization_id
                                ,p_org_code      => NULL);
        
        IF g_debug <= gme_debug.g_log_procedure THEN
           gme_debug.put_line ('After calling  gme_common_pvt.setup.');
        END IF;
        
        IF NOT gme_common_pvt.g_setup_done THEN
           x_return_status := fnd_api.g_ret_sts_error;
           RAISE fnd_api.g_exc_error;
        END IF;
        gme_common_pvt.set_timestamp;
        gme_common_pvt.materials           := p_formula_dtl_tbl;
        gme_common_pvt.routings            := p_recipe_rout_tbl;
        gme_common_pvt.steps               := p_recipe_step_tbl;
        gme_common_pvt.step_dependencies   := p_routing_depd_tbl;
        gme_common_pvt.activities          := p_oprn_act_tbl;
        gme_common_pvt.resources           := p_oprn_resc_tbl;
        gme_common_pvt.process_parameters  := p_proc_param_tbl;
        gme_create_batch_pvt.create_batch(
           p_validation_level        => 100
           ,x_return_status          => x_return_status
           ,p_batch_header_rec       => p_batch_header_rec
           ,x_batch_header_rec       => x_batch_header_rec
           ,p_batch_size             => p_formula_dtl_tbl(1).qty
           ,p_batch_size_uom         => p_formula_dtl_tbl(1).detail_uom   
           ,p_creation_mode          => 'LCF'
           ,p_ignore_qty_below_cap   => FND_API.G_TRUE
           ,p_use_workday_cal        => p_use_workday_cal
           ,p_contiguity_override    => p_contiguity_override
           ,p_is_phantom             => 'N'       
           ,x_exception_material_tbl => x_exception_material_tbl
         );
        IF x_return_status NOT IN (FND_API.G_RET_STS_SUCCESS, gme_common_pvt.g_inv_short_err,'C') THEN
           IF g_debug <= gme_debug.g_log_procedure THEN
              gme_debug.put_line ('Error in Create Batch: return status'||x_return_status);
           END IF;
           RAISE fnd_api.g_exc_error;
        END IF;
        IF g_debug <= gme_debug.g_log_procedure THEN
           gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
        END IF;


   EXCEPTION
      WHEN fnd_api.g_exc_error THEN
         x_return_status := fnd_api.g_ret_sts_error;
         x_batch_header_rec := null;
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
      WHEN fnd_api.g_exc_unexpected_error THEN
         x_return_status := fnd_api.g_ret_sts_unexp_error;
         x_batch_header_rec := null;
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
      WHEN OTHERS THEN
         x_return_status := fnd_api.g_ret_sts_unexp_error;
         x_batch_header_rec := null;
         IF (NVL (g_debug, -1) = gme_debug.g_log_unexpected) THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ':'
                                || 'WHEN OTHERS:'
                                || SQLERRM);
         END IF;

         fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);

    END create_lcf_batch;

   FUNCTION get_planning_open_qty (
      p_organization_id      IN   NUMBER
     ,p_batch_id             IN   NUMBER
     ,p_material_detail_id   IN   NUMBER
     ,p_prim_plan_qty        IN   NUMBER
     ,p_prim_wip_plan_qty    IN   NUMBER
     ,p_prim_actual_qty      IN   NUMBER
     ,p_prim_uom             IN   VARCHAR2)
      RETURN NUMBER
   IS
      l_api_name   CONSTANT VARCHAR2 (30)                  := 'get_planning_open_qty';
      l_open_qty            NUMBER                         := 0;
      l_return_status       VARCHAR2 (1);
      l_mtl_dtl_rec         gme_material_details%ROWTYPE;
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      l_mtl_dtl_rec.organization_id := p_organization_id;
      l_mtl_dtl_rec.batch_id := p_batch_id;
      l_mtl_dtl_rec.material_detail_id := p_material_detail_id;
      l_mtl_dtl_rec.plan_qty := p_prim_plan_qty;
      l_mtl_dtl_rec.wip_plan_qty := p_prim_wip_plan_qty;
      l_mtl_dtl_rec.actual_qty := p_prim_actual_qty;
      l_mtl_dtl_rec.dtl_um := p_prim_uom;
      gme_common_pvt.get_open_qty (p_mtl_dtl_rec        => l_mtl_dtl_rec
                                  ,p_called_by          => 'S'
                                  ,x_open_qty           => l_open_qty
                                  ,x_return_status      => l_return_status);

      IF (l_return_status <> fnd_api.g_ret_sts_success) THEN
         RETURN 0;
      ELSE
         RETURN l_open_qty;
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

         RETURN 0;
   END get_planning_open_qty;

   FUNCTION IS_RESERVATION_FULLY_SPECIFIED(p_reservation_id     IN      NUMBER )
     RETURN NUMBER 
     IS
      l_api_name   CONSTANT VARCHAR2 (30)   := 'Is_reservation_fully_specified';
      l_reservation_type      NUMBER        := 0;
      l_item_rec               mtl_system_items_b%ROWTYPE;
      l_rsv_rec                mtl_reservations%ROWTYPE;
      l_rsv_type                NUMBER;
      l_msg_count                   NUMBER;
      l_msg_list                    VARCHAR2(32767);
      l_return_status               VARCHAR2 (10);
      fetch_error              EXCEPTION;
      error_unexpected         EXCEPTION;
      
     CURSOR cur_rsv_rec (v_reservation_id NUMBER)
      IS 
          SELECT *
          FROM mtl_reservations 
          WHERE reservation_id = v_reservation_id;
          
     CURSOR cur_fetch_item (v_org_id NUMBER, v_inventory_item_id NUMBER)
      IS
         SELECT *
           FROM mtl_system_items_b
          WHERE organization_id = v_org_id
            AND inventory_item_id = v_inventory_item_id;
   BEGIN
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;
      IF p_reservation_id IS NOT NULL THEN
        OPEN cur_rsv_rec (p_reservation_id);
        FETCH cur_rsv_rec
        INTO l_rsv_rec;

         IF cur_rsv_rec%NOTFOUND THEN
           CLOSE cur_rsv_rec;
         
           gme_common_pvt.log_message ('GME_NO_DATA_FOUND'
                                      ,'TABLE_NAME'
                                      ,'MTL_RESERVATIONS');
         
            IF g_debug <= gme_debug.g_log_statement THEN
              gme_debug.put_line
                 (   g_pkg_name
                  || '.'
                  || l_api_name
                  || ' Retrieval failure against mtl_reservations using id of  '
                  || p_reservation_id);
            END IF;
         
            RAISE fetch_error;
         END IF;
         CLOSE cur_rsv_rec;
        END IF ;
     
     OPEN cur_fetch_item (l_rsv_rec.organization_id
                          ,l_rsv_rec.inventory_item_id);

      FETCH cur_fetch_item
       INTO l_item_rec;

      IF cur_fetch_item%NOTFOUND THEN
         CLOSE cur_fetch_item;

         gme_common_pvt.log_message ('GME_NO_DATA_FOUND'
                                    ,'TABLE_NAME'
                                    ,'MTL_SYSTEM_ITEMS');

         IF g_debug <= gme_debug.g_log_statement THEN
            gme_debug.put_line
               (   g_pkg_name
                || '.'
                || l_api_name
                || ' Retrieval failure against mtl_system_items using id of  '
                || l_rsv_rec.inventory_item_id);
         END IF;

         RAISE fetch_error;
      END IF;

      CLOSE cur_fetch_item;
      /* Bug 5441643 Added NVL condition for location control code*/
      l_rsv_type := 
                gme_reservations_pvt.reservation_fully_specified
               (p_reservation_rec             => l_rsv_rec
               ,p_item_location_control       => NVL(l_item_rec.location_control_code,1)
               ,p_item_restrict_locators      => l_item_rec.restrict_locators_code
               ,p_lot_control_code            => l_item_rec.lot_control_code); -- Bug 18044719


      IF g_debug <= gme_debug.g_log_statement THEN
         gme_debug.put_line
            (   g_pkg_name
             || '.'
             || l_api_name
             || ' Return rsv_type from gme_reservations_pvt.reservation_fully_specified is '
             || TO_CHAR (l_rsv_type) );
      END IF;

      IF l_rsv_type = -1 THEN
         gme_common_pvt.log_message ('GME_RSV_DETAIL_REQUIRED');
         RAISE error_unexpected;
      END IF;
      
      IF l_rsv_type IN (0, 2) THEN 
         RETURN 0;
      ELSE
         RETURN 1;
      END IF;
   EXCEPTION
      WHEN error_unexpected OR fetch_error THEN
         RETURN 0;
         gme_common_pvt.count_and_get (x_count        => l_msg_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => l_msg_list);
        
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

         RETURN 0;
   END IS_RESERVATION_FULLY_SPECIFIED;

/*======================================================================
--  PROCEDURE:
--    substitute_ingredients
--
--  DESCRIPTION:
--      Procedure to substitute ingredients for the passed item_no,
--      org_id, from and to batch_no, start and end dates.
--
--  HISTORY:
--     Sivakumar.G FPBug#4351032 08-DEC-2005
--      gme_api_grp.log_message calls replaced by gme_common_pvt.log_message
--     Namit Singhi FPBug#5674398 01-JAN-2007
--      Modified for ingred sub enhancement FP
--     G. Muratore    05-SEP-2008   Bug 7352169
--     Do not call auto detail line during item substitution.

-- A. Mishra 09-Nov-2009 Bug 8820175 
Commenting the duplicate code   to stop the "line" from appearing twice
-- Shaliu Chen     05-MAY-2015  ER 20938455
--    Modify for Batch On Hold enhancement,modify cursor - get_ingredients
--    to exclude the batches which is on hold
======================================================================*/
   PROCEDURE substitute_ingredients (
      errbuf          OUT NOCOPY      VARCHAR2,
      retcode         OUT NOCOPY      VARCHAR2,
      p_org_id        IN              NUMBER,
      p_from_batch_no IN              VARCHAR2,
      p_to_batch_no   IN              VARCHAR2,
      p_item_id       IN              NUMBER,
      p_start_date    IN              VARCHAR2,
      p_end_date      IN              VARCHAR2
   ) IS
      /* Bug 5212569 Removed * and selecting only required columns */
      CURSOR item_master_cursor (v_item_id NUMBER
                                 ,v_org_id NUMBER) IS
         SELECT concatenated_segments, mtl_transactions_enabled_flag, process_execution_enabled_flag,
                eng_item_flag, primary_uom_code, reservable_type
           FROM mtl_system_items_kfv
          WHERE inventory_item_id = v_item_id
            AND organization_id = v_org_id;

      CURSOR get_ingredients (
         v_org_id        IN   NUMBER,
         v_item_id       IN   NUMBER,
         v_start_date    IN   DATE,
         v_end_date      IN   DATE,
         v_from_batch_no IN VARCHAR2,
         v_to_batch_no   IN VARCHAR2
      ) IS
         SELECT   m.material_detail_id, m.batch_id, h.batch_no, m.material_requirement_date, m.line_no,
                  m.formulaline_id, m.plan_qty, m.dtl_um, m.scale_multiple, h.formula_id,
                  m.inventory_item_id, m.move_order_line_id, i.concatenated_segments item_no, h.organization_id, h.batch_type
             FROM gme_material_details m, gme_batch_header h, mtl_system_items_kfv i
            WHERE m.batch_id = h.batch_id
              AND h.organization_id = v_org_id
              AND h.batch_status = 1
              AND m.material_requirement_date >= v_start_date
              AND m.material_requirement_date <= NVL (v_end_date, m.material_requirement_date)
              AND m.line_type = -1
              AND m.phantom_type = 0 -- Non phantom ingrdients
              AND m.inventory_item_id = i.inventory_item_id
              AND m.organization_id = i.organization_id
              AND (m.inventory_item_id = v_item_id OR v_item_id IS NULL)
              AND (h.batch_no >= v_from_batch_no OR v_from_batch_no is null)
              AND (h.batch_no <= v_to_batch_no OR v_to_batch_no is null)
              AND m.formulaline_id IS NOT NULL --only for the ingredients in the formula substitution happens
              /* ER 20938455 05-MAY-2015 Shaliu Chen
                 Add following condition to exclude the batch which is on hold with STOP
              */
              AND gme_common_pvt.get_batch_hold_status(h.batch_id) <> 'S'              
         ORDER BY m.material_requirement_date, m.batch_id, m.material_detail_id;

      CURSOR c_batchsteps (p_material_detail_id IN NUMBER) IS
         SELECT b.*
           FROM gme_batch_step_items a, gme_batch_steps b
          WHERE a.batchstep_id = b.batchstep_id 
            AND a.material_detail_id = p_material_detail_id;
            
      CURSOR get_msg (v_msg_name IN VARCHAR2) IS
         SELECT substrb(message_text,1,50)
           FROM fnd_new_messages
          WHERE application_id = 553
            AND message_name = v_msg_name; 

      l_formula_tbl              gmdfmval_pub.formula_detail_tbl;
      l_trolin_tbl               inv_move_order_pub.trolin_tbl_type;
      l_material_details_tbl     gme_common_pvt.material_details_tab;
      l_material_details_tbl_out gme_common_pvt.material_details_tab;
      l_material_detail_rec      gme_material_details%ROWTYPE;
      l_return                   BOOLEAN;
      l_return_status            VARCHAR2 (1);
      x_return_status            VARCHAR2 (1);
      l_batch_header_rec         gme_batch_header%ROWTYPE;
      l_old_item_rec             item_master_cursor%ROWTYPE;
      l_new_item_rec             item_master_cursor%ROWTYPE;
      l_batchstep_rec            gme_batch_steps%ROWTYPE;
      x_batchstep_rec            gme_batch_steps%ROWTYPE;
      l_rsc_trans_count          NUMBER;
      l_temp_qty                 NUMBER;
      l_trans_loaded             BOOLEAN                           DEFAULT FALSE;
      l_oneitem_success          BOOLEAN                           DEFAULT FALSE;
      l_oneitem_error            BOOLEAN                           DEFAULT FALSE;
      l_api_name                 VARCHAR2 (50)                     := 'substitute_ingredients';
      l_message_count            NUMBER;
      l_message_list             VARCHAR2 (2048);
      l_start_date               DATE;
      l_end_date                 DATE;
      l_doc_str                  VARCHAR2(80);
      l_ingred_sub_date          DATE; -- nsinghi bug#5674398
      setup_failure              EXCEPTION;
   BEGIN

      gme_common_pvt.g_setup_done :=
         gme_common_pvt.setup (p_org_id  => p_org_id);

      IF NOT gme_common_pvt.g_setup_done THEN
         x_return_status := fnd_api.g_ret_sts_error;
         RAISE setup_failure;
      END IF;
      --set the timestamp
      gme_common_pvt.set_timestamp;

      IF (g_debug IS NOT NULL) THEN
         gme_debug.log_initialize ('IngredientSubstitution');
      END IF;

      IF p_item_id IS NOT NULL THEN
         OPEN item_master_cursor (p_item_id, p_org_id);
         FETCH item_master_cursor INTO l_old_item_rec;
         CLOSE item_master_cursor;
      END IF;

      --FPBug#4991508 hard codes strings are seeded for NLS complaint
      fnd_message.set_name('GME','GME_INPUT_PARAM');
      fnd_file.put (fnd_file.output,fnd_message.get );
      fnd_file.new_line (fnd_file.output, 1);
      fnd_message.set_name('GME','GME_ORG_ID');
      fnd_file.put (fnd_file.output, fnd_message.get || p_org_id);
      fnd_file.new_line (fnd_file.output, 1);
      fnd_message.set_name('GME','GME_OLD_ITEM');
      fnd_file.put (fnd_file.output, fnd_message.get|| l_old_item_rec.concatenated_segments);
      fnd_file.new_line (fnd_file.output, 1);
      l_start_date := TO_DATE (p_start_date, 'YYYY/MM/DD HH24:MI:SS');
      l_end_date := TO_DATE (p_end_date, 'YYYY/MM/DD HH24:MI:SS');
      fnd_message.set_name('GME','GME_DATE_RANGE');
      fnd_file.put (fnd_file.output,
                     fnd_message.get
                     || TO_CHAR (l_start_date, 'DD-MON-YYYY HH24:MI:SS')
                     || ' <-> '
                     || TO_CHAR (l_end_date, 'DD-MON-YYYY HH24:MI:SS')
                   );
      --Bug#4533850
      fnd_file.new_line (fnd_file.output, 1);
      fnd_message.set_name('GME','GME_FROM_BATCH');
      fnd_file.put (fnd_file.output, fnd_message.get|| p_from_batch_no);
      fnd_file.new_line (fnd_file.output, 1);
      fnd_message.set_name('GME','GME_TO_BATCH');
      fnd_file.put (fnd_file.output, fnd_message.get|| p_to_batch_no);
      fnd_file.new_line (fnd_file.output, 2);

      FOR rec IN get_ingredients (p_org_id, p_item_id, l_start_date, l_end_date,p_from_batch_no,p_to_batch_no) LOOP
         IF rec.batch_type = 0 THEN
            OPEN get_msg('GME_BATCH');
            FETCH get_msg INTO l_doc_str;
            CLOSE get_msg;
         ELSE
            OPEN get_msg('GME_FIRM_PLAN_ORDER');
            FETCH get_msg INTO l_doc_str;
            CLOSE get_msg;
         END IF;
	 
         -- nsinghi bug#5674398. Pass the ingredient substitution date rather than the material requirement date
         l_ingred_sub_date := gme_api_grp.get_ingr_sub_date(rec.batch_id,rec.material_detail_id);

         /* Though this call returns a table, we will be looking at the 1st record of the table */
         gmdfmval_pub.get_substitute_line_item (pformulaline_id        => rec.formulaline_id,
                                                pitem_id               => rec.inventory_item_id,
                                                pqty                   => rec.plan_qty,
                                                puom                   => rec.dtl_um,
                                                pscale_multiple        => rec.scale_multiple,
                                                pdate                  => l_ingred_sub_date,
                                                xformuladetail_tbl     => l_formula_tbl
                                               );

         IF l_formula_tbl.COUNT = 0 THEN
            GOTO NEXT_RECORD; --GO to next record
         END IF;

         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line (' I/P formulaline_id line id is: ' || rec.formulaline_id);
            gme_debug.put_line (' I/P item id is is: ' || rec.inventory_item_id);
            gme_debug.put_line (' I/P plan_qty Qty is: ' || rec.plan_qty);
            gme_debug.put_line (' I/P UOM is: ' || rec.dtl_um);
            gme_debug.put_line (' O/P item id is is: ' || l_formula_tbl (1).inventory_item_id);
            gme_debug.put_line (' O/P plan_qty Qty is: ' || l_formula_tbl (1).qty);
            gme_debug.put_line ('O/P UOM is: ' || l_formula_tbl (1).detail_uom);
         END IF;

         IF l_formula_tbl (1).inventory_item_id = rec.inventory_item_id AND
            l_formula_tbl (1).qty = rec.plan_qty AND
            l_formula_tbl (1).detail_uom = rec.dtl_um THEN
            GOTO NEXT_RECORD; --GO to next record
         END IF;

         --fetch batch header record
         l_batch_header_rec.batch_id := rec.batch_id;

         IF NOT (gme_batch_header_dbl.fetch_row (l_batch_header_rec, l_batch_header_rec)) THEN
            l_oneitem_error := TRUE;
            gme_common_pvt.count_and_get (x_count       => l_message_count,
                                          p_encoded     => fnd_api.g_false,
                                          x_data        => l_message_list
                                         );
            GOTO NEXT_RECORD; --GO to next record
         END IF;

         -- Fetch material record for the rec.material_detail_id;
         l_material_detail_rec.material_detail_id := rec.material_detail_id;

         IF NOT gme_material_details_dbl.fetch_row (p_material_detail     => l_material_detail_rec,
                                                    x_material_detail     => l_material_detail_rec
                                                   ) THEN
            l_oneitem_error := TRUE;
            gme_common_pvt.count_and_get (x_count       => l_message_count,
                                          p_encoded     => fnd_api.g_false,
                                          x_data        => l_message_list
                                         );
            --FPBug#4991508 replaced hard coded output messages
            /*
            fnd_message.set_name('GME','GME_NO_SUBSTITUTION');
            fnd_message.set_token('DOC',l_doc_str||' '||rec.batch_no);
            fnd_message.set_token('LINE',rec.line_no);
            fnd_message.set_token('ITEM',rec.item_no);
            fnd_message.set_token('PLANQTY',rec.plan_qty||' '||rec.dtl_um);
            fnd_message.set_token('MSG',l_message_list);
            fnd_file.put (fnd_file.output,fnd_message.get);

            fnd_file.new_line (fnd_file.output, 1);
            */
            
            
            --FPBug#4351032 used gme_common_pvt.log_message
            gme_common_pvt.log_message(p_message_code => 'GME_NO_SUBSTITUTION'
                                      ,p_token1_name  => 'DOC'
                                      ,P_token1_value => l_doc_str||' '||rec.batch_no
                                      ,p_token2_name  => 'LINE'
                                      ,P_token2_value => rec.line_no
                                      ,p_token3_name  => 'ITEM'
                                      ,P_token3_value => rec.item_no
                                      ,p_token4_name  => 'PLANQTY'
                                      ,P_token4_value => rec.plan_qty||' '||rec.dtl_um
                                      ,p_token5_name  => 'MSG'
                                      ,P_token5_value => l_message_list
                                      );
            gme_common_pvt.count_and_get (x_count       => l_message_count,
                                          p_encoded     => fnd_api.g_false,
                                          x_data        => l_message_list
                                         );
            fnd_file.put(fnd_file.output,l_message_list);       
            fnd_file.new_line (fnd_file.output, 1);
            GOTO NEXT_RECORD; --GO to next record
         END IF;
	
	 -- nsinghi bug#5674398. Moved the code to here.
         IF p_item_id IS NULL THEN
            OPEN item_master_cursor (l_material_detail_rec.inventory_item_id,l_material_detail_rec.organization_id);
            FETCH item_master_cursor INTO l_old_item_rec;
            CLOSE item_master_cursor;
         END IF;
	 
         OPEN item_master_cursor (l_formula_tbl (1).inventory_item_id,l_material_detail_rec.organization_id);
         FETCH item_master_cursor INTO l_new_item_rec;
         CLOSE item_master_cursor;

         IF l_new_item_rec.process_execution_enabled_flag <> 'Y' THEN
            l_oneitem_error := TRUE;
            
            /*
            --FPBug#4991508 replaced hard coded output messages
            fnd_message.set_name('GME','GME_NO_SUBSTITUTION_EXEC');
            fnd_message.set_token('DOC',l_doc_str||' '||rec.batch_no);
            fnd_message.set_token('LINE',rec.line_no);
            fnd_message.set_token('ITEM',rec.item_no);
            fnd_message.set_token('PLANQTY',rec.plan_qty||' '||rec.dtl_um);
            fnd_message.set_token('NEWITEM',l_new_item_rec.concatenated_segments);
            fnd_file.put (fnd_file.output,fnd_message.get);
            
            fnd_file.put(fnd_file.output,l_message_list);
            fnd_file.new_line (fnd_file.output, 1);
            */
            
            --FPBug#4351032 used gme_common_pvt.log_message
            gme_common_pvt.log_message(p_message_code => 'GME_NO_SUBSTITUTION_EXEC'
                                      ,p_token1_name  => 'DOC'
                                      ,P_token1_value => l_doc_str||' '||rec.batch_no
                                      ,p_token2_name  => 'LINE'
                                      ,P_token2_value => rec.line_no
                                      ,p_token3_name  => 'ITEM'
                                      ,P_token3_value => rec.item_no
                                      ,p_token4_name  => 'PLANQTY'
                                      ,P_token4_value => rec.plan_qty||' '||rec.dtl_um
                                      ,p_token5_name  => 'NEWITEM'
                                      ,P_token5_value => l_new_item_rec.concatenated_segments
                                      );
            gme_common_pvt.count_and_get (x_count       => l_message_count,
                                          p_encoded     => fnd_api.g_false,
                                          x_data        => l_message_list
                                         );
            fnd_file.put(fnd_file.output,l_message_list);       
            fnd_file.new_line (fnd_file.output, 1);
            GOTO NEXT_RECORD; --GO to next record
         END IF;

         IF l_new_item_rec.eng_item_flag = 'Y' AND
            NVL(l_batch_header_rec.laboratory_ind,0) <> 1 THEN
            l_oneitem_error := TRUE;
            /*
             --FPBug#4991508 replaced hard coded output messages
            fnd_message.set_name('GME','GME_NO_SUBSTITUTION_ENG');
            fnd_message.set_token('DOC',l_doc_str||' '||rec.batch_no);
            fnd_message.set_token('LINE',rec.line_no);
            fnd_message.set_token('ITEM',rec.item_no);
            fnd_message.set_token('PLANQTY',rec.plan_qty||' '||rec.dtl_um);
            fnd_message.set_token('NEWITEM',l_new_item_rec.concatenated_segments);
            fnd_file.put (fnd_file.output,fnd_message.get);
                    
            fnd_file.put(fnd_file.output,l_message_list);
            fnd_file.new_line (fnd_file.output, 1);
            */
            
            --FPBug#4351032 used gme_common_pvt.log_message
            gme_common_pvt.log_message(p_message_code => 'GME_NO_SUBSTITUTION_ENG'
                                      ,p_token1_name  => 'DOC'
                                      ,P_token1_value => l_doc_str||' '||rec.batch_no
                                      ,p_token2_name  => 'LINE'
                                      ,P_token2_value => rec.line_no
                                      ,p_token3_name  => 'ITEM'
                                      ,P_token3_value => rec.item_no
                                      ,p_token4_name  => 'PLANQTY'
                                      ,P_token4_value => rec.plan_qty||' '||rec.dtl_um
                                      ,p_token5_name  => 'NEWITEM'
                                      ,P_token5_value => l_new_item_rec.concatenated_segments
                                      );
            gme_common_pvt.count_and_get (x_count       => l_message_count,
                                          p_encoded     => fnd_api.g_false,
                                          x_data        => l_message_list
                                         );
            fnd_file.put(fnd_file.output,l_message_list);       
            fnd_file.new_line (fnd_file.output, 1);
            GOTO NEXT_RECORD; --GO to next record
         END IF;

         SAVEPOINT create_trans;

         IF l_batch_header_rec.update_inventory_ind = 'Y' THEN
            -- Delete all reservations for this material line
            gme_reservations_pvt.delete_material_reservations (
                      p_organization_id    => rec.organization_id
                     ,p_batch_id           => rec.batch_id
                     ,p_material_detail_id => rec.material_detail_id
                     ,x_return_status      => l_return_status);

            IF l_return_status <> fnd_api.g_ret_sts_success THEN
               l_oneitem_error := TRUE;
               gme_common_pvt.count_and_get (x_count       => l_message_count,
                                             p_encoded     => fnd_api.g_false,
                                             x_data        => l_message_list
                                            );
               --FPBug#4991508 replaced hard coded output messages
               /*
               fnd_message.set_name('GME','GME_NO_SUBSTITUTION');
               fnd_message.set_token('DOC',l_doc_str||' '||rec.batch_no);
               fnd_message.set_token('LINE',rec.line_no);
               fnd_message.set_token('ITEM',rec.item_no);
               fnd_message.set_token('PLANQTY',rec.plan_qty||' '||rec.dtl_um);
               fnd_message.set_token('MSG',l_message_list);
               fnd_file.put (fnd_file.output,fnd_message.get);
                      
               fnd_file.put(fnd_file.output,l_message_list);
               fnd_file.new_line (fnd_file.output, 1);
               */
               
               --FPBug#4351032 used gme_common_pvt.log_message
               gme_common_pvt.log_message(p_message_code => 'GME_NO_SUBSTITUTION'
                                         ,p_token1_name  => 'DOC'
                                         ,P_token1_value => l_doc_str||' '||rec.batch_no
                                         ,p_token2_name  => 'LINE'
                                         ,P_token2_value => rec.line_no
                                         ,p_token3_name  => 'ITEM'
                                         ,P_token3_value => rec.item_no
                                         ,p_token4_name  => 'PLANQTY'
                                         ,P_token4_value => rec.plan_qty||' '||rec.dtl_um
                                         ,p_token5_name  => 'MSG'
                                         ,P_token5_value => l_message_list
                                         );
               gme_common_pvt.count_and_get (x_count       => l_message_count,
                                             p_encoded     => fnd_api.g_false,
                                             x_data        => l_message_list
                                            );
               fnd_file.put(fnd_file.output,l_message_list);       
               fnd_file.new_line (fnd_file.output, 1);
               GOTO NEXT_RECORD; --GO to next record
            END IF;

            l_return_status := NULL;
            IF l_old_item_rec.mtl_transactions_enabled_flag = 'Y' THEN
               gme_move_orders_pvt.delete_move_order_lines
                    (p_organization_id         => rec.organization_id
                    ,p_batch_id                => rec.batch_id
                    ,p_material_detail_id      => rec.material_detail_id
                    ,p_invis_move_line_id      => rec.move_order_line_id
                    ,x_return_status           => l_return_status);

               IF l_return_status <> fnd_api.g_ret_sts_success THEN
                  l_oneitem_error := TRUE;
                  gme_common_pvt.count_and_get (x_count       => l_message_count,
                                                p_encoded     => fnd_api.g_false,
                                                x_data        => l_message_list
                                               );
                  --FPBug#4991508 replaced hard coded output messages
                  /*
                  fnd_message.set_name('GME','GME_NO_SUBSTITUTION');
                  fnd_message.set_token('DOC',l_doc_str||' '||rec.batch_no);
                  fnd_message.set_token('LINE',rec.line_no);
                  fnd_message.set_token('ITEM',rec.item_no);
                  fnd_message.set_token('PLANQTY',rec.plan_qty||' '||rec.dtl_um);
                  fnd_message.set_token('MSG',l_message_list);
                  fnd_file.put (fnd_file.output,fnd_message.get);
                  
                  fnd_file.put(fnd_file.output,l_message_list); 
                  fnd_file.new_line (fnd_file.output, 1);
                  */
                  
                  --FPBug#4351032 used gme_common_pvt.log_message
                  gme_common_pvt.log_message(p_message_code => 'GME_NO_SUBSTITUTION'
                                            ,p_token1_name  => 'DOC'
                                            ,P_token1_value => l_doc_str||' '||rec.batch_no
                                            ,p_token2_name  => 'LINE'
                                            ,P_token2_value => rec.line_no
                                            ,p_token3_name  => 'ITEM'
                                            ,P_token3_value => rec.item_no
                                            ,p_token4_name  => 'PLANQTY'
                                            ,P_token4_value => rec.plan_qty||' '||rec.dtl_um
                                            ,p_token5_name  => 'MSG'
                                            ,P_token5_value => l_message_list
                                            );
                  gme_common_pvt.count_and_get (x_count       => l_message_count,
                                                p_encoded     => fnd_api.g_false,
                                                x_data        => l_message_list
                                               );
                  fnd_file.put(fnd_file.output,l_message_list);       
                  fnd_file.new_line (fnd_file.output, 1);
                  GOTO NEXT_RECORD; --GO to next record
               END IF; --IF l_return_status <> fnd_api.g_ret_sts_success THEN
            END IF; --IF l_new_item_rec.mtl_transactions_enabled_flag = 'Y' THEN
         END IF; -- l_batch_header_rec.update_inventory_ind = 'Y'

         l_return_status := NULL;
         l_material_detail_rec.inventory_item_id := l_formula_tbl (1).inventory_item_id;
         l_material_detail_rec.plan_qty := l_formula_tbl (1).qty;
         l_material_detail_rec.dtl_um := l_formula_tbl (1).detail_uom;
         l_material_detail_rec.scale_multiple := l_formula_tbl (1).scale_multiple;
         l_material_detail_rec.original_qty := 0;

         IF l_formula_tbl (1).detail_uom <> l_new_item_rec.primary_uom_code THEN
            l_temp_qty := inv_convert.inv_um_convert
                                 (item_id       => l_material_detail_rec.inventory_item_id
                                 ,precision     => 5
                                 ,from_quantity => l_material_detail_rec.plan_qty
                                 ,from_unit     => l_material_detail_rec.dtl_um
                                 ,to_unit       => l_new_item_rec.primary_uom_code
                                 ,from_name     => NULL
                                 ,to_name       => NULL);
         ELSE
            l_temp_qty := l_material_detail_rec.plan_qty;
         END IF;

         l_material_detail_rec.original_primary_qty := l_temp_qty;

         IF l_batch_header_rec.update_inventory_ind = 'Y' THEN
            IF l_new_item_rec.mtl_transactions_enabled_flag = 'Y' THEN
               /* Insert Invisible Move Order Line */
               l_material_details_tbl(1) := l_material_detail_rec;
   
               gme_move_orders_pvt.create_move_order_lines (
                   p_move_order_header_id   => l_batch_header_rec.move_order_header_id
                  ,p_move_order_type        => gme_common_pvt.g_invis_move_order_type
                  ,p_material_details_tbl   => l_material_details_tbl
                  ,x_material_details_tbl   => l_material_details_tbl_out
                  ,x_trolin_tbl             => l_trolin_tbl
                  ,x_return_status          => l_return_status);
               
               IF l_return_status <> fnd_api.g_ret_sts_success THEN
                  l_oneitem_error := TRUE;
                  gme_common_pvt.count_and_get (x_count       => l_message_count,
                                             p_encoded     => fnd_api.g_false,
                                             x_data        => l_message_list
                                            );
                  
                   --FPBug#4991508 replaced hard coded output messages
                   /*
                   fnd_message.set_name('GME','GME_NO_SUBSTITUTION');
                   fnd_message.set_token('DOC',l_doc_str||' '||rec.batch_no);
                   fnd_message.set_token('LINE',rec.line_no);
                   fnd_message.set_token('ITEM',rec.item_no);
                   fnd_message.set_token('PLANQTY',rec.plan_qty||' '||rec.dtl_um);
                   fnd_message.set_token('MSG',l_message_list);
                   fnd_file.put (fnd_file.output,fnd_message.get);

                  fnd_file.put(fnd_file.output,l_message_list);
                  fnd_file.new_line (fnd_file.output, 1);
                  */
                  
                  --FPBug#4351032 used gme_common_pvt.log_message
                  gme_common_pvt.log_message(p_message_code => 'GME_NO_SUBSTITUTION'
                                            ,p_token1_name  => 'DOC'
                                            ,P_token1_value => l_doc_str||' '||rec.batch_no
                                            ,p_token2_name  => 'LINE'
                                            ,P_token2_value => rec.line_no
                                            ,p_token3_name  => 'ITEM'
                                            ,P_token3_value => rec.item_no
                                            ,p_token4_name  => 'PLANQTY'
                                            ,P_token4_value => rec.plan_qty||' '||rec.dtl_um
                                            ,p_token5_name  => 'MSG'
                                            ,P_token5_value => l_message_list
                                            );
                  gme_common_pvt.count_and_get (x_count       => l_message_count,
                                                p_encoded     => fnd_api.g_false,
                                                x_data        => l_message_list
                                               );
                  fnd_file.put(fnd_file.output,l_message_list);       
                  fnd_file.new_line (fnd_file.output, 1);
                  ROLLBACK TO create_trans;
                  GOTO NEXT_RECORD; --GO to next record
               END IF; 
               l_material_detail_rec.move_order_line_id := l_material_details_tbl_out(1).move_order_line_id;   
            ELSE --l_new_item_rec.mtl_transactions_enabled_flag <> 'Y' 
               l_material_detail_rec.move_order_line_id := NULL;   
            END IF; --IF l_new_item_rec.mtl_transactions_enabled_flag = 'Y' 
         END IF; -- IF l_batch_header_rec.update_inventory_ind = 'Y' THEN
         
         --Call material_line_dbl.update_rec
         l_return := gme_material_details_dbl.update_row (l_material_detail_rec);

         IF (l_return = FALSE) THEN
            gme_common_pvt.count_and_get (x_count       => l_message_count,
                                       p_encoded     => fnd_api.g_false,
                                       x_data        => l_message_list
                                      );
            l_oneitem_error := TRUE;
            --FPBug#4991508 replaced hard coded output messages
            /*
            fnd_message.set_name('GME','GME_NO_SUBSTITUTION');
            fnd_message.set_token('DOC',l_doc_str||' '||rec.batch_no);
            fnd_message.set_token('LINE',rec.line_no);
            fnd_message.set_token('ITEM',rec.item_no);
            fnd_message.set_token('PLANQTY',rec.plan_qty||' '||rec.dtl_um);
            fnd_message.set_token('MSG',l_message_list);
            fnd_file.put (fnd_file.output,fnd_message.get);
         
            fnd_file.put(fnd_file.output,l_message_list); 
            fnd_file.new_line (fnd_file.output, 1);
            */
            
            --FPBug#4351032 used gme_common_pvt.log_message
            gme_common_pvt.log_message(p_message_code => 'GME_NO_SUBSTITUTION'
                                      ,p_token1_name  => 'DOC'
                                      ,P_token1_value => l_doc_str||' '||rec.batch_no
                                      ,p_token2_name  => 'LINE'
                                      ,P_token2_value => rec.line_no
                                      ,p_token3_name  => 'ITEM'
                                      ,P_token3_value => rec.item_no
                                      ,p_token4_name  => 'PLANQTY'
                                      ,P_token4_value => rec.plan_qty||' '||rec.dtl_um
                                      ,p_token5_name  => 'MSG'
                                      ,P_token5_value => l_message_list
                                      );
            gme_common_pvt.count_and_get (x_count       => l_message_count,
                                          p_encoded     => fnd_api.g_false,
                                          x_data        => l_message_list
                                         );
            fnd_file.put(fnd_file.output,l_message_list);       
            fnd_file.new_line (fnd_file.output, 1);
            ROLLBACK TO create_trans;
            GOTO NEXT_RECORD; --GO to next record
         END IF; -- IF (l_return = FALSE) THEN

         OPEN c_batchsteps (l_material_detail_rec.material_detail_id);
         FETCH c_batchsteps INTO l_batchstep_rec;
         CLOSE c_batchsteps;

         /* FPBug#4351032 update original primary qty field as this field dont get updated
            using gme_material_details_dbl.update_row procedure */
         UPDATE gme_material_details
            SET original_primary_qty = l_material_detail_rec.original_primary_qty
          WHERE material_detail_id = l_material_detail_rec.material_detail_id;

         l_material_detail_rec.last_update_date := gme_common_pvt.get_timestamp;

/* Bug 7352169 - do not call auto detail line.
               -- Swapna K Bug#4354690 12-MAY-2005
         IF l_batch_header_rec.update_inventory_ind = 'Y' AND
            l_new_item_rec.mtl_transactions_enabled_flag = 'Y' AND
            l_new_item_rec.reservable_type = 1 
         THEN
            l_return_status := NULL;
            gme_reservations_pvt.auto_detail_line (
                        p_material_details_rec => l_material_detail_rec
                        ,x_return_status       => l_return_status);

            IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
               gme_debug.put_line ('return status from auto detail line is: ' || l_return_status);
            END IF;

            -- Validate Return Status
            IF l_return_status IN (fnd_api.g_ret_sts_unexp_error, fnd_api.g_ret_sts_error) THEN
               gme_common_pvt.count_and_get (x_count       => l_message_count,
                                             p_encoded     => fnd_api.g_false,
                                             x_data        => l_message_list
                                            );
               l_oneitem_error := TRUE;
          
              --FPBug#4991508 replaced hard coded output messages
               fnd_message.set_name('GME','GME_NO_SUBSTITUTION');
               fnd_message.set_token('DOC',l_doc_str||' '||rec.batch_no);
               fnd_message.set_token('LINE',rec.line_no);
               fnd_message.set_token('ITEM',rec.item_no);
               fnd_message.set_token('PLANQTY',rec.plan_qty||' '||rec.dtl_um);
               fnd_message.set_token('MSG',l_message_list);
               fnd_file.put (fnd_file.output,fnd_message.get);

               fnd_file.put(fnd_file.output,l_message_list);
               fnd_file.new_line (fnd_file.output, 1);
               --FPBug#4351032 used gme_common_pvt.log_message
               gme_common_pvt.log_message(p_message_code => 'GME_NO_SUBSTITUTION'
                                         ,p_token1_name  => 'DOC'
                                         ,P_token1_value => l_doc_str||' '||rec.batch_no
                                         ,p_token2_name  => 'LINE'
                                         ,P_token2_value => rec.line_no
                                         ,p_token3_name  => 'ITEM'
                                         ,P_token3_value => rec.item_no
                                         ,p_token4_name  => 'PLANQTY'
                                         ,P_token4_value => rec.plan_qty||' '||rec.dtl_um
                                         ,p_token5_name  => 'MSG'
                                         ,P_token5_value => l_message_list
                                         );
               gme_common_pvt.count_and_get (x_count       => l_message_count,
                                             p_encoded     => fnd_api.g_false,
                                             x_data        => l_message_list
                                            );
               fnd_file.put(fnd_file.output,l_message_list);       
               fnd_file.new_line (fnd_file.output, 1);
               ROLLBACK TO create_trans;
               GOTO NEXT_RECORD; --GO to next record
            END IF; -- IF l_return_status IN (fnd_api.g_ret_sts_unexp_error, fnd_api.g_ret_sts_error) THEN

         END IF;  -- update inventory ind 
         
End Bug 7352169 - do not call auto detail line. */

         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line ('ASQC is: ' || l_batch_header_rec.automatic_step_calculation);
         END IF;

         IF l_batch_header_rec.automatic_step_calculation = 1 THEN
            IF l_batch_header_rec.update_inventory_ind = 'Y' AND NOT l_trans_loaded THEN
               IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
                  gme_debug.put_line ('Entered into loading txns');
               END IF;

               -- Swapna K Bug#4354690 12-MAY-2005
               l_return_status := NULL;

               gme_trans_engine_util.load_rsrc_trans (p_batch_row         => l_batch_header_rec,
                                                      x_rsc_row_count     => l_rsc_trans_count,
                                                      x_return_status     => l_return_status
                                                     );

               IF l_return_status <> fnd_api.g_ret_sts_success THEN
                  gme_common_pvt.count_and_get (x_count       => l_message_count,
                                             p_encoded     => fnd_api.g_false,
                                             x_data        => l_message_list
                                            );
                  l_oneitem_error := TRUE;
                  --FPBug#4991508 replaced hard coded output messages
                  /*
                  fnd_message.set_name('GME','GME_NO_SUBSTITUTION');
                  fnd_message.set_token('DOC',l_doc_str||' '||rec.batch_no);
                  fnd_message.set_token('LINE',rec.line_no);
                  fnd_message.set_token('ITEM',rec.item_no);
                  fnd_message.set_token('PLANQTY',rec.plan_qty||' '||rec.dtl_um);
                  fnd_message.set_token('MSG',l_message_list);
                  fnd_file.put (fnd_file.output,fnd_message.get);

                  fnd_file.put(fnd_file.output,l_message_list);  
                  fnd_file.new_line (fnd_file.output, 1);
                  */
                  
                  --FPBug#4351032 used gme_common_pvt.log_message
                  gme_common_pvt.log_message(p_message_code => 'GME_NO_SUBSTITUTION'
                                            ,p_token1_name  => 'DOC'
                                            ,P_token1_value => l_doc_str||' '||rec.batch_no
                                            ,p_token2_name  => 'LINE'
                                            ,P_token2_value => rec.line_no
                                            ,p_token3_name  => 'ITEM'
                                            ,P_token3_value => rec.item_no
                                            ,p_token4_name  => 'PLANQTY'
                                            ,P_token4_value => rec.plan_qty||' '||rec.dtl_um
                                            ,p_token5_name  => 'MSG'
                                            ,P_token5_value => l_message_list
                                            );
                  gme_common_pvt.count_and_get (x_count       => l_message_count,
                                                p_encoded     => fnd_api.g_false,
                                                x_data        => l_message_list
                                               );
                  fnd_file.put(fnd_file.output,l_message_list);       
                  fnd_file.new_line (fnd_file.output, 1);
                  ROLLBACK TO create_trans;
                  GOTO NEXT_RECORD; --GO to next record
               END IF;

               l_trans_loaded := TRUE;
            END IF;   -- update inventory ind 

            IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
               gme_debug.put_line ('Before updating step qty');
            END IF;

            gme_update_step_qty_pvt.update_step_qty (p_batch_step_rec    => l_batchstep_rec,
                                                     x_message_count     => l_message_count,
                                                     x_message_list      => l_message_list,
                                                     x_return_status     => x_return_status,
                                                     x_batch_step_rec    => x_batchstep_rec
                                                    );

            IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
               gme_debug.put_line ('After update step qty, return status is: ' || x_return_status);
            END IF;
            -- Swapna K Bug#4354690 12-MAY-2005
            /*  l_return_status is replaced with x_return_status in the below if condition */
            IF x_return_status <> fnd_api.g_ret_sts_success THEN
               gme_common_pvt.count_and_get (x_count       => l_message_count,
                                          p_encoded     => fnd_api.g_false,
                                          x_data        => l_message_list
                                         );
               l_oneitem_error := TRUE;

               --FPBug#4991508 replaced hard coded output messages
               /*
               fnd_message.set_name('GME','GME_NO_SUBSTITUTION');
               fnd_message.set_token('DOC',l_doc_str||' '||rec.batch_no);
               fnd_message.set_token('LINE',rec.line_no);
               fnd_message.set_token('ITEM',rec.item_no);
               fnd_message.set_token('PLANQTY',rec.plan_qty||' '||rec.dtl_um);
               fnd_message.set_token('MSG',l_message_list);
               fnd_file.put (fnd_file.output,fnd_message.get);

               fnd_file.put(fnd_file.output,l_message_list);
               fnd_file.new_line (fnd_file.output, 1);
               */
               
               --FPBug#4351032 used gme_common_pvt.log_message
               gme_common_pvt.log_message(p_message_code => 'GME_NO_SUBSTITUTION'
                                         ,p_token1_name  => 'DOC'
                                         ,P_token1_value => l_doc_str||' '||rec.batch_no
                                         ,p_token2_name  => 'LINE'
                                         ,P_token2_value => rec.line_no
                                         ,p_token3_name  => 'ITEM'
                                         ,P_token3_value => rec.item_no
                                         ,p_token4_name  => 'PLANQTY'
                                         ,P_token4_value => rec.plan_qty||' '||rec.dtl_um
                                         ,p_token5_name  => 'MSG'
                                         ,P_token5_value => l_message_list
                                         );
               gme_common_pvt.count_and_get (x_count       => l_message_count,
                                             p_encoded     => fnd_api.g_false,
                                             x_data        => l_message_list
                                            );
               fnd_file.put(fnd_file.output,l_message_list);       
               fnd_file.new_line (fnd_file.output, 1);
               ROLLBACK TO create_trans;
               GOTO NEXT_RECORD; --GO to next record
            END IF;
         END IF;   -- ASQC 

         IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line ('Before consolidate transacitons');
         END IF;

         IF l_batch_header_rec.update_inventory_ind = 'Y' THEN

            IF l_batch_header_rec.automatic_step_calculation = 1 AND l_trans_loaded THEN
                gme_resource_engine_pvt.consolidate_batch_resources (l_batch_header_rec.batch_id,
                                                                     x_return_status
                                                                     );

                IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
                   gme_debug.put_line ('After consolidate resource transactions' || x_return_status);
                END IF;
                
                IF x_return_status <> fnd_api.g_ret_sts_success THEN
                   gme_common_pvt.count_and_get (x_count       => l_message_count,
                                              p_encoded     => fnd_api.g_false,
                                              x_data        => l_message_list
                                             );
                   l_oneitem_error := TRUE;

                   --FPBug#4991508 replaced hard coded output messages
                   /*
                   fnd_message.set_name('GME','GME_NO_SUBSTITUTION');
                   fnd_message.set_token('DOC',l_doc_str||' '||rec.batch_no);
                   fnd_message.set_token('LINE',rec.line_no);
                   fnd_message.set_token('ITEM',rec.item_no);
                   fnd_message.set_token('PLANQTY',rec.plan_qty||' '||rec.dtl_um);
                   fnd_message.set_token('MSG',l_message_list);
                   fnd_file.put (fnd_file.output,fnd_message.get);

                   fnd_file.put(fnd_file.output,l_message_list);
                   fnd_file.new_line (fnd_file.output, 1);
                   */
                   
                   --FPBug#4351032 used gme_common_pvt.log_message
                   gme_common_pvt.log_message(p_message_code => 'GME_NO_SUBSTITUTION'
                                             ,p_token1_name  => 'DOC'
                                             ,P_token1_value => l_doc_str||' '||rec.batch_no
                                             ,p_token2_name  => 'LINE'
                                             ,P_token2_value => rec.line_no
                                             ,p_token3_name  => 'ITEM'
                                             ,P_token3_value => rec.item_no
                                             ,p_token4_name  => 'PLANQTY'
                                             ,P_token4_value => rec.plan_qty||' '||rec.dtl_um
                                             ,p_token5_name  => 'MSG'
                                             ,P_token5_value => l_message_list
                                             );
                   gme_common_pvt.count_and_get (x_count       => l_message_count,
                                                 p_encoded     => fnd_api.g_false,
                                                 x_data        => l_message_list
                                                );
                   fnd_file.put(fnd_file.output,l_message_list);       
                   fnd_file.new_line (fnd_file.output, 1);
                   ROLLBACK TO create_trans;
                   GOTO NEXT_RECORD; --GO to next record
                END IF;
            END IF; /* l_batch_header_rec.automatic_step_calculation = 1 */
         END IF; /* l_batch_header_rec.update_inventory_ind = 'Y' */
         COMMIT;
         l_oneitem_success := TRUE;

         --FPBug#4991508 replaced hard coded output messages
         /*
         fnd_message.set_name('GME','GME_SUBSTITUTION_SUCCESS');
         fnd_message.set_token('DOC',l_doc_str||' '||rec.batch_no);
         fnd_message.set_token('LINE',rec.line_no);
         fnd_message.set_token('ITEM',rec.item_no);
         fnd_message.set_token('PLANQTY',rec.plan_qty||' '||rec.dtl_um);
         fnd_message.set_token('NEWITEM',l_new_item_rec.concatenated_segments);
         fnd_message.set_token('NEWQTY',l_formula_tbl (1).qty||' '||l_formula_tbl (1).detail_uom);
         fnd_file.put (fnd_file.output,fnd_message.get);

         fnd_file.put(fnd_file.output,l_message_list);  
         fnd_file.new_line (fnd_file.output, 1);
         */
         
         --FPBug#4351032 used gme_common_pvt.log_message
         gme_common_pvt.log_message(p_message_code => 'GME_SUBSTITUTION_SUCCESS'
                                   ,p_token1_name  => 'DOC'
                                   ,P_token1_value => l_doc_str||' '||rec.batch_no
                                   ,p_token2_name  => 'LINE'
                                   ,P_token2_value => rec.line_no
                                   ,p_token3_name  => 'ITEM'
                                   ,P_token3_value => rec.item_no
                                   ,p_token4_name  => 'PLANQTY'
                                   ,P_token4_value => rec.plan_qty||' '||rec.dtl_um
                                   ,p_token5_name  => 'NEWITEM'
                                   ,P_token5_value => l_new_item_rec.concatenated_segments
                                   ,p_token6_name  => 'NEWQTY'
                                   ,P_token6_value => l_formula_tbl (1).qty||' '||l_formula_tbl (1).detail_uom                                      
                                   );
         gme_common_pvt.count_and_get (x_count       => l_message_count,
                                       p_encoded     => fnd_api.g_false,
                                       x_data        => l_message_list
                                      );
         fnd_file.put(fnd_file.output,l_message_list);       
         fnd_file.new_line (fnd_file.output, 1);

         <<NEXT_RECORD>>
         NULL;
      END LOOP;

      fnd_file.new_line (fnd_file.output, 1);

      IF l_oneitem_success = TRUE AND l_oneitem_error = TRUE THEN
         gme_common_pvt.log_message('GME_ATLEAST_ONE_NOT_SUBSTITUTE');
         gme_common_pvt.count_and_get (x_count       => l_message_count,
                                       p_encoded     => fnd_api.g_false,
                                       x_data        => l_message_list
                                      );
         fnd_file.put(fnd_file.output,l_message_list);

         --FPBug#4991508 replaced hard coded output messages
         /*
         fnd_message.set_name('GME','GME_ATLEAST_ONE_NOT_SUBSTITUTE');
         fnd_file.put_line (fnd_file.output, fnd_message.get);
         */

         errbuf := l_message_list;
         retcode := 1; --warning
      ELSIF l_oneitem_success = FALSE AND l_oneitem_error = TRUE THEN
         --FPBug#4991508 replaced hard coded output messages
         /*
         fnd_message.set_name('GME','GME_NONE_SUBSTITUTED');
         fnd_file.put_line (fnd_file.output, fnd_message.get);
         */

         gme_common_pvt.log_message('GME_NONE_SUBSTITUTED');
         gme_common_pvt.count_and_get (x_count       => l_message_count,
                                       p_encoded     => fnd_api.g_false,
                                       x_data        => l_message_list
                                      );
         fnd_file.put(fnd_file.output,l_message_list);       
         errbuf := l_message_list;
         retcode := 2; --error
      ELSE
         --FPBug#4991508 replaced hard coded output messages
         /*
         fnd_message.set_name('GME','GME_SUBSTITUTION_SUCCESSFUL');
         fnd_file.put_line (fnd_file.output, fnd_message.get);
         */
         
         errbuf := 'Substitutions are successful';
         gme_common_pvt.log_message('GME_SUBSTITUTION_SUCCESSFUL');
         gme_common_pvt.count_and_get (x_count       => l_message_count,
                                       p_encoded     => fnd_api.g_false,
                                       x_data        => l_message_list
                                      );
         fnd_file.put(fnd_file.output,l_message_list);       
         errbuf := l_message_list;
         retcode := 0; --success
      END IF;
   EXCEPTION
      WHEN SETUP_FAILURE THEN
         fnd_file.put (fnd_file.LOG, 'Setup Failed for organization ID '||p_org_id);
         fnd_file.new_line (fnd_file.LOG, 1);
         errbuf := 'Setup Failed for organization ID '||p_org_id;
      WHEN OTHERS THEN
         fnd_file.put (fnd_file.LOG, SQLERRM);
         fnd_file.new_line (fnd_file.LOG, 1);
         errbuf := SQLERRM;
   END substitute_ingredients;


 /*======================================================================
  --  PROCEDURE:
  --    get_total_quantity
  --
  --  DESCRIPTION:
  --      Procedure to sum up all product quantities.
  --
  --  HISTORY:
  --    siva  FPBug# 4684029
  --    siva  FPBug#4684029 rework
  --      In exception block 'E', 'S' are replaced by FND_API variables.
  --    SivakumarG Bug#5111078 Added x_total_wip_plan_qty parameter
  ======================================================================*/
   PROCEDURE get_total_qty(
                   p_batch_id           IN         NUMBER,
                   p_line_type          IN         NUMBER,
                   p_uom                IN         VARCHAR2,
                   x_total_plan_qty     OUT NOCOPY NUMBER,
                   x_total_wip_plan_qty OUT NOCOPY NUMBER,
                   x_total_actual_qty   OUT NOCOPY NUMBER,
                   x_uom                OUT NOCOPY VARCHAR2,
                   x_return_status      OUT NOCOPY VARCHAR2) 
   IS
     CURSOR get_primary_product_uom ( v_batch_id IN NUMBER )IS
      SELECT gm.dtl_um
        FROM gmd_recipe_validity_rules vr, gme_material_details gm, gme_batch_header bh
       WHERE bh.recipe_validity_rule_id = vr.recipe_validity_rule_id
         AND bh.batch_id = gm.batch_id
         AND vr.inventory_item_id = gm.inventory_item_id
         AND gm.line_type = 1  /*FPBug# 4684029 rework */
         AND bh.batch_id = v_batch_id
         AND rownum = 1 ;

     CURSOR get_quantities ( v_batch_id IN NUMBER, v_line_type IN NUMBER )IS
      SELECT inventory_item_id, plan_qty, wip_plan_qty, actual_qty, dtl_um
       FROM  gme_material_details 
       WHERE batch_id = v_batch_id
         AND line_type = v_line_type;
          
     l_api_name                VARCHAR2 (30):= 'get_total_quantity';
     prod_uom                  VARCHAR2(3);
     l_item_id                 NUMBER;
     l_actual_qty              NUMBER := 0;
     l_plan_qty                NUMBER := 0;
     l_wip_plan_qty            NUMBER := 0;
     l_item_um                 VARCHAR2(3);     
     l_total_actual_qty        NUMBER := 0;
     l_total_plan_qty          NUMBER := 0;
     l_total_wip_plan_qty      NUMBER := 0;
      
     uom_conversion_failure    EXCEPTION ;
     invalid_batch_id          EXCEPTION ;
   BEGIN
     IF (NVL (g_debug, -1) = gme_debug.g_log_procedure) THEN
      gme_debug.put_line('Entering gme_api_grp.get_total_quantity with batch id '||p_batch_id);
     END IF;

     IF p_batch_id IS NULL THEN
      gme_common_pvt.log_message ('GME_INVALID_BATCH','ID','BATCH_ID');
      RAISE invalid_batch_id;
     END IF;
      
               
     IF p_uom IS NULL THEN
      OPEN get_primary_product_uom (p_batch_id) ;
      FETCH get_primary_product_uom INTO prod_uom ;
      CLOSE get_primary_product_uom ;
     ELSE
      prod_uom := p_uom;
     END IF;

     IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
      gme_debug.put_line('UOM being used for possible conversion is '||prod_uom);
     END IF;

     OPEN get_quantities(p_batch_id, p_line_type);
     LOOP
       FETCH get_quantities INTO l_item_id, l_plan_qty, l_wip_plan_qty, l_actual_qty, l_item_um;
       EXIT when get_quantities%NOTFOUND ;
       
       IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
         gme_debug.put_line('Fetched values for item id '||l_item_id||' are:');
         gme_debug.put_line('Plan qty '||l_plan_qty);
         gme_debug.put_line('WIP Plan qty '||l_wip_plan_qty);
         gme_debug.put_line('Actual qty '||l_actual_qty);
         gme_debug.put_line('UOM '||l_item_um);
       END IF;

       IF l_item_um <> prod_uom THEN
        l_plan_qty := inv_convert.inv_um_convert(l_item_id,
                                                 5,
                                                 l_plan_qty,
                                                 l_item_um,
                                                 prod_uom,
                                                 NULL,
                                                 NULL);
        IF l_plan_qty < 0 THEN
          CLOSE get_quantities;
          IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
           gme_debug.put_line('Conversion failed for item id is '||l_item_id);
           gme_debug.put_line('Plan qty '||l_plan_qty);
           gme_debug.put_line('From UOM '||l_item_um);
           gme_debug.put_line('To UOM '||prod_uom);
          END IF;

          -- Bug 9975725
          gme_common_pvt.log_message
             (p_message_code      => 'INV_UOM_CONVERSION_ERROR'
             ,p_token1_name       => 'uom1'
             ,p_token1_value      => l_item_um
             ,p_token2_name       => 'uom2'
             ,p_token2_value      => prod_uom
             ,p_token3_name       => 'module'
             ,p_token3_value      => 'GME_API_GRP.get_total_qty'
             ,p_product_code      => 'INV');
          
          RAISE uom_conversion_failure;
        END IF;
        
        l_actual_qty := inv_convert.inv_um_convert(l_item_id,
                                                   5,
                                                   l_actual_qty,
                                                   l_item_um,
                                                   prod_uom,
                                                   NULL,
                                                   NULL);
        IF l_actual_qty < 0 THEN
          CLOSE get_quantities;
          IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line('Converted values for item id '||l_item_id||' are:');
            gme_debug.put_line('Plan qty '||l_plan_qty);
            gme_debug.put_line('Actual qty '||l_actual_qty);
          END IF;
          RAISE uom_conversion_failure;
        END IF;

        --Bug#5111078 Begin
        l_wip_plan_qty := inv_convert.inv_um_convert(l_item_id,
                                                     5,
                                                     l_wip_plan_qty,
                                                     l_item_um,
                                                     prod_uom,
                                                     NULL,
                                                     NULL);
         IF l_wip_plan_qty < 0 THEN
          CLOSE get_quantities;
          IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
            gme_debug.put_line('Converted values for item id '||l_item_id||' are:');
            gme_debug.put_line('Plan qty '||l_plan_qty);
            gme_debug.put_line('WIP Plan qty '||l_wip_plan_qty);
            gme_debug.put_line('Actual qty '||l_actual_qty);
          END IF;
          RAISE uom_conversion_failure;
         END IF;
         --Bug#5111078 End
      END IF ;
        
      l_total_plan_qty := l_total_plan_qty + l_plan_qty ;
      --Bug#5111078
      l_total_wip_plan_qty := l_total_wip_plan_qty + l_wip_plan_qty;
      l_total_actual_qty := l_total_actual_qty + l_actual_qty ;
     END LOOP ;
     CLOSE get_quantities;

     x_total_plan_qty := l_total_plan_qty;
     --Bug#5111078
     x_total_wip_plan_qty:=l_total_wip_plan_qty;
     x_total_actual_qty := l_total_actual_qty;
     -- This line will return uom used for conversion.
     x_uom := prod_uom;
     
     IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
      gme_debug.put_line('Qtys being returned are:');
      gme_debug.put_line('Total plan qty '||l_total_plan_qty);
      gme_debug.put_line('Total actual qty '||l_total_actual_qty);
     END IF;
     x_return_status := fnd_api.g_ret_sts_success;
   EXCEPTION
     WHEN uom_conversion_failure THEN
             x_return_status := fnd_api.g_ret_sts_error;
     WHEN invalid_batch_id THEN
             x_return_status := fnd_api.g_ret_sts_error;        
     WHEN OTHERS THEN
             x_return_status := fnd_api.g_ret_sts_unexp_error;
        fnd_file.put (fnd_file.LOG, SQLERRM);
        fnd_file.new_line (fnd_file.LOG, 1);
 END get_total_qty;

 --siva  FPBug# 4684029 End
 
 /*======================================================================
  --  PROCEDURE:
  --    check_inv_negative
  --
  --  DESCRIPTION:
  --      Procedure to check whether inventory will be driven negative.
  --      RETURNS TRUE WHEN
  --        Org does not allow negative and transaction will drive qty -ve
  --        OR
  --        Org allows negative but reservations exist and transaction 
  --        will drive qty -ve
  --
  --  HISTORY:
  --    Jalaj Srivastava Created for Bug 5021522 
  ======================================================================*/   
  PROCEDURE check_inv_negative 
    ( p_transaction_id IN  NUMBER
     ,p_item_no        IN VARCHAR2
     ,x_msg_count      OUT NOCOPY NUMBER 
     ,x_msg_data       OUT NOCOPY VARCHAR2 
     ,x_return_status  OUT NOCOPY VARCHAR2
    ) IS
    l_mmt_rec  mtl_material_transactions%ROWTYPE;
    l_mmln_tbl gme_common_pvt.mtl_trans_lots_num_tbl; 
    l_ret boolean;
    l_api_name                VARCHAR2 (50)     := 'check_inv_negative';
  BEGIN 
    IF NVL (g_debug, gme_debug.g_log_procedure + 1) <=
                                                     gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
    END IF;
    -- Initially let us assign the return status to success
    x_return_status := fnd_api.g_ret_sts_success;
    
    /* Get transaction line and lots */
    gme_transactions_pvt.get_transactions
      ( p_transaction_id   => p_transaction_id
       ,x_mmt_rec          => l_mmt_rec
       ,x_mmln_tbl         => l_mmln_tbl
       ,x_return_status    => x_return_status
      );
    IF x_return_status <> fnd_api.g_ret_sts_success THEN
      RAISE fnd_api.g_exc_error;
    END IF;
      
    l_ret := gme_unrelease_batch_pvt.check_inv_negative
               ( p_mmt_rec         => l_mmt_rec
                ,p_mmln_tbl        => l_mmln_tbl
                ,p_org_neg_control => gme_common_pvt.g_allow_neg_inv
                ,p_item_no         => p_item_no
               );
    IF l_ret THEN
      RAISE fnd_api.g_exc_error;
    END IF;

    gme_common_pvt.count_and_get 
      ( x_count        => x_msg_count
       ,p_encoded      => fnd_api.g_false
       ,x_data         => x_msg_data);

    IF (g_debug <= gme_debug.g_log_statement) THEN
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
      gme_common_pvt.count_and_get 
        ( x_count        => x_msg_count
         ,p_encoded      => fnd_api.g_false
         ,x_data         => x_msg_data);
    WHEN fnd_api.g_exc_unexpected_error THEN
      x_return_status := fnd_api.g_ret_sts_unexp_error;
      gme_common_pvt.count_and_get 
        ( x_count        => x_msg_count
         ,p_encoded      => fnd_api.g_false
         ,x_data         => x_msg_data);
    WHEN OTHERS THEN
      x_return_status := fnd_api.g_ret_sts_unexp_error;
      fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
      gme_common_pvt.count_and_get 
        ( x_count        => x_msg_count
         ,p_encoded      => fnd_api.g_false
         ,x_data         => x_msg_data);
      IF (NVL (g_debug, -1) = gme_debug.g_log_unexpected) THEN
            gme_debug.put_line (   g_pkg_name
                                || '.'
                                || l_api_name
                                || ':'
                                || 'WHEN OTHERS:'
                                || SQLERRM);
      END IF;
  END check_inv_negative;      
  
  --nsinghi bug#5674398 Added following API
  /*======================================================================
  --  FUNCTION:
  --    get_ingr_sub_date
  --
  --  DESCRIPTION:
  --      Function to return the substitution effective date.
  --  HISTORY:
  --      Namit S. 27-NOV-2006   bug#5674398 
  ======================================================================*/

  FUNCTION get_ingr_sub_date (p_batch_id  IN gme_batch_header.batch_id%TYPE,
     p_material_detail_id  IN gme_material_details.material_detail_id%TYPE) RETURN DATE
  IS
      CURSOR cur_get_start_end_Date(
         p_batch_id  IN gme_batch_header.batch_id%TYPE
      ) IS
         SELECT plan_start_date,plan_cmplt_date
           FROM gme_batch_header
          WHERE batch_id = p_batch_id;

      CURSOR cur_get_matl_requirement_dt(
         p_material_detail_id   gme_material_details.material_detail_id%TYPE
      ) IS
         SELECT material_requirement_date
           FROM gme_material_details
          WHERE material_detail_id = p_material_detail_id;

        l_plan_start_date       DATE;
        l_plan_cmplt_date       DATE;
        l_ingred_sub_date       NUMBER;
        l_matl_requirement_dt   DATE;
        l_api_name              VARCHAR2 (50)     := 'get_ingr_sub_date';

  BEGIN
     --Take the value of the profile,GME: Ingredient Substitution Date
     l_ingred_sub_date := gme_common_pvt.g_ingr_sub_date;
     IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
           gme_debug.put_line (   g_pkg_name
                               || '.'
                               || l_api_name
                               || ':'
                               || 'l_ingred_sub_date : '
                               || l_ingred_sub_date);
     END IF;

     IF l_ingred_sub_date = 2 THEN -- Ingredient Requirement Date
        OPEN cur_get_matl_requirement_dt (p_material_detail_id);
        FETCH cur_get_matl_requirement_dt INTO l_matl_requirement_dt;
        CLOSE cur_get_matl_requirement_dt;
        IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
              gme_debug.put_line (   g_pkg_name
                                  || '.'
                                  || l_api_name
                                  || ':'
                                  || 'l_matl_requirement_dt : '
                                  || TO_CHAR(l_matl_requirement_dt, 'MON-DD-YYYY HH24:MI:SS'));
        END IF;
        RETURN l_matl_requirement_dt;
     ELSE
        --Fetch batch start and end dates
        OPEN cur_get_start_end_Date (p_batch_id);
        FETCH cur_get_start_end_Date INTO l_plan_start_date,l_plan_cmplt_date;
        CLOSE cur_get_start_end_Date;
        IF l_ingred_sub_date = 1 THEN -- Batch Start Date
           IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
                 gme_debug.put_line (   g_pkg_name
                                     || '.'
                                     || l_api_name
                                     || ':'
                                     || 'l_ingr_sub_dt = l_plan_start_date : '
                                     || TO_CHAR(l_plan_start_date, 'MON-DD-YYYY HH24:MI:SS'));
           END IF;
           RETURN l_plan_start_date;
        ELSIF l_ingred_sub_date = 3 THEN -- Batch Completion Date
           IF (NVL (g_debug, -1) = gme_debug.g_log_statement) THEN
                 gme_debug.put_line (   g_pkg_name
                                     || '.'
                                     || l_api_name
                                     || ':'
                                     || 'l_ingr_sub_dt = l_plan_cmplt_date : '
                                     || TO_CHAR(l_plan_cmplt_date, 'MON-DD-YYYY HH24:MI:SS'));
           END IF;
           RETURN l_plan_cmplt_date;
        END IF;
     END IF;

  END get_ingr_sub_date;

  /* Bug 5597385 Added below procedures */
  PROCEDURE get_mat_resvns(p_organization_id IN         NUMBER,
                           p_mat_det_id      IN         NUMBER,
                           p_batch_id        IN         NUMBER,
                           x_resvns_cur      OUT NOCOPY g_gmo_resvns,
                           x_return_status   OUT NOCOPY VARCHAR2) IS
    l_api_name            CONSTANT VARCHAR2 (30) := 'get_mat_resvns';
  BEGIN
    IF g_debug <= gme_debug.g_log_procedure THEN
      gme_debug.put_line ('Entering api ' || g_pkg_name || '.' || l_api_name);
    END IF;
    x_return_status := FND_API.G_RET_STS_SUCCESS;
    OPEN x_resvns_cur FOR
      SELECT mr.*
      FROM   mtl_reservations mr
      WHERE  mr.organization_id = p_organization_id
             AND mr.demand_source_type_id = gme_common_pvt.g_txn_source_type
             AND mr.demand_source_header_id = p_batch_id
             AND mr.demand_source_line_id = p_mat_det_id
             AND NOT EXISTS (SELECT 1
                             FROM   mtl_material_transactions_temp
                             WHERE  reservation_id = mr.reservation_id
                                    AND organization_id = p_organization_id)
      ORDER BY mr.requirement_date, mr.reservation_id;
  EXCEPTION
    WHEN OTHERS THEN
      x_return_status := FND_API.G_RET_STS_UNEXP_ERROR;
      fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
      IF (NVL (g_debug, -1) = gme_debug.g_log_unexpected) THEN
        gme_debug.put_line(g_pkg_name|| '.'|| l_api_name|| ':'|| 'WHEN OTHERS:'|| SQLERRM);
      END IF;
  END get_mat_resvns;
  
  PROCEDURE get_mat_pplots(p_mat_det_id      IN         NUMBER,
                           x_pplot_cur       OUT NOCOPY g_gmo_pplots,
                           x_return_status   OUT NOCOPY VARCHAR2) IS
    l_api_name            CONSTANT VARCHAR2 (30) := 'get_mat_pplots';
  BEGIN
    IF g_debug <= gme_debug.g_log_procedure THEN
      gme_debug.put_line ('Entering api ' || g_pkg_name || '.' || l_api_name);
    END IF;
    x_return_status := FND_API.G_RET_STS_SUCCESS;
    OPEN x_pplot_cur FOR
      SELECT *
      FROM   gme_pending_product_lots
      WHERE  material_detail_id = p_mat_det_id
      ORDER BY sequence asc, lot_number asc;
  EXCEPTION
    WHEN OTHERS THEN
      x_return_status := FND_API.G_RET_STS_UNEXP_ERROR;
      fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
      IF (NVL (g_debug, -1) = gme_debug.g_log_unexpected) THEN
        gme_debug.put_line(g_pkg_name|| '.'|| l_api_name|| ':'|| 'WHEN OTHERS:'|| SQLERRM);
      END IF;
  END get_mat_pplots;
  
  PROCEDURE get_mat_trans(p_organization_id IN         NUMBER,
                          p_mat_det_id      IN         NUMBER,
                          p_batch_id        IN         NUMBER,
                          x_txns_cur        OUT NOCOPY g_gmo_txns,
                          x_return_status   OUT NOCOPY VARCHAR2) IS
    l_api_name            CONSTANT VARCHAR2 (30) := 'get_mat_trans';
  BEGIN
    IF g_debug <= gme_debug.g_log_procedure THEN
      gme_debug.put_line ('Entering api ' || g_pkg_name || '.' || l_api_name);
    END IF;
    x_return_status := FND_API.G_RET_STS_SUCCESS;
    OPEN x_txns_cur FOR
      SELECT *
      FROM   mtl_material_transactions mmt
      WHERE  trx_source_line_id = p_mat_det_id
             AND transaction_source_id = p_batch_id
             AND transaction_source_type_id = gme_common_pvt.g_txn_source_type
             AND NOT EXISTS (SELECT /*+ no_unnest */
                                   transaction_id1
                             FROM  gme_transaction_pairs
                             WHERE transaction_id1 = mmt.transaction_id
                                   AND pair_type = gme_common_pvt.g_pairs_reversal_type)
      ORDER BY mmt.transaction_id;
  EXCEPTION
    WHEN OTHERS THEN
      x_return_status := FND_API.G_RET_STS_UNEXP_ERROR;
      fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
      IF (NVL (g_debug, -1) = gme_debug.g_log_unexpected) THEN
        gme_debug.put_line(g_pkg_name|| '.'|| l_api_name|| ':'|| 'WHEN OTHERS:'|| SQLERRM);
      END IF;
  END get_mat_trans;
  
  PROCEDURE get_lot_trans(p_transaction_id  IN  NUMBER,
                          x_lot_txns_cur    OUT NOCOPY g_gmo_lot_txns,
                          x_return_status   OUT NOCOPY VARCHAR2) IS
    l_api_name            CONSTANT VARCHAR2 (30) := 'get_lot_trans';
  BEGIN
    IF g_debug <= gme_debug.g_log_procedure THEN
      gme_debug.put_line ('Entering api ' || g_pkg_name || '.' || l_api_name);
    END IF;
    x_return_status := FND_API.G_RET_STS_SUCCESS;
    OPEN x_lot_txns_cur FOR
      SELECT *
      FROM  mtl_transaction_lot_numbers
      WHERE transaction_id = p_transaction_id;
  EXCEPTION
    WHEN OTHERS THEN
      x_return_status := FND_API.G_RET_STS_UNEXP_ERROR;
      fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
      IF (NVL (g_debug, -1) = gme_debug.g_log_unexpected) THEN
        gme_debug.put_line(g_pkg_name|| '.'|| l_api_name|| ':'|| 'WHEN OTHERS:'|| SQLERRM);
      END IF;
  END get_lot_trans;
                                
  PROCEDURE create_material_txn(p_mmti_rec        IN         mtl_transactions_interface%ROWTYPE,
                                p_mmli_tbl        IN         gme_common_pvt.mtl_trans_lots_inter_tbl,
                                x_return_status   OUT NOCOPY VARCHAR2) IS
    l_api_name            CONSTANT VARCHAR2 (30) := 'create_material_txn';
    l_return_status       VARCHAR2(1);
    setup_failed          EXCEPTION;
    create_txn_fail       EXCEPTION;
    validate_txn_fail     EXCEPTION;
  BEGIN
    IF g_debug <= gme_debug.g_log_procedure THEN
      gme_debug.put_line ('Entering api ' || g_pkg_name || '.' || l_api_name);
    END IF;
    x_return_status := FND_API.G_RET_STS_SUCCESS;
    IF NOT(gme_common_pvt.g_setup_done) THEN
      gme_common_pvt.g_setup_done := gme_common_pvt.setup(p_org_id => p_mmti_rec.organization_id);
      IF NOT(gme_common_pvt.g_setup_done) THEN      
        RAISE setup_failed;
      END IF;
    END IF;
    IF (gme_common_pvt.g_timestamp IS NULL) THEN
      gme_common_pvt.set_timestamp;
    END IF;
    gme_transactions_pvt.gmo_pre_process_val(p_mmti_rec      => p_mmti_rec,
                                             p_mmli_tbl      => p_mmli_tbl,
                                             p_mode          => 'I',
                                             x_return_status => l_return_status);
    IF (l_return_status <> FND_API.G_RET_STS_SUCCESS) THEN
      RAISE validate_txn_fail;
    END IF;
    gme_transactions_pvt.create_material_txn(p_mmti_rec      => p_mmti_rec,
                                             p_mmli_tbl      => p_mmli_tbl,
                                             x_return_status => l_return_status);
    IF (l_return_status <> FND_API.G_RET_STS_SUCCESS) THEN
      RAISE create_txn_fail;
    END IF;
    IF g_debug <= gme_debug.g_log_procedure THEN
      gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
    END IF;
  EXCEPTION
    WHEN setup_failed THEN
      x_return_status := FND_API.G_RET_STS_ERROR;
    WHEN create_txn_fail OR validate_txn_fail THEN
      x_return_status := l_return_status;
    WHEN OTHERS THEN
      x_return_status := FND_API.G_RET_STS_UNEXP_ERROR;
      fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
      IF (NVL (g_debug, -1) = gme_debug.g_log_unexpected) THEN
        gme_debug.put_line(g_pkg_name|| '.'|| l_api_name|| ':'|| 'WHEN OTHERS:'|| SQLERRM);
      END IF;
  END create_material_txn;
  
  PROCEDURE update_material_txn(p_transaction_id  IN         NUMBER,
                                p_mmti_rec        IN         mtl_transactions_interface%ROWTYPE,
                                p_mmli_tbl        IN         gme_common_pvt.mtl_trans_lots_inter_tbl,
                                x_return_status   OUT NOCOPY VARCHAR2) IS
    l_api_name            CONSTANT VARCHAR2 (30) := 'update_material_txn';
    l_return_status       VARCHAR2(1);
    l_org_id              NUMBER;
    l_batch_id            NUMBER;
    l_material_detail_id  NUMBER;
    l_txn_type_id         NUMBER;
    CURSOR Cur_get_trans(v_transaction_id IN NUMBER) IS
      SELECT organization_id, transaction_source_id, trx_source_line_id, transaction_type_id
      FROM   mtl_material_transactions
      WHERE  transaction_id = v_transaction_id;
    setup_failed          EXCEPTION;
    update_txn_fail       EXCEPTION;
    validate_txn_fail     EXCEPTION;
    update_txn_mismatch   EXCEPTION;    
  BEGIN
    IF g_debug <= gme_debug.g_log_procedure THEN
      gme_debug.put_line ('Entering api ' || g_pkg_name || '.' || l_api_name);
    END IF;
    x_return_status := FND_API.G_RET_STS_SUCCESS;
    IF NOT(gme_common_pvt.g_setup_done) THEN
      gme_common_pvt.g_setup_done := gme_common_pvt.setup(p_org_id => p_mmti_rec.organization_id);
      IF NOT(gme_common_pvt.g_setup_done) THEN      
        RAISE setup_failed;
      END IF;
    END IF;
    IF (gme_common_pvt.g_timestamp IS NULL) THEN
      gme_common_pvt.set_timestamp;
    END IF;
    OPEN Cur_get_trans(p_transaction_id);
    FETCH Cur_get_trans INTO l_org_id, l_batch_id, l_material_detail_id, l_txn_type_id;
    CLOSE Cur_get_trans;
    IF (p_mmti_rec.organization_id <> l_org_id
        OR p_mmti_rec.transaction_source_id <> l_batch_id
        OR p_mmti_rec.trx_source_line_id <> l_material_detail_id
        OR p_mmti_rec.transaction_type_id <> l_txn_type_id) THEN
      RAISE update_txn_mismatch;
    END IF;
    gme_transactions_pvt.gmo_pre_process_val(p_mmti_rec      => p_mmti_rec,
                                             p_mmli_tbl      => p_mmli_tbl,
                                             p_mode          => 'U',
                                             x_return_status => l_return_status);
    IF (l_return_status <> FND_API.G_RET_STS_SUCCESS) THEN
      RAISE validate_txn_fail;
    END IF;
    gme_transactions_pvt.update_material_txn(p_transaction_id => p_transaction_id,
                                             p_mmti_rec       => p_mmti_rec,
                                             p_mmli_tbl       => p_mmli_tbl,
                                             x_return_status  => l_return_status);
    IF (l_return_status <> FND_API.G_RET_STS_SUCCESS) THEN
      RAISE update_txn_fail;
    END IF;
    IF g_debug <= gme_debug.g_log_procedure THEN
      gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
    END IF;
  EXCEPTION
    WHEN setup_failed THEN
      x_return_status := FND_API.G_RET_STS_ERROR;
    WHEN update_txn_mismatch THEN
      gme_common_pvt.log_message('GME_TXN_UPDATE_MISMATCH');
      x_return_status := FND_API.G_RET_STS_ERROR;
    WHEN update_txn_fail OR validate_txn_fail THEN
      x_return_status := l_return_status;
    WHEN OTHERS THEN
      x_return_status := FND_API.G_RET_STS_UNEXP_ERROR;
      fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
      IF (NVL (g_debug, -1) = gme_debug.g_log_unexpected) THEN
        gme_debug.put_line(g_pkg_name|| '.'|| l_api_name|| ':'|| 'WHEN OTHERS:'|| SQLERRM);
      END IF;
  END update_material_txn;
  
  PROCEDURE delete_material_txn(p_organization_id IN         NUMBER,
                                p_transaction_id  IN         NUMBER,
                                x_return_status   OUT NOCOPY VARCHAR2) IS
    l_api_name            CONSTANT VARCHAR2 (30) := 'delete_material_txn';
    l_return_status       VARCHAR2(1);
    l_mmt_rec             mtl_material_transactions%ROWTYPE;
    l_mmln_tbl            gme_common_pvt.mtl_trans_lots_num_tbl;
    l_mmti_rec            mtl_transactions_interface%ROWTYPE;
    l_mmli_tbl            gme_common_pvt.mtl_trans_lots_inter_tbl;
    setup_failed          EXCEPTION;
    delete_txn_fail       EXCEPTION;
    get_txn_fail          EXCEPTION;
    const_txn_fail        EXCEPTION;
    validate_txn_fail     EXCEPTION;
  BEGIN
    IF g_debug <= gme_debug.g_log_procedure THEN
      gme_debug.put_line ('Entering api ' || g_pkg_name || '.' || l_api_name);
    END IF;
    x_return_status := FND_API.G_RET_STS_SUCCESS;
    IF NOT(gme_common_pvt.g_setup_done) THEN
      gme_common_pvt.g_setup_done := gme_common_pvt.setup(p_org_id => p_organization_id);
      IF NOT(gme_common_pvt.g_setup_done) THEN      
        RAISE setup_failed;
      END IF;
    END IF;
    IF (gme_common_pvt.g_timestamp IS NULL) THEN
      gme_common_pvt.set_timestamp;
    END IF;
    gme_transactions_pvt.get_mmt_transactions(p_transaction_id => p_transaction_id,
                                              x_mmt_rec        => l_mmt_rec,
                                              x_mmln_tbl       => l_mmln_tbl,
                                              x_return_status  => l_return_status);
    IF (l_return_status <> FND_API.G_RET_STS_SUCCESS) THEN
      RAISE get_txn_fail;
    END IF;
    gme_transactions_pvt.construct_mmti(p_mmt_rec       => l_mmt_rec,
                                        p_mmln_tbl      => l_mmln_tbl,
                                        x_mmti_rec      => l_mmti_rec,
                                        x_mmli_tbl      => l_mmli_tbl,
                                        x_return_status => l_return_status);
    IF (l_return_status <> FND_API.G_RET_STS_SUCCESS) THEN
      RAISE const_txn_fail;
    END IF;
    gme_transactions_pvt.gmo_pre_process_val(p_mmti_rec      => l_mmti_rec,
                                             p_mmli_tbl      => l_mmli_tbl,
                                             p_mode          => 'D',
                                             x_return_status => l_return_status);
    IF (l_return_status <> FND_API.G_RET_STS_SUCCESS) THEN
      RAISE validate_txn_fail;
    END IF;
    gme_transactions_pvt.delete_material_txn(p_transaction_id => p_transaction_id,
                                             x_return_status  => l_return_status);
    IF (l_return_status <> FND_API.G_RET_STS_SUCCESS) THEN
      RAISE delete_txn_fail;
    END IF;
    IF g_debug <= gme_debug.g_log_procedure THEN
      gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
    END IF;
  EXCEPTION
    WHEN setup_failed THEN
      x_return_status := FND_API.G_RET_STS_ERROR;
    WHEN delete_txn_fail OR get_txn_fail OR const_txn_fail OR validate_txn_fail THEN
      x_return_status := l_return_status;
    WHEN OTHERS THEN
      x_return_status := FND_API.G_RET_STS_UNEXP_ERROR;
      fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
      IF (NVL (g_debug, -1) = gme_debug.g_log_unexpected) THEN
        gme_debug.put_line(g_pkg_name|| '.'|| l_api_name|| ':'|| 'WHEN OTHERS:'|| SQLERRM);
      END IF;
  END delete_material_txn;
  
  PROCEDURE create_resource_txn(p_rsrc_txn_gtmp_rec IN gme_resource_txns_gtmp%ROWTYPE,
                                x_rsrc_txn_gtmp_rec OUT NOCOPY gme_resource_txns_gtmp%ROWTYPE,
                                x_return_status     OUT NOCOPY VARCHAR2) IS
    l_api_name            CONSTANT VARCHAR2 (30) := 'create_resource_txn';
    l_return_status       VARCHAR2(1);
    l_hour_um             VARCHAR2(3);
    l_line_id             NUMBER;
    l_instance_id         NUMBER;
    l_reason_id           NUMBER;
    l_step_status         NUMBER;
    l_usage_time          NUMBER;
    l_txn_usage           NUMBER;
    l_rsrc_trans_count    NUMBER;
    l_trans_date          DATE;
    l_batch_header_rec    gme_batch_header%ROWTYPE;
    l_rsrc_txn_gtmp_rec   gme_resource_txns_gtmp%ROWTYPE;
    l_step_resources      gme_batch_step_resources%ROWTYPE;
    setup_failed           EXCEPTION;
    create_txn_fail        EXCEPTION;
    validate_txn_fail      EXCEPTION;
    uom_conversion_err     EXCEPTION;
    missing_profile_option EXCEPTION;
    rsrc_fetch_err         EXCEPTION;
    rsrc_update_err        EXCEPTION;
    reduce_pend_usage_err  EXCEPTION;
    error_load_trans       EXCEPTION;
  BEGIN
    IF g_debug <= gme_debug.g_log_procedure THEN
      gme_debug.put_line ('Entering api ' || g_pkg_name || '.' || l_api_name);
    END IF;
    x_return_status := FND_API.G_RET_STS_SUCCESS;
    IF NOT(gme_common_pvt.g_setup_done) THEN
      gme_common_pvt.g_setup_done := gme_common_pvt.setup(p_org_id => p_rsrc_txn_gtmp_rec.organization_id);
      IF NOT(gme_common_pvt.g_setup_done) THEN      
        RAISE setup_failed;
      END IF;
    END IF;
    gme_common_pvt.set_timestamp;
    gme_resource_engine_pvt.validate_rsrc_txn_param(p_called_from       => 3
                                                   ,p_batchstep_rsrc_id => p_rsrc_txn_gtmp_rec.line_id
                                                   ,p_org_code          => gme_common_pvt.g_organization_code
                                                   ,p_trans_date        => p_rsrc_txn_gtmp_rec.trans_date
                                                   ,p_start_date        => p_rsrc_txn_gtmp_rec.start_date
                                                   ,p_end_date          => p_rsrc_txn_gtmp_rec.end_date
                                                   ,p_usage             => p_rsrc_txn_gtmp_rec.resource_usage
                                                   ,p_reason_name       => NULL
                                                   ,p_reason_id         => p_rsrc_txn_gtmp_rec.reason_id
                                                   ,p_instance_no       => NULL
                                                   ,p_instance_id       => p_rsrc_txn_gtmp_rec.instance_id
                                                   ,x_line_id           => l_line_id
                                                   ,x_step_status       => l_step_status
                                                   ,x_batch_header_rec  => l_batch_header_rec
                                                   ,x_instance_id       => l_instance_id
                                                   ,x_reason_id         => l_reason_id
                                                   ,x_return_status     => l_return_status
                                                   ,x_trans_date        => l_trans_date);
    IF (l_return_status <> FND_API.G_RET_STS_SUCCESS) THEN
      RAISE validate_txn_fail;
    END IF;
    l_usage_time := (p_rsrc_txn_gtmp_rec.end_date - p_rsrc_txn_gtmp_rec.start_date) * 24;
    l_hour_um    := fnd_profile.value_specific(name         => 'BOM:HOUR_UOM_CODE'
                                              ,user_id      => gme_common_pvt.g_user_ident);
    IF (l_hour_um IS NULL) THEN
      gme_common_pvt.log_message('GME_API_UNABLE_TO_GET_CONSTANT','CONSTANT_NAME','BOM:HOUR_UOM_CODE');
      RAISE missing_profile_option;
    END IF;
    IF l_hour_um <> p_rsrc_txn_gtmp_rec.trans_um THEN
      l_txn_usage := inv_convert.inv_um_convert (item_id            => 0
                                                ,PRECISION          => 5
                                                ,from_quantity      => l_usage_time
                                                ,from_unit          => l_hour_um
                                                ,to_unit            => p_rsrc_txn_gtmp_rec.trans_um
                                                ,from_name          => NULL
                                                ,to_name            => NULL);
      IF (l_txn_usage = -99999) THEN
        gme_common_pvt.log_message ('GME_RSRC_USG_NT_CNV_SYUOM', 'SY_UOM', l_hour_um, 'RSRC_USG_UOM', p_rsrc_txn_gtmp_rec.trans_um);
        RAISE uom_conversion_err;
      END IF;
    ELSE
      l_txn_usage := l_usage_time;
    END IF;
    l_rsrc_txn_gtmp_rec := p_rsrc_txn_gtmp_rec;
    l_rsrc_txn_gtmp_rec.resource_usage := l_txn_usage;
    gme_resource_engine_pvt.create_resource_trans(p_tran_rec      => l_rsrc_txn_gtmp_rec
                                                 ,x_tran_rec      => x_rsrc_txn_gtmp_rec
                                                 ,x_return_status => l_return_status);
    IF (l_return_status <> FND_API.G_RET_STS_SUCCESS) THEN
      RAISE create_txn_fail;
    END IF;
    l_step_resources.batchstep_resource_id := l_rsrc_txn_gtmp_rec.line_id;
    IF NOT Gme_Batch_Step_Resources_Dbl.fetch_row(p_batch_step_resources      => l_step_resources
                                                 ,x_batch_step_resources      => l_step_resources) THEN
      RAISE rsrc_fetch_err;
    END IF;
    l_step_resources.actual_rsrc_usage := NVL (l_step_resources.actual_rsrc_usage, 0) + l_txn_usage;
    IF l_step_status = 2 THEN
      Gme_Trans_Engine_Util.load_rsrc_trans(p_batch_row          => l_batch_header_rec
                                           ,x_rsc_row_count      => l_rsrc_trans_count
                                           ,x_return_status      => l_return_status);
      IF l_return_status <> x_return_status THEN
        RAISE error_load_trans;
      END IF;
      Gme_Update_Step_Qty_Pvt.reduce_pending_usage(p_batch_step_resources_rec      => l_step_resources
                                                  ,x_return_status                 => l_return_status);
      IF l_return_status <> 'S' THEN
        RAISE reduce_pend_usage_err;
      END IF;
    END IF;
    IF NOT Gme_Batch_Step_Resources_Dbl.update_row(p_batch_step_resources => l_step_resources) THEN
      RAISE rsrc_update_err;
    END IF;
    IF g_debug <= gme_debug.g_log_procedure THEN
      gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
    END IF;
  EXCEPTION
    WHEN setup_failed OR uom_conversion_err OR missing_profile_option OR rsrc_fetch_err OR rsrc_update_err THEN
      x_return_status := FND_API.G_RET_STS_ERROR;
    WHEN create_txn_fail OR validate_txn_fail OR error_load_trans OR reduce_pend_usage_err THEN
      x_return_status := l_return_status;
    WHEN OTHERS THEN
      x_return_status := FND_API.G_RET_STS_UNEXP_ERROR;
      fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
      IF (NVL (g_debug, -1) = gme_debug.g_log_unexpected) THEN
        gme_debug.put_line(g_pkg_name|| '.'|| l_api_name|| ':'|| 'WHEN OTHERS:'|| SQLERRM);
      END IF;
  END create_resource_txn;
  
  PROCEDURE update_resource_txn(p_rsrc_txn_gtmp_rec IN gme_resource_txns_gtmp%ROWTYPE,
                                x_return_status     OUT NOCOPY VARCHAR2) IS
    l_api_name            CONSTANT VARCHAR2 (30) := 'update_resource_txn';
    l_return_status       VARCHAR2(1);
    l_line_id             NUMBER;
    l_instance_id         NUMBER;
    l_reason_id           NUMBER;
    l_step_status         NUMBER;
    l_rsrc_trans_count    NUMBER;
    l_trans_date          DATE;
    l_batch_header_rec    gme_batch_header%ROWTYPE;
    l_step_resources      gme_batch_step_resources%ROWTYPE;
    l_new_step_resources  gme_batch_step_resources%ROWTYPE;
    error_load_trans      EXCEPTION;
    setup_failed          EXCEPTION;
    update_txn_fail       EXCEPTION;
    validate_txn_fail     EXCEPTION;
    rsrc_fetch_err        EXCEPTION;
    upd_rsrc_err          EXCEPTION;
    get_usage_fail        EXCEPTION;
    reduce_pend_usage_err EXCEPTION;
  BEGIN
    IF g_debug <= gme_debug.g_log_procedure THEN
      gme_debug.put_line ('Entering api ' || g_pkg_name || '.' || l_api_name);
    END IF;
    x_return_status := FND_API.G_RET_STS_SUCCESS;
    IF NOT(gme_common_pvt.g_setup_done) THEN
      gme_common_pvt.g_setup_done := gme_common_pvt.setup(p_org_id => p_rsrc_txn_gtmp_rec.organization_id);
      IF NOT(gme_common_pvt.g_setup_done) THEN      
        RAISE setup_failed;
      END IF;
    END IF;
    gme_common_pvt.set_timestamp;
    gme_resource_engine_pvt.validate_rsrc_txn_param(p_called_from       => 1
                                                   ,p_batchstep_rsrc_id => p_rsrc_txn_gtmp_rec.line_id
                                                   ,p_org_code          => gme_common_pvt.g_organization_code
                                                   ,p_trans_date        => p_rsrc_txn_gtmp_rec.trans_date
                                                   ,p_start_date        => p_rsrc_txn_gtmp_rec.start_date
                                                   ,p_end_date          => p_rsrc_txn_gtmp_rec.end_date
                                                   ,p_usage             => p_rsrc_txn_gtmp_rec.resource_usage
                                                   ,p_reason_name       => NULL
                                                   ,p_reason_id         => p_rsrc_txn_gtmp_rec.reason_id
                                                   ,p_instance_no       => NULL
                                                   ,p_instance_id       => p_rsrc_txn_gtmp_rec.instance_id
                                                   ,x_line_id           => l_line_id
                                                   ,x_step_status       => l_step_status
                                                   ,x_batch_header_rec  => l_batch_header_rec
                                                   ,x_instance_id       => l_instance_id
                                                   ,x_reason_id         => l_reason_id
                                                   ,x_return_status     => l_return_status
                                                   ,x_trans_date        => l_trans_date);
    IF (l_return_status <> FND_API.G_RET_STS_SUCCESS) THEN
      RAISE validate_txn_fail;
    END IF;
    gme_resource_engine_pvt.update_resource_trans(p_tran_rec      => p_rsrc_txn_gtmp_rec
                                                 ,x_return_status => l_return_status);
    IF (l_return_status <> FND_API.G_RET_STS_SUCCESS) THEN
      RAISE update_txn_fail;
    END IF;
    l_step_resources.batchstep_resource_id := p_rsrc_txn_gtmp_rec.line_id;
    IF NOT Gme_Batch_Step_Resources_Dbl.fetch_row(p_batch_step_resources => l_step_resources
                                                 ,x_batch_step_resources => l_step_resources) THEN
      RAISE rsrc_fetch_err;
    END IF;
    gme_resource_engine_pvt.get_resource_usage(p_step_resources_rec => l_step_resources
                                              ,x_step_resources_rec => l_new_step_resources
                                              ,x_return_status      => l_return_status);
    IF (l_return_status <> FND_API.G_RET_STS_SUCCESS) THEN
      RAISE get_usage_fail;
    END IF;
    IF (NVL(l_step_resources.actual_rsrc_usage, -1) <> NVL(l_new_step_resources.actual_rsrc_usage, -1)) THEN
      l_step_resources.actual_rsrc_usage := l_new_step_resources.actual_rsrc_usage;
      IF l_step_status = 2 THEN    
        gme_update_step_qty_pvt.reduce_pending_usage(p_batch_step_resources_rec => l_step_resources,
                                                     x_return_status            => l_return_status);
        IF (l_return_status <> FND_API.G_RET_STS_SUCCESS) THEN
          RAISE reduce_pend_usage_err;
        END IF;
      END IF;
      IF NOT gme_batch_step_resources_dbl.update_row(p_batch_step_resources => l_step_resources) THEN
        RAISE upd_rsrc_err;
      END IF;    
    END IF;
    IF g_debug <= gme_debug.g_log_procedure THEN
      gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
    END IF;
  EXCEPTION
    WHEN setup_failed OR error_load_trans OR rsrc_fetch_err OR get_usage_fail OR upd_rsrc_err THEN
      x_return_status := FND_API.G_RET_STS_ERROR;
    WHEN update_txn_fail OR validate_txn_fail OR reduce_pend_usage_err THEN
      x_return_status := l_return_status;
    WHEN OTHERS THEN
      x_return_status := FND_API.G_RET_STS_UNEXP_ERROR;
      fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
      IF (NVL (g_debug, -1) = gme_debug.g_log_unexpected) THEN
        gme_debug.put_line(g_pkg_name|| '.'|| l_api_name|| ':'|| 'WHEN OTHERS:'|| SQLERRM);
      END IF;
  END update_resource_txn;
  
  PROCEDURE delete_resource_txn(p_rsrc_txn_gtmp_rec IN gme_resource_txns_gtmp%ROWTYPE,
                                x_return_status     OUT NOCOPY VARCHAR2) IS
    l_api_name            CONSTANT VARCHAR2 (30) := 'delete_resource_txn';
    l_asqc                NUMBER;
    l_return_status       VARCHAR2(1);
    l_step_status         VARCHAR2(1);
    l_step_resources      gme_batch_step_resources%ROWTYPE;
    l_new_step_resources  gme_batch_step_resources%ROWTYPE;
    l_rsrc_txn_gtmp_rec   gme_resource_txns_gtmp%ROWTYPE;
    CURSOR Cur_get_step(v_line_id NUMBER) IS
      SELECT s.step_status, h.automatic_step_calculation
      FROM   gme_batch_steps s, gme_batch_step_activities a, gme_batch_step_resources r, gme_batch_header h
      WHERE  r.batchstep_resource_id = v_line_id
             AND a.batchstep_activity_id = r.batchstep_activity_id
             AND s.batchstep_id = a.batchstep_id
             AND h.batch_id = s.batch_id;
    setup_failed          EXCEPTION;
    delete_txn_fail       EXCEPTION;
    fetch_txn_failed      EXCEPTION;
    validation_fail       EXCEPTION;
    rsrc_fetch_err        EXCEPTION;
    upd_rsrc_err          EXCEPTION;
    get_usage_fail        EXCEPTION;
    reduce_pend_usage_err EXCEPTION;
  BEGIN
    IF g_debug <= gme_debug.g_log_procedure THEN
      gme_debug.put_line ('Entering api ' || g_pkg_name || '.' || l_api_name);
    END IF;
    x_return_status := FND_API.G_RET_STS_SUCCESS;
    IF NOT(gme_common_pvt.g_setup_done) THEN
      gme_common_pvt.g_setup_done := gme_common_pvt.setup(p_org_id => p_rsrc_txn_gtmp_rec.organization_id);
      IF NOT(gme_common_pvt.g_setup_done) THEN      
        RAISE setup_failed;
      END IF;
    END IF;
    gme_common_pvt.set_timestamp;
    IF NOT gme_resource_txns_gtmp_dbl.fetch_row(p_rsrc_txn_gtmp_rec, l_rsrc_txn_gtmp_rec) THEN
      RAISE fetch_txn_failed;
    END IF;
    OPEN Cur_get_step(l_rsrc_txn_gtmp_rec.line_id);
    FETCH Cur_get_step INTO l_step_status, l_asqc;
    CLOSE Cur_get_step;
    IF l_step_status NOT IN (2, 3) THEN
      gme_common_pvt.log_message ('PC_STEP_STATUS_ERR');
      RAISE validation_fail;
    END IF;    
    IF (l_asqc = 1 AND l_step_status = 2) THEN
      gme_common_pvt.log_message ('GME_INV_STEP_STATUS_ASQC');
      RAISE validation_fail;
    END IF;
    gme_resource_engine_pvt.delete_resource_trans(p_tran_rec      => l_rsrc_txn_gtmp_rec
                                                 ,x_return_status => l_return_status);
    IF (l_return_status <> FND_API.G_RET_STS_SUCCESS) THEN
      RAISE delete_txn_fail;
    END IF;
    l_step_resources.batchstep_resource_id := p_rsrc_txn_gtmp_rec.line_id;
    IF NOT Gme_Batch_Step_Resources_Dbl.fetch_row(p_batch_step_resources => l_step_resources
                                                 ,x_batch_step_resources => l_step_resources) THEN
      RAISE rsrc_fetch_err;
    END IF;
    gme_resource_engine_pvt.get_resource_usage(p_step_resources_rec => l_step_resources
                                              ,x_step_resources_rec => l_new_step_resources
                                              ,x_return_status      => l_return_status);
    IF (l_return_status <> FND_API.G_RET_STS_SUCCESS) THEN
      RAISE get_usage_fail;
    END IF;
    IF (NVL(l_step_resources.actual_rsrc_usage, -1) <> NVL(l_new_step_resources.actual_rsrc_usage, -1)) THEN
      l_step_resources.actual_rsrc_usage := l_new_step_resources.actual_rsrc_usage;
      IF l_step_status = 2 THEN    
        gme_update_step_qty_pvt.reduce_pending_usage(p_batch_step_resources_rec => l_step_resources,
                                                     x_return_status            => l_return_status);
        IF (l_return_status <> FND_API.G_RET_STS_SUCCESS) THEN
          RAISE reduce_pend_usage_err;
        END IF;
      END IF;
      IF NOT gme_batch_step_resources_dbl.update_row(p_batch_step_resources => l_step_resources) THEN
        RAISE upd_rsrc_err;
      END IF;    
    END IF;
    IF g_debug <= gme_debug.g_log_procedure THEN
      gme_debug.put_line ('Exiting api ' || g_pkg_name || '.' || l_api_name);
    END IF;
  EXCEPTION
    WHEN setup_failed OR fetch_txn_failed OR validation_fail OR rsrc_fetch_err OR upd_rsrc_err THEN
      x_return_status := FND_API.G_RET_STS_ERROR;
    WHEN delete_txn_fail OR get_usage_fail OR reduce_pend_usage_err THEN
      x_return_status := l_return_status;
    WHEN OTHERS THEN
      x_return_status := FND_API.G_RET_STS_UNEXP_ERROR;
      fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
      IF (NVL (g_debug, -1) = gme_debug.g_log_unexpected) THEN
        gme_debug.put_line(g_pkg_name|| '.'|| l_api_name|| ':'|| 'WHEN OTHERS:'|| SQLERRM);
      END IF;
  END delete_resource_txn;
END gme_api_grp;
/
COMMIT;
EXIT;
